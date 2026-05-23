#!/usr/bin/env python3
# -*- coding: utf-8 -*-


import argparse
from pathlib import Path
from typing import Dict, List

import numpy as np
import pandas as pd


PROMPT_SHEET = "Z-Image Complementary"

RAW_REQUIRED_COLS = [
    "ModelShortName",
    "PromptID",
    "Language",
    "Resolution",
    "Steps",
    "SeedNo",
    "OpenCLIPScore",
    "AestheticScore",
    "MUSIQScore",
    "ImageSizeBytes",
    "ElapsedSeconds",
    "IterPerSecond",
    "VRAMUsage",
    "GPUUsage",
    "MemoryUsage",
    "CPUUsage",
    "FileName",
]

PROMPT_REQUIRED_COLS = [
    "ID",
    "Module",
    "Prompt_Group",
    "Source_ID",
    "Prompt_Version",
    "Content",
    "Style",
    "Complexity",
    "Context",
    "Chinese_Prompt",
    "Generate_N",
    "Analysis_Note",
]


def safe_str(x) -> str:
    if pd.isna(x):
        return ""
    return str(x).strip()


def validate_columns(df: pd.DataFrame, required: List[str], df_name: str) -> None:
    missing = [c for c in required if c not in df.columns]
    if missing:
        raise ValueError(f"{df_name} missing required columns: {missing}")


def robust_minmax_normalize(series: pd.Series, q_low: float = 0.05, q_high: float = 0.95) -> pd.Series:
    """
    Fixed robust normalization:
    1) Use the 5th and 95th percentiles as robust lower and upper bounds;
    2) Scale by min-max;
    3) Clip values outside the bounds to [0, 1].

    If the 5th and 95th percentiles have no valid difference, fall back to ordinary min-max;
    If there is still no difference, return 0.5.
    """
    s = pd.to_numeric(series, errors="coerce")
    non_na = s.dropna()

    if non_na.empty:
        return pd.Series(np.nan, index=series.index)

    low = non_na.quantile(q_low)
    high = non_na.quantile(q_high)

    if pd.isna(low) or pd.isna(high) or high <= low:
        low = non_na.min()
        high = non_na.max()

    if pd.isna(low) or pd.isna(high) or high <= low:
        return pd.Series(0.5, index=series.index)

    norm = (s - low) / (high - low)
    norm = norm.clip(lower=0.0, upper=1.0)
    return norm


def map_prompt_version(v: str) -> str:
    """Standardize Prompt_Version names for later pivoting."""
    x = safe_str(v)
    x_low = x.lower().replace(" ", "")

    if x in ["BaseVersion", "Base", "Original"] or "Base" in x or x_low in ["base", "basic"]:
        return "BaseVersion"
    if x in ["RevisionA", "RevisionA", "RevisionA"] or "rewritea" in x_low or x_low.endswith("a"):
        return "RevisionA"
    if x in ["RevisionB", "RevisionB", "RevisionB"] or "rewriteb" in x_low or x_low.endswith("b"):
        return "RevisionB"

    return x if x else "UnknownVersion"


def combine_prompt_type(row: pd.Series) -> str:
    return f"{safe_str(row['Content'])} | {safe_str(row['Style'])} | {safe_str(row['Complexity'])} | {safe_str(row['Context'])}"


def classify_stability(max_diff: float, group_sd: float) -> str:
    """
    A transparent and interpretable empirical rule:
    - Stable:MaxDifference < 0.05 andWithinGroupSD < 0.03
    - BasicallyStable:MaxDifference < 0.10 andWithinGroupSD < 0.05
    - LargeFluctuation:otherwise
    """
    if pd.isna(max_diff):
        return "Undetermined"
    if max_diff < 0.05 and (pd.isna(group_sd) or group_sd < 0.03):
        return "Stable"
    if max_diff < 0.10 and (pd.isna(group_sd) or group_sd < 0.05):
        return "BasicallyStable"
    return "LargeFluctuation"


def load_data(raw_excel: str, prompt_excel: str):
    raw_df = pd.read_excel(raw_excel)
    prompt_df = pd.read_excel(prompt_excel, sheet_name=PROMPT_SHEET)

    validate_columns(raw_df, RAW_REQUIRED_COLS, "raw objective-score Excel")
    validate_columns(prompt_df, PROMPT_REQUIRED_COLS, f"prompt Excel {PROMPT_SHEET} sheet")

    raw_df = raw_df.copy()
    raw_df["PromptID"] = raw_df["PromptID"].map(safe_str)
    raw_df["Language"] = raw_df["Language"].map(safe_str)

    prompt_df = prompt_df.copy()
    prompt_df["ID"] = prompt_df["ID"].map(safe_str)
    prompt_df["Prompt_Group"] = prompt_df["Prompt_Group"].map(safe_str)
    prompt_df["Prompt_Version"] = prompt_df["Prompt_Version"].map(map_prompt_version)

    prompt_m1 = prompt_df[prompt_df["ID"].str.startswith("M1-", na=False)].copy()
    if prompt_m1.empty:
        raise ValueError("No Module 1 sample with an ID starting with M1- was found in the prompt table.")

    merged = pd.merge(
        raw_df,
        prompt_m1,
        left_on="PromptID",
        right_on="ID",
        how="inner",
        suffixes=("", "_prompt"),
    )

    if merged.empty:
        raise ValueError("The merge between raw objective scores and Module 1 prompt information is empty. Please check PromptID / ID for consistency.")

    return merged


def calc_image_level_scores(df: pd.DataFrame) -> pd.DataFrame:
    out = df.copy()

    out["OpenCLIPNormScore"] = robust_minmax_normalize(out["OpenCLIPScore"])
    out["AestheticNormScore"] = robust_minmax_normalize(out["AestheticScore"])
    out["MUSIQNormScore"] = robust_minmax_normalize(out["MUSIQScore"])

    out["WeightedCompositeScore"] = (
        0.45 * out["OpenCLIPNormScore"]
        + 0.35 * out["AestheticNormScore"]
        + 0.20 * out["MUSIQNormScore"]
    )

    out["PromptType"] = out.apply(combine_prompt_type, axis=1)
    return out


def build_group_detail_table(image_df: pd.DataFrame) -> pd.DataFrame:
    version_mean = (
        image_df.groupby(["Prompt_Group", "Prompt_Version"], as_index=False)["WeightedCompositeScore"]
        .mean()
        .rename(columns={"WeightedCompositeScore": "VersionCompositeMean"})
    )

    pivot = version_mean.pivot(index="Prompt_Group", columns="Prompt_Version", values="VersionCompositeMean")
    pivot = pivot.reset_index()

    meta = (
        image_df.groupby("Prompt_Group", as_index=False)
        .agg({
            "Content": "first",
            "Style": "first",
            "Complexity": "first",
            "Context": "first",
            "PromptType": "first",
        })
    )

    detail = pd.merge(meta, pivot, on="Prompt_Group", how="left")

    for col in ["BaseVersion", "RevisionA", "RevisionB"]:
        if col not in detail.columns:
            detail[col] = np.nan

    version_cols = ["BaseVersion", "RevisionA", "RevisionB"]
    detail["WithinGroupMaxDifference"] = detail[version_cols].max(axis=1) - detail[version_cols].min(axis=1)
    detail["WithinGroupSD"] = detail[version_cols].std(axis=1, ddof=1)

    detail["ManualCheckConclusion"] = ""
    detail["StabilityJudgment"] = detail.apply(
        lambda r: classify_stability(r["WithinGroupMaxDifference"], r["WithinGroupSD"]), axis=1
    )

    detail = detail.rename(columns={
        "BaseVersion": "BaseVersionMean",
        "RevisionA": "RevisionAMean",
        "RevisionB": "RevisionBMean",
    })

    detail = detail[
        [
            "Prompt_Group",
            "PromptType",
            "BaseVersionMean",
            "RevisionAMean",
            "RevisionBMean",
            "WithinGroupMaxDifference",
            "WithinGroupSD",
            "ManualCheckConclusion",
            "StabilityJudgment",
            "Content",
            "Style",
            "Complexity",
            "Context",
        ]
    ].sort_values("Prompt_Group")

    return detail


def build_summary_tables(image_df: pd.DataFrame, detail_df: pd.DataFrame):
    overall = pd.DataFrame([
        {
            "PromptType": "Overall",
            "BasePromptCount": detail_df["Prompt_Group"].nunique(),
            "RevisionVersionCount": 2,
            "CompositeScoreMean": image_df["WeightedCompositeScore"].mean(),
            "CompositeScoreSD": image_df["WeightedCompositeScore"].std(ddof=1),
            "MaxDifference": detail_df["WithinGroupMaxDifference"].max(),
            "StabilityJudgment": classify_stability(
                detail_df["WithinGroupMaxDifference"].max(),
                detail_df["WithinGroupSD"].mean() if not detail_df["WithinGroupSD"].isna().all() else np.nan,
            ),
            "Notes": "MaxDifference is the maximum WithinGroupMaxDifference across prompt groups.",
        }
    ])

    content_map = detail_df[["Prompt_Group", "Content"]].copy()
    tmp = pd.merge(image_df[["Prompt_Group", "WeightedCompositeScore"]], content_map, on="Prompt_Group", how="left")
    detail_with_content = detail_df[["Prompt_Group", "Content", "WithinGroupMaxDifference", "WithinGroupSD"]].copy()

    by_content_score = (
        tmp.groupby("Content", as_index=False)
        .agg(
            CompositeScoreMean=("WeightedCompositeScore", "mean"),
            CompositeScoreSD=("WeightedCompositeScore", "std"),
            ImageCount=("WeightedCompositeScore", "count"),
        )
        .rename(columns={"CompositeScoreSD": "CompositeScoreSD"})
    )

    by_content_prompt = (
        detail_with_content.groupby("Content", as_index=False)
        .agg(
            BasePromptCount=("Prompt_Group", "nunique"),
            MaxDifference=("WithinGroupMaxDifference", "max"),
            MeanWithinGroupSD=("WithinGroupSD", "mean"),
        )
    )

    by_content = pd.merge(by_content_prompt, by_content_score, on="Content", how="left")
    by_content["RevisionVersionCount"] = 2
    by_content["StabilityJudgment"] = by_content.apply(
        lambda r: classify_stability(r["MaxDifference"], r["MeanWithinGroupSD"]), axis=1
    )
    by_content["Notes"] = "MaxDifference is the maximum WithinGroupMaxDifference within each content type."
    by_content = by_content.rename(columns={
        "Content": "PromptType",
        "BasePromptCount": "BasePromptCount",
    })
    by_content = by_content[
        [
            "PromptType",
            "BasePromptCount",
            "RevisionVersionCount",
            "CompositeScoreMean",
            "CompositeScoreSD",
            "MaxDifference",
            "StabilityJudgment",
            "Notes",
            "ImageCount",
            "MeanWithinGroupSD",
        ]
    ].sort_values("PromptType")

    return overall, by_content


def round_numeric(df: pd.DataFrame, digits: int = 4) -> pd.DataFrame:
    out = df.copy()
    num_cols = out.select_dtypes(include=[np.number]).columns
    out[num_cols] = out[num_cols].round(digits)
    return out


def export_excel(output_path: str, image_df: pd.DataFrame, detail_df: pd.DataFrame,
                 overall_df: pd.DataFrame, by_content_df: pd.DataFrame) -> None:
    output_path = str(output_path)
    Path(output_path).parent.mkdir(parents=True, exist_ok=True)

    with pd.ExcelWriter(output_path, engine="openpyxl") as writer:
        round_numeric(image_df).to_excel(writer, sheet_name="m1_image_scores", index=False)
        round_numeric(detail_df).to_excel(writer, sheet_name="m1_group_detail", index=False)
        round_numeric(overall_df).to_excel(writer, sheet_name="m1_summary_overall", index=False)
        round_numeric(by_content_df).to_excel(writer, sheet_name="m1_summary_by_content", index=False)


def parse_args():
    parser = argparse.ArgumentParser(description="Stage 4 Module 1 prompt micro-revision stability analysis")
    parser.add_argument("raw_scores_excel", help="Raw objective-score Excel file")
    parser.add_argument("prompt_excel", help="Prompt metadata Excel file (sheet: Z-Image Complementary)")
    parser.add_argument("output_excel", help="Output Excel file")
    return parser.parse_args()


def main():
    args = parse_args()

    merged = load_data(args.raw_scores_excel, args.prompt_excel)
    image_df = calc_image_level_scores(merged)
    detail_df = build_group_detail_table(image_df)
    overall_df, by_content_df = build_summary_tables(image_df, detail_df)
    export_excel(args.output_excel, image_df, detail_df, overall_df, by_content_df)

    print("Module 1 stability analysis completed:", args.output_excel)
    print("Output sheets:")
    print("  - m1_image_scores       image-level objective and composite scores")
    print("  - m1_group_detail       prompt group-level stability details")
    print("  - m1_summary_overall    overall stability summary")
    print("  - m1_summary_by_content summary by content type")


if __name__ == "__main__":
    main()
