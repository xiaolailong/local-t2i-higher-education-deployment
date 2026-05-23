#!/usr/bin/env python3
# -*- coding: utf-8 -*-


import argparse
from pathlib import Path
from typing import Dict, List, Tuple

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib import rcParams

rcParams["font.sans-serif"] = [
    "Microsoft YaHei",
    "SimHei",
    "Noto Sans CJK SC",
    "WenQuanYi Zen Hei",
    "Arial Unicode MS",
    "DejaVu Sans",
]
rcParams["axes.unicode_minus"] = False

REQUIRED_COLUMNS = [
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

METRICS = ["OpenCLIPScore", "AestheticScore", "MUSIQScore"]

NORMALIZED_METRIC_COLUMNS = {
    "OpenCLIPScore": "OpenCLIPNormalizedScore",
    "AestheticScore": "AestheticNormalizedScore",
    "MUSIQScore": "MUSIQNormalizedScore",
}

WEIGHTED_OBJECTIVE_COLUMN = "WeightedObjectiveScore"
EQUAL_OBJECTIVE_COLUMN = "EqualObjectiveScore"

RULES_TEXT = """\
Normalization strategy notes for the Stage 3 Extended objective validation
================================================================================

Analysis unit
-------------
Stage 3 uses "Language x Metric" as the normalization decision unit.
That is:
- within the Chinese group, Qwen / Z-Image / SDXL-Turbo jointly estimate the normalization scale;
- within the English group, Qwen / Z-Image / SDXL-Turbo jointly estimate the normalization scale;
- OpenCLIP / Aesthetic / MUSIQ are evaluated and normalized separately within each language group.

Rationale
---------
Stage 3 aims to compare the objective performance of different models under the same language condition, so normalization should not be performed separately within each model.
If normalization were performed by "Model x Language", each model would be compressed into a similar 0-1 range, which would weaken or even remove between-model differences.
Therefore, Stage 3 uses cross-model normalization within the same language condition to ensure comparability among models.

Candidate normalization strategies
----------------------------------
1. Standard within-group Min-Max
2. Fixed robust normalization (P5-P95 + clipping)
3. Percentile scaling without clipping
4. Simple within-group Min-Max or z-score for small samples; the actual output uses Min-Max to keep values near the 0-1 scale

Decision rules
--------------
Let the statistics of a given "Language x Metric" unit be:
- n                 : number of non-missing samples
- skew              : skewness
- outlier_ratio     : IQR outlier ratio
- boundary_ratio    : below-P5 ratio + above-P95 ratio
- unique_count      : number of unique values

Rule 1: Small-sample priority
If n < 20:
    recommendation = Simple within-group Min-Max or z-score

Rule 2: Narrow distribution / low discreteness
If unique_count <= 5:
    recommendation = Simple within-group Min-Max

Rule 3: Clear skewness or many outliers, with low boundary-flattening risk
If (abs(skew) >= 1.0 or outlier_ratio >= 0.08) and boundary_ratio <= 0.12:
    recommendation = Fixed robust normalization (P5-P95 + clipping)

Rule 4: Skewness or outliers with high boundary-flattening risk
If (abs(skew) >= 1.0 or outlier_ratio >= 0.08) and boundary_ratio > 0.12:
    recommendation = Percentile scaling without clipping

Rule 5: Otherwise
If none of the above rules is met:
    recommendation = Standard within-group Min-Max
"""



def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Stage 3 Extended objective validation: cross-model normalization within each language group")
    parser.add_argument("excel_path", help="Input Excel file path")
    parser.add_argument("--sheet-name", default=0, help="Input Excel sheet name or index; default is the first sheet")
    parser.add_argument("--output-dir", default=None, help="Output directory. By default, an extended_analysis folder is created next to the Excel file")
    parser.add_argument("--bins", type=int, default=20, help="Number of histogram bins; default is 20")
    parser.add_argument("--openclip-weight", type=float, default=0.45, help="Weight for the normalized OpenCLIP score; default is 0.45")
    parser.add_argument("--aesthetic-weight", type=float, default=0.35, help="Weight for the normalized Aesthetic score; default is 0.35")
    parser.add_argument("--musiq-weight", type=float, default=0.20, help="Weight for the normalized MUSIQ score; default is 0.20")
    return parser.parse_args()


def read_excel_with_sheet(excel_path: Path, sheet_name):
    if isinstance(sheet_name, str) and sheet_name.isdigit():
        sheet_name = int(sheet_name)
    return pd.read_excel(excel_path, sheet_name=sheet_name)


def ensure_required_columns(df: pd.DataFrame) -> None:
    missing = [col for col in REQUIRED_COLUMNS if col not in df.columns]
    if missing:
        raise ValueError("The Excel file is missing the following required columns:\n- " + "\n- ".join(missing))


def coerce_columns(df: pd.DataFrame) -> pd.DataFrame:
    out = df.copy()
    for col in METRICS + ["ElapsedSeconds", "InferenceSpeedIterPerSecond", "VRAMUsage", "GPUUsage", "MemoryUsage", "CPUUsage"]:
        if col in out.columns:
            out[col] = pd.to_numeric(out[col], errors="coerce")
    out["ModelShortName"] = out["ModelShortName"].astype(str).str.strip()
    out["Language"] = out["Language"].astype(str).str.strip().str.lower()
    return out


def safe_name(text: str, max_len: int = 120) -> str:
    text = str(text)
    for ch in ['\\', '/', '*', '[', ']', ':', '?', '"', '<', '>', '|']:
        text = text.replace(ch, "_")
    return text[:max_len]


def compute_outlier_ratio_iqr(series: pd.Series) -> float:
    s = series.dropna().astype(float)
    if len(s) < 4:
        return float("nan")
    q1 = s.quantile(0.25)
    q3 = s.quantile(0.75)
    iqr = q3 - q1
    if pd.isna(iqr) or abs(iqr) < 1e-12:
        return 0.0
    lower = q1 - 1.5 * iqr
    upper = q3 + 1.5 * iqr
    return float(((s < lower) | (s > upper)).mean())


def compute_stats(series: pd.Series) -> Dict[str, float]:
    s = series.dropna().astype(float)
    if len(s) == 0:
        return {
            "SampleSize": 0,
            "mean": np.nan,
            "std": np.nan,
            "P1": np.nan,
            "P5": np.nan,
            "P50": np.nan,
            "P95": np.nan,
            "P99": np.nan,
            "BelowP5Ratio": np.nan,
            "AboveP95Ratio": np.nan,
            "Min": np.nan,
            "Max": np.nan,
            "Skewness": np.nan,
            "IQROutlierRatio": np.nan,
            "UniqueCount": 0,
        }
    p1 = float(np.percentile(s, 1))
    p5 = float(np.percentile(s, 5))
    p50 = float(np.percentile(s, 50))
    p95 = float(np.percentile(s, 95))
    p99 = float(np.percentile(s, 99))
    return {
        "SampleSize": int(len(s)),
        "mean": float(s.mean()),
        "std": float(s.std(ddof=1)) if len(s) > 1 else 0.0,
        "P1": p1,
        "P5": p5,
        "P50": p50,
        "P95": p95,
        "P99": p99,
        "BelowP5Ratio": float((s < p5).mean()),
        "AboveP95Ratio": float((s > p95).mean()),
        "Min": float(s.min()),
        "Max": float(s.max()),
        "Skewness": float(s.skew()) if len(s) > 2 else np.nan,
        "IQROutlierRatio": compute_outlier_ratio_iqr(s),
        "UniqueCount": int(s.nunique(dropna=True)),
    }


def recommend_metric_normalization(stats: Dict[str, float]) -> Dict[str, object]:
    n = stats["SampleSize"]
    skew = stats["Skewness"]
    outlier_ratio = stats["IQROutlierRatio"]
    lower_ratio = stats["BelowP5Ratio"] if pd.notna(stats["BelowP5Ratio"]) else 0.0
    upper_ratio = stats["AboveP95Ratio"] if pd.notna(stats["AboveP95Ratio"]) else 0.0
    boundary_ratio = lower_ratio + upper_ratio
    unique_count = stats["UniqueCount"]

    if n < 20:
        return {
            "RecommendedNormalizationStrategy": "SimpleWithinGroupMinMaxOrZScore",
            "TriggeredRule": "Rule1SmallSamplePriority",
            "Rationale": f"The effective sample size is {n}, below 20; P5/P95 is not stable enough. The actual output uses Min-Max to keep the 0-1 scale.",
            "BoundaryRatioTotal": boundary_ratio,
        }

    if unique_count <= 5:
        return {
            "RecommendedNormalizationStrategy": "SimpleWithinGroupMinMax",
            "TriggeredRule": "Rule2NarrowDistributionOrLowDiscreteness",
            "Rationale": f"The number of unique values is {unique_count}, indicating low discreteness; complex percentile normalization has limited benefit.",
            "BoundaryRatioTotal": boundary_ratio,
        }

    has_skew = pd.notna(skew) and abs(skew) >= 1.0
    has_outlier = pd.notna(outlier_ratio) and outlier_ratio >= 0.08

    if (has_skew or has_outlier) and boundary_ratio <= 0.12:
        return {
            "RecommendedNormalizationStrategy": "FixedRobustP5P95Clipping",
            "TriggeredRule": "Rule3SkewedOrOutliersLowBoundaryRisk",
            "Rationale": f"Skewness={skew:.3f}, IQR outlier ratio={outlier_ratio:.2%}, boundary ratio={boundary_ratio:.2%}; P5-P95 clipping is suitable for reducing the influence of extreme values.",
            "BoundaryRatioTotal": boundary_ratio,
        }

    if (has_skew or has_outlier) and boundary_ratio > 0.12:
        return {
            "RecommendedNormalizationStrategy": "PercentileScalingWithoutClipping",
            "TriggeredRule": "Rule4SkewedOrOutliersHighBoundaryRisk",
            "Rationale": f"Skewness={skew:.3f}, IQR outlier ratio={outlier_ratio:.2%}, boundary ratio={boundary_ratio:.2%}; direct clipping may flatten boundary distinctions.",
            "BoundaryRatioTotal": boundary_ratio,
        }

    return {
        "RecommendedNormalizationStrategy": "StandardWithinGroupMinMax",
        "TriggeredRule": "Rule5DefaultStableDistribution",
        "Rationale": f"Skewness={skew:.3f}, IQR outlier ratio={outlier_ratio:.2%}, boundary ratio={boundary_ratio:.2%}; standard Min-Max is sufficient.",
        "BoundaryRatioTotal": boundary_ratio,
    }


def safe_divide(numerator: pd.Series, denominator: float) -> pd.Series:
    if pd.isna(denominator) or abs(denominator) < 1e-12:
        return pd.Series(np.zeros(len(numerator)), index=numerator.index, dtype=float)
    return numerator / denominator


def apply_normalization_by_strategy(series: pd.Series, stats: Dict[str, float], strategy: str) -> pd.Series:
    s = pd.to_numeric(series, errors="coerce").astype(float)
    min_v = stats["Min"]
    max_v = stats["Max"]
    p5 = stats["P5"]
    p95 = stats["P95"]

    if strategy in ["StandardWithinGroupMinMax", "SimpleWithinGroupMinMax", "SimpleWithinGroupMinMaxOrZScore"]:
        return safe_divide(s - min_v, max_v - min_v)

    if strategy == "FixedRobustP5P95Clipping":
        clipped = s.clip(lower=p5, upper=p95)
        return safe_divide(clipped - p5, p95 - p5)

    if strategy == "PercentileScalingWithoutClipping":
        return safe_divide(s - p5, p95 - p5)

    return safe_divide(s - min_v, max_v - min_v)


def draw_histogram(series: pd.Series, title: str, out_path: Path, bins: int) -> None:
    s = series.dropna().astype(float)
    if len(s) == 0:
        return
    p5 = float(np.percentile(s, 5))
    p50 = float(np.percentile(s, 50))
    p95 = float(np.percentile(s, 95))
    plt.figure(figsize=(8, 5))
    plt.hist(s, bins=bins)
    plt.axvline(p5, linestyle="--", linewidth=1, label="P5")
    plt.axvline(p50, linestyle="--", linewidth=1, label="P50")
    plt.axvline(p95, linestyle="--", linewidth=1, label="P95")
    plt.title(title)
    plt.xlabel("Score")
    plt.ylabel("Frequency")
    plt.legend()
    plt.tight_layout()
    plt.savefig(out_path, dpi=180)
    plt.close()


def build_distribution_tables(df: pd.DataFrame, hist_root: Path, bins: int) -> Tuple[pd.DataFrame, pd.DataFrame]:
    stats_rows: List[Dict[str, object]] = []
    rec_rows: List[Dict[str, object]] = []

    for lang, group in df.groupby("Language", dropna=False):
        lang_name = str(lang)
        lang_dir = hist_root / safe_name(lang_name)
        lang_dir.mkdir(parents=True, exist_ok=True)

        for metric in METRICS:
            series = group[metric].dropna()
            stats = compute_stats(series)
            recommendation = recommend_metric_normalization(stats)
            draw_histogram(
                series=series,
                title=f"{lang_name} - {metric}",
                out_path=lang_dir / f"{safe_name(lang_name)}_{safe_name(metric)}.png",
                bins=bins,
            )
            stats_rows.append({
                "Language": lang_name,
                "GroupName": lang_name,
                "Metric": metric,
                **stats,
                "BoundaryRatioTotal": recommendation["BoundaryRatioTotal"],
            })
            rec_rows.append({
                "Language": lang_name,
                "GroupName": lang_name,
                "Metric": metric,
                "SampleSize": stats["SampleSize"],
                "RecommendedNormalizationStrategy": recommendation["RecommendedNormalizationStrategy"],
                "TriggeredRule": recommendation["TriggeredRule"],
                "Rationale": recommendation["Rationale"],
            })

    return pd.DataFrame(stats_rows), pd.DataFrame(rec_rows)


def add_normalized_scores(df: pd.DataFrame, stats_df: pd.DataFrame, rec_df: pd.DataFrame, weights: Dict[str, float]) -> pd.DataFrame:
    out_df = df.copy()
    stats_lookup = stats_df.set_index(["Language", "Metric"]).to_dict("index")
    rec_lookup = rec_df.set_index(["Language", "Metric"])["RecommendedNormalizationStrategy"].to_dict()

    for metric, norm_col in NORMALIZED_METRIC_COLUMNS.items():
        out_df[norm_col] = np.nan
        for lang, idx in out_df.groupby("Language", dropna=False).groups.items():
            lang_key = str(lang)
            key = (lang_key, metric)
            if key not in stats_lookup or key not in rec_lookup:
                continue
            stats = stats_lookup[key]
            strategy = rec_lookup[key]
            out_df.loc[idx, norm_col] = apply_normalization_by_strategy(out_df.loc[idx, metric], stats, strategy)

    out_df[WEIGHTED_OBJECTIVE_COLUMN] = (
        out_df["OpenCLIPNormalizedScore"] * weights["OpenCLIPScore"]
        + out_df["AestheticNormalizedScore"] * weights["AestheticScore"]
        + out_df["MUSIQNormalizedScore"] * weights["MUSIQScore"]
    )
    out_df[EQUAL_OBJECTIVE_COLUMN] = out_df[
        ["OpenCLIPNormalizedScore", "AestheticNormalizedScore", "MUSIQNormalizedScore"]
    ].mean(axis=1)
    return out_df


def describe_metric(grouped: pd.core.groupby.generic.DataFrameGroupBy, metric: str, prefix: str) -> pd.DataFrame:
    desc = grouped[metric].agg(["count", "mean", "std", "median", "min", "max"]).reset_index()
    desc = desc.rename(columns={
        "count": f"{prefix}SampleSize",
        "mean": f"{prefix}Mean",
        "std": f"{prefix}SD",
        "median": f"{prefix}Median",
        "min": f"{prefix}Min",
        "max": f"{prefix}Max",
    })
    return desc


def build_model_summary(normalized_df: pd.DataFrame) -> pd.DataFrame:
    grouped = normalized_df.groupby(["Language", "ModelShortName"], dropna=False)
    summary = describe_metric(grouped, WEIGHTED_OBJECTIVE_COLUMN, "WeightedObjectiveScore")

    for metric, prefix in [
        (EQUAL_OBJECTIVE_COLUMN, "EqualObjectiveScore"),
        ("OpenCLIPNormalizedScore", "OpenCLIPNormalizedScore"),
        ("AestheticNormalizedScore", "AestheticNormalizedScore"),
        ("MUSIQNormalizedScore", "MUSIQNormalizedScore"),
        ("ElapsedSeconds", "ElapsedSeconds"),
        ("VRAMUsage", "VRAMUsage"),
    ]:
        if metric in normalized_df.columns:
            one = describe_metric(grouped, metric, prefix)
            one = one.drop(columns=["Language", "ModelShortName"])
            summary = pd.concat([summary, one], axis=1)

    summary["WeightedObjectiveScoreRank"] = summary.groupby("Language")["WeightedObjectiveScoreMean"].rank(method="min", ascending=False).astype(int)
    summary["EqualObjectiveScoreRank"] = summary.groupby("Language")["EqualObjectiveScoreMean"].rank(method="min", ascending=False).astype(int)
    summary = summary.sort_values(["Language", "WeightedObjectiveScoreRank", "ModelShortName"])
    return summary


def make_ranking_label(sub: pd.DataFrame, metric_mean_col: str) -> str:
    ordered = sub.sort_values(metric_mean_col, ascending=False)["ModelShortName"].astype(str).tolist()
    return " > ".join(ordered)


def judge_support(ranking: str) -> str:
    parts = [x.strip() for x in ranking.split(">")]
    if len(parts) < 3:
        return "NotAssessable"
    normalized = [p.lower().replace("-", "").replace("_", "") for p in parts]
    if normalized[0].startswith("qwen") and normalized[1].startswith("zimage") and normalized[2].startswith("sdxl"):
        return "SupportsMainFinding"
    if normalized[0].startswith("zimage") and normalized[1].startswith("qwen") and normalized[2].startswith("sdxl"):
        return "PartiallySupportsMainFindingQwenZImageSwappedSDXLStillLowest"
    if normalized[-1].startswith("sdxl"):
        return "PartiallySupportsMainFindingSDXLStillLowest"
    return "DoesNotSupportOrNeedsExplanation"


def build_stability_check(model_summary: pd.DataFrame) -> pd.DataFrame:
    metric_map = {
        "WeightedObjectiveScore": "WeightedObjectiveScoreMean",
        "EqualObjectiveScore": "EqualObjectiveScoreMean",
        "OpenCLIP": "OpenCLIPNormalizedScoreMean",
        "Aesthetic": "AestheticNormalizedScoreMean",
        "MUSIQ": "MUSIQNormalizedScoreMean",
    }
    rows: List[Dict[str, object]] = []
    for lang, sub in model_summary.groupby("Language", dropna=False):
        for metric_label, mean_col in metric_map.items():
            if mean_col not in sub.columns:
                continue
            rank_series = sub.set_index("ModelShortName")[mean_col].rank(method="min", ascending=False).astype(int)
            ranking = make_ranking_label(sub, mean_col)
            rows.append({
                "Language": lang,
                "Metric": metric_label,
                "QwenRank": rank_series.get("qwen", rank_series.get("Qwen", np.nan)),
                "ZImageRank": rank_series.get("zimage", rank_series.get("Z-Image", np.nan)),
                "SDXLRank": rank_series.get("sdxl", rank_series.get("SDXL", np.nan)),
                "Order": ranking,
                "SupportForMainFinding": judge_support(ranking),
            })
    return pd.DataFrame(rows)


def build_prompt_level_summary(normalized_df: pd.DataFrame) -> pd.DataFrame:
    group_cols = ["Language", "PromptID", "ModelShortName"]
    summary = normalized_df.groupby(group_cols, dropna=False).agg(
        GenerationCount=("FileName", "count"),
        WeightedObjectiveScoreMean=(WEIGHTED_OBJECTIVE_COLUMN, "mean"),
        WeightedObjectiveScoreSD=(WEIGHTED_OBJECTIVE_COLUMN, "std"),
        EqualObjectiveScoreMean=(EQUAL_OBJECTIVE_COLUMN, "mean"),
        OpenCLIPNormalizedScoreMean=("OpenCLIPNormalizedScore", "mean"),
        AestheticNormalizedScoreMean=("AestheticNormalizedScore", "mean"),
        MUSIQNormalizedScoreMean=("MUSIQNormalizedScore", "mean"),
    ).reset_index()
    summary["WeightedObjectiveScoreSD"] = summary["WeightedObjectiveScoreSD"].fillna(0.0)
    return summary


def build_rules_df() -> pd.DataFrame:
    return pd.DataFrame([
        {"RuleID": "Rule1", "Condition": "SampleSize n < 20", "RecommendedStrategy": "SimpleWithinGroupMinMaxOrZScore", "Description": "P5/P95 is unstable for small samples; the actual output uses Min-Max to maintain a consistent scale."},
        {"RuleID": "Rule2", "Condition": "UniqueCount <= 5", "RecommendedStrategy": "SimpleWithinGroupMinMax", "Description": "The distribution is too narrow or has low discreteness; complex percentile normalization has limited benefit."},
        {"RuleID": "Rule3", "Condition": "(abs(skew) >= 1.0 or IQROutlierRatio >= 0.08) and BoundaryRatioTotal <= 0.12", "RecommendedStrategy": "FixedRobustP5P95Clipping", "Description": "Clear skewness or outliers are present, and clipping is unlikely to cause serious boundary flattening."},
        {"RuleID": "Rule4", "Condition": "(abs(skew) >= 1.0 or IQROutlierRatio >= 0.08) and BoundaryRatioTotal > 0.12", "RecommendedStrategy": "PercentileScalingWithoutClipping", "Description": "Long tails or outliers are present, but direct clipping may reduce boundary discrimination."},
        {"RuleID": "Rule5", "Condition": "None of the above conditions is met", "RecommendedStrategy": "StandardWithinGroupMinMax", "Description": "The distribution is relatively stable; Min-Max normalization is sufficient."},
    ])


def write_report(report_out: Path, excel_path: Path, df: pd.DataFrame, stats_df: pd.DataFrame, rec_df: pd.DataFrame, model_summary: pd.DataFrame, stability_df: pd.DataFrame, weights: Dict[str, float]) -> None:
    with open(report_out, "w", encoding="utf-8") as f:
        f.write("Stage 3 Extended objective validation report\n")
        f.write("=" * 100 + "\n\n")
        f.write(f"Input file: {excel_path}\n")
        f.write(f"Total image samples: {len(df)}\n")
        f.write(f"Number of language groups: {df['Language'].nunique(dropna=False)}\n")
        f.write(f"Number of models: {df['ModelShortName'].nunique(dropna=False)}\n")
        f.write(f"Number of prompts: {df['PromptID'].nunique(dropna=False)}\n")
        f.write(f"Weights: OpenCLIP={weights['OpenCLIPScore']}, Aesthetic={weights['AestheticScore']}, MUSIQ={weights['MUSIQScore']}\n\n")

        f.write("1. Normalization strategy notes\n")
        f.write("-" * 100 + "\n")
        f.write(RULES_TEXT + "\n\n")

        f.write("2. Normalization recommendations by language group and metric\n")
        f.write("-" * 100 + "\n")
        for _, row in rec_df.iterrows():
            stat = stats_df[(stats_df["Language"] == row["Language"]) & (stats_df["Metric"] == row["Metric"])].iloc[0]
            f.write(f"Language: {row['Language']}, Metric: {row['Metric']}\n")
            f.write(f"  n={row['SampleSize']}, mean={stat['mean']:.6f}, std={stat['std']:.6f}\n")
            f.write(f"  P5={stat['P5']:.6f}, P50={stat['P50']:.6f}, P95={stat['P95']:.6f}\n")
            f.write(f"  Skewness={stat['Skewness']:.6f}, IQR outlier ratio={stat['IQROutlierRatio']:.2%}, boundary ratio={stat['BoundaryRatioTotal']:.2%}\n")
            f.write(f"  Recommended strategy: {row['RecommendedNormalizationStrategy']}\n")
            f.write(f"  Triggered rule: {row['TriggeredRule']}\n")
            f.write(f"  Rationale: {row['Rationale']}\n\n")

        f.write("3. Model-level descriptive statistics for the weighted objective score\n")
        f.write("-" * 100 + "\n")
        for _, row in model_summary.iterrows():
            f.write(
                f"Language={row['Language']}, Model={row['ModelShortName']}, "
                f"n={row['WeightedObjectiveScoreSampleSize']}, "
                f"WeightedObjectiveScore={row['WeightedObjectiveScoreMean']:.6f} +/- {row['WeightedObjectiveScoreSD']:.6f}, "
                f"Rank={row['WeightedObjectiveScoreRank']}\n"
            )
        f.write("\n")

        f.write("4. Robustness check\n")
        f.write("-" * 100 + "\n")
        for _, row in stability_df.iterrows():
            f.write(f"Language={row['Language']}, Metric={row['Metric']}, Order={row['Order']}, Judgment={row['SupportForMainFinding']}\n")


def analyze_excel(excel_path: Path, sheet_name, output_dir: Path, bins: int, weights: Dict[str, float]) -> None:
    weight_sum = sum(weights.values())
    if abs(weight_sum - 1.0) > 1e-8:
        weights = {k: v / weight_sum for k, v in weights.items()}
        print(f"The weight sum is not 1. The weights have been normalized automatically: {weights}")

    df = read_excel_with_sheet(excel_path, sheet_name=sheet_name)
    ensure_required_columns(df)
    df = coerce_columns(df)

    output_dir.mkdir(parents=True, exist_ok=True)
    hist_root = output_dir / "histograms"
    hist_root.mkdir(exist_ok=True)

    stats_df, rec_df = build_distribution_tables(df, hist_root=hist_root, bins=bins)
    normalized_df = add_normalized_scores(df, stats_df=stats_df, rec_df=rec_df, weights=weights)
    model_summary = build_model_summary(normalized_df)
    stability_df = build_stability_check(model_summary)
    prompt_level_summary = build_prompt_level_summary(normalized_df)
    rules_df = build_rules_df()

    output_excel = output_dir / "objective_analysis.xlsx"
    normalized_excel = output_dir / "normalized_scores.xlsx"
    model_summary_excel = output_dir / "model_summary.xlsx"
    report_out = output_dir / "analysis_report.txt"
    rules_out = output_dir / "normalization_rules.txt"

    with pd.ExcelWriter(output_excel, engine="openpyxl") as writer:
        stats_df.to_excel(writer, index=False, sheet_name="LangStats")
        rec_df.to_excel(writer, index=False, sheet_name="NormAdvice")
        rules_df.to_excel(writer, index=False, sheet_name="DecisionRules")
        normalized_df.to_excel(writer, index=False, sheet_name="NormDetails")
        model_summary.to_excel(writer, index=False, sheet_name="ModelSummary")
        prompt_level_summary.to_excel(writer, index=False, sheet_name="PromptModelMean")
        stability_df.to_excel(writer, index=False, sheet_name="RobustRankCheck")

    normalized_df.to_excel(normalized_excel, index=False)
    model_summary.to_excel(model_summary_excel, index=False)
    with open(rules_out, "w", encoding="utf-8") as f:
        f.write(RULES_TEXT)
    write_report(report_out, excel_path, df, stats_df, rec_df, model_summary, stability_df, weights)

    print("Analysis completed.")
    print(f"Output directory: {output_dir}")
    print(f"Combined results Excel: {output_excel}")
    print(f"Normalized score details Excel: {normalized_excel}")
    print(f"Model descriptive statistics Excel: {model_summary_excel}")
    print(f"Text report: {report_out}")
    print(f"Normalization rules: {rules_out}")
    print(f"Histogram directory: {hist_root}")


def main() -> None:
    args = parse_args()
    excel_path = Path(args.excel_path).expanduser().resolve()
    if not excel_path.exists():
        raise FileNotFoundError(f"Input file not found: {excel_path}")

    output_dir = Path(args.output_dir).expanduser().resolve() if args.output_dir else excel_path.parent / "extended_analysis"
    weights = {
        "OpenCLIPScore": args.openclip_weight,
        "AestheticScore": args.aesthetic_weight,
        "MUSIQScore": args.musiq_weight,
    }
    analyze_excel(
        excel_path=excel_path,
        sheet_name=args.sheet_name,
        output_dir=output_dir,
        bins=args.bins,
        weights=weights,
    )


if __name__ == "__main__":
    main()
