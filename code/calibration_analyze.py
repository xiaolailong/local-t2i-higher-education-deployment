#!/usr/bin/env python3
# -*- coding: utf-8 -*-


import argparse
from pathlib import Path
from typing import Dict, List

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


RULES_TEXT = """\
Normalization strategy rules
================================================================================

Analysis unit
-------------
Calibration normalization is diagnosed by ModelShortName, Language, and Metric.
This avoids mixing parameter candidates across models while keeping the parameter search reproducible.

Rules
-----
1. If n < 20, use SimpleMinMaxOrZScore; the implemented output uses min-max scaling.
2. If UniqueValueCount <= 5, use SimpleMinMax.
3. If skewness or IQR outliers are substantial and boundary flattening risk is low, use RobustP5P95Clipped.
4. If skewness or IQR outliers are substantial and boundary flattening risk is high, use PercentileScalingNoClipping.
5. Otherwise, use SimpleMinMax.
"""


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Analyze calibration objective-score distributions and normalize metrics.")
    parser.add_argument("excel_path", help="Input objective-score Excel file path")
    parser.add_argument(
        "--output-dir",
        default=None,
        help="Output directory. By default, create a *_distribution_analysis folder next to the input Excel file.",
    )
    parser.add_argument(
        "--bins",
        type=int,
        default=20,
        help="Number of histogram bins. Default: 20.",
    )
    return parser.parse_args()


def ensure_required_columns(df: pd.DataFrame) -> None:
    missing = [c for c in REQUIRED_COLUMNS if c not in df.columns]
    if missing:
        raise ValueError("Excel is missing the following required columns:\n- " + "\n- ".join(missing))


def coerce_metric_columns(df: pd.DataFrame) -> pd.DataFrame:
    out = df.copy()
    for col in METRICS:
        out[col] = pd.to_numeric(out[col], errors="coerce")
    return out


def safe_name(text: str, max_len: int = 120) -> str:
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
    if pd.isna(iqr) or iqr == 0:
        return 0.0
    lower = q1 - 1.5 * iqr
    upper = q3 + 1.5 * iqr
    return float(((s < lower) | (s > upper)).mean())


def compute_stats(series: pd.Series) -> Dict[str, float]:
    s = series.dropna().astype(float)
    if len(s) == 0:
        return {
            "SampleCount": 0,
            "mean": np.nan,
            "std": np.nan,
            "P1": np.nan,
            "P5": np.nan,
            "P50": np.nan,
            "P95": np.nan,
            "P99": np.nan,
            "BelowP5Ratio": np.nan,
            "AboveP95Ratio": np.nan,
            "MinValue": np.nan,
            "MaxValue": np.nan,
            "Skewness": np.nan,
            "IQROutlierRatio": np.nan,
            "UniqueValueCount": 0,
        }

    p1 = float(np.percentile(s, 1))
    p5 = float(np.percentile(s, 5))
    p50 = float(np.percentile(s, 50))
    p95 = float(np.percentile(s, 95))
    p99 = float(np.percentile(s, 99))

    return {
        "SampleCount": int(len(s)),
        "mean": float(s.mean()),
        "std": float(s.std(ddof=1)) if len(s) > 1 else 0.0,
        "P1": p1,
        "P5": p5,
        "P50": p50,
        "P95": p95,
        "P99": p99,
        "BelowP5Ratio": float((s < p5).mean()),
        "AboveP95Ratio": float((s > p95).mean()),
        "MinValue": float(s.min()),
        "MaxValue": float(s.max()),
        "Skewness": float(s.skew()) if len(s) > 2 else np.nan,
        "IQROutlierRatio": compute_outlier_ratio_iqr(s),
        "UniqueValueCount": int(s.nunique(dropna=True)),
    }


def draw_histogram(series: pd.Series, title: str, out_path: Path, bins: int = 20) -> None:
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


def recommend_metric_normalization(stats: Dict[str, float]) -> Dict[str, str]:
    n = stats["SampleCount"]
    skew = stats["Skewness"]
    outlier_ratio = stats["IQROutlierRatio"]
    boundary_ratio = (
        (stats["BelowP5Ratio"] if pd.notna(stats["BelowP5Ratio"]) else 0.0)
        + (stats["AboveP95Ratio"] if pd.notna(stats["AboveP95Ratio"]) else 0.0)
    )
    unique_count = stats["UniqueValueCount"]

    if n < 20:
        strategy = "SimpleMinMaxOrZScore"
        triggered_rule = "Rule1: small sample (n < 20)"
        reason = f"The metric has only {n} valid samples; P5/P95 estimates may be unstable, so Min-Max or z-score normalization is recommended."
    elif unique_count <= 5:
        strategy = "SimpleMinMax"
        triggered_rule = "Rule2: low value diversity (UniqueValueCount <= 5)"
        reason = f"The metric has only {unique_count} unique values; simple Min-Max normalization is sufficient."
    else:
        has_skew = pd.notna(skew) and abs(skew) >= 1.0
        has_outlier = pd.notna(outlier_ratio) and outlier_ratio >= 0.08
        if (has_skew or has_outlier) and boundary_ratio <= 0.12:
            strategy = "RobustP5P95Clipped"
            triggered_rule = "Rule3: skewed or outlier-prone distribution with low boundary risk"
            reason = f"Skewness={skew:.3f}, IQROutlierRatio={outlier_ratio:.1%}, BoundaryRatioTotal={boundary_ratio:.1%}; P5-P95 clipped scaling is recommended."
        elif (has_skew or has_outlier) and boundary_ratio > 0.12:
            strategy = "PercentileScalingNoClipping"
            triggered_rule = "Rule4: skewed or outlier-prone distribution with high boundary risk"
            reason = f"Skewness={skew:.3f}, IQROutlierRatio={outlier_ratio:.1%}, BoundaryRatioTotal={boundary_ratio:.1%}; percentile scaling without clipping is recommended."
        else:
            strategy = "SimpleMinMax"
            triggered_rule = "Rule5: default case"
            reason = f"Skewness={skew:.3f}, IQROutlierRatio={outlier_ratio:.1%}, BoundaryRatioTotal={boundary_ratio:.1%}; simple Min-Max normalization is recommended."

    return {
        "SuggestedNormalizationStrategy": strategy,
        "TriggerRule": triggered_rule,
        "Reason": reason,
        "boundary_ratio": boundary_ratio,
    }


def build_triggered_rule_explanation(stats: Dict[str, float], strategy: str) -> str:
    n = stats["SampleCount"]
    skew = stats["Skewness"]
    outlier_ratio = stats["IQROutlierRatio"]
    lower_ratio = stats["BelowP5Ratio"]
    upper_ratio = stats["AboveP95Ratio"]
    boundary_ratio = (
        (lower_ratio if pd.notna(lower_ratio) else 0.0)
        + (upper_ratio if pd.notna(upper_ratio) else 0.0)
    )
    unique_count = stats["UniqueValueCount"]

    if strategy == "SimpleMinMaxOrZScore":
        return f"SampleCount={n} is below 20; percentile estimates may be unstable, so a simple method is recommended."
    if strategy == "SimpleMinMax":
        return f"UniqueValueCount={unique_count}, Skewness={skew:.3f}, IQROutlierRatio={outlier_ratio:.2%}, BoundaryRatioTotal={boundary_ratio:.2%}; simple Min-Max normalization is adequate."
    if strategy == "RobustP5P95Clipped":
        return f"Skewness={skew:.3f}, IQROutlierRatio={outlier_ratio:.2%}, BoundaryRatioTotal={boundary_ratio:.2%}; robust P5-P95 clipped scaling is used."
    if strategy == "PercentileScalingNoClipping":
        return f"Skewness={skew:.3f}, IQROutlierRatio={outlier_ratio:.2%}, BoundaryRatioTotal={boundary_ratio:.2%}; percentile scaling without clipping is used."
    return "No rule explanation available."


def build_paper_method_table(rec_df: pd.DataFrame, stats_df: pd.DataFrame) -> pd.DataFrame:
    """
    Info paper_method_table.xlsx.
    Info:
    1. InfoMappingModelShortName, Info merge Info"ModelShortName/GroupName", InfodifferentModelInfoLanguage+MetricSummary.
    2. stats_df MediumNo"SuggestedNormalizationStrategy"Info, InfoasInfo merge Info.
    """
    merged = rec_df.merge(
        stats_df[
            [
                "ModelShortName",
                "Language",
                "GroupName",
                "Metric",
                "SampleCount",
                "mean",
                "std",
                "BelowP5Ratio",
                "AboveP95Ratio",
                "Skewness",
                "IQROutlierRatio",
            ]
        ],
        on=["ModelShortName", "Language", "GroupName", "Metric", "SampleCount"],
        how="left",
        validate="one_to_one",
    ).copy()

    merged["Triggered Rule"] = merged.apply(
        lambda row: build_triggered_rule_explanation(
            {
                "SampleCount": row["SampleCount"],
                "Skewness": row["Skewness"],
                "IQROutlierRatio": row["IQROutlierRatio"],
                "BelowP5Ratio": row["BelowP5Ratio"],
                "AboveP95Ratio": row["AboveP95Ratio"],
                "UniqueValueCount": np.nan,
            },
            row["SuggestedNormalizationStrategy"],
        ),
        axis=1,
    )

    paper_df = merged[
        [
            "Language",
            "Metric",
            "SampleCount",
            "mean",
            "std",
            "BelowP5Ratio",
            "AboveP95Ratio",
            "Skewness",
            "IQROutlierRatio",
            "SuggestedNormalizationStrategy",
            "Triggered Rule",
        ]
    ].copy()

    paper_df = paper_df.rename(
        columns={
            "Language": "Language",
            "Metric": "Metric",
            "SampleCount": "n",
            "SuggestedNormalizationStrategy": "Recommended Normalization",
        }
    )
    return paper_df


NORMALIZED_METRIC_COLUMNS = {
    "OpenCLIPScore": "OpenCLIPNormScore",
    "AestheticScore": "AestheticNormScore",
    "MUSIQScore": "MUSIQNormScore",
}

WEIGHTED_TOTAL_COLUMN = "WeightedTotalScore"


def safe_divide(numerator: pd.Series, denominator: float) -> pd.Series:
    if pd.isna(denominator) or abs(denominator) < 1e-12:
        return pd.Series(np.zeros(len(numerator)), index=numerator.index, dtype=float)
    return numerator / denominator


def apply_normalization_by_strategy(series: pd.Series, stats: Dict[str, float], strategy: str) -> pd.Series:
    s = series.astype(float)

    min_v = stats["MinValue"]
    max_v = stats["MaxValue"]
    p5 = stats["P5"]
    p95 = stats["P95"]
    mean_v = stats["mean"]
    std_v = stats["std"]

    if strategy in ["SimpleMinMax", "SimpleMinMaxOrZScore"]:
        return safe_divide(s - min_v, max_v - min_v)

    if strategy == "RobustP5P95Clipped":
        clipped = s.clip(lower=p5, upper=p95)
        return safe_divide(clipped - p5, p95 - p5)

    if strategy == "PercentileScalingNoClipping":
        return safe_divide(s - p5, p95 - p5)

    return safe_divide(s - min_v, max_v - min_v)


def add_normalized_scores(df: pd.DataFrame, stats_df: pd.DataFrame, rec_df: pd.DataFrame) -> pd.DataFrame:
    """
    Info"ModelShortName × Language × Metric"InfoSuggestedStrategy, InfoScoreInfo.
    """
    out_df = df.copy()

    stats_lookup = stats_df.set_index(["ModelShortName", "Language", "Metric"]).to_dict("index")
    rec_lookup = rec_df.set_index(["ModelShortName", "Language", "Metric"])["SuggestedNormalizationStrategy"].to_dict()

    for metric, norm_col in NORMALIZED_METRIC_COLUMNS.items():
        out_df[norm_col] = np.nan

        for (model, lang), idx in out_df.groupby(["ModelShortName", "Language"]).groups.items():
            key = (model, lang, metric)
            if key not in stats_lookup or key not in rec_lookup:
                continue

            stats = stats_lookup[key]
            strategy = rec_lookup[key]
            normalized = apply_normalization_by_strategy(out_df.loc[idx, metric], stats, strategy)
            out_df.loc[idx, norm_col] = normalized

    out_df[WEIGHTED_TOTAL_COLUMN] = (
        out_df["OpenCLIPNormScore"] * 0.45
        + out_df["AestheticNormScore"] * 0.35
        + out_df["MUSIQNormScore"] * 0.2
    )

    return out_df


def build_parameter_group_ranking(normalized_df: pd.DataFrame) -> pd.DataFrame:
    """Build parameter-group rankings by model and language."""
    penalty = 0.1

    grouped = (
        normalized_df.groupby(["ModelShortName", "Language", "Resolution", "Steps"], dropna=False)[WEIGHTED_TOTAL_COLUMN]
        .agg(["mean", "std"])
        .reset_index()
        .rename(columns={"mean": "ParameterGroupWeightedMean", "std": "ParameterGroupSD"})
    )

    grouped["ParameterGroupSD"] = grouped["ParameterGroupSD"].fillna(0.0)
    grouped["StabilityAdjustedScore"] = grouped["ParameterGroupWeightedMean"] - penalty * grouped["ParameterGroupSD"]

    grouped["ParameterGroupWeightedMeanRank"] = (
        grouped.groupby(["ModelShortName", "Language"])["ParameterGroupWeightedMean"]
        .rank(method="min", ascending=False)
        .astype(int)
    )

    grouped["StabilityAdjustedRank"] = (
        grouped.groupby(["ModelShortName", "Language"])["StabilityAdjustedScore"]
        .rank(method="min", ascending=False)
        .astype(int)
    )

    zh_df = (
        grouped[grouped["Language"] == "chinese"][[
            "ModelShortName", "Resolution", "Steps", "ParameterGroupWeightedMean",
            "ParameterGroupWeightedMeanRank", "StabilityAdjustedScore", "StabilityAdjustedRank"
        ]]
        .rename(columns={
            "ParameterGroupWeightedMean": "ChineseWeightedMean",
            "ParameterGroupWeightedMeanRank": "ChineseWeightedMeanRank",
            "StabilityAdjustedScore": "ChineseStabilityAdjustedScore",
            "StabilityAdjustedRank": "ChineseStabilityAdjustedRank",
        })
    )

    en_df = (
        grouped[grouped["Language"] == "english"][[
            "ModelShortName", "Resolution", "Steps", "ParameterGroupWeightedMean",
            "ParameterGroupWeightedMeanRank", "StabilityAdjustedScore", "StabilityAdjustedRank"
        ]]
        .rename(columns={
            "ParameterGroupWeightedMean": "EnglishWeightedMean",
            "ParameterGroupWeightedMeanRank": "EnglishWeightedMeanRank",
            "StabilityAdjustedScore": "EnglishStabilityAdjustedScore",
            "StabilityAdjustedRank": "EnglishStabilityAdjustedRank",
        })
    )

    merged = pd.merge(
        zh_df,
        en_df,
        on=["ModelShortName", "Resolution", "Steps"],
        how="outer",
        validate="one_to_one",
    )

    merged = merged[[
        "ModelShortName", "Resolution", "Steps",
        "ChineseWeightedMean", "ChineseWeightedMeanRank", "ChineseStabilityAdjustedScore", "ChineseStabilityAdjustedRank",
        "EnglishWeightedMean", "EnglishWeightedMeanRank", "EnglishStabilityAdjustedScore", "EnglishStabilityAdjustedRank",
    ]].sort_values(
        by=["ModelShortName", "ChineseWeightedMeanRank", "EnglishWeightedMeanRank", "Resolution", "Steps"],
        na_position="last",
    )

    return merged


def analyze_excel(excel_path: Path, output_dir: Path, bins: int = 20) -> None:
    df = pd.read_excel(excel_path)
    ensure_required_columns(df)
    df = coerce_metric_columns(df)

    output_dir.mkdir(parents=True, exist_ok=True)
    hist_root = output_dir / "histograms"
    hist_root.mkdir(exist_ok=True)

    stats_rows: List[Dict[str, object]] = []
    rec_rows: List[Dict[str, object]] = []

    grouped = df.groupby(["ModelShortName", "Language"], dropna=False)

    for (model, lang), group in grouped:
        group_name = f"{model}_{lang}"
        group_dir = hist_root / safe_name(group_name)
        group_dir.mkdir(exist_ok=True)

        for metric in METRICS:
            series = group[metric].dropna()
            stats = compute_stats(series)

            fig_name = f"{safe_name(group_name)}_{safe_name(metric)}.png"
            draw_histogram(
                series=series,
                title=f"{group_name} - {metric}",
                out_path=group_dir / fig_name,
                bins=bins,
            )

            recommendation = recommend_metric_normalization(stats)

            stats_row = {
                "ModelShortName": model,
                "Language": lang,
                "GroupName": group_name,
                "Metric": metric,
                **stats,
                "BoundaryRatioTotal": recommendation["boundary_ratio"],
            }
            stats_rows.append(stats_row)

            rec_row = {
                "ModelShortName": model,
                "Language": lang,
                "GroupName": group_name,
                "Metric": metric,
                "SampleCount": stats["SampleCount"],
                "SuggestedNormalizationStrategy": recommendation["SuggestedNormalizationStrategy"],
                "TriggerRule": recommendation["TriggerRule"],
                "Reason": recommendation["Reason"],
            }
            rec_rows.append(rec_row)

    stats_df = pd.DataFrame(stats_rows)
    rec_df = pd.DataFrame(rec_rows)

    rules_rows = [
        {
            "RuleID": "Rule1",
            "Condition": "SampleCount n < 20",
            "SuggestedStrategy": "SimpleMinMaxOrZScore",
            "Notes": "SmallSample P5/P95 InfoStable, InfoFixed robust normalization.",
        },
        {
            "RuleID": "Rule2",
            "Condition": "UniqueValueCount <= 5",
            "SuggestedStrategy": "SimpleMinMax",
            "Notes": "Info, Info.",
        },
        {
            "RuleID": "Rule3",
            "Condition": "(abs(skew) >= 1.0 Info IQROutlierRatio >= 0.08) and Info <= 0.12",
            "SuggestedStrategy": "RobustP5P95Clipped",
            "Notes": "Info/Info, and boundary risk is low.",
        },
        {
            "RuleID": "Rule4",
            "Condition": "(abs(skew) >= 1.0 or IQROutlierRatio >= 0.08) and BoundaryRatioTotal > 0.12",
            "SuggestedStrategy": "PercentileScalingNoClipping",
            "Notes": "Info/Info, Info.",
        },
        {
            "RuleID": "Rule5",
            "Condition": "Otherwise",
            "SuggestedStrategy": "SimpleMinMax",
            "Notes": "Info, Min-Max Info.",
        },
    ]
    rules_df = pd.DataFrame(rules_rows)

    paper_df = build_paper_method_table(rec_df, stats_df)
    normalized_df = add_normalized_scores(df, stats_df, rec_df)
    parameter_group_df = build_parameter_group_ranking(normalized_df)

    excel_out = output_dir / "summary_statistics.xlsx"
    paper_excel_out = output_dir / "paper_method_table.xlsx"
    normalized_excel_out = output_dir / "normalized_scores.xlsx"
    parameter_group_excel_out = output_dir / "parameter_group_ranking.xlsx"

    with pd.ExcelWriter(excel_out, engine="openpyxl") as writer:
        stats_df.to_excel(writer, index=False, sheet_name="MetricDistributionStats")
        rec_df.to_excel(writer, index=False, sheet_name="NormalizationStrategy")
        rules_df.to_excel(writer, index=False, sheet_name="DecisionRuleNotes")
        paper_df.to_excel(writer, index=False, sheet_name="PaperMethodTable")
        normalized_df.to_excel(writer, index=False, sheet_name="NormalizationDetails")
        parameter_group_df.to_excel(writer, index=False, sheet_name="ParameterGroupRank")

    paper_df.to_excel(paper_excel_out, index=False)
    normalized_df.to_excel(normalized_excel_out, index=False)
    parameter_group_df.to_excel(parameter_group_excel_out, index=False)

    report_out = output_dir / "analysis_report.txt"
    with open(report_out, "w", encoding="utf-8") as f:
        f.write("Calibration objective-score normalization report\n")
        f.write("=" * 100 + "\n\n")
        f.write(f"InputExcel: {excel_path}\n")
        f.write(f"SampleCount: {len(df)}\n")
        f.write(f"ModelLanguageGroups: {df.groupby(['ModelShortName', 'Language']).ngroups}\n")
        f.write(f"MetricCount: {len(METRICS)}\n\n")

        f.write("Info, DecisionRuleNotes\n")
        f.write("-" * 100 + "\n")
        f.write(RULES_TEXT)
        f.write("\n\n")

        f.write("Info, MetricSummary\n")
        f.write("-" * 100 + "\n\n")

        for (group_name, model, lang), sub_df in rec_df.groupby(["GroupName", "ModelShortName", "Language"], sort=False):
            f.write(f"[Info] {group_name}\n")
            f.write(f"ModelShortName:{model}\n")
            f.write(f"Language:{lang}\n")

            metric_stats = stats_df[stats_df["GroupName"] == group_name]

            for _, rec in sub_df.iterrows():
                metric = rec["Metric"]
                stat = metric_stats[metric_stats["Metric"] == metric].iloc[0]

                f.write(f"  Metric:{metric}\n")
                f.write(f"    SampleCount:{rec['SampleCount']}\n")
                f.write(f"    mean={stat['mean']:.6f}, std={stat['std']:.6f}\n")
                f.write(
                    f"    P1={stat['P1']:.6f}, P5={stat['P5']:.6f}, P50={stat['P50']:.6f}, "
                    f"P95={stat['P95']:.6f}, P99={stat['P99']:.6f}\n"
                )
                f.write(
                    f"    BelowP5Ratio={stat['BelowP5Ratio']:.2%}, AboveP95Ratio={stat['AboveP95Ratio']:.2%}, "
                    f"BoundaryRatioTotal={stat['BoundaryRatioTotal']:.2%}\n"
                )
                f.write(
                    f"    Skewness={stat['Skewness']:.6f}, IQROutlierRatio={stat['IQROutlierRatio']:.2%}, "
                    f"UniqueValueCount={stat['UniqueValueCount']}\n"
                )
                f.write(f"    SuggestedNormalizationStrategy:{rec['SuggestedNormalizationStrategy']}\n")
                f.write(f"    TriggerRule:{rec['TriggerRule']}\n")
                f.write(f"    Reason:{rec['Reason']}\n\n")

            f.write("\n")

    rules_out = output_dir / "normalization_rules.txt"
    with open(rules_out, "w", encoding="utf-8") as f:
        f.write(RULES_TEXT)

    print(f"Analysis completed. Output directory: {output_dir}")
    print(f"Summary Excel: {excel_out}")
    print(f"PaperMethodTable Excel:{paper_excel_out}")
    print(f"NormalizationDetails Excel:{normalized_excel_out}")
    print(f"ParameterGroupRank Excel:{parameter_group_excel_out}")
    print(f"Report: {report_out}")
    print(f"InfoNotes:{rules_out}")
    print(f"Info:{hist_root}")


def main() -> None:
    args = parse_args()
    excel_path = Path(args.excel_path).expanduser().resolve()
    if not excel_path.exists():
        raise FileNotFoundError(f"Info:{excel_path}")

    if args.output_dir:
        output_dir = Path(args.output_dir).expanduser().resolve()
    else:
        output_dir = excel_path.parent / f"calibration_analysis"

    analyze_excel(excel_path=excel_path, output_dir=output_dir, bins=args.bins)


if __name__ == "__main__":
    main()
