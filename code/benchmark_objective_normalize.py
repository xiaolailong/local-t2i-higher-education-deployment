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
    "SelectedForSubjectiveRating",
]

METRICS = ["OpenCLIPScore", "AestheticScore", "MUSIQScore"]

NORMALIZED_METRIC_COLUMNS = {
    "OpenCLIPScore": "OpenCLIPNormScore",
    "AestheticScore": "AestheticNormScore",
    "MUSIQScore": "MUSIQNormScore",
}

REVERSE_NORMALIZED_COLUMNS = {
    "ElapsedSeconds": "ElapsedSecondsInverseNormScore",
    "VRAMUsage": "VRAMUsageInverseNormScore",
}

WEIGHTED_TOTAL_COLUMN = "WeightedTotalScore"

RULES_TEXT = """\
Normalization strategy rules
================================================================================

Analysis unit
-------------
The Benchmark objective-score normalization is diagnosed by Language and Metric.
OpenCLIP, Aesthetic, and MUSIQ are normalized separately within each language group.
ElapsedSeconds and VRAMUsage are reverse-normalized so that higher normalized values indicate lower cost.

Rules
-----
1. If n < 20, use SimpleMinMax.
2. If UniqueValueCount <= 5, use SimpleMinMax.
3. If skewness or IQR outliers are substantial and boundary flattening risk is low, use RobustP5P95Clipped.
4. If skewness or IQR outliers are substantial and boundary flattening risk is high, use PercentileScalingNoClipping.
5. Otherwise, use SimpleMinMax.
"""


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Analyze objective-score distributions and normalize metrics for the Benchmark experiment.")
    parser.add_argument("excel_path", help="Input objective-score Excel file path")
    parser.add_argument(
        "--output-dir",
        default=None,
        help="Output directory. By default, create a benchmark_objective_analysis folder next to the input Excel file.",
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
    numeric_cols = [
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
    ]
    for col in numeric_cols:
        out[col] = pd.to_numeric(out[col], errors="coerce")
    return out


def normalize_language_value(v: object) -> str:
    if pd.isna(v):
        return "unknown"
    text = str(v).strip().lower()
    mapping = {
        "Chinese": "chinese",
        "Chinese": "chinese",
        "chinese": "chinese",
        "cn": "chinese",
        "zh": "chinese",
        "English": "english",
        "English": "english",
        "english": "english",
        "en": "english",
    }
    return mapping.get(text, str(v).strip())


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
        return {
            "SuggestedNormalizationStrategy": "SimpleMinMax",
            "TriggerRule": "Rule1: small sample (n < 20)",
            "Reason": f"The metric has only {n} valid samples; P5/P95 estimates may be unstable, so simple Min-Max normalization is used.",
            "BoundaryRatioTotal": boundary_ratio,
        }

    if unique_count <= 5:
        return {
            "SuggestedNormalizationStrategy": "SimpleMinMax",
            "TriggerRule": "Rule2: low value diversity (UniqueValueCount <= 5)",
            "Reason": f"The metric has only {unique_count} unique values; simple Min-Max normalization is sufficient.",
            "BoundaryRatioTotal": boundary_ratio,
        }

    has_skew = pd.notna(skew) and abs(skew) >= 1.0
    has_outlier = pd.notna(outlier_ratio) and outlier_ratio >= 0.08
    if (has_skew or has_outlier) and boundary_ratio <= 0.12:
        return {
            "SuggestedNormalizationStrategy": "RobustP5P95Clipped",
            "TriggerRule": "Rule3: skewed or outlier-prone distribution with low boundary risk",
            "Reason": f"Skewness={skew:.3f}, IQROutlierRatio={outlier_ratio:.1%}, BoundaryRatioTotal={boundary_ratio:.1%}; P5-P95 clipped scaling is used.",
            "BoundaryRatioTotal": boundary_ratio,
        }

    if (has_skew or has_outlier) and boundary_ratio > 0.12:
        return {
            "SuggestedNormalizationStrategy": "PercentileScalingNoClipping",
            "TriggerRule": "Rule4: skewed or outlier-prone distribution with high boundary risk",
            "Reason": f"Skewness={skew:.3f}, IQROutlierRatio={outlier_ratio:.1%}, BoundaryRatioTotal={boundary_ratio:.1%}; percentile scaling without clipping is used.",
            "BoundaryRatioTotal": boundary_ratio,
        }

    return {
        "SuggestedNormalizationStrategy": "SimpleMinMax",
        "TriggerRule": "Rule5: default case",
        "Reason": f"Skewness={skew:.3f}, IQROutlierRatio={outlier_ratio:.1%}, BoundaryRatioTotal={boundary_ratio:.1%}; simple Min-Max normalization is used.",
        "BoundaryRatioTotal": boundary_ratio,
    }


def summarize_group_strategy(group_rec_df: pd.DataFrame) -> Dict[str, str]:
    strategies = group_rec_df["SuggestedNormalizationStrategy"].tolist()
    unique_strategies = list(dict.fromkeys(strategies))

    if len(unique_strategies) == 1:
        conclusion = f"All metrics in this language group use {unique_strategies[0]}."
    else:
        conclusion = "Different metrics in this language group use different normalization strategies."

    detail = "; ".join(
        f"{row['Metric']}: {row['SuggestedNormalizationStrategy']}" for _, row in group_rec_df.iterrows()
    )

    return {
        "LanguageSummary": conclusion,
        "MetricSummary": detail,
    }


def safe_divide(series: pd.Series, denominator: float) -> pd.Series:
    if pd.isna(denominator) or abs(denominator) < 1e-12:
        return pd.Series(np.zeros(len(series)), index=series.index, dtype=float)
    return series / denominator


def reverse_minmax_normalize(series: pd.Series) -> pd.Series:
    s = series.astype(float)
    min_v = s.min(skipna=True)
    max_v = s.max(skipna=True)
    return safe_divide(max_v - s, max_v - min_v)


def apply_normalization_by_strategy(series: pd.Series, stats: Dict[str, float], strategy: str) -> pd.Series:
    s = series.astype(float)

    min_v = stats["MinValue"]
    max_v = stats["MaxValue"]
    p5 = stats["P5"]
    p95 = stats["P95"]

    if strategy == "SimpleMinMax":
        return safe_divide(s - min_v, max_v - min_v)

    if strategy == "RobustP5P95Clipped":
        clipped = s.clip(lower=p5, upper=p95)
        return safe_divide(clipped - p5, p95 - p5)

    if strategy == "PercentileScalingNoClipping":
        return safe_divide(s - p5, p95 - p5)

    return safe_divide(s - min_v, max_v - min_v)


def add_normalized_scores(df: pd.DataFrame, stats_df: pd.DataFrame, rec_df: pd.DataFrame) -> pd.DataFrame:
    out_df = df.copy()

    stats_lookup = stats_df.set_index(["Language", "Metric"]).to_dict("index")
    rec_lookup = rec_df.set_index(["Language", "Metric"])["SuggestedNormalizationStrategy"].to_dict()

    for metric, norm_col in NORMALIZED_METRIC_COLUMNS.items():
        out_df[norm_col] = np.nan

        for lang, idx in out_df.groupby("Language", dropna=False).groups.items():
            key = (lang, metric)
            if key not in stats_lookup or key not in rec_lookup:
                continue

            stats = stats_lookup[key]
            strategy = rec_lookup[key]
            normalized = apply_normalization_by_strategy(out_df.loc[idx, metric], stats, strategy)
            out_df.loc[idx, norm_col] = normalized

    for metric, norm_col in REVERSE_NORMALIZED_COLUMNS.items():
        out_df[norm_col] = np.nan

        for lang, idx in out_df.groupby("Language", dropna=False).groups.items():
            normalized = reverse_minmax_normalize(out_df.loc[idx, metric])
            out_df.loc[idx, norm_col] = normalized

    out_df[WEIGHTED_TOTAL_COLUMN] = (
        out_df["OpenCLIPNormScore"] * 0.45
        + out_df["AestheticNormScore"] * 0.35
        + out_df["MUSIQNormScore"] * 0.20
    )

    return out_df


def validate_normalized_scores(normalized_df: pd.DataFrame) -> None:
    expected_cols = list(NORMALIZED_METRIC_COLUMNS.values()) + list(REVERSE_NORMALIZED_COLUMNS.values()) + [WEIGHTED_TOTAL_COLUMN]
    missing_cols = [col for col in expected_cols if col not in normalized_df.columns]
    if missing_cols:
        raise ValueError("Missing normalized-score columns:\n- " + "\n- ".join(missing_cols))

    for col in list(NORMALIZED_METRIC_COLUMNS.values()) + list(REVERSE_NORMALIZED_COLUMNS.values()):
        if normalized_df[col].isna().all():
            raise ValueError(f"Normalization failed: column {col} is entirely empty. Please check group keys and strategies.")


def analyze_excel(excel_path: Path, output_dir: Path, bins: int = 20) -> None:
    df = pd.read_excel(excel_path)
    ensure_required_columns(df)
    df = coerce_metric_columns(df)
    df["Language"] = df["Language"].apply(normalize_language_value)

    output_dir.mkdir(parents=True, exist_ok=True)
    hist_root = output_dir / "histograms"
    hist_root.mkdir(exist_ok=True)

    stats_rows: List[Dict[str, object]] = []
    rec_rows: List[Dict[str, object]] = []
    group_summary_rows: List[Dict[str, object]] = []

    grouped = df.groupby("Language", dropna=False)

    for lang, group in grouped:
        group_name = f"language_{lang}"
        group_dir = hist_root / safe_name(group_name)
        group_dir.mkdir(exist_ok=True)

        current_group_recs = []

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
                "Language": lang,
                "GroupName": group_name,
                "Metric": metric,
                **stats,
                "BoundaryRatioTotal": recommendation["BoundaryRatioTotal"],
            }
            stats_rows.append(stats_row)

            rec_row = {
                "Language": lang,
                "GroupName": group_name,
                "Metric": metric,
                "SampleCount": stats["SampleCount"],
                "SuggestedNormalizationStrategy": recommendation["SuggestedNormalizationStrategy"],
                "TriggerRule": recommendation["TriggerRule"],
                "Reason": recommendation["Reason"],
            }
            rec_rows.append(rec_row)
            current_group_recs.append(rec_row)

        group_summary = summarize_group_strategy(pd.DataFrame(current_group_recs))
        group_summary_rows.append(
            {
                "Language": lang,
                "GroupName": group_name,
                **group_summary,
            }
        )

    stats_df = pd.DataFrame(stats_rows)
    rec_df = pd.DataFrame(rec_rows)
    group_summary_df = pd.DataFrame(group_summary_rows)
    normalized_df = add_normalized_scores(df, stats_df, rec_df)
    validate_normalized_scores(normalized_df)

    rules_rows = [
        {"RuleID": "Rule1", "Condition": "SampleCount n < 20", "SuggestedStrategy": "SimpleMinMax", "Notes": "Use simple Min-Max normalization because percentile estimates may be unstable in small samples."},
        {"RuleID": "Rule2", "Condition": "UniqueValueCount <= 5", "SuggestedStrategy": "SimpleMinMax", "Notes": "Use simple Min-Max normalization because the metric has low value diversity."},
        {"RuleID": "Rule3", "Condition": "(abs(skew) >= 1.0 or IQROutlierRatio >= 0.08) and BoundaryRatioTotal <= 0.12", "SuggestedStrategy": "RobustP5P95Clipped", "Notes": "Use clipped P5-P95 robust scaling when skew or outliers are present and clipping risk is low."},
        {"RuleID": "Rule4", "Condition": "(abs(skew) >= 1.0 or IQROutlierRatio >= 0.08) and BoundaryRatioTotal > 0.12", "SuggestedStrategy": "PercentileScalingNoClipping", "Notes": "Use percentile scaling without clipping when boundary compression risk is high."},
        {"RuleID": "Rule5", "Condition": "Otherwise", "SuggestedStrategy": "SimpleMinMax", "Notes": "Use simple Min-Max normalization for stable distributions."},
    ]
    rules_df = pd.DataFrame(rules_rows)

    excel_out = output_dir / "summary_statistics.xlsx"
    normalized_excel_out = output_dir / "normalized_scores.xlsx"

    with pd.ExcelWriter(excel_out, engine="openpyxl") as writer:
        stats_df.to_excel(writer, index=False, sheet_name="MetricDistributionStats")
        rec_df.to_excel(writer, index=False, sheet_name="NormalizationStrategy")
        group_summary_df.to_excel(writer, index=False, sheet_name="LanguageSummary")
        rules_df.to_excel(writer, index=False, sheet_name="DecisionRuleNotes")
        normalized_df.to_excel(writer, index=False, sheet_name="NormalizationDetails")

    normalized_df.to_excel(normalized_excel_out, index=False)

    report_out = output_dir / "analysis_report.txt"
    with open(report_out, "w", encoding="utf-8") as f:
        f.write("Benchmark objective-score normalization report\n")
        f.write("=" * 100 + "\n\n")
        f.write(f"InputExcel: {excel_path}\n")
        f.write(f"SampleCount: {len(df)}\n")
        f.write(f"LanguageGroups: {df.groupby(['Language']).ngroups}\n")
        f.write(f"MetricCount: {len(METRICS)}\n\n")

        f.write("1. Decision rule notes\n")
        f.write("-" * 100 + "\n")
        f.write(RULES_TEXT)
        f.write("\n\n")

        f.write("2. Language-level summary\n")
        f.write("-" * 100 + "\n\n")
        for _, row in group_summary_df.iterrows():
            f.write(f"[LanguageSummary] {row['Language']}\n")
            f.write(f"Summary: {row['LanguageSummary']}\n")
            f.write(f"MetricSummary: {row['MetricSummary']}\n\n")

        f.write("3. Metric-level suggestions by language\n")
        f.write("-" * 100 + "\n\n")

        for lang in rec_df["Language"].drop_duplicates().tolist():
            f.write(f"[LanguageSummary] {lang}\n")
            sub_rec = rec_df[rec_df["Language"] == lang]
            sub_stats = stats_df[stats_df["Language"] == lang]

            for _, rec in sub_rec.iterrows():
                metric = rec["Metric"]
                stat = sub_stats[sub_stats["Metric"] == metric].iloc[0]

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

    rules_out = output_dir / "normalization_rules.txt"
    with open(rules_out, "w", encoding="utf-8") as f:
        f.write(RULES_TEXT)

    print(f"Analysis completed. Output directory: {output_dir}")
    print(f"Summary Excel: {excel_out}")
    print(f"NormalizationDetails Excel:{normalized_excel_out}")
    print(f"Report: {report_out}")
    print(f"Rule notes: {rules_out}")
    print(f"Histogram directory: {hist_root}")


def main() -> None:
    args = parse_args()
    excel_path = Path(args.excel_path).expanduser().resolve()
    if not excel_path.exists():
        raise FileNotFoundError(f"Input file not found: {excel_path}")

    if args.output_dir:
        output_dir = Path(args.output_dir).expanduser().resolve()
    else:
        output_dir = excel_path.parent / "benchmark_objective_analysis"

    analyze_excel(excel_path=excel_path, output_dir=output_dir, bins=args.bins)


if __name__ == "__main__":
    main()
