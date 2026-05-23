#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import argparse
from pathlib import Path
from typing import List

import numpy as np
import pandas as pd


DEFAULT_SHEET = "m2_checklist_image_level"

REQUIRED_COLS = [
    "FileName",
    "ID",
    "Module",
    "Prompt_Group",
    "Source_ID",
    "Prompt_Version",
    "Content",
    "Style",
    "Complexity",
    "Context",
    "PromptText",
    "AddedInformation",
    "seed_no",
    "AddedElementPresent01",
    "AddedConstraintCorrect01",
    "OriginalSubjectPreserved01",
    "OriginalStructureBroken01",
    "ObviousConflictOrOmission01",
    "TeachingUsability",
    "MainIssue",
    "Notes",
]

BINARY_COLS = [
    "AddedElementPresent01",
    "AddedConstraintCorrect01",
    "OriginalSubjectPreserved01",
    "OriginalStructureBroken01",
    "ObviousConflictOrOmission01",
]

USABILITY_VALUES = ["Usable", "NeedsRevision", "NotUsable"]


def safe_str(x) -> str:
    if pd.isna(x):
        return ""
    return str(x).strip()


def validate_columns(df: pd.DataFrame) -> None:
    missing = [c for c in REQUIRED_COLS if c not in df.columns]
    if missing:
        raise ValueError(f"The checklist is missing required columns: {missing}")


def parse_binary(x) -> float:
    """Convert 0/1, NA, blank, yes/no, and true/false values to 0, 1, or NaN."""
    if pd.isna(x):
        return np.nan
    if isinstance(x, (int, float)) and not pd.isna(x):
        if int(x) == 1:
            return 1.0
        if int(x) == 0:
            return 0.0
    s = str(x).strip().lower()
    if s in ["", "na", "n/a", "nan", "none", "notapplicable", "not applicable", "-", "/"]:
        return np.nan
    if s in ["1", "yes", "y", "true", "present", "correct", "preserved", "kept", "included"]:
        return 1.0
    if s in ["0", "no", "n", "false", "absent", "incorrect", "notpreserved", "not preserved", "missing", "noneobserved", "none observed"]:
        return 0.0
    raise ValueError(f"Unrecognized binary value: {x}")


def normalize_usability(x) -> str:
    s = safe_str(x)
    if not s:
        return ""
    compact = s.replace(" ", "").replace("_", "").lower()
    if s in USABILITY_VALUES:
        return s
    if compact in ["usable", "directlyusable", "acceptable"]:
        return "Usable"
    if compact in ["needsrevision", "needsmodification", "needrevision", "needmodification", "partial", "partlyusable"]:
        return "NeedsRevision"
    if compact in ["notusable", "unusable", "notacceptable"]:
        return "NotUsable"
    return s


def clean_checklist(df: pd.DataFrame) -> pd.DataFrame:
    out = df.copy()
    for c in [
        "FileName", "ID", "Module", "Prompt_Group", "Source_ID", "Prompt_Version",
        "Content", "Style", "Complexity", "Context", "PromptText", "AddedInformation",
        "MainIssue", "Notes",
    ]:
        out[c] = out[c].map(safe_str)

    for c in BINARY_COLS:
        out[c + "_Num"] = out[c].map(parse_binary)

    out["TeachingUsabilityNorm"] = out["TeachingUsability"].map(normalize_usability)
    out["TeachingUsableNum"] = out["TeachingUsabilityNorm"].map(
        lambda x: 1.0 if x == "Usable" else (0.0 if x in ["NeedsRevision", "NotUsable"] else np.nan)
    )
    out["NeedsRevisionNum"] = out["TeachingUsabilityNorm"].map(
        lambda x: 1.0 if x == "NeedsRevision" else (0.0 if x in ["Usable", "NotUsable"] else np.nan)
    )
    out["NotUsableNum"] = out["TeachingUsabilityNorm"].map(
        lambda x: 1.0 if x == "NotUsable" else (0.0 if x in ["Usable", "NeedsRevision"] else np.nan)
    )

    return out


def rate(series: pd.Series) -> float:
    valid = series.dropna()
    if valid.empty:
        return np.nan
    return float(valid.mean())


def count_valid(series: pd.Series) -> int:
    return int(series.notna().sum())


def join_issues(series: pd.Series, max_items: int = 5) -> str:
    vals = [safe_str(x) for x in series if safe_str(x)]
    unique = []
    for v in vals:
        if v not in unique:
            unique.append(v)
    if not unique:
        return ""
    if len(unique) > max_items:
        return "; ".join(unique[:max_items]) + f"; and {len(unique) - max_items} more"
    return "; ".join(unique)


def summarize_version(df: pd.DataFrame) -> pd.DataFrame:
    group_cols = [
        "Prompt_Group", "Prompt_Version", "ID", "Source_ID", "Content", "Style", "Complexity", "Context", "AddedInformation"
    ]

    rows = []
    for keys, sub in df.groupby(group_cols, dropna=False):
        row = dict(zip(group_cols, keys))
        row.update({
            "ImageCount": len(sub),
            "AddedInformationResponseRate": rate(sub["AddedElementPresent01_Num"]),
            "AddedConstraintCorrectRate": rate(sub["AddedConstraintCorrect01_Num"]),
            "OriginalSubjectPreservationRate": rate(sub["OriginalSubjectPreserved01_Num"]),
            "OriginalStructureBreakRate": rate(sub["OriginalStructureBroken01_Num"]),
            "ObviousConflictRate": rate(sub["ObviousConflictOrOmission01_Num"]),
            "TeachingUsabilityRate": rate(sub["TeachingUsableNum"]),
            "NeedsRevisionRate": rate(sub["NeedsRevisionNum"]),
            "NotUsableRate": rate(sub["NotUsableNum"]),
            "AddedInformationValidN": count_valid(sub["AddedElementPresent01_Num"]),
            "TeachingUsabilityValidN": count_valid(sub["TeachingUsableNum"]),
            "MainIssueSummary": join_issues(sub["MainIssue"]),
        })
        rows.append(row)

    result = pd.DataFrame(rows)
    return result.sort_values(["Prompt_Group", "Prompt_Version", "ID"])


def summarize_group(df: pd.DataFrame) -> pd.DataFrame:
    group_cols = ["Prompt_Group", "Content", "Style", "Context"]
    rows = []
    for keys, sub in df.groupby(group_cols, dropna=False):
        row = dict(zip(group_cols, keys))
        inc = sub[~sub["Prompt_Version"].astype(str).str.contains("Base", case=False, na=False)].copy()
        row.update({
            "ImageCount": len(sub),
            "IncrementalImageCount": len(inc),
            "AddedInformationResponseRate": rate(inc["AddedElementPresent01_Num"]),
            "AddedConstraintCorrectRate": rate(inc["AddedConstraintCorrect01_Num"]),
            "OriginalSubjectPreservationRate": rate(sub["OriginalSubjectPreserved01_Num"]),
            "OriginalStructureBreakRate": rate(sub["OriginalStructureBroken01_Num"]),
            "ObviousConflictRate": rate(sub["ObviousConflictOrOmission01_Num"]),
            "TeachingUsabilityRate": rate(sub["TeachingUsableNum"]),
            "NeedsRevisionRate": rate(sub["NeedsRevisionNum"]),
            "NotUsableRate": rate(sub["NotUsableNum"]),
            "MainIssueSummary": join_issues(sub["MainIssue"], max_items=8),
        })
        rows.append(row)
    result = pd.DataFrame(rows)
    return result.sort_values("Prompt_Group")


def summarize_overall(df: pd.DataFrame) -> pd.DataFrame:
    inc = df[~df["Prompt_Version"].astype(str).str.contains("Base", case=False, na=False)].copy()
    return pd.DataFrame([
        {
            "AnalysisScope": "Module2OverallAllImages",
            "ImageCount": len(df),
            "PromptGroupCount": df["Prompt_Group"].nunique(),
            "AddedInformationResponseRate": rate(inc["AddedElementPresent01_Num"]),
            "AddedConstraintCorrectRate": rate(inc["AddedConstraintCorrect01_Num"]),
            "OriginalSubjectPreservationRate": rate(df["OriginalSubjectPreserved01_Num"]),
            "OriginalStructureBreakRate": rate(df["OriginalStructureBroken01_Num"]),
            "ObviousConflictRate": rate(df["ObviousConflictOrOmission01_Num"]),
            "TeachingUsabilityRate": rate(df["TeachingUsableNum"]),
            "NeedsRevisionRate": rate(df["NeedsRevisionNum"]),
            "NotUsableRate": rate(df["NotUsableNum"]),
        }
    ])


def build_issue_list(df: pd.DataFrame) -> pd.DataFrame:
    cols = ["FileName", "ID", "Prompt_Group", "Prompt_Version", "AddedInformation", "TeachingUsabilityNorm", "MainIssue", "Notes"]
    issues = df[df["MainIssue"].map(lambda x: bool(safe_str(x)))][cols].copy()
    return issues.sort_values(["Prompt_Group", "Prompt_Version", "ID", "FileName"])


def round_numeric(df: pd.DataFrame, digits: int = 4) -> pd.DataFrame:
    out = df.copy()
    num_cols = out.select_dtypes(include=[np.number]).columns
    out[num_cols] = out[num_cols].round(digits)
    return out


def export_results(output_excel: str, cleaned: pd.DataFrame, version_summary: pd.DataFrame,
                   group_summary: pd.DataFrame, overall_summary: pd.DataFrame, issue_list: pd.DataFrame) -> None:
    Path(output_excel).parent.mkdir(parents=True, exist_ok=True)
    with pd.ExcelWriter(output_excel, engine="openpyxl") as writer:
        round_numeric(cleaned).to_excel(writer, sheet_name="m2_image_cleaned", index=False)
        round_numeric(version_summary).to_excel(writer, sheet_name="m2_version_summary", index=False)
        round_numeric(group_summary).to_excel(writer, sheet_name="m2_group_summary", index=False)
        round_numeric(overall_summary).to_excel(writer, sheet_name="m2_overall_summary", index=False)
        issue_list.to_excel(writer, sheet_name="m2_issue_list", index=False)


def parse_args():
    parser = argparse.ArgumentParser(description="Summarize Module 2 incremental information checklist results.")
    parser.add_argument("checklist_excel", help="Completed checklist Excel file")
    parser.add_argument("output_excel", help="Output summary Excel file")
    parser.add_argument("--sheet", default=DEFAULT_SHEET, help=f"Input sheet name. Default: {DEFAULT_SHEET}")
    return parser.parse_args()


def main():
    args = parse_args()
    df = pd.read_excel(args.checklist_excel, sheet_name=args.sheet)
    validate_columns(df)
    cleaned = clean_checklist(df)
    version_summary = summarize_version(cleaned)
    group_summary = summarize_group(cleaned)
    overall_summary = summarize_overall(cleaned)
    issue_list = build_issue_list(cleaned)
    export_results(args.output_excel, cleaned, version_summary, group_summary, overall_summary, issue_list)

    print("Module 2 checklist summary completed:", args.output_excel)
    print("Output sheets:")
    print("  - m2_image_cleaned     Image-level cleaned results")
    print("  - m2_version_summary   Prompt version/level summary")
    print("  - m2_group_summary     Prompt group summary")
    print("  - m2_overall_summary   Module 2 overall summary")
    print("  - m2_issue_list        Main issue list")


if __name__ == "__main__":
    main()
