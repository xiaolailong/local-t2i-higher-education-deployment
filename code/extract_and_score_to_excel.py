#!/usr/bin/env python3
# -*- coding: utf-8 -*-


from __future__ import annotations

import argparse
import json
import re
import sys
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple, Union

from openpyxl import Workbook
from openpyxl.styles import Alignment, Font, PatternFill
from openpyxl.utils import get_column_letter
from openpyxl.worksheet.worksheet import Worksheet

from objective_score import objective_score

IMAGE_SUFFIXES = {".png", ".jpg", ".jpeg", ".webp", ".bmp"}
FILENAME_PATTERN = re.compile(
    r"^c_"
    r"(?P<model>[^_]+)_"
    r"(?P<prompt_no>[^_]+)_"
    r"(?P<language>chinese|english)_"
    r"(?P<width>\d+)-(?P<height>\d+)_"
    r"step(?P<step>\d+)_"
    r"(?P<seed_no>\d+)"
    r"\.(?P<ext>png|jpg|jpeg|webp|bmp)$",
    re.IGNORECASE,
)


def _smart_cast(value: Any) -> Any:
    if not isinstance(value, str):
        return value

    text = value.strip()
    if text == "":
        return ""

    lower = text.lower()
    if lower in {"none", "null"}:
        return None
    if lower in {"true", "false"}:
        return lower == "true"

    if re.fullmatch(r"[+-]?\d+", text):
        try:
            return int(text)
        except Exception:
            pass

    if re.fullmatch(r"[+-]?\d+(\.\d+)?", text):
        try:
            return float(text)
        except Exception:
            pass

    return text


def _parse_key_value_text(text: str) -> Dict[str, Any]:
    data: Dict[str, Any] = {}

    for raw_line in text.splitlines():
        line = raw_line.strip()
        if not line:
            continue

        m = re.match(r"^([A-Za-z0-9_\-\u4e00-\u9fa5]+)\s*:\s*(.+)$", line)
        if not m:
            m = re.match(r"^([A-Za-z0-9_\-\u4e00-\u9fa5]+)\s*=\s*(.+)$", line)

        if m:
            key = m.group(1).strip()
            value = m.group(2).strip()
            data[key] = _smart_cast(value)

    return data


def _read_sidecar_txt(txt_path: Path) -> Dict[str, Any]:
    text = txt_path.read_text(encoding="utf-8").strip()
    if not text:
        return {}

    try:
        obj = json.loads(text)
        if isinstance(obj, dict):
            return obj
    except Exception:
        pass

    return _parse_key_value_text(text)


def _pick_first_value(data: Dict[str, Any], keys: List[str], default: Any = None) -> Any:
    for key in keys:
        if key in data:
            return data[key]

    lowered = {str(k).lower(): v for k, v in data.items()}
    for key in keys:
        lk = key.lower()
        if lk in lowered:
            return lowered[lk]

    return default


def _to_float(value: Any) -> Optional[float]:
    if value is None:
        return None
    try:
        return float(value)
    except Exception:
        return None


def _get_nested(data: Dict[str, Any], path: str, default: Any = None) -> Any:
    current: Any = data
    for part in path.split("."):
        if not isinstance(current, dict) or part not in current:
            return default
        current = current[part]
    return current


RESOURCE_KEY_CANDIDATES: Dict[str, List[str]] = {
    "vram_usage": [
        "peak_vram_mb",
        "max_vram_mb",
        "peak_vram",
        "max_vram",
        "vram_peak",
        "gpu_memory_peak",
        "gpu_memory_max",
        "vram_usage",
        "vram",
        "gpu_memory",
        "gpu_memory_usage",
        "VRAMPeak",
        "MaxVRAM",
        "VRAMUsage",
        "Info",
    ],
    "gpu_usage": [
        "avg_gpu_usage",
        "avg_gpu_util",
        "avg_gpu_utilization",
        "gpu_usage_avg",
        "gpu_util_avg",
        "gpu_avg",
        "gpu_usage",
        "gpu_util",
        "gpu_utilization",
        "gpu_percent",
        "gpu",
        "MeanGPUUtilization",
        "GPUMeanUsage",
        "GPUUsage",
        "GPUUtilization",
    ],
    "memory_usage": [
        "avg_memory_usage",
        "avg_ram_usage",
        "memory_usage_avg",
        "ram_usage_avg",
        "system_memory_avg",
        "memory_usage",
        "memory",
        "ram_usage",
        "ram",
        "system_memory_usage",
        "MeanMemoryUsage",
        "MemoryMeanUsage",
        "MemoryUsage",
        "Memory",
    ],
    "cpu_usage": [
        "avg_cpu_usage",
        "avg_cpu_percent",
        "cpu_usage_avg",
        "cpu_percent_avg",
        "cpu_avg",
        "cpu_usage",
        "cpu",
        "cpu_percent",
        "MeanCPUUtilization",
        "CPUMeanUsage",
        "CPUUsage",
        "CPUUtilization",
    ],
}

def _first_not_none(*values):
    for v in values:
        if v is not None:
            return v
    return None

def _extract_resource_fields(txt_info: Dict[str, Any]) -> Tuple[Any, Any, Any, Any]:
    vram_usage = _first_not_none(
        _get_nested(txt_info, "monitor.gpu.memory_used_mb.max"),
        _get_nested(txt_info, "monitor.gpu.memory_used_mb.avg"),
        _pick_first_value(txt_info, RESOURCE_KEY_CANDIDATES["vram_usage"], default=None),
    )

    gpu_usage = _first_not_none(
        _get_nested(txt_info, "monitor.gpu.utilization_percent.avg"),
        _get_nested(txt_info, "monitor.gpu.utilization_percent.max"),
        _pick_first_value(txt_info, RESOURCE_KEY_CANDIDATES["gpu_usage"], default=None),
    )

    memory_usage = _first_not_none(
        _get_nested(txt_info, "monitor.comfyui_memory_rss_mb.avg"),
        _get_nested(txt_info, "monitor.system_memory_used_mb.avg"),
        _get_nested(txt_info, "monitor.comfyui_memory_percent.avg"),
        _get_nested(txt_info, "monitor.system_memory_percent.avg"),
        _pick_first_value(txt_info, RESOURCE_KEY_CANDIDATES["memory_usage"], default=None),
    )

    cpu_usage = _first_not_none(
        _get_nested(txt_info, "monitor.comfyui_cpu_percent.avg"),
        _get_nested(txt_info, "monitor.system_cpu_percent.avg"),
        _pick_first_value(txt_info, RESOURCE_KEY_CANDIDATES["cpu_usage"], default=None),
    )

    return vram_usage, gpu_usage, memory_usage, cpu_usage


def exact_and_cal_img_info(
    image_dir: Union[str, Path],
    objective_score_py: Optional[Union[str, Path]] = None,
) -> List[Dict[str, Any]]:
    """
    Extract information for all images in the directory and compute OpenCLIP, Aesthetic, and MUSIQ.
    """
    image_dir = Path(image_dir).expanduser().resolve()
    if not image_dir.exists():
        raise FileNotFoundError(f"Directory does not exist: {image_dir}")
    if not image_dir.is_dir():
        raise NotADirectoryError(f"Not a directory: {image_dir}")

    rows: List[Dict[str, Any]] = []

    for img_path in sorted(image_dir.iterdir()):
        if not img_path.is_file():
            continue
        if img_path.suffix.lower() not in IMAGE_SUFFIXES:
            continue
        if ".warm." in img_path.name:
            continue

        m = FILENAME_PATTERN.match(img_path.name)
        if not m:
            continue

        info = m.groupdict()
        model = info["model"]
        prompt_no = info["prompt_no"]
        language = info["language"].lower()
        width = int(info["width"])
        height = int(info["height"])
        step = int(info["step"])
        seed_no = info["seed_no"]

        txt_path = img_path.with_suffix(".txt")
        if not txt_path.exists():
            raise FileNotFoundError(f"Missing corresponding txt file: {txt_path}")

        txt_info = _read_sidecar_txt(txt_path)

        prompt_text = _pick_first_value(txt_info, ["prompt_text", "prompt", "text"], default=None)
        if not prompt_text:
            raise ValueError(f"Info {txt_path} Mediumprompt_text / prompt was not found")

        elapsed_seconds = _to_float(_get_nested(txt_info, "monitor.elapsed_seconds"))
        if elapsed_seconds is None:
            elapsed_seconds = _to_float(
                _pick_first_value(
                    txt_info,
                    ["elapsed_seconds", "elapsed", "time_seconds", "seconds", "duration", "ElapsedSeconds"],
                    default=None,
                )
            )

        iter_per_second = None
        if elapsed_seconds is not None and elapsed_seconds > 0:
            iter_per_second = step / elapsed_seconds

        vram_usage, gpu_usage, memory_usage, cpu_usage = _extract_resource_fields(txt_info)

        scores = objective_score(
            image_path=str(img_path),
            prompt=prompt_text,
            language=language,
        )

        row = {
            "model": model,
            "prompt_no": prompt_no,
            "language": language,
            "resolution": f"{width}*{height}",
            "step": step,
            "seed_no": seed_no,
            "OpenCLIP": scores.get("OpenCLIP"),
            "Aesthetic": scores.get("Aesthetic"),
            "MUSIQ": scores.get("MUSIQ"),
            "image_size_bytes": img_path.stat().st_size,
            "elapsed_seconds": elapsed_seconds,
            "iter_per_second": iter_per_second,
            "vram_usage": vram_usage,
            "gpu_usage": gpu_usage,
            "memory_usage": memory_usage,
            "cpu_usage": cpu_usage,
            "filename": img_path.name,
        }
        rows.append(row)

    return rows


OUTPUT_HEADERS = [
    ("model", "ModelShortName"),
    ("prompt_no", "PromptID"),
    ("language", "Language"),
    ("resolution", "Resolution"),
    ("step", "Steps"),
    ("seed_no", "SeedNo"),
    ("OpenCLIP", "OpenCLIPScore"),
    ("Aesthetic", "AestheticScore"),
    ("MUSIQ", "MUSIQScore"),
    ("image_size_bytes", "ImageSizeBytes"),
    ("elapsed_seconds", "ElapsedSeconds"),
    ("iter_per_second", "IterPerSecond"),
    ("vram_usage", "VRAMUsage"),
    ("gpu_usage", "GPUUsage"),
    ("memory_usage", "MemoryUsage"),
    ("cpu_usage", "CPUUsage"),
    ("filename", "FileName"),
]


def _set_column_widths(ws: Worksheet) -> None:
    widths = {
        1: 14,
        2: 12,
        3: 12,
        4: 14,
        5: 8,
        6: 14,
        7: 16,
        8: 16,
        9: 14,
        10: 18,
        11: 14,
        12: 18,
        13: 16,
        14: 14,
        15: 14,
        16: 12,
        17: 48,
    }
    for col_idx, width in widths.items():
        ws.column_dimensions[get_column_letter(col_idx)].width = width


def save_results_to_excel(rows: List[Dict[str, Any]], output_path: Union[str, Path]) -> Path:
    output_path = Path(output_path).expanduser().resolve()

    wb = Workbook()
    ws = wb.active
    ws.title = "image_scores"
    ws.freeze_panes = "A2"
    last_col = get_column_letter(len(OUTPUT_HEADERS))
    ws.auto_filter.ref = f"A1:{last_col}{max(1, len(rows) + 1)}"

    header_fill = PatternFill("solid", fgColor="1F4E78")
    header_font = Font(color="FFFFFF", bold=True)
    center_alignment = Alignment(horizontal="center", vertical="center")
    top_alignment = Alignment(vertical="top", wrap_text=True)

    for col_idx, (_, header_name) in enumerate(OUTPUT_HEADERS, start=1):
        cell = ws.cell(row=1, column=col_idx, value=header_name)
        cell.fill = header_fill
        cell.font = header_font
        cell.alignment = center_alignment

    for row_idx, row_data in enumerate(rows, start=2):
        for col_idx, (key, _) in enumerate(OUTPUT_HEADERS, start=1):
            value = row_data.get(key)
            if key == "seed_no" and value is not None:
                cell = ws.cell(row=row_idx, column=col_idx, value=str(value))
                cell.number_format = "@"
            else:
                cell = ws.cell(row=row_idx, column=col_idx, value=value)
            cell.alignment = top_alignment

    for row_idx in range(2, len(rows) + 2):
        ws.cell(row=row_idx, column=7).number_format = "0.0000"
        ws.cell(row=row_idx, column=8).number_format = "0.0000"
        ws.cell(row=row_idx, column=9).number_format = "0.0000"
        ws.cell(row=row_idx, column=11).number_format = "0.000"
        ws.cell(row=row_idx, column=12).number_format = "0.000"

    ws.row_dimensions[1].height = 22
    _set_column_widths(ws)

    note_ws = wb.create_sheet("Notes")
    note_ws["A1"] = "FieldNotes"
    note_ws["A1"].font = Font(bold=True)
    note_ws["A3"] = "IterPerSecond"
    note_ws["B3"] = "Number of sampling steps executed per second. It is calculated as steps / elapsed_seconds."
    note_ws["A4"] = "VRAMUsage"
    note_ws["B4"] = "Peak VRAM usage is used by default. If the peak VRAM field is not available in the txt file, the generic VRAM field is used instead."
    note_ws["A5"] = "GPUUsage / MemoryUsage / CPUUsage"
    note_ws["B5"] = "Average utilization is used by default. If the average value field is not available in the txt file, the generic field is used instead."
    note_ws["A6"] = "SeedNo"
    note_ws["B6"] = "Parsed from the image filename and written to Excel as text to avoid automatic scientific notation."
    note_ws["A7"] = "FileName"
    note_ws["B7"] = "Only the base filename is retained, without the path."
    note_ws.column_dimensions["A"].width = 28
    note_ws.column_dimensions["B"].width = 92
    for cell in ["B3", "B4", "B5", "B6", "B7"]:
        note_ws[cell].alignment = Alignment(wrap_text=True, vertical="top")

    wb.save(output_path)
    return output_path


def process_image_dir_to_excel(
    image_dir: Union[str, Path],
    objective_score_py: Optional[Union[str, Path]] = None,
    output_filename: str = "image_info_scores.xlsx",
) -> Path:
    image_dir = Path(image_dir).expanduser().resolve()
    rows = exact_and_cal_img_info(image_dir=image_dir, objective_score_py=objective_score_py)
    output_path = image_dir / output_filename
    return save_results_to_excel(rows, output_path)


def main() -> int:
    parser = argparse.ArgumentParser(description="Excel")
    parser.add_argument("image_dir", help="Info")
    parser.add_argument(
        "--objective-score-py",
        default=None,
        help="objective_score.py Info; Info, InfowhenInfo",
    )
    parser.add_argument(
        "--output-filename",
        default="image_info_scores.xlsx",
        help="Excel FileName, Info image_info_scores.xlsx",
    )
    args = parser.parse_args()

    output_path = process_image_dir_to_excel(
        image_dir=args.image_dir,
        objective_score_py=args.objective_score_py,
        output_filename=args.output_filename,
    )
    print(f"Excel: {output_path}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
