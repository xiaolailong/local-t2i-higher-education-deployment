#!/usr/bin/env python3
# -*- coding: utf-8 -*-


import argparse
import random
import re
import shutil
from collections import defaultdict, Counter
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List, Optional, Tuple

import pandas as pd


@dataclass
class SelectedItem:
    img_path: Path
    txt_path: Path
    model: str
    prompt_no: str
    language: str


FILENAME_RE = re.compile(
    r"^c_"
    r"(?P<model>.+?)_"
    r"(?P<promptNo>[^_]+)_"
    r"(?P<language>chinese|english)_"
    r"(?P<width>\d+)-(?P<height>\d+)_"
    r"step(?P<step>\d+)_"
    r"(?P<seedNo>[^_\.]+)"
    r"\.png$",
    re.IGNORECASE,
)


def parse_filename(filename: str) -> Optional[Dict[str, str]]:
    match = FILENAME_RE.match(filename)
    if not match:
        return None
    return match.groupdict()


def collect_groups(input_dir: Path):
    groups = defaultdict(list)

    for img in input_dir.iterdir():
        if not img.is_file():
            continue
        if img.suffix.lower() != ".png":
            continue
        if ".warm." in img.name:
            continue

        parsed = parse_filename(img.name)
        if parsed is None:
            print(f"[WARN] skipped invalid image filename: {img.name}")
            continue

        key = (
            parsed["model"],
            parsed["promptNo"],
            parsed["language"].lower(),
        )
        groups[key].append(img)

    return groups


def select_one(groups, rng):
    selected = []

    for (model, prompt_no, language), candidates in groups.items():
        chosen = rng.choice(candidates)

        txt = chosen.with_suffix(".txt")
        if not txt.exists():
            print(f"[WARN] missing metadata txt for: {chosen.name}")
            continue

        selected.append(
            SelectedItem(
                img_path=chosen,
                txt_path=txt,
                model=model,
                prompt_no=prompt_no,
                language=language,
            )
        )

        print(
            f"[SELECT] {model} | {prompt_no} | {language} | "
            f"{len(candidates)} -> {chosen.name}"
        )

    return selected


def split_by_language(items):
    groups = {"chinese": [], "english": []}

    for item in items:
        groups[item.language].append(item)

    return groups


def ensure_dirs(input_dir):
    base = input_dir / "subjective"
    base.mkdir(exist_ok=True)

    dirs = {}
    for lang in ["chinese", "english"]:
        d = base / lang
        d.mkdir(exist_ok=True)
        dirs[lang] = d

    return dirs


def move_item(item, dst):
    shutil.move(str(item.img_path), dst / item.img_path.name)
    shutil.move(str(item.txt_path), dst / item.txt_path.name)


def print_stats(groups):
    print("\n========== Summary ==========")
    for lang, items in groups.items():
        model_counter = Counter(i.model for i in items)
        print(f"\n[{lang}] selected images: {len(items)}")
        print("Model distribution:", dict(model_counter))

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("input_dir")
    parser.add_argument("--seed", type=int, default=None)
    parser.add_argument("--dry-run", action="store_true")

    args = parser.parse_args()

    input_dir = Path(args.input_dir)
    rng = random.Random(args.seed)

    print("[INFO] Scanning input directory...")
    groups = collect_groups(input_dir)

    print(f"[INFO] Found {len(groups)} model-prompt-language groups")

    selected = select_one(groups, rng)

    print(f"[INFO] Selected {len(selected)} images")

    lang_groups = split_by_language(selected)

    print_stats(lang_groups)

    if args.dry_run:
        print("[INFO] dry-run: files were not moved")
        return

    dirs = ensure_dirs(input_dir)

    for lang, items in lang_groups.items():
        for item in items:
            move_item(item, dirs[lang])

    print("\n[DONE] Sampling completed")


if __name__ == "__main__":
    main()