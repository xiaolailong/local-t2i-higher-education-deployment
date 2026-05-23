#!/usr/bin/env python3
# -*- coding: utf-8 -*-


import argparse
import os
import sys
import time
import subprocess
from pathlib import Path
from typing import Any

import pandas as pd
import requests

from comfy_utils import t2i
from config import COMFY_BASE_URL, COMFY_PORT, MODEL_CONFIG, MODEL_STEPS, RESOLUTIONS, SEEDS

MODEL_ORDER = ("qwen", "zimage", "sdxl")

COMFY_WORKDIR = "/home/chenll/ComfyUI"
COMFY_START_CMD = ["python", "main.py", "--listen", "0.0.0.0", "--port", "8188"]
COMFY_READY_TIMEOUT_SEC = 180
COMFY_STOP_TIMEOUT_SEC = 30


def safe_str(v: Any) -> str:
    if pd.isna(v):
        return ""
    return str(v).strip()


def ensure_exists(path: str) -> None:
    if not os.path.exists(path):
        raise FileNotFoundError(f"File does not exist:{path}")


def is_comfy_ready() -> bool:
    try:
        resp = requests.get(f"{COMFY_BASE_URL}/system_stats", timeout=3)
        return resp.status_code == 200
    except Exception:
        return False


def is_port_open(host: str = "127.0.0.1", port: int = COMFY_PORT) -> bool:
    import socket
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
        sock.settimeout(1.0)
        return sock.connect_ex((host, port)) == 0


def stop_comfyui() -> None:
    try:
        subprocess.run(
            ["pkill", "-f", r"python main\.py --listen 0\.0\.0\.0 --port 8188"],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
            check=False,
        )
    except Exception:
        pass

    start = time.time()
    while time.time() - start < COMFY_STOP_TIMEOUT_SEC:
        if not is_port_open(port=COMFY_PORT):
            return
        time.sleep(1)

    raise RuntimeError("Stopping ComfyUI timed out; port 8188 is still open.")


def start_comfyui() -> None:
    log_path = Path(COMFY_WORKDIR) / "calibration_comfyui_restart.log"
    log_f = open(log_path, "a", encoding="utf-8")

    subprocess.Popen(
        COMFY_START_CMD,
        cwd=COMFY_WORKDIR,
        stdout=log_f,
        stderr=log_f,
        start_new_session=True,
    )

    start = time.time()
    while time.time() - start < COMFY_READY_TIMEOUT_SEC:
        if is_comfy_ready():
            return
        time.sleep(2)

    raise RuntimeError(f"Starting ComfyUI timed out: not ready within {COMFY_READY_TIMEOUT_SEC}s.")


def restart_comfyui() -> None:
    print("=" * 80)
    print("[COMFYUI] restarting ...")
    stop_comfyui()
    start_comfyui()
    print("[COMFYUI] ready")
    print("=" * 80)


def build_prompt_tasks(df):
    tasks = []
    for _, row in df.iterrows():
        pid = safe_str(row["ID"])
        for lang, text in [
            ("chinese", safe_str(row["Chinese Prompt"])),
            ("english", safe_str(row["English Prompt"]))
        ]:
            if text:
                tasks.append((pid, lang, text))
    return tasks


def get_first_task(model, tasks):
    pid, lang, text = tasks[0]
    w, h = RESOLUTIONS[0]
    step = MODEL_STEPS[model][0]
    seed_no = 0
    seed = SEEDS[0]
    return pid, lang, text, w, h, step, seed, seed_no


def warm_once(model, workflow, task, output_dir):
    pid, lang, text, w, h, step, seed, seed_no = task
    print(f"[WARM] {model} ...")
    return t2i(
        model_name=model,
        workflow_json_path=workflow,
        prompt_no=pid,
        prompt_text=text,
        language=lang,
        width=w,
        height=h,
        step=step,
        seed=seed,
        seed_no=seed_no,
        output_dir=output_dir,
        is_warmup=True
    )


def run_batch(excel_path, output_dir):
    ensure_exists(excel_path)
    Path(output_dir).mkdir(parents=True, exist_ok=True)

    df = pd.read_excel(excel_path, sheet_name="prompt")
    df = df[df["Set"].str.lower() == "calibration"]

    tasks = build_prompt_tasks(df)
    if not tasks:
        raise ValueError("Notask")

    restart_comfyui()
    print("=== GREENLIGHT TEST ===")
    for model in MODEL_ORDER:
        wf = MODEL_CONFIG[model]["json_path"]
        task = get_first_task(model, tasks)
        try:
            warm_once(model, wf, task, output_dir)
        except Exception as e:
            raise RuntimeError(f"Green-light test failed: {model} {e}")

    print("=== GREENLIGHT OK ===")

    total = len(tasks) * sum(len(RESOLUTIONS) * len(MODEL_STEPS[m]) * len(SEEDS) for m in MODEL_ORDER)
    done = 0

    for model in MODEL_ORDER:
        wf = MODEL_CONFIG[model]["json_path"]

        restart_comfyui()

        task = get_first_task(model, tasks)
        warm_once(model, wf, task, output_dir)

        for pid, lang, text in tasks:
            for w, h in RESOLUTIONS:
                for step in MODEL_STEPS[model]:
                    for seed_no, seed in enumerate(SEEDS):
                        done += 1
                        print(
                            f"[{done}/{total}] model={model}, ID={pid}, lang={lang}, "
                            f"res={w}x{h}, step={step}, seed_no={seed_no}"
                        )
                        try:
                            img_path, txt_path = t2i(
                                model_name=model,
                                workflow_json_path=wf,
                                prompt_no=pid,
                                prompt_text=text,
                                language=lang,
                                width=w,
                                height=h,
                                step=step,
                                seed=seed,
                                seed_no=seed_no,
                                output_dir=output_dir,
                                is_warmup=False
                            )
                            print(f"    -> image: {img_path}")
                            print(f"    -> monitor: {txt_path}")
                        except Exception as e:
                            print(f"ERROR: {e}", file=sys.stderr)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Calibration batch text-to-image generation script")
    parser.add_argument("excel_path", help="Excel file path")
    parser.add_argument("output_dir", help="output directory")
    return parser.parse_args()


if __name__ == "__main__":
    args = parse_args()
    run_batch(args.excel_path, args.output_dir)
