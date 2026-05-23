#!/usr/bin/env python3
# -*- coding: utf-8 -*-


import os
import statistics
import subprocess
import threading
import time
from typing import Any, Dict, List, Optional, Tuple

import psutil

from config import COMFY_PORT, RESOURCE_SAMPLE_INTERVAL_SEC


def mean_or_none(values: List[float]) -> Optional[float]:
    if not values:
        return None
    return round(float(statistics.mean(values)), 4)


def max_or_none(values: List[float]) -> Optional[float]:
    if not values:
        return None
    return round(float(max(values)), 4)


def min_or_none(values: List[float]) -> Optional[float]:
    if not values:
        return None
    return round(float(min(values)), 4)


def find_process_by_listen_port(port: int) -> Optional[psutil.Process]:
    try:
        for conn in psutil.net_connections(kind="tcp"):
            if conn.laddr and conn.laddr.port == port and conn.status == psutil.CONN_LISTEN and conn.pid:
                try:
                    return psutil.Process(conn.pid)
                except psutil.Error:
                    continue
    except Exception:
        return None
    return None


class NvidiaSmiSampler:
    def __init__(self, gpu_index: Optional[int] = None) -> None:
        self.gpu_index = gpu_index
        self.available = self._check_available()
        self.selected_gpu_index: Optional[int] = None
        self.selected_gpu_name: Optional[str] = None

    def _check_available(self) -> bool:
        try:
            result = subprocess.run(
                ["nvidia-smi", "--help"],
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL,
                text=True,
                timeout=5,
            )
            return result.returncode == 0
        except Exception:
            return False

    def sample(self) -> Optional[Dict[str, Any]]:
        if not self.available:
            return None

        try:
            result = subprocess.run(
                [
                    "nvidia-smi",
                    "--query-gpu=index,name,utilization.gpu,memory.used,memory.total",
                    "--format=csv,noheader,nounits",
                ],
                capture_output=True,
                text=True,
                timeout=5,
            )
            if result.returncode != 0:
                return None

            lines = [x.strip() for x in result.stdout.splitlines() if x.strip()]
            if not lines:
                return None

            rows = []
            for line in lines:
                parts = [p.strip() for p in line.split(",")]
                if len(parts) < 5:
                    continue
                rows.append({
                    "index": int(parts[0]),
                    "name": parts[1],
                    "utilization_gpu_percent": float(parts[2]),
                    "memory_used_mb": float(parts[3]),
                    "memory_total_mb": float(parts[4]),
                })

            if not rows:
                return None

            if self.gpu_index is not None:
                chosen = next((r for r in rows if r["index"] == self.gpu_index), rows[0])
            else:
                env_visible = os.environ.get("CUDA_VISIBLE_DEVICES", "").strip()
                if env_visible:
                    try:
                        first_visible = int(env_visible.split(",")[0].strip())
                        chosen = next((r for r in rows if r["index"] == first_visible), rows[0])
                    except Exception:
                        chosen = rows[0]
                else:
                    chosen = sorted(rows, key=lambda x: x["index"])[0]

            self.selected_gpu_index = chosen["index"]
            self.selected_gpu_name = chosen["name"]
            return chosen
        except Exception:
            return None


class ResourceMonitor:
    def __init__(self, comfy_port: int = COMFY_PORT, sample_interval: float = RESOURCE_SAMPLE_INTERVAL_SEC):
        self.comfy_port = comfy_port
        self.sample_interval = sample_interval
        self._stop_event = threading.Event()
        self._thread: Optional[threading.Thread] = None

        self.system_cpu_percent: List[float] = []
        self.system_memory_percent: List[float] = []
        self.system_memory_used_mb: List[float] = []

        self.comfy_cpu_percent: List[float] = []
        self.comfy_memory_rss_mb: List[float] = []
        self.comfy_memory_percent: List[float] = []

        self.gpu_utilization_percent: List[float] = []
        self.gpu_memory_used_mb: List[float] = []
        self.gpu_memory_total_mb: List[float] = []

        self.comfy_pid: Optional[int] = None
        self.gpu_index: Optional[int] = None
        self.gpu_name: Optional[str] = None
        self.gpu_metrics_available = False
        self.comfy_process_found = False

        self._comfy_process: Optional[psutil.Process] = None
        self._gpu_sampler = NvidiaSmiSampler()

    def start(self) -> None:
        self._comfy_process = find_process_by_listen_port(self.comfy_port)
        if self._comfy_process is not None:
            self.comfy_pid = self._comfy_process.pid
            self.comfy_process_found = True
            try:
                self._comfy_process.cpu_percent(interval=None)
            except Exception:
                pass

        psutil.cpu_percent(interval=None)

        self._thread = threading.Thread(target=self._run, daemon=True)
        self._thread.start()

    def stop(self) -> None:
        self._stop_event.set()
        if self._thread is not None:
            self._thread.join(timeout=10)

    def _run(self) -> None:
        while not self._stop_event.is_set():
            try:
                vm = psutil.virtual_memory()
                self.system_cpu_percent.append(float(psutil.cpu_percent(interval=None)))
                self.system_memory_percent.append(float(vm.percent))
                self.system_memory_used_mb.append(float(vm.used / 1024 / 1024))

                if self._comfy_process is not None:
                    try:
                        self.comfy_cpu_percent.append(float(self._comfy_process.cpu_percent(interval=None)))
                        mem_info = self._comfy_process.memory_info()
                        self.comfy_memory_rss_mb.append(float(mem_info.rss / 1024 / 1024))
                        self.comfy_memory_percent.append(float(self._comfy_process.memory_percent()))
                    except (psutil.NoSuchProcess, psutil.AccessDenied):
                        self._comfy_process = None

                gpu = self._gpu_sampler.sample()
                if gpu is not None:
                    self.gpu_metrics_available = True
                    self.gpu_index = gpu["index"]
                    self.gpu_name = gpu["name"]
                    self.gpu_utilization_percent.append(float(gpu["utilization_gpu_percent"]))
                    self.gpu_memory_used_mb.append(float(gpu["memory_used_mb"]))
                    self.gpu_memory_total_mb.append(float(gpu["memory_total_mb"]))
            except Exception:
                pass

            time.sleep(self.sample_interval)

    def summary(self, started_at_utc: str, ended_at_utc: str, elapsed_sec: float) -> Dict[str, Any]:
        return {
            "monitor_version": 1,
            "started_at_utc": started_at_utc,
            "ended_at_utc": ended_at_utc,
            "elapsed_seconds": round(float(elapsed_sec), 4),
            "sample_interval_seconds": self.sample_interval,
            "sample_count": max(
                len(self.system_cpu_percent),
                len(self.comfy_cpu_percent),
                len(self.gpu_utilization_percent),
            ),
            "comfyui_process": {
                "found": self.comfy_process_found,
                "pid": self.comfy_pid,
            },
            "system_cpu_percent": {
                "avg": mean_or_none(self.system_cpu_percent),
                "max": max_or_none(self.system_cpu_percent),
                "min": min_or_none(self.system_cpu_percent),
            },
            "system_memory_percent": {
                "avg": mean_or_none(self.system_memory_percent),
                "max": max_or_none(self.system_memory_percent),
                "min": min_or_none(self.system_memory_percent),
            },
            "system_memory_used_mb": {
                "avg": mean_or_none(self.system_memory_used_mb),
                "max": max_or_none(self.system_memory_used_mb),
                "min": min_or_none(self.system_memory_used_mb),
            },
            "comfyui_cpu_percent": {
                "avg": mean_or_none(self.comfy_cpu_percent),
                "max": max_or_none(self.comfy_cpu_percent),
                "min": min_or_none(self.comfy_cpu_percent),
            },
            "comfyui_memory_rss_mb": {
                "avg": mean_or_none(self.comfy_memory_rss_mb),
                "max": max_or_none(self.comfy_memory_rss_mb),
                "min": min_or_none(self.comfy_memory_rss_mb),
            },
            "comfyui_memory_percent": {
                "avg": mean_or_none(self.comfy_memory_percent),
                "max": max_or_none(self.comfy_memory_percent),
                "min": min_or_none(self.comfy_memory_percent),
            },
            "gpu": {
                "metrics_available": self.gpu_metrics_available,
                "index": self.gpu_index,
                "name": self.gpu_name,
                "utilization_percent": {
                    "avg": mean_or_none(self.gpu_utilization_percent),
                    "max": max_or_none(self.gpu_utilization_percent),
                    "min": min_or_none(self.gpu_utilization_percent),
                },
                "memory_used_mb": {
                    "avg": mean_or_none(self.gpu_memory_used_mb),
                    "max": max_or_none(self.gpu_memory_used_mb),
                    "min": min_or_none(self.gpu_memory_used_mb),
                },
                "memory_total_mb": {
                    "avg": mean_or_none(self.gpu_memory_total_mb),
                    "max": max_or_none(self.gpu_memory_total_mb),
                    "min": min_or_none(self.gpu_memory_total_mb),
                },
            },
        }
