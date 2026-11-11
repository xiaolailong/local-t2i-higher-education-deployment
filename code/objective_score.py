#!/usr/bin/env python3
# -*- coding: utf-8 -*-


from __future__ import annotations

import os
from dataclasses import dataclass
from pathlib import Path

# ============================================================
# Objective evaluation model directory settings
# Keep all objective-evaluation models and caches under models/
# ============================================================

BASE_DIR = Path(__file__).resolve().parent

if (BASE_DIR / "models").exists():
    PROJECT_DIR = BASE_DIR
else:
    PROJECT_DIR = BASE_DIR.parent

MODELS_DIR = PROJECT_DIR / "models"

# Hugging Face / Transformers cache:
# used by ImageReward and VQAScore
HF_CACHE_DIR = MODELS_DIR / "hf_cache"
os.environ.setdefault("HF_HOME", str(HF_CACHE_DIR))
os.environ.setdefault("HUGGINGFACE_HUB_CACHE", str(HF_CACHE_DIR / "hub"))
os.environ.setdefault("TRANSFORMERS_CACHE", str(HF_CACHE_DIR / "transformers"))

# PyTorch / pyiqa cache:
# used by MUSIQScore
TORCH_CACHE_DIR = MODELS_DIR / "torch_cache"
os.environ.setdefault("TORCH_HOME", str(TORCH_CACHE_DIR))

# HPS v2.1 cache
HPS_ROOT_DIR = MODELS_DIR / "hpsv2"
os.environ.setdefault("HPS_ROOT", str(HPS_ROOT_DIR))

# Avoid tokenizer parallelism warnings
os.environ.setdefault("TOKENIZERS_PARALLELISM", "false")

# Optional: create directories automatically
HF_CACHE_DIR.mkdir(parents=True, exist_ok=True)
TORCH_CACHE_DIR.mkdir(parents=True, exist_ok=True)
HPS_ROOT_DIR.mkdir(parents=True, exist_ok=True)
(MODELS_DIR / "cnclip").mkdir(parents=True, exist_ok=True)

from typing import Dict, Literal, Optional
from urllib.request import urlretrieve

import torch
import torch.nn as nn
from PIL import Image
from transformers import CLIPModel, CLIPProcessor

try:
    import open_clip
except Exception as e:  # pragma: no cover
    raise ImportError("is not installed open_clip_torch.please run: pip install open_clip_torch") from e

try:
    import pyiqa
except Exception as e:  # pragma: no cover
    raise ImportError("is not installed pyiqa.please run: pip install pyiqa") from e

Language = Literal["chinese", "english", "zh", "en"]


BASE_DIR = Path(__file__).resolve().parent
MODELS_DIR = BASE_DIR.parent / "models"

LOCAL_FILES_ONLY = True

DEFAULT_OPENCLIP_MODEL_NAME = "ViT-H-14"
DEFAULT_OPENCLIP_PRETRAINED = str(MODELS_DIR / "openclip" / "open_clip_pytorch_model.bin")

DEFAULT_AESTHETIC_CLIP_MODEL = str(MODELS_DIR / "clip-vit-large-patch14")
DEFAULT_AESTHETIC_HEAD_URL = (
    "https://github.com/LAION-AI/aesthetic-predictor/raw/main/"
    "sa_0_4_vit_l_14_linear.pth"
)
DEFAULT_AESTHETIC_HEAD_PATH = str(
    MODELS_DIR / "aesthetic" / "sa_0_4_vit_l_14_linear.pth"
)
DEFAULT_MUSIQ_METRIC = "musiq"

_OPENCLIP_MODEL = None
_OPENCLIP_PREPROCESS = None
_OPENCLIP_TOKENIZER = None

_AESTHETIC_CLIP_MODEL_OBJ = None
_AESTHETIC_CLIP_PROCESSOR = None
_AESTHETIC_HEAD = None

_MUSIQ_METRIC = None


@dataclass
class ObjectiveScoreResult:
    openclip: float
    aesthetic: float
    musiq: float

    def to_dict(self) -> Dict[str, float]:
        return {
            "OpenCLIP": self.openclip,
            "Aesthetic": self.aesthetic,
            "MUSIQ": self.musiq,
        }


def _get_device(device: Optional[str] = None) -> torch.device:
    if device:
        return torch.device(device)
    return torch.device("cuda" if torch.cuda.is_available() else "cpu")


def _normalize_language(language: str) -> str:
    lang = language.strip().lower()
    if lang in {"chinese", "zh", "cn"}:
        return "chinese"
    if lang in {"english", "en"}:
        return "english"
    raise ValueError(f"Unsupported language: {language!r}, please use chinese/english Info zh/en.")


def _load_image_rgb(image_path: str | Path) -> Image.Image:
    return Image.open(image_path).convert("RGB")


def _l2_normalize(x: torch.Tensor) -> torch.Tensor:
    return x / x.norm(dim=-1, keepdim=True).clamp(min=1e-12)


def _resolve_path_if_exists(path_or_name: str) -> str:
    p = Path(path_or_name)
    return str(p) if p.exists() else path_or_name


def _require_exists(path_str: str, what: str) -> str:
    path = Path(path_str)
    if not path.exists():
        raise FileNotFoundError(
            f"{what} not found: {path}\n"
            f"Please check that objective_score.py and the models/ directory keep the following relative positions:\n"
            f"{BASE_DIR}/objective_score.py\n"
            f"{MODELS_DIR}/openclip/open_clip_pytorch_model.bin\n"
            f"{MODELS_DIR}/clip-vit-large-patch14/\n"
            f"{MODELS_DIR}/aesthetic/sa_0_4_vit_l_14_linear.pth"
        )
    return str(path)


def _get_openclip(device: torch.device):
    global _OPENCLIP_MODEL, _OPENCLIP_PREPROCESS, _OPENCLIP_TOKENIZER
    if _OPENCLIP_MODEL is None or _OPENCLIP_PREPROCESS is None or _OPENCLIP_TOKENIZER is None:
        model_name = DEFAULT_OPENCLIP_MODEL_NAME
        pretrained = _require_exists(DEFAULT_OPENCLIP_PRETRAINED, "OpenCLIP weights")

        if LOCAL_FILES_ONLY:
            os.environ.setdefault("HF_HUB_OFFLINE", "1")
            os.environ.setdefault("TRANSFORMERS_OFFLINE", "1")

        _OPENCLIP_MODEL, _, _OPENCLIP_PREPROCESS = open_clip.create_model_and_transforms(
            model_name=model_name,
            pretrained=pretrained,
            device=device,
        )
        _OPENCLIP_MODEL.eval()
        _OPENCLIP_TOKENIZER = open_clip.get_tokenizer(model_name)

    _OPENCLIP_MODEL.to(device)
    return _OPENCLIP_MODEL, _OPENCLIP_PREPROCESS, _OPENCLIP_TOKENIZER


def _compute_openclip(image: Image.Image, prompt: str, device: torch.device) -> float:
    model, preprocess, tokenizer = _get_openclip(device)
    image_tensor = preprocess(image).unsqueeze(0).to(device)
    text_tensor = tokenizer([prompt]).to(device)

    with torch.no_grad():
        image_features = model.encode_image(image_tensor)
        text_features = model.encode_text(text_tensor)
        image_features = _l2_normalize(image_features)
        text_features = _l2_normalize(text_features)
        cosine = torch.sum(image_features * text_features, dim=-1).item()

    return float(max(100.0 * cosine, 0.0))


def openclip_score(
    image_path: str | Path,
    prompt: str,
    language: Language,
    device: Optional[str] = None,
) -> float:
    """Compute only the OpenCLIP score."""
    _normalize_language(language)
    device_obj = _get_device(device)
    image = _load_image_rgb(image_path)
    return _compute_openclip(image=image, prompt=prompt, device=device_obj)


def _get_aesthetic_head_path() -> Path:
    return Path(DEFAULT_AESTHETIC_HEAD_PATH)


def _get_aesthetic_models(device: torch.device):
    global _AESTHETIC_CLIP_MODEL_OBJ, _AESTHETIC_CLIP_PROCESSOR, _AESTHETIC_HEAD

    if _AESTHETIC_CLIP_MODEL_OBJ is None or _AESTHETIC_CLIP_PROCESSOR is None:
        model_path = _require_exists(DEFAULT_AESTHETIC_CLIP_MODEL, "Aesthetic CLIP model directory")
        if LOCAL_FILES_ONLY:
            os.environ.setdefault("HF_HUB_OFFLINE", "1")
            os.environ.setdefault("TRANSFORMERS_OFFLINE", "1")

        _AESTHETIC_CLIP_MODEL_OBJ = CLIPModel.from_pretrained(
            model_path,
            local_files_only=LOCAL_FILES_ONLY,
        )
        _AESTHETIC_CLIP_PROCESSOR = CLIPProcessor.from_pretrained(
            model_path,
            local_files_only=LOCAL_FILES_ONLY,
        )
        _AESTHETIC_CLIP_MODEL_OBJ.eval()

    if _AESTHETIC_HEAD is None:
        head_path = _get_aesthetic_head_path()
        if not head_path.exists():
            if LOCAL_FILES_ONLY:
                raise FileNotFoundError(
                    f"Aesthetic head not found: {head_path}\n"
                    f"The current mode is offline; put the weight file locally first, "
                    f"or set OBJ_LOCAL_FILES_ONLY=0 to allow automatic download."
                )
            head_path.parent.mkdir(parents=True, exist_ok=True)
            urlretrieve(DEFAULT_AESTHETIC_HEAD_URL, head_path)

        head = nn.Linear(768, 1)
        state_dict = torch.load(head_path, map_location="cpu")
        head.load_state_dict(state_dict)
        head.eval()
        _AESTHETIC_HEAD = head

    _AESTHETIC_CLIP_MODEL_OBJ.to(device)
    _AESTHETIC_HEAD.to(device)
    return _AESTHETIC_CLIP_MODEL_OBJ, _AESTHETIC_CLIP_PROCESSOR, _AESTHETIC_HEAD


def _compute_aesthetic(image: Image.Image, device: torch.device) -> float:
    with torch.no_grad():
        model, processor, head = _get_aesthetic_models(device)
        inputs = processor(images=image, return_tensors="pt")
        inputs = {k: v.to(device) for k, v in inputs.items()}

        vision_outputs = model.vision_model(
            pixel_values=inputs["pixel_values"],
            return_dict=True,
        )
        image_features = vision_outputs.pooler_output
        image_features = model.visual_projection(image_features)
        image_features = _l2_normalize(image_features)

        score = head(image_features).squeeze().item()
        return float(score)


def aesthetic_score(
    image_path: str | Path,
    language: Language,
    device: Optional[str] = None,
) -> float:
    """Compute only the Aesthetic score. The language argument is kept for compatibility with the old interface."""
    _normalize_language(language)
    device_obj = _get_device(device)
    image = _load_image_rgb(image_path)
    return _compute_aesthetic(image=image, device=device_obj)


def _get_musiq(device: torch.device):
    global _MUSIQ_METRIC
    if _MUSIQ_METRIC is None:
        _MUSIQ_METRIC = pyiqa.create_metric(DEFAULT_MUSIQ_METRIC, device=device)
    return _MUSIQ_METRIC


def _compute_musiq(image_path: str | Path, device: torch.device) -> float:
    metric = _get_musiq(device)
    with torch.no_grad():
        score = metric(str(image_path))
    if isinstance(score, torch.Tensor):
        score = score.squeeze().item()
    return float(score)


def musiq_score(
    image_path: str | Path,
    language: Language,
    device: Optional[str] = None,
) -> float:
    """Compute only the MUSIQ score. The language argument is kept for compatibility with the old interface."""
    _normalize_language(language)
    device_obj = _get_device(device)
    return _compute_musiq(image_path=image_path, device=device_obj)


def objective_score(
    image_path: str | Path,
    prompt: str,
    language: Language,
    device: Optional[str] = None,
) -> Dict[str, float]:
    """
    Compute the three objective metrics for a single image.

    Return
    ----------
    Dict[str, float]
        {
          "OpenCLIP": ...,
          "Aesthetic": ...,
          "MUSIQ": ...
        }

    Notes
    ----------
    - OpenCLIP:Higher is better and indicates stronger text-image semantic consistency
    - Aesthetic:Higher is better and indicates higher visual aesthetics
    - MUSIQ:Higher is better and indicates higher no-reference image quality
    """
    _normalize_language(language)
    device_obj = _get_device(device)
    image = _load_image_rgb(image_path)

    openclip_score = _compute_openclip(image=image, prompt=prompt, device=device_obj)
    aesthetic_score = _compute_aesthetic(image=image, device=device_obj)
    musiq_score = _compute_musiq(image_path=image_path, device=device_obj)

    result = ObjectiveScoreResult(
        openclip=openclip_score,
        aesthetic=aesthetic_score,
        musiq=musiq_score,
    )
    return result.to_dict()


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser()
    parser.add_argument("image_path")
    parser.add_argument("prompt")
    parser.add_argument("language")
    parser.add_argument("--device", default=None)
    args = parser.parse_args()

    scores = objective_score(
        image_path=args.image_path,
        prompt=args.prompt,
        language=args.language,
        device=args.device,
    )
    print(scores)
