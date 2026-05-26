"""Load and validate AVITI mask strings.

Built-in masks come from masks.yaml (reused from the bash script). The UI
also accepts uploaded lists and a single typed mask — all three go through
the same regex validator before being persisted to a session masks.yaml.

Part of aviti_test_mask — VIB Nucleomics Core.
Author: Stephane Plaisance <stephane.plaisance@vib.be>
"""
from __future__ import annotations

import re
from dataclasses import dataclass
from pathlib import Path

import yaml

MASK_REGEX = re.compile(r"^R1:[YN0-9*]+-R2:[YN0-9*]+$")


@dataclass(frozen=True)
class Mask:
    text: str
    safe_name: str
    source: str  # builtin | uploaded | typed


def is_valid_mask(text: str) -> bool:
    return bool(MASK_REGEX.match(text.strip()))


def _safe_name(text: str) -> str:
    return re.sub(r"_+", "_", re.sub(r"[^A-Za-z0-9_.-]", "_", text))


def load_builtin_masks(path: Path) -> list[Mask]:
    if not path.exists():
        raise FileNotFoundError(f"masks.yaml not found at {path}")
    with path.open() as fh:
        data = yaml.safe_load(fh) or {}
    raw = data.get("masks") or []
    if not isinstance(raw, list) or not raw:
        raise ValueError(f"masks.yaml has no masks: {path}")
    out: list[Mask] = []
    for entry in raw:
        text = str(entry).strip()
        if not is_valid_mask(text):
            raise ValueError(f"invalid mask in {path}: {text!r}")
        out.append(Mask(text=text, safe_name=_safe_name(text), source="builtin"))
    return out


def parse_uploaded_masks(content: str) -> list[Mask]:
    """Parse a user-uploaded mask file.

    Accepts either a YAML doc with a top-level ``masks`` list, or a plain
    text file with one mask per line (``#`` comments allowed). Returns
    masks tagged as ``uploaded``. Raises ValueError listing every offending
    line on any invalid input.
    """
    if content.lstrip().startswith("masks:") or content.lstrip().startswith("- "):
        try:
            data = yaml.safe_load(content)
        except yaml.YAMLError as exc:
            raise ValueError(f"uploaded file is not valid YAML: {exc}") from exc
        if isinstance(data, dict):
            entries = data.get("masks", [])
        else:
            entries = data or []
        items = [str(x).strip() for x in entries]
    else:
        items = []
        for raw in content.splitlines():
            line = raw.split("#", 1)[0].strip()
            if line:
                items.append(line)

    if not items:
        raise ValueError("uploaded file contains no masks")
    bad = [m for m in items if not is_valid_mask(m)]
    if bad:
        raise ValueError(f"invalid mask lines: {bad}")
    return [Mask(text=m, safe_name=_safe_name(m), source="uploaded") for m in items]


def parse_typed_mask(text: str) -> Mask:
    text = text.strip()
    if not is_valid_mask(text):
        raise ValueError(f"invalid mask: {text!r}")
    return Mask(text=text, safe_name=_safe_name(text), source="typed")
