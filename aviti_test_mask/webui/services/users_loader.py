"""Load the lab-members allowlist from users.yaml.

Part of aviti_test_mask — VIB Nucleomics Core.
Author: Stephane Plaisance <stephane.plaisance@vib.be>
"""
from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path

import yaml


@dataclass(frozen=True)
class User:
    id: str
    display_name: str
    email: str | None
    admin: bool


def load_users(path: Path) -> list[User]:
    if not path.exists():
        raise FileNotFoundError(f"users.yaml not found at {path}")
    with path.open() as fh:
        data = yaml.safe_load(fh) or {}
    raw = data.get("users", [])
    if not isinstance(raw, list) or not raw:
        raise ValueError(f"users.yaml has no users defined: {path}")
    seen = set()
    users: list[User] = []
    for entry in raw:
        uid = str(entry["id"]).strip()
        if not uid or uid in seen:
            raise ValueError(f"users.yaml: invalid or duplicate id {uid!r}")
        seen.add(uid)
        users.append(User(
            id=uid,
            display_name=str(entry.get("display_name", uid)),
            email=entry.get("email"),
            admin=bool(entry.get("admin", False)),
        ))
    return users
