"""Tile-selection resolution.

Translates a form-level tile spec (``default`` / ``all`` / ``lane:N`` /
``spread:N`` / ``random:N`` / ``raw``) into a concrete
``bases2fastq --include-tile`` pattern. Resolution happens at submit
time so the queued row carries the exact tile list / pattern that will
run.

Part of aviti_test_mask — VIB Nucleomics Core.
Author: Stephane Plaisance <stephane.plaisance@vib.be>
"""
from __future__ import annotations

import json
from pathlib import Path


def _read_tiles_list(run_path: Path) -> list[str]:
    """Return RunParameters.Tiles as a list of strings (empty on failure)."""
    try:
        rp = json.loads((run_path / "RunParameters.json").read_text())
    except (OSError, json.JSONDecodeError):
        return []
    tiles = rp.get("Tiles")
    return [str(t) for t in tiles] if isinstance(tiles, list) else []


def _spread_pick(items: list[str], n: int) -> list[str]:
    """Evenly-spaced N picks across a sorted list — deterministic."""
    if n <= 0 or not items:
        return []
    if n >= len(items):
        return list(items)
    step = len(items) / n
    return [items[int(i * step)] for i in range(n)]


def resolve_tile_spec(
    run_path: Path,
    tiles_mode: str,
    *,
    tiles_n: int = 3,
    tiles_lane: int | None = None,
    tiles_raw: str | None = None,
    lanes: str = "all",
) -> dict:
    """Translate a form-level tile spec into a concrete bases2fastq pattern.

    Re-submit (📋) draws a fresh random pick because each submission lands
    here independently.

    Returns ``{"spec": <human label>, "pattern": <include-tile arg or None>,
    "tiles": <chosen tile list>, "count": <int>}``.
    """
    tiles_mode = (tiles_mode or "default").strip().lower()
    lane_filter: set[str] | None = None
    if lanes in ("1", "2"):
        lane_filter = {lanes}

    def filter_by_lane(items: list[str]) -> list[str]:
        if lane_filter is None:
            return items
        return [t for t in items if t and t[0] == "L" and t[1] in lane_filter]

    # Each picked tile becomes ^TILE$ so bases2fastq's --include-tile
    # (regex-based) matches only the exact tile name. Without anchors,
    # 'L1R09C01S1' was matching L1R09C02 / C03 as well, breaking the
    # single-tile fast path.
    def _anchored(tiles: list[str]) -> str:
        return "|".join(f"^{t}$" for t in tiles)

    if tiles_mode == "default":
        # bases2fastq with --qc-only and no --include-tile processes the
        # first tile of each lane present. We pick those tiles here AND
        # pass them concretely via an anchored --include-tile so the
        # lane filter actually takes effect AND we don't get neighbour
        # columns / surfaces matching by accident. Falls back to no
        # flag only when the manifest tile list is absent.
        all_tiles = filter_by_lane(_read_tiles_list(run_path))
        if not all_tiles:
            return {"spec": "default", "pattern": None, "tiles": [], "count": 0}
        by_lane: dict[str, str] = {}
        for t in sorted(all_tiles):
            if len(t) >= 2 and t[0] == "L":
                lane = t[1]
                if lane not in by_lane:
                    by_lane[lane] = t
        picked = [by_lane[k] for k in sorted(by_lane)]
        return {"spec": "default",
                "pattern": _anchored(picked) if picked else None,
                "tiles": picked, "count": len(picked)}

    if tiles_mode == "all":
        all_tiles = filter_by_lane(_read_tiles_list(run_path))
        # "all" pattern is regex with wildcards — anchor it too so it
        # only matches strings that are exactly a tile ID.
        return {"spec": "all", "pattern": "^L.R..C..S.$",
                "tiles": all_tiles, "count": len(all_tiles)}

    if tiles_mode == "lane":
        lane = int(tiles_lane or 1)
        return {"spec": f"lane:{lane}",
                "pattern": f"^L{lane}R..C..S.$",
                "tiles": [], "count": -1}

    if tiles_mode == "raw":
        raw = (tiles_raw or "").strip()
        if not raw:
            raise ValueError("raw tile pattern is empty")
        # Raw mode is the operator's escape hatch — don't touch it.
        return {"spec": "raw", "pattern": raw, "tiles": [], "count": -1}

    if tiles_mode in ("spread", "random"):
        all_tiles = filter_by_lane(_read_tiles_list(run_path))
        if not all_tiles:
            raise ValueError("RunParameters.Tiles missing; cannot pick tiles")
        n = max(1, int(tiles_n))
        if tiles_mode == "spread":
            picked = _spread_pick(sorted(all_tiles), n)
        else:
            import random as _r
            n = min(n, len(all_tiles))
            picked = _r.sample(all_tiles, n)
        return {
            "spec": f"{tiles_mode}:{len(picked)}",
            "pattern": _anchored(picked),
            "tiles": picked,
            "count": len(picked),
        }

    raise ValueError(f"unknown tiles_mode: {tiles_mode!r}")
