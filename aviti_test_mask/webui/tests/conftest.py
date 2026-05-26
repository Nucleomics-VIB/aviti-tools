"""Pytest fixtures and shared test config.

Tests import via the package paths (``from services.db import ...``)
rather than reaching into the webui root directly. We insert the
webui/ directory on sys.path so `services` and `routes` are
discoverable as packages.

Part of aviti_test_mask — VIB Nucleomics Core.
Author: Stephane Plaisance <stephane.plaisance@vib.be>
"""
from __future__ import annotations

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
