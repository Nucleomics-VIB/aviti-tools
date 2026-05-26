"""Pytest fixtures and shared test config."""
from __future__ import annotations

import sys
from pathlib import Path

# Make ``webui/`` importable from tests without packaging.
sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
