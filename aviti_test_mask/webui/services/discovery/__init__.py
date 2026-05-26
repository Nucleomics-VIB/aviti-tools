"""Discovery sub-package — NAS scan, validation, metadata, tile resolution.

The public surface is re-exported here so callers keep writing
``from services.discovery import scan_nas_for_runs`` regardless of
which submodule actually defines the function.

Part of aviti_test_mask — VIB Nucleomics Core.
Author: Stephane Plaisance <stephane.plaisance@vib.be>
"""
from .scan import (
    RunCandidate,
    check_nas_mount,
    extract_projects_from_run_id,
    is_test_run,
    scan_nas_for_runs,
)
from .validation import (
    EXPECTED_DIRS,
    EXPECTED_TOP_LEVEL,
    ZIP_MAGIC,
    iter_validated,
    validate_run,
)
from .metadata import read_run_metadata, read_run_start
from .tiles import resolve_tile_spec

__all__ = [
    "RunCandidate",
    "check_nas_mount",
    "extract_projects_from_run_id",
    "is_test_run",
    "scan_nas_for_runs",
    "validate_run",
    "iter_validated",
    "read_run_metadata",
    "read_run_start",
    "resolve_tile_spec",
    "EXPECTED_DIRS",
    "EXPECTED_TOP_LEVEL",
    "ZIP_MAGIC",
]
