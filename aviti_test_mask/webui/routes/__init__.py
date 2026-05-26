"""Flask blueprint registration.

Each submodule defines one Blueprint covering a coherent slice of the
HTTP surface. ``register_all(app)`` is the single entry point called
from ``create_app``.

Part of aviti_test_mask — VIB Nucleomics Core.
Author: Stephane Plaisance <stephane.plaisance@vib.be>
"""
from __future__ import annotations

from flask import Flask

from .api_jobs import bp as bp_api_jobs
from .api_misc import bp as bp_api_misc
from .api_runs import bp as bp_api_runs
from .pages import bp as bp_pages


def register_all(app: Flask) -> None:
    app.register_blueprint(bp_pages)
    app.register_blueprint(bp_api_misc)
    app.register_blueprint(bp_api_runs)
    app.register_blueprint(bp_api_jobs)
