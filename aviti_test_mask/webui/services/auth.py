"""Session-auth primitives: bcrypt hashing, route decorators, session cleanup.

Three things live here:

- ``hash_password`` / ``verify_password``: bcrypt round-trip. Hashes are stored
  as the standard ``$2b$…`` string in the ``users.hashed_password`` column.
- ``login_required`` / ``admin_required``: decorators that gate Flask routes
  on ``session['user_id']`` and (for admin) ``session['role']``. AJAX requests
  (``X-Requested-With: XMLHttpRequest`` or JSON ``Accept``) get JSON 401/403;
  browser requests get a redirect to ``/login``.
- ``cleanup_old_sessions``: best-effort prune of Flask-Session filesystem
  artefacts. **Deletion is bounded** — we only ``Path.unlink()`` files that
  match Flask-Session's filename pattern (32-char hex) inside the configured
  session dir. Never ``rmtree`` the parent, never touch anything else.
  (Matches the deletion-safety rule documented in
  ``dev_docs/HANDOFF.md`` and the project memory.)

Part of aviti_test_mask — VIB Nucleomics Core.
Author: Stephane Plaisance <stephane.plaisance@vib.be>
"""
from __future__ import annotations

import logging
import re
import time
from functools import wraps
from pathlib import Path
from typing import Any, Callable

import bcrypt
from flask import jsonify, redirect, request, session, url_for

log = logging.getLogger(__name__)


# ---- bcrypt helpers ----

def hash_password(plain: str) -> str:
    """Bcrypt-hash a plain password. Returns the printable ``$2b$…`` string.

    Cost factor is bcrypt's default (12). Bump in code if hardware allows;
    don't bump in config — verify must use whatever cost was hashed at,
    which is encoded in the string itself.
    """
    if not isinstance(plain, str) or not plain:
        raise ValueError("password must be a non-empty string")
    return bcrypt.hashpw(plain.encode("utf-8"), bcrypt.gensalt()).decode("utf-8")


def verify_password(plain: str, hashed: str) -> bool:
    """Constant-time compare. Returns False on any format/encoding error."""
    if not plain or not hashed:
        return False
    try:
        return bcrypt.checkpw(plain.encode("utf-8"), hashed.encode("utf-8"))
    except (ValueError, TypeError):
        return False


# ---- decorators ----

def _wants_json() -> bool:
    """Heuristic for 'this is an AJAX call' — we should return JSON, not HTML."""
    if request.headers.get("X-Requested-With") == "XMLHttpRequest":
        return True
    accept = request.headers.get("Accept", "")
    return "application/json" in accept and "text/html" not in accept


def login_required(fn: Callable[..., Any]) -> Callable[..., Any]:
    @wraps(fn)
    def _wrapped(*args: Any, **kwargs: Any) -> Any:
        if not session.get("user_id"):
            if _wants_json():
                return jsonify({"error": "authentication required"}), 401
            # ``next`` lets the login route bounce back after success.
            return redirect(url_for("auth.login", next=request.path))
        return fn(*args, **kwargs)
    return _wrapped


def admin_required(fn: Callable[..., Any]) -> Callable[..., Any]:
    @wraps(fn)
    def _wrapped(*args: Any, **kwargs: Any) -> Any:
        if not session.get("user_id"):
            if _wants_json():
                return jsonify({"error": "authentication required"}), 401
            return redirect(url_for("auth.login", next=request.path))
        if session.get("role") != "admin":
            if _wants_json():
                return jsonify({"error": "admin role required"}), 403
            return jsonify({"error": "admin role required"}), 403
        return fn(*args, **kwargs)
    return _wrapped


# ---- session-dir cleanup (bounded) ----

# Flask-Session's filesystem backend writes one file per session named
# with a 32-char hex digest of the session ID. We *only* unlink files
# matching that pattern — never rmtree, never glob the parent.
_SESSION_FILE_PATTERN = re.compile(r"^[0-9a-f]{32,}$")


def cleanup_old_sessions(session_dir: str | Path, age_hours: float = 72) -> int:
    """Delete Flask-Session files older than ``age_hours``.

    Returns the count of files deleted. Silent no-op when the directory
    doesn't exist yet (first run before Flask-Session has written
    anything). Logs warnings on individual unlink failures but does not
    raise — cleanup runs from the login path and must never block login.

    Bounded by design (see module docstring): only files matching
    ``_SESSION_FILE_PATTERN`` inside ``session_dir`` are eligible. A
    misconfigured ``session_dir`` cannot cause arbitrary deletion.
    """
    session_dir = Path(session_dir)
    if not session_dir.is_dir():
        return 0
    cutoff = time.time() - (age_hours * 3600)
    removed = 0
    for entry in session_dir.iterdir():
        if not entry.is_file():
            continue
        if not _SESSION_FILE_PATTERN.match(entry.name):
            continue
        try:
            if entry.stat().st_mtime < cutoff:
                entry.unlink()
                removed += 1
        except OSError as exc:
            log.warning("session cleanup: could not remove %s: %s",
                        entry, exc)
    return removed
