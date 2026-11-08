"""Exact-SHA append-only scheduled evidence. No gate list lives here."""
import datetime as dt
import importlib.util
import json
import re
import subprocess
from pathlib import Path

STATUSES = {"PASS", "FAIL", "INCOMPLETE", "MISSING"}
WINDOWS = {"daily": 36 * 3600, "weekly": 8 * 86400}

class EvidenceError(ValueError):
    pass

def require(ok, message):
    if not ok:
        raise EvidenceError(message)

def git(*args):
    return subprocess.run(["git", *args], check=True, capture_output=True,
                          text=True, timeout=60).stdout.strip()

def timestamp(value):
    try:
        parsed = dt.datetime.fromisoformat(value.replace("Z", "+00:00"))
        require(parsed.utcoffset() == dt.timedelta(0), "invalid UTC timestamp")
        return parsed.timestamp()
    except (ValueError, AttributeError):
        raise EvidenceError("invalid UTC timestamp")

def inventory():
    # ISS-0029 owns both parsing and the sole runner binding. No copied inventory.
    path = Path(__file__).resolve().parents[1] / "lib/repro_manifest_compare.py"
    require(path.is_file(), "INCOMPLETE dependency: ISS-0029 runner binding unavailable")
    spec = importlib.util.spec_from_file_location("ci2_binding", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module.read_inventory()

def validate(record, sha, tier, runners):
    require(re.fullmatch(r"[0-9a-f]{40}", sha) is not None, "invalid full SHA")
    require(record.get("sha") == sha, "wrong SHA")
    require(record.get("tier") == tier and tier in WINDOWS, "wrong tier")
    require(record.get("runner_environment") == "self-hosted" and
            record.get("runner_name") in runners.get(record.get("repository"), set()),
            "forged runner: not inventoried concrete-ci-2")
    require(re.fullmatch(r"[0-9]+/[0-9]+", record.get("attempt", "")) is not None,
            "invalid attempt identity")
    require(record.get("status") in STATUSES, "invalid status")
    require(record.get("conclusion") in {"success", "failure", "cancelled", "skipped", "queued-too-long", "absent", "incomplete"}, "invalid conclusion")
    if record["conclusion"] in {"cancelled", "skipped", "queued-too-long", "absent"}:
        require(record["status"] == "MISSING", "cancelled/skipped/overdue/absent must be MISSING")
    start, end = timestamp(record["started"]), timestamp(record["completed"])
    require(start <= end, "completion precedes start")
    require(record.get("occurrence"), "missing occurrence")
    require(record.get("artifact"), "missing result-index artifact")
    require(record.get("gate_revision") == sha, "wrong gate revision")
    gates = record.get("gates")
    require(isinstance(gates, list) and gates, "missing gate evidence")
    require(len({g["id"] for g in gates}) == len(gates), "duplicate gate evidence")
    for gate in gates:
        require(gate.get("status") in STATUSES, "invalid gate status")
        require(all(k in gate for k in ("command", "tools", "seed", "resources", "controls", "log")), "missing gate metadata")
        if gate["status"] == "PASS":
            require(gate["controls"] and all(c.get("status") == "PASS" for c in gate["controls"]), "missing/pending control evidence")
    if record["status"] == "PASS":
        require(record["conclusion"] == "success" and all(g["status"] == "PASS" for g in gates), "PASS contradicts evidence")
    return record

def append(history, record, sha, tier, runners):
    validate(record, sha, tier, runners)
    for old in history:
        validate(old, sha, tier, runners)
        require(old["attempt"] != record["attempt"], "replay would overwrite attempt")
    return [*history, record]

def observe(history, sha, tier, now, runners):
    if not history:
        raise EvidenceError(f"MISSING {tier} {sha}: absent run")
    for record in history:
        validate(record, sha, tier, runners)
        require(timestamp(record["completed"]) <= now, "future completion")
    require(len({r["attempt"] for r in history}) == len(history), "replay would overwrite attempt")
    recent = [r for r in history if now - timestamp(r["completed"]) <= WINDOWS[tier]]
    require(recent, f"STALE {tier} {sha}: no fresh PASS")
    # G2: a retry never erases disagreement in a fresh window.
    for status in ("FAIL", "INCOMPLETE", "MISSING"):
        require(not any(r["status"] == status for r in recent), f"{status} {tier} {sha}: recorded attempt")
    require(any(r["status"] == "PASS" for r in recent), f"MISSING {tier} {sha}: no PASS")
    return f"PASS {tier} {sha}"

def read_note(sha, tier):
    ref = "refs/notes/concrete-gates-" + tier
    listing = git("notes", "--ref=" + ref, "list")
    if not any(line.split()[-1] == sha for line in listing.splitlines()):
        return []
    return [json.loads(line) for line in git("notes", "--ref=" + ref, "show", sha).splitlines()]
