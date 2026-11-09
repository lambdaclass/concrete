"""Exact-SHA append-only scheduled evidence. No gate list lives here."""
import datetime as dt
import importlib.util
import json
import os
import re
import subprocess
from pathlib import Path

STATUSES = {"PASS", "FAIL", "INCOMPLETE", "MISSING"}
_registry_spec = importlib.util.spec_from_file_location("gate_manifest", Path(__file__).resolve().parents[1] / "lib/gate_manifest.py")
_registry = importlib.util.module_from_spec(_registry_spec)
_registry_spec.loader.exec_module(_registry)
WINDOWS = _registry.load()["windows"]

class EvidenceError(ValueError):
    pass

def require(ok, message):
    if not ok:
        raise EvidenceError(message)

def clean_env():
    return {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}

def git(*args):
    return subprocess.run(["git", *args], check=True, capture_output=True,
                          text=True, timeout=60, env=clean_env()).stdout.strip()

def timestamp(value):
    try:
        parsed = dt.datetime.fromisoformat(value.replace("Z", "+00:00"))
        require(parsed.utcoffset() == dt.timedelta(0), "invalid UTC timestamp")
        return parsed.timestamp()
    except (ValueError, AttributeError):
        raise EvidenceError("invalid UTC timestamp")

_binding_spec = importlib.util.spec_from_file_location("ci2_binding", Path(__file__).resolve().parents[1] / "lib/ci2_binding.py")
_binding = importlib.util.module_from_spec(_binding_spec)
_binding_spec.loader.exec_module(_binding)

def inventory():
    try:
        return _binding.read_inventory()
    except _binding.BindingError as error:
        raise EvidenceError(str(error)) from error

def validate(record, sha, tier, runners, repository=None):
    require(isinstance(record, dict), "invalid record shape")
    require(re.fullmatch(r"[0-9a-f]{40}", sha) is not None, "invalid full SHA")
    require(record.get("sha") == sha, "wrong SHA")
    require(record.get("tier") == tier and tier in WINDOWS, "wrong tier")
    target = repository or os.environ.get("GITHUB_REPOSITORY")
    repo = record.get("repository")
    require(isinstance(repo, str) and (repo == target if target else repo in runners), "wrong repository")
    missing = record.get("status") == "MISSING"
    dependency = (record.get("status") == "INCOMPLETE" and record.get("gate_revision") is None)
    require(missing or dependency or (record.get("runner_environment") == "self-hosted" and
            record.get("runner_name") in runners.get(record.get("repository"), set())),
            "forged runner: not inventoried concrete-ci-2")
    require(isinstance(record.get("attempt"), str) and re.fullmatch(r"[0-9]+/[0-9]+", record["attempt"]) is not None,
            "invalid attempt identity")
    require(isinstance(record.get("status"), str) and record["status"] in STATUSES, "invalid status")
    require(isinstance(record.get("conclusion"), str) and record["conclusion"] in {"success", "failure", "cancelled", "skipped", "queued-too-long", "absent", "incomplete"}, "invalid conclusion")
    if record["conclusion"] in {"cancelled", "skipped", "queued-too-long", "absent"}:
        require(record["status"] == "MISSING", "cancelled/skipped/overdue/absent must be MISSING")
    start, end = timestamp(record["started"]), timestamp(record["completed"])
    require(start <= end, "completion precedes start")
    require(record.get("occurrence"), "missing occurrence")
    require(missing or record.get("artifact"), "missing result-index artifact")
    require(missing or record.get("gate_revision") == sha or
            (record.get("status") == "INCOMPLETE" and record.get("gate_revision") is None), "wrong gate revision")
    gates = record.get("gates")
    require(isinstance(gates, list) and (missing or gates), "missing gate evidence")
    require(not missing or gates == [], "MISSING must have empty gates")
    require(not missing or record.get("runner_name") is None or
            record.get("runner_name") in runners.get(repo, set()), "MISSING has uninventoried runner")
    for gate in gates:
        require(isinstance(gate, dict), "invalid gate shape")
        require(isinstance(gate.get("id"), str) and bool(gate["id"]), "invalid gate id")
        require(isinstance(gate.get("status"), str), "invalid gate status")
        require(isinstance(gate.get("controls"), list) and all(isinstance(c, dict) for c in gate["controls"]), "invalid controls shape")
        require(all(isinstance(c.get("status"), str) and c["status"] in STATUSES for c in gate["controls"]), "invalid control status")
        require(isinstance(gate.get("command"), list) and all(isinstance(v, str) for v in gate["command"]) and
                isinstance(gate.get("tools"), dict) and isinstance(gate.get("resources"), dict) and
                isinstance(gate.get("log"), str), "invalid gate metadata shape")
    require(len({g["id"] for g in gates}) == len(gates), "duplicate gate evidence")
    for gate in gates:
        require(gate.get("status") in STATUSES, "invalid gate status")
        require(all(k in gate for k in ("command", "tools", "seed", "resources", "controls", "log")), "missing gate metadata")
        if gate["status"] == "PASS":
            require(gate["controls"] and all(c.get("status") == "PASS" for c in gate["controls"]), "missing/pending control evidence")
    statuses = {g["status"] for g in gates}
    expected = ("MISSING" if record["conclusion"] in {"cancelled", "skipped", "queued-too-long", "absent"}
                else "FAIL" if "FAIL" in statuses else "INCOMPLETE" if "INCOMPLETE" in statuses
                else "PASS" if gates and statuses == {"PASS"} else None)
    conclusions = {"PASS": "success", "FAIL": "failure", "INCOMPLETE": "incomplete"}
    require(record["status"] == expected and
            (missing or record["conclusion"] == conclusions.get(expected)), "status/conclusion contradicts evidence")
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
    require(not any(r["status"] == "FAIL" for r in history), f"FAIL {tier} {sha}: recorded attempt")
    recent = [r for r in history if now - timestamp(r["completed"]) <= WINDOWS[tier]]
    require(recent, f"STALE {tier} {sha}: no fresh PASS")
    latest = max(recent, key=lambda r: (timestamp(r["completed"]), timestamp(r["started"]), tuple(map(int, r["attempt"].split("/")))))
    require(latest["status"] == "PASS", f"{latest['status']} {tier} {sha}: recorded attempt")
    return f"PASS {tier} {sha}"

def read_note(sha, tier, listed=False):
    ref = "refs/notes/concrete-gates-" + tier
    if not listed:
        listing = git("notes", "--ref=" + ref, "list")
        if not any(line.split()[-1] == sha for line in listing.splitlines()):
            return []
    lines = git("notes", "--ref=" + ref, "show", sha).splitlines()
    require(lines, "empty annotated note")
    return [json.loads(line) for line in lines]
