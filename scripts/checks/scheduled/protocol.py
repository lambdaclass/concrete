"""Exact-SHA append-only scheduled evidence. No gate list lives here."""
from process_adapter import process_run
from scheduled_process import clean_env
from scheduler_policy import note_ref, BUDGET, WINDOWS
import os
import copy
import datetime as dt
import importlib.util
import json
import hashlib
import re
import subprocess
from pathlib import Path

STATUSES = {"PASS", "FAIL", "INCOMPLETE", "MISSING"}
_registry_spec = importlib.util.spec_from_file_location("gate_manifest", Path(__file__).resolve().parents[1] / "lib/gate_manifest.py")
_registry = importlib.util.module_from_spec(_registry_spec)
_registry_spec.loader.exec_module(_registry)
REGISTRY_ROOT = Path(__file__).resolve().parents[3]

class EvidenceError(ValueError):
    def __init__(self, message, *, status="INCOMPLETE", operational=False):
        super().__init__(message)
        self.status = status
        self.operational = operational


def observation_error(status, tier, sha, detail, *, operational=False):
    return EvidenceError(f"{status} {tier} {sha}: {detail}",
                         status=status, operational=operational)

def require(ok, message):
    if not ok:
        raise EvidenceError(message)

def git(*args, environment=None):
    return process_run(["git", *args], check=True, capture_output=True,
                          text=True, timeout=BUDGET["git"], env=clean_env(dict(os.environ) if environment is None else environment), cwd=REGISTRY_ROOT).stdout.strip()

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

def registry_at(sha):
    """Read data at the evidence revision with the trusted manifest reader."""
    try:
        body = git("-C", str(REGISTRY_ROOT), "show",
                   sha + ":scripts/checks/gates.json")
        return _registry.validate_manifest(json.loads(body))
    except (OSError, ValueError, subprocess.SubprocessError) as error:
        raise EvidenceError("INCOMPLETE registry: unavailable at " + sha) from error


def qualification(record, sha, tier):
    """A separate, recomputed verdict preserves partial and forged worker data."""
    if record["status"] == "MISSING":
        return []
    try:
        rows = _registry.scheduled_rows(tier, registry_at(sha))
    except (EvidenceError, _registry.GateManifestError) as error:
        return [dict(status="INCOMPLETE", diagnostic=str(error), operational=False)]
    issues = []
    def issue(status, message):
        issues.append(dict(status=status, diagnostic=status + " registry: " + message,
                           operational=status == "INCOMPLETE" and record["status"] == "INCOMPLETE"))
    expected = {row["id"]: row for row in rows}
    actual = {gate["id"]: gate for gate in record["gates"]}
    if not expected:
        issue("INCOMPLETE", "no registered gates for " + tier)
    for gid in sorted(actual.keys() - expected.keys()):
        issue("FAIL", "unregistered gate " + gid)
    for gid in sorted(expected.keys() - actual.keys()):
        issue("INCOMPLETE", "missing gate " + gid)
    for number, row in enumerate(rows):
        gid = row["id"]
        if gid not in actual:
            continue
        gate = actual[gid]
        if gate["command"] != row["command"]:
            issue("FAIL", "wrong command " + gid)
        controls = gate["controls"]
        ids = [c.get("id") for c in controls]
        if any(not isinstance(cid, str) for cid in ids) or len(set(ids)) != len(ids):
            issue("FAIL", "invalid control inventory " + gid)
            continue
        registered = {c["id"]: c for c in row["controls"]}
        observed = {c["id"]: c for c in controls}
        for cid in sorted(observed.keys() - registered.keys()):
            issue("FAIL", "unregistered control " + gid + "/" + cid)
        for cid in sorted(registered.keys() - observed.keys()):
            issue("INCOMPLETE", "missing control " + gid + "/" + cid)
        for cid in sorted(registered.keys() & observed.keys()):
            # Failed or interrupted controls retain their actual diagnostic. They
            # cannot qualify; only a claimed PASS attests the registered refusal.
            if observed[cid]["status"] == "PASS" and observed[cid].get("diagnostic") != registered[cid]["diagnostic"]:
                issue("FAIL", "wrong diagnostic " + gid + "/" + cid)
        artifacts = gate.get("artifacts", [])
        if not isinstance(artifacts, list) or any(not isinstance(a, dict) or not isinstance(a.get("path"), str) for a in artifacts):
            issue("FAIL", "invalid artifact inventory " + gid)
            continue
        paths = [a["path"] for a in artifacts]
        if len(set(paths)) != len(paths):
            issue("FAIL", "duplicate artifact " + gid)
        declarations = {f"gate-{number}-artifacts/" + a["path"]: a for a in row["artifacts"]}
        saved = {a["path"]: a for a in artifacts}
        for path in sorted(saved.keys() - declarations.keys()):
            issue("FAIL", "unregistered artifact " + gid + "/" + path)
        for path, declared in declarations.items():
            label = gid + "/" + declared["path"]
            if path not in saved:
                issue("INCOMPLETE", "missing artifact " + label)
                continue
            artifact = saved[path]
            if artifact.get("sha") is None:
                issue("INCOMPLETE", "missing SHA binding " + label)
            elif artifact["sha"] != sha:
                issue("FAIL", "wrong SHA binding " + label)
            elif artifact.get("binding_status") != "PASS":
                issue("INCOMPLETE", "missing SHA binding " + label)
            binding_path = f"gate-{number}-artifacts/" + declared["sha_file"]
            files = artifact.get("files", [])
            if not isinstance(files, list) or any(
                    not isinstance(f, dict) or not isinstance(f.get("path"), str) or
                    (f["path"] != path and not f["path"].startswith(path + "/")) or ".." in Path(f["path"]).parts or
                    type(f.get("bytes")) is not int or f["bytes"] < 0 or
                    not isinstance(f.get("sha256"), str) or re.fullmatch(r"[0-9a-f]{64}", f["sha256"]) is None
                    for f in files):
                issue("FAIL", "invalid artifact files " + label)
                continue
            if len({f["path"] for f in files}) != len(files):
                issue("FAIL", "duplicate artifact file " + label)
            if not any(isinstance(f, dict) and f.get("path") == binding_path for f in files):
                issue("INCOMPLETE", "missing binding artifact " + gid + "/" + declared["sha_file"])
            else:
                binding = next(f for f in files if isinstance(f, dict) and f.get("path") == binding_path)
                bindings = {(len(value), hashlib.sha256(value).hexdigest()) for value in
                            (sha.encode(), (sha + "\n").encode())}
                if artifact.get("sha") == sha and (binding.get("bytes"), binding.get("sha256")) not in bindings:
                    issue("FAIL", "wrong binding artifact " + gid + "/" + declared["sha_file"])
    return sorted(issues, key=lambda outcome: outcome["status"] != "FAIL")

def validate(record, sha, tier, runners, repository, attempt):
    """Validate explicit target identity; attempt=None validates a multi-attempt history."""
    require(isinstance(record, dict), "invalid record shape")
    require(re.fullmatch(r"[0-9a-f]{40}", sha) is not None, "invalid full SHA")
    require(record.get("sha") == sha, "wrong SHA")
    require(record.get("tier") == tier and tier in WINDOWS, "wrong tier")
    repo = record.get("repository")
    require(isinstance(repo, str) and repo == repository, "wrong repository")
    missing = record.get("status") == "MISSING"
    dependency = (record.get("status") == "INCOMPLETE" and record.get("gate_revision") is None)
    require(missing or dependency or (record.get("runner_environment") == "self-hosted" and
            record.get("runner_name") in runners.get(record.get("repository"), set())),
            "forged runner: not inventoried concrete-ci-2")
    require(isinstance(record.get("attempt"), str) and re.fullmatch(r"[0-9]+/[0-9]+", record["attempt"]) is not None,
            "invalid attempt identity")
    require(attempt is None or record["attempt"] == attempt, "wrong attempt")
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
    require(isinstance(gates, list), "missing gate evidence")
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
            require(all(c.get("status") == "PASS" for c in gate["controls"]), "missing/pending control evidence")
    statuses = {g["status"] for g in gates}
    expected = ("MISSING" if record["conclusion"] in {"cancelled", "skipped", "queued-too-long", "absent"}
                else "FAIL" if "FAIL" in statuses else "INCOMPLETE" if "INCOMPLETE" in statuses
                else "INCOMPLETE" if record.get("execution_error") or dependency else "PASS" if statuses <= {"PASS"} else None)
    conclusions = {"PASS": "success", "FAIL": "failure", "INCOMPLETE": "incomplete"}
    require(record["status"] == expected and
            (missing or record["conclusion"] == conclusions.get(expected)), "status/conclusion contradicts evidence")
    record = copy.deepcopy(record)
    record["qualification_outcomes"] = qualification(record, sha, tier)
    record["qualification"] = [o["diagnostic"] for o in record["qualification_outcomes"]]
    record["qualification_status"] = (
        "FAIL" if record["status"] == "FAIL" or any(o["status"] == "FAIL" for o in record["qualification_outcomes"])
        else "INCOMPLETE" if record["qualification"] else record["status"])
    return record

def append(history, record, sha, tier, runners, repository):
    record = validate(record, sha, tier, runners, repository, None)
    for old in history:
        validate(old, sha, tier, runners, repository, None)
        require(old["attempt"] != record["attempt"], "replay would overwrite attempt")
    return [*history, record]

def observe(history, sha, tier, now, runners, repository):
    if not history:
        raise observation_error("MISSING", tier, sha, "absent run", operational=True)
    history = [validate(record, sha, tier, runners, repository, None) for record in history]
    for record in history:
        require(timestamp(record["completed"]) <= now, "future completion")
    require(len({r["attempt"] for r in history}) == len(history), "replay would overwrite attempt")
    if any(r["status"] == "FAIL" for r in history):
        raise observation_error("FAIL", tier, sha, "recorded attempt")
    for record in history:
        for outcome in record["qualification_outcomes"]:
            if outcome["status"] == "FAIL":
                raise observation_error("FAIL", tier, sha, outcome["diagnostic"])
    recent = [r for r in history if now - timestamp(r["completed"]) <= WINDOWS[tier]]
    if not recent:
        raise observation_error("STALE", tier, sha, "no fresh PASS")
    latest = max(recent, key=lambda r: (timestamp(r["completed"]), timestamp(r["started"]), tuple(map(int, r["attempt"].split("/")))))
    if latest["qualification_outcomes"]:
        outcome = latest["qualification_outcomes"][0]
        raise observation_error(outcome["status"], tier, sha, outcome["diagnostic"],
                                operational=all(o["operational"] for o in latest["qualification_outcomes"]))
    if latest["status"] != "PASS":
        raise observation_error(latest["status"], tier, sha, "recorded attempt", operational=True)
    return f"PASS {tier} {sha}"

def read_note(sha, tier, listed=False, *, environment=None):
    ref = note_ref(tier)
    if not listed:
        listing = git("notes", "--ref=" + ref, "list", environment=environment)
        if not any(line.split()[-1] == sha for line in listing.splitlines()):
            return []
    lines = git("notes", "--ref=" + ref, "show", sha, environment=environment).splitlines()
    require(lines, "empty annotated note")
    return [json.loads(line) for line in lines]
