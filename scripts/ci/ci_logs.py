#!/usr/bin/env python3
"""Shared reading of GitHub Actions job logs, the failure classifier and the flake ledger.

Used by scripts/ci/triage.py (one run: classify, rerun once or report) and
scripts/ci/flake_history.py (per-gate outcomes across runs). Both call `gh`, never the network
directly, so the tests (scripts/tests/check_ci_triage.sh) replace `gh` with a stub on PATH.

THE CLASSES. Every failed or cancelled job of a run gets exactly one:

  infra         the failure says nothing about the commit: the runner was lost or shut down, the job
                or run was cancelled, a network/apt/cache download failed, or a bounded run timed
                out twice and printed its INCOMPLETE marker (scripts/tests/lib/bounded.sh) with no
                assertion failing. Rerun once.
  infra-defect  OUR tooling is broken in a way a rerun reproduces: building a pinned external tool
                failed ("could not build lean4export", e.g. under a leaked GIT_DIR). It is not about
                the commit's code either, but it is a defect, never a flake: it is reported like a
                real failure, never rerun, and never matched by the flake ledger.
  known-flaky   the failing gate has a ledger row (docs/project/flaky_gates.tsv) with status
                known-flaky, or quarantined and not expired, whose `match` regex matches one of the
                failure lines. Rerun once.
  real          everything else, including a job timeout WITHOUT an INCOMPLETE marker (a hang
                cannot be told apart from a regression) and an expired quarantine.

The order is the order above, except that infra-defect is checked first: a lean4export build
failure must never be absorbed as "network" or as a ledger flake.

Outcome vocabulary for flake history (the promotion pipeline's): passed, failed, skipped,
cancelled, incomplete. A timed-out step is incomplete, never passed.
"""
import datetime
import json
import os
import re
import subprocess

TS_RE = re.compile(r"^\ufeff?\d{4}-\d\d-\d\dT\d\d:\d\d:\d\d(?:\.\d+)?Z ?")
ANSI_RE = re.compile(r"\x1b\[[0-9;]*m")
# A repository script, relative to the checkout. `${GITHUB_ACTION_PATH}/scripts/...` is an action's
# own script (lean-action's lake_build.sh), not a gate, hence the lookbehind.
SCRIPT_RE = re.compile(r"(?<![\w}/$-])(?:\./)?(scripts/[A-Za-z0-9_./-]+\.(?:sh|py))((?:\s+--?[A-Za-z][A-Za-z0-9_=-]*)*)")
EXIT_ERR_RE = re.compile(r"^##\[error\]Process completed with exit code \d+")

# Our own tooling failing to build a pinned tool. Reproducible by a rerun, so never a flake.
INFRA_DEFECT = [
    (re.compile(r"could not build (lean4export|con-leche|nanoda)\b"),
     "a pinned external tool did not build (tooling defect, e.g. GIT_DIR leaking into the build)"),
]
# The runner itself went away, or the job was cancelled under it.
RUNNER_LOST = [
    re.compile(r"lost communication with the server"),
    re.compile(r"The runner has received a shutdown signal"),
    re.compile(r"The self-hosted runner: .* (lost|did not)"),
    re.compile(r"##\[error\]The operation was canceled"),
    re.compile(r"No space left on device"),
]
JOB_TIMEOUT_RE = re.compile(r"has exceeded the maximum execution time of \d+ minutes")
# Network, package-mirror and cache-service failures, read only inside the failing step.
NETWORK = [
    re.compile(p) for p in (
        r"Could not resolve host", r"Temporary failure (in name resolution|resolving)",
        r"Connection (reset|refused|timed out)", r"\b(ECONNRESET|ETIMEDOUT|EAI_AGAIN)\b",
        r"^E: (Failed|Unable) to fetch", r"^E: Could not get lock", r"curl: \((6|7|28|35|52|56)\)",
        r"\b(502 Bad Gateway|503 Service Unavailable|504 Gateway Time-?out)\b",
        r"TLS handshake timeout", r"fatal: unable to access 'https?://", r"RPC failed",
        r"Cache service responded with 5\d\d", r"Failed to download action",
        r"error: could not download",
    )
]
INCOMPLETE_RE = re.compile(r"^\s*INCOMPLETE:")
# An assertion that failed. A tally line (`X: PASS=3 FAIL=0`) is not an assertion.
ASSERT_FAIL_RE = re.compile(r"^\s*(FAIL|FAILED)\b(?!=)")
FAILISH_RE = re.compile(
    r"^\s*(FAIL|FAILED)\b(?!=)|^\s*INCOMPLETE:|\bREFUSED\b|^\s*refusals:|^\s*error:|"
    r"\bFAIL=[1-9]|^##\[error\](?!Process completed)|^\s*[A-Z][A-Z0-9_ ()-]+: .*\bFAIL=[1-9]"
)
OK_RE = re.compile(r"^\s*(ok|skip|warn)\b")
MAX_FAIL_LINES = 40
MAX_LINE = 400

SEVERITY = {"infra": 0, "known-flaky": 1, "infra-defect": 2, "real": 3}
LEDGER_COLS = ["gate", "first_seen", "count", "owner", "status", "expires", "match", "note"]
LEDGER_STATUSES = ("candidate", "known-flaky", "quarantined", "fixed")
DATE_RE = re.compile(r"^\d{4}-\d\d-\d\d$")


# ---------------------------------------------------------------------------------------------- gh

class GhError(Exception):
    pass


def gh(args, repo=None, binary=False):
    """Run `gh`; return stdout (str, or bytes with binary=True). Raises GhError on failure."""
    cmd = ["gh"] + list(args)
    if repo and args and args[0] in ("run", "workflow"):
        cmd += ["-R", repo]
    try:
        p = subprocess.run(cmd, capture_output=True)
    except OSError as e:
        raise GhError(f"cannot run gh: {e}")
    if p.returncode != 0:
        raise GhError(f"gh {' '.join(args)}: exit {p.returncode}: "
                      f"{p.stderr.decode('utf-8', 'replace').strip()[:300]}")
    return p.stdout if binary else p.stdout.decode("utf-8", "replace")


def gh_json(args, repo=None):
    out = gh(args, repo)
    try:
        return json.loads(out)
    except ValueError as e:
        raise GhError(f"gh {' '.join(args)}: not JSON: {e}")


RUN_FIELDS = "databaseId,attempt,headSha,headBranch,event,status,conclusion,url,workflowName,createdAt,jobs"


def run_record(run_id, repo):
    return gh_json(["run", "view", str(run_id), "--json", RUN_FIELDS], repo)


def job_log(job_id, repo):
    """The job's log text, or None when GitHub has none (runner lost before upload, expired)."""
    try:
        return gh(["api", f"repos/{repo}/actions/jobs/{job_id}/logs"])
    except GhError:
        return None


# --------------------------------------------------------------------------------------- log reading

def clean_lines(text):
    out = []
    for raw in (text or "").splitlines():
        line = ANSI_RE.sub("", TS_RE.sub("", raw)).rstrip("\r")
        out.append(line)
    return out


def failing_section(lines):
    """-> (command text, section lines) of the step that failed: the last `##[group]Run` block
    before the first `Process completed with exit code` error. A job cancelled or timed out
    mid-step has no such error: then it is the last `##[group]Run` block, to the end of the log
    (the step that was running). (None, []) when the log has no step at all."""
    err = next((i for i, l in enumerate(lines) if EXIT_ERR_RE.match(l)), None)
    if err is None:
        err = len(lines) - 1
    start = None
    for i in range(err, -1, -1):
        if lines[i].startswith("##[group]Run "):
            start = i
            break
    if start is None:
        return None, lines[:err + 1] if any(EXIT_ERR_RE.match(l) for l in lines) else []
    cmd, j = [lines[start][len("##[group]Run "):]], start + 1
    while j < err and not lines[j].startswith("##[endgroup]"):
        cmd.append(lines[j])
        j += 1
    if j >= err:  # no end of the command echo (a truncated log): the header line is the command
        cmd, j = cmd[:1], start
    cmd = [c for c in cmd if not re.match(r"^\s*(shell|env):", c) and c.strip()]
    # The header line repeats the command's first line.
    if len(cmd) > 1 and cmd[0].strip() == cmd[1].strip():
        cmd = cmd[1:]
    return "\n".join(cmd), lines[j:err + 1]


def gate_of(command, fallback):
    """The gate a step runs: its first scripts/... invocation with its leading flags, else the
    step name. `./scripts/tests/run_tests.sh --trust-gate` -> `run_tests.sh --trust-gate`."""
    m = SCRIPT_RE.search(command or "")
    if not m:
        return fallback
    return (os.path.basename(m.group(1)) + re.sub(r"\s+", " ", m.group(2))).strip()


def repro_command(command):
    """The failing invocation as a local command: the line naming the gate, minus `| tee`."""
    for line in (command or "").splitlines():
        if SCRIPT_RE.search(line):
            line = re.sub(r"\s*2>&1\s*\|\s*tee\b.*$", "", line)
            line = re.sub(r"\s*\|\s*tee\b.*$", "", line).strip()
            line = re.sub(r"^\./", "", line)
            if not re.match(r"^(bash|python3|sh|env|[A-Z_]+=)", line):
                line = "bash " + line
            return line
    return None


def fail_lines(section):
    # A passing assertion may mention a refusal it expected ("ok   ... is REFUSED"); it is not one.
    out = [l.strip()[:MAX_LINE] for l in section if FAILISH_RE.search(l) and not OK_RE.match(l)]
    return out[:MAX_FAIL_LINES], len(out)


# ------------------------------------------------------------------------------------------- ledger

def today():
    v = os.environ.get("CI_TRIAGE_TODAY")
    return datetime.date.fromisoformat(v) if v else datetime.date.today()


def load_ledger(path):
    """-> (rows, problems). Rows are dicts over LEDGER_COLS; a missing file is an empty ledger."""
    rows, problems = [], []
    if not path or not os.path.exists(path):
        return rows, problems
    seen = set()
    with open(path, encoding="utf-8") as fh:
        for n, raw in enumerate(fh, 1):
            line = raw.rstrip("\n")
            if not line.strip() or line.startswith("#"):
                continue
            cells = line.split("\t")
            if cells == LEDGER_COLS:
                continue
            if len(cells) != len(LEDGER_COLS):
                problems.append(f"line {n}: {len(cells)} cells, want {len(LEDGER_COLS)} ({', '.join(LEDGER_COLS)})")
                continue
            r = dict(zip(LEDGER_COLS, cells))
            r["line"] = n
            for c in LEDGER_COLS:
                if not r[c].strip():
                    problems.append(f"line {n}: empty {c} (use '-' for none)")
            if r["status"] not in LEDGER_STATUSES:
                problems.append(f"line {n}: status {r['status']!r} not in {', '.join(LEDGER_STATUSES)}")
            if not DATE_RE.match(r["first_seen"]):
                problems.append(f"line {n}: first_seen {r['first_seen']!r} is not YYYY-MM-DD")
            if not r["count"].isdigit() or int(r["count"]) < 1:
                problems.append(f"line {n}: count {r['count']!r} is not a positive integer")
            if r["status"] in ("known-flaky", "quarantined") and r["owner"] in ("-", ""):
                problems.append(f"line {n}: {r['status']} needs an owner")
            if r["status"] == "quarantined" and not DATE_RE.match(r["expires"]):
                problems.append(f"line {n}: quarantined needs an expiry date (YYYY-MM-DD), got {r['expires']!r}")
            if r["expires"] != "-" and not DATE_RE.match(r["expires"]):
                problems.append(f"line {n}: expires {r['expires']!r} is not YYYY-MM-DD or '-'")
            if r["match"] != "*":
                try:
                    rx = re.compile(r["match"])
                except re.error as e:
                    problems.append(f"line {n}: match is not a regex: {e}")
                    rx = None
                # An infra defect is never a flake: no row may be written to absorb one.
                if rx is not None:
                    for sample in ("could not build lean4export at 0123abc",
                                   "could not build con-leche", "could not build nanoda"):
                        if rx.search(sample):
                            problems.append(f"line {n}: match {r['match']!r} matches an infra defect "
                                            f"({sample!r}); infra defects are reported, never flakes")
                            break
            key = (r["gate"], r["match"])
            if key in seen:
                problems.append(f"line {n}: duplicate row for gate {r['gate']!r} and match {r['match']!r}")
            seen.add(key)
            rows.append(r)
    return rows, problems


def ledger_match(rows, gate, lines):
    """The active ledger row covering this failure, and whether it is an expired quarantine."""
    for r in rows:
        if r["gate"] != gate or r["status"] not in ("known-flaky", "quarantined"):
            continue
        if r["match"] != "*" and not any(re.search(r["match"], l) for l in lines):
            continue
        expired = (r["status"] == "quarantined"
                   and datetime.date.fromisoformat(r["expires"]) < today())
        return r, expired
    return None, False


# --------------------------------------------------------------------------------------- classifier

def classify_job(job, log_text, ledger_rows):
    """Classify one failed/cancelled job. Returns a dict: class, reason, gate, step, command,
    fail_lines, fail_total, repro, ledger (row or None), outcome (failed|cancelled|incomplete)."""
    lines = clean_lines(log_text)
    failed_steps = [s.get("name", "") for s in job.get("steps", [])
                    if s.get("conclusion") in ("failure", "cancelled", "timed_out")]
    step = failed_steps[0] if failed_steps else (job.get("name") or "")
    command, section = failing_section(lines)
    gate = gate_of(command, step)
    fl, ftotal = fail_lines(section)
    asserts = [l for l in section if ASSERT_FAIL_RE.match(l)]
    whole = "\n".join(lines)
    res = {"job": job.get("name", ""), "job_id": job.get("databaseId"), "step": step, "gate": gate,
           "command": command, "fail_lines": fl, "fail_total": ftotal, "repro": repro_command(command),
           "ledger": None, "outcome": "failed"}

    def done(cls, reason, outcome=None):
        res.update({"class": cls, "reason": reason})
        if outcome:
            res["outcome"] = outcome
        return res

    for rx, why in INFRA_DEFECT:
        if any(rx.search(l) for l in lines):
            return done("infra-defect", why)
    if JOB_TIMEOUT_RE.search(whole):
        if any(INCOMPLETE_RE.match(l) for l in lines):
            return done("infra", "job timed out after a bounded run reported INCOMPLETE", "incomplete")
        return done("real", "job timed out with no INCOMPLETE marker: a hang is not distinguishable "
                            "from a regression", "incomplete")
    if job.get("conclusion") == "cancelled":
        return done("infra", "job cancelled", "cancelled")
    if log_text is None:
        return done("infra", "no job log: the runner was lost before the log was uploaded")
    for rx in RUNNER_LOST:
        if rx.search(whole):
            return done("infra", f"runner lost or job cancelled ({rx.pattern})", "cancelled")
    if not asserts:
        for rx in NETWORK:
            if any(rx.search(l) for l in section):
                return done("infra", f"network, package mirror or cache service ({rx.pattern})")
        if any(INCOMPLETE_RE.match(l) for l in section):
            return done("infra", "a bounded run timed out twice and reported INCOMPLETE; no assertion failed",
                        "incomplete")
    row, expired = ledger_match(ledger_rows, gate, fl or section)
    if row is not None:
        res["ledger"] = row
        if expired:
            return done("real", f"quarantine of {gate} expired {row['expires']} (owner {row['owner']})")
        return done("known-flaky", f"{row['status']} in the flake ledger (owner {row['owner']}, "
                                   f"line {row['line']})")
    return done("real", "no infra signature and no ledger entry")


def is_aggregate(job_name):
    return job_name.startswith("Gate mutation coverage (aggregate")


def is_shard(job_name):
    return job_name.startswith("Gate mutation shard")
