#!/usr/bin/env python3
"""Per-gate pass/fail history of CI runs, flake candidates, and the flake ledger.

WHERE THE HISTORY LIVES. Two layers, chosen so nothing is committed per run:

  * The per-run OUTCOMES are not stored in git. They are derived on demand from GitHub's own run
    records (every attempt's jobs and steps, and the logs of failed jobs), which GitHub keeps for
    90 days. `record`/`scan` write them as a TSV; the nightly workflow uploads that TSV as the
    `flake-history-<run>` artifact, so each night's reading is kept for audit without a commit.
    Committing a row per run would conflict between concurrent runs and branches, and a cache
    would lose updates when two runs restore the same key; a derivation cannot do either.
  * The LEDGER, docs/project/flaky_gates.tsv, is committed and human-curated: one row per known
    flaky gate signature, with owner, status and expiry. `ledger --write` only proposes rows
    (status `candidate`) and raises counts; a person changes a status, in a reviewed commit.

A FLAKE CANDIDATE is a gate that failed and passed on the SAME commit: a later attempt of the run,
or another run at that SHA (a ci/* dispatch and the push to main of the same commit). Only failures
classified real or known-flaky count (scripts/ci/ci_logs.py): an infra failure is not the gate's,
and an infra defect (a pinned tool that does not build) is never a flake.

QUARANTINE needs an owner and an expiry in the ledger. A quarantined gate still runs; its step
still fails its job; triage and `report` list it in a separate section with its real outcome
(`failed (quarantined until ...)`), so it never silently passes. An expired quarantine is real.

Outcomes use the promotion pipeline's vocabulary: passed, failed, skipped, cancelled, incomplete
(a timed-out step is incomplete, never passed).

Commands:
  record --run ID [--run ID ...] [--out FILE]   outcomes of these runs (every attempt)
  scan [--limit N] [--branch-re RE] [--out FILE] outcomes of recent CI runs on main and ci/*
  candidates --history FILE                     gates that failed and passed on one SHA
  ledger --history FILE [--ledger P] [--write]  merge candidates into the ledger
  report --history FILE [--ledger P]            latest outcome per gate, quarantine separate
  new-failures --history FILE --run B --prev A  gates failed in B that did not fail in A
  check-ledger [--ledger P]                     validate the ledger (exit 1 on a problem)
"""
import argparse
import os
import re
import subprocess
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import ci_logs as L  # noqa: E402

ROOT = os.path.abspath(os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", ".."))
WORKFLOW = os.path.join(ROOT, ".github", "workflows", "lean_action_ci.yml")
LEDGER = os.path.join(ROOT, "docs", "project", "flaky_gates.tsv")
COLS = ["run_id", "attempt", "sha", "branch", "event", "created", "job", "step", "gate", "outcome", "class"]
OUTCOME = {"success": "passed", "failure": "failed", "skipped": "skipped", "cancelled": "cancelled",
           "timed_out": "incomplete"}


def die(msg, code=2):
    print(f"flake_history: {msg}", file=sys.stderr)
    sys.exit(code)


def step_gates(text):
    """[(job-name regex, step name, gate)] for every workflow step that runs a scripts/ gate."""
    try:
        import yaml
    except ImportError:
        die("PyYAML is required to read the workflow (python3-yaml)")
    try:
        doc = yaml.safe_load(text)
    except yaml.YAMLError as e:
        die(f"cannot parse the workflow: {e}")
    out = []
    for jid, j in ((doc or {}).get("jobs") or {}).items():
        name = str(j.get("name") or jid)
        rx = "^" + re.sub(r"\\\$\\\{\\\{.*?\\\}\\\}", ".+", re.escape(name)) + "$"
        for s_ in j.get("steps") or []:
            run = s_.get("run")
            if not isinstance(run, str) or not L.SCRIPT_RE.search(run):
                continue
            sname = s_.get("name") or ("Run " + run.splitlines()[0])
            out.append((re.compile(rx), sname, L.gate_of(run, sname)))
    return out


_GMAPS = {}


def gates_at(sha):
    """The step-to-gate map of the workflow AT THIS RUN'S COMMIT (a renamed or moved step maps by
    the workflow that ran it); the checked-out workflow when that commit is not available here."""
    if sha not in _GMAPS:
        try:
            text = subprocess.run(["git", "-C", ROOT, "show", f"{sha}:.github/workflows/lean_action_ci.yml"],
                                  capture_output=True, check=True).stdout.decode("utf-8")
        except (OSError, subprocess.CalledProcessError, UnicodeDecodeError):
            try:
                text = open(WORKFLOW, encoding="utf-8").read()
            except OSError as e:
                die(f"cannot read {WORKFLOW}: {e}")
        _GMAPS[sha] = step_gates(text)
    return _GMAPS[sha]


def gate_for(gmap, job, step):
    for rx, sname, gate in gmap:
        if sname == step and rx.match(job):
            return gate
    return None


def record_run(run_id, repo, ledger):
    """Outcome rows for every gate step of every attempt. A failed step is classified on its own
    log section; a job that never ran its steps (skipped, or cancelled before starting) yields a
    skipped/cancelled row for each gate the workflow puts in it, so a gate that stopped running
    never keeps showing an old pass as its latest outcome."""
    run = L.gh_json(["api", f"repos/{repo}/actions/runs/{run_id}"])
    gmap = gates_at(run.get("head_sha", ""))
    rows = []

    def row(att, jname, step, gate, outcome, cls):
        rows.append({"run_id": str(run_id), "attempt": str(att), "sha": run.get("head_sha", ""),
                     "branch": run.get("head_branch", ""), "event": run.get("event", ""),
                     "created": (run.get("created_at") or "")[:10], "job": jname,
                     "step": step, "gate": gate, "outcome": outcome, "class": cls})

    for att in range(1, int(run.get("run_attempt") or 1) + 1):
        data = L.gh_json(["api", f"repos/{repo}/actions/runs/{run_id}/attempts/{att}/jobs?per_page=100"])
        for j in data.get("jobs", []):
            jname, steps = j.get("name", ""), j.get("steps") or []
            if not steps:
                concl = j.get("conclusion")
                if concl in ("skipped", "cancelled", "timed_out", "failure"):
                    for rx, sname, gate in gmap:
                        if rx.match(jname):
                            row(att, jname, sname, gate, "incomplete" if concl == "timed_out" else
                                OUTCOME.get(concl, concl), "-")
                continue
            failed = [s_ for s_ in steps if s_.get("conclusion") == "failure"]
            res = None
            if failed:
                job = {"name": jname, "databaseId": j.get("id"), "conclusion": j.get("conclusion"),
                       "steps": steps}
                res = L.classify_job(job, L.job_log(j.get("id"), repo), ledger)
            secs = res["sections"] if res else []
            mapped = {gate_for(gmap, jname, s_.get("name", "")) for s_ in steps} - {None}
            unclaimed = [r for r in secs if r["gate"] not in mapped]
            unmapped_failed = [s_ for s_ in failed if gate_for(gmap, jname, s_.get("name", "")) is None]
            for s_ in steps:
                concl = s_.get("conclusion")
                if concl is None:
                    continue
                gate = gate_for(gmap, jname, s_.get("name", ""))
                if gate is None and concl != "failure":
                    continue
                outcome, cls = OUTCOME.get(concl, concl), "-"
                if concl == "failure":
                    # A failed step takes the classification of the log section that RAN ITS GATE
                    # (the last one, if it ran twice). Sections and steps are never paired by index:
                    # a tolerated continue-on-error failure is a section with no failed step.
                    # A failed step that runs no gate (an install or upload) has no gate to match its
                    # log section by. It takes the one section no gate step claims only when it is the
                    # one such step and that section is the only one; any ambiguity is recorded as
                    # failed/unattributed, never as another step's result.
                    if gate is not None:
                        own = [r for r in secs if r["gate"] == gate]
                        r = own[-1] if own else res
                    else:
                        gate = s_.get("name", "")
                        r = (unclaimed[0] if len(unclaimed) == 1 and len(unmapped_failed) == 1
                             else {"outcome": "failed", "class": "unattributed"})
                    outcome, cls = r["outcome"], r["class"]
                row(att, jname, s_.get("name", ""), gate, outcome, cls)
    return rows


def write_rows(rows, out):
    fh = open(out, "a", encoding="utf-8") if out else sys.stdout
    if out and os.path.getsize(out) == 0 or not out:
        fh.write("\t".join(COLS) + "\n")
    for r in rows:
        fh.write("\t".join(str(r[c]).replace("\t", " ") for c in COLS) + "\n")
    if out:
        fh.close()


def read_history(path):
    rows = []
    try:
        fh = open(path, encoding="utf-8")
    except OSError as e:
        die(f"cannot read history {path}: {e}")
    with fh:
        for line in fh:
            cells = line.rstrip("\n").split("\t")
            if cells == COLS or not line.strip():
                continue
            if len(cells) != len(COLS):
                die(f"{path}: malformed history row: {line.strip()[:120]}")
            rows.append(dict(zip(COLS, cells)))
    return rows


def candidates(rows):
    """{gate: {"shas": set, "first": date, "runs": set}} of gates failed and passed on one SHA."""
    by = {}
    for r in rows:
        by.setdefault((r["sha"], r["gate"]), []).append(r)
    out = {}
    for (sha, gate), rs in by.items():
        fails = [r for r in rs if r["outcome"] == "failed" and r["class"] in ("real", "known-flaky")]
        if fails and any(r["outcome"] == "passed" for r in rs):
            c = out.setdefault(gate, {"shas": set(), "first": "9999-99-99", "runs": set()})
            c["shas"].add(sha)
            c["first"] = min(c["first"], min(r["created"] for r in fails))
            c["runs"].update(r["run_id"] for r in fails)
    return out


def cmd_ledger(a):
    rows, problems = L.load_ledger(a.ledger)
    if problems:
        die("ledger malformed: " + "; ".join(problems))
    cand = candidates(read_history(a.history))
    lines = open(a.ledger, encoding="utf-8").read().splitlines() if os.path.exists(a.ledger) else \
        ["# flaky gates ledger", "\t".join(L.LEDGER_COLS)]
    changed = []
    for gate, c in sorted(cand.items()):
        n = len(c["shas"])
        existing = [r for r in rows if r["gate"] == gate]
        if existing:
            r = existing[0]
            if n > int(r["count"]):
                cells = lines[r["line"] - 1].split("\t")
                cells[2] = str(n)
                lines[r["line"] - 1] = "\t".join(cells)
                changed.append(f"count {gate}: {r['count']} -> {n}")
            continue
        sha = sorted(c["shas"])[0][:12]
        lines.append("\t".join([gate, c["first"], str(n), "-", "candidate", "-", "*",
                                f"auto: failed then passed at {sha} (runs {','.join(sorted(c['runs']))})"]))
        changed.append(f"candidate {gate} ({n} SHA(s))")
    for ch in changed:
        print(f"  {ch}")
    if not changed:
        print("  no new flake candidates")
    if a.write and changed:
        with open(a.ledger, "w", encoding="utf-8") as fh:
            fh.write("\n".join(lines) + "\n")
        print(f"  wrote {a.ledger}; review and commit it (status changes are a person's decision)")
    return 0


def cmd_report(a):
    rows = read_history(a.history)
    ledger, problems = L.load_ledger(a.ledger)
    if problems:
        die("ledger malformed: " + "; ".join(problems))
    latest = {}
    for r in rows:
        k = r["gate"]
        key = (r["created"], int(r["run_id"]), int(r["attempt"]))
        if k not in latest or key >= latest[k][0]:
            latest[k] = (key, r)
    quar = {r["gate"]: r for r in ledger if r["status"] == "quarantined"}
    print("| gate | latest outcome | run | class |\n|---|---|---|---|")
    for g in sorted(latest):
        if g in quar:
            continue
        r = latest[g][1]
        print(f"| `{g}` | {r['outcome']} | {r['run_id']}#{r['attempt']} | {r['class']} |")
    print("\nQuarantined gates (they still run; a failure here is a failure, never a pass):\n")
    if not quar:
        print("- none")
    for g, q in sorted(quar.items()):
        expired = L.today().isoformat() > q["expires"]
        if g in latest:
            r = latest[g][1]
            print(f"- `{g}`: {r['outcome']} in run {r['run_id']}#{r['attempt']} "
                  f"(quarantined until {q['expires']}, owner {q['owner']}"
                  f"{'; EXPIRED: treated as real' if expired else ''})")
        else:
            print(f"- `{g}`: not run in this history (quarantined until {q['expires']}, owner {q['owner']})")
    return 0


def cmd_new_failures(a):
    rows = read_history(a.history)
    cur = {r["gate"] for r in rows if r["run_id"] == a.run and r["outcome"] in ("failed", "incomplete")}
    prev_rows = [r for r in rows if r["run_id"] == a.prev]
    prev_bad = {r["gate"] for r in prev_rows if r["outcome"] in ("failed", "incomplete")}
    new = sorted(cur - prev_bad)
    for g in new:
        seen = "passed" if any(r["gate"] == g and r["outcome"] == "passed" for r in prev_rows) else "not run"
        print(f"new failure: {g} (previous run {a.prev}: {seen})")
    if not new:
        print(f"no new failures in run {a.run} relative to run {a.prev}")
    return 1 if new else 0


def main(argv):
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    sub = ap.add_subparsers(dest="cmd", required=True)
    p = sub.add_parser("record"); p.add_argument("--run", action="append", required=True)
    p.add_argument("--out"); p.add_argument("--repo")
    p = sub.add_parser("scan"); p.add_argument("--limit", type=int, default=50)
    p.add_argument("--branch-re", default=r"^(main|ci/.+)$"); p.add_argument("--out"); p.add_argument("--repo")
    p = sub.add_parser("candidates"); p.add_argument("--history", required=True)
    p = sub.add_parser("ledger"); p.add_argument("--history", required=True)
    p.add_argument("--ledger", default=LEDGER); p.add_argument("--write", action="store_true")
    p = sub.add_parser("report"); p.add_argument("--history", required=True); p.add_argument("--ledger", default=LEDGER)
    p = sub.add_parser("new-failures"); p.add_argument("--history", required=True)
    p.add_argument("--run", required=True); p.add_argument("--prev", required=True)
    p = sub.add_parser("check-ledger"); p.add_argument("--ledger", default=LEDGER)
    a = ap.parse_args(argv)

    if a.cmd == "check-ledger":
        rows, problems = L.load_ledger(a.ledger)
        for pr in problems:
            print(f"  FAIL {pr}")
        print(f"flaky ledger {a.ledger}: {len(rows)} row(s), {len(problems)} problem(s)")
        return 1 if problems else 0
    if a.cmd in ("record", "scan"):
        repo = a.repo or os.environ.get("GITHUB_REPOSITORY") or os.environ.get("GH_REPO")
        if not repo:
            die("pass --repo OWNER/NAME (or set GITHUB_REPOSITORY)")
        ledger, problems = L.load_ledger(LEDGER)
        if problems:
            die("ledger malformed: " + "; ".join(problems))
        try:
            if a.cmd == "record":
                ids = a.run
            else:
                runs = L.gh_json(["run", "list", "--workflow", "CI", "--limit", str(a.limit),
                                  "--json", "databaseId,headBranch,status"], repo)
                ids = [str(r["databaseId"]) for r in runs
                       if r.get("status") == "completed" and re.match(a.branch_re, r.get("headBranch", ""))]
            rows = []
            for rid in ids:
                rows += record_run(rid, repo, ledger)
        except L.GhError as e:
            die(str(e))
        if a.out and not os.path.exists(a.out):
            open(a.out, "w").close()
        write_rows(rows, a.out)
        return 0
    if a.cmd == "candidates":
        for gate, c in sorted(candidates(read_history(a.history)).items()):
            print(f"{gate}\tfirst_seen={c['first']}\tshas={len(c['shas'])}\truns={','.join(sorted(c['runs']))}")
        return 0
    return {"ledger": cmd_ledger, "report": cmd_report, "new-failures": cmd_new_failures}[a.cmd](a)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
