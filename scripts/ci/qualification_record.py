#!/usr/bin/env python3
"""One stage (c) qualification verdict over EVERY required CI job at one candidate SHA.

The mutation aggregate's `qualified=1` depends only on the mutation jobs: the aggregate job succeeds
while the proof replay, the fuzz campaigns, the multi-kernel checks or the regression suites fail. It
is therefore one COMPONENT of qualification, not the verdict. This script emits the verdict.

REQUIRED JOBS ARE DERIVED FROM THE WORKFLOW, not listed here: every job of
.github/workflows/lean_action_ci.yml is required (the push/pull-request jobs are stage (b), the
schedule/dispatch jobs complete stage (c)), so a job added to the workflow is required without
editing this file. A matrix job (`name: ... ${{ matrix.x }}`) is required as a family: at least one
instance, every instance successful. REQUIRED_STEPS names the steps whose own conclusion matters
because their job succeeds without them (a step skipped on push runs).

Inputs: one or more GitHub run records, either fetched (`--run ID`, via `gh run view`) or read
(`--run-json FILE`, the same JSON: headSha, event, status, conclusion, databaseId, jobs[].name,
jobs[].conclusion, jobs[].steps[].name/conclusion). Several runs may be combined (a weekly and a
nightly scheduled run at one SHA); every one must be completed and at the candidate SHA, and every
instance of a required job across them must be `success` — a later success never hides an earlier
failure at the same SHA.

A local closure-mode summary (run_ci_gates_local.sh) cannot qualify: it excludes the CI-only jobs,
so it is not accepted here.

Output: a key=value record on stdout (and --out). Exit 0 only when verdict=qualified; 1 when not
qualified; 2 on a usage or input error.
"""
import argparse
import json
import os
import re
import subprocess
import sys

REQUIRED_STEPS = {
    # job id -> step names that must themselves conclude `success`
    "multi-kernel": ["Multi-kernel mutation coverage (scheduled only — mutates source, 2 rebuilds)"],
}
MIN_JOBS = 10  # vacuity floor on the workflow parse


def die(msg):
    print(f"error: {msg}", file=sys.stderr)
    sys.exit(2)


def required_jobs(workflow):
    """[(job_id, display_name, is_matrix)] for every job in the workflow, plus the step names seen."""
    try:
        lines = open(workflow, encoding="utf-8").read().splitlines()
    except OSError as e:
        die(f"cannot read workflow {workflow}: {e}")
    jobs, steps, in_jobs, cur = [], {}, False, None
    for ln in lines:
        if re.match(r"^jobs:\s*$", ln):
            in_jobs = True
            continue
        if not in_jobs:
            continue
        if re.match(r"^\S", ln):
            in_jobs = False
            continue
        m = re.match(r"^  ([A-Za-z0-9_-]+):\s*$", ln)
        if m:
            cur = m.group(1)
            jobs.append([cur, None])
            steps[cur] = set()
            continue
        m = re.match(r"^    name:\s*(.+?)\s*$", ln)
        if m and cur and jobs[-1][1] is None:
            jobs[-1][1] = m.group(1).strip("\"'")
            continue
        m = re.match(r"^      - name:\s*(.+?)\s*$", ln)
        if m and cur:
            steps[cur].add(m.group(1).strip("\"'"))
    if len(jobs) < MIN_JOBS:
        die(f"workflow parse found {len(jobs)} jobs (< {MIN_JOBS}); refusing a vacuous requirement set")
    out = []
    for jid, name in jobs:
        if not name:
            die(f"job {jid} has no name: line; GitHub would display its id, refusing to guess")
        out.append((jid, name, "${{" in name))
    for jid, names in REQUIRED_STEPS.items():
        if jid not in steps:
            die(f"REQUIRED_STEPS names job {jid}, absent from the workflow")
        for s in names:
            if s not in steps[jid]:
                die(f"REQUIRED_STEPS names step '{s}' absent from job {jid}")
    return out


def matches(job, name):
    if not job[2]:
        return name == job[1]
    prefix = job[1].split("${{")[0]
    return name.startswith(prefix) and len(name) > len(prefix)


def load_runs(args):
    runs = []
    for rid in args.run or []:
        try:
            raw = subprocess.run(
                ["gh", "run", "view", str(rid), "--json",
                 "databaseId,headSha,event,status,conclusion,jobs"],
                check=True, capture_output=True, text=True).stdout
        except (OSError, subprocess.CalledProcessError) as e:
            die(f"gh run view {rid} failed: {e}")
        runs.append((str(rid), raw))
    for f in args.run_json or []:
        try:
            runs.append((f, open(f, encoding="utf-8").read()))
        except OSError as e:
            die(f"cannot read {f}: {e}")
    if not runs:
        die("no run given (--run ID or --run-json FILE)")
    parsed = []
    for src, raw in runs:
        try:
            r = json.loads(raw)
        except ValueError as e:
            die(f"{src}: not JSON: {e}")
        if not isinstance(r, dict) or not isinstance(r.get("jobs"), list):
            die(f"{src}: no jobs list")
        parsed.append((src, r))
    return parsed


def main():
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--candidate", help="the full candidate commit SHA")
    ap.add_argument("--workflow", default=os.path.join(
        os.path.dirname(os.path.abspath(__file__)), "..", "..", ".github", "workflows", "lean_action_ci.yml"))
    ap.add_argument("--run", action="append", help="a GitHub Actions run ID (fetched with gh)")
    ap.add_argument("--run-json", action="append", help="a saved `gh run view --json` record")
    ap.add_argument("--out", help="also write the record here")
    ap.add_argument("--list-required", action="store_true",
                    help="print the required jobs (id, name, matrix) and steps, then exit")
    args = ap.parse_args()
    if args.list_required:
        for jid, name, mx in required_jobs(args.workflow):
            print(f"job\t{jid}\t{name}\t{'matrix' if mx else 'single'}")
        for jid, names in REQUIRED_STEPS.items():
            for s in names:
                print(f"step\t{jid}\t{s}")
        return 0
    if not args.candidate or not re.fullmatch(r"[0-9a-f]{40}", args.candidate):
        die("--candidate must be a full 40-hex commit SHA")

    req = required_jobs(args.workflow)
    runs = load_runs(args)
    rec, reasons = [f"candidate={args.candidate}"], []

    instances = []  # (run src, job dict)
    for src, r in runs:
        sha, ev = r.get("headSha", ""), r.get("event", "")
        rid = r.get("databaseId", src)
        rec.append(f"run={rid} event={ev} head={sha} status={r.get('status', '')} conclusion={r.get('conclusion', '')}")
        if sha != args.candidate:
            reasons.append(f"run {rid} is at {sha or 'no SHA'}, not the candidate")
        if r.get("status") != "completed":
            reasons.append(f"run {rid} is not completed (status={r.get('status')})")
        for j in r["jobs"]:
            instances.append((rid, sha, j))

    for job in req:
        jid, name, _ = job
        hits = [(rid, sha, j) for rid, sha, j in instances if matches(job, j.get("name", ""))]
        if not hits:
            rec.append(f"job={jid} conclusion=missing")
            reasons.append(f"required job {jid} ({name}) has no result")
            continue
        for rid, sha, j in hits:
            c = j.get("conclusion") or j.get("status") or "unknown"
            rec.append(f"job={jid} name={j.get('name')} run={rid} head={sha} conclusion={c}")
            if c != "success":
                reasons.append(f"required job {j.get('name')} in run {rid}: {c}")
            if sha != args.candidate:
                reasons.append(f"required job {j.get('name')} ran at {sha or 'no SHA'}, not the candidate")
            for sname in REQUIRED_STEPS.get(jid, []):
                st = [s for s in j.get("steps") or [] if s.get("name") == sname]
                sc = (st[0].get("conclusion") or "unknown") if st else "missing"
                rec.append(f"step={jid}/{sname} run={rid} conclusion={sc}")
                if sc != "success":
                    reasons.append(f"required step '{sname}' in run {rid}: {sc}")

    agg = [c for c in rec if c.startswith("job=mutation-aggregate ")]
    rec.append("component=mutation-aggregate " + ("qualified=1" if agg and all(
        a.endswith("conclusion=success") for a in agg) else "qualified=0")
               + " (one component; it never decides the verdict alone)")
    verdict = "qualified" if not reasons else "not-qualified"
    rec.append(f"required_jobs={len(req)}")
    for r in reasons:
        rec.append(f"reason={r}")
    rec.append(f"verdict={verdict}")
    text = "\n".join(rec) + "\n"
    sys.stdout.write(text)
    if args.out:
        with open(args.out, "w", encoding="utf-8") as fh:
            fh.write(text)
    return 0 if verdict == "qualified" else 1


if __name__ == "__main__":
    sys.exit(main())
