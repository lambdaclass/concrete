#!/usr/bin/env python3
"""One stage (c) qualification verdict over EVERY required CI job at one candidate SHA.

The mutation aggregate's `qualified=1` depends only on the mutation jobs: the aggregate job succeeds
while the proof replay, the fuzz campaigns, the multi-kernel checks or the regression suites fail. It
is therefore one COMPONENT of qualification, not the verdict. This script emits the verdict.

REQUIRED JOBS AND STEPS ARE DERIVED FROM THE WORKFLOW, not listed here: every job of
.github/workflows/lean_action_ci.yml is required (the push/pull-request jobs are stage (b), the
schedule/dispatch jobs complete stage (c)), so a job added to the workflow is required without
editing this file. Within each job, every named step without an `if:` or `continue-on-error:` is
required to have run and succeeded; REQUIRED_STEPS adds conditional steps that qualification needs
(the scheduled multi-kernel mutation coverage). A matrix job is required instance by instance: its
size comes from the workflow (MATRIX_SIZES names where), every instance 1..N must be present exactly
once per run and succeed, and an unexpected instance refuses. The parser refuses any job header or
matrix it does not understand rather than dropping it.

Inputs: GitHub run records, fetched (`--run ID`, via `gh run view`) or read (`--run-json FILE`, the
same JSON: databaseId, headSha, event, status, conclusion, jobs[].name/status/conclusion,
jobs[].steps[].name/status/conclusion). THE SUPPORTED PATH IS ONE DISPATCHED RUN (`workflow_dispatch`
starts every job). Several runs may be supplied, but each must be completed, successful and at the
candidate SHA, and every instance of a required job in any of them must succeed, so a failure at the
candidate is never hidden behind a successful rerun; scheduled runs, which skip each other's jobs,
therefore cannot be combined into a qualification.

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
    # job id -> conditional step names that must themselves conclude `success`
    "multi-kernel": ["Multi-kernel mutation coverage (scheduled only — mutates source, 2 rebuilds)"],
}
# matrix job id -> (job id whose env sets the size, env variable, matrix key in the job name)
MATRIX_SIZES = {
    "mutation-shard": ("mutation-plan", "MUTATION_SHARDS", "shard"),
}
MIN_JOBS = 10  # vacuity floor on the workflow parse


def die(msg):
    print(f"error: {msg}", file=sys.stderr)
    sys.exit(2)


def scalar(v):
    """A YAML plain or quoted scalar on one line, without a trailing comment."""
    v = v.strip()
    if v[:1] in ("'", '"'):
        q = v[0]
        end = v.find(q, 1)
        if end < 0:
            die(f"unterminated quoted scalar: {v}")
        return v[1:end]
    return re.sub(r"\s+#.*$", "", v).strip()


def parse_workflow(path):
    try:
        lines = open(path, encoding="utf-8").read().splitlines()
    except OSError as e:
        die(f"cannot read workflow {path}: {e}")
    jobs, in_jobs, job, step = [], False, None, None
    for ln in lines:
        if re.match(r"^jobs:\s*(#.*)?$", ln):
            in_jobs = True
            continue
        if not in_jobs or not ln.strip() or ln.lstrip().startswith("#"):
            continue
        if re.match(r"^\S", ln):
            in_jobs = False
            continue
        indent = len(ln) - len(ln.lstrip(" "))
        if indent == 2:
            m = re.match(r"^  ([A-Za-z0-9_-]+):\s*(#.*)?$", ln)
            if not m:
                die(f"unsupported job header (not 'id:' on its own line): {ln.strip()}")
            job = {"id": m.group(1), "name": None, "env": {}, "steps": []}
            jobs.append(job)
            step = None
            continue
        if job is None:
            die(f"content under jobs: before any job header: {ln.strip()}")
        if indent == 4:
            step = None
            m = re.match(r"^    name:\s*(.+)$", ln)
            if m:
                job["name"] = scalar(m.group(1))
            continue
        m = re.match(r"^      ([A-Z_][A-Z0-9_]*):\s*(.+)$", ln)
        if m and step is None:
            job["env"][m.group(1)] = scalar(m.group(2))
            continue
        m = re.match(r"^      - (.*)$", ln)
        if m:
            step = {"name": None, "optional": False}
            job["steps"].append(step)
            rest = m.group(1)
            m2 = re.match(r"^name:\s*(.+)$", rest)
            if m2:
                step["name"] = scalar(m2.group(1))
            elif re.match(r"^(if|continue-on-error):", rest):
                step["optional"] = True
            continue
        if step is not None and indent == 8:
            m = re.match(r"^        (name|if|continue-on-error):\s*(.+)$", ln)
            if m and m.group(1) == "name":
                step["name"] = scalar(m.group(2))
            elif m and m.group(1) == "if":
                step["optional"] = True
            elif m and m.group(1) == "continue-on-error" and scalar(m.group(2)) != "false":
                step["optional"] = True
    if len(jobs) < MIN_JOBS:
        die(f"workflow parse found {len(jobs)} jobs (< {MIN_JOBS}); refusing a vacuous requirement set")
    return jobs


def required(path):
    """[(job id, [expected display names], [required step names])]"""
    jobs = parse_workflow(path)
    byid = {j["id"]: j for j in jobs}
    out = []
    for j in jobs:
        if not j["name"]:
            die(f"job {j['id']} has no name: line; GitHub would display its id, refusing to guess")
        stepnames = [s["name"] for s in j["steps"] if s["name"] and not s["optional"]]
        allnames = {s["name"] for s in j["steps"] if s["name"]}
        for s in REQUIRED_STEPS.get(j["id"], []):
            if s not in allnames:
                die(f"REQUIRED_STEPS names step '{s}' absent from job {j['id']}")
            if s not in stepnames:
                stepnames.append(s)
        if "${{" in j["name"]:
            if j["id"] not in MATRIX_SIZES:
                die(f"job {j['id']} has a computed name ({j['name']}) but no MATRIX_SIZES entry")
            src, var, key = MATRIX_SIZES[j["id"]]
            m = re.fullmatch(r"(.*)\$\{\{ matrix\." + re.escape(key) + r" \}\}", j["name"])
            if not m:
                die(f"matrix job {j['id']} name is not '<prefix>${{{{ matrix.{key} }}}}': {j['name']}")
            raw = byid.get(src, {}).get("env", {}).get(var, "")
            if not re.fullmatch(r"[1-9][0-9]*", raw):
                die(f"matrix size {var} in job {src} is not a positive integer: '{raw}'")
            names = [m.group(1) + str(i) for i in range(1, int(raw) + 1)]
        else:
            names = [j["name"]]
        out.append((j["id"], names, stepnames))
    for jid in REQUIRED_STEPS:
        if jid not in byid:
            die(f"REQUIRED_STEPS names job {jid}, absent from the workflow")
    for jid in MATRIX_SIZES:
        if jid not in byid:
            die(f"MATRIX_SIZES names job {jid}, absent from the workflow")
    return out


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
                    help="print the required job instances and steps, then exit")
    args = ap.parse_args()
    req = required(args.workflow)
    if args.list_required:
        for jid, names, steps in req:
            for n in names:
                print(f"job\t{jid}\t{n}")
            for s in steps:
                print(f"step\t{jid}\t{s}")
        return 0
    if not args.candidate or not re.fullmatch(r"[0-9a-f]{40}", args.candidate):
        die("--candidate must be a full 40-hex commit SHA")
    runs = load_runs(args)
    rec, reasons = [f"candidate={args.candidate}"], []

    for src, r in runs:
        sha, rid = r.get("headSha", ""), r.get("databaseId", src)
        rec.append(f"run={rid} event={r.get('event', '')} head={sha} status={r.get('status', '')} "
                   f"conclusion={r.get('conclusion', '')}")
        if sha != args.candidate:
            reasons.append(f"run {rid} is at {sha or 'no SHA'}, not the candidate")
        if r.get("status") != "completed":
            reasons.append(f"run {rid} is not completed (status={r.get('status')})")
        if r.get("conclusion") != "success":
            reasons.append(f"run {rid} concluded {r.get('conclusion') or 'nothing'}, not success")

    for jid, names, steps in req:
        prefix = names[0][:-1] if jid in MATRIX_SIZES else None
        for src, r in runs:
            rid = r.get("databaseId", src)
            if prefix is not None:
                for j in r["jobs"]:
                    n = j.get("name", "")
                    if n.startswith(prefix) and n not in names:
                        reasons.append(f"run {rid} has an unexpected {jid} instance '{n}'")
        for name in names:
            hits = []
            for src, r in runs:
                rid = r.get("databaseId", src)
                mine = [j for j in r["jobs"] if j.get("name") == name]
                if len(mine) > 1:
                    reasons.append(f"run {rid} has {len(mine)} instances of '{name}'")
                hits += [(rid, r.get("headSha", ""), j) for j in mine]
            if not hits:
                rec.append(f"job={jid} name={name} conclusion=missing")
                reasons.append(f"required job '{name}' has no result")
                continue
            for rid, sha, j in hits:
                st, c = j.get("status") or "unknown", j.get("conclusion") or "none"
                rec.append(f"job={jid} name={name} run={rid} head={sha} status={st} conclusion={c}")
                if st != "completed" or c != "success":
                    reasons.append(f"required job '{name}' in run {rid}: status={st} conclusion={c}")
                got = {}
                for s in j.get("steps") or []:
                    got.setdefault(s.get("name"), []).append(s)
                for sname in steps:
                    ss = got.get(sname, [])
                    if not ss:
                        reasons.append(f"required step '{sname}' of '{name}' in run {rid}: missing")
                        continue
                    for s in ss:
                        sst, sc = s.get("status") or "completed", s.get("conclusion") or "none"
                        if sst != "completed" or sc != "success":
                            reasons.append(f"required step '{sname}' of '{name}' in run {rid}: "
                                           f"status={sst} conclusion={sc}")
                if jid in REQUIRED_STEPS:
                    for sname in REQUIRED_STEPS[jid]:
                        ss = got.get(sname, [])
                        rec.append(f"step={jid}/{sname} run={rid} conclusion="
                                   + (",".join(s.get("conclusion") or "none" for s in ss) or "missing"))

    agg = [c for c in rec if c.startswith("job=mutation-aggregate ")]
    agg_ok = bool(agg) and all(a.endswith("status=completed conclusion=success") for a in agg)
    rec.append("component=mutation-aggregate qualified=" + ("1" if agg_ok else "0")
               + " (one component; it never decides the verdict alone)")
    verdict = "qualified" if not reasons else "not-qualified"
    rec.append(f"required_jobs={sum(len(n) for _, n, _ in req)}")
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
