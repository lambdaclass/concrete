#!/usr/bin/env python3
"""One stage (c) qualification verdict over EVERY required CI job at one candidate SHA.

The mutation aggregate's `qualified=1` depends only on the mutation jobs: the aggregate job succeeds
while the proof replay, the fuzz campaigns, the multi-kernel checks or the regression suites fail. It
is therefore one COMPONENT of qualification, not the verdict. This script emits the verdict.

THE REQUIREMENTS ARE THE CANDIDATE'S OWN WORKFLOW. The workflow is read from the candidate commit
(`git show <candidate>:.github/workflows/lean_action_ci.yml`); a `--workflow` file is accepted for a
verdict only when its bytes equal that blob. It is parsed structurally (PyYAML; absent PyYAML is a
refusal, never a fallback). Every job is required (the push/pull-request jobs are stage (b), the
schedule/dispatch jobs complete stage (c)), so a job added to the workflow is required without
editing this file. Within each job every step is required unless its `if:` is a condition
STEP_CONDITIONS marks optional for a dispatched run, or its `continue-on-error:` is true; an
unrecognized condition or expression refuses. A required step is named by its `name:` or, for an
unnamed action, GitHub's `Run <action>`; REQUIRED_STEPS pins conditional steps qualification needs
(the scheduled multi-kernel mutation coverage) even if their condition were reclassified. A matrix job is
required instance by instance; only the matrix shapes in MATRIX_SIZES are understood, and any other
matrix, a computed job name without one, an unnamed unconditional `run:` step, an empty name, a
`continue-on-error` that is not a constant boolean, an `if:` not in STEP_CONDITIONS, duplicate step names or two jobs with one display
name REFUSE (exit 2) rather than shrinking the requirement set.

Input: exactly ONE GitHub run record, fetched (`--run ID`, via `gh run view`) or read (`--run-json
FILE`, the same JSON: attempt, databaseId, headSha, event, status, conclusion,
jobs[].name/status/conclusion, jobs[].steps[].name/status/conclusion). It must be the first attempt of
a `workflow_dispatch` run dispatched with `campaign=true` (the only event that starts every job;
scheduled runs skip each other's jobs, a dispatch without the campaign input skips the campaign tier,
and a rerun can hide a failure at the candidate). The run record does not carry the dispatch inputs,
so a dispatch without `campaign=true` is refused by its skipped campaign jobs, not by reading the
input), completed and successful at the candidate
SHA, with every required job instance exactly once and every required step exactly once, completed
and successful. Partial runs are never combined.

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

WORKFLOW_PATH = ".github/workflows/lean_action_ci.yml"
ROOT = os.path.abspath(os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", ".."))
REQUIRED_STEPS = {
    # job id -> conditional step names that must themselves conclude `success`
    "multi-kernel": ["Multi-kernel mutation coverage (scheduled only — mutates source, 2 rebuilds)"],
}
# matrix job id -> (job id whose env sets the size, env variable, matrix key)
MATRIX_SIZES = {
    "mutation-shard": ("mutation-plan", "MUTATION_SHARDS", "shard"),
}
MIN_JOBS = 10  # vacuity floor on the workflow parse
# Step `if:` conditions this tool understands, normalized (the `${{ }}` wrapper and extra whitespace
# removed), and whether the step must run in a successful first-attempt workflow_dispatch run. An
# unlisted condition REFUSES: a condition cannot silently make a gate step optional.
STEP_CONDITIONS = {
    "true": "required",
    "success()": "required",
    "always()": "required",
    # The campaign tier's step condition (the multi-kernel mutation coverage). A qualification run is a
    # dispatch with campaign=true, so the step is required; a dispatch without it skips the campaign
    # JOBS too, and those skipped jobs already refuse the verdict.
    "(github.event_name == 'schedule' && github.repository == 'lambdaclass/concrete' && github.ref == 'refs/heads/main') || (github.event_name == 'workflow_dispatch' && inputs.campaign)": "required",
    "false": "optional",
    "failure()": "optional",
    "steps.lean_install.outcome == 'failure'": "optional",
}


def norm_expr(v):
    """A constant or condition expression, normalized; None when it is not a supported form."""
    if isinstance(v, bool):
        return "true" if v else "false"
    if not isinstance(v, str):
        return None
    t = v.strip()
    m = re.fullmatch(r"\$\{\{(.*)\}\}", t, re.S)
    if m:
        t = m.group(1).strip()
    return re.sub(r"\s+", " ", t) or None


def die(msg):
    print(f"error: {msg}", file=sys.stderr)
    sys.exit(2)


def workflow_text(candidate, override):
    """The candidate's workflow; an override must be byte-identical to it unless no candidate is
    given (--list-required against a local file, which issues no verdict)."""
    blob = None
    if candidate:
        try:
            blob = subprocess.run(["git", "-C", ROOT, "show", f"{candidate}:{WORKFLOW_PATH}"],
                                  check=True, capture_output=True).stdout.decode("utf-8")
        except (OSError, subprocess.CalledProcessError, UnicodeDecodeError) as e:
            die(f"cannot read {WORKFLOW_PATH} at the candidate {candidate[:12]}: {e}")
    if override is None:
        if blob is None:
            override = os.path.join(ROOT, WORKFLOW_PATH)
        else:
            return blob
    try:
        text = open(override, encoding="utf-8").read()
    except OSError as e:
        die(f"cannot read workflow {override}: {e}")
    if blob is not None and text != blob:
        die(f"--workflow {override} differs from the candidate's {WORKFLOW_PATH}; "
            "requirements must come from the candidate commit")
    return text


def parse_workflow(text):
    try:
        import yaml
    except ImportError:
        die("PyYAML is required to read the workflow structurally (python3-yaml)")
    try:
        doc = yaml.safe_load(text)
    except yaml.YAMLError as e:
        die(f"workflow is not valid YAML: {e}")
    jobs = doc.get("jobs") if isinstance(doc, dict) else None
    if not isinstance(jobs, dict) or len(jobs) < MIN_JOBS:
        die(f"workflow has {len(jobs) if isinstance(jobs, dict) else 0} jobs (< {MIN_JOBS}); "
            "refusing a vacuous requirement set")
    out = []
    for jid, j in jobs.items():
        if not isinstance(jid, str) or not isinstance(j, dict):
            die(f"job {jid!r} is not a mapping under a string id")
        name = j.get("name")
        if not isinstance(name, str) or not name.strip():
            die(f"job {jid} has no non-empty name; GitHub would display its id, refusing to guess")
        steps = j.get("steps")
        if not isinstance(steps, list) or not steps:
            die(f"job {jid} has no steps list (reusable-workflow jobs are not supported)")
        parsed = []
        for k, st in enumerate(steps):
            if not isinstance(st, dict):
                die(f"job {jid} step {k} is not a mapping")
            optional = False
            if "if" in st:
                cond = norm_expr(st["if"])
                if cond not in STEP_CONDITIONS:
                    die(f"job {jid} step {k}: unsupported if: {st['if']!r} (add it to STEP_CONDITIONS "
                        "with whether a dispatched run must run it)")
                optional = STEP_CONDITIONS[cond] == "optional"
            if "continue-on-error" in st:
                coe = norm_expr(st["continue-on-error"])
                if coe not in ("true", "false"):
                    die(f"job {jid} step {k}: unsupported continue-on-error {st['continue-on-error']!r}")
                optional = optional or coe == "true"
            sname = None
            if "name" in st:
                sname = st["name"]
                if not isinstance(sname, str) or not sname.strip():
                    die(f"job {jid} step {k}: empty or non-string name {sname!r}")
            elif isinstance(st.get("uses"), str) and st["uses"].strip():
                sname = "Run " + st["uses"].strip()  # GitHub's display name for an unnamed action
            elif not optional:
                die(f"job {jid} step {k}: an unconditional step with no name and no action")
            if sname is not None:
                parsed.append({"name": sname, "optional": optional})
        names = [s["name"] for s in parsed]
        dup = sorted({n for n in names if names.count(n) > 1})
        if dup:
            die(f"job {jid}: duplicate step names {dup}; a step result could not be told apart")
        env = j.get("env") if isinstance(j.get("env"), dict) else {}
        out.append({"id": jid, "name": name, "steps": parsed, "env": env,
                    "matrix": (j.get("strategy") or {}).get("matrix") if isinstance(j.get("strategy"), dict) else None,
                    "has_strategy": "strategy" in j})
    return out


def required(text):
    """[(job id, [expected display names], [required step names])]"""
    jobs = parse_workflow(text)
    byid = {j["id"]: j for j in jobs}
    out = []
    for j in jobs:
        jid = j["id"]
        stepnames = [s["name"] for s in j["steps"] if not s["optional"]]
        allnames = {s["name"] for s in j["steps"]}
        for s in REQUIRED_STEPS.get(jid, []):
            if s not in allnames:
                die(f"REQUIRED_STEPS names step '{s}' absent from job {jid}")
            if s not in stepnames:
                stepnames.append(s)
        if jid in MATRIX_SIZES:
            src, var, key = MATRIX_SIZES[jid]
            mx = j["matrix"]
            want = "${{ fromJSON(needs." + src + ".outputs.shards) }}"
            if not isinstance(mx, dict) or set(mx) != {key} or mx[key] != want:
                die(f"matrix job {jid} must have exactly matrix {{{key}: {want}}}; got {mx!r}")
            m = re.fullmatch(r"(.*)\$\{\{ matrix\." + re.escape(key) + r" \}\}", j["name"])
            if not m:
                die(f"matrix job {jid} name is not '<prefix>${{{{ matrix.{key} }}}}': {j['name']}")
            raw = str(byid.get(src, {}).get("env", {}).get(var, ""))
            if not re.fullmatch(r"[1-9][0-9]*", raw):
                die(f"matrix size {var} in job {src} is not a positive integer: '{raw}'")
            names = [m.group(1) + str(i) for i in range(1, int(raw) + 1)]
        else:
            if j["has_strategy"] or "${{" in j["name"]:
                die(f"job {jid} has a strategy or a computed name but no MATRIX_SIZES entry")
            names = [j["name"]]
        out.append((jid, names, stepnames))
    for jid in list(REQUIRED_STEPS) + list(MATRIX_SIZES):
        if jid not in byid:
            die(f"{jid} is named by REQUIRED_STEPS or MATRIX_SIZES but absent from the workflow")
    every = [n for _, names, _ in out for n in names]
    clash = sorted({n for n in every if every.count(n) > 1})
    if clash:
        die(f"two required job instances share a display name {clash}; one result cannot satisfy both")
    return out


def load_runs(args):
    runs = []
    for rid in args.run or []:
        try:
            raw = subprocess.run(
                ["gh", "run", "view", str(rid), "--json",
                 "attempt,databaseId,headSha,event,status,conclusion,jobs"],
                check=True, capture_output=True, text=True).stdout
        except (OSError, subprocess.CalledProcessError) as e:
            die(f"gh run view {rid} failed: {e}")
        runs.append((str(rid), raw))
    for f in args.run_json or []:
        try:
            runs.append((f, open(f, encoding="utf-8").read()))
        except OSError as e:
            die(f"cannot read {f}: {e}")
    if len(runs) != 1:
        die(f"exactly one run record is required, got {len(runs)}: qualification is one complete "
            "dispatched run, never a combination of partial runs")
    src, raw = runs[0]
    try:
        r = json.loads(raw)
    except ValueError as e:
        die(f"{src}: not JSON: {e}")
    if not isinstance(r, dict) or not isinstance(r.get("jobs"), list):
        die(f"{src}: no jobs list")
    return src, r


def main():
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--candidate", help="the full candidate commit SHA")
    ap.add_argument("--workflow", help="a workflow file; for a verdict it must equal the candidate's")
    ap.add_argument("--run", action="append", help="a GitHub Actions run ID (fetched with gh)")
    ap.add_argument("--run-json", action="append", help="a saved `gh run view --json` record")
    ap.add_argument("--out", help="also write the record here")
    ap.add_argument("--list-required", action="store_true",
                    help="print the required job instances and steps, then exit")
    args = ap.parse_args()
    if args.candidate is not None and not re.fullmatch(r"[0-9a-f]{40}", args.candidate):
        die("--candidate must be a full 40-hex commit SHA")
    if args.list_required:
        for jid, names, steps in required(workflow_text(args.candidate, args.workflow)):
            for n in names:
                print(f"job\t{jid}\t{n}")
            for s in steps:
                print(f"step\t{jid}\t{s}")
        return 0
    if not args.candidate:
        die("--candidate is required for a verdict")
    req = required(workflow_text(args.candidate, args.workflow))
    src, r = load_runs(args)
    rec, reasons = [f"candidate={args.candidate}", f"workflow={WORKFLOW_PATH}@{args.candidate[:12]}"], []

    sha, rid = r.get("headSha", ""), r.get("databaseId", src)
    rec.append(f"run={rid} attempt={r.get('attempt')} event={r.get('event', '')} head={sha} "
               f"status={r.get('status', '')} conclusion={r.get('conclusion', '')}")
    if sha != args.candidate:
        reasons.append(f"run {rid} is at {sha or 'no SHA'}, not the candidate")
    if r.get("status") != "completed":
        reasons.append(f"run {rid} is not completed (status={r.get('status')})")
    if r.get("conclusion") != "success":
        reasons.append(f"run {rid} concluded {r.get('conclusion') or 'nothing'}, not success")
    if r.get("event") != "workflow_dispatch":
        reasons.append(f"run {rid} is a {r.get('event') or 'unknown'} run; only a workflow_dispatch "
                       "run (with campaign=true) starts every required job")
    if r.get("attempt") != 1:
        reasons.append(f"run {rid} is attempt {r.get('attempt')}; a rerun can hide a failure at the "
                       "candidate, so only a first attempt qualifies")

    for jid, names, steps in req:
        if jid in MATRIX_SIZES:
            prefix = names[0][:-1]
            for j in r["jobs"]:
                n = j.get("name", "")
                if n.startswith(prefix) and n not in names:
                    reasons.append(f"run {rid} has an unexpected {jid} instance '{n}'")
        for name in names:
            mine = [j for j in r["jobs"] if j.get("name") == name]
            if not mine:
                rec.append(f"job={jid} name={name} conclusion=missing")
                reasons.append(f"required job '{name}' has no result")
                continue
            if len(mine) > 1:
                reasons.append(f"run {rid} has {len(mine)} instances of '{name}'")
            for j in mine:
                st, c = j.get("status"), j.get("conclusion")
                rec.append(f"job={jid} name={name} status={st} conclusion={c}")
                if st != "completed" or c != "success":
                    reasons.append(f"required job '{name}': status={st} conclusion={c}")
                got = {}
                for s in j.get("steps") or []:
                    got.setdefault(s.get("name"), []).append(s)
                for sname in steps:
                    ss = got.get(sname, [])
                    if len(ss) != 1:
                        reasons.append(f"required step '{sname}' of '{name}': {len(ss)} results, "
                                       "expected exactly one")
                        continue
                    sst, sc = ss[0].get("status"), ss[0].get("conclusion")
                    if sst != "completed" or sc != "success":
                        reasons.append(f"required step '{sname}' of '{name}': status={sst} conclusion={sc}")
                for sname in REQUIRED_STEPS.get(jid, []):
                    ss = got.get(sname, [])
                    rec.append(f"step={jid}/{sname} conclusion="
                               + (",".join(str(s.get("conclusion")) for s in ss) or "missing"))

    agg = [c for c in rec if c.startswith("job=mutation-aggregate ")]
    agg_ok = bool(agg) and all(a.endswith("status=completed conclusion=success") for a in agg)
    rec.append("component=mutation-aggregate qualified=" + ("1" if agg_ok else "0")
               + " (one component; it never decides the verdict alone)")
    verdict = "qualified" if not reasons else "not-qualified"
    rec.append(f"required_jobs={sum(len(n) for _, n, _ in req)}")
    for x in reasons:
        rec.append(f"reason={x}")
    rec.append(f"verdict={verdict}")
    text = "\n".join(rec) + "\n"
    sys.stdout.write(text)
    if args.out:
        with open(args.out, "w", encoding="utf-8") as fh:
            fh.write(text)
    return 0 if verdict == "qualified" else 1


if __name__ == "__main__":
    sys.exit(main())
