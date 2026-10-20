#!/usr/bin/env python3
"""Skip-owner checks for scripts/tests/check_examples.sh.

A manifest `skip owner=<gate>` is accepted only when both hold:

  ref  <owner> <src>       the owner names <src> as a complete path token, or, for a
                           .../src/main.con, the example directory as a complete
                           token. A longer name (<dir>.backup, main.con.disabled,
                           another file under <dir>) or a top-level directory plus a
                           common word does not count.
  runs <owner> <workflow>  CI is certain to execute the owner: some step's `run`
                           value is exactly the owner's path, optionally behind
                           bash/sh/python3 with a few allowed options (INVOCATION),
                           and the workflow, the step's job, every job it
                           transitively `needs`, and the step itself use only
                           allowlisted keys (no if:, env:, shell:,
                           working-directory:, defaults:, ... -- anything that can
                           skip the step or change how it runs).

The workflow is read with a real YAML parser (PyYAML), so quoting, spacing,
comments, block and multi-line scalars, flow mappings and indentation are the
parser's business, not this file's. Hand-rolled line matching was bypassed by each
of those in turn.

Exit status: 0 holds, 1 does not, 2 usage error (including PyYAML missing).
"""
import re
import sys

TOK = re.compile(r"[A-Za-z0-9_./-]+")

# ALLOWLISTS, not denylists: every key outside these can skip a step or job or
# change how it runs, so its presence disqualifies (contents are not vetted).
# PyYAML (YAML 1.1) reads the bare key `on` as the boolean True.
WORKFLOW_KEYS = {"name", "on", True, "concurrency", "jobs", "permissions"}
JOB_KEYS = {"name", "runs-on", "needs", "steps", "timeout-minutes", "permissions"}
STEP_KEYS = {"name", "run", "timeout-minutes"}
JOB_ID = re.compile(r"[A-Za-z0-9_-]+")

# The ONLY accepted spelling of an owner invocation: the whole run value is the
# script path, optionally behind bash/sh (with -e/-u/-x clusters or -o errexit|nounset|
# pipefail|xtrace) or python3 (with -I/-u/-B/-E). No arguments, assignments, quoting,
# expansions, redirections or operators -- every one of those has a way to keep the
# script from running ($((1/0)), ${UNSET:?}, < /missing, true ||, ...), and the CI
# workflow invokes its gates this way.
INVOCATION = re.compile(
    r"(?:(?:bash|sh)(?:\s+-[eux]+|\s+-o\s+(?:errexit|nounset|pipefail|xtrace))*\s+"
    r"|python3(?:\s+-[IuBE])*\s+)?"
    r"(?P<script>(?:\./)?[A-Za-z0-9_][A-Za-z0-9_./-]*)")


def tokens(text):
    raw = TOK.findall(text)
    return set(raw) | {t.rstrip(".") for t in raw}


def norm(p):
    return p[2:] if p.startswith("./") else p


def invoked_script(run):
    """The script a run value invokes, when the WHOLE value is exactly
    [bash|sh|python3 <allowed options>] <script>; else None."""
    if not isinstance(run, str) or "\n" in run.strip():
        return None
    m = INVOCATION.fullmatch(run.strip())
    return norm(m.group("script")) if m else None


def needs_of(job):
    """The literal job ids a job needs, or None when the form is not recognized."""
    needs = job.get("needs", [])
    if isinstance(needs, str):
        needs = [needs]
    if not isinstance(needs, list) or not all(isinstance(n, str) and JOB_ID.fullmatch(n) for n in needs):
        return None
    return needs


def runs_for_certain(job_id, jobs, seen=()):
    """The job and every job it transitively needs exist, are mappings with only
    JOB_KEYS (so none is conditional), and form no cycle."""
    if job_id in seen or job_id not in jobs:
        return False
    job = jobs[job_id]
    if not isinstance(job, dict) or not set(job) <= JOB_KEYS:
        return False
    needs = needs_of(job)
    if needs is None:
        return False
    return all(runs_for_certain(d, jobs, seen + (job_id,)) for d in needs)


def owner_runs(owner, workflow_path):
    try:
        import yaml
    except ImportError:
        print("example_skip_owner: PyYAML is required to read the workflow", file=sys.stderr)
        sys.exit(2)
    try:
        doc = yaml.safe_load(open(workflow_path).read())
    except yaml.YAMLError:
        return False
    if not isinstance(doc, dict) or not set(doc) <= WORKFLOW_KEYS:
        return False
    jobs = doc.get("jobs")
    if not isinstance(jobs, dict):
        return False
    for job_id, job in jobs.items():
        if not isinstance(job_id, str) or not runs_for_certain(job_id, jobs):
            continue
        steps = job.get("steps")
        if not isinstance(steps, list):
            continue
        for step in steps:
            if isinstance(step, dict) and set(step) <= STEP_KEYS \
                    and invoked_script(step.get("run")) == norm(owner):
                return True
    return False


def main(argv):
    if len(argv) != 4:
        return 2
    mode, a, b = argv[1], argv[2], argv[3]
    if mode == "ref":
        toks = tokens(open(a).read())
        if b in toks:
            return 0
        if b.endswith("/src/main.con"):
            d = b[: -len("/src/main.con")]
            if d in toks or d + "/" in toks:
                return 0
        return 1
    if mode == "runs":
        return 0 if owner_runs(a, b) else 1
    return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv))
