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
                           `bash` with a few allowed options (INVOCATION),
                           and the workflow, the step's job, every job it
                           transitively `needs`, and the step itself use only
                           allowlisted keys (no if:, env:, shell:,
                           working-directory:, defaults:, ... -- anything that can
                           skip the step or change how it runs). The one exception
                           is the workflow default that runs every step in the
                           gate shell (gates_shell.py), exactly that value.

The workflow is read with a real YAML parser (PyYAML), so quoting, spacing,
comments, block and multi-line scalars, flow mappings and indentation are the
parser's business, not this file's. Hand-rolled line matching was bypassed by each
of those in turn.

Exit status: 0 holds, 1 does not, 2 usage error (including PyYAML missing).
"""
import os
import re
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from gates_shell import RUN_SHELL  # noqa: E402

TOK = re.compile(r"[A-Za-z0-9_./-]+")

# ALLOWLISTS, not denylists: every key outside these can skip a step or job or
# change how it runs, so its presence disqualifies (contents are not vetted).
# PyYAML (YAML 1.1) reads the bare key `on` as the boolean True.
WORKFLOW_KEYS = {"name", "on", True, "concurrency", "jobs", "permissions"}
JOB_KEYS = {"name", "runs-on", "needs", "steps", "timeout-minutes", "permissions"}
STEP_KEYS = {"name", "run", "timeout-minutes"}
JOB_ID = re.compile(r"[A-Za-z0-9_-]+")

# The ONLY accepted spelling of an owner invocation: the whole run value is the
# owner run by `bash` (owners are scripts/tests/*.sh bash scripts: sh may be dash and
# python3 would not run them at all), with -e/-u/-x clusters or -o errexit|nounset|
# pipefail|xtrace. No direct execution (it depends on the file mode and shebang), no
# other interpreter, and no arguments, assignments, quoting,
# expansions, redirections or operators -- every one of those has a way to keep the
# script from running ($((1/0)), ${UNSET:?}, < /missing, true ||, ...), and the CI
# workflow invokes its gates this way.
INVOCATION = re.compile(
    r"bash(?: +-[eux]+| +-o +(?:errexit|nounset|pipefail|xtrace))* +"
    r"(?P<script>(?:\./)?[A-Za-z0-9_][A-Za-z0-9_./-]*)")


def tokens(text):
    raw = TOK.findall(text)
    return set(raw) | {t.rstrip(".") for t in raw}


def norm(p):
    return p[2:] if p.startswith("./") else p


def invoked_script(run):
    """The script a run value invokes, when the WHOLE value is exactly
    bash [<allowed options>] <script>; else None."""
    # ASCII only, and only plain spaces between words: Unicode whitespace (a no-break
    # space) is not a shell separator, so a \s match would accept a command bash
    # cannot find.
    if not isinstance(run, str) or not run.isascii():
        return None
    run = run.strip(" \n")
    if "\n" in run:
        return None
    m = INVOCATION.fullmatch(run)
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
    # DUPLICATE KEYS ARE REFUSED, as GitHub's own parser refuses them (the workflow would not run).
    # PyYAML keeps the last one, so `shell: bash -n {0}` followed by the gate shell compared equal
    # to the accepted default. Keys compare as GitHub reads them, as the text written: PyYAML turns a
    # bare `on` into True and a quoted "on" into a string, two keys to it and one to GitHub.
    class NoDupLoader(yaml.SafeLoader):
        pass

    def no_dup_mapping(loader, node, deep=False):
        keys = [k.value if isinstance(k, yaml.ScalarNode) else repr(loader.construct_object(k, deep=deep))
                for k, _ in node.value]
        if len(keys) != len(set(keys)):
            raise yaml.constructor.ConstructorError(None, None, "duplicate key", node.start_mark)
        return loader.construct_mapping(node, deep)

    NoDupLoader.add_constructor(yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG, no_dup_mapping)
    try:
        doc = yaml.load(open(workflow_path).read(), Loader=NoDupLoader)
    except yaml.YAMLError:
        return False
    if not isinstance(doc, dict) or not set(doc) <= WORKFLOW_KEYS | {"defaults"}:
        return False
    # The one workflow default accepted: every step runs in the gate shell. It decides which
    # userland runs the step, never whether or where it runs. Any other value (another shell, a
    # working directory, an extra key) still disqualifies.
    if "defaults" in doc and doc["defaults"] != {"run": {"shell": RUN_SHELL}}:
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
