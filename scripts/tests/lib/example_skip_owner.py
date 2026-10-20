#!/usr/bin/env python3
"""Skip-owner checks for scripts/tests/check_examples.sh.

A manifest `skip owner=<gate>` is accepted only when both hold:

  ref  <owner> <src>       the owner names <src> as a complete path token, or, for a
                           .../src/main.con, the example directory as a complete
                           token. A longer name (<dir>.backup, main.con.disabled,
                           another file under <dir>) or a top-level directory plus a
                           common word does not count.
  runs <owner> <workflow>  some INLINE `run:` value of the workflow is exactly the
                           owner's path, optionally behind bash/sh/python3 with a few
                           allowed options (see INVOCATION), in a workflow, job and
                           step that use only allowlisted, plainly spelled keys (see
                           run_commands): no if:, env:, shell:, working-directory:,
                           defaults:, continued scalars or anything else unexpected.

Exit status: 0 holds, 1 does not, 2 usage error.
"""
import re
import sys

TOK = re.compile(r"[A-Za-z0-9_./-]+")


def tokens(text):
    raw = TOK.findall(text)
    return set(raw) | {t.rstrip(".") for t in raw}


def strip_shell_comment(line):
    out, quote = [], None
    for i, c in enumerate(line):
        if quote:
            if c == quote:
                quote = None
        elif c in "\"'":
            quote = c
        elif c == "#" and (i == 0 or line[i - 1] in " \t;|&("):
            break
        out.append(c)
    return "".join(out)


def indent(line):
    return len(line) - len(line.lstrip())


# ALLOWLISTS, not denylists. Every way found so far to keep a listed gate from running
# was a key the checker did not expect (if:, env:, shell:, working-directory:,
# defaults:, a quoted or spaced spelling of one of them, a continued scalar). So an
# owner run counts only inside a workflow, job and step that use NOTHING beyond these
# keys, spelled plainly (`key:` with no quoting and no space before the colon).
WORKFLOW_KEYS = {"name", "on", "concurrency", "jobs", "permissions"}
JOB_KEYS = {"name", "runs-on", "needs", "steps", "timeout-minutes", "permissions"}
STEP_KEYS = {"name", "run", "timeout-minutes"}
PLAIN_KEY = re.compile(r"^\s*(?:-\s+)?([A-Za-z0-9_-]+):(?:\s|$)")


def plain_key(line):
    m = PLAIN_KEY.match(line)
    return m.group(1) if m else None


def job_table(visible, jobs_at, jcol):
    """id -> (plain-keys-ok, needs list or None when unrecognized) for every job."""
    table, j = {}, jobs_at + 1
    while j < len(visible):
        line = visible[j]
        if line.strip() and indent(line) < jcol:
            break
        if line.strip() and indent(line) == jcol:
            hm = re.match(r"^\s*([A-Za-z0-9_-]+):\s*$", line)
            k = j + 1
            while k < len(visible) and (not visible[k].strip() or indent(visible[k]) > jcol):
                k += 1
            props = [l for l in visible[j + 1:k] if l.strip()]
            pcol = indent(props[0]) if props else None
            top = [l for l in props if indent(l) == pcol]
            ok = bool(hm) and pcol is not None and all(plain_key(l) in JOB_KEYS for l in top)
            needs = []
            for l in top:
                if plain_key(l) == "needs":
                    v = l.split(":", 1)[1].strip()
                    nm = re.fullmatch(r"([A-Za-z0-9_-]+)|\[\s*([A-Za-z0-9_-]+(?:\s*,\s*[A-Za-z0-9_-]+)*)\s*\]", v)
                    if not nm:
                        needs = None          # block list, expression, ...: unrecognized
                    else:
                        needs += [x.strip() for x in (nm.group(1) or nm.group(2)).split(",")]
            if hm:
                table[hm.group(1)] = (ok, needs)
            j = k
        else:
            j += 1
    return table


def job_runs_for_certain(job, table, seen=()):
    """The job and every job it transitively needs are listed, allowlisted (so none
    has an if:) and acyclic: a skipped dependency would skip this job too."""
    if job in seen or job not in table:
        return False
    ok, needs = table[job]
    if not ok or needs is None:
        return False
    return all(job_runs_for_certain(d, table, seen + (job,)) for d in needs)


def run_commands(path):
    """The shell text of every inline `run:` value that CI is certain to execute:

    - the workflow's top-level keys are all in WORKFLOW_KEYS;
    - the run's job is found from the structure (the jobs' column is that of the first
      entry under `jobs:`; the nearest line at or left of it above the step must be a
      plain `<id>:` header at exactly that column), and the job's own keys -- and those
      of every job it transitively `needs` (a skipped dependency skips it) -- are all
      in JOB_KEYS, with `needs` a literal id or [id, ...] list and no cycle;
    - the run's step consists only of single-line STEP_KEYS at one column (no nested
      or continued lines).

    Block scalars of ANY key are skipped with their bodies, and comment-only lines are
    blanked first, so neither can be read as structure."""
    lines = open(path).read().split("\n")
    lines = ["" if l.strip().startswith("#") else l for l in lines]
    block = re.compile(r"^(\s*(?:-\s+)?)[^\s#:][^:]*:\s*[|>][-+0-9]*\s*(#.*)?$")
    visible, i = list(lines), 0
    while i < len(lines):
        b = block.match(lines[i])
        i += 1
        if b:
            col = len(b.group(1))
            while i < len(lines) and (not lines[i].strip() or indent(lines[i]) > col):
                visible[i] = ""
                i += 1
    if any(plain_key(l) not in WORKFLOW_KEYS for l in visible if l.strip() and indent(l) == 0):
        return []
    jobs_at = next((n for n, l in enumerate(visible) if re.match(r"^jobs:\s*$", l)), None)
    if jobs_at is None:
        return []
    jcol = next((indent(l) for l in visible[jobs_at + 1:] if l.strip()), 0)
    if not jcol:
        return []
    jobs = job_table(visible, jobs_at, jcol)
    cmds = []
    for n, line in enumerate(visible):
        m = re.match(r"^(\s*)(-\s+)?run:\s+(.*)$", line)
        if not m or not m.group(3).strip() or block.match(line) or n <= jobs_at:
            continue
        # The step: from its `- ` line to the next line at or left of the dash.
        start = n
        if not m.group(2):
            while start > jobs_at and not re.match(r"^\s*-\s", visible[start]):
                start -= 1
        dash = indent(visible[start])
        kcol = dash + len(re.match(r"^\s*(-\s+)", visible[start]).group(1))
        end = start + 1
        while end < len(visible) and (not visible[end].strip() or indent(visible[end]) > dash):
            end += 1
        step = [l for l in visible[start:end] if l.strip()]
        if any((l is not step[0] and indent(l) != kcol) or plain_key(l) not in STEP_KEYS for l in step):
            continue
        # The job: the nearest line at or left of the jobs' column above the step.
        j = start - 1
        while j > jobs_at and not (visible[j].strip() and indent(visible[j]) <= jcol):
            j -= 1
        hm = re.match(r"^\s*([A-Za-z0-9_-]+):\s*$", visible[j])
        if j <= jobs_at or indent(visible[j]) != jcol or not hm:
            continue
        if not job_runs_for_certain(hm.group(1), jobs):
            continue
        k = j + 1
        while k < len(visible) and (not visible[k].strip() or indent(visible[k]) > jcol):
            k += 1
        props = [l for l in visible[j + 1:k] if l.strip()]
        pcol = indent(props[0]) if props else None
        if pcol is None or any(plain_key(l) not in JOB_KEYS for l in props if indent(l) == pcol):
            continue
        cmds.append(strip_shell_comment(m.group(3)))
    return cmds


def norm(p):
    return p[2:] if p.startswith("./") else p


# The ONLY accepted spelling of an owner invocation: the whole inline value is the
# script path, optionally behind bash/sh (with -e/-u/-x clusters or -o errexit|nounset|
# pipefail|xtrace) or python3 (with -I/-u/-B/-E). No arguments, assignments, quoting,
# expansions, redirections or operators -- every one of those has a way to keep the
# script from running ($((1/0)), ${UNSET:?}, < /missing, true ||, ...), and the CI
# workflow invokes its gates this way.
INVOCATION = re.compile(
    r"(?:(?:bash|sh)(?:\s+-[eux]+|\s+-o\s+(?:errexit|nounset|pipefail|xtrace))*\s+"
    r"|python3(?:\s+-[IuBE])*\s+)?"
    r"(?P<script>(?:\./)?[A-Za-z0-9_][A-Za-z0-9_./-]*)")


def invoked_scripts(command):
    """The script an inline run: value invokes, when the WHOLE value is exactly
    [bash|sh|python3 <allowed options>] <script> (see INVOCATION); else nothing."""
    m = INVOCATION.fullmatch(command.strip())
    return [norm(m.group("script"))] if m else []


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
        for c in run_commands(b):
            if norm(a) in invoked_scripts(c):
                return 0
        return 1
    return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv))
