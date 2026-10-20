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
                           allowed options (see INVOCATION) -- no arguments,
                           assignments, quoting, expansions, redirections or
                           operators -- in a step and job with no `if:`, no `env:`,
                           no custom shell and no working-directory, at any
                           indentation (see run_commands). Block scalars, step names
                           and comments are never read as invocations.

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


EXECUTING_SHELLS = {"bash", "sh"}


def shell_values(block_lines):
    """Values of every `shell:` key in these lines; a `working-directory:` counts as a
    non-executing shell value, so the same checks refuse it (a relative owner path
    does not resolve from another directory)."""
    out = []
    for l in block_lines:
        m = re.match(r"^\s*(?:-\s+)?shell:\s*(.*?)\s*(#.*)?$", l)
        if m:
            out.append(m.group(1).strip().strip("\"'"))
        if re.match(r"^\s*(?:-\s+)?working-directory:", l):
            out.append("<working-directory>")
    return out


def defaults_shells(lines, key_indent):
    """`shell:` values under a `defaults:` key at this indentation."""
    out, i = [], 0
    while i < len(lines):
        if re.match(r"^" + " " * key_indent + r"defaults:\s*$", lines[i]):
            i += 1
            body = []
            while i < len(lines) and (not lines[i].strip() or indent(lines[i]) > key_indent):
                body.append(lines[i])
                i += 1
            out += shell_values(body)
        else:
            i += 1
    return out


def run_commands(path):
    """The shell text of every INLINE `run:` value (`run: <command>` on one line)
    of an UNCONDITIONAL step in an UNCONDITIONAL job.

    Block scalars of ANY key (`run: |`, `script: >-`, ...) are skipped together with
    their bodies, so a heredoc or quoted text that happens to contain "run: ..."
    is never read as a key. Block-style `run:` values are not read at all: their
    lines share quote, heredoc and continuation state, and reading them line by
    line took printed text for commands. A step or job carrying any `if:` is
    excluded (`if: false` disables it; no condition is evaluated here), and so is
    any run whose effective shell -- step `shell:`, job or workflow
    `defaults.run.shell` -- is not plain bash/sh (a custom template such as
    `bash -n {0}` would only syntax-check it). Any `env:` at workflow, job or step
    scope also disqualifies (SHELLOPTS: noexec, BASH_ENV, PATH ... can keep the
    script from running; rather than vet values, environments are refused). An owner
    must therefore be invoked by an inline `run:` of an unconditional step --
    fail-closed, and every CI owner today is spelled that way."""
    lines = open(path).read().split("\n")
    block = re.compile(r"^(\s*(?:-\s+)?)[^\s#:][^:]*:\s*[|>][-+0-9]*\s*(#.*)?$")
    # Pass 1: blank out block-scalar bodies so nothing inside them is a key.
    # Comment-only lines are not structure: blank them first, so a comment cannot set
    # a job's property column or end a block.
    lines = ["" if l.strip().startswith("#") else l for l in lines]
    visible, i = list(lines), 0
    while i < len(lines):
        b = block.match(lines[i])
        i += 1
        if b:
            col = len(b.group(1))
            while i < len(lines) and (not lines[i].strip() or indent(lines[i]) > col):
                visible[i] = ""
                i += 1
    if any(v not in EXECUTING_SHELLS for v in defaults_shells(visible, 0)):
        return []           # a workflow-wide custom shell: nothing qualifies
    if any(re.match(r"^env:", l) for l in visible):
        return []           # a workflow-wide environment: nothing qualifies
    jobs_at = next((n for n, l in enumerate(visible) if re.match(r"^jobs:\s*$", l)), None)
    jcol = None
    if jobs_at is not None:
        jcol = next((indent(l) for l in visible[jobs_at + 1:] if l.strip()), None)
        if not jcol:
            jobs_at = None
    cmds = []
    for n, line in enumerate(visible):
        m = re.match(r"^(\s*)(-\s+)?run:\s*(.*)$", line)
        if not m or not m.group(3).strip() or block.match(line):
            continue
        key_col = len(m.group(1)) + len(m.group(2) or "")
        # The step: from its `- ` line down to the next line at or left of the dash.
        start = n
        if not m.group(2):
            while start > 0 and not (re.match(r"^\s*-\s", visible[start])
                                     and indent(visible[start]) < key_col):
                start -= 1
        dash = indent(visible[start])
        end = n + 1
        while end < len(visible) and (not visible[end].strip() or indent(visible[end]) > dash):
            end += 1
        step = [visible[start][dash + 2:]] + [l for l in visible[start + 1:end]]
        if any(re.match(r"^\s*if:", l) for l in step):
            continue
        if any(v not in EXECUTING_SHELLS for v in shell_values(step)):
            continue
        if any(re.match(r"^\s*(?:-\s+)?env:", l) for l in step):
            continue
        # The job, from the structure: the nearest header at the jobs' own column
        # (derived from the first entry under `jobs:`, any indentation width) above
        # the step; its properties sit at the column of the job's first property.
        # A run whose job cannot be established does not count.
        if jobs_at is None or n <= jobs_at:
            continue
        j = start
        while j > jobs_at and not (indent(visible[j]) == jcol and visible[j].strip()
                                   and re.match(r"^\s*[A-Za-z0-9_-]+:\s*$", visible[j])):
            j -= 1
        if j <= jobs_at:
            continue
        k = j + 1
        while k < len(visible) and (not visible[k].strip() or indent(visible[k]) > jcol):
            k += 1
        body = visible[j + 1:k]
        props = [l for l in body if l.strip()]
        if not props:
            continue
        pcol = indent(props[0])
        if any(indent(l) == pcol and re.match(r"^\s*(if|env):", l) for l in props):
            continue
        if any(v not in EXECUTING_SHELLS for v in defaults_shells(body, pcol)):
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
