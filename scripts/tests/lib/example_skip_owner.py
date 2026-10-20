#!/usr/bin/env python3
"""Skip-owner checks for scripts/tests/check_examples.sh.

A manifest `skip owner=<gate>` is accepted only when both hold:

  ref  <owner> <src>       the owner names <src> as a complete path token, or, for a
                           .../src/main.con, the example directory as a complete
                           token. A longer name (<dir>.backup, main.con.disabled,
                           another file under <dir>) or a top-level directory plus a
                           common word does not count.
  runs <owner> <workflow>  some INLINE `run:` value of the workflow is exactly one
                           simple command invoking the owner (block scalars are not
                           read; see run_commands and invoked_scripts): after shell comments are stripped and
                           the command is split on UNQUOTED ; && || | ( ), the owner
                           is the executable of a simple command, or the script
                           operand of bash/sh/python3, after optional VAR=value
                           assignments and `env`/`timeout N` prefixes. A step name, a
                           comment, quoted text, the path as an ordinary argument
                           (echo, printf, an assignment), or an interpreter option
                           outside a small allowlist (-n, -o noexec, -c, -s, -m ...)
                           does not count.

Exit status: 0 holds, 1 does not, 2 usage error.
"""
import re
import shlex
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


def run_commands(path):
    """The shell text of every INLINE `run:` value (`run: <command>` on one line).

    Block scalars of ANY key (`run: |`, `script: >-`, ...) are skipped together with
    their bodies, so a heredoc or quoted text that happens to contain "run: ..."
    is never read as a key. Block-style `run:` values are not read at all: their
    lines share quote, heredoc and continuation state, and reading them line by
    line took printed text for commands. An owner must therefore be invoked by an
    inline `run:` -- fail-closed, and every CI owner today is spelled that way."""
    lines = open(path).read().split("\n")
    cmds, i = [], 0
    block = re.compile(r"^(\s*(?:-\s+)?)[^\s#:][^:]*:\s*[|>][-+0-9]*\s*(#.*)?$")
    while i < len(lines):
        line = lines[i]
        b = block.match(line)
        if b:
            col = len(b.group(1))
            i += 1
            while i < len(lines) and (not lines[i].strip()
                                      or len(lines[i]) - len(lines[i].lstrip()) > col):
                i += 1
            continue
        m = re.match(r"^\s*(?:-\s+)?run:\s*(.*)$", line)
        if m and m.group(1).strip():
            cmds.append(strip_shell_comment(m.group(1)))
        i += 1
    return cmds


def norm(p):
    return p[2:] if p.startswith("./") else p


def simple_commands(command):
    """Split the raw text at UNQUOTED, UNESCAPED operator characters ; & | ( ),
    tracking quote and backslash state, then word-split each piece with shlex.
    (Splitting shlex's output instead would see a quoted ";" as an operator.)"""
    pieces, cur, quote, i = [], [], None, 0
    while i < len(command):
        c = command[i]
        if quote == "'":
            if c == "'":
                quote = None
            cur.append(c)
        elif c == "\\" and i + 1 < len(command):
            cur.append(c + command[i + 1])
            i += 1
        elif quote == '"':
            if c == '"':
                quote = None
            cur.append(c)
        elif c in "'\"":
            quote = c
            cur.append(c)
        elif c in ";&|()":
            pieces.append("".join(cur))
            cur = []
        else:
            cur.append(c)
        i += 1
    if quote:
        return                  # unbalanced quotes: nothing in it counts
    pieces.append("".join(cur))
    for p in pieces:
        try:
            words = shlex.split(p, posix=True)
        except ValueError:
            continue
        if words:
            yield words


SHELL_FLAGS = set("eux")                     # errexit, nounset, xtrace
SHELL_O_NAMES = {"errexit", "nounset", "pipefail", "xtrace"}
PYTHON_FLAGS = {"-I", "-u", "-B", "-E"}


def interpreter_script(name, args):
    """The script a bash/sh/python3 invocation runs, or None. Options are an
    ALLOWLIST: anything else (-n / -o noexec syntax checks, -c command strings,
    -s stdin scripts, -m modules, long options) means no script operand is run."""
    k = 0
    while k < len(args) and args[k].startswith(("-", "+")) and args[k] not in ("-", "+"):
        opt = args[k]
        if name in ("bash", "sh"):
            if opt in ("-o", "+o"):
                if k + 1 < len(args) and args[k + 1] in SHELL_O_NAMES:
                    k += 2
                    continue
                return None
            if opt.startswith("--") or not set(opt[1:]) <= SHELL_FLAGS:
                return None
        elif opt not in PYTHON_FLAGS:
            return None
        k += 1
    return args[k] if k < len(args) else None


def invoked_scripts(command):
    """The script an inline run: value invokes, when the WHOLE value is one simple
    command: [VAR=value ...] [env [VAR=value ...]] [timeout N] <script> [args...]
    or the same with bash/sh/python3 [options] <script> [args...]. Any unquoted
    operator (; & | ( ) -- a list, a pipeline, a subshell, a function, a
    redirection like 2>&1) makes it not count: execution context such as
    `true || <owner>` is not modelled, so it is refused."""
    pieces = list(simple_commands(command))
    if len(pieces) != 1 or re.search(r"[;&|()]", unquoted_text(command)):
        return []
    words = pieces[0]
    k = 0
    while k < len(words) and re.match(r"^[A-Za-z_][A-Za-z0-9_]*=", words[k]):
        k += 1
    while k < len(words):
        if words[k] == "env":
            k += 1
            while k < len(words) and re.match(r"^[A-Za-z_][A-Za-z0-9_]*=", words[k]):
                k += 1
        elif words[k] == "timeout" and k + 1 < len(words):
            k += 2
        else:
            break
    if k >= len(words):
        return []
    if words[k] in ("bash", "sh", "python3"):
        script = interpreter_script(words[k], words[k + 1:])
        return [norm(script)] if script is not None else []
    return [norm(words[k])]


def unquoted_text(command):
    """The command with quoted and escaped characters removed."""
    out, quote, i = [], None, 0
    while i < len(command):
        c = command[i]
        if quote == "'":
            if c == "'":
                quote = None
        elif c == "\\":
            i += 1
        elif quote == '"':
            if c == '"':
                quote = None
        elif c in "'\"":
            quote = c
        else:
            out.append(c)
        i += 1
    return "".join(out)


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
