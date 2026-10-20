#!/usr/bin/env python3
"""Skip-owner checks for scripts/tests/check_examples.sh.

A manifest `skip owner=<gate>` is accepted only when both hold:

  ref  <owner> <src>       the owner names <src> as a complete path token, or, for a
                           .../src/main.con, the example directory as a complete
                           token. A longer name (<dir>.backup, main.con.disabled,
                           another file under <dir>) or a top-level directory plus a
                           common word does not count.
  runs <owner> <workflow>  some INLINE `run:` command of the workflow INVOKES the
                           owner (block scalars are not read; see run_commands): after shell comments are stripped and
                           the command is split on UNQUOTED ; && || | ( ), the owner
                           is the executable of a simple command, or the script
                           operand of bash/sh/python3, after optional VAR=value
                           assignments and `env`/`timeout N` prefixes. A step name, a
                           comment, quoted text, the path as an ordinary argument
                           (echo, printf, an assignment), a syntax check (bash -n,
                           -o noexec) or a command string (-c) does not count.

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

    Block scalars (`run: |`) are deliberately ignored: their lines share quote,
    heredoc and continuation state across newlines, and reading them line by line
    took printed text (a heredoc body, a multi-line echo) for commands. An owner
    must therefore be invoked by an inline `run:` -- fail-closed, and every CI
    owner today is spelled that way."""
    cmds = []
    for line in open(path).read().split("\n"):
        m = re.match(r"^\s*(?:-\s+)?run:\s*(.*)$", line)
        if not m:
            continue
        rest = m.group(1)
        if rest.strip()[:1] in ("|", ">", ""):
            continue
        cmds.append(strip_shell_comment(rest))
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


def interpreter_script(name, args):
    """The script a bash/sh/python3 invocation runs, or None when it runs none
    (a syntax check -n / -o noexec, a command string -c, a module -m)."""
    k = 0
    while k < len(args) and args[k].startswith("-") and args[k] != "-":
        opt = args[k]
        if opt in ("-c", "-m") or opt.startswith("--"):
            return None
        if name in ("bash", "sh"):
            if opt in ("-o", "+o"):
                if k + 1 < len(args) and args[k + 1] == "noexec":
                    return None
                k += 2
                continue
            if "n" in opt[1:] or "c" in opt[1:]:
                return None
        k += 1
    return args[k] if k < len(args) else None


def invoked_scripts(command):
    found = []
    for words in simple_commands(command):
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
            continue
        if words[k] in ("bash", "sh", "python3"):
            script = interpreter_script(words[k], words[k + 1:])
            if script is not None:
                found.append(norm(script))
        else:
            found.append(norm(words[k]))
    return found


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
