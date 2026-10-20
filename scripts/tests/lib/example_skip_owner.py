#!/usr/bin/env python3
"""Skip-owner checks for scripts/tests/check_examples.sh.

A manifest `skip owner=<gate>` is accepted only when both hold:

  ref  <owner> <src>       the owner names <src> as a complete path token, or, for a
                           .../src/main.con, the example directory as a complete
                           token. A longer name (<dir>.backup, main.con.disabled,
                           another file under <dir>) or a top-level directory plus a
                           common word does not count.
  runs <owner> <workflow>  some `run:` command of the workflow (inline or block)
                           INVOKES the owner: after shell comments are stripped and
                           the command is split on ; && || | ( ), the owner is the
                           executable of a simple command, or the script operand of
                           bash/sh/python3, after optional VAR=value assignments and
                           `env`/`timeout N` prefixes. A step name, a comment, or the
                           path as an ordinary argument (echo, printf, an assignment)
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
    """The shell text of every `run:` key, inline or block scalar."""
    lines = open(path).read().split("\n")
    cmds, i = [], 0
    while i < len(lines):
        m = re.match(r"^(\s*(?:-\s+)?)run:\s*(.*)$", lines[i])
        if not m:
            i += 1
            continue
        key_col = len(m.group(1))          # column of `run`, including any `- `
        rest = m.group(2)
        i += 1
        if rest.strip()[:1] in ("|", ">"):
            # Block scalar: lines indented deeper than the key; a sibling key at
            # the key's column (`name:` after `- run: |`) ends it.
            while i < len(lines) and (not lines[i].strip()
                                      or len(lines[i]) - len(lines[i].lstrip()) > key_col):
                cmds.append(lines[i])
                i += 1
        else:
            cmds.append(rest)
    return [strip_shell_comment(c) for c in cmds]


def norm(p):
    return p[2:] if p.startswith("./") else p


def invoked_scripts(command):
    found = []
    for simple in re.split(r"&&|\|\||[;|()]", command):
        try:
            words = shlex.split(simple, posix=True)
        except ValueError:
            continue
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
            k += 1
            while k < len(words) and words[k].startswith("-"):
                k += 1
            if k < len(words):
                found.append(norm(words[k]))
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
