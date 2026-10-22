#!/usr/bin/env python3
"""Find identifier tokens in Lean sources, outside comments and string literals.

  lean_idents.py NAME[,NAME...] FILE...

Prints `FILE:LINE: <source line>` for every occurrence of one of the NAMEs as a whole identifier
token (or as a component of a dotted name such as `Proof.PackageIdentity`), in code. Skipped:
line comments (`--` to end of line), block comments and docstrings (`/- ... -/`, nested, including
`/--` and `/-!`), string literals (with escapes) and character literals. `noPackageIdentity` is a
different identifier from `PackageIdentity` and is not reported.

Known limit: code interpolated inside a string literal (`s!"...{e}..."`) is not scanned.
Exit status: 0 always when the files were read (the output is the verdict); 2 on a usage or read
error, so a caller can tell "found nothing" from "could not look".
"""
import sys

IDENT_START = set("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_")
IDENT_REST = IDENT_START | set("0123456789'!?")


def scan(text, names):
    hits = []
    i, n, line = 0, len(text), 1
    depth = 0  # block-comment nesting
    while i < n:
        c = text[i]
        if c == "\n":
            line += 1
            i += 1
            continue
        if depth:
            if text.startswith("/-", i):
                depth += 1
                i += 2
            elif text.startswith("-/", i):
                depth -= 1
                i += 2
            else:
                i += 1
            continue
        if text.startswith("/-", i):
            depth = 1
            i += 2
            continue
        if text.startswith("--", i):
            while i < n and text[i] != "\n":
                i += 1
            continue
        if c == '"':
            i += 1
            while i < n and text[i] != '"':
                if text[i] == "\\":
                    i += 1
                elif text[i] == "\n":
                    line += 1
                i += 1
            i += 1
            continue
        if c == "'" and (i == 0 or text[i - 1] not in IDENT_REST):
            # a character literal: 'x' or '\n' / '\x41' / '\u{..}'
            j = i + 1
            if j < n and text[j] == "\\":
                j += 1
                while j < n and text[j] != "'" and text[j] != "\n":
                    j += 1
            else:
                j += 1
            if j < n and text[j] == "'":
                i = j + 1
                continue
            i += 1
            continue
        if c in IDENT_START and (i == 0 or text[i - 1] not in IDENT_REST):
            j = i
            while j < n and text[j] in IDENT_REST:
                j += 1
            if text[i:j] in names:
                hits.append(line)
            i = j
            continue
        i += 1
    return hits


def main(argv):
    if len(argv) < 2:
        print(__doc__, file=sys.stderr)
        return 2
    names = set(argv[0].split(","))
    for path in argv[1:]:
        try:
            text = open(path, encoding="utf-8").read()
        except OSError as e:
            print(f"lean_idents: cannot read {path}: {e}", file=sys.stderr)
            return 2
        lines = text.split("\n")
        for ln in sorted(set(scan(text, names))):
            print(f"{path}:{ln}: {lines[ln - 1].strip()}")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
