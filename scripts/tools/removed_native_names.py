#!/usr/bin/env python3
"""Find Lean references to the native-evaluation constants Lean 4.35 removed (lean4#14953).

usage: removed_native_names.py FILE...      prints `path:line: snippet` per offending use; exit 1 if any
       removed_native_names.py --selftest   checks the scanner on built-in cases; exit 1 on a miss

`Lean.ofReduceBool`, `Lean.ofReduceNat`, `Lean.trustCompiler`, `Lean.reduceBool` and `Lean.reduceNat`
exist up to Lean 4.34 and not in 4.35. A double-backtick literal (``Lean.trustCompiler) or a bare
identifier must resolve when the file elaborates, so either one stops the file compiling on 4.35.
A single-backtick literal (`Lean.trustCompiler) is an unchecked name and is the one allowed spelling;
comments are ignored. String literals are scanned like code, because the replay probe is Lean source
held in a string and elaborated later, by whichever toolchain runs it.
"""
import re
import sys

REMOVED = ("ofReduceBool", "ofReduceNat", "trustCompiler", "reduceBool", "reduceNat")
USE = re.compile(r"(`*)(?:@)?\bLean\.(" + "|".join(REMOVED) + r")\b")


def strip_comments(text):
    """Blank out `--` line comments and nested `/- -/` block comments, keeping line structure."""
    out, i, depth, n = [], 0, 0, len(text)
    while i < n:
        if depth == 0 and text.startswith("--", i):
            j = text.find("\n", i)
            j = n if j < 0 else j
            out.append(" " * (j - i))
            i = j
        elif text.startswith("/-", i):
            depth += 1
            out.append("  ")
            i += 2
        elif depth > 0 and text.startswith("-/", i):
            depth -= 1
            out.append("  ")
            i += 2
        elif depth > 0:
            out.append("\n" if text[i] == "\n" else " ")
            i += 1
        else:
            out.append(text[i])
            i += 1
    if depth:
        # A `/-` inside a string literal would otherwise blank the rest of the file from the scan.
        raise ValueError("unterminated block comment (a `/-` in a string?): the scan would be blind")
    return "".join(out)


def offences(text):
    found = []
    for lineno, line in enumerate(strip_comments(text).split("\n"), 1):
        for m in USE.finditer(line):
            if len(m.group(1)) != 1:  # zero backticks: an identifier; two or more: a resolved literal
                found.append((lineno, line.strip()))
    return found


SELFTEST = [
    ("  [`Lean.ofReduceBool, `Lean.ofReduceNat, `Lean.trustCompiler]", 0),
    ("if a == ``Lean.trustCompiler then", 1),
    ("theorem t : true = true := Lean.ofReduceBool _ _ rfl", 1),
    ("exact @Lean.ofReduceNat 1 1 rfl", 1),
    ("-- rests on `Lean.ofReduceBool` and ``Lean.trustCompiler", 0),
    ("/- block ``Lean.reduceBool\n   still a comment Lean.reduceNat -/ def x := 1", 0),
    ("def s := r#\"a == ``Lean.ofReduceBool\"#", 1),
    ("def ok := ``Lean.ofReduceBoolean", 0),
]


def selftest():
    bad = 0
    try:
        offences('def s := "/-" -- opens a comment the lexer never sees')
        print("selftest: an unterminated block comment was not refused")
        bad += 1
    except ValueError:
        pass
    for src, want in SELFTEST:
        got = len(offences(src))
        if got != want:
            print(f"selftest: {src!r}: {got} offences, expected {want}")
            bad += 1
    return bad


def main(argv):
    if argv == ["--selftest"]:
        return 1 if selftest() else 0
    hits = 0
    for path in argv:
        with open(path, encoding="utf-8") as f:
            try:
                found = offences(f.read())
            except ValueError as e:
                print(f"{path}: {e}")
                hits += 1
                continue
            for lineno, snippet in found:
                print(f"{path}:{lineno}: {snippet[:160]}")
                hits += 1
    return 1 if hits else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
