#!/usr/bin/env python3
"""Find Lean references to the native-evaluation constants Lean 4.35 removed (lean4#14953).

usage: removed_native_names.py FILE...      prints `path:line: snippet` per offending use; exit 1 if any
       removed_native_names.py --selftest   checks the scanner on built-in cases; exit 1 on a miss

`Lean.ofReduceBool`, `Lean.ofReduceNat`, `Lean.trustCompiler`, `Lean.reduceBool` and `Lean.reduceNat`
exist up to Lean 4.34 and not in 4.35. A double-backtick literal (``Lean.trustCompiler) or a bare
identifier must resolve when the file elaborates, so either one stops the file compiling on 4.35.
A single-backtick literal (`Lean.trustCompiler) is an unchecked name and is the one allowed spelling.

NO LEXING, ON PURPOSE. Comments are NOT skipped and strings are NOT parsed: the replay probe is Lean
source held in a string and elaborated later, and a comment-stripper that does not know where strings
end can be told by a `"/-"` in one string and a `"-/"` in another to blank whatever lies between
(Codex review, 2026-10-23). So every mention counts, prose included; prose names these constants in
single backticks, which the scan accepts. Over-reporting is the safe direction.
"""
import re
import sys

REMOVED = ("ofReduceBool", "ofReduceNat", "trustCompiler", "reduceBool", "reduceNat")
USE = re.compile(r"(`*)(?:@)?\bLean\.(" + "|".join(REMOVED) + r")\b")


def offences(text):
    found = []
    for lineno, line in enumerate(text.split("\n"), 1):
        for m in USE.finditer(line):
            if len(m.group(1)) != 1:  # zero backticks: an identifier; two or more: a resolved literal
                found.append((lineno, line.strip()))
    return found


SELFTEST = [
    ("  [`Lean.ofReduceBool, `Lean.ofReduceNat, `Lean.trustCompiler]", 0),
    ("if a == ``Lean.trustCompiler then", 1),
    ("theorem t : true = true := Lean.ofReduceBool _ _ rfl", 1),
    ("exact @Lean.ofReduceNat 1 1 rfl", 1),
    ("-- rests on `Lean.ofReduceBool`, fine in prose", 0),
    ("-- but a bare Lean.reduceBool in prose is reported too (no lexing)", 1),
    ('def a := "/-"\ndef broken := ``Lean.ofReduceBool\ndef b := "-/"', 1),
    ('def a := "--" ++ toString ``Lean.trustCompiler', 1),
    ("def s := r#\"a == ``Lean.ofReduceBool\"#", 1),
    ("def ok := ``Lean.ofReduceBoolean", 0),
]


def selftest():
    bad = 0
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
            for lineno, snippet in offences(f.read()):
                print(f"{path}:{lineno}: {snippet[:160]}")
                hits += 1
    return 1 if hits else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
