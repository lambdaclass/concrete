#!/usr/bin/env python3
"""Replace the first occurrence of <old> with <new> in <file>.

Exits 3 if the anchor is absent. Gates that edit real example projects use this
so a fixture drifting out from under them is a hard failure: an in-place `sed`
that matches nothing succeeds silently, which would leave the gate asserting
verdicts about a file it never modified.
"""
import sys

def main() -> int:
    if len(sys.argv) != 4:
        sys.stderr.write("usage: replace_once.py <file> <old> <new>\n")
        return 2
    path, old, new = sys.argv[1], sys.argv[2], sys.argv[3]
    text = open(path).read()
    if old not in text:
        sys.stderr.write("ANCHOR MISSING in %s: %r\n" % (path, old[:60]))
        return 3
    open(path, "w").write(text.replace(old, new, 1))
    return 0

if __name__ == "__main__":
    sys.exit(main())
