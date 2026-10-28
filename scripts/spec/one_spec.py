#!/usr/bin/env python3
"""S13 ONE SPECIFICATION (docs/spec/README.md §3): docs/spec is the one normative statement of the
language, and docs/language is being absorbed into it.

Every Markdown file under docs/language is a document not yet absorbed, listed in
docs/spec/unabsorbed.tsv with the chapters that will take it. A file not listed is refused, a row
whose file is gone is refused (delete the row with the file), and a row added relative to the base
revision is refused (the list only shrinks). Absorbing a document moves its rules into entries and
its useful rationale under them, its tracking material to ROADMAP or KNOWN_HOLES, and deletes it.

This is a source check with no compiler, run by check_docs_drift.sh on every push. It lives apart
from spec.py so that the gates which run the compiler do not depend on every docs/language file.

  one_spec.py [--root DIR] [--base REV]     check (base: merge-base with origin/main, or HEAD~1)
  one_spec.py --self-test                   plant one defect per refusal in a scratch copy
"""
import argparse, glob, os, shutil, subprocess, sys, tempfile

LIST = "docs/spec/unabsorbed.tsv"


def rows_of(text):
    return [l.split("\t") for l in text.split("\n") if l.strip() and not l.startswith("#")]


def check(root, base=None):
    fails = []
    p = os.path.join(root, LIST)
    if not os.path.exists(p):
        return [f"{LIST} is missing"]
    listed = {}
    for r in rows_of(open(p, encoding="utf-8").read()):
        if len(r) != 2:
            fails.append(f"{LIST}: row {r} is not (path, chapters)"); continue
        if r[0] in listed:
            fails.append(f"{LIST}: {r[0]} listed twice")
        listed[r[0]] = r[1]
        if not os.path.exists(os.path.join(root, r[0])):
            fails.append(f"{LIST}: {r[0]} no longer exists (remove its row in the commit that deletes it)")
    for f in sorted(glob.glob(os.path.join(root, "docs/language/**/*.md"), recursive=True)):
        rel = os.path.relpath(f, root)
        if rel not in listed:
            fails.append(f"{rel} is a new document under docs/language: the language is stated in docs/spec (add an entry or a rationale there)")
    if base:
        old = subprocess.run(["git", "-C", root, "show", f"{base}:{LIST}"], capture_output=True, text=True)
        if old.returncode == 0:
            before = {r[0] for r in rows_of(old.stdout)}
            for k in listed:
                if k not in before:
                    fails.append(f"{LIST}: {k} was added relative to {base} (the list only shrinks)")
    return fails, len(listed)


def default_base(root):
    for args in (["merge-base", "HEAD", "origin/main"], ["rev-parse", "HEAD~1"]):
        r = subprocess.run(["git", "-C", root] + args, capture_output=True, text=True)
        if r.returncode == 0 and r.stdout.strip():
            head = subprocess.run(["git", "-C", root, "rev-parse", "HEAD"], capture_output=True, text=True).stdout.strip()
            if r.stdout.strip() != head:
                return r.stdout.strip()
    return None


def self_test(root):
    bad = 0
    def scratch():
        d = tempfile.mkdtemp()
        shutil.copytree(os.path.join(root, "docs/spec"), os.path.join(d, "docs/spec"))
        shutil.copytree(os.path.join(root, "docs/language"), os.path.join(d, "docs/language"))
        return d
    for name, plant, frag in (
            ("a new document under docs/language", lambda d: open(os.path.join(d, "docs/language/NEW_RULES.md"), "w").write("# new\n"), "NEW_RULES.md is a new document"),
            ("a listed document deleted without its row", lambda d: os.remove(os.path.join(d, rows_of(open(os.path.join(d, LIST)).read())[0][0])), "no longer exists")):
        d = scratch(); plant(d)
        fails, _ = check(d)
        if any(frag in f for f in fails): print(f"  ok   control: {name} is refused")
        else: print(f"  FAIL control: {name} was not refused"); bad += 1
        shutil.rmtree(d)
    return bad


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--root", default=os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))
    ap.add_argument("--base")
    ap.add_argument("--self-test", action="store_true")
    a = ap.parse_args()
    if a.self_test:
        sys.exit(1 if self_test(a.root) else 0)
    res = check(a.root, a.base or default_base(a.root))
    if isinstance(res, list):
        fails, n = res, 0
    else:
        fails, n = res
    for f in fails: print(f"  FAIL [S13] {f}")
    if not fails: print(f"  ok   S13 one specification: {n} docs/language files still to absorb, none new")
    sys.exit(1 if fails else 0)


if __name__ == "__main__":
    main()
