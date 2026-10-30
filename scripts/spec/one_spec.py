#!/usr/bin/env python3
"""S13 ONE SPECIFICATION (docs/spec/README.md §3): docs/spec is the one normative statement of the
language, and docs/language is being absorbed into it.

Every Markdown file under docs/language is a document not yet absorbed, listed in
docs/spec/unabsorbed.tsv with the chapters that will take it. A file not listed is refused, a row
whose file is gone is refused (delete the row with the file), and a row added relative to the base
revision is refused (the list only shrinks). Absorbing a document moves its rules into entries and
its useful rationale under them, its tracking material to ROADMAP or KNOWN_HOLES, and deletes it.

docs/spec/absorb_inventory.tsv is the wider plan: every statement about the language found anywhere
outside docs/spec, with its target. Every file it names must exist, with one `*` verdict row, so a slice
that deletes or moves a file updates the plan in the same commit, and every unabsorbed docs/language
file must be in it.

This is a source check with no compiler, run by check_docs_drift.sh on every push. It lives apart
from spec.py so that the gates which run the compiler do not depend on every docs/language file.

  one_spec.py [--root DIR] [--base REV]     check (base: $ONE_SPEC_BASE, else the merge-base with
                                            origin/main, else HEAD~1; no base is a failure, and
                                            ONE_SPEC_BASE=none skips the history check outside CI)
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
        ok_rev = subprocess.run(["git", "-C", root, "rev-parse", "--verify", "--quiet", base + "^{commit}"],
                                capture_output=True, text=True)
        if ok_rev.returncode != 0:
            return [f"base revision '{base}' does not resolve (fetch history, or pass --base)"], len(listed)
        old = subprocess.run(["git", "-C", root, "show", f"{base}:{LIST}"], capture_output=True, text=True)
        # a base without the list is the inventory's first revision: nothing to compare
        if old.returncode == 0:
            before = {r[0] for r in rows_of(old.stdout)}
            for k in listed:
                if k not in before:
                    fails.append(f"{LIST}: {k} was added relative to {base} (the list only shrinks)")
    fails += check_inventory(root, listed)
    return fails, len(listed)


INVENTORY = "docs/spec/absorb_inventory.tsv"
ACTIONS = {"cite", "absorb", "open", "rationale", "none", "keep", "cite-only", "design-record", "absorb-delete"}


def check_inventory(root, listed):
    """The absorption inventory (slice 0's plan): every row has eight cells and a known action, every
    file it names exists (a slice that deletes a file removes its rows in the same commit), every file
    has exactly one `*` verdict row, and every unabsorbed docs/language file is in it."""
    p = os.path.join(root, INVENTORY)
    if not os.path.exists(p):
        return [f"{INVENTORY} is missing"]
    fails, verdicts, files = [], {}, set()
    for i, l in enumerate(open(p, encoding="utf-8").read().split("\n"), 1):
        if not l.strip() or l.startswith("#") or l.startswith("file\t"):
            continue
        c = l.split("\t")
        if len(c) != 8:
            fails.append(f"{INVENTORY}:{i}: {len(c)} cells, not 8"); continue
        if c[6] not in ACTIONS:
            fails.append(f"{INVENTORY}:{i}: unknown action '{c[6]}'")
        files.add(c[0])
        if c[1] == "*":
            verdicts[c[0]] = verdicts.get(c[0], 0) + 1
    for f in sorted(files):
        if not os.path.exists(os.path.join(root, f)):
            fails.append(f"{INVENTORY}: {f} no longer exists (remove its rows in the commit that deletes or moves it)")
        if verdicts.get(f, 0) != 1:
            fails.append(f"{INVENTORY}: {f} has {verdicts.get(f, 0)} `*` verdict rows, not 1")
    for f in sorted(listed):
        if f not in files:
            fails.append(f"{INVENTORY}: {f} is listed in {LIST} but has no inventory rows")
    return fails


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
        # works whatever is left to absorb, an empty list and no docs/language included
        d = tempfile.mkdtemp()
        shutil.copytree(os.path.join(root, "docs/spec"), os.path.join(d, "docs/spec"))
        # the inventory names files across the repository: keep only the rows the copy can resolve
        inv = os.path.join(d, INVENTORY)
        if os.path.exists(inv):
            keep = [l for l in open(inv, encoding="utf-8").read().split("\n")
                    if not l.strip() or l.startswith("#") or l.startswith("file\t")
                    or l.split("\t")[0].startswith(("docs/spec/", "docs/language/"))]
            open(inv, "w", encoding="utf-8").write("\n".join(keep))
        if os.path.isdir(os.path.join(root, "docs/language")):
            shutil.copytree(os.path.join(root, "docs/language"), os.path.join(d, "docs/language"))
        else:
            os.makedirs(os.path.join(d, "docs/language"))
        return d
    def deleted_with_row(d):
        open(os.path.join(d, LIST), "a").write("docs/language/PLANTED.md\tREADME\n")
        # listed, then deleted: the row stays behind
    def inventory_row_of_gone_file(d):
        open(os.path.join(d, INVENTORY), "a").write(
            "docs/language/GONE.md\t*\tplanted\t-\t-\t-\tkeep\t\n")
    for name, plant, frag in (
            ("a new document under docs/language", lambda d: open(os.path.join(d, "docs/language/NEW_RULES.md"), "w").write("# new\n"), "NEW_RULES.md is a new document"),
            ("a listed document deleted without its row", deleted_with_row, "PLANTED.md no longer exists"),
            ("an inventory row whose file is gone", inventory_row_of_gone_file, "GONE.md no longer exists")):
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
    base = a.base or os.environ.get("ONE_SPEC_BASE") or default_base(a.root)
    if base == "none":
        if os.environ.get("CI"):
            print("  FAIL [S13] ONE_SPEC_BASE=none is not allowed in CI"); sys.exit(1)
        base = None
        print("  SKIP S13 history (ONE_SPEC_BASE=none)")
    elif base is None:
        print("  FAIL [S13] no base revision to check that the list only shrinks (fetch origin/main, pass --base, or set ONE_SPEC_BASE=none outside CI)")
        sys.exit(1)
    res = check(a.root, base)
    if isinstance(res, list):
        fails, n = res, 0
    else:
        fails, n = res
    for f in fails: print(f"  FAIL [S13] {f}")
    if not fails: print(f"  ok   S13 one specification: {n} docs/language files still to absorb, none new; the absorption inventory names only existing files")
    sys.exit(1 if fails else 0)


if __name__ == "__main__":
    main()
