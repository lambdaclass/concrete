#!/usr/bin/env python3
"""CHANGELOG fragments: each branch adds its own file under changelog.d/, so no two branches edit one file.

  changelog.py --check     exit 1 when CHANGELOG.md was edited by hand (its stamp no longer matches
                           its text), exit 2 on a malformed fragment; check_docs_drift.sh runs this
  changelog.py --fold      move every fragment into CHANGELOG.md (newest first, at the top of "## Major
                           Milestones"), delete the fragments and restamp. Run on main after a landing
                           (a train, or any time): landings are serialized, so a fold never conflicts
  changelog.py --print     print the whole changelog as it reads with the pending fragments folded in
                           (the roadmap gates read Completed Task headings from it)
  changelog.py --restamp   accept a deliberate edit of CHANGELOG.md's history (a typo in an old entry)
  changelog.py --stamp-initial
                           the migration's one-time stamp of an unstamped CHANGELOG.md

A FRAGMENT is changelog.d/YYYY-MM-DD-<slug>.md (the date the change landed or was written; the slug is
the branch or topic, lowercase letters, digits and hyphens). Its first line is the entry's `### `
heading, exactly as it will appear in CHANGELOG.md (a `### Completed Task R-NNNN` heading retires a
roadmap task the same way); its body is ordinary Markdown with no `#`/`##` headings. Several entries in
one fragment are allowed. Fragments fold in date order, newest first; one date folds by slug.

THE STAMP is one line under "## Major Milestones", a digest of everything else in CHANGELOG.md. Only
--fold and --restamp write it, so an entry added to CHANGELOG.md by hand (instead of as a fragment)
fails --check. Before the migration stamps it, --check reports "not migrated" and passes.

--root overrides the repository root (a test seam for the gate's negative controls).
"""
import argparse
import glob
import hashlib
import os
import re
import sys

ANCHOR = "## Major Milestones"
STAMP = re.compile(r"^<!-- changelog stamp: ([0-9a-f]{32}) \(scripts/gen/changelog\.py; add a changelog\.d/ "
                   r"fragment, never edit this file by hand\) -->\n", re.M)
STAMP_FMT = ("<!-- changelog stamp: {d} (scripts/gen/changelog.py; add a changelog.d/ fragment, never edit "
             "this file by hand) -->\n")
NAME = re.compile(r"^(\d{4}-\d{2}-\d{2})-([a-z0-9][a-z0-9-]*)\.md$")


def die(msg):
    print(f"changelog: {msg}", file=sys.stderr)
    sys.exit(2)


def read(path):
    try:
        return open(path, encoding="utf-8").read()
    except OSError as e:
        die(f"cannot read {path}: {e}")


def fragments(root):
    out = []
    for p in sorted(glob.glob(os.path.join(root, "changelog.d", "*.md"))):
        base = os.path.basename(p)
        if base == "README.md":
            continue
        m = NAME.match(base)
        if not m:
            die(f"changelog.d/{base}: a fragment is named YYYY-MM-DD-<slug>.md (lowercase slug)")
        text = read(p).strip("\n")
        lines = text.split("\n")
        if not lines or not lines[0].startswith("### ") or len(lines[0]) < 8:
            die(f"changelog.d/{base}: the first line must be the entry's '### ' heading")
        for ln in lines:
            if re.match(r"^#{1,2} ", ln):
                die(f"changelog.d/{base}: a fragment holds '### ' entries only, no '#'/'##' heading: {ln[:50]}")
            if re.match(r"^(<{7}|={7}|>{7})", ln):
                die(f"changelog.d/{base}: unresolved conflict marker")
        if "changelog stamp:" in text:
            die(f"changelog.d/{base}: a fragment cannot carry the changelog stamp")
        out.append((m.group(1), m.group(2), base, text))
    # newest first; within one date, by slug
    out.sort(key=lambda f: f[1])
    out.sort(key=lambda f: f[0], reverse=True)
    return out


def digest(body):
    return hashlib.sha256(body.encode("utf-8")).hexdigest()[:32]


def split(text, path):
    if text.count(ANCHOR + "\n") != 1:
        die(f"{path} must hold exactly one '{ANCHOR}' heading")
    stamps = STAMP.findall(text)
    if len(stamps) > 1:
        die(f"{path} holds more than one changelog stamp")
    body = STAMP.sub("", text, count=1)
    return (stamps[0] if stamps else None), body


def stamped(body):
    i = body.index(ANCHOR + "\n") + len(ANCHOR) + 1
    return body[:i] + STAMP_FMT.format(d=digest(body)) + body[i:]


def folded(body, frs):
    if not frs:
        return body
    i = body.index(ANCHOR + "\n") + len(ANCHOR) + 1
    rest = body[i:].lstrip("\n")
    return body[:i] + "\n" + "\n\n".join(f[3] for f in frs) + "\n\n" + rest


def ledgers_migrated(root):
    """The ledger migration stamps CHANGELOG.md and gives every bug record a ledger block in one step, so
    a record with a block says the stamp must be there: a deleted stamp is a failure, not 'not migrated'."""
    for p in glob.glob(os.path.join(root, "docs", "bugs", "[0-9][0-9][0-9]_*.md")):
        if "<!-- bug-ledger" in read(p):
            return True
    return False


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--root", default=os.path.abspath(os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "..")))
    g = ap.add_mutually_exclusive_group(required=True)
    for f in ("check", "fold", "print", "restamp", "stamp_initial"):
        g.add_argument("--" + f.replace("_", "-"), action="store_true", dest=f)
    a = ap.parse_args()
    path = os.path.join(a.root, "CHANGELOG.md")
    text = read(path)
    if "changelog stamp:" in text and not STAMP.search(text):
        die(f"{path} holds a damaged changelog stamp line; restore it with --restamp after reviewing the file")
    stamp, body = split(text, path)
    frs = fragments(a.root)
    if a.print:
        sys.stdout.write(folded(body, frs))
        return 0
    if a.stamp_initial:
        if stamp:
            die(f"{path} is already stamped")
        open(path, "w", encoding="utf-8").write(stamped(body))
        print(f"changelog: stamped {path}")
        return 0
    if a.check:
        if stamp is None:
            if ledgers_migrated(a.root):
                print(f"changelog: {path} has lost its stamp, but the ledgers are migrated (the bug records carry "
                      "ledger blocks); review the file and run --restamp", file=sys.stderr)
                return 1
            print(f"changelog: not migrated yet ({path} has no stamp); {len(frs)} well-formed fragments")
            return 0
        if stamp != digest(body):
            print(f"changelog: {path} was edited by hand (its stamp {stamp} does not match its text); add the "
                  "entry as a changelog.d/ fragment instead, or, for a deliberate edit of an old entry, run "
                  "python3 scripts/gen/changelog.py --restamp", file=sys.stderr)
            return 1
        print(f"changelog: {path} matches its stamp; {len(frs)} well-formed pending fragments")
        return 0
    if stamp is None:
        die(f"{path} has no stamp; the migration (scripts/gen/migrate_ledgers.sh) stamps it first")
    if a.restamp:
        open(path, "w", encoding="utf-8").write(stamped(body))
        print(f"changelog: restamped {path}")
        return 0
    if stamp != digest(body):
        die(f"{path} was edited by hand since its last stamp; review the edit, --restamp, then fold")
    open(path, "w", encoding="utf-8").write(stamped(folded(body, frs)))
    for f in frs:
        os.remove(os.path.join(a.root, "changelog.d", f[2]))
    print(f"changelog: folded {len(frs)} fragments into {path}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
