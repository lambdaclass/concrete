#!/usr/bin/env python3
"""Mechanical conflict resolution for landing trains (scripts/land/train.sh build).

  resolve_conflicts.py resolve <path>...   resolve the listed unmerged paths of the current merge
  resolve_conflicts.py corpus-keys <file>  refuse duplicate keys in an audit_bug_corpus.sh map

Run from the repository root while a merge is stopped on conflicts. Only three kinds of path are
resolved; everything else is reported as a REAL conflict and nothing is written for it:

  union ledgers      append-only ledgers (UNION below), and only when every conflict hunk is an
                     INSERTION on both sides (its diff3 base is empty): `git merge-file --union` then
                     keeps both sides' lines. Two edits of one existing line are a real conflict. In a
                     map-shaped ledger (MAP_LEDGERS) every inserted line must also be a map entry or
                     comment inside a `declare -A` map. A union can still duplicate a key, so
                     train.sh runs `corpus-keys` after every merge, and the generators that read the
                     TSV ledgers refuse duplicates themselves.
  whole generated    files a generator rewrites whole (WHOLE_GENERATED): ours is taken, and train.sh
                     regenerates the file once on the finished train.
  generated blocks   any other text file whose EVERY conflict hunk lies inside a
                     `<!-- BEGIN GENERATED: ... (scripts/gen/<generator>; ...) -->` block (or differs
                     only inside such spans on one line): ours is taken for those hunks and train.sh
                     regenerates the blocks. One hunk outside a block, a hunk that touches a marker
                     line, or a block whose generator is not in the plan makes it a real conflict.

"In the plan" means listed in LAND_REGEN_PLAN, the generators train.sh will run on the finished train
(colon-separated). A generator that merely exists is not enough: taking one side of a view is only
correct when that view is then rewritten from the merged records.

A path with no common ancestor, or deleted on one side, is resolved only when it is whole-generated.

Exit status: 0 every listed path was resolved and staged; 1 at least one real conflict (the report
names them; resolved paths are still written but the caller aborts the merge); 2 usage or I/O error.
"""
import os
import re
import subprocess
import sys

UNION = {
    "scripts/tests/audit_bug_corpus.sh",
    "CHANGELOG.md",
    "docs/bugs/discovery.tsv",
    "docs/project/candidates.tsv",
}
# whole generated file -> the generator that rewrites it
WHOLE_GENERATED = {
    "Concrete/BuildIdentity.lean": "scripts/gen/build_identity.sh",
}
# Map-shaped ledgers: a union is accepted only when every conflicting line is a map entry or comment.
MAP_LEDGERS = {"scripts/tests/audit_bug_corpus.sh"}


def regen_plan():
    """The generators train.sh WILL run after the merges (LAND_REGEN_PLAN, colon-separated). A block
    or file is resolved by regeneration only when its generator is in this plan; an empty plan
    resolves nothing, so a caller that forgets to pass it fails closed."""
    return {g for g in os.environ.get("LAND_REGEN_PLAN", "").split(":") if g}
BEGIN = re.compile(r"<!-- BEGIN GENERATED: [^>]*?\((scripts/gen/[^;)\s]+)")
END = re.compile(r"<!-- END GENERATED: ")
DECLARE = re.compile(r"^\s*declare -A (\w+)=\(\s*$")
KEY = re.compile(r"^\s*\[([^\]]+)\]=")
INLINE_SPAN = re.compile(r"<!-- BEGIN GENERATED: .*?-->.*?<!-- END GENERATED: .*?-->")


def die(msg, code=2):
    print(f"resolve_conflicts: {msg}", file=sys.stderr)
    sys.exit(code)


def git(*args, check=True, data=None):
    p = subprocess.run(["git", *args], input=data, capture_output=True)
    if check and p.returncode != 0:
        die(f"git {' '.join(args)} failed: {p.stderr.decode(errors='replace').strip()}")
    return p


def stage_blob(path, n):
    """The content of index stage n (1 base, 2 ours, 3 theirs), or None when the stage is absent."""
    p = git("show", f":{n}:{path}", check=False)
    return p.stdout if p.returncode == 0 else None


def merge_file(ours, base, theirs, *flags):
    """git merge-file on temporary copies; returns (exit status, merged bytes)."""
    import tempfile
    with tempfile.TemporaryDirectory() as d:
        paths = []
        for name, data in (("ours", ours), ("base", base), ("theirs", theirs)):
            fp = os.path.join(d, name)
            with open(fp, "wb") as f:
                f.write(data)
            paths.append(fp)
        p = subprocess.run(["git", "merge-file", "-p", *flags,
                            "-L", "ours", "-L", "base", "-L", "theirs", *paths], capture_output=True)
    if p.returncode < 0 or p.returncode > 127:
        die(f"git merge-file failed: {p.stderr.decode(errors='replace').strip()}")
    return p.returncode, p.stdout


def hunks(text):
    """Split merge-file output (merge or diff3 style) into ('ctx', lines) and ('conflict', ours,
    theirs, base) pieces (base is None without diff3). A malformed marker sequence returns None."""
    out, ctx = [], []
    lines = text.split("\n")
    i = 0
    while i < len(lines):
        ln = lines[i]
        if ln.startswith("<<<<<<< "):
            out.append(("ctx", ctx)); ctx = []
            ours, theirs, base, mode = [], [], None, "ours"
            i += 1
            while i < len(lines):
                l2 = lines[i]
                if l2.startswith("||||||| ") and mode == "ours":
                    mode = "base"; base = []
                elif l2 == "=======" and mode in ("ours", "base"):
                    mode = "theirs"
                elif l2.startswith(">>>>>>> ") and mode == "theirs":
                    break
                elif mode == "ours":
                    ours.append(l2)
                elif mode == "theirs":
                    theirs.append(l2)
                else:
                    base.append(l2)
                i += 1
            else:
                return None
            out.append(("conflict", ours, theirs, base))
        else:
            ctx.append(ln)
        i += 1
    out.append(("ctx", ctx))
    return out


def generated_only(path, ours, base, theirs):
    """True when every conflict hunk lies inside generated blocks whose generators will run."""
    plan = regen_plan()
    rc, merged = merge_file(ours, base, theirs)
    if rc == 0:
        return True, "no conflict left"
    try:
        pieces = hunks(merged.decode("utf-8"))
    except UnicodeDecodeError:
        return False, "not UTF-8 text"
    if pieces is None:
        return False, "malformed conflict markers"
    inside = None          # generator of the open multi-line block, or None
    for piece in pieces:
        if piece[0] == "ctx":
            for ln in piece[1]:
                stripped = INLINE_SPAN.sub("", ln)
                m = BEGIN.search(stripped)
                if m:
                    inside = m.group(1)
                elif END.search(stripped):
                    inside = None
            continue
        _, o, t, _b = piece
        for ln in o + t:
            if BEGIN.search(ln) or END.search(ln):
                # Marker lines inside a hunk: allowed only when the two sides differ solely inside
                # complete one-line spans (a table row carrying an inline generated span).
                break
        else:
            if inside is not None:
                if inside not in plan:
                    return False, f"conflict inside a block generated by {inside}, which the train will not run"
                continue
            return False, "a conflict hunk lies outside every generated block"
        strip = lambda xs: [INLINE_SPAN.sub("", x) for x in xs]
        if len(o) == len(t) and strip(o) == strip(t):
            gens = {m.group(1) for x in o + t for m in BEGIN.finditer(x)}
            missing = [g for g in gens if g not in plan]
            if missing:
                return False, f"inline generated span whose generator is missing: {', '.join(missing)}"
            continue
        return False, "a conflict hunk touches a BEGIN/END GENERATED marker line"
    return True, "every hunk is inside a generated block"


def union_ok(path, ours, base, theirs):
    """A ledger conflict is a union only when both sides INSERTED lines at the same place: the base
    side of every hunk is empty. Two edits of one existing line are a real conflict, and a union would
    keep both versions. In a map-shaped ledger every inserted line must also be a map entry (or a
    comment) inside a `declare -A` map, so executable code is never unioned."""
    rc, merged = merge_file(ours, base, theirs, "--diff3")
    if rc == 0:
        return True, "no conflict left"
    try:
        pieces = hunks(merged.decode("utf-8"))
    except UnicodeDecodeError:
        return False, "not UTF-8 text"
    if pieces is None:
        return False, "malformed conflict markers"
    in_map = False
    for piece in pieces:
        if piece[0] == "ctx":
            for ln in piece[1]:
                if DECLARE.match(ln):
                    in_map = True
                elif in_map and ln.strip() == ")":
                    in_map = False
            continue
        _, o, t, b = piece
        if b is None:
            return False, "no base section to prove the conflict is insert-only"
        if any(x.strip() for x in b):
            return False, "both sides edit the same existing lines (not an append)"
        if path in MAP_LEDGERS:
            if not in_map:
                return False, "inserted lines outside every declare -A map"
            for x in o + t:
                if x.strip() and not KEY.match(x) and not x.lstrip().startswith("#"):
                    return False, f"inserted line is not a map entry: {x.strip()[:60]}"
    return True, "union of both sides' inserted lines (ledger)"


def write_and_stage(path, data):
    with open(path, "wb") as f:
        f.write(data)
    git("add", "--", path)


def resolve(paths):
    real = []
    for path in paths:
        base, ours, theirs = (stage_blob(path, n) for n in (1, 2, 3))
        if path in WHOLE_GENERATED:
            if WHOLE_GENERATED[path] not in regen_plan():
                real.append((path, f"generated by {WHOLE_GENERATED[path]}, which the train will not run"))
                continue
            if ours is None:
                real.append((path, "generated file deleted on the train side"))
                continue
            write_and_stage(path, ours)
            print(f"  resolved {path}: generated whole; ours taken, regenerated on the train")
            continue
        if ours is None or theirs is None:
            real.append((path, "deleted on one side (modify/delete)"))
            continue
        if path in UNION:
            b = base if base is not None else b""
            ok, why = union_ok(path, ours, b, theirs)
            if not ok:
                real.append((path, why))
                continue
            _, merged = merge_file(ours, b, theirs, "--union")
            write_and_stage(path, merged)
            print(f"  resolved {path}: {why}")
            continue
        if base is None:
            real.append((path, "added on both sides with different content"))
            continue
        ok, why = generated_only(path, ours, base, theirs)
        if not ok:
            real.append((path, why))
            continue
        _, merged = merge_file(ours, base, theirs, "--ours")
        write_and_stage(path, merged)
        print(f"  resolved {path}: {why}; ours taken, regenerated on the train")
    for path, why in real:
        print(f"  REAL CONFLICT {path}: {why}")
    return 1 if real else 0




def corpus_keys(path):
    """Every key of every `declare -A NAME=( ... )` map appears once, and no bug is in both
    BUG_TEST_MAP and SKIP_BUGS. bash keeps the LAST of two equal keys silently, so a union merge that
    kept both sides' entry for one bug would otherwise drop one of them without a word."""
    try:
        lines = open(path, encoding="utf-8").read().split("\n")
    except OSError as e:
        die(f"cannot read {path}: {e}")
    maps, cur, bad = {}, None, []
    for n, ln in enumerate(lines, 1):
        m = DECLARE.match(ln)
        if m:
            cur = m.group(1)
            maps.setdefault(cur, {})
            continue
        if cur is not None and ln.strip() == ")":
            cur = None
            continue
        if cur is not None:
            k = KEY.match(ln)
            if k:
                key = k.group(1)
                if key in maps[cur]:
                    bad.append(f"{path}:{n}: duplicate key [{key}] in {cur} (first at line {maps[cur][key]})")
                else:
                    maps[cur][key] = n
    if cur is not None:
        bad.append(f"{path}: map {cur} is not closed")
    both = sorted(set(maps.get("BUG_TEST_MAP", {})) & set(maps.get("SKIP_BUGS", {})))
    for key in both:
        bad.append(f"{path}: bug [{key}] is in both BUG_TEST_MAP (line {maps['BUG_TEST_MAP'][key]}) "
                   f"and SKIP_BUGS (line {maps['SKIP_BUGS'][key]})")
    for b in bad:
        print(f"  {b}")
    return 1 if bad else 0


def main(argv):
    if len(argv) >= 2 and argv[0] == "resolve":
        return resolve(argv[1:])
    if len(argv) == 2 and argv[0] == "corpus-keys":
        return corpus_keys(argv[1])
    die("usage: resolve_conflicts.py resolve <path>... | corpus-keys <file>")


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
