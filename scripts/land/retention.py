#!/usr/bin/env python3
"""RETENTION.tsv: parse it, and give each path its one governing row (ROADMAP R-0562).

Usage: retention.py check MANIFEST < paths     problems, one per line (empty: valid and complete)
       retention.py govern MANIFEST < paths    "<path>\\t<row path list>\\t<disposition>" per path

The rule (also the manifest's header): a row's first field is a space-separated list of paths; a
path is governed by the row that lists its longest prefix (a listed path equal to it, or a listed
directory containing it); legacy/P is governed as P. Rows carry a status (open | resolved)
about the legacy/ copy only (a retained row's root file is never deleted through the manifest). No path may be listed twice, so the governing row is unique. A path with
no listed prefix is uncovered, which `check` reports. Lives in scripts/land because the deletion
check of the legacy/ move will read deletion authority from `govern`, and the gate that holds the
manifest to the tree (scripts/tests/check_retention_manifest.sh) uses the same code.
"""
import sys

DISP = {"retained", "archived", "migrate", "retire"}
HDR = ["path", "kind", "disposition", "destination", "owner", "deletion_criterion", "status"]
STATUS = {"open", "resolved"}


def parse(path):
    problems, rows, listed = [], [], {}
    lines = [l.rstrip("\n") for l in open(path) if not l.startswith("#") and l.strip()]
    if not lines or lines[0].split("\t") != HDR:
        return ["header is not: " + "\t".join(HDR)], rows, listed
    for n, l in enumerate(lines[1:], 2):
        f = l.split("\t")
        if len(f) != len(HDR) or any(not x.strip() for x in f):
            problems.append(f"row {n}: needs {len(HDR)} non-empty tab-separated fields: {l[:60]}")
            continue
        if f[2] not in DISP:
            problems.append(f"row {n}: disposition {f[2]!r} is not one of {sorted(DISP)}")
        if f[6] not in STATUS:
            problems.append(f"row {n}: status {f[6]!r} is not one of {sorted(STATUS)}")
        if f[2] == "retained" and not (f[5].startswith("root: ") and "; legacy/ copy" in f[5]):
            # status releases only the legacy/ copy, so a retained row must say both halves
            problems.append(f"row {n}: a retained row's criterion must read 'root: ...; legacy/ copy...: ...'")
        for p in f[0].split():
            p = p.rstrip("/")
            if p in listed:
                problems.append(f"row {n}: {p} is already listed by the row for {listed[p][0]!r}")
                continue
            listed[p] = (f[0], f[2])
        rows.append(f)
    return problems, rows, listed


def governing(listed, path):
    if path.startswith("legacy/"):
        path = path[len("legacy/"):]  # after the move, legacy/P is the asset P
    parts = path.split("/")
    for i in range(len(parts), 0, -1):
        hit = listed.get("/".join(parts[:i]))
        if hit:
            return hit
    return None


def main():
    if len(sys.argv) != 3 or sys.argv[1] not in ("check", "govern"):
        sys.exit(__doc__)
    problems, rows, listed = parse(sys.argv[2])
    paths = [l.strip() for l in sys.stdin if l.strip()]
    if sys.argv[1] == "check":
        for p in problems:
            print(p)
        uncovered = sorted({p for p in paths if governing(listed, p) is None})
        for p in uncovered:
            print("no row governs " + p)
    else:
        for p in paths:
            g = governing(listed, p)
            print(f"{p}\t{g[0] if g else '-'}\t{g[1] if g else 'uncovered'}")


if __name__ == "__main__":
    main()
