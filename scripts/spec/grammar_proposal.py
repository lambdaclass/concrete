#!/usr/bin/env python3
"""Produce a word certificate from the canonical numeric CFG on stdin.

Header: nonterminal count, terminal count, start. Each subsequent line is
lhs followed by tN/nN symbols. Row order and empty right sides are significant.
The producer never reads verifier instructions or a reference prediction table.
"""
import sys


def build_proposal(n, terminals, start, rows):
    columns = terminals + 1
    positions = sum(1 + len(rhs) for _, rhs in rows)
    if not 0 <= start < n or any(not 0 <= lhs < n for lhs, _ in rows):
        raise ValueError("invalid nonterminal")
    if any(k not in ("n", "t") or not 0 <= x < (n if k == "n" else terminals)
           for _, rhs in rows for k, x in rhs):
        raise ValueError("invalid symbol")

    # Named vectors follow the public certificate format. Zero denotes absence.
    cells = [0] * (n * columns)
    ranks = [0] * n
    nullable = [0] * n
    productive = [0] * n
    first = [[0] * columns for _ in range(n)]
    follow = [[0] * columns for _ in range(n)]
    prefix = [0] * positions
    suffix = [0] * positions
    yields = [0] * positions
    starts = [[0] * columns for _ in range(positions)]
    follow[start][0] = 1

    def add(vector, slot, *parents):
        nonlocal changed
        if not vector[slot] and all(parents):
            vector[slot] = 1 + max(parents, default=0)
            changed = True

    changed = True
    while changed:
        changed = False
        base = 0
        for lhs, rhs in rows:
            end = base + len(rhs)
            prefix[base] = suffix[end] = yields[end] = 1
            for pos, (kind, x) in enumerate(rhs, base):
                if kind == "n":
                    add(prefix, pos + 1, prefix[pos], nullable[x])
            for pos in reversed(range(base, end)):
                kind, x = rhs[pos - base]
                if kind == "t":
                    add(starts[pos], x + 1)
                    add(yields, pos, yields[pos + 1])
                    continue
                add(suffix, pos, nullable[x], suffix[pos + 1])
                add(yields, pos, productive[x], yields[pos + 1])
                for c in range(columns):
                    add(follow[x], c, suffix[pos + 1], follow[lhs][c])
                    if c:
                        add(starts[pos], c, first[x][c])
                        add(starts[pos], c, nullable[x], starts[pos + 1][c])
                        add(follow[x], c, starts[pos + 1][c])
            add(nullable, lhs, suffix[base])
            add(productive, lhs, yields[base])
            for c in range(1, columns):
                add(first[lhs], c, starts[base][c])
            base = end + 1

    dependencies = [set() for _ in range(n)]
    base = 0
    for i, (lhs, rhs) in enumerate(rows, 1):
        for c in range(columns):
            if starts[base][c] or (suffix[base] and follow[lhs][c]):
                slot = lhs * columns + c
                if cells[slot]:
                    raise ValueError(f"prediction conflict at ({lhs}, {c})")
                cells[slot] = i
        for pos, (kind, x) in enumerate(rhs, base):
            if kind == "n" and prefix[pos]:
                dependencies[lhs].add(x)
        base += 1 + len(rhs)

    if not all(productive):
        raise ValueError("unproductive grammar")
    for _ in range(n):
        for lhs, children in enumerate(dependencies):
            if not ranks[lhs] and all(ranks[x] for x in children):
                ranks[lhs] = 1 + max((ranks[x] for x in children), default=0)
    if not all(ranks):
        raise ValueError("nullable-prefix cycle")
    words = (cells + ranks + nullable + productive +
             [v for row in first + follow for v in row] + prefix + suffix + yields +
             [v for row in starts for v in row] + [0])
    if max(words) >= 2**64:
        raise ValueError("certificate does not fit UInt64")
    return words


def main():
    n, terminals, start = map(int, sys.stdin.readline().split())
    rows = []
    for line in sys.stdin:
        lhs, *rhs = line.split()
        rows.append((int(lhs), [(sym[0], int(sym[1:])) for sym in rhs]))
    print(" ".join(map(str, build_proposal(n, terminals, start, rows))))


if __name__ == "__main__":
    main()
