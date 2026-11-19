"""Split a unified git diff into review pieces (R-0544).

Usage: python3 -I split_diff.py <diff-file> <out-dir> <limit-bytes> [--no-groups]

Writes <out-dir>/piece-NNN.diff and <out-dir>/manifest.tsv (one line per piece:
"<file>\t<group>\t<bytes>\t<paths, comma separated>"). Prints the piece count.

Pieces are formed per file group (compiler, tests, scripts, docs), so one piece
never mixes, say, compiler sources with documentation, and each piece stays at
or under <limit-bytes>. A file whose diff alone exceeds the limit is split at
hunk boundaries, and each part repeats the file header so it reads as a valid
diff. A single hunk that is still too large is cut at line boundaries.
--no-groups packs everything as one group; review.sh uses it to halve a piece
that a reviewer refused.
"""
import io
import os
import re
import sys

GROUP_ORDER = ["compiler", "tests", "scripts", "docs"]


def classify(path):
    top = path.split("/", 1)[0]
    if path.startswith("scripts/tests/"):
        return "tests"
    if top in ("Concrete", "std", "proofs", "grammar") or path in (
        "Concrete.lean", "Main.lean", "lakefile.toml", "lake-manifest.json", "lean-toolchain"
    ):
        return "compiler"
    if top in ("tests", "lean_tests", "examples", "bench"):
        return "tests"
    if top in ("docs", "paper", "site", "research") or path.endswith(".md"):
        return "docs"
    return "scripts"


def parse_files(patch):
    """Return [(path, header, [hunk, ...])] from the bytes of a `git diff`."""
    files = []
    cur = None
    for line in io.BytesIO(patch):
        if line.startswith(b"diff --git "):
            # "diff --git a/X b/X": take the b/ side (the post-image path).
            rest = line[len(b"diff --git "):].removesuffix(b"\n")
            path = rest.split(b" b/", 1)[-1].decode("utf-8", errors="replace")
            cur = [path, [line], []]
            files.append(cur)
        elif cur is None:
            # Text before any file section: keep it as its own pseudo-file.
            cur = ["(preamble)", [], [[line]]]
            files.append(cur)
        elif line.startswith(b"@@"):
            cur[2].append([line])
        elif cur[2]:
            cur[2][-1].append(line)
        else:
            cur[1].append(line)
    return [(p, b"".join(h), [b"".join(x) for x in hs]) for p, h, hs in files]


HUNK_RE = re.compile(rb"^@@ -(\d+)(?:,(\d+))? \+(\d+)(?:,(\d+))? @@")


def split_hunk(hunk, budget):
    """Cut one oversized hunk at line boundaries. Every part gets its own,
    recomputed @@ header, so each part is itself a well-formed hunk."""
    lines = list(io.BytesIO(hunk))
    m = HUNK_RE.match(lines[0])
    if not m:
        # Not a hunk we can renumber: plain line cut.
        parts, part = [], b""
        for line in lines:
            if part and len(part + line) > budget:
                parts.append(part)
                part = b""
            part += line
        return parts + ([part] if part else [])
    old, new = int(m.group(1)), int(m.group(3))
    body = lines[1:]
    digits = len(str(max(old, new) + len(body)))
    room = max(budget - (12 + 4 * digits), 64)  # room for the synthesized header
    parts, cur = [], []

    def flush():
        nonlocal old, new
        o = sum(1 for l in cur if l[:1] in (b" ", b"-"))
        n = sum(1 for l in cur if l[:1] in (b" ", b"+"))
        parts.append(b"@@ -%d,%d +%d,%d @@\n" % (old, o, new, n) + b"".join(cur))
        old += o
        new += n

    for line in body:
        if cur and len(b"".join(cur) + line) > room:
            flush()
            cur = []
        cur.append(line)
    if cur:
        flush()
    return parts


def file_chunks(header, hunks, limit):
    """Yield diff texts for one file, each <= limit where possible."""
    whole = header + b"".join(hunks)
    if len(whole) <= limit or not hunks:
        yield whole
        return
    budget = max(limit - len(header), 256)
    buf = b""
    for h in hunks:
        if len(h) > budget:
            if buf:
                yield header + buf
                buf = b""
            for part in split_hunk(h, budget):
                yield header + part
            continue
        if buf and len(buf + h) > budget:
            yield header + buf
            buf = b""
        buf += h
    if buf:
        yield header + buf


def main():
    args = sys.argv[1:]
    no_groups = len(args) == 4 and args[-1] == "--no-groups"
    if not (len(args) == 3 or no_groups) or any(arg.startswith("--") for arg in args[:3]):
        print(__doc__, file=sys.stderr)
        return 2
    diff_file, out_dir, limit_text = args[:3]
    try:
        limit = int(limit_text)
        if limit < 1:
            raise ValueError
    except ValueError:
        print("split_diff.py: limit-bytes must be a positive integer", file=sys.stderr)
        return 2
    with open(diff_file, "rb") as f:
        patch = f.read()
    os.makedirs(out_dir, exist_ok=True)

    by_group = {}
    for path, header, hunks in parse_files(patch):
        g = "all" if no_groups else classify(path)
        for chunk in file_chunks(header, hunks, limit):
            by_group.setdefault(g, []).append((path, chunk))

    pieces = []  # (group, [paths], text)
    for g in (["all"] if no_groups else GROUP_ORDER):
        cur_paths, cur = [], b""
        for path, chunk in by_group.get(g, []):
            if cur and len(cur + chunk) > limit:
                pieces.append((g, cur_paths, cur))
                cur_paths, cur = [], b""
            cur += chunk
            if path not in cur_paths:
                cur_paths.append(path)
        if cur:
            pieces.append((g, cur_paths, cur))

    with open(os.path.join(out_dir, "manifest.tsv"), "w", encoding="utf-8") as m:
        for i, (g, paths, body) in enumerate(pieces, 1):
            name = "piece-%03d.diff" % i
            with open(os.path.join(out_dir, name), "wb") as f:
                f.write(body)
            m.write("%s\t%s\t%d\t%s\n" % (name, g, len(body), ",".join(paths)))
    print(len(pieces))
    return 0


if __name__ == "__main__":
    sys.exit(main())
