#!/usr/bin/env python3
"""Every local Markdown link names a path that exists and, into Markdown, a heading that exists.

Usage: doc_links.py [--root DIR]
  --root  the tree to check (default: this repository); a test seam for the gate's controls

A LINK is an inline `[text](target)` / `![alt](target)` or a reference definition `[id]: target`,
outside fenced code and code spans (GitHub renders neither as a link). External targets
(http:, https:, mailto:, Zola's `@/`) are not checked.

THE PATH. A target resolves against the directory of the file that holds it, with two exceptions
that resolve against the repository root: a target with a leading `/` (GitHub reads it that way),
and any target in a changelog.d/ fragment, because scripts/gen/changelog.py folds fragments
verbatim into the root CHANGELOG.md and their links are written for that place.

THE ANCHOR. `#frag` (same file) or `path.md#frag` must name an anchor of that Markdown file:
  - a heading's GitHub slug: the heading's rendered text, lowercased, every character that is not a
    letter, mark, digit, space, hyphen or underscore removed, each space turned into a hyphen; the
    second heading with one slug gets `-1`, the third `-2` (github-slugger, which GitHub uses);
  - an explicit HTML id (`<a id="REF-3"></a>`, `name=` too), matched exactly.
Into CHANGELOG.md, and from a fragment to its own `#frag`, the anchors are those of CHANGELOG.md with
the pending fragments folded in by scripts/gen/changelog.py (its fold, not a copy of it).
HTML comments, fenced code and code spans hold no headings, ids or links.
A heading slug is compared case-insensitively (GitHub lowercases the fragment before its lookup);
a fragment is URL-decoded first. An anchor into a file that is not Markdown (`Foo.lean#L10`) or into
a directory is not checked: GitHub's line anchors are not headings.
"""

import argparse
import html
import re
import subprocess
import sys
import unicodedata
from pathlib import Path
from urllib.parse import unquote

DEFAULT_ROOT = Path(__file__).resolve().parents[3]
SKIP_PARTS = {".git", ".lake", ".cache", "node_modules"}
FRAGMENT_DIRS = ("changelog.d",)  # folded into the root CHANGELOG.md, so root-relative
INLINE = re.compile(r"!?\[(?:[^\[\]]|\[[^\[\]]*\])*\]\(\s*(<[^>\n]*>|[^)\s]+)(?:\s+[\"'(][^)\n]*)?\)")
IMAGE = re.compile(r"!\[[^\[\]]*\]\(\s*(<[^>\n]*>|[^)\s]+)(?:\s+[\"'(][^)\n]*)?\)")
REFDEF = re.compile(r"^ {0,3}\[[^\]\n]+\]:\s*(<[^>\n]*>|\S+)", re.MULTILINE)
EXTERNAL = re.compile(r"^(?:[a-zA-Z][a-zA-Z0-9+.-]*:|@/|//)")
FENCE = re.compile(r"^ {0,3}(`{3,}|~{3,})")
ATX = re.compile(r"^ {0,3}(#{1,6})(?:[ \t]+(.*?))?(?:[ \t]+#+)?[ \t]*$")
SETEXT = re.compile(r"^ {0,3}(=+|-+)[ \t]*$")
HTML_ID = re.compile(r"<[a-zA-Z][^>]*?\s(?:id|name)\s*=\s*[\"']([^\"']+)[\"']")
# A code span: a backtick run, then the same run again, never across a blank line (a paragraph ends).
CODE_SPAN = re.compile(r"(?<!`)(`+)(?!`)((?:(?!\n[ \t]*\n).)+?)(?<!`)\1(?!`)", re.DOTALL)
COMMENT = re.compile(r"<!--.*?-->", re.DOTALL)


def _spaces(m):
    """The match with every character but a newline turned into a space (offsets and lines kept)."""
    return re.sub(r"[^\n]", " ", m.group(0))


def blank_blocks(text):
    """The text with fenced code blocks and HTML comments blanked: neither holds headings or links."""
    out, fence = [], None
    for line in text.split("\n"):
        m = FENCE.match(line)
        if fence is None and m:
            fence = (m.group(1)[0], len(m.group(1)))
            out.append(" " * len(line))
        elif fence is not None:
            if m and m.group(1)[0] == fence[0] and len(m.group(1)) >= fence[1] \
                    and not line.strip().strip(fence[0]):
                fence = None
            out.append(" " * len(line))
        else:
            out.append(line)
    return COMMENT.sub(_spaces, "\n".join(out))


def blank_code(text):
    """blank_blocks, and code spans blanked too (a span may cross lines within a paragraph)."""
    return CODE_SPAN.sub(_spaces, blank_blocks(text))


def _strip_emphasis(s):
    """Remove paired emphasis delimiters; an unpaired `_` or `*` is a literal character."""
    for pat in (r"(?<![^\W_])(__?)(?=\S)(.+?)(?<=\S)\1(?![^\W_])", r"(\*{1,3})(?=\S)(.+?)(?<=\S)\1"):
        while True:
            t = re.sub(pat, r"\2", s)
            if t == s:
                break
            s = t
    return s


def heading_text(raw):
    """The rendered text of a heading's inline Markdown (what GitHub slugs)."""
    parts, pos = [], 0
    for m in CODE_SPAN.finditer(raw):
        parts.append(("md", raw[pos:m.start()]))
        parts.append(("code", m.group(2).strip() if m.group(2).strip() else m.group(2)))
        pos = m.end()
    parts.append(("md", raw[pos:]))
    out = []
    for kind, s in parts:
        if kind == "md":
            s = re.sub(r"\\([!-/:-@\[-`{-~])", lambda e: "\x00%d\x00" % ord(e.group(1)), s)
            s = re.sub(r"!\[[^\]]*\]\([^)]*\)", "", s)
            s = re.sub(r"\[([^\]]*)\]\([^)]*\)", r"\1", s)
            s = re.sub(r"\[([^\]]*)\]\[[^\]]*\]", r"\1", s)
            s = re.sub(r"<[^>]+>", "", s)
            s = _strip_emphasis(s)
            s = html.unescape(s)
            s = re.sub(r"\x00(\d+)\x00", lambda e: chr(int(e.group(1))), s)
        out.append(s)
    return "".join(out).strip()


def slug(text):
    keep = []
    for ch in text.lower():
        cat = unicodedata.category(ch)
        if ch in "-_ " or cat[0] in "LMN":
            keep.append("-" if ch == " " else ch)
    return "".join(keep)


def anchors(text):
    """(heading slugs, explicit ids) of a Markdown file, as GitHub assigns them."""
    seen, slugs = {}, set()
    prev = ""
    for line in blank_blocks(text).split("\n"):
        m = ATX.match(line)
        heading = None
        if m:
            heading = m.group(2) or ""
        elif SETEXT.match(line) and prev.strip() and not re.match(r"^ {0,3}([-*+>|#]|\d+[.)])", prev) \
                and not SETEXT.match(prev):
            heading = prev.strip()
        if heading is not None:
            base = slug(heading_text(heading))
            s, n = base, seen.get(base, 0)
            while s in slugs:
                n += 1
                s = f"{base}-{n}"
            seen[base] = n
            slugs.add(s)
        prev = line if not m else ""
    ids = set(HTML_ID.findall(blank_code(text)))
    return slugs, ids


def folded_changelog(root):
    """CHANGELOG.md as it reads with the pending fragments folded in, by changelog.py's own fold
    (the one owner of the fold order), or None when the tree has no foldable changelog."""
    sys.path.insert(0, str(DEFAULT_ROOT / "scripts" / "gen"))
    try:
        import changelog
        path = root / "CHANGELOG.md"
        _, body = changelog.split(changelog.read(str(path)), str(path))
        return changelog.folded(body, changelog.fragments(str(root)))
    except (SystemExit, ImportError, OSError, ValueError):
        return None
    finally:
        sys.path.pop(0)


def markdown_files(root):
    if (root / ".git").exists():
        r = subprocess.run(["git", "-C", str(root), "ls-files", "-z", "--cached", "--others",
                            "--exclude-standard", "--", "*.md"], capture_output=True)
        if r.returncode == 0:
            names = [n for n in r.stdout.decode().split("\0") if n]
            return sorted(root / n for n in names if (root / n).is_file())
    return sorted(p for p in root.rglob("*.md")
                  if not any(part in SKIP_PARTS for part in p.relative_to(root).parts))


def main():
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("--root", type=Path, default=DEFAULT_ROOT)
    root = ap.parse_args().root.resolve()

    cache = {}
    changelog = (root / "CHANGELOG.md").resolve()

    def anchors_of(p):
        """The anchors a link into p lands on. CHANGELOG.md is read with the pending fragments folded
        in, since that is the document a fragment's link (or a link to the changelog) reaches."""
        if p not in cache:
            text = folded_changelog(root) if p == changelog else None
            cache[p] = anchors(text if text is not None else p.read_text(errors="replace"))
        return cache[p]

    failures, links, frags = [], 0, 0
    for path in markdown_files(root):
        rel = path.relative_to(root)
        text = path.read_text(errors="replace")
        scan = blank_code(text)
        fragment = rel.parts[0] in FRAGMENT_DIRS and len(rel.parts) == 2 and rel.name != "README.md"
        base = root if fragment else path.parent
        # An image inside a link's text ([![badge](img)](target)) is a second link; key by position.
        found = {}
        for rx in (INLINE, IMAGE, REFDEF):
            for m in rx.finditer(scan):
                found.setdefault(m.start(1), m.group(1))
        for pos, target in sorted(found.items()):
            if target.startswith("<") and target.endswith(">"):
                target = target[1:-1]
            if EXTERNAL.match(target):
                continue
            local, _, frag = target.partition("#")
            local = unquote(local.split("?", 1)[0])
            line = text.count("\n", 0, pos) + 1
            if local.startswith("/"):  # GitHub reads a leading slash as the repository root
                dest = (root / local.lstrip("/")).resolve()
            elif local:
                dest = (base / local).resolve()
            else:  # same file; a fragment's own `#x` lands, once folded, in CHANGELOG.md
                dest = changelog if fragment and changelog.is_file() else path
            links += 1
            if local and not dest.exists():
                failures.append(f"{rel}:{line}: missing local link {target}")
                continue
            if not frag or not dest.is_file() or dest.suffix.lower() != ".md":
                continue
            frags += 1
            want = unquote(frag)
            slugs, ids = anchors_of(dest)
            if want in ids or want.lower() in slugs:
                continue
            where = dest.relative_to(root) if dest.is_relative_to(root) else dest
            failures.append(f"{rel}:{line}: no anchor #{want} in {where} (link {target})")

    for f in failures:
        print(f"FAIL {f}")
    if failures:
        print(f"DOC-LINKS: FAIL={len(failures)} CHECKED={links} ANCHORS={frags}")
        return 1
    print(f"DOC-LINKS: PASS CHECKED={links} ANCHORS={frags}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
