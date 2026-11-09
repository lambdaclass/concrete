#!/usr/bin/env python3
"""Shared reviewer-output contract reader (scripts/review/output_contract.py).

Every automated review output in Concrete — branch reviews (scripts/review/review.sh),
owner sign-off reviewer outputs (scripts/land/signoff.sh), and review-evidence
records — parses through this single module.

Contract for a reviewer's raw output:
  1. No repository-access failure text (sandbox startup failure, bwrap error,
     repository reads failed) in the output or log, and no `REPOSITORY: unreadable`
     line anywhere in the output. Any such indicator immediately produces `blocked`.
  2. Reviewer process exit code (`--rc`) must be 0; otherwise `error` (unless
     `--incomplete` is set for a bounded_run double-timeout, which produces `blocked`).
  3. The last non-empty line (after stripping markdown emphasis `` ` `` and `*`
     and surrounding whitespace) must be `VERDICT: <value>`, where `<value>` is in
     the caller-supplied `--verdicts` set (e.g. `clean,findings` or `ACCEPT,REJECT`).
     If not, the output is `refused` when refusal text is present, otherwise `noverdict`.
  4. The penultimate non-empty line must be `REPOSITORY: readable`. Otherwise `blocked`.
  5. At least one `READ: <path>` line must appear before the penultimate line, and
     every `READ: <path>` line must name a clean relative path that exists as a file
     (blob or symlink blob) in `--commit`'s git tree, or be explicitly declared
     by the harness as a supplied review-root file. Otherwise `blocked`.
  6. Every source location and identifier citation resolves against the reviewed tree,
     removed side of --base..--commit, an ABSENT report or pinned external name.
     Named anchors occur within the cited range. Explicit text:name links check text
     occurrence only and cannot support a positive verdict. Harness files never count
     as evidence. Positive verdicts require a supporting location.
     All failures are collected in the distinct `invalid` status (not a verdict).

CLI output (stdout, two lines):
  status: <verdict|invalid|blocked|refused|noverdict|error>
  reason: <human-readable explanation>
"""

from pathlib import Path

import argparse
import fnmatch
import os
import posixpath
import re
import subprocess
import sys

REFUSAL_RE = re.compile(
    r"(I can.?t|I cannot|I won.?t|I.?m (not able|unable)|unable to) "
    r"(help|assist|comply|review|continue|provide)"
    r"|cyber ?security"
    r"|content (filter|policy)"
    r"|usage polic"
    r"|safety (system|filter|policy)"
    r"|flagged (as|for)"
    r"|potentially (harmful|malicious)"
    r"|invalid_prompt"
    r"|violat(es|ion of) .*polic",
    re.IGNORECASE,
)

REPOSITORY_FAILURE_RE = re.compile(
    r"sandbox (could not|failed to) (start|initialize)"
    r"|bwrap: operation not permitted"
    r"|repository reads failed"
    r"|could not read the repository"
    r"|repository verification failed",
    re.IGNORECASE,
)


def normalize_line(line: str) -> str:
    """Strip whitespace and markdown emphasis (` and *) around directive lines."""
    return line.replace("`", "").replace("*", "").strip()


def commit_blob_paths(repo: str, commit: str) -> set[str]:
    """Return the set of file (blob) paths in `commit`'s tree."""
    proc = subprocess.run(
        ["git", "-C", repo, "ls-tree", "-r", "--full-tree", commit],
        capture_output=True,
        text=True,
        check=False,
    )
    if proc.returncode != 0:
        raise ValueError(f"cannot read tree for commit {commit}: {proc.stderr.strip()}")
    paths: set[str] = set()
    for raw in proc.stdout.splitlines():
        if "\t" not in raw:
            continue
        meta, path = raw.split("\t", 1)
        parts = meta.split()
        if len(parts) >= 2 and parts[1] == "blob":
            paths.add(path)
    return paths


def is_clean_relative_path(path: str) -> bool:
    if not path or path.startswith("/") or "\x00" in path:
        return False
    segments = path.split("/")
    if any(seg in ("", ".", "..") for seg in segments):
        return False
    return posixpath.normpath(path) == path



# Fixed grammar: Markdown links or bare path:line[-line][,line] locations.
IDENTIFIER = r"[A-Za-z_][\w]*[!?']?(?:\.[A-Za-z_][\w]*[!?']?)*"
LOCATION = re.compile(r"(?P<path>(?:file://)?[A-Za-z0-9_./-]+)(?::(?P<lines>[0-9]+(?:[-–][0-9]+)?(?:,[0-9]+(?:[-–][0-9]+)?)*)|#L(?P<anchor>[0-9]+(?:-L[0-9]+)?))")


# Evidence about the validator must not bootstrap identifier authority. Keep this
# declared scope beside the contract; explicit locations may still cite these files.
REVIEW_HARNESS_FILES = tuple(Path(__file__).with_name("harness-files.txt").read_text().splitlines())
IDENTIFIER_EXCLUSIONS = (
    "legacy/*", "ISSUES.tsv", "scripts/review/fixtures/*",
    "scripts/review/test_*.py", "scripts/review/external-names.tsv",
    "scripts/review/external-toolchains.tsv",
    *REVIEW_HARNESS_FILES, "_review/*",
)


def identifier_source(path):
    return not any(fnmatch.fnmatchcase(path, pattern) for pattern in IDENTIFIER_EXCLUSIONS)


def citation_error(text, repo, commit, paths, positive, base=None, review_root=None, harness_files=()):
    errors = []
    harness_files = set(harness_files)
    cache = {}
    def blob(path):
        if path not in cache:
            proc = subprocess.run(["git", "-C", repo, "show", f"{commit}:{path}"], capture_output=True, check=True)
            cache[path] = proc.stdout.decode("utf-8", errors="replace")
        return cache[path]
    def token(name, content):
        return re.search(r"(?<![\w])" + re.escape(name) + r"(?![\w])", content) is not None
    def resolve(path, exact=False):
        if path.startswith("file://"):
            absolute = path[7:]
            # Roots come from the caller, never from components guessed from a URL.
            for root in (review_root, os.path.abspath(repo)):
                if root and absolute.startswith(root.rstrip("/") + "/"):
                    candidate = absolute[len(root.rstrip("/")) + 1:]
                    return candidate if is_clean_relative_path(candidate) and candidate in paths else None
            return None
        if path in paths and is_clean_relative_path(path):
            return path
        if "/" not in path and not exact:
            candidates = [p for p in paths if not p.startswith("legacy/") and p.rsplit("/", 1)[-1] == path]
            return candidates[0] if len(candidates) == 1 else None
        return None
    basenames = {p.rsplit("/", 1)[-1] for p in paths}
    def filename(name):
        return name in paths or name in basenames
    def start(location):
        return int(re.split(r"[-–,]", location.group("lines") or location.group("anchor"))[0])
    for link in re.finditer(r"\]\(([^)\s]+)\)", text):
        target = link.group(1)
        if ("#L" in target or re.search(r":[0-9]", target)) and not LOCATION.fullmatch(target):
            errors.append(f"citation: unsupported location grammar '{target}'")
    # Extensionless prose (for example UTC-5:30) is a location only when the
    # reviewed tree supplies that filename. Dotted/slash paths remain checked
    # even when missing, so fabricated source paths still fail closed.
    locations = [m for m in LOCATION.finditer(text)
                 if filename(m.group("path")) or "." in m.group("path") or "/" in m.group("path")
                 or text[:m.start()].endswith("](")]

    valid = 0
    text_labels = set()
    for m in locations:
        raw = m.group("path")
        path = resolve(raw, exact=text[:m.start()].endswith("]("))
        # A basename label always describes its target, even if a root copy exists.
        following = re.match(r"`?\]\(([^)]+)\)", text[m.end():])
        if "/" not in raw and following:
            full = LOCATION.fullmatch(following.group(1))
            if full and full.group("path").rsplit("/", 1)[-1] == raw and start(full) == start(m):
                path = resolve(full.group("path"), exact=True)
            else:
                errors.append(f"citation: basename label '{m.group()}' disagrees with link target")
                continue
        # A separate parenthesized basename citation may sit on either side of
        # its own full Markdown link. Only immediate adjacency supplies context.
        if "/" not in raw and not following:
            before = re.search(r"\[[^\]\n]*\]\(([^)]+)\)\s*\(`?$", text[:m.start()])
            after = re.match(r"`?\)\s*\[[^\]\n]*\]\(([^)]+)\)", text[m.end():])
            adjacent = before or after
            if adjacent:
                full = LOCATION.fullmatch(adjacent.group(1))
                if full and full.group("path").rsplit("/", 1)[-1] == raw and start(full) == start(m):
                    path = resolve(full.group("path"), exact=True)
                else:
                    errors.append(f"citation: basename label '{m.group()}' disagrees with link target")
                    continue
        relative = raw
        if raw.startswith("file://"):
            absolute = raw[7:]
            roots = [r.rstrip("/") for r in (review_root, os.path.abspath(repo)) if r]
            root = next((r for r in roots if absolute.startswith(r + "/")), None)
            if root is None:
                errors.append(f"citation: link '{raw}' is outside the reviewed roots")
                continue
            relative = absolute[len(root) + 1:]
        if relative in harness_files:
            errors.append(f"citation: harness file '{relative}' is not evidence")
            continue
        if path is None:
            errors.append(f"citation: file '{raw}' does not resolve uniquely")
            continue
        ranges = (m.group("lines") or m.group("anchor").replace("-L", "-")).replace("–", "-").split(",")
        lines = blob(path).splitlines()
        selected = []
        for item in ranges:
            nums = item.split("-"); lo, hi = int(nums[0]), int(nums[-1])
            if not 1 <= lo <= hi <= len(lines):
                errors.append(f"citation: range '{m.group()}' outside 1..{len(lines)}")
            else:
                selected.extend(lines[lo-1:hi])
        # A named link is a stronger claim: its identifier occurs in the range.
        prefix = text[:m.start()]
        named = re.search(r"\[`([^`]+)`\]\($", prefix)
        if named and named.group(1).startswith("text:"):
            text_labels.add((named.start(1), named.end(1)))
            name = named.group(1)[5:]
            if not re.fullmatch(IDENTIFIER, name) or not token(name, "\n".join(selected)):
                errors.append(f"citation: text anchor '{name}' absent from '{m.group()}'")
        if named and not filename(named.group(1)) and re.fullmatch(IDENTIFIER, named.group(1)):
            components = named.group(1).split(".")
            if not token(components[-1], "\n".join(selected)) or not all(token(part, blob(path)) for part in components[:-1]):
                errors.append(f"citation: anchor '{named.group(1)}' absent from '{m.group()}'")
        if selected and not (named and named.group(1).startswith("text:")):
            valid += 1
    # Resolve identifiers against actual blobs, never prompt text or the allowlist itself.
    sources = sorted(path for path in paths if identifier_source(path))
    content = ""
    if sources:
        proc = subprocess.run(["git", "-C", repo, "grep", "-I", "-h", "-e", ".", commit, "--",
                               *[":(literal)" + path for path in sources]], capture_output=True, check=False)
        if proc.returncode not in (0, 1):
            raise ValueError("citation: cannot read reviewed tree identifiers")
        content = proc.stdout.decode("utf-8", errors="replace")
    removed = ""
    if base:
        proc = subprocess.run(["git", "-C", repo, "diff", "--no-ext-diff", "--unified=0", base, commit, "--", ".", *[":(exclude)" + p for p in IDENTIFIER_EXCLUSIONS]], capture_output=True, text=True, check=True)
        removed = "\n".join(l[1:] for l in proc.stdout.splitlines() if l.startswith("-") and not l.startswith("---"))
    absent = set(re.findall(r"^ABSENT: (" + IDENTIFIER + r")$", text, re.M))
    external = set()
    pins = set()
    if "lean-toolchain" in paths:
        pins.add(blob("lean-toolchain").strip())
    if "scripts/review/external-toolchains.tsv" in paths:
        for row in blob("scripts/review/external-toolchains.tsv").splitlines():
            fields = row.split("\t")
            if len(fields) == 2 and fields[0].startswith(("cpython:", "bash:")) and fields[1].startswith("https://"):
                pins.add(fields[0])
    if "scripts/review/external-names.tsv" in paths:
        for row in blob("scripts/review/external-names.tsv").splitlines():
            fields = row.split("\t")
            if len(fields) == 3 and fields[0] in pins and fields[2]:
                external.add(fields[1])
    for m in re.finditer(r"`([^`\n]+)`", text):
        name = m.group(1)
        if filename(name):
            continue
        if name.startswith("text:"):
            if (m.start(1), m.end(1)) not in text_labels:
                errors.append(f"citation: text name '{name}' requires a named location link")
            continue
        if name.startswith("proposed:") and name != "proposed:":
            if positive:
                errors.append(f"citation: proposed name '{name}' requires a FINDINGS/REJECT verdict")
            continue
        if not re.fullmatch(IDENTIFIER, name) or not ("." in name or "_" in name or re.search(r"[a-z][A-Z]", name)):
            continue
        if name in absent or name in external:
            continue
        if not all(token(part, content) or token(part, removed) for part in name.split(".")):
            errors.append(f"citation: identifier '{name}' unresolved")
    if positive and not valid:
        errors.append("citation: positive verdict requires at least one resolvable file:line citation")
    return "; ".join(dict.fromkeys(errors)) or None


def evaluate_output(
    text: str,
    log_text: str,
    commit: str,
    repo: str,
    verdicts: list[str],
    rc: int = 0,
    incomplete: bool = False,
    base: str | None = None,
    review_root: str | None = None,
    harness_files: tuple[str, ...] = (),
) -> tuple[str, str]:
    combined = f"{text}\n{log_text}" if log_text else text
    norm_nonempty = [normalize_line(line) for line in text.splitlines()]
    norm_nonempty = [line for line in norm_nonempty if line]

    for line in norm_nonempty:
        if re.match(r"^REPOSITORY:[ \t]*unreadable$", line):
            return "blocked", "reviewer reported REPOSITORY: unreadable"

    if REPOSITORY_FAILURE_RE.search(combined):
        return "blocked", "reviewer output or log reported a repository or sandbox failure"

    if incomplete:
        return "blocked", f"reviewer timed out twice under bounded_run (exit {rc})"

    if rc != 0:
        return "error", f"reviewer exited {rc}"

    if not norm_nonempty:
        if REFUSAL_RE.search(combined):
            return "refused", "reviewer refused the prompt"
        return "noverdict", "reviewer output is empty"

    last = norm_nonempty[-1]
    verdict_match = re.match(r"^VERDICT:[ \t]*(.+)$", last)
    matched_verdict = None
    if verdict_match:
        candidate = verdict_match.group(1).strip()
        matched_verdict = next((v for v in verdicts if v.casefold() == candidate.casefold()), None)

    if matched_verdict is None:
        if REFUSAL_RE.search(combined):
            return "refused", "reviewer refused the prompt without a valid verdict line"
        return (
            "noverdict",
            f"last non-empty line is not a valid VERDICT ({'|'.join(verdicts)}) line",
        )

    try:
        valid_blobs = commit_blob_paths(repo, commit)
        problem = citation_error(strip_directives(text), repo, commit, valid_blobs,
                                 matched_verdict.lower() in ("clean", "accept"), base, review_root, harness_files)
    except (ValueError, OSError, subprocess.SubprocessError) as exc:
        return "blocked", f"citation lookup failed: {exc}"
    if problem:
        return "invalid", problem

    if len(norm_nonempty) < 2:
        return "blocked", "missing REPOSITORY: readable penultimate line and READ: <path> lines"

    penultimate = norm_nonempty[-2]
    if not re.match(r"^REPOSITORY:[ \t]*readable$", penultimate):
        return (
            "blocked",
            f"penultimate non-empty line is '{penultimate}', not 'REPOSITORY: readable'",
        )

    read_paths: list[str] = []
    for line in norm_nonempty[:-2]:
        if re.match(r"^REPOSITORY:", line):
            return (
                "blocked",
                f"unexpected REPOSITORY directive before penultimate line: '{line}'",
            )
        m = re.match(r"^READ:(.*)$", line)
        if m:
            read_paths.append(m.group(1).strip())

    if not read_paths:
        return "blocked", "reviewer output has no 'READ: <path>' line"

    for rp in read_paths:
        if not is_clean_relative_path(rp):
            return "blocked", f"READ: path '{rp}' is not a clean relative path"
        if rp not in valid_blobs and rp not in harness_files:
            return (
                "blocked",
                f"READ: path '{rp}' does not exist as a file in commit {commit}",
            )

    return matched_verdict, f"verified {len(read_paths)} READ path(s), REPOSITORY: readable, VERDICT: {matched_verdict}"


def strip_directives(text: str) -> str:
    out_lines: list[str] = []
    for line in text.splitlines():
        norm = normalize_line(line)
        if re.match(r"^(VERDICT|REPOSITORY|READ):", norm):
            continue
        out_lines.append(line)
    return "\n".join(out_lines)


def main() -> int:
    parser = argparse.ArgumentParser(description="Validate reviewer output against the Concrete contract.")
    parser.add_argument("--commit", help="Reviewed commit SHA whose tree validates READ: paths")
    parser.add_argument("--review-root", help="Exact absolute root of the verified review archive")
    parser.add_argument("--harness-file", action="append", default=[], help="Explicit harness-owned review-root file (READ only)")
    parser.add_argument("--base", help="Base of reviewed diff for removed identifiers")
    parser.add_argument("--repo", default=".", help="Git repository path (default: .)")
    parser.add_argument("--verdicts", help="Comma-separated allowed VERDICT values (e.g. clean,findings)")
    parser.add_argument("--rc", type=int, default=0, help="Reviewer process exit status")
    parser.add_argument("--incomplete", action="store_true", help="Reviewer timed out twice under bounded_run")
    parser.add_argument("--log", help="Optional path to reviewer stderr/log file")
    parser.add_argument("--strip", action="store_true", help="Strip READ/REPOSITORY/VERDICT directive lines from input")
    parser.add_argument("input_file", nargs="?", help="Reviewer output file (default: stdin)")
    args = parser.parse_args()

    if args.input_file and args.input_file != "-":
        with open(args.input_file, "r", encoding="utf-8", errors="replace") as f:
            text = f.read()
    else:
        text = sys.stdin.read()

    if args.strip:
        stripped = strip_directives(text)
        if stripped:
            sys.stdout.write(stripped + "\n")
        return 0

    if not args.commit or not args.verdicts:
        parser.error("--commit and --verdicts are required unless --strip is set")

    verdicts = [v.strip() for v in args.verdicts.split(",") if v.strip()]
    log_text = ""
    if args.log:
        try:
            with open(args.log, "r", encoding="utf-8", errors="replace") as lf:
                log_text = lf.read()
        except OSError:
            log_text = ""

    status, reason = evaluate_output(
        text=text,
        log_text=log_text,
        commit=args.commit,
        repo=args.repo,
        verdicts=verdicts,
        rc=args.rc,
        incomplete=args.incomplete,
        base=args.base,
        review_root=args.review_root,
        harness_files=tuple(args.harness_file),
    )
    print(f"status: {status}")
    print(f"reason: {reason}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
