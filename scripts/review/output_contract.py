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
     (blob or symlink blob) in `--commit`'s git tree. Otherwise `blocked`.
  6. Every source location and identifier citation resolves against the reviewed tree,
     removed side of --base..--commit, an ABSENT report or pinned external name.
     Named anchors occur within the cited range. Positive verdicts require a location.
     All failures are collected in the distinct `invalid` status (not a verdict).

CLI output (stdout, two lines):
  status: <verdict|invalid|blocked|refused|noverdict|error>
  reason: <human-readable explanation>
"""

import argparse
import fnmatch
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
LOCATION = re.compile(r"(?P<path>(?:file://)?[A-Za-z0-9_./-]+\.[A-Za-z0-9]+)(?::(?P<lines>[0-9]+(?:[-–][0-9]+)?(?:,[0-9]+(?:[-–][0-9]+)?)*)|#L(?P<anchor>[0-9]+(?:-L[0-9]+)?))")


# Evidence about the validator must not bootstrap identifier authority. Keep this
# declared scope beside the contract; explicit locations may still cite these files.
IDENTIFIER_EXCLUSIONS = (
    "legacy/*", "ISSUES.tsv", "scripts/review/fixtures/*",
    "scripts/review/test_*.py", "scripts/review/external-names.tsv",
)


def identifier_source(path):
    return not any(fnmatch.fnmatchcase(path, pattern) for pattern in IDENTIFIER_EXCLUSIONS)


def citation_error(text, repo, commit, paths, positive, base=None):
    errors = []
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
            parts = path[7:].split("/")
            roots = {p.split("/", 1)[0] for p in paths if "/" in p}
            for i, part in enumerate(parts):
                if part in roots:
                    candidate = "/".join(parts[i:])
                    return candidate if candidate in paths else None
            # Root-level files have no directory component to recognize.
            candidate = parts[-1]
            return candidate if candidate in paths else None
        if "/" not in path and not exact:
            candidates = [p for p in paths if p.rsplit("/", 1)[-1] == path]
            return candidates[0] if len(candidates) == 1 and not candidates[0].startswith("legacy/") else None
        if path in paths and is_clean_relative_path(path):
            return path
        return None
    def filename(name):
        return bool(re.search(r"\.(lean|py|sh|md|tsv|json|yml|yaml|toml|txt|log)$", name))
    for link in re.finditer(r"\]\(([^)\s]+)\)", text):
        target = link.group(1)
        if ("#L" in target or re.search(r"\.[A-Za-z]+:[0-9]", target)) and not LOCATION.fullmatch(target):
            errors.append(f"citation: unsupported location grammar '{target}'")
    locations = list(LOCATION.finditer(text))
    valid = 0
    for m in locations:
        raw = m.group("path")
        path = resolve(raw, exact=text[:m.start()].endswith("]("))
        # A basename label always describes its target, even if a root copy exists.
        following = re.match(r"`?\]\(([^)]+)\)", text[m.end():])
        if "/" not in raw and following:
            full = LOCATION.fullmatch(following.group(1))
            def start(location):
                return int(re.split(r"[-–,]", location.group("lines") or location.group("anchor"))[0])
            if full and full.group("path").rsplit("/", 1)[-1] == raw and start(full) == start(m):
                path = resolve(full.group("path"), exact=True)
            else:
                errors.append(f"citation: basename label '{m.group()}' disagrees with link target")
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
        if named and not filename(named.group(1)) and re.fullmatch(IDENTIFIER, named.group(1)) and not token(named.group(1), "\n".join(selected)):
            errors.append(f"citation: anchor '{named.group(1)}' absent from '{m.group()}'")
        if selected:
            valid += 1
    # Resolve identifiers against actual blobs, never prompt text or the allowlist itself.
    content = "\n".join(blob(path) for path in sorted(paths) if identifier_source(path))
    removed = ""
    if base:
        proc = subprocess.run(["git", "-C", repo, "diff", "--no-ext-diff", "--unified=0", base, commit, "--", ".", *[":(exclude)" + p for p in IDENTIFIER_EXCLUSIONS]], capture_output=True, text=True, check=True)
        removed = "\n".join(l[1:] for l in proc.stdout.splitlines() if l.startswith("-") and not l.startswith("---"))
    absent = set(re.findall(r"^ABSENT: (" + IDENTIFIER + r")$", text, re.M))
    external = set()
    if "scripts/review/external-names.tsv" in paths and "lean-toolchain" in paths:
        for row in blob("scripts/review/external-names.tsv").splitlines():
            fields = row.split("\t")
            if len(fields) == 3 and fields[0] == blob("lean-toolchain").strip() and fields[2]:
                external.add(fields[1])
    for m in re.finditer(r"`([^`\n]+)`", text):
        name = m.group(1)
        if filename(name):
            continue
        if name.startswith("proposed:"):
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
                                 matched_verdict.lower() in ("clean", "accept"), base)
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
        if rp not in valid_blobs:
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
    )
    print(f"status: {status}")
    print(f"reason: {reason}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
