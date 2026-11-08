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

CLI output (stdout, two lines):
  status: <verdict|blocked|refused|noverdict|error>
  reason: <human-readable explanation>
"""

import argparse
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


def evaluate_output(
    text: str,
    log_text: str,
    commit: str,
    repo: str,
    verdicts: list[str],
    rc: int = 0,
    incomplete: bool = False,
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
        if candidate in verdicts:
            matched_verdict = candidate

    if matched_verdict is None:
        if REFUSAL_RE.search(combined):
            return "refused", "reviewer refused the prompt without a valid verdict line"
        return (
            "noverdict",
            f"last non-empty line is not a valid VERDICT ({'|'.join(verdicts)}) line",
        )

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

    try:
        valid_blobs = commit_blob_paths(repo, commit)
    except ValueError as exc:
        return "blocked", str(exc)

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
    )
    print(f"status: {status}")
    print(f"reason: {reason}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
