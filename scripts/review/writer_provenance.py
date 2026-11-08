#!/usr/bin/env python3
"""Writer-provenance reader and writer-log git-object manager (scripts/review/writer_provenance.py).

Writer identity comes from the writer harness, never from a caller flag or a
commit trailer alone.

1. Harness stamp on each writer commit:
   - Author: `<Model> (writer) <model@writers.concrete>`
     where `<model>` is one of `codex`, `kimi`, `agy`, `claude` and `<Model>`
     is `Codex`, `Kimi`, `agy` (or `Agy`), `Claude`.
   - Committer: the project owner's / repository committer's identity (never
     `*@writers.concrete`).
   - Commit trailer: `Writer: <model>`
   - Additional commit trailer when `<model> == agy`:
     `Agy-Diff-SHA256: <64-hex>` (the sha256 of the diff transported from agy's
     throwaway archive copy).

2. Writer-log entry (stored per commit under `refs/notes/concrete-writer` and
   exportable as a self-contained multi-stanza git blob for review-evidence
   records):
     model: <codex|kimi|agy|claude>
     round: <positive integer>
     branch: <branch-name>
     brief-sha256: <64-hex>
     start-sha: <40-hex>
     end-sha: <40-hex>
     commit: <40-hex>
     agy-diff-sha256: <64-hex>    # present iff model == agy

3. Provenance derivation & cross-check over `base..head`:
   - Any commit carrying ANY writer indicator (a `(writer)` / `@writers.concrete`
     author, a `Writer:` or `Agy-Diff-SHA256:` trailer, or a writer-log entry)
     must carry ALL required parts and they must agree; any partial or mismatched
     stamp is refused (a `Writer:` trailer alone is never enough).
   - Unstamped commits (human or pre-harness commits with no writer author,
     no writer trailer, and no writer-log entry) have an explicit, documented
     treatment: every unstamped non-merge commit (or `head` when `base..head`
     has no non-merge commits) is attributed to `claude` (the interactive /
     pre-harness writer identity) — never silently treated as written by nobody.
   - A branch may have several writers across rounds (e.g. `codex,agy` or
     `claude,codex`). The reviewer must be none of them.
"""

import argparse
import os
import re
import subprocess
import sys

VALID_MODELS = ("codex", "kimi", "agy", "claude")
MODEL_DISPLAY = {
    "codex": "Codex",
    "kimi": "Kimi",
    "agy": "agy",
    "claude": "Claude",
}
WRITER_NOTES_REF = os.environ.get("REVIEW_WRITER_NOTES_REF", "refs/notes/concrete-writer")
HEX40_RE = re.compile(r"^[0-9a-f]{40}$")
HEX64_RE = re.compile(r"^[0-9a-f]{64}$")
POS_INT_RE = re.compile(r"^[1-9][0-9]*$")
BRANCH_RE = re.compile(r"^[^\s~^:?*\[\]\\]+$")


class ProvenanceError(Exception):
    pass


def git_run(repo: str, args: list[str], check: bool = True, input_text: str | None = None) -> subprocess.CompletedProcess:
    proc = subprocess.run(
        ["git", "-C", repo] + args,
        input=input_text,
        capture_output=True,
        text=True,
        check=False,
    )
    if check and proc.returncode != 0:
        raise ProvenanceError(f"git {' '.join(args)} failed: {proc.stderr.strip()}")
    return proc


def resolve_commit(repo: str, rev: str, label: str = "commit") -> str:
    proc = git_run(repo, ["rev-parse", "-q", "--verify", f"{rev}^{{commit}}"], check=False)
    if proc.returncode != 0:
        raise ProvenanceError(f"{label} '{rev}' is not a valid commit")
    return proc.stdout.strip()


def is_ancestor(repo: str, anc: str, desc: str) -> bool:
    proc = git_run(repo, ["merge-base", "--is-ancestor", anc, desc], check=False)
    return proc.returncode == 0


def parse_writer_csv(csv_val: str, label: str = "writer") -> list[str]:
    if not csv_val:
        raise ProvenanceError(f"empty {label}")
    parts = csv_val.split(",")
    seen: list[str] = []
    for p in parts:
        if p not in VALID_MODELS:
            raise ProvenanceError(f"invalid {label} model '{p}' (expected codex, kimi, agy or claude)")
        if p in seen:
            raise ProvenanceError(f"duplicate model '{p}' in {label} '{csv_val}'")
        seen.append(p)
    return seen


def parse_log_stanza(stanza: str, source_desc: str = "writer-log") -> dict[str, str]:
    lines = [ln for ln in stanza.strip("\n").splitlines()]
    if not lines:
        raise ProvenanceError(f"{source_desc}: empty writer-log entry")
    entry: dict[str, str] = {}
    keys_in_order: list[str] = []
    for line in lines:
        m = re.match(r"^([a-z0-9-]+): (.*)$", line)
        if not m:
            raise ProvenanceError(f"{source_desc}: malformed line '{line}'")
        k, v = m.group(1), m.group(2)
        if k in entry:
            raise ProvenanceError(f"{source_desc}: duplicate key '{k}'")
        entry[k] = v
        keys_in_order.append(k)

    model = entry.get("model", "")
    if model not in VALID_MODELS:
        raise ProvenanceError(f"{source_desc}: invalid model '{model}'")

    expected_keys = ["model", "round", "branch", "brief-sha256", "start-sha", "end-sha", "commit"]
    if model == "agy":
        expected_keys.append("agy-diff-sha256")

    if keys_in_order != expected_keys:
        raise ProvenanceError(
            f"{source_desc}: keys {keys_in_order} do not match expected {expected_keys}"
        )

    if not POS_INT_RE.match(entry["round"]):
        raise ProvenanceError(f"{source_desc}: invalid round '{entry['round']}'")
    if not entry["branch"] or not BRANCH_RE.match(entry["branch"]) or ".." in entry["branch"]:
        raise ProvenanceError(f"{source_desc}: invalid branch '{entry['branch']}'")
    if not HEX64_RE.match(entry["brief-sha256"]):
        raise ProvenanceError(f"{source_desc}: invalid brief-sha256 '{entry['brief-sha256']}'")
    if not HEX40_RE.match(entry["start-sha"]):
        raise ProvenanceError(f"{source_desc}: invalid start-sha '{entry['start-sha']}'")
    if not HEX40_RE.match(entry["end-sha"]):
        raise ProvenanceError(f"{source_desc}: invalid end-sha '{entry['end-sha']}'")
    if not HEX40_RE.match(entry["commit"]):
        raise ProvenanceError(f"{source_desc}: invalid commit '{entry['commit']}'")
    if model == "agy" and not HEX64_RE.match(entry["agy-diff-sha256"]):
        raise ProvenanceError(f"{source_desc}: invalid agy-diff-sha256 '{entry['agy-diff-sha256']}'")

    return entry


def format_log_stanza(entry: dict[str, str]) -> str:
    keys = ["model", "round", "branch", "brief-sha256", "start-sha", "end-sha", "commit"]
    if entry.get("model") == "agy":
        keys.append("agy-diff-sha256")
    return "\n".join(f"{k}: {entry[k]}" for k in keys)


def parse_multi_stanza_log(text: str, source_desc: str = "writer-log") -> dict[str, dict[str, str]]:
    text_stripped = text.strip()
    if not text_stripped:
        return {}
    raw_stanzas = re.split(r"\n\n+", text_stripped)
    by_commit: dict[str, dict[str, str]] = {}
    for idx, raw in enumerate(raw_stanzas, 1):
        entry = parse_log_stanza(raw, f"{source_desc} stanza {idx}")
        c = entry["commit"]
        if c in by_commit:
            raise ProvenanceError(f"{source_desc}: duplicate writer-log entry for commit {c}")
        by_commit[c] = entry
    return by_commit


def load_log_object(repo: str, log_object: str) -> dict[str, dict[str, str]]:
    if os.path.isfile(log_object):
        with open(log_object, "r", encoding="utf-8") as f:
            content = f.read()
        return parse_multi_stanza_log(content, log_object)
    proc = git_run(repo, ["cat-file", "blob", log_object], check=False)
    if proc.returncode != 0:
        raise ProvenanceError(f"cannot read writer-log object '{log_object}'")
    return parse_multi_stanza_log(proc.stdout, f"blob {log_object}")


def get_commit_note_entry(repo: str, sha: str) -> dict[str, str] | None:
    proc = git_run(repo, ["notes", f"--ref={WRITER_NOTES_REF}", "show", sha], check=False)
    if proc.returncode != 0:
        return None
    stanzas = parse_multi_stanza_log(proc.stdout, f"{WRITER_NOTES_REF} on {sha[:10]}")
    if sha in stanzas:
        return stanzas[sha]
    if len(stanzas) == 1:
        only = next(iter(stanzas.values()))
        raise ProvenanceError(
            f"{WRITER_NOTES_REF} on {sha[:10]} records commit {only['commit']}, not {sha}"
        )
    raise ProvenanceError(f"{WRITER_NOTES_REF} on {sha[:10]} has no entry for {sha}")


def inspect_commit(
    repo: str,
    sha: str,
    head_sha: str,
    external_log: dict[str, dict[str, str]] | None,
) -> tuple[str | None, dict[str, str] | None, bool]:
    """Inspect a single commit's writer provenance.

    Returns `(writer_model, log_entry, is_merge)`.
    For a stamped commit, `writer_model` is the validated model and `log_entry` is its dict.
    For an unstamped commit, `writer_model` is None and `log_entry` is None.
    Raises `ProvenanceError` on any partial, malformed, or mismatched stamp.
    """
    proc = git_run(
        repo,
        ["show", "-s", "--format=%P%n%an%n%ae%n%cn%n%ce%n%B", sha],
        check=True,
    )
    lines = proc.stdout.splitlines()
    parents = lines[0].split() if lines else []
    is_merge = len(parents) > 1
    an = lines[1] if len(lines) > 1 else ""
    ae = lines[2] if len(lines) > 2 else ""
    cn = lines[3] if len(lines) > 3 else ""
    ce = lines[4] if len(lines) > 4 else ""
    body_lines = lines[5:] if len(lines) > 5 else []

    author_has_writer_tag = "(writer)" in an.lower() or ae.lower().endswith("@writers.concrete")

    writer_trailers: list[str] = []
    agy_trailers: list[str] = []
    for bl in body_lines:
        mw = re.match(r"^Writer:[ \t]*(.*)$", bl)
        if mw:
            writer_trailers.append(mw.group(1).strip())
        ma = re.match(r"^Agy-Diff-SHA256:[ \t]*(.*)$", bl)
        if ma:
            agy_trailers.append(ma.group(1).strip())

    if external_log is not None:
        log_entry = external_log.get(sha)
    else:
        log_entry = get_commit_note_entry(repo, sha)

    has_any_stamp = (
        author_has_writer_tag
        or bool(writer_trailers)
        or bool(agy_trailers)
        or (log_entry is not None)
    )

    if not has_any_stamp:
        return None, None, is_merge

    # Every stamped commit must have all required components and they must match.
    if len(writer_trailers) == 0:
        raise ProvenanceError(
            f"commit {sha[:10]} has writer author or log entry but no 'Writer: <model>' trailer"
        )
    if len(writer_trailers) > 1:
        raise ProvenanceError(f"commit {sha[:10]} has multiple 'Writer:' trailers")

    model = writer_trailers[0]
    if model not in VALID_MODELS:
        raise ProvenanceError(
            f"commit {sha[:10]} has invalid 'Writer: {model}' trailer (expected codex, kimi, agy or claude)"
        )

    expected_ae = f"{model}@writers.concrete"
    expected_an_choices = {f"{MODEL_DISPLAY[model]} (writer)", f"{model.capitalize()} (writer)", f"{model} (writer)"}
    if ae != expected_ae or an not in expected_an_choices:
        raise ProvenanceError(
            f"commit {sha[:10]} trailer 'Writer: {model}' does not match author '{an} <{ae}>' "
            f"(expected '{MODEL_DISPLAY[model]} (writer) <{expected_ae}>')"
        )

    if not cn or not ce or ce.lower().endswith("@writers.concrete") or "(writer)" in cn.lower():
        raise ProvenanceError(
            f"commit {sha[:10]} committer '{cn} <{ce}>' must be the owner's identity, not a writer identity"
        )

    if model == "agy":
        if len(agy_trailers) != 1:
            raise ProvenanceError(
                f"commit {sha[:10]} written by agy must have exactly one 'Agy-Diff-SHA256: <64-hex>' trailer"
            )
        if not HEX64_RE.match(agy_trailers[0]):
            raise ProvenanceError(
                f"commit {sha[:10]} has malformed 'Agy-Diff-SHA256: {agy_trailers[0]}' trailer"
            )
    else:
        if agy_trailers:
            raise ProvenanceError(
                f"commit {sha[:10]} written by {model} must not have an 'Agy-Diff-SHA256:' trailer"
            )

    if log_entry is None:
        raise ProvenanceError(
            f"commit {sha[:10]} has 'Writer: {model}' stamp but no matching writer-log entry "
            f"(a Writer: trailer alone is never enough)"
        )

    if log_entry["commit"] != sha:
        raise ProvenanceError(
            f"commit {sha[:10]} writer-log entry is for commit {log_entry['commit']}, not {sha}"
        )
    if log_entry["model"] != model:
        raise ProvenanceError(
            f"commit {sha[:10]} writer-log model '{log_entry['model']}' does not match commit stamp '{model}'"
        )
    if model == "agy" and log_entry.get("agy-diff-sha256") != agy_trailers[0]:
        raise ProvenanceError(
            f"commit {sha[:10]} writer-log agy-diff-sha256 '{log_entry.get('agy-diff-sha256')}' "
            f"does not match commit trailer '{agy_trailers[0]}'"
        )

    start_sha = resolve_commit(repo, log_entry["start-sha"], f"commit {sha[:10]} writer-log start-sha")
    end_sha = resolve_commit(repo, log_entry["end-sha"], f"commit {sha[:10]} writer-log end-sha")
    if start_sha == sha or not is_ancestor(repo, start_sha, sha):
        raise ProvenanceError(
            f"commit {sha[:10]} writer-log start-sha {start_sha[:10]} is not a strict ancestor of {sha[:10]}"
        )
    if not is_ancestor(repo, sha, end_sha):
        raise ProvenanceError(
            f"commit {sha[:10]} is not an ancestor-or-equal of its writer-log end-sha {end_sha[:10]}"
        )
    if not is_ancestor(repo, end_sha, head_sha):
        raise ProvenanceError(
            f"commit {sha[:10]} writer-log end-sha {end_sha[:10]} is not an ancestor-or-equal of head {head_sha[:10]}"
        )

    return model, log_entry, is_merge


def derive_range_provenance(
    repo: str,
    base_rev: str,
    head_rev: str,
    external_log: dict[str, dict[str, str]] | None = None,
) -> tuple[list[str], list[dict[str, str]]]:
    """Derive the ordered unique writer list and log entries for `base..head`.

    Unstamped non-merge commits (human or pre-harness commits) are explicitly
    attributed to `claude`. If `base == head` or `base..head` contains only
    unstamped merge commits, `head` is attributed to `claude` so no range is
    ever treated as written by nobody.
    """
    base_sha = resolve_commit(repo, base_rev, "base")
    head_sha = resolve_commit(repo, head_rev, "head")
    if not is_ancestor(repo, base_sha, head_sha):
        raise ProvenanceError(f"base {base_sha} is not an ancestor of head {head_sha}")

    if base_sha == head_sha:
        commits = [head_sha]
    else:
        proc = git_run(repo, ["rev-list", "--reverse", "--topo-order", f"{base_sha}..{head_sha}"])
        commits = [c.strip() for c in proc.stdout.splitlines() if c.strip()]

    # If external_log is not given, check whether head_sha itself carries a multi-stanza
    # log covering base..head under WRITER_NOTES_REF; if so, use it as fallback for commits
    # in base..head that do not carry an individual note.
    head_multi_log: dict[str, dict[str, str]] = {}
    if external_log is None:
        proc = git_run(repo, ["notes", f"--ref={WRITER_NOTES_REF}", "show", head_sha], check=False)
        if proc.returncode == 0 and proc.stdout.strip():
            head_multi_log = parse_multi_stanza_log(
                proc.stdout, f"{WRITER_NOTES_REF} on {head_sha[:10]}"
            )

    writers: list[str] = []
    log_entries: list[dict[str, str]] = []
    unstamped_non_merge = False

    def add_writer(w: str) -> None:
        if w not in writers:
            writers.append(w)

    for c in commits:
        eff_log = external_log if external_log is not None else (head_multi_log if c in head_multi_log else None)
        model, entry, is_merge = inspect_commit(repo, c, head_sha, eff_log)
        if model is not None and entry is not None:
            add_writer(model)
            log_entries.append(entry)
        elif not is_merge:
            # Explicit treatment for human / pre-harness unstamped commits:
            # attributed to `claude`, never silently ignored.
            unstamped_non_merge = True
            add_writer("claude")

    if not writers:
        # All commits in base..head were unstamped merge commits (or base == head was a merge).
        add_writer("claude")

    if external_log is not None:
        log_commits = set(external_log.keys())
        stamped_commits = {e["commit"] for e in log_entries}
        extra = log_commits - stamped_commits
        if extra:
            extra_str = ", ".join(sorted(c[:10] for c in extra))
            raise ProvenanceError(
                f"writer-log contains entries for commits not stamped in {base_sha[:10]}..{head_sha[:10]}: {extra_str}"
            )

    return writers, log_entries


def cmd_derive(args: argparse.Namespace) -> int:
    try:
        ext_log = load_log_object(args.repo, args.log_object) if args.log_object else None
        writers, _ = derive_range_provenance(args.repo, args.base, args.head, ext_log)
        if args.assert_writer:
            claimed = parse_writer_csv(args.assert_writer, "--writer")
            if set(claimed) != set(writers):
                raise ProvenanceError(
                    f"--writer '{args.assert_writer}' does not match derived writer provenance "
                    f"'{','.join(writers)}' for {args.base[:10]}..{args.head[:10]}"
                )
        print(",".join(writers))
        return 0
    except ProvenanceError as exc:
        print(f"writer-provenance: {exc}", file=sys.stderr)
        return 2


def cmd_verify(args: argparse.Namespace) -> int:
    try:
        if args.reviewer not in VALID_MODELS:
            raise ProvenanceError(f"invalid reviewer '{args.reviewer}'")
        claimed = parse_writer_csv(args.claimed_writer, "note writer")
        if args.reviewer in claimed:
            raise ProvenanceError(
                f"reviewer {args.reviewer} reviewed its own work (writer {args.claimed_writer})"
            )
        ext_log = load_log_object(args.repo, args.log_object) if args.log_object else None
        writers, _ = derive_range_provenance(args.repo, args.base, args.head, ext_log)
        if args.reviewer in writers:
            raise ProvenanceError(
                f"reviewer {args.reviewer} reviewed its own work (derived writer {','.join(writers)})"
            )
        if set(claimed) != set(writers):
            raise ProvenanceError(
                f"note writer '{args.claimed_writer}' does not match derived writer provenance "
                f"'{','.join(writers)}' for {args.base[:10]}..{args.head[:10]}"
            )
        return 0
    except ProvenanceError as exc:
        print(f"{exc}")
        return 1


def cmd_export_log(args: argparse.Namespace) -> int:
    try:
        ext_log = load_log_object(args.repo, args.log_object) if args.log_object else None
        _, entries = derive_range_provenance(args.repo, args.base, args.head, ext_log)
        content = "\n\n".join(format_log_stanza(e) for e in entries)
        if content:
            content += "\n"
        if args.write_blob:
            proc = git_run(
                args.repo,
                ["hash-object", "-w", "--no-filters", "--stdin"],
                input_text=content,
            )
            print(proc.stdout.strip())
        else:
            sys.stdout.write(content)
        return 0
    except ProvenanceError as exc:
        print(f"writer-provenance: {exc}", file=sys.stderr)
        return 1


def cmd_stamp_commit(args: argparse.Namespace) -> int:
    """Stamp HEAD (or the given commit at HEAD) with writer author, trailer(s), and writer-log note."""
    try:
        if args.model not in VALID_MODELS:
            raise ProvenanceError(f"invalid model '{args.model}'")
        if not POS_INT_RE.match(str(args.round)):
            raise ProvenanceError(f"invalid round '{args.round}'")
        if not HEX64_RE.match(args.brief_sha256):
            raise ProvenanceError(f"invalid brief-sha256 '{args.brief_sha256}'")
        if args.model == "agy":
            if not args.agy_diff_sha256 or not HEX64_RE.match(args.agy_diff_sha256):
                raise ProvenanceError("agy commits require --agy-diff-sha256 <64-hex>")
        elif args.agy_diff_sha256:
            raise ProvenanceError(f"{args.model} commits must not set --agy-diff-sha256")

        head_sha = resolve_commit(args.repo, "HEAD", "HEAD")
        start_sha = (
            resolve_commit(args.repo, args.start_sha, "start-sha")
            if args.start_sha
            else resolve_commit(args.repo, "HEAD~1", "HEAD~1")
        )
        proc = git_run(args.repo, ["show", "-s", "--format=%B", head_sha])
        msg = proc.stdout.rstrip("\n")
        # Remove any existing Writer: / Agy-Diff-SHA256: lines before appending fresh trailers
        cleaned_lines = [
            ln
            for ln in msg.splitlines()
            if not re.match(r"^(Writer|Agy-Diff-SHA256):[ \t]*", ln)
        ]
        cleaned_lines.append("")
        cleaned_lines.append(f"Writer: {args.model}")
        if args.model == "agy":
            cleaned_lines.append(f"Agy-Diff-SHA256: {args.agy_diff_sha256}")
        new_msg = "\n".join(cleaned_lines) + "\n"

        env = os.environ.copy()
        env["GIT_AUTHOR_NAME"] = f"{MODEL_DISPLAY[args.model]} (writer)"
        env["GIT_AUTHOR_EMAIL"] = f"{args.model}@writers.concrete"

        amend_proc = subprocess.run(
            ["git", "-C", args.repo, "commit", "-q", "--amend", "--reset-author", "-F", "-"],
            input=new_msg,
            capture_output=True,
            text=True,
            env=env,
            check=False,
        )
        if amend_proc.returncode != 0:
            raise ProvenanceError(f"git commit --amend failed: {amend_proc.stderr.strip()}")

        new_sha = resolve_commit(args.repo, "HEAD", "HEAD")
        end_sha = (
            resolve_commit(args.repo, args.end_sha, "end-sha")
            if args.end_sha
            else new_sha
        )
        entry = {
            "model": args.model,
            "round": str(args.round),
            "branch": args.branch,
            "brief-sha256": args.brief_sha256,
            "start-sha": start_sha,
            "end-sha": end_sha,
            "commit": new_sha,
        }
        if args.model == "agy":
            entry["agy-diff-sha256"] = args.agy_diff_sha256
        stanza = format_log_stanza(entry)
        git_run(
            args.repo,
            ["notes", f"--ref={WRITER_NOTES_REF}", "add", "-f", "-m", stanza, new_sha],
        )
        print(new_sha)
        return 0
    except ProvenanceError as exc:
        print(f"writer-provenance: {exc}", file=sys.stderr)
        return 2


def main() -> int:
    parser = argparse.ArgumentParser(description="Concrete writer-provenance reader and log manager.")
    sub = parser.add_subparsers(dest="cmd", required=True)

    p_derive = sub.add_parser("derive", help="Derive writer provenance for base..head")
    p_derive.add_argument("--repo", default=".", help="Git repository path")
    p_derive.add_argument("--base", required=True, help="Base commit")
    p_derive.add_argument("--head", required=True, help="Head commit")
    p_derive.add_argument("--assert-writer", help="Optional caller-supplied writer assertion to cross-check")
    p_derive.add_argument("--log-object", help="Optional multi-stanza writer-log blob or file")

    p_verify = sub.add_parser("verify", help="Verify note writer and reviewer independence against base..head")
    p_verify.add_argument("--repo", default=".", help="Git repository path")
    p_verify.add_argument("--base", required=True, help="Base commit")
    p_verify.add_argument("--head", required=True, help="Head commit")
    p_verify.add_argument("--claimed-writer", required=True, help="Writer field from review note")
    p_verify.add_argument("--reviewer", required=True, help="Reviewer field from review note")
    p_verify.add_argument("--log-object", help="Optional multi-stanza writer-log blob or file")

    p_export = sub.add_parser("export-log", help="Export canonical multi-stanza writer log for base..head")
    p_export.add_argument("--repo", default=".", help="Git repository path")
    p_export.add_argument("--base", required=True, help="Base commit")
    p_export.add_argument("--head", required=True, help="Head commit")
    p_export.add_argument("--log-object", help="Optional multi-stanza writer-log blob or file")
    p_export.add_argument("--write-blob", action="store_true", help="Write blob to git object store and print SHA")

    p_stamp = sub.add_parser("stamp-commit", help="Stamp HEAD with writer author, trailer(s), and writer-log note")
    p_stamp.add_argument("--repo", default=".", help="Git repository path")
    p_stamp.add_argument("--model", required=True, help="Writer model (codex|kimi|agy|claude)")
    p_stamp.add_argument("--round", default="1", help="Writer round number (default: 1)")
    p_stamp.add_argument("--branch", default="wip/branch", help="Branch name")
    p_stamp.add_argument("--brief-sha256", default="0" * 64, help="SHA-256 of the writer brief")
    p_stamp.add_argument("--start-sha", help="Start SHA of the writer round (default: HEAD~1)")
    p_stamp.add_argument("--end-sha", help="End SHA of the writer round (default: amended HEAD)")
    p_stamp.add_argument("--agy-diff-sha256", help="SHA-256 of the transported diff (required when model=agy)")

    args = parser.parse_args()
    if args.cmd == "derive":
        return cmd_derive(args)
    if args.cmd == "verify":
        return cmd_verify(args)
    if args.cmd == "export-log":
        return cmd_export_log(args)
    if args.cmd == "stamp-commit":
        return cmd_stamp_commit(args)
    return 2


if __name__ == "__main__":
    sys.exit(main())
