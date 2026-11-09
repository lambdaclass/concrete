#!/usr/bin/env python3
"""Writer-provenance reader and writer-log git-object manager (scripts/review/writer_provenance.py).

Writer identity comes from the writer harness or an explicit audited backfill in
the writer log, never from a caller flag, a commit trailer alone, or a default
attribution of unstamped commits.

1. Harness stamp on each harness-written commit (non-merge commits and resolved
   merge commits):
   - Author: `<Model> (writer) <model@writers.concrete>`
     where `<model>` is one of `codex`, `kimi`, `agy`, `claude` and `<Model>`
     is `Codex`, `Kimi`, `agy` (or `Agy`), `Claude`.
   - Committer: the project owner's / repository committer's identity (never
     `*@writers.concrete` or `*(writer)*`).
   - Commit trailer: `Writer: <model>`
   - Additional commit trailer when `<model> == agy`:
     `Agy-Diff-SHA256: <64-hex>` (the sha256 of the diff transported from agy's
     throwaway archive copy; the harness stamps resolved merges the same way).

2. Authenticated, append-only writer-log entries (stored per commit under
   `refs/notes/concrete-writer` and exportable as a self-contained multi-stanza
   git blob for review-evidence records):
   - Every entry (`source: harness` and `source: backfill`, including append-only
     correction entries) carries:
       entry-id: <64-hex sha256 of the canonical unsigned fields>
       signer: <authorized principal>
       -----BEGIN SSH SIGNATURE-----
       <OpenSSH signature over all key-value lines, namespace "git">
       -----END SSH SIGNATURE-----
     verified by `ssh-keygen -Y find-principals` and `ssh-keygen -Y verify -n git`
     against the tracked allowed-signers file
     (`scripts/review/allowed_writer_log_signers`, which lists the owner-approved
     `orchestrator@concrete` Ed25519 public key). If that file has no active key
     lines, every entry is unverified and provenance over any range that needs
     one fails closed.
   - Harness entry (written by `stamp-commit` at the end of a writer round):
       model: <codex|kimi|agy|claude>
       round: <positive integer>
       branch: <branch-name>
       brief-sha256: <64-hex>
       start-sha: <40-hex>
       end-sha: <40-hex>
       commit: <40-hex>
       agy-diff-sha256: <64-hex>    # present iff model == agy
       source: harness
       entry-id: <64-hex>
       signer: <principal>
       -----BEGIN SSH SIGNATURE-----
       ...
       -----END SSH SIGNATURE-----
   - Backfilled entry (written by `backfill` for a historical unstamped commit,
     computing `brief-sha256` and `output-sha256` from `--brief-file` and
     `--output-file`):
       model: <codex|kimi|agy|claude>
       round: <positive integer>
       branch: <branch-name>
       brief-sha256: <64-hex>
       output-sha256: <64-hex>
       start-sha: <40-hex>
       end-sha: <40-hex>
       commit: <40-hex>
       agy-diff-sha256: <64-hex>    # optional when model == agy
       source: backfill
       evidence: <non-empty single-line free text>
       supersedes: <64-hex>         # present iff superseding an earlier entry
       reason: <single-line text>   # present iff supersedes is present
       entry-id: <64-hex>
       signer: <principal>
       -----BEGIN SSH SIGNATURE-----
       ...
       -----END SSH SIGNATURE-----
   - Append-only log: an existing entry is never replaced or removed. A
     correction is a new signed entry appended to the commit's note that names
     the `entry-id` it supersedes (`supersedes: <64-hex>`) and states why
     (`reason: <why>`). The reader verifies every entry in the chain, reports
     the full history, and uses the latest valid entry. Any model attested as
     writer in an earlier superseded entry remains disqualified from reviewing
     that commit, and a backfill can never attribute a commit to a model that is
     also the note's reviewer unless an earlier signed entry already did so.

3. Provenance derivation & cross-check over `base..head`:
   - Any commit carrying ANY harness commit stamp (a `(writer)` /
     `@writers.concrete` author, or a `Writer:` or `Agy-Diff-SHA256:` trailer)
     must carry ALL required harness stamp parts plus a matching signed harness
     writer-log entry (`source` != `backfill`), and they must agree; any partial
     or mismatched stamp is refused (a `Writer:` trailer alone is never enough).
   - An unstamped commit (no writer author tag and no writer trailer) has an
     UNKNOWN writer unless the writer log holds a valid signed `source: backfill`
     entry for that commit. Review provenance over a range containing an
     unstamped authored commit without a backfilled entry is refused. `claude`
     is a writer only when explicitly logged.
   - Merge commits: a merge commit's resolution is authored work when its tree
     differs from git's conflict-free automatic merge (`git merge-tree
     --write-tree` of its two parents). A merge with no resolution adds no
     writer. A merge with a resolution is authored work and must carry a valid
     signed harness stamp + log entry or a signed backfilled writer-log entry.
"""

import argparse
import atexit
import concurrent.futures
import hashlib
import functools
import os
import re
import shutil
import subprocess
import sys
import tempfile
import threading
from collections import namedtuple
from types import MappingProxyType

sys.dont_write_bytecode = True

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
DEFAULT_ALLOWED_SIGNERS = os.path.join(SCRIPT_DIR, "allowed_writer_log_signers")
VALID_MODELS = ("codex", "kimi", "agy", "claude")
MODEL_DISPLAY = {
    "codex": "Codex",
    "kimi": "Kimi",
    "agy": "agy",
    "claude": "Claude",
}
WRITER_NOTES_REF = os.environ.get("REVIEW_WRITER_NOTES_REF", "refs/notes/concrete-writer")
REVIEW_NOTES_REF = os.environ.get("REVIEW_NOTES_REF", "refs/notes/concrete-review")
HEX40_RE = re.compile(r"^[0-9a-f]{40}$")
HEX64_RE = re.compile(r"^[0-9a-f]{64}$")
HEX_PREFIX_RE = re.compile(r"^[0-9a-f]{10,64}$")
POS_INT_RE = re.compile(r"^[1-9][0-9]*$")
BRANCH_RE = re.compile(r"^[^\s~^:?*\[\]\\]+$")
SIGNER_RE = re.compile(r"^[^\s]+$")
ZERO40 = "0" * 40
SIG_BEGIN = "-----BEGIN SSH SIGNATURE-----"
SIG_END = "-----END SSH SIGNATURE-----"


class ProvenanceError(Exception):
    pass


class FetchTransportError(ProvenanceError):
    pass


GIT_VIEW_ENV = (
    "GIT_REPLACE_REF_BASE", "GIT_GRAFT_FILE", "GIT_SHALLOW_FILE",
    "GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_OBJECT_DIRECTORY",
    "GIT_ALTERNATE_OBJECT_DIRECTORIES", "GIT_INDEX_FILE", "GIT_NAMESPACE",
)


# These two controls are deliberately inherited: git_sandbox uses them to
# isolate global/system configuration for gate fixtures. All other GIT_CONFIG_*
# variables can inject configuration and must be removed, including future keys.
_SANDBOX_CONFIG_ENV = frozenset(("GIT_CONFIG_GLOBAL", "GIT_CONFIG_NOSYSTEM"))


@functools.lru_cache(maxsize=1)
def _git_local_env_vars() -> frozenset[str]:
    # Cache only Git's immutable vocabulary, never repository state. Bootstrap
    # with every GIT_* variable removed except the sandbox isolation controls.
    proc = canonical_git_run(".", ["rev-parse", "--local-env-vars"],
                             verify=False, _discover_env=True)
    if proc.returncode or not proc.stdout.strip():
        raise ProvenanceError("cannot discover Git repository-local environment variables")
    return frozenset(proc.stdout.splitlines())


def canonical_git_env(env: dict[str, str] | None = None, *,
                      _discover: bool = False) -> dict[str, str]:
    source = os.environ if env is None else env
    local_vars = frozenset() if _discover else _git_local_env_vars()
    result = {key: value for key, value in source.items()
              if key in _SANDBOX_CONFIG_ENV or not (
                  key in local_vars or key in GIT_VIEW_ENV or key == "GIT_CONFIG"
                  or key.startswith("GIT_CONFIG_")
                  or (_discover and key.startswith("GIT_")))}
    result.update(GIT_NO_REPLACE_OBJECTS="1", GIT_OPTIONAL_LOCKS="0")
    return result


class _VerifiedRepo(str):
    """Operation-local capability; no repository state survives an invocation.

    Helpers preserve this immutable path object, so their Git calls share the
    entry verification. Plain-path library calls still verify independently.
    A new command always constructs a fresh capability, even given an old one.
    """

    def __new__(cls, repo: str):
        path = os.path.abspath(repo)
        verify_canonical_repo_view(path)
        return super().__new__(cls, path)


def canonical_git_run(repo: str, args: list[str], *, input=None,
                      text: bool = True, env=None, verify: bool = True,
                      _discover_env: bool = False, _interactive: bool = False, _timeout=None):
    # Only discovery and view probes use the unchecked bootstrap path.
    if verify and not isinstance(repo, _VerifiedRepo):
        verify_canonical_repo_view(repo)
    argv = ["git", "--no-replace-objects", "-c", "core.commitGraph=false", "-C", repo] + args
    if _interactive:
        return subprocess.Popen(argv, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE, text=text, start_new_session=True,
                                env=canonical_git_env(env, _discover=_discover_env))
    return subprocess.run(
        argv,
        input=input, capture_output=True, text=text,
        env=canonical_git_env(env, _discover=_discover_env), check=False, timeout=_timeout,
    )


def verify_canonical_repo_view(repo: str) -> None:
    def probe(args):
        proc = canonical_git_run(repo, args, verify=False)
        if proc.returncode:
            detail = (proc.stderr or proc.stdout).strip()
            raise ProvenanceError(f"git {' '.join(args)} failed: {detail}")
        return proc.stdout.strip()

    # Capture directory paths and shallow status in one Git process. Do not
    # cache them: every operation must observe newly planted view overrides.
    git_dir, common_dir, shallow = probe([
        "rev-parse", "--absolute-git-dir", "--path-format=absolute",
        "--git-common-dir", "--is-shallow-repository",
    ]).splitlines()
    dirs = [git_dir, common_dir]
    if (shallow == "true"
            or any(os.path.lexists(os.path.join(d, "shallow")) for d in dirs)):
        raise ProvenanceError(
            "repository is shallow (git rev-parse --is-shallow-repository is true): "
            "writer-notes continuity and ancestry require full commit history")
    for directory in dict.fromkeys(dirs):
        grafts = os.path.join(directory, "info", "grafts")
        if os.path.lexists(grafts):
            raise ProvenanceError(
                f"repository has grafted history ({grafts}): "
                "writer-notes continuity and ancestry forbid git grafts")
    replacements = probe(["for-each-ref", "--format=%(refname)", "refs/replace/"])
    if replacements:
        raise ProvenanceError(
            f"repository has git replacement refs ({replacements.splitlines()[0]}): "
            "writer-notes continuity and ancestry forbid replacement objects")


def git_run(repo: str, args: list[str], check: bool = True, input_text: str | None = None) -> subprocess.CompletedProcess:
    proc = canonical_git_run(repo, args, input=input_text)
    if check and proc.returncode != 0:
        detail = (proc.stderr or proc.stdout or f"exit {proc.returncode}").strip()
        raise ProvenanceError(f"git {' '.join(args)} failed: {detail}")
    return proc


class CheckpointSnapshot(namedtuple("_CheckpointSnapshot", "repo refs")):
    """Complete, immutable ref map accepted at one transaction boundary.

    Construct through capture_checkpoint_snapshot; None explicitly means ABSENT.
    The fresh capability belongs to this boundary, never to a path-keyed cache.
    """

    __slots__ = ()

    def __new__(cls, repo: _VerifiedRepo, refs: dict[str, str | None]):
        return super().__new__(cls, repo, MappingProxyType(dict(refs)))


class _CheckpointRace(ProvenanceError):
    pass


class _CheckpointTimeout(ProvenanceError):
    pass


def _checkpoint_git(repo, args, *, check=True, input_text=None):
    try:
        proc = canonical_git_run(repo, args, _timeout=10, input=input_text)
    except subprocess.TimeoutExpired as exc:
        raise _CheckpointTimeout("checkpoint transaction timeout (INCOMPLETE)") from exc
    if check and proc.returncode:
        raise ProvenanceError(f"checkpoint Git command failed: {proc.stderr.strip()}")
    return proc


_CHECKPOINT_REF_SYNTAX = {}


def _checkpoint_ref_syntax(repo, ref):
    # Only syntax is cached: check-ref-format is repository-independent and
    # immutable ref strings carry no repository state or authority.
    if ref not in _CHECKPOINT_REF_SYNTAX:
        valid = _checkpoint_git(repo, ["check-ref-format", ref], check=False).returncode == 0
        if len(_CHECKPOINT_REF_SYNTAX) >= 256:
            _CHECKPOINT_REF_SYNTAX.pop(next(iter(_CHECKPOINT_REF_SYNTAX)))
        _CHECKPOINT_REF_SYNTAX[ref] = valid
    return _CHECKPOINT_REF_SYNTAX[ref]


CHECKPOINT_PROTOCOL_DEADLINE = 10


class _CheckpointProtocol:
    """Nonblocking input and simultaneous output draining, with bounded teardown."""

    def __init__(self, proc, *, deadline_seconds=None):
        import selectors
        self.deadline_seconds = (CHECKPOINT_PROTOCOL_DEADLINE if deadline_seconds is None else deadline_seconds)
        self.proc = proc
        self.selector = selectors.DefaultSelector()
        self.output = bytearray()
        self.errors = bytearray()
        for stream in (proc.stdin, proc.stdout, proc.stderr):
            os.set_blocking(stream.fileno(), False)
        self.selector.register(proc.stdout, selectors.EVENT_READ)
        self.selector.register(proc.stderr, selectors.EVENT_READ)

    def exchange(self, command, expected):
        import selectors
        import time
        data = memoryview((command + "\n").encode())
        deadline = time.monotonic() + self.deadline_seconds
        self.selector.register(self.proc.stdin, selectors.EVENT_WRITE)
        try:
            while True:
                remaining = deadline - time.monotonic()
                if remaining <= 0:
                    raise _CheckpointTimeout("checkpoint transaction timeout (INCOMPLETE)")
                events = self.selector.select(remaining)
                if not events:
                    raise _CheckpointTimeout("checkpoint transaction timeout (INCOMPLETE)")
                for key, _ in events:
                    if key.fileobj is self.proc.stdin:
                        sent = os.write(key.fd, data[:4096])
                        data = data[sent:]
                        if not data:
                            self.selector.unregister(self.proc.stdin)
                    else:
                        chunk = os.read(key.fd, 4096)
                        if not chunk:
                            self.selector.unregister(key.fileobj)
                            if key.fileobj is self.proc.stdout:
                                raise ProvenanceError("checkpoint transaction protocol failed")
                        elif key.fileobj is self.proc.stderr:
                            self.errors.extend(chunk)
                            self.errors = self.errors[-8192:]
                        elif key.fileobj is self.proc.stdout:
                            self.output.extend(chunk)
                            if len(self.output) > 4096:
                                raise ProvenanceError("checkpoint transaction protocol failed")
                if not data and b"\n" in self.output:
                    reply, _, rest = self.output.partition(b"\n")
                    self.output = bytearray(rest)
                    if reply != expected.encode():
                        raise ProvenanceError("checkpoint transaction protocol failed")
                    return
        except OSError as exc:
            raise ProvenanceError("checkpoint transaction protocol failed") from exc
        finally:
            try:
                self.selector.unregister(self.proc.stdin)
            except KeyError:
                pass

    def close(self):
        # Kill before closing stdin: no buffered writes and no pipe-dependent
        # wait. The child cannot retain locks or fill a pipe during teardown.
        import signal
        def signal_group(sig):
            try:
                os.killpg(self.proc.pid, sig)
            except ProcessLookupError:
                pass
        try:
            signal_group(signal.SIGTERM)
            try:
                self.proc.wait(timeout=2)
            except subprocess.TimeoutExpired:
                signal_group(signal.SIGKILL)
                self.proc.wait(timeout=2)
        finally:
            # A hook may outlive the Git parent; reap the parent and terminate
            # the entire operation-owned group, never an unrelated process.
            signal_group(signal.SIGKILL)
            self.selector.close()
            for stream in (self.proc.stdin, self.proc.stdout, self.proc.stderr):
                try:
                    stream.close()
                except OSError:
                    pass


def _checkpoint_transaction(snapshot: CheckpointSnapshot,
                            updates: dict[str, str | None], op: str) -> None:
    repo = snapshot.repo
    backend = _checkpoint_git(repo, ["rev-parse", "--show-ref-format"]).stdout.strip()
    if backend not in ("files", "reftable"):
        raise ProvenanceError(f"unsupported checkpoint ref backend: {backend}")
    unknown = updates.keys() - snapshot.refs.keys()
    if unknown:
        raise ProvenanceError(f"checkpoint update outside snapshot: {sorted(unknown)[0]}")
    for ref in snapshot.refs:
        if not isinstance(ref, str) or not ref.startswith("refs/") or not _checkpoint_ref_syntax(repo, ref):
            raise ProvenanceError(f"invalid checkpoint ref name: {ref}")
        new = updates.get(ref)
        if new is not None and (not isinstance(new, str) or not HEX40_RE.fullmatch(new) or new == ZERO40):
            raise ProvenanceError(f"invalid checkpoint update SHA for {ref}: {new}")
    proc = canonical_git_run(repo, ["update-ref", "--no-deref", "--stdin"], _interactive=True)
    protocol = _CheckpointProtocol(proc)
    try:
        # Git validates queued update objects before prepare. A read-only
        # validation transaction is therefore necessary to reject missing new
        # objects under locks. Installation reacquires and checks the COMPLETE
        # snapshot, so the gap between transactions cannot authorize stale work.
        for installing in ((False, True) if updates else (False,)):
            commands = []
            for ref, old in snapshot.refs.items():
                # Prepared deletions lock existing, absent and dangling names
                # without requiring their targets to have any particular type.
                new = updates.get(ref, old) if installing else None
                commands.append(f"delete {ref}" if new is None else f"update {ref} {new}")
            protocol.exchange("start", "start: ok")
            try:
                protocol.exchange("\n".join([*commands, "prepare"]), "prepare: ok")
            except _CheckpointTimeout:
                raise
            except ProvenanceError as exc:
                # Prepare failures never justify inspecting an unlocked ref. Only
                # Git lock-contention failures are retryable; retain one race type.
                try:
                    detail = os.read(proc.stderr.fileno(), 8192)
                except (OSError, BlockingIOError):
                    detail = b""
                detail = bytes(protocol.errors) + detail
                if b"cannot lock ref" in detail or b"cannot lock references" in detail:
                    raise _CheckpointRace(f"checkpoint ref race during {op}") from exc
                raise
            rows = _checkpoint_git(repo, [
                "for-each-ref", "--format=%(refname) %(objectname) %(objecttype) %(symref)",
                "--", *snapshot.refs]).stdout
            # Git patterns also return descendants. Only exact participant names
            # are authoritative; DWIM and prefix lookalikes cannot supply values.
            exact = {fields[0]: fields for row in rows.splitlines()
                     if (fields := row.split()) and fields[0] in snapshot.refs}
            for ref in snapshot.refs:
                fields = exact.get(ref)
                if fields is not None:
                    if len(fields) > 3:
                        raise ProvenanceError(f"symbolic checkpoint ref is forbidden: {ref}")
                else:
                    # Enumeration omits dangling and looping symbolic refs. Only
                    # these missing names need a separate no-recurse inspection.
                    symbolic = _checkpoint_git(repo, ["symbolic-ref", "--no-recurse", "-q", ref], check=False)
                    if symbolic.returncode == 0:
                        raise ProvenanceError(f"symbolic checkpoint ref is forbidden: {ref}")
                    if symbolic.returncode != 1:
                        raise ProvenanceError(f"cannot inspect checkpoint ref: {ref}: {symbolic.stderr.strip()}")
            targets = [new for new in updates.values() if new is not None]
            if targets:
                obj = _checkpoint_git(repo, ["cat-file", "--batch-check=%(objectname) %(objecttype)"],
                                      input_text="\n".join(targets) + "\n", check=False)
                objects = dict(zip(targets, obj.stdout.splitlines()))
            for ref, expected in snapshot.refs.items():
                fields = exact.get(ref)
                value = fields[1] if fields else None
                if fields and (fields[2] != "commit" or not HEX40_RE.fullmatch(value)):
                    raise ProvenanceError(f"checkpoint ref does not name a 40-hex commit: {ref}")
                if value != expected:
                    raise _CheckpointRace(f"checkpoint ref race during {op}")
                new = updates.get(ref)
                if new is not None:
                    if obj.returncode or len(obj.stdout.splitlines()) != len(targets) or objects.get(new) != f"{new} commit":
                        raise ProvenanceError(f"checkpoint update does not name an existing commit: {ref}: {new}")
            protocol.exchange("commit" if installing else "abort", "commit: ok" if installing else "abort: ok")
        proc.stdin.close()
        proc.wait(timeout=10)
        if proc.returncode:
            raise ProvenanceError(f"checkpoint transaction failed during {op}")
    except subprocess.TimeoutExpired as exc:
        raise _CheckpointTimeout("checkpoint transaction timeout (INCOMPLETE)") from exc
    finally:
        protocol.close()


def cas_update_refs(snapshot: CheckpointSnapshot, updates: dict[str, str | None],
                    *, op: str = "checkpoint installation") -> None:
    """Install only against the entire captured map, even for an empty write set."""
    _checkpoint_transaction(snapshot, updates, op)


def capture_checkpoint_snapshot(repo: str, *, target_ref: str | None = None,
                                old_refs=(), check_refs=()) -> CheckpointSnapshot:
    """Capture default companions plus all named caller checkpoints; retry races.

    Immutable caller revisions/sentinels are separate inputs to later predicates,
    not ref names. No caller can subtract a default companion here.
    """
    names = tuple(dict.fromkeys([
        WRITER_NOTES_REF, "refs/notes/concrete-writer",
        "refs/notes/remote/concrete-writer", *([target_ref] if target_ref is not None else []),
        *old_refs, *check_refs,
    ]))
    for attempt in range(3):
        boundary = _VerifiedRepo(repo)
        for name in names:
            if not isinstance(name, str) or not name.startswith("refs/") or not _checkpoint_ref_syntax(boundary, name):
                raise ProvenanceError(f"invalid checkpoint ref name: {name}")
        rows = _checkpoint_git(boundary, ["for-each-ref", "--format=%(refname) %(objectname) %(objecttype)"]).stdout
        enumerated = {}
        for row in rows.splitlines():
            name, sha, kind = row.split()
            if name in names:
                enumerated[name] = sha
        snapshot = CheckpointSnapshot(boundary, {n: enumerated.get(n) for n in names})
        try:
            _checkpoint_transaction(snapshot, {}, "checkpoint capture")
            return snapshot
        except _CheckpointRace:
            if attempt == 2:
                raise _CheckpointRace("checkpoint ref race during checkpoint capture")


def sha256_file(path: str, label: str = "file") -> str:
    if not os.path.isfile(path):
        raise ProvenanceError(f"{label} '{path}' does not exist or is not a regular file")
    try:
        with open(path, "rb") as f:
            data = f.read()
    except OSError as exc:
        raise ProvenanceError(f"cannot read {label} '{path}': {exc}") from exc
    if not data:
        raise ProvenanceError(f"{label} '{path}' is empty")
    return hashlib.sha256(data).hexdigest()


def resolve_allowed_signers(repo: str, explicit_path: str | None = None) -> str:
    if explicit_path:
        return os.path.abspath(explicit_path)
    env_path = os.environ.get("REVIEW_WRITER_ALLOWED_SIGNERS", "").strip()
    if env_path:
        return os.path.abspath(env_path)
    repo_candidate = os.path.join(os.path.abspath(repo), "scripts", "review", "allowed_writer_log_signers")
    if os.path.isfile(repo_candidate):
        return repo_candidate
    return DEFAULT_ALLOWED_SIGNERS


def check_allowed_signers_has_keys(allowed_signers_path: str, source_desc: str, commit_hint: str = "") -> None:
    for_commit = f" for commit {commit_hint[:10]}" if commit_hint else ""
    if not os.path.isfile(allowed_signers_path):
        raise ProvenanceError(
            f"{source_desc}: unverified writer-log entry{for_commit}: "
            f"allowed-signers file '{allowed_signers_path}' does not exist"
        )
    try:
        with open(allowed_signers_path, "r", encoding="utf-8") as f:
            lines = f.readlines()
    except OSError as exc:
        raise ProvenanceError(
            f"{source_desc}: unverified writer-log entry{for_commit}: "
            f"cannot read allowed-signers file '{allowed_signers_path}': {exc}"
        ) from exc
    active = [ln.strip() for ln in lines if ln.strip() and not ln.lstrip().startswith("#")]
    if not active:
        raise ProvenanceError(
            f"{source_desc}: unverified writer-log entry{for_commit}: "
            f"no authorized signers configured in {allowed_signers_path} "
            f"(every entry is unverified until the orchestrator's public key is added after owner approval; see docs/REVIEW.md)"
        )


def resolve_signing_key_and_signer(
    repo: str,
    explicit_key: str | None = None,
    explicit_signer: str | None = None,
) -> tuple[str, str]:
    key_path = (explicit_key or os.environ.get("REVIEW_WRITER_SIGNING_KEY", "")).strip()
    if not key_path:
        proc = git_run(repo, ["config", "--get", "user.signingkey"], check=False)
        if proc.returncode == 0 and proc.stdout.strip():
            key_path = proc.stdout.strip()
    if not key_path or not os.path.isfile(key_path):
        raise ProvenanceError(
            "no SSH signing key configured (pass --signing-key <key>, set REVIEW_WRITER_SIGNING_KEY, "
            "or set git config gpg.format=ssh and user.signingkey=<key>)"
        )
    key_path = os.path.abspath(key_path)

    signer = (explicit_signer or os.environ.get("REVIEW_WRITER_SIGNER", "")).strip()
    if not signer and os.path.isfile(key_path + ".pub"):
        try:
            with open(key_path + ".pub", "r", encoding="utf-8") as f:
                pub_parts = f.read().strip().split(None, 2)
                if len(pub_parts) >= 3 and pub_parts[2].strip():
                    signer = pub_parts[2].strip()
        except OSError:
            pass
    if not signer:
        proc = git_run(repo, ["config", "--get", "user.email"], check=False)
        if proc.returncode == 0 and proc.stdout.strip():
            signer = proc.stdout.strip()
    if not signer:
        signer = "orchestrator@concrete"
    if not SIGNER_RE.match(signer):
        raise ProvenanceError(f"invalid signer principal '{signer}' (must not contain whitespace)")
    return key_path, signer


def resolve_commit(repo: str, rev: str, label: str = "commit") -> str:
    proc = git_run(repo, ["rev-parse", "-q", "--verify", f"{rev}^{{commit}}"], check=False)
    sha = proc.stdout.strip()
    if proc.returncode != 0 or not HEX40_RE.match(sha) or not commit_exists(repo, sha):
        raise ProvenanceError(f"{label} '{rev}' is not a valid commit")
    return sha


def resolve_optional_ref(repo: str, ref: str) -> str | None:
    """Resolve an optional ref to its 40-hex commit SHA, returning None iff the ref does not exist
    and raising ProvenanceError on any git failure or non-commit ref target."""
    ref_s = ref.strip()
    if not ref_s:
        return None
    if ref_s.startswith("refs/"):
        prefix = "refs/notes" if ref_s.startswith("refs/notes/") else "refs"
        fproc = git_run(repo, ["for-each-ref", "--format=%(refname) %(objectname)", prefix], check=True)
        matched_sha: str | None = None
        for ln in fproc.stdout.splitlines():
            parts = ln.strip().split(None, 1)
            if len(parts) == 2 and parts[0] == ref_s:
                matched_sha = parts[1].strip()
                break
        if matched_sha is None:
            return None
        if not HEX40_RE.match(matched_sha):
            raise ProvenanceError(f"ref '{ref_s}' did not resolve to a 40-hex object")
        return resolve_commit(repo, matched_sha, f"ref {ref_s}")
    return resolve_commit(repo, ref_s, f"ref {ref_s}")


def commit_exists(repo: str, sha: str) -> bool:
    rev = sha.strip()
    if not rev:
        return False
    if not HEX40_RE.match(rev):
        proc = git_run(repo, ["rev-parse", "-q", "--verify", f"{rev}^{{commit}}"], check=False)
        if proc.returncode != 0:
            err = (proc.stderr or proc.stdout or "").strip()
            if err:
                raise ProvenanceError(f"git rev-parse {rev} failed: {err}")
            return False
        rev = proc.stdout.strip()
        if not HEX40_RE.match(rev):
            raise ProvenanceError(f"git rev-parse {sha} returned invalid commit SHA '{rev}'")
    proc = git_run(repo, ["cat-file", "-t", rev], check=False)
    if proc.returncode == 0:
        return proc.stdout.strip() == "commit"
    err = (proc.stderr or "").strip().lower()
    if "not a valid object name" in err or "could not get object info" in err:
        return False
    detail = (proc.stderr or proc.stdout or f"exit {proc.returncode}").strip()
    raise ProvenanceError(f"git cat-file -t {rev[:10]} failed: {detail}")


def is_ancestor(repo: str, anc: str, desc: str) -> bool:
    proc = git_run(repo, ["merge-base", "--is-ancestor", anc, desc], check=False)
    if proc.returncode == 0:
        return True
    if proc.returncode == 1 and not (proc.stderr or "").strip() and not (proc.stdout or "").strip():
        return False
    detail = (proc.stderr or proc.stdout or f"exit {proc.returncode}").strip()
    raise ProvenanceError(f"git merge-base --is-ancestor {anc[:10]} {desc[:10]} failed: {detail}")


def is_clean_auto_merge(repo: str, sha: str, parents: list[str]) -> bool:
    """Return True iff `sha` is a two-parent merge whose tree equals git's conflict-free automatic merge."""
    if len(parents) != 2:
        return False
    proc = git_run(repo, ["merge-tree", "--write-tree", parents[0], parents[1]], check=False)
    first_line = proc.stdout.strip().splitlines()[0] if proc.stdout.strip() else ""
    if proc.returncode not in (0, 1) or not HEX40_RE.match(first_line):
        detail = (proc.stderr or proc.stdout or f"exit {proc.returncode}").strip()
        raise ProvenanceError(f"git merge-tree --write-tree failed for merge {sha[:10]}: {detail}")
    if proc.returncode != 0:
        return False
    commit_tree = git_run(repo, ["rev-parse", "--verify", f"{sha}^{{tree}}"], check=True).stdout.strip()
    if not HEX40_RE.match(commit_tree):
        raise ProvenanceError(f"cannot resolve tree for merge {sha[:10]}")
    return first_line == commit_tree


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


def canonical_body_keys(entry: dict[str, str]) -> list[str]:
    source = entry.get("source", "harness")
    if source == "backfill":
        keys = [
            "model",
            "round",
            "branch",
            "brief-sha256",
            "output-sha256",
            "start-sha",
            "end-sha",
            "commit",
        ]
        if entry.get("model") == "agy" and "agy-diff-sha256" in entry:
            keys.append("agy-diff-sha256")
        keys.extend(["source", "evidence"])
    else:
        keys = ["model", "round", "branch", "brief-sha256", "start-sha", "end-sha", "commit"]
        if entry.get("model") == "agy":
            keys.append("agy-diff-sha256")
        keys.append("source")
    if "supersedes" in entry:
        keys.extend(["supersedes", "reason"])
    return keys


def format_unsigned_body(entry: dict[str, str]) -> str:
    keys = canonical_body_keys(entry)
    return "\n".join(f"{k}: {entry[k]}" for k in keys)


def compute_entry_id(entry: dict[str, str]) -> str:
    body = format_unsigned_body(entry) + "\n"
    return hashlib.sha256(body.encode("utf-8")).hexdigest()


def sign_entry(entry: dict[str, str], signing_key: str, signer: str) -> dict[str, str]:
    entry = dict(entry)
    entry["entry-id"] = compute_entry_id(entry)
    entry["signer"] = signer
    payload = format_unsigned_body(entry) + f"\nentry-id: {entry['entry-id']}\nsigner: {signer}\n"
    with tempfile.TemporaryDirectory() as tmpdir:
        payload_path = os.path.join(tmpdir, "payload")
        with open(payload_path, "w", encoding="utf-8") as f:
            f.write(payload)
        proc = subprocess.run(
            ["ssh-keygen", "-Y", "sign", "-q", "-f", signing_key, "-n", "git", payload_path],
            capture_output=True,
            text=True,
            check=False,
        )
        if proc.returncode != 0:
            raise ProvenanceError(f"ssh-keygen -Y sign failed: {proc.stderr.strip()}")
        sig_path = payload_path + ".sig"
        if not os.path.isfile(sig_path):
            raise ProvenanceError("ssh-keygen -Y sign did not produce a .sig file")
        with open(sig_path, "r", encoding="utf-8") as f:
            sig_block = f.read().strip()
    entry["_sig_block"] = sig_block
    return entry


_SIG_TMPDIR: str | None = None
_SIG_TMPDIR_LOCK = threading.Lock()


def _get_sig_tmpdir() -> str:
    global _SIG_TMPDIR
    with _SIG_TMPDIR_LOCK:
        if _SIG_TMPDIR is None or not os.path.isdir(_SIG_TMPDIR):
            _SIG_TMPDIR = tempfile.mkdtemp(prefix="wp-sig-")
            atexit.register(shutil.rmtree, _SIG_TMPDIR, True)
        return _SIG_TMPDIR


def verify_stanza_signature(
    signed_payload: str,
    sig_block: str,
    signer: str,
    commit_sha: str,
    allowed_signers_path: str,
    source_desc: str,
) -> None:
    check_allowed_signers_has_keys(allowed_signers_path, source_desc, commit_sha)
    sig_path = os.path.join(_get_sig_tmpdir(), f"stanza-{threading.get_ident()}.sig")
    with open(sig_path, "w", encoding="utf-8") as f:
        f.write(sig_block + "\n")
    ver_proc = subprocess.run(
        ["ssh-keygen", "-Y", "verify", "-f", allowed_signers_path, "-I", signer, "-n", "git", "-s", sig_path],
        input=signed_payload,
        capture_output=True,
        text=True,
        check=False,
    )
    if ver_proc.returncode == 0:
        return
    find_proc = subprocess.run(
        ["ssh-keygen", "-Y", "find-principals", "-f", allowed_signers_path, "-s", sig_path],
        capture_output=True,
        text=True,
        check=False,
    )
    if find_proc.returncode != 0:
        detail = find_proc.stderr.strip() or find_proc.stdout.strip() or "unknown signing key"
        raise ProvenanceError(
            f"{source_desc}: unverified writer-log entry for commit {commit_sha[:10]}: "
            f"signing key is not authorized in {allowed_signers_path} ({detail})"
        )
    principals = [ln.strip() for ln in find_proc.stdout.splitlines() if ln.strip()]
    if signer not in principals:
        raise ProvenanceError(
            f"{source_desc}: unverified writer-log entry for commit {commit_sha[:10]}: "
            f"signer '{signer}' does not match authorized principal(s) {principals} in {allowed_signers_path}"
        )
    detail = ver_proc.stderr.strip() or ver_proc.stdout.strip() or "signature verification failed"
    raise ProvenanceError(
        f"{source_desc}: invalid SSH signature on writer-log entry for commit {commit_sha[:10]}: {detail}"
    )


def parse_log_stanza(
    stanza: str,
    source_desc: str = "writer-log",
    allowed_signers_path: str | None = None,
) -> dict[str, str]:
    lines = [ln for ln in stanza.strip("\n").splitlines()]
    if not lines:
        raise ProvenanceError(f"{source_desc}: empty writer-log entry")

    sig_start_idx: int | None = None
    for idx, ln in enumerate(lines):
        if ln == SIG_BEGIN:
            sig_start_idx = idx
            break

    if sig_start_idx is not None:
        kv_lines = lines[:sig_start_idx]
        sig_lines = lines[sig_start_idx:]
        if len(sig_lines) < 3 or sig_lines[0] != SIG_BEGIN or sig_lines[-1] != SIG_END:
            raise ProvenanceError(f"{source_desc}: malformed SSH signature block")
        for sl in sig_lines[1:-1]:
            if not sl or not re.match(r"^[A-Za-z0-9+/=]+$", sl):
                raise ProvenanceError(f"{source_desc}: malformed SSH signature line '{sl}'")
        sig_block = "\n".join(sig_lines)
    else:
        kv_lines = lines
        sig_block = ""

    if not kv_lines:
        raise ProvenanceError(f"{source_desc}: empty writer-log key-value body")

    entry: dict[str, str] = {}
    keys_in_order: list[str] = []
    for line in kv_lines:
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

    source = entry.get("source", "")
    if source not in ("harness", "backfill"):
        raise ProvenanceError(f"{source_desc}: invalid or missing source '{source}' (expected harness or backfill)")

    has_supersedes = "supersedes" in entry
    has_reason = "reason" in entry
    if has_supersedes != has_reason:
        raise ProvenanceError(f"{source_desc}: 'supersedes' and 'reason' must both be present or both omitted")
    if has_supersedes:
        if not HEX64_RE.match(entry["supersedes"]):
            raise ProvenanceError(f"{source_desc}: invalid supersedes entry-id '{entry['supersedes']}' (expected 64-hex)")
        if not entry["reason"].strip() or "\n" in entry["reason"] or "\r" in entry["reason"]:
            raise ProvenanceError(f"{source_desc}: correction entry requires non-empty single-line reason")

    if source == "harness":
        expected_keys = ["model", "round", "branch", "brief-sha256", "start-sha", "end-sha", "commit"]
        if model == "agy":
            expected_keys.append("agy-diff-sha256")
        expected_keys.append("source")
        if has_supersedes:
            expected_keys.extend(["supersedes", "reason"])
        if "entry-id" in entry:
            expected_keys.append("entry-id")
        if "signer" in entry:
            expected_keys.append("signer")
        if keys_in_order != expected_keys:
            raise ProvenanceError(
                f"{source_desc}: keys {keys_in_order} do not match expected {expected_keys}"
            )
    else:
        required_keys = {
            "model",
            "round",
            "branch",
            "brief-sha256",
            "output-sha256",
            "start-sha",
            "end-sha",
            "commit",
            "source",
            "evidence",
        }
        allowed_keys = set(required_keys) | {"entry-id", "signer", "supersedes", "reason"}
        if model == "agy":
            allowed_keys.add("agy-diff-sha256")
        present_keys = set(keys_in_order)
        missing = sorted(required_keys - present_keys)
        extra = sorted(present_keys - allowed_keys)
        if missing:
            raise ProvenanceError(f"{source_desc}: backfill entry missing required key(s): {', '.join(missing)}")
        if extra:
            raise ProvenanceError(f"{source_desc}: backfill entry has unknown key(s): {', '.join(extra)}")
        if not HEX64_RE.match(entry["output-sha256"]):
            raise ProvenanceError(f"{source_desc}: invalid output-sha256 '{entry['output-sha256']}'")
        if not entry["evidence"].strip() or "\n" in entry["evidence"] or "\r" in entry["evidence"]:
            raise ProvenanceError(f"{source_desc}: backfill entry requires non-empty single-line evidence")

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
    if "agy-diff-sha256" in entry:
        if model != "agy":
            raise ProvenanceError(f"{source_desc}: {model} entry must not have agy-diff-sha256")
        if not HEX64_RE.match(entry["agy-diff-sha256"]):
            raise ProvenanceError(f"{source_desc}: invalid agy-diff-sha256 '{entry['agy-diff-sha256']}'")

    entry["source"] = source

    if not sig_block or "entry-id" not in entry or "signer" not in entry:
        raise ProvenanceError(
            f"{source_desc}: unverified writer-log entry for commit {entry['commit'][:10]}: "
            f"missing entry-id, signer, or SSH signature"
        )
    if not HEX64_RE.match(entry["entry-id"]):
        raise ProvenanceError(f"{source_desc}: invalid entry-id '{entry['entry-id']}'")
    if not entry["signer"] or not SIGNER_RE.match(entry["signer"]):
        raise ProvenanceError(f"{source_desc}: invalid signer '{entry['signer']}'")
    if keys_in_order[-2:] != ["entry-id", "signer"]:
        raise ProvenanceError(f"{source_desc}: entry-id and signer must be the final two key-value lines before signature")

    expected_entry_id = compute_entry_id(entry)
    if entry["entry-id"] != expected_entry_id:
        raise ProvenanceError(
            f"{source_desc}: entry-id mismatch on commit {entry['commit'][:10]} "
            f"(recorded {entry['entry-id'][:10]}, computed {expected_entry_id[:10]}; entry modified after signing)"
        )

    eff_signers = allowed_signers_path or DEFAULT_ALLOWED_SIGNERS
    signed_payload = "\n".join(kv_lines) + "\n"
    verify_stanza_signature(
        signed_payload=signed_payload,
        sig_block=sig_block,
        signer=entry["signer"],
        commit_sha=entry["commit"],
        allowed_signers_path=eff_signers,
        source_desc=source_desc,
    )
    entry["_sig_block"] = sig_block
    return entry


def format_log_stanza(entry: dict[str, str]) -> str:
    body = format_unsigned_body(entry)
    if "entry-id" in entry and "signer" in entry and "_sig_block" in entry:
        return f"{body}\nentry-id: {entry['entry-id']}\nsigner: {entry['signer']}\n{entry['_sig_block']}"
    return body


def parse_multi_stanza_log(
    text: str,
    source_desc: str = "writer-log",
    allowed_signers_path: str | None = None,
) -> dict[str, list[dict[str, str]]]:
    text_stripped = text.strip()
    if not text_stripped:
        return {}
    raw_stanzas = re.split(r"\n\n+(?=model: )", text_stripped)
    if len(raw_stanzas) == 1:
        raw_stanzas = re.split(r"\n\n+", text_stripped)
    by_commit: dict[str, list[dict[str, str]]] = {}
    for idx, raw in enumerate(raw_stanzas, 1):
        entry = parse_log_stanza(raw, f"{source_desc} stanza {idx}", allowed_signers_path)
        c = entry["commit"]
        if c not in by_commit:
            if "supersedes" in entry:
                raise ProvenanceError(
                    f"{source_desc} stanza {idx}: initial writer-log entry for commit {c[:10]} "
                    f"must not set supersedes ({entry['supersedes'][:10]})"
                )
            by_commit[c] = [entry]
        else:
            prev = by_commit[c][-1]
            if "supersedes" not in entry:
                raise ProvenanceError(
                    f"{source_desc} stanza {idx}: duplicate writer-log entry for commit {c} without supersedes "
                    f"(the writer log is append-only; corrections must set supersedes and reason)"
                )
            if entry["supersedes"] != prev["entry-id"]:
                raise ProvenanceError(
                    f"{source_desc} stanza {idx}: correction entry for commit {c[:10]} supersedes "
                    f"{entry['supersedes'][:10]}, which is not the latest entry {prev['entry-id'][:10]}"
                )
            by_commit[c].append(entry)
    return by_commit


def load_log_object(
    repo: str,
    log_object: str,
    allowed_signers_path: str | None = None,
) -> dict[str, list[dict[str, str]]]:
    eff_signers = resolve_allowed_signers(repo, allowed_signers_path)
    if os.path.isfile(log_object):
        with open(log_object, "r", encoding="utf-8") as f:
            content = f.read()
        return parse_multi_stanza_log(content, log_object, eff_signers)
    proc = git_run(repo, ["cat-file", "blob", log_object], check=False)
    if proc.returncode != 0:
        raise ProvenanceError(f"cannot read writer-log object '{log_object}'")
    return parse_multi_stanza_log(proc.stdout, f"blob {log_object}", eff_signers)


def read_optional_note(repo: str, notes_ref: str, sha: str) -> str | None:
    """Read the note on `sha` under `notes_ref`, returning None iff the ref or note does not exist
    and raising ProvenanceError on any git failure."""
    if resolve_optional_ref(repo, notes_ref) is None:
        return None
    proc = git_run(repo, ["notes", f"--ref={notes_ref}", "show", sha], check=False)
    if proc.returncode == 0:
        return proc.stdout
    err = (proc.stderr or "").strip().lower()
    if proc.returncode == 1 and "no note found for object" in err:
        return None
    detail = (proc.stderr or proc.stdout or f"exit {proc.returncode}").strip()
    raise ProvenanceError(f"git notes --ref={notes_ref} show {sha[:10]} failed: {detail}")


_VALIDATED_NOTE_BLOB_CACHE: dict[tuple[str, str, str], list[dict[str, str]]] = {}
_BLOB_CACHE_LOCK = threading.Lock()


def _signers_file_sha256(eff_signers: str) -> str:
    try:
        with open(eff_signers, "rb") as f:
            return hashlib.sha256(f.read()).hexdigest()
    except OSError as exc:
        raise ProvenanceError(f"allowed-signers file '{eff_signers}' cannot be read: {exc}") from exc


def parse_and_verify_note_blob(
    annotated_sha: str,
    blob_text: str,
    eff_signers: str,
) -> list[dict[str, str]]:
    """Parse and cryptographically verify every stanza in a single commit's writer-note blob,
    enforcing single-commit note authority. Cached in-process only by content hash of
    (annotated_sha, blob_text, allowed_signers_content) after full validation in this process."""
    if not blob_text or not blob_text.strip():
        raise ProvenanceError(f"{WRITER_NOTES_REF} on {annotated_sha[:10]} is empty")
    blob_hash = hashlib.sha256(blob_text.encode("utf-8")).hexdigest()
    signers_hash = _signers_file_sha256(eff_signers)
    cache_key = (annotated_sha, blob_hash, signers_hash)
    with _BLOB_CACHE_LOCK:
        cached = _VALIDATED_NOTE_BLOB_CACHE.get(cache_key)
        if cached is not None:
            return [dict(e) for e in cached]
    stanzas = parse_multi_stanza_log(
        blob_text, f"{WRITER_NOTES_REF} on {annotated_sha[:10]}", eff_signers
    )
    if not stanzas:
        raise ProvenanceError(f"{WRITER_NOTES_REF} on {annotated_sha[:10]} is empty")
    for c_sha, c_hist in stanzas.items():
        if c_sha != annotated_sha:
            raise ProvenanceError(
                f"{WRITER_NOTES_REF} on {annotated_sha[:10]} records commit {c_hist[-1]['commit']}, not {annotated_sha} "
                f"(each commit has one authoritative writer history on its own note)"
            )
    history = stanzas[annotated_sha]
    with _BLOB_CACHE_LOCK:
        _VALIDATED_NOTE_BLOB_CACHE[cache_key] = [dict(e) for e in history]
    return history


def get_commit_note_history(
    repo: str,
    sha: str,
    allowed_signers_path: str | None = None,
) -> list[dict[str, str]] | None:
    note_text = read_optional_note(repo, WRITER_NOTES_REF, sha)
    if note_text is None:
        return None
    eff_signers = resolve_allowed_signers(repo, allowed_signers_path)
    return parse_and_verify_note_blob(sha, note_text, eff_signers)


def verify_history_against_authority(
    commit_sha: str,
    candidate: list[dict[str, str]] | None,
    authority: list[dict[str, str]] | None,
    source_desc: str = "exported writer-log",
) -> None:
    """Verify an exported or external per-commit history against the commit's authoritative note history."""
    if authority is None and candidate is None:
        return
    if authority is None and candidate is not None:
        raise ProvenanceError(
            f"{source_desc} contains history for commit {commit_sha[:10]}, which has no authoritative note in {WRITER_NOTES_REF}"
        )
    if authority is not None and candidate is None:
        raise ProvenanceError(
            f"{source_desc} is missing history for commit {commit_sha[:10]} "
            f"(authoritative note in {WRITER_NOTES_REF} has {len(authority)} entr{'y' if len(authority) == 1 else 'ies'})"
        )
    assert authority is not None and candidate is not None
    auth_ids = [e["entry-id"] for e in authority]
    cand_ids = [e["entry-id"] for e in candidate]
    auth_stanzas = [format_log_stanza(e) for e in authority]
    cand_stanzas = [format_log_stanza(e) for e in candidate]
    if len(candidate) < len(authority) and cand_ids == auth_ids[: len(candidate)]:
        raise ProvenanceError(
            f"{source_desc} has truncated history for commit {commit_sha[:10]} "
            f"({len(candidate)} of {len(authority)} entries, ending at {cand_ids[-1][:10]} instead of {auth_ids[-1][:10]})"
        )
    if cand_stanzas != auth_stanzas:
        raise ProvenanceError(
            f"{source_desc} conflicts with authoritative writer history for commit {commit_sha[:10]} in {WRITER_NOTES_REF}"
        )


def validate_entry_against_commit(
    repo: str,
    sha: str,
    head_sha: str,
    log_entry: dict[str, str],
    has_commit_stamp: bool,
    an: str,
    ae: str,
    writer_trailers: list[str],
    agy_trailers: list[str],
) -> str:
    if log_entry.get("source") == "backfill":
        if has_commit_stamp:
            raise ProvenanceError(
                f"commit {sha[:10]} carries a harness commit stamp and cannot use a backfilled writer-log entry"
            )
        if log_entry["commit"] != sha:
            raise ProvenanceError(
                f"commit {sha[:10]} writer-log entry is for commit {log_entry['commit']}, not {sha}"
            )
        model = log_entry["model"]
    else:
        if len(writer_trailers) == 0:
            raise ProvenanceError(
                f"commit {sha[:10]} has writer author or harness log entry but no 'Writer: <model>' trailer"
            )
        if len(writer_trailers) > 1:
            raise ProvenanceError(f"commit {sha[:10]} has multiple 'Writer:' trailers")

        model = writer_trailers[0]
        if model not in VALID_MODELS:
            raise ProvenanceError(
                f"commit {sha[:10]} has invalid 'Writer: {model}' trailer (expected codex, kimi, agy or claude)"
            )

        expected_ae = f"{model}@writers.concrete"
        expected_an_choices = {
            f"{MODEL_DISPLAY[model]} (writer)",
            f"{model.capitalize()} (writer)",
            f"{model} (writer)",
        }
        if ae != expected_ae or an not in expected_an_choices:
            raise ProvenanceError(
                f"commit {sha[:10]} trailer 'Writer: {model}' does not match author '{an} <{ae}>' "
                f"(expected '{MODEL_DISPLAY[model]} (writer) <{expected_ae}>')"
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
    return model


def inspect_commit(
    repo: str,
    sha: str,
    head_sha: str,
    external_log: dict[str, list[dict[str, str]]] | None,
    allowed_signers_path: str | None = None,
) -> tuple[str | None, dict[str, str] | None, list[dict[str, str]], bool, bool]:
    """Inspect a single commit's writer provenance and full entry history.

    Returns `(writer_model, active_entry, history_entries, is_merge, merge_has_resolution)`.
    """
    proc = git_run(
        repo,
        ["show", "-s", "--format=%P%n%an%n%ae%n%cn%n%ce%n%B", sha],
        check=True,
    )
    lines = proc.stdout.splitlines()
    parents = lines[0].split() if lines else []
    is_merge = len(parents) > 1
    merge_has_resolution = is_merge and not is_clean_auto_merge(repo, sha, parents)
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

    has_commit_stamp = author_has_writer_tag or bool(writer_trailers) or bool(agy_trailers)

    if external_log is not None:
        history = external_log.get(sha)
    else:
        history = get_commit_note_history(repo, sha, allowed_signers_path)

    if not has_commit_stamp and not history:
        return None, None, [], is_merge, merge_has_resolution

    if not cn or not ce or ce.lower().endswith("@writers.concrete") or "(writer)" in cn.lower():
        raise ProvenanceError(
            f"commit {sha[:10]} committer '{cn} <{ce}>' must be the owner's identity, not a writer identity"
        )

    if not history:
        if len(writer_trailers) == 0:
            raise ProvenanceError(
                f"commit {sha[:10]} has writer author or harness log entry but no 'Writer: <model>' trailer"
            )
        model = writer_trailers[0]
        if model not in VALID_MODELS:
            raise ProvenanceError(
                f"commit {sha[:10]} has invalid 'Writer: {model}' trailer (expected codex, kimi, agy or claude)"
            )
        expected_ae = f"{model}@writers.concrete"
        expected_an_choices = {
            f"{MODEL_DISPLAY[model]} (writer)",
            f"{model.capitalize()} (writer)",
            f"{model} (writer)",
        }
        if ae != expected_ae or an not in expected_an_choices:
            raise ProvenanceError(
                f"commit {sha[:10]} trailer 'Writer: {model}' does not match author '{an} <{ae}>' "
                f"(expected '{MODEL_DISPLAY[model]} (writer) <{expected_ae}>')"
            )
        raise ProvenanceError(
            f"commit {sha[:10]} has 'Writer: {model}' stamp but no matching writer-log entry "
            f"(a Writer: trailer alone is never enough)"
        )

    model: str | None = None
    for entry in history:
        model = validate_entry_against_commit(
            repo=repo,
            sha=sha,
            head_sha=head_sha,
            log_entry=entry,
            has_commit_stamp=has_commit_stamp,
            an=an,
            ae=ae,
            writer_trailers=writer_trailers,
            agy_trailers=agy_trailers,
        )
    active = dict(history[-1])
    return model, active, history, is_merge, merge_has_resolution


def parse_evidence_specs(
    repo: str,
    brief_specs: list[str] | None,
    output_specs: list[str] | None,
    evidence_dir: str | None,
) -> tuple[dict[str, str], dict[str, str], list[str], list[str], str | None]:
    by_commit_brief: dict[str, str] = {}
    by_commit_output: dict[str, str] = {}
    unkeyed_briefs: list[str] = []
    unkeyed_outputs: list[str] = []

    for item in brief_specs or []:
        if "=" in item and not os.path.exists(item):
            rev, path = item.split("=", 1)
            sha = resolve_commit(repo, rev, "brief-file commit")
            by_commit_brief[sha] = path
        else:
            unkeyed_briefs.append(item)

    for item in output_specs or []:
        if "=" in item and not os.path.exists(item):
            rev, path = item.split("=", 1)
            sha = resolve_commit(repo, rev, "output-file commit")
            by_commit_output[sha] = path
        else:
            unkeyed_outputs.append(item)

    eff_dir = evidence_dir or os.environ.get("REVIEW_WRITER_EVIDENCE_DIR", "").strip() or None
    return by_commit_brief, by_commit_output, unkeyed_briefs, unkeyed_outputs, eff_dir


def verify_backfill_evidence_for_entries(
    repo: str,
    base_sha: str,
    head_sha: str,
    entries: list[dict[str, str]],
    brief_specs: list[str] | None = None,
    output_specs: list[str] | None = None,
    evidence_dir: str | None = None,
) -> None:
    by_commit_brief, by_commit_output, unkeyed_briefs, unkeyed_outputs, eff_dir = parse_evidence_specs(
        repo, brief_specs, output_specs, evidence_dir
    )
    backfill_entries = [e for e in entries if e.get("source") == "backfill"]
    backfill_by_commit = {e["commit"]: e for e in backfill_entries}

    if unkeyed_briefs or unkeyed_outputs:
        if not backfill_entries:
            raise ProvenanceError(
                f"backfill evidence file(s) provided, but {base_sha[:10]}..{head_sha[:10]} has no backfilled commits"
            )
        if len(backfill_entries) > 1 or len(unkeyed_briefs) > 1 or len(unkeyed_outputs) > 1:
            raise ProvenanceError(
                "multiple backfilled commits in range; bind evidence files per commit with "
                "--brief-file <commit>=<path> and --output-file <commit>=<path> (or --evidence-dir <dir>)"
            )
        only_c = backfill_entries[0]["commit"]
        if unkeyed_briefs:
            by_commit_brief[only_c] = unkeyed_briefs[0]
        if unkeyed_outputs:
            by_commit_output[only_c] = unkeyed_outputs[0]

    extra_commits = (set(by_commit_brief.keys()) | set(by_commit_output.keys())) - set(backfill_by_commit.keys())
    if extra_commits:
        extra_str = ", ".join(sorted(c[:10] for c in extra_commits))
        raise ProvenanceError(
            f"backfill evidence file(s) provided for non-backfilled commit(s) in {base_sha[:10]}..{head_sha[:10]}: {extra_str}"
        )

    for e in backfill_entries:
        c = e["commit"]
        b_path = by_commit_brief.get(c)
        o_path = by_commit_output.get(c)
        if eff_dir and not b_path and not o_path:
            cand_b = os.path.join(eff_dir, f"{c}.brief")
            cand_o = os.path.join(eff_dir, f"{c}.output")
            if os.path.exists(cand_b) or os.path.exists(cand_o):
                if not (os.path.exists(cand_b) and os.path.exists(cand_o)):
                    raise ProvenanceError(
                        f"commit {c[:10]} backfill evidence directory '{eff_dir}' must provide both {c}.brief and {c}.output"
                    )
                b_path = cand_b
                o_path = cand_o
        if b_path:
            got_b = sha256_file(b_path, f"commit {c[:10]} brief-file")
            if got_b != e["brief-sha256"]:
                raise ProvenanceError(
                    f"commit {c[:10]} backfill brief-sha256 mismatch: entry records {e['brief-sha256']}, "
                    f"file '{b_path}' has {got_b}"
                )
        if o_path:
            got_o = sha256_file(o_path, f"commit {c[:10]} output-file")
            if got_o != e["output-sha256"]:
                raise ProvenanceError(
                    f"commit {c[:10]} backfill output-sha256 mismatch: entry records {e['output-sha256']}, "
                    f"file '{o_path}' has {got_o}"
                )
        if b_path and o_path:
            e["_evidence_verified"] = "yes"
        else:
            e["_evidence_verified"] = "no"


def derive_range_provenance(
    repo: str,
    base_rev: str,
    head_rev: str,
    external_log: dict[str, list[dict[str, str]]] | None = None,
    allowed_signers_path: str | None = None,
    brief_specs: list[str] | None = None,
    output_specs: list[str] | None = None,
    evidence_dir: str | None = None,
    log_object: str | None = None,
) -> tuple[list[str], list[dict[str, str]], dict[str, list[dict[str, str]]]]:
    """Derive the ordered unique writer list, active entries, and full per-commit histories for `base..head`.

    Authoritative per-commit validation, ancestor continuity, and companion-ref continuity run
    unconditionally on every call. Any `--log-object` / `external_log` export is only ever an extra
    cross-check after authoritative validation succeeds.
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

    eff_signers = resolve_allowed_signers(repo, allowed_signers_path)
    read_validated_writer_notes(repo, allowed_signers_path, target_commits=set(commits))

    writers: list[str] = []
    log_entries: list[dict[str, str]] = []
    histories: dict[str, list[dict[str, str]]] = {}

    def add_writer(w: str) -> None:
        if w not in writers:
            writers.append(w)

    for c in commits:
        model, entry, history, is_merge, merge_has_resolution = inspect_commit(
            repo, c, head_sha, None, eff_signers
        )
        if is_merge and not merge_has_resolution:
            continue
        if model is not None and entry is not None:
            add_writer(model)
            log_entries.append(entry)
            histories[c] = history
        elif is_merge:
            raise ProvenanceError(
                f"merge commit {c[:10]} has a merge resolution (differs from git's automatic merge) "
                f"but has an unknown writer (no harness stamp or backfilled writer-log entry)"
            )
        else:
            raise ProvenanceError(
                f"commit {c[:10]} is unstamped and has no backfilled writer-log entry "
                f"(unknown writer; unstamped commits are never attributed to claude by default)"
            )

    if not writers:
        raise ProvenanceError(
            f"no writer provenance recorded in {base_sha[:10]}..{head_sha[:10]}"
        )

    verify_backfill_evidence_for_entries(
        repo=repo,
        base_sha=base_sha,
        head_sha=head_sha,
        entries=log_entries,
        brief_specs=brief_specs,
        output_specs=output_specs,
        evidence_dir=evidence_dir,
    )

    if log_object is not None:
        external_log = load_log_object(repo, log_object, allowed_signers_path)

    if external_log is not None:
        for c in commits:
            if c not in histories:
                if c in external_log:
                    verify_history_against_authority(c, external_log.get(c), None)
                continue
            verify_history_against_authority(c, external_log.get(c), histories.get(c))
        log_commits = set(external_log.keys())
        stamped_commits = {e["commit"] for e in log_entries}
        extra = log_commits - stamped_commits
        if extra:
            extra_str = ", ".join(sorted(c[:10] for c in extra))
            raise ProvenanceError(
                f"writer-log contains entries for commits not in {base_sha[:10]}..{head_sha[:10]}: {extra_str}"
            )

    return writers, log_entries, histories


def check_reviewer_against_history(
    reviewer: str,
    writers: list[str],
    histories: dict[str, list[dict[str, str]]],
) -> None:
    if reviewer not in VALID_MODELS:
        raise ProvenanceError(f"invalid reviewer '{reviewer}'")
    if reviewer in writers:
        detail = ""
        for c, hist in histories.items():
            if hist and hist[-1]["model"] == reviewer:
                detail = (
                    f" — commit {c[:10]} writer-log entry attributes {c[:10]} to '{reviewer}', "
                    f"which is the reviewer on its review note"
                )
                break
        raise ProvenanceError(
            f"reviewer {reviewer} cannot review its own work (writer {','.join(writers)}){detail}"
        )
    for c, hist in histories.items():
        if len(hist) > 1:
            latest = hist[-1]
            for earlier in hist[:-1]:
                if earlier["model"] == reviewer:
                    raise ProvenanceError(
                        f"reviewer {reviewer} cannot review its own work: commit {c[:10]} was attributed to "
                        f"{reviewer} in earlier signed entry {earlier['entry-id'][:10]} before correction "
                        f"to {latest['model']} in {latest['entry-id'][:10]} (which is the reviewer on its review note)"
                    )


def get_review_note_reviewers(repo: str, sha: str) -> list[str]:
    reviewers: list[str] = []
    for ref in dict.fromkeys(
        [REVIEW_NOTES_REF, "refs/notes/concrete-review", "refs/notes/remote/concrete-review"]
    ):
        if not ref:
            continue
        note_text = read_optional_note(repo, ref, sha)
        if not note_text or not note_text.strip():
            continue
        for line in note_text.splitlines():
            m = re.match(r"^reviewer:[ \t]*(.+)$", line)
            if m:
                rev = m.group(1).strip()
                if rev and rev not in reviewers:
                    reviewers.append(rev)
    return reviewers


def get_review_note_reviewer(repo: str, sha: str) -> str | None:
    revs = get_review_note_reviewers(repo, sha)
    return revs[0] if revs else None


def format_provenance_summary(
    writers: list[str],
    entries: list[dict[str, str]],
    histories: dict[str, list[dict[str, str]]] | None = None,
) -> str:
    harness = [e for e in entries if e.get("source", "harness") == "harness"]
    backfill = [e for e in entries if e.get("source") == "backfill"]
    h_items = ",".join(f"{e['commit'][:10]}:{e['model']}@r{e['round']}" for e in harness) or "none"
    b_items = ",".join(f"{e['commit'][:10]}:{e['model']}@r{e['round']}" for e in backfill) or "none"
    parts = [
        f"writers: {','.join(writers)}",
        f"harness ({len(harness)}): {h_items}",
        f"backfill ({len(backfill)}): {b_items}",
    ]
    if histories:
        corr_chains: list[str] = []
        for e in entries:
            c = e["commit"]
            hist = histories.get(c, [e])
            if len(hist) > 1:
                steps = "->".join(
                    f"{h['model']}@r{h['round']}[{h['entry-id'][:10]}]" for h in hist
                )
                reasons = "; ".join(
                    f"{h['entry-id'][:10]} supersedes {h['supersedes'][:10]} ({h['reason']})"
                    for h in hist[1:]
                )
                corr_chains.append(f"{c[:10]}:{steps} ({reasons})")
        if corr_chains:
            parts.append(f"history: {', '.join(corr_chains)}")
    if backfill:
        all_verified = all(e.get("_evidence_verified") == "yes" for e in backfill)
        if all_verified:
            parts.append("backfill-evidence: verified")
        else:
            parts.append("backfill-evidence: evidence not re-checked")
    return "; ".join(parts)


def cmd_derive(args: argparse.Namespace) -> int:
    try:
        args = argparse.Namespace(**vars(args))
        args.repo = _VerifiedRepo(str(args.repo))
        writers, entries, histories = derive_range_provenance(
            repo=args.repo,
            base_rev=args.base,
            head_rev=args.head,
            allowed_signers_path=args.allowed_signers,
            brief_specs=args.brief_file,
            output_specs=args.output_file,
            evidence_dir=args.evidence_dir,
            log_object=args.log_object,
        )
        if args.assert_writer:
            claimed = parse_writer_csv(args.assert_writer, "--writer")
            if set(claimed) != set(writers):
                raise ProvenanceError(
                    f"--writer '{args.assert_writer}' does not match derived writer provenance "
                    f"'{','.join(writers)}' for {args.base[:10]}..{args.head[:10]}"
                )
        if getattr(args, "reviewer", None):
            check_reviewer_against_history(args.reviewer, writers, histories)
        if getattr(args, "report", False):
            print(format_provenance_summary(writers, entries, histories))
        else:
            print(",".join(writers))
        return 0
    except ProvenanceError as exc:
        print(f"writer-provenance: {exc}", file=sys.stderr)
        return 2


def cmd_report(args: argparse.Namespace) -> int:
    try:
        args = argparse.Namespace(**vars(args))
        args.repo = _VerifiedRepo(str(args.repo))
        writers, entries, histories = derive_range_provenance(
            repo=args.repo,
            base_rev=args.base,
            head_rev=args.head,
            allowed_signers_path=args.allowed_signers,
            brief_specs=args.brief_file,
            output_specs=args.output_file,
            evidence_dir=args.evidence_dir,
            log_object=args.log_object,
        )
        print(format_provenance_summary(writers, entries, histories))
        return 0
    except ProvenanceError as exc:
        print(f"writer-provenance: {exc}", file=sys.stderr)
        return 2


def cmd_verify(args: argparse.Namespace) -> int:
    try:
        args = argparse.Namespace(**vars(args))
        args.repo = _VerifiedRepo(str(args.repo))
        if args.reviewer not in VALID_MODELS:
            raise ProvenanceError(f"invalid reviewer '{args.reviewer}'")
        claimed = parse_writer_csv(args.claimed_writer, "note writer")
        if args.reviewer in claimed:
            raise ProvenanceError(
                f"reviewer {args.reviewer} reviewed its own work (writer {args.claimed_writer})"
            )
        writers, entries, histories = derive_range_provenance(
            repo=args.repo,
            base_rev=args.base,
            head_rev=args.head,
            allowed_signers_path=args.allowed_signers,
            brief_specs=args.brief_file,
            output_specs=args.output_file,
            evidence_dir=args.evidence_dir,
            log_object=args.log_object,
        )
        check_reviewer_against_history(args.reviewer, writers, histories)
        if set(claimed) != set(writers):
            raise ProvenanceError(
                f"note writer '{args.claimed_writer}' does not match derived writer provenance "
                f"'{','.join(writers)}' for {args.base[:10]}..{args.head[:10]}"
            )
        print(format_provenance_summary(writers, entries, histories))
        return 0
    except ProvenanceError as exc:
        print(f"{exc}")
        return 1


def cmd_export_log(args: argparse.Namespace) -> int:
    try:
        args = argparse.Namespace(**vars(args))
        args.repo = _VerifiedRepo(str(args.repo))
        _, entries, histories = derive_range_provenance(
            repo=args.repo,
            base_rev=args.base,
            head_rev=args.head,
            allowed_signers_path=args.allowed_signers,
            brief_specs=args.brief_file,
            output_specs=args.output_file,
            evidence_dir=args.evidence_dir,
            log_object=args.log_object,
        )
        all_stanzas: list[str] = []
        for e in entries:
            for h in histories.get(e["commit"], [e]):
                all_stanzas.append(format_log_stanza(h))
        content = "\n\n".join(all_stanzas)
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
    """Stamp HEAD with writer author, trailer(s), and an SSH-signed writer-log note."""
    try:
        args = argparse.Namespace(**vars(args))
        args.repo = _VerifiedRepo(str(args.repo))
        if args.model not in VALID_MODELS:
            raise ProvenanceError(f"invalid model '{args.model}'")
        if not POS_INT_RE.match(str(args.round)):
            raise ProvenanceError(f"invalid round '{args.round}'")
        brief_sha256 = args.brief_sha256
        if args.brief_file:
            file_brief_sha = sha256_file(args.brief_file, "--brief-file")
            if brief_sha256 and brief_sha256 != "0" * 64 and brief_sha256 != file_brief_sha:
                raise ProvenanceError(
                    f"--brief-sha256 '{brief_sha256}' does not match --brief-file '{args.brief_file}' ({file_brief_sha})"
                )
            brief_sha256 = file_brief_sha
        if not HEX64_RE.match(brief_sha256):
            raise ProvenanceError(f"invalid brief-sha256 '{brief_sha256}'")
        if args.model == "agy":
            if not args.agy_diff_sha256 or not HEX64_RE.match(args.agy_diff_sha256):
                raise ProvenanceError("agy commits require --agy-diff-sha256 <64-hex>")
        elif args.agy_diff_sha256:
            raise ProvenanceError(f"{args.model} commits must not set --agy-diff-sha256")

        eff_signers = resolve_allowed_signers(args.repo, args.allowed_signers)
        check_allowed_signers_has_keys(eff_signers, "stamp-commit")
        signing_key, signer = resolve_signing_key_and_signer(args.repo, args.signing_key, args.signer)
        read_validated_writer_notes(args.repo, args.allowed_signers, target_commits=None)

        head_sha = resolve_commit(args.repo, "HEAD", "HEAD")
        start_sha = (
            resolve_commit(args.repo, args.start_sha, "start-sha")
            if args.start_sha
            else resolve_commit(args.repo, "HEAD~1", "HEAD~1")
        )
        proc = git_run(args.repo, ["show", "-s", "--format=%B", head_sha])
        msg = proc.stdout.rstrip("\n")
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

        amend_proc = canonical_git_run(
            args.repo, ["commit", "-q", "--amend", "--reset-author", "-F", "-"],
            input=new_msg,
            text=True,
            env=env,
        )
        if amend_proc.returncode != 0:
            raise ProvenanceError(f"git commit --amend failed: {amend_proc.stderr.strip()}")

        new_sha = resolve_commit(args.repo, "HEAD", "HEAD")
        existing_note = read_optional_note(args.repo, WRITER_NOTES_REF, new_sha)
        if existing_note is not None and existing_note.strip():
            raise ProvenanceError(
                f"commit {new_sha[:10]} already has a writer-log entry; the writer log is append-only"
            )

        end_sha = (
            resolve_commit(args.repo, args.end_sha, "end-sha")
            if args.end_sha
            else new_sha
        )
        entry = {
            "model": args.model,
            "round": str(args.round),
            "branch": args.branch,
            "brief-sha256": brief_sha256,
            "start-sha": start_sha,
            "end-sha": end_sha,
            "commit": new_sha,
            "source": "harness",
        }
        if args.model == "agy":
            entry["agy-diff-sha256"] = args.agy_diff_sha256
        signed_entry = sign_entry(entry, signing_key, signer)
        stanza = format_log_stanza(signed_entry)
        parsed = parse_log_stanza(stanza, f"stamp-commit on {new_sha[:10]}", eff_signers)
        inspect_commit(args.repo, new_sha, end_sha, {new_sha: [parsed]}, eff_signers)
        git_run(
            args.repo,
            ["notes", f"--ref={WRITER_NOTES_REF}", "add", "-m", stanza, new_sha],
        )
        print(new_sha)
        return 0
    except ProvenanceError as exc:
        print(f"writer-provenance: {exc}", file=sys.stderr)
        return 2


def cmd_backfill(args: argparse.Namespace) -> int:
    """Record an explicit SSH-signed backfilled writer-log entry (or append-only correction) for an unstamped commit."""
    try:
        args = argparse.Namespace(**vars(args))
        args.repo = _VerifiedRepo(str(args.repo))
        target_rev = args.commit or args.commit_pos
        if not target_rev:
            raise ProvenanceError("backfill requires --commit <commit> (or positional <commit>)")
        if args.commit and args.commit_pos and args.commit != args.commit_pos:
            raise ProvenanceError(f"conflicting commit arguments '{args.commit}' and '{args.commit_pos}'")

        if args.model not in VALID_MODELS:
            raise ProvenanceError(f"invalid model '{args.model}' (expected codex, kimi, agy or claude)")
        if not POS_INT_RE.match(str(args.round)):
            raise ProvenanceError(f"invalid round '{args.round}'")
        if not args.branch or not BRANCH_RE.match(args.branch) or ".." in args.branch:
            raise ProvenanceError(f"invalid branch '{args.branch}'")

        if not args.brief_file:
            raise ProvenanceError(
                "backfill requires --brief-file <path> so brief-sha256 is verified from the actual brief file"
            )
        if not args.output_file:
            raise ProvenanceError(
                "backfill requires --output-file <path> so output-sha256 is verified from the writer's raw output log"
            )
        brief_sha256 = sha256_file(args.brief_file, "--brief-file")
        output_sha256 = sha256_file(args.output_file, "--output-file")
        if args.brief_sha256:
            if not HEX64_RE.match(args.brief_sha256):
                raise ProvenanceError(f"invalid brief-sha256 '{args.brief_sha256}'")
            if args.brief_sha256 != brief_sha256:
                raise ProvenanceError(
                    f"--brief-sha256 '{args.brief_sha256}' does not match --brief-file '{args.brief_file}' ({brief_sha256})"
                )
        if args.output_sha256:
            if not HEX64_RE.match(args.output_sha256):
                raise ProvenanceError(f"invalid output-sha256 '{args.output_sha256}'")
            if args.output_sha256 != output_sha256:
                raise ProvenanceError(
                    f"--output-sha256 '{args.output_sha256}' does not match --output-file '{args.output_file}' ({output_sha256})"
                )

        if not args.evidence or not args.evidence.strip() or "\n" in args.evidence or "\r" in args.evidence:
            raise ProvenanceError("backfill requires a non-empty single-line --evidence description")
        if args.agy_diff_sha256:
            if args.model != "agy":
                raise ProvenanceError(f"{args.model} backfill must not set --agy-diff-sha256")
            if not HEX64_RE.match(args.agy_diff_sha256):
                raise ProvenanceError(f"invalid agy-diff-sha256 '{args.agy_diff_sha256}'")

        if bool(args.supersedes) != bool(args.reason):
            raise ProvenanceError("backfill correction requires both --supersedes <entry-id> and --reason <why>")
        if args.reason and (not args.reason.strip() or "\n" in args.reason or "\r" in args.reason):
            raise ProvenanceError("backfill --reason must be a non-empty single-line string")

        eff_signers = resolve_allowed_signers(args.repo, args.allowed_signers)
        check_allowed_signers_has_keys(eff_signers, "backfill")
        signing_key, signer = resolve_signing_key_and_signer(args.repo, args.signing_key, args.signer)

        commit_sha = resolve_commit(args.repo, target_rev, "commit")
        start_sha = resolve_commit(args.repo, args.start_sha, "start-sha")
        end_sha = resolve_commit(args.repo, args.end_sha, "end-sha")
        read_validated_writer_notes(args.repo, args.allowed_signers, target_commits=None)

        existing_note_text = read_optional_note(args.repo, WRITER_NOTES_REF, commit_sha)
        existing_history: list[dict[str, str]] = []
        existing_raw = ""
        if existing_note_text is not None and existing_note_text.strip():
            existing_raw = existing_note_text.rstrip("\n")
            parsed_map = parse_multi_stanza_log(
                existing_raw, f"existing {WRITER_NOTES_REF} on {commit_sha[:10]}", eff_signers
            )
            for c_sha, c_hist in parsed_map.items():
                if c_sha != commit_sha:
                    raise ProvenanceError(
                        f"existing {WRITER_NOTES_REF} on {commit_sha[:10]} records commit {c_hist[-1]['commit']}, "
                        f"not {commit_sha} (each commit has one authoritative writer history on its own note)"
                    )
            existing_history = parsed_map.get(commit_sha, [])

        if existing_history and not args.supersedes:
            latest_id = existing_history[-1]["entry-id"]
            raise ProvenanceError(
                f"commit {commit_sha[:10]} already has a writer-log entry ({latest_id[:10]}); "
                f"the writer log is append-only and cannot be overwritten — pass "
                f"--supersedes {latest_id} --reason <why> to append a signed correction"
            )
        if not existing_history and args.supersedes:
            raise ProvenanceError(
                f"commit {commit_sha[:10]} has no existing writer-log entry to supersede"
            )

        resolved_supersedes = ""
        if args.supersedes:
            sup = args.supersedes.strip()
            if not HEX_PREFIX_RE.match(sup):
                raise ProvenanceError(
                    f"invalid --supersedes '{sup}' (expected 64-hex entry-id or 10+ hex prefix)"
                )
            latest_id = existing_history[-1]["entry-id"]
            if not latest_id.startswith(sup):
                raise ProvenanceError(
                    f"--supersedes '{sup}' does not match the latest entry-id {latest_id} on {commit_sha[:10]}"
                )
            resolved_supersedes = latest_id

        # A backfill can never attribute a commit to a model that is also the note's reviewer
        # unless an earlier signed entry already did so.
        earlier_models = {h["model"] for h in existing_history}
        for rev_target in dict.fromkeys([commit_sha, end_sha]):
            for rev_model in get_review_note_reviewers(args.repo, rev_target):
                if rev_model == args.model and args.model not in earlier_models:
                    raise ProvenanceError(
                        f"backfill on {commit_sha[:10]} cannot attribute commit to '{args.model}', "
                        f"which is the reviewer on {rev_target[:10]}'s review note "
                        f"(unless an earlier signed entry already did so)"
                    )

        entry: dict[str, str] = {
            "model": args.model,
            "round": str(args.round),
            "branch": args.branch,
            "brief-sha256": brief_sha256,
            "output-sha256": output_sha256,
            "start-sha": start_sha,
            "end-sha": end_sha,
            "commit": commit_sha,
            "source": "backfill",
            "evidence": args.evidence.strip(),
        }
        if args.agy_diff_sha256:
            entry["agy-diff-sha256"] = args.agy_diff_sha256
        if resolved_supersedes:
            entry["supersedes"] = resolved_supersedes
            entry["reason"] = args.reason.strip()

        signed_entry = sign_entry(entry, signing_key, signer)
        stanza = format_log_stanza(signed_entry)
        combined_text = f"{existing_raw}\n\n{stanza}" if existing_raw else stanza
        parsed_chain = parse_multi_stanza_log(
            combined_text, f"backfill on {commit_sha[:10]}", eff_signers
        )
        inspect_commit(args.repo, commit_sha, end_sha, parsed_chain, eff_signers)

        git_run(
            args.repo,
            ["notes", f"--ref={WRITER_NOTES_REF}", "add", "-f", "-m", combined_text, commit_sha],
        )
        print(commit_sha)
        return 0
    except ProvenanceError as exc:
        print(f"writer-provenance: {exc}", file=sys.stderr)
        return 2


def list_notes_tree(repo: str, notes_rev: str) -> dict[str, str]:
    """Return `{annotated_commit_sha: note_blob_sha}` for a git notes commit, validating tree layout."""
    notes_sha = resolve_commit(repo, notes_rev, "notes-rev")
    proc = git_run(
        repo,
        ["ls-tree", "-r", "--format=%(objectmode) %(objecttype) %(objectname) %(path)", notes_sha],
    )
    entries: dict[str, str] = {}
    for line in proc.stdout.splitlines():
        if not line.strip():
            continue
        parts = line.split(" ", 3)
        if len(parts) != 4:
            raise ProvenanceError(f"writer-notes {notes_sha[:10]}: malformed ls-tree line '{line}'")
        mode, objtype, blob_sha, path = parts
        if mode != "100644" or objtype != "blob":
            raise ProvenanceError(
                f"writer-notes {notes_sha[:10]}: entry '{path}' has mode/type {mode} {objtype}, expected 100644 blob"
            )
        annotated_sha = path.replace("/", "")
        if not HEX40_RE.match(annotated_sha):
            raise ProvenanceError(
                f"writer-notes {notes_sha[:10]}: path '{path}' is not a 40-hex commit note"
            )
        if annotated_sha in entries:
            raise ProvenanceError(
                f"writer-notes {notes_sha[:10]}: duplicate note path for {annotated_sha}"
            )
        entries[annotated_sha] = blob_sha
    if not entries:
        raise ProvenanceError(f"writer-notes {notes_sha[:10]} has an empty notes tree")
    return entries


def split_raw_stanzas(text: str) -> list[str]:
    text_stripped = text.strip()
    if not text_stripped:
        return []
    raw_stanzas = re.split(r"\n\n+(?=model: )", text_stripped)
    if len(raw_stanzas) == 1:
        raw_stanzas = re.split(r"\n\n+", text_stripped)
    return [s.strip("\n") for s in raw_stanzas if s.strip()]


def _blob_has_valid_signed_note(
    annotated_sha: str,
    blob_text: str,
    eff_signers: str,
) -> bool:
    try:
        parse_and_verify_note_blob(annotated_sha, blob_text, eff_signers)
        return True
    except ProvenanceError:
        return False


_VALIDATED_STATE_CACHE: dict[tuple[str, str, str], dict[str, list[dict[str, str]]]] = {}


def batch_cat_objects(repo: str, oids: list[str]) -> dict[str, tuple[str, str]]:
    """Read multiple git objects in a single `git cat-file --batch` call.
    Returns `{oid: (objtype, text)}` for existing objects."""
    unique_oids = list(dict.fromkeys(oid for oid in oids if oid and oid != ZERO40))
    if not unique_oids:
        return {}
    stdin_bytes = "".join(f"{oid}\n" for oid in unique_oids).encode("utf-8")
    proc = canonical_git_run(
        repo, ["cat-file", "--batch"],
        input=stdin_bytes,
        text=False,
    )
    if proc.returncode != 0:
        err = proc.stderr.decode("utf-8", errors="replace").strip()
        raise ProvenanceError(f"git cat-file --batch failed ({proc.returncode}): {err}")
    buf = proc.stdout
    pos = 0
    n = len(buf)
    result: dict[str, tuple[str, str]] = {}
    for req_oid in unique_oids:
        nl = buf.find(b"\n", pos)
        if nl < 0:
            raise ProvenanceError("truncated git cat-file --batch header output")
        header = buf[pos:nl].decode("utf-8", errors="replace")
        pos = nl + 1
        if header.endswith(" missing"):
            continue
        parts = header.split(" ")
        if len(parts) < 3:
            raise ProvenanceError(f"malformed git cat-file --batch header '{header}'")
        resolved_sha, objtype, size_str = parts[0], parts[1], parts[2]
        size = int(size_str)
        if pos + size > n:
            raise ProvenanceError("truncated git cat-file --batch object body")
        content = buf[pos : pos + size].decode("utf-8", errors="replace")
        pos += size
        if pos < n and buf[pos : pos + 1] == b"\n":
            pos += 1
        result[req_oid] = (objtype, content)
        result[resolved_sha] = (objtype, content)
    return result


def _parse_raw_commit_object(raw_text: str) -> tuple[list[str], str, str, str, str, list[str]]:
    """Parse `(parents, an, ae, cn, ce, body_lines)` from a raw git commit object body."""
    header_part, _, body_part = raw_text.partition("\n\n")
    parents: list[str] = []
    an = ""
    ae = ""
    cn = ""
    ce = ""
    for ln in header_part.splitlines():
        if ln.startswith("parent "):
            parents.append(ln[7:].strip())
        elif ln.startswith("author "):
            m = re.match(r"^author (.*) <([^>]*)> [0-9]+ [-+][0-9]+$", ln)
            if m:
                an, ae = m.group(1), m.group(2)
        elif ln.startswith("committer "):
            m = re.match(r"^committer (.*) <([^>]*)> [0-9]+ [-+][0-9]+$", ln)
            if m:
                cn, ce = m.group(1), m.group(2)
    return parents, an, ae, cn, ce, body_part.splitlines()


def validate_writer_history_state(
    repo: str,
    notes_rev: str,
    allowed_signers_path: str | None = None,
) -> dict[str, list[dict[str, str]]]:
    """Predicate 1 (pure state validity): validate a writer-history state as a function of that
    state alone (notes tree layout, every note blob's syntax, every stanza's entry-id and SSH
    signature against authorized signers, single-commit note authority, and commit-level semantic
    rules for every annotated commit present in `repo`).

    Never derived from, cached by, or skipped because of an old ref, checkpoint, blob identity,
    or option. Cached in-process only by content hash of a state that was fully validated in this
    process.
    """
    eff_signers = resolve_allowed_signers(repo, allowed_signers_path)
    signers_hash = _signers_file_sha256(eff_signers)
    notes_sha = resolve_commit(repo, notes_rev, "notes-rev")
    entries = list_notes_tree(repo, notes_sha)
    sorted_items = sorted(entries.items())
    blob_map = batch_cat_objects(repo, [b for _, b in sorted_items])
    state_hasher = hashlib.sha256()
    validated_state: dict[str, list[dict[str, str]]] = {}
    commit_oids_to_fetch: list[str] = []
    blob_texts: list[tuple[str, str]] = []
    for annotated_sha, blob_sha in sorted_items:
        blob_info = blob_map.get(blob_sha)
        if blob_info is None or blob_info[0] != "blob":
            raise ProvenanceError(f"writer-notes {notes_sha[:10]}: missing note blob {blob_sha[:10]} for {annotated_sha[:10]}")
        blob_text = blob_info[1]
        state_hasher.update(f"{annotated_sha}:{blob_sha}:".encode("utf-8"))
        state_hasher.update(hashlib.sha256(blob_text.encode("utf-8")).digest())
        blob_texts.append((annotated_sha, blob_text))

    if len(blob_texts) > 1:
        with concurrent.futures.ThreadPoolExecutor(max_workers=min(8, len(blob_texts))) as pool:
            futs = [
                (annotated_sha, pool.submit(parse_and_verify_note_blob, annotated_sha, blob_text, eff_signers))
                for annotated_sha, blob_text in blob_texts
            ]
            for annotated_sha, fut in futs:
                history = fut.result()
                validated_state[annotated_sha] = history
                commit_oids_to_fetch.append(annotated_sha)
                for h in history:
                    commit_oids_to_fetch.append(h["start-sha"])
                    commit_oids_to_fetch.append(h["end-sha"])
    else:
        for annotated_sha, blob_text in blob_texts:
            history = parse_and_verify_note_blob(annotated_sha, blob_text, eff_signers)
            validated_state[annotated_sha] = history
            commit_oids_to_fetch.append(annotated_sha)
            for h in history:
                commit_oids_to_fetch.append(h["start-sha"])
                commit_oids_to_fetch.append(h["end-sha"])

    commit_map = batch_cat_objects(repo, commit_oids_to_fetch)
    for c_oid in sorted(set(commit_oids_to_fetch)):
        c_info = commit_map.get(c_oid)
        if c_info is not None and c_info[0] == "commit":
            state_hasher.update(f"commit:{c_oid}:".encode("utf-8"))
            state_hasher.update(hashlib.sha256(c_info[1].encode("utf-8")).digest())

    state_cache_key = (os.path.abspath(repo), state_hasher.hexdigest(), signers_hash)
    cached_state = _VALIDATED_STATE_CACHE.get(state_cache_key)
    if cached_state is not None:
        return {k: [dict(e) for e in v] for k, v in cached_state.items()}

    parents_map: dict[str, list[str]] = {}
    parsed_commits: dict[str, tuple[list[str], str, str, str, str, list[str]]] = {}
    for c_oid, (objtype, raw_text) in commit_map.items():
        if objtype == "commit":
            parsed = _parse_raw_commit_object(raw_text)
            parsed_commits[c_oid] = parsed
            parents_map[c_oid] = parsed[0]

    def is_anc_fast(anc: str, desc: str) -> bool:
        if anc == desc:
            return True
        visited: set[str] = set()
        stack = list(parents_map.get(desc, ()))
        while stack:
            cur = stack.pop()
            if cur == anc:
                return True
            if cur not in visited:
                visited.add(cur)
                stack.extend(parents_map.get(cur, ()))
        return is_ancestor(repo, anc, desc)

    for annotated_sha, history in sorted(validated_state.items()):
        if annotated_sha not in parsed_commits:
            continue
        parents, an, ae, cn, ce, body_lines = parsed_commits[annotated_sha]
        is_merge = len(parents) > 1
        if is_merge:
            is_clean_auto_merge(repo, annotated_sha, parents)
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
        has_commit_stamp = author_has_writer_tag or bool(writer_trailers) or bool(agy_trailers)
        if not cn or not ce or ce.lower().endswith("@writers.concrete") or "(writer)" in cn.lower():
            raise ProvenanceError(
                f"commit {annotated_sha[:10]} committer '{cn} <{ce}>' must be the owner's identity, not a writer identity"
            )
        head_sha = history[-1]["end-sha"]
        if head_sha not in parsed_commits:
            head_sha = resolve_commit(repo, head_sha, f"commit {annotated_sha[:10]} writer-log end-sha")
        for entry in history:
            if entry.get("source") == "backfill":
                if has_commit_stamp:
                    raise ProvenanceError(
                        f"commit {annotated_sha[:10]} carries a harness commit stamp and cannot use a backfilled writer-log entry"
                    )
                if entry["commit"] != annotated_sha:
                    raise ProvenanceError(
                        f"commit {annotated_sha[:10]} writer-log entry is for commit {entry['commit']}, not {annotated_sha}"
                    )
            else:
                if len(writer_trailers) == 0:
                    raise ProvenanceError(
                        f"commit {annotated_sha[:10]} has writer author or harness log entry but no 'Writer: <model>' trailer"
                    )
                if len(writer_trailers) > 1:
                    raise ProvenanceError(f"commit {annotated_sha[:10]} has multiple 'Writer:' trailers")
                model = writer_trailers[0]
                if model not in VALID_MODELS:
                    raise ProvenanceError(
                        f"commit {annotated_sha[:10]} has invalid 'Writer: {model}' trailer (expected codex, kimi, agy or claude)"
                    )
                expected_ae = f"{model}@writers.concrete"
                expected_an_choices = {
                    f"{MODEL_DISPLAY[model]} (writer)",
                    f"{model.capitalize()} (writer)",
                    f"{model} (writer)",
                }
                if ae != expected_ae or an not in expected_an_choices:
                    raise ProvenanceError(
                        f"commit {annotated_sha[:10]} trailer 'Writer: {model}' does not match author '{an} <{ae}>' "
                        f"(expected '{MODEL_DISPLAY[model]} (writer) <{expected_ae}>')"
                    )
                if model == "agy":
                    if len(agy_trailers) != 1:
                        raise ProvenanceError(
                            f"commit {annotated_sha[:10]} written by agy must have exactly one 'Agy-Diff-SHA256: <64-hex>' trailer"
                        )
                    if not HEX64_RE.match(agy_trailers[0]):
                        raise ProvenanceError(
                            f"commit {annotated_sha[:10]} has malformed 'Agy-Diff-SHA256: {agy_trailers[0]}' trailer"
                        )
                else:
                    if agy_trailers:
                        raise ProvenanceError(
                            f"commit {annotated_sha[:10]} written by {model} must not have an 'Agy-Diff-SHA256:' trailer"
                        )
                if entry["commit"] != annotated_sha:
                    raise ProvenanceError(
                        f"commit {annotated_sha[:10]} writer-log entry is for commit {entry['commit']}, not {annotated_sha}"
                    )
                if entry["model"] != model:
                    raise ProvenanceError(
                        f"commit {annotated_sha[:10]} writer-log model '{entry['model']}' does not match commit stamp '{model}'"
                    )
                if model == "agy" and entry.get("agy-diff-sha256") != agy_trailers[0]:
                    raise ProvenanceError(
                        f"commit {annotated_sha[:10]} writer-log agy-diff-sha256 '{entry.get('agy-diff-sha256')}' "
                        f"does not match commit trailer '{agy_trailers[0]}'"
                    )
            start_sha = entry["start-sha"]
            if start_sha not in parsed_commits:
                start_sha = resolve_commit(repo, start_sha, f"commit {annotated_sha[:10]} writer-log start-sha")
            end_sha = entry["end-sha"]
            if end_sha not in parsed_commits:
                end_sha = resolve_commit(repo, end_sha, f"commit {annotated_sha[:10]} writer-log end-sha")
            if start_sha == annotated_sha or not is_anc_fast(start_sha, annotated_sha):
                raise ProvenanceError(
                    f"commit {annotated_sha[:10]} writer-log start-sha {start_sha[:10]} is not a strict ancestor of {annotated_sha[:10]}"
                )
            if not is_anc_fast(annotated_sha, end_sha):
                raise ProvenanceError(
                    f"commit {annotated_sha[:10]} is not an ancestor-or-equal of its writer-log end-sha {end_sha[:10]}"
                )
            if not is_anc_fast(end_sha, head_sha):
                raise ProvenanceError(
                    f"commit {annotated_sha[:10]} writer-log end-sha {end_sha[:10]} is not an ancestor-or-equal of head {head_sha[:10]}"
                )

    _VALIDATED_STATE_CACHE[state_cache_key] = {k: [dict(e) for e in v] for k, v in validated_state.items()}
    return validated_state


def verify_writer_history_continuity(
    repo: str,
    notes_rev: str | None,
    old_notes_rev: str | None = None,
    extra_revs: list[str] | None = None,
    allowed_signers_path: str | None = None,
    target_commits: set[str] | None = None,
) -> None:
    """Predicate 2 (continuity): verify that `notes_rev` is a valid append-only extension and
    fast-forward descendant of `old_notes_rev`, all ancestor commits of `notes_rev`, all companion
    writer-notes refs (`WRITER_NOTES_REF`, `refs/notes/concrete-writer`,
    `refs/notes/remote/concrete-writer`), and any `extra_revs`."""
    eff_signers = resolve_allowed_signers(repo, allowed_signers_path)
    notes_sha: str | None = None
    new_entries: dict[str, str] = {}
    if notes_rev is not None:
        notes_sha = resolve_commit(repo, notes_rev, "notes-rev")
        new_entries = list_notes_tree(repo, notes_sha)

    old_rev_stripped = ""
    old_entries: dict[str, str] = {}
    if old_notes_rev is not None:
        old_rev_stripped = old_notes_rev.strip()
        if not old_rev_stripped:
            raise ProvenanceError(
                "old-notes-rev is empty (expected 40-hex creation sentinel 0000... or an existing notes commit)"
            )
        if old_rev_stripped != ZERO40:
            if not commit_exists(repo, old_rev_stripped):
                raise ProvenanceError(
                    f"old-notes-rev '{old_rev_stripped}' is not available locally "
                    f"(every nonzero old notes SHA must resolve and be compared)"
                )
            old_entries = list_notes_tree(repo, old_rev_stripped)
            removed = sorted(set(old_entries.keys()) - set(new_entries.keys()))
            if removed:
                rem_str = ", ".join(c[:10] for c in removed)
                raise ProvenanceError(
                    f"writer-notes push deletes existing writer-log entry for commit(s): {rem_str}"
                )
            old_new_blobs = batch_cat_objects(
                repo, list(old_entries.values()) + [new_entries[c] for c in old_entries]
            )
            for annotated_sha, old_blob_sha in sorted(old_entries.items()):
                new_blob_sha = new_entries.get(annotated_sha)
                assert new_blob_sha is not None
                new_text = old_new_blobs[new_blob_sha][1]
                old_text = old_new_blobs[old_blob_sha][1]
                new_raw_stanzas = split_raw_stanzas(new_text)
                old_raw_stanzas = split_raw_stanzas(old_text)
                if (
                    len(new_raw_stanzas) < len(old_raw_stanzas)
                    or new_raw_stanzas[: len(old_raw_stanzas)] != old_raw_stanzas
                ):
                    raise ProvenanceError(
                        f"writer-notes push rewrites or drops existing writer-log entry on {annotated_sha[:10]} "
                        f"(the writer log is append-only; corrections must append a signed superseding entry)"
                    )

    combined_extra = list(extra_revs or [])
    if old_rev_stripped and old_rev_stripped != ZERO40 and old_rev_stripped not in combined_extra:
        combined_extra.append(old_rev_stripped)

    rev_roots: list[str] = []
    if notes_sha is not None:
        rev_roots.append(notes_sha)
    companion = collect_prior_writer_checkpoints(
        repo,
        extra_revs=combined_extra,
        check_refs=[
            WRITER_NOTES_REF,
            "refs/notes/concrete-writer",
            "refs/notes/remote/concrete-writer",
        ],
    )
    for sha in companion:
        if sha not in rev_roots:
            rev_roots.append(sha)

    if rev_roots:
        if notes_sha is None:
            prior_desc = ", ".join(p[:10] for p in rev_roots)
            raise ProvenanceError(
                f"local {WRITER_NOTES_REF} is missing, but prior writer-log history was "
                f"recorded at {prior_desc} (refusing deleted or missing local writer-notes ref)"
            )

        log_proc = git_run(
            repo,
            ["log", "--raw", "--root", "-m", "--no-abbrev", "--format=COMMIT %H", *rev_roots],
            check=True,
        )
        if not log_proc.stdout.strip():
            raise ProvenanceError("git log returned empty history for writer-notes continuity check")

        new_raw_cache: dict[str, list[str]] = {}
        checked_pairs: set[tuple[str, str]] = set()
        seen_commits: set[str] = set()

        blocks: list[tuple[str, list[str]]] = []
        cur_anc_sha = ""
        cur_lines: list[str] = []
        for raw_line in log_proc.stdout.splitlines():
            line = raw_line.strip()
            if not line:
                continue
            if line.startswith("COMMIT "):
                if cur_anc_sha:
                    blocks.append((cur_anc_sha, cur_lines))
                cur_anc_sha = line[7:].strip()
                if not HEX40_RE.match(cur_anc_sha):
                    raise ProvenanceError(f"malformed COMMIT line in git log output: '{line}'")
                seen_commits.add(cur_anc_sha)
                cur_lines = []
                continue
            if line.startswith(":") and "\t" in line:
                cur_lines.append(line)
        if cur_anc_sha:
            blocks.append((cur_anc_sha, cur_lines))

        for root_sha in rev_roots:
            if root_sha not in seen_commits:
                raise ProvenanceError(
                    f"git log output did not include required writer-notes commit {root_sha[:10]}"
                )

        ordered_blocks = (
            [b for b in blocks if b[0] == notes_sha]
            + [b for b in blocks if b[0] != notes_sha]
        )

        blobs_needed: list[str] = []
        for cur_anc_sha, diff_lines in ordered_blocks:
            for line in diff_lines:
                meta, path = line.split("\t", 1)
                c = path.replace("/", "").strip()
                if len(c) != 40 or not all(ch in "0123456789abcdef" for ch in c):
                    continue
                if target_commits is not None and c not in target_commits:
                    continue
                parts = meta.split()
                if len(parts) < 5:
                    continue
                old_bsha, new_bsha = parts[2], parts[3]
                if old_bsha != ZERO40:
                    blobs_needed.append(old_bsha)
                if cur_anc_sha != notes_sha and new_bsha != ZERO40 and new_bsha != old_bsha:
                    blobs_needed.append(new_bsha)
                cur_blob_sha = new_entries.get(c)
                if cur_blob_sha is not None:
                    blobs_needed.append(cur_blob_sha)

        blob_batch = batch_cat_objects(repo, blobs_needed)

        def get_blob_text(bsha: str) -> str:
            info = blob_batch.get(bsha)
            if info is not None:
                return info[1]
            return git_run(repo, ["cat-file", "blob", bsha], check=True).stdout

        for cur_anc_sha, diff_lines in ordered_blocks:
            for line in diff_lines:
                meta, path = line.split("\t", 1)
                c = path.replace("/", "").strip()
                if len(c) != 40 or not all(ch in "0123456789abcdef" for ch in c):
                    continue
                if target_commits is not None and c not in target_commits:
                    continue
                parts = meta.split()
                if len(parts) < 5:
                    continue
                old_bsha, new_bsha = parts[2], parts[3]
                candidate_blobs: list[str] = []
                if old_bsha != ZERO40:
                    candidate_blobs.append(old_bsha)
                if cur_anc_sha != notes_sha and new_bsha != ZERO40 and new_bsha != old_bsha:
                    candidate_blobs.append(new_bsha)
                cur_blob_sha = new_entries.get(c)
                for anc_blob_sha in candidate_blobs:
                    pair = (c, anc_blob_sha)
                    if pair in checked_pairs:
                        continue
                    checked_pairs.add(pair)
                    anc_text = get_blob_text(anc_blob_sha)
                    if cur_blob_sha is not None:
                        if c not in new_raw_cache:
                            new_raw_cache[c] = split_raw_stanzas(get_blob_text(cur_blob_sha))
                        new_raw = new_raw_cache[c]
                        anc_raw = split_raw_stanzas(anc_text)
                        if len(new_raw) >= len(anc_raw) and new_raw[: len(anc_raw)] == anc_raw:
                            continue
                    if not _blob_has_valid_signed_note(c, anc_text, eff_signers):
                        continue
                    if cur_blob_sha is None:
                        raise ProvenanceError(
                            f"writer-notes push deletes existing writer-log entry for commit(s): {c[:10]} "
                            f"(present in writer-notes commit {cur_anc_sha[:10]})"
                        )
                    raise ProvenanceError(
                        f"writer-notes push rewrites or drops existing writer-log entry on {c[:10]} "
                        f"from writer-notes commit {cur_anc_sha[:10]} "
                        f"(the writer log is append-only; corrections must append a signed superseding entry)"
                    )

    if old_rev_stripped and old_rev_stripped != ZERO40:
        assert notes_sha is not None
        old_sha = resolve_commit(repo, old_rev_stripped, "old-notes-rev")
        if not is_ancestor(repo, old_sha, notes_sha):
            raise ProvenanceError(
                f"writer-notes push is non-fast-forward ({old_sha[:10]} is not an ancestor of {notes_sha[:10]}): "
                f"refs/notes/concrete-writer history is append-only and may not be rewound or rewritten"
            )


def read_validated_writer_notes(
    repo: str,
    allowed_signers_path: str | None = None,
    target_commits: set[str] | None = None,
) -> dict[str, list[dict[str, str]]]:
    """Validate the current writer-notes state (Predicate 1) and its continuity against all
    ancestors and companion checkpoints (Predicate 2), in that order, before any reader or
    writer operation uses `WRITER_NOTES_REF`."""
    cur_sha = resolve_optional_ref(repo, WRITER_NOTES_REF)
    state: dict[str, list[dict[str, str]]] = {}
    if cur_sha is not None:
        state = validate_writer_history_state(repo, cur_sha, allowed_signers_path=allowed_signers_path)
    verify_writer_history_continuity(
        repo=repo,
        notes_rev=cur_sha,
        allowed_signers_path=allowed_signers_path,
        target_commits=target_commits,
    )
    return state


def validate_notes_ref(
    repo: str,
    notes_rev: str,
    old_notes_rev: str | None = None,
    allowed_signers_path: str | None = None,
    allow_missing_targets: bool = False,
) -> int:
    """Validate `notes_rev` by running Predicate 1 (`validate_writer_history_state`) first and
    Predicate 2 (`verify_writer_history_continuity`) second, in that order."""
    del allow_missing_targets
    validated_state = validate_writer_history_state(
        repo=repo,
        notes_rev=notes_rev,
        allowed_signers_path=allowed_signers_path,
    )
    verify_writer_history_continuity(
        repo=repo,
        notes_rev=notes_rev,
        old_notes_rev=old_notes_rev,
        allowed_signers_path=allowed_signers_path,
    )
    return len(validated_state)


def collect_prior_writer_checkpoints(
    repo: str,
    extra_revs: list[str] | None = None,
    check_refs: list[str] | None = None,
) -> list[str]:
    """Return deduplicated 40-hex commit SHAs of all recorded writer-notes checkpoints in `repo`."""
    priors: list[str] = []
    for raw_rev in extra_revs or []:
        if raw_rev is None:
            continue
        rev = raw_rev.strip()
        if not rev or rev == ZERO40:
            continue
        if not commit_exists(repo, rev):
            raise ProvenanceError(
                f"old-notes-rev '{rev}' is not available locally "
                f"(every nonzero old notes SHA must resolve and be compared)"
            )
        sha = resolve_commit(repo, rev, "old-notes-rev")
        if sha not in priors:
            priors.append(sha)

    refs = check_refs or [
        WRITER_NOTES_REF,
        "refs/notes/concrete-writer",
        "refs/notes/remote/concrete-writer",
    ]
    for ref in refs:
        if not ref:
            continue
        sha = resolve_optional_ref(repo, ref)
        if sha and sha not in priors:
            priors.append(sha)
    return priors


def fetch_notes_ref(
    repo: str,
    remote: str = "origin",
    remote_ref: str | None = None,
    target_ref: str | None = None,
    old_notes_revs: list[str] | None = None,
    update_local: bool = True,
    allowed_signers_path: str | None = None,
    no_fetch: bool = False,
) -> str | None:
    """Fetch `remote_ref` from `remote` (or validate local `target_ref` when `no_fetch=True`),
    requiring Predicate 1 (`validate_writer_history_state`) first and Predicate 2
    (`verify_writer_history_continuity`) second, in that order, before advancing any checkpoint
    or returning a writer-notes state.

    Checkpoints are never deleted or downgraded by a reader or rebuild. A missing remote or local
    ref is refused whenever any prior writer-log history was recorded.
    """
    eff_remote_ref = (remote_ref or WRITER_NOTES_REF).strip()
    eff_target_ref = (target_ref or WRITER_NOTES_REF).strip()
    local_cand_sha: str | None = None
    if no_fetch:
        local_cand_sha = resolve_optional_ref(repo, eff_target_ref)
        if local_cand_sha is None and eff_target_ref != "refs/notes/concrete-writer":
            local_cand_sha = resolve_optional_ref(repo, "refs/notes/concrete-writer")
    check_refs = [
        eff_target_ref,
        WRITER_NOTES_REF,
        "refs/notes/concrete-writer",
        "refs/notes/remote/concrete-writer",
    ]
    priors = collect_prior_writer_checkpoints(
        repo, extra_revs=old_notes_revs, check_refs=check_refs
    )
    # Monotone checkpoint ratchet: only a state that satisfies BOTH Predicate 1 (full state
    # validity) and Predicate 2 (continuity + fast-forward ancestry of all recorded checkpoints),
    # in that order, can ever advance checkpoint refs.
    if priors:
        for cand_prior in priors:
            try:
                validate_writer_history_state(
                    repo=repo,
                    notes_rev=cand_prior,
                    allowed_signers_path=allowed_signers_path,
                )
                if not all(is_ancestor(repo, other_prior, cand_prior) for other_prior in priors):
                    continue
                verify_writer_history_continuity(
                    repo=repo,
                    notes_rev=cand_prior,
                    extra_revs=priors,
                    allowed_signers_path=allowed_signers_path,
                )
            except ProvenanceError:
                continue
            if resolve_optional_ref(repo, eff_target_ref) != cand_prior:
                git_run(repo, ["update-ref", eff_target_ref, cand_prior])
            if resolve_optional_ref(repo, "refs/notes/remote/concrete-writer") != cand_prior:
                git_run(repo, ["update-ref", "refs/notes/remote/concrete-writer", cand_prior])
            if (
                update_local
                and eff_target_ref != "refs/notes/concrete-writer"
                and resolve_optional_ref(repo, "refs/notes/concrete-writer") != cand_prior
            ):
                git_run(repo, ["update-ref", "refs/notes/concrete-writer", cand_prior])
            break

    if no_fetch:
        if local_cand_sha is None:
            if priors:
                prior_desc = ", ".join(p[:10] for p in priors)
                raise ProvenanceError(
                    f"local {eff_target_ref} is missing, but prior writer-log history was "
                    f"recorded at {prior_desc} (refusing deleted or missing local writer-notes ref)"
                )
            return None
        validate_writer_history_state(
            repo=repo,
            notes_rev=local_cand_sha,
            allowed_signers_path=allowed_signers_path,
        )
        if priors:
            for prior in priors:
                verify_writer_history_continuity(
                    repo=repo,
                    notes_rev=local_cand_sha,
                    old_notes_rev=prior,
                    extra_revs=priors,
                    allowed_signers_path=allowed_signers_path,
                )
        else:
            verify_writer_history_continuity(
                repo=repo,
                notes_rev=local_cand_sha,
                old_notes_rev=ZERO40,
                allowed_signers_path=allowed_signers_path,
            )
        git_run(repo, ["update-ref", eff_target_ref, local_cand_sha])
        git_run(repo, ["update-ref", "refs/notes/remote/concrete-writer", local_cand_sha])
        if update_local and eff_target_ref != "refs/notes/concrete-writer":
            git_run(repo, ["update-ref", "refs/notes/concrete-writer", local_cand_sha])
        return local_cand_sha

    tmp_fetch_ref = f"refs/notes/remote/concrete-writer-fetch-{os.getpid()}"
    git_run(repo, ["update-ref", "-d", tmp_fetch_ref], check=False)
    try:
        fproc = git_run(
            repo,
            ["fetch", "-q", remote, f"+{eff_remote_ref}:{tmp_fetch_ref}"],
            check=False,
        )
        if fproc.returncode != 0:
            err_txt = (fproc.stderr or fproc.stdout or "").strip()
            if "couldn't find remote ref" in err_txt:
                if priors:
                    prior_desc = ", ".join(p[:10] for p in priors)
                    raise ProvenanceError(
                        f"remote {remote} has no {eff_remote_ref}, but prior writer-log history was "
                        f"recorded at {prior_desc} (refusing deleted or missing remote writer-notes ref)"
                    )
                return None
            raise FetchTransportError(
                f"could not fetch {eff_remote_ref} from {remote}: {err_txt}"
            )

        fetched_sha = resolve_commit(repo, tmp_fetch_ref, "fetched writer-notes")
        validate_writer_history_state(
            repo=repo,
            notes_rev=fetched_sha,
            allowed_signers_path=allowed_signers_path,
        )
        if priors:
            for prior in priors:
                verify_writer_history_continuity(
                    repo=repo,
                    notes_rev=fetched_sha,
                    old_notes_rev=prior,
                    extra_revs=priors,
                    allowed_signers_path=allowed_signers_path,
                )
        else:
            verify_writer_history_continuity(
                repo=repo,
                notes_rev=fetched_sha,
                old_notes_rev=ZERO40,
                allowed_signers_path=allowed_signers_path,
            )

        git_run(repo, ["update-ref", eff_target_ref, fetched_sha])
        git_run(repo, ["update-ref", "refs/notes/remote/concrete-writer", fetched_sha])
        if update_local and eff_target_ref != "refs/notes/concrete-writer":
            git_run(repo, ["update-ref", "refs/notes/concrete-writer", fetched_sha])
        return fetched_sha
    finally:
        git_run(repo, ["update-ref", "-d", tmp_fetch_ref], check=False)


def sync_notes_refs(
    repo: str,
    from_rev: str | None = None,
    remote: str | None = None,
    allowed_signers_path: str | None = None,
) -> str | None:
    """Merge `from_rev` (or `remote`'s `WRITER_NOTES_REF`) into `WRITER_NOTES_REF` using strict append-only
    stanza prefix continuity, refusing any rollback, deletion, or conflicting rewrite."""
    tmp_fetch_ref = "refs/notes/writer-sync-fetch-tmp"
    fetched_from_remote = False
    try:
        prev_remote_sha = resolve_optional_ref(repo, "refs/notes/remote/concrete-writer")

        if from_rev is None:
            if not remote:
                raise ProvenanceError("sync-notes requires --from-rev or --remote")
            git_run(repo, ["update-ref", "-d", tmp_fetch_ref], check=False)
            fproc = git_run(repo, ["fetch", "-q", remote, f"+{WRITER_NOTES_REF}:{tmp_fetch_ref}"], check=False)
            if fproc.returncode != 0:
                err_txt = (fproc.stderr or fproc.stdout or "").strip()
                if "couldn't find remote ref" in err_txt:
                    if prev_remote_sha:
                        raise ProvenanceError(
                            f"remote {remote} deleted {WRITER_NOTES_REF} (previously at {prev_remote_sha[:10]})"
                        )
                    lsha = resolve_optional_ref(repo, WRITER_NOTES_REF)
                    if lsha is not None:
                        validate_notes_ref(repo, lsha, allowed_signers_path=allowed_signers_path)
                        return lsha
                    priors = collect_prior_writer_checkpoints(repo)
                    if priors:
                        prior_desc = ", ".join(p[:10] for p in priors)
                        raise ProvenanceError(
                            f"local {WRITER_NOTES_REF} is missing, but prior writer-log history was "
                            f"recorded at {prior_desc} (refusing deleted or missing writer-notes ref)"
                        )
                    return None
                raise ProvenanceError(f"could not fetch {WRITER_NOTES_REF} from {remote}: {err_txt}")
            from_rev = tmp_fetch_ref
            fetched_from_remote = True

        remote_sha = resolve_commit(repo, from_rev, "from-rev")
        validate_writer_history_state(repo, remote_sha, allowed_signers_path=allowed_signers_path)
        local_sha = resolve_optional_ref(repo, WRITER_NOTES_REF)
        if local_sha is not None:
            validate_writer_history_state(repo, local_sha, allowed_signers_path=allowed_signers_path)

        if prev_remote_sha and remote_sha != prev_remote_sha and not is_ancestor(repo, prev_remote_sha, remote_sha):
            validate_notes_ref(
                repo,
                remote_sha,
                old_notes_rev=prev_remote_sha,
                allowed_signers_path=allowed_signers_path,
            )

        if local_sha is None:
            validate_notes_ref(
                repo,
                remote_sha,
                old_notes_rev=prev_remote_sha or ZERO40,
                allowed_signers_path=allowed_signers_path,
            )
            git_run(repo, ["update-ref", WRITER_NOTES_REF, remote_sha])
            if fetched_from_remote:
                git_run(repo, ["update-ref", "refs/notes/remote/concrete-writer", remote_sha])
            return remote_sha

        if local_sha == remote_sha:
            validate_notes_ref(
                repo,
                remote_sha,
                old_notes_rev=prev_remote_sha or local_sha,
                allowed_signers_path=allowed_signers_path,
            )
            if fetched_from_remote:
                git_run(repo, ["update-ref", "refs/notes/remote/concrete-writer", remote_sha])
            return local_sha

        if is_ancestor(repo, local_sha, remote_sha):
            if prev_remote_sha:
                validate_notes_ref(
                    repo,
                    remote_sha,
                    old_notes_rev=prev_remote_sha,
                    allowed_signers_path=allowed_signers_path,
                )
            validate_notes_ref(
                repo,
                remote_sha,
                old_notes_rev=local_sha,
                allowed_signers_path=allowed_signers_path,
            )
            git_run(repo, ["update-ref", WRITER_NOTES_REF, remote_sha])
            if fetched_from_remote:
                git_run(repo, ["update-ref", "refs/notes/remote/concrete-writer", remote_sha])
            return remote_sha

        if is_ancestor(repo, remote_sha, local_sha):
            if not fetched_from_remote and remote_sha != local_sha:
                validate_notes_ref(
                    repo,
                    remote_sha,
                    old_notes_rev=local_sha,
                    allowed_signers_path=allowed_signers_path,
                )
            if prev_remote_sha:
                validate_notes_ref(
                    repo,
                    local_sha,
                    old_notes_rev=prev_remote_sha,
                    allowed_signers_path=allowed_signers_path,
                )
            validate_notes_ref(
                repo,
                local_sha,
                old_notes_rev=remote_sha,
                allowed_signers_path=allowed_signers_path,
            )
            if fetched_from_remote:
                git_run(repo, ["update-ref", "refs/notes/remote/concrete-writer", remote_sha])
            return local_sha

        # Diverged notes commits: both states are already validated by Predicate 1 above; now
        # verify neither side dropped or rewrote any ancestor history, and merge per-commit stanza
        # lists by strict prefix extension.
        local_entries = list_notes_tree(repo, local_sha)
        remote_entries = list_notes_tree(repo, remote_sha)
        verify_writer_history_continuity(
            repo, local_sha, extra_revs=[local_sha], allowed_signers_path=allowed_signers_path
        )
        verify_writer_history_continuity(
            repo, remote_sha, extra_revs=[remote_sha], allowed_signers_path=allowed_signers_path
        )

        merged_entries: dict[str, str] = {}
        for c in sorted(set(local_entries.keys()) | set(remote_entries.keys())):
            l_blob = local_entries.get(c)
            r_blob = remote_entries.get(c)
            if l_blob is not None and r_blob is None:
                merged_entries[c] = l_blob
            elif r_blob is not None and l_blob is None:
                merged_entries[c] = r_blob
            else:
                assert l_blob is not None and r_blob is not None
                if l_blob == r_blob:
                    merged_entries[c] = l_blob
                else:
                    l_raw = split_raw_stanzas(git_run(repo, ["cat-file", "blob", l_blob]).stdout)
                    r_raw = split_raw_stanzas(git_run(repo, ["cat-file", "blob", r_blob]).stdout)
                    if len(l_raw) <= len(r_raw) and r_raw[: len(l_raw)] == l_raw:
                        merged_entries[c] = r_blob
                    elif len(r_raw) < len(l_raw) and l_raw[: len(r_raw)] == r_raw:
                        merged_entries[c] = l_blob
                    else:
                        raise ProvenanceError(
                            f"writer-notes sync conflict on {c[:10]} between {local_sha[:10]} and "
                            f"{remote_sha[:10]} (rewrites or drops existing writer-log entry)"
                        )

        mktree_lines = [f"100644 blob {bsha}\t{c}\n" for c, bsha in sorted(merged_entries.items())]
        tproc = canonical_git_run(
            repo, ["mktree"],
            input="".join(mktree_lines),
            text=True,
        )
        if tproc.returncode != 0 or not tproc.stdout.strip():
            raise ProvenanceError(f"git mktree failed during sync-notes: {tproc.stderr.strip()}")
        tree_sha = tproc.stdout.strip()
        cproc = git_run(
            repo,
            [
                "commit-tree",
                tree_sha,
                "-p",
                local_sha,
                "-p",
                remote_sha,
                "-m",
                "Merge append-only writer-provenance notes",
            ],
        )
        merged_sha = cproc.stdout.strip()
        validate_writer_history_state(repo, merged_sha, allowed_signers_path=allowed_signers_path)
        if prev_remote_sha:
            verify_writer_history_continuity(
                repo,
                merged_sha,
                old_notes_rev=prev_remote_sha,
                allowed_signers_path=allowed_signers_path,
            )
        verify_writer_history_continuity(
            repo, merged_sha, old_notes_rev=local_sha, allowed_signers_path=allowed_signers_path
        )
        verify_writer_history_continuity(
            repo, merged_sha, old_notes_rev=remote_sha, allowed_signers_path=allowed_signers_path
        )
        git_run(repo, ["update-ref", WRITER_NOTES_REF, merged_sha])
        if fetched_from_remote:
            git_run(repo, ["update-ref", "refs/notes/remote/concrete-writer", remote_sha])
        return merged_sha
    finally:
        if fetched_from_remote:
            git_run(repo, ["update-ref", "-d", tmp_fetch_ref], check=False)


def cmd_validate_notes(args: argparse.Namespace) -> int:
    try:
        args = argparse.Namespace(**vars(args))
        args.repo = _VerifiedRepo(str(args.repo))
        n = validate_notes_ref(
            repo=args.repo,
            notes_rev=args.notes_rev,
            old_notes_rev=args.old_notes_rev,
            allowed_signers_path=args.allowed_signers,
            allow_missing_targets=getattr(args, "allow_missing_targets", False),
        )
        print(f"validated {n} writer-log note(s)")
        return 0
    except ProvenanceError as exc:
        print(f"writer-provenance: {exc}", file=sys.stderr)
        return 1


def cmd_fetch_notes(args: argparse.Namespace) -> int:
    try:
        args = argparse.Namespace(**vars(args))
        args.repo = _VerifiedRepo(str(args.repo))
        sha = fetch_notes_ref(
            repo=args.repo,
            remote=args.remote,
            remote_ref=args.remote_ref,
            target_ref=args.target_ref,
            old_notes_revs=args.old_notes_rev,
            update_local=not getattr(args, "no_update_local", False),
            allowed_signers_path=args.allowed_signers,
            no_fetch=getattr(args, "no_fetch", False),
        )
        if sha:
            print(sha)
        return 0
    except FetchTransportError as exc:
        print(f"{exc}", file=sys.stderr)
        return 2
    except ProvenanceError as exc:
        print(f"writer-provenance: {exc}", file=sys.stderr)
        return 1


def cmd_sync_notes(args: argparse.Namespace) -> int:
    try:
        args = argparse.Namespace(**vars(args))
        args.repo = _VerifiedRepo(str(args.repo))
        sha = sync_notes_refs(
            repo=args.repo,
            from_rev=args.from_rev,
            remote=args.remote,
            allowed_signers_path=args.allowed_signers,
        )
        if sha:
            print(sha)
        return 0
    except ProvenanceError as exc:
        print(f"writer-provenance: {exc}", file=sys.stderr)
        return 1


def cmd_publish_notes(args: argparse.Namespace) -> int:
    try:
        args = argparse.Namespace(**vars(args))
        args.repo = _VerifiedRepo(str(args.repo))
        sha = sync_notes_refs(
            repo=args.repo,
            from_rev=None,
            remote=args.remote,
            allowed_signers_path=args.allowed_signers,
        )
        if not sha:
            raise ProvenanceError(f"no {WRITER_NOTES_REF} ref to publish")
        env = os.environ.copy()
        env.pop("CONCRETE_SKIP_GATES", None)
        pproc = canonical_git_run(
            args.repo, ["push", "-q", args.remote, f"{sha}:{WRITER_NOTES_REF}"],
            text=True,
            env=env,
        )
        if pproc.returncode != 0:
            err = (pproc.stderr or pproc.stdout or "").strip()
            raise ProvenanceError(f"failed to publish {WRITER_NOTES_REF} to {args.remote}: {err}")
        git_run(args.repo, ["update-ref", "refs/notes/remote/concrete-writer", sha])
        print(sha)
        return 0
    except ProvenanceError as exc:
        print(f"writer-provenance: {exc}", file=sys.stderr)
        return 1


def add_common_reader_args(p: argparse.ArgumentParser) -> None:
    p.add_argument("--allowed-signers", help="Path to allowed_writer_log_signers file")
    p.add_argument(
        "--brief-file",
        action="append",
        help="Brief file path (or <commit>=<path>) to re-check against backfill brief-sha256",
    )
    p.add_argument(
        "--output-file",
        action="append",
        help="Raw writer output file path (or <commit>=<path>) to re-check against backfill output-sha256",
    )
    p.add_argument(
        "--evidence-dir",
        help="Directory holding <commit>.brief and <commit>.output files for backfill verification",
    )


def main() -> int:
    parser = argparse.ArgumentParser(description="Concrete writer-provenance reader and log manager.")
    sub = parser.add_subparsers(dest="cmd", required=True)

    p_derive = sub.add_parser("derive", help="Derive writer provenance for base..head")
    p_derive.add_argument("--repo", default=".", help="Git repository path")
    p_derive.add_argument("--base", required=True, help="Base commit")
    p_derive.add_argument("--head", required=True, help="Head commit")
    p_derive.add_argument("--assert-writer", help="Optional caller-supplied writer assertion to cross-check")
    p_derive.add_argument("--reviewer", help="Optional reviewer model to check against active and historical writers")
    p_derive.add_argument("--log-object", help="Optional multi-stanza writer-log blob or file")
    p_derive.add_argument("--report", action="store_true", help="Report both harness and backfilled entries")
    add_common_reader_args(p_derive)

    p_report = sub.add_parser("report", help="Report derived writers, harness entries, backfilled entries, and history")
    p_report.add_argument("--repo", default=".", help="Git repository path")
    p_report.add_argument("--base", required=True, help="Base commit")
    p_report.add_argument("--head", required=True, help="Head commit")
    p_report.add_argument("--log-object", help="Optional multi-stanza writer-log blob or file")
    add_common_reader_args(p_report)

    p_verify = sub.add_parser("verify", help="Verify note writer and reviewer independence against base..head")
    p_verify.add_argument("--repo", default=".", help="Git repository path")
    p_verify.add_argument("--base", required=True, help="Base commit")
    p_verify.add_argument("--head", required=True, help="Head commit")
    p_verify.add_argument("--claimed-writer", required=True, help="Writer field from review note")
    p_verify.add_argument("--reviewer", required=True, help="Reviewer field from review note")
    p_verify.add_argument("--log-object", help="Optional multi-stanza writer-log blob or file")
    add_common_reader_args(p_verify)

    p_export = sub.add_parser("export-log", help="Export canonical multi-stanza writer log for base..head")
    p_export.add_argument("--repo", default=".", help="Git repository path")
    p_export.add_argument("--base", required=True, help="Base commit")
    p_export.add_argument("--head", required=True, help="Head commit")
    p_export.add_argument("--log-object", help="Optional multi-stanza writer-log blob or file")
    p_export.add_argument("--write-blob", action="store_true", help="Write blob to git object store and print SHA")
    add_common_reader_args(p_export)

    p_stamp = sub.add_parser("stamp-commit", help="Stamp HEAD with writer author, trailer(s), and signed writer-log note")
    p_stamp.add_argument("--repo", default=".", help="Git repository path")
    p_stamp.add_argument("--model", required=True, help="Writer model (codex|kimi|agy|claude)")
    p_stamp.add_argument("--round", default="1", help="Writer round number (default: 1)")
    p_stamp.add_argument("--branch", default="wip/branch", help="Branch name")
    p_stamp.add_argument("--brief-sha256", default="0" * 64, help="SHA-256 of the writer brief")
    p_stamp.add_argument("--brief-file", help="Optional path to the writer brief file")
    p_stamp.add_argument("--start-sha", help="Start SHA of the writer round (default: HEAD~1)")
    p_stamp.add_argument("--end-sha", help="End SHA of the writer round (default: amended HEAD)")
    p_stamp.add_argument("--agy-diff-sha256", help="SHA-256 of the transported diff (required when model=agy)")
    p_stamp.add_argument("--signing-key", help="SSH private key path for signing the writer-log entry")
    p_stamp.add_argument("--signer", help="Authorized signer principal (default: from key or orchestrator@concrete)")
    p_stamp.add_argument("--allowed-signers", help="Path to allowed_writer_log_signers file")

    for sub_name in ("backfill", "correct"):
        p_bf = sub.add_parser(
            sub_name,
            help="Record an explicit signed backfilled writer-log entry (or append-only correction) for an unstamped commit",
        )
        p_bf.add_argument("commit_pos", nargs="?", help="Unstamped commit to backfill")
        p_bf.add_argument("--repo", default=".", help="Git repository path")
        p_bf.add_argument("--commit", help="Unstamped commit to backfill")
        p_bf.add_argument("--model", required=True, help="Writer model (codex|kimi|agy|claude)")
        p_bf.add_argument("--round", required=True, help="Writer round number")
        p_bf.add_argument("--branch", required=True, help="Branch name")
        p_bf.add_argument("--brief-file", help="Path to the writer brief file (hashed and verified)")
        p_bf.add_argument(
            "--output-file",
            dest="output_file",
            help="Path to the writer's raw output log file (hashed and verified)",
        )
        p_bf.add_argument("--brief-sha256", help="Optional expected SHA-256 of the writer brief")
        p_bf.add_argument(
            "--output-sha256",
            dest="output_sha256",
            help="Optional expected SHA-256 of the writer's raw output log",
        )
        p_bf.add_argument("--start-sha", required=True, help="Start SHA of the writer round")
        p_bf.add_argument("--end-sha", required=True, help="End SHA of the writer round")
        p_bf.add_argument("--evidence", required=True, help="Free-text audit evidence for this backfill")
        p_bf.add_argument("--agy-diff-sha256", help="Optional SHA-256 of transported agy diff")
        p_bf.add_argument("--supersedes", help="Entry-id of the existing entry on this commit being superseded")
        p_bf.add_argument("--reason", help="Single-line explanation of why the superseded entry is corrected")
        p_bf.add_argument("--signing-key", help="SSH private key path for signing the writer-log entry")
        p_bf.add_argument("--signer", help="Authorized signer principal (default: from key or orchestrator@concrete)")
        p_bf.add_argument("--allowed-signers", help="Path to allowed_writer_log_signers file")

    p_valnotes = sub.add_parser("validate-notes", help="Semantically validate a refs/notes/concrete-writer commit")
    p_valnotes.add_argument("--repo", default=".", help="Git repository path")
    p_valnotes.add_argument("--notes-rev", required=True, help="Notes commit SHA/ref to validate")
    p_valnotes.add_argument("--old-notes-rev", help="Previous remote notes commit SHA (to detect deletions/rewrites)")
    p_valnotes.add_argument("--allowed-signers", help="Path to allowed_writer_log_signers file")
    p_valnotes.add_argument(
        "--allow-missing-targets",
        action="store_true",
        help="Allow notes on commits from unfetched branches while still verifying signatures and continuity",
    )

    p_fetchnotes = sub.add_parser(
        "fetch-notes",
        help="Fetch remote writer notes, verify continuity against all recorded checkpoints, and update local refs",
    )
    p_fetchnotes.add_argument("--repo", default=".", help="Git repository path")
    p_fetchnotes.add_argument("--remote", default="origin", help="Remote to fetch writer notes from")
    p_fetchnotes.add_argument("--remote-ref", help="Remote writer-notes ref (default: refs/notes/concrete-writer)")
    p_fetchnotes.add_argument("--target-ref", help="Local ref to update (default: refs/notes/concrete-writer)")
    p_fetchnotes.add_argument(
        "--old-notes-rev",
        action="append",
        help="Additional prior notes commit SHA checkpoint (may be repeated)",
    )
    p_fetchnotes.add_argument(
        "--no-update-local",
        action="store_true",
        help="Do not also update refs/notes/concrete-writer when --target-ref is a different ref",
    )
    p_fetchnotes.add_argument(
        "--no-fetch",
        action="store_true",
        help="Validate local target ref against all recorded continuity checkpoints without contacting remote",
    )
    p_fetchnotes.add_argument("--allowed-signers", help="Path to allowed_writer_log_signers file")

    p_syncnotes = sub.add_parser(
        "sync-notes", help="Merge remote/companion writer notes into refs/notes/concrete-writer with append-only validation"
    )
    p_syncnotes.add_argument("--repo", default=".", help="Git repository path")
    p_syncnotes.add_argument("--from-rev", help="Notes commit SHA/ref to merge into refs/notes/concrete-writer")
    p_syncnotes.add_argument("--remote", help="Remote to fetch refs/notes/concrete-writer from")
    p_syncnotes.add_argument("--allowed-signers", help="Path to allowed_writer_log_signers file")

    p_pubnotes = sub.add_parser(
        "publish-notes", help="Sync and publish append-only refs/notes/concrete-writer to remote without bypassing hook validation"
    )
    p_pubnotes.add_argument("--repo", default=".", help="Git repository path")
    p_pubnotes.add_argument("--remote", default="origin", help="Remote to publish refs/notes/concrete-writer to")
    p_pubnotes.add_argument("--allowed-signers", help="Path to allowed_writer_log_signers file")

    args = parser.parse_args()
    if args.cmd == "derive":
        return cmd_derive(args)
    if args.cmd == "report":
        return cmd_report(args)
    if args.cmd == "verify":
        return cmd_verify(args)
    if args.cmd == "export-log":
        return cmd_export_log(args)
    if args.cmd == "stamp-commit":
        return cmd_stamp_commit(args)
    if args.cmd in ("backfill", "correct"):
        return cmd_backfill(args)
    if args.cmd == "validate-notes":
        return cmd_validate_notes(args)
    if args.cmd == "fetch-notes":
        return cmd_fetch_notes(args)
    if args.cmd == "sync-notes":
        return cmd_sync_notes(args)
    if args.cmd == "publish-notes":
        return cmd_publish_notes(args)
    return 2


if __name__ == "__main__":
    sys.exit(main())
