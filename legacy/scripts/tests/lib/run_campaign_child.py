#!/usr/bin/env python3
"""Launch the campaign child in its own session and report on its process group.

WHAT THIS PROVES, AND WHAT IT DOES NOT.

`killpg(pgid, 0)` establishes exactly one fact: no process remains in the campaign's ORIGINAL
process group. It does NOT establish that no campaign work remains anywhere. A descendant may call
setpgid(2), or start a session of its own, and outlive the group it was born into. Measured, not
assumed: a grandchild started with start_new_session leaves this check reporting an empty group
while it is still running, and a committed control asserts that non-detection so the limitation
cannot quietly become a claim.

That escape is OUT OF THE THREAT MODEL, deliberately and narrowly. The processes this campaign
starts are its own gates, `lake` and the compiler; none leave their process group, and the hazard
being defended against is an ACCIDENTAL survivor — a backgrounded build, an orphaned worker — not a
descendant hiding from its supervisor. Real containment needs a cgroup or a jail, neither portable
across the platforms this repository supports.

So the reported field is `process_group_state`, never `descendants_state`, and the supervisor refuses
with `campaign_group_not_empty`. Whoever reads the artifact learns which question was answered.

FOUR EARLIER ATTEMPTS, three of them shipped, all found by RUNNING a campaign rather than reading:
`pgrep -P $$` (one generation, and the child is already reaped); matching the repository path (also
matched the launcher); matching the workspace path (matched the supervisor itself, and any shell
whose command line merely CONTAINED the search string); and /proc ancestry (correct on Linux, absent
on macOS, where this repository has already taken an outage). Attempts 2-4 refused CLEAN runs, and a
check that always fires carries no more information than one that never does.

Designed using portable POSIX mechanisms — setsid via start_new_session, and killpg — so Linux and
macOS are INTENDED to agree; that equivalence is established by running the gate on both in CI, not
by this file asserting it.

Usage:
    run_campaign_child.py --report <path> --run-id <id> -- <command> [args...]

The report is written to <path> ATOMICALLY (temp file in the same directory, then rename) and only
after the child has exited, so a reader never observes a partial record. The child's stdout and
stderr are inherited untouched: they are the campaign transcript, and a launcher that captured them
would destroy the evidence it exists to protect.

Report fields, exactly once each, in this order:
    protocol_version=1          the contract this record is written against
    run_id=<str>                the run this status belongs to; a stale report cannot be reused
    child_rc=<int>              exit status; for a signalled child, 128+signal
    child_signalled=<0|1>       1 iff terminated by a signal — this is what distinguishes a child
                                that EXITED 143 from one KILLED by signal 15, which child_rc alone
                                cannot
    child_signal=<int>          the signal number, or 0
    process_group_state=<str>   empty | nonempty | permission_denied | error:<detail>
    pgid=<int>                  the group that was checked
"""
import os
import signal
import subprocess
import sys
import tempfile

PROTOCOL_VERSION = 1


def process_group_state(pgid: int) -> str:
    """Classify the original process group, distinguishing every outcome.

    ONLY `empty` MAY PERMIT PUBLICATION. Collapsing these into a boolean is how "we could not tell"
    becomes "nothing is running": EPERM means the group EXISTS but is not ours to signal, which is
    alive, and any other error means the question was not answered at all. Neither is absence.
    """
    try:
        os.killpg(pgid, 0)
    except ProcessLookupError:
        return "empty"
    except PermissionError:
        return "permission_denied"
    except OSError as exc:
        return f"error:{exc.errno}"
    return "nonempty"


def lease_groups_dir():
    """The `groups/` directory of the repository lock this process inherited, or None (bug 244).

    This launcher deliberately LEAVES its caller's session, so the caller's process group no longer
    covers the child. A lock that is reclaimed when its recorded session is empty would then be
    reclaimed while the campaign still works the tree — the 2026-08-20 incident. So the group this
    launcher creates is registered in the lock it runs under, and fresh.sh keeps the lock while any
    registered group has a member. `pending.<pid>` is written before the launch, because the group's
    id exists only once the child does: a launcher killed in between leaves a pending entry, which
    makes reclaim refuse rather than proceed.
    """
    tok = os.environ.get("CONCRETE_GATE_LOCK", "")
    if ":" not in tok:
        return None
    d = tok.split(":", 1)[1]
    if not d.endswith("/.gate.lock") or not os.path.isdir(d):
        return None
    return os.path.join(d, "groups")


def _touch(path: str) -> None:
    with open(path, "w", encoding="utf-8") as fh:
        fh.write(f"launcher={os.getpid()}\n")


def main() -> int:
    argv = sys.argv[1:]
    if len(argv) < 6 or argv[0] != "--report" or argv[2] != "--run-id" or argv[4] != "--":
        print("usage: run_campaign_child.py --report <path> --run-id <id> -- <command> [args...]",
              file=sys.stderr)
        return 2
    report_path, run_id, cmd = argv[1], argv[3], argv[5:]
    if not run_id:
        print("error: --run-id must not be empty", file=sys.stderr)
        return 2

    # start_new_session=True calls setsid() in the child: it becomes session and group leader, so its
    # pgid is its pid and no ancestor shares the group.
    groups = lease_groups_dir()
    pending = None
    if groups is not None:
        pending = os.path.join(groups, f"pending.{os.getpid()}")
        try:
            os.makedirs(groups, exist_ok=True)
            _touch(pending)
        except OSError as exc:
            print(f"error: cannot register the campaign group in the repository lock ({exc}); "
                  "refusing to start work the lock could not account for", file=sys.stderr)
            return 2
    proc = subprocess.Popen(cmd, start_new_session=True)
    pgid = proc.pid
    registered = None
    if groups is not None:
        registered = os.path.join(groups, str(pgid))
        try:
            _touch(registered)
            os.unlink(pending)
        except OSError:
            pass  # the pending entry stays, and the lock with it: fail closed

    try:
        raw_rc = proc.wait()
    except KeyboardInterrupt:
        try:
            os.killpg(pgid, signal.SIGINT)
        except OSError:
            pass
        raw_rc = -signal.SIGINT

    # A SIGNALLED CHILD IS NOT AN EXIT STATUS. Popen.wait() returns -N for signal N, which a shell
    # comparison reads as a negative or coerces silently. child_rc carries the conventional 128+N so
    # a consumer reading only that field still sees failure, and child_signalled is what actually
    # separates "exited 143" from "killed by 15".
    signalled = raw_rc < 0
    signum = -raw_rc if signalled else 0
    rc_field = 128 + signum if signalled else raw_rc

    state = process_group_state(pgid)
    if registered is not None and state == "empty":
        try:
            os.unlink(registered)
        except OSError:
            pass

    body = (
        f"protocol_version={PROTOCOL_VERSION}\n"
        f"run_id={run_id}\n"
        f"child_rc={rc_field}\n"
        f"child_signalled={1 if signalled else 0}\n"
        f"child_signal={signum}\n"
        f"process_group_state={state}\n"
        f"pgid={pgid}\n"
    )

    # ATOMIC, AND LAUNCHER-OWNED. A reader must never see a half-written record, and a record left
    # by an earlier run must never be mistaken for this one — which is what run_id binding is for.
    directory = os.path.dirname(os.path.abspath(report_path)) or "."
    fd, tmp = tempfile.mkstemp(dir=directory, prefix=".launchreport.")
    try:
        with os.fdopen(fd, "w", encoding="utf-8") as fh:
            fh.write(body)
            fh.flush()
            os.fsync(fh.fileno())
        os.replace(tmp, report_path)
    except BaseException:
        try:
            os.unlink(tmp)
        except OSError:
            pass
        raise
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
