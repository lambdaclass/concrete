#!/usr/bin/env python3
"""Run one command under a time budget, owning its whole process group (lib/bounded.sh's runner).

  bounded_exec.py --timeout SECS --kill-after SECS --status FILE [--deadline-mono MONO] -- CMD [ARGS...]
  bounded_exec.py --monotonic-deadline SECS
  bounded_exec.py --monotonic-remaining MONO

The command inherits stdin, stdout and stderr (the caller redirects them) and runs as the leader of
a new session, so it and every descendant that does not create its own session share one process
group. The outcome goes to FILE, never to the command's streams, so nothing the command prints can
fake or hide it:
  exit N      the command finished with status N (128+S when killed by signal S, as a shell reports)
  timeout     the budget (or monotonic deadline) expired: the group got SIGTERM, then, KILL_AFTER
              seconds later (clamped to the monotonic deadline when active), SIGKILL, whether or not
              the leader had already exited, so a descendant that ignores SIGTERM does not outlive
              the attempt or the gate's monotonic deadline.
When the command finishes within its budget its group is left alone, as timeout(1) does.
Both durations must be finite numbers, the budget positive and the grace nonnegative.
If BOUNDED_DEADLINE_MONO (or --deadline-mono) is set to a monotonic clock timestamp, both the
attempt timeout and the SIGTERM->SIGKILL termination grace are bounded by that monotonic deadline.
This process exits 0 once FILE is written and 2 on a usage error. If it is itself cancelled
(SIGTERM, SIGHUP, SIGINT), it kills the command's group, writes no outcome, and exits 128+signal.
"""
import math
import os
import signal
import subprocess
import sys
import time


class Cancelled(Exception):
    def __init__(self, sig):
        super().__init__(sig)
        self.sig = sig


def killpg(pgid, sig):
    try:
        os.killpg(pgid, sig)
    except (ProcessLookupError, PermissionError):
        pass


def group_alive(pgid):
    try:
        os.killpg(pgid, 0)
        return True
    except ProcessLookupError:
        return False
    except PermissionError:
        return True


def duration(text, positive):
    try:
        v = float(text)
    except ValueError:
        return None
    if not math.isfinite(v) or v < 0 or (positive and v == 0):
        return None
    return v


def usage(msg):
    print(f"bounded_exec: {msg}\n{__doc__}", file=sys.stderr)
    return 2


def main(argv):
    if len(argv) == 2 and argv[0] == "--monotonic-deadline":
        d = duration(argv[1], True)
        if d is None:
            return usage(f"invalid duration: {argv[1]}")
        print(f"{time.monotonic() + d:.6f}")
        return 0
    if len(argv) == 2 and argv[0] == "--monotonic-remaining":
        try:
            dl = float(argv[1])
        except ValueError:
            return usage(f"invalid monotonic deadline: {argv[1]}")
        if not math.isfinite(dl):
            return usage(f"invalid monotonic deadline: {argv[1]}")
        rem = max(0.0, dl - time.monotonic())
        print(int(math.floor(rem)))
        return 0

    try:
        i = argv.index("--")
    except ValueError:
        return usage("missing --")
    opts, cmd = argv[:i], argv[i + 1:]
    deadline_mono = None
    env_dl = os.environ.get("BOUNDED_DEADLINE_MONO", "").strip()
    if env_dl:
        try:
            deadline_mono = float(env_dl)
            if not math.isfinite(deadline_mono):
                return usage(f"invalid BOUNDED_DEADLINE_MONO: {env_dl}")
        except ValueError:
            return usage(f"invalid BOUNDED_DEADLINE_MONO: {env_dl}")

    if len(opts) == 8 and opts[0::2] == ["--timeout", "--kill-after", "--status", "--deadline-mono"]:
        try:
            deadline_mono = float(opts[7])
            if not math.isfinite(deadline_mono):
                return usage(f"invalid --deadline-mono: {opts[7]}")
        except ValueError:
            return usage(f"invalid --deadline-mono: {opts[7]}")
        opts = opts[:6]

    if len(opts) != 6 or opts[0::2] != ["--timeout", "--kill-after", "--status"] or not cmd:
        return usage("expected --timeout SECS --kill-after SECS --status FILE [--deadline-mono MONO] -- CMD...")
    budget, kill_after, status = duration(opts[1], True), duration(opts[3], False), opts[5]
    if budget is None or kill_after is None:
        return usage(f"invalid duration: --timeout {opts[1]} --kill-after {opts[3]}")

    if deadline_mono is not None:
        rem = deadline_mono - time.monotonic()
        if rem <= 0.05:
            with open(status, "w") as fh:
                fh.write("timeout\n")
            return 0
        if budget + kill_after > rem:
            kill_after = min(kill_after, max(0.05, rem * 0.25))
            budget = min(budget, max(0.0, rem - kill_after))

    def cancel(sig, _frame):
        raise Cancelled(sig)

    cancel_sigs = {signal.SIGTERM, signal.SIGHUP, signal.SIGINT}
    for s in cancel_sigs:
        signal.signal(s, cancel)
    pgid = None
    try:
        # Cancellation is held off while the child is created, so it can never arrive between the
        # child starting and its group being recorded; the child itself starts with them unblocked.
        signal.pthread_sigmask(signal.SIG_BLOCK, cancel_sigs)
        try:
            proc = subprocess.Popen(cmd, start_new_session=True,
                                    preexec_fn=lambda: signal.pthread_sigmask(signal.SIG_UNBLOCK, cancel_sigs))
        except OSError as e:
            proc = None
            print(f"bounded_exec: cannot run {cmd[0]}: {e}", file=sys.stderr)
            outcome = "exit 127"
        else:
            pgid = proc.pid
        finally:
            signal.pthread_sigmask(signal.SIG_UNBLOCK, cancel_sigs)  # a held signal is delivered here
        if proc is not None:
            try:
                rc = proc.wait(timeout=budget)
                outcome = f"exit {rc if rc >= 0 else 128 - rc}"
            except subprocess.TimeoutExpired:
                killpg(pgid, signal.SIGTERM)
                grace_deadline = time.monotonic() + kill_after
                if deadline_mono is not None:
                    grace_deadline = min(grace_deadline, deadline_mono)
                while time.monotonic() < grace_deadline:
                    if proc.poll() is not None and not group_alive(pgid):
                        break
                    time.sleep(0.05)
                killpg(pgid, signal.SIGKILL)
                proc.wait()
                outcome = "timeout"
    except Cancelled as c:
        if pgid is not None:
            killpg(pgid, signal.SIGKILL)
            try:
                proc.wait(timeout=5)
            except Exception:
                pass
        return 128 + c.sig
    with open(status, "w") as fh:
        fh.write(outcome + "\n")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
