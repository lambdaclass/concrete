#!/usr/bin/env python3
"""Run one command under a time budget, owning its whole process group (lib/bounded.sh's runner).

  bounded_exec.py --timeout SECS --kill-after SECS --status FILE -- CMD [ARGS...]

The command inherits stdin, stdout and stderr (the caller redirects them) and runs as the leader of
a new session, so it and every descendant that does not create its own session share one process
group. The outcome goes to FILE, never to the command's streams, so nothing the command prints can
fake or hide it:
  exit N      the command finished with status N (128+S when killed by signal S, as a shell reports)
  timeout     the budget expired: the group got SIGTERM, then, KILL_AFTER seconds later, SIGKILL,
              whether or not the leader had already exited, so a descendant that ignores SIGTERM
              does not outlive the attempt.
When the command finishes within its budget its group is left alone, as timeout(1) does.
Both durations must be finite numbers, the budget positive and the grace nonnegative.
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
    try:
        i = argv.index("--")
    except ValueError:
        return usage("missing --")
    opts, cmd = argv[:i], argv[i + 1:]
    if len(opts) != 6 or opts[0::2] != ["--timeout", "--kill-after", "--status"] or not cmd:
        return usage("expected --timeout SECS --kill-after SECS --status FILE -- CMD...")
    budget, kill_after, status = duration(opts[1], True), duration(opts[3], False), opts[5]
    if budget is None or kill_after is None:
        return usage(f"invalid duration: --timeout {opts[1]} --kill-after {opts[3]}")

    def cancel(sig, _frame):
        raise Cancelled(sig)

    for s in (signal.SIGTERM, signal.SIGHUP, signal.SIGINT):
        signal.signal(s, cancel)
    pgid = None
    try:
        try:
            proc = subprocess.Popen(cmd, start_new_session=True)
        except OSError as e:
            print(f"bounded_exec: cannot run {cmd[0]}: {e}", file=sys.stderr)
            outcome = "exit 127"
        else:
            pgid = proc.pid
            try:
                rc = proc.wait(timeout=budget)
                outcome = f"exit {rc if rc >= 0 else 128 - rc}"
            except subprocess.TimeoutExpired:
                killpg(pgid, signal.SIGTERM)
                deadline = time.monotonic() + kill_after
                while time.monotonic() < deadline:
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
