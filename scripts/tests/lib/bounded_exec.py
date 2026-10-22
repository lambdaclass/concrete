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
This process exits 0 once FILE is written; 2 on a usage error. An interrupt kills the group.
"""
import os
import signal
import subprocess
import sys
import time


def killpg(pgid, sig):
    try:
        os.killpg(pgid, sig)
    except (ProcessLookupError, PermissionError):
        pass


def main(argv):
    try:
        i = argv.index("--")
    except ValueError:
        print(__doc__, file=sys.stderr)
        return 2
    opts, cmd = argv[:i], argv[i + 1:]
    if len(opts) != 6 or opts[0::2] != ["--timeout", "--kill-after", "--status"] or not cmd:
        print(__doc__, file=sys.stderr)
        return 2
    budget, kill_after, status = float(opts[1]), float(opts[3]), opts[5]
    try:
        proc = subprocess.Popen(cmd, start_new_session=True)
    except OSError as e:
        print(f"bounded_exec: cannot run {cmd[0]}: {e}", file=sys.stderr)
        with open(status, "w") as fh:
            fh.write("exit 127\n")
        return 0
    pgid = proc.pid
    try:
        try:
            rc = proc.wait(timeout=budget)
            outcome = f"exit {rc if rc >= 0 else 128 - rc}"
        except subprocess.TimeoutExpired:
            killpg(pgid, signal.SIGTERM)
            deadline = time.monotonic() + kill_after
            while time.monotonic() < deadline:
                if proc.poll() is not None:
                    # The leader is gone; descendants that ignored SIGTERM may remain. Give them
                    # the same grace, then kill the group either way.
                    time.sleep(min(0.2, max(0.0, deadline - time.monotonic())))
                    if not group_alive(pgid):
                        break
                else:
                    time.sleep(0.05)
            killpg(pgid, signal.SIGKILL)
            proc.wait()
            outcome = "timeout"
    except BaseException:
        killpg(pgid, signal.SIGKILL)
        raise
    with open(status, "w") as fh:
        fh.write(outcome + "\n")
    return 0


def group_alive(pgid):
    try:
        os.killpg(pgid, 0)
        return True
    except ProcessLookupError:
        return False
    except PermissionError:
        return True


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
