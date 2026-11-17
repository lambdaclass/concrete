"""Run one reviewer and reap its descendants before returning to archive cleanup."""
import ctypes
import os
from pathlib import Path
import signal
import subprocess
import sys
import time


def main():
    if sys.version_info < (3, 9):
        sys.exit("review runner: running reviews requires Python 3.9 or newer")
    # An orphaned grandchild is reparented here even after setsid/setpgid.
    # Fail before launch if this host cannot provide that guarantee.
    children = Path("/proc/self/task/{}/children".format(os.getpid()))
    pr_set_child_subreaper = 36
    if (sys.platform != "linux" or not children.is_file()
            or ctypes.CDLL(None, use_errno=True).prctl(pr_set_child_subreaper, 1, 0, 0, 0) != 0):
        sys.exit("review runner: running reviews requires Linux with procfs and child subreaping")
    if sys.argv[1:] == ["--check"]:
        return 0

    cancelled = False

    def cancel(_signum, _frame):
        nonlocal cancelled
        cancelled = True

    for sig in (signal.SIGHUP, signal.SIGINT, signal.SIGTERM):
        signal.signal(sig, cancel)
    if cancelled:
        return 2

    directory = sys.argv[1]
    arguments = sys.argv[2:]
    separator = arguments.index("--")
    environment = dict(entry.split("=", 1) for entry in arguments[:separator])
    command = arguments[separator + 1:]
    if not command or not os.path.isabs(command[0]):
        sys.exit("review runner: reviewer path must be absolute")
    reviewer = subprocess.Popen(command, cwd=directory, env=environment, start_new_session=True)
    while not cancelled and reviewer.poll() is None:
        time.sleep(0.01)

    # Terminate the reviewer and its ordinary group first. Detached groups
    # become our children as their parents exit. Keep reaping and scanning
    # until waitpid confirms there are no children left.
    if reviewer.returncode is None:
        try:
            os.killpg(reviewer.pid, signal.SIGTERM)
        except ProcessLookupError:
            pass
    deadline = time.monotonic() + 0.25
    while True:
        try:
            pid, status = os.waitpid(-1, os.WNOHANG)
        except ChildProcessError:
            break
        if pid:
            if pid == reviewer.pid:
                reviewer.returncode = os.waitstatus_to_exitcode(status)
            continue
        sig = signal.SIGTERM if time.monotonic() < deadline else signal.SIGKILL
        for child in children.read_text().split():
            try:
                os.kill(int(child), sig)
            except ProcessLookupError:
                pass
        time.sleep(0.01)
    return 2 if cancelled else reviewer.returncode


if __name__ == "__main__":
    sys.exit(main())
