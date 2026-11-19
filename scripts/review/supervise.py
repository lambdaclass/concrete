"""Run one reviewer and reap its descendants before returning to archive cleanup."""
import ctypes
import os
from pathlib import Path
import signal
import subprocess
import sys
import time


def launch_arguments(arguments):
    try:
        directory, *arguments = arguments
        separator = arguments.index("--")
        environment = dict(entry.split("=", 1) for entry in arguments[:separator])
    except ValueError:
        return None
    command = arguments[separator + 1:]
    if not directory or "" in environment or not command:
        return None
    return directory, environment, command


def main():
    check_only = sys.argv[1:] == ["--check"]
    if not check_only:
        launch = launch_arguments(sys.argv[1:])
        if launch is None:
            print("usage: supervise.py {--check | DIRECTORY [NAME=VALUE ...] -- REVIEWER [ARG ...]}",
                  file=sys.stderr)
            return 2
        directory, environment, command = launch
    if sys.version_info < (3, 9):
        sys.exit("review runner: running reviews requires Python 3.9 or newer")
    # An orphaned grandchild is reparented here even after setsid/setpgid.
    # Fail before launch if this host cannot provide that guarantee.
    children = Path("/proc/self/task/{}/children".format(os.getpid()))
    pr_set_child_subreaper = 36
    if (sys.platform != "linux" or not children.is_file()
            or ctypes.CDLL(None, use_errno=True).prctl(pr_set_child_subreaper, 1, 0, 0, 0) != 0):
        sys.exit("review runner: running reviews requires Linux with procfs and child subreaping")
    if check_only:
        return 0

    cancelled = False

    def cancel(_signum, _frame):
        nonlocal cancelled
        cancelled = True

    for sig in (signal.SIGHUP, signal.SIGINT, signal.SIGTERM):
        signal.signal(sig, cancel)
    if cancelled:
        return 2

    if any(not os.path.isabs(entry) for entry in environment.get("PATH", "").split(os.pathsep)):
        sys.exit("review runner: PATH entries must be absolute directories")
    for name in ("HOME", "CODEX_HOME", "TMPDIR", "SSL_CERT_FILE", "SSL_CERT_DIR",
                 "GIT_CEILING_DIRECTORIES"):
        if name not in environment:
            continue
        value = environment[name]
        entries = value.split(os.pathsep) if name in ("SSL_CERT_DIR", "GIT_CEILING_DIRECTORIES") else [value]
        if any(not os.path.isabs(entry) for entry in entries):
            sys.exit(f"review runner: {name} entries must be absolute paths")
    if not os.path.isabs(command[0]):
        print("review runner: reviewer path must be absolute", file=sys.stderr)
        return 2
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
