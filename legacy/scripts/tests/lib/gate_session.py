#!/usr/bin/env python3
"""Run a gate runner as its own session, and own every process of that session (bug 244).

THE DEFECT. A gate run is a tree: the runner, its pool of backgrounded gates, the compiler and lake
processes those start, and harnesses that copy the tree and mutate the copy. Killing the runner
killed one process of that tree. Non-interactive bash gives its children no group of their own, so
nothing could name "this run's processes": the rest were reparented to init and kept working the
tree, and the repository lock (scripts/tests/lib/fresh.sh) correctly refused every later run, then
refused again after they were gone, because a dead creator pid proves nothing about the children it
left behind. Recovery was a human deleting the lock by hand.

THE STRUCTURE. The run gets a process group, and the group is the unit of ownership:

    front   (this process, in the caller's group)    receives the caller's signals
      leader  (session and group leader, pgid = sid)  exports CONCRETE_GATE_SESSION=<pgid>
        runner and every gate it starts              inherit the group

  * A signal to the front (SIGINT, SIGTERM, SIGHUP) becomes SIGTERM to the whole group. SIGTERM, not
    the signal received: non-interactive bash makes its backgrounded children ignore SIGINT, so a
    forwarded SIGINT would leave the gate pool running.
  * When the leader exits, for any reason, the front sweeps the group: SIGTERM, a grace period, then
    SIGKILL, until killpg(pgid, 0) reports no member. Survivors of a normal run are swept too: a run
    whose gates left processes behind did not finish.
  * The front then asks fresh.sh to reclaim the lock, which it does only if the lock's recorded
    session has no live member (see `_gate_lock_lease_state` there). So a killed run leaves neither
    processes nor a lock, and a run whose children are still alive is never handed over. This is
    also the release of a NORMAL run: the session-owning runner leaves its lock in place, because a
    process its gates left behind is still working the tree until the sweep has removed it.
  * A signalled run is SIGKILLed 3 x grace + 5 s after the first signal, even while the runner is
    still alive: waiting for a runner that ignores SIGTERM would never reach the escalation. The
    margin lets the runner finish its own sweep (TERM, grace, KILL) first, so its summary and its
    heavy slot are settled only once its gates are gone.
  * Campaign groups that run_campaign_child.py registered in this session's lock are swept with the
    session: they left the group on purpose, so killpg() alone would not reach them.
  * If the front itself dies (SIGKILL, or the caller's whole group is killed), the leader sees its
    pipe from the front reach EOF and terminates the group itself. The front stays in the caller's
    group precisely so that terminal Ctrl-C and a caller's group kill reach it.

WHAT THIS DOES NOT COVER, stated rather than implied. A descendant that calls setsid() or setpgid()
leaves the group, and killpg() cannot see it. The one deliberate escaper in this repository,
scripts/tests/lib/run_campaign_child.py, registers the group it creates in the lock directory, so
the lock stays held while it lives. SIGKILL of the front and the leader together leaves the runner
running to completion with the lock held, which is a live run and not a wedge.

PORTABILITY. start_new_session (setsid(2)), killpg(2) and `ps -A -o pid=,pgid=` behave the same on
Linux and macOS; no setsid(1), flock(1) or /proc is used, because macOS CI has none of them.

Usage:
    gate_session.py [--grace SECONDS] -- <command> [args...]
Exit status: the command's, 128+N if it died of signal N; if the front itself was signalled, it
re-raises that signal after the sweep, so its caller sees the death it caused.
"""
import os
import signal
import subprocess
import sys
import threading
import time

FORWARDED = (signal.SIGINT, signal.SIGTERM, signal.SIGHUP)
FRESH_SH = os.path.join(os.path.dirname(os.path.abspath(__file__)), "fresh.sh")
LOCK_DIR = os.path.normpath(os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "..", "..", ".gate.lock"))


def registered_groups(session: int) -> list:
    """Campaign groups registered in the repository lock, if that lock is this session's (bug 244).

    run_campaign_child.py leaves the session on purpose, so killpg() on the session cannot reach its
    child; it records the group it creates under <lock>/groups instead, and cancelling the run must
    cancel those groups too. Only a lock whose owner names THIS session is read: another run's
    registrations are not this run's to kill.
    """
    try:
        with open(os.path.join(LOCK_DIR, "owner"), encoding="utf-8") as fh:
            if f"pgid={session}" not in fh.read().split():
                return []
        return [int(e) for e in os.listdir(os.path.join(LOCK_DIR, "groups")) if e.isdigit()]
    except (OSError, ValueError):
        return []


def note(msg: str) -> None:
    print(f"gate-session: {msg}", file=sys.stderr, flush=True)


def group_alive(pgid: int) -> bool:
    """True unless the kernel says the group has no member. EPERM is a member we cannot signal."""
    try:
        os.killpg(pgid, 0)
    except ProcessLookupError:
        return False
    except PermissionError:
        return True
    return True


def ps_members(pgid: int, exclude=()) -> list:
    """Processes whose pgid (or, where ps reports it, session id) is `pgid`. None if ps failed."""
    for fmt in ("pid=,pgid=,sid=", "pid=,pgid="):
        try:
            p = subprocess.Popen(["ps", "-A", "-o", fmt], stdout=subprocess.PIPE,
                                 stderr=subprocess.DEVNULL, text=True)
            out, _ = p.communicate()
        except OSError:
            continue
        if p.returncode != 0:
            continue
        rows = []
        for line in out.splitlines():
            f = line.split()
            if len(f) < 2 or not all(x.isdigit() for x in f):
                continue
            pid = int(f[0])
            # ps itself is a member when the leader probes its own group.
            if pid in exclude or pid == p.pid:
                continue
            if any(int(x) == pgid for x in f[1:]):
                rows.append(pid)
        return rows
    return None


def signal_pids(pids, sig) -> None:
    for pid in pids:
        try:
            os.kill(pid, sig)
        except OSError:
            pass


def sweep(pgid: int, grace: float, exclude=()) -> int:
    """Terminate every member of the group (and session); return how many were found alive.

    The group is probed with ps rather than killpg when `exclude` is given, because the leader
    sweeping its own group is itself a member.
    """
    # RE-READ ON EVERY PASS: a launcher still in flight can register its group during the sweep.
    def extra():
        return registered_groups(pgid)

    def alive():
        if exclude:
            m = ps_members(pgid, exclude)
            m = m if m is not None else [-1]
        else:
            m = ps_members(pgid) or []
            if group_alive(pgid) and not m:
                m = [-1]
        return m + [-g for g in extra() if group_alive(g)]

    found = alive()
    if not found:
        return 0
    n = len([p for p in found if p > 0]) or 1
    def signal_all(sig):
        for g in extra():
            try:
                os.killpg(g, sig)
            except OSError:
                pass
        if exclude:
            signal_pids([p for p in alive() if p > 0], sig)
        else:
            try:
                os.killpg(pgid, sig)
            except OSError:
                pass
            signal_pids([p for p in (ps_members(pgid) or []) if p > 0], sig)

    # EVERY POLL RE-SIGNALS what is alive now: a member that forked after the previous signal, or a
    # group registered after it, would otherwise be listed but never sent anything.
    for sig, wait_for in ((signal.SIGTERM, grace), (signal.SIGKILL, 5.0)):
        deadline = time.monotonic() + wait_for
        while time.monotonic() < deadline:
            signal_all(sig)
            time.sleep(0.1)
            if not alive():
                return n
    if alive():
        note(f"process group {pgid} still has members after SIGKILL; the lock stays held")
    return n


def exit_status(raw: int) -> int:
    return 128 - raw if raw < 0 else raw


def leader(fd: int, cmd: list, grace: float) -> int:
    pgid = os.getpgrp()
    me = os.getpid()
    os.environ["CONCRETE_GATE_SESSION"] = str(pgid)
    # HANDLED, NOT IGNORED. The runner must be able to trap these, and bash cannot trap a signal that
    # was ignored when it started; a Python handler resets to the default at exec, SIG_IGN would not.
    # The leader survives the group-wide SIGTERM it is part of, so it can report the runner's status —
    # but it REMEMBERS the signal: one that reached the group before the runner joined it would
    # otherwise be absorbed here, and the runner would start unsignalled with nobody waiting to stop it.
    cancelled = threading.Event()
    for s in FORWARDED:
        signal.signal(s, lambda *_: cancelled.set())
    # A TEST HOOK, inert by default: holds the startup window open so the regression leg can deliver
    # a signal inside it rather than hoping to land there.
    delay = float(os.environ.pop("CONCRETE_GATE_SESSION_STARTUP_DELAY", "0") or 0)
    if delay > 0:
        os.write(2, b"gate-session: leader waiting before launch\n")
        cancelled.wait(delay)
    if cancelled.is_set():
        return 128 + signal.SIGTERM
    child = subprocess.Popen(cmd)
    if cancelled.is_set():
        try:
            os.kill(child.pid, signal.SIGTERM)
        except OSError:
            pass
    front_gone = threading.Event()

    def watch_front():
        try:
            while os.read(fd, 64):
                pass
        except OSError:
            pass
        front_gone.set()
        if child.poll() is None:
            # THE ESCALATION RUNS HERE, not after the runner exits: a runner that ignores SIGTERM would
            # otherwise keep the main thread in wait() forever, with nobody left to send SIGKILL.
            note("the session's front process is gone; terminating the run")
            sweep(pgid, grace, exclude={me})

    threading.Thread(target=watch_front, daemon=True).start()
    rc = child.wait()
    # Normally the front sweeps. If the front is gone, nobody else will, so the leader does it before
    # leaving — excluding itself, which is a member of the group being swept.
    if front_gone.wait(0.2):
        sweep(pgid, grace, exclude={me})
    return exit_status(rc)


def front(cmd: list, grace: float) -> int:
    r, w = os.pipe()
    caught = []
    proc = None

    def on_signal(signum, _frame):
        caught.append((signum, time.monotonic()))
        if proc is not None:
            try:
                os.killpg(proc.pid, signal.SIGTERM)
            except OSError:
                pass

    for s in FORWARDED:
        signal.signal(s, on_signal)
    proc = subprocess.Popen([sys.executable, os.path.abspath(__file__), "--grace", str(grace),
                             "--leader", str(r), "--", *cmd],
                            start_new_session=True, pass_fds=(r,))
    os.close(r)
    if caught:
        on_signal(caught[0][0], None)
    # A SIGNALLED RUN GETS THE GRACE PERIOD, then SIGKILL — measured from the first signal, and while
    # the leader is still running. Waiting for the leader first would wait for a runner that ignores
    # SIGTERM, and the escalation would never start.
    # The runner sweeps its own group first (TERM, grace, KILL) so that its summary and heavy slot
    # are settled after its gates are gone; the launcher's SIGKILL waits long enough for that.
    deadline = 3 * grace + 5
    killed = False
    while True:
        try:
            raw = proc.wait(timeout=0.2)
            break
        except subprocess.TimeoutExpired:
            if caught and not killed and time.monotonic() - caught[0][1] > deadline:
                note(f"the run did not stop within {deadline:g} s of the signal; killing it")
                try:
                    os.killpg(proc.pid, signal.SIGKILL)
                except OSError:
                    pass
                killed = True
    found = sweep(proc.pid, grace)
    if found and not caught and raw == 0:
        # A BACKSTOP: the runner sweeps before it reconciles, so this means something outlived even
        # that — the run's verdict was published while its work went on, and it does not stand.
        note(f"{found} process(es) of the run outlived the runner and were terminated; the run fails")
        raw = 1
    os.close(w)
    # THE LOCK IS RECLAIMED BY THE SAME RULE ANY LATER RUN APPLIES, never released by fiat: the front
    # did not create it, and the rule refuses unless the recorded session is provably empty. This is
    # also how a session-owned runner's lock is released after a NORMAL run: the runner leaves it, so
    # the tree is not handed over while a process the run left behind is still being swept.
    if os.path.isfile(FRESH_SH):
        quiet = "quiet" if (not caught and raw == 0) else ""
        subprocess.run(["bash", "-c", '. "$1" && _gate_lock_reclaim_stale $2', "gate_session", FRESH_SH, quiet],
                       stdin=subprocess.DEVNULL, check=False)
    if caught:
        sig = caught[0][0]
        signal.signal(sig, signal.SIG_DFL)
        os.kill(os.getpid(), sig)
        return 128 + sig
    return exit_status(raw)


def main(argv: list) -> int:
    grace = float(os.environ.get("CONCRETE_GATE_SESSION_GRACE", "10"))
    fd = None
    while argv and argv[0] != "--":
        if argv[0] == "--grace" and len(argv) > 1:
            grace = float(argv[1]); argv = argv[2:]
        elif argv[0] == "--leader" and len(argv) > 1:
            fd = int(argv[1]); argv = argv[2:]
        else:
            break
    if not argv or argv[0] != "--" or len(argv) < 2:
        print("usage: gate_session.py [--grace SECONDS] -- <command> [args...]", file=sys.stderr)
        return 2
    cmd = argv[1:]
    if fd is not None:
        return leader(fd, cmd, grace)
    return front(cmd, grace)


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
