# Bug 244 — a killed gate run leaves its children running and the repository lock held

<!-- bug-ledger
summary: killing a gate run killed only the runner; its gates, compilers and harness copies kept working the tree under init, and the repository lock (which recorded only the creator pid, proving nothing about its children) refused every later run until removed by hand. A run is now one process group of its own session; a signal to the runner terminates the group, the lock records it, and a later run reclaims the lock only once the kernel reports that group (and any campaign group registered in the lock) empty. Covered by the repository-lock gate. Class 16.
coverage: a killed gate run left its children working the tree and the repository lock held until removed by hand. Regression vehicle is the repository-lock gate's bug-244 legs (a runner sandbox killed by SIGTERM, SIGINT and SIGKILL; the lock library fresh.sh reclaims only a provably empty session group), not a tests/programs fixture (2026-10-25)
class: 16 — a killed gate run's children and its lock, owned by no process group a signal or a later run could address
silent: no
ruling: Violates: unrecorded
-->

**Status:** FIXED (branch `wip/gate-runner-kill`)
**Found:** 2026-10-25, operating the pre-push hook on the OVH dev host: a gate run
(`run_ci_gates_local.sh`, started by `.githooks/pre-push`) was killed mid-run. Its gate processes
and mutation-harness temp trees kept running, reparented to init. The next hook run refused with
"REPOSITORY BUSY" because gate-like processes were still running, and once they had finished it
refused again ("the recorded creator is gone, but that does NOT mean the tree is free … recover
explicitly: rm -rf .gate.lock"). Only deleting the lock by hand let work continue.
**Severity:** loud. No wrong verdict: the lock refused correctly both times. The repository stayed
wedged until someone removed `.gate.lock` by hand, and until then the orphans kept loading the host
and working the tree.

Confirmed real by Codex review, 2026-10-25: Signals left gate children running, defeating ROADMAP.md R-0513’s harness lifecycle requirements; refusing a lock while child liveness was unknown was intentional.

## Reproducer

In a throwaway git repository holding the runner, its libraries, a stub `lake` and a gate that
starts a background `sleep` and a `sleep` that ignores SIGTERM:

```
bash scripts/tests/run_ci_gates_local.sh --first "bash scripts/tests/check_dummy_long.sh" &
kill -TERM $!          # or SIGINT
```

Observed on main 593dc100: the runner returned only after the gate's sleeps ended, `.gate.lock`
stayed (`pid=<runner> cmd=run_ci_gates_local.sh`, no group), and the next run refused with
"REPOSITORY BUSY". Killing the runner with SIGKILL left the gate and its children running.
Expected: the signal ends every process of the run, and the next run starts without a manual step,
except while a process of the old run is still alive.

## Cause

The run had no process-level identity. Non-interactive bash gives its children no process group of
their own: they stay in the caller's group (the hook's, git's, the terminal's). So nothing could
name "the processes of this run".

- `run_ci_gates_local.sh` trapped INT/TERM/HUP only to set `RUN_INTERRUPTED`. The signal stopped
  neither its pool nor its foreground gate (bash defers a trap until the foreground command returns),
  and backgrounded pool children ignore SIGINT.
- `.githooks/pre-push` ran the runner in the foreground, so a signal to the hook killed only the hook.
- `scripts/tests/lib/fresh.sh` recorded only the creating pid. A dead creator proves nothing about
  the children it started, and that file had already reclaimed on that basis once and run beside a
  live campaign (2026-08-20), so it refused for good. Its own comment said it had nothing better:
  "process groups do not survive the shells this runs under".

## Fix

A run is one process group, and the group is what a signal kills and what the lock records.

- `scripts/tests/lib/gate_session.py` (new). A real run re-executes the runner under it. A front
  stays in the caller's group, so terminal Ctrl-C and a caller's group kill still reach it. It starts
  a session leader (`start_new_session`, i.e. setsid(2)) that exports
  `CONCRETE_GATE_SESSION=<pgid>` and runs the runner in that group. A SIGINT, SIGTERM or SIGHUP to the
  front becomes SIGTERM to the whole group. SIGTERM is used because backgrounded bash children ignore
  SIGINT. A signalled run gets a grace period (`CONCRETE_GATE_SESSION_GRACE`, default 10 s) from the
  first signal and is then killed with SIGKILL, whether or not the runner has exited: a runner that
  ignores SIGTERM cannot hold off the escalation. Once the leader exits, the front sweeps the group in
  the same way until `killpg(pgid, 0)` reports no member, and then asks `fresh.sh` to reclaim the
  lock. If the front itself dies, the leader sees its pipe from the front reach EOF and runs the same
  escalation itself. A signal that reaches the leader before the runner exists is remembered, so the
  runner is not started unsignalled. Uses only setsid(2), killpg(2) and
  `ps -A -o pid=,pgid=`, all present on Linux and macOS: no setsid(1), flock or /proc.
- `run_ci_gates_local.sh`: every run except `--plan` enters a session unless it is already inside
  one, so a runner that a gate starts stays inside the enclosing session. The session owner (its
  parent is the session leader and its pgid is the session) signals its group from the trap. No new
  gate starts after an interruption. A foreground gate now runs in the background and is waited for,
  so the trap fires at once. The session owner does not release the lock even after a normal run:
  a gate may have left a process behind, so the release is the launcher's reclaim after its sweep.
- `fresh.sh`: the owner record gains `pgid=` when the kernel confirms that the acquirer is in the
  group `CONCRETE_GATE_SESSION` names (an inherited or forged value records nothing).
  `_gate_lock_lease_state` reports `gone` only when ps produced a snapshot that contains the asker
  and in it no process is in the recorded group (or, where ps reports session ids, in the session)
  or in any group registered under `groups/`, no launch is pending, and the creator pid is dead.
  Every other state is `alive` or `unprovable`, and both refuse. The registrations are read before
  and after the ps snapshot and must agree. Every launcher that registers is itself a member of the
  lease and registers before its child starts, so once a snapshot shows the lease empty, no
  registration can follow it. `_gate_lock_reclaim_stale` makes the
  judgment and the removal under a second `mkdir` mutex (`.gate.lock.reclaim`), so two reclaimers
  cannot each judge one lock stale and then delete the lock the other has just taken. A lock with no
  recorded group (one taken outside a gate session) still needs manual recovery, as before.
- `scripts/tests/lib/run_campaign_child.py` deliberately leaves its caller's session. It now records
  `groups/pending.<pid>` in the lock it runs under before it launches, then `groups/<pgid>` once the
  group exists, and removes the entry when the group is empty. A run that holds a live campaign
  therefore never looks finished, which is the 2026-08-20 incident.
- `.githooks/pre-push` waits for the runner in the background and forwards INT/TERM/HUP to it.

Containment: the bug-244 section of the repository-lock gate. A gate session's lock records its
group, and a forged marker records none. A dead creator whose group is alive is refused, and is
reclaimed once the group is empty. A registered escaper group, a pending launch and a held reclaim
mutex each refuse. The campaign launcher registers its group and then removes it. On the real
runner in a sandbox: SIGTERM and SIGINT each make the runner exit within seconds and end every
descendant, including one that ignores SIGTERM; the run leaves no lock, and the next run runs. With
the front, the leader and the runner stopped and then killed, the surviving gate keeps the next run
refused, and once it is gone the next run reclaims the lock by itself. After a normal run whose gate
left a process behind, the lock stays held until the sweep has removed that process. The launcher
ends a run that ignores SIGTERM when it is signalled, when it is killed outright, and when the signal
arrives as the session starts. With the launcher added to main 593dc100 and nothing else changed, 19 of these legs fail.

## Structural cause

Class 16, artifacts at paths no run owns, in its process form. The run's processes and its lock had
no owner that a signal or a later run could address. The only identity recorded was the creator's
pid, which describes one process and not the work that process started, so the lock could choose
only between reclaiming unsafely and refusing for good.

## Structural owner

The gate-session launcher (`scripts/tests/lib/gate_session.py`), which makes a run one process
group, and the repository lock (`scripts/tests/lib/fresh.sh`), whose lease is that group. The
repository-lock gate enforces both. Class 16 in [STRUCTURAL_CLASSES.md](STRUCTURAL_CLASSES.md).

## Residual

A descendant that calls setsid() or setpgid() on its own leaves the group without registering, and
neither the sweep nor the reclaim can see it. In this repository only the campaign launcher does
that, and it registers. A gate run directly rather than through the runner has no session: its own
release traps still apply, but if a lock stays behind, recovery is manual. If the front, the leader
and the runner are all SIGKILLed, the gate children keep running and the lock stays held until they
exit; that is a live run, not a wedge.
