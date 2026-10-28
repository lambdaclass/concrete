#!/usr/bin/env bash
# THE REPOSITORY LOCK REFUSES RATHER THAN RECLAIMS.
#
# `_gate_lock_acquire` used to reclaim a lock whose recorded owner PID was dead. That is unsafe, and
# it fired: on 2026-08-20 the recorded creator was gone while an isolated 78-family mutation campaign
# that its shell had launched was still running, and a later gate reclaimed the lock and ran
# alongside it. Nothing was corrupted only because both parties happened to work in disposable
# copies.
#
# The recorded PID is the process that CREATED the lock. Children inherit `CONCRETE_GATE_LOCK` and
# outlive it, so a dead creator says nothing about whether the tree is free. PID liveness cannot be
# repaired into a safe signal: the creator is the wrong process to probe, PIDs are reused, and there
# is no portable way to prove a lease is gone — process groups do not survive the shells this runs
# under, and `flock` is absent on macOS.
#
# So the policy is fail-closed and this gate holds it there. A stale lock costs one command to clear;
# a wrongly reclaimed one costs a silent verdict about an artifact that never existed.
#
# TESTED IN A SANDBOX, never against the real lock: a gate that experiments on the repository's own
# lock would be doing the exact thing it exists to forbid.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
# CHECKED. Unchecked under a shell without errexit, a failed `mktemp -d` left TMP empty, making
# SB="/sandbox" — after which this gate wrote /sandbox/probe.sh and repeatedly deleted
# /sandbox/.gate.lock. A temp-directory failure must not reach into a root-level directory.
TMP="$(mktemp -d)" || { echo "FATAL: could not create a sandbox directory" >&2; exit 2; }
[ -n "$TMP" ] && [ -d "$TMP" ] || { echo "FATAL: sandbox directory is not usable" >&2; exit 2; }
trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# Captured BEFORE any probe, so the non-interference check at the end has something to compare to.
REAL_LOCK_START="absent"
[ -d "$ROOT_DIR/.gate.lock" ] && REAL_LOCK_START="present:$(cat "$ROOT_DIR/.gate.lock/owner" 2>/dev/null | tr '\n' ' ')"

LIB="scripts/tests/lib/fresh.sh"
[ -f "$LIB" ] || { echo "FATAL: $LIB missing" >&2; exit 2; }

# A sandbox tree with its own .gate.lock, so nothing here can touch the repository's.
SB="$TMP/sandbox"; mkdir -p "$SB/scripts/tests/lib"
cp "$LIB" "$SB/scripts/tests/lib/fresh.sh"
cat > "$SB/probe.sh" <<'PROBE'
#!/usr/bin/env bash
source scripts/tests/lib/fresh.sh
_gate_lock_acquire && echo ACQUIRED || echo REFUSED
PROBE

# THE ENVIRONMENT IS CONTROLLED, NOT INHERITED. This gate asserts what an UNHELD lock does, so it
# must not inherit a live `CONCRETE_GATE_LOCK` from a parent. Run under `run_ci_gates_local.sh` —
# which now holds the lock for the whole pass and exports a token this gate's children legitimately
# satisfy by ancestry — every "must be REFUSED" case returned ACQUIRED and the gate failed. Measured
# 2026-08-21. Each probe below sets the token it means to test, and this default clears it.
probe(){ ( cd "$SB" && env -u CONCRETE_GATE_LOCK bash probe.sh 2>"$TMP/err" ); }

echo "=== the four states ==="

rm -rf "$SB/.gate.lock"
[ "$(probe)" = "ACQUIRED" ] \
  && ok "a free tree acquires the lock" \
  || no "a free tree could not acquire the lock — the lock is broken shut"

# THE REGRESSION THIS GATE EXISTS FOR. A dead creator must NOT be reclaimed.
# THE HELD STATE IS CONSTRUCTED, NOT INHERITED FROM THE PROBE ABOVE.
#
# These controls used to write an owner file into the lock directory the PREVIOUS probe had left
# behind — which only existed because a gate that acquired the lock LEAKED it. When that leak was
# fixed (it was the cause of FAST-SURFACE-GATES 8/3 and weeks of red CI) the directory was gone, the
# owner write went nowhere, and these controls silently began testing a FREE tree: the dead-creator
# control then reported the lock had been "RECLAIMED" when nothing had been held at all.
# A control whose precondition is another control's bug fails the moment the bug is fixed.
mkdir -p "$SB/.gate.lock"
printf 'pid=999999\ncmd=dead_creator.sh\n' > "$SB/.gate.lock/owner"
R="$(probe)"
if [ "$R" = "REFUSED" ]; then
  ok "a lock whose creator PID is DEAD is refused, not reclaimed"
else
  no "a dead creator PID was RECLAIMED ($R) — children outlive their creator, so this hands the tree to a concurrent run"
fi
grep -q 'recover explicitly' "$TMP/err" \
  && ok "the refusal tells the operator how to recover explicitly" \
  || no "the refusal gives no recovery path — an operator will delete the lock blindly or give up"
grep -q 'creator pid 999999 is dead' "$TMP/err" \
  && ok "the refusal reports the creator's liveness as evidence rather than acting on it" \
  || no "the refusal does not report creator liveness — the operator cannot judge the situation"

# A live creator must also be refused, and for a plainer reason.
mkdir -p "$SB/.gate.lock"
printf 'pid=%s\ncmd=live.sh\n' "$$" > "$SB/.gate.lock/owner"
[ "$(probe)" = "REFUSED" ] \
  && ok "a lock whose creator is ALIVE is refused" \
  || no "a live holder was displaced — this is the interleaving the lock exists to prevent"

# Explicit recovery is the only path back.
rm -rf "$SB/.gate.lock"
[ "$(probe)" = "ACQUIRED" ] \
  && ok "explicit removal is the recovery path, and it works" \
  || no "the lock cannot be recovered even by removing it — it is broken shut"

echo ""
echo "=== re-entrancy still works ==="
# One run, one lock: a gate invoked by a runner that already holds it must not deadlock. Without
# this, making the lock fail-closed would break every nested gate invocation.
rm -rf "$SB/.gate.lock"
# THE POSITIVE CONTROL: a token that names a REAL lock held by a LIVE process must proceed. This is
# the case that must keep working, and it is measured with a genuine lock rather than an arbitrary
# value, because this test previously passed `CONCRETE_GATE_LOCK=12345` and asserted that proceeding
# was correct — enshrining a fail-OPEN path as the specification. Any nonempty value bypassed the
# lock entirely, so `CONCRETE_GATE_LOCK=1 bash check_foo.sh` ran holding nothing while every message
# implied otherwise.
mkdir -p "$SB/.gate.lock"
printf 'pid=%s\ncmd=holder.sh\n' "$$" > "$SB/.gate.lock/owner"
R="$( cd "$SB" && CONCRETE_GATE_LOCK="$$:$SB/.gate.lock" bash probe.sh 2>/dev/null )"
[ "$R" = "ACQUIRED" ] \
  && ok "a child inheriting a token that names a live, real lock proceeds without taking its own" \
  || no "re-entrancy is broken ($R) — a runner would deadlock against the gates it invokes"

# ...and that inheritance must not create a SECOND lock directory of its own.
[ -d "$SB/.gate.lock" ] && [ "$(sed -n 's/^cmd=//p' "$SB/.gate.lock/owner")" = "holder.sh" ] \
  && ok "an inheriting child creates no lock of its own and does not overwrite the holder's" \
  || no "an inheriting child disturbed the holder's lock"

echo ""
echo "=== a token that does NOT name a live lock is refused (fail-closed) ==="
# Three ways a token can be a lie, each refused separately: no such directory, a directory whose
# recorded owner disagrees with the token, and an owner that is dead.
rm -rf "$SB/.gate.lock"
R="$( cd "$SB" && CONCRETE_GATE_LOCK="12345" bash probe.sh 2>/dev/null )"
[ "$R" = "REFUSED" ] \
  && ok "a bare value naming no lock directory is refused" \
  || no "a forged token '12345' was accepted ($R) — the gate would run holding nothing"

R="$( cd "$SB" && CONCRETE_GATE_LOCK="$$:$SB/.gate.lock" bash probe.sh 2>/dev/null )"
[ "$R" = "REFUSED" ] \
  && ok "a token naming a lock directory that does not exist is refused" \
  || no "a token pointing at a missing lock was accepted ($R)"

mkdir -p "$SB/.gate.lock"; printf 'pid=999999\ncmd=other.sh\n' > "$SB/.gate.lock/owner"
R="$( cd "$SB" && CONCRETE_GATE_LOCK="$$:$SB/.gate.lock" bash probe.sh 2>/dev/null )"
[ "$R" = "REFUSED" ] \
  && ok "a token whose pid disagrees with the lock's recorded owner is refused" \
  || no "a token was accepted against a lock owned by someone else ($R)"

printf 'pid=999999\ncmd=dead.sh\n' > "$SB/.gate.lock/owner"
R="$( cd "$SB" && CONCRETE_GATE_LOCK="999999:$SB/.gate.lock" bash probe.sh 2>/dev/null )"
[ "$R" = "REFUSED" ] \
  && ok "a token whose owner pid is dead is refused" \
  || no "a stale token from a dead run was accepted ($R)"

# THE CASE THAT SEPARATES "names a lock" FROM "holds a lock". Everything a token asserts can be
# fabricated — make a directory, write a live pid into `owner` — so the property that makes
# re-entrancy legitimate is that the holder is an ANCESTOR of this process. Here the owner is alive
# and the directory is real, and it must STILL be refused, because that process is a sibling.
if [ -r /proc/self/stat ]; then
  ( exec sleep 60 ) & SIBLING=$!
  printf 'pid=%s\ncmd=sibling.sh\n' "$SIBLING" > "$SB/.gate.lock/owner"
  R="$( cd "$SB" && CONCRETE_GATE_LOCK="$SIBLING:$SB/.gate.lock" bash probe.sh 2>/dev/null )"
  [ "$R" = "REFUSED" ] \
    && ok "a token naming a LIVE process that is not an ancestor is refused" \
    || no "a fabricated token naming an unrelated live process was accepted ($R) — any caller could forge one"
  kill "$SIBLING" 2>/dev/null; wait "$SIBLING" 2>/dev/null || true
else
  # NOT counted as a pass. Reporting a skip through `ok` produced the same 15/0 total whether or not
  # the ancestry property was ever tested, which is exactly a vacuous control.
  echo "  SKIP ancestry check: no /proc on this platform; the weaker token checks apply there"
fi
rm -rf "$SB/.gate.lock"

echo ""
echo "=== release is creator-only ==="
# A release by a non-creator would hand the tree to a concurrent run — the same failure as reclaiming
# on a dead PID, which this library refuses to do.
mkdir -p "$SB/.gate.lock"; printf 'pid=999999\ncmd=other.sh\n' > "$SB/.gate.lock/owner"
( cd "$SB" && CONCRETE_GATE_LOCK="999999:$SB/.gate.lock" bash -c '. "'"$ROOT_DIR"'/scripts/tests/lib/fresh.sh"; _gate_lock_release' ) >/dev/null 2>&1
[ -d "$SB/.gate.lock" ] \
  && ok "a non-creator cannot release another run's lock" \
  || no "a non-creator released the lock — a concurrent run would be handed the tree"
rm -rf "$SB/.gate.lock"

# THE PAIRED POSITIVE CONTROL. Without it this section only ever asserted that release does NOT
# happen, so replacing `_gate_lock_release` with a no-op would leave this gate fully green — a
# refusal-only control that cannot distinguish "correctly declined" from "does nothing at all".
# The creator acquires and releases in one process, and the lock must be GONE afterwards.
( cd "$SB" && env -u CONCRETE_GATE_LOCK bash -c '. scripts/tests/lib/fresh.sh; _gate_lock_acquire && _gate_lock_release' ) >/dev/null 2>&1
[ ! -d "$SB/.gate.lock" ] \
  && ok "a creator DOES release its own lock (the control that makes the refusal meaningful)" \
  || no "the creator did not release its lock — every run would wedge the repository for the next one"
rm -rf "$SB/.gate.lock"

echo ""
echo "=== the lock's LIFETIME is the run's lifetime ==="
# THE REGRESSION THIS SECTION EXISTS FOR. Release used to be entirely explicit, on the stated theory
# that "gates set their own trap". MEASURED: 137 of the 167 gates that take this lock install no EXIT
# trap at all, and 29 more install one AFTER acquiring — which would silently replace a release armed
# at acquisition. So the first gate in a run that actually rebuilt kept the lock forever, every later
# lock-taking gate refused, and aggregators counted each refusal as a FAILURE: FAST-SURFACE-GATES 8/3
# and weeks of red CI, produced entirely by the leak.
#
# Every probe below runs in the sandbox tree, never against the repository's own lock.
mkdir -p "$SB"
_mkprobe() { printf '%s\n' "$2" > "$SB/$1"; }
_held()   { [ -d "$SB/.gate.lock" ]; }
_clear()  { rm -rf "$SB/.gate.lock"; }
_run()    { ( cd "$SB" && env -u CONCRETE_GATE_LOCK bash "$1" >"$TMP/out" 2>"$TMP/err" ); }

_mkprobe ok.sh 'source scripts/tests/lib/fresh.sh
_gate_lock_acquire || exit 9
exit 0'
_clear; _run ok.sh; rc=$?
{ [ "$rc" = "0" ] && ! _held; } \
  && ok "a gate that acquires and SUCCEEDS leaves no lock" \
  || no "lock survived a successful gate (rc=$rc held=$(_held && echo yes || echo no))"

_mkprobe bad.sh 'source scripts/tests/lib/fresh.sh
_gate_lock_acquire || exit 9
exit 3'
_clear; _run bad.sh; rc=$?
{ [ "$rc" = "3" ] && ! _held; } \
  && ok "a gate that acquires and FAILS leaves no lock, and keeps its exit code" \
  || no "lock survived a failing gate, or its exit code changed (rc=$rc)"

# TWO SEQUENTIAL LOCK-TAKING GATES. This is the aggregate failure in miniature: under the leak the
# second could never acquire.
_clear; _run ok.sh; a=$?; _run ok.sh; b=$?
{ [ "$a" = "0" ] && [ "$b" = "0" ] && ! _held; } \
  && ok "two lock-taking gates run in sequence and BOTH acquire" \
  || no "the second sequential gate could not acquire (first=$a second=$b)"

# A GATE'S OWN CLEANUP IS PRESERVED, whether it is installed before or after the acquisition.
_mkprobe trap_before.sh 'source scripts/tests/lib/fresh.sh
trap "echo CLEANUP > cleanup.marker" EXIT
_gate_lock_acquire || exit 9
exit 0'
_clear; rm -f "$SB/cleanup.marker"; _run trap_before.sh
{ [ -f "$SB/cleanup.marker" ] && ! _held; } \
  && ok "a cleanup trap installed BEFORE the acquisition still runs, and the lock is released" \
  || no "trap-before-acquire: cleanup=$([ -f "$SB/cleanup.marker" ] && echo ran || echo lost) lock=$(_held && echo held || echo free)"

# THE 29-GATE CASE, and the one a release armed at acquisition would have silently lost.
_mkprobe trap_after.sh 'source scripts/tests/lib/fresh.sh
_gate_lock_acquire || exit 9
trap "echo CLEANUP > cleanup.marker" EXIT
exit 0'
_clear; rm -f "$SB/cleanup.marker"; _run trap_after.sh
{ [ -f "$SB/cleanup.marker" ] && ! _held; } \
  && ok "a cleanup trap installed AFTER the acquisition runs AND does not clobber the release" \
  || no "trap-after-acquire: cleanup=$([ -f "$SB/cleanup.marker" ] && echo ran || echo lost) lock=$(_held && echo HELD || echo free)"

# A RESET DROPS THE GATE'S CLEANUP, NOT THE REPOSITORY LOCK.
_mkprobe trap_reset.sh 'source scripts/tests/lib/fresh.sh
_gate_lock_acquire || exit 9
trap "echo CLEANUP > cleanup.marker" EXIT
trap - EXIT
exit 0'
_clear; rm -f "$SB/cleanup.marker"; _run trap_reset.sh
{ [ ! -f "$SB/cleanup.marker" ] && ! _held; } \
  && ok "'trap - EXIT' drops the gate's own cleanup but still releases the lock" \
  || no "trap reset: cleanup=$([ -f "$SB/cleanup.marker" ] && echo ran || echo dropped) lock=$(_held && echo HELD || echo free)"

# TERMINATING SIGNALS. SIGKILL is excluded on purpose and asserted below.
# `sleep 60 & wait` RATHER THAN `sleep 60`. Bash defers a trapped signal until the current
# FOREGROUND command finishes, so a plain `sleep 60` would swallow SIGTERM for a full minute and the
# control would time out instead of observing the release. Waiting on a background child lets the
# trap run at once.
_mkprobe hold.sh 'source scripts/tests/lib/fresh.sh
_gate_lock_acquire || exit 9
echo ready > ready.marker
sleep 60 &
wait $!'
for sig in TERM INT HUP; do
  _clear; rm -f "$SB/ready.marker"
  # `exec` so $! is the gate process itself. Without it the pid is the wrapping subshell, and a
  # signal sent there never reaches the shell that holds the lock — SIGTERM and SIGHUP appeared not
  # to release while SIGINT did, purely because INT reached the group and the others did not.
  ( cd "$SB" && exec env -u CONCRETE_GATE_LOCK bash hold.sh >/dev/null 2>&1 ) & pid=$!
  for _ in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15; do [ -f "$SB/ready.marker" ] && break; sleep 0.2; done
  if [ ! -f "$SB/ready.marker" ]; then
    no "SIG$sig: the probe never acquired, so this control proves nothing"
    kill -9 "$pid" 2>/dev/null; wait "$pid" 2>/dev/null
  else
    kill -"$sig" "$pid" 2>/dev/null; wait "$pid" 2>/dev/null
    _held && no "SIG$sig did not release the lock" || ok "SIG$sig releases the lock"
  fi
done

# THE STATED LIMIT, ASSERTED so that a change to it is noticed rather than assumed.
_clear; rm -f "$SB/ready.marker"
# `exec` so $! is the gate process itself. Without it the pid is the wrapping subshell, and a
  # signal sent there never reaches the shell that holds the lock — SIGTERM and SIGHUP appeared not
  # to release while SIGINT did, purely because INT reached the group and the others did not.
  ( cd "$SB" && exec env -u CONCRETE_GATE_LOCK bash hold.sh >/dev/null 2>&1 ) & pid=$!
for _ in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15; do [ -f "$SB/ready.marker" ] && break; sleep 0.2; done
if [ -f "$SB/ready.marker" ]; then
  kill -9 "$pid" 2>/dev/null; wait "$pid" 2>/dev/null
  _held \
    && ok "SIGKILL leaves the lock — untrappable by anyone, so recovery stays an operator action" \
    || no "SIGKILL released the lock, which no trap can do: the lifetime mechanism is not what it claims"
else
  no "the SIGKILL limit control never acquired, so it proves nothing"
fi
_clear

# RE-ENTRANCY: a child must not release the lock its parent is still working under.
_mkprobe child.sh 'source scripts/tests/lib/fresh.sh
_gate_lock_acquire || exit 9
exit 0'
_mkprobe parent.sh 'source scripts/tests/lib/fresh.sh
_gate_lock_acquire || exit 9
bash child.sh || exit 8
[ -d .gate.lock ] && echo PARENT_STILL_HOLDS > parent.marker || echo PARENT_LOST_LOCK > parent.marker
exit 0'
_clear; rm -f "$SB/parent.marker"; _run parent.sh
m="$(cat "$SB/parent.marker" 2>/dev/null)"
[ "$m" = "PARENT_STILL_HOLDS" ] \
  && ok "a re-entrant child does NOT release its parent's lock" \
  || no "the child released the parent's lock (marker=${m:-none}) — that hands the tree to a concurrent run"
! _held \
  && ok "...and the parent's own exit does release it" \
  || no "the parent kept the lock after exiting"
_clear

# POSITIVE CONTROL FOR THE WHOLE SECTION: the lock must still EXCLUDE. If releasing on exit had
# quietly turned the lock into a no-op, every control above would pass and the mechanism would be
# gone. A live, non-ancestor holder must still refuse an independent run.
_clear; rm -f "$SB/ready.marker"
# `exec` so $! is the gate process itself. Without it the pid is the wrapping subshell, and a
  # signal sent there never reaches the shell that holds the lock — SIGTERM and SIGHUP appeared not
  # to release while SIGINT did, purely because INT reached the group and the others did not.
  ( cd "$SB" && exec env -u CONCRETE_GATE_LOCK bash hold.sh >/dev/null 2>&1 ) & pid=$!
for _ in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15; do [ -f "$SB/ready.marker" ] && break; sleep 0.2; done
if [ -f "$SB/ready.marker" ]; then
  _run ok.sh; rc=$?
  [ "$rc" != "0" ] \
    && ok "CONTROL: an independent run is still REFUSED while a live holder has the lock" \
    || no "an independent run acquired while another process held the lock — the lock excludes nothing"
  kill -9 "$pid" 2>/dev/null; wait "$pid" 2>/dev/null
else
  no "the exclusion control never acquired, so it proves nothing"
fi
_clear

# THE DELIBERATE-HOLD OPT-OUT, EXERCISED IN BOTH DIRECTIONS.
#
# The mutation supervisor decides release by its own rule: a campaign group that is not proven empty
# must KEEP the lock. An unconditional release composed into its EXIT trap would reverse that — and
# did, until `_GATE_LOCK_SELF_MANAGED` was added. Both directions are asserted here, because an
# opt-out nobody tests is an opt-out that silently becomes universal.
_mkprobe selfmanaged.sh '_GATE_LOCK_SELF_MANAGED=1
source scripts/tests/lib/fresh.sh
_gate_lock_acquire || exit 9
trap "echo OWN_CLEANUP > cleanup.marker" EXIT
exit 0'
_clear; rm -f "$SB/cleanup.marker"; _run selfmanaged.sh
{ [ -f "$SB/cleanup.marker" ] && _held; } \
  && ok "a self-managed holder keeps its lock on exit, and its own cleanup still runs" \
  || no "self-managed: cleanup=$([ -f "$SB/cleanup.marker" ] && echo ran || echo lost) lock=$(_held && echo held || echo RELEASED-against-its-decision)"
# ...and it can still release explicitly, which is how the supervisor releases when it decides to.
_mkprobe selfmanaged_rel.sh '_GATE_LOCK_SELF_MANAGED=1
source scripts/tests/lib/fresh.sh
_gate_lock_acquire || exit 9
_gate_lock_release
exit 0'
_clear; _run selfmanaged_rel.sh
! _held \
  && ok "...and an explicit release by a self-managed holder still works" \
  || no "a self-managed holder could not release explicitly"
# The opt-out must not leak into ordinary gates: the default is still automatic release.
_clear; _run ok.sh
! _held \
  && ok "CONTROL: without the opt-out, the default is still automatic release" \
  || no "the opt-out leaked into ordinary gates — every gate would strand its lock again"
_clear

# THE FIVE DEFECTS THE ADVERSARIAL REVIEW FOUND IN THE FIRST VERSION OF THIS LIFETIME.
# Each is asserted behaviourally, because each was real and reproduced before it was fixed.

# A SUBSHELL IS NOT THE CREATOR. `$$` is identical inside `( ... )` and `_GATE_LOCK_CREATED` is
# inherited, so a subshell that merely installed a trap became a "creator" and deleted its PARENT's
# lock on leaving. Reproduced: this printed PARENT_LOST_LOCK.
_mkprobe subshell.sh 'source scripts/tests/lib/fresh.sh
_gate_lock_acquire || exit 9
( trap "true" EXIT; true )
[ -d .gate.lock ] && echo PARENT_STILL_HOLDS > sub.marker || echo PARENT_LOST_LOCK > sub.marker
exit 0'
_clear; rm -f "$SB/sub.marker"; _run subshell.sh
[ "$(cat "$SB/sub.marker" 2>/dev/null)" = "PARENT_STILL_HOLDS" ] \
  && ok "a subshell that installs a trap does NOT release its parent's lock" \
  || no "a subshell released the parent's lock ($(cat "$SB/sub.marker" 2>/dev/null))"
_clear

# AN APOSTROPHE IN A CLEANUP. `trap -p` prints a SHELL-QUOTED action; recovering it by stripping the
# outer quotes left an embedded '\'' malformed, so both the cleanup and the release could be lost
# while the gate still exited 0.
_mkprobe apostrophe.sh 'source scripts/tests/lib/fresh.sh
trap "echo ok > apos.marker # it'"'"'s fine" EXIT
_gate_lock_acquire || exit 9
exit 0'
_clear; rm -f "$SB/apos.marker"; _run apostrophe.sh
{ [ -f "$SB/apos.marker" ] && ! _held; } \
  && ok "a cleanup containing an apostrophe survives composition, and the lock is released" \
  || no "apostrophe cleanup: marker=$([ -f "$SB/apos.marker" ] && echo ran || echo LOST) lock=$(_held && echo HELD || echo free)"

# SIGTERM AND TERM NAME THE SAME SIGNAL. Matching only the bare form let `trap x SIGTERM` replace
# the release outright.
_mkprobe sigform.sh 'source scripts/tests/lib/fresh.sh
_gate_lock_acquire || exit 9
trap "echo T > sig.marker" SIGTERM
builtin trap -p SIGTERM > sigform.out
exit 0'
_clear; _run sigform.sh
grep -q '_gate_lock_release_auto' "$SB/sigform.out" 2>/dev/null \
  && ok "'trap ... SIGTERM' composes with the release, like the bare TERM form" \
  || no "the SIG-prefixed form replaced the release outright"
_clear

# A FAILED RELEASE IS NOT A RELEASE. The snippet discarded the library's own warning and forced
# success, so a gate could exit 0 having stranded the lock silently.
_mkprobe relfail.sh 'source scripts/tests/lib/fresh.sh
_gate_lock_acquire || exit 9
chmod 500 . 2>/dev/null
exit 0'
_clear; _run relfail.sh
chmod 700 "$SB" 2>/dev/null
# Matched on the MACHINE-READABLE marker, not on prose: the human sentence was reworded to
# "could NOT be released" and this control went red on the capitalisation alone.
if grep -q 'lock-not-released' "$TMP/err"; then
  ok "a release that FAILS emits its machine-readable postcondition marker"
else
  no "a failed automatic release was silent — a gate can exit 0 with the lock stranded"
fi
_clear

# THE SELF-MANAGED HOLDER KEEPS ITS LOCK ON A SIGNAL. This is the CI runner's fail-closed policy:
# its gates run as backgrounded children, so releasing on TERM would hand the tree to a new run
# while they are still building. Composition would have released on exactly that signal.
_mkprobe smsignal.sh '_GATE_LOCK_SELF_MANAGED=1
source scripts/tests/lib/fresh.sh
_gate_lock_acquire || exit 9
trap "RUN_INTERRUPTED=1" INT TERM HUP
echo ready > ready.marker
sleep 60 &
wait $!'
_clear; rm -f "$SB/ready.marker"
( cd "$SB" && exec env -u CONCRETE_GATE_LOCK bash smsignal.sh >/dev/null 2>&1 ) & pid=$!
for _ in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15; do [ -f "$SB/ready.marker" ] && break; sleep 0.2; done
if [ -f "$SB/ready.marker" ]; then
  kill -TERM "$pid" 2>/dev/null; wait "$pid" 2>/dev/null
  _held \
    && ok "a self-managed holder KEEPS its lock through SIGTERM (the runner's fail-closed policy)" \
    || no "SIGTERM released a self-managed holder's lock — the interrupted-run policy is defeated"
else
  no "the self-managed signal control never acquired, so it proves nothing"
fi
_clear

# A TERMINATING SIGNAL MUST TERMINATE, NOT JUST UNLOCK. Composing only a release meant the handler
# released the lock and execution CONTINUED — the gate kept working with no lock at all and then
# exited 0, which is worse than stranding it.
_mkprobe sigterm.sh 'source scripts/tests/lib/fresh.sh
_gate_lock_acquire || exit 9
echo ready > ready.marker
sleep 60 & wait $!
echo CONTINUED > continued.marker'
_clear; rm -f "$SB/ready.marker" "$SB/continued.marker"
( cd "$SB" && exec env -u CONCRETE_GATE_LOCK bash sigterm.sh >/dev/null 2>&1 ) & pid=$!
for _ in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15; do [ -f "$SB/ready.marker" ] && break; sleep 0.2; done
if [ -f "$SB/ready.marker" ]; then
  kill -TERM "$pid" 2>/dev/null; wait "$pid" 2>/dev/null; _rc=$?
  { [ "$_rc" = "143" ] && ! _held && [ ! -f "$SB/continued.marker" ]; } \
    && ok "SIGTERM releases, does not continue running unlocked, and exits 128+15" \
    || no "SIGTERM: exit=$_rc lock=$(_held && echo HELD || echo free) continued=$([ -f "$SB/continued.marker" ] && echo YES || echo no)"
else
  no "the SIGTERM termination control never acquired, so it proves nothing"
fi
_clear

# A FAILED RELEASE MUST REACH THE EXIT STATUS. Returning non-zero from an EXIT trap does not change
# the status bash already chose, so the gate exited 0 with the lock stranded and only a warning.
_mkprobe relfail2.sh 'source scripts/tests/lib/fresh.sh
_gate_lock_acquire || exit 9
chmod 500 . 2>/dev/null
exit 0'
_clear; _run relfail2.sh; _rc=$?
chmod 700 "$SB" 2>/dev/null
[ "$_rc" != "0" ] \
  && ok "a gate whose release FAILS exits nonzero instead of reporting success" \
  || no "a stranded lock still produced exit 0 — the postcondition is invisible to every consumer"
_clear

# THE macOS SHELL HAS NO BASHPID. Falling back to `$$` silently reinstated the subshell bug there,
# because `$$` is shared with every subshell. The fallback is exercised directly.
_mkprobe nobashpid.sh 'unset BASHPID
source scripts/tests/lib/fresh.sh
_gate_lock_acquire || exit 9
( trap "true" EXIT; true )
[ -d .gate.lock ] && echo PARENT_STILL_HOLDS > nb.marker || echo PARENT_LOST_LOCK > nb.marker
exit 0'
_clear; rm -f "$SB/nb.marker"; _run nobashpid.sh
[ "$(cat "$SB/nb.marker" 2>/dev/null)" = "PARENT_STILL_HOLDS" ] \
  && ok "without BASHPID (the macOS shell), a subshell still does not release its parent's lock" \
  || no "with BASHPID unavailable a subshell released the parent's lock ($(cat "$SB/nb.marker" 2>/dev/null)) — macOS would strand or mis-release"
_clear

# THE IDENTITY MUST BE STABLE, AND WITHOUT BASHPID THE LOCK MUST STILL BE RELEASED.
#
# This is the macOS regression, and only the macOS job could show it. `sh -c 'echo $PPID'` inside
# `$( )` reports the PPID of that `sh`, which is the SUBSTITUTION'S OWN SUBSHELL — a different pid
# on every call. bash 5 optimises the fork away so it looked stable locally and a control built on
# `unset BASHPID` passed; bash 3.2 forks, the identity differed between arming and release, the
# release declined silently, and one gate held the lock for the rest of the job while nine later
# ones were refused. Stability is asserted here, not assumed.
_mkprobe idstable.sh 'unset BASHPID
source scripts/tests/lib/fresh.sh
_gate_lock_acquire || exit 9
echo "method=${_GATE_LOCK_ID_METHOD:-unset}" > id.marker
case "${_GATE_LOCK_ID_METHOD:-pid}" in
  ppid) a="$(exec sh -c "echo \$PPID")"; b="$(exec sh -c "echo \$PPID")" ;;
  *)    a="$$"; b="$$" ;;
esac
[ "$a" = "$b" ] && echo STABLE >> id.marker || echo "UNSTABLE:$a:$b" >> id.marker
exit 0'
_clear; rm -f "$SB/id.marker"; _run idstable.sh
if grep -q '^STABLE$' "$SB/id.marker" 2>/dev/null; then
  ok "without BASHPID the shell identity is stable across reads ($(head -1 "$SB/id.marker"))"
else
  no "without BASHPID the shell identity is UNSTABLE ($(tr '\n' ' ' < "$SB/id.marker" 2>/dev/null)) — arming and release would disagree and the lock would never be released"
fi
# ...and the consequence that actually matters: the lock is released.
! _held \
  && ok "without BASHPID the lock is still released on exit (the macOS failure mode)" \
  || no "without BASHPID the lock was STRANDED — this is exactly what wedged the macOS job"
_clear

# THE RUNNER'S OPT-OUT IS ASSERTED WHERE IT ACTUALLY LIVES. A synthetic probe that hardcodes
# `_GATE_LOCK_SELF_MANAGED=1` stays green if the real declaration is deleted, which would silently
# restore the premature unlock this control exists to prevent.
_runner="$ROOT_DIR/scripts/tests/run_ci_gates_local.sh"
if [ -f "$_runner" ]; then
  # `grep -q` INSIDE A PIPELINE IS A FALSE NEGATIVE UNDER `pipefail`. grep exits as soon as it
  # matches, sed is killed by SIGPIPE and reports 141, and pipefail then makes a SUCCESSFUL match
  # look like a failed pipeline — this control reported the declaration missing while it sat at line
  # 63. The stripped text is captured first and matched without a pipeline.
  _runner_code="$(sed -E 's/(^|[[:space:]])#.*$/\1/' "$_runner")"
  case "$_runner_code" in
    *_GATE_LOCK_SELF_MANAGED=1*)
      ok "run_ci_gates_local.sh really declares the self-managed opt-out its policy depends on" ;;
    *)
      no "the runner no longer declares _GATE_LOCK_SELF_MANAGED — its interrupted-run policy is silently defeated" ;;
  esac
  # ORDER IS COMPARED AGAINST THE REAL CALL, NOT A MENTION OF IT. The first version matched the word
  # `_gate_lock_acquire` inside a comment on line 42 and concluded the declaration came too late.
  _decl="$(printf '%s\n' "$_runner_code" | grep -n '_GATE_LOCK_SELF_MANAGED=1' | head -1 | cut -d: -f1)"
  _acq="$(printf '%s\n' "$_runner_code" | grep -nE '(^|[^[:alnum:]_])_gate_lock_acquire[[:space:]]*(\|\||$|&&)' | head -1 | cut -d: -f1)"
  { [ -n "$_decl" ] && [ -n "$_acq" ] && [ "$_decl" -lt "$_acq" ]; } \
    && ok "...and declares it BEFORE acquiring, which is the only point at which it can take effect" \
    || no "the runner declares the opt-out after acquiring (decl=${_decl:-none} acquire=${_acq:-none}); arming has already happened"
else
  no "run_ci_gates_local.sh is missing; its lock policy cannot be checked"
fi

# THE CORPUS INVARIANT BEHIND THE ONE LIMIT THAT REMAINS. A cleanup that calls `exit` or `exec`
# terminates the handler before the composed release runs. No gate does that today; asserting it
# keeps the limit from starting to apply silently.
_bad_traps="$(grep -lE "^[[:space:]]*trap '[^']*\\b(exit|exec)\\b[^']*' *(EXIT|INT|TERM|HUP|SIG)" \
  "$ROOT_DIR"/scripts/tests/check_*.sh 2>/dev/null | head -5)"
[ -z "$_bad_traps" ] \
  && ok "no gate's terminating-signal cleanup calls exit/exec, so none can skip the release" \
  || no "these gates' cleanups call exit/exec and would skip the composed release: $_bad_traps"

# NO NEW PLATFORM AUTHORITY. The release path must not depend on flock or /proc; macOS has neither
# flock nor /proc, and this repository has already taken an outage from a /proc dependency.
# USE, NOT MENTION. The library's header explains WHY it cannot use flock ("flock is absent on
# macOS"), so grepping for the word failed on the sentence documenting the constraint — the same
# mention-versus-invocation error just fixed in the CI-reachability gate.
if sed -E 's/(^|[[:space:]])#.*$/\1/' "$LIB" | has_match -E '(^|[^[:alnum:]_])flock([[:space:]]|$)'; then
  no "the lock library CALLS flock, which macOS does not have"
else
  ok "the lock library requires no flock (checked against code, not comments)"
fi
awk '/^_gate_lock_release\(\)/,/^}/' "$LIB" | has_match '/proc' \
  && no "the RELEASE path reads /proc — it would behave differently on macOS" \
  || ok "the release path requires no /proc"

echo ""
echo "=== a killed run cannot wedge the tree (bug 244) ==="
# THE DEFECT. Killing a gate run killed one process: its gates, the compilers they ran and the harness
# copies they worked in were reparented to init and kept going, and the lock then refused every later
# run — rightly, since a dead creator says nothing about its children — until someone deleted it by
# hand. The run is now one process group (scripts/tests/lib/gate_session.py) and the lock records
# that group, so: a signal to the runner ends every descendant; the lock is reclaimed automatically
# once the kernel reports the group empty; and it is still REFUSED while any member is alive.
#
# Every probe below runs in a sandbox. The runner sandbox is a throwaway git repository holding the
# real runner and libraries, a stub `lake` and a dummy gate; its sessions are started with the
# enclosing session's marker removed, so each is a session of its own, and each is killed here.
GS="$ROOT_DIR/scripts/tests/lib/gate_session.py"
_sbprobe() { # dir -> ACQUIRED|REFUSED, stderr in $TMP/err; never inherits a lock or a session
  ( cd "$1" && env -u CONCRETE_GATE_LOCK -u CONCRETE_GATE_SESSION bash -c \
      'source scripts/tests/lib/fresh.sh; _gate_lock_acquire && echo ACQUIRED || echo REFUSED' 2>"$TMP/err" )
}
_alive_any() { local f p; for f in "$@"; do p="$(cat "$f" 2>/dev/null)"; [ -n "$p" ] && kill -0 "$p" 2>/dev/null && return 0; done; return 1; }
_wait_dead() { local i; for i in $(seq 1 100); do _alive_any "$@" || return 0; sleep 0.2; done; return 1; }
_wait_file() { local i; for i in $(seq 1 150); do [ -s "$1" ] && return 0; sleep 0.2; done; return 1; }
_dead_pid() { sh -c 'exit 0' & local p=$!; wait "$p" 2>/dev/null; echo "$p"; }
# A live process group of its own, standing in for a run's surviving children.
_live_group() { # pidfile
  # Started from a subshell, so it is not this shell's job and its death is not announced.
  ( python3 -c 'import os,sys,time; os.setsid(); open(sys.argv[1],"w").write(str(os.getpid())); time.sleep(60)' "$1" & )
  _wait_file "$1"
}
LIVE_PIDS=""
_bug244_cleanup() { local p; for p in $LIVE_PIDS; do kill -KILL -- "-$p" 2>/dev/null; kill -KILL "$p" 2>/dev/null; done; }
trap '_bug244_cleanup; rm -rf "${TMP:?}"' EXIT

if ! command -v python3 >/dev/null 2>&1 || [ ! -f "$GS" ]; then
  no "python3 or $GS is missing; a killed run would strand its children again"
else

# --- the record: a group is written only when the kernel agrees ---
_clear
_mkprobe rec.sh 'source scripts/tests/lib/fresh.sh
_gate_lock_acquire || exit 9
cp .gate.lock/owner rec.owner'
rm -f "$SB/rec.owner"
( cd "$SB" && env -u CONCRETE_GATE_LOCK -u CONCRETE_GATE_SESSION python3 "$GS" -- bash rec.sh >/dev/null 2>&1 )
grep -q '^pgid=[0-9][0-9]*$' "$SB/rec.owner" 2>/dev/null \
  && ok "a lock taken inside a gate session records the session's process group" \
  || no "a lock taken inside a gate session records no group ($(tr '\n' ' ' < "$SB/rec.owner" 2>/dev/null)) — nothing could ever prove its run over"
rm -f "$SB/rec.owner"
( cd "$SB" && env -u CONCRETE_GATE_LOCK CONCRETE_GATE_SESSION=1 bash rec.sh >/dev/null 2>&1 )
if [ -f "$SB/rec.owner" ] && ! grep -q '^pgid=' "$SB/rec.owner"; then
  ok "a forged or inherited session marker the kernel does not confirm records no group"
else
  no "an unconfirmed CONCRETE_GATE_SESSION was recorded as the lock's group — a reclaim could follow a lie"
fi
_clear

# --- the rule: dead creator + live group refuses; empty group reclaims ---
_live_group "$TMP/g1.pid" || no "could not start a live process group for the controls"
G1="$(cat "$TMP/g1.pid" 2>/dev/null)"; LIVE_PIDS="$LIVE_PIDS $G1"
DEAD="$(_dead_pid)"
mkdir -p "$SB/.gate.lock"; printf 'pid=%s\ncmd=killed_runner.sh\npgid=%s\n' "$DEAD" "$G1" > "$SB/.gate.lock/owner"
R="$(_sbprobe "$SB")"
if [ "$R" = "REFUSED" ] && grep -q 'still alive' "$TMP/err"; then
  ok "a dead creator whose group still has a live member is REFUSED, and the refusal says why"
else
  no "a dead creator with a live group was $R — that hands the tree to a run while the old one's children work it"
fi
kill -KILL -- "-$G1" 2>/dev/null; _wait_dead "$TMP/g1.pid"
R="$(_sbprobe "$SB")"
if [ "$R" = "ACQUIRED" ] && grep -q 'reclaimed the repository lock' "$TMP/err"; then
  ok "once no member of the recorded group remains, the next run reclaims the lock with no manual step"
else
  no "a provably finished run's lock was $R — a killed run still wedges the tree"
fi
_clear

# --- a group registered by a launcher that leaves the session keeps the lock ---
_live_group "$TMP/g2.pid"; G2="$(cat "$TMP/g2.pid" 2>/dev/null)"; LIVE_PIDS="$LIVE_PIDS $G2"
mkdir -p "$SB/.gate.lock/groups"; printf 'pid=%s\ncmd=x\npgid=%s\n' "$DEAD" "$DEAD" > "$SB/.gate.lock/owner"
: > "$SB/.gate.lock/groups/$G2"
[ "$(_sbprobe "$SB")" = "REFUSED" ] \
  && ok "a live group registered under groups/ (a campaign that left the session) keeps the lock" \
  || no "a registered escaper group was ignored — the 2026-08-20 incident, reachable again"
kill -KILL -- "-$G2" 2>/dev/null; _wait_dead "$TMP/g2.pid"
rm -f "$SB/.gate.lock/groups/$G2"; : > "$SB/.gate.lock/groups/pending.$DEAD"
[ "$(_sbprobe "$SB")" = "REFUSED" ] \
  && ok "a launch registered as pending (its group never recorded) is unprovable and refused" \
  || no "a pending launch was reclaimed — its child may be running unrecorded"
_clear
mkdir -p "$SB/.gate.lock"; printf 'pid=%s\ncmd=x\npgid=%s\n' "$DEAD" "$DEAD" > "$SB/.gate.lock/owner"
mkdir -p "$SB/.gate.lock.reclaim"
[ "$(_sbprobe "$SB")" = "REFUSED" ] \
  && ok "a reclaim mutex held by someone else refuses rather than racing it" \
  || no "reclaim proceeded under another reclaimer's mutex"
rm -rf "$SB/.gate.lock.reclaim"; _clear

# --- release is serialized with reclaim, and keeps the lock for live registered work ---
# A reclaimer judges a lock under the mutex; a release must wait for it, or the lock it judged could be
# released and re-taken by another run before the reclaimer deletes "it" — the new run's lock.
_mkprobe relwait.sh 'source scripts/tests/lib/fresh.sh
_gate_lock_acquire || exit 9
echo acquired > relwait.marker
while [ ! -f relwait.go ]; do sleep 0.1; done
exit 0'
_clear; rm -f "$SB/relwait.marker" "$SB/relwait.go"
( cd "$SB" && exec env -u CONCRETE_GATE_LOCK -u CONCRETE_GATE_SESSION bash relwait.sh >/dev/null 2>&1 ) & _rp=$!
_wait_file "$SB/relwait.marker"
mkdir "$SB/.gate.lock.reclaim"; : > "$SB/relwait.go"; sleep 1.5
if [ -d "$SB/.gate.lock" ] && kill -0 "$_rp" 2>/dev/null; then
  ok "a release waits while the reclaim mutex is held, instead of removing the lock under it"
else
  no "a release removed the lock while a reclaimer held the mutex — a reclaim could then delete a new run's lock"
fi
rmdir "$SB/.gate.lock.reclaim"; wait "$_rp" 2>/dev/null
[ ! -d "$SB/.gate.lock" ] \
  && ok "...and completes once the mutex is free" \
  || no "...but the lock was never released after the mutex was freed"
_clear
_live_group "$TMP/g3.pid"; G3="$(cat "$TMP/g3.pid" 2>/dev/null)"; LIVE_PIDS="$LIVE_PIDS $G3"
_mkprobe relreg.sh 'source scripts/tests/lib/fresh.sh
_gate_lock_acquire || exit 9
mkdir -p .gate.lock/groups; : > ".gate.lock/groups/$1"
exit 0'
( cd "$SB" && env -u CONCRETE_GATE_LOCK -u CONCRETE_GATE_SESSION bash relreg.sh "$G3" >/dev/null 2>"$TMP/err" ); _rc=$?
if [ -d "$SB/.gate.lock" ] && [ "$_rc" != "0" ] && grep -q 'registered campaign work is still alive' "$TMP/err"; then
  ok "a gate exiting while a campaign group it registered is alive keeps the lock, and says so"
else
  no "the lock was released (rc=$_rc) while registered campaign work was alive"
fi
kill -KILL -- "-$G3" 2>/dev/null; _wait_dead "$TMP/g3.pid"
_clear

# --- the escaping launcher registers its group, then removes it ---
mkdir -p "$SB/.gate.lock"; printf 'pid=%s\ncmd=holder\n' "$$" > "$SB/.gate.lock/owner"
CONCRETE_GATE_LOCK="$$:$SB/.gate.lock" python3 "$ROOT_DIR/scripts/tests/lib/run_campaign_child.py" \
  --report "$TMP/launch.rep" --run-id "GL-$$" -- sh -c '
    # The child may run before the launcher records its group (the pending entry covers that
    # interval), so it waits, bounded, for the settled state before reporting it.
    i=0; while [ $i -lt 50 ]; do
      l="$(ls "$1")"
      # grep -c reads all of its input: no early exit to SIGPIPE the printf (bug 283).
      [ "$(printf "%s\n" "$l" | grep -cE "^[0-9]+$")" -gt 0 ] && [ "$(printf "%s\n" "$l" | grep -c "^pending")" -eq 0 ] && break
      i=$((i+1)); sleep 0.1
    done; ls "$1"' _ "$SB/.gate.lock/groups" > "$TMP/launch.ls" 2>/dev/null
if grep -qE '^[0-9]+$' "$TMP/launch.ls" && ! grep -q '^pending' "$TMP/launch.ls"; then
  ok "run_campaign_child.py registers the group it creates in the lock before the child works"
else
  no "run_campaign_child.py did not register its group ($(tr '\n' ' ' < "$TMP/launch.ls")) — a reclaim could not see its child"
fi
[ -z "$(ls "$SB/.gate.lock/groups" 2>/dev/null)" ] \
  && ok "...and removes the registration once its group is empty" \
  || no "run_campaign_child.py left a registration behind ($(ls "$SB/.gate.lock/groups"))"
_clear

# --- the real runner, killed ---
RSB="$TMP/runner-sb"; MARK="$TMP/mark"; mkdir -p "$RSB/scripts/tests/lib" "$RSB/.github/workflows" \
  "$RSB/.lake/build/bin" "$RSB/Concrete" "$TMP/bin" "$MARK"
cp scripts/tests/run_ci_gates_local.sh "$RSB/scripts/tests/"
cp scripts/tests/lib/*.sh scripts/tests/lib/*.py "$RSB/scripts/tests/lib/"
cp .github/workflows/lean_action_ci.yml "$RSB/.github/workflows/"
[ -f scripts/tests/ci_gate_inventory.txt ] && cp scripts/tests/ci_gate_inventory.txt "$RSB/scripts/tests/"
cp .gitignore "$RSB/"
printf '#!/bin/sh\nexit 0\n' > "$TMP/bin/lake"; chmod +x "$TMP/bin/lake"
printf '#!/bin/sh\nexit 0\n' > "$RSB/.lake/build/bin/concrete"; chmod +x "$RSB/.lake/build/bin/concrete"
printf 'def buildIdentity : String := "%s"\n' 0123456789abcdef0123456789abcdef > "$RSB/Concrete/BuildIdentity.lean"
# The long gate: a backgrounded grandchild, and one that IGNORES SIGTERM, so the escalation is tested too.
cat > "$RSB/scripts/tests/check_dummy_long.sh" <<'DUMMY'
echo $$ > "$DUMMY_MARK/gate.pid"
( sleep 60 & echo $! > "$DUMMY_MARK/child.pid"; wait ) &
bash -c 'trap "" TERM; sleep 60' & echo $! > "$DUMMY_MARK/stubborn.pid"
sleep 0.3; echo ready > "$DUMMY_MARK/ready"
wait
DUMMY
printf 'echo quick\n' > "$RSB/scripts/tests/check_dummy_quick.sh"
( cd "$RSB" && git init -q && git add -A && git -c user.email=gate@local -c user.name=gate commit -qm sandbox ) >/dev/null 2>&1
_runner() { # out first-cmd -> starts the runner in the background as a fresh top-level run; sets RPID
  ( cd "$RSB" && exec env -u CONCRETE_GATE_LOCK -u CONCRETE_GATE_SESSION PATH="$TMP/bin:$PATH" \
      CONCRETE_GATE_SESSION_GRACE=2 DUMMY_MARK="$MARK" JOBS=1 \
      bash scripts/tests/run_ci_gates_local.sh --first "$2" >"$1" 2>&1 ) &
  RPID=$!; LIVE_PIDS="$LIVE_PIDS $RPID"
}
_survivors() { echo "$MARK/gate.pid" "$MARK/child.pid" "$MARK/stubborn.pid"; }
_reset_marks() { rm -f "${MARK:?}/gate.pid" "${MARK:?}/child.pid" "${MARK:?}/stubborn.pid" "${MARK:?}/ready"; rm -rf "${RSB:?}/.gate.lock"; }

for SIG in TERM INT; do
  _reset_marks
  _runner "$TMP/run.$SIG" "bash scripts/tests/check_dummy_long.sh"
  if ! _wait_file "$MARK/ready"; then
    no "SIG$SIG: the sandbox runner never started its gate ($(tail -3 "$TMP/run.$SIG" | tr '\n' ' '))"; kill -KILL "$RPID" 2>/dev/null; continue
  fi
  grep -q '^pgid=' "$RSB/.gate.lock/owner" 2>/dev/null \
    && ok "SIG$SIG: the running runner's lock records its session group" \
    || no "SIG$SIG: the runner's lock records no group ($(tr '\n' ' ' < "$RSB/.gate.lock/owner" 2>/dev/null))"
  kill -"$SIG" "$RPID"
  # BOUNDED. A runner that waits out its gates before exiting would make every check below pass by
  # waiting for the sleeps to end, so it must be gone within seconds of the signal.
  for _ in $(seq 1 75); do kill -0 "$RPID" 2>/dev/null || break; sleep 0.2; done
  if kill -0 "$RPID" 2>/dev/null; then
    no "SIG$SIG: the runner was still running 15 s after the signal"; kill -KILL "$RPID" 2>/dev/null
  else
    ok "SIG$SIG: the runner exits promptly"
  fi
  wait "$RPID" 2>/dev/null
  # shellcheck disable=SC2046
  if _wait_dead $(_survivors); then
    ok "SIG$SIG to the runner ends every descendant, including one that ignores SIGTERM"
  else
    no "SIG$SIG to the runner left descendants alive — orphans keep working the tree"
    for f in $(_survivors); do kill -KILL "$(cat "$f" 2>/dev/null)" 2>/dev/null; done
  fi
  [ ! -d "$RSB/.gate.lock" ] \
    && ok "SIG$SIG: the killed run left no lock behind" \
    || no "SIG$SIG: the killed run left its lock ($(tr '\n' ' ' < "$RSB/.gate.lock/owner" 2>/dev/null))"
  _runner "$TMP/next.$SIG" "bash scripts/tests/check_dummy_quick.sh"; wait "$RPID" 2>/dev/null
  if grep -q 'PASS=1 FAIL=0' "$TMP/next.$SIG" && ! grep -q 'REPOSITORY BUSY' "$TMP/next.$SIG"; then
    ok "SIG$SIG: the next run acquires the lock and runs, with no manual recovery"
  else
    no "SIG$SIG: the next run did not run ($(grep -m2 -E 'BUSY|PASS=|error' "$TMP/next.$SIG" | tr '\n' ' '))"
  fi
done

# --- a NORMAL run keeps its lock until what its gates left behind is gone ---
# The session-owning runner does not release: the launcher sweeps the group and then reclaims. A gate
# that returns leaving a process behind (here one that ignores SIGTERM, so the sweep needs its grace
# period) must not have the tree handed over while that process lives.
cat > "$RSB/scripts/tests/check_dummy_leaves.sh" <<'DUMMY'
bash -c 'trap "" TERM; sleep 60' >/dev/null 2>&1 & echo $! > "$DUMMY_MARK/stubborn.pid"
echo left-behind
DUMMY
( cd "$RSB" && git add -A && git -c user.email=gate@local -c user.name=gate commit -qm leaves ) >/dev/null 2>&1
_reset_marks
( cd "$RSB" && exec env -u CONCRETE_GATE_LOCK -u CONCRETE_GATE_SESSION PATH="$TMP/bin:$PATH" \
    CONCRETE_GATE_SESSION_GRACE=4 DUMMY_MARK="$MARK" JOBS=1 \
    bash scripts/tests/run_ci_gates_local.sh --first "bash scripts/tests/check_dummy_leaves.sh" >"$TMP/run.leaves" 2>&1 ) &
RPID=$!; LIVE_PIDS="$LIVE_PIDS $RPID"
# Sampled throughout the run: whenever the left-behind process is alive, the lock must exist.
_violation=0; _seen_alive=0; _dead_before_summary=unknown
for _ in $(seq 1 300); do
  kill -0 "$RPID" 2>/dev/null || break
  if _alive_any "$MARK/stubborn.pid"; then
    _seen_alive=1; [ -d "$RSB/.gate.lock" ] || _violation=1
  fi
  if [ "$_dead_before_summary" = unknown ] && grep -q 'summary written' "$TMP/run.leaves" 2>/dev/null; then
    _alive_any "$MARK/stubborn.pid" && _dead_before_summary=no || _dead_before_summary=yes
  fi
  sleep 0.1
done
wait "$RPID" 2>/dev/null
if [ "$_seen_alive" = 1 ] && [ "$_violation" = 0 ]; then
  ok "a run's lock stays held for as long as a process its gate left behind is alive"
else
  no "the lock was released while a left-behind process lived (seen_alive=$_seen_alive violation=$_violation)"
fi
[ "$_dead_before_summary" = yes ] \
  && ok "the runner ends what its gates left behind before it writes its summary" \
  || no "the summary was written while a left-behind process was alive ($_dead_before_summary)"
grep -q 'processes_outlived_gates' "$RSB/.ci-gates-summary" 2>/dev/null \
  && ok "...and the summary records it, so the run cannot be a closure" \
  || no "the summary does not record the left-behind process ($(grep refusals "$RSB/.ci-gates-summary" 2>/dev/null))"
if ! _alive_any "$MARK/stubborn.pid" && [ ! -d "$RSB/.gate.lock" ]; then
  ok "...and once the run is over, the lock is gone with no manual step"
else
  no "after a normal run the left-behind process survived or the lock stayed ($(tr '\n' ' ' < "$RSB/.gate.lock/owner" 2>/dev/null))"
fi

# --- a verdict deferred to CI (bug 315): exit 77 is DEFERRED only from a declared gate ---
# The sandbox's check_compile_scaling.sh stands in for the real one (host_tools.sh declares it, and
# the copied workflow runs it as an unconditional step); check_dummy_77.sh exits 77 undeclared.
printf 'echo "NOT-JUDGED stub"\nexit 77\n' > "$RSB/scripts/tests/check_compile_scaling.sh"
printf 'echo undeclared\nexit 77\n' > "$RSB/scripts/tests/check_dummy_77.sh"
( cd "$RSB" && git add -A && git -c user.email=gate@local -c user.name=gate commit -qm defer ) >/dev/null 2>&1
_reset_marks
for _jobs in 1 2; do
  rm -f "$TMP/deferred.out"
  # JOBS=2 with a second, passing gate sends the deferral through the parallel pool instead.
  ( cd "$RSB" && exec env -u CONCRETE_GATE_LOCK -u CONCRETE_GATE_SESSION PATH="$TMP/bin:$PATH" \
      DUMMY_MARK="$MARK" JOBS="$_jobs" CONCRETE_GATES_DEFERRED_OUT="$TMP/deferred.out" \
      bash scripts/tests/run_ci_gates_local.sh $([ "$_jobs" = 1 ] && echo --first || echo --filter) \
        "bash scripts/tests/check_compile_scaling.sh" \
        $([ "$_jobs" = 2 ] && echo --first "bash scripts/tests/check_dummy_quick.sh") >"$TMP/run.defer" 2>&1 ); _drc=$?
  if [ "$_drc" = 0 ] && grep -q '^  DEFERRED bash scripts/tests/check_compile_scaling.sh' "$TMP/run.defer" \
     && grep -qx 'deferred=1' "$RSB/.ci-gates-summary" && grep -qx 'deferred_gates=bash scripts/tests/check_compile_scaling.sh' "$RSB/.ci-gates-summary" \
     && grep -q 'deferred_to_ci(1 not judged here)' "$RSB/.ci-gates-summary" && grep -qx 'completed=0' "$RSB/.ci-gates-summary" \
     && grep -q 'CI-GATES-LOCAL: PARTIAL' "$TMP/run.defer" \
     && [ "$(cat "$TMP/deferred.out" 2>/dev/null)" = "bash scripts/tests/check_compile_scaling.sh" ]; then
    ok "JOBS=$_jobs: exit 77 from the declared gate is DEFERRED: run exit 0, PARTIAL, deferred=1 and named in the summary and the hook's list"
  else
    no "JOBS=$_jobs: the declared deferral was not recorded as one (rc=$_drc): $(grep -E 'DEFERRED|FAIL|CI-GATES' "$TMP/run.defer" | tr '\n' ' ') / $(grep -E '^(deferred|passed|failed|refusals)' "$RSB/.ci-gates-summary" 2>/dev/null | tr '\n' ' ')"
  fi
  if grep -qx "passed=$((_jobs - 1))" "$RSB/.ci-gates-summary" && grep -qx 'failed=0' "$RSB/.ci-gates-summary"; then
    ok "JOBS=$_jobs: ...and it is not counted as passed (nor failed)"
  else
    no "JOBS=$_jobs: the deferral was counted as a pass or a failure: $(grep -E '^(passed|failed)=' "$RSB/.ci-gates-summary" | tr '\n' ' ')"
  fi
done
( cd "$RSB" && exec env -u CONCRETE_GATE_LOCK -u CONCRETE_GATE_SESSION PATH="$TMP/bin:$PATH" \
    DUMMY_MARK="$MARK" JOBS=1 bash scripts/tests/run_ci_gates_local.sh --first "bash scripts/tests/check_dummy_77.sh" >"$TMP/run.undecl" 2>&1 ); _drc=$?
if [ "$_drc" != 0 ] && grep -q 'FAIL bash scripts/tests/check_dummy_77.sh.*does not declare' "$TMP/run.undecl" \
   && grep -qx 'deferred=0' "$RSB/.ci-gates-summary" && grep -qx 'failed=1' "$RSB/.ci-gates-summary"; then
  ok "CONTROL: exit 77 from a gate host_tools.sh does not declare is a FAIL, not a deferral"
else
  no "CONTROL: an undeclared gate's exit 77 was not a failure (rc=$_drc): $(grep -E 'DEFERRED|FAIL' "$TMP/run.undecl" | tr '\n' ' ')"
fi

# --- the launcher itself, against a run that ignores SIGTERM ---
# Escalation must not wait for the run to exit: a runner that ignores SIGTERM would hold the main
# wait forever. Both directions: the front signalled, and the front killed outright (the leader is then
# the only one left to escalate). And a signal arriving as the session starts must still end it.
for _how in TERM KILL; do
  rm -f "$TMP/stub.pid"
  ( exec env -u CONCRETE_GATE_SESSION CONCRETE_GATE_SESSION_GRACE=2 python3 "$GS" -- \
      bash -c 'trap "" TERM; echo $$ > "$1"; sleep 60' _ "$TMP/stub.pid" >/dev/null 2>&1 ) &
  _fp=$!; LIVE_PIDS="$LIVE_PIDS $_fp"
  # Readiness is published only once SIGTERM is ignored, and a run that never got ready fails here:
  # without it, an absent pid file would read as "ended".
  if ! _wait_file "$TMP/stub.pid"; then
    no "launcher $_how: the stub never started; the leg proves nothing"; kill -KILL "$_fp" 2>/dev/null; continue
  fi
  kill -"$_how" "$_fp" 2>/dev/null
  # BOUNDED, or a launcher that waits out the 60 s sleep would pass by outliving it. The launcher's
  # SIGKILL comes 3 x grace + 5 s = 11 s after the signal.
  for _ in $(seq 1 100); do kill -0 "$_fp" 2>/dev/null || break; sleep 0.2; done
  if kill -0 "$_fp" 2>/dev/null; then
    no "launcher $_how: still running 20 s after the signal (grace 2 s) — the escalation never started"
    kill -KILL "$_fp" 2>/dev/null
  fi
  wait "$_fp" 2>/dev/null
  if _wait_dead "$TMP/stub.pid"; then
    ok "launcher $_how: a run that ignores SIGTERM is still ended (escalation does not wait on it)"
  else
    no "launcher $_how: a run that ignores SIGTERM survived — the escalation never started"
    kill -KILL "$(cat "$TMP/stub.pid" 2>/dev/null)" 2>/dev/null
  fi
done

# A signal inside the startup window: the leader has installed its handlers and not yet started the
# run. The window is held open by the launcher's test hook, and the signal is sent only once the
# leader says it is inside it; a lost signal would let the run start when the delay ends.
rm -f "$TMP/stub.pid" "$TMP/start.err"
( exec env -u CONCRETE_GATE_SESSION CONCRETE_GATE_SESSION_STARTUP_DELAY=6 python3 "$GS" -- \
    bash -c 'echo $$ > "$1"; sleep 60' _ "$TMP/stub.pid" >/dev/null 2>"$TMP/start.err" ) &
_fp=$!; LIVE_PIDS="$LIVE_PIDS $_fp"
for _ in $(seq 1 100); do grep -q 'waiting before launch' "$TMP/start.err" 2>/dev/null && break; sleep 0.1; done
if grep -q 'waiting before launch' "$TMP/start.err" 2>/dev/null; then
  kill -TERM "$_fp" 2>/dev/null
  for _ in $(seq 1 50); do kill -0 "$_fp" 2>/dev/null || break; sleep 0.2; done
  kill -0 "$_fp" 2>/dev/null && kill -KILL "$_fp" 2>/dev/null
  wait "$_fp" 2>/dev/null
  sleep 7
  [ ! -s "$TMP/stub.pid" ] \
    && ok "a signal inside the startup window is remembered: the run never starts" \
    || { no "a signal inside the startup window was absorbed, and the run started anyway"; kill -KILL "$(cat "$TMP/stub.pid")" 2>/dev/null; }
else
  no "the startup-window control never reached its window ($(tr '\n' ' ' < "$TMP/start.err"))"
  kill -KILL "$_fp" 2>/dev/null
fi

# A campaign group that left the session is cancelled with the session. The launcher reads the lock
# beside its own copy, so this uses the runner sandbox's copy and lock.
rm -f "$MARK/camp.pid"; rm -rf "${RSB:?}/.gate.lock"
( cd "$RSB" && exec env -u CONCRETE_GATE_LOCK -u CONCRETE_GATE_SESSION CONCRETE_GATE_SESSION_GRACE=2 \
    python3 scripts/tests/lib/gate_session.py -- bash -c '
      source scripts/tests/lib/fresh.sh; _gate_lock_acquire || exit 9
      python3 scripts/tests/lib/run_campaign_child.py --report "$1/camp.rep" --run-id camp -- \
        bash -c "echo \$\$ > \"$1/camp.pid\"; trap \"\" TERM; sleep 60"' _ "$MARK" >/dev/null 2>&1 ) &
_fp=$!; LIVE_PIDS="$LIVE_PIDS $_fp"
if _wait_file "$MARK/camp.pid"; then
  LIVE_PIDS="$LIVE_PIDS $(cat "$MARK/camp.pid")"
  kill -TERM "$_fp" 2>/dev/null
  for _ in $(seq 1 100); do kill -0 "$_fp" 2>/dev/null || break; sleep 0.2; done
  wait "$_fp" 2>/dev/null
  if _wait_dead "$MARK/camp.pid"; then
    ok "cancelling a run also ends the campaign group it registered (which left the session)"
  else
    no "a registered campaign group survived the cancelled run"; kill -KILL "$(cat "$MARK/camp.pid")" 2>/dev/null
  fi
  [ ! -d "$RSB/.gate.lock" ] \
    && ok "...and with it gone, the run's lock is reclaimed" \
    || no "...but the lock stayed after the campaign group was gone ($(ls "$RSB/.gate.lock/groups" 2>/dev/null | tr '\n' ' '))"
else
  no "the campaign-cancel control never started its campaign child"
fi

# --- the safety property, on the real runner: inherited children alive => refuse ---
# Stopping the launcher, the session leader and the runner before killing them removes everyone who
# would clean up — the state a `kill -9` of the whole parent chain leaves: the creator is dead and the
# gate it launched is still working the tree.
_reset_marks
_runner "$TMP/run.kill9" "bash scripts/tests/check_dummy_long.sh"
if _wait_file "$MARK/ready"; then
  RPG="$(sed -n 's/^pgid=//p' "$RSB/.gate.lock/owner" 2>/dev/null)"; ROWN="$(sed -n 's/^pid=//p' "$RSB/.gate.lock/owner" 2>/dev/null)"
  LIVE_PIDS="$LIVE_PIDS $RPG"
  kill -STOP "$RPID" "$RPG" "$ROWN" 2>/dev/null
  kill -KILL "$RPID" "$RPG" "$ROWN" 2>/dev/null; wait "$RPID" 2>/dev/null
  sleep 0.5
  if _alive_any "$MARK/gate.pid" && ! kill -0 "$ROWN" 2>/dev/null; then
    R="$(_sbprobe "$RSB")"
    if [ "$R" = "REFUSED" ] && grep -q 'still alive' "$TMP/err"; then
      ok "creator SIGKILLed with its gate still running: the next run is REFUSED"
    else
      no "creator dead but its gate alive: the next run was $R, not refused for the live session — a reclaim here hands over a worked tree"
    fi
    kill -KILL -- "-$RPG" 2>/dev/null
    # shellcheck disable=SC2046
    _wait_dead $(_survivors) || no "the stranded group could not be killed for the control"
    R="$(_sbprobe "$RSB")"
    [ "$R" = "ACQUIRED" ] \
      && ok "...and once the last member is gone, the next run reclaims the lock by itself" \
      || no "...but after the last member exited the lock still refused ($R) — a wedge"
  else
    no "the SIGKILL control did not reach its state (gate alive, creator dead); it proves nothing"
  fi
else
  no "the SIGKILL control's runner never started its gate"
fi
_clear

fi

echo "=== the real repository lock was not touched ==="
# COMPARED AGAINST A SNAPSHOT TAKEN BEFORE ANY PROBE RAN. Both branches of this used to call `ok`, so
# the assertion could not fail — it would have reported success even if this gate had deleted or
# replaced the repository's own lock, which matters because the release probe deliberately sources the
# real library. An assertion with no failing branch is decoration.
REAL_LOCK_END="absent"
[ -d "$ROOT_DIR/.gate.lock" ] && REAL_LOCK_END="present:$(cat "$ROOT_DIR/.gate.lock/owner" 2>/dev/null | tr '\n' ' ')"
if [ "$REAL_LOCK_END" = "$REAL_LOCK_START" ]; then
  ok "the repository lock is exactly as this gate found it ($REAL_LOCK_END)"
else
  no "this gate CHANGED the repository lock: was [$REAL_LOCK_START], now [$REAL_LOCK_END]"
fi

echo ""
echo "GATE-LOCK: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
