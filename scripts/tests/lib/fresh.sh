#!/usr/bin/env bash
# Stale-compiler-artifact guard.
#
# Every gate that runs the compiler tests whatever binary happens to be on disk. When that binary
# predates the checked-out source, the gate still runs, still prints assertions, and still reports
# PASS/FAIL -- about a compiler that no longer exists. That is worse than an ordinary failure: a
# failure says "something is wrong", while a stale run produces plausible evidence for the wrong
# artifact.
#
# This happened four times in one session: `git checkout <branch>` without `lake build`, then
# reading the previous branch's behaviour as the current branch's. Once a shipped feature looked
# unimplemented; once three "parse errors" were about to be written up as a finding. Neither was
# real.
#
# `make` already protects the normal path -- the test targets depend on `build`. This protects the
# path used while iterating: `bash scripts/tests/check_foo.sh` run directly.
#
# MECHANISM: build, don't guess. An earlier version of this file compared mtimes, and measurement
# killed it: `lake build` completes successfully WITHOUT relinking the binary when the compiled
# output is unchanged, so a source file touched at 23:01 sits "newer" than a 22:53 binary that is
# in fact current. The guard fired permanently, and its own advice ("run lake build") could not
# clear it. A content fingerprint has the same shape of problem -- it needs a build-side hook to
# record what the binary was built FROM, which does not exist today.
#
# Delegating to lake sidesteps both: lake already knows whether the binary corresponds to the
# source, and a no-op build costs ~130ms (measured), which is affordable at the top of a gate.

_FRESH_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"

# ---------------------------------------------------------------------------
# EXCLUSIVE REPOSITORY LOCK.
#
# Gates share one `.lake` tree, one compiler binary, and one working tree. Two running at once is
# not slow, it is WRONG: `require_fresh_binary` rebuilds, so a second gate can read a half-relinked
# binary or a half-written olean and produce a confident verdict about an artifact that never
# existed. This has happened repeatedly in this repository — a 24-failure run that was pure build
# interleaving, a review whose gate aborted mid-measurement, and a "reconnaissance only" full-gate
# run — and every time the response was an intention to be more careful. Intentions have now failed
# often enough to be treated as a defect in the harness rather than in the operator.
#
# MECHANISM: `mkdir`, not `flock`. `mkdir` is atomic on every POSIX filesystem and exists
# everywhere; `flock` is util-linux and absent from stock macOS, which is an active CI platform —
# the exact portability trap that made receipt issuance refuse there. A lock that cannot be taken on
# one supported platform is not a lock, it is a platform-specific outage.
#
# NO EXIT TRAP, deliberately. Gates set their own `trap ... EXIT`/`ERR`, and installing one here
# would silently clobber theirs — replacing a concurrency bug with a reporting bug. Release is composed
# into the gate's own traps instead (see `trap()` below).
#
# A KILLED RUN IS RECLAIMED BY ITS PROCESS GROUP, NEVER BY ITS CREATOR'S PID (bug 244). A run started
# through scripts/tests/lib/gate_session.py is the whole of one session's process group, and the lock
# records that group (`pgid=`). The lease is over when the kernel reports no member of the group,
# nor of any group a deliberate escaper registered under `groups/` — not when the creator is gone,
# which is the unsafe rule this file once had (see "HELD" below). A lock with no recorded group is
# still recovered only by hand.
#
# RE-ENTRANT within one run: a runner that holds the lock exports `CONCRETE_GATE_LOCK`, and the
# gates it invokes inherit it rather than deadlocking against their own parent.
_GATE_LOCK_DIR="$_FRESH_ROOT/.gate.lock"

# THE GATE SESSION THIS PROCESS BELONGS TO, if any (bug 244). gate_session.py exports
# CONCRETE_GATE_SESSION=<pgid> into the group it creates. The value is believed only when the kernel
# agrees that this process is in that group, so an inherited or forged value records nothing.
_gate_lock_session_pgid() {
  local want="${CONCRETE_GATE_SESSION:-}" got
  case "$want" in ''|*[!0-9]*) return 1 ;; esac
  got="$(ps -o pgid= -p "$$" 2>/dev/null | tr -d ' ')" || return 1
  [ -n "$got" ] && [ "$got" = "$want" ] || return 1
  printf '%s' "$got"
}

# IS THE RUN THAT HOLDS A LOCK OVER? Prints `gone`, `alive:<evidence>` or `unprovable:<reason>`, and
# only `gone` permits a reclaim.
#
# `gone` means: the owner record names a session group, ps produced a snapshot of this machine (it
# contains the asking process), and no process in that snapshot belongs to that group — nor, where ps
# reports session ids (Linux), to that session — nor to any group registered under `groups/` by a
# launcher that deliberately leaves the session (run_campaign_child.py), and the creator pid is not
# alive. Everything that cannot be shown is `unprovable`, never `gone`: no group recorded (a lock taken
# outside a gate session), a launch registered as pending (its group was never recorded), an unreadable
# owner, a failed ps. A reused pid or pgid can only make a finished run look alive, which refuses.
_gate_lock_lease_state() {
  local dir="$1" owner pid pg groups="" g ps_out n live=""
  [ -f "$dir/owner" ] || { echo "unprovable:no-owner-record"; return 0; }
  owner="$(cat "$dir/owner" 2>/dev/null)" || { echo "unprovable:owner-unreadable"; return 0; }
  pid="$(printf '%s\n' "$owner" | sed -n 's/^pid=//p' | head -1)"
  pg="$(printf '%s\n' "$owner" | sed -n 's/^pgid=//p' | head -1)"
  case "$pid" in ''|*[!0-9]*) echo "unprovable:no-creator-pid"; return 0 ;; esac
  case "$pg" in ''|*[!0-9]*) echo "unprovable:no-session-recorded"; return 0 ;; esac
  [ "$pg" -gt 1 ] || { echo "unprovable:bad-session($pg)"; return 0; }
  # REGISTRATIONS ARE READ ON BOTH SIDES OF THE SNAPSHOT, and must agree. A registration is written by
  # a launcher that is itself a member of the lease (the session, or a group registered earlier) and
  # before its child starts. So once one snapshot shows every lease group empty, no registration can
  # be written afterwards — and one written between the first read and the snapshot, by a launcher
  # that has since died, appears in the second read. Equal reads around an all-empty snapshot are
  # therefore the complete lease; unequal reads are refused, and a later attempt judges again.
  groups="$(_gate_lock_registered "$dir")"
  case "$groups" in unprovable:*) echo "$groups"; return 0 ;; esac
  ps_out="$(ps -A -o pid=,pgid=,sid= 2>/dev/null)" || ps_out="$(ps -A -o pid=,pgid= 2>/dev/null)" \
    || { echo "unprovable:ps-failed"; return 0; }
  printf '%s\n' "$ps_out" | awk -v me="$$" '$1 == me { f = 1 } END { exit f ? 0 : 1 }' \
    || { echo "unprovable:ps-snapshot-incomplete"; return 0; }
  for g in $pg $groups; do
    n="$(printf '%s\n' "$ps_out" | awk -v g="$g" '{ for (i = 2; i <= NF; i++) if ($i == g) { c++; break } } END { print c + 0 }')"
    [ "$n" = "0" ] || live="$live group $g has $n live member(s);"
  done
  kill -0 "$pid" 2>/dev/null && live="$live creator $pid is alive;"
  [ -n "$live" ] || [ "$(_gate_lock_registered "$dir")" = "$groups" ] \
    || { echo "unprovable:registrations-changed-during-the-check"; return 0; }
  if [ -n "$live" ]; then echo "alive:${live# }"; else echo "gone"; fi
}

# The groups registered under <lock>/groups by launchers that leave the session, as a sorted
# space-separated list, or `unprovable:<why>` for a pending launch or a malformed entry.
_gate_lock_registered() {
  local g out=""
  [ -d "$1/groups" ] || return 0
  for g in "$1"/groups/*; do
    [ -e "$g" ] || continue
    case "${g##*/}" in
      pending.*) echo "unprovable:escaper-launch-unrecorded(${g##*/})"; return 0 ;;
      *[!0-9]*) echo "unprovable:bad-group-entry(${g##*/})"; return 0 ;;
    esac
    out="$out ${g##*/}"
  done
  printf '%s' "${out# }"
}

# RECLAIM A LOCK WHOSE RUN IS PROVABLY OVER (bug 244), and nothing else. The judgment and the removal
# happen under a second `mkdir` mutex: without it two runs could each judge the same lock stale, and
# the later one would delete the fresh lock the earlier one had just taken. A mutex stranded by a kill
# inside this function (no children, a few milliseconds) makes reclaim refuse, with its path in the
# message; it never makes reclaim proceed.
_gate_lock_reclaim_stale() { # [quiet]
  local mutex="$_GATE_LOCK_DIR.reclaim" desc
  _GATE_LOCK_LEASE=""
  [ -d "$_GATE_LOCK_DIR" ] || return 1
  case "$_GATE_LOCK_DIR" in */.gate.lock) ;; *) return 1 ;; esac
  if ! mkdir "$mutex" 2>/dev/null; then
    _GATE_LOCK_LEASE="unprovable:another reclaim holds $mutex (if no run is starting, remove it)"
    return 1
  fi
  _GATE_LOCK_LEASE="$(_gate_lock_lease_state "$_GATE_LOCK_DIR")"
  if [ "$_GATE_LOCK_LEASE" != "gone" ]; then
    rmdir "$mutex" 2>/dev/null
    return 1
  fi
  desc="$(tr '\n' ' ' < "$_GATE_LOCK_DIR/owner" 2>/dev/null)"
  if ! rm -rf "$_GATE_LOCK_DIR" 2>/dev/null || [ -d "$_GATE_LOCK_DIR" ]; then
    rmdir "$mutex" 2>/dev/null
    _GATE_LOCK_LEASE="unprovable:could not remove the stale lock"
    return 1
  fi
  rmdir "$mutex" 2>/dev/null
  [ "${1:-}" = "quiet" ] || echo "note: reclaimed the repository lock of a finished run (${desc% }): no process of its session remains" >&2
  return 0
}

_gate_lock_acquire() {
  # Already held by an ancestor of this process: one run, one lock.
  #
  # THE INHERITED TOKEN IS VERIFIED, NOT TRUSTED. Any nonempty value used to bypass acquisition
  # outright, so `CONCRETE_GATE_LOCK=1 bash check_foo.sh` ran with NO lock held at all while every
  # message claimed one was — a fail-open authority path in the one mechanism whose whole purpose is
  # to refuse. The token now names the lock it refers to (`pid:absolute-dir`), and all three of those
  # facts must hold: the directory still exists, its recorded owner is the token's pid, and that pid
  # is alive. Anything else is a stale or forged token and is refused.
  if [ -n "${CONCRETE_GATE_LOCK:-}" ]; then
    local tok_pid tok_dir tok_owner
    tok_pid="${CONCRETE_GATE_LOCK%%:*}"
    tok_dir="${CONCRETE_GATE_LOCK#*:}"
    # SHAPE-CONSTRAINED. The token must name something that looks like a lock, matching the
    # constraint the release path enforces. This does NOT make the token unforgeable: a PARENT process
    # can always create a directory, write its own pid into `owner`, and pass a valid-looking token —
    # and no check inside this file can prevent that, because a parent controls its children's
    # environment completely. That is stated rather than papered over: this lock defends against
    # ACCIDENTAL concurrency between cooperating runs, which is the failure that has actually
    # happened here repeatedly. It is not an adversarial boundary.
    case "$tok_dir" in */.gate.lock) ;; *) tok_dir="" ;; esac
    if [ -n "$tok_pid" ] && [ -n "$tok_dir" ] && [ "$tok_dir" != "$CONCRETE_GATE_LOCK" ] && [ -d "$tok_dir" ]; then
      tok_owner="$(sed -n 's/^pid=//p' "$tok_dir/owner" 2>/dev/null || true)"
      if [ "$tok_owner" = "$tok_pid" ] && kill -0 "$tok_pid" 2>/dev/null; then
        # ANCESTRY, not just existence. Everything above can be FABRICATED by the caller: make a
        # directory, write your own pid into `owner`, and the token validates. The property that
        # actually makes re-entrancy legitimate is that the holder is an ANCESTOR of this process —
        # "one run, one lock" is a statement about a process tree. Equality with this repository's own
        # lock path cannot be required instead, because gates legitimately run inside a disposable
        # COPY of the tree and inherit a token naming the real repository's lock.
        if [ -r /proc/self/stat ]; then
          local _p=$$ _ppid _guard=0
          while [ "$_p" != "1" ] && [ "$_p" != "0" ] && [ "$_guard" -lt 64 ]; do
            [ "$_p" = "$tok_pid" ] && return 0
            _ppid="$(awk '{print $4}' "/proc/$_p/stat" 2>/dev/null)" || break
            [ -n "$_ppid" ] || break
            _p="$_ppid"; _guard=$((_guard+1))
          done
          [ "$_p" = "$tok_pid" ] && return 0
          echo "GATE-PRECONDITION-FAILED: lock-token-not-ancestor" >&2
    echo "REPOSITORY LOCK TOKEN NOT HELD BY AN ANCESTOR — refusing to run this gate." >&2
          echo "  CONCRETE_GATE_LOCK names pid $tok_pid, which is not in this process's parent chain." >&2
          echo "  A token that merely points at a directory can be fabricated; re-entrancy is only" >&2
          echo "  legitimate for a lock a parent of this process actually holds." >&2
          return 1
        fi
        # No /proc: ancestry is not determinable, so fall back to the weaker checks already passed.
        return 0
      fi
    fi
    echo "GATE-PRECONDITION-FAILED: lock-token-invalid" >&2
    echo "REPOSITORY LOCK TOKEN INVALID — refusing to run this gate." >&2
    echo "  CONCRETE_GATE_LOCK='$CONCRETE_GATE_LOCK' does not name a live lock this run holds." >&2
    echo "  A gate that proceeds on an unverified token reports a verdict while holding nothing." >&2
    echo "  Unset it to acquire a lock normally." >&2
    return 1
  fi

  local owner_pid owner_desc
  # A LOCK WHOSE RECORDED SESSION IS PROVABLY EMPTY IS RECLAIMED, and only then (bug 244).
  local _took=0
  if mkdir "$_GATE_LOCK_DIR" 2>/dev/null; then
    _took=1
  elif _gate_lock_reclaim_stale && mkdir "$_GATE_LOCK_DIR" 2>/dev/null; then
    _took=1
  fi
  if [ "$_took" = "1" ]; then
    # THE OWNER RECORD IS PART OF ACQUISITION, not best-effort metadata. This was `|| true`, so a
    # failed write left a lock directory with no owner while acquisition reported SUCCESS — after
    # which every re-entrant child REJECTED the exported token (its owner check cannot match), and
    # test_mutation.sh counted those refusals as mutation kills. A lease nobody can verify is not a
    # lease, so the directory is removed and acquisition fails.
    # `pgid=` only when this process belongs to a gate session's group: that line is what makes a
    # later automatic reclaim possible, so it is never written on a guess.
    local _pg _pgline=""
    _pg="$(_gate_lock_session_pgid)" && _pgline="pgid=$_pg
"
    if ! printf 'pid=%s\ncmd=%s\n%s' "$$" "${0##*/}" "$_pgline" > "$_GATE_LOCK_DIR/owner" 2>/dev/null; then
      rmdir "$_GATE_LOCK_DIR" 2>/dev/null || rm -rf "$_GATE_LOCK_DIR" 2>/dev/null || true
      echo "GATE-PRECONDITION-FAILED: lock-owner-unrecordable" >&2
      echo "error: took the lock directory but could not record its owner — releasing it." >&2
      return 1
    fi
    export CONCRETE_GATE_LOCK="$$:$_GATE_LOCK_DIR"
    # THE LOCK'S LIFETIME IS THIS PROCESS'S LIFETIME, ARMED AT THE MOMENT OF CREATION.
    #
    # Release used to be entirely explicit, on the stated theory that "gates set their own trap".
    # They do not: of the 167 gates that call require_fresh_binary, 137 install no EXIT trap at all.
    # So the first gate in a run that actually rebuilt kept the lock forever, every later
    # lock-taking gate refused, and the aggregator counted each refusal as a FAILURE —
    # FAST-SURFACE-GATES 8/3, and weeks of red CI, produced entirely by this leak.
    #
    # Armed HERE rather than in require_fresh_binary, because the lock must cover the WHOLE gate
    # run: releasing it when the rebuild finishes would reopen the window while the gate is still
    # collecting evidence about the binary it just verified.
    #
    # `_GATE_LOCK_CREATED` distinguishes the creator from a re-entrant child that merely inherited a
    # valid token. Only the creator arms, and only the creator releases.
    _GATE_LOCK_CREATED=1
    # The creating SHELL INSTANCE, not just the process: `$$` is shared with every subshell.
    _gate_lock_note_creator
    _gate_lock_arm_release
    return 0
  fi

  # HELD. NO AUTOMATIC RECLAIM — and this used to reclaim on a dead owner PID, which is a defect,
  # not a convenience.
  #
  # The recorded PID is the process that CREATED the lock. Re-entrant children inherit
  # `CONCRETE_GATE_LOCK` and keep working after that creator exits, so a dead creator does NOT mean
  # the tree is free. Observed 2026-08-20: owner pid was dead while an isolated 78-family mutation
  # campaign, launched by that creator's shell, was still running — and a later gate DID reclaim the
  # lock on that basis and ran alongside it. Nothing was corrupted only because both parties happened
  # to work in disposable copies. The mechanism defeats the lock's entire purpose.
  #
  # PID liveness cannot be repaired into a safe signal. The creator is the wrong process to probe and
  # PIDs are reused. What CAN be proven is that a whole process group is empty — but only for a group
  # that is the run, which the shells this runs under never create. gate_session.py creates one, and
  # `_gate_lock_reclaim_stale` above reclaims on exactly that proof (bug 244). Everything else is
  # fail-closed as before: refuse, and require an explicit human recovery step. A stale lock costs one
  # command to clear; a wrongly reclaimed one costs a verdict about an artifact that never existed,
  # and costs it silently.
  owner_pid="$(sed -n 's/^pid=//p' "$_GATE_LOCK_DIR/owner" 2>/dev/null || true)"
  owner_desc="$(tr '\n' ' ' < "$_GATE_LOCK_DIR/owner" 2>/dev/null || true)"
  local liveness="unknown"
  if [ -n "$owner_pid" ]; then
    if kill -0 "$owner_pid" 2>/dev/null; then liveness="ALIVE"; else liveness="dead"; fi
  fi
  # Whether other gate work is running is reported as EVIDENCE for the operator's decision, never
  # used to reclaim automatically. A dead creator with live gates is the exact trap above.
  local live_gates
  live_gates="$(ps -eo cmd 2>/dev/null | grep -cE '(scripts/tests/(check|run)_|scripts/ci/)[a-z0-9_]*\.sh' || true)"

  echo "GATE-PRECONDITION-FAILED: repository-busy" >&2
  echo "REPOSITORY BUSY — refusing to run this gate." >&2
  echo "  Lock holder: ${owner_desc:-unknown owner} (creator pid ${owner_pid:-?} is $liveness)" >&2
  echo "  Gate-like processes currently running: ${live_gates:-0}" >&2
  echo "  Recorded session: ${_GATE_LOCK_LEASE:-not examined}" >&2
  echo "" >&2
  echo "  Gates share one .lake tree and one binary. Running alongside a rebuild yields a verdict" >&2
  echo "  about an artifact that never existed, which is worse than no verdict at all." >&2
  if [ "${_GATE_LOCK_LEASE%%:*}" = "alive" ]; then
    echo "" >&2
    echo "  Processes of the run that holds this lock are still alive, so the tree is NOT free even if" >&2
    echo "  its creator is gone. The lock is reclaimed automatically once none of them remains:" >&2
    echo "  wait for them, or stop them." >&2
  elif [ "$liveness" = "dead" ]; then
    echo "" >&2
    echo "  The recorded creator is gone, but that does NOT mean the tree is free: children that" >&2
    echo "  inherited this lock outlive their creator, and reclaiming on a dead PID has already" >&2
    echo "  handed the tree to a concurrent run once. Verify nothing is working the tree, then" >&2
    echo "  recover explicitly:" >&2
    echo "      rm -rf $_GATE_LOCK_DIR" >&2
  else
    echo "  Wait for it to finish." >&2
  fi
  return 1
}

# RELEASE — for an entry point that acquired the lock itself and knows its run is over.
#
# There is still no EXIT trap installed here (that would clobber gates' own traps), so release is
# EXPLICIT: a long-running entry point such as the mutation campaign calls this from its own cleanup
# path. Without it, the first gate to acquire a lock left it behind forever, and the next gate in the
# same tree refused — which the mutation campaign then counted as a KILL, manufacturing evidence
# from its own lock leak (measured 2026-08-21).
#
# ONLY THE CREATOR MAY RELEASE. A re-entrant child that merely inherited the token must not remove a
# lock its parent is still working under; that would hand the tree to a concurrent run, which is the
# same failure as reclaiming on a dead PID.
# THE RELEASE IS COMPOSED INTO EXISTING TRAPS, NEVER SUBSTITUTED FOR THEM.
#
# MEASURED on the committed tree, counting the first `trap` line naming EXIT/INT/TERM/HUP against the
# first `require_fresh_binary` line in each of the 167 callers: 137 install no such trap at all, 29
# install one AFTER the acquisition, 1 before. The method is stated because a different scan (for
# instance one that also counts `trap fatal ERR`) reaches different numbers, and a bare figure in a
# comment cannot be rechecked. What matters is the shape: a release armed at acquisition would have
# been silently replaced in the 29, and absent entirely for the 137.
#
# So `trap` is wrapped: later installation on a terminating signal COMPOSES with the release instead
# of replacing it, the gate's own cleanup runs first because it may still need the tree, and queries
# (`-p`, `-l`) and non-terminating signals — notably ERR, which nine gates use — pass through.
#
# SIGKILL is untrappable by anyone, so a lock stranded by `kill -9` still needs an operator.
# COMPOSED ACROSS NEWLINES, NOT WITH `;`. A cleanup ending in a `# comment` — or containing one —
# would have commented out an appended `; release` on the same line, losing it silently. The control
# for this found it: the handler ran the cleanup and kept the lock while exiting 0. The user action
# goes in a group on its own lines and the release follows on its own line, so nothing the action
# contains can reach past it.
_GATE_LOCK_RELEASE_SNIPPET='_gate_lock_release_auto'
# A TERMINATING SIGNAL MUST TERMINATE. Composing only a release meant the handler released the lock
# and then execution CONTINUED — the gate carried on doing work with no lock at all, which is worse
# than stranding it, and then exited 0. And `trap - TERM` was rewritten into a release handler, so
# the default "die on TERM" disposition could not be restored either. The signal form releases, then
# restores the default and re-raises, so the process dies of the signal it was sent.
_GATE_LOCK_SIGNAL_SNIPPET='_gate_lock_release_auto; builtin trap - %s; kill -%s $$'


# THE AUTOMATIC RELEASE IS NOT THE SAME OPERATION AS THE EXPLICIT ONE, and conflating them produced
# two defects at once.
#
#   A SUBSHELL IS NOT THE CREATOR. `$$` is identical inside `( ... )`, and `_GATE_LOCK_CREATED` is a
#   plain variable a subshell inherits — so a subshell that merely installed its own trap became a
#   creator by both tests and DELETED ITS PARENT'S LOCK on leaving. Verified before this fix:
#   `( trap ... EXIT; true )` printed PARENT_LOST_LOCK_TO_SUBSHELL. `BASHPID` differs in a subshell
#   where `$$` does not, so the creating shell instance is recorded and compared.
#
#   A FAILED RELEASE IS NOT A RELEASE. The snippet was `_gate_lock_release >/dev/null 2>&1 || true`,
#   which discarded the library's own "could not release" warning and forced success — a gate could
#   exit 0 having stranded the lock, with nothing said. The diagnostic is kept and the caller is told.
# THE SHELL INSTANCE, PORTABLY. `BASHPID` distinguishes a subshell from its parent, and bash 3.2 —
# which is what `bash` still is on the macOS runner this workflow uses — does not have it. Falling
# back to `$$` silently reinstated the bug the identity exists to prevent: on macOS a subshell would
# again have deleted its parent's lock. A child's PPID is the shell instance that forked it, which is
# the same fact and is available everywhere.
#
# Expanded INLINE rather than through a helper, because `BASHPID` inside `$( ... )` reports the
# command substitution's own subshell, not the caller's — a helper would have returned the wrong
# identity in exactly the case this is here to get right.
# THE FALLBACK WAS WRONG, AND ONLY THE macOS JOB COULD SHOW IT.
#
# `sh -c 'echo $PPID'` inside `$( )` reports the PPID of that `sh` — which is the COMMAND
# SUBSTITUTION'S OWN SUBSHELL, a different pid on every call. bash 5 optimises the fork away for a
# simple command, so locally the value looked stable and a control built on `unset BASHPID` passed.
# bash 3.2 on the macOS runner does fork, so the identity differed between arming and release, the
# release silently declined every time, and check_lex_escapes held the lock for the rest of the job
# while nine later gates were refused with `repository-busy`. That is what the retained diagnostics
# showed, and no local simulation established it.
#
# `exec sh -c` REPLACES the substitution subshell, so the reported PPID is the shell instance that
# forked it — the caller — on any shell. Stability is then VERIFIED rather than assumed: if two
# consecutive readings disagree, neither identity is usable and the process id is used instead. A
# subshell shares `$$`, so the subshell protection is weaker on such a shell — but never releasing
# at all wedges the tree, which is the failure being repaired.
_gate_lock_note_creator() {
  if [ -n "${BASHPID:-}" ]; then
    _GATE_LOCK_ID_METHOD=bashpid
    _GATE_LOCK_CREATED_BY="$BASHPID"
    return 0
  fi
  local _a _b
  _a="$(exec sh -c 'echo $PPID' 2>/dev/null || true)"
  _b="$(exec sh -c 'echo $PPID' 2>/dev/null || true)"
  if [ -n "$_a" ] && [ "$_a" = "$_b" ]; then
    _GATE_LOCK_ID_METHOD=ppid
    _GATE_LOCK_CREATED_BY="$_a"
  else
    _GATE_LOCK_ID_METHOD=pid
    _GATE_LOCK_CREATED_BY="$$"
  fi
}

_gate_lock_release_auto() {
  [ "${_GATE_LOCK_CREATED:-0}" = "1" ] || return 0
  # READ BY THE SAME METHOD THAT RECORDED IT, and never through a helper: `BASHPID` inside `$( )`
  # reports the substitution's subshell, so a helper would return the wrong identity in exactly the
  # case this exists to get right.
  local _me
  case "${_GATE_LOCK_ID_METHOD:-pid}" in
    bashpid) _me="${BASHPID:-$$}" ;;
    ppid)    _me="$(exec sh -c 'echo $PPID' 2>/dev/null || printf '%s' "$$")" ;;
    *)       _me="$$" ;;
  esac
  [ "$_me" = "${_GATE_LOCK_CREATED_BY:-}" ] || return 0
  if ! _gate_lock_release; then
    # A FAILED RELEASE MUST REACH THE EXIT STATUS. Returning non-zero from an EXIT trap does not
    # change the status bash has already selected, so the gate exited 0 with the lock stranded and
    # only a warning nobody reads. `exit` inside an EXIT trap does set it.
    echo "GATE-POSTCONDITION-FAILED: lock-not-released" >&2
    echo "error: the repository lock could NOT be released; the next run will refuse." >&2
    echo "       Verify nothing is working the tree, then: rm -rf ${_GATE_LOCK_DIR:-.gate.lock}" >&2
    exit 1
  fi
  return 0
}

_gate_lock_arm_release() {
  [ "${_GATE_LOCK_SELF_MANAGED:-0}" = "1" ] && return 0
  local s prev tail
  for s in EXIT INT TERM HUP; do
    if [ "$s" = "EXIT" ]; then tail="$_GATE_LOCK_RELEASE_SNIPPET"
    else tail="$(printf "$_GATE_LOCK_SIGNAL_SNIPPET" "$s" "$s")"; fi
    prev="$(_gate_lock_trap_body "$s")"
    if [ -n "$prev" ]; then
      builtin trap "{
$prev
}
$tail" "$s" 2>/dev/null || true
    else
      builtin trap "$tail" "$s" 2>/dev/null || true
    fi
  done
}

# `trap -p` PRINTS A SHELL-QUOTED ACTION, and it must be UNQUOTED before being re-composed.
# Stripping the outer quotes textually left an embedded `'\''` sequence malformed, so a cleanup
# containing an apostrophe produced a handler that could fail to run — losing both the gate's cleanup
# and the release while the process still exited 0. `eval` on the printed form is exactly the inverse
# of the quoting bash applied.
_gate_lock_trap_body() {
  local line body
  line="$(builtin trap -p "$1" 2>/dev/null)" || return 0
  [ -n "$line" ] || return 0
  body="$(eval "set -- $(printf '%s' "$line" | sed -e 's/^trap -- //' -e "s/ $1\$//")"; printf '%s' "${1:-}")" 2>/dev/null || body=""
  printf '%s' "$body"
}

trap() {
  case "${1:-}" in
    -p|-l|"") builtin trap "$@"; return $? ;;
    --) shift ;;
  esac
  local cmd="${1:-}"; shift 2>/dev/null || true
  [ "$#" -gt 0 ] || { builtin trap "$cmd"; return $?; }
  local s rc=0 norm
  for s in "$@"; do
    # SIGTERM AND TERM ARE THE SAME SIGNAL. The first version matched only the bare names, so
    # `trap cleanup SIGTERM` would have replaced the release outright.
    norm="${s#SIG}"
    case "$norm" in
      EXIT|0|INT|2|TERM|15|HUP|1)
        if [ "${_GATE_LOCK_CREATED:-0}" != "1" ] || [ "${_GATE_LOCK_SELF_MANAGED:-0}" = "1" ] \
           || [ "${BASHPID:-$$}" != "${_GATE_LOCK_CREATED_BY:-}" ]; then
          builtin trap "$cmd" "$s" || rc=1
        else
          local _tail
          if [ "$norm" = "EXIT" ] || [ "$norm" = "0" ]; then _tail="$_GATE_LOCK_RELEASE_SNIPPET"
          else _tail="$(printf "$_GATE_LOCK_SIGNAL_SNIPPET" "$s" "$s")"; fi
          if [ "$cmd" = "-" ]; then
            builtin trap "$_tail" "$s" || rc=1
          else
            builtin trap "{
$cmd
}
$_tail" "$s" || rc=1
          fi
        fi ;;
      *) builtin trap "$cmd" "$s" || rc=1 ;;
    esac
  done
  return $rc
}

_gate_lock_release() {
  [ -n "${CONCRETE_GATE_LOCK:-}" ] || return 0
  local tok_pid tok_dir owner
  tok_pid="${CONCRETE_GATE_LOCK%%:*}"
  tok_dir="${CONCRETE_GATE_LOCK#*:}"
  [ "$tok_pid" = "$$" ] || return 0
  [ -d "$tok_dir" ] || return 0
  owner="$(sed -n 's/^pid=//p' "$tok_dir/owner" 2>/dev/null || true)"
  [ "$owner" = "$$" ] || return 0
  # NAMED `.gate.lock`, and the removal is CHECKED. This is an `rm -rf` of a path that arrived in an
  # environment variable, so the shape is constrained; and a release that silently failed while
  # unsetting the token left the lock in place with nothing holding it, which wedges the repository
  # until someone deletes it by hand.
  case "$tok_dir" in
    */.gate.lock) ;;
    *) echo "warning: refusing to release '$tok_dir' — not a .gate.lock path" >&2; return 1 ;;
  esac
  if ! rm -rf "$tok_dir" 2>/dev/null; then
    echo "warning: could not release the repository lock at $tok_dir" >&2
    echo "         remove it explicitly before the next run: rm -rf $tok_dir" >&2
    return 1
  fi
  if [ -d "$tok_dir" ]; then
    echo "warning: the repository lock at $tok_dir still exists after release" >&2
    return 1
  fi
  unset CONCRETE_GATE_LOCK
  return 0
}

require_fresh_binary() {
  local bin="${1:-$_FRESH_ROOT/.lake/build/bin/concrete}"

  # THE LOCK COMES FIRST, before the rebuild below. Acquiring it after `lake build` would leave the
  # exact window this exists to close: two gates rebuilding the same tree at once.
  _gate_lock_acquire || return 1

  if command -v lake >/dev/null 2>&1; then
    local out
    if ! out="$(cd "$_FRESH_ROOT" && lake build 2>&1)"; then
      # A MACHINE-READABLE MARKER, because a consumer needs to distinguish "this gate ran and
      # disagreed" from "this gate never started". The mutation campaign reads gate logs to decide
      # whether a nonzero exit is rule evidence, and it cannot: the build output is filtered to
      # `^error` lines here, so the compiler diagnostic that names the mutated file never reaches the
      # log the campaign inspects. Its detector was therefore INERT and every such failure was scored
      # as a gate kill. One stable line fixes that for every consumer.
      echo "GATE-PRECONDITION-FAILED: compiler-build" >&2
      echo "COMPILER BUILD FAILED — refusing to run this gate against a stale binary." >&2
      echo "$out" | grep -E '^error' | head -20 >&2
      return 1
    fi
  else
    # NO LAKE MEANS NO VERDICT. This printed a warning and CONTINUED, running the gate against
    # whatever binary happened to be on disk — which is precisely the stale-artifact evidence this
    # file exists to prevent, permitted by the guard itself. A warning is not a control: it scrolls
    # past in a 200-gate run and the PASS lines that follow look identical to verified ones.
    #
    # Refused by default, with a named, explicit override for the environments that genuinely have no
    # toolchain and accept the consequence. The override has to be typed, so it cannot be reached by
    # accident.
    if [ "${CONCRETE_ALLOW_UNVERIFIED_BINARY:-0}" = "1" ]; then
      # A MACHINE-READABLE TAINT, not just a warning. In this mode the gate DOES run, so it is not a
      # precondition failure — but its verdict describes a binary nobody verified, and a human-readable
      # warning is indistinguishable from verified evidence to any consumer that reads logs or
      # artifacts. The campaign refuses completion when it sees this.
      echo "GATE-FRESHNESS-UNVERIFIED: no-toolchain" >&2
      echo "WARNING: 'lake' not on PATH and CONCRETE_ALLOW_UNVERIFIED_BINARY=1 — compiler freshness" >&2
      echo "         NOT verified. This gate's result describes an unverified binary." >&2
    else
      echo "CANNOT VERIFY COMPILER FRESHNESS — refusing to run this gate." >&2
      echo "  'lake' is not on PATH, so there is no way to establish that the binary on disk" >&2
      echo "  corresponds to the checked-out source. A gate that runs anyway produces plausible" >&2
      echo "  evidence about an artifact that may no longer exist, which is worse than no evidence." >&2
      echo "GATE-PRECONDITION-FAILED: no-toolchain" >&2
      echo "  Install the toolchain, or set CONCRETE_ALLOW_UNVERIFIED_BINARY=1 to accept that." >&2
      return 1
    fi
  fi

  if [ ! -x "$bin" ]; then
    echo "GATE-PRECONDITION-FAILED: missing-compiler" >&2
    echo "MISSING COMPILER ARTIFACT: '$bin' does not exist or is not executable." >&2
    return 1
  fi

  return 0
}
