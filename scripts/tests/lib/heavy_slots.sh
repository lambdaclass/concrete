#!/usr/bin/env bash
# MACHINE-WIDE LOAD CONTROL FOR HEAVY RUNS (R-0543).
#
# On 2026-10-22, 4-5 agents ran full gate sets on one laptop at the same time. The load average
# reached 140-240, gate timeouts produced false failures, and 25-minute runs took 90 minutes. The
# repository lock (fresh.sh, `.gate.lock`) cannot prevent that: it is per checkout, and every agent
# works in its own worktree.
#
# This is a counting semaphore shared by every worktree and clone of the machine's user:
# at most CONCRETE_HEAVY_SLOTS (default 2) heavy runs hold a slot at once; a further one waits and
# prints who holds the slots, or with --no-wait / CONCRETE_HEAVY_NO_WAIT=1 fails fast (exit 75).
#
# WHAT IS HEAVY: run_ci_gates_local.sh with no selector or a large selection, run_tests.sh --full,
# and the mutation drivers. Single gates, the fast surface and --plan never take a slot.
#
# MECHANISM: `mkdir`, as in fresh.sh, because macOS ships no `flock` binary. Slots are directories
#   ${CONCRETE_HEAVY_DIR:-${XDG_RUNTIME_DIR:-$HOME/.cache}/concrete-heavy}/slot-<i>
# each holding an `owner` file (pid, process start time, worktree, command, since). Every inspection
# and every acquisition happens under a short-lived mutex directory (`.mutex`), so a slot is never
# seen half-written and two waiters cannot both reclaim the same stale slot.
#
# STALE SLOTS ARE RECLAIMED ON A DEAD PID, unlike `.gate.lock`, and the difference is deliberate:
# `.gate.lock` guards CORRECTNESS (a wrong reclaim yields a verdict about a half-built artifact), so
# it fails closed. A slot guards only LOAD; a wrong reclaim costs one extra concurrent run, which is
# the status quo this replaces, while a stranded slot would wedge every worktree on the machine. A
# pid is "the same process" only if its start time also matches, so a reused pid is not mistaken for
# the holder. Children of a killed holder may outlive it; their load is not counted (stated limit).
#
# RE-ENTRANT: the holder exports CONCRETE_HEAVY_SLOT=<pid>:<slotdir>; a nested heavy run (a gate
# that runs run_tests.sh --full under a full gate pass) inherits it and takes no second slot, so two
# holders can never deadlock waiting for slots their own descendants need.

_HEAVY_DIR="${CONCRETE_HEAVY_DIR:-${XDG_RUNTIME_DIR:-$HOME/.cache}/concrete-heavy}"
_HEAVY_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." 2>/dev/null && pwd)"
HEAVY_SLOT_HELD=""          # the slot directory this shell instance created, if any
HEAVY_EXIT_BUSY=75          # EX_TEMPFAIL: "busy, try again", never a gate verdict

heavy_slots_n() {
  local n="${CONCRETE_HEAVY_SLOTS:-2}"
  case "$n" in ''|*[!0-9]*) n=2 ;; esac
  [ "$n" -ge 1 ] || n=1
  printf '%s' "$n"
}

# Start time of a pid, or empty if no such process. `ps -o lstart=` exists on macOS and procps.
_heavy_pid_start() { ps -o lstart= -p "$1" 2>/dev/null | sed 's/^ *//; s/ *$//'; }

# 0 when the owner recorded in $1/owner is a live process (same pid AND same start time).
_heavy_owner_alive() {
  local f="$1/owner" pid start now
  [ -f "$f" ] || return 2   # no owner file: being created or being released; not judged here
  pid="$(sed -n 's/^pid=//p' "$f" 2>/dev/null)"
  start="$(sed -n 's/^start=//p' "$f" 2>/dev/null)"
  [ -n "$pid" ] || return 1
  now="$(_heavy_pid_start "$pid")"
  [ -n "$now" ] || return 1
  [ -z "$start" ] || [ "$now" = "$start" ] || return 1
  return 0
}

_heavy_age() { # dir -> seconds since mtime
  local m
  m="$(stat -f %m "$1" 2>/dev/null || stat -c %Y "$1" 2>/dev/null || echo 0)"
  echo $(( $(date +%s) - m ))
}

# Remove a directory atomically from the namespace first (rename), then delete it.
_heavy_discard() {
  local trash="$_HEAVY_DIR/.trash.$$.$RANDOM"
  mv "$1" "$trash" 2>/dev/null && rm -rf "$trash" 2>/dev/null
}

_heavy_mutex_acquire() {
  local m="$_HEAVY_DIR/.mutex" tries=0 pid
  while ! mkdir "$m" 2>/dev/null; do
    tries=$((tries+1))
    if [ $((tries % 20)) -eq 0 ]; then
      # A mutex held by a dead process, or for longer than any critical section takes, is stale.
      pid="$(cat "$m/pid" 2>/dev/null)"
      if { [ -n "$pid" ] && [ -z "$(_heavy_pid_start "$pid")" ]; } || [ "$(_heavy_age "$m")" -gt 30 ]; then
        _heavy_discard "$m"
      fi
    fi
    [ "$tries" -lt 1200 ] || { echo "heavy-slots: could not take $m" >&2; return 1; }
    sleep 0.05
  done
  echo "$$" > "$m/pid" 2>/dev/null || true
  return 0
}
_heavy_mutex_release() { rm -rf "$_HEAVY_DIR/.mutex" 2>/dev/null || true; }

# Print one line per slot currently present. Called under the mutex or not (diagnostic only).
heavy_slot_holders() {
  local d pid wt cmd since st now
  now="$(date +%s)"
  for d in "$_HEAVY_DIR"/slot-*; do
    [ -d "$d" ] || continue
    pid="$(sed -n 's/^pid=//p' "$d/owner" 2>/dev/null)"
    wt="$(sed -n 's/^worktree=//p' "$d/owner" 2>/dev/null)"
    cmd="$(sed -n 's/^cmd=//p' "$d/owner" 2>/dev/null)"
    since="$(sed -n 's/^since=//p' "$d/owner" 2>/dev/null)"
    if _heavy_owner_alive "$d"; then st="alive"; else st="DEAD (reclaimable)"; fi
    printf '  %s: pid %s %s, %s, in %s, for %sm\n' "${d##*/}" "${pid:-?}" "$st" "${cmd:-?}" \
      "${wt:-?}" "$(( (now - ${since:-$now}) / 60 ))"
  done
}

# One acquisition attempt under the mutex. 0 = acquired, 1 = all slots busy, 2 = error.
_heavy_try() {
  local label="$1" n live=0 d i rc
  n="$(heavy_slots_n)"
  _heavy_mutex_acquire || return 2
  for d in "$_HEAVY_DIR"/slot-*; do
    [ -d "$d" ] || continue
    _heavy_owner_alive "$d"; rc=$?
    if [ "$rc" -eq 0 ]; then live=$((live+1))
    elif [ "$rc" -eq 2 ] && [ "$(_heavy_age "$d")" -lt 60 ]; then live=$((live+1))  # in flight
    else
      echo "heavy-slots: reclaiming ${d##*/} from a dead holder:" >&2
      sed 's/^/    /' "$d/owner" >&2 2>/dev/null || true
      _heavy_discard "$d"
    fi
  done
  if [ "$live" -ge "$n" ]; then _heavy_mutex_release; return 1; fi
  i=0
  while [ -e "$_HEAVY_DIR/slot-$i" ]; do i=$((i+1)); done
  d="$_HEAVY_DIR/slot-$i"
  if ! mkdir "$d" 2>/dev/null \
     || ! printf 'pid=%s\nstart=%s\nworktree=%s\ncmd=%s\nsince=%s\n' "$$" "$(_heavy_pid_start "$$")" \
            "$_HEAVY_ROOT" "$label" "$(date +%s)" > "$d/owner.tmp" 2>/dev/null \
     || ! mv "$d/owner.tmp" "$d/owner" 2>/dev/null; then
    rm -rf "$d" 2>/dev/null; _heavy_mutex_release
    echo "heavy-slots: could not create $d" >&2; return 2
  fi
  _heavy_mutex_release
  HEAVY_SLOT_HELD="$d"
  export CONCRETE_HEAVY_SLOT="$$:$d"
  return 0
}

# heavy_slot_acquire <label> [--no-wait]
#   0  slot held (or inherited from an ancestor holding one)
#   75 all slots busy and --no-wait / CONCRETE_HEAVY_NO_WAIT=1
#   2  the slot directory is unusable
heavy_slot_acquire() {
  local label="${1:-heavy run}" nowait="${CONCRETE_HEAVY_NO_WAIT:-0}"
  [ "${2:-}" = "--no-wait" ] && nowait=1
  # Inherited: a live ancestor holds a slot for this whole process tree.
  if [ -n "${CONCRETE_HEAVY_SLOT:-}" ]; then
    local tp="${CONCRETE_HEAVY_SLOT%%:*}" td="${CONCRETE_HEAVY_SLOT#*:}"
    if [ -d "$td" ] && [ "$(sed -n 's/^pid=//p' "$td/owner" 2>/dev/null)" = "$tp" ] && _heavy_owner_alive "$td"; then
      return 0
    fi
    unset CONCRETE_HEAVY_SLOT   # stale token from a dead holder: acquire normally
  fi
  mkdir -p "$_HEAVY_DIR" 2>/dev/null || { echo "heavy-slots: cannot create $_HEAVY_DIR" >&2; return 2; }
  local rc waited=0 last=-999 now start
  start="$(date +%s)"
  while :; do
    _heavy_try "$label"; rc=$?
    [ "$rc" -eq 0 ] && break
    [ "$rc" -eq 2 ] && return 2
    if [ "$nowait" = "1" ]; then
      echo "heavy-slots: all $(heavy_slots_n) heavy slots are busy (--no-wait). Holders:" >&2
      heavy_slot_holders >&2
      return "$HEAVY_EXIT_BUSY"
    fi
    now="$(date +%s)"
    if [ $((now - last)) -ge 60 ]; then
      echo "heavy-slots: waiting for one of $(heavy_slots_n) heavy slots ($label, waited $(( (now-start)/60 ))m). Holders:" >&2
      heavy_slot_holders >&2
      last="$now"
    fi
    waited=1
    sleep "${CONCRETE_HEAVY_POLL:-3}"
  done
  [ "$waited" = "1" ] && echo "heavy-slots: acquired ${HEAVY_SLOT_HELD##*/} after $(( ($(date +%s)-start) )) s" >&2
  return 0
}

# Release the slot this shell instance created; a no-op for an inherited one.
heavy_slot_release() {
  [ -n "$HEAVY_SLOT_HELD" ] || return 0
  if [ "$(sed -n 's/^pid=//p' "$HEAVY_SLOT_HELD/owner" 2>/dev/null)" = "$$" ]; then
    _heavy_discard "$HEAVY_SLOT_HELD"
  fi
  HEAVY_SLOT_HELD=""
  unset CONCRETE_HEAVY_SLOT
  return 0
}

# Default parallelism for one heavy run. With one slot the old rule stands (cores-2, at most 8).
# With N>1 slots the machine is shared: each run takes its fair share of the cores, (cores-2)/N,
# and never more than the cores currently free (cores minus the 1-minute load average), with no
# fixed cap, so a 64-core server is not held to 8 and a loaded laptop is not oversubscribed.
heavy_jobs_default() {
  local cores n load free fair j
  cores="$( (sysctl -n hw.ncpu 2>/dev/null || nproc 2>/dev/null) || echo 4)"
  n="$(heavy_slots_n)"
  if [ "$n" -le 1 ]; then
    j=$((cores - 2)); [ "$j" -gt 8 ] && j=8
  else
    load="$( (sysctl -n vm.loadavg 2>/dev/null | tr -d '{}' | awk '{print $1}') \
             || awk '{print $1}' /proc/loadavg 2>/dev/null)"
    [ -n "$load" ] || load="$(awk '{print $1}' /proc/loadavg 2>/dev/null)"
    load="$(printf '%s' "${load:-0}" | awk '{printf "%d", $1 + 0.999}')"
    free=$((cores - load))
    fair=$(( (cores - 2) / n ))
    j="$fair"; [ "$free" -lt "$j" ] && j="$free"
  fi
  [ "$j" -lt 1 ] && j=1
  printf '%s' "$j"
}
