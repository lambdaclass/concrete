#!/usr/bin/env bash
# Run one background agent job and pair it with a bounded watcher.
#
#   launch.sh start [--mem SIZE] [--tasks N] [--dir DIR] NAME SECS -- CMD [ARGS...]
#   launch.sh wait JOBDIR
#   launch.sh status JOBDIR
#
# start runs CMD once in a transient systemd user unit, with MemoryMax (8G by
# default), TasksMax (1024), RuntimeMaxSec=SECS, the caller's PATH, HOME and
# working directory. Its unique job directory records cmd, unit, deadline,
# out.log and, after a normal command exit, an atomically-written status. It
# prints only UNIT, DIR and the absolute WAIT command. Writers are never
# retried: a timeout or kill loses the unit without a status, not a second run.
#
# wait polls until status appears, the unit disappears, or deadline plus grace
# (120s by default) passes. A normal exit prints JOB ... ENDED status=N and the
# last 40 log lines, then returns N. A lost unit returns 125; an overrun returns
# 124 (INCOMPLETE). AGENT_WAIT_INTERVAL and AGENT_WAIT_GRACE override the 10s
# poll and 120s grace. status makes the same observation once, without the log.
set -uo pipefail

usage() {
  echo "usage: launch.sh start [--mem SIZE] [--tasks N] [--dir DIR] NAME SECS -- CMD [ARGS...] | launch.sh wait JOBDIR | launch.sh status JOBDIR" >&2
  exit 2
}

fail() {
  echo "launch.sh: $*" >&2
  exit 2
}

positive_integer() {
  case "$1" in
    ''|*[!0-9]*) return 1 ;;
  esac
  [ "${#1}" -le 9 ] || return 1
  [ "$((10#$1))" -gt 0 ]
}

self_dir() {
  local source dir
  source="${BASH_SOURCE[0]}"
  case "$source" in */*) dir="${source%/*}" ;; *) dir=. ;; esac
  (CDPATH= cd "$dir" && pwd -P)
}

start() {
  local mem=8G tasks=1024 dir="" name="" secs="" cwd="" safe="" unit="" jobdir="" base="" script="" arg=""
  while [ "$#" -gt 0 ]; do
    case "$1" in
      --mem) [ "$#" -ge 2 ] || usage; mem="$2"; shift 2 ;;
      --tasks) [ "$#" -ge 2 ] || usage; tasks="$2"; shift 2 ;;
      --dir) [ "$#" -ge 2 ] || usage; dir="$2"; shift 2 ;;
      --) break ;;
      -*) fail "unknown start option: $1" ;;
      *) break ;;
    esac
  done
  [ "$#" -ge 4 ] && [ "$3" = -- ] || usage
  name="$1"; secs="$2"; shift 3
  [ "$#" -gt 0 ] || usage
  case "$name" in
    ''|[!A-Za-z0-9]*|*[!A-Za-z0-9_.-]*)
      fail "NAME must be 1-64 characters: alphanumeric, then alphanumerics, '.', '_' or '-'" ;;
  esac
  [ "${#name}" -le 64 ] || fail "NAME must be at most 64 characters"
  positive_integer "$secs" || fail "SECS must be a positive integer"
  positive_integer "$tasks" || fail "--tasks must be a positive integer"
  [ -n "$mem" ] || fail "--mem must not be empty"
  command -v systemd-run >/dev/null 2>&1 || fail "systemd-run --user is unavailable (systemd-run not found)"

  secs="$((10#$secs))"; tasks="$((10#$tasks))"
  cwd="$(pwd -P)" || fail "cannot determine the working directory"
  safe="${name//[^A-Za-z0-9_.-]/-}"
  unit="agent-${safe}-$(date +%s)-$$.service"
  if [ -n "$dir" ]; then jobdir="$dir"; else
    base="${AGENT_JOBS_DIR:-$HOME/.cache/concrete-agent-jobs}"
    jobdir="$base/$unit"
  fi
  mkdir -p "$jobdir" || fail "cannot create job directory: $jobdir"
  jobdir="$(CDPATH= cd "$jobdir" && pwd -P)" || fail "cannot resolve job directory"
  [ ! -e "$jobdir/status" ] || fail "job directory already has a status: $jobdir"
  script="$(self_dir)/launch.sh"
  printf '%s\n' "$unit" > "$jobdir/unit" || exit 2
  printf '%s\n' "$(( $(date +%s) + secs ))" > "$jobdir/deadline" || exit 2
  printf '%q ' "$@" > "$jobdir/cmd" || exit 2
  printf '\n' >> "$jobdir/cmd" || exit 2

  if ! systemd-run --user --collect --unit="$unit" \
      --property=MemoryMax="$mem" --property=TasksMax="$tasks" \
      --property=RuntimeMaxSec="$secs" --property=WorkingDirectory="$cwd" \
      --setenv=PATH="$PATH" --setenv=HOME="$HOME" \
      bash -c '
        dir=$1; shift
        "$@" >"$dir/out.log" 2>&1
        rc=$?
        tmp=$dir/.status.$$
        if printf "%s\n" "$rc" >"$tmp" && mv "$tmp" "$dir/status"; then :
        else rm -f "$tmp"; fi
        exit "$rc"
      ' launch-job "$jobdir" "$@" >/dev/null; then
    fail "systemd-run --user is unavailable or could not start $unit"
  fi
  printf 'UNIT=%s\nDIR=%s\nWAIT=bash %s wait %s\n' "$unit" "$jobdir" "$script" "$jobdir"
}

JOBDIR="" UNIT="" DEADLINE="" ACTION=wait DONE=0

load_job() {
  JOBDIR="$1"
  [ -n "$JOBDIR" ] && [ -d "$JOBDIR" ] || fail "job directory not found: $JOBDIR"
  [ -r "$JOBDIR/unit" ] && [ -r "$JOBDIR/deadline" ] || fail "job directory lacks unit or deadline: $JOBDIR"
  IFS= read -r UNIT < "$JOBDIR/unit" || fail "cannot read $JOBDIR/unit"
  IFS= read -r DEADLINE < "$JOBDIR/deadline" || fail "cannot read $JOBDIR/deadline"
  case "$DEADLINE" in ''|*[!0-9]*) fail "invalid deadline in $JOBDIR/deadline" ;; esac
  command -v systemctl >/dev/null 2>&1 || fail "systemctl is unavailable"
}

unit_result() {
  local result=""
  result="$(systemctl --user show "$UNIT" --property=Result --value 2>/dev/null)"
  echo "${result:-unknown}"
}

inspect() {
  local rc="" state="" now="" grace="${AGENT_WAIT_GRACE:-120}"
  DONE=0
  if [ -f "$JOBDIR/status" ]; then
    IFS= read -r rc < "$JOBDIR/status" || fail "cannot read $JOBDIR/status"
    case "$rc" in ''|*[!0-9]*) fail "invalid status in $JOBDIR/status" ;; esac
    [ "$rc" -le 255 ] || fail "invalid status in $JOBDIR/status"
    DONE=1
    printf 'JOB %s ENDED status=%s\n' "$UNIT" "$rc"
    if [ "$ACTION" = wait ] && [ -f "$JOBDIR/out.log" ]; then tail -n 40 "$JOBDIR/out.log"; fi
    return "$rc"
  fi

  state="$(systemctl --user is-active "$UNIT" 2>/dev/null)"
  case "$state" in
    active|activating|reloading)
      now="$(date +%s)"
      case "$grace" in ''|*[!0-9]*) fail "AGENT_WAIT_GRACE must be a non-negative integer" ;; esac
      if [ "$now" -gt "$((DEADLINE + 10#$grace))" ]; then
        DONE=1
        printf 'JOB %s STILL RUNNING past its deadline\n' "$UNIT"
        return 124
      fi
      if [ "$ACTION" = status ]; then printf 'JOB %s RUNNING\n' "$UNIT"; fi
      return 0 ;;
    *)
      DONE=1
      printf 'JOB %s ENDED without a status (killed, timed out or OOM): %s\n' "$UNIT" "$(unit_result)"
      return 125 ;;
  esac
}

watch() {
  local interval="${AGENT_WAIT_INTERVAL:-10}" rc=0
  case "$interval" in ''|*[!0-9]*) fail "AGENT_WAIT_INTERVAL must be a positive integer" ;; esac
  [ "$interval" -gt 0 ] || fail "AGENT_WAIT_INTERVAL must be a positive integer"
  load_job "$1"
  while :; do
    inspect; rc=$?
    [ "$DONE" -eq 1 ] && return "$rc"
    [ "$ACTION" = wait ] || return 0
    sleep "$interval"
  done
}

[ "$#" -ge 1 ] || usage
ACTION="$1"; shift
case "$ACTION" in
  start) start "$@" ;;
  wait) [ "$#" -eq 1 ] || usage; watch "$1" ;;
  status) [ "$#" -eq 1 ] || usage; watch "$1" ;;
  *) usage ;;
esac
