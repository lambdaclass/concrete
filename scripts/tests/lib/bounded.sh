#!/usr/bin/env bash
# Bounded runs: a timeout is INCOMPLETE, never a failure and never a pass.
#
# The rule (ROADMAP "Promotion pipeline", result vocabulary): a test that exceeds its time budget
# has produced no observation. Under machine load a freshly linked binary or a compile can overrun
# a budget sized for an idle machine (test_ssa.sh once reported hundreds of 5-second alarms as
# failures), so a timeout is re-run once, alone, with a larger budget:
#   - the re-run completes      -> its result is judged normally (pass or FAIL), recorded "recovered";
#   - the re-run times out too  -> INCOMPLETE: not a pass, not a failure. A gate with an incomplete
#                                  result exits nonzero, and a qualifying run contains none.
#
# Two ways to use it:
#
#   source scripts/tests/lib/bounded.sh
#   bounded_run [-i STDIN_FILE] [-r RETRY_SECS] SECS -- CMD [ARGS...]
#     Runs CMD under timeout(1). Its stdout and stderr are buffered per attempt and only a COMPLETED
#     attempt's output is written: an incomplete run writes nothing, so output-matching checks can
#     never pass on the partial output of a run that hung. Without -i the command inherits stdin,
#     which a re-run cannot replay: callers that feed input pass -i. Returns CMD's status, or 124
#     when the command timed out on both attempts. Because a command may itself exit 124, test
#     incompleteness with `bounded_is_incomplete "$rc"`, which also reads the outcome bounded_run
#     recorded (in $BOUNDED_OUTCOME_FILE, so it works across `( cd dir && bounded_run ... )`
#     subshells). RETRY_SECS defaults to BOUNDED_RETRY_FACTOR (2) times SECS.
#   bounded_summary
#     Prints one line per recovered and per incomplete run of this gate and their counts; gates
#     call it before their final tally so a recovered run is visible, not silent.
#
#   TO="bash scripts/tests/lib/bounded.sh 300"; $TO CMD [ARGS...]
#     Watchdog form for gates that wrap single commands: the same retry, then, on a second
#     timeout, no output from the command, an "INCOMPLETE: ..." line on stderr and status 124.
#     A recovered run is appended to $BOUNDED_LOG when the caller exports it.
#
# Every recovered and incomplete run appends a line to $BOUNDED_LOG ("recovered <secs> <cmd>" /
# "incomplete <secs>+<retry> <cmd>"). Sourcing this file sets BOUNDED_OUTCOME_FILE and BOUNDED_LOG
# (exported) when the caller has not: set them under the gate's own scratch directory to avoid
# leaving two small files in $TMPDIR.
#
# Without timeout(1) (outside the devshell on macOS) the command runs once with no watchdog and a
# warning on stderr, as the gates' previous `TO=""` fallback did; such a run is never incomplete.

BOUNDED_RETRY_FACTOR="${BOUNDED_RETRY_FACTOR:-2}"
_bounded_inherited="${BOUNDED_OUTCOME_FILE:+1}"
if [ -z "${BOUNDED_OUTCOME_FILE:-}" ]; then
  BOUNDED_OUTCOME_FILE="$(mktemp "${TMPDIR:-/tmp}/bounded_outcome.XXXXXX")"
fi
export BOUNDED_OUTCOME_FILE
export BOUNDED_LOG="${BOUNDED_LOG:-$BOUNDED_OUTCOME_FILE.log}"

bounded_is_incomplete() {
  [ "$1" = 124 ] && [ "$(cat "$BOUNDED_OUTCOME_FILE" 2>/dev/null)" = incomplete ]
}

bounded_run() {
  local stdin_file="" retry="" secs="" rc tries=0 budget tmp done_rc
  while [ $# -gt 0 ]; do
    case "$1" in
      -i) stdin_file="$2"; shift 2 ;;
      -r) retry="$2"; shift 2 ;;
      --) shift; break ;;
      *) if [ -z "$secs" ]; then secs="$1"; shift; else break; fi ;;
    esac
  done
  [ -n "$secs" ] && [ $# -gt 0 ] || { echo "bounded_run: usage: bounded_run [-i FILE] [-r SECS] SECS -- CMD..." >&2; return 2; }
  [ -n "$retry" ] || retry=$((secs * BOUNDED_RETRY_FACTOR))
  echo done > "$BOUNDED_OUTCOME_FILE"
  if ! command -v timeout >/dev/null 2>&1; then
    echo "  warn 'timeout' not found — running without a hang watchdog" >&2
    if [ -n "$stdin_file" ]; then "$@" < "$stdin_file"; else "$@"; fi
    return $?
  fi
  tmp="$(mktemp -d)"
  budget="$secs"
  while :; do
    tries=$((tries + 1))
    rm -f "$tmp/rc"
    # The command's own status goes to a file, so a command that exits 124 is never read as a
    # timeout: no status file means timeout(1) stopped it. (timeout signals its whole process
    # group, so the command dies with the sh that runs it.)
    if [ -n "$stdin_file" ]; then
      timeout "$budget" sh -c '"$@"; echo $? > "$0"' "$tmp/rc" "$@" < "$stdin_file" > "$tmp/out" 2> "$tmp/err"
    else
      timeout "$budget" sh -c '"$@"; echo $? > "$0"' "$tmp/rc" "$@" > "$tmp/out" 2> "$tmp/err"
    fi
    if [ -s "$tmp/rc" ]; then done_rc="$(cat "$tmp/rc")"; break; fi
    done_rc=""
    [ "$tries" = 1 ] || break
    budget="$retry"
  done
  if [ -n "$done_rc" ]; then
    cat "$tmp/out"; cat "$tmp/err" >&2
    rc="$done_rc"
    [ "$tries" = 2 ] && echo "recovered ${secs}s $*" >> "$BOUNDED_LOG"
  else
    echo incomplete > "$BOUNDED_OUTCOME_FILE"
    echo "incomplete ${secs}s+${retry}s $*" >> "$BOUNDED_LOG"
    rc=124
  fi
  rm -rf "$tmp"
  return "$rc"
}

bounded_summary() {
  local nrec ninc
  nrec=$(grep -c '^recovered' "$BOUNDED_LOG" 2>/dev/null || true); nrec=${nrec:-0}
  ninc=$(grep -c '^incomplete' "$BOUNDED_LOG" 2>/dev/null || true); ninc=${ninc:-0}
  [ "$nrec" -gt 0 ] || [ "$ninc" -gt 0 ] || return 0
  echo "Bounded runs: $nrec recovered (timed out, then completed alone), $ninc incomplete (timed out twice):"
  sed 's/^/  /' "$BOUNDED_LOG"
}

# Watchdog form: `bash bounded.sh SECS CMD...`.
if [ "${BASH_SOURCE[0]}" = "$0" ]; then
  [ $# -ge 2 ] || { echo "usage: bash $0 SECS CMD [ARGS...]" >&2; exit 2; }
  secs="$1"; shift
  bounded_run "$secs" -- "$@"; rc=$?
  if bounded_is_incomplete "$rc"; then
    echo "INCOMPLETE: timed out twice (${secs}s, then $((secs * BOUNDED_RETRY_FACTOR))s alone): $*" >&2
  fi
  # A watchdog that created its own outcome file removes it (an inherited one is the caller's).
  [ -n "$_bounded_inherited" ] || rm -f "$BOUNDED_OUTCOME_FILE" "$BOUNDED_OUTCOME_FILE.log"
  exit "$rc"
fi
