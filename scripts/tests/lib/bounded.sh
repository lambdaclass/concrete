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
# Sourcing (`BOUNDED_DIR="$TMP"; source scripts/tests/lib/bounded.sh`) gives the gate PRIVATE state
# in BOUNDED_DIR (a fresh mktemp directory without it): an outcome file and a log. Nothing is read
# from the environment, so a gate run by another gate never shares or inherits its parent's state.
#
#   bounded_run [-i STDIN_FILE] [-r RETRY_SECS] SECS -- CMD [ARGS...]
#     Runs CMD (an executable, not a shell function) under timeout(1). Its stdout and stderr are
#     buffered per attempt and only a COMPLETED attempt's output is written: an incomplete run
#     writes nothing, so output-matching checks never pass on the partial output of a run that hung.
#     Without -i the command inherits stdin, which a re-run cannot replay: callers that feed input
#     pass -i. Returns CMD's status, or 124 when it timed out on both attempts. A command may itself
#     exit 124, so test with `bounded_is_incomplete "$rc"`, which also reads the outcome bounded_run
#     recorded (works across `( cd dir && bounded_run ... )` subshells; one bounded_run at a time per
#     gate). RETRY_SECS defaults to BOUNDED_RETRY_FACTOR (2) times SECS.
#   bounded_summary
#     Lists this gate's recovered and incomplete runs; gates print it before their final tally.
#   bounded_watchdog SECS  -> sets the array TO for the watchdog form: "${TO[@]}" CMD [ARGS...]
#     runs CMD through `bash bounded.sh SECS` in a separate process, with the same retry; on a second
#     timeout it writes no command output, prints "INCOMPLETE: ..." on stderr and exits 124. Its
#     recovered and incomplete lines go to this gate's log.
#   bounded_gate_status
#     The end-of-gate verdict for a gate using the watchdog form: lists recovered and incomplete
#     runs and returns 1 when any was incomplete, so the gate exits nonzero whatever its own checks
#     concluded from a hung command's empty output. Last line of such a gate:
#       bounded_gate_status && [ "$FAIL" -eq 0 ]
#
# Without timeout(1) (outside the devshell on macOS) the command runs once with no watchdog and a
# warning on stderr, as the gates' previous `TO=""` fallback did; such a run is never incomplete.

BOUNDED_RETRY_FACTOR="${BOUNDED_RETRY_FACTOR:-2}"
BOUNDED_SH="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/$(basename "${BASH_SOURCE[0]}")"
if [ -n "${BOUNDED_DIR:-}" ]; then
  _bounded_owns_dir=""
else
  BOUNDED_DIR="$(mktemp -d "${TMPDIR:-/tmp}/bounded.XXXXXX")"; _bounded_owns_dir=1
fi
BOUNDED_OUTCOME_FILE="$BOUNDED_DIR/bounded.outcome"
BOUNDED_LOG="$BOUNDED_DIR/bounded.log"
# The watchdog form appends to its caller's log, handed over explicitly on its command line.
if [ "${BASH_SOURCE[0]}" = "$0" ] && [ -n "${BOUNDED_WATCHDOG_LOG:-}" ]; then
  BOUNDED_LOG="$BOUNDED_WATCHDOG_LOG"
fi
: >> "$BOUNDED_LOG"

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
    # GNU timeout runs the command in its own process group and signals the whole group: TERM at
    # the budget, then KILL BOUNDED_KILL_AFTER seconds later for anything that ignored TERM, and it
    # returns only once the command is gone, so no attempt outlives its budget into the retry.
    # --verbose makes it say when it fired; that line, not status 124 (which a command may exit
    # with itself), is what marks a timeout.
    if [ -n "$stdin_file" ]; then
      timeout --verbose -k "${BOUNDED_KILL_AFTER:-10}" "$budget" "$@" < "$stdin_file" > "$tmp/out" 2> "$tmp/err"; rc=$?
    else
      timeout --verbose -k "${BOUNDED_KILL_AFTER:-10}" "$budget" "$@" > "$tmp/out" 2> "$tmp/err"; rc=$?
    fi
    if ! grep -qE '^timeout: sending signal (TERM|KILL) to command' "$tmp/err"; then done_rc="$rc"; break; fi
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

bounded_watchdog() {
  TO=(env "BOUNDED_WATCHDOG_LOG=$BOUNDED_LOG" bash "$BOUNDED_SH" "$1")
}

bounded_gate_status() {
  local ninc
  bounded_summary
  ninc=$(grep -c '^incomplete' "$BOUNDED_LOG" 2>/dev/null || true); ninc=${ninc:-0}
  [ "$ninc" -eq 0 ] || echo "INCOMPLETE: $ninc command(s) timed out twice: this run is incomplete, not passed"
  [ -z "$_bounded_owns_dir" ] || rm -rf "$BOUNDED_DIR"
  [ "$ninc" -eq 0 ]
}

# Watchdog form: `bash bounded.sh SECS CMD...` (through bounded_watchdog's TO array).
if [ "${BASH_SOURCE[0]}" = "$0" ]; then
  [ $# -ge 2 ] || { echo "usage: bash $0 SECS CMD [ARGS...]" >&2; exit 2; }
  secs="$1"; shift
  bounded_run "$secs" -- "$@"; rc=$?
  if bounded_is_incomplete "$rc"; then
    echo "INCOMPLETE: timed out twice (${secs}s, then $((secs * BOUNDED_RETRY_FACTOR))s alone): $*" >&2
  fi
  [ -z "$_bounded_owns_dir" ] || rm -rf "$BOUNDED_DIR"
  exit "$rc"
fi
