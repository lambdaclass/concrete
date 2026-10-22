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
# Each attempt runs under lib/bounded_exec.py, which makes the command the leader of a new process
# group, reports its outcome through a status file (never through the command's own streams, so
# nothing the command prints can fake or hide a timeout) and, when the budget expires, sends SIGTERM
# and then SIGKILL to the whole group, so no descendant outlives the attempt into the retry.
#
# Sourcing (`BOUNDED_PARENT="$TMP"; source scripts/tests/lib/bounded.sh`) gives the gate PRIVATE
# state: a fresh directory (mktemp under BOUNDED_PARENT, else $TMPDIR) with an outcome file and a
# log. Every sourcing creates its own directory, so nested or parallel gates never share state.
# With BOUNDED_PARENT under the gate's own scratch directory, its cleanup removes the state too.
#
#   bounded_run [-i STDIN_FILE] [-r RETRY_SECS] SECS -- CMD [ARGS...]
#     Runs CMD (an executable, not a shell function). Its stdout and stderr are buffered per
#     attempt and only a COMPLETED attempt's output is written: an incomplete run writes nothing,
#     so output-matching checks never pass on the partial output of a run that hung. Without -i the
#     command inherits stdin, which a re-run cannot replay: callers that feed input pass -i.
#     Returns CMD's status, or 124 when it timed out on both attempts or the runner reported no
#     outcome. SECS and RETRY_SECS are positive decimal seconds. A command may itself exit
#     124, so test with `bounded_is_incomplete "$rc"`, which also reads the outcome bounded_run
#     recorded (this works across `( cd dir && bounded_run ... )` subshells; run one bounded_run
#     at a time per sourcing). RETRY_SECS defaults to BOUNDED_RETRY_FACTOR (2) times SECS.
#   bounded_summary
#     Lists this gate's recovered and incomplete runs; gates print it before their final tally.
#   bounded_watchdog SECS  -> sets the array TO for the watchdog form: "${TO[@]}" CMD [ARGS...]
#     runs CMD through `bash bounded.sh SECS` in a separate process with the same retry; on a second
#     timeout it writes no command output, prints "INCOMPLETE: ..." on stderr and exits 124. Its
#     recovered and incomplete lines go to this gate's log (handed over on its command line).
#   bounded_gate_status
#     The end-of-gate verdict for a gate using the watchdog form: lists recovered, incomplete and
#     runner-failed runs, removes the state directory, and returns 1 when any run was incomplete or
#     the runner failed, so the gate exits nonzero whatever its own checks concluded from a hung
#     command's empty output. Last line:
#       bounded_gate_status && [ "$FAIL" -eq 0 ]
#
# If the runner itself fails (no outcome reported), the command's output is discarded and the run
# is incomplete (status 124, a "runner-failed" log line): never a pass. If the runner is cancelled (the
# gate is interrupted or terminated), it kills the command's group first.
#
# Without python3 commands run directly with no watchdog: one warning when the gate sources this
# file, never inside a command's output, and the watchdog form then runs commands directly too.
# Such a run is never incomplete.

BOUNDED_RETRY_FACTOR="${BOUNDED_RETRY_FACTOR:-2}"
if [[ "$BOUNDED_RETRY_FACTOR" =~ ^[0-9]+(\.[0-9]+)?$ ]] && awk -v f="$BOUNDED_RETRY_FACTOR" 'BEGIN { exit !(f + 0 >= 1) }'; then :
else echo "bounded.sh: BOUNDED_RETRY_FACTOR must be a number >= 1; using 2" >&2; BOUNDED_RETRY_FACTOR=2; fi
# _bounded_secs_ok S: S is a positive decimal number of seconds (fractions allowed).
_bounded_secs_ok() { [[ "$1" =~ ^[0-9]+(\.[0-9]+)?$ ]] && awk -v s="$1" 'BEGIN { exit !(s + 0 > 0) }'; }
# Fixed-point, trailing zeros trimmed: never scientific notation, which _bounded_secs_ok rejects.
_bounded_mul() { awk -v s="$1" -v f="$2" 'BEGIN { r = sprintf("%.6f", s * f); sub(/0+$/, "", r); sub(/\.$/, "", r); print r }'; }
BOUNDED_SH="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/$(basename "${BASH_SOURCE[0]}")"
BOUNDED_EXEC="$(dirname "$BOUNDED_SH")/bounded_exec.py"
BOUNDED_DIR="$(mktemp -d "${BOUNDED_PARENT:-${TMPDIR:-/tmp}}/bounded.XXXXXX")"
BOUNDED_OUTCOME_FILE="$BOUNDED_DIR/outcome"
BOUNDED_LOG="$BOUNDED_DIR/log"
# The watchdog form appends to its caller's log, handed over explicitly on its command line.
if [ "${BASH_SOURCE[0]}" = "$0" ]; then
  # A standalone watchdog removes its state however it ends.
  trap 'rm -rf "$BOUNDED_DIR"' EXIT
  [ -z "${BOUNDED_WATCHDOG_LOG:-}" ] || BOUNDED_LOG="$BOUNDED_WATCHDOG_LOG"
fi
: >> "$BOUNDED_LOG"
BOUNDED_HAVE_WATCHDOG=1
if ! command -v python3 >/dev/null 2>&1; then
  BOUNDED_HAVE_WATCHDOG=0
  [ "${BASH_SOURCE[0]}" = "$0" ] || echo "  warn python3 not found — commands run without a hang watchdog" >&2
fi

bounded_is_incomplete() {
  [ "$1" = 124 ] && [ "$(cat "$BOUNDED_OUTCOME_FILE" 2>/dev/null)" = incomplete ]
}

bounded_run() {
  local stdin_file="" retry="" secs="" rc tries=0 budget tmp outcome
  while [ $# -gt 0 ]; do
    case "$1" in
      -i) stdin_file="$2"; shift 2 ;;
      -r) retry="$2"; shift 2 ;;
      --) shift; break ;;
      *) if [ -z "$secs" ]; then secs="$1"; shift; else break; fi ;;
    esac
  done
  [ -n "$secs" ] && [ $# -gt 0 ] || { echo "bounded_run: usage: bounded_run [-i FILE] [-r SECS] SECS -- CMD..." >&2; return 2; }
  [ -n "$retry" ] || retry="$(_bounded_mul "$secs" "$BOUNDED_RETRY_FACTOR")"
  if ! _bounded_secs_ok "$secs" || ! _bounded_secs_ok "$retry"; then
    echo "bounded_run: budgets must be positive numbers of seconds (got $secs, retry $retry)" >&2; return 2
  fi
  echo done > "$BOUNDED_OUTCOME_FILE"
  if [ "$BOUNDED_HAVE_WATCHDOG" != 1 ]; then
    if [ -n "$stdin_file" ]; then "$@" < "$stdin_file"; else "$@"; fi
    return $?
  fi
  tmp="$(mktemp -d "$BOUNDED_DIR/run.XXXXXX")"
  budget="$secs"
  while :; do
    tries=$((tries + 1))
    rm -f "$tmp/status"
    if [ -n "$stdin_file" ]; then
      python3 -I "$BOUNDED_EXEC" --timeout "$budget" --kill-after "${BOUNDED_KILL_AFTER:-10}" --status "$tmp/status" -- "$@" < "$stdin_file" > "$tmp/out" 2> "$tmp/err"
    else
      python3 -I "$BOUNDED_EXEC" --timeout "$budget" --kill-after "${BOUNDED_KILL_AFTER:-10}" --status "$tmp/status" -- "$@" > "$tmp/out" 2> "$tmp/err"
    fi
    outcome="$(cat "$tmp/status" 2>/dev/null)"
    case "$outcome" in
      "exit "*) break ;;
      timeout) [ "$tries" = 1 ] || break; budget="$retry" ;;
      *) outcome=runner-failed; break ;;
    esac
  done
  if [ "$outcome" = timeout ]; then
    echo incomplete > "$BOUNDED_OUTCOME_FILE"
    echo "incomplete ${secs}s+${retry}s $*" >> "$BOUNDED_LOG"
    rc=124
  elif [ "$outcome" = runner-failed ]; then
    # The runner reported nothing (it failed or was cancelled): no observation, so the run is
    # incomplete exactly as a double timeout is (status 124, bounded_is_incomplete true).
    echo "bounded_run: the runner reported no outcome for: $*" >&2
    echo incomplete > "$BOUNDED_OUTCOME_FILE"
    echo "runner-failed ${secs}s $*" >> "$BOUNDED_LOG"
    rc=124
  else
    cat "$tmp/out"; cat "$tmp/err" >&2
    rc="${outcome#exit }"
    [ "$tries" = 2 ] && echo "recovered ${secs}s $*" >> "$BOUNDED_LOG"
  fi
  rm -rf "$tmp"
  return "$rc"
}

bounded_summary() {
  local nrec ninc
  nrec=$(grep -c '^recovered' "$BOUNDED_LOG" 2>/dev/null || true); nrec=${nrec:-0}
  ninc=$(grep -c '^incomplete\|^runner-failed' "$BOUNDED_LOG" 2>/dev/null || true); ninc=${ninc:-0}
  [ "$nrec" -gt 0 ] || [ "$ninc" -gt 0 ] || return 0
  echo "Bounded runs: $nrec recovered (timed out, then completed alone), $ninc incomplete (timed out twice, or the runner failed):"
  sed 's/^/  /' "$BOUNDED_LOG"
}

bounded_watchdog() {
  if [ "$BOUNDED_HAVE_WATCHDOG" = 1 ]; then
    TO=(env "BOUNDED_WATCHDOG_LOG=$BOUNDED_LOG" "BOUNDED_PARENT=$BOUNDED_DIR" bash "$BOUNDED_SH" "$1")
  else
    TO=(env)  # no python3: run commands directly (the gate warned once when it sourced this file)
  fi
}

bounded_gate_status() {
  local ninc
  bounded_summary
  ninc=$(grep -c '^incomplete\|^runner-failed' "$BOUNDED_LOG" 2>/dev/null || true); ninc=${ninc:-0}
  [ "$ninc" -eq 0 ] || echo "INCOMPLETE: $ninc command(s) timed out twice or lost their runner: this run is incomplete, not passed"
  rm -rf "$BOUNDED_DIR"
  [ "$ninc" -eq 0 ]
}

# Watchdog form: `bash bounded.sh SECS CMD...` (through bounded_watchdog's TO array).
if [ "${BASH_SOURCE[0]}" = "$0" ]; then
  [ $# -ge 2 ] || { echo "usage: bash $0 SECS CMD [ARGS...]" >&2; exit 2; }
  secs="$1"; shift
  bounded_run "$secs" -- "$@"; rc=$?
  if bounded_is_incomplete "$rc"; then
    echo "INCOMPLETE: timed out twice (${secs}s, then $(_bounded_mul "$secs" "$BOUNDED_RETRY_FACTOR")s alone) or lost its runner: $*" >&2
  fi
  exit "$rc"
fi
