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
#     Runs CMD under timeout(1). Its stdout and stderr are buffered per attempt and only the final
#     attempt's output is written (so `out=$(bounded_run ...)` and `bounded_run ... > f` never see
#     a timed-out attempt's partial output). Without -i the command inherits stdin, which a re-run
#     cannot replay: callers that feed input pass -i. Returns CMD's status; 124 means the command
#     timed out on both attempts (INCOMPLETE) — bounded_is_incomplete tests that. RETRY_SECS
#     defaults to BOUNDED_RETRY_FACTOR (2) times SECS.
#
#   TO="bash scripts/tests/lib/bounded.sh 300"; $TO CMD [ARGS...]
#     Watchdog form for gates that wrap single commands: the same retry, then, on a second
#     timeout, an "INCOMPLETE: ..." line on stderr and status 124.
#
# When BOUNDED_LOG names a file, every recovered and every incomplete run appends one line to it
# ("recovered <secs> <cmd>" / "incomplete <secs>+<retry> <cmd>"): gates read it to count recovered
# runs and to print what was incomplete, also from subshells (`(cd dir && bounded_run ...)`).
#
# Without timeout(1) (outside the devshell on macOS) the command runs once with no watchdog and a
# warning on stderr, as the gates' previous `TO=""` fallback did.

BOUNDED_RETRY_FACTOR="${BOUNDED_RETRY_FACTOR:-2}"

bounded_is_incomplete() { [ "$1" = 124 ]; }

bounded_run() {
  local stdin_file="" retry="" secs rc tries=0 budget tmp
  while [ $# -gt 0 ]; do
    case "$1" in
      -i) stdin_file="$2"; shift 2 ;;
      -r) retry="$2"; shift 2 ;;
      --) shift; break ;;
      *) if [ -z "${secs:-}" ]; then secs="$1"; shift; else break; fi ;;
    esac
  done
  [ -n "${secs:-}" ] && [ $# -gt 0 ] || { echo "bounded_run: usage: bounded_run [-i FILE] [-r SECS] SECS -- CMD..." >&2; return 2; }
  [ -n "$retry" ] || retry=$((secs * BOUNDED_RETRY_FACTOR))
  if ! command -v timeout >/dev/null 2>&1; then
    echo "  warn 'timeout' not found — running without a hang watchdog" >&2
    if [ -n "$stdin_file" ]; then "$@" < "$stdin_file"; else "$@"; fi
    return $?
  fi
  tmp="$(mktemp -d)"
  budget="$secs"
  while :; do
    tries=$((tries + 1))
    if [ -n "$stdin_file" ]; then
      timeout "$budget" "$@" < "$stdin_file" > "$tmp/out" 2> "$tmp/err"; rc=$?
    else
      timeout "$budget" "$@" > "$tmp/out" 2> "$tmp/err"; rc=$?
    fi
    if [ "$rc" = 124 ] && [ "$tries" = 1 ]; then budget="$retry"; continue; fi
    break
  done
  cat "$tmp/out"; cat "$tmp/err" >&2
  rm -rf "$tmp"
  if [ "$tries" = 2 ] && [ -n "${BOUNDED_LOG:-}" ]; then
    if [ "$rc" = 124 ]; then echo "incomplete ${secs}s+${retry}s $*" >> "$BOUNDED_LOG"
    else echo "recovered ${secs}s $*" >> "$BOUNDED_LOG"; fi
  fi
  return "$rc"
}

# Watchdog form: `bash bounded.sh SECS CMD...`.
if [ "${BASH_SOURCE[0]}" = "$0" ]; then
  [ $# -ge 2 ] || { echo "usage: bash $0 SECS CMD [ARGS...]" >&2; exit 2; }
  secs="$1"; shift
  bounded_run "$secs" -- "$@"; rc=$?
  if bounded_is_incomplete "$rc"; then
    echo "INCOMPLETE: timed out twice (${secs}s, then $((secs * BOUNDED_RETRY_FACTOR))s alone): $*" >&2
  fi
  exit "$rc"
fi
