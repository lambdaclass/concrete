# shellcheck shell=bash
# The ONE reader of `--report stack-depth`'s `Max stack bound:` summary line (bug 065).
#
# Every consumer of the program-level stack figure -- `[policy] max_stack_bytes`
# (check_policy.sh), `[allocation] stack_max_bytes` (check_assumptions.sh) and the release
# bundle's `evidence.max_stack_bytes` (capture_release_bundle.sh) -- goes through here, so the
# three cannot disagree about what the line means. Each used to grep `Max stack bound:\s*[0-9]+`
# on its own, and the compiler used to print a number even when some function had no bound, so a
# budget PASSED a program with unbounded stack. The producer now prints `unbounded` in that case
# (Concrete/Report/Report.lean, stackSummaryLine); this reader treats it, and anything it does not
# recognise, as a refusal -- never as a number.

# stack_max_bound: read stack-depth report text on stdin.
#   prints N           exit 0  -- every function bounded; the maximum is N bytes
#   prints unbounded   exit 3  -- at least one function has no bound
#   prints nothing     exit 2  -- no recognisable summary line (fail closed)
stack_max_bound() {
  local line
  line="$(grep -E '^Max stack bound:' | head -1)"
  if [[ "$line" =~ ^Max\ stack\ bound:\ unbounded ]]; then
    echo unbounded; return 3
  elif [[ "$line" =~ ^Max\ stack\ bound:\ ([0-9]+)\ bytes ]]; then
    echo "${BASH_REMATCH[1]}"; return 0
  fi
  return 2
}

# stack_budget_check KEY BUDGET REPORT_TEXT
#   Prints one `  ok   ...` or `  FAIL ...` line; returns 0 iff the budget is met by a BOUNDED
#   maximum. Unbounded, unparseable, or a non-integer budget all FAIL.
stack_budget_check() {
  local key="$1" budget="$2" report="$3" actual rc
  if ! [[ "$budget" =~ ^[0-9]+$ ]]; then
    echo "  FAIL $key='$budget' is not a byte count"; return 1
  fi
  actual="$(stack_max_bound <<<"$report")"; rc=$?
  case "$rc" in
    0)
      if [ "$actual" -gt "$budget" ]; then
        echo "  FAIL $key=$budget but actual max is $actual"; return 1
      fi
      echo "  ok   $key=$budget (actual=$actual)"; return 0 ;;
    3)
      echo "  FAIL $key=$budget but the stack is UNBOUNDED: $(grep -E '^Max stack bound:' <<<"$report" | head -1 | sed 's/^Max stack bound: //')"
      return 1 ;;
    *)
      echo "  FAIL $key=$budget but stack-depth report has no max bound"; return 1 ;;
  esac
}

# stack_bound_json REPORT_FILE: the JSON value for evidence.max_stack_bytes -- a number, the
# string "unbounded", or the string "unknown" when the report is missing or unparseable.
stack_bound_json() {
  local v rc
  v="$( { stack_max_bound < "$1"; } 2>/dev/null)"; rc=$?
  case "$rc" in
    0) echo "$v" ;;
    3) echo '"unbounded"' ;;
    *) echo '"unknown"' ;;
  esac
}
