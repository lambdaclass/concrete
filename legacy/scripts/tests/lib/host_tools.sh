# Host tools a workflow gate command needs beyond the repository's own toolchain.
#
# The CI workflow runs some gates in a mode that needs a tool only its runner image has: the
# Examples job runs `EXAMPLE_VALGRIND=1 bash ./scripts/tests/check_example_outputs.sh`, and valgrind
# does not exist on macOS. The gate itself refuses to run without the tool (fail closed, correct for
# CI). `run_ci_gates_local.sh` uses this table to report such a command as an explicit, NAMED skip
# that blocks a full or closure verdict, instead of a misleading FAIL or a silent pass.
#
# One row per requirement: "<substring of the normalized command>|<tool>". A tool is present when it
# is on PATH, except where `host_tool_present` asks the same question the gate itself asks.
HOST_TOOL_TABLE=(
  "EXAMPLE_VALGRIND=1 |valgrind"
  # The external checkers (con-leche, nanoda) build against elan-managed toolchains; the gate refuses
  # (exit 2) without $ELAN_HOME/bin/lake. CI installs elan; a nix-only host (concrete-ci-1) has none.
  # Present means that lake exists, not that elan is on PATH: the gate host keeps elan in ~/.elan
  # off its non-login ssh PATH, and asking PATH skipped a gate it could run (2026-10-23).
  "check_external_checker.sh|elan"
)

# host_tool_required <cmd>: prints the tools <cmd> needs (one per line); nothing when none.
host_tool_required() {
  local cmd="$1" row pat tool
  for row in "${HOST_TOOL_TABLE[@]}"; do
    pat="${row%%|*}"; tool="${row#*|}"
    case "$cmd" in *"$pat"*) printf '%s\n' "$tool" ;; esac
  done
}

# host_tool_present <tool>: whether this host has <tool>, asked the way the gate that needs it asks.
host_tool_present() {
  case "$1" in
    elan) [ -x "${ELAN_HOME:-$HOME/.elan}/bin/lake" ] ;;
    *) command -v "$1" >/dev/null 2>&1 ;;
  esac
}

# host_tool_missing <cmd>: prints the required tools this host lacks (space-separated) and returns
# 0 when at least one is missing; returns 1 when the host can run <cmd>.
host_tool_missing() {
  local tool missing=""
  while IFS= read -r tool; do
    [ -n "$tool" ] || continue
    host_tool_present "$tool" || missing="$missing${missing:+ }$tool"
  done < <(host_tool_required "$1")
  [ -n "$missing" ] || return 1
  printf '%s\n' "$missing"
  return 0
}

# GATES THAT DEFER A VERDICT TO CI (bug 315). Some verdicts only a capable host can make soundly: the
# compile-scaling gate judges growth and budgets by retired instructions, and a host whose perf cannot
# count instructions:u (the dev host: perf_event_paranoid 4) can only offer wall ratios, which measure
# its caches and load, not the compiler. Such a gate still runs everywhere (what does not depend on
# the host is judged), prints NOT-JUDGED for the rest, and exits GATE_DEFERRED_RC when nothing failed.
# run_ci_gates_local.sh records that exit as DEFERRED (not passed, not failed) only for a gate named
# below that CI runs as an unconditional step of its own; from any other gate, or from a gate whose CI
# step was removed or made conditional, the same exit is a FAIL. A deferral is a skip, never a pass: it
# keeps the run PARTIAL (completed=0) and is named in .ci-gates-summary and the pre-push record.
#
# One row per gate: "<gate file name>|<judged_on>". judged_on says where the verdict is decided:
#   ci-counted   on CI, by a runner whose perf counts instructions:u; the gate FAILS on a CI runner
#                that cannot count, so the verdict cannot be deferred again there.
GATE_DEFERRED_RC=77
HOST_DEFERRED_TABLE=(
  "check_compile_scaling.sh|ci-counted"
)
_ht_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=scripts/tests/lib/delegated_gates.sh
source "$_ht_dir/delegated_gates.sh" || return 2

# host_gate_judged_on <cmd>: prints the judged_on of the row naming <cmd>'s gate; 1 when no row does.
host_gate_judged_on() {
  local row
  row="$(_host_deferred_row "$1")" || return 1
  printf '%s\n' "${row#*|}"
}

_host_deferred_row() { # cmd -> the row whose gate <cmd> INVOKES
  # The command must BE the gate: bash/python3/sh or ./, the gate's path, then plain arguments. No
  # VAR=value prefix: BASH_ENV or ENV would run a startup file that can exit 77 before the gate does. A gate named only as an argument of
  # another command, or inside a pipeline, list or substitution, matches no row: that command's exit
  # 77 is not the declared gate's.
  local cmd="$1" row script
  # Anything that could make the command more than one plain invocation, anywhere in it (prefixes
  # included: `X=x;(exit 77);# ...` is a list), or any line break, or a comment.
  case "$cmd" in *[$'\n\r''|;&<>`$()#\\{}']*) return 1 ;; esac
  [[ "$cmd" =~ ^[A-Za-z_][A-Za-z0-9_]*= ]] && return 1
  if [[ "$cmd" =~ ^(bash|python3|sh)[[:space:]]+(.*)$ ]]; then cmd="${BASH_REMATCH[2]}"; fi
  cmd="${cmd#./}"
  script="${cmd%%[[:space:]]*}"
  for row in "${HOST_DEFERRED_TABLE[@]}"; do
    [ "$script" = "scripts/tests/${row%%|*}" ] && { printf '%s\n' "$row"; return 0; }
  done
  return 1
}

# gate_deferral <cmd> [workflow]: 0 when <cmd> may report GATE_DEFERRED_RC as deferred to CI: a row
# declares its gate and the workflow runs that gate as an unconditional step (delegated_gate_step).
# Otherwise prints why not and returns 1, and the runner counts the exit as a failure.
gate_deferral() {
  local cmd="$1" wf="${2:-.github/workflows/lean_action_ci.yml}" row
  if ! row="$(_host_deferred_row "$cmd")"; then
    echo "exit $GATE_DEFERRED_RC (not judged here) from a gate host_tools.sh does not declare judged on CI"
    return 1
  fi
  case "${row#*|}" in
    ci-counted) ;;
    *) echo "exit $GATE_DEFERRED_RC (not judged here), but ${row%%|*} declares judged_on '${row#*|}', which this file does not define"
       return 1 ;;
  esac
  delegated_gate_step "${row%%|*}" "$wf" >/dev/null && return 0
  echo "exit $GATE_DEFERRED_RC (not judged here), but ${row%%|*} is not an unconditional CI step of $wf: nothing would judge it"
  return 1
}
