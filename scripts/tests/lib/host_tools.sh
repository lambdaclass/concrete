# Host tools a workflow gate command needs beyond the repository's own toolchain.
#
# The CI workflow runs some gates in a mode that needs a tool only its runner image has: the
# Examples job runs `EXAMPLE_VALGRIND=1 bash ./scripts/tests/check_example_outputs.sh`, and valgrind
# does not exist on macOS. The gate itself refuses to run without the tool (fail closed, correct for
# CI). `run_ci_gates_local.sh` uses this table to report such a command as an explicit, NAMED skip
# that blocks a full or closure verdict, instead of a misleading FAIL or a silent pass.
#
# One row per requirement: "<substring of the normalized command>|<tool on PATH>".
HOST_TOOL_TABLE=(
  "EXAMPLE_VALGRIND=1 |valgrind"
  # The external checkers (con-leche, nanoda) build against elan-managed toolchains; the gate refuses
  # (exit 2) without $ELAN_HOME/bin/lake. CI installs elan; a nix-only host (concrete-ci-1) has none.
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

# host_tool_missing <cmd>: prints the required tools absent from PATH (space-separated) and returns
# 0 when at least one is missing; returns 1 when the host can run <cmd>.
host_tool_missing() {
  local tool missing=""
  while IFS= read -r tool; do
    [ -n "$tool" ] || continue
    command -v "$tool" >/dev/null 2>&1 || missing="$missing${missing:+ }$tool"
  done < <(host_tool_required "$1")
  [ -n "$missing" ] || return 1
  printf '%s\n' "$missing"
  return 0
}
