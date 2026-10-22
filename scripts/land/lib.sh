# Shared helpers for scripts/land/*.sh (sourced, never run). docs/project/LANDING.md is the manual.
#
#   land_in_nix cmd...          run cmd inside `nix develop` unless already there (LAND_NO_NIX=1 never)
#   land_assert_no_op what      die if a merge, rebase, cherry-pick or revert is in progress
#   legs_start name log cmd...  run cmd in the background as a named leg, output to log
#   legs_wait                   wait for every leg; sets LEGS_FAILED, LEGS_SUM, LEGS_REPORT
#
# A LEG is one independent validation (impact gates, remote full gates, Codex review). Legs of one
# candidate run at the same time and are all collected: one failing leg fails the whole, and no
# leg is abandoned while another is still running. LEGS_SUM is the sequential cost (the sum of the
# legs' durations); the caller compares it with the wall time to record what pipelining saved.

land_in_nix() {
  if [ "${LAND_NO_NIX:-0}" = 1 ] || [ -n "${IN_NIX_SHELL:-}" ] || ! command -v nix >/dev/null 2>&1 \
     || [ ! -f "$(git rev-parse --show-toplevel)/flake.nix" ]; then
    "$@"
  else
    nix develop "$(git rev-parse --show-toplevel)" --command "$@"
  fi
}

land_assert_no_op() {
  local what="$1" p
  for p in MERGE_HEAD CHERRY_PICK_HEAD REVERT_HEAD rebase-merge rebase-apply; do
    if [ -e "$(git rev-parse --git-path "$p")" ]; then
      echo "land: after $what: a git operation is still in progress ($p exists); stopping" >&2
      exit 1
    fi
  done
}

LEG_NAMES=(); LEG_PIDS=(); LEG_LOGS=()
legs_start() { # name log cmd...
  local n="$1" log="$2"; shift 2
  rm -f "$log.rc"
  (
    s="$(date +%s)"; rc=0
    "$@" > "$log" 2>&1 || rc=$?
    echo "$rc $(( $(date +%s) - s ))" > "$log.rc"
    exit "$rc"
  ) &
  LEG_NAMES+=("$n"); LEG_PIDS+=("$!"); LEG_LOGS+=("$log")
}

legs_wait() {
  LEGS_FAILED=""; LEGS_SUM=0; LEGS_REPORT=""
  local i rc d
  for i in "${!LEG_PIDS[@]}"; do
    wait "${LEG_PIDS[$i]}" 2>/dev/null || true
    rc=255; d=0
    [ -f "${LEG_LOGS[$i]}.rc" ] && read -r rc d < "${LEG_LOGS[$i]}.rc"
    LEGS_SUM=$(( LEGS_SUM + d ))
    LEGS_REPORT="${LEGS_REPORT:+$LEGS_REPORT }${LEG_NAMES[$i]}=$([ "$rc" = 0 ] && echo pass || echo "fail($rc)")/${d}s"
    [ "$rc" = 0 ] || LEGS_FAILED="${LEGS_FAILED:+$LEGS_FAILED }${LEG_NAMES[$i]}"
  done
  LEG_NAMES=(); LEG_PIDS=(); LEG_LOGS=()
}
