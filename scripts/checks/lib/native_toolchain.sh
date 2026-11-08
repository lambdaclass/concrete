#!/usr/bin/env bash
# One private scratch directory for each constructor, never ambient TMPDIR.
native_tmp=$(mktemp -d /tmp/concrete-native.XXXXXXXX) || return 1
trap 'rm -rf "$native_tmp"' EXIT
export CONCRETE_NATIVE_TMP="$native_tmp"
policy_shell=$(python3 "$policy_root/link_policy.py" --shell) || return 1
eval "$policy_shell"
cc=("${clean[@]}" "${driver[@]}")
link_native() {
  local output=$1 variant=normal; shift
  rm -f "$output"
  if [ "${1:-}" = --sanitize ]; then variant=sanitize; shift; fi
  # Validate the compile driver as well: consumed -B/-F/-L flags cannot vanish
  # inside clang or Nix wrappers before validation.
  python3 "$policy_root/link_policy.py" --driver "${cc[@]}" || return 1
  if ! python3 "$policy_root/link_command.py" "$output.command" "$output" "$variant" "$@" >"$output.trace" 2>&1; then
    cat "$output.trace" >&2; return 1
  fi
  local purity_mode=()
  if [ "$variant" = sanitize ]; then purity_mode=(--sanitize); fi
  if ! python3 "$policy_root/link_purity.py" "$output" "$output.trace" "${purity_mode[@]}" "$@"; then
    rm -f "$output"; return 1
  fi
}
