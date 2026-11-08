#!/usr/bin/env bash
# Source after choosing target and changing to the object directory. This is the
# sole environment/driver construction, shared with the surrogate-sysroot control.
# Fixed epoch rather than wall clock or caller environment. No debug data is requested, and
# prefix maps also cover future debug additions. Separate objects avoid random linker temp names.
eval "$(python3 "$policy_root/link_policy.py" --shell)"
cc=("${clean[@]}" "${driver[@]}")
expected_cc=("${cc[@]}")
expected_link=("${link[@]}")
# Every link is traced and validated before the executable is released to callers.
link_native() {
  local output=$1; shift
  rm -f "$output"
  if [ "$(printf '%q ' "${cc[@]}")" != "$(printf '%q ' "${expected_cc[@]}")" ] || [ "$(printf '%q ' "${link[@]}")" != "$(printf '%q ' "${expected_link[@]}")" ]; then
    echo 'link command: driver differs from flake policy' >&2; return 1
  fi
  python3 "$policy_root/link_command.py" --inputs "$@" || return 1
  # Validate the effective driver/linker search arguments before opening inputs.
  if ! "${cc[@]}" "${link[@]}" -### "$@" -o "$output" >"$output.command" 2>&1; then
    cat "$output.command" >&2; return 1
  fi
  if ! python3 "$policy_root/link_command.py" "$output.command"; then
    return 1
  fi
  if ! "${cc[@]}" "${link[@]}" -Wl,-t "$@" -o "$output" >"$output.trace" 2>&1; then
    cat "$output.trace" >&2; return 1
  fi
  if ! python3 "$policy_root/link_purity.py" "$output" "$output.trace" "$@"; then
    rm -f "$output"
  if [ "$(printf '%q ' "${cc[@]}")" != "$(printf '%q ' "${expected_cc[@]}")" ] || [ "$(printf '%q ' "${link[@]}")" != "$(printf '%q ' "${expected_link[@]}")" ]; then
    echo 'link command: driver differs from flake policy' >&2; return 1
  fi
  python3 "$policy_root/link_command.py" --inputs "$@" || return 1; return 1
  fi
}
