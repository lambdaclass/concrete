# shellcheck shell=bash
# One policy owner for production and real-trace controls.
mapfile -d '' -t trusted_trace_flags < <(python3 -I scripts/checks/lib/execve_names.py --trace-flags)
[ "${#trusted_trace_flags[@]}" -gt 0 ] || { echo "FAIL trace: missing trace policy" >&2; return 1; }
