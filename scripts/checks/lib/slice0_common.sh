# shellcheck shell=bash
# Shared machinery of the slice-0 example gates (PROVISIONAL): slice0_backends.sh (gates 4, 6, 6a)
# and slice0_artifacts.sh (gates 10, 11, 12). Source it from the repository root, inside the nix
# devshell, with `set -euo pipefail`. It builds the compiler and LCB's command line under the
# gate's limits (lib/limits.sh: an address-space cap of SLICE0_GATE_MEM_GB GiB, default 64; 0 for
# a gate whose executables under test need no cap, slice0_artifacts.sh) and defines:
#   capped (a command prefix, an array): run under a 64 GiB address-space cap of its own, so
#     every build and analysis command is capped whatever the gate's cap;
#   bounded SECS CMD...: CMD under capped and bounded_run (the gates' only time bound);
#   root, work (removed on exit), lcb, target, emitter (the Lean emitter command, an array);
#   expected_timeout CMD...: CMD with its bounded runs logged apart (a control expecting a timeout);
#   observe EXE OUT SECS RETRY [MEM_MB [ENV...]]: run EXE under bounded_run (and limited_exec.sh
#     MEM_MB, default 1024) with ENV assignments; OUT.status (status or "incomplete"),
#     OUT.stdout, OUT.stderr;
#   judge EXPECTED_DIR OBS: the observation OBS.* equals the expectation, byte for byte;
#   build NAME LOG CMD...: run CMD, or print LOG and fail the gate;
#   emit_examples: the example names, one per line, failing if there are none;
#   cap_out: copy stdin to stdout, failing above 1 MiB: every command output a gate captures
#     into a variable goes through it, so no producer can grow the gate's shell (which may run
#     with no address-space cap, see SLICE0_GATE_MEM_GB) without bound.
root=$(pwd)
work=$(mktemp -d)
# On every exit, the bounded-run summary (recovered and INCOMPLETE runs, whatever a caller did
# with a command's stderr) goes to the gate's stdout before the scratch directory is removed, and a
# gate that would pass with an INCOMPLETE run exits 3 instead (INCOMPLETE is never a pass).
trap 'rc=$?; bounded_summary || true
  if [ "$rc" = 0 ] && grep -Eqs "^(incomplete|runner-failed)" "${BOUNDED_LOG:-/nonexistent}"; then
    echo "INCOMPLETE: a bounded run timed out twice; not a pass"; rc=3
  fi
  rm -rf "${work:?}"; exit "$rc"' EXIT
BOUNDED_PARENT="$work"
# shellcheck source=scripts/checks/lib/bounded.sh
source scripts/checks/lib/bounded.sh
[ "$BOUNDED_HAVE_WATCHDOG" = 1 ] || { echo "slice0: python3 is required for bounded runs" >&2; exit 2; }
# shellcheck source=scripts/checks/lib/limits.sh
source scripts/checks/lib/limits.sh
gate_limits "${SLICE0_GATE_MEM_GB:-64}" 40000
capped=(bash "$root/scripts/checks/lib/limited_exec.sh" 65536)
# bounded SECS CMD...: CMD under `capped`, through bounded_run (stdin /dev/null; a timeout is
# re-run once with a larger budget; a second timeout writes no output, returns 124 and is reported
# INCOMPLETE, never a pass).
bounded() {
  local secs=$1 rc=0
  shift
  bounded_run -i /dev/null "$secs" -- "${capped[@]}" "$@" || rc=$?
  if [ "$rc" != 0 ] && bounded_is_incomplete "$rc"; then
    echo "slice0: INCOMPLETE (timed out twice): $*" >&2
  fi
  return "$rc"
}
# The build's output is kept and shown on failure: lake prints only "error: build failed" last.
bounded 1800 lake build Concrete lean-compiler-backend/lcb >"$work/lake-build.log" 2>&1 \
  || { tail -n 200 "$work/lake-build.log" >&2; echo "slice0: lake build Concrete failed" >&2; exit 1; }
lcb="$root/.lake/packages/lean-compiler-backend/.lake/build/bin/lcb"
target=$(bash "$root/scripts/checks/lib/slice0_native.sh" target)
# The time bounds of the emit step (the Lean emitter) and the native step (slice0_native.sh).
emit_secs=120
native_secs=300
emitter=(bounded "$emit_secs" lake env lean --run scripts/checks/compiler/slice0_bir.lean)

# expected_timeout CMD...: run CMD (a function or command) with its bounded runs logged apart, for
# a control whose expected observation IS a timeout (the diverging body): its INCOMPLETE runs are
# the observation that CMD checks, not an incomplete gate, so the exit trap does not count them.
expected_timeout() {
  local BOUNDED_LOG="$work/expected-timeouts.log"
  "$@"
}

observe() {
  local rc=0 mem=${5:-1024}
  local envs=("${@:6}")
  bounded_run -i /dev/null -r "$4" "$3" -- env ${envs[@]+"${envs[@]}"} \
    bash "$root/scripts/checks/lib/limited_exec.sh" "$mem" "$1" \
    >"$2.stdout" 2>"$2.stderr" || rc=$?
  if bounded_is_incomplete "$rc"; then echo incomplete >"$2.status"; else echo "$rc" >"$2.status"; fi
}

judge() {
  cmp -s "$1/status" "$2.status" && cmp -s "$1/stderr" "$2.stderr" && [ ! -s "$2.stdout" ]
}

build() {
  local name=$1 log=$2; shift 2
  "$@" >"$log" 2>&1 || { cat "$log" >&2; echo "slice0: $name: build failed: $*" >&2; exit 1; }
}

cap_out() {
  local LC_ALL=C out
  out=$(head -c 1048577; echo x)
  out=${out%x}
  [ "${#out}" -le 1048576 ] || { echo "slice0: captured output over 1 MiB" >&2; return 1; }
  printf '%s' "$out"
}

emit_examples() {
  local names
  names=$("${emitter[@]}" list | cap_out) || { echo "slice0: listing the examples failed" >&2; exit 1; }
  [ -n "$names" ] || { echo "slice0: no examples listed" >&2; exit 1; }
  printf '%s\n' "$names"
}
