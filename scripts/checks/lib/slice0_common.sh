# shellcheck shell=bash
# Shared machinery of the slice-0 example gates (PROVISIONAL): tests.sh (gates 4, 6, 6a)
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
#   manifest_rows KIND: validated rows, with the bir: inventory checked against the emitter;
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
# SLICE0_BUILD_SECS lets a gate with a total deadline include this build in that deadline.
# SLICE0_BUILD_TARGETS lets a gate name every library or module its driver imports (default:
# Concrete and ConcreteSpec.Spec.Slice0, so fresh checkouts have the complete import closure).
read -r -a slice0_build_targets <<<"${SLICE0_BUILD_TARGETS:-Concrete ConcreteSpec.Spec.Slice0}"
bounded "${SLICE0_BUILD_SECS:-1800}" lake build "${slice0_build_targets[@]}" lean-compiler-backend/lcb >"$work/lake-build.log" 2>&1 \
  || { tail -n 200 "$work/lake-build.log" >&2; echo "slice0: lake build ${slice0_build_targets[*]} failed" >&2; exit 1; }
for slice0_mod in "${slice0_build_targets[@]}"; do
  slice0_olean="$root/.lake/build/lib/lean/${slice0_mod//.//}.olean"
  [ -f "$slice0_olean" ] || { echo "slice0: missing built olean $slice0_olean for target $slice0_mod" >&2; exit 1; }
done
lcb="$root/.lake/packages/lean-compiler-backend/.lake/build/bin/lcb"
case "$(uname -s)-$(uname -m)" in
  Linux-x86_64) target=x86_64-linux ;;
  Linux-aarch64) target=aarch64-linux ;;
  Darwin-arm64) target=aarch64-macos ;;
  *) echo "slice0: unsupported host $(uname -s)-$(uname -m)" >&2; exit 2 ;;
esac
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

test_manifest=${CONCRETE_TEST_MANIFEST:-tests/manifest.tsv}
manifest_rows() {
  local kind=$1 rows emitter_names name
  rows=$(bounded 30 python3 -I "$root/scripts/checks/lib/test_manifest.py" rows --kind "$kind" "$test_manifest" | cap_out) ||
    { echo "slice0: reading $test_manifest failed" >&2; exit 1; }
  [ -n "$rows" ] || { echo "slice0: no $kind tests listed in $test_manifest" >&2; exit 1; }
  if [ "$kind" = bir ]; then
    printf '%s\n' "$rows" | awk -F '\t' '{sub(/^bir:/, "", $2); print $2}' |
      LC_ALL=C sort >"$work/manifest-programs"
    emitter_names=$("${emitter[@]}" list | cap_out) ||
      { echo "slice0: listing emitter programs failed" >&2; exit 1; }
    printf '%s\n' "$emitter_names" | LC_ALL=C sort >"$work/emitter-programs"
    comm -13 "$work/manifest-programs" "$work/emitter-programs" >"$work/programs-without-rows"
    comm -23 "$work/manifest-programs" "$work/emitter-programs" >"$work/rows-without-programs"
    if [ -s "$work/programs-without-rows" ] || [ -s "$work/rows-without-programs" ]; then
      while IFS= read -r name; do
        echo "slice0: program without a manifest row: $name" >&2
      done <"$work/programs-without-rows"
      while IFS= read -r name; do
        echo "slice0: manifest row names no program: $name" >&2
      done <"$work/rows-without-programs"
      exit 1
    fi
  fi
  printf '%s\n' "$rows"
}
