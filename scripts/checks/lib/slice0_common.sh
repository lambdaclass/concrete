# shellcheck shell=bash
# Shared machinery of the slice-0 example gates (PROVISIONAL): slice0_backends.sh (gates 4, 6, 6a)
# and slice0_artifacts.sh (gates 10, 11, 12). Source it from the repository root, inside the nix
# devshell, with `set -euo pipefail`. It builds the compiler and LCB's command line under the
# gate's limits and defines:
#   root, work (removed on exit), lcb, target, emitter (the Lean emitter command, an array);
#   observe EXE OUT SECS RETRY [MEM_MB [ENV...]]: run EXE under bounded_run (and limited_exec.sh
#     MEM_MB, default 1024) with ENV assignments; OUT.status (status or "incomplete"),
#     OUT.stdout, OUT.stderr;
#   judge EXPECTED_DIR OBS: the observation OBS.* equals the expectation, byte for byte;
#   build NAME LOG CMD...: run CMD, or print LOG and fail the gate;
#   emit_examples: the example names, one per line, failing if there are none.
root=$(pwd)
work=$(mktemp -d)
trap 'rm -rf "${work:?}"' EXIT
BOUNDED_PARENT="$work"
# shellcheck source=scripts/checks/lib/bounded.sh
source scripts/checks/lib/bounded.sh
[ "$BOUNDED_HAVE_WATCHDOG" = 1 ] || { echo "slice0: python3 is required for bounded runs" >&2; exit 2; }
# shellcheck source=scripts/checks/lib/limits.sh
source scripts/checks/lib/limits.sh
gate_limits 64 40000
timeout -k 30 1800 lake build Concrete lean-compiler-backend/lcb >/dev/null
lcb="$root/.lake/packages/lean-compiler-backend/.lake/build/bin/lcb"
case "$(uname -s)-$(uname -m)" in
  Linux-x86_64) target=x86_64-linux ;;
  Linux-aarch64) target=aarch64-linux ;;
  Darwin-arm64) target=aarch64-macos ;;
  *) echo "slice0: unsupported host $(uname -s)-$(uname -m)" >&2; exit 2 ;;
esac
emitter=(timeout -k 10 120 lake env lean --run scripts/checks/slice0_bir.lean)

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

emit_examples() {
  local names
  names=$("${emitter[@]}" list) || { echo "slice0: listing the examples failed" >&2; exit 1; }
  [ -n "$names" ] || { echo "slice0: no examples listed" >&2; exit 1; }
  printf '%s\n' "$names"
}
