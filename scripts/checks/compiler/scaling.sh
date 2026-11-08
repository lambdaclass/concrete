#!/usr/bin/env bash
# Scaling gate (x86-64 Linux only): one machine, one job. Generated BIR at k0, 10*k0 and 100*k0 is
# checked by the Lean evaluator, then every in-process backend stage and the native step are timed.
# Repetitions are interleaved across sizes and the minimum is used. The 10x->100x exponent must be
# at most SCALING_MAX_EXPONENT (default 1.35); 100x in-process stages must be at least
# SCALING_MIN_STAGE_NS (default 10 ms). k0=1 gives 201 blocks and about 58 KiB of LLVM at 100x;
# smoke measurements put all real in-process stages above 10 ms there, so k0=2 is unnecessary and
# would make the currently slow adapter exceed the job's approximately ten-minute budget.
# scaling.tsv records every minimum and both exponents. Native macOS hosted runners are too noisy
# for this gate, which therefore refuses non-x86-64-Linux hosts.
#
# usage: scaling.sh [REPORT_DIR] [--allow-planted-failure --plant linear|quadratic]
# SCALING_IN_PROCESS_ONLY=1 omits native timing for local smoke runs. Plant controls also omit it
# and analyze only the planted stage, so an independent pipeline scaling defect cannot mask them.
set -euo pipefail
root=$(cd "$(dirname "$0")/../../.." && pwd)
cd "$root"
# shellcheck source=scripts/checks/lib/limits.sh
source scripts/checks/lib/limits.sh
gate_limits 64 40000
work=$(mktemp -d)
trap 'rc=$?; bounded_summary || true
  if [ "$rc" = 0 ] && grep -Eqs "^(incomplete|runner-failed)" "${BOUNDED_LOG:-/nonexistent}"; then
    echo "INCOMPLETE: a bounded run timed out twice; not a pass"; rc=3
  fi
  rm -rf "${work:?}"; exit "$rc"' EXIT
BOUNDED_PARENT="$work"
# shellcheck source=scripts/checks/lib/bounded.sh
source scripts/checks/lib/bounded.sh
[ "$BOUNDED_HAVE_WATCHDOG" = 1 ] || { echo "scaling: python3 is required for bounded runs" >&2; exit 2; }
bounded() {
  local secs=$1 rc=0
  shift
  bounded_run -i /dev/null "$secs" -- "$@" || rc=$?
  if [ "$rc" != 0 ] && bounded_is_incomplete "$rc"; then
    echo "scaling: INCOMPLETE (timed out twice): $*" >&2
  fi
  return "$rc"
}

report=$work/report
if [ "$#" -gt 0 ] && [ "${1#-}" = "$1" ]; then report=$1; shift; fi
plant=
if [ "$#" -gt 0 ]; then
  [ "$#" = 3 ] && [ "$1" = --allow-planted-failure ] && [ "$2" = --plant ] &&
    { [ "$3" = linear ] || [ "$3" = quadratic ]; } ||
    { echo "scaling: plants require --allow-planted-failure --plant linear|quadratic" >&2; exit 2; }
  plant=$3
fi
k0=${SCALING_K0:-1}
repetitions=${SCALING_REPETITIONS:-5}
max_exponent=${SCALING_MAX_EXPONENT:-1.35}
min_stage_ns=${SCALING_MIN_STAGE_NS:-10000000}
in_process_only=${SCALING_IN_PROCESS_ONLY:-0}
[ -n "$plant" ] && in_process_only=1
case "$(uname -s)-$(uname -m)" in
  Linux-x86_64) target=x86_64-linux ;;
  *) echo "scaling: runs only on x86-64 Linux (one machine, one job)" >&2; exit 2 ;;
esac
case "$in_process_only" in 0|1) ;; *) echo "scaling: SCALING_IN_PROCESS_ONLY must be 0 or 1" >&2; exit 2 ;; esac
mkdir -p "$report"
samples=$work/samples.tsv
: >"$samples"
driver=(lake env lean --run "$root/scripts/checks/compiler/scaling.lean")
if [ "$in_process_only" = 1 ]; then
  bounded 1800 lake build Concrete >"$work/lake-build.log" 2>&1 ||
    { tail -n 100 "$work/lake-build.log" >&2; exit 1; }
else
  bounded 1800 lake build Concrete lean-compiler-backend/lcb >"$work/lake-build.log" 2>&1 ||
    { tail -n 100 "$work/lake-build.log" >&2; exit 1; }
fi
plant_args=()
[ -z "$plant" ] || plant_args=(--allow-planted-failure --plant "$plant")
bounded 600 "${driver[@]}" measure "$k0" "$repetitions" "$target" ${plant_args[@]+"${plant_args[@]}"} \
  >"$work/in-process.tsv"
cat "$work/in-process.tsv" >>"$samples"

if [ "$in_process_only" = 0 ]; then
  lcb=$root/.lake/packages/lean-compiler-backend/.lake/build/bin/lcb
  for k in "$k0" "$((10 * k0))" "$((100 * k0))"; do
    bounded 600 "${driver[@]}" emit "$k" "$work/native/$k" "$target"
  done
  now_ns() { python3 -I -c 'import time; print(time.monotonic_ns())'; }
  for ((rep = 0; rep < repetitions; rep++)); do
    for k in "$k0" "$((10 * k0))" "$((100 * k0))"; do
      log=$work/native/$k/build-$rep.log
      start=$(now_ns)
      if bounded 300 bash "$root/scripts/checks/lib/slice0_native.sh" "$work/native/$k" "$lcb" >"$log" 2>&1; then
        stop=$(now_ns)
        printf 'sample\t%s\t%s\tnative\t%s\t0\n' "$rep" "$k" "$((stop - start))" >>"$samples"
      else
        cat "$log" >&2
        echo "FAIL scaling: native build failed at size $k repetition $rep" >&2
        exit 1
      fi
    done
  done
fi

report_args=()
[ -z "$plant" ] || report_args=(--only-plant "$plant")
bounded 60 python3 -I "$root/scripts/checks/lib/scaling_report.py" "$samples" "$report/scaling.tsv" \
  --k0 "$k0" --repetitions "$repetitions" --max-exponent "$max_exponent" \
  --min-stage-ns "$min_stage_ns" ${report_args[@]+"${report_args[@]}"}
