#!/usr/bin/env bash
# Scaling gate (x86-64 Linux only): one machine, one job. Generated BIR at k0, 10*k0 and 100*k0 is
# checked by the Lean evaluator, then every in-process backend stage and the native step are timed.
# Repetitions are interleaved across sizes and the minimum is used. The 10x->100x exponent must be
# at most SCALING_MAX_EXPONENT (default 1.35); 100x in-process stages must be at least
# SCALING_MIN_STAGE_NS (default 10 ms), and native at least SCALING_MIN_NATIVE_NS (default 100 ms).
# k0=1 gives 201 blocks and about 58 KiB of LLVM at 100x; smoke measurements put all real in-process
# stages above 10 ms there, so k0=2 is unnecessary and would make the currently slow adapter exceed
# the approximately ten-minute budget. scaling.tsv records every minimum and both exponents.
# Hosted macOS runners are shared VMs; exponents there are not comparable, so x86-64-only is a decision.
# lean --run measures the interpreted compiler, the only compiler available until concrete exists.
#
# usage: scaling.sh [REPORT_DIR] [--allow-planted-failure --plant linear|quadratic]
# SCALING_IN_PROCESS_ONLY=1 omits native timing for local smoke runs. Every normal run automatically
# checks linear PASS and quadratic FAIL controls without rebuilding; they measure/analyze only their
# planted stage, so independent pipeline scaling defects (ISS-0030) cannot mask them. A control's
# validation, timeout, resolution, inventory or unexpected verdict is a "control setup" failure.
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
min_native_ns=${SCALING_MIN_NATIVE_NS:-100000000}
in_process_only=${SCALING_IN_PROCESS_ONLY:-0}
[ -n "$plant" ] && in_process_only=1
case "$(uname -s)-$(uname -m)" in
  Linux-x86_64) target=x86_64-linux ;;
  *) echo "scaling: runs only on x86-64 Linux (one machine, one job)" >&2; exit 2 ;;
esac
case "$in_process_only" in 0|1) ;; *) echo "scaling: SCALING_IN_PROCESS_ONLY must be 0 or 1" >&2; exit 2 ;; esac
mkdir -p "$report"
driver=(lake env lean --run "$root/scripts/checks/compiler/scaling.lean")
if [ "$in_process_only" = 1 ]; then
  bounded 1800 lake build Concrete >"$work/lake-build.log" 2>&1 ||
    { tail -n 100 "$work/lake-build.log" >&2; exit 1; }
else
  bounded 1800 lake build Concrete lean-compiler-backend/lcb >"$work/lake-build.log" 2>&1 ||
    { tail -n 100 "$work/lake-build.log" >&2; exit 1; }
fi

run_lean() { # run_lean PLANT OUTPUT
  local selected=$1 output=$2
  local -a plant_args=()
  [ -z "$selected" ] || plant_args=(--allow-planted-failure --plant "$selected")
  bounded 600 "${driver[@]}" measure "$k0" "$repetitions" "$target" \
    ${plant_args[@]+"${plant_args[@]}"} >"$output"
}

analyze() { # analyze SAMPLES REPORT MODE
  bounded 60 python3 -I "$root/scripts/checks/lib/scaling_report.py" "$1" "$2" \
    --k0 "$k0" --repetitions "$repetitions" --max-exponent "$max_exponent" \
    --min-stage-ns "$min_stage_ns" --min-native-ns "$min_native_ns" --mode "$3"
}

if [ -n "$plant" ]; then
  run_lean "$plant" "$work/plant.tsv"
  analyze "$work/plant.tsv" "$report/scaling.tsv" "$plant"
  exit
fi

mode=normal
[ "$in_process_only" = 0 ] || mode=in-process
clean_rc=0
run_lean "" "$work/clean.tsv" || clean_rc=$?

if [ "$clean_rc" = 0 ] && [ "$in_process_only" = 0 ]; then
  lcb=$root/.lake/packages/lean-compiler-backend/.lake/build/bin/lcb
  for k in "$k0" "$((10 * k0))" "$((100 * k0))"; do
    bounded 600 "${driver[@]}" emit "$k" "$work/native-inputs/$k" "$target" || clean_rc=$?
  done
  now_ns() { python3 -I -c 'import time; print(time.monotonic_ns())'; }
  if [ "$clean_rc" = 0 ]; then
    for ((rep = 0; rep < repetitions; rep++)); do
      for k in "$k0" "$((10 * k0))" "$((100 * k0))"; do
        run=$work/native-runs/$rep/$k
        mkdir -p "$(dirname "$run")"
        cp -R "$work/native-inputs/$k" "$run"
        log=$work/native-$k-$rep.log
        start=$(now_ns)
        if bounded 300 bash "$root/scripts/checks/lib/slice0_native.sh" "$run" "$lcb" >"$log" 2>&1; then
          stop=$(now_ns)
          printf 'sample\t%s\t%s\tnative\t%s\t0\n' "$rep" "$k" "$((stop - start))" >>"$work/clean.tsv"
        else
          cat "$log" >&2
          echo "FAIL scaling: native build failed at size $k repetition $rep" >&2
          clean_rc=1
          break 2
        fi
      done
    done
  fi
fi
if [ "$clean_rc" = 0 ]; then
  analyze "$work/clean.tsv" "$report/scaling.tsv" "$mode" || clean_rc=$?
else
  echo "FAIL scaling: clean measurement failed with status $clean_rc; running controls" >&2
fi

control_setup() { # control_setup PLANT REASON LOG
  echo "FAIL scaling control $1: control setup: $2" >&2
  [ ! -f "$3" ] || cat "$3" >&2
}

control_linear() {
  local samples=$work/control-linear.tsv log=$work/control-linear.log rc=0
  run_lean linear "$samples" >"$work/control-linear-driver.log" 2>&1 || rc=$?
  if [ "$rc" != 0 ]; then
    control_setup linear "driver exited $rc" "$work/control-linear-driver.log"
    return 1
  fi
  analyze "$samples" "$work/control-linear-report.tsv" linear >"$log" 2>&1 || rc=$?
  if [ "$rc" != 0 ]; then
    control_setup linear "linear plant was rejected (status $rc)" "$log"
    return 1
  fi
  cat "$log"
  echo 'ok scaling control linear: PASS'
}

control_quadratic() {
  local samples=$work/control-quadratic.tsv log=$work/control-quadratic.log rc=0 value
  run_lean quadratic "$samples" >"$work/control-quadratic-driver.log" 2>&1 || rc=$?
  if [ "$rc" != 0 ]; then
    control_setup quadratic "driver exited $rc" "$work/control-quadratic-driver.log"
    return 1
  fi
  analyze "$samples" "$work/control-quadratic-report.tsv" quadratic >"$log" 2>&1 || rc=$?
  if [ "$rc" = 0 ]; then
    control_setup quadratic "quadratic plant unexpectedly passed" "$log"
    return 1
  fi
  if [ "$(grep -c '^FAIL scaling:' "$log")" != 1 ] || ! grep -Eq \
      '^FAIL scaling: stage plant_quadratic exponent 10x->100x = [0-9]+\.[0-9]{3} exceeds [0-9]+\.[0-9]{3}$' "$log"; then
    control_setup quadratic "missing or additional analyzer failure" "$log"
    return 1
  fi
  value=$(sed -n 's/^FAIL scaling: stage plant_quadratic exponent 10x->100x = \([0-9.]*\) exceeds.*/\1/p' "$log")
  if ! bounded 30 python3 -I -c 'import sys; e=float(sys.argv[1]); t=float(sys.argv[2]); sys.exit(0 if 1.7 <= e <= 2.3 and e > t else 1)' \
      "$value" "$max_exponent"; then
    control_setup quadratic "planted exponent $value is outside [1.7, 2.3] or not above threshold" "$log"
    return 1
  fi
  cat "$log"
  echo "ok scaling control quadratic: refused for planted reason (exponent $value)"
}

controls_rc=0
control_linear || controls_rc=1
control_quadratic || controls_rc=1
if [ "$controls_rc" != 0 ]; then
  echo 'FAIL scaling: automatic planted controls failed' >&2
  [ "$clean_rc" != 0 ] || clean_rc=1
fi
exit "$clean_rc"
