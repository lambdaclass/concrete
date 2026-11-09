#!/usr/bin/env bash
# Scaling gate (x86-64 Linux only): one machine, one job. Generated BIR at k0, 10*k0 and 100*k0 is
# checked by the Lean evaluator, then every in-process backend stage and the native step are timed.
# Nine repetitions alternate size order; in-process samples batch >=20 ms. Minima and a
# conservative low-quantile noise envelope must both satisfy the unchanged exponent bound. The 10x->100x exponent must be
# at most SCALING_MAX_EXPONENT (default 1.35); 100x in-process stages must be at least
# SCALING_MIN_STAGE_NS (default 10 ms), and native at least SCALING_MIN_NATIVE_NS (default 100 ms).
# After ISS-0030 made LCB.emit and LLVM.module linear, the k0=1 sizing run (C 1f5457c9 + backend
# ee2d8252 on concrete-ci-2) measured lcb_emit = 3.7 ms and llvm_module = 4.2 ms at 100x (below the
# 10 ms floor). Requiring at least 2x the 10 ms floor (>= 20 ms) for the fastest stage gives
# k0 = ceil(20 ms / 3.7 ms) = 6 (1201 blocks and about 350 KiB of LLVM text at 100x, k=600),
# clearing the floor with margin. scaling.tsv records every minimum, both exponents and the upper
# low-quantile noise envelope.
# Hosted macOS runners are shared VMs; exponents there are not comparable, so x86-64-only is a decision.
# lean --run measures the interpreted compiler, the only compiler available until concrete exists.
#
# usage: scaling.sh [REPORT_DIR] [--allow-planted-failure --plant linear|quadratic]
# SELECTED_TIER=pr builds Concrete and runs deterministic family/depth coverage plus fast
# analyzer/refusal/provenance controls (no wall-clock qualification). SCALING_IN_PROCESS_ONLY=1 omits
# native timing for local smoke runs. Every normal qualification/daily run automatically checks
# linear PASS, quadratic FAIL, noise-envelope FAIL, plant-without-authorization refusal and
# ci2-provenance controls without rebuilding; they measure/analyze only their planted stage, so
# independent pipeline scaling defects cannot mask them. A control's validation, timeout, resolution,
# inventory or unexpected verdict is a "control setup" failure.
set -euo pipefail
root=$(cd "$(dirname "$0")/../../.." && pwd)
cd "$root"
# shellcheck source=scripts/checks/lib/limits.sh
source scripts/checks/lib/limits.sh
gate_limits 64 40000
# Shared C lifecycle; the scheduler owns orchestration and outcome interpretation.
source scripts/checks/compiler/campaign.sh

report=$work/report
if [ "$#" -gt 0 ] && [ "${1#-}" = "$1" ]; then report=$1; shift; fi
plant=
if [ "$#" -gt 0 ]; then
  [ "$#" = 3 ] && [ "$1" = --allow-planted-failure ] && [ "$2" = --plant ] &&
    { [ "$3" = linear ] || [ "$3" = quadratic ]; } ||
    { echo "scaling: plants require --allow-planted-failure --plant linear|quadratic" >&2; exit 2; }
  plant=$3
fi
k0=${SCALING_K0:-6}
repetitions=${SCALING_REPETITIONS:-9}
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
tier=${SELECTED_TIER:-qualification}
case "$tier" in pr|qualification|daily) ;; *) echo "scaling: invalid SELECTED_TIER" >&2; exit 2 ;; esac
[ "$tier" != pr ] || in_process_only=1
head_sha=$(git rev-parse HEAD)
expected_sha=${SELECTED_SHA:-$head_sha}
mkdir -p "$report"
printf 'sha=%s\ntier=%s\n' "$head_sha" "$tier" >"$report/identity.txt"

check_exact_sha() { # check_exact_sha ACTUAL_HEAD EXPECTED_SHA [EVENT_NAME] [REF_NAME] [RUNNER_ENV]
  local actual_head=$1 expected=$2 event=${3:-} ref=${4:-} runner_env=${5:-}
  if [ "$runner_env" = "self-hosted" ] && [ "$event" = "pull_request" ]; then
    echo "FAIL scaling: unauthorized event pull_request for self-hosted qualification" >&2
    return 1
  fi
  if [ -n "$ref" ]; then
    case "$ref" in
      refs/heads/main|refs/heads/ci/*|refs/heads/wip/*) ;;
      *)
        echo "FAIL scaling: unauthorized ref $ref for self-hosted qualification" >&2
        return 1
        ;;
    esac
  fi
  if ! [[ "$expected" =~ ^[0-9a-f]{40}$ ]]; then
    echo "FAIL scaling: unauthorized or invalid expected SHA: ${expected:-<empty>}" >&2
    return 1
  fi
  if [ "$actual_head" != "$expected" ]; then
    echo "FAIL scaling: checked-out HEAD does not match authorized expected SHA $expected" >&2
    return 1
  fi
}

verify_report_sha() { # verify_report_sha FILE EXPECTED_SHA EXPECTED_TIER
  local rc=0
  python3 -I - "$1" "$2" "$3" <<'PY' || rc=$?
import re
import sys
from pathlib import Path

path, expected_sha, expected_tier = sys.argv[1:]
if not re.fullmatch(r"[0-9a-f]{40}", expected_sha):
    sys.exit(f"FAIL scaling: unauthorized or invalid expected SHA: {expected_sha}")
try:
    text = Path(path).read_text(encoding="utf-8")
except OSError as error:
    sys.exit(f"FAIL scaling: cannot read report {path} ({error})")
sha_match = re.search(r"(?:^|[;\s])sha=([0-9a-fA-F]+)(?:$|[;\s])", text, re.MULTILINE)
tier_match = re.search(r"(?:^|[;\s])tier=([a-z_]+)(?:$|[;\s])", text, re.MULTILINE)
if not sha_match:
    sys.exit(f"FAIL scaling: report {path} is missing sha= binding")
got_sha = sha_match.group(1)
if got_sha != expected_sha:
    sys.exit(f"FAIL scaling: report SHA {got_sha} in {path} does not match authorized expected SHA {expected_sha}")
if not tier_match:
    sys.exit(f"FAIL scaling: report {path} is missing tier= binding")
got_tier = tier_match.group(1)
if got_tier != expected_tier:
    sys.exit(f"FAIL scaling: report tier {got_tier} in {path} does not match expected tier {expected_tier}")
PY
  if [ "$rc" != 0 ] && [ "${CAMPAIGN_EXPECT_FAILURE:-0}" != 1 ]; then
    campaign_outcome FAIL 'scaling report SHA/tier binding failed'
  fi
  return "$rc"
}

check_ci2_runner() {
  python3 -I "$root/scripts/checks/lib/ci2_binding.py"
}
check_exact_sha "$head_sha" "$expected_sha" "${GITHUB_EVENT_NAME:-}" "${GITHUB_REF:-}" "${RUNNER_ENVIRONMENT:-}"
campaign_identity "$report" "$expected_sha"
# Heavy CI and scheduled campaigns require authoritative runner verification. Local
# focused smoke modes remain explicit and cannot qualify wall-clock measurements.
if [ "$tier" != pr ] && [ "$in_process_only" = 0 ]; then
  binding_rc=0
  check_ci2_runner >"$work/binding.log" 2>&1 || binding_rc=$?
  if [ "$binding_rc" != 0 ]; then
    cat "$work/binding.log" >&2
    if grep -q '^INCOMPLETE ' "$work/binding.log"; then
      campaign_outcome INCOMPLETE 'runner inventory unavailable'
      exit 125
    fi
    exit "$binding_rc"
  fi
  source "$root/scripts/checks/lib/heavy_slots.sh"
  if ! heavy_slot_acquire; then
    campaign_outcome INCOMPLETE 'shared host heavy slot unavailable'
    exit 125
  fi
fi
verify_report_sha "$report/identity.txt" "$expected_sha" "$tier"
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
    --min-stage-ns "$min_stage_ns" --min-native-ns "$min_native_ns" --mode "$3" \
    --sha "$head_sha" --tier "$tier"
}

if [ -n "$plant" ]; then
  run_lean "$plant" "$work/plant.tsv"
  analyze "$work/plant.tsv" "$report/scaling.tsv" "$plant"
  verify_report_sha "$report/scaling.tsv" "$expected_sha" "$tier"
  exit
fi

control_setup() { # control_setup PLANT REASON LOG
  campaign_outcome FAIL "scaling control $1: control setup: $2"
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
  CAMPAIGN_EXPECT_FAILURE=1 analyze "$samples" "$work/control-quadratic-report.tsv" quadratic >"$log" 2>&1 || rc=$?
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

control_noise_envelope() {
  local samples=$work/control-envelope.tsv log=$work/control-envelope.log rc=0 rep large_ns
  : >"$samples"
  for ((rep = 0; rep < 9; rep++)); do
    large_ns=300000000
    [ "$rep" != 0 ] || large_ns=100000000
    printf 'sample\t%s\t6\tplant_linear\t1000000\t1\n' "$rep" >>"$samples"
    printf 'sample\t%s\t60\tplant_linear\t10000000\t1\n' "$rep" >>"$samples"
    printf 'sample\t%s\t600\tplant_linear\t%s\t1\n' "$rep" "$large_ns" >>"$samples"
  done
  CAMPAIGN_EXPECT_FAILURE=1 bounded 60 python3 -I "$root/scripts/checks/lib/scaling_report.py" "$samples" "$work/control-envelope-report.tsv" \
    --k0 6 --repetitions 9 --max-exponent 1.35 --min-stage-ns 10000000 --min-native-ns 100000000 \
    --mode linear >"$log" 2>&1 || rc=$?
  if [ "$rc" != 1 ] || [ "$(grep -c '^FAIL scaling:' "$log")" != 1 ] || ! grep -Eq \
      '^FAIL scaling: stage plant_linear noise envelope exponent = 1\.477 exceeds 1\.350; measurement is not noise-robust$' "$log"; then
    control_setup noise-envelope "did not reject noisy 100x distribution with exact envelope diagnostic (status $rc)" "$log"
    return 1
  fi
  cat "$log"
  echo 'ok scaling control noise-envelope: refused for planted reason (envelope 1.477 > 1.350)'
}

control_refusal() {
  local log=$work/control-refusal.log rc=0
  CAMPAIGN_EXPECT_FAILURE=1 bounded 60 "${driver[@]}" measure "$k0" "$repetitions" "$target" --plant quadratic >"$log" 2>&1 || rc=$?
  if [ "$rc" != 2 ] || ! grep -qxF 'plants require --allow-planted-failure --plant linear|quadratic' "$log"; then
    control_setup refusal "driver did not refuse --plant without --allow-planted-failure (status $rc)" "$log"
    return 1
  fi
  rc=0
  CAMPAIGN_EXPECT_FAILURE=1 bounded 60 env -u SCHEDULED_OUTCOME_FILE bash "$root/scripts/checks/compiler/scaling.sh" "$work/refusal-report" --plant quadratic >"$log" 2>&1 || rc=$?
  if [ "$rc" != 2 ] || ! grep -qxF 'scaling: plants require --allow-planted-failure --plant linear|quadratic' "$log"; then
    control_setup refusal "gate did not refuse --plant without --allow-planted-failure (status $rc)" "$log"
    return 1
  fi
  echo 'ok scaling control refusal: explicit flag required'
}

control_ci2_provenance() {
  # Exercise the shared verifier's public interface with a synthetic inventory;
  # qualification above alone consults the authoritative ISS-0029 inventory.
  bounded 30 python3 -I "$root/scripts/checks/compiler/scaling_binding_controls.py" "$root/scripts/checks/lib"
}

control_exact_sha() {
  local log=$work/control-sha.log bad_report=$work/control-bad-report.tsv rc=0
  if ! check_exact_sha "$head_sha" "$head_sha" "workflow_dispatch" "refs/heads/main" "self-hosted" >"$log" 2>&1 ||
      ! verify_report_sha "$report/identity.txt" "$head_sha" "$tier" >"$log" 2>&1; then
    control_setup exact-sha "authorized exact SHA was rejected" "$log"
    return 1
  fi
  for case_spec in \
      "unauthorized-event|$head_sha|pull_request|refs/heads/main|self-hosted|FAIL scaling: unauthorized event pull_request for self-hosted qualification" \
      "unauthorized-ref|$head_sha|workflow_dispatch|refs/pull/1/merge|self-hosted|FAIL scaling: unauthorized ref refs/pull/1/merge for self-hosted qualification" \
      "invalid-sha|not-a-sha|workflow_dispatch|refs/heads/main|self-hosted|FAIL scaling: unauthorized or invalid expected SHA: not-a-sha" \
      "mismatched-sha|0000000000000000000000000000000000000000|workflow_dispatch|refs/heads/main|self-hosted|FAIL scaling: checked-out HEAD does not match authorized expected SHA 0000000000000000000000000000000000000000"; do
    IFS='|' read -r label exp_sha ev ref renv expected <<<"$case_spec"
    rc=0
    check_exact_sha "$head_sha" "$exp_sha" "$ev" "$ref" "$renv" >"$log" 2>&1 || rc=$?
    if [ "$rc" = 0 ] || ! grep -qF "$expected" "$log"; then
      control_setup "exact-sha-$label" "did not refuse with '$expected' (status $rc)" "$log"
      return 1
    fi
  done
  printf '# sha=1111111111111111111111111111111111111111; tier=%s\n' "$tier" >"$bad_report"
  rc=0
  CAMPAIGN_EXPECT_FAILURE=1 verify_report_sha "$bad_report" "$head_sha" "$tier" >"$log" 2>&1 || rc=$?
  if [ "$rc" = 0 ] || ! grep -qF "FAIL scaling: report SHA 1111111111111111111111111111111111111111 in $bad_report does not match authorized expected SHA $head_sha" "$log"; then
    control_setup exact-sha-report-mismatch "did not refuse mismatched report SHA (status $rc)" "$log"
    return 1
  fi
  echo 'ok scaling control exact-sha: authorized exact SHA accepted and unauthorized/mismatched SHAs refused'
}

if [ "$tier" = pr ]; then
  printf '# sha=%s; tier=%s\n' "$head_sha" "$tier" >"$report/pr-coverage.txt"
  bounded 300 "${driver[@]}" check-small >>"$report/pr-coverage.txt"
  verify_report_sha "$report/pr-coverage.txt" "$expected_sha" "$tier"
  cat "$report/pr-coverage.txt"
  controls_rc=0
  control_noise_envelope || controls_rc=1
  control_refusal || controls_rc=1
  control_ci2_provenance || controls_rc=1
  control_exact_sha || controls_rc=1
  if [ "$controls_rc" != 0 ]; then
    echo 'FAIL scaling: automatic planted controls failed' >&2
    exit 1
  fi
  exit 0
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
      sizes=("$k0" "$((10 * k0))" "$((100 * k0))")
      if ((rep % 2)); then sizes=("$((100 * k0))" "$((10 * k0))" "$k0"); fi
      for k in "${sizes[@]}"; do
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
  cp "$work/clean.tsv" "$report/samples.tsv"
  analyze "$work/clean.tsv" "$report/scaling.tsv" "$mode" || clean_rc=$?
  if [ "$clean_rc" = 0 ]; then
    verify_report_sha "$report/scaling.tsv" "$expected_sha" "$tier" || clean_rc=$?
  fi
else
  echo "FAIL scaling: clean measurement failed with status $clean_rc; running controls" >&2
fi

# One policy/analyzer for all backend families. Large families and depth probes
# belong to the scheduled tier; no second backend-specific scaling workflow.
if [ "$tier" = daily ]; then
  family_k0=${SCALING_FAMILY_K0:-100}
  for family in wide_phi sparse_registers dense_registers many_blocks; do
    rc=0
    bounded 600 "${driver[@]}" measure-family "$family" "$family_k0" "$repetitions" "$target" \
      >"$report/$family.samples.tsv" || rc=$?
    if [ "$rc" = 0 ]; then
      bounded 60 python3 -I "$root/scripts/checks/lib/scaling_report.py" \
        "$report/$family.samples.tsv" "$report/$family.tsv" --k0 "$family_k0" \
        --repetitions "$repetitions" --max-exponent "$max_exponent" \
        --min-stage-ns "$min_stage_ns" --min-native-ns "$min_native_ns" --mode backend \
        --sha "$head_sha" --tier "$tier" || rc=$?
    fi
    if [ "$rc" = 0 ]; then
      verify_report_sha "$report/$family.tsv" "$expected_sha" "$tier" || rc=$?
    fi
    [ "$rc" = 0 ] || clean_rc=$rc
  done
  printf '# sha=%s; tier=%s\n' "$head_sha" "$tier" >"$report/depth.txt"
  bounded 600 "${driver[@]}" check-depth >>"$report/depth.txt" || clean_rc=$?
  if [ "$clean_rc" = 0 ]; then
    verify_report_sha "$report/depth.txt" "$expected_sha" "$tier" || clean_rc=$?
  fi
fi

controls_rc=0
control_linear || controls_rc=1
control_quadratic || controls_rc=1
control_noise_envelope || controls_rc=1
control_refusal || controls_rc=1
control_ci2_provenance || controls_rc=1
control_exact_sha || controls_rc=1
if [ "$controls_rc" != 0 ]; then
  echo 'FAIL scaling: automatic planted controls failed' >&2
  [ "$clean_rc" != 0 ] || clean_rc=1
fi
exit "$clean_rc"
