#!/usr/bin/env bash
# Deterministic bounded corpus for the current production slice. Run from the root in nix develop.
set -euo pipefail
source scripts/checks/lib/gate_env.sh
require_ready=0
if [ "${1:-}" = --require-ready ]; then require_ready=1; shift; fi
seed=${1:-1}
cases=${2:-8}
if [ "$#" -gt 2 ] || ! [[ "$seed" =~ ^(0|[1-9][0-9]{0,4})$ && "$cases" =~ ^[1-9][0-9]{0,2}$ ]] ||
    [ "$seed" -gt 65535 ] || [ "$cases" -gt 128 ]; then
  echo 'usage: fuzz_parity.sh [--require-ready] [seed 0..65535 [cases 1..128]]' >&2
  exit 2
fi
default_bir_fuel=32
default_lcb_fuel=512
default_spec_fuel=4
campaign_bir_fuel=${CONCRETE_BIR_FUEL-$default_bir_fuel}
campaign_lcb_fuel=${CONCRETE_LCB_FUEL-$default_lcb_fuel}
campaign_spec_fuel=${CONCRETE_SLICE1_FUEL-$default_spec_fuel}
for budget in "$campaign_bir_fuel" "$campaign_lcb_fuel" "$campaign_spec_fuel"; do
  if ! [[ "$budget" =~ ^[0-9]+$ ]]; then
    echo 'fuzz parity: evaluator fuel budgets must be natural numbers' >&2
    exit 2
  fi
done

source scripts/checks/lib/slice0_common.sh

export CONCRETE_BIR_FUEL=$default_bir_fuel
export CONCRETE_LCB_FUEL=$default_lcb_fuel

# Normalize bounded execution and evaluator exhaustion at the campaign boundary.
evaluation_status() {
  if [ "$1" = 3 ] || bounded_is_incomplete "$1"; then return 3; fi
  return "$1"
}

build_native() {
  local dir=$1
  local rc=0
  bounded "$native_secs" bash "$root/scripts/checks/lib/slice0_native.sh" "$dir/emit" "$lcb" \
    >"$dir/build.log" 2>&1 || rc=$?
  if [ "$rc" = 0 ]; then return 0; fi
  if bounded_is_incomplete "$rc"; then echo build-incomplete >"$dir/finding"; return 3; fi
  echo build-failure >"$dir/finding"
  return 2
}

compare_native() {
  local dir=$1
  local backend=$2
  local expected=$3
  observe "$dir/emit/$backend.exe" "$dir/$backend" 2 4 || return 2
  if [ "$(cat "$dir/$backend.status")" = incomplete ]; then
    echo "$backend-incomplete" >"$dir/finding"
    return 3
  fi
  if ! judge "$expected" "$dir/$backend"; then
    echo "$backend-observation-mismatch" >"$dir/finding"
    return 1
  fi
}

# Keep the source path stable across replay and reduction. The written plus tokens
# determine the expected sites independently of emitter output.
path=tests/slice0/fuzz.con
leading_add=0
trial() {
  local mode=$1
  local a=$2
  local b=$3
  local dir=$4
  local plant=${5:-}
  local sum=$((a + b))
  local status rc=0 backend expected comparison
  local left=$a
  local -a expectation
  mkdir -p "$dir/expected" "$dir/emit"
  if [ "$leading_add" = 1 ]; then left="0 + $a"; fi
  printf 'fn main() -> u8 {\n  let x: u8 = %s;\n  let y: u8 = %s;\n  return x + y;\n}\n' \
    "$left" "$b" >"$dir/input.con"
  : >"$dir/expected/stderr"
  case "$mode" in
    admitted)
      status=$sum
      expectation=(exit "$sum")
      ;;
    unchecked)
      status=134
      expectation=(trap arithmeticOverflow site "$leading_add")
      printf 'concrete: trap: overflow at %s:4:12 (site %s)\n' "$path" "$leading_add" >"$dir/expected/stderr"
      ;;
    refused)
      status=1
      expectation=(refuse "$status" CORE-CHECK 'checker refusal: Core check rejected the program')
      if [ "$plant" = refusal ]; then expectation[2]=SYNTAX; fi
      ;;
  esac
  printf '%s\n' "$status" >"$dir/expected/status"
  printf '%s\n' "${expectation[@]}" >"$dir/emitter-args"
  "${emitter[@]}" source "$path" "$dir/input.con" "$dir/emit" "$target" "${expectation[@]}" \
    >"$dir/emitter.stdout" 2>"$dir/emitter.stderr" || evaluation_status "$?" || rc=$?
  case "$rc" in
    0) ;;
    3) echo evaluator-incomplete >"$dir/finding"; return 3 ;;
    4)
      comparison=$(sed -n 's/^comparison: \([a-z-][a-z-]*\)$/\1/p' "$dir/emitter.stderr")
      if [[ "$comparison" =~ ^[a-z-]+$ ]]; then
        echo "emitter-$comparison-mismatch" >"$dir/finding"
        return 1
      fi
      echo emitter-invalid-mismatch >"$dir/finding"
      return 2
      ;;
    *) echo emitter-failure >"$dir/finding"; return 2 ;;
  esac
  if [ "$mode" = refused ]; then return 0; fi
  build_native "$dir" || return "$?"
  for backend in llvm lcb; do
    expected="$dir/expected"
    if [ "$plant" = "$backend" ]; then
      expected="$dir/planted"
      cp -R "$dir/expected" "$expected"
      case "$mode" in
        admitted) printf '%s\n' "$((status + 1))" >"$expected/status" ;;
        unchecked) echo 'planted wrong trap bytes' >"$expected/stderr" ;;
      esac
    fi
    compare_native "$dir" "$backend" "$expected" || return "$?"
  done
}

# Recompute candidates after each accepted reduction. A replay directory also
# records which pairs have already been tried, across all iterations.
reduce() {
  local a=$1
  local b=$2
  local mode=$3
  local original=$4
  local replay_trial=${5:-trial}
  local min=0 x y rc candidate replay changed
  local attempts=0
  local -a candidates
  if [ "$mode" != admitted ]; then min=256; fi
  mkdir -p "$original/reductions"
  while [ "$attempts" -lt 6 ]; do
    changed=0
    candidates=()
    if [ "$mode" = admitted ]; then candidates+=("0 0"); fi
    candidates+=(
      "$((min > b ? min - b : 0)) $b" "$a $((min > a ? min - a : 0))"
      "$((a-1)) $b" "$a $((b-1))"
    )
    for candidate in "${candidates[@]}"; do
      read -r x y <<<"$candidate"
      if (( x < 0 || y < 0 || x > a || y > b || x + y < min || x + y >= a + b )); then continue; fi
      replay="$original/reductions/$x-$y"
      if [ -d "$replay" ]; then continue; fi
      mkdir "$replay"
      attempts=$((attempts + 1))
      rc=0
      "$replay_trial" "$mode" "$x" "$y" "$replay" || rc=$?
      if [ "$rc" = 1 ] && cmp -s "$original/finding" "$replay/finding"; then
        a=$x
        b=$y
        printf '%s\n' "reductions/$x-$y" >"$original/reduced"
        changed=1
        break
      fi
      if [ "$attempts" = 6 ]; then break; fi
    done
    if [ "$changed" = 0 ]; then break; fi
  done
}

assert_control() {
  if [ "$1" = "$2" ] && [ "$(cat "$3/finding")" = "$4" ]; then
    echo "ok   $4 (status $1)"
    return
  fi
  if [ -s "$3/emitter.stderr" ]; then cat "$3/emitter.stderr" >&2; fi
  if [ -s "$3/build.log" ]; then cat "$3/build.log" >&2; fi
  echo "fuzz parity: expected $4 ($2), got $(cat "$3/finding" 2>/dev/null || echo no-finding) ($1)" >&2
  exit 1
}

for mode in admitted unchecked; do
  for backend in llvm lcb; do
    a=41
    if [ "$mode" = unchecked ]; then a=255; fi
    control="$work/plant-$mode-$backend"
    rc=0
    trial "$mode" "$a" 1 "$control" "$backend" || rc=$?
    assert_control "$rc" 1 "$control" "$backend-observation-mismatch"
  done
done
control="$work/plant-refusal"
rc=0
trial refused 255 1 "$control" refusal || rc=$?
assert_control "$rc" 1 "$control" emitter-refusal-mismatch

for evaluator in BIR LCB; do
  rc=0
  envname="CONCRETE_${evaluator}_FUEL"
  control="$work/exhaust-$evaluator"
  (
    export "$envname=0"
    trial admitted 41 1 "$control"
  ) || rc=$?
  assert_control "$rc" 3 "$control" evaluator-incomplete
  if ! has_match -F "$evaluator exhausted evaluation fuel (0)" <"$control/emitter.stderr"; then
    cat "$control/emitter.stderr" >&2
    exit 1
  fi
done

for result in returned stuck; do
  rc=0
  log="$work/loop-$result.log"
  "${emitter[@]}" loop-evaluator-control "$result" >"$log" 2>&1 || rc=$?
  if [ "$rc" != 4 ] || ! has_match -F "loop: LCB evaluator $result" <"$log"; then
    cat "$log" >&2
    exit 1
  fi
  echo "ok   loop evaluator check rejects $result"
done

loop="$work/loop"
"${emitter[@]}" loop "$loop/emit" "$target"
build_native "$loop" || { rc=$?; cat "$loop/build.log" >&2; exit "$rc"; }
for backend in llvm lcb; do
  rc=0
  expected_timeout compare_native "$loop" "$backend" "$loop/emit" || rc=$?
  assert_control "$rc" 3 "$loop" "$backend-incomplete"
  if ! judge "$loop/emit" "$loop/$backend"; then
    echo "fuzz parity: $backend loop did not match the incomplete expectation" >&2
    exit 1
  fi
done

# The coordinate-only reducer formerly stopped at (0, 50). A second control
# makes (0, 0) fail at a different comparison and requires it to be discarded.
reduction_trial() {
  local comparison=emitter-lcb-evaluator-mismatch
  if [ "$2-$3" = 0-0 ]; then comparison=$zero_comparison; fi
  echo "$comparison" >"$4/finding"
  return 1
}
for zero_comparison in emitter-lcb-evaluator-mismatch emitter-core-mismatch; do
  control="$work/reduce-$zero_comparison"
  mkdir "$control"
  echo emitter-lcb-evaluator-mismatch >"$control/finding"
  reduce 100 50 admitted "$control" reduction_trial
  want=reductions/0-0
  if [ "$zero_comparison" = emitter-core-mismatch ]; then want=reductions/0-46; fi
  if [ ! -f "$control/reduced" ] || [ "$(cat "$control/reduced")" != "$want" ]; then
    echo "fuzz parity: reduction with $zero_comparison at 0-0 did not reach $want" >&2
    exit 1
  fi
done
echo 'ok   reduction reaches the boundary and preserves the failed comparison'

run_evaluation() {
  local limit=$1
  local log=$2
  shift 2
  bounded "$limit" "$@" >"$log" 2>&1 || evaluation_status "$?"
}

run_slice1() {
  local log=$1
  shift
  run_evaluation "$emit_secs" "$log" lake env lean --run scripts/checks/compiler/fuzz_slice1_spec.lean "$@"
}

# A double timeout shares the slice-1 campaign's bounded status conversion.
rc=0
BOUNDED_RETRY_FACTOR=2 expected_timeout run_evaluation 0.2 "$work/slice1-timeout.log" sleep 10 || rc=$?
[ "$rc" = 3 ] || { cat "$work/slice1-timeout.log" >&2; exit 1; }
echo 'ok   bounded spec evaluation timeout is incomplete'

# This import is outside the Concrete build performed by slice0_common.
rc=0
run_evaluation 1800 "$work/slice1-build.log" lake build ConcreteSpec.Proofs.Surface1 || rc=$?
if [ "$rc" != 0 ]; then cat "$work/slice1-build.log" >&2; exit "$rc"; fi
run_slice1 "$work/slice1-controls.log" --controls "$default_spec_fuel" ||
  { rc=$?; cat "$work/slice1-controls.log" >&2; exit "$rc"; }
cat "$work/slice1-controls.log"
rc=0
run_slice1 "$work/slice1-exhaust.log" 1 1 0 || rc=$?
[ "$rc" = 3 ] && has_match -F 'slice-1 spec exhausted evaluation fuel (0)' <"$work/slice1-exhaust.log" ||
  { cat "$work/slice1-exhaust.log" >&2; exit 1; }
echo 'ok   slice-1 spec exhaustion is incomplete'

export CONCRETE_BIR_FUEL=$campaign_bir_fuel
export CONCRETE_LCB_FUEL=$campaign_lcb_fuel
printf 'fuzz parity: seed=%s cases per mode=%s BIR fuel=%s LCB fuel=%s slice-1 fuel=%s native seconds=2/4\n' \
  "$seed" "$cases" "$CONCRETE_BIR_FUEL" "$CONCRETE_LCB_FUEL" "$campaign_spec_fuel"

state=$seed
for ((i=0; i<cases; i++)); do
  state=$(((25173 * state + 13849) % 65536))
  a=$((state / 256))
  b=$((state % 256))
  for mode in admitted unchecked refused; do
    leading_add=$((i % 2))
    if [ "$mode" = admitted ]; then
      lhs=$a
      rhs=$((b % (256 - lhs)))
      # Always exercise the largest u8 and an ordinary return with trap status.
      case "$i" in 0) lhs=254; rhs=1 ;; 1) lhs=133; rhs=1 ;; esac
    else
      lhs=$((1 + a % 255))
      rhs=$((256 - lhs + b % lhs))
      if [ "$i" = 0 ]; then lhs=255; rhs=1; fi
    fi
    dir="$work/$mode-$i"
    rc=0
    trial "$mode" "$lhs" "$rhs" "$dir" || rc=$?
    if [ "$rc" -ne 0 ]; then
      if [ "$rc" = 1 ]; then reduce "$lhs" "$rhs" "$mode" "$dir"; fi
      artifact=$(mktemp -d "${TMPDIR:-/tmp}/concrete-fuzz.XXXXXXXX")
      cp -R "$dir/." "$artifact/"
      printf '%s\n' "$seed $cases $mode $i $target $CONCRETE_BIR_FUEL $CONCRETE_LCB_FUEL" >"$artifact/replay"
      echo "fuzz parity: $(cat "$dir/finding") in $mode case $i (status $rc), replay data at $artifact" >&2
      exit "$rc"
    fi
  done
done
echo "fuzz parity: $cases slice-0 cases per mode passed"
run_slice1 "$work/slice1-spec.log" "$seed" "$cases" "$campaign_spec_fuel" ||
  { rc=$?; cat "$work/slice1-spec.log" >&2; exit "$rc"; }
cat "$work/slice1-spec.log"
pending='slice-1 native parity and typed counterexamples await their production interfaces'
if [ "$require_ready" = 1 ]; then echo "INCOMPLETE: $pending"; exit 3; fi
echo "fuzz parity scope: $pending"
