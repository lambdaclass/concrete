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
source scripts/checks/lib/slice0_common.sh

export CONCRETE_BIR_FUEL=${CONCRETE_BIR_FUEL:-32}
export CONCRETE_LCB_FUEL=${CONCRETE_LCB_FUEL:-512}
printf 'fuzz parity: seed=%s cases per mode=%s BIR fuel=%s LCB fuel=%s native seconds=2/4\n' \
  "$seed" "$cases" "$CONCRETE_BIR_FUEL" "$CONCRETE_LCB_FUEL"

# Keep the source path stable across replay and reduction. The written plus tokens
# determine the expected sites independently of emitter output.
path=tests/slice0/fuzz.con
leading_add=0
trial() {
  local mode=$1
  local a=$2
  local b=$3
  local dir=$4
  local plant=${5:-0}
  local sum=$((a + b))
  local status rc=0 backend
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
      expectation=(exit "$((sum + plant))")
      ;;
    unchecked)
      status=134
      expectation=(trap arithmeticOverflow site "$((leading_add + plant))")
      printf 'concrete: trap: overflow at %s:4:12 (site %s)\n' "$path" "$leading_add" >"$dir/expected/stderr"
      ;;
    refused)
      expectation=(refuse CORE-CHECK 'checker refusal: Core check rejected the program')
      if [ "$plant" = 1 ]; then expectation[1]=SYNTAX; fi
      ;;
  esac
  printf '%s\n' "${expectation[@]}" >"$dir/emitter-args"
  "${emitter[@]}" source "$path" "$dir/input.con" "$dir/emit" "$target" "${expectation[@]}" \
    >"$dir/emitter.stdout" 2>"$dir/emitter.stderr" || rc=$?
  if [ "$rc" -ne 0 ]; then
    if [ "$rc" = 3 ] || bounded_is_incomplete "$rc"; then
      echo evaluator-incomplete >"$dir/finding"
      return 3
    fi
    if [ "$rc" = 4 ]; then
      echo emitter-mismatch >"$dir/finding"
      return 1
    fi
    echo emitter-failure >"$dir/finding"
    return 2
  fi
  if [ "$mode" = refused ]; then return 0; fi
  printf '%s\n' "$status" >"$dir/expected/status"
  rc=0
  bounded "$native_secs" bash "$root/scripts/checks/lib/slice0_native.sh" "$dir/emit" "$lcb" \
    >"$dir/build.log" 2>&1 || rc=$?
  if [ "$rc" -ne 0 ]; then
    if bounded_is_incomplete "$rc"; then echo build-incomplete >"$dir/finding"; return 3; fi
    echo build-failure >"$dir/finding"
    return 2
  fi
  for backend in llvm lcb; do
    observe "$dir/emit/$backend.exe" "$dir/$backend" 2 4
    if [ "$(cat "$dir/$backend.status")" = incomplete ]; then
      echo "$backend-incomplete" >"$dir/finding"
      return 3
    fi
    if ! judge "$dir/expected" "$dir/$backend"; then
      echo "$backend-observation-mismatch" >"$dir/finding"
      return 1
    fi
  done
  return 0
}

# Boundary-preserving operand reductions. Accept only the same comparison-mode
# failure at the same observation boundary. The attempt budget is six replays.
reduce() {
  local a=$1
  local b=$2
  local mode=$3
  local original=$4
  local min=0 x y rc candidate
  if [ "$mode" != admitted ]; then min=256; fi
  mkdir -p "$original/reductions"
  for candidate in "0 $b" "$a 0" "1 $((min > 0 ? 255 : 0))" "255 1" \
    "$((min > b ? min - b : 0)) $b" "$a $((min > a ? min - a : 0))"; do
    read -r x y <<<"$candidate"
    if [ "$x" -gt "$a" ] || [ "$y" -gt "$b" ] || [ "$((x+y))" -lt "$min" ] ||
        [ "$((x+y))" -ge "$((a+b))" ]; then continue; fi
    local replay="$original/reductions/$x-$y"
    rc=0
    trial "$mode" "$x" "$y" "$replay" || rc=$?
    if [ "$rc" = 1 ] && cmp -s "$original/finding" "$replay/finding"; then
      a=$x
      b=$y
      printf '%s\n' "reductions/$x-$y" >"$original/reduced"
    fi
  done
}

for mode in admitted unchecked refused; do
  case "$mode" in
    admitted) a=41; b=1; expected='Core evaluator did not exit with 43' ;;
    unchecked) a=255; b=1; expected='expectation mismatch: BIR run gives' ;;
    refused) a=255; b=1; expected='got CORE-CHECK: checker refusal: Core check rejected the program' ;;
  esac
  control="$work/plant-$mode"
  rc=0
  trial "$mode" "$a" "$b" "$control" 1 || rc=$?
  if [ "$rc" != 1 ] || ! has_match -F "$expected" <"$control/emitter.stderr"; then
    cat "$control/emitter.stderr" >&2
    echo "fuzz parity: $mode control failed (status $rc)" >&2
    exit 1
  fi
  echo "ok   $mode planted mismatch"
done

for evaluator in BIR LCB; do
  rc=0
  envname="CONCRETE_${evaluator}_FUEL"
  bounded "$emit_secs" env "$envname=0" lake env lean --run scripts/checks/compiler/slice0_bir.lean \
    emit answer "$work/exhaust-$evaluator" "$target" exit 42 \
    >"$work/exhaust-$evaluator.log" 2>&1 || rc=$?
  if [ "$rc" != 3 ] || ! has_match -F "$evaluator exhausted evaluation fuel (0)" <"$work/exhaust-$evaluator.log"; then
    cat "$work/exhaust-$evaluator.log" >&2
    echo "fuzz parity: $evaluator exhaustion control failed (status $rc)" >&2
    exit 1
  fi
  echo "ok   terminating answer exhausts $evaluator fuel at zero"
done
"${emitter[@]}" loop "$work/loop" "$target"

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
build slice1-spec "$work/slice1-spec.log" bounded "$emit_secs" \
  lake env lean --run scripts/checks/compiler/fuzz_slice1_spec.lean "$seed" "$cases"
cat "$work/slice1-spec.log"
echo "INCOMPLETE: slice-1 native parity awaits the production slice-1 compiler and typed observations"
if [ "$require_ready" = 1 ]; then exit 3; fi
