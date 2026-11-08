#!/usr/bin/env bash
# Seeded spec/compiler differential (Concrete/README.md gates 5 and 6). Run from the repo root in
# nix develop. The shared compiler build has the usual SLICE0_BUILD_SECS bound. SPEC_DIFF_SECS
# (default 600) then supplies ONE absolute deadline for generation, controls and comparisons,
# including bounded retries. SPEC_DIFF_N (default 300) caps candidates per leg. The minimum judged
# counts default to 8 for each compiled leg (SPEC_DIFF_MIN_JUDGED, SPEC_DIFF_MIN_SPEC_JUDGED).
# Coverage and BIR exit/trap vacuity are checked over programs judged on ALL THREE legs. A deadline
# or an insufficient judged set by the deadline is INCOMPLETE (3), never a pass.
#
# Replay: SPEC_DIFF_REPLAY=bir:SEED:INDEX or spec:SEED:INDEX (bare SEED:INDEX means bir).
# A mismatch writes a persistent bundle under SPEC_DIFF_REGRESSIONS (default
# .lake/concrete/spec_diff/regressions/<seed>-<index>/), with observations, generated terms, the
# exact manifest row and Examples entry, and replay command. CI uploads that directory on failure.
# To save a regression, copy its Examples definition/list entry and manifest row into the sources,
# replay it, then run the manifest Tests job.
#
# Planted controls first prove the unmutated candidate, then require the precise planted effect.
# Every setup step is guarded because an expected nonzero control result must not disable errexit
# inside the control. A timeout, crash, incomplete observation, driver failure or setup failure is
# not an expected control pass. Mutations require the driver's explicit planted-control command.
set -euo pipefail

seed=${SPEC_DIFF_SEED:-$(git rev-parse HEAD)}
candidate_count=${SPEC_DIFF_N:-300}
budget=${SPEC_DIFF_SECS:-600}
minimum_judged=${SPEC_DIFF_MIN_JUDGED:-8}
minimum_spec_judged=${SPEC_DIFF_MIN_SPEC_JUDGED:-8}
replay=${SPEC_DIFF_REPLAY:-}
for value in "$candidate_count" "$budget" "$minimum_judged" "$minimum_spec_judged"; do
  [[ "$value" =~ ^[0-9]+$ ]] && [ "$value" -gt 0 ] ||
    { echo "spec_diff: N, SECS and minimum judged counts must be positive integers" >&2; exit 2; }
done
printf 'spec_diff seed: %s\n' "$seed"
printf 'replay: SPEC_DIFF_REPLAY=bir:%s:INDEX bash scripts/checks/compiler/spec_diff.sh\n' "$seed"

driver_imports=$(awk '/^import / {for (i = 2; i <= NF; i++) printf "%s ", $i}' scripts/checks/compiler/spec_diff.lean)
[ -n "$driver_imports" ] || { echo "spec_diff: no imports found in spec_diff.lean" >&2; exit 2; }
SLICE0_BUILD_TARGETS="Concrete $driver_imports"
# shellcheck source=scripts/checks/lib/slice0_common.sh
source scripts/checks/lib/slice0_common.sh

start=$(date +%s)
deadline=$((start + budget))
remaining() { echo $((deadline - $(date +%s))); }
driver=(env "SPEC_DIFF_TARGET=$target" lake env lean --run scripts/checks/compiler/spec_diff.lean)
regressions=${SPEC_DIFF_REGRESSIONS:-.lake/concrete/spec_diff/regressions}
case "$regressions" in /*) ;; *) regressions="$root/$regressions" ;; esac
safe_seed=$(printf '%s' "$seed" | tr -c 'A-Za-z0-9_' '_')
fail=0
deadline_stopped=0
bir_judged=0
spec_judged=0
: >"$work/judged-bir.tsv"
: >"$work/judged-spec.tsv"

# Run one command with first and retry budgets whose sum is the time remaining now.
bounded_remaining() {
  local total first retry rc=0
  total=$(remaining)
  if [ "$total" -lt 2 ]; then
    echo "INCOMPLETE: deadline before bounded command: $*" >&2
    return 3
  fi
  first=$((total / 2))
  retry=$((total - first))
  bounded_run -i /dev/null -r "$retry" "$first" -- "${capped[@]}" "$@" || rc=$?
  if [ "$rc" -ne 0 ] && bounded_is_incomplete "$rc"; then
    echo "INCOMPLETE: bounded command timed out twice: $*" >&2
    return 3
  fi
  return "$rc"
}

require_bounded() {
  local rc=0
  bounded_remaining "$@" || rc=$?
  [ "$rc" -eq 0 ] && return 0
  [ "$rc" -ne 3 ] || exit 3
  echo "FAIL command (rc $rc): $*" >&2
  exit "$rc"
}

observe_remaining() { # observe_remaining EXE OUT: status incomplete is always INCOMPLETE
  local total first retry rc=0
  total=$(remaining)
  if [ "$total" -lt 2 ]; then
    echo "INCOMPLETE: deadline before observing $1" >&2
    return 3
  fi
  first=$((total / 2)); [ "$first" -le 10 ] || first=10
  retry=$((total - first))
  observe "$1" "$2" "$first" "$retry" || rc=$?
  [ "$rc" -eq 0 ] || { echo "INCOMPLETE: observer failed for $1" >&2; return 3; }
  if [ "$(cat "$2.status")" = incomplete ]; then
    echo "INCOMPLETE: observation timed out twice for $1" >&2
    return 3
  fi
}

summary_value() { # summary_value FILE KEY [FIELD]
  awk -F '\t' -v key="$2" -v field="${3:-2}" '$1 == key {print $field; exit}' "$1"
}

print_counts() { # print_counts LABEL SUMMARY: generated (not judged) counts
  echo "$1 generated outcome/construct counts:"
  awk -F '\t' '$1 == "outcome" || $1 == "construct" {printf "  %s %s %s\n", $1, $2, $3}' "$2"
  printf '  accepted %s, rejected %s, attempted %s\n' \
    "$(summary_value "$2" accepted)" "$(summary_value "$2" rejected)" "$(summary_value "$2" attempted)"
}

check_generation() { # only yield is a generated-corpus guard; coverage belongs to judged sets
  local label summary accepted rejected attempted
  label=$1
  summary=$2
  accepted=$(summary_value "$summary" accepted)
  rejected=$(summary_value "$summary" rejected)
  attempted=$(summary_value "$summary" attempted)
  [ -n "$accepted" ] && [ -n "$rejected" ] && [ -n "$attempted" ] ||
    { echo "FAIL $label: missing generation counts" >&2; fail=1; return; }
  if [ "$accepted" -eq 0 ] || [ $((accepted * 2)) -lt "$attempted" ]; then
    echo "FAIL $label: yield $accepted/$attempted is below the 50% floor" >&2
    fail=1
  fi
}

record_judged() { # record_judged LEG OUTCOME DIR: called only after all three judges ran
  local file="$work/judged-$1.tsv"
  printf 'outcome\t%s\n' "$2" >>"$file"
  tr ',' '\n' <"$3/constructs.txt" | while IFS= read -r construct; do
    [ -z "$construct" ] || printf 'construct\t%s\n' "$construct" >>"$file"
  done
}

print_judged_counts() { # print_judged_counts LEG COUNT
  local file="$work/judged-$1.tsv"
  echo "$1 judged outcome/construct counts ($2 programs):"
  awk -F '\t' '{count[$1 FS $2]++} END {for (key in count) print "  " key " " count[key]}' "$file" | LC_ALL=C sort
}

check_judged() { # check_judged LEG COUNT MIN OUTCOME-GUARD REQUIRED-CONSTRUCTS...
  local leg count minimum outcome_guard construct missing=0 n
  leg=$1
  count=$2
  minimum=$3
  outcome_guard=$4
  shift 4
  if [ "$count" -lt "$minimum" ]; then
    echo "$leg judged minimum: $count < $minimum" >&2
    missing=1
  fi
  if [ "$outcome_guard" = both ]; then
    for outcome in exit trap; do
      n=$(awk -F '\t' -v o="$outcome" '$1 == "outcome" && $2 == o {n++} END {print n+0}' "$work/judged-$leg.tsv")
      if [ "$n" -eq 0 ]; then
        echo "$leg judged vacuity: zero ${outcome}s" >&2
        missing=1
      fi
    done
  fi
  for construct in "$@"; do
    n=$(awk -F '\t' -v c="$construct" '$1 == "construct" && $2 == c {n++} END {print n+0}' "$work/judged-$leg.tsv")
    if [ "$n" -eq 0 ]; then
      echo "$leg judged coverage: zero $construct" >&2
      missing=1
    fi
  done
  [ "$missing" -eq 0 ] || fail=1
}

report_mismatch() { # report_mismatch LEG INDEX DIR BACKEND: persist before scratch cleanup
  local leg index d backend name term bundle replay_text
  leg=$1
  index=$2
  d=$3
  backend=$4
  bundle="$regressions/$safe_seed-$index"
  mkdir -p "$bundle/expected" "$bundle/observed-$backend"
  printf '%s\n' "$seed" >"$bundle/seed"
  printf '%s\n' "$index" >"$bundle/index"
  printf '%s\n' "$leg" >"$bundle/leg"
  printf '%s\n' "$backend" >"$bundle/backend"
  cp "$d/status" "$d/stderr" "$bundle/expected/"
  cp "$d/$backend.status" "$bundle/observed-$backend/status"
  cp "$d/$backend.stderr" "$bundle/observed-$backend/stderr"
  cp "$d/$backend.stdout" "$bundle/observed-$backend/stdout"
  cp "$d/program.lean.txt" "$bundle/program.lean.txt"
  cp "$d/constructs.txt" "$bundle/constructs.txt"
  if [ "$leg" = spec ]; then term="$d/bir.txt"; else term="$d/program.lean.txt"; fi
  name=$(cat "$d/regression-name.txt")
  cp "$d/manifest-row.txt" "$bundle/manifest-row.tsv"
  {
    printf 'def %s : Concrete.BIR.Program :=\n' "$name"
    cat "$term"
    printf '\n-- Add to Examples.all:\n("%s", %s),\n' "$name" "$name"
  } >"$bundle/examples.lean"
  replay_text="SPEC_DIFF_REPLAY='$leg:$seed:$index' bash scripts/checks/compiler/spec_diff.sh"
  printf '%s\n' "$replay_text" >"$bundle/replay.sh"
  echo "FAIL $leg seed $seed index $index [$backend]: observation mismatch" >&2
  echo "  expected status $(cat "$d/status"), observed $(cat "$d/$backend.status")" >&2
  echo "  regression bundle: $bundle" >&2
  echo "  exact manifest row:" >&2
  cat "$d/manifest-row.txt" >&2
  echo "  Examples definition: $bundle/examples.lean" >&2
  echo "  replay: $replay_text" >&2
}

native_one() { # native_one LEG INDEX DIR: 0 match, 1 mismatch judged, 3 incomplete, 4 build/setup
  local leg index d name backend bad=0 rc=0
  leg=$1
  index=$2
  d=$3
  name=$1-$2
  ( build "$name" "$d/build.log" bounded_remaining bash "$root/scripts/checks/lib/slice0_native.sh" "$d" "$lcb" ) || rc=$?
  if [ "$rc" -ne 0 ]; then
    if grep -q 'INCOMPLETE:' "$d/build.log"; then return 3; fi
    echo "FAIL $name: native build/setup failed" >&2
    return 4
  fi
  for backend in llvm lcb; do
    observe_remaining "$d/$backend.exe" "$d/$backend" || return 3
    if judge "$d" "$d/$backend"; then
      echo "ok   $name [$backend]: status $(cat "$d/$backend.status")"
    else
      report_mismatch "$leg" "$index" "$d" "$backend"
      bad=1
    fi
  done
  if judge "$d" "$d/lcb_eval"; then
    echo "ok   $name [lcb-eval]: status $(cat "$d/lcb_eval.status")"
  else
    report_mismatch "$leg" "$index" "$d" lcb_eval
    bad=1
  fi
  return "$bad"
}

run_control() { # an expected FAIL line is a pass; setup failures and INCOMPLETE are not
  local label expected fn log rc
  label=$1
  expected=$2
  fn=$3
  log="$work/ctl-$label.log"
  rc=0
  ( "$fn" ) >"$log" 2>&1 || rc=$?
  if [ "$rc" -ne 0 ] && grep -qF "$expected" "$log"; then
    echo "ok   control $label: failed for the planted reason"
  elif grep -q 'INCOMPLETE:' "$log"; then
    cat "$log" >&2
    exit 3
  else
    echo "FAIL control $label: did not fail as '$expected' (rc $rc)" >&2
    cat "$log" >&2
    fail=1
  fi
}

control_bounded() {
  local rc=0
  bounded_remaining "$@" || rc=$?
  [ "$rc" -eq 0 ] && return 0
  [ "$rc" -ne 3 ] || { echo "INCOMPLETE: control command deadline: $*" >&2; return 3; }
  echo "control setup: command failed with rc $rc: $*" >&2
  return 1
}

control_build() { # control_build NAME DIR
  local rc=0
  [ "$(remaining)" -gt 1 ] || { echo "INCOMPLETE: deadline before control build $1" >&2; return 3; }
  ( build "$1" "$2/build.log" bounded_remaining bash "$root/scripts/checks/lib/slice0_native.sh" "$2" "$lcb" ) || rc=$?
  [ "$rc" -eq 0 ] && return 0
  if grep -q 'INCOMPLETE:' "$2/build.log"; then return 3; fi
  echo "control setup: native build failed for $1" >&2
  return 1
}

control_observe() { # control_observe EXE OUT
  local rc=0
  observe_remaining "$1" "$2" || rc=$?
  [ "$rc" -eq 0 ] && return 0
  [ "$rc" -ne 3 ] || return 3
  echo "control setup: observation failed for $1" >&2
  return 1
}

control_match() { # control_match EXPECTED OBS DESCRIPTION
  if ! judge "$1" "$2"; then
    echo "control setup: $3 (status $(cat "$2.status"), expected $(cat "$1/status"))" >&2
    return 1
  fi
}

first_outcome_index() { # first_outcome_index SUMMARY exit|trap [below-255]
  local index
  while IFS=$'\t' read -r tag index outcome production; do
    [ "$tag" = program ] && [ "$outcome" = "$2" ] || continue
    if [ "${3:-}" = below-255 ] && [ "$(cat "$work/bir/p$index/status")" -ge 255 ]; then continue; fi
    printf '%s\n' "$index"
    return 0
  done < <(awk -F '\t' '$1 == "program"' "$1")
  return 1
}

control_drop_overflow() {
  local index original mutated actual
  index=$(first_outcome_index "$work/bir/summary.tsv" trap) ||
    { echo "control setup: no generated trap" >&2; return 1; }
  original="$work/control-drop-original"
  mutated="$work/control-drop-mutated"
  control_bounded "${driver[@]}" replay "$seed" "$index" "$original" "$target" || return $?
  control_build control-drop-original "$original" || return $?
  control_observe "$original/llvm.exe" "$original/llvm" || return $?
  control_observe "$original/lcb.exe" "$original/lcb" || return $?
  control_match "$original" "$original/llvm" "unmutated LLVM candidate mismatch" || return 1
  control_match "$original" "$original/lcb" "unmutated LCB candidate mismatch" || return 1
  control_match "$original" "$original/lcb_eval" "unmutated evaluator mismatch" || return 1
  control_bounded "${driver[@]}" control-emit --mutate drop-overflow-llvm "$seed" "$index" "$mutated" "$target" \
    --control SPEC_DIFF_PLANTED_CONTROL || return $?
  if cmp -s "$original/llvm.ll" "$mutated/llvm.ll"; then
    echo "control setup: drop-overflow-llvm artifact did not differ" >&2
    return 1
  fi
  control_build control-drop-mutated "$mutated" || return $?
  control_observe "$mutated/llvm.exe" "$mutated/llvm" || return $?
  control_observe "$mutated/lcb.exe" "$mutated/lcb" || return $?
  control_match "$mutated/control-llvm" "$mutated/llvm" "LLVM did not produce the computed non-trap result" || return 1
  control_match "$original" "$mutated/lcb" "LCB did not retain the original trap" || return 1
  control_match "$original" "$mutated/lcb_eval" "evaluator did not retain the original trap" || return 1
  actual=$(cat "$mutated/control-llvm.status")
  echo "FAIL drop-overflow-llvm: planted LLVM trap became exact exit $actual; LCB retained trap"
  return 1
}

control_lcb_off_by_one() {
  local index original mutated actual
  index=$(first_outcome_index "$work/bir/summary.tsv" exit below-255) ||
    { echo "control setup: no generated exit below 255" >&2; return 1; }
  original="$work/control-lcb-original"
  mutated="$work/control-lcb-mutated"
  control_bounded "${driver[@]}" replay "$seed" "$index" "$original" "$target" || return $?
  control_build control-lcb-original "$original" || return $?
  control_observe "$original/llvm.exe" "$original/llvm" || return $?
  control_observe "$original/lcb.exe" "$original/lcb" || return $?
  control_match "$original" "$original/llvm" "unmutated LLVM candidate mismatch" || return 1
  control_match "$original" "$original/lcb" "unmutated LCB candidate mismatch" || return 1
  control_match "$original" "$original/lcb_eval" "unmutated evaluator mismatch" || return 1
  control_bounded "${driver[@]}" control-emit --mutate lcb-off-by-one "$seed" "$index" "$mutated" "$target" \
    --control SPEC_DIFF_PLANTED_CONTROL || return $?
  if cmp -s "$original/lcb_body.s" "$mutated/lcb_body.s"; then
    echo "control setup: lcb-off-by-one artifact did not differ" >&2
    return 1
  fi
  control_build control-lcb-mutated "$mutated" || return $?
  control_observe "$mutated/lcb.exe" "$mutated/lcb" || return $?
  control_match "$mutated/control-lcb" "$mutated/lcb" "native LCB did not return exactly word+1" || return 1
  control_match "$mutated/control-lcb" "$mutated/lcb_eval" "evaluator did not return exactly word+1" || return 1
  actual=$(cat "$mutated/control-lcb.status")
  echo "FAIL lcb-off-by-one: native LCB and evaluator both returned exact exit $actual"
  return 1
}

control_spec_oracle() {
  local d="$work/control-spec-oracle"
  control_bounded "${driver[@]}" control-spec-oracle "$d" "$target" --control SPEC_DIFF_PLANTED_CONTROL || return $?
  control_build control-spec-oracle "$d" || return $?
  control_observe "$d/llvm.exe" "$d/llvm" || return $?
  control_observe "$d/lcb.exe" "$d/lcb" || return $?
  control_match "$d/spec-expected" "$d/llvm" "stand-in LLVM does not match the spec" || return 1
  control_match "$d/spec-expected" "$d/lcb" "stand-in LCB does not match the spec" || return 1
  control_match "$d/spec-expected" "$d/lcb_eval" "stand-in evaluator does not match the spec" || return 1
  if judge "$d/wrong-expected" "$d/llvm"; then
    echo "control setup: judge accepted planted expected exit 43" >&2
    return 1
  fi
  echo "FAIL spec-oracle: judge rejected planted exit 43 against spec exit 42"
  return 1
}

term_fingerprint() {
  local d=$1 out=$2 list="$work/fingerprint-list"
  (cd "$d" && find . -type f -name program.lean.txt -print | LC_ALL=C sort) >"$list"
  : >"$out"
  while IFS= read -r path; do
    printf '%s\n' "$path" >>"$out"
    cat "$d/$path" >>"$out"
  done <"$list"
}

control_determinism() {
  local leg seed_a="$work/determinism-a" seed_b="$work/determinism-b" seed_c="$work/determinism-c"
  for leg in spec bir; do
    rm -rf "$seed_a" "$seed_b" "$seed_c"
    if [ "$leg" = spec ]; then
      require_bounded "${driver[@]}" gen-spec determinism-seed 16 "$seed_a"
      require_bounded "${driver[@]}" gen-spec determinism-seed 16 "$seed_b"
      require_bounded "${driver[@]}" gen-spec determinism-other 16 "$seed_c"
    else
      require_bounded "${driver[@]}" gen-bir determinism-seed 16 "$seed_a" "$target"
      require_bounded "${driver[@]}" gen-bir determinism-seed 16 "$seed_b" "$target"
      require_bounded "${driver[@]}" gen-bir determinism-other 16 "$seed_c" "$target"
    fi
    term_fingerprint "$seed_a" "$work/$leg-a.terms"
    term_fingerprint "$seed_b" "$work/$leg-b.terms"
    term_fingerprint "$seed_c" "$work/$leg-c.terms"
    if ! cmp -s "$work/$leg-a.terms" "$work/$leg-b.terms"; then
      echo "FAIL determinism $leg: identical seeds produced different terms" >&2; fail=1
    elif cmp -s "$work/$leg-a.terms" "$work/$leg-c.terms"; then
      echo "FAIL determinism $leg: different seeds produced identical terms" >&2; fail=1
    else
      echo "ok   control determinism $leg: same seed identical, different seed distinct"
    fi
  done
}

if [ -n "$replay" ]; then
  replay_leg=bir
  case "$replay" in
    bir:*) replay=${replay#bir:} ;;
    spec:*) replay_leg=spec; replay=${replay#spec:} ;;
  esac
  case "$replay" in
    *:*) replay_seed=${replay%%:*}; replay_index=${replay#*:} ;;
    *) echo "spec_diff: SPEC_DIFF_REPLAY must be [bir:|spec:]SEED:INDEX" >&2; exit 2 ;;
  esac
  [[ "$replay_index" =~ ^[0-9]+$ ]] || { echo "spec_diff: replay index is not a natural" >&2; exit 2; }
  d="$work/replay"
  if [ "$replay_leg" = spec ]; then
    require_bounded "${driver[@]}" replay-spec "$replay_seed" "$replay_index" "$d" "$target"
  else
    require_bounded "${driver[@]}" replay "$replay_seed" "$replay_index" "$d" "$target"
  fi
  rc=0
  native_one "$replay_leg" "$replay_index" "$d" || rc=$?
  [ "$rc" -ne 3 ] || exit 3
  [ "$rc" -le 1 ] || exit "$rc"
  echo "spec_diff replay: $replay_leg:$replay_seed:$replay_index judged on LLVM, LCB and LCB evaluator"
  exit "$rc"
fi

require_bounded "${driver[@]}" gen-spec "$seed" "$candidate_count" "$work/spec"
require_bounded "${driver[@]}" gen-bir "$seed" "$candidate_count" "$work/bir" "$target"
print_counts "spec leg" "$work/spec/summary.tsv"
print_counts "BIR leg" "$work/bir/summary.tsv"
check_generation "spec leg" "$work/spec/summary.tsv"
check_generation "BIR leg" "$work/bir/summary.tsv"

control_determinism
run_control drop-overflow-llvm \
  "FAIL drop-overflow-llvm: planted LLVM trap became exact exit" control_drop_overflow
run_control lcb-off-by-one \
  "FAIL lcb-off-by-one: native LCB and evaluator both returned exact exit" control_lcb_off_by_one
run_control spec-oracle \
  "FAIL spec-oracle: judge rejected planted exit 43 against spec exit 42" control_spec_oracle

while IFS=$'\t' read -r tag index outcome production; do
  [ "$tag" = program ] || continue
  if [ "$(remaining)" -lt 2 ]; then deadline_stopped=1; break; fi
  d="$work/bir/p$index"
  rc=0
  native_one bir "$index" "$d" || rc=$?
  if [ "$rc" -eq 3 ]; then deadline_stopped=1; break; fi
  if [ "$rc" -le 1 ]; then
    record_judged bir "$outcome" "$d"
    bir_judged=$((bir_judged + 1))
    [ "$rc" -eq 0 ] || fail=1
  else
    fail=1
  fi
done < <(awk -F '\t' '$1 == "program"' "$work/bir/summary.tsv")

spec_compiled=$(summary_value "$work/spec/summary.tsv" compiled)
while IFS=$'\t' read -r tag index outcome compiled; do
  [ "$tag" = program ] || continue
  [ "$compiled" = true ] || continue
  if [ "$(remaining)" -lt 2 ]; then deadline_stopped=1; break; fi
  d="$work/spec/p$index"
  if [ ! -f "$d/native-ready" ]; then
    echo "FAIL spec leg: compiled program lacks native-ready marker" >&2
    fail=1
    continue
  fi
  rc=0
  native_one spec "$index" "$d" || rc=$?
  if [ "$rc" -eq 3 ]; then deadline_stopped=1; break; fi
  if [ "$rc" -le 1 ]; then
    record_judged spec "$outcome" "$d"
    spec_judged=$((spec_judged + 1))
    [ "$rc" -eq 0 ] || fail=1
  else
    fail=1
  fi
done < <(awk -F '\t' '$1 == "program"' "$work/spec/summary.tsv")

print_judged_counts bir "$bir_judged"
print_judged_counts spec "$spec_judged"
check_judged bir "$bir_judged" "$minimum_judged" both checked wrapped widening two_sites chain
if [ "$spec_compiled" -gt 0 ]; then
  check_judged spec "$spec_judged" "$minimum_spec_judged" exits \
    let_literal let_copy add_literal add_variable return_literal return_variable
fi
if [ "$deadline_stopped" -ne 0 ]; then
  echo "INCOMPLETE: absolute deadline stopped generation, controls or comparisons" >&2
  exit 3
fi
echo "spec_diff: $bir_judged BIR programs judged on 3 legs; $spec_judged spec programs judged; seed $seed"
exit "$fail"
