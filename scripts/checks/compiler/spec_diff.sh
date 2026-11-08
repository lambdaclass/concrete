#!/usr/bin/env bash
# Seeded spec/compiler differential (Concrete/README.md gates 5 and 6). Run from the repo root in
# nix develop. The shared compiler build has the usual SLICE0_BUILD_SECS bound and builds the
# complete import closure of spec_diff.lean (Concrete and ConcreteSpec.Spec.Slice0). SPEC_DIFF_SECS
# (default 600) then supplies ONE monotonic deadline for generation, controls and comparisons,
# including bounded retries and process-group termination grace (BOUNDED_DEADLINE_MONO).
# SPEC_DIFF_N (default 300) caps candidates per leg. The minimum judged counts default to the 50%
# yield floor ((SPEC_DIFF_N + 1) / 2) for each compiled leg (SPEC_DIFF_MIN_JUDGED,
# SPEC_DIFF_MIN_SPEC_JUDGED), and when the deadline does not stop the run every accepted candidate
# on each active leg must be judged. Coverage and BIR exit/trap vacuity are checked over programs
# judged on ALL THREE legs. Any real failure (mismatch, control failure, capability/generation/build
# failure) exits 1 even if the deadline is also reached; a deadline stop without a prior failure is
# INCOMPLETE (3), never a pass.
#
# Replay: SPEC_DIFF_REPLAY=bir:SEED:INDEX or spec:SEED:INDEX (bare SEED:INDEX means bir).
# A mismatch writes a persistent bundle under SPEC_DIFF_REGRESSIONS (default
# .lake/concrete/spec_diff/regressions/<leg>/<safe_seed>-<index>/), namespaced by leg and
# collision-resistant seed identity, preserving each failing backend's observation, generated terms
# (plus .con source for spec-leg failures), the exact manifest row and Examples entry, and the
# candidate's replay command. CI uploads that directory on failure.
#
# Planted controls run with set -euo pipefail active, first prove the unmutated candidate, then
# require the precise planted effect and return a dedicated planted-failure status (99).
set -euo pipefail

seed=${SPEC_DIFF_SEED:-$(git rev-parse HEAD)}
candidate_count=${SPEC_DIFF_N:-300}
budget=${SPEC_DIFF_SECS:-600}
default_min_judged=$(((candidate_count + 1) / 2))
minimum_judged=${SPEC_DIFF_MIN_JUDGED:-$default_min_judged}
minimum_spec_judged=${SPEC_DIFF_MIN_SPEC_JUDGED:-$default_min_judged}
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

export BOUNDED_KILL_AFTER=${BOUNDED_KILL_AFTER:-5}
deadline_mono=$(python3 -I "$BOUNDED_EXEC" --monotonic-deadline "$budget")
export BOUNDED_DEADLINE_MONO="$deadline_mono"
remaining() { python3 -I "$BOUNDED_EXEC" --monotonic-remaining "$deadline_mono"; }

step_first_cap=${SPEC_DIFF_STEP_FIRST_SECS:-120}
step_retry_cap=${SPEC_DIFF_STEP_RETRY_SECS:-240}
obs_first_cap=${SPEC_DIFF_OBS_FIRST_SECS:-10}
obs_retry_cap=${SPEC_DIFF_OBS_RETRY_SECS:-20}
min_candidate_secs=${SPEC_DIFF_MIN_CANDIDATE_SECS:-15}
if [ "$min_candidate_secs" -gt $((budget / 2)) ] && [ $((budget / 2)) -ge 2 ]; then
  min_candidate_secs=$((budget / 2))
fi

driver=(env "SPEC_DIFF_TARGET=$target" lake env lean --run scripts/checks/compiler/spec_diff.lean)
regressions=${SPEC_DIFF_REGRESSIONS:-.lake/concrete/spec_diff/regressions}
case "$regressions" in /*) ;; *) regressions="$root/$regressions" ;; esac
fail=0
coverage_fail=0
deadline_stopped=0
bir_judged=0
spec_judged=0
PLANTED_FAIL_RC=99
: >"$work/judged-bir.tsv"
: >"$work/judged-spec.tsv"

# Run one command with per-step capped first and retry budgets, reserving termination grace inside
# the monotonic deadline.
bounded_remaining() {
  local total grace usable first retry rc=0
  total=$(remaining)
  grace=$((2 * BOUNDED_KILL_AFTER))
  usable=$((total - grace))
  if [ "$usable" -lt 2 ]; then
    if [ "$total" -lt 2 ]; then
      echo "INCOMPLETE: monotonic deadline before bounded command: $*" >&2
      return 3
    fi
    usable=$total
  fi
  first=$(((usable + 2) / 3))
  [ "$first" -ge 1 ] || first=1
  [ "$first" -le "$step_first_cap" ] || first=$step_first_cap
  retry=$((usable - first))
  [ "$retry" -ge 1 ] || retry=1
  [ "$retry" -le "$step_retry_cap" ] || retry=$step_retry_cap
  bounded_run -i /dev/null -r "$retry" "$first" -- "${capped[@]}" "$@" || rc=$?
  if [ "$rc" -ne 0 ] && bounded_is_incomplete "$rc"; then
    echo "INCOMPLETE: bounded command timed out: $*" >&2
    return 3
  fi
  return "$rc"
}

require_bounded() {
  local rc=0
  bounded_remaining "$@" || rc=$?
  [ "$rc" -eq 0 ] && return 0
  if [ "$rc" -eq 3 ]; then
    [ "$fail" -eq 0 ] || exit 1
    exit 3
  fi
  echo "FAIL command (rc $rc): $*" >&2
  exit "$rc"
}

observe_remaining() { # observe_remaining EXE OUT: status incomplete is always INCOMPLETE
  local total grace usable first retry rc=0
  total=$(remaining)
  grace=$((2 * BOUNDED_KILL_AFTER))
  usable=$((total - grace))
  if [ "$usable" -lt 2 ]; then
    if [ "$total" -lt 2 ]; then
      echo "INCOMPLETE: monotonic deadline before observing $1" >&2
      return 3
    fi
    usable=$total
  fi
  first=$(((usable + 2) / 3))
  [ "$first" -ge 1 ] || first=1
  [ "$first" -le "$obs_first_cap" ] || first=$obs_first_cap
  retry=$((usable - first))
  [ "$retry" -ge 1 ] || retry=1
  [ "$retry" -le "$obs_retry_cap" ] || retry=$obs_retry_cap
  observe "$1" "$2" "$first" "$retry" || rc=$?
  [ "$rc" -eq 0 ] || { echo "INCOMPLETE: observer failed for $1" >&2; return 3; }
  if [ "$(cat "$2.status")" = incomplete ]; then
    echo "INCOMPLETE: observation timed out for $1" >&2
    return 3
  fi
}

summary_value() { # summary_value FILE KEY [FIELD]
  awk -F '\t' -v key="$2" -v field="${3:-2}" '$1 == key {print $field; exit}' "$1"
}

summary_tag_count() { # summary_tag_count FILE KIND TAG
  awk -F '\t' -v kind="$2" -v tag="$3" '$1 == kind && $2 == tag {n += $3} END {print n+0}' "$1"
}

print_counts() { # print_counts LABEL SUMMARY: generated (not judged) counts
  echo "$1 generated outcome/construct counts:"
  awk -F '\t' '$1 == "outcome" || $1 == "construct" || $1 == "rejection" {printf "  %s %s %s\n", $1, $2, $3}' "$2"
  printf '  accepted %s, rejected %s, attempted %s\n' \
    "$(summary_value "$2" accepted)" "$(summary_value "$2" rejected)" "$(summary_value "$2" attempted)"
}

check_spec_capability() { # check_spec_capability SUMMARY: enforce X3 lowering capability contract
  local summary=$1 avail accepted supported compiled
  avail=$(summary_value "$summary" lowering_available)
  accepted=$(summary_value "$summary" accepted)
  supported=$(summary_value "$summary" supported_accepted)
  compiled=$(summary_value "$summary" compiled)
  [ -n "$avail" ] && [ -n "$accepted" ] && [ -n "$supported" ] && [ -n "$compiled" ] ||
    { echo "FAIL spec leg capability contract: missing capability fields in $summary" >&2; return 1; }
  if [ "$supported" -ne "$accepted" ]; then
    echo "FAIL spec leg capability contract: supported_accepted ($supported) != accepted ($accepted)" >&2
    return 1
  fi
  if [ "$avail" = false ]; then
    if [ "$compiled" -ne 0 ]; then
      echo "FAIL spec leg capability contract: lowering_available=false but compiled=$compiled" >&2
      return 1
    fi
  elif [ "$avail" = true ]; then
    if [ "$compiled" -ne "$supported" ] || [ "$compiled" -eq 0 ]; then
      echo "FAIL spec leg capability contract: partial lowering compiled $compiled of $supported supported accepted programs" >&2
      return 1
    fi
  else
    echo "FAIL spec leg capability contract: invalid lowering_available=$avail" >&2
    return 1
  fi
  return 0
}

check_generation() { # yield floor + spec oracle construct/rejection/capability checks
  local label summary accepted rejected attempted construct reason n
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
  if [ "$label" = "spec leg" ]; then
    check_spec_capability "$summary" || fail=1
    n=$(summary_tag_count "$summary" outcome exit)
    if [ "$n" -eq 0 ]; then
      echo "FAIL spec leg: zero spec oracle exits" >&2
      fail=1
    fi
    n=$(summary_tag_count "$summary" outcome trap)
    if [ "$n" -ne 0 ]; then
      echo "FAIL spec leg: accepted slice-0 program trapped ($n)" >&2
      fail=1
    fi
    for construct in let_literal let_copy add_literal add_variable return_literal return_variable; do
      n=$(summary_tag_count "$summary" construct "$construct")
      if [ "$n" -eq 0 ]; then
        echo "FAIL spec leg oracle coverage: zero $construct" >&2
        fail=1
      fi
    done
    for reason in out_of_range_literal known_overflow unused_binding; do
      n=$(summary_tag_count "$summary" rejection "$reason")
      if [ "$n" -eq 0 ]; then
        echo "FAIL spec leg rejection coverage: zero $reason" >&2
        fail=1
      fi
    done
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

check_judged() { # check_judged LEG COUNT MIN EXPECTED_TOTAL OUTCOME-GUARD REQUIRED-CONSTRUCTS...
  local leg count minimum expected_total outcome_guard construct missing=0 n
  leg=$1
  count=$2
  minimum=$3
  expected_total=$4
  outcome_guard=$5
  shift 5
  if [ "$count" -lt "$minimum" ]; then
    echo "FAIL $leg judged minimum: $count < $minimum" >&2
    missing=1
  fi
  if [ "$deadline_stopped" -eq 0 ] && [ "$count" -ne "$expected_total" ]; then
    echo "FAIL $leg capability/judged contract: judged $count != accepted/supported $expected_total" >&2
    missing=1
  fi
  if [ "$outcome_guard" = both ]; then
    for outcome in exit trap; do
      n=$(awk -F '\t' -v o="$outcome" '$1 == "outcome" && $2 == o {n++} END {print n+0}' "$work/judged-$leg.tsv")
      if [ "$n" -eq 0 ]; then
        echo "FAIL $leg judged vacuity: zero ${outcome}s" >&2
        missing=1
      fi
    done
  elif [ "$outcome_guard" = exits ]; then
    n=$(awk -F '\t' '$1 == "outcome" && $2 == "exit" {n++} END {print n+0}' "$work/judged-$leg.tsv")
    if [ "$n" -eq 0 ]; then
      echo "FAIL $leg judged vacuity: zero exits" >&2
      missing=1
    fi
    n=$(awk -F '\t' '$1 == "outcome" && $2 != "exit" {n++} END {print n+0}' "$work/judged-$leg.tsv")
    if [ "$n" -ne 0 ]; then
      echo "FAIL $leg judged vacuity: unexpected non-exit outcomes ($n)" >&2
      missing=1
    fi
  else
    echo "FAIL $leg judged check: unknown outcome guard $outcome_guard" >&2
    missing=1
  fi
  for construct in "$@"; do
    n=$(awk -F '\t' -v c="$construct" '$1 == "construct" && $2 == c {n++} END {print n+0}' "$work/judged-$leg.tsv")
    if [ "$n" -eq 0 ]; then
      echo "FAIL $leg judged coverage: zero $construct" >&2
      missing=1
    fi
  done
  [ "$missing" -eq 0 ] || coverage_fail=1
}

report_mismatch() { # report_mismatch LEG CAND_SEED INDEX DIR BACKEND: persist before scratch cleanup
  local leg cand_seed index d backend cand_safe_seed name bundle replay_text
  leg=$1
  cand_seed=$2
  index=$3
  d=$4
  backend=$5
  cand_safe_seed=$(cat "$d/safe-seed.txt")
  name=$(cat "$d/regression-name.txt")
  bundle="$regressions/$leg/${cand_safe_seed}-$index"
  mkdir -p "$bundle/expected" "$bundle/observed-$backend"
  printf '%s\n' "$cand_seed" >"$bundle/seed"
  printf '%s\n' "$cand_safe_seed" >"$bundle/safe-seed"
  printf '%s\n' "$index" >"$bundle/index"
  printf '%s\n' "$leg" >"$bundle/leg"
  printf '%s\n' "$backend" >"$bundle/observed-$backend/backend"
  if ! grep -qxF "$backend" "$bundle/backends" 2>/dev/null; then
    printf '%s\n' "$backend" >>"$bundle/backends"
  fi
  cp "$d/status" "$d/stderr" "$bundle/expected/"
  cp "$d/$backend.status" "$bundle/observed-$backend/status"
  cp "$d/$backend.stderr" "$bundle/observed-$backend/stderr"
  cp "$d/$backend.stdout" "$bundle/observed-$backend/stdout"
  cp "$d/program.lean.txt" "$bundle/program.lean.txt"
  cp "$d/constructs.txt" "$bundle/constructs.txt"
  cp "$d/manifest-row.txt" "$bundle/manifest-row.tsv"
  if [ "$leg" = spec ]; then
    mkdir -p "$bundle/tests/slice0"
    cp "$d/program.con" "$bundle/program.con"
    cp "$d/program.con" "$bundle/tests/slice0/$name.con"
    [ ! -f "$d/bir.txt" ] || cp "$d/bir.txt" "$bundle/bir.txt"
    {
      printf 'def %s : ConcreteSpec.Slice0.Program :=\n' "$name"
      cat "$d/program.lean.txt"
      printf '\n-- Source regression written to: tests/slice0/%s.con\n' "$name"
    } >"$bundle/examples.lean"
  else
    {
      printf 'def %s : Concrete.BIR.Program :=\n' "$name"
      cat "$d/program.lean.txt"
      printf '\n-- Add to Examples.all:\n("%s", %s),\n' "$name" "$name"
    } >"$bundle/examples.lean"
  fi
  replay_text="SPEC_DIFF_REPLAY='$leg:$cand_seed:$index' bash scripts/checks/compiler/spec_diff.sh"
  printf '%s\n' "$replay_text" >"$bundle/replay.sh"
  echo "FAIL $leg seed $cand_seed index $index [$backend]: observation mismatch" >&2
  echo "  expected status $(cat "$d/status"), observed $(cat "$d/$backend.status")" >&2
  echo "  regression bundle: $bundle" >&2
  echo "  exact manifest row:" >&2
  cat "$d/manifest-row.txt" >&2
  echo "  Examples definition: $bundle/examples.lean" >&2
  echo "  replay: $replay_text" >&2
}

native_one() { # native_one LEG CAND_SEED INDEX DIR: 0 match, 1 mismatch, 3 incomplete, 4 build/setup
  local leg cand_seed index d name backend bad=0 rc=0
  leg=$1
  cand_seed=$2
  index=$3
  d=$4
  name=$leg-$index
  if [ "$(remaining)" -lt "$min_candidate_secs" ]; then
    echo "INCOMPLETE: remaining time ($(remaining)s) below candidate build threshold (${min_candidate_secs}s)" >&2
    return 3
  fi
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
      report_mismatch "$leg" "$cand_seed" "$index" "$d" "$backend"
      bad=1
    fi
  done
  if judge "$d" "$d/lcb_eval"; then
    echo "ok   $name [lcb-eval]: status $(cat "$d/lcb_eval.status")"
  else
    report_mismatch "$leg" "$cand_seed" "$index" "$d" lcb_eval
    bad=1
  fi
  return "$bad"
}

run_control() { # fn runs under set -euo pipefail and must return PLANTED_FAIL_RC (99) + match expected regex
  local label expected_regex fn log rc
  label=$1
  expected_regex=$2
  fn=$3
  log="$work/ctl-$label.log"
  rc=0
  set +e
  ( set -euo pipefail; "$fn" ) >"$log" 2>&1
  rc=$?
  set -e
  if [ "$rc" -eq "$PLANTED_FAIL_RC" ] &&
     ! grep -q '^control setup:' "$log" &&
     ! grep -q 'INCOMPLETE:' "$log" &&
     grep -qE -- "$expected_regex" "$log"; then
    echo "ok   control $label: failed for the planted reason"
  elif [ "$rc" -eq 3 ] || grep -q 'INCOMPLETE:' "$log"; then
    cat "$log" >&2
    deadline_stopped=1
    if [ "$fail" -ne 0 ]; then exit 1; fi
    exit 3
  else
    echo "FAIL control $label: did not fail with rc=$PLANTED_FAIL_RC and pattern '$expected_regex' (got rc $rc)" >&2
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
  [ "$(remaining)" -ge "$min_candidate_secs" ] ||
    { echo "INCOMPLETE: deadline before control build $1" >&2; return 3; }
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
  local tag index outcome production
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
  if judge "$original" "$mutated/llvm"; then
    echo "control setup: judge accepted mutated drop-overflow-llvm against original trap expectation" >&2
    return 1
  fi
  control_match "$mutated/control-llvm" "$mutated/llvm" "LLVM did not produce the computed non-trap result" || return 1
  control_match "$original" "$mutated/lcb" "LCB did not retain the original trap" || return 1
  control_match "$original" "$mutated/lcb_eval" "evaluator did not retain the original trap" || return 1
  actual=$(cat "$mutated/control-llvm/status")
  [[ "$actual" =~ ^[0-9]+$ ]] || { echo "control setup: invalid control-llvm/status '$actual'" >&2; return 1; }
  echo "FAIL drop-overflow-llvm: planted LLVM trap became exact exit $actual; LCB retained trap"
  return "$PLANTED_FAIL_RC"
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
  control_observe "$mutated/llvm.exe" "$mutated/llvm" || return $?
  control_observe "$mutated/lcb.exe" "$mutated/lcb" || return $?
  control_match "$original" "$mutated/llvm" "LLVM did not retain the original exit" || return 1
  if judge "$original" "$mutated/lcb"; then
    echo "control setup: judge accepted mutated native LCB off-by-one against original expectation" >&2
    return 1
  fi
  if judge "$original" "$mutated/lcb_eval"; then
    echo "control setup: judge accepted mutated evaluator off-by-one against original expectation" >&2
    return 1
  fi
  control_match "$mutated/control-lcb" "$mutated/lcb" "native LCB did not return exactly word+1" || return 1
  control_match "$mutated/control-lcb" "$mutated/lcb_eval" "evaluator did not return exactly word+1" || return 1
  actual=$(cat "$mutated/control-lcb/status")
  [[ "$actual" =~ ^[0-9]+$ ]] || { echo "control setup: invalid control-lcb/status '$actual'" >&2; return 1; }
  echo "FAIL lcb-off-by-one: native LCB and evaluator both returned exact exit $actual"
  return "$PLANTED_FAIL_RC"
}

control_spec_oracle() {
  local d="$work/control-spec-oracle" leg_obs
  control_bounded "${driver[@]}" control-spec-oracle "$d" "$target" --control SPEC_DIFF_PLANTED_CONTROL || return $?
  control_build control-spec-oracle "$d" || return $?
  control_observe "$d/llvm.exe" "$d/llvm" || return $?
  control_observe "$d/lcb.exe" "$d/lcb" || return $?
  control_match "$d/spec-expected" "$d/llvm" "stand-in LLVM does not match the spec" || return 1
  control_match "$d/spec-expected" "$d/lcb" "stand-in LCB does not match the spec" || return 1
  control_match "$d/spec-expected" "$d/lcb_eval" "stand-in evaluator does not match the spec" || return 1
  for leg_obs in "$d/llvm" "$d/lcb" "$d/lcb_eval"; do
    if judge "$d/wrong-expected" "$leg_obs"; then
      echo "control setup: judge accepted planted expected exit 43 on $leg_obs" >&2
      return 1
    fi
  done
  echo "FAIL spec-oracle: judge rejected planted exit 43 against spec exit 42 on all 3 legs"
  return "$PLANTED_FAIL_RC"
}

control_partial_lowering() {
  local d="$work/control-partial-lowering" drv_log="$work/control-partial-drv.log" rc=0
  bounded_remaining "${driver[@]}" control-partial-lowering "$seed" 16 "$d" "$target" \
    --control SPEC_DIFF_PLANTED_CONTROL >"$drv_log" 2>&1 || rc=$?
  if [ "$rc" -eq 3 ]; then
    echo "INCOMPLETE: control-partial-lowering deadline" >&2
    return 3
  fi
  if [ "$rc" -eq 0 ] ||
     ! grep -qF "FAIL spec leg capability contract: loweringAvailable = true but compileCore refused supported accepted candidate" "$drv_log"; then
    echo "control setup: driver did not reject partial lowering (rc=$rc)" >&2
    cat "$drv_log" >&2
    return 1
  fi
  mkdir -p "$d"
  cat >"$d/fake-summary.tsv" <<'EOF'
seed	partial-seed
lowering_available	true
accepted	10
supported_accepted	10
rejected	2
attempted	12
compiled	4
EOF
  rc=0
  check_spec_capability "$d/fake-summary.tsv" >"$d/cap.log" 2>&1 || rc=$?
  if [ "$rc" -eq 0 ] ||
     ! grep -qF "FAIL spec leg capability contract: partial lowering compiled 4 of 10 supported accepted programs" "$d/cap.log"; then
    echo "control setup: check_spec_capability accepted partial lowering summary" >&2
    cat "$d/cap.log" >&2
    return 1
  fi
  echo "FAIL partial-lowering: driver and gate rejected partial lowering (4 of 10 compiled)"
  return "$PLANTED_FAIL_RC"
}

control_report_mismatch() {
  local regressions="$work/control-regressions"
  local bir_dir_a="$work/control-mm-bir-a" bir_dir_b="$work/control-mm-bir-b" spec_dir="$work/control-mm-spec"
  local seed_a="Replay-Seed:A" seed_b="Replay_Seed_A"
  local safe_a safe_b bir_bundle_a bir_bundle_b spec_bundle spec_name mdir rc=0
  control_bounded "${driver[@]}" replay "$seed_a" 0 "$bir_dir_a" "$target" || return $?
  control_bounded "${driver[@]}" replay "$seed_b" 0 "$bir_dir_b" "$target" || return $?
  # replay-spec returns 4 while compileCore is unwired, after writing the spec candidate files
  bounded_remaining "${driver[@]}" replay-spec "$seed_a" 0 "$spec_dir" "$target" >/dev/null 2>&1 || rc=$?
  if [ "$rc" -eq 3 ]; then
    echo "INCOMPLETE: replay-spec deadline in control_report_mismatch" >&2
    return 3
  fi
  [ -f "$spec_dir/manifest-row.txt" ] && [ -f "$spec_dir/program.con" ] ||
    { echo "control setup: replay-spec did not write spec regression metadata (rc $rc)" >&2; return 1; }
  for bdir in "$bir_dir_a" "$bir_dir_b" "$spec_dir"; do
    for bk in llvm lcb; do
      printf '250\n' >"$bdir/$bk.status"
      : >"$bdir/$bk.stderr"
      : >"$bdir/$bk.stdout"
    done
  done
  report_mismatch bir "$seed_a" 0 "$bir_dir_a" llvm 2>"$work/mm-bir-llvm.log"
  report_mismatch bir "$seed_a" 0 "$bir_dir_a" lcb 2>"$work/mm-bir-lcb.log"
  report_mismatch bir "$seed_b" 0 "$bir_dir_b" llvm 2>"$work/mm-bir-b.log"
  report_mismatch spec "$seed_a" 0 "$spec_dir" llvm 2>"$work/mm-spec.log"
  safe_a=$(cat "$bir_dir_a/safe-seed.txt")
  safe_b=$(cat "$bir_dir_b/safe-seed.txt")
  if [ "$safe_a" = "$safe_b" ]; then
    echo "control setup: safe-seed collided between '$seed_a' and '$seed_b'" >&2
    return 1
  fi
  bir_bundle_a="$regressions/bir/${safe_a}-0"
  bir_bundle_b="$regressions/bir/${safe_b}-0"
  spec_bundle="$regressions/spec/${safe_a}-0"
  [ -d "$bir_bundle_a" ] && [ -d "$bir_bundle_b" ] && [ -d "$spec_bundle" ] ||
    { echo "control setup: expected distinct bir and spec regression bundle directories" >&2; return 1; }
  [ "$(cat "$bir_bundle_a/seed")" = "$seed_a" ] ||
    { echo "control setup: bir bundle recorded wrong seed $(cat "$bir_bundle_a/seed")" >&2; return 1; }
  [ "$(cat "$spec_bundle/seed")" = "$seed_a" ] ||
    { echo "control setup: spec bundle recorded wrong seed $(cat "$spec_bundle/seed")" >&2; return 1; }
  grep -qxF "llvm" "$bir_bundle_a/backends" && grep -qxF "lcb" "$bir_bundle_a/backends" ||
    { echo "control setup: multi-backend mismatch did not preserve both llvm and lcb in backends" >&2; return 1; }
  [ -f "$bir_bundle_a/observed-llvm/status" ] && [ -f "$bir_bundle_a/observed-lcb/status" ] ||
    { echo "control setup: multi-backend observations missing" >&2; return 1; }
  grep -qF "SPEC_DIFF_REPLAY='bir:${seed_a}:0'" "$bir_bundle_a/replay.sh" ||
    { echo "control setup: bir replay.sh lacks exact candidate seed" >&2; return 1; }
  grep -qF "SPEC_DIFF_REPLAY='spec:${seed_a}:0'" "$spec_bundle/replay.sh" ||
    { echo "control setup: spec replay.sh lacks exact candidate seed" >&2; return 1; }
  spec_name=$(cat "$spec_dir/regression-name.txt")
  [ -f "$spec_bundle/tests/slice0/$spec_name.con" ] ||
    { echo "control setup: spec bundle missing tests/slice0/$spec_name.con" >&2; return 1; }
  mdir="$work/control-mm-manifest"
  mkdir -p "$mdir/tests/slice0"
  cp "$spec_bundle/tests/slice0/$spec_name.con" "$mdir/tests/slice0/$spec_name.con"
  {
    printf 'id\tprogram\texpect\tslice\n'
    cat "$bir_bundle_a/manifest-row.tsv"
    cat "$spec_bundle/manifest-row.tsv"
  } >"$mdir/tests/manifest.tsv"
  python3 -I "$root/scripts/checks/lib/test_manifest.py" validate "$mdir/tests/manifest.tsv" >/dev/null ||
    { echo "control setup: test_manifest.py rejected emitted regression rows" >&2; return 1; }
  echo "FAIL report-mismatch: persisted distinct bir and spec bundles for $seed_a index 0 and validated manifest rows"
  return "$PLANTED_FAIL_RC"
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
  native_one "$replay_leg" "$replay_seed" "$replay_index" "$d" || rc=$?
  [ "$rc" -ne 3 ] || exit 3
  [ "$rc" -le 1 ] || exit "$rc"
  echo "spec_diff replay: $replay_leg:$replay_seed:$replay_index judged on LLVM, LCB and LCB evaluator"
  exit "$rc"
fi

require_bounded bash "$root/scripts/checks/import_layering.sh"
require_bounded "${driver[@]}" gen-spec "$seed" "$candidate_count" "$work/spec"
require_bounded "${driver[@]}" gen-bir "$seed" "$candidate_count" "$work/bir" "$target"
print_counts "spec leg" "$work/spec/summary.tsv"
print_counts "BIR leg" "$work/bir/summary.tsv"
check_generation "spec leg" "$work/spec/summary.tsv"
check_generation "BIR leg" "$work/bir/summary.tsv"

control_determinism
run_control drop-overflow-llvm \
  '^FAIL drop-overflow-llvm: planted LLVM trap became exact exit [0-9]+; LCB retained trap$' \
  control_drop_overflow
run_control lcb-off-by-one \
  '^FAIL lcb-off-by-one: native LCB and evaluator both returned exact exit [0-9]+$' \
  control_lcb_off_by_one
run_control spec-oracle \
  '^FAIL spec-oracle: judge rejected planted exit 43 against spec exit 42 on all 3 legs$' \
  control_spec_oracle
run_control partial-lowering \
  '^FAIL partial-lowering: driver and gate rejected partial lowering \(4 of 10 compiled\)$' \
  control_partial_lowering
run_control report-mismatch \
  '^FAIL report-mismatch: persisted distinct bir and spec bundles for Replay-Seed:A index 0 and validated manifest rows$' \
  control_report_mismatch

bir_accepted=$(summary_value "$work/bir/summary.tsv" accepted)
while IFS=$'\t' read -r tag index outcome production; do
  [ "$tag" = program ] || continue
  if [ "$(remaining)" -lt "$min_candidate_secs" ]; then deadline_stopped=1; break; fi
  d="$work/bir/p$index"
  rc=0
  native_one bir "$seed" "$index" "$d" || rc=$?
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
spec_lowering_available=$(summary_value "$work/spec/summary.tsv" lowering_available)
spec_supported_accepted=$(summary_value "$work/spec/summary.tsv" supported_accepted)
while IFS=$'\t' read -r tag index outcome compiled; do
  [ "$tag" = program ] || continue
  [ "$compiled" = true ] || continue
  if [ "$(remaining)" -lt "$min_candidate_secs" ]; then deadline_stopped=1; break; fi
  d="$work/spec/p$index"
  if [ ! -f "$d/native-ready" ]; then
    echo "FAIL spec leg: compiled program lacks native-ready marker" >&2
    fail=1
    continue
  fi
  rc=0
  native_one spec "$seed" "$index" "$d" || rc=$?
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
check_judged bir "$bir_judged" "$minimum_judged" "$bir_accepted" both \
  checked wrapped widening two_sites chain \
  widen_u8_u16 widen_i8_i16 widen_u8_i16 \
  add sub mul \
  u8 i8 u16 i16
if [ "$spec_lowering_available" = true ] || [ "$spec_compiled" -gt 0 ]; then
  check_judged spec "$spec_judged" "$minimum_spec_judged" "$spec_supported_accepted" exits \
    let_literal let_copy add_literal add_variable return_literal return_variable
else
  echo "spec leg: 0 programs compiled (no Core→BIR lowering; becomes required in the lowering branch)"
fi

if [ "$fail" -ne 0 ]; then
  exit 1
fi
if [ "$deadline_stopped" -ne 0 ]; then
  echo "INCOMPLETE: monotonic deadline stopped generation, controls or comparisons" >&2
  exit 3
fi
if [ "$coverage_fail" -ne 0 ]; then
  exit 1
fi
echo "spec_diff: $bir_judged BIR programs judged on 3 legs; $spec_judged spec programs judged; seed $seed"
exit 0
