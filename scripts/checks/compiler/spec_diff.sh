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
# Every candidate failure (lowering, emission, assembly, native_compilation, native_setup,
# observation, mismatch) writes one durable failure record into the candidate directory and
# persists a regression bundle under SPEC_DIFF_REGRESSIONS (default
# .lake/concrete/spec_diff/regressions/<leg>/<safe_seed>-<index>/) before scratch cleanup,
# preserving the seed/index, leg, generated terms (plus .con source for spec-leg failures),
# every artifact available at that point, failure-kinds.txt, failure-record.tsv, diagnostics.txt,
# the exact manifest row and Examples entry, and the candidate's shell-quoted replay command.
# CI uploads that directory on failure.
#
# Planted controls run with set -euo pipefail active, first prove the unmutated candidate, then
# require the precise planted effect and return a dedicated planted-failure status (99).
set -euo pipefail

# Shared by normal replay and the saved-command round-trip controls.
parse_replay() {
  local value=$1
  replay_leg=bir
  case "$value" in
    bir:*) value=${value#bir:} ;;
    spec:*) replay_leg=spec; value=${value#spec:} ;;
  esac
  case "$value" in
    *:*) replay_seed=${value%:*}; replay_index=${value##*:} ;;
    *) echo "spec_diff: SPEC_DIFF_REPLAY must be [bir:|spec:]SEED:INDEX" >&2; return 2 ;;
  esac
  [[ "$replay_index" =~ ^[0-9]+$ ]] || { echo "spec_diff: replay index is not a natural" >&2; return 2; }
}

replay_command() {
  printf 'SPEC_DIFF_REPLAY=%q SPEC_DIFF_REPLAY_CONTROL= bash scripts/checks/compiler/spec_diff.sh' "$1:$2:$3"
}

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
printf 'replay: %s\n' "$(replay_command bir "$seed" INDEX)"

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

# Registration precedes generation, including controls. Identity is independent of
# artifacts: a killed generator need not have reached its metadata writes.
register_candidate() { # LEG SEED INDEX DIR [SCENARIO]
  local leg=$1 cand_seed=$2 index=$3 d=$4 scenario=${5:-}
  mkdir -p "$d"
  printf '%s\n' "$leg" >"$d/leg"
  printf '%s\n' "$cand_seed" >"$d/seed"
  printf '%s\n' "$index" >"$d/index"
  if ! grep -qxF "$d" "$work/lifecycle-dirs" 2>/dev/null; then
    printf '%s\n' "$d" >>"$work/lifecycle-dirs"
  fi
  if [ -n "${control_registry:-}" ]; then
    printf '%s\n' "$control_registry" >"$d/control-owner"
    if ! grep -qxF "$d" "$control_registry" 2>/dev/null; then
      printf '%s\n' "$d" >>"$control_registry"
    fi
  fi
  [ -z "$scenario" ] || [ -f "$d/control-replay" ] || printf '%s\n' "$scenario" >"$d/control-replay"
}

register_generation() { # Driver argv, called before the bounded launch (and any timeout).
  local cmd leg=bir cand_seed index d n i scenario="" plant_index=-1
  while [ "$#" -gt 0 ] && [ "$1" != scripts/checks/compiler/spec_diff.lean ]; do shift; done
  [ "$#" -gt 0 ] || return 0
  shift; cmd=$1; shift
  case "$cmd" in
    gen-spec|gen-bir|control-selective-refusal|control-partial-lowering|control-plant-bir)
      if [ "$cmd" = control-plant-bir ]; then scenario=$1; shift; fi
      cand_seed=$1; n=$2; d=$3
      case "$cmd" in gen-spec|control-partial-lowering) leg=spec ;; esac
      case "$cmd" in
        control-selective-refusal) scenario=emission; plant_index=$((n - 1)) ;;
        control-plant-bir) plant_index=$((n - 1)) ;;
        control-partial-lowering) scenario=lowering; plant_index=2 ;;
      esac
      for ((i=0; i<n; i++)); do
        if [ "$i" -eq "$plant_index" ]; then
          register_candidate "$leg" "$cand_seed" "$i" "$d/p$i" "$scenario"
        else
          register_candidate "$leg" "$cand_seed" "$i" "$d/p$i"
        fi
      done ;;
    replay|replay-spec|control-emit)
      if [ "$cmd" = control-emit ]; then scenario=$2; shift 2; fi
      cand_seed=$1; index=$2; d=$3
      [ "$cmd" != replay-spec ] || leg=spec
      if [ "${5:-}" = --plant-failure ] || [ "${5:-}" = --mutate ]; then scenario=$6; fi
      register_candidate "$leg" "$cand_seed" "$index" "$d" "$scenario" ;;
    control-spec-oracle)
      register_candidate spec control-spec-oracle 0 "$1" spec-oracle ;;
    *) echo "FAIL unregistered differential driver command: $cmd" >&2; return 1 ;;
  esac
}

# Run one command with per-step capped first and retry budgets, reserving termination grace inside
# the monotonic deadline.
bounded_remaining() {
  local total grace usable first retry rc=0
  register_generation "$@" || return $?
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
  # Reserve 3 for watchdog incompletion; a child exiting 3 is a real failure.
  [ "$rc" -ne 3 ] || rc=4
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
  if [ "$(cat "${1%/*}/control-replay" 2>/dev/null)" = observation ]; then
    echo 'FAIL planted observer setup' >&2; return 4
  fi
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
  [ "$rc" -eq 0 ] || { echo "FAIL observer failed for $1" >&2; return 4; }
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

candidate_bundle_path() { # LEG DIR: plant scenarios are separate regression identities.
  local leg=$1 d=$2 scenario=""
  if [ -f "$d/control-replay" ]; then
    scenario=$(LC_ALL=C tr -c 'a-z0-9+_-' '_' <"$d/control-replay")
    scenario=${scenario%_}
    printf '%s/controls/%s/%s/%s-%s\n' "$regressions" "$scenario" "$leg" "$(cat "$d/safe-seed.txt")" "$(cat "$d/index")"
  else
    printf '%s/%s/%s-%s\n' "$regressions" "$leg" "$(cat "$d/safe-seed.txt")" "$(cat "$d/index")"
  fi
}

persist_candidate_bundle() { # persist_candidate_bundle LEG CAND_SEED INDEX DIR
  local leg=$1 cand_seed=$2 index=$3 d=$4 cand_safe_seed name bundle backend replay_text
  [ -d "$d" ] || return 0
  if [ -f "$d/leg" ]; then leg=$(cat "$d/leg"); fi
  if [ -f "$d/seed" ]; then cand_seed=$(cat "$d/seed"); fi
  if [ -f "$d/index" ]; then index=$(cat "$d/index"); fi
  if [ -f "$d/safe-seed.txt" ]; then
    cand_safe_seed=$(cat "$d/safe-seed.txt")
  else
    cand_safe_seed=$(printf '%s' "$cand_seed" | LC_ALL=C tr '[:upper:]' '[:lower:]' | LC_ALL=C sed 's/[^a-z0-9_]/_/g' | cut -c1-24)
    [ -n "$cand_safe_seed" ] || cand_safe_seed=seed
    printf '%s\n' "$cand_safe_seed" >"$d/safe-seed.txt"
  fi
  if [ -f "$d/regression-name.txt" ]; then
    name=$(cat "$d/regression-name.txt")
  else
    name="regression_${leg}_${cand_safe_seed}_${index}"
    printf '%s\n' "$name" >"$d/regression-name.txt"
  fi
  printf '%s\n' "$leg" >"$d/leg"
  printf '%s\n' "$cand_seed" >"$d/seed"
  printf '%s\n' "$index" >"$d/index"
  replay_text=$(replay_command "$leg" "$cand_seed" "$index")
  if [ -f "$d/control-replay" ]; then
    replay_text=$(printf 'SPEC_DIFF_REPLAY=%q SPEC_DIFF_REPLAY_CONTROL=%q bash scripts/checks/compiler/spec_diff.sh' \
      "$leg:$cand_seed:$index" "$(cat "$d/control-replay")")
  fi
  printf '%s\n' "$replay_text" >"$d/replay.sh"
  bundle=$(candidate_bundle_path "$leg" "$d")
  rm -rf "${bundle:?}"
  mkdir -p "$bundle"
  cp -R "$d/." "$bundle/"
  printf '%s\n' "$cand_seed" >"$bundle/seed"
  printf '%s\n' "$cand_safe_seed" >"$bundle/safe-seed"
  printf '%s\n' "$index" >"$bundle/index"
  printf '%s\n' "$leg" >"$bundle/leg"
  printf '%s\n' "$replay_text" >"$bundle/replay.sh"
  if [ -f "$d/status" ]; then
    mkdir -p "$bundle/expected"
    cp "$d/status" "$bundle/expected/status"
    if [ -f "$d/stderr" ]; then
      cp "$d/stderr" "$bundle/expected/stderr"
    else
      : >"$bundle/expected/stderr"
    fi
  fi
  if [ -f "$d/backends" ]; then
    while IFS= read -r backend; do
      [ -n "$backend" ] || continue
      [ -f "$d/$backend.status" ] || continue
      mkdir -p "$bundle/observed-$backend"
      printf '%s\n' "$backend" >"$bundle/observed-$backend/backend"
      cp "$d/$backend.status" "$bundle/observed-$backend/status"
      if [ -f "$d/$backend.stderr" ]; then
        cp "$d/$backend.stderr" "$bundle/observed-$backend/stderr"
      else
        : >"$bundle/observed-$backend/stderr"
      fi
      if [ -f "$d/$backend.stdout" ]; then
        cp "$d/$backend.stdout" "$bundle/observed-$backend/stdout"
      else
        : >"$bundle/observed-$backend/stdout"
      fi
    done <"$d/backends"
  fi
  if [ -f "$d/manifest-row.txt" ]; then
    cp "$d/manifest-row.txt" "$bundle/manifest-row.tsv"
  fi
  if [ -f "$d/program.lean.txt" ]; then
    if [ "$leg" = spec ]; then
      if [ -f "$d/program.con" ]; then
        mkdir -p "$bundle/tests/slice0"
        cp "$d/program.con" "$bundle/program.con"
        cp "$d/program.con" "$bundle/tests/slice0/$name.con"
      fi
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
  fi
}

record_candidate_failure() { # record_candidate_failure LEG CAND_SEED INDEX DIR KIND SUBJECT DIAGNOSTIC [EXTRA_LOG]
  local leg=$1 cand_seed=$2 index=$3 d=$4 kind=$5 subject=$6 diagnostic=$7 extra_log=${8:-}
  local cand_safe_seed bundle replay_text status_line=""
  mkdir -p "$d"
  [ -f "$d/leg" ] || printf '%s\n' "$leg" >"$d/leg"
  [ -f "$d/seed" ] || printf '%s\n' "$cand_seed" >"$d/seed"
  [ -f "$d/index" ] || printf '%s\n' "$index" >"$d/index"
  if ! grep -qxF "$kind" "$d/failure-kinds.txt" 2>/dev/null; then
    printf '%s\n' "$kind" >>"$d/failure-kinds.txt"
  fi
  if [ "$kind" = mismatch ] && [ -f "$d/status" ] && [ -f "$d/$subject.status" ]; then
    if ! grep -qxF "$subject" "$d/backends" 2>/dev/null; then
      printf '%s\n' "$subject" >>"$d/backends"
    fi
    status_line="  expected status $(cat "$d/status"), observed $(cat "$d/$subject.status")"
  fi
  if ! awk -F '\t' -v k="$kind" -v s="$subject" -v m="$diagnostic" \
      '$1 == k && $2 == s && $3 == m {found=1} END {exit !found}' "$d/failure-record.tsv" 2>/dev/null; then
    printf '%s\t%s\t%s\n' "$kind" "$subject" "$diagnostic" >>"$d/failure-record.tsv"
  fi
  if ! grep -qxF "$diagnostic" "$d/diagnostics.txt" 2>/dev/null; then
    printf '%s\n' "$diagnostic" >>"$d/diagnostics.txt"
    [ -z "$status_line" ] || printf '%s\n' "$status_line" >>"$d/diagnostics.txt"
    if [ -n "$extra_log" ] && [ -s "$extra_log" ]; then
      cat "$extra_log" >>"$d/diagnostics.txt"
    fi
  fi
  : >"$d/bundle-reported"
  persist_candidate_bundle "$leg" "$cand_seed" "$index" "$d"
  cand_safe_seed=$(cat "$d/safe-seed.txt")
  bundle=$(candidate_bundle_path "$leg" "$d")
  replay_text=$(cat "$bundle/replay.sh")
  echo "$diagnostic" >&2
  [ -z "$status_line" ] || echo "$status_line" >&2
  echo "  regression bundle: $bundle" >&2
  if [ -f "$d/manifest-row.txt" ]; then
    echo "  exact manifest row:" >&2
    cat "$d/manifest-row.txt" >&2
  fi
  if [ -f "$bundle/examples.lean" ]; then
    echo "  Examples definition: $bundle/examples.lean" >&2
  fi
  echo "  replay: $replay_text" >&2
}

report_mismatch() { # report_mismatch LEG CAND_SEED INDEX DIR BACKEND: persist before scratch cleanup
  record_candidate_failure "$1" "$2" "$3" "$4" mismatch "$5" \
    "FAIL $1 seed $2 index $3 [$5]: observation mismatch"
}

judge_evaluator() { # judge_evaluator LEG CAND_SEED INDEX DIR: judge in-process evaluator before native build/runs
  local leg=$1 cand_seed=$2 index=$3 d=$4 name=$1-$3
  if [ ! -f "$d/lcb_eval.status" ]; then
    record_candidate_failure "$leg" "$cand_seed" "$index" "$d" native_setup lcb_eval \
      "FAIL $name [lcb_eval]: missing evaluator observation"
    return 4
  fi
  judge_available "$leg" "$cand_seed" "$index" "$d" lcb_eval
}

# One judge for live execution and exit collection. A completed observation is never
# deferred until a later backend finishes. Mark only after its verdict is durable.
judge_available() { # LEG SEED INDEX DIR BACKEND
  local leg=$1 cand_seed=$2 index=$3 d=$4 backend=$5 rc=0
  [ -f "$d/$backend.status" ] || return 0
  # Generation can commit an observation before committing its oracle. That is
  # not yet a comparison; collection must not invent a mismatch against no oracle.
  [ -f "$d/status" ] && [ -f "$d/stderr" ] || return 0
  [ ! -f "$d/$backend.judged" ] || return "$(cat "$d/$backend.judged")"
  if [ "$(cat "$d/$backend.status")" = incomplete ]; then return 3; fi
  if judge "$d" "$d/$backend"; then
    echo "ok   $leg-$index [$backend]: status $(cat "$d/$backend.status")"
  else
    report_mismatch "$leg" "$cand_seed" "$index" "$d" "$backend"
    rc=1
  fi
  printf '%s\n' "$rc" >"$d/$backend.judged"
  return "$rc"
}

reset_observations() { # DIR: retain generated evaluator result, discard prior run state
  local d=$1 backend
  for backend in llvm lcb; do
    rm -f "$d/$backend.status" "$d/$backend.stderr" "$d/$backend.stdout"
  done
  rm -f "$d"/*.judged "$d/failure-kinds.txt" "$d/failure-record.tsv" \
    "$d/diagnostics.txt" "$d/backends" "$d/bundle-reported" "$d/control-verified"
}

native_build() {
  if [ "$(cat "$1/control-replay" 2>/dev/null)" = native-compilation ]; then
    echo 'planted native compiler error'; return 7
  fi
  local rc=0 scenario
  bounded_remaining bash "$root/scripts/checks/lib/slice0_native.sh" "$1" "$lcb" || rc=$?
  [ "$rc" -eq 0 ] || return "$rc"
  scenario=$(cat "$1/control-replay" 2>/dev/null || true)
  case "$scenario" in
    *+llvm-timeout) printf '#!/usr/bin/env bash\nexec sleep 30\n' >"$1/llvm.exe"; chmod +x "$1/llvm.exe" ;;
    *+lcb-timeout) printf '#!/usr/bin/env bash\nexec sleep 30\n' >"$1/lcb.exe"; chmod +x "$1/lcb.exe" ;;
  esac
}

native_one() { # Result axes: native_failed (sticky), native_incomplete, native_complete
  # 0 match, 1 mismatch (possibly incomplete), 3 incomplete, 4 build/setup. Inspect BOTH axes.
  # Judges every available observation independently of whether later builds/runs complete.
  local leg cand_seed index d name backend bad=0 eval_rc=0 build_rc=0 obs_rc=0 incomplete=0 complete=1
  leg=$1
  cand_seed=$2
  index=$3
  d=$4
  register_candidate "$leg" "$cand_seed" "$index" "$d"
  rm -f "$d/control-verified"
  native_failed=0
  if [ -f "$d/failure-kinds.txt" ]; then native_failed=1; bad=1; fi
  native_incomplete=0
  native_complete=0
  name=$leg-$index
  if [ ! -f "$d/status" ] || [ ! -f "$d/stderr" ]; then
    record_candidate_failure "$leg" "$cand_seed" "$index" "$d" native_setup oracle \
      "FAIL $name: missing oracle observation"
    native_failed=1
    return 4
  fi
  judge_evaluator "$leg" "$cand_seed" "$index" "$d" || eval_rc=$?
  if [ "$eval_rc" -eq 1 ]; then
    bad=1
    native_failed=1
  elif [ "$eval_rc" -eq 3 ]; then
    incomplete=1
    native_incomplete=1
  elif [ "$eval_rc" -ne 0 ]; then
    native_failed=1
    return 4
  fi
  if [ "$(remaining)" -lt "$min_candidate_secs" ]; then
    echo "INCOMPLETE: remaining time ($(remaining)s) below candidate build threshold (${min_candidate_secs}s)" >&2
    native_incomplete=1
    [ "$native_failed" -eq 0 ] || return 1
    return 3
  fi
  native_build "$d" >"$d/build.log" 2>&1 || build_rc=$?
  if [ "$build_rc" -ne 0 ]; then
    cat "$d/build.log" >&2
    if [ "$build_rc" -eq 3 ]; then
      native_incomplete=1
      [ "$native_failed" -eq 0 ] || return 1
      return 3
    fi
    record_candidate_failure "$leg" "$cand_seed" "$index" "$d" native_compilation native_build \
      "FAIL $name: native build/setup failed" "$d/build.log"
    native_failed=1
    return 4
  fi
  for backend in llvm lcb; do
    rm -f "$d/$backend.judged" "$d/$backend.status" "$d/$backend.stderr" "$d/$backend.stdout"
    obs_rc=0
    observe_remaining "$d/$backend.exe" "$d/$backend" >"$d/$backend.obs.log" 2>&1 || obs_rc=$?
    if [ "$obs_rc" -ne 0 ]; then
      cat "$d/$backend.obs.log" >&2
      if [ "$obs_rc" -eq 3 ]; then
        incomplete=1
        native_incomplete=1
      else
        record_candidate_failure "$leg" "$cand_seed" "$index" "$d" observation "$backend" \
          "FAIL $leg seed $cand_seed index $index [$backend]: observation failed" "$d/$backend.obs.log"
        bad=1
        native_failed=1
        complete=0
      fi
      continue
    fi
    obs_rc=0
    judge_available "$leg" "$cand_seed" "$index" "$d" "$backend" || obs_rc=$?
    if [ "$obs_rc" -ne 0 ]; then bad=1; native_failed=1; fi
  done
  if [ "$incomplete" -ne 0 ]; then
    native_incomplete=1
    [ "$native_failed" -eq 0 ] || return 1
    return 3
  fi
  native_complete=$complete
  return "$bad"
}

# Failure and completion are independent. Only explicit timeout state stops coverage;
# setup errors and mismatches remain sticky failures even alongside a timeout.
absorb_native_result() {
  local rc=$1
  if [ "$native_failed" -ne 0 ] || { [ "$rc" -ne 0 ] && [ "$rc" -ne 3 ]; }; then fail=1; fi
  [ "$native_incomplete" -eq 0 ] || deadline_stopped=1
}

collect_available() {
  local leg cand_leg d index rc cand_seed cand_safe_seed bundle replay_text backend
  for leg in bir spec; do
    while IFS= read -r d; do
      [ -d "$d" ] || continue
      [ ! -f "$d/control-verified" ] || continue
      cand_leg=$leg
      cand_seed=$seed
      index=${d##*/p}
      if [ "$d" = "$work/replay" ]; then
        [ -n "$replay" ] && [ "$leg" = "$replay_leg" ] || continue
        cand_seed=$replay_seed
        index=$replay_index
      fi
      if [ -f "$d/leg" ]; then cand_leg=$(cat "$d/leg"); fi
      if [ -f "$d/seed" ]; then cand_seed=$(cat "$d/seed"); fi
      if [ -f "$d/index" ]; then index=$(cat "$d/index"); fi
      [ "$cand_leg" = "$leg" ] || continue
      for backend in lcb_eval llvm lcb; do
        rc=0
        judge_available "$cand_leg" "$cand_seed" "$index" "$d" "$backend" || rc=$?
        case "$rc" in 0) ;; 3) deadline_stopped=1 ;; *) fail=1 ;; esac
      done
      if [ -f "$d/failure-kinds.txt" ]; then
        persist_candidate_bundle "$cand_leg" "$cand_seed" "$index" "$d"
        cand_safe_seed=$(cat "$d/safe-seed.txt")
        bundle=$(candidate_bundle_path "$cand_leg" "$d")
        replay_text=$(cat "$bundle/replay.sh")
        if [ ! -f "$d/bundle-reported" ]; then
          : >"$d/bundle-reported"
          cp "$d/bundle-reported" "$bundle/bundle-reported"
          [ ! -f "$d/diagnostics.txt" ] || cat "$d/diagnostics.txt" >&2
          echo "  regression bundle: $bundle" >&2
          if [ -f "$d/manifest-row.txt" ]; then
            echo "  exact manifest row:" >&2
            cat "$d/manifest-row.txt" >&2
          fi
          if [ -f "$bundle/examples.lean" ]; then
            echo "  Examples definition: $bundle/examples.lean" >&2
          fi
          echo "  replay: $replay_text" >&2
        fi
        fail=1
      fi
    done < <({ printf '%s\n' "$work/$leg"/p* "$work/replay"; cat "$work/lifecycle-dirs" 2>/dev/null; } | LC_ALL=C sort -u)
  done
}

# All exits, including control/generation timeouts and errexit, collect committed
# observations before the shared scratch cleanup. Never infer outcome from log prose.
finish_diff() {
  local rc=$1
  trap - EXIT
  set +e
  collect_available
  bounded_summary
  if [ "$fail" -ne 0 ] || { [ "$rc" -ne 0 ] && [ "$rc" -ne 3 ]; }; then
    rc=1
  elif [ "$deadline_stopped" -ne 0 ] || grep -Eq '^(incomplete|runner-failed)' "$BOUNDED_LOG"; then
    rc=3
  fi
  rm -rf "${work:?}"
  exit "$rc"
}
trap 'finish_diff $?' EXIT

run_control() { # fn runs under set -euo pipefail and must return PLANTED_FAIL_RC (99) + match expected regex
  local label expected_regex fn log rc d control_registry
  label=$1
  expected_regex=$2
  fn=$3
  log="$work/ctl-$label.log"
  control_registry="$log.dirs"
  : >"$control_registry"
  rc=0
  set +e
  ( set -euo pipefail; "$fn" ) >"$log" 2>&1
  rc=$?
  set -e
  if [ "$rc" -eq "$PLANTED_FAIL_RC" ] &&
     ! grep -q '^control setup:' "$log" &&
     ! grep -q 'INCOMPLETE:' "$log" &&
     grep -qE -- "$expected_regex" "$log"; then
    # Only a fully verified planted effect is exempt from the outer failure verdict.
    while IFS= read -r d; do
      [ -d "$d" ] || continue
      [ "$(cat "$d/control-owner" 2>/dev/null)" = "$control_registry" ] || continue
      # Only the explicitly planted scenarios may carry expected failures. Judge
      # generated baseline observations too, even if no native run was requested.
      if [ ! -f "$d/control-replay" ]; then
        local backend baseline_rc=0
        for backend in lcb_eval llvm lcb; do
          local verdict=0
          judge_available "$(cat "$d/leg")" "$(cat "$d/seed")" "$(cat "$d/index")" "$d" "$backend" || verdict=$?
          case "$verdict" in 0) ;; 3) deadline_stopped=1; baseline_rc=3 ;; *) fail=1; baseline_rc=1 ;; esac
        done
        if [ -f "$d/failure-kinds.txt" ]; then fail=1; fi
        [ "$baseline_rc" -eq 0 ] && [ ! -f "$d/failure-kinds.txt" ] || continue
      fi
      : >"$d/control-verified"
    done <"$control_registry"
    echo "ok   control $label: failed for the planted reason"
  elif [ "$rc" -eq 3 ]; then
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

control_candidate() { # DIR [expected mismatch]
  local d=$1 rc=0
  native_one "$(cat "$d/leg" 2>/dev/null || echo bir)" "$(cat "$d/seed" 2>/dev/null || printf '%s' "$seed")" \
    "$(cat "$d/index" 2>/dev/null || echo 0)" "$d" || rc=$?
  [ "$rc" -eq 0 ] && return 0
  # A planted mismatch is accepted only after every leg completed. Incomplete
  # planted runs remain FAIL when any observation already proved a mismatch.
  if [ "${2:-}" = mismatch ] && [ "$rc" -eq 1 ] && [ "$native_complete" -eq 1 ] &&
     grep -qxF mismatch "$d/failure-kinds.txt" &&
     ! grep -qvxF mismatch "$d/failure-kinds.txt"; then return 0; fi
  return "$rc"
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

run_replay() {
  parse_replay "$replay"
  local d="$work/replay" rc=0
  driver_extra_args=()
  register_candidate "$replay_leg" "$replay_seed" "$replay_index" "$d" "${SPEC_DIFF_REPLAY_CONTROL:-}"
  case "${SPEC_DIFF_REPLAY_CONTROL:-}" in
    ""|spec-oracle|native-compilation|observation|report-mismatch) ;;
    emission|assembly|native-setup|lowering)
      driver_extra_args=(--plant-failure "$SPEC_DIFF_REPLAY_CONTROL" --control SPEC_DIFF_PLANTED_CONTROL) ;;
    drop-overflow-llvm|lcb-off-by-one|drop-overflow-llvm+lcb-timeout|lcb-off-by-one+llvm-timeout|lcb-off-by-one+lcb-timeout)
      driver_extra_args=(--mutate "${SPEC_DIFF_REPLAY_CONTROL%%+*}" --control SPEC_DIFF_PLANTED_CONTROL)
      case "$SPEC_DIFF_REPLAY_CONTROL" in
        *+*-timeout) obs_first_cap=1; obs_retry_cap=1; BOUNDED_KILL_AFTER=0 ;;
      esac ;;
    *) echo "FAIL unknown replay control: $SPEC_DIFF_REPLAY_CONTROL" >&2; exit 1 ;;
  esac
  if [ "${SPEC_DIFF_REPLAY_CONTROL:-}" = spec-oracle ]; then
    require_bounded "${driver[@]}" control-spec-oracle "$d" "$target" --control SPEC_DIFF_PLANTED_CONTROL
    cp "$d/spec-expected/status" "$d/status"
    cp "$d/spec-expected/stderr" "$d/stderr"
  elif [ "$replay_leg" = spec ] && [ "${SPEC_DIFF_REPLAY_CONTROL:-}" = report-mismatch ]; then
    bounded_remaining "${driver[@]}" replay-spec "$replay_seed" "$replay_index" "$d" "$target" || rc=$?
    [ "$rc" -eq 0 ] || [ "$rc" -eq 4 ] || exit "$rc"
    rc=0
  elif [ "$replay_leg" = spec ]; then
    require_bounded "${driver[@]}" replay-spec "$replay_seed" "$replay_index" "$d" "$target" \
      ${driver_extra_args[@]+"${driver_extra_args[@]}"}
  else
    require_bounded "${driver[@]}" replay "$replay_seed" "$replay_index" "$d" "$target" \
      ${driver_extra_args[@]+"${driver_extra_args[@]}"}
  fi
  if [ -n "${SPEC_DIFF_REPLAY_CONTROL:-}" ]; then
    printf '%s\n' "$SPEC_DIFF_REPLAY_CONTROL" >"$d/control-replay"
  fi
  if [ "${SPEC_DIFF_REPLAY_CONTROL:-}" = report-mismatch ]; then
    for backend in llvm lcb; do
      printf '250\n' >"$d/$backend.status"
      : >"$d/$backend.stderr"; : >"$d/$backend.stdout"
    done
    collect_available
    exit "$fail"
  fi
  native_one "$replay_leg" "$replay_seed" "$replay_index" "$d" || rc=$?
  absorb_native_result "$rc"
  [ "$fail" -eq 0 ] || exit 1
  [ "$native_incomplete" -eq 0 ] || exit 3
  echo "spec_diff replay: $replay_leg:$replay_seed:$replay_index judged on LLVM, LCB and LCB evaluator"
  exit "$rc"
}

# Replay verification has exactly one launch path. Only the destination for newly
# produced bundles is supplied externally; all failure state comes from replay.sh.
verify_saved_replay() { # BUNDLE OUTPUT_ROOT LOG
  local saved=$1 out=$2 log=$3 rc=0 reproduced kind subject diagnostic scenario
  bounded_remaining env -u BASH_ENV -u ENV SPEC_DIFF_REGRESSIONS="$out" \
    bash --noprofile --norc "$saved/replay.sh" >"$log" 2>&1 || rc=$?
  [ "$rc" -ne 3 ] || { cat "$log" >&2; return 3; }
  reproduced=$(regressions=$out candidate_bundle_path "$(cat "$saved/leg")" "$saved")
  if [ "$rc" -ne 1 ] || [ ! -d "$reproduced" ]; then
    echo "control setup: standalone replay did not fail and persist $reproduced (rc=$rc)" >&2
    cat "$log" >&2
    return 1
  fi
  while IFS=$'\t' read -r kind subject diagnostic; do
    grep -qxF "$kind" "$reproduced/failure-kinds.txt" &&
      grep -qxF "$diagnostic" "$log" &&
      grep -qxF "$diagnostic" "$reproduced/diagnostics.txt" || {
        echo "control setup: standalone replay lost $kind diagnostic '$diagnostic'" >&2
        cat "$log" >&2
        return 1
      }
  done <"$saved/failure-record.tsv"
  scenario=$(cat "$saved/control-replay" 2>/dev/null || true)
  case "$scenario" in
    *+llvm-timeout|*+lcb-timeout)
      local backend=${scenario##*+}; backend=${backend%-timeout}
      grep -qxE "INCOMPLETE: observation timed out for /.+/$backend.exe" "$log" || {
        echo "control setup: standalone replay did not reproduce planted $backend timeout" >&2
        cat "$log" >&2
        return 1
      } ;;
  esac
}

# Execute a candidate failure path through finish_diff (including scratch deletion), verify the
# durable failure bundle and every available stage artifact survived exit, then execute the saved
# replay.sh through the production replay parser and run_replay and verify the same failure and
# bundle are reproduced.
verify_failure_bundle_and_replay() {
  local label=$1 leg=$2 cand_seed=$3 index=$4 kind=$5 expected_diag=$6 extra_diag=$7
  local required_csv=$8 forbidden_csv=$9 first_fn=${10}
  local ctl_dir="$work/ctl-bundle-$label" sub_work="$work/ctl-bundle-$label/work-1"
  local regressions_1="$work/ctl-bundle-$label/reg-1" regressions_2="$work/ctl-bundle-$label/reg-2"
  local log_1="$work/ctl-bundle-$label/run-1.log" log_2="$work/ctl-bundle-$label/run-2.log"
  local rc=0 safe_seed bundle_1="" bundle_2 art
  local -a req_arts=() forb_arts=()
  rm -rf "$ctl_dir"
  mkdir -p "$ctl_dir"
  (
    work=$sub_work
    regressions=$regressions_1
    mkdir -p "$work"
    : >"$work/judged-bir.tsv"
    : >"$work/judged-spec.tsv"
    fail=0
    deadline_stopped=0
    replay=""
    trap 'finish_diff $?' EXIT
    "$first_fn"
  ) >"$log_1" 2>&1 || rc=$?
  [ "$rc" -ne 3 ] || { cat "$log_1" >&2; return 3; }
  if [ "$rc" -ne 1 ] || [ -e "$sub_work" ] || ! grep -qxF "$expected_diag" "$log_1"; then
    echo "control setup: $label initial run failed to exit 1, clean work, or log '$expected_diag' (rc=$rc)" >&2
    cat "$log_1" >&2
    return 1
  fi
  if [ -n "$extra_diag" ] && ! grep -qxF "$extra_diag" "$log_1"; then
    echo "control setup: $label initial run missing extra diagnostic '$extra_diag'" >&2
    cat "$log_1" >&2
    return 1
  fi
  bundle_1=$(find "$regressions_1" -type f -name failure-kinds.txt -print | sed -n '1p')
  bundle_1=${bundle_1%/failure-kinds.txt}
  if [ ! -d "$bundle_1" ] || [ ! -f "$bundle_1/safe-seed" ]; then
    echo "control setup: $label did not persist a regression bundle under $regressions_1/$leg" >&2
    cat "$log_1" >&2
    return 1
  fi
  safe_seed=$(cat "$bundle_1/safe-seed")
  [ "${bundle_1##*/}" = "${safe_seed}-$index" ] &&
    [ "$(cat "$bundle_1/leg")" = "$leg" ] &&
    [ "$(cat "$bundle_1/seed")" = "$cand_seed" ] &&
    [ "$(cat "$bundle_1/index")" = "$index" ] &&
    [ -s "$bundle_1/program.lean.txt" ] &&
    [ -s "$bundle_1/constructs.txt" ] &&
    [ -s "$bundle_1/manifest-row.tsv" ] &&
    [ -s "$bundle_1/examples.lean" ] &&
    [ -s "$bundle_1/expected/status" ] && [ -f "$bundle_1/expected/stderr" ] &&
    [ -s "$bundle_1/replay.sh" ] &&
    grep -qxF "$kind" "$bundle_1/failure-kinds.txt" &&
    awk -F '\t' -v k="$kind" '$1 == k {found=1} END {exit !found}' "$bundle_1/failure-record.tsv" &&
    grep -qxF "$expected_diag" "$bundle_1/diagnostics.txt" || {
      echo "control setup: $label persisted bundle $bundle_1 is incomplete or has wrong metadata" >&2
      cat "$log_1" >&2
      return 1
    }
  if [ -n "$extra_diag" ] && ! grep -qxF "$extra_diag" "$bundle_1/diagnostics.txt"; then
    echo "control setup: $label persisted diagnostics.txt missing '$extra_diag'" >&2
    cat "$bundle_1/diagnostics.txt" >&2
    return 1
  fi
  IFS=',' read -r -a req_arts <<<"$required_csv"
  for art in "${req_arts[@]}"; do
    [ -z "$art" ] || [ -f "$bundle_1/$art" ] || {
      echo "control setup: $label bundle $bundle_1 missing required artifact '$art'" >&2
      return 1
    }
  done
  IFS=',' read -r -a forb_arts <<<"$forbidden_csv"
  for art in "${forb_arts[@]}"; do
    [ -z "$art" ] || [ ! -e "$bundle_1/$art" ] || {
      echo "control setup: $label bundle $bundle_1 unexpectedly contains forbidden artifact '$art'" >&2
      return 1
    }
  done
  rc=0
  # A new shell executes only the saved command; no functions, driver arguments,
  # plants or observations are supplied by the verifier. Output routing is not a plant.
  verify_saved_replay "$bundle_1" "$regressions_2" "$log_2" || return $?
  bundle_2="$regressions_2/${bundle_1#"$regressions_1/"}"
  if ! grep -qxF "$expected_diag" "$log_2" || [ ! -d "$bundle_2" ]; then
    echo "control setup: $label replay did not reproduce failure '$expected_diag' and persist $bundle_2 (rc=$rc)" >&2
    cat "$log_2" >&2
    return 1
  fi
  if [ -n "$extra_diag" ] && ! grep -qxF "$extra_diag" "$log_2"; then
    echo "control setup: $label replay missing extra diagnostic '$extra_diag'" >&2
    cat "$log_2" >&2
    return 1
  fi
  [ "$(cat "$bundle_2/leg")" = "$leg" ] &&
    [ "$(cat "$bundle_2/seed")" = "$cand_seed" ] &&
    [ "$(cat "$bundle_2/index")" = "$index" ] &&
    cmp -s "$bundle_1/program.lean.txt" "$bundle_2/program.lean.txt" &&
    cmp -s "$bundle_1/replay.sh" "$bundle_2/replay.sh" &&
    grep -qxF "$kind" "$bundle_2/failure-kinds.txt" &&
    awk -F '\t' -v k="$kind" '$1 == k {found=1} END {exit !found}' "$bundle_2/failure-record.tsv" &&
    grep -qxF "$expected_diag" "$bundle_2/diagnostics.txt" || {
      echo "control setup: $label replay bundle $bundle_2 does not match initial bundle $bundle_1" >&2
      cat "$log_2" >&2
      return 1
    }
  if [ -n "$extra_diag" ] && ! grep -qxF "$extra_diag" "$bundle_2/diagnostics.txt"; then
    echo "control setup: $label replay diagnostics.txt missing '$extra_diag'" >&2
    cat "$bundle_2/diagnostics.txt" >&2
    return 1
  fi
  for art in "${req_arts[@]}"; do
    [ -z "$art" ] || [ -f "$bundle_2/$art" ] || {
      echo "control setup: $label replay bundle $bundle_2 missing required artifact '$art'" >&2
      return 1
    }
  done
  for art in "${forb_arts[@]}"; do
    [ -z "$art" ] || [ ! -e "$bundle_2/$art" ] || {
      echo "control setup: $label replay bundle $bundle_2 unexpectedly contains forbidden artifact '$art'" >&2
      return 1
    }
  done
}

control_drop_overflow() {
  local index original mutated actual
  index=$(first_outcome_index "$work/bir/summary.tsv" trap) ||
    { echo "control setup: no generated trap" >&2; return 1; }
  original="$work/control-drop-original"
  mutated="$work/control-drop-mutated"
  control_bounded "${driver[@]}" replay "$seed" "$index" "$original" "$target" || return $?
  control_candidate "$original" || return $?
  control_bounded "${driver[@]}" control-emit --mutate drop-overflow-llvm "$seed" "$index" "$mutated" "$target" \
    --control SPEC_DIFF_PLANTED_CONTROL || return $?
  if cmp -s "$original/llvm.ll" "$mutated/llvm.ll"; then
    echo "control setup: drop-overflow-llvm artifact did not differ" >&2
    return 1
  fi
  echo drop-overflow-llvm >"$mutated/control-replay"
  control_candidate "$mutated" mismatch || return $?
  if judge "$original" "$mutated/llvm"; then
    echo "control setup: judge accepted mutated drop-overflow-llvm against original trap expectation" >&2
    return 1
  fi
  control_match "$mutated/control-llvm" "$mutated/llvm" "LLVM did not produce the computed non-trap result" || return 1
  control_match "$original" "$mutated/lcb" "LCB did not retain the original trap" || return 1
  control_match "$original" "$mutated/lcb_eval" "evaluator did not retain the original trap" || return 1
  actual=$(cat "$mutated/control-llvm/status")
  [[ "$actual" =~ ^[0-9]+$ ]] || { echo "control setup: invalid control-llvm/status '$actual'" >&2; return 1; }
  first_mismatch() {
    local rc=0
    mkdir -p "$work/bir"
    cp -R "$mutated" "$work/bir/p$index"
    rm -f "$work/bir/p$index/llvm.status" "$work/bir/p$index/llvm.stderr" "$work/bir/p$index/llvm.stdout" \
          "$work/bir/p$index/lcb.status" "$work/bir/p$index/lcb.stderr" "$work/bir/p$index/lcb.stdout" \
          "$work/bir/p$index/lcb_eval.judged" "$work/bir/p$index/failure-kinds.txt" \
          "$work/bir/p$index/failure-record.tsv" "$work/bir/p$index/diagnostics.txt" \
          "$work/bir/p$index/backends" "$work/bir/p$index/bundle-reported"
    native_build() { return 0; }
    native_one bir "$seed" "$index" "$work/bir/p$index" || rc=$?
    absorb_native_result "$rc"
    [ "$fail" -eq 0 ] || exit 1
    exit "$rc"
  }
  verify_failure_bundle_and_replay mismatch bir "$seed" "$index" mismatch \
    "FAIL bir seed $seed index $index [llvm]: observation mismatch" \
    "  expected status $(cat "$original/status"), observed $actual" \
    "program.lean.txt,bir.txt,llvm.ll,lcb_source.txt,lcb_job.txt,lcb_rt.ssa,lcb_body.ssa,lcb_body.s,lcb_eval.status,llvm.exe,lcb.exe,backends,observed-llvm/status,observed-llvm/stderr,observed-llvm/stdout,manifest-row.tsv,examples.lean" \
    "observed-lcb/status,observed-lcb_eval/status" \
    first_mismatch || return $?
  # Reuse the proven mutated LLVM binary, then force a real LCB observation timeout.
  # The build is already complete; do not regenerate the planted sleeper away.
  (
    local rc=0 obs_first_cap=1 obs_retry_cap=1 safe_seed bundle sub="$work/control-drop-timeout"
    local BOUNDED_KILL_AFTER=0 BOUNDED_LOG="$work/expected-timeouts.log"
    local regressions="$work/mismatch-timeout-regressions"
    rm -rf "$sub" "$regressions"
    cp -R "$mutated" "$sub"
    reset_observations "$sub"
    echo drop-overflow-llvm+lcb-timeout >"$sub/control-replay"
    native_build() { return 0; }
    printf '#!/usr/bin/env bash\nexec sleep 30\n' >"$sub/lcb.exe"
    chmod +x "$sub/lcb.exe"
    native_one bir "$seed" "$index" "$sub" >"$work/mismatch-timeout.log" 2>&1 || rc=$?
    safe_seed=$(cat "$sub/safe-seed.txt")
    bundle=$(candidate_bundle_path bir "$sub")
    [ "$rc" -eq 1 ] && [ "$native_failed" -eq 1 ] && [ "$native_complete" -eq 0 ] &&
      grep -qxF "FAIL bir seed $seed index $index [llvm]: observation mismatch" "$work/mismatch-timeout.log" &&
      grep -qxF "INCOMPLETE: observation timed out for $sub/lcb.exe" "$work/mismatch-timeout.log" &&
      [ -f "$bundle/observed-llvm/status" ] && grep -qxF llvm "$bundle/backends" || {
        echo "control setup: mismatch was lost or timeout did not occur for the planted reason" >&2
        cat "$work/mismatch-timeout.log" >&2
        return 1
      }
    verify_saved_replay "$bundle" "$work/replay-drop-timeout" "$work/replay-drop-timeout.log" || return $?
    echo 'FAIL mismatch-timeout: LLVM mismatch retained with incomplete LCB observation'
  ) || return $?
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
  control_candidate "$original" || return $?
  control_bounded "${driver[@]}" control-emit --mutate lcb-off-by-one "$seed" "$index" "$mutated" "$target" \
    --control SPEC_DIFF_PLANTED_CONTROL || return $?
  if cmp -s "$original/lcb_body.s" "$mutated/lcb_body.s"; then
    echo "control setup: lcb-off-by-one artifact did not differ" >&2
    return 1
  fi
  echo lcb-off-by-one >"$mutated/control-replay"
  control_candidate "$mutated" mismatch || return $?
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
  # Prove that an evaluator mismatch is judged and persisted even when LLVM or LCB native
  # observation times out, and that the surviving native backend is still observed and judged.
  (
    local rc=0 obs_first_cap=1 obs_retry_cap=1 safe_seed bundle log
    local BOUNDED_KILL_AFTER=0 BOUNDED_LOG="$work/expected-timeouts.log"
    local regressions="$work/eval-llvm-timeout-regressions"
    local sub="$work/control-lcb-llvm-timeout"
    rm -rf "$sub" "$regressions"
    cp -R "$mutated" "$sub"
    reset_observations "$sub"
    echo lcb-off-by-one+llvm-timeout >"$sub/control-replay"
    native_build() { return 0; }
    printf '#!/usr/bin/env bash\nexec sleep 30\n' >"$sub/llvm.exe"
    chmod +x "$sub/llvm.exe"
    log="$work/eval-llvm-timeout.log"
    native_one bir "$seed" "$index" "$sub" >"$log" 2>&1 || rc=$?
    safe_seed=$(cat "$sub/safe-seed.txt")
    bundle=$(candidate_bundle_path bir "$sub")
    [ "$rc" -eq 1 ] && [ "$native_failed" -eq 1 ] && [ "$native_complete" -eq 0 ] &&
      grep -qxF "FAIL bir seed $seed index $index [lcb_eval]: observation mismatch" "$log" &&
      grep -qxF "INCOMPLETE: observation timed out for $sub/llvm.exe" "$log" &&
      grep -qxF "FAIL bir seed $seed index $index [lcb]: observation mismatch" "$log" &&
      [ -f "$bundle/observed-lcb_eval/status" ] &&
      [ "$(cat "$bundle/observed-lcb_eval/status")" = "$actual" ] &&
      [ -f "$bundle/observed-lcb/status" ] &&
      [ "$(cat "$bundle/observed-lcb/status")" = "$actual" ] &&
      grep -qxF lcb_eval "$bundle/backends" && grep -qxF lcb "$bundle/backends" || {
        echo "control setup: evaluator/LCB mismatch or bundle lost under LLVM timeout" >&2
        cat "$log" >&2
        return 1
      }
    verify_saved_replay "$bundle" "$work/replay-eval-llvm-timeout" "$work/replay-eval-llvm-timeout.log" || return $?
    echo 'FAIL eval-llvm-timeout: evaluator and LCB mismatches retained and bundled despite LLVM timeout'
  ) || return $?
  (
    local rc=0 obs_first_cap=1 obs_retry_cap=1 safe_seed bundle log
    local BOUNDED_KILL_AFTER=0 BOUNDED_LOG="$work/expected-timeouts.log"
    local regressions="$work/eval-lcb-timeout-regressions"
    local sub="$work/control-lcb-lcb-timeout"
    rm -rf "$sub" "$regressions"
    cp -R "$mutated" "$sub"
    reset_observations "$sub"
    echo lcb-off-by-one+lcb-timeout >"$sub/control-replay"
    native_build() { return 0; }
    printf '#!/usr/bin/env bash\nexec sleep 30\n' >"$sub/lcb.exe"
    chmod +x "$sub/lcb.exe"
    log="$work/eval-lcb-timeout.log"
    native_one bir "$seed" "$index" "$sub" >"$log" 2>&1 || rc=$?
    safe_seed=$(cat "$sub/safe-seed.txt")
    bundle=$(candidate_bundle_path bir "$sub")
    [ "$rc" -eq 1 ] && [ "$native_failed" -eq 1 ] && [ "$native_complete" -eq 0 ] &&
      grep -qxF "FAIL bir seed $seed index $index [lcb_eval]: observation mismatch" "$log" &&
      grep -qxF "ok   bir-$index [llvm]: status $(cat "$original/status")" "$log" &&
      grep -qxF "INCOMPLETE: observation timed out for $sub/lcb.exe" "$log" &&
      [ -f "$bundle/observed-lcb_eval/status" ] &&
      [ "$(cat "$bundle/observed-lcb_eval/status")" = "$actual" ] &&
      grep -qxF lcb_eval "$bundle/backends" && [ ! -e "$bundle/observed-lcb" ] || {
        echo "control setup: evaluator mismatch or bundle lost under LCB timeout" >&2
        cat "$log" >&2
        return 1
      }
    verify_saved_replay "$bundle" "$work/replay-eval-lcb-timeout" "$work/replay-eval-lcb-timeout.log" || return $?
    echo 'FAIL eval-lcb-timeout: evaluator mismatch retained and bundled despite LCB timeout'
  ) || return $?
  echo "FAIL lcb-off-by-one: native LCB and evaluator both returned exact exit $actual"
  return "$PLANTED_FAIL_RC"
}

control_spec_oracle() {
  local d="$work/control-spec-oracle" leg_obs
  control_bounded "${driver[@]}" control-spec-oracle "$d" "$target" --control SPEC_DIFF_PLANTED_CONTROL || return $?
  cp "$d/spec-expected/status" "$d/status"
  cp "$d/spec-expected/stderr" "$d/stderr"
  echo spec-oracle >"$d/control-replay"
  control_candidate "$d" || return $?
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
  local d="$work/control-partial-lowering" rc=0
  local expected_diag="FAIL spec leg capability contract: loweringAvailable = true but compileCore refused supported accepted candidate (seed $seed index 2): planted partial-lowering capability refusal"
  mkdir -p "$d"
  first_lowering() {
    local inner_rc=0
    bounded_remaining "${driver[@]}" control-partial-lowering "$seed" 16 "$work/spec" "$target" \
      --control SPEC_DIFF_PLANTED_CONTROL || inner_rc=$?
    [ "$inner_rc" -ne 3 ] || exit 3
    [ -f "$work/spec/p0/native-ready" ] && [ -f "$work/spec/p1/native-ready" ] || {
      echo "control setup: driver did not emit native-ready for p0 and p1 before refusal" >&2
      exit 2
    }
    exit "$inner_rc"
  }
  verify_failure_bundle_and_replay lowering spec "$seed" 2 lowering "$expected_diag" "" \
    "program.lean.txt,program.con,oracle.txt,manifest-row.tsv,examples.lean" \
    "bir.txt,llvm.ll,lcb_body.s,native-ready" \
    first_lowering || return $?
  grep -qxF 'spec compiled candidate: index 0, compiled 1' "$work/ctl-bundle-lowering/run-1.log" &&
    grep -qxF 'spec compiled candidate: index 1, compiled 2' "$work/ctl-bundle-lowering/run-1.log" || {
      echo "control setup: driver did not log two compiled candidates before capability refusal" >&2
      cat "$work/ctl-bundle-lowering/run-1.log" >&2
      return 1
    }
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
  if [ "$rc" -ne 1 ] ||
     ! grep -qxF "FAIL spec leg capability contract: partial lowering compiled 4 of 10 supported accepted programs" "$d/cap.log"; then
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
  local seed_a="Replay-Seed:A's" seed_b="Replay_Seed_A_s"
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
    echo report-mismatch >"$bdir/control-replay"
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
  bir_bundle_a=$(candidate_bundle_path bir "$bir_dir_a")
  bir_bundle_b=$(candidate_bundle_path bir "$bir_dir_b")
  spec_bundle=$(candidate_bundle_path spec "$spec_dir")
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
  for replay_bundle in "$bir_bundle_a" "$bir_bundle_b" "$spec_bundle"; do
    local replay_reg="$work/report-replay-$(cat "$replay_bundle/leg")-$(cat "$replay_bundle/safe-seed")"
    local replay_log="$replay_reg.log"
    verify_saved_replay "$replay_bundle" "$replay_reg" "$replay_log" || return $?
  done
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

control_selective_refusal() {
  local d="$work/control-selective"
  local expected_diag="FAIL BIR emission: seed $seed index 63: planted selective emission refusal"
  mkdir -p "$d"
  control_bounded "${driver[@]}" gen-bir "$seed" 64 "$d-baseline" "$target" || return $?
  first_emission() {
    local inner_rc=0
    bounded_remaining "${driver[@]}" control-selective-refusal "$seed" 64 "$work/bir" "$target" \
      --control SPEC_DIFF_PLANTED_CONTROL || inner_rc=$?
    [ "$inner_rc" -ne 3 ] || exit 3
    cp "$work/bir/summary.tsv" "$d/summary.tsv"
    exit "$inner_rc"
  }
  verify_failure_bundle_and_replay emission bir "$seed" 63 emission "$expected_diag" "" \
    "program.lean.txt,bir.txt,llvm.ll,manifest-row.tsv,examples.lean" \
    "lcb_body.s,lcb_eval.status,llvm.exe,lcb.exe" \
    first_emission || return $?
  # The refused candidate is redundant for aggregate construct coverage and yield.
  [ "$(summary_value "$d/summary.tsv" accepted)" -ge 32 ] || return 1
  diff <(awk -F '\t' '$1 == "construct" {print $2}' "$d-baseline/summary.tsv" | sort) \
       <(awk -F '\t' '$1 == "construct" {print $2}' "$d/summary.tsv" | sort) || return 1
  echo 'FAIL selective-refusal: admitted candidate refused despite intact aggregate coverage'
  return "$PLANTED_FAIL_RC"
}

control_assembly_failure() {
  local expected_diag="FAIL bir assembly: seed $seed index 0 [aarch64-macos]: planted LCB assembly refusal"
  first_assembly() {
    require_bounded "${driver[@]}" control-plant-bir assembly "$seed" 1 "$work/bir" "$target" \
      --control SPEC_DIFF_PLANTED_CONTROL
  }
  verify_failure_bundle_and_replay assembly bir "$seed" 0 assembly "$expected_diag" "" \
    "program.lean.txt,bir.txt,llvm.ll,lcb_source.txt,lcb_job.txt,lcb_rt.ssa,lcb_body.ssa,manifest-row.tsv,examples.lean" \
    "lcb_body.s,lcb_eval.status,llvm.exe,lcb.exe" \
    first_assembly || return $?
  echo "FAIL assembly-failure: persisted and replayed LCB assembly failure bundle for seed $seed index 0"
  return "$PLANTED_FAIL_RC"
}

control_native_compilation_failure() {
  local parent_work=$work
  local expected_diag="FAIL bir-0: native build/setup failed"
  local extra_diag="planted native compiler error"
  first_native_comp() {
    local rc=0
    mkdir -p "$work/bir"
    cp -R "$parent_work/bir/p0" "$work/bir/p0"
    echo native-compilation >"$work/bir/p0/control-replay"
    native_one bir "$seed" 0 "$work/bir/p0" || rc=$?
    absorb_native_result "$rc"
    [ "$fail" -eq 0 ] || exit 1
    exit "$rc"
  }
  verify_failure_bundle_and_replay native-compilation bir "$seed" 0 native_compilation \
    "$expected_diag" "$extra_diag" \
    "program.lean.txt,bir.txt,llvm.ll,lcb_source.txt,lcb_job.txt,lcb_rt.ssa,lcb_body.ssa,lcb_body.s,lcb_eval.status,build.log,manifest-row.tsv,examples.lean" \
    "llvm.exe,lcb.exe" \
    first_native_comp || return $?
  echo "FAIL native-compilation-failure: persisted and replayed native build failure bundle for seed $seed index 0"
  return "$PLANTED_FAIL_RC"
}

control_native_setup_failure() {
  local expected_diag="FAIL bir-0 [lcb_eval]: missing evaluator observation"
  first_native_setup() {
    local rc=0
    require_bounded "${driver[@]}" control-plant-bir native-setup "$seed" 1 "$work/bir" "$target" \
      --control SPEC_DIFF_PLANTED_CONTROL
    native_one bir "$seed" 0 "$work/bir/p0" || rc=$?
    absorb_native_result "$rc"
    [ "$fail" -eq 0 ] || exit 1
    exit "$rc"
  }
  verify_failure_bundle_and_replay native-setup bir "$seed" 0 native_setup \
    "$expected_diag" "" \
    "program.lean.txt,bir.txt,llvm.ll,lcb_source.txt,lcb_job.txt,lcb_rt.ssa,lcb_body.ssa,lcb_body.s,manifest-row.tsv,examples.lean" \
    "lcb_eval.status,llvm.exe,lcb.exe" \
    first_native_setup || return $?
  echo "FAIL native-setup-failure: persisted and replayed native setup failure bundle for seed $seed index 0"
  return "$PLANTED_FAIL_RC"
}

control_observation_failure() {
  local index built="$work/control-drop-original"
  index=$(first_outcome_index "$work/bir/summary.tsv" trap) ||
    { echo "control setup: no generated trap for observation control" >&2; return 1; }
  [ -f "$built/llvm.exe" ] && [ -f "$built/lcb.exe" ] ||
    { echo "control setup: missing prebuilt control-drop-original binaries" >&2; return 1; }
  local expected_diag="FAIL bir seed $seed index $index [llvm]: observation failed"
  local extra_diag="FAIL planted observer setup"
  first_obs_fail() {
    local rc=0
    mkdir -p "$work/bir"
    cp -R "$built" "$work/bir/p$index"
    rm -f "$work/bir/p$index/llvm.status" "$work/bir/p$index/llvm.stderr" "$work/bir/p$index/llvm.stdout" \
          "$work/bir/p$index/lcb.status" "$work/bir/p$index/lcb.stderr" "$work/bir/p$index/lcb.stdout" \
          "$work/bir/p$index/lcb_eval.judged" "$work/bir/p$index/failure-kinds.txt" \
          "$work/bir/p$index/failure-record.tsv" "$work/bir/p$index/diagnostics.txt" \
          "$work/bir/p$index/backends" "$work/bir/p$index/bundle-reported"
    native_build() { return 0; }
    echo observation >"$work/bir/p$index/control-replay"
    native_one bir "$seed" "$index" "$work/bir/p$index" || rc=$?
    absorb_native_result "$rc"
    [ "$fail" -eq 0 ] || exit 1
    exit "$rc"
  }
  verify_failure_bundle_and_replay observation bir "$seed" "$index" observation \
    "$expected_diag" "$extra_diag" \
    "program.lean.txt,bir.txt,llvm.ll,lcb_source.txt,lcb_job.txt,lcb_rt.ssa,lcb_body.ssa,lcb_body.s,lcb_eval.status,llvm.exe,lcb.exe,llvm.obs.log,lcb.obs.log,manifest-row.tsv,examples.lean" \
    "observed-llvm/status,observed-lcb/status" \
    first_obs_fail || return $?
  echo "FAIL observation-failure: persisted and replayed observation failure bundle for seed $seed index $index"
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
  run_replay
fi

require_bounded python3 -I "$root/scripts/checks/compiler/test_spec_diff_orchestration.py"
require_bounded python3 -I "$root/scripts/checks/lib/test_bounded_exec.py"
require_bounded bash "$root/scripts/checks/import_layering.sh"
require_bounded "${driver[@]}" gen-spec "$seed" "$candidate_count" "$work/spec"
require_bounded "${driver[@]}" gen-bir "$seed" "$candidate_count" "$work/bir" "$target"
collect_available
print_counts "spec leg" "$work/spec/summary.tsv"
print_counts "BIR leg" "$work/bir/summary.tsv"
check_generation "spec leg" "$work/spec/summary.tsv"
check_generation "BIR leg" "$work/bir/summary.tsv"

control_determinism
run_control selective-refusal \
  '^FAIL selective-refusal: admitted candidate refused despite intact aggregate coverage$' \
  control_selective_refusal
run_control assembly-failure \
  "^FAIL assembly-failure: persisted and replayed LCB assembly failure bundle for seed .* index 0$" \
  control_assembly_failure
run_control native-compilation-failure \
  "^FAIL native-compilation-failure: persisted and replayed native build failure bundle for seed .* index 0$" \
  control_native_compilation_failure
run_control native-setup-failure \
  "^FAIL native-setup-failure: persisted and replayed native setup failure bundle for seed .* index 0$" \
  control_native_setup_failure
run_control drop-overflow-llvm \
  '^FAIL drop-overflow-llvm: planted LLVM trap became exact exit [0-9]+; LCB retained trap$' \
  control_drop_overflow
run_control observation-failure \
  "^FAIL observation-failure: persisted and replayed observation failure bundle for seed .* index [0-9]+$" \
  control_observation_failure
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
  "^FAIL report-mismatch: persisted distinct bir and spec bundles for Replay-Seed:A's index 0 and validated manifest rows$" \
  control_report_mismatch

bir_accepted=$(summary_value "$work/bir/summary.tsv" accepted)
while IFS=$'\t' read -r tag index outcome production; do
  [ "$tag" = program ] || continue
  d="$work/bir/p$index"
  if [ "$(remaining)" -lt "$min_candidate_secs" ]; then
    deadline_stopped=1
    eval_rc=0
    judge_evaluator bir "$seed" "$index" "$d" || eval_rc=$?
    [ "$eval_rc" -eq 0 ] || [ "$eval_rc" -eq 3 ] || fail=1
    continue
  fi
  rc=0
  native_one bir "$seed" "$index" "$d" || rc=$?
  absorb_native_result "$rc"
  if [ "$native_complete" -eq 0 ]; then
    continue
  fi
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
  d="$work/spec/p$index"
  if [ ! -f "$d/native-ready" ]; then
    record_candidate_failure spec "$seed" "$index" "$d" native_setup native-ready \
      "FAIL spec leg: compiled program lacks native-ready marker"
    fail=1
    continue
  fi
  if [ "$(remaining)" -lt "$min_candidate_secs" ]; then
    deadline_stopped=1
    eval_rc=0
    judge_evaluator spec "$seed" "$index" "$d" || eval_rc=$?
    [ "$eval_rc" -eq 0 ] || [ "$eval_rc" -eq 3 ] || fail=1
    continue
  fi
  rc=0
  native_one spec "$seed" "$index" "$d" || rc=$?
  absorb_native_result "$rc"
  if [ "$native_complete" -eq 0 ]; then
    continue
  fi
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
