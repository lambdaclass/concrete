#!/usr/bin/env bash
# Seeded spec/compiler differential (Concrete/README.md gates 5 and 6). Run from the repo root in
# nix develop. The seed is SPEC_DIFF_SEED or the commit SHA. SPEC_DIFF_SECS (default 600) bounds
# the shared build, generation, controls and native comparisons; SPEC_DIFF_N (default
# 300) is the candidate count per leg, and SPEC_DIFF_MIN_JUDGED (default 8) is the minimum number of
# BIR programs compared on all three legs. Too few comparisons by the deadline is INCOMPLETE (3),
# never a pass. Both generators require at least 50% accepted candidates. All native executables
# use the shared build/observe/judge functions; the in-process LCB observation uses the same judge.
#
# Replay one BIR candidate, without coverage or minimum-count checks:
#   SPEC_DIFF_REPLAY='SEED:INDEX' bash scripts/checks/compiler/spec_diff.sh
# On a mismatch the gate prints the seed/index, generated term, exact manifest row, Examples
# definition/list entry, and this replay command. To save the regression: put the printed definition
# in Concrete.BIR.Examples, put its tuple in Examples.all, add the row to tests/manifest.tsv, then
# replay the seed:index and run the manifest Tests job. No generated output is committed directly.
#
# Planted controls must fail for their stated reason. Mutations exist only in the Lean driver's
# explicit control-emit command and require --control SPEC_DIFF_PLANTED_CONTROL; normal commands
# reject them. A control that builds incorrectly or fails without its expected FAIL line is a gate
# failure. Determinism controls compare program.lean.txt fingerprints, not native artifacts.
set -euo pipefail

seed=${SPEC_DIFF_SEED:-$(git rev-parse HEAD)}
candidate_count=${SPEC_DIFF_N:-300}
budget=${SPEC_DIFF_SECS:-600}
minimum_judged=${SPEC_DIFF_MIN_JUDGED:-8}
replay=${SPEC_DIFF_REPLAY:-}
for value in "$candidate_count" "$budget" "$minimum_judged"; do
  [[ "$value" =~ ^[0-9]+$ ]] && [ "$value" -gt 0 ] ||
    { echo "spec_diff: N, SECS and MIN_JUDGED must be positive integers" >&2; exit 2; }
done
printf 'spec_diff seed: %s\n' "$seed"
printf 'replay: SPEC_DIFF_REPLAY=%s:INDEX bash scripts/checks/compiler/spec_diff.sh\n' "$seed"
start=$(date +%s)
deadline=$((start + budget))
remaining() { echo $((deadline - $(date +%s))); }
export SLICE0_BUILD_SECS=$budget

# shellcheck source=scripts/checks/lib/slice0_common.sh
source scripts/checks/lib/slice0_common.sh
driver=(env "SPEC_DIFF_TARGET=$target" lake env lean --run scripts/checks/compiler/spec_diff.lean)
fail=0
bir_judged=0
spec_judged=0

summary_value() { # summary_value FILE KEY [FIELD]
  awk -F '\t' -v key="$2" -v field="${3:-2}" '$1 == key {print $field; exit}' "$1"
}

print_counts() { # print_counts LABEL SUMMARY
  echo "$1 outcome/construct counts:"
  awk -F '\t' '$1 == "outcome" || $1 == "construct" {printf "  %s %s %s\n", $1, $2, $3}' "$2"
  printf '  accepted %s, rejected %s, attempted %s\n' \
    "$(summary_value "$2" accepted)" "$(summary_value "$2" rejected)" "$(summary_value "$2" attempted)"
}

check_generation() { # check_generation LABEL SUMMARY REQUIRED-CONSTRUCTS...
  local label=$1 summary=$2 accepted rejected attempted
  shift 2
  accepted=$(summary_value "$summary" accepted)
  rejected=$(summary_value "$summary" rejected)
  attempted=$(summary_value "$summary" attempted)
  [ -n "$accepted" ] && [ -n "$rejected" ] && [ -n "$attempted" ] ||
    { echo "FAIL $label: missing generation counts" >&2; fail=1; return; }
  if [ "$accepted" -eq 0 ] || [ $((accepted * 2)) -lt "$attempted" ]; then
    echo "FAIL $label: yield $accepted/$attempted is below the 50% floor" >&2
    fail=1
  fi
  local construct count
  for construct in "$@"; do
    count=$(awk -F '\t' -v c="$construct" '$1 == "construct" && $2 == c {print $3}' "$summary")
    if [ -z "$count" ] || [ "$count" -eq 0 ]; then
      echo "FAIL $label: construct $construct has zero coverage" >&2
      fail=1
    fi
  done
}

report_mismatch() { # report_mismatch LEG INDEX DIR BACKEND
  local leg=$1 index=$2 d=$3 backend=$4 name term
  echo "FAIL $leg seed $seed index $index [$backend]: observation mismatch" >&2
  echo "  expected status $(cat "$d/status"), observed $(cat "$d/$backend.status")" >&2
  echo "  expected stderr bytes $(wc -c <"$d/stderr"), observed $(wc -c <"$d/$backend.stderr"), stdout $(wc -c <"$d/$backend.stdout")" >&2
  echo "  generated program (program.lean.txt):" >&2
  cat "$d/program.lean.txt" >&2
  if [ "$leg" = spec ]; then term="$d/bir.txt"; else term="$d/program.lean.txt"; fi
  if [ -f "$d/regression-name.txt" ]; then
    name=$(cat "$d/regression-name.txt")
    echo "  add exactly this tests/manifest.tsv row (tab-separated):" >&2
    cat "$d/manifest-row.txt" >&2
    echo "  add to Concrete.BIR.Examples:" >&2
    printf 'def %s : Concrete.BIR.Program :=\n' "$name" >&2
    cat "$term" >&2
    printf '\n-- and add to Examples.all:\n("%s", %s),\n' "$name" "$name" >&2
  fi
  echo "  replay: SPEC_DIFF_REPLAY='$seed:$index' bash scripts/checks/compiler/spec_diff.sh" >&2
}

native_one() { # native_one LEG INDEX DIR: return 1 after reporting any observation mismatch
  local leg=$1 index=$2 d=$3 name=$1-$2 backend bad=0 secs
  build "$name" "$d/build.log" bounded "$(remaining)" bash "$root/scripts/checks/lib/slice0_native.sh" "$d" "$lcb"
  for backend in llvm lcb; do
    secs=$(remaining); [ "$secs" -gt 10 ] && secs=10
    [ "$secs" -gt 0 ] || { echo "spec_diff: deadline before $name [$backend]" >&2; return 2; }
    observe "$d/$backend.exe" "$d/$backend" "$secs" "$secs"
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

run_control() { # run_control LABEL EXPECTED-FAIL-LINE FUNCTION: an expected failure is a pass
  local label=$1 expected=$2 fn=$3 log="$work/ctl-$label.log" rc=0
  ( "$fn" ) >"$log" 2>&1 || rc=$?
  if [ "$rc" -ne 0 ] && grep -qF "$expected" "$log"; then
    echo "ok   control $label: failed for the planted reason"
  else
    echo "FAIL control $label: did not fail as '$expected' (rc $rc)" >&2
    cat "$log" >&2
    fail=1
  fi
}

first_outcome_index() { # first_outcome_index SUMMARY exit|trap
  awk -F '\t' -v outcome="$2" '$1 == "program" && $3 == outcome {print $2; exit}' "$1"
}

control_drop_overflow() {
  local index d="$work/control-drop-overflow"
  index=$(first_outcome_index "$work/bir/summary.tsv" trap)
  [ -n "$index" ] || { echo "control setup: no generated trap" >&2; return 0; }
  bounded 120 "${driver[@]}" control-emit --mutate drop-overflow-llvm "$seed" "$index" "$d" "$target" \
    --control SPEC_DIFF_PLANTED_CONTROL
  build control-drop-overflow "$d/build.log" bounded 300 bash "$root/scripts/checks/lib/slice0_native.sh" "$d" "$lcb"
  observe "$d/llvm.exe" "$d/llvm" 10 20
  if judge "$d" "$d/llvm"; then
    echo "control setup: drop-overflow-llvm did not change the observation" >&2
    return 0
  fi
  echo "FAIL drop-overflow-llvm: planted LLVM overflow removal changed status/stderr"
  return 1
}

control_lcb_off_by_one() {
  local index d="$work/control-lcb-off-by-one" native_bad=1 eval_bad=1
  index=$(first_outcome_index "$work/bir/summary.tsv" exit)
  [ -n "$index" ] || { echo "control setup: no generated exit" >&2; return 0; }
  bounded 120 "${driver[@]}" control-emit --mutate lcb-off-by-one "$seed" "$index" "$d" "$target" \
    --control SPEC_DIFF_PLANTED_CONTROL
  build control-lcb-off-by-one "$d/build.log" bounded 300 bash "$root/scripts/checks/lib/slice0_native.sh" "$d" "$lcb"
  observe "$d/lcb.exe" "$d/lcb" 10 20
  judge "$d" "$d/lcb" && native_bad=0
  judge "$d" "$d/lcb_eval" && eval_bad=0
  if [ "$native_bad" -eq 1 ] && [ "$eval_bad" -eq 1 ]; then
    echo "FAIL lcb-off-by-one: native LCB and in-process evaluator both changed exit status"
    return 1
  fi
  echo "control setup: off-by-one escaped native=$native_bad evaluator=$eval_bad (1 means caught)" >&2
  return 0
}

control_spec_oracle() {
  local d="$work/control-spec-oracle"
  bounded 120 "${driver[@]}" control-spec-oracle "$d" "$target" --control SPEC_DIFF_PLANTED_CONTROL
  build control-spec-oracle "$d/build.log" bounded 300 bash "$root/scripts/checks/lib/slice0_native.sh" "$d" "$lcb"
  observe "$d/llvm.exe" "$d/llvm" 10 20
  if ! judge "$d/spec-expected" "$d/llvm"; then
    echo "control setup: stand-in does not match the spec's actual outcome" >&2
    return 0
  fi
  if judge "$d/wrong-expected" "$d/llvm"; then
    echo "control setup: judge accepted planted expected exit 43" >&2
    return 0
  fi
  echo "FAIL spec-oracle: judge rejected planted exit 43 against spec exit 42"
  return 1
}

term_fingerprint() { # term_fingerprint DIRECTORY OUTPUT: paths and exact bytes of every Lean term
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
      bounded 120 "${driver[@]}" gen-spec determinism-seed 16 "$seed_a"
      bounded 120 "${driver[@]}" gen-spec determinism-seed 16 "$seed_b"
      bounded 120 "${driver[@]}" gen-spec determinism-other 16 "$seed_c"
    else
      bounded 120 "${driver[@]}" gen-bir determinism-seed 16 "$seed_a" "$target"
      bounded 120 "${driver[@]}" gen-bir determinism-seed 16 "$seed_b" "$target"
      bounded 120 "${driver[@]}" gen-bir determinism-other 16 "$seed_c" "$target"
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
  case "$replay" in
    *:*) replay_seed=${replay%%:*}; replay_index=${replay#*:} ;;
    *) echo "spec_diff: SPEC_DIFF_REPLAY must be SEED:INDEX" >&2; exit 2 ;;
  esac
  [[ "$replay_index" =~ ^[0-9]+$ ]] || { echo "spec_diff: replay index is not a natural" >&2; exit 2; }
  d="$work/replay"
  bounded "$(remaining)" "${driver[@]}" replay "$replay_seed" "$replay_index" "$d" "$target"
  native_one bir "$replay_index" "$d" || fail=1
  bir_judged=1
  echo "spec_diff replay: $replay_seed:$replay_index judged on LLVM, LCB and LCB evaluator"
  exit "$fail"
fi

left=$(remaining)
[ "$left" -gt 0 ] || { echo "INCOMPLETE: no time for spec generation" >&2; exit 3; }
bounded "$left" "${driver[@]}" gen-spec "$seed" "$candidate_count" "$work/spec"
left=$(remaining)
[ "$left" -gt 0 ] || { echo "INCOMPLETE: no time for BIR generation" >&2; exit 3; }
bounded "$left" "${driver[@]}" gen-bir "$seed" "$candidate_count" "$work/bir" "$target"
print_counts "spec leg" "$work/spec/summary.tsv"
print_counts "BIR leg" "$work/bir/summary.tsv"
check_generation "spec leg" "$work/spec/summary.tsv" \
  let_literal let_copy add_literal add_variable return_literal return_variable
check_generation "BIR leg" "$work/bir/summary.tsv" checked wrapped widening two_sites chain
for outcome in exit trap; do
  count=$(awk -F '\t' -v o="$outcome" '$1 == "outcome" && $2 == o {print $3}' "$work/bir/summary.tsv")
  if [ -z "$count" ] || [ "$count" -eq 0 ]; then
    echo "FAIL BIR leg: vacuity guard: zero ${outcome}s" >&2
    fail=1
  fi
done

control_determinism
run_control drop-overflow-llvm \
  "FAIL drop-overflow-llvm: planted LLVM overflow removal changed status/stderr" control_drop_overflow
run_control lcb-off-by-one \
  "FAIL lcb-off-by-one: native LCB and in-process evaluator both changed exit status" control_lcb_off_by_one
run_control spec-oracle \
  "FAIL spec-oracle: judge rejected planted exit 43 against spec exit 42" control_spec_oracle

while IFS=$'\t' read -r tag index outcome production; do
  [ "$tag" = program ] || continue
  [ "$(remaining)" -gt 0 ] || break
  d="$work/bir/p$index"
  rc=0
  native_one bir "$index" "$d" || rc=$?
  [ "$rc" -ne 2 ] || { echo "INCOMPLETE: deadline during BIR comparison $index" >&2; exit 3; }
  [ "$rc" -eq 0 ] || fail=1
  bir_judged=$((bir_judged + 1))
done < <(awk -F '\t' '$1 == "program"' "$work/bir/summary.tsv")

spec_compiled=$(summary_value "$work/spec/summary.tsv" compiled)
while IFS=$'\t' read -r tag index outcome compiled; do
  [ "$tag" = program ] || continue
  [ "$compiled" = true ] || continue
  [ "$(remaining)" -gt 0 ] || break
  d="$work/spec/p$index"
  [ -f "$d/native-ready" ] || { echo "FAIL spec leg: compiled program lacks native-ready marker" >&2; fail=1; continue; }
  rc=0
  native_one spec "$index" "$d" || rc=$?
  [ "$rc" -ne 2 ] || { echo "INCOMPLETE: deadline during spec comparison $index" >&2; exit 3; }
  [ "$rc" -eq 0 ] || fail=1
  spec_judged=$((spec_judged + 1))
done < <(awk -F '\t' '$1 == "program"' "$work/spec/summary.tsv")

if [ "$bir_judged" -lt "$minimum_judged" ]; then
  echo "INCOMPLETE: judged $bir_judged BIR programs, minimum $minimum_judged, before the deadline" >&2
  exit 3
fi
if [ "$spec_compiled" -gt 0 ] && [ "$spec_judged" -eq 0 ]; then
  echo "INCOMPLETE: $spec_compiled spec programs compiled but none was judged" >&2
  exit 3
fi
echo "spec_diff: $bir_judged BIR programs judged on 3 legs; $spec_judged spec programs judged; seed $seed"
exit "$fail"
