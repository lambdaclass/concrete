#!/usr/bin/env bash
# Manifest-driven slice-0 test gate (Concrete/README.md gates 4 and 6).
# Hand-written BIR and accepted or unchecked source go through the LLVM path (clang) and the LCB path: the
# adapted body assembled by LCB in process (no text on the proved path), linked with the runtime
# entry, which LCB's command line assembles from QBE text. The manifest's bir: inventory must equal
# the emitter's list in both directions. CONCRETE_TEST_MANIFEST selects another manifest (controls).
# Each executable runs under bounded_run; its observation is its exit status (or "incomplete"),
# the exact bytes on descriptor 2, and descriptor 1, which must be empty. All three must equal the
# manifest expectation checked by the emitter (which writes the expected status and stderr bytes).
#
# Planted controls exercise typed observation mismatches, manifest inventory and exact byte
# comparison. The two native runtime controls exercise divergence and invalid result words.
set -euo pipefail
# shellcheck source=scripts/checks/lib/slice0_common.sh
source scripts/checks/lib/slice0_common.sh
# shellcheck source=scripts/checks/lib/has_match.sh
source scripts/checks/lib/has_match.sh

# Emit-only runs validate fixture expectations without building native executables.
if [[ ${CONCRETE_TEST_EMIT_ONLY:-0} != 1 ]]; then
  bash scripts/checks/compiler/f31_census.sh
  build llvm-rendering "$work/llvm-rendering.log" bounded 120 \
    lake env lean scripts/checks/compiler/llvm_rendering.lean
  echo "ok   LLVM rendering controls"
  build llvm-mutation-controls "$work/llvm-mutation-controls.log" bounded 120 \
    python3 -B scripts/checks/compiler/llvm_mutation_controls.py
  cat "$work/llvm-mutation-controls.log"
fi

fail=0
count=0
examples=0
# check NAME DIR SECS RETRY: build both backends in DIR, run them and judge them.
check() {
  local name=$1
  local d=$2
  local backend
  build "$name" "$d/build.log" bounded "$native_secs" bash "$root/scripts/checks/lib/slice0_native.sh" "$d" "$lcb"
  for backend in llvm lcb; do
    observe "$d/$backend.exe" "$d/$backend" "$3" "$4"
    count=$((count + 1))
    if judge "$d" "$d/$backend"; then
      echo "ok   $name [$backend]: status $(cat "$d/$backend.status")"
    else
      echo "FAIL $name [$backend]: status $(cat "$d/$backend.status") (expected $(cat "$d/status"))," \
        "stderr $(od -An -c "$d/$backend.stderr" | tr -s ' ' | head -c 300)," \
        "stdout $(wc -c <"$d/$backend.stdout") bytes"
      fail=1
    fi
  done
}

# Compare the public driver's complete refusal against the typed oracle's rendering.
cli_resource_refused() {
  local dir=$1
  [ "$2" = 1 ] && [ ! -s "$dir/lcb.stdout" ] &&
    [ ! -e "$dir/lcb" ] && [ ! -L "$dir/lcb" ] &&
    cmp -s "$3" "$dir/lcb.stderr"
}

# The backend-specific expectation exercises the public driver and its native build policy.
check_backend_limit() {
  local path=${1#con:}
  local d=$2
  local expect=$3
  local status
  local project
  local -a cli
  project=$(cd "$(dirname "$test_manifest")/.." && pwd)
  if [ "$project" != "$root" ]; then
    mkdir -p "$d/project/$(dirname "$path")"
    cp -- "$project/$path" "$d/project/$path"
    ln -s "$root/scripts" "$d/project/scripts"
    ln -s "$root/.lake" "$d/project/.lake"
    project="$d/project"
  fi
  cli=(bash -c 'cd "$1" && shift && exec "$@"' _ "$project" "$root/.lake/build/bin/concrete")
  read -r _ _ status _ <<<"$expect"
  mkdir -p "$d/expected"
  printf '%s\n' "$status" >"$d/expected/status"
  : >"$d/expected/stderr"
  build "$path [llvm]" "$d/llvm-build.log" bounded "$native_secs" "${cli[@]}" \
    build "$path" -o "$d/llvm" --backend llvm
  observe "$d/llvm/llvm.exe" "$d/llvm-run" 10 20
  count=$((count + 1))
  if judge "$d/expected" "$d/llvm-run"; then
    echo "ok   $path [llvm]: status $status"
  else
    echo "FAIL $path [llvm]: expected status $status and empty streams"
    fail=1
  fi

  local rc=0
  bounded "$native_secs" "${cli[@]}" \
    build "$path" -o "$d/lcb" --backend lcb \
    >"$d/lcb.stdout" 2>"$d/lcb.stderr" || rc=$?
  if cli_resource_refused "$d" "$rc" "$d/lcb-expected.stderr"; then
    echo "ok   $path [lcb]: $(cat "$d/lcb.stderr")"
  else
    echo "FAIL $path [lcb]: expected LCB CLI refusal without published output, got status $rc"
    cat "$d/lcb.stderr"
    fail=1
  fi
  if [ "${CONCRETE_TEST_MANIFEST_SKIP_CONTROLS:-0}" != 1 ]; then
    local control="$d/cli-wrong-refusal"
    mkdir -p "$control"
    rc=0
    bounded "$native_secs" "${cli[@]}" build "./$path" -o "$control/lcb" --backend lcb \
      >"$control/lcb.stdout" 2>"$control/lcb.stderr" || rc=$?
    if [ "$rc" = 1 ] && [ ! -s "$control/lcb.stdout" ] && [ ! -e "$control/lcb" ] &&
        [ -s "$control/lcb.stderr" ] &&
        ! cli_resource_refused "$control" "$rc" "$d/lcb-expected.stderr"; then
      echo "ok   CLI control: unrelated refusal rejected"
    else
      echo "FAIL CLI control: expected rejection of an unrelated exit-1 refusal"
      fail=1
    fi
  fi
}

rows=$(manifest_rows)

mutate_field() {
  awk -F '\t' -v OFS='\t' -v id="$2" -v index_to_change="$3" -v value="$4" '
    $1 == id {
      n = split($3, fields, " ")
      fields[index_to_change] = value
      $3 = fields[1]
      for (i = 2; i <= n; i++) $3 = $3 " " fields[i]
    }
    {print}
  ' "$1" >"$1.new"
  mv "$1.new" "$1"
}
missing_row() { awk -F '\t' '$1 != "answer"' "$1" >"$1.new"; mv "$1.new" "$1"; }
nonexistent_program() { printf 'no_such_program\tbir:no_such_program\texit 0 -\tslice0\n' >>"$1"; }

manifest_control() {
  local label=$1
  local expected_status=$2
  local mutation=$3
  local row_id=$4
  local inventory_message=${5:-}
  shift 5
  local selected program expect
  local dir="$work/ctl/$label"
  local copy
  local rc=0
  mkdir -p "$dir"
  cp -R "$(dirname "$test_manifest")" "$dir/tests"
  copy="$dir/tests/$(basename "$test_manifest")"
  "$mutation" "$copy" "$row_id" "$@"
  if cmp -s "$test_manifest" "$copy"; then
    echo "FAIL manifest control $label: mutation changed nothing" >&2; fail=1; return
  fi
  if [ -n "$inventory_message" ]; then
    (test_manifest=$copy; manifest_rows) >"$work/ctl/$label.log" 2>&1 || rc=$?
  else
    selected=$(bounded 30 python3 -I "$root/scripts/checks/lib/test_manifest.py" rows "$copy" |
      awk -F '\t' -v id="$row_id" '$1 == id') || { fail=1; return; }
    if [ -z "$selected" ]; then
      echo "FAIL manifest control $label: missing mutated row $row_id" >&2
      fail=1
      return
    fi
    IFS=$'\t' read -r _ program expect <<<"$selected"
    emit_slice0_row "$copy" "$program" "$dir/out" "$target" "$expect" \
      >"$work/ctl/$label.log" 2>&1 || rc=$?
  fi
  if [ "$rc" = "$expected_status" ] &&
      { [ -z "$inventory_message" ] || has_match -Fx "$inventory_message" <"$work/ctl/$label.log"; }; then
    echo "ok   manifest control $label: refused for the planted reason"
  else
    echo "FAIL manifest control $label: expected status $expected_status, got $rc" >&2
    cat "$work/ctl/$label.log" >&2
    fail=1
  fi
}
mkdir -p "$work/ctl"
if [ "${CONCRETE_TEST_MANIFEST_SKIP_CONTROLS:-0}" != 1 ]; then
  manifest_control wrong-expectation 10 mutate_field answer "" 2 41
  manifest_control wrong-bir-exit-stderr 16 mutate_field answer "" 3 0a
  manifest_control wrong-source-exit-stderr 16 mutate_field source_answer "" 3 0a
  manifest_control wrong-source-exit-status 10 mutate_field source_answer "" 2 41
  for id in overflow source_trap; do
    manifest_control "$id-wrong-trap-site" 11 mutate_field "$id" "" 3 2
    manifest_control "$id-wrong-trap-kind" 12 mutate_field "$id" "" 2 division_by_zero
    manifest_control "$id-wrong-trap-stderr" 16 mutate_field "$id" "" 4 0a
  done
  manifest_control missing-row 1 missing_row "" "slice0: program without a manifest row: answer"
  manifest_control nonexistent-program 1 nonexistent_program "" "slice0: manifest row names no program: no_such_program"
  manifest_control wrong-source-refusal 13 mutate_field source_syntax "" 2 checker
  manifest_control wrong-rule-id 13 mutate_field source_after_return "" 3 STMT-15@1
  manifest_control wrong-rule-revision 13 mutate_field source_after_return "" 3 STMT-14@2
  manifest_control wrong-primary-origin 14 mutate_field source_after_return "" 4 18:27:1:19
  manifest_control wrong-related-origin 14 mutate_field source_after_return "" 5 anchor=18:27:1:19
  manifest_control wrong-resource-name 15 mutate_field source_lcb_value_limit "" 6 blocks
  manifest_control wrong-resource-limit 15 mutate_field source_lcb_value_limit "" 7 489

  rc=0
  fixture_dir=$(dirname "$test_manifest")
  "${emitter[@]}" control-wrong-compiler-origin tests/slice0/after_return.con \
    "$fixture_dir/slice0/after_return.con" || rc=$?
  if [ "$rc" = 18 ]; then
    echo "ok   compiler control: wrong production origin rejected"
  else
    echo "FAIL compiler control: expected wrong production origin status 18, got $rc"
    fail=1
  fi
  rc=0
  "${emitter[@]}" control-unsupported-lcb-profile || rc=$?
  if [ "$rc" = 17 ]; then
    echo "ok   unsupported LCB profile is distinct from a resource mismatch"
  else
    echo "FAIL unsupported LCB profile: expected status 17, got $rc"
    fail=1
  fi
  rc=0
  "${emitter[@]}" emit answer "$work/ctl/unsupported" unavailable-target exit 42 - || rc=$?
  if [ "$rc" = 17 ]; then
    echo "ok   unsupported target is distinct from schema errors and observation mismatches"
  else
    echo "FAIL unsupported target: expected status 17, got $rc"
    fail=1
  fi
fi

if [[ "$rows" == *$'\t'"llvm exit "* ]] && [ "${CONCRETE_TEST_EMIT_ONLY:-0}" != 1 ]; then
  build "concrete driver" "$work/cli-build.log" bounded 1800 lake build concrete
fi
while IFS=$'\t' read -r name program expect; do
  d="$work/ex/$name"
  emit_slice0_row "$test_manifest" "$program" "$d" "$target" "$expect"
  if [ "${CONCRETE_TEST_EMIT_ONLY:-0}" = 1 ]; then continue; fi
  case "$expect" in
    "refuse "*|"pending-refusal "*) continue ;;
    "llvm "*)
      check_backend_limit "$program" "$d" "$expect"
      examples=$((examples + 1))
      continue
      ;;
  esac
  check "$name" "$d" 10 20
  examples=$((examples + 1))
done <<<"$rows"
if [ "${CONCRETE_TEST_EMIT_ONLY:-0}" = 1 ]; then exit "$fail"; fi
"${emitter[@]}" loop "$work/ctl/loop" "$target"
expected_timeout check control-loop "$work/ctl/loop" 1 1
"${emitter[@]}" invalid "$work/ctl/invalid" "$target"
check control-invalid-word "$work/ctl/invalid" 10 20

# The comparison's own negative controls: each forged observation must be rejected.
e="$work/ctl/judge"; mkdir -p "$e"
echo 134 >"$e/status"; printf 'concrete: trap: x\n' >"$e/stderr"
forge() { # forge NAME STATUS STDERR-PRINTF STDOUT
  local o="$work/ctl/forged-$1"
  echo "$2" >"$o.status"; printf "$3" >"$o.stderr"; printf '%s' "$4" >"$o.stdout"
  echo "$o"
}
judge "$e" "$(forge same 134 'concrete: trap: x\n' '')" || { echo "FAIL judge rejects an exact match"; fail=1; }
for c in "no-newline|134|concrete: trap: x|" "extra-newline|134|concrete: trap: x\n\n|" \
         "wrong-status|1|concrete: trap: x\n|" "stray-stdout|134|concrete: trap: x\n|42" \
         "incomplete|incomplete||" "runner-failed|runner-failed||"; do
  IFS='|' read -r n s err out <<<"$c"
  if judge "$e" "$(forge "$n" "$s" "$err" "$out")"; then
    echo "FAIL judge accepts a forged observation: $n"; fail=1
  else
    echo "ok   judge rejects $n"
  fi
done

# Isolate the intentional watchdog failure from the gate's own bounded log.
(
  BOUNDED_LOG="$work/ctl/failed-watchdog.log"
  BOUNDED_EXEC="$work/ctl/nonexistent-watchdog.py"
  observe "$work/ctl/invalid/llvm.exe" "$work/ctl/failed-watchdog" 1 1
  [ "$(cat "$work/ctl/failed-watchdog.status")" = runner-failed ] &&
    ! judge "$work/ctl/loop" "$work/ctl/failed-watchdog"
) && echo "ok   observe distinguishes a failed watchdog from timeout" ||
  { echo "FAIL observe: watchdog failure was not classified separately"; fail=1; }

for unavailable in runner-failed unsupported-profile; do
  echo "$unavailable" >"$e/status"
  : >"$e/stderr"
  if judge "$e" "$(forge "$unavailable" "$unavailable" '' '')"; then
    echo "FAIL judge accepts $unavailable as an observation"
    fail=1
  else
    echo "ok   judge rejects $unavailable even when the expected status matches"
  fi
done

[ "$examples" -gt 0 ] || { echo "tests: no examples ran" >&2; exit 1; }
echo "tests: $examples examples, $count backend runs (controls included)"
exit "$fail"
