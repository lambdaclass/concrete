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
  if [ "$rc" = 1 ] && [ ! -s "$d/lcb.stdout" ] && [ ! -e "$d/lcb" ]; then
    echo "ok   $path [lcb]: $(cat "$d/lcb.stderr")"
  else
    echo "FAIL $path [lcb]: expected LCB CLI refusal without published output, got status $rc"
    cat "$d/lcb.stderr"
    fail=1
  fi
}

rows=$(manifest_rows)

mutate_expectation() {
  awk -F '\t' -v OFS='\t' -v id="$2" -v replacement="$3" '
    $1 == id {$3 = replacement} {print}
  ' "$1" >"$1.new"
  mv "$1.new" "$1"
}
wrong_expectation() { mutate_expectation "$1" answer 'exit 41 -'; }
wrong_trap_site() { mutate_expectation "$1" overflow 'trap overflow 1 0a'; }
wrong_trap_kind() { mutate_expectation "$1" overflow 'trap division_by_zero 0 0a'; }
wrong_trap_stderr() { mutate_expectation "$1" overflow 'trap overflow 0 0a'; }
missing_row() { awk -F '\t' '$1 != "answer"' "$1" >"$1.new"; mv "$1.new" "$1"; }
nonexistent_program() { printf 'no_such_program\tbir:no_such_program\texit 0 -\tslice0\n' >>"$1"; }
wrong_source_refusal() { mutate_expectation "$1" source_answer 'pending-refusal checker'; }
wrong_origin() { mutate_expectation "$1" source_after_return 'refuse unreachable_statement STMT-14@1 18:27:1:19 -'; }
wrong_related_origin() {
  mutate_expectation "$1" source_after_return 'refuse unreachable_statement STMT-14@1 28:37:1:29 anchor=18:27:1:19'
}
wrong_resource_limit() { mutate_expectation "$1" source_lcb_value_limit 'llvm exit 42 lcb resource values 489'; }

manifest_control() {
  local label=$1
  local expected_status=$2
  local mutation=$3
  local row_id=$4
  local inventory_message=${5:-}
  local selected program expect
  local dir="$work/ctl/$1"
  local copy
  local rc=0
  mkdir -p "$dir"
  cp -R "$(dirname "$test_manifest")" "$dir/tests"
  copy="$dir/tests/$(basename "$test_manifest")"
  "$mutation" "$copy"
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
  manifest_control wrong-expectation 10 wrong_expectation answer
  manifest_control wrong-trap-site 11 wrong_trap_site overflow
  manifest_control wrong-trap-kind 12 wrong_trap_kind overflow
  manifest_control wrong-trap-stderr 16 wrong_trap_stderr overflow
  manifest_control missing-row 1 missing_row "" "slice0: program without a manifest row: answer"
  manifest_control nonexistent-program 1 nonexistent_program "" "slice0: manifest row names no program: no_such_program"
  manifest_control wrong-source-refusal 13 wrong_source_refusal source_answer
  manifest_control wrong-primary-origin 14 wrong_origin source_after_return
  manifest_control wrong-related-origin 14 wrong_related_origin source_after_return
  manifest_control wrong-resource-limit 15 wrong_resource_limit source_lcb_value_limit
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

[ "$examples" -gt 0 ] || { echo "tests: no examples ran" >&2; exit 1; }
echo "tests: $examples examples, $count backend runs (controls included)"
exit "$fail"
