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
# Controls: wrong exit and trap expectations (both site and kind) must fail in the emitter; a
# missing row and a nonexistent program must fail opposite inventory directions; a wrong source
# refusal must fail in the source runner. Child controls are emit-only. A body that never returns must
# be observed as incomplete on both backends; a body returning a word that encodes nothing must reach
# the runtime's refusal; and the comparison itself must reject a missing or extra newline, a wrong
# status with the right stderr, and stray stdout. Run inside the nix devshell, from the repo root.
set -euo pipefail
# shellcheck source=scripts/checks/lib/slice0_common.sh
source scripts/checks/lib/slice0_common.sh
# shellcheck source=scripts/checks/lib/has_match.sh
source scripts/checks/lib/has_match.sh

# The emit-only manifest controls exercise only manifest validation.
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

# Check a diagnostic through the public CLI, including its process status.
check_cli_diagnostic() {
  local label=$1
  local output=$2
  local expectation=$3
  shift 3
  local status code message
  local rc=0
  read -r _ status code message <<<"$expectation"
  bounded "$native_secs" "$@" >"$output.stdout" 2>"$output.stderr" || rc=$?
  count=$((count + 1))
  local -a expected=(has_match -E "^$code: .+$")
  if [ -n "$message" ]; then
    expected=(cmp -s "$output.expected" -)
    printf '%s: %s\n' "$code" "$message" >"$output.expected"
  fi
  if [ "$rc" = "$status" ] && [ ! -s "$output.stdout" ] &&
      "${expected[@]}" <"$output.stderr"; then
    echo "ok   $label: status $rc ($code)"
  else
    echo "FAIL $label: expected status $status and $code, got status $rc"
    cat "$output.stderr"
    fail=1
  fi
}

cli_bin="$root/.lake/build/bin/concrete"
cli_project=$(cd "$(dirname "$test_manifest")/.." && pwd)
project_cli=(bash -c 'cd "$1" && shift && exec "$@"' _ "$cli_project" "$cli_bin")

# The backend-specific expectation exercises the public driver and its native build policy.
check_backend_limit() {
  local path=${1#con:}
  local d=$2
  local expect=$3
  local status diagnostic
  local project
  local -a cli
  project=$cli_project
  if [ "$project" != "$root" ]; then
    mkdir -p "$d/project/$(dirname "$path")"
    cp -- "$project/$path" "$d/project/$path"
    ln -s "$root/scripts" "$d/project/scripts"
    ln -s "$root/.lake" "$d/project/.lake"
    project="$d/project"
  fi
  cli=(bash -c 'cd "$1" && shift && exec "$@"' _ "$project" "$cli_bin")
  read -r _ _ status _ diagnostic <<<"$expect"
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

  check_cli_diagnostic "$path [lcb]" "$d/lcb" "$diagnostic" "${cli[@]}" \
    build "$path" -o "$d/lcb" --backend lcb
  if [ -e "$d/lcb" ]; then
    echo "FAIL $path [lcb]: incomplete build created an output directory"
    fail=1
  fi
}

rows=$(manifest_rows)

wrong_expectation() { awk -F '\t' -v OFS='\t' '$1 == "answer" {$3 = "exit 41"} {print}' "$1" >"$1.new"; mv "$1.new" "$1"; }
wrong_trap_expectation() { awk -F '\t' -v OFS='\t' '$1 == "overflow" {$3 = "trap arithmeticOverflow site 1"} {print}' "$1" >"$1.new"; mv "$1.new" "$1"; }
wrong_trap_kind() { awk -F '\t' -v OFS='\t' '$1 == "overflow" {$3 = "trap divisionByZero site 0"} {print}' "$1" >"$1.new"; mv "$1.new" "$1"; }
missing_row() { awk -F '\t' '$1 != "answer"' "$1" >"$1.new"; mv "$1.new" "$1"; }
nonexistent_program() { printf 'no_such_program\tbir:no_such_program\texit 0\tslice0\n' >>"$1"; }
wrong_source_refusal() {
  mkdir -p "$(dirname "$1")/slice0"
  printf 'fn main() -> u8 { return 42 }\n' >"$(dirname "$1")/slice0/wrong_refusal.con"
  awk -F '\t' '{print} $1 == "answer" {print "wrong_source_refusal\tcon:tests/slice0/wrong_refusal.con\trefuse 1 CORE-CHECK\tslice0"}' "$1" >"$1.new"
  mv "$1.new" "$1"
}
manifest_control() { # manifest_control LABEL EXPECTED-MESSAGE MUTATION-FUNCTION
  local label=$1
  local expected=$2
  local mutation=$3
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
  CONCRETE_TEST_MANIFEST="$copy" CONCRETE_TEST_MANIFEST_SKIP_CONTROLS=1 CONCRETE_TEST_EMIT_ONLY=1 \
    bash "$root/scripts/checks/compiler/tests.sh" >"$work/ctl/$label.log" 2>&1 || rc=$?
  if [ "$rc" -ne 0 ] && has_match -F "$expected" <"$work/ctl/$label.log"; then
    echo "ok   manifest control $label: refused for the planted reason"
  else
    echo "FAIL manifest control $label: not refused as '$expected'" >&2
    cat "$work/ctl/$label.log" >&2
    fail=1
  fi
}
mkdir -p "$work/ctl"
if [ "${CONCRETE_TEST_MANIFEST_SKIP_CONTROLS:-0}" != 1 ]; then
  manifest_control wrong-expectation "answer: expectation mismatch: BIR run gives" wrong_expectation
  manifest_control wrong-trap-expectation "overflow: expectation mismatch: BIR run gives" wrong_trap_expectation
  manifest_control wrong-trap-kind "overflow: expectation mismatch: trap site 0 has kind ConcreteSpec.Trap.arithmeticOverflow, expected divisionByZero" wrong_trap_kind
  manifest_control missing-row "program without a manifest row: answer" missing_row
  manifest_control nonexistent-program "manifest row names no program: no_such_program" nonexistent_program
  manifest_control wrong-source-refusal "expected refusal CORE-CHECK, got SYNTAX" wrong_source_refusal
fi

if [ "${CONCRETE_TEST_EMIT_ONLY:-0}" != 1 ]; then
  build "concrete driver" "$work/cli-build.log" bounded 1800 lake build concrete
fi
while IFS=$'\t' read -r name program expect; do
  d="$work/ex/$name"
  if [[ "$program" == cli:* ]]; then
    if [ "${CONCRETE_TEST_EMIT_ONLY:-0}" != 1 ]; then
      mkdir -p "$d"
      check_cli_diagnostic "$name" "$d/cli" "$expect" "$cli_bin" \
        build tests/slice0/answer.con -o "$d/out" --backend invalid
      examples=$((examples + 1))
    fi
    continue
  fi
  if [[ "$expect" == "llvm "* ]]; then
    if [ "${CONCRETE_TEST_EMIT_ONLY:-0}" != 1 ]; then
      check_backend_limit "$program" "$d" "$expect"
      examples=$((examples + 1))
    fi
    continue
  fi
  emit_slice0_row "$test_manifest" "$program" "$d" "$target" "$expect"
  # Emit-only controls visit every row without a native build.
  if [ "${CONCRETE_TEST_EMIT_ONLY:-0}" = 1 ]; then continue; fi
  if [[ "$expect" == "refuse "* ]]; then
    mkdir -p "$d"
    # The emitter checks optional exact stage messages, including syntax message formatting.
    read -r _ status code _ <<<"$expect"
    check_cli_diagnostic "$name" "$d/cli" "refuse $status $code" "${project_cli[@]}" \
      build "${program#con:}" -o "$d/out"
    if [ -e "$d/out" ]; then
      echo "FAIL $name: refused build created an output directory"
      fail=1
    fi
  else
    check "$name" "$d" 10 20
  fi
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
         "incomplete|incomplete||"; do
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
