#!/usr/bin/env bash
# Manifest-driven test gate (PROVISIONAL, Concrete/README.md gates 4 and 6, hand-written BIR leg):
# every bir: row in tests/manifest.tsv goes through the LLVM path (clang) and the LCB path: the
# adapted body assembled by LCB in process (no text on the proved path), linked with the runtime
# entry, which LCB's command line assembles from QBE text. The manifest's bir: inventory must equal
# the emitter's list in both directions. CONCRETE_TEST_MANIFEST selects another manifest (controls).
# Each executable runs under bounded_run; its observation is its exit status (or "incomplete"),
# the exact bytes on descriptor 2, and descriptor 1, which must be empty. All three must equal the
# manifest expectation checked by the emitter (which writes the expected status and stderr bytes).
#
# Controls: wrong exit and trap expectations (both site and kind) must fail in the emitter; a
# missing row and a nonexistent program must fail opposite inventory directions; a con: row must
# fail runner dispatch with an unsupported-program diagnostic (these child runs are emit-only,
# never native). A body that never returns must
# be observed as incomplete on both backends; a body returning a word that encodes nothing must reach
# the runtime's refusal; the prepared-numbering, predecessor-grouping, reference-equality and
# stack-depth regressions (slice0_bir.lean numbering) must pass with exact refusal diagnostics; and
# the comparison itself must reject a missing or extra newline, a wrong status with the right stderr,
# and stray stdout. Run inside the nix devshell, from the repo root.
set -euo pipefail
# shellcheck source=scripts/checks/lib/slice0_common.sh
source scripts/checks/lib/slice0_common.sh
# shellcheck source=scripts/checks/lib/has_match.sh
source scripts/checks/lib/has_match.sh

fail=0
count=0
examples=0
# check NAME DIR SECS RETRY: build both backends in DIR, run them and judge them.
check() {
  local name=$1 d=$2 backend
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

rows=$(manifest_rows)

wrong_expectation() { awk -F '\t' -v OFS='\t' '$1 == "answer" {$3 = "exit 41"} {print}' "$1" >"$1.new"; mv "$1.new" "$1"; }
wrong_trap_expectation() { awk -F '\t' -v OFS='\t' '$1 == "overflow" {$3 = "trap arithmeticOverflow site 1"} {print}' "$1" >"$1.new"; mv "$1.new" "$1"; }
wrong_trap_kind() { awk -F '\t' -v OFS='\t' '$1 == "overflow" {$3 = "trap divisionByZero site 0"} {print}' "$1" >"$1.new"; mv "$1.new" "$1"; }
missing_row() { awk -F '\t' '$1 != "answer"' "$1" >"$1.new"; mv "$1.new" "$1"; }
nonexistent_program() { printf 'no_such_program\tbir:no_such_program\texit 0\tslice0\n' >>"$1"; }
unsupported_source_program() {
  mkdir -p "$(dirname "$1")/slice0"
  printf 'return 0;\n' >"$(dirname "$1")/slice0/unrun.con"
  awk -F '\t' '{print} $1 == "answer" {print "unrun_source\tcon:tests/slice0/unrun.con\texit 0\tslice0"}' "$1" >"$1.new"
  mv "$1.new" "$1"
}
manifest_control() { # manifest_control LABEL EXPECTED-MESSAGE MUTATION-FUNCTION
  local label=$1 expected=$2 mutation=$3 dir="$work/ctl/$1" copy rc=0
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
  manifest_control unsupported-source "tests: unrun_source: unsupported program con:tests/slice0/unrun.con (source runner not implemented)" unsupported_source_program
fi

while IFS=$'\t' read -r name program expect; do
  case "$program" in
    bir:*)
      d="$work/ex/$name"
      read -r -a expectation <<<"$expect"
      "${emitter[@]}" emit "${program#bir:}" "$d" "$target" "${expectation[@]}"
      # Emit-only controls cover every row and never reach a native build, even if a planted defect is
      # accidentally accepted: the control then fails because the child returns zero after the loop.
      if [ "${CONCRETE_TEST_EMIT_ONLY:-0}" = 1 ]; then continue; fi
      check "$name" "$d" 10 20
      examples=$((examples + 1))
      ;;
    con:*)
      echo "tests: $name: unsupported program $program (source runner not implemented)" >&2
      exit 1
      ;;
    *)
      echo "tests: $name: unsupported program $program" >&2
      exit 1
      ;;
  esac
done <<<"$rows"
if [ "${CONCRETE_TEST_EMIT_ONLY:-0}" = 1 ]; then exit "$fail"; fi
"${emitter[@]}" loop "$work/ctl/loop" "$target"
expected_timeout check control-loop "$work/ctl/loop" 1 1
"${emitter[@]}" invalid "$work/ctl/invalid" "$target"
check control-invalid-word "$work/ctl/invalid" 10 20
if "${emitter[@]}" numbering; then
  echo "ok   numbering regressions (sparse/duplicate/missing/phi/condbr/target/spec-eq/stack-depth)"
else
  echo "FAIL numbering regressions"
  fail=1
fi

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
