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
# Controls: a wrong expectation must fail in the emitter; a missing row and a nonexistent program
# must fail inventory (these child runs are emit-only, never native). A body that never returns must
# be observed as incomplete on both backends; a body returning a word that encodes nothing must reach
# the runtime's refusal; and the comparison itself must reject a missing or extra newline, a wrong
# status with the right stderr, and stray stdout. Run inside the nix devshell, from the repo root.
set -euo pipefail
# shellcheck source=scripts/checks/lib/slice0_common.sh
source scripts/checks/lib/slice0_common.sh

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

rows=$(manifest_rows bir)
printf '%s\n' "$rows" | awk -F '\t' '{sub(/^bir:/, "", $2); print $2}' | LC_ALL=C sort >"$work/manifest-programs"
emitter_names=$("${emitter[@]}" list | cap_out) || { echo "tests: listing emitter programs failed" >&2; exit 1; }
printf '%s\n' "$emitter_names" | LC_ALL=C sort >"$work/emitter-programs"
if ! cmp -s "$work/manifest-programs" "$work/emitter-programs"; then
  echo "tests: test manifest inventory differs from emitter" >&2
  diff -u "$work/emitter-programs" "$work/manifest-programs" >&2 || true
  exit 1
fi

wrong_expectation() { awk -F '\t' -v OFS='\t' '$1 == "answer" {$3 = "exit 41"} {print}' "$1" >"$1.new"; mv "$1.new" "$1"; }
missing_row() { awk -F '\t' '$1 != "answer"' "$1" >"$1.new"; mv "$1.new" "$1"; }
nonexistent_program() { awk -F '\t' -v OFS='\t' '$1 == "answer" {$2 = "bir:no_such_program"} {print}' "$1" >"$1.new"; mv "$1.new" "$1"; }
manifest_control() { # manifest_control LABEL EXPECTED-MESSAGE MUTATION-FUNCTION
  local label=$1 expected=$2 mutation=$3 copy="$work/ctl/$1.tsv" rc=0
  cp "$test_manifest" "$copy"
  "$mutation" "$copy"
  if cmp -s "$test_manifest" "$copy"; then
    echo "FAIL manifest control $label: mutation changed nothing" >&2; fail=1; return
  fi
  CONCRETE_TEST_MANIFEST="$copy" CONCRETE_TEST_MANIFEST_SKIP_CONTROLS=1 CONCRETE_TEST_EMIT_ONLY=1 \
    bash "$root/scripts/checks/compiler/tests.sh" >"$work/ctl/$label.log" 2>&1 || rc=$?
  if [ "$rc" -ne 0 ] && grep -qF "$expected" "$work/ctl/$label.log"; then
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
  manifest_control missing-row "tests: test manifest inventory differs from emitter" missing_row
  manifest_control nonexistent-program "tests: test manifest inventory differs from emitter" nonexistent_program
fi

while IFS=$'\t' read -r name program expect; do
  d="$work/ex/$name"
  read -r -a expectation <<<"$expect"
  "${emitter[@]}" emit "${program#bir:}" "$d" "$target" "${expectation[@]}"
  # A child running a planted manifest control never reaches a native build, even if its planted
  # defect is accidentally accepted: the control then fails because the child returned zero.
  if [ "${CONCRETE_TEST_EMIT_ONLY:-0}" = 1 ]; then exit 0; fi
  check "$name" "$d" 10 20
  examples=$((examples + 1))
done <<<"$rows"
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
