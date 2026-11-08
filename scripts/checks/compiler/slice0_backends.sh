#!/usr/bin/env bash
# Slice-0 backend gate (PROVISIONAL, Concrete/README.md gates 4 and 6, hand-written BIR leg):
# every hand-written BIR example goes through the LLVM path (clang) and the LCB path: the adapted
# body assembled by LCB in process (no text on the proved path), linked with the runtime entry,
# which LCB's command line assembles from QBE text.
# Each executable runs under bounded_run; its observation is its exit status (or "incomplete"),
# the exact bytes on descriptor 2, and descriptor 1, which must be empty. All three must equal what
# the BIR semantics gives (the emitter writes the expected status and stderr bytes).
#
# Controls: a body that never returns must be observed as incomplete on both backends; a body
# returning a word that encodes nothing must reach the runtime's refusal; the prepared-numbering,
# predecessor-grouping, reference-equality and stack-depth regressions (slice0_bir.lean numbering)
# must pass with exact refusal diagnostics; and the comparison
# itself must reject a missing or extra newline, a wrong status with the right stderr, and stray
# stdout. Run inside the nix devshell, from the repository root.
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

names=$(emit_examples)
for name in $names; do
  d="$work/ex/$name"
  "${emitter[@]}" emit "$name" "$d" "$target"
  check "$name" "$d" 10 20
  examples=$((examples + 1))
done
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

[ "$examples" -gt 0 ] || { echo "slice0_backends: no examples ran" >&2; exit 1; }
echo "slice0_backends: $examples examples, $count backend runs (controls included)"
exit "$fail"
