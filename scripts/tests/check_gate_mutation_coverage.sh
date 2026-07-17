#!/usr/bin/env bash
# Phase 6C #5: gate mutation-testing — prove the pipeline gates are load-bearing.
#
# For each rule FAMILY, apply a one-line source mutation that disables the rule
# (while still building) and prove the family's SPECIFIC gate goes red (KILLED).
# A mutation whose gate stays green is a SURVIVOR = a decorative gate = failure.
#
# HEAVY / NIGHTLY: the behavioral families need a `lake build` per mutation (~1-3
# min each), so this is not a per-commit gate. Grep-only families (constructor
# coverage, source-maps) need no rebuild. Run one family with FAMILY=<n>, or all
# (default). Restore is via `git checkout --` (assumes a clean tree for the
# mutated files); a final clean rebuild is done at the end.
#
# 7 of 10 gates are OUTSIDE run_tests.sh --fast, so we invoke each gate directly.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
LAKE="${LAKE:-lake}"
ONLY="${FAMILY:-}"

# family i: NAME FILE GATE NEEDS_BUILD ; OLD/NEW written to temp files per family.
NAME=();  FILE=();  GATE=();  BUILD=()
OLD=();   NEW=()
add(){ NAME+=("$1"); FILE+=("$2"); GATE+=("$3"); BUILD+=("$4"); OLD+=("$5"); NEW+=("$6"); }

add "corecheck-unsafe-op" "Concrete/Check/CoreCheck.lean" "check_corecheck_boundary.sh" yes \
  'addCCError (.missingCapability "*raw_ptr" "Unsafe" "")' \
  'pure ()'
add "copy-predicate" "Concrete/Check/Layout.lean" "check_copy_judgment.sh" yes \
  '| some (isC, _, _) => isC' \
  '| some (_, _, _) => true'
add "checked-arith-trap" "Concrete/Backend/EmitSSA.lean" "check_checked_arith.sh" yes \
  $'      let mnem := if ssaIsSignedInt operandTy then "sadd" else "uadd"\n      emitStructured s (.call (some dst) iTy (.global (checkedCallName mnem operandTy)) [(iTy, lOp), (iTy, rOp)])' \
  $'      let _mnem := if ssaIsSignedInt operandTy then "sadd" else "uadd"\n      emitStructured s (.binOp dst .add iTy lOp rOp)'
add "capability-requirement" "Concrete/Check/CoreCheck.lean" "check_capability_judgment.sh" yes \
  'if !capD.satisfied then' \
  'if false then'
add "walker-constructor" "Concrete/Check/CoreCheck.lean" "check_constructor_coverage.sh" no \
  '| .intLit _ _ | .floatLit _ _ | .boolLit _ | .strLit _ | .charLit _ => pure ()' \
  '| .intLit _ _ | .floatLit _ _ | .boolLit _ | .strLit _ | _ => pure ()'
add "source-span-stamping" "Concrete/Elab/Elab.lean" "check_source_maps.sh" no \
  'declSpan := some f.span' \
  'declSpan := none'
add "mono-name-hygiene" "Concrete/IR/Mono.lean" "check_mono_name_collision.sh" yes \
  '| .generic n args => n ++ "_T_" ++ "_".intercalate (args.map tyToSuffix) ++ "_E"' \
  '| .generic n _args => n'
add "diagnostic-quality" "Concrete/Check/CoreCheck.lean" "check_diagnostics_quality.sh" yes \
  '| .insufficientCapabilities _ required _ => some s!"add '"'"'with({required})'"'"' to the calling function, or wrap the call in a trusted function"' \
  '| .insufficientCapabilities _ _ _ => none'
add "fact-invalidation" "Concrete/Report/ReportObligations.lean" "check_scoped_collector.sh" yes \
  '| _ => dropStaleHyps scope (assignedScalarsS s)' \
  '| _ => scope'
add "report-schema-row" "Concrete/Report/Report.lean" "check_vc_schema.sh" yes \
  '("kind", .str v.kind),' \
  '("knd", .str v.kind),'

N=${#NAME[@]}
PASS=0; FAIL=0
TMP=$(mktemp -d); trap 'rm -rf "$TMP"; git checkout -- $(printf "%s " "${FILE[@]}" | tr " " "\n" | sort -u) 2>/dev/null' EXIT

apply(){ # file oldfile newfile
  python3 - "$1" "$2" "$3" <<'PY'
import sys
f,ofl,nfl=sys.argv[1],sys.argv[2],sys.argv[3]
src=open(f).read(); old=open(ofl).read(); new=open(nfl).read()
assert src.count(old)>=1, f"OLD not found in {f}"
open(f,'w').write(src.replace(old,new,1))
PY
}

run_one(){
  local i="$1"
  local nm="${NAME[$i]}" file="${FILE[$i]}" gate="scripts/tests/${GATE[$i]}" needs="${BUILD[$i]}"
  printf '%s\n' "${OLD[$i]}" > "$TMP/old"; printf '%s\n' "${NEW[$i]}" > "$TMP/new"
  # strip the trailing newline the printf added (match raw substring)
  perl -i -pe 'chomp if eof' "$TMP/old" "$TMP/new"
  echo "--- family $((i+1))/$N: $nm ($file -> ${GATE[$i]}) ---"
  if ! apply "$file" "$TMP/old" "$TMP/new" 2>"$TMP/aerr"; then
    echo "  FAIL $nm: could not apply mutation ($(cat "$TMP/aerr"))"; FAIL=$((FAIL+1)); git checkout -- "$file" 2>/dev/null; return
  fi
  local killed=0 note=""
  if [ "$needs" = yes ]; then
    if ! "$LAKE" build >"$TMP/build.log" 2>&1; then
      killed=1; note="(killed by build — type system rejected the mutation)"
    fi
  fi
  if [ "$killed" -eq 0 ]; then
    if bash "$gate" >"$TMP/gate.log" 2>&1; then
      note="(SURVIVED — gate stayed green)"
    else
      killed=1; note="(killed by ${GATE[$i]})"
    fi
  fi
  git checkout -- "$file" 2>/dev/null
  if [ "$killed" -eq 1 ]; then echo "  ok   $nm KILLED $note"; PASS=$((PASS+1));
  else echo "  FAIL $nm $note"; FAIL=$((FAIL+1)); fi
}

echo "=== gate mutation coverage: $N families ==="
if [ -n "$ONLY" ]; then run_one "$((ONLY-1))"; else for i in $(seq 0 $((N-1))); do run_one "$i"; done; fi

# leave a clean binary behind
echo "--- restoring clean build ---"
"$LAKE" build >/dev/null 2>&1 || echo "  warn: clean rebuild failed; run 'lake build'"

echo
echo "GATE-MUTATION-COVERAGE: PASS=$PASS FAIL=$FAIL (of $N families)"
[ "$FAIL" -eq 0 ]
