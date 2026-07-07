#!/usr/bin/env bash
# Phase 1 source-contract VALIDATION ARTIFACT (ROADMAP Phase 1 #9).
#
# The single gate for the whole source-contract surface. It:
#   1. runs the two contract sub-gates (negatives+positive+hmac anchor, stability);
#   2. asserts one GOLDEN REPORT SNAPSHOT per contract failure class is
#      byte-identical (drift is real signal — a wording/behaviour change must be
#      reflected deliberately);
#   3. checks the README explains every fixture.
#
# Update snapshots deliberately:
#   UPDATE_PHASE1_SNAPSHOTS=1 bash scripts/tests/check_phase1_contracts.sh
#
# See examples/contract_negatives/README.md for the per-class explanation and
# Concrete/Proof/ProofSoundness.lean (R-22..R-28) for the soundness justification.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
SNAP_DIR="scripts/tests/phase1_snapshots"
README="examples/contract_negatives/README.md"
UPDATE="${UPDATE_PHASE1_SNAPSHOTS:-0}"
mkdir -p "$SNAP_DIR"
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0; UPD=0

# one row per failure class: <class> <report-kind> <fixture-source>
CN="examples/contract_negatives"
CLASSES=(
  "precondition_callsite|contracts|$CN/precondition_callsite/src/main.con"
  "missing_postcondition|contracts|$CN/missing_postcondition/src/main.con"
  "weakened_postcondition|contracts|$CN/weakened_postcondition/src/main.con"
  "invalid_attribute|contracts|$CN/invalid_attribute/src/main.con"
  "invalid_invariant|contracts|$CN/invalid_invariant/src/main.con"
  "invalid_contract_expression|contracts|$CN/invalid_contract_expression/src/main.con"
  "spec_ghost_totality|contracts|$CN/spec_ghost_totality/src/main.con"
  "vacuous_contract|contracts|$CN/vacuous_contract/src/main.con"
  "assert_obligation|contracts|$CN/assert_obligation/src/main.con"
  "assume_taint|contracts|$CN/assume_taint/src/main.con"
  "duplicate_links|contracts|$CN/duplicate_links/src/main.con"
  "fabricated_proof|proof-status|$CN/fabricated_proof/src/main.con"
  "valid_complex_contract_scope|contracts|examples/contract_positive/valid_complex_contract_scope/src/main.con"
)

snap_one(){ local class="$1" kind="$2" src="$3"
  local golden="$SNAP_DIR/$class.$kind.txt" actual="$TMP/$class.$kind.txt"
  "$COMPILER" "$src" --report "$kind" > "$actual" 2>&1
  if [ "$UPDATE" = "1" ]; then cp "$actual" "$golden"; echo "  UPD  $class ($kind)"; UPD=$((UPD+1)); return; fi
  if [ ! -f "$golden" ]; then echo "  FAIL $class — no golden snapshot (run UPDATE_PHASE1_SNAPSHOTS=1)"; FAIL=$((FAIL+1)); return; fi
  if cmp -s "$golden" "$actual"; then echo "  ok   $class ($kind) snapshot"; PASS=$((PASS+1));
  else echo "  FAIL $class — snapshot drift"; diff -u "$golden" "$actual" | head -20 | sed 's/^/    /'; FAIL=$((FAIL+1)); fi; }

echo "=== per-class report snapshots ==="
for row in "${CLASSES[@]}"; do IFS='|' read -r class kind src <<< "$row"; snap_one "$class" "$kind" "$src"; done

# stability class: the artifact is a `diff`, not a --report. Capture the diff text.
echo "=== contract_stability (diff) snapshot ==="
"$COMPILER" snapshot examples/contract_stability/v1.con -o "$TMP/v1.json" >/dev/null 2>&1
"$COMPILER" snapshot examples/contract_stability/v2.con -o "$TMP/v2.json" >/dev/null 2>&1
sdiff_golden="$SNAP_DIR/contract_stability.diff.txt"; sdiff_actual="$TMP/contract_stability.diff.txt"
"$COMPILER" diff "$TMP/v1.json" "$TMP/v2.json" > "$sdiff_actual" 2>&1
if [ "$UPDATE" = "1" ]; then cp "$sdiff_actual" "$sdiff_golden"; echo "  UPD  contract_stability (diff)"; UPD=$((UPD+1));
elif [ ! -f "$sdiff_golden" ]; then echo "  FAIL contract_stability — no golden (run UPDATE_PHASE1_SNAPSHOTS=1)"; FAIL=$((FAIL+1));
elif cmp -s "$sdiff_golden" "$sdiff_actual"; then echo "  ok   contract_stability (diff) snapshot"; PASS=$((PASS+1));
else echo "  FAIL contract_stability — diff drift"; diff -u "$sdiff_golden" "$sdiff_actual" | head -20 | sed 's/^/    /'; FAIL=$((FAIL+1)); fi

if [ "$UPDATE" = "1" ]; then echo ""; echo "PHASE1-CONTRACTS: UPDATED=$UPD snapshots"; exit 0; fi

echo "=== README covers every fixture ==="
[ -f "$README" ] || { echo "  FAIL README missing: $README"; FAIL=$((FAIL+1)); }
if [ -f "$README" ]; then
  for row in "${CLASSES[@]}"; do IFS='|' read -r class _ _ <<< "$row"
    if grep -qF "$class" "$README"; then PASS=$((PASS+1)); else echo "  FAIL README does not mention '$class'"; FAIL=$((FAIL+1)); fi
  done
  grep -qF "contract_stability" "$README" && PASS=$((PASS+1)) || { echo "  FAIL README omits contract_stability"; FAIL=$((FAIL+1)); }
  echo "  ok   README present and covers all classes"
fi

echo "=== sub-gates ==="
if bash scripts/tests/check_contract_negatives.sh >/dev/null 2>&1; then echo "  ok   check_contract_negatives.sh"; PASS=$((PASS+1)); else echo "  FAIL check_contract_negatives.sh"; FAIL=$((FAIL+1)); fi
if bash scripts/tests/check_contract_stability.sh >/dev/null 2>&1; then echo "  ok   check_contract_stability.sh"; PASS=$((PASS+1)); else echo "  FAIL check_contract_stability.sh"; FAIL=$((FAIL+1)); fi

echo ""
echo "PHASE1-CONTRACTS: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
