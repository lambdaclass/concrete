#!/usr/bin/env bash
# VC schema v1 gate (ROADMAP Phase 2 #2 — the foundation).
#
# `concrete <file> --report vcs --json` emits a versioned envelope of every
# generated verification condition. This gate pins schema v1: every VC carries
# id, span, kind, hypotheses, conclusion, origin, dependencies, arith_profile,
# and expected_discharge — and the trust boundary holds: NO VC is routed to an
# external SMT discharge path (Phase 2 has not added one). The schema and its
# evidence classes are nailed before discharge paths expand — so a solver can
# never silently become a "proved" path.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
PASS=0; FAIL=0

check_json(){ local label="$1" file="$2" expr="$3"
  local out; out="$("$COMPILER" "$file" --report vcs --json 2>/dev/null)"
  if printf '%s' "$out" | python3 -c "import json,sys; d=json.load(sys.stdin); sys.exit(0 if ($expr) else 1)" 2>/dev/null; then
    echo "  ok   $label"; PASS=$((PASS+1));
  else echo "  FAIL $label — assertion failed: $expr"; FAIL=$((FAIL+1)); fi; }

HMAC="examples/hmac_sha256/src/main.con"
POS="examples/contract_positive/valid_complex_contract_scope/src/main.con"

echo "=== envelope shape (schema v1) ==="
check_json "schema_kind == vcs"            "$HMAC" "d['schema_kind']=='vcs'"
check_json "vc_schema_version == 1"        "$HMAC" "d['vc_schema_version']==1"
check_json "count matches vcs length"      "$HMAC" "d['count']==len(d['vcs'])"
check_json "generates VCs (count > 0)"     "$HMAC" "d['count']>0"

echo "=== every VC carries all schema-v1 fields ==="
FIELDS="'id','kind','function','loc','hypotheses','conclusion','origin','dependencies','arith_profile','expected_discharge'"
check_json "all fields present on every VC" "$HMAC" \
  "all(all(k in v for k in [$FIELDS]) for v in d['vcs'])"
check_json "loc has file+line"              "$HMAC" \
  "all('file' in v['loc'] and 'line' in v['loc'] for v in d['vcs'])"
check_json "hypotheses/dependencies are lists" "$HMAC" \
  "all(isinstance(v['hypotheses'],list) and isinstance(v['dependencies'],list) for v in d['vcs'])"

echo "=== controlled vocabularies ==="
check_json "kind from the v1 set" "$HMAC" \
  "set(v['kind'] for v in d['vcs']) <= {'precondition','postcondition','assert','vacuity','array_bounds','div_nonzero','no_overflow','loop_invariant_init','loop_invariant_preservation','loop_exit_implies_post','variant_nonnegative','variant_decreases'}"
check_json "arith_profile from the v1 set" "$HMAC" \
  "set(v['arith_profile'] for v in d['vcs']) <= {'constant','linear','bitvector','nonlinear','refinement','operational','unsupported'}"
check_json "expected_discharge from the v1 set" "$HMAC" \
  "set(v['expected_discharge'] for v in d['vcs']) <= {'constant_fold','omega','bv_decide','lean','smt','none'}"

echo "=== TRUST BOUNDARY: SMT is not yet a discharge path ==="
# Phase 2 has not added an external solver. No VC may claim `smt` discharge —
# this pins that SMT cannot silently become a 'proved' route before its trust
# model lands (items #8/#9). When SMT arrives, this assertion is updated DELIBERATELY.
check_json "no VC routed to smt"   "$HMAC" "all(v['expected_discharge']!='smt' for v in d['vcs'])"
check_json "no VC routed to smt"   "$POS"  "all(v['expected_discharge']!='smt' for v in d['vcs'])"

echo "=== symbolic precondition carries hypotheses + omega (the mature path) ==="
check_json "block_to_words_at precond: omega + non-empty hyps" "$HMAC" \
  "any(v['kind']=='precondition' and 'block_to_words_at' in v['origin'] and v['expected_discharge']=='omega' and len(v['hypotheses'])>0 for v in d['vcs'])"

echo "=== loop obligations enumerated (O1/O4/O5 omega, O2/O3 lean) ==="
check_json "variant_decreases VC present, omega" "$POS" \
  "any(v['kind']=='variant_decreases' and v['expected_discharge']=='omega' for v in d['vcs'])"
check_json "loop_invariant_preservation is operational/lean (not a decision proc)" "$POS" \
  "all(v['expected_discharge']=='lean' for v in d['vcs'] if v['kind']=='loop_invariant_preservation')"

echo "=== postcondition with a registered proof links its dependency ==="
check_json "ch ensures depends on its ensures_proof" "$HMAC" \
  "any(v['kind']=='postcondition' and v['function']=='hmac_sha256.ch' and len(v['dependencies'])>0 and v['expected_discharge']=='lean' for v in d['vcs'])"

echo ""
echo "VC-SCHEMA: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
