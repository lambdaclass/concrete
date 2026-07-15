#!/usr/bin/env bash
export CONCRETE_ECHO_RESULT=1  # MAIN_EXIT_MODEL stage 1: legacy echoed-result mode until fixtures migrate (stage 2 deletes this)
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

echo "=== discharge outcome (kernel-checked) is folded in ==="
check_json "status from the v1 set" "$HMAC" \
  "set(v['status'] for v in d['vcs']) <= {'planned','proved_by_kernel_decision','proved_by_lean','arithmetic_proved','counterexample','unproven','missing'}"
check_json "engine from the v1 set" "$HMAC" \
  "set(v['engine'] for v in d['vcs']) <= {'constant_fold','omega','bv_decide','lean',''}"
check_json "every kernel-decision VC names an engine" "$HMAC" \
  "all(v['engine'] in ('constant_fold','omega','bv_decide') for v in d['vcs'] if v['status']=='proved_by_kernel_decision')"
check_json "something actually discharged (proved_by_kernel_decision present)" "$HMAC" \
  "any(v['status']=='proved_by_kernel_decision' for v in d['vcs'])"

echo "=== TRUST BOUNDARY: SMT is not yet a discharge path ==="
# Phase 2 has not added an external solver. No VC may claim `smt` as its expected
# OR actual discharge, and no status may be an external-solver class. This pins
# that SMT cannot silently become a 'proved' route before its trust model lands
# (items #8/#9). When SMT arrives, these assertions are updated DELIBERATELY.
check_json "no VC expects smt"            "$HMAC" "all(v['expected_discharge']!='smt' for v in d['vcs'])"
check_json "no VC expects smt"            "$POS"  "all(v['expected_discharge']!='smt' for v in d['vcs'])"
check_json "no engine is smt"             "$HMAC" "all(v['engine']!='smt' for v in d['vcs'])"
check_json "no proved_by_smt/solver_trusted status" "$HMAC" \
  "all(v['status'] not in ('proved_by_smt','solver_trusted') for v in d['vcs'])"

echo "=== honesty: loop preservation is never claimed by a decision procedure ==="
# omega can close O2's arithmetic half only; the operational realization needs
# Lean. So loop_invariant_preservation must NEVER be 'proved_by_kernel_decision'.
check_json "O2 never proved_by_kernel_decision" "$HMAC" \
  "all(v['status']!='proved_by_kernel_decision' for v in d['vcs'] if v['kind']=='loop_invariant_preservation')"
check_json "O2 never proved_by_kernel_decision" "$POS" \
  "all(v['status']!='proved_by_kernel_decision' for v in d['vcs'] if v['kind']=='loop_invariant_preservation')"

echo "=== symbolic precondition actually discharged by omega (the mature path) ==="
check_json "block_to_words_at precond: proved_by_kernel_decision via omega + hyps" "$HMAC" \
  "any(v['kind']=='precondition' and 'block_to_words_at' in v['origin'] and v['status']=='proved_by_kernel_decision' and v['engine']=='omega' and len(v['hypotheses'])>0 for v in d['vcs'])"

echo "=== loop obligations: variant_decreases discharged by omega ==="
check_json "variant_decreases proved_by_kernel_decision via omega" "$POS" \
  "any(v['kind']=='variant_decreases' and v['status']=='proved_by_kernel_decision' and v['engine']=='omega' for v in d['vcs'])"

echo "=== postcondition with a registered proof → proved_by_lean + dependency ==="
check_json "ch ensures proved_by_lean, depends on its ensures_proof" "$HMAC" \
  "any(v['kind']=='postcondition' and v['function']=='hmac_sha256.ch' and len(v['dependencies'])>0 and v['status']=='proved_by_lean' and v['engine']=='lean' for v in d['vcs'])"

echo ""
echo "VC-SCHEMA: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
