#!/usr/bin/env bash
# ObligationCore ledger gate (ROADMAP Phase 3 #1/#2 — schema + vocabulary).
#
# Pins the v1 obligation ledger: every obligation carries the full schema-v1
# field set; every status and kind comes from the ONE canonical vocabulary; the
# allowed-engines map matches the semantic profile; and the ledger AGREES with
# `--report vcs` (same ids and statuses) — proving it is a VIEW over the same
# discharge, not a parallel recompute. This is the foundation the rest of the
# Phase 3 migration builds on.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
HMAC="examples/hmac_sha256/src/main.con"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
ck(){ local label="$1" file="$2" expr="$3"
  "$COMPILER" "$file" --report obligation-ledger --json 2>/dev/null \
    | python3 -c "import json,sys; d=json.load(sys.stdin); sys.exit(0 if ($expr) else 1)" 2>/dev/null \
    && ok "$label" || no "$label"; }

echo "=== envelope + schema-v1 fields ==="
ck "schema_kind == obligation_ledger" "$HMAC" "d['schema_kind']=='obligation_ledger'"
ck "ledger_schema_version == 1"       "$HMAC" "d['ledger_schema_version']==1"
ck "count matches obligations length" "$HMAC" "d['count']==len(d['obligations'])"
ck "generates obligations"            "$HMAC" "d['count']>0"
FIELDS="'id','kind','function','loc','origin','variables','hypotheses','conclusion','semantic_profile','dependencies','allowed_engines','status','engine','counterexample','replay','policy_impact'"
ck "every obligation has all schema-v1 fields" "$HMAC" \
  "all(all(k in o for k in [$FIELDS]) for o in d['obligations'])"

echo "=== single canonical vocabulary (Phase 3 #2) ==="
ck "every status ∈ canonical statusVocabulary" "$HMAC" \
  "set(o['status'] for o in d['obligations']) <= {'proved_by_lean','proved_by_kernel_decision','proved_by_lean_replay','arithmetic_proved','solver_trusted','tested_by_oracle','runtime_checked','enforced','assumed','trusted','partial','stale','vacuous','missing','unproven','planned','counterexample','unknown','timeout','solver_error','ineligible'}"
ck "every kind ∈ canonical kindVocabulary" "$HMAC" \
  "set(o['kind'] for o in d['obligations']) <= {'requires_at_entry','postcondition','precondition','array_bounds','div_nonzero','no_overflow','assert','assume','vacuity','loop_invariant_init','loop_invariant_preservation','loop_exit_implies_post','variant_nonnegative','variant_decreases','invalid_contract_expression','impure_contract_call','source_proof_link','proof_fingerprint','spec_drift','missing_theorem','blocked_proof','ineligible_construct','smt_query','oracle_evidence','runtime_enforced','trusted_boundary'}"
ck "allowed_engines come from the tier set" "$HMAC" \
  "all(set(o['allowed_engines']) <= {'constant_fold','omega','bv_decide','smt','lean'} for o in d['obligations'])"
ck "kernel-decided obligations are owned by a kernel engine" "$HMAC" \
  "all(set(o['allowed_engines']) <= {'constant_fold','omega','bv_decide'} for o in d['obligations'] if o['status']=='proved_by_kernel_decision')"

echo "=== the ledger is a VIEW over the VC discharge (agrees with --report vcs) ==="
vcs_json="$("$COMPILER" "$HMAC" --report vcs --json 2>/dev/null)"
led_json="$("$COMPILER" "$HMAC" --report obligation-ledger --json 2>/dev/null)"
python3 -c "
import json,sys
vcs=json.loads('''$vcs_json'''.strip() or '{}')
led=json.loads('''$led_json'''.strip() or '{}')
" 2>/dev/null || true
# compare extracted id:status maps (never embed the JSON in python source).
vmap="$(printf '%s' "$vcs_json" | python3 -c "import json,sys;d=json.load(sys.stdin);print('|'.join(sorted(v['id']+':'+v['status'] for v in d['vcs'])))")"
lmap="$(printf '%s' "$led_json" | python3 -c "import json,sys;d=json.load(sys.stdin);print('|'.join(sorted(o['id']+':'+o['status'] for o in d['obligations'])))")"
[ -n "$vmap" ] && [ "$vmap" = "$lmap" ] && ok "ledger ids+statuses == VC ids+statuses (a view, not a recompute)" \
  || no "ledger drifted from the VC view"

echo "=== default: ledger carries no external-solver data unless --smt path engaged ==="
ck "no solver_trusted/counterexample by default (no --smt)" "$HMAC" \
  "all(o['status'] not in ('solver_trusted','counterexample') for o in d['obligations'])"

echo ""
echo "OBLIGATION-CORE: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
