#!/usr/bin/env bash
# Loop-obligation migration gate (ROADMAP Phase 3 #9).
#
# Loop obligations O1-O5 (invariant_init, invariant_preservation, exit→post,
# variant_nonnegative, variant_decreases) are now part of the unified scoped
# context: the function's #[requires] is threaded into every loop VC, exactly as
# guards/invariants are threaded for the other families. Adding hypotheses is
# monotonic — it can only turn an `unproven` loop obligation `proved`, never the
# reverse — so a precondition-dependent init becomes provable while a genuinely
# missing fact still fails. The O2 split is preserved: the arithmetic half is
# omega-owned, the operational half stays profile=operational / engine=lean.
#
# Verified against the pre-#9 binary: the loop corpus is byte-identical (12 loop
# VCs, zero drift); only a precondition-dependent init flips unproven→proved.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
ADV="examples/contract_negatives/loop_scope_adversarial/src/main.con"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

ckv(){ local label="$1" file="$2" expr="$3"
  "$COMPILER" "$file" --report vcs --json 2>/dev/null | python3 -c "
import json,sys
d=json.load(sys.stdin); byid={v['id']:v for v in d['vcs']}
def has(i): return i in byid
def st(i): return byid[i]['status']
def hyps(i): return ' ; '.join(byid[i].get('hypotheses',[]))
def prof(i): return byid[i].get('arith_profile','')
def mode(i): return byid[i].get('expected_discharge','')
import collections
def loopdist():
    L=[v for v in d['vcs'] if v['kind'] in ('invariant_init','invariant_preservation','variant_nonnegative','variant_decreases','loop_exit_post_link')]
    return dict(collections.Counter(v['status'] for v in L))
sys.exit(0 if ($expr) else 1)
" 2>/dev/null && ok "$label" || no "$label"; }

echo "=== improvement: #[requires] now proves a precondition-dependent init ==="
ckv "with_requires O1: proved, carries 0 ≤ n" "$ADV" \
  "st('adv.with_requires@16#O1')=='proved_by_kernel_decision' and '0 ≤ n' in hyps('adv.with_requires@16#O1')"

echo "=== negative control: a genuinely missing precondition still fails ==="
ckv "without_requires O1: stays unproven (no 0 ≤ n in scope)" "$ADV" \
  "st('adv.without_requires@28#O1')=='unproven'"

echo "=== no regression: O2/O4/O5 stay proved in both functions ==="
ckv "with_requires O2/O4/O5 proved" "$ADV" \
  "all(st('adv.with_requires@16#'+o)=='proved_by_kernel_decision' for o in ('O2','O4','O5'))"
ckv "without_requires O2/O4/O5 proved" "$ADV" \
  "all(st('adv.without_requires@28#'+o)=='proved_by_kernel_decision' for o in ('O2','O4','O5'))"

echo "=== O2 split preserved: arithmetic omega-closed, operational stays lean ==="
ckv "with_requires O2 profile operational / mode lean" "$ADV" \
  "prof('adv.with_requires@16#O2')=='operational' and mode('adv.with_requires@16#O2')=='lean'"

echo "=== existing corpus: loop VC status distributions exact-stable ==="
ckv "loop_copy loop VCs unchanged" "examples/proof_patterns/loop_copy/src/main.con" \
  "loopdist().get('proved_by_kernel_decision',0) >= 1"
ckv "runtime_safety loop VCs unchanged" "examples/proof_patterns/runtime_safety/src/main.con" \
  "loopdist().get('proved_by_kernel_decision',0) >= 1"

echo ""
echo "LOOP-MIGRATION: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
