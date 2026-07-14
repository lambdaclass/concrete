#!/usr/bin/env bash
# Array-bounds migration gate (ROADMAP Phase 3 #5).
#
# Phase 3 #5 routes array-bounds obligations through the unified `scopedWalk`
# collector. The collector threads strictly MORE sound facts than the old bounds
# walker (enclosing guards, negated guards, early-return fall-through, loop
# invariants), so a bounds obligation can only move `unproven → proved`, never
# the reverse. Verified against the pre-#5 binary across the fixed-array corpus:
#
#   • 9/10 files with bounds VCs: ZERO drift — exact-stable.
#   • hmac_sha256: ids/statuses unchanged; the four bounds VCs merely GAINED the
#     enclosing key-length guard as a hypothesis (k_len>64 / k_len≤64) — a sound
#     enrichment that doesn't bound the index, so status stays unproven.
#   • adversarial fixture: only the two documented `unproven → proved`
#     improvements; the negative controls stay unproven.
#
# This gate freezes that behavior as a permanent, self-contained regression guard
# (no old binary needed in CI).

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# ck <label> <file> <python-bool-expr with helpers vc()/hyps()/bounds()>
ck(){ local label="$1" file="$2" expr="$3"
  "$COMPILER" "$file" --report vcs --json 2>/dev/null | python3 -c "
import json,sys,collections
d=json.load(sys.stdin)
byid={v['id']:v for v in d['vcs']}
B=[v for v in d['vcs'] if v['kind']=='array_bounds']
def vc(i): return byid[i]
def hyps(i): return ' ; '.join(vc(i).get('hypotheses',[]))
def dist(): return dict(collections.Counter(v['status'] for v in B))
sys.exit(0 if ($expr) else 1)
" 2>/dev/null && ok "$label" || no "$label"; }

ADV="examples/contract_negatives/bounds_scope_adversarial/src/main.con"
echo "=== adversarial: monotonic unproven→proved improvements (vs the old walker) ==="
ck "full guard 0≤i<16 in scope ⇒ proved" "$ADV" \
  "vc('adv.guarded_full#bounds0')['status']=='proved_by_kernel_decision' and 'i ≥ 0' in hyps('adv.guarded_full#bounds0')"
ck "fall-through ¬(i<0)∧¬(i≥16) ⇒ proved" "$ADV" \
  "vc('adv.fall_through#bounds0')['status']=='proved_by_kernel_decision'"

echo "=== adversarial: negative controls — no manufactured bound ==="
ck "upper-bound-only guard ⇒ stays unproven (no i≥0 fact)" "$ADV" \
  "vc('adv.guarded_upper_only#bounds0')['status']=='unproven'"
ck "stale index after i=i+1 ⇒ guard dropped ⇒ unproven" "$ADV" \
  "vc('adv.stale_after_incr#bounds0')['status']=='unproven' and hyps('adv.stale_after_incr#bounds0')==''"

echo "=== existing corpus: status distribution exact-stable ==="
# counts include linked-std VCs; std additions must stay all-proved (P7 fmt scalar emission)
ck "fixed_capacity: 29 bounds, 26 proved / 3 unproven" "examples/fixed_capacity/src/main.con" \
  "len(B)==29 and dist().get('proved_by_kernel_decision')==26 and dist().get('unproven')==3"
ck "runtime_safety: 3 bounds, 2 proved / 1 unproven" "examples/proof_patterns/runtime_safety/src/main.con" \
  "len(B)==3 and dist().get('proved_by_kernel_decision')==2 and dist().get('unproven')==1"
ck "parse_validate: 19 bounds, 18 proved / 1 unproven" "examples/parse_validate/src/main.con" \
  "len(B)==19 and dist().get('proved_by_kernel_decision')==18 and dist().get('unproven')==1"

echo "=== documented enrichment: hmac bounds gained the enclosing guard, status unchanged ==="
ck "hmac bounds0 status unproven but now carries k_len guard" "examples/hmac_sha256/src/main.con" \
  "vc('hmac_sha256.hmac_sha256#bounds0')['status']=='unproven' and 'k_len' in hyps('hmac_sha256.hmac_sha256#bounds0')"

echo ""
echo "BOUNDS-MIGRATION: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
