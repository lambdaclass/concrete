#!/usr/bin/env bash
# Unified scoped collector gate (ROADMAP Phase 3 #3 — adversarial regression).
#
# Phase 3 introduces ONE scoped context collector (`scopedWalk` in
# Concrete/Report.lean). Obligation families migrate onto it one by one; asserts
# are the first migrated family. This gate is the adversarial guard the project
# requires for every Phase-3 pipeline change: it pins the path-fact threading on
# a fixture that exercises EACH mechanism the collector must get right, AND the
# negative controls that must NOT be mis-proved.
#
# The migration to the unified engine was verified byte-for-byte against the
# pre-refactor binary (old-vs-new assert VC diff, zero drift). This gate freezes
# that behavior so a future collector change cannot silently regress it — without
# needing the old binary in CI.
#
#   then-guard            → then-branch assumes the guard
#   negated else-guard    → else-branch assumes ¬guard
#   early-return fall-thru → code after `if c {return}` assumes ¬c
#   nested guards          → both guards in scope
#   stale-hyp invalidation → a guard fact is DROPPED after the var is reassigned
#   over-claim control     → sound facts that do NOT entail the claim stay unproven

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
FIX="examples/contract_negatives/assert_scope_adversarial/src/main.con"
JSON="$("$COMPILER" "$FIX" --report vcs --json 2>/dev/null)"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# ck <label> <python-bool-expr over `vc(id)` helper and `d`>
ck(){ local label="$1" expr="$2"
  printf '%s' "$JSON" | python3 -c "
import json,sys
d=json.load(sys.stdin)
byid={v['id']:v for v in d['vcs']}
def vc(i): return byid[i]
def hyps(i): return ' ; '.join(vc(i).get('hypotheses',[]))
sys.exit(0 if ($expr) else 1)
" 2>/dev/null && ok "$label" || no "$label"; }

echo "=== positive: each threading mechanism supplies the fact that proves the assert ==="
ck "then-guard: x<10 in scope ⇒ proved"          "vc('adv.then_guard#aa0')['status']=='proved_by_kernel_decision' and 'x < 10' in hyps('adv.then_guard#aa0')"
ck "negated else-guard: x≥10 in scope ⇒ proved"  "vc('adv.else_guard#aa0')['status']=='proved_by_kernel_decision' and 'x ≥ 10' in hyps('adv.else_guard#aa0')"
ck "early-return fall-through: ¬(x<0)∧¬(x>100) ⇒ proved" "vc('adv.fall_through#aa0')['status']=='proved_by_kernel_decision' and 'x ≥ 0' in hyps('adv.fall_through#aa0') and 'x ≤ 100' in hyps('adv.fall_through#aa0')"
ck "nested guards: both x>0 and x<10 in scope ⇒ proved" "vc('adv.nested#aa0')['status']=='proved_by_kernel_decision' and 'x > 0' in hyps('adv.nested#aa0') and 'x < 10' in hyps('adv.nested#aa0')"

echo "=== negative controls: the collector must NOT over-supply facts ==="
# the guard establishes x<10, but x is reassigned before the assert: the stale
# fact MUST be dropped, leaving nothing to prove x<16 → unproven (no mis-proof).
ck "stale-hyp drop: reassigned x invalidates the guard fact ⇒ unproven" \
  "vc('adv.stale_after_assign#aa0')['status']=='unproven' and 'x < 10' not in hyps('adv.stale_after_assign#aa0')"
# sound path facts 0≤x≤100 do NOT entail x==50.
ck "over-claim under fall-through: facts don't entail x==50 ⇒ unproven" \
  "vc('adv.over_claim#aa0')['status']=='unproven'"

echo "=== sanity: every assert VC is linear/omega-routed (no spurious SMT/profile drift) ==="
ck "all 6 asserts present and linear-profiled" \
  "len([v for v in d['vcs'] if '#aa' in v['id']])==6 and all(v['arith_profile'] in ('linear','constant') and not v.get('smt') for v in d['vcs'] if '#aa' in v['id'])"

echo ""
echo "SCOPED-COLLECTOR: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
