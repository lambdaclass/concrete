#!/usr/bin/env bash
export CONCRETE_ECHO_RESULT=1  # MAIN_EXIT_MODEL stage 1: legacy echoed-result mode until fixtures migrate (stage 2 deletes this)
# Div/mod + sound-division-lowering migration gate (ROADMAP Phase 3 #6).
#
# Phase 3 #6 routes `divisor ≠ 0` obligations through the unified `scopedWalk`
# collector (fourth family, after asserts, call-site preconditions, bounds). The
# collector threads guards / negated guards / fall-through / loop invariants, so
# a divisor obligation can only move `unproven → proved`, never the reverse.
#
# The migration touched ONLY the scope walker. The SOUND division/modulo lowering
# (`divSound`/`toLeanPropSound`) is unchanged: a `/`/`%` is lowered to Lean
# E-division ONLY when the dividend is provably non-negative, because Concrete
# truncates toward zero while Lean `Int` floors. Verified against the pre-#6
# binary: every div VC across the corpus is byte-identical, and the sound-lowering
# behaviour (an assert containing `/` is proved under a `0 ≤ n` guard but NOT
# lowered without it) is unchanged.
#
# This gate freezes both the divisor migration and the truncate-vs-floor guard.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

ck(){ local label="$1" file="$2" expr="$3"
  "$COMPILER" "$file" --report vcs --json 2>/dev/null | python3 -c "
import json,sys,collections
d=json.load(sys.stdin)
byid={v['id']:v for v in d['vcs']}
DZ=[v for v in d['vcs'] if v['kind']=='div_nonzero']
def has(i): return i in byid
def vc(i): return byid[i]
def hyps(i): return ' ; '.join(vc(i).get('hypotheses',[]))
def dist(): return dict(collections.Counter(v['status'] for v in DZ))
sys.exit(0 if ($expr) else 1)
" 2>/dev/null && ok "$label" || no "$label"; }

ADV="examples/contract_negatives/div_scope_adversarial/src/main.con"
echo "=== adversarial: divisor ≠ 0 — monotonic unproven→proved improvements ==="
ck "guarded: then-branch assumes d≠0 ⇒ proved" "$ADV" \
  "vc('adv.guarded#div0')['status']=='proved_by_kernel_decision' and 'd ≠ 0' in hyps('adv.guarded#div0')"
ck "fall-through: ¬(d==0) ⇒ d≠0 ⇒ proved" "$ADV" \
  "vc('adv.fall_through#div0')['status']=='proved_by_kernel_decision'"

echo "=== adversarial: negative controls — no manufactured nonzero divisor ==="
ck "unguarded: nothing establishes d≠0 ⇒ unproven" "$ADV" \
  "vc('adv.unguarded#div0')['status']=='unproven'"
ck "stale: guard d≠0 dropped after d reassigned ⇒ unproven" "$ADV" \
  "vc('adv.stale#div0')['status']=='unproven' and hyps('adv.stale#div0')==''"

echo "=== sound division lowering: the non-negative-dividend guard is load-bearing ==="
# 0 ≤ n in scope ⇒ n/2 is sound-lowered to E-division ⇒ omega proves the assert.
ck "sound_pos: (n/2 ≤ n) proved under 0≤n guard" "$ADV" \
  "has('adv.sound_pos#aa0') and vc('adv.sound_pos#aa0')['status']=='proved_by_kernel_decision'"
# without the guard, n may be negative; truncate≠floor, so n/2 is NOT lowered:
# the assert produces NO VC and is therefore never (mis-)proved.
ck "sound_neg: unconstrained n ⇒ n/2 NOT lowered ⇒ no assert VC (never proved)" "$ADV" \
  "not has('adv.sound_neg#aa0')"

echo "=== existing corpus: div VCs exact-stable (the migration changed nothing) ==="
# counts include linked-std VCs (P7: fmt/parse constant-divisor additions, all proved;
# the prior lone unproven std div left with the parse rewrite)
# 17->30 (2026-07-15): std.base64 landed (+13 div/mod VCs in linked std, all
# proved_by_kernel_decision — constant divisors). Zero-unproven invariant intact.
ck "hmac_sha256: 30 div VCs, all proved" "examples/hmac_sha256/src/main.con" \
  "len(DZ)==30 and dist().get('proved_by_kernel_decision')==30 and not dist().get('unproven')"
ck "precondition_callsite: ratio#div0 proved" "examples/contract_negatives/precondition_callsite/src/main.con" \
  "vc('cn.ratio#div0')['status']=='proved_by_kernel_decision'"
ck "range_block_count: 2 div VCs proved" "examples/smt/teaching/range_block_count.con" \
  "len(DZ)==2 and dist().get('proved_by_kernel_decision')==2"

echo ""
echo "DIV-MIGRATION: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
