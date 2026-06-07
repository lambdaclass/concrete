#!/usr/bin/env bash
# Call-site precondition migration gate (ROADMAP Phase 3 #4).
#
# Phase 3 #4 routes call-site precondition obligations through the unified
# `scopedWalk` collector. The collector threads strictly MORE sound facts than
# the old call walker (negated else-guards + early-return fall-through), so a
# precondition can only move `unproven → proved_by_kernel_decision`, never the
# reverse. The migration was verified against the pre-#4 binary:
#
#   • existing corpus (precondition_callsite): ZERO drift — exact-stable.
#   • adversarial fixture (call_scope_adversarial): the only changes are the two
#     documented `unproven → proved` improvements; the negative control and the
#     already-proved then-guard case are unchanged.
#
# This gate freezes BOTH halves so a future collector change cannot silently
# regress a proof to unproven, manufacture a wrong proof, or lose the existing
# stable behavior — without needing the old binary in CI.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# ck <label> <file> <python-bool-expr with helpers vc()/hyps()>
ck(){ local label="$1" file="$2" expr="$3"
  "$COMPILER" "$file" --report vcs --json 2>/dev/null | python3 -c "
import json,sys
d=json.load(sys.stdin)
byid={v['id']:v for v in d['vcs']}
def vc(i): return byid[i]
def hyps(i): return ' ; '.join(vc(i).get('hypotheses',[]))
sys.exit(0 if ($expr) else 1)
" 2>/dev/null && ok "$label" || no "$label"; }

ADV="examples/contract_negatives/call_scope_adversarial/src/main.con"
echo "=== adversarial: monotonic unproven→proved improvements (vs the old walker) ==="
ck "then-guard call: proved (unchanged from old walker)" "$ADV" \
  "vc('adv.in_then#pre0')['status']=='proved_by_kernel_decision'"
ck "else-branch call: IMPROVED to proved via negated guard (x>0)" "$ADV" \
  "vc('adv.in_else#pre1')['status']=='proved_by_kernel_decision' and 'x > 0' in hyps('adv.in_else#pre1')"
ck "fall-through call: IMPROVED to proved via ¬(x≤0) (x>0)" "$ADV" \
  "vc('adv.after_return#pre2')['status']=='proved_by_kernel_decision' and 'x > 0' in hyps('adv.after_return#pre2')"

echo "=== adversarial: negative control — no manufactured proof ==="
ck "unmet call: stays unproven (nothing in scope establishes 0<x)" "$ADV" \
  "vc('adv.unmet#pre3')['status']=='unproven' and hyps('adv.unmet#pre3')==''"

echo "=== existing corpus: exact-stable (the migration changed nothing here) ==="
PC="examples/contract_negatives/precondition_callsite/src/main.con"
ck "ok_via_requires#pre0 proved"                "$PC" "vc('cn.ok_via_requires#pre0')['status']=='proved_by_kernel_decision'"
ck "ok_via_guard#pre1 proved"                   "$PC" "vc('cn.ok_via_guard#pre1')['status']=='proved_by_kernel_decision'"
ck "violation#pre2 counterexample (caught)"     "$PC" "vc('cn.violation#pre2')['status']=='counterexample'"
ck "unmet#pre3 unproven (honest gap preserved)" "$PC" "vc('cn.unmet#pre3')['status']=='unproven'"

echo ""
echo "CALL-SITE-MIGRATION: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
