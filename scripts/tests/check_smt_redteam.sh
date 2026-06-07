#!/usr/bin/env bash
# Red-team VC/SMT gate (ROADMAP Phase 2 #4) — try to break everything.
#
# Every adversarial input must stay a NON-PROOF or an honest diagnostic, never a
# misleading green, and the compiler must not crash. This pins the breaking cases
# as standing regressions, not a one-time audit:
#   - reassigned guard       → stale hypothesis dropped (assert not proved)
#   - edge/huge literals      → no crash; honest discharge
#   - non-literal divisor     → not lowered, not mis-proved
#   - negative-operand division (see range_block_count::signed_div) → not mis-proved
#   - fake / garbage solver   → solver_error, never proof
#   - solver says sat w/ junk → counterexample (non-proof), never proof

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
ABS="$ROOT_DIR/$COMPILER"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
RT="examples/smt/red_team/src/main.con"
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# the omega/bv discharge is deterministic and reproducible; helper to grab an
# assert VC's status block.
vc_status(){ "$COMPILER" "$RT" --report vcs 2>/dev/null | awk -v a="$1" 'index($0,a){f=1} f{print} f&&/^$/{exit}'; }

echo "=== no crash on adversarial input (every report kind runs) ==="
for r in vcs contracts proof-status audit; do
  "$COMPILER" "$RT" --report "$r" >/dev/null 2>&1 && ok "no crash: --report $r" || no "crashed on --report $r"
done

echo "=== soundness: nothing adversarial is mis-proved ==="
printf '%s' "$(vc_status 'reassigned_guard#aa0')" | grep -qF "proved_by_kernel_decision" \
  && no "reassigned guard mis-proved (stale hypothesis used) — UNSOUND" || ok "reassigned guard → not proved (stale hypothesis dropped)"
# non-literal divisor: must not produce a kernel proof (no sound lowering).
"$COMPILER" "$RT" --report vcs 2>/dev/null | awk '/var_divisor#aa0/{f=1} f{print} f&&/^$/{exit}' | grep -qF "proved_by_kernel_decision" \
  && no "var_divisor (non-literal divisor) was mis-proved — UNSOUND" || ok "var_divisor → not mis-proved (no sound lowering)"
# edge/huge literal: omega proves the in-range assert (no crash, sound).
printf '%s' "$(vc_status 'edge_bounds#aa0')" | grep -qF "proved_by_kernel_decision (omega)" \
  && ok "edge_bounds (huge i32 literal) → omega-proved (no crash, sound)" || no "edge_bounds not omega-proved"

echo "=== default report carries no SMT data ==="
"$COMPILER" "$RT" --report vcs --json 2>/dev/null | python3 -c "
import json,sys
d=json.load(sys.stdin)
sys.exit(0 if all(v.get('smt') is None for v in d['vcs']) else 1)" \
  && ok "no SMT data without --smt" || no "SMT data leaked by default"

echo "=== fake / garbage solver on PATH → solver_error, never proof ==="
# a z3 that prints junk must never yield a proof; the nonlinear product VC must
# come back solver_error (or, with a 'sat' liar, counterexample) — never proved.
mkfake(){ mkdir -p "$TMP/$1"; printf '#!/bin/sh\n%s\n' "$2" > "$TMP/$1/z3"; chmod +x "$TMP/$1/z3"; }
run_fake(){ PATH="$TMP/$1:$PATH" "$ABS" "$RT" --report vcs --smt --json 2>/dev/null; }

mkfake junk 'echo banana; exit 0'
jr="$(run_fake junk)"
printf '%s' "$jr" | python3 -c "
import json,sys
d=json.load(sys.stdin)
v=next((v for v in d['vcs'] if v.get('smt') and v['kind']=='no_overflow'), None)
sys.exit(0 if v and v['status']=='solver_error' else 1)" \
  && ok "garbage solver output → solver_error" || no "garbage solver did not yield solver_error"
printf '%s' "$jr" | python3 -c "
import json,sys
d=json.load(sys.stdin)
proofs={'proved_by_kernel_decision','proved_by_lean','proved_by_lean_replay','solver_trusted'}
sys.exit(0 if not any(v.get('smt') and v['status'] in proofs for v in d['vcs']) else 1)" \
  && ok "garbage solver never yields any proof class" || no "garbage solver produced a proof class — UNSOUND"

# a lying solver that says `sat` (claims a counterexample) → counterexample, a
# NON-PROOF; it can never mark the VC proved.
mkfake liar 'echo sat'
lr="$(run_fake liar)"
printf '%s' "$lr" | python3 -c "
import json,sys
d=json.load(sys.stdin)
proofs={'proved_by_kernel_decision','proved_by_lean','proved_by_lean_replay','solver_trusted'}
sys.exit(0 if not any(v.get('smt') and v['status'] in proofs for v in d['vcs']) else 1)" \
  && ok "lying 'sat' solver never yields a proof (counterexample is a non-proof)" || no "lying solver produced a proof class — UNSOUND"

echo ""
echo "SMT-REDTEAM: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
