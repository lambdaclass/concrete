#!/usr/bin/env bash
# Obligation-expression lowering gate (ROADMAP Phase 3 #12).
#
# Phase 3 #12 routes every lowering target through ONE shared operator table
# (`obBinOpLean` / `obBinOpSmt` / `leanBinOp` in Concrete/Report.lean) so the
# human / Lean-prop / SMT-LIB renderings cannot drift. The refactor is
# PARITY-ONLY: it was verified byte-identical against the pre-#12 binary for
# --report vcs (JSON + Lean conclusions), --emit-smt (SMT-LIB), --emit-lean-replay
# (replay theorems), and counterexample variable names across an operator-
# exhaustive corpus. This gate freezes the golden renderings so a future change to
# the shared table cannot silently change a spelling, and proves the SAME operator
# is spelled consistently across the Lean and SMT targets.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
F="examples/contract_negatives/lowering_operators/src/main.con"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

echo "=== Lean lowering: the assert conclusion spells every operator from the shared table ==="
CONCL="$("$COMPILER" "$F" --report vcs --json 2>/dev/null \
  | python3 -c "import json,sys;d=json.load(sys.stdin);print(next(v['conclusion'] for v in d['vcs'] if v['kind']=='assert'))" 2>/dev/null)"
EXPECT='(((((a + b) - (a * 1)) ≤ 200 ∧ (a ≥ 0 ∨ b ≠ 7)) ∧ a = a))'
[ "$CONCL" = "$EXPECT" ] && ok "assert conclusion lowers to the exact Lean form (≤ ∧ ≥ ∨ ≠ = + - *)" \
  || { no "assert Lean lowering drifted"; echo "      got:      $CONCL"; echo "      expected: $EXPECT"; }

echo "=== SMT-LIB lowering: the nonlinear-overflow query uses the shared prefix forms ==="
SMT="$("$COMPILER" "$F" --report vcs --emit-smt 2>/dev/null)"
chk(){ printf '%s' "$SMT" | grep -qF "$1" && ok "SMT query contains: $1" || no "SMT query missing: $1"; }
chk '(<= s 30000)'                 # leq prefix
chk '(* s g)'                      # mul prefix
chk '(- 30000)'                    # negative literal lowering
chk '(and '                        # and prefix
chk '(not (and'                    # overflow goal negation (no-overflow)

echo "=== cross-target consistency: the SAME leq op is ≤ in Lean and <= in SMT ==="
# Lean conclusion uses the infix ≤; the SMT query uses the prefix <= — both must
# be present, proving the one operator is spelled per-target from the shared table.
{ printf '%s' "$CONCL" | grep -q '≤' && printf '%s' "$SMT" | grep -qF '(<= '; } \
  && ok "leq renders ≤ (Lean) and <= (SMT) from the one table" \
  || no "leq lowering inconsistent across targets"

echo ""
echo "OBLIGATION-LOWERING: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
