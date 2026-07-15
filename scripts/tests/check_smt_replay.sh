#!/usr/bin/env bash
export CONCRETE_ECHO_RESULT=1  # MAIN_EXIT_MODEL stage 1: legacy echoed-result mode until fixtures migrate (stage 2 deletes this)
# Lean-replay gate (ROADMAP Phase 2 #12).
#
# For each SMT VC we emit a standalone Lean theorem restating the SAME obligation,
# with an in-toolchain proof attempt (`by omega`). If a kernel-checked tactic
# closes it, the VC graduates solver_trusted → proved_by_lean_replay (solver
# dropped from the claim). The bounded NONLINEAR fragment we route to SMT is, by
# construction, outside omega's reach (and nlinarith is Mathlib, deliberately NOT a
# dependency), so today the attempt does not close and the VC honestly STAYS
# solver_trusted. This gate pins the artifact and that crisp boundary.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
EX="examples/smt/nonlinear_overflow/src/main.con"
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

echo "=== --emit-lean-replay produces a well-formed kernel-checkable theorem ==="
emit="$("$COMPILER" "$EX" --report vcs --emit-lean-replay 2>/dev/null)"
printf '%s' "$emit" | grep -qE "^theorem vc_replay .*: Int\)" && ok "theorem with Int binders" || no "missing theorem/Int binders"
printf '%s' "$emit" | grep -qF "(sample * gain)" && ok "restates the nonlinear obligation" || no "missing the product"
printf '%s' "$emit" | grep -qF ":= by omega" && ok "carries an in-toolchain proof attempt (by omega)" || no "missing proof attempt"
printf '%s' "$emit" | grep -qF "lake env lean" && ok "includes the check command" || no "missing check command"

echo "=== the in-toolchain attempt genuinely FAILS (nonlinear → not omega-replayable) ==="
# This is the honest negative: omega cannot close it, so the VC must stay
# solver_trusted — it is NOT silently upgraded. (No Z3 needed; lean via lake is.)
"$COMPILER" "$EX" --report vcs --emit-lean-replay 2>/dev/null | grep "^theorem" | head -1 > "$TMP/vc_replay.lean"
if command -v lake >/dev/null 2>&1; then
  if lake env lean "$TMP/vc_replay.lean" >/dev/null 2>&1; then
    no "omega closed the nonlinear replay (unexpected — would change the class story)"
  else
    ok "omega cannot close the nonlinear replay → VC stays solver_trusted (no Mathlib added)"
  fi
else
  echo "  skip omega-replay check (lake not on PATH)"
fi

echo "=== default --report vcs has NO lean_replay data ==="
"$COMPILER" "$EX" --report vcs --json 2>/dev/null | python3 -c "
import json,sys
d=json.load(sys.stdin)
sys.exit(0 if all(v.get('smt') is None for v in d['vcs']) else 1)" \
  && ok "no SMT/replay data without a flag" || no "replay/smt data leaked by default"

if command -v z3 >/dev/null 2>&1; then
  echo "=== Z3 present: --smt --replay keeps solver_trusted (replay does not rescue it) ==="
  rj="$("$COMPILER" "$EX" --report vcs --smt --replay --json 2>/dev/null)"
  printf '%s' "$rj" | python3 -c "
import json,sys
d=json.load(sys.stdin)
# scale was solver_trusted under --smt; replay can't close nonlinear → still solver_trusted.
sc=next((v for v in d['vcs'] if v['function'].endswith('.scale') and v.get('smt')), None)
ok = sc is not None and sc['status']=='solver_trusted'
# and NOTHING was upgraded to proved_by_lean_replay.
ok = ok and not any(v['status']=='proved_by_lean_replay' for v in d['vcs'])
# the replay artifact is present on the VC.
ok = ok and sc['smt'].get('lean_replay') is not None
sys.exit(0 if ok else 1)" \
    && ok "scale stays solver_trusted under --replay; nothing falsely upgraded; artifact present" || no "replay altered the class incorrectly"
else
  echo "  skip --smt --replay end-to-end (z3 not on PATH)"
fi

echo ""
echo "SMT-REPLAY: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
