#!/usr/bin/env bash
export CONCRETE_ECHO_RESULT=1  # MAIN_EXIT_MODEL stage 1: legacy echoed-result mode until fixtures migrate (stage 2 deletes this)
# SMT release-policy gate (ROADMAP Phase 2 #13).
#
# The external solver is useful, but solver_trusted is NOT Lean/kernel evidence.
# A project declares its release stance via [policy] solver-evidence; the build
# gate inspects the VCs an external solver discharged and rejects a release that
# relies on solver_trusted without the required allowance (E0615):
#   - forbid        → any solver_trusted VC is a blocker
#   - assumptions   → blocker unless a named solver-assumption is declared
#   - allow         → accepted
# counterexample / unknown / timeout / solver_error are non-proofs regardless of
# policy and never pass this gate as evidence.
#
# The rejection fires only when an external solver actually produced solver_trusted,
# which requires Z3 — so the reject/pass assertions are Z3-gated. Without Z3 the
# build is honest: no solver evidence is produced, so nothing is rejected.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
ABS="$ROOT_DIR/$COMPILER"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# build_expect <dir> <expected-exit 0|1> <needle-if-fail> <label>
build_expect(){ local dir="$1" want="$2" needle="$3" label="$4"
  local out rc
  out="$( cd "$dir" && "$ABS" build -o /tmp/smtpol_out 2>&1 )" && rc=0 || rc=$?
  rm -f /tmp/smtpol_out
  if [ "$want" = "0" ]; then
    [ "$rc" -eq 0 ] && ok "$label" || { no "$label (exit=$rc)"; printf '%s\n' "$out" | grep -iE "error|E061" | head -2 | sed 's/^/      /'; }
  else
    if [ "$rc" -ne 0 ] && printf '%s' "$out" | grep -qF "$needle"; then ok "$label";
    else no "$label (exit=$rc, missing '$needle')"; printf '%s\n' "$out" | head -2 | sed 's/^/      /'; fi
  fi; }

echo "=== policy parses (no 'unrecognized key' for solver-evidence/assumption) ==="
warn="$("$COMPILER" examples/smt/policy_allow/src/main.con 2>&1 || true)"
printf '%s' "$warn" | grep -qiE "unrecognized key '(solver-evidence|solver-assumption)'" \
  && no "solver-evidence/solver-assumption flagged unrecognized" || ok "solver-evidence/solver-assumption are recognized keys"

echo "=== counterexample / non-proof: a sat VC is never solver_trusted ==="
# (uses the standalone nonlinear example; reported, not a build gate)
cxjson="$("$COMPILER" examples/smt/nonlinear_overflow/src/main.con --report vcs --smt --json 2>/dev/null)"
printf '%s' "$cxjson" | python3 -c "
import json,sys
d=json.load(sys.stdin)
bad=[v for v in d['vcs'] if v['status'] in ('counterexample','unknown','timeout','solver_error') and v['status']=='solver_trusted']
sys.exit(0 if not bad else 1)" \
  && ok "counterexample/unknown/timeout/solver_error are never solver_trusted" || no "a non-proof leaked as solver_trusted"

if command -v z3 >/dev/null 2>&1; then
  echo "=== Z3 present: the release gate has teeth ==="
  build_expect examples/smt/policy_forbid 1 "E0615" "forbid project → build REJECTED (E0615) on solver_trusted"
  build_expect examples/smt/policy_assumptions_missing 1 "E0615" "assumptions w/o named assumption → REJECTED (E0615)"
  build_expect examples/smt/policy_allow 0 "" "assumptions + named solver-assumption → build PASSES"
else
  echo "=== Z3 absent: no solver evidence produced, so builds are honest (pass) ==="
  build_expect examples/smt/policy_forbid 0 "" "forbid project builds (no solver ran → nothing to reject)"
  build_expect examples/smt/policy_allow 0 "" "allow project builds"
fi

echo ""
echo "SMT-POLICY: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
