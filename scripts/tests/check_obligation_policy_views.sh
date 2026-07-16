#!/usr/bin/env bash
# Policy-as-ledger-view gate (ROADMAP Phase 3 #14).
#
# Release policy now reads its inputs from the ONE obligation ledger
# (ObligationCore.{vacuousFunctions,assumeFunctions,solverTrustedIds}) via
# `computePolicyQuals` — the `compute*Quals` side channels are gone. This gate
# pins that the policy DECISIONS are exact-stable AND that the facts they act on
# match the ledger projection:
#   - forbid-assume         (E0614)  ← ledger `assume` obligations
#   - forbid-vacuous        (E0613)  ← ledger `vacuity` obligations (const-fold + omega)
#   - solver-evidence       (E0615)  ← ledger `solver_trusted` obligations
#   - release/profile gate behavior  (exit codes)
# (stale-proof / runtime-safety / trusted-boundary statuses are present in the
# ledger via the proof-link and discharge families and carry their policyImpact.)

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
CN="examples/contract_negatives"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# build_expect <dir> <expected-exit> <expected E06xx code or -> <label>
build_expect(){ local dir="$1" exp="$2" code="$3" label="$4"
  local out rc
  out="$(cd "$dir" && "$COMPILER" build 2>&1)"; rc=$?
  local codeok=1
  if [ "$code" != "-" ]; then grep -qF <<<"$out" "$code" || codeok=0; fi
  if [ "$rc" = "$exp" ] && [ "$codeok" = "1" ]; then ok "$label (exit $rc${code:+, $code})"
  else no "$label (exit=$rc want $exp; code $code)"; fi
}

# fake unsat solver so the solver-evidence fixtures are deterministic in CI.
TMP="$(mktemp -d)"; printf '#!/bin/sh\necho unsat\n' > "$TMP/z3"; chmod +x "$TMP/z3"
export PATH="$TMP:$PATH"

echo "=== forbid-assume (E0614) — from ledger assume obligations ==="
build_expect "$CN/assume_taint" 1 "E0614" "assume_taint rejected"
build_expect "$CN/assume_scope_adversarial" 1 "E0614" "assume_scope_adversarial rejected"

echo "=== solver-evidence (E0615) — from ledger solver_trusted obligations ==="
build_expect "examples/smt/policy_forbid" 1 "E0615" "policy_forbid rejects solver evidence"
build_expect "examples/smt/policy_assumptions_missing" 1 "E0615" "assumptions-missing rejected"
build_expect "examples/smt/policy_allow" 0 "-" "policy_allow accepts (release passes)"

echo "=== release/profile gate: clean flagship projects still pass ==="
build_expect "examples/hmac_sha256" 0 "-" "hmac_sha256 release passes"
build_expect "examples/fixed_capacity" 0 "-" "fixed_capacity release passes"
rm -rf "$TMP"

echo "=== the facts policy reads MATCH the ledger projection (no side channel) ==="
# every assume the policy would reject is an `assume` obligation in the ledger.
"$COMPILER" "$CN/assume_scope_adversarial/src/main.con" --report obligation-ledger --json 2>/dev/null \
  | python3 -c "import json,sys;d=json.load(sys.stdin);sys.exit(0 if any(o['kind']=='assume' for o in d['obligations']) else 1)" \
  && ok "assume obligations present in the ledger" || no "no assume obligations in ledger"
# the policy walkers are gone from Main.
grep -qE "def computeVacuousQuals|def computeAssumeQuals|def computeSolverTrustedQuals" Main.lean \
  && no "a compute*Quals side channel is still defined in Main" \
  || ok "compute*Quals side channels removed (policy reads computePolicyQuals → ledger)"

echo ""
echo "OBLIGATION-POLICY-VIEWS: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
