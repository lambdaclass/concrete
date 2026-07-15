#!/usr/bin/env bash
export CONCRETE_ECHO_RESULT=1  # MAIN_EXIT_MODEL stage 1: legacy echoed-result mode until fixtures migrate (stage 2 deletes this)
# VC/discharge example matrix gate (ROADMAP Phase 2 #15).
#
# A small worked reference for EVERY VC status a user will see in --report vcs /
# --report contracts. Six statuses have tiny self-contained subexamples under
# examples/vc_discharge/ (no proof registry JSON). The three proof-backed statuses
# (proved_by_lean / partial / stale) would each require a real Lean proof to be
# honest — recreating one per status is not "compact" and a link to a nonexistent
# theorem would be a misleading green — so the matrix cites the existing VERIFIED
# references for those. One pinned assertion per status.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
VC="examples/vc_discharge"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# assert_report <label> <file> <report-args...> <needle>  (needle is the LAST arg)
assert_in(){ local label="$1" file="$2"; shift 2
  local needle="${@: -1}"; set -- "${@:1:$(($#-1))}"
  local out; out="$("$COMPILER" "$file" --report "$@" 2>/dev/null)"
  if printf '%s' "$out" | grep -qF -- "$needle"; then ok "$label"; else no "$label — missing '$needle'"; fi; }

echo "=== self-contained subexamples (examples/vc_discharge/) ==="
assert_in "omega          → proved_by_kernel_decision (omega)"      "$VC/omega.con"   vcs "proved_by_kernel_decision (omega)"
assert_in "bv_decide      → proved_by_kernel_decision (bv_decide)"  "$VC/bv_decide.con" vcs "proved_by_kernel_decision (bv_decide)"
assert_in "missing        → missing"                                "$VC/missing.con" vcs "status:  missing"
assert_in "assumed        → assumed (trust boundary)"               "$VC/assumed.con" contracts "assumed (trust, not proof"

if command -v z3 >/dev/null 2>&1; then
  echo "=== self-contained SMT subexamples (Z3 present) ==="
  st="$("$COMPILER" "$VC/solver_trusted.con" --report vcs --smt --json 2>/dev/null)"
  printf '%s' "$st" | python3 -c "import json,sys;d=json.load(sys.stdin);sys.exit(0 if any(v['status']=='solver_trusted' for v in d['vcs']) else 1)" \
    && ok "solver_trusted   → solver_trusted (external SMT)" || no "solver_trusted.con did not yield solver_trusted"
  cx="$("$COMPILER" "$VC/counterexample.con" --report vcs --smt --json 2>/dev/null)"
  printf '%s' "$cx" | python3 -c "import json,sys;d=json.load(sys.stdin);sys.exit(0 if any(v['status']=='counterexample' for v in d['vcs']) else 1)" \
    && ok "counterexample   → counterexample (external SMT)" || no "counterexample.con did not yield counterexample"
else
  echo "  skip solver_trusted / counterexample (z3 not on PATH)"
fi

echo "=== proof-backed statuses — existing verified references ==="
assert_in "proved_by_lean → proved_by_lean (ref: hmac_sha256 ch)" \
  "examples/hmac_sha256/src/main.con" contracts "status:  proved_by_lean"
assert_in "partial        → partial (ref: weakened_postcondition)" \
  "examples/contract_negatives/weakened_postcondition/src/main.con" contracts "partial — one direction proved_by_lean"
assert_in "stale          → proof stale (ref: stale_missing_partial)" \
  "examples/proof_patterns/stale_missing_partial/src/main.con" proof-status "proof stale"

echo ""
echo "VC-DISCHARGE-EXAMPLES: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
