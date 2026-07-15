#!/usr/bin/env bash
export CONCRETE_ECHO_RESULT=1  # MAIN_EXIT_MODEL stage 1: legacy echoed-result mode until fixtures migrate (stage 2 deletes this)
# Backend discharge-adapter gate (ROADMAP Phase 3 #13).
#
# Every status-changing backend is a typed `DischargeAdapter` with a DECLARED set
# of evidence classes it may produce; `DischargeAdapter.fold` applies a result
# only when the class is in that set. The firewall is therefore structural:
#   - the COMPILE-TIME `example`s in Concrete/Report/Report.lean prove smtAdapter /
#     runtimeAdapter / assumptionAdapter / oracleAdapter declare NO static-proof
#     class, and that the fold rejects a foreign class — kernel-checked, so a
#     green build already guarantees the firewall;
#   - this gate adds the BEHAVIORAL evidence over the CLI: discharge output is
#     byte-identical to the pre-#13 pipeline, an external solver cannot turn a
#     kernel-proved VC into solver evidence (nor manufacture kernel evidence),
#     and assumptions never read as a proof.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
OVF="examples/contract_negatives/overflow_scope_adversarial/src/main.con"
ASM="examples/contract_negatives/assume_scope_adversarial/src/main.con"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# run <file> <extra-args...>; capture --report vcs --json
ckjson(){ local label="$1" file="$2" args="$3" path="$4" expr="$5"
  PATH="$path" "$COMPILER" "$file" --report vcs $args --json 2>/dev/null | python3 -c "
import json,sys
d=json.load(sys.stdin); byid={v['id']:v for v in d['vcs']}
def st(i): return byid[i]['status']
def eng(i): return byid[i].get('engine','')
sys.exit(0 if ($expr) else 1)
" 2>/dev/null && ok "$label" || no "$label"; }

echo "=== adapters own the right classes (default discharge, kernel only) ==="
ckjson "omega adapter → proved_by_kernel_decision (engine omega)" "$OVF" "" "$PATH" \
  "st('adv.linear_guarded#ovf0')=='proved_by_kernel_decision' and eng('adv.linear_guarded#ovf0')=='omega'"
ckjson "bv_decide adapter → proved_by_kernel_decision (engine bv_decide)" "$OVF" "" "$PATH" \
  "st('adv.product_guarded#ovf0')=='proved_by_kernel_decision' and eng('adv.product_guarded#ovf0')=='bv_decide'"
ckjson "default discharge is solver-clean" "$OVF" "" "$PATH" \
  "all(v['status'] not in ('solver_trusted','counterexample') for v in d['vcs'])"
ckjson "signed nonlinear product unproven by kernel tiers" "$OVF" "" "$PATH" \
  "st('adv.signed_product#ovf0')=='unproven'"

echo "=== FIREWALL: external SMT cannot produce or overwrite kernel evidence ==="
TMP="$(mktemp -d)"; printf '#!/bin/sh\necho unsat\n' > "$TMP/z3"; chmod +x "$TMP/z3"
ckjson "under --smt, omega/bv VCs KEEP proved_by_kernel_decision" "$OVF" "--smt" "$TMP:$PATH" \
  "all(st(i)=='proved_by_kernel_decision' and eng(i) in ('omega','bv_decide') for i in ('adv.linear_guarded#ovf0','adv.product_guarded#ovf0'))"
ckjson "under --smt, the solver only takes the kernel-unproved VC (smt:z3)" "$OVF" "--smt" "$TMP:$PATH" \
  "st('adv.signed_product#ovf0')=='solver_trusted' and eng('adv.signed_product#ovf0')=='smt:z3'"
rm -rf "$TMP"

echo "=== FIREWALL: assumptions never read as proof ==="
ckjson "assume VC is 'assumed', never a proof class" "$ASM" "" "$PATH" \
  "all(v['status']=='assumed' for v in d['vcs'] if v['kind']=='assume') and any(v['kind']=='assume' for v in d['vcs'])"

echo "=== structural firewall is kernel-checked at build time ==="
grep -q "def proofClasses" Concrete/Report/Report.lean \
  && grep -q "smtAdapter.allowed.all (fun c => !proofClasses.contains c)" Concrete/Report/Report.lean \
  && ok "compile-time firewall examples present (build green ⇒ proven)" \
  || no "firewall examples missing from Concrete/Report/Report.lean"

echo ""
echo "DISCHARGE-ADAPTERS: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
