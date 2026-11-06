#!/usr/bin/env bash
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
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/fresh.sh"
require_fresh_binary || exit 1
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

echo "=== R-0465: the multi-kernel classes are INSIDE the firewall ==="
# These were the strongest classes in the system and the only ones the firewall did not
# govern: no adapter owned them, no `allowed` list constrained them, and no compile-time
# example covered them. `foldMultiKernelResults` assigned its status by direct record update
# behind a hand-rolled `actsOn` — not unsound, but convention where everything around it was
# structure, and that distinction is the firewall's entire purpose.
R="Concrete/Report/Report.lean"
grep -q "def multiKernelAdapter" "$R" \
  && ok "multiKernelAdapter exists" \
  || no "no multiKernelAdapter — the badge classes are outside the firewall again"
grep -q "assumptionAdapter, multiKernelAdapter\]" "$R" \
  && ok "it is registered in dischargeAdapters (so the all-adapter examples cover it)" \
  || no "multiKernelAdapter is NOT in dischargeAdapters — the all-adapter properties skip it"
grep -q "def DischargeAdapter.admits" "$R" \
  && ok "the admission test is extracted, so non-fold backends share it" \
  || no "no DischargeAdapter.admits — a backend that cannot use fold bypasses the firewall"
# The load-bearing part: the multi-kernel fold must ROUTE through it, twice (the dissent
# branch and the attesting branch each assign a status).
N="$(grep -c "multiKernelAdapter.admits v.status verdict.evidence.present" "$R" || true)"
[ "$N" = "2" ] \
  && ok "both status assignments in foldMultiKernelResults pass the admission test" \
  || no "$N of 2 status assignments are gated — an ungated branch is outside the firewall"
# proofClasses must cover the badges, or the "untrusted backends emit no proof class"
# examples are green while saying nothing about the strongest claims in the system.
grep -q '"proved_by_two_kernels", "proved_by_multi_kernel"\]' "$R" \
  && ok "the badges are in proofClasses (untrusted adapters provably cannot emit them)" \
  || no "proofClasses omits the multi-kernel badges — the firewall examples are vacuous for them"

echo "=== R-0465: the assumption axis is declared, not incidental ==="
# The firewall governed WHO MAY CLAIM WHAT. Evidence has a second axis — WHAT THE CLAIM
# RESTS ON — and H23 lived entirely in it, which is why a well-built firewall missed it.
grep -q "inductive AssumptionPolicy" "$R" \
  && ok "AssumptionPolicy exists (union vs reset is a declared choice)" \
  || no "no AssumptionPolicy — assumption propagation is convention again"
grep -q "assumptions : AssumptionPolicy := .union" "$R" \
  && ok "adapters default to union — never silently reset" \
  || no "the default is not union: a new adapter could drop assumptions by omission"
grep -q "example : dischargeAdapters.all (fun a => a.assumptions == .union) = true := rfl" "$R" \
  && ok "compile-time proof that no backend discharges assumptions today" \
  || no "missing the all-adapters union example"
grep -q "hypDebt := match a.assumptions with" "$R" \
  && ok "fold handles hypDebt EXPLICITLY (was an accident of record-update syntax)" \
  || no "fold no longer states its hypDebt behaviour — a rewrite could drop preservation"
grep -qE "\(omegaAdapter.fold \"\" \[debtVC" "$R" \
  && ok "behavioural lock: discharging a goal does not shrink what it rests on" \
  || no "no hypDebt-preservation example — the H23 axis is unpinned at build time"

echo "=== R-0465: one obligation, one verdict per kernel ==="
# Parallel closed/refused lists could not express disjointness. Given both, the old chain
# wrote both receipts but passed only .closed onward — proved_by_two_kernels beside a stored
# refusal. Unreachable then; unrepresentable now.
grep -q "rocqVerdicts isaVerdicts : List (String × KernelCell)" "$R" \
  && ok "the fold takes per-obligation cells, not two id lists" \
  || no "foldMultiKernelResults still takes parallel closed/refused lists"
if grep -qE "rocqClosed.contains v.id|rocqRefused.contains v.id" "$R"; then
  no "a membership test on the old lists survives — two sources of truth for one verdict"
else
  ok "no membership tests on parallel lists remain"
fi
grep -q "refused.map (fun k => (k, Report.KernelCell.refused))" Main.lean \
  && ok "overlap resolves to .refused — fail-closed if the upstream invariant ever breaks" \
  || no "the pairing is not fail-closed: an overlap could resolve to .closed and earn a badge"

echo ""
echo "DISCHARGE-ADAPTERS: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
