#!/usr/bin/env bash
export CONCRETE_ECHO_RESULT=1  # MAIN_EXIT_MODEL stage 1: legacy echoed-result mode until fixtures migrate (stage 2 deletes this)
# Assert/assume migration gate (ROADMAP Phase 3 #8).
#
# assert(e) rides the unified scopedWalk collector (proved/unproven by the kernel
# tiers — see check_scoped_collector.sh). assume(e) is its opposite: a trusted
# assumption fact now surfaced in the ONE ledger as `assumed`. This gate pins the
# trust boundary:
#   • assume is always `assumed`, never a proof;
#   • assume appears in BOTH --report vcs and the obligation-ledger (one ledger);
#   • the ledger marks assume as a forbid-assume policy input;
#   • NO LAUNDERING: assume(P) must not make a later assert(P) kernel-proved.
#
# Adding assume to the ledger was verified additive against the pre-#8 binary:
# every prior VC is byte-identical; only `assume … assumed` rows are new.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
ADV="examples/contract_negatives/assume_scope_adversarial/src/main.con"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

vcs(){ "$COMPILER" "$ADV" --report vcs --json 2>/dev/null; }
led(){ "$COMPILER" "$ADV" --report obligation-ledger --json 2>/dev/null; }
ckv(){ local label="$1" expr="$2"; vcs | python3 -c "
import json,sys
d=json.load(sys.stdin); byid={v['id']:v for v in d['vcs']}
def has(i): return i in byid
def st(i): return byid[i]['status']
def kind(i): return byid[i]['kind']
sys.exit(0 if ($expr) else 1)
" 2>/dev/null && ok "$label" || no "$label"; }
ckl(){ local label="$1" expr="$2"; led | python3 -c "
import json,sys
d=json.load(sys.stdin); byid={o['id']:o for o in d['obligations']}
def has(i): return i in byid
def st(i): return byid[i]['status']
def kind(i): return byid[i]['kind']
def pol(i): return byid[i].get('policy_impact','')
sys.exit(0 if ($expr) else 1)
" 2>/dev/null && ok "$label" || no "$label"; }

echo "=== assume is a ledger fact, status assumed, never a proof ==="
ckv "no_launder#aa0: kind assume, status assumed"     "kind('adv.no_launder#aa0')=='assume' and st('adv.no_launder#aa0')=='assumed'"
ckv "trusts#aa0: kind assume, status assumed"         "kind('adv.trusts#aa0')=='assume' and st('adv.trusts#aa0')=='assumed'"
ckv "no assume VC is ever proved_by_kernel_decision"  "all(v['status']=='assumed' for v in d['vcs'] if v['kind']=='assume') and any(v['kind']=='assume' for v in d['vcs'])"

echo "=== NO LAUNDERING: assume(P) does not make a later assert(P) kernel-proved ==="
ckv "no_launder#aa1: assert stays unproven despite assume(0<x)" \
  "st('adv.no_launder#aa1')=='unproven'"
ckv "assume_vs_assert#aa1: assert proved by the GUARD, not the assume" \
  "st('adv.assume_vs_assert#aa1')=='proved_by_kernel_decision'"

echo "=== one ledger: assume is in the obligation-ledger as a forbid-assume policy input ==="
ckl "ledger has the assume obligation as assumed"     "has('adv.trusts#aa0') and st('adv.trusts#aa0')=='assumed' and kind('adv.trusts#aa0')=='assume'"
ckl "ledger marks assume as a forbid-assume policy input" \
  "'forbid-assume' in pol('adv.trusts#aa0')"

echo "=== forbid-assume policy still rejects the build (E0614) ==="
ADVDIR="examples/contract_negatives/assume_scope_adversarial"
asm_out="$( cd "$ADVDIR" && "$ROOT_DIR/$COMPILER" build 2>&1 )" && asm_exit=0 || asm_exit=$?
if [ "$asm_exit" -ne 0 ] && printf '%s' "$asm_out" | grep -qF "E0614"; then
  ok "forbid-assume rejects build (E0614)"
else
  no "forbid-assume should reject build with E0614 (exit=$asm_exit)"
fi

echo ""
echo "ASSUME-MIGRATION: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
