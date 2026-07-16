#!/usr/bin/env bash
# Red-team gate for the ObligationCore ledger (Phase 3 hardening).
#
# Every function in examples/contract_negatives/obligation_redteam is an attack:
# a construct that must NEVER read as proved, must NOT launder trust into kernel
# evidence, must NOT drift between surfaces, and must be caught by release policy.
# This gate tries to break the ledger and asserts it refuses:
#   1. NO FALSE GREEN — unsafe overflow / OOB index / div-by-zero / a
#      laundering assert / a vacuous postcondition are never `proved`.
#   2. NO DRIFT — --report vcs and the ledger agree on every id+status.
#   3. FIREWALL — even a LYING external solver cannot turn a kernel-unproved VC
#      into kernel evidence, nor a kernel-owned VC into solver evidence.
#   4. POLICY catches every escape (E0613 vacuous, E0614 assume, E0615 solver).

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
DIR="examples/contract_negatives/obligation_redteam"
F="$DIR/src/main.con"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

led(){ "$COMPILER" "$F" --report obligation-ledger --json 2>/dev/null; }
ck(){ local label="$1" expr="$2"
  led | python3 -c "
import json,sys
d=json.load(sys.stdin); byid={o['id']:o for o in d['obligations']}
def st(i): return byid[i]['status'] if i in byid else '<absent>'
sys.exit(0 if ($expr) else 1)" 2>/dev/null && ok "$label" || no "$label"; }

echo "=== 1. NO FALSE GREEN: unsafe constructs are never proved ==="
ck "unbounded product overflow → unproven"        "st('rt.unsafe_mul#ovf0')=='unproven'"
ck "out-of-bounds index → unproven"                "st('rt.oob#bounds0')=='unproven'"
ck "division by possibly-zero divisor → unproven"  "st('rt.divzero#div0')=='unproven'"
ck "assume-laundered assert → unproven (not proof)" "st('rt.launder#aa1')=='unproven'"
ck "assume is 'assumed', never a proof class"       "st('rt.launder#aa0')=='assumed'"
ck "vacuous postcondition → NOT proved_by_lean"     "st('rt.vacuous_green#ensures0') in ('missing','vacuous','unproven')"
# in the red-team module the ONLY proved obligation is the vacuity DETECTION
# (flagging the unsat contract) — every other rt.* obligation is refused.
ck "no kernel proof in rt.* except the vacuity detector" \
  "all(o['status']!='proved_by_kernel_decision' or o['kind']=='vacuity' for o in d['obligations'] if o['id'].startswith('rt.'))"

echo "=== 2. NO DRIFT: --report vcs and the ledger agree ==="
vmap="$("$COMPILER" "$F" --report vcs --json 2>/dev/null | python3 -c "import json,sys;d=json.load(sys.stdin);print('|'.join(sorted(v['id']+':'+v['status'] for v in d['vcs'])))")"
lmap="$(led | python3 -c "import json,sys;d=json.load(sys.stdin);print('|'.join(sorted(o['id']+':'+o['status'] for o in d['obligations'] if not o['id'].endswith('#prooflink'))))")"
[ -n "$vmap" ] && [ "$vmap" = "$lmap" ] && ok "vcs ids+statuses == ledger VC subset (no drift)" || no "vcs/ledger drift under attack"

echo "=== 3. FIREWALL: a LYING solver cannot forge kernel evidence ==="
TMP="$(mktemp -d)"
printf '#!/bin/sh\necho unsat\n' > "$TMP/z3"; chmod +x "$TMP/z3"   # claims everything proved
liar="$(PATH="$TMP:$PATH" "$COMPILER" "$F" --report vcs --smt --json 2>/dev/null)"
printf '%s' "$liar" | python3 -c "
import json,sys
d=json.load(sys.stdin); byid={v['id']:v for v in d['vcs']}
ovf=byid.get('rt.unsafe_mul#ovf0',{})
# the lying solver can only ever reach solver_trusted, never a kernel class ...
a = ovf.get('status')=='solver_trusted' and ovf.get('engine')=='smt:z3'
# ... and the kernel-owned (omega) VCs it was NOT asked about stay unproven, never solver-classed.
b = byid['rt.oob#bounds0']['status']=='unproven' and byid['rt.divzero#div0']['status']=='unproven'
# nothing in the red-team module is kernel-proved by the solver run.
c = all(v['status']!='proved_by_kernel_decision' or v['kind']=='vacuity' for v in d['vcs'] if v['id'].startswith('rt.'))
sys.exit(0 if (a and b and c) else 1)" \
  && ok "lying 'unsat' solver → solver_trusted only; kernel VCs untouched" \
  || no "a lying solver breached the evidence-class firewall"
printf '#!/bin/sh\necho sat\necho "(model (define-fun a () Int 100000) (define-fun b () Int 100000))"\n' > "$TMP/z3"
sat="$(PATH="$TMP:$PATH" "$COMPILER" "$F" --report vcs --smt --json 2>/dev/null | python3 -c "import json,sys;d=json.load(sys.stdin);print(next((v['status'] for v in d['vcs'] if v['id']=='rt.unsafe_mul#ovf0'),'?'))")"
[ "$sat" = "counterexample" ] && ok "lying 'sat' solver → counterexample, never a proof" || no "sat result mis-classified ($sat)"

echo "=== 4. POLICY catches every escape (E0613 vacuous, E0614 assume, E0615 solver) ==="
# fresh HONEST-shaped solver: `unsat` so the nonlinear VC is solver_trusted and
# the solver-evidence policy must reject it (section 3 left z3 as the sat liar).
printf '#!/bin/sh\necho unsat\n' > "$TMP/z3"; chmod +x "$TMP/z3"
out="$(cd "$DIR" && PATH="$TMP:$PATH" "$COMPILER" build 2>&1)"; rc=$?
rm -rf "$TMP" "$DIR/obligation_redteam"
[ "$rc" -ne 0 ] && ok "strict release profile rejects the project (exit $rc)" || no "release profile accepted the red-team project"
for code in E0613 E0614 E0615; do
  grep -qF <<<"$out" "$code" && ok "policy raised $code" || no "policy did not raise $code"
done

echo ""
echo "OBLIGATION-REDTEAM: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
