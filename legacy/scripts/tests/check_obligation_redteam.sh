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
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/fresh.sh"
require_fresh_binary || exit 1
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
# THE CONSTANT-VERDICT PATH, which had no live case anywhere. Every attack in THIS fixture uses
# VARIABLE operands, so `cEvalInt` returns none and the `some k` branch never runs — two mutations
# survived a full campaign on that gap, replacing both constant verdicts with `decide (k == k)`.
#
# The violating constants live in their own fixture, `contract_negatives/const_violation`, and NOT
# here: a decidable violation (E0901 for a divisor proven zero) is a hard compile error, which would stop `concrete build`
# before the policy stage this gate asserts E0613/E0614/E0615 at. Measured — adding them here turned
# 13/0 into 13/5.
#
# `counterexample` rather than `unproven`: the compiler can DECIDE these, so reporting mere absence
# of proof would understate what it knows and leave a weaker control.
cvled(){ "$COMPILER" examples/contract_negatives/const_violation/src/main.con --report obligation-ledger --json 2>/dev/null; }
cvck(){ local label="$1" expr="$2"
  cvled | python3 -c "
import json,sys
d=json.load(sys.stdin); byid={o['id']:o for o in d['obligations']}
def st(i): return byid[i]['status'] if i in byid else '<absent>'
sys.exit(0 if ($expr) else 1)" 2>/dev/null && ok "$label" || no "$label"; }
cvck "division by a LITERAL zero → counterexample"  "st('cv.divzero_const#div0')=='counterexample'"
cvck "LITERAL out-of-range index → counterexample"  "st('cv.oob_const#bounds0')=='counterexample'"
# ...and the violation is REPORTED as a hard error, not merely recorded in the ledger. Without this
# the mutation could leave the ledger honest while the compiler shipped the program.
cv_out="$("$COMPILER" examples/contract_negatives/const_violation/src/main.con -o /dev/null 2>&1 || true)"
grep -qF 'E0901' <<<"$cv_out" && grep -qF 'divisor 0 is always zero' <<<"$cv_out" \
  && ok "a decidable violation is refused at compile time with E0901" \
  || no "no E0901 for a literal-zero divisor — a proven violation would compile"
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
# A lying `sat` solver. UPDATED 2026-08-04, and the change is a strengthening rather than an
# accommodation, so the reasoning is here rather than in a commit message.
#
# This used to assert `sat` -> `counterexample`. That expectation predates SMT verdicts being
# gated on lowering validation (`gateSmtOnValidation`). It meant the system BELIEVED a lying
# solver's model far enough to mark the obligation refuted — bad in the opposite direction
# from a false proof: a broken or hostile solver could brand correct programs as violating,
# using a model nobody checked.
#
# Now the same lying solver also lies on the AGREEMENT queries (whose expected answer is
# always `unsat`), so validation fails, the verdict is discarded, and the obligation stays
# `unproven`. A verdict on an unvalidated rendering is not evidence about the obligation —
# in EITHER direction. The lie is caught upstream of classification.
printf '#!/bin/sh\necho sat\necho "(model (define-fun a () Int 100000) (define-fun b () Int 100000))"\n' > "$TMP/z3"
satout="$(PATH="$TMP:$PATH" "$COMPILER" "$F" --report vcs --smt 2>&1 >/dev/null || true)"
sat="$(PATH="$TMP:$PATH" "$COMPILER" "$F" --report vcs --smt --json 2>/dev/null | python3 -c "import json,sys;d=json.load(sys.stdin);print(next((v['status'] for v in d['vcs'] if v['id']=='rt.unsafe_mul#ovf0'),'MISSING'))" 2>/dev/null)"
[ "$sat" = "unproven" ] \
  && ok "lying 'sat' solver → verdict DROPPED (never a proof, and never a counterexample either)" \
  || no "lying 'sat' solver produced '$sat' — an unvalidated rendering must yield no verdict"
has_match "DROPPED — rendering not validated" <<<"$satout" \
  && ok "and the drop is reported, not silent" \
  || no "the verdict vanished with no warning — a silent drop is indistinguishable from no solver run"

# THE POSITIVE CASE, without which the assertion above is satisfied by a compiler that simply
# never classifies anything. A REAL solver on a genuinely-violating obligation must still
# produce `counterexample`; if it does not, validation-gating has broken the feature rather
# than hardened it.
if command -v z3 >/dev/null 2>&1; then
  real="$("$COMPILER" "$F" --report vcs --smt --json 2>/dev/null | python3 -c "import json,sys;d=json.load(sys.stdin);print(next((v['status'] for v in d['vcs'] if v['id']=='rt.unsafe_mul#ovf0'),'MISSING'))" 2>/dev/null)"
  [ "$real" = "counterexample" ] \
    && ok "REAL solver on a genuine violation still yields counterexample (gating did not break it)" \
    || no "real solver yields '$real' for a genuinely violating obligation — counterexamples are lost"
else
  echo "  (skip real-solver counterexample check — z3 not on PATH)"
fi

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
