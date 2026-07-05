#!/usr/bin/env bash
# Overflow migration gate (ROADMAP Phase 3 #7) — three discharge routes.
#
# Phase 3 #7 routes no-overflow obligations (only from `#[overflow_checked]`)
# through the unified `scopedWalk` collector. Overflow owns THREE discharge
# routes: omega (linear), interval+bv_decide (kernel-checked, non-negative
# bounded), and opt-in external SMT (signed nonlinear, `--smt` only). The
# migration feeds enclosing guards into the in-scope facts, so proofs can only
# get stronger. The invariant this gate protects:
#
#   unified scoped facts may STRENGTHEN overflow proofs, but must not let SMT
#   blur into kernel evidence, nor let stale bounds prove arithmetic.
#
# Verified against the pre-#7 binary: every overflow VC across the SMT corpus is
# byte-identical (8 files), and the SMT emission set is unchanged. This gate
# freezes the migration improvements, the negative controls, and the trust
# boundary — runnable in CI with a fake solver (no real Z3 needed).

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
ADV="examples/contract_negatives/overflow_scope_adversarial/src/main.con"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# ck <label> <extra-args> <python-bool-expr with ovf()/eng()/status()>
ck(){ local label="$1" args="$2" expr="$3"
  "$COMPILER" "$ADV" --report vcs $args --json 2>/dev/null | python3 -c "
import json,sys
d=json.load(sys.stdin)
byid={v['id']:v for v in d['vcs']}
def has(i): return i in byid
def st(i): return byid[i]['status']
def eng(i): return byid[i].get('engine','')
def hassmt(i): return bool(byid[i].get('smt'))
def anysolver(): return any(v['status'] in ('solver_trusted','counterexample') for v in d['vcs'])
sys.exit(0 if ($expr) else 1)
" 2>/dev/null && ok "$label" || no "$label"; }

echo "=== migration improvements: guards now feed the kernel discharge ==="
ck "linear_guarded: guard ⇒ proved by omega"            "" "st('adv.linear_guarded#ovf0')=='proved_by_kernel_decision' and eng('adv.linear_guarded#ovf0')=='omega'"
ck "product_guarded: guard ⇒ proved by bv_decide"       "" "st('adv.product_guarded#ovf0')=='proved_by_kernel_decision' and eng('adv.product_guarded#ovf0')=='bv_decide'"

echo "=== negative controls: stale / missing bounds never falsely green ==="
ck "stale_bound a*b after a=a+1 ⇒ unproven"             "" "st('adv.stale_bound#ovf1')=='unproven'"
ck "no_lower_bound: interval needs both bounds ⇒ unproven" "" "st('adv.no_lower_bound#ovf0')=='unproven'"
ck "weakened: product can exceed i32 ⇒ unproven (no false green)" "" "st('adv.weakened#ovf0')=='unproven'"

echo "=== trust boundary: default report is solver-clean ==="
ck "no solver_trusted/counterexample without --smt"     "" "not anysolver()"
ck "signed nonlinear product unproven by kernel tiers"  "" "st('adv.signed_product#ovf0')=='unproven' and not hassmt('adv.signed_product#ovf0')"

echo "=== trust boundary: SMT emission only for kernel-unproved VCs (no overreach) ==="
EMIT="$("$COMPILER" "$ADV" --report vcs --emit-smt 2>&1 | grep -E '^;; ==== ' | sort -u)"
grep <<<"$EMIT" -q 'signed_product#ovf0'   && ! grep <<<"$EMIT" -q 'linear_guarded'   \
  && ! grep <<<"$EMIT" -q 'product_guarded' \
  && ok "queries emitted for signed_product but NOT for omega/bv-owned VCs" \
  || no "SMT emission overreached onto a kernel-owned VC"

echo "=== trust boundary: under --smt, a solver may NOT overwrite kernel evidence ==="
TMP="$(mktemp -d)"; printf '#!/bin/sh\necho unsat\n' > "$TMP/z3"; chmod +x "$TMP/z3"
SMTJSON="$(PATH="$TMP:$PATH" "$COMPILER" "$ADV" --report vcs --smt --json 2>/dev/null)"
rm -rf "$TMP"
printf '%s' "$SMTJSON" | python3 -c "
import json,sys
d=json.load(sys.stdin); byid={v['id']:v for v in d['vcs']}
kernel_kept = all(byid[i]['status']=='proved_by_kernel_decision' and byid[i]['engine'] in ('omega','bv_decide')
                  for i in ('adv.linear_guarded#ovf0','adv.product_guarded#ovf0','adv.stale_bound#ovf0'))
solver_only = byid['adv.signed_product#ovf0']['engine']=='smt:z3'
sys.exit(0 if (kernel_kept and solver_only) else 1)
" 2>/dev/null \
  && ok "kernel-proved VCs stay proved_by_kernel_decision; solver only takes the residue" \
  || no "a solver overwrote kernel evidence under --smt"

echo ""
echo "OVERFLOW-MIGRATION: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
