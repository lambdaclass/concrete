#!/usr/bin/env bash
# External-SMT teaching group gate (ROADMAP Phase 2 #16).
#
# Teaches when SMT is useful and when Concrete REFUSES it. Pins:
#   - the useful SMT case reports solver name/version/smtlib-hash/replay (--smt)
#   - kernel-owned facts (linear, bounded bv) emit NO SMT query
#   - out-of-fragment VCs are VISIBLE (unproven), not dropped, and emit no query
#   - counterexamples remain non-proofs
#   - default reports carry no SMT data unless --smt is passed

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
NL="examples/smt/nonlinear_overflow/src/main.con"
KP="examples/smt/teaching/kernel_preferred.con"
UN="examples/smt/teaching/unsupported_theory.con"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

echo "=== kernel_preferred: omega/bv own it → NO SMT query ==="
printf '%s' "$("$COMPILER" "$KP" --report vcs --emit-smt 2>/dev/null)" | grep -qiF "no SMT-eligible" \
  && ok "linear + bounded bv product → no SMT query" || no "kernel-owned facts produced an SMT query"
kv="$("$COMPILER" "$KP" --report vcs --json 2>/dev/null)"
printf '%s' "$kv" | python3 -c "
import json,sys
d=json.load(sys.stdin)
have_omega=any(v['status']=='proved_by_kernel_decision' and v['engine']=='omega' for v in d['vcs'])
have_bv=any(v['status']=='proved_by_kernel_decision' and v['engine']=='bv_decide' for v in d['vcs'])
sys.exit(0 if have_omega and have_bv else 1)" \
  && ok "facts are proved_by_kernel_decision (omega AND bv_decide), not solver_trusted" || no "expected omega+bv kernel proofs"

echo "=== unsupported_theory: out-of-fragment VC is VISIBLE (unproven), no query ==="
printf '%s' "$("$COMPILER" "$UN" --report vcs --emit-smt 2>/dev/null)" | grep -qiF "no SMT-eligible" \
  && ok "nonlinear array-bounds index → no SMT query" || no "out-of-fragment VC produced an SMT query"
printf '%s' "$("$COMPILER" "$UN" --report vcs --json 2>/dev/null)" | python3 -c "
import json,sys
d=json.load(sys.stdin)
# the obligation is shown (not dropped) and honestly unproven — never a proof/solver class.
v=next((v for v in d['vcs'] if v['kind']=='array_bounds'), None)
sys.exit(0 if v and v['status']=='unproven' else 1)" \
  && ok "obligation is visible as unproven (not silently dropped)" || no "out-of-fragment obligation missing or mis-statused"

echo "=== default reports carry no SMT data unless --smt ==="
for f in "$NL" "$KP" "$UN"; do
  printf '%s' "$("$COMPILER" "$f" --report vcs --json 2>/dev/null)" | python3 -c "
import json,sys
d=json.load(sys.stdin)
sys.exit(0 if all(v.get('smt') is None for v in d['vcs']) else 1)" \
    && ok "no SMT data by default: $(basename "$f")" || no "SMT data leaked by default in $(basename "$f")"
done

if command -v z3 >/dev/null 2>&1; then
  echo "=== useful SMT case reports solver provenance + replay (--smt) ==="
  uj="$("$COMPILER" "$NL" --report vcs --smt --json 2>/dev/null)"
  printf '%s' "$uj" | python3 -c "
import json,sys
d=json.load(sys.stdin)
v=next((v for v in d['vcs'] if v['function'].endswith('.scale') and v.get('smt')), None)
s=v['smt'] if v else {}
ok = (v and v['status']=='solver_trusted'
      and s.get('solver','').startswith('z3 ') and bool(s.get('smtlib_sha'))
      and 'replay' in s and s.get('lean_replay') is not None)
sys.exit(0 if ok else 1)" \
    && ok "scale → solver_trusted with solver/version/smtlib-sha/replay/lean_replay" || no "missing solver provenance"
  echo "=== counterexample remains a non-proof ==="
  printf '%s' "$uj" | python3 -c "
import json,sys
d=json.load(sys.stdin)
v=next((v for v in d['vcs'] if v['function'].endswith('.scale_unbounded') and v.get('smt')), None)
sys.exit(0 if v and v['status']=='counterexample' else 1)" \
    && ok "scale_unbounded → counterexample (non-proof, with source model)" || no "expected a counterexample"
else
  echo "  skip solver-provenance + counterexample (z3 not on PATH)"
fi

echo ""
echo "SMT-EXAMPLES: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
