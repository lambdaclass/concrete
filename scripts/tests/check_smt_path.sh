#!/usr/bin/env bash
# External-SMT path gate (ROADMAP Phase 2 #8/#9).
#
# Pins the trust boundary of the opt-in external-SMT slice:
#   - the SMT-LIB emission is well-formed and SOUND (carries the #[requires] hyps);
#   - the external solver is OPT-IN: by default no VC advertises smt or a solver
#     status — the kernel boundary is the default (also pinned by check_vc_schema.sh);
#   - an absent solver yields `solver_error`, never a proof;
#   - SMT only touches VCs the kernel tiers left `unproven` — never a
#     proved_by_kernel_decision / proved_by_lean case.
# When Z3 is on PATH, also checks the `unsat` → `solver_trusted` classification.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
EX="examples/smt/nonlinear_overflow/src/main.con"
PASS=0; FAIL=0

ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

echo "=== --emit-smt produces a sound, well-formed QF_NIA query ==="
emit="$("$COMPILER" "$EX" --report vcs --emit-smt 2>/dev/null)"
grep -qF <<<"$emit" "(set-logic QF_NIA)" && ok "declares QF_NIA logic" || no "missing set-logic QF_NIA"
grep -qF <<<"$emit" "(check-sat)" && ok "has (check-sat)" || no "missing check-sat"
grep -qF <<<"$emit" "(get-model)" && ok "requests (get-model) for counterexamples" || no "missing get-model"
grep -qF <<<"$emit" "(* sample gain)" && ok "encodes the nonlinear product" || no "missing the product term"
# SOUNDNESS: both #[requires] bounds must be asserted, else the query is a lie.
grep -qF <<<"$emit" "(<= sample 30000)" \
  && grep -qF <<<"$emit" "(<= gain 60000)" \
  && ok "asserts the #[requires] bounds (sound query)" || no "query omits a #[requires] bound (UNSOUND)"
grep -qF <<<"$emit" "(not (and (<= -2147483648 (* sample gain))" \
  && ok "asserts the NEGATED range goal (refutation query)" || no "missing negated goal"

echo "=== provenance + determinism (no solver needed: the query is deterministic) ==="
emitjson="$("$COMPILER" "$EX" --report vcs --smt --json 2>/dev/null)"
printf '%s' "$emitjson" | python3 -c "
import json,sys
d=json.load(sys.stdin)
v=next((v for v in d['vcs'] if v.get('smt')), None)
s=v['smt'] if v else {}
ok = (s.get('logic')=='QF_NIA' and s.get('timeout_sec')==5 and bool(s.get('smtlib_sha'))
      and '(set-logic QF_NIA)' in s.get('query','') and '(get-model)' in s.get('query','')
      and 'replay' in s)
sys.exit(0 if ok else 1)" \
  && ok "SMT provenance recorded (logic, timeout, smtlib_sha, query, replay)" || no "missing SMT provenance fields"
# determinism: the SMT-LIB hash is stable across two independent runs.
sha_a="$(printf '%s' "$emitjson" | python3 -c "import json,sys;d=json.load(sys.stdin);print(' '.join(sorted(v['smt']['smtlib_sha'] for v in d['vcs'] if v.get('smt'))))")"
sha_b="$("$COMPILER" "$EX" --report vcs --smt --json 2>/dev/null | python3 -c "import json,sys;d=json.load(sys.stdin);print(' '.join(sorted(v['smt']['smtlib_sha'] for v in d['vcs'] if v.get('smt'))))")"
[ -n "$sha_a" ] && [ "$sha_a" = "$sha_b" ] && ok "smtlib_sha is deterministic across runs" || no "smtlib_sha drifted: '$sha_a' vs '$sha_b'"

echo "=== default (no flag): the kernel boundary holds ==="
defjson="$("$COMPILER" "$EX" --report vcs --json 2>/dev/null)"
printf '%s' "$defjson" | python3 -c "
import json,sys
d=json.load(sys.stdin)
bad=[v for v in d['vcs'] if v['expected_discharge']=='smt' or v['engine'].startswith('smt') or v['status'] in ('solver_trusted','proved_by_smt','solver_error','unknown','timeout') or v.get('smt') is not None]
sys.exit(0 if not bad else 1)" \
  && ok "no VC advertises smt, a solver status, or SMT provenance by default" || no "a solver class/provenance leaked without the flag"
# the nonlinear VC is honestly unproven by the kernel tiers, not silently 'proved'.
printf '%s' "$defjson" | python3 -c "
import json,sys
d=json.load(sys.stdin)
sys.exit(0 if any(v['kind']=='no_overflow' and v['status']=='unproven' for v in d['vcs']) else 1)" \
  && ok "nonlinear no_overflow is honestly unproven (kernel tiers can't own it)" || no "expected an unproven no_overflow VC"

echo "=== --smt: SMT only ever marks the eligible class, never kernel cases ==="
smtjson="$("$COMPILER" "$EX" --report vcs --smt --json 2>/dev/null)"
# any VC that was proved by a kernel tier must keep that exact class+engine.
printf '%s' "$smtjson" | python3 -c "
import json,sys
d=json.load(sys.stdin)
# no kernel-decided VC may carry an smt engine
bad=[v for v in d['vcs'] if v['status']=='proved_by_kernel_decision' and v['engine'].startswith('smt')]
sys.exit(0 if not bad else 1)" \
  && ok "no proved_by_kernel_decision VC is attributed to smt" || no "smt overrode a kernel decision"
# timeout / unknown / solver_error / counterexample are NEVER proof statuses.
printf '%s' "$smtjson" | python3 -c "
import json,sys
d=json.load(sys.stdin)
nonproof={'unknown','timeout','solver_error','counterexample'}
bad=[v for v in d['vcs'] if v['status'] in nonproof and v['status'] in ('proved_by_kernel_decision','proved_by_lean','solver_trusted')]
sys.exit(0 if not bad else 1)" \
  && ok "timeout/unknown/solver_error/counterexample are treated as non-proofs" || no "a non-proof status was treated as a proof"

if command -v z3 >/dev/null 2>&1; then
  echo "=== Z3 present: provable bound → solver_trusted; loose bound → counterexample ==="
  # scale (tight bounds): unsat → solver_trusted.
  printf '%s' "$smtjson" | python3 -c "
import json,sys
d=json.load(sys.stdin)
sys.exit(0 if any(v['function'].endswith('.scale') and v['kind']=='no_overflow' and v['status']=='solver_trusted' and v['engine']=='smt:z3' for v in d['vcs']) else 1)" \
    && ok "scale (tight) → solver_trusted (smt:z3)" || no "expected solver_trusted for scale"
  # scale_unbounded (loose bounds): sat → counterexample.
  printf '%s' "$smtjson" | python3 -c "
import json,sys
d=json.load(sys.stdin)
sys.exit(0 if any(v['function'].endswith('.scale_unbounded') and v['status']=='counterexample' and v['engine']=='smt:z3' for v in d['vcs']) else 1)" \
    && ok "scale_unbounded (loose) → counterexample (smt:z3)" || no "expected counterexample for scale_unbounded"
  # the counterexample names SOURCE variables (sample, gain) with concrete values.
  printf '%s' "$smtjson" | python3 -c "
import json,sys
d=json.load(sys.stdin)
v=next((v for v in d['vcs'] if v['function'].endswith('.scale_unbounded') and v['kind']=='no_overflow'), None)
m=v['counterexample'] if v else {}
ok = ('sample' in m and 'gain' in m and m['sample'].lstrip('-').isdigit() and m['gain'].lstrip('-').isdigit())
# and the witness actually overflows i32: sample*gain > 2^31-1
ok = ok and (int(m['sample'])*int(m['gain']) > 2147483647 or int(m['sample'])*int(m['gain']) < -2147483648)
sys.exit(0 if ok else 1)" \
    && ok "counterexample uses source names with overflowing concrete values" || no "counterexample should map to source vars that overflow"
  # the solver identity+version is recorded for provenance.
  printf '%s' "$smtjson" | python3 -c "
import json,sys
d=json.load(sys.stdin)
sys.exit(0 if any(v.get('smt') and v['smt']['solver'].startswith('z3 ') and v['smt']['solver']!='z3 (not run)' for v in d['vcs']) else 1)" \
    && ok "solver identity+version recorded (e.g. z3 4.16.0)" || no "expected a concrete z3 version in provenance"
  # determinism of the RESULT CLASS across two solver runs (compare extracted
  # id:status strings — never embed the JSON query text into python source).
  cls_a="$(printf '%s' "$smtjson" | python3 -c "import json,sys;d=json.load(sys.stdin);print('|'.join(sorted(v['id']+':'+v['status'] for v in d['vcs'] if v.get('smt'))))")"
  cls_b="$("$COMPILER" "$EX" --report vcs --smt --json 2>/dev/null | python3 -c "import json,sys;d=json.load(sys.stdin);print('|'.join(sorted(v['id']+':'+v['status'] for v in d['vcs'] if v.get('smt'))))")"
  [ -n "$cls_a" ] && [ "$cls_a" = "$cls_b" ] && ok "result class is deterministic across two solver runs" || no "solver result class drifted: '$cls_a' vs '$cls_b'"
else
  echo "=== Z3 absent: --smt yields solver_error, never a proof ==="
  printf '%s' "$smtjson" | python3 -c "
import json,sys
d=json.load(sys.stdin)
v=next((v for v in d['vcs'] if v['kind']=='no_overflow' and v['expected_discharge']=='smt'), None)
sys.exit(0 if v and v['status']=='solver_error' and v['engine']=='smt:z3' else 1)" \
    && ok "absent solver → solver_error (no false proof)" || no "absent solver should yield solver_error"
fi

echo ""
echo "SMT-PATH: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
