#!/usr/bin/env bash
# BUG 063: "I DO NOT KNOW THIS TYPE" MUST NOT BE RECORDED AS "THIS CALLBACK NEEDS NO AUTHORITY".
#
# `peekExprType` is a cheap syntactic peek and answers `.placeholder` for what it does not
# handle. The cap-variable inference consumer mapped that onto `CapSet.empty`. Absence of
# information stored as a positive fact — and not a conservative approximation of the
# missing analysis, but the opposite one.
#
# The symptom was a wrong ACCUSATION rather than a wrong answer. A capability-polymorphic
# combinator accepted a callback written as a bare name and rejected every DERIVED form —
# struct field, call result, array element, parenthesised — with
#
#   E0220: type mismatch in argument 'f' of 'apply':
#          expected fn(i64) with() -> i64, got fn(i64) with(Console) -> i64
#
# `with()` is not something the program wrote. DECISIONS.md recommends a struct of
# function pointers as THE answer for pluggable interfaces, so the rejected form is the
# intended use.
#
# WHY IT WAS WORTH FIXING BEFORE IT BIT. With `C := {}` fabricated, `missingCaps` found
# nothing missing, so the AUTHORITY check passed; the program survived only because
# `expectTy` compares fn types for equality and happened to sit downstream. Relax that
# comparison to subsetting — the natural direction for passing a low-authority callback
# where a high-authority one is expected — and the only thing rejecting these programs
# disappears, turning a rejected-valid-program into an authority hole.
#
# TWO SITES, and the second is the one that matters. The function-call path in
# `Check.lean` and the method-call path in `CheckHelpers.lean` carried byte-identical
# copies of this defect; the first fix took only one, and the absence-as-positive-fact
# sweep found the other. The capability-polymorphic combinators that actually exist in
# `std` — `Set::fold`, `for_each`, `with_value` — are METHODS, so real callers reach the
# path that was missed. Both are asserted below so neither can regress alone.
#
# THE ERROR CODE IS THE ASSERTION, not merely that something failed. E0242 means the
# capability leg refused. E0220 means the fallback is back and type equality is doing the
# work. A gate that only checked "does it fail" would pass either way, and would go on
# passing after the subset relaxation — testing `expectTy`, not capabilities.
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
CC="$ROOT_DIR/.lake/build/bin/concrete"
FIX="$ROOT_DIR/tests/regressions/cap_inference"


PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

[ -x "$CC" ] || { echo "FATAL: compiler not built at $CC" >&2; exit 2; }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
# Hang watchdog (scripts/tests/lib/bounded.sh): a timeout is re-run once alone with twice the
# budget; a second timeout gives no output, prints INCOMPLETE and returns 124, and
# bounded_gate_status (the last line) then fails the gate whatever its checks concluded.
BOUNDED_PARENT="$TMP"
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/bounded.sh"
bounded_watchdog 300

echo "=== every derived fn-pointer form reaches a cap-polymorphic parameter ==="
d="$FIX/derived_forms"
out="$(cd "$d" && "${TO[@]}" "$CC" check . 2>&1)"
if [ -z "$(printf '%s' "$out" | grep -E 'error\[')" ]; then
  ok "struct field, call result, array element, parenthesised and bare name all check"
else
  no "a derived fn-pointer form is still rejected"
  printf '%s\n' "$out" | grep -E 'error\[' | awk 'NR<=3' | sed 's/^/       /'
fi

# Behaviour, not just acceptance: each of the six calls must actually run. 2*5 + 2 = 12.
if (cd "$d" && "${TO[@]}" "$CC" build . -o "$TMP/derived" >/dev/null 2>&1); then
  "$TMP/derived" >/dev/null 2>&1; rc=$?
  if [ "$rc" -eq 12 ]; then
    ok "all six calls dispatch correctly (exit 12)"
  else
    no "compiled but produced $rc, not 12 — a call is dispatching to the wrong target"
  fi
else
  no "derived_forms does not build"
fi

echo "=== a capability-FREE callback still works through the same combinator ==="
# The old fallback answered `empty` for everything, so it was accidentally right here.
# Fixing only the capability-carrying forms would break this one.
if printf '%s' "$out" | grep -qE 'error\['; then
  no "cannot assess: derived_forms does not check"
else
  ok "the empty-capset form is unaffected (it is the same fixture's last call)"
fi

echo "=== what remains unknowable SAYS SO, and says it in the right register ==="
u="$FIX/uninferable"
uout="$(cd "$u" && "${TO[@]}" "$CC" check . 2>&1)"
if printf '%s' "$uout" | grep -q "E0242"; then
  ok "a generic callee's return type yields E0242 cannotInferCapVariable"
else
  no "the uninferable case does not report E0242"
  printf '%s\n' "$uout" | grep -E 'error\[' | awk 'NR<=2' | sed 's/^/       /'
fi
if printf '%s' "$uout" | grep -q "capability variable 'C'"; then
  ok "the diagnostic NAMES the variable it could not infer"
else
  no "the diagnostic does not name the cap variable — it points somewhere unhelpful"
fi

echo "=== CONTROL: the refusal comes from the capability leg, not from type equality ==="
# This is the leg that would silently stop working if the fallback returned. E0220 here
# would mean a fabricated empty binding passed the authority check and `expectTy` caught
# the collision afterwards — the pre-fix behaviour, and the one that survives only until
# fn-type comparison is relaxed to subsetting.
if printf '%s' "$uout" | grep -q "E0220"; then
  no "E0220 is present: an empty binding was fabricated and type equality is doing the work"
else
  ok "no E0220 — nothing invented a capability set to compare against"
fi

echo "=== the METHOD path, which the first fix missed ==="
dm="$FIX/derived_forms_method"
mout="$(cd "$dm" && "${TO[@]}" "$CC" check . 2>&1)"
if [ -z "$(printf '%s' "$mout" | grep -E 'error\[')" ]; then
  ok "every derived form reaches a cap-polymorphic METHOD parameter"
else
  no "a derived form is still rejected on the method path"
  printf '%s\n' "$mout" | grep -E 'error\[' | awk 'NR<=3' | sed 's/^/       /'
fi
if (cd "$dm" && "${TO[@]}" "$CC" build . -o "$TMP/derivedm" >/dev/null 2>&1); then
  "$TMP/derivedm" >/dev/null 2>&1; rcm=$?
  [ "$rcm" -eq 12 ] && ok "all six method calls dispatch correctly (exit 12)" \
                    || no "method fixture produced $rcm, not 12"
else
  no "derived_forms_method does not build"
fi
um="$FIX/uninferable_method"
umout="$(cd "$um" && "${TO[@]}" "$CC" check . 2>&1)"
if printf '%s' "$umout" | grep -q "E0242"; then
  ok "the method path reports E0242 for what it cannot infer"
else
  no "the method path does not report E0242 — it may still fabricate an empty capset"
  printf '%s\n' "$umout" | grep -E 'error\[' | awk 'NR<=2' | sed 's/^/       /'
fi
if printf '%s' "$umout" | grep -q "E0220"; then
  no "E0220 on the method path: an empty binding was fabricated there"
else
  ok "no E0220 on the method path either"
fi

echo
echo "CAP-INFERENCE: PASS=$PASS FAIL=$FAIL"
bounded_gate_status && [ "$FAIL" -eq 0 ]
