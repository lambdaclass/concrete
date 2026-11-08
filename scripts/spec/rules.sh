#!/usr/bin/env bash
# THE RULE GATE: every registered rule (ConcreteSpec/Spec/Rules.lean) is documented, cites the
# decision row it implements, and has an example it accepts and one it refuses; every rule citation
# in the specification names a registered rule. The checks are scripts/spec/rules.py; the
# registry is read from the built environment by scripts/spec/rules.lean.
#
# NEGATIVE CONTROLS run the same checker on small fixtures and require each failure: a rule with no
# refused example, one whose doc cites no decision row, one registered twice, one naming no
# declaration, a citation of an unregistered rule, a verdict outside an example's doc comment and a
# truncated listing. A fixture that satisfies every check must pass (the positive control).
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/../checks/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
CHECK_NAME=SPEC-RULES
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
finish(){ echo "$CHECK_NAME: PASS=$PASS FAIL=$FAIL"; [ "$FAIL" = 0 ] && exit 0 || exit 1; }
TMP="$(mktemp -d)" || { echo "FATAL: mktemp failed" >&2; exit 2; }
trap 'rm -rf "$TMP"' EXIT
export LC_ALL=C
BOUNDED_PARENT="$TMP"; source scripts/checks/lib/bounded.sh || exit 2
# check BUDGET RULES-JSONL SRC-DIR → the checker's output and status, bounded: a second timeout is
# INCOMPLETE, status 2, which no check below accepts
check() {
  local out rc
  out="$(bounded_run -i /dev/null "$1" -- python3 -I scripts/spec/rules.py "$2" "$3" 2>&1)"; rc=$?
  if bounded_is_incomplete "$rc"; then bounded_summary >&2; echo "error: $CHECK_NAME: the checker timed out (INCOMPLETE)" >&2; return 2; fi
  printf '%s\n' "$out"; return "$rc"
}

command -v lake >/dev/null || { echo "error: $CHECK_NAME needs Lean's lake on PATH (elan)" >&2; exit 2; }
bounded_run -i /dev/null -r 3600 3600 -- lake build ConcreteSpec > "$TMP/build.log" 2>&1 \
  || { tail -20 "$TMP/build.log" >&2; bounded_summary >&2; echo "error: $CHECK_NAME: lake build ConcreteSpec failed or timed out" >&2; exit 2; }

echo "=== the specification ==="
bounded_run -i /dev/null 600 -- lake env lean scripts/spec/rules.lean > "$TMP/rules.jsonl" 2> "$TMP/rules.err" \
  || { cat "$TMP/rules.err" >&2; bounded_summary >&2; echo "error: scripts/spec/rules.lean failed or timed out" >&2; exit 2; }
OUT=$(check 120 "$TMP/rules.jsonl" ConcreteSpec); RC=$?
echo "$OUT"
[ "$RC" = 0 ] && ok "the registry and every rule citation of ConcreteSpec" || no "the specification (rc=$RC)"

echo "=== negative controls ==="
# fixture NAME RULES-JSONL SOURCE → runs the checker on them, sets OUT and RC
fixture() {
  mkdir -p "$TMP/$1"; printf '%s\n' "$2" > "$TMP/$1.jsonl"; printf '%s\n' "$3" > "$TMP/$1/A.lean"
  OUT=$(check 60 "$TMP/$1.jsonl" "$TMP/$1"); RC=$?
}
END='{"end":true}'
GOOD='{"name":"R.ok","rev":1,"exists":true,"doc":"The rule (Q9.1)."}'
EXA='/-- Accepted (rule `R.ok`). -/
example : True := trivial

/-- Refused (rule `R.ok`). -/
example : True := trivial'
fixture good "$GOOD
$END" "$EXA"
[ "$RC" = 0 ] && ok "control: a documented rule with both examples passes" || no "control: the good fixture failed: $OUT"
fixture norefused "$GOOD
$END" '/-- Accepted (rule `R.ok`). -/
example : True := trivial'
[ "$RC" = 1 ] && [[ "$OUT" == *"no example marked Refused"* ]] && ok "control: a rule with no refused example fails" \
  || no "control: missing refused example not reported: $OUT"
fixture norow '{"name":"R.ok","rev":1,"exists":true,"doc":"The rule."}
'"$END" "$EXA"
[ "$RC" = 1 ] && [[ "$OUT" == *"cites no decision row"* ]] && ok "control: a rule whose doc cites no decision row fails" \
  || no "control: missing decision row not reported: $OUT"
fixture twice "$GOOD
$GOOD
$END" "$EXA"
[ "$RC" = 1 ] && [[ "$OUT" == *"registered more than once"* ]] && ok "control: a rule registered twice fails" \
  || no "control: duplicate registration not reported: $OUT"
fixture absent '{"name":"R.ok","rev":1,"exists":false,"doc":"The rule (Q9.1)."}
'"$END" "$EXA"
[ "$RC" = 1 ] && [[ "$OUT" == *"names no declaration"* ]] && ok "control: a rule naming no declaration fails" \
  || no "control: missing declaration not reported: $OUT"
fixture unreg "$GOOD
$END" "$EXA
-- see (rule \`R.other\`)"
[ "$RC" = 1 ] && [[ "$OUT" == *"cites unregistered rule R.other"* ]] && ok "control: a citation of an unregistered rule fails" \
  || no "control: unregistered citation not reported: $OUT"
fixture stray "$GOOD
$END" "$EXA
/-- Refused (rule \`R.ok\`). -/
def notAnExample : Nat := 0"
[ "$RC" = 1 ] && [[ "$OUT" == *"outside an example's doc comment"* ]] && ok "control: a verdict on a non-example fails" \
  || no "control: stray verdict not reported: $OUT"
fixture rev0 '{"name":"R.ok","rev":0,"exists":true,"doc":"The rule (Q9.1)."}
'"$END" "$EXA"
[ "$RC" = 1 ] && [[ "$OUT" == *"revision 0"* ]] && ok "control: revision 0 fails" || no "control: revision 0 not reported: $OUT"
fixture trunc "$GOOD" "$EXA"
[ "$RC" = 3 ] && ok "control: a truncated listing is an error" || no "control: truncated listing gave rc=$RC: $OUT"

finish
