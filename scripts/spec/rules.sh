#!/usr/bin/env bash
# THE RULE GATE: every registered rule (ConcreteSpec/Spec/Rules.lean) is documented, cites the
# decision row it implements, and has an example it accepts and one it refuses; every rule citation
# in the specification names a registered rule. The checks are scripts/spec/rules.py; the
# registry is read from the built environment by scripts/spec/rules.lean.
#
# NEGATIVE CONTROLS run the same checker on small fixtures and require each failure: a rule with no
# refused or no accepted example, no doc comment, a doc citing no decision row or one no inventory
# holds, one registered twice, one naming no declaration, revision 0, a citation of an unregistered
# rule (a quoted name too), a verdict outside an example's doc comment or in a nested comment, one
# example claiming both verdicts, a truncated listing, an unlexable source, a missing or empty
# source directory, and a checker that hangs through both bounded attempts. Fixtures that satisfy
# every check must pass (the positive controls, attributed and modified examples included).
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
# run_bounded BUDGET CMD... → CMD's output and status, bounded: a second timeout is INCOMPLETE,
# status 2, which no check below accepts
run_bounded() {
  local out rc budget="$1"; shift
  out="$(bounded_run -i /dev/null -r "$budget" "$budget" -- "$@" 2>&1)"; rc=$?
  if bounded_is_incomplete "$rc"; then echo "error: $CHECK_NAME: timed out twice (INCOMPLETE)" >&2; return 2; fi
  printf '%s\n' "$out"; return "$rc"
}
# check BUDGET RULES-JSONL SRC-DIR ROWS... → the checker
check() { local b="$1"; shift; run_bounded "$b" python3 -I scripts/spec/rules.py "$@" 2>&1; }

command -v lake >/dev/null || { echo "error: $CHECK_NAME needs Lean's lake on PATH (elan)" >&2; exit 2; }
bounded_run -i /dev/null -r 3600 3600 -- lake build ConcreteSpec > "$TMP/build.log" 2>&1 \
  || { tail -20 "$TMP/build.log" >&2; bounded_summary >&2; echo "error: $CHECK_NAME: lake build ConcreteSpec failed or timed out" >&2; exit 2; }

echo "=== the specification ==="
bounded_run -i /dev/null 600 -- lake env lean scripts/spec/rules.lean > "$TMP/rules.jsonl" 2> "$TMP/rules.err" \
  || { cat "$TMP/rules.err" >&2; bounded_summary >&2; echo "error: scripts/spec/rules.lean failed or timed out" >&2; exit 2; }
OUT=$(check 120 "$TMP/rules.jsonl" ConcreteSpec docs/decisions.md ROADMAP.md); RC=$?
echo "$OUT"
[ "$RC" = 0 ] && ok "the registry and every rule citation of ConcreteSpec" || no "the specification (rc=$RC)"

echo "=== negative controls ==="
# fixture NAME RULES-JSONL SOURCE → runs the checker on them, sets OUT and RC
fixture() {
  mkdir -p "$TMP/$1"; printf '%s\n' "$2" > "$TMP/$1.jsonl"; printf '%s\n' "$3" > "$TMP/$1/A.lean"
  OUT=$(check 60 "$TMP/$1.jsonl" "$TMP/$1" "$TMP/rows.md"); RC=$?
}
printf -- '- **Q9.1** A decided row.\nR-0001 is only mentioned here.\n' > "$TMP/rows.md"
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
[ "$RC" = 3 ] && [[ "$OUT" == *"no end record"* ]] && ok "control: a truncated listing is an error" || no "control: truncated listing gave rc=$RC: $OUT"
fixture noaccepted "$GOOD
$END" '/-- Refused (rule `R.ok`). -/
example : True := trivial'
[ "$RC" = 1 ] && [[ "$OUT" == *"no example marked Accepted"* ]] && ok "control: a rule with no accepted example fails" \
  || no "control: missing accepted example not reported: $OUT"
fixture nodoc '{"name":"R.ok","rev":1,"exists":true,"doc":""}
'"$END" "$EXA"
[ "$RC" = 1 ] && [[ "$OUT" == *"has no doc comment"* ]] && ok "control: a rule with no doc comment fails" \
  || no "control: missing doc comment not reported: $OUT"
fixture badrow '{"name":"R.ok","rev":1,"exists":true,"doc":"The rule (Q999.999)."}
'"$END" "$EXA"
[ "$RC" = 1 ] && [[ "$OUT" == *"no existing decision row"* ]] && ok "control: a rule citing a row no inventory has fails" \
  || no "control: nonexistent decision row not reported: $OUT"
fixture both "$GOOD
$END" '/-- Accepted (rule `R.ok`). Refused (rule `R.ok`). -/
example : True := trivial'
[ "$RC" = 1 ] && [[ "$OUT" == *"both accepted and refused"* ]] && ok "control: one example claiming both verdicts fails" \
  || no "control: both verdicts on one example not reported: $OUT"
fixture nested "$GOOD
$END" "$EXA
/-- Refused /- Accepted (rule \`R.ok\`) -/ -/
example : True := trivial"
[ "$RC" = 1 ] && [[ "$OUT" == *"nested comment"* ]] && ok "control: a verdict in a nested comment fails" \
  || no "control: nested-comment verdict not reported: $OUT"
fixture quoted "$GOOD
$END" "$EXA
-- see (rule \`R.«missing rule»\`)"
[ "$RC" = 1 ] && [[ "$OUT" == *"cites unregistered rule R.«missing rule»"* ]] && ok "control: an unregistered quoted name fails" \
  || no "control: quoted unregistered citation not reported: $OUT"
fixture attr "$GOOD
$END" '/-- Accepted (rule `R.ok`). -/
@[simp] example : True := trivial

/-- Refused (rule `R.ok`). -/
noncomputable example : True := trivial'
[ "$RC" = 0 ] && ok "control: examples with an attribute or a modifier count" || no "control: attributed examples refused: $OUT"
fixture unlexable "$GOOD
$END" '/-- Accepted (rule `R.ok`). unterminated'
[ "$RC" = 3 ] && [[ "$OUT" == *"unterminated"* ]] && ok "control: an unlexable source is an error" || no "control: unlexable source gave rc=$RC: $OUT"
fixture literal "$GOOD
$END" "$EXA
def s : String := \"Accepted (rule \`R.ok\`)\""
[ "$RC" = 1 ] && [[ "$OUT" == *"outside an example's doc comment"* ]] && ok "control: a verdict in a string literal fails" \
  || no "control: string-literal verdict not reported: $OUT"
fixture nestedattr "$GOOD
$END" '/-- Accepted (rule `R.ok`). -/
@[simp, scoped [foo] bar] example : True := trivial

/-- Refused (rule `R.ok`). -/
example : True := trivial'
[ "$RC" = 0 ] && ok "control: an attribute with brackets inside counts" || no "control: bracketed attribute refused: $OUT"
fixture mention '{"name":"R.ok","rev":1,"exists":true,"doc":"The rule (R-0001)."}
'"$END" "$EXA"
[ "$RC" = 1 ] && [[ "$OUT" == *"no existing decision row"* ]] && ok "control: a row an inventory only mentions does not count" \
  || no "control: mention-only decision row not reported: $OUT"
fixture fakeattr "$GOOD
$END" '/-- Accepted (rule `R.ok`). -/
@[deprecated "] example" (since := "2026-11-08")]
def a : True := trivial

/-- Refused (rule `R.ok`). -/
@[deprecated "] example" (since := "2026-11-08")]
def b : True := trivial'
[ "$RC" = 1 ] && [[ "$OUT" == *"no example marked"* || "$OUT" == *"outside an example"* ]] \
  && ok "control: an attribute string cannot stand in for an example" || no "control: attribute-string example not reported: $OUT"
fixture strattr "$GOOD
$END" '/-- Accepted (rule `R.ok`). -/
@[deprecated "a ] b" (since := "2026-11-08")] example : True := trivial

/-- Refused (rule `R.ok`). -/
example : True := trivial'
[ "$RC" = 0 ] && ok "control: an attribute holding a string with a bracket counts" || no "control: string attribute refused: $OUT"
fixture backtick "$GOOD
$END" "$EXA
-- see (rule \`R.«missing\`rule»\`)"
[ "$RC" = 1 ] && [[ "$OUT" == *"cites unregistered rule R.«missing\`rule»"* ]] && ok "control: a quoted name holding a backtick is checked" \
  || no "control: backtick quoted citation not reported: $OUT"
fixture malformed "$GOOD
$END" "$EXA
-- see (rule \`R.ok"
[ "$RC" = 1 ] && [[ "$OUT" == *"malformed rule citation"* ]] && ok "control: a malformed citation fails" \
  || no "control: malformed citation not reported: $OUT"
fixture rawattr "$GOOD
$END" "$EXA"'
/-- Accepted (rule `R.ok`). -/
@[deprecated r#"a " ] example"#]
def a : True := trivial'
[ "$RC" = 1 ] && [[ "$OUT" == *"outside an example"* ]] \
  && ok "control: a raw string in an attribute cannot stand in for an example" || no "control: raw-string attribute example not reported: $OUT"
fixture qattr "$GOOD
$END" "$EXA"'
/-- Refused (rule `R.ok`). -/
@[deprecated «x ] example»]
def b : True := trivial'
[ "$RC" = 1 ] && [[ "$OUT" == *"outside an example"* ]] \
  && ok "control: a quoted name in an attribute cannot stand in for an example" || no "control: quoted-name attribute example not reported: $OUT"
fixture cmtattr "$GOOD
$END" '/-- Accepted (rule `R.ok`). -/
@[simp /- ] example -/]
def a : True := trivial

/-- Refused (rule `R.ok`). -/
example : True := trivial'
[ "$RC" = 1 ] && [[ "$OUT" == *"outside an example"* ]] \
  && ok "control: a comment in an attribute cannot stand in for an example" || no "control: comment attribute example not reported: $OUT"
QN='{"name":"R.«a (rule ` b` c»","rev":1,"exists":true,"doc":"The rule (Q9.1)."}'
fixture qname "$QN
$END" '/-- Accepted (rule `R.«a (rule ` b` c»`). -/
example : True := trivial

/-- Refused (rule `R.«a (rule ` b` c»`). -/
example : True := trivial'
[ "$RC" = 0 ] && ok "control: a quoted name holding citation-like text is one citation" || no "control: quoted citation-like name refused: $OUT"
OUT=$(check 60 "$TMP/good.jsonl" "$TMP/no-such-dir" "$TMP/rows.md"); RC=$?
[ "$RC" = 3 ] && [[ "$OUT" == *"not a directory"* ]] && ok "control: a missing source directory is an error" || no "control: missing source dir gave rc=$RC: $OUT"
mkdir -p "$TMP/empty"
OUT=$(check 60 "$TMP/good.jsonl" "$TMP/empty" "$TMP/rows.md"); RC=$?
[ "$RC" = 3 ] && [[ "$OUT" == *"no .lean source"* ]] && ok "control: a source directory with no .lean file is an error" || no "control: empty source dir gave rc=$RC: $OUT"
OUT=$(run_bounded 1 python3 -I -c 'import time; time.sleep(30)' 2>&1); RC=$?
[ "$RC" = 2 ] && [[ "$OUT" == *"timed out twice (INCOMPLETE)"* ]] && ok "control: a checker that hangs through both attempts is INCOMPLETE" || no "control: hung checker gave rc=$RC"

# The transport is a closed schema, not a permissive JSON cache. These controls
# exercise the production reader, including bool-as-int and duplicate-key traps.
for record in 'null' '[]' '{}' \
  '{"name":"R.ok","rev":true,"exists":true,"doc":"Q9.1"}' \
  '{"name":"R.ok","rev":1,"exists":1,"doc":"Q9.1"}' \
  '{"name":"R.ok","rev":1,"exists":true,"doc":"Q9.1","surplus":0}' \
  '{"end":true}'; do
  fixture schema "$record
$END" "$EXA"
  [ "$RC" = 3 ] && [[ "$OUT" == *"malformed registry record"* ]] \
    && ok "control: malformed/surplus record refused: $record" \
    || no "control: schema defect not reported: $OUT"
done
fixture duplicatefield '{"name":"R.ok","name":"R.other","rev":1,"exists":true,"doc":"Q9.1"}
'"$END" "$EXA"
[ "$RC" = 3 ] && [[ "$OUT" == *"duplicate JSON field"* ]] \
  && ok "control: duplicate JSON field refused" || no "control: duplicate field not reported: $OUT"
fixture empty "$END" 'namespace Empty
end Empty'
[ "$RC" = 0 ] && ok "control: complete empty registry passes" || no "control: empty registry refused: $OUT"
bounded_summary
finish
