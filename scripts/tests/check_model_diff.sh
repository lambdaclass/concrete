#!/usr/bin/env bash
# MODEL VS RUNTIME (bug 080) — the extracted PExpr model and the interpreter compute the same values.
#
# A proof is about the extracted model; the program runs Core. Bug 080 lived in the gap: `PExpr.cast`
# was the identity while the runtime wrapped, so `x as u8` was 300 in the model and 44 in the binary,
# and a theorem about the model was a theorem about a different function. `--report model-diff`
# evaluates both, in process, on boundary inputs of every scalar-signature extracted function.
#
#   C  the corpus (every example and std) has ZERO value mismatches, over a non-empty population;
#   V  every row's verdict is RE-DERIVED here from its model/runtime columns, so a comparator that
#      always answers `agree` cannot pass;
#   F  the bug 080 fixture: narrowing (300 -> 44), sign change and widening all agree, and the 300 row
#      shows the WRAPPED value on both sides.
#   E  the bug 084 fixture: match-arm binders bind BY POSITION in the model as in Elab, the
#      interpreter and the binary. Every row agrees, and the pinned rows carry the POSITIONAL
#      value — a by-name model reads 0 for `Full { v }`, 100 for `Mk { b, a }` and x for
#      `Mk { _, a }`, so each pin fails under the old binding. The compiled binary is run too
#      (exit 22 = 5+5+5+7), so "runtime" is not only the interpreter.
#   T  the bug 086 fixture: a discarded `e;` or a return out of a non-tail expression is not
#      extracted; a tail match/if whose arms return agrees.
#
# Sampling, not proof: agreement on boundaries is evidence, not a theorem (R-0460 owns the theorems).
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
BIN="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$BIN" ] || { echo "error: build first ($BIN missing)" >&2; exit 2; }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

echo "=== C/V: the corpus ==="
: > "$TMP/rows"
for f in $(ls examples/*/src/main.con examples/*/*/src/main.con 2>/dev/null) std/src/lib.con; do
  "$BIN" "$f" --report model-diff 2>/dev/null | grep -vE '^MODEL-DIFF|^$' >> "$TMP/rows" || true
done
N="$(grep -c . "$TMP/rows" || true)"
FNS="$(cut -f2 "$TMP/rows" | sort -u | grep -c . || true)"
CLAIMED="$(awk -F'\t' '$3 == "claimed"' "$TMP/rows" | cut -f2 | sort -u | grep -c . || true)"
[ "$N" -gt 1000 ] && [ "$FNS" -gt 50 ] && [ "$CLAIMED" -gt 5 ] \
  && ok "C: $FNS functions ($CLAIMED claimed) over $N inputs were compared" \
  || no "C: the population is too small to mean anything ($FNS functions, $CLAIMED claimed, $N inputs)"
MM="$(grep -c '^MISMATCH' "$TMP/rows" || true)"
[ "$MM" -eq 0 ] && ok "C: no input where the model and the runtime produce different values" \
  || no "C: $MM model/runtime value mismatches: $(grep -m3 '^MISMATCH' "$TMP/rows" | tr '\t' ' ' | tr '\n' '|')"
BAD="$(awk -F'\t' '{m=$5; r=$6; sub(/^model=/,"",m); sub(/^runtime=/,"",r);
  want = (m!="—" && r!="trap") ? (m==r ? "agree" : "MISMATCH") : (m!="—" ? "partial" : (r!="trap" ? "model_stuck" : "agree"));
  if (want != $1) print}' "$TMP/rows" | head -3)"
[ -z "$BAD" ] && ok "V: every verdict re-derives from its model and runtime columns" \
  || no "V: verdicts disagree with their own columns: $(echo "$BAD" | tr '\t' ' ' | tr '\n' '|')"

echo ""
echo "=== F: the bug 080 fixture ==="
F="tests/regressions/bug080_narrow_cast/src/main.con"
"$BIN" "$F" --report model-diff > "$TMP/f" 2>&1
grep -qP '^agree\tmain\.narrow\tunclaimed\t\(300\)\tmodel=44\truntime=44$' "$TMP/f" \
  && ok "F: narrow(300) is 44 in the model and at runtime" \
  || no "F: narrow(300) does not read model=44 runtime=44: $(grep -P 'narrow\t.*\(300\)' "$TMP/f" | tr '\t' ' ')"
for fn in narrow to_signed widen; do
  r="$(grep -P "\tmain\.$fn\t" "$TMP/f")"
  if [ -n "$r" ] && ! grep -qv '^agree' <<<"$r"; then ok "F: every input of $fn agrees ($(grep -c . <<<"$r"))"
  else no "F: $fn has a non-agreeing or missing row: $(grep -v '^agree' <<<"$r" | head -1 | tr '\t' ' ')"; fi
done

echo ""
echo "=== E: the bug 084 fixture ==="
E="tests/regressions/bug084_enum_arm_binding/repro.con"
"$BIN" "$E" --report model-diff > "$TMP/e" 2>&1
pin(){ # fn input value why
  grep -qP "^agree\tmain\.$1\tunclaimed\t\($2\)\tmodel=$3\truntime=$3\$" "$TMP/e" \
    && ok "E: $1($2) is $3 in the model and at runtime ($4)" \
    || no "E: $1($2) does not read model=$3 runtime=$3 ($4): $(grep -P "\tmain\.$1\t.*\($2\)" "$TMP/e" | tr '\t' ' ')"
}
pin renamed 300 300 "binder v takes field value; by name it was 0"
pin swapped 300 300 "{ b, a }: b takes field a; by name it was 100"
pin swapped -1 -1 "{ b, a }: b takes field a; by name it was 100"
pin skipped 300 7 "{ _, a }: a takes field b; by name it was 300"
pin same 300 300 "binder name = field name: the control"
for fn in renamed same swapped skipped; do
  r="$(grep -P "\tmain\.$fn\t" "$TMP/e")"
  if [ -n "$r" ] && ! grep -qv '^agree' <<<"$r"; then ok "E: every input of $fn agrees ($(grep -c . <<<"$r"))"
  else no "E: $fn has a non-agreeing or missing row: $(grep -v '^agree' <<<"$r" | head -1 | tr '\t' ' ')"; fi
done
if "$BIN" "$E" -o "$TMP/e.bin" > /dev/null 2>&1; then
  "$TMP/e.bin"; rc=$?
  [ "$rc" -eq 22 ] && ok "E: the compiled binary binds positionally too (exit 22)" \
    || no "E: the compiled binary exits $rc, expected 22 (5+5+5+7)"
else no "E: the fixture does not compile"; fi
echo "=== T: bug 086 — a discarded statement is not the value; a nested return exits ==="
want(){ grep -qP "^agree\tmain\.$2\t[a-z]+\t\($3\)\tmodel=$4\truntime=$4\$" "$1" \
  && ok "$5: $2($3) is $4 in the model and at runtime" \
  || no "$5: $2($3) does not read model=$4 runtime=$4: $(grep -P "\tmain\.$2\t.*\($3\)" "$1" | tr '\t' ' ')"; }
allagree(){ r="$(grep -P "\tmain\.$2\t" "$1")"
  if [ -n "$r" ] && ! grep -qvE '^(agree|partial)' <<<"$r"; then ok "$3: every input of $2 agrees ($(grep -c . <<<"$r"))"
  else no "$3: $2 has a disagreeing or missing row: $(grep -vE '^(agree|partial)' <<<"$r" | head -1 | tr '\t' ' ')"; fi; }
T="tests/regressions/bug086_discarded_tail/repro.con"
"$BIN" "$T" --report model-diff > "$TMP/t" 2>&1
# Not modelable exactly (a call the model cannot see; a return out of a non-tail expression):
# they must not be extracted at all, so no row can claim a value.
for fn in g letret argret; do
  grep -qP "\tmain\.$fn\t" "$TMP/t" && no "T: $fn is extracted: $(grep -m1 -P "\tmain\.$fn\t" "$TMP/t" | tr '\t' ' ')" \
    || ok "T: $fn is not extracted"
done
want "$TMP/t" tailmatch 0 5 T
want "$TMP/t" tailmatch 1 6 T
want "$TMP/t" tailif 1 3 T
want "$TMP/t" tailif 0 4 T
for fn in tailmatch tailif; do allagree "$TMP/t" "$fn" T; done

echo ""
echo "MODEL-DIFF-GATE: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
