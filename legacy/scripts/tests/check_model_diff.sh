#!/usr/bin/env bash
# MODEL VS RUNTIME (bug 080) — the extracted PExpr model and the interpreter compute the same values.
#
# A proof is about the extracted model; the program runs Core. Bug 080 lived in the gap: `PExpr.cast`
# was the identity while the runtime wrapped, so `x as u8` was 300 in the model and 44 in the binary,
# and a theorem about the model was a theorem about a different function. `--report model-diff`
# evaluates both, in process, on boundary inputs of every scalar-signature extracted function.
#
#   C  the corpus (every example and std) has ZERO value mismatches, over a non-empty population;
#      Every function the report lists as eligible (extracted, scalar signature) has at least one row
#      — a nullary or bool-only function used to get no input tuple at all — and at least one row
#      that is not ERROR (neither side produced a value, so the input compared nothing), except three
#      named, reasoned all-ERROR functions;
#   V  every row's verdict is RE-DERIVED here from its model/runtime columns;
#   N  the negative control: CONCRETE_MODEL_DIFF_PERTURB changes one function's MODEL value before the
#      PRODUCTION comparator sees it (an entry point's too, since entry points are compared), and every row of that function must read MISMATCH (and only its
#      rows). The corpus agrees everywhere, so V alone cannot show the comparator compares;
#   B  scalar coverage: a nullary function is compared on the empty tuple, bool parameters on false
#      and true (never an integer), and `b as bool` stays a bool on both sides;
#   F  the bug 080 fixture: narrowing (300 -> 44), sign change and widening all agree, and the 300 row
#      shows the WRAPPED value on both sides.
#   E  the bug 084 fixture: match-arm binders bind BY POSITION in the model as in Elab, the
#      interpreter and the binary. Every row agrees, and the pinned rows carry the POSITIONAL
#      value — a by-name model reads 0 for `Full { v }`, 100 for `Mk { b, a }` and x for
#      `Mk { _, a }`, so each pin fails under the old binding. The compiled binary is run too
#      (exit 22 = 5+5+5+7), so "runtime" is not only the interpreter.
#   S  the bug 085 fixture: loop bodies with sequential assigns, fall-through ifs and early returns
#      agree, each pinned to its runtime value; a branch- or body-local `let` named by what follows its
#      scope (bug 101) is not extracted, while a local nothing after it names still is.
#   O  the operators added on 2026-10-23 (division, shifts, bitwise and at every width, wrapping
#      and saturating add/sub, short-circuit && / ||, unary - and !): one function each, every one
#      compared, edge cases pinned, and each perturbed;
#   M  the state model outside loops (2026-10-23): assignments rebind, a match statement's arms
#      continue to the code after it, an if-expression branch that assigns is refused, and a
#      statement arm's value is discarded (the two counterexamples of its review);
#   T  the bug 086 fixture: a discarded `e;` or a return out of a non-tail expression is not
#      extracted; a tail match/if whose arms return agrees.
#
# Sampling, not proof: agreement on boundaries is evidence, not a theorem (R-0460 owns the theorems).
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
BIN="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$BIN" ] || { echo "error: build first ($BIN missing)" >&2; exit 2; }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0; INCOMPLETE=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
inc(){ echo "  INCOMPLETE $1"; INCOMPLETE=$((INCOMPLETE+1)); }
BOUNDED_PARENT="$TMP"
source "$ROOT_DIR/scripts/tests/lib/bounded.sh"

echo "=== C/V: the corpus ==="
: > "$TMP/rows"; : > "$TMP/elig"
# Bug 106: this loop used `|| true`, so a report that hung (thesis_demo ran for hours: an
# unbudgeted interpreter loop at len = i64::MAX) or crashed was invisible and the gate still
# printed PASS. Every report now runs under a watchdog (lib/bounded.sh): a report that times out
# is re-run once alone with a larger budget, and a second timeout is INCOMPLETE, which still fails
# the gate's exit status (a hung report can never pass) without being read as a wrong answer; a
# nonzero exit is a FAIL unless the file is one of the deliberately ill-formed examples below (which must
# keep failing by refusing the program, never by timing out).
command -v python3 >/dev/null 2>&1 || { echo "error: python3 is required for the report watchdog (a hung report must not pass, bug 106)" >&2; exit 2; }
EXPECT_REFUSED=" examples/contract_negatives/duplicate_links/src/main.con examples/contract_negatives/invalid_attribute/src/main.con examples/contract_negatives/invalid_contract_expression/src/main.con examples/contract_negatives/spec_ghost_totality/src/main.con examples/known_holes/returned_ref_provenance_map/src/main.con examples/known_holes/returned_ref_provenance_vec/src/main.con "
#   contract_negatives/*  are negative fixtures: the program is refused before any report;
#   known_holes/returned_ref_provenance_*  are refused by the returned-reference rule (H1).
HUNG=""; CRASHED=""; UNREFUSED=""
for f in $(ls examples/*/src/main.con examples/*/*/src/main.con 2>/dev/null) std/src/lib.con; do
  bounded_run -i /dev/null -r 450 300 -- "$BIN" "$f" --report model-diff > "$TMP/one" 2>"$TMP/one.err"; rc=$?
  if bounded_is_incomplete "$rc"; then HUNG="$HUNG $f"
  elif [ "$rc" -ne 0 ]; then
    case "$EXPECT_REFUSED" in *" $f "*) ;; *) CRASHED="$CRASHED $f(rc=$rc)";; esac
  else
    case "$EXPECT_REFUSED" in *" $f "*) UNREFUSED="$UNREFUSED $f";; esac
  fi
  grep -vE '^MODEL-DIFF|^$' "$TMP/one" >> "$TMP/rows" || true
  grep '^MODEL-DIFF-ELIGIBLE' "$TMP/one" | cut -f2 >> "$TMP/elig" || true
done
[ -z "$HUNG" ] && ok "C: no report timed out (300s watchdog per file, 450s on the re-run alone)" \
  || inc "C: --report model-diff timed out twice on:$HUNG (a hang is the bug 106 class; no observation was made)"
[ -z "$CRASHED" ] && ok "C: every report exited 0, except the named ill-formed examples" \
  || no "C: --report model-diff failed on:$CRASHED"
[ -z "$UNREFUSED" ] && ok "C: every named ill-formed example is still refused" \
  || no "C: expected-refused examples now succeed (update EXPECT_REFUSED):$UNREFUSED"
# Bug 106 fixture: thesis_demo's `validate(data, len)` loops `len` times; sampled at len = i64::MAX
# the runtime ran for hours. Under the report's loop budget that input is `exhausted` (compared
# nothing), and the small inputs still agree.
grep -qP '^exhausted\tmain\.validate\tunclaimed\t\(1, 9223372036854775807\)\tmodel=—\truntime=exhausted$' "$TMP/rows" \
  && ok "C: thesis_demo validate(1, i64::MAX) ends as 'exhausted' under the loop budget (bug 106)" \
  || no "C: thesis_demo validate(1, i64::MAX) is not an 'exhausted' row: $(grep -P 'validate\t.*9223372036854775807\)' "$TMP/rows" | head -1 | tr '\t' ' ')"
grep -qP '^agree\tmain\.validate\t' "$TMP/rows" \
  && ok "C: thesis_demo validate still agrees on small inputs" \
  || no "C: thesis_demo validate has no agreeing row"
N="$(grep -c . "$TMP/rows" || true)"
FNS="$(cut -f2 "$TMP/rows" | sort -u | grep -c . || true)"
CLAIMED="$(awk -F'\t' '$3 == "claimed"' "$TMP/rows" | cut -f2 | sort -u | grep -c . || true)"
[ "$N" -gt 1000 ] && [ "$FNS" -gt 50 ] && [ "$CLAIMED" -gt 5 ] \
  && ok "C: $FNS functions ($CLAIMED claimed) over $N inputs were compared" \
  || no "C: the population is too small to mean anything ($FNS functions, $CLAIMED claimed, $N inputs)"
ELIG="$(sort -u "$TMP/elig" | grep -c . || true)"
UNCOVERED="$(comm -23 <(sort -u "$TMP/elig") <(cut -f2 "$TMP/rows" | sort -u) | head -5 | tr '\n' ' ')"
[ "$ELIG" -gt 50 ] && [ -z "$UNCOVERED" ] \
  && ok "C: every one of the $ELIG eligible functions has at least one compared input" \
  || no "C: eligible functions without a compared input (of $ELIG): $UNCOVERED"
# ERROR rows (neither side produced a value) compared nothing. Most are inputs on which both sides
# legitimately refuse (a zero divisor). A function ALL of whose rows are ERROR was compared on nothing
# at all; only the functions named here may be, each for a stated reason, and each must still be all
# ERROR (so an entry cannot outlive its reason silently).
ALL_ERROR_OK=" cv.divzero_const cn.violation calls.middle calls.outer error_conventions.main project.main proven_oob.main proven_div_zero.main "
#   cv.divzero_const  divides by a constant zero: traps on every input, and the model has no value;
#   cn.violation      is ratio(10, 0): the same trap, nullary;
#   calls.middle, calls.outer (proof_patterns/composition_deep_trust) call the `trusted fn leaf`, which
#                     deliberately has no model: the model is stuck on every input by design;
#   (scope.accumulate left this list on 2026-10-24: it reads the module constant LIMIT, which
#   neither side bound; both bind module constants now (ModelDiff.modelDiffConstEnv), so it is
#   compared, as are stackvm's add_ok/sub_ok, which read I64_MAX/I64_MIN.)
#   (std.time.time_test_duration_from_millis left this list on 2026-10-23: its callee's division at
#   Int is modelled now, so it is compared.)
# Entry points are compared too (an entry point whose only exclusion from proof is being `main`):
#   error_conventions.main  matches on the Result its callee returns: the model is stuck there;
#   project.main      calls functions imported from other files of the project, which the in-process
#                     interpreter call does not resolve, so the runtime side traps (a harness gap;
#                     `--interp` itself returns 18);
#   proven_oob.main   (known_holes/proven_oob_index) indexes out of bounds on purpose: the runtime
#                     traps and the model has no value;
#   proven_div_zero.main  divides by zero on purpose (a known-hole example): both sides trap.
# Each entry must still be compared on nothing, so an entry cannot outlive its reason silently.
ERR="$(grep -c '^ERROR' "$TMP/rows" || true)"
NOVAL=""
for fn in $(sort -u "$TMP/elig"); do
  r="$(grep -P "\t\Q$fn\E\t" "$TMP/rows")"
  # A COMPARED value means BOTH sides produced one (agree or MISMATCH). A function whose every row is
  # partial, model_stuck or ERROR was never compared, and a regression making it model_stuck on every
  # input would otherwise stay green (Codex land1 confirmation review).
  if ! grep -qE '^(agree|MISMATCH)' <<<"$r"; then
    case "$ALL_ERROR_OK" in *" $fn "*) ;; *) NOVAL="$NOVAL $fn";; esac
  fi
done
STALE=""
for fn in $ALL_ERROR_OK; do
  r="$(grep -P "\t\Q$fn\E\t" "$TMP/rows")"
  { [ -z "$r" ] || grep -qE '^(agree|MISMATCH)' <<<"$r"; } && STALE="$STALE $fn"
done
[ -z "$NOVAL" ] && [ -z "$STALE" ] \
  && ok "C: $ERR inputs compared nothing (both sides failed), and every eligible function outside the $(wc -w <<<"$ALL_ERROR_OK" | tr -d ' ') named ones has a compared value" \
  || no "C: functions compared on nothing:${NOVAL:- none}; stale all-ERROR entries:${STALE:- none}"
MM="$(grep -c '^MISMATCH' "$TMP/rows" || true)"
[ "$MM" -eq 0 ] && ok "C: no input where the model and the runtime produce different values" \
  || no "C: $MM model/runtime value mismatches: $(grep -m3 '^MISMATCH' "$TMP/rows" | tr '\t' ' ' | tr '\n' '|')"
BAD="$(awk -F'\t' '{m=$5; r=$6; sub(/^model=/,"",m); sub(/^runtime=/,"",r);
  want = (r=="exhausted") ? "exhausted" : ((m!="—" && r!="trap") ? (m==r ? "agree" : "MISMATCH") : (m!="—" ? "partial" : (r!="trap" ? "model_stuck" : "ERROR")));
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

# A one-step newtype rebrand between two non-integer types (`Inner -> Middle`, `Middle -> Outer`) is the
# identity, as CoreCheck admits it; refusing it cost extraction of a well-typed body (Codex land1 review).
"$BIN" tests/programs/adversarial/newtype/chained.con --report proof-status > "$TMP/nt" 2>&1
grep -A1 '`main.Outer_unwrap_all`' "$TMP/nt" | has_match 'obligation: extractable' \
  && ok "F: a chained newtype unwrap (Outer -> Middle -> Inner) is extractable" \
  || no "F: Outer_unwrap_all is not extractable: $(grep -A1 'Outer_unwrap_all' "$TMP/nt" | tr '\n' ' ')"

echo ""
echo "=== N: the negative control — a perturbed model value reads MISMATCH ==="
CONCRETE_MODEL_DIFF_PERTURB=main.narrow "$BIN" "$F" --report model-diff > "$TMP/n" 2>&1
r="$(grep -P "\tmain\.narrow\t" "$TMP/n")"
if [ -n "$r" ] && ! grep -qv '^MISMATCH' <<<"$r"; then
  ok "N: every input of the perturbed narrow reads MISMATCH ($(grep -c . <<<"$r"))"
else no "N: a perturbed narrow row does not read MISMATCH: $(grep -v '^MISMATCH' <<<"$r" | head -1 | tr '\t' ' ')"; fi
grep -qP '^MISMATCH\tmain\.narrow\tunclaimed\t\(300\)\tmodel=45\truntime=44$' "$TMP/n" \
  && ok "N: narrow(300) reads model=45 runtime=44 under the perturbation" \
  || no "N: narrow(300) under the perturbation: $(grep -P 'narrow\t.*\(300\)' "$TMP/n" | tr '\t' ' ')"
other="$(grep -vP "\tmain\.narrow\t" "$TMP/n" | grep -vE '^MODEL-DIFF|^$' | grep -v '^agree' | head -1)"
[ -z "$other" ] && ok "N: only the perturbed function's rows changed" \
  || no "N: an unperturbed row does not agree: $(tr '\t' ' ' <<<"$other")"
BF="tests/regressions/model_diff_scalars/repro.con"
CONCRETE_MODEL_DIFF_PERTURB=main.flip "$BIN" "$BF" --report model-diff > "$TMP/nb" 2>&1
grep -qP '^MISMATCH\tmain\.flip\tunclaimed\t\(true\)\tmodel=true\truntime=false$' "$TMP/nb" \
  && ok "N: a perturbed bool result reads MISMATCH (flip(true): model=true runtime=false)" \
  || no "N: the perturbed bool row: $(grep -P 'flip\t' "$TMP/nb" | tr '\t' ' ' | tr '\n' '|')"

# The entry point (compared since an entry point whose only exclusion from proof is being main
# is extracted for this report): it agrees, and perturbing ITS model value reads MISMATCH.
EF="tests/regressions/model_diff_entry_point/repro.con"
"$BIN" "$EF" --report model-diff > "$TMP/ne" 2>&1
grep -qP '^agree\tmain\.main\tunclaimed\t\(\)\tmodel=42\truntime=42$' "$TMP/ne" \
  && ok "N: the entry point main() is 42 in the model and at runtime" \
  || no "N: the entry point row: $(grep -P '\tmain\.main\t' "$TMP/ne" | tr '\t' ' ' | tr '\n' '|')"
CONCRETE_MODEL_DIFF_PERTURB=main.main "$BIN" "$EF" --report model-diff > "$TMP/nep" 2>&1
grep -qP '^MISMATCH\tmain\.main\tunclaimed\t\(\)\tmodel=43\truntime=42$' "$TMP/nep" \
  && ok "N: a perturbed entry point reads MISMATCH (main(): model=43 runtime=42)" \
  || no "N: the perturbed entry point row: $(grep -P '\tmain\.main\t' "$TMP/nep" | tr '\t' ' ' | tr '\n' '|')"

echo ""
echo "=== M: the functional state model outside loops — assignments and match statements ==="
MF="tests/regressions/model_diff_state/repro.con"
"$BIN" "$MF" --report model-diff > "$TMP/m" 2>&1
"$BIN" "$MF" --report extraction > "$TMP/mx" 2>&1; mxrc=$?
grep -qE '^MISMATCH' "$TMP/m" && no "M: a mismatch: $(grep -m2 '^MISMATCH' "$TMP/m" | tr '\t' ' ' | tr '\n' '|')" \
  || ok "M: no mismatch on the state fixture"
# an assignment inside an if EXPRESSION would be lost: the extractor refuses the function, and
# the report must succeed and list escapes with a status that is not `extracted`
for fn in escapes discarded; do
esc_status="$(grep -A1 -E "^\\s*main\\.$fn" "$TMP/mx" | grep -oE 'status: .*' | head -1)"
if [ "$mxrc" -eq 0 ] && [ -n "$esc_status" ] && [ "$esc_status" != "status: extracted" ]; then
  ok "M: $fn is listed and not extracted ($esc_status)"
else no "M: $fn: report exit $mxrc, ${esc_status:-no entry} (its branch assignment would be lost)"; fi
done
for fn in armvalue state; do
  r="$(grep -P "\tmain\.$fn\t" "$TMP/m")"
  if [ -n "$r" ] && ! grep -qv '^agree' <<<"$r"; then ok "M: every input of $fn agrees ($(grep -c . <<<"$r"))"
  else no "M: $fn: $(grep -v '^agree' <<<"$r" | head -2 | tr '\t' ' ' | tr '\n' '|')"; fi
done
grep -qP '^agree\tmain\.armvalue\tunclaimed\t\(0\)\tmodel=42\truntime=42$' "$TMP/m" \
  && ok "M: armvalue(0) is 42: the arm's value is discarded" || no "M: armvalue(0): $(grep -P 'armvalue\t.*\(0\)' "$TMP/m" | tr '\t' ' ')"
grep -qP '^agree\tmain\.state\tunclaimed\t\(0\)\tmodel=100\truntime=100$' "$TMP/m" \
  && ok "M: state(0) is 100: the match statement's assignment reaches the return" || no "M: state(0): $(grep -P 'state\t.*\(0\)' "$TMP/m" | tr '\t' ' ')"

echo ""
echo "=== O: the operators added on 2026-10-23 — one function per class, each compared and perturbed ==="
OF="tests/regressions/model_diff_operators/repro.con"
OPS="divi shl8 shli8 shli andi wadd8 sadd8 ssub8 both either negi8 notb divneg scand scor"
"$BIN" "$OF" --report model-diff > "$TMP/o" 2>&1; orc=$?
[ "$orc" -eq 0 ] && ok "O: the report succeeds on the operator fixture" || no "O: the report failed (exit $orc)"
grep -qE '^MISMATCH' "$TMP/o" && no "O: a mismatch: $(grep -m3 '^MISMATCH' "$TMP/o" | tr '\t' ' ' | tr '\n' '|')" \
  || ok "O: no mismatch on any operator"
for fn in $OPS; do
  grep -qP "^agree\tmain\.$fn\t" "$TMP/o" && ok "O: $fn is compared ($(grep -cP "^agree\tmain\.$fn\t" "$TMP/o") agreeing inputs)" \
    || no "O: $fn has no agreeing input: $(grep -P "\tmain\.$fn\t" "$TMP/o" | head -2 | tr '\t' ' ' | tr '\n' '|')"
done
opin(){ grep -qP "^$1\tmain\.$2\tunclaimed\t\($3\)\tmodel=$4\truntime=$5\$" "$TMP/o" \
  && ok "O: $2($3): model=$4 runtime=$5 ($6)" || no "O: $2($3) should read model=$4 runtime=$5 ($6): $(grep -P "\tmain\.$2\t.*\($3\)" "$TMP/o" | tr '\t' ' ')"; }
opin agree shli8 "127, 1" -2 -2 "the bits shifted out are discarded, read signed"
opin agree wadd8 "255, 1" 0 0 "wraps modulo 256"
opin agree sadd8 "255, 1" 255 255 "clamps at the maximum"
opin agree ssub8 "-128, 1" -128 -128 "clamps at the minimum"
opin ERROR shl8 "1, 8" — trap "a count of the width traps on both sides"
opin ERROR divi "1, 0" — trap "a zero divisor traps on both sides"
opin partial negi8 "-128" 128 trap "-MIN: the model's subtraction is mathematical"
opin ERROR divneg "-9223372036854775808" — trap "MIN / -1 is not representable"
opin agree scand "false, 0" false false "&& does not evaluate its right operand"
opin agree scor "0" true true "|| does not evaluate its right operand"
# perturbing each class's function turns its compared rows, and only its rows, into MISMATCH
for fn in $OPS; do
  CONCRETE_MODEL_DIFF_PERTURB="main.$fn" "$BIN" "$OF" --report model-diff > "$TMP/op" 2>&1
  mine="$(grep -cP "^MISMATCH\tmain\.$fn\t" "$TMP/op" || true)"
  was="$(grep -cP "^agree\tmain\.$fn\t" "$TMP/o" || true)"
  others="$(grep -P '^MISMATCH\t' "$TMP/op" | grep -cvP "\tmain\.$fn\t" || true)"
  # every row that agreed must now disagree, and no other function's row may
  [ "$mine" -gt 0 ] && [ "$mine" -eq "$was" ] && [ "$others" -eq 0 ] \
    && ok "O: perturbing $fn turns all $was of its agreeing rows into MISMATCH, and only those" \
    || no "O: perturbing $fn: $mine MISMATCH rows of $was agreeing, $others others"
done

echo ""
echo "=== B: scalar coverage — nullary, bool and mixed signatures ==="
"$BIN" "$BF" --report model-diff > "$TMP/b" 2>&1
bpin(){ grep -qP "^agree\tmain\.$1\tunclaimed\t\($2\)\tmodel=$3\truntime=$3\$" "$TMP/b" \
  && ok "B: $1($2) is $3 in the model and at runtime" \
  || no "B: $1($2) does not read model=$3 runtime=$3: $(grep -P "\tmain\.$1\t" "$TMP/b" | head -3 | tr '\t' ' ' | tr '\n' '|')"; }
bpin seven "" 7
bpin flip false true
bpin flip true false
bpin pick "1, true" 1
bpin pick "0, false" 0
bpin keep true true
bpin keep false false
bpin cond_cast true 1
bpin cond_cast false 2
for fn in seven flip pick keep cond_cast; do
  grep -q "^MODEL-DIFF-ELIGIBLE	main\.$fn\$" "$TMP/b" && ok "B: $fn is listed eligible" || no "B: $fn is not listed eligible"
  r="$(grep -P "\tmain\.$fn\t" "$TMP/b")"
  if [ -n "$r" ] && ! grep -qv '^agree' <<<"$r"; then ok "B: every input of $fn agrees ($(grep -c . <<<"$r"))"
  else no "B: $fn has a non-agreeing or missing row: $(grep -v '^agree' <<<"$r" | head -1 | tr '\t' ' ')"; fi
done
# A bool parameter is never handed an integer: every pick row's second input is a boolean.
grep -P "\tmain\.pick\t" "$TMP/b" | cut -f4 | has_match -vE ', (true|false)\)$' \
  && no "B: a pick row passes a non-boolean for its bool parameter: $(grep -P "\tmain\.pick\t" "$TMP/b" | cut -f4 | grep -vE ', (true|false)\)$' | head -1)" \
  || ok "B: every pick row passes a boolean for its bool parameter"

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
echo "=== S: bug 085 — a loop body's statements run in order ==="
# Each pin is the RUNTIME value; before the fix the model gave the value in parentheses (or got
# stuck). `want FN INPUT VALUE` requires the row to read agree with that value on both sides.
want(){ grep -qP "^agree\tmain\.$2\t[a-z]+\t\($3\)\tmodel=$4\truntime=$4\$" "$1" \
  && ok "$5: $2($3) is $4 in the model and at runtime" \
  || no "$5: $2($3) does not read model=$4 runtime=$4: $(grep -P "\tmain\.$2\t.*\($3\)" "$1" | tr '\t' ' ')"; }
allagree(){ r="$(grep -P "\tmain\.$2\t" "$1")"
  if [ -n "$r" ] && ! grep -qvE '^(agree|partial)' <<<"$r"; then ok "$3: every input of $2 agrees ($(grep -c . <<<"$r"))"
  else no "$3: $2 has a disagreeing or missing row: $(grep -vE '^(agree|partial)' <<<"$r" | head -1 | tr '\t' ' ')"; fi; }
S="tests/regressions/bug085_loop_step/repro.con"
"$BIN" "$S" --report model-diff > "$TMP/s" 2>&1
want "$TMP/s" twice 7 14 S            # two assigns to one name compose (was 7)
want "$TMP/s" seq 7 28 S              # a later assign reads the earlier one (was 21)
want "$TMP/s" seq_assign 0 3 S        # (was 2)
want "$TMP/s" assign_then_let 0 60 S  # a let after an assign (was 30)
want "$TMP/s" use_i_after 0 6 S       # (was 3)
want "$TMP/s" if_fallthrough 0 104 S  # a fall-through then-branch keeps the step (was stuck)
want "$TMP/s" ifthen 0 10 S           # (was stuck)
want "$TMP/s" if_else_step 0 12 S
want "$TMP/s" ret_after_assign 0 21 S # Break reads the new value (was 14)
want "$TMP/s" ret_pending 7 8 S       # (was 7)
want "$TMP/s" branch_let_returns 0 20 S  # a branch-local let consumed before its branch returns
want "$TMP/s" fresh_branch_let 0 8 S     # a fresh branch-local let nothing after the branch names
want "$TMP/s" reloop 1 6 S               # a later for re-declares the branch's i (hmac_sha256's shape)
want "$TMP/s" reloop 0 4 S
for fn in twice seq seq_assign assign_then_let use_i_after if_fallthrough ifthen if_else_step \
          ret_after_assign ret_pending branch_let_returns fresh_branch_let reloop; do allagree "$TMP/s" "$fn" S; done
# Bug 101: a local named by what follows its scope. Before the fix scoped(1) read 10 in the model and 1
# at runtime; scoped_flat(0) likewise; body_local_assigned read 6 for 1. Bug 087's Elab alpha-renaming
# (batch 2) gives the branch-local `let x` its own name, so scoped/scoped_flat now EXTRACT and must
# AGREE with the runtime at pinned values (the old 10 is the bug); body_local_assigned is still refused.
want "$TMP/s" scoped 0 0 S
want "$TMP/s" scoped 1 1 S
want "$TMP/s" scoped 2 1 S
want "$TMP/s" scoped_flat 0 1 S
want "$TMP/s" scoped_flat 1 1 S
allagree "$TMP/s" scoped_flat S
grep -qP "\tmain\.body_local_assigned\t" "$TMP/s" \
  && no "S: body_local_assigned is extracted: $(grep -m1 -P "\tmain\.body_local_assigned\t" "$TMP/s" | tr '\t' ' ')" \
  || ok "S: body_local_assigned is not extracted (a local is named after its scope ends)"

echo ""
echo "=== T: bug 086 — a discarded statement is not the value; a nested return exits ==="
T="tests/regressions/bug086_discarded_tail/repro.con"
"$BIN" "$T" --report model-diff > "$TMP/t" 2>&1
# Not modelable exactly (a call the model cannot see; a return out of a let initializer):
# they must not be extracted at all, so no row can claim a value. (A return inside a call
# argument, `argret`, is refused by the language since bug 142 and left the fixture.)
for fn in g letret; do
  grep -qP "\tmain\.$fn\t" "$TMP/t" && no "T: $fn is extracted: $(grep -m1 -P "\tmain\.$fn\t" "$TMP/t" | tr '\t' ' ')" \
    || ok "T: $fn is not extracted"
done
want "$TMP/t" tailmatch 0 5 T
want "$TMP/t" tailmatch 1 6 T
want "$TMP/t" tailif 1 3 T
want "$TMP/t" tailif 0 4 T
for fn in tailmatch tailif; do allagree "$TMP/t" "$fn" T; done

echo ""
bounded_summary
echo "MODEL-DIFF-GATE: PASS=$PASS FAIL=$FAIL INCOMPLETE=$INCOMPLETE"
[ "$FAIL" -eq 0 ] && [ "$INCOMPLETE" -eq 0 ]
