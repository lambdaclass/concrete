#!/usr/bin/env bash
# TRUST BAN: no `sorry` and no `native_decide` anywhere in the specification (Q9.17), checked
# transitively in the kernel's terms and in the sources the environment does not keep.
#
#   E  no constant of ANY kind the ConcreteSpec library declares (definitions, instances and
#      generated helpers included, not only theorems) depends transitively on sorryAx, on
#      Lean.ofReduceBool or Lean.ofReduceNat (what native_decide and `decide +native` reduce through),
#      on Lean.trustCompiler, or on a generated `._native.` axiom;
#   D  the library declares no axiom of its own;
#   X  every `example`, which the kernel checks and Lean then discards (so E and D never see it),
#      depends only on the standard axioms: in a copy of the library each `example` becomes a
#      named declaration (scripts/proofs/lean_lex.py), the copy is built, and its axioms are
#      collected as E collects a theorem's;
#   S  defence in depth, lexical: the library builds with warningAsError and nothing in lakefile.toml
#      can change that or point the build at other sources; the sources, comments stripped and
#      string literals kept, never spell sorry, sorryAx, admit, stop, native_decide, +native, an
#      option on `decide`, ofReduceBool, ofReduceNat, trustCompiler or skipKernelTC, and define no
#      syntax extension, run no meta code, set no option and import nothing outside the library
#      (the lexer reads what Lean reads only for syntax it knows).
#
# NEGATIVE CONTROLS: a theorem proved by sorry, a definition whose body is sorry (no theorem about
# it), a theorem proved by native_decide and a declared axiom each fail E or D naming the constant;
# a source with an `example` proved by native_decide or a sorry hidden in an interpolation fails S,
# the same words inside comments do not, and an unreadable or unlexable source fails S; examples
# proved by `decide +native` and by `stop` fail X, and one proved by rfl does not.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/../checks/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
CHECK_NAME=PROOF-TRUST-BAN
source scripts/proofs/lib.sh

BANNED_AXIOM_RE='^(sorryAx|Lean[.]ofReduceBool|Lean[.]ofReduceNat|Lean[.]trustCompiler)$|[.]_native[.]'
BANNED_WORD_RE='\b(sorry|sorryAx|admit|stop|native_decide|ofReduceBool|ofReduceNat|trustCompiler|skipKernelTC)\b|\+native\b|\bnative»?\s*:=|\bdecide\s*\(\s*config\b|\bdecide\s*[+-]|\bDecide[.]Config\b'
# `decide` takes no options at all (`decide +native`, `decide (config := ⟨…⟩)` with a positional
# native flag): a term `decide (p)` is unaffected.
# The source scan is lexical, so it holds only for the syntax it lexes: Lean core's. A syntax
# extension could make Lean read a literal differently from the scan (a macro taking `s!` before a
# plain string), so the sources may define none and import nothing outside the library (only Init,
# implicitly, which is where s!, m! and f! live). They set no option either: the library builds with
# warningAsError (checked below in lakefile.toml), so a `sorry` that elaborates anywhere, whatever
# tactic produced it (`stop` is `repeat sorry`), fails the build; `set_option warningAsError false`
# would undo that. Nor do they run meta code (run_cmd, run_tac, #eval and the like).
EXTENSION_RE='#eval\b|\b(run_cmd|run_tac|run_elab|run_meta|set_option|macro|macro_rules|syntax|notation|infix|infixl|infixr|prefix|postfix|elab|elab_rules|declare_syntax_cat|initialize|builtin_initialize)\b'

# banned FILE → `<constant> (<axiom>)` for every constant of a PROOF_MODE=constants listing with a banned axiom
banned() {
  awk -F'\t' -v re="$BANNED_AXIOM_RE" '{ n = split($3, a, " ")
    for (i = 1; i <= n; i++) if (a[i] ~ re) { print $1 " (" a[i] ")"; break } }' "$1"
}
# declared FILE → the axioms a PROOF_MODE=constants listing declares
declared() { awk -F'\t' '$2 == "axiom" { print $1 }' "$1"; }
# scan FILE... → `file:line: text` for every banned word, syntax extension, option, meta command or
# outside import outside comments; exit 3 when a source cannot be lexed (lean_lex.py)
scan() { python3 "$PROOFS_DIR/lean_lex.py" scan "$BANNED_WORD_RE|$EXTENSION_RE" "$@"; }

echo "=== E/D: the environment ==="
population imported > "$TMP/imported.txt" 2> "$TMP/imported.err" || { no "PROOF_MODE=imported failed"; finish; }
GAP=$(closure_gap "$TMP/imported.txt")
[ -z "$GAP" ] && ok "all $(spec_modules | grep -c .) ConcreteSpec source modules are in the environment E and D read" \
  || no "source modules outside the root's import closure, unseen by E and D: $(echo $GAP)"
population constants > "$TMP/consts.tsv" 2> "$TMP/consts.err" \
  || { cat "$TMP/consts.err" >&2; echo "error: the enumerator failed" >&2; exit 2; }
NC=$(grep -c . "$TMP/consts.tsv")
B=$(banned "$TMP/consts.tsv")
[ "$NC" -ge 1000 ] && [ -z "$B" ] \
  && ok "none of the $NC ConcreteSpec constants depends on sorryAx, ofReduceBool/Nat, trustCompiler or a native axiom" \
  || no "$NC constants; banned dependencies: $(echo $B | cut -c1-400)"
A=$(declared "$TMP/consts.tsv")
[ -z "$A" ] && ok "ConcreteSpec declares no axiom" || no "ConcreteSpec declares axioms: $(echo $A)"

echo "=== S: the sources ==="
mapfile -t SRC < <({ echo ConcreteSpec.lean; find ConcreteSpec -name '*.lean'; } | sort)
W=$(scan "${SRC[@]}"); RC=$?
[ "$RC" = 0 ] && [ "${#SRC[@]}" -ge 10 ] && [ -z "$W" ] && ok "none of the ${#SRC[@]} source files spells a banned word outside comments" \
  || no "the source scan (rc=$RC) of ${#SRC[@]} files: $(printf '%s\n' "$W" | head -5 | tr '\n' '|')"

# lake_options LAKEFILE → empty when the one library builds from ConcreteSpec.lean and ConcreteSpec/
# (Lake's defaults: no srcDir, roots or globs) with warningAsError, and nothing can turn it off
lake_options() {
  python3 - "$1" <<'EOF'
import sys, tomllib
try:
    cfg = tomllib.load(open(sys.argv[1], "rb"))
except Exception as e:
    print(f"unreadable: {e}"); sys.exit(0)
bad = []
import os
if os.path.exists(os.path.join(os.path.dirname(sys.argv[1]) or ".", "lakefile.lean")):
    bad.append("a lakefile.lean, which Lake reads instead of lakefile.toml")
if cfg.get("leanOptions", {}).get("warningAsError") is not True:
    bad.append("[leanOptions] warningAsError is not true")
# Allow-lists, not deny-lists: a key not named here (srcDir, roots, globs, moreLeanArgs, …) could
# point the build at other sources or pass options, so it is refused.
extra = set(cfg) - {"name", "version", "defaultTargets", "leanOptions", "lean_lib"}
if extra: bad.append(f"the package sets {sorted(extra)}")
if set(cfg.get("leanOptions", {})) != {"warningAsError"}:
    bad.append(f"[leanOptions] sets {sorted(set(cfg.get('leanOptions', {})) - {'warningAsError'})}")
libs = cfg.get("lean_lib", [])
if [l.get("name") for l in libs] != ["ConcreteSpec"]: bad.append(f"libraries {[l.get('name') for l in libs]}")
for l in libs:
    extra = set(l) - {"name", "leanOptions"}
    if extra: bad.append(f"the library sets {sorted(extra)}")
    opts = set(l.get("leanOptions", {})) - {"autoImplicit"}
    if opts: bad.append(f"the library sets options {sorted(opts)}")
print("; ".join(bad))
EOF
}
L=$(lake_options lakefile.toml)
[ -z "$L" ] && ok "lakefile.toml builds the library with warningAsError, and nothing overrides it" || no "lakefile.toml: $L"

echo "=== X: the examples, in the kernel's terms ==="
# A copy of the library in which every `example` is a named declaration, built, and its axioms
# collected as E collects a theorem's. warningAsError is off in the copy only: a definition may draw
# a linter warning the example did not, and the copy is read for axioms, not judged for warnings.
EX="$TMP/ex"; mkdir -p "$EX"
cp lakefile.toml lean-toolchain lake-manifest.json "$EX"/ || { no "could not copy the Lake files"; finish; }
[ -d .lake/build ] && mkdir -p "$EX/.lake" && cp -r .lake/build "$EX/.lake/"
sed -i 's/^warningAsError = true$/warningAsError = false/' "$EX/lakefile.toml"
NEX=$(python3 "$PROOFS_DIR/lean_lex.py" examples . "$EX" "${SRC[@]}") \
  || { no "the examples could not be rewritten: $NEX"; finish; }
(cd "$EX" && "$LAKE" build ConcreteSpec) > "$TMP/ex-build.log" 2>&1 \
  || { no "the library with its examples as declarations does not build: $(grep -m3 error "$TMP/ex-build.log" | tr '\n' ' ')"; finish; }
(cd "$EX" && PROOF_MODE=examples "$LAKE" env lean "$ENUMERATOR") > "$TMP/examples.out" 2> "$TMP/examples.err" \
  || { no "PROOF_MODE=examples failed: $(head -2 "$TMP/examples.err" | tr '\n' ' ')"; finish; }
grep -P '\t' "$TMP/examples.out" > "$TMP/examples.tsv"
# nonstandard FILE → `<name> (<axiom>)` for every row whose axioms are not among the standard three
nonstandard() {
  awk -F'\t' -v std=" $STANDARD_AXIOMS " '{ n = split($2, a, " ")
    for (i = 1; i <= n; i++) if (index(std, " " a[i] " ") == 0) { print $1 " (" a[i] ")"; break } }' "$1"
}
NE=$(grep -c . "$TMP/examples.tsv"); X=$(nonstandard "$TMP/examples.tsv")
[ "$NEX" -ge 100 ] && [ "$NE" = "$NEX" ] && [ -z "$X" ] \
  && ok "all $NEX examples, kept as declarations, depend only on the standard axioms" \
  || no "examples: $NEX rewritten, $NE in the environment; on other axioms: $(echo $X | cut -c1-300)"

echo "=== negative controls ==="
# control NAME BODY AXIOM-ERE WHAT: the declarations in BODY must be refused at ConcreteSpecControl.NAME
control() {
  printf '%s\n' "$2" > "$TMP/c-$1.lean"
  population constants "$TMP/c-$1.lean" > "$TMP/c-$1.tsv" 2> "$TMP/c-$1.err"
  local got; got=$(banned "$TMP/c-$1.tsv" | grep -F "ConcreteSpecControl.$1 (")
  has_match -E "\(($3)\)\$" <<<"$got" \
    && ok "control: $4 is refused ($got)" || no "control: $4 was not refused with $3: '$got' $(head -2 "$TMP/c-$1.err" | tr '\n' ' ')"
}
control sorryThm 'theorem ConcreteSpecControl.sorryThm : 1 = 2 := sorry' 'sorryAx' "a theorem proved by sorry"
control sorryDef 'def ConcreteSpecControl.sorryDef : Nat := sorry' sorryAx "a definition with a sorry body and no theorem about it"
control nativeThm 'theorem ConcreteSpecControl.nativeThm : 2 + 2 = 4 := by native_decide' 'Lean\.ofReduceBool|.*\._native\..*' "a theorem proved by native_decide"
printf 'axiom ConcreteSpecControl.ax : 1 = 2\n' > "$TMP/c-ax.lean"
population constants "$TMP/c-ax.lean" > "$TMP/c-ax.tsv" 2>/dev/null
[ "$(declared "$TMP/c-ax.tsv")" = ConcreteSpecControl.ax ] \
  && ok "control: a declared axiom is reported" || no "control: a declared axiom was not reported: '$(declared "$TMP/c-ax.tsv")'"
mkdir -p "$TMP/src"
cat > "$TMP/src/Bad.lean" <<'EOF'
/- a comment may say sorry and native_decide, /- nested -/ and still sorry -/
-- and so may a line comment: sorry
def quote : Char := '"'
example : 2 + 2 = 4 := by native_decide
def interp : String := s!"{(sorry : Nat)}"
def dashes : String := "-- /- not a comment" ++ toString (sorry : Nat)
example : String := s!"{("--")}{(sorry : Nat)}"
example : String := s!"{"/-"}" ++ toString (sorry : Nat)
def chars : List Char := ['{', '}', '\'', '\\', '"', '\x41']
def h' : Nat := 0 -- a prime in a name starts no character literal: sorry
def r' : String := r#"raw "-- /-" text"# ++ toString (sorry : Nat)
example : True := by have «--» : True := True.intro; sorry
example : False := sorryAx False true
macro "plain" "s!" s:str : term => `($s)
import Lean
set_option warningAsError false in
example : False := by stop skip
run_cmd pure ()
#eval (pure () : IO Unit)
example : 2 + 2 = 4 := by decide (config := { native
  := true })
example : 2 + 2 = 4 := by decide (config := ⟨false, true, true, false⟩)
EOF
W=$(scan "$TMP/src/Bad.lean"); RC=$?
B="$TMP/src/Bad.lean"
EXPECT="$B:4: example : 2 + 2 = 4 := by native_decide
$B:5: def interp : String := s!\"{(sorry : Nat)}\"
$B:6: def dashes : String := \"-- /- not a comment\" ++ toString (sorry : Nat)
$B:7: example : String := s!\"{(\"--\")}{(sorry : Nat)}\"
$B:8: example : String := s!\"{\"/-\"}\" ++ toString (sorry : Nat)
$B:11: def r' : String := r#\"raw \"-- /-\" text\"# ++ toString (sorry : Nat)
$B:12: example : True := by have «--» : True := True.intro; sorry
$B:13: example : False := sorryAx False true
$B:14: macro \"plain\" \"s!\" s:str : term => \`(\$s)
$B:15: import Lean
$B:16: set_option warningAsError false in
$B:17: example : False := by stop skip
$B:18: run_cmd pure ()
$B:19: #eval (pure () : IO Unit)
$B:20: example : 2 + 2 = 4 := by decide (config := { native
$B:22: example : 2 + 2 = 4 := by decide (config := ⟨false, true, true, false⟩)"
[ "$RC" = 0 ] && [ "$W" = "$EXPECT" ] \
  && ok "control: native_decide, and sorry inside an interpolation, after a string, raw string or quoted name holding comment delimiters, after an interpolation holding a string, a direct sorryAx, a syntax extension, an outside import, an option, meta code, the stop tactic and decide options (native split across lines, positional) are found; the words in comments are not" \
  || no "control: source scan (rc=$RC) reported '$W'"
printf 'def s : String := s!"unterminated {\n' > "$TMP/src/Open.lean"
printf 'example : String × Nat := { fst := "{--", snd := sorry\n} -- "\n' > "$TMP/src/Brace.lean"
printf 'def q := `(command|\n/-! hidden -/\n)\n' > "$TMP/src/Quote.lean"
for imp in 'import ConcreteSpec.Spec Lean' 'import ConcreteSpec.Spec
  Lean' 'prelude
import ConcreteSpec.Spec'; do
  printf '%s\nnamespace X\nend X\n' "$imp" > "$TMP/src/Imp.lean"
  W=$(scan "$TMP/src/Imp.lean")
  [ -n "$W" ] && ok "control: the header $(printf '%s' "$imp" | tr '\n' ' ') is reported" \
    || no "control: the header $(printf '%s' "$imp" | tr '\n' ' ') passed the scan"
done
printf '[leanOptions]\nwarningAsError = true\n\n[[lean_lib]]\nname = "ConcreteSpec"\nleanOptions = { autoImplicit = false, warningAsError = false }\n' > "$TMP/lake-off.toml"
L=$(lake_options "$TMP/lake-off.toml")
[ -n "$L" ] && ok "control: a library that turns warningAsError off is refused ($L)" \
  || no "control: a lakefile turning warningAsError off was accepted"
printf '[leanOptions]\nwarningAsError = true\n\n[[lean_lib]]\nname = "ConcreteSpec"\nsrcDir = "shadow"\n' > "$TMP/lake-src.toml"
L=$(lake_options "$TMP/lake-src.toml")
[ -n "$L" ] && ok "control: a library built from another source directory is refused ($L)" \
  || no "control: a lakefile with srcDir was accepted"
for f in missing Open Brace Quote; do
  scan "$TMP/src/$f.lean" > /dev/null 2>&1; RC=$?
  [ "$RC" != 0 ] && ok "control: a source the scan cannot read or lex ($f) fails it (rc=$RC)" \
    || no "control: the scan of $f exited 0"
done
head -n 3 "$TMP/imported.txt" > "$TMP/imp-short.txt"
[ -n "$(closure_gap "$TMP/imp-short.txt")" ] \
  && ok "control: source modules outside a (truncated) import closure are reported" || no "control: a truncated closure reported no gap"
# The example leg's own controls: examples rewritten as that leg rewrites them, then enumerated.
mkdir -p "$TMP/exc-src" "$TMP/exc"
printf 'example : 2 + 2 = 4 := by decide +native\nexample : 1 = 2 := by stop skip\nexample : 1 = 1 := rfl\n' > "$TMP/exc-src/C.lean"
N=$(python3 "$PROOFS_DIR/lean_lex.py" examples "$TMP/exc-src" "$TMP/exc" C.lean)
# (Lean prints the warnings of the inserted declarations on stdout too; the rows are the tab lines.)
population examples "$TMP/exc/C.lean" 2>/dev/null | grep -P '\t' > "$TMP/exc.tsv"
X=$(nonstandard "$TMP/exc.tsv")
[ "$N" = 3 ] && [ "$(grep -c . "$TMP/exc.tsv")" = 3 ] && has_match -F "C.lean:1» (" <<<"$X" && has_match -F "C.lean:2» (sorryAx)" <<<"$X" \
  && ! has_match -F "C.lean:3»" <<<"$X" \
  && ok "control: an example proved by decide +native and one closed by stop are reported, rfl is not ($(echo $X | tr '\n' ' '))" \
  || no "control: rewritten examples: $N, rows $(grep -c . "$TMP/exc.tsv"), reported '$X'"
finish
