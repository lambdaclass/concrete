#!/usr/bin/env bash
# TRUST BAN: no `sorry` and no `native_decide` anywhere in the specification (Q9.17), checked
# transitively in the kernel's terms and in the sources the environment does not keep.
#
#   E  no constant of ANY kind the ConcreteSpec library declares (definitions, instances and
#      generated helpers included, not only theorems) depends transitively on sorryAx, on
#      Lean.ofReduceBool or Lean.ofReduceNat (what native_decide and `decide +native` reduce through),
#      on Lean.trustCompiler, or on a generated `._native.` axiom;
#   D  the library declares no axiom of its own;
#   S  the sources, comments stripped (string literals are scanned), never spell sorry, sorryAx,
#      admit, native_decide, +native, ofReduceBool, ofReduceNat, trustCompiler or skipKernelTC. An `example` is checked by the kernel
#      and then discarded, so E and D never see it; this leg does. The scan is lexical, so the
#      sources may also define no syntax extension and import nothing outside the library.
#
# NEGATIVE CONTROLS: a theorem proved by sorry, a definition whose body is sorry (no theorem about
# it), a theorem proved by native_decide and a declared axiom each fail E or D naming the constant;
# a source with an `example` proved by native_decide or a sorry hidden in an interpolation fails S,
# the same words inside comments do not, and an unreadable or unlexable source fails S.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/../checks/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
CHECK_NAME=PROOF-TRUST-BAN
source scripts/proofs/lib.sh

BANNED_AXIOM_RE='^(sorryAx|Lean[.]ofReduceBool|Lean[.]ofReduceNat|Lean[.]trustCompiler)$|[.]_native[.]'
BANNED_WORD_RE='\b(sorry|sorryAx|admit|stop|native_decide|ofReduceBool|ofReduceNat|trustCompiler|skipKernelTC)\b|\+native\b|\bnative\s*:='
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
# scan FILE... → `file:line: text` for every banned word, syntax extension or outside import outside
# comments; exit 3 when a source
# cannot be lexed. Comments (nested `/- -/`, `--`) are blanked; everything else is scanned, string
# literals included, since an interpolation such as s!"{(sorry : Nat)}" runs code. The lexer follows
# Lean's literal forms so that no comment delimiter inside a literal starts a comment: a string with
# its escapes, raw strings r"…" and r#"…"#, character literals and quoted names «…». Whether a
# string interpolates depends on the parser, not the lexer, so the scan admits interpolation only
# directly after `s!`, `m!` or `f!` (its `{ }` code, with its own strings and comments, is lexed
# recursively) and refuses any other string holding `{`. An unterminated string, comment,
# interpolation or name is an error too: the scan fails closed, never skips.
scan() {
  python3 - "$BANNED_WORD_RE|$EXTENSION_RE" "$@" <<'EOF'
import re, sys
word = re.compile(sys.argv[1], re.M)

class LexError(Exception):
    pass

def lex(src):
    n, out = len(src), []
    def ident(i):                                     # src[i] continues an identifier
        return i >= 0 and (src[i].isalnum() or src[i] in "_'!?." or ord(src[i]) > 127)
    def comment(i):                                   # at "/-": blank to the matching "-/"
        depth = 0
        while i < n:
            if src.startswith("/-", i): depth += 1; out.append("  "); i += 2
            elif src.startswith("-/", i):
                depth -= 1; out.append("  "); i += 2
                if depth == 0: return i
            else: out.append("\n" if src[i] == "\n" else " "); i += 1
        raise LexError("unterminated comment")
    def string(i):                                    # at '"'
        interp = src[i - 2:i] in ("s!", "m!", "f!") and not ident(i - 3)
        out.append('"'); i += 1
        while i < n:
            c = src[i]
            if c == "\\": out.append(src[i:i + 2]); i += 2
            elif c == '"': out.append(c); return i + 1
            elif c == "{" and interp: out.append(c); i = code(i + 1, True)
            elif c == "{":
                line = src.count("\n", 0, i) + 1
                raise LexError(f"line {line}: a string holding '{{' without s!, m! or f! before it")
            else: out.append(c); i += 1
        raise LexError("unterminated string")
    def raw(i):                                       # at 'r' of r#*"
        j = i + 1
        while j < n and src[j] == "#": j += 1
        close = '"' + "#" * (j - i - 1)
        k = src.find(close, j + 1)
        if k < 0: raise LexError("unterminated raw string")
        out.append(src[i:k + len(close)]); return k + len(close)
    def code(i, inner):                               # inner: inside an interpolation, stop at its '}'
        depth = 0
        while i < n:
            c = src[i]
            if src.startswith("/-", i): i = comment(i)
            elif src.startswith("--", i):
                j = src.find("\n", i); j = n if j < 0 else j; out.append(" " * (j - i)); i = j
            elif c == '"': i = string(i)
            elif c == "«":                            # a quoted name part: no comment inside it
                k = src.find("»", i + 1)
                if k < 0: raise LexError("unterminated «name»")
                out.append(src[i:k + 1]); i = k + 1
            elif c == "r" and not ident(i - 1) and re.match(r'r#*"', src[i:]): i = raw(i)
            elif c == "'" and not ident(i - 1):
                m = re.match(r"'(\\(x[0-9a-fA-F]{2}|u[0-9a-fA-F]{4}|.)|[^\\'\n])'", src[i:])
                k = len(m.group(0)) if m else 1
                out.append(src[i:i + k]); i += k
            elif inner and c == "{": depth += 1; out.append(c); i += 1
            elif inner and c == "}":
                out.append(c); i += 1
                if depth == 0: return i
                depth -= 1
            else: out.append(c); i += 1
        if inner: raise LexError("unterminated interpolation")
        return i
    code(0, False)
    return "".join(out)

rc = 0
for path in sys.argv[2:]:
    try:
        text = lex(open(path, encoding="utf-8").read())
    except (OSError, UnicodeDecodeError, LexError) as e:
        print(f"error: {path}: {e}", file=sys.stderr); rc = 3; continue
    lines = text.split("\n")
    # The header: only lines `import ConcreteSpec[.M...]`, one module each. Lean lets an import
    # list continue (`import A B`, or B on the next line), so the first other line must be a
    # command (`namespace`, …) at column 0; any other `import` is reported.
    k, header = 0, re.compile(r"import ConcreteSpec(\.[A-Za-z0-9_]+)*\s*$")
    while k < len(lines) and (not lines[k].strip() or header.match(lines[k])): k += 1
    if k < len(lines) and not re.match(r"(namespace|open|section|def|theorem|structure|inductive|abbrev)\b", lines[k]):
        print(f"{path}:{k + 1}: {lines[k].strip()} (the import header must end in a command at column 0)")
    for j, line in enumerate(lines, 1):
        if word.search(line) or (re.search(r"\bimport\b", line) and not (j <= k and header.match(line))):
            print(f"{path}:{j}: {line.strip()}")
sys.exit(rc)
EOF
}

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

# lakefile → empty when the library builds with warningAsError and nothing can turn it off
lake_options() {
  python3 - "$1" <<'EOF'
import sys, tomllib
try:
    cfg = tomllib.load(open(sys.argv[1], "rb"))
except Exception as e:
    print(f"unreadable: {e}"); sys.exit(0)
bad = []
if cfg.get("leanOptions", {}).get("warningAsError") is not True:
    bad.append("[leanOptions] warningAsError is not true")
for k in ("moreLeanArgs", "weakLeanArgs", "moreServerArgs", "moreServerOptions"):
    if k in cfg: bad.append(f"package sets {k}")
libs = [l for l in cfg.get("lean_lib", []) if l.get("name") == "ConcreteSpec"]
if len(libs) != 1: bad.append(f"{len(libs)} ConcreteSpec libraries")
for l in libs:
    extra = set(l) - {"name", "leanOptions", "roots", "globs", "srcDir"}
    if extra: bad.append(f"the library sets {sorted(extra)}")
    opts = set(l.get("leanOptions", {})) - {"autoImplicit"}
    if opts: bad.append(f"the library sets options {sorted(opts)}")
print("; ".join(bad))
EOF
}
L=$(lake_options lakefile.toml)
[ -z "$L" ] && ok "lakefile.toml builds the library with warningAsError, and nothing overrides it" || no "lakefile.toml: $L"

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
$B:19: #eval (pure () : IO Unit)"
[ "$RC" = 0 ] && [ "$W" = "$EXPECT" ] \
  && ok "control: native_decide, and sorry inside an interpolation, after a string, raw string or quoted name holding comment delimiters, after an interpolation holding a string, a direct sorryAx, a syntax extension, an outside import, an option, meta code and the stop tactic are found; the words in comments are not" \
  || no "control: source scan (rc=$RC) reported '$W'"
printf 'def s : String := s!"unterminated {\n' > "$TMP/src/Open.lean"
printf 'example : String × Nat := { fst := "{--", snd := sorry\n} -- "\n' > "$TMP/src/Brace.lean"
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
for f in missing Open Brace; do
  scan "$TMP/src/$f.lean" > /dev/null 2>&1; RC=$?
  [ "$RC" != 0 ] && ok "control: a source the scan cannot read or lex ($f) fails it (rc=$RC)" \
    || no "control: the scan of $f exited 0"
done
head -n 3 "$TMP/imported.txt" > "$TMP/imp-short.txt"
[ -n "$(closure_gap "$TMP/imp-short.txt")" ] \
  && ok "control: source modules outside a (truncated) import closure are reported" || no "control: a truncated closure reported no gap"
finish
