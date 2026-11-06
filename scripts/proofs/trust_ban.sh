#!/usr/bin/env bash
# TRUST BAN: no `sorry` and no `native_decide` anywhere in the specification (Q9.17), checked
# transitively in the kernel's terms and in the sources the environment does not keep.
#
#   E  no constant of ANY kind the ConcreteSpec library declares (definitions, instances and
#      generated helpers included, not only theorems) depends transitively on sorryAx, on
#      Lean.ofReduceBool or Lean.ofReduceNat (what native_decide and `decide +native` reduce through),
#      on Lean.trustCompiler, or on a generated `._native.` axiom;
#   D  the library declares no axiom of its own;
#   S  the sources, comments and strings stripped, never spell sorry, admit, native_decide, +native,
#      ofReduceBool, ofReduceNat, trustCompiler or skipKernelTC. An `example` is checked by the kernel
#      and then discarded, so E and D never see it; this leg does.
#
# NEGATIVE CONTROLS: a theorem proved by sorry, a definition whose body is sorry (no theorem about
# it), a theorem proved by native_decide and a declared axiom each fail E or D naming the constant;
# a source with an `example` proved by native_decide fails S, and the same words inside comments and
# strings do not.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/../checks/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
CHECK_NAME=PROOF-TRUST-BAN
source scripts/proofs/lib.sh

BANNED_AXIOM_RE='^(sorryAx|Lean[.]ofReduceBool|Lean[.]ofReduceNat|Lean[.]trustCompiler)$|[.]_native[.]'
BANNED_WORD_RE='\b(sorry|admit|native_decide|ofReduceBool|ofReduceNat|trustCompiler|skipKernelTC)\b|\+native\b'

# banned FILE → `<constant> (<axiom>)` for every constant of a PROOF_MODE=constants listing with a banned axiom
banned() {
  awk -F'\t' -v re="$BANNED_AXIOM_RE" '{ n = split($3, a, " ")
    for (i = 1; i <= n; i++) if (a[i] ~ re) { print $1 " (" a[i] ")"; break } }' "$1"
}
# declared FILE → the axioms a PROOF_MODE=constants listing declares
declared() { awk -F'\t' '$2 == "axiom" { print $1 }' "$1"; }
# scan FILE... → `file:line: text` for every banned word outside comments and strings
scan() {
  python3 - "$BANNED_WORD_RE" "$@" <<'EOF'
import re, sys
word = re.compile(sys.argv[1])
for path in sys.argv[2:]:
    src = open(path, encoding="utf-8").read()
    out, i, depth, n = [], 0, 0, len(src)
    while i < n:                      # blank out comments (nested /- -/, --) and string literals
        if depth:
            if src.startswith("/-", i): depth += 1; out.append("  "); i += 2
            elif src.startswith("-/", i): depth -= 1; out.append("  "); i += 2
            else: out.append("\n" if src[i] == "\n" else " "); i += 1
        elif src.startswith("/-", i): depth = 1; out.append("  "); i += 2
        elif src.startswith("--", i):
            j = src.find("\n", i); j = n if j < 0 else j; out.append(" " * (j - i)); i = j
        elif src[i] == '"':
            j = i + 1
            while j < n and src[j] != '"': j += 2 if src[j] == "\\" else 1
            out.append(" " * (min(j + 1, n) - i)); i = j + 1
        elif src[i] == "'" and i + 2 < n and src[i + 2] == "'":   # a character literal like '"'
            out.append("   "); i += 3
        else: out.append(src[i]); i += 1
    for k, line in enumerate("".join(out).split("\n"), 1):
        if word.search(line): print(f"{path}:{k}: {line.strip()}")
EOF
}

echo "=== E/D: the environment ==="
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
W=$(scan "${SRC[@]}")
[ "${#SRC[@]}" -ge 10 ] && [ -z "$W" ] && ok "none of the ${#SRC[@]} source files spells a banned word outside comments and strings" \
  || no "banned words in the sources (${#SRC[@]} files): $(printf '%s\n' "$W" | head -5 | tr '\n' '|')"

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
def msg : String := "a string may say \"sorry\" too"
def quote : Char := '"'
example : 2 + 2 = 4 := by native_decide
EOF
W=$(scan "$TMP/src/Bad.lean")
[ "$W" = "$TMP/src/Bad.lean:5: example : 2 + 2 = 4 := by native_decide" ] \
  && ok "control: an example proved by native_decide is found, and the words in comments and strings are not" \
  || no "control: source scan reported '$W'"
finish
