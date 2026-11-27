#!/usr/bin/env bash
# TRUST BAN: no `sorry` and no `native_decide` anywhere in the specification or compiler (Q9.17), checked
# transitively in the kernel's terms and in the sources the environment does not keep.
#
#   I  every proof module, recursively enumerated, imports exactly the modules defining its
#      directly referenced constants (including anonymous examples); see scripts/proofs/IMPORTS.md.
#   E  no constant of ANY kind the three libraries declare (definitions, instances and
#      generated helpers included, not only theorems) depends transitively on sorryAx, on
#      Lean.ofReduceBool or Lean.ofReduceNat (what native_decide and `decide +native` reduce through),
#      on Lean.trustCompiler, or on a generated `._native.` axiom;
#   D  none of the libraries declares an axiom of its own;
#   X  every `example`, which the kernel checks and Lean then discards (so E and D never see it),
#      depends only on the standard axioms: in a copy of the library each `example` becomes a
#      named declaration (scripts/proofs/lean_lex.py), the copy is built, and its axioms are
#      collected as E collects a theorem's;
#   S  defence in depth, lexical: the library builds with warningAsError and nothing in lakefile.toml
#      can change that or point the build at other sources; the sources, comments stripped and
#      string literals kept, never spell sorry, sorryAx, admit, native_decide, +native, an
#      option on `decide` (the compiler may use a single `+kernel`), ofReduceBool, ofReduceNat,
#      trustCompiler or skipKernelTC, and define no
#      syntax extension, run no meta code, set no option and import nothing outside the library
#      (the lexer reads what Lean reads only for syntax it knows).
#
# IMPORT CONTROLS: unused and transitive-only imports, open namespaces, recursive/nonempty
# populations, and comment/EOL-preserving idempotent header rewrites are checked by imports.py.
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

# Exact direct imports are part of this required Lean-enabled gate.
if python3 scripts/proofs/imports.py; then
  ok "direct proof imports and controls"
else
  no "direct proof imports or controls failed"
fi

# Identity and diagnostics are structural trust properties; the environment scan is load-bearing.
# Compiling negative controls cover spelling-carrier forms and the numeric/lookup invariants.
if python3 scripts/proofs/local_identity.py; then
  ok "numeric local identity, structural diagnostics separation and negative controls"
else
  no "local identity or a negative control failed"
fi

BANNED_AXIOM_RE='^(sorryAx|Lean[.]ofReduceBool|Lean[.]ofReduceNat|Lean[.]trustCompiler)$|[.]_native[.]'
TRUST_WORD_RE='\b(sorry|sorryAx|admit|native_decide|ofReduceBool|ofReduceNat|trustCompiler|skipKernelTC)\b|\+native\b|\bnative»?\s*:=|\bDecide[.]Config\b'
BANNED_WORD_RE="$TRUST_WORD_RE"'|\bdecide\s*(?:\(\s*config\b|[+-])'
# A single +kernel moves closed reduction into Lean's kernel. Keep all other compiler
# configurations barred, including additional options on subsequent lines.
DECIDE_CONFIG_ITEM_RE='[+-]|\(\s*(?:«[^»]*»|[^\s():]+)\s*:='
COMPILER_WORD_RE="$TRUST_WORD_RE|\bdecide\s*(?:(?!\+kernel\b)(?:$DECIDE_CONFIG_ITEM_RE)|\+kernel\b\s*(?:$DECIDE_CONFIG_ITEM_RE))"
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
scan_evidence() { python3 "$PROOFS_DIR/lean_lex.py" scan-evidence "$BANNED_WORD_RE|$EXTENSION_RE" "$@"; }
scan() { python3 "$PROOFS_DIR/lean_lex.py" scan "$BANNED_WORD_RE|$EXTENSION_RE" "$@"; }

echo "=== E/D: the environment ==="
for pkg in ConcreteSpec ConcreteSpecEvidence; do
  PROOF_PKG="$pkg" population imported > "$TMP/$pkg-imported.txt" \
    || { no "$pkg import enumeration failed"; finish; }
  GAP=$(closure_gap "$TMP/$pkg-imported.txt" "$pkg") \
    || { no "$pkg root module comparison failed"; finish; }
  [ -z "$GAP" ] && ok "all $pkg source modules are in the checked import closure" \
    || no "$pkg source modules outside the checked closure: $GAP"
  PROOF_PKG="$pkg" population environment > "$TMP/$pkg-environment.txt" \
    || { no "$pkg environment module enumeration failed"; finish; }
  GAP=$(closure_gap "$TMP/$pkg-environment.txt" "$pkg") \
    || { no "$pkg environment module comparison failed"; finish; }
  [ -z "$GAP" ] || { no "$pkg source modules outside the constant environment: $GAP"; finish; }
  PROOF_PKG="$pkg" population constants > "$TMP/consts.tsv" \
    || { no "$pkg constant enumeration failed"; finish; }
  NC=$(grep -c . "$TMP/consts.tsv")
  minimum=1
  [ "$pkg" != ConcreteSpec ] || minimum=1000
  B=$(banned "$TMP/consts.tsv")
  A=$(declared "$TMP/consts.tsv")
  [ "$NC" -ge "$minimum" ] && [ -z "$B$A" ] \
    && ok "all $NC $pkg constants obey the axiom ban" \
    || no "$pkg: $NC constants; banned dependencies: $B; declared axioms: $A"
done

echo "=== S: the sources ==="
mapfile -t SRC < <({ echo ConcreteSpec.lean; find ConcreteSpec -name '*.lean'; } | sort)
W=$(scan "${SRC[@]}"); RC=$?
[ "$RC" = 0 ] && [ "${#SRC[@]}" -ge 10 ] && [ -z "$W" ] && ok "none of the ${#SRC[@]} source files spells a banned word outside comments" \
  || no "the source scan (rc=$RC) of ${#SRC[@]} files: $(printf '%s\n' "$W" | head -5 | tr '\n' '|')"

{ echo ConcreteSpecEvidence.lean && find ConcreteSpecEvidence -name '*.lean'; } > "$TMP/evidence-sources.txt" \
  || { no "evidence source inventory failed"; finish; }
mapfile -t EVIDENCE_SRC < "$TMP/evidence-sources.txt"
[ "${#EVIDENCE_SRC[@]}" -gt 1 ] || { no "evidence source inventory contains only the root"; finish; }
W=$(scan_evidence "${EVIDENCE_SRC[@]}"); RC=$?
[ "$RC" = 0 ] && [ -z "$W" ] && ok "all evidence sources obey the trust ban" \
  || no "evidence source scan (rc=$RC): $(printf '%s\n' "$W" | head -5 | tr '\n' '|')"
printf '%s\n' 'import ConcreteSpecEvidence' 'def probe : Nat := 0' > "$TMP/reverse-import.lean"
W=$(scan "$TMP/reverse-import.lean"); RC=$?
[ "$RC" = 0 ] && [[ "$W" == *'import ConcreteSpecEvidence'* ]] \
  && ok "control: a specification import of evidence is refused" \
  || no "the source scan allowed a specification import of evidence"

# Each fixture is scanned through the production evidence entry point.
for command in 'import Lean' 'import Std' 'import Concrete' 'def bad := sorry' 'set_option maxRecDepth 100000'; do
  printf '%s\n' 'import ConcreteSpec.Spec.Basics' "$command" > "$TMP/evidence-control.lean"
  if W=$(scan_evidence "$TMP/evidence-control.lean") && grep -qxF "$TMP/evidence-control.lean:2: $command" <<<"$W"; then
    ok "control: evidence scan rejects $command"
  else
    no "control: evidence scan did not reject $command ($W)"
  fi
done

# Compiler sources obey the trust-word, configuration and extension ban. Compiler imports
# include Lean's premise-reporting API and the pinned backend, so scan their header
# grammar separately; no compiler file is exempt from the ban.
mapfile -t COMPILER_SRC < <({ printf "%s\n" Concrete.lean Main.lean; find Concrete -name '*.lean'; } | sort)
scan_compiler() { python3 "$PROOFS_DIR/lean_lex.py" scan-compiler "$COMPILER_WORD_RE|$EXTENSION_RE" "$@"; }
W=$(scan_compiler "${COMPILER_SRC[@]}"); RC=$?
[ "$RC" = 0 ] && [ -z "$W" ] && ok "all compiler sources obey the trust ban" \
  || no "compiler source scan (rc=$RC): $(printf '%s\n' "$W" | head -5 | tr '\n' '|')"

cat > "$TMP/kernel-decide.lean" <<'EOF'
namespace KernelDecideControl
example : 2 + 2 = 4 := by decide +kernel
example : 2 + 2 = 4 := by decide +native
example : 2 + 2 = 4 := by decide +kernel +native
example : 2 + 2 = 4 := by decide +kernel -kernel
example : 2 + 2 = 4 := by
  decide +kernel
    («config» := ⟨false, true, true, false⟩)
example : 2 + 2 = 4 := by decide (config := ⟨false, true, true, false⟩)
end KernelDecideControl
EOF
W=$(scan_compiler "$TMP/kernel-decide.lean"); RC=$?
B="$TMP/kernel-decide.lean"
EXPECT="$B:3: example : 2 + 2 = 4 := by decide +native
$B:4: example : 2 + 2 = 4 := by decide +kernel +native
$B:5: example : 2 + 2 = 4 := by decide +kernel -kernel
$B:7: decide +kernel
$B:9: example : 2 + 2 = 4 := by decide (config := ⟨false, true, true, false⟩)"
[ "$RC" = 0 ] && [ "$W" = "$EXPECT" ] \
  && ok "control: the compiler scan permits a single +kernel and rejects native and other configurations" \
  || no "control: compiler decide scan (rc=$RC) reported '$W'"

# Read every compiler constant as well, including declarations not reachable from
# the headline theorem registry. The compiler root must import every source module.
"$LAKE" build Concrete > "$TMP/compiler-build.log" 2>&1 \
  || { cat "$TMP/compiler-build.log" >&2; no "compiler build failed"; finish; }
PROOF_PKG=Concrete population constants > "$TMP/compiler-consts.tsv" \
  || { no "compiler constant enumeration failed"; finish; }
B=$(banned "$TMP/compiler-consts.tsv"); A=$(declared "$TMP/compiler-consts.tsv")
[ -s "$TMP/compiler-consts.tsv" ] && [ -z "$B$A" ] \
  && ok "all compiler constants obey the axiom ban" \
  || no "compiler banned dependencies: $B; declared axioms: $A"
PROOF_PKG=Concrete population imported > "$TMP/compiler-imported.txt" \
  || { no "compiler import enumeration failed"; finish; }
{ echo Concrete.lean; find Concrete -name '*.lean'; } | sed -E 's#\.lean$##; s#/#.#g' | sort -u > "$TMP/compiler-modules.txt"
GAP=$(comm -23 "$TMP/compiler-modules.txt" <(sort -u "$TMP/compiler-imported.txt"))
[ -z "$GAP" ] && ok "all compiler modules are in the checked import closure" \
  || no "compiler modules outside checked closure: $GAP"

# lake_options LAKEFILE → empty when the specification library builds from ConcreteSpec.lean and
# ConcreteSpec/ (Lake's defaults: no srcDir, roots or globs) with warningAsError, and nothing can turn
# it off. The stage-control library builds every module under its fixed glob. The compiler
# executable is checked separately. Its backend dependency must be a git URL pinned to a full commit and
# recorded with the same URL and commit in lake-manifest.json. The specification and evidence
# import directions are checked in the source scan.
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
extra = set(cfg) - {"name", "version", "defaultTargets", "leanOptions", "lean_lib", "lean_exe", "require"}
if extra: bad.append(f"the package sets {sorted(extra)}")
if set(cfg.get("leanOptions", {})) != {"warningAsError"}:
    bad.append(f"[leanOptions] sets {sorted(set(cfg.get('leanOptions', {})) - {'warningAsError'})}")
import json, re
reqs = cfg.get("require", [])
if reqs:
    try:
        man = json.load(open(os.path.join(os.path.dirname(sys.argv[1]) or ".", "lake-manifest.json")))
        pkgs = {p.get("name", "").strip("«»"): p for p in man.get("packages", [])}
    except Exception as e:
        bad.append(f"lake-manifest.json unreadable: {e}"); pkgs = {}
    for r in reqs:
        n = r.get("name")
        if set(r) != {"name", "git", "rev"}:
            bad.append(f"the require {n} sets {sorted(set(r) ^ {'name', 'git', 'rev'})}"); continue
        if not re.fullmatch(r"[0-9a-f]{40}", str(r["rev"])):
            bad.append(f"the require {n} is not pinned to a full commit ({r['rev']})")
        p = pkgs.get(n)
        if not p or p.get("type") != "git" or p.get("url") != r["git"] or p.get("rev") != r["rev"] or p.get("inputRev") != r["rev"]:
            bad.append(f"lake-manifest.json does not record the require {n} at {r['git']} {r['rev']}")
libs = cfg.get("lean_lib", [])
controls = "ConcreteChecks.StageInterfaces"
stage_library = {"name": controls, "globs": ["ConcreteChecks.+"],
                 "leanOptions": {"warningAsError": True, "autoImplicit": False}}
if [l for l in libs if l.get("name") == controls] != [stage_library]:
    bad.append("the stage-interface controls library configuration differs")
if controls not in cfg.get("defaultTargets", []):
    bad.append("the default build omits the stage-interface controls")
libs = [l for l in libs if l.get("name") != controls]
if [l.get("name") for l in libs] != ["ConcreteSpec", "ConcreteSpecEvidence", "Concrete"]: bad.append(f"libraries {[l.get('name') for l in libs]}")
for l in libs:
    extra = set(l) - {"name", "leanOptions"}
    if extra: bad.append(f"the library sets {sorted(extra)}")
    opts = set(l.get("leanOptions", {})) - {"autoImplicit"}
    if opts: bad.append(f"the library sets options {sorted(opts)}")
exe = cfg.get("lean_exe", [])
expected_exe = {
    "name": "concrete",
    "root": "Main",
    "needs": ["lean-compiler-backend/lcb"],
    "leanOptions": {"autoImplicit": False},
}
if exe != [expected_exe]:
    bad.append("the executable configuration differs from the fixed concrete CLI")
print("; ".join(bad))
EOF
}
L=$(lake_options lakefile.toml)
[ -z "$L" ] && ok "lakefile.toml builds the library with warningAsError, and nothing overrides it" || no "lakefile.toml: $L"

echo "=== X: the examples, in the kernel's terms ==="
# A copy of the library in which every `example` is a named declaration, built, and its axioms
# collected as E collects a theorem's. Each example becomes a noncomputable abbreviation.
# warningAsError stays off in this axiom-only copy: named abbreviations can trigger declaration
# linters that anonymous examples do not; warnings remain errors in the original library.
# Give a scratch build its own verified pinned dependency checkout via the shared helper.
# Never symlink $SEED_DIR (doing so would inherit local backend edits and prebuilt .olean
# outputs from the caller's workspace). Passing LAKE_SEED_PACKAGES_DIR only allows
# extracting cryptographically verified pinned commit objects from the caller's git object
# store into a fresh source-only checkout, falling back to a remote fetch on mismatch.
prepare_scratch_packages() {
  local seed_dir="$1"
  local workspace_dir="$2"
  (cd "$workspace_dir" && LAKE_SEED_PACKAGES_DIR="$seed_dir" bash "$ROOT_DIR/scripts/checks/lib/fetch_lake_packages.sh")
  [ ! -L "$workspace_dir/.lake/packages" ]
}

EX="$TMP/ex"; mkdir -p "$EX"
cp lakefile.toml lean-toolchain lake-manifest.json "$EX"/ || { no "could not copy the Lake files"; finish; }
prepare_scratch_packages "$ROOT_DIR/.lake/packages" "$EX" > "$TMP/ex-packages.log" 2>&1 \
  || { no "could not prepare verified pinned packages for examples workspace: $(tail -5 "$TMP/ex-packages.log" | tr '\n' ' ')"; finish; }
sed -i 's/^warningAsError = true$/warningAsError = false/' "$EX/lakefile.toml"
cp "$ENUMERATOR" "$TMP/examples-enumerator.lean"
NEX=$(python3 "$PROOFS_DIR/lean_lex.py" examples . "$EX" "${SRC[@]}" "${EVIDENCE_SRC[@]}" "${COMPILER_SRC[@]}") \
  || { no "the examples could not be rewritten: $NEX"; finish; }
(cd "$EX" && "$LAKE" build ConcreteSpec ConcreteSpecEvidence Concrete) > "$TMP/ex-build.log" 2>&1 \
  || { no "the library with its examples as declarations does not build: $(grep -m3 error "$TMP/ex-build.log" | tr '\n' ' ')"; finish; }
(cd "$EX" && PROOF_MODE=examples run_enumerator "$TMP/examples-enumerator.lean") > "$TMP/examples.out" 2> "$TMP/examples.err" \
  || { no "PROOF_MODE=examples failed: $(head -2 "$TMP/examples.err" | tr '\n' ' ')"; finish; }
grep -P '\t' "$TMP/examples.out" > "$TMP/examples.tsv"
# nonstandard FILE → `<name> (<axiom>)` for every row whose axioms are not among the standard three
nonstandard() {
  awk -F'\t' -v std=" $STANDARD_AXIOMS " '{ n = split($2, a, " ")
    for (i = 1; i <= n; i++) if (index(std, " " a[i] " ") == 0) { print $1 " (" a[i] ")"; break } }' "$1"
}
NE=$(grep -c . "$TMP/examples.tsv"); X=$(nonstandard "$TMP/examples.tsv")
[ "$NEX" -ge 100 ] && [ "$NE" = "$NEX" ] && [ -z "$X" ] \
  && ok "all $NEX spec, evidence and compiler examples, kept as declarations, depend only on the standard axioms" \
  || no "examples: $NEX rewritten, $NE in the environment; on other axioms: $(echo $X | cut -c1-300)"

echo "=== negative controls ==="
python3 "$PROOFS_DIR/test_lean_lex.py" > "$TMP/lexer-tests.log" 2>&1; RC=$?
[ "$RC" = 0 ] && ok "control: literal boundaries, interpolation spoofing and compiler module graph regressions" \
  || { cat "$TMP/lexer-tests.log" >&2; no "lexer regression controls (rc=$RC)"; }
# Bypass the evidence umbrella to simulate dropping its import of the spec root,
# while keeping the evidence leaf dependencies available to the enumerator. Isolate
# the two libraries so compiler import changes do not alter this control.
awk 'FNR == NR { if ($0 ~ /^import ConcreteSpecEvidence[.]/) imports = imports $0 "\n"; next }
     $0 == "import ConcreteSpecEvidence" { printf "%s", imports; next }
     $0 != "import Concrete" { print }' ConcreteSpecEvidence.lean "$ENUMERATOR" > "$TMP/no-umbrella.lean"
sed '/^import ConcreteSpec$/d' "$TMP/no-umbrella.lean" > "$TMP/no-spec-root.lean"
if PROOF_PKG=ConcreteSpec PROOF_MODE=environment run_enumerator "$TMP/no-umbrella.lean" > "$TMP/no-umbrella.txt" &&
   PROOF_PKG=ConcreteSpec PROOF_MODE=environment run_enumerator "$TMP/no-spec-root.lean" > "$TMP/no-spec-root.txt" &&
   whole=$(closure_gap "$TMP/no-umbrella.txt" ConcreteSpec) && [ -z "$whole" ] &&
   missing=$(closure_gap "$TMP/no-spec-root.txt" ConcreteSpec) && grep -qxF ConcreteSpec <<<"$missing"; then
  ok "control: the direct spec import keeps the constant environment complete without the evidence umbrella"
else
  no "control: bypassing the evidence umbrella did not leave coverage dependent on the direct spec import"
fi
# Exercise the compiler CLI itself: these must be reported, even with an otherwise valid header.
for body in 'import Std' 'import Lean.DoesNotExist' 'import ConcreteSpecEvidence' 'def bad : Nat := sorry' 'set_option maxRecDepth 100000'; do
  printf 'import Concrete.Lex\n%s\n' "$body" > "$TMP/compiler-bad.lean"
  W=$(python3 "$PROOFS_DIR/lean_lex.py" scan-compiler "$BANNED_WORD_RE|$EXTENSION_RE" "$TMP/compiler-bad.lean"); RC=$?
  [ "$RC" = 0 ] && [[ "$W" == *"$body"* ]] \
    && ok "control: compiler scan reports $body" || no "compiler scan missed $body (rc=$RC): $W"
done
# control NAME BODY AXIOM-ERE WHAT: the declarations in BODY must be refused at ${PROOF_PKG}Control.NAME
control() {
  printf '%s\n' "$2" > "$TMP/c-$1.lean"
  local got
  if ! population constants "$TMP/c-$1.lean" > "$TMP/c-$1.tsv" 2> "$TMP/c-$1.err"; then
    no "control: $PROOF_PKG constant enumeration failed for $1: $(head -2 "$TMP/c-$1.err")"
    return
  fi
  got=$(banned "$TMP/c-$1.tsv" | grep -F "${PROOF_PKG}Control.$1 (")
  has_match -E "\(($3)\)\$" <<<"$got" \
    && ok "control: $4 is refused ($got)" || no "control: $4 was not refused with $3: '$got' $(head -2 "$TMP/c-$1.err" | tr '\n' ' ')"
}
environment_controls() {
  local PROOF_PKG=$1
  export PROOF_PKG
  control sorryThm "theorem ${PROOF_PKG}Control.sorryThm : 1 = 2 := sorry" 'sorryAx' "a theorem proved by sorry"
  control sorryDef "def ${PROOF_PKG}Control.sorryDef : Nat := sorry" sorryAx "a definition with a sorry body and no theorem about it"
  control nativeThm "theorem ${PROOF_PKG}Control.nativeThm : 2 + 2 = 4 := by native_decide" 'Lean\.ofReduceBool|.*\._native\..*' "a theorem proved by native_decide"
  printf 'axiom %sControl.ax : 1 = 2\n' "$PROOF_PKG" > "$TMP/c-ax.lean"
  population constants "$TMP/c-ax.lean" > "$TMP/c-ax.tsv" &&
    [ "$(declared "$TMP/c-ax.tsv")" = "${PROOF_PKG}Control.ax" ] \
    && ok "control: a declared axiom is reported" || no "control: a declared axiom was not reported: '$(declared "$TMP/c-ax.tsv")'"
}
environment_controls ConcreteSpec
environment_controls ConcreteSpecEvidence
# A quoted name holding a tab would shift its row's columns (the axiom kind out of column 2) and
# hide the axiom from `declared`; theorems.lean refuses every such name before writing a row.
printf 'axiom ConcreteSpecControl.«a\tb» : 1 = 2\n' > "$TMP/c-tab.lean"
if population constants "$TMP/c-tab.lean" > "$TMP/c-tab.tsv" 2> "$TMP/c-tab.err"; then
  no "control: an axiom whose name holds a tab did not fail the run: '$(declared "$TMP/c-tab.tsv")'"
else
  grep -q 'control character' "$TMP/c-tab.err" "$TMP/c-tab.tsv" \
    && ok "control: an axiom whose name holds a tab fails the run (no row can carry it)" \
    || no "control: the tab-name run failed for another reason: $(head -2 "$TMP/c-tab.err" "$TMP/c-tab.tsv" | tr '\n' ' ')"
fi
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
macro "plain" "s!" s:str : term => pure s
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
$B:14: macro \"plain\" \"s!\" s:str : term => pure s
$B:15: import Lean
$B:16: set_option warningAsError false in
$B:18: run_cmd pure ()
$B:19: #eval (pure () : IO Unit)
$B:20: example : 2 + 2 = 4 := by decide (config := { native
$B:22: example : 2 + 2 = 4 := by decide (config := ⟨false, true, true, false⟩)"
[ "$RC" = 0 ] && [ "$W" = "$EXPECT" ] \
  && ok "control: native_decide, and sorry inside an interpolation, after a string, raw string or quoted name holding comment delimiters, after an interpolation holding a string, a direct sorryAx, a syntax extension, an outside import, an option, meta code and decide options (native split across lines, positional) are found; the words in comments are not" \
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
has_match -F "the library sets options ['warningAsError']" <<<"$L" && ok "control: a library that turns warningAsError off is refused ($L)" \
  || no "control: a lakefile turning warningAsError off was accepted"
printf '[leanOptions]\nwarningAsError = true\n\n[[lean_lib]]\nname = "ConcreteSpec"\nsrcDir = "shadow"\n' > "$TMP/lake-src.toml"
L=$(lake_options "$TMP/lake-src.toml")
has_match -F "the library sets ['srcDir']" <<<"$L" && ok "control: a library built from another source directory is refused ($L)" \
  || no "control: a lakefile with srcDir was accepted"
# The dependency controls run on a copy of this lakefile, beside a manifest, so each varies one thing.
mkdir -p "$TMP/lk"
lake_variant() { # NAME PYTHON-EDIT: lakefile.toml and lake-manifest.json copied to $TMP/lk/NAME, then edited
  mkdir -p "$TMP/lk/$1"; cp lakefile.toml lake-manifest.json "$TMP/lk/$1/"
  (cd "$TMP/lk/$1" && python3 -I -c "$2")
}
lake_variant short 'import re; p="lakefile.toml"; s=open(p).read(); r=re.search("[0-9a-f]{40}", s).group(0); open(p,"w").write(s.replace(r, r[:12]))'
lake_variant branch 'import re; p="lakefile.toml"; s=open(p).read(); open(p,"w").write(re.sub(r"rev = \"[0-9a-f]+\"", "rev = \"main\"", s))'
lake_variant drift 'import json; p="lake-manifest.json"; m=json.load(open(p)); m["packages"][0]["rev"]="0"*40; json.dump(m, open(p,"w"))'
lake_variant subdir 'p="lakefile.toml"; s=open(p).read(); open(p,"w").write(s.replace("[[require]]\n", "[[require]]\nsubDir = \"x\"\n", 1))'
lake_variant extralib 'p="lakefile.toml"; s=open(p).read(); open(p,"w").write(s + "\n[[lean_lib]]\nname = \"Other\"\n")'
lake_variant extraexe 'p="lakefile.toml"; s=open(p).read(); open(p,"w").write(s + "\n[[lean_exe]]\nname = \"Other\"\n")'
lake_variant stageglob 'p="lakefile.toml"; s=open(p).read(); open(p,"w").write(s.replace("ConcreteChecks.+", "ConcreteChecks.StageInterfaces.+"))'
lake_variant stagedefault 'p="lakefile.toml"; s=open(p).read(); open(p,"w").write(s.replace(", \"ConcreteChecks.StageInterfaces\"]", "]"))'
for v in short:"not pinned to a full commit" branch:"not pinned to a full commit" drift:"does not record the require" subdir:"sets ['subDir']" extralib:"libraries ['ConcreteSpec', 'ConcreteSpecEvidence', 'Concrete', 'Other']" extraexe:"the executable configuration differs" \
         stageglob:"the stage-interface controls library configuration differs" stagedefault:"the default build omits the stage-interface controls"; do
  L=$(lake_options "$TMP/lk/${v%%:*}/lakefile.toml")
  case "$L" in *"${v#*:}"*) ok "control: a lakefile whose dependency or library set is changed (${v%%:*}) is refused ($L)" ;;
    *) no "control: the ${v%%:*} lakefile was not refused for '${v#*:}': '$L'" ;; esac
done
for f in missing Open Quote Brace; do
  scan "$TMP/src/$f.lean" > /dev/null 2>&1; RC=$?
  [ "$RC" != 0 ] && ok "control: a source the scan cannot read or lex ($f) fails it (rc=$RC)" \
    || no "control: the scan of $f exited 0"
done
for pkg in ConcreteSpec ConcreteSpecEvidence; do
  removed=$(tail -1 "$TMP/$pkg-imported.txt")
  sed '$d' "$TMP/$pkg-imported.txt" > "$TMP/imp-short.txt"
  if [ -n "$removed" ] && GAP=$(closure_gap "$TMP/imp-short.txt" "$pkg") && [ "$GAP" = "$removed" ]; then
    ok "control: removing $removed from the complete $pkg closure reports exactly that module"
  else
    no "control: removing $removed from the $pkg closure reported: $GAP"
  fi
done
# The example leg's own controls: examples rewritten as that leg rewrites them, then enumerated.
mkdir -p "$TMP/exc-src" "$TMP/exc"
printf 'example : 2 + 2 = 4 := by decide +native\nexample : 1 = 2 := by stop skip\nexample : 1 = 1 := rfl\ndef w : String := "example"\n' > "$TMP/exc-src/C.lean"
N=$(python3 "$PROOFS_DIR/lean_lex.py" examples "$TMP/exc-src" "$TMP/exc" C.lean)
# (Lean prints the warnings of the inserted declarations on stdout too; the rows are the tab lines.)
population examples "$TMP/exc/C.lean" 2>/dev/null | grep -P '\t' > "$TMP/exc.tsv"
X=$(nonstandard "$TMP/exc.tsv")
[ "$N" = 3 ] && [ "$(grep -c . "$TMP/exc.tsv")" = 3 ] && has_match -F "C.lean:1» (" <<<"$X" && has_match -F "C.lean:2» (sorryAx)" <<<"$X" \
  && ! has_match -F "C.lean:3»" <<<"$X" \
  && ok "control: an example proved by decide +native and one closed by stop are reported, rfl is not, and the word example in a string is no example ($(echo $X | tr '\n' ' '))" \
  || no "control: rewritten examples: $N, rows $(grep -c . "$TMP/exc.tsv"), reported '$X'"
# Finding 2 (ISS-0130) negative control: the scratch examples workspace must never inherit
# local dependency edits or prebuilt .olean outputs from the caller's .lake/packages.
if (
  set -euo pipefail
  source "$ROOT_DIR/scripts/checks/lib/git_sandbox.sh" || exit 2
  git_sandbox "$TMP/scratch-ctl-gitconfig" || exit 2
  cat >"$GIT_CONFIG_GLOBAL" <<'EOF'
[user]
	name = trust-ban scratch control
	email = control@example.invalid
[init]
	defaultBranch = main
[advice]
	detachedHead = false
EOF
  d="$TMP/scratch-isolation"
  mkdir -p "$d/dep-src" "$d/caller" "$d/scratch"
  cp "$ROOT_DIR/lean-toolchain" "$d/dep-src/"
  printf 'name = "scratchDep"\n[[lean_lib]]\nname = "ScratchDep"\n' >"$d/dep-src/lakefile.toml"
  printf 'def pinnedVal : Nat := 1\n' >"$d/dep-src/ScratchDep.lean"
  git init -q "$d/dep-src"
  git -C "$d/dep-src" add lakefile.toml ScratchDep.lean lean-toolchain
  git -C "$d/dep-src" commit -qm "pinned"
  dep_rev="$(git -C "$d/dep-src" rev-parse HEAD)"
  git clone -q --bare "$d/dep-src" "$d/dep.git"
  for ws in "$d/caller" "$d/scratch"; do
    cp "$ROOT_DIR/lean-toolchain" "$ws/"
    cat >"$ws/lakefile.toml" <<EOF
name = "scratchCtl"
[[require]]
name = "scratchDep"
git = "$d/dep.git"
rev = "$dep_rev"
[[lean_lib]]
name = "ScratchMain"
EOF
    cat >"$ws/lake-manifest.json" <<EOF
{"version": "1.3.0",
 "packagesDir": ".lake/packages",
 "packages":
 [{"url": "$d/dep.git",
   "type": "git",
   "subDir": null,
   "scope": "",
   "rev": "$dep_rev",
   "name": "scratchDep",
   "manifestFile": "lake-manifest.json",
   "inputRev": "$dep_rev",
   "inherited": false,
   "configFile": "lakefile.toml"}],
 "name": "scratchCtl",
 "lakeDir": ".lake",
 "fixedToolchain": false}
EOF
    printf 'import ScratchDep\nexample : localAddedLemma = 42 := rfl\n' >"$ws/ScratchMain.lean"
  done
  (cd "$d/caller" && LAKE_FETCH_BACKOFF_BASE=0 bash "$ROOT_DIR/scripts/checks/lib/fetch_lake_packages.sh")
  printf 'def localAddedLemma : Nat := 42\n' >>"$d/caller/.lake/packages/scratchDep/ScratchDep.lean"
  # Caller builds with the locally added backend lemma (producing .olean outputs in caller/.lake/packages).
  (cd "$d/caller" && "$LAKE" build ScratchMain)
  # Take the remote offline to prove seeding uses only verified pinned objects with zero network access,
  # while rejecting the caller's locally added backend lemma and prebuilt .olean outputs.
  mv "$d/dep.git" "$d/dep-offline.git"
  prepare_scratch_packages "$d/caller/.lake/packages" "$d/scratch"
  [ ! -e "$d/scratch/.lake/packages/scratchDep/.lake" ]
  if (cd "$d/scratch" && "$LAKE" build ScratchMain) >"$d/scratch-bad.log" 2>&1; then
    echo "scratch build unexpectedly accepted locally added backend lemma" >&2
    exit 1
  fi
  has_match -F "localAddedLemma" <"$d/scratch-bad.log"
  # Clean example using only pinnedVal builds cleanly in the independent scratch workspace.
  printf 'import ScratchDep\nexample : pinnedVal = 1 := rfl\n' >"$d/scratch/ScratchMain.lean"
  (cd "$d/scratch" && "$LAKE" build ScratchMain)
) >"$TMP/scratch-isolation.log" 2>&1; then
  ok "control: examples scratch workspace rejects an example using a lemma added locally to the caller's backend"
else
  tail -20 "$TMP/scratch-isolation.log" >&2
  no "control: examples scratch workspace isolation control failed"
fi
finish
