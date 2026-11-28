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
# Assert WORKSPACE_DIR has a real .lake/packages directory containing a verified,
# non-symlink checkout at the exact pinned revision for every package in lake-manifest.json.
verify_scratch_packages_exist() {
  local workspace_dir="$1"
  [ -d "$workspace_dir" ] && [ ! -L "$workspace_dir" ] || {
    echo "verify_scratch_packages_exist: missing or symlinked workspace $workspace_dir" >&2
    return 1
  }
  [ -d "$workspace_dir/.lake" ] && [ ! -L "$workspace_dir/.lake" ] || {
    echo "verify_scratch_packages_exist: missing or symlinked $workspace_dir/.lake" >&2
    return 1
  }
  [ -d "$workspace_dir/.lake/packages" ] && [ ! -L "$workspace_dir/.lake/packages" ] || {
    echo "verify_scratch_packages_exist: missing or symlinked $workspace_dir/.lake/packages" >&2
    return 1
  }
  python3 -I - "$workspace_dir" <<'PY' || return 1
import json
import os
import re
import stat
import sys

ws = sys.argv[1]
manifest_path = os.path.join(ws, "lake-manifest.json")
try:
    st_man = os.lstat(manifest_path)
    if stat.S_ISLNK(st_man.st_mode) or not stat.S_ISREG(st_man.st_mode):
        sys.exit(f"verify_scratch_packages_exist: {manifest_path} is not a regular file")
    with open(manifest_path, "r", encoding="utf-8") as fh:
        data = json.load(fh)
except Exception as exc:
    sys.exit(f"verify_scratch_packages_exist: unreadable {manifest_path}: {exc}")

if not isinstance(data, dict) or data.get("packagesDir") != ".lake/packages":
    sys.exit("verify_scratch_packages_exist: unexpected packagesDir in lake-manifest.json")
packages = data.get("packages")
if not isinstance(packages, list):
    sys.exit("verify_scratch_packages_exist: packages field is not a list")

pkgs_dir = os.path.join(ws, ".lake", "packages")
for entry in packages:
    raw = entry.get("name", "")
    name = raw[1:-1] if raw.startswith("«") and raw.endswith("»") else raw
    rev = entry.get("rev", "")
    if not name or not re.fullmatch(r"[0-9a-f]{40}", rev):
        sys.exit(f"verify_scratch_packages_exist: invalid manifest entry {entry!r}")
    pkg = os.path.join(pkgs_dir, name)
    try:
        st = os.lstat(pkg)
    except OSError as exc:
        sys.exit(f"verify_scratch_packages_exist: missing package {pkg}: {exc}")
    if stat.S_ISLNK(st.st_mode) or not stat.S_ISDIR(st.st_mode):
        sys.exit(f"verify_scratch_packages_exist: {pkg} is not a real directory")
    git_dir = os.path.join(pkg, ".git")
    try:
        gst = os.lstat(git_dir)
    except OSError as exc:
        sys.exit(f"verify_scratch_packages_exist: missing .git in {pkg}: {exc}")
    if stat.S_ISLNK(gst.st_mode) or not stat.S_ISDIR(gst.st_mode):
        sys.exit(f"verify_scratch_packages_exist: {git_dir} is not a real directory")
    if os.path.lexists(os.path.join(pkg, ".lake")):
        sys.exit(f"verify_scratch_packages_exist: unexpected .lake in {pkg}")
    head_path = os.path.join(git_dir, "HEAD")
    try:
        hst = os.lstat(head_path)
        if stat.S_ISLNK(hst.st_mode) or not stat.S_ISREG(hst.st_mode):
            sys.exit(f"verify_scratch_packages_exist: invalid HEAD in {pkg}")
        with open(head_path, "r", encoding="utf-8") as fh:
            head_val = fh.read().strip()
    except OSError as exc:
        sys.exit(f"verify_scratch_packages_exist: unreadable HEAD in {pkg}: {exc}")
    if head_val != rev:
        sys.exit(f"verify_scratch_packages_exist: {pkg} HEAD is {head_val!r}, expected {rev!r}")
PY
  return 0
}

prepare_scratch_packages() {
  local seed_dir="$1"
  local workspace_dir="$2"
  [ -d "$workspace_dir" ] || {
    echo "prepare_scratch_packages: missing workspace $workspace_dir" >&2
    return 1
  }
  (cd "$workspace_dir" && LAKE_SEED_PACKAGES_DIR="$seed_dir" bash "$ROOT_DIR/scripts/checks/lib/fetch_lake_packages.sh") || return 1
  verify_scratch_packages_exist "$workspace_dir" || return 1
  return 0
}

# Prepare verified pinned packages and run `$LAKE build` only after verification passes.
# Returns 10 if package verification fails (before invoking Lake) or 11 if Lake fails.
build_verified_scratch() {
  local seed_dir="$1"
  local workspace_dir="$2"
  local pkg_log="$3"
  local build_log="$4"
  shift 4
  prepare_scratch_packages "$seed_dir" "$workspace_dir" > "$pkg_log" 2>&1 || return 10
  (cd "$workspace_dir" && "$LAKE" build "$@") > "$build_log" 2>&1 || return 11
  return 0
}

EX="$TMP/ex"; mkdir -p "$EX" || { no "could not create examples workspace"; finish; }
cp lakefile.toml lean-toolchain lake-manifest.json "$EX"/ || { no "could not copy the Lake files"; finish; }
sed -i 's/^warningAsError = true$/warningAsError = false/' "$EX/lakefile.toml" \
  || { no "could not update scratch lakefile.toml"; finish; }
cp "$ENUMERATOR" "$TMP/examples-enumerator.lean" || { no "could not copy examples enumerator"; finish; }
NEX=$(python3 "$PROOFS_DIR/lean_lex.py" examples . "$EX" "${SRC[@]}" "${EVIDENCE_SRC[@]}" "${COMPILER_SRC[@]}") \
  || { no "the examples could not be rewritten: $NEX"; finish; }
EX_BUILD_RC=0
build_verified_scratch "$ROOT_DIR/.lake/packages" "$EX" "$TMP/ex-packages.log" "$TMP/ex-build.log" ConcreteSpec ConcreteSpecEvidence Concrete || EX_BUILD_RC=$?
if [ "$EX_BUILD_RC" -eq 10 ]; then
  no "could not prepare verified pinned packages for examples workspace: $(tail -5 "$TMP/ex-packages.log" | tr '\n' ' ')"
  finish
elif [ "$EX_BUILD_RC" -ne 0 ]; then
  no "the library with its examples as declarations does not build: $(grep -m3 error "$TMP/ex-build.log" | tr '\n' ' ')"
  finish
fi
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
# Scratch-workspace verification and isolation controls (ISS-0130).
# Isolate git config once after production checks finish; never rely on `if ( set -e; ... )`
# subshells (which suppress `set -e` in bash) — every step and required observation checks
# its own exit status explicitly with `|| return 1`.
source "$ROOT_DIR/scripts/checks/lib/git_sandbox.sh" || { no "could not load git_sandbox.sh"; finish; }
git_sandbox "$TMP/scratch-ctl-gitconfig" || { no "git_sandbox failed for scratch controls"; finish; }
cat >"$GIT_CONFIG_GLOBAL" <<'EOF' || { no "could not write scratch gitconfig"; finish; }
[user]
	name = trust-ban scratch control
	email = control@example.invalid
[init]
	defaultBranch = main
[advice]
	detachedHead = false
EOF

# Finding 1 controls: failed scratch-package verification must propagate non-zero and
# stop build_verified_scratch BEFORE Lake is ever invoked.
SENTINEL_LAKE="$TMP/sentinel-lake"
SENTINEL_MARKER="$TMP/sentinel-lake-ran"
cat >"$SENTINEL_LAKE" <<EOF
#!/usr/bin/env bash
printf 'lake-invoked\n' >"$SENTINEL_MARKER"
exit 0
EOF
chmod +x "$SENTINEL_LAKE"

EMPTY_WS="$TMP/empty-scratch-ws"
mkdir -p "$EMPTY_WS"
rm -f "$SENTINEL_MARKER"
rc=0
(
  LAKE="$SENTINEL_LAKE"
  build_verified_scratch "$TMP/nonexistent-seed" "$EMPTY_WS" \
    "$TMP/empty-pkg.log" "$TMP/empty-build.log" ConcreteSpec
) || rc=$?
if [ "$rc" -eq 10 ] && [ ! -e "$SENTINEL_MARKER" ] && has_match -F "lean-toolchain" <"$TMP/empty-pkg.log"; then
  ok "control: failed scratch-package verification on empty workspace stops before Lake build"
else
  cat "$TMP/empty-pkg.log" >&2
  no "control: empty scratch workspace did not stop before build (rc=$rc, lake_ran=$([ -e "$SENTINEL_MARKER" ] && echo yes || echo no))"
fi

UNREACHABLE_WS="$TMP/unreachable-scratch-ws"
mkdir -p "$UNREACHABLE_WS"
cp "$ROOT_DIR/lean-toolchain" "$UNREACHABLE_WS/"
cat >"$UNREACHABLE_WS/lake-manifest.json" <<EOF
{"version": "1.3.0",
 "packagesDir": ".lake/packages",
 "packages":
 [{"url": "$TMP/no-such-remote.git",
   "type": "git",
   "subDir": null,
   "scope": "",
   "rev": "0123456789abcdef0123456789abcdef01234567",
   "name": "missingDep",
   "manifestFile": "lake-manifest.json",
   "inputRev": "0123456789abcdef0123456789abcdef01234567",
   "inherited": false,
   "configFile": "lakefile.toml"}],
 "name": "unreachableCtl",
 "lakeDir": ".lake",
 "fixedToolchain": false}
EOF
rm -f "$SENTINEL_MARKER"
rc=0
(
  LAKE="$SENTINEL_LAKE"
  LAKE_FETCH_ATTEMPTS=1 LAKE_FETCH_BACKOFF_BASE=0 \
    build_verified_scratch "$TMP/nonexistent-seed" "$UNREACHABLE_WS" \
      "$TMP/unreachable-pkg.log" "$TMP/unreachable-build.log" ConcreteSpec
) || rc=$?
if [ "$rc" -eq 10 ] && [ ! -e "$SENTINEL_MARKER" ] && has_match -F "failed to fetch" <"$TMP/unreachable-pkg.log"; then
  ok "control: failed scratch-package fetch with missing seed stops before Lake build"
else
  cat "$TMP/unreachable-pkg.log" >&2
  no "control: unreachable scratch fetch did not stop before build (rc=$rc, lake_ran=$([ -e "$SENTINEL_MARKER" ] && echo yes || echo no))"
fi

EMPTY_PKGS_WS="$TMP/missing-pkg-ws"
mkdir -p "$EMPTY_PKGS_WS/.lake/packages"
cp "$ROOT_DIR/lean-toolchain" "$ROOT_DIR/lake-manifest.json" "$EMPTY_PKGS_WS/"
rc=0
verify_scratch_packages_exist "$EMPTY_PKGS_WS" >"$TMP/missing-pkg.log" 2>&1 || rc=$?
if [ "$rc" -ne 0 ] && has_match -F "missing package" <"$TMP/missing-pkg.log"; then
  ok "control: final scratch-package assertion refuses an empty .lake/packages directory"
else
  cat "$TMP/missing-pkg.log" >&2
  no "control: final scratch-package assertion accepted an empty .lake/packages directory (rc=$rc)"
fi

# Run the end-to-end Lean isolation control in DIR, building CALLER_TARGET in the caller
# workspace and requiring EXPECTED_DIAG in the failing scratch build log. Optional 4th arg
# PLANT_INHERITED_LAKE=1 simulates leaked dependency .lake outputs to prove the check fails.
# Every required observation (caller build, caller output creation, offline package preparation,
# absence of inherited .lake outputs, rejection of localAddedLemma, expected diagnostic, and
# clean scratch build) is independently mandatory via explicit `|| return 1`.
run_scratch_isolation_control() {
  local d="$1"
  local caller_target="$2"
  local expected_diag="$3"
  local plant_inherited_lake="${4:-0}"
  local ws=""
  local dep_rev=""

  mkdir -p "$d/dep-src" "$d/caller" "$d/scratch" || return 1
  cp "$ROOT_DIR/lean-toolchain" "$d/dep-src/" || return 1
  printf 'name = "scratchDep"\n[[lean_lib]]\nname = "ScratchDep"\n' >"$d/dep-src/lakefile.toml" || return 1
  printf 'def pinnedVal : Nat := 1\n' >"$d/dep-src/ScratchDep.lean" || return 1
  git init -q "$d/dep-src" || return 1
  git -C "$d/dep-src" add lakefile.toml ScratchDep.lean lean-toolchain || return 1
  git -C "$d/dep-src" commit -qm "pinned" || return 1
  dep_rev="$(git -C "$d/dep-src" rev-parse HEAD)" || return 1
  [ -n "$dep_rev" ] || return 1
  git clone -q --bare "$d/dep-src" "$d/dep.git" || return 1

  for ws in "$d/caller" "$d/scratch"; do
    cp "$ROOT_DIR/lean-toolchain" "$ws/" || return 1
    cat >"$ws/lakefile.toml" <<EOF || return 1
name = "scratchCtl"
[[require]]
name = "scratchDep"
git = "$d/dep.git"
rev = "$dep_rev"
[[lean_lib]]
name = "ScratchMain"
EOF
    cat >"$ws/lake-manifest.json" <<EOF || return 1
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
    printf 'import ScratchDep\nexample : localAddedLemma = 42 := rfl\n' >"$ws/ScratchMain.lean" || return 1
  done

  (cd "$d/caller" && LAKE_FETCH_BACKOFF_BASE=0 bash "$ROOT_DIR/scripts/checks/lib/fetch_lake_packages.sh") || {
    echo "caller fetch_lake_packages.sh failed" >&2
    return 1
  }
  printf 'def localAddedLemma : Nat := 42\n' >>"$d/caller/.lake/packages/scratchDep/ScratchDep.lean" || return 1

  # Mandatory observation 1: caller builds with the locally added backend lemma and writes .lake outputs.
  (cd "$d/caller" && "$LAKE" build "$caller_target") || {
    echo "caller build of $caller_target failed" >&2
    return 1
  }
  [ -d "$d/caller/.lake/packages/scratchDep/.lake" ] || {
    echo "caller build did not produce scratchDep/.lake outputs" >&2
    return 1
  }

  # Take the remote offline to prove seeding uses only verified pinned objects with zero network access.
  mv "$d/dep.git" "$d/dep-offline.git" || return 1

  # Mandatory observation 2: prepare_scratch_packages succeeds offline from verified seed objects.
  prepare_scratch_packages "$d/caller/.lake/packages" "$d/scratch" || {
    echo "prepare_scratch_packages failed for scratch workspace" >&2
    return 1
  }

  if [ "$plant_inherited_lake" = "1" ]; then
    mkdir -p "$d/scratch/.lake/packages/scratchDep/.lake/build" || return 1
  fi

  # Mandatory observation 3: no inherited .lake build outputs exist in scratch dependency.
  [ ! -e "$d/scratch/.lake/packages/scratchDep/.lake" ] || {
    echo "scratch workspace inherited caller .lake build outputs" >&2
    return 1
  }

  # Mandatory observation 4: scratch build rejects the locally added backend lemma.
  if (cd "$d/scratch" && "$LAKE" build ScratchMain) >"$d/scratch-bad.log" 2>&1; then
    echo "scratch build unexpectedly accepted locally added backend lemma" >&2
    return 1
  fi

  # Mandatory observation 5: scratch build log contains the required diagnostic.
  has_match -F "$expected_diag" <"$d/scratch-bad.log" || {
    echo "scratch build log missing expected diagnostic '$expected_diag'" >&2
    return 1
  }

  # Mandatory observation 6: clean example using only pinnedVal builds cleanly in scratch.
  printf 'import ScratchDep\nexample : pinnedVal = 1 := rfl\n' >"$d/scratch/ScratchMain.lean" || return 1
  (cd "$d/scratch" && "$LAKE" build ScratchMain) || {
    echo "clean scratch build failed" >&2
    return 1
  }
  return 0
}

rc=0
run_scratch_isolation_control "$TMP/scratch-isolation" "ScratchMain" "localAddedLemma" \
  >"$TMP/scratch-isolation.log" 2>&1 || rc=$?
if [ "$rc" -eq 0 ]; then
  ok "control: examples scratch workspace rejects an example using a lemma added locally to the caller's backend"
else
  tail -20 "$TMP/scratch-isolation.log" >&2
  no "control: examples scratch workspace isolation control failed (rc=$rc)"
fi

# Finding 2 negative controls: planting a failing caller build, leaked dependency .lake
# outputs, or an absent required diagnostic MUST each independently fail the control
# instead of being masked by the final clean build.
rc=0
run_scratch_isolation_control "$TMP/scratch-isolation-bad-target" "NonexistentTarget" "localAddedLemma" \
  >"$TMP/scratch-isolation-bad-target.log" 2>&1 || rc=$?
if [ "$rc" -ne 0 ] && has_match -F "caller build of NonexistentTarget failed" <"$TMP/scratch-isolation-bad-target.log"; then
  ok "control: isolation control fails when caller build targets a nonexistent library"
else
  tail -20 "$TMP/scratch-isolation-bad-target.log" >&2
  no "control: isolation control masked a failing caller build (rc=$rc)"
fi

rc=0
run_scratch_isolation_control "$TMP/scratch-isolation-leaked-lake" "ScratchMain" "localAddedLemma" 1 \
  >"$TMP/scratch-isolation-leaked-lake.log" 2>&1 || rc=$?
if [ "$rc" -ne 0 ] && has_match -F "scratch workspace inherited caller .lake build outputs" <"$TMP/scratch-isolation-leaked-lake.log"; then
  ok "control: isolation control fails when inherited .lake outputs are planted in scratch dependency"
else
  tail -20 "$TMP/scratch-isolation-leaked-lake.log" >&2
  no "control: isolation control masked planted inherited .lake outputs (rc=$rc)"
fi

rc=0
run_scratch_isolation_control "$TMP/scratch-isolation-bad-diag" "ScratchMain" "DiagnosticThatCannotAppear" \
  >"$TMP/scratch-isolation-bad-diag.log" 2>&1 || rc=$?
if [ "$rc" -ne 0 ] && has_match -F "scratch build log missing expected diagnostic 'DiagnosticThatCannotAppear'" <"$TMP/scratch-isolation-bad-diag.log"; then
  ok "control: isolation control fails when required scratch build diagnostic is absent"
else
  tail -20 "$TMP/scratch-isolation-bad-diag.log" >&2
  no "control: isolation control masked an absent diagnostic (rc=$rc)"
fi
finish
