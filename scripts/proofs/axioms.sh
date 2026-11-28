#!/usr/bin/env bash
# AXIOM INVENTORY: every theorem of the specification, evidence, and compiler depends transitively
# on only propext, Quot.sound and Classical.choice (Q9.17). Each library has its own golden.
#
#   P  population: every source module in each library is in the root's import closure (a
#      module outside it is neither built by default nor read here), and every module whose file
#      declares a theorem contributes one, so the population cannot shrink unnoticed;
#   A  every theorem's transitive axioms (Lean.collectAxioms) are among the standard three;
#   G  the inventory (`<theorem>\t<axioms>`) equals its golden byte for byte. A theorem added,
#      removed or renamed, or one that starts or stops using an axiom, changes the golden in the same
#      commit, where review sees it. `--update` rewrites it from the build.
#
# Status: design. README.md records the owner-decided target: check standard axioms over every
# ConcreteSpec constant, including generated helpers; pin only source-named theorems in the golden.
# Classification and exact generating declarations must come from compiler-produced provenance
# persisted with declarations in oleans, never names or manual review. Compiler provenance is
# decided; toolchain implementation and task allocation remain prerequisites. Until validated
# persistent provenance and its consumer exist, the full-theorem G above is an interim safeguard.
# No baseline-SHA removal exception is the end state.
#
# NEGATIVE CONTROLS run the same code on bad inputs and require the failure: an axiom declared and
# used by a control theorem fails A naming it; new theorems, a private one included, differ from the
# golden; a golden missing a theorem, carrying an extra one, or
# recording other axioms fails G; a control module outside the import closure fails P.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/../checks/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
CHECK_NAME=PROOF-AXIOMS
source scripts/proofs/lib.sh
# nonstandard FILE → the theorems of an inventory with an axiom outside the standard three
nonstandard() {
  awk -F'\t' -v std=" $STANDARD_AXIOMS " '{ n = split($2, a, " ")
    for (i = 1; i <= n; i++) if (index(std, " " a[i] " ") == 0) { print $1 " (" a[i] ")"; break } }' "$1"
}
standard_only() {
  local bad
  bad=$(nonstandard "$1") || return 2
  if [ -n "$bad" ]; then
    printf '%s\n' "$bad"
    return 1
  fi
}
# golden_diff INVENTORY GOLDEN → the differing lines, empty when equal
golden_diff() { diff <(grep -v '^#' "$2") "$1" | grep '^[<>]'; }

audit_library() {
  local pkg=$1
  local golden=$2
  local inventory="$TMP/$pkg.tsv"
  local gap unseen count delta bad
  export PROOF_PKG="$pkg"

  echo "=== $pkg: theorem population and axioms ==="
  population theorems > "$inventory" || { cat "$inventory" >&2; no "$pkg theorem enumeration failed"; return; }
  population imported > "$TMP/imported.txt" || { no "$pkg import enumeration failed"; return; }
  gap=$(closure_gap "$TMP/imported.txt" "$pkg") || { no "$pkg root module comparison failed"; return; }
  [ -z "$gap" ] && ok "all $pkg source modules are in the root's import closure" \
    || no "$pkg modules outside the import closure: $gap"

  population environment > "$TMP/environment.txt" || { no "$pkg environment module enumeration failed"; return; }
  gap=$(closure_gap "$TMP/environment.txt" "$pkg") || { no "$pkg environment module comparison failed"; return; }
  [ -z "$gap" ] || { no "$pkg source modules outside the theorem environment: $gap"; return; }

  population modules > "$TMP/env-modules.txt" || { no "$pkg theorem module enumeration failed"; return; }
  grep -rlE '^\s*(@\[[^]]*\]\s*)?(private |protected )*theorem ' "$pkg" "$pkg.lean" --include='*.lean' \
    | sed -E 's#\.lean$##; s#/#.#g' | sort -u > "$TMP/src-theorem-modules.txt"
  unseen=$(comm -23 "$TMP/src-theorem-modules.txt" <(sort -u "$TMP/env-modules.txt"))
  count=$(grep -c . "$inventory")
  [ -s "$TMP/src-theorem-modules.txt" ] && [ -z "$unseen" ] && [ "$count" -gt 0 ] \
    && ok "$count $pkg theorems from all source modules that declare one" \
    || no "$pkg population: $count theorems; modules declaring theorems but absent: $unseen"

  if ! bad=$(standard_only "$inventory"); then
    no "$pkg theorems using a non-standard axiom: $bad"
    return
  fi
  ok "every $pkg theorem's transitive axioms are among $STANDARD_AXIOMS"

  if [ "${3:-}" = --update ]; then
    {
      echo "# Per-theorem transitive axioms of $pkg (scripts/proofs/axioms.sh). Regenerate with"
      echo "# bash scripts/proofs/axioms.sh --update; review every changed line."
      cat "$inventory"
    } > "$golden" || { no "could not write $golden"; return; }
    ok "wrote $golden ($count theorems)"
    return
  fi
  [ -r "$golden" ] || { no "$golden is missing or unreadable (bash scripts/proofs/axioms.sh --update)"; return; }
  sed '/^#/d' "$golden" > "$TMP/golden.tsv" || { no "could not read $golden"; return; }
  if cmp -s "$inventory" "$TMP/golden.tsv"; then
    ok "the inventory equals $golden ($count theorems)"
  else
    delta=$(golden_diff "$inventory" "$golden")
    no "the inventory differs from $golden (< golden, > build; regenerate with --update and review): $(printf '%s\n' "$delta" | head -10 | tr '\t\n' ' |')"
  fi
}

# check_no_classical_choice LOG_FILE EXPECTED_NAMES...
# Parses `#print axioms` output and rejects any missing/extra target or any axiom outside
# `{propext, Quot.sound}` (specifically banning `Classical.choice`, `sorryAx`, and native axioms).
check_no_classical_choice() {
  local log_file=$1
  shift
  python3 -I - "$log_file" "$@" <<'PY'
import re, sys
text = open(sys.argv[1], encoding="utf-8").read()
expected = sys.argv[2:]
allowed = {"propext", "Quot.sound"}
seen = []
bad = []
for line in text.splitlines():
    s = line.strip()
    m1 = re.match(r"^'([^']+)' does not depend on any axioms$", s)
    if m1:
        seen.append(m1.group(1))
        continue
    m2 = re.match(r"^'([^']+)' depends on axioms: \[([^\]]*)\]$", s)
    if m2:
        name = m2.group(1)
        seen.append(name)
        axs = [a.strip() for a in m2.group(2).split(",") if a.strip()]
        extra = [a for a in axs if a not in allowed]
        if extra:
            bad.append(f"{name}: {extra}")
if seen != expected:
    print(f"expected #print axioms for {expected}, saw {seen}")
    sys.exit(1)
if bad:
    print("forbidden axioms: " + "; ".join(bad))
    sys.exit(1)
PY
}

slice_parser_constructive_check() {
  local probe="$TMP/slice-parser-axioms.lean"
  local probe_out="$TMP/slice-parser-axioms.out"
  local probe_err="$TMP/slice-parser-axioms.err"
  local neg="$TMP/slice-parser-axioms-neg.lean"
  local neg_out="$TMP/slice-parser-axioms-neg.out"
  local neg_err="$TMP/slice-parser-axioms-neg.err"
  local msg
  local targets=(
    ConcreteSpec.StageGrammar.sliceParser
    ConcreteSpec.StageGrammar.sliceVerifierInputs
    ConcreteSpec.StageGrammar.earlySharedGrammar_productive
    ConcreteSpec.StageGrammar.sliceParser_rank_le
    ConcreteSpec.StageGrammar.sliceParser_stepsPerToken_ge
    ConcreteSpec.StageGrammar.sliceParser_nonExhausted
    ConcreteSpec.StageGrammar.sliceParser_steps_le
    ConcreteSpec.StageGrammar.sliceParser_ok_derives_iff
    ConcreteSpec.StageGrammar.sliceParser_ok_iff
    ConcreteSpec.StageGrammar.sliceParser_unsupported_iff
    ConcreteSpec.StageGrammar.sliceParser_malformed_iff
    ConcreteSpec.StageGrammar.sliceParser_unsupported_ne_program
  )
  echo "=== ConcreteSpec: constructive sliceParser boundary (no Classical.choice) ==="
  {
    echo "import ConcreteSpec.StageGrammar"
    echo "import ConcreteSpec.Proofs.SliceGrammar"
    local name
    for name in "${targets[@]}"; do
      echo "#print axioms $name"
    done
  } > "$probe"
  if run_enumerator "$probe" > "$probe_out" 2> "$probe_err"; then
    if msg=$(check_no_classical_choice "$probe_out" "${targets[@]}"); then
      ok "sliceParser, sliceVerifierInputs, earlySharedGrammar_productive, and all 9 sliceParser_* lemmas use no Classical.choice"
    else
      no "constructive sliceParser axiom check failed: $msg"
    fi
  else
    no "constructive sliceParser axiom probe failed to elaborate: $(head -5 "$probe_out" "$probe_err" | tr '\n' ' ')"
  fi

  cat > "$neg" <<'EOF'
theorem sliceParserClassicalControl (P : Prop) : P ∨ ¬ P := Classical.em P
#print axioms sliceParserClassicalControl
EOF
  if run_enumerator "$neg" > "$neg_out" 2> "$neg_err"; then
    if msg=$(check_no_classical_choice "$neg_out" sliceParserClassicalControl); then
      no "control: constructive sliceParser axiom checker unexpectedly accepted Classical.choice"
    elif printf '%s\n' "$msg" | grep -Fq "Classical.choice"; then
      ok "control: constructive sliceParser axiom checker rejects Classical.choice ($msg)"
    else
      no "control: constructive sliceParser axiom checker failed with unexpected message: $msg"
    fi
  else
    no "control: classical negative control for sliceParser failed to elaborate: $(head -5 "$neg_out" "$neg_err" | tr '\n' ' ')"
  fi
}

GOLDEN=scripts/proofs/axioms.golden
EVIDENCE_GOLDEN=scripts/proofs/evidence_axioms.golden
COMPILER_GOLDEN=scripts/proofs/compiler_axioms.golden
audit_library ConcreteSpec "$GOLDEN" "${1:-}"
audit_library ConcreteSpecEvidence "$EVIDENCE_GOLDEN" "${1:-}"
audit_library Concrete "$COMPILER_GOLDEN" "${1:-}"
if [ "${1:-}" != --update ]; then
  slice_parser_constructive_check
fi
if [ "${1:-}" = --update ] || [ "$FAIL" != 0 ]; then finish; fi

echo "=== negative controls ==="
# A retired library still resolves through raw Lean when its old output is on
# the search path. Exercise the same audit and exit status used by this gate.
(
  set -e
  TMP="$TMP/renamed-import"
  mkdir "$TMP"
  cp "$ROOT_DIR/lean-toolchain" "$TMP/"
  cd "$TMP"
  echo 'def rowProbe := true' > RetiredRowHelper.lean
  "$LAKE" -d "$ROOT_DIR" env lean -R . -o RetiredRowHelper.olean RetiredRowHelper.lean
  mv RetiredRowHelper.lean RenamedRowHelper.lean
  sed '/^import Lean$/a import RetiredRowHelper' "$ENUMERATOR" > broken-import.lean
  export LEAN_PATH="$TMP"
  cd "$ROOT_DIR"
  ENUMERATOR="$TMP/broken-import.lean"
  PROOF_MODE=modules "$LAKE" env lean "$ENUMERATOR" > "$TMP/raw-import.log"
  PASS=0; FAIL=0
  audit_library ConcreteSpec "$GOLDEN"
  finish
) > "$TMP/broken-import.log" 2>&1; rc=$?
if [ "$rc" = 1 ] && grep -Fq "unknown module prefix 'RetiredRowHelper'" "$TMP/broken-import.log" &&
   grep -Fxq 'PROOF-AXIOMS: PASS=0 FAIL=1' "$TMP/broken-import.log"; then
  ok "control: the axiom audit rejects the renamed enumerator import despite its loadable .olean"
else
  cat "$TMP/broken-import.log" >&2
  no "control: expected the missing RetiredRowHelper import to fail the axiom audit (rc=$rc)"
fi

axiom_controls() {
  local PROOF_PKG=$1
  export PROOF_PKG
  local golden=$2
  local inventory="$TMP/$PROOF_PKG.tsv"
  local namespace="${PROOF_PKG}Control"
  local bad delta first
  cat > "$TMP/c-axiom.lean" <<EOF
axiom $namespace.extra : 1 = 2
theorem $namespace.usesExtra : 2 = 1 := $namespace.extra.symm
EOF
  if ! population theorems "$TMP/c-axiom.lean" > "$TMP/c-axiom.tsv" 2>"$TMP/c-axiom.err"; then
    no "control: $PROOF_PKG axiom enumeration failed: $(head -2 "$TMP/c-axiom.err")"
  elif bad=$(standard_only "$TMP/c-axiom.tsv") || [ "$bad" != "$namespace.usesExtra ($namespace.extra)" ]; then
    no "control: $PROOF_PKG extra-axiom theorem was not refused alone: '$bad'"
  else
    ok "control: $PROOF_PKG refuses only the theorem using the declared axiom"
  fi

  cat > "$TMP/c-clean.lean" <<EOF
theorem $namespace.clean : 1 = 1 := rfl
private theorem $namespace.hidden : 2 = 2 := rfl
EOF
  if population theorems "$TMP/c-clean.lean" > "$TMP/c-clean.tsv" && standard_only "$TMP/c-clean.tsv"; then
    # A private name includes the random filename of the temporary enumerator.
    delta=$(golden_diff "$TMP/c-clean.tsv" "$golden" |
      sed -E "s/^> _private\\..*\\.0\\.$namespace\\./> _private.0.$namespace./")
    [ "$delta" = "$(printf '> %s.clean\t\n> _private.0.%s.hidden\t' "$namespace" "$namespace")" ] \
      && ok "control: new $PROOF_PKG public and private theorems change the golden" \
      || no "control: new $PROOF_PKG public and private theorems gave the diff '$delta'"
  else
    no "control: $PROOF_PKG axiom-free theorem enumeration or axiom check failed"
  fi

  first=$(head -1 "$inventory" | cut -f1)
  awk -F'\t' -v first="$first" '$1 != first' "$golden" > "$TMP/g-missing"
  [ "$(golden_diff "$inventory" "$TMP/g-missing")" = "> $(head -1 "$inventory")" ] \
    && ok "control: $PROOF_PKG golden missing $first differs" \
    || no "control: $PROOF_PKG golden missing $first did not produce the expected difference"
  awk -F'\t' 'BEGIN{OFS="\t"} !/^#/ && !done && $2 == "" { $2 = "propext"; done = 1 } { print }' "$golden" > "$TMP/g-axiom"
  [ -n "$(golden_diff "$inventory" "$TMP/g-axiom")" ] \
    && ok "control: a changed $PROOF_PKG golden axiom set differs" \
    || no "control: a changed $PROOF_PKG golden axiom set compared equal"
}
axiom_controls ConcreteSpec "$GOLDEN"
axiom_controls ConcreteSpecEvidence "$EVIDENCE_GOLDEN"
axiom_controls Concrete "$COMPILER_GOLDEN"
finish
