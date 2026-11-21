#!/usr/bin/env bash
# AXIOM INVENTORY: every theorem of the specification and compiler depends, transitively, on no axiom beyond
# propext, Quot.sound and Classical.choice (Q9.17), and the per-theorem inventory equals the golden
# lists scripts/proofs/axioms.golden and scripts/proofs/compiler_axioms.golden.
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
  source_modules "$pkg" > "$TMP/source-modules.txt" || { no "$pkg source inventory failed"; return; }
  gap=$(comm -23 "$TMP/source-modules.txt" <(sort -u "$TMP/imported.txt"))
  [ -z "$gap" ] && ok "all $pkg source modules are in the root's import closure" \
    || no "$pkg modules outside the import closure: $gap"

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

GOLDEN=scripts/proofs/axioms.golden
COMPILER_GOLDEN=scripts/proofs/compiler_axioms.golden
audit_library ConcreteSpec "$GOLDEN" "${1:-}"
audit_library Concrete "$COMPILER_GOLDEN" "${1:-}"
if [ "${1:-}" = --update ] || [ "$FAIL" != 0 ]; then finish; fi

export PROOF_PKG=ConcreteSpec
echo "=== negative controls ==="
cat > "$TMP/c-axiom.lean" <<'EOF'
axiom ConcreteSpecControl.extra : 1 = 2
theorem ConcreteSpecControl.usesExtra : 2 = 1 := ConcreteSpecControl.extra.symm
EOF
population theorems "$TMP/c-axiom.lean" > "$TMP/c-axiom.tsv" 2>"$TMP/c-axiom.err"
CB=$(nonstandard "$TMP/c-axiom.tsv")
[ "$CB" = "ConcreteSpecControl.usesExtra (ConcreteSpecControl.extra)" ] \
  && ok "control: a theorem using a declared axiom is refused, and only it ($CB)" \
  || no "control: the extra-axiom theorem was not refused alone: '$CB' $(head -2 "$TMP/c-axiom.err")"
cat > "$TMP/c-clean.lean" <<'EOF'
theorem ConcreteSpecControl.clean : 1 = 1 := rfl
private theorem ConcreteSpecControl.hidden : 2 = 2 := rfl
EOF
population theorems "$TMP/c-clean.lean" > "$TMP/c-clean.tsv" 2>/dev/null
# A private name carries the file it was declared in (`_private.<file>.0.…`); the temporary copy's
# name is random, so it is normalized before the comparison.
D=$(golden_diff "$TMP/c-clean.tsv" "$GOLDEN" | sed -E 's/^> _private\..*\.0\.ConcreteSpecControl\./> _private.0.ConcreteSpecControl./')
[ "$D" = "$(printf '> ConcreteSpecControl.clean\t\n> _private.0.ConcreteSpecControl.hidden\t')" ] \
  && ok "control: a new theorem, even axiom-free and even private, is a golden difference" \
  || no "control: new public and private theorems gave the diff '$D'"
FIRST=$(head -1 "$TMP/ConcreteSpec.tsv" | cut -f1)
awk -F'\t' -v first="$FIRST" '$1 != first' "$GOLDEN" > "$TMP/g-missing"
[ -n "$(golden_diff "$TMP/ConcreteSpec.tsv" "$TMP/g-missing")" ] \
  && ok "control: a golden missing a theorem ($FIRST) differs" || no "control: a golden missing $FIRST compared equal"
awk -F'\t' 'BEGIN{OFS="\t"} !/^#/ && !done && $2 == "" { $2 = "propext"; done = 1 } { print }' "$GOLDEN" > "$TMP/g-axiom"
! cmp -s "$GOLDEN" "$TMP/g-axiom" && [ -n "$(golden_diff "$TMP/ConcreteSpec.tsv" "$TMP/g-axiom")" ] \
  && ok "control: a golden recording other axioms for a theorem differs" || no "control: a changed axiom set compared equal"
printf '%s\n' ConcreteSpec ConcreteSpec.Spec > "$TMP/imp-short.txt"
[ -n "$(closure_gap "$TMP/imp-short.txt")" ] \
  && ok "control: source modules outside a (truncated) import closure are reported" || no "control: a truncated closure reported no gap"
cat > "$TMP/c-compiler.lean" <<'EOF'
axiom ConcreteControl.extra : 1 = 2
theorem ConcreteControl.usesExtra : 2 = 1 := ConcreteControl.extra.symm
EOF
if PROOF_PKG=Concrete population theorems "$TMP/c-compiler.lean" > "$TMP/c-compiler.tsv"; then
  if CB=$(standard_only "$TMP/c-compiler.tsv"); then
    no "control: a compiler theorem using a declared axiom passed"
  elif [ "$CB" = "ConcreteControl.usesExtra (ConcreteControl.extra)" ]; then
    ok "control: the compiler theorem using a declared axiom is refused ($CB)"
  else
    no "control: the compiler axiom check returned '$CB'"
  fi
else
  no "control: compiler theorem enumeration failed"
fi
cat > "$TMP/c-compiler-private.lean" <<'EOF'
private theorem ConcreteControl.hidden : 2 = 2 := rfl
EOF
if PROOF_PKG=Concrete population theorems "$TMP/c-compiler-private.lean" > "$TMP/c-compiler-private.tsv" &&
    standard_only "$TMP/c-compiler-private.tsv"; then
  D=$(golden_diff "$TMP/c-compiler-private.tsv" "$COMPILER_GOLDEN" |
    sed -E 's/^> _private\..*\.0\.ConcreteControl\./> _private.0.ConcreteControl./')
  [ "$D" = "$(printf '> _private.0.ConcreteControl.hidden\t')" ] \
    && ok "control: an axiom-free private compiler theorem changes the golden" \
    || no "control: the private compiler theorem gave the diff '$D'"
else
  no "control: axiom-free private compiler theorem enumeration or axiom check failed"
fi
finish
