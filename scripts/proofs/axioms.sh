#!/usr/bin/env bash
# AXIOM INVENTORY: every theorem of the specification depends, transitively, on no axiom beyond
# propext, Quot.sound and Classical.choice (Q9.17), and the per-theorem inventory equals the golden
# list scripts/proofs/axioms.golden.
#
#   P  population: every module the ConcreteSpec sources declare is in the root's import closure (a
#      module outside it is neither built by default nor read here), and every module whose file
#      declares a theorem contributes one, so the population cannot shrink unnoticed;
#   A  every theorem's transitive axioms (Lean.collectAxioms) are among the standard three;
#   G  the inventory (`<theorem>\t<axioms>`) equals axioms.golden byte for byte. A theorem added,
#      removed or renamed, or one that starts or stops using an axiom, changes the golden in the same
#      commit, where review sees it. `--update` rewrites it from the build.
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
GOLDEN=scripts/proofs/axioms.golden

population theorems > "$TMP/inv.tsv" 2> "$TMP/inv.err" \
  || { cat "$TMP/inv.err" >&2; echo "error: the enumerator failed" >&2; exit 2; }
if [ "${1:-}" = --update ]; then
  { echo "# Per-theorem transitive axioms of ConcreteSpec (scripts/proofs/axioms.sh). Regenerate with"
    echo "# bash scripts/proofs/axioms.sh --update; review every changed line."
    cat "$TMP/inv.tsv"; } > "$GOLDEN"
  echo "wrote $GOLDEN ($(grep -c . "$TMP/inv.tsv") theorems)"; exit 0
fi

# nonstandard FILE → the theorems of an inventory with an axiom outside the standard three
nonstandard() {
  awk -F'\t' -v std=" $STANDARD_AXIOMS " '{ n = split($2, a, " ")
    for (i = 1; i <= n; i++) if (index(std, " " a[i] " ") == 0) { print $1 " (" a[i] ")"; break } }' "$1"
}
# golden_diff INVENTORY GOLDEN → the differing lines, empty when equal
golden_diff() { diff <(grep -v '^#' "$2") "$1" | grep '^[<>]'; }
# closure_gap IMPORTED → source modules missing from the import closure
closure_gap() { spec_modules | comm -23 - <(sort -u "$1"); }

echo "=== P: population ==="
population imported > "$TMP/imported.txt" || { no "PROOF_MODE=imported failed"; finish; }
GAP=$(closure_gap "$TMP/imported.txt")
[ -z "$GAP" ] && ok "all $(spec_modules | grep -c .) ConcreteSpec source modules are in the root's import closure" \
  || no "source modules outside the import closure of ConcreteSpec: $(echo $GAP)"
population modules > "$TMP/env-modules.txt" || { no "PROOF_MODE=modules failed"; finish; }
grep -rlE '^\s*(@\[[^]]*\]\s*)?(private |protected )*theorem ' ConcreteSpec ConcreteSpec.lean --include='*.lean' \
  | sed -E 's#\.lean$##; s#/#.#g' | sort -u > "$TMP/src-modules.txt"
UNSEEN=$(comm -13 <(sort -u "$TMP/env-modules.txt") "$TMP/src-modules.txt")
NTHM=$(grep -c . "$TMP/inv.tsv")
[ -s "$TMP/src-modules.txt" ] && [ -z "$UNSEEN" ] && [ "$NTHM" -ge 100 ] \
  && ok "$NTHM theorems from all $(grep -c . "$TMP/src-modules.txt") source modules that declare one" \
  || no "population: $NTHM theorems; modules declaring theorems but absent: $(echo $UNSEEN)"

echo "=== A: standard axioms only ==="
BAD=$(nonstandard "$TMP/inv.tsv")
[ -z "$BAD" ] && ok "every theorem's transitive axioms are among $STANDARD_AXIOMS ($(cut -f2 "$TMP/inv.tsv" | tr ' ' '\n' | grep . | sort | uniq -c | awk '{printf "%s%s %s", s, $2, $1; s=", "}'))" \
  || no "theorems using a non-standard axiom: $(echo $BAD | cut -c1-400)"

echo "=== G: golden list ==="
[ -f "$GOLDEN" ] || { no "$GOLDEN is missing (bash scripts/proofs/axioms.sh --update)"; finish; }
D=$(golden_diff "$TMP/inv.tsv" "$GOLDEN")
[ -z "$D" ] && ok "the inventory equals $GOLDEN ($NTHM theorems)" \
  || no "the inventory differs from $GOLDEN (< golden, > build; regenerate with --update and review): $(printf '%s\n' "$D" | head -10 | tr '\t\n' ' |')"

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
FIRST=$(head -1 "$TMP/inv.tsv" | cut -f1)
grep -vF -- "$FIRST	" "$GOLDEN" > "$TMP/g-missing"
[ -n "$(golden_diff "$TMP/inv.tsv" "$TMP/g-missing")" ] \
  && ok "control: a golden missing a theorem ($FIRST) differs" || no "control: a golden missing $FIRST compared equal"
awk -F'\t' 'BEGIN{OFS="\t"} !/^#/ && !done && $2 == "" { $2 = "propext"; done = 1 } { print }' "$GOLDEN" > "$TMP/g-axiom"
! cmp -s "$GOLDEN" "$TMP/g-axiom" && [ -n "$(golden_diff "$TMP/inv.tsv" "$TMP/g-axiom")" ] \
  && ok "control: a golden recording other axioms for a theorem differs" || no "control: a changed axiom set compared equal"
printf '%s\n' ConcreteSpec ConcreteSpec.Spec > "$TMP/imp-short.txt"
[ -n "$(closure_gap "$TMP/imp-short.txt")" ] \
  && ok "control: source modules outside a (truncated) import closure are reported" || no "control: a truncated closure reported no gap"
finish
