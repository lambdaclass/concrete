#!/usr/bin/env bash
# AXIOM PROPERTIES: every theorem of the specification depends, transitively, on no axiom beyond
# propext, Quot.sound and Classical.choice (Q9.17), and the public design and safety theorems pinned
# in scripts/proofs/pinned_theorems.txt remain in the theorem population.
#
#   P  population: every module the ConcreteSpec sources declare is in the root's import closure (a
#      module outside it is neither built by default nor read here), and every module whose file
#      declares a theorem contributes one, so the population cannot shrink unnoticed;
#   A  every theorem's transitive axioms (Lean.collectAxioms) are among the standard three;
#   T  every name in pinned_theorems.txt is a public theorem in the population. Adding other
#      theorems, private or generated ones included, changes nothing; a pinned name is refused when
#      it has a private or generated shape.
#
# NEGATIVE CONTROLS run the same code on bad inputs and require the exact failure: a theorem proved
# by sorry and one using a declared axiom fail A naming it; a missing pinned theorem fails T, whether
# the pin or the population entry was removed; a generated pin is refused; new public, private and
# generated theorems pass T; a control module outside the import closure fails P.
set -uo pipefail
if [ "${1:-}" = --update ]; then
  echo "error: the axiom golden is gone; --update no longer exists (edit scripts/proofs/pinned_theorems.txt)" >&2
  exit 2
fi
source "$(dirname "${BASH_SOURCE[0]}")/../checks/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
CHECK_NAME=PROOF-AXIOMS
source scripts/proofs/lib.sh
PINNED=scripts/proofs/pinned_theorems.txt

population theorems > "$TMP/inv.tsv" 2> "$TMP/inv.err" \
  || { cat "$TMP/inv.err" >&2; echo "error: the enumerator failed" >&2; exit 2; }

# nonstandard FILE → the theorems of an inventory with an axiom outside the standard three
nonstandard() {
  awk -F'\t' -v std=" $STANDARD_AXIOMS " '{ n = split($2, a, " ")
    for (i = 1; i <= n; i++) if (index(std, " " a[i] " ") == 0) { print $1 " (" a[i] ")"; break } }' "$1"
}
# pinned_names FILE → one pinned name per non-comment, non-empty line
pinned_names() { awk '/^[[:space:]]*($|#)/ { next } { print $1 }' "$1"; }
# Lean's generated theorem helpers and private constants are not stable public specification names.
generated_or_private() {
  [[ $1 =~ (^|[.])(_private|match_[0-9]+|eq_[0-9]+|eq_def|proof_[0-9]+|_proof_[0-9]+(_[0-9]+)*|_sparseCasesOn(_[0-9]+)?|brecOn(_[0-9]+)?|casesOn|inj|injEq|sizeOf_spec|ofNat_ctorIdx|_sizeOf_[0-9]+_eq|_simp_[0-9]+(_[0-9]+)*|else_eq|congr_simp)([.]|$) ]]
}
# pinned_errors INVENTORY PINNED → every malformed or absent pin; empty when all are public theorems
pinned_errors() {
  local inventory=$1 pinned=$2 name
  [ -f "$pinned" ] || { echo "$pinned is missing"; return; }
  while IFS= read -r name; do
    if generated_or_private "$name"; then
      echo "refusing generated or private pinned name: $name"
    elif [[ $name != ConcreteSpec.* ]]; then
      echo "pinned name is not fully qualified under ConcreteSpec: $name"
    elif ! awk -F'\t' -v name="$name" '$1 == name { found = 1; exit } END { exit !found }' "$inventory"; then
      echo "pinned theorem missing or not a theorem: $name"
    fi
  done < <(pinned_names "$pinned")
}

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

echo "=== T: pinned public theorems ==="
T=$(pinned_errors "$TMP/inv.tsv" "$PINNED")
[ -z "$T" ] && ok "all $(pinned_names "$PINNED" | grep -c .) names in $PINNED are public theorems in the population" \
  || no "pinned theorem check failed: $(echo $T | cut -c1-400)"

echo "=== negative controls ==="
cat > "$TMP/c-sorry.lean" <<'EOF'
theorem ConcreteSpecControl.sorryThm : 1 = 2 := sorry
EOF
population theorems "$TMP/c-sorry.lean" > "$TMP/c-sorry.tsv" 2> "$TMP/c-sorry.err"
SB=$(nonstandard "$TMP/c-sorry.tsv")
[ "$SB" = "ConcreteSpecControl.sorryThm (sorryAx)" ] \
  && ok "control: a theorem proved by sorry fails A naming it ($SB)" \
  || no "control: the sorry theorem was not refused alone by A: '$SB' $(head -2 "$TMP/c-sorry.err")"
cat > "$TMP/c-axiom.lean" <<'EOF'
axiom ConcreteSpecControl.extra : 1 = 2
theorem ConcreteSpecControl.usesExtra : 2 = 1 := ConcreteSpecControl.extra.symm
EOF
population theorems "$TMP/c-axiom.lean" > "$TMP/c-axiom.tsv" 2>"$TMP/c-axiom.err"
CB=$(nonstandard "$TMP/c-axiom.tsv")
[ "$CB" = "ConcreteSpecControl.usesExtra (ConcreteSpecControl.extra)" ] \
  && ok "control: a theorem using a declared axiom fails A naming it, and only it ($CB)" \
  || no "control: the extra-axiom theorem was not refused alone by A: '$CB' $(head -2 "$TMP/c-axiom.err")"
printf 'ConcreteSpec.Control.absent # negative control\n' > "$TMP/p-absent.txt"
P=$(pinned_errors "$TMP/inv.tsv" "$TMP/p-absent.txt")
[ "$P" = "pinned theorem missing or not a theorem: ConcreteSpec.Control.absent" ] \
  && ok "control: a pin absent from the population fails T ($P)" \
  || no "control: an absent pin gave '$P'"
FIRST=$(pinned_names "$PINNED" | head -1)
awk -F'\t' -v name="$FIRST" '$1 != name' "$TMP/inv.tsv" > "$TMP/inv-filtered.tsv"
P=$(pinned_errors "$TMP/inv-filtered.tsv" "$PINNED")
[ "$P" = "pinned theorem missing or not a theorem: $FIRST" ] \
  && ok "control: removing pinned theorem $FIRST from the population fails T" \
  || no "control: removing $FIRST from the population gave '$P'"
printf 'ConcreteSpec.Control.match_1 # negative control\n' > "$TMP/p-generated.txt"
P=$(pinned_errors "$TMP/inv.tsv" "$TMP/p-generated.txt")
[ "$P" = "refusing generated or private pinned name: ConcreteSpec.Control.match_1" ] \
  && ok "control: a generated-shape pin is refused ($P)" \
  || no "control: a generated-shape pin gave '$P'"
cat > "$TMP/c-clean.lean" <<'EOF'
theorem ConcreteSpecControl.clean : 1 = 1 := rfl
private theorem ConcreteSpecControl.hidden : 2 = 2 := rfl
inductive ConcreteSpecControl.Shape where
  | leaf
  | node (n : Nat)
def ConcreteSpecControl.shapeSize : ConcreteSpecControl.Shape → Nat
  | .leaf => 0
  | .node n => n + 1
EOF
population theorems "$TMP/c-clean.lean" > "$TMP/c-clean.tsv" 2> "$TMP/c-clean.err"
P=$(pinned_errors "$TMP/c-clean.tsv" "$PINNED")
[ -z "$P" ] \
  && grep -q $'^ConcreteSpecControl.clean\t' "$TMP/c-clean.tsv" \
  && grep -qE '^_private\..*ConcreteSpecControl\.hidden[[:space:]]' "$TMP/c-clean.tsv" \
  && grep -qE '(^|[.])match_[0-9]+([.]|$)|(^|[.])eq_[0-9]+([.]|$)' "$TMP/c-clean.tsv" \
  && ok "control: added public, private and generated theorems pass T without a golden-style difference" \
  || no "control: added public, private and generated theorems did not all pass T: '$P' $(head -2 "$TMP/c-clean.err")"
printf '%s\n' ConcreteSpec ConcreteSpec.Spec > "$TMP/imp-short.txt"
[ -n "$(closure_gap "$TMP/imp-short.txt")" ] \
  && ok "control: source modules outside a (truncated) import closure are reported" || no "control: a truncated closure reported no gap"
finish
