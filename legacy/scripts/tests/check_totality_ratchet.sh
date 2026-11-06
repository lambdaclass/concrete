#!/usr/bin/env bash
# The totality ratchet: the compiler does not grow new non-total declarations (R-0506 totality lint;
# R-0553 the ratchet and the conversions; docs/compiler/VERIFIED_PIPELINE.md principle 8,
# "semantics, checkers, validators and proved passes are total").
#
# A `partial def` is opaque to the kernel: no theorem and no `rfl` can say anything about it, so
# every partial declaration in a stage the verified pipeline proves is a proof that cannot be
# stated yet. This gate holds the count where it is and lets it only fall:
#
#   1. scripts/tools/totality_inventory.lean walks the ELABORATED environment of every Concrete
#      module (grep misses `deriving`-generated partial instances and `where`/`mutual` helpers,
#      and counts `partial` keywords Lean ignores on non-recursive definitions);
#   2. scripts/tools/totality_ratchet.py compares it with docs/compiler/totality_baseline.tsv, and
#      that baseline with the base revision's:
#      a non-total declaration not in the baseline FAILS; a row the base revision's baseline did
#      not have FAILS (the baseline only shrinks); a baseline row that became total FAILS
#      until it is deleted (the baseline only shrinks); a proof-relevant stage's row
#      (docs/compiler/totality_stages.tsv) without a reason code FAILS; a `sorry` in any compiler
#      declaration FAILS; an axiom in a compiler module FAILS unless it is a native-trust axiom
#      minted by a lemma in scripts/tests/axiom_native_owners.txt; a declaration pinned total by
#      docs/compiler/totality_pinned.tsv (the point checks check_transform_register.sh used to
#      hold) FAILS if it is non-total, baseline or not.
#
# When a stage's baseline rows reach zero nothing here changes: every non-total declaration in it
# is then new, so the same comparison is the hard "no non-total declarations here" check.
#
# Controls, each must hold or the gate fails:
#   C1  the classifier on a known source (--control): exactly the expected rows, so a partial,
#       a mutual pair, a derived instance on a nested inductive, unsafe, implemented_by, opaque,
#       an axiom and a sorry are each seen, and a total definition and an inert `partial` are not;
#   C2  a planted NEW partial row in a proof-relevant stage fails the comparison;
#   C3  removing a baseline row's declaration from the inventory (it became total) fails;
#   C4  a planted sorry row, C5 a planted non-native axiom row, C6 a pinned declaration turned
#       partial, each fail;
#   C7  a baseline row the base revision did not have fails (admitting a partial by listing it);
#   C8  two declarations that normalize to one name fail instead of merging.
# And the walk is not vacuous: every module that declares anything contributed declarations.
#
# Requires the compiler modules built (it runs `lake build` on them first, a no-op when current).
# Usage: scripts/tests/check_totality_ratchet.sh
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
TOOL="scripts/tools/totality_inventory.lean"
RATCHET="scripts/tools/totality_ratchet.py"

# The compiler: the Concrete library and the executable roots built on it (lakefile.toml). NOT
# Browser.lean, the concrete-browser-native root: it is outside defaultTargets and does not build at
# origin/main ca49fada (`Unknown identifier Pipeline.desugar`, Browser.lean:43), so it cannot be
# imported. Add it here when it builds again; until then a partial added there goes unseen.
mapfile -t SRCS < <(git ls-files 'Concrete.lean' 'Concrete/*.lean' 'Main.lean')
MODS=(); for f in "${SRCS[@]}"; do m="${f%.lean}"; MODS+=("${m//\//.}"); done
[ "${#MODS[@]}" -gt 0 ] || { echo "FAIL: no compiler modules found"; exit 1; }

echo "=== build every compiler module (the inventory reads their .olean files) ==="
if lake build "${MODS[@]}" > "$TMP/build.log" 2>&1; then ok "${#MODS[@]} modules built"
else tail -20 "$TMP/build.log"; no "lake build of the compiler modules failed"; echo "TOTALITY-RATCHET: PASS=$PASS FAIL=$FAIL"; exit 1; fi

echo "=== C1: the classifier sees what it must on a known source ==="
lake env lean --run "$TOOL" --control > "$TMP/control.tsv" 2> "$TMP/control.err"
# runFrontend prints the control's own `sorry` warnings on stdout; the rows are the tab-separated lines.
grep '	' "$TMP/control.tsv" | cut -f1,2,4 | grep -v '^sorry	leafCount\.go\._mutual\.' | LC_ALL=C sort > "$TMP/control.got"
LC_ALL=C sort > "$TMP/control.want" <<'EOF'
axiom	controlAxiom	-
implemented_by	viaImplementedBy	-
opaque	opaqueNat	-
partial	evenSteps	mutual
partial	instBEqTree.beq	self
partial	oddSteps	mutual
partial	selfLoop	self
sorry	leafCount.go._mutual	-
sorry	sorried	-
unsafe	bogus._unsafe_rec	-
unsafe	unsafeImpl	-
EOF
if cmp -s "$TMP/control.got" "$TMP/control.want"; then
  ok "control source: partial/mutual/derived/unsafe/implemented_by/opaque/axiom/sorry seen; total and inert partial not"
else
  no "the classifier's control output changed (< want, > got):"; diff "$TMP/control.want" "$TMP/control.got" | head -20
  cat "$TMP/control.err" | head -5
fi

echo "=== the compiler's inventory against the baseline ==="
# TWO WALKS. Main and Concrete.Pipeline.PipelineTest are both executable roots and both declare
# `main`, so one environment cannot import them together. The second walk imports PipelineTest
# alone and keeps only that module's rows; everything it imports is already in the first walk.
SEPARATE="Concrete.Pipeline.PipelineTest"
MAIN_MODS=(); for m in "${MODS[@]}"; do [ "$m" = "$SEPARATE" ] || MAIN_MODS+=("$m"); done
inventory_failed() { head -5 "$1"; no "the inventory tool failed"; echo "TOTALITY-RATCHET: PASS=$PASS FAIL=$FAIL"; exit 1; }
lake env lean --run "$TOOL" "${MAIN_MODS[@]}" > "$TMP/inv.tsv" 2> "$TMP/inv.err" || inventory_failed "$TMP/inv.err"
if [ "${#MAIN_MODS[@]}" -ne "${#MODS[@]}" ]; then
  lake env lean --run "$TOOL" "$SEPARATE" > "$TMP/inv2.tsv" 2> "$TMP/inv2.err" || inventory_failed "$TMP/inv2.err"
  awk -F'\t' -v m="$SEPARATE" '$3 == m' "$TMP/inv2.tsv" >> "$TMP/inv.tsv"
  awk -F'\t' -v m="$SEPARATE" '$1 == "visited" && $2 == m' "$TMP/inv2.err" >> "$TMP/inv.err"
fi
# NOT VACUOUS, BY PROPERTY RATHER THAN COUNT: every module asked for that declares anything must
# have contributed declarations to the walk. A floor on the number of partial rows would turn red
# exactly when the conversions this gate exists to drive succeed.
missing=""
for i in "${!MODS[@]}"; do
  grep -qE '^(private |protected |noncomputable |partial |unsafe |@\[[^]]*\] )*(def|theorem|structure|inductive|abbrev|instance|class|opaque|axiom) ' "${SRCS[$i]}" || continue
  grep -qxF -- "$(printf 'visited\t%s' "${MODS[$i]}")" <(cut -f1,2 "$TMP/inv.err") || missing="$missing ${MODS[$i]}"
done
[ -z "$missing" ] && ok "the walk saw declarations from every declaring module (${#MODS[@]} asked for)" \
  || no "modules that declare things contributed nothing to the walk:$missing"

# THE BASELINE ONLY SHRINKS, against the base revision: merge-base with origin/main on a branch,
# the parent on main, or TOTALITY_BASE. A base without the baseline file is the change introducing it.
#   TOTALITY_BASE  set by CI on a push to the revision before the push (github.event.before), so a
#                  push of several commits is compared with what main held before ALL of them: the
#                  parent alone would accept a row added by an earlier commit of the same push. An
#                  all-zero value (a new ref) falls back to the merge-base.
#   no base given  merge-base(HEAD, origin/main). When HEAD is already on origin/main the push adds
#                  nothing and the parent is used. Without origin/main (a shallow or detached clone)
#                  the gate FAILS rather than guess: the parent of a multi-commit branch is the wrong base.
BASE="${TOTALITY_BASE:-}"
case "$BASE" in *[!0]*) ;; *) BASE="" ;; esac
BASE_ERR=""
if [ -z "$BASE" ]; then
  MB="$(git merge-base HEAD origin/main 2>/dev/null || true)"
  if [ -z "$MB" ]; then BASE_ERR="no merge-base with origin/main (fetch it, or set TOTALITY_BASE)"
  elif [ "$MB" != "$(git rev-parse HEAD)" ]; then BASE="$MB"
  else BASE="HEAD~1"; fi
fi
BASE_ARGS=()
if [ -n "$BASE_ERR" ]; then
  no "$BASE_ERR"
elif ! git rev-parse --verify -q "$BASE^{commit}" >/dev/null; then
  no "base revision '$BASE' does not resolve: fetch history (CI: fetch-depth 0) or set TOTALITY_BASE"
elif git show "$BASE:docs/compiler/totality_baseline.tsv" > "$TMP/base_baseline.tsv" 2>/dev/null; then
  BASE_ARGS=(--base-baseline "$TMP/base_baseline.tsv")
  ok "comparing the baseline with the base revision $(git rev-parse --short "$BASE")"
else
  ok "the base revision $(git rev-parse --short "$BASE") has no baseline: this change introduces it"
fi

if python3 "$RATCHET" check --inventory "$TMP/inv.tsv" "${BASE_ARGS[@]}" > "$TMP/check.out"; then
  ok "$(tail -1 "$TMP/check.out")"
else
  cat "$TMP/check.out"; no "the totality ratchet found the findings above"
fi

# A planted inventory must make the comparison fail with the named finding.
expect_fail() {  # $1 = label, $2 = inventory file, $3 = substring the finding must contain
  if python3 "$RATCHET" check --inventory "$2" > "$TMP/neg.out" 2>&1; then
    no "$1: the ratchet accepted it"
  elif grep -qF -- "$3" "$TMP/neg.out"; then ok "$1"
  else no "$1: failed, but without '$3'"; head -5 "$TMP/neg.out"; fi
}
echo "=== negative controls on planted inventories ==="
{ cat "$TMP/inv.tsv"; printf 'partial\tConcrete.totalityPlantedControl\tConcrete.IR.Lower\tself\t-\t1\n'; } > "$TMP/c2.tsv"
expect_fail "C2 a new partial in Lower fails" "$TMP/c2.tsv" "NEW non-total declaration: partial Concrete.totalityPlantedControl"
first="$(grep -m1 '^partial	' "$TMP/inv.tsv")"
grep -vxF -- "$first" "$TMP/inv.tsv" > "$TMP/c3.tsv"
expect_fail "C3 a baseline row whose declaration became total fails until deleted" "$TMP/c3.tsv" "is no longer non-total"
{ cat "$TMP/inv.tsv"; printf 'sorry\tConcrete.plantedSorry\tConcrete.Elab.Elab\t-\t-\t1\n'; } > "$TMP/c4.tsv"
expect_fail "C4 a sorry in a compiler declaration fails" "$TMP/c4.tsv" "SORRY reaches a compiler declaration"
{ cat "$TMP/inv.tsv"; printf 'axiom\tConcrete.plantedAxiom\tConcrete.Check.Check\t-\t-\t1\n'; } > "$TMP/c5.tsv"
expect_fail "C5 a non-native axiom in a compiler module fails" "$TMP/c5.tsv" "AXIOM declared in the compiler"
{ cat "$TMP/inv.tsv"; printf 'partial\tConcrete.TermIR.evalInt\tConcrete.Semantics.TermIR\tself\t-\t1\n'; } > "$TMP/c6.tsv"
expect_fail "C6 a pinned declaration turned partial fails" "$TMP/c6.tsv" "PINNED total"
# C7: admitting a new partial by adding its baseline row is refused against the base.
{ grep '^#' docs/compiler/totality_baseline.tsv; grep -v '^#' docs/compiler/totality_baseline.tsv | tail -n +2; } > "$TMP/c7_base.tsv"
if python3 "$RATCHET" check --inventory "$TMP/inv.tsv" --base-baseline "$TMP/c7_base.tsv" > "$TMP/neg.out" 2>&1; then
  no "C7 a row added to the baseline was accepted"
elif grep -qF "baseline row ADDED" "$TMP/neg.out"; then ok "C7 a row added to the baseline (relative to the base) fails"
else no "C7 failed, but not as an addition"; head -5 "$TMP/neg.out"; fi
# C8: two inventory rows that collapse to one name are refused rather than merged.
{ cat "$TMP/inv.tsv"; grep -m1 '^partial	' "$TMP/inv.tsv"; } > "$TMP/c8.tsv"
expect_fail "C8 two declarations sharing a normalized name are refused" "$TMP/c8.tsv" "share the name"

echo "TOTALITY-RATCHET: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
