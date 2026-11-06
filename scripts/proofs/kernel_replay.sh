#!/usr/bin/env bash
# FRESH KERNEL RE-CHECK: the built specification replayed through Lean's kernel from nothing, with
# leanchecker (the lean4checker that ships in the toolchain; no Mathlib).
#
# lake build checks each declaration once, as it is elaborated; what lands in the .olean files is
# whatever the elaborator added. A declaration added without checking (`debug.skipKernelTC`, or a
# metaprogram calling the kernel's unchecked add) is in the environment unchecked, and it can carry
# no axiom at all, so scripts/proofs/axioms.sh cannot see it. Replaying the stored declarations
# through the kernel can.
#
#   P  every module the ConcreteSpec sources declare is in the root's import closure, so replaying
#      the root replays all of them (the closure itself is listed from the environment);
#   F  `leanchecker --fresh ConcreteSpec`: every constant of the root's closure, Lean core included,
#      re-added to an EMPTY kernel environment, in dependency order;
#   M  `leanchecker ConcreteSpec`: every ConcreteSpec module's own declarations replayed on top of its
#      imports, module by module (the mode that names the module at fault).
#
# NEGATIVE CONTROL: a throwaway package of this toolchain with two modules. ReplayGood proves a true
# statement honestly; ReplayBad adds `ReplayBad.forged : False` with `debug.skipKernelTC` and proves
# a theorem from it. Both build, and the forged theorem uses no axiom (so the axiom inventory would
# pass it). leanchecker must accept ReplayGood and reject ReplayBad at the forged declaration, in
# both modes.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/../checks/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
CHECK_NAME=PROOF-KERNEL-REPLAY
source scripts/proofs/lib.sh

echo "=== P: population ==="
population imported > "$TMP/imported.txt" || { no "PROOF_MODE=imported failed"; finish; }
GAP=$(spec_modules | comm -23 - <(sort -u "$TMP/imported.txt"))
[ -z "$GAP" ] && ok "all $(spec_modules | grep -c .) ConcreteSpec source modules are in the closure the replay reads" \
  || no "source modules outside the import closure of ConcreteSpec: $(echo $GAP)"

echo "=== F/M: replay ==="
t0=$SECONDS
"$LAKE" env leanchecker --fresh ConcreteSpec > "$TMP/f.out" 2>&1; rc=$?
[ "$rc" = 0 ] && ok "leanchecker --fresh ConcreteSpec: the whole closure re-checked from an empty kernel environment ($((SECONDS-t0))s)" \
  || no "leanchecker --fresh ConcreteSpec failed (rc=$rc): $(tail -5 "$TMP/f.out" | tr '\n' ' ')"
t0=$SECONDS
"$LAKE" env leanchecker --verbose ConcreteSpec > "$TMP/m.out" 2>&1; rc=$?
NREP=$(grep -c '^replaying ConcreteSpec' "$TMP/m.out")
NMOD=$(grep -c . "$TMP/imported.txt")
[ "$rc" = 0 ] && [ "$NREP" = "$NMOD" ] \
  && ok "leanchecker ConcreteSpec: all $NREP modules replayed on their imports ($((SECONDS-t0))s)" \
  || no "leanchecker ConcreteSpec: rc=$rc, $NREP modules replayed of $NMOD: $(grep -v '^replaying' "$TMP/m.out" | tail -5 | tr '\n' ' ')"

echo "=== negative control: a declaration added without kernel checking ==="
C="$TMP/control"; mkdir -p "$C"
cp lean-toolchain "$C/"
cat > "$C/lakefile.toml" <<'EOF'
name = "replay-control"
[[lean_lib]]
name = "ReplayGood"
[[lean_lib]]
name = "ReplayBad"
EOF
cat > "$C/ReplayGood.lean" <<'EOF'
theorem ReplayGood.honest : 2 + 2 = 4 := rfl
EOF
cat > "$C/ReplayBad.lean" <<'EOF'
import Lean
open Lean Elab Command
set_option debug.skipKernelTC true in
run_cmd liftCoreM <| addDecl <| .thmDecl {
  name := `ReplayBad.forged, levelParams := [], type := mkConst ``False, value := mkConst ``True.intro }
theorem ReplayBad.anything : 1 = 2 := False.elim ReplayBad.forged
#print axioms ReplayBad.anything
EOF
if (cd "$C" && "$LAKE" build ReplayGood ReplayBad > build.out 2>&1); then
  has_match -F "'ReplayBad.anything' does not depend on any axioms" < "$C/build.out" \
    && ok "control: the forged theorem builds and depends on no axiom (the axiom inventory cannot see it)" \
    || no "control: unexpected axiom report for the forged theorem: $(grep -i axiom "$C/build.out" | head -2 | tr '\n' ' ')"
  for mode in --fresh ""; do
    (cd "$C" && "$LAKE" env leanchecker $mode ReplayGood) > "$TMP/g.out" 2>&1; rg=$?
    (cd "$C" && "$LAKE" env leanchecker $mode ReplayBad) > "$TMP/b.out" 2>&1; rb=$?
    [ "$rg" = 0 ] && ok "control: leanchecker ${mode:-(per module)} accepts the honest module" \
      || no "control: leanchecker ${mode:-(per module)} rejected the honest module (rc=$rg): $(tail -3 "$TMP/g.out" | tr '\n' ' ')"
    [ "$rb" != 0 ] && has_match -F "(kernel) declaration type mismatch, 'ReplayBad.forged'" < "$TMP/b.out" \
      && ok "control: leanchecker ${mode:-(per module)} rejects the forged declaration (rc=$rb)" \
      || no "control: leanchecker ${mode:-(per module)} on the forged module: rc=$rb: $(tail -3 "$TMP/b.out" | tr '\n' ' ')"
  done
else
  no "control: the control package did not build: $(tail -5 "$C/build.out" | tr '\n' ' ')"
fi
finish
