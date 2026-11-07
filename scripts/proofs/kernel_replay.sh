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
# THE MODULE LIST COMES FROM THE SOURCES (ISS-0016). Given a prefix, leanchecker replays every .olean
# on its search path under that prefix. The build directory also holds .olean files of modules whose
# source was deleted or renamed (the CI lake cache restores an older build through a restore-key
# prefix), so trusting it replays modules that no longer exist: CI run 42424028055 failed on
# ConcreteSpec.Spec.Ownership after its rename to Slice1Ownership. Both legs therefore run on a staged
# library holding the .olean files of exactly the modules the sources declare (spec_modules), with the
# package's build directory replaced by the stage in LEAN_PATH, and M checks that the replayed set is
# exactly that list. scripts/proofs/lib.sh also prunes orphaned build files before building.
#
# NEGATIVE CONTROL: a throwaway package of this toolchain with two modules. ReplayGood proves a true
# statement honestly; ReplayBad adds `ReplayBad.forged : False` with `debug.skipKernelTC` and proves
# a theorem from it. Both build, and the forged theorem uses no axiom (so the axiom inventory would
# pass it). leanchecker must accept ReplayGood and reject ReplayBad at the forged declaration, in
# both modes. ORPHAN CONTROL: that forged module planted as an orphan under ConcreteSpec/ in a copy of
# the build directory fails an unfiltered replay of the copy, and the staged replay of the same copy
# ignores it and passes; a stage missing a source module is refused.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/../checks/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
CHECK_NAME=PROOF-KERNEL-REPLAY
source scripts/proofs/lib.sh

echo "=== P: population ==="
population imported > "$TMP/imported.txt" || { no "PROOF_MODE=imported failed"; finish; }
GAP=$(closure_gap "$TMP/imported.txt")
[ -z "$GAP" ] && ok "all $(spec_modules | grep -c .) ConcreteSpec source modules are in the closure the replay reads" \
  || no "source modules outside the import closure of ConcreteSpec: $(echo $GAP)"

echo "=== F/M: replay ==="
spec_modules > "$TMP/modules.txt"
PKG_LIB="$ROOT_DIR/.lake/build/lib/lean"
TOOL_PATH="$("$LAKE" env printenv LEAN_PATH)" || { no "lake env printenv LEAN_PATH failed"; finish; }
# stage_lib FROM DEST < MODULES: copy each listed module's .olean files (FROM/A/B.olean*, no .hash)
# into DEST. A listed module with no .olean is a problem line; nothing unlisted is copied.
stage_lib() {
  local from="$1" dest="$2" m p f n
  mkdir -p "$dest"
  while read -r m; do
    p="${m//.//}"; n=0
    for f in "$from/$p".olean "$from/$p".olean.*; do
      case "$f" in *.hash) continue ;; esac
      [ -f "$f" ] || continue
      mkdir -p "$dest/$(dirname "$p")"; cp "$f" "$dest/$(dirname "$p")/" && n=$((n+1))
    done
    [ "$n" -gt 0 ] || echo "no .olean for source module $m under $from"
  done
}
# staged_path STAGE: LEAN_PATH with the package's build directory replaced by STAGE (exactly once).
staged_path() {
  printf '%s' "$TOOL_PATH" | tr ':' '\n' | awk -v pkg="$PKG_LIB" -v st="$1" \
    '$0 == pkg { print st; n++; next } { print } END { if (n != 1) exit 1 }' | paste -sd: -
}
# replay_problems EXPECTED REPLAYED-OUTPUT: the module sets differ, as one line per difference.
replay_problems() {
  sed -n 's/^replaying \(ConcreteSpec[^ ]*\).*/\1/p' "$2" | sort -u > "$2.set"
  comm -23 "$1" "$2.set" | sed 's/^/not replayed: /'
  comm -13 "$1" "$2.set" | sed 's/^/replayed but not a source module: /'
}
leancheck() { # LEAN_PATH args...
  local lp="$1"; shift; "$LAKE" env env LEAN_PATH="$lp" leanchecker "$@"; }
STAGE="$TMP/stage"
out="$(stage_lib "$PKG_LIB" "$STAGE" < "$TMP/modules.txt")"
[ -z "$out" ] || { no "staging the source modules: $(echo $out)"; finish; }
SPATH="$(staged_path "$STAGE")" || { no "LEAN_PATH ($TOOL_PATH) does not name $PKG_LIB exactly once"; finish; }
ok "staged the $(grep -c . "$TMP/modules.txt") source modules' .olean files; LEAN_PATH reads the stage, not the build directory"
t0=$SECONDS
leancheck "$SPATH" --fresh ConcreteSpec > "$TMP/f.out" 2>&1; rc=$?
[ "$rc" = 0 ] && ok "leanchecker --fresh ConcreteSpec: the whole closure re-checked from an empty kernel environment ($((SECONDS-t0))s)" \
  || no "leanchecker --fresh ConcreteSpec failed (rc=$rc): $(tail -5 "$TMP/f.out" | tr '\n' ' ')"
t0=$SECONDS
leancheck "$SPATH" --verbose ConcreteSpec > "$TMP/m.out" 2>&1; rc=$?
diff_m="$(replay_problems "$TMP/modules.txt" "$TMP/m.out")"
NREP=$(grep -c . "$TMP/m.out.set")
[ "$rc" = 0 ] && [ -z "$diff_m" ] \
  && ok "leanchecker ConcreteSpec: exactly the $NREP source modules replayed on their imports ($((SECONDS-t0))s)" \
  || no "leanchecker ConcreteSpec: rc=$rc, $NREP modules replayed; $(echo $diff_m) $(grep -v '^replaying' "$TMP/m.out" | tail -5 | tr '\n' ' ')"

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
  echo "=== orphan control: a build-directory module with no source ==="
  O="$TMP/orphan-lib"; cp -a "$PKG_LIB" "$O"
  cp "$C/.lake/build/lib/lean/ReplayBad.olean" "$O/ConcreteSpec/Spec/OrphanControl.olean"
  leancheck "$(staged_path "$O")" --verbose ConcreteSpec > "$TMP/o1.out" 2>&1; r1=$?
  [ "$r1" != 0 ] && has_match -F "(kernel) declaration type mismatch, 'ReplayBad.forged'" < "$TMP/o1.out" \
    && ok "control: an unfiltered replay of the build directory replays the planted orphan and fails (rc=$r1)" \
    || no "control: the planted orphan did not fail an unfiltered replay (rc=$r1): $(tail -3 "$TMP/o1.out" | tr '\n' ' ')"
  OS="$TMP/orphan-stage"; out="$(stage_lib "$O" "$OS" < "$TMP/modules.txt")"
  leancheck "$(staged_path "$OS")" --verbose ConcreteSpec > "$TMP/o2.out" 2>&1; r2=$?
  [ -z "$out" ] && [ "$r2" = 0 ] && [ -z "$(replay_problems "$TMP/modules.txt" "$TMP/o2.out")" ] \
    && [ ! -e "$OS/ConcreteSpec/Spec/OrphanControl.olean" ] \
    && ok "control: the staged replay of the same directory ignores the orphan and replays exactly the source modules" \
    || no "control: staged replay with a planted orphan: rc=$r2 $out $(replay_problems "$TMP/modules.txt" "$TMP/o2.out" | tr '\n' ' ')"
  PB="$TMP/prune-build"; mkdir -p "$PB/lib" "$PB/ir/ConcreteSpec/Spec"; cp -a "$PKG_LIB" "$PB/lib/lean"
  for x in olean ilean trace olean.hash; do echo x > "$PB/lib/lean/ConcreteSpec/Spec/OrphanControl.$x"; done
  echo x > "$PB/ir/ConcreteSpec/Spec/OrphanControl.c"
  pout="$(prune_orphans "$PB")"
  left="$(find "$PB" -name '*Orphan*' | tr '\n' ' ')"
  kept="$(stage_lib "$PB/lib/lean" "$TMP/prune-kept" < "$TMP/modules.txt")"
  [ -z "$left" ] && [ -z "$kept" ] && [ "$(grep -c '^pruned orphan' <<< "$pout")" = 5 ] \
    && ok "control: prune_orphans removes the 5 planted orphan files and keeps every source module's .olean" \
    || no "control: prune_orphans left '$left', lost '$kept', said: $(echo $pout)"
  { cat "$TMP/modules.txt"; echo ConcreteSpec.Spec.NoSuchModule; } | sort > "$TMP/modules-plus.txt"
  out="$(stage_lib "$PKG_LIB" "$TMP/missing-stage" < "$TMP/modules-plus.txt")"
  has_match -F "no .olean for source module ConcreteSpec.Spec.NoSuchModule" <<< "$out" \
    && ok "control: a source module with no built .olean is refused, not skipped" || no "control: missing module staged silently: $out"
  printf 'replaying ConcreteSpec.Spec.Basics\n' > "$TMP/short.out"
  [ -n "$(replay_problems "$TMP/modules.txt" "$TMP/short.out")" ] \
    && ok "control: a replay that skips source modules is refused by the set comparison" || no "control: short replay accepted"
else
  no "control: the control package did not build: $(tail -5 "$C/build.out" | tr '\n' ' ')"
fi
finish
