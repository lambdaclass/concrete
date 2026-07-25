#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

# Mutation testing for the Concrete compiler.
# Applies targeted source mutations, rebuilds, and checks if the test suite catches them.
# A surviving mutation = a test gap.
#
# Usage:
#   bash scripts/tests/test_mutation.sh              # run all mutations
#   bash scripts/tests/test_mutation.sh --list       # list mutations without running
#   bash scripts/tests/test_mutation.sh --mutation N # run only mutation N

# Resolve `lake`: PATH first (the nix devshell puts it there, as does elan's
# shim), then elan's default location. A hardcoded elan path reported every
# mutation as "KILLED (build)" inside `nix develop`, because a missing toolchain
# is indistinguishable from an ill-typed mutation once the build has failed —
# a harness that cannot build must not be able to claim kills.
LAKE="${LAKE:-$(command -v lake || true)}"
[ -n "$LAKE" ] || LAKE="$HOME/.elan/bin/lake"
if ! "$LAKE" --version >/dev/null 2>&1; then
  echo "error: no working 'lake' found (tried \$LAKE, PATH, ~/.elan/bin/lake)." >&2
  echo "hint: run inside the devshell, e.g. 'nix develop --command bash scripts/tests/test_mutation.sh'" >&2
  exit 2
fi

KILLED=0
SURVIVED=0
ERRORS=0
TOTAL=0

# ============================================================
# Mutation definitions — parallel arrays
# ============================================================

MUT_FILE=()
MUT_OLD=()
MUT_NEW=()
MUT_DESC=()

# 1. Layout: i32/u32/f32 size 4 → 8  (tySize)
MUT_FILE+=("Concrete/Check/Layout.lean")
MUT_OLD+=("  | .i32 | .u32 | .float32 => 4
  | .i16 | .u16 => 2")
MUT_NEW+=("  | .i32 | .u32 | .float32 => 8
  | .i16 | .u16 => 2")
MUT_DESC+=("Layout: tySize i32/u32/f32 4 → 8")

# 2. Layout: i32/u32/f32 alignment 4 → 1  (tyAlign)
MUT_FILE+=("Concrete/Check/Layout.lean")
MUT_OLD+=("partial def tyAlign (ctx : Ctx) : Ty → Nat
  | .int | .uint | .float64 => 8
  | .i32 | .u32 | .float32 => 4")
MUT_NEW+=("partial def tyAlign (ctx : Ctx) : Ty → Nat
  | .int | .uint | .float64 => 8
  | .i32 | .u32 | .float32 => 1")
MUT_DESC+=("Layout: tyAlign i32/u32/f32 4 → 1")

# 3. Layout: unit size 0 → 4
MUT_FILE+=("Concrete/Check/Layout.lean")
MUT_OLD+=("  | .unit => 0
  | .string => Builtin.stringSize")
MUT_NEW+=("  | .unit => 4
  | .string => Builtin.stringSize")
MUT_DESC+=("Layout: tySize unit 0 → 4")

# 4. Layout: string size 24 → 16
MUT_FILE+=("Concrete/Check/Layout.lean")
MUT_OLD+=("def stringSize : Nat := 24")
MUT_NEW+=("def stringSize : Nat := 16")
MUT_DESC+=("Layout: string size 24 → 16")

# 5. Layout: string not pass-by-ptr
MUT_FILE+=("Concrete/Check/Layout.lean")
MUT_OLD+=("def isPassByPtr (ctx : Ctx) (ty : Ty) : Bool :=
  match ty with
  | .string => true")
MUT_NEW+=("def isPassByPtr (ctx : Ctx) (ty : Ty) : Bool :=
  match ty with
  | .string => false")
MUT_DESC+=("Layout: isPassByPtr string → false")

# 6. Layout: isFFISafe rejects integers
MUT_FILE+=("Concrete/Check/Layout.lean")
MUT_OLD+=("def isFFISafe (ctx : Ctx) (ty : Ty) : Bool :=
  match ty with
  | .int | .uint | .i8 | .i16 | .i32 | .u8 | .u16 | .u32 => true")
MUT_NEW+=("def isFFISafe (ctx : Ctx) (ty : Ty) : Bool :=
  match ty with
  | .int | .uint | .i8 | .i16 | .i32 | .u8 | .u16 | .u32 => false")
MUT_DESC+=("Layout: isFFISafe rejects integers")

# 7. Shared: floats not numeric
MUT_FILE+=("Concrete/Resolve/Shared.lean")
MUT_OLD+=("def isNumeric : Ty → Bool
  | .int | .uint | .i8 | .i16 | .i32 | .u8 | .u16 | .u32 => true
  | .float64 | .float32 => true")
MUT_NEW+=("def isNumeric : Ty → Bool
  | .int | .uint | .i8 | .i16 | .i32 | .u8 | .u16 | .u32 => true
  | .float64 | .float32 => false")
MUT_DESC+=("Shared: isNumeric rejects floats")

# 8. Shared: i32 not integer
MUT_FILE+=("Concrete/Resolve/Shared.lean")
MUT_OLD+=("def isInteger : Ty → Bool
  | .int | .uint | .i8 | .i16 | .i32 | .u8 | .u16 | .u32 => true")
MUT_NEW+=("def isInteger : Ty → Bool
  | .int | .uint | .i8 | .i16 | .u8 | .u16 | .u32 => true")
MUT_DESC+=("Shared: isInteger excludes i32")

# 9. Check: disable use-after-move detection
MUT_FILE+=("Concrete/Check/Check.lean")
MUT_OLD+=("    | .consumed =>
      throwCheck (.variableUsedAfterMove name) span")
MUT_NEW+=("    | .consumed => return () -- MUTATION: skip use-after-move
      -- throwCheck (.variableUsedAfterMove name) span")
MUT_DESC+=("Check: disable use-after-move")

# 10. Check: disable loop-depth linearity check
MUT_FILE+=("Concrete/Check/Check.lean")
MUT_OLD+=("      if info.loopDepth < env.loopDepth then
        throwCheck (.cannotConsumeLinearInLoop name) span")
MUT_NEW+=("      if false then -- MUTATION: loop-depth disabled
        throwCheck (.cannotConsumeLinearInLoop name) span")
MUT_DESC+=("Check: disable loop-depth linearity")

# 11. Check: disable scope-exit unconsumed check
MUT_FILE+=("Concrete/Check/Check.lean")
MUT_OLD+=("      if !info.isCopy && info.state != .consumed && info.state != .reserved then
        throwCheck (.linearVariableNeverConsumed name) span")
MUT_NEW+=("      if false then -- MUTATION: scope check disabled
        throwCheck (.linearVariableNeverConsumed name) span")
MUT_DESC+=("Check: disable scope-exit linearity")

# 12. CoreCheck: disable match exhaustiveness
MUT_FILE+=("Concrete/Check/CoreCheck.lean")
MUT_OLD+=("            if !seenVariants.contains vn then
              addCCError (.matchMissingVariant name vn)")
MUT_NEW+=("            if !seenVariants.contains vn then
              pure () -- MUTATION: exhaustiveness disabled")
MUT_DESC+=("CoreCheck: disable match exhaustiveness")

# 13. CoreCheck: disable capability discipline
MUT_FILE+=("Concrete/Check/CoreCheck.lean")
MUT_OLD+=("      if !capsContain env.currentCapSet calleeCaps then
        addCCError (.insufficientCapabilities fn (capSetToString calleeCaps) (capSetToString env.currentCapSet))")
MUT_NEW+=("      if !capsContain env.currentCapSet calleeCaps then
        pure () -- MUTATION: capability check disabled")
MUT_DESC+=("CoreCheck: disable capability check")

# 14. CoreCheck: allow break outside loop
MUT_FILE+=("Concrete/Check/CoreCheck.lean")
MUT_OLD+=("    if !env.inLoop then
      addCCError .breakOutsideLoop")
MUT_NEW+=("    if false then -- MUTATION: break check disabled
      addCCError .breakOutsideLoop")
MUT_DESC+=("CoreCheck: allow break outside loop")

# 15. Lower: arrayIndex GEP uses .int instead of elem type
MUT_FILE+=("Concrete/IR/Lower.lean")
MUT_OLD+=("    emit (.gep gepDst aVal [iVal] ty)
    let loadDst ← freshReg
    emit (.load loadDst (.reg gepDst ty) ty)")
MUT_NEW+=("    emit (.gep gepDst aVal [iVal] .int)
    let loadDst ← freshReg
    emit (.load loadDst (.reg gepDst .int) .int)")
MUT_DESC+=("Lower: arrayIndex GEP uses .int")

# 16. EmitSSA: isReprCStruct always false
MUT_FILE+=("Concrete/Backend/EmitSSA.lean")
MUT_OLD+=("private def isReprCStruct (s : EmitSSAState) : Ty → Bool
  | .named name => (Layout.lookupStruct (layoutCtxOf s) name).any (·.isReprC)
  | _ => false")
MUT_NEW+=("private def isReprCStruct (_s : EmitSSAState) : Ty → Bool
  | _ => false")
MUT_DESC+=("EmitSSA: isReprCStruct always false")

# 17. SSAVerify: disable aggregate phi check
MUT_FILE+=("Concrete/IR/SSAVerify.lean")
MUT_OLD+=("      let ctx := if isAggregateType ty then
        addSSAError ctx (.aggregatePhi b.label dst (reprStr ty))
      else ctx")
MUT_NEW+=("      let ctx := if false then -- MUTATION: agg phi disabled
        addSSAError ctx (.aggregatePhi b.label dst (reprStr ty))
      else ctx")
MUT_DESC+=("SSAVerify: disable aggregate phi check")

# 18. SSAVerify: disable phi missing-predecessor check
MUT_FILE+=("Concrete/IR/SSAVerify.lean")
MUT_OLD+=("        if phiLabels.contains p then ctx
        else addSSAError ctx (.phiMissingPredecessor b.label p)")
MUT_NEW+=("        if phiLabels.contains p then ctx
        else ctx -- MUTATION: phi pred check disabled")
MUT_DESC+=("SSAVerify: disable phi predecessor check")

# 19. Mono: no user generic enum is specialized (R-0001 / bug 051)
# Treating every generic enum as a builtin removes BOTH halves of the fix at
# once: no per-instantiation declaration is created, and the residual E0808
# containment goes vacuous because its name list is empty. That is precisely the
# pre-fix state where instantiations of different size share one declaration, so
# a surviving mutation would mean nothing pins the layout.
MUT_FILE+=("Concrete/IR/Mono.lean")
MUT_OLD+=("  let isBuiltin (ed : CEnumDef) : Bool :=
    ed.builtinId.isSome || ed.name == optionEnumName || ed.name == resultEnumName")
MUT_NEW+=("  let isBuiltin (_ed : CEnumDef) : Bool := true -- MUTATION: enum mono disabled")
MUT_DESC+=("Mono: user generic enums not specialized (bug 051)")

# 20. Mono: enums recognized as generic but left out of the specialization map
# The complement of #19: detection stays ON while specialization is skipped, so
# the residual E0808 containment is armed and MUST fire. Correct programs never
# reach E0808, so this mutation is the only thing proving that path is live
# rather than dead code — without it, deleting the backstop would go unnoticed
# until something else regressed.
MUT_FILE+=("Concrete/IR/Mono.lean")
MUT_OLD+=("    if allStructs.any (fun sd => sd.name == name) || allEnums.any (fun ed => ed.name == name)
    then some (name, args, monoTypeName name args)")
MUT_NEW+=("    if allStructs.any (fun sd => sd.name == name) -- MUTATION: enums unmapped
    then some (name, args, monoTypeName name args)")
MUT_DESC+=("Mono: generic enums detected but unmapped (E0808 backstop)")

# 21. Elab: emit an indirect call as a DIRECT one (R-0002 / bug 050)
# Restores the pre-fix Core shape — a call through a fn-typed local becomes
# indistinguishable from a direct call, so Mono resolves the binding name against
# the global fn map again and a same-named generic hijacks the call. This is the
# mutation the roadmap asks for: "routes indirect calls through direct-name
# resolution". It must be caught by the fn-pointer fixtures, not merely by
# something downstream noticing an undefined symbol.
MUT_FILE+=("Concrete/Elab/Elab.lean")
MUT_OLD+=("    return .call (.indirect fnName) [] cArgs retTy")
MUT_NEW+=("    return .call (.direct fnName) [] cArgs retTy -- MUTATION: indirect call resolved by name")
MUT_DESC+=("Elab: indirect call emitted as direct (bug 050)")

# 22. std map: forget the remembered tombstone (R-0003 / bug 047)
# Insert stops at the first free-or-tombstone slot again, so a key living past a
# tombstone gets a second live slot. Independent of #23 and #24: this one is the
# duplication invariant only.
MUT_FILE+=("std/src/map.con")
MUT_OLD+=("                if flag == 2 {
                    if !have_tomb {
                        have_tomb = true;
                        tomb_idx = idx;
                    }
                }")
MUT_NEW+=("                if flag == 2 {
                    // MUTATION: write into the first tombstone immediately
                    let key_ptr: *mut K = self.keys + idx;
                    *key_ptr = key;
                    let val_ptr: *mut V = self.values + idx;
                    *val_ptr = value;
                    *flag_ptr = 1;
                    self.len = self.len + 1;
                    self.tombstones = self.tombstones - 1;
                    return Option::<V>::None;
                }")
MUT_DESC+=("std map: insert reuses the first tombstone (bug 047)")

# 23. std map: unbounded lookup probe (R-0003 / bug 048, half 1)
# The probe no longer stops after `cap` slots, so a missing-key lookup in a table
# with no empty slot wraps forever. Caught as a TIMEOUT, which is why the gate
# runs every leg under a watchdog.
MUT_FILE+=("std/src/map.con")
MUT_OLD+=("            while probes < self.cap {
                let flag_ptr: *mut u8 = self.flags + idx;
                let flag: u8 = *flag_ptr;

                if flag == 0 {
                    // Empty — the probe chain ends here, so the key is absent.
                    return Option::<u64>::None;
                }")
MUT_NEW+=("            while true { // MUTATION: probe bound removed
                let flag_ptr: *mut u8 = self.flags + idx;
                let flag: u8 = *flag_ptr;

                if flag == 0 {
                    // Empty — the probe chain ends here, so the key is absent.
                    return Option::<u64>::None;
                }")
MUT_DESC+=("std map: lookup probe unbounded (bug 048)")

# 24. std map: occupancy ignores tombstones (R-0003 / bug 048, half 2)
# The load factor counts only live entries again, so tombstones accumulate until
# the table has no empty slot. With #23's bound still in place the lookup returns
# rather than hanging, so what this must break is the VALUE invariants — proving
# the two halves of 048 are gated separately.
MUT_FILE+=("std/src/map.con")
MUT_OLD+=("            if (self.len + self.tombstones) * 4 >= self.cap * 3 {")
MUT_NEW+=("            if self.len * 4 >= self.cap * 3 { // MUTATION: tombstones uncounted")
MUT_DESC+=("std map: load factor ignores tombstones (bug 048)")

NUM_MUTATIONS=${#MUT_FILE[@]}

# ============================================================
# Argument parsing
# ============================================================

MODE="run"
SINGLE_IDX=0

while [[ $# -gt 0 ]]; do
  case "$1" in
    --list)
      MODE="list"
      shift
      ;;
    --mutation)
      MODE="single"
      SINGLE_IDX="$2"
      shift 2
      ;;
    *)
      echo "Unknown option: $1"
      echo "Usage: bash scripts/tests/test_mutation.sh [--list] [--mutation N]"
      exit 1
      ;;
  esac
done

# ============================================================
# List mode
# ============================================================

if [[ "$MODE" == "list" ]]; then
  echo "=== Mutation List ($NUM_MUTATIONS mutations) ==="
  for (( i=0; i<NUM_MUTATIONS; i++ )); do
    idx=$((i + 1))
    printf "  [%2d/%d] %-30s %s\n" "$idx" "$NUM_MUTATIONS" "${MUT_FILE[$i]}:" "${MUT_DESC[$i]}"
  done
  exit 0
fi

# ============================================================
# Apply / restore mutation using exact string replacement
# ============================================================

apply_mutation() {
  local idx=$1
  local file="${MUT_FILE[$idx]}"
  local old="${MUT_OLD[$idx]}"
  local new="${MUT_NEW[$idx]}"

  # Save backup
  cp "$file" "$file.mutbak"

  # Use python for reliable multi-line string replacement
  python3 -c "
import sys
path = sys.argv[1]
old = sys.argv[2]
new = sys.argv[3]
with open(path, 'r') as f:
    content = f.read()
if old not in content:
    sys.exit(1)
content = content.replace(old, new, 1)
with open(path, 'w') as f:
    f.write(content)
" "$file" "$old" "$new"
}

restore_mutation() {
  local idx=$1
  local file="${MUT_FILE[$idx]}"
  mv "$file.mutbak" "$file"
  # Rebuild so the tree's BINARY matches its restored SOURCE. Restoring only the
  # source leaves `.lake/build/bin/concrete` built from the mutation, and anything
  # run afterwards — a gate, a probe, another script — silently measures the
  # mutated compiler while the source looks clean. That is a trap this harness
  # has sprung on its callers more than once, and it costs more than the rebuild.
  $LAKE build > /tmp/mutation_restore_build.log 2>&1 \
    || echo "  WARNING: rebuild after restore FAILED — .lake holds a mutated binary (see /tmp/mutation_restore_build.log)" >&2
}

# ============================================================
# Run a single mutation
# ============================================================

run_mutation() {
  local num=$1       # 1-based index for display
  local idx=$((num - 1))  # 0-based index for arrays

  printf "[%2d/%d] %-30s %-45s ... " "$num" "$NUM_MUTATIONS" "${MUT_FILE[$idx]}:" "${MUT_DESC[$idx]}"

  # Apply mutation
  if ! apply_mutation "$idx"; then
    echo "SKIPPED (pattern not found in file)"
    ERRORS=$((ERRORS + 1))
    TOTAL=$((TOTAL + 1))
    # Restore if backup was created
    [[ -f "${MUT_FILE[$idx]}.mutbak" ]] && restore_mutation "$idx"
    return
  fi

  local result=""

  # Try to build
  if $LAKE build > /tmp/mutation_build.log 2>&1; then
    # Build succeeded — run tests
    if bash scripts/tests/run_tests.sh --fast > /tmp/mutation_test.log 2>&1; then
      result="SURVIVED"
      SURVIVED=$((SURVIVED + 1))
    else
      result="KILLED"
      KILLED=$((KILLED + 1))
    fi
  else
    # Build failed — type system caught it
    result="KILLED (build)"
    KILLED=$((KILLED + 1))
  fi

  # Restore original
  restore_mutation "$idx"
  TOTAL=$((TOTAL + 1))

  if [[ "$result" == "SURVIVED" ]]; then
    echo "$result  <-- TEST GAP"
  else
    echo "$result"
  fi
}

# ============================================================
# Main
# ============================================================

echo "=== Mutation Testing ($NUM_MUTATIONS mutations) ==="
echo ""

# Preflight: the PRISTINE tree must build. Otherwise every mutation reports
# "KILLED (build)" and the run claims perfect coverage while having tested
# nothing — the same shape as a CI job that is green because it never ran.
printf "preflight: pristine tree builds ... "
if $LAKE build > /tmp/mutation_preflight.log 2>&1; then
  echo "ok"
else
  echo "FAILED"
  echo "error: the unmutated tree does not build, so kill/survive verdicts would be meaningless." >&2
  echo "       see /tmp/mutation_preflight.log" >&2
  exit 2
fi
echo ""

if [[ "$MODE" == "single" ]]; then
  if [[ "$SINGLE_IDX" -lt 1 || "$SINGLE_IDX" -gt "$NUM_MUTATIONS" ]]; then
    echo "Error: mutation index must be between 1 and $NUM_MUTATIONS"
    exit 1
  fi
  run_mutation "$SINGLE_IDX"
else
  for (( i=1; i<=NUM_MUTATIONS; i++ )); do
    run_mutation "$i"
  done
fi

echo ""
echo "=== Results: $KILLED killed, $SURVIVED survived, $ERRORS errors ($TOTAL total) ==="

if [[ "$SURVIVED" -gt 0 ]]; then
  echo ""
  echo "WARNING: $SURVIVED mutation(s) survived — these represent test gaps."
  exit 1
fi
