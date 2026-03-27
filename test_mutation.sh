#!/usr/bin/env bash
set -euo pipefail

# Mutation testing for the Concrete compiler.
# Applies targeted source mutations, rebuilds, and checks if the test suite catches them.
# A surviving mutation = a test gap.
#
# Usage:
#   bash test_mutation.sh              # run all mutations
#   bash test_mutation.sh --list       # list mutations without running
#   bash test_mutation.sh --mutation N # run only mutation N

LAKE="$HOME/.elan/bin/lake"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR"

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
MUT_FILE+=("Concrete/Layout.lean")
MUT_OLD+=("  | .i32 | .u32 | .float32 => 4
  | .i16 | .u16 => 2")
MUT_NEW+=("  | .i32 | .u32 | .float32 => 8
  | .i16 | .u16 => 2")
MUT_DESC+=("Layout: tySize i32/u32/f32 4 → 8")

# 2. Layout: i32/u32/f32 alignment 4 → 1  (tyAlign)
MUT_FILE+=("Concrete/Layout.lean")
MUT_OLD+=("partial def tyAlign (ctx : Ctx) : Ty → Nat
  | .int | .uint | .float64 => 8
  | .i32 | .u32 | .float32 => 4")
MUT_NEW+=("partial def tyAlign (ctx : Ctx) : Ty → Nat
  | .int | .uint | .float64 => 8
  | .i32 | .u32 | .float32 => 1")
MUT_DESC+=("Layout: tyAlign i32/u32/f32 4 → 1")

# 3. Layout: unit size 0 → 4
MUT_FILE+=("Concrete/Layout.lean")
MUT_OLD+=("  | .unit => 0
  | .string => Builtin.stringSize")
MUT_NEW+=("  | .unit => 4
  | .string => Builtin.stringSize")
MUT_DESC+=("Layout: tySize unit 0 → 4")

# 4. Layout: string size 24 → 16
MUT_FILE+=("Concrete/Layout.lean")
MUT_OLD+=("def stringSize : Nat := 24")
MUT_NEW+=("def stringSize : Nat := 16")
MUT_DESC+=("Layout: string size 24 → 16")

# 5. Layout: string not pass-by-ptr
MUT_FILE+=("Concrete/Layout.lean")
MUT_OLD+=("def isPassByPtr (ctx : Ctx) (ty : Ty) : Bool :=
  match ty with
  | .string => true")
MUT_NEW+=("def isPassByPtr (ctx : Ctx) (ty : Ty) : Bool :=
  match ty with
  | .string => false")
MUT_DESC+=("Layout: isPassByPtr string → false")

# 6. Layout: isFFISafe rejects integers
MUT_FILE+=("Concrete/Layout.lean")
MUT_OLD+=("def isFFISafe (ctx : Ctx) (ty : Ty) : Bool :=
  match ty with
  | .int | .uint | .i8 | .i16 | .i32 | .u8 | .u16 | .u32 => true")
MUT_NEW+=("def isFFISafe (ctx : Ctx) (ty : Ty) : Bool :=
  match ty with
  | .int | .uint | .i8 | .i16 | .i32 | .u8 | .u16 | .u32 => false")
MUT_DESC+=("Layout: isFFISafe rejects integers")

# 7. Shared: floats not numeric
MUT_FILE+=("Concrete/Shared.lean")
MUT_OLD+=("def isNumeric : Ty → Bool
  | .int | .uint | .i8 | .i16 | .i32 | .u8 | .u16 | .u32 => true
  | .float64 | .float32 => true")
MUT_NEW+=("def isNumeric : Ty → Bool
  | .int | .uint | .i8 | .i16 | .i32 | .u8 | .u16 | .u32 => true
  | .float64 | .float32 => false")
MUT_DESC+=("Shared: isNumeric rejects floats")

# 8. Shared: i32 not integer
MUT_FILE+=("Concrete/Shared.lean")
MUT_OLD+=("def isInteger : Ty → Bool
  | .int | .uint | .i8 | .i16 | .i32 | .u8 | .u16 | .u32 => true")
MUT_NEW+=("def isInteger : Ty → Bool
  | .int | .uint | .i8 | .i16 | .u8 | .u16 | .u32 => true")
MUT_DESC+=("Shared: isInteger excludes i32")

# 9. Check: disable use-after-move detection
MUT_FILE+=("Concrete/Check.lean")
MUT_OLD+=("    | .consumed =>
      throwCheck (.variableUsedAfterMove name) span")
MUT_NEW+=("    | .consumed => return () -- MUTATION: skip use-after-move
      -- throwCheck (.variableUsedAfterMove name) span")
MUT_DESC+=("Check: disable use-after-move")

# 10. Check: disable loop-depth linearity check
MUT_FILE+=("Concrete/Check.lean")
MUT_OLD+=("      if info.loopDepth < env.loopDepth then
        throwCheck (.cannotConsumeLinearInLoop name) span")
MUT_NEW+=("      if false then -- MUTATION: loop-depth disabled
        throwCheck (.cannotConsumeLinearInLoop name) span")
MUT_DESC+=("Check: disable loop-depth linearity")

# 11. Check: disable scope-exit unconsumed check
MUT_FILE+=("Concrete/Check.lean")
MUT_OLD+=("      if !info.isCopy && info.state != .consumed && info.state != .reserved then
        throwCheck (.linearVariableNeverConsumed name) span")
MUT_NEW+=("      if false then -- MUTATION: scope check disabled
        throwCheck (.linearVariableNeverConsumed name) span")
MUT_DESC+=("Check: disable scope-exit linearity")

# 12. CoreCheck: disable match exhaustiveness
MUT_FILE+=("Concrete/CoreCheck.lean")
MUT_OLD+=("            if !seenVariants.contains vn then
              addCCError (.matchMissingVariant name vn)")
MUT_NEW+=("            if !seenVariants.contains vn then
              pure () -- MUTATION: exhaustiveness disabled")
MUT_DESC+=("CoreCheck: disable match exhaustiveness")

# 13. CoreCheck: disable capability discipline
MUT_FILE+=("Concrete/CoreCheck.lean")
MUT_OLD+=("      if !capsContain env.currentCapSet calleeCaps then
        addCCError (.insufficientCapabilities fn (capSetToString calleeCaps) (capSetToString env.currentCapSet))")
MUT_NEW+=("      if !capsContain env.currentCapSet calleeCaps then
        pure () -- MUTATION: capability check disabled")
MUT_DESC+=("CoreCheck: disable capability check")

# 14. CoreCheck: allow break outside loop
MUT_FILE+=("Concrete/CoreCheck.lean")
MUT_OLD+=("    if !env.inLoop then
      addCCError .breakOutsideLoop")
MUT_NEW+=("    if false then -- MUTATION: break check disabled
      addCCError .breakOutsideLoop")
MUT_DESC+=("CoreCheck: allow break outside loop")

# 15. Lower: arrayIndex GEP uses .int instead of elem type
MUT_FILE+=("Concrete/Lower.lean")
MUT_OLD+=("    emit (.gep gepDst aVal [iVal] ty)
    let loadDst ← freshReg
    emit (.load loadDst (.reg gepDst ty) ty)")
MUT_NEW+=("    emit (.gep gepDst aVal [iVal] .int)
    let loadDst ← freshReg
    emit (.load loadDst (.reg gepDst .int) .int)")
MUT_DESC+=("Lower: arrayIndex GEP uses .int")

# 16. EmitSSA: isReprCStruct always false
MUT_FILE+=("Concrete/EmitSSA.lean")
MUT_OLD+=("private def isReprCStruct (s : EmitSSAState) : Ty → Bool
  | .named name => (Layout.lookupStruct (layoutCtxOf s) name).any (·.isReprC)
  | _ => false")
MUT_NEW+=("private def isReprCStruct (_s : EmitSSAState) : Ty → Bool
  | _ => false")
MUT_DESC+=("EmitSSA: isReprCStruct always false")

# 17. SSAVerify: disable aggregate phi check
MUT_FILE+=("Concrete/SSAVerify.lean")
MUT_OLD+=("      let ctx := if isAggregateType ty then
        addSSAError ctx (.aggregatePhi b.label dst (reprStr ty))
      else ctx")
MUT_NEW+=("      let ctx := if false then -- MUTATION: agg phi disabled
        addSSAError ctx (.aggregatePhi b.label dst (reprStr ty))
      else ctx")
MUT_DESC+=("SSAVerify: disable aggregate phi check")

# 18. SSAVerify: disable phi missing-predecessor check
MUT_FILE+=("Concrete/SSAVerify.lean")
MUT_OLD+=("        if phiLabels.contains p then ctx
        else addSSAError ctx (.phiMissingPredecessor b.label p)")
MUT_NEW+=("        if phiLabels.contains p then ctx
        else ctx -- MUTATION: phi pred check disabled")
MUT_DESC+=("SSAVerify: disable phi predecessor check")

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
      echo "Usage: bash test_mutation.sh [--list] [--mutation N]"
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
    if bash run_tests.sh --fast > /tmp/mutation_test.log 2>&1; then
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
