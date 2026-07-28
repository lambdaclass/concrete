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
# Optional per-mutation gate. The fast suite is the default killer, but a defect
# whose guard is a `check_*.sh` gate would SURVIVE that suite and be reported as
# a test gap — the harness would be measuring the wrong thing and saying so
# confidently. Naming the gate turns "this gate is load-bearing" into a claim
# the harness actually checks. Set it with `gate_for_last`, which indexes off
# the current array length so it cannot drift out of alignment.
MUT_GATE=()
gate_for_last() { MUT_GATE[$(( ${#MUT_FILE[@]} - 1 ))]="$1"; }

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

# 25. DCE: unary ops are always removable again (R-0005 / bug 053)
# Restores the pre-fix state — `.unaryOp` falls through to the catch-all meaning
# "harmless" — so a discarded checked negation at MIN is deleted and the trap
# silently disappears. The roadmap asks for a mutation omitting a trapping unary
# constructor; this is that, and it must be caught by the differential (compiled
# vs interpreter), not merely by a value check.
MUT_FILE+=("Concrete/IR/SSACleanup.lean")
MUT_OLD+=("  | .unaryOp _ op operand ty =>
    if !(IntArith.unaryOpCanTrap op ty) then false")
MUT_NEW+=("  | .unaryOp _ op operand ty =>
    if true then false -- MUTATION: unary trap inventory ignored")
MUT_DESC+=("DCE: discarded trapping unary ops removable again (bug 053)")
gate_for_last "scripts/tests/check_trap_inventory.sh"

# 26. IntArith: the trap inventory's answer is inverted (R-0005 / bug 053)
# Mutates the SINGLE SOURCE rather than a consumer. If the centralisation is
# real, poisoning the inventory must break behaviour in the consumers — a
# consumer that still passes is deriving the answer locally, and the
# single-source claim is false.
#
# Inverted rather than constant-`false` on purpose: `| .neg => false` leaves
# `ty` unused, so Lean's linter rejects the file and the harness reports
# "KILLED (build)" — a kill that says nothing about whether any test can see
# the semantics. A mutation killed by the wrong mechanism is a mutation that
# never ran.
MUT_FILE+=("Concrete/Semantics/IntArith.lean")
MUT_OLD+=("  | .neg => isIntTy ty
  | .bitnot | .not_ => false")
MUT_NEW+=("  | .neg => !(isIntTy ty) -- MUTATION: inventory answer inverted
  | .bitnot | .not_ => false")
MUT_DESC+=("IntArith: unary trap inventory inverted (bug 053)")
gate_for_last "scripts/tests/check_trap_inventory.sh"

# 27. IntArith: checked negation wraps instead of trapping (R-0005 / bug 053)
# The other half of the inventory: `unaryOpCanTrap` says WHETHER, this says
# WHAT. Wrapping at MIN is the plausible-looking wrong answer, and it must be
# visible on the interpreter path — the differential is what makes a silent
# semantic drift observable rather than merely a missing abort.
MUT_FILE+=("Concrete/Semantics/IntArith.lean")
MUT_OLD+=("    | none   => .trap \"arithmetic overflow (checked negation)\"")
MUT_NEW+=("    | none   => .value (maskWidth ty (-n)) ty -- MUTATION: wrap, do not trap")
MUT_DESC+=("IntArith: checked negation wraps at MIN instead of trapping (bug 053)")
gate_for_last "scripts/tests/check_trap_inventory.sh"

# 28. SSACleanup: the indirect call target is not substituted (R-0436 / bug 056)
# Restores the pre-fix behaviour at the exact spot: the callee operand is left
# alone while every other operand is rewritten. Folding a fn-pointer phi then
# leaves a call through a value nothing defines.
MUT_FILE+=("Concrete/IR/SSACleanup.lean")
MUT_OLD+=("      | .indirect target => .indirect (r target)")
MUT_NEW+=("      | .indirect target => .indirect target -- MUTATION: callee not substituted")
MUT_DESC+=("SSACleanup: indirect call target escapes substitution (bug 056)")
gate_for_last "scripts/tests/check_fnptr_values.sh"

# 29. SSAVerify: the indirect call target is not a use (R-0436 / bug 056)
# This is the state the String callee forced — the verifier could not see a call
# target at all, so a call through an undefined register passed verification and
# was caught by llvm-as instead. DCE may also delete the producing instruction.
MUT_FILE+=("Concrete/IR/SSAVerify.lean")
MUT_OLD+=("    | .indirect target => svalRegs target ++ argRegs")
MUT_NEW+=("    | .indirect _ => argRegs -- MUTATION: callee is not a use")
MUT_DESC+=("SSAVerify: indirect call target invisible to use-checking (bug 056)")
gate_for_last "scripts/tests/check_fnptr_values.sh"

# 30. EmitSSA: a function reference stops resolving to a global (R-0436)
# Devirtualization is decided HERE, not in Lower: `.indirect (.fnRef f)` and
# `.direct f` both reach `svalToOperand` and both emit `call @f`, so removing
# Lower's `.direct` conversion leaves the emitted IR byte-identical (measured)
# and is NOT a behaviour change worth mutating. Lower's conversion still matters
# for passes that key on a direct callee — `checkCallArity` only validates
# those — but it cannot be what the direct-call assertions detect.
# Making `.fnRef` emit a register instead is the real inverse: correctness legs
# keep passing (the call still reaches the right function via a load) while the
# common case silently becomes an indirect call.
MUT_FILE+=("Concrete/Backend/EmitSSA.lean")
MUT_OLD+=("    .global resolved")
MUT_NEW+=("    .reg resolved -- MUTATION: fn reference emitted as a register")
MUT_DESC+=("EmitSSA: fn reference no longer resolves to a global (R-0436)")
gate_for_last "scripts/tests/check_fnptr_values.sh"

# 31. ProofCore: an applied parameter extracts as a definition call (R-0442 / 061)
# Restores bug 061 exactly: `.applyVar` collapses back into `.call`, so a
# parameter named `f` and a definition named `f` become the same node and the
# evaluator resolves the parameter through the global function table.
MUT_FILE+=("Concrete/Proof/ProofCore.lean")
MUT_OLD+=("    some (.applyVar binding pargs)")
MUT_NEW+=("    some (.call binding pargs) -- MUTATION: parameter application as a definition call")
MUT_DESC+=("ProofCore: applied parameter extracts as a global call (bug 061)")
gate_for_last "scripts/tests/check_proofcore_callable_identity.sh"

# 32. Proof: an applied local resolves in the GLOBAL namespace (R-0442 / 061)
# The other half. Extraction stays correct but `eval` looks the binding up among
# definitions, so a global `f` answers an application of a parameter `f` — the
# soundness hazard, in the evaluator rather than the extractor.
MUT_FILE+=("Concrete/Proof/Proof.lean")
MUT_OLD+=("    match fns.callables binding with")
MUT_NEW+=("    match fns.globals binding with -- MUTATION: local resolved as a global")
MUT_DESC+=("Proof: eval resolves an applied local through globals (bug 061)")
gate_for_last "scripts/tests/check_proofcore_callable_identity.sh"

# 33. Proof: the representative callback goes back into the global namespace
# The state R-0442 found: the HOF specs' callback bound as a DEFINITION.
# Measured outcome, better than expected: this and #32 are killed by the Lean
# KERNEL, not by the gate — the three map theorems reduce to `⊢ False` because
# `.applyVar f` is stuck when `f` lives in the wrong namespace. So the proofs
# themselves are load-bearing evidence for the separation; the gate's structural
# assertions are a second, independent line rather than the only one.
# (A `KILLED (build)` is weak when a LINTER rejects the file; it is the strongest
# possible signal when the kernel rejects the theorem.)
MUT_FILE+=("Concrete/Proof/Proof.lean")
MUT_OLD+=("  FnTable.withCallables (fun _ => none) pureCoreCallables")
MUT_NEW+=("  FnTable.withCallables pureCoreCallables (fun _ => none) -- MUTATION: callback as a global")
MUT_DESC+=("Proof: representative callback bound as a global (bug 061)")
gate_for_last "scripts/tests/check_proofcore_callable_identity.sh"

# 34. ProofCore: dependency containment removed entirely (R-0004 slice 3 / 062)
# Restores the pre-slice-3 state: notCurrentDeps is still computed and recorded,
# and still has no effect on the status — which is exactly what bug 062 was.
MUT_FILE+=("Concrete/Proof/ProofCore.lean")
MUT_OLD+=("    | some .proved => if (notCurrentOf n).isEmpty then .proved else .depsNotCurrent")
MUT_NEW+=("    | some .proved => .proved -- MUTATION: containment has no effect")
MUT_DESC+=("ProofCore: a non-current dependency no longer downgrades (bug 062)")
gate_for_last "scripts/tests/check_proof_freshness.sh"

# 35. ProofCore: containment stops at ONE hop (R-0004 slice 3 / 062)
# The subtler half. The direct dependent is still contained, so a gate that only
# checked one hop would pass; only the two-hop leg can see this.
MUT_FILE+=("Concrete/Proof/ProofCore.lean")
# Mutated INSIDE the walk so every binding stays used: replacing the call site
# left `reachableFrom` unused and Lean's linter rejected the file, which is a
# kill that says nothing about whether a test can see one-hop-only behaviour.
MUT_OLD+=("        else go fuel (rest ++ directCalleesOf n) (n :: seen)")
MUT_NEW+=("        else go fuel rest (n :: seen) -- MUTATION: frontier never expands (one hop)")
MUT_DESC+=("ProofCore: containment does not traverse the closure (bug 062 transitive)")
gate_for_last "scripts/tests/check_proof_freshness.sh"

# 36. ProofCore: a stale dependency counts as current (R-0004 slice 3)
# Mutates the single-source policy rather than a consumer. If the policy really
# is single-source, poisoning it must break containment everywhere at once.
MUT_FILE+=("Concrete/Proof/ProofCore.lean")
# Both arms replaced together: adding `.stale` to the first line alone leaves it
# overlapping the second, which Lean rejects structurally rather than any test
# catching the semantics.
MUT_OLD+=("  | .proved | .trusted => true
  | .stale | .missing | .blocked | .ineligible | .unbound | .depsNotCurrent => false")
MUT_NEW+=("  | .proved | .trusted | .stale => true -- MUTATION: stale counts as current
  | .missing | .blocked | .ineligible | .unbound | .depsNotCurrent => false")
MUT_DESC+=("ProofCore: trap inventory of dependency currency admits stale (slice 3)")
gate_for_last "scripts/tests/check_proof_freshness.sh"

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
    local gate="${MUT_GATE[$idx]:-}"
    if ! bash scripts/tests/run_tests.sh --fast > /tmp/mutation_test.log 2>&1; then
      result="KILLED"
      KILLED=$((KILLED + 1))
    elif [[ -n "$gate" ]] && ! bash "$gate" > /tmp/mutation_gate.log 2>&1; then
      result="KILLED (gate)"
      KILLED=$((KILLED + 1))
    else
      result="SURVIVED"
      SURVIVED=$((SURVIVED + 1))
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
