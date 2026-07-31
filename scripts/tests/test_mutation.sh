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
#   bash scripts/tests/test_mutation.sh --check-patterns  # assert none would SKIP
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

# EXCLUSIVE LOCK. This harness mutates source IN PLACE and restores from
# `<file>.mutbak`. Two concurrent runs in one worktree therefore clobber each
# other's backups: on 2026-07-30 a second run overwrote the first's .mutbak,
# leaving `keys.length == keys.length -- MUTATION` committed-adjacent in
# Concrete/Proof/Proof.lean, one run reporting "mv: cannot stat …mutbak" and
# another "SKIPPED (pattern not found)". Test machinery that can corrupt the tree
# it is testing must refuse to run twice, not rely on the operator remembering.
# KNOWN LIMIT, measured 2026-07-30: a TERM/INT arriving while a FOREGROUND CHILD
# runs (`lake build`, which dominates each mutation) does not restore promptly —
# bash defers the trap until the child exits, so the tree stays mutated for the
# rest of that build. Traps cannot fix this; only not touching the developer's
# tree can. The real answer is to run mutations in a DISPOSABLE WORKTREE, which is
# tracked and not yet built. Until then: the lock stops concurrent corruption, the
# hash postcondition refuses to exit quietly on an inexact restore, and a killed
# run must be followed by
#   grep -rn -- "-- MUTATION" Concrete && git checkout -- <files>
LOCK_DIR="$ROOT_DIR/.mutation.lock"
if ! mkdir "$LOCK_DIR" 2>/dev/null; then
  echo "error: another mutation run holds $LOCK_DIR" >&2
  echo "       this harness edits source in place; concurrent runs corrupt it." >&2
  echo "       if no run is active, remove the directory and check for stray" >&2
  echo "       '-- MUTATION' markers: grep -rn 'MUTATION' Concrete/ --include='*.lean'" >&2
  exit 2
fi
# Backups live in a unique temp dir, not beside the source: a `<file>.mutbak`
# sitting in the tree is itself a way to leave state behind, and two runs racing
# on the same path is what corrupted Proof.lean.
MUT_BACKUP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/concrete-mutation.XXXXXX")"

# Hashes of every target file, captured BEFORE any mutation. Restoration is
# verified against these, so "restored" means byte-identical rather than "the
# restore command ran".
declare -A MUT_HASH0=()
# Hash of the MUTATED content this harness wrote, per file. The difference
# between this and what is on disk at restore time is a THIRD PARTY's edit.
declare -A MUT_HASH_APPLIED=()
# Set when a restore was REFUSED because another writer owned the file. The run
# must then fail: a refused restore leaves the tree in a state nobody chose.
MUT_CONCURRENT=0
hash_of() { shasum -a 256 "$1" 2>/dev/null | cut -d' ' -f1; }

cleanup_lock() {
  local rc=$?
  # The lock is released at the END, after restoration. Releasing it first let a
  # second run start while this one still had a mutation applied — the exact race
  # the lock exists to prevent.
  # POSTCONDITION, not a warning. A mutation left applied is worse than a failed
  # run: the next build, gate or commit silently uses it, and a green result then
  # describes mutated code. This must make the harness FAIL.
  local bad=0
  # RESTORE FIRST, then verify. Detecting a stray mutation and exiting leaves the
  # tree modified, which is the failure this trap exists to prevent — a signalled
  # run must not be able to leave the developer's source semantically changed.
  # Backups live in $MUT_BACKUP_DIR, so this works even on INT/TERM mid-mutation.
  if [ -d "$MUT_BACKUP_DIR" ]; then
    # The backup dir mirrors the repo layout, so the relative path IS the target —
    # no guessing, no flattening to invert.
    while IFS= read -r bak; do
      local rel="${bak#$MUT_BACKUP_DIR/}"
      # Skip rescued copies of a third party's work — they are evidence, not backups.
      case "$rel" in CONCURRENT-EDIT/*) continue ;; esac
      if [ -f "$ROOT_DIR/$rel" ]; then
        # Same non-clobber rule as restore_mutation: on INT/TERM we must not
        # "restore" over an edit that was never ours.
        local now applied
        now="$(hash_of "$ROOT_DIR/$rel")"
        applied="${MUT_HASH_APPLIED[$rel]:-}"
        if [ -n "$applied" ] && [ "$now" != "$applied" ] && [ "$now" != "${MUT_HASH0[$rel]:-}" ]; then
          mkdir -p "$(dirname "$MUT_BACKUP_DIR/CONCURRENT-EDIT/$rel")"
          cp "$ROOT_DIR/$rel" "$MUT_BACKUP_DIR/CONCURRENT-EDIT/$rel"
          echo "  REFUSED to restore $rel — changed by another writer; theirs kept" >&2
          bad=1
          continue
        fi
        cp "$bak" "$ROOT_DIR/$rel"
        echo "  restored $rel from backup" >&2
      fi
    done < <(find "$MUT_BACKUP_DIR" -type f 2>/dev/null)
  fi
  if [ "$MUT_CONCURRENT" != 0 ]; then
    echo "" >&2
    echo "FATAL: a restore was refused because another writer held a target file." >&2
    echo "       The tree is in a state neither party chose — reconcile by hand." >&2
    bad=1
  fi
  local stray
  stray="$(grep -rn -- "-- MUTATION" "$ROOT_DIR"/Concrete "$ROOT_DIR"/std 2>/dev/null || true)"
  if [ -n "$stray" ]; then
    echo "" >&2
    echo "FATAL: a mutation survived restoration:" >&2
    printf '%s\n' "$stray" >&2
    bad=1
  fi
  # Exact restoration, per target file.
  for f in "${!MUT_HASH0[@]}"; do
    local now; now="$(hash_of "$ROOT_DIR/$f")"
    if [ "$now" != "${MUT_HASH0[$f]}" ]; then
      echo "" >&2
      echo "FATAL: $f was not restored exactly" >&2
      echo "  before: ${MUT_HASH0[$f]}" >&2
      echo "  after : $now" >&2
      bad=1
    fi
  done
  # KEEP the backup dir when a concurrent edit was refused. It holds both the
  # rescued foreign version and our original, which is exactly what reconciling
  # needs — and the refusal message names that path. A first version of this fix
  # printed the path and then deleted it two lines later.
  if [ "$MUT_CONCURRENT" != 0 ]; then
    echo "" >&2
    echo "  PRESERVED for reconciliation: $MUT_BACKUP_DIR" >&2
    echo "    CONCURRENT-EDIT/<path>  the other writer's version" >&2
    echo "    <path>                  the original this harness backed up" >&2
  else
    rm -rf "$MUT_BACKUP_DIR" 2>/dev/null || true
  fi
  rmdir "$LOCK_DIR" 2>/dev/null || true
  if [ "$bad" = 1 ]; then
    echo "" >&2
    echo "restore the tree before building, gating or committing:" >&2
    echo "  git diff -- Concrete std   # then git checkout -- <files>" >&2
    exit 3
  fi
  exit $rc
}
trap cleanup_lock EXIT INT TERM

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
  | .i16 | .u16 => 2
  | .i8 | .u8 | .char | .bool => 1
  | .unit => 0")
MUT_NEW+=("  | .i32 | .u32 | .float32 => 8
  | .i16 | .u16 => 2
  | .i8 | .u8 | .char | .bool => 1
  | .unit => 0")
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
MUT_OLD+=("        if !info.isCopy && info.state == .consumed then
          -- secondary span: where the value was moved (Phase 4 #11).")
MUT_NEW+=("        if false then -- MUTATION: use-after-move disabled
          -- secondary span: where the value was moved (Phase 4 #11).")
MUT_DESC+=("Check: disable use-after-move")

# 10. Check: disable loop-depth linearity check (enforcement lives in CheckHelpers)
MUT_FILE+=("Concrete/Check/CheckHelpers.lean")
MUT_OLD+=("      if info.loopDepth + breakDepthExempt < env.loopDepth && !env.inFnExitingBranch
          && env.rebindingVar != some name then
        throwCheck (.cannotConsumeLinearInLoop name) span")
MUT_NEW+=("      if false && (info.loopDepth + breakDepthExempt < env.loopDepth && !env.inFnExitingBranch
          && env.rebindingVar != some name) then -- MUTATION: loop-depth disabled
        throwCheck (.cannotConsumeLinearInLoop name) span")
MUT_DESC+=("Check: disable loop-depth linearity")

# 11. Check: disable scope-exit unconsumed check (enforcement lives in CheckHelpers)
MUT_FILE+=("Concrete/Check/CheckHelpers.lean")
MUT_OLD+=("      if !info.isCopy && info.state != .consumed && info.state != .reserved then")
MUT_NEW+=("      if false then -- MUTATION: scope check disabled")
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
MUT_OLD+=("      if !capD.satisfied then
        addCCError (.insufficientCapabilities fn (capSetToString capD.required) (capSetToString capD.callerHas))")
MUT_NEW+=("      if false then -- MUTATION: capability check disabled
        addCCError (.insufficientCapabilities fn (capSetToString capD.required) (capSetToString capD.callerHas))")
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
MUT_OLD+=("    some (.applyVar binding pargs)
  | .structLit name _ fields _ => do")
MUT_NEW+=("    some (.call binding pargs) -- MUTATION: parameter application as a definition call
  | .structLit name _ fields _ => do")
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

# 37. FnTable: entry order leaks into the root (R-0004 step 3)
# Canonical ordering is what makes the root a function of CONTENT. Sorting by
# insertion order instead makes two identical tables hash differently, so a
# receipt would depend on how the generator happened to emit entries.
MUT_FILE+=("Concrete/Proof/Proof.lean")
# Retargeted: the root no longer sorts (qsort does not kernel-reduce), so
# canonical order is now an ASSERTED property. Accepting any order is the defect.
MUT_OLD+=("  (keys.zip (keys.drop 1)).all fun (a, b) => a < b")
MUT_NEW+=("  (keys.zip (keys.drop 1)).all fun (a, b) => a <= b -- MUTATION: order not strict")
MUT_DESC+=("FnTable: entry order only non-decreasing, not strict (R-0004 step 3)")
gate_for_last "scripts/tests/check_callable_identity.sh"

# 38. FnTable: duplicate identities accepted (R-0004 step 3)
# Two entries claiming one identity means the table disagrees with itself.
# Accepting it makes lookup arbitrary and the root insertion-order dependent.
MUT_FILE+=("Concrete/Proof/Proof.lean")
MUT_OLD+=("  keys.length != keys.eraseDups.length")
MUT_NEW+=("  keys.length != keys.length -- MUTATION: duplicate identities accepted")
MUT_DESC+=("FnTable: duplicate CallableIds no longer rejected (R-0004 step 3)")
gate_for_last "scripts/tests/check_callable_identity.sh"

# 39. FnTable: the body/params are dropped from the root (R-0004 step 3)
# A root over identities alone cannot detect an altered body — the caller would
# keep a `current` verdict across a real semantic change, which is the whole
# class R-0004 exists to close.
MUT_FILE+=("Concrete/Proof/Proof.lean")
# The INNER per-param prefix is what makes the param list injective. Removing the
# outer one only changes formatting: each param is already self-delimiting, so
# two distinct tables still get distinct roots and there is nothing to catch — a
# first draft of this mutation SURVIVED for exactly that reason, correctly.
# Dropping the inner prefix is the real defect: ["a","b"] and ["a,b"] both render
# "a,b", so two different signatures collide on one root.
MUT_OLD+=("      let ps := String.intercalate \",\" (d.params.map fun p => lp \"p\" p)")
MUT_NEW+=("      let ps := String.intercalate \",\" d.params -- MUTATION: params not self-delimiting")
MUT_DESC+=("FnTable: param list not injective in the root (step 3)")
gate_for_last "scripts/tests/check_callable_identity.sh"

# 40. FnTable: the operational key index leaves the root (R-0004 step 3)
# Calls still select entries by STRING. Dropping the key index from the root
# means a receipt does not commit to the name->identity mapping it was produced
# under, so renaming a displayName silently keeps the old root.
MUT_FILE+=("Concrete/Proof/Proof.lean")
MUT_OLD+=("    some (s!\"tblv{t.schemaVersion}:\" ++ lp \"E\" (String.join parts) ++ lp \"K\" (String.join idx))")
MUT_NEW+=("    some (s!\"tblv{t.schemaVersion}:\" ++ lp \"E\" (String.join parts) ++ lp \"K\" (String.join (idx.take 0))) -- MUTATION: key index dropped")
MUT_DESC+=("FnTable: root omits the string-key index (R-0004 step 3)")
gate_for_last "scripts/tests/check_callable_identity.sh"

# 41. FnTable: one key reaching two entries is allowed (R-0004 step 3)
# While calls select by name, an ambiguous key means a call picks arbitrarily.
MUT_FILE+=("Concrete/Proof/Proof.lean")
MUT_OLD+=("  keys.length == keys.eraseDups.length")
MUT_NEW+=("  keys.length == keys.length -- MUTATION: ambiguous keys accepted")
MUT_DESC+=("FnTable: ambiguous string-key index accepted (R-0004 step 3)")
gate_for_last "scripts/tests/check_callable_identity.sh"

# 42. lookupById matches on the display NAME (R-0004 step 3)
# The mismatched-lookup-key case: resolving by name rather than identity is
# exactly the keyed identity the finite table exists to remove, and it makes a
# same-named callable in another module answer for this one.
MUT_FILE+=("Concrete/Proof/Proof.lean")
MUT_OLD+=("  (t.entries.find? fun d => d.identityKey == id.render).bind PFnDef.identified?")
MUT_NEW+=("  (t.entries.find? fun d => d.displayName == id.declName).bind PFnDef.identified? -- MUTATION: lookup by name")
MUT_DESC+=("FnTable: lookupById resolves by display name, not identity (step 3)")
gate_for_last "scripts/tests/check_callable_identity.sh"

# 43. FnTable: the root stops binding entry BODIES (R-0004 step 3)
# The original defect, kept as a permanent mutation: with bodies unbound, two
# tables with identical identities and parameters but different bodies had EQUAL
# roots while evaluating to 1 and 999. A root blind to behaviour cannot back a
# receipt, and the nine-table migration would have moved proofs onto it.
MUT_FILE+=("Concrete/Proof/Proof.lean")
MUT_OLD+=("      lp \"i\" d.identityKey ++ lp \"P\" ps ++ lp \"B\" (pexprCanonical d.body) ++ lp \"S\" sd")
MUT_NEW+=("      lp \"i\" d.identityKey ++ lp \"P\" ps ++ lp \"S\" sd -- MUTATION: root blind to bodies")
MUT_DESC+=("FnTable: root omits entry bodies (R-0004 step 3)")
gate_for_last "scripts/tests/check_callable_identity.sh"

# 44. FnTable: a type-erased generic identity is accepted as complete
# One entry standing in for every monomorphization, when the monomorphizations
# disagree: extracted arithmetic is width-free, so a kernel-true proof over `Int`
# is FALSE of an `i8` instance where 100 + 100 wraps. This is the fail-closed
# direction, so removing the check must not be silent.
MUT_FILE+=("Concrete/Proof/Proof.lean")
MUT_OLD+=("    | some id => id.isComplete")
MUT_NEW+=("    | some id => id.isComplete || true -- MUTATION: erased generics accepted")
MUT_DESC+=("FnTable: incomplete (type-erased) identities accepted")
gate_for_last "scripts/tests/check_callable_identity.sh"

# 45. Generator: a lookup lemma for the FIRST entry only
# Missing lemmas make an entry unreachable to the kernel while the table still
# looks complete — a proof about that identity cannot be used, and nothing says
# so. The incorrect/missing-lemma class.
MUT_FILE+=("Concrete/Report/Report.lean")
MUT_OLD+=("  let lookupLemmas := extracted.map fun e =>")
MUT_NEW+=("  let lookupLemmas := (extracted.take 1).map fun e => -- MUTATION: one lemma only")
MUT_DESC+=("Generator: lookup lemma emitted for one entry only")
gate_for_last "scripts/tests/check_callable_identity.sh"

# 46. Generator: an entry is dropped from the table but keeps its lemma
# The entry-deletion class. The table shrinks while the lemmas still claim the
# missing entry is reachable, so `rfl` on that lookup no longer holds.
MUT_FILE+=("Concrete/Report/Report.lean")
MUT_OLD+=("\", \".intercalate entryNames}]")
MUT_NEW+=("\", \".intercalate entryNames.dropLast}]")
MUT_DESC+=("Generator: an entry is dropped but keeps its lookup lemma")
gate_for_last "scripts/tests/check_callable_identity.sh"

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
    --check-patterns)
      MODE="check-patterns"
      shift
      ;;
    *)
      echo "Unknown option: $1"
      echo "Usage: bash scripts/tests/test_mutation.sh [--list] [--mutation N]"
      exit 1
      ;;
  esac
done

# ============================================================
# Pattern-freshness mode
# ============================================================
# A mutation whose MUT_OLD no longer occurs in its file reports
# "SKIPPED (pattern not found in file)" — it stops testing anything while still
# LOOKING like part of the suite. Refactoring the compiler silently retires
# mutations this way, and a suite that has quietly stopped covering a property
# is worse than a missing one, because the summary line still counts it.
#
# This iterates the REAL arrays the harness applies, so it cannot drift from
# them the way a separate parser of this file would (a re-parsing check
# mis-handled multi-line MUT_OLD entries and reported 17 false stalenesses).
# It touches no files and runs in about a second, so it is a cheap gate.
if [[ "$MODE" == "check-patterns" ]]; then
  echo "=== Mutation pattern freshness ($NUM_MUTATIONS mutations) ==="
  stale=0
  for (( i=0; i<NUM_MUTATIONS; i++ )); do
    f="${MUT_FILE[$i]}"
    if [[ ! -f "$f" ]]; then
      printf "  STALE  [%2d] missing file %s\n" "$((i+1))" "$f"
      stale=$((stale + 1)); continue
    fi
    # Count exact literal occurrences; awk avoids regex interpretation of the
    # pattern (these contain ., |, =>, ( and would misbehave under grep).
    n=$(awk -v pat="${MUT_OLD[$i]}" '
      BEGIN { RS="\0"; n=0 }
      { s=$0; l=length(pat); if (l==0) { print 0; exit }
        p=1
        while ((k=index(substr(s,p),pat)) > 0) { n++; p=p+k+l-1 }
        print n }' "$f")
    if [[ "$n" != "1" ]]; then
      printf "  STALE  [%2d] %s occurs %s time(s) in %s\n      %s\n" \
        "$((i+1))" "MUT_OLD" "$n" "$f" "${MUT_DESC[$i]}"
      stale=$((stale + 1))
    fi
  done
  if [[ $stale -gt 0 ]]; then
    echo ""
    echo "FAIL: $stale mutation(s) would SKIP rather than test."
    echo "A mutation must match its target EXACTLY ONCE: zero means the code moved,"
    echo "more than one means the harness would patch an unintended site too."
    exit 1
  fi
  echo "PASS: all $NUM_MUTATIONS mutation patterns match their target exactly once"
  exit 0
fi

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

  # Backup into the unique temp dir, NOT beside the source. A `<file>.mutbak` in
  # the tree is itself state left behind — one was staged into a commit before
  # being caught — and two runs racing on that path is what corrupted
  # Proof.lean. The key flattens the path so nested files cannot collide.
  # Mirror the path INSIDE the backup dir rather than flattening it. `tr / _`
  # collides (`a/b_c` and `a_b/c` both become `a_b_c`) and is not invertible, so
  # the restore loop had to guess which file a backup belonged to.
  mkdir -p "$MUT_BACKUP_DIR/$(dirname "$file")"
  cp "$file" "$MUT_BACKUP_DIR/$file"

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
  local rc=$?
  # Record the EXACT content this harness wrote. Restoration compares against it,
  # so "the file changed" can be told apart from "we changed the file". Without
  # this, a concurrent editor's save is indistinguishable from our own mutation
  # and gets silently overwritten by the restore below.
  [ "$rc" -eq 0 ] && MUT_HASH_APPLIED["$file"]="$(hash_of "$file")"
  return $rc
}

restore_mutation() {
  local idx=$1
  local file="${MUT_FILE[$idx]}"
  local bak="$MUT_BACKUP_DIR/$file"
  if [ -f "$bak" ]; then
    # DO NOT clobber someone else's work. If the file on disk is neither what we
    # wrote nor the original, a concurrent writer edited it while the mutation was
    # applied, and copying the backup over it DESTROYS that edit — then the
    # postcondition check compares against the PRE-mutation hash and reports
    # "restored exactly", confirming the loss. That happened: an edit to
    # ProofCore.lean vanished mid-session and the file dropped out of git status.
    local now; now="$(hash_of "$file")"
    local applied="${MUT_HASH_APPLIED[$file]:-}"
    if [ -n "$applied" ] && [ "$now" != "$applied" ] && [ "$now" != "${MUT_HASH0[$file]:-}" ]; then
      local rescue="$MUT_BACKUP_DIR/CONCURRENT-EDIT/$file"
      mkdir -p "$(dirname "$rescue")"; cp "$file" "$rescue"
      echo "" >&2
      echo "  FATAL: $file changed while a mutation was applied." >&2
      echo "         On-disk content is neither our mutation nor the original, so" >&2
      echo "         another writer edited it. Refusing to overwrite." >&2
      echo "         Their version: $rescue" >&2
      echo "         Our backup   : $bak" >&2
      echo "         Reconcile by hand. This harness needs exclusive use of the" >&2
      echo "         worktree; use a separate one (scripts/worktree-new.sh)." >&2
      MUT_CONCURRENT=1
      return 1
    fi
    cp "$bak" "$file"
    rm -f "$bak"
  else
    echo "  WARNING: no backup for $file — cannot restore" >&2
  fi
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
    # Restore if a backup was created. This guard tested `<file>.mutbak` for a
    # while after backups MOVED to $MUT_BACKUP_DIR, so it was never true and a
    # skipped mutation restored nothing — the source stayed mutated and the next
    # mutation backed up the ALREADY-MUTATED file. Test the real backup path.
    [[ -f "$MUT_BACKUP_DIR/${MUT_FILE[$idx]}" ]] && restore_mutation "$idx"
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

# Preflight: no target file may be dirty. A mutation applied on top of
# uncommitted work cannot be distinguished from that work on restore, and the
# hash postcondition would then compare against an already-modified baseline.
# Only the files this RUN will touch. Checking the whole target set would refuse
# a single-mutation run because some unrelated target happens to be dirty, which
# makes the guard obstructive rather than protective — and an obstructive guard
# gets bypassed.
if [ "$MODE" = "single" ]; then
  MUT_TARGETS=("${MUT_FILE[$((SINGLE_IDX - 1))]}")
else
  mapfile -t MUT_TARGETS < <(printf '%s\n' "${MUT_FILE[@]}" | sort -u)
fi
for f in "${MUT_TARGETS[@]}"; do
  # `--quiet` alone misses STAGED changes: a file that is `git add`-ed but not
  # committed reads as clean to `git diff`, so a mutation could be applied on top
  # of staged work and restored over it. Check the index too.
  if ! git -C "$ROOT_DIR" diff --quiet -- "$f" 2>/dev/null \
     || ! git -C "$ROOT_DIR" diff --quiet --cached -- "$f" 2>/dev/null; then
    echo "error: target file has uncommitted changes: $f" >&2
    echo "       this harness edits targets in place; commit or stash first." >&2
    exit 2
  fi
  MUT_HASH0["$f"]="$(hash_of "$ROOT_DIR/$f")"
done

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
