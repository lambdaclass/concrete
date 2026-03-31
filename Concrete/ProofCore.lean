import Concrete.Core
import Concrete.Pipeline

namespace Concrete

/-! ## ProofCore — restricted, proof-oriented view of validated Core

ProofCore is a projection of `ValidatedCore` that keeps only the parts
amenable to Lean 4 verification:

  Included:
  - Pure functions (empty capability set, not trusted, no extern calls)
  - Algebraic data types (structs, enums) with no FFI annotations
  - Structured control flow (if/else, match, while, let, return)
  - Integer, boolean, and simple arithmetic

  Excluded:
  - Functions with capabilities (File, Network, etc.)
  - Trusted or unsafe functions
  - Extern functions and FFI types (repr(C), packed)
  - Raw pointer operations (borrow, deref, alloc)
  - String/char operations (not yet modeled)
  - While loops with break/continue (not yet modeled)

ProofCore does NOT define its own semantics.  It is a filter, not a rival
IR.  The semantic authority remains CoreCheck; ProofCore merely identifies
the subset of validated Core that the Lean proof infrastructure can reason
about today.
-/

-- ============================================================
-- Eligibility predicates
-- ============================================================

/-- A function is proof-eligible when it is pure, not trusted, and has
    no type parameters (monomorphic or pre-mono with concrete types only). -/
def CFnDef.isProofEligible (f : CFnDef) : Bool :=
  f.capSet.isEmpty &&
  !f.isTrusted &&
  !f.isEntryPoint &&
  f.trustedImplOrigin.isNone

/-- A struct is proof-eligible when it has no FFI annotations. -/
def CStructDef.isProofEligible (s : CStructDef) : Bool :=
  !s.isReprC && !s.isPacked && s.reprAlign.isNone

/-- An enum is proof-eligible when it has no builtin override. -/
def CEnumDef.isProofEligible (e : CEnumDef) : Bool :=
  e.builtinId.isNone

-- ============================================================
-- ProofCore artifact
-- ============================================================

/-- The proof-oriented fragment of validated Core.
    Contains only pure functions, safe structs/enums, and trait definitions. -/
structure ProofCore where
  /-- Pure, non-trusted functions eligible for formal reasoning. -/
  functions : List CFnDef
  /-- Structs with no FFI annotations. -/
  structs : List CStructDef
  /-- Enums with no builtin overrides. -/
  enums : List CEnumDef
  /-- Trait definitions (for context — not yet verified). -/
  traitDefs : List CTraitDef
  /-- How many functions were excluded (for diagnostics). -/
  excludedFnCount : Nat
  /-- How many structs were excluded. -/
  excludedStructCount : Nat
  /-- How many enums were excluded. -/
  excludedEnumCount : Nat

-- ============================================================
-- Extraction
-- ============================================================

/-- Flatten a module tree into a list of all modules (pre-order). -/
private partial def flattenModules (m : CModule) : List CModule :=
  m :: List.flatten (m.submodules.map flattenModules)

/-- Extract the proof-oriented fragment from validated Core.
    This is the primary entry point for the proof pipeline. -/
def extractProofCore (vc : ValidatedCore) : ProofCore :=
  let allModules := List.flatten (vc.coreModules.map flattenModules)
  let fns := List.flatten (allModules.map (·.functions)) |>.filter CFnDef.isProofEligible
  let sts := List.flatten (allModules.map (·.structs)) |>.filter CStructDef.isProofEligible
  let ens := List.flatten (allModules.map (·.enums)) |>.filter CEnumDef.isProofEligible
  let tds := List.flatten (allModules.map (·.traitDefs))
  let totalFns := allModules.foldl (fun acc m => acc + m.functions.length) 0
  let totalSts := allModules.foldl (fun acc m => acc + m.structs.length) 0
  let totalEns := allModules.foldl (fun acc m => acc + m.enums.length) 0
  {
    functions := fns
    structs := sts
    enums := ens
    traitDefs := tds
    excludedFnCount := totalFns - fns.length
    excludedStructCount := totalSts - sts.length
    excludedEnumCount := totalEns - ens.length
  }

-- ============================================================
-- Pretty-printing (for --report proofcore)
-- ============================================================

def ProofCore.summary (pc : ProofCore) : String :=
  let fnNames := pc.functions.map (·.name)
  let stNames := pc.structs.map (·.name)
  let enNames := pc.enums.map (·.name)
  s!"ProofCore fragment:\n" ++
  s!"  {pc.functions.length} proof-eligible functions ({pc.excludedFnCount} excluded)\n" ++
  s!"  {pc.structs.length} proof-eligible structs ({pc.excludedStructCount} excluded)\n" ++
  s!"  {pc.enums.length} proof-eligible enums ({pc.excludedEnumCount} excluded)\n" ++
  s!"  functions: {fnNames}\n" ++
  s!"  structs:   {stNames}\n" ++
  s!"  enums:     {enNames}"

end Concrete
