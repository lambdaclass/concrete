import Concrete.AST
import Concrete.Shared
import Concrete.Intrinsic

namespace Concrete

/-! ## FileSummary — the declaration-level cross-file interface artifact

`FileSummary` is the single source of cross-file information. All passes
consume signatures and type declarations from it rather than rebuilding
their own views from raw ASTs.

Built once by `buildSummaryTable`, consumed by Resolve (shallow phase), Check, and Elab.
Each module's summary is computed exactly once; submodule summaries are reused
from the parent's `submoduleSummaries` field (no redundant rebuilds).

`ResolvedImports` is the single import artifact consumed by Check and Elab.
It is built once from the summary table (via `resolveImportsFromTable`) and shared,
not rebuilt ad hoc in each pass.

Artifact flow:
  ParsedModule → FileSummary → ResolvedImports → checked/elaborated module

Interface data (safe to cache/serialize):
  functions, externFnSigs, implMethodSigs, structs, enums, publicNames,
  traits, constants, typeAliases, newtypes, imports, submoduleSummaries

Implementation data (needed by Check/Elab for whole-program body compilation):
  implBlocks, traitImpls — these carry full method bodies, not just signatures.
  Check and Elab read imported impl/trait-impl bodies through ResolvedImports
  to type-check and elaborate cross-module method implementations.

TODO: For future incremental compilation, split into interface-only and body portions
so that downstream passes can consume cached interface summaries without method bodies.
-/

structure ConstSummary where
  name : String
  ty : Ty
  isPublic : Bool := false

structure FnSummary where
  params : List (String × Ty)
  retTy : Ty
  typeParams : List String := []
  typeBounds : List (String × List String) := []
  capParams : List String := []
  capSet : CapSet := .empty
  deriving Repr

structure FileSummary where
  name : String
  functions : List (String × FnSummary)
  structs : List StructDef
  enums : List EnumDef
  implBlocks : List ImplBlock
  traitImpls : List ImplTraitBlock
  publicNames : List String
  traits : List TraitDef := []
  constants : List ConstSummary := []
  typeAliases : List TypeAlias := []
  newtypes : List NewtypeDef := []
  externFns : List ExternFnDecl := []
  externFnSigs : List (String × FnSummary) := []
  implMethodSigs : List (String × FnSummary) := []  -- Self preserved (unresolved)
  imports : List ImportDecl := []
  submoduleSummaries : List (String × FileSummary) := []

instance : Inhabited FileSummary where
  default := { name := "", functions := [], structs := [], enums := [],
                implBlocks := [], traitImpls := [], publicNames := [] }

/-- All data imported into a module, resolved from the summary table.
    Single import artifact consumed by Check and Elab. -/
structure ResolvedImports where
  functions      : List (String × FnSummary) := []
  structs        : List StructDef := []
  enums          : List EnumDef := []
  implBlocks     : List ImplBlock := []
  traitImpls     : List ImplTraitBlock := []
  implMethodSigs : List (String × FnSummary) := []  -- pre-computed, Self preserved
  typeAliases    : List (String × Ty) := []
  /-- Maps local alias name → original linker symbol for aliased imports. -/
  linkerAliases  : List (String × String) := []

private def fnDefToSummary (f : FnDef) : String × FnSummary :=
  (f.name, { params := f.params.map fun p => (p.name, p.ty)
             retTy := f.retTy
             typeParams := f.typeParams
             typeBounds := f.typeBounds
             capParams := f.capParams
             capSet := f.capSet })

/-- Extract impl method summaries from impl blocks with Self preserved (unresolved). -/
def implBlocksToMethodSummaries (blocks : List ImplBlock) : List (String × FnSummary) :=
  blocks.foldl (fun acc ib =>
    acc ++ ib.methods.map fun f =>
      (ib.typeName ++ "_" ++ f.name,
       { params := f.params.map fun p => (p.name, p.ty)
         retTy := f.retTy
         typeParams := ib.typeParams ++ f.typeParams
         typeBounds := f.typeBounds
         capParams := f.capParams
         capSet := f.capSet })
  ) []

/-- Extract impl method summaries from trait impl blocks with Self preserved (unresolved). -/
def traitImplBlocksToMethodSummaries (blocks : List ImplTraitBlock) : List (String × FnSummary) :=
  blocks.foldl (fun acc tb =>
    acc ++ tb.methods.map fun f =>
      (tb.typeName ++ "_" ++ f.name,
       { params := f.params.map fun p => (p.name, p.ty)
         retTy := f.retTy
         typeParams := tb.typeParams ++ f.typeParams
         typeBounds := f.typeBounds
         capParams := f.capParams
         capSet := f.capSet })
  ) []

/-- Resolve Self in impl method signatures using the concrete impl type from blocks. -/
def resolveImplMethodSigs
    (sigs : List (String × FnSummary))
    (implBlocks : List ImplBlock)
    (traitImpls : List ImplTraitBlock)
    : List (String × FnSummary) :=
  let tyMap := implBlocks.foldl (fun acc ib =>
    let implTy := if ib.typeParams.isEmpty then Ty.named ib.typeName
                  else Ty.generic ib.typeName (ib.typeParams.map Ty.typeVar)
    acc ++ ib.methods.map fun f => (ib.typeName ++ "_" ++ f.name, implTy)
  ) []
  let tyMap := traitImpls.foldl (fun acc tb =>
    let implTy := if tb.typeParams.isEmpty then Ty.named tb.typeName
                  else Ty.generic tb.typeName (tb.typeParams.map Ty.typeVar)
    acc ++ tb.methods.map fun f => (tb.typeName ++ "_" ++ f.name, implTy)
  ) tyMap
  sigs.map fun (name, sig) =>
    match tyMap.lookup name with
    | some implTy =>
      (name, { sig with
        params := sig.params.map fun (n, t) => (n, resolveSelfTy t implTy)
        retTy := resolveSelfTy sig.retTy implTy })
    | none => (name, sig)

private partial def resolveAliasesInTy (aliases : List (String × Ty)) : Ty → Ty
  | .named name => match aliases.lookup name with
    | some resolved => resolved
    | none => .named name
  | .ref inner => .ref (resolveAliasesInTy aliases inner)
  | .refMut inner => .refMut (resolveAliasesInTy aliases inner)
  | .ptrMut inner => .ptrMut (resolveAliasesInTy aliases inner)
  | .heap inner => .heap (resolveAliasesInTy aliases inner)
  | .heapArray inner => .heapArray (resolveAliasesInTy aliases inner)
  | .generic name args => .generic name (args.map (resolveAliasesInTy aliases))
  | .fn_ params capSet retTy =>
    .fn_ (params.map (resolveAliasesInTy aliases)) capSet (resolveAliasesInTy aliases retTy)
  | ty => ty

private def resolveAliasesInSig (aliases : List (String × Ty)) (sig : FnSummary) : FnSummary :=
  { sig with
    params := sig.params.map fun (n, ty) => (n, resolveAliasesInTy aliases ty)
    retTy := resolveAliasesInTy aliases sig.retTy }

partial def buildFileSummary (m : Module) : FileSummary :=
  -- Build alias map for resolving type aliases in signatures
  let aliasMap := m.typeAliases.map fun ta => (ta.name, ta.targetTy)
  let functions := m.functions.map fun f =>
    let (name, sig) := fnDefToSummary f
    (name, resolveAliasesInSig aliasMap sig)
  let pubFns := m.functions.filter (·.isPublic) |>.map (·.name)
  let pubStructs := m.structs.filter (·.isPublic) |>.map (·.name)
  let pubEnums := m.enums.filter (·.isPublic) |>.map (·.name)
  let pubTraits := m.traits.filter (·.isPublic) |>.map (·.name)
  let pubExterns := m.externFns.filter (·.isPublic) |>.map (·.name)
  let pubConstants := m.constants.filter (·.isPublic) |>.map (·.name)
  let pubAliases := m.typeAliases.filter (·.isPublic) |>.map (·.name)
  let pubImplMethods := m.implBlocks.foldl (fun acc ib =>
    acc ++ (ib.methods.filter (·.isPublic)).map (fun f => ib.typeName ++ "_" ++ f.name)) []
  let pubTraitImplMethods := m.traitImpls.foldl (fun acc ti =>
    acc ++ (ti.methods.filter (·.isPublic)).map (fun f => ti.typeName ++ "_" ++ f.name)) []
  let publicNames := pubFns ++ pubStructs ++ pubEnums ++ pubTraits ++ pubExterns
                     ++ pubConstants ++ pubAliases ++ pubImplMethods ++ pubTraitImplMethods
  let externFnSigs := m.externFns.map fun ef =>
    let capSet := if ef.isTrusted then CapSet.empty else CapSet.concrete ["Unsafe"]
    let sig : FnSummary := {
      params := ef.params.map fun p => (p.name, p.ty)
      retTy := ef.retTy
      capSet := capSet
    }
    (ef.name, resolveAliasesInSig aliasMap sig)
  let implMethodSigs := (implBlocksToMethodSummaries m.implBlocks
                     ++ traitImplBlocksToMethodSummaries m.traitImpls).map fun (n, sig) =>
                     (n, resolveAliasesInSig aliasMap sig)
  { name := m.name
    functions := functions
    structs := m.structs
    enums := m.enums
    implBlocks := m.implBlocks
    traitImpls := m.traitImpls
    publicNames := publicNames
    traits := m.traits
    constants := m.constants.map fun c => { name := c.name, ty := c.ty, isPublic := c.isPublic }
    typeAliases := m.typeAliases
    newtypes := m.newtypes
    externFns := m.externFns
    externFnSigs := externFnSigs
    implMethodSigs := implMethodSigs
    imports := m.imports
    submoduleSummaries := m.submodules.map fun sub => (sub.name, buildFileSummary sub) }

/-- Recursively collect all submodule entries with all qualification prefixes.
    For each submodule, we register it under every suffix of its fully qualified path.
    E.g. for top-level `project` containing `mymod` containing `submodule`:
    - `project.mymod.submodule`, `mymod.submodule`, `submodule` -/
private partial def flattenSubmodules (prefixes : List String) (summary : FileSummary)
    : List (String × FileSummary) :=
  summary.submoduleSummaries.foldl (fun acc (subName, subSummary) =>
    -- Build all qualified names: each prefix + "." + subName, plus the bare subName
    let qualNames := prefixes.map (· ++ "." ++ subName) ++ [subName]
    let entries := qualNames.map fun qn => (qn, subSummary)
    -- Recurse with the new set of prefixes for deeper nesting
    acc ++ entries ++ flattenSubmodules qualNames subSummary
  ) []

def buildSummaryTable (modules : List Module) : List (String × FileSummary) :=
  modules.foldl (fun acc m =>
    let summary := buildFileSummary m
    let subEntries := flattenSubmodules [m.name] summary
    acc ++ [(m.name, summary)] ++ subEntries
  ) []

def resolveImports (imports : List ImportDecl)
    (summaryTable : List (String × FileSummary))
    (unknownModuleMsg : String → String)
    (notPublicMsg : String → String → String)
    : Except String ResolvedImports := do
  let resolved ← imports.foldlM (init := ({} : ResolvedImports)) fun acc imp =>
    match summaryTable.lookup imp.moduleName with
    | none => .error (unknownModuleMsg imp.moduleName)
    | some summary =>
      -- Build alias map from the exporting module's type aliases and newtypes
      -- (newtypes are erased at module boundaries for imported signatures)
      let aliasMap := (summary.typeAliases.map fun ta => (ta.name, ta.targetTy))
        ++ (summary.newtypes.map fun nt => (nt.name, nt.innerTy))
      let pubFns := summary.functions ++ summary.externFnSigs
      imp.symbols.foldlM (init := acc) fun acc sym =>
        let origName := sym.name
        let localName := sym.effectiveName
        match pubFns.find? fun (n, _) => n == origName with
        | some (_, sig) =>
          let resolvedSig := resolveAliasesInSig aliasMap sig
          let newAliases := if localName != origName
            then acc.linkerAliases ++ [(localName, origName)]
            else acc.linkerAliases
          .ok { acc with functions := acc.functions ++ [(localName, resolvedSig)],
                         linkerAliases := newAliases }
        | none =>
          match summary.structs.find? fun sd => sd.name == origName with
          | some sd =>
            let structImpls := summary.implBlocks.filter fun ib => ib.typeName == origName
            let structTraitImpls := summary.traitImpls.filter fun tb => tb.typeName == origName
            let mangledNames := structImpls.foldl (fun ns ib =>
              ns ++ (ib.methods.filter (·.isPublic)).map fun f => ib.typeName ++ "_" ++ f.name) []
              ++ structTraitImpls.foldl (fun ns tb =>
              ns ++ (tb.methods.filter (·.isPublic)).map fun f => tb.typeName ++ "_" ++ f.name) []
            let matchingSigs := summary.implMethodSigs.filter fun (name, _) =>
              mangledNames.contains name
            .ok { acc with structs := acc.structs ++ [sd],
                           implBlocks := acc.implBlocks ++ structImpls,
                           traitImpls := acc.traitImpls ++ structTraitImpls,
                           implMethodSigs := acc.implMethodSigs ++ matchingSigs }
          | none =>
            match summary.enums.find? fun ed => ed.name == origName with
            | some ed => .ok { acc with enums := acc.enums ++ [ed] }
            | none =>
              match summary.typeAliases.find? fun ta => ta.isPublic && ta.name == origName with
              | some ta => .ok { acc with typeAliases := acc.typeAliases ++ [(localName, ta.targetTy)] }
              | none => .error (notPublicMsg origName imp.moduleName)
  -- Auto-include impl methods for builtin types (String, Vec, etc.) from all
  -- loaded modules, so methods like String.drop() work without explicit import.
  let builtinNames := builtinTypeNames
  let builtinResult := summaryTable.foldl (init := resolved) fun acc (_, summary) =>
    let builtinImpls := summary.implBlocks.filter fun ib => List.elem ib.typeName builtinNames
    let builtinTraitImpls := summary.traitImpls.filter fun tb => List.elem tb.typeName builtinNames
    let mangledNames := builtinImpls.foldl (fun ns ib =>
      ns ++ (ib.methods.filter (·.isPublic)).map fun f => ib.typeName ++ "_" ++ f.name) []
      ++ builtinTraitImpls.foldl (fun ns tb =>
      ns ++ (tb.methods.filter (·.isPublic)).map fun f => tb.typeName ++ "_" ++ f.name) []
    let newSigs := summary.implMethodSigs.filter fun (name, _) =>
      mangledNames.contains name && !(acc.implMethodSigs.any fun (n, _) => n == name)
    let newImpls := builtinImpls.filter fun ib =>
      !(acc.implBlocks.any fun existing => existing.typeName == ib.typeName && existing.methods.length == ib.methods.length)
    let newTraitImpls := builtinTraitImpls.filter fun tb =>
      !(acc.traitImpls.any fun existing => existing.typeName == tb.typeName && existing.methods.length == tb.methods.length)
    { acc with implMethodSigs := acc.implMethodSigs ++ newSigs,
               implBlocks := acc.implBlocks ++ newImpls,
               traitImpls := acc.traitImpls ++ newTraitImpls }
  pure builtinResult

end Concrete
