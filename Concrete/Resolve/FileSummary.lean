import Concrete.Frontend.AST
import Concrete.Resolve.Shared
import Concrete.Resolve.Intrinsic
import Concrete.Report.Diagnostic

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
  /-- A bodiless declaration (`extern`, `#[intrinsic]`): the parser keeps no type-parameter list
      for one, so `typeParams` is empty because it was not recorded, not because there are none
      (`sizeof<T>`). Bug 221's arity check skips such a callee. -/
  typeParamsUnrecorded : Bool := false
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
  newtypes       : List NewtypeDef := []
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
         typeBounds := ib.typeBounds ++ f.typeBounds
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
    -- `impl<cap C> Writer<C>`: the self type carries the impl's capability parameters,
    -- by name, the convention capability arguments in types use (R-0484).
    let implTy := if ib.typeParams.isEmpty && ib.capParams.isEmpty then Ty.named ib.typeName
                  else Ty.generic ib.typeName (ib.typeParams.map Ty.typeVar)
                         (ib.capParams.map fun c => CapSet.concrete [c])
    acc ++ ib.methods.map fun f => (ib.typeName ++ "_" ++ f.name, implTy)
  ) []
  let tyMap := traitImpls.foldl (fun acc tb =>
    let implTy := if tb.typeParams.isEmpty then Ty.named tb.typeName
                  -- Trait impls take no capability parameters (refused at parse).
                  else Ty.generic tb.typeName (tb.typeParams.map Ty.typeVar) []
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
  | .generic name args caps => .generic name (args.map (resolveAliasesInTy aliases)) caps
  | .fn_ params capSet retTy =>
    .fn_ (params.map (resolveAliasesInTy aliases)) capSet (resolveAliasesInTy aliases retTy)
  | ty => ty

private def resolveAliasesInSig (aliases : List (String × Ty)) (sig : FnSummary) : FnSummary :=
  { sig with
    params := sig.params.map fun (n, ty) => (n, resolveAliasesInTy aliases ty)
    retTy := resolveAliasesInTy aliases sig.retTy }

partial def buildFileSummary (m : Module) (definitionPath : String := "") : FileSummary :=
  -- `FileSummary.name` remains the module's local name for the existing lookup
  -- table. Type identity needs the full definition path, including every
  -- enclosing module, so carry that separately through recursive summaries.
  let thisDefinitionPath := if definitionPath.isEmpty then m.name else definitionPath
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
  let pubNewtypes := m.newtypes.filter (·.isPublic) |>.map (·.name)
  let pubImplMethods := m.implBlocks.foldl (fun acc ib =>
    acc ++ (ib.methods.filter (·.isPublic)).map (fun f => ib.typeName ++ "_" ++ f.name)) []
  let pubTraitImplMethods := m.traitImpls.foldl (fun acc ti =>
    acc ++ (ti.methods.filter (·.isPublic)).map (fun f => ti.typeName ++ "_" ++ f.name)) []
  let publicNames := pubFns ++ pubStructs ++ pubEnums ++ pubTraits ++ pubExterns
                     ++ pubConstants ++ pubAliases ++ pubNewtypes
                     ++ pubImplMethods ++ pubTraitImplMethods
  let externFnSigs := m.externFns.map fun ef =>
    -- R-0484 ROOT SITE. A `trusted extern` is typed with an EMPTY capability set, which
    -- is correct for what `with(...)` means — required ambient authority, and a trusted
    -- author has taken responsibility for supplying it — and is also the single point
    -- where operational effects stop being tracked. `libc_write` declares nothing, so
    -- `console_write` wrapping it can declare nothing, so `Writer`'s `write_fn` field is
    -- typed capability-free, so a helper calling `w.write(b)` reports `(pure)`.
    --
    -- DO NOT "fix" this by putting a capability here. Required authority and performed
    -- effects are different facts: a function holding an authorized `Writer` legitimately
    -- requires nothing and performs I/O, so tightening `requires` would break the
    -- object-capability model (authority checked at acquisition, carried by the handle)
    -- without making the effect visible.
    --
    -- The repair is a SEPARATE `performs` fact that an opaque extern must declare
    -- conservatively, with missing information reading as Unknown rather than as none.
    -- Recorded in docs/project/ABSENCE_IS_NOT_A_FACT.md; this line is the reason the
    -- effect model is a language change and not a checker patch.
    -- R-0484 (2026-10-01) settled this site differently from the note above, which is
    -- kept as the record of the earlier plan: a foreign binding DECLARES its effects
    -- (`extern fn write(..) with(Console)`), and that declaration is part of what a
    -- caller requires, across packages too. `Unsafe` is added unless the binding is a
    -- `trusted extern`. Resolve refuses a binding with no declaration, so `getD` only
    -- covers the tolerant path that continues after that error.
    let capSet := Capabilities.externFnRequiredCaps ef.isTrusted (ef.capSet.getD .empty)
    let sig : FnSummary := {
      params := ef.params.map fun p => (p.name, p.ty)
      retTy := ef.retTy
      capSet := capSet
      typeParamsUnrecorded := true
    }
    (ef.name, resolveAliasesInSig aliasMap sig)
  let implMethodSigs := (implBlocksToMethodSummaries m.implBlocks
                     ++ traitImplBlocksToMethodSummaries m.traitImpls).map fun (n, sig) =>
                     (n, resolveAliasesInSig aliasMap sig)
  { name := m.name
    functions := functions
    -- R-0004 path 1 (local): stamp both definition-site components once.
    -- Import aliases may later change `.name`; they never change these fields.
    structs := m.structs.map (fun sd =>
      { sd with definedIn := thisDefinitionPath, definitionName := sd.name })
    enums := m.enums.map (fun ed =>
      { ed with definedIn := thisDefinitionPath, definitionName := ed.name })
    implBlocks := m.implBlocks
    traitImpls := m.traitImpls
    publicNames := publicNames
    traits := m.traits
    constants := m.constants.map fun c => { name := c.name, ty := c.ty, isPublic := c.isPublic }
    typeAliases := m.typeAliases
    newtypes := m.newtypes.map (fun nt => { nt with definedIn := thisDefinitionPath })
    externFns := m.externFns
    externFnSigs := externFnSigs
    implMethodSigs := implMethodSigs
    imports := m.imports
    submoduleSummaries := m.submodules.map fun sub =>
      let childPath := if thisDefinitionPath.isEmpty then sub.name
                       else thisDefinitionPath ++ "." ++ sub.name
      (sub.name, buildFileSummary sub childPath) }

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

/-- Every user-type head name referenced by a type (for the bug-036 import
    closure): `.named`/`.generic` heads, recursing through wrappers. -/
partial def tyHeadNames : Ty → List String
  | .named n => [n]
  | .generic n args _ => n :: args.flatMap tyHeadNames
  | .ref i | .refMut i | .ptrMut i | .ptrConst i | .heap i | .heapArray i => tyHeadNames i
  | .array e _ => tyHeadNames e
  | .fn_ ps _ r => ps.flatMap tyHeadNames ++ tyHeadNames r
  | _ => []

/-- Bug 206: the code of an import refusal, by the pass that reports it (MOD-2): an unknown
    module is E0110 / E0284 / E0418 and a symbol that is not public E0111 / E0285 / E0419 in
    Resolve / Check / Elab. -/
def importCode (pass : String) (unknownModule : Bool) : String :=
  match pass, unknownModule with
  | "check", true => "E0284" | "check", false => "E0285"
  | "elab", true => "E0418" | "elab", false => "E0419"
  | _, true => "E0110" | _, false => "E0111"

/-- Bug 233: the kind of a public declaration of `summary` that no import binds, a trait or a
    constant, if `name` is one. Every pass's public-name list (`publicNames`) holds them, so the
    import rule fell through to "not public", which they are not. Traits are module-local and cannot
    be imported (TRAIT spec); a constant cannot be imported yet, because Core names a constant by its
    bare spelling (bug 132) and an imported one could not be told from the importer's own. -/
def unimportableKind (summary : FileSummary) (name : String) : Option String :=
  if summary.traits.any (fun t => t.isPublic && t.name == name) then some "trait"
  else if summary.constants.any (fun c => c.isPublic && c.name == name) then some "constant"
  else none

/-- Bug 233: the refusal of an import naming a public trait or constant (`unimportableKind`). -/
def unimportableMsg (kind symbol moduleName : String) : String :=
  if kind == "trait" then
    s!"'{symbol}' is a trait of module '{moduleName}', and a trait cannot be imported: traits are module-local"
  else
    s!"'{symbol}' is a constant of module '{moduleName}', and a constant cannot be imported yet: Core names a constant by its spelling, so an imported one could not be told from a constant of the same name in the importer (bug 233)"

/-- Bug 233: the code of that refusal, by the pass that reports it (as `importCode`). -/
def unimportableCode (pass : String) : String :=
  match pass with
  | "check" => "E0323" | "elab" => "E0423" | _ => "E0126"

/-- Bug 222: what travels with an imported type declared as `name` in `summary`, whatever kind
    of declaration it is (struct, enum or newtype): its inherent impl blocks, its trait impls and
    the signatures of their public methods. The struct and newtype import paths each restated this
    selection and the enum paths omitted it, so every bound needing a trait impl on an imported enum was E0241
    outside its module (its method calls resolved through the callee table regardless). Every
    type import now takes its impls from here. -/
private def typeImplsOf (summary : FileSummary) (name : String) :
    List ImplBlock × List ImplTraitBlock × List (String × FnSummary) :=
  let impls := summary.implBlocks.filter fun ib => ib.typeName == name
  let traitImpls := summary.traitImpls.filter fun tb => tb.typeName == name
  let mangled := impls.foldl (fun ns ib =>
    ns ++ (ib.methods.filter (·.isPublic)).map fun f => ib.typeName ++ "_" ++ f.name) []
    ++ traitImpls.foldl (fun ns tb =>
    ns ++ (tb.methods.filter (·.isPublic)).map fun f => tb.typeName ++ "_" ++ f.name) []
  let sigs := summary.implMethodSigs.filter fun (n, _) => mangled.contains n
  (impls, traitImpls, sigs)

def resolveImports (imports : List ImportDecl)
    (summaryTable : List (String × FileSummary))
    (unknownModuleMsg : String → String)
    (notPublicMsg : String → String → String)
    (pass : String := "resolve")
    : Except Diagnostics ResolvedImports := do
  let resolved ← imports.foldlM (init := ({} : ResolvedImports)) fun acc imp =>
    match summaryTable.lookup imp.moduleName with
    | none => .error [{ severity := .error, message := unknownModuleMsg imp.moduleName, pass := pass, span := some imp.span, hint := none, code := importCode pass true }]
    | some summary =>
      -- Build alias map from the exporting module's type aliases only.
      -- Newtypes are NOT erased at the module boundary anymore: Layout
      -- resolves newtype names directly via Layout.Ctx, and erasing here
      -- would break type identity (Option<Port> would arrive as Option<u16>).
      let aliasMap := summary.typeAliases.map fun ta => (ta.name, ta.targetTy)
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
            let (structImpls, structTraitImpls, matchingSigs) := typeImplsOf summary origName
            -- Bug 064: register under the name the IMPORTER uses. The original
            -- `sd` kept `name = origName`, so `lookupStruct alias` found nothing
            -- and a field access on an aliased imported type was rejected as
            -- E0254 "non-struct type" — while the SAME import unaliased reached
            -- E0298 (a privacy answer), proving the type resolved fine and the
            -- alias alone broke resolution.
            --
            -- A no-op when unaliased, since `localName == origName` there.
            --
            -- R-0004 path 2 (explicit import). Change only the lookup spelling.
            -- `definedIn` and `definitionName` were minted by the defining
            -- summary and must survive aliases and re-exports unchanged.
            .ok { acc with structs := acc.structs ++ [{ sd with name := localName }],
                           implBlocks := acc.implBlocks ++ structImpls,
                           traitImpls := acc.traitImpls ++ structTraitImpls,
                           implMethodSigs := acc.implMethodSigs ++ matchingSigs }
          | none =>
            match summary.enums.find? fun ed => ed.name == origName with
            | some ed =>
              let (enumImpls, enumTraitImpls, enumSigs) := typeImplsOf summary origName
              .ok { acc with enums := acc.enums ++ [{ ed with name := localName }],
                             implBlocks := acc.implBlocks ++ enumImpls,
                             traitImpls := acc.traitImpls ++ enumTraitImpls,
                             implMethodSigs := acc.implMethodSigs ++ enumSigs }
            | none =>
              match summary.typeAliases.find? fun ta => ta.isPublic && ta.name == origName with
              | some ta => .ok { acc with typeAliases := acc.typeAliases ++ [(localName, ta.targetTy)] }
              | none =>
                match summary.newtypes.find? fun nt => nt.isPublic && nt.name == origName with
                | some nt =>
                  -- Pull along inherent impl methods on the newtype, mirroring
                  -- the struct path. This keeps `Wrap::try_new(...)` callable
                  -- after `import Wrap.{Port}`.
                  let (newtypeImpls, newtypeTraitImpls, matchingSigs) := typeImplsOf summary origName
                  .ok { acc with newtypes := acc.newtypes ++ [nt],
                                 implBlocks := acc.implBlocks ++ newtypeImpls,
                                 traitImpls := acc.traitImpls ++ newtypeTraitImpls,
                                 implMethodSigs := acc.implMethodSigs ++ matchingSigs }
                | none =>
                  match unimportableKind summary origName with
                  | some kind => .error [{ severity := .error, message := unimportableMsg kind origName imp.moduleName, pass := pass, span := some imp.span, hint := none, code := unimportableCode pass }]
                  | none => .error [{ severity := .error, message := notPublicMsg origName imp.moduleName, pass := pass, span := some imp.span, hint := none, code := importCode pass false }]
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
  -- Bug 036: a type's metadata (Copy marker, impl methods) must travel with
  -- the TYPE, not with whether the user named it in the import list. Close
  -- over public types REACHABLE through what was imported: an enum variant's
  -- payload type (CliResult -> CliError/CliFlags), a struct field's type, a
  -- signature's param/return types. Without this, matching a payload out of
  -- an imported enum produced false E0295 (payload judged non-Copy) and
  -- false E0264 (its methods invisible). Same-module closure only — a public
  -- export referencing a PRIVATE type still requires today's behavior.
  let importedModules := imports.filterMap fun imp =>
    (summaryTable.lookup imp.moduleName).map fun s => s
  let closed := Id.run do
    let mut acc := builtinResult
    for _round in [0:4] do
      let referenced :=
        acc.functions.flatMap (fun (_, sig) =>
          sig.params.flatMap (fun (_, t) => tyHeadNames t) ++ tyHeadNames sig.retTy)
        ++ acc.implMethodSigs.flatMap (fun (_, sig) =>
          sig.params.flatMap (fun (_, t) => tyHeadNames t) ++ tyHeadNames sig.retTy)
        ++ acc.structs.flatMap (fun sd => sd.fields.flatMap (fun f => tyHeadNames f.ty))
        ++ acc.enums.flatMap (fun ed =>
          ed.variants.flatMap (fun v => v.fields.flatMap (fun f => tyHeadNames f.ty)))
        ++ acc.newtypes.flatMap (fun nt => tyHeadNames nt.innerTy)
      let present := acc.structs.map (·.name) ++ acc.enums.map (·.name)
        ++ acc.newtypes.map (·.name) ++ acc.typeAliases.map (·.1)
      let missing := (referenced.filter fun n => !present.contains n).eraseDups
      if missing.isEmpty then break
      let mut grew := false
      for summary in importedModules do
        for n in missing do
          if !summary.publicNames.contains n then continue
          -- re-check presence (an earlier module this round may have added it)
          if acc.structs.any (·.name == n) || acc.enums.any (·.name == n)
             || acc.newtypes.any (·.name == n) then continue
          match summary.structs.find? fun sd => sd.name == n with
          | some sd =>
            let (impls, traitImpls, sigs) := typeImplsOf summary n
            -- R-0004 path 3 (transitive closure): preserve the identity already
            -- minted by the defining summary. Reminting from this traversal's
            -- module would turn a transport path into an identity owner.
            acc := { acc with structs := acc.structs ++ [sd],
                              implBlocks := acc.implBlocks ++ impls,
                              traitImpls := acc.traitImpls ++ traitImpls,
                              implMethodSigs := acc.implMethodSigs ++ sigs }
            grew := true
          | none =>
            match summary.enums.find? fun ed => ed.name == n with
            | some ed =>
              let (impls, traitImpls, sigs) := typeImplsOf summary n
              acc := { acc with enums := acc.enums ++ [ed],
                                implBlocks := acc.implBlocks ++ impls,
                                traitImpls := acc.traitImpls ++ traitImpls,
                                implMethodSigs := acc.implMethodSigs ++ sigs }
              grew := true
            | none =>
              match summary.newtypes.find? fun nt => nt.isPublic && nt.name == n with
              | some nt =>
                let (impls, traitImpls, sigs) := typeImplsOf summary n
                acc := { acc with newtypes := acc.newtypes ++ [nt],
                                  implBlocks := acc.implBlocks ++ impls,
                                  traitImpls := acc.traitImpls ++ traitImpls,
                                  implMethodSigs := acc.implMethodSigs ++ sigs }
                grew := true
              | none => pure ()
      if !grew then break
    return acc
  pure closed

end Concrete
