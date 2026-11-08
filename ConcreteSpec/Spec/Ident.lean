import ConcreteSpec.Spec.Basics

/-!
# Identities, sites and versions

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). Part of `ConcreteSpec.Spec`;
see `ConcreteSpec/README.md`.

Core names everything by a typed identity, never by a bare string or number: locals, declarations,
constants, fields, functions, packages, modules, trait instances, specializations, loop labels,
brands, allocator lineages, loans, grants, blocks, capability variables, theorems, obligations and
premises each have their own type. A module contains its package and non-empty package-rooted path,
so a declaration, field, variant, function or constant carries that complete identity. A local is
numbered by elaboration and keeps its source name only as a diagnostic hint. Equality of identities
is what the rules use (the facts decision: no bare-name collisions).

A *site* (`SiteId`) is one per operation and per refusable statement. Diagnostics, facts, traps
and discharge tokens are keyed by site; a program's site table maps each site to its byte-ranged
source `Span`, which elaboration produces (README obligation O1). Lexical site and trait-instance
numbers are distinct from persistent `StableKey`s. Specialization request edges retain their sites
separately from identities keyed by generic arguments. Obligations carry site, kind and a same-site
discriminator; conditional facts carry typed premise identities.

A rule is cited by `RuleId`: the Lean name of the declaration that states it and that rule's own
revision. Every output carries one `specVersion`. Which rules exist is the registry in `Rules`;
this module depends on nothing downstream.

These types are part of the diagnostics and facts interface: the Lean types are the source of its
JSON schema (owner decision 2026-11-07), so a field's name is part of the interface and renaming
one is an interface change.
-/

namespace ConcreteSpec.Core

/-- A local variable: a source-name hint for presentation and a numeric semantic identity.
Elaboration gives every local of a function its own number (a local is bound at most once, Q9.1)
and its source name as the hint, so two locals of one function never share a number. The hint never
determines binding or evaluation; `Prim.eval_rehint` proves that changing every hint consistently
changes no result. Rehinting must be applied consistently to binders and occurrences because C1's
`AList.lookup` compares the whole `Var`, including its hint. -/
structure Var where
  hint : String
  id : Nat
  deriving DecidableEq, Repr

/-- A place of safe structured storage. C4 and C5 will relate this identity to their root/projection
and hybrid-state representations; this C9 base deliberately does not edit or choose that mapping. -/
structure PlaceId where
  id : Nat
  deriving DecidableEq, Repr

/-- A primitive operation in a registry (Q9.6, Q9.7). -/
structure PrimId where
  id : Nat
  deriving DecidableEq, Repr

/-- A particular revision of a primitive's complete contract. Evidence binds this identity rather
than comparing contract predicates or inferring a contract from a primitive's spelling. -/
structure ContractId where
  prim : PrimId
  rev : Nat
  deriving DecidableEq, Repr

/-- A site: one operation or one refusable statement. Its number is lexical, assigned afresh by
elaboration; persistent evidence keys are defined separately below. -/
structure SiteId where
  id : Nat
  deriving DecidableEq, Repr

/-- The kinds of proof obligation named by the Core rules. `safety` covers an operation's residual
safety condition; the other constructors name the contract, invariant, assertion, resource-bound
and information-flow obligations explicitly stated by the spec. -/
inductive ObligationKind where
  | safety
  | termination
  | requires
  | ensures
  | loopInvariant
  | structInvariant
  | assertion
  | stepBound
  | stackBound
  | noninterference
  | constantTime
  deriving DecidableEq, Repr

/-- One proof obligation: its originating site, its kind, and a local discriminator that keeps two
obligations of the same kind at one site distinct. Facts and discharge evidence name this identity,
never a source string or a position in a generated list. -/
structure ObligationId where
  site : SiteId
  kind : ObligationKind
  discriminator : Nat
  deriving DecidableEq, Repr

/-- A package version. Its own syntax and ordering are deliberately opaque: semver has not been
chosen. This is part of package identity, not an artifact content hash (owner decision 2026-11-08). -/
structure PackageVersion where
  token : String
  deriving DecidableEq, Repr

/-- A package identity is its name and version. Artifact hashes and load origins are separate. -/
structure PackageId where
  name : String
  version : PackageVersion
  deriving DecidableEq, Repr

/-- Where a loaded package came from. Origin is deliberately outside `PackageId`: equal name and
version from different origins are a load-time collision, not two package identities. -/
structure PackageOrigin where
  token : String
  deriving DecidableEq, Repr

/-- A module: its package and its non-empty path from that package's root (`["std", "bytes"]`).
The first segment is therefore always a real top-level module name. -/
structure ModuleId where
  package : PackageId
  path : List String
  path_nonempty : path ≠ []
  deriving DecidableEq, Repr

/-- The module's required top-level name. -/
def ModuleId.topLevel (m : ModuleId) : String := m.path.head m.path_nonempty

/-- One loaded module and the origin of its package. -/
structure LoadedModule where
  module : ModuleId
  origin : PackageOrigin
  deriving DecidableEq, Repr

/-- A load-time refusal, retaining both complete loaded-module records for the diagnostic. OD44
refuses equal top-level names from different packages. The 2026-11-08 package-identity decision
also refuses equal package name/version arriving from different origins. -/
inductive LoadCollision where
  | topLevel (first second : LoadedModule)
      (sameTopLevel : first.module.topLevel = second.module.topLevel)
      (differentPackages : first.module.package ≠ second.module.package)
  | packageOrigin (first second : LoadedModule)
      (samePackage : first.module.package = second.module.package)
      (differentOrigins : first.origin ≠ second.origin)

/-- The first loaded module named by a collision diagnostic. -/
def LoadCollision.first : LoadCollision → LoadedModule
  | .topLevel first _ _ _ | .packageOrigin first _ _ _ => first

/-- The second loaded module named by a collision diagnostic. -/
def LoadCollision.second : LoadCollision → LoadedModule
  | .topLevel _ second _ _ | .packageOrigin _ second _ _ => second

/-- Both sides of a collision occur in the loaded input. -/
def LoadedModules.HasCollision (modules : List LoadedModule) (collision : LoadCollision) : Prop :=
  collision.first ∈ modules ∧ collision.second ∈ modules

/-- A loaded set is well formed exactly when it contains no collision. -/
def LoadedModules.WellFormed (modules : List LoadedModule) : Prop :=
  ∀ collision, ¬ LoadedModules.HasCollision modules collision

/-- Check one ordered pair, producing a diagnostic that retains both modules. -/
def LoadCollision.between? (first second : LoadedModule) : Option LoadCollision :=
  if package : first.module.package = second.module.package then
    if origin : first.origin = second.origin then none
    else some (.packageOrigin first second package origin)
  else if top : first.module.topLevel = second.module.topLevel then
    some (.topLevel first second top package)
  else none

/-- Find the first collision between one module and any module in a list. -/
def LoadCollision.withModule (first : LoadedModule) : List LoadedModule → Option LoadCollision
  | [] => none
  | second :: rest => match LoadCollision.between? first second with
    | some collision => some collision
    | none => LoadCollision.withModule first rest

/-- Search every candidate against the complete loaded set. Ordered pairs make completeness
independent of which member of a collision appears first in the input. -/
def LoadedModules.findCollisionFrom : List LoadedModule → List LoadedModule → Option LoadCollision
  | [], _ => none
  | first :: rest, modules => match LoadCollision.withModule first modules with
    | some collision => some collision
    | none => LoadedModules.findCollisionFrom rest modules

/-- Find the first collision in a loaded set. -/
def LoadedModules.findCollision (modules : List LoadedModule) : Option LoadCollision :=
  LoadedModules.findCollisionFrom modules modules

/-- Either admit exactly the loaded input or refuse it with a two-sided collision diagnostic. -/
inductive LoadResult where
  | admitted (modules : List LoadedModule)
  | refused (collision : LoadCollision)

/-- The OD44/package-origin load boundary. There is no coexistence fallback. -/
def LoadedModules.load (modules : List LoadedModule) : LoadResult :=
  match LoadedModules.findCollision modules with
  | some collision => .refused collision
  | none => .admitted modules

/-- A type declaration (a struct or an enum): its module and its name there. Declaration names are
nominal (Q9.2): two declarations are the same exactly when their identities are equal. -/
structure DeclId where
  module : ModuleId
  name : String
  deriving DecidableEq, Repr

/-- A variant of an enum declaration, by its declaration and name. -/
structure VariantId where
  decl : DeclId
  name : String
  deriving DecidableEq, Repr

/-- What a field belongs to: a struct declaration, or one variant of an enum. -/
inductive FieldOwner where
  | struct (d : DeclId)
  | variant (v : VariantId)
  deriving DecidableEq, Repr

/-- A field, by its owner and name: two variants of one enum may each have a field `value`, and
they are different fields. -/
structure FieldId where
  owner : FieldOwner
  name : String
  deriving DecidableEq, Repr

/-- A function: its module and its name there. -/
structure FnId where
  module : ModuleId
  name : String
  deriving DecidableEq, Repr

/-- A module constant (`const N: T = e;`): its module and its name there. -/
structure ConstId where
  module : ModuleId
  name : String
  deriving DecidableEq, Repr

/-- A trait instance, numbered by elaboration. -/
structure InstanceId where
  id : Nat
  deriving DecidableEq, Repr

/-- A generic definition that can be specialized: a generic function or a generic type declaration.
OD77 refuses generic trait methods and OD148 refuses type parameters on traits. No decided
associated-item generic category is added here. -/
inductive GenericId where
  | function (f : FnId)
  | declaration (d : DeclId)
  deriving DecidableEq, Repr

/-- A specialization keyed by the generic and all of its type and capability arguments. The argument
representations are parameters because `Ident` precedes the Core type and capability languages;
`InstanceId` remains the separate, elaboration-numbered identity of a trait instance. -/
structure SpecializationId (Ty Cap : Type) where
  generic : GenericId
  typeArguments : List Ty
  capabilityArguments : List Cap
  deriving DecidableEq, Repr

/-- A call-site request edge. The site is metadata of the request, never a component of the
specialization identity, so equal requests from different sites share one specialization. -/
structure SpecializationRequest (Ty Cap : Type) where
  site : SiteId
  specialization : SpecializationId Ty Cap
  deriving DecidableEq, Repr

/-- Record that one site requests the specialization of a generic at exactly these arguments. -/
def SpecializationRequest.for {Ty Cap : Type} (site : SiteId) (generic : GenericId)
    (typeArguments : List Ty) (capabilityArguments : List Cap) : SpecializationRequest Ty Cap :=
  ⟨site, ⟨generic, typeArguments, capabilityArguments⟩⟩

/-- A resolved request edge points to a numeric entry in the canonical specialization table while
retaining the complete request for diagnostics and audit. -/
structure SpecializationEdge (Ty Cap : Type) where
  request : SpecializationRequest Ty Cap
  entry : Nat
  deriving DecidableEq, Repr

/-- Canonical specialization entries and the resolved edges of every request. -/
structure SpecializationTable (Ty Cap : Type) where
  entries : List (SpecializationId Ty Cap)
  edges : List (SpecializationEdge Ty Cap)
  deriving DecidableEq, Repr

/-- Resolve a numeric specialization edge against an entry list. -/
def SpecializationTable.entryAt {Ty Cap : Type} (entries : List (SpecializationId Ty Cap))
    (index : Nat) : Option (SpecializationId Ty Cap) := entries[index]?

/-- Find an existing specialization entry. -/
def SpecializationTable.entryIndex {Ty Cap : Type} [DecidableEq Ty] [DecidableEq Cap] :
    List (SpecializationId Ty Cap) → SpecializationId Ty Cap → Option Nat
  | [], _ => none
  | entry :: rest, requested =>
    if entry = requested then some 0 else (entryIndex rest requested).map Nat.succ

/-- Intern one specialization, reusing its entry or appending exactly one new entry. -/
def SpecializationTable.intern {Ty Cap : Type} [DecidableEq Ty] [DecidableEq Cap]
    (entries : List (SpecializationId Ty Cap)) (requested : SpecializationId Ty Cap) :
    List (SpecializationId Ty Cap) × Nat :=
  match entryIndex entries requested with
  | some index => (entries, index)
  | none => (entries ++ [requested], entries.length)

/-- Canonicalize all requests and keep one resolved edge for each, in request order. -/
def SpecializationTable.canonicalize {Ty Cap : Type} [DecidableEq Ty] [DecidableEq Cap] :
    List (SpecializationRequest Ty Cap) → SpecializationTable Ty Cap
  | [] => ⟨[], []⟩
  | request :: rest =>
    let table := canonicalize rest
    let assigned := intern table.entries request.specialization
    ⟨assigned.1, ⟨request, assigned.2⟩ :: table.edges⟩

/-- The construct kinds for which the current Core and ghost specifications have occurrences.
Later source constructs join this enumeration only with their specifying slice. -/
inductive ConstructKind where
  | skip
  | binding
  | assignment
  | sequence
  | block
  | deferredCall
  | conditional
  | loop
  | break
  | continue
  | return
  | call
  | primitive
  | abort
  | assertion
  | assumption
  deriving DecidableEq, Repr

/-- Source naming of one construct. A named item and an unnamed item remain different even when an
unnamed item has a user label; this is what lets renaming preserve its siblings' ordinals. -/
inductive ConstructName where
  | named (name : String)
  | unnamed (label : Option String)
  deriving DecidableEq, Repr

/-- A labelled segment is keyed by its label; an unlabelled unnamed segment by its per-kind ordinal. -/
inductive SegmentName where
  | label (name : String)
  | ordinal (n : Nat)
  deriving DecidableEq, Repr

/-- One segment of the persistent syntactic path from the enclosing item to a construct. -/
structure KeySegment where
  kind : ConstructKind
  name : SegmentName
  deriving DecidableEq, Repr

/-- A structured function-relative path, not a display token or a raw child-index list. -/
structure FunctionRelativeKey where
  path : List KeySegment
  deriving DecidableEq, Repr

/-- An item that can own obligations: a function body, a declaration such as a struct invariant,
or a constant initializer. -/
inductive StableOwner where
  | function (f : FnId)
  | declaration (d : DeclId)
  | constant (c : ConstId)
  deriving DecidableEq, Repr

/-- A persistent evidence key: its owning item and structured path within that item. -/
structure StableKey where
  owner : StableOwner
  relative : FunctionRelativeKey
  deriving DecidableEq, Repr

/-- A minimal syntactic construct tree used by the key-assignment interface. -/
structure KeyConstruct where
  kind : ConstructKind
  name : ConstructName
  children : List KeyConstruct
  deriving Repr

/-- Whether this construct participates in the unnamed-sibling ordinal sequence. -/
def ConstructName.isUnnamed : ConstructName → Bool
  | .named _ => false
  | .unnamed _ => true

/-- Rename without changing named/unnamed status, so siblings' ordinals cannot change. -/
def ConstructName.rename : ConstructName → String → ConstructName
  | .named _, name => .named name
  | .unnamed _, name => .unnamed (some name)

/-- Rename one construct root, retaining its kind and complete subtree. -/
def KeyConstruct.rename (construct : KeyConstruct) (name : String) : KeyConstruct :=
  { construct with name := construct.name.rename name }

/-- The persistent segment assigned among these preceding siblings. Named siblings do not consume
unnamed ordinals; ordinals are per construct kind, and labels replace ordinals. -/
def KeyConstruct.segment (preceding : List KeyConstruct) (construct : KeyConstruct) : KeySegment :=
  let ordinal := (preceding.filter fun sibling =>
    sibling.kind == construct.kind && sibling.name.isUnnamed).length
  ⟨construct.kind, match construct.name with
    | .named name => .label name
    | .unnamed (some label) => .label label
    | .unnamed none => .ordinal ordinal⟩

/-- The key of a construct root under its preceding siblings. -/
def KeyConstruct.key (preceding : List KeyConstruct) (construct : KeyConstruct) : FunctionRelativeKey :=
  ⟨[construct.segment preceding]⟩

/-- Assign the complete persistent path to an occurrence in a construct forest. The numeric
route is only a locator in this particular syntax tree; no child index enters the assigned key.
Every level uses the construct's kind and name or same-kind unnamed ordinal. -/
def KeyConstruct.pathAt (forest : List KeyConstruct) : List Nat → Option FunctionRelativeKey
  | [] => some ⟨[]⟩
  | index :: route => do
    let construct ← forest[index]?
    let descendant ← KeyConstruct.pathAt construct.children route
    pure ⟨construct.segment (forest.take index) :: descendant.path⟩

/-- Assign a descendant path beneath a located root. This is also the compositional interface
used when elaboration already knows the root and its preceding siblings. -/
def KeyConstruct.descendantKey (preceding : List KeyConstruct) (construct : KeyConstruct)
    (route : List Nat) : Option FunctionRelativeKey :=
  (KeyConstruct.pathAt construct.children route).map fun descendant =>
    ⟨construct.segment preceding :: descendant.path⟩

/-- The premise and declared-assumption kinds named by decided rows. This is a closed schema
enumeration: a constructor is added only when a decided row names that category. -/
inductive PremiseKind where
  /-- A declared target assumption (`decided-project-concrete-toml`, 2026-11-06). -/
  | target
  /-- A declared heap assumption (`decided-project-concrete-toml`, 2026-11-06). -/
  | heap
  /-- A declared stack assumption (`decided-project-concrete-toml`, 2026-11-06). -/
  | stack
  /-- A declared foreign-function assumption (`decided-project-concrete-toml`, 2026-11-06). -/
  | foreignFunction
  /-- The declared trusted surface (`decided-project-concrete-toml`, 2026-11-06). -/
  | trustedSurface
  /-- The named entropy source (`decided-2026-11-07-stack-randomness`). -/
  | entropy
  /-- A source `assume`, whose dependency is tracked (OD80). -/
  | assume
  deriving DecidableEq, Repr

/-- The numeric representation of a premise category on the versioned wire. There is deliberately
no open-string or catch-all category. -/
def PremiseKind.encode : PremiseKind → Nat
  | .target => 0
  | .heap => 1
  | .stack => 2
  | .foreignFunction => 3
  | .trustedSurface => 4
  | .entropy => 5
  | .assume => 6

/-- An opaque key relative to a package's `Concrete.toml`. -/
structure PackageRelativeKey where
  token : String
  deriving DecidableEq, Repr

/-- The scope that makes a premise identity unique. Project assumptions use their package and a
key within its `Concrete.toml`; a source `assume` uses its owner-relative stable key. -/
inductive PremiseScope where
  | package (package : PackageId) (relative : PackageRelativeKey)
  | function (key : StableKey)
  deriving DecidableEq, Repr

/-- A typed premise identity. Conditional facts, declared assumptions and source `assume` debt key
on this record rather than on a display string. -/
structure PremiseId where
  kind : PremiseKind
  scope : PremiseScope
  deriving DecidableEq, Repr

/-- A fact together with the exact typed premises on which it is conditional. -/
structure ConditionalFact (Fact : Type) where
  conclusion : Fact
  premises : List PremiseId
  deriving DecidableEq, Repr

/-- The persistent key associated with one lexical site. -/
structure StableSiteKey where
  site : SiteId
  key : StableKey
  deriving DecidableEq, Repr

/-- A finite site-to-key table. -/
abbrev SiteKeyTable := AList SiteId StableKey

/-- The persistent key associated with one lexical trait instance. -/
structure StableInstanceKey where
  instanceId : InstanceId
  key : StableKey
  deriving DecidableEq, Repr

/-- An injective renumbering of lexical site numbers. -/
structure SiteRenumbering where
  apply : Nat → Nat
  injective : Function.Injective apply

/-- Renumber a site's lexical identity while retaining its persistent evidence key. -/
def StableSiteKey.renumber (r : SiteRenumbering) (k : StableSiteKey) : StableSiteKey :=
  { k with site := ⟨r.apply k.site.id⟩ }

/-- Rename table sites with an arbitrary function. Laws that require preservation of lookup and
duplicate-freedom take an injective `SiteRenumbering`; bad assignments can also be represented. -/
def SiteKeyTable.renumberSites (apply : Nat → Nat) (table : SiteKeyTable) : SiteKeyTable :=
  List.map (fun (site, key) => (⟨apply site.id⟩, key)) table

/-- Renumber every site in a key table without changing any persistent key or entry order. -/
def SiteKeyTable.renumber (r : SiteRenumbering) (table : SiteKeyTable) : SiteKeyTable :=
  table.renumberSites r.apply

/-- Distinct table entries have distinct lexical sites. -/
def SiteKeyTable.NoDupSites (table : SiteKeyTable) : Prop := (List.map Prod.fst table).Nodup

instance (table : SiteKeyTable) : Decidable table.NoDupSites := by
  unfold SiteKeyTable.NoDupSites
  infer_instance

/-- Obligation (discharged by elaboration, slice R-0492). Editing outside `edited` leaves the
complete sequence of persistent keys of every other function unchanged. Lexical sites may be
renumbered independently; persistent keys may not be rewritten by the edit. -/
structure OutsideFunctionEdit (before after : FnId → SiteKeyTable) (edited : FnId) : Prop where
  keysUnchanged : ∀ f, f ≠ edited → List.map Prod.snd (after f) = List.map Prod.snd (before f)

/-- A loop label (Q9.3): `break L` and `continue L` name the loop they leave. -/
structure LoopId where
  id : Nat
  deriving DecidableEq, Repr

/-- An allocator brand (Q1.1): a compile-time name for one allocator, erased at run time. -/
structure BrandId where
  id : Nat
  deriving DecidableEq, Repr

/-- An allocator lineage: an allocator and every allocator a reset of it produces. Sized
allocation is accounted per lineage (the memory decision of 2026-11-06). -/
structure LineageId where
  id : Nat
  deriving DecidableEq, Repr

/-- A loan: one open borrow, from its opening to the end of its block. -/
structure LoanId where
  id : Nat
  deriving DecidableEq, Repr

/-- A grant parameter of `main` (the authority rules, Q5.1–Q5.4). -/
structure GrantId where
  id : Nat
  deriving DecidableEq, Repr

/-- A block, the scope of a `defer` and of a borrow (Q3.7). -/
structure BlockId where
  id : Nat
  deriving DecidableEq, Repr

/-- A capability variable, a parameter of a function's `with(...)` (Q5.1–Q5.4). -/
structure CapVarId where
  id : Nat
  deriving DecidableEq, Repr

/-- A theorem a `#[proof(...)]` attribute names. It is resolved to the theorem's Lean declaration
(rule 13), never matched as text. -/
structure TheoremId where
  name : Lean.Name
  deriving DecidableEq, Repr

/-- Source text as its actual bytes, before any Unicode-scalar interpretation. -/
abbrev SourceText := List UInt8

/-- The 1-based line at a byte offset: one plus the number of newline bytes before it. -/
def SourceText.lineAt (source : SourceText) (offset : Nat) : Nat :=
  (source.take offset).count 10 + 1

/-- The 1-based byte column at an offset: one plus the number of bytes since the last newline.
Every byte advances one column; in particular a tab (`9`) counts one byte, as decided by foundation
question 10 (2026-11-08). -/
def SourceText.columnAt (source : SourceText) (offset : Nat) : Nat :=
  ((source.take offset).reverse.takeWhile (fun byte => byte != 10)).length + 1

/-- A source range and the start position used by the trap-line decision (R-0496). The path is
relative to the project root with forward slashes; `startOffset` and `endOffset` are byte offsets
with `startOffset ≤ endOffset`. Line and column are 1-based and describe the start. They are stored
for diagnostics and rendering, while `Span.LocationOk` checks them against the actual source bytes.
The path constraint is `Span.PathOk`; elaboration's normalized-path obligation remains O1. -/
structure Span where
  path : String
  startOffset : Nat
  endOffset : Nat
  range_ok : startOffset ≤ endOffset
  line : Nat
  col : Nat
  line_pos : 0 < line
  col_pos : 0 < col
  deriving DecidableEq, Repr

/-- A span's range is in bounds and its stored start line and byte column are exactly those
determined by its start offset in the source bytes. -/
def Span.LocationOk (sp : Span) (source : SourceText) : Prop :=
  sp.endOffset ≤ source.length ∧ sp.line = source.lineAt sp.startOffset ∧
    sp.col = source.columnAt sp.startOffset

instance (sp : Span) (source : SourceText) : Decidable (sp.LocationOk source) := by
  unfold Span.LocationOk
  infer_instance

/-- A span's path is relative to the project root and uses forward slashes: it is not empty,
does not start with `/`, and holds no `\`. This is the minimal syntactic check; that the path is
the one normalized path of its file (no drive prefix such as `C:`, no `.` or `..` segment, no
empty segment) is part of elaboration's site-table obligation (README obligation O1). -/
def Span.PathOk (sp : Span) : Prop :=
  sp.path.toList ≠ [] ∧ sp.path.toList.head? ≠ some '/' ∧ '\\' ∉ sp.path.toList

instance (sp : Span) : Decidable sp.PathOk := by unfold Span.PathOk; infer_instance

/-- A slice of the language, in the agreed slice order: 0 to 28, concurrency last (2026-11-07). -/
structure SliceId where
  n : Fin 29
  deriving DecidableEq, Repr

/-- A rule's identity: the Lean name of the declaration that states it and that rule's own
revision, bumped whenever its meaning changes, including through a definition it depends on
(R-0593, R-0603). -/
structure RuleId where
  name : Lean.Name
  rev : Nat
  deriving DecidableEq, Repr

/-- A version of this specification: `0.x` before the freeze, `1.0` at the freeze (A2c), bumped
by every normative change (R-0603). -/
structure SpecVersion where
  major : Nat
  minor : Nat
  deriving DecidableEq, Repr

/-- The version every output carries, in one header covering all its records. -/
def specVersion : SpecVersion := ⟨0, 1⟩

/-- A premise record as stored in the versioned evidence schema. -/
structure PremiseRecord where
  kind : PremiseKind
  scope : PremiseScope
  spec : SpecVersion
  deriving DecidableEq, Repr

/-- The serialized premise shape. A numeric code can be unknown to an old reader; it is not an
open semantic category and cannot bypass the closed `PremiseKind` enumeration. -/
structure EncodedPremise where
  category : Nat
  scope : PremiseScope
  spec : SpecVersion
  deriving DecidableEq, Repr

/-- Serialize a premise without erasing its writer's specification version. -/
def PremiseRecord.encode (record : PremiseRecord) : EncodedPremise :=
  ⟨record.kind.encode, record.scope, record.spec⟩

/-- Decode only categories known at the reader's version. Cross-version compatibility is
fail-closed until a later decided schema row defines it. -/
def PremiseKind.decode (reader : SpecVersion) (category : Nat) : Option PremiseKind :=
  if reader = specVersion then
    match category with
    | 0 => some .target
    | 1 => some .heap
    | 2 => some .stack
    | 3 => some .foreignFunction
    | 4 => some .trustedSurface
    | 5 => some .entropy
    | 6 => some .assume
    | _ => none
  else none

/-- Read a versioned premise, refusing unknown categories and versions instead of assigning a
catch-all meaning. -/
def EncodedPremise.read (reader : SpecVersion) (record : EncodedPremise) : Option PremiseRecord :=
  if record.spec = reader then
    match PremiseKind.decode reader record.category with
    | some kind => some ⟨kind, record.scope, record.spec⟩
    | none => none
  else none

/-! ### Examples -/

/-- Two declarations named `Pair` in different modules are different declarations. -/
example : DeclId.mk (ModuleId.mk ⟨"p", ⟨"1"⟩⟩ ["a"] (by decide)) "Pair" ≠
    DeclId.mk (ModuleId.mk ⟨"p", ⟨"1"⟩⟩ ["b"] (by decide)) "Pair" := by decide

/-- The same module path in two packages also yields different declarations. -/
example : DeclId.mk (ModuleId.mk ⟨"p1", ⟨"1"⟩⟩ ["a"] (by decide)) "Pair" ≠
    DeclId.mk (ModuleId.mk ⟨"p2", ⟨"1"⟩⟩ ["a"] (by decide)) "Pair" := by decide

/-- A field belongs to its owner: `a` of `Pair` is not `a` of `Point`, and `value` of variant
`E.A` is not `value` of variant `E.B`. -/
example : FieldId.mk (.struct ⟨ModuleId.mk ⟨"p", ⟨"1"⟩⟩ ["m"] (by decide), "Pair"⟩) "a" ≠
    FieldId.mk (.struct ⟨ModuleId.mk ⟨"p", ⟨"1"⟩⟩ ["m"] (by decide), "Point"⟩) "a" := by decide

example : FieldId.mk (.variant ⟨⟨ModuleId.mk ⟨"p", ⟨"1"⟩⟩ ["m"] (by decide), "E"⟩, "A"⟩) "value" ≠
    FieldId.mk (.variant ⟨⟨ModuleId.mk ⟨"p", ⟨"1"⟩⟩ ["m"] (by decide), "E"⟩, "B"⟩) "value" := by decide

/-- The first and the last slice. -/
example : (SliceId.mk 0).n.val = 0 ∧ (SliceId.mk 28).n.val = 28 := ⟨rfl, rfl⟩

/-- A span path is relative, with forward slashes. -/
example : (Span.mk "src/main.con" 0 0 (by decide) 3 5 (by decide) (by decide)).PathOk := by decide

example : ¬ (Span.mk "/abs/main.con" 0 0 (by decide) 3 5 (by decide) (by decide)).PathOk := by decide

example : ¬ (Span.mk "src\\main.con" 0 0 (by decide) 3 5 (by decide) (by decide)).PathOk := by decide

/-- Bytes for `a`, a tab, and `b`. After the tab, the range starts at line 1, byte column 3. -/
def tabSource : SourceText := [97, 9, 98]
def tabSpan : Span := ⟨"src/main.con", 2, 3, by decide, 1, 3, by decide, by decide⟩

/-- Accepted: the tab advances exactly one byte column. -/
example : tabSpan.LocationOk tabSource := by decide

/-- Refused: treating the tab as zero-width does not satisfy the location relation. -/
example : ¬ ({ tabSpan with col := 2, col_pos := by decide }.LocationOk tabSource) := by decide

/-- Two locals named `x` that elaboration numbered differently (in two separate blocks, say) are
different locals. -/
example : Var.mk "x" 0 ≠ Var.mk "x" 1 := by decide

/-- Hints can distinguish two raw records for diagnostics, but they are not semantic names:
elaboration never gives two locals of one function one number, and evaluation is invariant under
consistent rehinting. -/
example : Var.mk "x" 0 ≠ Var.mk "y" 0 := by decide

/-- Places, primitives and contract revisions have distinct typed identities. -/
example : PlaceId.mk 0 ≠ PlaceId.mk 1 ∧ PrimId.mk 0 ≠ PrimId.mk 1 ∧
    ContractId.mk ⟨0⟩ 1 ≠ ContractId.mk ⟨0⟩ 2 := by decide

/-- Two obligations at one site differ by kind; two of one kind differ by their local discriminator. -/
example : ObligationId.mk ⟨0⟩ .safety 0 ≠ ObligationId.mk ⟨0⟩ .termination 0 ∧
    ObligationId.mk ⟨0⟩ .safety 0 ≠ ObligationId.mk ⟨0⟩ .safety 1 := by decide

/-- A revised rule is a different rule identity. -/
example : RuleId.mk `ConcreteSpec.Core.example 1 ≠ RuleId.mk `ConcreteSpec.Core.example 2 := by
  decide

/-- Before the freeze the version is `0.x`. -/
example : specVersion.major = 0 := rfl

end ConcreteSpec.Core
