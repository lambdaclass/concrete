import ConcreteSpec.Spec.Basics

/-!
# Identities, sites and versions

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). Part of `ConcreteSpec.Spec`;
see `ConcreteSpec/README.md`.

Core names everything by a typed identity, never by a bare string or number: locals, declarations,
constants, fields, functions, packages, modules, trait instances, specializations, loop labels,
brands, allocator lineages, loans, grants, blocks, capability variables, theorems, obligations and
premises each have their own type. A module contains its package and non-empty package-rooted path,
so a declaration, field, variant, function or constant carries that complete identity. A local uses Basics’ numeric `ConcreteSpec.Var`; spellings live only in the diagnostics side
table. Equality of identities is what the rules use (the facts decision: no bare-name collisions).

A *site* (`SiteId`) is one per operation and per refusable statement. Diagnostics, facts, traps
and discharge tokens are keyed by site. Source spans carry checked local path identities.
Lexical site and trait-instance numbers are distinct from persistent `StableKey`s. Specialization request edges retain their sites
separately from identities keyed by generic arguments. Obligations carry site, kind and a same-site
discriminator; conditional facts carry typed premise identities.

A rule is cited by `RuleId`: the Lean name of the declaration that states it and that rule's own
revision. Every output carries one `specVersion`. The rule registry is a later piece;
this module depends on nothing downstream.

These types are part of the diagnostics and facts interface: the Lean types are the source of its
JSON schema (owner decision 2026-11-07), so a field's name is part of the interface and renaming
one is an interface change.

Authority (all in `docs/decisions.md` on main):
- `decided-project-facts-report`: typed identities and conditional evidence;
  Q9.1–Q9.3, Q9.6–Q9.8: fresh locals, nominal declarations, loops and primitives;
  OD77/OD148: no generic trait-method/trait-parameter category; OD109: capability-sensitive specialization.
- `decided-2026-11-08-c9-identities`: structured stable keys, package name/version,
  origin collision refusal, closed versioned premise categories; OD44: top-level collisions.
- `decided-project-concrete-toml`, `decided-2026-11-07-stack-randomness`, OD80:
  the seven premise categories. Numeric wire tags are this schema's closed encoding.
- Q1.1, Q3.7, Q5.1–Q5.4, `decided-project-memory-2026-11-06`:
  brands, blocks, loans, grants, capability variables and allocator lineages.
- `decided-project-diagnostics-2026-11-06`, `decided-project-runtime-2026-11-06`:
  rule revisions, spec versions and source positions; `foundation-q10`: byte columns,
  with a tab counting one byte. Byte arithmetic is not a lexer or source-admission policy.
- `decided-project-vertical-slices-2026-11-06`: slice identities 0–28.

All identity text uses Basics.Name, preserving exact text equality while
keeping nominal identity distinct from local diagnostic spellings. Numeric routes
use Basics.Numbers; neither wrapper adds source-path or normalization policy.

The key-assignment helpers implement the settled structured-key interface, not a source parser.
Local and project-wide path admission are defined below. Span projection and source binding
remain separate obligations.
-/

namespace ConcreteSpec.Core

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
  token : Name
  deriving DecidableEq, Repr

/-- A package identity is its name and version. Artifact hashes and load origins are separate. -/
structure PackageId where
  name : Name
  version : PackageVersion
  deriving DecidableEq, Repr

/-- Where a loaded package came from. Origin is deliberately outside `PackageId`: equal name and
version from different origins are a load-time collision, not two package identities. -/
structure PackageOrigin where
  token : Name
  deriving DecidableEq, Repr

/-- A module: its package and its non-empty path from that package's root (`[⟨"std"⟩, ⟨"bytes"⟩]`).
The first segment is therefore always a real top-level module name. -/
structure ModuleId where
  package : PackageId
  path : List Name
  path_nonempty : path ≠ []
  deriving DecidableEq, Repr

/-- The module's required top-level name. -/
def ModuleId.topLevel (m : ModuleId) : Name := m.path.head m.path_nonempty

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
  name : Name
  deriving DecidableEq, Repr

/-- A variant of an enum declaration, by its declaration and name. -/
structure VariantId where
  decl : DeclId
  name : Name
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
  name : Name
  deriving DecidableEq, Repr

/-- A function: its module and its name there. -/
structure FnId where
  module : ModuleId
  name : Name
  deriving DecidableEq, Repr

/-- A module constant (`const N: T = e;`): its module and its name there. -/
structure ConstId where
  module : ModuleId
  name : Name
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
  | named (name : Name)
  | unnamed (label : Option Name)
  deriving DecidableEq, Repr

/-- A named segment is keyed by its source item name; an unnamed segment with a user label by its
pinned label; an unlabelled unnamed segment by its per-kind ordinal within the enclosing named
scope. -/
inductive SegmentName where
  | named (name : Name)
  | pinned (label : Name)
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

/-- Rename a named construct without changing named/unnamed status. Unnamed constructs have no item
name to rename; pinning or unpinning an unnamed construct's user label is `ConstructName.pin`. -/
def ConstructName.rename : ConstructName → Name → ConstructName
  | .named _, name => .named name
  | .unnamed label, _ => .unnamed label

/-- Pin or unpin an unnamed construct's user label without changing its unnamed status, so
siblings' ordinals cannot change. Named constructs are unchanged. -/
def ConstructName.pin : ConstructName → Option Name → ConstructName
  | .named name, _ => .named name
  | .unnamed _, label => .unnamed label

/-- Rename one named construct root, retaining its kind and complete subtree. -/
def KeyConstruct.rename (construct : KeyConstruct) (name : Name) : KeyConstruct :=
  { construct with name := construct.name.rename name }

/-- Pin or unpin one unnamed construct root's label, retaining its kind and complete subtree. -/
def KeyConstruct.pin (construct : KeyConstruct) (label : Option Name) : KeyConstruct :=
  { construct with name := construct.name.pin label }

/-- The persistent segment assigned among these preceding constructs in the enclosing named scope.
Named constructs do not consume unnamed ordinals; ordinals are per construct kind, and pinned labels
replace ordinals in the emitted segment while still participating in the unnamed ordinal sequence. -/
def KeyConstruct.segment (preceding : List KeyConstruct) (construct : KeyConstruct) : KeySegment :=
  let ordinal := (preceding.filter fun sibling =>
    sibling.kind == construct.kind && sibling.name.isUnnamed).length
  ⟨construct.kind, match construct.name with
    | .named name => .named name
    | .unnamed (some label) => .pinned label
    | .unnamed none => .ordinal ordinal⟩

/-- The key of a construct root under its preceding constructs in the enclosing named scope. -/
def KeyConstruct.key (preceding : List KeyConstruct) (construct : KeyConstruct) : FunctionRelativeKey :=
  ⟨[construct.segment preceding]⟩

/-- The enclosing named scope passed to a construct's children: a `.named` construct establishes a
fresh named scope (`[]`), whereas an `.unnamed` construct retains the enclosing named scope's
preceding constructs (`localPreceding`) (docs/decisions.md#decided-2026-11-08-c9-identities). -/
def KeyConstruct.childScope (localPreceding : List KeyConstruct) (construct : KeyConstruct) :
    List KeyConstruct :=
  match construct.name with
  | .named _ => []
  | .unnamed _ => localPreceding

/-- Assign the complete persistent path to an occurrence in a construct forest within an inherited
unnamed-ancestor scope `scope`. The numeric route is only a locator in this particular syntax tree;
no child index enters the assigned key. Every level counts its per-kind unnamed ordinal against
`scope ++ forest.take index` and resets the child scope to `[]` only when entering a `.named`
construct. -/
def KeyConstruct.pathAtIn (scope : List KeyConstruct) (forest : List KeyConstruct) :
    Numbers → Option FunctionRelativeKey
  | [] => some ⟨[]⟩
  | index :: route => do
    let construct ← forest[index]?
    let localPreceding := scope ++ forest.take index
    let descendant ← KeyConstruct.pathAtIn (construct.childScope localPreceding) construct.children route
    pure ⟨construct.segment localPreceding :: descendant.path⟩

/-- Assign the complete persistent path to an occurrence from the root of an enclosing item's
construct forest. -/
def KeyConstruct.pathAt (forest : List KeyConstruct) (route : Numbers) :
    Option FunctionRelativeKey :=
  KeyConstruct.pathAtIn [] forest route

/-- Assign a descendant path beneath a located root with preceding constructs `preceding` in the
enclosing named scope. -/
def KeyConstruct.descendantKey (preceding : List KeyConstruct) (construct : KeyConstruct)
    (route : Numbers) : Option FunctionRelativeKey :=
  (KeyConstruct.pathAtIn (construct.childScope preceding) construct.children route).map
    fun descendant => ⟨construct.segment preceding :: descendant.path⟩

/-- Check that every construct encountered along `route` in `forest` within the current enclosing
named scope has kind different from `k`. Entering a `.named` construct starts a new named scope and
needs no further check on `k`. -/
def KeyConstruct.routeAvoidsKind (k : ConstructKind) : List KeyConstruct → Numbers → Bool
  | _, [] => true
  | forest, index :: route =>
    match forest[index]? with
    | none => true
    | some construct =>
      (k != construct.kind) &&
        match construct.name with
        | .named _ => true
        | .unnamed _ => KeyConstruct.routeAvoidsKind k construct.children route

/-- Every construct encountered along `route` in `forest` within the current enclosing named scope
has kind different from `k`. -/
def KeyConstruct.RouteAvoidsKind (k : ConstructKind) (forest : List KeyConstruct)
    (route : Numbers) : Prop :=
  KeyConstruct.routeAvoidsKind k forest route = true

instance (k : ConstructKind) (forest : List KeyConstruct) (route : Numbers) :
    Decidable (KeyConstruct.RouteAvoidsKind k forest route) := by
  unfold KeyConstruct.RouteAvoidsKind
  infer_instance

/-- Check that every construct encountered from `target` along `route` within `target`'s enclosing
named scope has kind different from `k`. -/
def KeyConstruct.subtreeAvoidsKind (k : ConstructKind) (target : KeyConstruct)
    (route : Numbers) : Bool :=
  (k != target.kind) &&
    match target.name with
    | .named _ => true
    | .unnamed _ => KeyConstruct.routeAvoidsKind k target.children route

/-- Every construct encountered from `target` along `route` within `target`'s enclosing named scope
has kind different from `k`. -/
def KeyConstruct.SubtreeAvoidsKind (k : ConstructKind) (target : KeyConstruct)
    (route : Numbers) : Prop :=
  KeyConstruct.subtreeAvoidsKind k target route = true

instance (k : ConstructKind) (target : KeyConstruct) (route : Numbers) :
    Decidable (KeyConstruct.SubtreeAvoidsKind k target route) := by
  unfold KeyConstruct.SubtreeAvoidsKind
  infer_instance

/-- Collect the persistent function-relative keys of a construct forest in order within the
inherited unnamed-ancestor scope `scope`. -/
def KeyConstruct.collectKeysIn (scope : List KeyConstruct) :
    List KeyConstruct → List FunctionRelativeKey
  | [] => []
  | construct :: rest =>
    construct.key scope ::
      ((KeyConstruct.collectKeysIn (construct.childScope scope) construct.children).map
        (fun key => ⟨construct.segment scope :: key.path⟩) ++
       KeyConstruct.collectKeysIn (scope ++ [construct]) rest)
termination_by forest => sizeOf forest
decreasing_by
  all_goals simp_wf
  all_goals cases construct <;> simp_all <;> omega

/-- A whole-collection output contract: every returned key names an actual nonempty
occurrence in the input forest. The occurrence resolver supplies the independent semantics;
this predicate neither repeats the collector's recursion nor checks an admission guard. -/
def KeyConstruct.CollectionSound (scope forest : List KeyConstruct)
    (keys : List FunctionRelativeKey) : Prop :=
  ∀ key, key ∈ keys →
    ∃ route, route ≠ [] ∧ KeyConstruct.pathAtIn scope forest route = some key

/-- Collect the persistent function-relative keys of a function body's construct forest in order. -/
def KeyConstruct.collectKeys (body : List KeyConstruct) : List FunctionRelativeKey :=
  KeyConstruct.collectKeysIn [] body

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
  token : Name
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

/-- Pair a sequence of function-relative keys with sequential lexical `SiteId`s starting at
`startSite` under owner `StableOwner.function f`. -/
def KeyConstruct.numberFunctionKeys (f : FnId) :
    Nat → List FunctionRelativeKey → SiteKeyTable
  | _, [] => []
  | nextSite, rel :: rest =>
    (⟨nextSite⟩, ⟨.function f, rel⟩) :: KeyConstruct.numberFunctionKeys f (nextSite + 1) rest

/-- Assign a function's `SiteKeyTable` from its `FnId`, its starting lexical site number, and its
construct forest. -/
def KeyConstruct.assignFunctionTable (f : FnId) (startSite : Nat)
    (body : List KeyConstruct) : SiteKeyTable :=
  KeyConstruct.numberFunctionKeys f startSite (KeyConstruct.collectKeys body)

/-- Assign `SiteKeyTable`s to every function in a program from a function-body map and a per-function
starting lexical site offset. Editing one function can shift the starting lexical site offsets of
other functions without changing their construct bodies or persistent keys. -/
def KeyConstruct.assignProgramTables (bodies : FnId → List KeyConstruct)
    (startSite : FnId → Nat) (f : FnId) : SiteKeyTable :=
  KeyConstruct.assignFunctionTable f (startSite f) (bodies f)

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

/-- The outside-function edit obligation. Editing outside `edited` leaves the
complete sequence of persistent keys of every other function unchanged. Lexical sites may be
renumbered independently; persistent keys may not be rewritten by the edit. -/
def OutsideFunctionEdit (before after : FnId → SiteKeyTable) (edited : FnId) : Prop :=
  ∀ f, f ≠ edited → List.map Prod.snd (after f) = List.map Prod.snd (before f)

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

/-- The shared carrier for loaded source text and raw path bytes. Each number records one
byte in input order, including bytes of ill-formed UTF-8. The bound makes conversion to
a host byte exact. This common representation lets path admission inspect the original bytes,
as required by docs/decisions.md#decided-2026-11-16-file-paths. Source coordinates use
the same bytes under docs/decisions.md#foundation-q10. -/
structure SourceText where
  bytes : ConcreteSpec.Numbers
  byte_lt : ∀ byte ∈ bytes, byte < 256
  deriving DecidableEq, Repr

/-- Store each input byte as its exact numeric value, preserving order. -/
def SourceText.ofBytes (bytes : List UInt8) : SourceText where
  bytes := bytes.map UInt8.toNat
  byte_lt := by
    intro byte member
    obtain ⟨original, _, rfl⟩ := List.mem_map.mp member
    exact original.toNat_lt_size

/-- Recover the bytes in their original order. -/
def SourceText.toBytes (source : SourceText) : List UInt8 :=
  source.bytes.map UInt8.ofNat

/-- The length in bytes, including every byte of a multibyte sequence. -/
def SourceText.length (source : SourceText) : Nat :=
  source.bytes.length

/-- Add one byte at the end of the source. -/
def SourceText.push (source : SourceText) (byte : UInt8) : SourceText where
  bytes := source.bytes ++ [byte.toNat]
  byte_lt := by
    intro value member
    rcases List.mem_append.mp member with original | last
    · exact source.byte_lt value original
    · rw [List.mem_singleton.mp last]
      exact byte.toNat_lt_size

/-- The 1-based line at a byte offset: one plus the number of newline bytes before it. -/
def SourceText.lineAt (source : SourceText) (offset : Nat) : Nat :=
  (source.bytes.take offset).count 10 + 1

/-- The 1-based byte column at an offset: one plus the number of bytes since the last newline.
Every non-newline byte advances one column, including tab (`9`) and each byte of a multibyte sequence
(docs/decisions.md#foundation-q10). -/
def SourceText.columnAt (source : SourceText) (offset : Nat) : Nat :=
  ((source.bytes.take offset).reverse.takeWhile (fun byte => byte != 10)).length + 1

namespace SourcePath

/-- Maximum number of bytes in a single component. -/
def componentLimit : Nat := 100

/-- Maximum number of bytes in the full path, including separators. -/
def pathLimit : Nat := 200

/-- One reason per local check. `Reason.inOrder` determines the diagnostic tie-break order. -/
inductive Reason where
  | empty | absolute | drive | traversal | singleDot | emptyComponent
  | nonAscii | backslash | control | space | leadingDot | trailingDot
  | reserved | componentCap | pathCap | disallowed
  deriving DecidableEq, Repr

/-- Local checks in their fixed tie-break order. An earlier byte always takes precedence. -/
def Reason.inOrder : List Reason :=
  [.empty, .absolute, .drive, .traversal, .singleDot, .emptyComponent, .nonAscii, .backslash,
   .control, .space, .leadingDot, .trailingDot, .reserved, .componentCap, .pathCap, .disallowed]

/-- A half-open component range, with the byte or boundary selected within it. -/
structure Location (length : Nat) where
  start : Nat
  stop : Nat
  offset : Nat
  start_le : start ≤ offset
  offset_le : offset ≤ stop
  stop_le : stop ≤ length
  deriving DecidableEq, Repr

/-- A derived component view of the path bytes. The separator is outside the range. -/
structure Component (length : Nat) where
  start : Nat
  bytes : ConcreteSpec.Numbers
  stop_le : start + bytes.length ≤ length

/-- Split at every slash and emit a component at EOF, including an empty final component. -/
def scan {length : Nat} (start : Nat) (rev : ConcreteSpec.Numbers) :
    (rest : ConcreteSpec.Numbers) → start + rev.length + rest.length = length →
      List (Component length)
  | [], total => [⟨start, rev.reverse, by simpa using Nat.le_of_eq total⟩]
  | byte :: rest, total =>
    if byte = 47 then
      ⟨start, rev.reverse, by simp at total ⊢; omega⟩ ::
        scan (start + rev.length + 1) [] rest (by simp at total ⊢; omega)
    else
      scan start (byte :: rev) rest (by simp at total ⊢; omega)

/-- The exact components in byte order. -/
def components (raw : SourceText) : List (Component raw.length) :=
  scan 0 [] raw.bytes (by simp [SourceText.length])

/-- The same ASCII fold serves reserved stems and project comparison keys. -/
def foldAscii (byte : Nat) : Nat :=
  if 65 ≤ byte ∧ byte ≤ 90 then byte + 32 else byte

/-- Test for one of the 52 ASCII letters. -/
def asciiLetter (byte : Nat) : Bool :=
  (65 ≤ byte && byte ≤ 90) || (97 ≤ byte && byte ≤ 122)

/-- Test the ASCII filename whitelist before component-level exclusions. -/
def filenameByte (byte : Nat) : Bool :=
  asciiLetter byte || (48 ≤ byte && byte ≤ 57) || byte == 95 || byte == 45 || byte == 46

/-- Match the stem before the first dot using a supplied byte fold and lower digit bound. -/
def reservedStemWith (fold : Nat → Nat) (firstDigit : Nat) (bytes : ConcreteSpec.Numbers) : Bool :=
  match (bytes.takeWhile (· != 46)).map fold with
  | [99, 111, 110] | [112, 114, 110] | [97, 117, 120] | [110, 117, 108] => true
  | [99, 111, 109, digit] | [108, 112, 116, digit] => firstDigit ≤ digit && digit ≤ 57
  | _ => false

/-- Match a reserved stem using the production case fold and digit range. -/
def reservedStem := reservedStemWith foldAscii 49

/-- The local predicates and limits used by the diagnostic search. -/
structure ProbePolicy where
  empty : ConcreteSpec.Numbers → Bool
  absolute : ConcreteSpec.Numbers → Bool
  drive : Nat → Nat → Bool
  traversal : ConcreteSpec.Numbers → Bool
  singleDot : ConcreteSpec.Numbers → Bool
  emptyComponent : ConcreteSpec.Numbers → Bool
  nonAscii : Nat → Bool
  backslash : Nat → Bool
  control : Nat → Bool
  space : Nat → Bool
  leadingDot : Nat → Bool
  trailingDot : Nat → Bool
  reserved : ConcreteSpec.Numbers → Bool
  componentCap : Nat
  pathCap : Nat
  disallowed : Nat → Bool
  findByte : Reason → (bytes : ConcreteSpec.Numbers) → (Nat → Bool) → Option (Fin bytes.length)

/-- The production policy for project-relative source paths. -/
def localPolicy : ProbePolicy where
  empty := List.isEmpty
  absolute := fun bytes => bytes.head? == some 47
  drive := fun letter next => asciiLetter letter && next == 58
  traversal := (· == [46, 46])
  singleDot := (· == [46])
  emptyComponent := List.isEmpty
  nonAscii := (128 ≤ ·)
  backslash := (· == 92)
  control := fun byte => byte < 32 || byte == 127
  space := (· == 32)
  leadingDot := (· == 46)
  trailingDot := (· == 46)
  reserved := reservedStem
  componentCap := componentLimit
  pathCap := pathLimit
  disallowed := fun byte => 32 < byte && byte < 127 && byte != 92 && !filenameByte byte
  findByte := fun _ bytes test => bytes.findFinIdx? test

/-- Translate a component-relative byte or boundary into whole-path coordinates. -/
def componentLocation {length : Nat} (c : Component length)
    (relative : Nat) (bound : relative ≤ c.bytes.length) : Location length :=
  ⟨c.start, c.start + c.bytes.length, c.start + relative, by omega,
    Nat.add_le_add_left bound _, c.stop_le⟩

/-- Locate the match selected by a byte probe in whole-path coordinates. -/
def firstByte {length : Nat} (policy : ProbePolicy) (reason : Reason) (c : Component length)
    (test : Nat → Bool) : Option (Location length) :=
  (policy.findByte reason c.bytes test).map fun (i : Fin c.bytes.length) => componentLocation c i (Nat.le_of_lt i.isLt)

/-- Locate a rejected endpoint, leaving the special dot components to their own probes. -/
def endpointDot {length : Nat} (c : Component length) (leading : Bool)
    (test : Nat → Bool) : Option (Location length) :=
  if c.bytes == [46] || c.bytes == [46, 46] then none
  else if (if leading then c.bytes.head? else c.bytes.getLast?).any test then
    some (componentLocation c (if leading then 0 else c.bytes.length - 1) (by split <;> omega))
  else none

/-- Locate a component-wide violation at the start of its component. -/
def startIf {length : Nat} (c : Component length) (found : Bool) : Option (Location length) :=
  if found then some (componentLocation c 0 (Nat.zero_le _)) else none

/-- Select the first witness of a given check in a component. -/
def checkComponentWith (policy : ProbePolicy) {length : Nat} (reason : Reason)
    (c : Component length) : Option (Location length) :=
  match reason with
  | .drive =>
    if size : 1 < c.bytes.length then
      if c.start == 0 && policy.drive (c.bytes[0]'(by omega)) c.bytes[1] then
        some (componentLocation c 1 (Nat.le_of_lt size))
      else none
    else none
  | .traversal => startIf c (policy.traversal c.bytes)
  | .singleDot => startIf c (policy.singleDot c.bytes)
  | .emptyComponent => startIf c (policy.emptyComponent c.bytes)
  | .nonAscii => firstByte policy .nonAscii c policy.nonAscii
  | .backslash => firstByte policy .backslash c policy.backslash
  | .control => firstByte policy .control c policy.control
  | .space => firstByte policy .space c policy.space
  | .leadingDot => endpointDot c true policy.leadingDot
  | .trailingDot => endpointDot c false policy.trailingDot
  | .reserved => startIf c (policy.reserved c.bytes)
  | .componentCap =>
    if h : policy.componentCap < c.bytes.length then
      some (componentLocation c policy.componentCap (Nat.le_of_lt h))
    else none
  | .disallowed => firstByte policy .disallowed c policy.disallowed
  | .empty | .absolute | .pathCap => none

/-- The production component check. -/
def checkComponent := @checkComponentWith localPolicy

/-- Locate a violation of a whole-path rule. -/
def wholeLocation (raw : SourceText) (offset : Nat) (bound : offset ≤ raw.length) :
    Location raw.length :=
  ⟨0, raw.length, offset, Nat.zero_le _, bound, Nat.le_refl _⟩

/-- One rule probe for the whole path, or one per component for a component rule. -/
def probesWith (policy : ProbePolicy) (raw : SourceText) (parts : List (Component raw.length))
    (reason : Reason) : List (Option (Location raw.length)) :=
  match reason with
  | .empty => [if policy.empty raw.bytes then some (wholeLocation raw 0 (Nat.zero_le _)) else none]
  | .absolute => [if policy.absolute raw.bytes then some (wholeLocation raw 0 (Nat.zero_le _)) else none]
  | .pathCap =>
    [if h : policy.pathCap < raw.length then some (wholeLocation raw policy.pathCap (Nat.le_of_lt h)) else none]
  | other => parts.map (checkComponentWith policy other)

/-- The production probes for an individual rule. -/
def probes := probesWith localPolicy

/-- Collect the violations of a single check under the supplied policy. -/
def checkWith (policy : ProbePolicy) (raw : SourceText) (parts : List (Component raw.length))
    (reason : Reason) : List (Location raw.length) :=
  (probesWith policy raw parts reason).filterMap id

/-- Collect the production violations of a single check. -/
def check := checkWith localPolicy

/-- Each applicable probe of the rule must pass. -/
def ProbePermitted (reason : Reason) (raw : SourceText) : Prop :=
  ∀ probe ∈ probes raw (components raw) reason, probe = none

instance (reason : Reason) (raw : SourceText) : Decidable (ProbePermitted reason raw) := by
  unfold ProbePermitted
  infer_instance

/-- Helper: connect the diagnostic collection to the individual validity judgments. -/
theorem check_witness_iff (reason : Reason) (raw : SourceText) :
    check raw (components raw) reason = [] ↔ ProbePermitted reason raw := by
  simp [check, checkWith, ProbePermitted, probes]

namespace Reference

/-- Tie-break order from the local diagnostic design. Byte offsets take precedence. -/
def priority : List Reason :=
  [.empty, .absolute, .drive, .traversal, .singleDot, .emptyComponent, .nonAscii, .backslash,
   .control, .space, .leadingDot, .trailingDot, .reserved, .componentCap, .pathCap, .disallowed]

/-- Number the pieces of a slash split by their original byte offsets. -/
def locate (start : Nat) : List ConcreteSpec.Numbers → List (Nat × ConcreteSpec.Numbers)
  | [] => []
  | bytes :: rest => (start, bytes) :: locate (start + bytes.length + 1) rest

/-- The slash-delimited pieces, including any empty pieces. -/
def pieces (bytes : ConcreteSpec.Numbers) : List (Nat × ConcreteSpec.Numbers) :=
  locate 0 (bytes.splitOn 47)

/-- ASCII A–Z or a–z. -/
def letter (byte : Nat) : Bool :=
  (65 ≤ byte && byte ≤ 90) || (97 ≤ byte && byte ≤ 122)

/-- ASCII letters, digits, underscore, hyphen and dot. -/
def filename (byte : Nat) : Bool :=
  letter byte || (48 ≤ byte && byte ≤ 57) || byte == 95 || byte == 45 || byte == 46

/-- ASCII lowercase for reserved stem comparison. -/
def lower (byte : Nat) : Nat :=
  if 65 ≤ byte ∧ byte ≤ 90 then byte + 32 else byte

/-- CON, PRN, AUX, NUL, COM1–COM9 and LPT1–LPT9 before the first dot, ignoring ASCII case. -/
def reserved (bytes : ConcreteSpec.Numbers) : Bool :=
  match (bytes.takeWhile (· != 46)).map lower with
  | [99, 111, 110] | [112, 114, 110] | [97, 117, 120] | [110, 117, 108] => true
  | [99, 111, 109, digit] | [108, 112, 116, digit] => 49 ≤ digit && digit ≤ 57
  | _ => false

/-- All indices where a byte fails a byte rule, including repeated violations. -/
def byteOffsets (bytes : ConcreteSpec.Numbers) (offends : Nat → Bool) : List Nat :=
  bytes.zipIdx.filterMap fun (byte, offset) => if offends byte then some offset else none

/-- Relative offsets of all offenses in one component. Component-wide offenses use its
start, and a length offense uses the first excess byte. -/
def componentOffsets (reason : Reason) (start : Nat) (bytes : ConcreteSpec.Numbers) : List Nat :=
  match reason with
  | .drive =>
    if start == 0 && bytes[0]?.any letter && bytes[1]? == some 58 then [1] else []
  | .traversal => if bytes == [46, 46] then [0] else []
  | .singleDot => if bytes == [46] then [0] else []
  | .emptyComponent => if bytes == [] then [0] else []
  | .nonAscii => byteOffsets bytes (128 ≤ ·)
  | .backslash => byteOffsets bytes (· == 92)
  | .control => byteOffsets bytes (fun byte => byte < 32 || byte == 127)
  | .space => byteOffsets bytes (· == 32)
  | .leadingDot =>
    if bytes == [46] || bytes == [46, 46] then []
    else if bytes[0]?.any (· == 46) then [0] else []
  | .trailingDot =>
    if bytes == [46] || bytes == [46, 46] then []
    else if bytes.getLast?.any (· == 46) then [bytes.length - 1] else []
  | .reserved => if reserved bytes then [0] else []
  | .componentCap => if 100 < bytes.length then [100] else []
  | .disallowed =>
    byteOffsets bytes (fun byte => 32 < byte && byte < 127 && byte != 92 && !filename byte)
  | .empty | .absolute | .pathCap => []

/-- Absolute byte or boundary offsets for every applicable offense of a rule. -/
def offsets (reason : Reason) (raw : SourceText) : List Nat :=
  match reason with
  | .empty => if raw.bytes == [] then [0] else []
  | .absolute => if raw.bytes[0]? == some 47 then [0] else []
  | .pathCap => if 200 < raw.bytes.length then [200] else []
  | other => (pieces raw.bytes).flatMap fun (start, bytes) =>
      (componentOffsets other start bytes).map (start + ·)

/-- Lexicographic precedence on the original byte offset and then the design tie-break order. -/
def Precedes (leftReason : Reason) (leftOffset : Nat)
    (rightReason : Reason) (rightOffset : Nat) : Prop :=
  leftOffset < rightOffset ∨
    (leftOffset = rightOffset ∧ priority.idxOf leftReason ≤ priority.idxOf rightReason)

/-- An applicable offense that precedes every applicable offense of every rule. -/
def IsFirst (raw : SourceText) (reason : Reason) (offset : Nat) : Prop :=
  offset ∈ offsets reason raw ∧
    ∀ otherReason, ∀ otherOffset ∈ offsets otherReason raw,
      Precedes reason offset otherReason otherOffset

end Reference

/-- A rule permits the original byte sequence exactly when it has no reference offenses. -/
def Permitted (reason : Reason) (raw : SourceText) : Prop :=
  Reference.offsets reason raw = []

instance (reason : Reason) (raw : SourceText) : Decidable (Permitted reason raw) :=
  inferInstanceAs (Decidable (Reference.offsets reason raw = []))

/-- All sixteen local byte rules permit this input. -/
def Reference.Valid (raw : SourceText) : Prop :=
  ∀ reason ∈ Reference.priority, Permitted reason raw

instance (raw : SourceText) : Decidable (Reference.Valid raw) := by
  unfold Reference.Valid
  infer_instance

/-- The full path has at least one byte. See also
docs/decisions.md#decided-2026-11-14-trap-sites-and-law-tests.
R-0709, docs/decisions.md#decided-2026-11-16-file-paths. -/
def Nonempty (raw : SourceText) : Prop := Permitted .empty raw

/-- The first byte is not a slash. See also
docs/decisions.md#decided-2026-11-14-trap-sites-and-law-tests.
R-0709, docs/decisions.md#decided-2026-11-16-file-paths. -/
def Relative (raw : SourceText) : Prop := Permitted .absolute raw

/-- An initial ASCII letter followed by a colon is forbidden.
R-0709, docs/decisions.md#decided-2026-11-16-file-paths. -/
def NoDrivePrefix (raw : SourceText) : Prop := Permitted .drive raw

/-- A parent component is forbidden. See also
docs/decisions.md#decided-2026-11-14-trap-sites-and-law-tests.
R-0709, docs/decisions.md#decided-2026-11-16-file-paths. -/
def NoTraversal (raw : SourceText) : Prop := Permitted .traversal raw

/-- A single-dot component is forbidden.
R-0709, docs/decisions.md#decided-2026-11-16-file-paths. -/
def NoSingleDot (raw : SourceText) : Prop := Permitted .singleDot raw

/-- Each slash-delimited component has at least one byte.
R-0709, docs/decisions.md#decided-2026-11-16-file-paths. -/
def NonemptyComponents (raw : SourceText) : Prop := Permitted .emptyComponent raw

/-- Every byte is ASCII, including when the input is ill-formed UTF-8.
R-0709, docs/decisions.md#decided-2026-11-16-file-paths. -/
def Ascii (raw : SourceText) : Prop := Permitted .nonAscii raw

/-- Backslash is forbidden. Slash is the sole separator.
R-0709, docs/decisions.md#decided-2026-11-16-file-paths. -/
def SlashSeparator (raw : SourceText) : Prop := Permitted .backslash raw

/-- Bytes below 32 and DEL are forbidden.
R-0709, docs/decisions.md#decided-2026-11-16-file-paths. -/
def NoControls (raw : SourceText) : Prop := Permitted .control raw

/-- ASCII space is forbidden.
R-0709, docs/decisions.md#decided-2026-11-16-file-paths. -/
def NoSpaces (raw : SourceText) : Prop := Permitted .space raw

/-- A component cannot start with a dot. Single dot and parent traversal have separate checks.
R-0709, docs/decisions.md#decided-2026-11-16-file-paths. -/
def NoLeadingDot (raw : SourceText) : Prop := Permitted .leadingDot raw

/-- A component cannot end with a dot. Single dot and parent traversal have separate checks.
R-0709, docs/decisions.md#decided-2026-11-16-file-paths. -/
def NoTrailingDot (raw : SourceText) : Prop := Permitted .trailingDot raw

/-- Windows reserved stems are forbidden, ignoring ASCII case and inspecting only the bytes before the first dot.
R-0709, docs/decisions.md#decided-2026-11-17-file-names. -/
def NoReservedStem (raw : SourceText) : Prop := Permitted .reserved raw

/-- Each component is at most 100 bytes.
R-0709, docs/decisions.md#decided-2026-11-16-file-paths. -/
def ComponentByteCap (raw : SourceText) : Prop := Permitted .componentCap raw

/-- The whole path is at most 200 bytes, counting slashes.
R-0709, docs/decisions.md#decided-2026-11-16-file-paths. -/
def PathByteCap (raw : SourceText) : Prop := Permitted .pathCap raw

/-- Component bytes admit exactly ASCII letters, digits, underscore, hyphen and dot, subject to the dedicated character checks.
R-0709, docs/decisions.md#decided-2026-11-17-file-names. -/
def FilenameCharacters (raw : SourceText) : Prop := Permitted .disallowed raw

/-- The certificate carried by a successful production search. -/
def RawOk (raw : SourceText) : Prop :=
  ∀ reason ∈ Reason.inOrder, ProbePermitted reason raw

instance (raw : SourceText) : Decidable (RawOk raw) := by
  unfold RawOk
  infer_instance

/-- A reason with its byte or boundary witness. -/
structure Violation (length : Nat) where
  reason : Reason
  location : Location length
  deriving DecidableEq, Repr

/-- Candidates grouped in rule order, with each component checked against the supplied policy. -/
def candidatesWith (policy : ProbePolicy) (raw : SourceText) : List (Violation raw.length) :=
  let parts := components raw
  Reason.inOrder.flatMap fun reason => (checkWith policy raw parts reason).map (Violation.mk reason)

/-- The production refusal candidates. -/
def candidates := candidatesWith localPolicy

/-- Encode the lexicographic pair of witness offset and check priority as a natural number. -/
def Violation.key {length : Nat} (violation : Violation length) : Nat :=
  violation.location.offset * Reason.inOrder.length + Reason.inOrder.idxOf violation.reason

/-- Select a refusal candidate using the supplied ordering. -/
def selectViolation (policy : ProbePolicy) (raw : SourceText) (key : Violation raw.length → Nat) :
    Option (Violation raw.length) :=
  (candidatesWith policy raw).minOn? key

/-- Select the earliest offset, keeping the first check in priority order on a tie. -/
def firstViolation (raw : SourceText) : Option (Violation raw.length) :=
  selectViolation localPolicy raw Violation.key

/-- The original bytes and the selected local refusal. -/
structure Refusal where
  raw : SourceText
  violation : Violation raw.length
  deriving Repr

/-- The byte slice named by the refusal, retaining its exact spelling. -/
def Refusal.contextBytes (refusal : Refusal) : ConcreteSpec.Numbers :=
  let location := refusal.violation.location
  (refusal.raw.bytes.drop location.start).take (location.stop - location.start)

end SourcePath

/-- A checked project-relative path. Its identity is exactly the original bytes. -/
structure SourcePath where
  raw : SourceText
  valid : SourcePath.RawOk raw
  deriving DecidableEq, Repr

namespace SourcePath

/-- A successful diagnostic search establishes all local validity judgments. -/
theorem rawOk_of_noViolation (raw : SourceText) (clear : firstViolation raw = none) :
    RawOk raw := by
  intro reason member
  apply (check_witness_iff reason raw).1
  cases found : check raw (components raw) reason with
  | nil => rfl
  | cons location rest =>
    have present : Violation.mk reason location ∈ candidates raw := by
      unfold candidates candidatesWith
      apply List.mem_flatMap.mpr
      exact ⟨reason, member, List.mem_map.mpr ⟨location, by
        change location ∈ check raw (components raw) reason
        simp [found], rfl⟩⟩
    have someViolation := List.isSome_minOn?_of_mem (f := Violation.key) present
    change (firstViolation raw).isSome = true at someViolation
    simp [clear] at someViolation

/-- The local admission boundary over original bytes. -/
def parse (raw : SourceText) : Except Refusal SourcePath :=
  match h : firstViolation raw with
  | some violation => .error ⟨raw, violation⟩
  | none => .ok ⟨raw, rawOk_of_noViolation raw h⟩

/-- The exact admitted bytes. -/
def renderBytes (path : SourcePath) : SourceText := path.raw

/-- Render the admitted ASCII byte values as the identical ASCII name. -/
def render (path : SourcePath) : ConcreteSpec.Name :=
  ⟨String.ofList (path.raw.bytes.map Char.ofNat)⟩

end SourcePath

namespace SourcePath.Project

/-- The three project-wide refusal reasons. -/
inductive Reason where
  | caseCollision
  | fileDirectoryConflict
  | duplicate
  deriving DecidableEq, Repr

/-- Compare exact components, using the ASCII fold only at the first spelling difference. -/
def classifyComponents : List ConcreteSpec.Numbers → List ConcreteSpec.Numbers → Option Reason
  | [], [] => some .duplicate
  | [], _ :: _ | _ :: _, [] => some .fileDirectoryConflict
  | left :: ls, right :: rs =>
    if left = right then classifyComponents ls rs
    else if left.map foldAscii = right.map foldAscii then some .caseCollision
    else none

/-- Identify a conflict without changing either path. -/
def classify (left right : SourcePath) : Option Reason :=
  classifyComponents ((components left.raw).map Component.bytes)
    ((components right.raw).map Component.bytes)

/-- A conflict identifies two zero-based positions in the original input. -/
structure Refusal where
  reason : Reason
  first : Nat
  second : Nat
  deriving DecidableEq, Repr

/-- Search distinct positions for a conflicting pair. -/
def checkWith (compare : SourcePath → SourcePath → Option Reason) (paths : List SourcePath) :
    Except Refusal Unit :=
  let entries := paths.toArray
  let positions := List.finRange paths.length
  match positions.findSome? (fun first =>
    positions.findSome? (fun second =>
      if first.val < second.val then
        let left := entries[first.val]'(by simp [entries])
        let right := entries[second.val]'(by simp [entries])
        (compare left right).map fun reason =>
          { reason, first := first.val, second := second.val }
      else none)) with
  | none => .ok ()
  | some refusal => .error refusal

/-- Check project-wide admission of locally admitted paths. -/
def check := checkWith classify

/-- The preliminary local parse preserves the first LOCAL refusal. -/
def parseLocal : List SourceText → Except SourcePath.Refusal (List SourcePath)
  | [] => .ok []
  | raw :: rest => do
    let path ← SourcePath.parse raw
    let paths ← parseLocal rest
    return path :: paths

/-- The admission boundary for raw project inputs. -/
def parse (inputs : List SourceText) :
    Except (SourcePath.Refusal ⊕ Refusal) (List SourcePath) := do
  let paths ← (parseLocal inputs).mapError Sum.inl
  (check paths).mapError Sum.inr
  return paths

namespace Reference

/-- Slash-delimited byte strings, independently of the executable scanner. -/
def parts (path : SourcePath) : List ConcreteSpec.Numbers :=
  path.raw.bytes.splitOn 47

/-- Raw inequality and ASCII-folded equality at an aligned component prefix. -/
def CaseCollision (left right : List ConcreteSpec.Numbers) : Prop :=
  ∃ n, n ≤ left.length ∧ n ≤ right.length ∧ left.take n ≠ right.take n ∧
    (left.take n).map (List.map SourcePath.Reference.lower) =
      (right.take n).map (List.map SourcePath.Reference.lower)

/-- A reason holds exactly when its corresponding pair conflict holds. -/
def Conflict (reason : Reason) (left right : SourcePath) : Prop :=
  let ls := parts left
  let rs := parts right
  match reason with
  | .caseCollision => CaseCollision ls rs
  | .fileDirectoryConflict => ls ≠ rs ∧ (ls <+: rs ∨ rs <+: ls)
  | .duplicate => left.raw = right.raw

/-- Every pair of occurrences must satisfy all project rules. -/
def Admitted (paths : List SourcePath) : Prop :=
  paths.Pairwise fun left right => ∀ reason, ¬ Conflict reason left right

end Reference

/-- R-0709, docs/decisions.md#decided-2026-11-16-file-paths.
Matching ASCII-folded component prefixes must have identical spelling. -/
def NoCaseCollision (pair : SourcePath × SourcePath) : Prop :=
  ¬ Reference.Conflict .caseCollision pair.1 pair.2

/-- R-0709, docs/decisions.md#decided-2026-11-17-file-names.
A file path cannot also be an exact proper directory prefix of another path. -/
def NoFileDirectoryConflict (pair : SourcePath × SourcePath) : Prop :=
  ¬ Reference.Conflict .fileDirectoryConflict pair.1 pair.2

/-- R-0709, docs/decisions.md#decided-2026-11-21-duplicate-project-path.
A project cannot list the same path at two distinct positions. -/
def NoDuplicate (pair : SourcePath × SourcePath) : Prop :=
  ¬ Reference.Conflict .duplicate pair.1 pair.2

end SourcePath.Project

/-- A source span has an ordered half-open byte range and positive coordinates at its start.
The loader binds its checked path and coordinates to source bytes. -/
structure Span where
  path : SourcePath
  startOffset : Nat
  endOffset : Nat
  range_ok : startOffset ≤ endOffset
  line : Nat
  col : Nat
  line_pos : 0 < line
  col_pos : 0 < col
  deriving DecidableEq, Repr

/-- The range fits the source, and the stored line and column describe its exact start. -/
def Span.LocationOk (span : Span) (source : SourceText) : Prop :=
  span.endOffset ≤ source.length ∧ span.line = source.lineAt span.startOffset ∧
    span.col = source.columnAt span.startOffset

instance (span : Span) (source : SourceText) : Decidable (span.LocationOk source) := by
  unfold Span.LocationOk
  infer_instance

/-- Compatibility view of the admitted path carried by a span. -/
def Span.PathOk (span : Span) : Prop :=
  SourcePath.RawOk span.path.renderBytes

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

/-- The diagnostic citation of a local refusal. Priority is set separately by
`Reason.inOrder`. -/
def SourcePath.Reason.ruleId : SourcePath.Reason → RuleId
  | .empty => ⟨``SourcePath.Nonempty, 1⟩
  | .absolute => ⟨``SourcePath.Relative, 1⟩
  | .drive => ⟨``SourcePath.NoDrivePrefix, 1⟩
  | .traversal => ⟨``SourcePath.NoTraversal, 1⟩
  | .singleDot => ⟨``SourcePath.NoSingleDot, 1⟩
  | .emptyComponent => ⟨``SourcePath.NonemptyComponents, 1⟩
  | .nonAscii => ⟨``SourcePath.Ascii, 1⟩
  | .backslash => ⟨``SourcePath.SlashSeparator, 1⟩
  | .control => ⟨``SourcePath.NoControls, 1⟩
  | .space => ⟨``SourcePath.NoSpaces, 1⟩
  | .leadingDot => ⟨``SourcePath.NoLeadingDot, 1⟩
  | .trailingDot => ⟨``SourcePath.NoTrailingDot, 1⟩
  | .reserved => ⟨``SourcePath.NoReservedStem, 1⟩
  | .componentCap => ⟨``SourcePath.ComponentByteCap, 1⟩
  | .pathCap => ⟨``SourcePath.PathByteCap, 1⟩
  | .disallowed => ⟨``SourcePath.FilenameCharacters, 1⟩

/-- The diagnostic citation of a project-wide refusal. -/
def SourcePath.Project.Reason.ruleId : SourcePath.Project.Reason → RuleId
  | .caseCollision => ⟨``SourcePath.Project.NoCaseCollision, 1⟩
  | .fileDirectoryConflict => ⟨``SourcePath.Project.NoFileDirectoryConflict, 1⟩
  | .duplicate => ⟨``SourcePath.Project.NoDuplicate, 1⟩

/-- A version of this specification: `0.x` before the freeze, `1.0` at the freeze (A2c), bumped
by every normative change (R-0603). -/
structure SpecVersion where
  major : Nat
  minor : Nat
  deriving DecidableEq, Repr

/-- The initial pre-freeze schema version every output carries in one header.
This extraction starts the schema at 0.1; it does not claim foundation freeze. -/
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

/-- Two loaded modules with the same package identity and origin coexist. -/
def loadedA : LoadedModule := ⟨⟨⟨⟨"p"⟩, ⟨⟨"1"⟩⟩⟩, [⟨"a"⟩], by decide⟩, ⟨⟨"origin-a"⟩⟩⟩

/-- A second module from the same package and origin. -/
def loadedB : LoadedModule := ⟨⟨loadedA.module.package, [⟨"b"⟩], by decide⟩, loadedA.origin⟩

theorem example_LoadedModules_findCollision_loadedA_loadedB_isNone : (LoadedModules.findCollision [loadedA, loadedB]).isNone = true := by decide

/-- A collision refuses the entire load and retains both inputs for diagnostics. -/
theorem example_collision_refuses_entire_load_and_retains : (match LoadedModules.load [loadedA, { loadedA with origin := ⟨⟨"origin-b"⟩⟩ }] with
    | .refused collision => decide (collision.first = loadedA ∧
        collision.second.origin = ⟨⟨"origin-b"⟩⟩)
    | .admitted _ => false) = true := by decide

/-- OD44 refuses one top-level module name shared by distinct packages. -/
theorem example_OD44_refuses_one_top_level_module : (match LoadedModules.load [loadedA,
      { loadedA with module := { loadedA.module with package := ⟨⟨"q"⟩, ⟨⟨"1"⟩⟩⟩ } }] with
    | .refused (.topLevel _ _ _ _) => true
    | _ => false) = true := by decide

/-- Ordinals are per kind; named siblings consume none, while pinned unnamed siblings still
consume one. The literal list checks both completeness and traversal order. -/
theorem example_Ordinals_are_per_kind_named_siblings : KeyConstruct.collectKeys
    [⟨.call, .unnamed none, []⟩,
     ⟨.binding, .unnamed none, []⟩,
     ⟨.call, .named ⟨"helper"⟩, []⟩,
     ⟨.call, .unnamed (some ⟨"keep"⟩), []⟩,
     ⟨.call, .unnamed none, []⟩,
     ⟨.binding, .unnamed none, []⟩] =
    [⟨[⟨.call, .ordinal 0⟩]⟩,
     ⟨[⟨.binding, .ordinal 0⟩]⟩,
     ⟨[⟨.call, .named ⟨"helper"⟩⟩]⟩,
     ⟨[⟨.call, .pinned ⟨"keep"⟩⟩]⟩,
     ⟨[⟨.call, .ordinal 2⟩]⟩,
     ⟨[⟨.binding, .ordinal 1⟩]⟩] := by cbv

/-- Entering a named construct resets the scope, but entering an unnamed one inherits it.
Both the collector and the occurrence resolver must retain the complete ancestor path. -/
theorem example_Entering_named_construct_resets_scope_but : let body : List KeyConstruct :=
    [⟨.call, .unnamed none, []⟩,
     ⟨.block, .named ⟨"inner"⟩, [⟨.call, .unnamed none, []⟩]⟩,
     ⟨.block, .unnamed none,
       [⟨.call, .unnamed none,
         [⟨.binding, .unnamed none, []⟩]⟩]⟩]
    KeyConstruct.collectKeys body =
      [⟨[⟨.call, .ordinal 0⟩]⟩,
       ⟨[⟨.block, .named ⟨"inner"⟩⟩]⟩,
       ⟨[⟨.block, .named ⟨"inner"⟩⟩, ⟨.call, .ordinal 0⟩]⟩,
       ⟨[⟨.block, .ordinal 0⟩]⟩,
       ⟨[⟨.block, .ordinal 0⟩, ⟨.call, .ordinal 1⟩]⟩,
       ⟨[⟨.block, .ordinal 0⟩, ⟨.call, .ordinal 1⟩, ⟨.binding, .ordinal 0⟩]⟩] ∧
    KeyConstruct.pathAt body [1, 0] =
      some ⟨[⟨.block, .named ⟨"inner"⟩⟩, ⟨.call, .ordinal 0⟩]⟩ ∧
    KeyConstruct.pathAt body [2, 0, 0] =
      some ⟨[⟨.block, .ordinal 0⟩, ⟨.call, .ordinal 1⟩, ⟨.binding, .ordinal 0⟩]⟩ := by cbv

/-- Descendant assignment retains a pinned ancestor and inherited per-kind ordinals. -/
theorem example_Descendant_assignment_retains_pinned_ancestor_and : KeyConstruct.descendantKey [⟨.call, .unnamed none, []⟩]
    ⟨.block, .unnamed (some ⟨"outer"⟩), [⟨.call, .unnamed none, []⟩]⟩ [0] =
    some ⟨[⟨.block, .pinned ⟨"outer"⟩⟩, ⟨.call, .ordinal 1⟩]⟩ := by cbv

/-- Assigned tables attach every key, in preorder, to its actual owner and sequential sites
starting at a nonzero offset. Program assignment uses the selected function's body and offset. -/
theorem example_Assigned_tables_attach_every_key_in : let f : FnId := ⟨loadedA.module, ⟨"f"⟩⟩
    let g : FnId := ⟨loadedA.module, ⟨"g"⟩⟩
    let body : List KeyConstruct :=
      [⟨.block, .named ⟨"inner"⟩, [⟨.call, .unnamed (some ⟨"invoke"⟩), []⟩]⟩]
    KeyConstruct.assignFunctionTable f 7 body =
      [(⟨7⟩, ⟨.function f, ⟨[⟨.block, .named ⟨"inner"⟩⟩]⟩⟩),
       (⟨8⟩, ⟨.function f, ⟨[⟨.block, .named ⟨"inner"⟩⟩, ⟨.call, .pinned ⟨"invoke"⟩⟩]⟩⟩)] ∧
    KeyConstruct.assignProgramTables
      (fun owner => if owner = f then body else [⟨.abort, .unnamed none, []⟩])
      (fun owner => if owner = f then 7 else 20) g =
      [(⟨20⟩, ⟨.function g, ⟨[⟨.abort, .ordinal 0⟩]⟩⟩)] := by cbv

/-- Equal specialization requests at different sites share one entry, retaining both edges. -/
theorem example_Equal_specialization_requests_at_different_sites : let generic := GenericId.function ⟨loadedA.module, ⟨"f"⟩⟩
    let table := SpecializationTable.canonicalize
      [SpecializationRequest.for ⟨0⟩ generic ([1] : Numbers) ([2] : Numbers),
       SpecializationRequest.for ⟨1⟩ generic [1] [2]]
    table.entries.length = 1 ∧ table.edges.map SpecializationEdge.entry = [0, 0] := by decide

/-- Known premise categories round-trip; unknown tags and mismatched versions are refused. -/
theorem example_Known_premise_categories_round_trip_unknown : let scope := PremiseScope.package loadedA.module.package ⟨⟨"heap"⟩⟩
    (PremiseRecord.mk .heap scope specVersion).encode.read specVersion =
      some ⟨.heap, scope, specVersion⟩ ∧
    EncodedPremise.read specVersion ⟨99, scope, specVersion⟩ = none ∧
    EncodedPremise.read specVersion ⟨0, scope, ⟨9, 9⟩⟩ = none := by decide

/-- Two declarations named `Pair` in different modules are different declarations. -/
theorem example_Two_declarations_named_Pair_in_different : DeclId.mk (ModuleId.mk ⟨⟨"p"⟩, ⟨⟨"1"⟩⟩⟩ [⟨"a"⟩] (by decide)) ⟨"Pair"⟩ ≠
    DeclId.mk (ModuleId.mk ⟨⟨"p"⟩, ⟨⟨"1"⟩⟩⟩ [⟨"b"⟩] (by decide)) ⟨"Pair"⟩ := by decide

/-- The same module path in two packages also yields different declarations. -/
theorem example_same_module_path_in_two_packages : DeclId.mk (ModuleId.mk ⟨⟨"p1"⟩, ⟨⟨"1"⟩⟩⟩ [⟨"a"⟩] (by decide)) ⟨"Pair"⟩ ≠
    DeclId.mk (ModuleId.mk ⟨⟨"p2"⟩, ⟨⟨"1"⟩⟩⟩ [⟨"a"⟩] (by decide)) ⟨"Pair"⟩ := by decide

/-- A field belongs to its owner: `a` of `Pair` is not `a` of `Point`, and `value` of variant
`E.A` is not `value` of variant `E.B`. -/
theorem example_field_belongs_its_owner_Pair_is : FieldId.mk (.struct ⟨ModuleId.mk ⟨⟨"p"⟩, ⟨⟨"1"⟩⟩⟩ [⟨"m"⟩] (by decide), ⟨"Pair"⟩⟩) ⟨"a"⟩ ≠
    FieldId.mk (.struct ⟨ModuleId.mk ⟨⟨"p"⟩, ⟨⟨"1"⟩⟩⟩ [⟨"m"⟩] (by decide), ⟨"Point"⟩⟩) ⟨"a"⟩ := by decide

theorem example_FieldId_mk_variant_ModuleId_mk : FieldId.mk (.variant ⟨⟨ModuleId.mk ⟨⟨"p"⟩, ⟨⟨"1"⟩⟩⟩ [⟨"m"⟩] (by decide), ⟨"E"⟩⟩, ⟨"A"⟩⟩) ⟨"value"⟩ ≠
    FieldId.mk (.variant ⟨⟨ModuleId.mk ⟨⟨"p"⟩, ⟨⟨"1"⟩⟩⟩ [⟨"m"⟩] (by decide), ⟨"E"⟩⟩, ⟨"B"⟩⟩) ⟨"value"⟩ := by decide

/-- The first and the last slice. -/
theorem example_first_and_last_slice : (SliceId.mk 0).n.val = 0 ∧ (SliceId.mk 28).n.val = 28 := ⟨rfl, rfl⟩

/-- Bytes for `a`, a tab, and `b`. After the tab, the range starts at line 1, byte column 3. -/
def tabSource : SourceText := SourceText.ofBytes [97, 9, 98]

/-- Example: a path carrying the exact spelling `main.con`. -/
def mainSourcePath : SourcePath :=
  ⟨SourceText.ofBytes [109, 97, 105, 110, 46, 99, 111, 110], by decide⟩

/-- Example: the byte after the tab is at line 1, byte column 3. -/
def tabSpan : Span := ⟨mainSourcePath, 2, 3, by decide, 1, 3, by decide, by decide⟩

/-- A tab advances one byte column, independently of any source-path policy. -/
theorem example_tab_advances_one_byte_column_independently : tabSource.lineAt 2 = 1 ∧ tabSource.columnAt 2 = 3 := by decide

/-- Counting the tab as zero bytes gives the wrong column. -/
theorem example_Counting_tab_as_zero_bytes_gives : tabSource.columnAt 2 ≠ 2 := by decide

/-- A newline starts a new 1-based byte column; offsets count bytes, not scalars. -/
theorem example_newline_starts_new_based_byte_column : (SourceText.ofBytes [97, 10, 195, 169]).lineAt 3 = 2 ∧
    (SourceText.ofBytes [97, 10, 195, 169]).columnAt 3 = 2 := by decide

/-- Local identity is the numeric type from Basics, with no presentation component. -/
theorem example_Local_identity_is_numeric_type_from : (⟨0⟩ : ConcreteSpec.Var) ≠ ⟨1⟩ := by decide

/-- Places, primitives and contract revisions have distinct typed identities. -/
theorem example_Places_primitives_and_contract_revisions_have : PlaceId.mk 0 ≠ PlaceId.mk 1 ∧ PrimId.mk 0 ≠ PrimId.mk 1 ∧
    ContractId.mk ⟨0⟩ 1 ≠ ContractId.mk ⟨0⟩ 2 := by decide

/-- Two obligations at one site differ by kind; two of one kind differ by their local discriminator. -/
theorem example_Two_obligations_at_one_site_differ : ObligationId.mk ⟨0⟩ .safety 0 ≠ ObligationId.mk ⟨0⟩ .termination 0 ∧
    ObligationId.mk ⟨0⟩ .safety 0 ≠ ObligationId.mk ⟨0⟩ .safety 1 := by decide

/-- A revised rule is a different rule identity. -/
theorem example_revised_rule_is_different_rule_identity : RuleId.mk `ConcreteSpec.Core.example 1 ≠ RuleId.mk `ConcreteSpec.Core.example 2 := by
  decide

/-- Before the freeze the version is `0.x`. -/
theorem example_Before_freeze_version_is_x : specVersion.major = 0 := rfl

end ConcreteSpec.Core
