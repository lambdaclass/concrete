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
and discharge tokens are keyed by site. Source spans carry file indices scoped to a project table.
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
Local and project-wide path admission, span projection and source binding are defined below.
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
safety condition; the other constructors name the contract, invariant, assertion and resource-bound
obligations explicitly stated by the spec. -/
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

/-- A byte cursor. CR advances the line immediately and the LF in CRLF
completes the same line ending. This preserves the surface cursor's existing rule. -/
structure Position where
  byte : Nat := 0
  line : Nat := 1
  column : Nat := 1
  afterCR : Bool := false
  deriving DecidableEq, Repr, Inhabited

/-- Advance by an encoded scalar of the given byte width. A raw byte has width one. -/
def Position.step (pos : Position) (value width : Nat) : Position :=
  if value == 13 then ⟨pos.byte + width, pos.line + 1, 1, true⟩
  else if value == 10 then
    ⟨pos.byte + width, if pos.afterCR then pos.line else pos.line + 1, 1, false⟩
  else ⟨pos.byte + width, pos.line, pos.column + width, false⟩

def Position.advanceByte (pos : Position) (byte : Nat) : Position :=
  pos.step byte 1

/-- Coordinates after the prefix of the given byte length. -/
def SourceText.positionAt (source : SourceText) (offset : Nat) : Position :=
  (source.bytes.take offset).foldl Position.advanceByte {}

def SourceText.lineAt (source : SourceText) (offset : Nat) : Nat :=
  (source.positionAt offset).line

/-- One-based byte column. Tabs and individual bytes of multibyte scalars each
occupy one column (docs/decisions.md#foundation-q10). -/
def SourceText.columnAt (source : SourceText) (offset : Nat) : Nat :=
  (source.positionAt offset).column

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

/-- A file-table index, scoped to the immutable table that issued it. -/
structure FileId where
  index : Nat
  deriving DecidableEq, Repr

/-- A half-open byte range in a file table. Empty ranges are allowed, including
at EOF. The starting line and byte column are one-based. -/
structure Span where
  fileId : FileId
  startOffset : Nat
  endOffset : Nat
  range_ok : startOffset ≤ endOffset
  line : Nat
  col : Nat
  line_pos : 0 < line
  col_pos : 0 < col
  deriving DecidableEq, Repr

namespace SourcePathSpan

/-- R-0709, docs/decisions.md#decided-2026-11-14-trap-sites-and-law-tests.
A span end must fit the file, including the boundary at EOF. -/
def EndInSource (input : SourceText × Nat) : Prop :=
  input.2 ≤ input.1.length

/-- R-0709, docs/decisions.md#decided-project-runtime-2026-11-06.
The stored line must equal the 1-based line at the starting byte offset. -/
def LineMatches (input : Span × SourceText) : Prop :=
  input.1.line = input.2.lineAt input.1.startOffset

/-- R-0709, docs/decisions.md#foundation-q10.
The stored column must equal the 1-based byte column at the starting offset. -/
def ColumnMatches (input : Span × SourceText) : Prop :=
  input.1.col = input.2.columnAt input.1.startOffset

end SourcePathSpan

/-- The range fits the source, and the stored line and column describe its exact start. -/
def Span.LocationOk (span : Span) (source : SourceText) : Prop :=
  SourcePathSpan.EndInSource (source, span.endOffset) ∧
    SourcePathSpan.LineMatches (span, source) ∧ SourcePathSpan.ColumnMatches (span, source)

instance (span : Span) (source : SourceText) : Decidable (span.LocationOk source) := by
  unfold Span.LocationOk SourcePathSpan.EndInSource SourcePathSpan.LineMatches
    SourcePathSpan.ColumnMatches
  infer_instance

namespace SourcePathSpan

/-- The exact bytes supplied for one checked project-relative file path, with a
coordinate index built in a single pass at load time. -/
structure File where
  path : SourcePath
  source : SourceText
  private positions : Array Position
  private positions_eq : positions = (source.bytes.scanl Position.advanceByte {}).toArray
  deriving DecidableEq

def File.load (path : SourcePath) (source : SourceText) : File :=
  ⟨path, source, (source.bytes.scanl Position.advanceByte {}).toArray, rfl⟩

def File.length (file : File) : Nat := file.positions.size - 1

-- The per-span bounds check relies on an array size read.
private theorem File.length_eq_indexSize (file : File) : file.length = file.positions.size - 1 := rfl

theorem File.length_eq (file : File) : file.length = file.source.length := by
  simp [file.length_eq_indexSize, file.positions_eq, SourceText.length, List.length_scanl]

private theorem File.offset_lt_size (file : File) (offset : Nat) (bounded : offset ≤ file.length) :
    offset < file.positions.size := by
  rw [file.positions_eq]
  simp [List.length_scanl]
  simpa [File.length_eq, SourceText.length, Nat.lt_succ_iff] using bounded

/-- Read one array element and charge the access. -/
@[inline] def readArrayWithWork {α : Type} (array : Array α) (index : Nat)
    (bounded : index < array.size) : α × Nat :=
  (array[index], 1)

private def scanPositionWithWork : List Nat → Nat → Position × Nat → Position × Nat
  | byte :: rest, offset + 1, (position, work) =>
    scanPositionWithWork rest offset (position.advanceByte byte, work + 1)
  | _, _, result => result

private theorem scanPositionWithWork_position (bytes : List Nat) (offset : Nat)
    (result : Position × Nat) :
    (scanPositionWithWork bytes offset result).1 =
      (bytes.take offset).foldl Position.advanceByte result.1 := by
  induction bytes generalizing offset result with
  | nil => cases offset <;> rfl
  | cons byte rest ih =>
    cases offset with
    | zero => rfl
    | succ offset =>
      rcases result with ⟨position, work⟩
      exact ih offset (position.advanceByte byte, work + 1)

/-- A coordinate access that counts its work and returns the reference position. -/
def File.CoordinateLookup :=
  (file : File) → (offset : Nat) → offset ≤ file.length →
    { result : Position × Nat // result.1 = file.source.positionAt offset }

/-- Read one indexed coordinate. -/
@[inline] def File.indexedCoordinate : File.CoordinateLookup :=
  fun file offset bounded =>
    ⟨readArrayWithWork file.positions offset (file.offset_lt_size offset bounded), by
      simp [readArrayWithWork, file.positions_eq, List.getElem_scanl, SourceText.positionAt]⟩

/-- Compute the reference prefix fold, charging each byte as it is visited. -/
def File.prefixCoordinate : File.CoordinateLookup :=
  fun file offset _ =>
    ⟨scanPositionWithWork file.source.bytes offset ({}, 0),
      scanPositionWithWork_position _ _ _⟩

def File.positionAt (file : File) (offset : Nat) (bounded : offset ≤ file.length) : Position :=
  (File.indexedCoordinate file offset bounded).val.1

theorem File.positionAt_eq (file : File) (offset : Nat) (bounded : offset ≤ file.length) :
    file.positionAt offset bounded = file.source.positionAt offset :=
  (File.indexedCoordinate file offset bounded).property

private theorem Position.step_positive (pos : Position) (value width : Nat)
    (line : 0 < pos.line) (col : 0 < pos.column) :
    0 < (pos.step value width).line ∧ 0 < (pos.step value width).column := by
  unfold Position.step
  split <;> (try split) <;> simp_all [Nat.lt_of_lt_of_le col (Nat.le_add_right ..)]
  split <;> simp_all

theorem SourceText.positionAt_positive (source : SourceText) (offset : Nat) :
    0 < (source.positionAt offset).line ∧ 0 < (source.positionAt offset).column := by
  unfold SourceText.positionAt
  generalize source.bytes.take offset = bytes
  suffices ∀ pos : Position, 0 < pos.line → 0 < pos.column →
      0 < (bytes.foldl Position.advanceByte pos).line ∧
        0 < (bytes.foldl Position.advanceByte pos).column by
    exact this {} (by decide) (by decide)
  induction bytes with
  | nil => exact fun _ line col => ⟨line, col⟩
  | cons byte bytes ih =>
    intro pos line col
    exact ih _ (Position.step_positive pos byte 1 line col).1
      (Position.step_positive pos byte 1 line col).2

-- A byte trie for exact paths. Replacing a child keeps at most one edge for
-- each byte, so traversal at each node is bounded by the byte alphabet.
private inductive PathIndex where
  | node (slot : Option Nat) (children : List (Nat × PathIndex))

private def PathIndex.empty : PathIndex := .node none []

/-- Look up the first matching edge, charging each key comparison. -/
def lookupEdgeWithWork {α : Type} (key : Nat) : List (Nat × α) → Nat → Option α × Nat
  | [], work => (none, work)
  | (byte, child) :: rest, work =>
    if key == byte then (some child, work + 1)
    else lookupEdgeWithWork key rest (work + 1)

private theorem lookupEdgeWithWork_value {α : Type} (key : Nat) (edges : List (Nat × α))
    (work : Nat) : (lookupEdgeWithWork key edges work).1 = edges.lookup key := by
  induction edges generalizing work with
  | nil => rfl
  | cons edge rest ih =>
    rcases edge with ⟨byte, child⟩
    simp only [lookupEdgeWithWork, List.lookup_cons]
    cases key == byte <;> simp [ih]

private def PathIndex.lookupWithWork : List Nat → PathIndex → Nat → Option Nat × Nat
  | [], .node slot _, work => (slot, work + 1)
  | byte :: rest, .node _ children, work =>
    let edge := lookupEdgeWithWork byte children (work + 1)
    match edge.1 with
    | none => (none, edge.2)
    | some child => lookupWithWork rest child edge.2

private def PathIndex.lookup (key : List Nat) (index : PathIndex) : Option Nat :=
  (index.lookupWithWork key 0).1

private theorem PathIndex.lookupWithWork_slot (key : List Nat) (index : PathIndex)
    (a b : Nat) : (index.lookupWithWork key a).1 = (index.lookupWithWork key b).1 := by
  induction key generalizing index a b with
  | nil => cases index; rfl
  | cons byte rest ih =>
    cases index with
    | node slot children =>
      simp only [lookupWithWork, lookupEdgeWithWork_value]
      split
      · rfl
      · apply ih

private theorem PathIndex.lookup_nil (slot : Option Nat) (children : List (Nat × PathIndex)) :
    lookup [] (.node slot children) = slot := rfl

private theorem PathIndex.lookup_cons (byte : Nat) (rest : List Nat) (slot : Option Nat)
    (children : List (Nat × PathIndex)) :
    lookup (byte :: rest) (.node slot children) = (children.lookup byte).bind (lookup rest) := by
  unfold lookup
  rw [lookupWithWork, lookupEdgeWithWork_value]
  cases children.lookup byte with
  | none => rfl
  | some child => apply lookupWithWork_slot

private def PathIndex.insert : List Nat → Nat → PathIndex → PathIndex
  | [], slot, .node _ children => .node (some slot) children
  | b :: rest, slot, .node current children =>
    let child := (children.lookup b).getD .empty
    .node current ((b, child.insert rest slot) :: children.filter (fun edge => edge.1 != b))

private theorem lookup_filter_other {α : Type} (children : List (Nat × α))
    (b q : Nat) (different : q ≠ b) :
    (children.filter (fun edge => edge.1 != b)).lookup q = children.lookup q := by
  induction children with
  | nil => rfl
  | cons edge rest ih =>
    rcases edge with ⟨key, value⟩
    by_cases same : key = b
    · subst key
      simp [List.lookup_cons, beq_eq_false_iff_ne.mpr different, ih]
    · simp [List.lookup_cons, same, ih]

private theorem PathIndex.lookup_empty (key : List Nat) : empty.lookup key = none := by
  cases key <;> rfl

private theorem PathIndex.lookup_insert (key query : List Nat) (slot : Nat)
    (index : PathIndex) :
    (index.insert key slot).lookup query =
      if query = key then some slot else index.lookup query := by
  induction key generalizing query index with
  | nil =>
    cases query <;> cases index <;>
      simp only [insert, lookup_nil, lookup_cons, ite_true, List.cons_ne_nil, ite_false]
  | cons b rest ih =>
    cases index with
    | node current children =>
      cases query with
      | nil => rfl
      | cons q tail =>
        by_cases same : q = b
        · subst q
          have selfMatch : (b == b) = true := beq_iff_eq.mpr rfl
          simp only [insert, lookup_cons, List.lookup_cons, selfMatch, Option.bind_some, ih,
            List.cons.injEq, true_and]
          cases children.lookup b with
          | none => simp only [Option.getD_none, lookup_empty, Option.bind_none]
          | some child => rfl
        · have noMatch : q :: tail ≠ b :: rest := fun eq => same (List.cons.inj eq).1
          simp only [insert, lookup_cons, List.lookup_cons, beq_eq_false_iff_ne.mpr same,
            lookup_filter_other _ _ _ same, ite_eq_right noMatch]


private def PathIndex.ofFiles (files : List File) (offset : Nat) : PathIndex :=
  (files.zipIdx offset).foldr
    (fun (file, slot) index => index.insert file.path.raw.bytes slot) .empty

private theorem PathIndex.lookup_ofFiles (files : List File) (path : SourcePath) (offset : Nat) :
    (ofFiles files offset).lookup path.raw.bytes =
      (files.findIdx? (fun file => decide (file.path = path))).map (offset + ·) := by
  induction files generalizing offset with
  | nil => cases path.raw.bytes <;> rfl
  | cons file rest ih =>
    have exactPath : path.raw.bytes = file.path.raw.bytes ↔ file.path = path := by
      cases path
      cases file.path
      rename_i a _ b _
      cases a
      cases b
      simp [eq_comm]
    rw [show ofFiles (file :: rest) offset =
      (ofFiles rest (offset + 1)).insert file.path.raw.bytes offset from rfl]
    simp only [lookup_insert, exactPath, List.findIdx?_cons]
    by_cases same : file.path = path
    · simp [same]
    · simp [same, ih, Option.map_map, Function.comp_def, Nat.add_right_comm, Nat.add_assoc]

/-- Loaded files with the project admission invariant. The November 16, November 17
and November 21 path decisions govern this invariant (R-0709). -/
structure Project where
  private mk ::
  entries : Array File
  admitted : SourcePath.Project.Reference.Admitted (entries.toList.map File.path)
  private index : PathIndex
  private index_eq : index = PathIndex.ofFiles entries.toList 0

def Project.load (entries : Array File)
    (admitted : SourcePath.Project.Reference.Admitted (entries.toList.map File.path)) : Project :=
  ⟨entries, admitted, PathIndex.ofFiles entries.toList 0, rfl⟩

def Project.singleton (path : SourcePath) (source : SourceText) : Project :=
  .load #[File.load path source] (by simp [SourcePath.Project.Reference.Admitted])

def Project.files (project : Project) : List File := project.entries.toList

/-- Resolve a table-local file ID. The issuing table supplies the exact bytes. -/
def Project.fileAt (project : Project) (id : FileId) : Option File :=
  project.entries[id.index]?

/-- A span whose issuing slot resolves to a file validating its entire location.
The table parameter and issuance proof are erased at runtime. -/
structure ScopedSpan (origin : Project) extends Span where
  private mk ::
  issued : ∃ file, toSpan.LocationOk file.source ∧ origin.fileAt toSpan.fileId = some file
  deriving DecidableEq, Repr

theorem ScopedSpan.ext {origin : Project} {a b : ScopedSpan origin}
    (same : a.toSpan = b.toSpan) : a = b := by
  cases a
  cases b
  cases same
  rfl

/-- R-0709, R-0496, docs/decisions.md#decided-2026-11-14-trap-sites-and-law-tests.
The requested exact path must name a file in the admitted project. -/
def FilePresent (input : Project × SourcePath) : Prop :=
  input.2 ∈ input.1.files.map File.path

/-- R-0709, docs/decisions.md#decided-2026-11-14-trap-sites-and-law-tests.
Half-open span offsets are ordered. Equal offsets describe an empty span. -/
def OrderedOffsets (input : Nat × Nat) : Prop :=
  input.1 ≤ input.2

/-- Resolve an exact path and count node visits and edge comparisons. -/
@[inline] def Project.findIdWithWork (project : Project) (path : SourcePath) : Option FileId × Nat :=
  let result := project.index.lookupWithWork path.raw.bytes 0
  (result.1.map FileId.mk, result.2)

/-- Resolve an exact path in time proportional to its byte length. -/
def Project.findId (project : Project) (path : SourcePath) : Option FileId :=
  (project.findIdWithWork path).1

theorem Project.findId_eq (project : Project) (path : SourcePath) :
    project.findId path = (project.files.findIdx? (fun file => decide (file.path = path))).map FileId.mk := by
  change (project.index.lookup path.raw.bytes).map FileId.mk = _
  simp [project.index_eq, PathIndex.lookup_ofFiles, files]

/-- Look up the exact path among admitted files, preserving the supplied bytes. -/
def Project.findFile (project : Project) (path : SourcePath) : Option File :=
  (project.findId path).bind project.fileAt

/-- Project an ordered, in-bounds byte range with a certified coordinate lookup. -/
@[inline] def fileSpanWithWork (id : FileId) (file : File) (start stop : Nat)
    (ordered : start ≤ stop) (bounded : start ≤ file.length)
    (lookup : File.CoordinateLookup := File.indexedCoordinate) : Span × Nat :=
  let result := lookup file start bounded
  let pos := result.val.1
  have positive : 0 < pos.line ∧ 0 < pos.column := by
    simp only [pos, result.property]
    exact SourceText.positionAt_positive _ _
  (⟨id, start, stop, ordered, pos.line, pos.column, positive.1, positive.2⟩, result.val.2)

def fileSpan (id : FileId) (file : File) (start stop : Nat)
    (ordered : start ≤ stop) (bounded : start ≤ file.length) : Span :=
  (fileSpanWithWork id file start stop ordered bounded).1

private theorem fileSpanWithWork_location (id : FileId) (file : File) (start stop : Nat)
    (ordered : start ≤ stop) (within : stop ≤ file.length) (lookup : File.CoordinateLookup) :
    (fileSpanWithWork id file start stop ordered (Nat.le_trans ordered within) lookup).1.LocationOk
      file.source := by
  have position := (lookup file start (Nat.le_trans ordered within)).property
  simp [Span.LocationOk, EndInSource, LineMatches, ColumnMatches, fileSpanWithWork,
    position, SourceText.lineAt, SourceText.columnAt, ← File.length_eq, within]

theorem fileSpan_location (id : FileId) (file : File) (start stop : Nat)
    (ordered : start ≤ stop) (within : stop ≤ file.length) :
    (fileSpan id file start stop ordered (Nat.le_trans ordered within)).LocationOk file.source :=
  fileSpanWithWork_location id file start stop ordered within File.indexedCoordinate

/-- Issue a span and count the file read and coordinate lookup work. -/
@[inline] def projectSpanAtWithWork (project : Project) (id : FileId) (start stop : Nat)
    (lookup : File.CoordinateLookup := File.indexedCoordinate) : Option (ScopedSpan project) × Nat :=
  if slot : id.index < project.entries.size then
    let entry := readArrayWithWork project.entries id.index slot
    let file := entry.1
    if ordered : start ≤ stop then
      if within : stop ≤ file.length then
        let result := fileSpanWithWork id file start stop ordered (Nat.le_trans ordered within) lookup
        (some ⟨result.1,
          ⟨file, fileSpanWithWork_location id file start stop ordered within lookup,
            Array.getElem?_eq_getElem slot⟩⟩, entry.2 + result.2)
      else (none, entry.2)
    else (none, entry.2)
  else (none, 0)

/-- Issue a span using a resolved file ID, with constant-time file and coordinate access. -/
def projectSpanAt (project : Project) (id : FileId) (start stop : Nat) : Option (ScopedSpan project) :=
  (projectSpanAtWithWork project id start stop).1

@[inline] def projectSpanWithWork (project : Project) (path : SourcePath) (start stop : Nat) :
    Option (ScopedSpan project) × Nat :=
  match project.findIdWithWork path with
  | (none, work) => (none, work)
  | (some id, work) =>
    let (span, spanWork) := projectSpanAtWithWork project id start stop
    (span, work + spanWork)

def projectSpan (project : Project) (path : SourcePath) (start stop : Nat) : Option (ScopedSpan project) :=
  (projectSpanWithWork project path start stop).1

/-- Check a stored location using the loaded file's coordinate index. -/
def File.matchesLocation (file : File) (span : Span) : Bool :=
  if fits : span.endOffset ≤ file.length then
    let pos := file.positionAt span.startOffset (Nat.le_trans span.range_ok fits)
    decide (span.line = pos.line ∧ span.col = pos.column)
  else false

theorem File.matchesLocation_iff (file : File) (span : Span) :
    file.matchesLocation span = true ↔ span.LocationOk file.source := by
  unfold File.matchesLocation Span.LocationOk SourcePathSpan.EndInSource
    SourcePathSpan.LineMatches SourcePathSpan.ColumnMatches
  rw [← File.length_eq]
  by_cases fits : span.endOffset ≤ file.length
  · simp [fits, file.positionAt_eq _ (Nat.le_trans span.range_ok fits),
      SourceText.lineAt, SourceText.columnAt]
  · simp [fits]

/-- Validate a span from its issuing table against a target project. A file-table
index alone is meaningful only in its issuing table. Rebinding compares the
exact path and bytes, including when the target uses a different file order. -/
def bindSource (origin target : Project) (span : ScopedSpan origin) : Option File := do
  let original ← origin.fileAt span.fileId
  let file ← target.findFile original.path
  if file.matchesLocation span.toSpan ∧ original.source = file.source then some file else none

namespace Reference

/-- An exact target binding preserves the originating path and bytes. -/
def Binds (origin target : Project) (span : ScopedSpan origin) (file : File) : Prop :=
  ∃ original, origin.fileAt span.fileId = some original ∧
    file ∈ target.files ∧ file.path = original.path ∧
    span.toSpan.LocationOk file.source ∧ original.source = file.source

/-- An independently stated projection preserves the requested path and valid range. -/
def Projects (project : Project) (path : SourcePath) (start stop : Nat) (span : ScopedSpan project) : Prop :=
  span.startOffset = start ∧ span.endOffset = stop ∧ OrderedOffsets (start, stop) ∧
    ∃ file, project.fileAt span.fileId = some file ∧ file.path = path ∧ span.toSpan.LocationOk file.source

end Reference
end SourcePathSpan

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
def tabSpan : Span := ⟨⟨0⟩, 2, 3, by decide, 1, 3, by decide, by decide⟩

namespace SourcePathSpan

/-- A single-file admitted project containing the tab example. -/
def tabProject : Project := .singleton mainSourcePath tabSource

/-- The tab example scoped to the table that issued its file ID. -/
def scopedTabSpan : ScopedSpan tabProject :=
  ⟨tabSpan, File.load mainSourcePath tabSource, by decide, rfl⟩

/-- A tab advances the starting byte column by one. -/
theorem projectTabSpan : projectSpan tabProject mainSourcePath 2 3 = some scopedTabSpan := by
  decide

/-- Binding the tab span in the example project returns the loaded tab file. -/
theorem bindTabSpan : bindSource tabProject tabProject scopedTabSpan = some (File.load mainSourcePath tabSource) := by decide

example : (projectSpan tabProject mainSourcePath 3 3).map ScopedSpan.toSpan =
    some ⟨⟨0⟩, 3, 3, by decide, 1, 4, by decide, by decide⟩ := by decide

-- The replacement preserves the length and coordinates but changes the spanned byte.
theorem bindEditedSpan_refused :
    let edited := SourceText.ofBytes [97, 9, 99]
    let project := Project.singleton mainSourcePath edited
    tabSpan.LocationOk edited ∧ bindSource tabProject project scopedTabSpan = none := by
  decide

end SourcePathSpan


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
