import ConcreteSpec.Spec.Basics

/-!
# Identities, sites and versions

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). Part of `ConcreteSpec.Spec`;
see `ConcreteSpec/README.md`.

Core names everything by a typed identity, never by a bare string or number: a local, a
declaration, a constant, a field, a function, a module, an instance, a loop label, a brand, an allocator
lineage, a loan, a grant, a block, a capability variable and a theorem each have their own type,
so one can never stand for another, and two declarations with the same source name in different
modules are different declarations (the facts decision: no bare-name collisions). A declaration,
field, variant or function is named by its module (or owner) and its source name, and both are
part of its identity. A local is numbered by elaboration and keeps its source name as a hint for
diagnostics. Equality of identities is what the rules use.

A *site* (`SiteId`) is one per operation and per refusable statement. Diagnostics, facts, traps
and discharge tokens are keyed by site; a program's site table maps each site to its source
`Span`, which elaboration produces (README obligation O1).

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

/-- A package identity. The token is opaque here: whether it is derived from a name, a name and
version, or a content hash is an interface choice not yet fixed by a decided row. -/
structure PackageId where
  token : String
  deriving DecidableEq, Repr

/-- A module: its package and its path from that package's root (`["std", "bytes"]`). -/
structure ModuleId where
  package : PackageId
  path : List String
  deriving DecidableEq, Repr

/-- A load-time collision: two modules from different packages have the same top-level module name.
The two complete identities retain both package names for the required diagnostic (OD44). -/
structure ModuleCollision where
  first : ModuleId
  second : ModuleId
  sameTopLevel : first.path.head? = second.path.head?
  differentPackages : first.package ≠ second.package

/-- Program well-formedness under OD44: until R-0440, distinct packages in one loaded program may
not define the same top-level module name. This is refusal, not package-qualified coexistence. -/
def LoadedModules.WellFormed (modules : List ModuleId) : Prop :=
  ∀ first ∈ modules, ∀ second ∈ modules,
    first.path.head? = second.path.head? → first.package = second.package

/-- A collision witness whose two modules are both in the loaded program. -/
def LoadedModules.HasCollision (modules : List ModuleId) (collision : ModuleCollision) : Prop :=
  collision.first ∈ modules ∧ collision.second ∈ modules

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

/-- A generic definition that can be specialized: a generic function or a generic type declaration. -/
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

/-- An opaque key relative to one enclosing function. Its token carries stable source-relative data;
whether that data is a syntactic path, an ordinal per construct kind, a user label, or another
representation is deliberately not fixed here (R-0492). -/
structure FunctionRelativeKey where
  token : String
  deriving DecidableEq, Repr

/-- A persistent evidence key: the enclosing function plus a key relative to that function. This is
distinct from lexical identities such as `SiteId` and `InstanceId`, which elaboration may renumber. -/
structure StableKey where
  function : FnId
  relative : FunctionRelativeKey
  deriving DecidableEq, Repr

/-- The premise and declared-assumption kinds named by decided rows: the five `Concrete.toml`
assumption sections, entropy's named source, and a source `assume`. No solver or trusted-computation
premise is introduced here. -/
inductive PremiseKind where
  | target
  | heap
  | stack
  | foreignFunction
  | trustedSurface
  | entropy
  | assume
  deriving DecidableEq, Repr

/-- The scope that makes a premise identity unique. Project assumptions use their package and a
key within its `Concrete.toml`; a source `assume` uses its function-relative stable key. -/
inductive PremiseScope where
  | package (package : PackageId) (relative : String)
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

/-- A position in source text (the trap-line decision, R-0496): the file's path relative to the
project root, with forward slashes, and a line and a column, both counted from 1. The type keeps
line and column positive; that the path is relative with forward slashes is `Span.PathOk`, which
the program's site table must satisfy (elaboration, README obligation O1). A column counts UTF-8
bytes, with a tab counting one byte (foundation question 10, 2026-11-08). -/
structure Span where
  path : String
  line : Nat
  col : Nat
  line_pos : 0 < line
  col_pos : 0 < col
  deriving DecidableEq, Repr

/-- A span's path is relative to the project root and uses forward slashes: it is not empty,
does not start with `/`, and holds no `\`. This is the minimal syntactic check; that the path is
the one normalized path of its file (no drive prefix such as `C:`, no `.` or `..` segment, no
empty segment) is part of
elaboration's site-table obligation (README obligation O1). -/
def Span.PathOk (sp : Span) : Prop :=
  sp.path.toList ≠ [] ∧ sp.path.toList.head? ≠ some '/' ∧ '\\' ∉ sp.path.toList

instance (sp : Span) : Decidable sp.PathOk := by unfold Span.PathOk; infer_instance

/-- A slice of the language, in the agreed slice order: 0 to 28, concurrency last (2026-11-07). -/
structure SliceId where
  n : Fin 29
  deriving DecidableEq, Repr

/-- A rule's identity: the Lean name of the declaration that states it and the rule's own
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

/-! ### Examples -/

/-- Two declarations named `Pair` in different modules are different declarations. -/
example : DeclId.mk (ModuleId.mk ⟨"p"⟩ ["a"]) "Pair" ≠
    DeclId.mk (ModuleId.mk ⟨"p"⟩ ["b"]) "Pair" := by decide

/-- The same module path in two packages also yields different declarations. -/
example : DeclId.mk (ModuleId.mk ⟨"p1"⟩ ["a"]) "Pair" ≠
    DeclId.mk (ModuleId.mk ⟨"p2"⟩ ["a"]) "Pair" := by decide

/-- A field belongs to its owner: `a` of `Pair` is not `a` of `Point`, and `value` of variant
`E.A` is not `value` of variant `E.B`. -/
example : FieldId.mk (.struct ⟨ModuleId.mk ⟨"p"⟩ [], "Pair"⟩) "a" ≠
    FieldId.mk (.struct ⟨ModuleId.mk ⟨"p"⟩ [], "Point"⟩) "a" := by decide

example : FieldId.mk (.variant ⟨⟨ModuleId.mk ⟨"p"⟩ [], "E"⟩, "A"⟩) "value" ≠
    FieldId.mk (.variant ⟨⟨ModuleId.mk ⟨"p"⟩ [], "E"⟩, "B"⟩) "value" := by decide

/-- The first and the last slice. -/
example : (SliceId.mk 0).n.val = 0 ∧ (SliceId.mk 28).n.val = 28 := ⟨rfl, rfl⟩

/-- A span path is relative, with forward slashes. -/
example : (Span.mk "src/main.con" 3 5 (by decide) (by decide)).PathOk := by decide

example : ¬ (Span.mk "/abs/main.con" 3 5 (by decide) (by decide)).PathOk := by decide

example : ¬ (Span.mk "src\\main.con" 3 5 (by decide) (by decide)).PathOk := by decide

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
