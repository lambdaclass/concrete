import ConcreteSpec.Spec.Basics

/-!
# Identities, sites and versions

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). Part of `ConcreteSpec.Spec`;
see `ConcreteSpec/README.md`.

Core names everything by a typed identity, never by a bare string or number: a local, a
declaration, a field, a function, a module, an instance, a loop label, a brand, an allocator
lineage, a loan, a grant, a block, a capability variable and a theorem each have their own type,
so one can never stand for another, and two declarations with the same source name in different
modules are different declarations (the facts decision: no bare-name collisions). Source names are
kept as hints for diagnostics; equality of identities is what the rules use.

A *site* (`SiteId`) is one per operation and per refusable statement. Diagnostics, facts, traps
and discharge tokens are keyed by site; a program's site table maps each site to its source
`Span`, which elaboration produces (README obligation O1).

A rule is cited by `RuleId`: the Lean name of the declaration that states it and that rule's own
revision. Every output carries one `specVersion`. Which rules exist, and how a diagnostic cites
them, is in `Checker`; this module depends on nothing downstream.

These types are part of the diagnostics and facts interface: the Lean types are the source of its
JSON schema (owner decision 2026-11-07), so a field's name is part of the interface and renaming
one is an interface change.
-/

namespace ConcreteSpec.Core

/-- A local variable: a source-name hint and a number. A local is bound at most once (Q9.1), so the
number alone identifies it; the hint is for diagnostics. -/
structure Var where
  hint : String
  id : Nat
  deriving DecidableEq, Repr

/-- A module, by its path from the package root (`["std", "bytes"]`). -/
structure ModuleId where
  path : List String
  deriving DecidableEq, Repr

/-- A type declaration (a struct or an enum): its module and its name there. Declaration names are
nominal (Q9.2): two declarations are the same exactly when their identities are equal. -/
structure DeclId where
  module : ModuleId
  name : String
  deriving DecidableEq, Repr

/-- A field (or an enum variant's field) of a declaration, by its declaration and name. -/
structure FieldId where
  decl : DeclId
  name : String
  deriving DecidableEq, Repr

/-- A function: its module and its name there. -/
structure FnId where
  module : ModuleId
  name : String
  deriving DecidableEq, Repr

/-- A trait instance, numbered by elaboration. -/
structure InstanceId where
  id : Nat
  deriving DecidableEq, Repr

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

/-- A site: one operation or one refusable statement. -/
structure SiteId where
  id : Nat
  deriving DecidableEq, Repr

/-- A position in source text: the file's path relative to the project root, with forward
slashes, and a 1-based line and column (the trap-line decision, R-0496). -/
structure Span where
  path : String
  line : Nat
  col : Nat
  deriving DecidableEq, Repr

/-- A slice of the language, in the agreed slice order: 0 to 28, concurrency last (2026-11-07). -/
structure SliceId where
  n : Nat
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
example : DeclId.mk ⟨["a"]⟩ "Pair" ≠ DeclId.mk ⟨["b"]⟩ "Pair" := by decide

/-- A field belongs to its declaration: `a` of `Pair` is not `a` of `Point`. -/
example : FieldId.mk ⟨⟨[]⟩, "Pair"⟩ "a" ≠ FieldId.mk ⟨⟨[]⟩, "Point"⟩ "a" := by decide

/-- Two locals with the same source name are different locals when their numbers differ
(shadowing, resolved by elaboration). -/
example : Var.mk "x" 0 ≠ Var.mk "x" 1 := by decide

/-- A revised rule is a different rule identity. -/
example : RuleId.mk `ConcreteSpec.Core.example 1 ≠ RuleId.mk `ConcreteSpec.Core.example 2 := by
  decide

/-- Before the freeze the version is `0.x`. -/
example : specVersion.major = 0 := rfl

end ConcreteSpec.Core
