import ConcreteSpec.Spec.Basics
import ConcreteSpec.Spec.Slice1Semantics
import ConcreteSpec.Spec.Slice1Syntax

/-!
# Numeric local identity

Names are an external diagnostics table. The frame law below quantifies over *every* observation
of semantic state, so it applies to reads, updates, erasure, evaluation and any future serializer
without giving a semantic operation access to that table. The concrete controls additionally
pin the operations that exist today. Slice 1 has no assignment or scope-exit erasure operation;
`AList.update` and `AList.erase` are the shared map operations, not invented language semantics.
-/

namespace ConcreteSpec.LocalIdentity
open Core

/-- Replacing diagnostics preserves every observation of semantic state. For example, taking
`observe := fun env => readVar [] env ⟨7⟩ false` preserves a read when `count` becomes `total`.
The observer receives only the semantic component, never the diagnostics table. -/
theorem diagnostics_frame {σ α : Type} (observe : σ → α) (state : σ)
    (before after : LocalNames) :
    observe (state, before).1 = observe (state, after).1 := rfl

/-- Numeric identities are equal exactly when their numbers are equal. For example,
locals 7 and 8 remain distinct even if both diagnostics entries say `count`. -/
theorem numeric_identity (x y : Var) : x = y ↔ x.id = y.id := by
  cases x; cases y; simp

/-- The first binding wins, including when a later binding has the same number.
For example, reading 7 from `[(7, 10), (7, 20)]` gives 10. -/
theorem first_binding {α : Type} (x : Var) (a : α) (rest : AList Var α) :
    AList.lookup ((x, a) :: rest) x = some a := by simp [AList.lookup]

/-- A read sees the same environment after a diagnostic rename. For example, local 7
still returns 42 when its displayed name changes from `count` to `total`. -/
theorem read_presentation (Δ : Decls) (env : Env) (x : Var) (mv : Bool)
    (before after : LocalNames) :
    readVar Δ (env, before).1 x mv = readVar Δ (env, after).1 x mv := rfl

/-- Updating a binding ignores diagnostic names. For example, incrementing local 7
changes its first binding whether its displayed name is `count` or `total`. -/
theorem update_presentation {α : Type} (env : AList Var α) (x : Var) (f : α → α)
    (before after : LocalNames) :
    AList.update (env, before).1 x f = AList.update (env, after).1 x f := rfl

/-- Removing a local ignores diagnostic names. For example, erasing local 7 removes
all its bindings even if the diagnostics table was renamed. -/
theorem erase_presentation {α : Type} (env : AList Var α) (x : Var)
    (before after : LocalNames) :
    AList.erase (env, before).1 x = AList.erase (env, after).1 x := rfl

/-- Dropping an expression ignores diagnostic names. For example, `let _: u8 = copy x`
keeps the same environment when the displayed name of `x` changes. -/
theorem drop_presentation (Δ : Decls) (env : Env) (ty : Ty) (e : Expr)
    (before after : LocalNames) :
    execStmt Δ (env, before).1 (.let_ .wild ty e) =
      execStmt Δ (env, after).1 (.let_ .wild ty e) := rfl

/-- The numeric representation ignores diagnostics. For example, local 7 stays 7
when printed as `total`; `numeric_identity` ensures this representation is injective.
There is no local-variable JSON codec in the root compiler yet. -/
theorem number_presentation (x : Var) (before after : LocalNames) :
    (x, before).1.id = (x, after).1.id := rfl

-- Positive controls and negated planted results: the kernel must reject each mutant result.
example : AList.lookup ([(⟨7⟩, 10), (⟨7⟩, 20)] : AList Var Nat) ⟨7⟩ = some 10 := rfl
example : AList.lookup ([(⟨7⟩, 10), (⟨7⟩, 20)] : AList Var Nat) ⟨7⟩ ≠ some 20 := by decide

-- Equal spellings do not merge identities; changing the number does change the lookup.
example : AList.lookup ([(⟨7⟩, "count"), (⟨8⟩, "count")] : LocalNames) ⟨8⟩ =
    some "count" := rfl
example : (⟨7⟩ : Var) ≠ ⟨8⟩ := by decide
example : AList.lookup ([(⟨7⟩, 10), (⟨8⟩, 20)] : AList Var Nat) ⟨8⟩ ≠ some 10 := by decide

-- Reads: changing diagnostics leaves the actual evaluator unchanged.
example : readVar [] ((([(⟨7⟩, .val (.scalar (.int .u8 42)))],
    [(⟨7⟩, "count")]) : Env × LocalNames).1) ⟨7⟩ false =
    readVar [] ((([(⟨7⟩, .val (.scalar (.int .u8 42)))],
    [(⟨7⟩, "total")]) : Env × LocalNames).1) ⟨7⟩ false := rfl
example : readVar [] [(⟨7⟩, .val (.scalar (.int .u8 42)))] ⟨8⟩ false =
    .stuck .unbound := rfl

-- Updates change only the first matching identity; erasure removes all its bindings.
example : AList.update ([(⟨7⟩, 10), (⟨8⟩, 20), (⟨7⟩, 30)] : AList Var Nat)
    ⟨7⟩ (· + 1) = [(⟨7⟩, 11), (⟨8⟩, 20), (⟨7⟩, 30)] := rfl
example : AList.update ([(⟨7⟩, 10), (⟨8⟩, 20)] : AList Var Nat)
    ⟨7⟩ (· + 1) ≠ [(⟨7⟩, 10), (⟨8⟩, 21)] := by decide
example : AList.erase ([(⟨7⟩, 10), (⟨8⟩, 20), (⟨7⟩, 30)] : AList Var Nat)
    ⟨7⟩ = [(⟨8⟩, 20)] := rfl
example : AList.erase ([(⟨7⟩, 10), (⟨8⟩, 20), (⟨7⟩, 30)] : AList Var Nat)
    ⟨7⟩ ≠ [(⟨8⟩, 20), (⟨7⟩, 30)] := by decide

-- Dropping a copy keeps the environment; a mutant which erases its source is refused.
example : execStmt [] [(⟨7⟩, .val (.scalar (.int .u8 42)))]
    (.let_ .wild (.scalar (.int .u8)) (.atom (.copy ⟨7⟩))) =
    .ok [(⟨7⟩, .val (.scalar (.int .u8 42)))] := rfl
example : execStmt [] [(⟨7⟩, .val (.scalar (.int .u8 42)))]
    (.let_ .wild (.scalar (.int .u8)) (.atom (.copy ⟨7⟩))) ≠ .ok [] := by
  intro h; cases h
example : (⟨7⟩ : Var).id = 7 := rfl
example : (⟨7⟩ : Var).id ≠ (⟨8⟩ : Var).id := by decide

end ConcreteSpec.LocalIdentity
