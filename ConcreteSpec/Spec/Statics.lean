import ConcreteSpec.Spec.Slice1Syntax

/-!
# Slice-1 typing

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). Part of `ConcreteSpec.Spec`;
see `ConcreteSpec/README.md`.
-/

namespace ConcreteSpec

/-! ### Typing -/

/-- Variable types visible at a point. -/
abbrev TyCtx := List (Var × Ty)

/-- The scalar type of an atom used as a scalar (an operand of `+`, a struct-literal field). A
literal must fit its type (ARITH-2); a local has its declared type (TYPE-4). -/
inductive AtomTy (Γ : TyCtx) : Atom → STy → Prop where
  | int {t n} : t.inRange n = true → AtomTy Γ (.int t n) (.int t)
  | bool {b} : AtomTy Γ (.bool b) .bool
  | copy {x st} : lookupF x Γ = some (.scalar st) → AtomTy Γ (.copy x) st
  | move {x st} : lookupF x Γ = some (.scalar st) → AtomTy Γ (.move x) st

/-- A struct literal gives each declared field exactly once (TYPE-7): no field is written twice,
every written field is declared and its value has the declared type, and every declared field is
written. -/
structure FieldsTy (Γ : TyCtx) (d : StructDecl) (fs : List (Name × Atom)) : Prop where
  nodup : (fs.map Prod.fst).Nodup
  typed : ∀ p ∈ fs, ∃ st, lookupF p.1 d.fields = some st ∧ AtomTy Γ p.2 st
  covers : ∀ f st, lookupF f d.fields = some st → ∃ a, lookupF f fs = some a

/-- Every expression has exactly one type (TYPE-4).

* A literal has its type and must fit it (ARITH-2).
* `copy x` and `move x` have the type `x` was bound with.
* `l + r` takes two operands of one integer type and has that type (TYPE-6).
* A struct literal names a declared struct and gives each field exactly once (TYPE-7). -/
inductive HasType (Δ : Decls) (Γ : TyCtx) : Expr → Ty → Prop where
  | int {t n} : t.inRange n = true → HasType Δ Γ (.atom (.int t n)) (.scalar (.int t))
  | bool {b} : HasType Δ Γ (.atom (.bool b)) (.scalar .bool)
  | copy {x t} : lookupF x Γ = some t → HasType Δ Γ (.atom (.copy x)) t
  | move {x t} : lookupF x Γ = some t → HasType Δ Γ (.atom (.move x)) t
  | add {l r t} : AtomTy Γ l (.int t) → AtomTy Γ r (.int t) →
      HasType Δ Γ (.add l r) (.scalar (.int t))
  | mk {s d fs} : findDecl Δ s = some d → FieldsTy Γ d fs → HasType Δ Γ (.mk s fs) (.struct s)

end ConcreteSpec
