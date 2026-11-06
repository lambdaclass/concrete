import ConcreteSpec.Spec.Statics

/-!
# Slice-1 ownership, known values and acceptance

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). Part of `ConcreteSpec.Spec`;
see `ConcreteSpec/README.md`.
-/

namespace ConcreteSpec

/-! ### Ownership and known values -/

/-- The checker's state for one binding: its type, whether it still holds its value (`live`),
whether it has been read (`used`), its statically known value, if it has one (`known`, Q6.2), and
the known values of its fields when it is a struct (`kfields`). -/
structure Entry where
  name : Var
  ty : Ty
  live : Bool
  used : Bool
  known : Option Scalar
  /-- The statically known field values of a struct binding (Q6.2, as amended 2026-11-05): for
  each field, its known scalar or `none`. Empty for a scalar binding. -/
  kfields : List (Name × Option Scalar) := []
  deriving DecidableEq, Repr

/-- The ownership context, newest binding first. -/
abbrev Ctx := List Entry

/-- The variable types of a context. -/
def Ctx.tys (Γ : Ctx) : TyCtx := Γ.map (fun e => (e.name, e.ty))

/-- The entry of `x`. -/
def ctxFind : Ctx → Var → Option Entry
  | [], _ => none
  | e :: Γ, x => if e.name = x then some e else ctxFind Γ x

/-- Record a read of `x`: it is now used, and if `moved` it no longer holds its value. -/
def ctxMark : Ctx → Var → Bool → Ctx
  | [], _, _ => []
  | e :: Γ, x, moved =>
    if e.name = x then { e with used := true, live := e.live && !moved } :: Γ
    else e :: ctxMark Γ x moved

/-- Reading an atom (OWN-1, OWN-D1, Q9.1). A literal changes nothing. `copy x` reads a live local
of a Copy type and keeps it; `move x` reads a live local of a linear type and moves the value out.
Either read marks the local used. -/
inductive UseAtom (Δ : Decls) : Ctx → Atom → Ctx → Prop where
  | int {Γ t n} : UseAtom Δ Γ (.int t n) Γ
  | bool {Γ b} : UseAtom Δ Γ (.bool b) Γ
  | copy {Γ x e} : ctxFind Γ x = some e → e.live = true → e.ty.isCopy Δ = true →
      UseAtom Δ Γ (.copy x) (ctxMark Γ x false)
  | move {Γ x e} : ctxFind Γ x = some e → e.live = true → e.ty.isCopy Δ = false →
      UseAtom Δ Γ (.move x) (ctxMark Γ x true)

/-- Reading a list of atoms left to right (STMT-4). -/
inductive UseAtoms (Δ : Decls) : Ctx → List Atom → Ctx → Prop where
  | nil {Γ} : UseAtoms Δ Γ [] Γ
  | cons {Γ Γ₁ Γ₂ a as} : UseAtom Δ Γ a Γ₁ → UseAtoms Δ Γ₁ as Γ₂ → UseAtoms Δ Γ (a :: as) Γ₂

/-- The ownership effect of an expression, left to right (STMT-4). A moved value is not used
again (OWN-1): the `live` premise refuses the second read of a linear value. -/
inductive Uses (Δ : Decls) : Ctx → Expr → Ctx → Prop where
  | atom {Γ Γ' a} : UseAtom Δ Γ a Γ' → Uses Δ Γ (.atom a) Γ'
  | add {Γ Γ₁ Γ₂ l r} : UseAtom Δ Γ l Γ₁ → UseAtom Δ Γ₁ r Γ₂ → Uses Δ Γ (.add l r) Γ₂
  | mk {Γ Γ' s fs} : UseAtoms Δ Γ (fs.map Prod.snd) Γ' → Uses Δ Γ (.mk s fs) Γ'

/-- The statically known value of an atom (Q6.2): a literal, or `copy x` of a local bound to a
known value. `move x` has no known scalar: only linear values are moved, and no scalar is linear.
A moved struct's field facts travel with it (`knownFieldsAtom`). -/
def knownAtom (Γ : Ctx) : Atom → Option Scalar
  | .int t n => some (.int t n)
  | .bool b => some (.bool b)
  | .copy x => (ctxFind Γ x).bind Entry.known
  | .move _ => none

/-- The statically known value of an expression (Q6.2): a known atom, or a checked `+` of two
known operands whose result fits. A struct literal has no scalar value; its field facts are
`knownFields`. -/
def knownExpr (Γ : Ctx) : Expr → Option Scalar
  | .atom a => knownAtom Γ a
  | .add l r => match knownAtom Γ l, knownAtom Γ r with
    | some (.int t a), some (.int t' b) =>
      if t = t' ∧ t.inRange (a + b) = true then some (.int t (a + b)) else none
    | _, _ => none
  | .mk _ _ => none

/-- The known value of field `f` in a list of field facts: `none` when the field is unknown or
absent. -/
def fieldFact (K : List (Name × Option Scalar)) (f : Name) : Option Scalar := (lookupF f K).bind id

/-- The known field values of the struct an atom reads (Q6.2): a local's recorded field facts. A
read keeps them for the receiving binding, whether it copies or moves (an ownership transfer keeps
the facts and still consumes the source). A literal is a scalar and has none. -/
def knownFieldsAtom (Γ : Ctx) : Atom → List (Name × Option Scalar)
  | .int _ _ => []
  | .bool _ => []
  | .copy x => match ctxFind Γ x with
    | some e => e.kfields
    | none => []
  | .move x => match ctxFind Γ x with
    | some e => e.kfields
    | none => []

/-- The known field values of an expression's struct value (Q6.2, as amended 2026-11-05).
Constructing a struct records each field's fact independently: a field written with a known atom
is known, any other field is unknown, and an unknown field does not discard its known siblings. A
read of a local carries the local's field facts. `+` produces a scalar. -/
def knownFields (Γ : Ctx) : Expr → List (Name × Option Scalar)
  | .atom a => knownFieldsAtom Γ a
  | .add _ _ => []
  | .mk _ fs => fs.map (fun p => (p.1, knownAtom Γ p.2))

/-- Field facts hold of a struct's field values: every known field has exactly its known value. -/
def FieldsHold (K : List (Name × Option Scalar)) (vs : List (Name × Scalar)) : Prop :=
  ∀ f s, fieldFact K f = some s → lookupF f vs = some s

/-- A statically known violation (Q6.1): a checked `+` whose operands are known, of one type,
and whose result does not fit. Such an operation is refused wherever it is written (Q6.4,
OD118), whether or not it would run. -/
def knownViolates (Γ : Ctx) : Expr → Bool
  | .add l r => match knownAtom Γ l, knownAtom Γ r with
    | some (.int t a), some (.int t' b) => decide (t = t') && !t.inRange (a + b)
    | _, _ => false
  | _ => false

/-- A new binder is a fresh identity (Q9.1): no binding of the context has it, moved or not. -/
def Fresh (Γ : Ctx) (x : Var) : Prop := ctxFind Γ x = none

/-- A destructuring pattern names every field exactly once (PAT-5): no field twice, every named
field declared, every declared field named (an ignored field written `_`). Every slice-1 field is
a scalar, so every `_` is eligible (OWN-5). -/
structure PatOk (d : StructDecl) (pat : List (Name × Binder)) : Prop where
  nodup : (pat.map Prod.fst).Nodup
  declared : ∀ p ∈ pat, ∃ st, lookupF p.1 d.fields = some st
  covers : ∀ f st, lookupF f d.fields = some st → ∃ b, lookupF f pat = some b

/-- The locals a pattern binds, in order. -/
def patNames : List (Name × Binder) → List Var
  | [] => []
  | (_, .name x) :: rest => x :: patNames rest
  | (_, .wild) :: rest => patNames rest

/-- The context after a destructuring `let`: each named field becomes a new live, unused binding
of the field's type, in pattern order. Destructuring distributes the scrutinee's field facts `K` to
the binders (Q6.2, as amended 2026-11-05): a binder of a known field is known, a binder of an
unknown field is not. -/
def bindPat (d : StructDecl) (K : List (Name × Option Scalar)) : List (Name × Binder) → Ctx → Ctx
  | [], Γ => Γ
  | (_, .wild) :: rest, Γ => bindPat d K rest Γ
  | (f, .name x) :: rest, Γ =>
    match lookupF f d.fields with
    | some st => bindPat d K rest
        ({ name := x, ty := .scalar st, live := true, used := false, known := fieldFact K f } :: Γ)
    | none => bindPat d K rest Γ

/-- Every binder of a pattern is fresh, and no two binders share an identity. -/
def PatFresh (Γ : Ctx) (pat : List (Name × Binder)) : Prop :=
  (patNames pat).Nodup ∧ ∀ x ∈ patNames pat, Fresh Γ x

/-- A statement is well typed, respects ownership and contains no statically known violation.

* `let x: T = e;` — `e` has type `T` (TYPE-4), contains no known violation (Q6.1), its reads are
  allowed (OWN-1), and `x` is fresh (Q9.1). `x` starts live and unused, with `e`'s known value and,
  for a struct, `e`'s known field values.
* `let _: T = e;` — as above, but nothing is bound and the value is dropped, so `T` must be Copy:
  `_` never discards a linear value (OWN-5). Slice 1 has no `Result` or `Option`, the other
  values `_` refuses.
* `let S { f: b, ... } = e;` — `e` has type `S`, the pattern names every field (PAT-5), the
  scrutinee is consumed (PAT-5: a linear `e` is moved), and the binders are fresh. Each binder
  receives the known value of its field, if the scrutinee has one. -/
inductive StmtOk (Δ : Decls) : Ctx → Stmt → Ctx → Prop where
  | let_ {Γ Γ₁ x t e} : HasType Δ Γ.tys e t → knownViolates Γ e = false → Uses Δ Γ e Γ₁ →
      Fresh Γ₁ x →
      StmtOk Δ Γ (.let_ (.name x) t e)
        ({ name := x, ty := t, live := true, used := false, known := knownExpr Γ e,
           kfields := knownFields Γ e } :: Γ₁)
  | letWild {Γ Γ₁ t e} : HasType Δ Γ.tys e t → knownViolates Γ e = false → t.isCopy Δ = true →
      Uses Δ Γ e Γ₁ → StmtOk Δ Γ (.let_ .wild t e) Γ₁
  | letStruct {Γ Γ₁ s d pat e} : HasType Δ Γ.tys e (.struct s) → knownViolates Γ e = false →
      findDecl Δ s = some d → PatOk d pat → Uses Δ Γ e Γ₁ → PatFresh Γ₁ pat →
      StmtOk Δ Γ (.letStruct s pat e) (bindPat d (knownFields Γ e) pat Γ₁)

/-- At `return`, every linear binding has been consumed (OWN-2) and every binding has been used
(OWN-5). -/
def Discharged (Δ : Decls) (Γ : Ctx) : Prop :=
  ∀ e ∈ Γ, (e.ty.isCopy Δ = true ∨ e.live = false) ∧ e.used = true

/-- A body is accepted: each statement in order, then `return a` with `a : u8` (STMT-7), after
which nothing linear is left and every binding was used. An atom contains no operation, so the
`return` has no known violation to check. -/
inductive BodyOk (Δ : Decls) : Ctx → List Stmt → Atom → Prop where
  | ret {Γ Γ₁ r} : HasType Δ Γ.tys (.atom r) (.scalar (.int .u8)) → Uses Δ Γ (.atom r) Γ₁ →
      Discharged Δ Γ₁ → BodyOk Δ Γ [] r
  | cons {Γ Γ₁ s ss r} : StmtOk Δ Γ s Γ₁ → BodyOk Δ Γ₁ ss r → BodyOk Δ Γ (s :: ss) r

/-- Struct declarations are well formed: struct names are distinct, none is `main`, and each
struct's field names are distinct. -/
def DeclsOk (Δ : Decls) : Prop :=
  (Δ.map StructDecl.name).Nodup ∧ (∀ d ∈ Δ, d.name ≠ "main" ∧ (d.fields.map Prod.fst).Nodup)

/-- A slice-1 program is accepted: its declarations are well formed and its body is accepted,
including the mandatory static refusal of known violations (Q6.1–Q6.6). -/
def Checked (p : Program) : Prop := DeclsOk p.decls ∧ BodyOk p.decls [] p.body p.ret

end ConcreteSpec
