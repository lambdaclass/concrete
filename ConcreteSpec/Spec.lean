/-!
# Concrete specification — slice 1

**Status: provisional model, not yet normative.** Core's shape is the owner's decision (ROADMAP
A2, the Core session) and is not taken yet. This file models slice 1 ahead of that decision so the
proof architecture can be exercised; it becomes the normative specification only once Core is
approved, and every choice below that Core may change is listed under "Choices pending Core".

This file covers slice 1 of the new compiler (ROADMAP phase B). The Lean definitions are the rules
of the model. Each rule has a plain-words doc comment naming the decided rule it
encodes (docs/spec/*.md rule IDs and the owner decisions ODnn in docs/spec/decisions.md), and
conformance examples at the end of the file show accepted and refused programs.

What slice 1 contains:

* the scalar types: the eight integer types (TYPE-19) and `bool`;
* `let` with a type, a struct literal, and a struct-destructuring `let` (PAT-5);
* struct declarations, `Copy` or not (TYPE-14), with scalar fields; the slice's central case is
  an empty struct without `Copy`, moved and then consumed by a destructuring `let`;
* the use-after-move refusal (OWN-1), consumption before exit (OWN-2) and used bindings (OWN-5);
* one checked operation, integer `+` (ARITH-3), which traps on overflow;
* one function, `main`, with no parameters, whose body is a list of `let` statements followed by
  `return e` with `e : u8` (STMT-7, STMT-9).

What this file is not:

* It is not a parser or an elaborator. Programs are given as abstract syntax. In particular an
  integer literal carries its type (`Atom.int t n`): how surface literals receive a type
  (ARITH-2, TYPE-4) and how an unannotated `let` gets its type are elaboration rules for a later
  file, which must be proved to produce this syntax.
* Nothing outside slice 1 is modelled: no calls, no branches, no loops, no references, no heap, no
  capabilities, no `defer`. Struct fields are scalars only.

The judgments (`HasType`, `Uses`, `StmtOk`, `BodyOk`, `Checked`) are inductive relations,
independent of the executable checker (`check`); `check_iff` proves the two agree. The semantics is
a total, fuel-indexed evaluator (`run`) whose outcome separates exit, trap, stuck and running out
of fuel (STMT-1). `Exec` is the fuel-free form of the same statement-level evaluator: adequacy and
determinism are about fuel, not an independent cross-check of expression evaluation.

Choices pending Core (ROADMAP A2; not decided by this file):

* names are strings, not Core identities (the approved architecture asks for Core with identities);
* a read of a variable copies or moves according to its type, where the Core proposal (Q9.1)
  recommends explicit moves and copies in Core;
* integer literals carry their type in the syntax;
* the body is a list of `let` statements followed by one `return`.

Not modelled, because the decision is open:

* ARITH-13, refusing an operation proved to overflow or divide by zero: its mandatory proof
  criterion is undecided (docs/spec/decisions.md, core-static-refusal). `Checked` is the slice's
  rules without ARITH-13, so it accepts programs that ARITH-13 may later refuse, and no example
  claims that a constant overflow is accepted.

Obligations not discharged here (ROADMAP phase B):

* the elaboration relation from surface syntax: ARITH-2's expected-type rule for literals, OD138's
  propagation positions, and unannotated `let`;
* the lexer, model-translation and native-image obligations;
* erasure of the instrumentation (`Slot.moved`, the `used` and `live` flags): stated as intent,
  with no erasure definition or theorem yet.

Only Lean core is imported. No proof here uses `native_decide` or `sorry`.
-/

namespace ConcreteSpec

abbrev Name := String

/-! ## Lists keyed by name -/

/-- The first value bound to `x` in a list of named pairs. -/
def lookupF {α : Type} (x : Name) : List (Name × α) → Option α
  | [] => none
  | (y, a) :: rest => if y = x then some a else lookupF x rest

theorem lookupF_mem {α : Type} {x : Name} {a : α} :
    ∀ {l : List (Name × α)}, lookupF x l = some a → (x, a) ∈ l
  | [], h => by simp [lookupF] at h
  | (y, b) :: rest, h => by
    unfold lookupF at h
    split at h
    · cases h; subst_vars; exact List.Mem.head _
    · exact List.Mem.tail _ (lookupF_mem h)

theorem lookupF_of_mem {α : Type} {x : Name} {a : α} :
    ∀ {l : List (Name × α)}, (x, a) ∈ l → ∃ b, lookupF x l = some b
  | [], h => by cases h
  | (y, b) :: rest, h => by
    unfold lookupF
    split
    · exact ⟨b, rfl⟩
    · cases h with
      | head => contradiction
      | tail _ h => exact lookupF_of_mem h

/-! ## Types -/

/-- The integer types (TYPE-19): signed `i8`…`i64` and unsigned `u8`…`u64`, each as many bits wide
as its name says. -/
inductive IntTy where
  | i8 | i16 | i32 | i64 | u8 | u16 | u32 | u64
  deriving DecidableEq, Repr

namespace IntTy

/-- The width in bits (TYPE-19). -/
def bits : IntTy → Nat
  | i8 => 8 | i16 => 16 | i32 => 32 | i64 => 64
  | u8 => 8 | u16 => 16 | u32 => 32 | u64 => 64

/-- Whether the type is signed (TYPE-19). -/
def signed : IntTy → Bool
  | i8 | i16 | i32 | i64 => true
  | u8 | u16 | u32 | u64 => false

/-- The smallest value: `-2^(bits-1)` for a signed type, `0` for an unsigned one. -/
def min (t : IntTy) : Int := if t.signed then -((2 : Int) ^ (t.bits - 1)) else 0

/-- The largest value: `2^(bits-1) - 1` for a signed type, `2^bits - 1` for an unsigned one. -/
def max (t : IntTy) : Int := if t.signed then (2 : Int) ^ (t.bits - 1) - 1 else (2 : Int) ^ t.bits - 1

/-- `n` is representable in `t`. A literal must fit its type (ARITH-2), and a checked operation
traps when its mathematical result does not fit (ARITH-3). -/
def inRange (t : IntTy) (n : Int) : Bool := decide (t.min ≤ n ∧ n ≤ t.max)

end IntTy

/-- A scalar type: an integer type or `bool`. -/
inductive STy where
  | int (t : IntTy)
  | bool
  deriving DecidableEq, Repr

/-- A type in slice 1: a scalar or a named struct. -/
inductive Ty where
  | scalar (t : STy)
  | struct (s : Name)
  deriving DecidableEq, Repr

/-- A struct declaration. `copy` records whether it is declared `Copy`: Copy is declared, never
derived (TYPE-14). In slice 1 every field is a scalar, so a `Copy` declaration always satisfies
OWN-14 (a `Copy` declaration has only Copy fields). -/
structure StructDecl where
  name : Name
  copy : Bool
  fields : List (Name × STy)
  deriving DecidableEq, Repr

abbrev Decls := List StructDecl

/-- The declaration named `s`. -/
def findDecl (Δ : Decls) (s : Name) : Option StructDecl := Δ.find? (fun d => d.name == s)

/-- Copy or linear (OWN-D1). Every scalar is Copy. A struct is Copy exactly when its declaration
says `Copy` (TYPE-14); every other type is linear. -/
def Ty.isCopy (Δ : Decls) : Ty → Bool
  | .scalar _ => true
  | .struct s => match findDecl Δ s with
    | some d => d.copy
    | none => false

/-! ## Syntax -/

/-- An atom: a typed integer literal, a `bool` literal or a variable. Struct literal fields are
atoms in slice 1. -/
inductive Atom where
  | int (t : IntTy) (n : Int)
  | bool (b : Bool)
  | var (x : Name)
  deriving DecidableEq, Repr

/-- An expression: an atom, checked integer addition `l + r` (ARITH-3), or a struct literal
`S { f: a, ... }` (TYPE-7). -/
inductive Expr where
  | atom (a : Atom)
  | add (l r : Expr)
  | mk (s : Name) (fields : List (Name × Atom))
  deriving DecidableEq, Repr

/-- A destructuring pattern binds a field to a name, or ignores it with `_` (PAT-1, PAT-5). -/
inductive Binder where
  | name (x : Name)
  | wild
  deriving DecidableEq, Repr

/-- A statement: `let x: T = e;`, `let _: T = e;`, or a destructuring `let S { f: b, ... } = e;`
(PAT-5). -/
inductive Stmt where
  | let_ (b : Binder) (t : Ty) (e : Expr)
  | letStruct (s : Name) (pat : List (Name × Binder)) (e : Expr)
  deriving DecidableEq, Repr

/-- A slice-1 program: struct declarations and `fn main() -> u8 { body; return ret; }` (STMT-7:
`main` returns `u8` and may declare no grants; STMT-9: the body ends in `return`). -/
structure Program where
  decls : Decls
  body : List Stmt
  ret : Expr
  deriving Repr

/-! ## Typing -/

/-- Variable types visible at a point. -/
abbrev TyCtx := List (Name × Ty)

/-- The type of an atom used as a scalar (a struct-literal field). A literal must fit its type
(ARITH-2); a variable has its declared type (TYPE-4). -/
inductive AtomTy (Γ : TyCtx) : Atom → STy → Prop where
  | int {t n} : t.inRange n = true → AtomTy Γ (.int t n) (.int t)
  | bool {b} : AtomTy Γ (.bool b) .bool
  | var {x st} : lookupF x Γ = some (.scalar st) → AtomTy Γ (.var x) st

/-- A struct literal gives each declared field exactly once (TYPE-7): no field is written twice,
every written field is declared and its value has the declared type, and every declared field is
written. -/
structure FieldsTy (Γ : TyCtx) (d : StructDecl) (fs : List (Name × Atom)) : Prop where
  nodup : (fs.map Prod.fst).Nodup
  typed : ∀ p ∈ fs, ∃ st, lookupF p.1 d.fields = some st ∧ AtomTy Γ p.2 st
  covers : ∀ f st, lookupF f d.fields = some st → ∃ a, lookupF f fs = some a

/-- Every expression has exactly one type (TYPE-4).

* A literal has its type and must fit it (ARITH-2).
* A variable has the type it was bound with.
* `l + r` takes two operands of one integer type and has that type (TYPE-6).
* A struct literal names a declared struct and gives each field exactly once (TYPE-7). -/
inductive HasType (Δ : Decls) (Γ : TyCtx) : Expr → Ty → Prop where
  | int {t n} : t.inRange n = true → HasType Δ Γ (.atom (.int t n)) (.scalar (.int t))
  | bool {b} : HasType Δ Γ (.atom (.bool b)) (.scalar .bool)
  | var {x t} : lookupF x Γ = some t → HasType Δ Γ (.atom (.var x)) t
  | add {l r t} : HasType Δ Γ l (.scalar (.int t)) → HasType Δ Γ r (.scalar (.int t)) →
      HasType Δ Γ (.add l r) (.scalar (.int t))
  | mk {s d fs} : findDecl Δ s = some d → FieldsTy Γ d fs → HasType Δ Γ (.mk s fs) (.struct s)

/-! ## Ownership -/

/-- The ownership state of one binding: its type, whether it still holds its value (`live`) and
whether it has been read (`used`). -/
structure Entry where
  name : Name
  ty : Ty
  live : Bool
  used : Bool
  deriving DecidableEq, Repr

/-- The ownership context, newest binding first. -/
abbrev Ctx := List Entry

/-- The variable types of a context. -/
def Ctx.tys (Γ : Ctx) : TyCtx := Γ.map (fun e => (e.name, e.ty))

/-- The entry of `x`. -/
def ctxFind : Ctx → Name → Option Entry
  | [], _ => none
  | e :: Γ, x => if e.name = x then some e else ctxFind Γ x

/-- Record a read of `x`: it is now used, and if `moved` it no longer holds its value. -/
def ctxMark : Ctx → Name → Bool → Ctx
  | [], _, _ => []
  | e :: Γ, x, moved =>
    if e.name = x then { e with used := true, live := e.live && !moved } :: Γ
    else e :: ctxMark Γ x moved

/-- Reading an atom (OWN-1, OWN-D1). A literal changes nothing. A variable must still hold its
value; reading it marks it used, and if its type is linear the read moves the value out. -/
inductive UseAtom (Δ : Decls) : Ctx → Atom → Ctx → Prop where
  | int {Γ t n} : UseAtom Δ Γ (.int t n) Γ
  | bool {Γ b} : UseAtom Δ Γ (.bool b) Γ
  | var {Γ x e} : ctxFind Γ x = some e → e.live = true →
      UseAtom Δ Γ (.var x) (ctxMark Γ x (!e.ty.isCopy Δ))

/-- Reading a list of atoms left to right (STMT-4). -/
inductive UseAtoms (Δ : Decls) : Ctx → List Atom → Ctx → Prop where
  | nil {Γ} : UseAtoms Δ Γ [] Γ
  | cons {Γ Γ₁ Γ₂ a as} : UseAtom Δ Γ a Γ₁ → UseAtoms Δ Γ₁ as Γ₂ → UseAtoms Δ Γ (a :: as) Γ₂

/-- The ownership effect of an expression, left to right (STMT-4). A moved value is not used
again (OWN-1): the `var` premise `live` refuses the second read of a linear value. -/
inductive Uses (Δ : Decls) : Ctx → Expr → Ctx → Prop where
  | atom {Γ Γ' a} : UseAtom Δ Γ a Γ' → Uses Δ Γ (.atom a) Γ'
  | add {Γ Γ₁ Γ₂ l r} : Uses Δ Γ l Γ₁ → Uses Δ Γ₁ r Γ₂ → Uses Δ Γ (.add l r) Γ₂
  | mk {Γ Γ' s fs} : UseAtoms Δ Γ (fs.map Prod.snd) Γ' → Uses Δ Γ (.mk s fs) Γ'

/-- A new binder reuses no visible name: not a local binding (moved or not, SCOPE-D1), not a
struct and not `main`, since types and values share one namespace (SCOPE-1, SCOPE-2). `_` is not a
name: it is the wildcard `Binder.wild`. -/
def Fresh (Δ : Decls) (Γ : Ctx) (x : Name) : Prop :=
  ctxFind Γ x = none ∧ findDecl Δ x = none ∧ x ≠ "main" ∧ x ≠ "_"

/-- A destructuring pattern names every field exactly once (PAT-5): no field twice, every named
field declared, every declared field named (an ignored field written `_`). Every slice-1 field is
a scalar, so every `_` is eligible (OWN-5). -/
structure PatOk (d : StructDecl) (pat : List (Name × Binder)) : Prop where
  nodup : (pat.map Prod.fst).Nodup
  declared : ∀ p ∈ pat, ∃ st, lookupF p.1 d.fields = some st
  covers : ∀ f st, lookupF f d.fields = some st → ∃ b, lookupF f pat = some b

/-- The names a pattern binds, in order. -/
def patNames : List (Name × Binder) → List Name
  | [] => []
  | (_, .name x) :: rest => x :: patNames rest
  | (_, .wild) :: rest => patNames rest

/-- The context after a destructuring `let`: each named field becomes a new live, unused binding
of the field's type, in pattern order. -/
def bindPat (d : StructDecl) : List (Name × Binder) → Ctx → Ctx
  | [], Γ => Γ
  | (_, .wild) :: rest, Γ => bindPat d rest Γ
  | (f, .name x) :: rest, Γ =>
    match lookupF f d.fields with
    | some st => bindPat d rest ({ name := x, ty := .scalar st, live := true, used := false } :: Γ)
    | none => bindPat d rest Γ

/-- Every binder of a pattern is fresh, and no two binders share a name (SCOPE-1). -/
def PatFresh (Δ : Decls) (Γ : Ctx) (pat : List (Name × Binder)) : Prop :=
  (patNames pat).Nodup ∧ ∀ x ∈ patNames pat, Fresh Δ Γ x

/-- A statement is well typed and respects ownership.

* `let x: T = e;` — `e` has type `T` (TYPE-4), its reads are allowed (OWN-1), and `x` is fresh
  (SCOPE-1). `x` starts live and unused.
* `let _: T = e;` — as above, but nothing is bound and the value is dropped, so `T` must be Copy:
  `_` never discards a linear value (OWN-5; decisions.md, "`_` names an unused binding"). Slice 1
  has no `Result` or `Option`, the other values `_` refuses.
* `let S { f: b, ... } = e;` — `e` has type `S`, the pattern names every field (PAT-5), the
  scrutinee is consumed (PAT-5: reading a linear `e` moves it), and the binders are fresh. -/
inductive StmtOk (Δ : Decls) : Ctx → Stmt → Ctx → Prop where
  | let_ {Γ Γ₁ x t e} : HasType Δ Γ.tys e t → Uses Δ Γ e Γ₁ → Fresh Δ Γ₁ x →
      StmtOk Δ Γ (.let_ (.name x) t e) ({ name := x, ty := t, live := true, used := false } :: Γ₁)
  | letWild {Γ Γ₁ t e} : HasType Δ Γ.tys e t → t.isCopy Δ = true → Uses Δ Γ e Γ₁ →
      StmtOk Δ Γ (.let_ .wild t e) Γ₁
  | letStruct {Γ Γ₁ s d pat e} : HasType Δ Γ.tys e (.struct s) → findDecl Δ s = some d →
      PatOk d pat → Uses Δ Γ e Γ₁ → PatFresh Δ Γ₁ pat →
      StmtOk Δ Γ (.letStruct s pat e) (bindPat d pat Γ₁)

/-- At `return`, every linear binding has been consumed (OWN-2) and every binding has been used
(OWN-5). -/
def Discharged (Δ : Decls) (Γ : Ctx) : Prop :=
  ∀ e ∈ Γ, (e.ty.isCopy Δ = true ∨ e.live = false) ∧ e.used = true

/-- A body is well typed and respects ownership: each statement in order, then `return r` with
`r : u8` (STMT-7), after which nothing linear is left and every binding was used. -/
inductive BodyOk (Δ : Decls) : Ctx → List Stmt → Expr → Prop where
  | ret {Γ Γ₁ r} : HasType Δ Γ.tys r (.scalar (.int .u8)) → Uses Δ Γ r Γ₁ → Discharged Δ Γ₁ →
      BodyOk Δ Γ [] r
  | cons {Γ Γ₁ s ss r} : StmtOk Δ Γ s Γ₁ → BodyOk Δ Γ₁ ss r → BodyOk Δ Γ (s :: ss) r

/-- Struct declarations are well formed: struct names are distinct, none is `main`, and each
struct's field names are distinct. -/
def DeclsOk (Δ : Decls) : Prop :=
  (Δ.map StructDecl.name).Nodup ∧ (∀ d ∈ Δ, d.name ≠ "main" ∧ (d.fields.map Prod.fst).Nodup)

/-- A program is accepted by the rules this file models. ARITH-13 is not among them (see the
header), so this is not yet the full acceptance judgment. -/
def Checked (p : Program) : Prop := DeclsOk p.decls ∧ BodyOk p.decls [] p.body p.ret

/-! ## Semantics -/

/-- A scalar value. -/
inductive Scalar where
  | int (t : IntTy) (n : Int)
  | bool (b : Bool)
  deriving DecidableEq, Repr

/-- A value: a scalar, or a struct with its field values. -/
inductive Val where
  | scalar (v : Scalar)
  | struct (s : Name) (fields : List (Name × Scalar))
  deriving DecidableEq, Repr

/-- A binding's slot holds a value, or has been moved out of. Slots are instrumentation: the
runtime representation erases them, and a moved slot exists to make a read after a move
observable in the model. -/
inductive Slot where
  | val (v : Val)
  | moved
  deriving DecidableEq, Repr

/-- The environment, newest binding first. -/
abbrev Env := List (Name × Slot)

/-- The defined traps of slice 1 (ARITH-3). -/
inductive Trap where
  | arithmeticOverflow
  deriving DecidableEq, Repr

/-- Why execution got stuck. Safe code never gets stuck (STMT-1); `checked_never_stuck` proves it
for slice 1. -/
inductive Stuck where
  | movedRead
  | unbound
  | typeError
  deriving DecidableEq, Repr

/-- The result of one evaluation step. -/
inductive R (α : Type) where
  | ok (a : α)
  | trap (k : Trap)
  | stuck (w : Stuck)
  deriving Repr

/-- How a run ends (STMT-1): exit with a status, abort at a defined trap (STMT-2), stuck, or the
evaluator's fuel ran out. Running out of fuel is inconclusive; it is not divergence. -/
inductive Outcome where
  | exit (code : Nat)
  | trap (k : Trap)
  | stuck (w : Stuck)
  | outOfFuel
  deriving DecidableEq, Repr

/-- The slot of `x`. -/
def envFind : Env → Name → Option Slot
  | [], _ => none
  | (y, s) :: env, x => if y = x then some s else envFind env x

/-- Mark `x` moved. -/
def envMove : Env → Name → Env
  | [], _ => []
  | (y, s) :: env, x => if y = x then (y, .moved) :: env else (y, s) :: envMove env x

/-- Whether a value is Copy (OWN-D1, TYPE-14). -/
def Val.isCopy (Δ : Decls) : Val → Bool
  | .scalar _ => true
  | .struct s _ => match findDecl Δ s with
    | some d => d.copy
    | none => false

/-- Read a variable. A moved slot is stuck. Reading a linear value moves it out (OWN-D1). -/
def readVar (Δ : Decls) (env : Env) (x : Name) : R (Val × Env) :=
  match envFind env x with
  | none => .stuck .unbound
  | some .moved => .stuck .movedRead
  | some (.val v) => .ok (v, if v.isCopy Δ then env else envMove env x)

/-- Evaluate an atom that must be a scalar. -/
def evalAtom (Δ : Decls) (env : Env) : Atom → R (Scalar × Env)
  | .int t n => .ok (.int t n, env)
  | .bool b => .ok (.bool b, env)
  | .var x => match readVar Δ env x with
    | .ok (.scalar v, env') => .ok (v, env')
    | .ok _ => .stuck .typeError
    | .trap k => .trap k
    | .stuck w => .stuck w

/-- Evaluate struct-literal fields in the order written (STMT-4). -/
def evalFields (Δ : Decls) : Env → List (Name × Atom) → R (List (Name × Scalar) × Env)
  | env, [] => .ok ([], env)
  | env, (f, a) :: rest => match evalAtom Δ env a with
    | .ok (v, env₁) => match evalFields Δ env₁ rest with
      | .ok (vs, env₂) => .ok ((f, v) :: vs, env₂)
      | .trap k => .trap k
      | .stuck w => .stuck w
    | .trap k => .trap k
    | .stuck w => .stuck w

/-- Checked addition (ARITH-3): the mathematical sum if it fits the operand type, else the
`arithmeticOverflow` trap. -/
def checkedAdd (t : IntTy) (a b : Int) : R Val :=
  if t.inRange (a + b) then .ok (.scalar (.int t (a + b))) else .trap .arithmeticOverflow

/-- Evaluate an expression, left operand first (STMT-4). Expressions are finite trees, so this is
total without fuel. -/
def evalExpr (Δ : Decls) : Env → Expr → R (Val × Env)
  | env, .atom (.int t n) => .ok (.scalar (.int t n), env)
  | env, .atom (.bool b) => .ok (.scalar (.bool b), env)
  | env, .atom (.var x) => readVar Δ env x
  | env, .add l r => match evalExpr Δ env l with
    | .ok (.scalar (.int t a), env₁) => match evalExpr Δ env₁ r with
      | .ok (.scalar (.int t' b), env₂) =>
        if t = t' then match checkedAdd t a b with
          | .ok v => .ok (v, env₂)
          | .trap k => .trap k
          | .stuck w => .stuck w
        else .stuck .typeError
      | .ok _ => .stuck .typeError
      | .trap k => .trap k
      | .stuck w => .stuck w
    | .ok _ => .stuck .typeError
    | .trap k => .trap k
    | .stuck w => .stuck w
  | env, .mk s fs => match evalFields Δ env fs with
    | .ok (vs, env₁) => .ok (.struct s vs, env₁)
    | .trap k => .trap k
    | .stuck w => .stuck w

/-- Bind the named fields of a destructured struct value, in pattern order. -/
def destructure : List (Name × Binder) → List (Name × Scalar) → Env → R Env
  | [], _, env => .ok env
  | (_, .wild) :: rest, vs, env => destructure rest vs env
  | (f, .name x) :: rest, vs, env => match lookupF f vs with
    | some v => destructure rest vs ((x, .val (.scalar v)) :: env)
    | none => .stuck .typeError

/-- Execute one statement. A destructuring `let` consumes its scrutinee and binds its fields. -/
def execStmt (Δ : Decls) (env : Env) : Stmt → R Env
  | .let_ (.name x) _ e => match evalExpr Δ env e with
    | .ok (v, env₁) => .ok ((x, .val v) :: env₁)
    | .trap k => .trap k
    | .stuck w => .stuck w
  | .let_ .wild _ e => match evalExpr Δ env e with
    | .ok (_, env₁) => .ok env₁
    | .trap k => .trap k
    | .stuck w => .stuck w
  | .letStruct s pat e => match evalExpr Δ env e with
    | .ok (.struct s' vs, env₁) => if s' = s then destructure pat vs env₁ else .stuck .typeError
    | .ok _ => .stuck .typeError
    | .trap k => .trap k
    | .stuck w => .stuck w

/-- The outcome of `return r`: the `u8` value is the exit status (STMT-7). -/
def retOutcome (Δ : Decls) (env : Env) (r : Expr) : Outcome :=
  match evalExpr Δ env r with
  | .ok (.scalar (.int .u8 k), _) => .exit k.toNat
  | .ok _ => .stuck .typeError
  | .trap k => .trap k
  | .stuck w => .stuck w

/-- The fuel-indexed evaluator: each statement and the final `return` costs one unit of fuel. -/
def runBody (Δ : Decls) : Nat → Env → List Stmt → Expr → Outcome
  | 0, _, _, _ => .outOfFuel
  | _ + 1, env, [], r => retOutcome Δ env r
  | n + 1, env, s :: ss, r => match execStmt Δ env s with
    | .ok env' => runBody Δ n env' ss r
    | .trap k => .trap k
    | .stuck w => .stuck w

/-- Run a program with the given fuel, from an empty environment. -/
def run (p : Program) (fuel : Nat) : Outcome := runBody p.decls fuel [] p.body p.ret

/-- The fuel-free semantics: the outcome a body reaches. -/
inductive Exec (Δ : Decls) : Env → List Stmt → Expr → Outcome → Prop where
  | ret {env r} : Exec Δ env [] r (retOutcome Δ env r)
  | step {env env' s ss r o} : execStmt Δ env s = .ok env' → Exec Δ env' ss r o →
      Exec Δ env (s :: ss) r o
  | trap {env s ss r k} : execStmt Δ env s = .trap k → Exec Δ env (s :: ss) r (.trap k)
  | stuck {env s ss r w} : execStmt Δ env s = .stuck w → Exec Δ env (s :: ss) r (.stuck w)

/-! ## Properties of the semantics -/

/-- Fuel monotonicity: a run that finishes with some fuel finishes the same way with more. -/
theorem runBody_mono {Δ : Decls} {r : Expr} :
    ∀ {n : Nat} {env : Env} {ss : List Stmt} {o : Outcome}, runBody Δ n env ss r = o →
      o ≠ .outOfFuel → ∀ m, n ≤ m → runBody Δ m env ss r = o := by
  intro n
  induction n with
  | zero => intro env ss o h hne; simp [runBody] at h; exact absurd h.symm hne
  | succ n ih =>
    intro env ss o h hne m hm
    obtain ⟨m, rfl⟩ : ∃ m', m = m' + 1 := ⟨m - 1, by omega⟩
    cases ss with
    | nil => simpa [runBody] using h
    | cons s ss =>
      simp only [runBody] at h ⊢
      split at h
      · exact ih h hne m (by omega)
      · exact h
      · exact h

/-- The `return` step never runs out of fuel. -/
theorem retOutcome_ne_outOfFuel (Δ : Decls) (env : Env) (r : Expr) :
    retOutcome Δ env r ≠ .outOfFuel := by
  unfold retOutcome; split <;> simp

/-- A body of `k` statements finishes with `k + 1` units of fuel. -/
theorem runBody_enough {Δ : Decls} {r : Expr} :
    ∀ (ss : List Stmt) (env : Env), runBody Δ (ss.length + 1) env ss r ≠ .outOfFuel
  | [], env => by simpa [runBody] using retOutcome_ne_outOfFuel Δ env r
  | s :: ss, env => by
    simp only [List.length_cons, runBody]
    split
    · exact runBody_enough ss _
    · simp
    · simp

/-- The evaluator is sound for `Exec`: an outcome reached with some fuel is an `Exec` outcome. -/
theorem runBody_sound {Δ : Decls} {r : Expr} :
    ∀ {n : Nat} {env : Env} {ss : List Stmt} {o : Outcome}, runBody Δ n env ss r = o →
      o ≠ .outOfFuel → Exec Δ env ss r o := by
  intro n
  induction n with
  | zero => intro env ss o h hne; simp [runBody] at h; exact absurd h.symm hne
  | succ n ih =>
    intro env ss o h hne
    cases ss with
    | nil => simp only [runBody] at h; subst h; exact .ret
    | cons s ss =>
      simp only [runBody] at h
      split at h
      · rename_i env' he; exact .step he (ih h hne)
      · rename_i k he; subst h; exact .trap he
      · rename_i w he; subst h; exact .stuck he

/-- The evaluator is complete for `Exec`: every `Exec` outcome is reached with enough fuel. -/
theorem exec_complete {Δ : Decls} {r : Expr} {env : Env} {ss : List Stmt} {o : Outcome}
    (h : Exec Δ env ss r o) : runBody Δ (ss.length + 1) env ss r = o := by
  induction h with
  | ret => simp [runBody]
  | step he _ ih => simp only [List.length_cons, runBody, he]; exact ih
  | trap he => simp [runBody, he]
  | stuck he => simp [runBody, he]

/-- `Exec` never ends in `outOfFuel`. -/
theorem exec_ne_outOfFuel {Δ : Decls} {r : Expr} {env : Env} {ss : List Stmt} {o : Outcome}
    (h : Exec Δ env ss r o) : o ≠ .outOfFuel := by
  induction h with
  | ret => exact retOutcome_ne_outOfFuel _ _ _
  | step _ _ ih => exact ih
  | trap => simp
  | stuck => simp

/-- Determinism: a body has at most one outcome. -/
theorem exec_deterministic {Δ : Decls} {r : Expr} {env : Env} {ss : List Stmt} {o₁ o₂ : Outcome}
    (h₁ : Exec Δ env ss r o₁) (h₂ : Exec Δ env ss r o₂) : o₁ = o₂ := by
  rw [← exec_complete h₁, ← exec_complete h₂]

/-- Totality: every body has exactly one outcome (slice 1 has no loops or calls). -/
theorem exec_total (Δ : Decls) (env : Env) (ss : List Stmt) (r : Expr) :
    ∃ o, Exec Δ env ss r o :=
  ⟨_, runBody_sound rfl (runBody_enough ss env)⟩

/-! ## Safety: accepted programs never get stuck -/

/-- A scalar value has a scalar type: an integer of its type and in its range, or a `bool`. -/
inductive SVTy : Scalar → STy → Prop where
  | int {t n} : t.inRange n = true → SVTy (.int t n) (.int t)
  | bool {b} : SVTy (.bool b) .bool

/-- A value has a type: a scalar of its scalar type, or a struct value of a declared struct with
a well-typed value for every declared field. -/
inductive VTy (Δ : Decls) : Val → Ty → Prop where
  | scalar {v st} : SVTy v st → VTy Δ (.scalar v) (.scalar st)
  | struct {s d vs} : findDecl Δ s = some d →
      (∀ f st, lookupF f d.fields = some st → ∃ v, lookupF f vs = some v ∧ SVTy v st) →
      VTy Δ (.struct s vs) (.struct s)

/-- The ownership context describes the environment: the same names in the same order, and every
live binding holds a value of its type. A binding the context records as moved is unconstrained
here; the evaluator marks such a slot `moved` when it moves the value out. -/
inductive Corr (Δ : Decls) : Ctx → Env → Prop where
  | nil : Corr Δ [] []
  | cons {e sl Γ env} : (e.live = true → ∃ v, sl = .val v ∧ VTy Δ v e.ty) → Corr Δ Γ env →
      Corr Δ (e :: Γ) ((e.name, sl) :: env)

theorem tys_lookup (x : Name) : ∀ Γ : Ctx, lookupF x Γ.tys = (ctxFind Γ x).map Entry.ty
  | [] => rfl
  | e :: Γ => by
    simp only [Ctx.tys, List.map_cons, lookupF, ctxFind] at *
    split
    · rfl
    · exact tys_lookup x Γ

theorem ctxMark_tys (x : Name) (mv : Bool) : ∀ Γ : Ctx, (ctxMark Γ x mv).tys = Γ.tys
  | [] => rfl
  | e :: Γ => by
    simp only [ctxMark]
    split
    · rfl
    · simp only [Ctx.tys, List.map_cons] at *
      rw [show (ctxMark Γ x mv).map (fun e => (e.name, e.ty)) = (ctxMark Γ x mv).tys from rfl,
        ctxMark_tys x mv Γ]
      rfl

theorem useAtom_tys {Δ : Decls} {Γ Γ' : Ctx} {a : Atom} (h : UseAtom Δ Γ a Γ') : Γ'.tys = Γ.tys := by
  cases h with
  | int => rfl
  | bool => rfl
  | var => exact ctxMark_tys _ _ _

theorem useAtoms_tys {Δ : Decls} {Γ Γ' : Ctx} {as : List Atom} (h : UseAtoms Δ Γ as Γ') :
    Γ'.tys = Γ.tys := by
  induction h with
  | nil => rfl
  | cons h₁ _ ih => rw [ih, useAtom_tys h₁]

theorem uses_tys {Δ : Decls} {Γ Γ' : Ctx} {e : Expr} (h : Uses Δ Γ e Γ') : Γ'.tys = Γ.tys := by
  induction h with
  | atom h => exact useAtom_tys h
  | add _ _ ih₁ ih₂ => rw [ih₂, ih₁]
  | mk h => exact useAtoms_tys h

theorem vty_isCopy {Δ : Decls} {v : Val} {t : Ty} (h : VTy Δ v t) : v.isCopy Δ = t.isCopy Δ := by
  cases h <;> rfl

theorem corr_find {Δ : Decls} {x : Name} {Γ : Ctx} {env : Env} (hc : Corr Δ Γ env) :
    ∀ {e : Entry}, ctxFind Γ x = some e → e.live = true →
      ∃ v, envFind env x = some (.val v) ∧ VTy Δ v e.ty := by
  induction hc with
  | nil => intro e h; simp [ctxFind] at h
  | cons hsl _ ih =>
    rename_i e' sl Γ env
    intro e h hl
    simp only [ctxFind, envFind] at h ⊢
    split at h
    · cases h
      rename_i heq
      obtain ⟨v, rfl, hv⟩ := hsl hl
      exact ⟨v, by simp [heq], hv⟩
    · rename_i hne
      simp only [hne, ↓reduceIte]
      exact ih h hl

theorem corr_mark_copy {Δ : Decls} {x : Name} {Γ : Ctx} {env : Env} (hc : Corr Δ Γ env) :
    Corr Δ (ctxMark Γ x false) env := by
  induction hc with
  | nil => exact .nil
  | cons hsl hc ih =>
    simp only [ctxMark]
    split
    · exact .cons (by simpa using hsl) hc
    · exact .cons hsl ih

theorem corr_mark_move {Δ : Decls} {x : Name} {Γ : Ctx} {env : Env} (hc : Corr Δ Γ env) :
    Corr Δ (ctxMark Γ x true) (envMove env x) := by
  induction hc with
  | nil => exact .nil
  | cons hsl hc ih =>
    rename_i e sl Γ env
    simp only [ctxMark, envMove]
    split
    · exact .cons (by simp) hc
    · exact .cons hsl ih

/-- Reading a live variable succeeds with a value of its type, and the context's record of the
read matches what the evaluator did. -/
theorem readVar_safe {Δ : Decls} {Γ : Ctx} {env : Env} {x : Name} {e : Entry}
    (hc : Corr Δ Γ env) (hf : ctxFind Γ x = some e) (hl : e.live = true) :
    ∃ v env', readVar Δ env x = .ok (v, env') ∧ VTy Δ v e.ty ∧
      Corr Δ (ctxMark Γ x (!e.ty.isCopy Δ)) env' := by
  obtain ⟨v, hv, hty⟩ := corr_find hc hf hl
  refine ⟨v, _, by simp [readVar, hv]; rfl, hty, ?_⟩
  rw [vty_isCopy hty]
  cases hcopy : e.ty.isCopy Δ
  · simpa [hcopy] using corr_mark_move hc
  · simpa [hcopy] using corr_mark_copy hc

theorem evalAtom_safe {Δ : Decls} {Γ Γ' : Ctx} {env : Env} {a : Atom} {st : STy}
    (hc : Corr Δ Γ env) (ht : AtomTy Γ.tys a st) (hu : UseAtom Δ Γ a Γ') :
    ∃ v env', evalAtom Δ env a = .ok (v, env') ∧ SVTy v st ∧ Corr Δ Γ' env' := by
  cases hu with
  | int => cases ht with | int h => exact ⟨_, _, rfl, .int h, hc⟩
  | bool => cases ht with | bool => exact ⟨_, _, rfl, .bool, hc⟩
  | var hf hl =>
    rename_i x e
    cases ht with
    | var hx =>
      rw [tys_lookup, hf] at hx
      simp only [Option.map_some, Option.some.injEq] at hx
      obtain ⟨v, env', hr, hv, hc'⟩ := readVar_safe hc hf hl
      rw [hx] at hv
      cases hv with
      | scalar hs =>
        refine ⟨_, env', ?_, hs, hc'⟩
        simp [evalAtom, hr]

theorem evalFields_safe {Δ : Decls} :
    ∀ {fs : List (Name × Atom)} {Γ Γ' : Ctx} {env : Env}, Corr Δ Γ env →
      (∀ p ∈ fs, ∃ st, AtomTy Γ.tys p.2 st) → UseAtoms Δ Γ (fs.map Prod.snd) Γ' →
      ∃ vs env', evalFields Δ env fs = .ok (vs, env') ∧ Corr Δ Γ' env' ∧
        ∀ f a, lookupF f fs = some a → ∀ st, AtomTy Γ.tys a st →
          ∃ v, lookupF f vs = some v ∧ SVTy v st
  | [], Γ, Γ', env, hc, _, hu => by
    cases hu
    exact ⟨[], env, rfl, hc, by intro f a h; simp [lookupF] at h⟩
  | (g, a) :: rest, Γ, Γ', env, hc, ht, hu => by
    cases hu with
    | cons hu₁ hu₂ =>
      rename_i Γ₁
      obtain ⟨st₀, ha⟩ := ht (g, a) (List.Mem.head _)
      obtain ⟨v, env₁, he, _, hc₁⟩ := evalAtom_safe hc ha hu₁
      have htys : Γ₁.tys = Γ.tys := useAtom_tys hu₁
      have ht' : ∀ p ∈ rest, ∃ st, AtomTy Γ₁.tys p.2 st := by
        intro p hp; rw [htys]; exact ht p (List.Mem.tail _ hp)
      obtain ⟨vs, env₂, hr, hc₂, hl⟩ := evalFields_safe hc₁ ht' hu₂
      refine ⟨(g, v) :: vs, env₂, by simp [evalFields, he, hr], hc₂, ?_⟩
      intro f a' hf st hst
      simp only [lookupF] at hf ⊢
      split at hf
      next hgf =>
        cases hf
        obtain ⟨v', env', he', hv', _⟩ := evalAtom_safe hc hst hu₁
        rw [he] at he'
        cases he'
        exact ⟨v, by simp [hgf], hv'⟩
      next hne =>
        simp only [hne, ↓reduceIte]
        exact hl f a' hf st (by rw [htys]; exact hst)

/-- Expression safety: a well-typed expression whose reads are allowed evaluates to a value of its
type, or traps. It never gets stuck, and in particular never reads a moved variable. -/
theorem evalExpr_safe {Δ : Decls} :
    ∀ {e : Expr} {Γ Γ' : Ctx} {env : Env} {t : Ty}, Corr Δ Γ env → HasType Δ Γ.tys e t →
      Uses Δ Γ e Γ' →
      (∃ k, evalExpr Δ env e = .trap k) ∨
      ∃ v env', evalExpr Δ env e = .ok (v, env') ∧ VTy Δ v t ∧ Corr Δ Γ' env'
  | .atom (.int t n), Γ, Γ', env, _, hc, ht, hu => by
    cases ht with
    | int h => cases hu with | atom hu => cases hu; exact .inr ⟨_, _, rfl, .scalar (.int h), hc⟩
  | .atom (.bool b), Γ, Γ', env, _, hc, ht, hu => by
    cases ht with
    | bool => cases hu with | atom hu => cases hu; exact .inr ⟨_, _, rfl, .scalar .bool, hc⟩
  | .atom (.var x), Γ, Γ', env, t, hc, ht, hu => by
    cases ht with
    | var hx =>
      cases hu with
      | atom hu =>
        cases hu with
        | var hf hl =>
          rw [tys_lookup, hf] at hx
          simp only [Option.map_some, Option.some.injEq] at hx
          subst hx
          obtain ⟨v, env', hr, hv, hc'⟩ := readVar_safe hc hf hl
          exact .inr ⟨v, env', by simpa [evalExpr] using hr, hv, hc'⟩
  | .add l r, Γ, Γ', env, _, hc, ht, hu => by
    cases ht with
    | add hl hr =>
      rename_i t
      cases hu with
      | add hul hur =>
        rename_i Γ₁
        rcases evalExpr_safe hc hl hul with ⟨k, hk⟩ | ⟨v₁, env₁, he₁, hv₁, hc₁⟩
        · exact .inl ⟨k, by simp [evalExpr, hk]⟩
        · rw [← uses_tys hul] at hr
          cases hv₁ with
          | scalar hs₁ =>
            cases hs₁ with
            | int hin₁ =>
              rename_i a
              rcases evalExpr_safe hc₁ hr hur with ⟨k, hk⟩ | ⟨v₂, env₂, he₂, hv₂, hc₂⟩
              · exact .inl ⟨k, by simp [evalExpr, he₁, hk]⟩
              · cases hv₂ with
                | scalar hs₂ =>
                  cases hs₂ with
                  | int hin₂ =>
                    rename_i b
                    by_cases hsum : t.inRange (a + b) = true
                    · exact .inr ⟨_, env₂, by simp [evalExpr, he₁, he₂, checkedAdd, hsum],
                        .scalar (.int hsum), hc₂⟩
                    · exact .inl ⟨.arithmeticOverflow,
                        by simp [evalExpr, he₁, he₂, checkedAdd, hsum]⟩
  | .mk s fs, Γ, Γ', env, _, hc, ht, hu => by
    cases ht with
    | mk hd hf =>
      rename_i d
      cases hu with
      | mk hu =>
        have htyped : ∀ p ∈ fs, ∃ st, AtomTy Γ.tys p.2 st := by
          intro p hp
          obtain ⟨st, _, h⟩ := hf.typed p hp
          exact ⟨st, h⟩
        obtain ⟨vs, env', he, hc', hl⟩ := evalFields_safe hc htyped hu
        refine .inr ⟨.struct s vs, env', by simp [evalExpr, he], .struct hd ?_, hc'⟩
        intro f st hfs
        obtain ⟨a, ha⟩ := hf.covers f st hfs
        obtain ⟨st', hst', hat⟩ := hf.typed (f, a) (lookupF_mem ha)
        rw [hfs] at hst'
        cases hst'
        exact hl f a ha st hat

theorem destructure_safe {Δ : Decls} {d : StructDecl} {vs : List (Name × Scalar)}
    (hvs : ∀ f st, lookupF f d.fields = some st → ∃ v, lookupF f vs = some v ∧ SVTy v st) :
    ∀ {pat : List (Name × Binder)} {Γ : Ctx} {env : Env}, Corr Δ Γ env →
      (∀ p ∈ pat, ∃ st, lookupF p.1 d.fields = some st) →
      ∃ env', destructure pat vs env = .ok env' ∧ Corr Δ (bindPat d pat Γ) env'
  | [], Γ, env, hc, _ => ⟨env, rfl, hc⟩
  | (f, .wild) :: rest, Γ, env, hc, hp => by
    simp only [destructure, bindPat]
    exact destructure_safe hvs hc (fun p h => hp p (List.Mem.tail _ h))
  | (f, .name x) :: rest, Γ, env, hc, hp => by
    obtain ⟨st, hst⟩ := hp (f, .name x) (List.Mem.head _)
    obtain ⟨v, hv, hsv⟩ := hvs f st hst
    simp only [destructure, bindPat, hst, hv]
    exact destructure_safe hvs
      (Corr.cons (e := { name := x, ty := .scalar st, live := true, used := false })
        (fun _ => ⟨_, rfl, .scalar hsv⟩) hc)
      (fun p h => hp p (List.Mem.tail _ h))

/-- Statement safety: a checked statement executes or traps; it never gets stuck. -/
theorem execStmt_safe {Δ : Decls} {Γ Γ' : Ctx} {env : Env} {s : Stmt}
    (hc : Corr Δ Γ env) (hs : StmtOk Δ Γ s Γ') :
    (∃ k, execStmt Δ env s = .trap k) ∨ ∃ env', execStmt Δ env s = .ok env' ∧ Corr Δ Γ' env' := by
  cases hs with
  | let_ ht hu _ =>
    rcases evalExpr_safe hc ht hu with ⟨k, hk⟩ | ⟨v, env₁, he, hv, hc₁⟩
    · exact .inl ⟨k, by simp [execStmt, hk]⟩
    · exact .inr ⟨_, by simp [execStmt, he], .cons (fun _ => ⟨v, rfl, hv⟩) hc₁⟩
  | letWild ht _ hu =>
    rcases evalExpr_safe hc ht hu with ⟨k, hk⟩ | ⟨v, env₁, he, _, hc₁⟩
    · exact .inl ⟨k, by simp [execStmt, hk]⟩
    · exact .inr ⟨_, by simp [execStmt, he], hc₁⟩
  | letStruct ht hd hp hu _ =>
    rcases evalExpr_safe hc ht hu with ⟨k, hk⟩ | ⟨v, env₁, he, hv, hc₁⟩
    · exact .inl ⟨k, by simp [execStmt, hk]⟩
    · cases hv with
      | struct hd' hvs =>
        rw [hd] at hd'
        cases hd'
        obtain ⟨env', hde, hc'⟩ := destructure_safe hvs hc₁ hp.declared
        exact .inr ⟨env', by simp [execStmt, he, hde], hc'⟩

/-- Body safety: from a described environment, a checked body never gets stuck, for any fuel. -/
theorem runBody_safe {Δ : Decls} {Γ : Ctx} {ss : List Stmt} {r : Expr} (hb : BodyOk Δ Γ ss r) :
    ∀ {env : Env}, Corr Δ Γ env → ∀ n w, runBody Δ n env ss r ≠ .stuck w := by
  induction hb with
  | ret ht hu _ =>
    intro env hc n w
    cases n with
    | zero => simp [runBody]
    | succ n =>
      simp only [runBody, retOutcome]
      rcases evalExpr_safe hc ht hu with ⟨k, hk⟩ | ⟨v, env', he, hv, _⟩
      · simp [hk]
      · cases hv with
        | scalar hs => cases hs with | int _ => simp [he]
  | cons hs _ ih =>
    intro env hc n w
    cases n with
    | zero => simp [runBody]
    | succ n =>
      simp only [runBody]
      rcases execStmt_safe hc hs with ⟨k, hk⟩ | ⟨env', he, hc'⟩
      · simp [hk]
      · simp only [he]; exact ih hc' n w

/-- **Slice-1 safety.** An accepted program never gets stuck, with any amount of fuel: it never
reads a moved variable, never reads an unbound one and never meets a value of the wrong type
(STMT-1: safe code never reaches a stuck state; OWN-1). -/
theorem checked_never_stuck {p : Program} (h : Checked p) : ∀ n w, run p n ≠ .stuck w :=
  runBody_safe h.2 .nil

/-- In particular an accepted program never reads a moved variable (OWN-1). -/
theorem checked_no_use_after_move {p : Program} (h : Checked p) (n : Nat) :
    run p n ≠ .stuck .movedRead :=
  checked_never_stuck h n _

/-- **Slice-1 behaviour.** An accepted program has exactly one outcome, and it is an exit or a
defined trap (STMT-1, ARITH-3). -/
theorem checked_exits_or_traps {p : Program} (h : Checked p) :
    ∃ o, Exec p.decls [] p.body p.ret o ∧ ((∃ c, o = .exit c) ∨ ∃ k, o = .trap k) := by
  obtain ⟨o, ho⟩ := exec_total p.decls [] p.body p.ret
  refine ⟨o, ho, ?_⟩
  have hfuel := exec_complete ho
  have hns := checked_never_stuck h (p.body.length + 1)
  have hnf := exec_ne_outOfFuel ho
  unfold run at hns
  rw [hfuel] at hns
  cases o with
  | exit c => exact .inl ⟨c, rfl⟩
  | trap k => exact .inr ⟨k, rfl⟩
  | stuck w => exact absurd rfl (hns w)
  | outOfFuel => exact absurd rfl hnf

/-! ## The checker and its correspondence with the judgments

`check` is an executable checker. It is not the specification: the judgments above are.
`check_iff` proves the two agree, which is also what lets the conformance examples below be
decided by evaluation. -/

instance (Δ : Decls) (Γ : Ctx) (x : Name) : Decidable (Fresh Δ Γ x) := by
  unfold Fresh; infer_instance

instance (Δ : Decls) (Γ : Ctx) (pat : List (Name × Binder)) : Decidable (PatFresh Δ Γ pat) := by
  unfold PatFresh; infer_instance

instance (Δ : Decls) (Γ : Ctx) : Decidable (Discharged Δ Γ) := by
  unfold Discharged; infer_instance

instance (Δ : Decls) : Decidable (DeclsOk Δ) := by
  unfold DeclsOk; infer_instance

/-- The scalar type of an atom, if it has one. -/
def atomTy (Γ : TyCtx) : Atom → Option STy
  | .int t n => if t.inRange n then some (.int t) else none
  | .bool _ => some .bool
  | .var x => match lookupF x Γ with
    | some (.scalar st) => some st
    | _ => none

theorem atomTy_iff {Γ : TyCtx} {a : Atom} {st : STy} : atomTy Γ a = some st ↔ AtomTy Γ a st := by
  constructor
  · intro h
    cases a with
    | int t n =>
      simp only [atomTy] at h
      split at h
      · cases h; exact .int (by assumption)
      · cases h
    | bool b => simp only [atomTy, Option.some.injEq] at h; subst h; exact .bool
    | var x =>
      simp only [atomTy] at h
      split at h
      · cases h; exact .var (by assumption)
      · cases h
  · intro h
    cases h with
    | int h => simp [atomTy, h]
    | bool => rfl
    | var h => simp [atomTy, h]

/-- The executable form of `FieldsTy`. -/
def fieldsOkB (Γ : TyCtx) (d : StructDecl) (fs : List (Name × Atom)) : Bool :=
  decide ((fs.map Prod.fst).Nodup) &&
  fs.all (fun p => match lookupF p.1 d.fields with
    | some st => atomTy Γ p.2 == some st
    | none => false) &&
  d.fields.all (fun q => (lookupF q.1 fs).isSome)

theorem covers_iff {α β : Type} {l : List (Name × α)} {m : List (Name × β)} :
    (∀ f a, lookupF f l = some a → ∃ b, lookupF f m = some b) ↔
      l.all (fun q => (lookupF q.1 m).isSome) = true := by
  rw [List.all_eq_true]
  constructor
  · intro h q hq
    obtain ⟨a, ha⟩ := lookupF_of_mem hq
    obtain ⟨b, hb⟩ := h q.1 a ha
    simp [hb]
  · intro h f a ha
    have := h (f, a) (lookupF_mem ha)
    simp only [Option.isSome_iff_exists] at this
    exact this

theorem fieldsOkB_iff {Γ : TyCtx} {d : StructDecl} {fs : List (Name × Atom)} :
    fieldsOkB Γ d fs = true ↔ FieldsTy Γ d fs := by
  simp only [fieldsOkB, Bool.and_eq_true, decide_eq_true_eq, ← covers_iff]
  constructor
  · rintro ⟨⟨hn, ht⟩, hc⟩
    refine ⟨hn, ?_, hc⟩
    intro p hp
    have := (List.all_eq_true.mp ht) p hp
    split at this
    · rename_i st hst
      exact ⟨st, hst, atomTy_iff.mp (by simpa using this)⟩
    · cases this
  · rintro ⟨hn, ht, hc⟩
    refine ⟨⟨hn, List.all_eq_true.mpr ?_⟩, hc⟩
    intro p hp
    obtain ⟨st, hst, ha⟩ := ht p hp
    simp [hst, atomTy_iff.mpr ha]

/-- The type of an expression, if it has one. -/
def tyOf (Δ : Decls) (Γ : TyCtx) : Expr → Option Ty
  | .atom (.int t n) => if t.inRange n then some (.scalar (.int t)) else none
  | .atom (.bool _) => some (.scalar .bool)
  | .atom (.var x) => lookupF x Γ
  | .add l r => match tyOf Δ Γ l, tyOf Δ Γ r with
    | some (.scalar (.int t)), some (.scalar (.int t')) =>
      if t = t' then some (.scalar (.int t)) else none
    | _, _ => none
  | .mk s fs => match findDecl Δ s with
    | some d => if fieldsOkB Γ d fs then some (.struct s) else none
    | none => none

theorem tyOf_iff {Δ : Decls} {Γ : TyCtx} :
    ∀ {e : Expr} {t : Ty}, tyOf Δ Γ e = some t ↔ HasType Δ Γ e t
  | .atom (.int t n), ty => by
    simp only [tyOf]
    constructor
    · intro h; split at h
      · cases h; exact .int (by assumption)
      · cases h
    · intro h; cases h with | int h => simp [h]
  | .atom (.bool b), ty => by
    simp only [tyOf, Option.some.injEq]
    constructor
    · intro h; subst h; exact .bool
    · intro h; cases h; rfl
  | .atom (.var x), ty => by
    simp only [tyOf]
    constructor
    · exact .var
    · intro h; cases h with | var h => exact h
  | .add l r, ty => by
    constructor
    · intro h
      simp only [tyOf] at h
      split at h
      · rename_i t t' hl hr
        split at h
        · cases h; subst_vars
          exact .add (tyOf_iff.mp hl) (tyOf_iff.mp hr)
        · cases h
      · cases h
    · intro h
      cases h with
      | add hl hr => simp [tyOf, tyOf_iff.mpr hl, tyOf_iff.mpr hr]
  | .mk s fs, ty => by
    constructor
    · intro h
      simp only [tyOf] at h
      split at h
      · rename_i d hd
        split at h
        · cases h; exact .mk hd (fieldsOkB_iff.mp (by assumption))
        · cases h
      · cases h
    · intro h
      cases h with
      | mk hd hf => simp [tyOf, hd, fieldsOkB_iff.mpr hf]

/-- The context after reading an atom, if the read is allowed. -/
def useAtom (Δ : Decls) (Γ : Ctx) : Atom → Option Ctx
  | .int _ _ => some Γ
  | .bool _ => some Γ
  | .var x => match ctxFind Γ x with
    | some e => if e.live then some (ctxMark Γ x (!e.ty.isCopy Δ)) else none
    | none => none

/-- The context after reading atoms left to right. -/
def useAtoms (Δ : Decls) (Γ : Ctx) : List Atom → Option Ctx
  | [] => some Γ
  | a :: as => match useAtom Δ Γ a with
    | some Γ₁ => useAtoms Δ Γ₁ as
    | none => none

/-- The context after evaluating an expression, if its reads are allowed. -/
def uses (Δ : Decls) (Γ : Ctx) : Expr → Option Ctx
  | .atom a => useAtom Δ Γ a
  | .add l r => match uses Δ Γ l with
    | some Γ₁ => uses Δ Γ₁ r
    | none => none
  | .mk _ fs => useAtoms Δ Γ (fs.map Prod.snd)

theorem useAtom_iff {Δ : Decls} {Γ Γ' : Ctx} {a : Atom} :
    useAtom Δ Γ a = some Γ' ↔ UseAtom Δ Γ a Γ' := by
  constructor
  · intro h
    cases a with
    | int => cases h; exact .int
    | bool => cases h; exact .bool
    | var x =>
      simp only [useAtom] at h
      split at h
      · rename_i e he
        split at h
        · cases h; exact .var he (by assumption)
        · cases h
      · cases h
  · intro h
    cases h with
    | int => rfl
    | bool => rfl
    | var hf hl => simp [useAtom, hf, hl]

theorem useAtoms_iff {Δ : Decls} :
    ∀ {Γ Γ' : Ctx} {as : List Atom}, useAtoms Δ Γ as = some Γ' ↔ UseAtoms Δ Γ as Γ'
  | Γ, Γ', [] => by
    simp only [useAtoms, Option.some.injEq]
    constructor
    · intro h; subst h; exact .nil
    · intro h; cases h; rfl
  | Γ, Γ', a :: as => by
    simp only [useAtoms]
    constructor
    · intro h
      split at h
      · rename_i Γ₁ h₁; exact .cons (useAtom_iff.mp h₁) (useAtoms_iff.mp h)
      · cases h
    · intro h
      cases h with
      | cons h₁ h₂ => rw [useAtom_iff.mpr h₁]; exact useAtoms_iff.mpr h₂

theorem uses_iff {Δ : Decls} : ∀ {Γ Γ' : Ctx} {e : Expr}, uses Δ Γ e = some Γ' ↔ Uses Δ Γ e Γ'
  | Γ, Γ', .atom a => by
    simp only [uses]
    constructor
    · intro h; exact .atom (useAtom_iff.mp h)
    · intro h; cases h with | atom h => exact useAtom_iff.mpr h
  | Γ, Γ', .add l r => by
    simp only [uses]
    constructor
    · intro h
      split at h
      · rename_i Γ₁ h₁; exact .add (uses_iff.mp h₁) (uses_iff.mp h)
      · cases h
    · intro h
      cases h with
      | add h₁ h₂ => rw [uses_iff.mpr h₁]; exact uses_iff.mpr h₂
  | Γ, Γ', .mk s fs => by
    simp only [uses]
    constructor
    · intro h; exact .mk (useAtoms_iff.mp h)
    · intro h; cases h with | mk h => exact useAtoms_iff.mpr h

/-- The executable form of `PatOk`. -/
def patOkB (d : StructDecl) (pat : List (Name × Binder)) : Bool :=
  decide ((pat.map Prod.fst).Nodup) &&
  pat.all (fun p => (lookupF p.1 d.fields).isSome) &&
  d.fields.all (fun q => (lookupF q.1 pat).isSome)

theorem patOkB_iff {d : StructDecl} {pat : List (Name × Binder)} : patOkB d pat = true ↔ PatOk d pat := by
  simp only [patOkB, Bool.and_eq_true, decide_eq_true_eq]
  constructor
  · rintro ⟨⟨hn, hd⟩, hc⟩
    refine ⟨hn, ?_, covers_iff.mpr hc⟩
    intro p hp
    have := (List.all_eq_true.mp hd) p hp
    simpa [Option.isSome_iff_exists] using this
  · rintro ⟨hn, hd, hc⟩
    refine ⟨⟨hn, List.all_eq_true.mpr ?_⟩, covers_iff.mp hc⟩
    intro p hp
    obtain ⟨st, hst⟩ := hd p hp
    simp [hst]

/-- The context after a statement, if the statement is accepted. -/
def stmtOut (Δ : Decls) (Γ : Ctx) : Stmt → Option Ctx
  | .let_ (.name x) t e =>
    if tyOf Δ Γ.tys e = some t then
      match uses Δ Γ e with
      | some Γ₁ => if Fresh Δ Γ₁ x then
          some ({ name := x, ty := t, live := true, used := false } :: Γ₁) else none
      | none => none
    else none
  | .let_ .wild t e =>
    if tyOf Δ Γ.tys e = some t ∧ t.isCopy Δ = true then uses Δ Γ e else none
  | .letStruct s pat e =>
    if tyOf Δ Γ.tys e = some (.struct s) then
      match findDecl Δ s with
      | some d => if patOkB d pat then
          match uses Δ Γ e with
          | some Γ₁ => if PatFresh Δ Γ₁ pat then some (bindPat d pat Γ₁) else none
          | none => none
        else none
      | none => none
    else none

theorem stmtOut_iff {Δ : Decls} {Γ Γ' : Ctx} {s : Stmt} :
    stmtOut Δ Γ s = some Γ' ↔ StmtOk Δ Γ s Γ' := by
  constructor
  · intro h
    cases s with
    | let_ b t e =>
      cases b with
      | wild =>
        simp only [stmtOut] at h
        split at h
        · rename_i hc
          exact .letWild (tyOf_iff.mp hc.1) hc.2 (uses_iff.mp h)
        · cases h
      | name x =>
          simp only [stmtOut] at h
          split at h
          · rename_i ht
            split at h
            · rename_i Γ₁ hu
              split at h
              · cases h; exact .let_ (tyOf_iff.mp ht) (uses_iff.mp hu) (by assumption)
              · cases h
            · cases h
          · cases h
    | letStruct s pat e =>
      simp only [stmtOut] at h
      split at h
      · rename_i ht
        split at h
        · rename_i d hd
          split at h
          · rename_i hp
            split at h
            · rename_i Γ₁ hu
              split at h
              · cases h
                exact .letStruct (tyOf_iff.mp ht) hd (patOkB_iff.mp hp) (uses_iff.mp hu)
                  (by assumption)
              · cases h
            · cases h
          · cases h
        · cases h
      · cases h
  · intro h
    cases h with
    | let_ ht hu hf => simp [stmtOut, tyOf_iff.mpr ht, uses_iff.mpr hu, hf]
    | letWild ht hcopy hu => simp [stmtOut, tyOf_iff.mpr ht, hcopy, uses_iff.mpr hu]
    | letStruct ht hd hp hu hf =>
      simp [stmtOut, tyOf_iff.mpr ht, hd, patOkB_iff.mpr hp, uses_iff.mpr hu, hf]

/-- The executable form of `BodyOk`. -/
def bodyOkB (Δ : Decls) (Γ : Ctx) : List Stmt → Expr → Bool
  | [], r => tyOf Δ Γ.tys r == some (.scalar (.int .u8)) &&
      match uses Δ Γ r with
      | some Γ₁ => decide (Discharged Δ Γ₁)
      | none => false
  | s :: ss, r => match stmtOut Δ Γ s with
    | some Γ₁ => bodyOkB Δ Γ₁ ss r
    | none => false

theorem bodyOkB_iff {Δ : Decls} :
    ∀ {Γ : Ctx} {ss : List Stmt} {r : Expr}, bodyOkB Δ Γ ss r = true ↔ BodyOk Δ Γ ss r
  | Γ, [], r => by
    simp only [bodyOkB, Bool.and_eq_true, beq_iff_eq]
    constructor
    · rintro ⟨ht, hu⟩
      split at hu
      · rename_i Γ₁ hu'
        exact .ret (tyOf_iff.mp ht) (uses_iff.mp hu') (by simpa using hu)
      · cases hu
    · intro h
      cases h with
      | ret ht hu hd => simp [tyOf_iff.mpr ht, uses_iff.mpr hu, hd]
  | Γ, s :: ss, r => by
    simp only [bodyOkB]
    constructor
    · intro h
      split at h
      · rename_i Γ₁ hs; exact .cons (stmtOut_iff.mp hs) (bodyOkB_iff.mp h)
      · cases h
    · intro h
      cases h with
      | cons hs hb => rw [stmtOut_iff.mpr hs]; exact bodyOkB_iff.mpr hb

/-- The executable checker. -/
def check (p : Program) : Bool := decide (DeclsOk p.decls) && bodyOkB p.decls [] p.body p.ret

/-- **Checker correspondence.** The checker accepts a program exactly when the judgments do. -/
theorem check_iff (p : Program) : check p = true ↔ Checked p := by
  simp only [check, Bool.and_eq_true, decide_eq_true_eq, bodyOkB_iff, Checked]

end ConcreteSpec

/-! ## Conformance examples

Each example gives the surface program in its doc comment, its abstract syntax, and what the
specification says about it: accepted with an outcome, or refused. Accepted examples are proved
with `check_iff` and run with the evaluator; refused ones are proved not `Checked`. -/

namespace ConcreteSpec.Examples

open ConcreteSpec

/-- `struct Token {}` — empty, not `Copy`, so linear (OWN-D1). -/
def token : StructDecl := { name := "Token", copy := false, fields := [] }

/-- `struct Copy Unit0 {}` — empty and declared `Copy` (TYPE-14). -/
def unit0 : StructDecl := { name := "Unit0", copy := true, fields := [] }

/-- `struct Pair { a: u8, b: u8 }` — not `Copy`. -/
def pair : StructDecl := { name := "Pair", copy := false, fields := [("a", .int .u8), ("b", .int .u8)] }

def decls : Decls := [token, unit0, pair]

def lit (n : Int) : Expr := .atom (.int .u8 n)
def v (x : Name) : Expr := .atom (.var x)

/-- The slice's central program: an empty linear struct is created, moved, and consumed by a
destructuring `let` (OWN-1, OWN-2, PAT-5). It exits with 42.
```
fn main() -> u8 {
    let t: Token = Token {};
    let u: Token = t;
    let Token {} = u;
    return 42;
}
``` -/
def moveThenConsume : Program :=
  { decls, body := [.let_ (.name "t") (.struct "Token") (.mk "Token" []),
                    .let_ (.name "u") (.struct "Token") (v "t"),
                    .letStruct "Token" [] (v "u")],
    ret := lit 42 }

example : Checked moveThenConsume := (check_iff _).mp (by decide)
example : run moveThenConsume 10 = .exit 42 := by decide

/-- Refused: `t` is used after it was moved into `u` (OWN-1).
```
fn main() -> u8 {
    let t: Token = Token {};
    let u: Token = t;
    let Token {} = t;      // error: t was moved
    let Token {} = u;
    return 42;
}
``` -/
def useAfterMove : Program :=
  { decls, body := [.let_ (.name "t") (.struct "Token") (.mk "Token" []),
                    .let_ (.name "u") (.struct "Token") (v "t"),
                    .letStruct "Token" [] (v "t"),
                    .letStruct "Token" [] (v "u")],
    ret := lit 42 }

example : ¬ Checked useAfterMove := fun h => absurd ((check_iff _).mpr h) (by decide)

/-- The refusal is what keeps the program from getting stuck: run anyway, the model reads a moved
slot. This is the behaviour `checked_no_use_after_move` rules out for accepted programs. -/
example : run useAfterMove 10 = .stuck .movedRead := by decide

/-- Refused: the linear `t` is never consumed before `return` (OWN-2).
```
fn main() -> u8 {
    let t: Token = Token {};
    return 42;             // error: t is not consumed
}
``` -/
def notConsumed : Program :=
  { decls, body := [.let_ (.name "t") (.struct "Token") (.mk "Token" [])], ret := lit 42 }

example : ¬ Checked notConsumed := fun h => absurd ((check_iff _).mpr h) (by decide)

/-- Accepted: a `Copy` struct is copied, not moved, so it can be read twice (OWN-D1, TYPE-14).
```
fn main() -> u8 {
    let p: Unit0 = Unit0 {};
    let q: Unit0 = p;
    let Unit0 {} = p;
    let Unit0 {} = q;
    return 0;
}
``` -/
def copyTwice : Program :=
  { decls, body := [.let_ (.name "p") (.struct "Unit0") (.mk "Unit0" []),
                    .let_ (.name "q") (.struct "Unit0") (v "p"),
                    .letStruct "Unit0" [] (v "p"),
                    .letStruct "Unit0" [] (v "q")],
    ret := lit 0 }

example : Checked copyTwice := (check_iff _).mp (by decide)
example : run copyTwice 10 = .exit 0 := by decide

/-- Refused: `x` is bound and never used (OWN-5).
```
fn main() -> u8 {
    let x: u8 = 1;         // error: x is never used
    return 42;
}
``` -/
def unusedBinding : Program :=
  { decls, body := [.let_ (.name "x") (.scalar (.int .u8)) (lit 1)], ret := lit 42 }

example : ¬ Checked unusedBinding := fun h => absurd ((check_iff _).mpr h) (by decide)

/-- Accepted: destructuring binds every field, here under new names (PAT-1, PAT-5), and checked
addition in range (ARITH-3). Exits with 42.
```
fn main() -> u8 {
    let p: Pair = Pair { a: 40, b: 2 };
    let Pair { a: x, b: y } = p;
    return x + y;
}
``` -/
def destructureAdd : Program :=
  { decls, body := [.let_ (.name "p") (.struct "Pair") (.mk "Pair" [("a", .int .u8 40), ("b", .int .u8 2)]),
                    .letStruct "Pair" [("a", .name "x"), ("b", .name "y")] (v "p")],
    ret := .add (v "x") (v "y") }

example : Checked destructureAdd := (check_iff _).mp (by decide)
example : run destructureAdd 10 = .exit 42 := by decide

/-- Refused: the pattern does not name field `b` (PAT-5).
```
fn main() -> u8 {
    let p: Pair = Pair { a: 40, b: 2 };
    let Pair { a: x } = p;   // error: field b is not named
    return x;
}
``` -/
def missingField : Program :=
  { decls, body := [.let_ (.name "p") (.struct "Pair") (.mk "Pair" [("a", .int .u8 40), ("b", .int .u8 2)]),
                    .letStruct "Pair" [("a", .name "x")] (v "p")],
    ret := v "x" }

example : ¬ Checked missingField := fun h => absurd ((check_iff _).mpr h) (by decide)

/-- Accepted with `_` for an ignored field (PAT-5; a scalar field is an eligible wildcard, OWN-5).
```
fn main() -> u8 {
    let p: Pair = Pair { a: 42, b: 0 };
    let Pair { a: x, b: _ } = p;
    return x;
}
``` -/
def wildcardField : Program :=
  { decls, body := [.let_ (.name "p") (.struct "Pair") (.mk "Pair" [("a", .int .u8 42), ("b", .int .u8 0)]),
                    .letStruct "Pair" [("a", .name "x"), ("b", .wild)] (v "p")],
    ret := v "x" }

example : Checked wildcardField := (check_iff _).mp (by decide)
example : run wildcardField 10 = .exit 42 := by decide

/-- Traps when run: `200 + 100` does not fit `u8`, so the checked addition aborts with the
overflow trap (ARITH-3, STMT-2). Whether the program is accepted is not claimed: ARITH-13 may
refuse an operation proved to overflow, and its proof criterion is open (see the header).
```
fn main() -> u8 {
    let a: u8 = 200;
    let b: u8 = 100;
    return a + b;          // traps: arithmetic overflow
}
``` -/
def overflowTrap : Program :=
  { decls, body := [.let_ (.name "a") (.scalar (.int .u8)) (lit 200),
                    .let_ (.name "b") (.scalar (.int .u8)) (lit 100)],
    ret := .add (v "a") (v "b") }

example : run overflowTrap 10 = .trap .arithmeticOverflow := by decide

/-- Refused: `300` does not fit `u8` (ARITH-2). -/
def literalTooBig : Program :=
  { decls, body := [.let_ (.name "a") (.scalar (.int .u8)) (lit 300)], ret := v "a" }

example : ¬ Checked literalTooBig := fun h => absurd ((check_iff _).mpr h) (by decide)

/-- Refused: `+` takes two operands of one integer type (TYPE-6).
```
fn main() -> u8 {
    let a: u8 = 1;
    let b: i64 = 2;
    return a + b;          // error: u8 + i64
}
``` -/
def mixedAdd : Program :=
  { decls, body := [.let_ (.name "a") (.scalar (.int .u8)) (lit 1),
                    .let_ (.name "b") (.scalar (.int .i64)) (.atom (.int .i64 2))],
    ret := .add (v "a") (v "b") }

example : ¬ Checked mixedAdd := fun h => absurd ((check_iff _).mpr h) (by decide)

/-- Refused: `x` is bound twice (SCOPE-1).
```
fn main() -> u8 {
    let x: u8 = 1;
    let x: u8 = 41;        // error: x is already bound
    return x;
}
``` -/
def rebind : Program :=
  { decls, body := [.let_ (.name "x") (.scalar (.int .u8)) (lit 1),
                    .let_ (.name "x") (.scalar (.int .u8)) (lit 41)],
    ret := v "x" }

example : ¬ Checked rebind := fun h => absurd ((check_iff _).mpr h) (by decide)

/-- Refused: a local may not reuse a struct's name; types and values share one namespace
(SCOPE-2). -/
def bindStructName : Program :=
  { decls, body := [.let_ (.name "Pair") (.scalar (.int .u8)) (lit 42)], ret := v "Pair" }

example : ¬ Checked bindStructName := fun h => absurd ((check_iff _).mpr h) (by decide)

/-- Accepted: `_` drops a Copy value (OWN-5).
```
fn main() -> u8 {
    let _: u8 = 1;
    return 42;
}
``` -/
def wildCopy : Program :=
  { decls, body := [.let_ .wild (.scalar (.int .u8)) (lit 1)], ret := lit 42 }

example : Checked wildCopy := (check_iff _).mp (by decide)
example : run wildCopy 10 = .exit 42 := by decide

/-- Refused: `_` never discards a linear value (OWN-5).
```
fn main() -> u8 {
    let _: Token = Token {};   // error: a linear value is discarded
    return 42;
}
``` -/
def wildLinear : Program :=
  { decls, body := [.let_ .wild (.struct "Token") (.mk "Token" [])], ret := lit 42 }

example : ¬ Checked wildLinear := fun h => absurd ((check_iff _).mpr h) (by decide)

/-- Refused: `main` returns `u8` (STMT-7). -/
def wrongReturnType : Program :=
  { decls, body := [], ret := .atom (.int .i64 42) }

example : ¬ Checked wrongReturnType := fun h => absurd ((check_iff _).mpr h) (by decide)

end ConcreteSpec.Examples
