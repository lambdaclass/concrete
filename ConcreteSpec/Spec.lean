/-!
# Concrete specification — Core design formalization (A2b)

**PROPOSED: not normative until Core freeze (A2c).** The owner provisionally approved the Core
design on 2026-11-05 (ROADMAP A2a; the decision rows Q1.1–Q9.19). This file is stage A2b: a
proposed `Spec.lean` that formalizes the approved rows far enough to check the four design
theorems the freeze requires (row Q9.14). It becomes normative only when the owner freezes Core
(A2c, row Q9.19), after the four theorems are checked and the outside-reader test passes. Until
then it is an opt-in Lake target (`lake build ConcreteSpec`), and its examples are not
conformance claims for any compiler. Any finding here that would change an approved row goes back
to the owner before the freeze.

The decisions are cited by their row ids (Q1.1 … Q9.19, P1 … P23) and by the recorded rule ids
of docs/spec (ODnn, TYPE-n, OWN-n, …). Recording the 2026-11-05 answers in docs/spec/decisions.md
is a separate branch (wip/record-decisions); until it lands, the row ids refer to the decision
rows the owner approved.

The file has five parts. Each part is a small Core fragment with its own judgments, defined only
as far as its theorem needs (R4 F8: "defines only what the four theorems need"). Unifying the
fragments into one Core is work for A3 (R-0560), after the freeze.

1. **Slice 1** (ROADMAP phase B): scalars, `let`, structs, the move and use-after-move refusal,
   checked `+`, `return`, with Core's binding form (Q9.1: A-normal form, fresh identities,
   explicit `copy`/`move`) and the mandatory static refusal of statically known violations
   (Q6.1–Q6.6). Theorems: safety (`checked_never_stuck`), behaviour (`checked_exits_or_traps`),
   justified refusal (`knownViolation_traps`), checker correspondence (`check_iff`).
2. **Representation** (Q3.11): `rep_roundtrip`.
3. **Structured cleanup** (Q9.8, Q2.1, Q2.7, Q1.5): `defer_cleanup_releases`.
4. **Scoped borrows** (Q3.3, Q3.4, Q9.5, REF-11): `open_mut_freezes_root`, with its shared
   half `shared_open_grants_no_write`.
5. **Allocator brands** (Q1.1–Q1.5, Q1.13): `reset_fresh`.

Only Lean core is imported (Q9.16). No proof uses `native_decide` or `sorry` (Q9.17).

## Slice 1

What slice 1 contains:

* the scalar types: the eight integer types (TYPE-19) and `bool`;
* `let` with a type, a struct literal, and a struct-destructuring `let` (PAT-5);
* struct declarations, `Copy` or not (TYPE-14), with scalar fields; the slice's central case is
  an empty struct without `Copy`, moved and then consumed by a destructuring `let`;
* the use-after-move refusal (OWN-1), consumption before exit (OWN-2) and used bindings (OWN-5);
* one checked operation, integer `+` (ARITH-3), which traps on overflow;
* the static refusal of an operation whose operands are statically known and whose result
  violates its contract (Q6.1, Q6.2, Q6.4; ARITH-13, OD118);
* one function, `main`, with no parameters, whose body is a list of `let` statements followed by
  `return a` with `a : u8` (STMT-7, STMT-9).

Core's binding form (Q9.1):

* **A-normal form.** Every operand is an atom: a literal, `copy x` or `move x`. `x + y` is an
  expression whose operands are atoms; a nested surface expression becomes a sequence of `let`s
  of temporaries, and `return` takes an atom.
* **Fresh identities.** A local is a `Var`, an identity (a source-name hint and a number) that is
  bound at most once. Surface names, shadowing and the namespace rules (SCOPE-1, SCOPE-2) are
  elaboration rules that map names to identities; Core only requires each identity to be fresh.
* **Explicit moves and copies.** Reading a local is written `copy x` (its type is Copy) or
  `move x` (its type is linear). There is no read whose effect depends on the type.

What slice 1 is not:

* It is not a parser or an elaborator. Programs are given as Core abstract syntax. How surface
  literals receive a type (ARITH-2, TYPE-4), how an unannotated `let` gets its type, how names
  become identities and how nested expressions become A-normal form are elaboration rules for a
  later file, which must be proved to produce this syntax.
* Nothing outside slice 1 is modelled in this part: no calls, no branches, no loops, no
  references, no heap, no capabilities, no `defer`. Struct fields are scalars only. Parts 3–5
  model the pieces of those that the design theorems need.

The judgments (`HasType`, `UseAtom`, `Uses`, `StmtOk`, `BodyOk`, `Checked`) are inductive
relations, independent of the executable checker (`check`); `check_iff` proves the two agree. The
semantics is a total, fuel-indexed evaluator (`run`) whose outcome separates exit, trap, stuck and
running out of fuel (STMT-1). `Exec` is the fuel-free form of the same statement-level evaluator:
adequacy and determinism are about fuel, not an independent cross-check of expression evaluation.

Statically known values (Q6.2). A value is known when it comes from the fixed table: a literal,
an immutable `let` of a known value, or a checked operation on known operands. In slice 1 every
`let` is immutable. A struct literal is not in the table, so a field bound by destructuring a
struct is not known; `fieldOverflowTrap` below is the resulting accepted program that traps. The
table has more entries (module constants, the six named wrapping/saturating operations, lossless
`as`) that slice 1 has no syntax for.

Obligations not discharged here:

* the elaboration relation from surface syntax (see above);
* the lexer, model-translation and native-image obligations;
* erasure of the instrumentation (`Slot.moved`, the `used`, `live` and `known` fields of `Entry`):
  stated as intent, with no erasure definition or theorem yet (Q9.10);
* the observable contract of a trap: its source location and message, and the mapping of exit
  values and traps to the process exit code (row Q7.10: trap exit 134); `Trap` here is an
  abstract outcome with neither;
* the target observations (the launcher contract): every result proved here is about the model's
  abstract outcomes, and a claim about an executable needs a transport to the approved launcher
  contract first;
* parts 3–5 model only the structure their theorems are about. Part 3 abstracts each deferred call
  to its identity, dependencies, loans and whether it traps, and does not model the statements
  between registrations or the evaluation of a `return` value before cleanup. Part 4 does not
  model the range clause and `else` of Q4.7, element access, or the no-escape rules. Part 5 does
  not prove that a checked program passes every ghost brand check (the erasure correspondence of
  Q1.1).
-/

namespace ConcreteSpec

/-- A declaration name: a struct or a field. Declaration names are nominal (Q9.2). -/
abbrev Name := String

/-- A local variable's identity in Core (Q9.1, fresh identities). `hint` is the source name, kept
for reading; the identity is the pair, and each identity is bound at most once (`Fresh`). -/
structure Var where
  hint : String
  id : Nat
  deriving DecidableEq, Repr

/-! ### Lists keyed by name -/

/-- The first value bound to `x` in a list of keyed pairs. -/
def lookupF {κ α : Type} [DecidableEq κ] (x : κ) : List (κ × α) → Option α
  | [] => none
  | (y, a) :: rest => if y = x then some a else lookupF x rest

theorem lookupF_mem {κ α : Type} [DecidableEq κ] {x : κ} {a : α} :
    ∀ {l : List (κ × α)}, lookupF x l = some a → (x, a) ∈ l
  | [], h => by simp [lookupF] at h
  | (y, b) :: rest, h => by
    unfold lookupF at h
    split at h
    · cases h; subst_vars; exact List.Mem.head _
    · exact List.Mem.tail _ (lookupF_mem h)

theorem lookupF_of_mem {κ α : Type} [DecidableEq κ] {x : κ} {a : α} :
    ∀ {l : List (κ × α)}, (x, a) ∈ l → ∃ b, lookupF x l = some b
  | [], h => by cases h
  | (y, b) :: rest, h => by
    unfold lookupF
    split
    · exact ⟨b, rfl⟩
    · cases h with
      | head => contradiction
      | tail _ h => exact lookupF_of_mem h

/-! ### Types -/

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

/-- A scalar value. -/
inductive Scalar where
  | int (t : IntTy) (n : Int)
  | bool (b : Bool)
  deriving DecidableEq, Repr

/-! ### Syntax -/

/-- An atom (Q9.1): a typed integer literal, a `bool` literal, `copy x` (read a Copy local and
keep it) or `move x` (move a linear local out). -/
inductive Atom where
  | int (t : IntTy) (n : Int)
  | bool (b : Bool)
  | copy (x : Var)
  | move (x : Var)
  deriving DecidableEq, Repr

/-- An expression in A-normal form (Q9.1): an atom, checked integer addition `l + r` of two atoms
(ARITH-3), or a struct literal `S { f: a, ... }` whose fields are atoms (TYPE-7). -/
inductive Expr where
  | atom (a : Atom)
  | add (l r : Atom)
  | mk (s : Name) (fields : List (Name × Atom))
  deriving DecidableEq, Repr

/-- A destructuring pattern binds a field to a local, or ignores it with `_` (PAT-1, PAT-5). -/
inductive Binder where
  | name (x : Var)
  | wild
  deriving DecidableEq, Repr

/-- A statement: `let x: T = e;`, `let _: T = e;`, or a destructuring `let S { f: b, ... } = e;`
(PAT-5). -/
inductive Stmt where
  | let_ (b : Binder) (t : Ty) (e : Expr)
  | letStruct (s : Name) (pat : List (Name × Binder)) (e : Expr)
  deriving DecidableEq, Repr

/-- A slice-1 program: struct declarations and `fn main() -> u8 { body; return ret; }` (STMT-7:
`main` returns `u8` and may declare no grants; STMT-9: the body ends in `return`). In A-normal form
`return` takes an atom. -/
structure Program where
  decls : Decls
  body : List Stmt
  ret : Atom
  deriving Repr

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

/-! ### Ownership and known values -/

/-- The checker's state for one binding: its type, whether it still holds its value (`live`),
whether it has been read (`used`), and its statically known value, if it has one (`known`, Q6.2). -/
structure Entry where
  name : Var
  ty : Ty
  live : Bool
  used : Bool
  known : Option Scalar
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
known value. `move x` is never known: only linear values are moved, and no linear value is in the
table. -/
def knownAtom (Γ : Ctx) : Atom → Option Scalar
  | .int t n => some (.int t n)
  | .bool b => some (.bool b)
  | .copy x => (ctxFind Γ x).bind Entry.known
  | .move _ => none

/-- The statically known value of an expression (Q6.2): a known atom, or a checked `+` of two
known operands whose result fits. A struct literal is not in the table. -/
def knownExpr (Γ : Ctx) : Expr → Option Scalar
  | .atom a => knownAtom Γ a
  | .add l r => match knownAtom Γ l, knownAtom Γ r with
    | some (.int t a), some (.int t' b) =>
      if t = t' ∧ t.inRange (a + b) = true then some (.int t (a + b)) else none
    | _, _ => none
  | .mk _ _ => none

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
of the field's type, in pattern order. A field of a struct value is not a known value (Q6.2). -/
def bindPat (d : StructDecl) : List (Name × Binder) → Ctx → Ctx
  | [], Γ => Γ
  | (_, .wild) :: rest, Γ => bindPat d rest Γ
  | (f, .name x) :: rest, Γ =>
    match lookupF f d.fields with
    | some st => bindPat d rest
        ({ name := x, ty := .scalar st, live := true, used := false, known := none } :: Γ)
    | none => bindPat d rest Γ

/-- Every binder of a pattern is fresh, and no two binders share an identity. -/
def PatFresh (Γ : Ctx) (pat : List (Name × Binder)) : Prop :=
  (patNames pat).Nodup ∧ ∀ x ∈ patNames pat, Fresh Γ x

/-- A statement is well typed, respects ownership and contains no statically known violation.

* `let x: T = e;` — `e` has type `T` (TYPE-4), contains no known violation (Q6.1), its reads are
  allowed (OWN-1), and `x` is fresh (Q9.1). `x` starts live and unused, with `e`'s known value.
* `let _: T = e;` — as above, but nothing is bound and the value is dropped, so `T` must be Copy:
  `_` never discards a linear value (OWN-5). Slice 1 has no `Result` or `Option`, the other
  values `_` refuses.
* `let S { f: b, ... } = e;` — `e` has type `S`, the pattern names every field (PAT-5), the
  scrutinee is consumed (PAT-5: a linear `e` is moved), and the binders are fresh. -/
inductive StmtOk (Δ : Decls) : Ctx → Stmt → Ctx → Prop where
  | let_ {Γ Γ₁ x t e} : HasType Δ Γ.tys e t → knownViolates Γ e = false → Uses Δ Γ e Γ₁ →
      Fresh Γ₁ x →
      StmtOk Δ Γ (.let_ (.name x) t e)
        ({ name := x, ty := t, live := true, used := false, known := knownExpr Γ e } :: Γ₁)
  | letWild {Γ Γ₁ t e} : HasType Δ Γ.tys e t → knownViolates Γ e = false → t.isCopy Δ = true →
      Uses Δ Γ e Γ₁ → StmtOk Δ Γ (.let_ .wild t e) Γ₁
  | letStruct {Γ Γ₁ s d pat e} : HasType Δ Γ.tys e (.struct s) → knownViolates Γ e = false →
      findDecl Δ s = some d → PatOk d pat → Uses Δ Γ e Γ₁ → PatFresh Γ₁ pat →
      StmtOk Δ Γ (.letStruct s pat e) (bindPat d pat Γ₁)

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

/-! ### Semantics -/

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
abbrev Env := List (Var × Slot)

/-- The defined traps of slice 1 (ARITH-3). -/
inductive Trap where
  | arithmeticOverflow
  deriving DecidableEq, Repr

/-- Why execution got stuck. Safe code never gets stuck (STMT-1); `checked_never_stuck` proves it
for slice 1. `wrongMode` is a `copy` of a linear value or a `move` of a Copy one. -/
inductive Stuck where
  | movedRead
  | unbound
  | wrongMode
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
def envFind : Env → Var → Option Slot
  | [], _ => none
  | (y, s) :: env, x => if y = x then some s else envFind env x

/-- Mark `x` moved. -/
def envMove : Env → Var → Env
  | [], _ => []
  | (y, s) :: env, x => if y = x then (y, .moved) :: env else (y, s) :: envMove env x

/-- Whether a value is Copy (OWN-D1, TYPE-14). -/
def Val.isCopy (Δ : Decls) : Val → Bool
  | .scalar _ => true
  | .struct s _ => match findDecl Δ s with
    | some d => d.copy
    | none => false

/-- Read a local: `copy` (`mv = false`) keeps a Copy value, `move` (`mv = true`) moves a linear
value out (Q9.1). A moved slot is stuck, and so is a read in the wrong mode. -/
def readVar (Δ : Decls) (env : Env) (x : Var) (mv : Bool) : R (Val × Env) :=
  match envFind env x with
  | none => .stuck .unbound
  | some .moved => .stuck .movedRead
  | some (.val v) =>
    if v.isCopy Δ = !mv then .ok (v, if mv then envMove env x else env) else .stuck .wrongMode

/-- Evaluate an atom that must be a scalar. -/
def evalAtom (Δ : Decls) (env : Env) : Atom → R (Scalar × Env)
  | .int t n => .ok (.int t n, env)
  | .bool b => .ok (.bool b, env)
  | .copy x => match readVar Δ env x false with
    | .ok (.scalar v, env') => .ok (v, env')
    | .ok _ => .stuck .typeError
    | .trap k => .trap k
    | .stuck w => .stuck w
  | .move x => match readVar Δ env x true with
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

/-- Evaluate an expression, left operand first (STMT-4). -/
def evalExpr (Δ : Decls) : Env → Expr → R (Val × Env)
  | env, .atom (.int t n) => .ok (.scalar (.int t n), env)
  | env, .atom (.bool b) => .ok (.scalar (.bool b), env)
  | env, .atom (.copy x) => readVar Δ env x false
  | env, .atom (.move x) => readVar Δ env x true
  | env, .add l r => match evalAtom Δ env l with
    | .ok (.int t a, env₁) => match evalAtom Δ env₁ r with
      | .ok (.int t' b, env₂) =>
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

/-- The outcome of `return a`: the `u8` value is the exit status (STMT-7). -/
def retOutcome (Δ : Decls) (env : Env) (r : Atom) : Outcome :=
  match evalAtom Δ env r with
  | .ok (.int .u8 k, _) => .exit k.toNat
  | .ok _ => .stuck .typeError
  | .trap k => .trap k
  | .stuck w => .stuck w

/-- The fuel-indexed evaluator: each statement and the final `return` costs one unit of fuel. -/
def runBody (Δ : Decls) : Nat → Env → List Stmt → Atom → Outcome
  | 0, _, _, _ => .outOfFuel
  | _ + 1, env, [], r => retOutcome Δ env r
  | n + 1, env, s :: ss, r => match execStmt Δ env s with
    | .ok env' => runBody Δ n env' ss r
    | .trap k => .trap k
    | .stuck w => .stuck w

/-- Run a program with the given fuel, from an empty environment. -/
def run (p : Program) (fuel : Nat) : Outcome := runBody p.decls fuel [] p.body p.ret

/-- The fuel-free semantics: the outcome a body reaches. -/
inductive Exec (Δ : Decls) : Env → List Stmt → Atom → Outcome → Prop where
  | ret {env r} : Exec Δ env [] r (retOutcome Δ env r)
  | step {env env' s ss r o} : execStmt Δ env s = .ok env' → Exec Δ env' ss r o →
      Exec Δ env (s :: ss) r o
  | trap {env s ss r k} : execStmt Δ env s = .trap k → Exec Δ env (s :: ss) r (.trap k)
  | stuck {env s ss r w} : execStmt Δ env s = .stuck w → Exec Δ env (s :: ss) r (.stuck w)

/-! ### Properties of the semantics -/

/-- Fuel monotonicity: a run that finishes with some fuel finishes the same way with more. -/
theorem runBody_mono {Δ : Decls} {r : Atom} :
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
theorem retOutcome_ne_outOfFuel (Δ : Decls) (env : Env) (r : Atom) :
    retOutcome Δ env r ≠ .outOfFuel := by
  unfold retOutcome; split <;> simp

/-- A body of `k` statements finishes with `k + 1` units of fuel. -/
theorem runBody_enough {Δ : Decls} {r : Atom} :
    ∀ (ss : List Stmt) (env : Env), runBody Δ (ss.length + 1) env ss r ≠ .outOfFuel
  | [], env => by simpa [runBody] using retOutcome_ne_outOfFuel Δ env r
  | s :: ss, env => by
    simp only [List.length_cons, runBody]
    split
    · exact runBody_enough ss _
    · simp
    · simp

/-- The evaluator is sound for `Exec`: an outcome reached with some fuel is an `Exec` outcome. -/
theorem runBody_sound {Δ : Decls} {r : Atom} :
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
theorem exec_complete {Δ : Decls} {r : Atom} {env : Env} {ss : List Stmt} {o : Outcome}
    (h : Exec Δ env ss r o) : runBody Δ (ss.length + 1) env ss r = o := by
  induction h with
  | ret => simp [runBody]
  | step he _ ih => simp only [List.length_cons, runBody, he]; exact ih
  | trap he => simp [runBody, he]
  | stuck he => simp [runBody, he]

/-- `Exec` never ends in `outOfFuel`. -/
theorem exec_ne_outOfFuel {Δ : Decls} {r : Atom} {env : Env} {ss : List Stmt} {o : Outcome}
    (h : Exec Δ env ss r o) : o ≠ .outOfFuel := by
  induction h with
  | ret => exact retOutcome_ne_outOfFuel _ _ _
  | step _ _ ih => exact ih
  | trap => simp
  | stuck => simp

/-- Determinism: a body has at most one outcome. -/
theorem exec_deterministic {Δ : Decls} {r : Atom} {env : Env} {ss : List Stmt} {o₁ o₂ : Outcome}
    (h₁ : Exec Δ env ss r o₁) (h₂ : Exec Δ env ss r o₂) : o₁ = o₂ := by
  rw [← exec_complete h₁, ← exec_complete h₂]

/-- Totality: every body has exactly one outcome (slice 1 has no loops or calls). -/
theorem exec_total (Δ : Decls) (env : Env) (ss : List Stmt) (r : Atom) :
    ∃ o, Exec Δ env ss r o :=
  ⟨_, runBody_sound rfl (runBody_enough ss env)⟩

/-! ### Safety: accepted programs never get stuck -/

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

/-- The context describes the environment: the same identities in the same order, every live
binding holds a value of its type, and a binding with a known value holds exactly that scalar and
has a Copy type. A binding the context records as moved is otherwise unconstrained here; the
evaluator marks such a slot `moved` when it moves the value out. -/
inductive Corr (Δ : Decls) : Ctx → Env → Prop where
  | nil : Corr Δ [] []
  | cons {e sl Γ env} : (e.live = true → ∃ v, sl = .val v ∧ VTy Δ v e.ty) →
      (∀ s, e.known = some s → sl = .val (.scalar s) ∧ e.ty.isCopy Δ = true) →
      Corr Δ Γ env → Corr Δ (e :: Γ) ((e.name, sl) :: env)

theorem tys_lookup (x : Var) : ∀ Γ : Ctx, lookupF x Γ.tys = (ctxFind Γ x).map Entry.ty
  | [] => rfl
  | e :: Γ => by
    simp only [Ctx.tys, List.map_cons, lookupF, ctxFind] at *
    split
    · rfl
    · exact tys_lookup x Γ

theorem ctxMark_tys (x : Var) (mv : Bool) : ∀ Γ : Ctx, (ctxMark Γ x mv).tys = Γ.tys
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
  | copy => exact ctxMark_tys _ _ _
  | move => exact ctxMark_tys _ _ _

theorem useAtoms_tys {Δ : Decls} {Γ Γ' : Ctx} {as : List Atom} (h : UseAtoms Δ Γ as Γ') :
    Γ'.tys = Γ.tys := by
  induction h with
  | nil => rfl
  | cons h₁ _ ih => rw [ih, useAtom_tys h₁]

theorem uses_tys {Δ : Decls} {Γ Γ' : Ctx} {e : Expr} (h : Uses Δ Γ e Γ') : Γ'.tys = Γ.tys := by
  cases h with
  | atom h => exact useAtom_tys h
  | add h₁ h₂ => rw [useAtom_tys h₂, useAtom_tys h₁]
  | mk h => exact useAtoms_tys h

theorem vty_isCopy {Δ : Decls} {v : Val} {t : Ty} (h : VTy Δ v t) : v.isCopy Δ = t.isCopy Δ := by
  cases h <;> rfl

theorem corr_find {Δ : Decls} {x : Var} {Γ : Ctx} {env : Env} (hc : Corr Δ Γ env) :
    ∀ {e : Entry}, ctxFind Γ x = some e →
      (e.live = true → ∃ v, envFind env x = some (.val v) ∧ VTy Δ v e.ty) ∧
      (∀ s, e.known = some s → envFind env x = some (.val (.scalar s)) ∧ e.ty.isCopy Δ = true) := by
  induction hc with
  | nil => intro e h; simp [ctxFind] at h
  | cons hsl hk _ ih =>
    rename_i e' sl Γ env
    intro e h
    simp only [ctxFind, envFind] at h ⊢
    split at h
    · cases h
      rename_i heq
      refine ⟨fun hl => ?_, fun s hs => ?_⟩
      · obtain ⟨v, rfl, hv⟩ := hsl hl
        exact ⟨v, by simp [heq], hv⟩
      · obtain ⟨rfl, hcp⟩ := hk s hs
        exact ⟨by simp [heq], hcp⟩
    · rename_i hne
      simp only [hne, ↓reduceIte]
      exact ih h

theorem corr_mark_copy {Δ : Decls} {x : Var} {Γ : Ctx} {env : Env} (hc : Corr Δ Γ env) :
    Corr Δ (ctxMark Γ x false) env := by
  induction hc with
  | nil => exact .nil
  | cons hsl hk hc ih =>
    simp only [ctxMark]
    split
    · exact .cons (by simpa using hsl) hk hc
    · exact .cons hsl hk ih

/-- Moving `x` out keeps the description, provided `x`'s binding has no known value (a known
value is a Copy scalar, which is never moved). -/
theorem corr_mark_move {Δ : Decls} {x : Var} {Γ : Ctx} {env : Env} (hc : Corr Δ Γ env) :
    (∀ e, ctxFind Γ x = some e → e.known = none) →
    Corr Δ (ctxMark Γ x true) (envMove env x) := by
  induction hc with
  | nil => intro _; exact .nil
  | cons hsl hk hc ih =>
    rename_i e sl Γ env
    intro hn
    simp only [ctxMark, envMove]
    split
    · rename_i heq
      have hkn : e.known = none := hn e (by simp [ctxFind, heq])
      exact .cons (by simp) (by intro s hs; simp [hkn] at hs) hc
    · rename_i hne
      exact .cons hsl hk (ih (fun e' h => hn e' (by simp [ctxFind, hne, h])))

/-- Reading a live local in the mode its type requires succeeds with a value of its type, and the
context's record of the read matches what the evaluator did. -/
theorem readVar_safe {Δ : Decls} {Γ : Ctx} {env : Env} {x : Var} {e : Entry} {mv : Bool}
    (hc : Corr Δ Γ env) (hf : ctxFind Γ x = some e) (hl : e.live = true)
    (hm : e.ty.isCopy Δ = !mv) :
    ∃ v env', readVar Δ env x mv = .ok (v, env') ∧ VTy Δ v e.ty ∧
      Corr Δ (ctxMark Γ x mv) env' := by
  obtain ⟨hlive, hknown⟩ := corr_find hc hf
  obtain ⟨v, hv, hty⟩ := hlive hl
  have hvc : v.isCopy Δ = !mv := by rw [vty_isCopy hty]; exact hm
  refine ⟨v, _, by simp [readVar, hv, hvc]; rfl, hty, ?_⟩
  cases mv
  · simpa using corr_mark_copy hc
  · simp only [↓reduceIte]
    refine corr_mark_move hc ?_
    intro e' he'
    rw [hf] at he'
    cases he'
    cases hk : e.known with
    | none => rfl
    | some s => have := (hknown s hk).2; rw [hm] at this; cases this

theorem evalAtom_safe {Δ : Decls} {Γ Γ' : Ctx} {env : Env} {a : Atom} {st : STy}
    (hc : Corr Δ Γ env) (ht : AtomTy Γ.tys a st) (hu : UseAtom Δ Γ a Γ') :
    ∃ v env', evalAtom Δ env a = .ok (v, env') ∧ SVTy v st ∧ Corr Δ Γ' env' := by
  cases hu with
  | int => cases ht with | int h => exact ⟨_, _, rfl, .int h, hc⟩
  | bool => cases ht with | bool => exact ⟨_, _, rfl, .bool, hc⟩
  | copy hf hl hcp =>
    cases ht with
    | copy hx =>
      rw [tys_lookup, hf] at hx
      simp only [Option.map_some, Option.some.injEq] at hx
      obtain ⟨v, env', hr, hv, hc'⟩ := readVar_safe (mv := false) hc hf hl (by simpa using hcp)
      rw [hx] at hv
      cases hv with
      | scalar hs => exact ⟨_, env', by simp [evalAtom, hr], hs, hc'⟩
  | move hf hl hcp =>
    cases ht with
    | move hx =>
      rw [tys_lookup, hf] at hx
      simp only [Option.map_some, Option.some.injEq] at hx
      obtain ⟨v, env', hr, hv, hc'⟩ := readVar_safe (mv := true) hc hf hl (by simpa using hcp)
      rw [hx] at hv
      cases hv with
      | scalar hs => exact ⟨_, env', by simp [evalAtom, hr], hs, hc'⟩

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
theorem evalExpr_safe {Δ : Decls} {e : Expr} {Γ Γ' : Ctx} {env : Env} {t : Ty}
    (hc : Corr Δ Γ env) (ht : HasType Δ Γ.tys e t) (hu : Uses Δ Γ e Γ') :
    (∃ k, evalExpr Δ env e = .trap k) ∨
    ∃ v env', evalExpr Δ env e = .ok (v, env') ∧ VTy Δ v t ∧ Corr Δ Γ' env' := by
  cases ht with
  | int h => cases hu with | atom hu => cases hu; exact .inr ⟨_, _, rfl, .scalar (.int h), hc⟩
  | bool => cases hu with | atom hu => cases hu; exact .inr ⟨_, _, rfl, .scalar .bool, hc⟩
  | copy hx =>
    cases hu with
    | atom hu =>
      cases hu with
      | copy hf hl hcp =>
        rw [tys_lookup, hf] at hx
        simp only [Option.map_some, Option.some.injEq] at hx
        subst hx
        obtain ⟨v, env', hr, hv, hc'⟩ := readVar_safe (mv := false) hc hf hl (by simpa using hcp)
        exact .inr ⟨v, env', by simpa [evalExpr] using hr, hv, hc'⟩
  | move hx =>
    cases hu with
    | atom hu =>
      cases hu with
      | move hf hl hcp =>
        rw [tys_lookup, hf] at hx
        simp only [Option.map_some, Option.some.injEq] at hx
        subst hx
        obtain ⟨v, env', hr, hv, hc'⟩ := readVar_safe (mv := true) hc hf hl (by simpa using hcp)
        exact .inr ⟨v, env', by simpa [evalExpr] using hr, hv, hc'⟩
  | add hl hr =>
    rename_i l r t
    cases hu with
    | add hul hur =>
      rename_i Γ₁
      obtain ⟨v₁, env₁, he₁, hs₁, hc₁⟩ := evalAtom_safe hc hl hul
      rw [← useAtom_tys hul] at hr
      obtain ⟨v₂, env₂, he₂, hs₂, hc₂⟩ := evalAtom_safe hc₁ hr hur
      cases hs₁ with
      | int hin₁ =>
        rename_i a
        cases hs₂ with
        | int hin₂ =>
          rename_i b
          by_cases hsum : t.inRange (a + b) = true
          · exact .inr ⟨_, env₂, by simp [evalExpr, he₁, he₂, checkedAdd, hsum],
              .scalar (.int hsum), hc₂⟩
          · exact .inl ⟨.arithmeticOverflow, by simp [evalExpr, he₁, he₂, checkedAdd, hsum]⟩
  | mk hd hf =>
    rename_i s d fs
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

/-- A known atom evaluates to its known value without changing the environment (Q6.2). -/
theorem knownAtom_sound {Δ : Decls} {Γ : Ctx} {env : Env} {a : Atom} {s : Scalar}
    (hc : Corr Δ Γ env) (hk : knownAtom Γ a = some s) : evalAtom Δ env a = .ok (s, env) := by
  cases a with
  | int t n => simp [knownAtom] at hk; subst hk; rfl
  | bool b => simp [knownAtom] at hk; subst hk; rfl
  | move x => simp [knownAtom] at hk
  | copy x =>
    simp only [knownAtom, Option.bind_eq_some_iff] at hk
    obtain ⟨e, hf, hke⟩ := hk
    obtain ⟨hv, _⟩ := (corr_find hc hf).2 s hke
    simp [evalAtom, readVar, hv, Val.isCopy]

/-- A known expression evaluates to its known value (Q6.2): the static table agrees with the
evaluator. -/
theorem knownExpr_sound {Δ : Decls} {Γ : Ctx} {env : Env} {e : Expr} {s : Scalar}
    (hc : Corr Δ Γ env) (hk : knownExpr Γ e = some s) : evalExpr Δ env e = .ok (.scalar s, env) := by
  cases e with
  | atom a =>
    have hk' : knownAtom Γ a = some s := hk
    have h := knownAtom_sound hc hk'
    cases a with
    | int t n => simp [knownAtom] at hk'; subst hk'; rfl
    | bool b => simp [knownAtom] at hk'; subst hk'; rfl
    | move x => simp [knownAtom] at hk'
    | copy x =>
      simp only [evalAtom] at h
      simp only [evalExpr]
      split at h <;> simp_all
  | add l r =>
    simp only [knownExpr] at hk
    split at hk
    · rename_i t a t' b hl hr
      split at hk
      · rename_i hcond
        cases hk
        obtain ⟨rfl, hin⟩ := hcond
        simp [evalExpr, knownAtom_sound hc hl, knownAtom_sound hc hr, checkedAdd, hin]
      · cases hk
    · cases hk
  | mk s fs => simp [knownExpr] at hk

/-- **Justified refusal** (Q6.1, OD118). An operation refused as a known violation would trap if
it ran: in any environment the checker's context describes, evaluating it ends in the overflow
trap. The checker refuses only operations that are proven to fail, never a correct one. Every
environment reached by running an accepted prefix is described by its context (`execStmt_safe`
below), so this covers every statement position. -/
theorem knownViolation_traps {Δ : Decls} {Γ : Ctx} {env : Env} {e : Expr}
    (hc : Corr Δ Γ env) (hv : knownViolates Γ e = true) :
    evalExpr Δ env e = .trap .arithmeticOverflow := by
  cases e with
  | atom a => simp [knownViolates] at hv
  | mk s fs => simp [knownViolates] at hv
  | add l r =>
    simp only [knownViolates] at hv
    split at hv
    · rename_i t a t' b hl hr
      simp only [Bool.and_eq_true, decide_eq_true_eq, Bool.not_eq_true'] at hv
      obtain ⟨rfl, hout⟩ := hv
      simp [evalExpr, knownAtom_sound hc hl, knownAtom_sound hc hr, checkedAdd, hout]
    · cases hv

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
      (Corr.cons (e := { name := x, ty := .scalar st, live := true, used := false, known := none })
        (fun _ => ⟨_, rfl, .scalar hsv⟩) (by intro s hs; cases hs) hc)
      (fun p h => hp p (List.Mem.tail _ h))

/-- Statement safety: a checked statement executes or traps; it never gets stuck. -/
theorem execStmt_safe {Δ : Decls} {Γ Γ' : Ctx} {env : Env} {s : Stmt}
    (hc : Corr Δ Γ env) (hs : StmtOk Δ Γ s Γ') :
    (∃ k, execStmt Δ env s = .trap k) ∨ ∃ env', execStmt Δ env s = .ok env' ∧ Corr Δ Γ' env' := by
  cases hs with
  | let_ ht _ hu _ =>
    rename_i x t e
    rcases evalExpr_safe hc ht hu with ⟨k, hk⟩ | ⟨v, env₁, he, hv, hc₁⟩
    · exact .inl ⟨k, by simp [execStmt, hk]⟩
    · refine .inr ⟨_, by simp [execStmt, he], .cons (fun _ => ⟨v, rfl, hv⟩) ?_ hc₁⟩
      intro s hs
      have hk := knownExpr_sound hc hs
      rw [he] at hk
      cases hk
      cases hv with | scalar _ => exact ⟨rfl, rfl⟩
  | letWild ht _ _ hu =>
    rcases evalExpr_safe hc ht hu with ⟨k, hk⟩ | ⟨v, env₁, he, _, hc₁⟩
    · exact .inl ⟨k, by simp [execStmt, hk]⟩
    · exact .inr ⟨_, by simp [execStmt, he], hc₁⟩
  | letStruct ht _ hd hp hu _ =>
    rcases evalExpr_safe hc ht hu with ⟨k, hk⟩ | ⟨v, env₁, he, hv, hc₁⟩
    · exact .inl ⟨k, by simp [execStmt, hk]⟩
    · cases hv with
      | struct hd' hvs =>
        rw [hd] at hd'
        cases hd'
        obtain ⟨env', hde, hc'⟩ := destructure_safe hvs hc₁ hp.declared
        exact .inr ⟨env', by simp [execStmt, he, hde], hc'⟩

/-- An atom with a scalar type in `HasType` has that type in `AtomTy`. -/
theorem hasType_atom_scalar {Δ : Decls} {Γ : TyCtx} {a : Atom} {st : STy}
    (h : HasType Δ Γ (.atom a) (.scalar st)) : AtomTy Γ a st := by
  cases h with
  | int h => exact .int h
  | bool => exact .bool
  | copy h => exact .copy h
  | move h => exact .move h

/-- Body safety: from a described environment, a checked body never gets stuck, for any fuel. -/
theorem runBody_safe {Δ : Decls} {Γ : Ctx} {ss : List Stmt} {r : Atom} (hb : BodyOk Δ Γ ss r) :
    ∀ {env : Env}, Corr Δ Γ env → ∀ n w, runBody Δ n env ss r ≠ .stuck w := by
  induction hb with
  | ret ht hu _ =>
    intro env hc n w
    cases n with
    | zero => simp [runBody]
    | succ n =>
      simp only [runBody, retOutcome]
      cases hu with
      | atom hu =>
        obtain ⟨v, env', he, hv, _⟩ := evalAtom_safe hc (hasType_atom_scalar ht) hu
        cases hv with | int _ => simp [he]
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
reads a moved variable, never reads an unbound one, never reads in the wrong mode and never meets
a value of the wrong type (STMT-1: safe code never reaches a stuck state; OWN-1). -/
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

/-! ### The checker and its correspondence with the judgments

`check` is an executable checker. It is not the specification: the judgments above are.
`check_iff` proves the two agree, which is also what lets the conformance examples below be
decided by evaluation. -/

instance (Γ : Ctx) (x : Var) : Decidable (Fresh Γ x) := by
  unfold Fresh; infer_instance

instance (Γ : Ctx) (pat : List (Name × Binder)) : Decidable (PatFresh Γ pat) := by
  unfold PatFresh; infer_instance

instance (Δ : Decls) (Γ : Ctx) : Decidable (Discharged Δ Γ) := by
  unfold Discharged; infer_instance

instance (Δ : Decls) : Decidable (DeclsOk Δ) := by
  unfold DeclsOk; infer_instance

/-- The scalar type of an atom, if it has one. -/
def atomTy (Γ : TyCtx) : Atom → Option STy
  | .int t n => if t.inRange n then some (.int t) else none
  | .bool _ => some .bool
  | .copy x => match lookupF x Γ with
    | some (.scalar st) => some st
    | _ => none
  | .move x => match lookupF x Γ with
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
    | copy x =>
      simp only [atomTy] at h
      split at h
      · cases h; exact .copy (by assumption)
      · cases h
    | move x =>
      simp only [atomTy] at h
      split at h
      · cases h; exact .move (by assumption)
      · cases h
  · intro h
    cases h with
    | int h => simp [atomTy, h]
    | bool => rfl
    | copy h => simp [atomTy, h]
    | move h => simp [atomTy, h]

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
  | .atom (.copy x) => lookupF x Γ
  | .atom (.move x) => lookupF x Γ
  | .add l r => match atomTy Γ l, atomTy Γ r with
    | some (.int t), some (.int t') => if t = t' then some (.scalar (.int t)) else none
    | _, _ => none
  | .mk s fs => match findDecl Δ s with
    | some d => if fieldsOkB Γ d fs then some (.struct s) else none
    | none => none

theorem tyOf_iff {Δ : Decls} {Γ : TyCtx} {e : Expr} {ty : Ty} :
    tyOf Δ Γ e = some ty ↔ HasType Δ Γ e ty := by
  cases e with
  | atom a =>
    cases a with
    | int t n =>
      simp only [tyOf]
      constructor
      · intro h; split at h
        · cases h; exact .int (by assumption)
        · cases h
      · intro h; cases h with | int h => simp [h]
    | bool b =>
      simp only [tyOf, Option.some.injEq]
      constructor
      · intro h; subst h; exact .bool
      · intro h; cases h; rfl
    | copy x =>
      simp only [tyOf]
      constructor
      · exact .copy
      · intro h; cases h with | copy h => exact h
    | move x =>
      simp only [tyOf]
      constructor
      · exact .move
      · intro h; cases h with | move h => exact h
  | add l r =>
    constructor
    · intro h
      simp only [tyOf] at h
      split at h
      · rename_i t t' hl hr
        split at h
        · cases h; subst_vars
          exact .add (atomTy_iff.mp hl) (atomTy_iff.mp hr)
        · cases h
      · cases h
    · intro h
      cases h with
      | add hl hr => simp [tyOf, atomTy_iff.mpr hl, atomTy_iff.mpr hr]
  | mk s fs =>
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
  | .copy x => match ctxFind Γ x with
    | some e => if e.live ∧ e.ty.isCopy Δ = true then some (ctxMark Γ x false) else none
    | none => none
  | .move x => match ctxFind Γ x with
    | some e => if e.live ∧ e.ty.isCopy Δ = false then some (ctxMark Γ x true) else none
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
  | .add l r => match useAtom Δ Γ l with
    | some Γ₁ => useAtom Δ Γ₁ r
    | none => none
  | .mk _ fs => useAtoms Δ Γ (fs.map Prod.snd)

theorem useAtom_iff {Δ : Decls} {Γ Γ' : Ctx} {a : Atom} :
    useAtom Δ Γ a = some Γ' ↔ UseAtom Δ Γ a Γ' := by
  constructor
  · intro h
    cases a with
    | int => cases h; exact .int
    | bool => cases h; exact .bool
    | copy x =>
      simp only [useAtom] at h
      split at h
      · rename_i e he
        split at h
        · rename_i hc; cases h; exact .copy he hc.1 hc.2
        · cases h
      · cases h
    | move x =>
      simp only [useAtom] at h
      split at h
      · rename_i e he
        split at h
        · rename_i hc; cases h; exact .move he hc.1 hc.2
        · cases h
      · cases h
  · intro h
    cases h with
    | int => rfl
    | bool => rfl
    | copy hf hl hcp => simp [useAtom, hf, hl, hcp]
    | move hf hl hcp => simp [useAtom, hf, hl, hcp]

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

theorem uses_iff {Δ : Decls} {Γ Γ' : Ctx} {e : Expr} : uses Δ Γ e = some Γ' ↔ Uses Δ Γ e Γ' := by
  cases e with
  | atom a =>
    simp only [uses]
    constructor
    · intro h; exact .atom (useAtom_iff.mp h)
    · intro h; cases h with | atom h => exact useAtom_iff.mpr h
  | add l r =>
    simp only [uses]
    constructor
    · intro h
      split at h
      · rename_i Γ₁ h₁; exact .add (useAtom_iff.mp h₁) (useAtom_iff.mp h)
      · cases h
    · intro h
      cases h with
      | add h₁ h₂ => rw [useAtom_iff.mpr h₁]; exact useAtom_iff.mpr h₂
  | mk s fs =>
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
    if tyOf Δ Γ.tys e = some t ∧ knownViolates Γ e = false then
      match uses Δ Γ e with
      | some Γ₁ => if Fresh Γ₁ x then
          some ({ name := x, ty := t, live := true, used := false, known := knownExpr Γ e } :: Γ₁)
          else none
      | none => none
    else none
  | .let_ .wild t e =>
    if tyOf Δ Γ.tys e = some t ∧ knownViolates Γ e = false ∧ t.isCopy Δ = true then uses Δ Γ e
    else none
  | .letStruct s pat e =>
    if tyOf Δ Γ.tys e = some (.struct s) ∧ knownViolates Γ e = false then
      match findDecl Δ s with
      | some d => if patOkB d pat then
          match uses Δ Γ e with
          | some Γ₁ => if PatFresh Γ₁ pat then some (bindPat d pat Γ₁) else none
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
          exact .letWild (tyOf_iff.mp hc.1) hc.2.1 hc.2.2 (uses_iff.mp h)
        · cases h
      | name x =>
          simp only [stmtOut] at h
          split at h
          · rename_i ht
            split at h
            · rename_i Γ₁ hu
              split at h
              · cases h; exact .let_ (tyOf_iff.mp ht.1) ht.2 (uses_iff.mp hu) (by assumption)
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
                exact .letStruct (tyOf_iff.mp ht.1) ht.2 hd (patOkB_iff.mp hp) (uses_iff.mp hu)
                  (by assumption)
              · cases h
            · cases h
          · cases h
        · cases h
      · cases h
  · intro h
    cases h with
    | let_ ht hk hu hf => simp [stmtOut, tyOf_iff.mpr ht, hk, uses_iff.mpr hu, hf]
    | letWild ht hk hcopy hu => simp [stmtOut, tyOf_iff.mpr ht, hk, hcopy, uses_iff.mpr hu]
    | letStruct ht hk hd hp hu hf =>
      simp [stmtOut, tyOf_iff.mpr ht, hk, hd, patOkB_iff.mpr hp, uses_iff.mpr hu, hf]

/-- The executable form of `BodyOk`. -/
def bodyOkB (Δ : Decls) (Γ : Ctx) : List Stmt → Atom → Bool
  | [], r => tyOf Δ Γ.tys (.atom r) == some (.scalar (.int .u8)) &&
      match uses Δ Γ (.atom r) with
      | some Γ₁ => decide (Discharged Δ Γ₁)
      | none => false
  | s :: ss, r => match stmtOut Δ Γ s with
    | some Γ₁ => bodyOkB Δ Γ₁ ss r
    | none => false

theorem bodyOkB_iff {Δ : Decls} :
    ∀ {Γ : Ctx} {ss : List Stmt} {r : Atom}, bodyOkB Δ Γ ss r = true ↔ BodyOk Δ Γ ss r
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

/-! ### Slice-1 conformance examples

Each example gives the surface program in its doc comment, its Core syntax, and what the
specification says about it: accepted with an outcome, or refused. Accepted examples are proved
with `check_iff` and run with the evaluator; refused ones are proved not `Checked`. Locals are
written with their source name and a number: `t₀` is `⟨"t", 0⟩`. -/

namespace ConcreteSpec.Examples

open ConcreteSpec

/-- `struct Token {}` — empty, not `Copy`, so linear (OWN-D1). -/
def token : StructDecl := { name := "Token", copy := false, fields := [] }

/-- `struct Copy Unit0 {}` — empty and declared `Copy` (TYPE-14). -/
def unit0 : StructDecl := { name := "Unit0", copy := true, fields := [] }

/-- `struct Pair { a: u8, b: u8 }` — not `Copy`. -/
def pair : StructDecl := { name := "Pair", copy := false, fields := [("a", .int .u8), ("b", .int .u8)] }

def decls : Decls := [token, unit0, pair]

def u8 : Ty := .scalar (.int .u8)
def lit (n : Int) : Expr := .atom (.int .u8 n)
def mv (x : Var) : Expr := .atom (.move x)
def cp (x : Var) : Expr := .atom (.copy x)

def t₀ : Var := ⟨"t", 0⟩
def u₁ : Var := ⟨"u", 1⟩
def p₀ : Var := ⟨"p", 0⟩
def q₁ : Var := ⟨"q", 1⟩
def x₀ : Var := ⟨"x", 0⟩
def x₁ : Var := ⟨"x", 1⟩
def y₁ : Var := ⟨"y", 1⟩
def a₀ : Var := ⟨"a", 0⟩
def b₁ : Var := ⟨"b", 1⟩
def s₂ : Var := ⟨"s", 2⟩

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
  { decls, body := [.let_ (.name t₀) (.struct "Token") (.mk "Token" []),
                    .let_ (.name u₁) (.struct "Token") (mv t₀),
                    .letStruct "Token" [] (mv u₁)],
    ret := .int .u8 42 }

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
  { decls, body := [.let_ (.name t₀) (.struct "Token") (.mk "Token" []),
                    .let_ (.name u₁) (.struct "Token") (mv t₀),
                    .letStruct "Token" [] (mv t₀),
                    .letStruct "Token" [] (mv u₁)],
    ret := .int .u8 42 }

example : ¬ Checked useAfterMove := fun h => absurd ((check_iff _).mpr h) (by decide)

/-- The refusal is what keeps the program from getting stuck: run anyway, the model reads a moved
slot. This is the behaviour `checked_no_use_after_move` rules out for accepted programs. -/
example : run useAfterMove 10 = .stuck .movedRead := by decide

/-- Refused: a linear value read with `copy` (Q9.1: a linear local is only moved). Run anyway, the
read is in the wrong mode. -/
def copyLinear : Program :=
  { decls, body := [.let_ (.name t₀) (.struct "Token") (.mk "Token" []),
                    .letStruct "Token" [] (cp t₀)],
    ret := .int .u8 42 }

example : ¬ Checked copyLinear := fun h => absurd ((check_iff _).mpr h) (by decide)
example : run copyLinear 10 = .stuck .wrongMode := by decide

/-- Refused: the linear `t` is never consumed before `return` (OWN-2).
```
fn main() -> u8 {
    let t: Token = Token {};
    return 42;             // error: t is not consumed
}
``` -/
def notConsumed : Program :=
  { decls, body := [.let_ (.name t₀) (.struct "Token") (.mk "Token" [])], ret := .int .u8 42 }

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
  { decls, body := [.let_ (.name p₀) (.struct "Unit0") (.mk "Unit0" []),
                    .let_ (.name q₁) (.struct "Unit0") (cp p₀),
                    .letStruct "Unit0" [] (cp p₀),
                    .letStruct "Unit0" [] (cp q₁)],
    ret := .int .u8 0 }

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
  { decls, body := [.let_ (.name x₀) u8 (lit 1)], ret := .int .u8 42 }

example : ¬ Checked unusedBinding := fun h => absurd ((check_iff _).mpr h) (by decide)

/-- Accepted: destructuring binds every field, here under new names (PAT-1, PAT-5), and checked
addition in range (ARITH-3). In A-normal form `return x + y` binds the sum to a temporary first.
Exits with 42.
```
fn main() -> u8 {
    let p: Pair = Pair { a: 40, b: 2 };
    let Pair { a: x, b: y } = p;
    return x + y;
}
``` -/
def destructureAdd : Program :=
  { decls, body := [.let_ (.name p₀) (.struct "Pair") (.mk "Pair" [("a", .int .u8 40), ("b", .int .u8 2)]),
                    .letStruct "Pair" [("a", .name x₁), ("b", .name y₁)] (mv p₀),
                    .let_ (.name s₂) u8 (.add (.copy x₁) (.copy y₁))],
    ret := .copy s₂ }

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
  { decls, body := [.let_ (.name p₀) (.struct "Pair") (.mk "Pair" [("a", .int .u8 40), ("b", .int .u8 2)]),
                    .letStruct "Pair" [("a", .name x₁)] (mv p₀)],
    ret := .copy x₁ }

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
  { decls, body := [.let_ (.name p₀) (.struct "Pair") (.mk "Pair" [("a", .int .u8 42), ("b", .int .u8 0)]),
                    .letStruct "Pair" [("a", .name x₁), ("b", .wild)] (mv p₀)],
    ret := .copy x₁ }

example : Checked wildcardField := (check_iff _).mp (by decide)
example : run wildcardField 10 = .exit 42 := by decide

/-- Refused (Q6.1, Q6.2, Q6.4; ARITH-13, OD118): `a` and `b` are immutable `let`s of literals, so
they are statically known, and `a + b` is known to overflow `u8`. The refusal is justified: run
anyway, the addition traps (`knownViolation_traps`).
```
fn main() -> u8 {
    let a: u8 = 200;
    let b: u8 = 100;
    return a + b;          // error: 200 + 100 overflows u8
}
``` -/
def overflowTrap : Program :=
  { decls, body := [.let_ (.name a₀) u8 (lit 200),
                    .let_ (.name b₁) u8 (lit 100),
                    .let_ (.name s₂) u8 (.add (.copy a₀) (.copy b₁))],
    ret := .copy s₂ }

example : ¬ Checked overflowTrap := fun h => absurd ((check_iff _).mpr h) (by decide)
example : run overflowTrap 10 = .trap .arithmeticOverflow := by decide

/-- Refused for the same reason in a position whose value is never used (Q6.4: all written code
is checked): the known overflow is dropped by `_`, and the program would otherwise exit 42.
```
fn main() -> u8 {
    let _: u8 = 200 + 100;   // error: 200 + 100 overflows u8
    return 42;
}
``` -/
def overflowDropped : Program :=
  { decls, body := [.let_ .wild u8 (.add (.int .u8 200) (.int .u8 100))], ret := .int .u8 42 }

example : ¬ Checked overflowDropped := fun h => absurd ((check_iff _).mpr h) (by decide)

/-- Accepted, and traps at run time: the fields of a struct are not statically known values
(Q6.2's table has no entry for a struct literal or a field bound by destructuring), so the
checker cannot prove the addition overflows, and the checked operation traps when it runs
(ARITH-3, STMT-2). This is slice 1's accepted program with a runtime overflow.
```
fn main() -> u8 {
    let p: Pair = Pair { a: 200, b: 100 };
    let Pair { a: x, b: y } = p;
    return x + y;          // traps: arithmetic overflow
}
``` -/
def fieldOverflowTrap : Program :=
  { decls, body := [.let_ (.name p₀) (.struct "Pair") (.mk "Pair" [("a", .int .u8 200), ("b", .int .u8 100)]),
                    .letStruct "Pair" [("a", .name x₁), ("b", .name y₁)] (mv p₀),
                    .let_ (.name s₂) u8 (.add (.copy x₁) (.copy y₁))],
    ret := .copy s₂ }

example : Checked fieldOverflowTrap := (check_iff _).mp (by decide)
example : run fieldOverflowTrap 10 = .trap .arithmeticOverflow := by decide

/-- Refused: `300` does not fit `u8` (ARITH-2). -/
def literalTooBig : Program :=
  { decls, body := [.let_ (.name a₀) u8 (lit 300)], ret := .copy a₀ }

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
  { decls, body := [.let_ (.name a₀) u8 (lit 1),
                    .let_ (.name b₁) (.scalar (.int .i64)) (.atom (.int .i64 2)),
                    .let_ (.name s₂) u8 (.add (.copy a₀) (.copy b₁))],
    ret := .copy s₂ }

example : ¬ Checked mixedAdd := fun h => absurd ((check_iff _).mpr h) (by decide)

/-- Refused: one identity bound twice (Q9.1: every Core binder is fresh). Elaboration gives two
surface bindings of one name different identities, so this arises only from a faulty elaborator;
the surface rule against rebinding (SCOPE-1) is an elaboration rule. -/
def rebind : Program :=
  { decls, body := [.let_ (.name x₀) u8 (lit 1),
                    .let_ (.name x₀) u8 (lit 41)],
    ret := .copy x₀ }

example : ¬ Checked rebind := fun h => absurd ((check_iff _).mpr h) (by decide)

/-- Accepted: `_` drops a Copy value (OWN-5).
```
fn main() -> u8 {
    let _: u8 = 1;
    return 42;
}
``` -/
def wildCopy : Program :=
  { decls, body := [.let_ .wild u8 (lit 1)], ret := .int .u8 42 }

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
  { decls, body := [.let_ .wild (.struct "Token") (.mk "Token" [])], ret := .int .u8 42 }

example : ¬ Checked wildLinear := fun h => absurd ((check_iff _).mpr h) (by decide)

/-- Refused: `main` returns `u8` (STMT-7). -/
def wrongReturnType : Program :=
  { decls, body := [], ret := .int .i64 42 }

example : ¬ Checked wrongReturnType := fun h => absurd ((check_iff _).mpr h) (by decide)

end ConcreteSpec.Examples


/-! ## Part 2. Representation (Q3.11): `rep_roundtrip`

Core joins structured values and the raw store by a representation relation (Q3.11). This part
defines the layout and the byte encoding of the slice-1 types and proves that decoding an encoded
value gives the value back (row Q9.14's `rep_roundtrip`). The layout facts are language rules
(OD126): byte order is little-endian, a `bool` is one byte, and a struct with no fields occupies
no bytes (F13: a zero-sized value occupies no bytes). Integer widths follow from the type names
(TYPE-19). A `bool` byte is `0` for `false` and `1` for `true`; every other byte value, and an
uninitialized byte, is not a valid `bool`.

Not modelled here, because slice 1 does not need them: pointer fragments, permissions and
external frames (Q3.11), struct fields and their padding, and load/store against a block memory.
-/

namespace ConcreteSpec.Rep

open ConcreteSpec

/-- A memory byte: uninitialized, or initialized with a value below 256. -/
inductive MByte where
  | undef
  | byte (b : Fin 256)
  deriving DecidableEq, Repr

/-- The size in bytes of a scalar type (TYPE-19, OD126). -/
def scalarSize : STy → Nat
  | .int t => t.bits / 8
  | .bool => 1

/-- The size in bytes of a slice-1 type this part covers: a scalar, or a struct declared with no
fields (OD126, F13). Other structs are outside this part. -/
def Ty.size (Δ : Decls) : Ty → Option Nat
  | .scalar st => some (scalarSize st)
  | .struct s => match findDecl Δ s with
    | some d => if d.fields = [] then some 0 else none
    | none => none

/-- `n` as `w` little-endian bytes (OD126), modulo `256^w`. -/
def encodeNat : Nat → Nat → List MByte
  | 0, _ => []
  | w + 1, n => .byte ⟨n % 256, Nat.mod_lt _ (by decide)⟩ :: encodeNat w (n / 256)

/-- The number little-endian bytes denote, if every byte is initialized. -/
def decodeNat : List MByte → Option Nat
  | [] => some 0
  | .byte b :: rest => (decodeNat rest).map (fun m => b.val + 256 * m)
  | .undef :: _ => none

/-- A scalar's bytes. An integer is stored as its two's-complement bit pattern (its value modulo
`2^bits`) in `bits / 8` little-endian bytes; a `bool` is one byte, `0` or `1`. -/
def encodeScalar : Scalar → List MByte
  | .int t n => encodeNat (t.bits / 8) (n % (2 : Int) ^ t.bits).toNat
  | .bool b => [.byte (if b then 1 else 0)]

/-- The integer a bit pattern `m` denotes in `t`: `m` itself, or `m - 2^bits` for a signed type
whose top bit is set. -/
def ofBits (t : IntTy) (m : Nat) : Int :=
  if t.signed = true ∧ m ≥ 2 ^ (t.bits - 1) then (m : Int) - (2 : Int) ^ t.bits else (m : Int)

/-- The scalar of type `st` that `bs` represents, if any. The byte count must be the type's size,
every byte must be initialized, and a `bool` byte must be `0` or `1`. -/
def decodeScalar : STy → List MByte → Option Scalar
  | .int t, bs =>
    if bs.length = t.bits / 8 then (decodeNat bs).map (fun m => .int t (ofBits t m)) else none
  | .bool, [.byte b] =>
    if b.val = 0 then some (.bool false) else if b.val = 1 then some (.bool true) else none
  | .bool, _ => none

/-- A value's bytes: a scalar's encoding, or no bytes for a struct with no fields. -/
def encodeVal : Val → List MByte
  | .scalar s => encodeScalar s
  | .struct _ _ => []

/-- The value of type `t` that `bs` represents, if any. -/
def decodeVal (Δ : Decls) : Ty → List MByte → Option Val
  | .scalar st, bs => (decodeScalar st bs).map .scalar
  | .struct s, [] => match findDecl Δ s with
    | some d => if d.fields = [] then some (.struct s []) else none
    | none => none
  | .struct _, _ :: _ => none

/-- `v` is a value of type `t` that this part represents: a well-typed scalar, or the value of a
struct declared with no fields. -/
inductive RepVal (Δ : Decls) : Val → Ty → Prop where
  | scalar {v st} : SVTy v st → RepVal Δ (.scalar v) (.scalar st)
  | empty {s d} : findDecl Δ s = some d → d.fields = [] → RepVal Δ (.struct s []) (.struct s)

theorem encodeNat_length : ∀ (w n : Nat), (encodeNat w n).length = w
  | 0, _ => rfl
  | w + 1, n => by simp [encodeNat, encodeNat_length w (n / 256)]

theorem decodeNat_encodeNat : ∀ (w n : Nat), decodeNat (encodeNat w n) = some (n % 256 ^ w)
  | 0, n => by simp [encodeNat, decodeNat, Nat.mod_one]
  | w + 1, n => by
    simp only [encodeNat, decodeNat, decodeNat_encodeNat w (n / 256), Option.map_some,
      Option.some.injEq]
    rw [Nat.pow_succ, Nat.mul_comm (256 ^ w) 256, Nat.mod_mul]

/-- The integer round trip, for every integer type: the bit pattern of an in-range value decodes
to the value. -/
theorem int_roundtrip (t : IntTy) (n : Int) (h : t.inRange n = true) :
    ofBits t ((n % (2 : Int) ^ t.bits).toNat % 256 ^ (t.bits / 8)) = n := by
  have hpos : (0 : Int) < (2 : Int) ^ t.bits := by
    cases t <;> decide
  have hnn : 0 ≤ n % (2 : Int) ^ t.bits := Int.emod_nonneg _ (by omega)
  have hlt : n % (2 : Int) ^ t.bits < (2 : Int) ^ t.bits := Int.emod_lt_of_pos _ hpos
  have hpow : ((256 ^ (t.bits / 8) : Nat) : Int) = (2 : Int) ^ t.bits := by
    cases t <;> decide
  have hsmall : (n % (2 : Int) ^ t.bits).toNat < 256 ^ (t.bits / 8) := by
    have : (((n % (2 : Int) ^ t.bits).toNat : Nat) : Int) < ((256 ^ (t.bits / 8) : Nat) : Int) := by
      rw [Int.toNat_of_nonneg hnn, hpow]; exact hlt
    exact Int.ofNat_lt.mp this
  rw [Nat.mod_eq_of_lt hsmall]
  unfold ofBits
  have hm := Int.toNat_of_nonneg hnn
  generalize (n % (2 : Int) ^ t.bits).toNat = M at hm ⊢
  simp only [IntTy.inRange, decide_eq_true_eq] at h
  split
  · rename_i hc
    cases t <;>
      simp only [IntTy.signed, IntTy.min, IntTy.max, IntTy.bits, Nat.reducePow, Int.reducePow,
        Nat.reduceSub, ↓reduceIte, Bool.false_eq_true, false_and, true_and] at hc h hm ⊢ <;>
      omega
  · rename_i hc
    cases t <;>
      simp only [IntTy.signed, IntTy.min, IntTy.max, IntTy.bits, Nat.reducePow, Int.reducePow,
        Nat.reduceSub, ↓reduceIte, Bool.false_eq_true, false_and, true_and, Nat.not_le] at hc h hm ⊢ <;>
      omega

/-- **`rep_roundtrip`** (Q9.14, Q3.11). For every slice-1 type this part covers (`u8`…`u64`,
`i8`…`i64`, `bool` and a struct with no fields), encoding a well-typed value gives exactly the
type's size in bytes, and decoding those bytes gives the value back. -/
theorem rep_roundtrip {Δ : Decls} {v : Val} {t : Ty} (h : RepVal Δ v t) :
    Ty.size Δ t = some (encodeVal v).length ∧ decodeVal Δ t (encodeVal v) = some v := by
  cases h with
  | empty hd hf => simp [Ty.size, encodeVal, decodeVal, hd, hf]
  | scalar hs =>
    cases hs with
    | bool =>
      rename_i b
      cases b <;> simp [Ty.size, scalarSize, encodeVal, encodeScalar, decodeVal, decodeScalar]
    | int hin =>
      rename_i t n
      refine ⟨by simp [Ty.size, scalarSize, encodeVal, encodeScalar, encodeNat_length], ?_⟩
      simp only [decodeVal, encodeVal, encodeScalar, decodeScalar, encodeNat_length,
        ↓reduceIte, decodeNat_encodeNat, Option.map_some, int_roundtrip t n hin]

/-- Non-vacuity: decoding is not total. An uninitialized byte is no `u8`, byte `2` is no `bool`,
and a byte count other than the size is no value. -/
example : decodeScalar (.int .u8) [.undef] = none := by decide
example : decodeScalar .bool [.byte 2] = none := by decide
example : decodeScalar (.int .u16) [.byte 1] = none := by decide

/-- Little-endian, two's complement (OD126): `-2 : i16` is the bytes `0xfe 0xff`. -/
example : encodeScalar (.int .i16 (-2)) = [.byte 254, .byte 255] := by decide

end ConcreteSpec.Rep

/-! ## Part 3. Structured cleanup (Q9.8): `defer_cleanup_releases`

Core keeps `defer` as nested deferred scopes (Q9.8). A block registers Unit-only cleanup calls
(`destroy(x)` for a destroyable value, `give_up(x)` for resource cleanup; Q2.1, Q2.7); each call
may hold loans, released after the call. On a structured exit (falling off the end of a block,
`break L`, `continue L`, `return`) the registered calls of every scope the exit leaves run in
reverse registration order, innermost scope first. An abort runs no cleanup, and a cleanup call
that traps stops the run (Q9.8). A value's cleanup must run before the cleanup of an allocator it
depends on (Q1.5, Q1.3: resources are consumed before their allocator); registration checks this.

The model is the cleanup stack itself: frames of registered calls and the exits that unwind them.
What a deferred call does is abstracted to its identity, its dependencies, its loans and whether
it traps (`ok`). Not modelled here: the statements between registrations, late reads of the
deferred arguments, and evaluating a `return` value into an owned temporary before cleanup (Q9.8).
-/

namespace ConcreteSpec.Cleanup

/-- A resource in this part: a value to destroy or give up, or an allocator. -/
abbrev Res := Nat

/-- A loan held by a deferred call. -/
abbrev Loan := Nat

/-- The kind of a deferred call (Q2.1): `destroy` consumes a destroyable value, `give_up` cleans up
a resource (an allocator's release included). Both are Unit-only (Q9.8). -/
inductive Kind where
  | destroy
  | giveUp
  deriving DecidableEq, Repr

/-- A registered cleanup call: its kind, the resource it consumes, the allocators that resource
depends on (Q1.5) and the loans it holds until it has run (Q9.8). -/
structure Deferred where
  kind : Kind
  target : Res
  deps : List Res
  loans : List Loan
  deriving DecidableEq, Repr

/-- What a scope is: a plain block, a loop body (with its loop identity, F7) or a function body. -/
inductive FrameKind where
  | block
  | loopBody (l : Nat)
  | fnBody
  deriving DecidableEq, Repr

/-- A scope and the calls it has registered, newest first. -/
structure Frame where
  kind : FrameKind
  defers : List Deferred
  deriving DecidableEq, Repr

/-- The scopes at a point, innermost first. -/
abbrev Stack := List Frame

/-- The calls the frames hold, in the order they run: innermost frame first, and within a frame
the newest registration first (reverse registration order). -/
def pending (fs : List Frame) : List Deferred := fs.flatMap Frame.defers

/-- A structured exit (F7: `break` and `continue` name their loop), or an abort. -/
inductive Exit where
  | fallthrough
  | brk (l : Nat)
  | cont (l : Nat)
  | ret
  | abort
  deriving DecidableEq, Repr

/-- The frames up to and including the innermost one of the given kind. -/
def unwindTo (p : FrameKind → Bool) : Stack → Option (List Frame)
  | [] => none
  | f :: st => if p f.kind then some [f] else (unwindTo p st).map (f :: ·)

/-- The frames an exit leaves, innermost first: falling off a block leaves that block; `break L`
and `continue L` leave every scope up to and including loop `L`'s body (`continue` then tests the
condition again, F7); `return` leaves every scope of the function. An abort runs no cleanup. -/
def unwound : Exit → Stack → Option (List Frame)
  | .fallthrough, f :: _ => some [f]
  | .fallthrough, [] => none
  | .brk l, st => unwindTo (fun k => k == .loopBody l) st
  | .cont l, st => unwindTo (fun k => k == .loopBody l) st
  | .ret, st => unwindTo (fun k => k == .fnBody) st
  | .abort, _ => some []

/-- An observable cleanup event: a call, or the release of a loan. -/
inductive Event where
  | call (d : Deferred)
  | release (l : Loan)
  deriving DecidableEq, Repr

/-- How cleanup ends. -/
inductive Result where
  | done
  | trapped
  deriving DecidableEq, Repr

/-- Running cleanup calls in order (Q9.8). `ok d` says whether call `d` returns or traps. A call
that returns is followed by the release of each loan it held; a call that traps ends the run,
and no later cleanup runs. -/
inductive RunCleanup (ok : Deferred → Bool) : List Deferred → List Event → Result → Prop where
  | nil : RunCleanup ok [] [] .done
  | step {d ds tr r} : ok d = true → RunCleanup ok ds tr r →
      RunCleanup ok (d :: ds) (.call d :: d.loans.map .release ++ tr) r
  | trap {d ds} : ok d = false → RunCleanup ok (d :: ds) [.call d] .trapped

/-- The events of calls that all return: each call followed by its loan releases. -/
def expected (ds : List Deferred) : List Event :=
  ds.flatMap (fun d => .call d :: d.loans.map .release)

/-- The dependency order of a run list (Q1.5): no call releases a resource that a later call
depends on, and no resource depends on itself. -/
inductive DepOrder : List Deferred → Prop where
  | nil : DepOrder []
  | cons {d ds} : d.target ∉ d.deps → (∀ d' ∈ ds, d.target ∉ d'.deps) → DepOrder ds →
      DepOrder (d :: ds)

/-- How a block builds its cleanup stack. Entering a scope pushes an empty frame; `defer` adds a
call to the innermost frame, provided the call's resource does not depend on itself and no call
already registered, in any enclosing scope, depends on it (Q1.5: a value is cleaned up before its
allocator); leaving a scope removes its frame after its cleanup ran. -/
inductive Reg : Stack → Prop where
  | empty : Reg []
  | enter {st k} : Reg st → Reg (⟨k, []⟩ :: st)
  | push {k ds st d} : Reg (⟨k, ds⟩ :: st) → d.target ∉ d.deps →
      (∀ d' ∈ pending (⟨k, ds⟩ :: st), d.target ∉ d'.deps) → Reg (⟨k, d :: ds⟩ :: st)
  | leave {f st} : Reg (f :: st) → Reg st

theorem depOrder_append_left : ∀ {l₁ l₂ : List Deferred}, DepOrder (l₁ ++ l₂) → DepOrder l₁
  | [], _, _ => .nil
  | d :: l₁, l₂, h => by
    cases h with
    | cons hs hall hrest =>
      exact .cons hs (fun d' hd' => hall d' (List.mem_append_left _ hd'))
        (depOrder_append_left hrest)

theorem depOrder_append_right : ∀ {l₁ l₂ : List Deferred}, DepOrder (l₁ ++ l₂) → DepOrder l₂
  | [], _, h => h
  | _ :: l₁, _, h => by cases h with | cons _ _ hrest => exact depOrder_append_right hrest

theorem pending_cons (f : Frame) (st : Stack) : pending (f :: st) = f.defers ++ pending st := by
  simp [pending]

theorem pending_append (fs st : List Frame) : pending (fs ++ st) = pending fs ++ pending st := by
  simp [pending]

/-- Registration keeps the run order dependency-safe. -/
theorem reg_depOrder {st : Stack} (h : Reg st) : DepOrder (pending st) := by
  induction h with
  | empty => exact .nil
  | enter _ ih => simpa [pending_cons] using ih
  | push _ hs hall ih =>
    rw [pending_cons] at ih hall ⊢
    exact .cons hs hall ih
  | leave _ ih => rw [pending_cons] at ih; exact depOrder_append_right ih

theorem unwindTo_prefix {p : FrameKind → Bool} :
    ∀ {st : Stack} {fs : List Frame}, unwindTo p st = some fs → ∃ rest, st = fs ++ rest
  | [], fs, h => by simp [unwindTo] at h
  | f :: st, fs, h => by
    simp only [unwindTo] at h
    split at h
    · cases h; exact ⟨st, rfl⟩
    · cases hu : unwindTo p st with
      | none => rw [hu] at h; cases h
      | some fs' =>
        rw [hu] at h
        cases h
        obtain ⟨rest, rfl⟩ := unwindTo_prefix hu
        exact ⟨rest, rfl⟩

/-- An exit leaves a prefix of the scopes: the innermost ones. -/
theorem unwound_prefix {e : Exit} {st : Stack} {fs : List Frame} (h : unwound e st = some fs) :
    ∃ rest, st = fs ++ rest := by
  cases e with
  | fallthrough =>
    cases st with
    | nil => simp [unwound] at h
    | cons f st => simp [unwound] at h; subst h; exact ⟨st, rfl⟩
  | brk l => exact unwindTo_prefix h
  | cont l => exact unwindTo_prefix h
  | ret => exact unwindTo_prefix h
  | abort => simp [unwound] at h; subst h; exact ⟨st, rfl⟩

theorem runCleanup_all_ok {ok : Deferred → Bool} :
    ∀ {ds : List Deferred}, (∀ d ∈ ds, ok d = true) →
      ∀ tr r, RunCleanup ok ds tr r ↔ tr = expected ds ∧ r = .done
  | [], _, tr, r => by
    constructor
    · intro h; cases h; exact ⟨rfl, rfl⟩
    · rintro ⟨rfl, rfl⟩; exact .nil
  | d :: ds, hok, tr, r => by
    have hd : ok d = true := hok d (List.Mem.head _)
    have ih := runCleanup_all_ok (ds := ds) (fun x hx => hok x (List.Mem.tail _ hx))
    constructor
    · intro h
      cases h with
      | step _ h' =>
        obtain ⟨rfl, rfl⟩ := (ih _ _).mp h'
        exact ⟨by simp [expected], rfl⟩
      | trap hf => rw [hd] at hf; cases hf
    · rintro ⟨rfl, rfl⟩
      have := RunCleanup.step (ok := ok) hd ((ih (expected ds) .done).mpr ⟨rfl, rfl⟩)
      simpa [expected] using this

theorem runCleanup_trap {ok : Deferred → Bool} {d : Deferred} {post : List Deferred} :
    ∀ {pre : List Deferred}, (∀ x ∈ pre, ok x = true) → ok d = false →
      ∀ tr r, RunCleanup ok (pre ++ d :: post) tr r ↔ tr = expected pre ++ [.call d] ∧ r = .trapped
  | [], _, hd, tr, r => by
    simp only [List.nil_append, expected, List.flatMap_nil]
    constructor
    · intro h
      cases h with
      | step h' _ => rw [hd] at h'; cases h'
      | trap _ => exact ⟨rfl, rfl⟩
    · rintro ⟨rfl, rfl⟩; exact .trap hd
  | x :: pre, hok, hd, tr, r => by
    have hx : ok x = true := hok x (List.Mem.head _)
    have ih := runCleanup_trap (post := post) (pre := pre) (fun y hy => hok y (List.Mem.tail _ hy)) hd
    simp only [List.cons_append]
    constructor
    · intro h
      cases h with
      | step _ h' =>
        obtain ⟨rfl, rfl⟩ := (ih _ _).mp h'
        exact ⟨by simp [expected], rfl⟩
      | trap hf => rw [hx] at hf; cases hf
    · rintro ⟨rfl, rfl⟩
      have := RunCleanup.step (ok := ok) hx ((ih (expected pre ++ [.call d]) .trapped).mpr ⟨rfl, rfl⟩)
      simpa [expected] using this

/-- In a dependency-ordered run list, a call runs before the release of an allocator it depends
on. -/
theorem depOrder_before :
    ∀ {ds : List Deferred}, DepOrder ds → ∀ {d da : Deferred}, d ∈ ds → da ∈ ds →
      da.target ∈ d.deps → ∃ pre mid post, ds = pre ++ d :: mid ++ da :: post
  | [], _, _, _, hd, _, _ => by cases hd
  | x :: ds, .cons hself hall hrest, d, da, hd, hda, hdep => by
    by_cases hxa : x = da
    · subst hxa
      by_cases hxd : x = d
      · subst hxd; exact absurd hdep hself
      · have hd' : d ∈ ds := by
          cases hd with
          | head => exact absurd rfl hxd
          | tail _ h => exact h
        exact absurd hdep (hall d hd')
    · have hda' : da ∈ ds := by
        cases hda with
        | head => exact absurd rfl hxa
        | tail _ h => exact h
      by_cases hxd : x = d
      · subst hxd
        obtain ⟨mid, post, hsplit⟩ := List.append_of_mem hda'
        exact ⟨[], mid, post, by simp [hsplit]⟩
      · have hd' : d ∈ ds := by
          cases hd with
          | head => exact absurd rfl hxd
          | tail _ h => exact h
        obtain ⟨pre, mid, post, hsplit⟩ := depOrder_before hrest hd' hda' hdep
        exact ⟨x :: pre, mid, post, by simp [hsplit]⟩

/-- **`defer_cleanup_releases`** (Q9.14, Q9.8, Q1.5). For every cleanup stack a block can build
and every structured exit (falling through, `break L`, `continue L`, `return`), the calls of the
scopes the exit leaves:

1. when every call returns, run exactly in reverse registration order, innermost scope first,
   each call immediately followed by the release of each loan it held (`expected`);
2. respect the allocator-dependency check: a call runs before the release of any allocator its
   resource depends on;
3. when a call traps, stop there: the earlier calls ran with their loan releases, the trapping
   call is the last event, and no later cleanup runs. -/
theorem defer_cleanup_releases {ok : Deferred → Bool} {st : Stack} (hreg : Reg st) {e : Exit}
    (he : e ≠ .abort) {fs : List Frame} (hu : unwound e st = some fs) :
    ((∀ d ∈ pending fs, ok d = true) →
      ∀ tr r, RunCleanup ok (pending fs) tr r ↔ tr = expected (pending fs) ∧ r = .done) ∧
    (∀ d da, d ∈ pending fs → da ∈ pending fs → da.target ∈ d.deps →
      ∃ t₁ t₂ t₃, expected (pending fs) = t₁ ++ .call d :: t₂ ++ .call da :: t₃) ∧
    (∀ pre d post, pending fs = pre ++ d :: post → (∀ x ∈ pre, ok x = true) → ok d = false →
      ∀ tr r, RunCleanup ok (pending fs) tr r ↔ tr = expected pre ++ [.call d] ∧ r = .trapped) := by
  have _ := he
  obtain ⟨rest, hst⟩ := unwound_prefix hu
  have hdo : DepOrder (pending fs) := by
    have := reg_depOrder hreg
    rw [hst, pending_append] at this
    exact depOrder_append_left this
  refine ⟨fun hok => runCleanup_all_ok hok, ?_, ?_⟩
  · intro d da hd hda hdep
    obtain ⟨pre, mid, post, hsplit⟩ := depOrder_before hdo hd hda hdep
    refine ⟨expected pre, d.loans.map .release ++ expected mid, da.loans.map .release ++ expected post, ?_⟩
    rw [hsplit]
    simp [expected, List.flatMap_append]
  · intro pre d post hsplit hok hd tr r
    rw [hsplit]
    exact runCleanup_trap hok hd tr r

/-- An abort runs no cleanup (Q9.8). -/
theorem abort_no_cleanup (st : Stack) : unwound .abort st = some [] ∧ pending [] = [] := ⟨rfl, rfl⟩

/-! ### Examples -/

/-- An allocator `a` (resource 0) and a value `v` (resource 1) allocated from it, which holds
loan 7 while it is cleaned up. `defer give_up(a)` comes first and `defer destroy(v)` second. -/
def relA : Deferred := { kind := .giveUp, target := 0, deps := [], loans := [] }
def desV : Deferred := { kind := .destroy, target := 1, deps := [0], loans := [7] }

/-- Registration in that order is accepted. -/
example : Reg [⟨.fnBody, [desV, relA]⟩] :=
  .push (.push (.enter .empty) (by decide) (by decide)) (by decide) (by decide)

/-- `return` runs `destroy(v)`, releases loan 7, then runs `give_up(a)`. -/
example : RunCleanup (fun _ => true) (pending [⟨.fnBody, [desV, relA]⟩])
    [.call desV, .release 7, .call relA] .done := .step rfl (.step rfl .nil)

/-- Registering them the other way round is refused: `give_up(a)` would run first while `v`
still depends on `a`. -/
example : ¬ (∀ d' ∈ pending [⟨.fnBody, [desV]⟩], relA.target ∉ d'.deps) := by decide

end ConcreteSpec.Cleanup

/-! ## Part 4. Scoped borrows (Q3.3, Q3.4, Q9.5, REF-11): `open_mut_freezes_root`

Core's one borrow form, `borrow [mut] src as v { body }` (Q9.5; the range clause and `else` of
Q4.7 add a validity test before the body and do not change the access rules modelled here).
Every place records the owner root it comes from (Q3.3: views keep the original owner root), whether
it may be written, whether it owns its value, and how the enclosing borrows lock it. A shared open
locks every place of the root against writes and moves; a mutable open freezes every place of the
root, the root included, for the whole block (REF-11). Two accesses conflict only when one of
them is exclusive, so shared opens nest. The view binder is fresh and second-class.
-/

namespace ConcreteSpec.Borrow

/-- How the enclosing borrows lock a place. -/
inductive Lock where
  | free
  | shared
  | excl
  deriving DecidableEq, Repr

/-- A place: its owner root, whether it may be written, whether it owns its value (only owners
move), and its lock. -/
structure Place where
  root : Nat
  mutable : Bool
  owner : Bool
  lock : Lock
  deriving DecidableEq, Repr

/-- The places in scope, by fresh identity (Q9.1). -/
abbrev Ctx := List (Nat × Place)

def find : Ctx → Nat → Option Place
  | [], _ => none
  | (n, q) :: Γ, p => if n = p then some q else find Γ p

def mapPlaces (f : Place → Place) (Γ : Ctx) : Ctx := Γ.map fun e => (e.1, f e.2)

def remove (p : Nat) : Ctx → Ctx
  | [] => []
  | (n, q) :: Γ => if n = p then remove p Γ else (n, q) :: remove p Γ

/-- The lock a borrow of root `r` puts on each place: a mutable open freezes every place of the
root; a shared open locks the free ones against writes and moves. -/
def lockFor (m : Bool) (r : Nat) (q : Place) : Place :=
  if q.root = r then
    if m then { q with lock := .excl }
    else if q.lock = .free then { q with lock := .shared } else q
  else q

/-- The view a borrow introduces: same root, writable only when the open is mutable, never an
owner. -/
def view (m : Bool) (src : Place) : Place :=
  { root := src.root, mutable := m, owner := false, lock := .free }

/-- The places inside `borrow [mut] src as v { … }`. -/
def bodyCtx (m : Bool) (v : Nat) (src : Place) (Γ : Ctx) : Ctx :=
  (v, view m src) :: mapPlaces (lockFor m src.root) Γ

inductive Stmt where
  | read (p : Nat)
  | write (p : Nat)
  | move (p : Nat)
  | borrow (m : Bool) (src v : Nat) (body : List Stmt)

/-- The access rules. Read: not frozen. Write: unlocked and writable. Move: unlocked and owner;
the place is gone afterwards. Shared open: the source is not frozen. Mutable open: the source is
unlocked and writable (a shared view never upgrades, Q3.3). The view name is fresh. After the
block the outer places are as before. -/
inductive Ok : Ctx → List Stmt → Prop where
  | nil {Γ} : Ok Γ []
  | read {Γ p q rest} : find Γ p = some q → q.lock ≠ .excl → Ok Γ rest → Ok Γ (.read p :: rest)
  | write {Γ p q rest} : find Γ p = some q → q.lock = .free → q.mutable = true → Ok Γ rest →
      Ok Γ (.write p :: rest)
  | move {Γ p q rest} : find Γ p = some q → q.lock = .free → q.owner = true →
      Ok (remove p Γ) rest → Ok Γ (.move p :: rest)
  | borrow {Γ m s v q body rest} : find Γ s = some q → q.lock ≠ .excl →
      (m = true → q.lock = .free ∧ q.mutable = true) → find Γ v = none →
      Ok (bodyCtx m v q Γ) body → Ok Γ rest → Ok Γ (.borrow m s v body :: rest)

/-- A statement list names place `p`, at any depth. -/
inductive Touches (p : Nat) : List Stmt → Prop where
  | read {rest} : Touches p (.read p :: rest)
  | write {rest} : Touches p (.write p :: rest)
  | move {rest} : Touches p (.move p :: rest)
  | src {m v body rest} : Touches p (.borrow m p v body :: rest)
  | body {m s v body rest} : Touches p body → Touches p (.borrow m s v body :: rest)
  | tail {s rest} : Touches p rest → Touches p (s :: rest)

/-- The places after a statement. -/
def next (Γ : Ctx) : Stmt → Ctx
  | .move p => remove p Γ
  | _ => Γ

/-- A statement list writes root `r`, or grants a write to it, at any depth: a write or move of a
place of `r`, or a mutable open of one, with places resolved as the checker resolves them. -/
inductive WritesRoot (r : Nat) : Ctx → List Stmt → Prop where
  | write {Γ p q rest} : find Γ p = some q → q.root = r → WritesRoot r Γ (.write p :: rest)
  | move {Γ p q rest} : find Γ p = some q → q.root = r → WritesRoot r Γ (.move p :: rest)
  | mutOpen {Γ s q v body rest} : find Γ s = some q → q.root = r →
      WritesRoot r Γ (.borrow true s v body :: rest)
  | body {Γ m s q v body rest} : find Γ s = some q → WritesRoot r (bodyCtx m v q Γ) body →
      WritesRoot r Γ (.borrow m s v body :: rest)
  | tail {Γ s rest} : WritesRoot r (next Γ s) rest → WritesRoot r Γ (s :: rest)

theorem find_mapPlaces (f : Place → Place) : ∀ (Γ : Ctx) (p : Nat),
    find (mapPlaces f Γ) p = (find Γ p).map f
  | [], _ => rfl
  | (n, q) :: Γ, p => by
    show find ((n, f q) :: mapPlaces f Γ) p = _
    simp only [find]
    split
    · rfl
    · exact find_mapPlaces f Γ p

theorem find_remove (p' : Nat) : ∀ (Γ : Ctx) (p : Nat),
    find (remove p' Γ) p = if p = p' then none else find Γ p
  | [], _ => by simp [remove, find]
  | (n, q) :: Γ, p => by
    have ih := find_remove p' Γ p
    by_cases hn : n = p' <;> by_cases hp : p = p' <;> by_cases hnp : n = p <;>
      simp_all [remove, find] <;> omega

theorem find_bodyCtx {m v src Γ p} (hv : p ≠ v) :
    find (bodyCtx m v src Γ) p = (find Γ p).map (lockFor m src.root) := by
  simp only [bodyCtx, find, show ¬ v = p from fun h => hv h.symm, ↓reduceIte]
  exact find_mapPlaces _ Γ p

theorem lockFor_excl {m r q} (h : q.lock = .excl) : (lockFor m r q).lock = .excl := by
  unfold lockFor; split <;> (try split) <;> (try split) <;> simp_all

theorem ne_of_find {Γ p v q} (hp : find Γ p = some q) (hv : find Γ v = none) : p ≠ v := by
  rintro rfl; rw [hp] at hv; cases hv

/-- A frozen place is never named, at any depth. -/
theorem excl_untouched {Γ ss} (h : Ok Γ ss) :
    ∀ {p q}, find Γ p = some q → q.lock = .excl → ¬ Touches p ss := by
  induction h with
  | nil => intro p q _ _ ht; cases ht
  | read hf hl _ ih =>
    intro p q hp hq ht
    cases ht with
    | read => rw [hf] at hp; cases hp; exact hl hq
    | tail ht => exact ih hp hq ht
  | write hf hl _ _ ih =>
    intro p q hp hq ht
    cases ht with
    | write => rw [hf] at hp; cases hp; rw [hl] at hq; cases hq
    | tail ht => exact ih hp hq ht
  | @move Γ p0 q0 rest hf hl _ _ ih =>
    intro p q hp hq ht
    have hne : p ≠ p0 := by
      rintro rfl; rw [hf] at hp; cases hp; rw [hl] at hq; cases hq
    cases ht with
    | move => exact hne rfl
    | tail ht =>
      refine ih (q := q) ?_ hq ht
      rw [find_remove]; simp only [hne, ↓reduceIte]; exact hp
  | @borrow Γ m s v q0 body rest hs hnex _ hv _ _ ihb ih =>
    intro p q hp hq ht
    cases ht with
    | src => rw [hs] at hp; cases hp; exact hnex hq
    | body ht =>
      refine ihb (q := lockFor m q0.root q) ?_ (lockFor_excl hq) ht
      rw [find_bodyCtx (ne_of_find hp hv), hp]; rfl
    | tail ht => exact ih hp hq ht

/-- **`open_mut_freezes_root`** (Q9.14, Q3.3, Q3.4, REF-11). Inside an accepted
`borrow mut src as v { body }`, no place of `src`'s root that existed before the open (the root
itself and every view derived from it) is read, written, moved or opened again, at any depth.
Only the new view `v` reaches the root. -/
theorem open_mut_freezes_root {Γ s v body rest q} (h : Ok Γ (.borrow true s v body :: rest))
    (hs : find Γ s = some q) :
    ∀ p q', find Γ p = some q' → q'.root = q.root → ¬ Touches p body := by
  intro p q' hp hr
  cases h with
  | borrow hs' _ _ hv hb _ =>
    rw [hs] at hs'; cases hs'
    refine excl_untouched hb (q := lockFor true q.root q') ?_ (by simp [lockFor, hr]) 
    rw [find_bodyCtx (ne_of_find hp hv), hp]; rfl

/-- The places of root `r` grant no write: each is locked, or neither writable nor an owner. -/
def NoWrite (r : Nat) (Γ : Ctx) : Prop :=
  ∀ p q, find Γ p = some q → q.root = r → q.lock = .free → q.mutable = false ∧ q.owner = false

theorem noWrite_remove {r Γ p'} (h : NoWrite r Γ) : NoWrite r (remove p' Γ) := by
  intro p q hp
  rw [find_remove] at hp
  split at hp
  · cases hp
  · exact h p q hp

theorem noWrite_bodyCtx {r Γ m v q0} (h : NoWrite r Γ)
    (hm : m = true → q0.lock = .free ∧ q0.mutable = true)
    (hsrc : q0.root = r → m = true → False) : NoWrite r (bodyCtx m v q0 Γ) := by
  intro p q hp hr hl
  by_cases hpv : p = v
  · subst hpv
    simp only [bodyCtx, find, ↓reduceIte] at hp
    cases hp
    simp only [view] at hr ⊢
    cases m with
    | false => simp
    | true => exact (hsrc hr rfl).elim
  · rw [find_bodyCtx hpv] at hp
    cases hq : find Γ p with
    | none => rw [hq] at hp; cases hp
    | some q1 =>
      rw [hq] at hp; cases hp
      have _ := hm
      by_cases h1 : q1.root = q0.root
      · by_cases h2 : q1.lock = .free <;> cases m <;> simp [lockFor, h1, h2] at hl
      · simp only [lockFor, h1, ↓reduceIte] at hr hl ⊢
        exact h p q1 hq hr hl

/-- An accepted list never writes a root whose places grant no write. -/
theorem noWrite_sound {Γ ss} (h : Ok Γ ss) : ∀ {r}, NoWrite r Γ → ¬ WritesRoot r Γ ss := by
  induction h with
  | nil => intro r _ hw; cases hw
  | read _ _ _ ih => intro r hn hw; cases hw with | tail hw => exact ih hn hw
  | write hf hl hmut _ ih =>
    intro r hn hw
    cases hw with
    | write hf' hr => rw [hf] at hf'; cases hf'; rw [(hn _ _ hf hr hl).1] at hmut; cases hmut
    | tail hw => exact ih hn hw
  | move hf hl hown _ ih =>
    intro r hn hw
    cases hw with
    | move hf' hr => rw [hf] at hf'; cases hf'; rw [(hn _ _ hf hr hl).2] at hown; cases hown
    | tail hw => exact ih (noWrite_remove hn) hw
  | @borrow Γ m s v q0 body rest hs _ hm _ _ _ ihb ih =>
    intro r hn hw
    have hbad : q0.root = r → m = true → False := fun hr hmt => by
      obtain ⟨hl, hmut⟩ := hm hmt
      rw [(hn _ _ hs hr hl).1] at hmut; cases hmut
    cases hw with
    | mutOpen hs' hr => rw [hs] at hs'; cases hs'; exact hbad hr rfl
    | body hs' hw => rw [hs] at hs'; cases hs'; exact ihb (noWrite_bodyCtx hn hm hbad) hw
    | tail hw => exact ih hn hw

/-- **`open_mut_freezes_root`, shared half** (Q9.14, Q3.3). A shared open never grants a write:
inside an accepted `borrow src as v { body }`, nothing at any depth writes, moves or mutably
opens a place of `src`'s root, whether through the root, through `v`, or through any view
derived from either. -/
theorem shared_open_grants_no_write {Γ s v body rest q}
    (h : Ok Γ (.borrow false s v body :: rest)) (hs : find Γ s = some q) :
    ¬ WritesRoot q.root (bodyCtx false v q Γ) body := by
  cases h with
  | borrow hs' _ hm _ hb _ =>
    rw [hs] at hs'; cases hs'
    refine noWrite_sound hb ?_
    intro p q' hp hr hl
    by_cases hpv : p = v
    · subst hpv; simp only [bodyCtx, find, ↓reduceIte] at hp; cases hp; exact ⟨rfl, rfl⟩
    · rw [find_bodyCtx hpv] at hp
      cases hq : find Γ p with
      | none => rw [hq] at hp; cases hp
      | some q1 =>
        rw [hq] at hp; cases hp
        by_cases h1 : q1.root = q.root
        · by_cases h2 : q1.lock = .free <;> simp [lockFor, h1, h2] at hl
        · simp [lockFor, h1] at hr

/-! ### Examples

Place 0 is a mutable owned variable; views take fresh numbers. -/

def x0 : Ctx := [(0, { root := 0, mutable := true, owner := true, lock := .free })]

/-- `borrow mut x as v { v = …; } x = …;` is accepted: the view writes, and the root is usable
again after the block. -/
example : Ok x0 [.borrow true 0 1 [.write 1], .write 0] :=
  .borrow rfl (by decide) (fun _ => ⟨rfl, rfl⟩) rfl
    (.write rfl rfl rfl .nil) (.write rfl rfl rfl .nil)

/-- Shared opens nest, and the root stays readable under them (REF-11). -/
example : Ok x0 [.borrow false 0 1 [.borrow false 1 2 [.read 0, .read 2]]] :=
  .borrow rfl (by decide) (by intro h; cases h) rfl
    (.borrow rfl (by decide) (by intro h; cases h) rfl
      (.read rfl (by decide) (.read rfl (by decide) .nil)) .nil) .nil

/-- Reading the root inside its mutable open is refused. -/
example : ¬ Ok x0 [.borrow true 0 1 [.read 0]] := by
  intro h
  cases h with
  | borrow hs _ _ _ hb _ =>
    simp only [x0, find, ↓reduceIte, Option.some.injEq] at hs; subst hs
    cases hb with
    | read hf hl _ => simp [bodyCtx, find, mapPlaces, lockFor, x0, view] at hf; subst hf; simp at hl

/-- Writing the root inside a shared open is refused. -/
example : ¬ Ok x0 [.borrow false 0 1 [.write 0]] := by
  intro h
  cases h with
  | borrow hs _ _ _ hb _ =>
    simp only [x0, find, ↓reduceIte, Option.some.injEq] at hs; subst hs
    cases hb with
    | write hf hl _ _ => simp [bodyCtx, find, mapPlaces, lockFor, x0, view] at hf; subst hf; simp at hl

/-- Writing through a shared view is refused. -/
example : ¬ Ok x0 [.borrow false 0 1 [.write 1]] := by
  intro h
  cases h with
  | borrow hs _ _ _ hb _ =>
    simp only [x0, find, ↓reduceIte, Option.some.injEq] at hs; subst hs
    cases hb with
    | write hf _ hm _ => simp [bodyCtx, find, view] at hf; subst hf; simp at hm

/-- A shared view never upgrades to a mutable one. -/
example : ¬ Ok x0 [.borrow false 0 1 [.borrow true 1 2 []]] := by
  intro h
  cases h with
  | borrow hs _ _ _ hb _ =>
    simp only [x0, find, ↓reduceIte, Option.some.injEq] at hs; subst hs
    cases hb with
    | borrow hf _ hm _ _ _ => simp [bodyCtx, find, view] at hf; subst hf; have := (hm rfl).2; simp at this

/-- Moving the root inside its shared open is refused. -/
example : ¬ Ok x0 [.borrow false 0 1 [.move 0]] := by
  intro h
  cases h with
  | borrow hs _ _ _ hb _ =>
    simp only [x0, find, ↓reduceIte, Option.some.injEq] at hs; subst hs
    cases hb with
    | move hf hl _ _ => simp [bodyCtx, find, mapPlaces, lockFor, x0, view] at hf; subst hf; simp at hl

end ConcreteSpec.Borrow

/-! ## Part 5. Allocator brands (Q1.1–Q1.5, Q1.13): `reset_fresh`

An allocator's brand is an erased kind (Q1.1) with no runtime representation. Each `new` and
`reset` binder introduces a brand that differs from every other in scope (generative, Q1.2), and
brands are equal only when they have the same name (nominal, Q1.4). A heap value records the
brand of the allocator it came from, and operations on it require that allocator's brand.
Resources are consumed before their allocator (Q1.5): releasing or resetting an allocator
requires that no value of its brand is live. A loop that carries an allocator carries
`∃ β. alloc β` (Q1.3): each iteration, and the code after the loop, opens it with a fresh name.

Two views of the same programs: the static check (`Ok`), and a ghost run (`Run`) that gives
every allocator a run-time brand from a counter, checks brand equality on each operation, and is
erased by Q1.1. The proof that a checked program passes every ghost check (the erasure
correspondence) is not discharged here.
-/

namespace ConcreteSpec.Brand

def findL {α : Type} : List (Nat × α) → Nat → Option α
  | [], _ => none
  | (n, v) :: l, p => if n = p then some v else findL l p

def removeL {α : Type} (p : Nat) : List (Nat × α) → List (Nat × α)
  | [] => []
  | (n, v) :: l => if n = p then removeL p l else (n, v) :: removeL p l

theorem findL_mem {α : Type} : ∀ {l : List (Nat × α)} {p : Nat} {v : α},
    findL l p = some v → (p, v) ∈ l
  | [], _, _, h => by simp [findL] at h
  | (n, w) :: l, p, v, h => by
    simp only [findL] at h
    split at h
    · next hn => cases h; subst hn; exact List.mem_cons_self ..
    · exact List.mem_cons_of_mem _ (findL_mem h)

theorem mem_removeL {α : Type} (p : Nat) : ∀ {l : List (Nat × α)} {e : Nat × α},
    e ∈ removeL p l → e ∈ l
  | [], _, h => by simp [removeL] at h
  | (n, w) :: l, e, h => by
    simp only [removeL] at h
    split at h
    · exact List.mem_cons_of_mem _ (mem_removeL p h)
    · rcases List.mem_cons.mp h with rfl | h
      · exact List.mem_cons_self ..
      · exact List.mem_cons_of_mem _ (mem_removeL p h)

theorem findL_removeL {α : Type} (p' : Nat) : ∀ (l : List (Nat × α)) (p : Nat),
    findL (removeL p' l) p = if p = p' then none else findL l p
  | [], _ => by simp [removeL, findL]
  | (n, q) :: l, p => by
    have ih := findL_removeL p' l p
    by_cases hn : n = p' <;> by_cases hp : p = p' <;> by_cases hnp : n = p <;>
      simp_all [removeL, findL] <;> omega

/-! ### The static check -/

/-- Static types: an allocator of brand `β`, or a heap value allocated from brand `β`. Both are
linear. -/
inductive STy where
  | alloc (β : Nat)
  | heap (β : Nat)
  deriving DecidableEq, Repr

abbrev Ctx := List (Nat × STy)

/-- No value of brand `β` is live. -/
def NoHeap (Γ : Ctx) (β : Nat) : Prop := ∀ e ∈ Γ, e.2 ≠ .heap β

instance (Γ : Ctx) (β : Nat) : Decidable (NoHeap Γ β) := by unfold NoHeap; infer_instance

inductive Stmt where
  /-- `new a` with brand binder `β`. -/
  | newAlloc (a β : Nat)
  /-- Allocate `h` from `a`. -/
  | alloc (h a : Nat)
  /-- An operation on `h` through `a`. -/
  | use (h a : Nat)
  /-- Free `h` through `a`. -/
  | free (h a : Nat)
  /-- Release `a`. -/
  | release (a : Nat)
  /-- `a' = reset(a)`, with brand binder `β'` for `a'`. -/
  | reset (a' a β' : Nat)
  /-- A loop of `k` iterations carrying allocator `a`; `β` names its brand inside one
  iteration and `βx` after the loop. -/
  | loop (k a β βx : Nat) (body : List Stmt)

/-- The static check, from the brand names in use `B` and the context `Γ` to the context at the
end. A brand binder must be fresh in `B` (generative); brand equality is name equality
(nominal). -/
inductive Ok : List Nat → Ctx → List Stmt → Ctx → Prop where
  | nil {B Γ} : Ok B Γ [] Γ
  | newAlloc {B Γ a β rest Γ'} : findL Γ a = none → β ∉ B →
      Ok (β :: B) ((a, .alloc β) :: Γ) rest Γ' → Ok B Γ (.newAlloc a β :: rest) Γ'
  | alloc {B Γ h a β rest Γ'} : findL Γ a = some (.alloc β) → findL Γ h = none →
      Ok B ((h, .heap β) :: Γ) rest Γ' → Ok B Γ (.alloc h a :: rest) Γ'
  | use {B Γ h a β rest Γ'} : findL Γ h = some (.heap β) → findL Γ a = some (.alloc β) →
      Ok B Γ rest Γ' → Ok B Γ (.use h a :: rest) Γ'
  | free {B Γ h a β rest Γ'} : findL Γ h = some (.heap β) → findL Γ a = some (.alloc β) →
      Ok B (removeL h Γ) rest Γ' → Ok B Γ (.free h a :: rest) Γ'
  | release {B Γ a β rest Γ'} : findL Γ a = some (.alloc β) → NoHeap Γ β →
      Ok B (removeL a Γ) rest Γ' → Ok B Γ (.release a :: rest) Γ'
  | reset {B Γ a' a β β' rest Γ'} : findL Γ a = some (.alloc β) → NoHeap Γ β →
      findL (removeL a Γ) a' = none → β' ∉ B →
      Ok (β' :: B) ((a', .alloc β') :: removeL a Γ) rest Γ' → Ok B Γ (.reset a' a β' :: rest) Γ'
  | loop {B Γ k a β0 β βx β'' body rest Γ'} : findL Γ a = some (.alloc β0) → NoHeap Γ β0 →
      β ∉ B → βx ∉ B →
      Ok (β :: B) ((a, .alloc β) :: removeL a Γ) body ((a, .alloc β'') :: removeL a Γ) →
      Ok (βx :: B) ((a, .alloc βx) :: removeL a Γ) rest Γ' →
      Ok B Γ (.loop k a β βx body :: rest) Γ'

/-- Every brand in the context is a brand name in use. -/
def BrandsIn (B : List Nat) (Γ : Ctx) : Prop :=
  ∀ e ∈ Γ, ∀ β, (e.2 = .alloc β ∨ e.2 = .heap β) → β ∈ B

/-- **`reset_fresh`, static part** (Q1.2, Q1.4). The brand of a reset allocator differs from the
brand of every value live before the reset, so each operation of the new allocator on such a
value (`use`, `free`) is refused. -/
theorem reset_refuses_old {B Γ a' a β' rest Γ'} (hw : BrandsIn B Γ)
    (h : Ok B Γ (.reset a' a β' :: rest) Γ') {hv βh} (hh : findL Γ hv = some (.heap βh)) :
    βh ≠ β' ∧ ∀ r, rest = .use hv a' :: r ∨ rest = .free hv a' :: r → False := by
  cases h with
  | @reset _ _ _ _ β _ _ _ ha hno hfresh hb hrest =>
    have hβh : βh ∈ B := hw _ (findL_mem hh) βh (Or.inr rfl)
    have hne : βh ≠ β' := fun e => hb (e ▸ hβh)
    have hva : hv ≠ a := by
      intro e; subst e; rw [ha] at hh; cases hh
    have hold : findL (removeL a Γ) hv = some (.heap βh) := by
      rw [findL_removeL]; simp only [hva, ↓reduceIte]; exact hh
    have hva' : hv ≠ a' := by
      intro e; subst e; rw [hold] at hfresh; cases hfresh
    have hfind : findL ((a', STy.alloc β') :: removeL a Γ) hv = some (.heap βh) := by
      simp only [findL, show ¬ a' = hv from fun e => hva' e.symm, ↓reduceIte]; exact hold
    have hnew : findL ((a', STy.alloc β') :: removeL a Γ) a' = some (.alloc β') := by
      simp [findL]
    refine ⟨hne, fun r hr => ?_⟩
    rcases hr with rfl | rfl
    · cases hrest with
      | use h1 h2 _ => rw [hfind] at h1; rw [hnew] at h2; cases h1; cases h2; exact hne rfl
    · cases hrest with
      | free h1 h2 _ => rw [hfind] at h1; rw [hnew] at h2; cases h1; cases h2; exact hne rfl

/-! ### The ghost run -/

inductive Val where
  | allocV (b : Nat)
  | heapV (b : Nat)
  deriving DecidableEq, Repr

def brandOf : Val → Nat
  | .allocV b => b
  | .heapV b => b

/-- The ghost state: the live values, the next unused brand, and every brand generated so far in
order. -/
structure State where
  env : List (Nat × Val)
  next : Nat
  gen : List Nat
  deriving Repr

/-- Generate a brand for allocator `a`. -/
def State.bind (s : State) (a : Nat) (env : List (Nat × Val)) : State :=
  { env := (a, .allocV s.next) :: env, next := s.next + 1, gen := s.gen ++ [s.next] }

/-- The ghost run. Each operation on a heap value checks that the allocator has the value's
brand; without a matching rule the run is stuck. -/
inductive Run : State → List Stmt → State → Prop where
  | nil {s} : Run s [] s
  | newAlloc {s a β rest s'} : Run (s.bind a s.env) rest s' → Run s (.newAlloc a β :: rest) s'
  | alloc {s h a b rest s'} : findL s.env a = some (.allocV b) →
      Run { s with env := (h, .heapV b) :: s.env } rest s' → Run s (.alloc h a :: rest) s'
  | use {s h a b rest s'} : findL s.env h = some (.heapV b) → findL s.env a = some (.allocV b) →
      Run s rest s' → Run s (.use h a :: rest) s'
  | free {s h a b rest s'} : findL s.env h = some (.heapV b) → findL s.env a = some (.allocV b) →
      Run { s with env := removeL h s.env } rest s' → Run s (.free h a :: rest) s'
  | release {s a b rest s'} : findL s.env a = some (.allocV b) →
      Run { s with env := removeL a s.env } rest s' → Run s (.release a :: rest) s'
  | reset {s a' a b β' rest s'} : findL s.env a = some (.allocV b) →
      Run (s.bind a' (removeL a s.env)) rest s' → Run s (.reset a' a β' :: rest) s'
  | loopDone {s a β βx body rest s'} : Run s rest s' → Run s (.loop 0 a β βx body :: rest) s'
  | loopStep {s k a β βx body rest s'} : Run s (body ++ .loop k a β βx body :: rest) s' →
      Run s (.loop (k + 1) a β βx body :: rest) s'

/-- Every live brand and every generated brand is below the counter, and the generated brands
are pairwise distinct. -/
def Inv (s : State) : Prop :=
  (∀ e ∈ s.env, brandOf e.2 < s.next) ∧ (∀ b ∈ s.gen, b < s.next) ∧ s.gen.Nodup

theorem nodup_snoc : ∀ {l : List Nat} {x : Nat}, l.Nodup → x ∉ l → (l ++ [x]).Nodup
  | [], _, _, _ => by simp
  | y :: l, x, h, hx => by
    rw [List.nodup_cons] at h
    simp only [List.cons_append]
    rw [List.nodup_cons]
    refine ⟨?_, nodup_snoc h.2 (fun hm => hx (List.mem_cons_of_mem _ hm))⟩
    simp only [List.mem_append, List.mem_singleton, not_or]
    exact ⟨h.1, fun e => hx (by simp [e])⟩

theorem inv_bind {s : State} (a : Nat) {env : List (Nat × Val)} (hi : Inv s)
    (henv : ∀ e ∈ env, brandOf e.2 < s.next) : Inv (s.bind a env) := by
  obtain ⟨_, hg, hn⟩ := hi
  refine ⟨?_, ?_, ?_⟩
  · intro e he
    simp only [State.bind, List.mem_cons] at he ⊢
    rcases he with rfl | he
    · simp [brandOf]
    · have := henv e he; omega
  · intro b hb
    simp only [State.bind, List.mem_append, List.mem_singleton] at hb ⊢
    rcases hb with hb | rfl
    · have := hg b hb; omega
    · omega
  · exact nodup_snoc hn (fun hm => by have := hg _ hm; omega)

/-- The ghost run keeps the invariant. -/
theorem run_inv {s ss s'} (h : Run s ss s') : Inv s → Inv s' := by
  induction h with
  | nil => exact id
  | newAlloc _ ih => intro hi; exact ih (inv_bind _ hi hi.1)
  | alloc ha _ ih =>
    intro hi
    refine ih ⟨?_, hi.2.1, hi.2.2⟩
    intro e he
    rcases List.mem_cons.mp he with rfl | he
    · have := hi.1 _ (findL_mem ha); simpa [brandOf] using this
    · exact hi.1 e he
  | use _ _ _ ih => exact ih
  | free _ _ _ ih => intro hi; exact ih ⟨fun e he => hi.1 e (mem_removeL _ he), hi.2.1, hi.2.2⟩
  | release _ _ ih => intro hi; exact ih ⟨fun e he => hi.1 e (mem_removeL _ he), hi.2.1, hi.2.2⟩
  | reset _ _ ih => intro hi; exact ih (inv_bind _ hi (fun e he => hi.1 e (mem_removeL _ he)))
  | loopDone _ ih => exact ih
  | loopStep _ ih => exact ih

/-- The start of every run: nothing live, no brand generated. -/
def State.init : State := { env := [], next := 0, gen := [] }

theorem inv_init : Inv State.init := ⟨by simp [State.init], by simp [State.init], by simp [State.init]⟩

/-- **`reset_fresh`** (Q9.14, Q1.2, Q1.4, Q1.5).

1. The brand a reset gives the new allocator differs from the brand of every value live at the
   reset, in any reachable ghost state.
2. Every brand a run generates (each `new` and each `reset`, in every loop iteration) differs
   from every other: the generated brands are pairwise distinct.
3. The new allocator's operations refuse the old brand: statically (`reset_refuses_old`), and
   in the ghost run, where `use` or `free` of a value live before the reset through the new
   allocator is stuck. -/
theorem reset_fresh :
    (∀ {s a' a β' rest s'}, Inv s → Run s (.reset a' a β' :: rest) s' →
      ∃ mid, mid.env = (a', .allocV s.next) :: removeL a s.env ∧ Run mid rest s' ∧
        ∀ e ∈ s.env, brandOf e.2 ≠ s.next) ∧
    (∀ {prog s'}, Run State.init prog s' → s'.gen.Nodup) ∧
    (∀ {s a' a β' h r s'}, Inv s → (∃ v, findL s.env h = some v) → h ≠ a' →
      ¬ Run s (.reset a' a β' :: .use h a' :: r) s' ∧
      ¬ Run s (.reset a' a β' :: .free h a' :: r) s') := by
  refine ⟨?_, ?_, ?_⟩
  · intro s a' a β' rest s' hi h
    cases h with
    | reset _ hr =>
      exact ⟨_, rfl, hr, fun e he => Nat.ne_of_lt (hi.1 e he)⟩
  · intro prog s' h
    exact (run_inv h inv_init).2.2
  · intro s a' a β' h r s' hi ⟨v, hv⟩ hne
    have key : ∀ b, findL ((a', Val.allocV s.next) :: removeL a s.env) h = some (.heapV b) →
        b ≠ s.next := by
      intro b hb
      simp only [findL, show ¬ a' = h from fun e => hne e.symm, ↓reduceIte] at hb
      exact Nat.ne_of_lt (hi.1 _ (findL_mem (α := Val) (l := removeL a s.env) hb |> mem_removeL a))
    have hnew : findL ((a', Val.allocV s.next) :: removeL a s.env) a' = some (.allocV s.next) := by
      simp [findL]
    have _ := hv
    refine ⟨fun hrun => ?_, fun hrun => ?_⟩
    · cases hrun with
      | reset _ hr =>
        cases hr with
        | use h1 h2 _ =>
          simp only [State.bind] at h1 h2
          rw [hnew] at h2; cases h2; exact key _ h1 rfl
    · cases hrun with
      | reset _ hr =>
        cases hr with
        | free h1 h2 _ =>
          simp only [State.bind] at h1 h2
          rw [hnew] at h2; cases h2; exact key _ h1 rfl

/-! ### Examples -/

/-- `new a; h = alloc(a); reset(a)` is refused: `h` is still live (Q1.5). -/
example : ¬ Ok [] [] [.newAlloc 0 10, .alloc 1 0, .reset 0 0 11, .free 1 0, .release 0] [] := by
  intro hok
  cases hok with
  | newAlloc _ _ h1 =>
    cases h1 with
    | alloc ha' _ h2 =>
      simp [findL] at ha'; subst ha'
      cases h2 with
      | reset ha hno _ _ _ =>
        simp [findL] at ha; subst ha; exact hno (1, .heap 10) (by simp) rfl

/-- `new a; h = alloc(a); free(h); a = reset(a); h2 = alloc(a); free(h2); release(a)` is accepted. -/
example : Ok [] [] [.newAlloc 0 10, .alloc 1 0, .free 1 0, .reset 0 0 11, .alloc 2 0, .free 2 0,
    .release 0] [] :=
  .newAlloc rfl (by decide) <| .alloc rfl rfl <| .free rfl rfl <|
    .reset rfl (by decide) rfl (by decide) <| .alloc rfl rfl <| .free rfl rfl <|
    .release rfl (by decide) .nil

/-- A loop that resets its allocator each iteration is accepted (existential loop state, Q1.3). -/
example : Ok [] [] [.newAlloc 0 10, .loop 3 0 20 30 [.alloc 1 0, .free 1 0, .reset 0 0 21],
    .release 0] [] :=
  .newAlloc rfl (by decide) <|
    .loop (β'' := 21) rfl (by decide) (by decide) (by decide)
      (.alloc rfl rfl <| .free rfl rfl <| .reset rfl (by decide) rfl (by decide) .nil)
      (.release rfl (by decide) .nil)

/-- Three iterations of that loop generate four pairwise distinct brands: 0 for `new`, then 1, 2
and 3 for the resets. -/
example : Run State.init [.newAlloc 0 10, .loop 3 0 20 30 [.alloc 1 0, .free 1 0, .reset 0 0 21],
    .release 0] { env := [], next := 4, gen := [0, 1, 2, 3] } :=
  .newAlloc <| .loopStep <| .alloc rfl <| .free rfl rfl <| .reset rfl <|
    .loopStep <| .alloc rfl <| .free rfl rfl <| .reset rfl <|
    .loopStep <| .alloc rfl <| .free rfl rfl <| .reset rfl <|
    .loopDone <| .release rfl .nil

end ConcreteSpec.Brand
