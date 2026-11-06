/-!
# The Concrete specification

**PROPOSED: not normative until the owner freezes Core (ROADMAP A2c).** The owner provisionally
approved the Core design on 2026-11-05 (ROADMAP A2a; decision rows Q1.1–Q9.19 in
docs/spec/decisions.md). This file is the one main normative file (row Q9.15): every rule is here,
as Lean definitions with plain-English doc comments, each followed by example programs that it
accepts or refuses and what they do. It becomes normative when the owner freezes Core (A2c, row
Q9.19). Until then its examples are not conformance claims for any compiler, and any finding that
would change an approved row goes back to the owner.

How to read it. Read this file top to bottom; you never need the proofs to know what a rule says.
The proofs are in `ConcreteSpec/Proofs/`, one file per part, and say why the rules hang together
(an accepted program never gets stuck, the checker agrees with the rules, and the design theorems).
`ConcreteSpec/README.md` is the reading guide and lists what is not yet formalized.

The examples are decided by running the definitions: `check p = true` means the checker accepts
`p`, and `check_iff` (Proofs/Checker.lean) proves that this is exactly the rule `Checked p`;
`run p 10 = .exit 42` means `p` exits with 42.

The decisions are cited by their row ids (Q1.1 … Q9.19, P1 … P23) and by the recorded rule ids of
docs/spec (ODnn, TYPE-n, OWN-n, …).

The file has five parts. Each is a small Core fragment with its own judgments, defined only as far
as its design theorem needs (R4 F8). Unifying them into one Core is work for A3 (R-0560).

1. **Slice 1** (ROADMAP phase B): scalars, `let`, structs, moves and the use-after-move refusal,
   checked `+`, `return`, in Core's binding form (Q9.1), with the static refusal of statically
   known violations (Q6.1–Q6.6).
2. **Representation** (Q3.11): the layout and byte encoding of the slice-1 types.
3. **Structured cleanup** (Q9.8, Q2.1, Q2.7, Q1.5): `defer`, frames and exits.
4. **Scoped borrows** (Q3.3, Q3.4, Q9.5, REF-11).
5. **Allocator brands** (Q1.1–Q1.5, Q1.13).

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
  `move x` (its type is linear). There is no read whose effect depends on the type. One read form
  per type (owner-confirmed 2026-11-05): `copy` is allowed only on a Copy type and `move` only on a
  linear one; `move` of a Copy value and `copy` of a linear value are both refused.

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

Statically known values (Q6.2, as amended by the owner on 2026-11-05). The checker tracks an
abstract value for each binding: unknown, a known scalar, or a struct whose fields each carry
their own fact. A value is known when it comes from the fixed table: a literal, an immutable `let`
of a known value, or a checked operation on known operands. Constructing a struct records each
field's fact independently; an immutable binding keeps the facts; destructuring hands each field's
fact to its binder; an unknown field stays unknown without discarding its known siblings; and an
ownership transfer (`move`) keeps the facts for the receiving binding while still consuming the
source. So `let Pair { a: x, b: y } = Pair { a: 200, b: 100 }; x + y` is refused exactly as
`let a = 200; let b = 100; a + b` is (`fieldOverflowTrap`, `overflowTrap`). The exclusions stay:
nothing is tracked through mutable state (slice 1 has none), along branches or paths, through
user-function calls, or through calls the table does not name. Slice 1 judges Core, where every
`let` is immutable and a binding's origin is not recorded, so a temporary the elaborator introduces
gets the same fact as a source binding of the same expression. The remaining exclusion, that
acceptance of a source program does not depend on how the elaborator names its subexpressions, is
an obligation on the source-to-Core elaboration relation, which slice 1 does not define; it is not
proved here. In slice 1 struct fields are scalars, so the
rule's recursive form (a struct field holding a struct, nested patterns, tuples) has only its
one-level instance here. An input is not known: `add100` and `partialUnknownAccepted` are the
accepted bodies that trap for some input values. The table has more entries (module constants, the
six named wrapping/saturating operations, lossless `as`) that slice 1 has no syntax for. The
refusal applies to all written code, including trusted and unreachable code (Q6.4); slice 1 has
neither, so its examples show only reachable positions and a discarded one (`overflowDropped`).
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

/-! ### Types -/

/-- The integer types (TYPE-19): signed `i8`…`i64` and unsigned `u8`…`u64`, each as many bits wide
as its name says. -/
inductive IntTy where
  | i8 | i16 | i32 | i64 | u8 | u16 | u32 | u64
  deriving DecidableEq, Repr

end ConcreteSpec

namespace ConcreteSpec.IntTy

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

end ConcreteSpec.IntTy

namespace ConcreteSpec

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

/-! ## Part 1. Slice 1

The slice-1 rules: the syntax, the typing and ownership judgments, the evaluator, value typing and
the checker. The examples follow in `ConcreteSpec.Examples`.

### Syntax -/

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

/-! ### Value typing

What it means for a run-time value to have a type. The safety theorems (`checked_never_stuck`,
`checked_exits_or_traps`, in `Proofs/Slice1.lean`) are stated with it. -/

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

/-! ### The checker

`check` is an executable checker. It is not the specification: the judgments above are.
`check_iff` (`Proofs/Checker.lean`) proves the two agree, which is what lets the examples below be
decided by running `check`. -/

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

/-- The executable form of `FieldsTy`. -/
def fieldsOkB (Γ : TyCtx) (d : StructDecl) (fs : List (Name × Atom)) : Bool :=
  decide ((fs.map Prod.fst).Nodup) &&
  fs.all (fun p => match lookupF p.1 d.fields with
    | some st => atomTy Γ p.2 == some st
    | none => false) &&
  d.fields.all (fun q => (lookupF q.1 fs).isSome)

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

/-- The executable form of `PatOk`. -/
def patOkB (d : StructDecl) (pat : List (Name × Binder)) : Bool :=
  decide ((pat.map Prod.fst).Nodup) &&
  pat.all (fun p => (lookupF p.1 d.fields).isSome) &&
  d.fields.all (fun q => (lookupF q.1 pat).isSome)

/-- The context after a statement, if the statement is accepted. -/
def stmtOut (Δ : Decls) (Γ : Ctx) : Stmt → Option Ctx
  | .let_ (.name x) t e =>
    if tyOf Δ Γ.tys e = some t ∧ knownViolates Γ e = false then
      match uses Δ Γ e with
      | some Γ₁ => if Fresh Γ₁ x then
          some ({ name := x, ty := t, live := true, used := false, known := knownExpr Γ e,
                  kfields := knownFields Γ e } :: Γ₁)
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
          | some Γ₁ => if PatFresh Γ₁ pat then some (bindPat d (knownFields Γ e) pat Γ₁) else none
          | none => none
        else none
      | none => none
    else none

/-- The executable form of `BodyOk`. -/
def bodyOkB (Δ : Decls) (Γ : Ctx) : List Stmt → Atom → Bool
  | [], r => tyOf Δ Γ.tys (.atom r) == some (.scalar (.int .u8)) &&
      match uses Δ Γ (.atom r) with
      | some Γ₁ => decide (Discharged Δ Γ₁)
      | none => false
  | s :: ss, r => match stmtOut Δ Γ s with
    | some Γ₁ => bodyOkB Δ Γ₁ ss r
    | none => false

/-- The executable checker. -/
def check (p : Program) : Bool := decide (DeclsOk p.decls) && bodyOkB p.decls [] p.body p.ret

end ConcreteSpec

namespace ConcreteSpec.Examples
open ConcreteSpec

/-! ### Slice-1 conformance examples

Each example gives the surface program in its doc comment, its Core syntax, and what the
specification says about it: accepted (`check p = true`) with an outcome (`run p 10 = …`), or
refused (`check p = false`). By `check_iff` (`Proofs/Checker.lean`), `check p = true` is exactly
the rule `Checked p`, and `bodyOkB … = true` is exactly `BodyOk …`. Locals are written with their
source name and a number: `t₀` is `⟨"t", 0⟩`. The examples that need a theorem (`add100_safe`, the
accepted runtime overflow from an input) are in `Proofs/Examples.lean`. -/

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

def z₃ : Var := ⟨"z", 3⟩

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

example : check moveThenConsume = true := by decide

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

example : check useAfterMove = false := by decide

/-- The refusal is what keeps the program from getting stuck: run anyway, the model reads a moved
slot. This is the behaviour `checked_no_use_after_move` rules out for accepted programs. -/
example : run useAfterMove 10 = .stuck .movedRead := by decide

/-- Refused: a linear value read with `copy` (Q9.1: a linear local is only moved). Run anyway, the
read is in the wrong mode. -/
def copyLinear : Program :=
  { decls, body := [.let_ (.name t₀) (.struct "Token") (.mk "Token" []),
                    .letStruct "Token" [] (cp t₀)],
    ret := .int .u8 42 }

example : check copyLinear = false := by decide

example : run copyLinear 10 = .stuck .wrongMode := by decide

/-- Refused: a Copy value read with `move` (Q9.1, owner-confirmed 2026-11-05: `move` only for
non-Copy types). Run anyway, the read is in the wrong mode. -/
def moveCopy : Program :=
  { decls, body := [.let_ (.name x₀) u8 (lit 7)],
    ret := .move x₀ }

example : check moveCopy = false := by decide

example : run moveCopy 10 = .stuck .wrongMode := by decide

/-- Refused: the linear `t` is never consumed before `return` (OWN-2).
```
fn main() -> u8 {
    let t: Token = Token {};
    return 42;             // error: t is not consumed
}
``` -/
def notConsumed : Program :=
  { decls, body := [.let_ (.name t₀) (.struct "Token") (.mk "Token" [])], ret := .int .u8 42 }

example : check notConsumed = false := by decide

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

example : check copyTwice = true := by decide

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

example : check unusedBinding = false := by decide

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

example : check destructureAdd = true := by decide

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

example : check missingField = false := by decide

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

example : check wildcardField = true := by decide

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

example : check overflowTrap = false := by decide

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

example : check overflowDropped = false := by decide

/-- Refused (Q6.2 as amended 2026-11-05, Q6.4; OD118): known values flow through an immutable
struct. Constructing `p` records that field `a` is `200` and field `b` is `100`; destructuring
hands those facts to `x` and `y`, so `x + y` is known to overflow `u8`, exactly as for two plain
`let`s (`overflowTrap`). The refusal is justified: run anyway, the addition traps.
```
fn main() -> u8 {
    let p: Pair = Pair { a: 200, b: 100 };
    let Pair { a: x, b: y } = p;
    return x + y;          // error: 200 + 100 overflows u8
}
``` -/
def fieldOverflowTrap : Program :=
  { decls, body := [.let_ (.name p₀) (.struct "Pair") (.mk "Pair" [("a", .int .u8 200), ("b", .int .u8 100)]),
                    .letStruct "Pair" [("a", .name x₁), ("b", .name y₁)] (mv p₀),
                    .let_ (.name s₂) u8 (.add (.copy x₁) (.copy y₁))],
    ret := .copy s₂ }

example : check fieldOverflowTrap = false := by decide

example : run fieldOverflowTrap 10 = .trap .arithmeticOverflow := by decide

/-- Refused: an ownership transfer keeps the facts for the receiving binding (Q6.2 as amended).
`p` is moved into `q`, which may no longer read `p` (OWN-1), and `q` carries `p`'s field facts.
```
fn main() -> u8 {
    let p: Pair = Pair { a: 200, b: 100 };
    let q: Pair = p;
    let Pair { a: x, b: y } = q;
    return x + y;          // error: 200 + 100 overflows u8
}
``` -/
def fieldOverflowMoved : Program :=
  { decls, body := [.let_ (.name p₀) (.struct "Pair") (.mk "Pair" [("a", .int .u8 200), ("b", .int .u8 100)]),
                    .let_ (.name q₁) (.struct "Pair") (mv p₀),
                    .letStruct "Pair" [("a", .name x₁), ("b", .name y₁)] (mv q₁),
                    .let_ (.name s₂) u8 (.add (.copy x₁) (.copy y₁))],
    ret := .copy s₂ }

example : check fieldOverflowMoved = false := by decide

example : run fieldOverflowMoved 10 = .trap .arithmeticOverflow := by decide

/-- Accepted with the facts in range: the known sum fits, and the program exits with it. The
known field values make no program with a fitting sum fail to check. -/
def fieldSumFits : Program :=
  { decls, body := [.let_ (.name p₀) (.struct "Pair") (.mk "Pair" [("a", .int .u8 200), ("b", .int .u8 55)]),
                    .letStruct "Pair" [("a", .name x₁), ("b", .name y₁)] (mv p₀),
                    .let_ (.name s₂) u8 (.add (.copy x₁) (.copy y₁))],
    ret := .copy s₂ }

example : check fieldSumFits = true := by decide

example : run fieldSumFits 10 = .exit 255 := by decide

/-- **An accepted overflow from an input** (Q6.2, Q6.4). The checker does not know an input's
value. Slice 1 has no source of input: `main` takes no parameters (Q8.4), and an input reaches a
program through a capability (Q5), which slice 1 does not model. The witness is therefore a body
checked from one binding `x : u8` with no known value, as the body of a function with a `u8`
parameter would be:
```
fn add100(x: u8) -> u8 {
    return x + 100;        // accepted; traps when x > 155
}
```
In Core the sum is bound to a temporary first (Q9.1). -/
def inputCtx : Ctx := [{ name := x₀, ty := u8, live := true, used := false, known := none }]

def inputEnv (n : Int) : Env := [(x₀, .val (.scalar (.int .u8 n)))]

def add100 : List Stmt := [.let_ (.name s₂) u8 (.add (.copy x₀) (.int .u8 100))]

example : runBody decls 10 (inputEnv 200) add100 (.copy s₂) = .trap .arithmeticOverflow := by decide

example : runBody decls 10 (inputEnv 1) add100 (.copy s₂) = .exit 101 := by decide

/-- **Partial facts** (Q6.2 as amended): an unknown field does not discard a known sibling. `p`'s
field `a` comes from the input `x` and is unknown; field `b` is `200` and is known. Adding `100` to
the known field is refused; the body that adds the unknown field to the known one is accepted, and
traps only for some inputs.
```
fn f(x: u8) -> u8 {
    let p: Pair = Pair { a: x, b: 200 };
    let Pair { a: c, b: d } = p;
    let s: u8 = d + 100;     // error: 200 + 100 overflows u8
    let z: u8 = c + s;
    return z;
}
``` -/
def partialKnownRefused : List Stmt :=
  [.let_ (.name p₀) (.struct "Pair") (.mk "Pair" [("a", .copy x₀), ("b", .int .u8 200)]),
   .letStruct "Pair" [("a", .name x₁), ("b", .name y₁)] (mv p₀),
   .let_ (.name s₂) u8 (.add (.copy y₁) (.int .u8 100)),
   .let_ (.name z₃) u8 (.add (.copy x₁) (.copy s₂))]

example : bodyOkB decls inputCtx partialKnownRefused (.copy z₃) = false := by decide

/-- Accepted: the sum of the unknown field and the known one is not known, so it is checked at run
time (ARITH-3).
```
fn f(x: u8) -> u8 {
    let p: Pair = Pair { a: x, b: 200 };
    let Pair { a: c, b: d } = p;
    return c + d;          // accepted; traps when x > 55
}
``` -/
def partialUnknownAccepted : List Stmt :=
  [.let_ (.name p₀) (.struct "Pair") (.mk "Pair" [("a", .copy x₀), ("b", .int .u8 200)]),
   .letStruct "Pair" [("a", .name x₁), ("b", .name y₁)] (mv p₀),
   .let_ (.name s₂) u8 (.add (.copy x₁) (.copy y₁))]

example : bodyOkB decls inputCtx partialUnknownAccepted (.copy s₂) = true := by decide

example : runBody decls 10 (inputEnv 55) partialUnknownAccepted (.copy s₂) = .exit 255 := by decide

example : runBody decls 10 (inputEnv 56) partialUnknownAccepted (.copy s₂) = .trap .arithmeticOverflow := by
  decide

/-- Refused: `300` does not fit `u8` (ARITH-2). -/
def literalTooBig : Program :=
  { decls, body := [.let_ (.name a₀) u8 (lit 300)], ret := .copy a₀ }

example : check literalTooBig = false := by decide

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

example : check mixedAdd = false := by decide

/-- Refused: one identity bound twice (Q9.1: every Core binder is fresh). Elaboration gives two
surface bindings of one name different identities, so this arises only from a faulty elaborator;
the surface rule against rebinding (SCOPE-1) is an elaboration rule. -/
def rebind : Program :=
  { decls, body := [.let_ (.name x₀) u8 (lit 1),
                    .let_ (.name x₀) u8 (lit 41)],
    ret := .copy x₀ }

example : check rebind = false := by decide

/-- Accepted: `_` drops a Copy value (OWN-5).
```
fn main() -> u8 {
    let _: u8 = 1;
    return 42;
}
``` -/
def wildCopy : Program :=
  { decls, body := [.let_ .wild u8 (lit 1)], ret := .int .u8 42 }

example : check wildCopy = true := by decide

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

example : check wildLinear = false := by decide

/-- Refused: `main` returns `u8` (STMT-7). -/
def wrongReturnType : Program :=
  { decls, body := [], ret := .int .i64 42 }

example : check wrongReturnType = false := by decide

end ConcreteSpec.Examples

namespace ConcreteSpec.Rep
open ConcreteSpec

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

/-- Non-vacuity: decoding is not total. An uninitialized byte is no `u8`, byte `2` is no `bool`,
and a byte count other than the size is no value. -/
example : decodeScalar (.int .u8) [.undef] = none := by decide

example : decodeScalar .bool [.byte 2] = none := by decide

example : decodeScalar (.int .u16) [.byte 1] = none := by decide

/-- Little-endian, two's complement (OD126): `-2 : i16` is the bytes `0xfe 0xff`. -/
example : encodeScalar (.int .i16 (-2)) = [.byte 254, .byte 255] := by decide

/-- Round trips at the ends of each unsigned range, for both `bool` values and for the empty
struct `Token` (zero bytes, F13). -/
example : decodeVal [] (.scalar (.int .u8)) (encodeVal (.scalar (.int .u8 255))) =
    some (.scalar (.int .u8 255)) := by decide

example : decodeVal [] (.scalar (.int .u16)) (encodeVal (.scalar (.int .u16 65535))) =
    some (.scalar (.int .u16 65535)) := by decide

example : decodeVal [] (.scalar (.int .u32)) (encodeVal (.scalar (.int .u32 4294967295))) =
    some (.scalar (.int .u32 4294967295)) := by decide

example : decodeVal [] (.scalar (.int .u64)) (encodeVal (.scalar (.int .u64 18446744073709551615))) =
    some (.scalar (.int .u64 18446744073709551615)) := by decide

example : decodeVal [] (.scalar (.int .u64)) (encodeVal (.scalar (.int .u64 0))) =
    some (.scalar (.int .u64 0)) := by decide

example : decodeVal [] (.scalar .bool) (encodeVal (.scalar (.bool true))) =
    some (.scalar (.bool true)) := by decide

example : decodeVal [] (.scalar .bool) (encodeVal (.scalar (.bool false))) =
    some (.scalar (.bool false)) := by decide

example : encodeVal (.struct "Token" []) = [] ∧
    decodeVal Examples.decls (.struct "Token") [] = some (.struct "Token" []) := by decide

end ConcreteSpec.Rep

namespace ConcreteSpec.Cleanup

/-! ## Part 3. Structured cleanup (Q9.8): `defer_cleanup_releases`

Core keeps `defer` as nested deferred scopes (Q9.8). A block registers Unit-only cleanup calls
(`destroy(x)` for a destroyable value, `give_up(x)` for resource cleanup; Q2.1, Q2.7); each call
may hold loans, released after the call. On a structured exit (falling off the end of a block,
`break L`, `continue L`, `return`) the registered calls of every scope the exit leaves run in
reverse registration order, innermost scope first. An abort runs no cleanup, and a cleanup call
that traps stops the run (Q9.8). A value's cleanup must run before the cleanup of an allocator it
depends on (Q1.5, Q1.3: resources are consumed before their allocator); registration checks this.

The model is the cleanup stack itself: frames of registered calls and the exits that unwind them.
What a deferred call does is abstracted to its identity, the resource it consumes, the resources
it needs (allocator dependencies and the sources of its loans), its loans, and whether it traps
(`ok`). Registration tracks which resources are still owned: a call may consume only an owned
resource, each resource at most once, and may need only resources that are owned or consumed by
an earlier-registered (so later-running) call. Capability checking of the calls (Q5, Q2.7) is not
modelled here. Not modelled here: the statements between registrations, late reads of the
deferred arguments, and evaluating a `return` value into an owned temporary before cleanup (Q9.8).
-/

/-- A resource in this part: a value to destroy or give up, or an allocator. -/
abbrev Res := Nat

/-- A loan held by a deferred call, and the resource it borrows from. -/
structure Loan where
  id : Nat
  src : Res
  deriving DecidableEq, Repr

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

/-- The resources a call needs to stay unconsumed until it and its loan releases have run: the
allocators its resource depends on and the sources of its loans. -/
def Deferred.needs (d : Deferred) : List Res := d.deps ++ d.loans.map Loan.src

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

/-- The dependency order of a run list (Q1.5): no call consumes a resource that a later call
needs, and no call needs its own resource. -/
inductive DepOrder : List Deferred → Prop where
  | nil : DepOrder []
  | cons {d ds} : d.target ∉ d.needs → (∀ d' ∈ ds, d.target ∉ d'.needs) → DepOrder ds →
      DepOrder (d :: ds)

/-- The resources the calls consume. -/
def targets (ds : List Deferred) : List Res := ds.map Deferred.target

/-- How a block builds its cleanup stack, starting from the resources it owns (`live`). Entering
a scope pushes an empty frame. `defer` adds a call to the innermost frame, provided: the call
consumes a resource still owned and not yet scheduled (so no resource is consumed twice); it does
not need its own resource; everything it needs is owned or consumed by an earlier-registered call
(which runs later); and no call already registered, in any enclosing scope, needs the resource it
consumes (Q1.5: a value is cleaned up before its allocator, and a loan's source outlives the
loan). The resource is then no longer owned. Leaving a scope removes its frame after its cleanup
ran. -/
inductive Reg : (Res → Bool) → Stack → Prop where
  | empty {live} : Reg live []
  | enter {live st k} : Reg live st → Reg live (⟨k, []⟩ :: st)
  | push {live k ds st d} : Reg live (⟨k, ds⟩ :: st) → live d.target = true →
      d.target ∉ d.needs →
      (∀ r ∈ d.needs, live r = true ∨ r ∈ targets (pending (⟨k, ds⟩ :: st))) →
      (∀ d' ∈ pending (⟨k, ds⟩ :: st), d.target ∉ d'.needs) →
      Reg (fun r => live r && r != d.target) (⟨k, d :: ds⟩ :: st)
  | leave {live f st} : Reg live (f :: st) → Reg live st

/-! ### Examples

A function owns allocator `a` (resource 0), a value `v` (resource 1) allocated from `a`, and a
buffer `b` (resource 2). The body registers `defer give_up(a)`; then, inside loop `L1`, `defer
destroy(v)`, a call that holds loan 7 borrowed from `b`; then, in a block inside the loop,
`defer destroy(b)`. -/

def relA : Deferred := { kind := .giveUp, target := 0, deps := [], loans := [] }

def desV : Deferred := { kind := .destroy, target := 1, deps := [0], loans := [⟨7, 2⟩] }

def desB : Deferred := { kind := .destroy, target := 2, deps := [], loans := [] }

def owned : Res → Bool := fun r => decide (r ∈ [0, 1, 2])

/-- `desB` in the inner block would consume `b` while `desV`'s loan from `b` is still held after
`desV` runs: registration refuses it. -/
example : ¬ (∀ d' ∈ pending [⟨.loopBody 1, [desV]⟩, ⟨.fnBody, [relA]⟩], desB.target ∉ d'.needs) := by
  decide

/-- Registering `destroy(b)` in the function body before the loop is accepted instead. -/
def stack : Stack := [⟨.block, []⟩, ⟨.loopBody 1, [desV]⟩, ⟨.fnBody, [desB, relA]⟩]

/-- What the function still owns afterwards: nothing of the three. -/
def ownedAfter : Res → Bool := fun r => ((owned r && r != 0) && r != 2) && r != 1

/-- Registering `give_up(a)` after `destroy(v)` is refused: `a` would be released first. -/
example : ¬ (∀ d' ∈ pending [⟨.fnBody, [desV]⟩], relA.target ∉ d'.needs) := by decide

/-- Falling off the inner block runs nothing; `break L1` and `continue L1` run `destroy(v)` and
release loan 7; `return` then also runs `destroy(b)` and `give_up(a)`, in that order. -/
example : (unwound .fallthrough stack).map pending = some [] := rfl

example : (unwound (.brk 1) stack).map (expected ∘ pending) =
    some [.call desV, .release ⟨7, 2⟩] := rfl

example : (unwound (.cont 1) stack).map (expected ∘ pending) =
    some [.call desV, .release ⟨7, 2⟩] := rfl

example : (unwound .ret stack).map (expected ∘ pending) =
    some [.call desV, .release ⟨7, 2⟩, .call desB, .call relA] := rfl

/-- Nested labelled loops: inside loop `L1`, loop `L2` registers `destroy(d)` (resource 4), and
a block inside it registers `destroy(c)` (resource 3). -/
def desC : Deferred := { kind := .destroy, target := 3, deps := [], loans := [] }

def desD : Deferred := { kind := .destroy, target := 4, deps := [], loans := [] }

def owned2 : Res → Bool := fun r => decide (r ∈ [0, 1, 2, 3, 4])

def ownedAfter2 : Res → Bool :=
  fun r => ((((owned2 r && r != 0) && r != 2) && r != 1) && r != 4) && r != 3

def stack2 : Stack :=
  [⟨.block, [desC]⟩, ⟨.loopBody 2, [desD]⟩, ⟨.loopBody 1, [desV]⟩, ⟨.fnBody, [desB, relA]⟩]

end ConcreteSpec.Cleanup

namespace ConcreteSpec.Borrow

/-! ## Part 4. Scoped borrows (Q3.3, Q3.4, Q9.5, REF-11): `open_mut_freezes_root`

Core's one borrow form, `borrow [mut] src as v { body }` (Q9.5; the range clause and `else` of
Q4.7 add a validity test before the body and do not change the access rules modelled here).
Every place records the owner root it comes from (Q3.3: views keep the original owner root), whether
it may be written, whether it owns its value, and how the enclosing borrows lock it. A shared open
locks every place of the root against writes and moves; a mutable open freezes every place of the
root, the root included, for the whole block (REF-11). Two accesses conflict only when one of
them is exclusive, so shared opens nest. The view binder is fresh and second-class. The check
threads the places through the statements: a move inside a block stays a move after it, and the
locks of the block end with it.
-/

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

/-- The outer places after a borrow block: those the block did not move, with the locks they
had before it. The view goes out of scope with the block. -/
def restore (Γ Γb : Ctx) : Ctx := Γ.filter fun e => (find Γb e.1).isSome

/-- The access rules, from the places before a statement list to the places after it. Read: not
frozen. Write: unlocked and writable. Move: unlocked and owner; the place is gone afterwards.
Shared open: the source is not frozen. Mutable open: the source is unlocked and writable (a shared
view never upgrades, Q3.3). The view name is fresh. -/
inductive Ok : Ctx → List Stmt → Ctx → Prop where
  | nil {Γ} : Ok Γ [] Γ
  | read {Γ p q rest Γ'} : find Γ p = some q → q.lock ≠ .excl → Ok Γ rest Γ' →
      Ok Γ (.read p :: rest) Γ'
  | write {Γ p q rest Γ'} : find Γ p = some q → q.lock = .free → q.mutable = true →
      Ok Γ rest Γ' → Ok Γ (.write p :: rest) Γ'
  | move {Γ p q rest Γ'} : find Γ p = some q → q.lock = .free → q.owner = true →
      Ok (remove p Γ) rest Γ' → Ok Γ (.move p :: rest) Γ'
  | borrow {Γ m s v q body rest Γb Γ'} : find Γ s = some q → q.lock ≠ .excl →
      (m = true → q.lock = .free ∧ q.mutable = true) → find Γ v = none →
      Ok (bodyCtx m v q Γ) body Γb → Ok (restore Γ Γb) rest Γ' →
      Ok Γ (.borrow m s v body :: rest) Γ'

/-- A statement list names place `p`, at any depth. -/
inductive Touches (p : Nat) : List Stmt → Prop where
  | read {rest} : Touches p (.read p :: rest)
  | write {rest} : Touches p (.write p :: rest)
  | move {rest} : Touches p (.move p :: rest)
  | src {m v body rest} : Touches p (.borrow m p v body :: rest)
  | body {m s v body rest} : Touches p body → Touches p (.borrow m s v body :: rest)
  | tail {s rest} : Touches p rest → Touches p (s :: rest)

/-- A statement list writes root `r`, or grants a write to it, at any depth: a write or move of a
place of `r`, or a mutable open of one, with places resolved as the checker resolves them. -/
inductive WritesRoot (r : Nat) : Ctx → List Stmt → Prop where
  | write {Γ p q rest} : find Γ p = some q → q.root = r → WritesRoot r Γ (.write p :: rest)
  | move {Γ p q rest} : find Γ p = some q → q.root = r → WritesRoot r Γ (.move p :: rest)
  | mutOpen {Γ s q v body rest} : find Γ s = some q → q.root = r →
      WritesRoot r Γ (.borrow true s v body :: rest)
  | body {Γ m s q v body rest} : find Γ s = some q → WritesRoot r (bodyCtx m v q Γ) body →
      WritesRoot r Γ (.borrow m s v body :: rest)
  | tail {Γ Γ₁ s rest} : Ok Γ [s] Γ₁ → WritesRoot r Γ₁ rest → WritesRoot r Γ (s :: rest)

/-! ### Examples

Place 0 is a mutable owned variable; views take fresh numbers. -/

def x0 : Ctx := [(0, { root := 0, mutable := true, owner := true, lock := .free })]

/-- `x0` and a second owned variable, place 5. -/
def xy : Ctx := (5, { root := 5, mutable := true, owner := true, lock := .free }) :: x0

/-- `borrow mut x as v { v = …; } x = …;` is accepted: the view writes, and the root is usable
again after the block. -/
example : ∃ Γ', Ok x0 [.borrow true 0 1 [.write 1], .write 0] Γ' :=
  ⟨_, .borrow rfl (by decide) (fun _ => ⟨rfl, rfl⟩) rfl
    (.write rfl rfl rfl .nil) (.write rfl rfl rfl .nil)⟩

/-- Shared opens nest, and the root stays readable under them (REF-11). -/
example : ∃ Γ', Ok x0 [.borrow false 0 1 [.borrow false 1 2 [.read 0, .read 2]]] Γ' :=
  ⟨_, .borrow rfl (by decide) (by intro h; cases h) rfl
    (.borrow rfl (by decide) (by intro h; cases h) rfl
      (.read rfl (by decide) (.read rfl (by decide) .nil)) .nil) .nil⟩

/-- Moving another owner inside a block is accepted, and stays a move after the block:
`borrow mut x as v { consume(y); } read(y)` is refused. -/
example : ∃ Γ', Ok xy [.borrow true 0 1 [.move 5], .write 0] Γ' :=
  ⟨_, .borrow rfl (by decide) (fun _ => ⟨rfl, rfl⟩) rfl
    (.move rfl rfl rfl .nil) (.write rfl rfl rfl .nil)⟩

example : ¬ ∃ Γ', Ok xy [.borrow true 0 1 [.move 5], .read 5] Γ' := by
  rintro ⟨Γ', h⟩
  cases h with
  | borrow hs _ _ _ hb hr =>
    simp [xy, x0, find] at hs; subst hs
    cases hb with
    | move _ _ _ h0 =>
      cases h0
      cases hr with
      | read hf _ _ => simp [restore, remove, bodyCtx, mapPlaces, lockFor, view, find, xy, x0] at hf

/-- Reading the root inside its mutable open is refused. -/
example : ¬ ∃ Γ', Ok x0 [.borrow true 0 1 [.read 0]] Γ' := by
  rintro ⟨Γ', h⟩
  cases h with
  | borrow hs _ _ _ hb _ =>
    simp only [x0, find, ↓reduceIte, Option.some.injEq] at hs; subst hs
    cases hb with
    | read hf hl _ => simp [bodyCtx, find, mapPlaces, lockFor, x0, view] at hf; subst hf; simp at hl

/-- Writing the root inside a shared open is refused. -/
example : ¬ ∃ Γ', Ok x0 [.borrow false 0 1 [.write 0]] Γ' := by
  rintro ⟨Γ', h⟩
  cases h with
  | borrow hs _ _ _ hb _ =>
    simp only [x0, find, ↓reduceIte, Option.some.injEq] at hs; subst hs
    cases hb with
    | write hf hl _ _ => simp [bodyCtx, find, mapPlaces, lockFor, x0, view] at hf; subst hf; simp at hl

/-- Writing through a shared view is refused. -/
example : ¬ ∃ Γ', Ok x0 [.borrow false 0 1 [.write 1]] Γ' := by
  rintro ⟨Γ', h⟩
  cases h with
  | borrow hs _ _ _ hb _ =>
    simp only [x0, find, ↓reduceIte, Option.some.injEq] at hs; subst hs
    cases hb with
    | write hf _ hm _ => simp [bodyCtx, find, view] at hf; subst hf; simp at hm

/-- A shared view never upgrades to a mutable one. -/
example : ¬ ∃ Γ', Ok x0 [.borrow false 0 1 [.borrow true 1 2 []]] Γ' := by
  rintro ⟨Γ', h⟩
  cases h with
  | borrow hs _ _ _ hb _ =>
    simp only [x0, find, ↓reduceIte, Option.some.injEq] at hs; subst hs
    cases hb with
    | borrow hf _ hm _ _ _ => simp [bodyCtx, find, view] at hf; subst hf; have := (hm rfl).2; simp at this

/-- Moving the root inside its shared open is refused. -/
example : ¬ ∃ Γ', Ok x0 [.borrow false 0 1 [.move 0]] Γ' := by
  rintro ⟨Γ', h⟩
  cases h with
  | borrow hs _ _ _ hb _ =>
    simp only [x0, find, ↓reduceIte, Option.some.injEq] at hs; subst hs
    cases hb with
    | move hf hl _ _ => simp [bodyCtx, find, mapPlaces, lockFor, x0, view] at hf; subst hf; simp at hl

end ConcreteSpec.Borrow

namespace ConcreteSpec.Brand

/-! ## Part 5. Allocator brands (Q1.1–Q1.5, Q1.13): `reset_fresh`

An allocator's brand is an erased kind (Q1.1) with no runtime representation. Each `new` and
`reset` binder introduces a brand that differs from every other in scope (generative, Q1.2), and
brands are equal only when they have the same name (nominal, Q1.4). A heap value records the
brand of the allocator it came from, and operations on it require that allocator's brand.
Resources are consumed before their allocator (Q1.5): releasing or resetting an allocator
requires that no value of its brand is live. A `while` loop (Q9.3, Core's one loop) that carries
an allocator carries `∃ β. alloc β` (Q1.3): each iteration, and the code after the loop, opens it
with a fresh name. The loop's condition region is abstracted: the ghost run may stop or iterate
at each test, so the results hold for every outcome of the conditions. Capability checking of the
allocator operations (Q5, Q2.7) is not modelled here.

Two views of the same programs: the static check (`Ok`), and a ghost run (`Run`) that gives
every allocator a run-time brand from a counter, checks brand equality on each operation, and is
erased by Q1.1. The proof that a checked program passes every ghost check (the erasure
correspondence) is not discharged here.
-/

def findL {α : Type} : List (Nat × α) → Nat → Option α
  | [], _ => none
  | (n, v) :: l, p => if n = p then some v else findL l p

def removeL {α : Type} (p : Nat) : List (Nat × α) → List (Nat × α)
  | [] => []
  | (n, v) :: l => if n = p then removeL p l else (n, v) :: removeL p l

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
  /-- `while cond { body }` carrying allocator `a`; `β` names its brand inside one iteration and
  `βx` after the loop. The condition is abstracted. -/
  | while_ (a β βx : Nat) (body : List Stmt)

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
  | while_ {B Γ a β0 β βx β'' body rest Γ'} : findL Γ a = some (.alloc β0) → NoHeap Γ β0 →
      β ∉ B → βx ∉ B →
      Ok (β :: B) ((a, .alloc β) :: removeL a Γ) body ((a, .alloc β'') :: removeL a Γ) →
      Ok (βx :: B) ((a, .alloc βx) :: removeL a Γ) rest Γ' →
      Ok B Γ (.while_ a β βx body :: rest) Γ'

/-- Every brand in the context is a brand name in use. -/
def BrandsIn (B : List Nat) (Γ : Ctx) : Prop :=
  ∀ e ∈ Γ, ∀ β, (e.2 = .alloc β ∨ e.2 = .heap β) → β ∈ B

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
  | whileExit {s a β βx body rest s'} : Run s rest s' → Run s (.while_ a β βx body :: rest) s'
  | whileIter {s a β βx body rest s'} : Run s (body ++ .while_ a β βx body :: rest) s' →
      Run s (.while_ a β βx body :: rest) s'

/-- The start of every run: nothing live, no brand generated. -/
def State.init : State := { env := [], next := 0, gen := [] }

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
example : Ok [] [] [.newAlloc 0 10, .while_ 0 20 30 [.alloc 1 0, .free 1 0, .reset 0 0 21],
    .release 0] [] :=
  .newAlloc rfl (by decide) <|
    .while_ (β'' := 21) rfl (by decide) (by decide) (by decide)
      (.alloc rfl rfl <| .free rfl rfl <| .reset rfl (by decide) rfl (by decide) .nil)
      (.release rfl (by decide) .nil)

/-- Three iterations of that loop generate four pairwise distinct brands: 0 for `new`, then 1, 2
and 3 for the resets. -/
example : Run State.init [.newAlloc 0 10, .while_ 0 20 30 [.alloc 1 0, .free 1 0, .reset 0 0 21],
    .release 0] { env := [], next := 4, gen := [0, 1, 2, 3] } :=
  .newAlloc <| .whileIter <| .alloc rfl <| .free rfl rfl <| .reset rfl <|
    .whileIter <| .alloc rfl <| .free rfl rfl <| .reset rfl <|
    .whileIter <| .alloc rfl <| .free rfl rfl <| .reset rfl <|
    .whileExit <| .release rfl .nil

end ConcreteSpec.Brand
