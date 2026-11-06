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
The proofs are in `ConcreteSpec/Proofs/` and say why the rules hang together
(an accepted program never gets stuck, the checker agrees with the rules, and the design theorems).
`ConcreteSpec/README.md` is the reading guide and lists what is not yet formalized.

The examples are decided by running the definitions: `check p = true` means the checker accepts
`p`, and `check_iff` (Proofs/Checker.lean) proves that this is exactly the rule `Checked p`;
`run p 10 = .exit 42` means `p` exits with 42.

The decisions are cited by their row ids (Q1.1 … Q9.19, P1 … P23) and by the recorded rule ids of
docs/spec (ODnn, TYPE-n, OWN-n, …).

The file has seven parts. Each is a small Core fragment with its own judgments, defined only as
far as its theorems need (R4 F8). Unifying them into one Core is work for A3 (R-0560).

1. **Slice 1** (ROADMAP phase B): scalars, `let`, structs, moves and the use-after-move refusal,
   checked `+`, `return`, in Core's binding form (Q9.1), with the static refusal of statically
   known violations (Q6.1–Q6.6). It ends with the slice-0 fragment, the first subset the compiler
   accepts (`ConcreteSpec.Slice0`).
2. **Representation** (Q3.11): the layout and byte encoding of the slice-1 types.
3. **Structured cleanup** (Q9.8, Q2.1, Q2.7, Q1.5): `defer`, frames and exits.
4. **Scoped borrows** (Q3.3, Q3.4, Q9.5, REF-11), with range subviews (Q4.7).
5. **Allocator brands** (Q1.1–Q1.5, Q1.13).
6. **Authority**: capabilities and grants (the authority rules, Q5.1–Q5.4, Q7.11).
7. **Observations**: what the parent process sees, the exit status and traps (Q7.10, Q8.5).

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
  references, no heap, no capabilities, no `defer`. Struct fields are scalars only. Parts 3–7
  model the pieces of those that their theorems need.

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
source name and a number: `t₀` is `⟨"t", 0⟩`. That the runtime-overflow body `add100` is
safe for every input is the theorem `add100_safe` in `Proofs/Examples.lean`. -/

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

example : bodyOkB decls inputCtx add100 (.copy s₂) = true := by decide

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

namespace ConcreteSpec.Slice0
open ConcreteSpec

/-! ### The slice-0 fragment

Slice 0 is the first subset the new compiler accepts (ROADMAP B0, proposed): `u8` literals,
immutable `let` of a `u8`, reads of `u8` locals, checked `+`, and `fn main() -> u8` ending in
`return`. It is a fragment of slice 1, not a second language: the types below are its Core syntax,
and `toCore` embeds it into slice 1's Core, whose rules, evaluator and checker apply unchanged. A
compiler imports `ConcreteSpec.Spec`, produces a `Slice0.Program` for a slice-0 source program,
and checks and runs it as `toCore q`.

What slice 0 leaves out of slice 1: structs and destructuring, `bool`, the integer types other than
`u8`, `move` (every slice-0 value is Copy), and `let _`. Grouping and nested expressions such as
`a + b + c` are surface forms that elaboration turns into A-normal form; elaboration is an open
obligation (README).

Every slice-0 value is statically known (Q6.2): a literal is known, and a local is bound to a known
literal or a known sum. So a checked `+` that could overflow is a known violation, refused (Q6.1),
and an accepted slice-0 program always exits, never traps (`slice0_exits`, `Proofs/Slice0.lean`).
-/

/-- A slice-0 atom: a decimal `u8` literal (a natural number; whether it fits `u8` is checked,
ARITH-2) or a read of a local. -/
inductive Atom where
  | lit (n : Nat)
  | var (x : Var)
  deriving DecidableEq, Repr

/-- A slice-0 expression: an atom, or checked `l + r` of two atoms (ARITH-3). -/
inductive Expr where
  | atom (a : Atom)
  | add (l r : Atom)
  deriving DecidableEq, Repr

/-- `let x: u8 = e;` — the only slice-0 statement. -/
structure Stmt where
  x : Var
  e : Expr
  deriving DecidableEq, Repr

/-- `fn main() -> u8 { body; return ret; }` with no structs. -/
structure Program where
  body : List Stmt
  ret : Atom
  deriving Repr

/-- An atom in Core: a literal is `u8`, a local is read with `copy` (`u8` is Copy). -/
def Atom.toCore : Atom → ConcreteSpec.Atom
  | .lit n => .int .u8 n
  | .var x => .copy x

def Expr.toCore : Expr → ConcreteSpec.Expr
  | .atom a => .atom a.toCore
  | .add l r => .add l.toCore r.toCore

def Stmt.toCore (s : Stmt) : ConcreteSpec.Stmt := .let_ (.name s.x) (.scalar (.int .u8)) s.e.toCore

/-- The slice-1 program a slice-0 program is: no declarations, the same body and return. -/
def toCore (q : Program) : ConcreteSpec.Program :=
  { decls := [], body := q.body.map Stmt.toCore, ret := q.ret.toCore }

/-- A slice-1 program is in slice 0 when it is the Core form of a slice-0 program: no
declarations, every statement a `let` of a `u8` local from a slice-0 expression, and a slice-0
return atom. The slice-0 program is unique (`toCore_injective`, `Proofs/Slice0.lean`). -/
def InSlice0 (p : ConcreteSpec.Program) : Prop := ∃ q, toCore q = p

/-! ### Slice-0 examples

The required slice-0 behaviours: normal exit 42, exit 255 at the boundary, static refusal of a
known overflow and of an out-of-range literal, refusal of an unused binding, and the use of an
undeclared local. -/

private def a₀ : Var := ⟨"a", 0⟩
private def b₁ : Var := ⟨"b", 1⟩

/-- ```
fn main() -> u8 {
    let a: u8 = 20;
    let b: u8 = 22;
    return a + b;
}
```
In A-normal form the returned sum gets its own binding `s`. It exits with 42. -/
def answer : Program :=
  { body := [⟨a₀, .atom (.lit 20)⟩, ⟨b₁, .atom (.lit 22)⟩, ⟨⟨"s", 2⟩, .add (.var a₀) (.var b₁)⟩],
    ret := .var ⟨"s", 2⟩ }

example : check (toCore answer) = true := by decide
example : run (toCore answer) 10 = .exit 42 := by decide

/-- `let a: u8 = 200; let b: u8 = 55; return a + b;` exits with 255, the largest `u8`. -/
def boundary : Program :=
  { body := [⟨a₀, .atom (.lit 200)⟩, ⟨b₁, .atom (.lit 55)⟩, ⟨⟨"s", 2⟩, .add (.var a₀) (.var b₁)⟩],
    ret := .var ⟨"s", 2⟩ }

example : check (toCore boundary) = true := by decide
example : run (toCore boundary) 10 = .exit 255 := by decide

/-- `let a: u8 = 200; let b: u8 = 56; return a + b;` is refused: `a + b` is known to overflow
(Q6.1), so it is never run. -/
def overflow : Program :=
  { body := [⟨a₀, .atom (.lit 200)⟩, ⟨b₁, .atom (.lit 56)⟩, ⟨⟨"s", 2⟩, .add (.var a₀) (.var b₁)⟩],
    ret := .var ⟨"s", 2⟩ }

example : check (toCore overflow) = false := by decide

/-- `return 256;` is refused: the literal does not fit `u8` (ARITH-2). -/
example : check (toCore { body := [], ret := .lit 256 }) = false := by decide

/-- `let a: u8 = 1; return 0;` is refused: `a` is never used (OWN-5). -/
example : check (toCore { body := [⟨a₀, .atom (.lit 1)⟩], ret := .lit 0 }) = false := by decide

/-- `return a;` with no `a` in scope is refused. -/
example : check (toCore { body := [], ret := .var a₀ }) = false := by decide

/-- A local may be read many times (`u8` is Copy): `let a: u8 = 7; let b: u8 = a + a; return b;`
exits with 14. -/
def double : Program :=
  { body := [⟨a₀, .atom (.lit 7)⟩, ⟨b₁, .add (.var a₀) (.var a₀)⟩], ret := .var b₁ }

example : check (toCore double) = true := by decide
example : run (toCore double) 10 = .exit 14 := by decide

end ConcreteSpec.Slice0

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

/-- The registration is accepted (`Reg`). -/
theorem stack_reg : Reg ownedAfter stack :=
  .enter (k := .block) <|
    .push (k := .loopBody 1) (d := desV)
      (.enter (k := .loopBody 1) <|
        .push (k := .fnBody) (d := desB)
          (.push (k := .fnBody) (d := relA) (.enter (k := .fnBody) .empty)
            rfl (by decide) (by decide) (by decide))
          rfl (by decide) (by decide) (by decide))
      rfl (by decide) (by decide) (by decide)

/-- On `return` with every call returning, the cleanup run is `destroy(v)`, the release of its loan
7, `destroy(b)` and `give_up(a)`, in that order. -/
example : RunCleanup (fun _ => true) (pending stack)
    [.call desV, .release ⟨7, 2⟩, .call desB, .call relA] .done :=
  .step rfl (.step rfl (.step rfl .nil))

/-- If `destroy(b)` traps on `return`, `destroy(v)` ran and released its loan first, and
`give_up(a)` never runs. -/
example : RunCleanup (fun d => d != desB) (pending stack)
    [.call desV, .release ⟨7, 2⟩, .call desB] .trapped :=
  .step rfl (.trap rfl)

/-- If `destroy(v)` itself traps while holding loan 7, the trace stops at that call: loan 7 is not
released by cleanup, and `destroy(b)` and `give_up(a)` never run. -/
example : RunCleanup (fun d => d != desV) (pending stack) [.call desV] .trapped :=
  .trap rfl

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

/-- The registration is accepted (`Reg`). -/
theorem stack2_reg : Reg ownedAfter2 stack2 :=
  .push (k := .block) (d := desC)
    (.enter (k := .block) <|
      .push (k := .loopBody 2) (d := desD)
        (.enter (k := .loopBody 2) <|
          .push (k := .loopBody 1) (d := desV)
            (.enter (k := .loopBody 1) <|
              .push (k := .fnBody) (d := desB)
                (.push (k := .fnBody) (d := relA) (.enter (k := .fnBody) .empty)
                  rfl (by decide) (by decide) (by decide))
                rfl (by decide) (by decide) (by decide))
            rfl (by decide) (by decide) (by decide))
        rfl (by decide) (by decide) (by decide))
    rfl (by decide) (by decide) (by decide)

/-- Falling off the block runs `destroy(c)`; `continue L2` also runs `destroy(d)`; `break L1` and
`continue L1` (the outer label) also run `destroy(v)` and release its loan. `continue L1` then
tests `L1`'s condition again (F7). -/
example : (unwound .fallthrough stack2).map (expected ∘ pending) = some [.call desC] := rfl

example : (unwound (.cont 2) stack2).map (expected ∘ pending) = some [.call desC, .call desD] := rfl

example : (unwound (.brk 1) stack2).map (expected ∘ pending) =
    some [.call desC, .call desD, .call desV, .release ⟨7, 2⟩] := rfl

example : (unwound (.cont 1) stack2).map (expected ∘ pending) =
    some [.call desC, .call desD, .call desV, .release ⟨7, 2⟩] := rfl

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

/-! ### Range subviews (Q4.7)

`borrow [mut] src from a until b as v { body } else { els }` opens the half-open range `[a, b)`
of `src` as the view `v` (Q4.7: one scoped form for every view; there is no separate slice
construct). The source and then both `u64` bounds are evaluated once, left to right; in Core all
three are atoms, so their evaluation cannot trap. The range is valid when `a ≤ b ≤ len(src)`,
counted in elements for an array and in bytes for text; text also needs both offsets on UTF-8
scalar boundaries (R4 F4.2: `0`, the length, or an offset whose byte is not a continuation byte
`0b10xxxxxx`). If the range is valid, the body runs with `v`, indexed relative to `a`, under the
access rules of `borrow [mut] src as v { body }` (`Ok`): the view keeps the source's root, and a
mutable open freezes it. If the range is invalid, `else` runs; without `else` the program traps,
like indexing out of range. A range statically known to be invalid is refused when there is no
`else` (OD118, everywhere, including trusted code) and accepted when there is one, whose `else`
then runs as ordinary branching (Q4.7). Without a range there is no `else` (the whole view always
opens). Element access, the no-escape rules and text validity follow the examples below.
-/

/-- A source's value at run time: an array of `len` elements, or text with its bytes. -/
inductive Source where
  | array (len : Nat)
  | text (bytes : List Nat)
  deriving DecidableEq, Repr

/-- The length of a source: elements for an array, bytes for text. -/
def Source.len : Source → Nat
  | .array n => n
  | .text bs => bs.length

/-- `i` is a UTF-8 scalar boundary of the text `bs` (R4 F4.2): `0`, the length, or an offset
whose byte is not a continuation byte (`x / 64 = 2` is `0b10xxxxxx`). -/
def boundary (bs : List Nat) (i : Nat) : Bool :=
  i == 0 || i == bs.length ||
    (match bs[i]? with
     | some x => x / 64 != 2
     | none => false)

/-- The range `[a, b)` is valid for the source (Q4.7). -/
def validRange : Source → Nat → Nat → Bool
  | .array n, a, b => decide (a ≤ b ∧ b ≤ n)
  | .text bs, a, b => decide (a ≤ b ∧ b ≤ bs.length) && boundary bs a && boundary bs b

/-- What opening a range does at run time. -/
inductive RangeStep where
  /-- Run the body with the view `[a, b)`. -/
  | enter
  /-- Run `else`. -/
  | orElse
  /-- There is no `else`: trap. -/
  | trap
  deriving DecidableEq, Repr

/-- Open the range `[a, b)` of `src`, with or without an `else`. -/
def openRange (src : Source) (a b : Nat) (hasElse : Bool) : RangeStep :=
  if validRange src a b then .enter else if hasElse then .orElse else .trap

/-- The view's index `i` is the source's index `a + i` (Q4.7: indexed relative to its start). -/
def viewIndex (a i : Nat) : Nat := a + i

/-- What the checker knows statically about the source and the bounds, from the fixed table of
statically known values (Q6.2: literals, immutable bindings of known values; an array's length
from its type). `none` is unknown. -/
structure RangeFacts where
  src : Option Source
  a : Option Nat
  b : Option Nat
  deriving DecidableEq, Repr

/-- `i` can be an end of a range of the source: at most its length, and for text a UTF-8 scalar
boundary. A range `[a, b)` is valid exactly when `a ≤ b` and both are ends (`validRange_iff`,
`Proofs/Borrow.lean`). -/
def Source.edge : Source → Nat → Bool
  | .array n, i => decide (i ≤ n)
  | .text bs, i => decide (i ≤ bs.length) && boundary bs i

/-- The range is proven invalid from what the checker knows (Q4.7, Q6.1): the known bounds are
reversed (`b < a`), or a known bound cannot be an end of the known source. These are all the
cases: the range is known invalid exactly when every source and bounds agreeing with the facts
fail the range test (`knownInvalid_iff`, `Proofs/Borrow.lean`), so a partial fact such as
`from 5 until 2` over an unknown source is enough. -/
def knownInvalid (k : RangeFacts) : Bool :=
  (match k.a, k.b with
   | some a, some b => decide (b < a)
   | _, _ => false) ||
  (match k.src, k.a with
   | some s, some a => !s.edge a
   | _, _ => false) ||
  (match k.src, k.b with
   | some s, some b => !s.edge b
   | _, _ => false)

/-- A run-time source and bounds agree with what the checker knows. -/
def Agrees (k : RangeFacts) (s : Source) (a b : Nat) : Prop :=
  (∀ s', k.src = some s' → s' = s) ∧ (∀ a', k.a = some a' → a' = a) ∧ (∀ b', k.b = some b' → b' = b)

/-- The range form is accepted, from the places `Γ` to `Γ'` with the statements `rest` after it.
Its success path is checked as the plain borrow (`Ok`, the same access rules); its failure path is
`else` followed by `rest`, and both paths reach the same places. Without `else`, a range known to
be invalid is refused (OD118). -/
def RangeOk (Γ : Ctx) (m : Bool) (src v : Nat) (k : RangeFacts) (body : List Stmt)
    (els : Option (List Stmt)) (rest : List Stmt) (Γ' : Ctx) : Prop :=
  Ok Γ (.borrow m src v body :: rest) Γ' ∧
    match els with
    | none => knownInvalid k = false
    | some e => Ok Γ (e ++ rest) Γ'

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

/-! ### Range examples (Q4.7) -/

/-- An array of 10 elements: `from 2 until 5` opens; `from 5 until 2` and `from 3 until 11` do
not, and run `else` if there is one, or trap. -/
example : openRange (.array 10) 2 5 false = .enter := by decide
example : openRange (.array 10) 5 2 true = .orElse := by decide
example : openRange (.array 10) 3 11 false = .trap := by decide

/-- The text `"é!"` is the bytes `0xC3 0xA9 0x21`. `from 0 until 2` is `"é"`; `from 1 until 3`
starts inside `é` and is invalid. -/
example : openRange (.text [0xC3, 0xA9, 0x21]) 0 2 false = .enter := by decide
example : openRange (.text [0xC3, 0xA9, 0x21]) 1 3 false = .trap := by decide
example : openRange (.text [0xC3, 0xA9, 0x21]) 3 3 false = .enter := by decide

/-- `borrow xs from 2 until 9 as s { … }` over an array of 4 is known invalid: refused without
`else`. -/
example : knownInvalid ⟨some (.array 4), some 2, some 9⟩ = true := by decide

example : ¬ ∃ Γ', RangeOk x0 false 0 1 ⟨some (.array 4), some 2, some 9⟩ [.read 1] none [] Γ' := by
  rintro ⟨_, _, h⟩
  exact absurd (show knownInvalid ⟨some (.array 4), some 2, some 9⟩ = false from h) (by decide)

/-- With `else` the same range is accepted, and the `else` runs (ordinary branching). -/
example : ∃ Γ', RangeOk x0 false 0 1 ⟨some (.array 4), some 2, some 9⟩ [.read 1] (some [.read 0])
    [] Γ' :=
  ⟨_, .borrow rfl (by decide) (by intro h; cases h) rfl (.read rfl (by decide) .nil) .nil,
    .read rfl (by decide) .nil⟩

/-- Partial facts can prove a range invalid: `from 5 until 2` over any source, and
`until 9` over an array of 4 whatever the start. -/
example : knownInvalid ⟨none, some 5, some 2⟩ = true := by decide
example : knownInvalid ⟨some (.array 4), none, some 9⟩ = true := by decide

/-- A known end inside a UTF-8 scalar is invalid whatever the other bound: `from 1` in `"é!"`. -/
example : knownInvalid ⟨some (.text [0xC3, 0xA9, 0x21]), some 1, none⟩ = true := by decide

/-- With an unknown bound that could make the range valid, the range is accepted without `else`,
and traps if it is invalid at run time. -/
example : ∃ Γ', RangeOk x0 false 0 1 ⟨some (.array 4), some 2, none⟩ [.read 1] none [] Γ' :=
  ⟨_, .borrow rfl (by decide) (by intro h; cases h) rfl (.read rfl (by decide) .nil) .nil, rfl⟩


/-! ### No escape (Q3.3, Q4.7)

A view is second-class: `view` makes it a non-owner, and only owners move (`Ok.move`). In Core,
returning a value, storing it in another binding or field, passing it by value and capturing it
are all moves, so a view does none of them (`view_not_moved`, `Proofs/Borrow.lean`). The view's
name is fresh and goes out of scope with the block (`restore` keeps only the outer places), so no
statement after the block names it (`view_not_used_after`), and every place of the source's root
is back after the block with exactly the entry it had before the open, its lock included: the
freeze of a mutable open ends with the block (`root_restored`). -/

/-- `p` is named in `ss` where it refers to the outer place `p`: not inside a block whose view
is also called `p`. -/
inductive FreeUse (p : Nat) : List Stmt → Prop where
  | read {rest} : FreeUse p (.read p :: rest)
  | write {rest} : FreeUse p (.write p :: rest)
  | move {rest} : FreeUse p (.move p :: rest)
  | src {m v body rest} : FreeUse p (.borrow m p v body :: rest)
  | body {m s v body rest} : v ≠ p → FreeUse p body → FreeUse p (.borrow m s v body :: rest)
  | tail {s rest} : FreeUse p rest → FreeUse p (s :: rest)

/-- Returning, storing or passing the view is a move of it, and is refused. -/
example : ¬ ∃ Γ', Ok x0 [.borrow true 0 1 [.move 1]] Γ' := by
  rintro ⟨Γ', h⟩
  cases h with
  | borrow hs _ _ _ hb _ =>
    simp only [x0, find, ↓reduceIte, Option.some.injEq] at hs; subst hs
    cases hb with
    | move hf _ ho _ => simp [bodyCtx, find, view] at hf; subst hf; simp at ho

/-- Naming the view after its block is refused: `borrow mut x as v { } read(v)`. -/
example : ¬ ∃ Γ', Ok x0 [.borrow true 0 1 [], .read 1] Γ' := by
  rintro ⟨Γ', h⟩
  cases h with
  | borrow hs _ _ _ hb hr =>
    cases hb
    cases hr with
    | read hf _ _ => simp [restore, find, bodyCtx, mapPlaces, x0] at hf

/-! ### Range views at run time (Q4.7)

The run of array sources and range views. Memory maps each owner place to its elements; a view
is its source place, its range `[a, b)` and whether it is mutable. `v[i]` reads or writes the
source's element `a + i` when `i < b - a`, and traps otherwise, like indexing (Q4.7). Opening a
range runs the range test of `openRange`; the view exists only inside its block. The results
(`Proofs/Borrow.lean`): an access through a view reaches exactly the source element `a + i`,
which lies in `[a, b)` and inside the source, or traps (`view_get`, `view_set`, `view_in_source`);
a write through a view changes exactly that element (`view_write_exact`); and a block that
accesses only its view changes nothing outside `[a, b)` of its source (`range_block_frame`).

This run covers array sources; a text view reads bytes with the same index arithmetic, and text
validity is below. Views of views (opening a range of a view) are not modelled, and the run is
not yet joined to the access rules `Ok` in one theorem (they are separate fragments, joined by A3).
-/

/-- The entry of `p` in a list keyed by place. -/
def getN {α : Type} : List (Nat × α) → Nat → Option α
  | [], _ => none
  | (n, x) :: l, p => if n = p then some x else getN l p

/-- Replace the entry of `p`. -/
def putN {α : Type} (p : Nat) (x : α) : List (Nat × α) → List (Nat × α)
  | [] => []
  | (n, y) :: l => if n = p then (n, x) :: l else (n, y) :: putN p x l

/-- Memory: each owner array place and its elements. -/
abbrev Mem := List (Nat × List Nat)

/-- The element `j` of place `p`. -/
def elem (μ : Mem) (p j : Nat) : Option Nat := (getN μ p).bind (·[j]?)

/-- A view at run time: its source place, its range `[a, b)`, and whether it is mutable. -/
structure VInfo where
  src : Nat
  a : Nat
  b : Nat
  mutable : Bool
  deriving DecidableEq, Repr

abbrev Views := List (Nat × VInfo)

/-- The source index of the view's index `i`: `a + i` when `i < b - a` (Q4.7: indexed relative
to the start), else none (the access traps). -/
def elemIdx (w : VInfo) (i : Nat) : Option Nat := if i < w.b - w.a then some (w.a + i) else none

inductive VStmt where
  /-- Read `v[i]` and output it. -/
  | get (v i : Nat)
  /-- `v[i] = x`. -/
  | set (v i x : Nat)
  /-- Read `p[i]` of an owner array, and output it. -/
  | getS (p i : Nat)
  /-- `p[i] = x` on an owner array. -/
  | setS (p i x : Nat)
  /-- `borrow [mut] p from a until b as v { body } [else { els }]`. -/
  | range (m : Bool) (p v a b : Nat) (body : List VStmt) (els : Option (List VStmt))

/-- The outcome of a run: the memory and the values output, a trap, stuck (an unbound name or a
write through a shared view, which the access rules refuse), or out of fuel. -/
inductive VR where
  | ok (μ : Mem) (out : List Nat)
  | trap
  | stuck
  | outOfFuel
  deriving DecidableEq, Repr

/-- The run. Each statement costs one unit of fuel; a block runs with the fuel left at its
open. The view is bound inside its block only. -/
def vrun : Nat → Mem → Views → List Nat → List VStmt → VR
  | 0, _, _, _, _ => .outOfFuel
  | _ + 1, μ, _, out, [] => .ok μ out
  | n + 1, μ, V, out, .get v i :: rest => match getN V v with
    | none => .stuck
    | some w => match elemIdx w i with
      | none => .trap
      | some j => match elem μ w.src j with
        | some x => vrun n μ V (out ++ [x]) rest
        | none => .stuck
  | n + 1, μ, V, out, .set v i x :: rest => match getN V v with
    | none => .stuck
    | some w =>
      if w.mutable then
        match elemIdx w i with
        | none => .trap
        | some j => match getN μ w.src with
          | some xs => if j < xs.length then vrun n (putN w.src (xs.set j x) μ) V out rest
            else .stuck
          | none => .stuck
      else .stuck
  | n + 1, μ, V, out, .getS p i :: rest => match getN μ p with
    | none => .stuck
    | some xs => match xs[i]? with
      | some x => vrun n μ V (out ++ [x]) rest
      | none => .trap
  | n + 1, μ, V, out, .setS p i x :: rest => match getN μ p with
    | none => .stuck
    | some xs => if i < xs.length then vrun n (putN p (xs.set i x) μ) V out rest else .trap
  | n + 1, μ, V, out, .range m p v a b body els :: rest => match getN μ p with
    | none => .stuck
    | some xs => match openRange (.array xs.length) a b els.isSome with
      | .enter => match vrun n μ ((v, ⟨p, a, b, m⟩) :: V) out body with
        | .ok μ' out' => vrun n μ' V out' rest
        | r => r
      | .orElse => match els with
        | some e => match vrun n μ V out e with
          | .ok μ' out' => vrun n μ' V out' rest
          | r => r
        | none => .stuck
      | .trap => .trap

/-- `xs = [10, 20, 30, 40, 50]`; `borrow mut xs from 1 until 4 as s { s[0] = 7; s[2] = 9;
print(s[1]); }`: the writes land on `xs[1]` and `xs[3]`, the read is `xs[2]`. -/
example : vrun 10 [(0, [10, 20, 30, 40, 50])] [] []
    [.range true 0 1 1 4 [.set 1 0 7, .set 1 2 9, .get 1 1] none] =
    .ok [(0, [10, 7, 30, 9, 50])] [30] := by decide

/-- `s[3]` in a view of length 3 traps, even though `xs[4]` exists. -/
example : vrun 10 [(0, [10, 20, 30, 40, 50])] [] [] [.range true 0 1 1 4 [.set 1 3 7] none] =
    .trap := by decide

/-- An invalid range runs `else` when there is one, and traps when there is none. -/
example : vrun 10 [(0, [10, 20])] [] [] [.range false 0 1 1 5 [.get 1 0] (some [.getS 0 0])] =
    .ok [(0, [10, 20])] [10] := by decide
example : vrun 10 [(0, [10, 20])] [] [] [.range false 0 1 1 5 [.get 1 0] none] = .trap := by
  decide

/-- After the block the view is gone: naming it is stuck (and the access rules refuse it). -/
example : vrun 10 [(0, [10, 20])] [] [] [.range false 0 1 0 2 [] none, .get 1 0] = .stuck := by
  decide

/-- A write through a shared view is stuck (the access rules refuse it). -/
example : vrun 10 [(0, [10, 20])] [] [] [.range false 0 1 0 2 [.set 1 0 5] none] = .stuck := by
  decide

/-- A block whose statements all access its view `v`. -/
inductive ViewOnly (v : Nat) : List VStmt → Prop where
  | nil : ViewOnly v []
  | get {i rest} : ViewOnly v rest → ViewOnly v (.get v i :: rest)
  | set {i x rest} : ViewOnly v rest → ViewOnly v (.set v i x :: rest)

/-! ### Text views (Q4.6, Q4.7)

Well-formed UTF-8 (RFC 3629, Unicode table 3-7): a byte list split into scalars, each one to four
bytes: a lead byte and continuation bytes `0x80`–`0xBF`, with the second byte restricted after
`0xE0`, `0xED`, `0xF0` and `0xF4` so that no overlong form, surrogate or value above `U+10FFFF` is
well formed. A text range on scalar boundaries of well-formed text is itself well formed
(`text_view_utf8`, `Proofs/Borrow.lean`). -/

/-- A continuation byte. -/
def cont (c : Nat) : Bool := 0x80 ≤ c && c ≤ 0xBF

/-- One well-formed UTF-8 encoded scalar. -/
def isScalar : List Nat → Bool
  | [x] => x < 0x80
  | [x, c] => 0xC2 ≤ x && x ≤ 0xDF && cont c
  | [x, c, d] =>
    ((x == 0xE0 && 0xA0 ≤ c && c ≤ 0xBF) ||
      (((0xE1 ≤ x && x ≤ 0xEC) || (0xEE ≤ x && x ≤ 0xEF)) && cont c) ||
      (x == 0xED && 0x80 ≤ c && c ≤ 0x9F)) && cont d
  | [x, c, d, e] =>
    ((x == 0xF0 && 0x90 ≤ c && c ≤ 0xBF) || (0xF1 ≤ x && x ≤ 0xF3 && cont c) ||
      (x == 0xF4 && 0x80 ≤ c && c ≤ 0x8F)) && cont d && cont e
  | _ => false

/-- Well-formed UTF-8: a sequence of well-formed scalars. -/
inductive Utf8 : List Nat → Prop where
  | nil : Utf8 []
  | cons {s rest} : isScalar s = true → Utf8 rest → Utf8 (s ++ rest)

/-- The bytes of the text range `[a, b)`. -/
def textSlice (bs : List Nat) (a b : Nat) : List Nat := (bs.drop a).take (b - a)

/-- `"é!"` is well formed, and so is its range `[0, 2)`, `"é"`. -/
example : Utf8 [0xC3, 0xA9, 0x21] := .cons (s := [0xC3, 0xA9]) rfl (.cons (s := [0x21]) rfl .nil)
example : textSlice [0xC3, 0xA9, 0x21] 0 2 = [0xC3, 0xA9] := rfl

/-- A lone continuation byte, an overlong `/` (`0xC0 0xAF`) and a surrogate (`0xED 0xA0 0x80`) are
not scalars. -/
example : isScalar [0xA9] = false := rfl
example : isScalar [0xC0, 0xAF] = false := rfl
example : isScalar [0xED, 0xA0, 0x80] = false := rfl

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
allocator operations (Q5, Q2.7) is not modelled here (part 6 models capabilities in its own
fragment).

Three views of the same programs: the static check (`Ok`); a ghost run (`Run`, executable as
`ghost`) that gives every allocator a run-time brand from a counter and checks brand equality on
each operation; and the erased run (`erased`), the program as it runs with no brands (Q1.1). A
checked program passes every ghost check (`checked_passes_brand_checks`), and its erased run does
exactly what its ghost run does, brands removed (`brand_erasure`), for every outcome of its loop
tests and any fuel.
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
  deriving DecidableEq, Repr

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

/-- The result of the executable ghost run. -/
inductive GR where
  /-- The run finished in state `s`, having made its loop tests up to number `k`. -/
  | done (s : State) (k : Nat)
  /-- A brand check failed, or a value was missing: no rule of `Run` applies. -/
  | stuck
  /-- The fuel ran out; inconclusive. -/
  | outOfFuel
  deriving DecidableEq, Repr

/-- The ghost run as an executable function (`ghost_run`, `Proofs/Brand.lean`: every finished
ghost run is a `Run`). The loop conditions are abstracted: loop test number `k` of the run takes
the outcome `σ k`, and the next test is number `k + 1`, so every sequence of test outcomes is
some `σ`. Each statement costs one unit of fuel, and the body of an iteration runs with the fuel
left at its test. -/
def ghost (σ : Nat → Bool) : Nat → Nat → State → List Stmt → GR
  | 0, _, _, _ => .outOfFuel
  | _ + 1, k, s, [] => .done s k
  | n + 1, k, s, .newAlloc a _ :: rest => ghost σ n k (s.bind a s.env) rest
  | n + 1, k, s, .alloc h a :: rest => match findL s.env a with
    | some (.allocV b) => ghost σ n k { s with env := (h, .heapV b) :: s.env } rest
    | _ => .stuck
  | n + 1, k, s, .use h a :: rest => match findL s.env h, findL s.env a with
    | some (.heapV b), some (.allocV b') => if b = b' then ghost σ n k s rest else .stuck
    | _, _ => .stuck
  | n + 1, k, s, .free h a :: rest => match findL s.env h, findL s.env a with
    | some (.heapV b), some (.allocV b') =>
      if b = b' then ghost σ n k { s with env := removeL h s.env } rest else .stuck
    | _, _ => .stuck
  | n + 1, k, s, .release a :: rest => match findL s.env a with
    | some (.allocV _) => ghost σ n k { s with env := removeL a s.env } rest
    | _ => .stuck
  | n + 1, k, s, .reset a' a _ :: rest => match findL s.env a with
    | some (.allocV _) => ghost σ n k (s.bind a' (removeL a s.env)) rest
    | _ => .stuck
  | n + 1, k, s, .while_ a β βx body :: rest =>
    if σ k then
      match ghost σ n (k + 1) s body with
      | .done s₁ k₁ => ghost σ n k₁ s₁ (.while_ a β βx body :: rest)
      | r => r
    else ghost σ n (k + 1) s rest

/-! ### The erased run (Q1.1)

The program as it runs with brands erased: an allocator and a heap value carry no brand, and no
operation compares brands. Only the presence and kind of each value is checked. -/

/-- A run-time value without its brand. -/
inductive EVal where
  | alloc
  | heap
  deriving DecidableEq, Repr

/-- Erase a value's brand. -/
def Val.erase : Val → EVal
  | .allocV _ => .alloc
  | .heapV _ => .heap

/-- Erase every brand of an environment. -/
def eraseEnv (env : List (Nat × Val)) : List (Nat × EVal) := env.map (fun p => (p.1, p.2.erase))

/-- The result of the erased run. -/
inductive ER where
  | done (env : List (Nat × EVal)) (k : Nat)
  | stuck
  | outOfFuel
  deriving DecidableEq, Repr

/-- The erased run, with the same fuel and loop tests as `ghost`. -/
def erased (σ : Nat → Bool) : Nat → Nat → List (Nat × EVal) → List Stmt → ER
  | 0, _, _, _ => .outOfFuel
  | _ + 1, k, env, [] => .done env k
  | n + 1, k, env, .newAlloc a _ :: rest => erased σ n k ((a, .alloc) :: env) rest
  | n + 1, k, env, .alloc h a :: rest => match findL env a with
    | some .alloc => erased σ n k ((h, .heap) :: env) rest
    | _ => .stuck
  | n + 1, k, env, .use h a :: rest => match findL env h, findL env a with
    | some .heap, some .alloc => erased σ n k env rest
    | _, _ => .stuck
  | n + 1, k, env, .free h a :: rest => match findL env h, findL env a with
    | some .heap, some .alloc => erased σ n k (removeL h env) rest
    | _, _ => .stuck
  | n + 1, k, env, .release a :: rest => match findL env a with
    | some .alloc => erased σ n k (removeL a env) rest
    | _ => .stuck
  | n + 1, k, env, .reset a' a _ :: rest => match findL env a with
    | some .alloc => erased σ n k ((a', .alloc) :: removeL a env) rest
    | _ => .stuck
  | n + 1, k, env, .while_ a β βx body :: rest =>
    if σ k then
      match erased σ n (k + 1) env body with
      | .done env₁ k₁ => erased σ n k₁ env₁ (.while_ a β βx body :: rest)
      | r => r
    else erased σ n (k + 1) env rest

/-- Erase the brands of a ghost result. -/
def GR.erase : GR → ER
  | .done s k => .done (eraseEnv s.env) k
  | .stuck => .stuck
  | .outOfFuel => .outOfFuel

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

/-- A value of brand 10 used through an allocator of brand 11 is refused (nominal brand equality,
Q1.4). -/
example : ¬ Ok [10, 11] [(1, .heap 10), (0, .alloc 11)] [.use 1 0] [(1, .heap 10), (0, .alloc 11)] := by
  intro hok
  cases hok with
  | use h1 h2 _ => simp [findL] at h1 h2; subst h1; cases h2

/-- A loop that resets its allocator each iteration is accepted (existential loop state, Q1.3). -/
example : Ok [] [] [.newAlloc 0 10, .while_ 0 20 30 [.alloc 1 0, .free 1 0, .reset 0 0 21],
    .release 0] [] :=
  .newAlloc rfl (by decide) <|
    .while_ (β'' := 21) rfl (by decide) (by decide) (by decide)
      (.alloc rfl rfl <| .free rfl rfl <| .reset rfl (by decide) rfl (by decide) .nil)
      (.release rfl (by decide) .nil)

/-- The ghost run of that loop, with loop tests that iterate three times (tests 0–2 iterate,
test 3 exits), finishes without a failed brand check, in the state of the `Run` below. By `checked_passes_brand_checks` (`Proofs/Brand.lean`) every checked
program does so, for every outcome of its loop tests. -/
example : ghost (fun k => decide (k < 3)) 20 0 State.init [.newAlloc 0 10,
    .while_ 0 20 30 [.alloc 1 0, .free 1 0, .reset 0 0 21], .release 0] =
    .done { env := [], next := 4, gen := [0, 1, 2, 3] } 4 := by
  decide

/-- Its erased run does the same, without brands. -/
example : erased (fun k => decide (k < 3)) 20 0 [] [.newAlloc 0 10,
    .while_ 0 20 30 [.alloc 1 0, .free 1 0, .reset 0 0 21], .release 0] = .done [] 4 := by
  decide

/-- The refused brand mismatch fails its ghost check: brand 0's value through brand 1's
allocator. -/
example : ghost (fun _ => false) 10 0 State.init [.newAlloc 0 10, .newAlloc 2 11,
    .alloc 1 0, .use 1 2] = .stuck := by decide

/-- Erasure removes that check: the erased run of the refused program does not notice the
mismatch. Brand checking is static only. -/
example : erased (fun _ => false) 10 0 [] [.newAlloc 0 10, .newAlloc 2 11, .alloc 1 0, .use 1 2] =
    .done [(1, .heap), (2, .alloc), (0, .alloc)] 0 := by decide

/-- Three iterations of that loop generate four pairwise distinct brands: 0 for `new`, then 1, 2
and 3 for the resets. -/
example : Run State.init [.newAlloc 0 10, .while_ 0 20 30 [.alloc 1 0, .free 1 0, .reset 0 0 21],
    .release 0] { env := [], next := 4, gen := [0, 1, 2, 3] } :=
  .newAlloc <| .whileIter <| .alloc rfl <| .free rfl rfl <| .reset rfl <|
    .whileIter <| .alloc rfl <| .free rfl rfl <| .reset rfl <|
    .whileIter <| .alloc rfl <| .free rfl rfl <| .reset rfl <|
    .whileExit <| .release rfl .nil

end ConcreteSpec.Brand

namespace ConcreteSpec.Authority

/-! ## Part 6. Authority: capabilities and grants (authority rules 1–8, 13; Q5.1–Q5.4, Q7.11)

A function's `with(...)` clause lists the permissions it may use. Permissions are capabilities;
they exist only at compile time. The resources a function may reach are grant values, passed to
it as parameters. An operation needs both (rules 1 and 2): a grant of the operation's type, and
every capability the primitive table gives the operation (rule 7). A call needs the callee's
permissions, with the callee's capability variables replaced by the call's explicit capability
arguments (Q5.1, Q5.4: no inference), to be included in the caller's (TRUST-4, rule 7). A trusted
function is checked the same way, and `Unsafe` never stands in for an operational capability
(rule 5). A function's permissions are checked once, for every value of its capability variables
(rule 3), and `sub_iff` (`Proofs/Authority.lean`) proves that the check is exactly inclusion for
every value. `authority_contained` proves what the rules buy: every event a run of `main`
produces, whether or not the run finishes, is an operation on a grant `main` received, through
capabilities `main` declares.

`ProgOk` checks authority under an assumed valid launch: it does not check `main`'s grant
parameters against the target (the hosted table's instance limits, such as at most one `StdOut`,
and the grants and permissions the target offers; row Q7.10). `observe` (part 7) refuses a start
whose configured grants are missing.

Not modelled here: the ownership of grants (that `main` consumes each grant before it returns,
rule 12, and narrowing), checked acquisition, minting and unforgeability of grants and the
identity of trusted wrappers (rule 11), the target's own effects (startup, shutdown, the trap
message), the "may block" table (rule 6), returned values (rule 8), function-pointer values and
their exact capability sets (rule 4), foreign calls, trait dispatch, generic methods and generated
cleanup (rules 5 and 7; this part has direct calls only), and reserving the capability names
against other uses (rule 13). The operations are a representative part of the hosted table (row
Q7.10), one per grant type used here.
-/

/-- The reserved capability names (authority rule 13). `Console` is the permission for operations
on the standard-stream grants `StdIn`, `StdOut` and `StdErr` (row Q7.11); `Device` is for device
grants; `Unsafe` is not an operational capability (rule 5). -/
inductive Cap where
  | fs | network | env | time | alloc | device | console | process | «unsafe»
  deriving DecidableEq, Repr

/-- Every capability, once. -/
def Cap.all : List Cap := [.fs, .network, .env, .time, .alloc, .device, .console, .process, .«unsafe»]

/-- A capability-set expression as written in `with(...)` or as an explicit capability argument
(Q5.2): named capabilities and capability variables, joined by union. A function's capability
variables are numbered by position, `0, 1, …`. `with()` is `⟨[], []⟩`. -/
structure CapExpr where
  caps : List Cap
  vars : List Nat
  deriving DecidableEq, Repr

/-- A written set names no capability and no variable twice (OD108, Q5.3: a repeated name is
refused, not repaired), and uses only the `n` capability variables in scope. -/
def CapExpr.WellFormed (n : Nat) (e : CapExpr) : Prop :=
  e.caps.Nodup ∧ e.vars.Nodup ∧ ∀ v ∈ e.vars, v < n

instance (n : Nat) (e : CapExpr) : Decidable (e.WellFormed n) := by
  unfold CapExpr.WellFormed; infer_instance

/-- What a set expression contains once each variable `v` stands for the set `ρ v`. -/
def CapExpr.Mem (ρ : Nat → List Cap) (e : CapExpr) (c : Cap) : Prop :=
  c ∈ e.caps ∨ ∃ v ∈ e.vars, c ∈ ρ v

instance (ρ : Nat → List Cap) (e : CapExpr) (c : Cap) : Decidable (e.Mem ρ c) := by
  unfold CapExpr.Mem; infer_instance

/-- Inclusion of `e` in `f`, checked once for every value of the variables (rule 3): every
capability `e` names is named by `f`, and every variable of `e` is a variable of `f`, unless `f`
names every capability. `sub_iff` proves this is exactly "`e` ⊆ `f` for every value of the
variables" (Q5.3, semantic equality). -/
def CapExpr.Sub (e f : CapExpr) : Prop :=
  (∀ c ∈ e.caps, c ∈ f.caps) ∧ (∀ v ∈ e.vars, v ∈ f.vars ∨ ∀ c ∈ Cap.all, c ∈ f.caps)

instance (e f : CapExpr) : Decidable (e.Sub f) := by unfold CapExpr.Sub; infer_instance

/-- Two set expressions are the same set when each is included in the other (Q5.3: canonical
semantic equality). By `sub_iff` this is "equal for every value of the variables": the order and
grouping of the names do not matter. `CapExpr`'s derived `=` compares the written form only. -/
def CapExpr.Equiv (e f : CapExpr) : Prop := e.Sub f ∧ f.Sub e

instance (e f : CapExpr) : Decidable (e.Equiv f) := by unfold CapExpr.Equiv; infer_instance

/-- The callee's set `e` at a call whose explicit capability arguments are `σ`: each variable `v`
is replaced by the argument `σ[v]` (Q5.4, substitution checking). -/
def CapExpr.subst (σ : List CapExpr) (e : CapExpr) : CapExpr :=
  ⟨e.caps ++ e.vars.flatMap (fun v => (σ.getD v ⟨[], []⟩).caps),
   e.vars.flatMap (fun v => (σ.getD v ⟨[], []⟩).vars)⟩

/-- The grant types of the hosted target (row Q7.10, R4 F6) that this part uses. -/
inductive GrantTy where
  | stdin | stdout | stderr | directory | allocator | clock | environment | connect
  deriving DecidableEq, Repr

/-- The primitive operations of this part, each on one grant. -/
inductive Op where
  | readStdin | writeStdout | writeStderr | openDir | alloc | now | readEnv | connect
  deriving DecidableEq, Repr

/-- The primitive effect table (rule 7), first column: the grant type each operation acts on. -/
def Op.grant : Op → GrantTy
  | .readStdin => .stdin
  | .writeStdout => .stdout
  | .writeStderr => .stderr
  | .openDir => .directory
  | .alloc => .allocator
  | .now => .clock
  | .readEnv => .environment
  | .connect => .connect

/-- The primitive effect table (rule 7), second column: the capabilities each operation needs.
The standard streams need `Console` (Q7.11). -/
def Op.caps : Op → List Cap
  | .readStdin => [.console]
  | .writeStdout => [.console]
  | .writeStderr => [.console]
  | .openDir => [.fs]
  | .alloc => [.alloc]
  | .now => [.time]
  | .readEnv => [.env]
  | .connect => [.network]

/-- A statement of this part. -/
inductive Stmt where
  /-- Operation `op` on the function's grant parameter at position `g`. -/
  | prim (op : Op) (g : Nat)
  /-- A call of `f` with explicit capability arguments `caps`, one for each capability parameter
  of `f` (Q5.1), and grant arguments `gs`, positions of the caller's grant parameters. -/
  | call (f : String) (caps : List CapExpr) (gs : List Nat)
  deriving DecidableEq, Repr

/-- A function: its capability parameters (a count), its grant parameters, its permissions
`with(...)`, whether it is `trusted`, and its body. -/
structure Fn where
  name : String
  capParams : Nat
  grants : List GrantTy
  perms : CapExpr
  trusted : Bool
  body : List Stmt
  deriving DecidableEq, Repr

abbrev Prog := List Fn

/-- The function named `f`. -/
def Prog.find (P : Prog) (f : String) : Option Fn := P.find? (fun g => g.name == f)

/-- An operation `op` on `f`'s grant parameter `g` is permitted: the parameter has the operation's
grant type, and `f`'s permissions include every capability the table gives the operation (rules 1,
2, 7). -/
def PrimOk (f : Fn) (op : Op) (g : Nat) : Prop :=
  f.grants[g]? = some op.grant ∧ CapExpr.Sub ⟨op.caps, []⟩ f.perms

instance (f : Fn) (op : Op) (g : Nat) : Decidable (PrimOk f op g) := by
  unfold PrimOk; infer_instance

/-- The arguments of a call from `f` to `callee` are permitted: exactly one well-formed capability
argument per capability parameter (Q5.1: none is inferred), grant arguments of the callee's
grant types, and the callee's permissions, with the arguments substituted, included in `f`'s
(Q5.4, TRUST-4). -/
def ArgsOk (f callee : Fn) (caps : List CapExpr) (gs : List Nat) : Prop :=
  caps.length = callee.capParams ∧ (∀ e ∈ caps, e.WellFormed f.capParams) ∧
    gs.map (fun i => f.grants[i]?) = callee.grants.map some ∧
    (callee.perms.subst caps).Sub f.perms

instance (f callee : Fn) (caps : List CapExpr) (gs : List Nat) :
    Decidable (ArgsOk f callee caps gs) := by
  unfold ArgsOk; infer_instance

/-- A call of `h` from `f` is permitted: `h` names a function, and the arguments are permitted. -/
def CallOk (P : Prog) (f : Fn) (h : String) (caps : List CapExpr) (gs : List Nat) : Prop :=
  ∃ callee, P.find h = some callee ∧ ArgsOk f callee caps gs

instance (P : Prog) (f : Fn) (h : String) (caps : List CapExpr) (gs : List Nat) :
    Decidable (CallOk P f h caps gs) :=
  match hf : P.find h with
  | none => isFalse (fun ⟨_, hc, _⟩ => by rw [hf] at hc; cases hc)
  | some c =>
    if hok : ArgsOk f c caps gs then isTrue ⟨c, hf, hok⟩
    else isFalse (fun ⟨_, hc, h'⟩ => by rw [hf] at hc; cases hc; exact hok h')

/-- A statement of `f` is permitted. Whether `f` is `trusted` plays no part (rule 5), and
`Unsafe` in `f`'s permissions satisfies no other capability. -/
def StmtOk (P : Prog) (f : Fn) : Stmt → Prop
  | .prim op g => PrimOk f op g
  | .call h caps gs => CallOk P f h caps gs

instance (P : Prog) (f : Fn) : (s : Stmt) → Decidable (StmtOk P f s)
  | .prim op g => inferInstanceAs (Decidable (PrimOk f op g))
  | .call h caps gs => inferInstanceAs (Decidable (CallOk P f h caps gs))

/-- A function is permitted: its own set is well formed and every statement is permitted. -/
def FnOk (P : Prog) (f : Fn) : Prop := f.perms.WellFormed f.capParams ∧ ∀ s ∈ f.body, StmtOk P f s

instance (P : Prog) (f : Fn) : Decidable (FnOk P f) := by unfold FnOk; infer_instance

/-- A program is permitted: function names are distinct, every function is permitted, and `main`
exists with no capability variables (rule 12: it names its permissions concretely). -/
def ProgOk (P : Prog) : Prop :=
  (P.map Fn.name).Nodup ∧ (∀ f ∈ P, FnOk P f) ∧
    ∃ m, P.find "main" = some m ∧ m.capParams = 0

instance (P : Prog) : Decidable (ProgOk P) := by
  unfold ProgOk
  have : Decidable (∃ m, P.find "main" = some m ∧ m.capParams = 0) :=
    match h : P.find "main" with
    | some m => if h0 : m.capParams = 0 then isTrue ⟨m, rfl, h0⟩
        else isFalse (fun ⟨m', h', h0'⟩ => by cases h'; exact h0 h0')
    | none => isFalse (fun ⟨m', h', _⟩ => by cases h')
  infer_instance

/-- What an outside observer sees of this part: an operation on a grant. The launcher numbers the
grants it mints. -/
structure Event where
  op : Op
  grant : Nat
  deriving DecidableEq, Repr

/-- What a run has produced so far: the events in order, and whether the run is complete. A run
that is not complete stopped early: it ran out of fuel, or met a missing grant or callee (which a
permitted program never meets). Its events are still observable, so the containment theorem
covers them too: it needs no termination premise. -/
structure Trace where
  events : List Event
  complete : Bool
  deriving DecidableEq, Repr

/-- Run a body whose grant parameters are bound, by position, to the launch grants `env`. Fuel
bounds the depth of the run, not its length: each step takes one unit, and a call runs its callee
and then the rest of the body each with the remaining fuel. Capabilities have no run-time representation: the run never
consults them. -/
def exec (P : Prog) : Nat → List Nat → List Stmt → Trace
  | 0, _, _ => ⟨[], false⟩
  | _ + 1, _, [] => ⟨[], true⟩
  | n + 1, env, .prim op g :: rest =>
    match env[g]? with
    | none => ⟨[], false⟩
    | some id =>
      let t := exec P n env rest
      ⟨⟨op, id⟩ :: t.events, t.complete⟩
  | n + 1, env, .call h _ gs :: rest =>
    match P.find h, gs.mapM (fun i => env[i]?) with
    | some callee, some env' =>
      let t₁ := exec P n env' callee.body
      if t₁.complete then
        let t₂ := exec P n env rest
        ⟨t₁.events ++ t₂.events, t₂.complete⟩
      else t₁
    | _, _ => ⟨[], false⟩

/-- Run `main` on the grants `launch` the launcher minted for its parameters. -/
def runMain (P : Prog) (fuel : Nat) (launch : List Nat) : Trace :=
  match P.find "main" with
  | some m => exec P fuel launch m.body
  | none => ⟨[], false⟩

/-! ### Examples -/

/-- `with(Console)`. -/
def console : CapExpr := ⟨[.console], []⟩

/-- Accepted: writing to standard output needs the `StdOut` grant and `Console`.
```
fn log(out: &StdOut) with(Console) { write(out, "hi"); }
``` -/
def logFn : Fn :=
  { name := "log", capParams := 0, grants := [.stdout], perms := console, trusted := false,
    body := [.prim .writeStdout 0] }

example : FnOk [logFn] logFn := by decide

/-- Refused: the grant without the permission (rule 1). -/
example : ¬ FnOk [logFn] { logFn with perms := ⟨[], []⟩ } := by decide

/-- Refused: the permission without the grant (rule 2): `log` has no `StdIn`. -/
example : ¬ FnOk [logFn] { logFn with body := [.prim .readStdin 0] } := by decide

/-- Refused: `Unsafe` does not stand in for `Console`, and neither does `trusted` (rule 5). -/
example : ¬ FnOk [logFn] { logFn with perms := ⟨[.«unsafe»], []⟩, trusted := true } := by decide

/-- Refused: `Device` is not the standard streams' permission (Q7.11). -/
example : ¬ FnOk [logFn] { logFn with perms := ⟨[.device], []⟩ } := by decide

/-- Refused: a capability written twice (OD108). -/
example : ¬ FnOk [logFn] { logFn with perms := ⟨[.console, .console], []⟩ } := by decide

/-- A capability-polymorphic function: `C` is its capability variable `0`.
```
fn emit<cap C>(out: &StdOut) with(C, Console) { write(out, "x"); }
``` -/
def emitFn : Fn :=
  { name := "emit", capParams := 1, grants := [.stdout], perms := ⟨[.console], [0]⟩,
    trusted := false, body := [.prim .writeStdout 0] }

/-- `main` calls `emit` with the explicit capability argument `{}`.
```
fn main(out: StdOut) with(Console) -> u8 { emit<cap {}>(&out); log(&out); … }
``` -/
def mainFn : Fn :=
  { name := "main", capParams := 0, grants := [.stdout], perms := console, trusted := false,
    body := [.call "emit" [⟨[], []⟩] [0], .call "log" [] [0]] }

def prog : Prog := [mainFn, emitFn, logFn]

example : ProgOk prog := by decide

/-- Refused: `emit<cap {Fs}>` needs `Fs`, which `main` does not declare. -/
example : ¬ ProgOk [{ mainFn with body := [.call "emit" [⟨[.fs], []⟩] [0]] }, emitFn, logFn] := by
  decide

/-- Refused: a call without its capability argument; nothing is inferred (Q5.1). -/
example : ¬ ProgOk [{ mainFn with body := [.call "emit" [] [0]] }, emitFn, logFn] := by decide

/-- A function may forward its own capability variable (Q5.2): `relay<cap D>` calls
`emit<cap D>` and declares `D` and `Console`. -/
def relayFn : Fn :=
  { name := "relay", capParams := 1, grants := [.stdout], perms := ⟨[.console], [0]⟩,
    trusted := false, body := [.call "emit" [⟨[], [0]⟩] [0]] }

example : FnOk [emitFn, relayFn] relayFn := by decide

/-- Refused: `relay` may not drop `D` from its permissions while passing it on. -/
example : ¬ FnOk [emitFn, relayFn] { relayFn with perms := console } := by decide

/-- A run of `prog` on launch grant `7`: `emit` writes, then `log` writes, both to grant `7`. -/
example : runMain prog 10 [7] = ⟨[⟨.writeStdout, 7⟩, ⟨.writeStdout, 7⟩], true⟩ := by decide

/-- With too little fuel the run stops early, and the events so far are kept: `emit`'s write. -/
example : runMain prog 2 [7] = ⟨[⟨.writeStdout, 7⟩], false⟩ := by decide

/-- Set equality is semantic (Q5.3): the order of the names does not matter. -/
example : CapExpr.Equiv ⟨[.console, .fs], []⟩ ⟨[.fs, .console], []⟩ := by decide

/-- A variable is a set like any other: `with(C)` is not `with(C, Console)`. -/
example : ¬ CapExpr.Equiv ⟨[], [0]⟩ ⟨[.console], [0]⟩ := by decide

end ConcreteSpec.Authority

namespace ConcreteSpec.Observe
open ConcreteSpec

/-! ## Part 7. What the parent process observes (rows Q7.10, Q8.5; R4 F6)

The launcher of the hosted target turns the outcome of a run of slice 1 into what the parent
process observes (row Q7.10, R4 F6 "Startup and shutdown"):

* if a grant `main` asks for is not configured at launch, the launcher refuses to start, before
  `main` runs, with exit status 125;
* if `main` returns `code : u8`, the exit status is `code`;
* if the run traps, the runtime writes one line naming the trap and its source location to
  descriptor 2 (OD101's content) and the exit status is 134.

These effects belong to the target, not to source code: no function gains a permission from them
(authority rule 12). A status of 134 alone does not tell a trap from `return 134`; the trap
message does, and the observation records which trap occurred.

Not modelled here: the bytes of the trap line (OD101's content) and of the startup refusal, and
the trap's source location, which the decision requires in that line but which needs the
elaborator's source positions (not yet defined, README); the observation records only which trap
occurred. A run
that does not end (here: one that runs out of fuel) has no observation, and neither has a stuck
run, which a checked program never reaches (`checked_never_stuck`).
-/

/-- What the parent process observes. -/
inductive Observation where
  /-- A configured grant was missing: the launcher refused to start, before `main`. -/
  | startupRefused
  /-- `main` returned `code`. -/
  | exited (code : Nat)
  /-- The run trapped with `k`; the runtime wrote `k`'s message to descriptor 2. -/
  | trapped (k : Trap)
  deriving DecidableEq, Repr

/-- The exit status (Q7.10): 125 for a refused start, the returned code, or 134 for a trap. -/
def Observation.status : Observation → Nat
  | .startupRefused => 125
  | .exited code => code
  | .trapped _ => 134

/-- The launcher's map from a run's outcome to the observation. `configured` says that every grant
`main` asks for was configured at launch; if not, `main` never runs. -/
def observe (configured : Bool) (o : Outcome) : Option Observation :=
  if configured then
    match o with
    | .exit code => some (.exited code)
    | .trap k => some (.trapped k)
    | .stuck _ => none
    | .outOfFuel => none
  else some .startupRefused

/-! ### Examples -/

/-- `moveThenConsume` exits with status 42. -/
example : (observe true (run Examples.moveThenConsume 10)).map Observation.status = some 42 := by
  decide

/-- The accepted body `add100` traps on input 200 (it is not statically known): the trap message
goes to descriptor 2 and the status is 134. -/
example : observe true (runBody Examples.decls 10 (Examples.inputEnv 200) Examples.add100
    (.copy Examples.s₂)) = some (.trapped .arithmeticOverflow) := by decide

example : (Observation.trapped .arithmeticOverflow).status = 134 := rfl

/-- A missing configured grant: status 125, and `main` does not run. -/
example : (observe false (run Examples.moveThenConsume 10)).map Observation.status = some 125 := by
  decide

/-- The status of a trap and of `return 134` agree; the observations do not. -/
example : (Observation.trapped .arithmeticOverflow).status = (Observation.exited 134).status ∧
    Observation.trapped .arithmeticOverflow ≠ .exited 134 := by decide

end ConcreteSpec.Observe
