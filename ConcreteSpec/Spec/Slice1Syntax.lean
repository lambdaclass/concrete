import ConcreteSpec.Spec.Basics
import ConcreteSpec.Spec.Ident

/-!
# Types and slice-1 syntax

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). Part of `ConcreteSpec.Spec`;
see `ConcreteSpec/README.md`.

## Slice 1 and the wider Core

Surface1 admits the slice-1 subset of this Core: u8, checked addition, and nominal empty
non-Copy structs. It maps typed lets, explicit moves, consuming destructuring and the final
return or abort to the definitions below. The wider Core also models other scalar types and
structs with fields for the foundation proofs.

Core's binding form (Q9.1):

* **A-normal form.** Every operand is an atom: a literal, `copy x` or `move x`. `x + y` is an
  expression whose operands are atoms; a nested surface expression becomes a sequence of `let`s
  of temporaries, and `return` takes an atom.
* **Fresh identities.** A local is a `Var`, a numeric identity (source spellings live in `LocalNames`) that is
  bound at most once. Surface names, shadowing and the namespace rules (SCOPE-1, SCOPE-2) are
  elaboration rules that map names to identities; Core only requires each identity to be fresh.
* **Explicit moves and copies.** Reading a local is written `copy x` (its type is Copy) or
  `move x` (its type is linear). There is no read whose effect depends on the type. One read form
  per type (owner-confirmed 2026-11-05): `copy` is allowed only on a Copy type and `move` only on a
  linear one; `move` of a Copy value and `copy` of a linear value are both refused.

The declarative judgments are independent of the executable checker (`check`).
`check_iff` proves agreement. The fuel-indexed evaluator (`run`) separates return, written
abort, implicit trap, stuckness and fuel exhaustion. `Exec` states the same execution without
a fuel index.

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
gets the same fact as a source binding of the same expression. The wider Core has scalar fields,
so these field facts have one level. An input is bounded by its type: `add100` and
`partialUnknownRefused` trap for some input values and are refused (no hidden traps, decision of
2026-11-06), and `add0`, whose sum fits for every input, is accepted. The table has more entries (module constants, the
six named wrapping/saturating operations, lossless `as`) that slice 1 has no syntax for. The
surface rejects statements after a terminal action. The wider Core examples also cover discarded
results (`overflowDropped`).
-/

namespace ConcreteSpec

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
  deriving DecidableEq

/-- An expression in A-normal form (Q9.1): an atom, checked integer addition `l + r` of two atoms
(ARITH-3), or a struct literal `S { f: a, ... }` whose fields are atoms (TYPE-7). -/
inductive Expr where
  | atom (a : Atom)
  | add (l r : Atom)
  | mk (s : Name) (fields : List (Name × Atom))
  deriving DecidableEq

/-- A destructuring pattern binds a field to a local, or ignores it with `_` (PAT-1, PAT-5). -/
inductive Binder where
  | name (x : Var)
  | wild
  deriving DecidableEq

/-- A statement: `let x: T = e;`, `let _: T = e;`, or a destructuring `let S { f: b, ... } = e;`
(PAT-5). -/
inductive Stmt where
  | let_ (b : Binder) (t : Ty) (e : Expr)
  | letStruct (s : Name) (pat : List (Name × Binder)) (e : Expr)
  deriving DecidableEq

/-- A straight-line body's final action. Abort takes its written site and performs no reads. -/
inductive Terminator where
  | ret (value : Atom)
  | abort (site : Core.SiteId)
  deriving DecidableEq

/-- Core for one straight-line main. The body contains bindings and ends in return or abort. -/
structure Program where
  decls : Decls
  body : List Stmt
  ending : Terminator



end ConcreteSpec
