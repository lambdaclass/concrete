import ConcreteSpec.Spec.Basics

/-!
# Types and slice-1 syntax

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). Part of `ConcreteSpec.Spec`;
see `ConcreteSpec/README.md`.

## Slice 1

What slice 1 contains:

* the scalar types: the eight integer types (TYPE-19) and `bool`;
* `let` with a type, a struct literal, and a struct-destructuring `let` (PAT-5);
* struct declarations, `Copy` or not (TYPE-14), with scalar fields; the slice's central case is
  an empty struct without `Copy`, moved and then consumed by a destructuring `let`;
* the use-after-move refusal (OWN-1), consumption before exit (OWN-2) and used bindings (OWN-5);
* one checked operation, integer `+` (ARITH-3), which traps on overflow;
* evidence-required admission (no hidden traps, decision of 2026-11-06, Q1): `+` is accepted only
  when the facts the checker keeps (known values, Q6.2, and type bounds) prove the sum fits; every
  other `+` is refused, so an accepted program never traps (`checked_no_implicit_traps`). This
  subsumes the refusal of proven violations (Q6.1, Q6.4; ARITH-13, OD118);
* one function, `main`, with no parameters, whose body is a list of `let` statements followed by
  `return a` with `a : u8` (STMT-7, STMT-9).

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
one-level instance here. An input is not known, only bounded by its type: `add100` and
`partialUnknownRefused` trap for some input values and are refused (no hidden traps, decision of
2026-11-06), and `add0`, whose sum fits for every input, is accepted. The table has more entries (module constants, the
six named wrapping/saturating operations, lossless `as`) that slice 1 has no syntax for. The
refusal applies to all written code, including trusted and unreachable code (Q6.4); slice 1 has
neither, so its examples show only reachable positions and a discarded one (`overflowDropped`).
-/

namespace ConcreteSpec

/-- A declaration name: a struct or a field. Declaration names are nominal (Q9.2). -/
structure Name where
  text : String
  deriving DecidableEq

-- Keep diagnostic rendering identical to the previous String representation.
instance : Repr Name where
  reprPrec name prec := reprPrec name.text prec

instance : Coe String Name := ⟨Name.mk⟩

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

/-- A slice-1 program: struct declarations and `fn main() -> u8 { body; return ret; }` (STMT-7:
`main` returns `u8` and may declare no grants; STMT-9: the body ends in `return`). In A-normal form
`return` takes an atom. -/
structure Program where
  decls : Decls
  body : List Stmt
  ret : Atom



end ConcreteSpec
