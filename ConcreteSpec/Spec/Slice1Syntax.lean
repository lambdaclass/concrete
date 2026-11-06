/-!
# Types and slice-1 syntax

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). Part of `ConcreteSpec.Spec`;
see `ConcreteSpec/README.md`.
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


end ConcreteSpec
