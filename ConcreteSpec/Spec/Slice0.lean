import ConcreteSpec.Spec.Examples

/-!
# The slice-0 fragment

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). Part of `ConcreteSpec.Spec`;
see `ConcreteSpec/README.md`.
-/

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

def a₀ : Var := ⟨"a", 0⟩
def b₁ : Var := ⟨"b", 1⟩

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
