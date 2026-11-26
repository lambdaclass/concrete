import ConcreteSpec.Spec.Slice1Syntax

/-! Canonical A-normal syntax of the first slice, shared by the integrated Core bridge. -/
namespace ConcreteSpec.Slice0

/-- A slice-0 atom: a decimal `u8` literal (a natural number; whether it fits `u8` is checked,
ARITH-2) or a read of a local. -/
inductive Atom where
  | lit (n : Nat)
  | var (x : Var)
  deriving DecidableEq

/-- A slice-0 expression: an atom, or checked `l + r` of two atoms (ARITH-3). -/
inductive Expr where
  | atom (a : Atom)
  | add (l r : Atom)
  deriving DecidableEq

/-- `let x: u8 = e;` — the only slice-0 statement. -/
structure Stmt where
  x : Var
  e : Expr
  deriving DecidableEq

/-- An atom in Core: a literal is `u8`, a local is read with `copy` (`u8` is Copy). -/
def Atom.toCore : Atom → ConcreteSpec.Atom
  | .lit n => .int .u8 n
  | .var x => .copy x

def Expr.toCore : Expr → ConcreteSpec.Expr
  | .atom a => .atom a.toCore
  | .add l r => .add l.toCore r.toCore

def Stmt.toCore (s : Stmt) : ConcreteSpec.Stmt := .let_ (.name s.x) (.scalar (.int .u8)) s.e.toCore

end ConcreteSpec.Slice0
