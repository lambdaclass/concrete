import ConcreteSpec.Spec.LL1

namespace Concrete.LL1

namespace Examples

inductive Tm where
  | num | plus | lparen | rparen
  deriving DecidableEq, Repr

inductive Nt where
  | expr | rest | atom
  deriving DecidableEq, Repr

open Sym in
/-- `expr → atom rest`, `rest → + atom rest | ε`, `atom → num | ( expr )`: LL(1). -/
def sums : Grammar Tm Nt :=
  { start := .expr,
    prods := [(.expr, [n .atom, n .rest]),
              (.rest, [t .plus, n .atom, n .rest]), (.rest, []),
              (.atom, [t .num]), (.atom, [t .lparen, n .expr, t .rparen])] }

example : ll1 sums = true := by decide

open Sym in
/-- `expr → atom + expr | atom`: both alternatives start with `atom`, so not LL(1). -/
def unfactored : Grammar Tm Nt :=
  { start := .expr,
    prods := [(.expr, [n .atom, t .plus, n .expr]), (.expr, [n .atom]),
              (.atom, [t .num]), (.atom, [t .lparen, n .expr, t .rparen])] }

theorem unfactored_rejected : ll1 unfactored = false := by decide

open Sym in
/-- A FIRST/FOLLOW conflict: `rest → + atom | ε` where `+` can also follow `rest`. -/
def followConflict : Grammar Tm Nt :=
  { start := .expr,
    prods := [(.expr, [n .rest, t .plus, n .atom]),
              (.rest, [t .plus, n .atom]), (.rest, []),
              (.atom, [t .num])] }

theorem followConflict_rejected : ll1 followConflict = false := by decide

end Examples

end Concrete.LL1
