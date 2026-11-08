import ConcreteSpec.Spec.Ident

/-!
# Traps and the ways a run stops

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). Part of `ConcreteSpec.Spec`;
see `ConcreteSpec/README.md`. Foundation contracts C1 and C2 (the foundation proposal approved by
the owner on 2026-11-08, "ok all recommended").

A run of a Core program can stop short of completing in two ways that the specification keeps
apart (row Q6.7):

- a **trap**: a terminal, observable stop with a kind and a site. The program's own `abort()` is a
  trap, and so is a failing operation in code admitted without the evidence that excludes it
  (trusted code; safe code never reaches one, by the no-hidden-traps decision of 2026-11-06). A trap
  runs no cleanup and does not unwind (C1).
- **unsafe stuckness**: no rule applies. It has no meaning and is never observed: a safe program
  never reaches it (that is what safety admission proves), and reaching it in trusted code means a
  trusted premise was broken, which yields no claim at all, never a fabricated value.

Two further outcomes are *not* stops of the program, and are not here: an allocation failure is an
ordinary typed result that returns the rejected input (rows Q1.8, Q1.9, C4), and running out of
evaluator fuel (C8) is a report about the simulation budget, not about the program.

The trap kinds are diagnostic categories (owner decision 2026-11-08, item 4): they name why a run
trapped. They are a different type from the error values a checked operation returns
(`ArithmeticError`, `OutOfRange`, C2), which are ordinary values a program handles.
-/

-- Results of operations that can fail (`Except`) are compared in the examples.
deriving instance DecidableEq for Except

namespace ConcreteSpec.Core

/-- Why a run trapped (owner decision 2026-11-08, item 4). The first three are the failures of a
plain arithmetic operation, `outOfBounds` an index outside its sequence, `invalidRange` a range
whose bounds are out of order or outside the sequence, and `abort` the program's own `abort()`.
A new kind is added only by an owner decision. -/
inductive TrapKind where
  | overflow
  | divisionByZero
  | invalidShift
  | outOfBounds
  | invalidRange
  | abort
  deriving DecidableEq, Repr

/-- The name of a trap kind, as the trap line prints it (owner decision 2026-11-08, item 4). How the
whole line is rendered is C6 (`concrete: trap: <kind> at <path>:<line>:<col> (site <id>)`). -/
def TrapKind.name : TrapKind → String
  | .overflow => "overflow"
  | .divisionByZero => "division_by_zero"
  | .invalidShift => "invalid_shift"
  | .outOfBounds => "out_of_bounds"
  | .invalidRange => "invalid_range"
  | .abort => "abort"

/-- Every trap kind, once each. -/
def TrapKind.all : List TrapKind :=
  [.overflow, .divisionByZero, .invalidShift, .outOfBounds, .invalidRange, .abort]

/-- The result of one step of evaluating an operand: a value, a trap of some kind, or unsafe
stuckness (row Q6.7). An operand is evaluated without effects (Core is in A-normal form, Q9.1),
so these are its only results. -/
inductive Res (V : Type) where
  | ok (v : V)
  | trap (k : TrapKind)
  | stuck
  deriving DecidableEq, Repr

namespace Res

variable {V W : Type}

/-- Continue with the value, keeping a trap or stuckness. -/
def bind : Res V → (V → Res W) → Res W
  | .ok v, f => f v
  | .trap k, _ => .trap k
  | .stuck, _ => .stuck

/-- Apply a function to the value. -/
def map (f : V → W) (r : Res V) : Res W := r.bind (fun v => .ok (f v))

end Res

/-! ### Examples -/

/-- The names are the decided ones. -/
example : TrapKind.all.map TrapKind.name =
    ["overflow", "division_by_zero", "invalid_shift", "out_of_bounds", "invalid_range", "abort"] :=
  rfl

/-- A trap passes through `bind`; the continuation never runs. -/
example : (Res.trap .overflow : Res Nat).bind (fun n => .ok (n + 1)) = .trap .overflow := rfl

end ConcreteSpec.Core
