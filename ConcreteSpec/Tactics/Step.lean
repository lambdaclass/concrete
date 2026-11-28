/-!
# Constructive step-inversion and machine-unfolding tactics

Inverts operational step/evaluation hypotheses into their constructor cases with equations
substituted (`invert_step`), and unfolds one step of a named transition machine while
normalizing `match` and `if` on known constructors (`unfold_step`). Elaborates purely
constructively (`propext`/`Quot.sound` only).
-/

namespace ConcreteSpec.Tactics

/-- Invert an inductive step or evaluation hypothesis `h` into its constructor cases,
substitute all generated equations with `subst_vars`, simplify constructor injectivity,
and close impossible or trivial branches constructively.

With `with [f, g]`, first unfolds `f, g` at `h` before inverting.

Example:
```lean
inductive MiniStep : Nat → Nat → Prop where
  | inc (n : Nat) : MiniStep n (n + 1)

example (a b : Nat) (h : MiniStep a b) : a ≤ b := by
  invert_step h
  exact Nat.le_succ _
```
-/
syntax "invert_step " ident ("with " "[" ident,* "]")? : tactic

macro_rules
  | `(tactic| invert_step $h:ident) =>
    `(tactic| (
      first
        | cases $h:term
        | (injection $h:term with h_inj; all_goals try injection h_inj)
      all_goals try subst_vars
      all_goals try simp only [Option.some.injEq, Except.ok.injEq, Except.error.injEq,
        Prod.mk.injEq, List.cons.injEq, and_self, true_and, and_true] at *
      all_goals try subst_vars
      all_goals try first | contradiction | rfl | trivial | assumption))
  | `(tactic| invert_step $h:ident with [$fs:ident,*]) =>
    `(tactic| (
      try unfold $fs:ident* at $h:term
      try simp only [$[$fs:term],*, Option.bind_some, Option.bind_none, Option.map_some, Option.map_none,
        Option.some.injEq, Except.ok.injEq, Except.error.injEq,
        Bool.false_eq_true, Bool.true_eq_false,
        ↓reduceIte, ite_true, ite_false, dite_true, dite_false] at $h:term
      invert_step $h))

/-- Unfold one step of one or more named machine definitions (`unfold_step f, g`) in the goal,
at a hypothesis (`at h`), or everywhere (`at *`), and normalize `match` and `if` reductions
on known constructors constructively. Closes the goal automatically if it reduces to `rfl`
or `contradiction`.

Example:
```lean
def miniMachine : Option Nat → Option Nat
  | none => none
  | some n => some (n + 1)

example (n : Nat) : miniMachine (some n) = some (n + 1) := by
  unfold_step miniMachine
```
-/
syntax "unfold_step " ident,+ ("at " (ident <|> "*"))? : tactic

macro_rules
  | `(tactic| unfold_step $fs:ident,*) =>
    `(tactic| (
      unfold $fs:ident*
      try dsimp only
      try simp only [Option.bind_eq_bind, Option.pure_def, pure, bind,
        Option.bind_some, Option.bind_none, Option.map_some, Option.map_none,
        Option.some.injEq, Except.ok.injEq, Except.error.injEq,
        Bool.false_eq_true, Bool.true_eq_false,
        ↓reduceIte, ite_true, ite_false, dite_true, dite_false]
      try first | rfl | contradiction | trivial))
  | `(tactic| unfold_step $fs:ident,* at $h:ident) =>
    `(tactic| (
      unfold $fs:ident* at $h:term
      try dsimp only at $h:term
      try simp only [Option.bind_eq_bind, Option.pure_def, pure, bind,
        Option.bind_some, Option.bind_none, Option.map_some, Option.map_none,
        Option.some.injEq, Except.ok.injEq, Except.error.injEq,
        Bool.false_eq_true, Bool.true_eq_false,
        ↓reduceIte, ite_true, ite_false, dite_true, dite_false] at $h:term
      try first | contradiction | rfl | trivial))
  | `(tactic| unfold_step $fs:ident,* at *) =>
    `(tactic| (
      unfold $fs:ident* at *
      try dsimp only at *
      try simp only [Option.bind_eq_bind, Option.pure_def, pure, bind,
        Option.bind_some, Option.bind_none, Option.map_some, Option.map_none,
        Option.some.injEq, Except.ok.injEq, Except.error.injEq,
        Bool.false_eq_true, Bool.true_eq_false,
        ↓reduceIte, ite_true, ite_false, dite_true, dite_false] at *
      try first | contradiction | rfl | trivial))

end ConcreteSpec.Tactics
