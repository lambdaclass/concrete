/-!
# Constructive structure-update frame tactic

Closes goals asserting that a structure update (or conditional / chained structure update,
or a conjunction / frame record of such equalities) leaves unchanged fields untouched.
Elaborates without any axioms beyond `propext`/`Quot.sound` (typically zero axioms).
-/

namespace ConcreteSpec.Tactics

/-- Close a goal proving that a structure update leaves one or more unrelated fields unchanged,
including nested updates, conditional updates (`if ... then { s with ... } else s`),
conjunctions of field equalities, and named frame structures.

Optional helper names `[f, g]` are unfolded before closing. Fails if any subgoal remains open.

Example:
```lean
structure Regs where
  pc : Nat
  flags : Bool

example (r : Regs) (n : Nat) : ({ r with pc := n }).flags = r.flags := by
  frame_auto
```
-/
syntax "frame_auto" ("[" ident,* "]")? : tactic

macro_rules
  | `(tactic| frame_auto) =>
    `(tactic| (
      try dsimp only
      try subst_vars
      repeat' first
        | rfl
        | assumption
        | exact Eq.symm (by assumption)
        | contradiction
        | (constructor; all_goals try dsimp only)
        | (split; all_goals try dsimp only)
      done))
  | `(tactic| frame_auto [$fs:ident,*]) =>
    `(tactic| (
      try unfold $fs:ident*
      try dsimp only [$[$fs:term],*]
      frame_auto))

end ConcreteSpec.Tactics
