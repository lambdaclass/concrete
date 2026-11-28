/-!
# Constructive Boolean and `if`-splitting tactics

Addresses two high-frequency proof patterns across `ConcreteSpec/Proofs/` and `Concrete/`:
Boolean guard normalization (`Bool.and_eq_true`, `beq_iff_eq`, `decide_eq_true_eq`, 75+ sites)
and `if`/`dite` case splitting (`ite_eq_left`/`ite_eq_right`/`↓reduceIte`, 390+ sites).
Elaborates purely constructively (`propext`/`Quot.sound` only).
-/

namespace ConcreteSpec.Tactics

/-- Normalize Boolean connectives (`&&`, `||`, `!`, `==`, `!=`, `decide`) constructively
in the goal and hypotheses (or at a specific hypothesis `h`), and close trivial or
contradictory branches.

Example:
```lean
example (a b : Bool) (h : (a && b) = true) : a = true ∧ b = true := by
  crush_bool at h
```
-/
syntax "crush_bool" ("at " ident)? : tactic

macro_rules
  | `(tactic| crush_bool) =>
    `(tactic| (
      try simp only [Bool.and_eq_true, Bool.or_eq_true, Bool.not_eq_true',
        Bool.not_eq_false', Bool.false_eq_true, Bool.true_eq_false,
        beq_iff_eq, bne_iff_ne, decide_eq_true_eq, decide_eq_false_iff_not,
        not_false_eq_true, not_true_eq_false, and_true, true_and] at *
      all_goals try first | contradiction | rfl | trivial | assumption))
  | `(tactic| crush_bool at $h:ident) =>
    `(tactic| (
      try simp only [Bool.and_eq_true, Bool.or_eq_true, Bool.not_eq_true',
        Bool.not_eq_false', Bool.false_eq_true, Bool.true_eq_false,
        beq_iff_eq, bne_iff_ne, decide_eq_true_eq, decide_eq_false_iff_not,
        not_false_eq_true, not_true_eq_false, and_true, true_and] at $h:term
      all_goals try first | contradiction | rfl | trivial | assumption))

/-- Split `if`/`dite` expressions in the goal and hypotheses (or at a specific hypothesis `h`),
reduce known branches constructively (`↓reduceIte`, `ite_true`, `ite_false`, `dite_true`,
`dite_false`), and close trivial or contradictory branches.

Optional definition names `[f, g]` are unfolded before splitting.

Example:
```lean
example (b : Bool) (x : Nat)
    (h : (if b = true then some x else none) = some 3) : b = true := by
  crush_ite at h
```
-/
syntax "crush_ite" ("[" ident,* "]")? ("at " ident)? : tactic

macro_rules
  | `(tactic| crush_ite) =>
    `(tactic| (
      repeat' first
        | contradiction
        | rfl
        | trivial
        | split at *
        | split
        | simp only [Bool.false_eq_true, Bool.true_eq_false,
            ↓reduceIte, ite_true, ite_false, dite_true, dite_false] at *
      all_goals try first | contradiction | rfl | trivial | assumption))
  | `(tactic| crush_ite at $h:ident) =>
    `(tactic| (
      repeat' first
        | contradiction
        | rfl
        | trivial
        | split at $h:term
        | simp only [Bool.false_eq_true, Bool.true_eq_false,
            ↓reduceIte, ite_true, ite_false, dite_true, dite_false] at $h:term
      all_goals try first | contradiction | rfl | trivial | assumption))
  | `(tactic| crush_ite [$fs:ident,*]) =>
    `(tactic| (
      try unfold $fs:ident* at *
      try simp only [$[$fs:term],*] at *
      crush_ite))
  | `(tactic| crush_ite [$fs:ident,*] at $h:ident) =>
    `(tactic| (
      try unfold $fs:ident* at $h:term
      try simp only [$[$fs:term],*] at $h:term
      crush_ite at $h))

end ConcreteSpec.Tactics
