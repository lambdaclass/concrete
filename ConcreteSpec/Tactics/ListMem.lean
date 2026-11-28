/-!
# Constructive list-membership tactics

Addresses list-membership case splitting (`List.mem_cons`, 270+ sites across
`ConcreteSpec/Proofs/` and `Concrete/`) and lifting universal quantifiers across cons cells
(`∀ x ∈ a :: xs, P x` from `∀ x ∈ xs, P x`). Elaborates purely constructively
(`propext`/`Quot.sound` only).
-/

namespace ConcreteSpec.Tactics

/-- Simplify and split a list-membership hypothesis `h` (`x ∈ a :: xs`, `x ∈ []`,
`x ∈ xs ++ ys`, `x ∈ [a]`), substitute equalities arising from head matches, and discharge
trivial or contradictory branches constructively.

Example:
```lean
example (x a : Nat) (xs : List Nat)
    (h : x ∈ a :: xs) (hne : x ≠ a) : x ∈ xs := by
  crush_mem at h
```
-/
syntax "crush_mem" ("at " ident)? : tactic

macro_rules
  | `(tactic| crush_mem) =>
    `(tactic| (
      try simp only [List.mem_cons, List.mem_nil_iff, List.mem_append, List.mem_singleton,
        or_false, false_or] at *
      all_goals try first | contradiction | rfl | trivial | assumption))
  | `(tactic| crush_mem at $h:ident) =>
    `(tactic| (
      simp only [List.mem_cons, List.mem_nil_iff, List.mem_append, List.mem_singleton,
        or_false, false_or] at $h:term
      try cases $h:term
      all_goals try (rename_i h_mem_eq; simp only [Prod.mk.injEq] at h_mem_eq; cases h_mem_eq)
      all_goals try subst_vars
      all_goals try first | contradiction | rfl | trivial | assumption))

/-- Discharge a universal membership goal of the form `∀ x ∈ a :: xs, P x` (or an implication
`(∀ x ∈ xs, P x) → ∀ x ∈ a :: xs, P x`) by splitting `x ∈ a :: xs` into head and tail cases,
closing the tail case from an existing `∀ x ∈ xs, P x` hypothesis and trying `trivial`, `rfl`,
or `assumption` on the head case.

Example:
```lean
example (P : Nat → Prop) (a : Nat) (xs : List Nat)
    (ha : P a) (hxs : ∀ x ∈ xs, P x) : ∀ x ∈ a :: xs, P x := by
  forall_cons
```
-/
syntax "forall_cons" : tactic

macro_rules
  | `(tactic| forall_cons) =>
    `(tactic| (
      first
        | (intro htail x hx
           cases hx with
           | head => try first | trivial | rfl | assumption
           | tail _ hmem => exact htail _ hmem)
        | (intro x hx
           cases hx with
           | head => try first | trivial | rfl | assumption
           | tail _ hmem =>
             first
               | exact ‹∀ _ ∈ _, _› _ hmem
               | assumption)))

end ConcreteSpec.Tactics
