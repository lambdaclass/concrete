/-!
# Constructive Option and Except case-splitting tactics

Splits `match`, `if`, `bind`, `map` and `mapError` terms on `Option` and `Except` in goals
and hypotheses, then closes trivial or impossible branches using only constructive core
tactics (`contradiction`, `rfl`, `trivial`, `assumption`) and explicit `simp only` constructor
injectivity rules. Never invokes `Classical.choice`, `grind`, or default `simp` sets.
-/

namespace ConcreteSpec.Tactics

/-- Split every `Option` `match`, `bind`, `map` and `if` expression in the goal and hypotheses
(or at a specific hypothesis `h`), simplify `Option` constructor injectivity, and discharge
trivial or contradictory branches constructively (`propext`/`Quot.sound` only).

Optional definition names `[f, g]` are unfolded before splitting.

Example:
```lean
example (x : Option Nat) (y : Nat)
    (h : (do let a ← x; pure (a + 1)) = some y) : ∃ a, a + 1 = y := by
  crush_option at h
  exact ⟨_, h⟩
```
-/
syntax "crush_option" ("[" ident,* "]")? ("at " ident)? : tactic

macro_rules
  | `(tactic| crush_option) =>
    `(tactic| (
      try simp only [Option.bind_eq_bind, Option.pure_def, pure, bind,
        Option.bind_some, Option.bind_none, Option.map_some, Option.map_none] at *
      repeat' first
        | contradiction
        | rfl
        | trivial
        | ((try unfold Option.bind at *); (try unfold Option.map at *); split at *)
        | ((try unfold Option.bind); (try unfold Option.map); split)
        | simp only [Option.bind_eq_bind, Option.pure_def, pure, bind,
            Option.bind_some, Option.bind_none, Option.map_some, Option.map_none,
            Option.some.injEq, Bool.false_eq_true, Bool.true_eq_false,
            ↓reduceIte, ite_true, ite_false, dite_true, dite_false] at *
      all_goals try first | contradiction | rfl | trivial | assumption))
  | `(tactic| crush_option at $h:ident) =>
    `(tactic| (
      try simp only [Option.bind_eq_bind, Option.pure_def, pure, bind,
        Option.bind_some, Option.bind_none, Option.map_some, Option.map_none] at $h:term
      repeat' first
        | contradiction
        | rfl
        | trivial
        | (cases $h:term; done)
        | ((try unfold Option.bind at $h:term); (try unfold Option.map at $h:term); split at $h:term)
        | simp only [Option.bind_eq_bind, Option.pure_def, pure, bind,
            Option.bind_some, Option.bind_none, Option.map_some, Option.map_none,
            Option.some.injEq, Bool.false_eq_true, Bool.true_eq_false,
            ↓reduceIte, ite_true, ite_false, dite_true, dite_false] at $h:term
      all_goals try first | contradiction | rfl | trivial | assumption))
  | `(tactic| crush_option [$fs:ident,*]) =>
    `(tactic| (
      try unfold $fs:ident* at *
      try simp only [$[$fs:term],*] at *
      crush_option))
  | `(tactic| crush_option [$fs:ident,*] at $h:ident) =>
    `(tactic| (
      try unfold $fs:ident* at $h:term
      try simp only [$[$fs:term],*] at $h:term
      crush_option at $h))

/-- Split every `Except` `match`, `bind`, `map`, `mapError` and `if` expression in the goal
and hypotheses (or at a specific hypothesis `h`), simplify `Except` constructor injectivity,
and discharge trivial or contradictory branches constructively (`propext`/`Quot.sound` only).

Optional definition names `[f, g]` are unfolded before splitting.

Example:
```lean
example (x : Except String Nat) (y : Nat)
    (h : (do let a ← x; pure (a + 1)) = .ok y) : ∃ a, a + 1 = y := by
  crush_except at h
  exact ⟨_, h⟩
```
-/
syntax "crush_except" ("[" ident,* "]")? ("at " ident)? : tactic

macro_rules
  | `(tactic| crush_except) =>
    `(tactic| (
      try simp only [bind, pure, throw, throwThe, MonadExceptOf.throw,
        Except.instMonad, Except.pure, Option.bind_eq_bind, Option.pure_def,
        Option.bind_some, Option.bind_none, Option.map_some, Option.map_none] at *
      repeat' first
        | contradiction
        | rfl
        | trivial
        | ((try unfold Except.bind at *); (try unfold Except.map at *);
            (try unfold Except.mapError at *); (try unfold Option.bind at *);
            (try unfold Option.map at *); split at *)
        | ((try unfold Except.bind); (try unfold Except.map);
            (try unfold Except.mapError); (try unfold Option.bind);
            (try unfold Option.map); split)
        | simp only [bind, pure, throw, throwThe, MonadExceptOf.throw,
            Except.instMonad, Except.pure,
            Except.ok.injEq, Except.error.injEq, Option.some.injEq,
            Option.bind_some, Option.bind_none, Option.map_some, Option.map_none,
            Bool.false_eq_true, Bool.true_eq_false,
            ↓reduceIte, ite_true, ite_false, dite_true, dite_false] at *
      all_goals try first | contradiction | rfl | trivial | assumption))
  | `(tactic| crush_except at $h:ident) =>
    `(tactic| (
      try simp only [bind, pure, throw, throwThe, MonadExceptOf.throw,
        Except.instMonad, Except.pure, Option.bind_eq_bind, Option.pure_def,
        Option.bind_some, Option.bind_none, Option.map_some, Option.map_none] at $h:term
      repeat' first
        | contradiction
        | rfl
        | trivial
        | (cases $h:term; done)
        | ((try unfold Except.bind at $h:term); (try unfold Except.map at $h:term);
            (try unfold Except.mapError at $h:term); (try unfold Option.bind at $h:term);
            (try unfold Option.map at $h:term); split at $h:term)
        | simp only [bind, pure, throw, throwThe, MonadExceptOf.throw,
            Except.instMonad, Except.pure,
            Except.ok.injEq, Except.error.injEq, Option.some.injEq,
            Option.bind_some, Option.bind_none, Option.map_some, Option.map_none,
            Bool.false_eq_true, Bool.true_eq_false,
            ↓reduceIte, ite_true, ite_false, dite_true, dite_false] at $h:term
      all_goals try first | contradiction | rfl | trivial | assumption))
  | `(tactic| crush_except [$fs:ident,*]) =>
    `(tactic| (
      try unfold $fs:ident* at *
      try simp only [$[$fs:term],*] at *
      crush_except))
  | `(tactic| crush_except [$fs:ident,*] at $h:ident) =>
    `(tactic| (
      try unfold $fs:ident* at $h:term
      try simp only [$[$fs:term],*] at $h:term
      crush_except at $h))

end ConcreteSpec.Tactics
