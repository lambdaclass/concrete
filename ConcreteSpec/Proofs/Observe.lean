import ConcreteSpec.Spec.Basics
import ConcreteSpec.Proofs.Slice1
import ConcreteSpec.Spec.Observation
import ConcreteSpec.Spec.Slice1Ownership
import ConcreteSpec.Spec.Slice1Semantics
import ConcreteSpec.Spec.Slice1Syntax
import ConcreteSpec.Spec.Statics

/-!
# Part 7: `checked_observation`

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). See `ConcreteSpec/README.md`.
-/

namespace ConcreteSpec

/-- From a described environment, a checked body that exits exits with a `u8` code. -/
theorem runBody_exit_u8 {Δ : Decls} {Γ : Ctx} {ss : List Stmt} {r : Atom} (hb : BodyOk Δ Γ ss r) :
    ∀ {env : Env}, Corr Δ Γ env → ∀ n c, runBody Δ n env ss r = .exit c → c < 256 := by
  induction hb with
  | ret ht hu _ =>
    intro env hc n c h
    cases n with
    | zero => simp [runBody] at h
    | succ n =>
      simp only [runBody, retOutcome] at h
      cases hu with
      | atom hu =>
        obtain ⟨v, env', he, hv, _⟩ := evalAtom_safe hc (hasType_atom_scalar ht) hu
        cases hv with
        | int hr =>
          simp [he] at h
          subst h
          simp [IntTy.inRange, IntTy.min, IntTy.max, IntTy.signed, IntTy.bits] at hr
          omega
  | cons hs _ ih =>
    intro env hc n c h
    cases n with
    | zero => simp [runBody] at h
    | succ n =>
      simp only [runBody] at h
      rcases execStmt_safe hc hs with ⟨k, hk⟩ | ⟨env', he, hc'⟩
      · simp [hk] at h
      · simp only [he] at h; exact ih hc' n c h

end ConcreteSpec

namespace ConcreteSpec.Observe
open ConcreteSpec

/-- **What a checked program shows its parent.** With any fuel, the run of a checked slice-1
program either has not ended yet, or the parent observes an exit with the `u8` code `main`
returned, or a trap (status 134 and the trap's message). It is never stuck. -/
theorem checked_observation {p : Program} (h : Checked p) (n : Nat) :
    run p n = .outOfFuel ∨ (∃ c, c < 256 ∧ observe true (run p n) = some (.exited c)) ∨
      ∃ k, observe true (run p n) = some (.trapped k) := by
  cases ho : run p n with
  | outOfFuel => exact .inl rfl
  | exit c =>
    have hc : c < 256 := runBody_exit_u8 h.2 .nil n c ho
    exact .inr (.inl ⟨c, hc, by simp [observe]⟩)
  | trap k => exact .inr (.inr ⟨k, by simp [observe]⟩)
  | stuck w => exact absurd ho (checked_never_stuck h n w)

/-- With enough fuel (one unit per statement and one for `return`), a checked program's run ends,
and the parent observes a status below 256 that is the returned code, or 134 for a trap. -/
theorem checked_observable {p : Program} (h : Checked p) :
    ∃ obs, observe true (run p (p.body.length + 1)) = some obs ∧ obs.status < 256 := by
  rcases checked_observation h (p.body.length + 1) with hf | ⟨c, hc, ho⟩ | ⟨k, ho⟩
  · exact absurd hf (runBody_enough p.body [])
  · exact ⟨_, ho, hc⟩
  · exact ⟨_, ho, by simp [Observation.status]⟩

/-- A refused start never runs `main`: the observation does not depend on the run. -/
theorem refused_start (o : Outcome) : observe false o = some .startupRefused := rfl

end ConcreteSpec.Observe
