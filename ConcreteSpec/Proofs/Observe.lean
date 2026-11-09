import ConcreteSpec.Proofs.Slice1

/-!
# Part 7: `checked_observation`

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). See `ConcreteSpec/README.md`.
-/

namespace ConcreteSpec

/-- The result invariant required of normal hosted exits; traps and unfinished runs carry no status. -/
def ExitU8 : Outcome → Prop
  | .exit c => c < 256
  | _ => True

/-- Law (subject `ConcreteSpec.runBody`). From a described environment, every normal exit of
an accepted body has a status representable by the return type, an unsigned byte. -/
theorem runBody_exit_u8 {Δ : Decls} {Γ : Ctx} {ss : List Stmt} {r : Atom} (hb : BodyOk Δ Γ ss r)
    {env : Env} (hc : Corr Δ Γ env) (n : Nat) : ExitU8 (runBody Δ n env ss r) := by
  have bound : ∀ {env : Env}, Corr Δ Γ env → ∀ n c,
      runBody Δ n env ss r = .exit c → c < 256 := by
    clear hc n env
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
  cases ho : runBody Δ n env ss r with
  | exit c => exact bound hc n c ho
  | trap => trivial
  | stuck => trivial
  | outOfFuel => trivial

namespace ExitU8Evidence

def wideExit : Decls → Nat → Env → List Stmt → Atom → Outcome := fun _ _ _ _ _ => .exit 300

/-- Instance (law `ConcreteSpec.runBody_exit_u8`, case `literal`). -/
example : ExitU8 (runBody [] 1 [] [] (.int .u8 42)) :=
  @runBody_exit_u8 [] [] [] (.int .u8 42) (.ret (.int (by decide)) (.atom .int) (by decide)) [] .nil 1

/-- Planted (law `ConcreteSpec.runBody_exit_u8`, case `literal`, subject `ConcreteSpec.runBody`, mutant `ConcreteSpec.ExitU8Evidence.wideExit`). -/
example : BodyOk [] [] [] (.int .u8 42) ∧ Corr [] [] [] ∧
    ¬ ExitU8 (wideExit [] 1 [] [] (.int .u8 42)) :=
  ⟨.ret (.int (by decide)) (.atom .int) (by decide), .nil, by unfold wideExit ExitU8; decide⟩

end ExitU8Evidence

end ConcreteSpec

namespace ConcreteSpec.Observe
open ConcreteSpec

/-- Law (subject `ConcreteSpec.Observe.observe`). **What a checked program shows its parent.** With any fuel, the run of a checked slice-1
program either has not ended yet, or the parent observes an exit with the unsigned-byte code main
returned, or a trap (status 134 and the trap's message). It is never stuck. -/
theorem checked_observation {p : Program} (h : Checked p) (n : Nat) :
    run p n = .outOfFuel ∨ (∃ c, c < 256 ∧ observe true (run p n) = some (.exited c)) ∨
      ∃ k, observe true (run p n) = some (.trapped k) := by
  cases ho : run p n with
  | outOfFuel => exact .inl rfl
  | exit c =>
    have hc : c < 256 := by
      have hb := runBody_exit_u8 h.2 .nil n
      change ExitU8 (run p n) at hb
      simpa [ho, ExitU8] using hb
    exact .inr (.inl ⟨c, hc, by simp [observe]⟩)
  | trap k => exact .inr (.inr ⟨k, by simp [observe]⟩)
  | stuck w => exact absurd ho (checked_never_stuck h n w)

/-- Law (subject `ConcreteSpec.Observe.observe`). With enough fuel (one unit per statement and one for return), a checked program's run ends,
and the parent observes a status below 256 that is the returned code, or 134 for a trap. -/
theorem checked_observable {p : Program} (h : Checked p) :
    ∃ obs, observe true (run p (p.body.length + 1)) = some obs ∧ obs.status < 256 := by
  rcases checked_observation h (p.body.length + 1) with hf | ⟨c, hc, ho⟩ | ⟨k, ho⟩
  · exact absurd hf (runBody_enough p.body [])
  · exact ⟨_, ho, hc⟩
  · exact ⟨_, ho, by simp [Observation.status]⟩

/-- Representation lemma. A refused start never runs main: the observation does not depend on the run. This is the defining refusal branch of the observation map. (approved: REV-base1c-agy-01025713b) -/
theorem refused_start (o : Outcome) : observe false o = some .startupRefused := rfl

namespace CheckedEvidence

def literal : Program := ⟨[], [], .int .u8 42⟩

def noObservation : Bool → Outcome → Option Observation := fun _ _ => none

/-- Instance (law `ConcreteSpec.Observe.checked_observation`, case `literal`). -/
example : run literal 1 = .outOfFuel ∨
    (∃ c, c < 256 ∧ observe true (run literal 1) = some (.exited c)) ∨
    ∃ k, observe true (run literal 1) = some (.trapped k) :=
  @checked_observation literal ⟨by decide, .ret (.int (by decide)) (.atom .int) (by decide)⟩ 1

/-- Planted (law `ConcreteSpec.Observe.checked_observation`, case `literal`, subject `ConcreteSpec.Observe.observe`, mutant `ConcreteSpec.Observe.CheckedEvidence.noObservation`). -/
example : Checked literal ∧ ¬ (run literal 1 = .outOfFuel ∨
    (∃ c, c < 256 ∧ noObservation true (run literal 1) = some (.exited c)) ∨
    ∃ k, noObservation true (run literal 1) = some (.trapped k)) :=
  ⟨⟨by decide, .ret (.int (by decide)) (.atom .int) (by decide)⟩,
    by simp [literal, run, runBody, retOutcome, evalAtom, noObservation]⟩

/-- Instance (law `ConcreteSpec.Observe.checked_observable`, case `literal`). -/
example : ∃ obs, observe true (run literal (literal.body.length + 1)) = some obs ∧ obs.status < 256 :=
  @checked_observable literal ⟨by decide, .ret (.int (by decide)) (.atom .int) (by decide)⟩

/-- Planted (law `ConcreteSpec.Observe.checked_observable`, case `literal`, subject `ConcreteSpec.Observe.observe`, mutant `ConcreteSpec.Observe.CheckedEvidence.noObservation`). -/
example : Checked literal ∧ ¬ (∃ obs,
    noObservation true (run literal (literal.body.length + 1)) = some obs ∧ obs.status < 256) :=
  ⟨⟨by decide, .ret (.int (by decide)) (.atom .int) (by decide)⟩, by simp [noObservation]⟩

end CheckedEvidence

end ConcreteSpec.Observe
