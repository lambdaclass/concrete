import ConcreteSpec.Spec.Leak
import ConcreteSpec.Proofs.Exec

/-!
# The laws of leakage, release and accounting (`Spec/Leak.lean`, foundation contract C7)

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). See `ConcreteSpec/README.md`.

The execution example is a small C1 operand language with one explicit release followed by one
output. `secure_ni` and `secure_ct` prove delimited-release NI and constant time for the start
configuration whose output and variable-time operands are public, for every pair of complete
instrumented runs with matched choices. The leaky start configuration releases the same authorized
public zero but outputs its secret: `authorized_release_not_ni` and `authorized_release_not_ct` are
concrete counterexamples to deriving either security claim from policy authorization alone.

The accounting theorems prove that every successful abstract C4 action sequence conserves requested
and live bytes, preserves well-formedness and never lowers the historical peak. Free and reset
conserve cumulative history and both historical and function-local peaks while removing exactly
their stated live bytes. Zero-byte allocation is the identity on a well-formed abstract memory.
Report soundness uses only the requests and peak since function entry.
-/

namespace ConcreteSpec.Core.Leak
open ConcreteSpec.Core
open ConcreteSpec.Core.Exec

namespace Demo

/-- Values in the example are integers. The shared integer is the secret input. -/
inductive Expr where
  | lit (n : Int)
  | secret

/-- An explicit public release or a public output. -/
inductive Prim where
  | release
  | output

/-- Decidable equality for the example's primitive answers. -/
instance answerDecidableEq : DecidableEq (Answer Int Int Int) := fun a b =>
  decidable_of_iff (a.event = b.event ∧ a.result = b.result ∧ a.store = b.store) (by
    constructor
    · intro h
      cases a
      cases b
      simp_all
    · intro h
      cases h
      exact ⟨rfl, rfl, rfl⟩)

/-- The Boolean answer contract for each primitive request in a given secret state. -/
def allowed (p : Prim) (vs : List Int) (st : Int) (a : Answer Int Int Int) : Bool :=
  match p, vs with
  | .release, [_] => decide (a = ⟨none, .ok 0, st⟩)
  | .output, [v] => decide (a = ⟨some v, .ok 0, st⟩)
  | _, _ => false

/-- Evaluate a public literal or read the secret shared input. -/
def eval (st : Int) : Expr → Res (Int × Int)
  | .lit n => .ok (n, st)
  | .secret => .ok (st, st)

/-- The example C1 operand instance. -/
abbrev ops : Operands where
  Val := Int
  Expr := Expr
  Ty := Unit
  Store := Int
  Prim := Prim
  Event := Int
  unitVal := 0
  unitTy := ()
  isCopy := fun _ => true
  eval := fun _ st e => eval st e
  life := fun _ st => some st
  test := fun v => some (v ≠ 0)
  allows := allowed

/-- Release zero, then evaluate `result` as the output. -/
def body (result : Expr) : Stmt ops :=
  .seq (.prim none .release [.lit 0] ⟨0⟩) (.prim none .output [result] ⟨1⟩)

/-- The shared empty function table; the secure and leaky examples differ in their initial
configurations, not in their program. -/
def program : Program ops := ⟨[]⟩

/-- Start the secure example from secret input `secret`. Its public output and variable-time
operands do not depend on the secret, and there are no public local inputs. -/
def start (secret : Int) : Config ops := ⟨.exec (body (.lit 7)), [], secret, []⟩

/-- Start the leaky example from the same secret input. It releases the same public zero but outputs
the secret. -/
def leakyStart (secret : Int) : Config ops := ⟨.exec (body .secret), [], secret, []⟩

/-- The configuration after sequencing into the release. -/
def afterSeq (result : Expr) (secret : Int) : Config ops :=
  ⟨.exec (.prim none .release [.lit 0] ⟨0⟩), [], secret,
    [.seq (.prim none .output [result] ⟨1⟩)]⟩

/-- The configuration after the release answer. -/
def afterRelease (result : Expr) (secret : Int) : Config ops :=
  ⟨.done, [], secret, [.seq (.prim none .output [result] ⟨1⟩)]⟩

/-- The configuration before the output answer. -/
def beforeOutput (result : Expr) (secret : Int) : Config ops :=
  ⟨.exec (.prim none .output [result] ⟨1⟩), [], secret, []⟩

/-- The common final configuration. -/
def finished (secret : Int) : Config ops := ⟨.done, [], secret, []⟩

/-- Public observations are output values and the C1 final outcome. -/
inductive Observation where
  | output (v : Int)
  | final (f : Final Int)
  deriving DecidableEq, Repr

/-- The primitive request at a configuration, with its evaluated operands, if that is what C1 is
about to execute. -/
def request : Config ops → Option (Prim × List Int × SiteId)
  | ⟨.exec (.prim _ p args site), locals, st, _⟩ =>
      match evalArgs ops locals st args with
      | .ok (vs, _) => some (p, vs, site)
      | _ => none
  | _ => none

/-- The release record emitted by the example's release primitive. -/
def releaseZero : Release Unit Int :=
  ⟨⟨⟨0⟩, ()⟩, 0, "public zero", []⟩

/-- C7 instrumentation of the example. Every primitive is treated as variable-time, so its evaluated
operands are leaked. The external choice records only that a step was taken; answers themselves are
fixed by `allowed`. -/
abbrev instrumentation : Instrumentation ops where
  Address := Unit
  Effect := Unit
  Observation := Observation
  Choice := Unit
  observeStep := fun _ label _ => match label with
    | .silent => []
    | .emit v => [.output v]
  observeFinal := fun c => (c.final?).map .final
  leakage := fun c _ _ => match request c with
    | some (_, vs, site) => [.variableTime site vs]
    | none => []
  releases := fun c _ _ => match request c with
    | some (.release, [0], _) => [releaseZero]
    | _ => []
  choices := fun _ _ _ => [()]

/-- All values are secret in the example labelling; public shared state carries no information. -/
abbrev labelling : Labelling ops where
  PublicValue := Unit
  project := fun _ => none
  lowStore := fun _ _ => True

/-- The policy authorizes exactly the typed release site used by both start configurations. -/
abbrev policy : ReleasePolicy Unit := ⟨[⟨⟨0⟩, ()⟩]⟩

theorem allowed_unique {p : Prim} {vs : List Int} {st : Int} {a b : Answer Int Int Int}
    (ha : ops.allows p vs st a = true) (hb : ops.allows p vs st b = true) : a = b := by
  change allowed p vs st a = true at ha
  change allowed p vs st b = true at hb
  cases p <;> cases vs with
  | nil => simp [allowed] at ha
  | cons v vs =>
    cases vs with
    | nil =>
      simp only [allowed, decide_eq_true_eq] at ha hb
      exact ha.trans hb.symm
    | cons w vs => simp [allowed] at ha

/-- Fixed external choices make executions from either example start configuration deterministic. -/
theorem step_unique {P : Program ops} {c c₁ c₂ : Config ops} {l₁ l₂ : Label ops}
    (h₁ : Step P c l₁ c₁) (h₂ : Step P c l₂ c₂) : l₁ = l₂ ∧ c₁ = c₂ := by
  cases h₁ with
  | silent hs =>
    cases h₂ with
    | silent hs' =>
      rw [hs] at hs'
      cases hs'
      exact ⟨rfl, rfl⟩
    | prim hr _ _ => rw [hs] at hr; cases hr
  | prim hp ha hk =>
    cases h₂ with
    | silent hs => rw [hp] at hs; cases hs
    | prim hp' ha' hk' =>
      rw [hp] at hp'
      cases hp'
      have hab := allowed_unique ha ha'
      rw [hab] at hk
      rw [hk] at hk'
      cases hk'
      exact ⟨by rw [hab], rfl⟩

/-- Every complete run of the secure four-step example has exactly the public traces below. -/
theorem secure_run_unique (secret : Int)
    (r : Run instrumentation program (start secret)) :
    r.observations = [.output 7] ∧
      r.leakage = [.variableTime ⟨0⟩ [0], .variableTime ⟨1⟩ [7]] ∧
      r.releases = [releaseZero] ∧ r.choices = [(), (), (), ()] ∧
      r.final = ⟨.done, [], secret, []⟩ := by
  obtain ⟨final, ending, hf, observations, leakage, releases, choices, hs⟩ := r
  cases hs with
  | refl => simp [start, Config.final?] at hf
  | step h₁ rest₁ =>
    obtain ⟨rfl, rfl⟩ := step_unique h₁ (.silent (P := program) rfl)
    cases rest₁ with
    | refl => simp [Config.final?] at hf
    | step h₂ rest₂ =>
      obtain ⟨rfl, rfl⟩ := step_unique h₂
        (Step.prim (O := ops) (P := program) (p := .release) (vs := [0]) (st := secret)
          (a := (⟨none, .ok 0, secret⟩ : Answer Int Int Int)) rfl (by simp [allowed]) rfl)
      cases rest₂ with
      | refl => simp [Config.final?] at hf
      | step h₃ rest₃ =>
        obtain ⟨rfl, rfl⟩ := step_unique h₃ (.silent (P := program) rfl)
        cases rest₃ with
        | refl => simp [Config.final?] at hf
        | step h₄ rest₄ =>
          obtain ⟨rfl, rfl⟩ := step_unique h₄
            (Step.prim (O := ops) (P := program) (p := .output) (vs := [7]) (st := secret)
              (a := (⟨some 7, .ok 0, secret⟩ : Answer Int Int Int)) rfl (by simp [allowed]) rfl)
          cases rest₄ with
          | refl => exact ⟨rfl, rfl, rfl, rfl, rfl⟩
          | step hs _ => exact (no_step_of_none (P := program)
            (c := (⟨.done, [], secret, []⟩ : Config ops)) rfl _ _ hs).elim

/-- **Delimited-release NI, proved for the secure example.** Low-equivalent secret inputs, matched
choices and equal release traces give the same output and final public observation. -/
theorem secure_ni (s₁ s₂ : Int) :
    DelimitedReleaseNI instrumentation program labelling (start s₁) (start s₂) := by
  intro r₁ r₂ _ _ _
  have h₁ := secure_run_unique s₁ r₁
  have h₂ := secure_run_unique s₂ r₂
  simp only [Run.publicObservations, h₁.1, h₂.1, h₁.2.2.2.2, h₂.2.2.2.2]
  rfl

/-- **Constant time, proved separately for the secure example.** Under the same premises the entire
leakage traces, including all variable-time operands, are equal. -/
theorem secure_ct (s₁ s₂ : Int) :
    ConstantTime instrumentation program labelling (start s₁) (start s₂) := by
  intro r₁ r₂ _ _ _
  have h₁ := secure_run_unique s₁ r₁
  have h₂ := secure_run_unique s₂ r₂
  rw [h₁.2.1, h₂.2.1]

/-- One complete secure execution: release zero, output seven, and finish normally. -/
def secureRun (secret : Int) : Run instrumentation program (start secret) where
  final := finished secret
  ending := .normal
  finalEnding := rfl
  observations := [.output 7]
  leakage := [.variableTime ⟨0⟩ [0], .variableTime ⟨1⟩ [7]]
  releases := [releaseZero]
  choices := [(), (), (), ()]
  steps :=
    InstrumentedSteps.step (I := instrumentation)
      (Step.silent (P := program) (c := start secret) (c' := afterSeq (.lit 7) secret) rfl) <|
    InstrumentedSteps.step (I := instrumentation)
      (Step.prim (O := ops) (P := program) (c := afterSeq (.lit 7) secret)
        (c' := afterRelease (.lit 7) secret) (p := .release) (vs := [0]) (st := secret)
        (a := (⟨none, .ok 0, secret⟩ : Answer Int Int Int)) rfl (by simp [allowed]) rfl) <|
    InstrumentedSteps.step (I := instrumentation)
      (Step.silent (P := program) (c := afterRelease (.lit 7) secret)
        (c' := beforeOutput (.lit 7) secret) rfl) <|
    InstrumentedSteps.step (I := instrumentation)
      (Step.prim (O := ops) (P := program) (c := beforeOutput (.lit 7) secret)
        (c' := finished secret) (p := .output) (vs := [7]) (st := secret)
        (a := (⟨some 7, .ok 0, secret⟩ : Answer Int Int Int)) rfl (by simp [allowed]) rfl) <|
    InstrumentedSteps.refl (I := instrumentation) (P := program) (finished secret)

/-- One complete leaky execution. Its release and choice traces are exactly `secureRun`'s. -/
def leakyRun (secret : Int) : Run instrumentation program (leakyStart secret) where
  final := finished secret
  ending := .normal
  finalEnding := rfl
  observations := [.output secret]
  leakage := [.variableTime ⟨0⟩ [0], .variableTime ⟨1⟩ [secret]]
  releases := [releaseZero]
  choices := [(), (), (), ()]
  steps :=
    InstrumentedSteps.step (I := instrumentation)
      (Step.silent (P := program) (c := leakyStart secret) (c' := afterSeq .secret secret) rfl) <|
    InstrumentedSteps.step (I := instrumentation)
      (Step.prim (O := ops) (P := program) (c := afterSeq .secret secret)
        (c' := afterRelease .secret secret) (p := .release) (vs := [0]) (st := secret)
        (a := (⟨none, .ok 0, secret⟩ : Answer Int Int Int)) rfl (by simp [allowed]) rfl) <|
    InstrumentedSteps.step (I := instrumentation)
      (Step.silent (P := program) (c := afterRelease .secret secret)
        (c' := beforeOutput .secret secret) rfl) <|
    InstrumentedSteps.step (I := instrumentation)
      (Step.prim (O := ops) (P := program) (c := beforeOutput .secret secret)
        (c' := finished secret) (p := .output) (vs := [secret]) (st := secret)
        (a := (⟨some secret, .ok 0, secret⟩ : Answer Int Int Int)) rfl (by simp [allowed]) rfl) <|
    InstrumentedSteps.refl (I := instrumentation) (P := program) (finished secret)

theorem leaky_low (s₁ s₂ : Int) : LowEquivalent labelling (leakyStart s₁) (leakyStart s₂) := by
  simp [LowEquivalent, publicLocals, labelling, leakyStart]

theorem release_authorized (secret : Int) : policy.Authorized (leakyRun secret).releases := by
  simp [ReleasePolicy.Authorized, ReleasePolicy.Permits, leakyRun, releaseZero]

/-- **Authorization does not imply NI.** These two executions have low-equivalent inputs, matched
choices and identical authorized releases, yet output different public observations. -/
theorem authorized_release_not_ni :
    policy.Authorized (leakyRun 1).releases ∧ LowEquivalent labelling (leakyStart 1) (leakyStart 2) ∧
      (leakyRun 1).choices = (leakyRun 2).choices ∧
      (leakyRun 1).releases = (leakyRun 2).releases ∧
      (leakyRun 1).publicObservations ≠ (leakyRun 2).publicObservations := by
  refine ⟨release_authorized 1, leaky_low 1 2, rfl, rfl, ?_⟩
  decide

/-- **Authorization does not imply CT.** The same premises and authorized release coexist with
 different variable-time operands and therefore different leakage traces. -/
theorem authorized_release_not_ct :
    policy.Authorized (leakyRun 1).releases ∧ LowEquivalent labelling (leakyStart 1) (leakyStart 2) ∧
      (leakyRun 1).choices = (leakyRun 2).choices ∧
      (leakyRun 1).releases = (leakyRun 2).releases ∧
      (leakyRun 1).leakage ≠ (leakyRun 2).leakage := by
  refine ⟨release_authorized 1, leaky_low 1 2, rfl, rfl, ?_⟩
  intro h
  simp [leakyRun] at h

end Demo

/-! ## Accounting conservation -/

theorem Account.alloc_zero (a : Account) (_h : a.WF) : a.alloc 0 = a := by
  cases a
  simp_all [Account.alloc, Account.WF]

theorem Memory.set_same (m : Memory) (lineage : LineageId) :
    Memory.set m lineage (m lineage) = m := by
  funext l
  by_cases h : l = lineage <;> simp [Memory.set, h]

theorem Account.enter_wf (a : Account) (h : a.WF) : a.enter.WF := by
  exact ⟨h.1, Nat.le_refl _⟩

theorem Account.alloc_wf (a : Account) (bytes : Nat) (_h : a.WF) : (a.alloc bytes).WF := by
  exact ⟨Nat.le_max_right _ _, Nat.le_max_right _ _⟩

theorem Account.free_wf (a : Account) (bytes : Nat) (h : a.WF) : (a.free bytes).WF := by
  exact ⟨Nat.le_trans (Nat.sub_le _ _) h.1, Nat.le_trans (Nat.sub_le _ _) h.2⟩

theorem Account.reset_wf (a : Account) : a.reset.WF := by
  simp [Account.WF, Account.reset]

/-- Every successful accounting action preserves well-formedness at every lineage. -/
theorem apply_wf {m : Memory} {action : Action} {m' : Memory}
    (h : apply m action = some m') (hwf : ∀ l, (m l).WF) : ∀ l, (m' l).WF := by
  cases action with
  | alloc lineage bytes =>
    cases h
    intro l
    by_cases hl : l = lineage
    · subst l
      simpa [Memory.set] using Account.alloc_wf (m lineage) bytes (hwf lineage)
    · simpa [Memory.set, hl] using hwf l
  | free lineage bytes =>
    simp only [apply] at h
    split at h
    next hle =>
      cases h
      intro l
      by_cases hl : l = lineage
      · subst l
        simpa [Memory.set] using Account.free_wf (m lineage) bytes (hwf lineage)
      · simpa [Memory.set, hl] using hwf l
    next => cases h
  | reset lineage =>
    cases h
    intro l
    by_cases hl : l = lineage
    · subst l
      simpa [Memory.set] using Account.reset_wf (m lineage)
    · simpa [Memory.set, hl] using hwf l

/-- Every successful sequence preserves well-formedness. -/
theorem run_wf {m m' : Memory} {actions : List Action}
    (h : run m actions = some m') (hwf : ∀ l, (m l).WF) : ∀ l, (m' l).WF := by
  induction actions generalizing m with
  | nil => cases h; exact hwf
  | cons action rest ih =>
    cases ha : apply m action with
    | none => simp [run, ha] at h
    | some m₁ =>
      have hr : run m₁ rest = some m' := by simpa [run, ha] using h
      exact ih hr (apply_wf ha hwf)

/-- A zero-byte allocation changes no account at any lineage. -/
theorem apply_alloc_zero {m : Memory} (hwf : ∀ l, (m l).WF) (lineage : LineageId) :
    apply m (.alloc lineage 0) = some m := by
  simp only [apply, Account.alloc_zero (m lineage) (hwf lineage), Memory.set_same]

/-- A successful free conserves cumulative requested bytes and both peaks, changes live bytes by
exactly the freed amount, and leaves every other lineage unchanged. -/
theorem free_conservation {m m' : Memory} {lineage : LineageId} {bytes : Nat}
    (h : apply m (.free lineage bytes) = some m') :
    (m' lineage).requested = (m lineage).requested ∧
      (m' lineage).live = (m lineage).live - bytes ∧
      (m' lineage).peak = (m lineage).peak ∧
      (m' lineage).functionPeak = (m lineage).functionPeak ∧
      ∀ l, l ≠ lineage → m' l = m l := by
  simp only [apply] at h
  split at h
  next hle =>
    cases h
    simp only [Memory.set, ↓reduceIte]
    refine ⟨rfl, rfl, rfl, rfl, ?_⟩
    intro l hl
    simp [hl]
  next => cases h

/-- A successful reset satisfies the independent conservation predicate with the actual live bytes
before the transition as its outflow. Other lineages are unchanged. -/
theorem reset_conservation {m m' : Memory} {lineage : LineageId}
    (h : apply m (.reset lineage) = some m') :
    ResetConservation (m lineage) (m' lineage) (m lineage).live ∧
      ∀ l, l ≠ lineage → m' l = m l := by
  cases h
  constructor
  · simp [ResetConservation, Memory.set, Account.reset]
  · intro l hl
    simp [Memory.set, hl]

/-- Non-trivial reset conservation: five live bytes are released and all history is retained. -/
example : ResetConservation ⟨100, 5, 12, 5⟩ (Account.reset ⟨100, 5, 12, 5⟩) 5 := by
  simp [ResetConservation, Account.reset]

/-- Planted bad reset record: four bytes cannot account for the five bytes actually released. -/
example : ¬ ResetConservation ⟨100, 5, 12, 5⟩ (Account.reset ⟨100, 5, 12, 5⟩) 4 := by
  simp [ResetConservation, Account.reset]

@[simp] theorem resetBytes_cons {m m' : Memory} {action : Action} {rest : List Action}
    (lineage : LineageId) (h : apply m action = some m') :
    resetBytes lineage m (action :: rest) =
      resetOutflow lineage m action + resetBytes lineage m' rest := by
  simp [resetBytes, h]

/-- **Whole-sequence conservation.** Requested bytes increase by exactly the sequence's allocation
requests. Over the integers, net live bytes equal allocations minus explicit frees and reset
outflows read from the actual intermediate states; no truncated natural subtraction hides an
outflow. The statement holds independently for every allocator lineage, including untouched ones. -/
theorem run_conservation {m m' : Memory} {actions : List Action}
    (h : run m actions = some m') (lineage : LineageId) :
    (m' lineage).requested = (m lineage).requested + allocated lineage actions ∧
      ((m' lineage).live : Int) = (m lineage).live + allocated lineage actions -
        freed lineage actions - resetBytes lineage m actions := by
  induction actions generalizing m with
  | nil => cases h; simp [allocated, freed, resetBytes]
  | cons action rest ih =>
    cases ha : apply m action with
    | none => simp [run, ha] at h
    | some m₁ =>
      have hr : run m₁ rest = some m' := by simpa [run, ha] using h
      have hc := ih hr
      rw [resetBytes_cons lineage ha]
      cases action with
      | alloc l bytes =>
        simp only [apply, Option.some.injEq] at ha
        subst m₁
        by_cases hl : l = lineage
        · subst l
          simp [Memory.set, Account.alloc, allocated, freed, resetOutflow] at hc ⊢
          omega
        · simp [Memory.set, hl, Ne.symm hl, allocated, freed, resetOutflow] at hc ⊢
          omega
      | free l bytes =>
        by_cases hle : bytes ≤ (m l).live
        · simp only [apply, hle, ↓reduceIte, Option.some.injEq] at ha
          subst m₁
          by_cases hl : l = lineage
          · subst l
            simp [Memory.set, Account.free, allocated, freed, resetOutflow] at hc ⊢
            omega
          · simp [Memory.set, hl, Ne.symm hl, allocated, freed, resetOutflow] at hc ⊢
            omega
        · simp [apply, hle] at ha
      | reset l =>
        simp only [apply, Option.some.injEq] at ha
        subst m₁
        by_cases hl : l = lineage
        · subst l
          simp [Memory.set, Account.reset, allocated, freed, resetOutflow] at hc ⊢
          omega
        · simp [Memory.set, hl, Ne.symm hl, allocated, freed, resetOutflow] at hc ⊢
          omega

/-- No successful action lowers either the historical or function-local peak of any lineage. -/
theorem apply_peak_monotone {m m' : Memory} {action : Action}
    (h : apply m action = some m') (lineage : LineageId) :
    (m lineage).peak ≤ (m' lineage).peak ∧
      (m lineage).functionPeak ≤ (m' lineage).functionPeak := by
  cases action with
  | alloc l bytes =>
    cases h
    by_cases hl : l = lineage
    · subst l
      simp [Memory.set, Account.alloc, Nat.le_max_left]
    · simp [Memory.set, Ne.symm hl]
  | free l bytes =>
    simp only [apply] at h
    split at h
    next =>
      cases h
      by_cases hl : l = lineage
      · subst l; simp [Memory.set, Account.free]
      · simp [Memory.set, Ne.symm hl]
    next => cases h
  | reset l =>
    cases h
    by_cases hl : l = lineage
    · subst l; simp [Memory.set, Account.reset]
    · simp [Memory.set, Ne.symm hl]

/-- No successful sequence lowers either the historical or function-local peak of any lineage. -/
theorem run_peak_monotone {m m' : Memory} {actions : List Action}
    (h : run m actions = some m') (lineage : LineageId) :
    (m lineage).peak ≤ (m' lineage).peak ∧
      (m lineage).functionPeak ≤ (m' lineage).functionPeak := by
  induction actions generalizing m with
  | nil => cases h; exact ⟨Nat.le_refl _, Nat.le_refl _⟩
  | cons action rest ih =>
    cases ha : apply m action with
    | none => simp [run, ha] at h
    | some m₁ =>
      have hr : run m₁ rest = some m' := by simpa [run, ha] using h
      have ha' := apply_peak_monotone ha lineage
      have hr' := ih hr
      exact ⟨Nat.le_trans ha'.1 hr'.1, Nat.le_trans ha'.2 hr'.2⟩

/-- An arbitrary report is sound for an actual run when its established limits bound quantities
obtained independently from the action sequence and function entry. Requested and net-change bounds
are translated to entry/exit accounts by `run_conservation`; unestablished fields assert nothing. -/
theorem run_report_sound {m m' : Memory} {actions : List Action} {lineage : LineageId}
    {footprint : Option Nat} {r : LineageReport} (h : run m actions = some m')
    (hrequested : Bound.Sound (allocated lineage actions) r.requested)
    (hnet : Bound.Sound ((allocated lineage actions : Int) - freed lineage actions -
      resetBytes lineage m actions) r.netChange)
    (hpeak : Bound.Sound (m' lineage).functionPeak r.peak)
    (hfootprint : Bound.SoundOptional footprint r.footprint) :
    LineageReport.Sound (m lineage) (m' lineage) footprint r := by
  have hc := run_conservation h lineage
  have hreqEq : (m' lineage).requested - (m lineage).requested = allocated lineage actions := by
    omega
  have hnetEq : ((m' lineage).live : Int) - ((m lineage).live : Int) =
      (allocated lineage actions : Int) - freed lineage actions - resetBytes lineage m actions := by
    omega
  simp only [LineageReport.Sound]
  rw [hreqEq, hnetEq]
  exact ⟨hrequested, hnet, hpeak, hfootprint⟩

/-- Independent numeric limits establish all four fields of a report for an actual run. The limits
are parameters, not expressions copied from the exit account. -/
theorem established_report_sound {m m' : Memory} {actions : List Action} {lineage : LineageId}
    {footprint : Option Nat} {requestedLimit : Nat} {netLimit : Int} {peakLimit footprintLimit : Nat}
    (h : run m actions = some m') (hrequested : allocated lineage actions ≤ requestedLimit)
    (hnet : (allocated lineage actions : Int) - freed lineage actions -
      resetBytes lineage m actions ≤ netLimit)
    (hpeak : (m' lineage).functionPeak ≤ peakLimit)
    (hfootprint : ∃ bytes, footprint = some bytes ∧ bytes ≤ footprintLimit) :
    LineageReport.Sound (m lineage) (m' lineage) footprint
      ⟨lineage, ⟨requestedLimit, .boundEstablished⟩, ⟨netLimit, .boundEstablished⟩,
        ⟨peakLimit, .boundEstablished⟩, ⟨footprintLimit, .boundEstablished⟩⟩ := by
  apply run_report_sound h
  · intro _; exact hrequested
  · intro _; exact hnet
  · intro _; exact hpeak
  · intro _; exact hfootprint

/-- Entry for the concrete report example: 100 historical requested bytes and two live bytes. -/
def reportEntry : Account := ⟨100, 2, 100, 2⟩

def reportEntryMemory : Memory := Memory.set Memory.empty ⟨0⟩ reportEntry

def reportActions : List Action := [.alloc ⟨0⟩ 8, .free ⟨0⟩ 3]

/-- The actual intermediate states of `reportActions`, retained explicitly so `run` is `rfl`. -/
def reportExitMemory : Memory :=
  Memory.set (Memory.set reportEntryMemory ⟨0⟩ (reportEntry.alloc 8)) ⟨0⟩
    ((reportEntry.alloc 8).free 3)

def reportExit : Account := ⟨108, 7, 100, 10⟩

theorem reportExitMemory_lineage : reportExitMemory ⟨0⟩ = reportExit := rfl

/-- An independently written report with all fields established. -/
def establishedReport : LineageReport :=
  ⟨⟨0⟩, ⟨8, .boundEstablished⟩, ⟨5, .boundEstablished⟩,
    ⟨10, .boundEstablished⟩, ⟨12, .boundEstablished⟩⟩

/-- A mixed-evidence report: net change is unknown, while the other three fields are established. -/
def mixedStatusReport : LineageReport :=
  ⟨⟨0⟩, ⟨8, .boundEstablished⟩, ⟨-100, .notEstablished⟩,
    ⟨10, .boundEstablished⟩, ⟨12, .boundEstablished⟩⟩

/-- Four independently established bounds for the concrete allocation/free run. -/
def establishedReportWith (requested : Nat) (net : Int) (peak footprint : Nat) : LineageReport :=
  ⟨⟨0⟩, ⟨requested, .boundEstablished⟩, ⟨net, .boundEstablished⟩,
    ⟨peak, .boundEstablished⟩, ⟨footprint, .boundEstablished⟩⟩

/-- The independent report 8/5/10/12 is sound for the actual 100+8 requested bytes, two-byte entry,
allocation of eight and free of three. -/
theorem independent_report_sound :
    LineageReport.Sound reportEntry reportExit (some 10) establishedReport := by
  have h := established_report_sound (m := reportEntryMemory) (m' := reportExitMemory)
    (actions := reportActions) (lineage := ⟨0⟩) (requestedLimit := 8) (netLimit := 5)
    (peakLimit := 10) (footprintLimit := 12) (footprint := some 10) rfl
    (by decide) (by decide) (by decide) ⟨10, rfl, by decide⟩
  simpa only [establishedReport, reportEntryMemory, Memory.set, ↓reduceIte,
    reportExitMemory_lineage] using h

/-- The established fields of the mixed-status report constrain the same actual run; its unknown
net-change field makes no numeric claim. -/
theorem mixed_status_report_sound :
    LineageReport.Sound reportEntry reportExit (some 10) mixedStatusReport := by
  have h := run_report_sound (m := reportEntryMemory) (m' := reportExitMemory)
    (actions := reportActions) (lineage := ⟨0⟩) (r := mixedStatusReport) (footprint := some 10) rfl
    (by simp [mixedStatusReport, Bound.Sound, reportActions, allocated])
    (by simp [mixedStatusReport, Bound.Sound])
    (by simp [mixedStatusReport, Bound.Sound, reportExitMemory_lineage, reportExit])
    (by simp [mixedStatusReport, Bound.SoundOptional])
  simpa only [reportEntryMemory, Memory.set, ↓reduceIte, reportExitMemory_lineage] using h

/-- A function with an empty actual run establishes zero local request and peak bounds even when
earlier execution requested 100 bytes. -/
theorem empty_function_report_sound :
    LineageReport.Sound (Account.enter ⟨100, 0, 100, 0⟩) (Account.enter ⟨100, 0, 100, 0⟩) none
      ⟨⟨0⟩, ⟨0, .boundEstablished⟩, ⟨0, .boundEstablished⟩,
        ⟨0, .boundEstablished⟩, ⟨0, .notEstablished⟩⟩ := by
  apply run_report_sound
    (m := Memory.set Memory.empty ⟨0⟩ (Account.enter ⟨100, 0, 100, 0⟩))
    (m' := Memory.set Memory.empty ⟨0⟩ (Account.enter ⟨100, 0, 100, 0⟩))
    (actions := []) (lineage := ⟨0⟩) rfl
  · simp [Bound.Sound, allocated]
  · simp [Bound.Sound, allocated, freed, resetBytes]
  · simp [Bound.Sound, Memory.set, Account.enter]
  · simp [Bound.SoundOptional]

/-- Planted underbounds: each established field independently rejects an understated limit. -/
example : ¬ LineageReport.Sound reportEntry reportExit (some 10)
    (establishedReportWith 7 5 10 12) := by
  simp [LineageReport.Sound, Bound.Sound, establishedReportWith, reportEntry, reportExit]

example : ¬ LineageReport.Sound reportEntry reportExit (some 10)
    (establishedReportWith 8 4 10 12) := by
  simp [LineageReport.Sound, Bound.Sound, establishedReportWith, reportEntry, reportExit]

example : ¬ LineageReport.Sound reportEntry reportExit (some 10)
    (establishedReportWith 8 5 9 12) := by
  simp [LineageReport.Sound, Bound.Sound, establishedReportWith, reportEntry, reportExit]

example : ¬ LineageReport.Sound reportEntry reportExit (some 10)
    (establishedReportWith 8 5 10 9) := by
  simp [LineageReport.Sound, Bound.Sound, Bound.SoundOptional, establishedReportWith,
    reportEntry, reportExit]

/-- Established footprint evidence cannot certify an absent allocator-defined footprint. -/
example : ¬ LineageReport.Sound reportEntry reportExit none establishedReport := by
  simp [LineageReport.Sound, Bound.SoundOptional, establishedReport]

/-- Representation helper only, not soundness evidence: unestablished fields make no claim, so this
lemma is intentionally vacuous about the actual accounting values. -/
theorem unestablished_report_sound (lineage : LineageId) (entry exit : Account)
    (footprint : Option Nat) (requested : Nat) (net : Int) (peak footprintLimit : Nat) :
    LineageReport.Sound entry exit footprint
      ⟨lineage, ⟨requested, .notEstablished⟩, ⟨net, .notEstablished⟩,
        ⟨peak, .notEstablished⟩, ⟨footprintLimit, .notEstablished⟩⟩ := by
  simp [LineageReport.Sound, Bound.Sound, Bound.SoundOptional]

end ConcreteSpec.Core.Leak
