import ConcreteSpec.Spec.Leak
import ConcreteSpec.Proofs.Exec

/-!
# The laws of leakage, release and accounting (`Spec/Leak.lean`, foundation contract C7)

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). See `ConcreteSpec/README.md`.

The execution example is a small C1 operand language with one explicit release followed by one
output. `secure_ni` and `secure_ct` prove delimited-release NI and constant time for the program
whose output and variable-time operands are public, for every pair of complete instrumented runs
with matched choices. The leaky program releases the same authorized public zero but outputs its secret:
`authorized_release_not_ni` and `authorized_release_not_ct` are concrete counterexamples to deriving
either security claim from policy authorization alone.

The accounting theorems prove that every successful abstract C4 action sequence conserves requested
and live bytes, preserves well-formedness and never lowers the peak. Free and reset conserve the
cumulative history and peak while removing exactly their stated live bytes. Zero-byte allocation is
the identity on a well-formed abstract memory.
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

/-- The program whose public observation and variable-time operands do not depend on the secret. -/
def secureProgram : Program ops := ⟨[]⟩

/-- The program that differs only by outputting the secret. -/
def leakyProgram : Program ops := ⟨[]⟩

/-- Run either program from secret input `secret`; there are no public local inputs. -/
def start (secret : Int) : Config ops := ⟨.exec (body (.lit 7)), [], secret, []⟩

/-- Start the leaky program. -/
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

/-- The policy authorizes exactly the typed release site used by both programs. -/
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

/-- Fixed external choices make both example programs deterministic as C1 executions. -/
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

/-- A C1-final configuration has no machine step. -/
theorem step_none_of_final {P : Program ops} {c : Config ops} {f : Final Int}
    (hf : c.final? = some f) : step P c = none := by
  cases c with
  | mk ctrl locals st frames =>
    cases ctrl with
    | exec s => simp [Config.final?] at hf
    | done =>
      cases frames with
      | nil => rfl
      | cons frame frames => simp [Config.final?] at hf
    | exiting e =>
      cases e with
      | brk l => simp [Config.final?] at hf
      | cont l => simp [Config.final?] at hf
      | ret v =>
        cases frames with
        | nil => rfl
        | cons frame frames => simp [Config.final?] at hf
    | trapped kd site => rfl

/-- Every complete run of the secure four-step example has exactly the public traces below. -/
theorem secure_run_unique (secret : Int)
    (r : Run instrumentation secureProgram (start secret)) :
    r.observations = [.output 7] ∧
      r.leakage = [.variableTime ⟨0⟩ [0], .variableTime ⟨1⟩ [7]] ∧
      r.releases = [releaseZero] ∧ r.choices = [(), (), (), ()] ∧
      r.final = ⟨.done, [], secret, []⟩ := by
  obtain ⟨final, ending, hf, observations, leakage, releases, choices, hs⟩ := r
  cases hs with
  | refl => simp [start, Config.final?] at hf
  | step h₁ rest₁ =>
    obtain ⟨rfl, rfl⟩ := step_unique h₁ (.silent (P := secureProgram) rfl)
    cases rest₁ with
    | refl => simp [Config.final?] at hf
    | step h₂ rest₂ =>
      obtain ⟨rfl, rfl⟩ := step_unique h₂
        (Step.prim (O := ops) (P := secureProgram) (p := .release) (vs := [0]) (st := secret)
          (a := (⟨none, .ok 0, secret⟩ : Answer Int Int Int)) rfl (by simp [allowed]) rfl)
      cases rest₂ with
      | refl => simp [Config.final?] at hf
      | step h₃ rest₃ =>
        obtain ⟨rfl, rfl⟩ := step_unique h₃ (.silent (P := secureProgram) rfl)
        cases rest₃ with
        | refl => simp [Config.final?] at hf
        | step h₄ rest₄ =>
          obtain ⟨rfl, rfl⟩ := step_unique h₄
            (Step.prim (O := ops) (P := secureProgram) (p := .output) (vs := [7]) (st := secret)
              (a := (⟨some 7, .ok 0, secret⟩ : Answer Int Int Int)) rfl (by simp [allowed]) rfl)
          cases rest₄ with
          | refl => exact ⟨rfl, rfl, rfl, rfl, rfl⟩
          | step hs _ => exact (no_step_of_none (P := secureProgram)
            (c := (⟨.done, [], secret, []⟩ : Config ops)) rfl _ _ hs).elim

/-- **Delimited-release NI, proved for the secure example.** Low-equivalent secret inputs, matched
choices and equal release traces give the same output and final public observation. -/
theorem secure_ni (s₁ s₂ : Int) :
    DelimitedReleaseNI instrumentation secureProgram labelling (start s₁) (start s₂) := by
  intro r₁ r₂ _ _ _
  have h₁ := secure_run_unique s₁ r₁
  have h₂ := secure_run_unique s₂ r₂
  simp only [Run.publicObservations, h₁.1, h₂.1, h₁.2.2.2.2, h₂.2.2.2.2]
  rfl

/-- **Constant time, proved separately for the secure example.** Under the same premises the entire
leakage traces, including all variable-time operands, are equal. -/
theorem secure_ct (s₁ s₂ : Int) :
    ConstantTime instrumentation secureProgram labelling (start s₁) (start s₂) := by
  intro r₁ r₂ _ _ _
  have h₁ := secure_run_unique s₁ r₁
  have h₂ := secure_run_unique s₂ r₂
  rw [h₁.2.1, h₂.2.1]

/-- One complete secure execution: release zero, output seven, and finish normally. -/
def secureRun (secret : Int) : Run instrumentation secureProgram (start secret) where
  final := finished secret
  ending := .normal
  finalEnding := rfl
  observations := [.output 7]
  leakage := [.variableTime ⟨0⟩ [0], .variableTime ⟨1⟩ [7]]
  releases := [releaseZero]
  choices := [(), (), (), ()]
  steps :=
    InstrumentedSteps.step (I := instrumentation)
      (Step.silent (P := secureProgram) (c := start secret) (c' := afterSeq (.lit 7) secret) rfl) <|
    InstrumentedSteps.step (I := instrumentation)
      (Step.prim (O := ops) (P := secureProgram) (c := afterSeq (.lit 7) secret)
        (c' := afterRelease (.lit 7) secret) (p := .release) (vs := [0]) (st := secret)
        (a := (⟨none, .ok 0, secret⟩ : Answer Int Int Int)) rfl (by simp [allowed]) rfl) <|
    InstrumentedSteps.step (I := instrumentation)
      (Step.silent (P := secureProgram) (c := afterRelease (.lit 7) secret)
        (c' := beforeOutput (.lit 7) secret) rfl) <|
    InstrumentedSteps.step (I := instrumentation)
      (Step.prim (O := ops) (P := secureProgram) (c := beforeOutput (.lit 7) secret)
        (c' := finished secret) (p := .output) (vs := [7]) (st := secret)
        (a := (⟨some 7, .ok 0, secret⟩ : Answer Int Int Int)) rfl (by simp [allowed]) rfl) <|
    InstrumentedSteps.refl (I := instrumentation) (P := secureProgram) (finished secret)

/-- One complete leaky execution. Its release and choice traces are exactly `secureRun`'s. -/
def leakyRun (secret : Int) : Run instrumentation leakyProgram (leakyStart secret) where
  final := finished secret
  ending := .normal
  finalEnding := rfl
  observations := [.output secret]
  leakage := [.variableTime ⟨0⟩ [0], .variableTime ⟨1⟩ [secret]]
  releases := [releaseZero]
  choices := [(), (), (), ()]
  steps :=
    InstrumentedSteps.step (I := instrumentation)
      (Step.silent (P := leakyProgram) (c := leakyStart secret) (c' := afterSeq .secret secret) rfl) <|
    InstrumentedSteps.step (I := instrumentation)
      (Step.prim (O := ops) (P := leakyProgram) (c := afterSeq .secret secret)
        (c' := afterRelease .secret secret) (p := .release) (vs := [0]) (st := secret)
        (a := (⟨none, .ok 0, secret⟩ : Answer Int Int Int)) rfl (by simp [allowed]) rfl) <|
    InstrumentedSteps.step (I := instrumentation)
      (Step.silent (P := leakyProgram) (c := afterRelease .secret secret)
        (c' := beforeOutput .secret secret) rfl) <|
    InstrumentedSteps.step (I := instrumentation)
      (Step.prim (O := ops) (P := leakyProgram) (c := beforeOutput .secret secret)
        (c' := finished secret) (p := .output) (vs := [secret]) (st := secret)
        (a := (⟨some secret, .ok 0, secret⟩ : Answer Int Int Int)) rfl (by simp [allowed]) rfl) <|
    InstrumentedSteps.refl (I := instrumentation) (P := leakyProgram) (finished secret)

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

theorem Account.alloc_wf (a : Account) (bytes : Nat) (_h : a.WF) : (a.alloc bytes).WF := by
  exact Nat.le_max_right _ _

theorem Account.free_wf (a : Account) (bytes : Nat) (h : a.WF) : (a.free bytes).WF := by
  exact Nat.le_trans (Nat.sub_le _ _) h

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
  | reset lineage released =>
    simp only [apply] at h
    split at h
    next heq =>
      cases h
      intro l
      by_cases hl : l = lineage
      · subst l
        simpa [Memory.set] using Account.reset_wf (m lineage)
      · simpa [Memory.set, hl] using hwf l
    next => cases h

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

/-- A successful free conserves cumulative requested bytes and peak, and its net change is exactly
the freed bytes. -/
theorem free_conservation {m m' : Memory} {lineage : LineageId} {bytes : Nat}
    (h : apply m (.free lineage bytes) = some m') :
    (m' lineage).requested = (m lineage).requested ∧
      (m' lineage).live = (m lineage).live - bytes ∧
      (m' lineage).peak = (m lineage).peak := by
  simp only [apply] at h
  split at h
  next hle =>
    cases h
    simp [Memory.set, Account.free]
  next => cases h

/-- A successful reset conserves cumulative requested bytes and peak, and releases exactly the live
bytes named by the action. -/
theorem reset_conservation {m m' : Memory} {lineage : LineageId} {released : Nat}
    (h : apply m (.reset lineage released) = some m') :
    (m' lineage).requested = (m lineage).requested ∧
      (m' lineage).live = (m lineage).live - released ∧
      (m' lineage).peak = (m lineage).peak := by
  simp only [apply] at h
  split at h
  next heq =>
    cases h
    simp [Memory.set, Account.reset, heq]
  next => cases h

/-- **Whole-sequence conservation.** Requested bytes increase by exactly the sequence's allocation
requests. Net live bytes equal allocations minus explicit frees and reset outflows. The statement
holds independently for every allocator lineage, including lineages the sequence never touches. -/
theorem run_conservation {m m' : Memory} {actions : List Action}
    (h : run m actions = some m') (lineage : LineageId) :
    (m' lineage).requested = (m lineage).requested + allocated lineage actions ∧
      (m' lineage).live = (m lineage).live + allocated lineage actions -
        freed lineage actions - resetBytes lineage actions := by
  induction actions generalizing m with
  | nil => cases h; simp [allocated, freed, resetBytes]
  | cons action rest ih =>
    cases ha : apply m action with
    | none => simp [run, ha] at h
    | some m₁ =>
      have hr : run m₁ rest = some m' := by simpa [run, ha] using h
      have hc := ih hr
      cases action with
      | alloc l bytes =>
        simp only [apply, Option.some.injEq] at ha
        subst m₁
        by_cases hl : l = lineage
        · subst l
          simp [Memory.set, Account.alloc, allocated, freed, resetBytes] at hc ⊢
          omega
        · simp [Memory.set, hl, Ne.symm hl, allocated, freed, resetBytes] at hc ⊢
          omega
      | free l bytes =>
        by_cases hle : bytes ≤ (m l).live
        · simp only [apply, hle, ↓reduceIte, Option.some.injEq] at ha
          subst m₁
          by_cases hl : l = lineage
          · subst l
            simp [Memory.set, Account.free, allocated, freed, resetBytes] at hc ⊢
            omega
          · simp [Memory.set, hl, Ne.symm hl, allocated, freed, resetBytes] at hc ⊢
            omega
        · simp [apply, hle] at ha
      | reset l released =>
        by_cases heq : released = (m l).live
        · simp only [apply, heq, ↓reduceIte, Option.some.injEq] at ha
          subst m₁
          by_cases hl : l = lineage
          · subst l
            simp [Memory.set, Account.reset, allocated, freed, resetBytes, heq] at hc ⊢
            omega
          · simp [Memory.set, hl, Ne.symm hl, allocated, freed, resetBytes] at hc ⊢
            omega
        · simp [apply, heq] at ha

/-- No successful action lowers any lineage's historical peak. -/
theorem apply_peak_monotone {m m' : Memory} {action : Action}
    (h : apply m action = some m') (lineage : LineageId) :
    (m lineage).peak ≤ (m' lineage).peak := by
  cases action with
  | alloc l bytes =>
    cases h
    by_cases hl : l = lineage
    · subst l
      simpa [Memory.set, Account.alloc] using
        Nat.le_max_left (m lineage).peak ((m lineage).live + bytes)
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
  | reset l released =>
    simp only [apply] at h
    split at h
    next =>
      cases h
      by_cases hl : l = lineage
      · subst l; simp [Memory.set, Account.reset]
      · simp [Memory.set, Ne.symm hl]
    next => cases h

/-- No successful sequence lowers any lineage's historical peak. -/
theorem run_peak_monotone {m m' : Memory} {actions : List Action}
    (h : run m actions = some m') (lineage : LineageId) :
    (m lineage).peak ≤ (m' lineage).peak := by
  induction actions generalizing m with
  | nil => cases h; exact Nat.le_refl _
  | cons action rest ih =>
    cases ha : apply m action with
    | none => simp [run, ha] at h
    | some m₁ =>
      have hr : run m₁ rest = some m' := by simpa [run, ha] using h
      exact Nat.le_trans (apply_peak_monotone ha lineage) (ih hr)

/-- A report whose fields are all explicitly unestablished is sound whatever the actual accounting
is; no numeric placeholder becomes a claim. -/
theorem unestablished_report_sound (lineage : LineageId) (entry exit : Account)
    (footprint : Option Nat) (requested : Nat) (net : Int) (peak footprintLimit : Nat) :
    LineageReport.Sound entry exit footprint
      ⟨lineage, ⟨requested, .notEstablished⟩, ⟨net, .notEstablished⟩,
        ⟨peak, .notEstablished⟩, ⟨footprintLimit, .notEstablished⟩⟩ := by
  simp [LineageReport.Sound, Bound.Sound, Bound.SoundOptional]

end ConcreteSpec.Core.Leak
