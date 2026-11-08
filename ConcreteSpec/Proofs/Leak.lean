import ConcreteSpec.Spec.Leak
import ConcreteSpec.Proofs.Exec

/-!
# The laws of leakage, release and accounting (`Spec/Leak.lean`, foundation contract C7)

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). See `ConcreteSpec/README.md`.

The execution examples use a small C1 operand language. `secure_ni` and `secure_ct` declassify the
released component while retaining an independent hidden secret, so equal release records are needed. `timing_ni` and `timing_not_ct`
separate the claims with public output but secret-dependent variable-time work. The leaky start
configuration releases authorized public zero but outputs its secret: `authorized_release_not_ni`
and `authorized_release_not_ct` refute deriving either claim from authorization alone.

The accounting theorems prove that every successful abstract C4 action sequence conserves requested
and live bytes, preserves well-formedness and never lowers either historical or function-local
peaks. Reset outflow is read from actual intermediate states. Zero-byte allocation is the identity
on a well-formed abstract memory. Report soundness connects independent limits to actual runs.
-/

namespace ConcreteSpec.Core.Leak
open ConcreteSpec.Core
open ConcreteSpec.Core.Exec

/-- Helper (projection). Forgets annotations from the C1 derivation; not an independent semantics theorem. (approved: PENDING).

Forget only the C7 instrumentation: an instrumented execution projects to the C1 steps and their
ordinary event trace, with every machine transition unchanged. -/
theorem InstrumentedSteps.toSteps {O : Operands} {I : Instrumentation O} {P : Program O}
    {c : Config O} {observations : List I.Observation}
    {leakage : List (Leakage O.Val I.Address I.Effect)} {releases : List (Release O.Ty O.Val)}
    {choices : List I.Choice} {c' : Config O}
    (h : InstrumentedSteps I P c observations leakage releases choices c') :
    ∃ events, Steps P c events c' := by
  induction h with
  | refl c => exact ⟨[], Steps.refl c⟩
  | step h rest ih =>
    obtain ⟨events, steps⟩ := ih
    exact ⟨_, Steps.step h steps⟩

namespace Demo

/-- Values in the example are integers. The shared integer is the secret input. -/
inductive Expr where
  | lit (n : Int)
  | secret

/-- An explicit public release, a public output, or unobserved variable-time work. -/
inductive Prim where
  | release
  | output
  | work

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
  | .work, [_] => decide (a = ⟨none, .ok 0, st⟩)
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

/-- Release `released`, then evaluate `result` as the output. -/
def body (released result : Expr) : Stmt ops :=
  .seq (.prim none .release [released] ⟨0⟩) (.prim none .output [result] ⟨1⟩)

/-- The shared empty function table; the examples differ in their initial configurations. -/
def program : Program ops := ⟨[]⟩

/-- A separate, unreleased secret local. Zero retains the original empty-local example. -/
def hiddenLocals (hidden : Nat) : AList Var Int :=
  if hidden = 0 then [] else [(⟨"hidden", 0⟩, -(Int.ofNat hidden) - 1)]

/-- Release and output the shared secret while retaining an independent, unreleased secret local.
Equal release traces constrain the released component, but leave the hidden component unequal. -/
def start (secret : Int) (hidden : Nat := 0) : Config ops :=
  ⟨.exec (body .secret .secret), hiddenLocals hidden, secret, []⟩

/-- The leaky example releases public zero but outputs its secret. -/
def leakyStart (secret : Int) : Config ops := ⟨.exec (body (.lit 0) .secret), [], secret, []⟩

/-- Public output with secret-dependent variable-time work and no secret-dependent release. -/
def timingBody : Stmt ops := .seq (.prim none .work [.secret] ⟨2⟩) (body (.lit 0) (.lit 7))
def timingStart (secret : Int) : Config ops := ⟨.exec timingBody, [], secret, []⟩

/-- The configuration after sequencing into the release. -/
def afterSeq (released result : Expr) (secret : Int) (locals : AList Var Int := []) : Config ops :=
  ⟨.exec (.prim none .release [released] ⟨0⟩), locals, secret,
    [.seq (.prim none .output [result] ⟨1⟩)]⟩

/-- The configuration after the release answer. -/
def afterRelease (result : Expr) (secret : Int) (locals : AList Var Int := []) : Config ops :=
  ⟨.done, locals, secret, [.seq (.prim none .output [result] ⟨1⟩)]⟩

/-- The configuration before the output answer. -/
def beforeOutput (result : Expr) (secret : Int) (locals : AList Var Int := []) : Config ops :=
  ⟨.exec (.prim none .output [result] ⟨1⟩), locals, secret, []⟩

/-- The two configurations specific to sequencing and answering the timing example's work. -/
def timingAfterSeq (secret : Int) : Config ops :=
  ⟨.exec (.prim none .work [.secret] ⟨2⟩), [], secret, [.seq (body (.lit 0) (.lit 7))]⟩
def timingAfterWork (secret : Int) : Config ops :=
  ⟨.done, [], secret, [.seq (body (.lit 0) (.lit 7))]⟩
def timingBeforeBody (secret : Int) : Config ops :=
  ⟨.exec (body (.lit 0) (.lit 7)), [], secret, []⟩

/-- The common final configuration. -/
def finished (secret : Int) (locals : AList Var Int := []) : Config ops := ⟨.done, locals, secret, []⟩

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

/-- A release record carries the actual evaluated operand, not a constant chosen by the observer. -/
def releaseValue (value : Int) : Release Unit Int :=
  ⟨⟨⟨0⟩, ()⟩, value, "released operand", []⟩

def releaseZero : Release Unit Int := releaseValue 0

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
    | some (.release, [v], _) => [releaseValue v]
    | _ => []
  choices := fun _ _ _ => [()]

/-- Negative local values are secret; nonnegative local values are public. The shared store remains
secret, which is what lets the input configurations be low-equivalent. -/
abbrev labelling : Labelling ops where
  PublicValue := Int
  project := fun v => if v < 0 then none else some v
  lowStore := fun _ _ => True

/-- The policy authorizes exactly the typed release site used by the examples. -/
abbrev policy : ReleasePolicy Unit := ⟨[⟨⟨0⟩, ()⟩]⟩

/-- Helper (computation). The answer representation permits exactly its specified answer. (approved: PENDING). -/
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

/-- Helper (inversion). Identifies the next step of this fixed example machine. (approved: PENDING).

Fixed external choices make executions from either example start configuration deterministic. -/
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

/-- Helper (inversion). Calculates traces of the fixed four-step example. (approved: PENDING).

Every complete run of the secure four-step example has exactly the secret-dependent traces
below. In particular, its release record carries the actual secret operand. -/
theorem secure_run_unique (secret : Int) (hidden : Nat := 0)
    (r : Run instrumentation program (start secret hidden)) :
    r.observations = [.output secret] ∧
      r.leakage = [.variableTime ⟨0⟩ [secret], .variableTime ⟨1⟩ [secret]] ∧
      r.releases = [releaseValue secret] ∧ r.choices = [(), (), (), ()] ∧
      r.final = ⟨.done, hiddenLocals hidden, secret, []⟩ := by
  obtain ⟨final, ending, hf, observations, leakage, releases, choices, hs⟩ := r
  cases hs with
  | refl => simp [start, Config.final?] at hf
  | step h₁ rest₁ =>
    obtain ⟨rfl, rfl⟩ := step_unique h₁ (.silent (P := program) rfl)
    cases rest₁ with
    | refl => dsimp [timingAfterSeq, timingAfterWork, timingBeforeBody, afterSeq, afterRelease, beforeOutput] at hf <;> simp [Config.final?] at hf
    | step h₂ rest₂ =>
      obtain ⟨rfl, rfl⟩ := step_unique h₂
        (Step.prim (O := ops) (P := program) (p := .release) (vs := [secret]) (st := secret)
          (a := (⟨none, .ok 0, secret⟩ : Answer Int Int Int)) rfl (by simp [allowed]) rfl)
      cases rest₂ with
      | refl => dsimp [timingAfterSeq, timingAfterWork, timingBeforeBody, afterSeq, afterRelease, beforeOutput] at hf <;> simp [Config.final?] at hf
      | step h₃ rest₃ =>
        obtain ⟨rfl, rfl⟩ := step_unique h₃ (.silent (P := program) rfl)
        cases rest₃ with
        | refl => dsimp [timingAfterSeq, timingAfterWork, timingBeforeBody, afterSeq, afterRelease, beforeOutput] at hf <;> simp [Config.final?] at hf
        | step h₄ rest₄ =>
          obtain ⟨rfl, rfl⟩ := step_unique h₄
            (Step.prim (O := ops) (P := program) (p := .output) (vs := [secret]) (st := secret)
              (a := (⟨some secret, .ok 0, secret⟩ : Answer Int Int Int)) rfl (by simp [allowed]) rfl)
          cases rest₄ with
          | refl => exact ⟨rfl, rfl, rfl, rfl, rfl⟩
          | step hs _ => exact (no_step_of_none (P := program)
            (c := (⟨.done, hiddenLocals hidden, secret, []⟩ : Config ops)) rfl _ _ hs).elim

/-- Law (subject `start`).

**Delimited-release NI, proved for the secure example.** Equality of the actual release records
is used to derive equality of the secrets, hence of their public outputs. -/
theorem secure_ni (s₁ s₂ : Int) (hidden₁ hidden₂ : Nat := 0) :
    DelimitedReleaseNI instrumentation program labelling (start s₁ hidden₁) (start s₂ hidden₂) := by
  intro r₁ r₂ _ _ hreleases
  have h₁ := secure_run_unique s₁ hidden₁ r₁
  have h₂ := secure_run_unique s₂ hidden₂ r₂
  have hs : s₁ = s₂ := by
    rw [h₁.2.2.1, h₂.2.2.1] at hreleases
    simpa [releaseValue] using hreleases
  subst s₂
  simp only [Run.publicObservations, h₁.1, h₂.1, h₁.2.2.2.2, h₂.2.2.2.2, Config.final?]

/-- Law (subject `start`).

**Constant time, proved separately for the secure example.** Equal release values likewise make
the two secret-dependent variable-time traces equal. -/
theorem secure_ct (s₁ s₂ : Int) (hidden₁ hidden₂ : Nat := 0) :
    ConstantTime instrumentation program labelling (start s₁ hidden₁) (start s₂ hidden₂) := by
  intro r₁ r₂ _ _ hreleases
  have h₁ := secure_run_unique s₁ hidden₁ r₁
  have h₂ := secure_run_unique s₂ hidden₂ r₂
  have hs : s₁ = s₂ := by
    rw [h₁.2.2.1, h₂.2.2.1] at hreleases
    simpa [releaseValue] using hreleases
  subst s₂
  rw [h₁.2.1, h₂.2.1]

/-- One complete secure execution: declassify the secret, output it, and finish normally. -/
def secureRun (secret : Int) (hidden : Nat := 0) : Run instrumentation program (start secret hidden) where
  final := finished secret (hiddenLocals hidden)
  ending := .normal
  finalEnding := rfl
  observations := [.output secret]
  leakage := [.variableTime ⟨0⟩ [secret], .variableTime ⟨1⟩ [secret]]
  releases := [releaseValue secret]
  choices := [(), (), (), ()]
  steps :=
    InstrumentedSteps.step (I := instrumentation)
      (Step.silent (P := program) (c := start secret hidden) (c' := afterSeq .secret .secret secret (hiddenLocals hidden)) rfl) <|
    InstrumentedSteps.step (I := instrumentation)
      (Step.prim (O := ops) (P := program) (c := afterSeq .secret .secret secret (hiddenLocals hidden))
        (c' := afterRelease .secret secret (hiddenLocals hidden)) (p := .release) (vs := [secret]) (st := secret)
        (a := (⟨none, .ok 0, secret⟩ : Answer Int Int Int)) rfl (by simp [allowed]) rfl) <|
    InstrumentedSteps.step (I := instrumentation)
      (Step.silent (P := program) (c := afterRelease .secret secret (hiddenLocals hidden))
        (c' := beforeOutput .secret secret (hiddenLocals hidden)) rfl) <|
    InstrumentedSteps.step (I := instrumentation)
      (Step.prim (O := ops) (P := program) (c := beforeOutput .secret secret (hiddenLocals hidden))
        (c' := finished secret (hiddenLocals hidden)) (p := .output) (vs := [secret]) (st := secret)
        (a := (⟨some secret, .ok 0, secret⟩ : Answer Int Int Int)) rfl (by simp [allowed]) rfl) <|
    InstrumentedSteps.refl (I := instrumentation) (P := program) (finished secret (hiddenLocals hidden))

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING).

Distinct unreleased secrets inhabit every delimited-release premise. -/
example : start 7 1 ≠ start 7 2 ∧
    LowEquivalent labelling (start 7 1) (start 7 2) ∧
    (secureRun 7 1).choices = (secureRun 7 2).choices ∧
    (secureRun 7 1).releases = (secureRun 7 2).releases := by
  refine ⟨?_, ?_, rfl, rfl⟩
  · intro h; have hl := congrArg Config.locals h; simp [start, hiddenLocals] at hl
  · simp [LowEquivalent, publicLocals, labelling, start, hiddenLocals]

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : DelimitedReleaseNI instrumentation program labelling (start 7 1) (start 7 2) :=
  secure_ni 7 7 1 2

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : ConstantTime instrumentation program labelling (start 7 1) (start 7 2) :=
  secure_ct 7 7 1 2

/-- One complete leaky execution: release public zero but output the secret. -/
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
      (Step.silent (P := program) (c := leakyStart secret)
        (c' := afterSeq (.lit 0) .secret secret) rfl) <|
    InstrumentedSteps.step (I := instrumentation)
      (Step.prim (O := ops) (P := program) (c := afterSeq (.lit 0) .secret secret)
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

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING).

The concrete instrumented run projects to C1, not to a second semantics. -/
example : ∃ events, Steps program (start 1) events (finished 1) :=
  (secureRun 1).steps.toSteps

/-- Helper (inversion). Calculates traces of the fixed timing example. (approved: PENDING).

Every complete timing run has public output seven but leaks its secret work operand. -/
theorem timing_run_unique (secret : Int)
    (r : Run instrumentation program (timingStart secret)) :
    r.observations = [.output 7] ∧
      r.leakage = [.variableTime ⟨2⟩ [secret], .variableTime ⟨0⟩ [0], .variableTime ⟨1⟩ [7]] ∧
      r.releases = [releaseZero] ∧ r.choices = [(), (), (), (), (), (), ()] ∧
      r.final = ⟨.done, [], secret, []⟩ := by
  obtain ⟨final, ending, hf, observations, leakage, releases, choices, hs⟩ := r
  cases hs with
  | refl => simp [timingStart, Config.final?] at hf
  | step h₁ rest₁ =>
    obtain ⟨rfl, rfl⟩ := step_unique h₁ (.silent (P := program) rfl)
    cases rest₁ with
    | refl => dsimp [timingAfterSeq, timingAfterWork, timingBeforeBody, afterSeq, afterRelease, beforeOutput] at hf <;> simp [Config.final?] at hf
    | step h₂ rest₂ =>
      obtain ⟨rfl, rfl⟩ := step_unique h₂
        (Step.prim (O := ops) (P := program) (c := timingAfterSeq secret)
          (c' := timingAfterWork secret) (p := .work) (vs := [secret]) (st := secret)
          (a := (⟨none, .ok 0, secret⟩ : Answer Int Int Int)) rfl (by simp [allowed]) rfl)
      cases rest₂ with
      | refl => dsimp [timingAfterSeq, timingAfterWork, timingBeforeBody, afterSeq, afterRelease, beforeOutput] at hf <;> simp [Config.final?] at hf
      | step h₃ rest₃ =>
        obtain ⟨rfl, rfl⟩ := step_unique h₃ (.silent (P := program)
          (c := timingAfterWork secret) (c' := timingBeforeBody secret) rfl)
        cases rest₃ with
        | refl => dsimp [timingAfterSeq, timingAfterWork, timingBeforeBody, afterSeq, afterRelease, beforeOutput] at hf <;> simp [Config.final?] at hf
        | step h₄ rest₄ =>
          obtain ⟨rfl, rfl⟩ := step_unique h₄ (.silent (P := program)
            (c := timingBeforeBody secret) (c' := afterSeq (.lit 0) (.lit 7) secret) rfl)
          cases rest₄ with
          | refl => dsimp [timingAfterSeq, timingAfterWork, timingBeforeBody, afterSeq, afterRelease, beforeOutput] at hf <;> simp [Config.final?] at hf
          | step h₅ rest₅ =>
            obtain ⟨rfl, rfl⟩ := step_unique h₅
              (Step.prim (O := ops) (P := program) (c := afterSeq (.lit 0) (.lit 7) secret)
                (c' := afterRelease (.lit 7) secret) (p := .release) (vs := [0]) (st := secret)
                (a := (⟨none, .ok 0, secret⟩ : Answer Int Int Int)) rfl (by simp [allowed]) rfl)
            cases rest₅ with
            | refl => dsimp [timingAfterSeq, timingAfterWork, timingBeforeBody, afterSeq, afterRelease, beforeOutput] at hf <;> simp [Config.final?] at hf
            | step h₆ rest₆ =>
              obtain ⟨rfl, rfl⟩ := step_unique h₆ (.silent (P := program)
                (c := afterRelease (.lit 7) secret) (c' := beforeOutput (.lit 7) secret) rfl)
              cases rest₆ with
              | refl => dsimp [timingAfterSeq, timingAfterWork, timingBeforeBody, afterSeq, afterRelease, beforeOutput] at hf <;> simp [Config.final?] at hf
              | step h₇ rest₇ =>
                obtain ⟨rfl, rfl⟩ := step_unique h₇
                  (Step.prim (O := ops) (P := program) (c := beforeOutput (.lit 7) secret)
                    (c' := finished secret) (p := .output) (vs := [7]) (st := secret)
                    (a := (⟨some 7, .ok 0, secret⟩ : Answer Int Int Int)) rfl
                    (by simp [allowed]) rfl)
                cases rest₇ with
                | refl => exact ⟨rfl, rfl, rfl, rfl, rfl⟩
                | step hs _ => exact (no_step_of_none (P := program)
                  (c := (⟨.done, [], secret, []⟩ : Config ops)) rfl _ _ hs).elim

/-- One complete seven-step timing execution. -/
def timingRun (secret : Int) : Run instrumentation program (timingStart secret) where
  final := finished secret
  ending := .normal
  finalEnding := rfl
  observations := [.output 7]
  leakage := [.variableTime ⟨2⟩ [secret], .variableTime ⟨0⟩ [0], .variableTime ⟨1⟩ [7]]
  releases := [releaseZero]
  choices := [(), (), (), (), (), (), ()]
  steps :=
    InstrumentedSteps.step (I := instrumentation)
      (Step.silent (P := program) (c := timingStart secret) (c' := timingAfterSeq secret) rfl) <|
    InstrumentedSteps.step (I := instrumentation)
      (Step.prim (O := ops) (P := program) (c := timingAfterSeq secret)
        (c' := timingAfterWork secret) (p := .work) (vs := [secret]) (st := secret)
        (a := (⟨none, .ok 0, secret⟩ : Answer Int Int Int)) rfl (by simp [allowed]) rfl) <|
    InstrumentedSteps.step (I := instrumentation)
      (Step.silent (P := program) (c := timingAfterWork secret)
        (c' := timingBeforeBody secret) rfl) <|
    InstrumentedSteps.step (I := instrumentation)
      (Step.silent (P := program) (c := timingBeforeBody secret)
        (c' := afterSeq (.lit 0) (.lit 7) secret) rfl) <|
    InstrumentedSteps.step (I := instrumentation)
      (Step.prim (O := ops) (P := program) (c := afterSeq (.lit 0) (.lit 7) secret)
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

/-- Law (subject `timingStart`).

NI does not use the release premise here: every timing run emits the same public output. -/
theorem timing_ni (s₁ s₂ : Int) :
    DelimitedReleaseNI instrumentation program labelling (timingStart s₁) (timingStart s₂) := by
  intro r₁ r₂ _ _ _
  have h₁ := timing_run_unique s₁ r₁
  have h₂ := timing_run_unique s₂ r₂
  simp only [Run.publicObservations, h₁.1, h₂.1, h₁.2.2.2.2, h₂.2.2.2.2, Config.final?]

/-- Law (subject `timingStart`).

The same program is not CT: secret-dependent work produces different nonempty leakage traces. -/
theorem timing_not_ct : ¬ ConstantTime instrumentation program labelling (timingStart 1) (timingStart 2) := by
  intro h
  have hl := h (timingRun 1) (timingRun 2)
    (by simp [LowEquivalent, publicLocals, labelling, timingStart])
    (by simp [timingRun]) (by simp [timingRun, releaseZero, releaseValue])
  simp [timingRun] at hl

/-- A deliberately permissive output oracle admits two distinct labels for one machine request. -/
abbrev permissiveOps : Operands := { ops with allows := fun p vs st a =>
  match p, vs with
  | .output, [_] => true
  | _, _ => allowed p vs st a }

def permissiveBeforeOutput : Config permissiveOps :=
  ⟨.exec (.prim none .output [.lit 7] ⟨1⟩), [], 0, []⟩
def permissiveFinished : Config permissiveOps := ⟨.done, [], 0, []⟩

/-- Helper (computation). Concrete fixture or counterexample calculation, not independent contract evidence. (approved: PENDING). -/
theorem permissive_step_not_unique :
    ∃ (l₁ l₂ : Label permissiveOps) (c₁ c₂ : Config permissiveOps),
      Step (⟨[]⟩ : Program permissiveOps) permissiveBeforeOutput l₁ c₁ ∧
      Step (⟨[]⟩ : Program permissiveOps) permissiveBeforeOutput l₂ c₂ ∧
      (l₁ ≠ l₂ ∨ c₁ ≠ c₂) := by
  refine ⟨.emit 7, .emit 8, permissiveFinished, permissiveFinished, ?_, ?_, Or.inl ?_⟩
  · exact Step.prim (O := permissiveOps) (P := (⟨[]⟩ : Program permissiveOps))
      (p := .output) (vs := [7]) (st := 0)
      (a := (⟨some 7, .ok 0, 0⟩ : Answer Int Int Int)) rfl rfl rfl
  · exact Step.prim (O := permissiveOps) (P := (⟨[]⟩ : Program permissiveOps))
      (p := .output) (vs := [7]) (st := 0)
      (a := (⟨some 8, .ok 0, 0⟩ : Answer Int Int Int)) rfl rfl rfl
  · intro h
    injection h with hv
    exact (by decide : (7 : Int) ≠ 8) hv

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING).

Planted trace mutations: reversed leakage and a missing final observation are detectable. -/
example : (secureRun 1).leakage ≠ [.variableTime ⟨1⟩ [1], .variableTime ⟨0⟩ [1]] := by
  intro h
  simp [secureRun] at h

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : (secureRun 1).publicObservations ≠ [.output 1] := by decide

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING).

A nonfinal execution prefix is not a complete run. -/
example : ¬ ∃ ending, (afterRelease .secret 1).final? = some ending := by
  simp [afterRelease, Config.final?]

/-- A configuration with one secret and one public local. -/
def mixedConfig (secret publicValue : Int) : Config ops :=
  ⟨.done, [(⟨"secret", 0⟩, secret), (⟨"public", 1⟩, publicValue)], 0, []⟩

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING).

Secret local changes are invisible; a planted public local change rejects low-equivalence. -/
example : LowEquivalent labelling (mixedConfig (-1) 7) (mixedConfig (-2) 7) := by
  simp [LowEquivalent, publicLocals, labelling, mixedConfig]

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : ¬ LowEquivalent labelling (mixedConfig (-1) 7) (mixedConfig (-2) 8) := by
  simp [LowEquivalent, publicLocals, labelling, mixedConfig]

/-- A store-sensitive labelling supplies the corresponding public-store negative control. -/
abbrev storeSensitiveLabelling : Labelling ops where
  PublicValue := Int
  project := fun _ => none
  lowStore := fun s₁ s₂ => s₁ = s₂

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : ¬ LowEquivalent storeSensitiveLabelling (leakyStart 1) (leakyStart 2) := by
  simp [LowEquivalent, storeSensitiveLabelling, leakyStart]

/-- Helper (computation). Concrete fixture or counterexample calculation, not independent contract evidence. (approved: PENDING). -/
theorem leaky_low (s₁ s₂ : Int) : LowEquivalent labelling (leakyStart s₁) (leakyStart s₂) := by
  simp [LowEquivalent, publicLocals, labelling, leakyStart]

/-- Helper (computation). Concrete fixture or counterexample calculation, not independent contract evidence. (approved: PENDING). -/
theorem release_authorized (secret : Int) : policy.Authorized (leakyRun secret).releases := by
  simp [ReleasePolicy.Authorized, ReleasePolicy.Permits, leakyRun, releaseZero, releaseValue]

/-- Helper (computation). Concrete fixture or counterexample calculation, not independent contract evidence. (approved: PENDING).

**Authorization does not imply NI.** These two executions have low-equivalent inputs, matched
choices and identical authorized releases, yet output different public observations. -/
theorem authorized_release_not_ni :
    policy.Authorized (leakyRun 1).releases ∧ LowEquivalent labelling (leakyStart 1) (leakyStart 2) ∧
      (leakyRun 1).choices = (leakyRun 2).choices ∧
      (leakyRun 1).releases = (leakyRun 2).releases ∧
      (leakyRun 1).publicObservations ≠ (leakyRun 2).publicObservations := by
  refine ⟨release_authorized 1, leaky_low 1 2, rfl, rfl, ?_⟩
  decide

/-- Helper (computation). Concrete fixture or counterexample calculation, not independent contract evidence. (approved: PENDING).

**Authorization does not imply CT.** The same premises and authorized release coexist with
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

/-- Law (subject `Account.alloc`). -/
theorem Account.alloc_zero (a : Account) (h : a.WF) : a.alloc 0 = a := by
  cases a
  simp_all [Account.alloc, Account.WF]

/-- Helper (computation). Extensional identity of the functional memory representation. (approved: PENDING). -/
theorem Memory.set_same (m : Memory) (lineage : LineageId) :
    Memory.set m lineage (m lineage) = m := by
  funext l
  by_cases h : l = lineage <;> simp [Memory.set, h]

/-- Helper (arithmetic). Reuses historical WF and initializes the local bound to live bytes. (approved: PENDING). -/
theorem Account.enter_wf (a : Account) (h : a.WF) : a.enter.WF := by
  exact ⟨h.1, Nat.le_refl _⟩

/-- Helper (arithmetic). The new live operand is bounded by both maxima by construction. (approved: PENDING). -/
theorem Account.alloc_wf (a : Account) (bytes : Nat) : (a.alloc bytes).WF := by
  exact ⟨Nat.le_max_right _ _, Nat.le_max_right _ _⟩

/-- Helper (arithmetic). Subtraction cannot increase live bytes. (approved: PENDING). -/
theorem Account.free_wf (a : Account) (bytes : Nat) (h : a.WF) : (a.free bytes).WF := by
  exact ⟨Nat.le_trans (Nat.sub_le _ _) h.1, Nat.le_trans (Nat.sub_le _ _) h.2⟩

/-- Helper (arithmetic). Zero live bytes satisfy nonnegative peak inequalities by construction. (approved: PENDING). -/
theorem Account.reset_wf (a : Account) : a.reset.WF := by
  simp [Account.WF, Account.reset]

/-- Helper (inversion). Lifts account arithmetic through one successful transition. (approved: PENDING).

Every successful accounting action preserves well-formedness at every lineage. -/
theorem apply_wf {m : Memory} {action : Action} {m' : Memory}
    (h : apply m action = some m') (hwf : ∀ l, (m l).WF) : ∀ l, (m' l).WF := by
  cases action with
  | alloc lineage bytes =>
    cases h
    intro l
    by_cases hl : l = lineage
    · subst l
      simpa [Memory.set] using Account.alloc_wf (m lineage) bytes
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

/-- Helper (inversion). Eliminates a fixed successful result for the sequence_wf executor obligation. (approved: PENDING).

Every successful sequence preserves well-formedness. -/
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

/-- Law (subject `apply`).

A zero-byte allocation changes no account at any lineage. -/
theorem apply_alloc_zero {m : Memory} (hwf : ∀ l, (m l).WF) (lineage : LineageId) :
    apply m (.alloc lineage 0) = some m := by
  simp only [apply, Account.alloc_zero (m lineage) (hwf lineage), Memory.set_same]

/-- Helper (computation). Unfolds the free update and setter; whole-sequence conservation is separate. (approved: PENDING).

A successful free conserves cumulative requested bytes and both peaks, changes live bytes by
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

/-- Helper (computation). Calculates reset fields against the independent exact conservation predicate. (approved: PENDING).

A successful reset satisfies the independent conservation predicate with the actual live bytes
before the transition as its outflow. Other lineages are unchanged. -/
theorem reset_conservation {m m' : Memory} {lineage : LineageId}
    (h : apply m (.reset lineage) = some m') :
    ResetConservation (m lineage) (m' lineage) (resetOutflow lineage m (.reset lineage)) ∧
      ∀ l, l ≠ lineage → m' l = m l := by
  cases h
  constructor
  · simp [ResetConservation, resetOutflow, Memory.set, Account.reset]
  · intro l hl
    simp [Memory.set, hl]

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING).

Non-trivial reset conservation: five live bytes are released and all history is retained. -/
example : ResetConservation ⟨100, 5, 12, 5⟩ (Account.reset ⟨100, 5, 12, 5⟩) 5 := by
  simp [ResetConservation, Account.reset]

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING).

Planted bad reset record: four bytes cannot account for the five bytes actually released. -/
example : ¬ ResetConservation ⟨100, 5, 12, 5⟩ (Account.reset ⟨100, 5, 12, 5⟩) 4 := by
  simp [ResetConservation, Account.reset]

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING).

Over-recording and a partial reset both violate exact reset conservation. -/
example : ¬ ResetConservation ⟨100, 5, 12, 5⟩ (Account.reset ⟨100, 5, 12, 5⟩) 6 := by
  simp [ResetConservation, Account.reset]

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : ¬ ResetConservation ⟨100, 5, 12, 5⟩ ⟨100, 1, 12, 5⟩ 4 := by
  simp [ResetConservation]

/-- Helper (computation). Unfolds the outflow observer on a successful transition. (approved: PENDING). -/
@[simp] theorem resetBytes_cons {m m' : Memory} {action : Action} {rest : List Action}
    (lineage : LineageId) (h : apply m action = some m') :
    resetBytes lineage m (action :: rest) =
      resetOutflow lineage m action + resetBytes lineage m' rest := by
  simp [resetBytes, h]

/-- Helper (arithmetic). Induction on a fixed successful result, used by sequence_conservation. (approved: PENDING).

**Whole-sequence conservation.** Requested bytes increase by exactly the sequence's allocation
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

/-- Helper (arithmetic). Maximum-retention calculation for a single transition. (approved: PENDING).

No successful action lowers either the historical or function-local peak of any lineage. -/
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

/-- Helper (arithmetic). Induction on a fixed successful result, used by sequence_peaks. (approved: PENDING).

No successful sequence lowers either the historical or function-local peak of any lineage. -/
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

/-- Helper (computation). Single-update maximum identity used by trajectory induction. (approved: PENDING).

Each successful transition records the maximum of its old peak and new live bytes.
This is a calculation used to connect the implementation to the independent trajectory observer. -/
theorem apply_peak_exact {m m' : Memory} {action : Action}
    (hwf : ∀ l, (m l).WF) (h : apply m action = some m') (lineage : LineageId) :
    (m' lineage).functionPeak = max (m lineage).functionPeak (m' lineage).live := by
  cases action with
  | alloc l bytes =>
    cases h
    by_cases hl : lineage = l
    · subst lineage; simp [Memory.set, Account.alloc]
    · simp [Memory.set, hl, Nat.max_eq_left (hwf lineage).2]
  | free l bytes =>
    simp only [apply] at h
    split at h
    next =>
      cases h
      by_cases hl : lineage = l
      · subst lineage
        simp only [Memory.set, ↓reduceIte, Account.free]
        exact (Nat.max_eq_left (Nat.le_trans (Nat.sub_le _ _) (hwf l).2)).symm
      · simp [Memory.set, hl, Nat.max_eq_left (hwf lineage).2]
    next => cases h
  | reset l =>
    cases h
    by_cases hl : lineage = l
    · subst lineage; simp [Memory.set, Account.reset]
    · simp [Memory.set, hl, Nat.max_eq_left (hwf lineage).2]

/-- Helper (arithmetic). Induction relating a fixed result to independent live states; used by sequence_peak_exact. (approved: PENDING).

The recorded local peak equals an independently observed maximum over the live-byte trajectory,
including entry. Initialization at function entry is required, rather than an exit bound premise. -/
theorem run_peak_exact {m m' : Memory} {actions : List Action}
    (hwf : ∀ l, (m l).WF) (h : run m actions = some m') (lineage : LineageId) :
    (m' lineage).functionPeak = max (m lineage).functionPeak (observedPeak lineage m actions) := by
  induction actions generalizing m with
  | nil =>
    cases h
    exact (Nat.max_eq_left (hwf lineage).2).symm
  | cons action rest ih =>
    cases ha : apply m action with
    | none => simp [run, ha] at h
    | some m₁ =>
      have hr : run m₁ rest = some m' := by simpa [run, ha] using h
      rw [ih (apply_wf ha hwf) hr, apply_peak_exact hwf ha lineage]
      simp only [observedPeak, ha]
      rw [Nat.max_assoc]
      have hrest : (m₁ lineage).live ≤ observedPeak lineage m₁ rest := by
        cases rest with
        | nil => exact Nat.le_refl _
        | cons a xs =>
          simp only [observedPeak]
          split
          · exact Nat.le_refl _
          · exact Nat.le_max_left _ _
      rw [Nat.max_eq_right hrest, ← Nat.max_assoc, Nat.max_eq_left (hwf lineage).2]

/-! ## Concrete accounting controls -/

/-- A nonempty well-formed account and a second lineage with independent accounting. -/
def accountA : Account := ⟨100, 5, 12, 9⟩
def accountB : Account := ⟨7, 2, 2, 2⟩
def memoryA : Memory := Memory.set (Memory.set Memory.empty ⟨0⟩ accountA) ⟨1⟩ accountB

def conservationActions : List Action := [.alloc ⟨0⟩ 8, .free ⟨0⟩ 3, .reset ⟨0⟩]
def twoLineageActions : List Action :=
  [.alloc ⟨0⟩ 8, .alloc ⟨1⟩ 4, .free ⟨0⟩ 3, .reset ⟨0⟩,
    .alloc ⟨0⟩ 6, .reset ⟨1⟩, .reset ⟨0⟩]

/-- Peaks lowered below live bytes, and a reset that incorrectly clears both peaks. -/
def badPeakAccount : Account := { accountA.alloc 10 with peak := 14, functionPeak := 14 }
def badResetPeaks : Account := { accountA.reset with peak := 0, functionPeak := 0 }

/-- Helper (computation). Concrete fixture or counterexample calculation, not independent contract evidence. (approved: PENDING). -/
theorem memoryA_wf : ∀ l, (memoryA l).WF := by
  intro l
  by_cases h₀ : l = ⟨0⟩
  · subst l
    simp [memoryA, Memory.set, Account.WF, accountA]
  · by_cases h₁ : l = ⟨1⟩
    · subst l
      simp [memoryA, Memory.set, Account.WF, accountB]
    · simp [memoryA, Memory.set, h₀, h₁, Account.WF, Memory.empty, Account.empty]

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING).

Empty initialization and explicit well-formedness controls. -/
example : Account.empty.WF ∧ Memory.empty ⟨0⟩ = Account.empty := by
  simp [Account.WF, Memory.empty, Account.empty]

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : Memory.empty ⟨0⟩ ≠ accountA := by decide

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : accountA.WF := by simp [Account.WF, accountA]

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : ¬ (⟨0, 5, 0, 0⟩ : Account).WF := by simp [Account.WF]

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING).

`alloc_zero`: a well-formed nonzero-history instance and an ill-formed planted counterexample. -/
example : accountA.alloc 0 = accountA := Account.alloc_zero _ (by simp [Account.WF, accountA])

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : (⟨0, 5, 0, 0⟩ : Account).alloc 0 ≠ ⟨0, 5, 0, 0⟩ := by decide

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : accountA.alloc 1 ≠ accountA := by decide

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING).

`Memory.set_same` and two-lineage setter isolation; a planted second-lineage change fails. -/
example : Memory.set memoryA ⟨0⟩ (memoryA ⟨0⟩) = memoryA := Memory.set_same _ _

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : Memory.set memoryA ⟨0⟩ (accountA.alloc 1) ⟨1⟩ = accountB := rfl

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : Memory.set memoryA ⟨1⟩ (accountB.alloc 1) ⟨1⟩ ≠ accountB := by decide

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING).

`enter_wf`: nonzero live-on-entry starts the local peak at five, not below it. -/
example : accountA.enter = ⟨100, 5, 12, 5⟩ ∧ accountA.enter.WF :=
  ⟨rfl, Account.enter_wf _ (by simp [Account.WF, accountA])⟩

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : ¬ ({ accountA.enter with functionPeak := 4 } : Account).WF := by
  simp [Account.WF, accountA, Account.enter]

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING).

`alloc_wf`: cumulative requests, peak growth, and retention of already-greater peaks. -/
example : accountA.alloc 10 = ⟨110, 15, 15, 15⟩ ∧ (accountA.alloc 10).WF :=
  ⟨rfl, Account.alloc_wf _ _⟩

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : accountA.alloc 1 = ⟨101, 6, 12, 9⟩ := rfl

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : ¬ badPeakAccount.WF := by
  simp [badPeakAccount, Account.WF, accountA, Account.alloc]

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING).

`free_wf`: history and both peaks survive; a planted live/peak inconsistency fails. -/
example : accountA.free 3 = ⟨100, 2, 12, 9⟩ ∧ (accountA.free 3).WF :=
  ⟨rfl, Account.free_wf _ _ (by simp [Account.WF, accountA])⟩

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : ¬ ({ accountA.free 3 with live := 13 } : Account).WF := by
  simp [Account.WF, accountA, Account.free]

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING).

`reset_wf`: zero live bytes satisfy the inequalities; a separate control rejects cleared history. -/
example : accountA.reset = ⟨100, 0, 12, 9⟩ ∧ accountA.reset.WF :=
  ⟨rfl, Account.reset_wf _⟩

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : badResetPeaks ≠ accountA.reset := by decide

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING).

`apply_wf`: a nonzero allocation and independent controls for touched/untouched corruption. -/
example : (apply memoryA (.alloc ⟨0⟩ 10)).map (· ⟨0⟩) = some ⟨110, 15, 15, 15⟩ := by decide

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : ¬ ({ accountB with peak := 1 } : Account).WF := by
  simp [Account.WF, accountB]

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING).

`apply_alloc_zero`: the whole nonempty memory is unchanged; another lineage cannot be changed. -/
example : apply memoryA (.alloc ⟨0⟩ 0) = some memoryA := apply_alloc_zero memoryA_wf _

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : Memory.set memoryA ⟨1⟩ (accountB.alloc 1) ⟨1⟩ ≠ memoryA ⟨1⟩ := by decide

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING).

`run_wf` and `run`: the actual 8/3/reset run and a successful prefix followed by refusal. -/
example : (run memoryA conservationActions).map (· ⟨0⟩) = some ⟨108, 0, 13, 13⟩ := by decide

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : (run memoryA conservationActions).map (· ⟨1⟩) = some accountB := by decide

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : run Memory.empty [.alloc ⟨0⟩ 8, .free ⟨0⟩ 9, .alloc ⟨0⟩ 1] = none := by decide

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : ¬ badPeakAccount.WF := by simp [badPeakAccount, Account.WF, accountA, Account.alloc]

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING).

`free_conservation`: the actual free and planted requested/history/local-peak corruption. -/
example : (apply memoryA (.free ⟨0⟩ 3)).map (· ⟨0⟩) = some ⟨100, 2, 12, 9⟩ := by decide

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : ¬ (99 = accountA.requested ∧ 2 = accountA.live - 3 ∧
    12 = accountA.peak ∧ 9 = accountA.functionPeak) := by decide

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : ¬ (8 = accountA.functionPeak) := by decide

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING).

`run_conservation`: the actual run and lost-allocation/double-free/bad-reset mutations. -/
example : 108 = accountA.requested + allocated ⟨0⟩ conservationActions ∧
    (0 : Int) = accountA.live + allocated ⟨0⟩ conservationActions -
      freed ⟨0⟩ conservationActions - resetBytes ⟨0⟩ memoryA conservationActions := by decide

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : ¬ (108 = accountA.requested + 7) := by decide

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : ¬ ((0 : Int) = 5 + 7 - 3 - 10) := by decide

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : ¬ ((0 : Int) = 5 + 8 - 6 - 10) := by decide

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : ¬ ((0 : Int) = 5 + 8 - 3 - 9) := by decide

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING).

`apply_peak_monotone` and `run_peak_monotone`: free/reset preserve both peaks; clearing or
lowering either peak in a planted target rejects the monotonicity claim. -/
example : accountA.peak ≤ (accountA.alloc 10).peak ∧
    accountA.functionPeak ≤ (accountA.alloc 10).functionPeak := by decide

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : accountA.peak ≤ (accountA.free 3).peak ∧
    accountA.functionPeak ≤ (accountA.free 3).functionPeak := by decide

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : accountA.peak ≤ accountA.reset.peak ∧
    accountA.functionPeak ≤ accountA.reset.functionPeak := by decide

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : ¬ (accountA.peak ≤ badResetPeaks.peak ∧
    accountA.functionPeak ≤ badResetPeaks.functionPeak) := by decide

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : ¬ (accountA.peak ≤ 11 ∧ accountA.functionPeak ≤ 8) := by decide

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING).

Live bytes, unlike peaks, decrease on a real free. -/
example : ∃ m m' : Memory, ∃ l : LineageId,
    apply m (.free l 3) = some m' ∧ ¬ (m l).live ≤ (m' l).live := by
  refine ⟨memoryA, Memory.set memoryA ⟨0⟩ (accountA.free 3), ⟨0⟩, rfl, ?_⟩
  decide

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING).

Interleaved two-lineage allocation/free and multi-reset counting controls. -/
example : allocated ⟨0⟩ twoLineageActions = 14 ∧ allocated ⟨1⟩ twoLineageActions = 4 := by decide

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : freed ⟨0⟩ twoLineageActions = 3 ∧ freed ⟨1⟩ twoLineageActions = 0 := by decide

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : resetBytes ⟨0⟩ memoryA twoLineageActions = 16 ∧
    resetBytes ⟨1⟩ memoryA twoLineageActions = 6 := by decide

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : allocated ⟨0⟩ twoLineageActions ≠ 4 := by decide

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : freed ⟨0⟩ twoLineageActions ≠ 16 := by decide

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : resetBytes ⟨0⟩ memoryA twoLineageActions ≠ 10 := by decide

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : resetBytes ⟨1⟩ memoryA twoLineageActions ≠ 0 := by decide

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : (run memoryA twoLineageActions).map (· ⟨0⟩) = some ⟨114, 0, 13, 13⟩ := by decide

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : (run memoryA twoLineageActions).map (· ⟨1⟩) = some ⟨11, 0, 6, 6⟩ := by decide

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING).

Established and unestablished bound controls, including optional footprint evidence. -/
example : Bound.Sound 10 ⟨12, .boundEstablished⟩ := by simp [Bound.Sound]

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : ¬ Bound.Sound 10 ⟨9, .boundEstablished⟩ := by simp [Bound.Sound]

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : Bound.SoundOptional (some 10) ⟨12, .boundEstablished⟩ := by
  simp [Bound.SoundOptional]

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : ¬ Bound.SoundOptional (some 10) ⟨9, .boundEstablished⟩ := by
  simp [Bound.SoundOptional]

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : ¬ Bound.SoundOptional none ⟨12, .boundEstablished⟩ := by
  simp [Bound.SoundOptional]

/-- Helper (congruence). Transports bounds through conservation; peak and footprint are explicit premises, not new evidence. (approved: PENDING).

An arbitrary report is sound for an actual run when its established limits bound quantities
obtained independently from the action sequence and function entry. Requested and net-change bounds
are translated to entry/exit accounts by `run_conservation`; unestablished fields assert nothing. -/
theorem run_report_transport {m m' : Memory} {actions : List Action} {lineage : LineageId}
    {footprint : Option Nat} {r : LineageReport} (hlineage : r.lineage = lineage)
    (h : run m actions = some m')
    (hrequested : Bound.Sound (allocated lineage actions) r.requested)
    (hnet : Bound.Sound ((allocated lineage actions : Int) - freed lineage actions -
      resetBytes lineage m actions) r.netChange)
    (hpeak : Bound.Sound (m' lineage).functionPeak r.peak)
    (hfootprint : Bound.SoundOptional footprint r.footprint) :
    LineageReport.Sound lineage (m lineage) (m' lineage) footprint r := by
  have hc := run_conservation h lineage
  have hreqEq : (m' lineage).requested - (m lineage).requested = allocated lineage actions := by
    omega
  have hnetEq : ((m' lineage).live : Int) - ((m lineage).live : Int) =
      (allocated lineage actions : Int) - freed lineage actions - resetBytes lineage m actions := by
    omega
  simp only [LineageReport.Sound]
  rw [hreqEq, hnetEq]
  exact ⟨hlineage, hrequested, hnet, hpeak, hfootprint⟩

/-- Helper (congruence). Combines analysis bounds with trajectory equality for report_certified. (approved: PENDING).

Independent analysis bounds establish a report for an actual function run. Peak evidence is
about the maximum of observed live states, never the recorded exit peak. Footprint is supplied by
the named allocator contract and its bound remains an explicit allocator premise. -/
theorem established_report_sound {m m' : Memory} {actions : List Action} {lineage : LineageId}
    {allocator : AllocatorContract} {requestedLimit : Nat} {netLimit : Int}
    {peakLimit footprintLimit : Nat}
    (hwf : ∀ l, (m l).WF) (hentry : (m lineage).functionPeak = (m lineage).live)
    (h : run m actions = some m') (hrequested : allocated lineage actions ≤ requestedLimit)
    (hnet : (allocated lineage actions : Int) - freed lineage actions -
      resetBytes lineage m actions ≤ netLimit)
    (hpeak : observedPeak lineage m actions ≤ peakLimit)
    (hfootprint : ∃ bytes, allocator.footprint lineage m actions = some bytes ∧
      bytes ≤ footprintLimit) :
    LineageReport.Sound lineage (m lineage) (m' lineage) (allocator.footprint lineage m actions)
      ⟨lineage, ⟨requestedLimit, .boundEstablished⟩, ⟨netLimit, .boundEstablished⟩,
        ⟨peakLimit, .boundEstablished⟩, ⟨footprintLimit, .boundEstablished⟩⟩ := by
  apply run_report_transport rfl h
  · intro _; exact hrequested
  · intro _; exact hnet
  · intro _
    rw [run_peak_exact hwf h lineage, hentry]
    apply Nat.max_le.mpr
    constructor
    · apply Nat.le_trans _ hpeak
      cases actions with
      | nil => exact Nat.le_refl _
      | cons a xs =>
        simp only [observedPeak]
        split
        · exact Nat.le_refl _
        · exact Nat.le_max_left _ _
    · exact hpeak
  · intro _; exact hfootprint

/-- Law (subject `run`).
Every successful sequence preserves WF. -/
theorem sequence_wf (m : Memory) (actions : List Action) : PreservesWF run m actions := by
  intro hwf m' h
  exact run_wf h hwf

/-- Law (subject `run`).
Every successful sequence conserves requested and live bytes exactly. -/
theorem sequence_conservation (m : Memory) (actions : List Action) (lineage : LineageId) :
    Conserves run m actions lineage := by
  intro m' h
  exact run_conservation h lineage

/-- Law (subject `run`).
Whole-sequence retention of both maxima. -/
theorem sequence_peaks (m : Memory) (actions : List Action) (lineage : LineageId) :
    RetainsPeaks run m actions lineage := by
  intro m' h
  exact run_peak_monotone h lineage

/-- Law (subject `run`).
Recorded peak agrees with the independently observed live trajectory. -/
theorem sequence_peak_exact (m : Memory) (actions : List Action) (lineage : LineageId) :
    TracksPeak run m actions lineage := by
  intro hwf m' h
  exact run_peak_exact hwf h lineage

/-- Law (subject `run`).
Independently analyzed quantities certify every successful result;
footprint evidence belongs to the named allocator contract. -/
theorem report_certified (m : Memory) (actions : List Action) (allocator : AllocatorContract)
    (lineage : LineageId) (requestedLimit : Nat) (netLimit : Int) (peakLimit footprintLimit : Nat) :
    CertifiesReport run m actions allocator lineage requestedLimit netLimit peakLimit footprintLimit := by
  intro hwf hentry hreq hnet hpeak hfoot m' h
  exact established_report_sound hwf hentry h hreq hnet hpeak hfoot

/-- Example allocator contract: its footprint is the independent logical trajectory maximum.
A real allocator may include metadata and slack; no such equality is assumed generically. -/
def demoAllocator : AllocatorContract := ⟨fun lineage m actions =>
  some (observedPeak lineage m actions)⟩

/-- Entry for the concrete report example: 100 historical requested bytes and two live bytes. -/
def reportEntry : Account := ⟨100, 2, 100, 2⟩

def reportEntryMemory : Memory := Memory.set Memory.empty ⟨0⟩ reportEntry

def reportActions : List Action := [.alloc ⟨0⟩ 8, .free ⟨0⟩ 3]

/-- The actual intermediate states of `reportActions`, retained explicitly so `run` is `rfl`. -/
def reportExitMemory : Memory :=
  Memory.set (Memory.set reportEntryMemory ⟨0⟩ (reportEntry.alloc 8)) ⟨0⟩
    ((reportEntry.alloc 8).free 3)

def reportExit : Account := ⟨108, 7, 100, 10⟩

/-- Helper (computation). Reduces the explicit fixture memory at lineage zero. (approved: PENDING). -/
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

/-- Helper (computation). Concrete fixture or counterexample calculation, not independent contract evidence. (approved: PENDING).

The independent report 8/5/10/12 is sound for the actual 100+8 requested bytes, two-byte entry,
allocation of eight and free of three. -/
theorem independent_report_sound :
    LineageReport.Sound ⟨0⟩ reportEntry reportExit (some 10) establishedReport := by
  have h := established_report_sound (m := reportEntryMemory) (m' := reportExitMemory)
    (actions := reportActions) (lineage := ⟨0⟩) (requestedLimit := 8) (netLimit := 5)
    (peakLimit := 10) (footprintLimit := 12) (allocator := demoAllocator)
    (by
      intro l
      by_cases hl : l = ⟨0⟩
      · subst l; simp [reportEntryMemory, Memory.set, reportEntry, Account.WF]
      · simp [reportEntryMemory, Memory.set, hl, Memory.empty, Account.empty, Account.WF])
    rfl rfl
    (by decide) (by decide) (by decide) ⟨10, rfl, by decide⟩
  exact h

/-- Helper (computation). Concrete fixture or counterexample calculation, not independent contract evidence. (approved: PENDING).

The established fields of the mixed-status report constrain the same actual run; its unknown
net-change field makes no numeric claim. -/
theorem mixed_status_report_sound :
    LineageReport.Sound ⟨0⟩ reportEntry reportExit (some 10) mixedStatusReport := by
  have h := run_report_transport (m := reportEntryMemory) (m' := reportExitMemory)
    (actions := reportActions) (lineage := ⟨0⟩) (r := mixedStatusReport) (footprint := some 10) rfl rfl
    (by simp [mixedStatusReport, Bound.Sound, reportActions, allocated])
    (by simp [mixedStatusReport, Bound.Sound])
    (by simp [mixedStatusReport, Bound.Sound, reportExitMemory_lineage, reportExit])
    (by simp [mixedStatusReport, Bound.SoundOptional])
  simpa only [reportEntryMemory, Memory.set, ↓reduceIte, reportExitMemory_lineage] using h

/-- Helper (computation). Concrete fixture or counterexample calculation, not independent contract evidence. (approved: PENDING).

A function with an empty actual run establishes zero local request and peak bounds even when
earlier execution requested 100 bytes. -/
theorem empty_function_report_sound :
    LineageReport.Sound ⟨0⟩ (Account.enter ⟨100, 0, 100, 0⟩) (Account.enter ⟨100, 0, 100, 0⟩) none
      ⟨⟨0⟩, ⟨0, .boundEstablished⟩, ⟨0, .boundEstablished⟩,
        ⟨0, .boundEstablished⟩, ⟨0, .notEstablished⟩⟩ := by
  apply run_report_transport
    (m := Memory.set Memory.empty ⟨0⟩ (Account.enter ⟨100, 0, 100, 0⟩))
    (m' := Memory.set Memory.empty ⟨0⟩ (Account.enter ⟨100, 0, 100, 0⟩))
    (actions := []) (lineage := ⟨0⟩) rfl rfl
  · simp [Bound.Sound, allocated]
  · simp [Bound.Sound, allocated, freed, resetBytes]
  · simp [Bound.Sound, Memory.set, Account.enter]
  · simp [Bound.SoundOptional]

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING).

Planted empty-report mutation: established footprint evidence is impossible with no footprint. -/
example : ¬ LineageReport.Sound ⟨0⟩ (Account.enter ⟨100, 0, 100, 0⟩)
    (Account.enter ⟨100, 0, 100, 0⟩) none
    ⟨⟨0⟩, ⟨0, .boundEstablished⟩, ⟨0, .boundEstablished⟩,
      ⟨0, .boundEstablished⟩, ⟨0, .boundEstablished⟩⟩ := by
  simp [LineageReport.Sound, Bound.SoundOptional]

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING).

Planted underbounds: each established field independently rejects an understated limit. -/
example : ¬ LineageReport.Sound ⟨0⟩ reportEntry reportExit (some 10)
    (establishedReportWith 7 5 10 12) := by
  simp [LineageReport.Sound, Bound.Sound, establishedReportWith, reportEntry, reportExit]

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : ¬ LineageReport.Sound ⟨0⟩ reportEntry reportExit (some 10)
    (establishedReportWith 8 4 10 12) := by
  simp [LineageReport.Sound, Bound.Sound, establishedReportWith, reportEntry, reportExit]

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : ¬ LineageReport.Sound ⟨0⟩ reportEntry reportExit (some 10)
    (establishedReportWith 8 5 9 12) := by
  simp [LineageReport.Sound, Bound.Sound, establishedReportWith, reportEntry, reportExit]

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : ¬ LineageReport.Sound ⟨0⟩ reportEntry reportExit (some 10)
    (establishedReportWith 8 5 10 9) := by
  simp [LineageReport.Sound, Bound.Sound, Bound.SoundOptional, establishedReportWith,
    reportEntry, reportExit]

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING).

Established footprint evidence cannot certify an absent allocator-defined footprint. -/
example : ¬ LineageReport.Sound ⟨0⟩ reportEntry reportExit none establishedReport := by
  simp [LineageReport.Sound, Bound.SoundOptional, establishedReport]

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING).

Relabelling cannot certify another lineage's accounts, even with ample numeric limits.
The second lineage allocates four bytes while the first allocates fourteen. -/
example : ¬ LineageReport.Sound ⟨0⟩ (memoryA ⟨0⟩)
    ⟨114, 0, 13, 13⟩ (some 10) { establishedReport with lineage := ⟨1⟩ } := by
  simp [LineageReport.Sound, establishedReport]

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : ¬ LineageReport.Sound ⟨1⟩ (memoryA ⟨1⟩)
    ⟨11, 0, 6, 6⟩ (some 10)
    ⟨⟨1⟩, ⟨0, .boundEstablished⟩, ⟨0, .notEstablished⟩,
      ⟨0, .notEstablished⟩, ⟨0, .notEstablished⟩⟩ := by
  simp [LineageReport.Sound, Bound.Sound, memoryA, Memory.set, accountB]

/-! ## Closed law instances and premise-preserving planted subjects -/

namespace Audit

/-- A mutant allocator invents one live byte even for a zero request. -/
def allocInventsByte (a : Account) (bytes : Nat) : Account :=
  { a.alloc bytes with live := a.live + bytes + 1 }

/-- Instance (law `Account.alloc_zero`, case `accountA`). -/
example : accountA.alloc 0 = accountA :=
  Account.alloc_zero accountA (by simp [Account.WF, accountA])

/-- Planted (law `Account.alloc_zero`, case `accountA`, subject `Account.alloc`, mutant `allocInventsByte`). -/
example : accountA.WF ∧ ¬ (allocInventsByte accountA 0 = accountA) := by
  constructor
  · simp [Account.WF, accountA]
  · decide

/-- A mutant transition corrupts an unrelated lineage on a zero-byte allocation. -/
def applyCorruptsOther (m : Memory) (_ : Action) : Option Memory :=
  some (Memory.set m ⟨1⟩ ((m ⟨1⟩).alloc 1))

/-- Instance (law `apply_alloc_zero`, case `memoryA`). -/
example : apply memoryA (.alloc ⟨0⟩ 0) = some memoryA :=
  @apply_alloc_zero memoryA memoryA_wf ⟨0⟩

/-- Planted (law `apply_alloc_zero`, case `memoryA`, subject `apply`, mutant `applyCorruptsOther`). -/
example : (∀ l, (memoryA l).WF) ∧ ¬ (applyCorruptsOther memoryA (.alloc ⟨0⟩ 0) = some memoryA) := by
  refine ⟨memoryA_wf, ?_⟩
  intro h
  have h₁ := congrArg (fun result => result.map (fun m => m ⟨1⟩)) h
  change some (accountB.alloc 1) = some accountB at h₁
  exact (by decide : some (accountB.alloc 1) ≠ some accountB) h₁

/-- One defective executor: its reported result corrupts history, live bytes and both maxima.
The ordinary successful action trace remains unchanged. -/
def brokenMemory : Memory := Memory.set memoryA ⟨0⟩ ⟨999, 99, 0, 0⟩
def brokenRun (_ : Memory) (_ : List Action) : Option Memory := some brokenMemory

/-- Instance (law `sequence_wf`, case `conservationActions`). -/
example : PreservesWF run memoryA conservationActions :=
  sequence_wf memoryA conservationActions

/-- Planted (law `sequence_wf`, case `conservationActions`, subject `run`, mutant `brokenRun`). -/
example : ¬ PreservesWF brokenRun memoryA conservationActions := by
  intro h
  have hwf := h memoryA_wf brokenMemory rfl ⟨0⟩
  change 99 ≤ 0 ∧ 99 ≤ 0 at hwf
  omega

/-- Instance (law `sequence_conservation`, case `conservationActions`). -/
example : Conserves run memoryA conservationActions ⟨0⟩ :=
  sequence_conservation memoryA conservationActions ⟨0⟩

/-- Planted (law `sequence_conservation`, case `conservationActions`, subject `run`, mutant `brokenRun`). -/
example : ¬ Conserves brokenRun memoryA conservationActions ⟨0⟩ := by
  intro h
  have hc := h brokenMemory rfl
  have hr : (999 : Nat) = 108 := hc.1
  omega

/-- Instance (law `sequence_peaks`, case `conservationActions`). -/
example : RetainsPeaks run memoryA conservationActions ⟨0⟩ :=
  sequence_peaks memoryA conservationActions ⟨0⟩

/-- Planted (law `sequence_peaks`, case `conservationActions`, subject `run`, mutant `brokenRun`). -/
example : ¬ RetainsPeaks brokenRun memoryA conservationActions ⟨0⟩ := by
  intro h
  have hp : (12 : Nat) ≤ 0 := (h brokenMemory rfl).1
  omega

/-- Instance (law `sequence_peak_exact`, case `conservationActions`). -/
example : TracksPeak run memoryA conservationActions ⟨0⟩ :=
  sequence_peak_exact memoryA conservationActions ⟨0⟩

/-- Planted (law `sequence_peak_exact`, case `conservationActions`, subject `run`, mutant `brokenRun`). -/
example : ¬ TracksPeak brokenRun memoryA conservationActions ⟨0⟩ := by
  intro h
  have hp : (0 : Nat) = 13 := h memoryA_wf brokenMemory rfl
  omega

/-- Helper (computation). Concrete fixture or counterexample calculation, not independent contract evidence. (approved: PENDING).
The report entry is initialized at function entry; this calculation is shared by controls. -/
theorem reportEntry_wf : ∀ l, (reportEntryMemory l).WF := by
  intro l
  by_cases hl : l = ⟨0⟩
  · subst l; simp [reportEntryMemory, Memory.set, reportEntry, Account.WF]
  · simp [reportEntryMemory, Memory.set, hl, Memory.empty, Account.empty, Account.WF]

/-- Instance (law `report_certified`, case `reportActions`). -/
example : CertifiesReport run reportEntryMemory reportActions demoAllocator ⟨0⟩ 8 5 10 12 :=
  report_certified reportEntryMemory reportActions demoAllocator ⟨0⟩ 8 5 10 12

/-- Planted (law `report_certified`, case `reportActions`, subject `run`, mutant `brokenRun`). -/
example : ¬ CertifiesReport brokenRun reportEntryMemory reportActions demoAllocator ⟨0⟩ 8 5 10 12 := by
  intro h
  have hr := h reportEntry_wf rfl (by decide) (by decide) (by decide)
    ⟨10, rfl, by decide⟩ brokenMemory rfl
  have hreq : (899 : Nat) ≤ 8 := hr.2.1 rfl
  omega

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING).
The initial maximum includes already-live bytes; a freed suffix cannot erase it. -/
example : observedPeak ⟨0⟩ reportEntryMemory reportActions = 10 ∧
    observedPeak ⟨0⟩ reportEntryMemory [] = 2 := by decide

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING).
Bad reset results must fail the exact WF and conservation obligations, rather than merely
differing from the implementation's chosen record. -/
example : ¬ ({ accountA.reset with live := 13 } : Account).WF := by
  simp [Account.WF, accountA, Account.reset]
/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : ¬ ResetConservation accountA badResetPeaks 5 := by
  simp [ResetConservation, accountA, badResetPeaks, Account.reset]

/-- A setter with the same signature corrupts the value even when writing the original value. -/
def corruptSetter (m : Memory) (lineage : LineageId) (a : Account) : Memory :=
  Memory.set m lineage { a with live := a.live + 1 }

/-- Representation lemma. Same-value control for the setter helper (approved: PENDING). -/
example : corruptSetter memoryA ⟨0⟩ (memoryA ⟨0⟩) ≠ memoryA := by
  intro h
  have hc := congrArg (fun m => (m ⟨0⟩).live) h
  change (6 : Nat) = 5 at hc
  omega

open Demo

/-- This mutant releases zero and exposes the unreleased local as its shared secret. -/
def exposingStart (_ : Int) (hidden : Nat) : Config Demo.ops := leakyStart (Int.ofNat hidden)

/-- Instance (law `secure_ni`, case `hiddenLocals`). -/
example : DelimitedReleaseNI instrumentation program labelling (start 7 1) (start 7 2) :=
  secure_ni 7 7 1 2

/-- Planted (law `secure_ni`, case `hiddenLocals`, subject `start`, mutant `exposingStart`). -/
example : ¬ DelimitedReleaseNI instrumentation program labelling (exposingStart 7 1) (exposingStart 7 2) := by
  intro h
  have ho := h (leakyRun 1) (leakyRun 2) (leaky_low 1 2) rfl rfl
  exact authorized_release_not_ni.2.2.2.2 ho

/-- Instance (law `secure_ct`, case `hiddenLocals`). -/
example : ConstantTime instrumentation program labelling (start 7 1) (start 7 2) :=
  secure_ct 7 7 1 2

/-- Planted (law `secure_ct`, case `hiddenLocals`, subject `start`, mutant `exposingStart`). -/
example : ¬ ConstantTime instrumentation program labelling (exposingStart 7 1) (exposingStart 7 2) := by
  intro h
  have hl := h (leakyRun 1) (leakyRun 2) (leaky_low 1 2) rfl rfl
  exact authorized_release_not_ct.2.2.2.2 hl

/-- Instance (law `timing_ni`, case `timingStart`). -/
example : DelimitedReleaseNI instrumentation program labelling (timingStart 1) (timingStart 2) :=
  timing_ni 1 2

/-- Planted (law `timing_ni`, case `timingStart`, subject `timingStart`, mutant `leakyStart`). -/
example : ¬ DelimitedReleaseNI instrumentation program labelling (leakyStart 1) (leakyStart 2) := by
  intro h
  exact authorized_release_not_ni.2.2.2.2
    (h (leakyRun 1) (leakyRun 2) (leaky_low 1 2) rfl rfl)

/-- A unary secure start has the same type as the timing-start subject. -/
def secureUnary (secret : Int) : Config Demo.ops := start secret 0

/-- Instance (law `timing_not_ct`, case `timingStart`). -/
example : ¬ ConstantTime instrumentation program labelling (timingStart 1) (timingStart 2) :=
  timing_not_ct

/-- Planted (law `timing_not_ct`, case `timingStart`, subject `timingStart`, mutant `secureUnary`). -/
example : ¬ (¬ ConstantTime instrumentation program labelling (secureUnary 1) (secureUnary 2)) := by
  intro h
  exact h (secure_ct 1 2 0 0)

end Audit

/-! ## Wipe-preservation witnesses -/

namespace WipeDemo

inductive Statement where
  | pure (value : Nat)
  | output (value : Nat)
  | wipe (event : WipeEvent)
  deriving DecidableEq, Repr

abbrev Program := List Statement

/-- Erase only pure arithmetic; output and wipe effects remain in their original order. -/
def erasePure : Program → Program
  | [] => []
  | .pure _ :: rest => erasePure rest
  | .output v :: rest => .output v :: erasePure rest
  | .wipe w :: rest => .wipe w :: erasePure rest

def wipes : Program → List WipeEvent
  | [] => []
  | .pure _ :: rest => wipes rest
  | .output _ :: rest => wipes rest
  | .wipe w :: rest => w :: wipes rest

def observations : Program → List Nat
  | [] => []
  | .pure _ :: rest => observations rest
  | .output v :: rest => v :: observations rest
  | .wipe _ :: rest => observations rest

/-- The actual optimization relation: the target is exactly pure-instruction erasure of the source. -/
def optimized (source target : Program) : Prop := target = erasePure source

/-- A relation that certifies only ordinary observation preservation. -/
def observationRelated (source target : Program) : Prop :=
  observations source = observations target

/-- Law (subject `optimized`).

The erasure pass preserves every ordered wipe effect. -/
theorem erasePure_preserves_wipes : WipePreservation optimized wipes wipes := by
  intro source target h
  change target = erasePure source at h
  subst target
  induction source with
  | nil => rfl
  | cons statement rest ih =>
    cases statement <;> simp [erasePure, wipes, ih]

/-- Instance (law `erasePure_preserves_wipes`, case `optimized`). -/
example : WipePreservation optimized wipes wipes := erasePure_preserves_wipes

def firstWipe : WipeEvent := ⟨⟨0⟩, ⟨0⟩, 4⟩
def secondWipe : WipeEvent := ⟨⟨1⟩, ⟨0⟩, 2⟩

def source : Program :=
  [.pure 99, .wipe firstWipe, .output 7, .pure 100, .wipe secondWipe]

def target : Program := erasePure source

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING).

Non-trivial related pair: two nonzero wipes survive pure-instruction erasure in order. -/
example : optimized source target ∧ wipes source = [firstWipe, secondWipe] ∧
    wipes target = [firstWipe, secondWipe] := by
  exact ⟨rfl, rfl, rfl⟩

/-- Planted bad targets retain the source's ordinary output but mishandle its wipes. -/
def droppedTarget : Program := [.pure 99, .wipe secondWipe, .output 7, .pure 100]
def changedTarget : Program :=
  [.pure 99, .wipe ⟨⟨0⟩, ⟨0⟩, 5⟩, .output 7, .pure 100, .wipe secondWipe]
def reorderedTarget : Program :=
  [.pure 99, .wipe secondWipe, .output 7, .pure 100, .wipe firstWipe]

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : observationRelated source droppedTarget ∧ wipes source ≠ wipes droppedTarget :=
  ⟨rfl, by decide⟩

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : observationRelated source changedTarget ∧ wipes source ≠ wipes changedTarget :=
  ⟨rfl, by decide⟩

/-- Representation lemma. Concrete control calculation, not independent soundness evidence (approved: PENDING). -/
example : observationRelated source reorderedTarget ∧ wipes source ≠ wipes reorderedTarget :=
  ⟨rfl, by decide⟩

/-- Planted (law `erasePure_preserves_wipes`, case `optimized`, subject `optimized`, mutant `observationRelated`). -/
example : ¬ WipePreservation observationRelated wipes wipes := by
  intro h
  exact (by decide : wipes source ≠ wipes droppedTarget) (h source droppedTarget rfl)

end WipeDemo

end ConcreteSpec.Core.Leak
