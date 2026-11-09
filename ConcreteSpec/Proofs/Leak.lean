import ConcreteSpec.Spec.Leak
import ConcreteSpec.Proofs.Exec

/-!
# The laws of leakage, release and accounting (`Spec/Leak.lean`, foundation contract C7)

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). See `ConcreteSpec/README.md`.

The execution examples use a small C1 operand language. `secure_ni` and `secure_ct` declassify a
masked combination of two secret components (`secret + hidden`) and output only that released sum,
so distinct secret states (`(6, 1)` vs `(5, 2)`) share the same release record (`7`) and require
the release-equality premise. `timing_ni` and `timing_not_ct` separate the claims with public
output but secret-dependent variable-time work. The leaky start configuration releases authorized
public zero but outputs its secret: `authorized_release_not_ni` and `authorized_release_not_ct`
refute deriving either claim from policy authorization alone.

The accounting theorems prove that every successful abstract C4 action sequence conserves requested
and live bytes, preserves well-formedness, never lowers either historical or function-local peaks,
and tracks the local peak over the live-byte trajectory. Reset outflow is read from actual
intermediate states. Zero-byte allocation is the identity on a well-formed abstract memory.
`report_certified` connects independent trajectory and flow bounds to arbitrary lineage reports,
including partial (.notEstablished) reports and allocators with no defined footprint.
-/

namespace ConcreteSpec.Core.Leak
open ConcreteSpec.Core
open ConcreteSpec.Core.Exec

/-- Helper (projection). Forgets C7 instrumentation traces from an `InstrumentedSteps` derivation to recover the underlying C1 `Steps` derivation (approved: APPR-C7-agy-5d94c4b4d).

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

/-- Law (subject `ReleasePolicy.Authorized`).

A policy that forbids release rejects every nonempty release trace. -/
theorem forbidsRelease_not_authorized {Ty Val : Type} (policy : ReleasePolicy Ty)
    (r : Release Ty Val) (rs : List (Release Ty Val))
    (hforbids : policy.ForbidsRelease) :
    ¬ policy.Authorized (r :: rs) := by
  intro hauth
  have hr := hauth r (by simp)
  simp [ReleasePolicy.Permits, ReleasePolicy.ForbidsRelease] at hforbids hr
  rw [hforbids] at hr
  contradiction

namespace Demo

/-- Values in the example are integers. `.secret` reads the shared secret store; `.masked` adds the
shared secret store to the decoded magnitude of the negative secret local `"hidden"`. -/
inductive Expr where
  | lit (n : Int)
  | secret
  | masked

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

/-- The local variable holding the second, unreleased secret component. -/
def hiddenVar : Var := ⟨"hidden", 0⟩

/-- Encode `hidden : Nat` as `Int.negSucc hidden` (`-(Int.ofNat hidden) - 1`), which is always
strictly negative and therefore always classified as secret by `labelling`. -/
def hiddenLocals (hidden : Nat) : AList Var Int :=
  [(hiddenVar, Int.negSucc hidden)]

/-- Decode the non-negative offset `hidden` from a negative secret local encoding. -/
def decodeHidden : Int → Int
  | .negSucc n => Int.ofNat n
  | .ofNat _ => 0

/-- Read the decoded secret local offset from a local environment. -/
def hiddenOffset (locals : AList Var Int) : Int :=
  match locals.lookup hiddenVar with
  | some v => decodeHidden v
  | none => 0

/-- Evaluate a public literal, the raw shared secret, or the masked sum `st + hidden`. -/
def eval (locals : AList Var Int) (st : Int) : Expr → Res (Int × Int)
  | .lit n => .ok (n, st)
  | .secret => .ok (st, st)
  | .masked => .ok (st + hiddenOffset locals, st)

/-- Build the example C1 operand instance for a given primitive answer oracle. -/
abbrev opsWith (oracle : Prim → List Int → Int → Answer Int Int Int → Bool) : Operands where
  Val := Int
  Expr := Expr
  Ty := Unit
  Store := Int
  Prim := Prim
  Event := Int
  unitVal := 0
  unitTy := ()
  isCopy := fun _ => true
  eval := eval
  life := fun _ st => some st
  test := fun v => some (v ≠ 0)
  allows := oracle

/-- The example C1 operand instance with deterministic primitive answers `allowed`. -/
abbrev ops : Operands := opsWith allowed

/-- Release `released`, then evaluate `result` as the output. -/
def body (released result : Expr) : Stmt ops :=
  .seq (.prim none .release [released] ⟨0⟩) (.prim none .output [result] ⟨1⟩)

/-- The shared empty function table; the examples differ in their initial configurations. -/
def program : Program ops := ⟨[]⟩

/-- Declassify and output the masked sum `secret + hidden` while keeping `secret` and `hidden`
individually secret. Distinct secret pairs with the same sum (such as `(6, 1)` and `(5, 2)`) produce
identical release traces and equal outputs. -/
def start (secret : Int) (hidden : Nat := 0) : Config ops :=
  ⟨.exec (body .masked .masked), hiddenLocals hidden, secret, []⟩

/-- Mutant start configuration that declassifies the masked sum `secret + hidden` (preserving equal
release traces on `(6, 1)` and `(5, 2)`) but outputs the unmasked secret `secret`. -/
def exposingStart (secret : Int) (hidden : Nat := 0) : Config ops :=
  ⟨.exec (body .masked .secret), hiddenLocals hidden, secret, []⟩

/-- The leaky example releases public zero (authorized by `policy`) but outputs its secret. -/
def leakyStart (secret : Int) : Config ops := ⟨.exec (body (.lit 0) .secret), [], secret, []⟩

/-- Public output with secret-dependent variable-time work and no secret-dependent release. -/
def timingBody : Stmt ops := .seq (.prim none .work [.secret] ⟨2⟩) (body (.lit 0) (.lit 7))
def timingStart (secret : Int) : Config ops := ⟨.exec timingBody, [], secret, []⟩

/-- Constant-time start configuration that releases public zero and outputs public seven without
secret-dependent work, inhabiting all `DelimitedReleaseNI`, `ConstantTime`, and
`ReleasePolicy.RunsAuthorized` hypotheses across distinct secrets. -/
def ctStart (secret : Int) : Config ops := ⟨.exec (body (.lit 0) (.lit 7)), [], secret, []⟩

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

/-- The configurations specific to sequencing and answering the timing example's work. -/
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

/-- Helper (computation). Discharges primitive-answer determinism for the three-operation `Demo.allowed` oracle by case analysis on the request (approved: APPR-C7-agy-5d94c4b4d). -/
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

/-- Helper (inversion). Determinism of a single C1 step under `Demo.ops`, used to unroll complete runs in the trace inversion helpers and `step_unique` (approved: APPR-C7-agy-5d94c4b4d). -/
theorem step_det {P : Program ops} {c c₁ c₂ : Config ops} {l₁ l₂ : Label ops}
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

/-- Law (subject `allowed`).

Fixed external choices make primitive transitions of the example machine deterministic in both
their emitted label and their target configuration. -/
theorem step_unique (p : Prim) (arg : Int) (st : Int) (site : SiteId)
    (hex : ∃ (l : Label (opsWith allowed)) (c' : Config (opsWith allowed)),
      Step (⟨[]⟩ : Program (opsWith allowed))
        ⟨.exec (.prim none p [.lit arg] site), [], st, []⟩ l c') :
    ∃ (l : Label (opsWith allowed)) (c' : Config (opsWith allowed)),
      Step (⟨[]⟩ : Program (opsWith allowed))
        ⟨.exec (.prim none p [.lit arg] site), [], st, []⟩ l c' ∧
      ∀ (l₂ : Label (opsWith allowed)) (c₂ : Config (opsWith allowed)),
        Step (⟨[]⟩ : Program (opsWith allowed))
          ⟨.exec (.prim none p [.lit arg] site), [], st, []⟩ l₂ c₂ →
        l = l₂ ∧ c' = c₂ := by
  obtain ⟨l, c', hstep⟩ := hex
  exact ⟨l, c', hstep, fun _ _ h₂ => step_det hstep h₂⟩

/-- Helper (inversion). Unrolls the four-step C1 derivation from `Demo.start` to compute its exact observation, leakage, release, choice, and final-state traces (approved: APPR-C7-agy-5d94c4b4d).

Every complete run of the secure four-step example releases and outputs secret + Int.ofNat hidden. -/
theorem secure_run_unique (secret : Int) (hidden : Nat := 0)
    (r : Run instrumentation program (start secret hidden)) :
    r.observations = [.output (secret + Int.ofNat hidden)] ∧
      r.leakage = [.variableTime ⟨0⟩ [secret + Int.ofNat hidden],
        .variableTime ⟨1⟩ [secret + Int.ofNat hidden]] ∧
      r.releases = [releaseValue (secret + Int.ofNat hidden)] ∧
      r.choices = [(), (), (), ()] ∧
      r.final = ⟨.done, hiddenLocals hidden, secret, []⟩ := by
  obtain ⟨final, ending, hf, observations, leakage, releases, choices, hs⟩ := r
  cases hs with
  | refl => simp [start, Config.final?] at hf
  | step h₁ rest₁ =>
    obtain ⟨rfl, rfl⟩ := step_det h₁ (.silent (P := program) rfl)
    cases rest₁ with
    | refl => dsimp [afterSeq, afterRelease, beforeOutput] at hf <;> simp [Config.final?] at hf
    | step h₂ rest₂ =>
      obtain ⟨rfl, rfl⟩ := step_det h₂
        (Step.prim (O := ops) (P := program) (p := .release)
          (vs := [secret + Int.ofNat hidden]) (st := secret)
          (a := (⟨none, .ok 0, secret⟩ : Answer Int Int Int)) rfl (by simp [allowed]) rfl)
      cases rest₂ with
      | refl => dsimp [afterSeq, afterRelease, beforeOutput] at hf <;> simp [Config.final?] at hf
      | step h₃ rest₃ =>
        obtain ⟨rfl, rfl⟩ := step_det h₃ (.silent (P := program) rfl)
        cases rest₃ with
        | refl => dsimp [afterSeq, afterRelease, beforeOutput] at hf <;> simp [Config.final?] at hf
        | step h₄ rest₄ =>
          obtain ⟨rfl, rfl⟩ := step_det h₄
            (Step.prim (O := ops) (P := program) (p := .output)
              (vs := [secret + Int.ofNat hidden]) (st := secret)
              (a := (⟨some (secret + Int.ofNat hidden), .ok 0, secret⟩ : Answer Int Int Int))
              rfl (by simp [allowed]) rfl)
          cases rest₄ with
          | refl => exact ⟨rfl, rfl, rfl, rfl, rfl⟩
          | step hs _ => exact (no_step_of_none (P := program)
            (c := (⟨.done, hiddenLocals hidden, secret, []⟩ : Config ops)) rfl _ _ hs).elim

/-- One complete secure execution: declassify `secret + hidden`, output that sum, and finish. -/
def secureRun (secret : Int) (hidden : Nat := 0) : Run instrumentation program (start secret hidden) where
  final := finished secret (hiddenLocals hidden)
  ending := .normal
  finalEnding := rfl
  observations := [.output (secret + Int.ofNat hidden)]
  leakage := [.variableTime ⟨0⟩ [secret + Int.ofNat hidden],
    .variableTime ⟨1⟩ [secret + Int.ofNat hidden]]
  releases := [releaseValue (secret + Int.ofNat hidden)]
  choices := [(), (), (), ()]
  steps :=
    InstrumentedSteps.step (I := instrumentation)
      (Step.silent (P := program) (c := start secret hidden)
        (c' := afterSeq .masked .masked secret (hiddenLocals hidden)) rfl) <|
    InstrumentedSteps.step (I := instrumentation)
      (Step.prim (O := ops) (P := program)
        (c := afterSeq .masked .masked secret (hiddenLocals hidden))
        (c' := afterRelease .masked secret (hiddenLocals hidden))
        (p := .release) (vs := [secret + Int.ofNat hidden]) (st := secret)
        (a := (⟨none, .ok 0, secret⟩ : Answer Int Int Int)) rfl (by simp [allowed]) rfl) <|
    InstrumentedSteps.step (I := instrumentation)
      (Step.silent (P := program) (c := afterRelease .masked secret (hiddenLocals hidden))
        (c' := beforeOutput .masked secret (hiddenLocals hidden)) rfl) <|
    InstrumentedSteps.step (I := instrumentation)
      (Step.prim (O := ops) (P := program)
        (c := beforeOutput .masked secret (hiddenLocals hidden))
        (c' := finished secret (hiddenLocals hidden))
        (p := .output) (vs := [secret + Int.ofNat hidden]) (st := secret)
        (a := (⟨some (secret + Int.ofNat hidden), .ok 0, secret⟩ : Answer Int Int Int))
        rfl (by simp [allowed]) rfl) <|
    InstrumentedSteps.refl (I := instrumentation) (P := program) (finished secret (hiddenLocals hidden))

/-- One complete execution of the mutant `exposingStart`: declassifies `secret + hidden` (matching
`secureRun`'s release trace) but outputs the unmasked secret `secret`. -/
def exposingRun (secret : Int) (hidden : Nat := 0) : Run instrumentation program (exposingStart secret hidden) where
  final := finished secret (hiddenLocals hidden)
  ending := .normal
  finalEnding := rfl
  observations := [.output secret]
  leakage := [.variableTime ⟨0⟩ [secret + Int.ofNat hidden], .variableTime ⟨1⟩ [secret]]
  releases := [releaseValue (secret + Int.ofNat hidden)]
  choices := [(), (), (), ()]
  steps :=
    InstrumentedSteps.step (I := instrumentation)
      (Step.silent (P := program) (c := exposingStart secret hidden)
        (c' := afterSeq .masked .secret secret (hiddenLocals hidden)) rfl) <|
    InstrumentedSteps.step (I := instrumentation)
      (Step.prim (O := ops) (P := program)
        (c := afterSeq .masked .secret secret (hiddenLocals hidden))
        (c' := afterRelease .secret secret (hiddenLocals hidden))
        (p := .release) (vs := [secret + Int.ofNat hidden]) (st := secret)
        (a := (⟨none, .ok 0, secret⟩ : Answer Int Int Int)) rfl (by simp [allowed]) rfl) <|
    InstrumentedSteps.step (I := instrumentation)
      (Step.silent (P := program) (c := afterRelease .secret secret (hiddenLocals hidden))
        (c' := beforeOutput .secret secret (hiddenLocals hidden)) rfl) <|
    InstrumentedSteps.step (I := instrumentation)
      (Step.prim (O := ops) (P := program)
        (c := beforeOutput .secret secret (hiddenLocals hidden))
        (c' := finished secret (hiddenLocals hidden))
        (p := .output) (vs := [secret]) (st := secret)
        (a := (⟨some secret, .ok 0, secret⟩ : Answer Int Int Int))
        rfl (by simp [allowed]) rfl) <|
    InstrumentedSteps.refl (I := instrumentation) (P := program) (finished secret (hiddenLocals hidden))

/-- Law (subject `start`).

**Delimited-release NI, proved for the secure example.** Equality of the ordered release records
equates s₁ + Int.ofNat hidden₁ and s₂ + Int.ofNat hidden₂ across distinct secret pairs,
yielding equal public observations for every pair of complete runs. -/
theorem secure_ni (s₁ s₂ : Int) (hidden₁ hidden₂ : Nat)
    (_hlow : LowEquivalent labelling (start s₁ hidden₁) (start s₂ hidden₂))
    (hrel : ∃ (r₁ : Run instrumentation program (start s₁ hidden₁))
              (r₂ : Run instrumentation program (start s₂ hidden₂)),
              r₁.choices = r₂.choices ∧ r₁.releases = r₂.releases) :
    DelimitedReleaseNI instrumentation program labelling (start s₁ hidden₁) (start s₂ hidden₂) := by
  obtain ⟨r₁₀, r₂₀, _, hreleases⟩ := hrel
  have h₁₀ := secure_run_unique s₁ hidden₁ r₁₀
  have h₂₀ := secure_run_unique s₂ hidden₂ r₂₀
  have hsum : s₁ + Int.ofNat hidden₁ = s₂ + Int.ofNat hidden₂ := by
    rw [h₁₀.2.2.1, h₂₀.2.2.1] at hreleases
    simpa [releaseValue] using hreleases
  intro r₁ r₂ _ _ _
  have h₁ := secure_run_unique s₁ hidden₁ r₁
  have h₂ := secure_run_unique s₂ hidden₂ r₂
  simp only [Run.publicObservations, h₁.1, h₂.1, h₁.2.2.2.2, h₂.2.2.2.2, hsum, Config.final?]

/-- Law (subject `start`).

**Constant time, proved separately for the secure example.** Equality of the ordered release
records equates the masked sums across distinct secret pairs, making the leakage traces equal. -/
theorem secure_ct (s₁ s₂ : Int) (hidden₁ hidden₂ : Nat)
    (_hlow : LowEquivalent labelling (start s₁ hidden₁) (start s₂ hidden₂))
    (hrel : ∃ (r₁ : Run instrumentation program (start s₁ hidden₁))
              (r₂ : Run instrumentation program (start s₂ hidden₂)),
              r₁.choices = r₂.choices ∧ r₁.releases = r₂.releases) :
    ConstantTime instrumentation program labelling (start s₁ hidden₁) (start s₂ hidden₂) := by
  obtain ⟨r₁₀, r₂₀, _, hreleases⟩ := hrel
  have h₁₀ := secure_run_unique s₁ hidden₁ r₁₀
  have h₂₀ := secure_run_unique s₂ hidden₂ r₂₀
  have hsum : s₁ + Int.ofNat hidden₁ = s₂ + Int.ofNat hidden₂ := by
    rw [h₁₀.2.2.1, h₂₀.2.2.1] at hreleases
    simpa [releaseValue] using hreleases
  intro r₁ r₂ _ _ _
  have h₁ := secure_run_unique s₁ hidden₁ r₁
  have h₂ := secure_run_unique s₂ hidden₂ r₂
  rw [h₁.2.1, h₂.2.1, hsum]

/-- One complete leaky execution: release public zero (authorized by `policy`) but output the secret. -/
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

/-- Helper (inversion). Unrolls the four-step C1 derivation from `Demo.leakyStart` to show every complete run releases only `releaseZero` (approved: APPR-C7-agy-5d94c4b4d). -/
theorem leaky_run_releases (secret : Int)
    (r : Run instrumentation program (leakyStart secret)) :
    r.releases = [releaseZero] := by
  obtain ⟨final, ending, hf, observations, leakage, releases, choices, hs⟩ := r
  cases hs with
  | refl => simp [leakyStart, Config.final?] at hf
  | step h₁ rest₁ =>
    obtain ⟨rfl, rfl⟩ := step_det h₁ (.silent (P := program) rfl)
    cases rest₁ with
    | refl => dsimp [afterSeq, afterRelease, beforeOutput] at hf <;> simp [Config.final?] at hf
    | step h₂ rest₂ =>
      obtain ⟨rfl, rfl⟩ := step_det h₂
        (Step.prim (O := ops) (P := program) (p := .release) (vs := [0]) (st := secret)
          (a := (⟨none, .ok 0, secret⟩ : Answer Int Int Int)) rfl (by simp [allowed]) rfl)
      cases rest₂ with
      | refl => dsimp [afterSeq, afterRelease, beforeOutput] at hf <;> simp [Config.final?] at hf
      | step h₃ rest₃ =>
        obtain ⟨rfl, rfl⟩ := step_det h₃ (.silent (P := program) rfl)
        cases rest₃ with
        | refl => dsimp [afterSeq, afterRelease, beforeOutput] at hf <;> simp [Config.final?] at hf
        | step h₄ rest₄ =>
          obtain ⟨rfl, rfl⟩ := step_det h₄
            (Step.prim (O := ops) (P := program) (p := .output) (vs := [secret]) (st := secret)
              (a := (⟨some secret, .ok 0, secret⟩ : Answer Int Int Int)) rfl (by simp [allowed]) rfl)
          cases rest₄ with
          | refl => rfl
          | step hs _ => exact (no_step_of_none (P := program)
            (c := (⟨.done, [], secret, []⟩ : Config ops)) rfl _ _ hs).elim

/-- One complete constant-time execution: release public zero and output public seven without
secret-dependent work. -/
def ctRun (secret : Int) : Run instrumentation program (ctStart secret) where
  final := finished secret
  ending := .normal
  finalEnding := rfl
  observations := [.output 7]
  leakage := [.variableTime ⟨0⟩ [0], .variableTime ⟨1⟩ [7]]
  releases := [releaseZero]
  choices := [(), (), (), ()]
  steps :=
    InstrumentedSteps.step (I := instrumentation)
      (Step.silent (P := program) (c := ctStart secret)
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

/-- Helper (inversion). Unrolls the four-step C1 derivation from `Demo.ctStart` to compute its constant-time observation, leakage, release, choice, and final-state traces (approved: APPR-C7-agy-5d94c4b4d). -/
theorem ct_run_unique (secret : Int)
    (r : Run instrumentation program (ctStart secret)) :
    r.observations = [.output 7] ∧
      r.leakage = [.variableTime ⟨0⟩ [0], .variableTime ⟨1⟩ [7]] ∧
      r.releases = [releaseZero] ∧ r.choices = [(), (), (), ()] ∧
      r.final = ⟨.done, [], secret, []⟩ := by
  obtain ⟨final, ending, hf, observations, leakage, releases, choices, hs⟩ := r
  cases hs with
  | refl => simp [ctStart, Config.final?] at hf
  | step h₁ rest₁ =>
    obtain ⟨rfl, rfl⟩ := step_det h₁ (.silent (P := program) rfl)
    cases rest₁ with
    | refl => dsimp [afterSeq, afterRelease, beforeOutput] at hf <;> simp [Config.final?] at hf
    | step h₂ rest₂ =>
      obtain ⟨rfl, rfl⟩ := step_det h₂
        (Step.prim (O := ops) (P := program) (p := .release) (vs := [0]) (st := secret)
          (a := (⟨none, .ok 0, secret⟩ : Answer Int Int Int)) rfl (by simp [allowed]) rfl)
      cases rest₂ with
      | refl => dsimp [afterSeq, afterRelease, beforeOutput] at hf <;> simp [Config.final?] at hf
      | step h₃ rest₃ =>
        obtain ⟨rfl, rfl⟩ := step_det h₃ (.silent (P := program) rfl)
        cases rest₃ with
        | refl => dsimp [afterSeq, afterRelease, beforeOutput] at hf <;> simp [Config.final?] at hf
        | step h₄ rest₄ =>
          obtain ⟨rfl, rfl⟩ := step_det h₄
            (Step.prim (O := ops) (P := program) (p := .output) (vs := [7]) (st := secret)
              (a := (⟨some 7, .ok 0, secret⟩ : Answer Int Int Int)) rfl (by simp [allowed]) rfl)
          cases rest₄ with
          | refl => exact ⟨rfl, rfl, rfl, rfl, rfl⟩
          | step hs _ => exact (no_step_of_none (P := program)
            (c := (⟨.done, [], secret, []⟩ : Config ops)) rfl _ _ hs).elim

/-- Representation lemma. Concrete projection check showing secureRun 1 maps via `InstrumentedSteps.toSteps` to an underlying C1 `Steps` derivation (approved: APPR-C7-agy-5d94c4b4d).

The concrete instrumented run projects to C1, not to a second semantics. -/
example : ∃ events, Steps program (start 1) events (finished 1 (hiddenLocals 0)) :=
  (secureRun 1).steps.toSteps

/-- Helper (inversion). Unrolls the seven-step C1 derivation from `Demo.timingStart` to compute its exact observation, leakage, release, choice, and final-state traces (approved: APPR-C7-agy-5d94c4b4d).

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
    obtain ⟨rfl, rfl⟩ := step_det h₁ (.silent (P := program) rfl)
    cases rest₁ with
    | refl => dsimp [timingAfterSeq, timingAfterWork, timingBeforeBody, afterSeq, afterRelease, beforeOutput] at hf <;> simp [Config.final?] at hf
    | step h₂ rest₂ =>
      obtain ⟨rfl, rfl⟩ := step_det h₂
        (Step.prim (O := ops) (P := program) (c := timingAfterSeq secret)
          (c' := timingAfterWork secret) (p := .work) (vs := [secret]) (st := secret)
          (a := (⟨none, .ok 0, secret⟩ : Answer Int Int Int)) rfl (by simp [allowed]) rfl)
      cases rest₂ with
      | refl => dsimp [timingAfterSeq, timingAfterWork, timingBeforeBody, afterSeq, afterRelease, beforeOutput] at hf <;> simp [Config.final?] at hf
      | step h₃ rest₃ =>
        obtain ⟨rfl, rfl⟩ := step_det h₃ (.silent (P := program)
          (c := timingAfterWork secret) (c' := timingBeforeBody secret) rfl)
        cases rest₃ with
        | refl => dsimp [timingAfterSeq, timingAfterWork, timingBeforeBody, afterSeq, afterRelease, beforeOutput] at hf <;> simp [Config.final?] at hf
        | step h₄ rest₄ =>
          obtain ⟨rfl, rfl⟩ := step_det h₄ (.silent (P := program)
            (c := timingBeforeBody secret) (c' := afterSeq (.lit 0) (.lit 7) secret) rfl)
          cases rest₄ with
          | refl => dsimp [timingAfterSeq, timingAfterWork, timingBeforeBody, afterSeq, afterRelease, beforeOutput] at hf <;> simp [Config.final?] at hf
          | step h₅ rest₅ =>
            obtain ⟨rfl, rfl⟩ := step_det h₅
              (Step.prim (O := ops) (P := program) (c := afterSeq (.lit 0) (.lit 7) secret)
                (c' := afterRelease (.lit 7) secret) (p := .release) (vs := [0]) (st := secret)
                (a := (⟨none, .ok 0, secret⟩ : Answer Int Int Int)) rfl (by simp [allowed]) rfl)
            cases rest₅ with
            | refl => dsimp [timingAfterSeq, timingAfterWork, timingBeforeBody, afterSeq, afterRelease, beforeOutput] at hf <;> simp [Config.final?] at hf
            | step h₆ rest₆ =>
              obtain ⟨rfl, rfl⟩ := step_det h₆ (.silent (P := program)
                (c := afterRelease (.lit 7) secret) (c' := beforeOutput (.lit 7) secret) rfl)
              cases rest₆ with
              | refl => dsimp [timingAfterSeq, timingAfterWork, timingBeforeBody, afterSeq, afterRelease, beforeOutput] at hf <;> simp [Config.final?] at hf
              | step h₇ rest₇ =>
                obtain ⟨rfl, rfl⟩ := step_det h₇
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

Every pair of timing runs from low-equivalent entries with matched choices and releases emits the
same public observations ([.output 7, .final .normal]). -/
theorem timing_ni (s₁ s₂ : Int)
    (_hlow : LowEquivalent labelling (timingStart s₁) (timingStart s₂))
    (_hrel : ∃ (r₁ : Run instrumentation program (timingStart s₁))
               (r₂ : Run instrumentation program (timingStart s₂)),
               r₁.choices = r₂.choices ∧ r₁.releases = r₂.releases) :
    DelimitedReleaseNI instrumentation program labelling (timingStart s₁) (timingStart s₂) := by
  intro r₁ r₂ _ _ _
  have h₁ := timing_run_unique s₁ r₁
  have h₂ := timing_run_unique s₂ r₂
  simp only [Run.publicObservations, h₁.1, h₂.1, h₁.2.2.2.2, h₂.2.2.2.2, Config.final?]

/-- Helper (inversion). Inverts the concrete complete runs to refute the corresponding
noninterference claim. The proof applies the distinct-secret premise, so it is excluded
from Law evidence by the projection convention (approved: PENDING).

The timing program is not constant-time for any two distinct secrets: secret-dependent work
produces different nonempty leakage traces. -/
theorem timing_not_ct (s₁ s₂ : Int)
    (hlow : LowEquivalent labelling (timingStart s₁) (timingStart s₂))
    (hs : s₁ ≠ s₂) :
    ¬ ConstantTime instrumentation program labelling (timingStart s₁) (timingStart s₂) := by
  intro hct
  have hl := hct (timingRun s₁) (timingRun s₂) hlow rfl rfl
  simp [timingRun] at hl
  exact hs hl

/-- A deliberately permissive output oracle admits two distinct labels for one machine request. -/
def permissiveAllowed (p : Prim) (vs : List Int) (st : Int) (a : Answer Int Int Int) : Bool :=
  match p, vs with
  | .output, [_] => true
  | _, _ => allowed p vs st a

/-- Representation lemma. Concrete order-sensitivity check showing secureRun 1's leakage list is not equal to its reversal (approved: APPR-C7-agy-5d94c4b4d).

Planted trace mutations: reversed leakage and a missing final observation are detectable. -/
example : (secureRun 1).leakage ≠ [.variableTime ⟨1⟩ [1], .variableTime ⟨0⟩ [1]] := by
  intro h
  simp [secureRun] at h

/-- Representation lemma. Concrete finality check showing `Run.publicObservations` includes .final .normal beyond the step output (approved: APPR-C7-agy-5d94c4b4d). -/
example : (secureRun 1).publicObservations ≠ [.output 1] := by decide

/-- Representation lemma. Concrete non-finality check showing the intermediate `afterRelease` state has final? = none (approved: APPR-C7-agy-5d94c4b4d).

A nonfinal execution prefix is not a complete run. -/
example : ¬ ∃ ending, (afterRelease .secret 1).final? = some ending := by
  simp [afterRelease, Config.final?]

/-- A configuration with one secret and one public local. -/
def mixedConfig (secret publicValue : Int) : Config ops :=
  ⟨.done, [(⟨"secret", 0⟩, secret), (⟨"public", 1⟩, publicValue)], 0, []⟩

/-- Representation lemma. Concrete `LowEquivalent` acceptance check when only negative (secret) local bindings differ (approved: APPR-C7-agy-5d94c4b4d).

Secret local changes are invisible; a planted public local change rejects low-equivalence. -/
example : LowEquivalent labelling (mixedConfig (-1) 7) (mixedConfig (-2) 7) := by
  simp [LowEquivalent, publicLocals, labelling, mixedConfig]

/-- Representation lemma. Concrete `LowEquivalent` refusal check when non-negative (public) local bindings differ (approved: APPR-C7-agy-5d94c4b4d). -/
example : ¬ LowEquivalent labelling (mixedConfig (-1) 7) (mixedConfig (-2) 8) := by
  simp [LowEquivalent, publicLocals, labelling, mixedConfig]

/-- A store-sensitive labelling supplies the corresponding public-store negative control. -/
abbrev storeSensitiveLabelling : Labelling ops where
  PublicValue := Int
  project := fun _ => none
  lowStore := fun s₁ s₂ => s₁ = s₂

/-- Representation lemma. Concrete `LowEquivalent` refusal check under `storeSensitiveLabelling` when store states differ (approved: APPR-C7-agy-5d94c4b4d). -/
example : ¬ LowEquivalent storeSensitiveLabelling (leakyStart 1) (leakyStart 2) := by
  simp [LowEquivalent, storeSensitiveLabelling, leakyStart]

/-- Helper (computation). Evaluates `publicLocals` on `Demo.leakyStart` to show secret store states are `LowEquivalent` under `Demo.labelling` (approved: APPR-C7-agy-5d94c4b4d). -/
theorem leaky_low (s₁ s₂ : Int) : LowEquivalent labelling (leakyStart s₁) (leakyStart s₂) := by
  simp [LowEquivalent, publicLocals, labelling, leakyStart]

/-- Helper (computation). Checks that every run from `Demo.leakyStart` emits only releases permitted by `Demo.policy` (approved: APPR-C7-agy-5d94c4b4d). -/
theorem release_authorized (secret : Int) :
    policy.RunsAuthorized instrumentation program (leakyStart secret) := by
  intro r
  rw [leaky_run_releases secret r]
  simp [ReleasePolicy.Authorized, ReleasePolicy.Permits, releaseZero, releaseValue]

/-- Helper (computation). Checks that every run from `Demo.ctStart` emits only releases permitted by `Demo.policy` (approved: APPR-C7-agy-5d94c4b4d). -/
theorem ct_release_authorized (secret : Int) :
    policy.RunsAuthorized instrumentation program (ctStart secret) := by
  intro r
  rw [(ct_run_unique secret r).2.2.1]
  simp [ReleasePolicy.Authorized, ReleasePolicy.Permits, releaseZero, releaseValue]

/-- Helper (inversion). Inverts the concrete complete runs to refute the corresponding
noninterference claim. The proof applies the distinct-secret premise, so it is excluded
from Law evidence by the projection convention (approved: PENDING).

**Authorization does not imply NI.** All runs from leakyStart s₁ and leakyStart s₂ emit only
policy-authorized releases, yet `DelimitedReleaseNI` fails for any distinct secrets s₁ ≠ s₂
because their public outputs differ. -/
theorem authorized_release_not_ni (s₁ s₂ : Int)
    (_hauth₁ : policy.RunsAuthorized instrumentation program (leakyStart s₁))
    (_hauth₂ : policy.RunsAuthorized instrumentation program (leakyStart s₂))
    (hlow : LowEquivalent labelling (leakyStart s₁) (leakyStart s₂))
    (hs : s₁ ≠ s₂) :
    ¬ DelimitedReleaseNI instrumentation program labelling (leakyStart s₁) (leakyStart s₂) := by
  intro hni
  have hobs := hni (leakyRun s₁) (leakyRun s₂) hlow rfl rfl
  simp [Run.publicObservations, leakyRun, finished, Config.final?] at hobs
  exact hs hobs

/-- Helper (inversion). Inverts the concrete complete runs to refute the corresponding
noninterference claim. The proof applies the distinct-secret premise, so it is excluded
from Law evidence by the projection convention (approved: PENDING).

**Authorization does not imply CT.** All runs from leakyStart s₁ and leakyStart s₂ emit only
policy-authorized releases, yet `ConstantTime` fails for any distinct secrets s₁ ≠ s₂ because
their variable-time leakage traces differ. -/
theorem authorized_release_not_ct (s₁ s₂ : Int)
    (_hauth₁ : policy.RunsAuthorized instrumentation program (leakyStart s₁))
    (_hauth₂ : policy.RunsAuthorized instrumentation program (leakyStart s₂))
    (hlow : LowEquivalent labelling (leakyStart s₁) (leakyStart s₂))
    (hs : s₁ ≠ s₂) :
    ¬ ConstantTime instrumentation program labelling (leakyStart s₁) (leakyStart s₂) := by
  intro hct
  have hleak := hct (leakyRun s₁) (leakyRun s₂) hlow rfl rfl
  simp [leakyRun] at hleak
  exact hs hleak

end Demo

/-! ## Accounting conservation -/

/-- Law (subject `Account.alloc`). -/
theorem Account.alloc_zero (a : Account) (h : a.WF) : a.alloc 0 = a := by
  cases a
  simp_all [Account.alloc, Account.WF]

/-- Law (subject `Memory.set`).

Writing back the existing account at a lineage is the identity on `Memory`. -/
theorem Memory.set_same (m : Memory) (lineage : LineageId) :
    Memory.set m lineage (m lineage) = m := by
  funext l
  by_cases h : l = lineage <;> simp [Memory.set, h]

/-- Helper (arithmetic). Initializes the local peak at live bytes and retains the historical
peak bound; used to initialize `function_entry_peak` (approved: APPR-C7-agy-5d94c4b4d). -/
theorem Account.enter_wf (a : Account) (h : a.WF) : a.enter.WF := by
  cases a
  simp_all [Account.enter, Account.WF]

/-- Helper (arithmetic). Upper-bounds live + bytes by both `max` expressions in `Account.alloc` for single-step WF preservation (approved: APPR-C7-agy-5d94c4b4d). -/
theorem Account.alloc_wf (a : Account) (bytes : Nat) : (a.alloc bytes).WF := by
  exact ⟨Nat.le_max_right _ _, Nat.le_max_right _ _⟩

/-- Helper (arithmetic). Uses `Nat.sub_le` transitivity to preserve peak bounds across `Account.free` for single-step WF preservation (approved: APPR-C7-agy-5d94c4b4d). -/
theorem Account.free_wf (a : Account) (bytes : Nat) (h : a.WF) : (a.free bytes).WF := by
  exact ⟨Nat.le_trans (Nat.sub_le _ _) h.1, Nat.le_trans (Nat.sub_le _ _) h.2⟩

/-- Helper (arithmetic). Shows zero live bytes after `Account.reset` satisfy the non-negative peak bounds for single-step WF preservation (approved: APPR-C7-agy-5d94c4b4d). -/
theorem Account.reset_wf (a : Account) : a.reset.WF := by
  simp [Account.WF, Account.reset]

/-- Helper (inversion). Lifts `Account.alloc_wf`, `Account.free_wf`, and `Account.reset_wf` across one successful `apply` transition for `run_wf` and `run_peak_exact` (approved: APPR-C7-agy-5d94c4b4d).

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

/-- Law (subject `run`).

Every successful sequence preserves well-formedness at every lineage. -/
theorem run_wf (m : Memory) (actions : List Action)
    (hwf : ∀ l, (m l).WF) (hrun : (run m actions).isSome = true) :
    ∃ m', run m actions = some m' ∧ ∀ l, (m' l).WF := by
  obtain ⟨m', h⟩ := Option.isSome_iff_exists.mp hrun
  clear hrun
  refine ⟨m', h, ?_⟩
  induction actions generalizing m with
  | nil => cases h; exact hwf
  | cons action rest ih =>
    cases ha : apply m action with
    | none => simp [run, ha] at h
    | some m₁ =>
      have hr : run m₁ rest = some m' := by simpa [run, ha] using h
      exact ih m₁ (apply_wf ha hwf) hr

/-- Law (subject `apply`).

A zero-byte allocation changes no account at any lineage. -/
theorem apply_alloc_zero {m : Memory} (hwf : ∀ l, (m l).WF) (lineage : LineageId) :
    apply m (.alloc lineage 0) = some m := by
  simp only [apply, Account.alloc_zero (m lineage) (hwf lineage), Memory.set_same]

/-- Helper (computation). Exposes the fields of a successful free and its frame for the
independent byte-balance theorem `free_conservation` (approved: APPR-C7-agy-5d94c4b4d). -/
theorem free_update (m : Memory) (lineage : LineageId) (bytes : Nat)
    (hle : bytes ≤ (m lineage).live) :
    ∃ m', apply m (.free lineage bytes) = some m' ∧
      (m' lineage).requested = (m lineage).requested ∧
      (m' lineage).live = (m lineage).live - bytes ∧
      (m' lineage).peak = (m lineage).peak ∧
      (m' lineage).functionPeak = (m lineage).functionPeak ∧
      ∀ l, l ≠ lineage → m' l = m l := by
  refine ⟨Memory.set m lineage ((m lineage).free bytes), ?_, ?_⟩
  · simp [apply, hle]
  · simp only [Memory.set, ↓reduceIte]
    refine ⟨rfl, rfl, rfl, rfl, ?_⟩
    intro l hl
    simp [hl]

/-- Law (subject `apply`).

A successful free balances the remaining bytes plus its outflow against the bytes owned before
it, while retaining request history, both peaks, and all other lineages. -/
theorem free_conservation (m : Memory) (lineage : LineageId) (bytes : Nat)
    (hle : bytes ≤ (m lineage).live) :
    ∃ m', apply m (.free lineage bytes) = some m' ∧
      (m' lineage).requested = (m lineage).requested ∧
      (m' lineage).live + bytes = (m lineage).live ∧
      (m' lineage).peak = (m lineage).peak ∧
      (m' lineage).functionPeak = (m lineage).functionPeak ∧
      ∀ l, l ≠ lineage → m' l = m l := by
  obtain ⟨m', h, hreq, hlive, hp, hfp, hframe⟩ := free_update m lineage bytes hle
  refine ⟨m', h, hreq, ?_, hp, hfp, hframe⟩
  omega

/-- Law (subject `apply`).

A reset satisfies the independent `ResetConservation` predicate with the actual live bytes before
the transition as its outflow, and leaves every other lineage unchanged. -/
theorem reset_conservation (m : Memory) (lineage : LineageId) :
    ∃ m', apply m (.reset lineage) = some m' ∧
      ResetConservation (m lineage) (m' lineage) (resetOutflow lineage m (.reset lineage)) ∧
      ∀ l, l ≠ lineage → m' l = m l := by
  refine ⟨Memory.set m lineage (m lineage).reset, rfl, ?_, ?_⟩
  · simp [ResetConservation, resetOutflow, Memory.set, Account.reset]
  · intro l hl
    simp [Memory.set, hl]

/-- Helper (computation). Unfolds one cons step of `resetBytes` on a successful `apply` transition for `run_conservation` (approved: APPR-C7-agy-5d94c4b4d). -/
@[simp] theorem resetBytes_cons {m m' : Memory} {action : Action} {rest : List Action}
    (lineage : LineageId) (h : apply m action = some m') :
    resetBytes lineage m (action :: rest) =
      resetOutflow lineage m action + resetBytes lineage m' rest := by
  simp [resetBytes, h]

/-- Law (subject `run`).

**Whole-sequence conservation.** Requested bytes increase by exactly the sequence's allocation
requests. Over the integers, net live bytes equal allocations minus explicit frees and reset
outflows read from the actual intermediate states; no truncated natural subtraction hides an
outflow. The statement holds independently for every allocator lineage, including untouched ones. -/
theorem run_conservation (m : Memory) (actions : List Action) (lineage : LineageId)
    (hrun : (run m actions).isSome = true) :
    ∃ m', run m actions = some m' ∧
      (m' lineage).requested = (m lineage).requested + allocated lineage actions ∧
      ((m' lineage).live : Int) = (m lineage).live + allocated lineage actions -
        freed lineage actions - resetBytes lineage m actions := by
  obtain ⟨m', h⟩ := Option.isSome_iff_exists.mp hrun
  clear hrun
  refine ⟨m', h, ?_⟩
  induction actions generalizing m with
  | nil => cases h; simp [allocated, freed, resetBytes]
  | cons action rest ih =>
    cases ha : apply m action with
    | none => simp [run, ha] at h
    | some m₁ =>
      have hr : run m₁ rest = some m' := by simpa [run, ha] using h
      have hc := ih m₁ hr
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

/-- Helper (arithmetic). Shows one successful `apply` transition never decreases peak or functionPeak for `run_peak_monotone` (approved: APPR-C7-agy-5d94c4b4d).

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

/-- Law (subject `run`).

No successful sequence lowers either the historical or function-local peak of any lineage. -/
theorem run_peak_monotone (m : Memory) (actions : List Action) (lineage : LineageId)
    (hrun : (run m actions).isSome = true) :
    ∃ m', run m actions = some m' ∧
      (m lineage).peak ≤ (m' lineage).peak ∧
      (m lineage).functionPeak ≤ (m' lineage).functionPeak := by
  obtain ⟨m', h⟩ := Option.isSome_iff_exists.mp hrun
  clear hrun
  refine ⟨m', h, ?_⟩
  induction actions generalizing m with
  | nil => cases h; exact ⟨Nat.le_refl _, Nat.le_refl _⟩
  | cons action rest ih =>
    cases ha : apply m action with
    | none => simp [run, ha] at h
    | some m₁ =>
      have hr : run m₁ rest = some m' := by simpa [run, ha] using h
      have ha' := apply_peak_monotone ha lineage
      have hr' := ih m₁ hr
      exact ⟨Nat.le_trans ha'.1 hr'.1, Nat.le_trans ha'.2 hr'.2⟩

/-- Helper (computation). Expresses the post-step functionPeak as `max` of the pre-step functionPeak and post-step live bytes for `run_peak_exact` (approved: APPR-C7-agy-5d94c4b4d).

Each successful transition records the maximum of its old peak and new live bytes. -/
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

/-- Law (subject `run`).

The recorded local peak equals the maximum of the entry functionPeak and the independently
observed maximum `observedPeak` over the live-byte trajectory. -/
theorem run_peak_exact (m : Memory) (actions : List Action) (lineage : LineageId)
    (hwf : ∀ l, (m l).WF) (hrun : (run m actions).isSome = true) :
    ∃ m', run m actions = some m' ∧
      (m' lineage).functionPeak = max (m lineage).functionPeak (observedPeak lineage m actions) := by
  obtain ⟨m', h⟩ := Option.isSome_iff_exists.mp hrun
  clear hrun
  refine ⟨m', h, ?_⟩
  induction actions generalizing m with
  | nil =>
    cases h
    exact (Nat.max_eq_left (hwf lineage).2).symm
  | cons action rest ih =>
    cases ha : apply m action with
    | none => simp [run, ha] at h
    | some m₁ =>
      have hr : run m₁ rest = some m' := by simpa [run, ha] using h
      rw [ih m₁ (apply_wf ha hwf) hr, apply_peak_exact hwf ha lineage]
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

/-- Law (subject `Account.enter`).

Initializing function accounting and then executing a successful action sequence records exactly
its independently observed live-byte maximum, with no contribution from a caller's local peak. -/
theorem function_entry_peak (m : Memory) (actions : List Action) (lineage : LineageId)
    (hwf : ∀ l, (m l).WF)
    (hrun : (run (fun l => (m l).enter) actions).isSome = true) :
    ∃ m', run (fun l => (m l).enter) actions = some m' ∧
      (m' lineage).functionPeak = observedPeak lineage (fun l => (m l).enter) actions := by
  obtain ⟨m', h, hp⟩ := run_peak_exact (fun l => (m l).enter) actions lineage
    (fun l => Account.enter_wf (m l) (hwf l)) hrun
  refine ⟨m', h, ?_⟩
  have hentry : (m lineage).live ≤ observedPeak lineage (fun l => (m l).enter) actions := by
    cases actions with
    | nil => exact Nat.le_refl _
    | cons a xs =>
      simp only [observedPeak]
      split
      · exact Nat.le_refl _
      · exact Nat.le_max_left _ _
  change (m' lineage).functionPeak =
    max (m lineage).live (observedPeak lineage (fun l => (m l).enter) actions) at hp
  rw [Nat.max_eq_right hentry] at hp
  exact hp

/-- Law (subject `run`).

Independent analysis bounds certify any `LineageReport` (whether all bounds are established, some
fields are .notEstablished, or the allocator defines no footprint) for an actual function run.
Peak evidence is derived from the independent live-state trajectory `observedPeak` and function
entry initialization (m lineage).functionPeak = (m lineage).live, never from the exit peak. -/
theorem report_certified (m : Memory) (actions : List Action) (allocator : AllocatorContract)
    (lineage : LineageId) (r : LineageReport)
    (hlineage : r.lineage = lineage)
    (hwf : ∀ l, (m l).WF)
    (hentry : (m lineage).functionPeak = (m lineage).live)
    (hrun : (run m actions).isSome = true)
    (hrequested : Bound.Sound (allocated lineage actions) r.requested)
    (hnet : Bound.Sound ((allocated lineage actions : Int) - freed lineage actions -
      resetBytes lineage m actions) r.netChange)
    (hpeak : Bound.Sound (observedPeak lineage m actions) r.peak)
    (hfootprint : Bound.SoundOptional (allocator.footprint lineage m actions) r.footprint) :
    ∃ m', run m actions = some m' ∧
      LineageReport.Sound lineage (m lineage) (m' lineage)
        (allocator.footprint lineage m actions) r := by
  obtain ⟨m', h, hreqEq, hliveEq⟩ := run_conservation m actions lineage hrun
  obtain ⟨m'', h', hpeakEq⟩ := run_peak_exact m actions lineage hwf hrun
  rw [h] at h'
  cases h'
  refine ⟨m', h, hlineage, ?_, ?_, ?_, hfootprint⟩
  · have hd : (m' lineage).requested - (m lineage).requested = allocated lineage actions := by omega
    rw [hd]
    exact hrequested
  · have hn : ((m' lineage).live : Int) - ((m lineage).live : Int) =
        (allocated lineage actions : Int) - freed lineage actions -
          resetBytes lineage m actions := by omega
    rw [hn]
    exact hnet
  · intro hstatus
    rw [hpeakEq, hentry]
    have hobserved : (m lineage).live ≤ observedPeak lineage m actions := by
      cases actions with
      | nil => exact Nat.le_refl _
      | cons a xs =>
        simp only [observedPeak]
        split
        · exact Nat.le_refl _
        · exact Nat.le_max_left _ _
    rw [Nat.max_eq_right hobserved]
    exact hpeak hstatus

/-! ## Concrete accounting fixtures and definition controls -/

/-- A nonempty well-formed account and a second lineage with independent accounting. -/
def accountA : Account := ⟨100, 5, 12, 9⟩
def accountB : Account := ⟨7, 2, 2, 2⟩
def memoryA : Memory := Memory.set (Memory.set Memory.empty ⟨0⟩ accountA) ⟨1⟩ accountB

def conservationActions : List Action := [.alloc ⟨0⟩ 8, .free ⟨0⟩ 3, .reset ⟨0⟩]
def twoLineageActions : List Action :=
  [.alloc ⟨0⟩ 8, .alloc ⟨1⟩ 4, .free ⟨0⟩ 3, .reset ⟨0⟩,
    .alloc ⟨0⟩ 6, .reset ⟨1⟩, .reset ⟨0⟩]

/-- Peaks lowered below live bytes. -/
def badPeakAccount : Account := { accountA.alloc 10 with peak := 14, functionPeak := 14 }

/-- Helper (computation). Evaluates `Account.WF` across all lineages of the concrete `memoryA` fixture for Audit instances and mutants (approved: APPR-C7-agy-5d94c4b4d). -/
theorem memoryA_wf : ∀ l, (memoryA l).WF := by
  intro l
  by_cases h₀ : l = ⟨0⟩
  · subst l
    simp [memoryA, Memory.set, Account.WF, accountA]
  · by_cases h₁ : l = ⟨1⟩
    · subst l
      simp [memoryA, Memory.set, Account.WF, accountB]
    · simp [memoryA, Memory.set, h₀, h₁, Account.WF, Memory.empty, Account.empty]

/-- Representation lemma. Concrete initialization check showing `Account.empty` is well-formed and `Memory.empty` maps lineage 0 to `Account.empty` (approved: APPR-C7-agy-5d94c4b4d). -/
example : Account.empty.WF ∧ Memory.empty ⟨0⟩ = Account.empty := by
  simp [Account.WF, Memory.empty, Account.empty]

/-- Representation lemma. Concrete refusal check showing an account whose live exceeds peak and functionPeak fails `Account.WF` (approved: APPR-C7-agy-5d94c4b4d). -/
example : ¬ (⟨0, 5, 0, 0⟩ : Account).WF := by simp [Account.WF]

/-- Representation lemma. Concrete frame check showing `Memory.set` on lineage 0 leaves lineage 1 unchanged at `accountB` (approved: APPR-C7-agy-5d94c4b4d). -/
example : Memory.set memoryA ⟨0⟩ (accountA.alloc 1) ⟨1⟩ = accountB := rfl

/-- Representation lemma. Concrete refusal check showing `badPeakAccount` (where peaks are 14 but live is 15) fails `Account.WF` (approved: APPR-C7-agy-5d94c4b4d). -/
example : ¬ badPeakAccount.WF := by
  simp [badPeakAccount, Account.WF, accountA, Account.alloc]

/-- Representation lemma. Concrete refusal check showing `run` returns `none` when an intermediate .free exceeds current live bytes (approved: APPR-C7-agy-5d94c4b4d). -/
example : run Memory.empty [.alloc ⟨0⟩ 8, .free ⟨0⟩ 9, .alloc ⟨0⟩ 1] = none := by decide

/-- Representation lemma. Concrete witness showing live bytes (unlike peak and functionPeak) strictly decrease on a non-zero .free transition (approved: APPR-C7-agy-5d94c4b4d). -/
example : ∃ m m' : Memory, ∃ l : LineageId,
    apply m (.free l 3) = some m' ∧ ¬ (m l).live ≤ (m' l).live := by
  refine ⟨memoryA, Memory.set memoryA ⟨0⟩ (accountA.free 3), ⟨0⟩, rfl, ?_⟩
  decide

/-- Representation lemma. Concrete per-lineage sum check for `allocated` on interleaved `twoLineageActions` (approved: APPR-C7-agy-5d94c4b4d). -/
example : allocated ⟨0⟩ twoLineageActions = 14 ∧ allocated ⟨1⟩ twoLineageActions = 4 := by decide

/-- Representation lemma. Concrete per-lineage sum check for `freed` on interleaved `twoLineageActions` (approved: APPR-C7-agy-5d94c4b4d). -/
example : freed ⟨0⟩ twoLineageActions = 3 ∧ freed ⟨1⟩ twoLineageActions = 0 := by decide

/-- Representation lemma. Concrete per-lineage outflow check for `resetBytes` across multiple resets in `twoLineageActions` (approved: APPR-C7-agy-5d94c4b4d). -/
example : resetBytes ⟨0⟩ memoryA twoLineageActions = 16 ∧
    resetBytes ⟨1⟩ memoryA twoLineageActions = 6 := by decide

/-- Representation lemma. Concrete check that `Bound.Sound` holds when an .boundEstablished limit is at least the actual value (approved: APPR-C7-agy-5d94c4b4d). -/
example : Bound.Sound 10 ⟨12, .boundEstablished⟩ := by simp [Bound.Sound]

/-- Representation lemma. Concrete check that `Bound.Sound` fails when an .boundEstablished limit is strictly less than the actual value (approved: APPR-C7-agy-5d94c4b4d). -/
example : ¬ Bound.Sound 10 ⟨9, .boundEstablished⟩ := by simp [Bound.Sound]

/-- Representation lemma. Concrete check that `Bound.SoundOptional` holds when an optional footprint is some 10 and the established limit is 12 (approved: APPR-C7-agy-5d94c4b4d). -/
example : Bound.SoundOptional (some 10) ⟨12, .boundEstablished⟩ := by
  simp [Bound.SoundOptional]

/-- Representation lemma. Concrete check that `Bound.SoundOptional` fails when an optional footprint is some 10 and the established limit is 9 (approved: APPR-C7-agy-5d94c4b4d). -/
example : ¬ Bound.SoundOptional (some 10) ⟨9, .boundEstablished⟩ := by
  simp [Bound.SoundOptional]

/-- Representation lemma. Concrete check that `Bound.SoundOptional` fails when the allocator footprint is `none` but the bound claims .boundEstablished (approved: APPR-C7-agy-5d94c4b4d). -/
example : ¬ Bound.SoundOptional none ⟨12, .boundEstablished⟩ := by
  simp [Bound.SoundOptional]

/-- Example allocator contract: its footprint is the independent logical trajectory maximum.
A real allocator may include metadata and slack; no such equality is assumed generically. -/
def demoAllocator : AllocatorContract := ⟨fun lineage m actions =>
  some (observedPeak lineage m actions)⟩

/-- Allocator contract that defines no footprint (`none`), exercising .notEstablished footprint
reports. -/
def noFootprintAllocator : AllocatorContract := ⟨fun _ _ _ => none⟩

/-- Entry for the concrete report example: 100 historical requested bytes and two live bytes. -/
def reportEntry : Account := ⟨100, 2, 100, 2⟩

def reportEntryMemory : Memory := Memory.set Memory.empty ⟨0⟩ reportEntry

def reportActions : List Action := [.alloc ⟨0⟩ 8, .free ⟨0⟩ 3]

def reportExit : Account := ⟨108, 7, 100, 10⟩

/-- Entry memory for an empty-run function after `Account.enter` on 100 historical requested bytes
and zero live bytes. -/
def emptyFunctionEntryMemory : Memory :=
  Memory.set Memory.empty ⟨0⟩ (Account.enter ⟨100, 0, 100, 0⟩)

/-- An independently written report with all fields established. -/
def establishedReport : LineageReport :=
  ⟨⟨0⟩, ⟨8, .boundEstablished⟩, ⟨5, .boundEstablished⟩,
    ⟨10, .boundEstablished⟩, ⟨12, .boundEstablished⟩⟩

/-- A mixed-evidence report: net change is .notEstablished, while the other three fields are
.boundEstablished. -/
def mixedStatusReport : LineageReport :=
  ⟨⟨0⟩, ⟨8, .boundEstablished⟩, ⟨-100, .notEstablished⟩,
    ⟨10, .boundEstablished⟩, ⟨12, .boundEstablished⟩⟩

/-- Report for an empty function run under `noFootprintAllocator`: zero local request, net, and
peak bounds are .boundEstablished, while footprint is .notEstablished. -/
def emptyFunctionReport : LineageReport :=
  ⟨⟨0⟩, ⟨0, .boundEstablished⟩, ⟨0, .boundEstablished⟩,
    ⟨0, .boundEstablished⟩, ⟨0, .notEstablished⟩⟩

/-- Representation lemma. Concrete refusal check showing `LineageReport.Sound` rejects a report whose lineage field (⟨1⟩) does not match the queried lineage (⟨0⟩) (approved: APPR-C7-agy-5d94c4b4d). -/
example : ¬ LineageReport.Sound ⟨0⟩ reportEntry reportExit (some 10)
    { establishedReport with lineage := ⟨1⟩ } := by
  simp [LineageReport.Sound, establishedReport]

/-! ## Closed law instances and premise-preserving planted subjects -/

namespace Audit

/-- Mutant authorization predicate that accepts any nonempty release trace regardless of the policy. -/
def ignoresPolicyAuthorized {Ty Val : Type} (_ : ReleasePolicy Ty)
    (rs : List (Release Ty Val)) : Prop :=
  rs ≠ []

/-- Instance (law `forbidsRelease_not_authorized`, case `emptyPolicy`). -/
example : ¬ (ReleasePolicy.mk ([] : List (ReleaseSite Unit))).Authorized [Demo.releaseZero] :=
  forbidsRelease_not_authorized (ReleasePolicy.mk []) Demo.releaseZero [] rfl

/-- Planted (law `forbidsRelease_not_authorized`, case `emptyPolicy`, subject `ReleasePolicy.Authorized`, mutant `ignoresPolicyAuthorized`). -/
example : (ReleasePolicy.mk ([] : List (ReleaseSite Unit))).ForbidsRelease ∧
    ¬ (¬ ignoresPolicyAuthorized (ReleasePolicy.mk ([] : List (ReleaseSite Unit)))
      [Demo.releaseZero]) :=
  ⟨rfl, fun h => h nofun⟩

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

/-- A setter with the same signature corrupts live even when writing back the existing account. -/
def corruptSetter (m : Memory) (lineage : LineageId) (a : Account) : Memory :=
  Memory.set m lineage { a with live := a.live + 1 }

/-- Instance (law `Memory.set_same`, case `memoryA`). -/
example : Memory.set memoryA ⟨0⟩ (memoryA ⟨0⟩) = memoryA :=
  Memory.set_same memoryA ⟨0⟩

/-- Planted (law `Memory.set_same`, case `memoryA`, subject `Memory.set`, mutant `corruptSetter`). -/
example : ¬ (corruptSetter memoryA ⟨0⟩ (memoryA ⟨0⟩) = memoryA) := by
  intro h
  have hc := congrArg (fun m => (m ⟨0⟩).live) h
  change (6 : Nat) = 5 at hc
  omega

/-- Mutant function-entry operation that clears functionPeak to zero instead of `a.live`. -/
def enterLowersPeak (a : Account) : Account :=
  { a.enter with functionPeak := 0 }

/-- Instance (law `function_entry_peak`, case `freeAfterEntry`). -/
example : ∃ m', run (fun l => (memoryA l).enter) [.free ⟨0⟩ 3] = some m' ∧
    (m' ⟨0⟩).functionPeak = observedPeak ⟨0⟩ (fun l => (memoryA l).enter) [.free ⟨0⟩ 3] :=
  function_entry_peak memoryA [.free ⟨0⟩ 3] ⟨0⟩ memoryA_wf rfl

/-- Planted (law `function_entry_peak`, case `freeAfterEntry`, subject `Account.enter`, mutant `enterLowersPeak`). -/
example : (∀ l, (memoryA l).WF) ∧
    (run (fun l => enterLowersPeak (memoryA l)) [.free ⟨0⟩ 3]).isSome = true ∧
    ¬ (∃ m', run (fun l => enterLowersPeak (memoryA l)) [.free ⟨0⟩ 3] = some m' ∧
      (m' ⟨0⟩).functionPeak =
        observedPeak ⟨0⟩ (fun l => enterLowersPeak (memoryA l)) [.free ⟨0⟩ 3]) := by
  refine ⟨memoryA_wf, rfl, ?_⟩
  rintro ⟨m', hm', hp⟩
  cases hm'
  change (0 : Nat) = 5 at hp
  omega

/-- A mutant transition corrupts an unrelated lineage on any action. -/
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

/-- Instance (law `free_conservation`, case `memoryA`). -/
example : ∃ m', apply memoryA (.free ⟨0⟩ 3) = some m' ∧
    (m' ⟨0⟩).requested = (memoryA ⟨0⟩).requested ∧
    (m' ⟨0⟩).live + 3 = (memoryA ⟨0⟩).live ∧
    (m' ⟨0⟩).peak = (memoryA ⟨0⟩).peak ∧
    (m' ⟨0⟩).functionPeak = (memoryA ⟨0⟩).functionPeak ∧
    ∀ l, l ≠ ⟨0⟩ → m' l = memoryA l :=
  free_conservation memoryA ⟨0⟩ 3 (by decide)

/-- Planted (law `free_conservation`, case `memoryA`, subject `apply`, mutant `applyCorruptsOther`). -/
example : 3 ≤ (memoryA ⟨0⟩).live ∧
    ¬ (∃ m', applyCorruptsOther memoryA (.free ⟨0⟩ 3) = some m' ∧
      (m' ⟨0⟩).requested = (memoryA ⟨0⟩).requested ∧
      (m' ⟨0⟩).live + 3 = (memoryA ⟨0⟩).live ∧
      (m' ⟨0⟩).peak = (memoryA ⟨0⟩).peak ∧
      (m' ⟨0⟩).functionPeak = (memoryA ⟨0⟩).functionPeak ∧
      ∀ l, l ≠ ⟨0⟩ → m' l = memoryA l) := by
  refine ⟨by decide, ?_⟩
  rintro ⟨m', hm', _, hlive, _, _, _⟩
  cases hm'
  change (5 : Nat) + 3 = 5 at hlive
  omega

/-- Mutant transition that clears peak and functionPeak on a reset instead of retaining them. -/
def applyBadReset (m : Memory) : Action → Option Memory
  | .reset lineage => some (Memory.set m lineage { (m lineage).reset with peak := 0, functionPeak := 0 })
  | action => apply m action

/-- Instance (law `reset_conservation`, case `memoryA`). -/
example : ∃ m', apply memoryA (.reset ⟨0⟩) = some m' ∧
    ResetConservation (memoryA ⟨0⟩) (m' ⟨0⟩) (resetOutflow ⟨0⟩ memoryA (.reset ⟨0⟩)) ∧
    ∀ l, l ≠ ⟨0⟩ → m' l = memoryA l :=
  reset_conservation memoryA ⟨0⟩

/-- Planted (law `reset_conservation`, case `memoryA`, subject `apply`, mutant `applyBadReset`). -/
example : ¬ (∃ m', applyBadReset memoryA (.reset ⟨0⟩) = some m' ∧
    ResetConservation (memoryA ⟨0⟩) (m' ⟨0⟩) (resetOutflow ⟨0⟩ memoryA (.reset ⟨0⟩)) ∧
    ∀ l, l ≠ ⟨0⟩ → m' l = memoryA l) := by
  rintro ⟨m', hm', hrc, _⟩
  cases hm'
  have hp : (0 : Nat) = 12 := hrc.2.2.2.1
  omega

/-- One defective executor: its reported result corrupts history, live bytes and both maxima. -/
def brokenMemory : Memory := Memory.set memoryA ⟨0⟩ ⟨999, 99, 0, 0⟩
def brokenRun (_ : Memory) (_ : List Action) : Option Memory := some brokenMemory

/-- Instance (law `run_wf`, case `conservationActions`). -/
example : ∃ m', run memoryA conservationActions = some m' ∧ ∀ l, (m' l).WF :=
  run_wf memoryA conservationActions memoryA_wf rfl

/-- Planted (law `run_wf`, case `conservationActions`, subject `run`, mutant `brokenRun`). -/
example : (∀ l, (memoryA l).WF) ∧ (brokenRun memoryA conservationActions).isSome = true ∧
    ¬ (∃ m', brokenRun memoryA conservationActions = some m' ∧ ∀ l, (m' l).WF) := by
  refine ⟨memoryA_wf, rfl, ?_⟩
  rintro ⟨m', hm', hwf⟩
  cases hm'
  have h₀ := hwf ⟨0⟩
  change 99 ≤ 0 ∧ 99 ≤ 0 at h₀
  omega

/-- Instance (law `run_conservation`, case `conservationActions`). -/
example : ∃ m', run memoryA conservationActions = some m' ∧
    (m' ⟨0⟩).requested = (memoryA ⟨0⟩).requested + allocated ⟨0⟩ conservationActions ∧
    ((m' ⟨0⟩).live : Int) = (memoryA ⟨0⟩).live + allocated ⟨0⟩ conservationActions -
      freed ⟨0⟩ conservationActions - resetBytes ⟨0⟩ memoryA conservationActions :=
  run_conservation memoryA conservationActions ⟨0⟩ rfl

/-- Planted (law `run_conservation`, case `conservationActions`, subject `run`, mutant `brokenRun`). -/
example : (brokenRun memoryA conservationActions).isSome = true ∧
    ¬ (∃ m', brokenRun memoryA conservationActions = some m' ∧
      (m' ⟨0⟩).requested = (memoryA ⟨0⟩).requested + allocated ⟨0⟩ conservationActions ∧
      ((m' ⟨0⟩).live : Int) = (memoryA ⟨0⟩).live + allocated ⟨0⟩ conservationActions -
        freed ⟨0⟩ conservationActions - resetBytes ⟨0⟩ memoryA conservationActions) := by
  refine ⟨rfl, ?_⟩
  rintro ⟨m', hm', hreq, _⟩
  cases hm'
  change (999 : Nat) = 108 at hreq
  omega

/-- Instance (law `run_peak_monotone`, case `conservationActions`). -/
example : ∃ m', run memoryA conservationActions = some m' ∧
    (memoryA ⟨0⟩).peak ≤ (m' ⟨0⟩).peak ∧
    (memoryA ⟨0⟩).functionPeak ≤ (m' ⟨0⟩).functionPeak :=
  run_peak_monotone memoryA conservationActions ⟨0⟩ rfl

/-- Planted (law `run_peak_monotone`, case `conservationActions`, subject `run`, mutant `brokenRun`). -/
example : (brokenRun memoryA conservationActions).isSome = true ∧
    ¬ (∃ m', brokenRun memoryA conservationActions = some m' ∧
      (memoryA ⟨0⟩).peak ≤ (m' ⟨0⟩).peak ∧
      (memoryA ⟨0⟩).functionPeak ≤ (m' ⟨0⟩).functionPeak) := by
  refine ⟨rfl, ?_⟩
  rintro ⟨m', hm', hpeak, _⟩
  cases hm'
  change (12 : Nat) ≤ 0 at hpeak
  omega

/-- Instance (law `run_peak_exact`, case `conservationActions`). -/
example : ∃ m', run memoryA conservationActions = some m' ∧
    (m' ⟨0⟩).functionPeak = max (memoryA ⟨0⟩).functionPeak
      (observedPeak ⟨0⟩ memoryA conservationActions) :=
  run_peak_exact memoryA conservationActions ⟨0⟩ memoryA_wf rfl

/-- Planted (law `run_peak_exact`, case `conservationActions`, subject `run`, mutant `brokenRun`). -/
example : (∀ l, (memoryA l).WF) ∧ (brokenRun memoryA conservationActions).isSome = true ∧
    ¬ (∃ m', brokenRun memoryA conservationActions = some m' ∧
      (m' ⟨0⟩).functionPeak = max (memoryA ⟨0⟩).functionPeak
        (observedPeak ⟨0⟩ memoryA conservationActions)) := by
  refine ⟨memoryA_wf, rfl, ?_⟩
  rintro ⟨m', hm', hpeak⟩
  cases hm'
  change (0 : Nat) = 13 at hpeak
  omega

/-- Helper (computation). Evaluates `Account.WF` across all lineages of `reportEntryMemory` for `report_certified` instances and mutants (approved: APPR-C7-agy-5d94c4b4d). -/
theorem reportEntry_wf : ∀ l, (reportEntryMemory l).WF := by
  intro l
  by_cases hl : l = ⟨0⟩
  · subst l; simp [reportEntryMemory, Memory.set, reportEntry, Account.WF]
  · simp [reportEntryMemory, Memory.set, hl, Memory.empty, Account.empty, Account.WF]

/-- Helper (computation). Evaluates `Account.WF` across all lineages of `emptyFunctionEntryMemory` for the emptyFunction instance and mutant of `report_certified` (approved: APPR-C7-agy-5d94c4b4d). -/
theorem emptyFunctionEntry_wf : ∀ l, (emptyFunctionEntryMemory l).WF := by
  intro l
  by_cases hl : l = ⟨0⟩
  · subst l; simp [emptyFunctionEntryMemory, Memory.set, Account.enter, Account.WF]
  · simp [emptyFunctionEntryMemory, Memory.set, hl, Memory.empty, Account.empty, Account.WF]

/-- Instance (law `report_certified`, case `established`). -/
example : ∃ m', run reportEntryMemory reportActions = some m' ∧
    LineageReport.Sound ⟨0⟩ (reportEntryMemory ⟨0⟩) (m' ⟨0⟩)
      (demoAllocator.footprint ⟨0⟩ reportEntryMemory reportActions) establishedReport :=
  report_certified reportEntryMemory reportActions demoAllocator ⟨0⟩ establishedReport
    rfl reportEntry_wf rfl rfl
    (fun _ => by decide) (fun _ => by decide) (fun _ => by decide)
    (fun _ => ⟨10, rfl, by decide⟩)

/-- Planted (law `report_certified`, case `established`, subject `run`, mutant `brokenRun`). -/
example : establishedReport.lineage = ⟨0⟩ ∧
    (∀ l, (reportEntryMemory l).WF) ∧
    (reportEntryMemory ⟨0⟩).functionPeak = (reportEntryMemory ⟨0⟩).live ∧
    (brokenRun reportEntryMemory reportActions).isSome = true ∧
    Bound.Sound (allocated ⟨0⟩ reportActions) establishedReport.requested ∧
    Bound.Sound ((allocated ⟨0⟩ reportActions : Int) - freed ⟨0⟩ reportActions -
      resetBytes ⟨0⟩ reportEntryMemory reportActions) establishedReport.netChange ∧
    Bound.Sound (observedPeak ⟨0⟩ reportEntryMemory reportActions) establishedReport.peak ∧
    Bound.SoundOptional (demoAllocator.footprint ⟨0⟩ reportEntryMemory reportActions)
      establishedReport.footprint ∧
    ¬ (∃ m', brokenRun reportEntryMemory reportActions = some m' ∧
      LineageReport.Sound ⟨0⟩ (reportEntryMemory ⟨0⟩) (m' ⟨0⟩)
        (demoAllocator.footprint ⟨0⟩ reportEntryMemory reportActions) establishedReport) := by
  refine ⟨rfl, reportEntry_wf, rfl, rfl, fun _ => by decide, fun _ => by decide,
    fun _ => by decide, fun _ => ⟨10, rfl, by decide⟩, ?_⟩
  rintro ⟨m', hm', hsound⟩
  cases hm'
  have hreq : (899 : Nat) ≤ 8 := hsound.2.1 rfl
  omega

/-- Instance (law `report_certified`, case `mixedStatus`). -/
example : ∃ m', run reportEntryMemory reportActions = some m' ∧
    LineageReport.Sound ⟨0⟩ (reportEntryMemory ⟨0⟩) (m' ⟨0⟩)
      (demoAllocator.footprint ⟨0⟩ reportEntryMemory reportActions) mixedStatusReport :=
  report_certified reportEntryMemory reportActions demoAllocator ⟨0⟩ mixedStatusReport
    rfl reportEntry_wf rfl rfl
    (fun _ => by decide) nofun (fun _ => by decide)
    (fun _ => ⟨10, rfl, by decide⟩)

/-- Planted (law `report_certified`, case `mixedStatus`, subject `run`, mutant `brokenRun`). -/
example : mixedStatusReport.lineage = ⟨0⟩ ∧
    (∀ l, (reportEntryMemory l).WF) ∧
    (reportEntryMemory ⟨0⟩).functionPeak = (reportEntryMemory ⟨0⟩).live ∧
    (brokenRun reportEntryMemory reportActions).isSome = true ∧
    Bound.Sound (allocated ⟨0⟩ reportActions) mixedStatusReport.requested ∧
    Bound.Sound ((allocated ⟨0⟩ reportActions : Int) - freed ⟨0⟩ reportActions -
      resetBytes ⟨0⟩ reportEntryMemory reportActions) mixedStatusReport.netChange ∧
    Bound.Sound (observedPeak ⟨0⟩ reportEntryMemory reportActions) mixedStatusReport.peak ∧
    Bound.SoundOptional (demoAllocator.footprint ⟨0⟩ reportEntryMemory reportActions)
      mixedStatusReport.footprint ∧
    ¬ (∃ m', brokenRun reportEntryMemory reportActions = some m' ∧
      LineageReport.Sound ⟨0⟩ (reportEntryMemory ⟨0⟩) (m' ⟨0⟩)
        (demoAllocator.footprint ⟨0⟩ reportEntryMemory reportActions) mixedStatusReport) := by
  refine ⟨rfl, reportEntry_wf, rfl, rfl, fun _ => by decide, nofun,
    fun _ => by decide, fun _ => ⟨10, rfl, by decide⟩, ?_⟩
  rintro ⟨m', hm', hsound⟩
  cases hm'
  have hreq : (899 : Nat) ≤ 8 := hsound.2.1 rfl
  omega

/-- Instance (law `report_certified`, case emptyFunction). -/
example : ∃ m', run emptyFunctionEntryMemory [] = some m' ∧
    LineageReport.Sound ⟨0⟩ (emptyFunctionEntryMemory ⟨0⟩) (m' ⟨0⟩)
      (noFootprintAllocator.footprint ⟨0⟩ emptyFunctionEntryMemory []) emptyFunctionReport :=
  report_certified emptyFunctionEntryMemory [] noFootprintAllocator ⟨0⟩ emptyFunctionReport
    rfl emptyFunctionEntry_wf rfl rfl
    (fun _ => by decide) (fun _ => by decide) (fun _ => by decide) nofun

/-- Planted (law `report_certified`, case emptyFunction, subject `run`, mutant `brokenRun`). -/
example : emptyFunctionReport.lineage = ⟨0⟩ ∧
    (∀ l, (emptyFunctionEntryMemory l).WF) ∧
    (emptyFunctionEntryMemory ⟨0⟩).functionPeak = (emptyFunctionEntryMemory ⟨0⟩).live ∧
    (brokenRun emptyFunctionEntryMemory []).isSome = true ∧
    Bound.Sound (allocated ⟨0⟩ []) emptyFunctionReport.requested ∧
    Bound.Sound ((allocated ⟨0⟩ [] : Int) - freed ⟨0⟩ [] -
      resetBytes ⟨0⟩ emptyFunctionEntryMemory []) emptyFunctionReport.netChange ∧
    Bound.Sound (observedPeak ⟨0⟩ emptyFunctionEntryMemory []) emptyFunctionReport.peak ∧
    Bound.SoundOptional (noFootprintAllocator.footprint ⟨0⟩ emptyFunctionEntryMemory [])
      emptyFunctionReport.footprint ∧
    ¬ (∃ m', brokenRun emptyFunctionEntryMemory [] = some m' ∧
      LineageReport.Sound ⟨0⟩ (emptyFunctionEntryMemory ⟨0⟩) (m' ⟨0⟩)
        (noFootprintAllocator.footprint ⟨0⟩ emptyFunctionEntryMemory []) emptyFunctionReport) := by
  refine ⟨rfl, emptyFunctionEntry_wf, rfl, rfl, fun _ => by decide, fun _ => by decide,
    fun _ => by decide, nofun, ?_⟩
  rintro ⟨m', hm', hsound⟩
  cases hm'
  have hreq : (899 : Nat) ≤ 0 := hsound.2.1 rfl
  omega

/-- Representation lemma. Concrete trajectory calculation showing `observedPeak` records 10 across `reportActions` (including 2 bytes live at entry) and 2 on an empty action list (approved: APPR-C7-agy-5d94c4b4d). -/
example : observedPeak ⟨0⟩ reportEntryMemory reportActions = 10 ∧
    observedPeak ⟨0⟩ reportEntryMemory [] = 2 := by decide

open Demo

/-- Instance (law `step_unique`, case `outputSeven`). -/
example : ∃ (l : Label (opsWith allowed)) (c' : Config (opsWith allowed)),
    Step (⟨[]⟩ : Program (opsWith allowed))
      ⟨.exec (.prim none .output [.lit 7] ⟨1⟩), [], 0, []⟩ l c' ∧
    ∀ (l₂ : Label (opsWith allowed)) (c₂ : Config (opsWith allowed)),
      Step (⟨[]⟩ : Program (opsWith allowed))
        ⟨.exec (.prim none .output [.lit 7] ⟨1⟩), [], 0, []⟩ l₂ c₂ →
      l = l₂ ∧ c' = c₂ :=
  step_unique .output 7 0 ⟨1⟩
    ⟨.emit 7, finished 0, Step.prim (O := opsWith allowed)
      (P := (⟨[]⟩ : Program (opsWith allowed))) (p := .output) (vs := [7]) (st := 0)
      (a := (⟨some 7, .ok 0, 0⟩ : Answer Int Int Int)) rfl (by simp [allowed]) rfl⟩

/-- Planted (law `step_unique`, case `outputSeven`, subject `allowed`, mutant `permissiveAllowed`). -/
example : (∃ (l : Label (opsWith permissiveAllowed)) (c' : Config (opsWith permissiveAllowed)),
    Step (⟨[]⟩ : Program (opsWith permissiveAllowed))
      ⟨.exec (.prim none .output [.lit 7] ⟨1⟩), [], 0, []⟩ l c') ∧
    ¬ (∃ (l : Label (opsWith permissiveAllowed)) (c' : Config (opsWith permissiveAllowed)),
      Step (⟨[]⟩ : Program (opsWith permissiveAllowed))
        ⟨.exec (.prim none .output [.lit 7] ⟨1⟩), [], 0, []⟩ l c' ∧
      ∀ (l₂ : Label (opsWith permissiveAllowed)) (c₂ : Config (opsWith permissiveAllowed)),
        Step (⟨[]⟩ : Program (opsWith permissiveAllowed))
          ⟨.exec (.prim none .output [.lit 7] ⟨1⟩), [], 0, []⟩ l₂ c₂ →
        l = l₂ ∧ c' = c₂) := by
  have h₇ : Step (⟨[]⟩ : Program (opsWith permissiveAllowed))
      ⟨.exec (.prim none .output [.lit 7] ⟨1⟩), [], 0, []⟩ (.emit 7) ⟨.done, [], 0, []⟩ :=
    Step.prim (O := opsWith permissiveAllowed)
      (P := (⟨[]⟩ : Program (opsWith permissiveAllowed))) (p := .output) (vs := [7]) (st := 0)
      (a := (⟨some 7, .ok 0, 0⟩ : Answer Int Int Int)) rfl rfl rfl
  have h₈ : Step (⟨[]⟩ : Program (opsWith permissiveAllowed))
      ⟨.exec (.prim none .output [.lit 7] ⟨1⟩), [], 0, []⟩ (.emit 8) ⟨.done, [], 0, []⟩ :=
    Step.prim (O := opsWith permissiveAllowed)
      (P := (⟨[]⟩ : Program (opsWith permissiveAllowed))) (p := .output) (vs := [7]) (st := 0)
      (a := (⟨some 8, .ok 0, 0⟩ : Answer Int Int Int)) rfl rfl rfl
  refine ⟨⟨.emit 7, ⟨.done, [], 0, []⟩, h₇⟩, ?_⟩
  rintro ⟨l, c', _, huniq⟩
  have e₇ := (huniq _ _ h₇).1
  have e₈ := (huniq _ _ h₈).1
  have h78 : (Label.emit 7 : Label (opsWith permissiveAllowed)) = .emit 8 := e₇.symm.trans e₈
  injection h78 with hv
  exact (by decide : (7 : Int) ≠ 8) hv

/-- Instance (law `secure_ni`, case `maskedSecrets`). -/
example : DelimitedReleaseNI instrumentation program labelling (start 6 1) (start 5 2) :=
  secure_ni 6 5 1 2 ⟨rfl, trivial⟩ ⟨secureRun 6 1, secureRun 5 2, rfl, rfl⟩

/-- Planted (law `secure_ni`, case `maskedSecrets`, subject `ConcreteSpec.Core.Leak.Demo.start`, mutant `exposingStart`). -/
example : LowEquivalent labelling (exposingStart 6 1) (exposingStart 5 2) ∧
    (∃ (r₁ : Run instrumentation program (exposingStart 6 1))
       (r₂ : Run instrumentation program (exposingStart 5 2)),
       r₁.choices = r₂.choices ∧ r₁.releases = r₂.releases) ∧
    ¬ DelimitedReleaseNI instrumentation program labelling (exposingStart 6 1) (exposingStart 5 2) := by
  refine ⟨⟨rfl, trivial⟩, ⟨exposingRun 6 1, exposingRun 5 2, rfl, rfl⟩, ?_⟩
  intro h
  have hobs := h (exposingRun 6 1) (exposingRun 5 2) ⟨rfl, trivial⟩ rfl rfl
  exact (by decide : (exposingRun 6 1).publicObservations ≠ (exposingRun 5 2).publicObservations) hobs

/-- Instance (law `secure_ct`, case `maskedSecrets`). -/
example : ConstantTime instrumentation program labelling (start 6 1) (start 5 2) :=
  secure_ct 6 5 1 2 ⟨rfl, trivial⟩ ⟨secureRun 6 1, secureRun 5 2, rfl, rfl⟩

/-- Planted (law `secure_ct`, case `maskedSecrets`, subject `ConcreteSpec.Core.Leak.Demo.start`, mutant `exposingStart`). -/
example : LowEquivalent labelling (exposingStart 6 1) (exposingStart 5 2) ∧
    (∃ (r₁ : Run instrumentation program (exposingStart 6 1))
       (r₂ : Run instrumentation program (exposingStart 5 2)),
       r₁.choices = r₂.choices ∧ r₁.releases = r₂.releases) ∧
    ¬ ConstantTime instrumentation program labelling (exposingStart 6 1) (exposingStart 5 2) := by
  refine ⟨⟨rfl, trivial⟩, ⟨exposingRun 6 1, exposingRun 5 2, rfl, rfl⟩, ?_⟩
  intro h
  have hleak := h (exposingRun 6 1) (exposingRun 5 2) ⟨rfl, trivial⟩ rfl rfl
  simp [exposingRun] at hleak

/-- Instance (law `timing_ni`, case `timingStart`). -/
example : DelimitedReleaseNI instrumentation program labelling (timingStart 1) (timingStart 2) :=
  timing_ni 1 2 ⟨rfl, trivial⟩ ⟨timingRun 1, timingRun 2, rfl, rfl⟩

/-- Planted (law `timing_ni`, case `timingStart`, subject `timingStart`, mutant `leakyStart`). -/
example : LowEquivalent labelling (leakyStart 1) (leakyStart 2) ∧
    (∃ (r₁ : Run instrumentation program (leakyStart 1))
       (r₂ : Run instrumentation program (leakyStart 2)),
       r₁.choices = r₂.choices ∧ r₁.releases = r₂.releases) ∧
    ¬ DelimitedReleaseNI instrumentation program labelling (leakyStart 1) (leakyStart 2) := by
  refine ⟨leaky_low 1 2, ⟨leakyRun 1, leakyRun 2, rfl, rfl⟩, ?_⟩
  intro h
  have hobs := h (leakyRun 1) (leakyRun 2) ⟨rfl, trivial⟩ rfl rfl
  exact (by decide : (leakyRun 1).publicObservations ≠ (leakyRun 2).publicObservations) hobs

/-- Representation lemma. Concrete counterexample or mutant check for `timing_not_ct`; retained separately from Law evidence (approved: PENDING). -/
example : ¬ ConstantTime instrumentation program labelling (timingStart 1) (timingStart 2) :=
  timing_not_ct 1 2 ⟨rfl, trivial⟩ (by decide)

/-- Representation lemma. Concrete counterexample or mutant check for `timing_not_ct`; retained separately from Law evidence (approved: PENDING). -/
example : LowEquivalent labelling (ctStart 1) (ctStart 2) ∧ (1 : Int) ≠ 2 ∧
    ¬ (¬ ConstantTime instrumentation program labelling (ctStart 1) (ctStart 2)) := by
  refine ⟨⟨rfl, trivial⟩, by decide, ?_⟩
  intro h
  apply h
  intro r₁ r₂ _ _ _
  rw [(ct_run_unique 1 r₁).2.1, (ct_run_unique 2 r₂).2.1]

/-- Representation lemma. Concrete witness confirming that ctRun 1 and ctRun 2 inhabit all `ConstantTime` and `DelimitedReleaseNI` premises (`LowEquivalent`, equal choices, equal releases) for `ctStart` (approved: APPR-C7-agy-5d94c4b4d). -/
example : ctStart 1 ≠ ctStart 2 ∧
    LowEquivalent labelling (ctStart 1) (ctStart 2) ∧
    (ctRun 1).choices = (ctRun 2).choices ∧
    (ctRun 1).releases = (ctRun 2).releases := by
  refine ⟨?_, ⟨rfl, trivial⟩, rfl, rfl⟩
  intro h
  have hs := congrArg Config.store h
  exact (by decide : (1 : Int) ≠ 2) hs

/-- Representation lemma. Concrete counterexample or mutant check for `authorized_release_not_ni`; retained separately from Law evidence (approved: PENDING). -/
example : ¬ DelimitedReleaseNI instrumentation program labelling (leakyStart 1) (leakyStart 2) :=
  authorized_release_not_ni 1 2 (release_authorized 1) (release_authorized 2)
    (leaky_low 1 2) (by decide)

/-- Representation lemma. Concrete counterexample or mutant check for `authorized_release_not_ni`; retained separately from Law evidence (approved: PENDING). -/
example : policy.RunsAuthorized instrumentation program (ctStart 1) ∧
    policy.RunsAuthorized instrumentation program (ctStart 2) ∧
    LowEquivalent labelling (ctStart 1) (ctStart 2) ∧
    (1 : Int) ≠ 2 ∧
    ¬ (¬ DelimitedReleaseNI instrumentation program labelling (ctStart 1) (ctStart 2)) := by
  refine ⟨ct_release_authorized 1, ct_release_authorized 2, ⟨rfl, trivial⟩, by decide, ?_⟩
  intro h
  apply h
  intro r₁ r₂ _ _ _
  have h₁ := ct_run_unique 1 r₁
  have h₂ := ct_run_unique 2 r₂
  simp only [Run.publicObservations, h₁.1, h₂.1, h₁.2.2.2.2, h₂.2.2.2.2, Config.final?]

/-- Representation lemma. Concrete counterexample or mutant check for `authorized_release_not_ct`; retained separately from Law evidence (approved: PENDING). -/
example : ¬ ConstantTime instrumentation program labelling (leakyStart 1) (leakyStart 2) :=
  authorized_release_not_ct 1 2 (release_authorized 1) (release_authorized 2)
    (leaky_low 1 2) (by decide)

/-- Representation lemma. Concrete counterexample or mutant check for `authorized_release_not_ct`; retained separately from Law evidence (approved: PENDING). -/
example : policy.RunsAuthorized instrumentation program (ctStart 1) ∧
    policy.RunsAuthorized instrumentation program (ctStart 2) ∧
    LowEquivalent labelling (ctStart 1) (ctStart 2) ∧
    (1 : Int) ≠ 2 ∧
    ¬ (¬ ConstantTime instrumentation program labelling (ctStart 1) (ctStart 2)) := by
  refine ⟨ct_release_authorized 1, ct_release_authorized 2, ⟨rfl, trivial⟩, by decide, ?_⟩
  intro h
  apply h
  intro r₁ r₂ _ _ _
  rw [(ct_run_unique 1 r₁).2.1, (ct_run_unique 2 r₂).2.1]

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

/-- The actual optimization relation: the target is pure-instruction erasure of the source. -/
def optimized (source target : Program) : Prop := target = erasePure source

/-- A relation that certifies only ordinary observation preservation. -/
def observationRelated (source target : Program) : Prop :=
  observations source = observations target

/-- Law (subject `erasePure`).

The pure-instruction erasure pass preserves every ordered wipe effect. -/
theorem erasePure_preserves_wipes (s : Program) :
    wipes (erasePure s) = wipes s := by
  induction s with
  | nil => rfl
  | cons statement rest ih =>
    cases statement <;> simp [erasePure, wipes, ih]

/-- Helper (congruence). Transports the erasure law along the optimization relation
so that the public wipe obligation is explicitly discharged (approved: PENDING). -/
theorem optimized_preserves_wipes : WipePreservation optimized wipes wipes := by
  intro source target h
  subst target
  exact (erasePure_preserves_wipes source).symm

def firstWipe : WipeEvent := ⟨⟨0⟩, ⟨0⟩, 4⟩
def secondWipe : WipeEvent := ⟨⟨1⟩, ⟨0⟩, 2⟩

def source : Program :=
  [.pure 99, .wipe firstWipe, .output 7, .pure 100, .wipe secondWipe]

/-- Mutant pass that drops wipe statements while preserving ordinary `.output` observations. -/
def dropWipes : Program → Program
  | [] => []
  | .pure _ :: rest => dropWipes rest
  | .output v :: rest => .output v :: dropWipes rest
  | .wipe _ :: rest => dropWipes rest

/-- Mutant pass that inflates each wipe's byte count by 1 while preserving `.output` observations. -/
def changeWipes : Program → Program
  | [] => []
  | .pure _ :: rest => changeWipes rest
  | .output v :: rest => .output v :: changeWipes rest
  | .wipe w :: rest => .wipe { w with bytes := w.bytes + 1 } :: changeWipes rest

/-- Mutant pass that reverses the erased program, swapping wipe order while preserving single-output observations. -/
def reorderWipes (p : Program) : Program := (erasePure p).reverse

/-- Instance (law `erasePure_preserves_wipes`, case `source`). -/
example : wipes (erasePure source) = wipes source :=
  erasePure_preserves_wipes source

/-- Planted (law `erasePure_preserves_wipes`, case `source`, subject `erasePure`, mutant `dropWipes`). -/
example : ¬ (wipes (dropWipes source) = wipes source) := by decide

/-- Planted (law `erasePure_preserves_wipes`, case `source`, subject `erasePure`, mutant `changeWipes`). -/
example : ¬ (wipes (changeWipes source) = wipes source) := by decide

/-- Planted (law `erasePure_preserves_wipes`, case `source`, subject `erasePure`, mutant `reorderWipes`). -/
example : ¬ (wipes (reorderWipes source) = wipes source) := by decide

/-- Representation lemma. Concrete check that `observationRelated` alone fails `WipePreservation` because dropWipes source preserves `observations` while dropping wipes (approved: APPR-C7-agy-5d94c4b4d). -/
example : observationRelated source (dropWipes source) ∧
    ¬ WipePreservation observationRelated wipes wipes := by
  refine ⟨rfl, ?_⟩
  intro h
  exact (by decide : wipes source ≠ wipes (dropWipes source)) (h source (dropWipes source) rfl)

end WipeDemo

end ConcreteSpec.Core.Leak
