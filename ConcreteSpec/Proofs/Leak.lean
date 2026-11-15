import ConcreteSpec.Spec.Leak
import ConcreteSpec.Proofs.Exec
import ConcreteSpec.Spec.Basics
import ConcreteSpec.Spec.Exec
import ConcreteSpec.Spec.Ident
import ConcreteSpec.Spec.LeakData
import ConcreteSpec.Spec.Outcome

/-!
# The laws of leakage and release (`Spec/Leak.lean`, foundation contract C7)

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). See `ConcreteSpec/README.md`.

The execution examples use a small C1 operand language. `secure_ni` and `secure_ct` declassify a
masked combination of two secret components (`secret + hidden`) and output only that released sum,
so distinct secret states (`(6, 1)` vs `(5, 2)`) share the same release record (`7`) and require
the release-equality premise. `timing_ni` and `timing_not_ct` separate the claims with public
output but secret-dependent variable-time work. The leaky start configuration releases authorized
public zero but outputs its secret: `authorized_release_not_ni` and `authorized_release_not_ct`
refute deriving either claim from policy authorization alone.
-/

namespace ConcreteSpec.Core.Leak
open ConcreteSpec.Core Exec

/-- Helper (inversion). A missing C1 implementation result excludes every relational step. -/
private theorem no_step_of_none {O : Operands} {P : Program O} {c : Config O}
    (h : step P c = none) : ∀ l c', ¬ Step P c l c' := by
  intro l c' hs
  obtain ⟨answer, rule⟩ := (step_iff_rules P c l c').mp hs
  exact (DeterminismEvidence.no_dispatch_no_rule h).1 ⟨answer, l, c', rule⟩

/-- Helper (projection). Forgets C7 instrumentation traces from an `InstrumentedSteps` derivation to recover the underlying C1 `Steps` derivation.

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

/-- Law (subject `InstrumentedSteps`). Instrumentation neither adds nor removes C1 runs.
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7 erasure boundary).
The existential traces are outputs, never additional execution premises. -/
theorem instrumentation_erasure {O : Operands} (I : Instrumentation O) (P : Program O)
    (c c' : Config O) :
    (∃ obs leaks rs choices, InstrumentedSteps I P c obs leaks rs choices c') ↔
      ∃ events, Steps P c events c' := by
  constructor
  · rintro ⟨_, _, _, _, h⟩
    exact h.toSteps
  · rintro ⟨_, h⟩
    induction h with
    | refl c => exact ⟨[], [], [], [], .refl c⟩
    | step h rest ih =>
      obtain ⟨obs, leaks, rs, choices, hs⟩ := ih
      exact ⟨_, _, _, _, .step h hs⟩

/-- Law (subject `ReleasePolicy.Authorized`).
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7),
docs/decisions.md#decided-project-infoflow-2026-11-06.

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

/-- Law: reason and call-path facts do not strengthen the ordered release premise.
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7),
docs/decisions.md#decided-project-infoflow-2026-11-06. -/
theorem release_metadata_irrelevant {Ty Val : Type} (rs : List (ReleaseFact Ty Val))
    (reason : String) (path : List FnId) :
    ReleaseFact.semanticTrace (rs.map fun r => { r with reason := reason, path := path }) =
      ReleaseFact.semanticTrace rs := by
  simp [ReleaseFact.semanticTrace, releaseTrace, List.map_map]

/-- Helper (projection). Run-connected facts use precisely the run's release premise. -/
theorem RunFacts.trace_eq {O : Operands} {I : Instrumentation O} {P : Program O}
    {c : Config O} {r : Run I P c} (facts : RunFacts r.releases) :
    ReleaseFact.semanticTrace facts.entries = releaseTrace r.releases := by
  simp only [ReleaseFact.semanticTrace, facts.erases]

/-- A populated facts record, whose reason and call path never enter the semantic release. -/
def metadataFact : ReleaseFact String Nat := ⟨typedRelease "public" 7, "example", []⟩

/-- Instance: a populated release retains its typed site and value after changing both metadata fields. -/
theorem release_metadata_instance :
    ReleaseFact.semanticTrace [{ metadataFact with reason := "new", path := [Exec.Demo.logF] }] =
      ReleaseFact.semanticTrace [metadataFact] :=
  release_metadata_irrelevant [metadataFact] "new" [Exec.Demo.logF]

/-- Planted definition: incorrectly suppress releases with an empty reason. -/
def reasonSensitiveTrace {Ty Val : Type} (rs : List (ReleaseFact Ty Val)) : List (ReleaseSite Ty × Val) :=
  (rs.filter fun r => !r.reason.isEmpty).map fun r => (r.release.site, r.release.value)

/-- Planted (law `release_metadata_irrelevant`, case `metadataReason`, mutant `reasonSensitiveTrace`).
changing only reason metadata now changes the release comparison. -/
theorem release_metadata_planted :
    ¬ (reasonSensitiveTrace [{ metadataFact with reason := "", path := [Exec.Demo.logF] }] =
      reasonSensitiveTrace [metadataFact]) := by decide

namespace Demo

/-- Helper (computation). Discharges primitive-answer determinism for the three-operation `Demo.allowed` oracle by case analysis on the request. -/
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

/-- Helper (inversion). Determinism of a single C1 step under `Demo.ops`, used to unroll complete runs in the trace inversion helpers and `step_unique`. -/
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

/-- Representation lemma. Function-backed step uniqueness for the toy oracle.
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7),
docs/decisions.md#decided-project-infoflow-2026-11-06.

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

/-- Helper: a C1 final configuration has no machine step. -/
private theorem final_no_step {P : Program ops} {c : Config ops} {e : Final Int}
    (hf : c.final? = some e) : step P c = none := by
  obtain ⟨ctrl, locals, store, frames⟩ := c
  cases ctrl with
  | exiting x => cases x <;> cases frames <;> simp_all [Config.final?, step, stepExit]
  | _ => cases frames <;> simp_all [Config.final?, step]

/-- Helper: deterministic complete runs have identical traces and endpoint, for any observer.
Induction follows the supplied C1 derivation; neither run may stop before a final outcome. -/
theorem complete_unique {I : Instrumentation ops} {P : Program ops} {c : Config ops}
    (a b : Run I P c) : a.observations = b.observations ∧ a.leakage = b.leakage ∧
      a.releases = b.releases ∧ a.choices = b.choices ∧ a.final = b.final := by
  obtain ⟨af, ae, ah, ao, al, ar, ac, ast⟩ := a
  obtain ⟨bf, be, bh, bo, bl, br, bc, bst⟩ := b
  induction ast generalizing bo bl br bc bf with
  | refl c =>
    cases bst with
    | refl => exact ⟨rfl, rfl, rfl, rfl, rfl⟩
    | step h _ => exact (no_step_of_none (P := P) (c := c)
        (final_no_step ah) _ _ h).elim
  | step h rest ih =>
    cases bst with
    | refl => exact (no_step_of_none (P := P)
        (final_no_step bh) _ _ h).elim
    | step h' rest' =>
      obtain ⟨rfl, rfl⟩ := step_det h h'
      obtain ⟨rfl, rfl, rfl, rfl, rfl⟩ := ih ah _ bh _ _ _ _ rest'
      exact ⟨rfl, rfl, rfl, rfl, rfl⟩

/-- Helper: total value of a toy expression, shared by the execution witnesses. -/
def value (locals : AList Var Int) (secret : Int) : Expr → Int
  | .lit n => n
  | .secret => secret
  | .masked => secret + hiddenOffset locals

/-- Helper: one four-step release/output execution, with arbitrary instrumentation.
All concrete security and observer controls reuse these same C1 steps. -/
def bodyRun (I : Instrumentation ops) (released result : Expr) (secret : Int)
    (locals : AList Var Int) : Run I program ⟨.exec (body released result), locals, secret, []⟩ := by
  let c₀ : Config ops := ⟨.exec (body released result), locals, secret, []⟩
  let c₁ := afterSeq released result secret locals
  let c₂ := afterRelease result secret locals
  let c₃ := beforeOutput result secret locals
  let c₄ := finished secret locals
  let l : Label ops := .emit (value locals secret result)
  refine ⟨c₄, .normal, rfl,
    I.observeStep c₀ .silent c₁ ++ I.observeStep c₁ .silent c₂ ++ I.observeStep c₂ .silent c₃ ++ I.observeStep c₃ l c₄,
    I.leakage c₀ .silent c₁ ++ I.leakage c₁ .silent c₂ ++ I.leakage c₂ .silent c₃ ++ I.leakage c₃ l c₄,
    I.releases c₀ .silent c₁ ++ I.releases c₁ .silent c₂ ++ I.releases c₂ .silent c₃ ++ I.releases c₃ l c₄,
    I.choices c₀ .silent c₁ ++ I.choices c₁ .silent c₂ ++ I.choices c₂ .silent c₃ ++ I.choices c₃ l c₄, ?_⟩
  simp only [List.append_assoc]
  apply InstrumentedSteps.step (c := c₀) (c' := c₁) (.silent rfl)
  apply InstrumentedSteps.step (c := c₁) (c' := c₂) (Step.prim (O := ops) (P := program) (c := c₁) (c' := c₂) (p := .release) (site := ⟨0⟩)
    (vs := [value locals secret released]) (st := secret)
    (a := (⟨none, .ok 0, secret⟩ : Answer Int Int Int))
    (by dsimp [c₁, afterSeq, step, evalArgs, ops, opsWith]; cases released <;> rfl) (by simp [allowed]) rfl)
  apply InstrumentedSteps.step (c := c₂) (c' := c₃) (.silent rfl)
  simpa only [List.append_nil, Answer.label, l] using InstrumentedSteps.step (I := I)
    (Step.prim (P := program) (c := c₃) (c' := c₄) (p := .output) (site := ⟨1⟩)
      (vs := [value locals secret result]) (st := secret)
      (a := (⟨some (value locals secret result), .ok 0, secret⟩ : Answer Int Int Int))
      (by dsimp [c₃, beforeOutput, step, evalArgs, ops, opsWith]; cases result <;> rfl) (by simp [allowed]) rfl)
    (InstrumentedSteps.refl (I := I) (P := program) c₄)

/-- One complete secure execution of the shared release/output body. -/
def secureRun (secret : Int) (hidden : Nat := 0) : Run instrumentation program (start secret hidden) :=
  bodyRun instrumentation .masked .masked secret (hiddenLocals hidden)

/-- Helper: every secure run has exactly the canonical traces and final state. -/
theorem secure_run_unique (secret : Int) (hidden : Nat := 0)
    (r : Run instrumentation program (start secret hidden)) :
    r.observations = [.output (secret + Int.ofNat hidden)] ∧
      r.leakage = [.variableTime ⟨0⟩ [secret + Int.ofNat hidden],
        .variableTime ⟨1⟩ [secret + Int.ofNat hidden]] ∧
      r.releases = [releaseValue (secret + Int.ofNat hidden)] ∧
      r.choices = [(), (), (), ()] ∧
      r.final = ⟨.done, hiddenLocals hidden, secret, []⟩ := by
  exact complete_unique r (secureRun secret hidden)

/-- Planted execution: only output reads the unmasked secret. -/
def exposingRun (secret : Int) (hidden : Nat := 0) : Run instrumentation program (exposingStart secret hidden) :=
  bodyRun instrumentation .masked .secret secret (hiddenLocals hidden)

/-- Law (subject `start`).
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7),
docs/decisions.md#decided-project-infoflow-2026-11-06.

**Delimited-release NI, proved for the secure example.** Equality of the ordered release records
equates s₁ + Int.ofNat hidden₁ and s₂ + Int.ofNat hidden₂ across distinct secret pairs,
yielding equal public observations for every pair of complete runs. -/
theorem secure_ni (s₁ s₂ : Int) (hidden₁ hidden₂ : Nat)
    (_hlow : LowEquivalent labelling (start s₁ hidden₁) (start s₂ hidden₂))
    (hrel : ∃ (r₁ : Run instrumentation program (start s₁ hidden₁))
              (r₂ : Run instrumentation program (start s₂ hidden₂)),
              r₁.choices = r₂.choices ∧ releaseTrace r₁.releases = releaseTrace r₂.releases) :
    DelimitedReleaseNI instrumentation program labelling (start s₁ hidden₁) (start s₂ hidden₂) := by
  obtain ⟨r₁₀, r₂₀, _, hreleases⟩ := hrel
  have h₁₀ := secure_run_unique s₁ hidden₁ r₁₀
  have h₂₀ := secure_run_unique s₂ hidden₂ r₂₀
  have hsum : s₁ + Int.ofNat hidden₁ = s₂ + Int.ofNat hidden₂ := by
    rw [h₁₀.2.2.1, h₂₀.2.2.1] at hreleases
    simpa [releaseTrace, releaseValue] using hreleases
  intro r₁ r₂ _ _ _
  have h₁ := secure_run_unique s₁ hidden₁ r₁
  have h₂ := secure_run_unique s₂ hidden₂ r₂
  constructor
  · simp only [Run.publicObservations, h₁.1, h₂.1, h₁.2.2.2.2, h₂.2.2.2.2, hsum, Config.final?]
  · simp [h₁.2.1, h₂.2.1, publicLengths]

/-- Law (subject `start`).
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7),
docs/decisions.md#decided-project-infoflow-2026-11-06.

**Constant time, proved separately for the secure example.** Equality of the ordered release
records equates the masked sums across distinct secret pairs, making the leakage traces equal. -/
theorem secure_ct (s₁ s₂ : Int) (hidden₁ hidden₂ : Nat)
    (_hlow : LowEquivalent labelling (start s₁ hidden₁) (start s₂ hidden₂))
    (hrel : ∃ (r₁ : Run instrumentation program (start s₁ hidden₁))
              (r₂ : Run instrumentation program (start s₂ hidden₂)),
              r₁.choices = r₂.choices ∧ releaseTrace r₁.releases = releaseTrace r₂.releases) :
    ConstantTime instrumentation program labelling (start s₁ hidden₁) (start s₂ hidden₂) := by
  obtain ⟨r₁₀, r₂₀, _, hreleases⟩ := hrel
  have h₁₀ := secure_run_unique s₁ hidden₁ r₁₀
  have h₂₀ := secure_run_unique s₂ hidden₂ r₂₀
  have hsum : s₁ + Int.ofNat hidden₁ = s₂ + Int.ofNat hidden₂ := by
    rw [h₁₀.2.2.1, h₂₀.2.2.1] at hreleases
    simpa [releaseTrace, releaseValue] using hreleases
  intro r₁ r₂ _ _ _
  have h₁ := secure_run_unique s₁ hidden₁ r₁
  have h₂ := secure_run_unique s₂ hidden₂ r₂
  rw [h₁.2.1, h₂.2.1, hsum]

/-- One complete execution releasing public zero and outputting the secret. -/
def leakyRun (secret : Int) : Run instrumentation program (leakyStart secret) :=
  bodyRun instrumentation (.lit 0) .secret secret []

/-- Helper: deterministic execution fixes the entire release trace. -/
theorem leaky_run_releases (secret : Int) (r : Run instrumentation program (leakyStart secret)) :
    r.releases = [releaseZero] := (complete_unique r (leakyRun secret)).2.2.1

/-- One complete execution releasing public zero and outputting public seven. -/
def ctRun (secret : Int) : Run instrumentation program (ctStart secret) :=
  bodyRun instrumentation (.lit 0) (.lit 7) secret []

/-- Helper: every constant-time run has the canonical traces and final state. -/
theorem ct_run_unique (secret : Int) (r : Run instrumentation program (ctStart secret)) :
    r.observations = [.output 7] ∧
      r.leakage = [.variableTime ⟨0⟩ [0], .variableTime ⟨1⟩ [7]] ∧
      r.releases = [releaseZero] ∧ r.choices = [(), (), (), ()] ∧
      r.final = ⟨.done, [], secret, []⟩ := complete_unique r (ctRun secret)

/-- Representation lemma. Concrete projection check showing secureRun 1 maps via `InstrumentedSteps.toSteps` to an underlying C1 `Steps` derivation.

The concrete instrumented run projects to C1, not to a second semantics. -/
example : ∃ events, Steps program (start 1) events (finished 1 (hiddenLocals 0)) :=
  (secureRun 1).steps.toSteps

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

/-- Helper: every timing run has the canonical traces by complete-run determinism. -/
theorem timing_run_unique (secret : Int)
    (r : Run instrumentation program (timingStart secret)) :
    r.observations = [.output 7] ∧
      r.leakage = [.variableTime ⟨2⟩ [secret], .variableTime ⟨0⟩ [0], .variableTime ⟨1⟩ [7]] ∧
      r.releases = [releaseZero] ∧ r.choices = [(), (), (), (), (), (), ()] ∧
      r.final = ⟨.done, [], secret, []⟩ := complete_unique r (timingRun secret)

/-- Planted definition: change only the output operand, retaining secret-dependent work. -/
def leakyTimingStart (secret : Int) : Config ops :=
  ⟨.exec (.seq (.prim none .work [.secret] ⟨2⟩) (body (.lit 0) .secret)), [], secret, []⟩
/-- Intermediate configuration of the same-shape timing mutant. -/
def leakyTimingAfterSeq (secret : Int) : Config ops :=
  ⟨.exec (.prim none .work [.secret] ⟨2⟩), [], secret, [.seq (body (.lit 0) .secret)]⟩
/-- Intermediate configuration of the same-shape timing mutant. -/
def leakyTimingAfterWork (secret : Int) : Config ops :=
  ⟨.done, [], secret, [.seq (body (.lit 0) .secret)]⟩
/-- Intermediate configuration of the same-shape timing mutant. -/
def leakyTimingBeforeBody (secret : Int) : Config ops :=
  ⟨.exec (body (.lit 0) .secret), [], secret, []⟩
/-- A complete execution of the mutant with the same seven steps, choices and releases. -/
def leakyTimingRun (secret : Int) : Run instrumentation program (leakyTimingStart secret) where
  final := finished secret
  ending := .normal
  finalEnding := rfl
  observations := [.output secret]
  leakage := [.variableTime ⟨2⟩ [secret], .variableTime ⟨0⟩ [0], .variableTime ⟨1⟩ [secret]]
  releases := [releaseZero]
  choices := [(), (), (), (), (), (), ()]
  steps :=
    InstrumentedSteps.step (I := instrumentation)
      (Step.silent (P := program) (c := leakyTimingStart secret) (c' := leakyTimingAfterSeq secret) rfl) <|
    InstrumentedSteps.step (I := instrumentation)
      (Step.prim (O := ops) (P := program) (c := leakyTimingAfterSeq secret)
        (c' := leakyTimingAfterWork secret) (p := .work) (vs := [secret]) (st := secret)
        (a := (⟨none, .ok 0, secret⟩ : Answer Int Int Int)) rfl (by simp [allowed]) rfl) <|
    InstrumentedSteps.step (I := instrumentation)
      (Step.silent (P := program) (c := leakyTimingAfterWork secret)
        (c' := leakyTimingBeforeBody secret) rfl) <|
    InstrumentedSteps.step (I := instrumentation)
      (Step.silent (P := program) (c := leakyTimingBeforeBody secret)
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

/-- Law (subject `timingStart`).
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7),
docs/decisions.md#decided-project-infoflow-2026-11-06.

Every pair of timing runs from low-equivalent entries with matched choices and releases emits the
same public observations ([.output 7, .final .normal]). -/
theorem timing_ni (s₁ s₂ : Int)
    (_hlow : LowEquivalent labelling (timingStart s₁) (timingStart s₂))
    (_hrel : ∃ (r₁ : Run instrumentation program (timingStart s₁))
               (r₂ : Run instrumentation program (timingStart s₂)),
               r₁.choices = r₂.choices ∧ releaseTrace r₁.releases = releaseTrace r₂.releases) :
    DelimitedReleaseNI instrumentation program labelling (timingStart s₁) (timingStart s₂) := by
  intro r₁ r₂ _ _ _
  have h₁ := timing_run_unique s₁ r₁
  have h₂ := timing_run_unique s₂ r₂
  constructor
  · simp only [Run.publicObservations, h₁.1, h₂.1, h₁.2.2.2.2, h₂.2.2.2.2, Config.final?]
  · simp [h₁.2.1, h₂.2.1, publicLengths]

/-- Law: C7's distinct security obligations cannot be discharged by a weaker observation.
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7),
docs/decisions.md#decided-project-infoflow-2026-11-06. This theorem concerns the explicit countermodel below;
it does not claim that every program has this behavior.

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

/-- Representation lemma. Concrete order-sensitivity check showing secureRun 1's leakage list is not equal to its reversal.

Planted trace mutations: reversed leakage and a missing final observation are detectable. -/
example : (secureRun 1).leakage ≠ [.variableTime ⟨1⟩ [1], .variableTime ⟨0⟩ [1]] := by
  intro h
  change [Leakage.variableTime ⟨0⟩ [1], .variableTime ⟨1⟩ [1]] =
    [.variableTime ⟨1⟩ [1], .variableTime ⟨0⟩ [1]] at h
  cases h

/-- Representation lemma. Concrete finality check showing `Run.publicObservations` includes .final .normal beyond the step output. -/
example : (secureRun 1).publicObservations ≠ [.output 1] := by decide

/-- Representation lemma. Concrete non-finality check showing the intermediate `afterRelease` state has final? = none.

A nonfinal execution prefix is not a complete run. -/
example : ¬ ∃ ending, (afterRelease .secret 1).final? = some ending := by
  simp [afterRelease, Config.final?]

/-- A configuration with one secret and one public local. -/
def mixedConfig (secret publicValue : Int) : Config ops :=
  ⟨.done, [(⟨0⟩, secret), (⟨1⟩, publicValue)], 0, []⟩

/-- Instance. Concrete `LowEquivalent` acceptance check when only negative (secret) local bindings differ.

Secret local changes are invisible; a planted public local change rejects low-equivalence. -/
example : LowEquivalent labelling (mixedConfig (-1) 7) (mixedConfig (-2) 7) := by
  simp [LowEquivalent, publicLocals, labelling, mixedConfig]

/-- Instance. Concrete `LowEquivalent` refusal check when non-negative (public) local bindings differ. -/
example : ¬ LowEquivalent labelling (mixedConfig (-1) 7) (mixedConfig (-2) 8) := by
  simp [LowEquivalent, publicLocals, labelling, mixedConfig]

/-- A store-sensitive labelling supplies the corresponding public-store negative control. -/
abbrev storeSensitiveLabelling : Labelling ops where
  PublicValue := Int
  project := fun _ => none
  lowStore := fun s₁ s₂ => s₁ = s₂

/-- Instance. Concrete `LowEquivalent` refusal check under `storeSensitiveLabelling` when store states differ. -/
example : ¬ LowEquivalent storeSensitiveLabelling (leakyStart 1) (leakyStart 2) := by
  simp [LowEquivalent, storeSensitiveLabelling, leakyStart]

/-- Helper (computation). Evaluates `publicLocals` on `Demo.leakyStart` to show secret store states are `LowEquivalent` under `Demo.labelling`. -/
theorem leaky_low (s₁ s₂ : Int) : LowEquivalent labelling (leakyStart s₁) (leakyStart s₂) := by
  simp [LowEquivalent, publicLocals, labelling, leakyStart]

/-- Helper (computation). Checks that every run from `Demo.leakyStart` emits only releases permitted by `Demo.policy`. -/
theorem release_authorized (secret : Int) :
    policy.RunsAuthorized instrumentation program (leakyStart secret) := by
  intro r
  rw [leaky_run_releases secret r]
  simp [ReleasePolicy.Authorized, ReleasePolicy.Permits, releaseZero, releaseValue]

/-- Helper (computation). Checks that every run from `Demo.ctStart` emits only releases permitted by `Demo.policy`. -/
theorem ct_release_authorized (secret : Int) :
    policy.RunsAuthorized instrumentation program (ctStart secret) := by
  intro r
  rw [(ct_run_unique secret r).2.2.1]
  simp [ReleasePolicy.Authorized, ReleasePolicy.Permits, releaseZero, releaseValue]

/-- Law: C7's distinct security obligations cannot be discharged by a weaker observation.
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7),
docs/decisions.md#decided-project-infoflow-2026-11-06. This theorem concerns the explicit countermodel below;
it does not claim that every program has this behavior.

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
  have hobs := (hni (leakyRun s₁) (leakyRun s₂) hlow rfl rfl).1
  simp [Run.publicObservations, leakyRun, finished, Config.final?, bodyRun] at hobs
  exact hs hobs

/-- Law: C7's distinct security obligations cannot be discharged by a weaker observation.
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7),
docs/decisions.md#decided-project-infoflow-2026-11-06. This theorem concerns the explicit countermodel below;
it does not claim that every program has this behavior.

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
  change [Leakage.variableTime ⟨0⟩ [0], .variableTime ⟨1⟩ [s₁]] =
    [.variableTime ⟨0⟩ [0], .variableTime ⟨1⟩ [s₂]] at hleak
  simp only [List.cons.injEq, Leakage.variableTime.injEq] at hleak
  have hleak : s₁ = s₂ := by simpa using hleak
  exact hs hleak

end Demo

namespace Audit
/-- Planted definition: omit only site permission, retaining the trace quantifiers. -/
def ignoresPolicyAuthorized {Ty Val : Type} (_ : ReleasePolicy Ty)
    (rs : List (Release Ty Val)) : Prop :=
  ∀ r ∈ rs, (True : Prop)

/-- Instance (law `forbidsRelease_not_authorized`, case `emptyPolicy`). -/
example : ¬ (ReleasePolicy.mk ([] : List (ReleaseSite Unit))).Authorized [Demo.releaseZero] :=
  forbidsRelease_not_authorized (ReleasePolicy.mk []) Demo.releaseZero [] rfl

/-- Planted (law `forbidsRelease_not_authorized`, case `emptyPolicy`, subject `ReleasePolicy.Authorized`, mutant `ignoresPolicyAuthorized`). -/
example : (ReleasePolicy.mk ([] : List (ReleaseSite Unit))).ForbidsRelease ∧
    ¬ (¬ ignoresPolicyAuthorized (ReleasePolicy.mk ([] : List (ReleaseSite Unit)))
      [Demo.releaseZero]) :=
  ⟨rfl, fun h => h (fun _ _ => True.intro)⟩

open Demo

/-- Representation lemma: a concrete control for a function-backed identity, not Law evidence. -/
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

/-- Representation lemma: a concrete control for a function-backed identity, not Law evidence. -/
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
       r₁.choices = r₂.choices ∧ releaseTrace r₁.releases = releaseTrace r₂.releases) ∧
    ¬ DelimitedReleaseNI instrumentation program labelling (exposingStart 6 1) (exposingStart 5 2) := by
  refine ⟨⟨rfl, trivial⟩, ⟨exposingRun 6 1, exposingRun 5 2, rfl, rfl⟩, ?_⟩
  intro h
  have hobs := h (exposingRun 6 1) (exposingRun 5 2) ⟨rfl, trivial⟩ rfl rfl
  exact (by decide : (exposingRun 6 1).publicObservations ≠ (exposingRun 5 2).publicObservations) hobs.1

/-- Instance (law `secure_ct`, case `maskedSecrets`). -/
example : ConstantTime instrumentation program labelling (start 6 1) (start 5 2) :=
  secure_ct 6 5 1 2 ⟨rfl, trivial⟩ ⟨secureRun 6 1, secureRun 5 2, rfl, rfl⟩

/-- Planted (law `secure_ct`, case `maskedSecrets`, subject `ConcreteSpec.Core.Leak.Demo.start`, mutant `exposingStart`). -/
example : LowEquivalent labelling (exposingStart 6 1) (exposingStart 5 2) ∧
    (∃ (r₁ : Run instrumentation program (exposingStart 6 1))
       (r₂ : Run instrumentation program (exposingStart 5 2)),
       r₁.choices = r₂.choices ∧ releaseTrace r₁.releases = releaseTrace r₂.releases) ∧
    ¬ ConstantTime instrumentation program labelling (exposingStart 6 1) (exposingStart 5 2) := by
  refine ⟨⟨rfl, trivial⟩, ⟨exposingRun 6 1, exposingRun 5 2, rfl, rfl⟩, ?_⟩
  intro h
  have hleak := h (exposingRun 6 1) (exposingRun 5 2) ⟨rfl, trivial⟩ rfl rfl
  change [Leakage.variableTime ⟨0⟩ [7], .variableTime ⟨1⟩ [6]] =
    [.variableTime ⟨0⟩ [7], .variableTime ⟨1⟩ [5]] at hleak
  cases hleak

/-- Instance (law `timing_ni`, case `timingStart`). -/
example : DelimitedReleaseNI instrumentation program labelling (timingStart 1) (timingStart 2) :=
  timing_ni 1 2 ⟨rfl, trivial⟩ ⟨timingRun 1, timingRun 2, rfl, rfl⟩

/-- Planted (law `timing_ni`, case `timingStart`, subject `timingStart`, mutant `leakyTimingStart`). -/
example : LowEquivalent labelling (leakyTimingStart 1) (leakyTimingStart 2) ∧
    (∃ (r₁ : Run instrumentation program (leakyTimingStart 1))
       (r₂ : Run instrumentation program (leakyTimingStart 2)),
       r₁.choices = r₂.choices ∧ releaseTrace r₁.releases = releaseTrace r₂.releases) ∧
    ¬ DelimitedReleaseNI instrumentation program labelling (leakyTimingStart 1) (leakyTimingStart 2) := by
  refine ⟨⟨rfl, trivial⟩, ⟨leakyTimingRun 1, leakyTimingRun 2, rfl, rfl⟩, ?_⟩
  intro h
  have hobs := h (leakyTimingRun 1) (leakyTimingRun 2) ⟨rfl, trivial⟩ rfl rfl
  exact (by decide : (leakyTimingRun 1).publicObservations ≠ (leakyTimingRun 2).publicObservations) hobs.1

/-- Instance: secret-dependent work refutes constant time even with public output. -/
example : ¬ ConstantTime instrumentation program labelling (timingStart 1) (timingStart 2) :=
  timing_not_ct 1 2 ⟨rfl, trivial⟩ (by decide)

/-- Representation lemma. Concrete witness confirming that ctRun 1 and ctRun 2 inhabit all `ConstantTime` and `DelimitedReleaseNI` premises (`LowEquivalent`, equal choices, equal releases) for `ctStart`. -/
example : ctStart 1 ≠ ctStart 2 ∧
    LowEquivalent labelling (ctStart 1) (ctStart 2) ∧
    (ctRun 1).choices = (ctRun 2).choices ∧
    (ctRun 1).releases = (ctRun 2).releases := by
  refine ⟨?_, ⟨rfl, trivial⟩, rfl, rfl⟩
  intro h
  have hs := congrArg Config.store h
  exact (by decide : (1 : Int) ≠ 2) hs

/-- Instance: authorized release cannot excuse an unreleased public output. -/
example : ¬ DelimitedReleaseNI instrumentation program labelling (leakyStart 1) (leakyStart 2) :=
  authorized_release_not_ni 1 2 (release_authorized 1) (release_authorized 2)
    (leaky_low 1 2) (by decide)

/-- Instance: authorized release cannot excuse unreleased timing leakage. -/
example : ¬ ConstantTime instrumentation program labelling (leakyStart 1) (leakyStart 2) :=
  authorized_release_not_ct 1 2 (release_authorized 1) (release_authorized 2)
    (leaky_low 1 2) (by decide)

/-- Planted definition: omit only public-output equality from NI. The complete-run domain,
all premises and public-length equality remain unchanged. -/
def blindNI {O : Operands} (I : Instrumentation O) (P : Program O)
    (L : Labelling O) (c₁ c₂ : Config O) : Prop :=
  ∀ (r₁ : Run I P c₁) (r₂ : Run I P c₂),
    LowEquivalent L c₁ c₂ → r₁.choices = r₂.choices →
      releaseTrace r₁.releases = releaseTrace r₂.releases →
      True ∧ publicLengths r₁.leakage = publicLengths r₂.leakage

/-- Planted definition: CT's single leakage-equality obligation is omitted; every premise remains. -/
def blindCT {O : Operands} (I : Instrumentation O) (P : Program O)
    (L : Labelling O) (c₁ c₂ : Config O) : Prop :=
  ∀ (r₁ : Run I P c₁) (r₂ : Run I P c₂),
    LowEquivalent L c₁ c₂ → r₁.choices = r₂.choices →
      releaseTrace r₁.releases = releaseTrace r₂.releases → True

/-- Planted (law `timing_not_ct`, case `timingStart`, mutant `blindCT`).
the added leakage permission invalidates timing_not_ct with its premises inhabited. -/
theorem timing_absence_planted :
    LowEquivalent labelling (timingStart 1) (timingStart 2) ∧ (1 : Int) ≠ 2 ∧
    ¬ (¬ blindCT instrumentation program labelling (timingStart 1) (timingStart 2)) := by
  exact ⟨⟨rfl, trivial⟩, by decide, fun h => h (fun _ _ _ _ _ => True.intro)⟩

/-- Planted (law `authorized_release_not_ni`, case `leakyStart`, mutant `blindNI`).
the added output permission invalidates authorized_release_not_ni on the same runs. -/
theorem authorization_ni_planted :
    policy.RunsAuthorized instrumentation program (leakyStart 1) ∧
    policy.RunsAuthorized instrumentation program (leakyStart 2) ∧
    LowEquivalent labelling (leakyStart 1) (leakyStart 2) ∧ (1 : Int) ≠ 2 ∧
    ¬ (¬ blindNI instrumentation program labelling (leakyStart 1) (leakyStart 2)) := by
  refine ⟨release_authorized 1, release_authorized 2, leaky_low 1 2, by decide, ?_⟩
  intro h
  apply h
  intro r₁ r₂ _ _ _
  have h₁ := (complete_unique r₁ (leakyRun 1)).2.1
  have h₂ := (complete_unique r₂ (leakyRun 2)).2.1
  exact ⟨True.intro, by rw [h₁, h₂]; decide⟩

/-- Planted (law `authorized_release_not_ct`, case `leakyStart`, mutant `blindCT`).
the added leakage permission invalidates authorized_release_not_ct on the same runs. -/
theorem authorization_ct_planted :
    policy.RunsAuthorized instrumentation program (leakyStart 1) ∧
    policy.RunsAuthorized instrumentation program (leakyStart 2) ∧
    LowEquivalent labelling (leakyStart 1) (leakyStart 2) ∧ (1 : Int) ≠ 2 ∧
    ¬ (¬ blindCT instrumentation program labelling (leakyStart 1) (leakyStart 2)) := by
  exact ⟨release_authorized 1, release_authorized 2, leaky_low 1 2, by decide,
    fun h => h (fun _ _ _ _ _ => True.intro)⟩

/-- Planted observer. The secure program is unchanged; final shared secret becomes public. -/
abbrev secretSeeing : Instrumentation Demo.ops :=
  { instrumentation with observeFinal := fun c => some (.output c.store) }

/-- Helper (transport). A final observer cannot affect the instrumented step derivation. -/
private theorem secretSeeingSteps {c c' : Config Demo.ops} {obs leaks rs choices}
    (h : InstrumentedSteps instrumentation program c obs leaks rs choices c') :
    InstrumentedSteps secretSeeing program c obs leaks rs choices c' := by
  induction h with
  | refl c => exact .refl c
  | step h _ ih => exact .step h ih

/-- Planted execution. All transitions and releases are the unchanged secure execution. -/
def secretSeeingRun (s : Int) (h : Nat) : Run secretSeeing program (start s h) where
  final := (secureRun s h).final
  ending := .normal
  finalEnding := rfl
  observations := (secureRun s h).observations
  leakage := (secureRun s h).leakage
  releases := (secureRun s h).releases
  choices := (secureRun s h).choices
  steps := secretSeeingSteps (secureRun s h).steps

/-- Planted (law `secure_ni`, fault `secretSeeingObserver`).
Same program and entries as the positive instance; only the observer is changed. -/
example : LowEquivalent labelling (start 6 1) (start 5 2) ∧
    (secretSeeingRun 6 1).choices = (secretSeeingRun 5 2).choices ∧
    releaseTrace (secretSeeingRun 6 1).releases = releaseTrace (secretSeeingRun 5 2).releases ∧
    ¬ DelimitedReleaseNI secretSeeing program labelling (start 6 1) (start 5 2) := by
  refine ⟨⟨rfl, trivial⟩, rfl, rfl, ?_⟩
  intro h
  have bad := (h (secretSeeingRun 6 1) (secretSeeingRun 5 2) ⟨rfl, trivial⟩ rfl rfl).1
  exact (by decide : (secretSeeingRun 6 1).publicObservations ≠
    (secretSeeingRun 5 2).publicObservations) bad

/-- Planted observer. Secret state is misreported as public container length. -/
abbrev metadataSeeing : Instrumentation Demo.ops :=
  { instrumentation with leakage := fun c _ _ => [.publicLength ⟨9⟩ c.store.natAbs] }

/-- Planted execution. Only public-length metadata differs from the secure observer. -/
def metadataSeeingRun (secret : Int) (hidden : Nat := 0) : Run metadataSeeing program (start secret hidden) :=
  bodyRun metadataSeeing .masked .masked secret (hiddenLocals hidden)

/-- Planted (law `secure_ni`, fault `secretPublicLength`).
NI rejects secret-dependent public metadata even when ordinary outputs still match. -/
example : LowEquivalent labelling (start 6 1) (start 5 2) ∧
    (metadataSeeingRun 6 1).choices = (metadataSeeingRun 5 2).choices ∧
    releaseTrace (metadataSeeingRun 6 1).releases = releaseTrace (metadataSeeingRun 5 2).releases ∧
    ¬ DelimitedReleaseNI metadataSeeing program labelling (start 6 1) (start 5 2) := by
  refine ⟨⟨rfl, trivial⟩, rfl, rfl, ?_⟩
  intro h
  have bad := (h (metadataSeeingRun 6 1) (metadataSeeingRun 5 2) ⟨rfl, trivial⟩ rfl rfl).2
  exact (by decide : publicLengths (metadataSeeingRun 6 1).leakage ≠
    publicLengths (metadataSeeingRun 5 2).leakage) bad

/-- Planted definition. Call-path text incorrectly filters semantic release comparison. -/
def pathSensitiveTrace {Ty Val : Type} (rs : List (ReleaseFact Ty Val)) :
    List (ReleaseSite Ty × Val) :=
  (rs.filter fun r => !r.path.isEmpty).map fun r => (r.release.site, r.release.value)

/-- Planted (law `release_metadata_irrelevant`, fault `pathSensitiveComparison`). -/
example : ¬ (pathSensitiveTrace [{ metadataFact with path := [Exec.Demo.logF] }] =
    pathSensitiveTrace [metadataFact]) := by decide

/-- Instance (law `instrumentation_erasure`, case `secureExecution`). -/
example : ∃ events, Steps program (start 6 1) events (secureRun 6 1).final :=
  (instrumentation_erasure instrumentation program _ _).mp
    ⟨_, _, _, _, (secureRun 6 1).steps⟩

/-- Planted relation: only reflexivity is restricted, to non-final configurations.
All trace indices and the step constructor retain their production shape. -/
inductive RestrictedSteps {O : Operands} (I : Instrumentation O) (P : Program O) :
    Config O → List I.Observation → List (Leakage O.Val I.Address I.Effect) →
      List (Release O.Ty O.Val) → List I.Choice → Config O → Prop where
  | refl (c : Config O) (unfinished : c.final? = none) : RestrictedSteps I P c [] [] [] [] c
  | step {c l c' obs leaks rs choices c''} (h : Step P c l c')
      (rest : RestrictedSteps I P c' obs leaks rs choices c'') :
      RestrictedSteps I P c (I.observeStep c l c' ++ obs) (I.leakage c l c' ++ leaks)
        (I.releases c l c' ++ rs) (I.choices c l c' ++ choices) c''

/-- Planted (law `instrumentation_erasure`, fault `restrictedRefl`).
The erased reflexive run inhabits the reverse implication's premise. -/
example : (∃ events, Steps program (finished 1) events (finished 1)) ∧
    ¬ ((∃ obs leaks rs choices, RestrictedSteps instrumentation program
      (finished 1) obs leaks rs choices (finished 1)) ↔
      ∃ events, Steps program (finished 1) events (finished 1)) := by
  refine ⟨⟨[], .refl _⟩, ?_⟩
  intro h
  obtain ⟨_, _, _, _, hs⟩ := h.mpr ⟨[], .refl _⟩
  cases hs with
  | refl _ unfinished => cases unfinished
  | step hs _ => exact no_step_of_none (P := program) (c := finished 1) rfl _ _ hs

/-- Planted relation: the step rule alone admits the illicit final-to-entry transition.
Reflexivity, recursive trace indices and trace accumulation are unchanged. -/
inductive AdmittingSteps (I : Instrumentation Demo.ops) (P : Program Demo.ops) :
    Config Demo.ops → List I.Observation → List (Leakage Int I.Address I.Effect) →
      List (Release Unit Int) → List I.Choice → Config Demo.ops → Prop where
  | refl (c) : AdmittingSteps I P c [] [] [] [] c
  | step {c l c' obs leaks rs choices c''}
      (h : Step P c l c' ∨ (c = finished 1 ∧ c' = start 1))
      (rest : AdmittingSteps I P c' obs leaks rs choices c'') :
      AdmittingSteps I P c (I.observeStep c l c' ++ obs) (I.leakage c l c' ++ leaks)
        (I.releases c l c' ++ rs) (I.choices c l c' ++ choices) c''

/-- Planted (law `instrumentation_erasure`, fault `illicitStep`).
The added transition inhabits the forward implication's premise. -/
example : (∃ obs leaks rs choices, AdmittingSteps instrumentation program
      (finished 1) obs leaks rs choices (start 1)) ∧
    ¬ ((∃ obs leaks rs choices, AdmittingSteps instrumentation program
      (finished 1) obs leaks rs choices (start 1)) ↔
      ∃ events, Steps program (finished 1) events (start 1)) := by
  have bad : ∃ obs leaks rs choices, AdmittingSteps instrumentation program
      (finished 1) obs leaks rs choices (start 1) :=
    ⟨_, _, _, _, .step (l := .silent) (Or.inr ⟨rfl, rfl⟩) (.refl _)⟩
  refine ⟨bad, ?_⟩
  intro h
  obtain ⟨_, hs⟩ := h.mp bad
  cases hs with
  | step hs _ => exact no_step_of_none (P := program) (c := finished 1) rfl _ _ hs

/-- Instance (law `release_metadata_irrelevant`, case `factsOfSecureRun`).
Reasons and paths are connected to an actual run while remaining outside its release premise. -/
example : ReleaseFact.semanticTrace
    (ReleaseFact.attach (secureRun 6 1).releases
      (fun _ => "changed reason") (fun _ => [Exec.Demo.logF])).entries =
    releaseTrace (secureRun 6 1).releases := RunFacts.trace_eq _


end Audit
end ConcreteSpec.Core.Leak
