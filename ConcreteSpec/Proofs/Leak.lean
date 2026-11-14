import ConcreteSpec.Proofs.Exec
import ConcreteSpec.Spec.Basics
import ConcreteSpec.Spec.Exec
import ConcreteSpec.Spec.Ident
import ConcreteSpec.Spec.Leak
import ConcreteSpec.Spec.Outcome

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

namespace Evidence

/-- Helper definition. Transition-parametric fold for uniform planted substitution. -/
def run (m : Memory) (actions : List Action)
    (transition : Memory → Action → Option Memory := apply) : Option Memory :=
  match actions with
  | [] => some m
  | a :: rest => (transition m a).bind fun m' => run m' rest transition

/-- Helper definition. Transition-parametric fold for uniform planted substitution. -/
def resetBytes (lineage : LineageId) (m : Memory) (actions : List Action)
    (transition : Memory → Action → Option Memory := apply) : Nat :=
  match actions with
  | [] => 0
  | action :: rest =>
      match transition m action with
      | none => 0
      | some m' => resetOutflow lineage m action + resetBytes lineage m' rest transition

/-- Helper definition. Transition-parametric fold for uniform planted substitution. -/
def observedPeak (lineage : LineageId) (m : Memory) (actions : List Action)
    (transition : Memory → Action → Option Memory := apply) : Nat :=
  match actions with
  | [] => (m lineage).live
  | action :: rest =>
      match transition m action with
      | none => (m lineage).live
      | some m' => max (m lineage).live (observedPeak lineage m' rest transition)

/-- Helper (agreement). The evidence fold at `apply` is the normative execution. -/
theorem run_apply (m : Memory) (actions : List Action) :
    run m actions apply = ConcreteSpec.Core.Leak.run m actions := by
  induction actions generalizing m with
  | nil => rfl
  | cons a rest ih =>
    simp only [run, ConcreteSpec.Core.Leak.run]
    cases h : apply m a <;> simp [ih]

/-- Helper (agreement). Reset observation at `apply` is the normative reset observation. -/
theorem resetBytes_apply (l : LineageId) (m : Memory) (actions : List Action) :
    resetBytes l m actions apply = ConcreteSpec.Core.Leak.resetBytes l m actions := by
  induction actions generalizing m with
  | nil => rfl
  | cons a rest ih =>
    simp only [resetBytes, ConcreteSpec.Core.Leak.resetBytes]
    cases h : apply m a <;> simp [ih]

/-- Helper (agreement). Peak observation at `apply` is the normative independent observer. -/
theorem observedPeak_apply (l : LineageId) (m : Memory) (actions : List Action) :
    observedPeak l m actions apply = ConcreteSpec.Core.Leak.observedPeak l m actions := by
  induction actions generalizing m with
  | nil => rfl
  | cons a rest ih =>
    simp only [observedPeak, ConcreteSpec.Core.Leak.observedPeak]
    cases h : apply m a <;> simp [ih]

end Evidence

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
#decided-project-infoflow-2026-11-06.

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
#decided-project-infoflow-2026-11-06. -/
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
def hiddenVar : Var := ⟨0⟩

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

/-- Build the toy C1 operand instance for a given primitive answer oracle. Its nonzero condition
test is local to this example and chooses no Concrete Boolean encoding (ISS-0069). -/
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
  ⟨⟨⟨0⟩, ()⟩, value⟩

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
#decided-project-infoflow-2026-11-06.

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

/-- Helper (inversion). Unrolls the four-step C1 derivation from `Demo.start` to compute its exact observation, leakage, release, choice, and final-state traces.

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
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7),
#decided-project-infoflow-2026-11-06.

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
#decided-project-infoflow-2026-11-06.

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

/-- Helper (inversion). Unrolls the four-step C1 derivation from `Demo.leakyStart` to show every complete run releases only `releaseZero`. -/
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

/-- Helper (inversion). Unrolls the four-step C1 derivation from `Demo.ctStart` to compute its constant-time observation, leakage, release, choice, and final-state traces. -/
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

/-- Representation lemma. Concrete projection check showing secureRun 1 maps via `InstrumentedSteps.toSteps` to an underlying C1 `Steps` derivation.

The concrete instrumented run projects to C1, not to a second semantics. -/
example : ∃ events, Steps program (start 1) events (finished 1 (hiddenLocals 0)) :=
  (secureRun 1).steps.toSteps

/-- Helper (inversion). Unrolls the seven-step C1 derivation from `Demo.timingStart` to compute its exact observation, leakage, release, choice, and final-state traces.

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
#decided-project-infoflow-2026-11-06.

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
#decided-project-infoflow-2026-11-06. This theorem concerns the explicit countermodel below;
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
  simp [secureRun] at h

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
#decided-project-infoflow-2026-11-06. This theorem concerns the explicit countermodel below;
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
  simp [Run.publicObservations, leakyRun, finished, Config.final?] at hobs
  exact hs hobs

/-- Law: C7's distinct security obligations cannot be discharged by a weaker observation.
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7),
#decided-project-infoflow-2026-11-06. This theorem concerns the explicit countermodel below;
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
  simp [leakyRun] at hleak
  exact hs hleak

end Demo

/-! ## Accounting conservation -/

/-- Law (subject `Account.alloc`). Zero-sized requests invent no bytes.
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7). -/
theorem Account.alloc_zero (a : Account) (h : a.WF) : a.alloc 0 = a := by
  cases a
  simp_all [Account.alloc, Account.WF]

/-- Representation lemma. Functional update identity.
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7),
#decided-project-memory-2026-11-06.

Writing back the existing account at a lineage is the identity on `Memory`. -/
theorem Memory.set_same (m : Memory) (lineage : LineageId) :
    Memory.set m lineage (m lineage) = m := by
  funext l
  by_cases h : l = lineage <;> simp [Memory.set, h]

/-- Helper (arithmetic). Initializes the local peak at live bytes and retains the historical
peak bound; used to initialize `function_entry_peak`. -/
theorem Account.enter_wf (a : Account) (h : a.WF) : a.enter.WF := by
  cases a
  simp_all [Account.enter, Account.WF]

/-- Helper (arithmetic). Upper-bounds live + bytes by both `max` expressions in `Account.alloc` for single-step WF preservation. -/
theorem Account.alloc_wf (a : Account) (bytes : Nat) : (a.alloc bytes).WF := by
  exact ⟨Nat.le_max_right _ _, Nat.le_max_right _ _⟩

/-- Helper (arithmetic). Uses `Nat.sub_le` transitivity to preserve peak bounds across `Account.free` for single-step WF preservation. -/
theorem Account.free_wf (a : Account) (bytes : Nat) (h : a.WF) : (a.free bytes).WF := by
  exact ⟨Nat.le_trans (Nat.sub_le _ _) h.1, Nat.le_trans (Nat.sub_le _ _) h.2⟩

/-- Helper (arithmetic). Shows zero live bytes after `Account.reset` satisfy the non-negative peak bounds for single-step WF preservation. -/
theorem Account.reset_wf (a : Account) : a.reset.WF := by
  simp [Account.WF, Account.reset]

/-- Helper (inversion). Lifts `Account.alloc_wf`, `Account.free_wf`, and `Account.reset_wf` across one successful `apply` transition for `run_wf` and `run_peak_exact`.

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
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7),
#decided-project-memory-2026-11-06.

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
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7),
#decided-project-memory-2026-11-06.

A zero-byte allocation changes no account at any lineage. -/
theorem apply_alloc_zero {m : Memory} (hwf : ∀ l, (m l).WF) (lineage : LineageId) :
    apply m (.alloc lineage 0) = some m := by
  simp only [apply, Account.alloc_zero (m lineage) (hwf lineage), Memory.set_same]

/-- Helper (computation). Exposes the fields of a successful free and its frame for the
independent byte-balance theorem `free_conservation`. -/
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

/-- Law (subject `ActionPermitted`). Free succeeds exactly when its outflow is owned.
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7),
#decided-project-memory-2026-11-06. Both implications are required. -/
theorem free_permitted_iff (m : Memory) (l : LineageId) (bytes : Nat) :
    ActionPermitted m (.free l bytes) ↔ bytes ≤ (m l).live := by
  simp [ActionPermitted, apply]

/-- Law (subject `apply`).
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7),
#decided-project-memory-2026-11-06.

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
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7),
#decided-project-memory-2026-11-06.

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

/-- Helper (computation). Unfolds one cons step of `resetBytes` on a successful `apply` transition for `run_conservation`. -/
@[simp] theorem resetBytes_cons {m m' : Memory} {action : Action} {rest : List Action}
    (lineage : LineageId) (h : apply m action = some m') :
    resetBytes lineage m (action :: rest) =
      resetOutflow lineage m action + resetBytes lineage m' rest := by
  simp [resetBytes, h]

/-- Law (subject `run`).
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7),
#decided-project-memory-2026-11-06.

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

/-- Helper (arithmetic). Shows one successful `apply` transition never decreases peak or functionPeak for `run_peak_monotone`.

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
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7),
#decided-project-memory-2026-11-06.

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

/-- Helper (computation). Expresses the post-step functionPeak as `max` of the pre-step functionPeak and post-step live bytes for `run_peak_exact`.

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
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7),
#decided-project-memory-2026-11-06.

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

/-- Helper (arithmetic). A successful step records the exact historical high-water mark. -/
theorem apply_historical_peak_exact {m m' : Memory} {action : Action}
    (hwf : ∀ l, (m l).WF) (h : apply m action = some m') (lineage : LineageId) :
    (m' lineage).peak = max (m lineage).peak (m' lineage).live := by
  cases action with
  | alloc l bytes =>
    cases h
    by_cases hl : lineage = l
    · subst lineage; simp [Memory.set, Account.alloc]
    · simp [Memory.set, hl, Nat.max_eq_left (hwf lineage).1]
  | free l bytes =>
    simp only [apply] at h
    split at h
    next =>
      cases h
      by_cases hl : lineage = l
      · subst lineage
        simp only [Memory.set, ↓reduceIte, Account.free]
        exact (Nat.max_eq_left (Nat.le_trans (Nat.sub_le _ _) (hwf l).1)).symm
      · simp [Memory.set, hl, Nat.max_eq_left (hwf lineage).1]
    next => cases h
  | reset l =>
    cases h
    by_cases hl : lineage = l
    · subst lineage; simp [Memory.set, Account.reset]
    · simp [Memory.set, hl, Nat.max_eq_left (hwf lineage).1]

/-- Law (subject `run`).
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7),
#decided-project-memory-2026-11-06.

The recorded historical peak equals the maximum of the entry historical peak and the independently
observed maximum `observedPeak` over the live-byte trajectory. -/
theorem run_historical_peak_exact (m : Memory) (actions : List Action) (lineage : LineageId)
    (hwf : ∀ l, (m l).WF) (hrun : (run m actions).isSome = true) :
    ∃ m', run m actions = some m' ∧
      (m' lineage).peak = max (m lineage).peak (observedPeak lineage m actions) := by
  obtain ⟨m', h⟩ := Option.isSome_iff_exists.mp hrun
  clear hrun
  refine ⟨m', h, ?_⟩
  induction actions generalizing m with
  | nil =>
    cases h
    exact (Nat.max_eq_left (hwf lineage).1).symm
  | cons action rest ih =>
    cases ha : apply m action with
    | none => simp [run, ha] at h
    | some m₁ =>
      have hr : run m₁ rest = some m' := by simpa [run, ha] using h
      rw [ih m₁ (apply_wf ha hwf) hr, apply_historical_peak_exact hwf ha lineage]
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
      rw [Nat.max_eq_right hrest, ← Nat.max_assoc, Nat.max_eq_left (hwf lineage).1]

/-- Law (subject `Account.enter`).
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7),
#decided-project-memory-2026-11-06.

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
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7),
#decided-project-memory-2026-11-06.

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
    (hnet : SignedInterval.Sound ((allocated lineage actions : Int) - freed lineage actions -
      resetBytes lineage m actions) r.netChange)
    (hpeak : Bound.Sound (observedPeak lineage m actions - (m lineage).live) r.peak)
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
    change (m' lineage).functionPeak - (m lineage).live ≤ r.peak.limit
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

/-- Helper (computation). Evaluates `Account.WF` across all lineages of the concrete `memoryA` fixture for Audit instances and mutants. -/
theorem memoryA_wf : ∀ l, (memoryA l).WF := by
  intro l
  by_cases h₀ : l = ⟨0⟩
  · subst l
    simp [memoryA, Memory.set, Account.WF, accountA]
  · by_cases h₁ : l = ⟨1⟩
    · subst l
      simp [memoryA, Memory.set, Account.WF, accountB]
    · simp [memoryA, Memory.set, h₀, h₁, Account.WF, Memory.empty, Account.empty]

/-- Representation lemma. Concrete initialization check showing `Account.empty` is well-formed and `Memory.empty` maps lineage 0 to `Account.empty`. -/
example : Account.empty.WF ∧ Memory.empty ⟨0⟩ = Account.empty := by
  simp [Account.WF, Memory.empty, Account.empty]

/-- Representation lemma. Concrete refusal check showing an account whose live exceeds peak and functionPeak fails `Account.WF`. -/
example : ¬ (⟨0, 5, 0, 0⟩ : Account).WF := by simp [Account.WF]

/-- Representation lemma. Concrete frame check showing `Memory.set` on lineage 0 leaves lineage 1 unchanged at `accountB`. -/
example : Memory.set memoryA ⟨0⟩ (accountA.alloc 1) ⟨1⟩ = accountB := rfl

/-- Representation lemma. Concrete refusal check showing `badPeakAccount` (where peaks are 14 but live is 15) fails `Account.WF`. -/
example : ¬ badPeakAccount.WF := by
  simp [badPeakAccount, Account.WF, accountA, Account.alloc]

/-- Representation lemma. Concrete refusal check showing `run` returns `none` when an intermediate .free exceeds current live bytes. -/
example : run Memory.empty [.alloc ⟨0⟩ 8, .free ⟨0⟩ 9, .alloc ⟨0⟩ 1] = none := by decide

/-- Representation lemma. Concrete witness showing live bytes (unlike peak and functionPeak) strictly decrease on a non-zero .free transition. -/
example : ∃ m m' : Memory, ∃ l : LineageId,
    apply m (.free l 3) = some m' ∧ ¬ (m l).live ≤ (m' l).live := by
  refine ⟨memoryA, Memory.set memoryA ⟨0⟩ (accountA.free 3), ⟨0⟩, rfl, ?_⟩
  decide

/-- Representation lemma. Concrete per-lineage sum check for `allocated` on interleaved `twoLineageActions`. -/
example : allocated ⟨0⟩ twoLineageActions = 14 ∧ allocated ⟨1⟩ twoLineageActions = 4 := by decide

/-- Representation lemma. Concrete per-lineage sum check for `freed` on interleaved `twoLineageActions`. -/
example : freed ⟨0⟩ twoLineageActions = 3 ∧ freed ⟨1⟩ twoLineageActions = 0 := by decide

/-- Representation lemma. Concrete per-lineage outflow check for `resetBytes` across multiple resets in `twoLineageActions`. -/
example : resetBytes ⟨0⟩ memoryA twoLineageActions = 16 ∧
    resetBytes ⟨1⟩ memoryA twoLineageActions = 6 := by decide

/-- Representation lemma. Concrete check that `Bound.Sound` holds when an .boundEstablished limit is at least the actual value. -/
example : Bound.Sound 10 ⟨12, .boundEstablished⟩ := by simp [Bound.Sound]

/-- Representation lemma. Concrete check that `Bound.Sound` fails when an .boundEstablished limit is strictly less than the actual value. -/
example : ¬ Bound.Sound 10 ⟨9, .boundEstablished⟩ := by simp [Bound.Sound]

/-- Representation lemma. Concrete check that `Bound.SoundOptional` holds when an optional footprint is some 10 and the established limit is 12. -/
example : Bound.SoundOptional (some 10) ⟨12, .boundEstablished⟩ := by
  simp [Bound.SoundOptional]

/-- Representation lemma. Concrete check that `Bound.SoundOptional` fails when an optional footprint is some 10 and the established limit is 9. -/
example : ¬ Bound.SoundOptional (some 10) ⟨9, .boundEstablished⟩ := by
  simp [Bound.SoundOptional]

/-- Representation lemma. Concrete check that `Bound.SoundOptional` fails when the allocator footprint is `none` but the bound claims .boundEstablished. -/
example : ¬ Bound.SoundOptional none ⟨12, .boundEstablished⟩ := by
  simp [Bound.SoundOptional]

/-- Example allocator contract: its footprint is the independent logical trajectory maximum.
A real allocator may include metadata and slack; no such equality is assumed generically. -/
def demoAllocator (transition : Memory → Action → Option Memory := apply) : AllocatorContract :=
  ⟨fun lineage m actions => some (Evidence.observedPeak lineage m actions transition)⟩

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
  ⟨⟨0⟩, ⟨8, .boundEstablished⟩, ⟨5, 5, .boundEstablished⟩,
    ⟨8, .boundEstablished⟩, ⟨12, .boundEstablished⟩⟩

/-- A mixed-evidence report: net change is .notEstablished, while the other three fields are
.boundEstablished. -/
def mixedStatusReport : LineageReport :=
  ⟨⟨0⟩, ⟨8, .boundEstablished⟩, ⟨-100, 100, .notEstablished⟩,
    ⟨8, .boundEstablished⟩, ⟨12, .boundEstablished⟩⟩

/-- Report for an empty function run under `noFootprintAllocator`: zero local request, net, and
peak bounds are .boundEstablished, while footprint is .notEstablished. -/
def emptyFunctionReport : LineageReport :=
  ⟨⟨0⟩, ⟨0, .boundEstablished⟩, ⟨0, 0, .boundEstablished⟩,
    ⟨0, .boundEstablished⟩, ⟨0, .notEstablished⟩⟩

/-- Representation lemma. Concrete refusal check showing `LineageReport.Sound` rejects a report whose lineage field (⟨1⟩) does not match the queried lineage (⟨0⟩). -/
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

/-- Representation lemma: a concrete control for a function-backed identity, not Law evidence. -/
example : Memory.set memoryA ⟨0⟩ (memoryA ⟨0⟩) = memoryA :=
  Memory.set_same memoryA ⟨0⟩

/-- Representation lemma: a concrete control for a function-backed identity, not Law evidence. -/
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

/-- Mutant transition that clears peak and functionPeak on a reset instead of retaining them. -/
def applyBadReset (m : Memory) : Action → Option Memory
  | .reset lineage => some (Memory.set m lineage { (m lineage).reset with peak := 0 })
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


/-- Instance (law `run_wf`, case `conservationActions`). -/
example : ∃ m', run memoryA conservationActions = some m' ∧ ∀ l, (m' l).WF :=
  run_wf memoryA conservationActions memoryA_wf rfl

/-- Instance (law `run_conservation`, case `conservationActions`). -/
example : ∃ m', run memoryA conservationActions = some m' ∧
    (m' ⟨0⟩).requested = (memoryA ⟨0⟩).requested + allocated ⟨0⟩ conservationActions ∧
    ((m' ⟨0⟩).live : Int) = (memoryA ⟨0⟩).live + allocated ⟨0⟩ conservationActions -
      freed ⟨0⟩ conservationActions - resetBytes ⟨0⟩ memoryA conservationActions :=
  run_conservation memoryA conservationActions ⟨0⟩ rfl

/-- Instance (law `run_peak_monotone`, case `conservationActions`). -/
example : ∃ m', run memoryA conservationActions = some m' ∧
    (memoryA ⟨0⟩).peak ≤ (m' ⟨0⟩).peak ∧
    (memoryA ⟨0⟩).functionPeak ≤ (m' ⟨0⟩).functionPeak :=
  run_peak_monotone memoryA conservationActions ⟨0⟩ rfl

/-- Instance (law `run_peak_exact`, case `conservationActions`). -/
example : ∃ m', run memoryA conservationActions = some m' ∧
    (m' ⟨0⟩).functionPeak = max (memoryA ⟨0⟩).functionPeak
      (observedPeak ⟨0⟩ memoryA conservationActions) :=
  run_peak_exact memoryA conservationActions ⟨0⟩ memoryA_wf rfl

/-- Helper (computation). Evaluates `Account.WF` across all lineages of `reportEntryMemory` for `report_certified` instances and mutants. -/
theorem reportEntry_wf : ∀ l, (reportEntryMemory l).WF := by
  intro l
  by_cases hl : l = ⟨0⟩
  · subst l; simp [reportEntryMemory, Memory.set, reportEntry, Account.WF]
  · simp [reportEntryMemory, Memory.set, hl, Memory.empty, Account.empty, Account.WF]

/-- Helper (computation). Evaluates `Account.WF` across all lineages of `emptyFunctionEntryMemory` for the emptyFunction instance and mutant of `report_certified`. -/
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
    (by dsimp [Bound.Sound, SignedInterval.Sound, establishedReport]; decide)
    (by dsimp [Bound.Sound, SignedInterval.Sound, establishedReport]; decide)
    (by dsimp [Bound.Sound, SignedInterval.Sound, establishedReport]; decide)
    (by intro _; exact ⟨10, rfl, by decide⟩)

/-- Instance (law `report_certified`, case `mixedStatus`). -/
example : ∃ m', run reportEntryMemory reportActions = some m' ∧
    LineageReport.Sound ⟨0⟩ (reportEntryMemory ⟨0⟩) (m' ⟨0⟩)
      (demoAllocator.footprint ⟨0⟩ reportEntryMemory reportActions) mixedStatusReport :=
  report_certified reportEntryMemory reportActions demoAllocator ⟨0⟩ mixedStatusReport
    rfl reportEntry_wf rfl rfl
    (by dsimp [Bound.Sound, SignedInterval.Sound, mixedStatusReport, establishedReport]; decide)
    (by simp [SignedInterval.Sound, mixedStatusReport])
    (by dsimp [Bound.Sound, SignedInterval.Sound, mixedStatusReport, establishedReport]; decide)
    (by intro _; exact ⟨10, rfl, by decide⟩)

/-- Instance (law `report_certified`, case `emptyFunction`). -/
example : ∃ m', run emptyFunctionEntryMemory [] = some m' ∧
    LineageReport.Sound ⟨0⟩ (emptyFunctionEntryMemory ⟨0⟩) (m' ⟨0⟩)
      (noFootprintAllocator.footprint ⟨0⟩ emptyFunctionEntryMemory []) emptyFunctionReport :=
  report_certified emptyFunctionEntryMemory [] noFootprintAllocator ⟨0⟩ emptyFunctionReport
    rfl emptyFunctionEntry_wf rfl rfl
    (by dsimp [Bound.Sound, SignedInterval.Sound, emptyFunctionReport, establishedReport]; decide)
    (by dsimp [Bound.Sound, SignedInterval.Sound, emptyFunctionReport, establishedReport]; decide)
    (by dsimp [Bound.Sound, SignedInterval.Sound, emptyFunctionReport, establishedReport]; decide)
    (by simp [Bound.SoundOptional, emptyFunctionReport])

/-- Representation lemma. Concrete trajectory calculation showing `observedPeak` records 10 across `reportActions` (including 2 bytes live at entry) and 2 on an empty action list. -/
example : observedPeak ⟨0⟩ reportEntryMemory reportActions = 10 ∧
    observedPeak ⟨0⟩ reportEntryMemory [] = 2 := by decide

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
  simp [exposingRun] at hleak

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

/-- Planted definition: a free forgets to subtract its bytes; all other fields and rules agree. -/
def freeKeepsLive (m : Memory) : Action → Option Memory
  | .free l n => if n ≤ (m l).live then some m else none
  | a => apply m a

/-- Planted (law `free_conservation`, case `populatedFree`, mutant `freeKeepsLive`).
the populated free has all original premises, but its live-byte equation fails. -/
theorem free_keeps_live : 3 ≤ (memoryA ⟨0⟩).live ∧
    ¬ (∃ m', freeKeepsLive memoryA (.free ⟨0⟩ 3) = some m' ∧
      (m' ⟨0⟩).requested = (memoryA ⟨0⟩).requested ∧
      (m' ⟨0⟩).live + 3 = (memoryA ⟨0⟩).live ∧
      (m' ⟨0⟩).peak = (memoryA ⟨0⟩).peak ∧
      (m' ⟨0⟩).functionPeak = (memoryA ⟨0⟩).functionPeak ∧
      ∀ l, l ≠ ⟨0⟩ → m' l = memoryA l) := by
  refine ⟨by decide, ?_⟩
  rintro ⟨m', h, _, bad, _, _, _⟩
  cases h
  change 5 + 3 = (5 : Nat) at bad
  omega

/-- Planted definition: allocation forgets to record requested bytes, changing only that rule. -/
def losesRequest (m : Memory) : Action → Option Memory
  | .alloc l n => some (Memory.set m l { (m l).alloc n with requested := (m l).requested })
  | a => apply m a

/-- Planted definition: allocation overestimates its local peak by one; flow and historical peak agree. -/
def extraPeak (m : Memory) : Action → Option Memory
  | .alloc l n => some (Memory.set m l
      { (m l).alloc n with functionPeak := max (m l).functionPeak ((m l).live + n + 1) })
  | a => apply m a

/-- Planted definition: allocation fails to update the local peak, retaining every other rule. -/
def stalePeak (m : Memory) : Action → Option Memory
  | .alloc l n => some (Memory.set m l
      { (m l).alloc n with functionPeak := (m l).functionPeak })
  | a => apply m a

/-- Planted (law `run_wf`, case `populatedAllocation`, mutant `stalePeak`).
run_wf fails with a stale local peak on a successful allocation from populated memory. -/
theorem run_wf_planted : (∀ l, (memoryA l).WF) ∧
    (Evidence.run memoryA [.alloc ⟨0⟩ 8] stalePeak).isSome = true ∧
    ¬ (∃ m', Evidence.run memoryA [.alloc ⟨0⟩ 8] stalePeak = some m' ∧ ∀ l, (m' l).WF) := by
  refine ⟨memoryA_wf, rfl, ?_⟩
  rintro ⟨m', h, wf⟩
  cases h
  have bad := (wf ⟨0⟩).2
  change 13 ≤ 9 at bad
  omega

/-- Planted (law `run_conservation`, case `populatedSequence`, mutant `losesRequest`).
run_conservation rejects lost requested bytes. Reset outflows use the same mutant
transition at every intermediate state, including the derived observer. -/
theorem conservation_planted :
    (Evidence.run memoryA conservationActions losesRequest).isSome = true ∧
    ¬ (∃ m', Evidence.run memoryA conservationActions losesRequest = some m' ∧
      (m' ⟨0⟩).requested = (memoryA ⟨0⟩).requested + allocated ⟨0⟩ conservationActions ∧
      ((m' ⟨0⟩).live : Int) = (memoryA ⟨0⟩).live + allocated ⟨0⟩ conservationActions -
        freed ⟨0⟩ conservationActions - Evidence.resetBytes ⟨0⟩ memoryA conservationActions losesRequest) := by
  refine ⟨rfl, ?_⟩
  rintro ⟨m', h, bad, _⟩
  cases h
  change (100 : Nat) = 108 at bad
  omega

/-- Planted (law `run_peak_monotone`, case `populatedReset`, mutant `applyBadReset`).
run_peak_monotone rejects resetting the historical peak on a populated account. -/
theorem monotone_planted :
    (Evidence.run memoryA conservationActions applyBadReset).isSome = true ∧
    ¬ (∃ m', Evidence.run memoryA conservationActions applyBadReset = some m' ∧
      (memoryA ⟨0⟩).peak ≤ (m' ⟨0⟩).peak ∧
      (memoryA ⟨0⟩).functionPeak ≤ (m' ⟨0⟩).functionPeak) := by
  refine ⟨rfl, ?_⟩
  rintro ⟨m', h, bad, _⟩
  cases h
  change 12 ≤ 0 at bad
  omega

/-- Planted (law `run_peak_exact`, case `oneExtraByte`, mutant `extraPeak`).
run_peak_exact rejects a one-byte overestimate. The trajectory uses the mutant too;
its live fields remain correct, so this is independent of both flow and well-formedness. -/
theorem exact_peak_planted : (∀ l, (memoryA l).WF) ∧
    (Evidence.run memoryA conservationActions extraPeak).isSome = true ∧
    ¬ (∃ m', Evidence.run memoryA conservationActions extraPeak = some m' ∧
      (m' ⟨0⟩).functionPeak = max (memoryA ⟨0⟩).functionPeak
        (Evidence.observedPeak ⟨0⟩ memoryA conservationActions extraPeak)) := by
  refine ⟨memoryA_wf, rfl, ?_⟩
  rintro ⟨m', h, bad⟩
  cases h
  change (14 : Nat) = 13 at bad
  omega

/-- Planted definition: allocation records one extra requested byte, keeping live and peaks correct. -/
def extraRequest (m : Memory) : Action → Option Memory
  | .alloc l n => some (Memory.set m l { (m l).alloc n with requested := (m l).requested + n + 1 })
  | a => apply m a

/-- Planted (law `report_certified`, case `populatedReport`, mutant `extraRequest`).
report_certified rejects an extra request even with correct independent analysis bounds.
Both reset and peak observers use the substituted transition. -/
theorem report_planted : establishedReport.lineage = ⟨0⟩ ∧
    (∀ l, (reportEntryMemory l).WF) ∧
    (reportEntryMemory ⟨0⟩).functionPeak = (reportEntryMemory ⟨0⟩).live ∧
    (Evidence.run reportEntryMemory reportActions extraRequest).isSome = true ∧
    Bound.Sound (allocated ⟨0⟩ reportActions) establishedReport.requested ∧
    SignedInterval.Sound ((allocated ⟨0⟩ reportActions : Int) - freed ⟨0⟩ reportActions -
      Evidence.resetBytes ⟨0⟩ reportEntryMemory reportActions extraRequest) establishedReport.netChange ∧
    Bound.Sound (Evidence.observedPeak ⟨0⟩ reportEntryMemory reportActions extraRequest - (reportEntryMemory ⟨0⟩).live) establishedReport.peak ∧
    Bound.SoundOptional ((demoAllocator extraRequest).footprint ⟨0⟩ reportEntryMemory reportActions) establishedReport.footprint ∧
    ¬ (∃ m', Evidence.run reportEntryMemory reportActions extraRequest = some m' ∧
      LineageReport.Sound ⟨0⟩ (reportEntryMemory ⟨0⟩) (m' ⟨0⟩)
        ((demoAllocator extraRequest).footprint ⟨0⟩ reportEntryMemory reportActions) establishedReport) := by
  refine ⟨rfl, reportEntry_wf, rfl, rfl, by dsimp [Bound.Sound, SignedInterval.Sound, establishedReport]; decide,
    by dsimp [Bound.Sound, SignedInterval.Sound, establishedReport]; decide,
    by dsimp [Bound.Sound, SignedInterval.Sound, establishedReport]; decide,
    (by intro _; exact ⟨10, rfl, by decide⟩), ?_⟩
  rintro ⟨m', h, sound⟩
  cases h
  have bad := sound.2.1 rfl
  change (9 : Nat) ≤ 8 at bad
  omega

/-- Planted definition: retain NI's complete-run domain and premises but admit every output pair.
This adds permitted observations rather than manufacturing an elaboration failure. -/
def blindNI {O : Operands} (I : Instrumentation O) (P : Program O)
    (L : Labelling O) (c₁ c₂ : Config O) : Prop :=
  ∀ (r₁ : Run I P c₁) (r₂ : Run I P c₂),
    LowEquivalent L c₁ c₂ → r₁.choices = r₂.choices →
      releaseTrace r₁.releases = releaseTrace r₂.releases → True

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
  exact ⟨release_authorized 1, release_authorized 2, leaky_low 1 2, by decide,
    fun h => h (fun _ _ _ _ _ => True.intro)⟩

/-- Planted (law `authorized_release_not_ct`, case `leakyStart`, mutant `blindCT`).
the added leakage permission invalidates authorized_release_not_ct on the same runs. -/
theorem authorization_ct_planted :
    policy.RunsAuthorized instrumentation program (leakyStart 1) ∧
    policy.RunsAuthorized instrumentation program (leakyStart 2) ∧
    LowEquivalent labelling (leakyStart 1) (leakyStart 2) ∧ (1 : Int) ≠ 2 ∧
    ¬ (¬ blindCT instrumentation program labelling (leakyStart 1) (leakyStart 2)) := by
  exact ⟨release_authorized 1, release_authorized 2, leaky_low 1 2, by decide,
    fun h => h (fun _ _ _ _ _ => True.intro)⟩

/-- Instance (law `run_historical_peak_exact`, case `populatedHistory`). -/
example : ∃ m', run memoryA conservationActions = some m' ∧
    (m' ⟨0⟩).peak = max (memoryA ⟨0⟩).peak
      (observedPeak ⟨0⟩ memoryA conservationActions) :=
  run_historical_peak_exact memoryA conservationActions ⟨0⟩ memoryA_wf rfl

/-- Planted definition. Add one byte only to the historical peak. -/
def extraHistoricalPeak (m : Memory) : Action → Option Memory
  | .alloc l n => some (Memory.set m l
      { (m l).alloc n with peak := max (m l).peak ((m l).live + n + 1) })
  | a => apply m a

/-- Planted (law `run_historical_peak_exact`, fault `oneExtraHistoricalByte`).
The successful run and its independent observer both use the substituted transition. -/
theorem historical_exact_planted : (∀ l, (memoryA l).WF) ∧
    (Evidence.run memoryA conservationActions extraHistoricalPeak).isSome = true ∧
    ¬ (∃ m', Evidence.run memoryA conservationActions extraHistoricalPeak = some m' ∧
      (m' ⟨0⟩).peak = max (memoryA ⟨0⟩).peak
        (Evidence.observedPeak ⟨0⟩ memoryA conservationActions extraHistoricalPeak)) := by
  refine ⟨memoryA_wf, rfl, ?_⟩
  rintro ⟨m', h, bad⟩
  cases h
  change (14 : Nat) = 13 at bad
  omega

/-- Instance (law `free_permitted_iff`, case `ownedAndExcessive`). -/
example : ActionPermitted memoryA (.free ⟨0⟩ 5) ∧
    ¬ ActionPermitted memoryA (.free ⟨0⟩ 6) := by
  constructor <;> rw [free_permitted_iff] <;> decide

/-- Planted definition. Excessive frees are admitted, retaining every account update. -/
def admitsOverfree (m : Memory) : Action → Option Memory
  | .free l n => some (Memory.set m l ((m l).free n))
  | a => apply m a

/-- Planted (law `free_permitted_iff`, fault `admitsExcessiveOutflow`). -/
example : ¬ ((admitsOverfree memoryA (.free ⟨0⟩ 6)).isSome = true ↔
    6 ≤ (memoryA ⟨0⟩).live) := by decide

/-- Planted definition. A valid positive free is refused as though no bytes were owned. -/
def refusesOwnedFree (m : Memory) : Action → Option Memory
  | .free _ _ => none
  | a => apply m a

/-- Planted (law `free_permitted_iff`, fault `refusesOwnedOutflow`). -/
example : ¬ ((refusesOwnedFree memoryA (.free ⟨0⟩ 5)).isSome = true ↔
    5 ≤ (memoryA ⟨0⟩).live) := by decide

/-- Instance (law `free_permitted_iff`, case `doubleFree`). The second free is refused. -/
example : run Memory.empty [.alloc ⟨0⟩ 5, .free ⟨0⟩ 5, .free ⟨0⟩ 5] = none := by decide

/-- Planted (law `run_conservation`, fault `doubleFreeAdmitted`).
All execution and observer occurrences use the same guard-dropping mutant. -/
example : (Evidence.run Memory.empty [.alloc ⟨0⟩ 5, .free ⟨0⟩ 5, .free ⟨0⟩ 5]
    admitsOverfree).isSome = true ∧
    ¬ (∃ m', Evidence.run Memory.empty [.alloc ⟨0⟩ 5, .free ⟨0⟩ 5, .free ⟨0⟩ 5]
      admitsOverfree = some m' ∧
      ((m' ⟨0⟩).live : Int) = (Memory.empty ⟨0⟩).live +
        allocated ⟨0⟩ [.alloc ⟨0⟩ 5, .free ⟨0⟩ 5, .free ⟨0⟩ 5] -
        freed ⟨0⟩ [.alloc ⟨0⟩ 5, .free ⟨0⟩ 5, .free ⟨0⟩ 5] -
        Evidence.resetBytes ⟨0⟩ Memory.empty [.alloc ⟨0⟩ 5, .free ⟨0⟩ 5, .free ⟨0⟩ 5]
          admitsOverfree) := by
  refine ⟨rfl, ?_⟩
  rintro ⟨m', h, bad⟩
  cases h
  change (0 : Int) = -5 at bad
  omega

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
def metadataSeeingRun (secret : Int) (hidden : Nat := 0) : Run metadataSeeing program (start secret hidden) where
  final := finished secret (hiddenLocals hidden)
  ending := .normal
  finalEnding := rfl
  observations := [.output (secret + Int.ofNat hidden)]
  leakage := List.replicate 4 (.publicLength ⟨9⟩ secret.natAbs)
  releases := [releaseValue (secret + Int.ofNat hidden)]
  choices := [(), (), (), ()]
  steps :=
    InstrumentedSteps.step (I := metadataSeeing)
      (Step.silent (P := program) (c := start secret hidden)
        (c' := afterSeq .masked .masked secret (hiddenLocals hidden)) rfl) <|
    InstrumentedSteps.step (I := metadataSeeing)
      (Step.prim (O := Demo.ops) (P := program)
        (c := afterSeq .masked .masked secret (hiddenLocals hidden))
        (c' := afterRelease .masked secret (hiddenLocals hidden))
        (p := .release) (vs := [secret + Int.ofNat hidden]) (st := secret)
        (a := (⟨none, .ok 0, secret⟩ : Answer Int Int Int)) rfl (by simp [allowed]) rfl) <|
    InstrumentedSteps.step (I := metadataSeeing)
      (Step.silent (P := program) (c := afterRelease .masked secret (hiddenLocals hidden))
        (c' := beforeOutput .masked secret (hiddenLocals hidden)) rfl) <|
    InstrumentedSteps.step (I := metadataSeeing)
      (Step.prim (O := Demo.ops) (P := program)
        (c := beforeOutput .masked secret (hiddenLocals hidden))
        (c' := finished secret (hiddenLocals hidden))
        (p := .output) (vs := [secret + Int.ofNat hidden]) (st := secret)
        (a := (⟨some (secret + Int.ofNat hidden), .ok 0, secret⟩ : Answer Int Int Int))
        rfl (by simp [allowed]) rfl) <|
    InstrumentedSteps.refl (I := metadataSeeing) (P := program) (finished secret (hiddenLocals hidden))


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

/-- Planted definition. Instrumentation incorrectly deletes every reflexive execution. -/
def filteredInstrumentation {O : Operands} (I : Instrumentation O) (P : Program O)
    (c c' : Config O) : Prop :=
  (∃ obs leaks rs choices, InstrumentedSteps I P c obs leaks rs choices c') ∧ c ≠ c'

/-- Planted (law `instrumentation_erasure`, fault `removesC1Execution`). -/
example : ¬ (filteredInstrumentation instrumentation program (finished 1) (finished 1) ↔
    ∃ events, Steps program (finished 1) events (finished 1)) := by
  intro h
  exact (h.mpr ⟨[], .refl _⟩).2 rfl

/-- Planted definition. Instrumentation admits every pair of endpoints. -/
def admittingInstrumentation (_c _c' : Config Demo.ops) : Prop := True

/-- Planted (law `instrumentation_erasure`, fault `addsC1Execution`). -/
example : ¬ (admittingInstrumentation (finished 1) (start 1) ↔
    ∃ events, Steps program (finished 1) events (start 1)) := by
  intro h
  obtain ⟨_, hs⟩ := h.mp trivial
  cases hs with
  | step hs _ => exact no_step_of_none (P := program) (c := finished 1) rfl _ _ hs

/-- Planted definition. Explicit frees are over-counted by one byte. -/
def extraFreed (l : LineageId) (actions : List Action) : Nat := freed l actions + 1

/-- Planted (law `run_conservation`, fault `extraFreedByte`). -/
example : (run memoryA conservationActions).isSome = true ∧
    ¬ (∃ m', run memoryA conservationActions = some m' ∧
      (m' ⟨0⟩).requested = (memoryA ⟨0⟩).requested + allocated ⟨0⟩ conservationActions ∧
      ((m' ⟨0⟩).live : Int) = (memoryA ⟨0⟩).live + allocated ⟨0⟩ conservationActions -
        extraFreed ⟨0⟩ conservationActions - resetBytes ⟨0⟩ memoryA conservationActions) := by
  refine ⟨rfl, ?_⟩
  rintro ⟨m', h, _, bad⟩
  cases h
  change (0 : Int) = -1 at bad
  omega

/-- Planted definition. Reset outflow is over-counted by one byte. -/
def extraResetBytes (l : LineageId) (m : Memory) (actions : List Action) : Nat :=
  resetBytes l m actions + 1

/-- Planted (law `run_conservation`, fault `extraResetByte`). -/
example : (run memoryA conservationActions).isSome = true ∧
    ¬ (∃ m', run memoryA conservationActions = some m' ∧
      (m' ⟨0⟩).requested = (memoryA ⟨0⟩).requested + allocated ⟨0⟩ conservationActions ∧
      ((m' ⟨0⟩).live : Int) = (memoryA ⟨0⟩).live + allocated ⟨0⟩ conservationActions -
        freed ⟨0⟩ conservationActions - extraResetBytes ⟨0⟩ memoryA conservationActions) := by
  refine ⟨rfl, ?_⟩
  rintro ⟨m', h, _, bad⟩
  cases h
  change (0 : Int) = -1 at bad
  omega

/-- Instance. A no-allocation call reports zero peak with two caller-live bytes. -/
example : LineageReport.Sound ⟨0⟩ reportEntry reportEntry none
    ⟨⟨0⟩, ⟨0, .boundEstablished⟩, ⟨0, 0, .boundEstablished⟩,
      ⟨0, .boundEstablished⟩, ⟨0, .notEstablished⟩⟩ := by
  simp [LineageReport.Sound, reportEntry, Bound.Sound, SignedInterval.Sound, Bound.SoundOptional]

/-- Instance. A signed net interval permits released entry storage to give a negative change. -/
example : SignedInterval.Sound (-3) ⟨-4, -2, .boundEstablished⟩ := by
  simp [SignedInterval.Sound]

/-- Planted definition. Dropping the lower endpoint falsely accepts unbounded net release. -/
def upperOnlyNet (actual : Int) (b : SignedInterval) : Prop :=
  b.status = .boundEstablished → actual ≤ b.hi

/-- Planted. Upper-only checking cannot establish the signed interval contract. -/
example : upperOnlyNet (-5) ⟨-4, -2, .boundEstablished⟩ ∧
    ¬ SignedInterval.Sound (-5) ⟨-4, -2, .boundEstablished⟩ := by
  simp [upperOnlyNet, SignedInterval.Sound]

/-- Instance (law `release_metadata_irrelevant`, case `factsOfSecureRun`).
Reasons and paths are connected to an actual run while remaining outside its release premise. -/
example : ReleaseFact.semanticTrace
    (ReleaseFact.attach (secureRun 6 1).releases
      (fun _ => "changed reason") (fun _ => [Exec.Demo.logF])).entries =
    releaseTrace (secureRun 6 1).releases := RunFacts.trace_eq _

end Audit

end ConcreteSpec.Core.Leak
