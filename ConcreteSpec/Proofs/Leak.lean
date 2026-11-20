import ConcreteSpec.Spec.Leak
import ConcreteSpec.Spec.ReleaseCalls
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
theorem no_step_of_none {O : Operands} {P : Program O} {c : Config O}
    (h : step P c = none) : ∀ l c', ¬ Step P c l c' := by
  intro l c' hs
  obtain ⟨answer, rule⟩ := (step_iff_rules P c l c').mp hs
  exact (DeterminismEvidence.no_dispatch_no_rule h).1 ⟨answer, l, c', rule⟩

/-- Erase the answer index while preserving the C1 transition. -/
theorem AnsweredStep.toStep {O : Operands} {P : Program O} {c d : Config O}
    {l : Label O} {a : Option (Answer O.Val O.Event O.Store)} (h : AnsweredStep P c l d a) :
    Step P c l d := by
  cases h with
  | silent h => exact .silent h
  | prim hr ha hk => exact .prim hr ha hk

/-- C1 exposes an answer witness for every primitive step. -/
theorem step_has_answer {O : Operands} {P : Program O} {c d : Config O}
    {l : Label O} (h : Step P c l d) : ∃ a, AnsweredStep P c l d a := by
  cases h with
  | silent h => exact ⟨none, .silent h⟩
  | prim hr ha hk => exact ⟨some _, .prim hr ha hk⟩

/-- Helper (projection). Forgets C7 instrumentation traces from an `InstrumentedSteps` derivation to recover the underlying C1 `Steps` derivation.

Forget only the C7 instrumentation: an instrumented execution projects to the C1 steps and their
ordinary event trace, with every machine transition unchanged. -/
theorem InstrumentedExecution.toSteps {O : Operands} {I : Instrumentation O} {P : Program O}
    {c d : Config O} {trace : ExecutionTrace O} (h : InstrumentedExecution I P c trace d) :
    ∃ events, Steps P c events d := by
  induction h with
  | refl c => exact ⟨[], Steps.refl c⟩
  | step h _ ih =>
    obtain ⟨events, steps⟩ := ih
    exact ⟨_, Steps.step h.toStep steps⟩

/-- Helper: the individual trace view has the same C1 execution erasure. -/
theorem InstrumentedSteps.toSteps {O : Operands} {I : Instrumentation O} {P : Program O}
    {c d : Config O} {obs leaks rs choices} (h : InstrumentedSteps I P c obs leaks rs choices d) :
    ∃ events, Steps P c events d :=
  InstrumentedExecution.toSteps h

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
      obtain ⟨answer, h⟩ := step_has_answer h
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

/-- Law: a trace whose typed sites are all listed is authorized, including the empty trace
under a forbid-release policy.
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7). -/
theorem listed_trace_authorized {Ty Val : Type} (policy : ReleasePolicy Ty)
    (rs : List (Release Ty Val)) (listed : ∀ r ∈ rs, r.site ∈ policy.allowed) :
    policy.Authorized rs :=
  listed

/-- Law: permission implies membership of the exact typed site. -/
theorem permits_listed_site {Ty Val : Type} (p : ReleasePolicy Ty) (r : Release Ty Val) :
    p.Permits r → r.site ∈ p.allowed := id

/-- Law: membership suffices for permission for any released value. -/
theorem listed_site_permitted {Ty Val : Type} (p : ReleasePolicy Ty) (r : Release Ty Val) :
    r.site ∈ p.allowed → p.Permits r := id

/-- Law: a forbidding policy has no listed site. -/
theorem forbidding_policy_empty {Ty : Type} (p : ReleasePolicy Ty) :
    p.ForbidsRelease → p.allowed = [] := id

/-- Law: an empty policy forbids release. -/
theorem empty_policy_forbidding {Ty : Type} (p : ReleasePolicy Ty) :
    p.allowed = [] → p.ForbidsRelease := id

/-- Law: an unlisted release refutes trace authorization under any policy. -/
theorem unlisted_trace_refused {Ty Val : Type} (p : ReleasePolicy Ty)
    (rs : List (Release Ty Val)) (r : Release Ty Val)
    (hr : r ∈ rs) (unlisted : r.site ∉ p.allowed) : ¬ p.Authorized rs :=
  fun authorized => unlisted (permits_listed_site p r (authorized r hr))

/-- Law: an unlisted release on any reachable prefix refutes runtime authorization. -/
theorem unlisted_prefix_refused {O : Operands} (p : ReleasePolicy O.Ty)
    {I : Instrumentation O} {P : Program O} {c d : Config O} {obs leaks rs choices}
    (prefixSteps : InstrumentedSteps I P c obs leaks rs choices d)
    (r : Release O.Ty O.Val) (hr : r ∈ rs) (unlisted : r.site ∉ p.allowed) :
    ¬ p.RunsAuthorized I P c :=
  fun authorized => unlisted_trace_refused p rs r hr unlisted (authorized.2 prefixSteps)

/-- Law: runtime authorization holds if every emitted release on a reachable prefix is listed. -/
theorem listed_prefixes_authorized {O : Operands} (p : ReleasePolicy O.Ty)
    (I : Instrumentation O) (P : Program O) (c : Config O)
    (qualified : ObserverQualified I)
    (listed : ∀ ⦃obs leaks rs choices d⦄, InstrumentedSteps I P c obs leaks rs choices d →
      ∀ r ∈ rs, r.site ∈ p.allowed) :
    p.RunsAuthorized I P c := by
  refine ⟨qualified, ?_⟩
  intro _ _ _ _ _ steps r member
  exact listed_site_permitted p r (listed steps r member)

/-- Law: low equivalence preserves public locals and the public-store relation. -/
theorem low_equivalent_public_state {O : Operands} (L : Labelling O) (c d : Config O) :
    LowEquivalent L c d → publicLocals L c = publicLocals L d ∧ L.lowStore c.store d.store := id

/-- Law: equality of public locals and related public stores suffice for low equivalence. -/
theorem public_state_low_equivalent {O : Operands} (L : Labelling O) (c d : Config O) :
    publicLocals L c = publicLocals L d → L.lowStore c.store d.store → LowEquivalent L c d :=
  And.intro

/-- Law: NI evidence retains machine qualification independently of the run comparison. -/
theorem ni_observer_qualified {O : Operands} {I : Instrumentation O}
    {P : Program O} {L : Labelling O} {c d : Config O} :
    DelimitedReleaseNI I P L c d → ObserverQualified I := And.left

/-- Law: CT evidence retains machine qualification independently of the run comparison. -/
theorem ct_observer_qualified {O : Operands} {I : Instrumentation O}
    {P : Program O} {L : Labelling O} {c d : Config O} :
    ConstantTime I P L c d → ObserverQualified I := And.left

/-- Law: reason and call-path facts do not strengthen the ordered release premise.
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7),
docs/decisions.md#decided-project-infoflow-2026-11-06. -/
theorem release_metadata_irrelevant {Ty Val : Type} (rs : List (ReleaseFact Ty Val))
    (reason : String) (path : List FnId) :
    ReleaseFact.semanticTrace (rs.map fun r => { r with reason := reason, path := path }) =
      ReleaseFact.semanticTrace rs := by
  simp [ReleaseFact.semanticTrace, releaseTrace, List.map_map]

/-- Law: runtime authorization requires the machine's observer. -/
theorem authorization_observer_qualified {O : Operands} {I : Instrumentation O}
    {policy : ReleasePolicy O.Ty} {P : Program O} {c : Config O} :
    policy.RunsAuthorized I P c → ObserverQualified I := And.left

/-- Law: any two qualified observers for a fixed operand machine are equal. -/
theorem qualified_observers_agree {O : Operands} {I J : Instrumentation O}
    (hi : ObserverQualified I) (hj : ObserverQualified J) : I = J :=
  hi.trans hj.symm

/-- Law: step observations are exactly the C1 emission, with the payload intact. -/
theorem emitted_observations {O : Operands} (c d : Config O) (l : Label O) :
    (machineInstrumentation O).observeStep c l d = l.events.map Observation.output := rfl

/-- Law: final observations are exactly the C1 outcome. -/
theorem final_observations {O : Operands} (c : Config O) :
    (machineInstrumentation O).observeFinal c = c.final?.map Observation.final := rfl

/-- Law: internal transitions contribute no external choice. -/
theorem internal_choices_empty {O : Operands} (c d : Config O) :
    (machineInstrumentation O).choices c .silent d none = [] := rfl

/-- Law: a deterministic primitive contributes no external choice. -/
theorem deterministic_primitive_choices {O : Operands} {c : Config O}
    {p : O.Prim} {vs : List O.Val} {site : SiteId} {st : O.Store} {a : Answer O.Val O.Event O.Store}
    (request : primitiveRequest c = some (p, vs, site, st))
    (unique : ∀ b, O.allows p vs st b = true → b = a) :
    externalChoices c (some a) = [] := by
  have noAlternative : ¬ ∃ b, b ≠ a ∧ O.allows p vs st b = true :=
    fun ⟨b, distinct, accepted⟩ => distinct (unique b accepted)
  simp [externalChoices, request, noAlternative]

/-- Law: an accepted oracle answer with an allowed alternative contributes that exact answer. -/
theorem nondeterministic_primitive_choices {O : Operands} {c : Config O}
    {p : O.Prim} {vs : List O.Val} {site : SiteId} {st : O.Store} {a : Answer O.Val O.Event O.Store}
    (request : primitiveRequest c = some (p, vs, site, st))
    (accepted : O.allows p vs st a = true)
    (alternative : ∃ b, b ≠ a ∧ O.allows p vs st b = true) :
    externalChoices c (some a) = [a] := by
  simp [externalChoices, request, accepted, alternative]

/-- Law: evaluated conditionals retain the machine's source site and decision. -/
theorem machine_conditional_leakage {O : Operands} (e : O.Expr) (s t : Exec.Stmt O)
    (site : SiteId) (locals : AList Var O.Val) (st next : O.Store) (v : O.Val)
    (frames : List (Frame O)) (label : Label O) (target : Config O)
    {b : Bool} (evaluated : O.eval locals st e = .ok (v, next)) (decision : O.test v = some b) :
    (machineInstrumentation O).leakage
      ⟨.exec (.cond e s t site), locals, st, frames⟩ label target none = [.branch site b] := by
  simp [machineInstrumentation, instrumentationWithChoices, controlLeakage, primitiveRequest, evaluated, decision]

/-- Law: evaluated loop tests retain the machine's site, decision and iteration contribution. -/
theorem machine_loop_leakage {O : Operands} (loop : LoopId) (region body : Exec.Stmt O)
    (e : O.Expr) (site : SiteId) (dom : List Var) (locals : AList Var O.Val) (st next : O.Store)
    (v : O.Val) (frames : List (Frame O)) (label : Label O) (target : Config O)
    {b : Bool} (evaluated : O.eval locals st e = .ok (v, next)) (decision : O.test v = some b) :
    (machineInstrumentation O).leakage
      ⟨.done, locals, st, .test loop region e body site dom :: frames⟩ label target none =
        [.branch site b, .loop loop (if b then 1 else 0)] := by
  simp [machineInstrumentation, instrumentationWithChoices, controlLeakage, primitiveRequest, evaluated, decision]

/-- Law: a primitive with one evaluated operand contributes exactly its classified type, actual
source site and actual operand value when classified as a release. -/
theorem release_value_agrees {O : Operands} (site : SiteId) (p : O.Prim) (args : List O.Expr)
    (locals : AList Var O.Val) (st next : O.Store) (v : O.Val) (label : Label O)
    (target : Config O) (dst : Option Var) (frames : List (Frame O))
    (evaluated : evalArgs O locals st args = .ok ([v], next)) :
    (machineInstrumentation O).releases ⟨.exec (.prim dst p args site), locals, st, frames⟩ label target =
      ((O.releaseType p).map fun ty => (⟨⟨site, ty⟩, v⟩ : Release O.Ty O.Val)).toList := by
  simp [machineInstrumentation, instrumentationWithChoices, primitiveRequest, evaluated]

/-- Law: runtime release sites agree with static extraction using the machine's classifier. -/
theorem release_sites_agree {O : Operands} (site : SiteId) (p : O.Prim) (args : List O.Expr)
    (locals : AList Var O.Val) (st next : O.Store) (v : O.Val) (label : Label O)
    (target : Config O) (dst : Option Var) (frames : List (Frame O))
    (evaluated : evalArgs O locals st args = .ok ([v], next)) :
    ((machineInstrumentation O).releases ⟨.exec (.prim dst p args site), locals, st, frames⟩ label target).map
      Release.site = statementReleases O.releaseType (.prim dst p args site : Exec.Stmt O) := by
  rw [release_value_agrees site p args locals st next v label target dst frames evaluated]
  unfold statementReleases
  cases O.releaseType p <;> rfl

/-- Law: an accepted classified release preserves the evaluated operand in the runtime trace.
The operand contract guarantees the singleton operand for every allowed release answer. -/
theorem executed_release_value {O : Operands} {P : Program O}
    (site : SiteId) (p : O.Prim) (args : List O.Expr)
    (locals : AList Var O.Val) (st next : O.Store) (vs : List O.Val)
    (dst : Option Var) (frames : List (Frame O)) (target : Config O)
    {k : Answer O.Val O.Event O.Store → Option (Config O)}
    {a : Answer O.Val O.Event O.Store} {ty : O.Ty}
    (dispatch : step P (⟨.exec (.prim dst p args site), locals, st, frames⟩ : Config O) =
      some (.request p vs site next k))
    (classified : O.releaseType p = some ty)
    (accepted : O.allows p vs next a = true)
    (_resumed : k a = some target) :
    ∃ v, vs = [v] ∧
      (machineInstrumentation O).releases ⟨.exec (.prim dst p args site), locals, st, frames⟩
        a.label target = [⟨⟨site, ty⟩, v⟩] := by
  obtain ⟨v, rfl⟩ := O.release.singleOperand classified accepted
  have evaluated : evalArgs O locals st args = .ok ([v], next) := by
    cases he : evalArgs O locals st args <;>
      simp [step, onRes, he] at dispatch
    rw [← dispatch.1, ← dispatch.2.1]
  refine ⟨v, rfl, ?_⟩
  rw [release_value_agrees site p args locals st next v a.label target dst frames evaluated, classified]
  rfl

/-- Law: incompatible public observations or lengths on a matched pair of prefixes refute NI. -/
theorem ni_prefixes_compatible {O : Operands} {I : Instrumentation O} {P : Program O}
    {L : Labelling O} {c₁ c₂ : Config O} :
    DelimitedReleaseNI I P L c₁ c₂ → NIPrefixComparison I P L c₁ c₂ :=
  fun ni => ni.2.2

/-- Law: incompatible leakage on a matched pair of prefixes refutes CT. -/
theorem ct_prefixes_compatible {O : Operands} {I : Instrumentation O} {P : Program O}
    {L : Labelling O} {c₁ c₂ : Config O} :
    ConstantTime I P L c₁ c₂ → TimingPrefixComparison I P L c₁ c₂ :=
  fun ct => ct.2.2

/-- Law: metadata projection preserves every typed site and value in order, including repeats.
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7). -/
theorem release_metadata_preserves_trace {Ty Val : Type} (facts : List (ReleaseFact Ty Val)) :
    ReleaseFact.semanticTrace facts = facts.map (fun f => (f.release.site, f.release.value)) := by
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

/-- Planted control for trace preservation: an empty reason cannot suppress a release. -/
theorem empty_reason_still_projects_release :
    let fact := { metadataFact with reason := "" }
    ReleaseFact.semanticTrace [fact] = [(fact.release.site, fact.release.value)] ∧
      reasonSensitiveTrace [fact] ≠ [(fact.release.site, fact.release.value)] :=
  ⟨release_metadata_preserves_trace _, by decide⟩

/-- Planted (law `release_metadata_irrelevant`, mutant `reasonSensitiveTrace`).
changing only reason metadata now changes the release comparison. -/
theorem release_metadata_planted :
    ¬ (reasonSensitiveTrace [{ metadataFact with reason := "", path := [Exec.Demo.logF] }] =
      reasonSensitiveTrace [metadataFact]) := by decide

namespace Demo

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

/-- Determinism also fixes the answer index retained by the observer. -/
theorem answered_step_det {P : Program ops} {c d₁ d₂ : Config ops} {l₁ l₂ : Label ops}
    {a₁ a₂ : Option (Answer Int Int Int)}
    (h₁ : AnsweredStep P c l₁ d₁ a₁) (h₂ : AnsweredStep P c l₂ d₂ a₂) :
    l₁ = l₂ ∧ d₁ = d₂ ∧ a₁ = a₂ := by
  obtain ⟨hl, hd⟩ := step_det h₁.toStep h₂.toStep
  refine ⟨hl, hd, ?_⟩
  cases h₁ with
  | silent h =>
    cases h₂ with
    | silent => rfl
    | prim h' _ _ => rw [h] at h'; cases h'
  | prim h ha _ =>
    cases h₂ with
    | silent h' => rw [h] at h'; cases h'
    | prim h' ha' _ =>
      rw [h] at h'
      cases h'
      exact congrArg some (allowed_unique ha ha')

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
theorem final_no_step {P : Program ops} {c : Config ops} {e : Final Int}
    (hf : c.final? = some e) : step P c = none := by
  obtain ⟨ctrl, locals, store, frames⟩ := c
  cases ctrl with
  | exiting x => cases x <;> cases frames <;> simp_all [Config.final?, step, stepExit]
  | _ => cases frames <;> simp_all [Config.final?, step]

/-- Two finite executions of the deterministic machine that reach final configurations agree. -/
private theorem final_execution_unique {I : Instrumentation ops} {P : Program ops}
    {c d f : Config ops} {a b : ExecutionTrace ops} {ed ef : Final Int}
    (left : InstrumentedExecution I P c a d) (right : InstrumentedExecution I P c b f)
    (hd : d.final? = some ed) (hf : f.final? = some ef) :
    a = b ∧ d = f := by
  induction left generalizing b f with
  | refl c =>
    cases right with
    | refl => exact ⟨rfl, rfl⟩
    | step h _ => exact (no_step_of_none (P := P) (final_no_step hd) _ _ h.toStep).elim
  | step h _ ih =>
    cases right with
    | refl => exact (no_step_of_none (P := P) (final_no_step hf) _ _ h.toStep).elim
    | step h' rest =>
      obtain ⟨rfl, rfl, rfl⟩ := answered_step_det h h'
      obtain ⟨rfl, rfl⟩ := ih rest hd hf
      exact ⟨rfl, rfl⟩

/-- Helper: deterministic complete runs have identical traces and endpoint, for any observer. -/
theorem complete_unique {I : Instrumentation ops} {P : Program ops} {c : Config ops}
    (a b : Run I P c) : a.observations = b.observations ∧ a.leakage = b.leakage ∧
      a.releases = b.releases ∧ a.choices = b.choices ∧ a.final = b.final := by
  obtain ⟨trace, endpoint⟩ := final_execution_unique a.steps b.steps a.finalEnding b.finalEnding
  exact ⟨congrArg ExecutionTrace.observations trace, congrArg ExecutionTrace.leakage trace,
    congrArg ExecutionTrace.releases trace, congrArg ExecutionTrace.choices trace, endpoint⟩

/-- A reachable prefix can be canceled from an execution to a terminal configuration. -/
private theorem terminal_suffix {I : Instrumentation ops} {P : Program ops}
    {c d f : Config ops} {trace full : ExecutionTrace ops}
    (pre : InstrumentedExecution I P c trace d)
    (run : InstrumentedExecution I P c full f)
    (terminal : step P f = none) :
    ∃ tail, InstrumentedExecution I P d tail f ∧ full = trace.append tail := by
  induction pre generalizing full with
  | refl => exact ⟨_, run, by cases full; rfl⟩
  | step h _ ih =>
    cases run with
    | refl => exact (no_step_of_none terminal _ _ h.toStep).elim
    | step h' rest =>
      obtain ⟨rfl, rfl, rfl⟩ := answered_step_det h h'
      obtain ⟨tail, suffix, rfl⟩ := ih rest
      exact ⟨tail, suffix, by simp [ExecutionTrace.append, List.append_assoc]⟩

/-- The reachable trace projections each prefix the corresponding terminal trace projections. -/
theorem terminal_execution_suffix {I : Instrumentation ops} {P : Program ops}
    {c d f : Config ops} {obs leaks rs choices fullObs fullLeaks fullRs fullChoices}
    (pre : InstrumentedSteps I P c obs leaks rs choices d)
    (full : InstrumentedSteps I P c fullObs fullLeaks fullRs fullChoices f)
    (terminal : step P f = none) :
    ∃ tailObs tailLeaks tailRs tailChoices,
      InstrumentedSteps I P d tailObs tailLeaks tailRs tailChoices f ∧
      fullObs = obs ++ tailObs ∧ fullLeaks = leaks ++ tailLeaks ∧
      fullRs = rs ++ tailRs ∧ fullChoices = choices ++ tailChoices := by
  obtain ⟨⟨tailObs, tailLeaks, tailRs, tailChoices⟩, suffix, trace⟩ :=
    terminal_suffix pre full terminal
  cases trace
  exact ⟨tailObs, tailLeaks, tailRs, tailChoices, suffix, rfl, rfl, rfl, rfl⟩

/-- Authorization of a known complete trace covers every reachable prefix of that
deterministic execution, including prefixes ending before the final outcome. -/
theorem prefixes_authorized_of_complete {I : Instrumentation ops} {P : Program ops}
    {c : Config ops} (p : ReleasePolicy Unit) (r : Run I P c)
    (authorized : p.Authorized r.releases) : p.PrefixesAuthorized I P c := by
  intro obs leaks rs choices d pre release member
  obtain ⟨_, _, _, _, _, _, _, hr, _⟩ := terminal_execution_suffix pre r.steps (final_no_step r.finalEnding)
  exact permits_listed_site p release (authorized release (hr ▸ List.mem_append_left _ member))

/-- The two observed projections of a finite prefix are prefixes of a known complete run. -/
theorem traces_prefix_of_complete {I : Instrumentation ops} {P : Program ops}
    {c d : Config ops} {obs leaks rs choices}
    (pre : InstrumentedSteps I P c obs leaks rs choices d) (r : Run I P c) :
    obs <+: r.observations ∧ leaks <+: r.leakage := by
  obtain ⟨o, l, _, _, _, ho, hl, _, _⟩ :=
    terminal_execution_suffix pre r.steps (final_no_step r.finalEnding)
  exact ⟨⟨o, ho.symm⟩, ⟨l, hl.symm⟩⟩

/-- On the deterministic demo, equal complete public traces imply compatibility of all prefixes. -/
theorem ni_prefixes_of_complete {I : Instrumentation ops} {P : Program ops}
    {L : Labelling ops} {c₁ c₂ : Config ops} (r₁ : Run I P c₁) (r₂ : Run I P c₂)
    (outputs : r₁.observations = r₂.observations)
    (lengths : publicLengths r₁.leakage = publicLengths r₂.leakage) :
    NIPrefixComparison I P L c₁ c₂ := by
  intro o₁ l₁ rs₁ x₁ d₁ o₂ l₂ rs₂ x₂ d₂ pre₁ pre₂ _ _ _
  obtain ⟨ho₁, hl₁⟩ := traces_prefix_of_complete pre₁ r₁
  obtain ⟨ho₂, hl₂⟩ := traces_prefix_of_complete pre₂ r₂
  have hp₁ : publicLengths l₁ <+: publicLengths r₁.leakage := hl₁.filterMap _
  have hp₂ : publicLengths l₂ <+: publicLengths r₂.leakage := hl₂.filterMap _
  exact ⟨List.prefix_or_prefix_of_prefix (outputs ▸ ho₁) ho₂,
    List.prefix_or_prefix_of_prefix (lengths ▸ hp₁) hp₂⟩

/-- On the deterministic demo, equal complete leakage implies compatibility of all prefixes. -/
theorem timing_prefixes_of_complete {I : Instrumentation ops} {P : Program ops}
    {L : Labelling ops} {c₁ c₂ : Config ops} (r₁ : Run I P c₁) (r₂ : Run I P c₂)
    (leakage : r₁.leakage = r₂.leakage) : TimingPrefixComparison I P L c₁ c₂ := by
  intro o₁ l₁ rs₁ x₁ d₁ o₂ l₂ rs₂ x₂ d₂ pre₁ pre₂ _ _ _
  exact List.prefix_or_prefix_of_prefix (leakage ▸ (traces_prefix_of_complete pre₁ r₁).2)
    (traces_prefix_of_complete pre₂ r₂).2

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
  let releaseAnswer : Answer Int Int Int := ⟨none, .ok 0, secret⟩
  let outputAnswer : Answer Int Int Int := ⟨some (value locals secret result), .ok 0, secret⟩
  refine ⟨c₄, .normal, rfl,
    I.observeStep c₀ .silent c₁ ++ I.observeStep c₁ .silent c₂ ++ I.observeStep c₂ .silent c₃ ++ I.observeStep c₃ l c₄,
    I.leakage c₀ .silent c₁ none ++ I.leakage c₁ .silent c₂ (some releaseAnswer) ++ I.leakage c₂ .silent c₃ none ++ I.leakage c₃ l c₄ (some outputAnswer),
    I.releases c₀ .silent c₁ ++ I.releases c₁ .silent c₂ ++ I.releases c₂ .silent c₃ ++ I.releases c₃ l c₄,
    I.choices c₀ .silent c₁ none ++ I.choices c₁ .silent c₂ (some releaseAnswer) ++ I.choices c₂ .silent c₃ none ++ I.choices c₃ l c₄ (some outputAnswer), ?_⟩
  simp only [List.append_assoc]
  apply InstrumentedSteps.step (c := c₀) (c' := c₁) (.silent rfl)
  apply InstrumentedSteps.step (c := c₁) (c' := c₂) (AnsweredStep.prim (O := ops) (P := program) (c := c₁) (c' := c₂) (p := .release) (site := ⟨0⟩)
    (vs := [value locals secret released]) (st := secret)
    (a := (⟨none, .ok 0, secret⟩ : Answer Int Int Int))
    (by dsimp [c₁, afterSeq, step, evalArgs, ops, opsWith]; cases released <;> rfl) (by simp [allowed]) rfl)
  apply InstrumentedSteps.step (c := c₂) (c' := c₃) (.silent rfl)
  simpa only [List.append_nil, Answer.label, l] using InstrumentedSteps.step (I := I)
    (AnsweredStep.prim (P := program) (c := c₃) (c' := c₄) (p := .output) (site := ⟨1⟩)
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
      r.choices = [] ∧
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
    simpa [releaseTrace, releaseValue, releaseAt] using hreleases
  refine ⟨instrumentation_qualified, ?_, ?_⟩
  · intro r₁ r₂ _ _ _
    have h₁ := secure_run_unique s₁ hidden₁ r₁
    have h₂ := secure_run_unique s₂ hidden₂ r₂
    constructor
    · rw [Run.publicObservations, Run.publicObservations, h₁.1, h₂.1, h₁.2.2.2.2, h₂.2.2.2.2, hsum]
      rfl
    · simp [h₁.2.1, h₂.2.1, publicLengths]
  · exact ni_prefixes_of_complete (secureRun s₁ hidden₁) (secureRun s₂ hidden₂) (by rw [show (secureRun s₁ hidden₁).observations = [.output (s₁ + Int.ofNat hidden₁)] from rfl, hsum]; rfl) rfl

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
    simpa [releaseTrace, releaseValue, releaseAt] using hreleases
  refine ⟨instrumentation_qualified, ?_, ?_⟩
  · intro r₁ r₂ _ _ _
    have h₁ := secure_run_unique s₁ hidden₁ r₁
    have h₂ := secure_run_unique s₂ hidden₂ r₂
    rw [h₁.2.1, h₂.2.1, hsum]
  · exact timing_prefixes_of_complete (secureRun s₁ hidden₁) (secureRun s₂ hidden₂) (by change [Leakage.variableTime ⟨0⟩ [s₁ + Int.ofNat hidden₁], .variableTime ⟨1⟩ [s₁ + Int.ofNat hidden₁]] = _; rw [hsum]; rfl)

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
      r.releases = [releaseZero] ∧ r.choices = [] ∧
      r.final = ⟨.done, [], secret, []⟩ := complete_unique r (ctRun secret)

/-- Representation lemma. Concrete projection check showing secureRun 1 maps via `InstrumentedSteps.toSteps` to an underlying C1 `Steps` derivation.

The concrete instrumented run projects to C1, not to a second semantics. -/
theorem bound_execution : ∃ events, Steps program (start 1) events (finished 1 (hiddenLocals 0)) :=
  (secureRun 1).steps.toSteps

/-- One complete seven-step timing execution. -/
def timingRun (secret : Int) : Run instrumentation program (timingStart secret) where
  final := finished secret
  ending := .normal
  finalEnding := rfl
  observations := [.output 7]
  leakage := [.variableTime ⟨2⟩ [secret], .variableTime ⟨0⟩ [0], .variableTime ⟨1⟩ [7]]
  releases := [releaseZero]
  choices := []
  steps :=
    InstrumentedSteps.step (I := instrumentation)
      (AnsweredStep.silent (P := program) (c := timingStart secret) (c' := timingAfterSeq secret) rfl) <|
    InstrumentedSteps.step (I := instrumentation)
      (AnsweredStep.prim (O := ops) (P := program) (c := timingAfterSeq secret)
        (c' := timingAfterWork secret) (p := .work) (vs := [secret]) (st := secret)
        (a := (⟨none, .ok 0, secret⟩ : Answer Int Int Int)) rfl (by simp [allowed]) rfl) <|
    InstrumentedSteps.step (I := instrumentation)
      (AnsweredStep.silent (P := program) (c := timingAfterWork secret)
        (c' := timingBeforeBody secret) rfl) <|
    InstrumentedSteps.step (I := instrumentation)
      (AnsweredStep.silent (P := program) (c := timingBeforeBody secret)
        (c' := afterSeq (.lit 0) (.lit 7) secret) rfl) <|
    InstrumentedSteps.step (I := instrumentation)
      (AnsweredStep.prim (O := ops) (P := program) (c := afterSeq (.lit 0) (.lit 7) secret)
        (c' := afterRelease (.lit 7) secret) (p := .release) (vs := [0]) (st := secret)
        (a := (⟨none, .ok 0, secret⟩ : Answer Int Int Int)) rfl (by simp [allowed]) rfl) <|
    InstrumentedSteps.step (I := instrumentation)
      (AnsweredStep.silent (P := program) (c := afterRelease (.lit 7) secret)
        (c' := beforeOutput (.lit 7) secret) rfl) <|
    InstrumentedSteps.step (I := instrumentation)
      (AnsweredStep.prim (O := ops) (P := program) (c := beforeOutput (.lit 7) secret)
        (c' := finished secret) (p := .output) (vs := [7]) (st := secret)
        (a := (⟨some 7, .ok 0, secret⟩ : Answer Int Int Int)) rfl (by simp [allowed]) rfl) <|
    InstrumentedSteps.refl (I := instrumentation) (P := program) (finished secret)

/-- Helper: every timing run has the canonical traces by complete-run determinism. -/
theorem timing_run_unique (secret : Int)
    (r : Run instrumentation program (timingStart secret)) :
    r.observations = [.output 7] ∧
      r.leakage = [.variableTime ⟨2⟩ [secret], .variableTime ⟨0⟩ [0], .variableTime ⟨1⟩ [7]] ∧
      r.releases = [releaseZero] ∧ r.choices = [] ∧
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
  choices := []
  steps :=
    InstrumentedSteps.step (I := instrumentation)
      (AnsweredStep.silent (P := program) (c := leakyTimingStart secret) (c' := leakyTimingAfterSeq secret) rfl) <|
    InstrumentedSteps.step (I := instrumentation)
      (AnsweredStep.prim (O := ops) (P := program) (c := leakyTimingAfterSeq secret)
        (c' := leakyTimingAfterWork secret) (p := .work) (vs := [secret]) (st := secret)
        (a := (⟨none, .ok 0, secret⟩ : Answer Int Int Int)) rfl (by simp [allowed]) rfl) <|
    InstrumentedSteps.step (I := instrumentation)
      (AnsweredStep.silent (P := program) (c := leakyTimingAfterWork secret)
        (c' := leakyTimingBeforeBody secret) rfl) <|
    InstrumentedSteps.step (I := instrumentation)
      (AnsweredStep.silent (P := program) (c := leakyTimingBeforeBody secret)
        (c' := afterSeq (.lit 0) .secret secret) rfl) <|
    InstrumentedSteps.step (I := instrumentation)
      (AnsweredStep.prim (O := ops) (P := program) (c := afterSeq (.lit 0) .secret secret)
        (c' := afterRelease .secret secret) (p := .release) (vs := [0]) (st := secret)
        (a := (⟨none, .ok 0, secret⟩ : Answer Int Int Int)) rfl (by simp [allowed]) rfl) <|
    InstrumentedSteps.step (I := instrumentation)
      (AnsweredStep.silent (P := program) (c := afterRelease .secret secret)
        (c' := beforeOutput .secret secret) rfl) <|
    InstrumentedSteps.step (I := instrumentation)
      (AnsweredStep.prim (O := ops) (P := program) (c := beforeOutput .secret secret)
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
  refine ⟨instrumentation_qualified, ?_, ?_⟩
  · intro r₁ r₂ _ _ _
    have h₁ := timing_run_unique s₁ r₁
    have h₂ := timing_run_unique s₂ r₂
    constructor
    · rw [Run.publicObservations, Run.publicObservations, h₁.1, h₂.1, h₁.2.2.2.2, h₂.2.2.2.2]
      rfl
    · simp [h₁.2.1, h₂.2.1, publicLengths]
  · exact ni_prefixes_of_complete (timingRun s₁) (timingRun s₂) rfl rfl

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
  have hl := hct.2.1 (timingRun s₁) (timingRun s₂) hlow rfl rfl
  simp [timingRun] at hl
  exact hs hl

/-- Law: identical timing inputs satisfy CT even when they execute variable-time work.
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7). -/
theorem same_timing_input_ct (secret : Int) :
    ConstantTime instrumentation program labelling (timingStart secret) (timingStart secret) :=
  ⟨instrumentation_qualified, (fun a b _ _ _ => (complete_unique a b).2.1),
    timing_prefixes_of_complete (timingRun secret) (timingRun secret) rfl⟩

/-- A deliberately permissive output oracle admits two distinct labels for one machine request. -/
def permissiveAllowed (p : Prim) (vs : List Int) (st : Int) (a : Answer Int Int Int) : Bool :=
  match p, vs with
  | .output, [_] => true
  | _, _ => allowed p vs st a

/-- Representation lemma. Concrete order-sensitivity check showing secureRun 1's leakage list is not equal to its reversal.

Planted trace mutations: reversed leakage and a missing final observation are detectable. -/
theorem leakage_order_matters : (secureRun 1).leakage ≠ [.variableTime ⟨1⟩ [1], .variableTime ⟨0⟩ [1]] := by
  intro h
  change [Leakage.variableTime ⟨0⟩ [1], .variableTime ⟨1⟩ [1]] =
    [.variableTime ⟨1⟩ [1], .variableTime ⟨0⟩ [1]] at h
  cases h

/-- Representation lemma. Concrete finality check showing `Run.publicObservations` includes .final .normal beyond the step output. -/
theorem public_output_uses_released_sum : (secureRun 1).publicObservations ≠ [.output 1] := by decide

/-- Representation lemma. Concrete non-finality check showing the intermediate `afterRelease` state has final? = none.

A nonfinal execution prefix is not a complete run. -/
theorem release_transition_is_nonterminal : ¬ ∃ ending, (afterRelease .secret 1).final? = some ending := by
  simp [afterRelease, Config.final?]

/-- A configuration with one secret and one public local. -/
def mixedConfig (secret publicValue : Int) : Config ops :=
  ⟨.done, [(⟨0⟩, secret), (⟨1⟩, publicValue)], 0, []⟩

/-- Instance. Concrete `LowEquivalent` acceptance check when only negative (secret) local bindings differ.

Secret local changes are invisible; a planted public local change rejects low-equivalence. -/
theorem secret_local_change_is_low_equivalent : LowEquivalent labelling (mixedConfig (-1) 7) (mixedConfig (-2) 7) := by
  simp [LowEquivalent, publicLocals, labelling, mixedConfig]

/-- Instance. Concrete `LowEquivalent` refusal check when non-negative (public) local bindings differ. -/
theorem public_local_change_breaks_low_equivalence : ¬ LowEquivalent labelling (mixedConfig (-1) 7) (mixedConfig (-2) 8) := by
  simp [LowEquivalent, publicLocals, labelling, mixedConfig]

/-- A store-sensitive labelling supplies the corresponding public-store negative control. -/
abbrev storeSensitiveLabelling : Labelling ops where
  PublicValue := Int
  project := fun _ => none
  lowStore := fun s₁ s₂ => s₁ = s₂

/-- Instance. Concrete `LowEquivalent` refusal check under `storeSensitiveLabelling` when store states differ. -/
theorem public_store_change_breaks_low_equivalence : ¬ LowEquivalent storeSensitiveLabelling (leakyStart 1) (leakyStart 2) := by
  simp [LowEquivalent, storeSensitiveLabelling, leakyStart]

/-- Helper (computation). Evaluates `publicLocals` on `Demo.leakyStart` to show secret store states are `LowEquivalent` under `Demo.labelling`. -/
theorem leaky_low (s₁ s₂ : Int) : LowEquivalent labelling (leakyStart s₁) (leakyStart s₂) := by
  simp [LowEquivalent, publicLocals, labelling, leakyStart]

/-- Helper (computation). Checks that every run from `Demo.leakyStart` emits only releases permitted by `Demo.policy`. -/
theorem release_authorized (secret : Int) :
    policy.RunsAuthorized instrumentation program (leakyStart secret) := by
  refine ⟨instrumentation_qualified, ?_⟩
  apply prefixes_authorized_of_complete policy (leakyRun secret)
  change policy.Authorized [releaseValue 0]
  simp [ReleasePolicy.Authorized, ReleasePolicy.Permits, releaseValue, releaseAt]

/-- Helper (computation). Checks that every run from `Demo.ctStart` emits only releases permitted by `Demo.policy`. -/
theorem ct_release_authorized (secret : Int) :
    policy.RunsAuthorized instrumentation program (ctStart secret) := by
  refine ⟨instrumentation_qualified, ?_⟩
  apply prefixes_authorized_of_complete policy (ctRun secret)
  change policy.Authorized [releaseValue 0]
  simp [ReleasePolicy.Authorized, ReleasePolicy.Permits, releaseValue, releaseAt]

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
  have hobs := (hni.2.1 (leakyRun s₁) (leakyRun s₂) hlow rfl rfl).1
  change [Observation.output s₁, .final .normal] = [.output s₂, .final .normal] at hobs
  exact hs (Observation.output.inj (List.cons.inj hobs).1)

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
  have hleak := hct.2.1 (leakyRun s₁) (leakyRun s₂) hlow rfl rfl
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

/-- Planted: requiring a release wrongly refuses an empty trace under the empty policy. -/
def requiresRelease {Ty Val : Type} (policy : ReleasePolicy Ty) (rs : List (Release Ty Val)) : Prop :=
  rs ≠ [] ∧ policy.Authorized rs

/-- Instance and planted control for `listed_trace_authorized`. -/
theorem requires_release_refuses_empty_trace :
    (ReleasePolicy.mk ([] : List (ReleaseSite Unit))).Authorized ([] : List (Release Unit Int)) ∧
    ¬ requiresRelease (ReleasePolicy.mk ([] : List (ReleaseSite Unit))) ([] : List (Release Unit Int)) :=
  ⟨listed_trace_authorized _ _ (by simp), fun h => h.1 rfl⟩

/-- Instance (law `forbidsRelease_not_authorized`). -/
theorem empty_policy_refuses_release : ¬ (ReleasePolicy.mk ([] : List (ReleaseSite Unit))).Authorized [Demo.releaseZero] :=
  forbidsRelease_not_authorized (ReleasePolicy.mk []) Demo.releaseZero [] rfl

/-- Planted (law `forbidsRelease_not_authorized`, subject `ReleasePolicy.Authorized`, mutant `ignoresPolicyAuthorized`). -/
theorem ignores_policy_accepts_release : (ReleasePolicy.mk ([] : List (ReleaseSite Unit))).ForbidsRelease ∧
    ¬ (¬ ignoresPolicyAuthorized (ReleasePolicy.mk ([] : List (ReleaseSite Unit)))
      [Demo.releaseZero]) :=
  ⟨rfl, fun h => h (fun _ _ => True.intro)⟩

open Demo

/-- Representation lemma: a concrete control for a function-backed identity, not Law evidence. -/
theorem allowed_identity_step : ∃ (l : Label (opsWith allowed)) (c' : Config (opsWith allowed)),
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
theorem permissive_identity_adds_step : (∃ (l : Label (opsWith permissiveAllowed)) (c' : Config (opsWith permissiveAllowed)),
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

/-- Instance (law `secure_ni`). -/
theorem masked_secrets_ni : DelimitedReleaseNI instrumentation program labelling (start 6 1) (start 5 2) :=
  secure_ni 6 5 1 2 ⟨rfl, trivial⟩ ⟨secureRun 6 1, secureRun 5 2, rfl, rfl⟩

/-- Planted (law `secure_ni`, subject `ConcreteSpec.Core.Leak.Demo.start`, mutant `exposingStart`). -/
theorem exposing_start_breaks_ni : LowEquivalent labelling (exposingStart 6 1) (exposingStart 5 2) ∧
    (∃ (r₁ : Run instrumentation program (exposingStart 6 1))
       (r₂ : Run instrumentation program (exposingStart 5 2)),
       r₁.choices = r₂.choices ∧ releaseTrace r₁.releases = releaseTrace r₂.releases) ∧
    ¬ DelimitedReleaseNI instrumentation program labelling (exposingStart 6 1) (exposingStart 5 2) := by
  refine ⟨⟨rfl, trivial⟩, ⟨exposingRun 6 1, exposingRun 5 2, rfl, rfl⟩, ?_⟩
  intro h
  have hobs := h.2.1 (exposingRun 6 1) (exposingRun 5 2) ⟨rfl, trivial⟩ rfl rfl
  exact (by decide : (exposingRun 6 1).publicObservations ≠ (exposingRun 5 2).publicObservations) hobs.1

/-- Instance (law `secure_ct`). -/
theorem masked_secrets_ct : ConstantTime instrumentation program labelling (start 6 1) (start 5 2) :=
  secure_ct 6 5 1 2 ⟨rfl, trivial⟩ ⟨secureRun 6 1, secureRun 5 2, rfl, rfl⟩

/-- Planted (law `secure_ct`, subject `ConcreteSpec.Core.Leak.Demo.start`, mutant `exposingStart`). -/
theorem exposing_start_breaks_ct : LowEquivalent labelling (exposingStart 6 1) (exposingStart 5 2) ∧
    (∃ (r₁ : Run instrumentation program (exposingStart 6 1))
       (r₂ : Run instrumentation program (exposingStart 5 2)),
       r₁.choices = r₂.choices ∧ releaseTrace r₁.releases = releaseTrace r₂.releases) ∧
    ¬ ConstantTime instrumentation program labelling (exposingStart 6 1) (exposingStart 5 2) := by
  refine ⟨⟨rfl, trivial⟩, ⟨exposingRun 6 1, exposingRun 5 2, rfl, rfl⟩, ?_⟩
  intro h
  have hleak := h.2.1 (exposingRun 6 1) (exposingRun 5 2) ⟨rfl, trivial⟩ rfl rfl
  change [Leakage.variableTime ⟨0⟩ [7], .variableTime ⟨1⟩ [6]] =
    [.variableTime ⟨0⟩ [7], .variableTime ⟨1⟩ [5]] at hleak
  cases hleak

/-- Instance (law `timing_ni`, case `timingStart`). -/
theorem timing_pair_ni : DelimitedReleaseNI instrumentation program labelling (timingStart 1) (timingStart 2) :=
  timing_ni 1 2 ⟨rfl, trivial⟩ ⟨timingRun 1, timingRun 2, rfl, rfl⟩

/-- Planted (law `timing_ni`, case `timingStart`, subject `timingStart`, mutant `leakyTimingStart`). -/
theorem leaky_timing_start_breaks_ni : LowEquivalent labelling (leakyTimingStart 1) (leakyTimingStart 2) ∧
    (∃ (r₁ : Run instrumentation program (leakyTimingStart 1))
       (r₂ : Run instrumentation program (leakyTimingStart 2)),
       r₁.choices = r₂.choices ∧ releaseTrace r₁.releases = releaseTrace r₂.releases) ∧
    ¬ DelimitedReleaseNI instrumentation program labelling (leakyTimingStart 1) (leakyTimingStart 2) := by
  refine ⟨⟨rfl, trivial⟩, ⟨leakyTimingRun 1, leakyTimingRun 2, rfl, rfl⟩, ?_⟩
  intro h
  have hobs := h.2.1 (leakyTimingRun 1) (leakyTimingRun 2) ⟨rfl, trivial⟩ rfl rfl
  exact (by decide : (leakyTimingRun 1).publicObservations ≠ (leakyTimingRun 2).publicObservations) hobs.1

/-- Instance: secret-dependent work refutes constant time even with public output. -/
theorem timing_pair_not_ct : ¬ ConstantTime instrumentation program labelling (timingStart 1) (timingStart 2) :=
  timing_not_ct 1 2 ⟨rfl, trivial⟩ (by decide)

/-- Representation lemma: `ctRun 1` and `ctRun 2` inhabit all CT and NI premises:
low equivalence, equal choices and equal releases for `ctStart`. -/
theorem distinct_ct_entries : ctStart 1 ≠ ctStart 2 ∧
    LowEquivalent labelling (ctStart 1) (ctStart 2) ∧
    (ctRun 1).choices = (ctRun 2).choices ∧
    (ctRun 1).releases = (ctRun 2).releases := by
  refine ⟨?_, ⟨rfl, trivial⟩, rfl, rfl⟩
  intro h
  have hs := congrArg Config.store h
  exact (by decide : (1 : Int) ≠ 2) hs

/-- Instance: authorized release cannot excuse an unreleased public output. -/
theorem authorized_pair_not_ni : ¬ DelimitedReleaseNI instrumentation program labelling (leakyStart 1) (leakyStart 2) :=
  authorized_release_not_ni 1 2 (release_authorized 1) (release_authorized 2)
    (leaky_low 1 2) (by decide)

/-- Instance: authorized release cannot excuse unreleased timing leakage. -/
theorem authorized_pair_not_ct : ¬ ConstantTime instrumentation program labelling (leakyStart 1) (leakyStart 2) :=
  authorized_release_not_ct 1 2 (release_authorized 1) (release_authorized 2)
    (leaky_low 1 2) (by decide)

/-- Planted: require equality of the raw store in addition to the security judgment. -/
def requiresSameStore (judgment : Config Demo.ops → Config Demo.ops → Prop) (a b : Config Demo.ops) : Prop :=
  a.store = b.store ∧ judgment a b

/-- Planted control: raw-store equality wrongly refuses both admitted masked-secret
comparisons and the timing pair admitted by NI. -/
theorem raw_store_equality_refuses_allowed_pairs :
    ¬ requiresSameStore (DelimitedReleaseNI instrumentation program labelling) (start 6 1) (start 5 2) ∧
    ¬ requiresSameStore (ConstantTime instrumentation program labelling) (start 6 1) (start 5 2) ∧
    ¬ requiresSameStore (DelimitedReleaseNI instrumentation program labelling) (timingStart 1) (timingStart 2) :=
  ⟨fun h => (by decide : (6 : Int) ≠ 5) h.1, fun h => (by decide : (6 : Int) ≠ 5) h.1,
    fun h => (by decide : (1 : Int) ≠ 2) h.1⟩

/-- Planted: require empty leakage even when CT already guarantees equal traces. -/
def requiresEmptyLeakage (a b : Config Demo.ops) : Prop :=
  ConstantTime instrumentation program labelling a b ∧
    ∀ r : Run instrumentation program a, r.leakage = []

/-- Instance and planted control for `same_timing_input_ct`: equal nonempty traces pass CT. -/
theorem empty_leakage_requirement_refuses_matched_work :
    ConstantTime instrumentation program labelling (timingStart 1) (timingStart 1) ∧
    ¬ requiresEmptyLeakage (timingStart 1) (timingStart 1) :=
  ⟨same_timing_input_ct 1, fun h => by have empty := h.2 (timingRun 1); simp [timingRun] at empty⟩

/-- Planted definition: omit only the public-output comparison, retaining all public-length requirements. -/
def blindNI {O : Operands} (I : Instrumentation O) (P : Program O)
    (L : Labelling O) (c₁ c₂ : Config O) : Prop :=
  ObserverQualified I ∧
    (∀ (r₁ : Run I P c₁) (r₂ : Run I P c₂),
      LowEquivalent L c₁ c₂ → r₁.choices = r₂.choices →
      releaseTrace r₁.releases = releaseTrace r₂.releases →
      publicLengths r₁.leakage = publicLengths r₂.leakage) ∧
    (∀ ⦃o₁ l₁ r₁ x₁ d₁ o₂ l₂ r₂ x₂ d₂⦄,
      InstrumentedSteps I P c₁ o₁ l₁ r₁ x₁ d₁ →
      InstrumentedSteps I P c₂ o₂ l₂ r₂ x₂ d₂ →
      LowEquivalent L c₁ c₂ → x₁ = x₂ → releaseTrace r₁ = releaseTrace r₂ →
      PrefixCompatible (publicLengths l₁) (publicLengths l₂))

/-- Planted definition: omit the leakage comparisons on complete runs and prefixes. -/
def blindCT {O : Operands} (I : Instrumentation O) (_P : Program O)
    (_L : Labelling O) (_c₁ _c₂ : Config O) : Prop :=
  ObserverQualified I

/-- Planted (law `timing_not_ct`, case `timingStart`, mutant `blindCT`).
The added leakage permission invalidates `timing_not_ct` with its premises inhabited. -/
theorem timing_absence_planted :
    LowEquivalent labelling (timingStart 1) (timingStart 2) ∧ (1 : Int) ≠ 2 ∧
    ¬ (¬ blindCT instrumentation program labelling (timingStart 1) (timingStart 2)) := by
  exact ⟨⟨rfl, trivial⟩, by decide, fun h => h instrumentation_qualified⟩

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
  refine ⟨instrumentation_qualified, ?_, ?_⟩
  · intro r₁ r₂ _ _ _
    rw [(complete_unique r₁ (leakyRun 1)).2.1, (complete_unique r₂ (leakyRun 2)).2.1]
    rfl
  · intro o₁ l₁ r₁ x₁ d₁ o₂ l₂ r₂ x₂ d₂ pre₁ pre₂ _ _ _
    have lengths₁ : publicLengths l₁ <+: [] := (traces_prefix_of_complete pre₁ (leakyRun 1)).2.filterMap _
    have lengths₂ : publicLengths l₂ <+: [] := (traces_prefix_of_complete pre₂ (leakyRun 2)).2.filterMap _
    exact List.prefix_or_prefix_of_prefix lengths₁ lengths₂

/-- Planted (law `authorized_release_not_ct`, case `leakyStart`, mutant `blindCT`).
the added leakage permission invalidates authorized_release_not_ct on the same runs. -/
theorem authorization_ct_planted :
    policy.RunsAuthorized instrumentation program (leakyStart 1) ∧
    policy.RunsAuthorized instrumentation program (leakyStart 2) ∧
    LowEquivalent labelling (leakyStart 1) (leakyStart 2) ∧ (1 : Int) ≠ 2 ∧
    ¬ (¬ blindCT instrumentation program labelling (leakyStart 1) (leakyStart 2)) := by
  exact ⟨release_authorized 1, release_authorized 2, leaky_low 1 2, by decide,
    fun h => h instrumentation_qualified⟩

/-- Planted observer. The secure program is unchanged; final shared secret becomes public. -/
abbrev secretSeeing : Instrumentation Demo.ops :=
  { instrumentation with observeFinal := fun c => some (.output c.store) }

/-- Helper (transport). A final observer cannot affect the instrumented step derivation. -/
private theorem secretSeeingSteps {c c' : Config Demo.ops} {trace : ExecutionTrace Demo.ops}
    (h : InstrumentedExecution instrumentation program c trace c') :
    InstrumentedExecution secretSeeing program c trace c' := by
  induction h with
  | refl => exact .refl _
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

/-- Planted (law `secure_ni`, mutant `secretSeeing`).
Same program and entries as the positive instance; only the observer is changed. -/
theorem secret_observer_breaks_ni : LowEquivalent labelling (start 6 1) (start 5 2) ∧
    (secretSeeingRun 6 1).choices = (secretSeeingRun 5 2).choices ∧
    releaseTrace (secretSeeingRun 6 1).releases = releaseTrace (secretSeeingRun 5 2).releases ∧
    ¬ NIComparison secretSeeing program labelling (start 6 1) (start 5 2) := by
  refine ⟨⟨rfl, trivial⟩, rfl, rfl, ?_⟩
  intro h
  have bad := (h.1 (secretSeeingRun 6 1) (secretSeeingRun 5 2) ⟨rfl, trivial⟩ rfl rfl).1
  exact (by decide : (secretSeeingRun 6 1).publicObservations ≠
    (secretSeeingRun 5 2).publicObservations) bad

/-- Planted observer. Secret state is misreported as public container length. -/
abbrev metadataSeeing : Instrumentation Demo.ops :=
  { instrumentation with leakage := fun c _ _ _ => [.publicLength ⟨9⟩ c.store.natAbs] }

/-- Planted execution. Only public-length metadata differs from the secure observer. -/
def metadataSeeingRun (secret : Int) (hidden : Nat := 0) : Run metadataSeeing program (start secret hidden) :=
  bodyRun metadataSeeing .masked .masked secret (hiddenLocals hidden)

/-- Planted (law `secure_ni`, mutant `metadataSeeing`).
NI rejects secret-dependent public metadata even when ordinary outputs still match. -/
theorem secret_length_breaks_ni : LowEquivalent labelling (start 6 1) (start 5 2) ∧
    (metadataSeeingRun 6 1).choices = (metadataSeeingRun 5 2).choices ∧
    releaseTrace (metadataSeeingRun 6 1).releases = releaseTrace (metadataSeeingRun 5 2).releases ∧
    ¬ NIComparison metadataSeeing program labelling (start 6 1) (start 5 2) := by
  refine ⟨⟨rfl, trivial⟩, rfl, rfl, ?_⟩
  intro h
  have bad := (h.1 (metadataSeeingRun 6 1) (metadataSeeingRun 5 2) ⟨rfl, trivial⟩ rfl rfl).2
  exact (by decide : publicLengths (metadataSeeingRun 6 1).leakage ≠
    publicLengths (metadataSeeingRun 5 2).leakage) bad

/-- Planted definition. Call-path text incorrectly filters semantic release comparison. -/
def pathSensitiveTrace {Ty Val : Type} (rs : List (ReleaseFact Ty Val)) :
    List (ReleaseSite Ty × Val) :=
  (rs.filter fun r => !r.path.isEmpty).map fun r => (r.release.site, r.release.value)

/-- Planted (law `release_metadata_irrelevant`, mutant `pathSensitiveTrace`). -/
theorem path_sensitive_trace_changes : ¬ (pathSensitiveTrace [{ metadataFact with path := [Exec.Demo.logF] }] =
    pathSensitiveTrace [metadataFact]) := by decide

/-- Instance (law `instrumentation_erasure`). -/
theorem secure_execution_erases : ∃ events, Steps program (start 6 1) events (secureRun 6 1).final :=
  (instrumentation_erasure instrumentation program _ _).mp
    ⟨_, _, _, _, (secureRun 6 1).steps⟩

/-- Instance: erasure also reconstructs instrumentation from the concrete C1 execution. -/
theorem secure_execution_instruments : ∃ obs leaks rs choices,
    InstrumentedSteps instrumentation program (start 6 1) obs leaks rs choices (secureRun 6 1).final :=
  (instrumentation_erasure instrumentation program _ _).mpr
    ((instrumentation_erasure instrumentation program _ _).mp
      ⟨_, _, _, _, (secureRun 6 1).steps⟩)

/-- Planted relation: only reflexivity is restricted, to non-final configurations.
All trace indices and the step constructor retain their production shape. -/
inductive RestrictedSteps {O : Operands} (I : Instrumentation O) (P : Program O) :
    Config O → List (Observation O.Event O.Val) → List (Leakage O.Val O.Address O.Effect) →
      List (Release O.Ty O.Val) → List (Answer O.Val O.Event O.Store) → Config O → Prop where
  | refl (c : Config O) (unfinished : c.final? = none) : RestrictedSteps I P c [] [] [] [] c
  | step {c l c' answer obs leaks rs choices c''} (h : AnsweredStep P c l c' answer)
      (rest : RestrictedSteps I P c' obs leaks rs choices c'') :
      RestrictedSteps I P c (I.observeStep c l c' ++ obs) (I.leakage c l c' answer ++ leaks)
        (I.releases c l c' ++ rs) (I.choices c l c' answer ++ choices) c''

/-- Planted (law `instrumentation_erasure`, mutant `RestrictedSteps`).
The erased reflexive run inhabits the reverse implication's premise. -/
theorem restricted_refl_loses_execution : (∃ events, Steps program (finished 1) events (finished 1)) ∧
    ¬ ((∃ obs leaks rs choices, RestrictedSteps instrumentation program
      (finished 1) obs leaks rs choices (finished 1)) ↔
      ∃ events, Steps program (finished 1) events (finished 1)) := by
  refine ⟨⟨[], .refl _⟩, ?_⟩
  intro h
  obtain ⟨_, _, _, _, hs⟩ := h.mpr ⟨[], .refl _⟩
  cases hs with
  | refl _ unfinished => cases unfinished
  | step hs _ => exact no_step_of_none (P := program) (c := finished 1) rfl _ _ hs.toStep

/-- Planted relation: the step rule alone admits the illicit final-to-entry transition.
Reflexivity, recursive trace indices and trace accumulation are unchanged. -/
inductive AdmittingSteps (I : Instrumentation Demo.ops) (P : Program Demo.ops) :
    Config Demo.ops → List (Observation Int Int) → List (Leakage Int Unit Unit) →
      List (Release Unit Int) → List (Answer Int Int Int) → Config Demo.ops → Prop where
  | refl (c) : AdmittingSteps I P c [] [] [] [] c
  | step {c l c' answer obs leaks rs choices c''}
      (h : AnsweredStep P c l c' answer ∨ (c = finished 1 ∧ c' = start 1))
      (rest : AdmittingSteps I P c' obs leaks rs choices c'') :
      AdmittingSteps I P c (I.observeStep c l c' ++ obs) (I.leakage c l c' answer ++ leaks)
        (I.releases c l c' ++ rs) (I.choices c l c' answer ++ choices) c''

/-- Planted (law `instrumentation_erasure`, mutant `AdmittingSteps`).
The added transition inhabits the forward implication's premise. -/
theorem illicit_step_adds_execution : (∃ obs leaks rs choices, AdmittingSteps instrumentation program
      (finished 1) obs leaks rs choices (start 1)) ∧
    ¬ ((∃ obs leaks rs choices, AdmittingSteps instrumentation program
      (finished 1) obs leaks rs choices (start 1)) ↔
      ∃ events, Steps program (finished 1) events (start 1)) := by
  have bad : ∃ obs leaks rs choices, AdmittingSteps instrumentation program
      (finished 1) obs leaks rs choices (start 1) :=
    ⟨_, _, _, _, .step (l := .silent) (answer := none) (Or.inr ⟨rfl, rfl⟩) (.refl _)⟩
  refine ⟨bad, ?_⟩
  intro h
  obtain ⟨_, hs⟩ := h.mp bad
  cases hs with
  | step hs _ => exact no_step_of_none (P := program) (c := finished 1) rfl _ _ hs

/-- Instance (law `release_metadata_irrelevant`).
Reasons and paths are connected to an actual run while remaining outside its release premise. -/
theorem run_facts_keep_release_trace : ReleaseFact.semanticTrace
    (ReleaseFact.attach (secureRun 6 1).releases
      (fun _ => "changed reason") (fun _ => [Exec.Demo.logF])).entries =
    releaseTrace (secureRun 6 1).releases := RunFacts.trace_eq _


end Audit
end ConcreteSpec.Core.Leak

namespace ConcreteSpec.Core.Leak.ControlAudit
open _root_.ConcreteSpec.Core.Exec
open _root_.ConcreteSpec.Core.Leak.Demo

/-- Helper: prepend one actual C1 transition. -/
def prepend {I : Instrumentation ops} {c d : Config ops} {l : Label ops}
    {answer : Option (Answer Int Int Int)}
    (h : AnsweredStep program c l d answer) (r : Run I program d) : Run I program c :=
  ⟨r.final, r.ending, r.finalEnding, I.observeStep c l d ++ r.observations,
   I.leakage c l d answer ++ r.leakage, I.releases c l d ++ r.releases,
   I.choices c l d answer ++ r.choices, .step h r.steps⟩
/-- Helper: complete normal execution at an empty continuation. -/
def doneRun (I : Instrumentation ops) (s : Int) : Run I program (finished s) :=
  ⟨finished s, .normal, rfl, [], [], [], [], .refl _⟩
/-- Helper: a literal output at source site eight. -/
def out (n : Int) : ConcreteSpec.Core.Exec.Stmt ops := .prim none .output [.lit n] ⟨8⟩
/-- Helper: asymmetric secret branching with distinct public outputs. -/
def entry (s : Int) : Config ops :=
  ⟨.exec (.cond .secret (.seq .skip (out 1)) (out 0) ⟨7⟩), [], s, []⟩
/-- Helper: the deterministic primitive output transition. -/
def outputRun (I : Instrumentation ops) (s n : Int) :
    Run I program ⟨.exec (out n), [], s, []⟩ :=
  prepend (AnsweredStep.prim (vs := [n]) (st := s)
    (a := (⟨some n, .ok 0, s⟩ : Answer Int Int Int)) rfl (by simp [allowed]) rfl) (doneRun I s)
/-- Helper: the false arm takes two C1 steps. -/
def left (I : Instrumentation ops) : Run I program (entry 0) :=
  prepend (.silent rfl) (outputRun I 0 0)
/-- Helper: the true arm takes five C1 steps. -/
def right (I : Instrumentation ops) : Run I program (entry 1) :=
  prepend (.silent rfl) (prepend (.silent rfl)
    (prepend (.silent rfl) (prepend (.silent rfl) (outputRun I 1 1))))
/-- Helper: secret branching with identical silent arms. -/
def branchEntry (s : Int) : Config ops := ⟨.exec (.cond .secret .skip .skip ⟨7⟩), [], s, []⟩
/-- Helper: complete execution of either silent arm. -/
def branchRun (I : Instrumentation ops) (s : Int) : Run I program (branchEntry s) := by
  apply prepend (d := ⟨.exec .skip, [], s, []⟩) (.silent ?_)
    (prepend (.silent rfl) (doneRun I s))
  simp [step, branchEntry, eval, onRes, ops, opsWith]

/-- Law: deterministic steps contribute no external environment choices.
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7 matched external choices). -/
theorem deterministic_choices (c d : Config ops) (label : Label ops) :
    instrumentation.choices c label d none = [] := rfl

/-- Instance: the asymmetric execution has matched choices and releases despite different lengths. -/
theorem asymmetric_pair_choices_match : (left instrumentation).choices = (right instrumentation).choices ∧
    (left instrumentation).releases = (right instrumentation).releases := ⟨rfl, rfl⟩

/-- Planted: only the choice projection is replaced by an internal step counter. -/
abbrev stepCount : Instrumentation ops := { instrumentation with choices := fun _ _ _ _ => [⟨none, .ok 0, 0⟩] }

/-- Instance: an inhabited output transition exercises the deterministic-choice Law. -/
theorem output_has_no_choices : instrumentation.choices ⟨.exec (out 0), [], 0, []⟩ (.emit 0) (finished 0) (some ⟨some 0, .ok 0, 0⟩) = [] :=
  rfl

/-- Planted (law `deterministic_choices`): the same actual output transition is wrongly counted. -/
theorem step_count_adds_choice : stepCount.choices ⟨.exec (out 0), [], 0, []⟩ (.emit 0) (finished 0) (some ⟨some 0, .ok 0, 0⟩) ≠ [] := by decide

/-- Instance: NI refuses an asymmetric unreleased output leak with matched external choices. -/
theorem asymmetric_ni_refused :
    ¬ DelimitedReleaseNI instrumentation program labelling (entry 0) (entry 1) := by
  intro h
  exact (by decide : (left instrumentation).publicObservations ≠
    (right instrumentation).publicObservations)
      (h.2.1 (left instrumentation) (right instrumentation) ⟨rfl, trivial⟩ rfl rfl).1

/-- Helper: project the first recorded control decision for constructor discrimination. -/
def firstDecision : List (Leakage Int Unit Unit) → Bool
  | .branch _ b :: _ => b
  | _ => false

/-- Instance: CT independently refuses the same asymmetric execution. -/
theorem asymmetric_ct_refused :
    ¬ ConstantTime instrumentation program labelling (entry 0) (entry 1) := by
  intro h
  exact (show (left instrumentation).leakage ≠ (right instrumentation).leakage from by
    intro eq
    have bad := congrArg firstDecision eq
    change false = true at bad
    cases bad)
    (h.2.1 (left instrumentation) (right instrumentation) ⟨rfl, trivial⟩ rfl rfl)

/-- Planted: changing only choices to the step counter wrongly admits the asymmetric leak
in both completed-run comparisons. All other observer projections are the machine projections. -/
theorem counted_leak_admitted :
    LowEquivalent labelling (entry 0) (entry 1) ∧
    CompletedNIComparison stepCount program labelling (entry 0) (entry 1) ∧
    CompletedTimingComparison stepCount program labelling (entry 0) (entry 1) := by
  have impossible : ∀ (a : Run stepCount program (entry 0))
      (b : Run stepCount program (entry 1)), a.choices ≠ b.choices := by
    intro a b h
    rw [(complete_unique a (left stepCount)).2.2.2.1,
        (complete_unique b (right stepCount)).2.2.2.1] at h
    exact (by decide : (left stepCount).choices ≠ (right stepCount).choices) h
  exact ⟨⟨rfl, trivial⟩, fun a b _ h _ => (impossible a b h).elim,
    fun a b _ h _ => (impossible a b h).elim⟩

/-- Dropping all public and timing observations leaves choices and releases unchanged. -/
abbrev emptyObserver : Instrumentation ops := { instrumentation with
  observeStep := fun _ _ _ => []
  observeFinal := fun _ => none
  leakage := fun _ _ _ _ => [] }

/-- Planted: run comparison alone admits the known leak when observations are erased.
The qualification Laws kill this substitution for both security judgments. -/
theorem empty_observer_killed :
    NIComparison emptyObserver program labelling (entry 0) (entry 1) ∧
    TimingComparison emptyObserver program labelling (entry 0) (entry 1) ∧
    ¬ DelimitedReleaseNI emptyObserver program labelling (entry 0) (entry 1) ∧
    ¬ ConstantTime emptyObserver program labelling (entry 0) (entry 1) := by
  have unqualified : ¬ ObserverQualified emptyObserver := by
    intro h
    have same := qualified_observers_agree h instrumentation_qualified
    have final := congrArg (fun I : Instrumentation ops => (I.observeFinal (finished 0)).isSome) same
    change false = true at final
    cases final
  refine ⟨?_, ?_, fun h => unqualified (ni_observer_qualified h),
    fun h => unqualified (ct_observer_qualified h)⟩
  · refine ⟨?_, ni_prefixes_of_complete (left emptyObserver) (right emptyObserver) rfl rfl⟩
    intro a b _ _ _
    have leftTrace := complete_unique a (left emptyObserver)
    have rightTrace := complete_unique b (right emptyObserver)
    simp only [Run.publicObservations, leftTrace.1, rightTrace.1, leftTrace.2.1, rightTrace.2.1]
    exact ⟨rfl, rfl⟩
  · refine ⟨?_, timing_prefixes_of_complete (left emptyObserver) (right emptyObserver) rfl⟩
    intro a b _ _ _
    rw [(complete_unique a (left emptyObserver)).2.1, (complete_unique b (right emptyObserver)).2.1]
    rfl

/-- Planted: the step counter fails both qualification Laws even when only completed runs are compared.
The witnessed conditional transition is internal and consumes no external answer. -/
theorem step_counter_killed :
    CompletedNIComparison stepCount program labelling (entry 0) (entry 1) ∧
    CompletedTimingComparison stepCount program labelling (entry 0) (entry 1) ∧
    ¬ DelimitedReleaseNI stepCount program labelling (entry 0) (entry 1) ∧
    ¬ ConstantTime stepCount program labelling (entry 0) (entry 1) := by
  have unqualified : ¬ ObserverQualified stepCount := by
    intro h
    let target : Config ops := ⟨.exec (out 0), [], 0, []⟩
    have noChoice := internal_choices_empty (entry 0) target
    have choice := congrArg (fun I : Instrumentation ops =>
      (I.choices (entry 0) .silent target none).isEmpty) h
    exact Bool.noConfusion (choice.trans (congrArg List.isEmpty noChoice))
  exact ⟨counted_leak_admitted.2.1, counted_leak_admitted.2.2,
    fun h => unqualified (ni_observer_qualified h), fun h => unqualified (ct_observer_qualified h)⟩

/-- Law: evaluated conditionals retain the real source site and Boolean decision.
Authority: docs/decisions.md#decided-project-infoflow-2026-11-06 (baseline branches),
docs/decisions.md#decided-2026-11-08-foundation (C7). -/
theorem conditional_leakage (e : Demo.Expr) (s t : ConcreteSpec.Core.Exec.Stmt ops)
    (site : SiteId) (locals : AList Var Int) (st next v : Int) (frames : List (Frame ops))
    (label : Label ops) (target : Config ops)
    (evaluated : eval locals st e = .ok (v, next)) :
    instrumentation.leakage ⟨.exec (.cond e s t site), locals, st, frames⟩ label target none =
      [.branch site (decide (v ≠ 0))] := by
  exact machine_conditional_leakage (O := ops) e s t site locals st next v frames label target evaluated rfl

/-- Instance: site seven and a true decision exercise conditional leakage. -/
theorem conditional_true_leakage : instrumentation.leakage (branchEntry 1) .silent (finished 1) none = [.branch ⟨7⟩ true] :=
  conditional_leakage .secret .skip .skip ⟨7⟩ [] 1 1 1 [] .silent _ rfl

/-- Planted: omit only conditional leakage; loop and primitive observations are unchanged. -/
abbrev noCondition : Instrumentation ops := { instrumentation with leakage := fun c l d a =>
  match c.ctrl with
  | .exec (.cond _ _ _ _) => []
  | _ => instrumentation.leakage c l d a }

/-- Planted (law `conditional_leakage`): omitting just conditionals loses the real decision. -/
theorem no_condition_loses_decision : noCondition.leakage (branchEntry 1) .silent (finished 1) none ≠ [.branch ⟨7⟩ true] := by
  intro h
  cases h

/-- Instance: secret conditions with identical arms are refused by CT, with premises inhabited. -/
theorem condition_ct_refused :
    LowEquivalent labelling (branchEntry 0) (branchEntry 1) ∧
    (branchRun instrumentation 0).choices = (branchRun instrumentation 1).choices ∧
    (branchRun instrumentation 0).releases = (branchRun instrumentation 1).releases ∧
    ¬ ConstantTime instrumentation program labelling (branchEntry 0) (branchEntry 1) := by
  refine ⟨⟨rfl, trivial⟩, rfl, rfl, ?_⟩
  intro h
  exact (show (branchRun instrumentation 0).leakage ≠ (branchRun instrumentation 1).leakage from by
    intro eq
    have bad := congrArg firstDecision eq
    change false = true at bad
    cases bad)
    (h.2.1 (branchRun instrumentation 0) (branchRun instrumentation 1) ⟨rfl, trivial⟩ rfl rfl)

/-- Planted: omitting only conditional leakage wrongly accepts the same secret condition. -/
theorem no_condition_wrongly_passes_ct : TimingComparison noCondition program labelling (branchEntry 0) (branchEntry 1) := by
  refine ⟨?_, timing_prefixes_of_complete (branchRun noCondition 0) (branchRun noCondition 1) rfl⟩
  intro a b _ _ _
  rw [(complete_unique a (branchRun noCondition 0)).2.1,
      (complete_unique b (branchRun noCondition 1)).2.1]
  rfl

/-- Law: every evaluated loop test records its source decision and the iteration contribution.
True contributes one iteration, false zero; the complete ordered trace retains the exit test.
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7 loop counts),
docs/decisions.md#decided-project-infoflow-2026-11-06 (baseline branches). -/
theorem loop_leakage (l : LoopId) (cr body : ConcreteSpec.Core.Exec.Stmt ops)
    (e : Demo.Expr) (site : SiteId) (dom : List Var) (locals : AList Var Int)
    (st next v : Int) (frames : List (Frame ops)) (label : Label ops) (target : Config ops)
    (evaluated : eval locals st e = .ok (v, next)) :
    instrumentation.leakage ⟨.done, locals, st, .test l cr e body site dom :: frames⟩ label target none =
      [.branch site (decide (v ≠ 0)), .loop l (if v ≠ 0 then 1 else 0)] := by
  simpa [machineInstrumentation] using machine_loop_leakage (O := ops) l cr body e site dom locals st next v frames
    label target evaluated (b := decide (v ≠ 0)) rfl

/-- Helper: a finite loop takes zero or one iteration, breaking immediately in its body. -/
def loopEntry (s : Int) : Config ops :=
  ⟨.exec (.loop ⟨3⟩ .skip .secret (.brk ⟨3⟩) ⟨9⟩), [], s, []⟩

/-- Helper: actual C1 execution of the false loop test. -/
def loopLeft (I : Instrumentation ops) : Run I program (loopEntry 0) :=
  prepend (.silent rfl) (prepend (.silent rfl) (prepend (.silent rfl) (doneRun I 0)))

/-- Helper: actual C1 execution of the true test and the matching break. -/
def loopRight (I : Instrumentation ops) : Run I program (loopEntry 1) :=
  prepend (.silent rfl) (prepend (.silent rfl) (prepend (.silent rfl)
    (prepend (.silent rfl) (prepend (.silent rfl) (doneRun I 1)))))

/-- Instance: nonzero loop identity and site, true decision, and a nonempty continuation. -/
theorem loop_test_leakage : instrumentation.leakage
    ⟨.done, [], 1, [.test ⟨3⟩ .skip .secret (.brk ⟨3⟩) ⟨9⟩ [], .seq .skip]⟩
    .silent (finished 1) none = [.branch ⟨9⟩ true, .loop ⟨3⟩ 1] :=
  loop_leakage ⟨3⟩ .skip (.brk ⟨3⟩) .secret ⟨9⟩ [] [] 1 1 1 [.seq .skip] .silent _ rfl

/-- Planted: omit only loop-test leakage. Conditional and primitive projections are unchanged. -/
abbrev noLoop : Instrumentation ops := { instrumentation with leakage := fun c l d a =>
  match c.ctrl, c.frames with
  | .done, .test _ _ _ _ _ _ :: _ => []
  | _, _ => instrumentation.leakage c l d a }

/-- Planted (law `loop_leakage`): omitting loop tests loses both site and iteration evidence. -/
theorem no_loop_loses_test : noLoop.leakage
    ⟨.done, [], 1, [.test ⟨3⟩ .skip .secret (.brk ⟨3⟩) ⟨9⟩ [], .seq .skip]⟩
    .silent (finished 1) none ≠ [.branch ⟨9⟩ true, .loop ⟨3⟩ 1] := by decide

/-- Instance: CT refuses a secret-dependent finite loop, with matched choices and no releases. -/
theorem loop_ct_refused :
    LowEquivalent labelling (loopEntry 0) (loopEntry 1) ∧
    (loopLeft instrumentation).choices = (loopRight instrumentation).choices ∧
    (loopLeft instrumentation).releases = (loopRight instrumentation).releases ∧
    ¬ ConstantTime instrumentation program labelling (loopEntry 0) (loopEntry 1) := by
  refine ⟨⟨rfl, trivial⟩, rfl, rfl, ?_⟩
  intro h
  exact (show (loopLeft instrumentation).leakage ≠ (loopRight instrumentation).leakage from by
    intro eq
    have bad := congrArg firstDecision eq
    change false = true at bad
    cases bad)
    (h.2.1 (loopLeft instrumentation) (loopRight instrumentation) ⟨rfl, trivial⟩ rfl rfl)

/-- Planted: omitting only loop tests wrongly accepts that same finite loop. -/
theorem no_loop_wrongly_passes_ct : TimingComparison noLoop program labelling (loopEntry 0) (loopEntry 1) := by
  refine ⟨?_, timing_prefixes_of_complete (loopLeft noLoop) (loopRight noLoop) rfl⟩
  intro a b _ _ _
  rw [(complete_unique a (loopLeft noLoop)).2.1,
      (complete_unique b (loopRight noLoop)).2.1]
  rfl

/-- Planted: erase just the emitted events. -/
abbrev noObservations : Instrumentation ops := { instrumentation with observeStep := fun _ _ _ => [] }

/-- Planted: erase just the final observation. -/
abbrev noFinal : Instrumentation ops := { instrumentation with observeFinal := fun _ => none }

/-- Planted: erase just the baseline leakage. -/
abbrev noLeakage : Instrumentation ops := { instrumentation with leakage := fun _ _ _ _ => [] }

/-- Planted (Laws `emitted_observations` and `qualified_observers_agree`). -/
theorem erased_observations_refused : ¬ ObserverQualified noObservations := by
  intro qualified
  let c : Config ops := ⟨.exec (out 0), [], 0, []⟩
  have actual := emitted_observations c (finished 0) (.emit 0)
  have eq := congrArg (fun I : Instrumentation ops =>
    (I.observeStep c (.emit 0) (finished 0)).isEmpty) qualified
  have observed := congrArg List.isEmpty actual
  exact Bool.noConfusion (eq.trans observed)

/-- Planted (Laws `final_observations` and `qualified_observers_agree`). -/
theorem erased_final_refused : ¬ ObserverQualified noFinal := by
  intro qualified
  have actual := final_observations (finished 0)
  have eq := congrArg (fun I : Instrumentation ops => (I.observeFinal (finished 0)).isSome) qualified
  have observed := congrArg Option.isSome actual
  exact Bool.noConfusion (eq.trans observed)

/-- Planted (Law `machine_conditional_leakage`): deleting the entire leakage projection,
or deleting just conditionals, independently fails qualification. -/
theorem erased_branch_leakage_refused :
    ¬ ObserverQualified noLeakage ∧ ¬ ObserverQualified noCondition := by
  have expected := machine_conditional_leakage (O := ops) .secret .skip .skip ⟨7⟩ [] 1 1 1
    [] .silent (finished 1) rfl rfl
  constructor
  · intro qualified
    rw [← qualified] at expected
    cases expected
  · intro qualified
    rw [← qualified] at expected
    cases expected

/-- Planted (Law `machine_loop_leakage`): deleting just the loop tests fails qualification. -/
theorem erased_loop_leakage_refused : ¬ ObserverQualified noLoop := by
  intro qualified
  have expected := machine_loop_leakage (O := ops) ⟨3⟩ .skip (.brk ⟨3⟩) .secret ⟨9⟩
    [] [] 1 1 1 [.seq .skip] .silent (finished 1) (b := true) rfl rfl
  rw [← qualified] at expected
  cases expected

/-- Planted: turn each primitive answer's store into a choice, leaving internal steps silent. -/
abbrev storeChoices : Instrumentation ops := { instrumentation with
  choices := fun _ _ _ a => a.toList.map fun answer => ⟨none, .ok 0, answer.store⟩ }

/-- Planted: count just primitive answers as choices, leaving internal steps silent. -/
abbrev primitiveCount : Instrumentation ops := { instrumentation with
  choices := fun _ _ _ a => a.toList.map fun _ => ⟨none, .ok 0, 0⟩ }

/-- Planted (Law `deterministic_primitive_choices`): the accepted deterministic answer
cannot inject store contents or a primitive-step token into either judgment's premise. -/
theorem deterministic_choice_forgery_refused :
    ¬ ObserverQualified storeChoices ∧ ¬ ObserverQualified primitiveCount := by
  let c : Config ops := ⟨.exec (out 0), [], 0, []⟩
  let a : Answer Int Int Int := ⟨some 0, .ok 0, 0⟩
  have accepted : ops.allows .output [0] 0 a = true := rfl
  have choices : (machineInstrumentation ops).choices c (.emit 0) (finished 0) (some a) = [] :=
    deterministic_primitive_choices (O := ops) (c := c) (a := a) (p := .output) (vs := [0]) (site := ⟨8⟩) (st := 0) rfl
      (fun _ h => allowed_unique h accepted)
  constructor
  · intro qualified
    rw [← qualified] at choices
    cases choices
  · intro qualified
    rw [← qualified] at choices
    cases choices

/-- Planted: erase only choices on the machine with two allowed answers to one output request. -/
abbrev noChoices : Instrumentation (opsWith permissiveAllowed) :=
  instrumentationWithChoices (opsWith permissiveAllowed) (fun _ _ => [])

/-- Planted (Law `nondeterministic_primitive_choices`): an actual nondeterministic choice
cannot be erased, even when all other projections agree by construction. -/
theorem erased_choices_refused : ¬ ObserverQualified noChoices := by
  intro qualified
  let O := opsWith permissiveAllowed
  let c : Config O := ⟨.exec (.prim none .output [.lit 7] ⟨8⟩), [], 0, []⟩
  let d : Config O := ⟨.done, [], 0, []⟩
  let a : Answer Int Int Int := ⟨some 7, .ok 0, 0⟩
  let b : Answer Int Int Int := ⟨some 8, .ok 0, 0⟩
  have distinct : b ≠ a := by decide
  have choices : (machineInstrumentation O).choices c (.emit 7) d (some a) = [a] :=
    nondeterministic_primitive_choices (O := O) (c := c) (a := a) (p := .output) (vs := [7]) (site := ⟨8⟩) (st := 0) rfl
      rfl ⟨b, distinct, rfl⟩
  have eq := congrArg (fun I : Instrumentation O => (I.choices c (.emit 7) d (some a)).isEmpty) qualified
  exact Bool.noConfusion (eq.trans (congrArg List.isEmpty choices))

/-- Planted: retain all releases except those with a bound result destination. -/
abbrev unboundOnly : Instrumentation ops := { instrumentation with releases := fun c l d =>
  match c.ctrl with
  | .exec (.prim (some _) _ _ _) => []
  | _ => instrumentation.releases c l d }

/-- Planted: retain all releases except those with a nonempty continuation. -/
abbrev emptyStackOnly : Instrumentation ops := { instrumentation with releases := fun c l d =>
  match c.frames with
  | [] => instrumentation.releases c l d
  | _ :: _ => [] }

/-- Instance: bound destination and nonempty stack both inhabit the generalized site Law. -/
theorem bound_stacked_release_agrees : (instrumentation.releases
    ⟨.exec (.prim (some ⟨2⟩) .release [.lit 42] ⟨7⟩), [], 0, [.seq .skip]⟩
    .silent (finished 0)).map Release.site =
    statementReleases (fun p : Prim => match p with | .release => some () | _ => none)
      (.prim (some ⟨2⟩) .release [.lit 42] ⟨7⟩ : ConcreteSpec.Core.Exec.Stmt ops) :=
  release_sites_agree (O := ops) ⟨7⟩ .release [.lit 42] [] 0 0 42 .silent (finished 0) (some ⟨2⟩) [.seq .skip] rfl

/-- Planted (law `release_sites_agree`): bound-destination filtering alone breaks agreement. -/
theorem bound_filter_loses_release : evalArgs ops [] 0 [.lit 42] = .ok ([42], 0) ∧
    (unboundOnly.releases
      ⟨.exec (.prim (some ⟨2⟩) .release [.lit 42] ⟨7⟩), [], 0, []⟩
      .silent (finished 0)).map Release.site ≠
    statementReleases (fun p : Prim => match p with | .release => some () | _ => none)
      (.prim (some ⟨2⟩) .release [.lit 42] ⟨7⟩ : ConcreteSpec.Core.Exec.Stmt ops) := by decide

/-- Planted (law `release_sites_agree`): stack filtering alone breaks agreement. -/
theorem stack_filter_loses_release : evalArgs ops [] 0 [.lit 42] = .ok ([42], 0) ∧
    (emptyStackOnly.releases
      ⟨.exec (.prim none .release [.lit 42] ⟨7⟩), [], 0, [.seq .skip]⟩
      .silent (finished 0)).map Release.site ≠
    statementReleases (fun p : Prim => match p with | .release => some () | _ => none)
      (.prim none .release [.lit 42] ⟨7⟩ : ConcreteSpec.Core.Exec.Stmt ops) := by decide

end ConcreteSpec.Core.Leak.ControlAudit
