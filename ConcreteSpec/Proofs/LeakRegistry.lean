import ConcreteSpec.Spec.LeakData
import ConcreteSpec.Spec.ReleaseCalls
import ConcreteSpec.Proofs.Leak
import ConcreteSpec.Proofs.ReleaseCalls
import ConcreteSpec.Spec.Basics
import ConcreteSpec.Spec.Exec
import ConcreteSpec.Spec.Ident
import ConcreteSpec.Spec.Leak
import ConcreteSpec.Spec.Outcome
import ConcreteSpec.Spec.Rules

/-! C7 registry conformance evidence. The specification registry stores names only;
the exporter loads these kernel-checked proofs without making a Spec module import Proofs. -/
namespace ConcreteSpec.Core

namespace Leak.Registry
open Demo _root_.ConcreteSpec.Core.Exec

/-- Concrete release followed by an unreleased secret output. -/
def unreleasedOutput (secret : Int) : IntegerEntry :=
  ⟨.seq (.prim none .release [.lit 0] ⟨0⟩) (.prim none .output [.secret] ⟨1⟩), [], secret⟩

/-- Concrete release and public output of the same masked sum. -/
def releasedSum (publicValue secret : Int) : IntegerEntry :=
  ⟨.seq (.prim none .release [.masked] ⟨0⟩) (.prim none .output [.masked] ⟨1⟩),
    [(⟨0⟩, -secret)], publicValue⟩

/-- Variable-time secret work followed by a fixed authorized release and public output. -/
def variableWork (secret : Int) : IntegerEntry :=
  ⟨.seq (.prim none .work [.secret] ⟨2⟩)
    (.seq (.prim none .release [.lit 0] ⟨0⟩) (.prim none .output [.lit 7] ⟨1⟩)), [], secret⟩

/-- Instance: the concrete call graph passes the full function and site policy. -/
theorem callsAccepted : RuleAccepted UnitCallsAdmitted (CallControls.allAllowed, CallControls.chain) :=
    CallControls.admitted
/-- Instance: omitting only the outer caller refuses the same two-level release chain. -/
theorem callsRefused : RuleRefused UnitCallsAdmitted (CallControls.outerDenied, CallControls.chain) :=
    CallControls.refused

/-- Instance: a declared release in an allowlisted function passes program admission. -/
theorem programAccepted : RuleAccepted IntegerProgramAdmitted
    (⟨[CallControls.fn "2"], ⟨[⟨⟨7⟩, ()⟩]⟩⟩,
     [(CallControls.fn "2", ⟨[], ⟨0⟩, .prim none .release [] ⟨7⟩⟩)]) := by
  intro caller owner site hc hr
  have hs : owner = CallControls.fn "2" ∧ site = ⟨⟨7⟩, ()⟩ := by
    change (owner, site) ∈ [(CallControls.fn "2", ⟨⟨7⟩, ()⟩)] at hr
    simpa using hr
  obtain ⟨rfl, rfl⟩ := hs
  cases hc with
  | self => simp [ReleasePolicy.Permits]
  | call edge _ => simp [integerProgram, ReleaseCalls.ofProgram, statementCalls, IntegerStatement.statement] at edge
/-- Instance: removing only that function refuses the same populated program. -/
theorem programRefused : RuleRefused IntegerProgramAdmitted
    (⟨[], ⟨[⟨⟨7⟩, ()⟩]⟩⟩,
     [(CallControls.fn "2", ⟨[], ⟨0⟩, .prim none .release [] ⟨7⟩⟩)]) := by
  intro h
  have denied := (h (CallControls.fn "2") (CallControls.fn "2") ⟨⟨7⟩, ()⟩ (.self _)
    (by simp [integerProgram, ReleaseCalls.ofProgram, statementReleases, IntegerStatement.statement])).1
  exact List.not_mem_nil denied

/-- Planted: compare only the numeric site, discarding its declared type. -/
def permitsUntyped (p : ReleasePolicy String) (r : Release String Nat) : Prop :=
  r.site.site ∈ p.allowed.map ReleaseSite.site

/-- Planted control for `permits_listed_site`: a site with the wrong type is admitted. -/
theorem untyped_permission_admits_wrong_type :
    permitsUntyped typedReleasePolicy (typedRelease "secret" 7) ∧
      (typedRelease "secret" 7).site ∉ typedReleasePolicy.allowed := by
  simp [permitsUntyped, typedRelease, typedReleasePolicy]

/-- Planted: also require a zero value, even at an exactly listed typed site. -/
def permitsOnlyZero (p : ReleasePolicy String) (r : Release String Nat) : Prop :=
  p.Permits r ∧ r.value = 0

/-- Planted control for `listed_site_permitted`: a listed release has a nonzero value. -/
theorem zero_requirement_refuses_listed_site :
    (typedRelease "public" 7).site ∈ typedReleasePolicy.allowed ∧
      ¬ permitsOnlyZero typedReleasePolicy (typedRelease "public" 7) :=
  ⟨by decide, fun h => by cases h.2⟩

/-- Planted: classify a singleton allowlist as forbidding releases. -/
def forbidsSingleton (p : ReleasePolicy Unit) : Prop :=
  p.allowed.length ≤ 1

/-- Planted control for `forbidding_policy_empty`: a populated list is misclassified. -/
theorem singleton_is_misclassified :
    forbidsSingleton policy ∧ policy.allowed ≠ [] := by
  simp [forbidsSingleton]

/-- Planted: an off-by-one test requires the empty list to have negative length. -/
def forbidsTooStrict (p : ReleasePolicy Unit) : Prop :=
  p.allowed.length < 0

/-- Planted control for `empty_policy_forbidding`: an empty list is misclassified. -/
theorem empty_policy_is_misclassified :
    ∃ p : ReleasePolicy Unit, p.allowed = [] ∧ ¬ forbidsTooStrict p :=
  ⟨⟨[]⟩, rfl, by simp [forbidsTooStrict]⟩

/-- Planted: replace exact permission with mere nonemptiness of the allowlist. -/
def authorizesWithAnySite (p : ReleasePolicy Unit) (rs : List (Release Unit Int)) : Prop :=
  ∀ _r ∈ rs, p.allowed ≠ []

/-- Planted control for `unlisted_trace_refused`: a populated policy still omits site seven. -/
theorem populated_policy_admits_unlisted_release :
    let release := releaseAt ⟨7⟩ 42
    release ∈ [release] ∧ release.site ∉ policy.allowed ∧ authorizesWithAnySite policy [release] := by
  refine ⟨by simp, by decide, ?_⟩
  exact fun _ _ => by decide

/-- Planted: drop just the public-local equality conjunct. -/
def lowIgnoresLocals {O : Operands} (L : Labelling O) (c d : Config O) : Prop :=
  L.lowStore c.store d.store

/-- Planted control for `low_equivalent_public_state`: differing public locals are admitted. -/
theorem ignoring_locals_admits_public_change :
    lowIgnoresLocals labelling (mixedConfig (-1) 7) (mixedConfig (-2) 8) ∧
      publicLocals labelling (mixedConfig (-1) 7) ≠ publicLocals labelling (mixedConfig (-2) 8) :=
  ⟨trivial, by decide⟩

/-- Planted: retain public-local equality and drop the public-store relation. -/
def lowIgnoresStore {O : Operands} (L : Labelling O) (c d : Config O) : Prop :=
  publicLocals L c = publicLocals L d

/-- Planted control for `low_equivalent_public_state`: unrelated public stores are admitted. -/
theorem ignoring_store_admits_public_change :
    lowIgnoresStore storeSensitiveLabelling (leakyStart 1) (leakyStart 2) ∧
      ¬ storeSensitiveLabelling.lowStore (leakyStart 1).store (leakyStart 2).store :=
  ⟨rfl, by decide⟩

/-- Planted: additionally require exact equality of all local bindings. -/
def lowRequiresRawLocals {O : Operands} (L : Labelling O) (c d : Config O) : Prop :=
  LowEquivalent L c d ∧ c.locals = d.locals

/-- Planted control for `public_state_low_equivalent`: differing secret locals are refused. -/
theorem raw_locals_refuse_secret_change :
    publicLocals labelling (mixedConfig (-1) 7) = publicLocals labelling (mixedConfig (-2) 7) ∧
      labelling.lowStore (mixedConfig (-1) 7).store (mixedConfig (-2) 7).store ∧
      ¬ lowRequiresRawLocals labelling (mixedConfig (-1) 7) (mixedConfig (-2) 7) :=
  ⟨rfl, trivial, fun h => (by decide : (mixedConfig (-1) 7).locals ≠ (mixedConfig (-2) 7).locals) h.2⟩

/-- Instance: the listed typed site is permitted. -/
theorem permitsAccepted : RuleAccepted StringReleasePermitted (typedReleasePolicy, typedRelease "public" 7) := by
    simp [RuleAccepted, StringReleasePermitted, ReleasePolicy.Permits, typedReleasePolicy, typedRelease]
/-- Instance: changing only the type at that site is refused. -/
theorem permitsRefused : RuleRefused StringReleasePermitted (typedReleasePolicy, typedRelease "secret" 7) := by simp
    [RuleRefused, StringReleasePermitted, ReleasePolicy.Permits, typedReleasePolicy, typedRelease]
/-- Instance: an empty policy forbids releases. -/
theorem forbidsAccepted : RuleAccepted UnitReleaseForbidden ⟨[]⟩ := rfl
/-- Instance: the populated policy does not forbid release. -/
theorem forbidsRefused : RuleRefused UnitReleaseForbidden policy := by
  simp [RuleRefused, UnitReleaseForbidden, ReleasePolicy.ForbidsRelease]
/-- Instance: both explicit releases are authorized. -/
theorem authorizedAccepted : RuleAccepted UnitTraceAuthorized (policy, [releaseValue 7, releaseValue 8]) := by
  simp [RuleAccepted, UnitTraceAuthorized, ReleasePolicy.Authorized, ReleasePolicy.Permits, releaseValue, releaseAt]
/-- Instance: an empty policy rejects the same populated trace. -/
theorem authorizedRefused : RuleRefused UnitTraceAuthorized (⟨[]⟩, [releaseValue 7, releaseValue 8]) :=
  forbidsRelease_not_authorized ⟨[]⟩ (releaseValue 7) [releaseValue 8] rfl
/-- Instance: authorization quantifies over every reachable execution prefix of this concrete entry. -/
theorem runsAccepted : RuleAccepted IntegerRunsAuthorized
    (⟨[⟨⟨0⟩, ()⟩]⟩, ⟨[], unreleasedOutput 1⟩) := release_authorized 1
/-- Instance: the same populated execution is refused by the empty policy. -/
theorem runsRefused : RuleRefused IntegerRunsAuthorized
    (⟨[]⟩, ⟨[], unreleasedOutput 1⟩) := by
  intro h
  have bad := h.2 (leakyRun 1).steps
  change ReleasePolicy.Authorized ⟨[]⟩ [releaseValue 0] at bad
  simp [ReleasePolicy.Authorized, ReleasePolicy.Permits] at bad
/-- One release at an arbitrary source site, with a populated operand. -/
def releaseEntry (site : SiteId) : IntegerEntry :=
  ⟨.prim none .release [.lit 42] site, [], 0⟩

/-- Helper: a complete C1 execution of the single release, for any observer and source site. -/
def siteRun (I : Instrumentation ops) (site : SiteId) :
    Run I program (releaseEntry site).config := by
  let c := (releaseEntry site).config
  let done : Config ops := ⟨.done, [], 0, []⟩
  let answer : Answer Int Int Int := ⟨none, .ok 0, 0⟩
  refine ⟨done, .normal, rfl, I.observeStep c .silent done, I.leakage c .silent done (some answer),
    I.releases c .silent done, I.choices c .silent done (some answer), ?_⟩
  simpa only [List.append_nil, Answer.label] using InstrumentedSteps.step (I := I)
    (AnsweredStep.prim (P := program) (c := c) (c' := done) (p := .release) (site := site)
      (vs := [42]) (st := 0) (a := (⟨none, .ok 0, 0⟩ : Answer Int Int Int))
      rfl (by simp [allowed]) rfl) (InstrumentedSteps.refl done)

/-- Release at site seven, then invoke a function absent from the empty program. -/
def releaseThenStuck : IntegerEntry :=
  ⟨.seq (.prim none .release [.lit 42] ⟨7⟩) (.call none (CallControls.fn "missing") [] ⟨8⟩),
    [], 0⟩

def missingCall : Exec.Stmt ops :=
  .call none (CallControls.fn "missing") [] ⟨8⟩

def stuckAfterRelease : Config ops := ⟨.exec missingCall, [], 0, []⟩

/-- The executed release is retained in a finite prefix whose endpoint is stuck. -/
theorem release_then_stuck_prefix :
    InstrumentedSteps instrumentation program releaseThenStuck.config []
      [.variableTime ⟨7⟩ [42]] [releaseAt ⟨7⟩ 42] [] stuckAfterRelease := by
  let pending : Config ops := ⟨.exec (.prim none .release [.lit 42] ⟨7⟩), [], 0, [.seq missingCall]⟩
  let released : Config ops := ⟨.done, [], 0, [.seq missingCall]⟩
  have enter : AnsweredStep program releaseThenStuck.config .silent pending none := .silent rfl
  have emitRelease : AnsweredStep program pending .silent released (some ⟨none, .ok 0, 0⟩) :=
    AnsweredStep.prim (P := program) (c := pending) (c' := released) (p := .release) (site := ⟨7⟩) (vs := [42]) (st := 0)
      (a := (⟨none, .ok 0, 0⟩ : Answer Int Int Int)) rfl (by simp [allowed]) rfl
  have resume : AnsweredStep program released .silent stuckAfterRelease none := .silent rfl
  exact .step enter (.step emitRelease (.step resume (.refl _)))

/-- A prefix reaching a stuck endpoint excludes all complete runs on this deterministic machine. -/
theorem no_complete_run_from_stuck_prefix {c d : Config ops} {obs leaks rs choices}
    (pre : InstrumentedSteps instrumentation program c obs leaks rs choices d)
    (stuck : step program d = none) (unfinished : d.final? = none) :
    ¬ Nonempty (Run instrumentation program c) := by
  rintro ⟨r⟩
  obtain ⟨_, _, _, _, suffix, _, _, _, _⟩ := terminal_execution_suffix r.steps pre stuck
  have endpoint : r.final = d := by
    have terminal : step program r.final = none := final_no_step r.finalEnding
    have erased := suffix.toSteps
    generalize r.final = f at terminal erased ⊢
    obtain ⟨_, steps⟩ := erased
    cases steps with
    | refl => rfl
    | step h _ => exact (no_step_of_none terminal _ _ h).elim
  have ending := r.finalEnding
  rw [endpoint, unfinished] at ending
  cases ending

/-- Instance of `unlisted_prefix_refused`: the empty policy refuses the release followed by a stuck call. -/
theorem stuckRunsRefused : RuleRefused IntegerRunsAuthorized (⟨[]⟩, ⟨[], releaseThenStuck⟩) :=
  unlisted_prefix_refused ⟨[]⟩ release_then_stuck_prefix (releaseAt ⟨7⟩ 42)
    (by simp) (List.not_mem_nil)

/-- Planted: replace the prefix quantifier with a completed-run quantifier. -/
def completeRunsOnly {O : Operands} (p : ReleasePolicy O.Ty)
    (I : Instrumentation O) (P : Exec.Program O) (c : Config O) : Prop :=
  ∀ r : Run I P c, p.Authorized r.releases

/-- Planted control for `unlisted_prefix_refused` with `completeRunsOnly`. -/
theorem completed_only_admits_stuck_release :
    completeRunsOnly ⟨[]⟩ instrumentation program releaseThenStuck.config ∧
      ¬ (ReleasePolicy.mk []).RunsAuthorized instrumentation program releaseThenStuck.config := by
  refine ⟨?_, stuckRunsRefused⟩
  intro r
  exact (no_complete_run_from_stuck_prefix release_then_stuck_prefix rfl rfl ⟨r⟩).elim

/-- Planted: require a release on every prefix in addition to checking authorization. -/
def requiresReleaseOnEveryPrefix {O : Operands} (p : ReleasePolicy O.Ty)
    (I : Instrumentation O) (P : Exec.Program O) (c : Config O) : Prop :=
  ∀ ⦃obs leaks rs choices d⦄, InstrumentedSteps I P c obs leaks rs choices d →
    p.Authorized rs ∧ rs ≠ []

/-- Planted control for `listed_prefixes_authorized`: requiring a release wrongly refuses
a permitted entry whose initial prefix is empty and whose later release is listed. -/
theorem release_requirement_refuses_prefixes :
    policy.RunsAuthorized instrumentation program (releaseEntry ⟨0⟩).config ∧
      ¬ requiresReleaseOnEveryPrefix policy instrumentation program (releaseEntry ⟨0⟩).config := by
  refine ⟨⟨instrumentation_qualified, prefixes_authorized_of_complete policy (siteRun instrumentation ⟨0⟩) ?_⟩, ?_⟩
  · change policy.Authorized [releaseAt ⟨0⟩ 42]
    simp [ReleasePolicy.Authorized, ReleasePolicy.Permits, releaseAt]
  · intro h
    exact (h (.refl _)).2 rfl

/-- Instance of `executed_release_value`: a C1 request and accepted answer witness the release. -/
theorem nonzero_release_execution :
    (machineInstrumentation ops).releases (releaseEntry ⟨7⟩).config .silent (finished 0) =
      [releaseAt ⟨7⟩ 42] := by
  let a : Answer Int Int Int := ⟨none, .ok 0, 0⟩
  obtain ⟨v, operand, observed⟩ := executed_release_value (P := program) ⟨7⟩ .release
    [.lit 42] [] 0 0 [42] none [] (finished 0) (a := a) (ty := ()) rfl rfl rfl rfl
  have value : v = 42 := (List.cons.inj operand).1.symm
  rw [value] at observed
  exact observed

/-- Instance: a nonzero site and nonzero operand inhabit the agreement Law. -/
theorem nonzero_site_agreement :
    (instrumentation.releases (releaseEntry ⟨7⟩).config .silent ⟨.done, [], 0, []⟩).map Release.site =
    statementReleases (fun p : Prim => match p with | .release => some () | _ => none)
      (.prim none .release [.lit 42] ⟨7⟩ : ConcreteSpec.Core.Exec.Stmt ops) :=
  release_sites_agree (O := ops) ⟨7⟩ .release [.lit 42] [] 0 0 42 .silent (finished 0) none [] rfl

/-- Planted: replace only the source site with zero, preserving the evaluated operand. -/
abbrev zeroSite : Instrumentation ops := { instrumentation with
  releases := fun c _ _ => match primitiveRequest c with
    | some (.release, [v], _, _) => [releaseAt ⟨0⟩ v]
    | _ => [] }

/-- Planted: the same inhabited evaluation refutes site agreement for the zero-site observer. -/
theorem zero_site_breaks_agreement : evalArgs ops [] 0 [.lit 42] = .ok ([42], 0) ∧
    (zeroSite.releases (releaseEntry ⟨7⟩).config .silent ⟨.done, [], 0, []⟩).map Release.site ≠
    statementReleases (fun p : Prim => match p with | .release => some () | _ => none)
      (.prim none .release [.lit 42] ⟨7⟩ : ConcreteSpec.Core.Exec.Stmt ops) := by decide

/-- Instance: all execution prefixes at site seven pass the site-seven policy. -/
theorem nonzeroRunsAccepted : RuleAccepted IntegerRunsAuthorized
    (⟨[⟨⟨7⟩, ()⟩]⟩, ⟨[], releaseEntry ⟨7⟩⟩) := by
  refine ⟨instrumentation_qualified, ?_⟩
  apply prefixes_authorized_of_complete _ (siteRun instrumentation ⟨7⟩)
  change ReleasePolicy.Authorized ⟨[⟨⟨7⟩, ()⟩]⟩ [releaseAt ⟨7⟩ 42]
  simp [ReleasePolicy.Authorized, ReleasePolicy.Permits, releaseAt]

/-- Instance: changing only the policy site to zero refuses that same populated execution. -/
theorem nonzeroRunsRefused : RuleRefused IntegerRunsAuthorized
    (⟨[⟨⟨0⟩, ()⟩]⟩, ⟨[], releaseEntry ⟨7⟩⟩) := by
  intro h
  have bad := h.2 (siteRun instrumentation ⟨7⟩).steps
  change ReleasePolicy.Authorized ⟨[⟨⟨0⟩, ()⟩]⟩ [releaseAt ⟨7⟩ 42] at bad
  simp [ReleasePolicy.Authorized, ReleasePolicy.Permits, releaseAt] at bad

/-- Planted: the zero-site substitution wrongly authorizes every run under the site-zero policy. -/
theorem zero_site_wrongly_authorizes : policy.PrefixesAuthorized zeroSite program (releaseEntry ⟨7⟩).config := by
  apply prefixes_authorized_of_complete policy (siteRun zeroSite ⟨7⟩)
  change ReleasePolicy.Authorized policy [releaseAt ⟨0⟩ 42]
  simp [ReleasePolicy.Authorized, ReleasePolicy.Permits, releaseAt]

/-- Planted: only the release projection is erased. -/
abbrev noReleases : Instrumentation ops := { instrumentation with releases := fun _ _ _ => [] }

/-- Helper: erasing releases forces every raw prefix trace to be empty. -/
private theorem erased_release_trace {c d : Config ops} {trace : ExecutionTrace ops}
    (pre : InstrumentedExecution noReleases program c trace d) : trace.releases = [] := by
  induction pre with
  | refl => rfl
  | step _ _ ih => exact ih

/-- Planted (Laws `release_sites_agree`, `authorization_observer_qualified`):
erasure passes raw authorization on the very release-then-stuck entry that emits site seven. -/
theorem erased_releases_refused :
    (ReleasePolicy.mk []).PrefixesAuthorized noReleases program releaseThenStuck.config ∧
    ¬ (ReleasePolicy.mk []).RunsAuthorized noReleases program releaseThenStuck.config := by
  constructor
  · intro obs leaks rs choices d pre
    have erased : rs = [] := erased_release_trace pre
    rw [erased]
    exact fun _ h => (List.not_mem_nil h).elim
  · intro h
    let pending : Config ops := ⟨.exec (.prim none .release [.lit 42] ⟨7⟩), [], 0, [.seq missingCall]⟩
    let released : Config ops := ⟨.done, [], 0, [.seq missingCall]⟩
    have expected := release_sites_agree (O := ops) ⟨7⟩ .release [.lit 42] [] 0 0 42
      .silent released none [.seq missingCall] rfl
    have same := authorization_observer_qualified h
    have site := congrArg (fun I : Instrumentation ops =>
      (I.releases pending .silent released).map Release.site) same
    change [] = ((machineInstrumentation ops).releases pending .silent released).map Release.site at site
    rw [expected] at site
    cases site

/-- Planted (Laws `release_sites_agree`, `authorization_observer_qualified`):
the forged site passes the raw helper and fails qualified authorization. -/
theorem zero_site_qualification_refused :
    ¬ policy.RunsAuthorized zeroSite program (releaseEntry ⟨7⟩).config := by
  intro h
  have same := qualified_observers_agree (authorization_observer_qualified h) instrumentation_qualified
  have site := congrArg (fun I : Instrumentation ops =>
    (I.releases (releaseEntry ⟨7⟩).config .silent (finished 0)).map Release.site) same
  exact zero_site_breaks_agreement.2 (site.trans nonzero_site_agreement)

/-- Instance: negative locals are secret in this integer-machine labelling. -/
theorem lowAccepted : RuleAccepted IntegerLowEquivalent
    ⟨[], ⟨.skip, [(⟨0⟩, -1), (⟨1⟩, 7)], 0⟩,
     ⟨.skip, [(⟨0⟩, -2), (⟨1⟩, 7)], 0⟩⟩ := by
  simp [RuleAccepted, IntegerLowEquivalent, IntegerEntry.config, LowEquivalent, publicLocals, labelling]
/-- Instance: changing only the public local breaks the same equivalence. -/
theorem lowRefused : RuleRefused IntegerLowEquivalent
    ⟨[], ⟨.skip, [(⟨0⟩, -1), (⟨1⟩, 7)], 0⟩,
     ⟨.skip, [(⟨0⟩, -2), (⟨1⟩, 8)], 0⟩⟩ := by
  simp [RuleRefused, IntegerLowEquivalent, IntegerEntry.config, LowEquivalent, publicLocals, labelling]
/-- Instance: distinct secrets with equal released sums satisfy NI. -/
theorem niAccepted : RuleAccepted IntegerDelimitedReleaseNI
    ⟨[], releasedSum 6 2,
     releasedSum 5 3⟩ :=
  secure_ni 6 5 1 2 ⟨rfl, trivial⟩ ⟨secureRun 6 1, secureRun 5 2, rfl, rfl⟩
/-- Instance: an unreleased output is refused despite authorized releases. -/
theorem niRefused : RuleRefused IntegerDelimitedReleaseNI
    ⟨[], unreleasedOutput 1, unreleasedOutput 2⟩ :=
  authorized_release_not_ni 1 2 (release_authorized 1) (release_authorized 2) (leaky_low 1 2) (by decide)
/-- Instance: equal sums independently satisfy CT. -/
theorem ctAccepted : RuleAccepted IntegerConstantTime
    ⟨[], releasedSum 6 2,
     releasedSum 5 3⟩ :=
  secure_ct 6 5 1 2 ⟨rfl, trivial⟩ ⟨secureRun 6 1, secureRun 5 2, rfl, rfl⟩
/-- Instance: variable-time work leaks while public output agrees. -/
theorem ctRefused : RuleRefused IntegerConstantTime
    ⟨[], variableWork 1, variableWork 2⟩ :=
  timing_not_ct 1 2 ⟨rfl, trivial⟩ (by decide)

/-- An asymmetric secret branch with two different public outputs. -/
def asymmetricOutput (secret : Int) : IntegerEntry :=
  ⟨.cond .secret (.seq .skip (.prim none .output [.lit 1] ⟨8⟩))
    (.prim none .output [.lit 0] ⟨8⟩) ⟨7⟩, [], secret⟩

/-- Instance: matched external choices cannot hide an asymmetric unreleased output. -/
theorem asymmetricNIRefused : RuleRefused IntegerDelimitedReleaseNI
    ⟨[], asymmetricOutput 0, asymmetricOutput 1⟩ := ControlAudit.asymmetric_ni_refused

/-- Instance: CT separately refuses the asymmetric secret branch. -/
theorem asymmetricCTRefused : RuleRefused IntegerConstantTime
    ⟨[], asymmetricOutput 0, asymmetricOutput 1⟩ := ControlAudit.asymmetric_ct_refused

/-- Instance: CT refuses secret decisions even when both arms are silent and identical. -/
theorem conditionalCTRefused : RuleRefused IntegerConstantTime
    ⟨[], ⟨.cond .secret .skip .skip ⟨7⟩, [], 0⟩,
         ⟨.cond .secret .skip .skip ⟨7⟩, [], 1⟩⟩ := ControlAudit.condition_ct_refused.2.2.2

/-- Instance: CT refuses a finite secret loop with zero versus one entered iteration. -/
theorem loopCTRefused : RuleRefused IntegerConstantTime
    ⟨[], ⟨.loop ⟨3⟩ .skip .secret (.brk ⟨3⟩) ⟨9⟩, [], 0⟩,
         ⟨.loop ⟨3⟩ .skip .secret (.brk ⟨3⟩) ⟨9⟩, [], 1⟩⟩ := ControlAudit.loop_ct_refused.2.2.2

/-- Emit an unreleased secret, then attempt a missing call. -/
def outputThenStuck (secret : Int) : IntegerEntry :=
  ⟨.seq (.prim none .output [.secret] ⟨7⟩) (IntegerStatement.encode missingCall), [], secret⟩

/-- The output and variable-time leakage both precede the stuck call. -/
theorem output_then_stuck_prefix (secret : Int) :
    InstrumentedSteps instrumentation program (outputThenStuck secret).config
      [.output secret] [.variableTime ⟨7⟩ [secret]] [] [] ⟨.exec missingCall, [], secret, []⟩ := by
  let pending : Config ops := ⟨.exec (.prim none .output [.secret] ⟨7⟩), [], secret, [.seq missingCall]⟩
  let emitted : Config ops := ⟨.done, [], secret, [.seq missingCall]⟩
  have emit : AnsweredStep program pending (.emit secret) emitted
      (some ⟨some secret, .ok 0, secret⟩) :=
    AnsweredStep.prim (P := program) (c := pending) (c' := emitted)
      (p := .output) (site := ⟨7⟩) (vs := [secret]) (st := secret)
      (a := (⟨some secret, .ok 0, secret⟩ : Answer Int Int Int)) rfl (by simp [allowed]) rfl
  exact .step (.silent rfl) (.step emit (.step (.silent rfl) (.refl _)))

/-- Instance of `ni_prefixes_compatible`: the public output differs before stuckness. -/
theorem stuckNIRefused : RuleRefused IntegerDelimitedReleaseNI
    ⟨[], outputThenStuck 1, outputThenStuck 2⟩ := by
  intro h
  have bad := (ni_prefixes_compatible h (output_then_stuck_prefix 1) (output_then_stuck_prefix 2)
    ⟨rfl, trivial⟩ rfl rfl).1
  simp [PrefixCompatible] at bad

/-- Instance of `ct_prefixes_compatible`: variable-time leakage differs before stuckness. -/
theorem stuckCTRefused : RuleRefused IntegerConstantTime
    ⟨[], outputThenStuck 1, outputThenStuck 2⟩ := by
  intro h
  have bad := ct_prefixes_compatible h (output_then_stuck_prefix 1) (output_then_stuck_prefix 2)
    ⟨rfl, trivial⟩ rfl rfl
  simp [PrefixCompatible] at bad

/-- Planted (Laws `ni_prefixes_compatible` and `ct_prefixes_compatible`):
keep qualification and completed-run equality, deleting only the prefix obligations. -/
theorem completed_only_admits_stuck_output :
    ObserverQualified instrumentation ∧
    CompletedNIComparison instrumentation program labelling
      (outputThenStuck 1).config (outputThenStuck 2).config ∧
    CompletedTimingComparison instrumentation program labelling
      (outputThenStuck 1).config (outputThenStuck 2).config ∧
    ¬ IntegerDelimitedReleaseNI ⟨[], outputThenStuck 1, outputThenStuck 2⟩ ∧
    ¬ IntegerConstantTime ⟨[], outputThenStuck 1, outputThenStuck 2⟩ := by
  have absent := no_complete_run_from_stuck_prefix (output_then_stuck_prefix 1) rfl rfl
  exact ⟨instrumentation_qualified, fun r _ _ _ _ => (absent ⟨r⟩).elim,
    fun r _ _ _ _ => (absent ⟨r⟩).elim, stuckNIRefused, stuckCTRefused⟩

end Leak.Registry
end ConcreteSpec.Core
