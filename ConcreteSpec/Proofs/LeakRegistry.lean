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
    simpa [integerProgram, ReleaseCalls.ofProgram, statementReleases, IntegerStatement.statement] using hr
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
/-- Instance: authorization quantifies over every complete execution of this concrete entry. -/
theorem runsAccepted : RuleAccepted IntegerRunsAuthorized
    (⟨[⟨⟨0⟩, ()⟩]⟩, ⟨[], unreleasedOutput 1⟩) := release_authorized 1
/-- Instance: the same populated execution is refused by the empty policy. -/
theorem runsRefused : RuleRefused IntegerRunsAuthorized
    (⟨[]⟩, ⟨[], unreleasedOutput 1⟩) := by
  intro h
  have bad := h (leakyRun 1)
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
  refine ⟨done, .normal, rfl, I.observeStep c .silent done, I.leakage c .silent done,
    I.releases c .silent done, I.choices c .silent done, ?_⟩
  simpa only [List.append_nil, Answer.label] using InstrumentedSteps.step (I := I)
    (Step.prim (P := program) (c := c) (c' := done) (p := .release) (site := site)
      (vs := [42]) (st := 0) (a := (⟨none, .ok 0, 0⟩ : Answer Int Int Int))
      rfl (by simp [allowed]) rfl) (InstrumentedSteps.refl done)

/-- Instance: a nonzero site and nonzero operand inhabit the agreement Law. -/
example :
    (instrumentation.releases (releaseEntry ⟨7⟩).config .silent ⟨.done, [], 0, []⟩).map Release.site =
    statementReleases (fun p : Prim => match p with | .release => some () | _ => none)
      (.prim none .release [.lit 42] ⟨7⟩ : ConcreteSpec.Core.Exec.Stmt ops) :=
  release_sites_agree ⟨7⟩ [.lit 42] [] 0 0 42 .silent _ none [] rfl

/-- Planted: replace only the source site with zero, preserving the evaluated operand. -/
abbrev zeroSite : Instrumentation ops := { instrumentation with
  releases := fun c _ _ => match request c with
    | some (.release, [v], _) => [releaseAt ⟨0⟩ v]
    | _ => [] }

/-- Planted: the same inhabited evaluation refutes site agreement for the zero-site observer. -/
example : evalArgs ops [] 0 [.lit 42] = .ok ([42], 0) ∧
    (zeroSite.releases (releaseEntry ⟨7⟩).config .silent ⟨.done, [], 0, []⟩).map Release.site ≠
    statementReleases (fun p : Prim => match p with | .release => some () | _ => none)
      (.prim none .release [.lit 42] ⟨7⟩ : ConcreteSpec.Core.Exec.Stmt ops) := by decide

/-- Instance: all complete executions at site seven pass the site-seven policy. -/
theorem nonzeroRunsAccepted : RuleAccepted IntegerRunsAuthorized
    (⟨[⟨⟨7⟩, ()⟩]⟩, ⟨[], releaseEntry ⟨7⟩⟩) := by
  intro r
  have same := (complete_unique r (siteRun instrumentation ⟨7⟩)).2.2.1
  rw [same]
  change ReleasePolicy.Authorized ⟨[⟨⟨7⟩, ()⟩]⟩ [releaseAt ⟨7⟩ 42]
  simp [ReleasePolicy.Authorized, ReleasePolicy.Permits, releaseAt]

/-- Instance: changing only the policy site to zero refuses that same populated execution. -/
theorem nonzeroRunsRefused : RuleRefused IntegerRunsAuthorized
    (⟨[⟨⟨0⟩, ()⟩]⟩, ⟨[], releaseEntry ⟨7⟩⟩) := by
  intro h
  have bad := h (siteRun instrumentation ⟨7⟩)
  change ReleasePolicy.Authorized ⟨[⟨⟨0⟩, ()⟩]⟩ [releaseAt ⟨7⟩ 42] at bad
  simp [ReleasePolicy.Authorized, ReleasePolicy.Permits, releaseAt] at bad

/-- Planted: the zero-site substitution wrongly authorizes every run under the site-zero policy. -/
example : policy.RunsAuthorized zeroSite program (releaseEntry ⟨7⟩).config := by
  intro r
  rw [(complete_unique r (siteRun zeroSite ⟨7⟩)).2.2.1]
  change ReleasePolicy.Authorized policy [releaseAt ⟨0⟩ 42]
  simp [ReleasePolicy.Authorized, ReleasePolicy.Permits, releaseAt]

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

end Leak.Registry
end ConcreteSpec.Core
