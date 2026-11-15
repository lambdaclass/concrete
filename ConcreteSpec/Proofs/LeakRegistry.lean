import ConcreteSpec.Spec.LeakData
import ConcreteSpec.Spec.ReleaseCalls
import ConcreteSpec.Proofs.Leak
import ConcreteSpec.Proofs.ReleaseCalls
import ConcreteSpec.Spec.Basics
import ConcreteSpec.Spec.Exec
import ConcreteSpec.Spec.Ident
import ConcreteSpec.Spec.Leak
import ConcreteSpec.Spec.Rules

/-! C7 registry conformance evidence. The specification registry stores names only;
the exporter loads these kernel-checked proofs without making a Spec module import Proofs. -/
namespace ConcreteSpec.Core

namespace Leak.Registry
open Demo

/-- Instance: the concrete call graph passes the full function and site policy. -/
theorem callsAccepted : RuleAccepted UnitCallsAdmitted (CallControls.allAllowed, CallControls.chain) := CallControls.admitted
/-- Instance: omitting only the outer caller refuses the same two-level release chain. -/
theorem callsRefused : RuleRefused UnitCallsAdmitted (CallControls.outerDenied, CallControls.chain) := CallControls.refused

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
theorem permitsAccepted : RuleAccepted StringReleasePermitted (typedReleasePolicy, typedRelease "public" 7) := by simp [RuleAccepted, StringReleasePermitted, ReleasePolicy.Permits, typedReleasePolicy, typedRelease]
/-- Instance: changing only the type at that site is refused. -/
theorem permitsRefused : RuleRefused StringReleasePermitted (typedReleasePolicy, typedRelease "secret" 7) := by simp [RuleRefused, StringReleasePermitted, ReleasePolicy.Permits, typedReleasePolicy, typedRelease]
/-- Instance: an empty policy forbids releases. -/
theorem forbidsAccepted : RuleAccepted UnitReleaseForbidden ⟨[]⟩ := rfl
/-- Instance: the populated policy does not forbid release. -/
theorem forbidsRefused : RuleRefused UnitReleaseForbidden policy := by
  simp [RuleRefused, UnitReleaseForbidden, ReleasePolicy.ForbidsRelease]
/-- Instance: both explicit releases are authorized. -/
theorem authorizedAccepted : RuleAccepted UnitTraceAuthorized (policy, [releaseValue 7, releaseValue 8]) := by
  simp [RuleAccepted, UnitTraceAuthorized, ReleasePolicy.Authorized, ReleasePolicy.Permits, releaseValue]
/-- Instance: an empty policy rejects the same populated trace. -/
theorem authorizedRefused : RuleRefused UnitTraceAuthorized (⟨[]⟩, [releaseValue 7, releaseValue 8]) :=
  forbidsRelease_not_authorized ⟨[]⟩ (releaseValue 7) [releaseValue 8] rfl
/-- Instance: authorization quantifies over every complete execution of this concrete entry. -/
theorem runsAccepted : RuleAccepted IntegerRunsAuthorized
    (⟨[⟨⟨0⟩, ()⟩]⟩, ⟨[], ⟨.seq (.prim none .release [.lit 0] ⟨0⟩) (.prim none .output [.secret] ⟨1⟩), [], 1⟩⟩) := release_authorized 1
/-- Instance: the same populated execution is refused by the empty policy. -/
theorem runsRefused : RuleRefused IntegerRunsAuthorized
    (⟨[]⟩, ⟨[], ⟨.seq (.prim none .release [.lit 0] ⟨0⟩) (.prim none .output [.secret] ⟨1⟩), [], 1⟩⟩) := by
  intro h
  have bad := h (leakyRun 1)
  change ReleasePolicy.Authorized ⟨[]⟩ [releaseValue 0] at bad
  simp [ReleasePolicy.Authorized, ReleasePolicy.Permits] at bad
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
    ⟨[], ⟨.seq (.prim none .release [.masked] ⟨0⟩) (.prim none .output [.masked] ⟨1⟩), [(⟨0⟩, .negSucc 1)], 6⟩,
     ⟨.seq (.prim none .release [.masked] ⟨0⟩) (.prim none .output [.masked] ⟨1⟩), [(⟨0⟩, .negSucc 2)], 5⟩⟩ :=
  secure_ni 6 5 1 2 ⟨rfl, trivial⟩ ⟨secureRun 6 1, secureRun 5 2, rfl, rfl⟩
/-- Instance: an unreleased output is refused despite authorized releases. -/
theorem niRefused : RuleRefused IntegerDelimitedReleaseNI
    ⟨[], ⟨.seq (.prim none .release [.lit 0] ⟨0⟩) (.prim none .output [.secret] ⟨1⟩), [], 1⟩, ⟨.seq (.prim none .release [.lit 0] ⟨0⟩) (.prim none .output [.secret] ⟨1⟩), [], 2⟩⟩ :=
  authorized_release_not_ni 1 2 (release_authorized 1) (release_authorized 2) (leaky_low 1 2) (by decide)
/-- Instance: equal sums independently satisfy CT. -/
theorem ctAccepted : RuleAccepted IntegerConstantTime
    ⟨[], ⟨.seq (.prim none .release [.masked] ⟨0⟩) (.prim none .output [.masked] ⟨1⟩), [(⟨0⟩, .negSucc 1)], 6⟩,
     ⟨.seq (.prim none .release [.masked] ⟨0⟩) (.prim none .output [.masked] ⟨1⟩), [(⟨0⟩, .negSucc 2)], 5⟩⟩ :=
  secure_ct 6 5 1 2 ⟨rfl, trivial⟩ ⟨secureRun 6 1, secureRun 5 2, rfl, rfl⟩
/-- Instance: variable-time work leaks while public output agrees. -/
theorem ctRefused : RuleRefused IntegerConstantTime
    ⟨[], ⟨.seq (.prim none .work [.secret] ⟨2⟩) (.seq (.prim none .release [.lit 0] ⟨0⟩) (.prim none .output [.lit 7] ⟨1⟩)), [], 1⟩, ⟨.seq (.prim none .work [.secret] ⟨2⟩) (.seq (.prim none .release [.lit 0] ⟨0⟩) (.prim none .output [.lit 7] ⟨1⟩)), [], 2⟩⟩ :=
  timing_not_ct 1 2 ⟨rfl, trivial⟩ (by decide)

end Leak.Registry
end ConcreteSpec.Core
