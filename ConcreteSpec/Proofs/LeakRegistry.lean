import ConcreteSpec.Proofs.Leak
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

/-- Instance: the listed typed site is permitted. -/
theorem permitsAccepted : RuleAccepted (ReleasePolicy.Permits typedReleasePolicy)
    (typedRelease "public" 7) := by simp [RuleAccepted, ReleasePolicy.Permits, typedReleasePolicy, typedRelease]
/-- Instance: changing only the type at that site is refused. -/
theorem permitsRefused : RuleRefused (ReleasePolicy.Permits typedReleasePolicy)
    (typedRelease "secret" 7) := by simp [RuleRefused, ReleasePolicy.Permits, typedReleasePolicy, typedRelease]
/-- Instance: an empty policy forbids releases. -/
theorem forbidsAccepted : RuleAccepted (ReleasePolicy.ForbidsRelease (Ty := Unit)) ⟨[]⟩ := rfl
/-- Instance: the populated policy does not forbid release. -/
theorem forbidsRefused : RuleRefused (ReleasePolicy.ForbidsRelease (Ty := Unit)) policy := by
  simp [RuleRefused, ReleasePolicy.ForbidsRelease]
/-- Instance: both explicit releases are authorized. -/
theorem authorizedAccepted : RuleAccepted (ReleasePolicy.Authorized policy)
    [releaseValue 7, releaseValue 8] := by
  simp [RuleAccepted, ReleasePolicy.Authorized, ReleasePolicy.Permits, releaseValue]
/-- Instance: an empty policy rejects the same populated trace. -/
theorem authorizedRefused : RuleRefused (ReleasePolicy.Authorized (ReleasePolicy.mk ([] : List (ReleaseSite Unit))))
    [releaseValue 7, releaseValue 8] :=
  forbidsRelease_not_authorized ⟨[]⟩ (releaseValue 7) [releaseValue 8] rfl
/-- Instance: authorization quantifies over all complete runs. -/
theorem runsAccepted : RuleAccepted (ReleasePolicy.RunsAuthorized policy instrumentation program) (leakyStart 1) :=
  release_authorized 1
/-- Instance: a real run witnesses the refusal under an empty policy. -/
theorem runsRefused : RuleRefused
    (ReleasePolicy.RunsAuthorized (ReleasePolicy.mk ([] : List (ReleaseSite Unit))) instrumentation program)
    (leakyStart 1) := by
  intro h
  have bad := h (leakyRun 1)
  simp [leakyRun, ReleasePolicy.Authorized, ReleasePolicy.Permits] at bad
/-- Instance: changing only secret state preserves the public view. -/
theorem lowAccepted : RuleAccepted (LowEquivalent labelling (mixedConfig (-1) 7)) (mixedConfig (-2) 7) := by
  simp [RuleAccepted, LowEquivalent, publicLocals, labelling, mixedConfig]
/-- Instance: changing a public binding breaks the public view. -/
theorem lowRefused : RuleRefused (LowEquivalent labelling (mixedConfig (-1) 7)) (mixedConfig (-2) 8) := by
  simp [RuleRefused, LowEquivalent, publicLocals, labelling, mixedConfig]
/-- Instance: distinct secret pairs with equal released sums satisfy NI. -/
theorem niAccepted : RuleAccepted (DelimitedReleaseNI instrumentation program labelling (start 6 1)) (start 5 2) :=
  secure_ni 6 5 1 2 ⟨rfl, trivial⟩ ⟨secureRun 6 1, secureRun 5 2, rfl, rfl⟩
/-- Instance: authorization does not excuse an unreleased public output. -/
theorem niRefused : RuleRefused (DelimitedReleaseNI instrumentation program labelling (leakyStart 1)) (leakyStart 2) :=
  authorized_release_not_ni 1 2 (release_authorized 1) (release_authorized 2) (leaky_low 1 2) (by decide)
/-- Instance: equal released sums also satisfy the separate leakage claim. -/
theorem ctAccepted : RuleAccepted (ConstantTime instrumentation program labelling (start 6 1)) (start 5 2) :=
  secure_ct 6 5 1 2 ⟨rfl, trivial⟩ ⟨secureRun 6 1, secureRun 5 2, rfl, rfl⟩
/-- Instance: public output can agree while variable-time work leaks. -/
theorem ctRefused : RuleRefused (ConstantTime instrumentation program labelling (timingStart 1)) (timingStart 2) :=
  timing_not_ct 1 2 ⟨rfl, trivial⟩ (by decide)
/-- Instance: populated history bounds live bytes. -/
theorem wfAccepted : RuleAccepted Account.WF accountA := by
  simp only [RuleAccepted, Account.WF]
  decide
/-- Instance: a local peak below live bytes is refused. -/
theorem wfRefused : RuleRefused Account.WF { accountA with functionPeak := 4 } := by
  simp only [RuleRefused, Account.WF]
  decide
/-- Instance: reset records its actual outflow. -/
theorem resetAccepted : RuleAccepted (ResetConservation accountA accountA.reset) 5 := by
  simp only [RuleAccepted, ResetConservation]
  decide
/-- Instance: an incorrect outflow cannot certify the same reset. -/
theorem resetRefused : RuleRefused (ResetConservation accountA accountA.reset) 4 := by
  simp only [RuleRefused, ResetConservation]
  decide
/-- Instance: an established bound covers the actual bytes. -/
theorem boundAccepted : RuleAccepted (Bound.Sound (10 : Nat)) ⟨12, .boundEstablished⟩ := by
  simp [RuleAccepted, Bound.Sound]
/-- Instance: an established underestimate is refused. -/
theorem boundRefused : RuleRefused (Bound.Sound (10 : Nat)) ⟨9, .boundEstablished⟩ := by
  simp [RuleRefused, Bound.Sound]
/-- Instance: a defined allocator footprint supports evidence. -/
theorem optionalAccepted : RuleAccepted (Bound.SoundOptional (some (10 : Nat))) ⟨12, .boundEstablished⟩ := by
  simp [RuleAccepted, Bound.SoundOptional]
/-- Instance: an absent footprint cannot be established. -/
theorem optionalRefused : RuleRefused (Bound.SoundOptional (none : Option Nat)) ⟨12, .boundEstablished⟩ := by
  simp [RuleRefused, Bound.SoundOptional]
/-- Instance: a report distinguishes function-local peak 8 from historical peak 100. -/
theorem reportAccepted : RuleAccepted (LineageReport.Sound ⟨0⟩ reportEntry reportExit (some 10)) establishedReport := by
  simp [RuleAccepted, LineageReport.Sound, Bound.Sound, SignedInterval.Sound, Bound.SoundOptional, establishedReport, reportEntry, reportExit]
/-- Instance: correct numbers cannot be attributed to another lineage. -/
theorem reportRefused : RuleRefused (LineageReport.Sound ⟨1⟩ reportEntry reportExit (some 10)) establishedReport := by
  simp [RuleRefused, LineageReport.Sound, establishedReport]

/-- Instance: a populated lineage admits an allocation without affecting another lineage. -/
theorem actionAccepted : RuleAccepted (ActionPermitted memoryA) (.alloc ⟨0⟩ 8) := rfl
/-- Instance: the same lineage refuses an excessive free. -/
theorem actionRefused : RuleRefused (ActionPermitted memoryA) (.free ⟨0⟩ 6) := by
  simp [RuleRefused, ActionPermitted, apply, memoryA, Memory.set, accountA]
/-- Instance: allocation, free and reset compose through the production fold. -/
theorem sequenceAccepted : RuleAccepted (SequencePermitted memoryA) conservationActions := rfl
/-- Instance: a bad middle free refuses the whole sequence even though its first action succeeds. -/
theorem sequenceRefused : RuleRefused (SequencePermitted memoryA)
    [.alloc ⟨0⟩ 8, .free ⟨0⟩ 14, .reset ⟨0⟩] := by
  change ¬ (false = true)
  decide

end Leak.Registry

end ConcreteSpec.Core
