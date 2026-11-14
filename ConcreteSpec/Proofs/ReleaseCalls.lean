import ConcreteSpec.Spec.Basics
import ConcreteSpec.Spec.Exec
import ConcreteSpec.Spec.Ident
import ConcreteSpec.Spec.Leak
import ConcreteSpec.Spec.ReleaseCalls
import ConcreteSpec.Spec.LeakData

/-! Call-chain admission laws and isolated wrong-definition controls.
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7). -/
namespace ConcreteSpec.Core.Leak
open Exec

/-- Law: an unlisted caller cannot reach a release in an admitted program, at any call depth.
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7). -/
theorem unlisted_caller_refused {Ty : Type} (p : ReleaseInterface Ty) (g : ReleaseCalls Ty)
    (caller owner : FnId) (site : ReleaseSite Ty) (hcall : g.Reaches caller owner)
    (hrelease : (owner, site) ∈ g.releases) (hnot : caller ∉ p.functions) : ¬ p.Admitted g := by
  intro h
  exact hnot (h caller owner site hcall hrelease).1

namespace CallControls
/-- Nominal function identities in one fixed control module. -/
def fn (name : String) : FnId :=
  ⟨⟨⟨⟨"c7"⟩, ⟨⟨"1"⟩⟩⟩, [⟨"calls"⟩], by decide⟩, ⟨name⟩⟩
/-- Data for the chain 0 → 1 → 2; only function 2 releases, at site 7. -/
def chain : ReleaseCalls Unit := ⟨[((fn "0"), (fn "1")), ((fn "1"), (fn "2"))], [((fn "2"), ⟨⟨7⟩, ()⟩)]⟩
/-- The full function and site allowlist. -/
def allAllowed : ReleaseInterface Unit := ⟨[(fn "0"), (fn "1"), (fn "2")], ⟨[⟨⟨7⟩, ()⟩]⟩⟩
/-- Only the outer caller is absent; direct release owner and intermediate caller remain listed. -/
def outerDenied : ReleaseInterface Unit := ⟨[(fn "1"), (fn "2")], ⟨[⟨⟨7⟩, ()⟩]⟩⟩

/-- Helper: every source of an edge in this concrete graph is listed in the full policy. -/
theorem chain_source {f g : FnId} (h : (f, g) ∈ chain.calls) : f ∈ allAllowed.functions := by
  simp [chain] at h
  rcases h with ⟨rfl, _⟩ | ⟨rfl, _⟩ <;> simp [allAllowed]

/-- Instance: all callers and the explicit typed site are permitted. -/
theorem admitted : allAllowed.Admitted chain := by
  intro caller owner site reach release
  have hr : owner = (fn "2") ∧ site = ⟨⟨7⟩, ()⟩ := by simpa [chain] using release
  obtain ⟨rfl, rfl⟩ := hr
  constructor
  · cases reach with
    | self => simp [allAllowed]
    | call edge _ => exact chain_source edge
  · simp [allAllowed, ReleasePolicy.Permits]

/-- Helper: two edges establish reachability to the release, rather than a direct-release witness. -/
theorem twoLevels : chain.Reaches (fn "0") (fn "2") :=
  .call (middle := fn "1") (by simp [chain])
    (.call (middle := fn "2") (by simp [chain]) (.self _))

/-- Instance: withholding only function 0 refuses the two-level call chain. -/
theorem refused : ¬ outerDenied.Admitted chain :=
  unlisted_caller_refused outerDenied chain (fn "0") (fn "2") ⟨⟨7⟩, ()⟩ twoLevels
    (by simp [chain]) (by decide)

/-- Wrong definition: checks the release owner but omits the transitive callers. -/
def directOnly (p : ReleaseInterface Unit) (g : ReleaseCalls Unit) : Prop :=
  ∀ owner site, (owner, site) ∈ g.releases →
    owner ∈ p.functions ∧ p.sites.Permits (⟨site, ()⟩ : Release Unit Unit)

/-- Planted: unlisted_caller_refused rejects direct-only checking on an inhabited two-level chain.
Only the transitive caller check is removed; the site and direct owner still pass. -/
theorem kills_directOnly : directOnly outerDenied chain ∧ ¬ outerDenied.Admitted chain := by
  constructor
  · intro owner site h
    have hr : owner = (fn "2") ∧ site = ⟨⟨7⟩, ()⟩ := by simpa [chain] using h
    obtain ⟨rfl, rfl⟩ := hr
    simp [outerDenied, ReleasePolicy.Permits]
  · exact refused

/-- Wrong definition: retains reachability and site checks, but ignores the function allowlist. -/
def ignoresFunctions (p : ReleaseInterface Unit) (g : ReleaseCalls Unit) : Prop :=
  ∀ caller owner site, g.Reaches caller owner → (owner, site) ∈ g.releases →
    p.sites.Permits (⟨site, ()⟩ : Release Unit Unit)

/-- Planted: unlisted_caller_refused rejects ignoring the function allowlist.
The exact same graph, site and declared value permissions remain in force. -/
theorem kills_ignoresFunctions : ignoresFunctions outerDenied chain ∧ ¬ outerDenied.Admitted chain := by
  constructor
  · intro _ owner site _ h
    have hr : owner = (fn "2") ∧ site = ⟨⟨7⟩, ()⟩ := by simpa [chain] using h
    obtain ⟨rfl, rfl⟩ := hr
    simp [outerDenied, ReleasePolicy.Permits]
  · exact refused

/-- A real C1 program with two ordinary calls and a declared release primitive. -/
def program : Program Demo.ops := ⟨[
  (fn "0", ⟨[], (), ⟨0⟩, .call none (fn "1") [] ⟨3⟩⟩),
  (fn "1", ⟨[], (), ⟨1⟩, .call none (fn "2") [] ⟨4⟩⟩),
  (fn "2", ⟨[], (), ⟨2⟩, .prim none .release [.lit 7] ⟨7⟩⟩)]⟩

/-- Representation lemma: the control graph is extracted from actual C1 call and release syntax. -/
theorem extracted : ReleaseCalls.ofProgram
    (fun p : Demo.Prim => match p with | .release => some () | _ => none) program = chain := rfl

/-- Instance: the real C1 program passes with all functions listed. -/
example : allAllowed.ProgramAdmitted
    (fun p : Demo.Prim => match p with | .release => some () | _ => none) program := admitted

/-- Instance: the real C1 program is refused when its outer caller is absent. -/
example : ¬ outerDenied.ProgramAdmitted
    (fun p : Demo.Prim => match p with | .release => some () | _ => none) program := refused

/-- Representation lemma: deferred calls are included, even inside a branch and loop region. -/
example : statementCalls (O := Demo.ops)
    (.block ⟨0⟩ (.cond (.lit 1) (.defer (fn "1") [] ⟨3⟩)
      (.loop ⟨0⟩ (.call none (fn "2") [] ⟨4⟩) (.lit 1) .skip ⟨5⟩) ⟨6⟩)) = [fn "1", fn "2"] := rfl
end CallControls
end ConcreteSpec.Core.Leak
