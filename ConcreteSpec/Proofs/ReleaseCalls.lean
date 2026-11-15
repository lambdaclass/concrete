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

/-- Law: reaching an unlisted typed release site refuses admission even when every function is listed.
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7). -/
theorem unlisted_site_refused {Ty : Type} (p : ReleaseInterface Ty) (g : ReleaseCalls Ty)
    (caller owner : FnId) (site : ReleaseSite Ty) (hc : g.Reaches caller owner)
    (hr : (owner, site) ∈ g.releases)
    (hn : ¬ p.sites.Permits (⟨site, ()⟩ : Release Ty Unit)) : ¬ p.Admitted g :=
  fun h => hn (h caller owner site hc hr).2

/-- Law: a forbid-release policy refuses every reachable release, independently of its value.
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7). -/
theorem forbidden_release_refused {Ty : Type} (p : ReleaseInterface Ty) (g : ReleaseCalls Ty)
    (caller owner : FnId) (site : ReleaseSite Ty) (hc : g.Reaches caller owner)
    (hr : (owner, site) ∈ g.releases) (hf : p.sites.ForbidsRelease) : ¬ p.Admitted g := by
  apply unlisted_site_refused p g caller owner site hc hr
  simp [ReleasePolicy.Permits, show p.sites.allowed = [] from hf]

/-- Law: C1 program admission refuses an unlisted caller reaching a declared release through
ordinary/deferred calls and nested regions in the extracted graph.
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7). -/
theorem unlisted_program_caller_refused {O : Operands} (p : ReleaseInterface O.Ty)
    (declared : O.Prim → Option O.Ty) (program : Program O) (caller owner : FnId)
    (site : ReleaseSite O.Ty)
    (hc : (ReleaseCalls.ofProgram declared program).Reaches caller owner)
    (hr : (owner, site) ∈ (ReleaseCalls.ofProgram declared program).releases)
    (hn : caller ∉ p.functions) : ¬ p.ProgramAdmitted declared program :=
  unlisted_caller_refused p _ caller owner site hc hr hn

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
theorem kills_directOnly : chain.Reaches (fn "0") (fn "2") ∧
  (fn "2", ⟨⟨7⟩, ()⟩) ∈ chain.releases ∧ fn "0" ∉ outerDenied.functions ∧
  directOnly outerDenied chain ∧ ¬ outerDenied.Admitted chain := by
  refine ⟨twoLevels, by simp [chain], by decide, ?_, refused⟩
  intro owner site h
  have hr : owner = (fn "2") ∧ site = ⟨⟨7⟩, ()⟩ := by simpa [chain] using h
  obtain ⟨rfl, rfl⟩ := hr
  simp [outerDenied, ReleasePolicy.Permits]


/-- Wrong definition: retains reachability and site checks, but ignores the function allowlist. -/
def ignoresFunctions (p : ReleaseInterface Unit) (g : ReleaseCalls Unit) : Prop :=
  ∀ caller owner site, g.Reaches caller owner → (owner, site) ∈ g.releases →
    p.sites.Permits (⟨site, ()⟩ : Release Unit Unit)

/-- Planted: unlisted_caller_refused rejects ignoring the function allowlist.
The exact same graph, site and declared value permissions remain in force. -/
theorem kills_ignoresFunctions : chain.Reaches (fn "0") (fn "2") ∧
  (fn "2", ⟨⟨7⟩, ()⟩) ∈ chain.releases ∧ fn "0" ∉ outerDenied.functions ∧
  ignoresFunctions outerDenied chain ∧ ¬ outerDenied.Admitted chain := by
  refine ⟨twoLevels, by simp [chain], by decide, ?_, refused⟩
  intro _ owner site _ h
  have hr : owner = (fn "2") ∧ site = ⟨⟨7⟩, ()⟩ := by simpa [chain] using h
  obtain ⟨rfl, rfl⟩ := hr
  simp [outerDenied, ReleasePolicy.Permits]


/-- Planted (law `unlisted_caller_refused`): an empty function allowlist forbids a populated release chain.
Only the function conjunct is removed in the mutant; the typed site remains permitted. -/
example : let p : ReleaseInterface Unit := { allAllowed with functions := [] }
    chain.Reaches (fn "0") (fn "2") ∧ (fn "2", ⟨⟨7⟩, ()⟩) ∈ chain.releases ∧
    fn "0" ∉ p.functions ∧ ignoresFunctions p chain ∧ ¬ p.Admitted chain := by
  dsimp only
  refine ⟨twoLevels, by simp [chain], by simp, ?_, ?_⟩
  · intro caller owner site hc hr
    exact (admitted caller owner site hc hr).2
  · exact unlisted_caller_refused _ chain (fn "0") (fn "2") ⟨⟨7⟩, ()⟩ twoLevels
      (by simp [chain]) (by simp)

/-- Instance: changing only the type at a listed numeric site refuses the same reachable owner.
The allowed site is public; the declared site is secret. -/
example : let p : ReleaseInterface String := ⟨[fn "2"], ⟨[⟨⟨7⟩, "public"⟩]⟩⟩
    let g : ReleaseCalls String := ⟨[], [(fn "2", ⟨⟨7⟩, "secret"⟩)]⟩
    ¬ p.Admitted g := by
  dsimp only
  apply unlisted_site_refused _ _ (fn "2") (fn "2") ⟨⟨7⟩, "secret"⟩ (.self _)
  · simp
  · simp [ReleasePolicy.Permits]

/-- A populated function allowlist with no permitted typed sites. -/
def siteDenied : ReleaseInterface Unit := { allAllowed with sites := ⟨[]⟩ }

/-- Wrong definition: deletes only the typed-site policy conjunct. -/
def ignoresSites (p : ReleaseInterface Unit) (g : ReleaseCalls Unit) : Prop :=
  ∀ caller owner site, g.Reaches caller owner → (owner, site) ∈ g.releases → caller ∈ p.functions

/-- Instance: the typed-site law refuses a reachable release with every function listed. -/
theorem site_refused : ¬ siteDenied.Admitted chain :=
  unlisted_site_refused siteDenied chain (fn "0") (fn "2") ⟨⟨7⟩, ()⟩ twoLevels
    (by simp [chain]) (by simp [siteDenied, ReleasePolicy.Permits])

/-- Instance: the same populated graph is refused by the forbid-release law. -/
example : ¬ siteDenied.Admitted chain :=
  forbidden_release_refused siteDenied chain (fn "0") (fn "2") ⟨⟨7⟩, ()⟩ twoLevels
    (by simp [chain]) rfl

/-- Planted: both typed-site and forbid-release laws reject the ignored-site mutant.
Their reachability, declared release, missing permission and forbidding premises are inhabited. -/
example : chain.Reaches (fn "0") (fn "2") ∧ (fn "2", ⟨⟨7⟩, ()⟩) ∈ chain.releases ∧
    (¬ siteDenied.sites.Permits (⟨⟨⟨7⟩, ()⟩, ()⟩ : Release Unit Unit)) ∧
    siteDenied.sites.ForbidsRelease ∧ ignoresSites siteDenied chain ∧ ¬ siteDenied.Admitted chain := by
  refine ⟨twoLevels, by simp [chain], by simp [siteDenied, ReleasePolicy.Permits], rfl, ?_, site_refused⟩
  intro caller owner site hc hr
  exact (admitted caller owner site hc hr).1

/-- A real C1 program with two ordinary calls and a declared release primitive. -/
def program : Program Demo.ops := ⟨[
  (fn "0", ⟨[], (), ⟨0⟩, .block ⟨3⟩ (.cond (.lit 1) (.loop ⟨0⟩ .skip (.lit 1)
    (.defer (fn "1") [] ⟨3⟩) ⟨5⟩) .skip ⟨6⟩)⟩),
  (fn "1", ⟨[], (), ⟨1⟩, .call none (fn "2") [] ⟨4⟩⟩),
  (fn "2", ⟨[], (), ⟨2⟩, .block ⟨2⟩ (.prim none .release [.lit 7] ⟨7⟩)⟩)]⟩

/-- Representation lemma: the control graph is extracted from actual C1 call and release syntax. -/
theorem extracted : ReleaseCalls.ofProgram
    (fun p : Demo.Prim => match p with | .release => some () | _ => none) program = chain := rfl

/-- Instance: the real C1 program passes with all functions listed. -/
example : allAllowed.ProgramAdmitted
    (fun p : Demo.Prim => match p with | .release => some () | _ => none) program := admitted

/-- Instance: the real C1 program is refused when its outer caller is absent. -/
example : ¬ outerDenied.ProgramAdmitted
    (fun p : Demo.Prim => match p with | .release => some () | _ => none) program :=
  unlisted_program_caller_refused (O := Demo.ops) outerDenied
    (fun p : Demo.Prim => match p with | .release => some () | _ => none) program (fn "0") (fn "2") ⟨⟨7⟩, ()⟩
    twoLevels (by change (fn "2", ⟨⟨7⟩, ()⟩) ∈ chain.releases; simp [chain]) (by decide)

/-- Representation lemma: deferred calls are included, even inside a branch and loop region. -/
example : statementCalls (O := Demo.ops)
    (.block ⟨0⟩ (.cond (.lit 1) (.defer (fn "1") [] ⟨3⟩)
      (.loop ⟨0⟩ (.call none (fn "2") [] ⟨4⟩) (.lit 1) .skip ⟨5⟩) ⟨6⟩)) = [fn "1", fn "2"] := rfl
/-- Wrong extraction fold: drops exactly the named constructor; every other traversal is unchanged. -/
def droppedCalls (drop : Nat) : Stmt Demo.ops → List FnId
  | .call _ f _ _ => [f]
  | .defer f _ _ => if drop = 0 then [] else [f]
  | .seq s t => droppedCalls drop s ++ droppedCalls drop t
  | .cond _ s t _ => if drop = 1 then [] else droppedCalls drop s ++ droppedCalls drop t
  | .loop _ s _ t _ => if drop = 2 then [] else droppedCalls drop s ++ droppedCalls drop t
  | .block _ s => if drop = 3 then [] else droppedCalls drop s
  | _ => []

/-- Substitute the faulty traversal throughout program admission, retaining release extraction. -/
def droppedGraph (drop : Nat) : ReleaseCalls Unit :=
  { ReleaseCalls.ofProgram (fun p : Demo.Prim => match p with | .release => some () | _ => none) program with
    calls := program.fns.flatMap fun (f, body) => (droppedCalls drop body.body).map fun g => (f, g) }

/-- Helper: after losing the outer edge, the remaining release callers are listed. -/
theorem dropped_admitted (drop : Nat) (hd : drop < 4) : outerDenied.Admitted (droppedGraph drop) := by
  intro caller owner site hc hr
  have hg : droppedGraph drop = { chain with calls := [(fn "1", fn "2")] } := by
    have cases4 : drop = 0 ∨ drop = 1 ∨ drop = 2 ∨ drop = 3 := by omega
    rcases cases4 with rfl | rfl | rfl | rfl <;> rfl
  rw [hg] at hc hr
  have hs : owner = fn "2" ∧ site = ⟨⟨7⟩, ()⟩ := by simpa [chain] using hr
  obtain ⟨rfl, rfl⟩ := hs
  constructor
  · cases hc with
    | self => simp [outerDenied]
    | call edge _ =>
      have he : caller = fn "1" := congrArg Prod.fst (List.mem_singleton.mp edge)
      simp [he, outerDenied]
  · simp [outerDenied, ReleasePolicy.Permits]

/-- Planted: unlisted_program_caller_refused kills each single-constructor omission:
defer, conditional region, loop region, and block. The real extracted path and missing
caller premises hold, while substituting each wrong graph admits that same program. -/
example : (ReleaseCalls.ofProgram
      (fun p : Demo.Prim => match p with | .release => some () | _ => none) program).Reaches (fn "0") (fn "2") ∧
    (fn "2", ⟨⟨7⟩, ()⟩) ∈ chain.releases ∧ fn "0" ∉ outerDenied.functions ∧
    (∀ drop, drop < 4 → outerDenied.Admitted (droppedGraph drop)) ∧
    ¬ outerDenied.ProgramAdmitted
      (fun p : Demo.Prim => match p with | .release => some () | _ => none) program :=
  ⟨twoLevels, by simp [chain], by decide, dropped_admitted, refused⟩

end CallControls
end ConcreteSpec.Core.Leak
