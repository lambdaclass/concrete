import ConcreteSpec.Spec.Basics
import ConcreteSpec.Spec.Exec
import ConcreteSpec.Spec.Ident
import ConcreteSpec.Spec.Leak
import ConcreteSpec.Spec.ReleaseCalls
import ConcreteSpec.Spec.LeakData

/-! Call-chain admission laws and isolated wrong-definition controls.
Authority: docs/decisions.md#foundation-q1 (C7). -/
namespace ConcreteSpec.Core.Leak
open Exec

/-- Law: every source call or release occurrence is retained by its extraction fold.
The premise uses source constructors, so omitting a traversal cannot erase the premise.
Authority: docs/decisions.md#foundation-q1 (C7). -/
theorem extraction_complete {O : Operands} (declared : O.Prim → Option O.Ty)
    {s : Stmt O} {e : Sum FnId (ReleaseSite O.Ty)} (h : SourceOccurrence declared s e) :
    match e with
    | .inl f => f ∈ statementCalls s
    | .inr r => r ∈ statementReleases declared s := by
  induction h with
  | call => simp [statementCalls]
  | defer => simp [statementCalls]
  | release hp => simp [statementReleases, hp]
  | @seqLeft s t e h ih => cases e <;> simp_all [statementCalls, statementReleases]
  | @seqRight s t e h ih => cases e <;> simp_all [statementCalls, statementReleases]
  | @condLeft s t e c site h ih => cases e <;> simp_all [statementCalls, statementReleases]
  | @condRight s t e c site h ih => cases e <;> simp_all [statementCalls, statementReleases]
  | @loopTest s t e l c site h ih => cases e <;> simp_all [statementCalls, statementReleases]
  | @loopBody s t e l c site h ih => cases e <;> simp_all [statementCalls, statementReleases]
  | @block s e b h ih => cases e <;> simpa [statementCalls, statementReleases] using ih

/-- Law: every source call path is reachable in the extracted graph, including its empty path.
Source membership and occurrence premises remain inhabited under an extraction substitution.
Authority: docs/decisions.md#foundation-q1 (C7). -/
theorem source_reaches_complete {O : Operands} (declared : O.Prim → Option O.Ty)
    (p : Program O) {f g : FnId} (h : SourceReaches declared p f g) :
    (ReleaseCalls.ofProgram declared p).Reaches f g := by
  induction h with
  | self f => exact .self f
  | call member occurrence _ ih =>
      apply ReleaseCalls.Reaches.call (middle := _) _ ih
      simp only [ReleaseCalls.ofProgram, List.mem_flatMap, List.mem_map]
      exact ⟨_, member, _, extraction_complete declared occurrence, rfl⟩

/-- Law: a declared release occurrence in any source function appears in the program graph.
Neither source-table membership nor the syntax occurrence depends on extraction.
Authority: docs/decisions.md#foundation-q1 (C7). -/
theorem program_release_complete {O : Operands} (declared : O.Prim → Option O.Ty)
    (p : Program O) {owner : FnId} {body : Fn O} {site : ReleaseSite O.Ty}
    (hm : (owner, body) ∈ p.fns) (hr : SourceOccurrence declared body.body (.inr site)) :
    (owner, site) ∈ (ReleaseCalls.ofProgram declared p).releases := by
  simp only [ReleaseCalls.ofProgram, List.mem_flatMap, List.mem_map]
  exact ⟨_, hm, _, extraction_complete declared hr, rfl⟩

/-- Law: an unlisted caller cannot reach a release in an admitted program, at any call depth.
Authority: docs/decisions.md#foundation-q1 (C7). -/
theorem unlisted_caller_refused {Ty : Type} (p : ReleaseInterface Ty) (g : ReleaseCalls Ty)
    (caller owner : FnId) (site : ReleaseSite Ty) (hcall : g.Reaches caller owner)
    (hrelease : (owner, site) ∈ g.releases) (hnot : caller ∉ p.functions) : ¬ p.Admitted g := by
  intro h
  exact hnot (h caller owner site hcall hrelease).1

/-- Law: reaching an unlisted typed release site refuses admission even when every function is listed.
Authority: docs/decisions.md#foundation-q1 (C7). -/
theorem unlisted_site_refused {Ty : Type} (p : ReleaseInterface Ty) (g : ReleaseCalls Ty)
    (caller owner : FnId) (site : ReleaseSite Ty) (hc : g.Reaches caller owner)
    (hr : (owner, site) ∈ g.releases)
    (hn : ¬ p.sites.Permits (⟨site, ()⟩ : Release Ty Unit)) : ¬ p.Admitted g :=
  fun h => hn (h caller owner site hc hr).2

/-- Law: a forbid-release policy refuses every reachable release, independently of its value.
Authority: docs/decisions.md#foundation-q1 (C7). -/
theorem forbidden_release_refused {Ty : Type} (p : ReleaseInterface Ty) (g : ReleaseCalls Ty)
    (caller owner : FnId) (site : ReleaseSite Ty) (hc : g.Reaches caller owner)
    (hr : (owner, site) ∈ g.releases) (hf : p.sites.ForbidsRelease) : ¬ p.Admitted g := by
  apply unlisted_site_refused p g caller owner site hc hr
  simp [ReleasePolicy.Permits, show p.sites.allowed = [] from hf]

/-- Law: C1 program admission refuses an unlisted caller reaching a declared release through
ordinary/deferred calls and nested regions in the source program.
All premises describe source syntax, independently of extraction or admission.
Authority: docs/decisions.md#foundation-q1 (C7). -/
theorem unlisted_program_caller_refused {O : Operands} (p : ReleaseInterface O.Ty)
    (declared : O.Prim → Option O.Ty) (program : Program O) (caller owner : FnId)
    (site : ReleaseSite O.Ty)
    (hc : SourceReaches declared program caller owner)
    {body : Fn O} (hm : (owner, body) ∈ program.fns)
    (hr : SourceOccurrence declared body.body (.inr site))
    (hn : caller ∉ p.functions) : ¬ p.ProgramAdmitted declared program :=
  unlisted_caller_refused p _ caller owner site
    (source_reaches_complete declared program hc)
    (program_release_complete declared program hm hr) hn

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

/-- Representation lemma: deferred calls are included, even inside a branch and loop region. -/
example : statementCalls (O := Demo.ops)
    (.block ⟨0⟩ (.cond (.lit 1) (.defer (fn "1") [] ⟨3⟩)
      (.loop ⟨0⟩ (.call none (fn "2") [] ⟨4⟩) (.lit 1) .skip ⟨5⟩) ⟨6⟩)) = [fn "1", fn "2"] := rfl
/-- Wrong extraction fold: drops exactly the named traversal case; every other traversal is unchanged. -/
def droppedCalls (drop : Nat) : Stmt Demo.ops → List FnId
  | .call _ f _ _ => if drop = 5 then [] else [f]
  | .defer f _ _ => if drop = 0 then [] else [f]
  | .seq s t => (if drop = 4 then [] else droppedCalls drop s) ++
      (if drop = 6 then [] else droppedCalls drop t)
  | .cond _ s t _ => (if drop = 1 then [] else droppedCalls drop s) ++
      (if drop = 7 then [] else droppedCalls drop t)
  | .loop _ s _ t _ => (if drop = 2 then [] else droppedCalls drop s) ++
      (if drop = 8 then [] else droppedCalls drop t)
  | .block _ s => if drop = 3 then [] else droppedCalls drop s
  | _ => []

/-- Substitute the faulty traversal throughout program admission, retaining release extraction. -/
def droppedGraph (drop : Nat) : ReleaseCalls Unit :=
  { ReleaseCalls.ofProgram (fun p : Demo.Prim => match p with | .release => some () | _ => none) program with
    calls := program.fns.flatMap fun (f, body) => (droppedCalls (if drop = 2 then 8 else drop) body.body).map fun g
        => (f, g) }

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

/-- Instance: comparison of admission after each single-constructor omission:
defer, conditional region, loop region, and block. This admission comparison is not a uniform-substitution law
    control; the source-grounded
extraction controls below qualify the traversals independently. -/
example : (ReleaseCalls.ofProgram
      (fun p : Demo.Prim => match p with | .release => some () | _ => none) program).Reaches (fn "0") (fn "2") ∧
    (fn "2", ⟨⟨7⟩, ()⟩) ∈ chain.releases ∧ fn "0" ∉ outerDenied.functions ∧
    (∀ drop, drop < 4 → outerDenied.Admitted (droppedGraph drop)) ∧
    ¬ outerDenied.ProgramAdmitted
      (fun p : Demo.Prim => match p with | .release => some () | _ => none) program :=
  ⟨twoLevels, by simp [chain], by decide, dropped_admitted, refused⟩


/-- Primitive classification in the integer control machine. -/
def declared : Demo.Prim → Option Unit
  | .release => some ()
  | _ => none

/-- Wrong release fold: omit exactly one traversal case, preserving every other case. -/
def droppedReleases (drop : Nat) : Stmt Demo.ops → List (ReleaseSite Unit)
  | .prim _ p _ site => if drop = 0 then [] else (declared p).toList.map fun ty => ⟨site, ty⟩
  | .seq s t => (if drop = 1 then [] else droppedReleases drop s) ++
      (if drop = 5 then [] else droppedReleases drop t)
  | .cond _ s t _ => (if drop = 2 then [] else droppedReleases drop s) ++
      (if drop = 6 then [] else droppedReleases drop t)
  | .loop _ s _ t _ => (if drop = 3 then [] else droppedReleases drop s) ++
      (if drop = 7 then [] else droppedReleases drop t)
  | .block _ s => if drop = 4 then [] else droppedReleases drop s
  | _ => []

/-- Instance: extraction_complete retains the call occurrence in leaf syntax. -/
example : (fn "1") ∈ statementCalls (O := Demo.ops) ((.call none (fn "1") [] ⟨7⟩)) :=
  extraction_complete (O := Demo.ops) declared (show SourceOccurrence (O := Demo.ops) declared ((.call none (fn "1")
      [] ⟨7⟩)) (.inl (fn "1")) from .call)

/-- Planted: extraction_complete rejects omitting only call/leaf traversal.
The same source occurrence inhabits the uniformly substituted law's premise. -/
example : SourceOccurrence (O := Demo.ops) declared ((.call none (fn "1") [] ⟨7⟩)) (.inl (fn "1")) ∧
    (fn "1") ∉ droppedCalls 5 ((.call none (fn "1") [] ⟨7⟩)) := by
  exact ⟨.call, by decide⟩

/-- Instance: extraction_complete retains the call occurrence in seqLeft syntax. -/
example : (fn "1") ∈ statementCalls (O := Demo.ops) (.seq (.call none (fn "1") [] ⟨7⟩) .skip) :=
  extraction_complete (O := Demo.ops) declared (show SourceOccurrence (O := Demo.ops) declared (.seq (.call none (fn
      "1") [] ⟨7⟩) .skip) (.inl (fn "1")) from .seqLeft (.call))

/-- Planted: extraction_complete rejects omitting only call/seqLeft traversal.
The same source occurrence inhabits the uniformly substituted law's premise. -/
example : SourceOccurrence (O := Demo.ops) declared (.seq (.call none (fn "1") [] ⟨7⟩) .skip) (.inl (fn "1")) ∧
    (fn "1") ∉ droppedCalls 4 (.seq (.call none (fn "1") [] ⟨7⟩) .skip) := by
  exact ⟨.seqLeft (.call), by decide⟩

/-- Instance: extraction_complete retains the call occurrence in seqRight syntax. -/
example : (fn "1") ∈ statementCalls (O := Demo.ops) (.seq .skip (.call none (fn "1") [] ⟨7⟩)) :=
  extraction_complete (O := Demo.ops) declared (show SourceOccurrence (O := Demo.ops) declared (.seq .skip (.call
      none (fn "1") [] ⟨7⟩)) (.inl (fn "1")) from .seqRight (.call))

/-- Planted: extraction_complete rejects omitting only call/seqRight traversal.
The same source occurrence inhabits the uniformly substituted law's premise. -/
example : SourceOccurrence (O := Demo.ops) declared (.seq .skip (.call none (fn "1") [] ⟨7⟩)) (.inl (fn "1")) ∧
    (fn "1") ∉ droppedCalls 6 (.seq .skip (.call none (fn "1") [] ⟨7⟩)) := by
  exact ⟨.seqRight (.call), by decide⟩

/-- Instance: extraction_complete retains the call occurrence in condLeft syntax. -/
example : (fn "1") ∈ statementCalls (O := Demo.ops) (.cond (.lit 1) (.call none (fn "1") [] ⟨7⟩) .skip ⟨6⟩) :=
  extraction_complete (O := Demo.ops) declared (show SourceOccurrence (O := Demo.ops) declared (.cond (.lit 1)
      (.call none (fn "1") [] ⟨7⟩) .skip ⟨6⟩) (.inl (fn "1")) from .condLeft (.call))

/-- Planted: extraction_complete rejects omitting only call/condLeft traversal.
The same source occurrence inhabits the uniformly substituted law's premise. -/
example : SourceOccurrence (O := Demo.ops) declared (.cond (.lit 1) (.call none (fn "1") [] ⟨7⟩) .skip ⟨6⟩) (.inl
    (fn "1")) ∧
    (fn "1") ∉ droppedCalls 1 (.cond (.lit 1) (.call none (fn "1") [] ⟨7⟩) .skip ⟨6⟩) := by
  exact ⟨.condLeft (.call), by decide⟩

/-- Instance: extraction_complete retains the call occurrence in condRight syntax. -/
example : (fn "1") ∈ statementCalls (O := Demo.ops) (.cond (.lit 1) .skip (.call none (fn "1") [] ⟨7⟩) ⟨6⟩) :=
  extraction_complete (O := Demo.ops) declared (show SourceOccurrence (O := Demo.ops) declared (.cond (.lit 1) .skip
      (.call none (fn "1") [] ⟨7⟩) ⟨6⟩) (.inl (fn "1")) from .condRight (.call))

/-- Planted: extraction_complete rejects omitting only call/condRight traversal.
The same source occurrence inhabits the uniformly substituted law's premise. -/
example : SourceOccurrence (O := Demo.ops) declared (.cond (.lit 1) .skip (.call none (fn "1") [] ⟨7⟩) ⟨6⟩) (.inl
    (fn "1")) ∧
    (fn "1") ∉ droppedCalls 7 (.cond (.lit 1) .skip (.call none (fn "1") [] ⟨7⟩) ⟨6⟩) := by
  exact ⟨.condRight (.call), by decide⟩

/-- Instance: extraction_complete retains the call occurrence in loopTest syntax. -/
example : (fn "1") ∈ statementCalls (O := Demo.ops) (.loop ⟨0⟩ (.call none (fn "1") [] ⟨7⟩) (.lit 1) .skip ⟨6⟩) :=
  extraction_complete (O := Demo.ops) declared (show SourceOccurrence (O := Demo.ops) declared (.loop ⟨0⟩ (.call
      none (fn "1") [] ⟨7⟩) (.lit 1) .skip ⟨6⟩) (.inl (fn "1")) from .loopTest (.call))

/-- Planted: extraction_complete rejects omitting only call/loopTest traversal.
The same source occurrence inhabits the uniformly substituted law's premise. -/
example : SourceOccurrence (O := Demo.ops) declared (.loop ⟨0⟩ (.call none (fn "1") [] ⟨7⟩) (.lit 1) .skip ⟨6⟩)
    (.inl (fn "1")) ∧
    (fn "1") ∉ droppedCalls 2 (.loop ⟨0⟩ (.call none (fn "1") [] ⟨7⟩) (.lit 1) .skip ⟨6⟩) := by
  exact ⟨.loopTest (.call), by decide⟩

/-- Instance: extraction_complete retains the call occurrence in loopBody syntax. -/
example : (fn "1") ∈ statementCalls (O := Demo.ops) (.loop ⟨0⟩ .skip (.lit 1) (.call none (fn "1") [] ⟨7⟩) ⟨6⟩) :=
  extraction_complete (O := Demo.ops) declared (show SourceOccurrence (O := Demo.ops) declared (.loop ⟨0⟩ .skip
      (.lit 1) (.call none (fn "1") [] ⟨7⟩) ⟨6⟩) (.inl (fn "1")) from .loopBody (.call))

/-- Planted: extraction_complete rejects omitting only call/loopBody traversal.
The same source occurrence inhabits the uniformly substituted law's premise. -/
example : SourceOccurrence (O := Demo.ops) declared (.loop ⟨0⟩ .skip (.lit 1) (.call none (fn "1") [] ⟨7⟩) ⟨6⟩)
    (.inl (fn "1")) ∧
    (fn "1") ∉ droppedCalls 8 (.loop ⟨0⟩ .skip (.lit 1) (.call none (fn "1") [] ⟨7⟩) ⟨6⟩) := by
  exact ⟨.loopBody (.call), by decide⟩

/-- Instance: extraction_complete retains the call occurrence in block syntax. -/
example : (fn "1") ∈ statementCalls (O := Demo.ops) (.block ⟨0⟩ (.call none (fn "1") [] ⟨7⟩)) :=
  extraction_complete (O := Demo.ops) declared (show SourceOccurrence (O := Demo.ops) declared (.block ⟨0⟩ (.call
      none (fn "1") [] ⟨7⟩)) (.inl (fn "1")) from .block (.call))

/-- Planted: extraction_complete rejects omitting only call/block traversal.
The same source occurrence inhabits the uniformly substituted law's premise. -/
example : SourceOccurrence (O := Demo.ops) declared (.block ⟨0⟩ (.call none (fn "1") [] ⟨7⟩)) (.inl (fn "1")) ∧
    (fn "1") ∉ droppedCalls 3 (.block ⟨0⟩ (.call none (fn "1") [] ⟨7⟩)) := by
  exact ⟨.block (.call), by decide⟩

/-- Instance: extraction_complete retains the defer occurrence in leaf syntax. -/
example : (fn "1") ∈ statementCalls (O := Demo.ops) ((.defer (fn "1") [] ⟨7⟩)) :=
  extraction_complete (O := Demo.ops) declared (show SourceOccurrence (O := Demo.ops) declared ((.defer (fn "1") []
      ⟨7⟩)) (.inl (fn "1")) from .defer)

/-- Planted: extraction_complete rejects omitting only defer/leaf traversal.
The same source occurrence inhabits the uniformly substituted law's premise. -/
example : SourceOccurrence (O := Demo.ops) declared ((.defer (fn "1") [] ⟨7⟩)) (.inl (fn "1")) ∧
    (fn "1") ∉ droppedCalls 0 ((.defer (fn "1") [] ⟨7⟩)) := by
  exact ⟨.defer, by decide⟩

/-- Instance: extraction_complete retains the defer occurrence in seqLeft syntax. -/
example : (fn "1") ∈ statementCalls (O := Demo.ops) (.seq (.defer (fn "1") [] ⟨7⟩) .skip) :=
  extraction_complete (O := Demo.ops) declared (show SourceOccurrence (O := Demo.ops) declared (.seq (.defer (fn
      "1") [] ⟨7⟩) .skip) (.inl (fn "1")) from .seqLeft (.defer))

/-- Planted: extraction_complete rejects omitting only defer/seqLeft traversal.
The same source occurrence inhabits the uniformly substituted law's premise. -/
example : SourceOccurrence (O := Demo.ops) declared (.seq (.defer (fn "1") [] ⟨7⟩) .skip) (.inl (fn "1")) ∧
    (fn "1") ∉ droppedCalls 4 (.seq (.defer (fn "1") [] ⟨7⟩) .skip) := by
  exact ⟨.seqLeft (.defer), by decide⟩

/-- Instance: extraction_complete retains the defer occurrence in seqRight syntax. -/
example : (fn "1") ∈ statementCalls (O := Demo.ops) (.seq .skip (.defer (fn "1") [] ⟨7⟩)) :=
  extraction_complete (O := Demo.ops) declared (show SourceOccurrence (O := Demo.ops) declared (.seq .skip (.defer
      (fn "1") [] ⟨7⟩)) (.inl (fn "1")) from .seqRight (.defer))

/-- Planted: extraction_complete rejects omitting only defer/seqRight traversal.
The same source occurrence inhabits the uniformly substituted law's premise. -/
example : SourceOccurrence (O := Demo.ops) declared (.seq .skip (.defer (fn "1") [] ⟨7⟩)) (.inl (fn "1")) ∧
    (fn "1") ∉ droppedCalls 6 (.seq .skip (.defer (fn "1") [] ⟨7⟩)) := by
  exact ⟨.seqRight (.defer), by decide⟩

/-- Instance: extraction_complete retains the defer occurrence in condLeft syntax. -/
example : (fn "1") ∈ statementCalls (O := Demo.ops) (.cond (.lit 1) (.defer (fn "1") [] ⟨7⟩) .skip ⟨6⟩) :=
  extraction_complete (O := Demo.ops) declared (show SourceOccurrence (O := Demo.ops) declared (.cond (.lit 1)
      (.defer (fn "1") [] ⟨7⟩) .skip ⟨6⟩) (.inl (fn "1")) from .condLeft (.defer))

/-- Planted: extraction_complete rejects omitting only defer/condLeft traversal.
The same source occurrence inhabits the uniformly substituted law's premise. -/
example : SourceOccurrence (O := Demo.ops) declared (.cond (.lit 1) (.defer (fn "1") [] ⟨7⟩) .skip ⟨6⟩) (.inl (fn
    "1")) ∧
    (fn "1") ∉ droppedCalls 1 (.cond (.lit 1) (.defer (fn "1") [] ⟨7⟩) .skip ⟨6⟩) := by
  exact ⟨.condLeft (.defer), by decide⟩

/-- Instance: extraction_complete retains the defer occurrence in condRight syntax. -/
example : (fn "1") ∈ statementCalls (O := Demo.ops) (.cond (.lit 1) .skip (.defer (fn "1") [] ⟨7⟩) ⟨6⟩) :=
  extraction_complete (O := Demo.ops) declared (show SourceOccurrence (O := Demo.ops) declared (.cond (.lit 1) .skip
      (.defer (fn "1") [] ⟨7⟩) ⟨6⟩) (.inl (fn "1")) from .condRight (.defer))

/-- Planted: extraction_complete rejects omitting only defer/condRight traversal.
The same source occurrence inhabits the uniformly substituted law's premise. -/
example : SourceOccurrence (O := Demo.ops) declared (.cond (.lit 1) .skip (.defer (fn "1") [] ⟨7⟩) ⟨6⟩) (.inl (fn
    "1")) ∧
    (fn "1") ∉ droppedCalls 7 (.cond (.lit 1) .skip (.defer (fn "1") [] ⟨7⟩) ⟨6⟩) := by
  exact ⟨.condRight (.defer), by decide⟩

/-- Instance: extraction_complete retains the defer occurrence in loopTest syntax. -/
example : (fn "1") ∈ statementCalls (O := Demo.ops) (.loop ⟨0⟩ (.defer (fn "1") [] ⟨7⟩) (.lit 1) .skip ⟨6⟩) :=
  extraction_complete (O := Demo.ops) declared (show SourceOccurrence (O := Demo.ops) declared (.loop ⟨0⟩ (.defer
      (fn "1") [] ⟨7⟩) (.lit 1) .skip ⟨6⟩) (.inl (fn "1")) from .loopTest (.defer))

/-- Planted: extraction_complete rejects omitting only defer/loopTest traversal.
The same source occurrence inhabits the uniformly substituted law's premise. -/
example : SourceOccurrence (O := Demo.ops) declared (.loop ⟨0⟩ (.defer (fn "1") [] ⟨7⟩) (.lit 1) .skip ⟨6⟩) (.inl
    (fn "1")) ∧
    (fn "1") ∉ droppedCalls 2 (.loop ⟨0⟩ (.defer (fn "1") [] ⟨7⟩) (.lit 1) .skip ⟨6⟩) := by
  exact ⟨.loopTest (.defer), by decide⟩

/-- Instance: extraction_complete retains the defer occurrence in loopBody syntax. -/
example : (fn "1") ∈ statementCalls (O := Demo.ops) (.loop ⟨0⟩ .skip (.lit 1) (.defer (fn "1") [] ⟨7⟩) ⟨6⟩) :=
  extraction_complete (O := Demo.ops) declared (show SourceOccurrence (O := Demo.ops) declared (.loop ⟨0⟩ .skip
      (.lit 1) (.defer (fn "1") [] ⟨7⟩) ⟨6⟩) (.inl (fn "1")) from .loopBody (.defer))

/-- Planted: extraction_complete rejects omitting only defer/loopBody traversal.
The same source occurrence inhabits the uniformly substituted law's premise. -/
example : SourceOccurrence (O := Demo.ops) declared (.loop ⟨0⟩ .skip (.lit 1) (.defer (fn "1") [] ⟨7⟩) ⟨6⟩) (.inl
    (fn "1")) ∧
    (fn "1") ∉ droppedCalls 8 (.loop ⟨0⟩ .skip (.lit 1) (.defer (fn "1") [] ⟨7⟩) ⟨6⟩) := by
  exact ⟨.loopBody (.defer), by decide⟩

/-- Instance: extraction_complete retains the defer occurrence in block syntax. -/
example : (fn "1") ∈ statementCalls (O := Demo.ops) (.block ⟨0⟩ (.defer (fn "1") [] ⟨7⟩)) :=
  extraction_complete (O := Demo.ops) declared (show SourceOccurrence (O := Demo.ops) declared (.block ⟨0⟩ (.defer
      (fn "1") [] ⟨7⟩)) (.inl (fn "1")) from .block (.defer))

/-- Planted: extraction_complete rejects omitting only defer/block traversal.
The same source occurrence inhabits the uniformly substituted law's premise. -/
example : SourceOccurrence (O := Demo.ops) declared (.block ⟨0⟩ (.defer (fn "1") [] ⟨7⟩)) (.inl (fn "1")) ∧
    (fn "1") ∉ droppedCalls 3 (.block ⟨0⟩ (.defer (fn "1") [] ⟨7⟩)) := by
  exact ⟨.block (.defer), by decide⟩

/-- Instance: extraction_complete retains the release occurrence in leaf syntax. -/
example : (⟨⟨7⟩, ()⟩ : ReleaseSite Unit) ∈ statementReleases (O := Demo.ops) declared ((.prim none .release [] ⟨7⟩)) :=
  extraction_complete (O := Demo.ops) declared (show SourceOccurrence (O := Demo.ops) declared ((.prim none .release
      [] ⟨7⟩)) (.inr ⟨⟨7⟩, ()⟩) from .release rfl)

/-- Planted: extraction_complete rejects omitting only release/leaf traversal.
The same source occurrence inhabits the uniformly substituted law's premise. -/
example : SourceOccurrence (O := Demo.ops) declared ((.prim none .release [] ⟨7⟩)) (.inr ⟨⟨7⟩, ()⟩) ∧
    (⟨⟨7⟩, ()⟩ : ReleaseSite Unit) ∉ droppedReleases 0 ((.prim none .release [] ⟨7⟩)) := by
  exact ⟨.release rfl, by decide⟩

/-- Instance: extraction_complete retains the release occurrence in seqLeft syntax. -/
example : (⟨⟨7⟩, ()⟩ : ReleaseSite Unit) ∈ statementReleases (O := Demo.ops) declared (.seq (.prim none .release []
    ⟨7⟩) .skip) :=
  extraction_complete (O := Demo.ops) declared (show SourceOccurrence (O := Demo.ops) declared (.seq (.prim none
      .release [] ⟨7⟩) .skip) (.inr ⟨⟨7⟩, ()⟩) from .seqLeft (.release rfl))

/-- Planted: extraction_complete rejects omitting only release/seqLeft traversal.
The same source occurrence inhabits the uniformly substituted law's premise. -/
example : SourceOccurrence (O := Demo.ops) declared (.seq (.prim none .release [] ⟨7⟩) .skip) (.inr ⟨⟨7⟩, ()⟩) ∧
    (⟨⟨7⟩, ()⟩ : ReleaseSite Unit) ∉ droppedReleases 1 (.seq (.prim none .release [] ⟨7⟩) .skip) := by
  exact ⟨.seqLeft (.release rfl), by decide⟩

/-- Instance: extraction_complete retains the release occurrence in seqRight syntax. -/
example : (⟨⟨7⟩, ()⟩ : ReleaseSite Unit) ∈ statementReleases (O := Demo.ops) declared (.seq .skip (.prim none
    .release [] ⟨7⟩)) :=
  extraction_complete (O := Demo.ops) declared (show SourceOccurrence (O := Demo.ops) declared (.seq .skip (.prim
      none .release [] ⟨7⟩)) (.inr ⟨⟨7⟩, ()⟩) from .seqRight (.release rfl))

/-- Planted: extraction_complete rejects omitting only release/seqRight traversal.
The same source occurrence inhabits the uniformly substituted law's premise. -/
example : SourceOccurrence (O := Demo.ops) declared (.seq .skip (.prim none .release [] ⟨7⟩)) (.inr ⟨⟨7⟩, ()⟩) ∧
    (⟨⟨7⟩, ()⟩ : ReleaseSite Unit) ∉ droppedReleases 5 (.seq .skip (.prim none .release [] ⟨7⟩)) := by
  exact ⟨.seqRight (.release rfl), by decide⟩

/-- Instance: extraction_complete retains the release occurrence in condLeft syntax. -/
example : (⟨⟨7⟩, ()⟩ : ReleaseSite Unit) ∈ statementReleases (O := Demo.ops) declared (.cond (.lit 1) (.prim none
    .release [] ⟨7⟩) .skip ⟨6⟩) :=
  extraction_complete (O := Demo.ops) declared (show SourceOccurrence (O := Demo.ops) declared (.cond (.lit 1)
      (.prim none .release [] ⟨7⟩) .skip ⟨6⟩) (.inr ⟨⟨7⟩, ()⟩) from .condLeft (.release rfl))

/-- Planted: extraction_complete rejects omitting only release/condLeft traversal.
The same source occurrence inhabits the uniformly substituted law's premise. -/
example : SourceOccurrence (O := Demo.ops) declared (.cond (.lit 1) (.prim none .release [] ⟨7⟩) .skip ⟨6⟩) (.inr
    ⟨⟨7⟩, ()⟩) ∧
    (⟨⟨7⟩, ()⟩ : ReleaseSite Unit) ∉ droppedReleases 2 (.cond (.lit 1) (.prim none .release [] ⟨7⟩) .skip ⟨6⟩) := by
  exact ⟨.condLeft (.release rfl), by decide⟩

/-- Instance: extraction_complete retains the release occurrence in condRight syntax. -/
example : (⟨⟨7⟩, ()⟩ : ReleaseSite Unit) ∈ statementReleases (O := Demo.ops) declared (.cond (.lit 1) .skip (.prim
    none .release [] ⟨7⟩) ⟨6⟩) :=
  extraction_complete (O := Demo.ops) declared (show SourceOccurrence (O := Demo.ops) declared (.cond (.lit 1) .skip
      (.prim none .release [] ⟨7⟩) ⟨6⟩) (.inr ⟨⟨7⟩, ()⟩) from .condRight (.release rfl))

/-- Planted: extraction_complete rejects omitting only release/condRight traversal.
The same source occurrence inhabits the uniformly substituted law's premise. -/
example : SourceOccurrence (O := Demo.ops) declared (.cond (.lit 1) .skip (.prim none .release [] ⟨7⟩) ⟨6⟩) (.inr
    ⟨⟨7⟩, ()⟩) ∧
    (⟨⟨7⟩, ()⟩ : ReleaseSite Unit) ∉ droppedReleases 6 (.cond (.lit 1) .skip (.prim none .release [] ⟨7⟩) ⟨6⟩) := by
  exact ⟨.condRight (.release rfl), by decide⟩

/-- Instance: extraction_complete retains the release occurrence in loopTest syntax. -/
example : (⟨⟨7⟩, ()⟩ : ReleaseSite Unit) ∈ statementReleases (O := Demo.ops) declared (.loop ⟨0⟩ (.prim none
    .release [] ⟨7⟩) (.lit 1) .skip ⟨6⟩) :=
  extraction_complete (O := Demo.ops) declared (show SourceOccurrence (O := Demo.ops) declared (.loop ⟨0⟩ (.prim
      none .release [] ⟨7⟩) (.lit 1) .skip ⟨6⟩) (.inr ⟨⟨7⟩, ()⟩) from .loopTest (.release rfl))

/-- Planted: extraction_complete rejects omitting only release/loopTest traversal.
The same source occurrence inhabits the uniformly substituted law's premise. -/
example : SourceOccurrence (O := Demo.ops) declared (.loop ⟨0⟩ (.prim none .release [] ⟨7⟩) (.lit 1) .skip ⟨6⟩)
    (.inr ⟨⟨7⟩, ()⟩) ∧
    (⟨⟨7⟩, ()⟩ : ReleaseSite Unit) ∉ droppedReleases 3 (.loop ⟨0⟩ (.prim none .release [] ⟨7⟩) (.lit 1) .skip ⟨6⟩) := by
  exact ⟨.loopTest (.release rfl), by decide⟩

/-- Instance: extraction_complete retains the release occurrence in loopBody syntax. -/
example : (⟨⟨7⟩, ()⟩ : ReleaseSite Unit) ∈ statementReleases (O := Demo.ops) declared (.loop ⟨0⟩ .skip (.lit 1)
    (.prim none .release [] ⟨7⟩) ⟨6⟩) :=
  extraction_complete (O := Demo.ops) declared (show SourceOccurrence (O := Demo.ops) declared (.loop ⟨0⟩ .skip
      (.lit 1) (.prim none .release [] ⟨7⟩) ⟨6⟩) (.inr ⟨⟨7⟩, ()⟩) from .loopBody (.release rfl))

/-- Planted: extraction_complete rejects omitting only release/loopBody traversal.
The same source occurrence inhabits the uniformly substituted law's premise. -/
example : SourceOccurrence (O := Demo.ops) declared (.loop ⟨0⟩ .skip (.lit 1) (.prim none .release [] ⟨7⟩) ⟨6⟩)
    (.inr ⟨⟨7⟩, ()⟩) ∧
    (⟨⟨7⟩, ()⟩ : ReleaseSite Unit) ∉ droppedReleases 7 (.loop ⟨0⟩ .skip (.lit 1) (.prim none .release [] ⟨7⟩) ⟨6⟩) := by
  exact ⟨.loopBody (.release rfl), by decide⟩

/-- Instance: extraction_complete retains the release occurrence in block syntax. -/
example : (⟨⟨7⟩, ()⟩ : ReleaseSite Unit) ∈ statementReleases (O := Demo.ops) declared (.block ⟨0⟩ (.prim none
    .release [] ⟨7⟩)) :=
  extraction_complete (O := Demo.ops) declared (show SourceOccurrence (O := Demo.ops) declared (.block ⟨0⟩ (.prim
      none .release [] ⟨7⟩)) (.inr ⟨⟨7⟩, ()⟩) from .block (.release rfl))

/-- Planted: extraction_complete rejects omitting only release/block traversal.
The same source occurrence inhabits the uniformly substituted law's premise. -/
example : SourceOccurrence (O := Demo.ops) declared (.block ⟨0⟩ (.prim none .release [] ⟨7⟩)) (.inr ⟨⟨7⟩, ()⟩) ∧
    (⟨⟨7⟩, ()⟩ : ReleaseSite Unit) ∉ droppedReleases 4 (.block ⟨0⟩ (.prim none .release [] ⟨7⟩)) := by
  exact ⟨.block (.release rfl), by decide⟩

/-- Instance: the source empty path includes the release owner, with no graph premise. -/
example : (ReleaseCalls.ofProgram declared program).Reaches (fn "2") (fn "2") :=
  source_reaches_complete (O := Demo.ops) declared program (.self _)

/-- Instance: the deferred source call survives the nested block, branch and loop. -/
example : (ReleaseCalls.ofProgram declared program).Reaches (fn "0") (fn "1") :=
  source_reaches_complete (O := Demo.ops) declared program
    (.call (O := Demo.ops) (body := ⟨[], (), ⟨0⟩, .block ⟨3⟩ (.cond (.lit 1) (.loop ⟨0⟩ .skip (.lit 1) (.defer (fn
        "1") [] ⟨3⟩) ⟨5⟩) .skip ⟨6⟩)⟩) (by simp [program]) (.block (.condLeft (.loopBody .defer))) (.self _))

/-- Wrong reachability: nonempty paths retain every edge but drop the empty path. -/
inductive NonemptyReaches (g : ReleaseCalls Unit) : FnId → FnId → Prop where
  | direct {f t} : (f, t) ∈ g.calls → NonemptyReaches g f t
  | call {f m t} : (f, m) ∈ g.calls → NonemptyReaches g m t → NonemptyReaches g f t

/-- Planted: source_reaches_complete rejects deleting reflexive reachability. The source
empty path is inhabited independently; the unchanged release owner has no outgoing edge. -/
example : SourceReaches declared program (fn "2") (fn "2") ∧
    ¬ NonemptyReaches (ReleaseCalls.ofProgram declared program) (fn "2") (fn "2") := by
  refine ⟨.self _, ?_⟩
  intro h
  cases h with
  | direct edge => simp [program, ReleaseCalls.ofProgram, statementCalls, fn] at edge
  | call edge _ => simp [program, ReleaseCalls.ofProgram, statementCalls, fn] at edge

/-- Planted: source_reaches_complete rejects each omitted nested call traversal. Unlike the
admission comparison, its source path premise is independent of the replacement graph. -/
example : SourceReaches declared program (fn "0") (fn "1") ∧
    ∀ drop, drop < 4 → ¬ (droppedGraph drop).Reaches (fn "0") (fn "1") := by
  refine ⟨.call (O := Demo.ops) (body := ⟨[], (), ⟨0⟩, .block ⟨3⟩ (.cond (.lit 1) (.loop ⟨0⟩ .skip (.lit 1) (.defer
      (fn "1") [] ⟨3⟩) ⟨5⟩) .skip ⟨6⟩)⟩) (by simp [program]) (.block (.condLeft (.loopBody .defer))) (.self _), ?_⟩
  intro drop hd h
  have hg : droppedGraph drop = { chain with calls := [(fn "1", fn "2")] } := by
    have cases4 : drop = 0 ∨ drop = 1 ∨ drop = 2 ∨ drop = 3 := by omega
    rcases cases4 with rfl | rfl | rfl | rfl <;> rfl
  rw [hg] at h
  generalize h0 : fn "0" = f at h
  cases h with
  | self => simp [fn] at h0
  | call edge _ => simp [← h0, fn] at edge

/-- The single releasing source function used to qualify graph release extraction. -/
def releaseBody : Fn Demo.ops := ⟨[], (), ⟨2⟩,
  .block ⟨2⟩ (.prim none .release [.lit 7] ⟨7⟩)⟩

/-- Instance: program_release_complete retains a declared release inside its function block. -/
example : (fn "2", ⟨⟨7⟩, ()⟩) ∈ (ReleaseCalls.ofProgram declared program).releases :=
  program_release_complete declared program (body := releaseBody) (by simp [program, releaseBody])
    (.block (.release rfl))

/-- Wrong program extraction: retain calls and omit exactly the chosen release traversal case. -/
def droppedReleaseTraversalGraph (drop : Nat) : ReleaseCalls Unit :=
  { ReleaseCalls.ofProgram declared program with
    releases := program.fns.flatMap fun (f, b) => (droppedReleases drop b.body).map fun r => (f, r) }

/-- Planted: program_release_complete rejects omitted primitive and block release traversal.
The same source function and declared occurrence inhabit its premises under either substitution. -/
example : (fn "2", releaseBody) ∈ program.fns ∧
    SourceOccurrence declared releaseBody.body (.inr ⟨⟨7⟩, ()⟩) ∧
    (fn "2", ⟨⟨7⟩, ()⟩) ∉ (droppedReleaseTraversalGraph 0).releases ∧
    (fn "2", ⟨⟨7⟩, ()⟩) ∉ (droppedReleaseTraversalGraph 4).releases := by
  exact ⟨by simp [program, releaseBody], .block (.release rfl), by decide, by decide⟩

/-- Helper: the two-level source path follows the actual deferred and ordinary calls. -/
theorem sourceTwoLevels : SourceReaches declared program (fn "0") (fn "2") :=
  .call (O := Demo.ops) (body := ⟨[], (), ⟨0⟩, .block ⟨3⟩ (.cond (.lit 1)
      (.loop ⟨0⟩ .skip (.lit 1) (.defer (fn "1") [] ⟨3⟩) ⟨5⟩) .skip ⟨6⟩)⟩)
    (by simp [program]) (.block (.condLeft (.loopBody .defer)))
    (.call (body := ⟨[], (), ⟨1⟩, .call none (fn "2") [] ⟨4⟩⟩)
      (by simp [program]) .call (.self _))

/-- Instance: extraction preserves a path with two edges, rather than only direct calls. -/
example : (ReleaseCalls.ofProgram declared program).Reaches (fn "0") (fn "2") :=
  source_reaches_complete declared program sourceTwoLevels

/-- Wrong reachability retains reflexivity and every direct edge but omits chaining. -/
def DirectReaches (g : ReleaseCalls Unit) (f t : FnId) : Prop :=
  f = t ∨ (f, t) ∈ g.calls

/-- Planted: source_reaches_complete rejects dropping transitive chaining alone.
The same two-edge source path remains inhabited under substitution. -/
example : SourceReaches declared program (fn "0") (fn "2") ∧
    ¬ DirectReaches (ReleaseCalls.ofProgram declared program) (fn "0") (fn "2") := by
  exact ⟨sourceTwoLevels, by
    simp [DirectReaches, ReleaseCalls.ofProgram, program, statementCalls, fn]⟩

/-- Helper: a populated source release witness, independent of the extracted graph. -/
theorem sourceRelease : (fn "2", releaseBody) ∈ program.fns ∧
    SourceOccurrence declared releaseBody.body (.inr ⟨⟨7⟩, ()⟩) :=
  ⟨by simp [program, releaseBody], .block (.release rfl)⟩

/-- Instance (law `unlisted_program_caller_refused`): the outer caller is unlisted,
while the complete source path and the owner's declared release remain present. -/
example : SourceReaches declared program (fn "0") (fn "2") ∧
    (fn "2", releaseBody) ∈ program.fns ∧
    SourceOccurrence declared releaseBody.body (.inr ⟨⟨7⟩, ()⟩) ∧
    fn "0" ∉ outerDenied.functions ∧ ¬ outerDenied.ProgramAdmitted declared program :=
  ⟨sourceTwoLevels, sourceRelease.1, sourceRelease.2, by decide,
    unlisted_program_caller_refused outerDenied declared program _ _ _
      sourceTwoLevels sourceRelease.1 sourceRelease.2 (by decide)⟩

/-- Planted (law `unlisted_program_caller_refused`, subject `ReleaseCalls.ofProgram`):
uniformly replacing program extraction by a fold omitting one call traversal makes the
conclusion false. No occurrence, table-membership or path premise mentions extraction. -/
example : SourceReaches declared program (fn "0") (fn "2") ∧
    (fn "2", releaseBody) ∈ program.fns ∧
    SourceOccurrence declared releaseBody.body (.inr ⟨⟨7⟩, ()⟩) ∧
    fn "0" ∉ outerDenied.functions ∧
    ∀ drop, drop < 4 → ¬ (¬ outerDenied.Admitted (droppedGraph drop)) :=
  ⟨sourceTwoLevels, sourceRelease.1, sourceRelease.2, by decide,
    fun drop hd h => h (dropped_admitted drop hd)⟩

/-- Planted definition: program admission checks only direct release owners. Extraction,
typed-site permission and the source program remain unchanged. -/
def directOnlyProgram (p : ReleaseInterface Unit) (d : Demo.Prim → Option Unit)
    (prog : Program Demo.ops) : Prop := directOnly p (ReleaseCalls.ofProgram d prog)

/-- Planted definition: program admission drops only the function allowlist conjunct. -/
def ignoresFunctionsProgram (p : ReleaseInterface Unit) (d : Demo.Prim → Option Unit)
    (prog : Program Demo.ops) : Prop := ignoresFunctions p (ReleaseCalls.ofProgram d prog)

/-- Planted (law `unlisted_program_caller_refused`, subject `ProgramAdmitted`,
mutant `directOnlyProgram`): all source premises hold but the substituted refusal is false. -/
example : SourceReaches declared program (fn "0") (fn "2") ∧
    (fn "2", releaseBody) ∈ program.fns ∧
    SourceOccurrence declared releaseBody.body (.inr ⟨⟨7⟩, ()⟩) ∧
    fn "0" ∉ outerDenied.functions ∧
    ¬ (¬ directOnlyProgram outerDenied declared program) :=
  ⟨sourceTwoLevels, sourceRelease.1, sourceRelease.2, by decide,
    fun h => h kills_directOnly.2.2.2.1⟩

/-- Planted (law `unlisted_program_caller_refused`, subject `ProgramAdmitted`,
mutant `ignoresFunctionsProgram`): the same source witnesses survive uniform substitution. -/
example : SourceReaches declared program (fn "0") (fn "2") ∧
    (fn "2", releaseBody) ∈ program.fns ∧
    SourceOccurrence declared releaseBody.body (.inr ⟨⟨7⟩, ()⟩) ∧
    fn "0" ∉ outerDenied.functions ∧
    ¬ (¬ ignoresFunctionsProgram outerDenied declared program) :=
  ⟨sourceTwoLevels, sourceRelease.1, sourceRelease.2, by decide,
    fun h => h kills_ignoresFunctions.2.2.2.1⟩

/-- Planted definition: omit one function-table entry from release extraction only.
Call extraction and statement traversal remain the production definitions. -/
def droppedReleaseGraph {O : Operands} (d : O.Prim → Option O.Ty)
    (prog : Program O) (omitted : FnId) : ReleaseCalls O.Ty :=
  { ReleaseCalls.ofProgram d prog with
    releases := (prog.fns.filter fun entry => entry.1 != omitted).flatMap fun (f, b) =>
      (statementReleases d b.body).map fun r => (f, r) }

/-- Planted (law `program_release_complete`, subject `ReleaseCalls.ofProgram`):
the releasing function still belongs to the SOURCE table and has a release occurrence;
dropping its table entry only in extraction falsifies the substituted conclusion. -/
example : (fn "2", releaseBody) ∈ program.fns ∧
    SourceOccurrence declared releaseBody.body (.inr ⟨⟨7⟩, ()⟩) ∧
    (droppedReleaseGraph declared program (fn "2")).calls =
      (ReleaseCalls.ofProgram declared program).calls ∧
    (fn "2", ⟨⟨7⟩, ()⟩) ∉ (droppedReleaseGraph declared program (fn "2")).releases :=
  ⟨sourceRelease.1, sourceRelease.2, rfl, by decide⟩

/-- Planted (law `unlisted_program_caller_refused`, subject `ReleaseCalls.ofProgram`):
dropping the owner's function-table entry from the release projection makes admission vacuous,
while the complete source path, source-table membership and source occurrence remain inhabited. -/
example : SourceReaches declared program (fn "0") (fn "2") ∧
    (fn "2", releaseBody) ∈ program.fns ∧
    SourceOccurrence declared releaseBody.body (.inr ⟨⟨7⟩, ()⟩) ∧
    fn "0" ∉ outerDenied.functions ∧
    ¬ (¬ outerDenied.Admitted (droppedReleaseGraph declared program (fn "2"))) := by
  refine ⟨sourceTwoLevels, sourceRelease.1, sourceRelease.2, by decide, ?_⟩
  intro h
  apply h
  intro caller owner site _ hr
  simp [droppedReleaseGraph, program, statementReleases, fn] at hr

end CallControls
end ConcreteSpec.Core.Leak
