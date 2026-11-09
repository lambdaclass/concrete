import ConcreteSpec.Spec.Grant
import ConcreteSpec.Proofs.Basics

/-! # C5 brand lifetime and grant attenuation laws

Preservation establishes scoped dependencies and live allocation owners. `fresh_nominal`,
`retired_never_revives` and `scope_exit_lifetime` establish generativity and non-escape across
executions; `run_erasure` establishes concrete allocator execution agreement. Grant laws state
extensional permissions (`narrow_permissions`), attenuation and irreversible consumption over
executions. Guard enumerations and copied constructor records remain internal computations,
with pending independent review, rather than being presented as separate soundness laws.
-/
namespace ConcreteSpec.Core.Generative

/-- Partial-result wrapper so generative laws state properties of the actual operation output
rather than taking a fixed output state in a premise. -/
def ResultSatisfies {α : Type} (P : α → Prop) : Option α → Prop
  | none => True
  | some x => P x

instance {α : Type} (P : α → Prop) (x : Option α) [∀ a, Decidable (P a)] :
    Decidable (ResultSatisfies P x) :=
  match x with
  | none => isTrue trivial
  | some a => inferInstanceAs (Decidable (P a))

/-- Law (subject `ConcreteSpec.Core.Generative.fresh`). Allocating a fresh generative binder
preserves `WellFormed`. -/
theorem fresh_preserves (s : State) (scope : BlockId) (hs : WellFormed s) :
    ResultSatisfies WellFormed (fresh s scope) := by
  change WellFormed { s with
    binders := ⟨⟨s.nextBrand⟩, ⟨s.nextLineage⟩, scope, true⟩ :: s.binders
    nextBrand := s.nextBrand + 1
    nextLineage := s.nextLineage + 1 }
  rcases hs with ⟨⟨hn, hd⟩, hb, hdep⟩
  refine ⟨⟨?_, hd⟩, ?_, ?_⟩
  · simp only [List.map_cons, List.nodup_cons]
    refine ⟨?_, hn⟩
    intro hi
    obtain ⟨b, hm, he⟩ := List.mem_map.mp hi
    have hlt := (hb b hm).1
    rw [he] at hlt
    exact Nat.lt_irrefl _ hlt
  · intro b hm
    rcases List.mem_cons.mp hm with rfl | hm
    · exact ⟨Nat.lt_succ_self _, Nat.lt_succ_self _⟩
    · exact ⟨Nat.lt_succ_of_lt (hb b hm).1, Nat.lt_succ_of_lt (hb b hm).2⟩
  · intro d hm
    obtain ⟨hbr, hal⟩ := hdep d hm
    constructor
    · intro i hi
      obtain ⟨b, hb, he⟩ := hbr i hi
      exact ⟨b, List.mem_cons_of_mem _ hb, he⟩
    · intro i hi
      obtain ⟨hbr, b, hb, he⟩ := hal i hi
      exact ⟨hbr, b, List.mem_cons_of_mem _ hb, he⟩

/-- Helper (computation). Computes the total constructor; fresh_nominal establishes that the new identity differs from every old scoped identity. (approved: APPR-C5-codex-902ce88a). -/
theorem fresh_progress (s : State) (scope : BlockId) :
    ∃ t, fresh s scope = some t := ⟨_, rfl⟩

/-- Law (subject `ConcreteSpec.Core.Generative.depend`). Registering a dependency preserves `WellFormed`. -/
theorem depend_preserves (s : State) (d : Dependency) (hs : WellFormed s) :
    ResultSatisfies WellFormed (depend s d) := by
  cases h : depend s d with
  | none => trivial
  | some t =>
    unfold depend at h
    split at h
    · rename_i hg
      cases h
      rcases hs with ⟨⟨hb, hd⟩, hbound, hdep⟩
      refine ⟨⟨hb, ?_⟩, hbound, ?_⟩
      · simp only [List.map_cons, List.nodup_cons]
        refine ⟨?_, hd⟩
        intro hm
        obtain ⟨a, ha, he⟩ := List.mem_map.mp hm
        have hn := List.all_eq_true.mp hg.1 a ha
        simp [he] at hn
      · intro a ha
        rcases List.mem_cons.mp ha with rfl | ha
        · exact hg.2
        · exact hdep a ha
    · contradiction

/-- Law (subject `ConcreteSpec.Core.Generative.consume`). Consuming a dependent place preserves `WellFormed`. -/
theorem consume_preserves (s : State) (p : Hybrid.Place) (hs : WellFormed s) :
    ResultSatisfies WellFormed (consume s p) := by
  cases h : consume s p with
  | none => trivial
  | some t =>
    unfold consume at h
    split at h
    · cases h
      refine ⟨⟨hs.1.1, ?_⟩, hs.2.1, ?_⟩
      · exact hs.1.2.sublist (List.Sublist.map _ (List.filter_sublist))
      · intro d hd
        exact hs.2.2 d (List.mem_filter.mp hd).1
    · contradiction

/-- Law (subject `ConcreteSpec.Core.Generative.release`). Releasing an allocator preserves `WellFormed`. -/
theorem release_preserves (s : State) (i : BrandId) (hs : WellFormed s) :
    ResultSatisfies WellFormed (release s i) := by
  cases h : release s i with
  | none => trivial
  | some t =>
    unfold release at h
    split at h
    · rename_i hg
      cases h
      let change := fun b : Binder => if b.brand = i then { b with allocatorLive := false } else b
      have brand (b : Binder) : (change b).brand = b.brand := by simp [change]; split <;> rfl
      have lineage (b : Binder) : (change b).lineage = b.lineage := by simp [change]; split <;> rfl
      refine ⟨⟨?_, hs.1.2⟩, ?_, ?_⟩
      · change (s.binders.map change |>.map Binder.brand).Nodup
        simpa only [List.map_map, Function.comp_def, brand] using hs.1.1
      · intro b hb
        obtain ⟨a, ha, rfl⟩ := List.mem_map.mp hb
        change (change a).brand.id < s.nextBrand ∧ (change a).lineage.id < s.nextLineage
        simpa only [brand, lineage] using hs.2.1 a ha
      · intro d hd
        obtain ⟨hbr, hal⟩ := hs.2.2 d hd
        constructor
        · intro j hj
          obtain ⟨b, hb, he⟩ := hbr j hj
          exact ⟨change b, List.mem_map.mpr ⟨b, hb, rfl⟩, (brand b).trans he⟩
        · intro j hj
          obtain ⟨hbr, b, hb, he, hl⟩ := hal j hj
          have hni : j ≠ i := by
            intro heq
            have := List.all_eq_true.mp (Bool.and_eq_true_iff.mp hg).2 d hd
            exact (of_decide_eq_true this) (heq ▸ hj)
          refine ⟨hbr, change b, List.mem_map.mpr ⟨b, hb, rfl⟩, (brand b).trans he, ?_⟩
          have hbi : b.brand ≠ i := fun h => hni (he.symm.trans h)
          simpa [change, hbi] using hl
    · contradiction

/-- Law (subject `ConcreteSpec.Core.Generative.reset`). Resetting an allocator to a fresh nominal
generation preserves `WellFormed`. -/
theorem reset_preserves (s : State) (i : BrandId) (hs : WellFormed s) :
    ResultSatisfies WellFormed (reset s i) := by
  cases h : reset s i with
  | none => trivial
  | some t =>
    unfold reset at h
    obtain ⟨b, hb, hh⟩ := Option.bind_eq_some_iff.mp h
    obtain ⟨u, hu, hh⟩ := Option.bind_eq_some_iff.mp hh
    cases hh
    have hw : WellFormed u := by
      have hr := release_preserves s i hs
      rwa [hu] at hr
    have hmem : b ∈ s.binders := List.mem_of_find?_eq_some hb
    have hlin := (hs.2.1 b hmem).2
    have counters : u.nextBrand = s.nextBrand ∧ u.nextLineage = s.nextLineage := by
      unfold release at hu
      split at hu
      · cases hu; exact ⟨rfl, rfl⟩
      · contradiction
    rcases hw with ⟨⟨hn, hd⟩, hbound, hdep⟩
    refine ⟨⟨?_, hd⟩, ?_, ?_⟩
    · simp only [List.map_cons, List.nodup_cons]
      refine ⟨?_, hn⟩
      intro hm
      obtain ⟨a, ha, he⟩ := List.mem_map.mp hm
      have hlt := (hbound a ha).1
      rw [he, counters.1] at hlt
      exact Nat.lt_irrefl _ hlt
    · intro a ha
      rcases List.mem_cons.mp ha with rfl | ha
      · exact ⟨Nat.lt_succ_self _, by simpa only [counters.2] using hlin⟩
      · exact ⟨Nat.lt_succ_of_lt (by simpa only [counters.1] using (hbound a ha).1), (hbound a ha).2⟩
    · intro d hd
      obtain ⟨hbr, hal⟩ := hdep d hd
      constructor
      · intro j hj
        obtain ⟨a, ha, he⟩ := hbr j hj
        exact ⟨a, List.mem_cons_of_mem _ ha, he⟩
      · intro j hj
        obtain ⟨hbr, a, ha, he⟩ := hal j hj
        exact ⟨hbr, a, List.mem_cons_of_mem _ ha, he⟩

/-- Law (subject `ConcreteSpec.Core.Generative.endScope`). Ending a lexical brand scope preserves
`WellFormed`. -/
theorem endScope_preserves (s : State) (scope : BlockId) (hs : WellFormed s) :
    ResultSatisfies WellFormed (endScope s scope) := by
  cases h : endScope s scope with
  | none => trivial
  | some t =>
    unfold endScope at h
    split at h
    · rename_i hg
      cases h
      refine ⟨⟨hs.1.1.sublist (List.Sublist.map _ (List.filter_sublist)), hs.1.2⟩, ?_, ?_⟩
      · intro b hb
        exact hs.2.1 b (List.mem_filter.mp hb).1
      · intro d hd
        obtain ⟨hbr, hal⟩ := hs.2.2 d hd
        have remains (j : BrandId) (hj : j ∈ d.brands) (b : Binder) (hb : b ∈ s.binders)
            (he : b.brand = j) : (b.scope != scope) = true := by
          have hg' := List.all_eq_true.mp (Bool.and_eq_true_iff.mp hg).2 d hd
          have hb' := List.all_eq_true.mp hg' b hb
          by_cases hscope : b.scope = scope
          · simp [hscope, he, hj] at hb'
          · simp [hscope]
        constructor
        · intro j hj
          obtain ⟨b, hb, he⟩ := hbr j hj
          exact ⟨b, List.mem_filter.mpr ⟨hb, remains j hj b hb he⟩, he⟩
        · intro j hj
          obtain ⟨hj', b, hb, he, hl⟩ := hal j hj
          exact ⟨hj', b, List.mem_filter.mpr ⟨hb, remains j hj' b hb he⟩, he, hl⟩
    · contradiction

/-- Helper (computation). Normalizes dependency admission; depend_preserves establishes lexical and allocation lifetime safety. (approved: APPR-C5-codex-902ce88a). -/
theorem depend_progress (s : State) (d : Dependency)
    (unique : ∀ a ∈ s.dependencies, a.place ≠ d.place)
    (brands : ∀ i ∈ d.brands, ∃ b ∈ s.binders, b.brand = i)
    (allocations : ∀ i ∈ d.allocations, i ∈ d.brands ∧
      ∃ b ∈ s.binders, b.brand = i ∧ b.allocatorLive = true) :
    ∃ t, depend s d = some t := by
  unfold depend
  have hu : (s.dependencies.all (·.place != d.place)) = true := by
    apply List.all_eq_true.mpr
    intro a ha
    simpa using unique a ha
  rw [ite_eq_left ⟨hu, brands, allocations⟩]
  exact ⟨_, rfl⟩

/-- Helper (computation). Normalizes the dependency lookup guard; retired_never_revives establishes that consumption cannot revive ended identities. (approved: APPR-C5-codex-902ce88a). -/
theorem consume_progress (s : State) (p : Hybrid.Place)
    (present : ∃ d ∈ s.dependencies, d.place = p) : ∃ t, consume s p = some t := by
  obtain ⟨d, hd, hp⟩ := present
  have hg : (s.dependencies.any (·.place == p)) = true :=
    List.any_eq_true.mpr ⟨d, hd, by simp [hp]⟩
  simp [consume, hg]

/-- Helper (computation). Normalizes the live-allocation exclusion guard; release_preserves and allocations_have_live_allocator establish allocator safety. (approved: APPR-C5-codex-902ce88a). -/
theorem release_progress (s : State) (i : BrandId)
    (present : ∃ b ∈ s.binders, b.brand = i ∧ b.allocatorLive = true)
    (clear : ∀ d ∈ s.dependencies, i ∉ d.allocations) :
    ∃ t, release s i = some t := by
  obtain ⟨b, hb, hi, hl⟩ := present
  have hp : (s.binders.any (fun b => b.brand == i && b.allocatorLive)) = true :=
    List.any_eq_true.mpr ⟨b, hb, by simp [hi, hl]⟩
  have hc : (s.dependencies.all (fun d => decide (i ∉ d.allocations))) = true := by
    apply List.all_eq_true.mpr
    intro d hd
    simp [clear d hd]
  unfold release
  rw [ite_eq_left (by rw [hp, hc]; rfl)]
  exact ⟨_, rfl⟩

/-- Helper (computation). Composes lookup and release computations; reset_preserves and retired_never_revives establish generativity and lifetime safety. (approved: APPR-C5-codex-902ce88a). -/
theorem reset_progress (s : State) (i : BrandId) (b : Binder)
    (found : s.binders.find? (·.brand == i) = some b)
    (live : b.allocatorLive = true)
    (clear : ∀ d ∈ s.dependencies, i ∉ d.allocations) :
    ∃ t, reset s i = some t := by
  have hb : b ∈ s.binders := List.mem_of_find?_eq_some found
  have hi : b.brand = i := by
    have := List.find?_some found
    simpa using this
  obtain ⟨u, hu⟩ := release_progress s i ⟨b, hb, hi, live⟩ clear
  simp [reset, found, hu]

/-- Helper (computation). Normalizes lexical exit guards; scope_exit_lifetime establishes non-escape and non-revival across later executions. (approved: APPR-C5-codex-902ce88a). -/
theorem endScope_progress (s : State) (scope : BlockId)
    (released : ∀ b ∈ s.binders, b.scope = scope → b.allocatorLive = false)
    (clear : ∀ d ∈ s.dependencies, ∀ b ∈ s.binders, b.scope = scope → b.brand ∉ d.brands) :
    ∃ t, endScope s scope = some t := by
  have hr : (s.binders.all (fun b => b.scope != scope || !b.allocatorLive)) = true := by
    apply List.all_eq_true.mpr
    intro b hb
    by_cases he : b.scope = scope
    · simp [he, released b hb he]
    · simp [he]
  have hc : (s.dependencies.all (fun d => s.binders.all (fun b =>
      b.scope != scope || decide (b.brand ∉ d.brands)))) = true := by
    apply List.all_eq_true.mpr
    intro d hd
    apply List.all_eq_true.mpr
    intro b hb
    by_cases he : b.scope = scope
    · simp [he, clear d hd b hb he]
    · simp [he]
  unfold endScope
  rw [ite_eq_left (by rw [hr, hc]; rfl)]
  exact ⟨_, rfl⟩

/-- Helper (arithmetic). A new or reset generation is unequal to every previously scoped brand,
because every active binder's brand ID is strictly below `s.nextBrand`. (approved: APPR-C5-codex-902ce88a). -/
theorem nextBrand_fresh (s : State) (hs : WellFormed s) (b : Binder) (hb : b ∈ s.binders) :
    b.brand ≠ BrandId.mk s.nextBrand := by
  intro he
  have hn := (hs.2.1 b hb).1
  rw [he] at hn
  exact Nat.lt_irrefl _ hn

/-- Helper (inversion). Generation birth for `fresh` on a given output state. (approved: APPR-C5-codex-902ce88a). -/
theorem fresh_generation_result (s t : State) (scope : BlockId) (h : fresh s scope = some t) :
    t.nextBrand = s.nextBrand + 1 ∧ ∃ b ∈ t.binders, b.brand = ⟨s.nextBrand⟩ := by
  cases h
  exact ⟨rfl, _, List.mem_cons_self, rfl⟩

/-- Helper (computation). Calculates constructor fields; fresh_nominal supplies independent nominal freshness evidence. (approved: APPR-C5-codex-902ce88a). -/
theorem fresh_generation (s : State) (scope : BlockId) :
    ResultSatisfies (fun t : State =>
      t.nextBrand = s.nextBrand + 1 ∧ ∃ b ∈ t.binders, b.brand = ⟨s.nextBrand⟩)
      (fresh s scope) := by
  cases h : fresh s scope with
  | none => trivial
  | some t => exact fresh_generation_result s t scope h

/-- Helper (inversion). Generation birth for `reset` on a given output state. (approved: APPR-C5-codex-902ce88a). -/
theorem reset_generation_result (s t : State) (i : BrandId) (h : reset s i = some t) :
    t.nextBrand = s.nextBrand + 1 ∧ ∃ b ∈ t.binders, b.brand = ⟨s.nextBrand⟩ := by
  unfold reset at h
  obtain ⟨b, _, hh⟩ := Option.bind_eq_some_iff.mp h
  obtain ⟨u, _, hh⟩ := Option.bind_eq_some_iff.mp hh
  cases hh
  exact ⟨rfl, _, List.mem_cons_self, rfl⟩

/-- Helper (computation). Calculates the reset birth counter; reset_preserves and retired_never_revives establish generation separation over executions. (approved: APPR-C5-codex-902ce88a). -/
theorem reset_generation (s : State) (i : BrandId) :
    ResultSatisfies (fun t : State =>
      t.nextBrand = s.nextBrand + 1 ∧ ∃ b ∈ t.binders, b.brand = ⟨s.nextBrand⟩)
      (reset s i) := by
  cases h : reset s i with
  | none => trivial
  | some t => exact reset_generation_result s t i h

/-- Helper (inversion). Scope-exit non-escape on a given output state. (approved: APPR-C5-codex-902ce88a). -/
theorem endScope_non_escape_result (s t : State) (scope : BlockId)
    (h : endScope s scope = some t) (b : Binder) (hb : b ∈ s.binders)
    (hscope : b.scope = scope) (d : Dependency) (hd : d ∈ t.dependencies) :
    b.brand ∉ d.brands := by
  unfold endScope at h
  split at h
  · rename_i guard
    cases h
    have hg := Bool.and_eq_true_iff.mp guard |>.2
    have hdg := List.all_eq_true.mp hg d hd
    have hbg := List.all_eq_true.mp hdg b hb
    simpa [hscope] using hbg
  · contradiction

/-- Helper (computation). Extracts the lexical admission check; scope_exit_lifetime establishes scoped dependencies and prevents later resurrection. (approved: APPR-C5-codex-902ce88a). -/
theorem endScope_non_escape (s : State) (scope : BlockId) :
    ResultSatisfies (fun t : State =>
      ∀ b ∈ s.binders, b.scope = scope → ∀ d ∈ t.dependencies, b.brand ∉ d.brands)
      (endScope s scope) := by
  cases h : endScope s scope with
  | none => trivial
  | some t =>
    intro b hb hscope d hd
    exact endScope_non_escape_result s t scope h b hb hscope d hd

/-- Helper (projection). Extracts the live-allocator witness for an allocation dependency from
`WellFormed`. (approved: APPR-C5-codex-902ce88a). -/
theorem allocations_have_live_allocator_of_wf (s : State) (hs : WellFormed s) (d : Dependency)
    (hd : d ∈ s.dependencies) (i : BrandId) (hi : i ∈ d.allocations) :
    ∃ b ∈ s.binders, b.brand = i ∧ b.allocatorLive = true :=
  (hs.2.2 d hd).2 i hi |>.2

/-- Law (subject `ConcreteSpec.Core.Generative.release`). Releasing an allocator never leaves a
remaining allocation dependency naming a consumed allocator (Q1.3, Q1.5). -/
theorem allocations_have_live_allocator (s : State) (i : BrandId) (hs : WellFormed s) :
    ResultSatisfies (fun t : State =>
      ∀ d ∈ t.dependencies, ∀ j ∈ d.allocations,
        ∃ b ∈ t.binders, b.brand = j ∧ b.allocatorLive = true)
      (release s i) := by
  cases h : release s i with
  | none => trivial
  | some t =>
    intro d hd j hj
    have hw : WellFormed t := by
      have hr := release_preserves s i hs
      rwa [h] at hr
    exact allocations_have_live_allocator_of_wf t hw d hd j hj

/-- Helper (inversion). Single-command brand erasure on a given output pair. (approved: APPR-C5-codex-902ce88a). -/
theorem instrument_erasure_result {σ α : Type} (transition : State → Option State)
    (operation : σ → Option (α × σ)) (s : σ × State) (a : α) (concrete : σ) (ghost : State)
    (h : instrument transition operation s = some (a, (concrete, ghost))) :
    operation s.1 = some (a, concrete) := by
  unfold instrument at h
  obtain ⟨_, _, hh⟩ := Option.bind_eq_some_iff.mp h
  obtain ⟨r, hr, hh⟩ := Option.bind_eq_some_iff.mp hh
  cases r
  cases hh
  exact hr

/-- Helper (computation). Calculates one allocator wrapper; run_erasure establishes the independent finite execution contract. (approved: APPR-C5-codex-902ce88a). -/
theorem instrument_erasure {σ α : Type} (transition : State → Option State)
    (operation : σ → Option (α × σ)) (s : σ × State) :
    ResultSatisfies (fun r : α × (σ × State) => operation s.1 = some (r.1, r.2.1))
      (instrument transition operation s) := by
  cases h : instrument transition operation s with
  | none => trivial
  | some r => exact instrument_erasure_result transition operation s r.1 r.2.1 r.2.2 h

/-- Law (subject `ConcreteSpec.Core.Generative.run`). Every successful finite allocator run
erases to the same ordered results and concrete state in `runErased`. -/
theorem run_erasure {σ α : Type} (cs : List (Command σ α)) (s : σ × State) :
    ResultSatisfies (fun r : List α × (σ × State) => runErased cs s.1 = some (r.1, r.2.1))
      (run cs s) := by
  induction cs generalizing s with
  | nil => rfl
  | cons c cs ih =>
    cases h : run (c :: cs) s with
    | none => trivial
    | some r =>
      rcases r with ⟨results, t⟩
      simp only [run] at h
      obtain ⟨⟨a, u⟩, hu, h⟩ := Option.bind_eq_some_iff.mp h
      obtain ⟨⟨as, v⟩, hv, h⟩ := Option.bind_eq_some_iff.mp h
      cases h
      have hc := instrument_erasure_result c.transition c.operation s a u.1 u.2 hu
      have ht : runErased cs u.1 = some (as, v.1) := by
        have ihu := ih u
        rwa [hv] at ihu
      simp [ResultSatisfies, runErased, hc, ht]

namespace PreservationWitnesses
open ConcreteSpec.Core.Generative.Examples

def emptyState : State := {}
def phantomOnly : State := { initial with dependencies := [phantom] }
def dependent : State := { initial with dependencies := [heap, phantom] }
def released : State := (release initial ⟨0⟩).getD {}
def resetOnce : State := (reset initial ⟨0⟩).getD {}
def outer : State := (fresh initial ⟨1⟩).getD {}
def outside : Dependency := ⟨⟨3⟩, [⟨1⟩], []⟩
def departing : State := ((depend outer outside).bind fun s => release s ⟨0⟩).getD {}
def departed : State := (endScope departing ⟨0⟩).getD {}
def releasedWithPhantom : State := { released with dependencies := [phantom] }
/-- Two independent allocators; only the outside allocator has a live allocation. -/
def releaseInput : State := { outer with
  dependencies := [⟨⟨4⟩, [⟨1⟩], [⟨1⟩]⟩] }

/-- Mutant: a successful release also invalidates the unrelated live allocator. -/
def releaseAll (s : State) (i : BrandId) : Option State :=
  (release s i).map fun t => { t with binders := t.binders.map fun b =>
    { b with allocatorLive := false } }

/-- Mutant: successful scope exit lets a dependent value retain the departed brand. -/
def endScopeLeak (s : State) (scope : BlockId) : Option State :=
  (endScope s scope).map fun t => { t with dependencies := s.dependencies ++
    (s.binders.filterMap (fun b => if b.scope = scope then
      some ⟨⟨s.nextBrand + b.brand.id⟩, [b.brand], []⟩ else none)) }

/-- The successful witnesses retain an unrelated dependency. -/
example : (release releaseInput ⟨0⟩).isSome = true ∧
    (endScope departing ⟨0⟩).isSome = true := by decide

abbrev binder0 : Binder := ⟨⟨0⟩, ⟨0⟩, ⟨0⟩, true⟩

/-- Mutant: fresh binder allocation fails to advance `nextBrand`. -/
def freshNoAdvance (s : State) (scope : BlockId) : Option State :=
  some { s with binders := ⟨⟨s.nextBrand⟩, ⟨s.nextLineage⟩, scope, true⟩ :: s.binders }

/-- Mutant: dependency registration omits the unique-place guard and duplicates the dependency. -/
def dependDuplicatePlace (s : State) (d : Dependency) : Option State :=
  some { s with dependencies := d :: d :: s.dependencies }

/-- Mutant: consuming a dependent place drops the binder table while leaving dependencies intact. -/
def consumeDropBinders (s : State) (_p : Hybrid.Place) : Option State :=
  some { s with binders := [] }

/-- Mutant: releasing an allocator ignores live allocation dependencies. -/
def releaseIgnoreLiveDeps (s : State) (i : BrandId) : Option State :=
  some { s with binders := s.binders.map fun b =>
    if b.brand = i then { b with allocatorLive := false } else b }

/-- Mutant: resetting an allocator fails to advance `nextBrand`. -/
def resetNoAdvance (s : State) (i : BrandId) : Option State := do
  let b ← s.binders.find? (·.brand == i)
  let t ← release s i
  some { t with binders := ⟨⟨s.nextBrand⟩, b.lineage, b.scope, true⟩ :: t.binders }

/-- Mutant: ending a scope drops all binders instead of only those of the exiting scope. -/
def endScopeDropAllBinders (s : State) (_scope : BlockId) : Option State :=
  some { s with binders := [] }

/-- Mutant: ending a scope ignores remaining phantom dependencies on the exiting scope's brands. -/
def endScopeIgnoreDeps (s : State) (scope : BlockId) : Option State :=
  some { s with binders := s.binders.filter (·.scope != scope) }

def freshRefuse (_s : State) (_scope : BlockId) : Option State := none
def dependRefuse (_s : State) (_d : Dependency) : Option State := none
def consumeRefuse (_s : State) (_p : Hybrid.Place) : Option State := none
def releaseRefuse (_s : State) (_i : BrandId) : Option State := none
def resetRefuse (_s : State) (_i : BrandId) : Option State := none
def endScopeRefuse (_s : State) (_scope : BlockId) : Option State := none

-- Preservation laws (6)

/-- Instance (law `ConcreteSpec.Core.Generative.fresh_preserves`, case `ConcreteSpec.Core.Generative.PreservationWitnesses.emptyState`). -/
example : ResultSatisfies WellFormed (fresh emptyState ⟨0⟩) :=
  @fresh_preserves emptyState ⟨0⟩ (by decide)

/-- Planted (law `ConcreteSpec.Core.Generative.fresh_preserves`, case `ConcreteSpec.Core.Generative.PreservationWitnesses.emptyState`, subject `ConcreteSpec.Core.Generative.fresh`, mutant `ConcreteSpec.Core.Generative.PreservationWitnesses.freshNoAdvance`). -/
example : WellFormed emptyState ∧
    ¬ ResultSatisfies WellFormed (freshNoAdvance emptyState ⟨0⟩) := by decide

/-- Instance (law `ConcreteSpec.Core.Generative.depend_preserves`, case `ConcreteSpec.Core.Generative.PreservationWitnesses.phantomOnly`). -/
example : ResultSatisfies WellFormed (depend phantomOnly heap) :=
  @depend_preserves phantomOnly heap (by decide)

/-- Planted (law `ConcreteSpec.Core.Generative.depend_preserves`, case `ConcreteSpec.Core.Generative.PreservationWitnesses.phantomOnly`, subject `ConcreteSpec.Core.Generative.depend`, mutant `ConcreteSpec.Core.Generative.PreservationWitnesses.dependDuplicatePlace`). -/
example : WellFormed phantomOnly ∧
    ¬ ResultSatisfies WellFormed (dependDuplicatePlace phantomOnly heap) := by decide

/-- Instance (law `ConcreteSpec.Core.Generative.consume_preserves`, case `ConcreteSpec.Core.Generative.PreservationWitnesses.dependent`). -/
example : ResultSatisfies WellFormed (consume dependent ⟨0⟩) :=
  @consume_preserves dependent ⟨0⟩ (by decide)

/-- Planted (law `ConcreteSpec.Core.Generative.consume_preserves`, case `ConcreteSpec.Core.Generative.PreservationWitnesses.dependent`, subject `ConcreteSpec.Core.Generative.consume`, mutant `ConcreteSpec.Core.Generative.PreservationWitnesses.consumeDropBinders`). -/
example : WellFormed dependent ∧
    ¬ ResultSatisfies WellFormed (consumeDropBinders dependent ⟨0⟩) := by decide

/-- Instance (law `ConcreteSpec.Core.Generative.release_preserves`, case `ConcreteSpec.Core.Generative.PreservationWitnesses.releaseInput`). -/
example : ResultSatisfies WellFormed (release releaseInput ⟨0⟩) :=
  @release_preserves releaseInput ⟨0⟩ (by decide)

/-- Planted (law `ConcreteSpec.Core.Generative.release_preserves`, case `ConcreteSpec.Core.Generative.PreservationWitnesses.releaseInput`, subject `ConcreteSpec.Core.Generative.release`, mutant `ConcreteSpec.Core.Generative.PreservationWitnesses.releaseAll`). -/
example : WellFormed releaseInput ∧
    ¬ ResultSatisfies WellFormed (releaseAll releaseInput ⟨0⟩) := by decide

/-- Instance (law `ConcreteSpec.Core.Generative.reset_preserves`, case `ConcreteSpec.Core.Generative.Examples.initial`). -/
example : ResultSatisfies WellFormed (reset initial ⟨0⟩) :=
  @reset_preserves initial ⟨0⟩ (by decide)

/-- Planted (law `ConcreteSpec.Core.Generative.reset_preserves`, case `ConcreteSpec.Core.Generative.Examples.initial`, subject `ConcreteSpec.Core.Generative.reset`, mutant `ConcreteSpec.Core.Generative.PreservationWitnesses.resetNoAdvance`). -/
example : WellFormed initial ∧
    ¬ ResultSatisfies WellFormed (resetNoAdvance initial ⟨0⟩) := by decide

/-- Instance (law `ConcreteSpec.Core.Generative.endScope_preserves`, case `ConcreteSpec.Core.Generative.PreservationWitnesses.departing`). -/
example : ResultSatisfies WellFormed (endScope departing ⟨0⟩) :=
  @endScope_preserves departing ⟨0⟩ (by decide)

/-- Planted (law `ConcreteSpec.Core.Generative.endScope_preserves`, case `ConcreteSpec.Core.Generative.PreservationWitnesses.departing`, subject `ConcreteSpec.Core.Generative.endScope`, mutant `ConcreteSpec.Core.Generative.PreservationWitnesses.endScopeDropAllBinders`). -/
example : WellFormed departing ∧
    ¬ ResultSatisfies WellFormed (endScopeDropAllBinders departing ⟨0⟩) := by decide

-- Progress laws (6)

/-- Computation observation. -/
example : ∃ t, fresh initial ⟨1⟩ = some t :=
  @fresh_progress initial ⟨1⟩

/-- Computation observation. -/
example : ¬ (∃ t, freshRefuse initial ⟨1⟩ = some t) := by simp [freshRefuse]

/-- Computation observation. -/
example : ∃ t, depend initial heap = some t :=
  @depend_progress initial heap (by decide) (by decide) (by decide)

/-- Computation observation. -/
example : (∀ a ∈ initial.dependencies, a.place ≠ heap.place) ∧
    (∀ i ∈ heap.brands, ∃ b ∈ initial.binders, b.brand = i) ∧
    (∀ i ∈ heap.allocations, i ∈ heap.brands ∧
      ∃ b ∈ initial.binders, b.brand = i ∧ b.allocatorLive = true) ∧
    ¬ (∃ t, dependRefuse initial heap = some t) :=
  ⟨by decide, by decide, by decide, by simp [dependRefuse]⟩

/-- Computation observation. -/
example : ∃ t, consume dependent ⟨0⟩ = some t :=
  @consume_progress dependent ⟨0⟩ (by decide)

/-- Computation observation. -/
example : (∃ d ∈ dependent.dependencies, d.place = ⟨0⟩) ∧
    ¬ (∃ t, consumeRefuse dependent ⟨0⟩ = some t) :=
  ⟨by decide, by simp [consumeRefuse]⟩

/-- Computation observation. -/
example : ∃ t, release initial ⟨0⟩ = some t :=
  @release_progress initial ⟨0⟩ (by decide) (by decide)

/-- Computation observation. -/
example : (∃ b ∈ initial.binders, b.brand = ⟨0⟩ ∧ b.allocatorLive = true) ∧
    (∀ d ∈ initial.dependencies, BrandId.mk 0 ∉ d.allocations) ∧
    ¬ (∃ t, releaseRefuse initial ⟨0⟩ = some t) :=
  ⟨by decide, by decide, by simp [releaseRefuse]⟩

/-- Computation observation. -/
example : ∃ t, reset initial ⟨0⟩ = some t :=
  @reset_progress initial ⟨0⟩ binder0 rfl rfl (by decide)

/-- Computation observation. -/
example : initial.binders.find? (·.brand == ⟨0⟩) = some binder0 ∧
    binder0.allocatorLive = true ∧
    (∀ d ∈ initial.dependencies, BrandId.mk 0 ∉ d.allocations) ∧
    ¬ (∃ t, resetRefuse initial ⟨0⟩ = some t) :=
  ⟨rfl, rfl, by decide, by simp [resetRefuse]⟩

/-- Computation observation. -/
example : ∃ t, endScope released ⟨0⟩ = some t :=
  @endScope_progress released ⟨0⟩ (by decide) (by decide)

/-- Computation observation. -/
example : (∀ b ∈ released.binders, b.scope = ⟨0⟩ → b.allocatorLive = false) ∧
    (∀ d ∈ released.dependencies, ∀ b ∈ released.binders, b.scope = ⟨0⟩ → b.brand ∉ d.brands) ∧
    ¬ (∃ t, endScopeRefuse released ⟨0⟩ = some t) :=
  ⟨by decide, by decide, by simp [endScopeRefuse]⟩

-- Generation, non-escape, and live-allocator laws (4)

/-- Computation observation. -/
example : ResultSatisfies (fun t : State =>
      t.nextBrand = initial.nextBrand + 1 ∧ ∃ b ∈ t.binders, b.brand = ⟨initial.nextBrand⟩)
    (fresh initial ⟨1⟩) :=
  @fresh_generation initial ⟨1⟩

/-- Computation observation. -/
example : ¬ ResultSatisfies (fun t : State =>
      t.nextBrand = initial.nextBrand + 1 ∧ ∃ b ∈ t.binders, b.brand = ⟨initial.nextBrand⟩)
    (freshNoAdvance initial ⟨1⟩) := by decide

/-- Computation observation. -/
example : ResultSatisfies (fun t : State =>
      t.nextBrand = initial.nextBrand + 1 ∧ ∃ b ∈ t.binders, b.brand = ⟨initial.nextBrand⟩)
    (reset initial ⟨0⟩) :=
  @reset_generation initial ⟨0⟩

/-- Computation observation. -/
example : ¬ ResultSatisfies (fun t : State =>
      t.nextBrand = initial.nextBrand + 1 ∧ ∃ b ∈ t.binders, b.brand = ⟨initial.nextBrand⟩)
    (resetNoAdvance initial ⟨0⟩) := by decide

/-- Computation observation. -/
example : ResultSatisfies (fun t : State =>
      ∀ b ∈ departing.binders, b.scope = ⟨0⟩ → ∀ d ∈ t.dependencies, b.brand ∉ d.brands)
    (endScope departing ⟨0⟩) :=
  @endScope_non_escape departing ⟨0⟩

/-- Computation observation. -/
example : ¬ ResultSatisfies (fun t : State =>
      ∀ b ∈ departing.binders, b.scope = ⟨0⟩ → ∀ d ∈ t.dependencies, b.brand ∉ d.brands)
    (endScopeLeak departing ⟨0⟩) := by decide

/-- Instance (law `ConcreteSpec.Core.Generative.allocations_have_live_allocator`, case `ConcreteSpec.Core.Generative.PreservationWitnesses.releaseInput`). -/
example : ResultSatisfies (fun t : State =>
      ∀ d ∈ t.dependencies, ∀ j ∈ d.allocations,
        ∃ b ∈ t.binders, b.brand = j ∧ b.allocatorLive = true)
    (release releaseInput ⟨0⟩) :=
  @allocations_have_live_allocator releaseInput ⟨0⟩ (by decide)

/-- Planted (law `ConcreteSpec.Core.Generative.allocations_have_live_allocator`, case `ConcreteSpec.Core.Generative.PreservationWitnesses.releaseInput`, subject `ConcreteSpec.Core.Generative.release`, mutant `ConcreteSpec.Core.Generative.PreservationWitnesses.releaseAll`). -/
example : WellFormed releaseInput ∧
    ¬ ResultSatisfies (fun t : State =>
      ∀ d ∈ t.dependencies, ∀ j ∈ d.allocations,
        ∃ b ∈ t.binders, b.brand = j ∧ b.allocatorLive = true)
      (releaseAll releaseInput ⟨0⟩) := by decide

end PreservationWitnesses

namespace RunWitnesses
open ConcreteSpec.Core.Generative.Examples

def resetCommand (i : BrandId) : Command Nat Nat :=
  ⟨fun s => reset s i, fun n => some (n, n + 1)⟩
def commands : List (Command Nat Nat) := [resetCommand ⟨0⟩, resetCommand ⟨1⟩]
def finalState : State := ((reset initial ⟨0⟩).bind fun s => reset s ⟨1⟩).getD {}
def runStart : Nat × State := (7, initial)
def incNat (n : Nat) : Option (Nat × Nat) := some (n, n + 1)
def reset0 (s : State) : Option State := reset s ⟨0⟩

/-- Mutant: brand instrumentation keeps the old concrete state instead of the operation's new concrete state. -/
def instrumentKeepOldStore {σ α : Type} (transition : State → Option State)
    (operation : σ → Option (α × σ)) (s : σ × State) : Option (α × (σ × State)) := do
  let ghost ← transition s.2
  let (result, _) ← operation s.1
  pure (result, (s.1, ghost))

/-- Mutant: finite allocator stream execution drops the head command's result from the output list. -/
def runDropResults {σ α : Type} : List (Command σ α) → σ × State → Option (List α × (σ × State))
  | [], s => some ([], s)
  | c :: cs, s => do
    let (_, t) ← instrument c.transition c.operation s
    let (_, u) ← run cs t
    pure ([], u)

/-- Computation observation. -/
example : ResultSatisfies (fun r : Nat × (Nat × State) => incNat runStart.1 = some (r.1, r.2.1))
    (instrument reset0 incNat runStart) :=
  @instrument_erasure Nat Nat reset0 incNat runStart

/-- Computation observation. -/
example : ¬ ResultSatisfies (fun r : Nat × (Nat × State) => incNat runStart.1 = some (r.1, r.2.1))
    (instrumentKeepOldStore reset0 incNat runStart) := by decide

/-- Instance (law `ConcreteSpec.Core.Generative.run_erasure`, case `ConcreteSpec.Core.Generative.RunWitnesses.runStart`). -/
example : ResultSatisfies (fun r : List Nat × (Nat × State) => runErased commands runStart.1 = some (r.1, r.2.1))
    (run commands runStart) :=
  @run_erasure Nat Nat commands runStart

/-- Planted (law `ConcreteSpec.Core.Generative.run_erasure`, case `ConcreteSpec.Core.Generative.RunWitnesses.runStart`, subject `ConcreteSpec.Core.Generative.run`, mutant `ConcreteSpec.Core.Generative.RunWitnesses.runDropResults`). -/
example : ¬ ResultSatisfies (fun r : List Nat × (Nat × State) => runErased commands runStart.1 = some (r.1, r.2.1))
    (runDropResults commands runStart) := by decide

/-- Observation: a refused brand operation is not a refused concrete allocator operation. -/
example : run [resetCommand ⟨0⟩] (7, PreservationWitnesses.dependent) = none ∧
    runErased [resetCommand ⟨0⟩] 7 = some ([7], 8) := by decide
end RunWitnesses

/-- Helper (inversion). Release preserves all nominal binder identities and the generation
supply, even when it consumes the corresponding allocator. (approved: APPR-C5-codex-902ce88a). -/
private theorem release_retired (s t : State) (i j : BrandId) (hr : Retired s j)
    (h : release s i = some t) : Retired t j := by
  unfold release at h
  split at h
  · cases h
    refine ⟨hr.1, ?_⟩
    intro b hb
    obtain ⟨a, ha, rfl⟩ := List.mem_map.mp hb
    split <;> exact hr.2 a ha
  · contradiction

/-- Law (subject `ConcreteSpec.Core.Generative.lifetimeStep`). Each lifetime operation keeps ended
nominal identities unavailable; new generations lie strictly above every retired identity. -/
theorem lifetime_retired_step (s : State) (op : LifetimeOp) (i : BrandId)
    (hr : Retired s i) : ResultSatisfies (fun t => Retired t i) (lifetimeStep s op) := by
  cases h : lifetimeStep s op with
  | none => trivial
  | some t =>
    have bound := hr.1
    cases op with
    | fresh scope =>
      cases h
      refine ⟨Nat.lt_succ_of_lt hr.1, ?_⟩
      intro b hb
      rcases List.mem_cons.mp hb with rfl | hb
      · intro he
        have hid := congrArg BrandId.id he
        dsimp at hid
        omega
      · exact hr.2 b hb
    | depend d =>
      change depend s d = some t at h
      unfold depend at h
      split at h
      · cases h; exact hr
      · contradiction
    | consume p =>
      change consume s p = some t at h
      unfold consume at h
      split at h
      · cases h; exact hr
      · contradiction
    | release j => exact release_retired s t j i hr h
    | reset j =>
      change reset s j = some t at h
      unfold reset at h
      obtain ⟨b, _, h⟩ := Option.bind_eq_some_iff.mp h
      obtain ⟨u, hu, h⟩ := Option.bind_eq_some_iff.mp h
      have hur := release_retired s u j i hr hu
      have supply : u.nextBrand = s.nextBrand := by
        unfold release at hu
        split at hu
        · cases hu; rfl
        · contradiction
      cases h
      refine ⟨by dsimp; omega, ?_⟩
      intro a ha
      rcases List.mem_cons.mp ha with rfl | ha
      · intro he
        have hid := congrArg BrandId.id he
        dsimp at hid
        have := hr.1
        omega
      · exact hur.2 a ha
    | endScope scope =>
      change endScope s scope = some t at h
      unfold endScope at h
      split at h
      · cases h
        exact ⟨hr.1, fun b hb => hr.2 b (List.mem_filter.mp hb).1⟩
      · contradiction

/-- Law (subject `ConcreteSpec.Core.Generative.runLifetime`). Once a lexical brand has ended,
no finite sequence of allocation, reset, release, dependency registration/consumption or scope
exit can bring that nominal identity back. Reentering the same scope uses a new generation. -/
theorem retired_never_revives (s : State) (i : BrandId) (ops : List LifetimeOp)
    (hr : Retired s i) : ResultSatisfies (fun t => Retired t i) (runLifetime s ops) := by
  cases h : runLifetime s ops with
  | none => trivial
  | some t =>
    unfold runLifetime at h
    induction ops generalizing s with
    | nil => simp at h; subst t; exact hr
    | cons op ops ih =>
      simp only [List.foldlM_cons] at h
      obtain ⟨u, hu, h⟩ := Option.bind_eq_some_iff.mp h
      have hur : Retired u i := by
        have hs := lifetime_retired_step s op i hr
        rwa [hu] at hs
      exact ih u hur h

/-- Law (subject `ConcreteSpec.Core.Generative.fresh`). Generativity is nominal: allocation
succeeds and supplies a live binder whose brand differs from every previously scoped brand,
including allocators already released but still held by phantom dependencies. -/
theorem fresh_nominal (s : State) (scope : BlockId) (hs : WellFormed s) :
    ∃ t, fresh s scope = some t ∧ ∃ b ∈ t.binders,
      b.scope = scope ∧ b.allocatorLive = true ∧ ∀ a ∈ s.binders, a.brand ≠ b.brand := by
  refine ⟨_, rfl, ⟨⟨s.nextBrand⟩, ⟨s.nextLineage⟩, scope, true⟩,
    List.mem_cons_self, rfl, rfl, ?_⟩
  intro a ha he
  have hb := (hs.2.1 a ha).1
  have hid := congrArg BrandId.id he
  dsimp at hid
  omega

namespace LifetimeEvidence
open ConcreteSpec.Core.Generative.Examples PreservationWitnesses

def operations : List LifetimeOp := [.fresh ⟨0⟩, .reset ⟨1⟩]

/-- Mutant: a single lifetime step resurrects a previously ended lexical brand. -/
def lifetimeStepRevive (s : State) (op : LifetimeOp) : Option State :=
  (lifetimeStep s op).map fun t => { t with binders := ⟨⟨0⟩, ⟨0⟩, ⟨0⟩, true⟩ :: t.binders }

/-- Mutant: executing a lifetime sequence resurrects a previously ended lexical brand. -/
def revive (s : State) (ops : List LifetimeOp) : Option State :=
  (runLifetime s ops).map fun t => { t with binders := ⟨⟨0⟩, ⟨0⟩, ⟨0⟩, true⟩ :: t.binders }

/-- Mutant: a generative binder reuses the previous generation. -/
def freshReuse (s : State) (scope : BlockId) : Option State :=
  some { s with
    binders := ⟨⟨s.nextBrand - 1⟩, ⟨s.nextLineage⟩, scope, true⟩ :: s.binders
    nextBrand := s.nextBrand + 1
    nextLineage := s.nextLineage + 1 }

/-- Successful scope exit retains the outside dependency; reentry and reset both run. -/
example : (runLifetime departed operations).isSome = true ∧ departed.dependencies = [outside] := by decide

/-- Instance (law `ConcreteSpec.Core.Generative.lifetime_retired_step`, case `ConcreteSpec.Core.Generative.PreservationWitnesses.departed`). -/
example : ResultSatisfies (fun t => Retired t ⟨0⟩) (lifetimeStep departed (.fresh ⟨0⟩)) :=
  @lifetime_retired_step departed (.fresh ⟨0⟩) ⟨0⟩ (by decide)

/-- Planted (law `ConcreteSpec.Core.Generative.lifetime_retired_step`, case `ConcreteSpec.Core.Generative.PreservationWitnesses.departed`, subject `ConcreteSpec.Core.Generative.lifetimeStep`, mutant `ConcreteSpec.Core.Generative.LifetimeEvidence.lifetimeStepRevive`). -/
example : Retired departed ⟨0⟩ ∧ ¬ ResultSatisfies (fun t => Retired t ⟨0⟩)
    (lifetimeStepRevive departed (.fresh ⟨0⟩)) := by decide

/-- Instance (law `ConcreteSpec.Core.Generative.retired_never_revives`, case `ConcreteSpec.Core.Generative.PreservationWitnesses.departed`). -/
example : ResultSatisfies (fun t => Retired t ⟨0⟩) (runLifetime departed operations) :=
  @retired_never_revives departed ⟨0⟩ operations (by decide)

/-- Planted (law `ConcreteSpec.Core.Generative.retired_never_revives`, case `ConcreteSpec.Core.Generative.PreservationWitnesses.departed`, subject `ConcreteSpec.Core.Generative.runLifetime`, mutant `ConcreteSpec.Core.Generative.LifetimeEvidence.revive`). -/
example : Retired departed ⟨0⟩ ∧ ¬ ResultSatisfies (fun t => Retired t ⟨0⟩)
    (revive departed operations) := by decide

/-- Instance (law `ConcreteSpec.Core.Generative.fresh_nominal`, case `ConcreteSpec.Core.Generative.PreservationWitnesses.releasedWithPhantom`). -/
example : ∃ t, fresh releasedWithPhantom ⟨1⟩ = some t ∧ ∃ b ∈ t.binders,
    b.scope = ⟨1⟩ ∧ b.allocatorLive = true ∧
      ∀ a ∈ releasedWithPhantom.binders, a.brand ≠ b.brand :=
  @fresh_nominal releasedWithPhantom ⟨1⟩ (by decide)

/-- Planted (law `ConcreteSpec.Core.Generative.fresh_nominal`, case `ConcreteSpec.Core.Generative.PreservationWitnesses.releasedWithPhantom`, subject `ConcreteSpec.Core.Generative.fresh`, mutant `ConcreteSpec.Core.Generative.LifetimeEvidence.freshReuse`). -/
example : WellFormed releasedWithPhantom ∧
    ¬ (∃ t, freshReuse releasedWithPhantom ⟨1⟩ = some t ∧ ∃ b ∈ t.binders,
      b.scope = ⟨1⟩ ∧ b.allocatorLive = true ∧
        ∀ a ∈ releasedWithPhantom.binders, a.brand ≠ b.brand) := by
  refine ⟨by decide, ?_⟩
  rintro ⟨t, ht, b, hb, hscope, _, hfresh⟩
  cases ht
  rcases List.mem_cons.mp hb with rfl | hb
  · exact hfresh _ List.mem_cons_self rfl
  · have hb' : b = ⟨⟨0⟩, ⟨0⟩, ⟨0⟩, false⟩ := by
        change b ∈ [⟨⟨0⟩, ⟨0⟩, ⟨0⟩, false⟩] at hb
        exact List.mem_singleton.mp hb
    subst b
    cases hscope

end LifetimeEvidence

/-- Helper (inversion). The unique nominal binder table identifies two records with the same
brand; lexical exit cannot confuse an outside binder with an exiting one. (approved: APPR-C5-codex-902ce88a). -/
private theorem binder_unique (xs : List Binder) (hn : (xs.map Binder.brand).Nodup)
    (a b : Binder) (ha : a ∈ xs) (hb : b ∈ xs) (he : a.brand = b.brand) : a = b := by
  induction xs with
  | nil => simp at ha
  | cons c cs ih =>
    simp only [List.map_cons, List.nodup_cons] at hn
    simp only [List.mem_cons] at ha hb
    rcases ha with rfl | ha
    · rcases hb with rfl | hb
      · rfl
      · exact False.elim (hn.1 (List.mem_map.mpr ⟨b, hb, he.symm⟩))
    · rcases hb with rfl | hb
      · exact False.elim (hn.1 (List.mem_map.mpr ⟨a, ha, he⟩))
      · exact ih hn.2 ha hb

/-- Law (subject `ConcreteSpec.Core.Generative.runLifetime`). Every finite lifetime execution
preserves `WellFormed`. -/
theorem lifetime_wf (s : State) (ops : List LifetimeOp) (hs : WellFormed s) :
    ResultSatisfies WellFormed (runLifetime s ops) := by
  cases h : runLifetime s ops with
  | none => trivial
  | some t =>
    unfold runLifetime at h
    induction ops generalizing s with
    | nil => simp at h; subst t; exact hs
    | cons op ops ih =>
      simp only [List.foldlM_cons] at h
      obtain ⟨u, hu, h⟩ := Option.bind_eq_some_iff.mp h
      apply ih u ?_ h
      cases op with
      | fresh b =>
        have hw := fresh_preserves s b hs
        rwa [show fresh s b = some u from hu] at hw
      | depend d =>
        have hw := depend_preserves s d hs
        rwa [show depend s d = some u from hu] at hw
      | consume p =>
        have hw := consume_preserves s p hs
        rwa [show consume s p = some u from hu] at hw
      | release i =>
        have hw := release_preserves s i hs
        rwa [show release s i = some u from hu] at hw
      | reset i =>
        have hw := reset_preserves s i hs
        rwa [show reset s i = some u from hu] at hw
      | endScope b =>
        have hw := endScope_preserves s b hs
        rwa [show endScope s b = some u from hu] at hw

/-- Law (subject `ConcreteSpec.Core.Generative.runLifetime`). Scope exit permanently retires
its nominal brands. Every subsequent finite lifetime execution, including reentry and resets,
keeps them absent from both scoped binders and dependent values. -/
theorem scope_exit_lifetime (s : State) (scope : BlockId) (ops : List LifetimeOp)
    (hs : WellFormed s) :
    ResultSatisfies (fun t => ∀ b ∈ s.binders, b.scope = scope →
      Retired t b.brand ∧ ∀ d ∈ t.dependencies, b.brand ∉ d.brands)
      ((endScope s scope).bind fun u => runLifetime u ops) := by
  cases he : endScope s scope with
  | none => trivial
  | some u =>
    cases hx : runLifetime u ops with
    | none => simp only [Option.bind_some, hx, ResultSatisfies]
    | some t =>
      simp only [Option.bind_some, hx, ResultSatisfies]
      have huw : WellFormed u := by
        have hw := endScope_preserves s scope hs
        rwa [he] at hw
      have htw : WellFormed t := by
        have hw := lifetime_wf u ops huw
        rwa [hx] at hw
      intro b hb hscope
      have hur : Retired u b.brand := by
        unfold endScope at he
        split at he
        · cases he
          refine ⟨(hs.2.1 b hb).1, ?_⟩
          intro a ha heq
          obtain ⟨ham, hn⟩ := List.mem_filter.mp ha
          have hab : a = b := by
            exact binder_unique s.binders hs.1.1 a b ham hb heq
          subst a
          simp [hscope] at hn
        · contradiction
      have htr : Retired t b.brand := by
        have h := retired_never_revives u b.brand ops hur
        simpa only [hx, ResultSatisfies] using h
      refine ⟨htr, ?_⟩
      intro d hd hi
      obtain ⟨a, ha, hab⟩ := (htw.2.2 d hd).1 b.brand hi
      exact htr.2 a ha hab

namespace ScopeLifetimeEvidence
open ConcreteSpec.Core.Generative.Examples PreservationWitnesses LifetimeEvidence

/-- Mutant: executing a lifetime sequence duplicates a binder brand, breaking `WellFormed`. -/
def runLifetimeDuplicateBrand (s : State) (ops : List LifetimeOp) : Option State :=
  (runLifetime s ops).map fun t =>
    { t with binders := ⟨⟨0⟩, ⟨0⟩, ⟨0⟩, true⟩ :: ⟨⟨0⟩, ⟨0⟩, ⟨0⟩, true⟩ :: t.binders }

/-- Instance (law `ConcreteSpec.Core.Generative.lifetime_wf`, case `ConcreteSpec.Core.Generative.PreservationWitnesses.departed`). -/
example : ResultSatisfies WellFormed (runLifetime departed operations) :=
  @lifetime_wf departed operations (by decide)

/-- Planted (law `ConcreteSpec.Core.Generative.lifetime_wf`, case `ConcreteSpec.Core.Generative.PreservationWitnesses.departed`, subject `ConcreteSpec.Core.Generative.runLifetime`, mutant `ConcreteSpec.Core.Generative.ScopeLifetimeEvidence.runLifetimeDuplicateBrand`). -/
example : WellFormed departed ∧
    ¬ ResultSatisfies WellFormed (runLifetimeDuplicateBrand departed operations) := by decide

/-- Instance (law `ConcreteSpec.Core.Generative.scope_exit_lifetime`, case `ConcreteSpec.Core.Generative.PreservationWitnesses.departing`). -/
example : ResultSatisfies (fun t => ∀ b ∈ departing.binders, b.scope = ⟨0⟩ →
    Retired t b.brand ∧ ∀ d ∈ t.dependencies, b.brand ∉ d.brands)
    ((endScope departing ⟨0⟩).bind fun u => runLifetime u operations) :=
  @scope_exit_lifetime departing ⟨0⟩ operations (by decide)

/-- Planted (law `ConcreteSpec.Core.Generative.scope_exit_lifetime`, case `ConcreteSpec.Core.Generative.PreservationWitnesses.departing`, subject `ConcreteSpec.Core.Generative.runLifetime`, mutant `ConcreteSpec.Core.Generative.LifetimeEvidence.revive`). -/
example : WellFormed departing ∧
    ¬ ResultSatisfies (fun t => ∀ b ∈ departing.binders, b.scope = ⟨0⟩ →
      Retired t b.brand ∧ ∀ d ∈ t.dependencies, b.brand ∉ d.brands)
      ((endScope departing ⟨0⟩).bind fun u => revive u operations) := by decide

end ScopeLifetimeEvidence

end ConcreteSpec.Core.Generative

namespace ConcreteSpec.Core.Grants
open ConcreteSpec.Authority (Cap)

variable {H : Type}

/-- Partial-result wrapper so grant laws state properties of the actual operation output
rather than taking a fixed output state in a premise. -/
def ResultSatisfies {α : Type} (P : α → Prop) : Option α → Prop
  | none => True
  | some x => P x

instance {α : Type} (P : α → Prop) (x : Option α) [∀ a, Decidable (P a)] :
    Decidable (ResultSatisfies P x) :=
  match x with
  | none => isTrue trivial
  | some a => inferInstanceAs (Decidable (P a))

/-- Helper (congruence). Updating a single entry in an association list preserves a pointwise
property whenever the update function preserves it at the target key. (approved: APPR-C5-codex-902ce88a). -/
private theorem update_all {α : Type} (P : GrantId × α → Prop) (i : GrantId) (f : α → α)
    (hf : ∀ a, P (i, a) → P (i, f a)) (xs : AList GrantId α)
    (hxs : ∀ e ∈ xs, P e) : ∀ e ∈ AList.update xs i f, P e := by
  induction xs with
  | nil => simp [AList.update]
  | cons e xs ih =>
    rw [AList.update]
    split
    · rename_i he
      intro q hq
      simp only [List.mem_cons] at hq
      rcases hq with rfl | hq
      · subst he; exact hf e.2 (hxs e (List.mem_cons_self))
      · exact hxs q (List.mem_cons_of_mem _ hq)
    · intro q hq
      simp only [List.mem_cons] at hq
      rcases hq with rfl | hq
      · exact hxs e List.mem_cons_self
      · exact ih (fun q hq => hxs q (List.mem_cons_of_mem _ hq)) q hq

/-- Helper (inversion). Successful usability exposes both the original resource and its live flag. (approved: APPR-C5-codex-902ce88a). -/
theorem usable_some (s : State H) (i : GrantId) (r : Resource H) (h : usable s i = some r) :
    AList.lookup s.resources i = some r ∧ r.live = true := by
  unfold usable at h
  obtain ⟨a, ha, hh⟩ := Option.bind_eq_some_iff.mp h
  split at hh
  · cases hh; exact ⟨ha, by assumption⟩
  · contradiction

/-- Helper (congruence). Marking a grant consumed preserves `Bounded`. (approved: APPR-C5-codex-902ce88a). -/
theorem consumed_bounded (budget : List Nat) (s : State H) (i : GrantId)
    (hs : Bounded budget s) : Bounded budget (consumed s i) :=
  update_all (fun (e : GrantId × Resource H) => ∀ right ∈ e.2.rights, right ∈ budget)
    i (fun r => { r with live := false }) (fun _ h => h) s.resources hs

/-- Helper (congruence). Marking a grant consumed preserves `Fresh`. (approved: APPR-C5-codex-902ce88a). -/
theorem consumed_fresh (s : State H) (i : GrantId) (hs : Fresh s) : Fresh (consumed s i) :=
  update_all (fun (e : GrantId × Resource H) => e.1.id < s.next)
    i (fun r => { r with live := false }) (fun _ h => h) s.resources hs

/-- Helper (congruence). Issuing a resource whose rights are within `budget` preserves `Bounded`. (approved: APPR-C5-codex-902ce88a). -/
theorem issue_bounded (budget : List Nat) (s : State H) (r : Resource H)
    (hs : Bounded budget s) (hr : ∀ x ∈ r.rights, x ∈ budget) : Bounded budget (issue s r) := by
  intro e he
  change e ∈ (⟨s.next⟩, r) :: s.resources at he
  rcases List.mem_cons.mp he with rfl | he
  · exact hr
  · exact hs e he

/-- Helper (congruence). Issuing a resource at `⟨s.next⟩` and advancing `next` preserves `Fresh`. (approved: APPR-C5-codex-902ce88a). -/
theorem issue_fresh (s : State H) (r : Resource H) (hs : Fresh s) : Fresh (issue s r) := by
  intro e he
  change e ∈ (⟨s.next⟩, r) :: s.resources at he
  rcases List.mem_cons.mp he with rfl | he
  · exact Nat.lt_succ_self _
  · exact Nat.lt_succ_of_lt (hs e he)

/-- Law (subject `ConcreteSpec.Core.Grants.narrow`). Narrowing a grant preserves `Bounded` (Q7.2). -/
theorem narrow_bounded (budget : List Nat) (s : State H) (i : GrantId) (requested : List Nat)
    (hs : Bounded budget s) :
    ResultSatisfies (Bounded budget) (narrow s i requested) := by
  cases h : narrow s i requested with
  | none => trivial
  | some t =>
    unfold narrow at h
    obtain ⟨r, hr, hh⟩ := Option.bind_eq_some_iff.mp h
    cases hh
    apply issue_bounded _ _ _ (consumed_bounded budget s i hs)
    intro x hx
    exact hs _ (AList.lookup_mem (usable_some s i r hr).1) x (List.mem_filter.mp hx).1

/-- Law (subject `ConcreteSpec.Core.Grants.acquire`). Acquiring a child grant preserves `Bounded`
(Q7.2, Q7.8). -/
theorem acquire_bounded (budget : List Nat) (s : State H) (i : GrantId)
    (requested selected : List Nat) (permissions : List Cap) (allowed : Bool) (handle : H)
    (hs : Bounded budget s) :
    ResultSatisfies (Bounded budget) (acquire s i requested selected permissions allowed handle) := by
  cases h : acquire s i requested selected permissions allowed handle with
  | none => trivial
  | some t =>
    unfold acquire at h
    obtain ⟨r, hr, hh⟩ := Option.bind_eq_some_iff.mp h
    split at hh
    · rename_i guard
      cases hh
      apply issue_bounded _ _ _ hs
      intro x hx
      exact hs _ (AList.lookup_mem (usable_some s i r hr).1) x (guard.2.2 x hx).1
    · contradiction

/-- Law (subject `ConcreteSpec.Core.Grants.close`). Closing a grant preserves `Bounded` (Q7.3). -/
theorem close_bounded (budget : List Nat) (s : State H) (i : GrantId) (result : CloseResult)
    (hs : Bounded budget s) :
    ResultSatisfies (fun r : CloseResult × State H => Bounded budget r.2)
      (close s i result) := by
  cases h : close s i result with
  | none => trivial
  | some r =>
    unfold close at h
    obtain ⟨_, _, hh⟩ := Option.bind_eq_some_iff.mp h
    cases hh
    exact consumed_bounded budget s i hs

/-- Helper (inversion). Executing `runOps` from a state `s` with `Steps s₀ s` produces a state `t`
with `Steps s₀ t`. (approved: APPR-C5-codex-902ce88a). -/
private theorem steps_of_runOps (s₀ : State H) :
    ∀ (ops : List (Op H)) (s t : State H),
      Steps s₀ s → runOps s ops = some t → Steps s₀ t
  | [], s, t, hsteps, hrun => by
    cases hrun
    exact hsteps
  | op :: ops, s, t, hsteps, hrun => by
    simp only [runOps, Option.bind_eq_some_iff] at hrun
    obtain ⟨u, hu, hrest⟩ := hrun
    have hstep_u : Steps s₀ u := by
      cases op with
      | narrow i req => exact .narrow hsteps hu
      | acquire i req sel perms allowed hdl => exact .acquire hsteps hu
      | close i out =>
        simp only [stepOp, Option.map_eq_some_iff] at hu
        obtain ⟨⟨out', u'⟩, hc, rfl⟩ := hu
        have hout : out' = out := by
          unfold close at hc
          obtain ⟨_, _, hh⟩ := Option.bind_eq_some_iff.mp hc
          cases hh
          rfl
        subst hout
        exact .close hsteps hc
    exact steps_of_runOps s₀ ops u t hstep_u hrest

/-- Law (subject `ConcreteSpec.Core.Grants.runOps`). Every finite sequence of narrowing,
acquisition and close stays within the initial launch rights. Retaining parents during acquisition
does not enlarge the authority set (Q7.2, Q7.3). -/
theorem rights_never_grow (budget : List Nat) (s : State H) (ops : List (Op H))
    (hs : Bounded budget s) :
    ResultSatisfies (Bounded budget) (runOps s ops) := by
  cases h : runOps s ops with
  | none => trivial
  | some t =>
    suffices ∀ u, Steps s u → Bounded budget u from
      this t (steps_of_runOps s ops s t (.refl s) h)
    intro u hsteps
    induction hsteps with
    | refl => exact hs
    | @narrow t' u' i req _ hn ih =>
      have hw := narrow_bounded budget t' i req ih
      rwa [hn] at hw
    | @acquire t' u' i req sel perms allowed hdl _ ha ih =>
      have hw := acquire_bounded budget t' i req sel perms allowed hdl ih
      rwa [ha] at hw
    | @close t' u' i res _ hc ih =>
      have hw := close_bounded budget t' i res ih
      rwa [hc] at hw

/-- Law (subject `ConcreteSpec.Core.Grants.narrow`). Narrowing preserves supply freshness `Fresh`. -/
theorem narrow_fresh (s : State H) (i : GrantId) (requested : List Nat) (hs : Fresh s) :
    ResultSatisfies Fresh (narrow s i requested) := by
  cases h : narrow s i requested with
  | none => trivial
  | some t =>
    unfold narrow at h
    obtain ⟨_, _, hh⟩ := Option.bind_eq_some_iff.mp h
    cases hh
    exact issue_fresh _ _ (consumed_fresh s i hs)

/-- Law (subject `ConcreteSpec.Core.Grants.acquire`). Acquisition preserves supply freshness `Fresh`. -/
theorem acquire_fresh (s : State H) (i : GrantId) (requested selected : List Nat)
    (permissions : List Cap) (allowed : Bool) (handle : H) (hs : Fresh s) :
    ResultSatisfies Fresh (acquire s i requested selected permissions allowed handle) := by
  cases h : acquire s i requested selected permissions allowed handle with
  | none => trivial
  | some t =>
    unfold acquire at h
    obtain ⟨_, _, hh⟩ := Option.bind_eq_some_iff.mp h
    split at hh
    · cases hh; exact issue_fresh _ _ hs
    · contradiction

/-- Law (subject `ConcreteSpec.Core.Grants.close`). Closing a grant preserves supply freshness `Fresh`. -/
theorem close_fresh (s : State H) (i : GrantId) (result : CloseResult) (hs : Fresh s) :
    ResultSatisfies (fun r : CloseResult × State H => Fresh r.2) (close s i result) := by
  cases h : close s i result with
  | none => trivial
  | some r =>
    unfold close at h
    obtain ⟨_, _, hh⟩ := Option.bind_eq_some_iff.mp h
    cases hh
    exact consumed_fresh s i hs

/-- Consumption is a persistent historical fact, rather than absence from a map. -/
def Dead (s : State H) (i : GrantId) : Prop :=
  ∃ r, AList.lookup s.resources i = some r ∧ r.live = false

instance [DecidableEq H] (s : State H) (i : GrantId) : Decidable (Dead s i) :=
  match h : AList.lookup s.resources i with
  | none => isFalse (by rintro ⟨r, hr, _⟩; rw [h] at hr; cases hr)
  | some r =>
    if hl : r.live = false then
      isTrue ⟨r, h, hl⟩
    else
      isFalse (by rintro ⟨r', hr', hl'⟩; rw [h] at hr'; injection hr' with hr'; subst hr'; exact hl hl')

/-- Helper (computation). Unpacks the usability guard; consumed_never_revives establishes unavailability after every subsequent grant operation. (approved: APPR-C5-codex-902ce88a). -/
theorem dead_unusable (s : State H) (i : GrantId) (h : Dead s i) : usable s i = none := by
  obtain ⟨r, hr, hd⟩ := h
  simp [usable, hr, hd]

/-- Helper (congruence). Marking an existing grant consumed makes it `Dead`. (approved: APPR-C5-codex-902ce88a). -/
theorem consumed_dead (s : State H) (i : GrantId) (r : Resource H)
    (hr : AList.lookup s.resources i = some r) : Dead (consumed s i) i := by
  refine ⟨{ r with live := false }, ?_, rfl⟩
  simp [consumed, AList.lookup_update, hr]

/-- Helper (congruence). Consuming any grant preserves `Dead` on already-consumed grants. (approved: APPR-C5-codex-902ce88a). -/
theorem consumed_keeps_dead (s : State H) (i j : GrantId) (h : Dead s i) :
    Dead (consumed s j) i := by
  obtain ⟨r, hr, hd⟩ := h
  by_cases he : i = j
  · subst j; exact consumed_dead s i r hr
  · exact ⟨r, by simp [consumed, AList.lookup_update, he, hr], hd⟩

/-- Law (subject `ConcreteSpec.Core.Grants.issue`). Issuing a fresh grant preserves `Dead` on
already-consumed grants. -/
theorem issue_keeps_dead (s : State H) (r : Resource H) (i : GrantId)
    (hs : Fresh s) (h : Dead s i) : Dead (issue s r) i := by
  obtain ⟨a, ha, hd⟩ := h
  have hb := hs _ (AList.lookup_mem ha)
  have hi : GrantId.mk s.next ≠ i := by
    intro he; rw [← he] at hb; exact Nat.lt_irrefl _ hb
  exact ⟨a, by simp [issue, AList.lookup, hi, ha], hd⟩

/-- Law (subject `ConcreteSpec.Core.Grants.close`). Every close outcome consumes the grant,
including when close returns an error code; the result never contains a handle (Q7.3). -/
theorem close_consumes (s : State H) (i : GrantId) (result : CloseResult) :
    ResultSatisfies (fun r : CloseResult × State H => r.1 = result ∧ Dead r.2 i)
      (close s i result) := by
  cases h : close s i result with
  | none => trivial
  | some r =>
    unfold close at h
    obtain ⟨res, hr, hh⟩ := Option.bind_eq_some_iff.mp h
    cases hh
    exact ⟨rfl, consumed_dead s i res (usable_some s i res hr).1⟩

/-- Law (subject `ConcreteSpec.Core.Grants.narrow`). Narrowing consumes the parent grant identity
(Q7.2). -/
theorem narrow_consumes (s : State H) (i : GrantId) (requested : List Nat) (hs : Fresh s) :
    ResultSatisfies (fun t : State H => Dead t i) (narrow s i requested) := by
  cases h : narrow s i requested with
  | none => trivial
  | some t =>
    unfold narrow at h
    obtain ⟨r, hr, hh⟩ := Option.bind_eq_some_iff.mp h
    cases hh
    exact issue_keeps_dead _ _ i (consumed_fresh s i hs)
      (consumed_dead s i r (usable_some s i r hr).1)

/-- Law (subject `ConcreteSpec.Core.Grants.runOps`). Once consumed, no sequence of permitted grant
operations can revive a resource identity (Q7.2, Q7.3). -/
theorem consumed_never_revives (s : State H) (i : GrantId) (ops : List (Op H))
    (hs : Fresh s) (hd : Dead s i) :
    ResultSatisfies (fun t : State H => Dead t i ∧ Fresh t) (runOps s ops) := by
  cases h : runOps s ops with
  | none => trivial
  | some t =>
    suffices ∀ u, Steps s u → Dead u i ∧ Fresh u from
      this t (steps_of_runOps s ops s t (.refl s) h)
    intro u hsteps
    induction hsteps with
    | refl => exact ⟨hd, hs⟩
    | @narrow t' u' j req _ hn ih =>
      obtain ⟨hd, hf⟩ := ih
      have hfr : Fresh u' := by
        have hw := narrow_fresh t' j req hf
        rwa [hn] at hw
      refine ⟨?_, hfr⟩
      unfold narrow at hn
      obtain ⟨_, _, hh⟩ := Option.bind_eq_some_iff.mp hn
      cases hh
      exact issue_keeps_dead _ _ i (consumed_fresh _ _ hf) (consumed_keeps_dead _ _ _ hd)
    | @acquire t' u' j req sel perms allowed hdl _ ha ih =>
      obtain ⟨hd, hf⟩ := ih
      have hfr : Fresh u' := by
        have hw := acquire_fresh t' j req sel perms allowed hdl hf
        rwa [ha] at hw
      refine ⟨?_, hfr⟩
      unfold acquire at ha
      obtain ⟨_, _, hh⟩ := Option.bind_eq_some_iff.mp ha
      split at hh
      · cases hh; exact issue_keeps_dead _ _ i hf hd
      · contradiction
    | @close t' u' j res _ hc ih =>
      obtain ⟨hd, hf⟩ := ih
      have hfr : Fresh u' := by
        have hw := close_fresh t' j res hf
        rwa [hc] at hw
      refine ⟨?_, hfr⟩
      unfold close at hc
      obtain ⟨_, _, hh⟩ := Option.bind_eq_some_iff.mp hc
      cases hh
      exact consumed_keeps_dead _ _ _ hd

/-- Helper (inversion). `narrow` intersection characterization on a given output state. (approved: APPR-C5-codex-902ce88a). -/
theorem narrow_intersection_result (s t : State H) (i : GrantId) (requested : List Nat)
    (h : narrow s i requested = some t) : ∃ r,
      usable s i = some r ∧ AList.lookup t.resources ⟨s.next⟩ =
        some { r with rights := r.rights.filter (fun x => decide (x ∈ requested)) } := by
  unfold narrow at h
  obtain ⟨r, hr, hh⟩ := Option.bind_eq_some_iff.mp h
  cases hh
  exact ⟨r, hr, by simp [issue, consumed, AList.lookup]⟩

/-- Helper (computation). Calculates the issued child record; narrow_permissions establishes the extensional permission contract independently of list representation. (approved: APPR-C5-codex-902ce88a). -/
theorem narrow_intersection (s : State H) (i : GrantId) (requested : List Nat) :
    ResultSatisfies (fun t : State H =>
      ∃ r, usable s i = some r ∧ AList.lookup t.resources ⟨s.next⟩ =
        some { r with rights := r.rights.filter (fun x => decide (x ∈ requested)) })
      (narrow s i requested) := by
  cases h : narrow s i requested with
  | none => trivial
  | some t => exact narrow_intersection_result s t i requested h

/-- Law (subject `ConcreteSpec.Core.Grants.acquire`). Acquisition leaves its parent's resource,
rights and availability unchanged (Q7.2). -/
theorem acquire_keeps_parent (s : State H) (i : GrantId) (requested selected : List Nat)
    (permissions : List Cap) (allowed : Bool) (handle : H) (hs : Fresh s) :
    ResultSatisfies (fun t : State H => usable t i = usable s i)
      (acquire s i requested selected permissions allowed handle) := by
  cases h : acquire s i requested selected permissions allowed handle with
  | none => trivial
  | some t =>
    unfold acquire at h
    obtain ⟨r, hr, hh⟩ := Option.bind_eq_some_iff.mp h
    have hb := hs _ (AList.lookup_mem (usable_some s i r hr).1)
    have hi : GrantId.mk s.next ≠ i := by
      intro he; rw [← he] at hb; exact Nat.lt_irrefl _ hb
    split at hh
    · cases hh
      simp [ResultSatisfies, usable, issue, AList.lookup, hi]
    · contradiction

namespace GrantWitnesses
open ConcreteSpec.Core.Grants.Examples

def launchBudget : List Nat := [0, 1]
def afterAcquire : State Nat := (acquire initial ⟨0⟩ [0, 1] [1] [.fs] true 99).getD initial
def afterClose0 : State Nat := ((close afterAcquire ⟨0⟩ (.error 5)).map Prod.snd).getD afterAcquire
def sampleResource : Resource Nat := ⟨77, [1], .fs, true⟩
def validOps : List (Op Nat) :=
  [.acquire ⟨0⟩ [0, 1] [1] [.fs] true 99, .narrow ⟨1⟩ [1], .close ⟨2⟩ (.ok ())]
def postCloseOps : List (Op Nat) := [.narrow ⟨1⟩ [1]]

/-- Mutant: issuing a resource overwrites grant `0` instead of allocating at `s.next`. -/
def issueOverwriteZero {H : Type} (s : State H) (r : Resource H) : State H :=
  ⟨(⟨0⟩, r) :: s.resources, s.next + 1⟩

/-- Mutant: narrowing replaces rights with `requested` instead of intersecting with parent rights. -/
def narrowTakeRequested {H : Type} (s : State H) (i : GrantId) (requested : List Nat) :
    Option (State H) := do
  let r ← usable s i
  pure (issue (consumed s i) { r with rights := requested })

/-- Mutant: narrowing issues the child without advancing `s.next`. -/
def narrowNoAdvance {H : Type} (s : State H) (i : GrantId) (requested : List Nat) :
    Option (State H) := do
  let r ← usable s i
  let c := consumed s i
  pure ⟨(⟨c.next⟩, { r with rights := r.rights.filter (fun x => decide (x ∈ requested)) }) :: c.resources, c.next⟩

/-- Mutant: narrowing leaves the parent live instead of consuming it. -/
def narrowKeepParent {H : Type} (s : State H) (i : GrantId) (requested : List Nat) :
    Option (State H) := do
  let r ← usable s i
  pure (issue s { r with rights := r.rights.filter (fun x => decide (x ∈ requested)) })

/-- Mutant: acquisition grants `selected` without checking containment in parent rights. -/
def acquireUncheckedSelected {H : Type} (s : State H) (i : GrantId) (_requested selected : List Nat)
    (_permissions : List Cap) (_policyAllowed : Bool) (handle : H) : Option (State H) := do
  let r ← usable s i
  some (issue s ⟨handle, selected, r.permission, true⟩)

/-- Mutant: acquisition issues the child without advancing `s.next`. -/
def acquireNoAdvance {H : Type} (s : State H) (i : GrantId) (requested selected : List Nat)
    (permissions : List Cap) (policyAllowed : Bool) (handle : H) : Option (State H) := do
  let r ← usable s i
  if r.permission ∈ permissions ∧ policyAllowed = true ∧
      ∀ right ∈ selected, right ∈ r.rights ∧ right ∈ requested then
    some ⟨(⟨s.next⟩, ⟨handle, selected, r.permission, true⟩) :: s.resources, s.next⟩
  else none

/-- Mutant: acquisition consumes its parent like narrowing does. -/
def acquireConsumeParent {H : Type} (s : State H) (i : GrantId) (requested selected : List Nat)
    (permissions : List Cap) (policyAllowed : Bool) (handle : H) : Option (State H) := do
  let r ← usable s i
  if r.permission ∈ permissions ∧ policyAllowed = true ∧
      ∀ right ∈ selected, right ∈ r.rights ∧ right ∈ requested then
    some (issue (consumed s i) ⟨handle, selected, r.permission, true⟩)
  else none

/-- Mutant: close escalates the closed resource's rights outside the launch budget. -/
def closeEscalateRights {H : Type} (s : State H) (i : GrantId) (outcome : CloseResult) :
    Option (CloseResult × State H) := do
  let _ ← usable s i
  pure (outcome, { s with resources := AList.update s.resources i (fun r => { r with rights := 99 :: r.rights, live := false }) })

/-- Mutant: close rewinds `s.next` to `0`. -/
def closeRewindSupply {H : Type} (s : State H) (i : GrantId) (outcome : CloseResult) :
    Option (CloseResult × State H) := do
  let _ ← usable s i
  pure (outcome, { consumed s i with next := 0 })

/-- Mutant: close leaves the resource live when an error code is returned. -/
def closeKeepLive {H : Type} (s : State H) (i : GrantId) (outcome : CloseResult) :
    Option (CloseResult × State H) := do
  let _ ← usable s i
  pure (outcome, s)

/-- Mutant: usability lookup ignores the `live` flag and returns consumed resources. -/
def usableIgnoreLive {H : Type} (s : State H) (i : GrantId) : Option (Resource H) :=
  AList.lookup s.resources i

/-- Mutant: multi-step execution mints an out-of-budget right `99` on every resource at completion. -/
def runOpsMintRight {H : Type} (s : State H) (ops : List (Op H)) : Option (State H) :=
  (runOps s ops).map fun t =>
    { t with resources := t.resources.map fun r => { r with rights := 99 :: r.rights } }

/-- Mutant: multi-step execution revives consumed resources by setting `live := true`. -/
def runOpsRevive {H : Type} (s : State H) (ops : List (Op H)) : Option (State H) :=
  (runOps s ops).map fun t =>
    { t with resources := t.resources.map fun r => { r with live := true } }

/-- Mutant: successful acquisition inserts an ungranted right into its child. -/
def acquireEscalate {H : Type} (s : State H) (i : GrantId) (requested selected : List Nat)
    (permissions : List Cap) (allowed : Bool) (handle : H) : Option (State H) :=
  (acquire s i requested selected permissions allowed handle).map fun t =>
    { t with resources := (AList.update t.resources ⟨s.next⟩
      (fun r => { r with rights := 99 :: r.rights })) }

-- Grant laws (14)

/-- Instance (law `ConcreteSpec.Core.Grants.issue_keeps_dead`, case `ConcreteSpec.Core.Grants.GrantWitnesses.afterClose0`). -/
example : Dead (issue afterClose0 sampleResource) ⟨0⟩ :=
  @issue_keeps_dead Nat afterClose0 sampleResource ⟨0⟩ (by decide) (by decide)

/-- Planted (law `ConcreteSpec.Core.Grants.issue_keeps_dead`, case `ConcreteSpec.Core.Grants.GrantWitnesses.afterClose0`, subject `ConcreteSpec.Core.Grants.issue`, mutant `ConcreteSpec.Core.Grants.GrantWitnesses.issueOverwriteZero`). -/
example : Fresh afterClose0 ∧ Dead afterClose0 ⟨0⟩ ∧
    ¬ Dead (issueOverwriteZero afterClose0 sampleResource) ⟨0⟩ := by decide

/-- Instance (law `ConcreteSpec.Core.Grants.narrow_bounded`, case `ConcreteSpec.Core.Grants.Examples.initial`). -/
example : ResultSatisfies (Bounded launchBudget) (narrow initial ⟨0⟩ [1, 2]) :=
  @narrow_bounded Nat launchBudget initial ⟨0⟩ [1, 2] (by decide)

/-- Planted (law `ConcreteSpec.Core.Grants.narrow_bounded`, case `ConcreteSpec.Core.Grants.Examples.initial`, subject `ConcreteSpec.Core.Grants.narrow`, mutant `ConcreteSpec.Core.Grants.GrantWitnesses.narrowTakeRequested`). -/
example : Bounded launchBudget initial ∧
    ¬ ResultSatisfies (Bounded launchBudget) (narrowTakeRequested initial ⟨0⟩ [1, 2]) := by decide

/-- Instance (law `ConcreteSpec.Core.Grants.acquire_bounded`, case `ConcreteSpec.Core.Grants.Examples.initial`). -/
example : ResultSatisfies (Bounded launchBudget) (acquire initial ⟨0⟩ [1, 2] [1] [.fs] true 99) :=
  @acquire_bounded Nat launchBudget initial ⟨0⟩ [1, 2] [1] [.fs] true 99 (by decide)

/-- Planted (law `ConcreteSpec.Core.Grants.acquire_bounded`, case `ConcreteSpec.Core.Grants.Examples.initial`, subject `ConcreteSpec.Core.Grants.acquire`, mutant `ConcreteSpec.Core.Grants.GrantWitnesses.acquireEscalate`). -/
example : Bounded launchBudget initial ∧
    ¬ ResultSatisfies (Bounded launchBudget)
      (acquireEscalate initial ⟨0⟩ [1, 2] [1] [.fs] true 99) := by decide

/-- Instance (law `ConcreteSpec.Core.Grants.close_bounded`, case `ConcreteSpec.Core.Grants.Examples.initial`). -/
example : ResultSatisfies (fun r : CloseResult × State Nat => Bounded launchBudget r.2)
    (close initial ⟨0⟩ (.error 5)) :=
  @close_bounded Nat launchBudget initial ⟨0⟩ (.error 5) (by decide)

/-- Planted (law `ConcreteSpec.Core.Grants.close_bounded`, case `ConcreteSpec.Core.Grants.Examples.initial`, subject `ConcreteSpec.Core.Grants.close`, mutant `ConcreteSpec.Core.Grants.GrantWitnesses.closeEscalateRights`). -/
example : Bounded launchBudget initial ∧
    ¬ ResultSatisfies (fun r : CloseResult × State Nat => Bounded launchBudget r.2)
      (closeEscalateRights initial ⟨0⟩ (.error 5)) := by decide

/-- Instance (law `ConcreteSpec.Core.Grants.rights_never_grow`, case `ConcreteSpec.Core.Grants.Examples.initial`). -/
example : ResultSatisfies (Bounded launchBudget) (runOps initial validOps) :=
  @rights_never_grow Nat launchBudget initial validOps (by decide)

/-- Planted (law `ConcreteSpec.Core.Grants.rights_never_grow`, case `ConcreteSpec.Core.Grants.Examples.initial`, subject `ConcreteSpec.Core.Grants.runOps`, mutant `ConcreteSpec.Core.Grants.GrantWitnesses.runOpsMintRight`). -/
example : Bounded launchBudget initial ∧
    ¬ ResultSatisfies (Bounded launchBudget) (runOpsMintRight initial validOps) := by decide

/-- Instance (law `ConcreteSpec.Core.Grants.narrow_fresh`, case `ConcreteSpec.Core.Grants.Examples.initial`). -/
example : ResultSatisfies Fresh (narrow initial ⟨0⟩ [1, 2]) :=
  @narrow_fresh Nat initial ⟨0⟩ [1, 2] (by decide)

/-- Planted (law `ConcreteSpec.Core.Grants.narrow_fresh`, case `ConcreteSpec.Core.Grants.Examples.initial`, subject `ConcreteSpec.Core.Grants.narrow`, mutant `ConcreteSpec.Core.Grants.GrantWitnesses.narrowNoAdvance`). -/
example : Fresh initial ∧
    ¬ ResultSatisfies Fresh (narrowNoAdvance initial ⟨0⟩ [1, 2]) := by decide

/-- Instance (law `ConcreteSpec.Core.Grants.acquire_fresh`, case `ConcreteSpec.Core.Grants.Examples.initial`). -/
example : ResultSatisfies Fresh (acquire initial ⟨0⟩ [1] [1] [.fs] true 99) :=
  @acquire_fresh Nat initial ⟨0⟩ [1] [1] [.fs] true 99 (by decide)

/-- Planted (law `ConcreteSpec.Core.Grants.acquire_fresh`, case `ConcreteSpec.Core.Grants.Examples.initial`, subject `ConcreteSpec.Core.Grants.acquire`, mutant `ConcreteSpec.Core.Grants.GrantWitnesses.acquireNoAdvance`). -/
example : Fresh initial ∧
    ¬ ResultSatisfies Fresh (acquireNoAdvance initial ⟨0⟩ [1] [1] [.fs] true 99) := by decide

/-- Instance (law `ConcreteSpec.Core.Grants.close_fresh`, case `ConcreteSpec.Core.Grants.Examples.initial`). -/
example : ResultSatisfies (fun r : CloseResult × State Nat => Fresh r.2)
    (close initial ⟨0⟩ (.error 5)) :=
  @close_fresh Nat initial ⟨0⟩ (.error 5) (by decide)

/-- Planted (law `ConcreteSpec.Core.Grants.close_fresh`, case `ConcreteSpec.Core.Grants.Examples.initial`, subject `ConcreteSpec.Core.Grants.close`, mutant `ConcreteSpec.Core.Grants.GrantWitnesses.closeRewindSupply`). -/
example : Fresh initial ∧
    ¬ ResultSatisfies (fun r : CloseResult × State Nat => Fresh r.2)
      (closeRewindSupply initial ⟨0⟩ (.error 5)) := by decide

/-- Computation observation. -/
example : usable afterClose0 ⟨0⟩ = none :=
  @dead_unusable Nat afterClose0 ⟨0⟩ (by decide)

/-- Computation observation. -/
example : Dead afterClose0 ⟨0⟩ ∧
    ¬ (usableIgnoreLive afterClose0 ⟨0⟩ = none) := by decide

/-- Instance (law `ConcreteSpec.Core.Grants.close_consumes`, case `ConcreteSpec.Core.Grants.Examples.initial`). -/
example : ResultSatisfies (fun r : CloseResult × State Nat => r.1 = .error 5 ∧ Dead r.2 ⟨0⟩)
    (close initial ⟨0⟩ (.error 5)) :=
  @close_consumes Nat initial ⟨0⟩ (.error 5)

/-- Planted (law `ConcreteSpec.Core.Grants.close_consumes`, case `ConcreteSpec.Core.Grants.Examples.initial`, subject `ConcreteSpec.Core.Grants.close`, mutant `ConcreteSpec.Core.Grants.GrantWitnesses.closeKeepLive`). -/
example : ¬ ResultSatisfies (fun r : CloseResult × State Nat => r.1 = .error 5 ∧ Dead r.2 ⟨0⟩)
    (closeKeepLive initial ⟨0⟩ (.error 5)) := by decide

/-- Instance (law `ConcreteSpec.Core.Grants.narrow_consumes`, case `ConcreteSpec.Core.Grants.Examples.initial`). -/
example : ResultSatisfies (fun t : State Nat => Dead t ⟨0⟩) (narrow initial ⟨0⟩ [1, 2]) :=
  @narrow_consumes Nat initial ⟨0⟩ [1, 2] (by decide)

/-- Planted (law `ConcreteSpec.Core.Grants.narrow_consumes`, case `ConcreteSpec.Core.Grants.Examples.initial`, subject `ConcreteSpec.Core.Grants.narrow`, mutant `ConcreteSpec.Core.Grants.GrantWitnesses.narrowKeepParent`). -/
example : Fresh initial ∧
    ¬ ResultSatisfies (fun t : State Nat => Dead t ⟨0⟩)
      (narrowKeepParent initial ⟨0⟩ [1, 2]) := by decide

/-- Instance (law `ConcreteSpec.Core.Grants.consumed_never_revives`, case `ConcreteSpec.Core.Grants.GrantWitnesses.afterClose0`). -/
example : ResultSatisfies (fun t : State Nat => Dead t ⟨0⟩ ∧ Fresh t)
    (runOps afterClose0 postCloseOps) :=
  @consumed_never_revives Nat afterClose0 ⟨0⟩ postCloseOps (by decide) (by decide)

/-- Planted (law `ConcreteSpec.Core.Grants.consumed_never_revives`, case `ConcreteSpec.Core.Grants.GrantWitnesses.afterClose0`, subject `ConcreteSpec.Core.Grants.runOps`, mutant `ConcreteSpec.Core.Grants.GrantWitnesses.runOpsRevive`). -/
example : Fresh afterClose0 ∧
    Dead afterClose0 ⟨0⟩ ∧
    ¬ ResultSatisfies (fun t : State Nat => Dead t ⟨0⟩ ∧ Fresh t)
      (runOpsRevive afterClose0 postCloseOps) := by decide

/-- Computation observation. -/
example : ResultSatisfies (fun t : State Nat =>
      ∃ r, usable initial ⟨0⟩ = some r ∧ AList.lookup t.resources ⟨initial.next⟩ =
        some { r with rights := r.rights.filter (fun x => decide (x ∈ [1, 2])) })
    (narrow initial ⟨0⟩ [1, 2]) :=
  @narrow_intersection Nat initial ⟨0⟩ [1, 2]

/-- Computation observation. -/
example : ¬ ResultSatisfies (fun t : State Nat =>
      ∃ r, usable initial ⟨0⟩ = some r ∧ AList.lookup t.resources ⟨initial.next⟩ =
        some { r with rights := r.rights.filter (fun x => decide (x ∈ [1, 2])) })
    (narrowTakeRequested initial ⟨0⟩ [1, 2]) := by
  rintro ⟨r, hr, hlookup⟩
  have hr' : r = ⟨42, [0, 1], .fs, true⟩ := Option.some.inj (hr.symm.trans rfl)
  subst hr'
  revert hlookup
  decide

/-- Instance (law `ConcreteSpec.Core.Grants.acquire_keeps_parent`, case `ConcreteSpec.Core.Grants.Examples.initial`). -/
example : ResultSatisfies (fun t : State Nat => usable t ⟨0⟩ = usable initial ⟨0⟩)
    (acquire initial ⟨0⟩ [1] [1] [.fs] true 99) :=
  @acquire_keeps_parent Nat initial ⟨0⟩ [1] [1] [.fs] true 99 (by decide)

/-- Planted (law `ConcreteSpec.Core.Grants.acquire_keeps_parent`, case `ConcreteSpec.Core.Grants.Examples.initial`, subject `ConcreteSpec.Core.Grants.acquire`, mutant `ConcreteSpec.Core.Grants.GrantWitnesses.acquireConsumeParent`). -/
example : Fresh initial ∧
    ¬ ResultSatisfies (fun t : State Nat => usable t ⟨0⟩ = usable initial ⟨0⟩)
      (acquireConsumeParent initial ⟨0⟩ [1] [1] [.fs] true 99) := by decide

end GrantWitnesses

/-- Law (subject `ConcreteSpec.Core.Grants.narrow`). A narrowed child permits exactly those
operations that both its parent permitted and its requested policy permits. This includes
completeness of the intersection, not just a budget bound or a copied resource constructor. -/
theorem narrow_permissions (s : State H) (i : GrantId) (requested : List Nat) :
    ResultSatisfies (fun t => ∀ right,
      Permits t ⟨s.next⟩ right ↔ Permits s i right ∧ right ∈ requested)
      (narrow s i requested) := by
  cases h : narrow s i requested with
  | none => trivial
  | some t =>
    obtain ⟨r, hr, hc⟩ := narrow_intersection_result s t i requested h
    have live := (usable_some s i r hr).2
    have hu : usable t ⟨s.next⟩ =
        some { r with rights := r.rights.filter (fun x => decide (x ∈ requested)) } := by
      simp [usable, hc, live]
    intro right
    simp [Permits, hu, hr]

namespace PermissionEvidence
open ConcreteSpec.Core.Grants.Examples GrantWitnesses

/-- Instance (law `ConcreteSpec.Core.Grants.narrow_permissions`, case `ConcreteSpec.Core.Grants.Examples.initial`). -/
example : ResultSatisfies (fun t => ∀ right,
    Permits t ⟨initial.next⟩ right ↔ Permits initial ⟨0⟩ right ∧ right ∈ [1, 2])
    (narrow initial ⟨0⟩ [1, 2]) :=
  @narrow_permissions Nat initial ⟨0⟩ [1, 2]

/-- Planted (law `ConcreteSpec.Core.Grants.narrow_permissions`, case `ConcreteSpec.Core.Grants.Examples.initial`, subject `ConcreteSpec.Core.Grants.narrow`, mutant `ConcreteSpec.Core.Grants.GrantWitnesses.narrowTakeRequested`). -/
example : ¬ ResultSatisfies (fun t => ∀ right,
    Permits t ⟨initial.next⟩ right ↔ Permits initial ⟨0⟩ right ∧ right ∈ [1, 2])
    (narrowTakeRequested initial ⟨0⟩ [1, 2]) := by
  intro h
  have bad := (h 2).mp ⟨⟨42, [1, 2], .fs, true⟩, rfl, by decide⟩
  have no : ¬ Permits initial ⟨0⟩ 2 := by
    simp [Permits, Examples.initial, usable, AList.lookup]
  exact no bad.1

end PermissionEvidence

end ConcreteSpec.Core.Grants
