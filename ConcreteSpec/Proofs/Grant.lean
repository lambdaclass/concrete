import ConcreteSpec.Spec.Grant
import ConcreteSpec.Proofs.Basics

/-! # C5 brand lifetime and grant attenuation laws -/
namespace ConcreteSpec.Core.Generative

private theorem fresh_wf (s : State) (scope : BlockId) (hs : WellFormed s) :
    WellFormed { s with
      binders := ⟨⟨s.nextBrand⟩, ⟨s.nextLineage⟩, scope, true⟩ :: s.binders
      nextBrand := s.nextBrand + 1
      nextLineage := s.nextLineage + 1 } := by
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

theorem fresh_preserves (s t : State) (scope : BlockId) (hs : WellFormed s)
    (h : fresh s scope = some t) : WellFormed t := by
  cases h
  exact fresh_wf s scope hs

theorem fresh_progress (s : State) (scope : BlockId) :
    ∃ t, fresh s scope = some t := ⟨_, rfl⟩

theorem depend_preserves (s t : State) (d : Dependency) (hs : WellFormed s)
    (h : depend s d = some t) : WellFormed t := by
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

theorem consume_preserves (s t : State) (p : Hybrid.Place) (hs : WellFormed s)
    (h : consume s p = some t) : WellFormed t := by
  unfold consume at h
  split at h
  · cases h
    refine ⟨⟨hs.1.1, ?_⟩, hs.2.1, ?_⟩
    · exact hs.1.2.sublist (List.Sublist.map _ (List.filter_sublist))
    · intro d hd
      exact hs.2.2 d (List.mem_filter.mp hd).1
  · contradiction

theorem release_preserves (s t : State) (i : BrandId) (hs : WellFormed s)
    (h : release s i = some t) : WellFormed t := by
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

theorem reset_preserves (s t : State) (i : BrandId) (hs : WellFormed s)
    (h : reset s i = some t) : WellFormed t := by
  unfold reset at h
  obtain ⟨b, hb, hh⟩ := Option.bind_eq_some_iff.mp h
  obtain ⟨u, hu, hh⟩ := Option.bind_eq_some_iff.mp hh
  cases hh
  have hw := release_preserves s u i hs hu
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

theorem endScope_preserves (s t : State) (scope : BlockId) (hs : WellFormed s)
    (h : endScope s scope = some t) : WellFormed t := by
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

/-- All guards concern the dependency being registered, not the rest of the invariant. -/
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

theorem consume_progress (s : State) (p : Hybrid.Place)
    (present : ∃ d ∈ s.dependencies, d.place = p) : ∃ t, consume s p = some t := by
  obtain ⟨d, hd, hp⟩ := present
  have hg : (s.dependencies.any (·.place == p)) = true :=
    List.any_eq_true.mpr ⟨d, hd, by simp [hp]⟩
  simp [consume, hg]

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

namespace PreservationWitnesses
open ConcreteSpec.Core.Generative.Examples

def dependent : State := { initial with dependencies := [heap, phantom] }
def released : State := (release initial ⟨0⟩).getD {}
def resetOnce : State := (reset initial ⟨0⟩).getD {}

/-- Positive instances of all six structural preservation laws. -/
example : WellFormed initial ∧ fresh {} ⟨0⟩ = some initial := by decide
example : WellFormed dependent ∧ depend { initial with dependencies := [phantom] } heap =
    some dependent := by decide
example : WellFormed dependent ∧
    (consume dependent ⟨0⟩).isSome = true := by decide
example : WellFormed initial ∧ release initial ⟨0⟩ = some released ∧
    WellFormed released := by decide
example : WellFormed initial ∧ reset initial ⟨0⟩ = some resetOnce ∧
    WellFormed resetOnce := by decide
example : WellFormed released ∧ (endScope released ⟨0⟩).isSome = true := by decide

/-- Planted fresh/reset mutations fail supply freshness. -/
example : ¬ WellFormed { initial with nextBrand := 0 } := by decide
example : ¬ WellFormed { resetOnce with nextBrand := 1 } := by decide
/-- Planted depend mutation duplicates a resource place. -/
example : ¬ WellFormed { dependent with dependencies := heap :: dependent.dependencies } := by decide
/-- Planted consume mutation removes the binder instead of the dependent. -/
example : ¬ WellFormed { dependent with binders := [] } := by decide
/-- Planted release mutation ignores the live-allocation guard. -/
example : ¬ WellFormed { released with dependencies := [heap] } := by decide
/-- Planted scope-exit mutation ignores phantom dependencies. -/
example : ¬ WellFormed { dependent with binders := [] , dependencies := [phantom] } := by decide

/-- Positive progress instances and negated success for violated operational guards. -/
example : (fresh initial ⟨1⟩).isSome = true := by decide
example : (depend initial heap).isSome = true ∧ depend dependent heap = none := by decide
example : (consume dependent ⟨0⟩).isSome = true ∧ consume initial ⟨0⟩ = none := by decide
example : (release initial ⟨0⟩).isSome = true ∧ release dependent ⟨0⟩ = none := by decide
example : (endScope released ⟨0⟩).isSome = true ∧ endScope initial ⟨0⟩ = none := by decide
/-- A mutated fresh rule that reuses the counter does not provide fresh generation. -/
example : ¬ (initial.nextBrand = ({} : State).nextBrand) := by decide
/-- A non-escaping dependent survives a different binder's successful scope exit:
the non-escape law's dependent-membership hypothesis is inhabited. -/
def outer : State := (fresh initial ⟨1⟩).getD {}
def outside : Dependency := ⟨⟨3⟩, [⟨1⟩], []⟩
def departing : State := ((depend outer outside).bind fun s => release s ⟨0⟩).getD {}
def departed : State := (endScope departing ⟨0⟩).getD {}
example : WellFormed departing ∧ endScope departing ⟨0⟩ = some departed ∧
    outside ∈ departed.dependencies ∧ (BrandId.mk 0) ∉ outside.brands := by decide
/-- Planted exit mutation lets the surviving dependent mention the exited brand. -/
example : ¬ ((BrandId.mk 0) ∉ ({ outside with brands := [⟨0⟩] } : Dependency).brands) := by decide
end PreservationWitnesses

/-- A new/reset generation is unequal to every previously scoped brand, independently of
representation or lexical site. Supplies are never decremented when a scope ends. -/
theorem nextBrand_fresh (s : State) (hs : WellFormed s) (b : Binder) (hb : b ∈ s.binders) :
    b.brand ≠ BrandId.mk s.nextBrand := by
  intro he
  have hn := (hs.2.1 b hb).1
  rw [he] at hn
  exact Nat.lt_irrefl _ hn

theorem fresh_generation (s t : State) (scope : BlockId) (h : fresh s scope = some t) :
    t.nextBrand = s.nextBrand + 1 ∧ ∃ b ∈ t.binders, b.brand = ⟨s.nextBrand⟩ := by
  cases h
  exact ⟨rfl, _, List.mem_cons_self, rfl⟩

theorem reset_generation (s t : State) (i : BrandId) (h : reset s i = some t) :
    t.nextBrand = s.nextBrand + 1 ∧ ∃ b ∈ t.binders, b.brand = ⟨s.nextBrand⟩ := by
  unfold reset at h
  obtain ⟨b, _, hh⟩ := Option.bind_eq_some_iff.mp h
  obtain ⟨u, _, hh⟩ := Option.bind_eq_some_iff.mp hh
  cases hh
  exact ⟨rfl, _, List.mem_cons_self, rfl⟩

/-- Successful lexical exit leaves no dependent value mentioning any brand bound in that
scope. This includes phantom dependencies, which need no live allocation. -/
theorem endScope_non_escape (s t : State) (scope : BlockId)
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

/-- Reset/release cannot leave a dependent allocation naming a consumed allocator. -/
theorem allocations_have_live_allocator (s : State) (hs : WellFormed s) (d : Dependency)
    (hd : d ∈ s.dependencies) (i : BrandId) (hi : i ∈ d.allocations) :
    ∃ b ∈ s.binders, b.brand = i ∧ b.allocatorLive = true :=
  (hs.2.2 d hd).2 i hi |>.2

/-- Successful brand instrumentation preserves exactly the concrete operation's result,
state and branch. Reset generations cannot influence that result. -/
theorem instrument_erasure {σ α : Type} (transition : State → Option State)
    (operation : σ → Option (α × σ)) (s : σ × State) (a : α) (concrete : σ) (ghost : State)
    (h : instrument transition operation s = some (a, (concrete, ghost))) :
    operation s.1 = some (a, concrete) := by
  unfold instrument at h
  obtain ⟨_, _, hh⟩ := Option.bind_eq_some_iff.mp h
  obtain ⟨r, hr, hh⟩ := Option.bind_eq_some_iff.mp hh
  cases r
  cases hh
  exact hr

/-- Every successful finite allocator run erases to the same ordered results and concrete
state. This is one-way: instrumentation refusal does not imply concrete refusal. -/
theorem run_erasure {σ α : Type} (cs : List (Command σ α)) (s : σ × State)
    (results : List α) (t : σ × State) (h : run cs s = some (results, t)) :
    runErased cs s.1 = some (results, t.1) := by
  induction cs generalizing s results with
  | nil => cases h; rfl
  | cons c cs ih =>
    simp only [run] at h
    obtain ⟨⟨a, u⟩, hu, h⟩ := Option.bind_eq_some_iff.mp h
    obtain ⟨⟨as, v⟩, hv, h⟩ := Option.bind_eq_some_iff.mp h
    cases h
    have hc := instrument_erasure c.transition c.operation s a u.1 u.2 hu
    have ht := ih u as hv
    simp [runErased, hc, ht]

namespace RunWitnesses
open ConcreteSpec.Core.Generative.Examples

def resetCommand (i : BrandId) : Command Nat Nat :=
  ⟨fun s => reset s i, fun n => some (n, n + 1)⟩
def commands : List (Command Nat Nat) := [resetCommand ⟨0⟩, resetCommand ⟨1⟩]
def finalState : State := ((reset initial ⟨0⟩).bind fun s => reset s ⟨1⟩).getD {}
/-- Positive: two generative resets preserve both concrete results, their order and state. -/
example : run commands (7, initial) = some ([7, 8], (9, finalState)) ∧
    runErased commands 7 = some ([7, 8], 9) := by decide
/-- Planted erasure mutation drops the first instruction, losing its effect and result. -/
example : ¬ runErased commands.tail 7 = some ([7, 8], 9) := by decide
/-- A refused brand operation is not a refused concrete allocator operation. -/
example : run [resetCommand ⟨0⟩] (7, PreservationWitnesses.dependent) = none ∧
    runErased [resetCommand ⟨0⟩] 7 = some ([7], 8) := by decide
end RunWitnesses

end ConcreteSpec.Core.Generative

namespace ConcreteSpec.Core.Grants
open ConcreteSpec.Authority (Cap)

variable {H : Type}

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

/-- Successful usability exposes both the original resource and its live flag. -/
theorem usable_some (s : State H) (i : GrantId) (r : Resource H) (h : usable s i = some r) :
    AList.lookup s.resources i = some r ∧ r.live = true := by
  unfold usable at h
  obtain ⟨a, ha, hh⟩ := Option.bind_eq_some_iff.mp h
  split at hh
  · cases hh; exact ⟨ha, by assumption⟩
  · contradiction

theorem consumed_bounded (budget : List Nat) (s : State H) (i : GrantId)
    (hs : Bounded budget s) : Bounded budget (consumed s i) :=
  update_all (fun (e : GrantId × Resource H) => ∀ right ∈ e.2.rights, right ∈ budget)
    i (fun r => { r with live := false }) (fun _ h => h) s.resources hs

theorem consumed_fresh (s : State H) (i : GrantId) (hs : Fresh s) : Fresh (consumed s i) :=
  update_all (fun (e : GrantId × Resource H) => e.1.id < s.next)
    i (fun r => { r with live := false }) (fun _ h => h) s.resources hs

theorem issue_bounded (budget : List Nat) (s : State H) (r : Resource H)
    (hs : Bounded budget s) (hr : ∀ x ∈ r.rights, x ∈ budget) : Bounded budget (issue s r) := by
  intro e he
  change e ∈ (⟨s.next⟩, r) :: s.resources at he
  rcases List.mem_cons.mp he with rfl | he
  · exact hr
  · exact hs e he

theorem issue_fresh (s : State H) (r : Resource H) (hs : Fresh s) : Fresh (issue s r) := by
  intro e he
  change e ∈ (⟨s.next⟩, r) :: s.resources at he
  rcases List.mem_cons.mp he with rfl | he
  · exact Nat.lt_succ_self _
  · exact Nat.lt_succ_of_lt (hs e he)

theorem narrow_bounded (budget : List Nat) (s t : State H) (i : GrantId) (requested : List Nat)
    (hs : Bounded budget s) (h : narrow s i requested = some t) : Bounded budget t := by
  unfold narrow at h
  obtain ⟨r, hr, hh⟩ := Option.bind_eq_some_iff.mp h
  cases hh
  apply issue_bounded _ _ _ (consumed_bounded budget s i hs)
  intro x hx
  exact hs _ (AList.lookup_mem (usable_some s i r hr).1) x (List.mem_filter.mp hx).1

theorem acquire_bounded (budget : List Nat) (s t : State H) (i : GrantId)
    (requested selected : List Nat) (permissions : List Cap) (allowed : Bool) (handle : H)
    (hs : Bounded budget s) (h : acquire s i requested selected permissions allowed handle = some t) :
    Bounded budget t := by
  unfold acquire at h
  obtain ⟨r, hr, hh⟩ := Option.bind_eq_some_iff.mp h
  split at hh
  · rename_i guard
    cases hh
    apply issue_bounded _ _ _ hs
    intro x hx
    exact hs _ (AList.lookup_mem (usable_some s i r hr).1) x (guard.2.2 x hx).1
  · contradiction

theorem close_bounded (budget : List Nat) (s t : State H) (i : GrantId) (result : CloseResult)
    (hs : Bounded budget s) (h : close s i result = some (result, t)) : Bounded budget t := by
  unfold close at h
  obtain ⟨_, _, hh⟩ := Option.bind_eq_some_iff.mp h
  cases hh
  exact consumed_bounded budget s i hs

/-- Every finite sequence of narrowing, acquisition and close stays within the initial
launch rights. Retaining parents during acquisition does not enlarge the authority set. -/
theorem rights_never_grow (budget : List Nat) (s t : State H) (hs : Bounded budget s)
    (h : Steps s t) : Bounded budget t := by
  induction h with
  | refl => exact hs
  | narrow _ hn ih => exact narrow_bounded _ _ _ _ _ ih hn
  | acquire _ ha ih => exact acquire_bounded _ _ _ _ _ _ _ _ _ ih ha
  | close _ hc ih => exact close_bounded _ _ _ _ _ ih hc

theorem narrow_fresh (s t : State H) (i : GrantId) (requested : List Nat)
    (hs : Fresh s) (h : narrow s i requested = some t) : Fresh t := by
  unfold narrow at h
  obtain ⟨_, _, hh⟩ := Option.bind_eq_some_iff.mp h
  cases hh
  exact issue_fresh _ _ (consumed_fresh s i hs)

theorem acquire_fresh (s t : State H) (i : GrantId) (requested selected : List Nat)
    (permissions : List Cap) (allowed : Bool) (handle : H)
    (hs : Fresh s) (h : acquire s i requested selected permissions allowed handle = some t) : Fresh t := by
  unfold acquire at h
  obtain ⟨_, _, hh⟩ := Option.bind_eq_some_iff.mp h
  split at hh
  · cases hh; exact issue_fresh _ _ hs
  · contradiction

theorem close_fresh (s t : State H) (i : GrantId) (result : CloseResult)
    (hs : Fresh s) (h : close s i result = some (result, t)) : Fresh t := by
  unfold close at h
  obtain ⟨_, _, hh⟩ := Option.bind_eq_some_iff.mp h
  cases hh
  exact consumed_fresh s i hs

/-- Consumption is a persistent historical fact, rather than absence from a map. -/
def Dead (s : State H) (i : GrantId) : Prop :=
  ∃ r, AList.lookup s.resources i = some r ∧ r.live = false

theorem dead_unusable (s : State H) (i : GrantId) (h : Dead s i) : usable s i = none := by
  obtain ⟨r, hr, hd⟩ := h
  simp [usable, hr, hd]

theorem consumed_dead (s : State H) (i : GrantId) (r : Resource H)
    (hr : AList.lookup s.resources i = some r) : Dead (consumed s i) i := by
  refine ⟨{ r with live := false }, ?_, rfl⟩
  simp [consumed, AList.lookup_update, hr]

theorem consumed_keeps_dead (s : State H) (i j : GrantId) (h : Dead s i) :
    Dead (consumed s j) i := by
  obtain ⟨r, hr, hd⟩ := h
  by_cases he : i = j
  · subst j; exact consumed_dead s i r hr
  · exact ⟨r, by simp [consumed, AList.lookup_update, he, hr], hd⟩

theorem issue_keeps_dead (s : State H) (r : Resource H) (i : GrantId)
    (hs : Fresh s) (h : Dead s i) : Dead (issue s r) i := by
  obtain ⟨a, ha, hd⟩ := h
  have hb := hs _ (AList.lookup_mem ha)
  have hi : GrantId.mk s.next ≠ i := by
    intro he; rw [← he] at hb; exact Nat.lt_irrefl _ hb
  exact ⟨a, by simp [issue, AList.lookup, hi, ha], hd⟩

/-- Every close outcome consumes; the result never contains a handle. -/
theorem close_consumes (s t : State H) (i : GrantId) (result : CloseResult)
    (h : close s i result = some (result, t)) : Dead t i := by
  unfold close at h
  obtain ⟨r, hr, hh⟩ := Option.bind_eq_some_iff.mp h
  cases hh
  exact consumed_dead s i r (usable_some s i r hr).1

theorem narrow_consumes (s t : State H) (i : GrantId) (requested : List Nat)
    (hs : Fresh s) (h : narrow s i requested = some t) : Dead t i := by
  unfold narrow at h
  obtain ⟨r, hr, hh⟩ := Option.bind_eq_some_iff.mp h
  cases hh
  exact issue_keeps_dead _ _ i (consumed_fresh s i hs)
    (consumed_dead s i r (usable_some s i r hr).1)

/-- Once consumed, no sequence of these operations can revive a resource identity. -/
theorem consumed_never_revives (s t : State H) (i : GrantId) (hs : Fresh s) (hd : Dead s i)
    (h : Steps s t) : Dead t i ∧ Fresh t := by
  induction h with
  | refl => exact ⟨hd, hs⟩
  | narrow _ hn ih =>
    obtain ⟨hd, hf⟩ := ih
    refine ⟨?_, narrow_fresh _ _ _ _ hf hn⟩
    unfold narrow at hn
    obtain ⟨_, _, hh⟩ := Option.bind_eq_some_iff.mp hn
    cases hh
    exact issue_keeps_dead _ _ i (consumed_fresh _ _ hf) (consumed_keeps_dead _ _ _ hd)
  | acquire _ ha ih =>
    obtain ⟨hd, hf⟩ := ih
    refine ⟨?_, acquire_fresh _ _ _ _ _ _ _ _ hf ha⟩
    unfold acquire at ha
    obtain ⟨_, _, hh⟩ := Option.bind_eq_some_iff.mp ha
    split at hh
    · cases hh; exact issue_keeps_dead _ _ i hf hd
    · contradiction
  | close _ hc ih =>
    obtain ⟨hd, hf⟩ := ih
    refine ⟨?_, close_fresh _ _ _ _ hf hc⟩
    unfold close at hc
    obtain ⟨_, _, hh⟩ := Option.bind_eq_some_iff.mp hc
    cases hh
    exact consumed_keeps_dead _ _ _ hd

/-- Narrowing's result is exactly an intersection, with the same opaque resource identity. -/
theorem narrow_intersection (s t : State H) (i : GrantId) (requested : List Nat)
    (h : narrow s i requested = some t) : ∃ r,
      usable s i = some r ∧ AList.lookup t.resources ⟨s.next⟩ =
        some { r with rights := r.rights.filter (fun x => decide (x ∈ requested)) } := by
  unfold narrow at h
  obtain ⟨r, hr, hh⟩ := Option.bind_eq_some_iff.mp h
  cases hh
  exact ⟨r, hr, by simp [issue, consumed, AList.lookup]⟩

/-- Acquisition leaves its parent's resource, rights and availability exactly unchanged. -/
theorem acquire_keeps_parent (s t : State H) (i : GrantId) (requested selected : List Nat)
    (permissions : List Cap) (allowed : Bool) (handle : H) (hs : Fresh s)
    (h : acquire s i requested selected permissions allowed handle = some t) :
    usable t i = usable s i := by
  unfold acquire at h
  obtain ⟨r, hr, hh⟩ := Option.bind_eq_some_iff.mp h
  have hb := hs _ (AList.lookup_mem (usable_some s i r hr).1)
  have hi : GrantId.mk s.next ≠ i := by
    intro he; rw [← he] at hb; exact Nat.lt_irrefl _ hb
  split at hh
  · cases hh
    simp [usable, issue, AList.lookup, hi]
  · contradiction

end ConcreteSpec.Core.Grants
