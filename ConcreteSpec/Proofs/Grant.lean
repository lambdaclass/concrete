import ConcreteSpec.Spec.Grant
import ConcreteSpec.Proofs.Basics

/-! # C5 brand lifetime and grant attenuation laws -/
namespace ConcreteSpec.Core.Generative

theorem validate_iff (s t : State) : validate s = some t ↔ s = t ∧ WellFormed t := by
  unfold validate
  split
  · rename_i hs
    constructor
    · intro h; cases h; exact ⟨rfl, hs⟩
    · rintro ⟨rfl, _⟩; rfl
  · rename_i hs
    constructor
    · intro h; cases h
    · rintro ⟨rfl, ht⟩; exact False.elim (hs ht)

theorem fresh_preserves (s t : State) (scope : BlockId) (h : fresh s scope = some t) :
    WellFormed t := (validate_iff _ _).mp h |>.2

theorem depend_preserves (s t : State) (d : Dependency) (h : depend s d = some t) :
    WellFormed t := by
  unfold depend at h
  split at h
  · contradiction
  · exact (validate_iff _ _).mp h |>.2

theorem consume_preserves (s t : State) (p : Hybrid.Place) (h : consume s p = some t) :
    WellFormed t := by
  unfold consume at h
  split at h
  · exact (validate_iff _ _).mp h |>.2
  · contradiction

theorem release_preserves (s t : State) (i : BrandId) (h : release s i = some t) :
    WellFormed t := by
  unfold release at h
  split at h
  · exact (validate_iff _ _).mp h |>.2
  · contradiction

theorem reset_preserves (s t : State) (i : BrandId) (h : reset s i = some t) :
    WellFormed t := by
  unfold reset at h
  obtain ⟨_, _, hh⟩ := Option.bind_eq_some_iff.mp h
  obtain ⟨_, _, hh⟩ := Option.bind_eq_some_iff.mp hh
  exact (validate_iff _ _).mp hh |>.2

theorem endScope_preserves (s t : State) (scope : BlockId) (h : endScope s scope = some t) :
    WellFormed t := by
  unfold endScope at h
  split at h
  · exact (validate_iff _ _).mp h |>.2
  · contradiction

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
  obtain ⟨he, _⟩ := (validate_iff _ _).mp h
  subst t
  exact ⟨rfl, _, List.mem_cons_self, rfl⟩

theorem reset_generation (s t : State) (i : BrandId) (h : reset s i = some t) :
    t.nextBrand = s.nextBrand + 1 ∧ ∃ b ∈ t.binders, b.brand = ⟨s.nextBrand⟩ := by
  unfold reset at h
  obtain ⟨b, _, hh⟩ := Option.bind_eq_some_iff.mp h
  obtain ⟨u, _, hh⟩ := Option.bind_eq_some_iff.mp hh
  obtain ⟨he, _⟩ := (validate_iff _ _).mp hh
  subst t
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
    obtain ⟨he, _⟩ := (validate_iff _ _).mp h
    subst t
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

end ConcreteSpec.Core.Grants
