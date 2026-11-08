import ConcreteSpec.Spec.Own

/-! # C5 preservation and erasure
The executable model checks a finite invariant on candidates. These laws prove successful
transitions preserve it and expose the concrete operation unchanged. They do not claim that
an arbitrary checker/elaborator enforces these transitions; that is C9's correspondence duty.
-/
namespace ConcreteSpec.Core.Own

/-- Validation returns exactly its candidate, never a repaired state. -/
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

theorem validate_wf {s t : State} (h : validate s = some t) : WellFormed t :=
  (validate_iff s t).mp h |>.2

private theorem fold_preserves {α : Type} (f : State → α → Option State)
    (hf : ∀ s a t, f s a = some t → WellFormed t) (xs : List α)
    (s t : State) (hs : WellFormed s) (h : xs.foldlM f s = some t) : WellFormed t := by
  induction xs generalizing s with
  | nil => simp at h; subst t; exact hs
  | cons a xs ih =>
    simp only [List.foldlM_cons] at h
    cases he : f s a with
    | none => simp [he] at h
    | some u => simp only [he] at h <;> dsimp at h; exact ih u (hf s a u he) h

/-- Every primitive transition ends at the same finite invariant check. -/
theorem move_preserves (s t : State) (p : Hybrid.Place) (h : move s p = some t) :
    WellFormed t := by
  unfold move at h
  cases he : slot s p <;> simp only [he] at h <;> dsimp at h
  · contradiction
  · split at h
    · contradiction
    · exact validate_wf h

theorem reinitialize_preserves (s t : State) (p : Hybrid.Place)
    (h : reinitialize s p = some t) : WellFormed t := by
  unfold reinitialize at h
  cases he : slot s p <;> simp only [he] at h <;> dsimp at h
  · contradiction
  · split at h
    · exact validate_wf h
    · contradiction

theorem borrow_preserves (s t : State) (p : Hybrid.Place) (path : List Projection)
    (m : Mode) (h : borrow s p path m = some t) : WellFormed t := by
  unfold borrow at h
  cases he : s.frames.head? <;> simp only [he] at h <;> dsimp at h
  · contradiction
  · split at h
    · exact validate_wf h
    · contradiction

theorem reborrowAt_preserves (s t : State) (i : LoanId) (path : List Projection)
    (m : Mode) (a : Nat) (h : reborrowAt s i path m a = some t) : WellFormed t := by
  unfold reborrowAt at h
  cases he : loan s i <;> simp only [he] at h <;> dsimp at h
  · contradiction
  · split at h
    · exact validate_wf h
    · contradiction

theorem reborrow_preserves (s t : State) (i : LoanId) (path : List Projection)
    (m : Mode) (h : reborrow s i path m = some t) : WellFormed t := by
  unfold reborrow at h
  cases he : s.frames.head? <;> simp only [he] at h <;> dsimp at h
  · contradiction
  · exact reborrowAt_preserves _ _ _ _ _ _ h

theorem endLoan_preserves (s t : State) (i : LoanId) (h : endLoan s i = some t) :
    WellFormed t := by
  unfold endLoan at h
  cases he : loan s i <;> simp only [he] at h <;> dsimp at h
  · contradiction
  · split at h
    · contradiction
    · exact validate_wf h

theorem accessOwner_preserves (s t : State) (p : Hybrid.Place) (m : Mode)
    (h : accessOwner s p m = some t) : WellFormed t := by
  unfold accessOwner at h
  split at h
  · exact validate_wf h
  · contradiction

theorem accessLoan_preserves (s t : State) (i : LoanId) (m : Mode)
    (h : accessLoan s i m = some t) : WellFormed t := by
  unfold accessLoan at h
  split at h
  · exact validate_wf h
  · contradiction

theorem bind_preserves (types : Var → Option Descriptor) (s t : State) (x : Var)
    (h : bind types x s = some t) : WellFormed t := by
  unfold bind at h
  cases he : s.frames.head? <;> simp only [he] at h <;> dsimp at h
  · contradiction
  · split at h
    · exact reinitialize_preserves _ _ _ h
    · cases hd : types x <;> simp only [hd] at h <;> dsimp at h
      · contradiction
      · exact validate_wf h

theorem drop_preserves (s t : State) (xs : List Var) (h : drop xs s = some t) :
    WellFormed t := by
  unfold drop at h
  cases he : s.frames.head? <;> simp only [he] at h <;> dsimp at h
  · contradiction
  · split at h
    · exact validate_wf h
    · contradiction

theorem enter_preserves (types : Var → Option Descriptor) (s t : State) (xs : List Var)
    (h : enter types xs s = some t) : WellFormed t := by
  unfold enter at h
  obtain ⟨u, hu, hh⟩ := Option.bind_eq_some_iff.mp h
  exact fold_preserves _ (fun s x t h => bind_preserves types s t x h) xs u t
    (validate_wf hu) hh

theorem endLocals_preserves (s t : State) (a : Nat) (hs : WellFormed s)
    (h : endLocals a s = some t) : WellFormed t :=
  fold_preserves _ (fun s l t h => endLoan_preserves s t l.id h) _ _ _ hs h

theorem leave_preserves (s t : State) (h : leave s = some t) : WellFormed t := by
  unfold leave at h
  obtain ⟨f, _, hh⟩ := Option.bind_eq_some_iff.mp h
  obtain ⟨u, _, hh⟩ := Option.bind_eq_some_iff.mp hh
  obtain ⟨v, _, hh⟩ := Option.bind_eq_some_iff.mp hh
  exact validate_wf hh

theorem enterDown_preserves (s t : State) (args : List (Var × LoanId))
    (h : enterDown s args = some t) : WellFormed t := by
  unfold enterDown at h
  obtain ⟨u, hu, hh⟩ := Option.bind_eq_some_iff.mp h
  refine fold_preserves _ ?_ args u t (enter_preserves _ _ _ _ hu) hh
  intro s arg t hh
  obtain ⟨a, _, hh⟩ := Option.bind_eq_some_iff.mp hh
  obtain ⟨f, _, hh⟩ := Option.bind_eq_some_iff.mp hh
  obtain ⟨v, _, hh⟩ := Option.bind_eq_some_iff.mp hh
  exact validate_wf hh

theorem life_preserves (types : Var → Option Descriptor) (e : Exec.Life) (s t : State)
    (h : life types e s = some t) : WellFormed t := by
  cases e with
  | bind x => exact bind_preserves types s t x h
  | enter ps => exact enter_preserves types s t ps h
  | leave => exact leave_preserves s t h
  | drop xs => exact drop_preserves s t xs h

/-- No active loan can outlive its original owned root, including across calls. -/
theorem loan_has_owned_root (s : State) (hs : WellFormed s) (l : Loan) (hl : l ∈ s.loans) :
    ∃ x ∈ s.slots, x.place = l.root ∧ x.status = .owned := (hs.2.2.2.1 l hl).2.2.1

private theorem slot_unique (xs : List Slot) (hn : (xs.map Slot.place).Nodup)
    (x y : Slot) (hx : x ∈ xs) (hy : y ∈ xs) (he : x.place = y.place) : x = y := by
  induction xs with
  | nil => simp at hx
  | cons z zs ih =>
    simp only [List.map_cons, List.nodup_cons] at hn
    simp only [List.mem_cons] at hx hy
    rcases hx with rfl | hx
    · rcases hy with rfl | hy
      · rfl
      · exact False.elim (hn.1 (List.mem_map.mpr ⟨y, hy, he.symm⟩))
    · rcases hy with rfl | hy
      · exact False.elim (hn.1 (List.mem_map.mpr ⟨x, hx, he⟩))
      · exact ih hn.2 hx hy

/-- A moved slot cannot have an active loan, since place identities are unique. -/
theorem moved_no_loan (s : State) (hs : WellFormed s) (x : Slot) (hx : x ∈ s.slots)
    (hm : x.status = .moved) (l : Loan) (hl : l ∈ s.loans) : x.place ≠ l.root := by
  intro he
  obtain ⟨y, hy, hp, ho⟩ := loan_has_owned_root s hs l hl
  have hxy : x = y := slot_unique s.slots hs.1 x y hx hy (he.trans hp.symm)
  subst y
  rw [hm] at ho
  cases ho

/-- Success projects to precisely the uninstrumented result and concrete store. This covers
moves, permissions, lifecycle and concrete reads/writes by choosing their permission transition. -/
theorem instrument_erasure {σ α : Type} (permit : State → Option State)
    (operation : σ → Option (α × σ)) (s : Store σ) (a : α) (t : Store σ)
    (h : instrument permit operation s = some (a, t)) :
    operation (erase s) = some (a, erase t) := by
  unfold instrument at h
  cases hp : permit s.own <;> simp only [hp] at h <;> dsimp at h
  · contradiction
  · cases hc : operation s.concrete with
    | none => simp [hc] at h
    | some r => cases r; simp [hc] at h
                rcases h with ⟨rfl, rfl⟩; exact hc

theorem life_erasure {σ : Type} (types : Var → Option Descriptor) (e : Exec.Life)
    (s t : Store σ) (h : lifeStore types e s = some t) : erase t = erase s := by
  unfold lifeStore at h
  cases he : life types e s.own <;> simp [he] at h
  subst t
  rfl

end ConcreteSpec.Core.Own
