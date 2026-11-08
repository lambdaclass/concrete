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
  split at hh
  · contradiction
  · exact validate_wf hh

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

theorem adoptDown_preserves (s t : State) (args : List (Var × LoanId))
    (h : adoptDown s args = some t) : WellFormed t := by
  unfold adoptDown at h
  obtain ⟨_, _, hh⟩ := Option.bind_eq_some_iff.mp h
  split at hh
  · exact validate_wf hh
  · contradiction

theorem passDown_preserves (s t : CallState) (i : LoanId) (parameter : Var)
    (h : passDown s i parameter = some t) : WellFormed t.own := by
  unfold passDown at h
  obtain ⟨_, _, hh⟩ := Option.bind_eq_some_iff.mp h
  obtain ⟨_, _, hh⟩ := Option.bind_eq_some_iff.mp hh
  obtain ⟨own, ho, hh⟩ := Option.bind_eq_some_iff.mp hh
  cases hh
  exact validate_wf ho

/-- C1's actual call/lifecycle callback preserves ownership, for both owner and view parameters. -/
theorem callLife_preserves (types : Var → Option Descriptor) (e : Exec.Life)
    (s t : CallState) (h : callLife types e s = some t) : WellFormed t.own := by
  cases e with
  | enter params =>
    simp only [callLife] at h
    split at h
    · obtain ⟨u, _, hh⟩ := Option.bind_eq_some_iff.mp h
      obtain ⟨v, hv, hh⟩ := Option.bind_eq_some_iff.mp hh
      cases hh
      exact adoptDown_preserves u v s.pending hv
    · contradiction
  | bind x =>
    simp only [callLife] at h
    obtain ⟨own, ho, hh⟩ := Option.map_eq_some_iff.mp h
    subst t
    exact life_preserves types (.bind x) s.own own ho
  | leave =>
    simp only [callLife] at h
    obtain ⟨own, ho, hh⟩ := Option.map_eq_some_iff.mp h
    subst t
    exact life_preserves types .leave s.own own ho
  | drop xs =>
    simp only [callLife] at h
    obtain ⟨own, ho, hh⟩ := Option.map_eq_some_iff.mp h
    subst t
    exact life_preserves types (.drop xs) s.own own ho

/-- Passing loans changes only instrumentation, including at mixed-parameter call entry. -/
theorem callLife_erasure {σ : Type} (types : Var → Option Descriptor) (e : Exec.Life)
    (s t : σ × CallState) (h : callLifeStore types e s = some t) : t.1 = s.1 := by
  unfold callLifeStore at h
  obtain ⟨_, _, hh⟩ := Option.map_eq_some_iff.mp h
  subst t
  rfl

/-- Any successful instrumented operation preserves the invariant when its permission
transition does. Concrete values and concrete branch choices are irrelevant to this proof. -/
theorem instrument_preserves {σ α : Type} (permit : State → Option State)
    (hp : ∀ s t, permit s = some t → WellFormed t) (operation : σ → Option (α × σ))
    (s : Store σ) (a : α) (t : Store σ) (h : instrument permit operation s = some (a, t)) :
    WellFormed t.own := by
  unfold instrument at h
  obtain ⟨own, ho, hh⟩ := Option.bind_eq_some_iff.mp h
  obtain ⟨r, _, hh⟩ := Option.bind_eq_some_iff.mp hh
  cases r
  cases hh
  exact hp _ _ ho

theorem readOwner_erasure (tgt : Target) (p : Hybrid.Place) (s t : Store Hybrid.State)
    (v : Hybrid.Value) (h : readOwner tgt p s = some (v, t)) :
    s.concrete.read tgt p = some v ∧ t.concrete = s.concrete := by
  have he := instrument_erasure _ _ _ _ _ h
  dsimp [erase] at he
  cases hc : s.concrete.read tgt p with
  | none => simp [hc] at he
  | some w => simp [hc] at he; rcases he with ⟨rfl, hstore⟩; exact ⟨rfl, hstore.symm⟩

theorem writeOwner_erasure (tgt : Target) (p : Hybrid.Place) (s t : Store Hybrid.State)
    (v : Hybrid.Value) (h : writeOwner tgt p v s = some ((), t)) :
    s.concrete.write tgt p v = some t.concrete := by
  have he := instrument_erasure _ _ _ _ _ h
  dsimp [erase] at he
  cases hc : s.concrete.write tgt p v with
  | none => simp [hc] at he
  | some st => simp [hc] at he; rw [← he]

theorem readLoan_erasure (tgt : Target) (i : LoanId) (s t : Store Hybrid.State)
    (v : Hybrid.Value) (h : readLoan tgt i s = some (v, t)) :
    ∃ l, loan s.own i = some l ∧ l.path = [] ∧
      s.concrete.read tgt l.root = some v ∧ t.concrete = s.concrete := by
  unfold readLoan at h
  obtain ⟨l, hl, hh⟩ := Option.bind_eq_some_iff.mp h
  split at hh
  · rename_i hpath
    have he := instrument_erasure _ _ _ _ _ hh
    dsimp [erase] at he
    cases hc : s.concrete.read tgt l.root with
    | none => simp [hc] at he
    | some w => simp [hc] at he; rcases he with ⟨rfl, hstore⟩
                exact ⟨l, hl, hpath, hc, hstore.symm⟩
  · contradiction

theorem writeLoan_erasure (tgt : Target) (i : LoanId) (s t : Store Hybrid.State)
    (v : Hybrid.Value) (h : writeLoan tgt i v s = some ((), t)) :
    ∃ l, loan s.own i = some l ∧ l.path = [] ∧
      s.concrete.write tgt l.root v = some t.concrete := by
  unfold writeLoan at h
  obtain ⟨l, hl, hh⟩ := Option.bind_eq_some_iff.mp h
  split at hh
  · rename_i hpath
    have he := instrument_erasure _ _ _ _ _ hh
    dsimp [erase] at he
    cases hc : s.concrete.write tgt l.root v with
    | none => simp [hc] at he
    | some st => simp [hc] at he; exact ⟨l, hl, hpath, he ▸ hc⟩
  · contradiction

/-- The next identity has never belonged to any currently active loan. Combined with the
unchanged/incremented supply laws, ending a loan cannot make its identity fresh again. -/
theorem nextLoan_fresh (s : State) (hs : WellFormed s) (l : Loan) (hl : l ∈ s.loans) :
    l.id ≠ LoanId.mk s.nextLoan := by
  intro he
  have hb := (hs.2.2.2.1 l hl).1
  rw [he] at hb
  exact Nat.lt_irrefl _ hb

/-- A successful borrow issues the old supply and advances it exactly once. -/
theorem borrow_birth (s t : State) (p : Hybrid.Place) (path : List Projection)
    (m : Mode) (h : borrow s p path m = some t) :
    t.nextLoan = s.nextLoan + 1 ∧
      ∃ l ∈ t.loans, l.id = ⟨s.nextLoan⟩ ∧ l.root = p ∧ l.path = path := by
  unfold borrow at h
  obtain ⟨f, _, hh⟩ := Option.bind_eq_some_iff.mp h
  split at hh
  · obtain ⟨he, _⟩ := (validate_iff _ _).mp hh
    subst t
    exact ⟨rfl, _, List.mem_cons_self, rfl, rfl, rfl⟩
  · contradiction

/-- A reborrow keeps the original root and extends the parent's projection. -/
theorem reborrow_birth (s t : State) (i : LoanId) (suffix : List Projection)
    (m : Mode) (activation : Nat) (h : reborrowAt s i suffix m activation = some t) :
    t.nextLoan = s.nextLoan + 1 ∧ ∃ a l, loan s i = some a ∧ l ∈ t.loans ∧
      l.id = ⟨s.nextLoan⟩ ∧ l.root = a.root ∧ l.path = a.path ++ suffix ∧ l.parent = some i := by
  unfold reborrowAt at h
  obtain ⟨a, ha, hh⟩ := Option.bind_eq_some_iff.mp h
  split at hh
  · obtain ⟨he, _⟩ := (validate_iff _ _).mp hh
    subst t
    exact ⟨rfl, a, _, ha, List.mem_cons_self, rfl, rfl, rfl, rfl⟩
  · contradiction

private theorem endLoan_frames (s t : State) (i : LoanId) (h : endLoan s i = some t) :
    t.frames = s.frames := by
  unfold endLoan at h
  obtain ⟨_, _, hh⟩ := Option.bind_eq_some_iff.mp h
  split at hh
  · contradiction
  · obtain ⟨he, _⟩ := (validate_iff _ _).mp hh
    subst t
    rfl

/-- Ending any loan leaves the freshness supply unchanged; an ended identity is not recycled. -/
theorem endLoan_supply (s t : State) (i : LoanId) (h : endLoan s i = some t) :
    t.nextLoan = s.nextLoan := by
  unfold endLoan at h
  obtain ⟨_, _, hh⟩ := Option.bind_eq_some_iff.mp h
  split at hh
  · contradiction
  · obtain ⟨he, _⟩ := (validate_iff _ _).mp hh
    subst t
    rfl

private theorem fold_frames {α : Type} (f : State → α → Option State)
    (hf : ∀ s a t, f s a = some t → t.frames = s.frames) (xs : List α)
    (s t : State) (h : xs.foldlM f s = some t) : t.frames = s.frames := by
  induction xs generalizing s with
  | nil => simp at h; subst t; rfl
  | cons a xs ih =>
    simp only [List.foldlM_cons] at h
    obtain ⟨u, hu, hh⟩ := Option.bind_eq_some_iff.mp h
    exact (ih u hh).trans (hf s a u hu)

private theorem drop_absent (s t : State) (xs : List Var) (f : Activation)
    (hf : s.frames.head? = some f) (h : drop xs s = some t)
    (p : Hybrid.Place) (hp : p ∈ List.map Prod.snd (f.locals.filter fun e => decide (e.1 ∈ xs))) :
    ∀ x ∈ t.slots, x.place ≠ p := by
  unfold drop at h
  rw [hf] at h
  dsimp at h
  split at h
  · obtain ⟨he, _⟩ := (validate_iff _ _).mp h
    subst t
    intro x hx he
    simp only [List.mem_filter, decide_eq_true_eq] at hx
    exact hx.2 (he ▸ hp)
  · contradiction

/-- On leaving a call, no active loan remains rooted at any of that callee's locals.
Outer owners passed downward are not locals; their parent loans remain usable after return. -/
theorem leave_no_local_loan (s t : State) (f : Activation)
    (hf : s.frames.head? = some f) (h : leave s = some t)
    (entry : Var × Hybrid.Place) (he : entry ∈ f.locals)
    (l : Loan) (hl : l ∈ t.loans) : l.root ≠ entry.2 := by
  have ht := leave_preserves s t h
  unfold leave at h
  rw [hf] at h
  dsimp at h
  obtain ⟨u, hu, hh⟩ := Option.bind_eq_some_iff.mp h
  have hframes := fold_frames _ (fun s l t hh => endLoan_frames s t l.id hh) _ s u hu
  obtain ⟨v, hv, hh⟩ := Option.bind_eq_some_iff.mp hh
  split at hh
  · contradiction
  rename_i _
  obtain ⟨heq, _⟩ := (validate_iff _ _).mp hh
  have hp : entry.2 ∈ List.map Prod.snd (f.locals.filter fun e =>
      decide (e.1 ∈ List.map Prod.fst f.locals)) := by
    apply List.mem_map.mpr
    exact ⟨entry, List.mem_filter.mpr ⟨he, by simp; exact ⟨entry.2, he⟩⟩, rfl⟩
  have habsent := drop_absent u v _ f (hframes ▸ hf) hv entry.2 hp
  intro hroot
  obtain ⟨x, hx, hxroot, _⟩ := loan_has_owned_root t ht l hl
  have hxv : x ∈ v.slots := by rw [← heq] at hx; exact hx
  exact habsent x hxv (hxroot.trans hroot)

end ConcreteSpec.Core.Own
