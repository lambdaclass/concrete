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

/-- Move changes only the selected slot's status, after excluding every loan of its root. -/
theorem move_preserves (s t : State) (p : Hybrid.Place) (hs : WellFormed s)
    (h : move s p = some t) : WellFormed t := by
  unfold move at h
  obtain ⟨x, _, h⟩ := Option.bind_eq_some_iff.mp h
  split at h
  · contradiction
  · rename_i hg
    cases h
    let change := fun x : Slot => if x.place = p then { x with status := .moved } else x
    have place (x : Slot) : (change x).place = x.place := by simp [change]; split <;> rfl
    have activation (x : Slot) : (change x).activation = x.activation := by simp [change]; split <;> rfl
    have clear : ∀ l ∈ s.loans, l.root ≠ p := by
      intro l hl he
      apply hg
      simp only [Bool.or_eq_true, List.any_eq_true]
      exact Or.inr ⟨l, hl, by simp [he]⟩
    refine ⟨?_, hs.2.1, ?_, ?_, hs.2.2.2.2⟩
    · change (s.slots.map change |>.map Slot.place).Nodup
      simpa only [List.map_map, Function.comp_def, place] using hs.1
    · intro a ha
      obtain ⟨b, hb, rfl⟩ := List.mem_map.mp ha
      change (change b).place.id < s.nextPlace ∧ (change b).activation < s.nextActivation
      simpa only [place, activation] using hs.2.2.1 b hb
    · intro l hl
      obtain ⟨hid, hact, ⟨a, ha, he, ho⟩, hp, hpend⟩ := hs.2.2.2.1 l hl
      refine ⟨hid, hact, ⟨change a, List.mem_map.mpr ⟨a, ha, rfl⟩, (place a).trans he, ?_⟩, hp, hpend⟩
      have hn : a.place ≠ p := fun h => clear l hl (he.symm.trans h)
      simpa [change, hn] using ho

theorem reinitialize_preserves (s t : State) (p : Hybrid.Place)
    (h : reinitialize s p = some t) : WellFormed t := by
  unfold reinitialize at h
  cases he : slot s p <;> simp only [he] at h <;> dsimp at h
  · contradiction
  · split at h
    · exact validate_wf h
    · contradiction

private theorem compatible_symm (a b : Loan) (h : Compatible a b) : Compatible b a := by
  unfold Compatible at *
  rcases h with h | h | h | h | h
  · exact Or.inl (Ne.symm h)
  · exact Or.inr (Or.inl h.symm)
  · exact Or.inr (Or.inr (Or.inr (Or.inl h)))
  · exact Or.inr (Or.inr (Or.inl h))
  · exact Or.inr (Or.inr (Or.inr (Or.inr h.symm)))

private theorem insert_loan (s : State) (a : Loan) (hs : WellFormed s)
    (hid : a.id = ⟨s.nextLoan⟩) (hact : a.activation < s.nextActivation)
    (hroot : ∃ x ∈ s.slots, x.place = a.root ∧ x.status = .owned)
    (hparent : ParentValid s a) (hpending : a.pendingArgument = true → a.parent.isSome = true)
    (hcompat : ∀ b ∈ s.loans, Compatible a b) :
    WellFormed { s with loans := a :: s.loans, nextLoan := s.nextLoan + 1 } := by
  have fresh : a.id ∉ s.loans.map Loan.id := by
    intro hm
    obtain ⟨b, hb, he⟩ := List.mem_map.mp hm
    have hn := (hs.2.2.2.1 b hb).1
    rw [he, hid] at hn
    exact Nat.lt_irrefl _ hn
  have parent (l : Loan) (hl : ParentValid s l) :
      ParentValid { s with loans := a :: s.loans, nextLoan := s.nextLoan + 1 } l := by
    unfold ParentValid at hl ⊢
    cases he : l.parent with
    | none => simpa only [he] using hl
    | some p =>
      simp only [he] at hl ⊢
      obtain ⟨b, hb, rest⟩ := hl
      exact ⟨b, List.mem_cons_of_mem _ hb, rest⟩
  refine ⟨hs.1, ?_, hs.2.2.1, ?_, ?_⟩
  · simpa only [List.map_cons, List.nodup_cons] using And.intro fresh hs.2.1
  · intro l hl
    rcases List.mem_cons.mp hl with he | hl
    · subst l
      refine ⟨?_, hact, hroot, parent a hparent, hpending⟩
      simp [hid]
    · obtain ⟨hb, ha, hr, hp, hpend⟩ := hs.2.2.2.1 l hl
      exact ⟨Nat.lt_succ_of_lt hb, ha, hr, parent l hp, hpend⟩
  · intro b hb c hc
    change b ∈ a :: s.loans at hb
    change c ∈ a :: s.loans at hc
    rcases List.mem_cons.mp hb with he | hb
    · subst b
      rcases List.mem_cons.mp hc with he | hc
      · subst c; exact Or.inr (Or.inl rfl)
      · exact hcompat c hc
    · rcases List.mem_cons.mp hc with he | hc
      · subst c; exact compatible_symm a b (hcompat b hb)
      · exact hs.2.2.2.2 b hb c hc

private theorem owner_guard (s : State) (p : Hybrid.Place) (m : Mode)
    (h : ownerAccess s p m = true) :
    (∃ x ∈ s.slots, x.place = p ∧ x.status = .owned) ∧
    ∀ l ∈ s.loans, l.root ≠ p ∨ (m = .shared ∧ l.mode = .shared) := by
  unfold ownerAccess at h
  cases hx : slot s p with
  | none => simp [hx] at h
  | some x =>
    simp only [hx, Option.any_some] at h
    have hm : x ∈ s.slots := List.mem_of_find?_eq_some hx
    have hp : x.place = p := by simpa using List.find?_some hx
    simp only [Bool.and_eq_true, beq_iff_eq] at h
    refine ⟨⟨x, hm, hp, h.1.1⟩, ?_⟩
    intro l hl
    have := List.all_eq_true.mp h.2 l hl
    simpa only [Bool.or_eq_true, bne_iff_ne, Bool.and_eq_true, beq_iff_eq] using this

theorem borrow_preserves (s t : State) (p : Hybrid.Place) (path : List Projection)
    (m : Mode) (hs : WellFormed s) (h : borrow s p path m = some t) : WellFormed t := by
  unfold borrow at h
  obtain ⟨f, _, h⟩ := Option.bind_eq_some_iff.mp h
  split at h
  · rename_i hg
    cases h
    obtain ⟨hg, hact⟩ := Bool.and_eq_true_iff.mp hg
    obtain ⟨hroot, hconf⟩ := owner_guard s p m hg
    apply insert_loan s ⟨⟨s.nextLoan⟩, p, path, m, none, [], f.id, false⟩ hs rfl
      (of_decide_eq_true hact) hroot (by rfl) (by simp)
    intro b hb
    rcases hconf b hb with hn | hm
    · exact Or.inl (Ne.symm hn)
    · exact Or.inr (Or.inr (Or.inr (Or.inr hm)))
  · contradiction

theorem reborrowAt_preserves (s t : State) (i : LoanId) (path : List Projection)
    (m : Mode) (activation : Nat) (hs : WellFormed s)
    (h : reborrowAt s i path m activation = some t) : WellFormed t := by
  unfold reborrowAt at h
  obtain ⟨a, ha, h⟩ := Option.bind_eq_some_iff.mp h
  have hmem : a ∈ s.loans := List.mem_of_find?_eq_some ha
  have hi : a.id = i := by simpa using List.find?_some ha
  split at h
  · rename_i hg
    cases h
    obtain ⟨hg, hact⟩ := Bool.and_eq_true_iff.mp hg
    unfold loanAccess at hg
    rw [ha] at hg
    simp only [Option.any_some, Bool.and_eq_true, Bool.not_eq_true', Bool.or_eq_true,
      beq_iff_eq] at hg
    obtain ⟨⟨_, hmode⟩, hconf⟩ := hg
    have upgrade : a.mode = .shared → m = .shared := by
      intro he
      rcases hmode with hm | hm
      · exact hm
      · simp [he] at hm
    apply insert_loan s ⟨⟨s.nextLoan⟩, a.root, a.path ++ path, m, some i,
      i :: a.ancestors, activation, false⟩ hs rfl (of_decide_eq_true hact)
      (hs.2.2.2.1 a hmem).2.2.1 ?_ (by simp) ?_
    · unfold ParentValid
      exact ⟨a, hmem, hi, rfl, by simpa [hi] using (hs.2.2.2.1 a hmem).1,
        by simp [hi], upgrade, by simp⟩
    · intro b hb
      have hc := List.all_eq_true.mp hconf b hb
      simp only [Bool.or_eq_true, beq_iff_eq, decide_eq_true_eq, bne_iff_ne,
        Bool.and_eq_true] at hc
      rcases hc with ((he | he) | he) | he
      · exact Or.inr (Or.inr (Or.inr (Or.inl (by simp [he]))))
      · exact Or.inr (Or.inr (Or.inr (Or.inl (List.mem_cons_of_mem _ he))))
      · exact Or.inl (Ne.symm he)
      · exact Or.inr (Or.inr (Or.inr (Or.inr he)))
  · contradiction

theorem reborrow_preserves (s t : State) (i : LoanId) (path : List Projection)
    (m : Mode) (hs : WellFormed s) (h : reborrow s i path m = some t) : WellFormed t := by
  unfold reborrow at h
  obtain ⟨f, _, h⟩ := Option.bind_eq_some_iff.mp h
  exact reborrowAt_preserves s t i path m f.id hs h

theorem endLoan_preserves (s t : State) (i : LoanId) (h : endLoan s i = some t) :
    WellFormed t := by
  unfold endLoan at h
  cases he : loan s i <;> simp only [he] at h <;> dsimp at h
  · contradiction
  · split at h
    · contradiction
    · exact validate_wf h

theorem accessOwner_preserves (s t : State) (p : Hybrid.Place) (m : Mode)
    (hs : WellFormed s) (h : accessOwner s p m = some t) : WellFormed t := by
  unfold accessOwner at h
  split at h
  · cases h; exact hs
  · contradiction

theorem accessLoan_preserves (s t : State) (i : LoanId) (m : Mode)
    (hs : WellFormed s) (h : accessLoan s i m = some t) : WellFormed t := by
  unfold accessLoan at h
  split at h
  · cases h; exact hs
  · contradiction

/-- A movable non-Copy slot with no outstanding loan always moves; unrelated invariant
checks cannot make this transition stuck. -/
theorem move_progress (s : State) (p : Hybrid.Place) (x : Slot)
    (found : slot s p = some x) (linear : x.desc.copy = false)
    (owned : x.status = .owned) (clear : ∀ l ∈ s.loans, l.root ≠ p) :
    ∃ t, move s p = some t := by
  have hc : s.loans.any (·.root == p) = false := by
    apply Bool.eq_false_iff.mpr
    intro hg
    obtain ⟨l, hl, he⟩ := List.any_eq_true.mp hg
    exact clear l hl (by simpa using he)
  simp [move, found, linear, owned, hc]

theorem borrow_progress (s : State) (p : Hybrid.Place) (path : List Projection) (m : Mode)
    (f : Activation) (frame : s.frames.head? = some f)
    (issued : f.id < s.nextActivation) (allowed : ownerAccess s p m = true) :
    ∃ t, borrow s p path m = some t := by
  simp [borrow, frame, issued, allowed]

theorem reborrowAt_progress (s : State) (i : LoanId) (path : List Projection) (m : Mode)
    (activation : Nat) (a : Loan) (found : loan s i = some a)
    (issued : activation < s.nextActivation) (allowed : loanAccess s i m = true) :
    ∃ t, reborrowAt s i path m activation = some t := by
  simp [reborrowAt, found, issued, allowed]

theorem reborrow_progress (s : State) (i : LoanId) (path : List Projection) (m : Mode)
    (f : Activation) (frame : s.frames.head? = some f) (issued : f.id < s.nextActivation)
    (a : Loan) (found : loan s i = some a) (allowed : loanAccess s i m = true) :
    ∃ t, reborrow s i path m = some t := by
  simpa [reborrow, frame] using
    reborrowAt_progress s i path m f.id a found issued allowed

theorem accessOwner_progress (s : State) (p : Hybrid.Place) (m : Mode)
    (allowed : ownerAccess s p m = true) : accessOwner s p m = some s := by
  simp [accessOwner, allowed]

theorem accessLoan_progress (s : State) (i : LoanId) (m : Mode)
    (allowed : loanAccess s i m = true) : accessLoan s i m = some s := by
  simp [accessLoan, allowed]

namespace PrimitiveWitnesses
open ConcreteSpec.Core.Own.Examples
/-- Positive preservation/progress instances use live, nonempty owner/loan states. -/
example : WellFormed initial ∧ (move initial ⟨0⟩).isSome = true ∧
    WellFormed ((move initial ⟨0⟩).getD initial) := by decide
example : WellFormed shared ∧ (borrow shared ⟨0⟩ [] .shared).isSome = true ∧
    WellFormed ((borrow shared ⟨0⟩ [] .shared).getD initial) := by decide
example : WellFormed exclusive ∧ (reborrowAt exclusive ⟨0⟩ [] .mutable 0).isSome = true ∧
    WellFormed child := by decide
example : accessOwner shared ⟨0⟩ .shared = some shared ∧ WellFormed shared := by decide
example : accessLoan child ⟨1⟩ .mutable = some child ∧ WellFormed child := by decide
/-- Planted move/access mutations destroy an owned root under a live loan. -/
example : ¬ WellFormed { shared with slots := shared.slots.map fun x =>
    { x with status := .moved } } := by decide
example : ¬ WellFormed { child with slots := [] } := by decide
/-- Planted borrow mutation reuses the supply; planted reborrow mutation loses its parent. -/
example : ¬ WellFormed { shared with nextLoan := 0 } := by decide
example : ¬ WellFormed { child with loans := child.loans.filter (·.id != ⟨0⟩) } := by decide
/-- Negated success with each operational permission violated. -/
example : ¬ (move shared ⟨0⟩).isSome = true := by decide
example : ¬ (borrow exclusive ⟨0⟩ [] .shared).isSome = true := by decide
example : ¬ (reborrowAt shared ⟨0⟩ [] .mutable 0).isSome = true := by decide
example : ¬ (reborrow shared ⟨0⟩ [] .mutable).isSome = true := by decide
example : ¬ (accessOwner shared ⟨0⟩ .mutable).isSome = true := by decide
example : ¬ (accessLoan child ⟨0⟩ .mutable).isSome = true := by decide
end PrimitiveWitnesses

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
  split at h
  · contradiction
  · next =>
    obtain ⟨_, _, hh⟩ := Option.bind_eq_some_iff.mp h
    obtain ⟨_, _, hh⟩ := Option.bind_eq_some_iff.mp hh
    obtain ⟨own, ho, hh⟩ := Option.bind_eq_some_iff.mp hh
    cases hh
    exact validate_wf ho


/-- Pending metadata stays synchronized when argument evaluation adds a fresh child. -/
theorem passDown_call_preserves (s t : CallState) (i : LoanId) (parameter : Var)
    (hs : CallState.WellFormed s) (h : passDown s i parameter = some t) :
    CallState.WellFormed t := by
  have hw := passDown_preserves s t i parameter h
  unfold passDown at h
  split at h
  · contradiction
  · rename_i unique
    obtain ⟨a, ha, hh⟩ := Option.bind_eq_some_iff.mp h
    obtain ⟨u, hu, hh⟩ := Option.bind_eq_some_iff.mp hh
    obtain ⟨v, hv, hh⟩ := Option.bind_eq_some_iff.mp hh
    cases hh
    obtain ⟨hv, _⟩ := (validate_iff _ _).mp hv
    subst v
    unfold reborrow at hu
    obtain ⟨f, hf, hu⟩ := Option.bind_eq_some_iff.mp hu
    unfold reborrowAt at hu
    rw [ha] at hu
    dsimp at hu
    split at hu
    · cases hu
      have fresh : ∀ l ∈ s.own.loans, l.id.id ≠ s.own.nextLoan := by
        intro l hl he
        have := (hs.1.2.2.2.1 l hl).1
        omega
      have pendingFresh : LoanId.mk s.own.nextLoan ∉ s.pending.map Prod.snd := by
        intro hm
        obtain ⟨e, he, hid⟩ := List.mem_map.mp hm
        obtain ⟨l, hl, hh⟩ := hs.2.2.2.1 e he
        exact fresh l hl (congrArg LoanId.id (hh.trans hid))
      refine ⟨hw, ?_, ?_, ?_, ?_⟩
      · simp only [List.map_append, List.map_cons, List.map_nil, List.nodup_append]
        refine ⟨hs.2.1, by simp, ?_⟩
        intro a ha b hb
        simp only [List.mem_singleton] at hb
        subst b
        intro he
        exact unique (he ▸ ha)
      · simp only [List.map_append, List.map_cons, List.map_nil, List.nodup_append]
        refine ⟨hs.2.2.1, by simp, ?_⟩
        intro a ha b hb
        simp only [List.mem_singleton] at hb
        subst b
        intro he
        exact pendingFresh (he ▸ ha)
      · intro e he
        rcases List.mem_append.mp he with he | he
        · obtain ⟨l, hl, hid⟩ := hs.2.2.2.1 e he
          refine ⟨l, ?_, hid⟩
          apply List.mem_map.mpr
          exact ⟨l, List.mem_cons_of_mem _ hl, by simp [fresh l hl]⟩
        · simp only [List.mem_singleton] at he
          subst e
          refine ⟨_, List.mem_map.mpr ⟨_, List.mem_cons_self, rfl⟩, ?_⟩
          simp
      · intro l hl
        obtain ⟨b, hb, rfl⟩ := List.mem_map.mp hl
        rcases List.mem_cons.mp hb with rfl | hb
        · simp
        · simpa [fresh b hb, List.map_append, pendingFresh,
            (show b.id ≠ LoanId.mk s.own.nextLoan from fun he => fresh b hb (congrArg LoanId.id he))]
            using hs.2.2.2.2 b hb
    · contradiction

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
    split at h
    · obtain ⟨own, ho, hh⟩ := Option.map_eq_some_iff.mp h
      subst t
      exact life_preserves types (.bind x) s.own own ho
    · contradiction
  | leave =>
    simp only [callLife] at h
    split at h
    · obtain ⟨own, ho, hh⟩ := Option.map_eq_some_iff.mp h
      subst t
      exact life_preserves types .leave s.own own ho
    · contradiction
  | drop xs =>
    simp only [callLife] at h
    split at h
    · obtain ⟨own, ho, hh⟩ := Option.map_eq_some_iff.mp h
      subst t
      exact life_preserves types (.drop xs) s.own own ho
    · contradiction

private theorem reinitialize_loans (s t : State) (p : Hybrid.Place)
    (h : reinitialize s p = some t) : t.loans = s.loans := by
  unfold reinitialize at h
  obtain ⟨x, _, h⟩ := Option.bind_eq_some_iff.mp h
  split at h
  · obtain ⟨h, _⟩ := (validate_iff _ _).mp h
    cases h; rfl
  · contradiction

private theorem bind_loans (types : Var → Option Descriptor) (s t : State) (x : Var)
    (h : bind types x s = some t) : t.loans = s.loans := by
  unfold bind at h
  obtain ⟨f, _, h⟩ := Option.bind_eq_some_iff.mp h
  split at h
  · exact reinitialize_loans _ _ _ h
  · obtain ⟨d, _, h⟩ := Option.bind_eq_some_iff.mp h
    obtain ⟨h, _⟩ := (validate_iff _ _).mp h
    cases h; rfl

private theorem fold_loans {α : Type} (f : State → α → Option State)
    (hf : ∀ s a t, f s a = some t → ∀ l ∈ t.loans, l ∈ s.loans)
    (xs : List α) (s t : State) (h : xs.foldlM f s = some t) :
    ∀ l ∈ t.loans, l ∈ s.loans := by
  induction xs generalizing s with
  | nil => simp at h; subst t; exact fun _ h => h
  | cons a xs ih =>
    simp only [List.foldlM_cons] at h
    obtain ⟨u, hu, h⟩ := Option.bind_eq_some_iff.mp h
    exact fun l hl => hf s a u hu l (ih u h l hl)

private theorem enter_loans (types : Var → Option Descriptor) (s t : State) (xs : List Var)
    (h : enter types xs s = some t) : t.loans = s.loans := by
  unfold enter at h
  obtain ⟨u, hu, h⟩ := Option.bind_eq_some_iff.mp h
  obtain ⟨hu, _⟩ := (validate_iff _ _).mp hu
  subst u
  suffices ∀ (xs : List Var) (u v : State),
      xs.foldlM (fun s x => bind types x s) u = some v → v.loans = u.loans by
    exact this xs { s with
      nextActivation := s.nextActivation + 1
      frames := ⟨s.nextActivation, [], []⟩ :: s.frames } t h
  intro xs
  induction xs with
  | nil => intro u v h; simp at h; subst v; rfl
  | cons x xs ih =>
    intro u v h
    simp only [List.foldlM_cons] at h
    obtain ⟨w, hw, h⟩ := Option.bind_eq_some_iff.mp h
    exact (ih w v h).trans (bind_loans types u w x hw)

private theorem drop_loans (s t : State) (xs : List Var) (h : drop xs s = some t) :
    t.loans = s.loans := by
  unfold drop at h
  obtain ⟨f, _, h⟩ := Option.bind_eq_some_iff.mp h
  dsimp at h
  split at h
  · obtain ⟨h, _⟩ := (validate_iff _ _).mp h
    cases h; rfl
  · contradiction

private theorem endLoan_subset (s t : State) (i : LoanId) (h : endLoan s i = some t) :
    ∀ l ∈ t.loans, l ∈ s.loans := by
  unfold endLoan at h
  obtain ⟨a, _, h⟩ := Option.bind_eq_some_iff.mp h
  split at h
  · contradiction
  · obtain ⟨h, _⟩ := (validate_iff _ _).mp h
    cases h
    exact fun _ hl => (List.mem_filter.mp hl).1

private theorem life_subset (types : Var → Option Descriptor) (e : Exec.Life) (s t : State)
    (h : life types e s = some t) : ∀ l ∈ t.loans, l ∈ s.loans := by
  cases e with
  | bind x => rw [bind_loans types s t x h]; exact fun _ h => h
  | enter xs => rw [enter_loans types s t xs h]; exact fun _ h => h
  | drop xs => rw [drop_loans s t xs h]; exact fun _ h => h
  | leave =>
    unfold life leave at h
    obtain ⟨f, _, h⟩ := Option.bind_eq_some_iff.mp h
    obtain ⟨u, hu, h⟩ := Option.bind_eq_some_iff.mp h
    obtain ⟨v, hv, h⟩ := Option.bind_eq_some_iff.mp h
    split at h
    · contradiction
    · obtain ⟨h, _⟩ := (validate_iff _ _).mp h
      cases h
      intro l hl
      rw [drop_loans u v _ hv] at hl
      exact fold_loans _ (fun s l t h => endLoan_subset s t l.id h) _ s u hu l hl

/-- Entry consumes the pending metadata exactly once; other lifecycle events require an
empty argument buffer, so they cannot leave dangling argument identities. -/
theorem callLife_call_preserves (types : Var → Option Descriptor) (e : Exec.Life)
    (s t : CallState) (hs : CallState.WellFormed s) (h : callLife types e s = some t) :
    CallState.WellFormed t := by
  have hw := callLife_preserves types e s t h
  cases e with
  | enter params =>
    simp only [callLife] at h
    split at h
    · obtain ⟨u, hu, h⟩ := Option.bind_eq_some_iff.mp h
      obtain ⟨v, hv, h⟩ := Option.bind_eq_some_iff.mp h
      cases h
      have he := enter_loans types s.own u _ hu
      unfold adoptDown at hv
      obtain ⟨f, _, hv⟩ := Option.bind_eq_some_iff.mp hv
      split at hv
      · obtain ⟨hv, _⟩ := (validate_iff _ _).mp hv
        subst v
        refine ⟨hw, by simp, by simp, by simp, ?_⟩
        intro l hl
        obtain ⟨b, hb, rfl⟩ := List.mem_map.mp hl
        rw [he] at hb
        have flag := hs.2.2.2.2 b hb
        by_cases hm : b.id ∈ s.pending.map Prod.snd
        · simp [hm]
        · have hf : b.pendingArgument ≠ true := fun h => hm (flag.mp h)
          simp [hm, hf]
      · contradiction
    · contradiction
  | bind x | leave | drop x =>
    all_goals
      simp only [callLife] at h
      split at h
      · rename_i hempty
        obtain ⟨own, ho, hh⟩ := Option.map_eq_some_iff.mp h
        subst t
        refine ⟨hw, ?_, ?_, ?_, ?_⟩
        · simp [hempty]
        · simp [hempty]
        · simp [hempty]
        · intro l hl
          have hmem := life_subset types _ s.own own ho l hl
          have hf := hs.2.2.2.2 l hmem
          simpa [hempty] using hf
      · contradiction

namespace CallWitnesses
open ConcreteSpec.Core.Own.Examples

def waiting : CallState := (passDown ⟨exclusive, []⟩ ⟨0⟩ y).getD ⟨initial, []⟩
example : CallState.WellFormed ⟨exclusive, []⟩ ∧ CallState.WellFormed waiting := by decide
example : (passDown waiting ⟨0⟩ y) = none := by decide
example : callLife types .leave waiting = none := by decide
example : (callLife types (.enter [y]) waiting).isSome = true := by decide
example : CallState.WellFormed ((callLife types (.enter [y]) waiting).getD waiting) := by decide
/-- Planted bookkeeping mutations: duplicate parameters and a dangling identity. -/
example : ¬ CallState.WellFormed { waiting with pending := waiting.pending ++ waiting.pending } := by decide
example : ¬ CallState.WellFormed { waiting with own := exclusive } := by decide
/-- Planted leave mutation: discard loans but retain the pending buffer. -/
example : ¬ CallState.WellFormed { waiting with own := initial } := by decide
end CallWitnesses

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
  · cases hh
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
  · cases hh
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

/-- Adoption cannot transfer ordinary caller loans: every adopted identity must have been
marked as a pending argument by the argument-opening transition. -/
theorem adoptDown_only_arguments (s t : State) (args : List (Var × LoanId))
    (h : adoptDown s args = some t) (e : Var × LoanId) (he : e ∈ args) :
    ∃ l ∈ s.loans, l.id = e.2 ∧ l.pendingArgument = true := by
  unfold adoptDown at h
  obtain ⟨_, _, hh⟩ := Option.bind_eq_some_iff.mp h
  split at hh
  · rename_i guard
    exact guard.2.2.1 e he
  · contradiction

/-- Exit accounts for every slot of the departing activation, independently of whether its
locals table named the slot. A successful exit leaves none behind. -/
theorem leave_no_activation_slot (s t : State) (f : Activation)
    (hf : s.frames.head? = some f) (h : leave s = some t) :
    ∀ x ∈ t.slots, x.activation ≠ f.id := by
  unfold leave at h
  rw [hf] at h
  dsimp at h
  obtain ⟨_, _, hh⟩ := Option.bind_eq_some_iff.mp h
  obtain ⟨v, _, hh⟩ := Option.bind_eq_some_iff.mp hh
  split at hh
  · contradiction
  · rename_i guard
    obtain ⟨heq, _⟩ := (validate_iff _ _).mp hh
    subst t
    intro x hx he
    exact guard (List.any_eq_true.mpr ⟨x, hx, by simp [he]⟩)

end ConcreteSpec.Core.Own
