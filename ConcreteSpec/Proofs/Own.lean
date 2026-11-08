import ConcreteSpec.Spec.Own

/-! # C5 preservation and erasure
These laws derive preservation structurally from well-formed inputs and local operational
guards, and expose the concrete operation unchanged. They do not claim that
an arbitrary checker/elaborator enforces these transitions; that is C9's correspondence duty.
-/
namespace ConcreteSpec.Core.Own

/-- Helper (congruence). Lifts a supplied one-step preservation proof through a monadic list fold. (approved: PENDING). -/
private theorem fold_preserves {α : Type} (f : State → α → Option State)
    (hf : ∀ s a t, WellFormed s → f s a = some t → WellFormed t) (xs : List α)
    (s t : State) (hs : WellFormed s) (h : xs.foldlM f s = some t) : WellFormed t := by
  induction xs generalizing s with
  | nil => simp at h; subst t; exact hs
  | cons a xs ih =>
    simp only [List.foldlM_cons] at h
    cases he : f s a with
    | none => simp [he] at h
    | some u => simp only [he] at h <;> dsimp at h; exact ih u (hf s a u hs he) h

/-- Helper (inversion). Structural successful-result proof used by the operation-result law. (approved: PENDING). Move changes only the selected slot's status, after excluding every loan of its root. -/
theorem move_preserves_result (s t : State) (p : Hybrid.Place) (hs : WellFormed s)
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

/-- Helper (inversion). Structural successful-result proof used by the operation-result law. (approved: PENDING).  -/
theorem reinitialize_preserves_result (s t : State) (p : Hybrid.Place) (hs : WellFormed s)
    (h : reinitialize s p = some t) : WellFormed t := by
  unfold reinitialize at h
  obtain ⟨x, _, h⟩ := Option.bind_eq_some_iff.mp h
  split at h
  · cases h
    let change := fun x : Slot => if x.place = p then { x with status := .owned } else x
    have place (x : Slot) : (change x).place = x.place := by simp [change]; split <;> rfl
    have activation (x : Slot) : (change x).activation = x.activation := by simp [change]; split <;> rfl
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
      change (if a.place = p then { a with status := .owned } else a).status = .owned
      split
      · rfl
      · exact ho
  · contradiction

/-- Helper (computation). Reorders the disjunction defining symmetric loan compatibility. (approved: PENDING). -/
private theorem compatible_symm (a b : Loan) (h : Compatible a b) : Compatible b a := by
  unfold Compatible at *
  rcases h with h | h | h | h | h
  · exact Or.inl (Ne.symm h)
  · exact Or.inr (Or.inl h.symm)
  · exact Or.inr (Or.inr (Or.inr (Or.inl h)))
  · exact Or.inr (Or.inr (Or.inl h))
  · exact Or.inr (Or.inr (Or.inr (Or.inr h.symm)))

/-- Helper (congruence). Repackages freshness, parent validity and compatibility proofs for a single loan insertion. (approved: PENDING). -/
private theorem insert_loan (s : State) (a : Loan) (hs : WellFormed s)
    (hid : a.id = ⟨s.nextLoan⟩) (hact : a.activation < s.nextActivation)
    (hroot : ∃ x ∈ s.slots, x.place = a.root ∧ x.status = .owned)
    (hparent : ParentValid s a) (hpending : a.pendingArgument = true → (a.parent.isSome = true ∨ a.callRoot = true))
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

/-- Helper (inversion). Extracts owned-root and whole-root permission facts from the owner access guard. (approved: PENDING). -/
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

/-- Helper (inversion). Structural successful-result proof used by the operation-result law. (approved: PENDING).  -/
theorem borrow_preserves_result (s t : State) (p : Hybrid.Place) (path : List Projection)
    (m : Mode) (hs : WellFormed s) (h : borrow s p path m = some t) : WellFormed t := by
  unfold borrow at h
  obtain ⟨f, _, h⟩ := Option.bind_eq_some_iff.mp h
  split at h
  · rename_i hg
    cases h
    obtain ⟨hg, hact⟩ := Bool.and_eq_true_iff.mp hg
    obtain ⟨hroot, hconf⟩ := owner_guard s p m hg
    apply insert_loan s ⟨⟨s.nextLoan⟩, p, path, m, none, [], f.id, false, false⟩ hs rfl
      (of_decide_eq_true hact) hroot (by rfl) (by simp)
    intro b hb
    rcases hconf b hb with hn | hm
    · exact Or.inl (Ne.symm hn)
    · exact Or.inr (Or.inr (Or.inr (Or.inr hm)))
  · contradiction

/-- Helper (inversion). Structural successful-result proof used by the operation-result law. (approved: PENDING).  -/
theorem reborrowTo_preserves_result (s t : State) (i : LoanId) (path : List Projection)
    (m : Mode) (activation : Nat) (hs : WellFormed s)
    (h : reborrowTo s i path m activation = some t) : WellFormed t := by
  unfold reborrowTo at h
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
      i :: a.ancestors, activation, false, false⟩ hs rfl (of_decide_eq_true hact).1
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

/-- Helper (inversion). Structural successful-result proof used by the operation-result law. (approved: PENDING).  -/
theorem reborrow_preserves_result (s t : State) (i : LoanId) (path : List Projection)
    (m : Mode) (hs : WellFormed s) (h : reborrow s i path m = some t) : WellFormed t := by
  unfold reborrow at h
  obtain ⟨f, _, h⟩ := Option.bind_eq_some_iff.mp h
  exact reborrowTo_preserves_result s t i path m f.id hs h

/-- Helper (inversion). Structural successful-result proof used by the operation-result law. (approved: PENDING).  -/
theorem endLoan_preserves_result (s t : State) (i : LoanId) (hs : WellFormed s)
    (h : endLoan s i = some t) : WellFormed t := by
  unfold endLoan at h
  obtain ⟨a, _, h⟩ := Option.bind_eq_some_iff.mp h
  split at h
  · contradiction
  · rename_i clear
    cases h
    refine ⟨hs.1, hs.2.1.sublist (List.Sublist.map _ List.filter_sublist), hs.2.2.1, ?_, ?_⟩
    · intro l hl
      have hl' := (List.mem_filter.mp hl).1
      obtain ⟨hb, ha, hr, hp, hpend⟩ := hs.2.2.2.1 l hl'
      refine ⟨hb, ha, hr, ?_, hpend⟩
      unfold ParentValid at hp ⊢
      cases he : l.parent with
      | none => simpa only [he] using hp
      | some p =>
        simp only [he] at hp ⊢
        obtain ⟨b, hbm, hid, rest⟩ := hp
        refine ⟨b, List.mem_filter.mpr ⟨hbm, ?_⟩, hid, rest⟩
        have hn : b.id ≠ i := by
          intro hbi
          apply clear
          exact List.any_eq_true.mpr ⟨l, hl', by simp [he, ← hid, hbi]⟩
        simpa using hn
    · intro b hb c hc
      exact hs.2.2.2.2 b (List.mem_filter.mp hb).1 c (List.mem_filter.mp hc).1

/-- Helper (congruence). Owner access returns the identical state after its permission guard succeeds. (approved: PENDING). -/
theorem accessOwner_preserves (s t : State) (p : Hybrid.Place) (m : Mode)
    (hs : WellFormed s) (h : accessOwner s p m = some t) : WellFormed t := by
  unfold accessOwner at h
  split at h
  · cases h; exact hs
  · contradiction

/-- Helper (congruence). Loan access returns the identical state after its permission guard succeeds. (approved: PENDING). -/
theorem accessLoan_preserves (s t : State) (i : LoanId) (m : Mode)
    (hs : WellFormed s) (h : accessLoan s i m = some t) : WellFormed t := by
  unfold accessLoan at h
  split at h
  · cases h; exact hs
  · contradiction

/-- Helper (computation). Computes the transition or extracts its checked guard; not independent soundness evidence. (approved: PENDING). A movable non-Copy slot with no outstanding loan always moves; unrelated invariant
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

/-- Helper (computation). Computes the transition or extracts its checked guard; not independent soundness evidence. (approved: PENDING). -/
theorem borrow_progress (s : State) (p : Hybrid.Place) (path : List Projection) (m : Mode)
    (f : Activation) (frame : s.frames.head? = some f)
    (issued : f.id < s.nextActivation) (allowed : ownerAccess s p m = true) :
    ∃ t, borrow s p path m = some t := by
  simp [borrow, frame, issued, allowed]

/-- Helper (computation). Computes the transition or extracts its checked guard; not independent soundness evidence. (approved: PENDING). -/
theorem reborrowTo_progress (s : State) (i : LoanId) (path : List Projection) (m : Mode)
    (activation : Nat) (a : Loan) (found : loan s i = some a)
    (issued : activation < s.nextActivation)
    (current : s.frames.head?.map Activation.id = some activation)
    (allowed : loanAccess s i m = true) :
    ∃ t, reborrowTo s i path m activation = some t := by
  simp [reborrowTo, found, issued, current, allowed]

/-- Helper (computation). Computes the transition or extracts its checked guard; not independent soundness evidence. (approved: PENDING). -/
theorem reborrow_progress (s : State) (i : LoanId) (path : List Projection) (m : Mode)
    (f : Activation) (frame : s.frames.head? = some f) (issued : f.id < s.nextActivation)
    (a : Loan) (found : loan s i = some a) (allowed : loanAccess s i m = true) :
    ∃ t, reborrow s i path m = some t := by
  simpa [reborrow, frame] using
    reborrowTo_progress s i path m f.id a found issued (by simp [frame]) allowed

/-- Helper (computation). Computes the transition or extracts its checked guard; not independent soundness evidence. (approved: PENDING). -/
theorem accessOwner_progress (s : State) (p : Hybrid.Place) (m : Mode)
    (allowed : ownerAccess s p m = true) : accessOwner s p m = some s := by
  simp [accessOwner, allowed]

/-- Helper (computation). Computes the transition or extracts its checked guard; not independent soundness evidence. (approved: PENDING). -/
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
example : WellFormed exclusive ∧ (reborrowTo exclusive ⟨0⟩ [] .mutable 0).isSome = true ∧
    WellFormed child := by decide
example : accessOwner shared ⟨0⟩ .shared = some shared ∧ WellFormed shared := by decide
example : accessLoan child ⟨1⟩ .mutable = some child ∧ WellFormed child := by decide
/-- Malformed move/access observations destroy an owned root under a live loan. -/
example : ¬ WellFormed { shared with slots := shared.slots.map fun x =>
    { x with status := .moved } } := by decide
example : ¬ WellFormed { child with slots := [] } := by decide
/-- A malformed borrow reuses the supply; a malformed reborrow loses its parent. -/
example : ¬ WellFormed { shared with nextLoan := 0 } := by decide
example : ¬ WellFormed { child with loans := child.loans.filter (·.id != ⟨0⟩) } := by decide
/-- Negated success with each operational permission violated. -/
example : ¬ (move shared ⟨0⟩).isSome = true := by decide
example : ¬ (borrow exclusive ⟨0⟩ [] .shared).isSome = true := by decide
example : ¬ (reborrowTo shared ⟨0⟩ [] .mutable 0).isSome = true := by decide
example : ¬ (reborrow shared ⟨0⟩ [] .mutable).isSome = true := by decide
example : ¬ (accessOwner shared ⟨0⟩ .mutable).isSome = true := by decide
example : ¬ (accessLoan child ⟨0⟩ .mutable).isSome = true := by decide
end PrimitiveWitnesses

/-- Helper (inversion). Structural successful-result proof used by the operation-result law. (approved: PENDING).  -/
theorem bind_preserves_result (types : Var → Option Descriptor) (s t : State) (x : Var)
    (hs : WellFormed s) (h : bind types x s = some t) : WellFormed t := by
  unfold bind at h
  obtain ⟨f, _, h⟩ := Option.bind_eq_some_iff.mp h
  split at h
  · exact reinitialize_preserves_result _ _ _ hs h
  · obtain ⟨d, _, h⟩ := Option.bind_eq_some_iff.mp h
    split at h
    · rename_i hact
      cases h
      refine ⟨?_, hs.2.1, ?_, ?_, hs.2.2.2.2⟩
      · simp only [List.map_cons, List.nodup_cons]
        refine ⟨?_, hs.1⟩
        intro hm
        obtain ⟨a, ha, he⟩ := List.mem_map.mp hm
        have hb := (hs.2.2.1 a ha).1
        rw [he] at hb
        exact Nat.lt_irrefl _ hb
      · intro a ha
        rcases List.mem_cons.mp ha with he | ha
        · subst a; exact ⟨Nat.lt_succ_self _, hact⟩
        · exact ⟨Nat.lt_succ_of_lt (hs.2.2.1 a ha).1, (hs.2.2.1 a ha).2⟩
      · intro l hl
        obtain ⟨hb, ha, ⟨a, ham, he, ho⟩, hp, hpend⟩ := hs.2.2.2.1 l hl
        exact ⟨hb, ha, ⟨a, List.mem_cons_of_mem _ ham, he, ho⟩, hp, hpend⟩
    · contradiction

/-- Helper (inversion). Structural successful-result proof used by the operation-result law. (approved: PENDING).  -/
theorem drop_preserves_result (s t : State) (xs : List Var) (hs : WellFormed s)
    (h : drop xs s = some t) : WellFormed t := by
  unfold drop at h
  obtain ⟨f, _, h⟩ := Option.bind_eq_some_iff.mp h
  dsimp at h
  split at h
  · rename_i hg
    cases h
    refine ⟨hs.1.sublist (List.Sublist.map _ List.filter_sublist), hs.2.1, ?_, ?_, hs.2.2.2.2⟩
    · intro a ha
      exact hs.2.2.1 a (List.mem_filter.mp ha).1
    · intro l hl
      obtain ⟨hb, ha, ⟨a, ham, he, ho⟩, hp, hpend⟩ := hs.2.2.2.1 l hl
      refine ⟨hb, ha, ⟨a, List.mem_filter.mpr ⟨ham, ?_⟩, he, ho⟩, hp, hpend⟩
      have hc := List.all_eq_true.mp (Bool.and_eq_true_iff.mp hg).2 l hl
      simpa [he] using hc
  · contradiction

/-- Helper (inversion). Structural successful-result proof used by the operation-result law. (approved: PENDING).  -/
theorem enter_preserves_result (types : Var → Option Descriptor) (s t : State) (xs : List Var)
    (hs : WellFormed s) (h : enter types xs s = some t) : WellFormed t := by
  have hw : WellFormed { s with
      nextActivation := s.nextActivation + 1
      frames := ⟨s.nextActivation, [], []⟩ :: s.frames } := by
    refine ⟨hs.1, hs.2.1, ?_, ?_, hs.2.2.2.2⟩
    · intro a ha
      exact ⟨(hs.2.2.1 a ha).1, Nat.lt_succ_of_lt (hs.2.2.1 a ha).2⟩
    · intro l hl
      obtain ⟨hb, ha, hr, hp, hpend⟩ := hs.2.2.2.1 l hl
      exact ⟨hb, Nat.lt_succ_of_lt ha, hr, hp, hpend⟩
  exact fold_preserves _ (fun s x t hs h => bind_preserves_result types s t x hs h) xs _ t hw h

/-- Helper (inversion). Structural successful-result proof used by the operation-result law. (approved: PENDING).  -/
theorem endLocals_preserves_result (s t : State) (a : Nat) (hs : WellFormed s)
    (h : endLocals a s = some t) : WellFormed t :=
  fold_preserves _ (fun s l t hs h => endLoan_preserves_result s t l.id hs h) _ _ _ hs h

/-- Helper (inversion). Structural successful-result proof used by the operation-result law. (approved: PENDING).  -/
theorem leave_preserves_result (s t : State) (hs : WellFormed s)
    (h : leave s = some t) : WellFormed t := by
  unfold leave at h
  obtain ⟨f, _, h⟩ := Option.bind_eq_some_iff.mp h
  obtain ⟨u, hu, h⟩ := Option.bind_eq_some_iff.mp h
  obtain ⟨v, hv, h⟩ := Option.bind_eq_some_iff.mp h
  split at h
  · contradiction
  · rename_i guard
    cases h
    have hw := drop_preserves_result u v _ (endLocals_preserves_result s u f.id hs hu) hv
    have ok : (v.slots.all (fun x => x.activation != f.id || x.desc.copy || x.status == .moved) &&
        v.loans.all (fun l => v.slots.all (fun x => x.activation != f.id || x.place != l.root))) = true := by
      cases hb : (v.slots.all (fun x => x.activation != f.id || x.desc.copy || x.status == .moved) &&
        v.loans.all (fun l => v.slots.all (fun x => x.activation != f.id || x.place != l.root))) <;>
        simp_all
    have clear := (Bool.and_eq_true_iff.mp ok).2
    refine ⟨hw.1.sublist (List.Sublist.map _ List.filter_sublist), hw.2.1, ?_, ?_, hw.2.2.2.2⟩
    · intro x hx
      exact hw.2.2.1 x (List.mem_filter.mp hx).1
    · intro l hl
      obtain ⟨hid, ha, ⟨x, hx, he, ho⟩, hp, hpend⟩ := hw.2.2.2.1 l hl
      refine ⟨hid, ha, ⟨x, List.mem_filter.mpr ⟨hx, ?_⟩, he, ho⟩, hp, hpend⟩
      have hc := List.all_eq_true.mp (List.all_eq_true.mp clear l hl) x hx
      simpa [he] using hc

/-- Helper (inversion). Structural successful-result proof used by the operation-result law. (approved: PENDING).  -/
theorem enterDown_preserves_result (s t : State) (args : List (Var × LoanId)) (hs : WellFormed s)
    (h : enterDown s args = some t) : WellFormed t := by
  unfold enterDown at h
  obtain ⟨u, hu, hh⟩ := Option.bind_eq_some_iff.mp h
  refine fold_preserves _ ?_ args u t (enter_preserves_result _ _ _ _ hs hu) hh
  intro s arg t hs hh
  obtain ⟨a, _, hh⟩ := Option.bind_eq_some_iff.mp hh
  obtain ⟨f, _, hh⟩ := Option.bind_eq_some_iff.mp hh
  obtain ⟨v, hv, hh⟩ := Option.bind_eq_some_iff.mp hh
  cases hh
  exact reborrowTo_preserves_result s v arg.2 [] a.mode f.id hs hv

/-- Helper (inversion). Structural successful-result proof used by the operation-result law. (approved: PENDING).  -/
theorem life_preserves_result (types : Var → Option Descriptor) (e : Exec.Life) (s t : State)
    (hs : WellFormed s) (h : life types e s = some t) : WellFormed t := by
  cases e with
  | bind x => exact bind_preserves_result types s t x hs h
  | enter ps => exact enter_preserves_result types s t ps hs h
  | leave => exact leave_preserves_result s t hs h
  | drop xs => exact drop_preserves_result s t xs hs h

/-- Helper (computation). Computes the transition or extracts its checked guard; not independent soundness evidence. (approved: PENDING). -/
theorem reinitialize_progress (s : State) (p : Hybrid.Place) (x : Slot)
    (found : slot s p = some x)
    (available : x.status = .moved ∨ (x.desc.copy = true ∧ x.desc.mutable = true))
    (clear : ∀ l ∈ s.loans, l.root ≠ p) : ∃ t, reinitialize s p = some t := by
  have hc : s.loans.all (·.root != p) = true := by
    apply List.all_eq_true.mpr
    intro l hl
    simpa using clear l hl
  have ha : (x.status == .moved || (x.desc.copy && x.desc.mutable)) = true := by
    simpa only [Bool.or_eq_true, beq_iff_eq, Bool.and_eq_true] using available
  unfold reinitialize
  rw [found]
  dsimp
  rw [ha, hc]
  exact ⟨_, rfl⟩

/-- Helper (computation). Computes the transition or extracts its checked guard; not independent soundness evidence. (approved: PENDING). -/
theorem endLoan_progress (s : State) (i : LoanId) (a : Loan)
    (found : loan s i = some a) (children : ∀ b ∈ s.loans, b.parent ≠ some i) :
    ∃ t, endLoan s i = some t := by
  have hc : s.loans.any (·.parent == some i) = false := by
    apply Bool.eq_false_iff.mpr
    intro hg
    obtain ⟨b, hb, he⟩ := List.any_eq_true.mp hg
    exact children b hb (by simpa using he)
  simp [endLoan, found, hc]

/-- Helper (computation). Computes the transition or extracts its checked guard; not independent soundness evidence. (approved: PENDING). -/
theorem bind_fresh_progress (types : Var → Option Descriptor) (s : State) (x : Var)
    (f : Activation) (d : Descriptor) (frame : s.frames.head? = some f)
    (issued : f.id < s.nextActivation) (fresh : AList.lookup f.locals x = none)
    (descriptor : types x = some d) : ∃ t, bind types x s = some t := by
  simp [bind, frame, issued, fresh, descriptor]

/-- Helper (congruence). Composes fresh typed bindings by induction over the parameter list. (approved: PENDING). -/
private theorem bind_params_progress (types : Var → Option Descriptor) (xs : List Var)
    (s : State) (f : Activation) (frame : s.frames.head? = some f)
    (issued : f.id < s.nextActivation) (unique : xs.Nodup)
    (fresh : ∀ x ∈ xs, AList.lookup f.locals x = none)
    (typed : ∀ x ∈ xs, ∃ d, types x = some d) :
    ∃ t, xs.foldlM (fun s x => bind types x s) s = some t := by
  induction xs generalizing s f with
  | nil => exact ⟨s, rfl⟩
  | cons x xs ih =>
    obtain ⟨d, hd⟩ := typed x List.mem_cons_self
    let u : State := { s with
      nextPlace := s.nextPlace + 1
      slots := ⟨⟨s.nextPlace⟩, f.id, d, .owned⟩ :: s.slots
      frames := { f with locals := (x, ⟨s.nextPlace⟩) :: f.locals } :: s.frames.tail }
    have hb : bind types x s = some u := by
      simp [bind, frame, fresh x List.mem_cons_self, hd, issued, u]
    obtain ⟨t, ht⟩ := ih u { f with locals := (x, ⟨s.nextPlace⟩) :: f.locals } rfl issued
      (List.nodup_cons.mp unique).2
      (by
        intro y hy
        have hn : x ≠ y := fun he => (List.nodup_cons.mp unique).1 (he ▸ hy)
        simpa [AList.lookup, hn] using fresh y (List.mem_cons_of_mem _ hy))
      (fun y hy => typed y (List.mem_cons_of_mem _ hy))
    exact ⟨t, by simp [List.foldlM_cons, hb, ht]⟩

/-- Law (subject `ConcreteSpec.Core.Own.enter`). Fresh, distinct, typed parameters always enter: no global invariant scan can refuse them. -/
theorem enter_progress (types : Var → Option Descriptor) (s : State) (params : List Var)
    (unique : params.Nodup) (typed : ∀ x ∈ params, ∃ d, types x = some d) :
    ∃ t, enter types params s = some t :=
  bind_params_progress types params
    { s with nextActivation := s.nextActivation + 1
             frames := ⟨s.nextActivation, [], []⟩ :: s.frames }
    ⟨s.nextActivation, [], []⟩ rfl (Nat.lt_succ_self _) unique
    (fun _ _ => rfl) typed

/-- Helper (computation). Computes the transition or extracts its checked guard; not independent soundness evidence. (approved: PENDING). -/
theorem drop_progress (s : State) (xs : List Var) (f : Activation)
    (frame : s.frames.head? = some f)
    (discharged : ∀ a ∈ s.slots,
      a.place ∈ (f.locals.filter (fun e => decide (e.1 ∈ xs))).map Prod.snd →
      a.desc.copy = true ∨ a.status = .moved)
    (clear : ∀ l ∈ s.loans,
      l.root ∉ (f.locals.filter (fun e => decide (e.1 ∈ xs))).map Prod.snd) :
    ∃ t, drop xs s = some t := by
  have hd : (s.slots.all (fun a =>
      decide (a.place ∉ (f.locals.filter (fun e => decide (e.1 ∈ xs))).map Prod.snd) ||
      a.desc.copy || a.status == .moved)) = true := by
    apply List.all_eq_true.mpr
    intro a ha
    by_cases hm : a.place ∈ (f.locals.filter (fun e => decide (e.1 ∈ xs))).map Prod.snd
    · rcases discharged a ha hm with hc | hs
      · simp [hc]
      · simp [hs]
    · simp [hm]
  have hc : (s.loans.all (fun l =>
      decide (l.root ∉ (f.locals.filter (fun e => decide (e.1 ∈ xs))).map Prod.snd))) = true := by
    apply List.all_eq_true.mpr
    intro l hl
    simp [clear l hl]
  unfold drop
  rw [frame]
  dsimp
  rw [hd, hc]
  exact ⟨_, rfl⟩

namespace LifecycleWitnesses
open ConcreteSpec.Core.Own.Examples

def moved : State := (move initial ⟨0⟩).getD initial
def entered : State := (enter types [y] shared).getD initial
def called : State := (enterDown exclusive [(y, ⟨0⟩)]).getD initial
/-- Each lifecycle/loan-ending law has a live-state success instance. -/
example : WellFormed moved ∧ reinitialize moved ⟨0⟩ = some initial := by decide
example : WellFormed child ∧ (endLoan child ⟨1⟩).isSome = true ∧
    WellFormed ((endLoan child ⟨1⟩).getD initial) := by decide
example : WellFormed shared ∧ (bind types y shared).isSome = true ∧
    WellFormed ((bind types y shared).getD initial) := by decide
example : WellFormed moved ∧ (drop [x] moved).isSome = true ∧
    WellFormed ((drop [x] moved).getD initial) := by decide
example : WellFormed entered ∧ (enter types [y] shared).isSome = true := by decide
example : WellFormed called ∧ (endLocals 1 called).isSome = true ∧
    WellFormed ((endLocals 1 called).getD initial) := by decide
example : (leave called).isSome = true ∧ WellFormed ((leave called).getD initial) := by decide
example : life types (.bind y) shared = bind types y shared := by rfl
/-- Malformed reinitialization/drop/leave observations lose a borrowed root. -/
example : ¬ WellFormed { shared with slots := [] } := by decide
/-- A malformed bind reuses the place identity. -/
example : ¬ WellFormed { initial with slots := initial.slots ++ initial.slots } := by decide
/-- Planted entry/adoption mutation assigns an unissued activation. -/
example : ¬ WellFormed { shared with loans := shared.loans.map fun l =>
    { l with activation := shared.nextActivation } } := by decide
/-- Planted endLoan/endLocals mutation removes the parent before the child. -/
example : ¬ WellFormed { child with loans := child.loans.filter (·.id != ⟨0⟩) } := by decide
/-- Progress counterexamples: live linear replacement, a child and a live local obligation. -/
example : ¬ (reinitialize initial ⟨0⟩).isSome = true := by decide
example : ¬ (endLoan child ⟨0⟩).isSome = true := by decide
example : ¬ (bind (fun _ => none) y shared).isSome = true := by decide
example : ¬ (drop [x] initial).isSome = true := by decide
end LifecycleWitnesses

/-- Helper (projection). Projects the owned-root clause of the well-formedness invariant. (approved: PENDING). -/
theorem loan_has_owned_root (s : State) (hs : WellFormed s) (l : Loan) (hl : l ∈ s.loans) :
    ∃ x ∈ s.slots, x.place = l.root ∧ x.status = .owned := (hs.2.2.2.1 l hl).2.2.1

/-- Helper (inversion). Uses a duplicate-free place index to identify two slot records. (approved: PENDING). -/
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

/-- Helper (inversion). An invariant consequence using uniqueness of slot identities and owned roots; not an independent transition law. (approved: PENDING). -/
theorem moved_no_loan (s : State) (hs : WellFormed s) (x : Slot) (hx : x ∈ s.slots)
    (hm : x.status = .moved) (l : Loan) (hl : l ∈ s.loans) : x.place ≠ l.root := by
  intro he
  obtain ⟨y, hy, hp, ho⟩ := loan_has_owned_root s hs l hl
  have hxy : x = y := slot_unique s.slots hs.1 x y hx hy (he.trans hp.symm)
  subst y
  rw [hm] at ho
  cases ho

/-- Helper (computation). Computes the transition or extracts its checked guard; not independent soundness evidence. (approved: PENDING). Success projects to precisely the uninstrumented result and concrete store. This covers
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

/-- Helper (computation). Computes the transition or extracts its checked guard; not independent soundness evidence. (approved: PENDING). -/
theorem life_erasure {σ : Type} (types : Var → Option Descriptor) (e : Exec.Life)
    (s t : Store σ) (h : lifeStore types e s = some t) : erase t = erase s := by
  unfold lifeStore at h
  cases he : life types e s.own <;> simp [he] at h
  subst t
  rfl

/-- Helper (congruence). Transports the invariant through a loan map that retains semantic identity fields. (approved: PENDING). -/
private theorem map_metadata_preserves (s : State) (hs : WellFormed s)
    (act : Loan → Nat) (pending : Loan → Bool)
    (issued : ∀ l ∈ s.loans, act l < s.nextActivation)
    (parents : ∀ l ∈ s.loans, pending l = true → (l.parent.isSome = true ∨ l.callRoot = true)) :
    WellFormed { s with loans := s.loans.map fun l =>
      { l with activation := act l, pendingArgument := pending l } } := by
  let change := fun l : Loan => { l with activation := act l, pendingArgument := pending l }
  refine ⟨hs.1, ?_, hs.2.2.1, ?_, ?_⟩
  · simpa only [List.map_map, Function.comp_def] using hs.2.1
  · intro l hl
    obtain ⟨a, ha, rfl⟩ := List.mem_map.mp hl
    obtain ⟨hb, _, hr, hp, _⟩ := hs.2.2.2.1 a ha
    refine ⟨hb, issued a ha, hr, ?_, parents a ha⟩
    unfold ParentValid at hp ⊢
    cases he : a.parent with
    | none => simpa only [he] using hp
    | some p =>
      simp only [he] at hp ⊢
      obtain ⟨b, hb, rest⟩ := hp
      exact ⟨change b, List.mem_map.mpr ⟨b, hb, rfl⟩, rest⟩
  · intro a ha b hb
    obtain ⟨x, hx, rfl⟩ := List.mem_map.mp ha
    obtain ⟨y, hy, rfl⟩ := List.mem_map.mp hb
    exact hs.2.2.2.2 x hx y hy

/-- Helper (inversion). Structural successful-result proof used by the operation-result law. (approved: PENDING).  -/
theorem adoptDown_preserves_result (s t : State) (args : List (Var × LoanId))
    (hs : WellFormed s) (h : adoptDown s args = some t) : WellFormed t := by
  unfold adoptDown at h
  obtain ⟨f, _, h⟩ := Option.bind_eq_some_iff.mp h
  split at h
  · rename_i hg
    cases h
    have hw := map_metadata_preserves s hs
      (fun l => if l.id ∈ args.map Prod.snd then f.id else l.activation)
      (fun l => if l.id ∈ args.map Prod.snd then false else l.pendingArgument)
      (by intro l hl; split; exact hg.1; exact (hs.2.2.2.1 l hl).2.1)
      (by intro l hl; split; simp; exact (hs.2.2.2.1 l hl).2.2.2.2)
    have eq : (fun l : Loan => if l.id ∈ args.map Prod.snd then
        { l with activation := f.id, pendingArgument := false } else l) =
        (fun l : Loan => { l with
          activation := if l.id ∈ args.map Prod.snd then f.id else l.activation
          pendingArgument := if l.id ∈ args.map Prod.snd then false else l.pendingArgument }) := by
      funext l
      split <;> rfl
    rw [eq]
    exact hw
  · contradiction

/-- Helper (inversion). Structural successful-result proof used by the operation-result law. (approved: PENDING).  -/
theorem passDown_preserves_result (s t : CallState) (i : LoanId) (parameter : Var)
    (hs : WellFormed s.own) (h : passDown s i parameter = some t) : WellFormed t.own := by
  unfold passDown at h
  split at h
  · contradiction
  · obtain ⟨a, ha, h⟩ := Option.bind_eq_some_iff.mp h
    obtain ⟨u, hu, h⟩ := Option.bind_eq_some_iff.mp h
    cases h
    have hw := reborrow_preserves_result s.own u i [] a.mode hs hu
    have shape := hu
    unfold reborrow at shape
    obtain ⟨f, _, shape⟩ := Option.bind_eq_some_iff.mp shape
    unfold reborrowTo at shape
    rw [ha] at shape
    dsimp at shape
    split at shape
    · cases shape
      have hh := map_metadata_preserves _ hw (fun l => l.activation)
        (fun l => if l.id.id = s.own.nextLoan then true else l.pendingArgument)
        (fun l hl => (hw.2.2.2.1 l hl).2.1)
        (by
          intro l hl
          rcases List.mem_cons.mp hl with he | hl
          · subst l; simp
          · have hn : l.id.id ≠ s.own.nextLoan := by
              have := (hs.2.2.2.1 l hl).1
              omega
            simpa [hn] using (hs.2.2.2.1 l hl).2.2.2.2)
      have eq : (fun l : Loan => if l.id.id = s.own.nextLoan then
          { l with pendingArgument := true } else l) =
          (fun l : Loan => { l with
            activation := l.activation
            pendingArgument := if l.id.id = s.own.nextLoan then true else l.pendingArgument }) := by
        funext l
        split <;> rfl
      dsimp only
      rw [eq]
      exact hh
    · contradiction

/-- Helper (inversion). Structural successful-result proof used by the operation-result law. (approved: PENDING). Pending metadata stays synchronized when argument evaluation adds a fresh child. -/
theorem passDown_call_preserves_result (s t : CallState) (i : LoanId) (parameter : Var)
    (hs : CallState.WellFormed s) (h : passDown s i parameter = some t) :
    CallState.WellFormed t := by
  have hw := passDown_preserves_result s t i parameter hs.1 h
  unfold passDown at h
  split at h
  · contradiction
  · rename_i unique
    obtain ⟨a, ha, hh⟩ := Option.bind_eq_some_iff.mp h
    obtain ⟨u, hu, hh⟩ := Option.bind_eq_some_iff.mp hh
    cases hh
    unfold reborrow at hu
    obtain ⟨f, hf, hu⟩ := Option.bind_eq_some_iff.mp hu
    unfold reborrowTo at hu
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

/-- Helper (inversion). Structural successful-result proof used by the operation-result law. (approved: PENDING). C1's actual call/lifecycle callback preserves ownership, for both owner and view parameters. -/
theorem callLife_preserves_result (types : Var → Option Descriptor) (e : Exec.Life)
    (s t : CallState) (hs : WellFormed s.own)
    (h : callLife types e s = some t) : WellFormed t.own := by
  cases e with
  | enter params =>
    simp only [callLife] at h
    split at h
    · obtain ⟨u, hu, hh⟩ := Option.bind_eq_some_iff.mp h
      obtain ⟨v, hv, hh⟩ := Option.bind_eq_some_iff.mp hh
      cases hh
      exact adoptDown_preserves_result u v s.pending (enter_preserves_result types s.own u _ hs hu) hv
    · contradiction
  | bind x =>
    simp only [callLife] at h
    split at h
    · obtain ⟨own, ho, hh⟩ := Option.map_eq_some_iff.mp h
      subst t
      exact life_preserves_result types (.bind x) s.own own hs ho
    · contradiction
  | leave =>
    simp only [callLife] at h
    split at h
    · obtain ⟨own, ho, hh⟩ := Option.map_eq_some_iff.mp h
      subst t
      exact life_preserves_result types .leave s.own own hs ho
    · contradiction
  | drop xs =>
    simp only [callLife] at h
    split at h
    · obtain ⟨own, ho, hh⟩ := Option.map_eq_some_iff.mp h
      subst t
      exact life_preserves_result types (.drop xs) s.own own hs ho
    · contradiction

/-- Helper (computation). Reinitialization changes slot status while leaving the loan table unchanged. (approved: PENDING). -/
private theorem reinitialize_loans (s t : State) (p : Hybrid.Place)
    (h : reinitialize s p = some t) : t.loans = s.loans := by
  unfold reinitialize at h
  obtain ⟨x, _, h⟩ := Option.bind_eq_some_iff.mp h
  split at h
  · cases h; rfl
  · contradiction

/-- Helper (computation). Both binding branches leave the loan table unchanged. (approved: PENDING). -/
private theorem bind_loans (types : Var → Option Descriptor) (s t : State) (x : Var)
    (h : bind types x s = some t) : t.loans = s.loans := by
  unfold bind at h
  obtain ⟨f, _, h⟩ := Option.bind_eq_some_iff.mp h
  split at h
  · exact reinitialize_loans _ _ _ h
  · obtain ⟨d, _, h⟩ := Option.bind_eq_some_iff.mp h
    split at h
    · cases h; rfl
    · contradiction

/-- Helper (congruence). Lifts a supplied loan-table equality through a monadic fold. (approved: PENDING). -/
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

/-- Helper (congruence). Composes binding loan-table equalities through parameter entry. (approved: PENDING). -/
private theorem enter_loans (types : Var → Option Descriptor) (s t : State) (xs : List Var)
    (h : enter types xs s = some t) : t.loans = s.loans := by
  unfold enter at h
  dsimp at h
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

/-- Helper (computation). Dropping slots does not edit the loan table. (approved: PENDING). -/
private theorem drop_loans (s t : State) (xs : List Var) (h : drop xs s = some t) :
    t.loans = s.loans := by
  unfold drop at h
  obtain ⟨f, _, h⟩ := Option.bind_eq_some_iff.mp h
  dsimp at h
  split at h
  · cases h; rfl
  · contradiction

/-- Helper (inversion). Inverts membership in the filtered loan table. (approved: PENDING). -/
private theorem endLoan_subset (s t : State) (i : LoanId) (h : endLoan s i = some t) :
    ∀ l ∈ t.loans, l ∈ s.loans := by
  unfold endLoan at h
  obtain ⟨a, _, h⟩ := Option.bind_eq_some_iff.mp h
  split at h
  · contradiction
  · cases h
    exact fun _ hl => (List.mem_filter.mp hl).1

/-- Helper (inversion). Combines lifecycle branch inversions to trace surviving loans to their input. (approved: PENDING). -/
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
    · cases h
      intro l hl
      rw [drop_loans u v _ hv] at hl
      exact fold_loans _ (fun s l t h => endLoan_subset s t l.id h) _ s u hu l hl

/-- Helper (inversion). Structural successful-result proof used by the operation-result law. (approved: PENDING). Entry consumes the pending metadata exactly once; other lifecycle events require an
empty argument buffer, so they cannot leave dangling argument identities. -/
theorem callLife_call_preserves_result (types : Var → Option Descriptor) (e : Exec.Life)
    (s t : CallState) (hs : CallState.WellFormed s) (h : callLife types e s = some t) :
    CallState.WellFormed t := by
  have hw := callLife_preserves_result types e s t hs.1 h
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
      · cases hv
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
/-- Malformed bookkeeping observations: duplicate parameters and a dangling identity. -/
example : ¬ CallState.WellFormed { waiting with pending := waiting.pending ++ waiting.pending } := by decide
example : ¬ CallState.WellFormed { waiting with own := exclusive } := by decide
/-- Planted leave mutation: discard loans but retain the pending buffer. -/
example : ¬ CallState.WellFormed { waiting with own := initial } := by decide
end CallWitnesses

/-- Helper (computation). Computes the transition or extracts its checked guard; not independent soundness evidence. (approved: PENDING). Passing loans changes only instrumentation, including at mixed-parameter call entry. -/
theorem callLife_erasure {σ : Type} (types : Var → Option Descriptor) (e : Exec.Life)
    (s t : σ × CallState) (h : callLifeStore types e s = some t) : t.1 = s.1 := by
  unfold callLifeStore at h
  obtain ⟨_, _, hh⟩ := Option.map_eq_some_iff.mp h
  subst t
  rfl

/-- Helper (congruence). Derived invariant or supplied-preservation fact; not independent soundness evidence. (approved: PENDING). Any successful instrumented operation preserves the invariant when its permission
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

/-- Helper (computation). Computes the transition or extracts its checked guard; not independent soundness evidence. (approved: PENDING). -/
theorem readOwner_erasure (tgt : Target) (p : Hybrid.Place) (s t : Store Hybrid.State)
    (v : Hybrid.Value) (h : readOwner tgt p s = some (v, t)) :
    s.concrete.read tgt p = some v ∧ t.concrete = s.concrete := by
  have he := instrument_erasure _ _ _ _ _ h
  dsimp [erase] at he
  cases hc : s.concrete.read tgt p with
  | none => simp [hc] at he
  | some w => simp [hc] at he; rcases he with ⟨rfl, hstore⟩; exact ⟨rfl, hstore.symm⟩

/-- Helper (computation). Computes the transition or extracts its checked guard; not independent soundness evidence. (approved: PENDING). -/
theorem writeOwner_erasure (tgt : Target) (p : Hybrid.Place) (s t : Store Hybrid.State)
    (v : Hybrid.Value) (h : writeOwner tgt p v s = some ((), t)) :
    s.concrete.write tgt p v = some t.concrete := by
  have he := instrument_erasure _ _ _ _ _ h
  dsimp [erase] at he
  cases hc : s.concrete.write tgt p v with
  | none => simp [hc] at he
  | some st => simp [hc] at he; rw [← he]

/-- Helper (computation). Computes the transition or extracts its checked guard; not independent soundness evidence. (approved: PENDING). -/
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

/-- Helper (computation). Computes the transition or extracts its checked guard; not independent soundness evidence. (approved: PENDING). -/
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

/-- Helper (arithmetic). Derived invariant or supplied-preservation fact; not independent soundness evidence. (approved: PENDING). The next identity has never belonged to any currently active loan. Combined with the
unchanged/incremented supply laws, ending a loan cannot make its identity fresh again. -/
theorem nextLoan_fresh (s : State) (hs : WellFormed s) (l : Loan) (hl : l ∈ s.loans) :
    l.id ≠ LoanId.mk s.nextLoan := by
  intro he
  have hb := (hs.2.2.2.1 l hl).1
  rw [he] at hb
  exact Nat.lt_irrefl _ hb

/-- Helper (computation). Computes the transition or extracts its checked guard; not independent soundness evidence. (approved: PENDING). A successful borrow issues the old supply and advances it exactly once. -/
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

/-- Helper (computation). Computes the transition or extracts its checked guard; not independent soundness evidence. (approved: PENDING). A reborrow keeps the original root and extends the parent's projection. -/
theorem reborrow_birth (s t : State) (i : LoanId) (suffix : List Projection)
    (m : Mode) (activation : Nat) (h : reborrowTo s i suffix m activation = some t) :
    t.nextLoan = s.nextLoan + 1 ∧ ∃ a l, loan s i = some a ∧ l ∈ t.loans ∧
      l.id = ⟨s.nextLoan⟩ ∧ l.root = a.root ∧ l.path = a.path ++ suffix ∧ l.parent = some i ∧ a ∈ t.loans := by
  unfold reborrowTo at h
  obtain ⟨a, ha, hh⟩ := Option.bind_eq_some_iff.mp h
  split at hh
  · cases hh
    exact ⟨rfl, a, _, ha, List.mem_cons_self, rfl, rfl, rfl, rfl,
      List.mem_cons_of_mem _ (List.mem_of_find?_eq_some ha)⟩
  · contradiction

/-- Helper (computation). Ending a loan leaves activation frames unchanged. (approved: PENDING). -/
private theorem endLoan_frames (s t : State) (i : LoanId) (h : endLoan s i = some t) :
    t.frames = s.frames := by
  unfold endLoan at h
  obtain ⟨_, _, hh⟩ := Option.bind_eq_some_iff.mp h
  split at hh
  · contradiction
  · cases hh
    rfl

/-- Helper (computation). Computes the transition or extracts its checked guard; not independent soundness evidence. (approved: PENDING). Ending any loan leaves the freshness supply unchanged; an ended identity is not recycled. -/
theorem endLoan_supply (s t : State) (i : LoanId) (h : endLoan s i = some t) :
    t.nextLoan = s.nextLoan := by
  unfold endLoan at h
  obtain ⟨_, _, hh⟩ := Option.bind_eq_some_iff.mp h
  split at hh
  · contradiction
  · cases hh
    rfl

/-- Helper (congruence). Lifts a supplied frame equality through a monadic fold. (approved: PENDING). -/
private theorem fold_frames {α : Type} (f : State → α → Option State)
    (hf : ∀ s a t, f s a = some t → t.frames = s.frames) (xs : List α)
    (s t : State) (h : xs.foldlM f s = some t) : t.frames = s.frames := by
  induction xs generalizing s with
  | nil => simp at h; subst t; rfl
  | cons a xs ih =>
    simp only [List.foldlM_cons] at h
    obtain ⟨u, hu, hh⟩ := Option.bind_eq_some_iff.mp h
    exact (ih u hh).trans (hf s a u hu)

/-- Helper (inversion). Inverts the slot-removal filter for a selected local place. (approved: PENDING). -/
private theorem drop_absent (s t : State) (xs : List Var) (f : Activation)
    (hf : s.frames.head? = some f) (h : drop xs s = some t)
    (p : Hybrid.Place) (hp : p ∈ List.map Prod.snd (f.locals.filter fun e => decide (e.1 ∈ xs))) :
    ∀ x ∈ t.slots, x.place ≠ p := by
  unfold drop at h
  rw [hf] at h
  dsimp at h
  split at h
  · cases h
    intro x hx he
    simp only [List.mem_filter, decide_eq_true_eq] at hx
    exact hx.2 (he ▸ hp)
  · contradiction

/-- Helper (inversion). Structural result proof for the operation-result law. (approved: PENDING). -/
theorem leave_no_local_loan_result (s t : State) (f : Activation)
    (hf : s.frames.head? = some f) (hs : WellFormed s) (h : leave s = some t)
    (entry : Var × Hybrid.Place) (he : entry ∈ f.locals)
    (l : Loan) (hl : l ∈ t.loans) : l.root ≠ entry.2 := by
  have ht := leave_preserves_result s t hs h
  unfold leave at h
  rw [hf] at h
  dsimp at h
  obtain ⟨u, hu, hh⟩ := Option.bind_eq_some_iff.mp h
  have hframes := fold_frames _ (fun s l t hh => endLoan_frames s t l.id hh) _ s u hu
  obtain ⟨v, hv, hh⟩ := Option.bind_eq_some_iff.mp hh
  split at hh
  · contradiction
  rename_i _
  have heq := Option.some.inj hh
  have hp : entry.2 ∈ List.map Prod.snd (f.locals.filter fun e =>
      decide (e.1 ∈ List.map Prod.fst f.locals)) := by
    apply List.mem_map.mpr
    exact ⟨entry, List.mem_filter.mpr ⟨he, by simp; exact ⟨entry.2, he⟩⟩, rfl⟩
  have habsent := drop_absent u v _ f (hframes ▸ hf) hv entry.2 hp
  intro hroot
  obtain ⟨x, hx, hxroot, _⟩ := loan_has_owned_root t ht l hl
  have hxv : x ∈ v.slots := by
    rw [← heq] at hx
    exact (List.mem_filter.mp hx).1
  exact habsent x hxv (hxroot.trans hroot)

/-- Helper (projection). Extracts exactly the pending-argument witness checked by adoption. (approved: PENDING). -/
theorem adoptDown_only_arguments (s t : State) (args : List (Var × LoanId))
    (h : adoptDown s args = some t) (e : Var × LoanId) (he : e ∈ args) :
    ∃ l ∈ s.loans, l.id = e.2 ∧ l.pendingArgument = true := by
  unfold adoptDown at h
  obtain ⟨_, _, hh⟩ := Option.bind_eq_some_iff.mp h
  split at hh
  · rename_i guard
    exact guard.2.2.2.1 e he
  · contradiction

/-- Helper (inversion). Structural result proof for the operation-result law. (approved: PENDING). -/
theorem leave_no_activation_slot_result (s t : State) (f : Activation)
    (hf : s.frames.head? = some f) (h : leave s = some t) :
    ∀ x ∈ t.slots, x.activation ≠ f.id := by
  unfold leave at h
  rw [hf] at h
  dsimp at h
  obtain ⟨_, _, hh⟩ := Option.bind_eq_some_iff.mp h
  obtain ⟨v, _, hh⟩ := Option.bind_eq_some_iff.mp hh
  split at hh
  · contradiction
  · cases hh
    intro x hx
    simpa using (List.mem_filter.mp hx).2

/-- Helper (projection). Extracts exactly the active-current-frame destination checked by reborrowing. (approved: PENDING). -/
theorem reborrowTo_current (s t : State) (i : LoanId) (suffix : List Projection)
    (m : Mode) (activation : Nat) (h : reborrowTo s i suffix m activation = some t) :
    ∃ f ∈ s.frames, f.id = activation ∧ s.frames.head? = some f := by
  unfold reborrowTo at h
  obtain ⟨a, _, h⟩ := Option.bind_eq_some_iff.mp h
  split at h
  · rename_i guard
    have hc := (of_decide_eq_true (Bool.and_eq_true_iff.mp guard).2).2
    obtain ⟨f, hf, he⟩ := Option.map_eq_some_iff.mp hc
    exact ⟨f, List.mem_of_head? hf, he, hf⟩
  · contradiction

/-- Helper (inversion). Structural successful-result proof used by the operation-result law. (approved: PENDING). A direct call argument adds a parentless pending root loan and preserves ownership. -/
theorem passOwner_preserves_result (s t : CallState) (p : Hybrid.Place) (path : List Projection)
    (m : Mode) (x : Var) (hs : WellFormed s.own)
    (h : passOwner s p path m x = some t) : WellFormed t.own := by
  unfold passOwner at h
  split at h
  · contradiction
  · obtain ⟨f, _, h⟩ := Option.bind_eq_some_iff.mp h
    split at h
    · rename_i guard
      cases h
      obtain ⟨access, issued⟩ := Bool.and_eq_true_iff.mp guard
      obtain ⟨root, compat⟩ := owner_guard s.own p m access
      apply insert_loan s.own _ hs rfl (of_decide_eq_true issued) root (by rfl) (by simp)
      intro b hb
      rcases compat b hb with hn | hm
      · exact Or.inl (Ne.symm hn)
      · exact Or.inr (Or.inr (Or.inr (Or.inr hm)))
    · contradiction

/-- Helper (inversion). Structural successful-result proof used by the operation-result law. (approved: PENDING). Direct owner argument evaluation preserves synchronized call metadata as well as ownership. -/
theorem passOwner_call_preserves_result (s t : CallState) (p : Hybrid.Place)
    (path : List Projection) (m : Mode) (x : Var) (hs : CallState.WellFormed s)
    (h : passOwner s p path m x = some t) : CallState.WellFormed t := by
  have hw := passOwner_preserves_result s t p path m x hs.1 h
  unfold passOwner at h
  split at h
  · contradiction
  · rename_i unique
    obtain ⟨f, _, h⟩ := Option.bind_eq_some_iff.mp h
    split at h
    · cases h
      have fresh : ∀ l ∈ s.own.loans, l.id ≠ LoanId.mk s.own.nextLoan := by
        intro l hl he
        have := (hs.1.2.2.2.1 l hl).1
        rw [he] at this
        exact Nat.lt_irrefl _ this
      have pendingFresh : LoanId.mk s.own.nextLoan ∉ s.pending.map Prod.snd := by
        intro hm
        obtain ⟨e, he, hid⟩ := List.mem_map.mp hm
        obtain ⟨l, hl, hh⟩ := hs.2.2.2.1 e he
        exact fresh l hl (hh.trans hid)
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
          exact ⟨l, List.mem_cons_of_mem _ hl, hid⟩
        · simp only [List.mem_singleton] at he
          subst e
          exact ⟨_, List.mem_cons_self, rfl⟩
      · intro l hl
        rcases List.mem_cons.mp hl with rfl | hl
        · simp
        · simpa [List.map_append, fresh l hl] using hs.2.2.2.2 l hl
    · contradiction

/-- Helper (inversion). Structural result proof for the operation-result law. (approved: PENDING). -/
theorem passOwner_freezes_result (s t : CallState) (p : Hybrid.Place) (path : List Projection)
    (m : Mode) (x : Var) (h : passOwner s p path m x = some t) :
    ownerAccess t.own p .mutable = false ∧
      (m = .mutable → ownerAccess t.own p .shared = false) := by
  unfold passOwner at h
  split at h
  · contradiction
  · obtain ⟨f, _, h⟩ := Option.bind_eq_some_iff.mp h
    split at h
    · cases h
      cases m <;> simp [ownerAccess, show (Mode.mutable == Mode.shared) = false from rfl]
    · contradiction

/-- Helper (computation). Computes the transition or extracts its checked guard; not independent soundness evidence. (approved: PENDING). Direct argument loans have no caller parent; the caller loan table is retained as a tail. -/
theorem passOwner_birth (s t : CallState) (p : Hybrid.Place) (path : List Projection)
    (m : Mode) (x : Var) (h : passOwner s p path m x = some t) :
    ∃ l, t.own.loans = l :: s.own.loans ∧ l.id = LoanId.mk s.own.nextLoan ∧
      l.root = p ∧ l.path = path ∧ l.mode = m ∧ l.parent = none ∧
      l.pendingArgument = true ∧ l.callRoot = true := by
  unfold passOwner at h
  split at h
  · contradiction
  · obtain ⟨f, _, h⟩ := Option.bind_eq_some_iff.mp h
    split at h
    · cases h; exact ⟨_, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩
    · contradiction

/-- Helper (inversion). Unique loan identities identify the full loan record. (approved: PENDING). -/
private theorem loan_unique (xs : List Loan) (hn : (xs.map Loan.id).Nodup)
    (x y : Loan) (hx : x ∈ xs) (hy : y ∈ xs) (he : x.id = y.id) : x = y := by
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

/-- Helper (inversion). Ending a list of other identities retains the entire loan record. (approved: PENDING). -/
private theorem endLoans_retains (xs : List Loan) (l : Loan)
    (different : ∀ a ∈ xs, l.id ≠ a.id) (s t : State) (hl : l ∈ s.loans)
    (h : xs.foldlM (fun s a => endLoan s a.id) s = some t) : l ∈ t.loans := by
  induction xs generalizing s with
  | nil => simp at h; subst t; exact hl
  | cons a xs ih =>
    simp only [List.foldlM_cons] at h
    obtain ⟨u, hu, h⟩ := Option.bind_eq_some_iff.mp h
    apply ih (fun a ha => different a (List.mem_cons_of_mem _ ha)) u ?_ h
    unfold endLoan at hu
    obtain ⟨b, _, hu⟩ := Option.bind_eq_some_iff.mp hu
    split at hu
    · contradiction
    · cases hu
      exact List.mem_filter.mpr ⟨hl, by simpa using different a List.mem_cons_self⟩

/-- Helper (inversion). Structural result proof for the operation-result law. (approved: PENDING). -/
theorem leave_retains_caller_result (s t : State) (f : Activation) (l : Loan)
    (hs : WellFormed s) (hf : s.frames.head? = some f) (hl : l ∈ s.loans)
    (caller : l.activation ≠ f.id) (h : leave s = some t) : l ∈ t.loans := by
  unfold leave at h
  rw [hf] at h
  dsimp at h
  obtain ⟨u, hu, h⟩ := Option.bind_eq_some_iff.mp h
  have retained : l ∈ u.loans := by
    apply endLoans_retains _ l ?_ s u hl hu
    intro a ha he
    obtain ⟨ha, act⟩ := List.mem_filter.mp ha
    have eq := loan_unique s.loans hs.2.1 l a hl ha he
    subst a
    exact caller (by simpa using act)
  obtain ⟨v, hv, h⟩ := Option.bind_eq_some_iff.mp h
  rw [← drop_loans u v _ hv] at retained
  split at h
  · contradiction
  · cases h; exact retained

/-- Helper (inversion). Ending loans removes every scheduled identity and never adds loans. (approved: PENDING). -/
private theorem endLoans_removed (xs : List Loan) (s t : State)
    (h : xs.foldlM (fun s a => endLoan s a.id) s = some t) :
    ∀ l ∈ t.loans, l ∈ s.loans ∧ ∀ a ∈ xs, l.id ≠ a.id := by
  induction xs generalizing s with
  | nil => simp at h; subst t; simp
  | cons a xs ih =>
    simp only [List.foldlM_cons] at h
    obtain ⟨u, hu, h⟩ := Option.bind_eq_some_iff.mp h
    intro l hl
    obtain ⟨hl, rest⟩ := ih u h l hl
    unfold endLoan at hu
    obtain ⟨b, _, hu⟩ := Option.bind_eq_some_iff.mp hu
    split at hu
    · contradiction
    · cases hu
      obtain ⟨hs, hn⟩ := List.mem_filter.mp hl
      refine ⟨hs, ?_⟩
      intro b hb
      rcases List.mem_cons.mp hb with rfl | hb
      · simpa using hn
      · exact rest b hb

/-- Helper (inversion). Structural result proof for the operation-result law. (approved: PENDING). -/
theorem leave_no_activation_loan_result (s t : State) (f : Activation)
    (hf : s.frames.head? = some f) (h : leave s = some t) :
    ∀ l ∈ t.loans, l.activation ≠ f.id := by
  unfold leave at h
  rw [hf] at h
  dsimp at h
  obtain ⟨u, hu, h⟩ := Option.bind_eq_some_iff.mp h
  obtain ⟨v, hv, h⟩ := Option.bind_eq_some_iff.mp h
  split at h
  · contradiction
  · cases h
    intro l hl he
    rw [drop_loans u v _ hv] at hl
    obtain ⟨hs, removed⟩ := endLoans_removed _ s u hu l hl
    exact removed l (List.mem_filter.mpr ⟨hs, by simpa using he⟩) rfl

/-- Helper (computation). Computes the transition or extracts its checked guard; not independent soundness evidence. (approved: PENDING). Registering a deferred call preserves the entire store: it neither ends existing loans
nor evaluates late-read arguments. The ordinary call at execution time opens/adopts its loan. -/
theorem defer_keeps_store (O : Exec.Operands) (P : Exec.Program O)
    (f : FnId) (args : List O.Expr) (site : SiteId) (L : AList Var O.Val)
    (s : O.Store) (k : List (Exec.Frame O)) (c : Exec.Config O)
    (h : Exec.step P ⟨.exec (.defer f args site), L, s, k⟩ = some (.next c)) :
    c.store = s := by
  simp only [Exec.step] at h
  obtain ⟨k', _, h⟩ := Option.map_eq_some_iff.mp h
  cases h
  rfl

namespace RoundThreeWitnesses
open ConcreteSpec.Core.Own.Examples
/-- Observation. A projected owner loan issues identity zero. -/
example : borrow initial ⟨0⟩ [.index 0] .shared = some shared ∧
    shared.nextLoan = initial.nextLoan + 1 ∧ ∃ l ∈ shared.loans,
      l.id = LoanId.mk initial.nextLoan ∧ l.root = ⟨0⟩ ∧ l.path = [.index 0] := by decide
/-- Rejection observation. Wrong projection falsifies the existential birth conclusion. -/
example : ¬ (∃ l ∈ shared.loans, l.id = LoanId.mk initial.nextLoan ∧
    l.root = ⟨0⟩ ∧ l.path = [.index 99]) := by decide
/-- Observation. The child retains its parent's root, extended path and parent identity. -/
example : reborrowTo exclusive ⟨0⟩ [.index 1] .mutable 0 = some child ∧
    child.nextLoan = exclusive.nextLoan + 1 ∧ ∃ a l, loan exclusive ⟨0⟩ = some a ∧
      l ∈ child.loans ∧ l.id = LoanId.mk exclusive.nextLoan ∧ l.root = a.root ∧
      l.path = a.path ++ [.index 1] ∧ l.parent = some ⟨0⟩ := by
  simp [Examples.exclusive, Examples.child, Examples.initial, reborrow, reborrowTo, borrow,
    ownerAccess, loanAccess, slot, loan]
/-- Rejection observation. Removing the child's parent field falsifies the birth conclusion itself. -/
example : ¬ (∃ a l, loan exclusive ⟨0⟩ = some a ∧
    l ∈ (child.loans.map fun l => { l with parent := none }) ∧
    l.id = LoanId.mk exclusive.nextLoan ∧ l.root = a.root ∧
    l.path = a.path ++ [.index 1] ∧ l.parent = some ⟨0⟩) := by
  simp [Examples.exclusive, Examples.initial, borrow, ownerAccess, slot, loan]
/-- Observation. Ending a child preserves the nonzero supply. -/
example : ∃ t, endLoan child ⟨1⟩ = some t ∧ t.nextLoan = child.nextLoan := by decide
/-- Rejection observation. A mutated end decrements the supply. -/
example : ¬ (({ child with nextLoan := child.nextLoan - 1 } : State).nextLoan =
    child.nextLoan) := by decide

/-- A callee owns a Copy local and borrows it; an unrelated caller loan survives its exit. -/
def localFrame : State := (enter (fun _ => some ⟨.int .i32, true, true⟩) [y] shared).getD initial
def localLoan : State := (borrow localFrame ⟨1⟩ [] .shared).getD initial
def exited : State := (leave localLoan).getD initial
/-- Observation. The departing local had a live loan; the caller's loan survives. -/
example : WellFormed localLoan ∧ leave localLoan = some exited ∧
    (y, Hybrid.Place.mk 1) ∈ (localLoan.frames.headD ⟨99, [], []⟩).locals ∧
    (∃ l ∈ localLoan.loans, l.root = ⟨1⟩) ∧
    (∃ l ∈ exited.loans, l.root = ⟨0⟩) ∧
    (∀ l ∈ exited.loans, l.root ≠ Hybrid.Place.mk 1) ∧
    (∀ x ∈ exited.slots, x.activation ≠ 1) := by decide
/-- Rejection observation. Exit retaining the local's loan fails the root exclusion conclusion. -/
example : ¬ (∀ l ∈ localLoan.loans, l.root ≠ Hybrid.Place.mk 1) := by decide
/-- Rejection observation. Exit retaining a departing slot fails activation exclusion. -/
example : ¬ (∀ x ∈ localLoan.slots, x.activation ≠ 1) := by decide
/-- Observation. A child marked during argument evaluation can be adopted. -/
example : (callLife types (.enter [y]) CallWitnesses.waiting).isSome = true ∧
    ∃ l ∈ CallWitnesses.waiting.own.loans, l.id = LoanId.mk 1 ∧
      l.pendingArgument = true := by decide
/-- Rejection observation. Adopting a normal caller loan lacks the required pending witness. -/
example : ¬ (∃ l ∈ exclusive.loans, l.id = LoanId.mk 0 ∧
    l.pendingArgument = true) := by decide
/-- Rejection observation. An untyped parameter refuses entry. -/
example : ¬ (enter (fun _ => none) [y] shared).isSome = true := by decide
/-- Observation. A typed fresh parameter enters over a borrowed caller. -/
example : (enter types [y] shared).isSome = true := by decide
/-- Observation. Lending downward after entry targets the newly active callee. -/
example : (reborrowTo LifecycleWitnesses.called ⟨1⟩ [] .shared 1).isSome = true ∧
    ∃ f ∈ LifecycleWitnesses.called.frames, f.id = 1 ∧
      LifecycleWitnesses.called.frames.head? = some f := by decide
/-- Refused: activation 1 cannot lend upward into its caller, activation 0. -/
example : reborrowTo LifecycleWitnesses.called ⟨1⟩ [] .shared 0 = none := by decide
/-- Rejection observation. The former arbitrary-destination rule admits upward lending, whose destination is not current. -/
example : ¬ (∃ f ∈ LifecycleWitnesses.called.frames, f.id = 0 ∧
    LifecycleWitnesses.called.frames.head? = some f) := by decide
/-- Rejection observation. A rule that issues the wrong birth counter fails the supply conclusion. -/
example : ¬ (({ shared with nextLoan := 7 } : State).nextLoan = initial.nextLoan + 1) := by decide
/-- Rejection observation. Failing to append the requested suffix falsifies the path conclusion. -/
example : ¬ (∃ l ∈ child.loans, l.id = LoanId.mk exclusive.nextLoan ∧
    l.path = [.index 0, .index 99]) := by decide
/-- Observation. Reborrowing retains the original parent record. -/
example : ∃ a, loan exclusive ⟨0⟩ = some a ∧ a ∈ child.loans := by
  simp [Examples.exclusive, Examples.child, Examples.initial, reborrow, reborrowTo,
    borrow, ownerAccess, loanAccess, slot, loan]
/-- Rejection observation. Removing the parent now falsifies the strengthened retention conclusion. -/
example : ¬ (∃ a, loan exclusive ⟨0⟩ = some a ∧
    a ∈ child.loans.filter (·.id != LoanId.mk 0)) := by
  simp [Examples.exclusive, Examples.child, Examples.initial, reborrow, reborrowTo,
    borrow, ownerAccess, loanAccess, slot, loan]
end RoundThreeWitnesses

/-- Law (subject `ConcreteSpec.Core.Own.passOwner`). A direct owner call with no intervening operation restores the caller's complete slot
and loan tables. Only the fresh identity supplies advance. Thus every owner permission and
every pre-existing caller loan is exactly as before the call. -/
theorem directOwner_roundtrip (s : State) (p : Hybrid.Place) (path : List Projection)
    (m : Mode) (x : Var) (types : Var → Option Descriptor) (f : Activation)
    (hs : WellFormed s) (hf : s.frames.head? = some f)
    (issued : f.id < s.nextActivation) (allowed : ownerAccess s p m = true) :
    (do
      let waiting ← passOwner ⟨s, []⟩ p path m x
      let called ← callLife types (.enter [x]) waiting
      callLife types .leave called) =
    some ⟨{ s with nextLoan := s.nextLoan + 1, nextActivation := s.nextActivation + 1 }, []⟩ := by
  have fresh : ∀ l ∈ s.loans, l.id ≠ LoanId.mk s.nextLoan := by
    intro l hl he
    have hb := (hs.2.2.2.1 l hl).1
    rw [he] at hb
    exact Nat.lt_irrefl _ hb
  have oldAct : ∀ l ∈ s.loans, l.activation ≠ s.nextActivation := by
    intro l hl he
    have hb := (hs.2.2.2.1 l hl).2.1
    omega
  have oldSlots : ∀ a ∈ s.slots, a.activation ≠ s.nextActivation := by
    intro a ha he
    have hb := (hs.2.2.1 a ha).2
    omega
  have noChild : ∀ l ∈ s.loans, l.parent ≠ some (LoanId.mk s.nextLoan) := by
    intro l hl he
    have hp := (hs.2.2.2.1 l hl).2.2.2.1
    unfold ParentValid at hp
    rw [he] at hp
    obtain ⟨a, ha, hid, _⟩ := hp
    exact fresh a ha hid
  have noChildBool : s.loans.any (·.parent == some (LoanId.mk s.nextLoan)) = false := by
    apply Bool.eq_false_iff.mpr
    intro h
    obtain ⟨l, hl, he⟩ := List.any_eq_true.mp h
    exact noChild l hl (by simpa using he)
  have filterId : s.loans.filter (·.id != LoanId.mk s.nextLoan) = s.loans := by
    apply List.filter_eq_self.mpr
    intro l hl
    simpa using fresh l hl
  have filterAct : s.loans.filter (·.activation == s.nextActivation) = [] := by
    apply List.filter_eq_nil_iff.mpr
    intro l hl
    simpa using oldAct l hl
  have slotsBool : s.slots.any (·.activation == s.nextActivation) = false := by
    apply Bool.eq_false_iff.mpr
    intro h
    obtain ⟨a, ha, he⟩ := List.any_eq_true.mp h
    exact oldSlots a ha (by simpa using he)
  have mapOld : (s.loans.map fun l => if l.id = LoanId.mk s.nextLoan then
      { l with activation := s.nextActivation, pendingArgument := false } else l) = s.loans := by
    calc
      _ = s.loans.map id := by
        apply List.map_congr_left
        intro l hl
        simp [fresh l hl]
      _ = s.loans := List.map_id _
  have noChildExists : ¬ ∃ l ∈ s.loans, l.parent = some (LoanId.mk s.nextLoan) := by
    rintro ⟨l, hl, he⟩
    exact noChild l hl he
  simp [passOwner, hf, issued, allowed, callLife, enter, adoptDown, life, leave,
    endLocals, endLoan, drop, loan, AList.lookup, mapOld, filterAct, noChildExists,
    filterId]
  exact ⟨⟨fun a ha he _ => False.elim (oldSlots a ha he),
    fun _ _ a ha he => False.elim (oldSlots a ha he)⟩, oldSlots⟩

namespace DirectArgumentWitnesses
open ConcreteSpec.Core.Own.Examples

def z : Var := ⟨"z", 2⟩
def caller : CallState := ⟨{ shared with
  slots := ⟨⟨1⟩, 0, ⟨.int .i32, false, true⟩, .owned⟩ :: shared.slots
  frames := [⟨0, [(x, ⟨0⟩), (y, ⟨1⟩)], []⟩]
  nextPlace := 2 }, []⟩
def waiting : CallState := (passOwner caller ⟨1⟩ [.index 3] .mutable z).getD caller
def called : CallState := (callLife types (.enter [z]) waiting).getD caller
def returned : CallState := (callLife types .leave called).getD caller

/-- Observation. A direct mutable argument coexists with an unrelated caller loan. -/
example : CallState.WellFormed caller ∧ passOwner caller ⟨1⟩ [.index 3] .mutable z = some waiting ∧
    CallState.WellFormed waiting := by decide
/-- Rejection observation. A mutated direct loan reuses the birth supply. -/
example : ¬ WellFormed { waiting.own with nextLoan := 1 } := by decide
/-- Observation. Direct lending has no caller-side parent. -/
example : ∃ l, waiting.own.loans = l :: caller.own.loans ∧ l.id = LoanId.mk caller.own.nextLoan ∧
    l.root = Hybrid.Place.mk 1 ∧ l.path = [.index 3] ∧ l.mode = .mutable ∧
    l.parent = none ∧ l.pendingArgument = true ∧ l.callRoot = true := by
  exact passOwner_birth caller waiting ⟨1⟩ [.index 3] .mutable z (by decide)
/-- Rejection observation. A child of the caller loan cannot be the required parentless root. -/
example : ¬ (∃ l ∈ child.loans, l.id = LoanId.mk 1 ∧ l.parent = none) := by decide
/-- Observation. Argument evaluation freezes both reads and writes before entry. -/
example : ownerAccess waiting.own ⟨1⟩ .mutable = false ∧
    ownerAccess waiting.own ⟨1⟩ .shared = false := by decide
/-- Rejection observation. Removing the loan prematurely fails the freeze conclusion. -/
example : ¬ (ownerAccess { waiting.own with loans := caller.own.loans } ⟨1⟩ .mutable = false) := by decide
/-- Observation. Entry adopts the direct root and return ends it; its owner is immediately writable. -/
example : callLife types (.enter [z]) waiting = some called ∧
    callLife types .leave called = some returned ∧
    (∃ l ∈ called.own.loans, l.callRoot = true ∧ l.activation = 1) ∧
    (∀ l ∈ returned.own.loans, l.activation ≠ 1) ∧
    ownerAccess returned.own ⟨1⟩ .mutable = true := by decide
/-- Rejection observation. A return that never ends its direct argument retains a callee loan and freezes the owner. -/
example : ¬ (∀ l ∈ called.own.loans, l.activation ≠ 1) := by decide
/-- Observation. The original caller loan is unchanged after return. -/
example : WellFormed called.own ∧
    (∀ l ∈ caller.own.loans, l ∈ called.own.loans ∧ l ∈ returned.own.loans) := by decide
/-- Rejection observation. Ending every loan would destroy the pre-existing caller loan. -/
example : ¬ (∀ l ∈ caller.own.loans, l ∈ ([] : List Loan)) := by decide

/-- Observation. Registering a deferred call keeps a live direct root loan unchanged. -/
example : ∃ c : Exec.Config callOps,
    Exec.step callProgram ⟨.exec (.defer callee [.ownerArgument ⟨1⟩ .mutable z] ⟨0⟩),
      [], waiting, [.scope ⟨0⟩ [] []]⟩ = some (.next c) ∧ c.store = waiting := by
  exact ⟨_, rfl, rfl⟩
/-- Rejection observation. Ending the call loan at registration changes the store and unfreezes the owner prematurely. -/
example : ¬ ({ waiting with own := { waiting.own with loans := caller.own.loans } } = waiting) := by decide

/-- Accepted: a real C1 direct owner call returns, and the next operand reads the owner. -/
example : (match (Exec.run callProgram (fun _ p => nomatch p) 100
    ⟨.exec (.seq (.call none callee [.ownerArgument ⟨0⟩ .mutable y] ⟨0⟩)
      (.cond (.value (.read x)) .skip .skip ⟨0⟩)), [(x, 7)], ⟨initial, []⟩, []⟩).ending with
    | .final .normal => true
    | _ => false) = true := by decide
/-- Accepted: deferred direct lending uses late-read arguments; its call loan ends only when
that call returns, and the caller reads its owner after cleanup. -/
example : (match (Exec.run callProgram (fun _ p => nomatch p) 100
    ⟨.exec (.seq (.block ⟨0⟩ (.defer callee [.ownerArgument ⟨0⟩ .mutable y] ⟨0⟩))
      (.cond (.value (.read x)) .skip .skip ⟨0⟩)), [(x, 7)], ⟨initial, []⟩, []⟩).ending with
    | .final .normal => true
    | _ => false) = true := by decide
/-- Observation. A call restores two caller owners and a pre-existing caller loan. -/
example : (do
    let waiting ← passOwner caller ⟨1⟩ [.index 3] .mutable z
    let called ← callLife types (.enter [z]) waiting
    callLife types .leave called) = some returned ∧
    returned = ⟨{ caller.own with nextLoan := caller.own.nextLoan + 1, nextActivation := caller.own.nextActivation + 1 }, []⟩ := by decide
/-- Rejection observation. Keeping the call loan forever fails exact restoration of the caller tables. -/
example : ¬ (called.own.loans = caller.own.loans) := by decide
end DirectArgumentWitnesses

/-- Helper (inversion). Structural result proof for the operation-result law. (approved: PENDING). -/
theorem reborrowTo_live_result (s t : State) (i : LoanId) (path : List Projection)
    (m : Mode) (activation : Nat) (hs : LiveDestinations s)
    (h : reborrowTo s i path m activation = some t) : LiveDestinations t := by
  obtain ⟨f, hf, he, _⟩ := reborrowTo_current s t i path m activation h
  have live : activation ∈ s.frames.map Activation.id :=
    List.mem_map.mpr ⟨f, hf, he⟩
  unfold reborrowTo at h
  obtain ⟨a, _, h⟩ := Option.bind_eq_some_iff.mp h
  split at h
  · cases h
    intro l hl
    rcases List.mem_cons.mp hl with rfl | hl
    · exact live
    · exact hs l hl
  · contradiction

/-- Helper (inversion). Structural result proof for the operation-result law. (approved: PENDING). -/
theorem adoptDown_live_result (s t : State) (args : List (Var × LoanId))
    (hs : LiveDestinations s) (h : adoptDown s args = some t) : LiveDestinations t := by
  unfold adoptDown at h
  obtain ⟨f, hf, h⟩ := Option.bind_eq_some_iff.mp h
  cases hframes : s.frames with
  | nil => simp [hframes] at hf
  | cons a rest =>
    simp only [hframes, List.head?_cons, Option.some.injEq] at hf
    subst a
    split at h
    · cases h
      intro l hl
      obtain ⟨a, ha, rfl⟩ := List.mem_map.mp hl
      split
      · simp
      · simpa [hframes] using hs a ha
    · contradiction

/-- Helper (projection). Successful return pops exactly the current activation frame. (approved: PENDING). -/
private theorem leave_frames_proj (s t : State) (f : Activation)
    (hf : s.frames.head? = some f) (h : leave s = some t) :
    t.frames = s.frames.tail := by
  unfold leave at h
  rw [hf] at h
  dsimp at h
  obtain ⟨u, hu, h⟩ := Option.bind_eq_some_iff.mp h
  have frames := fold_frames _ (fun s l t hh => endLoan_frames s t l.id hh) _ s u hu
  obtain ⟨v, hv, h⟩ := Option.bind_eq_some_iff.mp h
  unfold drop at hv
  rw [frames, hf] at hv
  dsimp at hv
  split at hv
  · cases hv
    split at h
    · contradiction
    · cases h
      simp
  · contradiction

/-- Helper (inversion). Structural result proof for the operation-result law. (approved: PENDING). -/
theorem leave_live_result (s t : State) (f : Activation) (hs : LiveDestinations s)
    (hf : s.frames.head? = some f) (h : leave s = some t) : LiveDestinations t := by
  intro l hl
  have old := life_subset (fun _ => none) .leave s t h l hl
  have live := hs l old
  have other := leave_no_activation_loan_result s t f hf h l hl
  rw [leave_frames_proj s t f hf h]
  cases frames : s.frames with
  | nil => simp [frames] at hf
  | cons a rest =>
    simp only [frames, List.head?_cons, Option.some.injEq] at hf
    subst a
    simpa [frames, other] using live

namespace LiveDestinationWitnesses
open ConcreteSpec.Core.Own.Examples
/-- Observation. A mutable loan and child target their active caller frame. -/
example : LiveDestinations exclusive ∧ LiveDestinations child ∧
    reborrowTo exclusive ⟨0⟩ [.index 1] .mutable 0 = some child := by decide
/-- Rejection observation. Targeting an inactive but previously issued activation breaks actual liveness. -/
example : ¬ LiveDestinations { child with
    nextActivation := 3
    loans := child.loans.map fun l => { l with activation := 2 } } := by decide
/-- Observation. A pending direct root becomes a loan of the active callee. -/
example : LiveDestinations DirectArgumentWitnesses.waiting.own ∧
    LiveDestinations DirectArgumentWitnesses.called.own := by decide
/-- Rejection observation. Adoption into an inactive destination violates the conclusion. -/
example : ¬ LiveDestinations { DirectArgumentWitnesses.called.own with
    loans := DirectArgumentWitnesses.called.own.loans.map fun l => { l with activation := 7 } } := by decide
/-- Observation. Return retains the caller loan and its live destination. -/
example : LiveDestinations DirectArgumentWitnesses.called.own ∧
    LiveDestinations DirectArgumentWitnesses.returned.own := by decide
/-- Rejection observation. Popping the callee without ending its root loan strands that loan. -/
example : ¬ LiveDestinations { DirectArgumentWitnesses.called.own with
    frames := DirectArgumentWitnesses.called.own.frames.tail } := by decide
end LiveDestinationWitnesses

section ExecErasure
variable {O : Exec.Operands} {types : Var → Option Descriptor}
  {permit : AList Var O.Val → O.Expr → State → Option State}

/-- Helper (computation). Computes the transition or extracts its checked guard; not independent soundness evidence. (approved: PENDING). Operand evaluation forward simulation (Q9.10, owner decision 2026-11-08 Q3): a
successful instrumented operand evaluation evaluates to the same value and concrete store
in the erased operand instance. -/
theorem eval_ok_erasure (L : AList Var O.Val) (s t : Store O.Store) (e : O.Expr) (v : O.Val)
    (h : (instrumentOperands O types permit).eval L s e = .ok (v, t)) :
    O.eval L (erase s) e = .ok (v, erase t) := by
  dsimp [instrumentOperands, instrumentEval] at h
  cases hp : permit L e s.own with
  | none => rw [hp] at h; cases h
  | some own =>
    rw [hp] at h
    cases he : O.eval L s.concrete e with
    | ok r =>
      rcases r with ⟨w, concrete⟩
      rw [he] at h
      cases h
      exact he
    | trap _ => rw [he] at h; cases h
    | stuck => rw [he] at h; cases h

/-- Helper (computation). Computes the transition or extracts its checked guard; not independent soundness evidence. (approved: PENDING). Operand trap forward simulation (Q9.10, owner decision 2026-11-08 Q3): an instrumented
operand evaluation that traps traps with the same kind in the erased operand instance. -/
theorem eval_trap_erasure (L : AList Var O.Val) (s : Store O.Store) (e : O.Expr) (kd : TrapKind)
    (h : (instrumentOperands O types permit).eval L s e = .trap kd) :
    O.eval L (erase s) e = .trap kd := by
  dsimp [instrumentOperands, instrumentEval] at h
  cases hp : permit L e s.own with
  | none => rw [hp] at h; cases h
  | some own =>
    rw [hp] at h
    cases he : O.eval L s.concrete e with
    | ok r => rcases r with ⟨_, _⟩; rw [he] at h; cases h
    | trap _ => rw [he] at h; cases h; exact he
    | stuck => rw [he] at h; cases h

/-- Helper (inversion). Structural induction for the argument-result law. (approved: PENDING). -/
theorem evalArgs_ok_erasure_result (L : AList Var O.Val) :
    ∀ (args : List O.Expr) (s t : Store O.Store) (vs : List O.Val),
      Exec.evalArgs (instrumentOperands O types permit) L s args = .ok (vs, t) →
      Exec.evalArgs O L (erase s) args = .ok (vs, erase t)
  | [], _, _, _, h => by
    cases h
    rfl
  | e :: es, s, t, vs, h => by
    simp only [Exec.evalArgs, instrumentOperands] at h ⊢
    cases he : instrumentEval O permit L s e with
    | ok r =>
      rcases r with ⟨v, s₁⟩
      rw [he] at h
      rw [eval_ok_erasure (O := O) (types := types) L s s₁ e v he]
      dsimp [Res.bind] at h ⊢
      cases hes : Exec.evalArgs (instrumentOperands O types permit) L s₁ es with
      | ok r' =>
        rcases r' with ⟨vs', s₂⟩
        have ih := evalArgs_ok_erasure_result L es s₁ s₂ vs' hes
        simp only [instrumentOperands] at hes
        rw [hes] at h
        rw [ih]
        cases h
        rfl
      | trap _ =>
        simp only [instrumentOperands] at hes
        rw [hes] at h; cases h
      | stuck =>
        simp only [instrumentOperands] at hes
        rw [hes] at h; cases h
    | trap _ => rw [he] at h; cases h
    | stuck => rw [he] at h; cases h

/-- Helper (inversion). Structural induction for the argument-result law. (approved: PENDING). -/
theorem evalArgs_trap_erasure_result (L : AList Var O.Val) :
    ∀ (args : List O.Expr) (s : Store O.Store) (kd : TrapKind),
      Exec.evalArgs (instrumentOperands O types permit) L s args = .trap kd →
      Exec.evalArgs O L (erase s) args = .trap kd
  | [], _, _, h => by cases h
  | e :: es, s, kd, h => by
    simp only [Exec.evalArgs, instrumentOperands] at h ⊢
    cases he : instrumentEval O permit L s e with
    | ok r =>
      rcases r with ⟨v, s₁⟩
      rw [he] at h
      rw [eval_ok_erasure (O := O) (types := types) L s s₁ e v he]
      dsimp [Res.bind] at h ⊢
      cases hes : Exec.evalArgs (instrumentOperands O types permit) L s₁ es with
      | ok r' =>
        rcases r' with ⟨_, _⟩
        simp only [instrumentOperands] at hes
        rw [hes] at h; cases h
      | trap _ =>
        have ih := evalArgs_trap_erasure_result L es s₁ _ hes
        simp only [instrumentOperands] at hes
        rw [hes] at h
        cases h
        rw [ih]
        rfl
      | stuck =>
        simp only [instrumentOperands] at hes
        rw [hes] at h; cases h
    | trap _ =>
      rw [he] at h
      cases h
      rw [eval_trap_erasure (O := O) (types := types) L s e kd he]
      rfl
    | stuck => rw [he] at h; cases h

/-- Helper (computation). Computes the transition or extracts its checked guard; not independent soundness evidence. (approved: PENDING). Condition tests are identical in the instrumented and erased operand instances. -/
theorem test_erasure (v : O.Val) :
    (instrumentOperands O types permit).test v = O.test v := rfl

/-- Helper (computation). Computes the transition or extracts its checked guard; not independent soundness evidence. (approved: PENDING). Lifecycle forward simulation: a successful instrumented lifecycle event performs
the matching concrete lifecycle transition on the erased store. -/
theorem instrumentLife_erasure (ev : Exec.Life) (s t : Store O.Store)
    (h : (instrumentOperands O types permit).life ev s = some t) :
    O.life ev (erase s) = some (erase t) := by
  dsimp [instrumentOperands, instrumentLife] at h
  cases hown : life types ev s.own with
  | none => rw [hown] at h; cases h
  | some own =>
    rw [hown] at h
    cases hc : O.life ev s.concrete with
    | none => rw [hc] at h; cases h
    | some concrete =>
      rw [hc] at h
      cases h
      exact hc

/-- Helper (projection). Extracts the two defining conjuncts of the instrumented answer guard. (approved: PENDING). -/
theorem allows_erasure (p : O.Prim) (vs : List O.Val) (s : Store O.Store)
    (a : Exec.Answer O.Val O.Event (Store O.Store))
    (h : (instrumentOperands O types permit).allows p vs s a = true) :
    O.allows p vs (erase s) (eraseAnswer a) = true ∧ a.store.own = s.own := by
  dsimp [instrumentOperands, instrumentAllows] at h
  obtain ⟨ha, hown⟩ := Bool.and_eq_true_iff.mp h
  exact ⟨ha, of_decide_eq_true hown⟩

/-- Helper (projection). Result-binding lifecycle erases to the concrete `bindLife`. (approved: PENDING). -/
private theorem bindLife_erasure_proj (dst : Option Var) (s t : Store O.Store)
    (h : Exec.bindLife (instrumentOperands O types permit) dst s = some t) :
    Exec.bindLife O dst (erase s) = some (erase t) := by
  cases dst with
  | none => cases h; rfl
  | some x => exact instrumentLife_erasure (.bind x) s t h

/-- Helper (projection). Scope-exit drop erases to the concrete `dropOut`. (approved: PENDING). -/
private theorem dropOut_erasure_proj (dm : List Var) (L : AList Var O.Val) (s t : Store O.Store)
    (h : Exec.dropOut (instrumentOperands O types permit) dm L s = some t) :
    Exec.dropOut O dm L (erase s) = some (erase t) :=
  instrumentLife_erasure (.drop (Exec.dead dm L)) s t h

/-- Helper (projection). Deferred cleanup registration commutes with frame erasure. (approved: PENDING). -/
private theorem register_erasure_proj (d : Exec.Deferred (instrumentOperands O types permit)) :
    ∀ (k k' : List (Exec.Frame (instrumentOperands O types permit))),
      Exec.register d k = some k' →
      Exec.register (eraseDeferred d) (k.map eraseFrame) = some (k'.map eraseFrame)
  | [], _, h => by cases h
  | .seq t :: k, k', h => by
    simp only [Exec.register, Option.map_eq_some_iff] at h
    obtain ⟨k₁, hk₁, rfl⟩ := h
    simp [Exec.register, eraseFrame, register_erasure_proj d k k₁ hk₁]
  | .scope b dom ds :: k, k', h => by
    cases h
    rfl
  | .test .. :: _, _, h => by cases h
  | .body .. :: _, _, h => by cases h
  | .resume _ :: _, _, h => by cases h
  | .ret .. :: _, _, h => by cases h

/-- Helper (projection). Function lookup in an erased program erases the looked-up function. (approved: PENDING). -/
private theorem lookup_eraseProgram_proj :
    ∀ (fns : AList FnId (Exec.Fn (instrumentOperands O types permit))) (f : FnId),
      AList.lookup (List.map (fun e => (e.1, eraseFn e.2)) fns) f =
        (AList.lookup fns f).map eraseFn
  | [], _ => rfl
  | (g, fn) :: fns, f => by
    simp only [List.map_cons, AList.lookup]
    by_cases h : g = f
    · simp [h]
    · simp [h, lookup_eraseProgram_proj fns f]

/-- Helper (projection). Structured exit unwinding commutes with configuration erasure. (approved: PENDING). -/
private theorem stepExit_erasure_proj (L : AList Var O.Val) (s : Store O.Store)
    (e : Exec.Exit O.Val) (k : List (Exec.Frame (instrumentOperands O types permit)))
    (out : Exec.Out (instrumentOperands O types permit))
    (h : Exec.stepExit (O := instrumentOperands O types permit) L s e k = some out) :
    ∃ c', out = .next c' ∧
      Exec.stepExit (O := O) L (erase s) e (k.map eraseFrame) = some (.next (eraseConfig c')) := by
  cases k with
  | nil => cases h
  | cons f k =>
    cases f with
    | seq t =>
      cases h
      exact ⟨_, rfl, rfl⟩
    | scope b dm ds =>
      cases ds with
      | nil =>
        simp only [Exec.stepExit, Option.map_eq_some_iff] at h
        obtain ⟨st', hst', rfl⟩ := h
        refine ⟨_, rfl, ?_⟩
        simp [Exec.stepExit, eraseFrame, dropOut_erasure_proj dm L s st' hst', eraseConfig, eraseCtrl]
      | cons d ds =>
        cases h
        exact ⟨_, rfl, rfl⟩
    | test l cr t body site dm =>
      simp only [Exec.stepExit, Option.map_eq_some_iff] at h
      obtain ⟨st', hst', rfl⟩ := h
      refine ⟨_, rfl, ?_⟩
      simp [Exec.stepExit, eraseFrame, dropOut_erasure_proj dm L s st' hst', eraseConfig, eraseCtrl]
    | body l cr t b site =>
      cases e with
      | brk l' =>
        by_cases hl : l' = l
        · simp only [Exec.stepExit, hl, ↓reduceIte, Option.some.injEq] at h
          subst h
          exact ⟨_, rfl, by simp [Exec.stepExit, eraseFrame, hl, eraseConfig, eraseCtrl]⟩
        · simp only [Exec.stepExit, hl, ↓reduceIte, Option.some.injEq] at h
          subst h
          exact ⟨_, rfl, by simp [Exec.stepExit, eraseFrame, hl, eraseConfig, eraseCtrl]⟩
      | cont l' =>
        by_cases hl : l' = l
        · simp only [Exec.stepExit, hl, ↓reduceIte, Option.some.injEq] at h
          subst h
          exact ⟨_, rfl, by simp [Exec.stepExit, eraseFrame, hl, eraseConfig, eraseCtrl, eraseStmt]⟩
        · simp only [Exec.stepExit, hl, ↓reduceIte, Option.some.injEq] at h
          subst h
          exact ⟨_, rfl, by simp [Exec.stepExit, eraseFrame, hl, eraseConfig, eraseCtrl]⟩
      | ret v =>
        cases h
        exact ⟨_, rfl, rfl⟩
    | resume p => cases h
    | ret dst caller =>
      cases e with
      | brk _ => cases h
      | cont _ => cases h
      | ret v =>
        simp only [Exec.stepExit, Option.bind_eq_some_iff, Option.map_eq_some_iff] at h
        obtain ⟨st', hst', st'', hst'', rfl⟩ := h
        refine ⟨_, rfl, ?_⟩
        dsimp [Exec.stepExit, eraseFrame, eraseConfig, eraseCtrl, List.map]
        rw [instrumentLife_erasure (O := O) (permit := permit) .leave s st' hst']
        dsimp [Option.bind]
        rw [bindLife_erasure_proj (O := O) (permit := permit) dst st' st'' hst'']
        rfl

/-- Law (subject `ConcreteSpec.Core.Own.eraseConfig`). Silent machine-step forward simulation (owner decision 2026-11-08 Q3): whenever `step`
on the instrumented C1 machine produces a next configuration, `step` on the erased machine
produces the erased configuration. -/
theorem step_next_erasure (P : Exec.Program (instrumentOperands O types permit))
    (c c' : Exec.Config (instrumentOperands O types permit))
    (h : Exec.step P c = some (.next c')) :
    Exec.step (eraseProgram P) (eraseConfig c) = some (.next (eraseConfig c')) := by
  rcases c with ⟨ctrl, L, st, k⟩
  cases ctrl with
  | exec s =>
    cases s with
    | skip =>
      cases h
      rfl
    | bind x e site =>
      simp only [Exec.step, eraseConfig, eraseCtrl, eraseStmt, instrumentOperands] at h ⊢
      cases he : instrumentEval O permit L st e with
      | ok r =>
        rcases r with ⟨v, st'⟩
        rw [he] at h
        simp only [Exec.onRes, Option.map_eq_some_iff] at h
        obtain ⟨st'', hst'', hnext⟩ := h
        cases hnext
        rw [eval_ok_erasure (O := O) (types := types) L st st' e v he]
        simp [Exec.onRes,
          instrumentLife_erasure (O := O) (permit := permit) (.bind x) st' st'' hst'']
      | trap kd =>
        rw [he] at h
        cases h
        rw [eval_trap_erasure (O := O) (types := types) L st e kd he]
        simp [Exec.onRes]
      | stuck => rw [he] at h; cases h
    | assign x e site =>
      simp only [Exec.step, eraseConfig, eraseCtrl, eraseStmt, instrumentOperands] at h ⊢
      cases he : instrumentEval O permit L st e with
      | ok r =>
        rcases r with ⟨v, st'⟩
        rw [he] at h
        simp only [Exec.onRes, Option.bind_eq_some_iff, Option.map_eq_some_iff] at h
        obtain ⟨w, hw, st'', hst'', hnext⟩ := h
        cases hnext
        rw [eval_ok_erasure (O := O) (types := types) L st st' e v he]
        simp [Exec.onRes, hw,
          instrumentLife_erasure (O := O) (permit := permit) (.bind x) st' st'' hst'']
      | trap kd =>
        rw [he] at h
        cases h
        rw [eval_trap_erasure (O := O) (types := types) L st e kd he]
        simp [Exec.onRes]
      | stuck => rw [he] at h; cases h
    | seq s t =>
      cases h
      rfl
    | block b s =>
      cases h
      rfl
    | defer f args site =>
      simp only [Exec.step, Option.map_eq_some_iff] at h
      obtain ⟨k', hk', hnext⟩ := h
      cases hnext
      have hreg := register_erasure_proj (O := O) (types := types) (permit := permit) ⟨f, args, site⟩ k k' hk'
      simp only [eraseDeferred] at hreg
      simp [Exec.step, eraseConfig, eraseCtrl, eraseStmt, hreg]
    | cond e s t site =>
      simp only [Exec.step, eraseConfig, eraseCtrl, eraseStmt, instrumentOperands] at h ⊢
      cases he : instrumentEval O permit L st e with
      | ok r =>
        rcases r with ⟨v, st'⟩
        rw [he] at h
        simp only [Exec.onRes, Option.map_eq_some_iff] at h
        obtain ⟨b, hb, hnext⟩ := h
        cases hnext
        rw [eval_ok_erasure (O := O) (types := types) L st st' e v he]
        cases b <;> simp [Exec.onRes, hb]
      | trap kd =>
        rw [he] at h
        cases h
        rw [eval_trap_erasure (O := O) (types := types) L st e kd he]
        simp [Exec.onRes]
      | stuck => rw [he] at h; cases h
    | loop l cr t b site =>
      cases h
      rfl
    | brk l =>
      cases h
      rfl
    | cont l =>
      cases h
      rfl
    | ret e site =>
      simp only [Exec.step, eraseConfig, eraseCtrl, eraseStmt, instrumentOperands] at h ⊢
      cases he : instrumentEval O permit L st e with
      | ok r =>
        rcases r with ⟨v, st'⟩
        rw [he] at h
        cases h
        rw [eval_ok_erasure (O := O) (types := types) L st st' e v he]
        simp [Exec.onRes]
      | trap kd =>
        rw [he] at h
        cases h
        rw [eval_trap_erasure (O := O) (types := types) L st e kd he]
        simp [Exec.onRes]
      | stuck => rw [he] at h; cases h
    | call dst f args site =>
      dsimp [Exec.step, eraseConfig, eraseCtrl, eraseStmt] at h ⊢
      cases he : Exec.evalArgs (instrumentOperands O types permit) L st args with
      | ok r =>
        rcases r with ⟨vs, st'⟩
        rw [he] at h
        simp only [Exec.onRes, Option.bind_eq_some_iff] at h
        obtain ⟨fn, hfn, h⟩ := h
        by_cases hlen : fn.params.length = vs.length
        · simp only [hlen, ↓reduceIte, Option.map_eq_some_iff] at h
          obtain ⟨st'', hst'', hnext⟩ := h
          cases hnext
          rw [evalArgs_ok_erasure_result (O := O) L args st st' vs he]
          dsimp [Exec.onRes, eraseProgram]
          rw [lookup_eraseProgram_proj P.fns f, hfn]
          dsimp [Option.bind, eraseFn]
          simp only [hlen, ↓reduceIte]
          rw [instrumentLife_erasure (O := O) (permit := permit)
            (.enter (fn.params.map Prod.fst)) st' st'' hst'']
          rfl
        · simp only [hlen, ↓reduceIte] at h
          cases h
      | trap kd =>
        rw [he] at h
        cases h
        rw [evalArgs_trap_erasure_result (O := O) L args st kd he]
        rfl
      | stuck => rw [he] at h; cases h
    | prim x p args site =>
      dsimp [Exec.step, eraseConfig, eraseCtrl, eraseStmt] at h ⊢
      cases he : Exec.evalArgs (instrumentOperands O types permit) L st args with
      | ok r =>
        rcases r with ⟨_, _⟩
        rw [he] at h
        cases h
      | trap kd =>
        rw [he] at h
        cases h
        rw [evalArgs_trap_erasure_result (O := O) L args st kd he]
        rfl
      | stuck => rw [he] at h; cases h
    | abort site =>
      cases h
      rfl
  | done =>
    cases k with
    | nil => cases h
    | cons f k =>
      cases f with
      | seq t =>
        cases h
        rfl
      | scope b dm ds =>
        cases ds with
        | nil =>
          simp only [Exec.step, Option.map_eq_some_iff] at h
          obtain ⟨st', hst', hnext⟩ := h
          cases hnext
          dsimp [Exec.step, eraseConfig, eraseCtrl, eraseFrame, List.map]
          rw [dropOut_erasure_proj (O := O) (permit := permit) dm L st st' hst']
          rfl
        | cons d ds =>
          cases h
          rfl
      | test l cr t b site dm =>
        dsimp [Exec.step, eraseConfig, eraseCtrl, eraseFrame, List.map, instrumentOperands] at h ⊢
        cases he : instrumentEval O permit L st t with
        | ok r =>
          rcases r with ⟨v, st'⟩
          rw [he] at h
          simp only [Exec.onRes, Option.bind_eq_some_iff, Option.map_eq_some_iff] at h
          obtain ⟨go, hgo, st'', hst'', hnext⟩ := h
          rw [eval_ok_erasure (O := O) (types := types) L st st' t v he]
          dsimp [Exec.onRes]
          rw [hgo]
          dsimp [Option.bind]
          rw [dropOut_erasure_proj (O := O) (permit := permit) dm L st' st'' hst'']
          cases go <;> cases hnext <;> rfl
        | trap kd =>
          rw [he] at h
          cases h
          rw [eval_trap_erasure (O := O) (types := types) L st t kd he]
          rfl
        | stuck =>
          rw [he] at h
          cases h
      | body l cr t b site =>
        cases h
        rfl
      | resume p =>
        cases p <;> cases h <;> rfl
      | ret dst caller =>
        simp only [Exec.step, Option.bind_eq_some_iff, Option.map_eq_some_iff] at h
        obtain ⟨st', hst', st'', hst'', hnext⟩ := h
        cases hnext
        dsimp [Exec.step, eraseConfig, eraseCtrl, eraseFrame, List.map]
        rw [instrumentLife_erasure (O := O) (permit := permit) .leave st st' hst']
        dsimp [Option.bind]
        rw [bindLife_erasure_proj (O := O) (permit := permit) dst st' st'' hst'']
        rfl
  | exiting e =>
    dsimp [Exec.step, eraseConfig, eraseCtrl] at h ⊢
    obtain ⟨c₁, hc₁, hstep⟩ := stepExit_erasure_proj (O := O) L st e k (.next c') h
    cases hc₁
    exact hstep
  | trapped _ _ => cases h

/-- Law (subject `ConcreteSpec.Core.Own.eraseConfig`). Primitive-request forward simulation (owner decision 2026-11-08 Q3): whenever `step`
on the instrumented C1 machine issues a primitive request, `step` on the erased machine
issues the matching request with the erased store and agreeing continuation. -/
theorem step_request_erasure (P : Exec.Program (instrumentOperands O types permit))
    (c : Exec.Config (instrumentOperands O types permit))
    (p : O.Prim) (vs : List O.Val) (site : SiteId) (st : Store O.Store)
    (k : Exec.Answer O.Val O.Event (Store O.Store) →
      Option (Exec.Config (instrumentOperands O types permit)))
    (h : Exec.step P c = some (.request p vs site st k)) :
    ∃ k_erased,
      Exec.step (eraseProgram P) (eraseConfig c) =
        some (.request p vs site (erase st) k_erased) ∧
      ∀ a c', k a = some c' → k_erased (eraseAnswer a) = some (eraseConfig c') := by
  rcases c with ⟨ctrl, L, s, frames⟩
  cases ctrl with
  | exec stmt =>
    cases stmt with
    | skip => cases h
    | bind x e site' =>
      simp only [Exec.step, instrumentOperands] at h
      cases he : instrumentEval O permit L s e with
      | ok r =>
        rcases r with ⟨_, _⟩
        rw [he] at h
        simp only [Exec.onRes, Option.map_eq_some_iff] at h
        obtain ⟨_, _, h⟩ := h; cases h
      | trap _ => rw [he] at h; cases h
      | stuck => rw [he] at h; cases h
    | assign x e site' =>
      simp only [Exec.step, instrumentOperands] at h
      cases he : instrumentEval O permit L s e with
      | ok r =>
        rcases r with ⟨_, _⟩
        rw [he] at h
        simp only [Exec.onRes, Option.bind_eq_some_iff, Option.map_eq_some_iff] at h
        obtain ⟨_, _, _, _, h⟩ := h; cases h
      | trap _ => rw [he] at h; cases h
      | stuck => rw [he] at h; cases h
    | seq _ _ => cases h
    | block _ _ => cases h
    | defer _ _ _ =>
      simp only [Exec.step, Option.map_eq_some_iff] at h
      obtain ⟨_, _, h⟩ := h; cases h
    | cond e _ _ _ =>
      simp only [Exec.step, instrumentOperands] at h
      cases he : instrumentEval O permit L s e with
      | ok r =>
        rcases r with ⟨_, _⟩
        rw [he] at h
        simp only [Exec.onRes, Option.map_eq_some_iff] at h
        obtain ⟨_, _, h⟩ := h; cases h
      | trap _ => rw [he] at h; cases h
      | stuck => rw [he] at h; cases h
    | loop _ _ _ _ _ => cases h
    | brk _ => cases h
    | cont _ => cases h
    | ret e _ =>
      simp only [Exec.step, instrumentOperands] at h
      cases he : instrumentEval O permit L s e with
      | ok r => rcases r with ⟨_, _⟩; rw [he] at h; cases h
      | trap _ => rw [he] at h; cases h
      | stuck => rw [he] at h; cases h
    | call dst f args site' =>
      simp only [Exec.step] at h
      cases he : Exec.evalArgs (instrumentOperands O types permit) L s args with
      | ok r =>
        rcases r with ⟨vs', st'⟩
        rw [he] at h
        simp only [Exec.onRes, Option.bind_eq_some_iff] at h
        obtain ⟨fn, _, h⟩ := h
        by_cases hlen : fn.params.length = vs'.length
        · simp only [hlen, ↓reduceIte, Option.map_eq_some_iff] at h
          obtain ⟨_, _, h⟩ := h; cases h
        · simp only [hlen, ↓reduceIte] at h; cases h
      | trap _ => rw [he] at h; cases h
      | stuck => rw [he] at h; cases h
    | prim x p' args site' =>
      dsimp [Exec.step, eraseConfig, eraseCtrl, eraseStmt] at h ⊢
      cases he : Exec.evalArgs (instrumentOperands O types permit) L s args with
      | ok r =>
        rcases r with ⟨vs', st'⟩
        rw [he] at h
        dsimp [Exec.onRes] at h
        injection h with h
        injection h with hp hvs hsite hst hk_eq
        subst hp; subst hvs; subst hsite; subst hst; subst hk_eq
        refine ⟨_, by rw [evalArgs_ok_erasure_result (O := O) L args s st' vs' he]; rfl, ?_⟩
        intro a c' hk
        dsimp at hk
        cases ha : a.result with
        | ok v =>
          rw [ha] at hk
          simp only [Option.map_eq_some_iff] at hk
          obtain ⟨st'', hst'', rfl⟩ := hk
          dsimp [eraseAnswer]
          rw [ha]
          dsimp
          rw [bindLife_erasure_proj (O := O) (permit := permit) x a.store st'' hst'']
          rfl
        | error kd =>
          rw [ha] at hk
          cases hk
          dsimp [eraseAnswer]
          rw [ha]
      | trap _ => rw [he] at h; cases h
      | stuck => rw [he] at h; cases h
    | abort _ => cases h
  | done =>
    cases frames with
    | nil => cases h
    | cons f k' =>
      cases f with
      | seq _ => cases h
      | scope b dm ds =>
        cases ds with
        | nil =>
          simp only [Exec.step, Option.map_eq_some_iff] at h
          obtain ⟨_, _, h⟩ := h; cases h
        | cons _ _ => cases h
      | test l cr t b site' dm =>
        simp only [Exec.step, instrumentOperands] at h
        cases he : instrumentEval O permit L s t with
        | ok r =>
          rcases r with ⟨_, _⟩
          rw [he] at h
          simp only [Exec.onRes, Option.bind_eq_some_iff, Option.map_eq_some_iff] at h
          obtain ⟨go, _, _, _, h⟩ := h
          cases go <;> cases h
        | trap _ => rw [he] at h; cases h
        | stuck => rw [he] at h; cases h
      | body _ _ _ _ _ => cases h
      | resume p' => cases p' <;> cases h
      | ret _ _ =>
        simp only [Exec.step, Option.bind_eq_some_iff, Option.map_eq_some_iff] at h
        obtain ⟨_, _, _, _, h⟩ := h; cases h
  | exiting e =>
    dsimp [Exec.step] at h
    obtain ⟨_, hc, _⟩ := stepExit_erasure_proj (O := O) L s e frames _ h
    cases hc
  | trapped _ _ => cases h

/-- Helper (projection). Erasing a step label preserves its emitted event list. (approved: PENDING). -/
private theorem eraseLabel_events_proj (l : Exec.Label (instrumentOperands O types permit)) :
    (eraseLabel l).events = l.events := by
  cases l <;> rfl

/-- Helper (projection). Erasing an answer's step label equals the erased answer's label. (approved: PENDING). -/
private theorem eraseLabel_answer_proj (a : Exec.Answer O.Val O.Event (Store O.Store)) :
    eraseLabel (O := O) (types := types) (permit := permit) a.label = (eraseAnswer a).label := by
  rcases a with ⟨ev, _, _⟩
  cases ev <;> rfl

/-- Law (subject `ConcreteSpec.Core.Own.eraseConfig`). Single-step forward simulation (owner decision 2026-11-08 Q3): every labeled step of
the instrumented C1 machine is matched by a step of the erased machine with the same
event label and concrete store. -/
theorem Step_erasure (P : Exec.Program (instrumentOperands O types permit))
    (c c' : Exec.Config (instrumentOperands O types permit))
    (l : Exec.Label (instrumentOperands O types permit))
    (h : Exec.Step P c l c') :
    Exec.Step (eraseProgram P) (eraseConfig c) (eraseLabel l) (eraseConfig c') := by
  cases h with
  | silent hs => exact .silent (step_next_erasure P c c' hs)
  | prim hs hallows hk =>
    obtain ⟨k_erased, hs_erased, hk_erased⟩ := step_request_erasure P c _ _ _ _ _ hs
    obtain ⟨hallows_erased, _⟩ := allows_erasure _ _ _ _ hallows
    rw [eraseLabel_answer_proj]
    exact .prim hs_erased hallows_erased (hk_erased _ _ hk)

/-- Law (subject `ConcreteSpec.Core.Own.eraseConfig`). Multi-step forward simulation (owner decision 2026-11-08 Q3): every multi-step run
of the instrumented C1 machine is matched by a multi-step run of the erased machine
emitting the exact same events in the same order and reaching the erased configuration. -/
theorem Steps_erasure (P : Exec.Program (instrumentOperands O types permit))
    (c c' : Exec.Config (instrumentOperands O types permit))
    (tr : List O.Event)
    (h : Exec.Steps P c tr c') :
    Exec.Steps (eraseProgram P) (eraseConfig c) tr (eraseConfig c') := by
  induction h with
  | refl c => exact .refl (eraseConfig c)
  | @step c l c₁ tr c₂ hs _ ih =>
    have hs' := Step_erasure P c c₁ l hs
    simpa [eraseLabel_events_proj] using Exec.Steps.step hs' ih

/-- Helper (projection). Configuration finality is invariant under erasure. (approved: PENDING). -/
private theorem final?_eraseConfig_proj (c : Exec.Config (instrumentOperands O types permit)) :
    (eraseConfig c).final? = c.final? := by
  rcases c with ⟨ctrl, L, st, frames⟩
  cases ctrl with
  | exec _ => rfl
  | done => cases frames <;> rfl
  | exiting e =>
    cases e with
    | brk _ => rfl
    | cont _ => rfl
    | ret _ => cases frames <;> rfl
  | trapped _ _ => rfl

/-- Helper (projection). Lifting a concrete answer preserves the primitive contract check. (approved: PENDING). -/
private theorem allows_liftAnswer_proj (p : O.Prim) (vs : List O.Val) (s : Store O.Store)
    (a : Exec.Answer O.Val O.Event O.Store) :
    (instrumentOperands O types permit).allows p vs s (liftAnswer s.own a) =
      O.allows p vs (erase s) a := by
  simp [instrumentOperands, instrumentAllows, eraseAnswer, liftAnswer, erase]

/-- Law (subject `ConcreteSpec.Core.Own.eraseConfig`). Fuel-indexed evaluator forward simulation (owner decision 2026-11-08 Q3): whenever
the instrumented run reaches a non-`stuck` outcome (`final`—normal, returned or trapped—
`noAnswer`, `badAnswer` or `outOfFuel`), the erased run reaches the matching outcome with
the same events in order and the same concrete store. -/
theorem runFrom_erasure (P : Exec.Program (instrumentOperands O types permit))
    (ρ : Exec.Oracle O) :
    ∀ (fuel : Nat) (hist : List O.Event)
      (c : Exec.Config (instrumentOperands O types permit)),
      Ending.NotStuck (Exec.runFrom P (liftOracle O types permit ρ) fuel hist c).ending →
      Exec.runFrom (eraseProgram P) ρ fuel hist (eraseConfig c) =
        eraseReport (Exec.runFrom P (liftOracle O types permit ρ) fuel hist c)
  | 0, hist, c, _ => by
    unfold Exec.runFrom
    rw [final?_eraseConfig_proj]
    cases c.final? <;> rfl
  | n + 1, hist, c, hns => by
    unfold Exec.runFrom at hns ⊢
    rw [final?_eraseConfig_proj]
    cases hf : c.final? with
    | some f => rfl
    | none =>
      rw [hf] at hns
      dsimp at hns ⊢
      cases hs : Exec.step P c with
      | none =>
        rw [hs] at hns
        cases hns
      | some out =>
        rw [hs] at hns
        cases out with
        | next c' =>
          rw [step_next_erasure P c c' hs]
          exact runFrom_erasure P ρ n hist c' hns
        | request p vs site st k =>
          obtain ⟨k_erased, hs_erased, hk_erased⟩ := step_request_erasure P c p vs site st k hs
          rw [hs_erased]
          dsimp [liftOracle] at hns ⊢
          cases hρ : ρ hist p vs site (erase st) with
          | none => rfl
          | some a₀ =>
            rw [hρ] at hns
            have ha_eq : instrumentAllows O p vs st (liftAnswer st.own a₀) =
                O.allows p vs (erase st) a₀ :=
              allows_liftAnswer_proj (O := O) (types := types) (permit := permit) p vs st a₀
            by_cases ha : O.allows p vs (erase st) a₀ = true
            · have ha' : instrumentAllows O p vs st (liftAnswer st.own a₀) = true :=
                ha_eq.trans ha
              dsimp [instrumentOperands] at hns ⊢
              simp only [ha', ha, ↓reduceIte] at hns ⊢
              cases hk : k (liftAnswer st.own a₀) with
              | none =>
                rw [hk] at hns
                cases hns
              | some c' =>
                rw [hk] at hns
                have hk' := hk_erased (liftAnswer st.own a₀) c' hk
                have ha_ans : eraseAnswer (liftAnswer st.own a₀) = a₀ := by
                  cases a₀; rfl
                rw [ha_ans] at hk'
                rw [hk']
                exact runFrom_erasure P ρ n (hist ++ a₀.event.toList) c' hns
            · have ha' : ¬ instrumentAllows O p vs st (liftAnswer st.own a₀) = true := by
                rw [ha_eq]; exact ha
              dsimp [instrumentOperands] at hns ⊢
              simp only [ha', ha] at hns ⊢
              rfl

/-- Law (subject `ConcreteSpec.Core.Own.eraseConfig`). Whole-run forward simulation from an empty event history (owner decision 2026-11-08 Q3). -/
theorem run_erasure (P : Exec.Program (instrumentOperands O types permit))
    (ρ : Exec.Oracle O) (fuel : Nat)
    (c : Exec.Config (instrumentOperands O types permit))
    (h : Ending.NotStuck (Exec.run P (liftOracle O types permit ρ) fuel c).ending) :
    Exec.run (eraseProgram P) ρ fuel (eraseConfig c) =
      eraseReport (Exec.run P (liftOracle O types permit ρ) fuel c) :=
  runFrom_erasure P ρ fuel [] c h

/-- Helper (inversion). Under `EvalPermitted`, an instrumented operand evaluation that is
stuck is also stuck in the erased operand instance. (approved: PENDING). -/
private theorem eval_stuck_of_permitted_inv (L : AList Var O.Val) (s : Store O.Store)
    (e : O.Expr) (hp : EvalPermitted O permit L s e)
    (hs : (instrumentOperands O types permit).eval L s e = .stuck) :
    O.eval L (erase s) e = .stuck := by
  dsimp [instrumentOperands, instrumentEval] at hs
  cases hc : O.eval L s.concrete e with
  | ok r =>
    rcases r with ⟨_, _⟩
    have hns : O.eval L s.concrete e ≠ .stuck := by rw [hc]; intro h; cases h
    obtain ⟨own', hown⟩ := Option.isSome_iff_exists.mp (hp hns)
    rw [hown, hc] at hs
    cases hs
  | trap _ =>
    have hns : O.eval L s.concrete e ≠ .stuck := by rw [hc]; intro h; cases h
    obtain ⟨own', hown⟩ := Option.isSome_iff_exists.mp (hp hns)
    rw [hown, hc] at hs
    cases hs
  | stuck => exact hc

/-- Helper (inversion). Under `ArgsPermitted`, an instrumented argument list that is
stuck is also stuck in the erased operand instance. (approved: PENDING). -/
private theorem evalArgs_stuck_of_permitted_inv (L : AList Var O.Val) :
    ∀ (args : List O.Expr) (s : Store O.Store),
      ArgsPermitted O types permit L s args →
      Exec.evalArgs (instrumentOperands O types permit) L s args = .stuck →
      Exec.evalArgs O L (erase s) args = .stuck
  | [], _, _, hs => by cases hs
  | e :: es, s, hp, hs => by
    obtain ⟨hpe, hpes⟩ := hp
    simp only [Exec.evalArgs, instrumentOperands] at hs ⊢
    cases he : instrumentEval O permit L s e with
    | ok r =>
      rcases r with ⟨v, s₁⟩
      rw [he] at hs
      rw [eval_ok_erasure (O := O) (types := types) L s s₁ e v he]
      dsimp [Res.bind] at hs ⊢
      cases hes : Exec.evalArgs (instrumentOperands O types permit) L s₁ es with
      | ok r' =>
        rcases r' with ⟨_, _⟩
        simp only [instrumentOperands] at hes
        rw [hes] at hs; cases hs
      | trap _ =>
        simp only [instrumentOperands] at hes
        rw [hes] at hs; cases hs
      | stuck =>
        have ih := evalArgs_stuck_of_permitted_inv L es s₁ (hpes v s₁ he) hes
        rw [ih]
        rfl
    | trap _ =>
      rw [he] at hs; cases hs
    | stuck =>
      rw [eval_stuck_of_permitted_inv (O := O) (types := types) L s e hpe he]
      rfl

/-- Helper (inversion). Under `LifePermitted`, an instrumented lifecycle transition that
returns `none` also returns `none` on the erased store. (approved: PENDING). -/
private theorem life_none_of_permitted_inv (ev : Exec.Life) (s : Store O.Store)
    (hp : LifePermitted O types ev s)
    (hs : (instrumentOperands O types permit).life ev s = none) :
    O.life ev (erase s) = none := by
  dsimp [instrumentOperands, instrumentLife] at hs
  cases hc : O.life ev s.concrete with
  | none => exact hc
  | some _ =>
    have hsome : (O.life ev s.concrete).isSome = true := by rw [hc]; rfl
    obtain ⟨own', hown⟩ := Option.isSome_iff_exists.mp (hp hsome)
    rw [hown, hc] at hs
    cases hs

/-- Helper (inversion). Under `BindDstPermitted`, an instrumented destination-binding
lifecycle that returns `none` also returns `none` on the erased store. (approved: PENDING). -/
private theorem bindLife_none_of_permitted_inv (dst : Option Var) (s : Store O.Store)
    (hp : BindDstPermitted O types dst s)
    (hs : Exec.bindLife (instrumentOperands O types permit) dst s = none) :
    Exec.bindLife O dst (erase s) = none := by
  cases dst with
  | none => cases hs
  | some x => exact life_none_of_permitted_inv (O := O) (permit := permit) (.bind x) s hp hs

/-- Helper (inversion). Under `LifePermitted`, an instrumented scope-exit drop that
returns `none` also returns `none` on the erased store. (approved: PENDING). -/
private theorem dropOut_none_of_permitted_inv (dm : List Var) (L : AList Var O.Val)
    (s : Store O.Store) (hp : LifePermitted O types (.drop (Exec.dead dm L)) s)
    (hs : Exec.dropOut (instrumentOperands O types permit) dm L s = none) :
    Exec.dropOut O dm L (erase s) = none :=
  life_none_of_permitted_inv (O := O) (permit := permit) (.drop (Exec.dead dm L)) s hp hs

/-- Helper (projection). Deferred registration failure is invariant under frame erasure. (approved: PENDING). -/
private theorem register_none_proj (d : Exec.Deferred (instrumentOperands O types permit)) :
    ∀ (k : List (Exec.Frame (instrumentOperands O types permit))),
      Exec.register d k = none →
      Exec.register (eraseDeferred d) (k.map eraseFrame) = none
  | [], _ => rfl
  | .seq t :: k, h => by
    simp only [Exec.register, Option.map_eq_none_iff] at h
    simp [Exec.register, eraseFrame, register_none_proj d k h]
  | .scope _ _ _ :: _, h => by cases h
  | .test .. :: _, _ => rfl
  | .body .. :: _, _ => rfl
  | .resume _ :: _, _ => rfl
  | .ret .. :: _, _ => rfl

/-- Helper (inversion). Under `ExitFramePermitted`, structured exit unwinding that returns
`none` on the instrumented machine also returns `none` on the erased machine. (approved: PENDING). -/
private theorem stepExit_none_of_permitted_inv (L : AList Var O.Val) (s : Store O.Store)
    (e : Exec.Exit O.Val) (k : List (Exec.Frame (instrumentOperands O types permit)))
    (hp : match k with
      | f :: _ => ExitFramePermitted O types permit L s e f
      | [] => True)
    (hs : Exec.stepExit (O := instrumentOperands O types permit) L s e k = none) :
    Exec.stepExit (O := O) L (erase s) e (k.map eraseFrame) = none := by
  cases k with
  | nil => rfl
  | cons f k =>
    cases f with
    | seq _ => cases hs
    | scope b dm ds =>
      cases ds with
      | nil =>
        simp only [Exec.stepExit, Option.map_eq_none_iff] at hs
        dsimp [Exec.stepExit, eraseFrame, List.map]
        rw [dropOut_none_of_permitted_inv (O := O) (permit := permit) dm L s hp hs]
        rfl
      | cons _ _ => cases hs
    | test l cr t body site dm =>
      simp only [Exec.stepExit, Option.map_eq_none_iff] at hs
      dsimp [Exec.stepExit, eraseFrame, List.map]
      rw [dropOut_none_of_permitted_inv (O := O) (permit := permit) dm L s hp hs]
      rfl
    | body l cr t b site =>
      cases e with
      | brk l' =>
        by_cases hl : l' = l <;> simp [Exec.stepExit, hl] at hs
      | cont l' =>
        by_cases hl : l' = l <;> simp [Exec.stepExit, hl] at hs
      | ret _ => cases hs
    | resume _ => rfl
    | ret dst caller =>
      cases e with
      | brk _ => rfl
      | cont _ => rfl
      | ret v =>
        obtain ⟨hleave, hbind⟩ := hp
        dsimp [Exec.stepExit, eraseFrame, List.map] at hs ⊢
        cases hl : (instrumentOperands O types permit).life .leave s with
        | none =>
          rw [life_none_of_permitted_inv (O := O) (permit := permit) .leave s hleave hl]
          rfl
        | some st' =>
          have hl' : instrumentLife O types .leave s = some st' := hl
          rw [hl'] at hs
          dsimp [Option.bind] at hs
          simp only [Option.map_eq_none_iff] at hs
          rw [instrumentLife_erasure (O := O) (permit := permit) .leave s st' hl]
          dsimp [Option.bind]
          rw [bindLife_none_of_permitted_inv (O := O) (permit := permit) dst st' (hbind st' hl) hs]
          rfl

/-- Helper (inversion). Under `StepPermitted`, if `step` on the instrumented C1 machine
returns `none`, then `step` on the erased machine also returns `none`. (approved: PENDING). -/
private theorem step_none_of_permitted_inv
    (P : Exec.Program (instrumentOperands O types permit))
    (c : Exec.Config (instrumentOperands O types permit))
    (hp : StepPermitted O types permit P c)
    (hs : Exec.step P c = none) :
    Exec.step (eraseProgram P) (eraseConfig c) = none := by
  rcases c with ⟨ctrl, L, st, k⟩
  cases ctrl with
  | exec s =>
    cases s with
    | skip => cases hs
    | bind x e site =>
      obtain ⟨hpe, hpl⟩ := hp
      dsimp [Exec.step, eraseConfig, eraseCtrl, eraseStmt, instrumentOperands] at hs ⊢
      cases he : instrumentEval O permit L st e with
      | ok r =>
        rcases r with ⟨v, st'⟩
        rw [he] at hs
        simp only [Exec.onRes, Option.map_eq_none_iff] at hs
        rw [eval_ok_erasure (O := O) (types := types) L st st' e v he]
        dsimp [Exec.onRes]
        rw [life_none_of_permitted_inv (O := O) (permit := permit) (.bind x) st' (hpl v st' he) hs]
        rfl
      | trap _ => rw [he] at hs; cases hs
      | stuck =>
        rw [eval_stuck_of_permitted_inv (O := O) (types := types) L st e hpe he]
        rfl
    | assign x e site =>
      obtain ⟨hpe, hpl⟩ := hp
      dsimp [Exec.step, eraseConfig, eraseCtrl, eraseStmt, instrumentOperands] at hs ⊢
      cases he : instrumentEval O permit L st e with
      | ok r =>
        rcases r with ⟨v, st'⟩
        rw [he] at hs
        rw [eval_ok_erasure (O := O) (types := types) L st st' e v he]
        dsimp [Exec.onRes] at hs ⊢
        cases hlook : AList.lookup L x with
        | none => rfl
        | some w =>
          rw [hlook] at hs
          dsimp [Option.bind] at hs ⊢
          simp only [Option.map_eq_none_iff] at hs
          have hsome : (AList.lookup L x).isSome = true := by rw [hlook]; rfl
          rw [life_none_of_permitted_inv (O := O) (permit := permit) (.bind x) st'
            (hpl v st' he hsome) hs]
          rfl
      | trap _ => rw [he] at hs; cases hs
      | stuck =>
        rw [eval_stuck_of_permitted_inv (O := O) (types := types) L st e hpe he]
        rfl
    | seq _ _ => cases hs
    | block _ _ => cases hs
    | defer f args site =>
      simp only [Exec.step, Option.map_eq_none_iff] at hs
      have hreg := register_none_proj (O := O) (types := types) (permit := permit) ⟨f, args, site⟩ k hs
      simp only [eraseDeferred] at hreg
      simp [Exec.step, eraseConfig, eraseCtrl, eraseStmt, hreg]
    | cond e s t site =>
      dsimp [Exec.step, eraseConfig, eraseCtrl, eraseStmt, instrumentOperands] at hs ⊢
      cases he : instrumentEval O permit L st e with
      | ok r =>
        rcases r with ⟨v, st'⟩
        rw [he] at hs
        simp only [Exec.onRes, Option.map_eq_none_iff] at hs
        rw [eval_ok_erasure (O := O) (types := types) L st st' e v he]
        simp [Exec.onRes, hs]
      | trap _ => rw [he] at hs; cases hs
      | stuck =>
        rw [eval_stuck_of_permitted_inv (O := O) (types := types) L st e hp he]
        rfl
    | loop _ _ _ _ _ => cases hs
    | brk _ => cases hs
    | cont _ => cases hs
    | ret e site =>
      dsimp [Exec.step, eraseConfig, eraseCtrl, eraseStmt, instrumentOperands] at hs ⊢
      cases he : instrumentEval O permit L st e with
      | ok r => rcases r with ⟨_, _⟩; rw [he] at hs; cases hs
      | trap _ => rw [he] at hs; cases hs
      | stuck =>
        rw [eval_stuck_of_permitted_inv (O := O) (types := types) L st e hp he]
        rfl
    | call dst f args site =>
      obtain ⟨hpa, hpl⟩ := hp
      dsimp [Exec.step, eraseConfig, eraseCtrl, eraseStmt] at hs ⊢
      cases he : Exec.evalArgs (instrumentOperands O types permit) L st args with
      | ok r =>
        rcases r with ⟨vs, st'⟩
        rw [he] at hs
        rw [evalArgs_ok_erasure_result (O := O) L args st st' vs he]
        dsimp [Exec.onRes, eraseProgram] at hs ⊢
        rw [lookup_eraseProgram_proj P.fns f]
        cases hfn : AList.lookup P.fns f with
        | none => rfl
        | some fn =>
          rw [hfn] at hs
          dsimp [Option.bind, eraseFn] at hs ⊢
          by_cases hlen : fn.params.length = vs.length
          · simp only [hlen, ↓reduceIte, Option.map_eq_none_iff] at hs ⊢
            exact life_none_of_permitted_inv (O := O) (permit := permit)
              (.enter (fn.params.map Prod.fst)) st' (hpl vs st' fn he hfn hlen) hs
          · simp [hlen]
      | trap _ => rw [he] at hs; cases hs
      | stuck =>
        rw [evalArgs_stuck_of_permitted_inv (O := O) L args st hpa he]
        rfl
    | prim x p args site =>
      obtain ⟨hpa, _⟩ := hp
      dsimp [Exec.step, eraseConfig, eraseCtrl, eraseStmt] at hs ⊢
      cases he : Exec.evalArgs (instrumentOperands O types permit) L st args with
      | ok r => rcases r with ⟨_, _⟩; rw [he] at hs; cases hs
      | trap _ => rw [he] at hs; cases hs
      | stuck =>
        rw [evalArgs_stuck_of_permitted_inv (O := O) L args st hpa he]
        rfl
    | abort _ => cases hs
  | done =>
    cases k with
    | nil => rfl
    | cons f k =>
      cases f with
      | seq _ => cases hs
      | scope b dm ds =>
        cases ds with
        | nil =>
          simp only [Exec.step, Option.map_eq_none_iff] at hs
          dsimp [Exec.step, eraseConfig, eraseCtrl, eraseFrame, List.map]
          rw [dropOut_none_of_permitted_inv (O := O) (permit := permit) dm L st hp hs]
          rfl
        | cons _ _ => cases hs
      | test l cr t b site dm =>
        obtain ⟨hpt, hpd⟩ := hp
        dsimp [Exec.step, eraseConfig, eraseCtrl, eraseFrame, List.map, instrumentOperands] at hs ⊢
        cases he : instrumentEval O permit L st t with
        | ok r =>
          rcases r with ⟨v, st'⟩
          rw [he] at hs
          rw [eval_ok_erasure (O := O) (types := types) L st st' t v he]
          dsimp [Exec.onRes] at hs ⊢
          cases hgo : O.test v with
          | none => rfl
          | some go =>
            rw [hgo] at hs
            dsimp [Option.bind] at hs ⊢
            simp only [Option.map_eq_none_iff] at hs
            have hsome : (O.test v).isSome = true := by rw [hgo]; rfl
            rw [dropOut_none_of_permitted_inv (O := O) (permit := permit) dm L st'
              (hpd v st' he hsome) hs]
            rfl
        | trap _ => rw [he] at hs; cases hs
        | stuck =>
          rw [eval_stuck_of_permitted_inv (O := O) (types := types) L st t hpt he]
          rfl
      | body _ _ _ _ _ => cases hs
      | resume p => cases p <;> cases hs
      | ret dst caller =>
        obtain ⟨hleave, hbind⟩ := hp
        dsimp [Exec.step, eraseConfig, eraseCtrl, eraseFrame, List.map] at hs ⊢
        cases hl : (instrumentOperands O types permit).life .leave st with
        | none =>
          rw [life_none_of_permitted_inv (O := O) (permit := permit) .leave st hleave hl]
          rfl
        | some st' =>
          have hl' : instrumentLife O types .leave st = some st' := hl
          rw [hl'] at hs
          dsimp [Option.bind] at hs
          simp only [Option.map_eq_none_iff] at hs
          rw [instrumentLife_erasure (O := O) (permit := permit) .leave st st' hl]
          dsimp [Option.bind]
          rw [bindLife_none_of_permitted_inv (O := O) (permit := permit) dst st' (hbind st' hl) hs]
          rfl
  | exiting e =>
    exact stepExit_none_of_permitted_inv (O := O) L st e k hp hs
  | trapped _ _ => rfl

/-- Helper (inversion). Under `StepPermitted`, if a primitive request continuation returns
`none` on an allowed answer, the erased continuation also returns `none`. (approved: PENDING). -/
private theorem request_none_of_permitted_inv
    (P : Exec.Program (instrumentOperands O types permit))
    (c : Exec.Config (instrumentOperands O types permit))
    (p : O.Prim) (vs : List O.Val) (site : SiteId) (st : Store O.Store)
    (k : Exec.Answer O.Val O.Event (Store O.Store) →
      Option (Exec.Config (instrumentOperands O types permit)))
    (a : Exec.Answer O.Val O.Event (Store O.Store))
    (hp : StepPermitted O types permit P c)
    (hs : Exec.step P c = some (.request p vs site st k))
    (hallows : (instrumentOperands O types permit).allows p vs st a = true)
    (hk : k a = none) :
    ∃ k_erased,
      Exec.step (eraseProgram P) (eraseConfig c) =
        some (.request p vs site (erase st) k_erased) ∧
      k_erased (eraseAnswer a) = none := by
  rcases c with ⟨ctrl, L, s, frames⟩
  cases ctrl with
  | exec stmt =>
    cases stmt with
    | skip => cases hs
    | bind x e _ =>
      simp only [Exec.step, instrumentOperands] at hs
      cases he : instrumentEval O permit L s e with
      | ok r =>
        rcases r with ⟨_, _⟩; rw [he] at hs
        simp only [Exec.onRes, Option.map_eq_some_iff] at hs
        obtain ⟨_, _, hs⟩ := hs; cases hs
      | trap _ => rw [he] at hs; cases hs
      | stuck => rw [he] at hs; cases hs
    | assign x e _ =>
      simp only [Exec.step, instrumentOperands] at hs
      cases he : instrumentEval O permit L s e with
      | ok r =>
        rcases r with ⟨_, _⟩; rw [he] at hs
        simp only [Exec.onRes, Option.bind_eq_some_iff, Option.map_eq_some_iff] at hs
        obtain ⟨_, _, _, _, hs⟩ := hs; cases hs
      | trap _ => rw [he] at hs; cases hs
      | stuck => rw [he] at hs; cases hs
    | seq _ _ => cases hs
    | block _ _ => cases hs
    | defer _ _ _ =>
      simp only [Exec.step, Option.map_eq_some_iff] at hs
      obtain ⟨_, _, hs⟩ := hs; cases hs
    | cond e _ _ _ =>
      simp only [Exec.step, instrumentOperands] at hs
      cases he : instrumentEval O permit L s e with
      | ok r =>
        rcases r with ⟨_, _⟩; rw [he] at hs
        simp only [Exec.onRes, Option.map_eq_some_iff] at hs
        obtain ⟨_, _, hs⟩ := hs; cases hs
      | trap _ => rw [he] at hs; cases hs
      | stuck => rw [he] at hs; cases hs
    | loop _ _ _ _ _ => cases hs
    | brk _ => cases hs
    | cont _ => cases hs
    | ret e _ =>
      simp only [Exec.step, instrumentOperands] at hs
      cases he : instrumentEval O permit L s e with
      | ok r => rcases r with ⟨_, _⟩; rw [he] at hs; cases hs
      | trap _ => rw [he] at hs; cases hs
      | stuck => rw [he] at hs; cases hs
    | call dst f args _ =>
      simp only [Exec.step] at hs
      cases he : Exec.evalArgs (instrumentOperands O types permit) L s args with
      | ok r =>
        rcases r with ⟨vs', st'⟩; rw [he] at hs
        simp only [Exec.onRes, Option.bind_eq_some_iff] at hs
        obtain ⟨fn, _, hs⟩ := hs
        by_cases hlen : fn.params.length = vs'.length
        · simp only [hlen, ↓reduceIte, Option.map_eq_some_iff] at hs
          obtain ⟨_, _, hs⟩ := hs; cases hs
        · simp only [hlen, ↓reduceIte] at hs; cases hs
      | trap _ => rw [he] at hs; cases hs
      | stuck => rw [he] at hs; cases hs
    | prim x p' args site' =>
      obtain ⟨_, hpb⟩ := hp
      dsimp [Exec.step, eraseConfig, eraseCtrl, eraseStmt] at hs ⊢
      cases he : Exec.evalArgs (instrumentOperands O types permit) L s args with
      | ok r =>
        rcases r with ⟨vs', st'⟩
        rw [he] at hs
        dsimp [Exec.onRes] at hs
        injection hs with hs
        injection hs with hp_eq hvs hsite hst hk_eq
        subst hp_eq; subst hvs; subst hsite; subst hst; subst hk_eq
        refine ⟨_, by rw [evalArgs_ok_erasure_result (O := O) L args s st' vs' he]; rfl, ?_⟩
        dsimp at hk ⊢
        cases ha : a.result with
        | ok v =>
          rw [ha] at hk
          simp only [Option.map_eq_none_iff] at hk
          have hok : a.result.isOk = true := by rw [ha]; rfl
          have hbind := hpb vs' st' a he hallows hok
          dsimp [eraseAnswer]
          rw [ha]
          dsimp
          rw [bindLife_none_of_permitted_inv (O := O) (permit := permit) x a.store hbind hk]
          rfl
        | error kd =>
          rw [ha] at hk
          cases hk
      | trap _ => rw [he] at hs; cases hs
      | stuck => rw [he] at hs; cases hs
    | abort _ => cases hs
  | done =>
    cases frames with
    | nil => cases hs
    | cons f k' =>
      cases f with
      | seq _ => cases hs
      | scope b dm ds =>
        cases ds with
        | nil =>
          simp only [Exec.step, Option.map_eq_some_iff] at hs
          obtain ⟨_, _, hs⟩ := hs; cases hs
        | cons _ _ => cases hs
      | test l cr t b site' dm =>
        simp only [Exec.step, instrumentOperands] at hs
        cases he : instrumentEval O permit L s t with
        | ok r =>
          rcases r with ⟨_, _⟩; rw [he] at hs
          simp only [Exec.onRes, Option.bind_eq_some_iff, Option.map_eq_some_iff] at hs
          obtain ⟨go, _, _, _, hs⟩ := hs
          cases go <;> cases hs
        | trap _ => rw [he] at hs; cases hs
        | stuck => rw [he] at hs; cases hs
      | body _ _ _ _ _ => cases hs
      | resume p' => cases p' <;> cases hs
      | ret _ _ =>
        simp only [Exec.step, Option.bind_eq_some_iff, Option.map_eq_some_iff] at hs
        obtain ⟨_, _, _, _, hs⟩ := hs; cases hs
  | exiting e =>
    dsimp [Exec.step] at hs
    obtain ⟨_, hc, _⟩ := stepExit_erasure_proj (O := O) L s e frames _ hs
    cases hc
  | trapped _ _ => cases hs

/-- Helper (inversion). `CheckerAccepted` is preserved across any single machine step. (approved: PENDING). -/
private theorem checkerAccepted_step_inv
    (P : Exec.Program (instrumentOperands O types permit))
    (c c' : Exec.Config (instrumentOperands O types permit))
    (l : Exec.Label (instrumentOperands O types permit))
    (hacc : CheckerAccepted O types permit P c)
    (hs : Exec.Step P c l c') :
    CheckerAccepted O types permit P c' :=
  fun tr c'' hsteps => hacc (l.events ++ tr) c'' (.step hs hsteps)

/-- Law (subject `ConcreteSpec.Core.Own.eraseConfig`). Single-step stuck equivalence for checker-accepted programs (owner decision 2026-11-08 Q3):
at any configuration accepted by the ownership checker, the instrumented C1 machine has no
step iff the erased machine has no step. -/
theorem step_stuck_iff_of_accepted
    (P : Exec.Program (instrumentOperands O types permit))
    (c : Exec.Config (instrumentOperands O types permit))
    (hacc : CheckerAccepted O types permit P c) :
    Exec.step P c = none ↔ Exec.step (eraseProgram P) (eraseConfig c) = none := by
  constructor
  · exact step_none_of_permitted_inv P c (hacc [] c (.refl c)).2
  · intro herased
    cases hs : Exec.step P c with
    | none => rfl
    | some out =>
      cases out with
      | next c' =>
        have h' := step_next_erasure P c c' hs
        rw [herased] at h'
        cases h'
      | request p vs site st k =>
        obtain ⟨_, h', _⟩ := step_request_erasure P c p vs site st k hs
        rw [herased] at h'
        cases h'

/-- Law (subject `ConcreteSpec.Core.Own.eraseConfig`). Exact fuel-indexed simulation for checker-accepted programs (owner decision 2026-11-08 Q3):
when `(P, c)` satisfies `CheckerAccepted`, the erased run equals the erased report of the
instrumented run for every outcome, including `stuck`. -/
theorem runFrom_erasure_of_accepted
    (P : Exec.Program (instrumentOperands O types permit))
    (ρ : Exec.Oracle O) :
    ∀ (fuel : Nat) (hist : List O.Event)
      (c : Exec.Config (instrumentOperands O types permit)),
      CheckerAccepted O types permit P c →
      Exec.runFrom (eraseProgram P) ρ fuel hist (eraseConfig c) =
        eraseReport (Exec.runFrom P (liftOracle O types permit ρ) fuel hist c)
  | 0, hist, c, _ => by
    unfold Exec.runFrom
    rw [final?_eraseConfig_proj]
    cases c.final? <;> rfl
  | n + 1, hist, c, hacc => by
    have hperm : StepPermitted O types permit P c := (hacc [] c (.refl c)).2
    unfold Exec.runFrom
    rw [final?_eraseConfig_proj]
    cases hf : c.final? with
    | some _ => rfl
    | none =>
      dsimp
      cases hs : Exec.step P c with
      | none =>
        rw [step_none_of_permitted_inv P c hperm hs]
        rfl
      | some out =>
        cases out with
        | next c' =>
          rw [step_next_erasure P c c' hs]
          exact runFrom_erasure_of_accepted P ρ n hist c'
            (checkerAccepted_step_inv P c c' .silent hacc (.silent hs))
        | request p vs site st k =>
          obtain ⟨k_erased, hs_erased, hk_erased⟩ := step_request_erasure P c p vs site st k hs
          rw [hs_erased]
          dsimp [liftOracle]
          cases hρ : ρ hist p vs site (erase st) with
          | none => rfl
          | some a₀ =>
            have ha_eq : instrumentAllows O p vs st (liftAnswer st.own a₀) =
                O.allows p vs (erase st) a₀ :=
              allows_liftAnswer_proj (O := O) (types := types) (permit := permit) p vs st a₀
            by_cases ha : O.allows p vs (erase st) a₀ = true
            · have ha' : (instrumentOperands O types permit).allows p vs st
                  (liftAnswer st.own a₀) = true := ha_eq.trans ha
              have ha_inst : instrumentAllows O p vs st (liftAnswer st.own a₀) = true := ha'
              dsimp [instrumentOperands]
              simp only [ha_inst, ha, ↓reduceIte]
              cases hk : k (liftAnswer st.own a₀) with
              | none =>
                obtain ⟨k_e2, hs_e2, hk_none⟩ :=
                  request_none_of_permitted_inv P c p vs site st k
                    (liftAnswer st.own a₀) hperm hs ha' hk
                rw [hs_erased] at hs_e2
                injection hs_e2 with hs_e2
                injection hs_e2 with _ _ _ _ hk_eq
                subst hk_eq
                have ha_ans : eraseAnswer (liftAnswer st.own a₀) = a₀ := by
                  cases a₀; rfl
                rw [ha_ans] at hk_none
                rw [hk_none]
                rfl
              | some c' =>
                have hk' := hk_erased (liftAnswer st.own a₀) c' hk
                have ha_ans : eraseAnswer (liftAnswer st.own a₀) = a₀ := by
                  cases a₀; rfl
                rw [ha_ans] at hk'
                rw [hk']
                exact runFrom_erasure_of_accepted P ρ n (hist ++ a₀.event.toList) c'
                  (checkerAccepted_step_inv P c c' (liftAnswer st.own a₀).label hacc
                    (.prim hs ha' hk))
            · have ha_inst : ¬ instrumentAllows O p vs st (liftAnswer st.own a₀) = true := by
                rw [ha_eq]; exact ha
              dsimp [instrumentOperands]
              simp only [ha_inst, ha]
              rfl

/-- Helper (projection). `Ending.IsStuck` is invariant under `eraseEnding`. (approved: PENDING). -/
private theorem isStuck_eraseEnding_proj (e : Exec.Ending (instrumentOperands O types permit)) :
    Ending.IsStuck (eraseEnding e) = Ending.IsStuck e := by
  cases e <;> rfl

/-- Law (subject `ConcreteSpec.Core.Own.eraseConfig`). Converse stuck equivalence for checker-accepted programs (owner decision 2026-11-08 Q3):
for a program and initial configuration accepted by the ownership checker, the instrumented
C1 machine is stuck iff the erased machine is stuck. -/
theorem run_stuck_iff_of_accepted
    (P : Exec.Program (instrumentOperands O types permit))
    (ρ : Exec.Oracle O) (fuel : Nat)
    (c : Exec.Config (instrumentOperands O types permit))
    (hacc : CheckerAccepted O types permit P c) :
    Ending.IsStuck (Exec.run P (liftOracle O types permit ρ) fuel c).ending =
      Ending.IsStuck (Exec.run (eraseProgram P) ρ fuel (eraseConfig c)).ending := by
  unfold Exec.run
  rw [runFrom_erasure_of_accepted P ρ fuel [] c hacc]
  exact (isStuck_eraseEnding_proj _).symm

end ExecErasure

namespace ExecErasureWitnesses
open ConcreteSpec.Core.Own.Examples

private def emptyProg : Exec.Program instrumentedOps := ⟨[]⟩

private def bindMoveConfig : Exec.Config instrumentedOps :=
  ⟨.exec (.bind y (.move x) ⟨0⟩), [(x, 7)], ⟨0, initial⟩, []⟩

private def primOneConfig : Exec.Config instrumentedOps :=
  ⟨.exec (.prim (some y) () [.read x] ⟨0⟩), [(x, 7)], ⟨0, initial⟩, []⟩

/-- Helper (inversion). A step from a configuration whose `step` is `some (.next c₁)` must
land in `c₁`. (approved: PENDING). -/
private theorem step_det_next_inv {O : Exec.Operands} {P : Exec.Program O}
    {c c₁ c' : Exec.Config O} {l : Exec.Label O}
    (hs : Exec.step P c = some (.next c₁)) (hstep : Exec.Step P c l c') :
    c' = c₁ := by
  cases hstep with
  | silent h =>
    rw [hs] at h
    injection h with h
    injection h with h
    exact h.symm
  | prim h _ _ =>
    rw [hs] at h
    cases h

/-- Helper (inversion). A configuration whose `step` is `none` cannot take a `Step`. (approved: PENDING). -/
private theorem step_det_none_inv {O : Exec.Operands} {P : Exec.Program O}
    {c c' : Exec.Config O} {l : Exec.Label O}
    (hs : Exec.step P c = none) (hstep : Exec.Step P c l c') : False := by
  cases hstep with
  | silent h => rw [hs] at h; cases h
  | prim h _ _ => rw [hs] at h; cases h

/-- Helper (inversion). The moved-owner program `moveConfig` is rejected by `CheckerAccepted`
because after moving `x` to `y` it reaches `.ret (.read x)` where concrete evaluation succeeds
while `permitE` refuses. (approved: PENDING). -/
private theorem move_not_checkerAccepted_inv :
    ¬ CheckerAccepted erasedOps types permitE emptyProg moveConfig := by
  intro hacc
  have hsteps : Exec.Steps emptyProg moveConfig [] _ :=
    .step (.silent rfl) (.step (.silent rfl) (.step (.silent rfl) (.refl _)))
  have hperm := (hacc _ _ hsteps).2
  have hok : erasedEval [(y, 7), (x, 7)] 0 (.read x) ≠ .stuck := by decide
  have hsome := hperm hok
  cases hsome

/-- Helper (inversion). The valid move-and-return program `acceptedMoveConfig`
(`seq (bind y (move x)) (ret (read y))`) satisfies `CheckerAccepted`. (approved: PENDING). -/
private theorem acceptedMove_checkerAccepted_inv :
    CheckerAccepted erasedOps types permitE emptyProg acceptedMoveConfig := by
  intro tr c hsteps
  cases hsteps with
  | refl => exact ⟨by decide, trivial⟩
  | step hs₁ hsteps =>
    obtain rfl := step_det_next_inv rfl hs₁
    cases hsteps with
    | refl =>
      refine ⟨by decide, ⟨fun _ => by decide, fun v s' he => ?_⟩⟩
      intro _
      have he' : instrumentEval erasedOps permitE [(x, 7)] ⟨0, initial⟩ (.move x) = .ok (v, s') := he
      injection he' with hpair
      rcases Prod.mk.inj hpair with ⟨_, rfl⟩
      decide
    | step hs₂ hsteps =>
      obtain rfl := step_det_next_inv rfl hs₂
      cases hsteps with
      | refl => exact ⟨by decide, trivial⟩
      | step hs₃ hsteps =>
        obtain rfl := step_det_next_inv rfl hs₃
        cases hsteps with
        | refl => exact ⟨by decide, fun _ => by decide⟩
        | step hs₄ hsteps =>
          obtain rfl := step_det_next_inv rfl hs₄
          cases hsteps with
          | refl => exact ⟨by decide, trivial⟩
          | step hs₅ _ => exact False.elim (step_det_none_inv rfl hs₅)

/-- Helper (inversion). The unbound-assignment configuration `acceptedStuckConfig`
satisfies `CheckerAccepted` and gets stuck in both the instrumented and erased machines. (approved: PENDING). -/
private theorem acceptedStuck_checkerAccepted_inv :
    CheckerAccepted erasedOps types permitE emptyProg acceptedStuckConfig := by
  intro tr c hsteps
  cases hsteps with
  | refl =>
    exact ⟨by decide, ⟨fun _ => by decide, fun _ _ _ hlook => by cases hlook⟩⟩
  | step hs _ => exact False.elim (step_det_none_inv rfl hs)

/-- Observation. Moving an initialized owner in the instrumented operand instance erases to the concrete value and store. -/
example : ∃ s' : Store Int,
    instrumentedOps.eval [(x, 7)] ⟨0, initial⟩ (.move x) = .ok (7, s') ∧
    erasedOps.eval [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) (.move x) = .ok (7, erase s') := by
  refine ⟨⟨0, (move initial ⟨0⟩).getD initial⟩, by decide, ?_⟩
  exact eval_ok_erasure (O := erasedOps) (types := types) (permit := permitE)
    [(x, 7)] ⟨0, initial⟩ _ (.move x) 7 (by decide)

/-- Rejection observation. A corrupted concrete value disagrees with the erased evaluation of `.move x`. -/
example : ¬ (erasedOps.eval [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) (.move x) = .ok (8, 0)) := by
  decide

/-- Observation. A trapping operand in the instrumented instance traps with the same kind in the erased instance. -/
example : instrumentedOps.eval [(x, 7)] ⟨0, initial⟩ (.fail .abort) = .trap .abort ∧
    erasedOps.eval [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) (.fail .abort) = .trap .abort :=
  ⟨by decide, eval_trap_erasure (O := erasedOps) (types := types) (permit := permitE)
    [(x, 7)] ⟨0, initial⟩ (.fail .abort) .abort (by decide)⟩

/-- Rejection observation. A trapping operand does not erase to a normal value. -/
example : ¬ (erasedOps.eval [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) (.fail .abort) = .ok (0, 0)) := by
  decide

/-- Observation. Left-to-right instrumented argument evaluation erases to the concrete argument values and store. -/
example : ∃ s' : Store Int,
    Exec.evalArgs instrumentedOps [(x, 7)] ⟨0, initial⟩ [.read x, .move x] = .ok ([7, 7], s') ∧
    Exec.evalArgs erasedOps [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) [.read x, .move x] =
      .ok ([7, 7], erase s') := by
  refine ⟨⟨0, (move initial ⟨0⟩).getD initial⟩, by decide, ?_⟩
  exact evalArgs_ok_erasure_result (O := erasedOps) (types := types) (permit := permitE)
    [(x, 7)] [.read x, .move x] ⟨0, initial⟩ _ [7, 7] (by decide)

/-- Rejection observation. Reversing or corrupting the argument list violates erased argument evaluation. -/
example : ¬ (Exec.evalArgs erasedOps [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) [.read x, .move x] =
    .ok ([7, 0], 0)) := by
  decide

/-- Observation. An argument list that traps after a move traps with the same kind in the erased instance. -/
example : Exec.evalArgs instrumentedOps [(x, 7)] ⟨0, initial⟩ [.move x, .fail .overflow] = .trap .overflow ∧
    Exec.evalArgs erasedOps [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) [.move x, .fail .overflow] =
      .trap .overflow :=
  ⟨by decide, evalArgs_trap_erasure_result (O := erasedOps) (types := types) (permit := permitE)
    [(x, 7)] [.move x, .fail .overflow] ⟨0, initial⟩ .overflow (by decide)⟩

/-- Rejection observation. A trapping argument list cannot erase to a different trap kind. -/
example : ¬ (Exec.evalArgs erasedOps [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) [.move x, .fail .overflow] =
    .trap .abort) := by
  decide

/-- Observation. Condition testing agrees between the instrumented and erased instances on both truth values. -/
example : instrumentedOps.test 7 = erasedOps.test 7 ∧
    instrumentedOps.test 0 = erasedOps.test 0 ∧
    erasedOps.test 7 = some true ∧ erasedOps.test 0 = some false :=
  ⟨test_erasure (O := erasedOps) (types := types) (permit := permitE) 7,
   test_erasure (O := erasedOps) (types := types) (permit := permitE) 0,
   by decide, by decide⟩

/-- Rejection observation. A nonzero condition value does not test as `false`. -/
example : ¬ (erasedOps.test 7 = some false) := by decide

/-- Observation. Binding a fresh variable updates the ownership frame and erases to the concrete lifecycle step. -/
example : ∃ s' : Store Int,
    instrumentedOps.life (.bind y) ⟨7, initial⟩ = some s' ∧
    erasedOps.life (.bind y) (erase (⟨7, initial⟩ : Store Int)) = some (erase s') := by
  refine ⟨⟨7, (bind types y initial).getD initial⟩, by decide, ?_⟩
  exact instrumentLife_erasure (O := erasedOps) (types := types) (permit := permitE)
    (.bind y) ⟨7, initial⟩ _ (by decide)

/-- Rejection observation. A concrete lifecycle step cannot alter the concrete store to a different integer. -/
example : ¬ (erasedOps.life (.bind y) (erase (⟨7, initial⟩ : Store Int)) = some 99) := by
  decide

/-- Observation. An allowed primitive answer satisfies the erased primitive contract and preserves the ownership state. -/
example : let a : Exec.Answer Int Int (Store Int) := ⟨some 7, .ok 7, ⟨7, initial⟩⟩
    instrumentedOps.allows () [7] ⟨0, initial⟩ a = true ∧
    erasedOps.allows () [7] (erase (⟨0, initial⟩ : Store Int)) (eraseAnswer a) = true ∧
    a.store.own = initial := by
  dsimp
  exact ⟨by decide, allows_erasure (O := erasedOps) (types := types) (permit := permitE)
    () [7] ⟨0, initial⟩ ⟨some 7, .ok 7, ⟨7, initial⟩⟩ (by decide)⟩

/-- Rejection observation. A primitive answer that mutates the ownership state or emits the wrong event is rejected by `instrumentAllows`. -/
example : ¬ (instrumentedOps.allows () [7] ⟨0, initial⟩
    ⟨some 7, .ok 7, ⟨7, (move initial ⟨0⟩).getD initial⟩⟩ = true) := by
  decide

/-- Observation. A silent `.bind` step on the instrumented machine erases to the matching `.bind` step on the erased machine. -/
example : ∃ c' : Exec.Config instrumentedOps,
    Exec.step emptyProg bindMoveConfig = some (.next c') ∧
    Exec.step (eraseProgram emptyProg) (eraseConfig bindMoveConfig) =
      some (.next (eraseConfig c')) := by
  refine ⟨_, rfl, ?_⟩
  exact step_next_erasure emptyProg bindMoveConfig _ rfl

/-- Rejection observation. A silent step cannot land in a configuration with a different local binding value. -/
example : ¬ (Exec.step (eraseProgram emptyProg) (eraseConfig bindMoveConfig) =
    some (.next ⟨.done, [(y, 99), (x, 7)], 0, []⟩)) := by
  intro h
  cases h

/-- Observation. A primitive request step erases to the concrete request and its continuation commutes with `eraseConfig`. -/
example : ∃ p vs site st k,
    Exec.step emptyProg primOneConfig = some (.request p vs site st k) ∧
    ∃ k_erased,
      Exec.step (eraseProgram emptyProg) (eraseConfig primOneConfig) =
        some (.request p vs site (erase st) k_erased) ∧
      ∀ a c', k a = some c' → k_erased (eraseAnswer a) = some (eraseConfig c') := by
  refine ⟨(), [7], ⟨0⟩, ⟨0, initial⟩, _, rfl, ?_⟩
  exact step_request_erasure emptyProg primOneConfig () [7] ⟨0⟩ ⟨0, initial⟩ _ rfl

/-- Rejection observation. A primitive request step never erases to `none`. -/
example : ¬ (Exec.step (eraseProgram emptyProg) (eraseConfig primOneConfig) = none) := by
  intro h
  cases h

/-- Observation. Both silent and primitive-event single steps of the instrumented machine erase to matching steps of the erased machine. -/
example : ∃ c' : Exec.Config instrumentedOps,
    Exec.Step emptyProg primOneConfig
      (liftAnswer initial ⟨some 7, .ok 7, 7⟩).label c' ∧
    Exec.Step (eraseProgram emptyProg) (eraseConfig primOneConfig)
      (eraseLabel (O := erasedOps) (types := types) (permit := permitE)
        (liftAnswer initial ⟨some 7, .ok 7, 7⟩).label)
      (eraseConfig c') := by
  have hs : Exec.Step emptyProg primOneConfig
      (liftAnswer initial ⟨some 7, .ok 7, 7⟩).label _ :=
    .prim rfl (by decide) rfl
  exact ⟨_, hs, Step_erasure emptyProg primOneConfig _ _ hs⟩

/-- Rejection observation. Erasing an event label never drops or alters its emitted event. -/
example : ¬ ((eraseLabel (O := erasedOps) (types := types) (permit := permitE)
    (liftAnswer initial ⟨some 7, .ok 7, 7⟩).label).events = [8]) := by
  decide

/-- Observation. A multi-step instrumented run emitting two primitive events `[7, 3]` in order erases to a multi-step erased run with the exact same event trace `[7, 3]` and concrete store `10`. -/
example : ∃ c' : Exec.Config instrumentedOps,
    Exec.Steps emptyProg primTraceConfig [7, 3] c' ∧
    Exec.Steps (eraseProgram emptyProg) (eraseConfig primTraceConfig) [7, 3] (eraseConfig c') ∧
    erase c'.store = 10 ∧ c'.final? = some (.returned 7) := by
  have hsteps : Exec.Steps emptyProg primTraceConfig _ _ :=
    .step (.silent rfl)
      (.step (.prim (a := liftAnswer initial ⟨some 7, .ok 7, 7⟩) rfl rfl rfl)
        (.step (.silent rfl)
          (.step (.silent rfl)
            (.step (.prim (a := liftAnswer ((bind types y initial).getD initial) ⟨some 3, .ok 10, 10⟩)
                rfl rfl rfl)
              (.step (.silent rfl)
                (.step (.silent rfl) (.refl _)))))))
  exact ⟨_, hsteps, Steps_erasure emptyProg primTraceConfig _ [7, 3] hsteps, rfl, rfl⟩

/-- Rejection observation. Reversing the event order `[3, 7]` or corrupting the final store falsifies the erased trace report. -/
example : ¬ ((Exec.run (eraseProgram emptyProg) erasedOracle 100 (eraseConfig primTraceConfig)).events = [3, 7]) := by
  decide

/-- Observation. Non-`stuck` fuel-indexed runs (`returned`, `normal`, `trapped`, `noAnswer`, `badAnswer` and `outOfFuel`) erase with identical events and concrete stores. -/
example :
    let ρI := liftOracle erasedOps types permitE erasedOracle
    let cTrap : Exec.Config instrumentedOps :=
      ⟨.exec (.ret (.fail .abort) ⟨0⟩), [(x, 7)], ⟨0, initial⟩, []⟩
    let cNoAns : Exec.Config instrumentedOps :=
      ⟨.exec (.prim none () [.lit 1, .lit 2] ⟨0⟩), [(x, 7)], ⟨0, initial⟩, []⟩
    Exec.runFrom (eraseProgram emptyProg) erasedOracle 100 [1] (eraseConfig primTraceConfig) =
      eraseReport (Exec.runFrom emptyProg ρI 100 [1] primTraceConfig) ∧
    Exec.runFrom (eraseProgram emptyProg) erasedOracle 100 [] (eraseConfig cTrap) =
      eraseReport (Exec.runFrom emptyProg ρI 100 [] cTrap) ∧
    Exec.runFrom (eraseProgram emptyProg) erasedOracle 100 [] (eraseConfig cNoAns) =
      eraseReport (Exec.runFrom emptyProg ρI 100 [] cNoAns) ∧
    Exec.runFrom (eraseProgram emptyProg) badOracle 100 [] (eraseConfig primTraceConfig) =
      eraseReport (Exec.runFrom emptyProg (liftOracle erasedOps types permitE badOracle) 100 [] primTraceConfig) ∧
    Exec.runFrom (eraseProgram emptyProg) erasedOracle 2 [] (eraseConfig primTraceConfig) =
      eraseReport (Exec.runFrom emptyProg ρI 2 [] primTraceConfig) :=
  ⟨runFrom_erasure emptyProg erasedOracle 100 [1] primTraceConfig rfl,
   runFrom_erasure emptyProg erasedOracle 100 [] _ rfl,
   runFrom_erasure emptyProg erasedOracle 100 [] _ rfl,
   runFrom_erasure emptyProg badOracle 100 [] primTraceConfig rfl,
   runFrom_erasure emptyProg erasedOracle 2 [] primTraceConfig rfl⟩

/-- Rejection observation. Dropping the `NotStuck` premise fails on the moved-owner counterexample: the instrumented run is `stuck` while the erased run returns `7`. -/
example : ¬ (Exec.runFrom (eraseProgram emptyProg)
      erasedOracle 100 [] (eraseConfig moveConfig) =
    eraseReport (Exec.runFrom emptyProg (liftOracle erasedOps types permitE erasedOracle) 100 [] moveConfig)) := by
  intro h
  have h1 := congrArg (fun r => Ending.IsStuck r.ending) h
  revert h1
  decide

/-- Observation. A full instrumented run with primitive events `[7, 3]`, store updates `0 → 7 → 10`, and return value `7` erases to the identical concrete run. -/
example :
    Exec.run (eraseProgram emptyProg)
      erasedOracle 100 (eraseConfig primTraceConfig) =
    eraseReport (Exec.run emptyProg (liftOracle erasedOps types permitE erasedOracle) 100 primTraceConfig) ∧
    (Exec.run (eraseProgram emptyProg) erasedOracle 100 (eraseConfig primTraceConfig)).events = [7, 3] ∧
    (Exec.run (eraseProgram emptyProg) erasedOracle 100 (eraseConfig primTraceConfig)).ending =
      .final (.returned 7) :=
  ⟨run_erasure emptyProg erasedOracle 100 primTraceConfig rfl, rfl, rfl⟩

/-- Rejection observation. The moved-owner counterexample refutes unconditional converse erasure on `Exec.run`: instrumented is `stuck`, erased returns `7`. -/
example : ¬ (erasedMoveRun = eraseReport instrumentedMoveRun) := by
  intro h
  have h1 := congrArg (fun r => Ending.IsStuck r.ending) h
  revert h1
  decide

/-- Observation. On checker-accepted configurations (`acceptedMoveConfig` and `acceptedStuckConfig`), the instrumented step is `none` iff the erased step is `none`, while `CheckerAccepted` rejects `moveConfig`. -/
example :
    CheckerAccepted erasedOps types permitE emptyProg acceptedMoveConfig ∧
    CheckerAccepted erasedOps types permitE emptyProg acceptedStuckConfig ∧
    ¬ CheckerAccepted erasedOps types permitE emptyProg moveConfig ∧
    (Exec.step emptyProg acceptedMoveConfig = none ↔
      Exec.step (eraseProgram emptyProg) (eraseConfig acceptedMoveConfig) = none) ∧
    (Exec.step emptyProg acceptedStuckConfig = none ↔
      Exec.step (eraseProgram emptyProg) (eraseConfig acceptedStuckConfig) = none) :=
  ⟨acceptedMove_checkerAccepted_inv,
   acceptedStuck_checkerAccepted_inv,
   move_not_checkerAccepted_inv,
   step_stuck_iff_of_accepted emptyProg acceptedMoveConfig acceptedMove_checkerAccepted_inv,
   step_stuck_iff_of_accepted emptyProg acceptedStuckConfig acceptedStuck_checkerAccepted_inv⟩

/-- Rejection observation. At the unaccepted moved-read step `.ret (.read x)`, the instrumented step is `none` while the erased step succeeds. -/
example :
    let cMovedRead : Exec.Config instrumentedOps :=
      ⟨.exec (.ret (.read x) ⟨1⟩), [(y, 7), (x, 7)], ⟨7, (move initial ⟨0⟩).getD initial⟩, []⟩
    ¬ (Exec.step emptyProg cMovedRead = none ↔
      Exec.step (eraseProgram emptyProg) (eraseConfig cMovedRead) = none) := by
  dsimp
  intro h
  have h1 : Exec.step emptyProg
      ⟨.exec (.ret (.read x) ⟨1⟩), [(y, 7), (x, 7)], ⟨7, (move initial ⟨0⟩).getD initial⟩, []⟩ = none := rfl
  have h2 := h.mp h1
  cases h2

/-- Observation. For both non-stuck (`acceptedMoveConfig`) and stuck (`acceptedStuckConfig`) checker-accepted programs, `runFrom` on the erased machine equals `eraseReport` of the instrumented `runFrom`. -/
example :
    let ρI := liftOracle erasedOps types permitE erasedOracle
    Exec.runFrom (eraseProgram emptyProg) erasedOracle 100 [] (eraseConfig acceptedMoveConfig) =
      eraseReport (Exec.runFrom emptyProg ρI 100 [] acceptedMoveConfig) ∧
    Exec.runFrom (eraseProgram emptyProg) erasedOracle 100 [] (eraseConfig acceptedStuckConfig) =
      eraseReport (Exec.runFrom emptyProg ρI 100 [] acceptedStuckConfig) :=
  ⟨runFrom_erasure_of_accepted emptyProg erasedOracle 100 [] acceptedMoveConfig
      acceptedMove_checkerAccepted_inv,
   runFrom_erasure_of_accepted emptyProg erasedOracle 100 [] acceptedStuckConfig
      acceptedStuck_checkerAccepted_inv⟩

/-- Rejection observation. Without `CheckerAccepted`, `moveConfig` violates `runFrom` erasure because instrumentation refuses `.read x` after `.move x`. -/
example : ¬ (Exec.runFrom (eraseProgram emptyProg)
      erasedOracle 100 [] (eraseConfig moveConfig) =
    eraseReport (Exec.runFrom emptyProg (liftOracle erasedOps types permitE erasedOracle) 100 [] moveConfig)) := by
  intro h
  have h1 := congrArg (fun r => Ending.IsStuck r.ending) h
  revert h1
  decide

/-- Observation. For checker-accepted programs, the instrumented run is `stuck` iff the erased run is `stuck` (`false` on `acceptedMoveConfig`, `true` on `acceptedStuckConfig`). -/
example :
    let ρI := liftOracle erasedOps types permitE erasedOracle
    Ending.IsStuck (Exec.run emptyProg ρI 100 acceptedMoveConfig).ending =
      Ending.IsStuck (Exec.run (eraseProgram emptyProg) erasedOracle 100 (eraseConfig acceptedMoveConfig)).ending ∧
    Ending.IsStuck (Exec.run emptyProg ρI 100 acceptedMoveConfig).ending = false ∧
    Ending.IsStuck (Exec.run emptyProg ρI 100 acceptedStuckConfig).ending =
      Ending.IsStuck (Exec.run (eraseProgram emptyProg) erasedOracle 100 (eraseConfig acceptedStuckConfig)).ending ∧
    Ending.IsStuck (Exec.run emptyProg ρI 100 acceptedStuckConfig).ending = true :=
  ⟨run_stuck_iff_of_accepted emptyProg erasedOracle 100 acceptedMoveConfig acceptedMove_checkerAccepted_inv,
   by decide,
   run_stuck_iff_of_accepted emptyProg erasedOracle 100 acceptedStuckConfig acceptedStuck_checkerAccepted_inv,
   by decide⟩

/-- Rejection observation. Without `CheckerAccepted`, `moveConfig` is stuck in the instrumented run (`true`) and not stuck in the erased run (`false`). -/
example : ¬ (Ending.IsStuck instrumentedMoveRun.ending = Ending.IsStuck erasedMoveRun.ending) := by
  decide

/-- Candidate unconditional converse, deliberately without checker acceptance. The moved-owner
execution below refutes this stronger statement; it is not a control for a conditional law. -/
def UnconditionalRunConverse : Prop :=
  ∀ (fuel : Nat) (c : Exec.Config instrumentedOps),
    Exec.run (eraseProgram emptyProg) erasedOracle fuel (eraseConfig c) =
      eraseReport (Exec.run emptyProg (liftOracle erasedOps types permitE erasedOracle) fuel c)

/-- Helper (computation). The owner-required moved-owner execution refutes the explicitly
unconditional candidate. This makes no claim against the checker-accepted converse.
(approved: PENDING). -/
theorem unconditionalRunConverse_refuted : ¬ UnconditionalRunConverse := by
  intro h
  have he := congrArg (fun r => Ending.IsStuck r.ending) (h 100 moveConfig)
  revert he
  decide

/-- A same-typed faulty erasure that discards control and continuations. It changes an
implementation of erasure, keeping the concrete operand semantics and expected result fixed. -/
def eraseConfigDone {O : Exec.Operands} {types : Var → Option Descriptor}
    {permit : AList Var O.Val → O.Expr → State → Option State}
    (c : Exec.Config (instrumentOperands O types permit)) : Exec.Config O :=
  { eraseConfig c with ctrl := .done, frames := [] }

/-- A same-typed faulty erasure that invents a runnable skip at a stuck configuration. -/
def eraseConfigSkip {O : Exec.Operands} {types : Var → Option Descriptor}
    {permit : AList Var O.Val → O.Expr → State → Option State}
    (c : Exec.Config (instrumentOperands O types permit)) : Exec.Config O :=
  { eraseConfig c with ctrl := .exec .skip, frames := [] }

/-- Instance (law `ConcreteSpec.Core.Own.runFrom_erasure`, case `ConcreteSpec.Core.Own.Examples.primTraceConfig`). -/
example : Exec.runFrom (eraseProgram emptyProg) erasedOracle 100 [] (eraseConfig primTraceConfig) =
    eraseReport (Exec.runFrom emptyProg (liftOracle erasedOps types permitE erasedOracle) 100 [] primTraceConfig) :=
  @runFrom_erasure erasedOps types permitE emptyProg erasedOracle 100 [] primTraceConfig rfl

/-- Planted (law `ConcreteSpec.Core.Own.runFrom_erasure`, case `ConcreteSpec.Core.Own.Examples.primTraceConfig`, subject `ConcreteSpec.Core.Own.eraseConfig`, mutant `ConcreteSpec.Core.Own.ExecErasureWitnesses.eraseConfigDone`). -/
example : Ending.NotStuck (Exec.runFrom emptyProg
      (liftOracle erasedOps types permitE erasedOracle) 100 [] primTraceConfig).ending ∧
    ¬ (Exec.runFrom (eraseProgram emptyProg) erasedOracle 100 [] (eraseConfigDone primTraceConfig) =
      eraseReport (Exec.runFrom emptyProg (liftOracle erasedOps types permitE erasedOracle) 100 [] primTraceConfig)) := by
  exact ⟨rfl, by intro h; have he := congrArg (fun r => r.events) h; cases he⟩

/-- Instance (law `ConcreteSpec.Core.Own.run_erasure`, case `ConcreteSpec.Core.Own.Examples.primTraceConfig`). -/
example : Exec.run (eraseProgram emptyProg) erasedOracle 100 (eraseConfig primTraceConfig) =
    eraseReport (Exec.run emptyProg (liftOracle erasedOps types permitE erasedOracle) 100 primTraceConfig) :=
  @run_erasure erasedOps types permitE emptyProg erasedOracle 100 primTraceConfig rfl

/-- Planted (law `ConcreteSpec.Core.Own.run_erasure`, case `ConcreteSpec.Core.Own.Examples.primTraceConfig`, subject `ConcreteSpec.Core.Own.eraseConfig`, mutant `ConcreteSpec.Core.Own.ExecErasureWitnesses.eraseConfigDone`). -/
example : Ending.NotStuck (Exec.run emptyProg
      (liftOracle erasedOps types permitE erasedOracle) 100 primTraceConfig).ending ∧
    ¬ (Exec.run (eraseProgram emptyProg) erasedOracle 100 (eraseConfigDone primTraceConfig) =
      eraseReport (Exec.run emptyProg (liftOracle erasedOps types permitE erasedOracle) 100 primTraceConfig)) := by
  exact ⟨rfl, by intro h; have he := congrArg (fun r => r.events) h; cases he⟩

/-- Instance (law `ConcreteSpec.Core.Own.step_stuck_iff_of_accepted`, case `ConcreteSpec.Core.Own.Examples.acceptedStuckConfig`). -/
example : Exec.step emptyProg acceptedStuckConfig = none ↔
    Exec.step (eraseProgram emptyProg) (eraseConfig acceptedStuckConfig) = none :=
  @step_stuck_iff_of_accepted erasedOps types permitE emptyProg acceptedStuckConfig
    acceptedStuck_checkerAccepted_inv

/-- Planted (law `ConcreteSpec.Core.Own.step_stuck_iff_of_accepted`, case `ConcreteSpec.Core.Own.Examples.acceptedStuckConfig`, subject `ConcreteSpec.Core.Own.eraseConfig`, mutant `ConcreteSpec.Core.Own.ExecErasureWitnesses.eraseConfigSkip`). -/
example : CheckerAccepted erasedOps types permitE emptyProg acceptedStuckConfig ∧
    ¬ (Exec.step emptyProg acceptedStuckConfig = none ↔
      Exec.step (eraseProgram emptyProg) (eraseConfigSkip acceptedStuckConfig) = none) := by
  exact ⟨acceptedStuck_checkerAccepted_inv, fun h => by cases h.mp rfl⟩

/-- Instance (law `ConcreteSpec.Core.Own.runFrom_erasure_of_accepted`, case `ConcreteSpec.Core.Own.Examples.acceptedMoveConfig`). -/
example : Exec.runFrom (eraseProgram emptyProg) erasedOracle 100 [] (eraseConfig acceptedMoveConfig) =
    eraseReport (Exec.runFrom emptyProg (liftOracle erasedOps types permitE erasedOracle) 100 [] acceptedMoveConfig) :=
  @runFrom_erasure_of_accepted erasedOps types permitE emptyProg erasedOracle 100 []
    acceptedMoveConfig acceptedMove_checkerAccepted_inv

/-- Planted (law `ConcreteSpec.Core.Own.runFrom_erasure_of_accepted`, case `ConcreteSpec.Core.Own.Examples.acceptedMoveConfig`, subject `ConcreteSpec.Core.Own.eraseConfig`, mutant `ConcreteSpec.Core.Own.ExecErasureWitnesses.eraseConfigDone`). -/
example : CheckerAccepted erasedOps types permitE emptyProg acceptedMoveConfig ∧
    ¬ (Exec.runFrom (eraseProgram emptyProg) erasedOracle 100 [] (eraseConfigDone acceptedMoveConfig) =
      eraseReport (Exec.runFrom emptyProg (liftOracle erasedOps types permitE erasedOracle) 100 [] acceptedMoveConfig)) := by
  exact ⟨acceptedMove_checkerAccepted_inv,
    by intro h; have he := congrArg (fun r => r.ending) h; cases he⟩

/-- Instance (law `ConcreteSpec.Core.Own.run_stuck_iff_of_accepted`, case `ConcreteSpec.Core.Own.Examples.acceptedStuckConfig`). -/
example : Ending.IsStuck (Exec.run emptyProg (liftOracle erasedOps types permitE erasedOracle) 100 acceptedStuckConfig).ending =
    Ending.IsStuck (Exec.run (eraseProgram emptyProg) erasedOracle 100 (eraseConfig acceptedStuckConfig)).ending :=
  @run_stuck_iff_of_accepted erasedOps types permitE emptyProg erasedOracle 100
    acceptedStuckConfig acceptedStuck_checkerAccepted_inv

/-- Planted (law `ConcreteSpec.Core.Own.run_stuck_iff_of_accepted`, case `ConcreteSpec.Core.Own.Examples.acceptedStuckConfig`, subject `ConcreteSpec.Core.Own.eraseConfig`, mutant `ConcreteSpec.Core.Own.ExecErasureWitnesses.eraseConfigDone`). -/
example : CheckerAccepted erasedOps types permitE emptyProg acceptedStuckConfig ∧
    ¬ (Ending.IsStuck (Exec.run emptyProg (liftOracle erasedOps types permitE erasedOracle) 100 acceptedStuckConfig).ending =
      Ending.IsStuck (Exec.run (eraseProgram emptyProg) erasedOracle 100 (eraseConfigDone acceptedStuckConfig)).ending) := by
  exact ⟨acceptedStuck_checkerAccepted_inv, by decide⟩

/-- The exact successor of the closed move-binding step. -/
def bindSuccessor : Exec.Config instrumentedOps :=
  match Exec.step emptyProg bindMoveConfig with
  | some (.next c) => c
  | _ => bindMoveConfig

/-- The exact continuation of the closed primitive request. -/
def primitiveContinuation : Exec.Answer Int Int (Store Int) → Option (Exec.Config instrumentedOps) :=
  match Exec.step emptyProg primOneConfig with
  | some (.request _ _ _ _ k) => k
  | _ => fun _ => none

def primitiveSuccessor : Exec.Config instrumentedOps :=
  (primitiveContinuation (liftAnswer initial ⟨some 7, .ok 7, 7⟩)).getD primOneConfig

/-- Helper (computation). Closed event-producing step used as a law hypothesis.
(approved: PENDING). -/
private theorem primitiveStep : Exec.Step emptyProg primOneConfig (.emit 7) primitiveSuccessor :=
  @Exec.Step.prim instrumentedOps emptyProg primOneConfig primitiveSuccessor () [7] ⟨0⟩
    ⟨0, initial⟩ primitiveContinuation (liftAnswer initial ⟨some 7, .ok 7, 7⟩) rfl rfl rfl

/-- Helper (computation). Closed nonempty trace used as a law hypothesis.
(approved: PENDING). -/
private theorem primitiveSteps : Exec.Steps emptyProg primOneConfig [7] primitiveSuccessor :=
  .step primitiveStep (.refl _)

/-- Instance (law `ConcreteSpec.Core.Own.step_next_erasure`, case `ConcreteSpec.Core.Own.Examples.bindMoveConfig`). -/
example : Exec.step (eraseProgram emptyProg) (eraseConfig bindMoveConfig) =
    some (.next (eraseConfig bindSuccessor)) :=
  @step_next_erasure erasedOps types permitE emptyProg bindMoveConfig bindSuccessor rfl

/-- Planted (law `ConcreteSpec.Core.Own.step_next_erasure`, case `ConcreteSpec.Core.Own.Examples.bindMoveConfig`, subject `ConcreteSpec.Core.Own.eraseConfig`, mutant `ConcreteSpec.Core.Own.ExecErasureWitnesses.eraseConfigDone`). -/
example : Exec.step emptyProg bindMoveConfig = some (.next bindSuccessor) ∧
    ¬ (Exec.step (eraseProgram emptyProg) (eraseConfigDone bindMoveConfig) =
      some (.next (eraseConfigDone bindSuccessor))) := ⟨rfl, by intro h; cases h⟩

/-- Instance (law `ConcreteSpec.Core.Own.step_request_erasure`, case `ConcreteSpec.Core.Own.Examples.primOneConfig`). -/
example : ∃ k_erased,
    Exec.step (eraseProgram emptyProg) (eraseConfig primOneConfig) =
      some (.request () [7] ⟨0⟩ (erase (⟨0, initial⟩ : Store Int)) k_erased) ∧
    ∀ a c', primitiveContinuation a = some c' →
      k_erased (eraseAnswer a) = some (eraseConfig c') :=
  @step_request_erasure erasedOps types permitE emptyProg primOneConfig () [7] ⟨0⟩
    ⟨0, initial⟩ primitiveContinuation rfl

/-- Planted (law `ConcreteSpec.Core.Own.step_request_erasure`, case `ConcreteSpec.Core.Own.Examples.primOneConfig`, subject `ConcreteSpec.Core.Own.eraseConfig`, mutant `ConcreteSpec.Core.Own.ExecErasureWitnesses.eraseConfigDone`). -/
example : Exec.step emptyProg primOneConfig =
      some (.request () [7] ⟨0⟩ ⟨0, initial⟩ primitiveContinuation) ∧
    ¬ (∃ k_erased,
      Exec.step (eraseProgram emptyProg) (eraseConfigDone primOneConfig) =
        some (.request () [7] ⟨0⟩ (erase (⟨0, initial⟩ : Store Int)) k_erased) ∧
      ∀ a c', primitiveContinuation a = some c' →
        k_erased (eraseAnswer a) = some (eraseConfigDone c')) := by
  refine ⟨rfl, ?_⟩
  rintro ⟨_, h, _⟩
  cases h

/-- Instance (law `ConcreteSpec.Core.Own.Step_erasure`, case `ConcreteSpec.Core.Own.Examples.primOneConfig`). -/
example : Exec.Step (eraseProgram emptyProg) (eraseConfig primOneConfig)
    (eraseLabel (O := erasedOps) (types := types) (permit := permitE) (.emit 7))
    (eraseConfig primitiveSuccessor) :=
  @Step_erasure erasedOps types permitE emptyProg primOneConfig primitiveSuccessor (.emit 7) primitiveStep

/-- Planted (law `ConcreteSpec.Core.Own.Step_erasure`, case `ConcreteSpec.Core.Own.Examples.primOneConfig`, subject `ConcreteSpec.Core.Own.eraseConfig`, mutant `ConcreteSpec.Core.Own.ExecErasureWitnesses.eraseConfigDone`). -/
example : Exec.Step emptyProg primOneConfig (.emit 7) primitiveSuccessor ∧
    ¬ Exec.Step (eraseProgram emptyProg) (eraseConfigDone primOneConfig)
      (eraseLabel (O := erasedOps) (types := types) (permit := permitE) (.emit 7))
      (eraseConfigDone primitiveSuccessor) :=
  ⟨primitiveStep, step_det_none_inv rfl⟩

/-- Instance (law `ConcreteSpec.Core.Own.Steps_erasure`, case `ConcreteSpec.Core.Own.Examples.primOneConfig`). -/
example : Exec.Steps (eraseProgram emptyProg) (eraseConfig primOneConfig) [7]
    (eraseConfig primitiveSuccessor) :=
  @Steps_erasure erasedOps types permitE emptyProg primOneConfig primitiveSuccessor [7] primitiveSteps

/-- Planted (law `ConcreteSpec.Core.Own.Steps_erasure`, case `ConcreteSpec.Core.Own.Examples.primOneConfig`, subject `ConcreteSpec.Core.Own.eraseConfig`, mutant `ConcreteSpec.Core.Own.ExecErasureWitnesses.eraseConfigDone`). -/
example : Exec.Steps emptyProg primOneConfig [7] primitiveSuccessor ∧
    ¬ Exec.Steps (eraseProgram emptyProg) (eraseConfigDone primOneConfig) [7]
      (eraseConfigDone primitiveSuccessor) := by
  refine ⟨primitiveSteps, ?_⟩
  intro hs
  have traceEmpty : ∀ {tr c'},
      Exec.Steps (eraseProgram emptyProg) (eraseConfigDone primOneConfig) tr c' → tr = [] := by
    intro tr c' h
    cases h with
    | refl => rfl
    | step first _ => exact False.elim (step_det_none_inv rfl first)
  have he := traceEmpty hs
  cases he

end ExecErasureWitnesses

/-- Preservation is a property of the actual result, so an operation mutant can falsify
it without changing a fixed output argument or the well-formed input premise. -/
def StateResult : Option State → Prop
  | none => True
  | some s => WellFormed s

def CallOwnResult : Option CallState → Prop
  | none => True
  | some s => WellFormed s.own

def CallResult : Option CallState → Prop
  | none => True
  | some s => CallState.WellFormed s

instance (r : Option State) : Decidable (StateResult r) := by
  cases r <;> unfold StateResult <;> infer_instance
instance (r : Option CallState) : Decidable (CallOwnResult r) := by
  cases r <;> unfold CallOwnResult <;> infer_instance
instance (r : Option CallState) : Decidable (CallResult r) := by
  cases r <;> unfold CallResult <;> infer_instance

/-- Law (subject `ConcreteSpec.Core.Own.move`). Every successful result preserves
ownership; refusal does not fabricate a result. -/
theorem move_preserves (s : State) (p : Hybrid.Place) (hs : WellFormed s) : StateResult (move s p) := by
  cases he : move s p with
  | none => trivial
  | some t => exact move_preserves_result s t p hs he

/-- Law (subject `ConcreteSpec.Core.Own.reinitialize`). Every successful result preserves
ownership; refusal does not fabricate a result. -/
theorem reinitialize_preserves (s : State) (p : Hybrid.Place) (hs : WellFormed s) : StateResult (reinitialize s p) := by
  cases he : reinitialize s p with
  | none => trivial
  | some t => exact reinitialize_preserves_result s t p hs he

/-- Law (subject `ConcreteSpec.Core.Own.borrow`). Every successful result preserves
ownership; refusal does not fabricate a result. -/
theorem borrow_preserves (s : State) (p : Hybrid.Place) (path : List Projection) (m : Mode) (hs : WellFormed s) : StateResult (borrow s p path m) := by
  cases he : borrow s p path m with
  | none => trivial
  | some t => exact borrow_preserves_result s t p path m hs he

/-- Law (subject `ConcreteSpec.Core.Own.reborrowTo`). Every successful result preserves
ownership; refusal does not fabricate a result. -/
theorem reborrowTo_preserves (s : State) (i : LoanId) (path : List Projection) (m : Mode) (a : Nat) (hs : WellFormed s) : StateResult (reborrowTo s i path m a) := by
  cases he : reborrowTo s i path m a with
  | none => trivial
  | some t => exact reborrowTo_preserves_result s t i path m a hs he

/-- Law (subject `ConcreteSpec.Core.Own.reborrow`). Every successful result preserves
ownership; refusal does not fabricate a result. -/
theorem reborrow_preserves (s : State) (i : LoanId) (path : List Projection) (m : Mode) (hs : WellFormed s) : StateResult (reborrow s i path m) := by
  cases he : reborrow s i path m with
  | none => trivial
  | some t => exact reborrow_preserves_result s t i path m hs he

/-- Law (subject `ConcreteSpec.Core.Own.endLoan`). Every successful result preserves
ownership; refusal does not fabricate a result. -/
theorem endLoan_preserves (s : State) (i : LoanId) (hs : WellFormed s) : StateResult (endLoan s i) := by
  cases he : endLoan s i with
  | none => trivial
  | some t => exact endLoan_preserves_result s t i hs he

/-- Law (subject `ConcreteSpec.Core.Own.bind`). Every successful result preserves
ownership; refusal does not fabricate a result. -/
theorem bind_preserves (types : Var → Option Descriptor) (s : State) (x : Var) (hs : WellFormed s) : StateResult (bind types x s) := by
  cases he : bind types x s with
  | none => trivial
  | some t => exact bind_preserves_result types s t x hs he

/-- Law (subject `ConcreteSpec.Core.Own.drop`). Every successful result preserves
ownership; refusal does not fabricate a result. -/
theorem drop_preserves (s : State) (xs : List Var) (hs : WellFormed s) : StateResult (drop xs s) := by
  cases he : drop xs s with
  | none => trivial
  | some t => exact drop_preserves_result s t xs hs he

/-- Law (subject `ConcreteSpec.Core.Own.enter`). Every successful result preserves
ownership; refusal does not fabricate a result. -/
theorem enter_preserves (types : Var → Option Descriptor) (s : State) (xs : List Var) (hs : WellFormed s) : StateResult (enter types xs s) := by
  cases he : enter types xs s with
  | none => trivial
  | some t => exact enter_preserves_result types s t xs hs he

/-- Law (subject `ConcreteSpec.Core.Own.endLocals`). Every successful result preserves
ownership; refusal does not fabricate a result. -/
theorem endLocals_preserves (s : State) (a : Nat) (hs : WellFormed s) : StateResult (endLocals a s) := by
  cases he : endLocals a s with
  | none => trivial
  | some t => exact endLocals_preserves_result s t a hs he

/-- Law (subject `ConcreteSpec.Core.Own.leave`). Every successful result preserves
ownership; refusal does not fabricate a result. -/
theorem leave_preserves (s : State) (hs : WellFormed s) : StateResult (leave s) := by
  cases he : leave s with
  | none => trivial
  | some t => exact leave_preserves_result s t hs he

/-- Law (subject `ConcreteSpec.Core.Own.enterDown`). Every successful result preserves
ownership; refusal does not fabricate a result. -/
theorem enterDown_preserves (s : State) (args : List (Var × LoanId)) (hs : WellFormed s) : StateResult (enterDown s args) := by
  cases he : enterDown s args with
  | none => trivial
  | some t => exact enterDown_preserves_result s t args hs he

/-- Law (subject `ConcreteSpec.Core.Own.life`). Every successful result preserves
ownership; refusal does not fabricate a result. -/
theorem life_preserves (types : Var → Option Descriptor) (e : Exec.Life) (s : State) (hs : WellFormed s) : StateResult (life types e s) := by
  cases he : life types e s with
  | none => trivial
  | some t => exact life_preserves_result types e s t hs he

/-- Law (subject `ConcreteSpec.Core.Own.adoptDown`). Every successful result preserves
ownership; refusal does not fabricate a result. -/
theorem adoptDown_preserves (s : State) (args : List (Var × LoanId)) (hs : WellFormed s) : StateResult (adoptDown s args) := by
  cases he : adoptDown s args with
  | none => trivial
  | some t => exact adoptDown_preserves_result s t args hs he

/-- Law (subject `ConcreteSpec.Core.Own.passDown`). Every successful result preserves
ownership; refusal does not fabricate a result. -/
theorem passDown_preserves (s : CallState) (i : LoanId) (x : Var) (hs : WellFormed s.own) : CallOwnResult (passDown s i x) := by
  cases he : passDown s i x with
  | none => trivial
  | some t => exact passDown_preserves_result s t i x hs he

/-- Law (subject `ConcreteSpec.Core.Own.passDown`). Every successful result preserves
ownership; refusal does not fabricate a result. -/
theorem passDown_call_preserves (s : CallState) (i : LoanId) (x : Var) (hs : CallState.WellFormed s) : CallResult (passDown s i x) := by
  cases he : passDown s i x with
  | none => trivial
  | some t => exact passDown_call_preserves_result s t i x hs he

/-- Law (subject `ConcreteSpec.Core.Own.callLife`). Every successful result preserves
ownership; refusal does not fabricate a result. -/
theorem callLife_preserves (types : Var → Option Descriptor) (e : Exec.Life) (s : CallState) (hs : WellFormed s.own) : CallOwnResult (callLife types e s) := by
  cases he : callLife types e s with
  | none => trivial
  | some t => exact callLife_preserves_result types e s t hs he

/-- Law (subject `ConcreteSpec.Core.Own.callLife`). Every successful result preserves
ownership; refusal does not fabricate a result. -/
theorem callLife_call_preserves (types : Var → Option Descriptor) (e : Exec.Life) (s : CallState) (hs : CallState.WellFormed s) : CallResult (callLife types e s) := by
  cases he : callLife types e s with
  | none => trivial
  | some t => exact callLife_call_preserves_result types e s t hs he

/-- Law (subject `ConcreteSpec.Core.Own.passOwner`). Every successful result preserves
ownership; refusal does not fabricate a result. -/
theorem passOwner_preserves (s : CallState) (p : Hybrid.Place) (path : List Projection) (m : Mode) (x : Var) (hs : WellFormed s.own) : CallOwnResult (passOwner s p path m x) := by
  cases he : passOwner s p path m x with
  | none => trivial
  | some t => exact passOwner_preserves_result s t p path m x hs he

/-- Law (subject `ConcreteSpec.Core.Own.passOwner`). Every successful result preserves
ownership; refusal does not fabricate a result. -/
theorem passOwner_call_preserves (s : CallState) (p : Hybrid.Place) (path : List Projection) (m : Mode) (x : Var) (hs : CallState.WellFormed s) : CallResult (passOwner s p path m x) := by
  cases he : passOwner s p path m x with
  | none => trivial
  | some t => exact passOwner_call_preserves_result s t p path m x hs he

namespace PreservationEvidence
/-- A fresh callee with a pending direct owner argument, before adoption. -/
def adoptionInput : State :=
  (enter Examples.types [] DirectArgumentWitnesses.waiting.own).getD Examples.initial

/-- A duplicate typed owner identity, as produced by a broken allocator or cleanup.
The corruption is in instrumentation, not in the expected well-formedness predicate. -/
def corruptState (s : State) : State :=
  { s with slots := Examples.initial.slots ++ Examples.initial.slots }

/-- Mutant: corrupt the slot inventory after a successful operation. -/
def moveCorrupt (s : State) (p : Hybrid.Place) := (move s p).map corruptState

/-- Mutant: corrupt the slot inventory after a successful operation. -/
def reinitializeCorrupt (s : State) (p : Hybrid.Place) := (reinitialize s p).map corruptState

/-- Mutant: corrupt the slot inventory after a successful operation. -/
def borrowCorrupt (s : State) (p : Hybrid.Place) (path : List Projection) (m : Mode) := (borrow s p path m).map corruptState

/-- Mutant: corrupt the slot inventory after a successful operation. -/
def reborrowToCorrupt (s : State) (i : LoanId) (path : List Projection) (m : Mode) (a : Nat) := (reborrowTo s i path m a).map corruptState

/-- Mutant: corrupt the slot inventory after a successful operation. -/
def reborrowCorrupt (s : State) (i : LoanId) (path : List Projection) (m : Mode) := (reborrow s i path m).map corruptState

/-- Mutant: corrupt the slot inventory after a successful operation. -/
def endLoanCorrupt (s : State) (i : LoanId) := (endLoan s i).map corruptState

/-- Mutant: corrupt the slot inventory after a successful operation. -/
def bindCorrupt (types : Var → Option Descriptor) (x : Var) (s : State) := (bind types x s).map corruptState

/-- Mutant: corrupt the slot inventory after a successful operation. -/
def dropCorrupt (xs : List Var) (s : State) := (drop xs s).map corruptState

/-- Mutant: corrupt the slot inventory after a successful operation. -/
def enterCorrupt (types : Var → Option Descriptor) (xs : List Var) (s : State) := (enter types xs s).map corruptState

/-- Mutant: corrupt the slot inventory after a successful operation. -/
def endLocalsCorrupt (a : Nat) (s : State) := (endLocals a s).map corruptState

/-- Mutant: corrupt the slot inventory after a successful operation. -/
def leaveCorrupt (s : State) := (leave s).map corruptState

/-- Mutant: corrupt the slot inventory after a successful operation. -/
def enterDownCorrupt (s : State) (args : List (Var × LoanId)) := (enterDown s args).map corruptState

/-- Mutant: corrupt the slot inventory after a successful operation. -/
def lifeCorrupt (types : Var → Option Descriptor) (e : Exec.Life) (s : State) := (life types e s).map corruptState

/-- Mutant: corrupt the slot inventory after a successful operation. -/
def adoptDownCorrupt (s : State) (args : List (Var × LoanId)) := (adoptDown s args).map corruptState

/-- Mutant: corrupt the slot inventory after a successful operation. -/
def passDownCorrupt (s : CallState) (i : LoanId) (x : Var) := (passDown s i x).map fun t => { t with own := corruptState t.own }

/-- Mutant: corrupt the slot inventory after a successful operation. -/
def callLifeCorrupt (types : Var → Option Descriptor) (e : Exec.Life) (s : CallState) := (callLife types e s).map fun t => { t with own := corruptState t.own }

/-- Mutant: corrupt the slot inventory after a successful operation. -/
def passOwnerCorrupt (s : CallState) (p : Hybrid.Place) (path : List Projection) (m : Mode) (x : Var) := (passOwner s p path m x).map fun t => { t with own := corruptState t.own }

end PreservationEvidence

namespace PreservationEvidence
open Examples

/-- Instance (law `ConcreteSpec.Core.Own.move_preserves`, case `ConcreteSpec.Core.Own.Examples.initial`). -/
example : StateResult (move (initial) (⟨0⟩)) :=
  @move_preserves initial ⟨0⟩ (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.move_preserves`, case `ConcreteSpec.Core.Own.Examples.initial`, subject `ConcreteSpec.Core.Own.move`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.moveCorrupt`). -/
example : (WellFormed (initial)) ∧ ¬ StateResult (moveCorrupt (initial) (⟨0⟩)) := by decide

/-- Instance (law `ConcreteSpec.Core.Own.reinitialize_preserves`, case `ConcreteSpec.Core.Own.LifecycleWitnesses.moved`). -/
example : StateResult (reinitialize (LifecycleWitnesses.moved) (⟨0⟩)) :=
  @reinitialize_preserves LifecycleWitnesses.moved ⟨0⟩ (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.reinitialize_preserves`, case `ConcreteSpec.Core.Own.LifecycleWitnesses.moved`, subject `ConcreteSpec.Core.Own.reinitialize`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.reinitializeCorrupt`). -/
example : (WellFormed (LifecycleWitnesses.moved)) ∧ ¬ StateResult (reinitializeCorrupt (LifecycleWitnesses.moved) (⟨0⟩)) := by decide

/-- Instance (law `ConcreteSpec.Core.Own.borrow_preserves`, case `ConcreteSpec.Core.Own.Examples.shared`). -/
example : StateResult (borrow (shared) (⟨0⟩) ([.index 1]) (.shared)) :=
  @borrow_preserves shared ⟨0⟩ [.index 1] .shared (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.borrow_preserves`, case `ConcreteSpec.Core.Own.Examples.shared`, subject `ConcreteSpec.Core.Own.borrow`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.borrowCorrupt`). -/
example : (WellFormed (shared)) ∧ ¬ StateResult (borrowCorrupt (shared) (⟨0⟩) ([.index 1]) (.shared)) := by decide

/-- Instance (law `ConcreteSpec.Core.Own.reborrowTo_preserves`, case `ConcreteSpec.Core.Own.Examples.exclusive`). -/
example : StateResult (reborrowTo (exclusive) (⟨0⟩) ([.index 1]) (.mutable) (0)) :=
  @reborrowTo_preserves exclusive ⟨0⟩ [.index 1] .mutable 0 (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.reborrowTo_preserves`, case `ConcreteSpec.Core.Own.Examples.exclusive`, subject `ConcreteSpec.Core.Own.reborrowTo`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.reborrowToCorrupt`). -/
example : (WellFormed (exclusive)) ∧ ¬ StateResult (reborrowToCorrupt (exclusive) (⟨0⟩) ([.index 1]) (.mutable) (0)) := by decide

/-- Instance (law `ConcreteSpec.Core.Own.reborrow_preserves`, case `ConcreteSpec.Core.Own.Examples.exclusive`). -/
example : StateResult (reborrow (exclusive) (⟨0⟩) ([.index 1]) (.mutable)) :=
  @reborrow_preserves exclusive ⟨0⟩ [.index 1] .mutable (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.reborrow_preserves`, case `ConcreteSpec.Core.Own.Examples.exclusive`, subject `ConcreteSpec.Core.Own.reborrow`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.reborrowCorrupt`). -/
example : (WellFormed (exclusive)) ∧ ¬ StateResult (reborrowCorrupt (exclusive) (⟨0⟩) ([.index 1]) (.mutable)) := by decide

/-- Instance (law `ConcreteSpec.Core.Own.endLoan_preserves`, case `ConcreteSpec.Core.Own.Examples.child`). -/
example : StateResult (endLoan (child) (⟨1⟩)) :=
  @endLoan_preserves child ⟨1⟩ (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.endLoan_preserves`, case `ConcreteSpec.Core.Own.Examples.child`, subject `ConcreteSpec.Core.Own.endLoan`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.endLoanCorrupt`). -/
example : (WellFormed (child)) ∧ ¬ StateResult (endLoanCorrupt (child) (⟨1⟩)) := by decide

/-- Instance (law `ConcreteSpec.Core.Own.bind_preserves`, case `ConcreteSpec.Core.Own.Examples.shared`). -/
example : StateResult (bind (types) (y) (shared)) :=
  @bind_preserves types shared y (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.bind_preserves`, case `ConcreteSpec.Core.Own.Examples.shared`, subject `ConcreteSpec.Core.Own.bind`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.bindCorrupt`). -/
example : (WellFormed (shared)) ∧ ¬ StateResult (bindCorrupt (types) (y) (shared)) := by decide

/-- Instance (law `ConcreteSpec.Core.Own.drop_preserves`, case `ConcreteSpec.Core.Own.LifecycleWitnesses.moved`). -/
example : StateResult (drop ([x]) (LifecycleWitnesses.moved)) :=
  @drop_preserves LifecycleWitnesses.moved [x] (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.drop_preserves`, case `ConcreteSpec.Core.Own.LifecycleWitnesses.moved`, subject `ConcreteSpec.Core.Own.drop`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.dropCorrupt`). -/
example : (WellFormed (LifecycleWitnesses.moved)) ∧ ¬ StateResult (dropCorrupt ([x]) (LifecycleWitnesses.moved)) := by decide

/-- Instance (law `ConcreteSpec.Core.Own.enter_preserves`, case `ConcreteSpec.Core.Own.Examples.shared`). -/
example : StateResult (enter (types) ([y]) (shared)) :=
  @enter_preserves types shared [y] (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.enter_preserves`, case `ConcreteSpec.Core.Own.Examples.shared`, subject `ConcreteSpec.Core.Own.enter`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.enterCorrupt`). -/
example : (WellFormed (shared)) ∧ ¬ StateResult (enterCorrupt (types) ([y]) (shared)) := by decide

/-- Instance (law `ConcreteSpec.Core.Own.endLocals_preserves`, case `ConcreteSpec.Core.Own.LifecycleWitnesses.called`). -/
example : StateResult (endLocals (1) (LifecycleWitnesses.called)) :=
  @endLocals_preserves LifecycleWitnesses.called 1 (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.endLocals_preserves`, case `ConcreteSpec.Core.Own.LifecycleWitnesses.called`, subject `ConcreteSpec.Core.Own.endLocals`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.endLocalsCorrupt`). -/
example : (WellFormed (LifecycleWitnesses.called)) ∧ ¬ StateResult (endLocalsCorrupt (1) (LifecycleWitnesses.called)) := by decide

/-- Instance (law `ConcreteSpec.Core.Own.leave_preserves`, case `ConcreteSpec.Core.Own.RoundThreeWitnesses.localLoan`). -/
example : StateResult (leave (RoundThreeWitnesses.localLoan)) :=
  @leave_preserves RoundThreeWitnesses.localLoan (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.leave_preserves`, case `ConcreteSpec.Core.Own.RoundThreeWitnesses.localLoan`, subject `ConcreteSpec.Core.Own.leave`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.leaveCorrupt`). -/
example : (WellFormed (RoundThreeWitnesses.localLoan)) ∧ ¬ StateResult (leaveCorrupt (RoundThreeWitnesses.localLoan)) := by decide

/-- Instance (law `ConcreteSpec.Core.Own.enterDown_preserves`, case `ConcreteSpec.Core.Own.Examples.exclusive`). -/
example : StateResult (enterDown (exclusive) ([(y, ⟨0⟩)])) :=
  @enterDown_preserves exclusive [(y, ⟨0⟩)] (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.enterDown_preserves`, case `ConcreteSpec.Core.Own.Examples.exclusive`, subject `ConcreteSpec.Core.Own.enterDown`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.enterDownCorrupt`). -/
example : (WellFormed (exclusive)) ∧ ¬ StateResult (enterDownCorrupt (exclusive) ([(y, ⟨0⟩)])) := by decide

/-- Instance (law `ConcreteSpec.Core.Own.life_preserves`, case `ConcreteSpec.Core.Own.Examples.shared`). -/
example : StateResult (life (types) ((.bind y)) (shared)) :=
  @life_preserves types (.bind y) shared (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.life_preserves`, case `ConcreteSpec.Core.Own.Examples.shared`, subject `ConcreteSpec.Core.Own.life`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.lifeCorrupt`). -/
example : (WellFormed (shared)) ∧ ¬ StateResult (lifeCorrupt (types) ((.bind y)) (shared)) := by decide

/-- Instance (law `ConcreteSpec.Core.Own.adoptDown_preserves`, case `ConcreteSpec.Core.Own.PreservationEvidence.adoptionInput`). -/
example : StateResult (adoptDown (adoptionInput) (DirectArgumentWitnesses.waiting.pending)) :=
  @adoptDown_preserves adoptionInput DirectArgumentWitnesses.waiting.pending (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.adoptDown_preserves`, case `ConcreteSpec.Core.Own.PreservationEvidence.adoptionInput`, subject `ConcreteSpec.Core.Own.adoptDown`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.adoptDownCorrupt`). -/
example : (WellFormed (adoptionInput)) ∧ ¬ StateResult (adoptDownCorrupt (adoptionInput) (DirectArgumentWitnesses.waiting.pending)) := by decide

/-- Instance (law `ConcreteSpec.Core.Own.passDown_preserves`, case `ConcreteSpec.Core.Own.Examples.exclusive`). -/
example : CallOwnResult (passDown (⟨exclusive, []⟩) (⟨0⟩) (y)) :=
  @passDown_preserves ⟨exclusive, []⟩ ⟨0⟩ y (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.passDown_preserves`, case `ConcreteSpec.Core.Own.Examples.exclusive`, subject `ConcreteSpec.Core.Own.passDown`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.passDownCorrupt`). -/
example : (WellFormed (⟨exclusive, []⟩ : CallState).own) ∧ ¬ CallOwnResult (passDownCorrupt (⟨exclusive, []⟩) (⟨0⟩) (y)) := by decide

/-- Instance (law `ConcreteSpec.Core.Own.passDown_call_preserves`, case `ConcreteSpec.Core.Own.Examples.exclusive`). -/
example : CallResult (passDown (⟨exclusive, []⟩) (⟨0⟩) (y)) :=
  @passDown_call_preserves ⟨exclusive, []⟩ ⟨0⟩ y (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.passDown_call_preserves`, case `ConcreteSpec.Core.Own.Examples.exclusive`, subject `ConcreteSpec.Core.Own.passDown`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.passDownCorrupt`). -/
example : (CallState.WellFormed (⟨exclusive, []⟩)) ∧ ¬ CallResult (passDownCorrupt (⟨exclusive, []⟩) (⟨0⟩) (y)) := by decide

/-- Instance (law `ConcreteSpec.Core.Own.callLife_preserves`, case `ConcreteSpec.Core.Own.CallWitnesses.waiting`). -/
example : CallOwnResult (callLife (types) ((.enter [y])) (CallWitnesses.waiting)) :=
  @callLife_preserves types (.enter [y]) CallWitnesses.waiting (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.callLife_preserves`, case `ConcreteSpec.Core.Own.CallWitnesses.waiting`, subject `ConcreteSpec.Core.Own.callLife`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.callLifeCorrupt`). -/
example : (WellFormed (CallWitnesses.waiting).own) ∧ ¬ CallOwnResult (callLifeCorrupt (types) ((.enter [y])) (CallWitnesses.waiting)) := by decide

/-- Instance (law `ConcreteSpec.Core.Own.callLife_call_preserves`, case `ConcreteSpec.Core.Own.CallWitnesses.waiting`). -/
example : CallResult (callLife (types) ((.enter [y])) (CallWitnesses.waiting)) :=
  @callLife_call_preserves types (.enter [y]) CallWitnesses.waiting (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.callLife_call_preserves`, case `ConcreteSpec.Core.Own.CallWitnesses.waiting`, subject `ConcreteSpec.Core.Own.callLife`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.callLifeCorrupt`). -/
example : (CallState.WellFormed (CallWitnesses.waiting)) ∧ ¬ CallResult (callLifeCorrupt (types) ((.enter [y])) (CallWitnesses.waiting)) := by decide

/-- Instance (law `ConcreteSpec.Core.Own.passOwner_preserves`, case `ConcreteSpec.Core.Own.DirectArgumentWitnesses.caller`). -/
example : CallOwnResult (passOwner (DirectArgumentWitnesses.caller) (⟨1⟩) ([.index 3]) (.mutable) (DirectArgumentWitnesses.z)) :=
  @passOwner_preserves DirectArgumentWitnesses.caller ⟨1⟩ [.index 3] .mutable DirectArgumentWitnesses.z (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.passOwner_preserves`, case `ConcreteSpec.Core.Own.DirectArgumentWitnesses.caller`, subject `ConcreteSpec.Core.Own.passOwner`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.passOwnerCorrupt`). -/
example : (WellFormed (DirectArgumentWitnesses.caller).own) ∧ ¬ CallOwnResult (passOwnerCorrupt (DirectArgumentWitnesses.caller) (⟨1⟩) ([.index 3]) (.mutable) (DirectArgumentWitnesses.z)) := by decide

/-- Instance (law `ConcreteSpec.Core.Own.passOwner_call_preserves`, case `ConcreteSpec.Core.Own.DirectArgumentWitnesses.caller`). -/
example : CallResult (passOwner (DirectArgumentWitnesses.caller) (⟨1⟩) ([.index 3]) (.mutable) (DirectArgumentWitnesses.z)) :=
  @passOwner_call_preserves DirectArgumentWitnesses.caller ⟨1⟩ [.index 3] .mutable DirectArgumentWitnesses.z (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.passOwner_call_preserves`, case `ConcreteSpec.Core.Own.DirectArgumentWitnesses.caller`, subject `ConcreteSpec.Core.Own.passOwner`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.passOwnerCorrupt`). -/
example : (CallState.WellFormed (DirectArgumentWitnesses.caller)) ∧ ¬ CallResult (passOwnerCorrupt (DirectArgumentWitnesses.caller) (⟨1⟩) ([.index 3]) (.mutable) (DirectArgumentWitnesses.z)) := by decide

end PreservationEvidence

/-- Apply a semantic postcondition to an actual successful result. -/
def ResultSatisfies {α : Type} (P : α → Prop) : Option α → Prop
  | none => True
  | some t => P t
instance {α : Type} (P : α → Prop) [∀ t, Decidable (P t)] (r : Option α) :
    Decidable (ResultSatisfies P r) := by
  cases r <;> unfold ResultSatisfies <;> infer_instance

/-- Law (subject `ConcreteSpec.Core.Own.leave`). The semantic postcondition holds
of the actual successful operation result. -/
theorem leave_no_local_loan (s : State) (f : Activation) (entry : Var × Hybrid.Place) (hf : s.frames.head? = some f) (hs : WellFormed s) (he : entry ∈ f.locals) :
    ResultSatisfies (fun t : State => ∀ l ∈ t.loans, l.root ≠ entry.2) (leave s) := by
  cases h : leave s with
  | none => trivial
  | some t => exact fun l hl => leave_no_local_loan_result s t f hf hs h entry he l hl

/-- Law (subject `ConcreteSpec.Core.Own.leave`). The semantic postcondition holds
of the actual successful operation result. -/
theorem leave_no_activation_slot (s : State) (f : Activation) (hf : s.frames.head? = some f) :
    ResultSatisfies (fun t : State => ∀ x ∈ t.slots, x.activation ≠ f.id) (leave s) := by
  cases h : leave s with
  | none => trivial
  | some t => exact leave_no_activation_slot_result s t f hf h

/-- Law (subject `ConcreteSpec.Core.Own.passOwner`). The semantic postcondition holds
of the actual successful operation result. -/
theorem passOwner_freezes (s : CallState) (p : Hybrid.Place) (path : List Projection) (m : Mode) (x : Var)  :
    ResultSatisfies (fun t : CallState => ownerAccess t.own p .mutable = false ∧ (m = .mutable → ownerAccess t.own p .shared = false)) (passOwner s p path m x) := by
  cases h : passOwner s p path m x with
  | none => trivial
  | some t => exact passOwner_freezes_result s t p path m x h

/-- Law (subject `ConcreteSpec.Core.Own.leave`). The semantic postcondition holds
of the actual successful operation result. -/
theorem leave_retains_caller (s : State) (f : Activation) (l : Loan) (hs : WellFormed s) (hf : s.frames.head? = some f) (hl : l ∈ s.loans) (caller : l.activation ≠ f.id) :
    ResultSatisfies (fun t : State => l ∈ t.loans) (leave s) := by
  cases h : leave s with
  | none => trivial
  | some t => exact leave_retains_caller_result s t f l hs hf hl caller h

/-- Law (subject `ConcreteSpec.Core.Own.leave`). The semantic postcondition holds
of the actual successful operation result. -/
theorem leave_no_activation_loan (s : State) (f : Activation) (hf : s.frames.head? = some f) :
    ResultSatisfies (fun t : State => ∀ l ∈ t.loans, l.activation ≠ f.id) (leave s) := by
  cases h : leave s with
  | none => trivial
  | some t => exact leave_no_activation_loan_result s t f hf h

/-- Law (subject `ConcreteSpec.Core.Own.reborrowTo`). The semantic postcondition holds
of the actual successful operation result. -/
theorem reborrowTo_live (s : State) (i : LoanId) (path : List Projection) (m : Mode) (a : Nat) (hs : LiveDestinations s) :
    ResultSatisfies (fun t : State => LiveDestinations t) (reborrowTo s i path m a) := by
  cases h : reborrowTo s i path m a with
  | none => trivial
  | some t => exact reborrowTo_live_result s t i path m a hs h

/-- Law (subject `ConcreteSpec.Core.Own.adoptDown`). The semantic postcondition holds
of the actual successful operation result. -/
theorem adoptDown_live (s : State) (args : List (Var × LoanId)) (hs : LiveDestinations s) :
    ResultSatisfies (fun t : State => LiveDestinations t) (adoptDown s args) := by
  cases h : adoptDown s args with
  | none => trivial
  | some t => exact adoptDown_live_result s t args hs h

/-- Law (subject `ConcreteSpec.Core.Own.leave`). The semantic postcondition holds
of the actual successful operation result. -/
theorem leave_live (s : State) (f : Activation) (hs : LiveDestinations s) (hf : s.frames.head? = some f) :
    ResultSatisfies (fun t : State => LiveDestinations t) (leave s) := by
  cases h : leave s with
  | none => trivial
  | some t => exact leave_live_result s t f hs hf h

namespace LifetimeEvidence
open Examples
/-- The callee is current, with a pending direct owner argument ready to adopt. -/
def adoptionInput : State :=
  (enter types [] DirectArgumentWitnesses.waiting.own).getD initial
/-- Mutant: return without cleanup. -/
def leaveIdentity (s : State) : Option State := some s
/-- Mutant: cleanup indiscriminately ends caller loans too. -/
def leaveAllLoans (s : State) : Option State :=
  (leave s).map fun t => { t with loans := [] }
/-- Mutant: pop the callee but strand its loans. -/
def leaveStranded (s : State) : Option State := some { s with frames := s.frames.tail }
/-- Mutant: argument evaluation omits the call-duration freeze. -/
def passOwnerNoFreeze (s : CallState) (_ : Hybrid.Place) (_ : List Projection)
    (_ : Mode) (_ : Var) : Option CallState := some s
/-- Mutant: a fresh reborrow loses its destination frame. -/
def reborrowToDead (s : State) (i : LoanId) (path : List Projection) (m : Mode) (a : Nat) :=
  (reborrowTo s i path m a).map fun t => { t with frames := [] }
/-- Mutant: adoption loses the active call stack. -/
def adoptDownDead (s : State) (args : List (Var × LoanId)) :=
  (adoptDown s args).map fun t => { t with frames := [] }

def leave_no_local_loanCase := (RoundThreeWitnesses.localLoan)

/-- Instance (law `ConcreteSpec.Core.Own.leave_no_local_loan`, case `ConcreteSpec.Core.Own.LifetimeEvidence.leave_no_local_loanCase`). -/
example : ResultSatisfies (fun t : State => ∀ l ∈ t.loans, l.root ≠ ((y, Hybrid.Place.mk 1)).2) (leave (RoundThreeWitnesses.localLoan)) :=
  @leave_no_local_loan RoundThreeWitnesses.localLoan ⟨1, [(y, ⟨1⟩)], []⟩ (y, ⟨1⟩) (rfl) (by decide) (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.leave_no_local_loan`, case `ConcreteSpec.Core.Own.LifetimeEvidence.leave_no_local_loanCase`, subject `ConcreteSpec.Core.Own.leave`, mutant `ConcreteSpec.Core.Own.LifetimeEvidence.leaveIdentity`). -/
example : ((RoundThreeWitnesses.localLoan).frames.head? = some (⟨1, [(y, ⟨1⟩)], []⟩ : Activation)) ∧ (WellFormed (RoundThreeWitnesses.localLoan)) ∧ (((y, Hybrid.Place.mk 1)) ∈ (⟨1, [(y, ⟨1⟩)], []⟩ : Activation).locals) ∧ ¬ ResultSatisfies (fun t : State => ∀ l ∈ t.loans, l.root ≠ ((y, Hybrid.Place.mk 1)).2) (leaveIdentity (RoundThreeWitnesses.localLoan)) := by decide

def leave_no_activation_slotCase := (RoundThreeWitnesses.localLoan)

/-- Instance (law `ConcreteSpec.Core.Own.leave_no_activation_slot`, case `ConcreteSpec.Core.Own.LifetimeEvidence.leave_no_activation_slotCase`). -/
example : ResultSatisfies (fun t : State => ∀ x ∈ t.slots, x.activation ≠ (⟨1, [(y, ⟨1⟩)], []⟩ : Activation).id) (leave (RoundThreeWitnesses.localLoan)) :=
  @leave_no_activation_slot RoundThreeWitnesses.localLoan ⟨1, [(y, ⟨1⟩)], []⟩ (rfl)

/-- Planted (law `ConcreteSpec.Core.Own.leave_no_activation_slot`, case `ConcreteSpec.Core.Own.LifetimeEvidence.leave_no_activation_slotCase`, subject `ConcreteSpec.Core.Own.leave`, mutant `ConcreteSpec.Core.Own.LifetimeEvidence.leaveIdentity`). -/
example : ((RoundThreeWitnesses.localLoan).frames.head? = some (⟨1, [(y, ⟨1⟩)], []⟩ : Activation)) ∧ ¬ ResultSatisfies (fun t : State => ∀ x ∈ t.slots, x.activation ≠ (⟨1, [(y, ⟨1⟩)], []⟩ : Activation).id) (leaveIdentity (RoundThreeWitnesses.localLoan)) := by decide

def passOwner_freezesCase := (DirectArgumentWitnesses.caller)

/-- Instance (law `ConcreteSpec.Core.Own.passOwner_freezes`, case `ConcreteSpec.Core.Own.LifetimeEvidence.passOwner_freezesCase`). -/
example : ResultSatisfies (fun t : CallState => ownerAccess t.own (⟨1⟩) .mutable = false ∧ ((Mode.mutable) = .mutable → ownerAccess t.own (⟨1⟩) .shared = false)) (passOwner (DirectArgumentWitnesses.caller) (⟨1⟩) ([.index 3]) (Mode.mutable) (DirectArgumentWitnesses.z)) :=
  @passOwner_freezes DirectArgumentWitnesses.caller ⟨1⟩ [.index 3] .mutable DirectArgumentWitnesses.z

/-- Planted (law `ConcreteSpec.Core.Own.passOwner_freezes`, case `ConcreteSpec.Core.Own.LifetimeEvidence.passOwner_freezesCase`, subject `ConcreteSpec.Core.Own.passOwner`, mutant `ConcreteSpec.Core.Own.LifetimeEvidence.passOwnerNoFreeze`). -/
example : ¬ ResultSatisfies (fun t : CallState => ownerAccess t.own (⟨1⟩) .mutable = false ∧ ((Mode.mutable) = .mutable → ownerAccess t.own (⟨1⟩) .shared = false)) (passOwnerNoFreeze (DirectArgumentWitnesses.caller) (⟨1⟩) ([.index 3]) (Mode.mutable) (DirectArgumentWitnesses.z)) := by decide

def leave_retains_callerCase := (RoundThreeWitnesses.localLoan)

/-- Instance (law `ConcreteSpec.Core.Own.leave_retains_caller`, case `ConcreteSpec.Core.Own.LifetimeEvidence.leave_retains_callerCase`). -/
example : ResultSatisfies (fun t : State => ((Examples.shared.loans.headD ⟨⟨99⟩, ⟨99⟩, [], .shared, none, [], 99, false, false⟩)) ∈ t.loans) (leave (RoundThreeWitnesses.localLoan)) :=
  @leave_retains_caller RoundThreeWitnesses.localLoan ⟨1, [(y, ⟨1⟩)], []⟩ (Examples.shared.loans.headD ⟨⟨99⟩, ⟨99⟩, [], .shared, none, [], 99, false, false⟩) (by decide) (rfl) (by decide) (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.leave_retains_caller`, case `ConcreteSpec.Core.Own.LifetimeEvidence.leave_retains_callerCase`, subject `ConcreteSpec.Core.Own.leave`, mutant `ConcreteSpec.Core.Own.LifetimeEvidence.leaveAllLoans`). -/
example : (WellFormed (RoundThreeWitnesses.localLoan)) ∧ ((RoundThreeWitnesses.localLoan).frames.head? = some (⟨1, [(y, ⟨1⟩)], []⟩ : Activation)) ∧ (((Examples.shared.loans.headD ⟨⟨99⟩, ⟨99⟩, [], .shared, none, [], 99, false, false⟩)) ∈ (RoundThreeWitnesses.localLoan).loans) ∧ (((Examples.shared.loans.headD ⟨⟨99⟩, ⟨99⟩, [], .shared, none, [], 99, false, false⟩)).activation ≠ (⟨1, [(y, ⟨1⟩)], []⟩ : Activation).id) ∧ ¬ ResultSatisfies (fun t : State => ((Examples.shared.loans.headD ⟨⟨99⟩, ⟨99⟩, [], .shared, none, [], 99, false, false⟩)) ∈ t.loans) (leaveAllLoans (RoundThreeWitnesses.localLoan)) := by decide

def leave_no_activation_loanCase := (RoundThreeWitnesses.localLoan)

/-- Instance (law `ConcreteSpec.Core.Own.leave_no_activation_loan`, case `ConcreteSpec.Core.Own.LifetimeEvidence.leave_no_activation_loanCase`). -/
example : ResultSatisfies (fun t : State => ∀ l ∈ t.loans, l.activation ≠ (⟨1, [(y, ⟨1⟩)], []⟩ : Activation).id) (leave (RoundThreeWitnesses.localLoan)) :=
  @leave_no_activation_loan RoundThreeWitnesses.localLoan ⟨1, [(y, ⟨1⟩)], []⟩ (rfl)

/-- Planted (law `ConcreteSpec.Core.Own.leave_no_activation_loan`, case `ConcreteSpec.Core.Own.LifetimeEvidence.leave_no_activation_loanCase`, subject `ConcreteSpec.Core.Own.leave`, mutant `ConcreteSpec.Core.Own.LifetimeEvidence.leaveIdentity`). -/
example : ((RoundThreeWitnesses.localLoan).frames.head? = some (⟨1, [(y, ⟨1⟩)], []⟩ : Activation)) ∧ ¬ ResultSatisfies (fun t : State => ∀ l ∈ t.loans, l.activation ≠ (⟨1, [(y, ⟨1⟩)], []⟩ : Activation).id) (leaveIdentity (RoundThreeWitnesses.localLoan)) := by decide

def reborrowTo_liveCase := (exclusive)

/-- Instance (law `ConcreteSpec.Core.Own.reborrowTo_live`, case `ConcreteSpec.Core.Own.LifetimeEvidence.reborrowTo_liveCase`). -/
example : ResultSatisfies (fun t : State => LiveDestinations t) (reborrowTo (exclusive) (⟨0⟩) ([.index 1]) (Mode.mutable) (0)) :=
  @reborrowTo_live exclusive ⟨0⟩ [.index 1] .mutable 0 (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.reborrowTo_live`, case `ConcreteSpec.Core.Own.LifetimeEvidence.reborrowTo_liveCase`, subject `ConcreteSpec.Core.Own.reborrowTo`, mutant `ConcreteSpec.Core.Own.LifetimeEvidence.reborrowToDead`). -/
example : (LiveDestinations (exclusive)) ∧ ¬ ResultSatisfies (fun t : State => LiveDestinations t) (reborrowToDead (exclusive) (⟨0⟩) ([.index 1]) (Mode.mutable) (0)) := by decide

def adoptDown_liveCase := (adoptionInput)

/-- Instance (law `ConcreteSpec.Core.Own.adoptDown_live`, case `ConcreteSpec.Core.Own.LifetimeEvidence.adoptDown_liveCase`). -/
example : ResultSatisfies (fun t : State => LiveDestinations t) (adoptDown (adoptionInput) (DirectArgumentWitnesses.waiting.pending)) :=
  @adoptDown_live adoptionInput DirectArgumentWitnesses.waiting.pending (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.adoptDown_live`, case `ConcreteSpec.Core.Own.LifetimeEvidence.adoptDown_liveCase`, subject `ConcreteSpec.Core.Own.adoptDown`, mutant `ConcreteSpec.Core.Own.LifetimeEvidence.adoptDownDead`). -/
example : (LiveDestinations (adoptionInput)) ∧ ¬ ResultSatisfies (fun t : State => LiveDestinations t) (adoptDownDead (adoptionInput) (DirectArgumentWitnesses.waiting.pending)) := by decide

def leave_liveCase := (RoundThreeWitnesses.localLoan)

/-- Instance (law `ConcreteSpec.Core.Own.leave_live`, case `ConcreteSpec.Core.Own.LifetimeEvidence.leave_liveCase`). -/
example : ResultSatisfies (fun t : State => LiveDestinations t) (leave (RoundThreeWitnesses.localLoan)) :=
  @leave_live RoundThreeWitnesses.localLoan ⟨1, [(y, ⟨1⟩)], []⟩ (by decide) (rfl)

/-- Planted (law `ConcreteSpec.Core.Own.leave_live`, case `ConcreteSpec.Core.Own.LifetimeEvidence.leave_liveCase`, subject `ConcreteSpec.Core.Own.leave`, mutant `ConcreteSpec.Core.Own.LifetimeEvidence.leaveStranded`). -/
example : (LiveDestinations (RoundThreeWitnesses.localLoan)) ∧ ((RoundThreeWitnesses.localLoan).frames.head? = some (⟨1, [(y, ⟨1⟩)], []⟩ : Activation)) ∧ ¬ ResultSatisfies (fun t : State => LiveDestinations t) (leaveStranded (RoundThreeWitnesses.localLoan)) := by decide

/-- The slot inventory remains complete when the variable lookup index is empty. -/
def unindexedCopy : State :=
  { initial with
    slots := initial.slots.map fun x => { x with desc := { x.desc with copy := true } }
    frames := [⟨0, [], []⟩] }

/-- Instance (law `ConcreteSpec.Core.Own.leave_no_activation_slot`, case `ConcreteSpec.Core.Own.LifetimeEvidence.unindexedCopy`). -/
example : ResultSatisfies (fun t : State => ∀ x ∈ t.slots, x.activation ≠ 0) (leave unindexedCopy) :=
  @leave_no_activation_slot unindexedCopy ⟨0, [], []⟩ rfl

/-- Planted (law `ConcreteSpec.Core.Own.leave_no_activation_slot`, case `ConcreteSpec.Core.Own.LifetimeEvidence.unindexedCopy`, subject `ConcreteSpec.Core.Own.leave`, mutant `ConcreteSpec.Core.Own.LifetimeEvidence.leaveIdentity`). -/
example : unindexedCopy.frames.head? = some (⟨0, [], []⟩ : Activation) ∧
    ¬ ResultSatisfies (fun t : State => ∀ x ∈ t.slots, x.activation ≠ 0) (leaveIdentity unindexedCopy) := by decide

/-- Accepted: cleanup removes an unindexed Copy owner using the complete slot inventory. -/
example : (leave unindexedCopy).isSome = true ∧
    ((leave unindexedCopy).getD initial).slots = [] := by decide

/-- Refused: an unindexed live linear obligation still prevents exit. -/
example : leave { initial with frames := [⟨0, [], []⟩] } = none := by decide

end LifetimeEvidence

namespace FinalEvidence
open Examples

/-- Mutant: typed entry spuriously refuses every call. -/
def enterRefuse (_ : Var → Option Descriptor) (_ : List Var) (_ : State) : Option State := none

/-- Instance (law `ConcreteSpec.Core.Own.enter_progress`, case `ConcreteSpec.Core.Own.Examples.shared`). -/
example : ∃ t, enter types [y] shared = some t :=
  @enter_progress types shared [y] (by decide) (fun _ _ => ⟨_, rfl⟩)

/-- Planted (law `ConcreteSpec.Core.Own.enter_progress`, case `ConcreteSpec.Core.Own.Examples.shared`, subject `ConcreteSpec.Core.Own.enter`, mutant `ConcreteSpec.Core.Own.FinalEvidence.enterRefuse`). -/
example : [y].Nodup ∧ (∀ x ∈ [y], ∃ d, types x = some d) ∧
    ¬ (∃ t, enterRefuse types [y] shared = some t) :=
  ⟨by decide, (fun _ _ => ⟨_, rfl⟩), by simp [enterRefuse]⟩

/-- Mutant: direct lending spuriously refuses an otherwise permitted owner. -/
def passOwnerRefuse (_ : CallState) (_ : Hybrid.Place) (_ : List Projection)
    (_ : Mode) (_ : Var) : Option CallState := none

def callerFrame : Activation := ⟨0, [(x, ⟨0⟩), (y, ⟨1⟩)], []⟩

/-- Instance (law `ConcreteSpec.Core.Own.directOwner_roundtrip`, case `ConcreteSpec.Core.Own.DirectArgumentWitnesses.caller`). -/
example : (do
    let waiting ← passOwner DirectArgumentWitnesses.caller ⟨1⟩ [.index 3] .mutable DirectArgumentWitnesses.z
    let called ← callLife types (.enter [DirectArgumentWitnesses.z]) waiting
    callLife types .leave called) =
    some ⟨{ DirectArgumentWitnesses.caller.own with
      nextLoan := DirectArgumentWitnesses.caller.own.nextLoan + 1
      nextActivation := DirectArgumentWitnesses.caller.own.nextActivation + 1 }, []⟩ :=
  @directOwner_roundtrip DirectArgumentWitnesses.caller.own ⟨1⟩ [.index 3] .mutable
    DirectArgumentWitnesses.z types callerFrame (by decide) rfl (by decide) (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.directOwner_roundtrip`, case `ConcreteSpec.Core.Own.DirectArgumentWitnesses.caller`, subject `ConcreteSpec.Core.Own.passOwner`, mutant `ConcreteSpec.Core.Own.FinalEvidence.passOwnerRefuse`). -/
example : WellFormed DirectArgumentWitnesses.caller.own ∧
    DirectArgumentWitnesses.caller.own.frames.head? = some callerFrame ∧
    callerFrame.id < DirectArgumentWitnesses.caller.own.nextActivation ∧
    ownerAccess DirectArgumentWitnesses.caller.own ⟨1⟩ .mutable = true ∧
    ¬ ((do
      let waiting ← passOwnerRefuse DirectArgumentWitnesses.caller ⟨1⟩ [.index 3] .mutable DirectArgumentWitnesses.z
      let called ← callLife types (.enter [DirectArgumentWitnesses.z]) waiting
      callLife types .leave called) =
      some ⟨{ DirectArgumentWitnesses.caller.own with
        nextLoan := DirectArgumentWitnesses.caller.own.nextLoan + 1
        nextActivation := DirectArgumentWitnesses.caller.own.nextActivation + 1 }, []⟩) := by decide

end FinalEvidence

/-- Law (subject `ConcreteSpec.Core.Own.instrumentOperands`). Successful argument evaluation
preserves values and concrete storage. The result remains inside the conclusion so a mutant
cannot hide a wrong value behind an output fixed as a theorem argument. -/
theorem evalArgs_ok_erasure {O : Exec.Operands} {types : Var → Option Descriptor}
    {permit : AList Var O.Val → O.Expr → State → Option State}
    (L : AList Var O.Val) (args : List O.Expr) (s : Store O.Store) :
    match Exec.evalArgs (instrumentOperands O types permit) L s args with
    | .ok (vs, t) => Exec.evalArgs O L (erase s) args = .ok (vs, erase t)
    | _ => True := by
  cases h : Exec.evalArgs (instrumentOperands O types permit) L s args with
  | ok pair => exact evalArgs_ok_erasure_result L args s pair.2 pair.1 h
  | trap _ => trivial
  | stuck => trivial

/-- Law (subject `ConcreteSpec.Core.Own.instrumentOperands`). A trapping argument evaluation
preserves the actual trap kind, without fixing that kind in a success premise. -/
theorem evalArgs_trap_erasure {O : Exec.Operands} {types : Var → Option Descriptor}
    {permit : AList Var O.Val → O.Expr → State → Option State}
    (L : AList Var O.Val) (args : List O.Expr) (s : Store O.Store) :
    match Exec.evalArgs (instrumentOperands O types permit) L s args with
    | .trap kd => Exec.evalArgs O L (erase s) args = .trap kd
    | _ => True := by
  cases h : Exec.evalArgs (instrumentOperands O types permit) L s args with
  | ok _ => trivial
  | trap kd => exact evalArgs_trap_erasure_result L args s kd h
  | stuck => trivial

namespace ArgumentEvidence
open Examples

/-- Mutant: instrumentation replaces every argument value by unit. The erased evaluator
and all expected values are unchanged. -/
abbrev zeroOperands (O : Exec.Operands) (types : Var → Option Descriptor)
    (permit : AList Var O.Val → O.Expr → State → Option State) : Exec.Operands :=
  { instrumentOperands O types permit with eval := fun _ s _ => .ok (O.unitVal, s) }

/-- Mutant: instrumentation replaces every outcome by abort. -/
abbrev abortOperands (O : Exec.Operands) (types : Var → Option Descriptor)
    (permit : AList Var O.Val → O.Expr → State → Option State) : Exec.Operands :=
  { instrumentOperands O types permit with eval := fun _ _ _ => .trap .abort }

/-- Instance (law `ConcreteSpec.Core.Own.evalArgs_ok_erasure`, case `ConcreteSpec.Core.Own.Examples.initial`). -/
example : (match Exec.evalArgs (instrumentOperands erasedOps types permitE) [(x, 7)] ⟨0, initial⟩ [.read x, .move x] with
    | .ok (vs, t) => Exec.evalArgs erasedOps [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) [.read x, .move x] = .ok (vs, erase t)
    | _ => True) :=
  @evalArgs_ok_erasure erasedOps types permitE [(x, 7)] [.read x, .move x] ⟨0, initial⟩

/-- Planted (law `ConcreteSpec.Core.Own.evalArgs_ok_erasure`, case `ConcreteSpec.Core.Own.Examples.initial`, subject `ConcreteSpec.Core.Own.instrumentOperands`, mutant `ConcreteSpec.Core.Own.ArgumentEvidence.zeroOperands`). -/
example : ¬ (match Exec.evalArgs (zeroOperands erasedOps types permitE) [(x, 7)] ⟨0, initial⟩ [.read x, .move x] with
    | .ok (vs, t) => Exec.evalArgs erasedOps [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) [.read x, .move x] = .ok (vs, erase t)
    | _ => True) := by
  change ¬ ((Res.ok ([7, 7], 0) : Res (List Int × Int)) = .ok ([0, 0], 0))
  decide

/-- Instance (law `ConcreteSpec.Core.Own.evalArgs_trap_erasure`, case `ConcreteSpec.Core.Own.Examples.initial`). -/
example : (match Exec.evalArgs (instrumentOperands erasedOps types permitE) [(x, 7)] ⟨0, initial⟩ [.move x, .fail .overflow] with
    | .trap kd => Exec.evalArgs erasedOps [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) [.move x, .fail .overflow] = .trap kd
    | _ => True) :=
  @evalArgs_trap_erasure erasedOps types permitE [(x, 7)] [.move x, .fail .overflow] ⟨0, initial⟩

/-- Planted (law `ConcreteSpec.Core.Own.evalArgs_trap_erasure`, case `ConcreteSpec.Core.Own.Examples.initial`, subject `ConcreteSpec.Core.Own.instrumentOperands`, mutant `ConcreteSpec.Core.Own.ArgumentEvidence.abortOperands`). -/
example : ¬ (match Exec.evalArgs (abortOperands erasedOps types permitE) [(x, 7)] ⟨0, initial⟩ [.move x, .fail .overflow] with
    | .trap kd => Exec.evalArgs erasedOps [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) [.move x, .fail .overflow] = .trap kd
    | _ => True) := by
  change ¬ ((Res.trap .overflow : Res (List Int × Int)) = .trap .abort)
  decide

end ArgumentEvidence

end ConcreteSpec.Core.Own
