import ConcreteSpec.Spec.Own

/-! # C5 preservation and erasure
These laws derive preservation structurally from well-formed inputs and local operational
guards, and expose the concrete operation unchanged. They do not claim that
an arbitrary checker/elaborator enforces these transitions; that is C9's correspondence duty.

The independent obligations are operation preservation and cleanup, scoped protocols
(`move_reinitialize_protocol`, `downward_reborrow_protocol`, `directOwner_roundtrip`,
`deferred_call_lifetime`), finite loan history (`ended_loans_never_revive`), and execution
simulation (`Steps_erasure`, `runFrom_erasure` and the accepted converse). Constructor, guard,
identity and wrapper calculations are helpers supporting these obligations, never additional
soundness evidence. In particular the finite simulation owns eval/test/life/allows agreement;
a separate restatement of each wrapper's definition would not establish that contract.
-/
namespace ConcreteSpec.Core.Own

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

/-- Apply a semantic postcondition to an actual successful result. -/
def ResultSatisfies {α : Type} (P : α → Prop) : Option α → Prop
  | none => True
  | some t => P t
instance {α : Type} (P : α → Prop) [∀ t, Decidable (P t)] (r : Option α) :
    Decidable (ResultSatisfies P r) := by
  cases r <;> unfold ResultSatisfies <;> infer_instance

/-- Helper (congruence). Lifts a supplied one-step preservation proof through a monadic list fold. (approved: APPR-C5-codex-902ce88a). -/
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

/-- Law (subject `ConcreteSpec.Core.Own.move`). Every successful result preserves
ownership; refusal does not fabricate a result. Move changes only the selected slot's status,
after excluding every loan of its root. -/
theorem move_preserves (s : State) (p : Hybrid.Place) (hs : WellFormed s) : StateResult (move s p) := by
  cases h : move s p with
  | none => trivial
  | some t =>
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
      refine ⟨?_, hs.2.1, ?_, ?_, hs.2.2.2.2.1, hs.2.2.2.2.2.1, ?_⟩
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
      · refine ⟨?_, ?_, ?_⟩
        · intro f hf
          refine ⟨(hs.2.2.2.2.2.2.1 f hf).1, ?_⟩
          intro a ha hact
          obtain ⟨b, hb, rfl⟩ := List.mem_map.mp ha
          rw [place]
          exact (hs.2.2.2.2.2.2.1 f hf).2 b hb ((activation b).symm.trans hact)
        · intro a ha
          obtain ⟨b, hb, rfl⟩ := List.mem_map.mp ha
          change (change b).activation ∈ s.frames.map Activation.id
          rw [activation]
          exact hs.2.2.2.2.2.2.2.1 b hb
        · intro f hf p hp
          obtain ⟨b, hb, he, hact⟩ := hs.2.2.2.2.2.2.2.2 f hf p hp
          exact ⟨change b, List.mem_map.mpr ⟨b, hb, rfl⟩,
            (place b).trans he, (activation b).trans hact⟩

/-- Law (subject `ConcreteSpec.Core.Own.reinitialize`). Every successful result preserves
ownership; refusal does not fabricate a result. -/
theorem reinitialize_preserves (s : State) (p : Hybrid.Place) (hs : WellFormed s) : StateResult (reinitialize s p) := by
  cases h : reinitialize s p with
  | none => trivial
  | some t =>
    unfold reinitialize at h
    obtain ⟨x, _, h⟩ := Option.bind_eq_some_iff.mp h
    split at h
    · cases h
      let change := fun x : Slot => if x.place = p then { x with status := .owned } else x
      have place (x : Slot) : (change x).place = x.place := by simp [change]; split <;> rfl
      have activation (x : Slot) : (change x).activation = x.activation := by simp [change]; split <;> rfl
      refine ⟨?_, hs.2.1, ?_, ?_, hs.2.2.2.2.1, hs.2.2.2.2.2.1, ?_⟩
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
      · refine ⟨?_, ?_, ?_⟩
        · intro f hf
          refine ⟨(hs.2.2.2.2.2.2.1 f hf).1, ?_⟩
          intro a ha hact
          obtain ⟨b, hb, rfl⟩ := List.mem_map.mp ha
          rw [place]
          exact (hs.2.2.2.2.2.2.1 f hf).2 b hb ((activation b).symm.trans hact)
        · intro a ha
          obtain ⟨b, hb, rfl⟩ := List.mem_map.mp ha
          change (change b).activation ∈ s.frames.map Activation.id
          rw [activation]
          exact hs.2.2.2.2.2.2.2.1 b hb
        · intro f hf p hp
          obtain ⟨b, hb, he, hact⟩ := hs.2.2.2.2.2.2.2.2 f hf p hp
          exact ⟨change b, List.mem_map.mpr ⟨b, hb, rfl⟩,
            (place b).trans he, (activation b).trans hact⟩
    · contradiction

/-- Helper (computation). Reorders the disjunction defining symmetric loan compatibility. (approved: APPR-C5-codex-902ce88a). -/
private theorem compatible_symm (a b : Loan) (h : Compatible a b) : Compatible b a := by
  unfold Compatible at *
  rcases h with h | h | h | h | h
  · exact Or.inl (Ne.symm h)
  · exact Or.inr (Or.inl h.symm)
  · exact Or.inr (Or.inr (Or.inr (Or.inl h)))
  · exact Or.inr (Or.inr (Or.inl h))
  · exact Or.inr (Or.inr (Or.inr (Or.inr h.symm)))

/-- Helper (inversion). Extracts owned-root and whole-root permission facts from the owner access guard. (approved: APPR-C5-codex-902ce88a). -/
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

/-- Law (subject `ConcreteSpec.Core.Own.borrow`). Every successful result preserves
ownership; refusal does not fabricate a result. -/
theorem borrow_preserves (s : State) (p : Hybrid.Place) (path : List Projection)
    (m : Mode) (hs : WellFormed s) : StateResult (borrow s p path m) := by
  cases h : borrow s p path m with
  | none => trivial
  | some t =>
    unfold borrow at h
    obtain ⟨f, _, h⟩ := Option.bind_eq_some_iff.mp h
    split at h
    · rename_i hg
      cases h
      obtain ⟨hg, hact⟩ := Bool.and_eq_true_iff.mp hg
      obtain ⟨hroot, hconf⟩ := owner_guard s p m hg
      let a : Loan := ⟨⟨s.nextLoan⟩, p, path, m, none, [], f.id, false, false⟩
      have fresh : a.id ∉ s.loans.map Loan.id := by
        intro hm
        obtain ⟨b, hb, he⟩ := List.mem_map.mp hm
        have hn := (hs.2.2.2.1 b hb).1
        rw [he] at hn
        exact Nat.lt_irrefl _ hn
      have parent (l : Loan) (hl : ParentValid s l) :
          ParentValid { s with loans := a :: s.loans, nextLoan := s.nextLoan + 1 } l := by
        unfold ParentValid at hl ⊢
        cases he : l.parent with
        | none => simpa only [he] using hl
        | some p' =>
          simp only [he] at hl ⊢
          obtain ⟨b, hb, rest⟩ := hl
          exact ⟨b, List.mem_cons_of_mem _ hb, rest⟩
      have hcompat : ∀ b ∈ s.loans, Compatible a b := by
        intro b hb
        rcases hconf b hb with hn | hm
        · exact Or.inl (Ne.symm hn)
        · exact Or.inr (Or.inr (Or.inr (Or.inr hm)))
      refine ⟨hs.1, ?_, hs.2.2.1, ?_, ?_, hs.2.2.2.2.2⟩
      · simpa only [List.map_cons, List.nodup_cons] using And.intro fresh hs.2.1
      · intro l hl
        rcases List.mem_cons.mp hl with he | hl
        · subst l
          exact ⟨Nat.lt_succ_self _, of_decide_eq_true hact, hroot, by simp [ParentValid], by simp⟩
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
          · exact hs.2.2.2.2.1 b hb c hc
    · contradiction

/-- Law (subject `ConcreteSpec.Core.Own.reborrowTo`). Every successful result preserves
ownership; refusal does not fabricate a result. -/
theorem reborrowTo_preserves (s : State) (i : LoanId) (path : List Projection)
    (m : Mode) (activation : Nat) (hs : WellFormed s) :
    StateResult (reborrowTo s i path m activation) := by
  cases h : reborrowTo s i path m activation with
  | none => trivial
  | some t =>
    unfold reborrowTo at h
    obtain ⟨a₀, ha, h⟩ := Option.bind_eq_some_iff.mp h
    have hmem : a₀ ∈ s.loans := List.mem_of_find?_eq_some ha
    have hi : a₀.id = i := by simpa using List.find?_some ha
    split at h
    · rename_i hg
      cases h
      obtain ⟨hg, hact⟩ := Bool.and_eq_true_iff.mp hg
      unfold loanAccess at hg
      rw [ha] at hg
      simp only [Option.any_some, Bool.and_eq_true, Bool.not_eq_true', Bool.or_eq_true,
        beq_iff_eq] at hg
      obtain ⟨⟨_, hmode⟩, hconf⟩ := hg
      have upgrade : a₀.mode = .shared → m = .shared := by
        intro he
        rcases hmode with hm | hm
        · exact hm
        · simp [he] at hm
      let a : Loan := ⟨⟨s.nextLoan⟩, a₀.root, a₀.path ++ path, m, some i,
        i :: a₀.ancestors, activation, false, false⟩
      have fresh : a.id ∉ s.loans.map Loan.id := by
        intro hm
        obtain ⟨b, hb, he⟩ := List.mem_map.mp hm
        have hn := (hs.2.2.2.1 b hb).1
        rw [he] at hn
        exact Nat.lt_irrefl _ hn
      have parent (l : Loan) (hl : ParentValid s l) :
          ParentValid { s with loans := a :: s.loans, nextLoan := s.nextLoan + 1 } l := by
        unfold ParentValid at hl ⊢
        cases he : l.parent with
        | none => simpa only [he] using hl
        | some p' =>
          simp only [he] at hl ⊢
          obtain ⟨b, hb, rest⟩ := hl
          exact ⟨b, List.mem_cons_of_mem _ hb, rest⟩
      have hparent : ParentValid s a :=
        ⟨a₀, hmem, hi, rfl, by simpa [hi] using (hs.2.2.2.1 a₀ hmem).1,
          by simp [a, hi], upgrade, by simp [a]⟩
      have hcompat : ∀ b ∈ s.loans, Compatible a b := by
        intro b hb
        have hc := List.all_eq_true.mp hconf b hb
        simp only [Bool.or_eq_true, beq_iff_eq, decide_eq_true_eq, bne_iff_ne,
          Bool.and_eq_true] at hc
        rcases hc with ((he | he) | he) | he
        · exact Or.inr (Or.inr (Or.inr (Or.inl (by simp [a, he]))))
        · exact Or.inr (Or.inr (Or.inr (Or.inl (List.mem_cons_of_mem _ he))))
        · exact Or.inl (Ne.symm he)
        · exact Or.inr (Or.inr (Or.inr (Or.inr he)))
      refine ⟨hs.1, ?_, hs.2.2.1, ?_, ?_, hs.2.2.2.2.2⟩
      · simpa only [List.map_cons, List.nodup_cons] using And.intro fresh hs.2.1
      · intro l hl
        rcases List.mem_cons.mp hl with he | hl
        · subst l
          exact ⟨Nat.lt_succ_self _, (of_decide_eq_true hact).1,
            (hs.2.2.2.1 a₀ hmem).2.2.1, parent a hparent, by simp⟩
        · obtain ⟨hb, ha', hr, hp, hpend⟩ := hs.2.2.2.1 l hl
          exact ⟨Nat.lt_succ_of_lt hb, ha', hr, parent l hp, hpend⟩
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
          · exact hs.2.2.2.2.1 b hb c hc
    · contradiction

/-- Law (subject `ConcreteSpec.Core.Own.reborrow`). Every successful result preserves
ownership; refusal does not fabricate a result. -/
theorem reborrow_preserves (s : State) (i : LoanId) (path : List Projection)
    (m : Mode) (hs : WellFormed s) : StateResult (reborrow s i path m) := by
  cases h : reborrow s i path m with
  | none => trivial
  | some t =>
    unfold reborrow at h
    obtain ⟨f, _, h⟩ := Option.bind_eq_some_iff.mp h
    have hw := reborrowTo_preserves s i path m f.id hs
    rwa [h] at hw

/-- Law (subject `ConcreteSpec.Core.Own.endLoan`). Every successful result preserves
ownership; refusal does not fabricate a result. -/
theorem endLoan_preserves (s : State) (i : LoanId) (hs : WellFormed s) : StateResult (endLoan s i) := by
  cases h : endLoan s i with
  | none => trivial
  | some t =>
    unfold endLoan at h
    obtain ⟨a, _, h⟩ := Option.bind_eq_some_iff.mp h
    split at h
    · contradiction
    · rename_i clear
      cases h
      refine ⟨hs.1, hs.2.1.sublist (List.Sublist.map _ List.filter_sublist), hs.2.2.1, ?_, ?_, hs.2.2.2.2.2⟩
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
        exact hs.2.2.2.2.1 b (List.mem_filter.mp hb).1 c (List.mem_filter.mp hc).1

/-- Helper (inversion). Structural successful-result proof used by the operation-result theorem. (approved: APPR-C5-codex-902ce88a). -/
theorem accessOwner_preserves_result (s t : State) (p : Hybrid.Place) (m : Mode)
    (hs : WellFormed s) (h : accessOwner s p m = some t) : WellFormed t := by
  unfold accessOwner at h
  split at h
  · cases h; exact hs
  · contradiction

/-- Helper (inversion). Structural successful-result proof used by the operation-result theorem. (approved: APPR-C5-codex-902ce88a). -/
theorem accessLoan_preserves_result (s t : State) (i : LoanId) (m : Mode)
    (hs : WellFormed s) (h : accessLoan s i m = some t) : WellFormed t := by
  unfold accessLoan at h
  split at h
  · cases h; exact hs
  · contradiction

/-- Helper (computation). Normalizes the move guard; linear consumption soundness is established by move_preserves and moved_no_loan. (approved: APPR-C5-codex-902ce88a). -/
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

/-- Helper (computation). Normalizes the borrowing guard; live roots and conflict safety are established by borrow_preserves. (approved: APPR-C5-codex-902ce88a). -/
theorem borrow_progress (s : State) (p : Hybrid.Place) (path : List Projection) (m : Mode)
    (f : Activation) (frame : s.frames.head? = some f)
    (issued : f.id < s.nextActivation) (allowed : ownerAccess s p m = true) :
    ∃ t, borrow s p path m = some t := by
  simp [borrow, frame, issued, allowed]

/-- Helper (computation). Normalizes the explicit destination guard; downward_reborrow_protocol establishes the complete lending boundary. (approved: APPR-C5-codex-902ce88a). -/
theorem reborrowTo_progress (s : State) (i : LoanId) (path : List Projection) (m : Mode)
    (activation : Nat) (a : Loan) (found : loan s i = some a)
    (issued : activation < s.nextActivation)
    (current : s.frames.head?.map Activation.id = some activation)
    (allowed : loanAccess s i m = true) :
    ∃ t, reborrowTo s i path m activation = some t := by
  simp [reborrowTo, found, issued, current, allowed]

/-- Helper (computation). Specializes the destination computation to the current frame; downward_reborrow_protocol supplies scoped soundness. (approved: APPR-C5-codex-902ce88a). -/
theorem reborrow_progress (s : State) (i : LoanId) (path : List Projection) (m : Mode)
    (f : Activation) (frame : s.frames.head? = some f) (issued : f.id < s.nextActivation)
    (a : Loan) (found : loan s i = some a) (allowed : loanAccess s i m = true) :
    ∃ t, reborrow s i path m = some t := by
  simpa [reborrow, frame] using
    reborrowTo_progress s i path m f.id a found issued (by simp [frame]) allowed

/-- Helper (computation). Computes the successful identity transition from owner permission; execution soundness belongs to Steps_erasure. (approved: APPR-C5-codex-902ce88a). -/
theorem accessOwner_progress (s : State) (p : Hybrid.Place) (m : Mode)
    (allowed : ownerAccess s p m = true) : accessOwner s p m = some s := by
  simp [accessOwner, allowed]

/-- Helper (computation). Computes the successful identity transition from loan permission; execution soundness belongs to Steps_erasure. (approved: APPR-C5-codex-902ce88a). -/
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

/-- Law (subject `ConcreteSpec.Core.Own.bind`). Every successful result preserves
ownership; refusal does not fabricate a result. -/
theorem bind_preserves (types : Var → Option Descriptor) (s : State) (x : Var)
    (hs : WellFormed s) : StateResult (bind types x s) := by
  cases h : bind types x s with
  | none => trivial
  | some t =>
    unfold bind at h
    obtain ⟨f, hf, h⟩ := Option.bind_eq_some_iff.mp h
    split at h
    · rename_i p _
      have hw := reinitialize_preserves s p hs
      rwa [h] at hw
    · obtain ⟨d, _, h⟩ := Option.bind_eq_some_iff.mp h
      split at h
      · rename_i hact
        cases h
        cases hframes : s.frames with
        | nil => simp [hframes] at hf
        | cons f₀ tail =>
          simp only [hframes, List.head?_cons, Option.some.injEq] at hf
          subst f₀
          have hnodup : (s.frames.map Activation.id).Nodup := hs.2.2.2.2.2.1
          rw [hframes] at hnodup
          refine ⟨?_, hs.2.1, ?_, ?_, hs.2.2.2.2.1, hnodup, ?_⟩
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
          · refine ⟨?_, ?_, ?_⟩
            · intro g hg
              rcases List.mem_cons.mp hg with rfl | hg
              · refine ⟨hact, ?_⟩
                intro a ha ha_act
                rcases List.mem_cons.mp ha with rfl | ha
                · simp
                · exact List.mem_cons_of_mem _
                    ((hs.2.2.2.2.2.2.1 f (by simp [hframes])).2 a ha ha_act)
              · dsimp at hg
                have hg_mem : g ∈ s.frames := by simp [hframes, hg]
                refine ⟨(hs.2.2.2.2.2.2.1 g hg_mem).1, ?_⟩
                intro a ha ha_act
                rcases List.mem_cons.mp ha with rfl | ha
                · simp only [List.map_cons, List.nodup_cons] at hnodup
                  exfalso
                  exact hnodup.1 (List.mem_map.mpr ⟨g, hg, ha_act.symm⟩)
                · exact (hs.2.2.2.2.2.2.1 g hg_mem).2 a ha ha_act
            · intro a ha
              rcases List.mem_cons.mp ha with rfl | ha
              · exact List.mem_cons_self
              · simpa only [hframes, List.map_cons, List.tail_cons] using hs.2.2.2.2.2.2.2.1 a ha
            · intro g hg p hp
              rcases List.mem_cons.mp hg with rfl | hg
              · simp only [List.map_cons, List.mem_cons] at hp
                rcases hp with rfl | hp
                · exact ⟨_, List.mem_cons_self, rfl, rfl⟩
                · obtain ⟨a, ha, he, hact⟩ := hs.2.2.2.2.2.2.2.2 f (by simp [hframes]) p hp
                  exact ⟨a, List.mem_cons_of_mem _ ha, he, hact⟩
              · dsimp at hg
                obtain ⟨a, ha, he, hact⟩ := hs.2.2.2.2.2.2.2.2 g (by simp [hframes, hg]) p hp
                exact ⟨a, List.mem_cons_of_mem _ ha, he, hact⟩
      · contradiction

/-- Helper (inversion). Uses a duplicate-free place index to identify two slot records. (approved: APPR-C5-codex-902ce88a). -/
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

/-- Law (subject `ConcreteSpec.Core.Own.drop`). Every successful result preserves
ownership; refusal does not fabricate a result. -/
theorem drop_preserves (s : State) (xs : List Var) (hs : WellFormed s) : StateResult (drop xs s) := by
  cases h : drop xs s with
  | none => trivial
  | some t =>
    unfold drop at h
    obtain ⟨f, hf, h⟩ := Option.bind_eq_some_iff.mp h
    dsimp at h
    split at h
    · rename_i hg
      cases h
      cases hframes : s.frames with
      | nil => simp [hframes] at hf
      | cons f₀ tail =>
        simp only [hframes, List.head?_cons, Option.some.injEq] at hf
        subst f₀
        have hnodup : (s.frames.map Activation.id).Nodup := hs.2.2.2.2.2.1
        rw [hframes] at hnodup
        refine ⟨hs.1.sublist (List.Sublist.map _ List.filter_sublist), hs.2.1, ?_, ?_, hs.2.2.2.2.1, hnodup, ?_⟩
        · intro a ha
          exact hs.2.2.1 a (List.mem_filter.mp ha).1
        · intro l hl
          obtain ⟨hb, ha, ⟨a, ham, he, ho⟩, hp, hpend⟩ := hs.2.2.2.1 l hl
          refine ⟨hb, ha, ⟨a, List.mem_filter.mpr ⟨ham, ?_⟩, he, ho⟩, hp, hpend⟩
          have hc := List.all_eq_true.mp (Bool.and_eq_true_iff.mp hg).2 l hl
          simpa [he] using hc
        · refine ⟨?_, ?_, ?_⟩
          · intro g hg
            rcases List.mem_cons.mp hg with rfl | hg
            · have hf_mem : f ∈ s.frames := by simp [hframes]
              refine ⟨(hs.2.2.2.2.2.2.1 f hf_mem).1, ?_⟩
              intro a ha ha_act
              obtain ⟨ha_mem, ha_not_ps⟩ := List.mem_filter.mp ha
              simp only [decide_eq_true_eq] at ha_not_ps
              have hplace := (hs.2.2.2.2.2.2.1 f hf_mem).2 a ha_mem ha_act
              obtain ⟨e, he_mem, he_snd⟩ := List.mem_map.mp hplace
              apply List.mem_map.mpr
              refine ⟨e, List.mem_filter.mpr ⟨he_mem, ?_⟩, he_snd⟩
              simp only [decide_eq_true_eq]
              simpa [he_snd] using ha_not_ps
            · dsimp at hg
              have hg_mem : g ∈ s.frames := by simp [hframes, hg]
              refine ⟨(hs.2.2.2.2.2.2.1 g hg_mem).1, ?_⟩
              intro a ha ha_act
              exact (hs.2.2.2.2.2.2.1 g hg_mem).2 a (List.mem_filter.mp ha).1 ha_act
          · intro a ha
            simpa only [hframes, List.map_cons, List.tail_cons] using
              hs.2.2.2.2.2.2.2.1 a (List.mem_filter.mp ha).1
          · intro g hg p hp
            rcases List.mem_cons.mp hg with rfl | hg
            · obtain ⟨e, he, rfl⟩ := List.mem_map.mp hp
              obtain ⟨hem, hen⟩ := List.mem_filter.mp he
              obtain ⟨a, ha, heq, hact⟩ := hs.2.2.2.2.2.2.2.2 f (by simp [hframes]) e.2
                (List.mem_map.mpr ⟨e, hem, rfl⟩)
              exact ⟨a, List.mem_filter.mpr ⟨ha, by simpa [heq] using hen⟩, heq, hact⟩
            · dsimp at hg
              have hgm : g ∈ s.frames := by simp [hframes, hg]
              obtain ⟨a, ha, heq, hact⟩ := hs.2.2.2.2.2.2.2.2 g hgm p hp
              refine ⟨a, List.mem_filter.mpr ⟨ha, ?_⟩, heq, hact⟩
              apply decide_eq_true
              intro hm
              obtain ⟨e, hem, hes⟩ := List.mem_map.mp hm
              have hep : e.2 ∈ (List.map Prod.snd f.locals) :=
                List.mem_map.mpr ⟨e, (List.mem_filter.mp hem).1, rfl⟩
              obtain ⟨b, hb, hbp, hba⟩ := hs.2.2.2.2.2.2.2.2 f (by simp [hframes]) e.2 hep
              have hab : a = b := by
                exact slot_unique s.slots hs.1 a b ha hb (hes.symm.trans hbp.symm)
              have hgf : g.id = f.id := hact.symm.trans (hab ▸ hba)
              simp only [List.map_cons, List.nodup_cons] at hnodup
              exact hnodup.1 (List.mem_map.mpr ⟨g, hg, hgf⟩)
    · contradiction

/-- Law (subject `ConcreteSpec.Core.Own.enter`). Every successful result preserves
ownership; refusal does not fabricate a result. -/
theorem enter_preserves (types : Var → Option Descriptor) (s : State) (xs : List Var)
    (hs : WellFormed s) : StateResult (enter types xs s) := by
  cases h : enter types xs s with
  | none => trivial
  | some t =>
    have hw : WellFormed { s with
        nextActivation := s.nextActivation + 1
        frames := ⟨s.nextActivation, [], []⟩ :: s.frames } := by
      refine ⟨hs.1, hs.2.1, ?_, ?_, hs.2.2.2.2.1, ?_, ?_⟩
      · intro a ha
        exact ⟨(hs.2.2.1 a ha).1, Nat.lt_succ_of_lt (hs.2.2.1 a ha).2⟩
      · intro l hl
        obtain ⟨hb, ha, hr, hp, hpend⟩ := hs.2.2.2.1 l hl
        exact ⟨hb, Nat.lt_succ_of_lt ha, hr, hp, hpend⟩
      · simp only [List.map_cons, List.nodup_cons]
        refine ⟨?_, hs.2.2.2.2.2.1⟩
        intro hm
        obtain ⟨g, hg, he⟩ := List.mem_map.mp hm
        have hlt := (hs.2.2.2.2.2.2.1 g hg).1
        omega
      · refine ⟨?_, ?_, ?_⟩
        · intro g hg
          rcases List.mem_cons.mp hg with rfl | hg
          · refine ⟨Nat.lt_succ_self _, ?_⟩
            intro a ha ha_act
            have hlt := (hs.2.2.1 a ha).2
            dsimp at ha_act
            omega
          · exact ⟨Nat.lt_succ_of_lt (hs.2.2.2.2.2.2.1 g hg).1, (hs.2.2.2.2.2.2.1 g hg).2⟩
        · intro a ha
          exact List.mem_cons_of_mem _ (hs.2.2.2.2.2.2.2.1 a ha)
        · intro g hg p hp
          rcases List.mem_cons.mp hg with rfl | hg
          · simp at hp
          · exact hs.2.2.2.2.2.2.2.2 g hg p hp
    exact fold_preserves _ (fun s x t hs h => by
      have h' := bind_preserves types s x hs
      rwa [h] at h') xs _ t hw h

/-- Law (subject `ConcreteSpec.Core.Own.endLocals`). Every successful result preserves
ownership; refusal does not fabricate a result. -/
theorem endLocals_preserves (s : State) (a : Nat) (hs : WellFormed s) : StateResult (endLocals a s) := by
  cases h : endLocals a s with
  | none => trivial
  | some t =>
    exact fold_preserves _ (fun s l t hs h => by
      have h' := endLoan_preserves s l.id hs
      rwa [h] at h') _ _ _ hs h

/-- Helper (computation). Ending a loan leaves activation frames unchanged. (approved: APPR-C5-codex-902ce88a). -/
private theorem endLoan_frames (s t : State) (i : LoanId) (h : endLoan s i = some t) :
    t.frames = s.frames := by
  unfold endLoan at h
  obtain ⟨_, _, hh⟩ := Option.bind_eq_some_iff.mp h
  split at hh
  · contradiction
  · cases hh
    rfl

/-- Helper (congruence). Lifts a supplied frame equality through a monadic fold. (approved: APPR-C5-codex-902ce88a). -/
private theorem fold_frames {α : Type} (f : State → α → Option State)
    (hf : ∀ s a t, f s a = some t → t.frames = s.frames) (xs : List α)
    (s t : State) (h : xs.foldlM f s = some t) : t.frames = s.frames := by
  induction xs generalizing s with
  | nil => simp at h; subst t; rfl
  | cons a xs ih =>
    simp only [List.foldlM_cons] at h
    obtain ⟨u, hu, hh⟩ := Option.bind_eq_some_iff.mp h
    exact (ih u hh).trans (hf s a u hu)

/-- Helper (inversion). Inverts the slot-removal filter for a selected local place. (approved: APPR-C5-codex-902ce88a). -/
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

/-- Law (subject `ConcreteSpec.Core.Own.leave`). The semantic postcondition holds
of the actual successful operation result. -/
theorem leave_no_activation_slot (s : State) (f : Activation)
    (hs : WellFormed s) (hf : s.frames.head? = some f) :
    ResultSatisfies (fun t : State => ∀ x ∈ t.slots, x.activation ≠ f.id) (leave s) := by
  cases h : leave s with
  | none => trivial
  | some t =>
    unfold leave at h
    rw [hf] at h
    dsimp at h
    obtain ⟨u, hu, hh⟩ := Option.bind_eq_some_iff.mp h
    have hu_wf : WellFormed u := by
      have h' := endLocals_preserves s f.id hs
      rwa [hu] at h'
    have hframes := fold_frames _ (fun s l t hh => endLoan_frames s t l.id hh) _ s u hu
    obtain ⟨v, hv, hh⟩ := Option.bind_eq_some_iff.mp hh
    cases hh
    intro x hx hact
    have hxu : x ∈ u.slots := by
      unfold drop at hv
      rw [hframes, hf] at hv
      dsimp at hv
      split at hv
      · cases hv
        exact (List.mem_filter.mp hx).1
      · contradiction
    have hf_mem : f ∈ u.frames := by
      rw [hframes]
      exact List.mem_of_head? hf
    have hplace := (hu_wf.2.2.2.2.2.2.1 f hf_mem).2 x hxu hact
    obtain ⟨entry, he_mem, he_snd⟩ := List.mem_map.mp hplace
    have hp : x.place ∈ List.map Prod.snd (f.locals.filter fun e =>
        decide (e.1 ∈ List.map Prod.fst f.locals)) := by
      apply List.mem_map.mpr
      exact ⟨entry, List.mem_filter.mpr ⟨he_mem, by simp; exact ⟨entry.2, he_mem⟩⟩, he_snd⟩
    exact drop_absent u v _ f (hframes ▸ hf) hv x.place hp x hx rfl

/-- Law (subject `ConcreteSpec.Core.Own.leave`). Every successful result preserves
ownership; refusal does not fabricate a result. -/
theorem leave_preserves (s : State) (hs : WellFormed s) : StateResult (leave s) := by
  cases h : leave s with
  | none => trivial
  | some t =>
    obtain ⟨f, hf⟩ := Option.isSome_iff_exists.mp (show s.frames.head?.isSome = true from by
      cases he : s.frames.head? <;> simp [leave, he] at h ⊢)
    have absent : ∀ x ∈ t.slots, x.activation ≠ f.id := by
      have h' := leave_no_activation_slot s f hs hf
      rwa [h] at h'
    unfold leave at h
    obtain ⟨f', hf', h⟩ := Option.bind_eq_some_iff.mp h
    have hff : f' = f := Option.some.inj (hf'.symm.trans hf)
    subst f'
    obtain ⟨u, hu, h⟩ := Option.bind_eq_some_iff.mp h
    obtain ⟨v, hv, h⟩ := Option.bind_eq_some_iff.mp h
    cases h
    have hu_wf : WellFormed u := by
      have h' := endLocals_preserves s f.id hs
      rwa [hu] at h'
    have hw : WellFormed v := by
      have h' := drop_preserves u (List.map Prod.fst f.locals) hu_wf
      rwa [hv] at h'
    refine ⟨hw.1, hw.2.1, hw.2.2.1, hw.2.2.2.1, hw.2.2.2.2.1,
      hw.2.2.2.2.2.1.sublist (List.Sublist.map _ (List.tail_sublist _)), ?_⟩
    refine ⟨?_, ?_, ?_⟩
    · intro g hg
      exact hw.2.2.2.2.2.2.1 g (List.mem_of_mem_tail hg)
    · intro x hx
      have hm := hw.2.2.2.2.2.2.2.1 x hx
      have hu_frames := fold_frames _ (fun s l t hh => endLoan_frames s t l.id hh) _ s u hu
      have hv_ids : v.frames.map Activation.id = s.frames.map Activation.id := by
        unfold drop at hv
        rw [hu_frames, hf] at hv
        dsimp at hv
        split at hv
        · cases hv
          cases hsf : s.frames with
          | nil => simp [hsf] at hf
          | cons g gs =>
            simp only [hsf, List.head?_cons, Option.some.injEq] at hf
            subst g
            rfl
        · contradiction
      cases hvf : v.frames with
      | nil => simp [hvf] at hm
      | cons g gs =>
        have hgf : g.id = f.id := by
          have he := congrArg List.head? hv_ids
          simpa [hvf, hf, List.head?_map] using he
        simp only [hvf, List.map_cons, List.mem_cons] at hm
        rcases hm with he | hm
        · exact False.elim (absent x hx (he.trans hgf))
        · simpa [hvf] using hm
    · intro g hg p hp
      exact hw.2.2.2.2.2.2.2.2 g (List.mem_of_mem_tail hg) p hp

/-- Helper (congruence). Updating only the view-binding lookup table of the current frame preserves `WellFormed`. (approved: APPR-C5-codex-902ce88a). -/
private theorem set_views_preserves (s : State) (f : Activation) (views : AList Var LoanId)
    (hs : WellFormed s) (hf : s.frames.head? = some f) :
    WellFormed { s with frames := { f with views := views } :: s.frames.tail } := by
  cases hframes : s.frames with
  | nil => simp [hframes] at hf
  | cons f₀ tail =>
    simp only [hframes, List.head?_cons, Option.some.injEq] at hf
    subst f₀
    have hnodup : (s.frames.map Activation.id).Nodup := hs.2.2.2.2.2.1
    rw [hframes] at hnodup
    refine ⟨hs.1, hs.2.1, hs.2.2.1, hs.2.2.2.1, hs.2.2.2.2.1, hnodup, ?_⟩
    refine ⟨?_, ?_, ?_⟩
    · intro g hg
      rcases List.mem_cons.mp hg with rfl | hg
      · exact hs.2.2.2.2.2.2.1 f (by simp [hframes])
      · dsimp at hg
        exact hs.2.2.2.2.2.2.1 g (by simp [hframes, hg])
    · intro a ha
      simpa only [hframes, List.map_cons, List.tail_cons] using hs.2.2.2.2.2.2.2.1 a ha
    · intro g hg p hp
      rcases List.mem_cons.mp hg with rfl | hg
      · exact hs.2.2.2.2.2.2.2.2 f (by simp [hframes]) p hp
      · dsimp at hg
        exact hs.2.2.2.2.2.2.2.2 g (by simp [hframes, hg]) p hp

/-- Law (subject `ConcreteSpec.Core.Own.enterDown`). Every successful result preserves
ownership; refusal does not fabricate a result. -/
theorem enterDown_preserves (s : State) (args : List (Var × LoanId)) (hs : WellFormed s) :
    StateResult (enterDown s args) := by
  cases h : enterDown s args with
  | none => trivial
  | some t =>
    unfold enterDown at h
    obtain ⟨u, hu, hh⟩ := Option.bind_eq_some_iff.mp h
    have hu_wf : WellFormed u := by
      have h' := enter_preserves (fun _ => none) s [] hs
      rwa [hu] at h'
    refine fold_preserves _ ?_ args u t hu_wf hh
    intro s arg t hs hh
    obtain ⟨a, _, hh⟩ := Option.bind_eq_some_iff.mp hh
    obtain ⟨f, hf, hh⟩ := Option.bind_eq_some_iff.mp hh
    obtain ⟨v, hv, hh⟩ := Option.bind_eq_some_iff.mp hh
    cases hh
    have hw : WellFormed v := by
      have h' := reborrowTo_preserves s arg.2 [] a.mode f.id hs
      rwa [hv] at h'
    have hv_frames : v.frames = s.frames := by
      unfold reborrowTo at hv
      obtain ⟨_, _, hv⟩ := Option.bind_eq_some_iff.mp hv
      split at hv
      · cases hv; rfl
      · contradiction
    rw [← hv_frames] at hf
    exact set_views_preserves v f ((arg.1, ⟨v.nextLoan - 1⟩) :: f.views) hw hf

/-- Law (subject `ConcreteSpec.Core.Own.life`). Every successful result preserves
ownership; refusal does not fabricate a result. -/
theorem life_preserves (types : Var → Option Descriptor) (e : Exec.Life) (s : State)
    (hs : WellFormed s) : StateResult (life types e s) := by
  cases e with
  | bind x => exact bind_preserves types s x hs
  | enter ps => exact enter_preserves types s ps hs
  | leave => exact leave_preserves s hs
  | drop xs => exact drop_preserves s xs hs

/-- Helper (computation). Normalizes the replacement guard; reinitialize_preserves establishes resource-state soundness. (approved: APPR-C5-codex-902ce88a). -/
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

/-- Helper (computation). Normalizes the child-exclusion guard; downward_reborrow_protocol establishes restoration without consuming older loans. (approved: APPR-C5-codex-902ce88a). -/
theorem endLoan_progress (s : State) (i : LoanId) (a : Loan)
    (found : loan s i = some a) (children : ∀ b ∈ s.loans, b.parent ≠ some i) :
    ∃ t, endLoan s i = some t := by
  have hc : s.loans.any (·.parent == some i) = false := by
    apply Bool.eq_false_iff.mpr
    intro hg
    obtain ⟨b, hb, he⟩ := List.any_eq_true.mp hg
    exact children b hb (by simpa using he)
  simp [endLoan, found, hc]

/-- Helper (computation). Computes a typed fresh binding; bind_preserves establishes ownership of its activation. (approved: APPR-C5-codex-902ce88a). -/
theorem bind_fresh_progress (types : Var → Option Descriptor) (s : State) (x : Var)
    (f : Activation) (d : Descriptor) (frame : s.frames.head? = some f)
    (issued : f.id < s.nextActivation) (fresh : AList.lookup f.locals x = none)
    (descriptor : types x = some d) : ∃ t, bind types x s = some t := by
  simp [bind, frame, issued, fresh, descriptor]

/-- Law (subject `ConcreteSpec.Core.Own.enter`). Fresh, distinct, typed parameters always enter: no global invariant scan can refuse them. -/
theorem enter_progress (types : Var → Option Descriptor) (s : State) (params : List Var)
    (unique : params.Nodup) (typed : ∀ x ∈ params, ∃ d, types x = some d) :
    ∃ t, enter types params s = some t := by
  suffices ∀ (xs : List Var) (s : State) (f : Activation),
      s.frames.head? = some f → f.id < s.nextActivation → xs.Nodup →
      (∀ x ∈ xs, AList.lookup f.locals x = none) →
      (∀ x ∈ xs, ∃ d, types x = some d) →
      ∃ t, xs.foldlM (fun s x => bind types x s) s = some t by
    exact this params
      { s with nextActivation := s.nextActivation + 1
               frames := ⟨s.nextActivation, [], []⟩ :: s.frames }
      ⟨s.nextActivation, [], []⟩ rfl (Nat.lt_succ_self _) unique
      (fun _ _ => rfl) typed
  intro xs
  induction xs with
  | nil => intro s _ _ _ _ _ _; exact ⟨s, rfl⟩
  | cons x xs ih =>
    intro s f frame issued unique fresh typed
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

/-- Helper (computation). Normalizes discharge and conflict checks; drop_preserves and leave_no_activation_slot establish cleanup safety. (approved: APPR-C5-codex-902ce88a). -/
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

/-- Helper (projection). Projects the owned-root clause of the well-formedness invariant for the operation-result law. (approved: APPR-C5-codex-902ce88a). -/
theorem loan_has_owned_root_of_wf (s : State) (hs : WellFormed s) (l : Loan) (hl : l ∈ s.loans) :
    ∃ x ∈ s.slots, x.place = l.root ∧ x.status = .owned := (hs.2.2.2.1 l hl).2.2.1


/-- Helper (inversion). Invariant consequence using uniqueness of slot identities and owned roots for the operation-result law. (approved: APPR-C5-codex-902ce88a). -/
theorem moved_no_loan_of_wf (s : State) (hs : WellFormed s) (x : Slot) (hx : x ∈ s.slots)
    (hm : x.status = .moved) (l : Loan) (hl : l ∈ s.loans) : x.place ≠ l.root := by
  intro he
  obtain ⟨y, hy, hp, ho⟩ := loan_has_owned_root_of_wf s hs l hl
  have hxy : x = y := slot_unique s.slots hs.1 x y hx hy (he.trans hp.symm)
  subst y
  rw [hm] at ho
  cases ho

/-- Helper (computation). Structural result proof for the operation-result theorem `instrument_erasure`. (approved: APPR-C5-codex-902ce88a). -/
theorem instrument_erasure_result {σ α : Type} (permit : State → Option State)
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

/-- Helper (computation). Structural result proof for the operation-result theorem `life_erasure`. (approved: APPR-C5-codex-902ce88a). -/
theorem life_erasure_result {σ : Type} (types : Var → Option Descriptor) (e : Exec.Life)
    (s t : Store σ) (h : lifeStore types e s = some t) :
    erase t = erase s ∧ life types e s.own = some t.own := by
  unfold lifeStore at h
  obtain ⟨own, he, h⟩ := Option.map_eq_some_iff.mp h
  subst t
  exact ⟨rfl, he⟩

/-- Helper (congruence). Transports the invariant through a loan map that retains semantic identity fields. (approved: APPR-C5-codex-902ce88a). -/
private theorem map_metadata_preserves (s : State) (hs : WellFormed s)
    (act : Loan → Nat) (pending : Loan → Bool)
    (issued : ∀ l ∈ s.loans, act l < s.nextActivation)
    (parents : ∀ l ∈ s.loans, pending l = true → (l.parent.isSome = true ∨ l.callRoot = true)) :
    WellFormed { s with loans := s.loans.map fun l =>
      { l with activation := act l, pendingArgument := pending l } } := by
  let change := fun l : Loan => { l with activation := act l, pendingArgument := pending l }
  refine ⟨hs.1, ?_, hs.2.2.1, ?_, ?_, hs.2.2.2.2.2⟩
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
    exact hs.2.2.2.2.1 x hx y hy

/-- Law (subject `ConcreteSpec.Core.Own.adoptDown`). Every successful result preserves
ownership; refusal does not fabricate a result. -/
theorem adoptDown_preserves (s : State) (args : List (Var × LoanId))
    (hs : WellFormed s) : StateResult (adoptDown s args) := by
  cases h : adoptDown s args with
  | none => trivial
  | some t =>
    unfold adoptDown at h
    obtain ⟨f, hf, h⟩ := Option.bind_eq_some_iff.mp h
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
      rw [← eq] at hw
      exact set_views_preserves _ f (args ++ f.views) hw hf
    · contradiction

/-- Law (subject `ConcreteSpec.Core.Own.passDown`). Every successful result preserves
ownership; refusal does not fabricate a result. -/
theorem passDown_preserves (s : CallState) (i : LoanId) (parameter : Var)
    (hs : WellFormed s.own) : CallOwnResult (passDown s i parameter) := by
  cases h : passDown s i parameter with
  | none => trivial
  | some t =>
    unfold passDown at h
    split at h
    · contradiction
    · obtain ⟨a, ha, h⟩ := Option.bind_eq_some_iff.mp h
      obtain ⟨u, hu, h⟩ := Option.bind_eq_some_iff.mp h
      cases h
      have hw : WellFormed u := by
        have h' := reborrow_preserves s.own i [] a.mode hs
        rwa [hu] at h'
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

/-- Law (subject `ConcreteSpec.Core.Own.passDown`). Every successful result preserves
ownership; refusal does not fabricate a result. Pending metadata stays synchronized when
argument evaluation adds a fresh child. -/
theorem passDown_call_preserves (s : CallState) (i : LoanId) (parameter : Var)
    (hs : CallState.WellFormed s) : CallResult (passDown s i parameter) := by
  cases h : passDown s i parameter with
  | none => trivial
  | some t =>
    have hw : WellFormed t.own := by
      have h' := passDown_preserves s i parameter hs.1
      rwa [h] at h'
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

/-- Law (subject `ConcreteSpec.Core.Own.callLife`). Every successful result preserves
ownership; refusal does not fabricate a result. C1's actual call/lifecycle callback preserves
ownership, for both owner and view parameters. -/
theorem callLife_preserves (types : Var → Option Descriptor) (e : Exec.Life)
    (s : CallState) (hs : WellFormed s.own) : CallOwnResult (callLife types e s) := by
  cases h : callLife types e s with
  | none => trivial
  | some t =>
    cases e with
    | enter params =>
      simp only [callLife] at h
      split at h
      · obtain ⟨u, hu, hh⟩ := Option.bind_eq_some_iff.mp h
        obtain ⟨v, hv, hh⟩ := Option.bind_eq_some_iff.mp hh
        cases hh
        have hu_wf : WellFormed u := by
          have h' := enter_preserves types s.own
            (params.filter fun x => decide (x ∉ s.pending.map Prod.fst)) hs
          rwa [hu] at h'
        have hv_wf := adoptDown_preserves u s.pending hu_wf
        rwa [hv] at hv_wf
      · contradiction
    | bind x =>
      simp only [callLife] at h
      split at h
      · obtain ⟨own, ho, hh⟩ := Option.map_eq_some_iff.mp h
        subst t
        have hw := life_preserves types (.bind x) s.own hs
        rwa [ho] at hw
      · contradiction
    | leave =>
      simp only [callLife] at h
      split at h
      · obtain ⟨own, ho, hh⟩ := Option.map_eq_some_iff.mp h
        subst t
        have hw := life_preserves types .leave s.own hs
        rwa [ho] at hw
      · contradiction
    | drop xs =>
      simp only [callLife] at h
      split at h
      · obtain ⟨own, ho, hh⟩ := Option.map_eq_some_iff.mp h
        subst t
        have hw := life_preserves types (.drop xs) s.own hs
        rwa [ho] at hw
      · contradiction

/-- Helper (computation). Reinitialization changes slot status while leaving the loan table unchanged. (approved: APPR-C5-codex-902ce88a). -/
private theorem reinitialize_loans (s t : State) (p : Hybrid.Place)
    (h : reinitialize s p = some t) : t.loans = s.loans := by
  unfold reinitialize at h
  obtain ⟨x, _, h⟩ := Option.bind_eq_some_iff.mp h
  split at h
  · cases h; rfl
  · contradiction

/-- Helper (computation). Both binding branches leave the loan table unchanged. (approved: APPR-C5-codex-902ce88a). -/
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

/-- Helper (congruence). Lifts a supplied loan-table equality through a monadic fold. (approved: APPR-C5-codex-902ce88a). -/
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

/-- Helper (congruence). Composes binding loan-table equalities through parameter entry. (approved: APPR-C5-codex-902ce88a). -/
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

/-- Helper (computation). Dropping slots does not edit the loan table. (approved: APPR-C5-codex-902ce88a). -/
private theorem drop_loans (s t : State) (xs : List Var) (h : drop xs s = some t) :
    t.loans = s.loans := by
  unfold drop at h
  obtain ⟨f, _, h⟩ := Option.bind_eq_some_iff.mp h
  dsimp at h
  split at h
  · cases h; rfl
  · contradiction

/-- Helper (inversion). Inverts membership in the filtered loan table. (approved: APPR-C5-codex-902ce88a). -/
private theorem endLoan_subset (s t : State) (i : LoanId) (h : endLoan s i = some t) :
    ∀ l ∈ t.loans, l ∈ s.loans := by
  unfold endLoan at h
  obtain ⟨a, _, h⟩ := Option.bind_eq_some_iff.mp h
  split at h
  · contradiction
  · cases h
    exact fun _ hl => (List.mem_filter.mp hl).1

/-- Helper (inversion). Combines lifecycle branch inversions to trace surviving loans to their input. (approved: APPR-C5-codex-902ce88a). -/
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
    cases h
    intro l hl
    rw [drop_loans u v _ hv] at hl
    exact fold_loans _ (fun s l t h => endLoan_subset s t l.id h) _ s u hu l hl

/-- Law (subject `ConcreteSpec.Core.Own.callLife`). Every successful result preserves
ownership; refusal does not fabricate a result. Entry consumes the pending metadata exactly
once; other lifecycle events require an empty argument buffer, so they cannot leave dangling
argument identities. -/
theorem callLife_call_preserves (types : Var → Option Descriptor) (e : Exec.Life)
    (s : CallState) (hs : CallState.WellFormed s) : CallResult (callLife types e s) := by
  cases h : callLife types e s with
  | none => trivial
  | some t =>
    have hw : WellFormed t.own := by
      have h' := callLife_preserves types e s hs.1
      rwa [h] at h'
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

/-- Helper (computation). Structural result proof for the operation-result theorem `callLife_erasure`. (approved: APPR-C5-codex-902ce88a). -/
theorem callLife_erasure_result {σ : Type} (types : Var → Option Descriptor) (e : Exec.Life)
    (s t : σ × CallState) (h : callLifeStore types e s = some t) :
    t.1 = s.1 ∧ callLife types e s.2 = some t.2 := by
  unfold callLifeStore at h
  obtain ⟨u, hu, hh⟩ := Option.map_eq_some_iff.mp h
  subst t
  exact ⟨rfl, hu⟩

/-- Helper (congruence). Structural result proof for the operation-result theorem `instrument_preserves`. (approved: APPR-C5-codex-902ce88a). -/
theorem instrument_preserves_result {σ α : Type} (permit : State → Option State)
    (hp : ∀ s t, WellFormed s → permit s = some t → WellFormed t) (operation : σ → Option (α × σ))
    (s : Store σ) (a : α) (t : Store σ) (hs : WellFormed s.own)
    (h : instrument permit operation s = some (a, t)) :
    WellFormed t.own := by
  unfold instrument at h
  obtain ⟨own, ho, hh⟩ := Option.bind_eq_some_iff.mp h
  obtain ⟨r, _, hh⟩ := Option.bind_eq_some_iff.mp hh
  cases r
  cases hh
  exact hp _ _ hs ho

/-- Helper (computation). Structural result proof for the operation-result theorem `readOwner_erasure`. (approved: APPR-C5-codex-902ce88a). -/
theorem readOwner_erasure_result (tgt : Target) (p : Hybrid.Place) (s t : Store Hybrid.State)
    (v : Hybrid.Value) (h : readOwner tgt p s = some (v, t)) :
    s.concrete.read tgt p = some v ∧ t.concrete = s.concrete := by
  have he := instrument_erasure_result _ _ _ _ _ h
  dsimp [erase] at he
  cases hc : s.concrete.read tgt p with
  | none => simp [hc] at he
  | some w => simp [hc] at he; rcases he with ⟨rfl, hstore⟩; exact ⟨rfl, hstore.symm⟩

/-- Helper (computation). Structural result proof for the operation-result theorem `writeOwner_erasure`. (approved: APPR-C5-codex-902ce88a). -/
theorem writeOwner_erasure_result (tgt : Target) (p : Hybrid.Place) (s t : Store Hybrid.State)
    (v : Hybrid.Value) (h : writeOwner tgt p v s = some ((), t)) :
    s.concrete.write tgt p v = some t.concrete := by
  have he := instrument_erasure_result _ _ _ _ _ h
  dsimp [erase] at he
  cases hc : s.concrete.write tgt p v with
  | none => simp [hc] at he
  | some st => simp [hc] at he; rw [← he]

/-- Helper (computation). Structural result proof for the operation-result theorem `readLoan_erasure`. (approved: APPR-C5-codex-902ce88a). -/
theorem readLoan_erasure_result (tgt : Target) (i : LoanId) (s t : Store Hybrid.State)
    (v : Hybrid.Value) (h : readLoan tgt i s = some (v, t)) :
    ∃ l, loan s.own i = some l ∧ l.path = [] ∧
      s.concrete.read tgt l.root = some v ∧ t.concrete = s.concrete := by
  unfold readLoan at h
  obtain ⟨l, hl, hh⟩ := Option.bind_eq_some_iff.mp h
  split at hh
  · rename_i hpath
    have he := instrument_erasure_result _ _ _ _ _ hh
    dsimp [erase] at he
    cases hc : s.concrete.read tgt l.root with
    | none => simp [hc] at he
    | some w => simp [hc] at he; rcases he with ⟨rfl, hstore⟩
                exact ⟨l, hl, hpath, hc, hstore.symm⟩
  · contradiction

/-- Helper (computation). Structural result proof for the operation-result theorem `writeLoan_erasure`. (approved: APPR-C5-codex-902ce88a). -/
theorem writeLoan_erasure_result (tgt : Target) (i : LoanId) (s t : Store Hybrid.State)
    (v : Hybrid.Value) (h : writeLoan tgt i v s = some ((), t)) :
    ∃ l, loan s.own i = some l ∧ l.path = [] ∧
      s.concrete.write tgt l.root v = some t.concrete := by
  unfold writeLoan at h
  obtain ⟨l, hl, hh⟩ := Option.bind_eq_some_iff.mp h
  split at hh
  · rename_i hpath
    have he := instrument_erasure_result _ _ _ _ _ hh
    dsimp [erase] at he
    cases hc : s.concrete.write tgt l.root v with
    | none => simp [hc] at he
    | some st => simp [hc] at he; exact ⟨l, hl, hpath, he ▸ hc⟩
  · contradiction

/-- Helper (arithmetic). Derived invariant or supplied-preservation fact; not independent soundness evidence. (approved: APPR-C5-codex-902ce88a). The next identity has never belonged to any currently active loan. Combined with the
unchanged/incremented supply laws, ending a loan cannot make its identity fresh again. -/
theorem nextLoan_fresh (s : State) (hs : WellFormed s) (l : Loan) (hl : l ∈ s.loans) :
    l.id ≠ LoanId.mk s.nextLoan := by
  intro he
  have hb := (hs.2.2.2.1 l hl).1
  rw [he] at hb
  exact Nat.lt_irrefl _ hb

/-- Helper (computation). Structural result proof for the operation-result theorem `borrow_birth`. (approved: APPR-C5-codex-902ce88a). -/
theorem borrow_birth_result (s t : State) (p : Hybrid.Place) (path : List Projection)
    (m : Mode) (h : borrow s p path m = some t) :
    t.nextLoan = s.nextLoan + 1 ∧
      ∃ l ∈ t.loans, l.id = ⟨s.nextLoan⟩ ∧ l.root = p ∧ l.path = path := by
  unfold borrow at h
  obtain ⟨f, _, hh⟩ := Option.bind_eq_some_iff.mp h
  split at hh
  · cases hh
    exact ⟨rfl, _, List.mem_cons_self, rfl, rfl, rfl⟩
  · contradiction

/-- Helper (computation). Structural result proof for the operation-result theorem `reborrow_birth`. (approved: APPR-C5-codex-902ce88a). -/
theorem reborrow_birth_result (s t : State) (i : LoanId) (suffix : List Projection)
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


/-- Helper (computation). Structural result proof for the operation-result theorem `endLoan_supply`. (approved: APPR-C5-codex-902ce88a). -/
theorem endLoan_supply_result (s t : State) (i : LoanId) (h : endLoan s i = some t) :
    t.nextLoan = s.nextLoan := by
  unfold endLoan at h
  obtain ⟨_, _, hh⟩ := Option.bind_eq_some_iff.mp h
  split at hh
  · contradiction
  · cases hh
    rfl

/-- Law (subject `ConcreteSpec.Core.Own.leave`). The semantic postcondition holds
of the actual successful operation result. -/
theorem leave_no_local_loan (s : State) (f : Activation) (entry : Var × Hybrid.Place)
    (hf : s.frames.head? = some f) (hs : WellFormed s) (he : entry ∈ f.locals) :
    ResultSatisfies (fun t : State => ∀ l ∈ t.loans, l.root ≠ entry.2) (leave s) := by
  cases h : leave s with
  | none => trivial
  | some t =>
    intro l hl
    have ht : WellFormed t := by
      have h' := leave_preserves s hs
      rwa [h] at h'
    unfold leave at h
    rw [hf] at h
    dsimp at h
    obtain ⟨u, hu, hh⟩ := Option.bind_eq_some_iff.mp h
    have hframes := fold_frames _ (fun s l t hh => endLoan_frames s t l.id hh) _ s u hu
    obtain ⟨v, hv, hh⟩ := Option.bind_eq_some_iff.mp hh
    have heq := Option.some.inj hh
    have hp : entry.2 ∈ List.map Prod.snd (f.locals.filter fun e =>
        decide (e.1 ∈ List.map Prod.fst f.locals)) := by
      apply List.mem_map.mpr
      exact ⟨entry, List.mem_filter.mpr ⟨he, by simp; exact ⟨entry.2, he⟩⟩, rfl⟩
    have habsent := drop_absent u v _ f (hframes ▸ hf) hv entry.2 hp
    intro hroot
    obtain ⟨x, hx, hxroot, _⟩ := loan_has_owned_root_of_wf t ht l hl
    have hxv : x ∈ v.slots := by
      rw [← heq] at hx
      exact hx
    exact habsent x hxv (hxroot.trans hroot)

/-- Helper (projection). Extracts the pending-argument witness checked by adoption. (approved: APPR-C5-codex-902ce88a). -/
theorem adoptDown_only_arguments_result (s t : State) (args : List (Var × LoanId))
    (h : adoptDown s args = some t) (e : Var × LoanId) (he : e ∈ args) :
    ∃ l ∈ s.loans, l.id = e.2 ∧ l.pendingArgument = true := by
  unfold adoptDown at h
  obtain ⟨_, _, hh⟩ := Option.bind_eq_some_iff.mp h
  split at hh
  · rename_i guard
    exact guard.2.2.2.1 e he
  · contradiction


/-- Helper (projection). Extracts the active-current-frame destination checked by reborrowing. (approved: APPR-C5-codex-902ce88a). -/
theorem reborrowTo_current_result (s t : State) (i : LoanId) (suffix : List Projection)
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

/-- Law (subject `ConcreteSpec.Core.Own.passOwner`). Every successful result preserves
ownership; refusal does not fabricate a result. A direct call argument adds a parentless
pending root loan and preserves ownership. -/
theorem passOwner_preserves (s : CallState) (p : Hybrid.Place) (path : List Projection)
    (m : Mode) (x : Var) (hs : WellFormed s.own) : CallOwnResult (passOwner s p path m x) := by
  cases h : passOwner s p path m x with
  | none => trivial
  | some t =>
    unfold passOwner at h
    split at h
    · contradiction
    · obtain ⟨f, _, h⟩ := Option.bind_eq_some_iff.mp h
      split at h
      · rename_i guard
        cases h
        obtain ⟨access, issued⟩ := Bool.and_eq_true_iff.mp guard
        obtain ⟨root, compat⟩ := owner_guard s.own p m access
        let a : Loan := ⟨⟨s.own.nextLoan⟩, p, path, m, none, [], f.id, true, true⟩
        have fresh : a.id ∉ s.own.loans.map Loan.id := by
          intro hm
          obtain ⟨b, hb, he⟩ := List.mem_map.mp hm
          have hn := (hs.2.2.2.1 b hb).1
          rw [he] at hn
          exact Nat.lt_irrefl _ hn
        have parent (l : Loan) (hl : ParentValid s.own l) :
            ParentValid { s.own with loans := a :: s.own.loans, nextLoan := s.own.nextLoan + 1 } l := by
          unfold ParentValid at hl ⊢
          cases he : l.parent with
          | none => simpa only [he] using hl
          | some p' =>
            simp only [he] at hl ⊢
            obtain ⟨b, hb, rest⟩ := hl
            exact ⟨b, List.mem_cons_of_mem _ hb, rest⟩
        have hcompat : ∀ b ∈ s.own.loans, Compatible a b := by
          intro b hb
          rcases compat b hb with hn | hm
          · exact Or.inl (Ne.symm hn)
          · exact Or.inr (Or.inr (Or.inr (Or.inr hm)))
        refine ⟨hs.1, ?_, hs.2.2.1, ?_, ?_, hs.2.2.2.2.2⟩
        · simpa only [List.map_cons, List.nodup_cons] using And.intro fresh hs.2.1
        · intro l hl
          rcases List.mem_cons.mp hl with he | hl
          · subst l
            exact ⟨Nat.lt_succ_self _, of_decide_eq_true issued, root, by simp [ParentValid], by simp⟩
          · obtain ⟨hb, ha, hr, hp, hpend⟩ := hs.2.2.2.1 l hl
            exact ⟨Nat.lt_succ_of_lt hb, ha, hr, parent l hp, hpend⟩
        · intro b hb c hc
          change b ∈ a :: s.own.loans at hb
          change c ∈ a :: s.own.loans at hc
          rcases List.mem_cons.mp hb with he | hb
          · subst b
            rcases List.mem_cons.mp hc with he | hc
            · subst c; exact Or.inr (Or.inl rfl)
            · exact hcompat c hc
          · rcases List.mem_cons.mp hc with he | hc
            · subst c; exact compatible_symm a b (hcompat b hb)
            · exact hs.2.2.2.2.1 b hb c hc
      · contradiction

/-- Law (subject `ConcreteSpec.Core.Own.passOwner`). Every successful result preserves
ownership; refusal does not fabricate a result. Direct owner argument evaluation preserves
synchronized call metadata as well as ownership. -/
theorem passOwner_call_preserves (s : CallState) (p : Hybrid.Place)
    (path : List Projection) (m : Mode) (x : Var) (hs : CallState.WellFormed s) :
    CallResult (passOwner s p path m x) := by
  cases h : passOwner s p path m x with
  | none => trivial
  | some t =>
    have hw : WellFormed t.own := by
      have h' := passOwner_preserves s p path m x hs.1
      rwa [h] at h'
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

/-- Law (subject `ConcreteSpec.Core.Own.passOwner`). The semantic postcondition holds
of the actual successful operation result. -/
theorem passOwner_freezes (s : CallState) (p : Hybrid.Place) (path : List Projection)
    (m : Mode) (x : Var) :
    ResultSatisfies (fun t : CallState =>
      ownerAccess t.own p .mutable = false ∧
        (m = .mutable → ownerAccess t.own p .shared = false))
      (passOwner s p path m x) := by
  cases h : passOwner s p path m x with
  | none => trivial
  | some t =>
    unfold passOwner at h
    split at h
    · contradiction
    · obtain ⟨f, _, h⟩ := Option.bind_eq_some_iff.mp h
      split at h
      · cases h
        cases m <;> simp [ResultSatisfies, ownerAccess, show (Mode.mutable == Mode.shared) = false from rfl]
      · contradiction

/-- Helper (computation). Structural result proof for the operation-result theorem `passOwner_birth`. (approved: APPR-C5-codex-902ce88a). -/
theorem passOwner_birth_result (s t : CallState) (p : Hybrid.Place) (path : List Projection)
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

/-- Helper (inversion). Unique loan identities identify the full loan record. (approved: APPR-C5-codex-902ce88a). -/
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

/-- Helper (inversion). Ending a list of other identities retains the entire loan record. (approved: APPR-C5-codex-902ce88a). -/
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

/-- Law (subject `ConcreteSpec.Core.Own.leave`). The semantic postcondition holds
of the actual successful operation result. -/
theorem leave_retains_caller (s : State) (f : Activation) (l : Loan)
    (hs : WellFormed s) (hf : s.frames.head? = some f) (hl : l ∈ s.loans)
    (caller : l.activation ≠ f.id) :
    ResultSatisfies (fun t : State => l ∈ t.loans) (leave s) := by
  cases h : leave s with
  | none => trivial
  | some t =>
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
    cases h
    exact retained

/-- Helper (inversion). Ending loans removes every scheduled identity and never adds loans. (approved: APPR-C5-codex-902ce88a). -/
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

/-- Law (subject `ConcreteSpec.Core.Own.leave`). The semantic postcondition holds
of the actual successful operation result. -/
theorem leave_no_activation_loan (s : State) (f : Activation)
    (hf : s.frames.head? = some f) :
    ResultSatisfies (fun t : State => ∀ l ∈ t.loans, l.activation ≠ f.id) (leave s) := by
  cases h : leave s with
  | none => trivial
  | some t =>
    unfold leave at h
    rw [hf] at h
    dsimp at h
    obtain ⟨u, hu, h⟩ := Option.bind_eq_some_iff.mp h
    obtain ⟨v, hv, h⟩ := Option.bind_eq_some_iff.mp h
    cases h
    intro l hl he
    rw [drop_loans u v _ hv] at hl
    obtain ⟨hs, removed⟩ := endLoans_removed _ s u hu l hl
    exact removed l (List.mem_filter.mpr ⟨hs, by simpa using he⟩) rfl

/-- Helper (computation). Structural result proof for the operation-result theorem `defer_keeps_store`. (approved: APPR-C5-codex-902ce88a). -/
theorem defer_keeps_store_result (O : Exec.Operands) (P : Exec.Program O)
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
  exact passOwner_birth_result caller waiting ⟨1⟩ [.index 3] .mutable z (by decide)
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

/-- Law (subject `ConcreteSpec.Core.Own.reborrowTo`). The semantic postcondition holds
of the actual successful operation result. -/
theorem reborrowTo_live (s : State) (i : LoanId) (path : List Projection)
    (m : Mode) (a : Nat) (hs : LiveDestinations s) :
    ResultSatisfies (fun t : State => LiveDestinations t) (reborrowTo s i path m a) := by
  cases h : reborrowTo s i path m a with
  | none => trivial
  | some t =>
    obtain ⟨f, hf, he, _⟩ := reborrowTo_current_result s t i path m a h
    have live : a ∈ s.frames.map Activation.id :=
      List.mem_map.mpr ⟨f, hf, he⟩
    unfold reborrowTo at h
    obtain ⟨_, _, h⟩ := Option.bind_eq_some_iff.mp h
    split at h
    · cases h
      intro l hl
      rcases List.mem_cons.mp hl with rfl | hl
      · exact live
      · exact hs l hl
    · contradiction

/-- Law (subject `ConcreteSpec.Core.Own.adoptDown`). The semantic postcondition holds
of the actual successful operation result. -/
theorem adoptDown_live (s : State) (args : List (Var × LoanId))
    (hs : LiveDestinations s) :
    ResultSatisfies (fun t : State => LiveDestinations t) (adoptDown s args) := by
  cases h : adoptDown s args with
  | none => trivial
  | some t =>
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

/-- Helper (projection). Successful return pops exactly the current activation frame. (approved: APPR-C5-codex-902ce88a). -/
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
    cases h
    simp
  · contradiction

/-- Law (subject `ConcreteSpec.Core.Own.leave`). The semantic postcondition holds
of the actual successful operation result. -/
theorem leave_live (s : State) (f : Activation) (hs : LiveDestinations s)
    (hf : s.frames.head? = some f) :
    ResultSatisfies (fun t : State => LiveDestinations t) (leave s) := by
  cases h : leave s with
  | none => trivial
  | some t =>
    intro l hl
    have old := life_subset (fun _ => none) .leave s t h l hl
    have live := hs l old
    have other : l.activation ≠ f.id := by
      have hno := leave_no_activation_loan s f hf
      rw [h] at hno
      exact hno l hl
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

/-- Helper (inversion). Structural inversion for `eval_ok_erasure` on a given ok result. (approved: APPR-C5-codex-902ce88a). -/
theorem eval_ok_erasure_result (L : AList Var O.Val) (s t : Store O.Store) (e : O.Expr) (v : O.Val)
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

/-- Helper (computation). Calculates one lifted operand result; the independently stated finite execution contract is Steps_erasure. (approved: APPR-C5-codex-902ce88a). -/
theorem eval_ok_erasure (L : AList Var O.Val) (s : Store O.Store) (e : O.Expr) :
    match (instrumentOperands O types permit).eval L s e with
    | .ok (v, t) => O.eval L (erase s) e = .ok (v, erase t)
    | _ => True := by
  cases h : (instrumentOperands O types permit).eval L s e with
  | ok r => exact eval_ok_erasure_result L s r.2 e r.1 h
  | trap _ => trivial
  | stuck => trivial

/-- Helper (inversion). Structural inversion for `eval_trap_erasure` on a given trap result. (approved: APPR-C5-codex-902ce88a). -/
theorem eval_trap_erasure_result (L : AList Var O.Val) (s : Store O.Store) (e : O.Expr) (kd : TrapKind)
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

/-- Helper (computation). Calculates one lifted trap result; runFrom_erasure establishes terminal execution agreement. (approved: APPR-C5-codex-902ce88a). -/
theorem eval_trap_erasure (L : AList Var O.Val) (s : Store O.Store) (e : O.Expr) :
    match (instrumentOperands O types permit).eval L s e with
    | .trap kd => O.eval L (erase s) e = .trap kd
    | _ => True := by
  cases h : (instrumentOperands O types permit).eval L s e with
  | ok _ => trivial
  | trap kd => exact eval_trap_erasure_result L s e kd h
  | stuck => trivial

@[reducible] def EvalArgsOkErasure (O : Exec.Operands) (L : AList Var O.Val) (args : List O.Expr)
    (s : Store O.Store) (r : Res (List O.Val × Store O.Store)) : Prop :=
  match r with
  | .ok (vs, t) => Exec.evalArgs O L (erase s) args = .ok (vs, erase t)
  | _ => True

/-- Law (subject `ConcreteSpec.Core.Own.instrumentOperands`). Successful argument evaluation
preserves values and concrete storage. The result remains inside the conclusion so a mutant
cannot hide a wrong value behind an output fixed as a theorem argument. -/
theorem evalArgs_ok_erasure (L : AList Var O.Val) (args : List O.Expr) (s : Store O.Store) :
    EvalArgsOkErasure O L args s (Exec.evalArgs (instrumentOperands O types permit) L s args) := by
  unfold EvalArgsOkErasure
  induction args generalizing s with
  | nil => rfl
  | cons e es ih =>
    cases h : Exec.evalArgs (instrumentOperands O types permit) L s (e :: es) with
    | trap _ => trivial
    | stuck => trivial
    | ok pair =>
      rcases pair with ⟨vs, t⟩
      simp only [Exec.evalArgs, instrumentOperands] at h ⊢
      cases he : instrumentEval O permit L s e with
      | ok r =>
        rcases r with ⟨v, s₁⟩
        rw [he] at h
        rw [eval_ok_erasure_result (O := O) (types := types) L s s₁ e v he]
        dsimp [Res.bind] at h ⊢
        have ih₁ := ih s₁
        cases hes : Exec.evalArgs (instrumentOperands O types permit) L s₁ es with
        | ok r' =>
          rcases r' with ⟨vs', s₂⟩
          rw [hes] at ih₁
          simp only [instrumentOperands] at hes
          rw [hes] at h
          rw [ih₁]
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

@[reducible] def EvalArgsTrapErasure (O : Exec.Operands) (L : AList Var O.Val) (args : List O.Expr)
    (s : Store O.Store) (r : Res (List O.Val × Store O.Store)) : Prop :=
  match r with
  | .trap kd => Exec.evalArgs O L (erase s) args = .trap kd
  | _ => True

/-- Law (subject `ConcreteSpec.Core.Own.instrumentOperands`). A trapping argument evaluation
preserves the actual trap kind, without fixing that kind in a success premise. -/
theorem evalArgs_trap_erasure (L : AList Var O.Val) (args : List O.Expr) (s : Store O.Store) :
    EvalArgsTrapErasure O L args s (Exec.evalArgs (instrumentOperands O types permit) L s args) := by
  unfold EvalArgsTrapErasure
  induction args generalizing s with
  | nil => trivial
  | cons e es ih =>
    cases h : Exec.evalArgs (instrumentOperands O types permit) L s (e :: es) with
    | ok _ => trivial
    | stuck => trivial
    | trap kd =>
      simp only [Exec.evalArgs, instrumentOperands] at h ⊢
      cases he : instrumentEval O permit L s e with
      | ok r =>
        rcases r with ⟨v, s₁⟩
        rw [he] at h
        rw [eval_ok_erasure_result (O := O) (types := types) L s s₁ e v he]
        dsimp [Res.bind] at h ⊢
        have ih₁ := ih s₁
        cases hes : Exec.evalArgs (instrumentOperands O types permit) L s₁ es with
        | ok r' =>
          rcases r' with ⟨_, _⟩
          simp only [instrumentOperands] at hes
          rw [hes] at h; cases h
        | trap kd' =>
          rw [hes] at ih₁
          simp only [instrumentOperands] at hes
          rw [hes] at h
          cases h
          rw [ih₁]
          rfl
        | stuck =>
          simp only [instrumentOperands] at hes
          rw [hes] at h; cases h
      | trap _ =>
        rw [he] at h
        cases h
        rw [eval_trap_erasure_result (O := O) (types := types) L s e kd he]
        rfl
      | stuck => rw [he] at h; cases h

/-- Helper (projection). Condition tests are identical in the instrumented and erased operand instances. (approved: APPR-C5-codex-902ce88a). -/
theorem test_erasure_result (v : O.Val) :
    (instrumentOperands O types permit).test v = O.test v := rfl

/-- Helper (computation). Projects the unchanged concrete test; Steps_erasure establishes agreement of actual branch execution. (approved: APPR-C5-codex-902ce88a). -/
theorem test_erasure (v : O.Val) :
    match (instrumentOperands O types permit).test v with
    | some b => O.test v = some b
    | none => O.test v = none := by
  dsimp [instrumentOperands]
  cases O.test v <;> rfl

/-- Helper (inversion). Structural inversion for `instrumentLife_erasure` on a given output store. (approved: APPR-C5-codex-902ce88a). -/
theorem instrumentLife_erasure_result (ev : Exec.Life) (s t : Store O.Store)
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

/-- Helper (computation). Calculates the lifted lifecycle result; Steps_erasure establishes execution agreement through cleanup. (approved: APPR-C5-codex-902ce88a). -/
theorem instrumentLife_erasure (ev : Exec.Life) (s : Store O.Store) :
    match (instrumentOperands O types permit).life ev s with
    | some t => O.life ev (erase s) = some (erase t) ∧ life types ev s.own = some t.own
    | none => True := by
  cases h : (instrumentOperands O types permit).life ev s with
  | none => trivial
  | some t =>
    refine ⟨instrumentLife_erasure_result ev s t h, ?_⟩
    dsimp [instrumentOperands, instrumentLife] at h
    cases hown : life types ev s.own with
    | none => rw [hown] at h; cases h
    | some own =>
      rw [hown] at h
      cases hc : O.life ev s.concrete with
      | none => rw [hc] at h; cases h
      | some concrete => rw [hc] at h; cases h; rfl

/-- Helper (projection). Extracts the two defining conjuncts of the instrumented answer guard. (approved: APPR-C5-codex-902ce88a). -/
theorem allows_erasure_result (p : O.Prim) (vs : List O.Val) (s : Store O.Store)
    (a : Exec.Answer O.Val O.Event (Store O.Store))
    (h : (instrumentOperands O types permit).allows p vs s a = true) :
    O.allows p vs (erase s) (eraseAnswer a) = true ∧ a.store.own = s.own := by
  dsimp [instrumentOperands, instrumentAllows] at h
  obtain ⟨ha, hown⟩ := Bool.and_eq_true_iff.mp h
  exact ⟨ha, of_decide_eq_true hown⟩

/-- Helper (computation). Unpacks the primitive answer guard; Steps_erasure and runFrom_erasure establish actual primitive execution agreement. (approved: APPR-C5-codex-902ce88a). -/
theorem allows_erasure (p : O.Prim) (vs : List O.Val) (s : Store O.Store)
    (a : Exec.Answer O.Val O.Event (Store O.Store)) :
    ((instrumentOperands O types permit).allows p vs s a = true ↔
      O.allows p vs (erase s) (eraseAnswer a) = true ∧ a.store.own = s.own) := by
  dsimp [instrumentOperands, instrumentAllows]
  rw [Bool.and_eq_true_iff, decide_eq_true_eq]

/-- Helper (projection). Result-binding lifecycle erases to the concrete `Exec.bindLife`. (approved: APPR-C5-codex-902ce88a). -/
private theorem bindLife_erasure_proj (dst : Option Var) (s t : Store O.Store)
    (h : Exec.bindLife (instrumentOperands O types permit) dst s = some t) :
    Exec.bindLife O dst (erase s) = some (erase t) := by
  cases dst with
  | none => cases h; rfl
  | some x => exact instrumentLife_erasure_result (.bind x) s t h

/-- Helper (projection). Scope-exit drop erases to the concrete `Exec.dropOut`. (approved: APPR-C5-codex-902ce88a). -/
private theorem dropOut_erasure_proj (dm : List Var) (L : AList Var O.Val) (s t : Store O.Store)
    (h : Exec.dropOut (instrumentOperands O types permit) dm L s = some t) :
    Exec.dropOut O dm L (erase s) = some (erase t) :=
  instrumentLife_erasure_result (.drop (Exec.dead dm L)) s t h

/-- Helper (projection). Deferred cleanup registration commutes with frame erasure. (approved: APPR-C5-codex-902ce88a). -/
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

/-- Helper (projection). Function lookup in an erased program erases the looked-up function. (approved: APPR-C5-codex-902ce88a). -/
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

@[reducible] def StepNextErasure (O : Exec.Operands) (types : Var → Option Descriptor)
    (permit : AList Var O.Val → O.Expr → State → Option State)
    (advance : Exec.Config (instrumentOperands O types permit) →
      Option (Exec.Out (instrumentOperands O types permit)))
    (P : Exec.Program (instrumentOperands O types permit))
    (c : Exec.Config (instrumentOperands O types permit)) : Prop :=
  match advance c with
  | some (.next c') => Exec.step (eraseProgram P) (eraseConfig c) = some (.next (eraseConfig c'))
  | _ => True

/-- Law (subject `ConcreteSpec.Core.Own.stepInstrumented`). Silent machine-step forward simulation
(owner decision 2026-11-08 Q3): whenever `stepInstrumented` on the instrumented C1 machine produces a next
configuration, `Exec.step` on the erased machine produces the erased configuration. -/
theorem step_next_erasure (P : Exec.Program (instrumentOperands O types permit))
    (c : Exec.Config (instrumentOperands O types permit)) :
    StepNextErasure O types permit (stepInstrumented O types permit P) P c := by
  unfold StepNextErasure
  cases h : stepInstrumented O types permit P c with
  | none => trivial
  | some out =>
    cases out with
    | request _ _ _ _ _ => trivial
    | next c' =>
      change Exec.step P c = some (.next c') at h
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
            rw [eval_ok_erasure_result (O := O) (types := types) L st st' e v he]
            simp [Exec.onRes,
              instrumentLife_erasure_result (O := O) (permit := permit) (.bind x) st' st'' hst'']
          | trap kd =>
            rw [he] at h
            cases h
            rw [eval_trap_erasure_result (O := O) (types := types) L st e kd he]
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
            rw [eval_ok_erasure_result (O := O) (types := types) L st st' e v he]
            simp [Exec.onRes, hw,
              instrumentLife_erasure_result (O := O) (permit := permit) (.bind x) st' st'' hst'']
          | trap kd =>
            rw [he] at h
            cases h
            rw [eval_trap_erasure_result (O := O) (types := types) L st e kd he]
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
            rw [eval_ok_erasure_result (O := O) (types := types) L st st' e v he]
            cases b <;> simp [Exec.onRes, hb]
          | trap kd =>
            rw [he] at h
            cases h
            rw [eval_trap_erasure_result (O := O) (types := types) L st e kd he]
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
            rw [eval_ok_erasure_result (O := O) (types := types) L st st' e v he]
            simp [Exec.onRes]
          | trap kd =>
            rw [he] at h
            cases h
            rw [eval_trap_erasure_result (O := O) (types := types) L st e kd he]
            simp [Exec.onRes]
          | stuck => rw [he] at h; cases h
        | call dst f args site =>
          dsimp [Exec.step, eraseConfig, eraseCtrl, eraseStmt] at h ⊢
          cases he : Exec.evalArgs (instrumentOperands O types permit) L st args with
          | ok r =>
            rcases r with ⟨vs, st'⟩
            have he_erased : Exec.evalArgs O L (erase st) args = .ok (vs, erase st') := by
              have h0 := evalArgs_ok_erasure (O := O) (types := types) (permit := permit) L args st
              rw [he] at h0
              exact h0
            rw [he] at h
            simp only [Exec.onRes, Option.bind_eq_some_iff] at h
            obtain ⟨fn, hfn, h⟩ := h
            by_cases hlen : fn.params.length = vs.length
            · simp only [hlen, ↓reduceIte, Option.map_eq_some_iff] at h
              obtain ⟨st'', hst'', hnext⟩ := h
              cases hnext
              rw [he_erased]
              dsimp [Exec.onRes, eraseProgram]
              rw [lookup_eraseProgram_proj P.fns f, hfn]
              dsimp [Option.bind, eraseFn]
              simp only [hlen, ↓reduceIte]
              rw [instrumentLife_erasure_result (O := O) (permit := permit)
                (.enter (fn.params.map Prod.fst)) st' st'' hst'']
              rfl
            · simp only [hlen, ↓reduceIte] at h
              cases h
          | trap kd =>
            have he_erased : Exec.evalArgs O L (erase st) args = .trap kd := by
              have h0 := evalArgs_trap_erasure (O := O) (types := types) (permit := permit) L args st
              rw [he] at h0
              exact h0
            rw [he] at h
            cases h
            rw [he_erased]
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
            have he_erased : Exec.evalArgs O L (erase st) args = .trap kd := by
              have h0 := evalArgs_trap_erasure (O := O) (types := types) (permit := permit) L args st
              rw [he] at h0
              exact h0
            rw [he] at h
            cases h
            rw [he_erased]
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
              rw [eval_ok_erasure_result (O := O) (types := types) L st st' t v he]
              dsimp [Exec.onRes]
              rw [hgo]
              dsimp [Option.bind]
              rw [dropOut_erasure_proj (O := O) (permit := permit) dm L st' st'' hst'']
              cases go <;> cases hnext <;> rfl
            | trap kd =>
              rw [he] at h
              cases h
              rw [eval_trap_erasure_result (O := O) (types := types) L st t kd he]
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
            rw [instrumentLife_erasure_result (O := O) (permit := permit) .leave st st' hst']
            dsimp [Option.bind]
            rw [bindLife_erasure_proj (O := O) (permit := permit) dst st' st'' hst'']
            rfl
      | exiting e =>
        dsimp [Exec.step, eraseConfig, eraseCtrl] at h ⊢
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
              simp only [Exec.stepExit, Option.map_eq_some_iff] at h
              obtain ⟨st', hst', hnext⟩ := h
              cases hnext
              simp [Exec.stepExit, eraseFrame, dropOut_erasure_proj (O := O) (permit := permit) dm L st st' hst']
            | cons d ds =>
              cases h
              rfl
          | test l cr t body site dm =>
            simp only [Exec.stepExit, Option.map_eq_some_iff] at h
            obtain ⟨st', hst', hnext⟩ := h
            cases hnext
            simp [Exec.stepExit, eraseFrame, dropOut_erasure_proj (O := O) (permit := permit) dm L st st' hst']
          | body l cr t b site =>
            cases e with
            | brk l' =>
              by_cases hl : l' = l
              · simp only [Exec.stepExit, hl, ↓reduceIte, Option.some.injEq] at h
                cases h
                simp [Exec.stepExit, eraseFrame, hl]
              · simp only [Exec.stepExit, hl, ↓reduceIte, Option.some.injEq] at h
                cases h
                simp [Exec.stepExit, eraseFrame, hl]
            | cont l' =>
              by_cases hl : l' = l
              · simp only [Exec.stepExit, hl, ↓reduceIte, Option.some.injEq] at h
                cases h
                simp [Exec.stepExit, eraseFrame, hl, eraseStmt]
              · simp only [Exec.stepExit, hl, ↓reduceIte, Option.some.injEq] at h
                cases h
                simp [Exec.stepExit, eraseFrame, hl]
            | ret v =>
              cases h
              rfl
          | resume p => cases h
          | ret dst caller =>
            cases e with
            | brk _ => cases h
            | cont _ => cases h
            | ret v =>
              simp only [Exec.stepExit, Option.bind_eq_some_iff, Option.map_eq_some_iff] at h
              obtain ⟨st', hst', st'', hst'', hnext⟩ := h
              cases hnext
              dsimp [Exec.stepExit, eraseFrame, eraseConfig, eraseCtrl, List.map]
              rw [instrumentLife_erasure_result (O := O) (permit := permit) .leave st st' hst']
              dsimp [Option.bind]
              rw [bindLife_erasure_proj (O := O) (permit := permit) dst st' st'' hst'']
              rfl
      | trapped _ _ => cases h

@[reducible] def StepRequestErasure (O : Exec.Operands) (types : Var → Option Descriptor)
    (permit : AList Var O.Val → O.Expr → State → Option State)
    (advance : Exec.Config (instrumentOperands O types permit) →
      Option (Exec.Out (instrumentOperands O types permit)))
    (P : Exec.Program (instrumentOperands O types permit))
    (c : Exec.Config (instrumentOperands O types permit)) : Prop :=
  match advance c with
  | some (.request p vs site st k) =>
    ∃ k_erased,
      Exec.step (eraseProgram P) (eraseConfig c) =
        some (.request p vs site (erase st) k_erased) ∧
      ∀ a c', k a = some c' → k_erased (eraseAnswer a) = some (eraseConfig c')
  | _ => True

/-- Law (subject `ConcreteSpec.Core.Own.stepInstrumented`). Primitive-request forward simulation
(owner decision 2026-11-08 Q3): whenever `stepInstrumented` on the instrumented C1 machine issues a primitive
request, `Exec.step` on the erased machine issues the matching request with the erased store and agreeing
continuation. -/
theorem step_request_erasure (P : Exec.Program (instrumentOperands O types permit))
    (c : Exec.Config (instrumentOperands O types permit)) :
    StepRequestErasure O types permit (stepInstrumented O types permit P) P c := by
  unfold StepRequestErasure
  cases h : stepInstrumented O types permit P c with
  | none => trivial
  | some out =>
    cases out with
    | next _ => trivial
    | request p vs site st k =>
      change Exec.step P c = some (.request p vs site st k) at h
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
            have he_erased : Exec.evalArgs O L (erase s) args = .ok (vs', erase st') := by
              have h0 := evalArgs_ok_erasure (O := O) (types := types) (permit := permit) L args s
              rw [he] at h0
              exact h0
            rw [he] at h
            dsimp [Exec.onRes] at h
            injection h with h
            injection h with hp hvs hsite hst hk_eq
            subst hp; subst hvs; subst hsite; subst hst; subst hk_eq
            refine ⟨_, by rw [he_erased]; rfl, ?_⟩
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
        cases frames with
        | nil => cases h
        | cons f k' =>
          cases f with
          | seq _ => cases h
          | scope b dm ds =>
            cases ds with
            | nil =>
              simp only [Exec.stepExit, Option.map_eq_some_iff] at h
              obtain ⟨_, _, h⟩ := h; cases h
            | cons _ _ => cases h
          | test _ _ _ _ _ _ =>
            simp only [Exec.stepExit, Option.map_eq_some_iff] at h
            obtain ⟨_, _, h⟩ := h; cases h
          | body l _ _ _ _ =>
            cases e with
            | brk l' => by_cases hl : l' = l <;> simp [Exec.stepExit, hl] at h
            | cont l' => by_cases hl : l' = l <;> simp [Exec.stepExit, hl] at h
            | ret _ => cases h
          | resume _ => cases h
          | ret _ _ =>
            cases e with
            | brk _ => cases h
            | cont _ => cases h
            | ret _ =>
              simp only [Exec.stepExit, Option.bind_eq_some_iff, Option.map_eq_some_iff] at h
              obtain ⟨_, _, _, _, h⟩ := h; cases h
      | trapped _ _ => cases h

/-- Helper (projection). Erasing a step label preserves its emitted event list. (approved: APPR-C5-codex-902ce88a). -/
private theorem eraseLabel_events_proj (l : Exec.Label (instrumentOperands O types permit)) :
    (eraseLabel l).events = l.events := by
  cases l <;> rfl

/-- Helper (projection). Erasing an answer's step label equals the erased answer's label. (approved: APPR-C5-codex-902ce88a). -/
private theorem eraseLabel_answer_proj (a : Exec.Answer O.Val O.Event (Store O.Store)) :
    eraseLabel (O := O) (types := types) (permit := permit) a.label = (eraseAnswer a).label := by
  rcases a with ⟨ev, _, _⟩
  cases ev <;> rfl

@[reducible] def StepErasure (O : Exec.Operands) (types : Var → Option Descriptor)
    (permit : AList Var O.Val → O.Expr → State → Option State)
    (advance : Exec.Config (instrumentOperands O types permit) →
      Option (Exec.Out (instrumentOperands O types permit)))
    (P : Exec.Program (instrumentOperands O types permit))
    (c : Exec.Config (instrumentOperands O types permit)) : Prop :=
  match advance c with
  | some (.next c') =>
    Exec.Step (eraseProgram P) (eraseConfig c) .silent (eraseConfig c')
  | some (.request p vs _site st k) =>
    ∀ a c', (instrumentOperands O types permit).allows p vs st a = true →
      k a = some c' →
      Exec.Step (eraseProgram P) (eraseConfig c) (eraseLabel a.label) (eraseConfig c')
  | none => True

/-- Law (subject `ConcreteSpec.Core.Own.stepInstrumented`). Single-step forward simulation
(owner decision 2026-11-08 Q3): every labeled step produced by `stepInstrumented` on the instrumented C1 machine is
matched by a step of the erased machine with the same event label and concrete store. -/
theorem Step_erasure (P : Exec.Program (instrumentOperands O types permit))
    (c : Exec.Config (instrumentOperands O types permit)) :
    StepErasure O types permit (stepInstrumented O types permit P) P c := by
  unfold StepErasure
  cases hs : stepInstrumented O types permit P c with
  | none => trivial
  | some out =>
    cases out with
    | next c' =>
      have hnext := step_next_erasure P c
      unfold StepNextErasure at hnext
      rw [hs] at hnext
      exact .silent hnext
    | request p vs site st k =>
      intro a c' hallows hk
      have hreq := step_request_erasure P c
      unfold StepRequestErasure at hreq
      rw [hs] at hreq
      obtain ⟨k_erased, hs_erased, hk_erased⟩ := hreq
      obtain ⟨hallows_erased, _⟩ := allows_erasure_result _ _ _ _ hallows
      rw [eraseLabel_answer_proj]
      exact .prim hs_erased hallows_erased (hk_erased _ _ hk)

/-- Law (subject `ConcreteSpec.Core.Own.stepInstrumented`). Every finite execution of the
instrumented machine is simulated by C1 with the instrumentation erased. This is the
unconditional Q3 obligation, separate from oracle evaluation and its `Ending.NotStuck` premise. -/
theorem Steps_erasure (P : Exec.Program (instrumentOperands O types permit))
    (c : Exec.Config (instrumentOperands O types permit)) :
    FiniteSimulation O types permit (stepInstrumented O types permit P) P c := by
  intro tr c' h
  induction h with
  | refl c => exact .refl (eraseConfig c)
  | @silent c₁ c₂ _ _ hs _ ih =>
    have hstep := Step_erasure P c₁
    unfold StepErasure at hstep
    rw [hs] at hstep
    exact .step hstep ih
  | @prim c₁ c₂ _ _ p vs site st k a hs ha hk _ ih =>
    have hstep := Step_erasure P c₁
    unfold StepErasure at hstep
    rw [hs] at hstep
    have hstep' := hstep a c₂ ha hk
    simpa [eraseLabel_events_proj] using Exec.Steps.step hstep' ih

/-- Helper (projection). Configuration finality is invariant under erasure. (approved: APPR-C5-codex-902ce88a). -/
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

/-- Helper (projection). Lifting a concrete answer preserves the primitive contract check. (approved: APPR-C5-codex-902ce88a). -/
private theorem allows_liftAnswer_proj (p : O.Prim) (vs : List O.Val) (s : Store O.Store)
    (a : Exec.Answer O.Val O.Event O.Store) :
    (instrumentOperands O types permit).allows p vs s (liftAnswer s.own a) =
      O.allows p vs (erase s) a := by
  simp [instrumentOperands, instrumentAllows, eraseAnswer, liftAnswer, erase]

/-- Law (subject `ConcreteSpec.Core.Own.liftOracle`). Fuel-indexed evaluator forward simulation (owner decision 2026-11-08 Q3): whenever
the instrumented run reaches a non-stuck outcome (final—normal, returned or trapped—
noAnswer, badAnswer or outOfFuel), the erased run reaches the matching outcome with
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
          have hnext := step_next_erasure P c
          unfold StepNextErasure at hnext
          rw [show stepInstrumented O types permit P c = some (.next c') from hs] at hnext
          rw [hnext]
          exact runFrom_erasure P ρ n hist c' hns
        | request p vs site st k =>
          have hreq := step_request_erasure P c
          unfold StepRequestErasure at hreq
          rw [show stepInstrumented O types permit P c = some (.request p vs site st k) from hs] at hreq
          obtain ⟨k_erased, hs_erased, hk_erased⟩ := hreq
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

/-- Law (subject `ConcreteSpec.Core.Own.liftOracle`). Whole-run forward simulation from an empty event history (owner decision 2026-11-08 Q3). -/
theorem run_erasure (P : Exec.Program (instrumentOperands O types permit))
    (ρ : Exec.Oracle O) (fuel : Nat)
    (c : Exec.Config (instrumentOperands O types permit))
    (h : Ending.NotStuck (Exec.run P (liftOracle O types permit ρ) fuel c).ending) :
    Exec.run (eraseProgram P) ρ fuel (eraseConfig c) =
      eraseReport (Exec.run P (liftOracle O types permit ρ) fuel c) :=
  runFrom_erasure P ρ fuel [] c h


/-- Helper (inversion). Under `EvalPermitted`, an instrumented operand evaluation that is
stuck is also stuck in the erased operand instance. (approved: APPR-C5-codex-902ce88a). -/
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

/-- Helper (inversion). Under `LifePermitted`, an instrumented lifecycle transition that
returns `none` also returns `none` on the erased store. (approved: APPR-C5-codex-902ce88a). -/
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
lifecycle that returns `none` also returns `none` on the erased store. (approved: APPR-C5-codex-902ce88a). -/
private theorem bindLife_none_of_permitted_inv (dst : Option Var) (s : Store O.Store)
    (hp : BindDstPermitted O types dst s)
    (hs : Exec.bindLife (instrumentOperands O types permit) dst s = none) :
    Exec.bindLife O dst (erase s) = none := by
  cases dst with
  | none => cases hs
  | some x => exact life_none_of_permitted_inv (O := O) (permit := permit) (.bind x) s hp hs

/-- Helper (inversion). Under `LifePermitted`, an instrumented scope-exit drop that
returns `none` also returns `none` on the erased store. (approved: APPR-C5-codex-902ce88a). -/
private theorem dropOut_none_of_permitted_inv (dm : List Var) (L : AList Var O.Val)
    (s : Store O.Store) (hp : LifePermitted O types (.drop (Exec.dead dm L)) s)
    (hs : Exec.dropOut (instrumentOperands O types permit) dm L s = none) :
    Exec.dropOut O dm L (erase s) = none :=
  life_none_of_permitted_inv (O := O) (permit := permit) (.drop (Exec.dead dm L)) s hp hs

/-- Helper (projection). Deferred registration failure is invariant under frame erasure. (approved: APPR-C5-codex-902ce88a). -/
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

/-- Helper (inversion). `CheckerAccepted` is preserved across any single machine step. (approved: APPR-C5-codex-902ce88a). -/
private theorem checkerAccepted_step_inv
    (P : Exec.Program (instrumentOperands O types permit))
    (c c' : Exec.Config (instrumentOperands O types permit))
    (l : Exec.Label (instrumentOperands O types permit))
    (hacc : CheckerAccepted O types permit P c)
    (hs : Exec.Step P c l c') :
    CheckerAccepted O types permit P c' :=
  fun tr c'' hsteps => hacc (l.events ++ tr) c'' (.step hs hsteps)

/-- Law (subject `ConcreteSpec.Core.Own.stepInstrumented`). Single-step stuck equivalence for checker-accepted programs (owner decision 2026-11-08 Q3):
at any configuration accepted by the ownership checker, the instrumented C1 machine has no
step iff the erased machine has no step. -/
theorem step_stuck_iff_of_accepted
    (P : Exec.Program (instrumentOperands O types permit))
    (c : Exec.Config (instrumentOperands O types permit))
    (hacc : CheckerAccepted O types permit P c) :
    stepInstrumented O types permit P c = none ↔
      Exec.step (eraseProgram P) (eraseConfig c) = none := by
  constructor
  · intro hs
    have hp : StepPermitted O types permit P c := (hacc [] c (.refl c)).2
    clear hacc
    change Exec.step P c = none at hs
    rcases c with ⟨ctrl, L, st, k⟩
    have hargs_stuck : ∀ (args : List O.Expr) (s : Store O.Store),
        ArgsPermitted O types permit L s args →
        Exec.evalArgs (instrumentOperands O types permit) L s args = .stuck →
        Exec.evalArgs O L (erase s) args = .stuck := by
      intro args
      induction args with
      | nil => intro _ _ h0; cases h0
      | cons e es ih =>
        intro s ⟨hpe, hpes⟩ h0
        simp only [Exec.evalArgs, instrumentOperands] at h0 ⊢
        cases he : instrumentEval O permit L s e with
        | ok r =>
          rcases r with ⟨v, s₁⟩
          rw [he] at h0
          rw [eval_ok_erasure_result (O := O) (types := types) L s s₁ e v he]
          dsimp [Res.bind] at h0 ⊢
          cases hes : Exec.evalArgs (instrumentOperands O types permit) L s₁ es with
          | ok r' =>
            rcases r' with ⟨_, _⟩
            simp only [instrumentOperands] at hes
            rw [hes] at h0; cases h0
          | trap _ =>
            simp only [instrumentOperands] at hes
            rw [hes] at h0; cases h0
          | stuck =>
            rw [ih s₁ (hpes v s₁ he) hes]
            rfl
        | trap _ => rw [he] at h0; cases h0
        | stuck =>
          rw [eval_stuck_of_permitted_inv (O := O) (types := types) L s e hpe he]
          rfl
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
          rw [eval_ok_erasure_result (O := O) (types := types) L st st' e v he]
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
          rw [eval_ok_erasure_result (O := O) (types := types) L st st' e v he]
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
          rw [eval_ok_erasure_result (O := O) (types := types) L st st' e v he]
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
          have he_erased : Exec.evalArgs O L (erase st) args = .ok (vs, erase st') := by
            have h0 := evalArgs_ok_erasure (O := O) (types := types) (permit := permit) L args st
            rw [he] at h0
            exact h0
          rw [he] at hs
          rw [he_erased]
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
          rw [hargs_stuck args st hpa he]
          rfl
      | prim x p args site =>
        obtain ⟨hpa, _⟩ := hp
        dsimp [Exec.step, eraseConfig, eraseCtrl, eraseStmt] at hs ⊢
        cases he : Exec.evalArgs (instrumentOperands O types permit) L st args with
        | ok r => rcases r with ⟨_, _⟩; rw [he] at hs; cases hs
        | trap _ => rw [he] at hs; cases hs
        | stuck =>
          rw [hargs_stuck args st hpa he]
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
            rw [eval_ok_erasure_result (O := O) (types := types) L st st' t v he]
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
            rw [instrumentLife_erasure_result (O := O) (permit := permit) .leave st st' hl]
            dsimp [Option.bind]
            rw [bindLife_none_of_permitted_inv (O := O) (permit := permit) dst st' (hbind st' hl) hs]
            rfl
    | exiting e =>
      dsimp [Exec.step, eraseConfig, eraseCtrl] at hs ⊢
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
            rw [dropOut_none_of_permitted_inv (O := O) (permit := permit) dm L st hp hs]
            rfl
          | cons _ _ => cases hs
        | test l cr t body site dm =>
          simp only [Exec.stepExit, Option.map_eq_none_iff] at hs
          dsimp [Exec.stepExit, eraseFrame, List.map]
          rw [dropOut_none_of_permitted_inv (O := O) (permit := permit) dm L st hp hs]
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
            cases hl : (instrumentOperands O types permit).life .leave st with
            | none =>
              rw [life_none_of_permitted_inv (O := O) (permit := permit) .leave st hleave hl]
              rfl
            | some st' =>
              have hl' : instrumentLife O types .leave st = some st' := hl
              rw [hl'] at hs
              dsimp [Option.bind] at hs
              simp only [Option.map_eq_none_iff] at hs
              rw [instrumentLife_erasure_result (O := O) (permit := permit) .leave st st' hl]
              dsimp [Option.bind]
              rw [bindLife_none_of_permitted_inv (O := O) (permit := permit) dst st' (hbind st' hl) hs]
              rfl
    | trapped _ _ => rfl
  · intro herased
    cases hs : stepInstrumented O types permit P c with
    | none => rfl
    | some out =>
      cases out with
      | next c' =>
        have h' := step_next_erasure P c
        unfold StepNextErasure at h'
        rw [hs, herased] at h'
        cases h'
      | request p vs site st k =>
        have h' := step_request_erasure P c
        unfold StepRequestErasure at h'
        rw [hs] at h'
        obtain ⟨_, h', _⟩ := h'
        rw [herased] at h'
        cases h'

/-- Law (subject `ConcreteSpec.Core.Own.liftOracle`). Exact fuel-indexed simulation for checker-accepted programs (owner decision 2026-11-08 Q3):
when the program and initial configuration satisfy `CheckerAccepted`, the erased run equals the erased report of the
instrumented run for every outcome, including stuck. -/
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
        have hnone : Exec.step (eraseProgram P) (eraseConfig c) = none :=
          (step_stuck_iff_of_accepted P c hacc).mp hs
        rw [hnone]
        rfl
      | some out =>
        cases out with
        | next c' =>
          have hnext := step_next_erasure P c
          unfold StepNextErasure at hnext
          rw [show stepInstrumented O types permit P c = some (.next c') from hs] at hnext
          rw [hnext]
          exact runFrom_erasure_of_accepted P ρ n hist c'
            (checkerAccepted_step_inv P c c' .silent hacc (.silent hs))
        | request p vs site st k =>
          have hreq := step_request_erasure P c
          unfold StepRequestErasure at hreq
          rw [show stepInstrumented O types permit P c = some (.request p vs site st k) from hs] at hreq
          obtain ⟨k_erased, hs_erased, hk_erased⟩ := hreq
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
                have hreq_none : ∃ k_e2,
                    Exec.step (eraseProgram P) (eraseConfig c) =
                      some (.request p vs site (erase st) k_e2) ∧
                    k_e2 (eraseAnswer (liftAnswer st.own a₀)) = none := by
                  clear hacc hf hs_erased hk_erased ha_eq hρ ha ha_inst
                  generalize liftAnswer st.own a₀ = a at ha' hk ⊢
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
                      obtain ⟨_, hpb⟩ := hperm
                      dsimp [Exec.step, eraseConfig, eraseCtrl, eraseStmt] at hs ⊢
                      cases he : Exec.evalArgs (instrumentOperands O types permit) L s args with
                      | ok r =>
                        rcases r with ⟨vs', st'⟩
                        have he_erased : Exec.evalArgs O L (erase s) args = .ok (vs', erase st') := by
                          have h0 := evalArgs_ok_erasure (O := O) (types := types) (permit := permit) L args s
                          rw [he] at h0
                          exact h0
                        rw [he] at hs
                        dsimp [Exec.onRes] at hs
                        injection hs with hs
                        injection hs with hp_eq hvs hsite hst hk_eq
                        subst hp_eq; subst hvs; subst hsite; subst hst; subst hk_eq
                        refine ⟨_, by rw [he_erased]; rfl, ?_⟩
                        dsimp at hk ⊢
                        cases ha : a.result with
                        | ok v =>
                          rw [ha] at hk
                          simp only [Option.map_eq_none_iff] at hk
                          have hok : a.result.isOk = true := by rw [ha]; rfl
                          have hbind := hpb vs' st' a he ha' hok
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
                    cases frames with
                    | nil => cases hs
                    | cons f k' =>
                      cases f with
                      | seq _ => cases hs
                      | scope b dm ds =>
                        cases ds with
                        | nil =>
                          simp only [Exec.stepExit, Option.map_eq_some_iff] at hs
                          obtain ⟨_, _, hs⟩ := hs; cases hs
                        | cons _ _ => cases hs
                      | test _ _ _ _ _ _ =>
                        simp only [Exec.stepExit, Option.map_eq_some_iff] at hs
                        obtain ⟨_, _, hs⟩ := hs; cases hs
                      | body l _ _ _ _ =>
                        cases e with
                        | brk l' => by_cases hl : l' = l <;> simp [Exec.stepExit, hl] at hs
                        | cont l' => by_cases hl : l' = l <;> simp [Exec.stepExit, hl] at hs
                        | ret _ => cases hs
                      | resume _ => cases hs
                      | ret _ _ =>
                        cases e with
                        | brk _ => cases hs
                        | cont _ => cases hs
                        | ret _ =>
                          simp only [Exec.stepExit, Option.bind_eq_some_iff, Option.map_eq_some_iff] at hs
                          obtain ⟨_, _, _, _, hs⟩ := hs; cases hs
                  | trapped _ _ => cases hs
                obtain ⟨k_e2, hs_e2, hk_none⟩ := hreq_none
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

/-- Helper (projection). `Ending.IsStuck` is invariant under `eraseEnding`. (approved: APPR-C5-codex-902ce88a). -/
private theorem isStuck_eraseEnding_proj (e : Exec.Ending (instrumentOperands O types permit)) :
    Ending.IsStuck (eraseEnding e) = Ending.IsStuck e := by
  cases e <;> rfl

/-- Law (subject `ConcreteSpec.Core.Own.liftOracle`). Converse stuck equivalence for checker-accepted programs (owner decision 2026-11-08 Q3):
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

instance (priority := 10000) decidableBAllSafe {α : Type} (P : α → Prop) [∀ a, Decidable (P a)] :
    (l : List α) → Decidable (∀ x ∈ l, P x)
  | [] => isTrue (fun x h =>
      @List.Mem.rec α x
        (fun l _ => match l with | [] => P x | _ :: _ => True)
        (fun _ => trivial)
        (fun _ _ _ _ => trivial)
        [] h)
  | a :: as =>
    match inferInstanceAs (Decidable (P a)), decidableBAllSafe P as with
    | isTrue ha, isTrue has => isTrue (fun x h =>
        (@List.Mem.rec α x
          (fun l _ =>
            match l with
            | [] => True
            | head :: tail => (head = x → P x) → (∀ y ∈ tail, P y) → P x)
          (fun _ hhead _ => hhead rfl)
          (fun _ _ hmem _ _ htail => htail x hmem)
          (a :: as) h)
          (fun heq => heq ▸ ha)
          has)
    | isFalse hna, _ => isFalse (fun h => hna (h a (.head as)))
    | _, isFalse hnas => isFalse (fun h => hnas (fun x hx => h x (.tail a hx)))

instance (priority := 10000) decidableBExSafe {α : Type} (P : α → Prop) [∀ a, Decidable (P a)] :
    (l : List α) → Decidable (∃ x, x ∈ l ∧ P x)
  | [] => isFalse (fun ⟨x, hx, _⟩ =>
      @List.Mem.rec α x
        (fun l _ => match l with | [] => False | _ :: _ => True)
        (fun _ => trivial)
        (fun _ _ _ _ => trivial)
        [] hx)
  | a :: as =>
    match inferInstanceAs (Decidable (P a)) with
    | isTrue ha => isTrue ⟨a, .head as, ha⟩
    | isFalse hna =>
      match decidableBExSafe P as with
      | isTrue has => isTrue (match has with | ⟨x, hx, hpx⟩ => ⟨x, .tail a hx, hpx⟩)
      | isFalse hnas => isFalse (fun ⟨x, hx, hpx⟩ =>
          (@List.Mem.rec α x
            (fun l _ =>
              match l with
              | [] => True
              | head :: tail => (head = x → P x → False) → ((∃ y, y ∈ tail ∧ P y) → False) → False)
            (fun _ hhead _ => hhead rfl hpx)
            (fun _ _ hmem _ _ htail => htail ⟨x, hmem, hpx⟩)
            (a :: as) hx)
            (fun heq hp => hna (heq.symm ▸ hp))
            hnas)

def decidableMemSafe {α : Type} [DecidableEq α] (x : α) :
    (l : List α) → Decidable (x ∈ l)
  | [] => isFalse (fun h =>
      @List.Mem.rec α x
        (fun l _ => match l with | [] => False | _ :: _ => True)
        (fun _ => trivial)
        (fun _ _ _ _ => trivial)
        [] h)
  | a :: as =>
    if h : x = a then
      isTrue (h ▸ .head as)
    else
      match decidableMemSafe x as with
      | isTrue hmem => isTrue (.tail a hmem)
      | isFalse hnmem => isFalse (fun hm =>
          (@List.Mem.rec α x
            (fun l _ =>
              match l with
              | [] => True
              | head :: tail => (x = head → False) → (x ∈ tail → False) → False)
            (fun _ hhead _ => hhead rfl)
            (fun _ _ htail _ _ hntail => hntail htail)
            (a :: as) hm)
            h hnmem)

def decidableIsPrefixSafe {α : Type} [DecidableEq α] :
    (l₁ l₂ : List α) → Decidable (l₁ <+: l₂)
  | [], l₂ => isTrue ⟨l₂, rfl⟩
  | a :: as, [] => isFalse (fun ⟨t, h⟩ =>
      @Eq.rec (List α) ((a :: as) ++ t)
        (fun l _ => match l with | [] => False | _ :: _ => True)
        trivial [] h)
  | a :: as, b :: bs =>
    if h : a = b then
      match decidableIsPrefixSafe as bs with
      | isTrue hp => isTrue (match hp with | ⟨t, ht⟩ => ⟨t, by rw [List.cons_append, h, ht]⟩)
      | isFalse hnp => isFalse (fun ⟨t, ht⟩ =>
          hnp ⟨t, @Eq.rec (List α) ((a :: as) ++ t)
            (fun l _ => match l with | [] => True | _ :: tail => as ++ t = tail)
            rfl (b :: bs) ht⟩)
    else
      isFalse (fun ⟨t, ht⟩ =>
        h (@Eq.rec (List α) ((a :: as) ++ t)
          (fun l _ => match l with | [] => True | head :: _ => a = head)
          rfl (b :: bs) ht))

instance (priority := 10000) decidablePairwiseSafe {α : Type} (R : α → α → Prop)
    [∀ a b, Decidable (R a b)] : (l : List α) → Decidable (List.Pairwise R l)
  | [] => isTrue .nil
  | a :: as =>
    match decidableBAllSafe (R a) as, decidablePairwiseSafe R as with
    | isTrue ha, isTrue has => isTrue (.cons ha has)
    | isFalse hna, _ => isFalse (fun h => by cases h with | cons ha _ => exact hna ha)
    | _, isFalse hnas => isFalse (fun h => by cases h with | cons _ has => exact hnas has)

instance (priority := 10000) decidableNodupSafe {α : Type} [DecidableEq α] (l : List α) :
    Decidable (List.Nodup l) :=
  decidablePairwiseSafe (· ≠ ·) l

section SafeDecidable
local instance (priority := 10000) {α : Type} [DecidableEq α] (x : α) (l : List α) :
    Decidable (x ∈ l) := decidableMemSafe x l
local instance (priority := 10000) {α : Type} [DecidableEq α] (l₁ l₂ : List α) :
    Decidable (l₁ <+: l₂) := decidableIsPrefixSafe l₁ l₂
instance (priority := 10000) (a b : Loan) : Decidable (Compatible a b) := by
  unfold Compatible; infer_instance
instance (priority := 10000) (s : State) (l : Loan) : Decidable (ParentValid s l) := by
  unfold ParentValid; cases l.parent <;> infer_instance
instance (priority := 10000) (s : State) : Decidable (WellFormed s) := by
  unfold WellFormed; infer_instance
instance (priority := 10000) (s : State) : Decidable (LiveDestinations s) := by
  unfold LiveDestinations; infer_instance
instance (priority := 10000) (s : State) (i : LoanId) : Decidable (RetiredLoan s i) := by
  unfold RetiredLoan; infer_instance
instance (priority := 10000) (s : CallState) : Decidable (CallState.WellFormed s) := by
  unfold CallState.WellFormed; infer_instance
end SafeDecidable

namespace ExecErasureWitnesses
open ConcreteSpec.Core.Own.Examples

def emptyProg : Exec.Program instrumentedOps := ⟨[]⟩

def bindMoveConfig : Exec.Config instrumentedOps :=
  ⟨.exec (.bind y (.move x) ⟨0⟩), [(x, 7)], ⟨0, initial⟩, []⟩

def primOneConfig : Exec.Config instrumentedOps :=
  ⟨.exec (.prim (some y) () [.read x] ⟨0⟩), [(x, 7)], ⟨0, initial⟩, []⟩

private def optionNoneMotive {α : Type} (Q : Prop) : Option α → Prop
  | none => Q
  | some _ => True

private def optionSomeMotive {α : Type} (P : α → Prop) : Option α → Prop
  | some y => P y
  | none => True

private def optionNextMotive {O : Exec.Operands} (P : Exec.Config O → Prop) : Option (Exec.Out O) → Prop
  | some (.next d) => P d
  | _ => True

private def optionRequestMotive {O : Exec.Operands}
    (P : O.Prim → List O.Val → SiteId → O.Store →
      (Exec.Answer O.Val O.Event O.Store → Option (Exec.Config O)) → Prop) :
    Option (Exec.Out O) → Prop
  | some (.request p vs site st k) => P p vs site st k
  | _ => True

private def boolTrueMotive (Q : Prop) : Bool → Prop
  | true => Q
  | false => True

/-- Helper (inversion). A step from a configuration whose `Exec.step` returns a next configuration must
land in that target configuration. (approved: APPR-C5-codex-902ce88a). -/
private theorem step_det_next_inv {O : Exec.Operands} {P : Exec.Program O}
    {c c₁ c' : Exec.Config O} {l : Exec.Label O}
    (hs : Exec.step P c = some (.next c₁)) (hstep : Exec.Step P c l c') :
    c' = c₁ := by
  cases hstep with
  | silent h =>
    exact @Eq.rec (Option (Exec.Out O)) (Exec.step P c)
      (fun o _ => optionNextMotive (fun d' => c' = d') o)
      (@Eq.rec (Option (Exec.Out O)) (some (.next c'))
        (fun o _ => optionNextMotive (fun d' => c' = d') o)
        rfl (Exec.step P c) h.symm)
      (some (.next c₁)) hs
  | @prim _ p vs site st k _ h _ _ =>
    exact @Eq.rec (Option (Exec.Out O)) (Exec.step P c)
      (fun o _ => optionNextMotive (fun _ => c' = c₁) o)
      (@Eq.rec (Option (Exec.Out O)) (some (.request p vs site st k))
        (fun o _ => optionNextMotive (fun _ => c' = c₁) o)
        trivial (Exec.step P c) h.symm)
      (some (.next c₁)) hs

/-- Helper (inversion). A configuration whose `Exec.step` is `none` cannot take an `Exec.Step`. (approved: APPR-C5-codex-902ce88a). -/
private theorem step_det_none_inv {O : Exec.Operands} {P : Exec.Program O}
    {c c' : Exec.Config O} {l : Exec.Label O} {Q : Prop}
    (hs : Exec.step P c = none) (hstep : Exec.Step P c l c') : Q := by
  cases hstep with
  | silent h =>
    exact @Eq.rec (Option (Exec.Out O)) (Exec.step P c)
      (fun o _ => optionNoneMotive Q o)
      (@Eq.rec (Option (Exec.Out O)) (some (.next c'))
        (fun o _ => optionNoneMotive Q o)
        trivial (Exec.step P c) h.symm)
      none hs
  | @prim _ p vs site st k _ h _ _ =>
    exact @Eq.rec (Option (Exec.Out O)) (Exec.step P c)
      (fun o _ => optionNoneMotive Q o)
      (@Eq.rec (Option (Exec.Out O)) (some (.request p vs site st k))
        (fun o _ => optionNoneMotive Q o)
        trivial (Exec.step P c) h.symm)
      none hs

/-- Helper (inversion). The moved-owner program `moveConfig` is rejected by `CheckerAccepted`
because after moving `x` to `y` it reaches a read of `x` where concrete evaluation succeeds
while `permitE` refuses. (approved: APPR-C5-codex-902ce88a). -/
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
satisfies `CheckerAccepted`. (approved: APPR-C5-codex-902ce88a). -/
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
      have hs' : ⟨0, (move initial ⟨0⟩).getD initial⟩ = s' :=
        congrArg (fun (r : Res (Int × Store Int)) =>
          match r with | .ok (_, s) => s | _ => ⟨0, initial⟩) he
      rw [← hs']
      intro _
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
          | step hs₅ _ => exact step_det_none_inv rfl hs₅

/-- Helper (inversion). The unbound-assignment configuration `acceptedStuckConfig`
satisfies `CheckerAccepted` and gets stuck in both the instrumented and erased machines. (approved: APPR-C5-codex-902ce88a). -/
private theorem acceptedStuck_checkerAccepted_inv :
    CheckerAccepted erasedOps types permitE emptyProg acceptedStuckConfig := by
  intro tr c hsteps
  cases hsteps with
  | refl =>
    exact ⟨by decide, ⟨fun _ => by decide, fun _ _ _ hlook => by cases hlook⟩⟩
  | step hs _ => exact step_det_none_inv rfl hs

/-- Observation. Moving an initialized owner in the instrumented operand instance erases to the concrete value and store. -/
example : ∃ s' : Store Int,
    instrumentedOps.eval [(x, 7)] ⟨0, initial⟩ (.move x) = .ok (7, s') ∧
    erasedOps.eval [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) (.move x) = .ok (7, erase s') := by
  refine ⟨⟨0, (move initial ⟨0⟩).getD initial⟩, by decide, ?_⟩
  exact eval_ok_erasure_result (O := erasedOps) (types := types) (permit := permitE)
    [(x, 7)] ⟨0, initial⟩ _ (.move x) 7 (by decide)

/-- Rejection observation. A corrupted concrete value disagrees with the erased evaluation of `.move x`. -/
example : ¬ (erasedOps.eval [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) (.move x) = .ok (8, 0)) := by
  decide

/-- Observation. A trapping operand in the instrumented instance traps with the same kind in the erased instance. -/
example : instrumentedOps.eval [(x, 7)] ⟨0, initial⟩ (.fail .abort) = .trap .abort ∧
    erasedOps.eval [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) (.fail .abort) = .trap .abort :=
  ⟨by decide, eval_trap_erasure_result (O := erasedOps) (types := types) (permit := permitE)
    [(x, 7)] ⟨0, initial⟩ (.fail .abort) .abort (by decide)⟩

/-- Rejection observation. A trapping operand does not erase to a normal value. -/
example : ¬ (erasedOps.eval [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) (.fail .abort) = .ok (0, 0)) := by
  decide

/-- Observation. Left-to-right instrumented argument evaluation erases to the concrete argument values and store. -/
example : ∃ s' : Store Int,
    Exec.evalArgs instrumentedOps [(x, 7)] ⟨0, initial⟩ [.read x, .move x] = .ok ([7, 7], s') ∧
    Exec.evalArgs erasedOps [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) [.read x, .move x] =
      .ok ([7, 7], erase s') :=
  ⟨⟨0, (move initial ⟨0⟩).getD initial⟩, by decide, by decide⟩

/-- Rejection observation. Reversing or corrupting the argument list violates erased argument evaluation. -/
example : ¬ (Exec.evalArgs erasedOps [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) [.read x, .move x] =
    .ok ([7, 0], 0)) := by
  decide

/-- Observation. An argument list that traps after a move traps with the same kind in the erased instance. -/
example : Exec.evalArgs instrumentedOps [(x, 7)] ⟨0, initial⟩ [.move x, .fail .overflow] = .trap .overflow ∧
    Exec.evalArgs erasedOps [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) [.move x, .fail .overflow] =
      .trap .overflow :=
  ⟨by decide, by decide⟩

/-- Rejection observation. A trapping argument list cannot erase to a different trap kind. -/
example : ¬ (Exec.evalArgs erasedOps [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) [.move x, .fail .overflow] =
    .trap .abort) := by
  decide

/-- Observation. Condition testing agrees between the instrumented and erased instances on both truth values. -/
example : instrumentedOps.test 7 = erasedOps.test 7 ∧
    instrumentedOps.test 0 = erasedOps.test 0 ∧
    erasedOps.test 7 = some true ∧ erasedOps.test 0 = some false :=
  ⟨test_erasure_result (O := erasedOps) (types := types) (permit := permitE) 7,
   test_erasure_result (O := erasedOps) (types := types) (permit := permitE) 0,
   by decide, by decide⟩

/-- Rejection observation. A nonzero condition value does not test as `false`. -/
example : ¬ (erasedOps.test 7 = some false) := by decide

/-- Observation. Binding a fresh variable updates the ownership frame and erases to the concrete lifecycle step. -/
example : ∃ s' : Store Int,
    instrumentedOps.life (.bind y) ⟨7, initial⟩ = some s' ∧
    erasedOps.life (.bind y) (erase (⟨7, initial⟩ : Store Int)) = some (erase s') := by
  refine ⟨⟨7, (bind types y initial).getD initial⟩, by decide, ?_⟩
  exact instrumentLife_erasure_result (O := erasedOps) (types := types) (permit := permitE)
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
  exact ⟨by decide, allows_erasure_result (O := erasedOps) (types := types) (permit := permitE)
    () [7] ⟨0, initial⟩ ⟨some 7, .ok 7, ⟨7, initial⟩⟩ (by decide)⟩

/-- Rejection observation. A primitive answer that mutates the ownership state or emits the wrong event is rejected by `instrumentAllows`. -/
example : ¬ (instrumentedOps.allows () [7] ⟨0, initial⟩
    ⟨some 7, .ok 7, ⟨7, (move initial ⟨0⟩).getD initial⟩⟩ = true) := by
  decide

/-- Observation. A silent `.bind` step on the instrumented machine erases to the matching `.bind` step on the erased machine. -/
example : ∃ c' : Exec.Config instrumentedOps,
    Exec.step emptyProg bindMoveConfig = some (.next c') ∧
    Exec.step (eraseProgram emptyProg) (eraseConfig bindMoveConfig) =
      some (.next (eraseConfig c')) :=
  ⟨_, rfl, @step_next_erasure erasedOps types permitE emptyProg bindMoveConfig⟩

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
      ∀ a c', k a = some c' → k_erased (eraseAnswer a) = some (eraseConfig c') :=
  ⟨(), [7], ⟨0⟩, ⟨0, initial⟩, _, rfl,
    @step_request_erasure erasedOps types permitE emptyProg primOneConfig⟩

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
  exact ⟨_, hs, @Step_erasure erasedOps types permitE emptyProg primOneConfig
    (liftAnswer initial ⟨some 7, .ok 7, 7⟩) _ (by decide) rfl⟩

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
  have hfin : FiniteExecution (stepInstrumented erasedOps types permitE emptyProg) primTraceConfig _ _ :=
    .silent rfl
      (.prim (a := liftAnswer initial ⟨some 7, .ok 7, 7⟩) rfl rfl rfl
        (.silent rfl
          (.silent rfl
            (.prim (a := liftAnswer ((bind types y initial).getD initial) ⟨some 3, .ok 10, 10⟩)
                rfl rfl rfl
              (.silent rfl
                (.silent rfl (.refl _)))))))
  exact ⟨_, hsteps, @Steps_erasure erasedOps types permitE emptyProg primTraceConfig [7, 3] _ hfin, rfl, rfl⟩

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
(approved: APPR-C5-codex-902ce88a). -/
theorem unconditionalRunConverse_refuted : ¬ UnconditionalRunConverse := by
  intro h
  have he := congrArg (fun r => Ending.IsStuck r.ending) (h 100 moveConfig)
  revert he
  decide

/-- Mutant: instrumentation replaces every operand value with `O.unitVal`. -/
abbrev zeroOperands (O : Exec.Operands) (types : Var → Option Descriptor)
    (permit : AList Var O.Val → O.Expr → State → Option State) : Exec.Operands :=
  { instrumentOperands O types permit with eval := fun _ s _ => .ok (O.unitVal, s) }

/-- Mutant: instrumentation replaces every operand evaluation with `.trap .abort`. -/
abbrev abortOperands (O : Exec.Operands) (types : Var → Option Descriptor)
    (permit : AList Var O.Val → O.Expr → State → Option State) : Exec.Operands :=
  { instrumentOperands O types permit with eval := fun _ _ _ => .trap .abort }

/-- Mutant: instrumentation makes every operand evaluation `.stuck`. -/
abbrev stuckOperands (O : Exec.Operands) (types : Var → Option Descriptor)
    (permit : AList Var O.Val → O.Expr → State → Option State) : Exec.Operands :=
  { instrumentOperands O types permit with eval := fun _ _ _ => .stuck }

/-- Mutant: instrumentation inverts condition truth values. -/
abbrev invertTestOperands (O : Exec.Operands) (types : Var → Option Descriptor)
    (permit : AList Var O.Val → O.Expr → State → Option State) : Exec.Operands :=
  { instrumentOperands O types permit with test := fun v => (O.test v).map Bool.not }

/-- Mutant: instrumentation skips the ownership lifecycle transition while running the concrete one. -/
abbrev skipLifeOperands (O : Exec.Operands) (types : Var → Option Descriptor)
    (permit : AList Var O.Val → O.Expr → State → Option State) : Exec.Operands :=
  { instrumentOperands O types permit with
    life := fun ev s => (O.life ev s.concrete).map fun concrete => ⟨concrete, s.own⟩ }

/-- Mutant: instrumentation rejects every primitive answer regardless of the contract check. -/
abbrev refuseAllowsOperands (O : Exec.Operands) (types : Var → Option Descriptor)
    (permit : AList Var O.Val → O.Expr → State → Option State) : Exec.Operands :=
  { instrumentOperands O types permit with allows := fun _ _ _ _ => false }

/-- Mutant: oracle lifting corrupts the ownership frame of the lifted answer (`nextLoan + 1`),
causing the instrumented machine to reject the answer with `.badAnswer` (which satisfies
`Ending.NotStuck`!) while the erased machine accepts it and emits `[7, 3]`. -/
def liftOracleMutateOwn (O : Exec.Operands) (types : Var → Option Descriptor)
    (permit : AList Var O.Val → O.Expr → State → Option State)
    (ρ : Exec.Oracle O) : Exec.Oracle (instrumentOperands O types permit) :=
  fun hist p vs site s =>
    (ρ hist p vs site s.concrete).map fun a =>
      liftAnswer { s.own with nextLoan := s.own.nextLoan + 1 } a

/-- Mutant: single-step instrumented execution zeroes local operand values before stepping. -/
def stepInstrumentedZeroVal (O : Exec.Operands) (types : Var → Option Descriptor)
    (permit : AList Var O.Val → O.Expr → State → Option State)
    (P : Exec.Program (instrumentOperands O types permit))
    (c : Exec.Config (instrumentOperands O types permit)) :
    Option (Exec.Out (instrumentOperands O types permit)) :=
  Exec.step P { c with locals := c.locals.map (fun _ => O.unitVal) }

/-- Mutant: single-step instrumented execution spuriously refuses every step. -/
def stepInstrumentedRefuse (O : Exec.Operands) (types : Var → Option Descriptor)
    (permit : AList Var O.Val → O.Expr → State → Option State)
    (_P : Exec.Program (instrumentOperands O types permit))
    (_c : Exec.Config (instrumentOperands O types permit)) :
    Option (Exec.Out (instrumentOperands O types permit)) :=
  none

/-- Mutant: oracle lifting drops the concrete oracle's answer and returns `none`. -/
def liftOracleDropAnswer (O : Exec.Operands) (types : Var → Option Descriptor)
    (permit : AList Var O.Val → O.Expr → State → Option State)
    (_ρ : Exec.Oracle O) : Exec.Oracle (instrumentOperands O types permit) :=
  fun _ _ _ _ _ => none

def primFrameStuckConfig : Exec.Config instrumentedOps :=
  ⟨.exec (.prim none () [.lit 0] ⟨0⟩), [(x, 7)], ⟨0, initial⟩,
    [.seq (.assign y (.lit 1) ⟨1⟩)]⟩

/-- Helper (inversion). `primFrameStuckConfig` satisfies `CheckerAccepted`. (approved: APPR-C5-codex-902ce88a). -/
private theorem primFrameStuck_checkerAccepted_inv :
    CheckerAccepted erasedOps types permitE emptyProg primFrameStuckConfig := by
  have optionSomeInj : ∀ {α : Type} {x : Option α} {a b : α},
      x = some a → x = some b → a = b :=
    fun {α} {x} {a} {b} ha hb =>
      @Eq.rec (Option α) x
        (fun o _ => optionSomeMotive (fun b' => a = b') o)
        (@Eq.rec (Option α) (some a)
          (fun o _ => optionSomeMotive (fun b' => a = b') o)
          rfl x ha.symm)
        (some b) hb
  have optionNextRequestElim : ∀ {O : Exec.Operands} {Q : Prop}
      {x : Option (Exec.Out O)} {d : Exec.Config O} {p : O.Prim} {vs : List O.Val}
      {site : SiteId} {st : O.Store}
      {k : Exec.Answer O.Val O.Event O.Store → Option (Exec.Config O)},
      x = some (.next d) → x = some (.request p vs site st k) → Q :=
    fun {O} {Q} {x} {d} {p} {vs} {site} {st} {k} hnext hreq =>
      @Eq.rec (Option (Exec.Out O)) x
        (fun o _ => optionNextMotive (fun _ => Q) o)
        (@Eq.rec (Option (Exec.Out O)) (some (.request p vs site st k))
          (fun o _ => optionNextMotive (fun _ => Q) o)
          trivial x hreq.symm)
        (some (.next d)) hnext
  have optionRequestElim : ∀ {O : Exec.Operands} {Q : Prop}
      {x : Option (Exec.Out O)}
      {p₀ p : O.Prim} {vs₀ vs : List O.Val} {site₀ site : SiteId} {st₀ st : O.Store}
      {k₀ k : Exec.Answer O.Val O.Event O.Store → Option (Exec.Config O)},
      x = some (.request p₀ vs₀ site₀ st₀ k₀) →
      x = some (.request p vs site st k) →
      (p = p₀ → vs = vs₀ → site = site₀ → st = st₀ → k = k₀ → Q) → Q :=
    fun {O} {Q} {x} {p₀} {p} {vs₀} {vs} {site₀} {site} {st₀} {st} {k₀} {k} h₀ h hq =>
      (@Eq.rec (Option (Exec.Out O)) x
        (fun o _ => optionRequestMotive (fun p' vs' site' st' k' =>
          (p' = p₀ → vs' = vs₀ → site' = site₀ → st' = st₀ → k' = k₀ → Q) → Q) o)
        (@Eq.rec (Option (Exec.Out O)) (some (.request p₀ vs₀ site₀ st₀ k₀))
          (fun o _ => optionRequestMotive (fun p' vs' site' st' k' =>
            (p' = p₀ → vs' = vs₀ → site' = site₀ → st' = st₀ → k' = k₀ → Q) → Q) o)
          (fun hq' => hq' rfl rfl rfl rfl rfl) x h₀.symm)
        (some (.request p vs site st k)) h) hq
  have bool_and_right : ∀ {b1 b2 : Bool}, (b1 && b2) = true → b2 = true :=
    fun {b1} {b2} h =>
      match b1, h with
      | true, h => h
      | false, h =>
        @Eq.rec Bool (false && b2)
          (fun b _ => boolTrueMotive (b2 = true) b)
          trivial true h
  have of_decide_eq_true_safe : ∀ {P : Prop} [d : Decidable P], @decide P d = true → P :=
    fun {P} [d : Decidable P] h =>
      @Decidable.rec P
        (fun d' => @decide P d' = true → P)
        (fun _ (h' : false = true) =>
          @Eq.rec Bool false
            (fun b _ => boolTrueMotive P b)
            trivial true h')
        (fun hp _ => hp)
        d h
  intro tr c hsteps
  cases hsteps with
  | refl =>
    exact ⟨by decide, ⟨⟨fun _ => rfl, fun _ _ _ => trivial⟩, fun _ _ _ _ _ _ => trivial⟩⟩
  | step hs₁ hsteps =>
    cases hs₁ with
    | silent h => exact optionNextRequestElim h rfl
    | @prim c₁ p vs site st k a hreq hallows hk =>
      refine optionRequestElim rfl hreq ?_
      rintro rfl rfl rfl rfl rfl
      have hown : a.store.own = initial :=
        of_decide_eq_true_safe (bool_and_right hallows)
      dsimp at hk
      cases ha : a.result with
      | error kd =>
        rw [ha] at hk
        have hc₁ := optionSomeInj rfl hk
        subst hc₁
        cases hsteps with
        | refl =>
          dsimp [StepPermitted]
          rw [hown]
          exact ⟨by decide, trivial⟩
        | step hs₂ _ => exact step_det_none_inv rfl hs₂
      | ok v =>
        rw [ha] at hk
        dsimp [Exec.bindLife] at hk
        have hc₁ := optionSomeInj rfl hk
        subst hc₁
        cases hsteps with
        | refl =>
          dsimp [StepPermitted, DoneFramePermitted]
          rw [hown]
          exact ⟨by decide, trivial⟩
        | step hs₂ hsteps =>
          obtain rfl := step_det_next_inv rfl hs₂
          cases hsteps with
          | refl =>
            dsimp [StepPermitted, StmtPermitted]
            rw [hown]
            exact ⟨by decide, ⟨fun _ => rfl, fun _ _ _ hlook => by cases hlook⟩⟩
          | step hs₃ _ => exact step_det_none_inv rfl hs₃

/-- Computation observation. -/
example : (match (instrumentOperands erasedOps types permitE).eval [(x, 7)] ⟨0, initial⟩ (.move x) with
    | .ok (v, t) => erasedOps.eval [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) (.move x) = .ok (v, erase t)
    | _ => True) :=
  @eval_ok_erasure erasedOps types permitE [(x, 7)] ⟨0, initial⟩ (.move x)

/-- Computation observation. -/
example : ¬ (match (zeroOperands erasedOps types permitE).eval [(x, 7)] ⟨0, initial⟩ (.move x) with
    | .ok (v, t) => erasedOps.eval [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) (.move x) = .ok (v, erase t)
    | _ => True) := by
  change ¬ ((Res.ok (7, 0) : Res (Int × Int)) = .ok (0, 0))
  decide

/-- Computation observation. -/
example : (match (instrumentOperands erasedOps types permitE).eval [(x, 7)] ⟨0, initial⟩ (.fail .overflow) with
    | .trap kd => erasedOps.eval [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) (.fail .overflow) = .trap kd
    | _ => True) :=
  @eval_trap_erasure erasedOps types permitE [(x, 7)] ⟨0, initial⟩ (.fail .overflow)

/-- Computation observation. -/
example : ¬ (match (abortOperands erasedOps types permitE).eval [(x, 7)] ⟨0, initial⟩ (.fail .overflow) with
    | .trap kd => erasedOps.eval [(x, 7)] (erase (⟨0, initial⟩ : Store Int)) (.fail .overflow) = .trap kd
    | _ => True) := by
  change ¬ ((Res.trap .overflow : Res (Int × Int)) = .trap .abort)
  decide

/-- Computation observation. -/
example : (match (instrumentOperands erasedOps types permitE).test 7 with
    | some b => erasedOps.test 7 = some b
    | none => erasedOps.test 7 = none) :=
  @test_erasure erasedOps types permitE 7

/-- Computation observation. -/
example : ¬ (match (invertTestOperands erasedOps types permitE).test 7 with
    | some b => erasedOps.test 7 = some b
    | none => erasedOps.test 7 = none) := by
  change ¬ (some true = some false)
  decide

/-- Computation observation. -/
example : (match (instrumentOperands erasedOps types permitE).life (.bind y) ⟨7, initial⟩ with
    | some t => erasedOps.life (.bind y) (erase (⟨7, initial⟩ : Store Int)) = some (erase t) ∧
        life types (.bind y) initial = some t.own
    | none => True) :=
  @instrumentLife_erasure erasedOps types permitE (.bind y) ⟨7, initial⟩

/-- Computation observation. -/
example : ¬ (match (skipLifeOperands erasedOps types permitE).life (.bind y) ⟨7, initial⟩ with
    | some t => erasedOps.life (.bind y) (erase (⟨7, initial⟩ : Store Int)) = some (erase t) ∧
        life types (.bind y) initial = some t.own
    | none => True) := by
  change ¬ (some 7 = some 7 ∧ life types (.bind y) initial = some initial)
  decide

/-- Computation observation. -/
example : ((instrumentOperands erasedOps types permitE).allows () [7] ⟨0, initial⟩
      ⟨some 7, .ok 7, ⟨7, initial⟩⟩ = true ↔
    erasedOps.allows () [7] (erase (⟨0, initial⟩ : Store Int))
      (eraseAnswer ⟨some 7, .ok 7, ⟨7, initial⟩⟩) = true ∧ initial = initial) :=
  @allows_erasure erasedOps types permitE () [7] ⟨0, initial⟩ ⟨some 7, .ok 7, ⟨7, initial⟩⟩

/-- Computation observation. -/
example : ¬ ((refuseAllowsOperands erasedOps types permitE).allows () [7] ⟨0, initial⟩
      ⟨some 7, .ok 7, ⟨7, initial⟩⟩ = true ↔
    erasedOps.allows () [7] (erase (⟨0, initial⟩ : Store Int))
      (eraseAnswer ⟨some 7, .ok 7, ⟨7, initial⟩⟩) = true ∧ initial = initial) := by
  decide

/-- Instance (law `ConcreteSpec.Core.Own.step_next_erasure`, case `ConcreteSpec.Core.Own.ExecErasureWitnesses.bindMoveConfig`). -/
example : StepNextErasure erasedOps types permitE
    (stepInstrumented erasedOps types permitE emptyProg) emptyProg bindMoveConfig :=
  @step_next_erasure erasedOps types permitE emptyProg bindMoveConfig

/-- Planted (law `ConcreteSpec.Core.Own.step_next_erasure`, case `ConcreteSpec.Core.Own.ExecErasureWitnesses.bindMoveConfig`, subject `ConcreteSpec.Core.Own.stepInstrumented`, mutant `ConcreteSpec.Core.Own.ExecErasureWitnesses.stepInstrumentedZeroVal`). -/
example : ¬ StepNextErasure erasedOps types permitE
    (stepInstrumentedZeroVal erasedOps types permitE emptyProg) emptyProg bindMoveConfig := by
  intro h
  cases h

/-- Instance (law `ConcreteSpec.Core.Own.step_request_erasure`, case `ConcreteSpec.Core.Own.ExecErasureWitnesses.primOneConfig`). -/
example : StepRequestErasure erasedOps types permitE
    (stepInstrumented erasedOps types permitE emptyProg) emptyProg primOneConfig :=
  @step_request_erasure erasedOps types permitE emptyProg primOneConfig

/-- Planted (law `ConcreteSpec.Core.Own.step_request_erasure`, case `ConcreteSpec.Core.Own.ExecErasureWitnesses.primOneConfig`, subject `ConcreteSpec.Core.Own.stepInstrumented`, mutant `ConcreteSpec.Core.Own.ExecErasureWitnesses.stepInstrumentedZeroVal`). -/
example : ¬ StepRequestErasure erasedOps types permitE
    (stepInstrumentedZeroVal erasedOps types permitE emptyProg) emptyProg primOneConfig := by
  rintro ⟨_, h, _⟩
  cases h

/-- Instance (law `ConcreteSpec.Core.Own.Step_erasure`, case `ConcreteSpec.Core.Own.ExecErasureWitnesses.bindMoveConfig`). -/
example : StepErasure erasedOps types permitE
    (stepInstrumented erasedOps types permitE emptyProg) emptyProg bindMoveConfig :=
  @Step_erasure erasedOps types permitE emptyProg bindMoveConfig

/-- Planted (law `ConcreteSpec.Core.Own.Step_erasure`, case `ConcreteSpec.Core.Own.ExecErasureWitnesses.bindMoveConfig`, subject `ConcreteSpec.Core.Own.stepInstrumented`, mutant `ConcreteSpec.Core.Own.ExecErasureWitnesses.stepInstrumentedZeroVal`). -/
example : ¬ StepErasure erasedOps types permitE
    (stepInstrumentedZeroVal erasedOps types permitE emptyProg) emptyProg bindMoveConfig := by
  intro h
  have heq := step_det_next_inv rfl h
  cases heq

/-- Endpoint of the complete two-primitive execution, including both concrete store updates. -/
def traceSuccessor : Exec.Config instrumentedOps :=
  ⟨.exiting (.ret 7), [(y, 7), (x, 7)], ⟨10, (bind types y initial).getD initial⟩, []⟩

/-- Helper (computation). The seven-step execution emits both primitive events in order.
(approved: APPR-C5-codex-902ce88a). -/
private theorem traceSteps : Exec.Steps emptyProg primTraceConfig [7, 3] traceSuccessor := by
  have hsteps : Exec.Steps emptyProg primTraceConfig _ _ :=
    .step (.silent rfl)
      (.step (.prim (a := liftAnswer initial ⟨some 7, .ok 7, 7⟩) rfl rfl rfl)
        (.step (.silent rfl)
          (.step (.silent rfl)
            (.step (.prim (a := liftAnswer ((bind types y initial).getD initial)
                  ⟨some 3, .ok 10, 10⟩) rfl rfl rfl)
              (.step (.silent rfl)
                (.step (.silent rfl) (.refl _)))))))
  exact hsteps

/-- Instance (law `ConcreteSpec.Core.Own.Steps_erasure`, case `ConcreteSpec.Core.Own.ExecErasureWitnesses.bindMoveConfig`). -/
example : FiniteSimulation erasedOps types permitE
    (stepInstrumented erasedOps types permitE emptyProg) emptyProg bindMoveConfig :=
  @Steps_erasure erasedOps types permitE emptyProg bindMoveConfig

/-- Endpoint of the mutant's one-step execution: it has corrupted both local values. -/
def zeroSuccessor : Exec.Config instrumentedOps :=
  ⟨.done, [(y, 0), (x, 0)], ⟨0, (bind types y ((move initial ⟨0⟩).getD initial)).getD initial⟩, []⟩

/-- Planted (law `ConcreteSpec.Core.Own.Steps_erasure`, case `ConcreteSpec.Core.Own.ExecErasureWitnesses.bindMoveConfig`, subject `ConcreteSpec.Core.Own.stepInstrumented`, mutant `ConcreteSpec.Core.Own.ExecErasureWitnesses.stepInstrumentedZeroVal`). -/
example : ¬ FiniteSimulation erasedOps types permitE
    (stepInstrumentedZeroVal erasedOps types permitE emptyProg) emptyProg bindMoveConfig := by
  intro h
  have bad : FiniteExecution (stepInstrumentedZeroVal erasedOps types permitE emptyProg)
      bindMoveConfig [] zeroSuccessor := .silent rfl (.refl _)
  have impossible : ∀ tr, ¬ Exec.Steps (eraseProgram emptyProg)
      (eraseConfig bindMoveConfig) tr (eraseConfig zeroSuccessor) := by
    intro tr he
    cases he with
    | step hs rest =>
      have hd := step_det_next_inv (O := erasedOps) (P := eraseProgram emptyProg) (c₁ := ⟨.done, [(y, 7), (x, 7)], 0, []⟩) rfl hs
      subst hd
      cases rest with
      | step hs _ => exact step_det_none_inv rfl hs
  exact impossible [] (h [] zeroSuccessor bad)

/-- Instance (law `ConcreteSpec.Core.Own.runFrom_erasure`, case `ConcreteSpec.Core.Own.Examples.primTraceConfig`). -/
example : Exec.runFrom (eraseProgram emptyProg) erasedOracle 100 [] (eraseConfig primTraceConfig) =
    eraseReport (Exec.runFrom emptyProg (liftOracle erasedOps types permitE erasedOracle) 100 [] primTraceConfig) :=
  @runFrom_erasure erasedOps types permitE emptyProg erasedOracle 100 [] primTraceConfig rfl

/-- Planted (law `ConcreteSpec.Core.Own.runFrom_erasure`, case `ConcreteSpec.Core.Own.Examples.primTraceConfig`, subject `ConcreteSpec.Core.Own.liftOracle`, mutant `ConcreteSpec.Core.Own.ExecErasureWitnesses.liftOracleMutateOwn`). -/
example : Ending.NotStuck (Exec.runFrom emptyProg
      (liftOracleMutateOwn erasedOps types permitE erasedOracle) 100 [] primTraceConfig).ending ∧
    ¬ (Exec.runFrom (eraseProgram emptyProg) erasedOracle 100 [] (eraseConfig primTraceConfig) =
      eraseReport (Exec.runFrom emptyProg (liftOracleMutateOwn erasedOps types permitE erasedOracle) 100 [] primTraceConfig)) := by
  exact ⟨rfl, by intro h; have he := congrArg (fun r => r.events) h; cases he⟩

/-- Instance (law `ConcreteSpec.Core.Own.run_erasure`, case `ConcreteSpec.Core.Own.Examples.primTraceConfig`). -/
example : Exec.run (eraseProgram emptyProg) erasedOracle 100 (eraseConfig primTraceConfig) =
    eraseReport (Exec.run emptyProg (liftOracle erasedOps types permitE erasedOracle) 100 primTraceConfig) :=
  @run_erasure erasedOps types permitE emptyProg erasedOracle 100 primTraceConfig rfl

/-- Planted (law `ConcreteSpec.Core.Own.run_erasure`, case `ConcreteSpec.Core.Own.Examples.primTraceConfig`, subject `ConcreteSpec.Core.Own.liftOracle`, mutant `ConcreteSpec.Core.Own.ExecErasureWitnesses.liftOracleMutateOwn`). -/
example : Ending.NotStuck (Exec.run emptyProg
      (liftOracleMutateOwn erasedOps types permitE erasedOracle) 100 primTraceConfig).ending ∧
    ¬ (Exec.run (eraseProgram emptyProg) erasedOracle 100 (eraseConfig primTraceConfig) =
      eraseReport (Exec.run emptyProg (liftOracleMutateOwn erasedOps types permitE erasedOracle) 100 primTraceConfig)) := by
  exact ⟨rfl, by intro h; have he := congrArg (fun r => r.events) h; cases he⟩

/-- Helper (inversion). The single-step move-binding configuration `bindMoveConfig`
satisfies `CheckerAccepted`. (approved: APPR-C5-codex-902ce88a). -/
private theorem bindMove_checkerAccepted_inv :
    CheckerAccepted erasedOps types permitE emptyProg bindMoveConfig := by
  intro tr c hsteps
  cases hsteps with
  | refl =>
    refine ⟨by decide, ⟨fun _ => by decide, fun v s' he => ?_⟩⟩
    have hs' : ⟨0, (move initial ⟨0⟩).getD initial⟩ = s' :=
      congrArg (fun (r : Res (Int × Store Int)) =>
        match r with | .ok (_, s) => s | _ => ⟨0, initial⟩) he
    rw [← hs']
    intro _
    decide
  | step hs₁ hsteps =>
    obtain rfl := step_det_next_inv rfl hs₁
    cases hsteps with
    | refl => exact ⟨by decide, trivial⟩
    | step hs₂ _ => exact step_det_none_inv rfl hs₂

/-- Instance (law `ConcreteSpec.Core.Own.step_stuck_iff_of_accepted`, case `ConcreteSpec.Core.Own.ExecErasureWitnesses.bindMoveConfig`). -/
example : stepInstrumented erasedOps types permitE emptyProg bindMoveConfig = none ↔
    Exec.step (eraseProgram emptyProg) (eraseConfig bindMoveConfig) = none :=
  @step_stuck_iff_of_accepted erasedOps types permitE emptyProg bindMoveConfig
    bindMove_checkerAccepted_inv

/-- Planted (law `ConcreteSpec.Core.Own.step_stuck_iff_of_accepted`, case `ConcreteSpec.Core.Own.ExecErasureWitnesses.bindMoveConfig`, subject `ConcreteSpec.Core.Own.stepInstrumented`, mutant `ConcreteSpec.Core.Own.ExecErasureWitnesses.stepInstrumentedRefuse`). -/
example : CheckerAccepted erasedOps types permitE emptyProg bindMoveConfig ∧
    ¬ (stepInstrumentedRefuse erasedOps types permitE emptyProg bindMoveConfig = none ↔
      Exec.step (eraseProgram emptyProg) (eraseConfig bindMoveConfig) = none) :=
  ⟨bindMove_checkerAccepted_inv, fun h => by cases h.mp rfl⟩

/-- Instance (law `ConcreteSpec.Core.Own.runFrom_erasure_of_accepted`, case `ConcreteSpec.Core.Own.ExecErasureWitnesses.primFrameStuckConfig`). -/
example : Exec.runFrom (eraseProgram emptyProg) erasedOracle 100 [] (eraseConfig primFrameStuckConfig) =
    eraseReport (Exec.runFrom emptyProg (liftOracle erasedOps types permitE erasedOracle) 100 [] primFrameStuckConfig) :=
  @runFrom_erasure_of_accepted erasedOps types permitE emptyProg erasedOracle 100 []
    primFrameStuckConfig primFrameStuck_checkerAccepted_inv

/-- Planted (law `ConcreteSpec.Core.Own.runFrom_erasure_of_accepted`, case `ConcreteSpec.Core.Own.ExecErasureWitnesses.primFrameStuckConfig`, subject `ConcreteSpec.Core.Own.liftOracle`, mutant `ConcreteSpec.Core.Own.ExecErasureWitnesses.liftOracleMutateOwn`). -/
example : CheckerAccepted erasedOps types permitE emptyProg primFrameStuckConfig ∧
    ¬ (Exec.runFrom (eraseProgram emptyProg) erasedOracle 100 [] (eraseConfig primFrameStuckConfig) =
      eraseReport (Exec.runFrom emptyProg (liftOracleMutateOwn erasedOps types permitE erasedOracle) 100 [] primFrameStuckConfig)) :=
  ⟨primFrameStuck_checkerAccepted_inv,
    by intro h; have he := congrArg (fun r => r.events) h; cases he⟩

/-- Instance (law `ConcreteSpec.Core.Own.run_stuck_iff_of_accepted`, case `ConcreteSpec.Core.Own.ExecErasureWitnesses.primFrameStuckConfig`). -/
example : Ending.IsStuck (Exec.run emptyProg (liftOracle erasedOps types permitE erasedOracle) 100 primFrameStuckConfig).ending =
    Ending.IsStuck (Exec.run (eraseProgram emptyProg) erasedOracle 100 (eraseConfig primFrameStuckConfig)).ending :=
  @run_stuck_iff_of_accepted erasedOps types permitE emptyProg erasedOracle 100
    primFrameStuckConfig primFrameStuck_checkerAccepted_inv

/-- Planted (law `ConcreteSpec.Core.Own.run_stuck_iff_of_accepted`, case `ConcreteSpec.Core.Own.ExecErasureWitnesses.primFrameStuckConfig`, subject `ConcreteSpec.Core.Own.liftOracle`, mutant `ConcreteSpec.Core.Own.ExecErasureWitnesses.liftOracleDropAnswer`). -/
example : CheckerAccepted erasedOps types permitE emptyProg primFrameStuckConfig ∧
    ¬ (Ending.IsStuck (Exec.run emptyProg (liftOracleDropAnswer erasedOps types permitE erasedOracle) 100 primFrameStuckConfig).ending =
      Ending.IsStuck (Exec.run (eraseProgram emptyProg) erasedOracle 100 (eraseConfig primFrameStuckConfig)).ending) :=
  ⟨primFrameStuck_checkerAccepted_inv, by decide⟩

end ExecErasureWitnesses

/-- Helper (computation). The permission-only transition is identity; operation preservation is not independent soundness evidence here. (approved: APPR-C5-codex-902ce88a). -/
theorem accessOwner_preserves (s : State) (p : Hybrid.Place) (m : Mode) (hs : WellFormed s) : StateResult (accessOwner s p m) := by
  cases he : accessOwner s p m with
  | none => trivial
  | some t => exact accessOwner_preserves_result s t p m hs he

/-- Helper (computation). The loan permission transition is identity; loan conflicts are justified by the invariant and scoped protocol laws. (approved: APPR-C5-codex-902ce88a). -/
theorem accessLoan_preserves (s : State) (i : LoanId) (m : Mode) (hs : WellFormed s) : StateResult (accessLoan s i m) := by
  cases he : accessLoan s i m with
  | none => trivial
  | some t => exact accessLoan_preserves_result s t i m hs he

namespace PreservationEvidence
/-- A fresh callee with a pending direct owner argument, before adoption. -/
def adoptionInput : State :=
  (enter Examples.types [] DirectArgumentWitnesses.waiting.own).getD Examples.initial

/-- Mutant: move appends a moved slot record without replacing the existing record for `p`,
breaking unique slot place identities (`(s.slots.map Slot.place).Nodup`). -/
def moveDuplicateSlot (s : State) (p : Hybrid.Place) : Option State := do
  let x ← slot s p
  if x.desc.copy || x.status == .moved || s.loans.any (·.root == p) then none else
    some { s with slots := { x with status := .moved } :: s.slots }

/-- Mutant: reinitialization assigns an unissued place identity `⟨s.nextPlace⟩` without advancing
`s.nextPlace`, breaking `x.place.id < s.nextPlace` and frame-local completeness. -/
def reinitializeUnissuedPlace (s : State) (p : Hybrid.Place) : Option State := do
  let _ ← reinitialize s p
  some { s with slots := s.slots.map fun x =>
    if x.place == p then { x with place := ⟨s.nextPlace⟩, status := .owned } else x }

/-- Mutant: borrow issues `⟨s.nextLoan⟩` without advancing `nextLoan`, breaking `l.id.id < s.nextLoan`. -/
def borrowNoAdvanceSupply (s : State) (p : Hybrid.Place) (path : List Projection) (m : Mode) : Option State := do
  let t ← borrow s p path m
  some { t with nextLoan := s.nextLoan }

/-- Mutant: reborrowTo removes the parent loan `i` when inserting its child, breaking `ParentValid`. -/
def reborrowToDropParent (s : State) (i : LoanId) (path : List Projection) (m : Mode) (a : Nat) : Option State := do
  let t ← reborrowTo s i path m a
  some { t with loans := t.loans.filter (·.id != i) }

/-- Mutant: reborrow clears the cached ancestor chain while retaining `parent = some i`,
breaking `ParentValid`'s ancestor-chain agreement. -/
def reborrowDropAncestors (s : State) (i : LoanId) (path : List Projection) (m : Mode) : Option State := do
  let t ← reborrow s i path m
  some { t with loans := t.loans.map fun l =>
    if l.id == ⟨s.nextLoan⟩ then { l with ancestors := [] } else l }

/-- Mutant: endLoan removes the parent of `i` instead of `i`, stranding child `i` and breaking `ParentValid`. -/
def endLoanRemoveParent (s : State) (i : LoanId) : Option State := do
  let a ← loan s i
  let _ ← endLoan s i
  some { s with loans := s.loans.filter fun l => some l.id != a.parent }

/-- Mutant: bind allocates `⟨s.nextPlace⟩` without advancing `nextPlace`, breaking `x.place.id < s.nextPlace`. -/
def bindNoAdvancePlace (types : Var → Option Descriptor) (x : Var) (s : State) : Option State := do
  let t ← bind types x s
  some { t with nextPlace := s.nextPlace }

/-- Mutant: drop removes the variable from `f.locals` while retaining its slot in `s.slots`,
breaking the frame-local completeness clause of `WellFormed`. -/
def dropLocalOnly (xs : List Var) (s : State) : Option State := do
  let t ← drop xs s
  some { t with slots := s.slots }

/-- Mutant: enter reuses the caller's activation ID `0` for the new frame instead of `s.nextActivation`,
breaking `(s.frames.map Activation.id).Nodup`. -/
def enterDuplicateFrameId (types : Var → Option Descriptor) (xs : List Var) (s : State) : Option State := do
  let t ← enter types xs s
  some { t with frames := t.frames.map fun f => { f with id := 0 } }

/-- Mutant: endLocals removes caller loans (`activation != a`) instead of ending callee loans (`activation == a`),
breaking `ParentValid` for the callee child loan. -/
def endLocalsCallerInstead (a : Nat) (s : State) : Option State := do
  let _ ← endLocals a s
  some { s with loans := s.loans.filter (·.activation == a) }

/-- Mutant: leave drops local slots and pops the frame without running `endLocals`, leaving the
departing local's loan without an owned root in `t.slots`. -/
def leaveKeepLocalLoans (s : State) : Option State := do
  let t ← leave s
  some { t with loans := s.loans }

/-- Mutant: enterDown reuses the parent loan's ID `⟨0⟩` for the new child loan, breaking `(s.loans.map Loan.id).Nodup`. -/
def enterDownReuseParentId (s : State) (args : List (Var × LoanId)) : Option State := do
  let t ← enterDown s args
  some { t with loans := t.loans.map fun l => { l with id := ⟨0⟩ } }

/-- Mutant: life on `.bind` adds the new slot to `s.slots` without recording it in `f.locals`,
breaking the frame-local completeness clause of `WellFormed`. -/
def lifeBindUnindexed (types : Var → Option Descriptor) (e : Exec.Life) (s : State) : Option State := do
  let t ← life types e s
  some { t with frames := s.frames }

/-- Mutant: adoptDown assigns the unissued activation ID `s.nextActivation` to adopted loans,
breaking `l.activation < s.nextActivation`. -/
def adoptDownUnissuedActivation (s : State) (args : List (Var × LoanId)) : Option State := do
  let t ← adoptDown s args
  some { t with loans := t.loans.map fun l =>
    if l.id ∈ args.map Prod.snd then { l with activation := t.nextActivation } else l }

/-- Mutant: passDown clears `parent` on the new non-root pending loan (`callRoot = false`),
breaking `l.pendingArgument = true → (l.parent.isSome = true ∨ l.callRoot = true)`. -/
def passDownClearParent (s : CallState) (i : LoanId) (x : Var) : Option CallState := do
  let t ← passDown s i x
  some { t with own := { t.own with loans := t.own.loans.map fun l =>
    if l.id == ⟨s.own.nextLoan⟩ then { l with parent := none, ancestors := [] } else l } }

/-- Mutant: passDown marks the new loan `pendingArgument = true` in `own` without appending it to
`s.pending`, breaking `CallState.WellFormed`'s pending-flag synchronization. -/
def passDownOmitPending (s : CallState) (i : LoanId) (x : Var) : Option CallState := do
  let t ← passDown s i x
  some { t with pending := s.pending }

/-- Mutant: callLife adopts pending loans into the unissued activation `own.nextActivation`,
breaking `l.activation < s.nextActivation`. -/
def callLifeUnissuedActivation (types : Var → Option Descriptor) (e : Exec.Life) (s : CallState) : Option CallState := do
  let t ← callLife types e s
  some { t with own := { t.own with loans := t.own.loans.map fun l =>
    { l with activation := t.own.nextActivation } } }

/-- Mutant: callLife adopts pending loans (clearing their `pendingArgument` flags) without clearing
`s.pending`, breaking `CallState.WellFormed`'s pending-flag synchronization. -/
def callLifeKeepPending (types : Var → Option Descriptor) (e : Exec.Life) (s : CallState) : Option CallState := do
  let t ← callLife types e s
  some { t with pending := s.pending }

/-- Mutant: passOwner creates a parentless pending loan with `callRoot = false`,
breaking `l.pendingArgument = true → (l.parent.isSome = true ∨ l.callRoot = true)`. -/
def passOwnerClearCallRoot (s : CallState) (p : Hybrid.Place) (path : List Projection) (m : Mode) (x : Var) : Option CallState := do
  let t ← passOwner s p path m x
  some { t with own := { t.own with loans := t.own.loans.map fun l =>
    if l.id == ⟨s.own.nextLoan⟩ then { l with callRoot := false } else l } }

/-- Mutant: passOwner marks the new root loan `pendingArgument = true` in `own` without appending
it to `s.pending`, breaking `CallState.WellFormed`'s pending-flag synchronization. -/
def passOwnerOmitPending (s : CallState) (p : Hybrid.Place) (path : List Projection) (m : Mode) (x : Var) : Option CallState := do
  let t ← passOwner s p path m x
  some { t with pending := s.pending }

/-- Mutant: accessOwner consumes the accessed root slot (marking it `.moved`) while a shared loan
remains active on `p`, breaking `∃ x ∈ s.slots, x.place = l.root ∧ x.status = .owned`. -/
def accessOwnerConsumeRoot (s : State) (p : Hybrid.Place) (m : Mode) : Option State := do
  let t ← accessOwner s p m
  some { t with slots := t.slots.map fun x =>
    if x.place == p then { x with status := .moved } else x }

/-- Mutant: accessLoan drops the parent of loan `i` while `i` remains active, breaking `ParentValid`. -/
def accessLoanDropParent (s : State) (i : LoanId) (m : Mode) : Option State := do
  let a ← loan s i
  let t ← accessLoan s i m
  some { t with loans := t.loans.filter fun l => some l.id != a.parent }

end PreservationEvidence

namespace PreservationEvidence
open Examples

/-- Instance (law `ConcreteSpec.Core.Own.move_preserves`, case `ConcreteSpec.Core.Own.Examples.initial`). -/
example : StateResult (move (initial) (⟨0⟩)) :=
  @move_preserves initial ⟨0⟩ (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.move_preserves`, case `ConcreteSpec.Core.Own.Examples.initial`, subject `ConcreteSpec.Core.Own.move`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.moveDuplicateSlot`). -/
example : (WellFormed (initial)) ∧ ¬ StateResult (moveDuplicateSlot (initial) (⟨0⟩)) := by decide

/-- Instance (law `ConcreteSpec.Core.Own.reinitialize_preserves`, case `ConcreteSpec.Core.Own.LifecycleWitnesses.moved`). -/
example : StateResult (reinitialize (LifecycleWitnesses.moved) (⟨0⟩)) :=
  @reinitialize_preserves LifecycleWitnesses.moved ⟨0⟩ (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.reinitialize_preserves`, case `ConcreteSpec.Core.Own.LifecycleWitnesses.moved`, subject `ConcreteSpec.Core.Own.reinitialize`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.reinitializeUnissuedPlace`). -/
example : (WellFormed (LifecycleWitnesses.moved)) ∧ ¬ StateResult (reinitializeUnissuedPlace (LifecycleWitnesses.moved) (⟨0⟩)) := by decide

/-- Instance (law `ConcreteSpec.Core.Own.borrow_preserves`, case `ConcreteSpec.Core.Own.Examples.shared`). -/
example : StateResult (borrow (shared) (⟨0⟩) ([.index 1]) (.shared)) :=
  @borrow_preserves shared ⟨0⟩ [.index 1] .shared (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.borrow_preserves`, case `ConcreteSpec.Core.Own.Examples.shared`, subject `ConcreteSpec.Core.Own.borrow`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.borrowNoAdvanceSupply`). -/
example : (WellFormed (shared)) ∧ ¬ StateResult (borrowNoAdvanceSupply (shared) (⟨0⟩) ([.index 1]) (.shared)) := by decide

/-- Instance (law `ConcreteSpec.Core.Own.reborrowTo_preserves`, case `ConcreteSpec.Core.Own.Examples.exclusive`). -/
example : StateResult (reborrowTo (exclusive) (⟨0⟩) ([.index 1]) (.mutable) (0)) :=
  @reborrowTo_preserves exclusive ⟨0⟩ [.index 1] .mutable 0 (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.reborrowTo_preserves`, case `ConcreteSpec.Core.Own.Examples.exclusive`, subject `ConcreteSpec.Core.Own.reborrowTo`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.reborrowToDropParent`). -/
example : (WellFormed (exclusive)) ∧ ¬ StateResult (reborrowToDropParent (exclusive) (⟨0⟩) ([.index 1]) (.mutable) (0)) := by decide

/-- Instance (law `ConcreteSpec.Core.Own.reborrow_preserves`, case `ConcreteSpec.Core.Own.Examples.exclusive`). -/
example : StateResult (reborrow (exclusive) (⟨0⟩) ([.index 1]) (.mutable)) :=
  @reborrow_preserves exclusive ⟨0⟩ [.index 1] .mutable (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.reborrow_preserves`, case `ConcreteSpec.Core.Own.Examples.exclusive`, subject `ConcreteSpec.Core.Own.reborrow`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.reborrowDropAncestors`). -/
example : (WellFormed (exclusive)) ∧ ¬ StateResult (reborrowDropAncestors (exclusive) (⟨0⟩) ([.index 1]) (.mutable)) := by decide

/-- Instance (law `ConcreteSpec.Core.Own.endLoan_preserves`, case `ConcreteSpec.Core.Own.Examples.child`). -/
example : StateResult (endLoan (child) (⟨1⟩)) :=
  @endLoan_preserves child ⟨1⟩ (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.endLoan_preserves`, case `ConcreteSpec.Core.Own.Examples.child`, subject `ConcreteSpec.Core.Own.endLoan`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.endLoanRemoveParent`). -/
example : (WellFormed (child)) ∧ ¬ StateResult (endLoanRemoveParent (child) (⟨1⟩)) := by decide

/-- Computation observation. -/
example : StateResult (accessOwner (shared) (⟨0⟩) (.shared)) :=
  @accessOwner_preserves shared ⟨0⟩ .shared (by decide)

/-- Computation observation. -/
example : (WellFormed (shared)) ∧ ¬ StateResult (accessOwnerConsumeRoot (shared) (⟨0⟩) (.shared)) := by decide

/-- Computation observation. -/
example : StateResult (accessLoan (child) (⟨1⟩) (.mutable)) :=
  @accessLoan_preserves child ⟨1⟩ .mutable (by decide)

/-- Computation observation. -/
example : (WellFormed (child)) ∧ ¬ StateResult (accessLoanDropParent (child) (⟨1⟩) (.mutable)) := by decide

/-- Instance (law `ConcreteSpec.Core.Own.bind_preserves`, case `ConcreteSpec.Core.Own.Examples.shared`). -/
example : StateResult (bind (types) (y) (shared)) :=
  @bind_preserves types shared y (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.bind_preserves`, case `ConcreteSpec.Core.Own.Examples.shared`, subject `ConcreteSpec.Core.Own.bind`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.bindNoAdvancePlace`). -/
example : (WellFormed (shared)) ∧ ¬ StateResult (bindNoAdvancePlace (types) (y) (shared)) := by decide

/-- Instance (law `ConcreteSpec.Core.Own.drop_preserves`, case `ConcreteSpec.Core.Own.LifecycleWitnesses.moved`). -/
example : StateResult (drop ([x]) (LifecycleWitnesses.moved)) :=
  @drop_preserves LifecycleWitnesses.moved [x] (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.drop_preserves`, case `ConcreteSpec.Core.Own.LifecycleWitnesses.moved`, subject `ConcreteSpec.Core.Own.drop`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.dropLocalOnly`). -/
example : (WellFormed (LifecycleWitnesses.moved)) ∧ ¬ StateResult (dropLocalOnly ([x]) (LifecycleWitnesses.moved)) := by decide

/-- Instance (law `ConcreteSpec.Core.Own.enter_preserves`, case `ConcreteSpec.Core.Own.Examples.shared`). -/
example : StateResult (enter (types) ([y]) (shared)) :=
  @enter_preserves types shared [y] (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.enter_preserves`, case `ConcreteSpec.Core.Own.Examples.shared`, subject `ConcreteSpec.Core.Own.enter`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.enterDuplicateFrameId`). -/
example : (WellFormed (shared)) ∧ ¬ StateResult (enterDuplicateFrameId (types) ([y]) (shared)) := by decide

/-- Instance (law `ConcreteSpec.Core.Own.endLocals_preserves`, case `ConcreteSpec.Core.Own.LifecycleWitnesses.called`). -/
example : StateResult (endLocals (1) (LifecycleWitnesses.called)) :=
  @endLocals_preserves LifecycleWitnesses.called 1 (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.endLocals_preserves`, case `ConcreteSpec.Core.Own.LifecycleWitnesses.called`, subject `ConcreteSpec.Core.Own.endLocals`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.endLocalsCallerInstead`). -/
example : (WellFormed (LifecycleWitnesses.called)) ∧ ¬ StateResult (endLocalsCallerInstead (1) (LifecycleWitnesses.called)) := by decide

/-- Instance (law `ConcreteSpec.Core.Own.leave_preserves`, case `ConcreteSpec.Core.Own.RoundThreeWitnesses.localLoan`). -/
example : StateResult (leave (RoundThreeWitnesses.localLoan)) :=
  @leave_preserves RoundThreeWitnesses.localLoan (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.leave_preserves`, case `ConcreteSpec.Core.Own.RoundThreeWitnesses.localLoan`, subject `ConcreteSpec.Core.Own.leave`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.leaveKeepLocalLoans`). -/
example : (WellFormed (RoundThreeWitnesses.localLoan)) ∧ ¬ StateResult (leaveKeepLocalLoans (RoundThreeWitnesses.localLoan)) := by decide

/-- Instance (law `ConcreteSpec.Core.Own.enterDown_preserves`, case `ConcreteSpec.Core.Own.Examples.exclusive`). -/
example : StateResult (enterDown (exclusive) ([(y, ⟨0⟩)])) :=
  @enterDown_preserves exclusive [(y, ⟨0⟩)] (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.enterDown_preserves`, case `ConcreteSpec.Core.Own.Examples.exclusive`, subject `ConcreteSpec.Core.Own.enterDown`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.enterDownReuseParentId`). -/
example : (WellFormed (exclusive)) ∧ ¬ StateResult (enterDownReuseParentId (exclusive) ([(y, ⟨0⟩)])) := by decide

/-- Instance (law `ConcreteSpec.Core.Own.life_preserves`, case `ConcreteSpec.Core.Own.Examples.shared`). -/
example : StateResult (life (types) ((.bind y)) (shared)) :=
  @life_preserves types (.bind y) shared (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.life_preserves`, case `ConcreteSpec.Core.Own.Examples.shared`, subject `ConcreteSpec.Core.Own.life`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.lifeBindUnindexed`). -/
example : (WellFormed (shared)) ∧ ¬ StateResult (lifeBindUnindexed (types) ((.bind y)) (shared)) := by decide

/-- Instance (law `ConcreteSpec.Core.Own.adoptDown_preserves`, case `ConcreteSpec.Core.Own.PreservationEvidence.adoptionInput`). -/
example : StateResult (adoptDown (adoptionInput) (DirectArgumentWitnesses.waiting.pending)) :=
  @adoptDown_preserves adoptionInput DirectArgumentWitnesses.waiting.pending (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.adoptDown_preserves`, case `ConcreteSpec.Core.Own.PreservationEvidence.adoptionInput`, subject `ConcreteSpec.Core.Own.adoptDown`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.adoptDownUnissuedActivation`). -/
example : (WellFormed (adoptionInput)) ∧ ¬ StateResult (adoptDownUnissuedActivation (adoptionInput) (DirectArgumentWitnesses.waiting.pending)) := by decide

/-- Instance (law `ConcreteSpec.Core.Own.passDown_preserves`, case `ConcreteSpec.Core.Own.Examples.exclusive`). -/
example : CallOwnResult (passDown (⟨exclusive, []⟩) (⟨0⟩) (y)) :=
  @passDown_preserves ⟨exclusive, []⟩ ⟨0⟩ y (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.passDown_preserves`, case `ConcreteSpec.Core.Own.Examples.exclusive`, subject `ConcreteSpec.Core.Own.passDown`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.passDownClearParent`). -/
example : (WellFormed (⟨exclusive, []⟩ : CallState).own) ∧ ¬ CallOwnResult (passDownClearParent (⟨exclusive, []⟩) (⟨0⟩) (y)) := by decide

/-- Instance (law `ConcreteSpec.Core.Own.passDown_call_preserves`, case `ConcreteSpec.Core.Own.Examples.exclusive`). -/
example : CallResult (passDown (⟨exclusive, []⟩) (⟨0⟩) (y)) :=
  @passDown_call_preserves ⟨exclusive, []⟩ ⟨0⟩ y (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.passDown_call_preserves`, case `ConcreteSpec.Core.Own.Examples.exclusive`, subject `ConcreteSpec.Core.Own.passDown`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.passDownOmitPending`). -/
example : (CallState.WellFormed (⟨exclusive, []⟩)) ∧ ¬ CallResult (passDownOmitPending (⟨exclusive, []⟩) (⟨0⟩) (y)) := by decide

/-- Instance (law `ConcreteSpec.Core.Own.callLife_preserves`, case `ConcreteSpec.Core.Own.CallWitnesses.waiting`). -/
example : CallOwnResult (callLife (types) ((.enter [y])) (CallWitnesses.waiting)) :=
  @callLife_preserves types (.enter [y]) CallWitnesses.waiting (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.callLife_preserves`, case `ConcreteSpec.Core.Own.CallWitnesses.waiting`, subject `ConcreteSpec.Core.Own.callLife`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.callLifeUnissuedActivation`). -/
example : (WellFormed (CallWitnesses.waiting).own) ∧ ¬ CallOwnResult (callLifeUnissuedActivation (types) ((.enter [y])) (CallWitnesses.waiting)) := by decide

/-- Instance (law `ConcreteSpec.Core.Own.callLife_call_preserves`, case `ConcreteSpec.Core.Own.CallWitnesses.waiting`). -/
example : CallResult (callLife (types) ((.enter [y])) (CallWitnesses.waiting)) :=
  @callLife_call_preserves types (.enter [y]) CallWitnesses.waiting (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.callLife_call_preserves`, case `ConcreteSpec.Core.Own.CallWitnesses.waiting`, subject `ConcreteSpec.Core.Own.callLife`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.callLifeKeepPending`). -/
example : (CallState.WellFormed (CallWitnesses.waiting)) ∧ ¬ CallResult (callLifeKeepPending (types) ((.enter [y])) (CallWitnesses.waiting)) := by decide

/-- Instance (law `ConcreteSpec.Core.Own.passOwner_preserves`, case `ConcreteSpec.Core.Own.DirectArgumentWitnesses.caller`). -/
example : CallOwnResult (passOwner (DirectArgumentWitnesses.caller) (⟨1⟩) ([.index 3]) (.mutable) (DirectArgumentWitnesses.z)) :=
  @passOwner_preserves DirectArgumentWitnesses.caller ⟨1⟩ [.index 3] .mutable DirectArgumentWitnesses.z (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.passOwner_preserves`, case `ConcreteSpec.Core.Own.DirectArgumentWitnesses.caller`, subject `ConcreteSpec.Core.Own.passOwner`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.passOwnerClearCallRoot`). -/
example : (WellFormed (DirectArgumentWitnesses.caller).own) ∧ ¬ CallOwnResult (passOwnerClearCallRoot (DirectArgumentWitnesses.caller) (⟨1⟩) ([.index 3]) (.mutable) (DirectArgumentWitnesses.z)) := by decide

/-- Instance (law `ConcreteSpec.Core.Own.passOwner_call_preserves`, case `ConcreteSpec.Core.Own.DirectArgumentWitnesses.caller`). -/
example : CallResult (passOwner (DirectArgumentWitnesses.caller) (⟨1⟩) ([.index 3]) (.mutable) (DirectArgumentWitnesses.z)) :=
  @passOwner_call_preserves DirectArgumentWitnesses.caller ⟨1⟩ [.index 3] .mutable DirectArgumentWitnesses.z (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.passOwner_call_preserves`, case `ConcreteSpec.Core.Own.DirectArgumentWitnesses.caller`, subject `ConcreteSpec.Core.Own.passOwner`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.passOwnerOmitPending`). -/
example : (CallState.WellFormed (DirectArgumentWitnesses.caller)) ∧ ¬ CallResult (passOwnerOmitPending (DirectArgumentWitnesses.caller) (⟨1⟩) ([.index 3]) (.mutable) (DirectArgumentWitnesses.z)) := by decide

end PreservationEvidence

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
  @leave_no_local_loan RoundThreeWitnesses.localLoan ⟨1, [(y, ⟨1⟩)], []⟩ (y, ⟨1⟩) (rfl) (by decide) (.head [])

/-- Planted (law `ConcreteSpec.Core.Own.leave_no_local_loan`, case `ConcreteSpec.Core.Own.LifetimeEvidence.leave_no_local_loanCase`, subject `ConcreteSpec.Core.Own.leave`, mutant `ConcreteSpec.Core.Own.LifetimeEvidence.leaveIdentity`). -/
example : ((RoundThreeWitnesses.localLoan).frames.head? = some (⟨1, [(y, ⟨1⟩)], []⟩ : Activation)) ∧ (WellFormed (RoundThreeWitnesses.localLoan)) ∧ (((y, Hybrid.Place.mk 1)) ∈ (⟨1, [(y, ⟨1⟩)], []⟩ : Activation).locals) ∧ ¬ ResultSatisfies (fun t : State => ∀ l ∈ t.loans, l.root ≠ ((y, Hybrid.Place.mk 1)).2) (leaveIdentity (RoundThreeWitnesses.localLoan)) := by decide

def leave_no_activation_slotCase := (RoundThreeWitnesses.localLoan)

/-- Instance (law `ConcreteSpec.Core.Own.leave_no_activation_slot`, case `ConcreteSpec.Core.Own.LifetimeEvidence.leave_no_activation_slotCase`). -/
example : ResultSatisfies (fun t : State => ∀ x ∈ t.slots, x.activation ≠ (⟨1, [(y, ⟨1⟩)], []⟩ : Activation).id) (leave (RoundThreeWitnesses.localLoan)) :=
  @leave_no_activation_slot RoundThreeWitnesses.localLoan ⟨1, [(y, ⟨1⟩)], []⟩ (by decide) (rfl)

/-- Planted (law `ConcreteSpec.Core.Own.leave_no_activation_slot`, case `ConcreteSpec.Core.Own.LifetimeEvidence.leave_no_activation_slotCase`, subject `ConcreteSpec.Core.Own.leave`, mutant `ConcreteSpec.Core.Own.LifetimeEvidence.leaveIdentity`). -/
example : (WellFormed (RoundThreeWitnesses.localLoan)) ∧ ((RoundThreeWitnesses.localLoan).frames.head? = some (⟨1, [(y, ⟨1⟩)], []⟩ : Activation)) ∧ ¬ ResultSatisfies (fun t : State => ∀ x ∈ t.slots, x.activation ≠ (⟨1, [(y, ⟨1⟩)], []⟩ : Activation).id) (leaveIdentity (RoundThreeWitnesses.localLoan)) := by decide

def passOwner_freezesCase := (DirectArgumentWitnesses.caller)

/-- Instance (law `ConcreteSpec.Core.Own.passOwner_freezes`, case `ConcreteSpec.Core.Own.LifetimeEvidence.passOwner_freezesCase`). -/
example : ResultSatisfies (fun t : CallState => ownerAccess t.own (⟨1⟩) .mutable = false ∧ ((Mode.mutable) = .mutable → ownerAccess t.own (⟨1⟩) .shared = false)) (passOwner (DirectArgumentWitnesses.caller) (⟨1⟩) ([.index 3]) (Mode.mutable) (DirectArgumentWitnesses.z)) :=
  @passOwner_freezes DirectArgumentWitnesses.caller ⟨1⟩ [.index 3] .mutable DirectArgumentWitnesses.z

/-- Planted (law `ConcreteSpec.Core.Own.passOwner_freezes`, case `ConcreteSpec.Core.Own.LifetimeEvidence.passOwner_freezesCase`, subject `ConcreteSpec.Core.Own.passOwner`, mutant `ConcreteSpec.Core.Own.LifetimeEvidence.passOwnerNoFreeze`). -/
example : ¬ ResultSatisfies (fun t : CallState => ownerAccess t.own (⟨1⟩) .mutable = false ∧ ((Mode.mutable) = .mutable → ownerAccess t.own (⟨1⟩) .shared = false)) (passOwnerNoFreeze (DirectArgumentWitnesses.caller) (⟨1⟩) ([.index 3]) (Mode.mutable) (DirectArgumentWitnesses.z)) := by decide

def leave_retains_callerCase := (RoundThreeWitnesses.localLoan)

/-- Instance (law `ConcreteSpec.Core.Own.leave_retains_caller`, case `ConcreteSpec.Core.Own.LifetimeEvidence.leave_retains_callerCase`). -/
example : ResultSatisfies (fun t : State => ((Examples.shared.loans.headD ⟨⟨99⟩, ⟨99⟩, [], .shared, none, [], 99, false, false⟩)) ∈ t.loans) (leave (RoundThreeWitnesses.localLoan)) :=
  @leave_retains_caller RoundThreeWitnesses.localLoan ⟨1, [(y, ⟨1⟩)], []⟩ (Examples.shared.loans.headD ⟨⟨99⟩, ⟨99⟩, [], .shared, none, [], 99, false, false⟩) (by decide) (rfl) (.tail _ (.head [])) (by decide)

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

/-- A state whose active frame omits a slot of its activation from `f.locals` violates
the frame-local completeness clause of `WellFormed`. -/
def unindexedCopy : State :=
  { initial with
    slots := initial.slots.map fun x => { x with desc := { x.desc with copy := true } }
    frames := [⟨0, [], []⟩] }

example : ¬ WellFormed unindexedCopy := by decide
example : ¬ WellFormed { initial with frames := [⟨0, [], []⟩] } := by decide

/-- An issued but inactive activation cannot retain an owner obligation. -/
example : ¬ WellFormed { initial with frames := [⟨1, [], []⟩], nextActivation := 2 } := by decide

/-- A callee cannot index a caller's Copy owner as its own local. -/
example : ¬ WellFormed { initial with
    slots := initial.slots.map fun x => { x with desc := { x.desc with copy := true } }
    frames := [⟨1, [(y, ⟨0⟩)], []⟩, ⟨0, [(x, ⟨0⟩)], []⟩]
    nextActivation := 2 } := by decide

/-- Cleanup removes every alias of a removed place, so no local index dangles. -/
example : let s : State := { LifecycleWitnesses.moved with
    frames := [⟨0, [(x, ⟨0⟩), (y, ⟨0⟩)], []⟩] }
    WellFormed s ∧ (drop [x] s).isSome = true ∧
      WellFormed ((drop [x] s).getD initial) := by decide

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

/-- Mutant: direct lending succeeds through `passOwner`, `enter` and `leave`, but also opens an
extra non-pending caller loan in activation 0 that survives callee exit and violates caller-table
restoration. -/
def passOwnerLeaked (s : CallState) (p : Hybrid.Place) (path : List Projection)
    (m : Mode) (x : Var) : Option CallState := do
  let waiting ← passOwner s p path m x
  some { waiting with own := { waiting.own with
    nextLoan := waiting.own.nextLoan + 1
    loans := ⟨⟨waiting.own.nextLoan⟩, p, path, m, none, [], 0, false, true⟩ :: waiting.own.loans } }

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

/-- Planted (law `ConcreteSpec.Core.Own.directOwner_roundtrip`, case `ConcreteSpec.Core.Own.DirectArgumentWitnesses.caller`, subject `ConcreteSpec.Core.Own.passOwner`, mutant `ConcreteSpec.Core.Own.FinalEvidence.passOwnerLeaked`). -/
example : WellFormed DirectArgumentWitnesses.caller.own ∧
    DirectArgumentWitnesses.caller.own.frames.head? = some callerFrame ∧
    callerFrame.id < DirectArgumentWitnesses.caller.own.nextActivation ∧
    ownerAccess DirectArgumentWitnesses.caller.own ⟨1⟩ .mutable = true ∧
    ¬ ((do
      let waiting ← passOwnerLeaked DirectArgumentWitnesses.caller ⟨1⟩ [.index 3] .mutable DirectArgumentWitnesses.z
      let called ← callLife types (.enter [DirectArgumentWitnesses.z]) waiting
      callLife types .leave called) =
      some ⟨{ DirectArgumentWitnesses.caller.own with
        nextLoan := DirectArgumentWitnesses.caller.own.nextLoan + 1
        nextActivation := DirectArgumentWitnesses.caller.own.nextActivation + 1 }, []⟩) := by decide

/-- The roundtrip mutant succeeds through `passOwnerLeaked`, `enter` and `leave` (`isSome = true`),
so the planted refutation above fails because the returned state leaks a loan, not because a step
returns `none`. -/
example : (do
    let waiting ← passOwnerLeaked DirectArgumentWitnesses.caller ⟨1⟩ [.index 3] .mutable DirectArgumentWitnesses.z
    let called ← callLife types (.enter [DirectArgumentWitnesses.z]) waiting
    callLife types .leave called).isSome = true := by decide

end FinalEvidence

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

def okLocals : AList Var erasedOps.Val := [(x, 7)]
def okArgs : List erasedOps.Expr := [.read x, .move x]
def trapArgs : List erasedOps.Expr := [.move x, .fail .overflow]
def initStore : Store erasedOps.Store := ⟨0, initial⟩

/-- Instance (law `ConcreteSpec.Core.Own.evalArgs_ok_erasure`, case `ConcreteSpec.Core.Own.Examples.initial`). -/
example : EvalArgsOkErasure erasedOps okLocals okArgs initStore
    (Exec.evalArgs (instrumentOperands erasedOps types permitE) okLocals initStore okArgs) :=
  @evalArgs_ok_erasure erasedOps types permitE okLocals okArgs initStore

/-- Planted (law `ConcreteSpec.Core.Own.evalArgs_ok_erasure`, case `ConcreteSpec.Core.Own.Examples.initial`, subject `ConcreteSpec.Core.Own.instrumentOperands`, mutant `ConcreteSpec.Core.Own.ArgumentEvidence.zeroOperands`). -/
example : ¬ EvalArgsOkErasure erasedOps okLocals okArgs initStore
    (Exec.evalArgs (zeroOperands erasedOps types permitE) okLocals initStore okArgs) := by
  change ¬ ((Res.ok ([7, 7], 0) : Res (List Int × Int)) = .ok ([0, 0], 0))
  decide

/-- Instance (law `ConcreteSpec.Core.Own.evalArgs_trap_erasure`, case `ConcreteSpec.Core.Own.Examples.initial`). -/
example : EvalArgsTrapErasure erasedOps okLocals trapArgs initStore
    (Exec.evalArgs (instrumentOperands erasedOps types permitE) okLocals initStore trapArgs) :=
  @evalArgs_trap_erasure erasedOps types permitE okLocals trapArgs initStore

/-- Planted (law `ConcreteSpec.Core.Own.evalArgs_trap_erasure`, case `ConcreteSpec.Core.Own.Examples.initial`, subject `ConcreteSpec.Core.Own.instrumentOperands`, mutant `ConcreteSpec.Core.Own.ArgumentEvidence.abortOperands`). -/
example : ¬ EvalArgsTrapErasure erasedOps okLocals trapArgs initStore
    (Exec.evalArgs (abortOperands erasedOps types permitE) okLocals initStore trapArgs) := by
  change ¬ ((Res.trap .overflow : Res (List Int × Int)) = .trap .abort)
  decide

end ArgumentEvidence

/-- Law (subject `ConcreteSpec.Core.Own.borrow`). In a well-formed ownership state, every loan in
the result of `borrow` has an owned root slot. -/
theorem loan_has_owned_root (s : State) (p : Hybrid.Place) (path : List Projection)
    (m : Mode) (hs : WellFormed s) :
    ResultSatisfies (fun t : State => ∀ l ∈ t.loans, ∃ x ∈ t.slots, x.place = l.root ∧ x.status = .owned)
      (borrow s p path m) := by
  cases h : borrow s p path m with
  | none => trivial
  | some t =>
    intro l hl
    have hwf : StateResult (borrow s p path m) := borrow_preserves s p path m hs
    rw [h] at hwf
    exact loan_has_owned_root_of_wf t hwf l hl

/-- Law (subject `ConcreteSpec.Core.Own.move`). In a well-formed ownership state, no loan in the
result of `move` has a moved root slot. -/
theorem moved_no_loan (s : State) (p : Hybrid.Place) (hs : WellFormed s) :
    ResultSatisfies (fun t : State => ∀ x ∈ t.slots, x.status = .moved → ∀ l ∈ t.loans, x.place ≠ l.root)
      (move s p) := by
  cases h : move s p with
  | none => trivial
  | some t =>
    intro x hx hstatus l hl
    have hwf : StateResult (move s p) := move_preserves s p hs
    rw [h] at hwf
    exact moved_no_loan_of_wf t hwf x hx hstatus l hl

/-- Helper (computation). Projects the concrete callback result; execution-level agreement is established independently by Steps_erasure. (approved: APPR-C5-codex-902ce88a). -/
theorem instrument_erasure {σ α : Type} (permit : State → Option State)
    (operation : σ → Option (α × σ)) (s : Store σ) :
    ResultSatisfies (fun r : α × Store σ => operation (erase s) = some (r.1, erase r.2))
      (instrument permit operation s) := by
  cases h : instrument permit operation s with
  | none => trivial
  | some r => exact instrument_erasure_result permit operation s r.1 r.2 h

/-- Helper (computation). Calculates the ownership-only lifecycle wrapper; cleanup safety is established by leave_preserves and leave_no_activation_slot. (approved: APPR-C5-codex-902ce88a). -/
theorem life_erasure {σ : Type} (types : Var → Option Descriptor)
    (e : Exec.Life) (s : Store σ) :
    ResultSatisfies (fun t : Store σ => erase t = erase s ∧ life types e s.own = some t.own)
      (lifeStore types e s) := by
  cases h : lifeStore types e s with
  | none => trivial
  | some t => exact life_erasure_result types e s t h

/-- Helper (computation). Calculates the call lifecycle wrapper; directOwner_roundtrip and deferred_call_lifetime establish the call protocols. (approved: APPR-C5-codex-902ce88a). -/
theorem callLife_erasure {σ : Type} (types : Var → Option Descriptor)
    (e : Exec.Life) (s : σ × CallState) :
    ResultSatisfies (fun t : σ × CallState => t.1 = s.1 ∧ callLife types e s.2 = some t.2)
      (callLifeStore types e s) := by
  cases h : callLifeStore types e s with
  | none => trivial
  | some t => exact callLife_erasure_result types e s t h

/-- Helper (computation). Transports a supplied preservation proof through the generic callback wrapper; it supplies no new soundness premise. (approved: APPR-C5-codex-902ce88a). -/
theorem instrument_preserves {σ α : Type} (permit : State → Option State)
    (hp : ∀ s t, WellFormed s → permit s = some t → WellFormed t)
    (operation : σ → Option (α × σ)) (s : Store σ) (hs : WellFormed s.own) :
    ResultSatisfies (fun r : α × Store σ => WellFormed r.2.own)
      (instrument permit operation s) := by
  cases h : instrument permit operation s with
  | none => trivial
  | some r => exact instrument_preserves_result permit hp operation s r.1 r.2 hs h

/-- Helper (computation). Calculates the scalar read wrapper; Hybrid supplies concrete storage semantics and Steps_erasure supplies C1 execution agreement. (approved: APPR-C5-codex-902ce88a). -/
theorem readOwner_erasure (tgt : Target) (p : Hybrid.Place) (s : Store Hybrid.State) :
    ResultSatisfies (fun r : Hybrid.Value × Store Hybrid.State =>
      s.concrete.read tgt p = some r.1 ∧ r.2.concrete = s.concrete)
      (readOwner tgt p s) := by
  cases h : readOwner tgt p s with
  | none => trivial
  | some r => exact readOwner_erasure_result tgt p s r.2 r.1 h

/-- Helper (computation). Calculates the scalar write wrapper; Hybrid supplies concrete write semantics and Steps_erasure supplies C1 execution agreement. (approved: APPR-C5-codex-902ce88a). -/
theorem writeOwner_erasure (tgt : Target) (p : Hybrid.Place) (v : Hybrid.Value)
    (s : Store Hybrid.State) :
    ResultSatisfies (fun r : Unit × Store Hybrid.State =>
      s.concrete.write tgt p v = some r.2.concrete)
      (writeOwner tgt p v s) := by
  cases h : writeOwner tgt p v s with
  | none => trivial
  | some r => exact writeOwner_erasure_result tgt p s r.2 v h

/-- Helper (computation). Calculates the empty-projection loan read wrapper; live roots and lending scope are established by the loan protocol laws. (approved: APPR-C5-codex-902ce88a). -/
theorem readLoan_erasure (tgt : Target) (i : LoanId) (s : Store Hybrid.State) :
    ResultSatisfies (fun r : Hybrid.Value × Store Hybrid.State =>
      ∃ l, loan s.own i = some l ∧ l.path = [] ∧
        s.concrete.read tgt l.root = some r.1 ∧ r.2.concrete = s.concrete)
      (readLoan tgt i s) := by
  cases h : readLoan tgt i s with
  | none => trivial
  | some r => exact readLoan_erasure_result tgt i s r.2 r.1 h

/-- Helper (computation). Calculates the empty-projection loan write wrapper; conflict soundness is carried by the loan invariant and scoped protocols. (approved: APPR-C5-codex-902ce88a). -/
theorem writeLoan_erasure (tgt : Target) (i : LoanId) (v : Hybrid.Value)
    (s : Store Hybrid.State) :
    ResultSatisfies (fun r : Unit × Store Hybrid.State =>
      ∃ l, loan s.own i = some l ∧ l.path = [] ∧
        s.concrete.write tgt l.root v = some r.2.concrete)
      (writeLoan tgt i v s) := by
  cases h : writeLoan tgt i v s with
  | none => trivial
  | some r => exact writeLoan_erasure_result tgt i s r.2 v h

/-- Helper (computation). Calculates the inserted loan record; borrow_preserves establishes root validity; ended_loans_never_revive establishes identity non-reuse across loan executions. (approved: APPR-C5-codex-902ce88a). -/
theorem borrow_birth (s : State) (p : Hybrid.Place)
    (path : List Projection) (m : Mode) :
    ResultSatisfies (fun t : State =>
      t.nextLoan = s.nextLoan + 1 ∧
        ∃ l ∈ t.loans, l.id = ⟨s.nextLoan⟩ ∧ l.root = p ∧ l.path = path)
      (borrow s p path m) := by
  cases h : borrow s p path m with
  | none => trivial
  | some t => exact borrow_birth_result s t p path m h

/-- Helper (computation). Calculates the inserted child record; downward_reborrow_protocol establishes that the fresh child ends without consuming its predecessors. (approved: APPR-C5-codex-902ce88a). -/
theorem reborrow_birth (s : State) (i : LoanId)
    (suffix : List Projection) (m : Mode) (activation : Nat) :
    ResultSatisfies (fun t : State =>
      t.nextLoan = s.nextLoan + 1 ∧ ∃ a l, loan s i = some a ∧ l ∈ t.loans ∧
        l.id = ⟨s.nextLoan⟩ ∧ l.root = a.root ∧ l.path = a.path ++ suffix ∧
        l.parent = some i ∧ a ∈ t.loans)
      (reborrowTo s i suffix m activation) := by
  cases h : reborrowTo s i suffix m activation with
  | none => trivial
  | some t => exact reborrow_birth_result s t i suffix m activation h

/-- Helper (computation). Calculates the unchanged supply field; ended_loans_never_revive establishes identity non-reuse after ending and reborrowing. (approved: APPR-C5-codex-902ce88a). -/
theorem endLoan_supply (s : State) (i : LoanId) :
    ResultSatisfies (fun t : State => t.nextLoan = s.nextLoan)
      (endLoan s i) := by
  cases h : endLoan s i with
  | none => trivial
  | some t => exact endLoan_supply_result s t i h

/-- Helper (computation). Extracts the argument marks checked during adoption; callLife_call_preserves and deferred_call_lifetime establish the cross-call protocol. (approved: APPR-C5-codex-902ce88a). -/
theorem adoptDown_only_arguments (s : State) (args : List (Var × LoanId)) :
    ResultSatisfies (fun t : State =>
      (∀ e ∈ args, ∃ l ∈ s.loans, l.id = e.2 ∧ l.pendingArgument = true) ∧
      (∀ l ∈ t.loans, l.id ∈ args.map Prod.snd → l.pendingArgument = false) ∧
      (∀ l ∈ t.loans, l.id ∉ args.map Prod.snd → l ∈ s.loans))
      (adoptDown s args) := by
  cases h : adoptDown s args with
  | none => trivial
  | some t =>
    refine ⟨adoptDown_only_arguments_result s t args h, ?_, ?_⟩
    · unfold adoptDown at h
      obtain ⟨f, _, h⟩ := Option.bind_eq_some_iff.mp h
      split at h
      · cases h
        intro l hl harg
        obtain ⟨l₀, _, rfl⟩ := List.mem_map.mp hl
        by_cases hmem : l₀.id ∈ List.map Prod.snd args
        · simp [hmem]
        · simp [hmem] at harg
      · contradiction
    · unfold adoptDown at h
      obtain ⟨f, _, h⟩ := Option.bind_eq_some_iff.mp h
      split at h
      · cases h
        intro l hl hnarg
        obtain ⟨l₀, hl₀, rfl⟩ := List.mem_map.mp hl
        by_cases hmem : l₀.id ∈ List.map Prod.snd args
        · simp [hmem] at hnarg
        · simpa [hmem] using hl₀
      · contradiction

/-- Helper (computation). Extracts the destination guard; downward_reborrow_protocol exercises both current-frame success and exact upward refusal. (approved: APPR-C5-codex-902ce88a). -/
theorem reborrowTo_current (s : State) (i : LoanId) (suffix : List Projection)
    (m : Mode) (activation : Nat) :
    ResultSatisfies (fun t : State =>
      ∃ f ∈ t.frames, f.id = activation ∧ t.frames.head? = some f ∧
        ∃ l ∈ t.loans, l.id = ⟨s.nextLoan⟩ ∧ l.activation = f.id)
      (reborrowTo s i suffix m activation) := by
  cases h : reborrowTo s i suffix m activation with
  | none => trivial
  | some t =>
    obtain ⟨f, hf, hact, hhead⟩ := reborrowTo_current_result s t i suffix m activation h
    unfold reborrowTo at h
    obtain ⟨a, _, h⟩ := Option.bind_eq_some_iff.mp h
    split at h
    · cases h
      exact ⟨f, hf, hact, hhead, _, List.mem_cons_self, rfl, hact.symm⟩
    · contradiction

/-- Helper (computation). Calculates the call-root record; directOwner_roundtrip and deferred_call_lifetime establish its lifetime. (approved: APPR-C5-codex-902ce88a). -/
theorem passOwner_birth (s : CallState) (p : Hybrid.Place)
    (path : List Projection) (m : Mode) (x : Var) :
    ResultSatisfies (fun t : CallState =>
      ∃ l, t.own.loans = l :: s.own.loans ∧ l.id = LoanId.mk s.own.nextLoan ∧
        l.root = p ∧ l.path = path ∧ l.mode = m ∧ l.parent = none ∧
        l.pendingArgument = true ∧ l.callRoot = true)
      (passOwner s p path m x) := by
  cases h : passOwner s p path m x with
  | none => trivial
  | some t => exact passOwner_birth_result s t p path m x h

/-- Helper (computation). Computes a registration step; deferred_call_lifetime establishes retention through invocation and restoration on actual return. (approved: APPR-C5-codex-902ce88a). -/
theorem defer_keeps_store {O : Exec.Operands} (P : Exec.Program O)
    (f : FnId) (args : List O.Expr) (site : SiteId)
    (L : AList Var O.Val) (s : O.Store) (k : List (Exec.Frame O)) :
    match Exec.step P ⟨.exec (.defer f args site), L, s, k⟩ with
    | some (.next c) => c.store = s
    | _ => True := by
  cases h : Exec.step P ⟨.exec (.defer f args site), L, s, k⟩ with
  | none => trivial
  | some out =>
    cases out with
    | next c => exact defer_keeps_store_result O P f args site L s k c h
    | request _ _ _ _ _ => trivial

namespace RestatedLawEvidence
open Examples
open ConcreteSpec.Core.Mem

/-- Mutant: move spuriously refuses all inputs. -/
def moveRefuse (_ : State) (_ : Hybrid.Place) : Option State := none
/-- Mutant: borrow spuriously refuses all inputs. -/
def borrowRefuse (_ : State) (_ : Hybrid.Place) (_ : List Projection) (_ : Mode) : Option State := none
/-- Mutant: reborrowTo spuriously refuses all inputs. -/
def reborrowToRefuse (_ : State) (_ : LoanId) (_ : List Projection) (_ : Mode) (_ : Nat) : Option State := none
/-- Mutant: reborrow spuriously refuses all inputs. -/
def reborrowRefuse (_ : State) (_ : LoanId) (_ : List Projection) (_ : Mode) : Option State := none
/-- Mutant: accessOwner spuriously refuses all inputs. -/
def accessOwnerRefuse (_ : State) (_ : Hybrid.Place) (_ : Mode) : Option State := none
/-- Mutant: accessLoan spuriously refuses all inputs. -/
def accessLoanRefuse (_ : State) (_ : LoanId) (_ : Mode) : Option State := none
/-- Mutant: reinitialize spuriously refuses all inputs. -/
def reinitializeRefuse (_ : State) (_ : Hybrid.Place) : Option State := none
/-- Mutant: endLoan spuriously refuses all inputs. -/
def endLoanRefuse (_ : State) (_ : LoanId) : Option State := none
/-- Mutant: bind spuriously refuses all inputs. -/
def bindRefuse (_ : Var → Option Descriptor) (_ : Var) (_ : State) : Option State := none
/-- Mutant: drop spuriously refuses all inputs. -/
def dropRefuse (_ : List Var) (_ : State) : Option State := none

/-- Mutant: borrow marks the borrowed root slot `.moved`, breaking `loan_has_owned_root`. -/
def borrowConsumeRoot (s : State) (p : Hybrid.Place) (path : List Projection) (m : Mode) : Option State := do
  let t ← borrow s p path m
  some { t with slots := t.slots.map fun x =>
    if x.place == p then { x with status := .moved } else x }

/-- Mutant: move marks `p` as `.moved` without checking whether active loans borrow `p`,
breaking `moved_no_loan`. -/
def moveIgnoreLoans (s : State) (p : Hybrid.Place) : Option State := do
  let x ← slot s p
  if x.desc.copy || x.status == .moved then none else
    some { s with slots := s.slots.map fun y =>
      if y.place == p then { y with status := .moved } else y }

/-- Mutant: instrument updates the ownership projection but discards the concrete store update
from `operation`, returning the old `s.concrete`. -/
def instrumentKeepOldConcrete {σ α : Type} (permit : State → Option State)
    (operation : σ → Option (α × σ)) (s : Store σ) : Option (α × Store σ) := do
  let own ← permit s.own
  let (a, _) ← operation s.concrete
  some (a, ⟨s.concrete, own⟩)

/-- Mutant: instrument corrupts the ownership projection by assigning an unissued place identity. -/
def instrumentUnissuedPlace {σ α : Type} (permit : State → Option State)
    (operation : σ → Option (α × σ)) (s : Store σ) : Option (α × Store σ) := do
  let (a, t) ← instrument permit operation s
  some (a, ⟨t.concrete, { t.own with slots := t.own.slots.map fun x =>
    { x with place := ⟨t.own.nextPlace⟩ } }⟩)

/-- Mutant: lifeStore returns the store unchanged without applying the ownership lifecycle step. -/
def lifeStoreNoOp {σ : Type} (_ : Var → Option Descriptor) (_ : Exec.Life) (s : Store σ) : Option (Store σ) :=
  some s

/-- Mutant: callLifeStore returns the pair unchanged without applying `callLife`. -/
def callLifeStoreNoOp {σ : Type} (_ : Var → Option Descriptor) (_ : Exec.Life) (s : σ × CallState) : Option (σ × CallState) :=
  some s

/-- Mutant: readOwner clears `concrete.cells`, mutating the concrete store during a read. -/
def readOwnerCorruptConcrete (tgt : Target) (p : Hybrid.Place)
    (s : Store Hybrid.State) : Option (Hybrid.Value × Store Hybrid.State) := do
  let (v, t) ← readOwner tgt p s
  some (v, ⟨⟨[], t.concrete.memory⟩, t.own⟩)

/-- Mutant: writeOwner checks permission but leaves `s.concrete` unchanged. -/
def writeOwnerNoOpConcrete (tgt : Target) (p : Hybrid.Place) (v : Hybrid.Value)
    (s : Store Hybrid.State) : Option (Unit × Store Hybrid.State) := do
  let (_, t) ← writeOwner tgt p v s
  some ((), ⟨s.concrete, t.own⟩)

/-- Mutant: readLoan clears `concrete.cells`, mutating the concrete store during a loan read. -/
def readLoanCorruptConcrete (tgt : Target) (i : LoanId)
    (s : Store Hybrid.State) : Option (Hybrid.Value × Store Hybrid.State) := do
  let (v, t) ← readLoan tgt i s
  some (v, ⟨⟨[], t.concrete.memory⟩, t.own⟩)

/-- Mutant: writeLoan checks permission on loan `i` but leaves `s.concrete` unchanged. -/
def writeLoanNoOpConcrete (tgt : Target) (i : LoanId) (v : Hybrid.Value)
    (s : Store Hybrid.State) : Option (Unit × Store Hybrid.State) := do
  let (_, t) ← writeLoan tgt i v s
  some ((), ⟨s.concrete, t.own⟩)

/-- Mutant: endLoan resets `nextLoan` to `0`, allowing retired loan IDs to be reissued. -/
def endLoanRewindSupply (s : State) (i : LoanId) : Option State := do
  let t ← endLoan s i
  some { t with nextLoan := 0 }

/-- Mutant: adoptDown returns `s` unchanged without clearing `pendingArgument` on adopted loans. -/
def adoptDownNoOp (s : State) (_ : List (Var × LoanId)) : Option State :=
  some s

/-- Mutant: reborrowTo places the newly issued loan in inactive activation `activation + 1`. -/
def reborrowToInactive (s : State) (i : LoanId) (suffix : List Projection)
    (m : Mode) (activation : Nat) : Option State := do
  let t ← reborrowTo s i suffix m activation
  some { t with loans := t.loans.map fun l =>
    if l.id == ⟨s.nextLoan⟩ then { l with activation := activation + 1 } else l }

/-- Mutant: `step` on `.defer` eagerly evaluates the deferred call's arguments at registration
time instead of waiting until scope exit (violating Q9.8 / OD2), mutating `c.store`. -/
def stepDeferEagerArgs {O : Exec.Operands} (P : Exec.Program O) (c : Exec.Config O) : Option (Exec.Out O) :=
  match c.ctrl with
  | .exec (.defer f args site) =>
    match Exec.evalArgs O c.locals c.store args with
    | .ok (_, st') =>
      (Exec.register ⟨f, args, site⟩ c.frames).map fun k' =>
        .next { c with ctrl := .done, store := st', frames := k' }
    | _ => none
  | _ => Exec.step P c

def deferCallerConfig : Exec.Config callOps :=
  ⟨.exec (.defer callee
      [.ownerArgument ⟨1⟩ .mutable DirectArgumentWitnesses.z] ⟨0⟩),
   [], DirectArgumentWitnesses.caller, [.scope ⟨0⟩ [] []]⟩

def intStore0 : Store Int := ⟨10, initial⟩
def intCallStore0 : Int × CallState := (10, CallWitnesses.waiting)
def incIntOp (n : Int) : Option (Int × Int) := some (n, n + 1)
def sharedPermit (s : State) : Option State := accessOwner s ⟨0⟩ .shared

def hybridCell0 : Hybrid.State :=
  ⟨[(⟨0⟩, ⟨.int .i32, .structured (.int .i32 7)⟩)], Memory.empty⟩

def wholeShared : State := (borrow initial ⟨0⟩ [] .shared).getD initial
def wholeExclusive : State := (borrow initial ⟨0⟩ [] .mutable).getD initial

def ownerStore0 : Store Hybrid.State := ⟨hybridCell0, initial⟩
def sharedLoanStore0 : Store Hybrid.State := ⟨hybridCell0, wholeShared⟩
def exclusiveLoanStore0 : Store Hybrid.State := ⟨hybridCell0, wholeExclusive⟩

abbrev descI32 : Descriptor := ⟨.int .i32, false, true⟩
abbrev slot0Owned : Slot := ⟨⟨0⟩, 0, descI32, .owned⟩
abbrev slot0Moved : Slot := ⟨⟨0⟩, 0, descI32, .moved⟩
abbrev frame0 : Activation := ⟨0, [(x, ⟨0⟩)], []⟩
abbrev sharedLoan0 : Loan := ⟨⟨0⟩, ⟨0⟩, [.index 0], .shared, none, [], 0, false, false⟩
abbrev exclusiveLoan0 : Loan := ⟨⟨0⟩, ⟨0⟩, [.index 0], .mutable, none, [], 0, false, false⟩

-- Progress laws (10)

/-- Computation observation. -/
example : ∃ t, move initial ⟨0⟩ = some t :=
  @move_progress initial ⟨0⟩ slot0Owned rfl rfl rfl (by decide)

/-- Computation observation. -/
example : slot initial ⟨0⟩ = some slot0Owned ∧
    slot0Owned.desc.copy = false ∧
    slot0Owned.status = .owned ∧
    (∀ l ∈ initial.loans, l.root ≠ ⟨0⟩) ∧
    ¬ (∃ t, moveRefuse initial ⟨0⟩ = some t) :=
  ⟨rfl, rfl, rfl, by decide, by simp [moveRefuse]⟩

/-- Computation observation. -/
example : ∃ t, borrow initial ⟨0⟩ [.index 1] .shared = some t :=
  @borrow_progress initial ⟨0⟩ [.index 1] .shared frame0 rfl (by decide) (by decide)

/-- Computation observation. -/
example : initial.frames.head? = some frame0 ∧
    frame0.id < initial.nextActivation ∧
    ownerAccess initial ⟨0⟩ .shared = true ∧
    ¬ (∃ t, borrowRefuse initial ⟨0⟩ [.index 1] .shared = some t) :=
  ⟨rfl, by decide, by decide, by simp [borrowRefuse]⟩

/-- Computation observation. -/
example : ∃ t, reborrowTo exclusive ⟨0⟩ [.index 1] .mutable 0 = some t :=
  @reborrowTo_progress exclusive ⟨0⟩ [.index 1] .mutable 0
    exclusiveLoan0 rfl (by decide) rfl (by decide)

/-- Computation observation. -/
example : loan exclusive ⟨0⟩ = some exclusiveLoan0 ∧
    0 < exclusive.nextActivation ∧
    exclusive.frames.head?.map Activation.id = some 0 ∧
    loanAccess exclusive ⟨0⟩ .mutable = true ∧
    ¬ (∃ t, reborrowToRefuse exclusive ⟨0⟩ [.index 1] .mutable 0 = some t) :=
  ⟨rfl, by decide, rfl, by decide, by simp [reborrowToRefuse]⟩

/-- Computation observation. -/
example : ∃ t, reborrow exclusive ⟨0⟩ [.index 1] .mutable = some t :=
  @reborrow_progress exclusive ⟨0⟩ [.index 1] .mutable
    frame0 rfl (by decide) exclusiveLoan0 rfl (by decide)

/-- Computation observation. -/
example : exclusive.frames.head? = some frame0 ∧
    frame0.id < exclusive.nextActivation ∧
    loan exclusive ⟨0⟩ = some exclusiveLoan0 ∧
    loanAccess exclusive ⟨0⟩ .mutable = true ∧
    ¬ (∃ t, reborrowRefuse exclusive ⟨0⟩ [.index 1] .mutable = some t) :=
  ⟨rfl, by decide, rfl, by decide, by simp [reborrowRefuse]⟩

/-- Computation observation. -/
example : accessOwner initial ⟨0⟩ .shared = some initial :=
  @accessOwner_progress initial ⟨0⟩ .shared (by decide)

/-- Computation observation. -/
example : ownerAccess initial ⟨0⟩ .shared = true ∧
    ¬ (accessOwnerRefuse initial ⟨0⟩ .shared = some initial) := by decide

/-- Computation observation. -/
example : accessLoan shared ⟨0⟩ .shared = some shared :=
  @accessLoan_progress shared ⟨0⟩ .shared (by decide)

/-- Computation observation. -/
example : loanAccess shared ⟨0⟩ .shared = true ∧
    ¬ (accessLoanRefuse shared ⟨0⟩ .shared = some shared) := by decide

/-- Computation observation. -/
example : ∃ t, reinitialize LifecycleWitnesses.moved ⟨0⟩ = some t :=
  @reinitialize_progress LifecycleWitnesses.moved ⟨0⟩ slot0Moved rfl (Or.inl rfl) (by decide)

/-- Computation observation. -/
example : slot LifecycleWitnesses.moved ⟨0⟩ = some slot0Moved ∧
    (slot0Moved.status = .moved ∨ (slot0Moved.desc.copy = true ∧ slot0Moved.desc.mutable = true)) ∧
    (∀ l ∈ LifecycleWitnesses.moved.loans, l.root ≠ ⟨0⟩) ∧
    ¬ (∃ t, reinitializeRefuse LifecycleWitnesses.moved ⟨0⟩ = some t) :=
  ⟨rfl, Or.inl rfl, by decide, by simp [reinitializeRefuse]⟩

/-- Computation observation. -/
example : ∃ t, endLoan shared ⟨0⟩ = some t :=
  @endLoan_progress shared ⟨0⟩ sharedLoan0 rfl (by decide)

/-- Computation observation. -/
example : loan shared ⟨0⟩ = some sharedLoan0 ∧
    (∀ b ∈ shared.loans, b.parent ≠ some ⟨0⟩) ∧
    ¬ (∃ t, endLoanRefuse shared ⟨0⟩ = some t) :=
  ⟨rfl, by decide, by simp [endLoanRefuse]⟩

/-- Computation observation. -/
example : ∃ t, bind types y initial = some t :=
  @bind_fresh_progress types initial y frame0 descI32 rfl (by decide) rfl rfl

/-- Computation observation. -/
example : initial.frames.head? = some frame0 ∧
    frame0.id < initial.nextActivation ∧
    AList.lookup frame0.locals y = none ∧
    types y = some descI32 ∧
    ¬ (∃ t, bindRefuse types y initial = some t) :=
  ⟨rfl, by decide, rfl, rfl, by simp [bindRefuse]⟩

/-- Computation observation. -/
example : ∃ t, drop [x] LifecycleWitnesses.moved = some t :=
  @drop_progress LifecycleWitnesses.moved [x] frame0 rfl (by decide) (by decide)

/-- Computation observation. -/
example : LifecycleWitnesses.moved.frames.head? = some frame0 ∧
    (∀ a ∈ LifecycleWitnesses.moved.slots,
      a.place ∈ (frame0.locals.filter (fun e => decide (e.1 ∈ [x]))).map Prod.snd →
      a.desc.copy = true ∨ a.status = .moved) ∧
    (∀ l ∈ LifecycleWitnesses.moved.loans,
      l.root ∉ (frame0.locals.filter (fun e => decide (e.1 ∈ [x]))).map Prod.snd) ∧
    ¬ (∃ t, dropRefuse [x] LifecycleWitnesses.moved = some t) :=
  ⟨rfl, by decide, by decide, by simp [dropRefuse]⟩

-- Domain evidence and internal wrapper computations

/-- Instance (law `ConcreteSpec.Core.Own.loan_has_owned_root`, case `ConcreteSpec.Core.Own.Examples.initial`). -/
example : ResultSatisfies (fun t : State => ∀ l ∈ t.loans, ∃ x ∈ t.slots, x.place = l.root ∧ x.status = .owned)
    (borrow initial ⟨0⟩ [.index 1] .shared) :=
  @loan_has_owned_root initial ⟨0⟩ [.index 1] .shared (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.loan_has_owned_root`, case `ConcreteSpec.Core.Own.Examples.initial`, subject `ConcreteSpec.Core.Own.borrow`, mutant `ConcreteSpec.Core.Own.RestatedLawEvidence.borrowConsumeRoot`). -/
example : WellFormed initial ∧
    ¬ ResultSatisfies (fun t : State => ∀ l ∈ t.loans, ∃ x ∈ t.slots, x.place = l.root ∧ x.status = .owned)
      (borrowConsumeRoot initial ⟨0⟩ [.index 1] .shared) := by decide

/-- A movable owner coexists with another root's live loan. -/
def moveWithOtherLoan : State := DirectArgumentWitnesses.caller.own

/-- Mutant: moving creates a loan of the moved root. -/
def moveLeak (s : State) (p : Hybrid.Place) : Option State :=
  (move s p).map fun t => { t with loans :=
    ⟨⟨s.nextLoan⟩, p, [], .shared, none, [], 0, false, false⟩ :: t.loans }

/-- Instance (law `ConcreteSpec.Core.Own.moved_no_loan`, case `ConcreteSpec.Core.Own.RestatedLawEvidence.moveWithOtherLoan`). -/
example : ResultSatisfies (fun t : State => ∀ x ∈ t.slots, x.status = .moved → ∀ l ∈ t.loans, x.place ≠ l.root)
    (move moveWithOtherLoan ⟨1⟩) :=
  @moved_no_loan moveWithOtherLoan ⟨1⟩ (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.moved_no_loan`, case `ConcreteSpec.Core.Own.RestatedLawEvidence.moveWithOtherLoan`, subject `ConcreteSpec.Core.Own.move`, mutant `ConcreteSpec.Core.Own.RestatedLawEvidence.moveLeak`). -/
example : WellFormed moveWithOtherLoan ∧
    ¬ ResultSatisfies (fun t : State => ∀ x ∈ t.slots, x.status = .moved → ∀ l ∈ t.loans, x.place ≠ l.root)
      (moveLeak moveWithOtherLoan ⟨1⟩) := by decide

/-- Computation observation. -/
example : ResultSatisfies (fun r : Int × Store Int => incIntOp (erase intStore0) = some (r.1, erase r.2))
    (instrument sharedPermit incIntOp intStore0) :=
  @instrument_erasure Int Int sharedPermit incIntOp intStore0

/-- Computation observation. -/
example : ¬ ResultSatisfies (fun r : Int × Store Int => incIntOp (erase intStore0) = some (r.1, erase r.2))
    (instrumentKeepOldConcrete sharedPermit incIntOp intStore0) := by decide

/-- Computation observation. -/
example : ResultSatisfies (fun t : Store Int => erase t = erase intStore0 ∧ life types (.bind y) intStore0.own = some t.own)
    (lifeStore types (.bind y) intStore0) :=
  @life_erasure Int types (.bind y) intStore0

/-- Computation observation. -/
example : ¬ ResultSatisfies (fun t : Store Int => erase t = erase intStore0 ∧ life types (.bind y) intStore0.own = some t.own)
    (lifeStoreNoOp types (.bind y) intStore0) := by decide

/-- Computation observation. -/
example : ResultSatisfies (fun t : Int × CallState => t.1 = intCallStore0.1 ∧ callLife types (.enter [y]) intCallStore0.2 = some t.2)
    (callLifeStore types (.enter [y]) intCallStore0) :=
  @callLife_erasure Int types (.enter [y]) intCallStore0

/-- Computation observation. -/
example : ¬ ResultSatisfies (fun t : Int × CallState => t.1 = intCallStore0.1 ∧ callLife types (.enter [y]) intCallStore0.2 = some t.2)
    (callLifeStoreNoOp types (.enter [y]) intCallStore0) := by decide

/-- Computation observation. -/
example : ResultSatisfies (fun r : Int × Store Int => WellFormed r.2.own)
    (instrument sharedPermit incIntOp intStore0) :=
  @instrument_preserves Int Int sharedPermit
    (fun s t hs ht => accessOwner_preserves_result s t ⟨0⟩ .shared hs ht)
    incIntOp intStore0 (by decide)

/-- Computation observation. -/
example : (∀ s t, WellFormed s → sharedPermit s = some t → WellFormed t) ∧
    WellFormed intStore0.own ∧
    ¬ ResultSatisfies (fun r : Int × Store Int => WellFormed r.2.own)
      (instrumentUnissuedPlace sharedPermit incIntOp intStore0) :=
  ⟨fun s t hs ht => accessOwner_preserves_result s t ⟨0⟩ .shared hs ht, by decide, by decide⟩

/-- Computation observation. -/
example : ResultSatisfies (fun r : Hybrid.Value × Store Hybrid.State =>
      ownerStore0.concrete.read linuxX8664 ⟨0⟩ = some r.1 ∧ r.2.concrete = ownerStore0.concrete)
    (readOwner linuxX8664 ⟨0⟩ ownerStore0) :=
  @readOwner_erasure linuxX8664 ⟨0⟩ ownerStore0

/-- Computation observation. -/
example : ¬ ResultSatisfies (fun r : Hybrid.Value × Store Hybrid.State =>
      ownerStore0.concrete.read linuxX8664 ⟨0⟩ = some r.1 ∧ r.2.concrete = ownerStore0.concrete)
    (readOwnerCorruptConcrete linuxX8664 ⟨0⟩ ownerStore0) := by
  rintro ⟨_, h⟩
  have hc := congrArg (fun st : Hybrid.State => st.cells.length) h
  cases hc

/-- Computation observation. -/
example : ResultSatisfies (fun r : Unit × Store Hybrid.State =>
      ownerStore0.concrete.write linuxX8664 ⟨0⟩ (.int .i32 19) = some r.2.concrete)
    (writeOwner linuxX8664 ⟨0⟩ (.int .i32 19) ownerStore0) :=
  @writeOwner_erasure linuxX8664 ⟨0⟩ (.int .i32 19) ownerStore0

/-- Computation observation. -/
example : ¬ ResultSatisfies (fun r : Unit × Store Hybrid.State =>
      ownerStore0.concrete.write linuxX8664 ⟨0⟩ (.int .i32 19) = some r.2.concrete)
    (writeOwnerNoOpConcrete linuxX8664 ⟨0⟩ (.int .i32 19) ownerStore0) := by
  intro h
  have hc := congrArg (fun ost : Option Hybrid.State => ost.bind (fun st => st.read linuxX8664 ⟨0⟩)) h
  revert hc
  decide

/-- Computation observation. -/
example : ResultSatisfies (fun r : Hybrid.Value × Store Hybrid.State =>
      ∃ l, loan sharedLoanStore0.own ⟨0⟩ = some l ∧ l.path = [] ∧
        sharedLoanStore0.concrete.read linuxX8664 l.root = some r.1 ∧
        r.2.concrete = sharedLoanStore0.concrete)
    (readLoan linuxX8664 ⟨0⟩ sharedLoanStore0) :=
  @readLoan_erasure linuxX8664 ⟨0⟩ sharedLoanStore0

/-- Computation observation. -/
example : ¬ ResultSatisfies (fun r : Hybrid.Value × Store Hybrid.State =>
      ∃ l, loan sharedLoanStore0.own ⟨0⟩ = some l ∧ l.path = [] ∧
        sharedLoanStore0.concrete.read linuxX8664 l.root = some r.1 ∧
        r.2.concrete = sharedLoanStore0.concrete)
    (readLoanCorruptConcrete linuxX8664 ⟨0⟩ sharedLoanStore0) := by
  rintro ⟨_, _, _, _, h⟩
  have hc := congrArg (fun st : Hybrid.State => st.cells.length) h
  cases hc

/-- Computation observation. -/
example : ResultSatisfies (fun r : Unit × Store Hybrid.State =>
      ∃ l, loan exclusiveLoanStore0.own ⟨0⟩ = some l ∧ l.path = [] ∧
        exclusiveLoanStore0.concrete.write linuxX8664 l.root (.int .i32 23) = some r.2.concrete)
    (writeLoan linuxX8664 ⟨0⟩ (.int .i32 23) exclusiveLoanStore0) :=
  @writeLoan_erasure linuxX8664 ⟨0⟩ (.int .i32 23) exclusiveLoanStore0

/-- Computation observation. -/
example : ¬ ResultSatisfies (fun r : Unit × Store Hybrid.State =>
      ∃ l, loan exclusiveLoanStore0.own ⟨0⟩ = some l ∧ l.path = [] ∧
        exclusiveLoanStore0.concrete.write linuxX8664 l.root (.int .i32 23) = some r.2.concrete)
    (writeLoanNoOpConcrete linuxX8664 ⟨0⟩ (.int .i32 23) exclusiveLoanStore0) := by
  rintro ⟨l, hl, _, hw⟩
  have hl' : l = ⟨⟨0⟩, ⟨0⟩, [], .mutable, none, [], 0, false, false⟩ :=
    Option.some.inj (hl.symm.trans rfl)
  subst hl'
  have hc := congrArg (fun ost : Option Hybrid.State => ost.bind (fun st => st.read linuxX8664 ⟨0⟩)) hw
  revert hc
  decide

/-- Computation observation. -/
example : ResultSatisfies (fun t : State =>
      t.nextLoan = initial.nextLoan + 1 ∧
        ∃ l ∈ t.loans, l.id = ⟨initial.nextLoan⟩ ∧ l.root = ⟨0⟩ ∧ l.path = [.index 1])
    (borrow initial ⟨0⟩ [.index 1] .shared) :=
  @borrow_birth initial ⟨0⟩ [.index 1] .shared

/-- Computation observation. -/
example : ¬ ResultSatisfies (fun t : State =>
      t.nextLoan = initial.nextLoan + 1 ∧
        ∃ l ∈ t.loans, l.id = ⟨initial.nextLoan⟩ ∧ l.root = ⟨0⟩ ∧ l.path = [.index 1])
    (PreservationEvidence.borrowNoAdvanceSupply initial ⟨0⟩ [.index 1] .shared) := by decide

/-- Computation observation. -/
example : ResultSatisfies (fun t : State =>
      t.nextLoan = exclusive.nextLoan + 1 ∧ ∃ a l, loan exclusive ⟨0⟩ = some a ∧ l ∈ t.loans ∧
        l.id = ⟨exclusive.nextLoan⟩ ∧ l.root = a.root ∧ l.path = a.path ++ [.index 1] ∧
        l.parent = some ⟨0⟩ ∧ a ∈ t.loans)
    (reborrowTo exclusive ⟨0⟩ [.index 1] .mutable 0) :=
  @reborrow_birth exclusive ⟨0⟩ [.index 1] .mutable 0

/-- Computation observation. -/
example : ¬ ResultSatisfies (fun t : State =>
      t.nextLoan = exclusive.nextLoan + 1 ∧ ∃ a l, loan exclusive ⟨0⟩ = some a ∧ l ∈ t.loans ∧
        l.id = ⟨exclusive.nextLoan⟩ ∧ l.root = a.root ∧ l.path = a.path ++ [.index 1] ∧
        l.parent = some ⟨0⟩ ∧ a ∈ t.loans)
    (PreservationEvidence.reborrowToDropParent exclusive ⟨0⟩ [.index 1] .mutable 0) := by
  rintro ⟨_, a, _, ha, _, _, _, _, _, ha_mem⟩
  have ha' : a = exclusiveLoan0 := Option.some.inj (ha.symm.trans rfl)
  subst ha'
  revert ha_mem
  decide

/-- Computation observation. -/
example : ResultSatisfies (fun t : State => t.nextLoan = shared.nextLoan)
    (endLoan shared ⟨0⟩) :=
  @endLoan_supply shared ⟨0⟩

/-- Computation observation. -/
example : ¬ ResultSatisfies (fun t : State => t.nextLoan = shared.nextLoan)
    (endLoanRewindSupply shared ⟨0⟩) := by decide

/-- Computation observation. -/
example : ResultSatisfies (fun t : State =>
      (∀ e ∈ DirectArgumentWitnesses.waiting.pending, ∃ l ∈ PreservationEvidence.adoptionInput.loans, l.id = e.2 ∧ l.pendingArgument = true) ∧
      (∀ l ∈ t.loans, l.id ∈ DirectArgumentWitnesses.waiting.pending.map Prod.snd → l.pendingArgument = false) ∧
      (∀ l ∈ t.loans, l.id ∉ DirectArgumentWitnesses.waiting.pending.map Prod.snd → l ∈ PreservationEvidence.adoptionInput.loans))
    (adoptDown PreservationEvidence.adoptionInput DirectArgumentWitnesses.waiting.pending) :=
  @adoptDown_only_arguments PreservationEvidence.adoptionInput DirectArgumentWitnesses.waiting.pending

/-- Computation observation. -/
example : ¬ ResultSatisfies (fun t : State =>
      (∀ e ∈ DirectArgumentWitnesses.waiting.pending, ∃ l ∈ PreservationEvidence.adoptionInput.loans, l.id = e.2 ∧ l.pendingArgument = true) ∧
      (∀ l ∈ t.loans, l.id ∈ DirectArgumentWitnesses.waiting.pending.map Prod.snd → l.pendingArgument = false) ∧
      (∀ l ∈ t.loans, l.id ∉ DirectArgumentWitnesses.waiting.pending.map Prod.snd → l ∈ PreservationEvidence.adoptionInput.loans))
    (adoptDownNoOp PreservationEvidence.adoptionInput DirectArgumentWitnesses.waiting.pending) := by decide

/-- Computation observation. -/
example : ResultSatisfies (fun t : State =>
      ∃ f ∈ t.frames, f.id = 0 ∧ t.frames.head? = some f ∧
        ∃ l ∈ t.loans, l.id = ⟨exclusive.nextLoan⟩ ∧ l.activation = f.id)
    (reborrowTo exclusive ⟨0⟩ [.index 1] .mutable 0) :=
  @reborrowTo_current exclusive ⟨0⟩ [.index 1] .mutable 0

/-- Computation observation. -/
example : ¬ ResultSatisfies (fun t : State =>
      ∃ f ∈ t.frames, f.id = 0 ∧ t.frames.head? = some f ∧
        ∃ l ∈ t.loans, l.id = ⟨exclusive.nextLoan⟩ ∧ l.activation = f.id)
    (reborrowToInactive exclusive ⟨0⟩ [.index 1] .mutable 0) := by decide

/-- Computation observation. -/
example : ResultSatisfies (fun t : CallState =>
      ∃ l, t.own.loans = l :: DirectArgumentWitnesses.caller.own.loans ∧
        l.id = LoanId.mk DirectArgumentWitnesses.caller.own.nextLoan ∧
        l.root = ⟨1⟩ ∧ l.path = [.index 3] ∧ l.mode = .mutable ∧ l.parent = none ∧
        l.pendingArgument = true ∧ l.callRoot = true)
    (passOwner DirectArgumentWitnesses.caller ⟨1⟩ [.index 3] .mutable DirectArgumentWitnesses.z) :=
  @passOwner_birth DirectArgumentWitnesses.caller ⟨1⟩ [.index 3] .mutable DirectArgumentWitnesses.z

/-- Computation observation. -/
example : ¬ ResultSatisfies (fun t : CallState =>
      ∃ l, t.own.loans = l :: DirectArgumentWitnesses.caller.own.loans ∧
        l.id = LoanId.mk DirectArgumentWitnesses.caller.own.nextLoan ∧
        l.root = ⟨1⟩ ∧ l.path = [.index 3] ∧ l.mode = .mutable ∧ l.parent = none ∧
        l.pendingArgument = true ∧ l.callRoot = true)
    (PreservationEvidence.passOwnerClearCallRoot DirectArgumentWitnesses.caller ⟨1⟩ [.index 3] .mutable DirectArgumentWitnesses.z) := by
  rintro ⟨l, hcons, _, _, _, _, _, _, hcr⟩
  injection hcons with hl _
  subst hl
  cases hcr

/-- Computation observation. -/
example : (match Exec.step callProgram deferCallerConfig with
    | some (.next c) => c.store = DirectArgumentWitnesses.caller
    | _ => True) :=
  @defer_keeps_store callOps callProgram callee
    [.ownerArgument ⟨1⟩ .mutable DirectArgumentWitnesses.z] ⟨0⟩
    [] DirectArgumentWitnesses.caller [.scope ⟨0⟩ [] []]

/-- Computation observation. -/
example : ¬ (match stepDeferEagerArgs callProgram deferCallerConfig with
    | some (.next c) => c.store = DirectArgumentWitnesses.caller
    | _ => True) := by
  intro h
  have hstep : stepDeferEagerArgs callProgram deferCallerConfig =
    some (.next ⟨.done, [], (passOwner DirectArgumentWitnesses.caller ⟨1⟩ [] .mutable DirectArgumentWitnesses.z).getD DirectArgumentWitnesses.caller,
      [.scope ⟨0⟩ [] [⟨callee, [.ownerArgument ⟨1⟩ .mutable DirectArgumentWitnesses.z], ⟨0⟩⟩]]⟩) := rfl
  rw [hstep] at h
  revert h
  decide

end RestatedLawEvidence

/-- A reborrow protocol permits a child in the current activation, refuses the caller as a
new destination, and restores the previous loan table when that child ends. The endpoint
comparison includes every pre-existing loan; a caller loan is never consumed implicitly. -/
def DownwardProtocol (lend : State → LoanId → List Projection → Mode → Nat → Option State)
    (s : State) (i : LoanId) (path : List Projection) (current caller : Nat) : Prop :=
  lend s i path .mutable caller = none ∧
    ∃ t, lend s i path .mutable current = some t ∧
      endLoan t ⟨s.nextLoan⟩ = some { s with nextLoan := s.nextLoan + 1 }

/-- Law (subject `ConcreteSpec.Core.Own.reborrowTo`). Downward lending is a complete scoped
protocol, rather than merely an admission guard: the child can be used in the current frame
and ended without consuming any caller loan, while lending back to the caller is refused
(owner Q1, 2026-11-08). -/
theorem downward_reborrow_protocol (s : State) (i : LoanId) (path : List Projection)
    (f : Activation) (caller : Nat) (a : Loan) (hs : WellFormed s)
    (hf : s.frames.head? = some f) (different : f.id ≠ caller)
    (found : loan s i = some a) (allowed : loanAccess s i .mutable = true) :
    DownwardProtocol reborrowTo s i path f.id caller := by
  have fm : f ∈ s.frames := List.mem_of_head? hf
  have issued := (hs.2.2.2.2.2.2.1 f fm).1
  have am : a ∈ s.loans := List.mem_of_find?_eq_some found
  have ai : a.id = i := by simpa using List.find?_some found
  have fresh : ∀ l ∈ s.loans, l.id ≠ LoanId.mk s.nextLoan := by
    intro l hl he
    have hb := (hs.2.2.2.1 l hl).1
    rw [he] at hb
    exact Nat.lt_irrefl _ hb
  have ine : i ≠ LoanId.mk s.nextLoan := by simpa [ai] using fresh a am
  have noChild : ∀ l ∈ s.loans, l.parent ≠ some (LoanId.mk s.nextLoan) := by
    intro l hl he
    have hp := (hs.2.2.2.1 l hl).2.2.2.1
    unfold ParentValid at hp
    rw [he] at hp
    obtain ⟨b, hb, hid, _⟩ := hp
    exact fresh b hb hid
  have children : s.loans.any (·.parent == some (LoanId.mk s.nextLoan)) = false := by
    apply Bool.eq_false_iff.mpr
    intro h
    obtain ⟨l, hl, he⟩ := List.any_eq_true.mp h
    exact noChild l hl (by simpa using he)
  have unchanged : s.loans.filter (·.id != LoanId.mk s.nextLoan) = s.loans := by
    apply List.filter_eq_self.mpr
    intro l hl
    simpa using fresh l hl
  refine ⟨?_, ?_⟩
  · simp [reborrowTo, found, allowed, hf, different]
  · refine ⟨{ s with
        nextLoan := s.nextLoan + 1
        loans := ⟨⟨s.nextLoan⟩, a.root, a.path ++ path, .mutable, some i,
          i :: a.ancestors, f.id, false, false⟩ :: s.loans }, ?_, ?_⟩
    · simp [reborrowTo, found, allowed, hf, issued]
    · simp [endLoan, loan, ine, children, unchanged]

namespace DownwardProtocolEvidence
open Examples

def source : Loan := ⟨⟨1⟩, ⟨0⟩, [.index 0], .mutable, some ⟨0⟩, [⟨0⟩], 1, false, false⟩
def calleeFrame : Activation := ⟨1, [], [(y, ⟨1⟩)]⟩

/-- Mutant: retain ordinary permissions, but allow a new child to target the active caller. -/
def allowUpward (s : State) (i : LoanId) (path : List Projection) (m : Mode)
    (activation : Nat) : Option State := do
  let f ← s.frames.head?
  let t ← reborrowTo s i path m f.id
  if activation ∈ s.frames.map Activation.id then
    some { t with loans := t.loans.map fun l =>
      if l.id = LoanId.mk s.nextLoan then { l with activation := activation } else l }
  else none

/-- Instance (law `ConcreteSpec.Core.Own.downward_reborrow_protocol`, case `ConcreteSpec.Core.Own.LifecycleWitnesses.called`). Current activation 1 refuses caller activation 0. -/
example : DownwardProtocol reborrowTo LifecycleWitnesses.called ⟨1⟩ [] 1 0 :=
  @downward_reborrow_protocol LifecycleWitnesses.called ⟨1⟩ [] calleeFrame 0 source
    (by decide) rfl (by decide) rfl (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.downward_reborrow_protocol`, case `ConcreteSpec.Core.Own.LifecycleWitnesses.called`, subject `ConcreteSpec.Core.Own.reborrowTo`, mutant `ConcreteSpec.Core.Own.DownwardProtocolEvidence.allowUpward`). The exact upward request is now admitted. -/
example : WellFormed LifecycleWitnesses.called ∧
    LifecycleWitnesses.called.frames.head? = some calleeFrame ∧ calleeFrame.id ≠ 0 ∧
    loan LifecycleWitnesses.called ⟨1⟩ = some source ∧
    loanAccess LifecycleWitnesses.called ⟨1⟩ .mutable = true ∧
    ¬ DownwardProtocol allowUpward LifecycleWitnesses.called ⟨1⟩ [] calleeFrame.id 0 := by
  refine ⟨by decide, rfl, by decide, rfl, by decide, ?_⟩
  intro h
  have hn := h.1
  cases hn

end DownwardProtocolEvidence

/-- The actual C1 transition for the direct-argument operand instance. Keeping the transition
as a subject lets the lifetime controls mutate execution without changing its observations. -/
def deferredStep := @Exec.step Examples.callOps

/-- A finite prefix of silent C1 transitions. Deferred calls here have no primitives; a request
or refused step cannot masquerade as a completed prefix. -/
def silentPrefix (advance : Exec.Config Examples.callOps → Option (Exec.Out Examples.callOps)) :
    Nat → Exec.Config Examples.callOps → Option (Exec.Config Examples.callOps)
  | 0, c => some c
  | n + 1, c => do
    let out ← advance c
    match out with
    | .next d => silentPrefix advance n d
    | .request p _ _ _ _ => nomatch p

def heldProgram (parameter : Var) : Exec.Program Examples.callOps :=
  ⟨[(Examples.callee, ⟨[(parameter, ())], (), ⟨1⟩, .skip⟩)]⟩

def heldRegistration (waiting : CallState) : Exec.Config Examples.callOps :=
  ⟨.exec (.defer Examples.callee [.value (.lit 0)] ⟨0⟩), [], waiting,
    [.scope ⟨0⟩ [] []]⟩

def callSnapshot (c : Exec.Config Examples.callOps) : List Slot × List Loan × List (Var × LoanId) :=
  (c.store.own.slots, c.store.own.loans, c.store.pending)

/-- The held loan survives registration and every silent prefix before invocation. Five actual
C1 steps then enter the deferred callee, run its body, clean its scope and return. At return the
caller has its exact original slots and loans, with no pending argument or leaked call loan. -/
def DeferredLifetime
    (advance : Exec.Program Examples.callOps → Exec.Config Examples.callOps → Option (Exec.Out Examples.callOps))
    (s : State) (root : Hybrid.Place) (path : List Projection) (parameter : Var) : Prop :=
  ∃ waiting registered,
    passOwner ⟨s, []⟩ root path .mutable parameter = some waiting ∧
    advance (heldProgram parameter) (heldRegistration waiting) = some (.next registered) ∧
    (∀ n, n ≤ 1 → ∃ c, silentPrefix (advance (heldProgram parameter)) n registered = some c ∧
      (∃ l ∈ c.store.own.loans, l.id = ⟨s.nextLoan⟩ ∧ l.root = root) ∧
      ownerAccess c.store.own root .mutable = false) ∧
    (silentPrefix (advance (heldProgram parameter)) 5 registered).map callSnapshot =
      some (s.slots, s.loans, [])

/-- Helper (inversion). A view-only callee has no owning locals, so its C1 scope drop leaves
its ownership and pending tables unchanged. (approved: APPR-C5-codex-902ce88a). -/
private theorem pending_enter_drop (s t : CallState) (parameter : Var) (i : LoanId)
    (pending : s.pending = [(parameter, i)])
    (h : callLife Examples.types (.enter [parameter]) s = some t) :
    callLife Examples.types (.drop []) t = some t := by
  have check : s.pending.all (fun e => decide (e.1 ∈ [parameter])) = true := by simp [pending]
  have he : enter Examples.types ([parameter].filter fun x => decide (x ∉ s.pending.map Prod.fst)) s.own =
      some { s.own with
        nextActivation := s.own.nextActivation + 1
        frames := ⟨s.own.nextActivation, [], []⟩ :: s.own.frames } := by
    simp [pending, enter]
  simp only [callLife] at h
  dsimp at h
  rw [check] at h
  dsimp at h
  rw [he] at h
  dsimp at h
  unfold adoptDown at h
  dsimp at h
  split at h
  · cases h
    simp [callLife, life, drop]
  · contradiction

/-- Law (subject `ConcreteSpec.Core.Own.deferredStep`). A direct root loan already held at
registration stays live and freezes its owner until invocation; actual C1 deferred execution
ends precisely that call's loan at return and restores every previous caller loan (Q2). -/
theorem deferred_call_lifetime (s : State) (root : Hybrid.Place) (path : List Projection)
    (parameter : Var) (f : Activation) (hs : WellFormed s)
    (hf : s.frames.head? = some f) (allowed : ownerAccess s root .mutable = true) :
    DeferredLifetime deferredStep s root path parameter := by
  have issued := (hs.2.2.2.2.2.2.1 f (List.mem_of_head? hf)).1
  obtain ⟨waiting, hpass⟩ : ∃ waiting, passOwner ⟨s, []⟩ root path .mutable parameter = some waiting := by
    simp [passOwner, hf, allowed, issued]
  have hpending : waiting.pending = [(parameter, ⟨s.nextLoan⟩)] := by
    unfold passOwner at hpass
    simp [hf, allowed, issued] at hpass
    subst waiting
    rfl
  have birth := passOwner_birth_result ⟨s, []⟩ waiting root path .mutable parameter hpass
  obtain ⟨held, hheld, hid, hroot, _⟩ := birth
  have freeze : ownerAccess waiting.own root .mutable = false := by
    have hf0 := passOwner_freezes ⟨s, []⟩ root path .mutable parameter
    rw [hpass] at hf0
    exact hf0.1
  have roundtrip := directOwner_roundtrip s root path .mutable parameter Examples.types f hs hf issued allowed
  rw [hpass] at roundtrip
  dsimp at roundtrip
  obtain ⟨called, henter, hleave⟩ := Option.bind_eq_some_iff.mp roundtrip
  have hdrop := pending_enter_drop waiting called parameter ⟨s.nextLoan⟩ hpending henter
  let registered : Exec.Config Examples.callOps :=
    ⟨.done, [], waiting, [.scope ⟨0⟩ [] [⟨Examples.callee, [.value (.lit 0)], ⟨0⟩⟩]]⟩
  refine ⟨waiting, registered, hpass, rfl, ?_, ?_⟩
  · intro n hn
    have hc : n = 0 ∨ n = 1 := by omega
    rcases hc with rfl | rfl
    · refine ⟨registered, rfl, ?_, freeze⟩
      exact ⟨held, by rw [hheld]; exact List.mem_cons_self, hid, hroot⟩
    · refine ⟨_, rfl, ?_, freeze⟩
      exact ⟨held, by rw [hheld]; exact List.mem_cons_self, hid, hroot⟩
  · simp [silentPrefix, deferredStep, registered, heldProgram, Exec.step, Exec.startCleanup,
      Exec.Deferred.stmt, Exec.evalArgs, Exec.onRes, Examples.callEval, Examples.eval, Res.map, Res.bind,
      AList.lookup, henter, Exec.dropOut, Exec.dead, Exec.dom, Exec.restrict, hdrop, hleave,
      Exec.bindLife, Exec.bindDst, callSnapshot]

namespace DeferredLifetimeEvidence
open Examples DirectArgumentWitnesses

def heldSource : State := caller.own
def heldSourceForLeak : State := caller.own

/-- Mutant: registration prematurely ends the already held direct root loan. -/
def endAtRegistration (P : Exec.Program callOps) (c : Exec.Config callOps) : Option (Exec.Out callOps) := do
  let out ← deferredStep P c
  match c.ctrl, out with
  | .exec (.defer _ _ _), .next d =>
    some (.next { d with store := { d.store with own := { d.store.own with
      loans := d.store.own.loans.filter (fun l => !l.callRoot) } } })
  | _, out => some out

/-- Mutant: return restores a call-duration root loan that C1 just ended. -/
def leakAtReturn (P : Exec.Program callOps) (c : Exec.Config callOps) : Option (Exec.Out callOps) := do
  let out ← deferredStep P c
  match c.ctrl, c.frames, out with
  | .done, .ret _ _ :: _, .next d =>
    some (.next { d with store := { d.store with own := { d.store.own with
      loans := c.store.own.loans.filter (·.callRoot) ++ d.store.own.loans } } })
  | _, _, out => some out

/-- Instance (law `ConcreteSpec.Core.Own.deferred_call_lifetime`, case `ConcreteSpec.Core.Own.DeferredLifetimeEvidence.heldSource`). -/
example : DeferredLifetime deferredStep heldSource ⟨1⟩ [.index 3] z :=
  @deferred_call_lifetime heldSource ⟨1⟩ [.index 3] z ⟨0, [(x, ⟨0⟩), (y, ⟨1⟩)], []⟩
    (by decide) rfl (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.deferred_call_lifetime`, case `ConcreteSpec.Core.Own.DeferredLifetimeEvidence.heldSource`, subject `ConcreteSpec.Core.Own.deferredStep`, mutant `ConcreteSpec.Core.Own.DeferredLifetimeEvidence.endAtRegistration`). -/
example : WellFormed heldSource ∧
    heldSource.frames.head? = some (⟨0, [(x, ⟨0⟩), (y, ⟨1⟩)], []⟩ : Activation) ∧
    ownerAccess heldSource ⟨1⟩ .mutable = true ∧
    ¬ DeferredLifetime endAtRegistration heldSource ⟨1⟩ [.index 3] z := by
  refine ⟨by decide, rfl, by decide, ?_⟩
  rintro ⟨w, r, hp, hr, keep, _⟩
  cases hp
  cases hr
  obtain ⟨c, hc, hloan, _⟩ := keep 0 (by decide)
  cases hc
  revert hloan
  decide

/-- Instance (law `ConcreteSpec.Core.Own.deferred_call_lifetime`, case `ConcreteSpec.Core.Own.DeferredLifetimeEvidence.heldSourceForLeak`). -/
example : DeferredLifetime deferredStep heldSourceForLeak ⟨1⟩ [.index 3] z :=
  @deferred_call_lifetime heldSourceForLeak ⟨1⟩ [.index 3] z ⟨0, [(x, ⟨0⟩), (y, ⟨1⟩)], []⟩
    (by decide) rfl (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.deferred_call_lifetime`, case `ConcreteSpec.Core.Own.DeferredLifetimeEvidence.heldSourceForLeak`, subject `ConcreteSpec.Core.Own.deferredStep`, mutant `ConcreteSpec.Core.Own.DeferredLifetimeEvidence.leakAtReturn`). -/
example : WellFormed heldSourceForLeak ∧
    heldSourceForLeak.frames.head? = some (⟨0, [(x, ⟨0⟩), (y, ⟨1⟩)], []⟩ : Activation) ∧
    ownerAccess heldSourceForLeak ⟨1⟩ .mutable = true ∧
    ¬ DeferredLifetime leakAtReturn heldSourceForLeak ⟨1⟩ [.index 3] z := by
  refine ⟨by decide, rfl, by decide, ?_⟩
  rintro ⟨w, r, hp, hr, _, restore⟩
  cases hp
  cases hr
  revert restore
  decide

end DeferredLifetimeEvidence

/-- Linear consumption denies both owner accesses until reinitialization restores the exact
original state. Unrelated owners, loans and activation indices survive the whole protocol. -/
def MoveLifecycle (consume : State → Hybrid.Place → Option State) (s : State) (p : Hybrid.Place) : Prop :=
  ∃ t, consume s p = some t ∧ accessOwner t p .shared = none ∧
    accessOwner t p .mutable = none ∧ reinitialize t p = some s

/-- Law (subject `ConcreteSpec.Core.Own.move`). A discharged linear owner is unusable between
consumption and reinitialization, and replacement restores all previous state. -/
theorem move_reinitialize_protocol (s : State) (p : Hybrid.Place) (x : Slot)
    (hs : WellFormed s) (found : slot s p = some x) (linear : x.desc.copy = false)
    (owned : x.status = .owned) (clear : ∀ l ∈ s.loans, l.root ≠ p) :
    MoveLifecycle move s p := by
  have xp : x.place = p := by simpa using List.find?_some found
  have xm : x ∈ s.slots := List.mem_of_find?_eq_some found
  let change := fun a : Slot => if a.place = p then { a with status := .moved } else a
  have place (a : Slot) : (change a).place = a.place := by simp [change]; split <;> rfl
  let t : State := { s with slots := s.slots.map change }
  have sf : slot t p = some { x with status := .moved } := by
    unfold slot at found ⊢
    simp only [t, List.find?_map, Function.comp_def, place, found, Option.map_some]
    simp [change, xp]
  have clearBool : s.loans.any (·.root == p) = false := by
    apply Bool.eq_false_iff.mpr
    intro h
    obtain ⟨l, hl, he⟩ := List.any_eq_true.mp h
    exact clear l hl (by simpa using he)
  have allClear : s.loans.all (·.root != p) = true := by
    apply List.all_eq_true.mpr
    intro l hl
    simpa using clear l hl
  have restoration :
      (s.slots.map change).map (fun a => if a.place = p then { a with status := .owned } else a) = s.slots := by
    rw [List.map_map]
    calc
      _ = s.slots.map id := by
        apply List.map_congr_left
        intro a ha
        dsimp only [Function.comp_def]
        rw [place]
        by_cases hp : a.place = p
        · have ax := slot_unique s.slots hs.1 a x ha xm (hp.trans xp.symm)
          subst a
          simp only [change, xp, ↓reduceIte]
          rw [← xp]
          cases x
          cases owned
          rfl
        · simp [change, hp]
      _ = s.slots := List.map_id _
  refine ⟨t, ?_, ?_, ?_, ?_⟩
  · simp [move, found, linear, owned, clearBool, t, change]
  · simp [accessOwner, ownerAccess, sf]
  · simp [accessOwner, ownerAccess, sf]
  · simp [reinitialize, sf, t, allClear, restoration]

namespace MoveLifecycleEvidence
open Examples DirectArgumentWitnesses

def source : State := caller.own
def selected : Slot := ⟨⟨1⟩, 0, ⟨.int .i32, false, true⟩, .owned⟩

/-- Mutant: consumption falsely reports success while retaining the owned value. -/
def keepOwned (s : State) (_ : Hybrid.Place) : Option State := some s

/-- Instance (law `ConcreteSpec.Core.Own.move_reinitialize_protocol`, case `ConcreteSpec.Core.Own.MoveLifecycleEvidence.source`). -/
example : MoveLifecycle move source ⟨1⟩ :=
  @move_reinitialize_protocol source ⟨1⟩ selected (by decide) rfl rfl rfl (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.move_reinitialize_protocol`, case `ConcreteSpec.Core.Own.MoveLifecycleEvidence.source`, subject `ConcreteSpec.Core.Own.move`, mutant `ConcreteSpec.Core.Own.MoveLifecycleEvidence.keepOwned`). -/
example : WellFormed source ∧ slot source ⟨1⟩ = some selected ∧ selected.desc.copy = false ∧
    selected.status = .owned ∧ (∀ l ∈ source.loans, l.root ≠ Hybrid.Place.mk 1) ∧
    ¬ MoveLifecycle keepOwned source ⟨1⟩ := by
  refine ⟨by decide, rfl, rfl, rfl, by decide, ?_⟩
  rintro ⟨t, ht, denied, _⟩
  cases ht
  cases denied

end MoveLifecycleEvidence

/-- Law (subject `ConcreteSpec.Core.Own.loanAction`). Every single loan-table transition
(`borrow`, `reborrowTo`, or `endLoan`) preserves `RetiredLoan` for any already retired
loan identity. -/
theorem loanSequence_retired_step (s : State) (op : LoanAction) (i : LoanId)
    (hr : RetiredLoan s i) :
    ResultSatisfies (fun t => RetiredLoan t i) (loanAction s op) := by
  cases h : loanAction s op with
  | none => trivial
  | some t =>
    have bound := hr.1
    cases op with
    | borrow p path mode =>
      change borrow s p path mode = some t at h
      unfold borrow at h
      obtain ⟨_, _, h⟩ := Option.bind_eq_some_iff.mp h
      split at h
      · cases h
        refine ⟨Nat.lt_succ_of_lt bound, ?_⟩
        intro l hl
        rcases List.mem_cons.mp hl with rfl | hl
        · intro he
          have hid := congrArg LoanId.id he
          dsimp at hid
          omega
        · exact hr.2 l hl
      · contradiction
    | reborrow i' path mode activation =>
      change reborrowTo s i' path mode activation = some t at h
      unfold reborrowTo at h
      obtain ⟨_, _, h⟩ := Option.bind_eq_some_iff.mp h
      split at h
      · cases h
        refine ⟨Nat.lt_succ_of_lt bound, ?_⟩
        intro l hl
        rcases List.mem_cons.mp hl with rfl | hl
        · intro he
          have hid := congrArg LoanId.id he
          dsimp at hid
          omega
        · exact hr.2 l hl
      · contradiction
    | endLoan i' =>
      change endLoan s i' = some t at h
      unfold endLoan at h
      obtain ⟨_, _, h⟩ := Option.bind_eq_some_iff.mp h
      split at h
      · contradiction
      · cases h
        exact ⟨bound, fun l hl => hr.2 l (List.mem_filter.mp hl).1⟩

/-- Law (subject `ConcreteSpec.Core.Own.runLoanSequence`). An ended loan identity cannot
reappear through any finite sequence of owner borrowing, projected reborrowing and ending.
The original roots may remain live and be borrowed again; their new loans have new identities. -/
theorem ended_loans_never_revive (s : State) (ops : List LoanAction) (i : LoanId)
    (hr : RetiredLoan s i) :
    ResultSatisfies (fun t => RetiredLoan t i) (runLoanSequence s ops) := by
  cases h : runLoanSequence s ops with
  | none => trivial
  | some t =>
    unfold runLoanSequence at h
    induction ops generalizing s with
    | nil => simp at h; subst t; exact hr
    | cons op ops ih =>
      simp only [List.foldlM_cons] at h
      obtain ⟨u, hu, h⟩ := Option.bind_eq_some_iff.mp h
      have hstep := loanSequence_retired_step s op i hr
      rw [hu] at hstep
      exact ih u hstep h

namespace LoanHistoryEvidence
open Examples

def endedChild : State := (endLoan child ⟨1⟩).getD initial
def actions : List LoanAction :=
  [.reborrow ⟨0⟩ [.index 2] .mutable 0, .endLoan ⟨2⟩,
    .reborrow ⟨0⟩ [.index 3] .mutable 0]

/-- Mutant: a single loan action recycles the ended child's identity `⟨1⟩`. -/
def recycleStep (s : State) (op : LoanAction) : Option State :=
  (loanAction s op).map fun t => { t with loans := t.loans.map fun l =>
    if l.id.id = t.nextLoan - 1 then { l with id := ⟨1⟩ } else l }

/-- Mutant: a later projected reborrow recycles the ended child's identity. -/
def recycle (s : State) (ops : List LoanAction) : Option State :=
  (runLoanSequence s ops).map fun t => { t with loans := t.loans.map fun l =>
    if l.id.id = t.nextLoan - 1 then { l with id := ⟨1⟩ } else l }

/-- Instance (law `ConcreteSpec.Core.Own.loanSequence_retired_step`, case `ConcreteSpec.Core.Own.LoanHistoryEvidence.endedChild`). -/
example : ResultSatisfies (fun t => RetiredLoan t ⟨1⟩)
    (loanAction endedChild (.reborrow ⟨0⟩ [.index 2] .mutable 0)) :=
  @loanSequence_retired_step endedChild (.reborrow ⟨0⟩ [.index 2] .mutable 0) ⟨1⟩ (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.loanSequence_retired_step`, case `ConcreteSpec.Core.Own.LoanHistoryEvidence.endedChild`, subject `ConcreteSpec.Core.Own.loanAction`, mutant `ConcreteSpec.Core.Own.LoanHistoryEvidence.recycleStep`). -/
example : RetiredLoan endedChild ⟨1⟩ ∧
    ¬ ResultSatisfies (fun t => RetiredLoan t ⟨1⟩)
      (recycleStep endedChild (.reborrow ⟨0⟩ [.index 2] .mutable 0)) := by decide

/-- The original mutable parent remains live while the ending/reborrowing sequence succeeds. -/
example : (runLoanSequence endedChild actions).isSome = true ∧
    (loan endedChild ⟨0⟩).isSome = true := by decide

/-- Instance (law `ConcreteSpec.Core.Own.ended_loans_never_revive`, case `ConcreteSpec.Core.Own.LoanHistoryEvidence.endedChild`). -/
example : ResultSatisfies (fun t => RetiredLoan t ⟨1⟩) (runLoanSequence endedChild actions) :=
  @ended_loans_never_revive endedChild actions ⟨1⟩ (by decide)

/-- Planted (law `ConcreteSpec.Core.Own.ended_loans_never_revive`, case `ConcreteSpec.Core.Own.LoanHistoryEvidence.endedChild`, subject `ConcreteSpec.Core.Own.runLoanSequence`, mutant `ConcreteSpec.Core.Own.LoanHistoryEvidence.recycle`). -/
example : RetiredLoan endedChild ⟨1⟩ ∧
    ¬ ResultSatisfies (fun t => RetiredLoan t ⟨1⟩) (recycle endedChild actions) := by decide

end LoanHistoryEvidence

end ConcreteSpec.Core.Own
