import ConcreteSpec.Spec.Basics
import ConcreteSpec.Spec.Exec
import ConcreteSpec.Spec.Hybrid
import ConcreteSpec.Spec.Ident
import ConcreteSpec.Spec.Outcome
import ConcreteSpec.Spec.Own
import ConcreteSpec.Spec.Scalar
import ConcreteSpec.Spec.Target

/-! # C5 ownership preservation and lifetimes (O1)

Structural preservation uses F13's local operational guards, not a run-time invariant
scan. Every Law below has a populated Instance and a kernel-checked Planted refutation
in PreservationEvidence, LifetimeEvidence or FinalEvidence. The same postcondition is
applied to the real and substituted operation. These are not checker admission proofs.

Authority: docs/decisions.md#decided-2026-11-05-core (Q3.1–Q3.4, Q3.8, Q9.10),
#od15/#od23 (whole-root exclusion), #od60/#od69 (explicit linear consumption,
no synthesized destruction), #decided-2026-11-08-recovered-d3 (C5 destinations and
call-duration lending), and #decided-2026-11-13-lifecycle-refusal. Each Law cites its
applicable subset. Q5/Q7 capability semantics are untouched. OD62 source-name and
Result/Option admission and OD64 raw-pointer confinement remain later typed-language
obligations, not claims of these scalar owner proofs.

Only Law counts as evidence; tags follow #decided-2026-11-13-proof-labels and controls
follow #decided-2026-11-14-trap-sites-and-law-tests. O2 execution erasure and O3 protocols
and history are outside this slice. No inherited approval receipt is evidence.
Held policies remain outside the imported F13 interface: ISS-0064/ISS-0065, ISS-0068
through ISS-0073 and ISS-0076 (source policy, constant restrictions, codecs, wiping,
ABI and aggregate layout). No law here resolves those decisions.
-/
namespace ConcreteSpec.Core.Own

/-- Ownership preservation specializes Basics' sole optional-result policy. -/
abbrev StateResult := ResultSatisfies WellFormed
/-- Ownership projection of a call result, using the same generic policy. -/
abbrev CallOwnResult := ResultSatisfies (fun s : CallState => WellFormed s.own)
/-- Full call bookkeeping postcondition, using the same generic policy. -/
abbrev CallResult := ResultSatisfies CallState.WellFormed

/-- Helper (congruence). Lifts a supplied one-step preservation proof through a monadic list fold. -/
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

/-- Law (authority: docs/decisions.md#foundation-q1 (C5); #od15; #od60; #decided-2026-11-05-core (Q3.8, Q9.10); #od69; #decided-2026-11-13-lifecycle-refusal (linear obligations and storage teardown)). (subject `ConcreteSpec.Core.Own.move`). Every successful result preserves
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
          change ∃ f ∈ s.frames, f.id = (change b).activation
          rw [activation]
          exact hs.2.2.2.2.2.2.2.1 b hb
        · intro f hf p hp
          obtain ⟨b, hb, he, hact⟩ := hs.2.2.2.2.2.2.2.2 f hf p hp
          exact ⟨change b, List.mem_map.mpr ⟨b, hb, rfl⟩,
            (place b).trans he, (activation b).trans hact⟩

/-- Law (authority: docs/decisions.md#foundation-q1 (C5); #od15; #od60; #decided-2026-11-05-core (Q3.8, Q9.10); #od69; #decided-2026-11-13-lifecycle-refusal (linear obligations and storage teardown)). (subject `ConcreteSpec.Core.Own.reinitialize`). Every successful result preserves
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
          change ∃ f ∈ s.frames, f.id = (change b).activation
          rw [activation]
          exact hs.2.2.2.2.2.2.2.1 b hb
        · intro f hf p hp
          obtain ⟨b, hb, he, hact⟩ := hs.2.2.2.2.2.2.2.2 f hf p hp
          exact ⟨change b, List.mem_map.mpr ⟨b, hb, rfl⟩,
            (place b).trans he, (activation b).trans hact⟩
    · contradiction

/-- Helper (computation). Reorders the disjunction defining symmetric loan compatibility. -/
private theorem compatible_symm (a b : Loan) (h : Compatible a b) : Compatible b a := by
  unfold Compatible at *
  rcases h with h | h | h | h | h
  · exact Or.inl (Ne.symm h)
  · exact Or.inr (Or.inl h.symm)
  · exact Or.inr (Or.inr (Or.inr (Or.inl h)))
  · exact Or.inr (Or.inr (Or.inl h))
  · exact Or.inr (Or.inr (Or.inr (Or.inr h.symm)))

/-- Helper (inversion). Extracts owned-root and whole-root permission facts from the owner access guard. -/
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

/-- Law (authority: docs/decisions.md#foundation-q1 (C5); #decided-2026-11-05-core (Q3.1–Q3.4); #od15; #od23; #decided-2026-11-08-recovered-d3 (C5 downward destinations and call duration)). (subject `ConcreteSpec.Core.Own.borrow`). Every successful result preserves
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

/-- Law (authority: docs/decisions.md#foundation-q1 (C5); #decided-2026-11-05-core (Q3.1–Q3.4); #od15; #od23; #decided-2026-11-08-recovered-d3 (C5 downward destinations and call duration)). (subject `ConcreteSpec.Core.Own.reborrowTo`). Every successful result preserves
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

/-- Law (authority: docs/decisions.md#foundation-q1 (C5); #decided-2026-11-05-core (Q3.1–Q3.4); #od15; #od23; #decided-2026-11-08-recovered-d3 (C5 downward destinations and call duration)). (subject `ConcreteSpec.Core.Own.reborrow`). Every successful result preserves
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

/-- Law (authority: docs/decisions.md#foundation-q1 (C5); #decided-2026-11-05-core (Q3.1–Q3.4); #od15; #od23; #decided-2026-11-08-recovered-d3 (C5 downward destinations and call duration)). (subject `ConcreteSpec.Core.Own.endLoan`). Every successful result preserves
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

/-- Helper (computation): projects the concrete result through this ownership operation. -/
theorem accessOwner_preserves_result (s t : State) (p : Hybrid.Place) (m : Mode)
    (hs : WellFormed s) (h : accessOwner s p m = some t) : WellFormed t := by
  unfold accessOwner at h
  split at h
  · cases h; exact hs
  · contradiction

/-- Helper (computation): projects the concrete result through this ownership operation. -/
theorem accessLoan_preserves_result (s t : State) (i : LoanId) (m : Mode)
    (hs : WellFormed s) (h : accessLoan s i m = some t) : WellFormed t := by
  unfold accessLoan at h
  split at h
  · cases h; exact hs
  · contradiction

/-- Helper (computation). Normalizes the move guard. -/
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

/-- Helper (computation). Normalizes the borrowing guard. -/
theorem borrow_progress (s : State) (p : Hybrid.Place) (path : List Projection) (m : Mode)
    (f : Activation) (frame : s.frames.head? = some f)
    (issued : f.id < s.nextActivation) (allowed : ownerAccess s p m = true) :
    ∃ t, borrow s p path m = some t := by
  simp [borrow, frame, issued, allowed]

/-- Helper (computation). Normalizes the explicit destination guard. -/
theorem reborrowTo_progress (s : State) (i : LoanId) (path : List Projection) (m : Mode)
    (activation : Nat) (a : Loan) (found : loan s i = some a)
    (issued : activation < s.nextActivation)
    (current : s.frames.head?.map Activation.id = some activation)
    (allowed : loanAccess s i m = true) :
    ∃ t, reborrowTo s i path m activation = some t := by
  simp [reborrowTo, found, issued, current, allowed]

/-- Helper (computation). Specializes the destination computation to the current frame. -/
theorem reborrow_progress (s : State) (i : LoanId) (path : List Projection) (m : Mode)
    (f : Activation) (frame : s.frames.head? = some f) (issued : f.id < s.nextActivation)
    (a : Loan) (found : loan s i = some a) (allowed : loanAccess s i m = true) :
    ∃ t, reborrow s i path m = some t := by
  simpa [reborrow, frame] using
    reborrowTo_progress s i path m f.id a found issued (by simp [frame]) allowed

/-- Helper (computation). Computes the successful identity transition from owner permission. -/
theorem accessOwner_progress (s : State) (p : Hybrid.Place) (m : Mode)
    (allowed : ownerAccess s p m = true) : accessOwner s p m = some s := by
  simp [accessOwner, allowed]

/-- Helper (computation). Computes the successful identity transition from loan permission. -/
theorem accessLoan_progress (s : State) (i : LoanId) (m : Mode)
    (allowed : loanAccess s i m = true) : accessLoan s i m = some s := by
  simp [accessLoan, allowed]

namespace PrimitiveWitnesses
open ConcreteSpec.Core.Own.Examples
/-- Instance: Positive preservation/progress instances use live, nonempty owner/loan states. -/
example : WellFormed initial ∧ (move initial ⟨0⟩).isSome = true ∧
    WellFormed ((move initial ⟨0⟩).getD initial) := by decide
/-- Instance: concrete ownership calculation. -/
example : WellFormed shared ∧ (borrow shared ⟨0⟩ [] .shared).isSome = true ∧
    WellFormed ((borrow shared ⟨0⟩ [] .shared).getD initial) := by decide
/-- Instance: concrete ownership calculation. -/
example : WellFormed exclusive ∧ (reborrowTo exclusive ⟨0⟩ [] .mutable 0).isSome = true ∧
    WellFormed child := by decide
/-- Instance: concrete ownership calculation. -/
example : accessOwner shared ⟨0⟩ .shared = some shared ∧ WellFormed shared := by decide
/-- Instance: concrete ownership calculation. -/
example : accessLoan child ⟨1⟩ .mutable = some child ∧ WellFormed child := by decide
/-- Planted: Malformed move/access observations destroy an owned root under a live loan. -/
example : ¬ WellFormed { shared with slots := shared.slots.map fun x =>
    { x with status := .moved } } := by decide
/-- Planted: concrete ownership calculation. -/
example : ¬ WellFormed { child with slots := [] } := by decide
/-- Planted: A malformed borrow reuses the supply; a malformed reborrow loses its parent. -/
example : ¬ WellFormed { shared with nextLoan := 0 } := by decide
/-- Planted: concrete ownership calculation. -/
example : ¬ WellFormed { child with loans := child.loans.filter (·.id != ⟨0⟩) } := by decide
/-- Planted: Negated success with each operational permission violated. -/
example : ¬ (move shared ⟨0⟩).isSome = true := by decide
/-- Planted: concrete ownership calculation. -/
example : ¬ (borrow exclusive ⟨0⟩ [] .shared).isSome = true := by decide
/-- Planted: concrete ownership calculation. -/
example : ¬ (reborrowTo shared ⟨0⟩ [] .mutable 0).isSome = true := by decide
/-- Planted: concrete ownership calculation. -/
example : ¬ (reborrow shared ⟨0⟩ [] .mutable).isSome = true := by decide
/-- Planted: concrete ownership calculation. -/
example : ¬ (accessOwner shared ⟨0⟩ .mutable).isSome = true := by decide
/-- Planted: concrete ownership calculation. -/
example : ¬ (accessLoan child ⟨0⟩ .mutable).isSome = true := by decide
end PrimitiveWitnesses

/-- Law (authority: docs/decisions.md#foundation-q1 (C5); #od15; #od60; #decided-2026-11-05-core (Q3.8, Q9.10); #od69; #decided-2026-11-13-lifecycle-refusal (linear obligations and storage teardown)). (subject `ConcreteSpec.Core.Own.bind`). Every successful result preserves
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
          have hnodup := hs.2.2.2.2.2.1
          rw [hframes] at hnodup
          refine ⟨?_, hs.2.1, ?_, ?_, hs.2.2.2.2.1, (by simpa only [List.tail_cons, List.pairwise_cons] using hnodup), ?_⟩
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
                · simp only [List.pairwise_cons] at hnodup
                  exfalso
                  exact hnodup.1 g hg ha_act
                · exact (hs.2.2.2.2.2.2.1 g hg_mem).2 a ha ha_act
            · intro a ha
              rcases List.mem_cons.mp ha with rfl | ha
              · exact ⟨_, List.mem_cons_self, rfl⟩
              · simpa only [hframes, List.tail_cons, List.mem_cons, or_and_right, exists_or, exists_eq_left] using hs.2.2.2.2.2.2.2.1 a ha
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

/-- Helper (inversion). Uses a duplicate-free place index to identify two slot records. -/
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

/-- Law (authority: docs/decisions.md#foundation-q1 (C5); #od15; #od60; #decided-2026-11-05-core (Q3.8, Q9.10); #od69; #decided-2026-11-13-lifecycle-refusal (linear obligations and storage teardown)). (subject `ConcreteSpec.Core.Own.drop`). Every successful result preserves
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
        have hnodup := hs.2.2.2.2.2.1
        rw [hframes] at hnodup
        refine ⟨hs.1.sublist (List.Sublist.map _ List.filter_sublist), hs.2.1, ?_, ?_, hs.2.2.2.2.1, (by simpa only [List.tail_cons, List.pairwise_cons] using hnodup), ?_⟩
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
            simpa only [hframes, List.tail_cons, List.mem_cons, or_and_right, exists_or, exists_eq_left] using
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
              simp only [List.pairwise_cons] at hnodup
              exact hnodup.1 g hg hgf.symm
    · contradiction

/-- Law (authority: docs/decisions.md#foundation-q1 (C5); #od15; #od60; #decided-2026-11-05-core (Q3.8, Q9.10); #od69; #decided-2026-11-13-lifecycle-refusal (linear obligations and storage teardown)). (subject `ConcreteSpec.Core.Own.enter`). Every successful result preserves
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
      · apply List.Pairwise.cons ?_ hs.2.2.2.2.2.1
        intro g hg he
        change s.nextActivation = g.id at he
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
          obtain ⟨f, hf, he⟩ := hs.2.2.2.2.2.2.2.1 a ha
          exact ⟨f, List.mem_cons_of_mem _ hf, he⟩
        · intro g hg p hp
          rcases List.mem_cons.mp hg with rfl | hg
          · simp at hp
          · exact hs.2.2.2.2.2.2.2.2 g hg p hp
    exact fold_preserves _ (fun s x t hs h => by
      have h' := bind_preserves types s x hs
      rwa [h] at h') xs _ t hw h

/-- Law (authority: docs/decisions.md#foundation-q1 (C5); #od69; #decided-2026-11-08-recovered-d3 (C5: existing loans never ended implicitly); #decided-2026-11-13-lifecycle-refusal). (subject `ConcreteSpec.Core.Own.endLocals`). Every successful result preserves
ownership; refusal does not fabricate a result. -/
theorem endLocals_preserves (s : State) (a : Nat) (hs : WellFormed s) : StateResult (endLocals a s) := by
  cases h : endLocals a s with
  | none => trivial
  | some t =>
    exact fold_preserves _ (fun s l t hs h => by
      have h' := endLoan_preserves s l.id hs
      rwa [h] at h') _ _ _ hs h

/-- Helper (computation). Ending a loan leaves activation frames unchanged. -/
private theorem endLoan_frames (s t : State) (i : LoanId) (h : endLoan s i = some t) :
    t.frames = s.frames := by
  unfold endLoan at h
  obtain ⟨_, _, hh⟩ := Option.bind_eq_some_iff.mp h
  split at hh
  · contradiction
  · cases hh
    rfl

/-- Helper (congruence). Lifts a supplied frame equality through a monadic fold. -/
private theorem fold_frames {α : Type} (f : State → α → Option State)
    (hf : ∀ s a t, f s a = some t → t.frames = s.frames) (xs : List α)
    (s t : State) (h : xs.foldlM f s = some t) : t.frames = s.frames := by
  induction xs generalizing s with
  | nil => simp at h; subst t; rfl
  | cons a xs ih =>
    simp only [List.foldlM_cons] at h
    obtain ⟨u, hu, hh⟩ := Option.bind_eq_some_iff.mp h
    exact (ih u hh).trans (hf s a u hu)

/-- Helper (inversion). Inverts the slot-removal filter for a selected local place. -/
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

/-- Law (authority: docs/decisions.md#foundation-q1 (C5); #od69; #decided-2026-11-08-recovered-d3 (C5: existing loans never ended implicitly); #decided-2026-11-13-lifecycle-refusal). (subject `ConcreteSpec.Core.Own.leave`). The semantic postcondition holds
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

/-- Law (authority: docs/decisions.md#foundation-q1 (C5); #od69; #decided-2026-11-08-recovered-d3 (C5: existing loans never ended implicitly); #decided-2026-11-13-lifecycle-refusal). (subject `ConcreteSpec.Core.Own.leave`). Every successful result preserves
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
      hw.2.2.2.2.2.1.sublist (List.tail_sublist _), ?_⟩
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
        obtain ⟨a, ha, he⟩ := hm
        rw [hvf] at ha
        rcases List.mem_cons.mp ha with rfl | ha
        · exact False.elim (absent x hx (he.symm.trans hgf))
        · exact ⟨a, ha, he⟩
    · intro g hg p hp
      exact hw.2.2.2.2.2.2.2.2 g (List.mem_of_mem_tail hg) p hp

/-- Helper (congruence). Updating only the view-binding lookup table of the current frame preserves `WellFormed`. -/
private theorem set_views_preserves (s : State) (f : Activation) (views : AList Var LoanId)
    (hs : WellFormed s) (hf : s.frames.head? = some f) :
    WellFormed { s with frames := { f with views := views } :: s.frames.tail } := by
  cases hframes : s.frames with
  | nil => simp [hframes] at hf
  | cons f₀ tail =>
    simp only [hframes, List.head?_cons, Option.some.injEq] at hf
    subst f₀
    have hnodup := hs.2.2.2.2.2.1
    rw [hframes] at hnodup
    refine ⟨hs.1, hs.2.1, hs.2.2.1, hs.2.2.2.1, hs.2.2.2.2.1, (by simpa only [List.tail_cons, List.pairwise_cons] using hnodup), ?_⟩
    refine ⟨?_, ?_, ?_⟩
    · intro g hg
      rcases List.mem_cons.mp hg with rfl | hg
      · exact hs.2.2.2.2.2.2.1 f (by simp [hframes])
      · dsimp at hg
        exact hs.2.2.2.2.2.2.1 g (by simp [hframes, hg])
    · intro a ha
      simpa only [hframes, List.tail_cons, List.mem_cons, or_and_right, exists_or, exists_eq_left] using hs.2.2.2.2.2.2.2.1 a ha
    · intro g hg p hp
      rcases List.mem_cons.mp hg with rfl | hg
      · exact hs.2.2.2.2.2.2.2.2 f (by simp [hframes]) p hp
      · dsimp at hg
        exact hs.2.2.2.2.2.2.2.2 g (by simp [hframes, hg]) p hp

/-- Law (authority: docs/decisions.md#foundation-q1 (C5); #decided-2026-11-05-core (Q3.1–Q3.4); #od15; #od23; #decided-2026-11-08-recovered-d3 (C5 downward destinations and call duration)). (subject `ConcreteSpec.Core.Own.enterDown`). Every successful result preserves
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

/-- Law (authority: docs/decisions.md#foundation-q1 (C5); #od69; #decided-2026-11-08-recovered-d3 (C5: existing loans never ended implicitly); #decided-2026-11-13-lifecycle-refusal). (subject `ConcreteSpec.Core.Own.life`). Every successful result preserves
ownership; refusal does not fabricate a result. -/
theorem life_preserves (types : Var → Option Descriptor) (e : Exec.Life) (s : State)
    (hs : WellFormed s) : StateResult (life types e s) := by
  cases e with
  | bind x => exact bind_preserves types s x hs
  | enter ps => exact enter_preserves types s ps hs
  | leave => exact leave_preserves s hs
  | drop xs => exact drop_preserves s xs hs

/-- Helper (computation). Normalizes the replacement guard. -/
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

/-- Helper (computation). Normalizes the child-exclusion guard. -/
theorem endLoan_progress (s : State) (i : LoanId) (a : Loan)
    (found : loan s i = some a) (children : ∀ b ∈ s.loans, b.parent ≠ some i) :
    ∃ t, endLoan s i = some t := by
  have hc : s.loans.any (·.parent == some i) = false := by
    apply Bool.eq_false_iff.mpr
    intro hg
    obtain ⟨b, hb, he⟩ := List.any_eq_true.mp hg
    exact children b hb (by simpa using he)
  simp [endLoan, found, hc]

/-- Helper (computation). Computes a typed fresh binding. -/
theorem bind_fresh_progress (types : Var → Option Descriptor) (s : State) (x : Var)
    (f : Activation) (d : Descriptor) (frame : s.frames.head? = some f)
    (issued : f.id < s.nextActivation) (fresh : AList.lookup f.locals x = none)
    (descriptor : types x = some d) : ∃ t, bind types x s = some t := by
  simp [bind, frame, issued, fresh, descriptor]

/-- Law (authority: docs/decisions.md#foundation-q1 (C5); #decided-2026-11-05-core (Q3.8, Q9.1, Q9.10); #decided-2026-11-13-lifecycle-refusal). Fresh, distinct, typed parameters always enter in a new activation. -/
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

/-- Helper (computation). Normalizes discharge and conflict checks. -/
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
/-- Instance: Each lifecycle/loan-ending law has a live-state success instance. -/
example : WellFormed moved ∧ reinitialize moved ⟨0⟩ = some initial := by decide
/-- Instance: concrete ownership calculation. -/
example : WellFormed child ∧ (endLoan child ⟨1⟩).isSome = true ∧
    WellFormed ((endLoan child ⟨1⟩).getD initial) := by decide
/-- Instance: concrete ownership calculation. -/
example : WellFormed shared ∧ (bind types y shared).isSome = true ∧
    WellFormed ((bind types y shared).getD initial) := by decide
/-- Instance: concrete ownership calculation. -/
example : WellFormed moved ∧ (drop [x] moved).isSome = true ∧
    WellFormed ((drop [x] moved).getD initial) := by decide
/-- Instance: concrete ownership calculation. -/
example : WellFormed entered ∧ (enter types [y] shared).isSome = true := by decide
/-- Instance: concrete ownership calculation. -/
example : WellFormed called ∧ (endLocals 1 called).isSome = true ∧
    WellFormed ((endLocals 1 called).getD initial) := by decide
/-- Instance: concrete ownership calculation. -/
example : (leave called).isSome = true ∧ WellFormed ((leave called).getD initial) := by decide
/-- Instance: concrete ownership calculation. -/
example : life types (.bind y) shared = bind types y shared := by rfl
/-- Planted: Malformed reinitialization/drop/leave observations lose a borrowed root. -/
example : ¬ WellFormed { shared with slots := [] } := by decide
/-- Planted: A malformed bind reuses the place identity. -/
example : ¬ WellFormed { initial with slots := initial.slots ++ initial.slots } := by decide
/-- Planted entry/adoption mutation assigns an unissued activation. -/
example : ¬ WellFormed { shared with loans := shared.loans.map fun l =>
    { l with activation := shared.nextActivation } } := by decide
/-- Planted endLoan/endLocals mutation removes the parent before the child. -/
example : ¬ WellFormed { child with loans := child.loans.filter (·.id != ⟨0⟩) } := by decide
/-- Planted: Progress counterexamples: live linear replacement, a child and a live local obligation. -/
example : ¬ (reinitialize initial ⟨0⟩).isSome = true := by decide
/-- Planted: concrete ownership calculation. -/
example : ¬ (endLoan child ⟨0⟩).isSome = true := by decide
/-- Planted: concrete ownership calculation. -/
example : ¬ (bind (fun _ => none) y shared).isSome = true := by decide
/-- Planted: concrete ownership calculation. -/
example : ¬ (drop [x] initial).isSome = true := by decide
end LifecycleWitnesses

/-- Helper (projection). Projects the owned-root clause of the well-formedness invariant for the operation-result law. -/
theorem loan_has_owned_root_of_wf (s : State) (hs : WellFormed s) (l : Loan) (hl : l ∈ s.loans) :
    ∃ x ∈ s.slots, x.place = l.root ∧ x.status = .owned := (hs.2.2.2.1 l hl).2.2.1


/-- Helper (inversion). Invariant consequence using uniqueness of slot identities and owned roots for the operation-result law. -/
theorem moved_no_loan_of_wf (s : State) (hs : WellFormed s) (x : Slot) (hx : x ∈ s.slots)
    (hm : x.status = .moved) (l : Loan) (hl : l ∈ s.loans) : x.place ≠ l.root := by
  intro he
  obtain ⟨y, hy, hp, ho⟩ := loan_has_owned_root_of_wf s hs l hl
  have hxy : x = y := slot_unique s.slots hs.1 x y hx hy (he.trans hp.symm)
  subst y
  rw [hm] at ho
  cases ho

/-- Helper (computation): projects the concrete result through this ownership operation. -/
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

/-- Helper (computation): projects the concrete result through this ownership operation. -/
theorem life_erasure_result {σ : Type} (types : Var → Option Descriptor) (e : Exec.Life)
    (s t : Store σ) (h : lifeStore types e s = some t) :
    erase t = erase s ∧ life types e s.own = some t.own := by
  unfold lifeStore at h
  obtain ⟨own, he, h⟩ := Option.map_eq_some_iff.mp h
  subst t
  exact ⟨rfl, he⟩

/-- Helper (congruence). Transports the invariant through a loan map that retains semantic identity fields. -/
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

/-- Law (authority: docs/decisions.md#foundation-q1 (C5); #decided-2026-11-05-core (Q3.1–Q3.4); #od15; #od23; #decided-2026-11-08-recovered-d3 (C5 downward destinations and call duration)). (subject `ConcreteSpec.Core.Own.adoptDown`). Every successful result preserves
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

/-- Law (authority: docs/decisions.md#foundation-q1 (C5); #decided-2026-11-05-core (Q3.1–Q3.4); #od15; #od23; #decided-2026-11-08-recovered-d3 (C5 downward destinations and call duration)). (subject `ConcreteSpec.Core.Own.passDown`). Every successful result preserves
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

/-- Law (authority: docs/decisions.md#foundation-q1 (C5); #decided-2026-11-05-core (Q3.1–Q3.4); #od15; #od23; #decided-2026-11-08-recovered-d3 (C5 downward destinations and call duration)). (subject `ConcreteSpec.Core.Own.passDown`). Every successful result preserves
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

/-- Law (authority: docs/decisions.md#foundation-q1 (C5); #decided-2026-11-05-core (Q3.1–Q3.4); #od15; #od23; #decided-2026-11-08-recovered-d3 (C5 downward destinations and call duration); #od69; #decided-2026-11-13-lifecycle-refusal). (subject `ConcreteSpec.Core.Own.callLife`). Every successful result preserves
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

/-- Helper (computation). Reinitialization changes slot status while leaving the loan table unchanged. -/
private theorem reinitialize_loans (s t : State) (p : Hybrid.Place)
    (h : reinitialize s p = some t) : t.loans = s.loans := by
  unfold reinitialize at h
  obtain ⟨x, _, h⟩ := Option.bind_eq_some_iff.mp h
  split at h
  · cases h; rfl
  · contradiction

/-- Helper (computation). Both binding branches leave the loan table unchanged. -/
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

/-- Helper (congruence). Lifts a supplied loan-table equality through a monadic fold. -/
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

/-- Helper (congruence). Composes binding loan-table equalities through parameter entry. -/
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

/-- Helper (computation). Dropping slots does not edit the loan table. -/
private theorem drop_loans (s t : State) (xs : List Var) (h : drop xs s = some t) :
    t.loans = s.loans := by
  unfold drop at h
  obtain ⟨f, _, h⟩ := Option.bind_eq_some_iff.mp h
  dsimp at h
  split at h
  · cases h; rfl
  · contradiction

/-- Helper (inversion). Inverts membership in the filtered loan table. -/
private theorem endLoan_subset (s t : State) (i : LoanId) (h : endLoan s i = some t) :
    ∀ l ∈ t.loans, l ∈ s.loans := by
  unfold endLoan at h
  obtain ⟨a, _, h⟩ := Option.bind_eq_some_iff.mp h
  split at h
  · contradiction
  · cases h
    exact fun _ hl => (List.mem_filter.mp hl).1

/-- Helper (inversion). Combines lifecycle branch inversions to trace surviving loans to their input. -/
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

/-- Law (authority: docs/decisions.md#foundation-q1 (C5); #decided-2026-11-05-core (Q3.1–Q3.4); #od15; #od23; #decided-2026-11-08-recovered-d3 (C5 downward destinations and call duration); #od69; #decided-2026-11-13-lifecycle-refusal). (subject `ConcreteSpec.Core.Own.callLife`). Every successful result preserves
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
/-- Instance: concrete ownership calculation. -/
example : CallState.WellFormed ⟨exclusive, []⟩ ∧ CallState.WellFormed waiting := by decide
/-- Instance: concrete ownership calculation. -/
example : (passDown waiting ⟨0⟩ y) = none := by decide
/-- Instance: concrete ownership calculation. -/
example : callLife types .leave waiting = none := by decide
/-- Instance: concrete ownership calculation. -/
example : (callLife types (.enter [y]) waiting).isSome = true := by decide
/-- Instance: concrete ownership calculation. -/
example : CallState.WellFormed ((callLife types (.enter [y]) waiting).getD waiting) := by decide
/-- Planted: Malformed bookkeeping observations: duplicate parameters and a dangling identity. -/
example : ¬ CallState.WellFormed { waiting with pending := waiting.pending ++ waiting.pending } := by decide
/-- Planted: concrete ownership calculation. -/
example : ¬ CallState.WellFormed { waiting with own := exclusive } := by decide
/-- Planted leave mutation: discard loans but retain the pending buffer. -/
example : ¬ CallState.WellFormed { waiting with own := initial } := by decide
end CallWitnesses

/-- Helper (computation): projects the concrete result through this ownership operation. -/
theorem callLife_erasure_result {σ : Type} (types : Var → Option Descriptor) (e : Exec.Life)
    (s t : σ × CallState) (h : callLifeStore types e s = some t) :
    t.1 = s.1 ∧ callLife types e s.2 = some t.2 := by
  unfold callLifeStore at h
  obtain ⟨u, hu, hh⟩ := Option.map_eq_some_iff.mp h
  subst t
  exact ⟨rfl, hu⟩

/-- Helper (computation): projects the concrete result through this ownership operation. -/
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

/-- Helper (computation): projects the concrete result through this ownership operation. -/
theorem readOwner_erasure_result (tgt : Target) (p : Hybrid.Place) (s t : Store Hybrid.State)
    (v : Hybrid.Value) (h : readOwner tgt p s = some (v, t)) :
    s.concrete.read tgt p = some v ∧ t.concrete = s.concrete := by
  have he := instrument_erasure_result _ _ _ _ _ h
  dsimp [erase] at he
  cases hc : s.concrete.read tgt p with
  | none => simp [hc] at he
  | some w => simp [hc] at he; rcases he with ⟨rfl, hstore⟩; exact ⟨rfl, hstore.symm⟩

/-- Helper (computation): projects the concrete result through this ownership operation. -/
theorem writeOwner_erasure_result (tgt : Target) (p : Hybrid.Place) (s t : Store Hybrid.State)
    (v : Hybrid.Value) (h : writeOwner tgt p v s = some ((), t)) :
    s.concrete.write tgt p v = some t.concrete := by
  have he := instrument_erasure_result _ _ _ _ _ h
  dsimp [erase] at he
  cases hc : s.concrete.write tgt p v with
  | none => simp [hc] at he
  | some st => simp [hc] at he; rw [← he]

/-- Helper (computation): projects the concrete result through this ownership operation. -/
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

/-- Helper (computation): projects the concrete result through this ownership operation. -/
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

/-- Helper (arithmetic). Derived invariant or supplied-preservation fact; not independent soundness evidence. The next identity has never belonged to any currently active loan. Combined with the
unchanged/incremented supply laws, ending a loan cannot make its identity fresh again. -/
theorem nextLoan_fresh (s : State) (hs : WellFormed s) (l : Loan) (hl : l ∈ s.loans) :
    l.id ≠ LoanId.mk s.nextLoan := by
  intro he
  have hb := (hs.2.2.2.1 l hl).1
  rw [he] at hb
  exact Nat.lt_irrefl _ hb

/-- Helper (computation): projects the concrete result through this ownership operation. -/
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

/-- Helper (computation): projects the concrete result through this ownership operation. -/
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


/-- Helper (computation): projects the concrete result through this ownership operation. -/
theorem endLoan_supply_result (s t : State) (i : LoanId) (h : endLoan s i = some t) :
    t.nextLoan = s.nextLoan := by
  unfold endLoan at h
  obtain ⟨_, _, hh⟩ := Option.bind_eq_some_iff.mp h
  split at hh
  · contradiction
  · cases hh
    rfl

/-- Law (authority: docs/decisions.md#foundation-q1 (C5); #od69; #decided-2026-11-08-recovered-d3 (C5: existing loans never ended implicitly); #decided-2026-11-13-lifecycle-refusal). (subject `ConcreteSpec.Core.Own.leave`). The semantic postcondition holds
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

/-- Helper (projection). Extracts the pending-argument witness checked by adoption. -/
theorem adoptDown_only_arguments_result (s t : State) (args : List (Var × LoanId))
    (h : adoptDown s args = some t) (e : Var × LoanId) (he : e ∈ args) :
    ∃ l ∈ s.loans, l.id = e.2 ∧ l.pendingArgument = true := by
  unfold adoptDown at h
  obtain ⟨_, _, hh⟩ := Option.bind_eq_some_iff.mp h
  split at hh
  · rename_i guard
    exact guard.2.2.2.1 e he
  · contradiction


/-- Helper (projection). Extracts the active-current-frame destination checked by reborrowing. -/
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

/-- Law (authority: docs/decisions.md#foundation-q1 (C5); #decided-2026-11-05-core (Q3.1–Q3.4); #od15; #od23; #decided-2026-11-08-recovered-d3 (C5 downward destinations and call duration)). (subject `ConcreteSpec.Core.Own.passOwner`). Every successful result preserves
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

/-- Law (authority: docs/decisions.md#foundation-q1 (C5); #decided-2026-11-05-core (Q3.1–Q3.4); #od15; #od23; #decided-2026-11-08-recovered-d3 (C5 downward destinations and call duration)). (subject `ConcreteSpec.Core.Own.passOwner`). Every successful result preserves
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

/-- Law (authority: docs/decisions.md#foundation-q1 (C5); #decided-2026-11-05-core (Q3.1–Q3.4); #od15; #od23; #decided-2026-11-08-recovered-d3 (C5 downward destinations and call duration)). (subject `ConcreteSpec.Core.Own.passOwner`). The semantic postcondition holds
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

/-- Helper (computation): projects the concrete result through this ownership operation. -/
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

/-- Helper (inversion). Unique loan identities identify the full loan record. -/
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

/-- Helper (inversion). Ending a list of other identities retains the entire loan record. -/
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

/-- Law (authority: docs/decisions.md#foundation-q1 (C5); #od69; #decided-2026-11-08-recovered-d3 (C5: existing loans never ended implicitly); #decided-2026-11-13-lifecycle-refusal). (subject `ConcreteSpec.Core.Own.leave`). The semantic postcondition holds
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

/-- Helper (inversion). Ending loans removes every scheduled identity and never adds loans. -/
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

/-- Law (authority: docs/decisions.md#foundation-q1 (C5); #od69; #decided-2026-11-08-recovered-d3 (C5: existing loans never ended implicitly); #decided-2026-11-13-lifecycle-refusal). (subject `ConcreteSpec.Core.Own.leave`). The semantic postcondition holds
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

/-- Helper (computation): projects the concrete result through this ownership operation. -/
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
/-- Instance: Observation. A projected owner loan issues identity zero. -/
example : borrow initial ⟨0⟩ [.index 0] .shared = some shared ∧
    shared.nextLoan = initial.nextLoan + 1 ∧ ∃ l ∈ shared.loans,
      l.id = LoanId.mk initial.nextLoan ∧ l.root = ⟨0⟩ ∧ l.path = [.index 0] := by decide
/-- Planted: Rejection observation. Wrong projection falsifies the existential birth conclusion. -/
example : ¬ (∃ l ∈ shared.loans, l.id = LoanId.mk initial.nextLoan ∧
    l.root = ⟨0⟩ ∧ l.path = [.index 99]) := by decide
/-- Instance: Observation. The child retains its parent's root, extended path and parent identity. -/
example : reborrowTo exclusive ⟨0⟩ [.index 1] .mutable 0 = some child ∧
    child.nextLoan = exclusive.nextLoan + 1 ∧ ∃ a l, loan exclusive ⟨0⟩ = some a ∧
      l ∈ child.loans ∧ l.id = LoanId.mk exclusive.nextLoan ∧ l.root = a.root ∧
      l.path = a.path ++ [.index 1] ∧ l.parent = some ⟨0⟩ := by
  simp [Examples.exclusive, Examples.child, Examples.initial, reborrow, reborrowTo, borrow,
    ownerAccess, loanAccess, slot, loan]
/-- Planted: Rejection observation. Removing the child's parent field falsifies the birth conclusion itself. -/
example : ¬ (∃ a l, loan exclusive ⟨0⟩ = some a ∧
    l ∈ (child.loans.map fun l => { l with parent := none }) ∧
    l.id = LoanId.mk exclusive.nextLoan ∧ l.root = a.root ∧
    l.path = a.path ++ [.index 1] ∧ l.parent = some ⟨0⟩) := by
  simp [Examples.exclusive, Examples.initial, borrow, ownerAccess, slot, loan]
/-- Instance: Observation. Ending a child preserves the nonzero supply. -/
example : ∃ t, endLoan child ⟨1⟩ = some t ∧ t.nextLoan = child.nextLoan := by decide
/-- Planted: Rejection observation. A mutated end decrements the supply. -/
example : ¬ (({ child with nextLoan := child.nextLoan - 1 } : State).nextLoan =
    child.nextLoan) := by decide

/-- A callee owns a Copy local and borrows it; an unrelated caller loan survives its exit. -/
def localFrame : State := (enter (fun _ => some ⟨.int .i32, true, true⟩) [y] shared).getD initial
def localLoan : State := (borrow localFrame ⟨1⟩ [] .shared).getD initial
def exited : State := (leave localLoan).getD initial
/-- Instance: Observation. The departing local had a live loan; the caller's loan survives. -/
example : WellFormed localLoan ∧ leave localLoan = some exited ∧
    (y, Hybrid.Place.mk 1) ∈ (localLoan.frames.headD ⟨99, [], []⟩).locals ∧
    (∃ l ∈ localLoan.loans, l.root = ⟨1⟩) ∧
    (∃ l ∈ exited.loans, l.root = ⟨0⟩) ∧
    (∀ l ∈ exited.loans, l.root ≠ Hybrid.Place.mk 1) ∧
    (∀ x ∈ exited.slots, x.activation ≠ 1) := by decide
/-- Planted: Rejection observation. Exit retaining the local's loan fails the root exclusion conclusion. -/
example : ¬ (∀ l ∈ localLoan.loans, l.root ≠ Hybrid.Place.mk 1) := by decide
/-- Planted: Rejection observation. Exit retaining a departing slot fails activation exclusion. -/
example : ¬ (∀ x ∈ localLoan.slots, x.activation ≠ 1) := by decide
/-- Instance: Observation. A child marked during argument evaluation can be adopted. -/
example : (callLife types (.enter [y]) CallWitnesses.waiting).isSome = true ∧
    ∃ l ∈ CallWitnesses.waiting.own.loans, l.id = LoanId.mk 1 ∧
      l.pendingArgument = true := by decide
/-- Planted: Rejection observation. Adopting a normal caller loan lacks the required pending witness. -/
example : ¬ (∃ l ∈ exclusive.loans, l.id = LoanId.mk 0 ∧
    l.pendingArgument = true) := by decide
/-- Planted: Rejection observation. An untyped parameter refuses entry. -/
example : ¬ (enter (fun _ => none) [y] shared).isSome = true := by decide
/-- Instance: Observation. A typed fresh parameter enters over a borrowed caller. -/
example : (enter types [y] shared).isSome = true := by decide
/-- Instance: Observation. Lending downward after entry targets the newly active callee. -/
example : (reborrowTo LifecycleWitnesses.called ⟨1⟩ [] .shared 1).isSome = true ∧
    ∃ f ∈ LifecycleWitnesses.called.frames, f.id = 1 ∧
      LifecycleWitnesses.called.frames.head? = some f := by decide
/-- Planted: Refused: activation 1 cannot lend upward into its caller, activation 0. -/
example : reborrowTo LifecycleWitnesses.called ⟨1⟩ [] .shared 0 = none := by decide
/-- Planted: Rejection observation. The former arbitrary-destination rule admits upward lending, whose destination is not current. -/
example : ¬ (∃ f ∈ LifecycleWitnesses.called.frames, f.id = 0 ∧
    LifecycleWitnesses.called.frames.head? = some f) := by decide
/-- Planted: Rejection observation. A rule that issues the wrong birth counter fails the supply conclusion. -/
example : ¬ (({ shared with nextLoan := 7 } : State).nextLoan = initial.nextLoan + 1) := by decide
/-- Planted: Rejection observation. Failing to append the requested suffix falsifies the path conclusion. -/
example : ¬ (∃ l ∈ child.loans, l.id = LoanId.mk exclusive.nextLoan ∧
    l.path = [.index 0, .index 99]) := by decide
/-- Instance: Observation. Reborrowing retains the original parent record. -/
example : ∃ a, loan exclusive ⟨0⟩ = some a ∧ a ∈ child.loans := by
  simp [Examples.exclusive, Examples.child, Examples.initial, reborrow, reborrowTo,
    borrow, ownerAccess, loanAccess, slot, loan]
/-- Planted: Rejection observation. Removing the parent now falsifies the strengthened retention conclusion. -/
example : ¬ (∃ a, loan exclusive ⟨0⟩ = some a ∧
    a ∈ child.loans.filter (·.id != LoanId.mk 0)) := by
  simp [Examples.exclusive, Examples.child, Examples.initial, reborrow, reborrowTo,
    borrow, ownerAccess, loanAccess, slot, loan]
end RoundThreeWitnesses

/-- Law (authority: docs/decisions.md#foundation-q1 (C5); #decided-2026-11-05-core (Q3.1–Q3.4); #od15; #od23; #decided-2026-11-08-recovered-d3 (C5 downward destinations and call duration)). (subject `ConcreteSpec.Core.Own.passOwner`; lifecycle authority: #od69; #decided-2026-11-13-lifecycle-refusal). A direct owner call with no intervening operation restores the caller's complete slot
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

def z : Var := ⟨2⟩
def caller : CallState := ⟨{ shared with
  slots := ⟨⟨1⟩, 0, ⟨.int .i32, false, true⟩, .owned⟩ :: shared.slots
  frames := [⟨0, [(x, ⟨0⟩), (y, ⟨1⟩)], []⟩]
  nextPlace := 2 }, []⟩
def waiting : CallState := (passOwner caller ⟨1⟩ [.index 3] .mutable z).getD caller
def called : CallState := (callLife types (.enter [z]) waiting).getD caller
def returned : CallState := (callLife types .leave called).getD caller

/-- Instance: Observation. A direct mutable argument coexists with an unrelated caller loan. -/
example : CallState.WellFormed caller ∧ passOwner caller ⟨1⟩ [.index 3] .mutable z = some waiting ∧
    CallState.WellFormed waiting := by decide
/-- Planted: Rejection observation. A mutated direct loan reuses the birth supply. -/
example : ¬ WellFormed { waiting.own with nextLoan := 1 } := by decide
/-- Instance: Observation. Direct lending has no caller-side parent. -/
example : ∃ l, waiting.own.loans = l :: caller.own.loans ∧ l.id = LoanId.mk caller.own.nextLoan ∧
    l.root = Hybrid.Place.mk 1 ∧ l.path = [.index 3] ∧ l.mode = .mutable ∧
    l.parent = none ∧ l.pendingArgument = true ∧ l.callRoot = true := by
  exact passOwner_birth_result caller waiting ⟨1⟩ [.index 3] .mutable z (by decide)
/-- Planted: Rejection observation. A child of the caller loan cannot be the required parentless root. -/
example : ¬ (∃ l ∈ child.loans, l.id = LoanId.mk 1 ∧ l.parent = none) := by decide
/-- Instance: Observation. Argument evaluation freezes both reads and writes before entry. -/
example : ownerAccess waiting.own ⟨1⟩ .mutable = false ∧
    ownerAccess waiting.own ⟨1⟩ .shared = false := by decide
/-- Planted: Rejection observation. Removing the loan prematurely fails the freeze conclusion. -/
example : ¬ (ownerAccess { waiting.own with loans := caller.own.loans } ⟨1⟩ .mutable = false) := by decide
/-- Instance: Observation. Entry adopts the direct root and return ends it; its owner is immediately writable. -/
example : callLife types (.enter [z]) waiting = some called ∧
    callLife types .leave called = some returned ∧
    (∃ l ∈ called.own.loans, l.callRoot = true ∧ l.activation = 1) ∧
    (∀ l ∈ returned.own.loans, l.activation ≠ 1) ∧
    ownerAccess returned.own ⟨1⟩ .mutable = true := by decide
/-- Planted: Rejection observation. A return that never ends its direct argument retains a callee loan and freezes the owner. -/
example : ¬ (∀ l ∈ called.own.loans, l.activation ≠ 1) := by decide
/-- Instance: Observation. The original caller loan is unchanged after return. -/
example : WellFormed called.own ∧
    (∀ l ∈ caller.own.loans, l ∈ called.own.loans ∧ l ∈ returned.own.loans) := by decide
/-- Planted: Rejection observation. Ending every loan would destroy the pre-existing caller loan. -/
example : ¬ (∀ l ∈ caller.own.loans, l ∈ ([] : List Loan)) := by decide

/-- Instance: Observation. Registering a deferred call keeps a live direct root loan unchanged. -/
example : ∃ c : Exec.Config callOps,
    Exec.step callProgram ⟨.exec (.defer callee [.ownerArgument ⟨1⟩ .mutable z] ⟨0⟩),
      [], waiting, [.scope ⟨0⟩ [] []]⟩ = some (.next c) ∧ c.store = waiting := by
  exact ⟨_, rfl, rfl⟩
/-- Planted: Rejection observation. Ending the call loan at registration changes the store and unfreezes the owner prematurely. -/
example : ¬ ({ waiting with own := { waiting.own with loans := caller.own.loans } } = waiting) := by decide

/-- Instance: Accepted: a real C1 direct owner call returns, and the next operand reads the owner. -/
example : (match (Exec.run callProgram (fun _ p => nomatch p) 100
    ⟨.exec (.seq (.call none callee [.ownerArgument ⟨0⟩ .mutable y] ⟨0⟩)
      (.cond (.value (.read x)) .skip .skip ⟨0⟩)), [(x, 7)], ⟨initial, []⟩, []⟩).ending with
    | .final .normal => true
    | _ => false) = true := by decide
/-- Instance: Accepted: deferred direct lending uses late-read arguments; its call loan ends only when
that call returns, and the caller reads its owner after cleanup. -/
example : (match (Exec.run callProgram (fun _ p => nomatch p) 100
    ⟨.exec (.seq (.block ⟨0⟩ (.defer callee [.ownerArgument ⟨0⟩ .mutable y] ⟨0⟩))
      (.cond (.value (.read x)) .skip .skip ⟨0⟩)), [(x, 7)], ⟨initial, []⟩, []⟩).ending with
    | .final .normal => true
    | _ => false) = true := by decide
/-- Instance: Observation. A call restores two caller owners and a pre-existing caller loan. -/
example : (do
    let waiting ← passOwner caller ⟨1⟩ [.index 3] .mutable z
    let called ← callLife types (.enter [z]) waiting
    callLife types .leave called) = some returned ∧
    returned = ⟨{ caller.own with nextLoan := caller.own.nextLoan + 1, nextActivation := caller.own.nextActivation + 1 }, []⟩ := by decide
/-- Planted: Rejection observation. Keeping the call loan forever fails exact restoration of the caller tables. -/
example : ¬ (called.own.loans = caller.own.loans) := by decide
end DirectArgumentWitnesses

/-- Law (authority: docs/decisions.md#foundation-q1 (C5); #decided-2026-11-05-core (Q3.1–Q3.4); #od15; #od23; #decided-2026-11-08-recovered-d3 (C5 downward destinations and call duration)). (subject `ConcreteSpec.Core.Own.reborrowTo`). The semantic postcondition holds
of the actual successful operation result. -/
theorem reborrowTo_live (s : State) (i : LoanId) (path : List Projection)
    (m : Mode) (a : Nat) (hs : LiveDestinations s) :
    ResultSatisfies (fun t : State => LiveDestinations t) (reborrowTo s i path m a) := by
  cases h : reborrowTo s i path m a with
  | none => trivial
  | some t =>
    obtain ⟨f, hf, he, _⟩ := reborrowTo_current_result s t i path m a h
    have live : ∃ f ∈ s.frames, f.id = a := ⟨f, hf, he⟩
    unfold reborrowTo at h
    obtain ⟨_, _, h⟩ := Option.bind_eq_some_iff.mp h
    split at h
    · cases h
      intro l hl
      rcases List.mem_cons.mp hl with rfl | hl
      · exact live
      · exact hs l hl
    · contradiction

/-- Law (authority: docs/decisions.md#foundation-q1 (C5); #decided-2026-11-05-core (Q3.1–Q3.4); #od15; #od23; #decided-2026-11-08-recovered-d3 (C5 downward destinations and call duration)). (subject `ConcreteSpec.Core.Own.adoptDown`). The semantic postcondition holds
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

/-- Helper (projection). Successful return pops exactly the current activation frame. -/
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

/-- Law (authority: docs/decisions.md#foundation-q1 (C5); #od69; #decided-2026-11-08-recovered-d3 (C5: existing loans never ended implicitly); #decided-2026-11-13-lifecycle-refusal). (subject `ConcreteSpec.Core.Own.leave`). The semantic postcondition holds
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
      obtain ⟨g, hg, he⟩ := live
      rw [frames] at hg
      rcases List.mem_cons.mp hg with rfl | hg
      · exact False.elim (other he.symm)
      · exact ⟨g, hg, he⟩

namespace LiveDestinationWitnesses
open ConcreteSpec.Core.Own.Examples
/-- Instance: Observation. A mutable loan and child target their active caller frame. -/
example : LiveDestinations exclusive ∧ LiveDestinations child ∧
    reborrowTo exclusive ⟨0⟩ [.index 1] .mutable 0 = some child := by decide
/-- Planted: Rejection observation. Targeting an inactive but previously issued activation breaks actual liveness. -/
example : ¬ LiveDestinations { child with
    nextActivation := 3
    loans := child.loans.map fun l => { l with activation := 2 } } := by decide
/-- Instance: Observation. A pending direct root becomes a loan of the active callee. -/
example : LiveDestinations DirectArgumentWitnesses.waiting.own ∧
    LiveDestinations DirectArgumentWitnesses.called.own := by decide
/-- Planted: Rejection observation. Adoption into an inactive destination violates the conclusion. -/
example : ¬ LiveDestinations { DirectArgumentWitnesses.called.own with
    loans := DirectArgumentWitnesses.called.own.loans.map fun l => { l with activation := 7 } } := by decide
/-- Instance: Observation. Return retains the caller loan and its live destination. -/
example : LiveDestinations DirectArgumentWitnesses.called.own ∧
    LiveDestinations DirectArgumentWitnesses.returned.own := by decide
/-- Planted: Rejection observation. Popping the callee without ending its root loan strands that loan. -/
example : ¬ LiveDestinations { DirectArgumentWitnesses.called.own with
    frames := DirectArgumentWitnesses.called.own.frames.tail } := by decide
end LiveDestinationWitnesses


/-- Helper (computation). The permission-only transition is identity. -/
theorem accessOwner_preserves (s : State) (p : Hybrid.Place) (m : Mode) (hs : WellFormed s) : StateResult (accessOwner s p m) := by
  cases he : accessOwner s p m with
  | none => trivial
  | some t => exact accessOwner_preserves_result s t p m hs he

/-- Helper (computation). The loan permission transition is identity. -/
theorem accessLoan_preserves (s : State) (i : LoanId) (m : Mode) (hs : WellFormed s) : StateResult (accessLoan s i m) := by
  cases he : accessLoan s i m with
  | none => trivial
  | some t => exact accessLoan_preserves_result s t i m hs he

namespace PreservationEvidence
/-- A fresh callee with a pending direct owner argument, before adoption. -/
def adoptionInput : State :=
  (enter Examples.types [] DirectArgumentWitnesses.waiting.own).getD Examples.initial

/-- Planted: move appends a moved slot record without replacing the existing record for `p`,
breaking unique slot place identities (`(s.slots.map Slot.place).Nodup`). -/
def moveDuplicateSlot (s : State) (p : Hybrid.Place) : Option State := do
  let x ← slot s p
  if x.desc.copy || x.status == .moved || s.loans.any (·.root == p) then none else
    some { s with slots := { x with status := .moved } :: s.slots }

/-- Planted: reinitialization assigns an unissued place identity `⟨s.nextPlace⟩` without advancing
`s.nextPlace`, breaking `x.place.id < s.nextPlace` and frame-local completeness. -/
def reinitializeUnissuedPlace (s : State) (p : Hybrid.Place) : Option State := do
  let _ ← reinitialize s p
  some { s with slots := s.slots.map fun x =>
    if x.place == p then { x with place := ⟨s.nextPlace⟩, status := .owned } else x }

/-- Planted: borrow issues `⟨s.nextLoan⟩` without advancing `nextLoan`, breaking `l.id.id < s.nextLoan`. -/
def borrowNoAdvanceSupply (s : State) (p : Hybrid.Place) (path : List Projection) (m : Mode) : Option State := do
  let t ← borrow s p path m
  some { t with nextLoan := s.nextLoan }

/-- Planted: reborrowTo removes the parent loan `i` when inserting its child, breaking `ParentValid`. -/
def reborrowToDropParent (s : State) (i : LoanId) (path : List Projection) (m : Mode) (a : Nat) : Option State := do
  let t ← reborrowTo s i path m a
  some { t with loans := t.loans.filter (·.id != i) }

/-- Planted: reborrow clears the cached ancestor chain while retaining `parent = some i`,
breaking `ParentValid`'s ancestor-chain agreement. -/
def reborrowDropAncestorsTo (s : State) (i : LoanId) (suffix : List Projection) (m : Mode)
    (activation : Nat) : Option State := do
  let a ← loan s i
  if loanAccess s i m && decide (activation < s.nextActivation ∧
      s.frames.head?.map Activation.id = some activation) then
    some { s with
      nextLoan := s.nextLoan + 1
      loans := ⟨⟨s.nextLoan⟩, a.root, a.path ++ suffix, m, some i,
        [], activation, false, false⟩ :: s.loans }
  else none

def reborrowDropAncestors (s : State) (i : LoanId) (path : List Projection) (m : Mode) : Option State := do
  let f ← s.frames.head?
  reborrowDropAncestorsTo s i path m f.id

/-- Planted: endLoan removes the parent of `i` instead of `i`, stranding child `i` and breaking `ParentValid`. -/
def endLoanRemoveParent (s : State) (i : LoanId) : Option State := do
  let a ← loan s i
  let _ ← endLoan s i
  some { s with loans := s.loans.filter fun l => some l.id != a.parent }

/-- Planted: bind allocates `⟨s.nextPlace⟩` without advancing `nextPlace`, breaking `x.place.id < s.nextPlace`. -/
def bindNoAdvancePlace (types : Var → Option Descriptor) (x : Var) (s : State) : Option State := do
  let t ← bind types x s
  some { t with nextPlace := s.nextPlace }

/-- Planted: drop removes the variable from `f.locals` while retaining its slot in `s.slots`,
breaking the frame-local completeness clause of `WellFormed`. -/
def dropLocalOnly (xs : List Var) (s : State) : Option State := do
  let t ← drop xs s
  some { t with slots := s.slots }

/-- Planted: enter reuses the caller's activation ID `0` for the new frame instead of `s.nextActivation`,
breaking `(s.frames.map Activation.id).Nodup`. -/
def enterDuplicateFrameId (types : Var → Option Descriptor) (params : List Var) (s : State) : Option State := do
  let s := { s with
    nextActivation := s.nextActivation + 1
    frames := ⟨s.nextActivation - 1, [], []⟩ :: s.frames }
  params.foldlM (fun s x => bind types x s) s

/-- Planted: endLocals removes caller loans (`activation != a`) instead of ending callee loans (`activation == a`),
breaking `ParentValid` for the callee child loan. -/
def endLocalsCallerInstead (a : Nat) (s : State) : Option State := do
  let _ ← endLocals a s
  some { s with loans := s.loans.filter (·.activation == a) }

/-- Planted: leave ends local loans but omits dropping the departing frame’s slots. -/
def leaveKeepSlots (s : State) : Option State := do
  let f ← s.frames.head?
  let t ← endLocals f.id s
  some { t with frames := t.frames.tail }

/-- Planted: enterDown reuses the parent loan's ID `⟨0⟩` for the new child loan, breaking `(s.loans.map Loan.id).Nodup`. -/
def reborrowReuseId (s : State) (i : LoanId) (suffix : List Projection) (m : Mode)
    (activation : Nat) : Option State := do
  let a ← loan s i
  if loanAccess s i m && decide (activation < s.nextActivation ∧
      s.frames.head?.map Activation.id = some activation) then
    some { s with
      nextLoan := s.nextLoan + 1
      loans := ⟨i, a.root, a.path ++ suffix, m, some i,
        i :: a.ancestors, activation, false, false⟩ :: s.loans }
  else none

/-- Planted: downward entry uses the ID-reusing child constructor at every lending site. -/
def enterDownReuseParentId (s : State) (args : List (Var × LoanId)) : Option State := do
  let s ← enter (fun _ => none) [] s
  args.foldlM (fun s (x, i) => do
    let a ← loan s i
    let f ← s.frames.head?
    let s ← reborrowReuseId s i [] a.mode f.id
    some { s with frames := { f with views := (x, ⟨s.nextLoan - 1⟩) :: f.views } :: s.frames.tail }) s

/-- Planted: life on `.bind` adds the new slot to `s.slots` without recording it in `f.locals`,
breaking the frame-local completeness clause of `WellFormed`. -/
def bindUnindexed (types : Var → Option Descriptor) (x : Var) (s : State) : Option State := do
  let f ← s.frames.head?
  match AList.lookup f.locals x with
  | some p => reinitialize s p
  | none =>
    let d ← types x
    if f.id < s.nextActivation then some { s with
      nextPlace := s.nextPlace + 1
      slots := ⟨⟨s.nextPlace⟩, f.id, d, .owned⟩ :: s.slots }
    else none

def lifeBindUnindexed (types : Var → Option Descriptor) : Exec.Life → State → Option State
  | .bind x, s => bindUnindexed types x s
  | e, s => life types e s

/-- Planted: adoptDown assigns the unissued activation ID `s.nextActivation` to adopted loans,
breaking `l.activation < s.nextActivation`. -/
def adoptDownUnissuedActivation (s : State) (args : List (Var × LoanId)) : Option State := do
  let f ← s.frames.head?
  if f.id < s.nextActivation ∧ (args.map Prod.fst).Nodup ∧ (args.map Prod.snd).Nodup ∧
      (∀ e ∈ args, ∃ l ∈ s.loans, l.id = e.2 ∧ l.pendingArgument = true) ∧
      (∀ e ∈ args, AList.lookup f.locals e.1 = none) then
    some { s with
      loans := s.loans.map fun l =>
        if l.id ∈ args.map Prod.snd then { l with activation := s.nextActivation, pendingArgument := false } else l
      frames := { f with views := args ++ f.views } :: s.frames.tail }
  else none

/-- Planted: passDown clears `parent` on the new non-root pending loan (`callRoot = false`),
breaking `l.pendingArgument = true → (l.parent.isSome = true ∨ l.callRoot = true)`. -/
def passDownClearParent (s : CallState) (i : LoanId) (x : Var) : Option CallState := do
  let t ← passDown s i x
  some { t with own := { t.own with loans := t.own.loans.map fun l =>
    if l.id == ⟨s.own.nextLoan⟩ then { l with parent := none, ancestors := [] } else l } }

/-- Planted: passDown marks the new loan `pendingArgument = true` in `own` without appending it to
`s.pending`, breaking `CallState.WellFormed`'s pending-flag synchronization. -/
def passDownOmitPending (s : CallState) (i : LoanId) (x : Var) : Option CallState := do
  let t ← passDown s i x
  some { t with pending := s.pending }

/-- Planted: callLife adopts pending loans into the unissued activation `own.nextActivation`,
breaking `l.activation < s.nextActivation`. -/
def callLifeUnissuedActivation (types : Var → Option Descriptor) : Exec.Life → CallState → Option CallState
  | .enter params, s => do
    if s.pending.all (fun e => decide (e.1 ∈ params)) then
      let own ← enter types (params.filter fun x => decide (x ∉ s.pending.map Prod.fst)) s.own
      let own ← adoptDownUnissuedActivation own s.pending
      pure ⟨own, []⟩
    else none
  | e, s => if s.pending = [] then
      (life types e s.own).map fun own => { s with own := own }
    else none

/-- Planted: callLife adopts pending loans (clearing their `pendingArgument` flags) without clearing
`s.pending`, breaking `CallState.WellFormed`'s pending-flag synchronization. -/
def callLifeKeepPending (types : Var → Option Descriptor) (e : Exec.Life) (s : CallState) : Option CallState := do
  let t ← callLife types e s
  some { t with pending := s.pending }

/-- Planted: passOwner creates a parentless pending loan with `callRoot = false`,
breaking `l.pendingArgument = true → (l.parent.isSome = true ∨ l.callRoot = true)`. -/
def passOwnerClearCallRoot (s : CallState) (p : Hybrid.Place) (path : List Projection) (m : Mode) (x : Var) : Option CallState := do
  let t ← passOwner s p path m x
  some { t with own := { t.own with loans := t.own.loans.map fun l =>
    if l.id == ⟨s.own.nextLoan⟩ then { l with callRoot := false } else l } }

/-- Planted: passOwner marks the new root loan `pendingArgument = true` in `own` without appending
it to `s.pending`, breaking `CallState.WellFormed`'s pending-flag synchronization. -/
def passOwnerOmitPending (s : CallState) (p : Hybrid.Place) (path : List Projection) (m : Mode) (x : Var) : Option CallState := do
  let t ← passOwner s p path m x
  some { t with pending := s.pending }

/-- Planted: accessOwner consumes the accessed root slot (marking it `.moved`) while a shared loan
remains active on `p`, breaking `∃ x ∈ s.slots, x.place = l.root ∧ x.status = .owned`. -/
def accessOwnerConsumeRoot (s : State) (p : Hybrid.Place) (m : Mode) : Option State := do
  let t ← accessOwner s p m
  some { t with slots := t.slots.map fun x =>
    if x.place == p then { x with status := .moved } else x }

/-- Planted: accessLoan drops the parent of loan `i` while `i` remains active, breaking `ParentValid`. -/
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

/-- Instance: Computation observation. -/
example : StateResult (accessOwner (shared) (⟨0⟩) (.shared)) :=
  @accessOwner_preserves shared ⟨0⟩ .shared (by decide)

/-- Planted: Computation observation. -/
example : (WellFormed (shared)) ∧ ¬ StateResult (accessOwnerConsumeRoot (shared) (⟨0⟩) (.shared)) := by decide

/-- Instance: Computation observation. -/
example : StateResult (accessLoan (child) (⟨1⟩) (.mutable)) :=
  @accessLoan_preserves child ⟨1⟩ .mutable (by decide)

/-- Planted: Computation observation. -/
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

/-- Planted (law `ConcreteSpec.Core.Own.leave_preserves`, case `ConcreteSpec.Core.Own.RoundThreeWitnesses.localLoan`, subject `ConcreteSpec.Core.Own.leave`, mutant `ConcreteSpec.Core.Own.PreservationEvidence.leaveKeepSlots`). -/
example : WellFormed RoundThreeWitnesses.localLoan ∧
    ¬ StateResult (leaveKeepSlots RoundThreeWitnesses.localLoan) := by decide

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
abbrev adoptionInput := PreservationEvidence.adoptionInput
/-- Planted: omit only slot teardown after loan cleanup and frame removal. -/
def leaveSlots (s : State) : Option State :=
  (leave s).map fun t => { t with slots := s.slots }
/-- Planted: add a refusal-bypassing exit without cleanup. -/
def leaveLocalLoans (s : State) : Option State :=
  match leave s with
  | some t => some t
  | none => some { s with frames := s.frames.tail }

/-- Planted: cleanup indiscriminately ends caller loans too. -/
def leaveAllLoans (s : State) : Option State :=
  (leave s).map fun t => { t with loans := [] }
/-- Planted: add a refusal-bypassing exit that strands its loans. -/
def leaveStranded (s : State) : Option State :=
  match leave s with
  | some t => some t
  | none => some { s with frames := s.frames.tail }

/-- Planted: mutable argument lending incorrectly issues a shared loan, permitting
shared owner access during the exclusive call. All bookkeeping still runs. -/
def passOwnerNoFreeze (s : CallState) (p : Hybrid.Place) (path : List Projection)
    (_ : Mode) (x : Var) : Option CallState := passOwner s p path .shared x
/-- Planted: only the fresh child targets an inactive activation. -/
def reborrowToDead (s : State) (i : LoanId) (path : List Projection) (m : Mode) (a : Nat) :=
  (reborrowTo s i path m a).map fun t => { t with loans := t.loans.map (fun (l : Loan) =>
    if l.id == LoanId.mk s.nextLoan then { l with activation := t.nextActivation } else l) }
/-- Planted: only adopted arguments target an inactive activation. -/
def adoptDownDead (s : State) (args : List (Var × LoanId)) :=
  (adoptDown s args).map fun t => { t with loans := t.loans.map (fun (l : Loan) =>
    if l.id ∈ (args.map Prod.snd) then { l with activation := t.nextActivation } else l) }

def leave_no_local_loanCase := (RoundThreeWitnesses.localLoan)

/-- Instance (law `ConcreteSpec.Core.Own.leave_no_local_loan`, case `ConcreteSpec.Core.Own.LifetimeEvidence.leave_no_local_loanCase`). -/
example : ResultSatisfies (fun t : State => ∀ l ∈ t.loans, l.root ≠ ((y, Hybrid.Place.mk 1)).2) (leave (RoundThreeWitnesses.localLoan)) :=
  @leave_no_local_loan RoundThreeWitnesses.localLoan ⟨1, [(y, ⟨1⟩)], []⟩ (y, ⟨1⟩) (rfl) (by decide) (.head [])

/-- Planted (law `ConcreteSpec.Core.Own.leave_no_local_loan`, case `ConcreteSpec.Core.Own.Examples.shared`, subject `ConcreteSpec.Core.Own.leave`, mutant `ConcreteSpec.Core.Own.LifetimeEvidence.leaveLocalLoans`). -/
example : shared.frames.head? = some (⟨0, [(x, ⟨0⟩)], []⟩ : Activation) ∧ WellFormed shared ∧
    ¬ ResultSatisfies (fun t : State => ∀ l ∈ t.loans, l.root ≠ ⟨0⟩) (leaveLocalLoans shared) := by decide

def leave_no_activation_slotCase := (RoundThreeWitnesses.localLoan)

/-- Instance (law `ConcreteSpec.Core.Own.leave_no_activation_slot`, case `ConcreteSpec.Core.Own.LifetimeEvidence.leave_no_activation_slotCase`). -/
example : ResultSatisfies (fun t : State => ∀ x ∈ t.slots, x.activation ≠ (⟨1, [(y, ⟨1⟩)], []⟩ : Activation).id) (leave (RoundThreeWitnesses.localLoan)) :=
  @leave_no_activation_slot RoundThreeWitnesses.localLoan ⟨1, [(y, ⟨1⟩)], []⟩ (by decide) (rfl)

/-- Planted (law `ConcreteSpec.Core.Own.leave_no_activation_slot`, case `ConcreteSpec.Core.Own.LifetimeEvidence.leave_no_activation_slotCase`, subject `ConcreteSpec.Core.Own.leave`, mutant `ConcreteSpec.Core.Own.LifetimeEvidence.leaveSlots`). -/
example : (WellFormed (RoundThreeWitnesses.localLoan)) ∧ ((RoundThreeWitnesses.localLoan).frames.head? = some (⟨1, [(y, ⟨1⟩)], []⟩ : Activation)) ∧ ¬ ResultSatisfies (fun t : State => ∀ x ∈ t.slots, x.activation ≠ (⟨1, [(y, ⟨1⟩)], []⟩ : Activation).id) (leaveSlots (RoundThreeWitnesses.localLoan)) := by decide

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

/-- Planted (law `ConcreteSpec.Core.Own.leave_no_activation_loan`, case `ConcreteSpec.Core.Own.Examples.shared`, subject `ConcreteSpec.Core.Own.leave`, mutant `ConcreteSpec.Core.Own.LifetimeEvidence.leaveLocalLoans`). -/
example : shared.frames.head? = some (⟨0, [(x, ⟨0⟩)], []⟩ : Activation) ∧
    ¬ ResultSatisfies (fun t : State => ∀ l ∈ t.loans, l.activation ≠ 0) (leaveLocalLoans shared) := by decide

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

/-- Planted (law `ConcreteSpec.Core.Own.leave_live`, case `ConcreteSpec.Core.Own.Examples.shared`, subject `ConcreteSpec.Core.Own.leave`, mutant `ConcreteSpec.Core.Own.LifetimeEvidence.leaveStranded`). -/
example : LiveDestinations shared ∧ shared.frames.head? = some (⟨0, [(x, ⟨0⟩)], []⟩ : Activation) ∧
    ¬ ResultSatisfies LiveDestinations (leaveStranded shared) := by decide

/-- A state whose active frame omits a slot of its activation from `f.locals` violates
the frame-local completeness clause of `WellFormed`. -/
def unindexedCopy : State :=
  { initial with
    slots := initial.slots.map fun x => { x with desc := { x.desc with copy := true } }
    frames := [⟨0, [], []⟩] }

/-- Planted: concrete ownership calculation. -/
example : ¬ WellFormed unindexedCopy := by decide
/-- Planted: concrete ownership calculation. -/
example : ¬ WellFormed { initial with frames := [⟨0, [], []⟩] } := by decide

/-- Planted: An issued but inactive activation cannot retain an owner obligation. -/
example : ¬ WellFormed { initial with frames := [⟨1, [], []⟩], nextActivation := 2 } := by decide

/-- Planted: A callee cannot index a caller's Copy owner as its own local. -/
example : ¬ WellFormed { initial with
    slots := initial.slots.map fun x => { x with desc := { x.desc with copy := true } }
    frames := [⟨1, [(y, ⟨0⟩)], []⟩, ⟨0, [(x, ⟨0⟩)], []⟩]
    nextActivation := 2 } := by decide

/-- Instance: Cleanup removes every alias of a removed place, so no local index dangles. -/
example : let s : State := { LifecycleWitnesses.moved with
    frames := [⟨0, [(x, ⟨0⟩), (y, ⟨0⟩)], []⟩] }
    WellFormed s ∧ (drop [x] s).isSome = true ∧
      WellFormed ((drop [x] s).getD initial) := by decide

end LifetimeEvidence

namespace FinalEvidence
open Examples

/-- Planted: entry wrongly forbids parameter shadowing of caller locals. -/
def enterRefuse (types : Var → Option Descriptor) (xs : List Var) (s : State) : Option State :=
  if xs.any (fun x => (s.frames.head?.bind fun f => AList.lookup f.locals x).isSome) then none
  else enter types xs s

/-- Instance: two distinct typed parameters include a name already bound in the caller. -/
example : [x, y].Nodup ∧ (enter types [x, y] shared).isSome = true := by decide
/-- Planted: forbidding caller shadowing rejects a permitted fresh callee frame. -/
example : [x, y].Nodup ∧ (∀ v ∈ [x, y], ∃ d, types v = some d) ∧
    ¬ (∃ t, enterRefuse types [x, y] shared = some t) :=
  ⟨by decide, (fun _ _ => ⟨_, rfl⟩), by change ¬ (∃ t, (none : Option State) = some t); simp⟩

/-- Planted: omit endLocals only, throughout the call lifecycle adapter. -/
def leaveLeaked (s : State) : Option State := do
  let f ← s.frames.head?
  let t ← drop (List.map Prod.fst f.locals) s
  pure { t with frames := t.frames.tail }
/-- Planted: every leave site uses the same cleanup omission; other events are unchanged. -/
def callLifeLeaked (types : Var → Option Descriptor) (e : Exec.Life) (s : CallState) : Option CallState :=
  match e with
  | .leave => if s.pending = [] then (leaveLeaked s.own).map fun own => { s with own := own } else none
  | _ => callLife types e s

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

/-- Planted (law `ConcreteSpec.Core.Own.directOwner_roundtrip`, case `ConcreteSpec.Core.Own.DirectArgumentWitnesses.caller`, subject `ConcreteSpec.Core.Own.callLife`, mutant `ConcreteSpec.Core.Own.FinalEvidence.callLifeLeaked`). -/
example : WellFormed DirectArgumentWitnesses.caller.own ∧
    DirectArgumentWitnesses.caller.own.frames.head? = some callerFrame ∧
    callerFrame.id < DirectArgumentWitnesses.caller.own.nextActivation ∧
    ownerAccess DirectArgumentWitnesses.caller.own ⟨1⟩ .mutable = true ∧
    ¬ ((do
      let waiting ← passOwner DirectArgumentWitnesses.caller ⟨1⟩ [.index 3] .mutable DirectArgumentWitnesses.z
      let called ← callLifeLeaked types (.enter [DirectArgumentWitnesses.z]) waiting
      callLifeLeaked types .leave called) =
      some ⟨{ DirectArgumentWitnesses.caller.own with
        nextLoan := DirectArgumentWitnesses.caller.own.nextLoan + 1
        nextActivation := DirectArgumentWitnesses.caller.own.nextActivation + 1 }, []⟩) := by decide

/-- Instance: The roundtrip mutant succeeds through `passOwner`, `enter` and `leave` (`isSome = true`),
so the planted refutation above fails because the returned state leaks a loan, not because a step
returns `none`. -/
example : (do
    let waiting ← passOwner DirectArgumentWitnesses.caller ⟨1⟩ [.index 3] .mutable DirectArgumentWitnesses.z
    let called ← callLifeLeaked types (.enter [DirectArgumentWitnesses.z]) waiting
    callLifeLeaked types .leave called).isSome = true := by decide

end FinalEvidence


namespace SuccessfulInstances
open Examples

/-- Instance: preservation and lifetime fixtures actually take their successful branches.
In particular, optional postconditions cannot hide a vacuous refusal in their tests. -/
example : (reborrow exclusive ⟨0⟩ [.index 1] .mutable).isSome = true ∧
    (enterDown exclusive [(y, ⟨0⟩)]).isSome = true ∧
    (life types (.bind y) shared).isSome = true ∧
    (leave RoundThreeWitnesses.localLoan).isSome = true ∧
    (adoptDown PreservationEvidence.adoptionInput DirectArgumentWitnesses.waiting.pending).isSome = true ∧
    (passDown ⟨exclusive, []⟩ ⟨0⟩ y).isSome = true ∧
    (callLife types (.enter [y]) CallWitnesses.waiting).isSome = true ∧
    (passOwner DirectArgumentWitnesses.caller ⟨1⟩ [.index 3] .mutable
      DirectArgumentWitnesses.z).isSome = true := by decide

end SuccessfulInstances


namespace RoundTwoEvidence
open Examples

/-- Two moved slots, including a nonzero target; frame indices are fully populated. -/
def twoMoved : State :=
  { LifecycleWitnesses.moved with
    nextPlace := 2
    slots := ⟨⟨1⟩, 0, ⟨.int .i32, false, true⟩, .moved⟩ :: LifecycleWitnesses.moved.slots
    frames := [⟨0, [(x, ⟨0⟩), (y, ⟨1⟩)], []⟩] }
/-- Planted: reinitialization ignores the requested place, always restoring place zero. -/
def restoreZero (s : State) (_ : Hybrid.Place) := reinitialize s ⟨0⟩
/-- Planted: reinitialization restores every moved slot instead of only its target. -/
def restoreAll (s : State) (p : Hybrid.Place) : Option State := do
  let _ ← reinitialize s p
  pure { s with slots := s.slots.map fun x => { x with status := .owned } }
/-- Instance: a nonzero target is restored and the other moved obligation remains moved.
This pins the selected-place behavior in addition to the preservation law's weaker invariant. -/
example : WellFormed twoMoved ∧ StateResult (reinitialize twoMoved ⟨1⟩) ∧
    ((reinitialize twoMoved ⟨1⟩).getD initial).slots.map Slot.status = [.owned, .moved] := by decide
/-- Planted: each wrong selector fails the same populated selected-place observation. -/
example : ((restoreZero twoMoved ⟨1⟩).getD initial).slots.map Slot.status ≠ [.owned, .moved] ∧
    ((restoreAll twoMoved ⟨1⟩).getD initial).slots.map Slot.status ≠ [.owned, .moved] := by decide

/-- Instance: a shared direct argument freezes owner writes while preserving shared reads. -/
example : (passOwner DirectArgumentWitnesses.caller ⟨1⟩ [] .shared y).isSome = true ∧
    ResultSatisfies (fun t : CallState => ownerAccess t.own ⟨1⟩ .mutable = false ∧
      ownerAccess t.own ⟨1⟩ .shared = true)
      (passOwner DirectArgumentWitnesses.caller ⟨1⟩ [] .shared y) := by decide
/-- Instance: return ends a populated callRoot loan, not just local non-call loans. -/
example : DirectArgumentWitnesses.called.own.loans.any Loan.callRoot = true ∧
    (leave DirectArgumentWitnesses.called.own).isSome = true ∧
    ResultSatisfies (fun t : State => ∀ l ∈ t.loans, l.activation ≠ 1)
      (leave DirectArgumentWitnesses.called.own) := by decide

/-- Planted: omit only endLocals; dropping an empty view-only frame still succeeds. -/
def leaveWithoutEnding (s : State) : Option State := do
  let f ← s.frames.head?
  let t ← drop (List.map Prod.fst f.locals) s
  pure { t with frames := t.frames.tail }
/-- Planted: the callRoot survives the omitted cleanup and targets a departed activation. -/
example : (leaveWithoutEnding DirectArgumentWitnesses.called.own).isSome = true ∧
    ¬ ResultSatisfies (fun t : State => ∀ l ∈ t.loans, l.activation ≠ 1)
      (leaveWithoutEnding DirectArgumentWitnesses.called.own) ∧
    ¬ ResultSatisfies LiveDestinations (leaveWithoutEnding DirectArgumentWitnesses.called.own) := by decide

/-- Planted: add an exit transition on refusal, popping the frame without teardown.
Successful exits are unchanged. This tests absence on an added transition. -/
def leaveBypass (s : State) : Option State :=
  match leave s with
  | some t => some t
  | none => some { s with frames := s.frames.tail }
/-- Planted: slot exclusion, root-loan exclusion, activation-loan exclusion and live destinations
all fail on the added transition, with the original laws' premises satisfied. -/
example : WellFormed shared ∧ LiveDestinations shared ∧
    shared.frames.head? = some (⟨0, [(x, ⟨0⟩)], []⟩ : Activation) ∧
    (x, Hybrid.Place.mk 0) ∈ (⟨0, [(x, ⟨0⟩)], []⟩ : Activation).locals ∧ leave shared = none ∧
    ¬ ResultSatisfies (fun t : State => ∀ x ∈ t.slots, x.activation ≠ 0) (leaveBypass shared) ∧
    ¬ ResultSatisfies (fun t : State => ∀ l ∈ t.loans, l.root ≠ ⟨0⟩) (leaveBypass shared) ∧
    ¬ ResultSatisfies (fun t : State => ∀ l ∈ t.loans, l.activation ≠ 0) (leaveBypass shared) ∧
    ¬ ResultSatisfies LiveDestinations (leaveBypass shared) ∧
    ¬ StateResult (leaveBypass shared) := by decide
/-- Planted: substitute the same broken exit at every lifecycle leave site. -/
def lifeBypass (types : Var → Option Descriptor) : Exec.Life → State → Option State
  | .leave, s => leaveBypass s
  | e, s => life types e s
/-- Planted: the call adapter delegates every non-entry event to the substituted lifecycle. -/
def callBypass (types : Var → Option Descriptor) (e : Exec.Life) (s : CallState) : Option CallState :=
  match e with
  | .enter _ => callLife types e s
  | _ => if s.pending = [] then (lifeBypass types e s.own).map fun own => { s with own := own }
    else none
/-- Planted: the added transition also defeats derived lifecycle and call preservation closures. -/
example : WellFormed shared ∧ CallState.WellFormed ⟨shared, []⟩ ∧
    ¬ StateResult (lifeBypass types .leave shared) ∧
    ¬ CallOwnResult (callBypass types .leave ⟨shared, []⟩) ∧
    ¬ CallResult (callBypass types .leave ⟨shared, []⟩) := by decide
/-- Planted: an added reborrow transition accepts an unissued destination, changing only
refusal at the destination guard. Loan identity, root, ancestry and permissions are retained. -/
def reborrowBypass (s : State) (i : LoanId) (a : Nat) : Option State := do
  let l ← loan s i
  if loanAccess s i l.mode then
    pure { s with
      nextLoan := s.nextLoan + 1
      loans := ⟨⟨s.nextLoan⟩, l.root, l.path, l.mode, some i,
        i :: l.ancestors, a, false, false⟩ :: s.loans }
  else none
/-- Planted: the added destination transition violates reborrowTo_live with live input. -/
example : LiveDestinations exclusive ∧ reborrowTo exclusive ⟨0⟩ [] .mutable 1 = none ∧
    ¬ ResultSatisfies LiveDestinations (reborrowBypass exclusive ⟨0⟩ 1) := by decide
end RoundTwoEvidence

end ConcreteSpec.Core.Own
