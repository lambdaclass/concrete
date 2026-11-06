import ConcreteSpec.Proofs.Basics

/-!
# Part 5: `reset_fresh` and the brand refusals.

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). See `ConcreteSpec/README.md`.
-/

namespace ConcreteSpec.Brand

theorem findL_mem {α : Type} : ∀ {l : List (Nat × α)} {p : Nat} {v : α},
    findL l p = some v → (p, v) ∈ l
  | [], _, _, h => by simp [findL] at h
  | (n, w) :: l, p, v, h => by
    simp only [findL] at h
    split at h
    · next hn => cases h; subst hn; exact List.mem_cons_self ..
    · exact List.mem_cons_of_mem _ (findL_mem h)

theorem mem_removeL {α : Type} (p : Nat) : ∀ {l : List (Nat × α)} {e : Nat × α},
    e ∈ removeL p l → e ∈ l
  | [], _, h => by simp [removeL] at h
  | (n, w) :: l, e, h => by
    simp only [removeL] at h
    split at h
    · exact List.mem_cons_of_mem _ (mem_removeL p h)
    · rcases List.mem_cons.mp h with rfl | h
      · exact List.mem_cons_self ..
      · exact List.mem_cons_of_mem _ (mem_removeL p h)

theorem findL_removeL {α : Type} (p' : Nat) : ∀ (l : List (Nat × α)) (p : Nat),
    findL (removeL p' l) p = if p = p' then none else findL l p
  | [], _ => by simp [removeL, findL]
  | (n, q) :: l, p => by
    have ih := findL_removeL p' l p
    by_cases hn : n = p' <;> by_cases hp : p = p' <;> by_cases hnp : n = p <;>
      simp_all [removeL, findL] <;> omega

/-- Brand equality is nominal (Q1.4): an operation that pairs a value of one brand with an
allocator of another is refused. -/
theorem brand_mismatch_refused {B Γ h a β1 β2 r Γ'} (hh : findL Γ h = some (.heap β1))
    (ha : findL Γ a = some (.alloc β2)) (hne : β1 ≠ β2) :
    ¬ Ok B Γ (.use h a :: r) Γ' ∧ ¬ Ok B Γ (.free h a :: r) Γ' := by
  refine ⟨fun hok => ?_, fun hok => ?_⟩
  · cases hok with
    | use h1 h2 _ => rw [hh] at h1; rw [ha] at h2; cases h1; cases h2; exact hne rfl
  · cases hok with
    | free h1 h2 _ => rw [hh] at h1; rw [ha] at h2; cases h1; cases h2; exact hne rfl

/-- A checked reset leaves no value of the old brand (Q1.5): none is live at the reset. -/
theorem reset_requires_no_old {B Γ a' a β' rest Γ' β} (h : Ok B Γ (.reset a' a β' :: rest) Γ')
    (ha : findL Γ a = some (.alloc β)) : ∀ hv, findL Γ hv ≠ some (.heap β) := by
  intro hv hh
  cases h with
  | reset ha' hno _ _ _ =>
    rw [ha] at ha'; cases ha'
    exact hno _ (findL_mem hh) rfl

/-- **`reset_fresh`, static part** (Q1.2, Q1.4). The brand of a reset allocator differs from the
brand of every value live before the reset, so each operation of the new allocator on such a
value (`use`, `free`) is refused. Values of the old brand cannot be live at a checked reset
(`reset_requires_no_old`); one that reached the new allocator anyway would be refused by
`brand_mismatch_refused`, and the ghost run refuses it too (`reset_fresh`, part 3). -/
theorem reset_refuses_old {B Γ a' a β' rest Γ'} (hw : BrandsIn B Γ)
    (h : Ok B Γ (.reset a' a β' :: rest) Γ') {hv βh} (hh : findL Γ hv = some (.heap βh)) :
    βh ≠ β' ∧ ∀ r, rest = .use hv a' :: r ∨ rest = .free hv a' :: r → False := by
  cases h with
  | @reset _ _ _ _ β _ _ _ ha hno hfresh hb hrest =>
    have hβh : βh ∈ B := hw _ (findL_mem hh) βh (Or.inr rfl)
    have hne : βh ≠ β' := fun e => hb (e ▸ hβh)
    have hva : hv ≠ a := by
      intro e; subst e; rw [ha] at hh; cases hh
    have hold : findL (removeL a Γ) hv = some (.heap βh) := by
      rw [findL_removeL]; simp only [hva, ↓reduceIte]; exact hh
    have hva' : hv ≠ a' := by
      intro e; subst e; rw [hold] at hfresh; cases hfresh
    have hfind : findL ((a', STy.alloc β') :: removeL a Γ) hv = some (.heap βh) := by
      simp only [findL, show ¬ a' = hv from fun e => hva' e.symm, ↓reduceIte]; exact hold
    have hnew : findL ((a', STy.alloc β') :: removeL a Γ) a' = some (.alloc β') := by
      simp [findL]
    refine ⟨hne, fun r hr => ?_⟩
    rcases hr with rfl | rfl
    · cases hrest with
      | use h1 h2 _ => rw [hfind] at h1; rw [hnew] at h2; cases h1; cases h2; exact hne rfl
    · cases hrest with
      | free h1 h2 _ => rw [hfind] at h1; rw [hnew] at h2; cases h1; cases h2; exact hne rfl

/-- Every live brand and every generated brand is below the counter, and the generated brands
are pairwise distinct. -/
def Inv (s : State) : Prop :=
  (∀ e ∈ s.env, brandOf e.2 < s.next) ∧ (∀ b ∈ s.gen, b < s.next) ∧ s.gen.Nodup

theorem nodup_snoc : ∀ {l : List Nat} {x : Nat}, l.Nodup → x ∉ l → (l ++ [x]).Nodup
  | [], _, _, _ => by simp
  | y :: l, x, h, hx => by
    rw [List.nodup_cons] at h
    simp only [List.cons_append]
    rw [List.nodup_cons]
    refine ⟨?_, nodup_snoc h.2 (fun hm => hx (List.mem_cons_of_mem _ hm))⟩
    simp only [List.mem_append, List.mem_singleton, not_or]
    exact ⟨h.1, fun e => hx (by simp [e])⟩

theorem inv_bind {s : State} (a : Nat) {env : List (Nat × Val)} (hi : Inv s)
    (henv : ∀ e ∈ env, brandOf e.2 < s.next) : Inv (s.bind a env) := by
  obtain ⟨_, hg, hn⟩ := hi
  refine ⟨?_, ?_, ?_⟩
  · intro e he
    simp only [State.bind, List.mem_cons] at he ⊢
    rcases he with rfl | he
    · simp [brandOf]
    · have := henv e he; omega
  · intro b hb
    simp only [State.bind, List.mem_append, List.mem_singleton] at hb ⊢
    rcases hb with hb | rfl
    · have := hg b hb; omega
    · omega
  · exact nodup_snoc hn (fun hm => by have := hg _ hm; omega)

/-- The ghost run keeps the invariant. -/
theorem run_inv {s ss s'} (h : Run s ss s') : Inv s → Inv s' := by
  induction h with
  | nil => exact id
  | newAlloc _ ih => intro hi; exact ih (inv_bind _ hi hi.1)
  | alloc ha _ ih =>
    intro hi
    refine ih ⟨?_, hi.2.1, hi.2.2⟩
    intro e he
    rcases List.mem_cons.mp he with rfl | he
    · have := hi.1 _ (findL_mem ha); simpa [brandOf] using this
    · exact hi.1 e he
  | use _ _ _ ih => exact ih
  | free _ _ _ ih => intro hi; exact ih ⟨fun e he => hi.1 e (mem_removeL _ he), hi.2.1, hi.2.2⟩
  | release _ _ ih => intro hi; exact ih ⟨fun e he => hi.1 e (mem_removeL _ he), hi.2.1, hi.2.2⟩
  | reset _ _ ih => intro hi; exact ih (inv_bind _ hi (fun e he => hi.1 e (mem_removeL _ he)))
  | whileExit _ ih => exact ih
  | whileIter _ ih => exact ih

theorem inv_init : Inv State.init := ⟨by simp [State.init], by simp [State.init], by simp [State.init]⟩

/-- **`reset_fresh`** (Q9.14, Q1.2, Q1.4, Q1.5).

1. The brand a reset gives the new allocator differs from the brand of every value live at the
   reset, in any reachable ghost state.
2. Every brand a run generates (each `new` and each `reset`, in every iteration of every loop,
   for every outcome of the loop conditions) differs from every other: the generated brands are
   pairwise distinct.
3. The new allocator's operations refuse the old brand: statically (`reset_refuses_old`,
   `brand_mismatch_refused`), and in the ghost run, where `use` or `free` through the new
   allocator of any value live before the reset, of the old brand or any other, is stuck. -/
theorem reset_fresh :
    (∀ {s a' a β' rest s'}, Inv s → Run s (.reset a' a β' :: rest) s' →
      ∃ mid, mid.env = (a', .allocV s.next) :: removeL a s.env ∧ Run mid rest s' ∧
        ∀ e ∈ s.env, brandOf e.2 ≠ s.next) ∧
    (∀ {prog s'}, Run State.init prog s' → s'.gen.Nodup) ∧
    (∀ {s a' a β' h r s'}, Inv s → (∃ v, findL s.env h = some v) → h ≠ a' →
      ¬ Run s (.reset a' a β' :: .use h a' :: r) s' ∧
      ¬ Run s (.reset a' a β' :: .free h a' :: r) s') := by
  refine ⟨?_, ?_, ?_⟩
  · intro s a' a β' rest s' hi h
    cases h with
    | reset _ hr =>
      exact ⟨_, rfl, hr, fun e he => Nat.ne_of_lt (hi.1 e he)⟩
  · intro prog s' h
    exact (run_inv h inv_init).2.2
  · intro s a' a β' h r s' hi ⟨v, hv⟩ hne
    have key : ∀ b, findL ((a', Val.allocV s.next) :: removeL a s.env) h = some (.heapV b) →
        b ≠ s.next := by
      intro b hb
      simp only [findL, show ¬ a' = h from fun e => hne e.symm, ↓reduceIte] at hb
      exact Nat.ne_of_lt (hi.1 _ (findL_mem (α := Val) (l := removeL a s.env) hb |> mem_removeL a))
    have hnew : findL ((a', Val.allocV s.next) :: removeL a s.env) a' = some (.allocV s.next) := by
      simp [findL]
    have _ := hv
    refine ⟨fun hrun => ?_, fun hrun => ?_⟩
    · cases hrun with
      | reset _ hr =>
        cases hr with
        | use h1 h2 _ =>
          simp only [State.bind] at h1 h2
          rw [hnew] at h2; cases h2; exact key _ h1 rfl
    · cases hrun with
      | reset _ hr =>
        cases hr with
        | free h1 h2 _ =>
          simp only [State.bind] at h1 h2
          rw [hnew] at h2; cases h2; exact key _ h1 rfl

end ConcreteSpec.Brand
