import ConcreteSpec.Proofs.Basics

/-!
# Part 5: `reset_fresh` and the brand refusals.

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). See `ConcreteSpec/README.md`.
-/

namespace ConcreteSpec.Brand
open Core (AList)

/-- Law (subject `ConcreteSpec.Brand.Ok`). Brand equality is nominal (Q1.4): an operation that pairs a value of one brand with an
allocator of another is refused. -/
theorem brand_mismatch_refused {B Γ h a β1 β2 r Γ'} (hh : AList.lookup Γ h = some (.heap β1))
    (ha : AList.lookup Γ a = some (.alloc β2)) (hne : β1 ≠ β2) :
    ¬ Ok B Γ (.use h a :: r) Γ' ∧ ¬ Ok B Γ (.free h a :: r) Γ' := by
  refine ⟨fun hok => ?_, fun hok => ?_⟩
  · cases hok with
    | use h1 h2 _ => rw [hh] at h1; rw [ha] at h2; cases h1; cases h2; exact hne rfl
  · cases hok with
    | free h1 h2 _ => rw [hh] at h1; rw [ha] at h2; cases h1; cases h2; exact hne rfl

/-- Law (subject `ConcreteSpec.Brand.Ok`). A checked reset leaves no value of the old brand (Q1.5): none is live at the reset. -/
theorem reset_requires_no_old {B Γ a' a β' rest Γ' β hv}
    (ha : AList.lookup Γ a = some (.alloc β)) (hh : AList.lookup Γ hv = some (.heap β)) :
    ¬ Ok B Γ (.reset a' a β' :: rest) Γ' := by
  intro h
  cases h with
  | reset ha' hno _ _ _ =>
    rw [ha] at ha'; cases ha'
    exact hno _ (AList.lookup_mem hh) rfl

/-- Law (subject `ConcreteSpec.Brand.Ok`). **reset_fresh, static part** (Q1.2, Q1.4). The brand of a reset allocator differs from the
brand of every value live before the reset, so each operation of the new allocator on such a
value (use, free) is refused. Values of the old brand cannot be live at a checked reset
(reset_requires_no_old); one that reached the new allocator anyway would be refused by
brand_mismatch_refused, and the ghost run refuses it too (reset_fresh, part 3). -/
theorem reset_refuses_old {B Γ a' a β' rest Γ' hv βh} (hw : BrandsIn B Γ)
    (hh : AList.lookup Γ hv = some (.heap βh))
    (hbad : βh = β' ∨ ∃ r, rest = .use hv a' :: r ∨ rest = .free hv a' :: r) :
    ¬ Ok B Γ (.reset a' a β' :: rest) Γ' := by
  intro h
  cases h with
  | @reset _ _ _ _ β _ _ _ ha hno hfresh hb hrest =>
    have hβh : βh ∈ B := hw _ (AList.lookup_mem hh) βh (Or.inr rfl)
    have hne : βh ≠ β' := fun e => hb (e ▸ hβh)
    have hva : hv ≠ a := by
      intro e; subst e; rw [ha] at hh; cases hh
    have hold : AList.lookup (AList.erase Γ a) hv = some (.heap βh) := by
      rw [AList.lookup_erase]; simp only [hva, ↓reduceIte]; exact hh
    have hva' : hv ≠ a' := by
      intro e; subst e; rw [hold] at hfresh; cases hfresh
    have hfind : AList.lookup ((a', STy.alloc β') :: AList.erase Γ a) hv = some (.heap βh) := by
      simp only [AList.lookup, show ¬ a' = hv from fun e => hva' e.symm, ↓reduceIte]; exact hold
    have hnew : AList.lookup ((a', STy.alloc β') :: AList.erase Γ a) a' = some (.alloc β') := by
      simp [AList.lookup]
    rcases hbad with he | ⟨r, hr⟩
    · exact hne he
    rcases hr with rfl | rfl
    · cases hrest with
      | use h1 h2 _ => rw [hfind] at h1; rw [hnew] at h2; cases h1; cases h2; exact hne rfl
    · cases hrest with
      | free h1 h2 _ => rw [hfind] at h1; rw [hnew] at h2; cases h1; cases h2; exact hne rfl

/-- Every live brand and every generated brand is below the counter, and the generated brands
are pairwise distinct. -/
def Inv (s : State) : Prop :=
  (∀ e ∈ s.env, brandOf e.2 < s.next) ∧ (∀ b ∈ s.gen, b < s.next) ∧ s.gen.Nodup

/-- Helper (inversion). Extend a no-duplicate list with a member excluded by the freshness premise. (approved: REV-base1b-agy-01025713b). -/
theorem nodup_snoc : ∀ {l : List Nat} {x : Nat}, l.Nodup → x ∉ l → (l ++ [x]).Nodup
  | [], _, _, _ => by simp
  | y :: l, x, h, hx => by
    rw [List.nodup_cons] at h
    simp only [List.cons_append]
    rw [List.nodup_cons]
    refine ⟨?_, nodup_snoc h.2 (fun hm => hx (List.mem_cons_of_mem _ hm))⟩
    simp only [List.mem_append, List.mem_singleton, not_or]
    exact ⟨h.1, fun e => hx (by simp [e])⟩

/-- Law (subject `ConcreteSpec.Brand.State.bind`). -/
theorem inv_bind {s : State} (a : Nat) {env : AList Nat Val} (hi : Inv s)
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

/-- Helper (inversion). Induction transports the input invariant through the single fixed run derivation; no premise-preserving run mutant tests its fixed output. (approved: PENDING). -/
theorem run_inv {s ss s'} (h : Run s ss s') : Inv s → Inv s' := by
  induction h with
  | nil => exact id
  | newAlloc _ ih => intro hi; exact ih (inv_bind _ hi hi.1)
  | alloc ha _ ih =>
    intro hi
    refine ih ⟨?_, hi.2.1, hi.2.2⟩
    intro e he
    rcases List.mem_cons.mp he with rfl | he
    · have := hi.1 _ (AList.lookup_mem ha); simpa [brandOf] using this
    · exact hi.1 e he
  | use _ _ _ ih => exact ih
  | free _ _ _ ih => intro hi; exact ih ⟨fun e he => hi.1 e (AList.mem_of_mem_erase _ he), hi.2.1, hi.2.2⟩
  | release _ _ ih => intro hi; exact ih ⟨fun e he => hi.1 e (AList.mem_of_mem_erase _ he), hi.2.1, hi.2.2⟩
  | reset _ _ ih => intro hi; exact ih (inv_bind _ hi (fun e he => hi.1 e (AList.mem_of_mem_erase _ he)))
  | whileExit _ ih => exact ih
  | whileIter _ ih => exact ih

/-- Representation lemma. The empty initialization fields reduce the bookkeeping invariant to empty-list identities. (approved: REV-base1b-agy-01025713b). -/
theorem inv_init : Inv State.init := ⟨by simp [State.init], by simp [State.init], by simp [State.init]⟩

/-- Law (subject `ConcreteSpec.Brand.Run`). Reset creates a brand different from every
live value's brand and continues with the freshly bound allocator. -/
theorem reset_fresh_brand {s : State} {a' a β' : Nat} {rest : List Stmt} {s' : State}
    (hi : Inv s) (h : Run s (.reset a' a β' :: rest) s') :
    ∃ mid, mid.env = (a', .allocV s.next) :: AList.erase s.env a ∧ Run mid rest s' ∧
      ∀ e ∈ s.env, brandOf e.2 ≠ s.next := by
  cases h with
  | reset _ hr => exact ⟨_, rfl, hr, fun e he => Nat.ne_of_lt (hi.1 e he)⟩

/-- Helper (projection). Specializes run preservation at initialization and extracts generated-brand uniqueness. (approved: PENDING). -/
theorem reset_fresh_nodup {prog : List Stmt} {s' : State} (h : Run State.init prog s') : s'.gen.Nodup := by
  exact (run_inv h inv_init).2.2

/-- Law (subject `ConcreteSpec.Brand.Run`). After reset, use and free through the new
allocator refuse every other handle. A pre-reset lookup premise is unnecessary: the use/free
rules themselves require the handle to exist, and the state invariant bounds its old brand. -/
theorem reset_fresh_refuses {s : State} {a' a β' h : Nat} {r : List Stmt} {s' : State}
    (hi : Inv s) (hne : h ≠ a') :
    ¬ Run s (.reset a' a β' :: .use h a' :: r) s' ∧
    ¬ Run s (.reset a' a β' :: .free h a' :: r) s' := by
  have key : ∀ b, AList.lookup ((a', Val.allocV s.next) :: AList.erase s.env a) h = some (.heapV b) →
      b ≠ s.next := by
    intro b hb
    simp only [AList.lookup, show ¬ a' = h from fun e => hne e.symm, ↓reduceIte] at hb
    exact Nat.ne_of_lt (hi.1 _ (AList.lookup_mem (α := Val) (l := AList.erase s.env a) hb |> AList.mem_of_mem_erase a))
  have hnew : AList.lookup ((a', Val.allocV s.next) :: AList.erase s.env a) a' = some (.allocV s.next) := by
    simp [AList.lookup]
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


/-! ### Erasure (Q1.1): a checked program passes every ghost brand check -/

/-- A static type's run-time value under the brand assignment φ. -/
def emb (φ : Nat → Nat) : STy → Val
  | .alloc β => .allocV (φ β)
  | .heap β => .heapV (φ β)

/-- The run-time values match the static context under the brand assignment φ: the same
names, and an allocator or value of brand β carries the run-time brand φ β. -/
def Rel (φ : Nat → Nat) (Γ : Ctx) (env : AList Nat Val) : Prop :=
  ∀ x, AList.lookup env x = (AList.lookup Γ x).map (emb φ)

/-- φ with brand β assigned v. -/
def upd (φ : Nat → Nat) (β v : Nat) : Nat → Nat := fun γ => if γ = β then v else φ γ

/-- Representation lemma. Transport the static-to-ghost representation across context extension, erasure or fresh brand renaming; no operational soundness is claimed here. (approved: REV-base1b-agy-01025713b). -/
theorem rel_cons {φ Γ env} (x : Nat) (t : STy) (h : Rel φ Γ env) :
    Rel φ ((x, t) :: Γ) ((x, emb φ t) :: env) := by
  intro y; by_cases hx : x = y <;> simp [AList.lookup, hx, h y]

/-- Representation lemma. Transport the static-to-ghost representation across context extension, erasure or fresh brand renaming; no operational soundness is claimed here. (approved: REV-base1b-agy-01025713b). -/
theorem rel_erase {φ Γ env} (p : Nat) (h : Rel φ Γ env) : Rel φ (AList.erase Γ p) (AList.erase env p) := by
  intro y; rw [AList.lookup_erase, AList.lookup_erase]; by_cases hy : y = p <;> simp [hy, h y]

/-- Representation lemma. Transport the static-to-ghost representation across context extension, erasure or fresh brand renaming; no operational soundness is claimed here. (approved: REV-base1b-agy-01025713b). Reassigning a brand no binding has changes no binding's run-time value. -/
theorem emb_upd {B Γ} {φ : Nat → Nat} {β v x : Nat} {t : STy} (hb : BrandsIn B Γ) (hβ : β ∉ B)
    (hf : AList.lookup Γ x = some t) : emb (upd φ β v) t = emb φ t := by
  have hm := AList.lookup_mem hf
  cases t with
  | alloc γ =>
    have : γ ≠ β := fun e => hβ (e ▸ hb _ hm γ (.inl rfl))
    simp [emb, upd, this]
  | heap γ =>
    have : γ ≠ β := fun e => hβ (e ▸ hb _ hm γ (.inr rfl))
    simp [emb, upd, this]

/-- Representation lemma. Transport the static-to-ghost representation across context extension, erasure or fresh brand renaming; no operational soundness is claimed here. (approved: REV-base1b-agy-01025713b). -/
theorem rel_upd {φ Γ env} {B : List Nat} {β : Nat} (v : Nat) (h : Rel φ Γ env) (hb : BrandsIn B Γ)
    (hβ : β ∉ B) : Rel (upd φ β v) Γ env := by
  intro y
  rw [h y]
  cases hf : AList.lookup Γ y with
  | none => rfl
  | some t => simp [emb_upd hb hβ hf]

/-- Representation lemma. Transport the static-to-ghost representation across context extension, erasure or fresh brand renaming; no operational soundness is claimed here. (approved: REV-base1b-agy-01025713b). -/
theorem brandsIn_erase {B Γ} (p : Nat) (h : BrandsIn B Γ) : BrandsIn B (AList.erase Γ p) :=
  fun e he => h e (AList.mem_of_mem_erase p he)

/-- Representation lemma. Transport the static-to-ghost representation across context extension, erasure or fresh brand renaming; no operational soundness is claimed here. (approved: REV-base1b-agy-01025713b). -/
theorem brandsIn_mono {B Γ} (β : Nat) (h : BrandsIn B Γ) : BrandsIn (β :: B) Γ :=
  fun e he γ hγ => List.mem_cons_of_mem _ (h e he γ hγ)

/-- Representation lemma. Transport the static-to-ghost representation across context extension, erasure or fresh brand renaming; no operational soundness is claimed here. (approved: REV-base1b-agy-01025713b). -/
theorem brandsIn_cons {B Γ} {x : Nat} {t : STy} (h : BrandsIn B Γ)
    (ht : ∀ β, (t = .alloc β ∨ t = .heap β) → β ∈ B) : BrandsIn B ((x, t) :: Γ) := by
  intro e he β hβ
  rcases List.mem_cons.mp he with rfl | he
  · exact ht β hβ
  · exact h e he β hβ

/-- Representation lemma. Transport the static-to-ghost representation across context extension, erasure or fresh brand renaming; no operational soundness is claimed here. (approved: REV-base1b-agy-01025713b). A fresh allocator binding keeps the brands in use. -/
theorem brandsIn_new {B Γ} (x β : Nat) (h : BrandsIn B Γ) : BrandsIn (β :: B) ((x, .alloc β) :: Γ) :=
  brandsIn_cons (brandsIn_mono β h) (fun γ hγ => by
    rcases hγ with hγ | hγ <;> cases hγ; exact List.mem_cons_self ..)

/-- Representation lemma. Transport the static-to-ghost representation across context extension, erasure or fresh brand renaming; no operational soundness is claimed here. (approved: REV-base1b-agy-01025713b). Opening allocator a, now of brand γ, under the new name β: what each loop iteration,
and the code after the loop, starts from (Q1.3). -/
theorem rel_reopen {φ Γ env} {B : List Nat} {a γ β : Nat}
    (h : Rel φ ((a, .alloc γ) :: AList.erase Γ a) env) (hb : BrandsIn B Γ) (hβ : β ∉ B) :
    Rel (upd φ β (φ γ)) ((a, .alloc β) :: AList.erase Γ a) env := by
  intro y
  rw [h y]
  by_cases hy : a = y
  · simp [AList.lookup, hy, emb, upd]
  · simp only [AList.lookup, hy, ↓reduceIte]
    cases hf : AList.lookup (AList.erase Γ a) y with
    | none => rfl
    | some t => simp [emb_upd (brandsIn_erase a hb) hβ hf]

/-- Representation lemma. Transport the static-to-ghost representation across context extension, erasure or fresh brand renaming; no operational soundness is claimed here. (approved: REV-base1b-agy-01025713b). Entering a loop: the allocator's current binding becomes the loop's. -/
theorem rel_split {φ Γ env} {a γ : Nat} (h : Rel φ Γ env) (ha : AList.lookup Γ a = some (.alloc γ)) :
    Rel φ ((a, .alloc γ) :: AList.erase Γ a) env := by
  intro y
  rw [h y]
  by_cases hy : a = y
  · subst hy; simp [AList.lookup, ha]
  · simp only [AList.lookup, hy, ↓reduceIte, AList.lookup_erase, show ¬ y = a from fun e => hy e.symm]

/-- Law (subject `ConcreteSpec.Brand.ghost`). What a checked statement list does in the ghost run: it runs out of fuel, or finishes in a
state that matches its final context. It never gets stuck. -/
theorem ghost_ok {σ : Nat → Bool} {B Γ ss Γ'} (h : Ok B Γ ss Γ') :
    ∀ (n k : Nat) (φ : Nat → Nat) (s : State), Rel φ Γ s.env → BrandsIn B Γ →
      ghost σ n k s ss = .outOfFuel ∨
        ∃ s' k' φ', ghost σ n k s ss = .done s' k' ∧ Rel φ' Γ' s'.env := by
  induction h with
  | nil =>
    intro n k φ s hr _
    cases n with
    | zero => exact .inl rfl
    | succ n => exact .inr ⟨s, k, φ, rfl, hr⟩
  | @newAlloc B Γ a β rest Γ' _ hβ _ ih =>
    intro n k φ s hr hb
    cases n with
    | zero => exact .inl rfl
    | succ n =>
      simp only [ghost]
      exact ih n k (upd φ β s.next) _
        (by
          have := rel_cons a (.alloc β) (rel_upd s.next hr hb hβ)
          simpa [State.bind, emb, upd] using this)
        (brandsIn_new a β hb)
  | @alloc B Γ h a β rest Γ' ha _ _ ih =>
    intro n k φ s hr hb
    cases n with
    | zero => exact .inl rfl
    | succ n =>
      have hea : AList.lookup s.env a = some (.allocV (φ β)) := by rw [hr a, ha]; rfl
      simp only [ghost, hea]
      refine ih n k φ _ (rel_cons h (.heap β) hr) (brandsIn_cons hb (fun γ hγ => ?_))
      rcases hγ with hγ | hγ <;> cases hγ
      exact hb _ (AList.lookup_mem ha) β (.inl rfl)
  | @use B Γ h a β rest Γ' hh ha _ ih =>
    intro n k φ s hr hb
    cases n with
    | zero => exact .inl rfl
    | succ n =>
      have heh : AList.lookup s.env h = some (.heapV (φ β)) := by rw [hr h, hh]; rfl
      have hea : AList.lookup s.env a = some (.allocV (φ β)) := by rw [hr a, ha]; rfl
      simp only [ghost, heh, hea, ↓reduceIte]
      exact ih n k φ s hr hb
  | @free B Γ h a β rest Γ' hh ha _ ih =>
    intro n k φ s hr hb
    cases n with
    | zero => exact .inl rfl
    | succ n =>
      have heh : AList.lookup s.env h = some (.heapV (φ β)) := by rw [hr h, hh]; rfl
      have hea : AList.lookup s.env a = some (.allocV (φ β)) := by rw [hr a, ha]; rfl
      simp only [ghost, heh, hea, ↓reduceIte]
      exact ih n k φ _ (rel_erase h hr) (brandsIn_erase h hb)
  | @release B Γ a β rest Γ' ha _ _ ih =>
    intro n k φ s hr hb
    cases n with
    | zero => exact .inl rfl
    | succ n =>
      have hea : AList.lookup s.env a = some (.allocV (φ β)) := by rw [hr a, ha]; rfl
      simp only [ghost, hea]
      exact ih n k φ _ (rel_erase a hr) (brandsIn_erase a hb)
  | @reset B Γ a' a β β' rest Γ' ha _ _ hβ' _ ih =>
    intro n k φ s hr hb
    cases n with
    | zero => exact .inl rfl
    | succ n =>
      have hea : AList.lookup s.env a = some (.allocV (φ β)) := by rw [hr a, ha]; rfl
      simp only [ghost, hea]
      refine ih n k (upd φ β' s.next) _ ?_ (brandsIn_new a' β' (brandsIn_erase a hb))
      have := rel_cons a' (.alloc β')
        (rel_upd s.next (rel_erase a hr) (brandsIn_erase a hb) hβ')
      simpa [State.bind, emb, upd] using this
  | @while_ B Γ a β0 β βx β'' body rest Γ' ha _ hβ hβx _ _ ihb ihr =>
    intro n₀ k₀ φ₀ s₀ hr₀ hb
    -- The loop invariant: the allocator `a` has some brand `γ`, the rest of the context is
    -- unchanged.
    have loop : ∀ (n k : Nat) (φ : Nat → Nat) (γ : Nat) (s : State),
        Rel φ ((a, .alloc γ) :: AList.erase Γ a) s.env →
        ghost σ n k s (.while_ a β βx body :: rest) = .outOfFuel ∨
          ∃ s' k' φ', ghost σ n k s (.while_ a β βx body :: rest) = .done s' k' ∧
            Rel φ' Γ' s'.env := by
      intro n
      induction n with
      | zero => intro _ _ _ _ _; exact .inl rfl
      | succ n ihn =>
        intro k φ γ s hr
        simp only [ghost]
        split
        · rcases ihb n (k + 1) _ s (rel_reopen hr hb hβ)
              (brandsIn_new a β (brandsIn_erase a hb)) with h | ⟨s₁, k₁, φ₁, h, hr₁⟩
          · rw [h]; exact .inl rfl
          · rw [h]; exact ihn k₁ φ₁ β'' s₁ hr₁
        · exact ihr n (k + 1) _ s (rel_reopen hr hb hβx) (brandsIn_new a βx (brandsIn_erase a hb))
    exact loop n₀ k₀ φ₀ β0 s₀ (rel_split hr₀ ha)

/-- Law (subject `ConcreteSpec.Brand.ghost`). **Checked programs pass every brand check** (Q1.1). The ghost run of a checked program never
gets stuck, for every outcome of its loop tests and any fuel. -/
theorem checked_passes_brand_checks {ss Γ'} (h : Ok [] [] ss Γ') (σ : Nat → Bool) (n : Nat) :
    ghost σ n 0 State.init ss ≠ .stuck := by
  rcases ghost_ok (σ := σ) h n 0 id State.init (fun _ => rfl) (fun _ he => nomatch he) with
    h | ⟨s', _, _, h, _⟩ <;> rw [h] <;> simp

/-- Law (subject `ConcreteSpec.Brand.erased`). The erased run simulates the ghost run: wherever the ghost run does not get stuck, the
erased run does the same thing with the brands removed. -/
theorem erased_sim {σ : Nat → Bool} : ∀ (n k : Nat) (s : State) (ss : List Stmt),
    ghost σ n k s ss ≠ .stuck → erased σ n k (AList.map Val.erase s.env) ss = (ghost σ n k s ss).erase := by
  intro n
  induction n with
  | zero => intro _ _ _ _; rfl
  | succ n ih =>
    intro k s ss hns
    match ss with
    | [] => rfl
    | .newAlloc a β :: rest =>
      simp only [ghost] at hns ⊢
      simp only [erased]
      have := ih k (s.bind a s.env) rest hns
      simpa [State.bind, AList.map, Val.erase] using this
    | .alloc h a :: rest =>
      simp only [ghost] at hns ⊢
      simp only [erased, AList.lookup_map]
      cases hf : AList.lookup s.env a with
      | none => simp [hf] at hns
      | some v =>
        cases v with
        | heapV b => simp [hf] at hns
        | allocV b =>
          simp only [hf] at hns ⊢
          have := ih k { s with env := (h, .heapV b) :: s.env } rest hns
          simpa [AList.map, Val.erase] using this
    | .use h a :: rest =>
      simp only [ghost] at hns ⊢
      simp only [erased, AList.lookup_map]
      cases hf : AList.lookup s.env h with
      | none => simp [hf] at hns
      | some v =>
        cases hg : AList.lookup s.env a with
        | none => cases v <;> simp [hf, hg] at hns
        | some w =>
          cases v <;> cases w <;> simp only [hf, hg] at hns ⊢ <;> try (exact absurd rfl hns)
          rename_i b b'
          by_cases hbb : b = b'
          · simp only [hbb, ↓reduceIte] at hns ⊢
            simpa [Val.erase] using ih k s rest hns
          · simp [hbb] at hns
    | .free h a :: rest =>
      simp only [ghost] at hns ⊢
      simp only [erased, AList.lookup_map]
      cases hf : AList.lookup s.env h with
      | none => simp [hf] at hns
      | some v =>
        cases hg : AList.lookup s.env a with
        | none => cases v <;> simp [hf, hg] at hns
        | some w =>
          cases v <;> cases w <;> simp only [hf, hg] at hns ⊢ <;> try (exact absurd rfl hns)
          rename_i b b'
          by_cases hbb : b = b'
          · simp only [hbb, ↓reduceIte] at hns ⊢
            have := ih k { s with env := AList.erase s.env h } rest hns
            simpa [Val.erase, AList.erase_map] using this
          · simp [hbb] at hns
    | .release a :: rest =>
      simp only [ghost] at hns ⊢
      simp only [erased, AList.lookup_map]
      cases hf : AList.lookup s.env a with
      | none => simp [hf] at hns
      | some v =>
        cases v with
        | heapV b => simp [hf] at hns
        | allocV b =>
          simp only [hf] at hns ⊢
          have := ih k { s with env := AList.erase s.env a } rest hns
          simpa [Val.erase, AList.erase_map] using this
    | .reset a' a β' :: rest =>
      simp only [ghost] at hns ⊢
      simp only [erased, AList.lookup_map]
      cases hf : AList.lookup s.env a with
      | none => simp [hf] at hns
      | some v =>
        cases v with
        | heapV b => simp [hf] at hns
        | allocV b =>
          simp only [hf] at hns ⊢
          have := ih k (s.bind a' (AList.erase s.env a)) rest hns
          rw [AList.erase_map]
          simpa [State.bind, AList.map, Val.erase] using this
    | .while_ a β βx body :: rest =>
      simp only [ghost] at hns ⊢
      simp only [erased]
      by_cases hσ : σ k = true
      · simp only [hσ, ↓reduceIte] at hns ⊢
        cases hb : ghost σ n (k + 1) s body with
        | done s₁ k₁ =>
          rw [hb] at hns
          rw [ih (k + 1) s body (by rw [hb]; simp)]
          simp only [hb, GR.erase]
          exact ih k₁ s₁ _ hns
        | stuck => rw [hb] at hns; exact absurd rfl hns
        | outOfFuel =>
          rw [ih (k + 1) s body (by rw [hb]; simp), hb]
          rfl
      · simp only [hσ, Bool.false_eq_true, ↓reduceIte] at hns ⊢
        exact ih (k + 1) s rest hns

/-- Law (subject `ConcreteSpec.Brand.erased`). **Brand erasure** (Q1.1). A checked program run with its brands erased does exactly what its
ghost run does, brands removed, for every outcome of its loop tests and any fuel; and the ghost
run never fails a brand check (checked_passes_brand_checks). Erasing the brands changes no
behaviour. -/
theorem brand_erasure {ss Γ'} (h : Ok [] [] ss Γ') (σ : Nat → Bool) (n : Nat) :
    erased σ n 0 [] ss = (ghost σ n 0 State.init ss).erase :=
  erased_sim n 0 State.init ss (checked_passes_brand_checks h σ n)

/-- Law (subject `ConcreteSpec.Brand.Run`). A run of l₁ and then l₂ is a run of l₁ ++ l₂. -/
theorem run_append {s l₁ s₁} (h : Run s l₁ s₁) : ∀ {l₂ s₂}, Run s₁ l₂ s₂ → Run s (l₁ ++ l₂) s₂ := by
  induction h with
  | nil => intro _ _ h; exact h
  | newAlloc _ ih => intro _ _ h; exact .newAlloc (ih h)
  | alloc ha _ ih => intro _ _ h; exact .alloc ha (ih h)
  | use hh ha _ ih => intro _ _ h; exact .use hh ha (ih h)
  | free hh ha _ ih => intro _ _ h; exact .free hh ha (ih h)
  | release ha _ ih => intro _ _ h; exact .release ha (ih h)
  | reset ha _ ih => intro _ _ h; exact .reset ha (ih h)
  | whileExit _ ih => intro _ _ h; exact .whileExit (ih h)
  | whileIter _ ih => intro _ _ h; exact .whileIter (by simpa using ih h)

/-- Law (subject `ConcreteSpec.Brand.Run`). The executable ghost run agrees with Run: a finished ghost run is a Run. -/
theorem ghost_run {σ : Nat → Bool} : ∀ {n k : Nat} {s : State} {ss : List Stmt} {s' : State}
    {k' : Nat}, ghost σ n k s ss = .done s' k' → Run s ss s' := by
  intro n
  induction n with
  | zero => intro _ _ _ _ _ h; simp [ghost] at h
  | succ n ih =>
    intro k s ss s' k' h
    match ss with
    | [] => simp only [ghost, GR.done.injEq] at h; obtain ⟨rfl, -⟩ := h; exact .nil
    | .newAlloc a β :: rest => simp only [ghost] at h; exact .newAlloc (ih h)
    | .alloc h' a :: rest =>
      simp only [ghost] at h
      split at h
      · rename_i b hb; exact .alloc hb (ih h)
      · cases h
    | .use h' a :: rest =>
      simp only [ghost] at h
      split at h
      · rename_i b b' hh ha
        split at h
        · subst_vars; exact .use hh ha (ih h)
        · cases h
      · cases h
    | .free h' a :: rest =>
      simp only [ghost] at h
      split at h
      · rename_i b b' hh ha
        split at h
        · subst_vars; exact .free hh ha (ih h)
        · cases h
      · cases h
    | .release a :: rest =>
      simp only [ghost] at h
      split at h
      · rename_i b hb; exact .release hb (ih h)
      · cases h
    | .reset a' a β' :: rest =>
      simp only [ghost] at h
      split at h
      · rename_i b hb; exact .reset hb (ih h)
      · cases h
    | .while_ a β βx body :: rest =>
      simp only [ghost] at h
      split at h
      · split at h
        · rename_i s₁ k₁ hb
          exact .whileIter (run_append (ih hb) (ih h))
        · rename_i hr; exact (hr s' k' h).elim
      · exact .whileExit (ih h)

namespace Evidence

/-- Reusing the counter leaves the newly allocated brand outside its strict upper bound. -/
def reuseCounter (s : State) (a : Nat) (env : AList Nat Val) : State :=
  { env := (a, .allocV s.next) :: env, next := s.next, gen := s.gen ++ [s.next] }

def stuckGhost : (Nat → Bool) → Nat → Nat → State → List Stmt → GR := fun _ _ _ _ _ => .stuck

def stuckErased : (Nat → Bool) → Nat → Nat → AList Nat EVal → List Stmt → ER := fun _ _ _ _ _ => .stuck

/-- Instance (law `ConcreteSpec.Brand.inv_bind`, case `firstAllocation`). -/
example := @inv_bind State.init 0 [] inv_init (by intro e he; cases he)

/-- Planted (law `ConcreteSpec.Brand.inv_bind`, case `firstAllocation`, subject `ConcreteSpec.Brand.State.bind`, mutant `ConcreteSpec.Brand.Evidence.reuseCounter`). -/
example : Inv State.init ∧ (∀ e ∈ ([] : AList Nat Val), brandOf e.2 < State.init.next) ∧
    ¬ Inv (reuseCounter State.init 0 []) := by
  refine ⟨inv_init, (by intro e he; cases he), ?_⟩
  intro hi
  have := hi.1 (0, .allocV 0) (by simp [reuseCounter, State.init])
  simp [reuseCounter, State.init, brandOf] at this

/-- Instance (law `ConcreteSpec.Brand.ghost_ok`, case `firstAllocation`). -/
example := @ghost_ok (fun _ => false) [] [] [.newAlloc 0 0] [(0, .alloc 0)]
  (.newAlloc rfl (by simp) .nil) 2 0 id State.init (fun _ => rfl) (by intro e he; cases he)

/-- Planted (law `ConcreteSpec.Brand.ghost_ok`, case `firstAllocation`, subject `ConcreteSpec.Brand.ghost`, mutant `ConcreteSpec.Brand.Evidence.stuckGhost`). -/
example : Ok [] [] [.newAlloc 0 0] [(0, .alloc 0)] ∧ Rel id [] State.init.env ∧ BrandsIn [] [] ∧
    ¬ (stuckGhost (fun _ => false) 2 0 State.init [.newAlloc 0 0] = .outOfFuel ∨
      ∃ s' k' φ', stuckGhost (fun _ => false) 2 0 State.init [.newAlloc 0 0] = .done s' k' ∧
        Rel φ' [(0, .alloc 0)] s'.env) :=
  ⟨.newAlloc rfl (by simp) .nil, (fun _ => rfl), (by intro e he; cases he), by simp [stuckGhost]⟩

/-- Instance (law `ConcreteSpec.Brand.checked_passes_brand_checks`, case `firstAllocation`). -/
example := @checked_passes_brand_checks [.newAlloc 0 0] [(0, .alloc 0)]
  (.newAlloc rfl (by simp) .nil) (fun _ => false) 2

/-- Planted (law `ConcreteSpec.Brand.checked_passes_brand_checks`, case `firstAllocation`, subject `ConcreteSpec.Brand.ghost`, mutant `ConcreteSpec.Brand.Evidence.stuckGhost`). -/
example : Ok [] [] [.newAlloc 0 0] [(0, .alloc 0)] ∧
    ¬ (stuckGhost (fun _ => false) 2 0 State.init [.newAlloc 0 0] ≠ .stuck) :=
  ⟨.newAlloc rfl (by simp) .nil, by simp [stuckGhost]⟩

/-- Instance (law `ConcreteSpec.Brand.erased_sim`, case `firstAllocation`). -/
example := @erased_sim (fun _ => false) 2 0 State.init [.newAlloc 0 0] (by decide)

/-- Planted (law `ConcreteSpec.Brand.erased_sim`, case `firstAllocation`, subject `ConcreteSpec.Brand.erased`, mutant `ConcreteSpec.Brand.Evidence.stuckErased`). -/
example : ghost (fun _ => false) 2 0 State.init [.newAlloc 0 0] ≠ .stuck ∧
    ¬ (stuckErased (fun _ => false) 2 0 (AList.map Val.erase State.init.env) [.newAlloc 0 0] =
      (ghost (fun _ => false) 2 0 State.init [.newAlloc 0 0]).erase) := by
  constructor <;> decide

/-- Instance (law `ConcreteSpec.Brand.brand_erasure`, case `firstAllocation`). -/
example := @brand_erasure [.newAlloc 0 0] [(0, .alloc 0)]
  (.newAlloc rfl (by simp) .nil) (fun _ => false) 2

/-- Planted (law `ConcreteSpec.Brand.brand_erasure`, case `firstAllocation`, subject `ConcreteSpec.Brand.erased`, mutant `ConcreteSpec.Brand.Evidence.stuckErased`). -/
example : Ok [] [] [.newAlloc 0 0] [(0, .alloc 0)] ∧
    ¬ (stuckErased (fun _ => false) 2 0 [] [.newAlloc 0 0] =
      (ghost (fun _ => false) 2 0 State.init [.newAlloc 0 0]).erase) :=
  ⟨.newAlloc rfl (by simp) .nil, by decide⟩

/-- Instance (law `ConcreteSpec.Brand.brand_mismatch_refused`, case `distinctBrands`). -/
example := @brand_mismatch_refused [10, 11] [(1, .heap 10), (0, .alloc 11)] 1 0 10 11 []
  [(1, .heap 10), (0, .alloc 11)] rfl rfl (by decide)




def beforeReset : State := ⟨[(0, .allocV 0), (1, .heapV 0)], 1, [0]⟩
def afterReset : State := beforeReset.bind 0 (AList.erase beforeReset.env 0)

/-- Helper (computation). Closed state with two live old-brand values and a fresh counter. (approved: REV-base23b-agy-8601f966a). -/
theorem beforeResetInv : Inv beforeReset := by
  refine ⟨?_, ?_, ?_⟩
  · intro e he; cases he with
    | head => decide
    | tail _ he => cases he with
      | head => decide
      | tail _ he => cases he
  · decide
  · decide

/-- Instance (law `ConcreteSpec.Brand.reset_fresh_brand`, case `liveOldBrand`). -/
example := @reset_fresh_brand beforeReset 0 0 1 [] afterReset beforeResetInv (.reset rfl .nil)


/-- Instance (law `ConcreteSpec.Brand.reset_fresh_refuses`, case `liveOldHandle`). -/
example := @reset_fresh_refuses beforeReset 0 0 1 1 [] afterReset beforeResetInv (by decide)

/-- Instance (law `ConcreteSpec.Brand.run_append`, case `twoAllocations`). -/
example := @run_append State.init [.newAlloc 0 0] (State.init.bind 0 []) (.newAlloc .nil)
  [.newAlloc 1 1] ((State.init.bind 0 []).bind 1 (State.init.bind 0 []).env) (.newAlloc .nil)

/-- Instance (law `ConcreteSpec.Brand.ghost_run`, case `firstAllocation`). -/
example := @ghost_run (fun _ => false) 2 0 State.init [.newAlloc 0 0] (State.init.bind 0 []) 0 rfl

/-- A broken check that accepts arbitrary brand combinations. -/
inductive PermissiveOk : List Nat → Ctx → List Stmt → Ctx → Prop where
  | accept {B Γ ss Γ'} : PermissiveOk B Γ ss Γ'

/-- Planted (law `ConcreteSpec.Brand.brand_mismatch_refused`, case `distinctBrands`, subject `ConcreteSpec.Brand.Ok`, mutant `ConcreteSpec.Brand.Evidence.PermissiveOk`). -/
example : AList.lookup [(1, STy.heap 10), (0, .alloc 11)] 1 = some (.heap 10) ∧
    AList.lookup [(1, STy.heap 10), (0, .alloc 11)] 0 = some (.alloc 11) ∧
    (10 : Nat) ≠ 11 ∧
    ¬ (¬ PermissiveOk [10, 11] [(1, .heap 10), (0, .alloc 11)] [.use 1 0]
          [(1, .heap 10), (0, .alloc 11)] ∧
       ¬ PermissiveOk [10, 11] [(1, .heap 10), (0, .alloc 11)] [.free 1 0]
          [(1, .heap 10), (0, .alloc 11)]) := by
  exact ⟨rfl, rfl, by decide, fun h => h.1 .accept⟩

/-- A broken ghost relation that can reset, but has lost the continuation rules. -/
inductive ResetWithoutContinuation : State → List Stmt → State → Prop where
  | reset : ResetWithoutContinuation beforeReset [.reset 0 0 1] afterReset

/-- Planted (law `ConcreteSpec.Brand.reset_fresh_brand`, case `liveOldBrand`, subject `ConcreteSpec.Brand.Run`, mutant `ConcreteSpec.Brand.Evidence.ResetWithoutContinuation`). -/
example : Inv beforeReset ∧ ResetWithoutContinuation beforeReset [.reset 0 0 1] afterReset ∧
    ¬ (∃ mid, mid.env = (0, .allocV beforeReset.next) :: AList.erase beforeReset.env 0 ∧
      ResetWithoutContinuation mid [] afterReset ∧
      ∀ e ∈ beforeReset.env, brandOf e.2 ≠ beforeReset.next) := by
  refine ⟨beforeResetInv, .reset, ?_⟩
  rintro ⟨_, _, h, _⟩; cases h

/-- A broken ghost relation that permits old handles after reset. -/
inductive PermissiveRun : State → List Stmt → State → Prop where
  | accept {s ss s'} : PermissiveRun s ss s'

/-- Planted (law `ConcreteSpec.Brand.reset_fresh_refuses`, case `liveOldHandle`, subject `ConcreteSpec.Brand.Run`, mutant `ConcreteSpec.Brand.Evidence.PermissiveRun`). -/
example : Inv beforeReset ∧ (1 : Nat) ≠ 0 ∧
    ¬ (¬ PermissiveRun beforeReset [.reset 0 0 1, .use 1 0] afterReset ∧
       ¬ PermissiveRun beforeReset [.reset 0 0 1, .free 1 0] afterReset) := by
  exact ⟨beforeResetInv, by decide, fun h => h.1 .accept⟩

/-- A broken ghost relation supporting individual allocations but not their composition. -/
inductive SingleAllocation : State → List Stmt → State → Prop where
  | alloc {s a β} : SingleAllocation s [.newAlloc a β] (s.bind a s.env)

/-- Planted (law `ConcreteSpec.Brand.run_append`, case `twoAllocations`, subject `ConcreteSpec.Brand.Run`, mutant `ConcreteSpec.Brand.Evidence.SingleAllocation`). -/
example : SingleAllocation State.init [.newAlloc 0 0] (State.init.bind 0 []) ∧
    SingleAllocation (State.init.bind 0 []) [.newAlloc 1 1]
      ((State.init.bind 0 []).bind 1 (State.init.bind 0 []).env) ∧
    ¬ SingleAllocation State.init ([.newAlloc 0 0] ++ [.newAlloc 1 1])
      ((State.init.bind 0 []).bind 1 (State.init.bind 0 []).env) := by
  exact ⟨.alloc, .alloc, fun h => nomatch h⟩

/-- A broken ghost relation with no operational rules. -/
inductive MissingRun : State → List Stmt → State → Prop where

/-- Planted (law `ConcreteSpec.Brand.ghost_run`, case `firstAllocation`, subject `ConcreteSpec.Brand.Run`, mutant `ConcreteSpec.Brand.Evidence.MissingRun`). -/
example : ghost (fun _ => false) 2 0 State.init [.newAlloc 0 0] = .done (State.init.bind 0 []) 0 ∧
    ¬ MissingRun State.init [.newAlloc 0 0] (State.init.bind 0 []) := by
  exact ⟨rfl, fun h => nomatch h⟩





/-! Closed uses of the preservation helpers. -/
example := @run_inv State.init [.newAlloc 0 0] (State.init.bind 0 []) (.newAlloc .nil) inv_init
example := @reset_fresh_nodup [.newAlloc 0 0] (State.init.bind 0 []) (.newAlloc .nil)

/-- Helper (computation). The singleton live handle has a brand in the closed allowed set. (approved: PENDING). -/
theorem liveBrandIn : BrandsIn [0, 2] [(1, .heap 2)] := by
  intro e he β ht
  simp only [List.mem_singleton] at he
  subst e
  rcases ht with ht | ht
  · cases ht
  · cases ht; simp

/-- Instance (law `ConcreteSpec.Brand.reset_requires_no_old`, case `liveOldHandle`). -/
example := @reset_requires_no_old [0] [(1, .heap 0), (0, .alloc 0)] 0 0 1 [] [] 0 1 rfl rfl

/-- Planted (law `ConcreteSpec.Brand.reset_requires_no_old`, case `liveOldHandle`, subject `ConcreteSpec.Brand.Ok`, mutant `ConcreteSpec.Brand.Evidence.PermissiveOk`). -/
example : AList.lookup [(1, STy.heap 0), (0, .alloc 0)] 0 = some (.alloc 0) ∧
    AList.lookup [(1, STy.heap 0), (0, .alloc 0)] 1 = some (.heap 0) ∧
    ¬ (¬ PermissiveOk [0] [(1, .heap 0), (0, .alloc 0)] [.reset 0 0 1] []) :=
  ⟨rfl, rfl, fun h => h .accept⟩

/-- Instance (law `ConcreteSpec.Brand.reset_refuses_old`, case `reusedLiveBrand`). -/
example := @reset_refuses_old [0, 2] [(1, .heap 2)] 0 0 2 [] [] 1 2 liveBrandIn rfl (Or.inl rfl)

/-- Planted (law `ConcreteSpec.Brand.reset_refuses_old`, case `reusedLiveBrand`, subject `ConcreteSpec.Brand.Ok`, mutant `ConcreteSpec.Brand.Evidence.PermissiveOk`). -/
example : BrandsIn [0, 2] [(1, .heap 2)] ∧ AList.lookup [(1, STy.heap 2)] 1 = some (.heap 2) ∧
    ((2 : Nat) = 2 ∨ ∃ r, ([] : List Stmt) = .use 1 0 :: r ∨ ([] : List Stmt) = .free 1 0 :: r) ∧
    ¬ (¬ PermissiveOk [0, 2] [(1, .heap 2)] [.reset 0 0 2] []) :=
  ⟨liveBrandIn, rfl, Or.inl rfl, fun h => h .accept⟩

end Evidence

end ConcreteSpec.Brand
