import ConcreteSpec.Proofs.Basics

/-!
# Part 4: `open_mut_freezes_root` and `shared_open_grants_no_write`.

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). See `ConcreteSpec/README.md`.
-/

namespace ConcreteSpec.Borrow

theorem find_mapPlaces (f : Place → Place) : ∀ (Γ : Ctx) (p : Nat),
    find (mapPlaces f Γ) p = (find Γ p).map f
  | [], _ => rfl
  | (n, q) :: Γ, p => by
    show find ((n, f q) :: mapPlaces f Γ) p = _
    simp only [find]
    split
    · rfl
    · exact find_mapPlaces f Γ p

theorem find_remove (p' : Nat) : ∀ (Γ : Ctx) (p : Nat),
    find (remove p' Γ) p = if p = p' then none else find Γ p
  | [], _ => by simp [remove, find]
  | (n, q) :: Γ, p => by
    have ih := find_remove p' Γ p
    by_cases hn : n = p' <;> by_cases hp : p = p' <;> by_cases hnp : n = p <;>
      simp_all [remove, find] <;> omega

theorem find_bodyCtx {m v src Γ p} (hv : p ≠ v) :
    find (bodyCtx m v src Γ) p = (find Γ p).map (lockFor m src.root) := by
  simp only [bodyCtx, find, show ¬ v = p from fun h => hv h.symm, ↓reduceIte]
  exact find_mapPlaces _ Γ p

theorem lockFor_excl {m r q} (h : q.lock = .excl) : (lockFor m r q).lock = .excl := by
  unfold lockFor; split <;> (try split) <;> (try split) <;> simp_all

theorem ne_of_find {Γ p v q} (hp : find Γ p = some q) (hv : find Γ v = none) : p ≠ v := by
  rintro rfl; rw [hp] at hv; cases hv

theorem find_restore (Γb : Ctx) : ∀ (Γ : Ctx) (p : Nat),
    find (restore Γ Γb) p = if (find Γb p).isSome then find Γ p else none
  | [], _ => by simp [restore, find]
  | (n, q) :: Γ, p => by
    have ih := find_restore Γb Γ p
    simp only [restore] at ih ⊢
    by_cases hn : (find Γb n).isSome = true <;> by_cases hnp : n = p
    · subst hnp; simp [List.filter, hn, find]
    · simp [List.filter, hn, find, hnp, ih]
    · subst hnp; simp [List.filter, hn, ih]
    · simp [List.filter, hn, find, hnp, ih]

theorem find_restore_some {Γ Γb p q} (h : find (restore Γ Γb) p = some q) : find Γ p = some q := by
  rw [find_restore] at h
  split at h
  · exact h
  · cases h

theorem lockFor_nonfree {m r q} (h : q.lock ≠ .free) : (lockFor m r q).lock ≠ .free := by
  unfold lockFor; split <;> (try split) <;> (try split) <;> simp_all

/-- The check is deterministic: the places after a list are a function of the places before. -/
theorem ok_det {Γ ss Γ₁} (h : Ok Γ ss Γ₁) : ∀ {Γ₂}, Ok Γ ss Γ₂ → Γ₁ = Γ₂ := by
  induction h with
  | nil => intro Γ₂ h2; cases h2; rfl
  | read _ _ _ ih => intro Γ₂ h2; cases h2 with | read _ _ h2 => exact ih h2
  | write _ _ _ _ ih => intro Γ₂ h2; cases h2 with | write _ _ _ h2 => exact ih h2
  | move _ _ _ _ ih => intro Γ₂ h2; cases h2 with | move _ _ _ h2 => exact ih h2
  | borrow hs _ _ _ _ _ ihb ih =>
    intro Γ₂ h2
    cases h2 with
    | borrow hs2 _ _ _ hb2 hr2 =>
      rw [hs] at hs2; cases hs2
      cases ihb hb2
      exact ih hr2

/-- A place a statement list cannot touch (locked) keeps its entry. -/
theorem ok_keeps_locked {Γ ss Γ'} (h : Ok Γ ss Γ') :
    ∀ {p q}, find Γ p = some q → q.lock ≠ .free → find Γ' p = some q := by
  induction h with
  | nil => intro p q hp _; exact hp
  | read _ _ _ ih => exact ih
  | write _ _ _ _ ih => exact ih
  | @move Γ p0 q0 rest Γ' hf hl _ _ ih =>
    intro p q hp hq
    have hne : p ≠ p0 := by rintro rfl; rw [hf] at hp; cases hp; exact hq hl
    exact ih (by rw [find_remove]; simp only [hne, ↓reduceIte]; exact hp) hq
  | @borrow Γ m s v q0 body rest Γb Γ' _ _ _ hv _ _ ihb ih =>
    intro p q hp hq
    have hb : find Γb p = some (lockFor m q0.root q) :=
      ihb (by rw [find_bodyCtx (ne_of_find hp hv), hp]; rfl) (lockFor_nonfree hq)
    exact ih (by rw [find_restore, hb]; exact hp) hq

/-- A frozen place is never named, at any depth. -/
theorem excl_untouched {Γ ss Γ'} (h : Ok Γ ss Γ') :
    ∀ {p q}, find Γ p = some q → q.lock = .excl → ¬ Touches p ss := by
  induction h with
  | nil => intro p q _ _ ht; cases ht
  | read hf hl _ ih =>
    intro p q hp hq ht
    cases ht with
    | read => rw [hf] at hp; cases hp; exact hl hq
    | tail ht => exact ih hp hq ht
  | write hf hl _ _ ih =>
    intro p q hp hq ht
    cases ht with
    | write => rw [hf] at hp; cases hp; rw [hl] at hq; cases hq
    | tail ht => exact ih hp hq ht
  | @move Γ p0 q0 rest Γ' hf hl _ _ ih =>
    intro p q hp hq ht
    have hne : p ≠ p0 := by
      rintro rfl; rw [hf] at hp; cases hp; rw [hl] at hq; cases hq
    cases ht with
    | move => exact hne rfl
    | tail ht =>
      refine ih (q := q) ?_ hq ht
      rw [find_remove]; simp only [hne, ↓reduceIte]; exact hp
  | @borrow Γ m s v q0 body rest Γb Γ' hs hnex _ hv hbody _ ihb ih =>
    intro p q hp hq ht
    have hpb : find (bodyCtx m v q0 Γ) p = some (lockFor m q0.root q) := by
      rw [find_bodyCtx (ne_of_find hp hv), hp]; rfl
    cases ht with
    | src => rw [hs] at hp; cases hp; exact hnex hq
    | body ht => exact ihb hpb (lockFor_excl hq) ht
    | tail ht =>
      have hb := ok_keeps_locked hbody hpb (lockFor_nonfree (by rw [hq]; decide))
      exact ih (by rw [find_restore, hb]; exact hp) hq ht

/-- **`open_mut_freezes_root`** (Q9.14, Q3.3, Q3.4, REF-11). Inside an accepted
`borrow mut src as v { body }`, no place of `src`'s root that existed before the open (the root
itself and every view derived from it) is read, written, moved or opened again, at any depth.
Only the new view `v` reaches the root. -/
theorem open_mut_freezes_root {Γ s v body rest q Γ'} (h : Ok Γ (.borrow true s v body :: rest) Γ')
    (hs : find Γ s = some q) :
    ∀ p q', find Γ p = some q' → q'.root = q.root → ¬ Touches p body := by
  intro p q' hp hr
  cases h with
  | borrow hs' _ _ hv hb _ =>
    rw [hs] at hs'; cases hs'
    refine excl_untouched hb (q := lockFor true q.root q') ?_ (by simp [lockFor, hr]) 
    rw [find_bodyCtx (ne_of_find hp hv), hp]; rfl

/-- The places of root `r` grant no write: each is locked, or neither writable nor an owner. -/
def NoWrite (r : Nat) (Γ : Ctx) : Prop :=
  ∀ p q, find Γ p = some q → q.root = r → q.lock = .free → q.mutable = false ∧ q.owner = false

theorem noWrite_restore {r Γ Γb} (h : NoWrite r Γ) : NoWrite r (restore Γ Γb) :=
  fun p q hp => h p q (find_restore_some hp)

theorem noWrite_remove {r Γ p'} (h : NoWrite r Γ) : NoWrite r (remove p' Γ) := by
  intro p q hp
  rw [find_remove] at hp
  split at hp
  · cases hp
  · exact h p q hp

theorem noWrite_bodyCtx {r Γ m v q0} (h : NoWrite r Γ)
    (hm : m = true → q0.lock = .free ∧ q0.mutable = true)
    (hsrc : q0.root = r → m = true → False) : NoWrite r (bodyCtx m v q0 Γ) := by
  intro p q hp hr hl
  by_cases hpv : p = v
  · subst hpv
    simp only [bodyCtx, find, ↓reduceIte] at hp
    cases hp
    simp only [view] at hr ⊢
    cases m with
    | false => simp
    | true => exact (hsrc hr rfl).elim
  · rw [find_bodyCtx hpv] at hp
    cases hq : find Γ p with
    | none => rw [hq] at hp; cases hp
    | some q1 =>
      rw [hq] at hp; cases hp
      have _ := hm
      by_cases h1 : q1.root = q0.root
      · by_cases h2 : q1.lock = .free <;> cases m <;> simp [lockFor, h1, h2] at hl
      · simp only [lockFor, h1, ↓reduceIte] at hr hl ⊢
        exact h p q1 hq hr hl

/-- An accepted list never writes a root whose places grant no write. -/
theorem noWrite_sound {Γ ss Γ'} (h : Ok Γ ss Γ') : ∀ {r}, NoWrite r Γ → ¬ WritesRoot r Γ ss := by
  induction h with
  | nil => intro r _ hw; cases hw
  | read _ _ _ ih =>
    intro r hn hw
    cases hw with
    | tail hs hw => cases hs with | read _ _ h0 => cases h0; exact ih hn hw
  | write hf hl hmut _ ih =>
    intro r hn hw
    cases hw with
    | write hf' hr => rw [hf] at hf'; cases hf'; rw [(hn _ _ hf hr hl).1] at hmut; cases hmut
    | tail hs hw => cases hs with | write _ _ _ h0 => cases h0; exact ih hn hw
  | move hf hl hown _ ih =>
    intro r hn hw
    cases hw with
    | move hf' hr => rw [hf] at hf'; cases hf'; rw [(hn _ _ hf hr hl).2] at hown; cases hown
    | tail hs hw => cases hs with | move _ _ _ h0 => cases h0; exact ih (noWrite_remove hn) hw
  | @borrow Γ m s v q0 body rest Γb Γ' hs _ hm _ hb _ ihb ih =>
    intro r hn hw
    have hbad : q0.root = r → m = true → False := fun hr hmt => by
      obtain ⟨hl, hmut⟩ := hm hmt
      rw [(hn _ _ hs hr hl).1] at hmut; cases hmut
    cases hw with
    | mutOpen hs' hr => rw [hs] at hs'; cases hs'; exact hbad hr rfl
    | body hs' hw => rw [hs] at hs'; cases hs'; exact ihb (noWrite_bodyCtx hn hm hbad) hw
    | tail hs1 hw =>
      cases hs1 with
      | borrow hs2 _ _ _ hb2 h0 =>
        rw [hs] at hs2; cases hs2
        cases ok_det hb hb2
        cases h0
        exact ih (noWrite_restore hn) hw

/-- **`open_mut_freezes_root`, shared half** (Q9.14, Q3.3). A shared open never grants a write:
inside an accepted `borrow src as v { body }`, nothing at any depth writes, moves or mutably
opens a place of `src`'s root, whether through the root, through `v`, or through any view
derived from either. -/
theorem shared_open_grants_no_write {Γ s v body rest q Γ'}
    (h : Ok Γ (.borrow false s v body :: rest) Γ') (hs : find Γ s = some q) :
    ¬ WritesRoot q.root (bodyCtx false v q Γ) body := by
  cases h with
  | borrow hs' _ hm _ hb _ =>
    rw [hs] at hs'; cases hs'
    refine noWrite_sound hb ?_
    intro p q' hp hr hl
    by_cases hpv : p = v
    · subst hpv; simp only [bodyCtx, find, ↓reduceIte] at hp; cases hp; exact ⟨rfl, rfl⟩
    · rw [find_bodyCtx hpv] at hp
      cases hq : find Γ p with
      | none => rw [hq] at hp; cases hp
      | some q1 =>
        rw [hq] at hp; cases hp
        by_cases h1 : q1.root = q.root
        · by_cases h2 : q1.lock = .free <;> simp [lockFor, h1, h2] at hl
        · simp [lockFor, h1] at hr

end ConcreteSpec.Borrow
