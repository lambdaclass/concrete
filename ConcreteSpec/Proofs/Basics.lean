import ConcreteSpec.Spec.Basics

/-!
# Lemmas about association lists (`Spec/Basics.lean`).

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). See `ConcreteSpec/README.md`.
-/

namespace ConcreteSpec.Core.AList

variable {κ α β : Type} [DecidableEq κ]

theorem lookup_cons (k' : κ) (a : α) (l : AList κ α) (k : κ) :
    lookup ((k', a) :: l) k = if k' = k then some a else lookup l k := rfl

theorem update_cons (k' : κ) (a : α) (l : AList κ α) (k : κ) (f : α → α) :
    update ((k', a) :: l) k f = if k' = k then (k', f a) :: l else (k', a) :: update l k f := rfl

theorem erase_cons (k' : κ) (a : α) (l : AList κ α) (k : κ) :
    erase ((k', a) :: l) k = if k' = k then erase l k else (k', a) :: erase l k := rfl

theorem lookup_mem : ∀ {l : AList κ α} {k : κ} {a : α}, lookup l k = some a → (k, a) ∈ l
  | [], _, _, h => nomatch h
  | (k', b) :: l, k, a, h => by
    rw [lookup_cons] at h
    by_cases hk : k' = k
    · rw [ite_eq_left hk] at h; cases h; subst hk; exact List.Mem.head _
    · rw [ite_eq_right hk] at h; exact List.Mem.tail _ (lookup_mem h)

theorem lookup_isSome_of_mem : ∀ {l : AList κ α} {k : κ} {a : α}, (k, a) ∈ l →
    ∃ b, lookup l k = some b
  | [], _, _, h => nomatch h
  | (k', b) :: l, k, a, h => by
    rw [lookup_cons]
    by_cases hk : k' = k
    · rw [ite_eq_left hk]; exact ⟨b, rfl⟩
    · rw [ite_eq_right hk]
      cases h with
      | head => exact absurd rfl hk
      | tail _ h => exact lookup_isSome_of_mem h

theorem lookup_map (f : α → β) : ∀ (l : AList κ α) (k : κ), lookup (map f l) k = (lookup l k).map f
  | [], _ => rfl
  | (k', a) :: l, k => by
    show lookup ((k', f a) :: map f l) k = _
    rw [lookup_cons, lookup_cons]
    by_cases hk : k' = k
    · rw [ite_eq_left hk, ite_eq_left hk]; rfl
    · rw [ite_eq_right hk, ite_eq_right hk]; exact lookup_map f l k

theorem lookup_update (k : κ) (f : α → α) : ∀ (l : AList κ α) (q : κ),
    lookup (update l k f) q = if q = k then (lookup l k).map f else lookup l q
  | [], q => by
    show none = _
    by_cases hq : q = k
    · rw [ite_eq_left hq]; rfl
    · rw [ite_eq_right hq]; rfl
  | (k', a) :: l, q => by
    rw [update_cons, lookup_cons k' a l k, lookup_cons k' a l q]
    by_cases hk : k' = k
    · rw [ite_eq_left hk, ite_eq_left hk, lookup_cons]
      by_cases hq : q = k
      · rw [ite_eq_left hq, ite_eq_left (hk.trans hq.symm)]; rfl
      · have hkq : ¬ k' = q := fun h => hq (h ▸ hk)
        rw [ite_eq_right hq, ite_eq_right hkq, ite_eq_right hkq]
    · rw [ite_eq_right hk, ite_eq_right hk, lookup_cons, lookup_update k f l q]
      by_cases hq : q = k
      · have hkq : ¬ k' = q := fun h => hk (h ▸ hq)
        rw [ite_eq_left hq, ite_eq_left hq, ite_eq_right hkq]
      · rw [ite_eq_right hq, ite_eq_right hq]

theorem lookup_erase (k : κ) : ∀ (l : AList κ α) (q : κ),
    lookup (erase l k) q = if q = k then none else lookup l q
  | [], q => by
    show none = _
    by_cases hq : q = k
    · rw [ite_eq_left hq]
    · rw [ite_eq_right hq]; rfl
  | (k', a) :: l, q => by
    rw [erase_cons, lookup_cons k' a l q]
    by_cases hk : k' = k
    · rw [ite_eq_left hk, lookup_erase k l q]
      by_cases hq : q = k
      · rw [ite_eq_left hq, ite_eq_left hq]
      · have hkq : ¬ k' = q := fun h => hq (h ▸ hk)
        rw [ite_eq_right hq, ite_eq_right hq, ite_eq_right hkq]
    · rw [ite_eq_right hk, lookup_cons, lookup_erase k l q]
      by_cases hq : q = k
      · have hkq : ¬ k' = q := fun h => hk (h ▸ hq)
        rw [ite_eq_left hq, ite_eq_left hq, ite_eq_right hkq]
      · rw [ite_eq_right hq, ite_eq_right hq]

theorem mem_of_mem_erase (k : κ) : ∀ {l : AList κ α} {e : κ × α}, e ∈ erase l k → e ∈ l
  | [], _, h => h
  | (k', a) :: l, e, h => by
    rw [erase_cons] at h
    by_cases hk : k' = k
    · rw [ite_eq_left hk] at h; exact List.Mem.tail _ (mem_of_mem_erase k h)
    · rw [ite_eq_right hk] at h
      cases h with
      | head => exact List.Mem.head _
      | tail _ h => exact List.Mem.tail _ (mem_of_mem_erase k h)

theorem erase_map (f : α → β) (k : κ) : ∀ l : AList κ α, erase (map f l) k = map f (erase l k)
  | [] => rfl
  | (k', a) :: l => by
    show erase ((k', f a) :: map f l) k = map f (erase ((k', a) :: l) k)
    rw [erase_cons, erase_cons]
    by_cases hk : k' = k
    · rw [ite_eq_left hk, ite_eq_left hk]; exact erase_map f k l
    · rw [ite_eq_right hk, ite_eq_right hk, erase_map f k l]; rfl

/-- `update` with a function that keeps what `g` sees changes nothing `map g` sees. -/
theorem map_update (g : α → β) (f : α → α) (hg : ∀ a, g (f a) = g a) :
    ∀ (l : AList κ α) (k : κ), map g (update l k f) = map g l
  | [], _ => rfl
  | (k', a) :: l, k => by
    rw [update_cons]
    by_cases hk : k' = k
    · rw [ite_eq_left hk]; show (k', g (f a)) :: map g l = (k', g a) :: map g l; rw [hg]
    · rw [ite_eq_right hk]; show (k', g a) :: map g (update l k f) = (k', g a) :: map g l
      rw [map_update g f hg l k]

/-- In a list with distinct registers, every listed binding is returned by the shared lookup. For example, a single register/id pair is found. -/
theorem lookup_of_mem {l : AList κ α} (hn : (List.map Prod.fst l).Nodup) {r : κ} {n : α}
    (hm : (r, n) ∈ l) : AList.lookup l r = some n := by
  induction l with
  | nil => simp at hm
  | cons a l ih =>
    obtain ⟨r', n'⟩ := a
    simp only [List.map_cons, List.nodup_cons, List.mem_map] at hn
    simp only [List.mem_cons, Prod.mk.injEq] at hm
    rcases hm with ⟨rfl, rfl⟩ | hm
    · simp [AList.lookup]
    · have hne : r ≠ r' := by
        rintro rfl; exact hn.1 ⟨(r, n), hm, rfl⟩
      have hb : r' ≠ r := Ne.symm hne
      simp only [AList.lookup, ite_eq_right hb]
      exact ih hn.2 hm


/-- Search the first environment before the second. For example, a first binding of register 7 hides any later binding of 7. -/
theorem lookup_append {l₁ l₂ : AList κ α} {r : κ} :
    AList.lookup (l₁ ++ l₂) r = match AList.lookup l₁ r with | some v => some v | none => AList.lookup l₂ r := by
  induction l₁ with
  | nil => rfl
  | cons x l ih =>
    obtain ⟨r', v'⟩ := x
    simp only [List.cons_append, AList.lookup]
    split <;> simp_all

/-- A lookup in zipped parameters and values has a matching position. For example, the first parameter is paired with the first value. -/
theorem lookup_zip {r : κ} : ∀ {ps : List κ} {vs : List α}, ps.length = vs.length →
    (∀ v, AList.lookup (ps.zip vs) r = some v → ∃ i : Nat, ps[i]? = some r ∧ vs[i]? = some v) ∧
    (AList.lookup (ps.zip vs) r = none → r ∉ ps)
  | [], [], _ => by simp [AList.lookup]
  | [], _ :: _, h => by simp at h
  | _ :: _, [], h => by simp at h
  | q :: ps, w :: vs, h => by
    simp only [List.length_cons, Nat.add_right_cancel_iff] at h
    obtain ⟨ih₁, ih₂⟩ := lookup_zip (r := r) h
    simp only [List.zip_cons_cons, AList.lookup]
    by_cases hq : q = r
    · subst hq
      simp only [↓reduceIte, Option.some.injEq, reduceCtorEq, false_implies, and_true]
      intro v hv
      exact ⟨0, rfl, by simp [hv]⟩
    · simp only [hq, ↓reduceIte]
      refine ⟨fun v hv => ?_, fun hn => ?_⟩
      · obtain ⟨i, h1, h2⟩ := ih₁ v hv
        exact ⟨i + 1, by simpa using h1, by simpa using h2⟩
      · simp only [List.mem_cons, not_or]
        exact ⟨fun e => hq e.symm, ih₂ hn⟩


theorem lookup_append_none {l₁ l₂ : AList κ α} {k : κ} (h : lookup l₁ k = none) :
    lookup (l₁ ++ l₂) k = lookup l₂ k := by rw [lookup_append, h]

theorem lookup_append_some {l₁ l₂ : AList κ α} {k : κ} {a : α}
    (h : lookup l₁ k = some a) : lookup (l₁ ++ l₂) k = some a := by rw [lookup_append, h]

end ConcreteSpec.Core.AList
