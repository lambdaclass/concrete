import DesignExamples.Linear

/-!
# Design example 1, general form: the linearity checker is sound for the model

`Linear.lean` checks concrete programs. This file proves the same properties for **every**
program the miniature checker accepts:

* `checkFn_sound` — on every input, an accepted function body never gets stuck in the
  instrumented semantics, destroys every resource exactly once at every fall-through or
  `return` exit, and never destroys a resource twice even when it aborts;
* `erasure` — whenever the instrumented semantics is not stuck, the erased machine produces
  the same outcome and trace (ghost ownership is erasable);
* `checkFn_erasure` — so for accepted programs the machine's trace is the instrumented one.

The proof is a simulation invariant (`Inv`) between the checker's live set and the
instrumented state. It is what S14 must do for Core at scale; doing it here shows the rule set
of the model (use-once, no overwrite of a live owner, branch agreement, consume-all on
`return`, `abort` exempt) is closed under the semantics.
-/

namespace DesignExamples.Linear

theorem lookup_isSome_iff (x : Nat) (own : List (Nat × Nat)) :
    (lookup x own).isSome ↔ x ∈ own.map Prod.fst := by
  induction own with
  | nil => simp [lookup]
  | cons p rest ih =>
    obtain ⟨y, r⟩ := p
    by_cases h : y = x
    · simp [lookup, h]
    · simp only [lookup, h, ite_false, ih, List.map_cons, List.mem_cons]
      exact ⟨Or.inr, fun e => e.resolve_left (fun e' => h e'.symm)⟩

theorem mem_of_lookup {x : Nat} {r : Nat} {own : List (Nat × Nat)}
    (h : lookup x own = some r) : (x, r) ∈ own := by
  induction own with
  | nil => simp [lookup] at h
  | cons p rest ih =>
    obtain ⟨y, r'⟩ := p
    by_cases hy : y = x
    · simp [lookup, hy] at h; subst hy; subst h; simp
    · simp [lookup, hy] at h; exact List.mem_cons_of_mem _ (ih h)

theorem erase_eq_filter (x : Nat) (own : List (Nat × Nat)) :
    erase x own = own.filter (fun p => p.1 != x) := by
  induction own with
  | nil => rfl
  | cons p rest ih =>
    obtain ⟨y, r⟩ := p
    by_cases h : y = x <;> simp [erase, h, ih]

theorem lookup_erase (x z : Nat) (own : List (Nat × Nat)) :
    lookup z (erase x own) = if z = x then none else lookup z own := by
  induction own with
  | nil => simp [erase, lookup]
  | cons p rest ih =>
    obtain ⟨y, r⟩ := p
    by_cases hy : y = x <;> by_cases hz : y = z <;> simp_all [erase, lookup] <;> omega

/-- Under unique keys, the owner of resource `r` is the only entry carrying `r`. -/
theorem key_unique {own : List (Nat × Nat)} (hk : (own.map Prod.fst).Nodup)
    {x : Nat} {r r' : Nat} (h1 : (x, r) ∈ own) (h2 : (x, r') ∈ own) : r = r' := by
  induction own with
  | nil => simp at h1
  | cons p rest ih =>
    simp only [List.map_cons, List.nodup_cons, List.mem_map] at hk
    rcases List.mem_cons.1 h1 with e1 | e1 <;> rcases List.mem_cons.1 h2 with e2 | e2
    · subst e1; cases e2; rfl
    · subst e1; exact absurd ⟨_, e2, rfl⟩ hk.1
    · subst e2; exact absurd ⟨_, e1, rfl⟩ hk.1
    · exact ih hk.2 e1 e2

theorem res_unique {own : List (Nat × Nat)} (hr : (own.map Prod.snd).Nodup)
    {x y : Nat} {r : Nat} (h1 : (x, r) ∈ own) (h2 : (y, r) ∈ own) : x = y := by
  induction own with
  | nil => simp at h1
  | cons p rest ih =>
    simp only [List.map_cons, List.nodup_cons, List.mem_map] at hr
    rcases List.mem_cons.1 h1 with e1 | e1 <;> rcases List.mem_cons.1 h2 with e2 | e2
    · subst e1; cases e2; rfl
    · subst e1; exact absurd ⟨_, e2, rfl⟩ hr.1
    · subst e2; exact absurd ⟨_, e1, rfl⟩ hr.1
    · exact ih hr.2 e1 e2

/-- The invariant linking the checker's live set to the instrumented state. -/
structure Inv (L : List Nat) (s : St) : Prop where
  nodup : L.Nodup
  live : ∀ x, x ∈ L ↔ x ∈ s.own.map Prod.fst
  keys : (s.own.map Prod.fst).Nodup
  res : (s.own.map Prod.snd).Nodup
  bound : ∀ p ∈ s.own, p.2 < s.next
  frees : ∀ r, frees s.trace r = if r < s.next ∧ r ∉ s.own.map Prod.snd then 1 else 0

def Good : CRes → Out → Prop
  | R, .fall s' => ∃ L', R = .fall L' ∧ Inv L' s'
  | _, .ret s' => Inv [] s'
  | _, .abort s' => ∃ L', Inv L' s'
  | _, .stuck => False

theorem frees_alloc (t : List Event) (n r : Nat) :
    frees (t ++ [.alloc n]) r = frees t r := by
  simp [frees, List.count_append]

theorem frees_free (t : List Event) (n r : Nat) :
    frees (t ++ [.free n]) r = frees t r + if r = n then 1 else 0 := by
  by_cases h : r = n
  · subst h; simp [frees, List.count_append]
  · simp [frees, List.count_append, h, Ne.symm h]

theorem mem_fst_filter (x z : Nat) (own : List (Nat × Nat)) :
    z ∈ (own.filter (fun p => p.1 != x)).map Prod.fst ↔ z ≠ x ∧ z ∈ own.map Prod.fst := by
  simp only [List.mem_map, List.mem_filter, bne_iff_ne, ne_eq]
  constructor
  · rintro ⟨p, ⟨hp, hx⟩, rfl⟩; exact ⟨hx, p, hp, rfl⟩
  · rintro ⟨hx, p, hp, rfl⟩; exact ⟨p, ⟨hp, hx⟩, rfl⟩

/-- Under unique keys and resources, removing owner `x` (which owns `r`) removes exactly `r`. -/
theorem mem_snd_filter {own : List (Nat × Nat)} (hk : (own.map Prod.fst).Nodup)
    (hr : (own.map Prod.snd).Nodup)
    {x : Nat} {r : Nat} (hx : (x, r) ∈ own) (r' : Nat) :
    r' ∈ (own.filter (fun p => p.1 != x)).map Prod.snd ↔ r' ≠ r ∧ r' ∈ own.map Prod.snd := by
  simp only [List.mem_map, List.mem_filter, bne_iff_ne, ne_eq]
  constructor
  · rintro ⟨⟨z, r''⟩, ⟨hp, hzx⟩, rfl⟩
    refine ⟨?_, ⟨z, r''⟩, hp, rfl⟩
    intro e; subst e
    exact hzx (res_unique hr hp hx)
  · rintro ⟨hne, ⟨z, r''⟩, hp, rfl⟩
    refine ⟨⟨z, r''⟩, ⟨hp, ?_⟩, rfl⟩
    intro e; subst e
    exact hne (key_unique hk hp hx)

theorem ite_t {α : Type} {p : Prop} [Decidable p] {a b : α} (h : p) : (if p then a else b) = a := by
  simp [h]
theorem ite_f {α : Type} {p : Prop} [Decidable p] {a b : α} (h : ¬p) : (if p then a else b) = b := by
  simp [h]

theorem ite_iff {p q : Prop} [Decidable p] [Decidable q] {a b : Nat} (h : p ↔ q) :
    (if p then a else b) = (if q then a else b) := by
  by_cases hp : p
  · rw [ite_t hp, ite_t (h.1 hp)]
  · rw [ite_f hp, ite_f (fun hq => hp (h.2 hq))]

theorem inv_acquire {L : List Nat} {s : St} {x : Nat} (h : Inv L s) (hx : x ∉ L) :
    Inv (x :: L) { own := (x, s.next) :: s.own, next := s.next + 1,
                   trace := s.trace ++ [.alloc s.next] } where
  nodup := List.nodup_cons.2 ⟨hx, h.nodup⟩
  live z := by simp [h.live z]
  keys := by
    refine List.nodup_cons.2 ⟨?_, h.keys⟩
    simpa [h.live x] using hx
  res := by
    refine List.nodup_cons.2 ⟨?_, h.res⟩
    simp only [List.mem_map, not_exists, not_and]
    intro p hp e; have := h.bound p hp
    omega
  bound p hp := by
    rcases List.mem_cons.1 hp with e | e
    · subst e; simp
    · have := h.bound p e; show p.2 < s.next + 1; omega
  frees r := by
    rw [frees_alloc, h.frees r]
    have hS : r ∈ s.own.map Prod.snd → r < s.next := by
      simp only [List.mem_map]; rintro ⟨p, hp, rfl⟩; exact h.bound p hp
    refine ite_iff ?_
    simp only [List.map_cons, List.mem_cons]
    constructor
    · rintro ⟨h1, h2⟩; refine ⟨by omega, ?_⟩
      rintro (e | e)
      · omega
      · exact h2 e
    · rintro ⟨h1, h2⟩
      have hne : r ≠ s.next := fun e => h2 (Or.inl e)
      exact ⟨by omega, fun e => h2 (Or.inr e)⟩

theorem inv_move {L : List Nat} {s : St} {x y : Nat} {r : Nat} (h : Inv L s)
    (hl : lookup x s.own = some r) (hy : y ∉ L.erase x) :
    Inv (y :: L.erase x) { s with own := (y, r) :: erase x s.own } where
  nodup := List.nodup_cons.2 ⟨hy, h.nodup.erase x⟩
  live z := by
    simp only [List.mem_cons, h.nodup.mem_erase_iff, erase_eq_filter, List.map_cons,
      mem_fst_filter, h.live z]
  keys := by
    rw [erase_eq_filter]
    refine List.nodup_cons.2 ⟨?_, h.keys.sublist (List.filter_sublist.map _)⟩
    rw [mem_fst_filter, ← h.live y, ← h.nodup.mem_erase_iff]; exact hy
  res := by
    rw [erase_eq_filter]
    refine List.nodup_cons.2 ⟨?_, h.res.sublist (List.filter_sublist.map _)⟩
    rw [mem_snd_filter h.keys h.res (mem_of_lookup hl)]; simp
  bound p hp := by
    rcases List.mem_cons.1 hp with e | e
    · subst e; show r < s.next; exact h.bound _ (mem_of_lookup hl)
    · rw [erase_eq_filter] at e; exact h.bound p (List.mem_filter.1 e).1
  frees r' := by
    rw [h.frees r']
    have hx := mem_of_lookup hl
    have : r' ∈ ((y, r) :: erase x s.own).map Prod.snd ↔ r' ∈ s.own.map Prod.snd := by
      rw [erase_eq_filter, List.map_cons, List.mem_cons, mem_snd_filter h.keys h.res hx]
      constructor
      · rintro (e | ⟨_, e⟩)
        · subst e; exact List.mem_map.2 ⟨_, hx, rfl⟩
        · exact e
      · intro e; by_cases he : r' = r
        · exact Or.inl he
        · exact Or.inr ⟨he, e⟩
    exact ite_iff (and_congr Iff.rfl (not_congr this.symm))

theorem inv_destroy {L : List Nat} {s : St} {x : Nat} {r : Nat} (h : Inv L s)
    (hl : lookup x s.own = some r) :
    Inv (L.erase x) { s with own := erase x s.own, trace := s.trace ++ [.free r] } where
  nodup := h.nodup.erase x
  live z := by
    simp only [h.nodup.mem_erase_iff, erase_eq_filter, mem_fst_filter, h.live z]
  keys := by rw [erase_eq_filter]; exact h.keys.sublist (List.filter_sublist.map _)
  res := by rw [erase_eq_filter]; exact h.res.sublist (List.filter_sublist.map _)
  bound p hp := by rw [erase_eq_filter] at hp; exact h.bound p (List.mem_filter.1 hp).1
  frees r' := by
    have hx := mem_of_lookup hl
    have hb := h.bound _ hx
    have hm := mem_snd_filter h.keys h.res hx
    have hrS : r ∈ s.own.map Prod.snd := List.mem_map.2 ⟨_, hx, rfl⟩
    show frees (s.trace ++ [.free r]) r' =
      if r' < s.next ∧ r' ∉ (erase x s.own).map Prod.snd then 1 else 0
    rw [frees_free, h.frees r', erase_eq_filter]
    by_cases he : r' = r
    · subst he
      rw [ite_f (fun hc => hc.2 hrS), ite_t rfl, ite_t ⟨hb, fun hc => ((hm r').1 hc).1 rfl⟩]
    · rw [ite_f he, Nat.add_zero]
      have hiff : r' ∈ (s.own.filter (fun p => p.1 != x)).map Prod.snd ↔
          r' ∈ s.own.map Prod.snd := by
        rw [hm r']; exact ⟨fun hc => hc.2, fun hc => ⟨he, hc⟩⟩
      exact ite_iff (and_congr Iff.rfl (not_congr hiff.symm))

theorem inv_setEq {La Lb : List Nat} {s : St} (h : Inv Lb s) (hn : La.Nodup)
    (he : setEq La Lb = true) : Inv La s where
  nodup := hn
  live z := by
    simp only [setEq, Bool.and_eq_true, List.all_eq_true, List.contains_iff_mem] at he
    rw [← h.live z]; exact ⟨he.1 z, he.2 z⟩
  keys := h.keys
  res := h.res
  bound := h.bound
  frees := h.frees

theorem some_fall_inj {L L' : List Nat} (h : some (CRes.fall L) = some (CRes.fall L')) :
    L = L' := by
  simpa using h

theorem chk_nodup (a : Stmt) :
    ∀ (L L' : List Nat), chk a L = some (.fall L') → L.Nodup → L'.Nodup := by
  induction a with
  | acquire x =>
    intro L L' h hn
    simp only [chk] at h
    split at h
    · cases h
    · rename_i hc; cases some_fall_inj h
      exact List.nodup_cons.2 ⟨by simpa using hc, hn⟩
  | move x y =>
    intro L L' h hn
    simp only [chk] at h
    split at h
    · split at h
      · cases h
      · rename_i hc; cases some_fall_inj h
        exact List.nodup_cons.2 ⟨by simpa using hc, hn.erase x⟩
    · cases h
  | destroy x =>
    intro L L' h hn
    simp only [chk] at h
    split at h
    · cases some_fall_inj h; exact hn.erase x
    · cases h
  | seq a b iha ihb =>
    intro L L' h hn
    simp only [chk] at h
    split at h
    · rename_i L1 h1; exact ihb L1 L' h (iha L L1 h1 hn)
    · cases h
    · cases h
  | ite c a b iha ihb =>
    intro L L' h hn
    simp only [chk] at h
    split at h
    · rename_i La Lb ha hb; split at h
      · exact some_fall_inj h ▸ iha L La ha hn
      · cases h
    · rename_i La ha hb; exact some_fall_inj h ▸ iha L La ha hn
    · rename_i Lb ha hb; exact some_fall_inj h ▸ ihb L Lb hb hn
    · cases h
    · cases h
  | ret =>
    intro L L' h _
    simp only [chk] at h
    split at h <;> cases h
  | abort => intro L L' h _; cases h
  | skip => intro L L' h hn; cases some_fall_inj h; exact hn

theorem good_change {R R' : CRes} {o : Out} (h : Good R o)
    (hf : ∀ L s', R = .fall L → Inv L s' → ∃ L', R' = .fall L' ∧ Inv L' s') : Good R' o := by
  cases o with
  | fall s' => obtain ⟨L, hR, hi⟩ := h; exact hf L s' hR hi
  | ret s' => exact h
  | abort s' => exact h
  | stuck => exact h

/-- **Checker soundness for the model**: an accepted statement, run from a state the
invariant relates to its live set, never gets stuck, and every outcome re-establishes the
invariant (with nothing live on a returning path). -/
theorem exec_sound (inp : List Bool) (a : Stmt) :
    ∀ (L : List Nat) (s : St) (R : CRes), chk a L = some R → Inv L s →
      Good R (exec inp a s) := by
  induction a with
  | acquire x =>
    intro L s R h hi
    simp only [chk] at h
    split at h
    · cases h
    · rename_i hc
      cases h
      have hx : x ∉ L := by simpa using hc
      have hl : (lookup x s.own).isSome = false := by
        rw [Bool.eq_false_iff]; intro e
        exact hx ((hi.live x).2 ((lookup_isSome_iff x _).1 e))
      simp only [exec, hl, Bool.false_eq_true, ite_false]
      exact ⟨_, rfl, inv_acquire hi hx⟩
  | move x y =>
    intro L s R h hi
    simp only [chk] at h
    split at h
    · rename_i hxL
      split at h
      · cases h
      · rename_i hy
        cases h
        have hxL' : x ∈ L := by simpa using hxL
        have hy' : y ∉ L.erase x := by simpa using hy
        obtain ⟨r, hr⟩ : ∃ r, lookup x s.own = some r :=
          Option.isSome_iff_exists.1 ((lookup_isSome_iff x s.own).2 ((hi.live x).1 hxL'))
        have hly : (lookup y (erase x s.own)).isSome = false := by
          rw [lookup_erase]
          by_cases hyx : y = x
          · simp [hyx]
          · rw [ite_f hyx, Bool.eq_false_iff]; intro e
            exact hy' ((hi.nodup.mem_erase_iff).2
              ⟨hyx, (hi.live y).2 ((lookup_isSome_iff y _).1 e)⟩)
        simp only [exec, hr, hly, Bool.false_eq_true, ite_false]
        exact ⟨_, rfl, inv_move hi hr hy'⟩
    · cases h
  | destroy x =>
    intro L s R h hi
    simp only [chk] at h
    split at h
    · rename_i hxL
      cases h
      have hxL' : x ∈ L := by simpa using hxL
      obtain ⟨r, hr⟩ : ∃ r, lookup x s.own = some r :=
        Option.isSome_iff_exists.1 ((lookup_isSome_iff x s.own).2 ((hi.live x).1 hxL'))
      simp only [exec, hr]
      exact ⟨_, rfl, inv_destroy hi hr⟩
    · cases h
  | seq a b iha ihb =>
    intro L s R h hi
    simp only [chk] at h
    split at h
    · rename_i L1 h1
      have ga := iha L s _ h1 hi
      cases hea : exec inp a s with
      | fall s' =>
        rw [hea] at ga; obtain ⟨L2, hL2, hi2⟩ := ga; cases hL2
        simp only [exec, hea]; exact ihb L1 s' R h hi2
      | ret s' => rw [hea] at ga; simp only [exec, hea]; exact ga
      | abort s' => rw [hea] at ga; simp only [exec, hea]; exact ga
      | stuck => rw [hea] at ga; exact ga.elim
    · rename_i h1
      cases h
      have ga := iha L s _ h1 hi
      cases hea : exec inp a s with
      | fall s' => rw [hea] at ga; obtain ⟨_, hc, _⟩ := ga; cases hc
      | ret s' => rw [hea] at ga; simp only [exec, hea]; exact ga
      | abort s' => rw [hea] at ga; simp only [exec, hea]; exact ga
      | stuck => rw [hea] at ga; exact ga.elim
    · cases h
  | ite c a b iha ihb =>
    intro L s R h hi
    simp only [chk] at h
    have gen : ∀ (Ra : CRes), chk a L = some Ra →
        (∀ L s', Ra = .fall L → Inv L s' → ∃ L', R = .fall L' ∧ Inv L' s') →
        ∀ (Rb : CRes), chk b L = some Rb →
        (∀ L s', Rb = .fall L → Inv L s' → ∃ L', R = .fall L' ∧ Inv L' s') →
        Good R (exec inp (.ite c a b) s) := by
      intro Ra ha fa Rb hb fb
      simp only [exec]
      split
      · exact good_change (iha L s Ra ha hi) fa
      · exact good_change (ihb L s Rb hb hi) fb
    split at h
    · rename_i La Lb ha hb
      split at h
      · rename_i he
        cases h
        have hn := chk_nodup a L La ha hi.nodup
        exact gen _ ha (fun L' s' e hi' => by cases e; exact ⟨_, rfl, hi'⟩) _ hb
          (fun L' s' e hi' => by cases e; exact ⟨_, rfl, inv_setEq hi' hn he⟩)
      · cases h
    · rename_i La ha hb
      cases h
      exact gen _ ha (fun L' s' e hi' => by cases e; exact ⟨_, rfl, hi'⟩) _ hb
        (fun _ _ e _ => by cases e)
    · rename_i Lb ha hb
      cases h
      exact gen _ ha (fun _ _ e _ => by cases e) _ hb
        (fun L' s' e hi' => by cases e; exact ⟨_, rfl, hi'⟩)
    · rename_i ha hb
      cases h
      exact gen _ ha (fun _ _ e _ => by cases e) _ hb (fun _ _ e _ => by cases e)
    · cases h
  | ret =>
    intro L s R h hi
    simp only [chk] at h
    split at h
    · rename_i hL
      cases h
      have : L = [] := by simpa using hL
      subst this
      exact hi
    · cases h
  | abort => intro L s R h hi; exact ⟨L, hi⟩
  | skip =>
    intro L s R h hi
    cases h
    exact ⟨L, rfl, hi⟩

theorem inv_noDoubleDestroy {L : List Nat} {s : St} (h : Inv L s) : noDoubleDestroy s = true := by
  simp only [noDoubleDestroy, Bool.and_eq_true, List.all_eq_true, decide_eq_true_eq]
  refine ⟨fun r _ => by rw [h.frees r]; split <;> omega, fun e he => ?_⟩
  cases e with
  | alloc _ => rfl
  | free r =>
    have hpos : frees s.trace r ≠ 0 := by
      simp only [frees]; have := List.count_pos_iff.2 he; omega
    rw [h.frees r] at hpos
    simp only [decide_eq_true_eq]
    by_cases hc : r < s.next ∧ r ∉ s.own.map Prod.snd
    · exact hc.1
    · rw [ite_f hc] at hpos; exact absurd rfl hpos

theorem inv_nil_noLeak {s : St} (h : Inv [] s) : noLeak s = true := by
  have hown : s.own = [] := by
    cases ho : s.own with
    | nil => rfl
    | cons p rest =>
      have := (h.live p.1).2 (by rw [ho]; exact List.mem_cons_self ..)
      simp at this
  simp only [noLeak, List.all_eq_true, List.mem_range, decide_eq_true_eq]
  intro r hr
  rw [h.frees r, hown, ite_t ⟨hr, by simp⟩]; exact Nat.le_refl 1

/-- **The general theorem.** Every function body the checker accepts, on every input:
never gets stuck, conserves every resource (each allocated resource is destroyed exactly
once) at every normal or `return` exit, and never destroys twice even when it aborts. -/
theorem checkFn_sound (p : Stmt) (h : checkFn p = true) (inp : List Bool) :
    safeOutcome (run inp p) = true := by
  have hinit : Inv [] init :=
    { nodup := List.nodup_nil, live := fun x => by simp [init], keys := List.nodup_nil,
      res := List.nodup_nil, bound := fun p hp => by simp [init] at hp,
      frees := fun r => by simp [init, frees] }
  unfold checkFn at h
  split at h
  · rename_i L hc
    have hL : L = [] := by simpa using h
    subst hL
    have g := exec_sound inp p [] init _ hc hinit
    unfold run
    cases ho : exec inp p init with
    | fall s' =>
      rw [ho] at g; obtain ⟨L', e, hi⟩ := g; cases e
      simp [safeOutcome, inv_noDoubleDestroy hi, inv_nil_noLeak hi]
    | ret s' =>
      rw [ho] at g
      simp [safeOutcome, inv_noDoubleDestroy g, inv_nil_noLeak g]
    | abort s' =>
      rw [ho] at g; obtain ⟨_, hi⟩ := g
      simp [safeOutcome, inv_noDoubleDestroy hi]
    | stuck => rw [ho] at g; exact g.elim
  · rename_i hc
    have g := exec_sound inp p [] init _ hc hinit
    unfold run
    cases ho : exec inp p init with
    | fall s' => rw [ho] at g; obtain ⟨_, e, _⟩ := g; cases e
    | ret s' =>
      rw [ho] at g
      simp [safeOutcome, inv_noDoubleDestroy g, inv_nil_noLeak g]
    | abort s' =>
      rw [ho] at g; obtain ⟨_, hi⟩ := g
      simp [safeOutcome, inv_noDoubleDestroy hi]
    | stuck => rw [ho] at g; exact g.elim
  · cases h

/-! ## Erasure: the machine agrees with the instrumented semantics whenever it is not stuck -/

/-- The erased state agrees with the instrumented one on every live owner. -/
structure ERel (e s : St) : Prop where
  next : e.next = s.next
  trace : e.trace = s.trace
  own : ∀ z, (lookup z s.own).isSome → lookup z e.own = lookup z s.own

def OutRel : Out → Out → Prop
  | .fall e, .fall s | .ret e, .ret s | .abort e, .abort s => ERel e s
  | _, _ => False

theorem erasure (inp : List Bool) (a : Stmt) :
    ∀ e s, ERel e s → exec inp a s ≠ .stuck → OutRel (execErased inp a e) (exec inp a s) := by
  induction a with
  | acquire x =>
    intro e s hr hns
    by_cases hc : (lookup x s.own).isSome = true
    · simp [exec, hc] at hns
    · simp only [exec, hc, Bool.false_eq_true, ite_false, execErased, OutRel]
      refine ⟨by simp [hr.next], by simp [hr.trace, hr.next], fun z hz => ?_⟩
      by_cases hzx : x = z
      · simp [lookup, hzx, hr.next]
      · simp only [lookup, ite_f hzx] at hz ⊢
        rw [lookup_erase, ite_f (Ne.symm hzx)]; exact hr.own z hz
  | move x y =>
    intro e s hr hns
    cases hl : lookup x s.own with
    | none => simp [exec, hl] at hns
    | some r =>
      by_cases hc : (lookup y (erase x s.own)).isSome = true
      · simp [exec, hl, hc] at hns
      · have hle := hr.own x (by rw [hl]; rfl)
        rw [hl] at hle
        simp only [exec, hl, hc, Bool.false_eq_true, ite_false, execErased, hle, OutRel]
        refine ⟨hr.next, hr.trace, fun z hz => ?_⟩
        by_cases hzy : y = z
        · simp [lookup, hzy]
        · simp only [lookup, ite_f hzy] at hz ⊢
          rw [lookup_erase, ite_f (Ne.symm hzy)]
          rw [lookup_erase] at hz ⊢
          by_cases hzx : z = x
          · simp [hzx] at hz
          · rw [ite_f hzx] at hz ⊢; exact hr.own z hz
  | destroy x =>
    intro e s hr hns
    cases hl : lookup x s.own with
    | none => simp [exec, hl] at hns
    | some r =>
      have hle := hr.own x (by rw [hl]; rfl)
      rw [hl] at hle
      simp only [exec, hl, execErased, hle, OutRel]
      refine ⟨hr.next, by simp [hr.trace], fun z hz => ?_⟩
      rw [lookup_erase] at hz ⊢
      by_cases hzx : z = x
      · simp [hzx] at hz
      · rw [ite_f hzx] at hz ⊢; exact hr.own z hz
  | seq a b iha ihb =>
    intro e s hr hns
    simp only [exec, execErased] at hns ⊢
    have ga := iha e s hr
    cases hea : exec inp a s with
    | fall s' =>
      rw [hea] at hns ga
      cases heb : execErased inp a e with
      | fall e' =>
        rw [heb] at ga
        exact ihb e' s' (ga (by simp)) hns
      | _ => rw [heb] at ga; exact (ga (by simp)).elim
    | ret s' =>
      rw [hea] at ga
      cases heb : execErased inp a e with
      | ret e' => rw [heb] at ga; exact ga (by simp)
      | _ => rw [heb] at ga; exact (ga (by simp)).elim
    | abort s' =>
      rw [hea] at ga
      cases heb : execErased inp a e with
      | abort e' => rw [heb] at ga; exact ga (by simp)
      | _ => rw [heb] at ga; exact (ga (by simp)).elim
    | stuck => rw [hea] at hns; exact absurd rfl hns
  | ite c a b iha ihb =>
    intro e s hr hns
    simp only [exec, execErased] at hns ⊢
    split
    · exact iha e s hr (by simpa [*] using hns)
    · exact ihb e s hr (by simpa [*] using hns)
  | ret => intro e s hr _; exact hr
  | abort => intro e s hr _; exact hr
  | skip => intro e s hr _; exact hr

/-- **Erasure for accepted programs**: the machine's trace is the instrumented trace. -/
theorem checkFn_erasure (p : Stmt) (h : checkFn p = true) (inp : List Bool) :
    traceOf (runErased inp p) = traceOf (run inp p) := by
  have hns : run inp p ≠ .stuck := by
    intro hs; have := checkFn_sound p h inp; rw [hs] at this; cases this
  have := erasure inp p init init ⟨rfl, rfl, fun _ _ => rfl⟩ hns
  unfold runErased; unfold run at hns ⊢
  revert this
  cases execErased inp p init <;> cases exec inp p init <;>
    simp [OutRel, traceOf] <;> intro hr <;> exact hr.trace

end DesignExamples.Linear
