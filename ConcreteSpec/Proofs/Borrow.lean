import ConcreteSpec.Proofs.Basics
import ConcreteSpec.Spec.Basics
import ConcreteSpec.Spec.Borrow

/-!
# Part 4: `open_mut_freezes_root` and `shared_open_grants_no_write`.

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). See `ConcreteSpec/README.md`.
-/

namespace ConcreteSpec.Borrow
open Core (AList)

theorem lookup_bodyCtx {m v src Γ p} (hv : p ≠ v) :
    AList.lookup (bodyCtx m v src Γ) p = (AList.lookup Γ p).map (lockFor m src.root) := by
  simp only [bodyCtx, AList.lookup, show ¬ v = p from fun h => hv h.symm, ↓reduceIte]
  exact AList.lookup_map _ Γ p

theorem lockFor_excl {m r q} (h : q.lock = .excl) : (lockFor m r q).lock = .excl := by
  unfold lockFor; split <;> (try split) <;> (try split) <;> simp_all

theorem ne_of_lookup {Γ : Ctx} {p v : Var} {q : Place} (hp : AList.lookup Γ p = some q) (hv : AList.lookup Γ v = none) : p ≠ v := by
  rintro rfl; rw [hp] at hv; cases hv

theorem lookup_restore (Γb : Ctx) : ∀ (Γ : Ctx) (p : Var),
    AList.lookup (restore Γ Γb) p = if (AList.lookup Γb p).isSome then AList.lookup Γ p else none
  | [], _ => by simp [restore, AList.lookup]
  | (n, q) :: Γ, p => by
    have ih := lookup_restore Γb Γ p
    unfold restore at ih ⊢
    cases hb : (AList.lookup Γb n).isSome
    · rw [List.filter_cons_of_neg (p := fun e : Var × Place => (AList.lookup Γb e.1).isSome) (a := (n, q))
        (fun h => Bool.noConfusion (hb.symm.trans h)), ih, AList.lookup_cons]
      by_cases hnp : n = p
      · subst hnp; rw [hb]; rfl
      · rw [ite_eq_right hnp]
    · rw [List.filter_cons_of_pos (p := fun e : Var × Place => (AList.lookup Γb e.1).isSome) (a := (n, q)) hb, AList.lookup_cons,
        AList.lookup_cons]
      by_cases hnp : n = p
      · subst hnp
        rw [hb, ite_eq_left (rfl : true = true), ite_eq_left (rfl : n = n), ite_eq_left (rfl : n = n)]
      · rw [ite_eq_right hnp, ite_eq_right hnp, ih]

theorem lookup_restore_some {Γ Γb p q} (h : AList.lookup (restore Γ Γb) p = some q) : AList.lookup Γ p = some q := by
  rw [lookup_restore] at h
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
    ∀ {p q}, AList.lookup Γ p = some q → q.lock ≠ .free → AList.lookup Γ' p = some q := by
  induction h with
  | nil => intro p q hp _; exact hp
  | read _ _ _ ih => exact ih
  | write _ _ _ _ ih => exact ih
  | @move Γ p0 q0 rest Γ' hf hl _ _ ih =>
    intro p q hp hq
    have hne : p ≠ p0 := by rintro rfl; rw [hf] at hp; cases hp; exact hq hl
    exact ih (by rw [AList.lookup_erase]; simp only [hne, ↓reduceIte]; exact hp) hq
  | @borrow Γ m s v q0 body rest Γb Γ' _ _ _ hv _ _ ihb ih =>
    intro p q hp hq
    have hb : AList.lookup Γb p = some (lockFor m q0.root q) :=
      ihb (by rw [lookup_bodyCtx (ne_of_lookup hp hv), hp]; rfl) (lockFor_nonfree hq)
    exact ih (by rw [lookup_restore, hb]; exact hp) hq

/-- A frozen place is never named, at any depth. -/
theorem excl_untouched {Γ ss Γ'} (h : Ok Γ ss Γ') :
    ∀ {p q}, AList.lookup Γ p = some q → q.lock = .excl → ¬ Touches p ss := by
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
      rw [AList.lookup_erase]; simp only [hne, ↓reduceIte]; exact hp
  | @borrow Γ m s v q0 body rest Γb Γ' hs hnex _ hv hbody _ ihb ih =>
    intro p q hp hq ht
    have hpb : AList.lookup (bodyCtx m v q0 Γ) p = some (lockFor m q0.root q) := by
      rw [lookup_bodyCtx (ne_of_lookup hp hv), hp]; rfl
    cases ht with
    | src => rw [hs] at hp; cases hp; exact hnex hq
    | body ht => exact ihb hpb (lockFor_excl hq) ht
    | tail ht =>
      have hb := ok_keeps_locked hbody hpb (lockFor_nonfree (by rw [hq]; decide))
      exact ih (by rw [lookup_restore, hb]; exact hp) hq ht

/-- **`open_mut_freezes_root`** (Q9.14, Q3.3, Q3.4, REF-11). Inside an accepted
`borrow mut src as v { body }`, no place of `src`'s root that existed before the open (the root
itself and every view derived from it) is read, written, moved or opened again, at any depth.
Only the new view `v` reaches the root. -/
theorem open_mut_freezes_root {Γ s v body rest q Γ'} (h : Ok Γ (.borrow true s v body :: rest) Γ')
    (hs : AList.lookup Γ s = some q) :
    ∀ p q', AList.lookup Γ p = some q' → q'.root = q.root → ¬ Touches p body := by
  intro p q' hp hr
  cases h with
  | borrow hs' _ _ hv hb _ =>
    rw [hs] at hs'; cases hs'
    refine excl_untouched hb (q := lockFor true q.root q') ?_ (by simp [lockFor, hr]) 
    rw [lookup_bodyCtx (ne_of_lookup hp hv), hp]; rfl

/-- The places of root `r` grant no write: each is locked, or neither writable nor an owner. -/
def NoWrite (r : Var) (Γ : Ctx) : Prop :=
  ∀ p q, AList.lookup Γ p = some q → q.root = r → q.lock = .free → q.mutable = false ∧ q.owner = false

theorem noWrite_restore {r Γ Γb} (h : NoWrite r Γ) : NoWrite r (restore Γ Γb) :=
  fun p q hp => h p q (lookup_restore_some hp)

theorem noWrite_erase {r Γ p'} (h : NoWrite r Γ) : NoWrite r (AList.erase Γ p') := by
  intro p q hp
  rw [AList.lookup_erase] at hp
  split at hp
  · cases hp
  · exact h p q hp

theorem noWrite_bodyCtx {r Γ m v q0} (h : NoWrite r Γ)
    (hm : m = true → q0.lock = .free ∧ q0.mutable = true)
    (hsrc : q0.root = r → m = true → False) : NoWrite r (bodyCtx m v q0 Γ) := by
  intro p q hp hr hl
  by_cases hpv : p = v
  · subst hpv
    simp only [bodyCtx, AList.lookup, ↓reduceIte] at hp
    cases hp
    simp only [view] at hr ⊢
    cases m with
    | false => simp
    | true => exact (hsrc hr rfl).elim
  · rw [lookup_bodyCtx hpv] at hp
    cases hq : AList.lookup Γ p with
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
    | tail hs hw => cases hs with | move _ _ _ h0 => cases h0; exact ih (noWrite_erase hn) hw
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
    (h : Ok Γ (.borrow false s v body :: rest) Γ') (hs : AList.lookup Γ s = some q) :
    ¬ WritesRoot q.root (bodyCtx false v q Γ) body := by
  cases h with
  | borrow hs' _ hm _ hb _ =>
    rw [hs] at hs'; cases hs'
    refine noWrite_sound hb ?_
    intro p q' hp hr hl
    by_cases hpv : p = v
    · subst hpv; simp only [bodyCtx, AList.lookup, ↓reduceIte] at hp; cases hp; exact ⟨rfl, rfl⟩
    · rw [lookup_bodyCtx hpv] at hp
      cases hq : AList.lookup Γ p with
      | none => rw [hq] at hp; cases hp
      | some q1 =>
        rw [hq] at hp; cases hp
        by_cases h1 : q1.root = q.root
        · by_cases h2 : q1.lock = .free <;> simp [lockFor, h1, h2] at hl
        · simp [lockFor, h1] at hr

/-- A range is valid exactly when `a ≤ b` and both bounds are ends of the source. -/
theorem validRange_iff (s : Source) (a b : Nat) :
    validRange s a b = (decide (a ≤ b) && s.edge a && s.edge b) := by
  cases s with
  | array n =>
    simp only [validRange, Source.edge]
    by_cases h1 : a ≤ b <;> by_cases h2 : b ≤ n <;> by_cases h3 : a ≤ n <;> simp [h1, h2, h3] <;> omega
  | text bs =>
    simp only [validRange, Source.edge]
    by_cases h1 : a ≤ b <;> by_cases h2 : b ≤ bs.length <;> by_cases h3 : a ≤ bs.length <;>
      simp [h1, h2, h3] <;> first | omega | (cases boundary bs a <;> cases boundary bs b <;> simp)

theorem boundary_zero (bs : List Nat) : boundary bs 0 = true := by simp [boundary]

theorem boundary_len (bs : List Nat) : boundary bs bs.length = true := by simp [boundary]

theorem edge_zero (s : Source) : s.edge 0 = true := by
  cases s <;> simp [Source.edge, boundary_zero]

theorem edge_len (s : Source) : s.edge s.len = true := by
  cases s <;> simp [Source.edge, Source.len, boundary_len]

theorem edge_le {s : Source} {i : Nat} (h : s.edge i = true) : i ≤ s.len := by
  cases s <;> simp_all [Source.edge, Source.len]

/-- **A range accepted without `else` never traps** (no hidden traps). Its evidence holds: every
source and bounds that agree with what the checker knows pass the range test, so the range opens. -/
theorem range_never_traps {Γ m src v k body rest Γ'} (h : RangeOk Γ m src v k body none rest Γ')
    {s : Source} {a b : Nat} (hs : Agrees k s a b) : openRange s a b false = .enter := by
  have hk : knownValid k = true := h.2
  obtain ⟨h1, h2, h3⟩ := hs
  unfold knownValid at hk
  split at hk
  · rename_i s' a' b' hsrc ha hb
    rw [h1 s' hsrc, h2 a' ha, h3 b' hb] at hk
    simp [openRange, hk]
  · cases hk

/-- **An opened view stays inside its source.** When the range opens, every index of the view
is an index of the source, and for text both ends are scalar boundaries. -/
theorem enter_in_bounds {s : Source} {a b : Nat} {e : Bool} (h : openRange s a b e = .enter) :
    a ≤ b ∧ b ≤ s.len ∧ (∀ i, i < b - a → viewIndex a i < s.len) ∧
      ∀ bs, s = .text bs → boundary bs a = true ∧ boundary bs b = true := by
  have hv : validRange s a b = true := by
    unfold openRange at h; split at h
    · assumption
    · split at h <;> cases h
  cases s with
  | array n =>
    simp only [validRange, decide_eq_true_eq] at hv
    refine ⟨hv.1, hv.2, fun i hi => ?_, fun bs h => by cases h⟩
    simp only [viewIndex, Source.len]; omega
  | text bs =>
    simp only [validRange, Bool.and_eq_true, decide_eq_true_eq] at hv
    obtain ⟨⟨⟨hab, hb⟩, ha'⟩, hb'⟩ := hv
    refine ⟨hab, hb, fun i hi => ?_, fun bs' h => ?_⟩
    · simp only [viewIndex, Source.len]; omega
    · cases h; exact ⟨ha', hb'⟩

/-- **`open_mut_freezes_root` for range opens.** Inside an accepted
`borrow mut src from a until b as v { body } [else { … }]`, no place of `src`'s root that existed
before the open is touched in the body: the range clause changes nothing about access (Q4.7). -/
theorem range_open_mut_freezes_root {Γ src v k body els rest Γ' q}
    (h : RangeOk Γ true src v k body els rest Γ') (hs : AList.lookup Γ src = some q) :
    ∀ p q', AList.lookup Γ p = some q' → q'.root = q.root → ¬ Touches p body :=
  open_mut_freezes_root h.1 hs


/-! ### No escape -/

/-- **A view never moves.** A view is not an owner, so moving it (returning it, storing it,
passing it by value, capturing it: all moves in Core) is refused. -/
theorem view_not_moved {m v q Γ rest Γ'} : ¬ Ok (bodyCtx m v q Γ) (.move v :: rest) Γ' := by
  intro h
  cases h with
  | move hf _ ho _ => simp [bodyCtx, AList.lookup, view] at hf; subst hf; simp at ho

/-- A name not in scope is never used, at any depth (except as a rebound view name). -/
theorem fresh_not_used {Γ ss Γ'} (h : Ok Γ ss Γ') : ∀ {p}, AList.lookup Γ p = none → ¬ FreeUse p ss := by
  induction h with
  | nil => intro p _ hu; cases hu
  | read hf _ _ ih =>
    intro p hp hu
    cases hu with
    | read => rw [hf] at hp; cases hp
    | tail hu => exact ih hp hu
  | write hf _ _ _ ih =>
    intro p hp hu
    cases hu with
    | write => rw [hf] at hp; cases hp
    | tail hu => exact ih hp hu
  | move hf _ _ _ ih =>
    intro p hp hu
    cases hu with
    | move => rw [hf] at hp; cases hp
    | tail hu => exact ih (p := p) (by rw [AList.lookup_erase]; split <;> first | rfl | exact hp) hu
  | @borrow Γ m s v q0 body rest Γb Γ' hs _ _ _ _ _ ihb ih =>
    intro p hp hu
    cases hu with
    | src => rw [hs] at hp; cases hp
    | body hne hu =>
      exact ihb (by rw [lookup_bodyCtx (fun h => hne h.symm), hp]; rfl) hu
    | tail hu => exact ih (by rw [lookup_restore, hp]; split <;> rfl) hu

/-- **The view does not outlive its block.** No statement after
`borrow [mut] src as v { body }` names `v` (until a new block rebinds the name). -/
theorem view_not_used_after {Γ m s v body rest Γ'} (h : Ok Γ (.borrow m s v body :: rest) Γ') :
    ¬ FreeUse v rest := by
  cases h with
  | borrow _ _ _ hv _ hr => exact fresh_not_used hr (by rw [lookup_restore, hv]; split <;> rfl)

/-- **The root is usable again exactly as before.** After `borrow [mut] src as v { body }`, the
rest is checked with every place of `src`'s root present with exactly its entry from before the
open, lock included: the freeze of a mutable open (and the lock of a shared one) ends with the
block. -/
theorem root_restored {Γ m s v body rest q Γ'} (h : Ok Γ (.borrow m s v body :: rest) Γ')
    (hs : AList.lookup Γ s = some q) :
    ∃ Γb, Ok (bodyCtx m v q Γ) body Γb ∧ Ok (restore Γ Γb) rest Γ' ∧
      ∀ p q', AList.lookup Γ p = some q' → q'.root = q.root → AList.lookup (restore Γ Γb) p = some q' := by
  cases h with
  | @borrow _ _ _ _ q0 _ _ Γb _ hs' _ _ hv hb hr =>
    rw [hs] at hs'; cases hs'
    refine ⟨Γb, hb, hr, fun p q' hp hroot => ?_⟩
    have hpb : AList.lookup (bodyCtx m v q Γ) p = some (lockFor m q.root q') := by
      rw [lookup_bodyCtx (ne_of_lookup hp hv), hp]; rfl
    have hl : (lockFor m q.root q').lock ≠ .free := by
      cases m <;> cases hq : q'.lock <;> simp [lockFor, hroot, hq]
    rw [lookup_restore, ok_keeps_locked hb hpb hl]
    exact hp

/-! ### Range views at run time -/

theorem elemIdx_some {w : VInfo} {i j : Nat} :
    elemIdx w i = some j ↔ i < w.b - w.a ∧ j = w.a + i := by
  unfold elemIdx; by_cases h : i < w.b - w.a <;> simp [h, eq_comm]

/-- An index through a view lands in the view's range `[a, b)`. -/
theorem elemIdx_range {w : VInfo} {i j : Nat} (h : elemIdx w i = some j) : w.a ≤ j ∧ j < w.b := by
  rw [elemIdx_some] at h; omega

/-- Every view in scope is a range `a ≤ b ≤ len` of a source in memory. -/
def ViewsOk (μ : Mem) (V : Views) : Prop :=
  ∀ v w, AList.lookup V v = some w → ∃ xs, AList.lookup μ w.src = some xs ∧ w.a ≤ w.b ∧ w.b ≤ xs.length

/-- Opening a valid range adds a view that satisfies `ViewsOk`. -/
theorem viewsOk_enter {μ V p xs a b e v m} (hV : ViewsOk μ V) (hx : AList.lookup μ p = some xs)
    (ho : openRange (.array xs.length) a b e = .enter) :
    ViewsOk μ ((v, ⟨p, a, b, m⟩) :: V) := by
  intro v' w hw
  simp only [AList.lookup] at hw
  split at hw
  · cases hw
    obtain ⟨hab, hb, _⟩ := enter_in_bounds ho
    exact ⟨xs, hx, hab, hb⟩
  · exact hV v' w hw

/-- A write that keeps the source's length keeps `ViewsOk`. -/
theorem viewsOk_put {μ V p xs ys} (hV : ViewsOk μ V) (hx : AList.lookup μ p = some xs)
    (hl : ys.length = xs.length) : ViewsOk (AList.update μ p (fun _ => ys)) V := by
  intro v w hw
  obtain ⟨zs, hz, h1, h2⟩ := hV v w hw
  rw [AList.lookup_update]
  by_cases hs : w.src = p
  · subst hs; rw [hx] at hz; cases hz; exact ⟨ys, by simp [hx], h1, by omega⟩
  · exact ⟨zs, by simp [hs, hz], h1, h2⟩

/-- **Every access through a view stays in its range and in its source.** The index `i` of a
view of `[a, b)` reaches the source index `a + i`, which lies in `[a, b)` and inside the source;
an index outside the view has no source index, and the access traps. -/
theorem view_in_source {μ V v w i j} (hV : ViewsOk μ V) (hw : AList.lookup V v = some w)
    (hj : elemIdx w i = some j) :
    ∃ xs, AList.lookup μ w.src = some xs ∧ j = w.a + i ∧ w.a ≤ j ∧ j < w.b ∧ j < xs.length := by
  obtain ⟨xs, hx, h1, h2⟩ := hV v w hw
  rw [elemIdx_some] at hj
  exact ⟨xs, hx, hj.2, by omega, by omega, by omega⟩

/-- **Reading `v[i]`** outputs the source element `a + i` when `i < b - a`, and traps otherwise. -/
theorem view_get {μ V v w i n out rest} (hV : ViewsOk μ V) (hw : AList.lookup V v = some w) :
    ∃ xs, AList.lookup μ w.src = some xs ∧
      vrun (n + 1) μ V out (.get v i :: rest) =
        if i < w.b - w.a then vrun n μ V (out ++ [xs.getD (w.a + i) 0]) rest else .trap := by
  obtain ⟨xs, hx, h1, h2⟩ := hV v w hw
  refine ⟨xs, hx, ?_⟩
  simp only [vrun, hw]
  by_cases hi : i < w.b - w.a
  · have hj : w.a + i < xs.length := by omega
    have he : elem μ w.src (w.a + i) = some (xs.getD (w.a + i) 0) := by
      simp [elem, hx, hj]
    simp [elemIdx, hi, he]
  · simp [elemIdx, hi]

/-- **Writing `v[i] = x`** through a mutable view writes the source element `a + i` when
`i < b - a`, and traps otherwise. -/
theorem view_set {μ V v w i x n out rest} (hV : ViewsOk μ V) (hw : AList.lookup V v = some w)
    (hm : w.mutable = true) :
    ∃ xs, AList.lookup μ w.src = some xs ∧
      vrun (n + 1) μ V out (.set v i x :: rest) =
        if i < w.b - w.a then vrun n (AList.update μ w.src (fun _ => xs.set (w.a + i) x)) V out rest
        else .trap := by
  obtain ⟨xs, hx, h1, h2⟩ := hV v w hw
  refine ⟨xs, hx, ?_⟩
  simp only [vrun, hw, hm, ↓reduceIte]
  by_cases hi : i < w.b - w.a
  · have hj : w.a + i < xs.length := by omega
    simp [elemIdx, hi, hx, hj]
  · simp [elemIdx, hi]

/-- **A write changes exactly one element.** Writing `x` at index `j` of source `p` changes
element `j` of `p` to `x` and no other element of any place. -/
theorem view_write_exact {μ : Mem} {p xs j x} (hx : AList.lookup μ p = some xs) (hj : j < xs.length) :
    ∀ q k, elem (AList.update μ p (fun _ => xs.set j x)) q k = if q = p ∧ k = j then some x else elem μ q k := by
  intro q k
  unfold elem
  rw [AList.lookup_update]
  by_cases hq : q = p
  · subst hq
    rw [hx]
    by_cases hk : k = j
    · subst hk; simp [hj]
    · simp [hk, Ne.symm hk]
  · simp [hq]

/-- A body that only accesses its view changes nothing outside the view's range. -/
theorem viewOnly_frame {V v w} (hw : AList.lookup V v = some w) {body : List VStmt} (hb : ViewOnly v body) :
    ∀ n μ out μ' out', vrun n μ V out body = .ok μ' out' →
      ∀ q k, ¬ (q = w.src ∧ w.a ≤ k ∧ k < w.b) → elem μ' q k = elem μ q k := by
  induction hb with
  | nil =>
    intro n μ out μ' out' h q k _
    cases n with
    | zero => simp [vrun] at h
    | succ n => simp only [vrun, VR.ok.injEq] at h; rw [h.1]
  | @get i rest _ ih =>
    intro n μ out μ' out' h q k hk
    cases n with
    | zero => simp [vrun] at h
    | succ n =>
      simp only [vrun, hw] at h
      split at h
      · cases h
      · split at h
        · exact ih n μ _ μ' out' h q k hk
        · cases h
  | @set i x rest _ ih =>
    intro n μ out μ' out' h q k hk
    cases n with
    | zero => simp [vrun] at h
    | succ n =>
      simp only [vrun, hw] at h
      split at h
      · split at h
        · cases h
        · rename_i j hj
          split at h
          · rename_i xs hx
            split at h
            · rename_i hlt
              rw [ih n _ out μ' out' h q k hk, view_write_exact hx hlt]
              have hr := elemIdx_range hj
              have hne : ¬ (q = w.src ∧ k = j) := fun ⟨h1, h2⟩ => hk ⟨h1, by omega, by omega⟩
              simp [hne]
            · cases h
          · cases h
      · cases h

/-- **Mutation through a view is confined to its range.** A range block without `else` whose
body only accesses its view changes no element outside `[a, b)` of its source. -/
theorem range_block_frame {μ V m p v a b body n out μ' out'} (hb : ViewOnly v body)
    (h : vrun (n + 1) μ V out [.range m p v a b body none] = .ok μ' out') :
    ∀ q k, ¬ (q = p ∧ a ≤ k ∧ k < b) → elem μ' q k = elem μ q k := by
  intro q k hk
  simp only [vrun] at h
  split at h
  · cases h
  · split at h
    · split at h
      · rename_i μ₁ out₁ h₁
        have hμ : μ' = μ₁ := by
          cases n with
          | zero => simp [vrun] at h₁
          | succ n => simp only [vrun, VR.ok.injEq] at h; exact h.1.symm
        subst hμ
        exact viewOnly_frame (w := ⟨p, a, b, m⟩) (by simp [AList.lookup]) hb n μ out _ out₁ h₁ q k hk
      · rename_i hn; exact (hn _ _ h).elim
    · simp at h
    · cases h

/-! ### Text views -/

theorem boundary_iff (bs : List Nat) (i : Nat) :
    boundary bs i = true ↔ i = 0 ∨ i = bs.length ∨ ∃ x, bs[i]? = some x ∧ x / 64 ≠ 2 := by
  unfold boundary
  cases h : bs[i]? <;> simp [or_assoc]

/-- Every byte of a scalar after the first is a continuation byte. -/
theorem isScalar_cont {s : List Nat} (h : isScalar s = true) :
    s ≠ [] ∧ ∀ j c, 0 < j → s[j]? = some c → c / 64 = 2 := by
  match s, h with
  | [x], h => refine ⟨by simp, fun j c hj hc => ?_⟩; rcases j with _ | j <;> simp_all
  | [x, c1], h =>
    refine ⟨by simp, fun j c hj hc => ?_⟩
    simp [isScalar, cont] at h
    rcases j with _ | _ | j <;> simp_all <;> omega
  | [x, c1, c2], h =>
    refine ⟨by simp, fun j c hj hc => ?_⟩
    simp [isScalar, cont] at h
    rcases j with _ | _ | _ | j <;> simp_all <;> omega
  | [x, c1, c2, c3], h =>
    refine ⟨by simp, fun j c hj hc => ?_⟩
    simp [isScalar, cont] at h
    rcases j with _ | _ | _ | _ | j <;> simp_all <;> omega

/-- Well-formed text splits at a scalar boundary into two well-formed halves. -/
theorem utf8_split {bs : List Nat} (h : Utf8 bs) :
    ∀ i, i ≤ bs.length → boundary bs i = true → Utf8 (bs.take i) ∧ Utf8 (bs.drop i) := by
  induction h with
  | nil => intro i _ _; simp [Utf8.nil]
  | @cons s rest hs hr ih =>
    intro i hi hb
    by_cases h0 : i = 0
    · subst h0; exact ⟨by simpa using Utf8.nil, by simpa using Utf8.cons hs hr⟩
    by_cases hge : s.length ≤ i
    · have hb' : boundary rest (i - s.length) = true := by
        rw [boundary_iff] at hb ⊢
        simp only [List.length_append] at hb hi
        rcases hb with hb | hb | ⟨x, hx, hx'⟩
        · exact absurd hb h0
        · exact .inr (.inl (by omega))
        · rw [List.getElem?_append_right hge] at hx
          exact .inr (.inr ⟨x, hx, hx'⟩)
      obtain ⟨h1, h2⟩ := ih (i - s.length) (by simp at hi; omega) hb'
      constructor
      · rw [List.take_append, List.take_of_length_le hge]
        exact .cons hs h1
      · rw [List.drop_append, List.drop_eq_nil_of_le hge, List.nil_append]
        exact h2
    · exfalso
      have hlt : i < s.length := by omega
      obtain ⟨_, hc⟩ := isScalar_cont hs
      rw [boundary_iff] at hb
      simp only [List.length_append] at hb
      rcases hb with hb | hb | ⟨x, hx, hx'⟩
      · exact h0 hb
      · have : 0 < rest.length ∨ rest.length = 0 := by omega
        omega
      · rw [List.getElem?_append_left hlt] at hx
        exact hx' (hc i x (by omega) hx)

/-- **A text view of well-formed text is well formed.** A valid text range (both ends on scalar
boundaries, `a ≤ b ≤ len`) of well-formed UTF-8 is itself well-formed UTF-8. -/
theorem text_view_utf8 {bs : List Nat} {a b : Nat} (h : Utf8 bs)
    (hv : validRange (.text bs) a b = true) : Utf8 (textSlice bs a b) := by
  simp only [validRange, Bool.and_eq_true, decide_eq_true_eq] at hv
  obtain ⟨⟨⟨hab, hbl⟩, ha⟩, hb⟩ := hv
  have hd := (utf8_split h a (by omega) ha).2
  have hb' : boundary (bs.drop a) (b - a) = true := by
    rw [boundary_iff] at hb ⊢
    simp only [List.length_drop]
    rcases hb with hb | hb | ⟨x, hx, hx'⟩
    · exact .inl (by omega)
    · exact .inr (.inl (by omega))
    · refine .inr (.inr ⟨x, ?_, hx'⟩)
      rw [List.getElem?_drop, show a + (b - a) = b by omega]
      exact hx
  exact (utf8_split hd (b - a) (by simp; omega) hb').1

end ConcreteSpec.Borrow
