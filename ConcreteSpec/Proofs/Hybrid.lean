import ConcreteSpec.Spec.Hybrid
import ConcreteSpec.Proofs.Memory

/-!
# Hybrid storage laws (C4)

Scalar representation round trip and decode validity; structured reads; materialization reuses
its stable projection; typed writes read back the value and frame inactive bytes. No theorem
assumes a materialized typed cache, and no write selects padding again.
-/
namespace ConcreteSpec.Core.Hybrid
open ConcreteSpec.Core.Mem

@[simp] theorem encode_length (tgt : Target) (v : Value) :
    (encode tgt v).length = sizeof v.ty := by
  cases v <;> simp [encode, Value.ty, sizeof, encodeBool]

theorem rep_roundtrip (tgt : Target) (v : Value) (hv : v.Valid) :
    Rep tgt v.ty (encode tgt v) v := by
  cases v with
  | int t n => simp [Rep, Value.ty, encode, decode, decodeInt_encodeInt tgt t n hv]
  | bool b => simp [Rep, Value.ty, encode, decode]

theorem decode_valid (tgt : Target) (t : Ty) (bs : List MByte) (v : Value)
    (h : decode tgt t bs = some v) : v.ty = t ∧ v.Valid := by
  cases t with
  | int t =>
    cases hd : decodeInt tgt t bs with
    | none => simp [decode, hd] at h
    | some n =>
      simp [decode, hd] at h
      subst v
      exact ⟨rfl, decodeInt_inRange tgt t bs n hd⟩
  | bool =>
    cases hd : decodeBool bs with
    | none => simp [decode, hd] at h
    | some b => simp [decode, hd] at h; subst v; exact ⟨rfl, trivial⟩

theorem read_structured (tgt : Target) (st : State) (p : Place) (t : Ty) (v : Value)
    (hc : st.places p = some ⟨t, .structured v⟩) (ht : v.ty = t) (hv : v.Valid) :
    st.read tgt p = some v := by simp [State.read, hc, ht, hv]

/-- Once materialized, repeated address-taking returns the same range and the same whole state. -/
theorem materialize_reuse (tgt : Target) (st : State) (p : Place) (t : Ty) (r : Range)
    (hc : st.places p = some ⟨t, .materialized r⟩) (s : AddrSpace) (base align : Nat)
    (padding : List MByte) : st.materialize tgt p s base align padding = some (r, st) := by
  simp [State.materialize, hc]

theorem write_read (tgt : Target) (st st' : State) (hw : st.memory.WellFormed tgt)
    (p : Place) (v : Value) (h : st.write tgt p v = some st') :
    st'.read tgt p = some v := by
  unfold State.write at h
  cases hc : st.places p with
  | none => simp [hc] at h
  | some c =>
    simp only [hc] at h
    dsimp at h
    split at h
    next hv =>
      cases hs : c.storage with
      | structured old =>
        simp only [hs, Option.some.injEq] at h
        subst st'
        simp [State.read, hv]
      | materialized r =>
        simp only [hs] at h
        cases hm : st.memory.store tgt r.ptr (encode tgt v) r.align with
        | none => simp [hm] at h
        | some m =>
          simp [hm] at h
          subst st'
          have hl := store_load tgt st.memory m hw r.ptr (encode tgt v) r.align hm
          rw [encode_length, hv.1] at hl
          simp only [State.read, State.places_memory, hc]
          dsimp
          rw [hs]
          dsimp
          rw [hl]
          dsimp
          rw [← hv.1]
          exact rep_roundtrip tgt v hv.2
    next => simp at h

/-- The low-level store frame theorem applies verbatim to a typed write's active encoding.
In particular every padding byte, including initialization and fragments, is preserved. -/
theorem typed_store_frame (tgt : Target) (m m' : Memory) (hw : m.WellFormed tgt)
    (r : Range) (v : Value) (b : MemBlockId) (off i : Nat) (x : Block)
    (ha : m.access tgt r.ptr (encode tgt v).length r.align = some (some (b, off)))
    (hx : m.block b = some x) (h : m.store tgt r.ptr (encode tgt v) r.align = some m')
    (hi : i < off ∨ off + sizeof v.ty ≤ i) :
    (m'.block b).map (fun y => y.bytes[i]?) = some (x.bytes[i]?) := by
  exact store_same_block_frame tgt m m' hw r.ptr (encode tgt v) r.align b off i x ha hx h
    (by simpa using hi)

/-- Every successful materialization leaves the returned range as the place's sole projection. -/
theorem materialize_projection (tgt : Target) (st st' : State) (p : Place) (s : AddrSpace)
    (base align : Nat) (padding : List MByte) (r : Range)
    (h : st.materialize tgt p s base align padding = some (r, st')) :
    ∃ t, st'.places p = some ⟨t, .materialized r⟩ := by
  unfold State.materialize at h
  cases hc : st.places p with
  | none => simp [hc] at h
  | some c =>
    simp only [hc] at h
    dsimp at h
    cases hs : c.storage with
    | materialized old =>
      simp only [hs, Option.some.injEq, Prod.mk.injEq] at h
      obtain ⟨rfl, rfl⟩ := h
      exact ⟨c.ty, by cases c; simp_all⟩
    | structured v =>
      simp only [hs] at h
      split at h
      next =>
        cases ha : (st.memory.alloc tgt s base (encode tgt v ++ padding).length align).toOption with
        | none => simp only [ha] at h; cases h
        | some pair =>
          obtain ⟨b, m⟩ := pair
          simp only [ha] at h
          cases hm : m.store tgt ⟨s, base, some b⟩ (encode tgt v ++ padding) align with
          | none => simp [hm] at h
          | some m' =>
            simp [hm] at h
            obtain ⟨rfl, rfl⟩ := h
            exact ⟨c.ty, by simp⟩
      next => simp at h

/-- Materialization is idempotent after the first success, independently of later address,
alignment or padding inputs. In particular it never asks for another padding selection. -/
theorem materialize_idempotent (tgt : Target) (st st' : State) (p : Place) (s s' : AddrSpace)
    (base align base' align' : Nat) (padding padding' : List MByte) (r : Range)
    (h : st.materialize tgt p s base align padding = some (r, st')) :
    st'.materialize tgt p s' base' align' padding' = some (r, st') := by
  obtain ⟨t, ht⟩ := materialize_projection tgt st st' p s base align padding r h
  exact materialize_reuse tgt st' p t r ht s' base' align' padding'

/-- Projection validity depends only on block geometry, not on bytes or exposure. -/
theorem projected_transport (m m' : Memory) (c : Cell) (hc : c.Projected m)
    (hg : ∀ b, (m'.block b).map Block.geometry = (m.block b).map Block.geometry) :
    c.Projected m' := by
  cases hs : c.storage with
  | structured v => simp [Cell.Projected, hs]
  | materialized r =>
    simp only [Cell.Projected, hs] at hc ⊢
    obtain ⟨b, x, hp, hx, hspace, hbase, hsize, halign, hfit⟩ := hc
    have hh := hg b
    rw [hx] at hh
    cases hy : m'.block b with
    | none => simp [hy] at hh
    | some y =>
      simp only [hy, Option.map_some, Option.some.injEq, Block.geometry,
        Prod.mk.injEq] at hh
      exact ⟨b, y, hp, hy, hspace.trans hh.1.symm, hbase.trans hh.2.1.symm,
        hsize.trans hh.2.2.1.symm, halign.trans hh.2.2.2.symm, hfit⟩

/-- Replacing bytes or exposure while preserving geometry preserves every cell projection. -/
theorem memory_wellFormed (tgt : Target) (st : State) (m : Memory)
    (hw : st.WellFormed tgt) (hm : m.WellFormed tgt)
    (hg : ∀ b, (m.block b).map Block.geometry = (st.memory.block b).map Block.geometry) :
    ({ st with memory := m } : State).WellFormed tgt := by
  refine ⟨hm, ?_⟩
  intro p c hc
  exact projected_transport _ _ c (hw.projected p c hc) hg

/-- Updating one structured cell preserves the hybrid invariant. -/
theorem set_wellFormed (tgt : Target) (st : State) (p : Place) (c : Cell)
    (hw : st.WellFormed tgt) (hc : c.Projected st.memory) :
    (st.set p c).WellFormed tgt := by
  refine ⟨hw.memory, ?_⟩
  intro q d hd
  simp only [State.places_set] at hd
  split at hd
  · cases hd; exact hc
  · exact hw.projected q d hd

/-- Typed writes preserve the byte-memory invariant and every projection's extent. -/
theorem write_wellFormed (tgt : Target) (st st' : State) (hw : st.WellFormed tgt)
    (p : Place) (v : Value) (h : st.write tgt p v = some st') : st'.WellFormed tgt := by
  unfold State.write at h
  cases hc : st.places p with
  | none => simp [hc] at h
  | some c =>
    simp only [hc] at h
    dsimp at h
    split at h
    next =>
      cases hs : c.storage with
      | structured old =>
        simp only [hs, Option.some.injEq] at h
        subst st'
        exact set_wellFormed tgt st p _ hw trivial
      | materialized r =>
        simp only [hs] at h
        cases hm : st.memory.store tgt r.ptr (encode tgt v) r.align with
        | none => simp [hm] at h
        | some m =>
          simp [hm] at h
          subst st'
          exact memory_wellFormed tgt st m hw
            (store_wellFormed tgt _ _ hw.memory _ _ _ hm)
            (store_geometry tgt _ _ _ _ _ hm)
    next => simp at h

/-- Raw writes likewise preserve geometry without maintaining a second typed copy. -/
theorem rawWrite_wellFormed (tgt : Target) (st st' : State) (hw : st.WellFormed tgt)
    (p : Ptr) (bs : List MByte) (align : Nat)
    (h : st.rawWrite tgt p bs align = some st') : st'.WellFormed tgt := by
  unfold State.rawWrite at h
  cases hm : st.memory.store tgt p bs align with
  | none => simp [hm] at h
  | some m =>
    simp [hm] at h
    subst st'
    exact memory_wellFormed tgt st m hw
      (store_wellFormed tgt _ _ hw.memory _ _ _ hm) (store_geometry tgt _ _ _ _ _ hm)

/-- Successful materialization either reuses the existing cell or allocates and fills one
fresh block. This decomposition keeps the later laws independent of monadic elaboration. -/
theorem materialize_cases (tgt : Target) (st st' : State) (p : Place) (s : AddrSpace)
    (base align : Nat) (padding : List MByte) (r : Range)
    (h : st.materialize tgt p s base align padding = some (r, st')) :
    (∃ c, st.places p = some c ∧ c.storage = .materialized r ∧ st' = st) ∨
    ∃ c v b m m', st.places p = some c ∧ c.storage = .structured v ∧
      v.ty = c.ty ∧ v.Valid ∧
      st.memory.alloc tgt s base (encode tgt v ++ padding).length align = .ok (b, m) ∧
      m.store tgt ⟨s, base, some b⟩ (encode tgt v ++ padding) align = some m' ∧
      r = ⟨⟨s, base, some b⟩, (encode tgt v ++ padding).length, align⟩ ∧
      st' = ({ st with memory := m' }).set p { c with storage := .materialized r } := by
  unfold State.materialize at h
  cases hc : st.places p with
  | none => simp [hc] at h
  | some c =>
    simp only [hc] at h
    dsimp at h
    cases hs : c.storage with
    | materialized old =>
      simp only [hs, Option.some.injEq, Prod.mk.injEq] at h
      obtain ⟨rfl, rfl⟩ := h
      exact .inl ⟨c, rfl, hs, rfl⟩
    | structured v =>
      simp only [hs] at h
      split at h
      next hv =>
        cases ha : st.memory.alloc tgt s base (encode tgt v ++ padding).length align with
        | error e => simp only [ha, Except.toOption, Option.bind_none] at h; cases h
        | ok pair =>
          obtain ⟨b, m⟩ := pair
          simp only [ha, Except.toOption, Option.bind_some] at h
          cases hm : m.store tgt ⟨s, base, some b⟩ (encode tgt v ++ padding) align with
          | none => simp [hm] at h
          | some m' =>
            simp only [hm, Option.bind_some, Option.some.injEq, Prod.mk.injEq] at h
            obtain ⟨rfl, rfl⟩ := h
            exact .inr ⟨c, v, b, m, m', rfl, hs, hv.1, hv.2, ha, hm, rfl, rfl⟩
      next => simp at h

/-- Allocation preserves the geometry of every preexisting projected block. -/
theorem alloc_projected (tgt : Target) (m m' : Memory) (hw : m.WellFormed tgt)
    (c : Cell) (hc : c.Projected m) (s : AddrSpace) (base size align : Nat)
    (b : MemBlockId) (ha : m.alloc tgt s base size align = .ok (b, m')) :
    c.Projected m' := by
  cases hs : c.storage with
  | structured v => simp [Cell.Projected, hs]
  | materialized r =>
    simp only [Cell.Projected, hs] at hc ⊢
    obtain ⟨d, x, hp, hx, rest⟩ := hc
    have hn := alloc_ne_existing tgt m hw s base size align b d m' x ha hx
    obtain ⟨_, rfl, _⟩ := alloc_result tgt m s base size align b m' ha
    refine ⟨d, x, hp, ?_, rest⟩
    simp [Memory.block, AList.lookup, hn, ← hx]

/-- Materialization preserves memory well-formedness and all cell-to-block projections. -/
theorem materialize_wellFormed (tgt : Target) (st st' : State) (hw : st.WellFormed tgt)
    (p : Place) (s : AddrSpace) (base align : Nat) (padding : List MByte) (r : Range)
    (h : st.materialize tgt p s base align padding = some (r, st')) : st'.WellFormed tgt := by
  rcases materialize_cases tgt st st' p s base align padding r h with reused | fresh
  · obtain ⟨_, _, _, rfl⟩ := reused; exact hw
  · obtain ⟨c, v, b, m, m', _, _, ht, _, ha, hm, rfl, rfl⟩ := fresh
    have ham := alloc_wellFormed tgt _ hw.memory s base _ align b m ha
    have halloc : ({ st with memory := m } : State).WellFormed tgt := by
      refine ⟨ham, ?_⟩
      intro q d hd
      exact alloc_projected tgt _ _ hw.memory d (hw.projected q d hd) s base _ align b ha
    have hwritten := memory_wellFormed tgt _ m' halloc
      (store_wellFormed tgt m m' ham _ _ _ hm) (store_geometry tgt _ _ _ _ _ hm)
    apply set_wellFormed tgt _ p _ hwritten
    apply projected_transport m m' _ _ (store_geometry tgt _ _ _ _ _ hm)
    simp only [Cell.Projected]
    refine ⟨b, candidate s base (encode tgt v ++ padding).length align, rfl,
      alloc_block tgt _ s base _ align b m ha, rfl, rfl, rfl, rfl, ?_⟩
    simp [List.length_append, encode_length, ht]

/-- Exposing a typed place preserves both layers of the invariant. -/
theorem expose_wellFormed (tgt : Target) (st st' : State) (hw : st.WellFormed tgt)
    (p : Place) (s : AddrSpace) (base align : Nat) (padding : List MByte) (r : Range)
    (h : st.expose tgt p s base align padding = some (r, st')) : st'.WellFormed tgt := by
  unfold State.expose at h
  cases hm : st.materialize tgt p s base align padding with
  | none => simp [hm] at h
  | some pair =>
    obtain ⟨r', mid⟩ := pair
    simp only [hm] at h
    dsimp at h
    cases he : mid.memory.expose r'.ptr with
    | none => simp [he] at h
    | some m =>
      simp [he] at h
      obtain ⟨rfl, rfl⟩ := h
      have hmid := materialize_wellFormed tgt st mid hw p s base align padding r' hm
      exact memory_wellFormed tgt mid m hmid
        (Mem.expose_wellFormed tgt _ _ hmid.memory _ he) (expose_geometry _ _ _ he)

end ConcreteSpec.Core.Hybrid
