import ConcreteSpec.Proofs.Memory
import ConcreteSpec.Spec.Basics
import ConcreteSpec.Spec.Hybrid
import ConcreteSpec.Spec.Ident
import ConcreteSpec.Spec.Memory
import ConcreteSpec.Spec.Rep
import ConcreteSpec.Spec.Scalar
import ConcreteSpec.Spec.Slice1Syntax
import ConcreteSpec.Spec.Target

/-!
# Hybrid storage laws (C4)

Integer representation round trip and decode validity; structured reads; materialization reuses
its stable projection; typed writes read back the value and frame inactive bytes. No theorem
assumes a materialized typed cache, and no write selects padding again. The memory and
projection invariants survive every state operation. First materialization preserves the
read value. Typed writes frame disjoint byte intervals; materialization and exposure frame
every other place without an extra separation premise.

Authority for all laws below: docs/decisions.md#foundation-q2, approving C4's hybrid-state,
stable-materialization, active-byte write, padding-frame and current-byte read contracts;
#decided-2026-11-05-core Q3.11 for representation, Q3.7 for valid initialized access,
Q3.9 for exposure, and #foundation-q3 / #od126 for integer width and target byte order.
Classification follows #decided-2026-11-13-proof-labels; only Law declarations count as evidence.
Representation adapters compare existing policies and do not introduce a language choice.

Extraction disposition: the complete Spec/Hybrid and Proofs/Hybrid command populations from
integration-head aa44140a3 are adapted here. Shared memory, scalar and execution definitions
are imported. Boolean encoding/decoding and their dependent examples are owner-held under
ISS-0069; controls instead exercise the independent u8 rule. Character and unit byte layout
are unsupported rather than inferred from their scalar constructors. The source had no admitted
struct or pointer cell codec; neither is added (ISS-0070). Inherited approval receipts are retired.
The source's broad invariant mutants are replaced with allocation-counter-only mutations;
raw-write/read correspondence and agreement with existing Spec/Rep are added explicitly.
-/
namespace ConcreteSpec.Core.Hybrid
open ConcreteSpec.Core.Mem

/-- Representation lemma. Scalar encoding produces sizeof v.ty bytes. -/
@[simp] theorem encode_length (tgt : Target) (v : Value) :
    (encode tgt v).length = sizeof v.ty := by
  cases v <;> simp [encode, Value.ty, CVal.ty, sizeof]

/-- Helper. The two byte containers enumerate the same little-endian octets. -/
private theorem slice_leBytes (w n : Nat) :
    ((leBytes w n).map (fun b => MByte.init b none)).map sliceByte =
      ConcreteSpec.Rep.encodeNat w n := by
  induction w generalizing n with
  | zero => rfl
  | succ w ih => simp [leBytes, ConcreteSpec.Rep.encodeNat, sliceByte, ih]

/-- Representation lemma. On Spec/Rep's fixed profile, integer encoding is the existing
representation under the byte-container adapter; the hybrid adds no second encoding policy. -/
theorem encode_agrees_slice (t : IntTy) (n : Int) :
    (encode linuxX8664 (.int t n)).map sliceByte =
      ConcreteSpec.Rep.encodeScalar (.int (sliceIntTy t) n) := by
  cases t <;> exact slice_leBytes _ _

/-- Helper. Initialized octets and uninitialized bytes decode identically in both containers. -/
private theorem slice_decodeNat (bs : List MByte) :
    ConcreteSpec.Rep.decodeNat (bs.map sliceByte) = (octets bs).map leValue := by
  induction bs with
  | nil => rfl
  | cons b bs ih =>
    cases b with
    | uninit => rfl
    | init b frag =>
      simp only [List.map_cons, sliceByte, ConcreteSpec.Rep.decodeNat, octets]
      rw [ih]
      cases octets bs <;> rfl

/-- Representation lemma. Integer decoding, including invalid lengths, uninitialized bytes
and fragment octets, agrees with Spec/Rep on its fixed little-endian profile. -/
theorem decode_agrees_slice (t : IntTy) (bs : List MByte) :
    ConcreteSpec.Rep.decodeScalar (.int (sliceIntTy t)) (bs.map sliceByte) =
      (decodeInt linuxX8664 t bs).map (ConcreteSpec.Scalar.int (sliceIntTy t)) := by
  cases t <;>
    simp only [ConcreteSpec.Rep.decodeScalar, sliceIntTy, ConcreteSpec.IntTy.bits,
      List.length_map, decodeInt, intBytes, IntTy.bits, slice_decodeNat]
  all_goals split <;> simp_all [ConcreteSpec.Rep.ofBits, ofBits, ordered, linuxX8664,
    ConcreteSpec.IntTy.signed, IntTy.signed, ConcreteSpec.IntTy.bits, IntTy.bits, Option.map_map, Function.comp_def]

/-- Law (authority: docs/decisions.md#foundation-q2; Q3.11, Q3.7; integer representation also #foundation-q3 and #od126).
(subject `ConcreteSpec.Core.Hybrid.Rep`, `ConcreteSpec.Core.Hybrid.encode`). Every storable scalar value round-trips through its byte representation. -/
theorem rep_roundtrip (tgt : Target) (v : Value) (hv : v.Storable) :
    Rep tgt v.ty (encode tgt v) v := by
  cases v with
  | int t n => simp [Rep, Value.ty, CVal.ty, encode, decode, decodeInt_encodeInt tgt t n hv.1]
  | bool b => exact False.elim (hv.2.2.1 rfl)
  | unit => exact False.elim (hv.2.1 rfl)
  | char c => exact False.elim (hv.2.2.2 rfl)

/-- Law (authority: docs/decisions.md#foundation-q2; Q3.11, Q3.7; integer representation also #foundation-q3 and #od126).
(subject `ConcreteSpec.Core.Hybrid.decode`). Every decoded scalar has its requested type and is storable. -/
theorem decode_valid (tgt : Target) (t : Ty) (bs : List MByte) :
    match decode tgt t bs with
    | some v => v.ty = t ∧ v.Storable
    | none => True := by
  cases h : decode tgt t bs with
  | none => trivial
  | some v =>
    cases t with
    | int t =>
      cases hd : decodeInt tgt t bs with
      | none => simp [decode, hd] at h
      | some n =>
        simp [decode, hd] at h
        subst v
        have hr := decodeInt_inRange tgt t bs
        rw [hd] at hr
        exact ⟨rfl, hr, by simp [Value.ty, CVal.ty], by simp [Value.ty, CVal.ty], by simp [Value.ty, CVal.ty]⟩
    | bool => cases h
    | unit => cases h
    | char => cases h

/-- Law (authority: docs/decisions.md#foundation-q2; Q3.11, Q3.7; integer representation also #foundation-q3 and #od126).
(subject `ConcreteSpec.Core.Hybrid.State.read`). Reading a structured cell with a well-typed storable value returns that value. -/
theorem read_structured (tgt : Target) (st : State) (p : Place) (t : Ty) (v : Value)
    (hc : st.places p = some ⟨t, .structured v⟩) (ht : v.ty = t) (hv : v.Storable) :
    st.read tgt p = some v := by simp [State.read, hc, ht, hv]

/-- Law (authority: docs/decisions.md#foundation-q2; Q3.11, Q3.7; integer representation also #foundation-q3 and #od126).
(subject `ConcreteSpec.Core.Hybrid.State.materialize`). Once materialized, repeated address-taking returns the same range and the same whole state. -/
theorem materialize_reuse (tgt : Target) (st : State) (p : Place) (t : Ty) (r : Range)
    (hc : st.places p = some ⟨t, .materialized r⟩) (s : AddrSpace) (base align : Nat)
    (padding : List MByte) : st.materialize tgt p s base align padding = some (r, st) := by
  simp [State.materialize, hc]

/-- Law (authority: docs/decisions.md#foundation-q2; Q3.11, Q3.7; integer representation also #foundation-q3 and #od126).
(subject `ConcreteSpec.Core.Hybrid.State.read`). Reading back a place after a successful typed write recovers the written value. -/
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

/-- Law (authority: docs/decisions.md#foundation-q2; Q3.11, Q3.7; integer representation also #foundation-q3 and #od126).
(subject `ConcreteSpec.Core.Hybrid.State.write`). A typed State.write preserves every byte outside the active scalar representation,
including padding, initialization and pointer fragments. -/
theorem typed_store_frame (tgt : Target) (st : State) (hw : st.WellFormed tgt)
    (p : Place) (t : Ty) (r : Range) (v : Value) (b : BlockId) (off i : Nat) (x : Block)
    (hc : st.places p = some ⟨t, .materialized r⟩)
    (ha : st.memory.access tgt r.ptr (sizeof t) r.align = some (some (b, off)))
    (hx : st.memory.block b = some x)
    (hi : i < off ∨ off + sizeof t ≤ i) :
    match st.write tgt p v with
    | some st' => (st'.memory.block b).map (fun y => y.bytes[i]?) = some (x.bytes[i]?)
    | none => True := by
  cases h : st.write tgt p v with
  | none => trivial
  | some st' =>
    simp only [State.write, hc] at h
    dsimp at h
    split at h
    next hv =>
      cases hm : st.memory.store tgt r.ptr (encode tgt v) r.align with
      | none => simp [hm] at h
      | some m =>
        simp [hm] at h
        subst st'
        have hsf := store_same_block_frame tgt st.memory hw.memory r.ptr (encode tgt v)
          r.align b off i x (by simpa [encode_length, hv.1] using ha) hx
          (by simpa [encode_length, hv.1] using hi)
        simpa [hm] using hsf
    next => simp at h

/-- Law (authority: docs/decisions.md#foundation-q2; Q3.11, Q3.7; integer representation also #foundation-q3 and #od126).
(subject `ConcreteSpec.Core.Hybrid.State.materialize`). Every successful materialization leaves the returned range as the place's sole projection. -/
theorem materialize_projection (tgt : Target) (st : State) (p : Place) (s : AddrSpace)
    (base align : Nat) (padding : List MByte) :
    match st.materialize tgt p s base align padding with
    | some (r, st') => ∃ t, st'.places p = some ⟨t, .materialized r⟩
    | none => True := by
  cases h : st.materialize tgt p s base align padding with
  | none => trivial
  | some pair =>
    obtain ⟨r, st'⟩ := pair
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

/-- Law (authority: docs/decisions.md#foundation-q2; Q3.11, Q3.7; integer representation also #foundation-q3 and #od126).
(subject `ConcreteSpec.Core.Hybrid.State.materialize`). Materialization is idempotent after the first success, independently of later address,
alignment or padding inputs. In particular it never asks for another padding selection. -/
theorem materialize_idempotent (tgt : Target) (st st' : State) (p : Place) (s s' : AddrSpace)
    (base align base' align' : Nat) (padding padding' : List MByte) (r : Range)
    (h : st.materialize tgt p s base align padding = some (r, st')) :
    st'.materialize tgt p s' base' align' padding' = some (r, st') := by
  have hp := materialize_projection tgt st p s base align padding
  rw [h] at hp
  obtain ⟨t, ht⟩ := hp
  exact materialize_reuse tgt st' p t r ht s' base' align' padding'

/-- Helper (congruence). Projection validity depends only on block geometry, not on bytes or exposure. -/
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

/-- Helper (congruence). Replacing bytes or exposure while preserving geometry preserves every cell projection. -/
theorem memory_wellFormed (tgt : Target) (st : State) (m : Memory)
    (hw : st.WellFormed tgt) (hm : m.WellFormed tgt)
    (hg : ∀ b, (m.block b).map Block.geometry = (st.memory.block b).map Block.geometry) :
    ({ st with memory := m } : State).WellFormed tgt := by
  refine ⟨hm, ?_⟩
  intro p c hc
  exact projected_transport _ _ c (hw.projected p c hc) hg

/-- Helper (congruence). Updating one structured cell preserves the hybrid invariant. -/
theorem set_wellFormed (tgt : Target) (st : State) (p : Place) (c : Cell)
    (hw : st.WellFormed tgt) (hc : c.Projected st.memory) :
    (st.set p c).WellFormed tgt := by
  refine ⟨hw.memory, ?_⟩
  intro q d hd
  simp only [State.places_set] at hd
  split at hd
  · cases hd; exact hc
  · exact hw.projected q d hd

/-- Law (authority: docs/decisions.md#foundation-q2; Q3.11, Q3.7; integer representation also #foundation-q3 and #od126).
(subject `ConcreteSpec.Core.Hybrid.State.write`). Typed writes preserve the byte-memory invariant and every projection's extent. -/
theorem write_wellFormed (tgt : Target) (st : State) (hw : st.WellFormed tgt)
    (p : Place) (v : Value) :
    match st.write tgt p v with
    | some st' => st'.WellFormed tgt
    | none => True := by
  cases h : st.write tgt p v with
  | none => trivial
  | some st' =>
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
            have hsw := store_wellFormed tgt st.memory hw.memory r.ptr (encode tgt v) r.align
            rw [hm] at hsw
            exact memory_wellFormed tgt st m hw hsw (store_geometry_inv tgt _ _ _ _ _ hm)
      next => simp at h

/-- Law (authority: docs/decisions.md#foundation-q2; Q3.11, Q3.7; integer representation also #foundation-q3 and #od126).
(subject `ConcreteSpec.Core.Hybrid.State.rawWrite`). Raw writes likewise preserve geometry without maintaining a second typed copy. -/
theorem rawWrite_wellFormed (tgt : Target) (st : State) (hw : st.WellFormed tgt)
    (p : Ptr) (bs : List MByte) (align : Nat) :
    match st.rawWrite tgt p bs align with
    | some st' => st'.WellFormed tgt
    | none => True := by
  cases h : st.rawWrite tgt p bs align with
  | none => trivial
  | some st' =>
    unfold State.rawWrite at h
    cases hm : st.memory.store tgt p bs align with
    | none => simp [hm] at h
    | some m =>
      simp [hm] at h
      subst st'
      have hsw := store_wellFormed tgt st.memory hw.memory p bs align
      rw [hm] at hsw
      exact memory_wellFormed tgt st m hw hsw (store_geometry_inv tgt _ _ _ _ _ hm)

/-- Law (authority: docs/decisions.md#foundation-q2, C4 current-byte reads).
An authorized whole-active-range raw write determines the next typed read, including decode
failure. There is no stale structured fallback after materialization. -/
theorem rawWrite_read (tgt : Target) (st st' : State) (hw : st.memory.WellFormed tgt)
    (p : Place) (t : Ty) (r : Range) (bs : List MByte)
    (hc : st.places p = some ⟨t, .materialized r⟩) (hlen : bs.length = sizeof t)
    (h : st.rawWrite tgt r.ptr bs r.align = some st') :
    st'.read tgt p = decode tgt t bs := by
  unfold State.rawWrite at h
  cases hm : st.memory.store tgt r.ptr bs r.align with
  | none => simp [hm] at h
  | some m =>
    simp [hm] at h
    subst st'
    have hl := store_load tgt st.memory m hw r.ptr bs r.align hm
    rw [hlen] at hl
    simp [State.read, hc, hl]

/-- Helper (inversion). Successful materialization either reuses the existing cell or allocates and fills one
fresh block. This decomposition keeps the later laws independent of monadic elaboration. -/
theorem materialize_cases (tgt : Target) (st st' : State) (p : Place) (s : AddrSpace)
    (base align : Nat) (padding : List MByte) (r : Range)
    (h : st.materialize tgt p s base align padding = some (r, st')) :
    (∃ c, st.places p = some c ∧ c.storage = .materialized r ∧ st' = st) ∨
    ∃ c v b m m', st.places p = some c ∧ c.storage = .structured v ∧
      v.ty = c.ty ∧ v.Storable ∧
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

/-- Helper (congruence). Allocation preserves the geometry of every preexisting projected block. -/
theorem alloc_projected (tgt : Target) (m m' : Memory) (hw : m.WellFormed tgt)
    (c : Cell) (hc : c.Projected m) (s : AddrSpace) (base size align : Nat)
    (b : BlockId) (ha : m.alloc tgt s base size align = .ok (b, m')) :
    c.Projected m' := by
  cases hs : c.storage with
  | structured v => simp [Cell.Projected, hs]
  | materialized r =>
    simp only [Cell.Projected, hs] at hc ⊢
    obtain ⟨d, x, hp, hx, rest⟩ := hc
    have hn := alloc_ne_existing tgt m hw s base size align d x hx
    rw [ha] at hn
    obtain ⟨_, rfl, _⟩ := alloc_result tgt m s base size align b m' ha
    refine ⟨d, x, hp, ?_, rest⟩
    simp [Memory.block, AList.lookup, hn, ← hx]

/-- Law (authority: docs/decisions.md#foundation-q2; Q3.11, Q3.7; integer representation also #foundation-q3 and #od126).
(subject `ConcreteSpec.Core.Hybrid.State.materialize`). Materialization preserves memory well-formedness and all cell-to-block projections. -/
theorem materialize_wellFormed (tgt : Target) (st : State) (hw : st.WellFormed tgt)
    (p : Place) (s : AddrSpace) (base align : Nat) (padding : List MByte) :
    match st.materialize tgt p s base align padding with
    | some (_, st') => st'.WellFormed tgt
    | none => True := by
  cases h : st.materialize tgt p s base align padding with
  | none => trivial
  | some pair =>
    obtain ⟨r, st'⟩ := pair
    rcases materialize_cases tgt st st' p s base align padding r h with reused | fresh
    · obtain ⟨_, _, _, rfl⟩ := reused; exact hw
    · obtain ⟨c, v, b, m, m', _, _, ht, _, ha, hm, rfl, rfl⟩ := fresh
      have ham := alloc_wellFormed tgt st.memory hw.memory s base (encode tgt v ++ padding).length align
      rw [ha] at ham
      have halloc : ({ st with memory := m } : State).WellFormed tgt := by
        refine ⟨ham, ?_⟩
        intro q d hd
        exact alloc_projected tgt _ _ hw.memory d (hw.projected q d hd) s base _ align b ha
      have hsw := store_wellFormed tgt m ham ⟨s, base, some b⟩ (encode tgt v ++ padding) align
      rw [hm] at hsw
      have hwritten := memory_wellFormed tgt _ m' halloc hsw (store_geometry_inv tgt _ _ _ _ _ hm)
      apply set_wellFormed tgt _ p _ hwritten
      apply projected_transport m m' _ _ (store_geometry_inv tgt _ _ _ _ _ hm)
      simp only [Cell.Projected]
      have hab := alloc_block tgt st.memory s base (encode tgt v ++ padding).length align
      rw [ha] at hab
      refine ⟨b, candidate s base (encode tgt v ++ padding).length align, rfl,
        hab, rfl, rfl, rfl, rfl, ?_⟩
      simp [List.length_append, encode_length, ht]

/-- Law (authority: docs/decisions.md#foundation-q2; Q3.11, Q3.7; integer representation also #foundation-q3 and #od126).
(subject `ConcreteSpec.Core.Hybrid.State.expose`). Exposing a typed place preserves both layers of the invariant. -/
theorem expose_wellFormed (tgt : Target) (st : State) (hw : st.WellFormed tgt)
    (p : Place) (s : AddrSpace) (base align : Nat) (padding : List MByte) :
    match st.expose tgt p s base align padding with
    | some (_, st') => st'.WellFormed tgt
    | none => True := by
  cases h : st.expose tgt p s base align padding with
  | none => trivial
  | some pair =>
    obtain ⟨r, st'⟩ := pair
    unfold State.expose at h
    cases hm : st.materialize tgt p s base align padding with
    | none => simp [hm] at h
    | some pair' =>
      obtain ⟨r', mid⟩ := pair'
      simp only [hm] at h
      dsimp at h
      cases he : mid.memory.expose r'.ptr with
      | none => simp [he] at h
      | some m =>
        simp [he] at h
        obtain ⟨rfl, rfl⟩ := h
        have hmid := materialize_wellFormed tgt st hw p s base align padding
        rw [hm] at hmid
        have hew := Mem.expose_wellFormed tgt mid.memory hmid.memory r'.ptr
        rw [he] at hew
        exact memory_wellFormed tgt mid m hmid hew (expose_geometry_inv _ _ _ he)

/-- Law (authority: docs/decisions.md#foundation-q2; Q3.11, Q3.7; integer representation also #foundation-q3 and #od126).
(subject `ConcreteSpec.Core.Hybrid.State.read`). Typed writes frame every other place with a distinct materialized allocation identity. -/
theorem write_frame (tgt : Target) (st st' : State) (p q : Place) (v : Value)
    (hne : q ≠ p) (hsep : st.Separated p q) (h : st.write tgt p v = some st') :
    st'.read tgt q = st.read tgt q := by
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
        simp [State.read, Ne.symm hne]
      | materialized r =>
        simp only [hs] at h
        cases hm : st.memory.store tgt r.ptr (encode tgt v) r.align with
        | none => simp [hm] at h
        | some m =>
          simp [hm] at h
          subst st'
          cases hq : st.places q with
          | none => simp [State.read, hq]
          | some d =>
            cases hd : d.storage with
            | structured w => simp [State.read, hq, hd]
            | materialized s =>
              have hh := store_load_other tgt st.memory m r.ptr s.ptr (encode tgt v)
                r.align (sizeof d.ty) s.align hm (Ne.symm (hsep c d r s hc hq hs hd))
              simp [State.read, hq, hd, hh]
    next => simp at h

/-- Helper (inversion). In the whole-cell model, disjoint byte ranges cannot share an allocation identity when
the written cell has a supported (therefore nonempty) scalar representation. -/
theorem rangesDisjoint_separated (tgt : Target) (st : State) (hw : st.WellFormed tgt)
    (p q : Place) (hd : st.RangesDisjoint p q)
    (ht : ∀ c, st.places p = some c → c.ty ≠ .unit ∧ c.ty ≠ .bool ∧ c.ty ≠ .char) : st.Separated p q := by
  intro c d r s hc hq hcr hds he
  have hr := hw.projected p c hc
  have hs := hw.projected q d hq
  simp only [Cell.Projected, hcr] at hr
  simp only [Cell.Projected, hds] at hs
  obtain ⟨b, x, hpb, hbx, hrspace, hrbase, hrsize, _, hrfit⟩ := hr
  obtain ⟨b', y, hpb', hby, hsspace, hsbase, hssize, _⟩ := hs
  have hbb : b = b' := by rw [hpb, hpb'] at he; exact Option.some.inj he
  subst b'
  rw [hbx] at hby
  cases hby
  have hpos : 0 < sizeof c.ty := by
    have hct := ht c hc
    cases hc' : c.ty with
    | int t => cases t <;> decide
    | bool => exact False.elim (hct.2.1 hc')
    | unit => exact False.elim (hct.1 hc')
    | char => exact False.elim (hct.2.2 hc')
  rcases hd c d r s hc hq hcr hds with hempty | hempty | hspace | hbefore | hafter
  · omega
  · omega
  · exact hspace (hrspace.trans hsspace.symm)
  · omega
  · omega

/-- Law (authority: docs/decisions.md#foundation-q2; Q3.11, Q3.7; integer representation also #foundation-q3 and #od126).
(subject `ConcreteSpec.Core.Hybrid.State.read`). The requested byte-range frame law: a successful typed write leaves every other place's
read unchanged when its materialized interval is disjoint from the written place's interval. -/
theorem write_frame_disjoint (tgt : Target) (st st' : State) (hw : st.WellFormed tgt)
    (p q : Place) (v : Value) (hne : q ≠ p) (hd : st.RangesDisjoint p q)
    (h : st.write tgt p v = some st') : st'.read tgt q = st.read tgt q := by
  have ht : ∀ c, st.places p = some c → c.ty ≠ .unit ∧ c.ty ≠ .bool ∧ c.ty ≠ .char := by
    intro c hc
    have hh := h
    simp only [State.write, hc] at hh
    dsimp at hh
    split at hh
    next hv => rw [← hv.1]; exact hv.2.2
    next => simp at hh
  exact write_frame tgt st st' p q v hne (rangesDisjoint_separated tgt st hw p q hd ht) h

/-- Law (authority: docs/decisions.md#foundation-q2; Q3.11, Q3.7; integer representation also #foundation-q3 and #od126).
(subject `ConcreteSpec.Core.Hybrid.State.read`). First materialization frames all other places; freshness supplies block separation, so no
additional separation premise is necessary. -/
theorem materialize_frame (tgt : Target) (st st' : State) (hw : st.WellFormed tgt)
    (p q : Place) (hne : q ≠ p) (space : AddrSpace) (base align : Nat)
    (padding : List MByte) (r : Range)
    (h : st.materialize tgt p space base align padding = some (r, st')) :
    st'.read tgt q = st.read tgt q := by
  rcases materialize_cases tgt st st' p space base align padding r h with reused | fresh
  · obtain ⟨_, _, _, rfl⟩ := reused; rfl
  · obtain ⟨c, v, b, m, m', _, _, _, _, ha, hm, rfl, rfl⟩ := fresh
    cases hq : st.places q with
    | none => simp [State.read, Ne.symm hne, hq]
    | some d =>
      cases hd : d.storage with
      | structured w => simp [State.read, Ne.symm hne, hq, hd]
      | materialized s =>
        have hproj := hw.projected q d hq
        simp only [Cell.Projected, hd] at hproj
        obtain ⟨old, x, hp, hx, _⟩ := hproj
        have hbo := alloc_ne_existing tgt st.memory hw.memory space base (encode tgt v ++ padding).length align old x hx
        rw [ha] at hbo
        have hload : m.load tgt s.ptr (sizeof d.ty) s.align =
            st.memory.load tgt s.ptr (sizeof d.ty) s.align := by
          apply load_congr
          intro old' hp'
          have he : old' = old := by rw [hp] at hp'; exact (Option.some.inj hp').symm
          subst old'
          obtain ⟨_, rfl, _⟩ := alloc_result tgt _ space base _ align b m ha
          simp [Memory.block, AList.lookup, hbo]
        have hstore := store_load_other tgt m m' ⟨space, base, some b⟩ s.ptr
          (encode tgt v ++ padding) align (sizeof d.ty) s.align hm
          (by simpa only [hp, ne_eq, Option.some.injEq] using Ne.symm hbo)
        simp [State.read, Ne.symm hne, hq, hd, hstore, hload]

/-- Law (authority: docs/decisions.md#foundation-q2; Q3.11, Q3.7; integer representation also #foundation-q3 and #od126).
(subject `ConcreteSpec.Core.Hybrid.State.read`). Exposure frames other places, including all byte decodes; it changes only reconstruction. -/
theorem expose_frame (tgt : Target) (st st' : State) (hw : st.WellFormed tgt)
    (p q : Place) (hne : q ≠ p) (space : AddrSpace) (base align : Nat)
    (padding : List MByte) (r : Range)
    (h : st.expose tgt p space base align padding = some (r, st')) :
    st'.read tgt q = st.read tgt q := by
  unfold State.expose at h
  cases hm : st.materialize tgt p space base align padding with
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
      rw [← materialize_frame tgt st mid hw p q hne space base align padding r' hm]
      cases hq : mid.places q with
      | none => simp [State.read, hq]
      | some d =>
        cases hd : d.storage with
        | structured v => simp [State.read, hq, hd]
        | materialized s =>
          simp [State.read, hq, hd, expose_load tgt _ _ r'.ptr s.ptr (sizeof d.ty) s.align he]

/-- Law (authority: docs/decisions.md#foundation-q2; Q3.11, Q3.7; integer representation also #foundation-q3 and #od126).
(subject `ConcreteSpec.Core.Hybrid.State.read`). Materialization preserves the value read from a place, including its first allocation. -/
theorem materialize_read (tgt : Target) (st st' : State) (hw : st.memory.WellFormed tgt)
    (p : Place) (v : Value) (space : AddrSpace) (base align : Nat)
    (padding : List MByte) (r : Range) (hr : st.read tgt p = some v)
    (h : st.materialize tgt p space base align padding = some (r, st')) :
    st'.read tgt p = some v := by
  rcases materialize_cases tgt st st' p space base align padding r h with reused | fresh
  · obtain ⟨_, _, _, rfl⟩ := reused; exact hr
  · obtain ⟨c, w, b, m, m', hc, hs, ht, hv, ha, hm, rfl, rfl⟩ := fresh
    have hvw : w = v := by
      simpa [State.read, hc, hs, ht, hv] using hr
    subst w
    have ham := alloc_wellFormed tgt st.memory hw space base (encode tgt v ++ padding).length align
    rw [ha] at ham
    have hl := store_load tgt m m' ham ⟨space, base, some b⟩ (encode tgt v ++ padding) align hm
    have hpos : 0 < sizeof v.ty := by
      cases v with
      | int t n => cases t <;> simp [Value.ty, CVal.ty, sizeof, intBytes, IntTy.bits]
      | bool b => exact False.elim (hv.2.2.1 rfl)
      | unit => exact False.elim (hv.2.1 rfl)
      | char c => exact False.elim (hv.2.2.2 rfl)
    have hshort := load_prefix tgt m' ⟨space, base, some b⟩
      (encode tgt v ++ padding).length (sizeof v.ty) align (encode tgt v ++ padding)
      (by simp [encode_length]; omega) (by omega) (by simp) hl
    have htake : (encode tgt v ++ padding).take (sizeof v.ty) = encode tgt v := by
      rw [← encode_length tgt v]; simp
    rw [htake] at hshort
    simpa [State.read, hshort, ← ht, Rep] using (rep_roundtrip tgt v hv)


namespace Audit

def range : Range := ⟨⟨⟨0⟩, 40, some ⟨0⟩⟩, 3, 1⟩
def memory : Memory := ⟨[(⟨0⟩, candidate ⟨0⟩ 40 3 1)], 1⟩
def before : State := ⟨[(⟨0⟩, ⟨(.int .u8), .materialized range⟩)], memory⟩
def after : State :=
  { before with memory := memory.modify ⟨0⟩ fun x =>
      { x with bytes := [.init 1 none, .uninit, .uninit] } }
def exposedAfter : State :=
  { before with memory := memory.modify ⟨0⟩ fun x =>
      { x with exposed := true } }

def structState : State := ⟨[(⟨0⟩, ⟨(.int .u8), .structured (.int .u8 1)⟩)], Memory.empty⟩
def matRange : Range := ⟨⟨⟨0⟩, 40, some ⟨0⟩⟩, 1, 1⟩
def matBlock : Block := { candidate ⟨0⟩ 40 1 1 with bytes := [.init 1 none] }
def matMemory : Memory := ⟨[(⟨0⟩, matBlock)], 1⟩
def matState : State := ({ structState with memory := matMemory }).set ⟨0⟩ ⟨(.int .u8), .materialized matRange⟩
def matExposedBlock : Block := { matBlock with exposed := true }
def matExposedState : State :=
  ⟨[(⟨0⟩, ⟨(.int .u8), .materialized matRange⟩), (⟨0⟩, ⟨(.int .u8), .structured (.int .u8 1)⟩)],
    ⟨[(⟨0⟩, matExposedBlock)], 1⟩⟩

/-- Mutate only the allocation counter; bytes, projections and all other geometry are retained. -/
def forgetFreshness (st : State) : State :=
  { st with memory := { st.memory with next := 0 } }

def badRep : Target → Ty → List MByte → Value → Prop := fun _ _ _ _ => False
def badEncode : Target → Value → List MByte := fun _ _ => []
def badDecodeUnit : Target → Ty → List MByte → Option Value := fun _ _ _ => some .unit
def badReadNone : Target → State → Place → Option Value := fun _ _ _ => none
def badReadStructOnly : Target → State → Place → Option Value :=
  fun tgt st p => if st.memory.next == 0 then st.read tgt p else none
def badMaterializeNone : Target → State → Place → AddrSpace → Nat → Nat → List MByte →
    Option (Range × State) := fun _ _ _ _ _ _ _ => none
def badMaterializeEmpty : Target → State → Place → AddrSpace → Nat → Nat → List MByte →
    Option (Range × State) :=
  fun tgt st p s base align pad =>
    (st.materialize tgt p s base align pad).map fun (r, out) =>
      (r, { out with cells := out.cells.filter (fun e => e.1 != p) })
def badMaterializeAddressGate : Target → State → Place → AddrSpace → Nat → Nat → List MByte →
    Option (Range × State) :=
  fun tgt st p s base align pad => if base == 40 then st.materialize tgt p s base align pad else none
def badMaterializeWf : Target → State → Place → AddrSpace → Nat → Nat → List MByte →
    Option (Range × State) :=
  fun tgt st p s base align pad =>
    (st.materialize tgt p s base align pad).map fun (r, out) => (r, forgetFreshness out)
def badWriteClobber : Target → State → Place → Value → Option State :=
  fun _ _ _ _ =>
    some { after with memory := after.memory.modify ⟨0⟩ fun x =>
      { x with bytes := [.init 1 none, .init 0 none, .uninit] } }
def badWriteWf : Target → State → Place → Value → Option State := fun tgt st p v => (st.write tgt p v).map forgetFreshness
def badRawWriteWf : Target → State → Ptr → List MByte → Nat → Option State :=
  fun tgt st p bs a => (st.rawWrite tgt p bs a).map forgetFreshness
def badExposeStateWf : Target → State → Place → AddrSpace → Nat → Nat → List MByte →
    Option (Range × State) :=
  fun tgt st p s base align pad =>
    (st.expose tgt p s base align pad).map fun (r, out) => (r, forgetFreshness out)

/-- Helper. A stored block cannot have an identity below a reset allocation counter. -/
private theorem not_reset_wf (st : State) (x : Block)
    (hx : st.memory.block ⟨0⟩ = some x) (hn : st.memory.next = 0) :
    ¬ st.WellFormed linuxX8664 := by
  intro hw
  have h := hw.memory.fresh ⟨0⟩ x hx
  rw [hn] at h
  exact Nat.not_lt_zero _ h

/-- Helper (congruence). The materialized audit fixture state satisfies the hybrid invariant. -/
private theorem before_wf : before.WellFormed linuxX8664 := by
  refine ⟨alloc_wellFormed linuxX8664 Memory.empty (empty_wellFormed linuxX8664)
    ⟨0⟩ 40 3 1, ?_⟩
  intro p c hc
  simp only [State.places, before, AList.lookup] at hc
  split at hc
  next =>
    cases hc
    exact ⟨⟨0⟩, candidate ⟨0⟩ 40 3 1, rfl, rfl, rfl, rfl, rfl, rfl, by decide⟩
  next => cases hc

/-- Helper (congruence). The structured audit fixture state satisfies the hybrid invariant. -/
private theorem structState_wf : structState.WellFormed linuxX8664 := by
  refine ⟨empty_wellFormed linuxX8664, ?_⟩
  intro p c hc
  simp only [State.places, structState, AList.lookup] at hc
  split at hc
  next => cases hc; trivial
  next => cases hc

-- 1. rep_roundtrip
/-- Instance (law `ConcreteSpec.Core.Hybrid.rep_roundtrip`, case `u8One`). -/
example : Rep linuxX8664 (Value.ty (.int .u8 1)) (encode linuxX8664 (.int .u8 1)) (.int .u8 1) :=
  rep_roundtrip linuxX8664 (.int .u8 1) (by decide)

/-- Planted (law `ConcreteSpec.Core.Hybrid.rep_roundtrip`, case `u8One`, subject `ConcreteSpec.Core.Hybrid.Rep`, mutant `ConcreteSpec.Core.Hybrid.Audit.badRep`). -/
example : Value.Storable (.int .u8 1) ∧
    ¬ badRep linuxX8664 (Value.ty (.int .u8 1)) (encode linuxX8664 (.int .u8 1)) (.int .u8 1) :=
  ⟨by decide, id⟩

/-- Planted (law `ConcreteSpec.Core.Hybrid.rep_roundtrip`, case `u8One`, subject `ConcreteSpec.Core.Hybrid.encode`, mutant `ConcreteSpec.Core.Hybrid.Audit.badEncode`). -/
example : Value.Storable (.int .u8 1) ∧
    ¬ Rep linuxX8664 (Value.ty (.int .u8 1)) (badEncode linuxX8664 (.int .u8 1)) (.int .u8 1) :=
  ⟨by decide, by intro h; cases h⟩

-- 2. decode_valid
/-- Instance (law `ConcreteSpec.Core.Hybrid.decode_valid`, case `u8One`). -/
example : match decode linuxX8664 (.int .u8) [.init 1 none] with
    | some v => v.ty = (.int .u8) ∧ v.Storable
    | none => True :=
  decode_valid linuxX8664 (.int .u8) [.init 1 none]

/-- Planted (law `ConcreteSpec.Core.Hybrid.decode_valid`, case `u8One`, subject `ConcreteSpec.Core.Hybrid.decode`, mutant `ConcreteSpec.Core.Hybrid.Audit.badDecodeUnit`). -/
example : ¬ (match badDecodeUnit linuxX8664 (.int .u8) [.init 1 none] with
    | some v => v.ty = (.int .u8) ∧ v.Storable
    | none => True) := by
  dsimp [badDecodeUnit]
  decide

-- 3. read_structured
/-- Instance (law `ConcreteSpec.Core.Hybrid.read_structured`, case `u8One`). -/
example : structState.read linuxX8664 ⟨0⟩ = some (.int .u8 1) :=
  read_structured linuxX8664 structState ⟨0⟩ (.int .u8) (.int .u8 1) rfl rfl (by decide)

/-- Planted (law `ConcreteSpec.Core.Hybrid.read_structured`, case `u8One`, subject `ConcreteSpec.Core.Hybrid.State.read`, mutant `ConcreteSpec.Core.Hybrid.Audit.badReadNone`). -/
example : structState.places ⟨0⟩ = some ⟨(.int .u8), .structured (.int .u8 1)⟩ ∧
    Value.ty (.int .u8 1) = (.int .u8) ∧ Value.Storable (.int .u8 1) ∧
    ¬ (badReadNone linuxX8664 structState ⟨0⟩ = some (.int .u8 1)) :=
  ⟨rfl, rfl, by decide, by decide⟩

-- 4. materialize_reuse
/-- Instance (law `ConcreteSpec.Core.Hybrid.materialize_reuse`, case `alreadyMat`). -/
example : before.materialize linuxX8664 ⟨0⟩ ⟨0⟩ 60 1 [] = some (range, before) :=
  materialize_reuse linuxX8664 before ⟨0⟩ (.int .u8) range rfl ⟨0⟩ 60 1 []

/-- Planted (law `ConcreteSpec.Core.Hybrid.materialize_reuse`, case `alreadyMat`, subject `ConcreteSpec.Core.Hybrid.State.materialize`, mutant `ConcreteSpec.Core.Hybrid.Audit.badMaterializeNone`). -/
example : before.places ⟨0⟩ = some ⟨(.int .u8), .materialized range⟩ ∧
    ¬ (badMaterializeNone linuxX8664 before ⟨0⟩ ⟨0⟩ 60 1 [] = some (range, before)) :=
  ⟨rfl, by intro h; cases h⟩

-- 5. write_read
/-- Instance (law `ConcreteSpec.Core.Hybrid.write_read`, case `u8One`). -/
example : after.read linuxX8664 ⟨0⟩ = some (.int .u8 1) :=
  write_read linuxX8664 before after before_wf.memory ⟨0⟩ (.int .u8 1) rfl

/-- Planted (law `ConcreteSpec.Core.Hybrid.write_read`, case `u8One`, subject `ConcreteSpec.Core.Hybrid.State.read`, mutant `ConcreteSpec.Core.Hybrid.Audit.badReadNone`). -/
example : before.memory.WellFormed linuxX8664 ∧
    before.write linuxX8664 ⟨0⟩ (.int .u8 1) = some after ∧
    ¬ (badReadNone linuxX8664 after ⟨0⟩ = some (.int .u8 1)) :=
  ⟨before_wf.memory, rfl, by decide⟩

-- 6. typed_store_frame
/-- Instance (law `ConcreteSpec.Core.Hybrid.typed_store_frame`, case `trailingPad`). Accepted: a real typed State.write updates the integer and preserves uninitialized trailing bytes in a well-formed state. -/
example : match before.write linuxX8664 ⟨0⟩ (.int .u8 1) with
    | some st' => (st'.memory.block ⟨0⟩).map (fun y => y.bytes[1]?) =
        some ((candidate ⟨0⟩ 40 3 1).bytes[1]?)
    | none => True :=
  typed_store_frame linuxX8664 before before_wf ⟨0⟩ (.int .u8) range (.int .u8 1) ⟨0⟩ 0 1
    (candidate ⟨0⟩ 40 3 1) rfl rfl rfl (.inr (by decide))

/-- Planted (law `ConcreteSpec.Core.Hybrid.typed_store_frame`, case `trailingPad`, subject `ConcreteSpec.Core.Hybrid.State.write`, mutant `ConcreteSpec.Core.Hybrid.Audit.badWriteClobber`). A typed write mutated to initialize trailing padding to zero violates the frame at byte 1 of the same well-formed state. -/
example : before.WellFormed linuxX8664 ∧
    before.places ⟨0⟩ = some ⟨(.int .u8), .materialized range⟩ ∧
    before.memory.access linuxX8664 range.ptr (sizeof (.int .u8)) range.align = some (some (⟨0⟩, 0)) ∧
    before.memory.block ⟨0⟩ = some (candidate ⟨0⟩ 40 3 1) ∧
    (1 < 0 ∨ 0 + sizeof (.int .u8) ≤ 1) ∧
    ¬ (match badWriteClobber linuxX8664 before ⟨0⟩ (.int .u8 1) with
      | some st' => (st'.memory.block ⟨0⟩).map (fun y => y.bytes[1]?) =
          some ((candidate ⟨0⟩ 40 3 1).bytes[1]?)
      | none => True) :=
  ⟨before_wf, rfl, rfl, rfl, .inr (by decide), by dsimp [badWriteClobber]; decide⟩

-- 7. materialize_projection
/-- Instance (law `ConcreteSpec.Core.Hybrid.materialize_projection`, case `firstMat`). -/
example : match structState.materialize linuxX8664 ⟨0⟩ ⟨0⟩ 40 1 [] with
    | some (r, st') => ∃ t, st'.places ⟨0⟩ = some ⟨t, .materialized r⟩
    | none => True :=
  materialize_projection linuxX8664 structState ⟨0⟩ ⟨0⟩ 40 1 []

/-- Planted (law `ConcreteSpec.Core.Hybrid.materialize_projection`, case `firstMat`, subject `ConcreteSpec.Core.Hybrid.State.materialize`, mutant `ConcreteSpec.Core.Hybrid.Audit.badMaterializeEmpty`). -/
example : ¬ (match badMaterializeEmpty linuxX8664 structState ⟨0⟩ ⟨0⟩ 40 1 [] with
    | some (r, st') => ∃ t, st'.places ⟨0⟩ = some ⟨t, .materialized r⟩
    | none => True) := by
  rintro ⟨t, ht⟩
  cases ht

-- 8. materialize_idempotent
/-- Instance (law `ConcreteSpec.Core.Hybrid.materialize_idempotent`, case `secondCall`). -/
example : before.materialize linuxX8664 ⟨0⟩ ⟨0⟩ 60 1 [] = some (range, before) :=
  materialize_idempotent linuxX8664 before before ⟨0⟩ ⟨0⟩ ⟨0⟩ 40 1 60 1 [] [] range rfl

/-- Planted (law `ConcreteSpec.Core.Hybrid.materialize_idempotent`, case `secondCall`, subject `ConcreteSpec.Core.Hybrid.State.materialize`, mutant `ConcreteSpec.Core.Hybrid.Audit.badMaterializeAddressGate`). -/
example : badMaterializeAddressGate linuxX8664 before ⟨0⟩ ⟨0⟩ 40 1 [] = some (range, before) ∧
    ¬ (badMaterializeAddressGate linuxX8664 before ⟨0⟩ ⟨0⟩ 60 1 [] = some (range, before)) :=
  ⟨rfl, by intro h; cases h⟩

/-- Instance (law `ConcreteSpec.Core.Hybrid.materialize_idempotent`, case `firstAllocation`). The first call allocates and initializes storage; a different second request reuses it. -/
example : matState.materialize linuxX8664 ⟨0⟩ ⟨0⟩ 60 1 [] = some (matRange, matState) :=
  materialize_idempotent linuxX8664 structState matState ⟨0⟩ ⟨0⟩ ⟨0⟩
    40 1 60 1 [] [] matRange rfl
/-- Planted (law `ConcreteSpec.Core.Hybrid.materialize_idempotent`, case `firstAllocation`, subject `ConcreteSpec.Core.Hybrid.State.materialize`, mutant `ConcreteSpec.Core.Hybrid.Audit.badMaterializeAddressGate`). -/
example : badMaterializeAddressGate linuxX8664 structState ⟨0⟩ ⟨0⟩ 40 1 [] = some (matRange, matState) ∧
    ¬ (badMaterializeAddressGate linuxX8664 matState ⟨0⟩ ⟨0⟩ 60 1 [] = some (matRange, matState)) :=
  ⟨rfl, by intro h; cases h⟩

-- 9. write_wellFormed
/-- Instance (law `ConcreteSpec.Core.Hybrid.write_wellFormed`, case `u8One`). -/
example : match before.write linuxX8664 ⟨0⟩ (.int .u8 1) with
    | some st' => st'.WellFormed linuxX8664
    | none => True :=
  write_wellFormed linuxX8664 before before_wf ⟨0⟩ (.int .u8 1)

/-- Planted (law `ConcreteSpec.Core.Hybrid.write_wellFormed`, case `u8One`, subject `ConcreteSpec.Core.Hybrid.State.write`, mutant `ConcreteSpec.Core.Hybrid.Audit.badWriteWf`). -/
example : before.WellFormed linuxX8664 ∧
    ¬ (match badWriteWf linuxX8664 before ⟨0⟩ (.int .u8 1) with
      | some st' => st'.WellFormed linuxX8664
      | none => True) :=
  ⟨before_wf, not_reset_wf _ _ rfl rfl⟩

-- 10. rawWrite_wellFormed
/-- Instance (law `ConcreteSpec.Core.Hybrid.rawWrite_wellFormed`, case `oneByte`). -/
example : match before.rawWrite linuxX8664 range.ptr [.init 1 none] 1 with
    | some st' => st'.WellFormed linuxX8664
    | none => True :=
  rawWrite_wellFormed linuxX8664 before before_wf range.ptr [.init 1 none] 1

/-- Planted (law `ConcreteSpec.Core.Hybrid.rawWrite_wellFormed`, case `oneByte`, subject `ConcreteSpec.Core.Hybrid.State.rawWrite`, mutant `ConcreteSpec.Core.Hybrid.Audit.badRawWriteWf`). -/
example : before.WellFormed linuxX8664 ∧
    ¬ (match badRawWriteWf linuxX8664 before range.ptr [.init 1 none] 1 with
      | some st' => st'.WellFormed linuxX8664
      | none => True) :=
  ⟨before_wf, not_reset_wf _ _ rfl rfl⟩

-- 11. materialize_wellFormed
/-- Instance (law `ConcreteSpec.Core.Hybrid.materialize_wellFormed`, case `firstMat`). -/
example : match structState.materialize linuxX8664 ⟨0⟩ ⟨0⟩ 40 1 [] with
    | some (_, st') => st'.WellFormed linuxX8664
    | none => True :=
  materialize_wellFormed linuxX8664 structState structState_wf ⟨0⟩ ⟨0⟩ 40 1 []

/-- Planted (law `ConcreteSpec.Core.Hybrid.materialize_wellFormed`, case `firstMat`, subject `ConcreteSpec.Core.Hybrid.State.materialize`, mutant `ConcreteSpec.Core.Hybrid.Audit.badMaterializeWf`). -/
example : structState.WellFormed linuxX8664 ∧
    ¬ (match badMaterializeWf linuxX8664 structState ⟨0⟩ ⟨0⟩ 40 1 [] with
      | some (_, st') => st'.WellFormed linuxX8664
      | none => True) :=
  ⟨structState_wf, not_reset_wf _ _ rfl rfl⟩

-- 12. expose_wellFormed
/-- Instance (law `ConcreteSpec.Core.Hybrid.expose_wellFormed`, case `firstExpose`). -/
example : match structState.expose linuxX8664 ⟨0⟩ ⟨0⟩ 40 1 [] with
    | some (_, st') => st'.WellFormed linuxX8664
    | none => True :=
  expose_wellFormed linuxX8664 structState structState_wf ⟨0⟩ ⟨0⟩ 40 1 []

/-- Planted (law `ConcreteSpec.Core.Hybrid.expose_wellFormed`, case `firstExpose`, subject `ConcreteSpec.Core.Hybrid.State.expose`, mutant `ConcreteSpec.Core.Hybrid.Audit.badExposeStateWf`). -/
example : structState.WellFormed linuxX8664 ∧
    ¬ (match badExposeStateWf linuxX8664 structState ⟨0⟩ ⟨0⟩ 40 1 [] with
      | some (_, st') => st'.WellFormed linuxX8664
      | none => True) :=
  ⟨structState_wf, not_reset_wf _ _ rfl rfl⟩

namespace Frame

/-- An existing materialized integer at address 80, independent of the place under test. -/
def observedStart : State := ⟨[(⟨1⟩, ⟨(.int .u8), .structured (.int .u8 1)⟩)], Memory.empty⟩
def observedRange : Range := ⟨⟨⟨0⟩, 80, some ⟨0⟩⟩, 1, 1⟩
def observed : State :=
  (observedStart.materialize linuxX8664 ⟨1⟩ ⟨0⟩ 80 1 []).getD (observedRange, observedStart) |>.2
def structState : State := observed.set ⟨0⟩ ⟨(.int .u8), .structured (.int .u8 0)⟩
def matRange : Range := ⟨⟨⟨0⟩, 40, some ⟨1⟩⟩, 1, 1⟩
def matState : State :=
  (structState.materialize linuxX8664 ⟨0⟩ ⟨0⟩ 40 1 []).getD (matRange, structState) |>.2
def matExposedState : State :=
  (structState.expose linuxX8664 ⟨0⟩ ⟨0⟩ 40 1 []).getD (matRange, structState) |>.2
def before : State := matState
def after : State := (before.write linuxX8664 ⟨0⟩ (.int .u8 1)).getD before

/-- Helper (congruence). Closed structured fixture has no memory projections yet. -/
private theorem observedStart_wf : observedStart.WellFormed linuxX8664 := by
  refine ⟨empty_wellFormed linuxX8664, ?_⟩
  intro p c hc
  simp only [State.places, observedStart, AList.lookup] at hc
  split at hc
  next => cases hc; trivial
  next => cases hc
/-- Helper (congruence). Derived from a successful first materialization, retaining its invariant. -/
private theorem observed_wf : observed.WellFormed linuxX8664 :=
  materialize_wellFormed linuxX8664 observedStart observedStart_wf ⟨1⟩ ⟨0⟩ 80 1 []
/-- Helper (congruence). Adding a structured cell does not disturb the existing projection. -/
private theorem structState_wf : structState.WellFormed linuxX8664 :=
  set_wellFormed linuxX8664 observed ⟨0⟩ ⟨(.int .u8), .structured (.int .u8 0)⟩ observed_wf trivial
/-- Helper (congruence). Both allocations and their materialized projections satisfy the invariant. -/
private theorem before_wf : before.WellFormed linuxX8664 :=
  materialize_wellFormed linuxX8664 structState structState_wf ⟨0⟩ ⟨0⟩ 40 1 []
/-- Helper (computation). Both existing places name distinct live block identities. -/
private theorem before_sep : before.Separated ⟨0⟩ ⟨1⟩ := by
  intro c d r s hc hd hr hs
  have hc' : c = ⟨(.int .u8), .materialized matRange⟩ := Option.some.inj hc.symm
  have hd' : d = ⟨(.int .u8), .materialized observedRange⟩ := Option.some.inj hd.symm
  subst c; subst d
  cases hr; cases hs
  decide
/-- Helper (computation). The live intervals 40..41 and 80..81 are disjoint. -/
private theorem before_disjoint : before.RangesDisjoint ⟨0⟩ ⟨1⟩ := by
  intro c d r s hc hd hr hs
  have hc' : c = ⟨(.int .u8), .materialized matRange⟩ := Option.some.inj hc.symm
  have hd' : d = ⟨(.int .u8), .materialized observedRange⟩ := Option.some.inj hd.symm
  subst c; subst d
  cases hr; cases hs
  exact .inr (.inr (.inr (.inl (by decide))))

/-- Corrupt the actual observed value by confusing the two live projections. -/
def badReadDiff : Target → State → Place → Option Value :=
  fun tgt st q => if q = ⟨1⟩ then st.read tgt ⟨0⟩ else st.read tgt q
/-- Corrupt the observed integer when a separate place becomes materialized. -/
def badReadEmptyDiff : Target → State → Place → Option Value :=
  fun tgt st q => if q = ⟨1⟩ then
    match st.places ⟨0⟩ with
    | some ⟨_, .materialized _⟩ => some (.int .u8 0)
    | _ => st.read tgt q
  else st.read tgt q

/-- Instance (laws `write_frame`, `materialize_frame`, `expose_frame`). The observed place decodes successfully before and after each transition. -/
example : before.read linuxX8664 ⟨1⟩ = some (.int .u8 1) ∧
    after.read linuxX8664 ⟨1⟩ = some (.int .u8 1) ∧
    structState.read linuxX8664 ⟨1⟩ = some (.int .u8 1) ∧
    matExposedState.read linuxX8664 ⟨1⟩ = some (.int .u8 1) := by decide

-- 13. write_frame
/-- Instance (law `ConcreteSpec.Core.Hybrid.write_frame`, case `otherPlace`). -/
example : after.read linuxX8664 ⟨1⟩ = before.read linuxX8664 ⟨1⟩ :=
  write_frame linuxX8664 before after ⟨0⟩ ⟨1⟩ (.int .u8 1) (by decide) before_sep rfl

/-- Planted (law `ConcreteSpec.Core.Hybrid.write_frame`, case `otherPlace`, subject `ConcreteSpec.Core.Hybrid.State.read`, mutant `ConcreteSpec.Core.Hybrid.Audit.Frame.badReadDiff`). -/
example : (⟨1⟩ : Place) ≠ ⟨0⟩ ∧ before.Separated ⟨0⟩ ⟨1⟩ ∧
    before.write linuxX8664 ⟨0⟩ (.int .u8 1) = some after ∧
    ¬ (badReadDiff linuxX8664 after ⟨1⟩ = badReadDiff linuxX8664 before ⟨1⟩) :=
  ⟨by decide, before_sep, rfl, by decide⟩

-- 14. write_frame_disjoint
/-- Instance (law `ConcreteSpec.Core.Hybrid.write_frame_disjoint`, case `otherPlace`). -/
example : after.read linuxX8664 ⟨1⟩ = before.read linuxX8664 ⟨1⟩ :=
  write_frame_disjoint linuxX8664 before after before_wf ⟨0⟩ ⟨1⟩ (.int .u8 1)
    (by decide) before_disjoint rfl

/-- Planted (law `ConcreteSpec.Core.Hybrid.write_frame_disjoint`, case `otherPlace`, subject `ConcreteSpec.Core.Hybrid.State.read`, mutant `ConcreteSpec.Core.Hybrid.Audit.Frame.badReadDiff`). -/
example : before.WellFormed linuxX8664 ∧ (⟨1⟩ : Place) ≠ ⟨0⟩ ∧
    before.RangesDisjoint ⟨0⟩ ⟨1⟩ ∧
    before.write linuxX8664 ⟨0⟩ (.int .u8 1) = some after ∧
    ¬ (badReadDiff linuxX8664 after ⟨1⟩ = badReadDiff linuxX8664 before ⟨1⟩) :=
  ⟨before_wf, by decide, before_disjoint, rfl, by decide⟩

-- 15. materialize_frame
/-- Instance (law `ConcreteSpec.Core.Hybrid.materialize_frame`, case `otherPlace`). -/
example : matState.read linuxX8664 ⟨1⟩ = structState.read linuxX8664 ⟨1⟩ :=
  materialize_frame linuxX8664 structState matState structState_wf ⟨0⟩ ⟨1⟩
    (by decide) ⟨0⟩ 40 1 [] matRange rfl

/-- Planted (law `ConcreteSpec.Core.Hybrid.materialize_frame`, case `otherPlace`, subject `ConcreteSpec.Core.Hybrid.State.read`, mutant `ConcreteSpec.Core.Hybrid.Audit.Frame.badReadEmptyDiff`). -/
example : structState.WellFormed linuxX8664 ∧ (⟨1⟩ : Place) ≠ ⟨0⟩ ∧
    structState.materialize linuxX8664 ⟨0⟩ ⟨0⟩ 40 1 [] = some (matRange, matState) ∧
    ¬ (badReadEmptyDiff linuxX8664 matState ⟨1⟩ =
      badReadEmptyDiff linuxX8664 structState ⟨1⟩) :=
  ⟨structState_wf, by decide, rfl, by decide⟩

-- 16. expose_frame
/-- Instance (law `ConcreteSpec.Core.Hybrid.expose_frame`, case `otherPlace`). -/
example : matExposedState.read linuxX8664 ⟨1⟩ = structState.read linuxX8664 ⟨1⟩ :=
  expose_frame linuxX8664 structState matExposedState structState_wf ⟨0⟩ ⟨1⟩
    (by decide) ⟨0⟩ 40 1 [] matRange rfl

/-- Planted (law `ConcreteSpec.Core.Hybrid.expose_frame`, case `otherPlace`, subject `ConcreteSpec.Core.Hybrid.State.read`, mutant `ConcreteSpec.Core.Hybrid.Audit.Frame.badReadEmptyDiff`). -/
example : structState.WellFormed linuxX8664 ∧ (⟨1⟩ : Place) ≠ ⟨0⟩ ∧
    structState.expose linuxX8664 ⟨0⟩ ⟨0⟩ 40 1 [] = some (matRange, matExposedState) ∧
    ¬ (badReadEmptyDiff linuxX8664 matExposedState ⟨1⟩ =
      badReadEmptyDiff linuxX8664 structState ⟨1⟩) :=
  ⟨structState_wf, by decide, rfl, by decide⟩

end Frame

-- 17. materialize_read
/-- Instance (law `ConcreteSpec.Core.Hybrid.materialize_read`, case `firstMat`). -/
example : matState.read linuxX8664 ⟨0⟩ = some (.int .u8 1) :=
  materialize_read linuxX8664 structState matState structState_wf.memory ⟨0⟩ (.int .u8 1)
    ⟨0⟩ 40 1 [] matRange rfl rfl

/-- Planted (law `ConcreteSpec.Core.Hybrid.materialize_read`, case `firstMat`, subject `ConcreteSpec.Core.Hybrid.State.read`, mutant `ConcreteSpec.Core.Hybrid.Audit.badReadStructOnly`). -/
example : structState.memory.WellFormed linuxX8664 ∧
    badReadStructOnly linuxX8664 structState ⟨0⟩ = some (.int .u8 1) ∧
    structState.materialize linuxX8664 ⟨0⟩ ⟨0⟩ 40 1 [] = some (matRange, matState) ∧
    ¬ (badReadStructOnly linuxX8664 matState ⟨0⟩ = some (.int .u8 1)) :=
  ⟨structState_wf.memory, rfl, rfl, by decide⟩

/-- Successful raw overwrite of the already materialized integer. -/
def rawAfter : State :=
  (matState.rawWrite linuxX8664 matRange.ptr [.init 2 none] 1).getD matState

/-- Wrong rule: keep reading the old value after a raw overwrite. -/
def staleRead (_tgt : Target) (_st : State) (_p : Place) : Option Value := some (.int .u8 1)

/-- Instance (law `ConcreteSpec.Core.Hybrid.rawWrite_read`, case `overwrite`). -/
example : rawAfter.read linuxX8664 ⟨0⟩ = decode linuxX8664 (.int .u8) [.init 2 none] :=
  rawWrite_read linuxX8664 matState rawAfter
    (materialize_wellFormed linuxX8664 structState structState_wf ⟨0⟩ ⟨0⟩ 40 1 []).memory
    ⟨0⟩ (.int .u8) matRange [.init 2 none] rfl rfl rfl

/-- Planted (law `ConcreteSpec.Core.Hybrid.rawWrite_read`, case `overwrite`). The same
well-formed state, projection, byte count and successful raw write reject a stale typed read. -/
example : matState.memory.WellFormed linuxX8664 ∧
    matState.places ⟨0⟩ = some ⟨.int .u8, .materialized matRange⟩ ∧
    ([MByte.init 2 none].length = sizeof (.int .u8)) ∧
    matState.rawWrite linuxX8664 matRange.ptr [.init 2 none] 1 = some rawAfter ∧
    ¬ (staleRead linuxX8664 rawAfter ⟨0⟩ = decode linuxX8664 (.int .u8) [.init 2 none]) :=
  ⟨(materialize_wellFormed linuxX8664 structState structState_wf ⟨0⟩ ⟨0⟩ 40 1 []).memory,
    rfl, rfl, rfl, by decide⟩

end Audit

end ConcreteSpec.Core.Hybrid
