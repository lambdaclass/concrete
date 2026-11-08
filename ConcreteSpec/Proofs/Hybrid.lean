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
        simp [State.read, State.set, hv]
      | materialized r =>
        simp only [hs] at h
        cases hm : st.memory.store tgt r.ptr (encode tgt v) r.align with
        | none => simp [hm] at h
        | some m =>
          simp [hm] at h
          subst st'
          have hl := store_load tgt st.memory m hw r.ptr (encode tgt v) r.align hm
          rw [encode_length, hv.1] at hl
          simp only [State.read, hc]
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

end ConcreteSpec.Core.Hybrid
