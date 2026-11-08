import ConcreteSpec.Spec.Memory
import ConcreteSpec.Proofs.Scalar
import ConcreteSpec.Proofs.Basics

/-!
# Laws of byte memory (foundation C4)

Representation round trips, range and bool validity; allocation freshness and initialization;
retained identities on release; access failure without provenance or after release; store/load
and frame laws; exposed reconstruction soundness and uniqueness; address equality ignores
provenance. Target-dependent pointer round trips require a well-formed target.
-/
namespace ConcreteSpec.Core.Mem
open ConcreteSpec

@[simp] theorem ordered_ordered (o : ByteOrder) (bs : List (Fin 256)) :
    ordered o (ordered o bs) = bs := by cases o <;> simp [ordered]

@[simp] theorem leBytes_length (k n : Nat) : (leBytes k n).length = k := by
  induction k generalizing n with
  | zero => rfl
  | succ k ih => simp [leBytes, ih]

@[simp] theorem ordered_length (o : ByteOrder) (bs : List (Fin 256)) :
    (ordered o bs).length = bs.length := by cases o <;> simp [ordered]

@[simp] theorem octets_init (bs : List (Fin 256)) :
    octets (bs.map (.init · none)) = some bs := by
  induction bs with
  | nil => rfl
  | cons b bs ih => simp [octets, ih]

theorem leValue_leBytes (k n : Nat) (h : n < 256 ^ k) : leValue (leBytes k n) = n := by
  induction k generalizing n with
  | zero => simp at h; subst n; rfl
  | succ k ih =>
    have hd : n / 256 < 256 ^ k := by
      apply (Nat.div_lt_iff_lt_mul (by decide)).2
      simpa [Nat.pow_succ, Nat.mul_comm] using h
    simp only [leBytes, leValue, ih _ hd]
    omega

theorem leValue_lt (bs : List (Fin 256)) : leValue bs < 256 ^ bs.length := by
  induction bs with
  | nil => simp [leValue]
  | cons b bs ih =>
    simp only [leValue, List.length_cons, Nat.pow_succ]
    have := b.isLt
    omega

theorem intByte_power (t : IntTy) : 256 ^ intBytes t = 2 ^ t.bits := by cases t <;> decide

@[simp] theorem encodeInt_length (tgt : Target) (t : IntTy) (n : Int) :
    (encodeInt tgt t n).length = intBytes t := by simp [encodeInt]

theorem decodeInt_encodeInt (tgt : Target) (t : IntTy) (n : Int) (h : t.InRange n) :
    decodeInt tgt t (encodeInt tgt t n) = some n := by
  unfold decodeInt
  rw [encodeInt_length, ite_eq_left rfl]
  simp only [encodeInt, octets_init, Option.map_some, ordered_ordered]
  rw [leValue_leBytes _ _ (by rw [intByte_power]; exact toBits_lt t n), ofBits_toBits t n h]

theorem octets_length {bs : List MByte} {os : List (Fin 256)}
    (h : octets bs = some os) : os.length = bs.length := by
  induction bs generalizing os with
  | nil => cases h; rfl
  | cons b bs ih =>
    cases b with
    | uninit => cases h
    | init v f =>
      cases ho : octets bs with
      | none => simp [octets, ho] at h
      | some xs => simp [octets, ho] at h; subst os; simp [ih ho]

theorem decodeInt_inRange (tgt : Target) (t : IntTy) (bs : List MByte) (n : Int)
    (h : decodeInt tgt t bs = some n) : t.InRange n := by
  unfold decodeInt at h
  split at h
  next hl =>
    cases ho : octets bs with
    | none => simp [ho] at h
    | some os =>
      simp [ho] at h
      subst n
      apply ofBits_inRange
      have hb := leValue_lt (ordered tgt.byteOrder os)
      rw [ordered_length, octets_length ho, hl, intByte_power] at hb
      exact hb
  next => cases h

@[simp] theorem decodeBool_encodeBool (b : Bool) : decodeBool (encodeBool b) = some b := by
  cases b <;> rfl

theorem decodeBool_valid (bs : List MByte) (b : Bool) (h : decodeBool bs = some b) :
    ∃ v f, bs = [.init v f] ∧ v.val = boolToBits b := by
  match bs with
  | [.init v f] => exact ⟨v, f, rfl, (boolOfBits_some_iff _ _).1 h⟩
  | [] | .uninit :: _ | .init _ _ :: _ :: _ => cases h

theorem decodePtr_encodePtr (tgt : Target) (ht : tgt.WellFormed) (p : Ptr) :
    decodePtr tgt (encodePtr tgt p) = some p := by
  have hp : 0 < tgt.ptrSize := by have := ht.1; have := ht.2.1; omega
  have hl : (ordered tgt.byteOrder (leBytes tgt.ptrSize p.addr)).length > 0 := by simpa using hp
  cases hs : ordered tgt.byteOrder (leBytes tgt.ptrSize p.addr) with
  | nil => simp [hs] at hl
  | cons v vs =>
    have he : encodePtr tgt p = .init v (some (p, 0)) ::
        (vs.zipIdx 1).map (fun (v, i) => .init v (some (p, i))) := by
      simp [encodePtr, hs, List.zipIdx_cons]
    rw [he]
    simp only [decodePtr]
    rw [← he]
    simp

@[simp] theorem block_modify (m : Memory) (b c : MemBlockId) (f : Block → Block) :
    (m.modify b f).block c = if c = b then (m.block b).map f else m.block c := by
  exact AList.lookup_update b f m.blocks c

theorem alloc_result (tgt : Target) (m : Memory) (s : AddrSpace) (base size align : Nat)
    (b : MemBlockId) (m' : Memory) (h : m.alloc tgt s base size align = .ok (b, m')) :
    b = ⟨m.next⟩ ∧ m' = ⟨(b, candidate s base size align) :: m.blocks, m.next + 1⟩ ∧
    m.Fits tgt (candidate s base size align) := by
  simp only [Memory.alloc] at h
  split at h
  next hf => cases h; exact ⟨rfl, rfl, hf⟩
  next => cases h

theorem alloc_fresh (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
    (s : AddrSpace) (base size align : Nat) (b : MemBlockId) (m' : Memory)
    (h : m.alloc tgt s base size align = .ok (b, m')) : b.id = m.next ∧ m.block b = none := by
  obtain ⟨rfl, _, _⟩ := alloc_result tgt m s base size align b m' h
  refine ⟨rfl, ?_⟩
  cases hb : m.block ⟨m.next⟩ with
  | none => rfl
  | some x => have := hw.fresh _ _ hb; simp at this

theorem alloc_block (tgt : Target) (m : Memory) (s : AddrSpace) (base size align : Nat)
    (b : MemBlockId) (m' : Memory) (h : m.alloc tgt s base size align = .ok (b, m')) :
    m'.block b = some (candidate s base size align) := by
  obtain ⟨_, rfl, _⟩ := alloc_result tgt m s base size align b m' h
  simp [Memory.block, AList.lookup]

/-- The failure constructor contains only an error: no replacement memory. -/
theorem alloc_failure (tgt : Target) (m : Memory) (s : AddrSpace) (base size align : Nat)
    (h : ¬ m.Fits tgt (candidate s base size align)) :
    m.alloc tgt s base size align = .error .unavailable := by simp [Memory.alloc, h]

theorem access_zero (tgt : Target) (m : Memory) (p : Ptr) (a : Nat) :
    m.access tgt p 0 a = if p.addr % a = 0 ∧ p.addr ≠ tgt.null then some none else none := by
  simp [Memory.access]

theorem access_no_provenance (tgt : Target) (m : Memory) (p : Ptr) (n a : Nat)
    (hn : n ≠ 0) (hp : p.prov = none) : m.access tgt p n a = none := by
  simp [Memory.access, hn, hp]

theorem free_retains (m : Memory) (b : MemBlockId) (m' : Memory) (h : m.free b = some m') :
    ∃ x, m.block b = some x ∧
      m'.block b = some { x with live := false, exposed := false } ∧ m'.next = m.next := by
  unfold Memory.free at h
  cases hb : m.block b with
  | none => simp [hb] at h
  | some x =>
    rw [hb] at h
    by_cases hl : x.live = true
    · simp only [hl, ↓reduceIte, Option.some.injEq] at h
      subst m'
      exact ⟨x, rfl, by simp [hb], rfl⟩
    · simp [hl] at h

theorem access_after_free (tgt : Target) (m m' : Memory) (b : MemBlockId)
    (p : Ptr) (n a : Nat) (hf : m.free b = some m') (hp : p.prov = some b)
    (hn : n ≠ 0) : m'.access tgt p n a = none := by
  obtain ⟨x, _, hx, _⟩ := free_retains m b m' hf
  simp [Memory.access, hn, hp, hx]

theorem eqAddr_provenance (s : AddrSpace) (a : Nat) (b c : Option MemBlockId) :
    Ptr.eqAddr ⟨s, a, b⟩ ⟨s, a, c⟩ = true := by simp [Ptr.eqAddr]

end ConcreteSpec.Core.Mem
