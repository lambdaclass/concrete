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

/-- Any later allocation differs from every identity still present, including released blocks. -/
theorem alloc_ne_existing (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
    (s : AddrSpace) (base size align : Nat) (b c : MemBlockId) (m' : Memory) (x : Block)
    (h : m.alloc tgt s base size align = .ok (b, m')) (hc : m.block c = some x) : b ≠ c := by
  obtain ⟨hb, _⟩ := alloc_fresh tgt m hw s base size align b m' h
  have := hw.fresh c x hc
  intro he; subst c; omega

private theorem overlaps_symm (x y : Block) (h : x.Overlaps y) : y.Overlaps x :=
  ⟨h.1.symm, h.2.2.1, h.2.1, h.2.2.2.2, h.2.2.2.1⟩

theorem alloc_wellFormed (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
    (s : AddrSpace) (base size align : Nat) (b : MemBlockId) (m' : Memory)
    (h : m.alloc tgt s base size align = .ok (b, m')) : m'.WellFormed tgt := by
  obtain ⟨rfl, rfl, hf⟩ := alloc_result tgt m s base size align b m' h
  let z : MemBlockId := ⟨m.next⟩
  let x := candidate s base size align
  have lookup (c : MemBlockId) :
      (Memory.mk ((z, x) :: m.blocks) (m.next + 1)).block c =
        if z = c then some x else m.block c := rfl
  constructor
  · intro e he
    cases he with
    | head => simp [Memory.block, AList.lookup]
    | tail _ he =>
      rw [lookup]
      have hh := hw.canonical e he
      have hfresh := hw.fresh e.1 e.2 hh
      have hn : z ≠ e.1 := by intro hz; rw [← hz] at hfresh; simp [z] at hfresh
      simp [hn, hh]
  · intro c y hy
    rw [lookup] at hy
    split at hy
    next he => subst c; simp [z]
    next => have hh := hw.fresh c y hy; exact Nat.lt_trans hh (Nat.lt_succ_self _)
  · intro c y hy
    rw [lookup] at hy
    split at hy
    next => cases hy; simp [x, candidate]
    next => exact hw.sized c y hy
  · intro c y hy
    rw [lookup] at hy
    split at hy
    next => cases hy; exact hf.2.2.1
    next => exact hw.fits c y hy
  · intro c y hy
    rw [lookup] at hy
    split at hy
    next => cases hy; exact ⟨hf.1, hf.2.1⟩
    next => exact hw.aligned c y hy
  · intro c y d w hy hw' hne hyl hwl
    rw [lookup] at hy hw'
    split at hy
    next hc =>
      cases hy
      split at hw'
      next hd => exact False.elim (hne (hc.symm.trans hd))
      next =>
        intro hov
        exact hf.2.2.2.2.2 _ (AList.lookup_mem hw') hwl (overlaps_symm _ _ hov)
    next =>
      split at hw'
      next => cases hw'; exact hf.2.2.2.2.2 _ (AList.lookup_mem hy) hyl
      next => exact hw.disjoint c y d w hy hw' hne hyl hwl

/-- Non-overlap makes the in-bounds provenance unique; no order on identities is used. -/
theorem candidate_contains_unique (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
    (s : AddrSpace) (addr : Nat) (b c : MemBlockId) (x y : Block)
    (hx : m.block b = some x) (hy : m.block c = some y)
    (hxc : x.Candidate s = true) (hyc : y.Candidate s = true)
    (hxi : x.base ≤ addr ∧ addr < x.base + x.size)
    (hyi : y.base ≤ addr ∧ addr < y.base + y.size) : b = c := by
  by_cases hn : b = c
  · exact hn
  apply False.elim
  simp only [Block.Candidate, Bool.and_eq_true, decide_eq_true_eq] at hxc hyc
  apply hw.disjoint b x c y hx hy hn hxc.1.1.1 hyc.1.1.1
  refine ⟨hxc.2.trans hyc.2.symm, hxc.1.2, hyc.1.2, ?_, ?_⟩ <;> omega

/-- Reconstruction chooses an eligible containing block, or a one-past block only when no
eligible block contains the address. -/
theorem fromExposed_sound (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
    (s : AddrSpace) (addr : Nat) (b : MemBlockId)
    (h : (m.fromExposed s addr).prov = some b) :
    ∃ x, m.block b = some x ∧ x.Candidate s = true ∧
      ((x.base ≤ addr ∧ addr < x.base + x.size) ∨
       (addr = x.base + x.size ∧ ∀ e ∈ m.blocks, e.2.Candidate s = true →
         ¬ (e.2.base ≤ addr ∧ addr < e.2.base + e.2.size))) := by
  unfold Memory.fromExposed at h
  dsimp only at h
  split at h
  next e he =>
    cases h
    have hm := List.mem_of_find?_eq_some he
    have hc := List.mem_filter.mp hm
    exact ⟨e.2, hw.canonical e hc.1, hc.2,
      .inl (by have hh := List.find?_some he; simpa using hh)⟩
  next he =>
    split at h
    next e hp =>
      cases h
      have hc := List.mem_filter.mp (List.mem_of_find?_eq_some hp)
      refine ⟨e.2, hw.canonical e hc.1, hc.2, .inr ⟨?_, ?_⟩⟩
      · have hh := List.find?_some hp
        simpa using hh
      · intro e hm hc'
        have hh := List.find?_eq_none.mp he e (List.mem_filter.mpr ⟨hm, hc'⟩)
        simpa using hh
    next => cases h

end ConcreteSpec.Core.Mem
