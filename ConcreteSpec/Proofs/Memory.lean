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

theorem decodePtr_encodePtr (tgt : Target) (ht : tgt.WellFormed) (p : Ptr) (hpv : p.Valid tgt) :
    decodePtr tgt (encodePtr tgt p) = some p := by
  have hp : 0 < tgt.ptrSize := by have := ht.1; have := ht.2.1; omega
  have hl : (ordered tgt.byteOrder (leBytes tgt.ptrSize p.addr)).length > 0 := by simpa using hp
  cases hs : ordered tgt.byteOrder (leBytes tgt.ptrSize p.addr) with
  | nil => simp [hs] at hl
  | cons v vs =>
    have he : encodePtr tgt p = .init v (some (p, 0)) ::
        (vs.zipIdx 1).map (fun (v, i) => .init v (some (p, i))) := by
      simp [encodePtr, hpv, hs, List.zipIdx_cons]
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
    m.access tgt p 0 a = if p.addr % a = 0 ∧ p.addr ≠ tgt.null ∧ p.Valid tgt then some none else none := by
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
  · simp only [List.map_cons, List.nodup_cons]
    refine ⟨?_, hw.unique⟩
    intro hm
    obtain ⟨e, he, hz⟩ := List.mem_map.mp hm
    have hh := hw.fresh e.1 e.2 (hw.canonical e he)
    rw [hz] at hh
    exact Nat.lt_irrefl _ hh
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
    next => cases hy; exact hf.2.2.2.2.2.2
    next => exact hw.spaces c y hy
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
        exact hf.2.2.2.2.2.1 _ (AList.lookup_mem hw') hwl (overlaps_symm _ _ hov)
    next =>
      split at hw'
      next => cases hw'; exact hf.2.2.2.2.2.1 _ (AList.lookup_mem hy) hyl
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

/-- A valid nonempty access identifies a live block and an in-bounds offset. -/
theorem access_some (tgt : Target) (m : Memory) (p : Ptr) (n a : Nat) (b : MemBlockId)
    (off : Nat) (h : m.access tgt p n a = some (some (b, off))) :
    ∃ x, p.prov = some b ∧ m.block b = some x ∧ n ≠ 0 ∧ x.live = true ∧
      x.space = p.space ∧ x.base ≤ p.addr ∧ p.addr + n ≤ x.base + x.size ∧
      p.addr % a = 0 ∧ off = p.addr - x.base ∧ p.Valid tgt := by
  unfold Memory.access at h
  split at h
  next => split at h <;> cases h
  next hn =>
    cases hp : p.prov with
    | none => simp [hp] at h
    | some c =>
      simp only [hp] at h
      cases hx : m.block c with
      | none => simp [hx] at h
      | some x =>
        simp only [hx] at h
        split at h
        next hv => cases h; exact ⟨x, rfl, hx, hn, hv.1, hv.2.1, hv.2.2.1,
          hv.2.2.2.1, hv.2.2.2.2.1, rfl, hv.2.2.2.2.2⟩
        next => cases h

/-- Replacing a range reads back exactly that range. -/
theorem splice_read (xs bs : List MByte) (off : Nat) (h : off ≤ xs.length) :
    ((xs.take off ++ bs ++ xs.drop (off + bs.length)).drop off).take bs.length = bs := by
  rw [List.append_assoc, List.drop_append_of_le_length (by simp [List.length_take, Nat.min_eq_left h])]
  simp

/-- The bytes before and after a splice are unchanged, including uninitialized bytes and fragments. -/
theorem splice_frame (xs bs : List MByte) (off i : Nat)
    (hb : off + bs.length ≤ xs.length) (hi : i < off ∨ off + bs.length ≤ i) :
    (xs.take off ++ bs ++ xs.drop (off + bs.length))[i]? = xs[i]? := by
  have ho : off ≤ xs.length := by omega
  have hl : (xs.take off).length = off := by simp [List.length_take, Nat.min_eq_left ho]
  rcases hi with hi | hi
  · rw [List.getElem?_append_left (by simp [hl]; omega)]
    rw [List.getElem?_append_left (by omega)]
    exact List.getElem?_take_of_lt hi
  · rw [List.getElem?_append_right (by simp [hl]; omega)]
    simp only [List.length_append, hl, List.getElem?_drop]
    congr 1
    omega

theorem store_load (tgt : Target) (m m' : Memory) (hw : m.WellFormed tgt)
    (p : Ptr) (bs : List MByte) (a : Nat) (h : m.store tgt p bs a = some m') :
    m'.load tgt p bs.length a = some bs := by
  unfold Memory.store at h
  cases ha : m.access tgt p bs.length a with
  | none => simp [ha] at h
  | some v =>
    rw [ha] at h
    cases v with
    | none =>
      cases h
      have hz : bs.length = 0 := by
        unfold Memory.access at ha
        split at ha
        next hn => exact hn
        next => split at ha <;> (try split at ha) <;> (try split at ha) <;> cases ha
      have he : bs = [] := List.eq_nil_of_length_eq_zero hz
      subst bs
      simp only [Memory.load, ha]
    | some v =>
      obtain ⟨b, off⟩ := v
      cases h
      obtain ⟨x, hp, hx, hn, hv, hs, hlo, hhi, hal, hoff, hpv⟩ := access_some tgt m p bs.length a b off ha
      have hsize := hw.sized b x hx
      have hbound : off + bs.length ≤ x.bytes.length := by omega
      have ha' : (m.modify b (fun x => { x with bytes := x.bytes.take off ++ bs ++ x.bytes.drop (off + bs.length) })).access tgt p bs.length a = some (some (b, off)) := by
        simp [Memory.access, hn, hp, hx, hv, hs, hlo, hhi, hal, hoff, hpv]
      simp only [Memory.load, ha', block_modify, ↓reduceIte, hx, Option.map_some]
      congr 1
      exact splice_read _ _ _ (by omega)

/-- A store frames every block except its access block. -/
theorem store_other_block (tgt : Target) (m m' : Memory) (p : Ptr) (bs : List MByte)
    (a : Nat) (b : MemBlockId) (off : Nat) (c : MemBlockId)
    (ha : m.access tgt p bs.length a = some (some (b, off)))
    (h : m.store tgt p bs a = some m') (hc : c ≠ b) : m'.block c = m.block c := by
  simp only [Memory.store, ha, Option.some.injEq] at h
  subst m'
  simp [hc]

/-- A store also frames each byte outside its range in the accessed block. -/
theorem store_same_block_frame (tgt : Target) (m m' : Memory) (hw : m.WellFormed tgt)
    (p : Ptr) (bs : List MByte) (a : Nat) (b : MemBlockId) (off i : Nat) (x : Block)
    (ha : m.access tgt p bs.length a = some (some (b, off)))
    (hx : m.block b = some x) (h : m.store tgt p bs a = some m')
    (hi : i < off ∨ off + bs.length ≤ i) :
    (m'.block b).map (fun y => y.bytes[i]?) = some (x.bytes[i]?) := by
  simp only [Memory.store, ha, Option.some.injEq] at h
  subst m'
  simp only [block_modify, ↓reduceIte, hx, Option.map_some]
  congr 1
  obtain ⟨y, _, hy, _, _, _, hlo, hhi, _, hoff, _⟩ := access_some tgt m p bs.length a b off ha
  rw [hx] at hy
  cases hy
  apply splice_frame _ _ _ _ _ hi
  have := hw.sized b x hx
  omega

/-- The allocated block is live and unexposed and every byte is uninitialized. -/
theorem alloc_initialized (tgt : Target) (m : Memory) (s : AddrSpace) (base size align : Nat)
    (b : MemBlockId) (m' : Memory) (h : m.alloc tgt s base size align = .ok (b, m')) :
    ∃ x, m'.block b = some x ∧ x.live = true ∧ x.exposed = false ∧
      x.bytes = List.replicate size .uninit :=
  ⟨candidate s base size align, alloc_block tgt m s base size align b m' h, rfl, rfl, rfl⟩

/-- Releasing a block cannot permit any existing identity to be reused by the next allocation,
even when the allocator reuses the released address. -/
theorem free_alloc_ne_existing (tgt : Target) (m freed allocated : Memory)
    (hw : m.WellFormed tgt) (b c new : MemBlockId) (x : Block)
    (hf : m.free b = some freed) (hc : m.block c = some x)
    (s : AddrSpace) (base size align : Nat)
    (ha : freed.alloc tgt s base size align = .ok (new, allocated)) : new ≠ c := by
  obtain ⟨_, _, _, hn⟩ := free_retains m b freed hf
  obtain ⟨he, _, _⟩ := alloc_result tgt freed s base size align new allocated ha
  have hlt := hw.fresh c x hc
  intro heq
  rw [heq] at he
  have hid := congrArg MemBlockId.id he
  dsimp at hid
  omega

end ConcreteSpec.Core.Mem
