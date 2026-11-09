import ConcreteSpec.Spec.Memory
import ConcreteSpec.Proofs.Scalar
import ConcreteSpec.Proofs.Basics

/-!
# Laws of byte memory (foundation C4)

Representation round trips, range and bool validity; allocation freshness and initialization;
retained identities on release; access failure without provenance or after release; store/load
and frame laws; exposed reconstruction soundness and uniqueness; address equality ignores
provenance. Empty memory is well-formed; geometry-preserving updates, store, free and exposure
preserve well-formedness. Exposure preserves byte reads. Reconstruction is complete at both
priorities, with no containing or one-past ties. Pointer round trips require a well-formed
target and a target-valid pointer.
-/
namespace ConcreteSpec.Core.Mem
open ConcreteSpec

/-- Representation lemma. Applying the byte-order permutation twice is the identity (approved: APPR-C4-agy-78ee5a2db). -/
@[simp] theorem ordered_ordered (o : ByteOrder) (bs : List (Fin 256)) :
    ordered o (ordered o bs) = bs := by cases o <;> simp [ordered]

/-- Representation lemma. Fixed-width little-endian encoding produces the requested octet count (approved: APPR-C4-agy-78ee5a2db). -/
@[simp] theorem leBytes_length (k n : Nat) : (leBytes k n).length = k := by
  induction k generalizing n with
  | zero => rfl
  | succ k ih => simp [leBytes, ih]

/-- Representation lemma. Byte-order permutation preserves octet count (approved: APPR-C4-agy-78ee5a2db). -/
@[simp] theorem ordered_length (o : ByteOrder) (bs : List (Fin 256)) :
    (ordered o bs).length = bs.length := by cases o <;> simp [ordered]

/-- Representation lemma. Plain initialized bytes extract to their underlying octets (approved: APPR-C4-agy-78ee5a2db). -/
@[simp] theorem octets_init (bs : List (Fin 256)) :
    octets (bs.map (.init · none)) = some bs := by
  induction bs with
  | nil => rfl
  | cons b bs ih => simp [octets, ih]

/-- Representation lemma. Little-endian decoding inverts fixed-width little-endian encoding below the width bound (approved: APPR-C4-agy-78ee5a2db). -/
theorem leValue_leBytes (k n : Nat) (h : n < 256 ^ k) : leValue (leBytes k n) = n := by
  induction k generalizing n with
  | zero => simp at h; subst n; rfl
  | succ k ih =>
    have hd : n / 256 < 256 ^ k := by
      apply (Nat.div_lt_iff_lt_mul (by decide)).2
      simpa [Nat.pow_succ, Nat.mul_comm] using h
    simp only [leBytes, leValue, ih _ hd]
    omega

/-- Representation lemma. Little-endian value is bounded by 256 to the octet length (approved: APPR-C4-agy-78ee5a2db). -/
theorem leValue_lt (bs : List (Fin 256)) : leValue bs < 256 ^ bs.length := by
  induction bs with
  | nil => simp [leValue]
  | cons b bs ih =>
    simp only [leValue, List.length_cons, Nat.pow_succ]
    have := b.isLt
    omega

/-- Representation lemma. Byte width of an integer type matches its bit-width power (approved: APPR-C4-agy-78ee5a2db). -/
theorem intByte_power (t : IntTy) : 256 ^ intBytes t = 2 ^ t.bits := by cases t <;> decide

/-- Representation lemma. Integer encoding produces intBytes octets (approved: APPR-C4-agy-78ee5a2db). -/
@[simp] theorem encodeInt_length (tgt : Target) (t : IntTy) (n : Int) :
    (encodeInt tgt t n).length = intBytes t := by simp [encodeInt]

/-- Law (subject `ConcreteSpec.Core.Mem.decodeInt`, `ConcreteSpec.Core.Mem.encodeInt`). Integer decoding inverts integer encoding for every in-range value. -/
theorem decodeInt_encodeInt (tgt : Target) (t : IntTy) (n : Int) (h : t.InRange n) :
    decodeInt tgt t (encodeInt tgt t n) = some n := by
  unfold decodeInt
  rw [encodeInt_length, ite_eq_left rfl]
  simp only [encodeInt, octets_init, Option.map_some, ordered_ordered]
  rw [leValue_leBytes _ _ (by rw [intByte_power]; exact toBits_lt t n), ofBits_toBits t n h]

/-- Representation lemma. Extracted octets preserve the byte-list length (approved: APPR-C4-agy-78ee5a2db). -/
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

/-- Law (subject `ConcreteSpec.Core.Mem.decodeInt`). Every decoded integer lies within its declared type's range. -/
theorem decodeInt_inRange (tgt : Target) (t : IntTy) (bs : List MByte) :
    match decodeInt tgt t bs with
    | some n => t.InRange n
    | none => True := by
  cases h : decodeInt tgt t bs with
  | none => trivial
  | some n =>
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

/-- Representation lemma. Computes the canonical boolean encoding round trip; semantic byte admission is covered by decodeBool_valid (approved: APPR-C4-agy-78ee5a2db). -/
@[simp] theorem decodeBool_encodeBool (b : Bool) : decodeBool (encodeBool b) = some b := by
  cases b <;> rfl

/-- Law (subject `ConcreteSpec.Core.Mem.decodeBool`). A byte list decodes to a boolean if and only if it is one initialized 0 or 1 byte. -/
theorem decodeBool_valid (bs : List MByte) (b : Bool) :
    decodeBool bs = some b ↔ ∃ v f, bs = [.init v f] ∧ v.val = boolToBits b := by
  constructor
  · intro h
    match bs with
    | [.init v f] => exact ⟨v, f, rfl, (boolOfBits_some_iff _ _).1 h⟩
    | [] | .uninit :: _ | .init _ _ :: _ :: _ => cases h
  · rintro ⟨v, f, rfl, hv⟩
    exact (boolOfBits_some_iff v.val b).2 hv

/-- Representation lemma. Fragment annotations do not alter the numeric octets (approved: APPR-C4-agy-78ee5a2db). -/
private theorem octets_fragments (os : List (Fin 256)) (p : Ptr) (start : Nat) :
    octets ((os.zipIdx start).map (fun (v, i) => MByte.init v (some (p, i)))) = some os := by
  induction os generalizing start with
  | nil => rfl
  | cons v vs ih => simp [List.zipIdx_cons, octets, ih]

/-- Representation lemma. Reading a fixed-width octet sequence and writing it back is exact (approved: APPR-C4-agy-78ee5a2db). -/
private theorem leBytes_leValue (os : List (Fin 256)) : leBytes os.length (leValue os) = os := by
  induction os with
  | nil => rfl
  | cons v vs ih =>
    simp [leBytes, leValue, Nat.add_mod, Nat.add_div, Nat.mod_eq_of_lt v.isLt,
      Nat.div_eq_of_lt v.isLt, Nat.not_le_of_gt v.isLt, ih]

/-- Law (subject `ConcreteSpec.Core.Mem.decodePtr`, `ConcreteSpec.Core.Mem.encodePtr`). A valid pointer round-trips by numeric octet decoding, independently of fragment validation. -/
theorem decodePtr_encodePtr (tgt : Target) (ht : tgt.WellFormed) (p : Ptr) (hpv : p.Valid tgt) :
    decodePtr tgt (encodePtr tgt p) = some p := by
  have hp : 0 < tgt.ptrSize := by have := ht.1; have := ht.2.1; omega
  have hl : (ordered tgt.byteOrder (leBytes tgt.ptrSize p.addr)).length > 0 := by simpa using hp
  have hb : p.addr < 256 ^ tgt.ptrSize := by
    have hh := Nat.pow_le_pow_right (by decide : 1 ≤ 2) ht.2.1
    have he : 256 ^ tgt.ptrSize = 2 ^ (8 * tgt.ptrSize) := by rw [Nat.pow_mul]
    rw [he]
    exact Nat.lt_of_lt_of_le hpv.1 hh
  have ho : octets (encodePtr tgt p) = some (ordered tgt.byteOrder (leBytes tgt.ptrSize p.addr)) :=
    octets_fragments _ _ 0
  have hlen : (encodePtr tgt p).length = tgt.ptrSize := by simp [encodePtr]
  cases hs : ordered tgt.byteOrder (leBytes tgt.ptrSize p.addr) with
  | nil => simp [hs] at hl
  | cons v vs =>
    have he : encodePtr tgt p = .init v (some (p, 0)) ::
        (vs.zipIdx 1).map (fun (v, i) => .init v (some (p, i))) := by
      simp [encodePtr, hs, List.zipIdx_cons]
    rw [he]
    simp only [decodePtr]
    rw [← he]
    simp only [hlen, ↓reduceIte, ho]
    simp [encodePtr, ordered_ordered, leValue_leBytes _ _ hb, hpv]

/-- Helper (computation). Block lookup after modify reduces to association-list update (approved: APPR-C4-agy-78ee5a2db). -/
@[simp] theorem block_modify (m : Memory) (b c : MemBlockId) (f : Block → Block) :
    (m.modify b f).block c = if c = b then (m.block b).map f else m.block c := by
  exact AList.lookup_update b f m.blocks c

/-- Law (subject `ConcreteSpec.Core.Mem.Memory.empty`). Empty memory satisfies every target's memory invariant. -/
theorem empty_wellFormed (tgt : Target) : Memory.empty.WellFormed tgt := by
  constructor <;> simp [Memory.empty, Memory.block, AList.lookup]

/-- Helper (congruence). Updating an association-list entry preserves the key list (approved: APPR-C4-agy-78ee5a2db). -/
private theorem keys_update (xs : AList MemBlockId Block) (b : MemBlockId) (f : Block → Block) :
    List.map Prod.fst (AList.update xs b f) = List.map Prod.fst xs := by
  induction xs with
  | nil => rfl
  | cons e xs ih =>
    simp only [AList.update]
    split <;> simp_all

/-- Helper (inversion). Nodup keys ensure every stored entry is returned by lookup (approved: APPR-C4-agy-78ee5a2db). -/
private theorem canonical_of_unique (xs : AList MemBlockId Block)
    (hu : (List.map Prod.fst xs).Nodup) :
    ∀ e ∈ xs, AList.lookup xs e.1 = some e.2 := by
  induction xs with
  | nil => simp
  | cons a xs ih =>
    simp only [List.map_cons, List.nodup_cons] at hu
    intro e he
    rcases List.mem_cons.mp he with he | he
    · subst e; simp [AList.lookup]
    · have hn : a.1 ≠ e.1 := by
        intro hh
        exact hu.1 (hh ▸ List.mem_map.mpr ⟨e, he, rfl⟩)
      simp [AList.lookup, hn, ih hu.2 e he]

/-- Helper (congruence). A block update preserving geometry and byte count, and never reviving a dead block,
preserves the whole memory invariant (approved: APPR-C4-agy-78ee5a2db). -/
theorem modify_wellFormed (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
    (b : MemBlockId) (f : Block → Block)
    (hg : ∀ x, (f x).space = x.space ∧ (f x).base = x.base ∧
      (f x).size = x.size ∧ (f x).align = x.align)
    (hs : ∀ x, m.block b = some x → (f x).bytes.length = x.size)
    (hl : ∀ x, (f x).live = true → x.live = true) :
    (m.modify b f).WellFormed tgt := by
  have hu : (List.map Prod.fst (m.modify b f).blocks).Nodup := by
    simpa only [Memory.modify, keys_update] using hw.unique
  have origin : ∀ c y, (m.modify b f).block c = some y →
      ∃ x, m.block c = some x ∧ y.space = x.space ∧ y.base = x.base ∧
        y.size = x.size ∧ y.align = x.align ∧ y.bytes.length = x.size ∧
        (y.live = true → x.live = true) := by
    intro c y hy
    rw [block_modify] at hy
    split at hy
    next he =>
      subst c
      cases hx : m.block b with
      | none => simp [hx] at hy
      | some x =>
        simp only [hx, Option.map_some, Option.some.injEq] at hy
        subst y
        exact ⟨x, rfl, (hg x).1, (hg x).2.1, (hg x).2.2.1,
          (hg x).2.2.2, hs x hx, hl x⟩
    next => exact ⟨y, hy, rfl, rfl, rfl, rfl, hw.sized c y hy, id⟩
  constructor
  · exact canonical_of_unique _ hu
  · exact hu
  · intro c y hy
    obtain ⟨x, hx, _⟩ := origin c y hy
    exact hw.fresh c x hx
  · intro c y hy
    obtain ⟨x, _, _, _, hsize, _, hlen, _⟩ := origin c y hy
    omega
  · intro c y hy
    obtain ⟨x, hx, _, hbase, hsize, _⟩ := origin c y hy
    rw [hbase, hsize]; exact hw.fits c x hx
  · intro c y hy
    obtain ⟨x, hx, hspace, _⟩ := origin c y hy
    rw [hspace]; exact hw.spaces c x hx
  · intro c y hy
    obtain ⟨x, hx, _, hbase, _, halign, _⟩ := origin c y hy
    rw [hbase, halign]; exact hw.aligned c x hx
  · intro c y d z hy hz hne hyl hzl
    obtain ⟨x, hx, hspace, hbase, hsize, _, _, hlive⟩ := origin c y hy
    obtain ⟨w, hw', hspace', hbase', hsize', _, _, hlive'⟩ := origin d z hz
    simpa only [Block.Overlaps, hspace, hbase, hsize, hspace', hbase', hsize'] using
      hw.disjoint c x d w hx hw' hne (hlive hyl) (hlive' hzl)

/-- Helper (inversion). Unpack the block identity, updated memory and fit condition of a successful allocation (approved: APPR-C4-agy-78ee5a2db). -/
theorem alloc_result (tgt : Target) (m : Memory) (s : AddrSpace) (base size align : Nat)
    (b : MemBlockId) (m' : Memory) (h : m.alloc tgt s base size align = .ok (b, m')) :
    b = ⟨m.next⟩ ∧ m' = ⟨(b, candidate s base size align) :: m.blocks, m.next + 1⟩ ∧
    m.Fits tgt (candidate s base size align) := by
  simp only [Memory.alloc] at h
  split at h
  next hf => cases h; exact ⟨rfl, rfl, hf⟩
  next => cases h

/-- Law (subject `ConcreteSpec.Core.Mem.Memory.alloc`). Successful allocation assigns the next fresh identity, absent from prior blocks. -/
theorem alloc_fresh (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
    (s : AddrSpace) (base size align : Nat) :
    match m.alloc tgt s base size align with
    | .ok (b, _) => b.id = m.next ∧ m.block b = none
    | .error _ => True := by
  cases h : m.alloc tgt s base size align with
  | error _ => trivial
  | ok pair =>
    obtain ⟨b, m'⟩ := pair
    obtain ⟨rfl, _, _⟩ := alloc_result tgt m s base size align b m' h
    refine ⟨rfl, ?_⟩
    cases hb : m.block ⟨m.next⟩ with
    | none => rfl
    | some x => have := hw.fresh _ _ hb; simp at this

/-- Law (subject `ConcreteSpec.Core.Mem.Memory.alloc`). Successful allocation installs the candidate block at the returned identity. -/
theorem alloc_block (tgt : Target) (m : Memory) (s : AddrSpace) (base size align : Nat) :
    match m.alloc tgt s base size align with
    | .ok (b, m') => m'.block b = some (candidate s base size align)
    | .error _ => True := by
  cases h : m.alloc tgt s base size align with
  | error _ => trivial
  | ok pair =>
    obtain ⟨b, m'⟩ := pair
    obtain ⟨_, rfl, _⟩ := alloc_result tgt m s base size align b m' h
    simp [Memory.block, AList.lookup]

/-- Law (subject `ConcreteSpec.Core.Mem.Memory.alloc`). Allocation rejects non-power-of-two alignment, base misalignment, a one-past address beyond the boundary, a size exceeding maxObject, a nonempty null base, overlap with an existing live block, or an unknown address space. -/
theorem alloc_failure (tgt : Target) (m : Memory) (s : AddrSpace) (base size align : Nat)
    (h : ¬ IsPow2 align ∨ base % align ≠ 0 ∨ 2 ^ tgt.addrBits - 1 < base + size ∨
      tgt.maxObject < size ∨ (0 < size ∧ base = tgt.null) ∨
      (∃ e ∈ m.blocks, e.2.live = true ∧ e.2.Overlaps (candidate s base size align)) ∨
      s ∉ tgt.spaces) :
    m.alloc tgt s base size align = .error .unavailable := by
  have hf : ¬ m.Fits tgt (candidate s base size align) := by
    intro hf
    rcases h with hp | ha | hb | hm | ⟨hpos, hnull⟩ | ⟨e, he, hl, ho⟩ | hs
    · exact hp hf.1
    · exact ha hf.2.1
    · exact Nat.not_lt_of_ge hf.2.2.1 hb
    · exact Nat.not_lt_of_ge hf.2.2.2.1 hm
    · exact hf.2.2.2.2.1 hpos hnull
    · exact hf.2.2.2.2.2.1 e he hl ho
    · exact hs hf.2.2.2.2.2.2
  simp [Memory.alloc, hf]

/-- Law (subject `ConcreteSpec.Core.Mem.Memory.load`, `ConcreteSpec.Core.Mem.Memory.store`). Zero-byte loads and stores succeed exactly at aligned, non-null, target-valid addresses;
no allocation or provenance is needed and an empty store preserves the whole memory. -/
theorem access_zero (tgt : Target) (m : Memory) (p : Ptr) (a : Nat) :
    (m.load tgt p 0 a = some [] ∧ m.store tgt p [] a = some m) ↔
      p.addr % a = 0 ∧ p.addr ≠ tgt.null ∧ p.Valid tgt := by
  by_cases h : p.addr % a = 0 ∧ p.addr ≠ tgt.null ∧ p.Valid tgt
  · simp [Memory.load, Memory.store, Memory.access, h]
  · simp [Memory.load, Memory.store, Memory.access, h]

/-- Law (subject `ConcreteSpec.Core.Mem.Memory.access`). A nonzero access through a pointer without provenance fails. -/
theorem access_no_provenance (tgt : Target) (m : Memory) (p : Ptr) (n a : Nat)
    (hn : n ≠ 0) (hp : p.prov = none) : m.access tgt p n a = none := by
  simp [Memory.access, hn, hp]

/-- Helper (inversion). Decomposes a successful release for the result-oriented free_retains law and access-after-free proof (approved: APPR-C4-agy-78ee5a2db). -/
theorem free_retains_inv (m : Memory) (b : MemBlockId) (m' : Memory) (h : m.free b = some m') :
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

/-- Law (subject `ConcreteSpec.Core.Mem.Memory.access`). Any nonzero access through a pointer to a released block fails. -/
theorem access_after_free (tgt : Target) (m m' : Memory) (b : MemBlockId)
    (p : Ptr) (n a : Nat) (hf : m.free b = some m') (hp : p.prov = some b)
    (hn : n ≠ 0) : m'.access tgt p n a = none := by
  obtain ⟨x, _, hx, _⟩ := free_retains_inv m b m' hf
  simp [Memory.access, hn, hp, hx]

/-- Helper (computation). Computes equality of identical space/address components; comparison separation is covered by eqAddr_separation (approved: APPR-C4-agy-78ee5a2db). -/
theorem eqAddr_provenance (s : AddrSpace) (a : Nat) (b c : Option MemBlockId) :
    Ptr.eqAddr ⟨s, a, b⟩ ⟨s, a, c⟩ = true := by simp [Ptr.eqAddr]

/-- Law (subject `ConcreteSpec.Core.Mem.Ptr.eqAddr`). Unequal pointers are separated by space or by strict numeric order, regardless of provenance. -/
theorem eqAddr_separation (p q : Ptr) :
    Ptr.eqAddr p q = false ↔ p.space ≠ q.space ∨ p.addr < q.addr ∨ q.addr < p.addr := by
  simp only [Ptr.eqAddr, decide_eq_false_iff_not]
  constructor
  · intro h
    by_cases hs : p.space = q.space
    · right; have hn : p.addr ≠ q.addr := fun ha => h ⟨hs, ha⟩
      omega
    · exact .inl hs
  · rintro (hs | ha | ha) ⟨hs', ha'⟩
    · exact hs hs'
    · omega
    · omega

/-- Law (subject `ConcreteSpec.Core.Mem.Memory.alloc`). Any later allocation differs from every identity still present, including released blocks. -/
theorem alloc_ne_existing (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
    (s : AddrSpace) (base size align : Nat) (c : MemBlockId) (x : Block)
    (hc : m.block c = some x) :
    match m.alloc tgt s base size align with
    | .ok (b, _) => b ≠ c
    | .error _ => True := by
  have hf := alloc_fresh tgt m hw s base size align
  cases h : m.alloc tgt s base size align with
  | error _ => trivial
  | ok pair =>
    obtain ⟨b, m'⟩ := pair
    rw [h] at hf
    have := hw.fresh c x hc
    intro he; subst c; omega

/-- Helper (projection). Block overlap is symmetric (approved: APPR-C4-agy-78ee5a2db). -/
private theorem overlaps_symm (x y : Block) (h : x.Overlaps y) : y.Overlaps x :=
  ⟨h.1.symm, h.2.2.1, h.2.1, h.2.2.2.2, h.2.2.2.1⟩

/-- Law (subject `ConcreteSpec.Core.Mem.Memory.alloc`). Successful allocation preserves memory well-formedness. -/
theorem alloc_wellFormed (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
    (s : AddrSpace) (base size align : Nat) :
    match m.alloc tgt s base size align with
    | .ok (_, m') => m'.WellFormed tgt
    | .error _ => True := by
  cases h : m.alloc tgt s base size align with
  | error _ => trivial
  | ok pair =>
    obtain ⟨b, m'⟩ := pair
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

/-- Helper (inversion). Non-overlap makes the in-bounds provenance unique; no order on identities is used (approved: APPR-C4-agy-78ee5a2db). -/
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

/-- Law (subject `ConcreteSpec.Core.Mem.Memory.fromExposed`). Reconstruction chooses an eligible containing block, or a one-past block only when no eligible block contains the address. -/
theorem fromExposed_sound (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
    (s : AddrSpace) (addr : Nat) :
    ∀ b, (m.fromExposed s addr).prov = some b →
    ∃ x, m.block b = some x ∧ x.Candidate s = true ∧
      ((x.base ≤ addr ∧ addr < x.base + x.size) ∨
       (addr = x.base + x.size ∧ ∀ e ∈ m.blocks, e.2.Candidate s = true →
         ¬ (e.2.base ≤ addr ∧ addr < e.2.base + e.2.size))) := by
  intro b h
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

/-- Helper (inversion). An eligible containing or one-past candidate suffices to recover some provenance (approved: APPR-C4-agy-78ee5a2db). -/
theorem fromExposed_exists (m : Memory) (s : AddrSpace) (addr : Nat)
    (e : MemBlockId × Block) (he : e ∈ m.blocks) (hc : e.2.Candidate s = true)
    (ha : (e.2.base ≤ addr ∧ addr < e.2.base + e.2.size) ∨
      addr = e.2.base + e.2.size) :
    ∃ b, (m.fromExposed s addr).prov = some b := by
  have hm : e ∈ m.blocks.filter (fun e => e.2.Candidate s) := List.mem_filter.mpr ⟨he, hc⟩
  unfold Memory.fromExposed
  dsimp only
  split
  next e' _ => exact ⟨e'.1, rfl⟩
  next hn =>
    split
    next e' _ => exact ⟨e'.1, rfl⟩
    next hp =>
      rcases ha with ha | ha
      · have hh := List.find?_eq_none.mp hn e hm
        simp [ha] at hh
      · have hh := List.find?_eq_none.mp hp e hm
        simp [ha] at hh

/-- Law (subject `ConcreteSpec.Core.Mem.Memory.fromExposed`). Completeness at the containing-block priority; no list-order tie-break enters the result. -/
theorem fromExposed_contains (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
    (s : AddrSpace) (addr : Nat) (b : MemBlockId) (x : Block)
    (hx : m.block b = some x) (hc : x.Candidate s = true)
    (hi : x.base ≤ addr ∧ addr < x.base + x.size) :
    (m.fromExposed s addr).prov = some b := by
  obtain ⟨c, hp⟩ := fromExposed_exists m s addr (b, x) (AList.lookup_mem hx) hc (.inl hi)
  obtain ⟨y, hy, hcy, hiy⟩ := fromExposed_sound tgt m hw s addr c hp
  rcases hiy with hiy | ⟨_, hn⟩
  · have := candidate_contains_unique tgt m hw s addr b c x y hx hy hc hcy hi hiy
    simpa only [← this] using hp
  · exact False.elim (hn (b, x) (AList.lookup_mem hx) hc hi)

/-- Helper (inversion). Nonempty disjoint blocks in one space cannot share their one-past endpoint either (approved: APPR-C4-agy-78ee5a2db). -/
theorem candidate_onePast_unique (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
    (s : AddrSpace) (addr : Nat) (b c : MemBlockId) (x y : Block)
    (hx : m.block b = some x) (hy : m.block c = some y)
    (hxc : x.Candidate s = true) (hyc : y.Candidate s = true)
    (hxi : addr = x.base + x.size) (hyi : addr = y.base + y.size) : b = c := by
  by_cases hn : b = c
  · exact hn
  apply False.elim
  simp only [Block.Candidate, Bool.and_eq_true, decide_eq_true_eq] at hxc hyc
  apply hw.disjoint b x c y hx hy hn hxc.1.1.1 hyc.1.1.1
  refine ⟨hxc.2.trans hyc.2.symm, hxc.1.2, hyc.1.2, ?_, ?_⟩ <;> omega

/-- Law (subject `ConcreteSpec.Core.Mem.Memory.fromExposed`). In the absence of any containing candidate, a one-past candidate is recovered exactly. -/
theorem fromExposed_onePast (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
    (s : AddrSpace) (addr : Nat) (b : MemBlockId) (x : Block)
    (hx : m.block b = some x) (hc : x.Candidate s = true) (hi : addr = x.base + x.size)
    (hn : ∀ e ∈ m.blocks, e.2.Candidate s = true →
      ¬ (e.2.base ≤ addr ∧ addr < e.2.base + e.2.size)) :
    (m.fromExposed s addr).prov = some b := by
  obtain ⟨c, hp⟩ := fromExposed_exists m s addr (b, x) (AList.lookup_mem hx) hc (.inr hi)
  obtain ⟨y, hy, hcy, hiy⟩ := fromExposed_sound tgt m hw s addr c hp
  rcases hiy with hiy | ⟨hiy, _⟩
  · exact False.elim (hn (c, y) (AList.lookup_mem hy) hcy hiy)
  · have := candidate_onePast_unique tgt m hw s addr b c x y hx hy hc hcy hi hiy
    simpa only [← this] using hp

/-- Law (subject `ConcreteSpec.Core.Mem.Memory.fromExposed`). If neither priority has a candidate, reconstruction supplies no provenance. -/
theorem fromExposed_none (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
    (s : AddrSpace) (addr : Nat)
    (hn : ∀ e ∈ m.blocks, e.2.Candidate s = true →
      ¬ (e.2.base ≤ addr ∧ addr < e.2.base + e.2.size) ∧ addr ≠ e.2.base + e.2.size) :
    (m.fromExposed s addr).prov = none := by
  cases hp : (m.fromExposed s addr).prov with
  | none => rfl
  | some b =>
    obtain ⟨x, hx, hc, hi⟩ := fromExposed_sound tgt m hw s addr b hp
    have hh := hn (b, x) (AList.lookup_mem hx) hc
    rcases hi with hi | ⟨hi, _⟩
    · exact False.elim (hh.1 hi)
    · exact False.elim (hh.2 hi)

/-- Helper (inversion). Extract the block and bounds of a successful nonempty access (approved: APPR-C4-agy-78ee5a2db). -/
theorem access_some_inv (tgt : Target) (m : Memory) (p : Ptr) (n a : Nat) (b : MemBlockId)
    (off : Nat) (h : m.access tgt p n a = some (some (b, off))) :
    ∃ x, p.prov = some b ∧ m.block b = some x ∧ n ≠ 0 ∧ x.live = true ∧
      x.space = p.space ∧ x.base ≤ p.addr ∧ p.addr + n ≤ x.base + x.size ∧
      p.addr % a = 0 ∧ off = p.addr - x.base := by
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
          hv.2.2.2.1, hv.2.2.2.2, rfl⟩
        next => cases h

/-- Law (subject `ConcreteSpec.Core.Mem.Memory.access`). Nonempty access validity follows from the stored block geometry, rather than a runtime check. -/
theorem access_valid (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
    (n a : Nat) (hn : n ≠ 0) :
    ∀ p, match m.access tgt p n a with
      | some _ => p.Valid tgt
      | none => True := by
  intro p
  cases h : m.access tgt p n a with
  | none => trivial
  | some r =>
    cases r with
    | none =>
      unfold Memory.access at h
      simp only [hn, ↓reduceIte] at h
      split at h
      next => cases h
      next => split at h <;> (try split at h) <;> cases h
    | some pair =>
      obtain ⟨b, off⟩ := pair
      obtain ⟨x, _, hx, _, _, hs, hlo, hhi, _, _⟩ := access_some_inv tgt m p n a b off h
      have hf := hw.fits b x hx
      have hpos : 0 < 2 ^ tgt.addrBits := Nat.two_pow_pos tgt.addrBits
      exact ⟨by have hnpos := Nat.pos_of_ne_zero hn; omega, hs ▸ hw.spaces b x hx⟩

/-- Helper (inversion). Zero-size access explicitly checks validity; this extracts that guard only (approved: APPR-C4-agy-78ee5a2db). -/
theorem access_zero_valid_inv (tgt : Target) (m : Memory) (p : Ptr) (a : Nat)
    (h : m.access tgt p 0 a = some none) : p.Valid tgt := by
  unfold Memory.access at h
  simp only [↓reduceIte] at h
  split at h
  next hv => exact hv.2.2
  next => cases h

/-- Helper (inversion). Extract the independent width, fragment, numeric and validity checks (approved: APPR-C4-agy-78ee5a2db). -/
private theorem decodePtr_checks_inv (tgt : Target) (bs : List MByte) (p : Ptr)
    (h : decodePtr tgt bs = some p) :
    ∃ os, octets bs = some os ∧ bs.length = tgt.ptrSize ∧
      bs = (os.zipIdx.map fun (v, i) => .init v (some (p, i))) ∧
      leValue (ordered tgt.byteOrder os) = p.addr ∧ p.Valid tgt := by
  match bs with
  | .init _ (some (q, 0)) :: _ =>
    simp only [decodePtr] at h
    split at h
    next hlen =>
      split at h
      next os ho =>
        split at h
        next hc => cases h; exact ⟨os, ho, hlen, hc⟩
        next => cases h
      next => cases h
    next => cases h
  | [] | .uninit :: _ | .init _ none :: _ | .init _ (some (_, Nat.succ _)) :: _ => cases h

/-- Law (subject `ConcreteSpec.Core.Mem.decodePtr`, `ConcreteSpec.Core.Mem.encodePtr`). Successful pointer decoding recovers exactly a whole stored representation and a valid
pointer. The representation follows from numeric byte reconstruction, not encoder comparison. -/
theorem decodePtr_some (tgt : Target) (ht : tgt.WellFormed) (bs : List MByte) (p : Ptr) :
    decodePtr tgt bs = some p ↔ bs = encodePtr tgt p ∧ p.Valid tgt := by
  constructor
  · intro h
    obtain ⟨os, ho, hlen, hf, hn, hv⟩ := decodePtr_checks_inv tgt bs p h
    have hos : os = ordered tgt.byteOrder (leBytes tgt.ptrSize p.addr) := by
      have hr := leBytes_leValue (ordered tgt.byteOrder os)
      rw [ordered_length, octets_length ho, hlen, hn] at hr
      have hh := congrArg (ordered tgt.byteOrder) hr
      simpa using hh.symm
    exact ⟨by rw [hf, hos]; rfl, hv⟩
  · rintro ⟨rfl, hv⟩
    exact decodePtr_encodePtr tgt ht p hv

/-- Law (subject `ConcreteSpec.Core.Mem.Memory.access`). Nonempty access succeeds on the independent liveness, provenance and geometry obligations.
Together with access_valid this proves deleting the redundant target-validity guard preserves
success on well-formed memory. -/
theorem access_success (tgt : Target) (m : Memory) (p : Ptr) (n a : Nat)
    (b : MemBlockId) (x : Block) (hn : n ≠ 0) (hp : p.prov = some b)
    (hx : m.block b = some x) (hl : x.live = true) (hs : x.space = p.space)
    (hlo : x.base ≤ p.addr) (hhi : p.addr + n ≤ x.base + x.size)
    (ha : p.addr % a = 0) :
    m.access tgt p n a = some (some (b, p.addr - x.base)) := by
  simp [Memory.access, hn, hp, hx, hl, hs, hlo, hhi, ha]

/-- Representation lemma. Replacing a range reads back exactly that range (approved: APPR-C4-agy-78ee5a2db). -/
theorem splice_read (xs bs : List MByte) (off : Nat) (h : off ≤ xs.length) :
    ((xs.take off ++ bs ++ xs.drop (off + bs.length)).drop off).take bs.length = bs := by
  rw [List.append_assoc, List.drop_append_of_le_length (by simp [List.length_take, Nat.min_eq_left h])]
  simp

/-- Representation lemma. The bytes before and after a splice are unchanged, including uninitialized bytes and fragments (approved: APPR-C4-agy-78ee5a2db). -/
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

/-- Law (subject `ConcreteSpec.Core.Mem.Memory.load`). Reading back a stored byte slice at the same pointer and alignment recovers the stored bytes. -/
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
      obtain ⟨x, hp, hx, hn, hv, hs, hlo, hhi, hal, hoff⟩ := access_some_inv tgt m p bs.length a b off ha
      have hsize := hw.sized b x hx
      have hbound : off + bs.length ≤ x.bytes.length := by omega
      have ha' : (m.modify b (fun x => { x with bytes := x.bytes.take off ++ bs ++ x.bytes.drop (off + bs.length) })).access tgt p bs.length a = some (some (b, off)) := by
        simp [Memory.access, hn, hp, hx, hv, hs, hlo, hhi, hal, hoff]
      simp only [Memory.load, ha', block_modify, ↓reduceIte, hx, Option.map_some]
      congr 1
      exact splice_read _ _ _ (by omega)

/-- Law (subject `ConcreteSpec.Core.Mem.Memory.store`). A store frames every block except its access block. -/
theorem store_other_block (tgt : Target) (m : Memory) (p : Ptr) (bs : List MByte)
    (a : Nat) (b : MemBlockId) (off : Nat) (c : MemBlockId)
    (ha : m.access tgt p bs.length a = some (some (b, off))) (hc : c ≠ b) :
    (m.store tgt p bs a).bind (fun m' => m'.block c) = m.block c := by
  simp [Memory.store, ha, hc]

/-- Law (subject `ConcreteSpec.Core.Mem.Memory.store`). A store also frames each byte outside its range in the accessed block. -/
theorem store_same_block_frame (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
    (p : Ptr) (bs : List MByte) (a : Nat) (b : MemBlockId) (off i : Nat) (x : Block)
    (ha : m.access tgt p bs.length a = some (some (b, off)))
    (hx : m.block b = some x) (hi : i < off ∨ off + bs.length ≤ i) :
    ((m.store tgt p bs a).bind fun m' => m'.block b).map (fun y => y.bytes[i]?) =
      some (x.bytes[i]?) := by
  simp only [Memory.store, ha, Option.bind_some, block_modify, ↓reduceIte, hx, Option.map_some]
  congr 1
  obtain ⟨y, _, hy, _, _, _, hlo, hhi, _, hoff⟩ := access_some_inv tgt m p bs.length a b off ha
  rw [hx] at hy
  cases hy
  apply splice_frame _ _ _ _ _ hi
  have := hw.sized b x hx
  omega

/-- Law (subject `ConcreteSpec.Core.Mem.Memory.alloc`). The allocated block is live and unexposed and every byte is uninitialized. -/
theorem alloc_initialized (tgt : Target) (m : Memory) (s : AddrSpace) (base size align : Nat) :
    match m.alloc tgt s base size align with
    | .ok (b, m') => ∃ x, m'.block b = some x ∧ x.live = true ∧ x.exposed = false ∧
        x.bytes = List.replicate size .uninit
    | .error _ => True := by
  have hb := alloc_block tgt m s base size align
  cases h : m.alloc tgt s base size align with
  | error _ => trivial
  | ok pair =>
    obtain ⟨b, m'⟩ := pair
    rw [h] at hb
    exact ⟨candidate s base size align, hb, rfl, rfl, rfl⟩

/-- Law (subject `ConcreteSpec.Core.Mem.Memory.store`). Successful byte stores preserve block geometry, liveness and well-formedness. -/
theorem store_wellFormed (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
    (p : Ptr) (bs : List MByte) (a : Nat) :
    match m.store tgt p bs a with
    | some m' => m'.WellFormed tgt
    | none => True := by
  cases h : m.store tgt p bs a with
  | none => trivial
  | some m' =>
    unfold Memory.store at h
    cases ha : m.access tgt p bs.length a with
    | none => simp [ha] at h
    | some v =>
      rw [ha] at h
      cases v with
      | none => cases h; exact hw
      | some pair =>
        obtain ⟨b, off⟩ := pair
        cases h
        obtain ⟨x, _, hx, _, _, _, hlo, hhi, _, hoff⟩ :=
          access_some_inv tgt m p bs.length a b off ha
        apply modify_wellFormed tgt m hw b
        · intro y; exact ⟨rfl, rfl, rfl, rfl⟩
        · intro y hy
          rw [hx] at hy; cases hy
          have := hw.sized b x hx
          simp only [List.length_append, List.length_take, List.length_drop]
          omega
        · intro y hy; exact hy

/-- Law (subject `ConcreteSpec.Core.Mem.Memory.free`). Successful release preserves the invariant without deleting the identity. -/
theorem free_wellFormed (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
    (b : MemBlockId) :
    match m.free b with
    | some m' => m'.WellFormed tgt
    | none => True := by
  cases h : m.free b with
  | none => trivial
  | some m' =>
    unfold Memory.free at h
    split at h
    next x hx =>
      split at h
      next =>
        cases h
        apply modify_wellFormed tgt m hw b
        · intro y; exact ⟨rfl, rfl, rfl, rfl⟩
        · intro y hy; exact hw.sized b y hy
        · intro y hy; cases hy
      next => cases h
    next => cases h

/-- Helper (inversion). Exposure changes exactly the exposed flag of a live provenance block (approved: APPR-C4-agy-78ee5a2db). -/
theorem expose_result (m m' : Memory) (p : Ptr) (h : m.expose p = some m') :
    ∃ b x, p.prov = some b ∧ m.block b = some x ∧ x.live = true ∧
      m' = m.modify b (fun y => { y with exposed := true }) := by
  unfold Memory.expose at h
  split at h
  next => cases h
  next b hp =>
    split at h
    next x hx =>
      split at h
      next hl => cases h; exact ⟨b, x, hp, hx, hl, rfl⟩
      next => cases h
    next => cases h

/-- Law (subject `ConcreteSpec.Core.Mem.Memory.expose`). Conversely every live provenance block can be exposed, preserving all other fields. -/
theorem expose_success (m : Memory) (p : Ptr) (b : MemBlockId) (x : Block)
    (hp : p.prov = some b) (hx : m.block b = some x) (hl : x.live = true) :
    m.expose p = some (m.modify b (fun y => { y with exposed := true })) := by
  simp [Memory.expose, hp, hx, hl]

/-- Helper (inversion). Decomposes a successful exposure for the result-oriented expose_frame law (approved: APPR-C4-agy-78ee5a2db). -/
theorem expose_frame_inv (m m' : Memory) (p : Ptr) (h : m.expose p = some m') :
    ∃ b x, p.prov = some b ∧ m.block b = some x ∧
      m'.block b = some { x with exposed := true } ∧ m'.next = m.next ∧
      ∀ c, c ≠ b → m'.block c = m.block c := by
  obtain ⟨b, x, hp, hx, _, rfl⟩ := expose_result m m' p h
  exact ⟨b, x, hp, hx, by simp [hx], rfl, by intro c hc; simp [hc]⟩

/-- Law (subject `ConcreteSpec.Core.Mem.Memory.expose`). Marking a block exposed preserves well-formedness. -/
theorem expose_wellFormed (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
    (p : Ptr) :
    match m.expose p with
    | some m' => m'.WellFormed tgt
    | none => True := by
  cases h : m.expose p with
  | none => trivial
  | some m' =>
    obtain ⟨b, _, _, _, _, rfl⟩ := expose_result m m' p h
    apply modify_wellFormed tgt m hw b
    · intro y; exact ⟨rfl, rfl, rfl, rfl⟩
    · intro y hy; exact hw.sized b y hy
    · intro y hy; exact hy

/-- Geometry relevant to hybrid projections; neither byte contents nor exposure is cached. -/
def Block.geometry (x : Block) : AddrSpace × Nat × Nat × Nat :=
  (x.space, x.base, x.size, x.align)

/-- Helper (congruence). Decomposes a successful store for store_geometry and hybrid invariant transport (approved: APPR-C4-agy-78ee5a2db). -/
theorem store_geometry_inv (tgt : Target) (m m' : Memory) (p : Ptr) (bs : List MByte)
    (a : Nat) (h : m.store tgt p bs a = some m') (b : MemBlockId) :
    (m'.block b).map Block.geometry = (m.block b).map Block.geometry := by
  unfold Memory.store at h
  split at h
  next => cases h; rfl
  next c off =>
    cases h
    simp only [block_modify]
    split
    · next he =>
      subst b
      cases m.block _ <;> rfl
    · rfl
  next => cases h

/-- Helper (congruence). Decomposes a successful exposure for expose_geometry and hybrid invariant transport (approved: APPR-C4-agy-78ee5a2db). -/
theorem expose_geometry_inv (m m' : Memory) (p : Ptr) (h : m.expose p = some m')
    (b : MemBlockId) :
    (m'.block b).map Block.geometry = (m.block b).map Block.geometry := by
  obtain ⟨c, _, _, _, _, rfl⟩ := expose_result m m' p h
  simp only [block_modify]
  split
  · next he =>
      subst b
      cases m.block _ <;> rfl
  · rfl

/-- Law (subject `ConcreteSpec.Core.Mem.Memory.free`). Releasing storage retains its identity and freshness, while marking the block dead and unexposed. -/
theorem free_retains (m : Memory) (b : MemBlockId) :
    match m.free b with
    | none => True
    | some m' => ∃ x, m.block b = some x ∧
        m'.block b = some { x with live := false, exposed := false } ∧ m'.next = m.next := by
  cases h : m.free b with
  | none => trivial
  | some m' => exact free_retains_inv m b m' h

/-- Law (subject `ConcreteSpec.Core.Mem.Memory.expose`). Exposure preserves every other block and freshness, changing only the exposed flag. -/
theorem expose_frame (m : Memory) (p : Ptr) :
    match m.expose p with
    | none => True
    | some m' => ∃ b x, p.prov = some b ∧ m.block b = some x ∧
        m'.block b = some { x with exposed := true } ∧ m'.next = m.next ∧
        ∀ c, c ≠ b → m'.block c = m.block c := by
  cases h : m.expose p with
  | none => trivial
  | some m' => exact expose_frame_inv m m' p h

/-- Law (subject `ConcreteSpec.Core.Mem.Memory.store`). Successful byte stores preserve all allocation geometry. -/
theorem store_geometry (tgt : Target) (m : Memory) (p : Ptr) (bs : List MByte) (a : Nat) :
    match m.store tgt p bs a with
    | none => True
    | some m' => ∀ b, (m'.block b).map Block.geometry = (m.block b).map Block.geometry := by
  cases h : m.store tgt p bs a with
  | none => trivial
  | some m' => exact store_geometry_inv tgt m m' p bs a h

/-- Law (subject `ConcreteSpec.Core.Mem.Memory.expose`). Exposure preserves all allocation geometry. -/
theorem expose_geometry (m : Memory) (p : Ptr) :
    match m.expose p with
    | none => True
    | some m' => ∀ b, (m'.block b).map Block.geometry = (m.block b).map Block.geometry := by
  cases h : m.expose p with
  | none => trivial
  | some m' => exact expose_geometry_inv m m' p h

/-- Helper (congruence). Reads depend only on their provenance block, independently of every other block (approved: APPR-C4-agy-78ee5a2db). -/
theorem load_congr (tgt : Target) (m m' : Memory) (p : Ptr) (n a : Nat)
    (h : ∀ b, p.prov = some b → m'.block b = m.block b) :
    m'.load tgt p n a = m.load tgt p n a := by
  by_cases hz : n = 0
  · by_cases hv : p.addr % a = 0 ∧ p.addr ≠ tgt.null ∧ p.Valid tgt
    all_goals simp [Memory.load, Memory.access, hz, hv]
  · cases hp : p.prov with
    | none => simp [Memory.load, Memory.access, hz, hp]
    | some b =>
      cases hx : m.block b with
      | none => simp [Memory.load, Memory.access, hz, hp, h b hp, hx]
      | some x =>
        simp only [Memory.load, Memory.access, hz, ↓reduceIte, hp, h b hp, hx]
        split <;> simp_all

/-- Law (subject `ConcreteSpec.Core.Mem.Memory.load`). A store frames reads using a different allocation identity. -/
theorem store_load_other (tgt : Target) (m m' : Memory) (p q : Ptr) (bs : List MByte)
    (a n align : Nat) (h : m.store tgt p bs a = some m') (hn : q.prov ≠ p.prov) :
    m'.load tgt q n align = m.load tgt q n align := by
  unfold Memory.store at h
  cases ha : m.access tgt p bs.length a with
  | none => simp [ha] at h
  | some v =>
    rw [ha] at h
    cases v with
    | none => cases h; rfl
    | some pair =>
      obtain ⟨b, off⟩ := pair
      cases h
      obtain ⟨_, hp, _⟩ := access_some_inv tgt m p bs.length a b off ha
      apply load_congr
      intro c hc
      have hcb : c ≠ b := by intro he; subst c; exact hn (hc.trans hp.symm)
      simp [hcb]

/-- Law (subject `ConcreteSpec.Core.Mem.Memory.load`). Exposure affects provenance reconstruction, but no byte read or access check. -/
theorem expose_load (tgt : Target) (m m' : Memory) (p q : Ptr) (n a : Nat)
    (h : m.expose p = some m') : m'.load tgt q n a = m.load tgt q n a := by
  obtain ⟨b, _, _, _, _, rfl⟩ := expose_result m m' p h
  by_cases hz : n = 0
  · by_cases hv : q.addr % a = 0 ∧ q.addr ≠ tgt.null ∧ q.Valid tgt
    all_goals simp [Memory.load, Memory.access, hz, hv]
  · cases hp : q.prov with
    | none => simp [Memory.load, Memory.access, hp, hz]
    | some c =>
      by_cases hc : c = b
      · subst c
        cases hx : m.block b with
        | none => simp [Memory.load, Memory.access, hp, hz, hx]
        | some x =>
          simp only [Memory.load, Memory.access, hz, ↓reduceIte, hp, block_modify, hx,
            Option.map_some]
          split <;> simp_all
      · apply load_congr
        intro d hd
        have hd' : d = c := by rw [hp] at hd; exact (Option.some.inj hd).symm
        subst d
        simp [hc]

/-- Law (subject `ConcreteSpec.Core.Mem.Memory.load`). A successful nonempty read can be shortened to any nonempty prefix. -/
theorem load_prefix (tgt : Target) (m : Memory) (p : Ptr) (n k a : Nat) (bs : List MByte)
    (hn : n ≠ 0) (hk : k ≠ 0) (hkn : k ≤ n) (h : m.load tgt p n a = some bs) :
    m.load tgt p k a = some (bs.take k) := by
  unfold Memory.load at h
  cases ha : m.access tgt p n a with
  | none => simp [ha] at h
  | some v =>
    rw [ha] at h
    cases v with
    | none => simp [Memory.access, hn] at ha; split at ha <;> (try split at ha) <;>
        (try split at ha) <;> cases ha
    | some pair =>
      obtain ⟨b, off⟩ := pair
      obtain ⟨x, hp, hx, _, hl, hs, hlo, hhi, hal, hoff⟩ :=
        access_some_inv tgt m p n a b off ha
      simp only [hx, Option.map_some, Option.some.injEq] at h
      subst bs
      have hb : p.addr + k ≤ x.base + x.size := by omega
      simp [Memory.load, Memory.access, hk, hp, hx, hl, hs, hlo, hb, hal, hoff,
        List.take_take, Nat.min_eq_left hkn]

/-- Law (subject `ConcreteSpec.Core.Mem.Memory.alloc`). Releasing a block cannot permit any existing identity to be reused by the next allocation,
even when the allocator reuses the released address. -/
theorem free_alloc_ne_existing (tgt : Target) (m freed : Memory)
    (hw : m.WellFormed tgt) (b c : MemBlockId) (x : Block)
    (hf : m.free b = some freed) (hc : m.block c = some x)
    (s : AddrSpace) (base size align : Nat) :
    match freed.alloc tgt s base size align with
    | .ok (new, _) => new ≠ c
    | .error _ => True := by
  cases ha : freed.alloc tgt s base size align with
  | error _ => trivial
  | ok pair =>
    obtain ⟨new, allocated⟩ := pair
    obtain ⟨_, _, _, hn⟩ := free_retains_inv m b freed hf
    obtain ⟨he, _, _⟩ := alloc_result tgt freed s base size align new allocated ha
    have hlt := hw.fresh c x hc
    intro heq
    rw [heq] at he
    have hid := congrArg MemBlockId.id he
    dsimp at hid
    omega


namespace Audit

def target : Target :=
  { linuxX8664 with addrBits := 8, ptrSize := 2, ptrAlign := 1, maxObject := 250 }
def ptr : Ptr := ⟨⟨0⟩, 40, some ⟨0⟩⟩
def other : Ptr := ⟨⟨0⟩, 41, some ⟨1⟩⟩
def badPtr : Ptr := ⟨⟨0⟩, 256, some ⟨0⟩⟩
def nullPtr : Ptr := ⟨⟨0⟩, 0, none⟩
def rawPtr : Ptr := ⟨⟨0⟩, 40, none⟩
def bytes : List MByte := [.init 40 (some (ptr, 0)), .init 0 (some (ptr, 1))]
def initBlock : Block := candidate ⟨0⟩ 40 8 1
def memory : Memory := ⟨[(⟨0⟩, initBlock)], 1⟩
def freedBlock : Block := { initBlock with live := false, exposed := false }
def freedMemory : Memory := ⟨[(⟨0⟩, freedBlock)], 1⟩
def exposedBlock : Block := { initBlock with exposed := true }
def exposedMemory : Memory := ⟨[(⟨0⟩, exposedBlock)], 1⟩
def storedBlock : Block :=
  { initBlock with bytes := .init 7 none :: List.replicate 7 .uninit }
def storedMemory : Memory := ⟨[(⟨0⟩, storedBlock)], 1⟩

/-- A second live allocation with actual initialized bytes for framing witnesses. -/
def frameBlock : Block := { candidate ⟨0⟩ 80 2 1 with bytes := [.init 9 none, .init 10 none] }
def frameMemory : Memory := ⟨[(⟨1⟩, frameBlock), (⟨0⟩, initBlock)], 2⟩
def frameStoredMemory : Memory := ⟨[(⟨1⟩, frameBlock), (⟨0⟩, storedBlock)], 2⟩
def frameOther : Ptr := ⟨⟨0⟩, 80, some ⟨1⟩⟩


def badDecodeInt : Target → IntTy → List MByte → Option Int := fun _ _ _ => none
def badDecodeIntRange : Target → IntTy → List MByte → Option Int := fun _ _ _ => some 200
def badEncodeInt : Target → IntTy → Int → List MByte := fun _ _ _ => []
def badDecodeBool : List MByte → Option Bool := fun _ => none
def badEncodeBool : Bool → List MByte := fun _ => []
def badDecodePtr (tgt : Target) (bs : List MByte) : Option Ptr :=
  decodePtr { tgt with byteOrder := .big } bs
def badEncodePtr : Target → Ptr → List MByte := fun _ _ => []
def firstFragment : Target → List MByte → Option Ptr
  | _, .init _ (some (p, _)) :: _ => some p
  | _, _ => none

def badEmpty : Memory := ⟨[(⟨1⟩, initBlock)], 0⟩

def badAllocId : Target → Memory → AddrSpace → Nat → Nat → Nat →
    Except AllocError (MemBlockId × Memory) :=
  fun _ m s base size align =>
    .ok (⟨m.next + 1⟩, ⟨(⟨m.next + 1⟩, candidate s base size align) :: m.blocks, m.next + 1⟩)

def badAllocEmpty : Target → Memory → AddrSpace → Nat → Nat → Nat →
    Except AllocError (MemBlockId × Memory) :=
  fun _ m _ _ _ _ => .ok (⟨m.next⟩, m)

def badAllocReuseZero : Target → Memory → AddrSpace → Nat → Nat → Nat →
    Except AllocError (MemBlockId × Memory) :=
  fun _ m _ _ _ _ => .ok (⟨0⟩, m)

/-- Deliberately wrong allocator for planted checks: skip exactly one geometry obligation.
0 skips power-of-two alignment, 1 skips base alignment, 2 skips the address boundary,
3 skips maxObject, 4 skips the non-null base check, 5 skips live overlap,
6 skips address-space membership. -/
def mutatedAlloc (skip : Nat) (tgt : Target) (m : Memory) (s : AddrSpace)
    (base size align : Nat) : Except AllocError (MemBlockId × Memory) :=
  let x := candidate s base size align
  if (skip = 0 ∨ IsPow2 align) ∧ (skip = 1 ∨ base % align = 0) ∧
      (skip = 2 ∨ base + size ≤ 2 ^ tgt.addrBits - 1) ∧
      (skip = 3 ∨ size ≤ tgt.maxObject) ∧
      (skip = 4 ∨ (0 < size → base ≠ tgt.null)) ∧
      (skip = 5 ∨ ∀ e ∈ m.blocks, e.2.live = true → ¬ e.2.Overlaps x) ∧
      (skip = 6 ∨ x.space ∈ tgt.spaces)
  then .ok (⟨m.next⟩, ⟨(⟨m.next⟩, x) :: m.blocks, m.next + 1⟩)
  else .error .unavailable

def mutatedAllocPow2 : Target → Memory → AddrSpace → Nat → Nat → Nat →
    Except AllocError (MemBlockId × Memory) := mutatedAlloc 0
def mutatedAllocAlign : Target → Memory → AddrSpace → Nat → Nat → Nat →
    Except AllocError (MemBlockId × Memory) := mutatedAlloc 1
def mutatedAllocBound : Target → Memory → AddrSpace → Nat → Nat → Nat →
    Except AllocError (MemBlockId × Memory) := mutatedAlloc 2
def mutatedAllocMaxObj : Target → Memory → AddrSpace → Nat → Nat → Nat →
    Except AllocError (MemBlockId × Memory) := mutatedAlloc 3
def mutatedAllocNull : Target → Memory → AddrSpace → Nat → Nat → Nat →
    Except AllocError (MemBlockId × Memory) := mutatedAlloc 4
def mutatedAllocOverlap : Target → Memory → AddrSpace → Nat → Nat → Nat →
    Except AllocError (MemBlockId × Memory) := mutatedAlloc 5
def mutatedAllocSpace : Target → Memory → AddrSpace → Nat → Nat → Nat →
    Except AllocError (MemBlockId × Memory) := mutatedAlloc 6

def badLoadNone : Target → Memory → Ptr → Nat → Nat → Option (List MByte) :=
  fun _ _ _ _ _ => none
def badLoadDiff : Target → Memory → Ptr → Nat → Nat → Option (List MByte) :=
  fun _ m _ _ _ =>
    match m.block ⟨0⟩ with
    | some x => if x.bytes == initBlock.bytes && !x.exposed then none else some []
    | none => none
def badLoadPrefix : Target → Memory → Ptr → Nat → Nat → Option (List MByte) :=
  fun tgt m p sz a => if sz == 2 then m.load tgt p sz a else none
def badStoreNone : Target → Memory → Ptr → List MByte → Nat → Option Memory :=
  fun _ _ _ _ _ => none
def badStoreDup : Target → Memory → Ptr → List MByte → Nat → Option Memory :=
  fun _ m _ _ _ => some ⟨(⟨1⟩, candidate ⟨0⟩ 80 8 1) :: m.blocks, m.next + 1⟩
def badStoreWf : Target → Memory → Ptr → List MByte → Nat → Option Memory :=
  fun _ _ _ _ _ => some badEmpty
def badAccessSome : Target → Memory → Ptr → Nat → Nat → Option (Option (MemBlockId × Nat)) :=
  fun _ _ _ _ _ => some none
def badAccessPair : Target → Memory → Ptr → Nat → Nat → Option (Option (MemBlockId × Nat)) :=
  fun _ _ _ _ _ => some (some (⟨0⟩, 0))
def badAccessNone : Target → Memory → Ptr → Nat → Nat → Option (Option (MemBlockId × Nat)) :=
  fun _ _ _ _ _ => none
def badEqAddr : Ptr → Ptr → Bool := fun p q => p == q
def badFromExposedNone : Memory → AddrSpace → Nat → Ptr := fun _ s addr => ⟨s, addr, none⟩
def badFromExposedSome : Memory → AddrSpace → Nat → Ptr := fun _ s addr => ⟨s, addr, some ⟨0⟩⟩
def badFreeWf : Memory → MemBlockId → Option Memory := fun _ _ => some badEmpty
def badExposeNone : Memory → Ptr → Option Memory := fun _ _ => none
def badExposeWf : Memory → Ptr → Option Memory := fun _ _ => some badEmpty

/-- Helper (inversion). The out-of-range block identity in badEmpty violates the freshness invariant (approved: APPR-C4-agy-78ee5a2db). -/
private theorem not_badEmpty_wf : ¬ badEmpty.WellFormed target := by
  intro hw
  have h := hw.fresh ⟨1⟩ initBlock rfl
  cases h

/-- Helper (congruence). The audit fixture memory satisfies the target memory invariant (approved: APPR-C4-agy-78ee5a2db). -/
private theorem memory_wf : memory.WellFormed target :=
  alloc_wellFormed target Memory.empty (empty_wellFormed target) ⟨0⟩ 40 8 1

/-- Helper (congruence). The exposed audit fixture memory satisfies the target memory invariant (approved: APPR-C4-agy-78ee5a2db). -/
private theorem exposedMemory_wf : exposedMemory.WellFormed target :=
  expose_wellFormed target memory memory_wf ptr

-- 1. decodeInt_encodeInt
/-- Instance (law `ConcreteSpec.Core.Mem.decodeInt_encodeInt`, case `i8`). -/
example : decodeInt target .i8 (encodeInt target .i8 (-5)) = some (-5) :=
  decodeInt_encodeInt target .i8 (-5) (by decide)

/-- Planted (law `ConcreteSpec.Core.Mem.decodeInt_encodeInt`, case `i8`, subject `ConcreteSpec.Core.Mem.decodeInt`, mutant `ConcreteSpec.Core.Mem.Audit.badDecodeInt`). -/
example : IntTy.i8.InRange (-5) ∧
    ¬ (badDecodeInt target .i8 (encodeInt target .i8 (-5)) = some (-5)) := by decide

/-- Planted (law `ConcreteSpec.Core.Mem.decodeInt_encodeInt`, case `i8`, subject `ConcreteSpec.Core.Mem.encodeInt`, mutant `ConcreteSpec.Core.Mem.Audit.badEncodeInt`). -/
example : IntTy.i8.InRange (-5) ∧
    ¬ (decodeInt target .i8 (badEncodeInt target .i8 (-5)) = some (-5)) := by decide

-- 2. decodeInt_inRange
/-- Instance (law `ConcreteSpec.Core.Mem.decodeInt_inRange`, case `i8`). -/
example : match decodeInt target .i8 [.init 251 none] with
    | some n => IntTy.i8.InRange n
    | none => True :=
  decodeInt_inRange target .i8 [.init 251 none]

/-- Planted (law `ConcreteSpec.Core.Mem.decodeInt_inRange`, case `i8`, subject `ConcreteSpec.Core.Mem.decodeInt`, mutant `ConcreteSpec.Core.Mem.Audit.badDecodeIntRange`). -/
example : ¬ (match badDecodeIntRange target .i8 [.init 251 none] with
    | some n => IntTy.i8.InRange n
    | none => True) := by
  dsimp [badDecodeIntRange]
  decide

-- 3. decodeBool_encodeBool
/-- Representation check. Canonical computation. -/
example : decodeBool (encodeBool true) = some true :=
  decodeBool_encodeBool true

/-- Representation check. Canonical computation. -/
example : ¬ (badDecodeBool (encodeBool true) = some true) := by decide

/-- Representation check. Canonical computation. -/
example : ¬ (decodeBool (badEncodeBool true) = some true) := by decide

-- 4. decodeBool_valid
/-- Instance (law `ConcreteSpec.Core.Mem.decodeBool_valid`, case `true`). -/
example : decodeBool (encodeBool true) = some true ↔
    ∃ v f, encodeBool true = [.init v f] ∧ v.val = boolToBits true :=
  decodeBool_valid (encodeBool true) true

/-- Planted (law `ConcreteSpec.Core.Mem.decodeBool_valid`, case `true`, subject `ConcreteSpec.Core.Mem.decodeBool`, mutant `ConcreteSpec.Core.Mem.Audit.badDecodeBool`). -/
example : ¬ (badDecodeBool (encodeBool true) = some true ↔
    ∃ v f, encodeBool true = [.init v f] ∧ v.val = boolToBits true) := by
  intro h
  have hh := h.mpr ⟨⟨1, by decide⟩, none, rfl, rfl⟩
  cases hh

-- 5. decodePtr_encodePtr
/-- Instance (law `ConcreteSpec.Core.Mem.decodePtr_encodePtr`, case `valid`). Accepted: a valid whole pointer. -/
example : decodePtr target (encodePtr target ptr) = some ptr :=
  decodePtr_encodePtr target (by decide) ptr (by decide)

/-- Planted (law `ConcreteSpec.Core.Mem.decodePtr_encodePtr`, case `valid`, subject `ConcreteSpec.Core.Mem.decodePtr`, mutant `ConcreteSpec.Core.Mem.Audit.badDecodePtr`). A decoder mutated to reverse violates the round trip at this valid pointer. -/
example : target.WellFormed ∧ ptr.Valid target ∧
    ¬ (badDecodePtr target (encodePtr target ptr) = some ptr) := by decide

/-- Planted (law `ConcreteSpec.Core.Mem.decodePtr_encodePtr`, case `valid`, subject `ConcreteSpec.Core.Mem.encodePtr`, mutant `ConcreteSpec.Core.Mem.Audit.badEncodePtr`). -/
example : target.WellFormed ∧ ptr.Valid target ∧
    ¬ (decodePtr target (badEncodePtr target ptr) = some ptr) := by decide

-- 6. decodePtr_some
/-- Instance (law `ConcreteSpec.Core.Mem.decodePtr_some`, case `valid`). Accepted: successful decoding has exactly the stored encoding, including provenance. -/
example : decodePtr target bytes = some ptr ↔ bytes = encodePtr target ptr ∧ ptr.Valid target :=
  decodePtr_some target (by decide) bytes ptr

/-- Planted (law `ConcreteSpec.Core.Mem.decodePtr_some`, case `valid`, subject `ConcreteSpec.Core.Mem.decodePtr`, mutant `ConcreteSpec.Core.Mem.Audit.badDecodePtr`). -/
example : target.WellFormed ∧
    ¬ (badDecodePtr target bytes = some ptr ↔ bytes = encodePtr target ptr ∧ ptr.Valid target) := by
  decide

/-- Planted (law `ConcreteSpec.Core.Mem.decodePtr_some`, case `valid`, subject `ConcreteSpec.Core.Mem.encodePtr`, mutant `ConcreteSpec.Core.Mem.Audit.badEncodePtr`). -/
example : target.WellFormed ∧
    ¬ (decodePtr target bytes = some ptr ↔ bytes = badEncodePtr target ptr ∧ ptr.Valid target) := by
  decide

/-- Instance (law `ConcreteSpec.Core.Mem.decodePtr_some`, case `partial`). Refused: partial pointer, with the second fragment missing. -/
example : decodePtr target [.init 40 (some (ptr, 0))] = some ptr ↔
    [.init 40 (some (ptr, 0))] = encodePtr target ptr ∧ ptr.Valid target :=
  decodePtr_some target (by decide) [.init 40 (some (ptr, 0))] ptr

/-- Planted (law `ConcreteSpec.Core.Mem.decodePtr_some`, case `partial`, subject `ConcreteSpec.Core.Mem.decodePtr`, mutant `ConcreteSpec.Core.Mem.Audit.firstFragment`). The first-fragment-only mutant violates the converse on a partial pointer. -/
example : target.WellFormed ∧
    ¬ (firstFragment target [.init 40 (some (ptr, 0))] = some ptr ↔
      [.init 40 (some (ptr, 0))] = encodePtr target ptr ∧ ptr.Valid target) := by decide

/-- Refused: partial pointer, with the second fragment missing. -/
example : decodePtr target [.init 40 (some (ptr, 0))] = none := by decide
/-- Refused: two pointers' fragments, with otherwise correct octets and indices. -/
example : decodePtr target [.init 40 (some (ptr, 0)), .init 0 (some (other, 1))] = none := by decide
/-- Refused: the right pointer's fragments in the wrong order. -/
example : decodePtr target [.init 0 (some (ptr, 1)), .init 40 (some (ptr, 0))] = none := by decide
/-- Refused: correct fragments and indices, but one corrupted octet. -/
example : decodePtr target [.init 41 (some (ptr, 0)), .init 0 (some (ptr, 1))] = none := by decide
/-- Refused: integer bytes spell a valid address but carry no pointer meaning. -/
example : decodePtr target [.init 40 none, .init 0 none] = none := by decide
/-- Refused: uninitialized bytes cannot supply a pointer. -/
example : decodePtr target [.uninit, .uninit] = none := by decide

-- 7. empty_wellFormed
/-- Instance (law `ConcreteSpec.Core.Mem.empty_wellFormed`, case `default`). -/
example : Memory.empty.WellFormed target :=
  empty_wellFormed target

/-- Planted (law `ConcreteSpec.Core.Mem.empty_wellFormed`, case `default`, subject `ConcreteSpec.Core.Mem.Memory.empty`, mutant `ConcreteSpec.Core.Mem.Audit.badEmpty`). -/
example : ¬ badEmpty.WellFormed target := not_badEmpty_wf

-- 8. alloc_fresh
/-- Instance (law `ConcreteSpec.Core.Mem.alloc_fresh`, case `first`). -/
example : match Memory.empty.alloc target ⟨0⟩ 40 8 1 with
    | .ok (b, _) => b.id = Memory.empty.next ∧ Memory.empty.block b = none
    | .error _ => True :=
  alloc_fresh target Memory.empty (empty_wellFormed target) ⟨0⟩ 40 8 1

/-- Planted (law `ConcreteSpec.Core.Mem.alloc_fresh`, case `first`, subject `ConcreteSpec.Core.Mem.Memory.alloc`, mutant `ConcreteSpec.Core.Mem.Audit.badAllocId`). -/
example : Memory.empty.WellFormed target ∧
    ¬ (match badAllocId target Memory.empty ⟨0⟩ 40 8 1 with
      | .ok (b, _) => b.id = Memory.empty.next ∧ Memory.empty.block b = none
      | .error _ => True) :=
  ⟨empty_wellFormed target, by rintro ⟨h, _⟩; cases h⟩

-- 9. alloc_block
/-- Instance (law `ConcreteSpec.Core.Mem.alloc_block`, case `first`). -/
example : match Memory.empty.alloc target ⟨0⟩ 40 8 1 with
    | .ok (b, m') => m'.block b = some (candidate ⟨0⟩ 40 8 1)
    | .error _ => True :=
  alloc_block target Memory.empty ⟨0⟩ 40 8 1

/-- Planted (law `ConcreteSpec.Core.Mem.alloc_block`, case `first`, subject `ConcreteSpec.Core.Mem.Memory.alloc`, mutant `ConcreteSpec.Core.Mem.Audit.badAllocEmpty`). -/
example : ¬ (match badAllocEmpty target Memory.empty ⟨0⟩ 40 8 1 with
    | .ok (b, m') => m'.block b = some (candidate ⟨0⟩ 40 8 1)
    | .error _ => True) := by
  intro h; cases h

-- 10. alloc_failure (all 7 failure branches)
/-- Instance (law `ConcreteSpec.Core.Mem.alloc_failure`, case `pow2`). -/
example : Memory.empty.alloc target ⟨0⟩ 42 2 3 = .error .unavailable :=
  alloc_failure target Memory.empty ⟨0⟩ 42 2 3 (.inl (by decide))

/-- Planted (law `ConcreteSpec.Core.Mem.alloc_failure`, case `pow2`, subject `ConcreteSpec.Core.Mem.Memory.alloc`, mutant `ConcreteSpec.Core.Mem.Audit.mutatedAllocPow2`). -/
example : (¬ IsPow2 3 ∨ 42 % 3 ≠ 0 ∨ 2 ^ target.addrBits - 1 < 42 + 2 ∨
      target.maxObject < 2 ∨ (0 < 2 ∧ 42 = target.null) ∨
      (∃ e ∈ Memory.empty.blocks, e.2.live = true ∧ e.2.Overlaps (candidate ⟨0⟩ 42 2 3)) ∨
      (⟨0⟩ : AddrSpace) ∉ target.spaces) ∧
    ¬ (mutatedAllocPow2 target Memory.empty ⟨0⟩ 42 2 3 = .error .unavailable) :=
  ⟨.inl (by decide), by intro h; cases h⟩

/-- Instance (law `ConcreteSpec.Core.Mem.alloc_failure`, case `align`). -/
example : Memory.empty.alloc target ⟨0⟩ 41 2 2 = .error .unavailable :=
  alloc_failure target Memory.empty ⟨0⟩ 41 2 2 (.inr (.inl (by decide)))

/-- Planted (law `ConcreteSpec.Core.Mem.alloc_failure`, case `align`, subject `ConcreteSpec.Core.Mem.Memory.alloc`, mutant `ConcreteSpec.Core.Mem.Audit.mutatedAllocAlign`). -/
example : (¬ IsPow2 2 ∨ 41 % 2 ≠ 0 ∨ 2 ^ target.addrBits - 1 < 41 + 2 ∨
      target.maxObject < 2 ∨ (0 < 2 ∧ 41 = target.null) ∨
      (∃ e ∈ Memory.empty.blocks, e.2.live = true ∧ e.2.Overlaps (candidate ⟨0⟩ 41 2 2)) ∨
      (⟨0⟩ : AddrSpace) ∉ target.spaces) ∧
    ¬ (mutatedAllocAlign target Memory.empty ⟨0⟩ 41 2 2 = .error .unavailable) :=
  ⟨.inr (.inl (by decide)), by intro h; cases h⟩

/-- Instance (law `ConcreteSpec.Core.Mem.alloc_failure`, case `bound`). -/
example : Memory.empty.alloc target ⟨0⟩ 254 2 1 = .error .unavailable :=
  alloc_failure target Memory.empty ⟨0⟩ 254 2 1 (.inr (.inr (.inl (by decide))))

/-- Planted (law `ConcreteSpec.Core.Mem.alloc_failure`, case `bound`, subject `ConcreteSpec.Core.Mem.Memory.alloc`, mutant `ConcreteSpec.Core.Mem.Audit.mutatedAllocBound`). -/
example : (¬ IsPow2 1 ∨ 254 % 1 ≠ 0 ∨ 2 ^ target.addrBits - 1 < 254 + 2 ∨
      target.maxObject < 2 ∨ (0 < 2 ∧ 254 = target.null) ∨
      (∃ e ∈ Memory.empty.blocks, e.2.live = true ∧ e.2.Overlaps (candidate ⟨0⟩ 254 2 1)) ∨
      (⟨0⟩ : AddrSpace) ∉ target.spaces) ∧
    ¬ (mutatedAllocBound target Memory.empty ⟨0⟩ 254 2 1 = .error .unavailable) :=
  ⟨.inr (.inr (.inl (by decide))), by intro h; cases h⟩

/-- Instance (law `ConcreteSpec.Core.Mem.alloc_failure`, case `maxObject`). -/
example : Memory.empty.alloc target ⟨0⟩ 1 251 1 = .error .unavailable :=
  alloc_failure target Memory.empty ⟨0⟩ 1 251 1 (.inr (.inr (.inr (.inl (by decide)))))

/-- Planted (law `ConcreteSpec.Core.Mem.alloc_failure`, case `maxObject`, subject `ConcreteSpec.Core.Mem.Memory.alloc`, mutant `ConcreteSpec.Core.Mem.Audit.mutatedAllocMaxObj`). -/
example : (¬ IsPow2 1 ∨ 1 % 1 ≠ 0 ∨ 2 ^ target.addrBits - 1 < 1 + 251 ∨
      target.maxObject < 251 ∨ (0 < 251 ∧ 1 = target.null) ∨
      (∃ e ∈ Memory.empty.blocks, e.2.live = true ∧ e.2.Overlaps (candidate ⟨0⟩ 1 251 1)) ∨
      (⟨0⟩ : AddrSpace) ∉ target.spaces) ∧
    ¬ (mutatedAllocMaxObj target Memory.empty ⟨0⟩ 1 251 1 = .error .unavailable) :=
  ⟨.inr (.inr (.inr (.inl (by decide)))), by intro h; cases h⟩

/-- Instance (law `ConcreteSpec.Core.Mem.alloc_failure`, case `null`). -/
example : Memory.empty.alloc target ⟨0⟩ 0 2 1 = .error .unavailable :=
  alloc_failure target Memory.empty ⟨0⟩ 0 2 1 (.inr (.inr (.inr (.inr (.inl (by decide))))))

/-- Planted (law `ConcreteSpec.Core.Mem.alloc_failure`, case `null`, subject `ConcreteSpec.Core.Mem.Memory.alloc`, mutant `ConcreteSpec.Core.Mem.Audit.mutatedAllocNull`). -/
example : (¬ IsPow2 1 ∨ 0 % 1 ≠ 0 ∨ 2 ^ target.addrBits - 1 < 0 + 2 ∨
      target.maxObject < 2 ∨ (0 < 2 ∧ 0 = target.null) ∨
      (∃ e ∈ Memory.empty.blocks, e.2.live = true ∧ e.2.Overlaps (candidate ⟨0⟩ 0 2 1)) ∨
      (⟨0⟩ : AddrSpace) ∉ target.spaces) ∧
    ¬ (mutatedAllocNull target Memory.empty ⟨0⟩ 0 2 1 = .error .unavailable) :=
  ⟨.inr (.inr (.inr (.inr (.inl (by decide))))), by intro h; cases h⟩

/-- Instance (law `ConcreteSpec.Core.Mem.alloc_failure`, case `overlap`). -/
example : memory.alloc target ⟨0⟩ 44 2 1 = .error .unavailable :=
  alloc_failure target memory ⟨0⟩ 44 2 1 (.inr (.inr (.inr (.inr (.inr (.inl (by decide)))))))

/-- Planted (law `ConcreteSpec.Core.Mem.alloc_failure`, case `overlap`, subject `ConcreteSpec.Core.Mem.Memory.alloc`, mutant `ConcreteSpec.Core.Mem.Audit.mutatedAllocOverlap`). -/
example : (¬ IsPow2 1 ∨ 44 % 1 ≠ 0 ∨ 2 ^ target.addrBits - 1 < 44 + 2 ∨
      target.maxObject < 2 ∨ (0 < 2 ∧ 44 = target.null) ∨
      (∃ e ∈ memory.blocks, e.2.live = true ∧ e.2.Overlaps (candidate ⟨0⟩ 44 2 1)) ∨
      (⟨0⟩ : AddrSpace) ∉ target.spaces) ∧
    ¬ (mutatedAllocOverlap target memory ⟨0⟩ 44 2 1 = .error .unavailable) :=
  ⟨.inr (.inr (.inr (.inr (.inr (.inl (by decide)))))), by intro h; cases h⟩

/-- Instance (law `ConcreteSpec.Core.Mem.alloc_failure`, case `space`). -/
example : Memory.empty.alloc target ⟨1⟩ 40 2 1 = .error .unavailable :=
  alloc_failure target Memory.empty ⟨1⟩ 40 2 1 (.inr (.inr (.inr (.inr (.inr (.inr (by decide)))))))

/-- Planted (law `ConcreteSpec.Core.Mem.alloc_failure`, case `space`, subject `ConcreteSpec.Core.Mem.Memory.alloc`, mutant `ConcreteSpec.Core.Mem.Audit.mutatedAllocSpace`). -/
example : (¬ IsPow2 1 ∨ 40 % 1 ≠ 0 ∨ 2 ^ target.addrBits - 1 < 40 + 2 ∨
      target.maxObject < 2 ∨ (0 < 2 ∧ 40 = target.null) ∨
      (∃ e ∈ Memory.empty.blocks, e.2.live = true ∧ e.2.Overlaps (candidate ⟨1⟩ 40 2 1)) ∨
      (⟨1⟩ : AddrSpace) ∉ target.spaces) ∧
    ¬ (mutatedAllocSpace target Memory.empty ⟨1⟩ 40 2 1 = .error .unavailable) :=
  ⟨.inr (.inr (.inr (.inr (.inr (.inr (by decide)))))), by intro h; cases h⟩

-- 11. access_zero
/-- Instance (law `ConcreteSpec.Core.Mem.access_zero`, case `alignedNonNull`). Accepted: zero-size load/store need no provenance, and the empty store preserves the complete memory. -/
example : (Memory.empty.load target rawPtr 0 2 = some [] ∧
      Memory.empty.store target rawPtr [] 2 = some Memory.empty) ↔
    rawPtr.addr % 2 = 0 ∧ rawPtr.addr ≠ target.null ∧ rawPtr.Valid target :=
  access_zero target Memory.empty rawPtr 2

/-- Planted (law `ConcreteSpec.Core.Mem.access_zero`, case `alignedNonNull`, subject `ConcreteSpec.Core.Mem.Memory.load`, mutant `ConcreteSpec.Core.Mem.Audit.badLoadNone`). -/
example : ¬ ((badLoadNone target Memory.empty rawPtr 0 2 = some [] ∧
      Memory.empty.store target rawPtr [] 2 = some Memory.empty) ↔
    rawPtr.addr % 2 = 0 ∧ rawPtr.addr ≠ target.null ∧ rawPtr.Valid target) := by
  intro h
  have hh := (h.mpr (by decide)).1
  cases hh

/-- Planted (law `ConcreteSpec.Core.Mem.access_zero`, case `alignedNonNull`, subject `ConcreteSpec.Core.Mem.Memory.store`, mutant `ConcreteSpec.Core.Mem.Audit.badStoreNone`). -/
example : ¬ ((Memory.empty.load target rawPtr 0 2 = some [] ∧
      badStoreNone target Memory.empty rawPtr [] 2 = some Memory.empty) ↔
    rawPtr.addr % 2 = 0 ∧ rawPtr.addr ≠ target.null ∧ rawPtr.Valid target) := by
  intro h
  have hh := (h.mpr (by decide)).2
  cases hh

-- 12. access_no_provenance
/-- Instance (law `ConcreteSpec.Core.Mem.access_no_provenance`, case `nonzero`). -/
example : memory.access target rawPtr 1 1 = none :=
  access_no_provenance target memory rawPtr 1 1 (by decide) rfl

/-- Planted (law `ConcreteSpec.Core.Mem.access_no_provenance`, case `nonzero`, subject `ConcreteSpec.Core.Mem.Memory.access`, mutant `ConcreteSpec.Core.Mem.Audit.badAccessSome`). -/
example : (1 : Nat) ≠ 0 ∧ rawPtr.prov = none ∧
    ¬ (badAccessSome target memory rawPtr 1 1 = none) := by decide

-- 13. access_after_free
/-- Instance (law `ConcreteSpec.Core.Mem.access_after_free`, case `freed`). -/
example : freedMemory.access target ptr 1 1 = none :=
  access_after_free target memory freedMemory ⟨0⟩ ptr 1 1 rfl rfl (by decide)

/-- Planted (law `ConcreteSpec.Core.Mem.access_after_free`, case `freed`, subject `ConcreteSpec.Core.Mem.Memory.access`, mutant `ConcreteSpec.Core.Mem.Audit.badAccessSome`). -/
example : memory.free ⟨0⟩ = some freedMemory ∧ ptr.prov = some ⟨0⟩ ∧ (1 : Nat) ≠ 0 ∧
    ¬ (badAccessSome target freedMemory ptr 1 1 = none) :=
  ⟨rfl, rfl, by decide, by decide⟩

-- 14. eqAddr_provenance
/-- Representation check. Canonical computation. -/
example : Ptr.eqAddr ⟨⟨0⟩, 40, some ⟨0⟩⟩ ⟨⟨0⟩, 40, none⟩ = true :=
  eqAddr_provenance ⟨0⟩ 40 (some ⟨0⟩) none

/-- Representation check. Canonical computation. -/
example : ¬ (badEqAddr ⟨⟨0⟩, 40, some ⟨0⟩⟩ ⟨⟨0⟩, 40, none⟩ = true) := by decide

-- 15. alloc_ne_existing
/-- Instance (law `ConcreteSpec.Core.Mem.alloc_ne_existing`, case `second`). -/
example : match memory.alloc target ⟨0⟩ 60 8 1 with
    | .ok (b, _) => b ≠ ⟨0⟩
    | .error _ => True :=
  alloc_ne_existing target memory memory_wf ⟨0⟩ 60 8 1 ⟨0⟩ initBlock rfl

/-- Planted (law `ConcreteSpec.Core.Mem.alloc_ne_existing`, case `second`, subject `ConcreteSpec.Core.Mem.Memory.alloc`, mutant `ConcreteSpec.Core.Mem.Audit.badAllocReuseZero`). -/
example : memory.WellFormed target ∧ memory.block ⟨0⟩ = some initBlock ∧
    ¬ (match badAllocReuseZero target memory ⟨0⟩ 60 8 1 with
      | .ok (b, _) => b ≠ ⟨0⟩
      | .error _ => True) :=
  ⟨memory_wf, rfl, fun h => h rfl⟩

-- 16. alloc_wellFormed
/-- Instance (law `ConcreteSpec.Core.Mem.alloc_wellFormed`, case `first`). -/
example : match Memory.empty.alloc target ⟨0⟩ 40 8 1 with
    | .ok (_, m') => m'.WellFormed target
    | .error _ => True :=
  alloc_wellFormed target Memory.empty (empty_wellFormed target) ⟨0⟩ 40 8 1

/-- Planted (law `ConcreteSpec.Core.Mem.alloc_wellFormed`, case `first`, subject `ConcreteSpec.Core.Mem.Memory.alloc`, mutant `ConcreteSpec.Core.Mem.Audit.badAllocId`). -/
example : Memory.empty.WellFormed target ∧
    ¬ (match badAllocId target Memory.empty ⟨0⟩ 40 8 1 with
      | .ok (_, m') => m'.WellFormed target
      | .error _ => True) := by
  refine ⟨empty_wellFormed target, ?_⟩
  intro hw
  have h : (1 : Nat) < 1 := hw.fresh ⟨1⟩ initBlock rfl
  exact (by decide : ¬ ((1 : Nat) < 1)) h

-- 17. fromExposed_contains
/-- Instance (law `ConcreteSpec.Core.Mem.fromExposed_contains`, case `interior`). -/
example : (exposedMemory.fromExposed ⟨0⟩ 42).prov = some ⟨0⟩ :=
  fromExposed_contains target exposedMemory exposedMemory_wf ⟨0⟩ 42 ⟨0⟩ exposedBlock
    rfl rfl (by decide)

/-- Planted (law `ConcreteSpec.Core.Mem.fromExposed_contains`, case `interior`, subject `ConcreteSpec.Core.Mem.Memory.fromExposed`, mutant `ConcreteSpec.Core.Mem.Audit.badFromExposedNone`). -/
example : exposedMemory.WellFormed target ∧ exposedMemory.block ⟨0⟩ = some exposedBlock ∧
    exposedBlock.Candidate ⟨0⟩ = true ∧
    (exposedBlock.base ≤ 42 ∧ 42 < exposedBlock.base + exposedBlock.size) ∧
    ¬ ((badFromExposedNone exposedMemory ⟨0⟩ 42).prov = some ⟨0⟩) :=
  ⟨exposedMemory_wf, rfl, rfl, by decide, by decide⟩

-- 18. fromExposed_onePast
/-- Instance (law `ConcreteSpec.Core.Mem.fromExposed_onePast`, case `boundary`). -/
example : (exposedMemory.fromExposed ⟨0⟩ 48).prov = some ⟨0⟩ :=
  fromExposed_onePast target exposedMemory exposedMemory_wf ⟨0⟩ 48 ⟨0⟩ exposedBlock
    rfl rfl rfl (by decide)

/-- Planted (law `ConcreteSpec.Core.Mem.fromExposed_onePast`, case `boundary`, subject `ConcreteSpec.Core.Mem.Memory.fromExposed`, mutant `ConcreteSpec.Core.Mem.Audit.badFromExposedNone`). -/
example : exposedMemory.WellFormed target ∧ exposedMemory.block ⟨0⟩ = some exposedBlock ∧
    exposedBlock.Candidate ⟨0⟩ = true ∧ 48 = exposedBlock.base + exposedBlock.size ∧
    (∀ e ∈ exposedMemory.blocks, e.2.Candidate ⟨0⟩ = true →
      ¬ (e.2.base ≤ 48 ∧ 48 < e.2.base + e.2.size)) ∧
    ¬ ((badFromExposedNone exposedMemory ⟨0⟩ 48).prov = some ⟨0⟩) :=
  ⟨exposedMemory_wf, rfl, rfl, rfl, by decide, by decide⟩

-- 19. fromExposed_none
/-- Instance (law `ConcreteSpec.Core.Mem.fromExposed_none`, case `outside`). -/
example : (exposedMemory.fromExposed ⟨0⟩ 100).prov = none :=
  fromExposed_none target exposedMemory exposedMemory_wf ⟨0⟩ 100 (by decide)

/-- Planted (law `ConcreteSpec.Core.Mem.fromExposed_none`, case `outside`, subject `ConcreteSpec.Core.Mem.Memory.fromExposed`, mutant `ConcreteSpec.Core.Mem.Audit.badFromExposedSome`). -/
example : exposedMemory.WellFormed target ∧
    (∀ e ∈ exposedMemory.blocks, e.2.Candidate ⟨0⟩ = true →
      ¬ (e.2.base ≤ 100 ∧ 100 < e.2.base + e.2.size) ∧
        100 ≠ e.2.base + e.2.size) ∧
    ¬ ((badFromExposedSome exposedMemory ⟨0⟩ 100).prov = none) :=
  ⟨exposedMemory_wf, by decide, by decide⟩

-- 20. access_valid
/-- Instance (law `ConcreteSpec.Core.Mem.access_valid`, case `nonempty`). The domain contains the successful two-byte access through ptr. -/
example : ∀ p, match memory.access target p 2 1 with
    | some _ => p.Valid target
    | none => True :=
  access_valid target memory memory_wf 2 1 (by decide)

/-- Accepted (rule C4). The nonempty validity instance includes an actual successful access. -/
example : memory.access target ptr 2 1 = some (some (⟨0⟩, 0)) := rfl

/-- Planted (law `ConcreteSpec.Core.Mem.access_valid`, case `nonempty`, subject `ConcreteSpec.Core.Mem.Memory.access`, mutant `ConcreteSpec.Core.Mem.Audit.badAccessPair`). -/
example : memory.WellFormed target ∧ (2 : Nat) ≠ 0 ∧
    ¬ (∀ p, match badAccessPair target memory p 2 1 with
      | some _ => p.Valid target
      | none => True) := by
  refine ⟨memory_wf, by decide, ?_⟩
  intro h
  have hv := h badPtr
  exact (by decide : ¬ badPtr.Valid target) hv

-- 21. access_success
/-- Instance (law `ConcreteSpec.Core.Mem.access_success`, case `interior`). Independent provenance, liveness, alignment and bounds obligations hold for an interior two-byte access. -/
example : memory.access target ptr 2 1 = some (some (⟨0⟩, ptr.addr - initBlock.base)) :=
  access_success target memory ptr 2 1 ⟨0⟩ initBlock (by decide) rfl rfl rfl rfl
    (by decide) (by decide) (by decide)

/-- Planted (law `ConcreteSpec.Core.Mem.access_success`, case `interior`, subject `ConcreteSpec.Core.Mem.Memory.access`, mutant `ConcreteSpec.Core.Mem.Audit.badAccessNone`). -/
example : (2 : Nat) ≠ 0 ∧ ptr.prov = some ⟨0⟩ ∧ memory.block ⟨0⟩ = some initBlock ∧
    initBlock.live = true ∧ initBlock.space = ptr.space ∧ initBlock.base ≤ ptr.addr ∧
    ptr.addr + 2 ≤ initBlock.base + initBlock.size ∧ ptr.addr % 1 = 0 ∧
    ¬ (badAccessNone target memory ptr 2 1 = some (some (⟨0⟩, ptr.addr - initBlock.base))) := by
  decide

-- 22. store_load
/-- Instance (law `ConcreteSpec.Core.Mem.store_load`, case `oneByte`). -/
example : storedMemory.load target ptr ([MByte.init 7 none]).length 1 =
    some [MByte.init 7 none] :=
  store_load target memory storedMemory memory_wf ptr [.init 7 none] 1 rfl

/-- Planted (law `ConcreteSpec.Core.Mem.store_load`, case `oneByte`, subject `ConcreteSpec.Core.Mem.Memory.load`, mutant `ConcreteSpec.Core.Mem.Audit.badLoadNone`). -/
example : memory.WellFormed target ∧
    memory.store target ptr [.init 7 none] 1 = some storedMemory ∧
    ¬ (badLoadNone target storedMemory ptr ([MByte.init 7 none]).length 1 =
      some [MByte.init 7 none]) :=
  ⟨memory_wf, rfl, by decide⟩

-- 23. store_other_block
/-- Instance (law `ConcreteSpec.Core.Mem.store_other_block`, case `otherBlock`). -/
example : (frameMemory.store target ptr [.init 7 none] 1).bind (fun m' => m'.block ⟨1⟩) =
    frameMemory.block ⟨1⟩ :=
  store_other_block target frameMemory ptr [.init 7 none] 1 ⟨0⟩ 0 ⟨1⟩ rfl (by decide)

/-- Planted (law `ConcreteSpec.Core.Mem.store_other_block`, case `otherBlock`, subject `ConcreteSpec.Core.Mem.Memory.store`, mutant `ConcreteSpec.Core.Mem.Audit.badStoreDup`). -/
example : frameMemory.access target ptr ([MByte.init 7 none]).length 1 = some (some (⟨0⟩, 0)) ∧
    (⟨1⟩ : MemBlockId) ≠ ⟨0⟩ ∧
    ¬ ((badStoreDup target frameMemory ptr [.init 7 none] 1).bind (fun m' => m'.block ⟨1⟩) =
      frameMemory.block ⟨1⟩) :=
  ⟨rfl, by decide, by intro h; cases h⟩

-- 24. store_same_block_frame
/-- Instance (law `ConcreteSpec.Core.Mem.store_same_block_frame`, case `afterSlice`). -/
example : ((memory.store target ptr [.init 7 none] 1).bind fun m' => m'.block ⟨0⟩).map
      (fun y => y.bytes[2]?) = some (initBlock.bytes[2]?) :=
  store_same_block_frame target memory memory_wf ptr [.init 7 none] 1 ⟨0⟩ 0 2 initBlock
    rfl rfl (.inr (by decide))

/-- Planted (law `ConcreteSpec.Core.Mem.store_same_block_frame`, case `afterSlice`, subject `ConcreteSpec.Core.Mem.Memory.store`, mutant `ConcreteSpec.Core.Mem.Audit.badStoreNone`). -/
example : memory.WellFormed target ∧
    memory.access target ptr ([MByte.init 7 none]).length 1 = some (some (⟨0⟩, 0)) ∧
    memory.block ⟨0⟩ = some initBlock ∧ (2 < 0 ∨ 0 + ([MByte.init 7 none]).length ≤ 2) ∧
    ¬ (((badStoreNone target memory ptr [.init 7 none] 1).bind fun m' => m'.block ⟨0⟩).map
      (fun y => y.bytes[2]?) = some (initBlock.bytes[2]?)) :=
  ⟨memory_wf, rfl, rfl, .inr (by decide), by decide⟩

-- 25. alloc_initialized
/-- Instance (law `ConcreteSpec.Core.Mem.alloc_initialized`, case `first`). -/
example : match Memory.empty.alloc target ⟨0⟩ 40 8 1 with
    | .ok (b, m') => ∃ x, m'.block b = some x ∧ x.live = true ∧ x.exposed = false ∧
        x.bytes = List.replicate 8 .uninit
    | .error _ => True :=
  alloc_initialized target Memory.empty ⟨0⟩ 40 8 1

/-- Planted (law `ConcreteSpec.Core.Mem.alloc_initialized`, case `first`, subject `ConcreteSpec.Core.Mem.Memory.alloc`, mutant `ConcreteSpec.Core.Mem.Audit.badAllocEmpty`). -/
example : ¬ (match badAllocEmpty target Memory.empty ⟨0⟩ 40 8 1 with
    | .ok (b, m') => ∃ x, m'.block b = some x ∧ x.live = true ∧ x.exposed = false ∧
        x.bytes = List.replicate 8 .uninit
    | .error _ => True) := by
  rintro ⟨x, hx, _⟩
  cases hx

-- 26. store_wellFormed
/-- Instance (law `ConcreteSpec.Core.Mem.store_wellFormed`, case `oneByte`). -/
example : match memory.store target ptr [.init 7 none] 1 with
    | some m' => m'.WellFormed target
    | none => True :=
  store_wellFormed target memory memory_wf ptr [.init 7 none] 1

/-- Planted (law `ConcreteSpec.Core.Mem.store_wellFormed`, case `oneByte`, subject `ConcreteSpec.Core.Mem.Memory.store`, mutant `ConcreteSpec.Core.Mem.Audit.badStoreWf`). -/
example : memory.WellFormed target ∧
    ¬ (match badStoreWf target memory ptr [.init 7 none] 1 with
      | some m' => m'.WellFormed target
      | none => True) :=
  ⟨memory_wf, not_badEmpty_wf⟩

-- 27. free_wellFormed
/-- Instance (law `ConcreteSpec.Core.Mem.free_wellFormed`, case `live`). -/
example : match memory.free ⟨0⟩ with
    | some m' => m'.WellFormed target
    | none => True :=
  free_wellFormed target memory memory_wf ⟨0⟩

/-- Planted (law `ConcreteSpec.Core.Mem.free_wellFormed`, case `live`, subject `ConcreteSpec.Core.Mem.Memory.free`, mutant `ConcreteSpec.Core.Mem.Audit.badFreeWf`). -/
example : memory.WellFormed target ∧
    ¬ (match badFreeWf memory ⟨0⟩ with
      | some m' => m'.WellFormed target
      | none => True) :=
  ⟨memory_wf, not_badEmpty_wf⟩

-- 28. expose_success
/-- Instance (law `ConcreteSpec.Core.Mem.expose_success`, case `live`). -/
example : memory.expose ptr = some (memory.modify ⟨0⟩ (fun y => { y with exposed := true })) :=
  expose_success memory ptr ⟨0⟩ initBlock rfl rfl rfl

/-- Planted (law `ConcreteSpec.Core.Mem.expose_success`, case `live`, subject `ConcreteSpec.Core.Mem.Memory.expose`, mutant `ConcreteSpec.Core.Mem.Audit.badExposeNone`). -/
example : ptr.prov = some ⟨0⟩ ∧ memory.block ⟨0⟩ = some initBlock ∧ initBlock.live = true ∧
    ¬ (badExposeNone memory ptr =
      some (memory.modify ⟨0⟩ (fun y => { y with exposed := true }))) :=
  ⟨rfl, rfl, rfl, by intro h; cases h⟩

-- 29. expose_wellFormed
/-- Instance (law `ConcreteSpec.Core.Mem.expose_wellFormed`, case `live`). -/
example : match memory.expose ptr with
    | some m' => m'.WellFormed target
    | none => True :=
  expose_wellFormed target memory memory_wf ptr

/-- Planted (law `ConcreteSpec.Core.Mem.expose_wellFormed`, case `live`, subject `ConcreteSpec.Core.Mem.Memory.expose`, mutant `ConcreteSpec.Core.Mem.Audit.badExposeWf`). -/
example : memory.WellFormed target ∧
    ¬ (match badExposeWf memory ptr with
      | some m' => m'.WellFormed target
      | none => True) :=
  ⟨memory_wf, not_badEmpty_wf⟩

-- 30. store_load_other
/-- Instance (law `ConcreteSpec.Core.Mem.store_load_other`, case `disjointProv`). -/
example : frameStoredMemory.load target frameOther 1 1 = frameMemory.load target frameOther 1 1 :=
  store_load_other target frameMemory frameStoredMemory ptr frameOther [.init 7 none] 1 1 1 rfl (by decide)

/-- Planted (law `ConcreteSpec.Core.Mem.store_load_other`, case `disjointProv`, subject `ConcreteSpec.Core.Mem.Memory.load`, mutant `ConcreteSpec.Core.Mem.Audit.badLoadDiff`). -/
example : frameMemory.store target ptr [.init 7 none] 1 = some frameStoredMemory ∧
    frameOther.prov ≠ ptr.prov ∧
    ¬ (badLoadDiff target frameStoredMemory frameOther 1 1 = badLoadDiff target frameMemory frameOther 1 1) :=
  ⟨rfl, by decide, by decide⟩

-- 31. expose_load
/-- Instance (law `ConcreteSpec.Core.Mem.expose_load`, case `samePtr`). -/
example : exposedMemory.load target ptr 1 1 = memory.load target ptr 1 1 :=
  expose_load target memory exposedMemory ptr ptr 1 1 rfl

/-- Planted (law `ConcreteSpec.Core.Mem.expose_load`, case `samePtr`, subject `ConcreteSpec.Core.Mem.Memory.load`, mutant `ConcreteSpec.Core.Mem.Audit.badLoadDiff`). -/
example : memory.expose ptr = some exposedMemory ∧
    ¬ (badLoadDiff target exposedMemory ptr 1 1 = badLoadDiff target memory ptr 1 1) :=
  ⟨rfl, by decide⟩

-- 32. load_prefix
/-- Instance (law `ConcreteSpec.Core.Mem.load_prefix`, case `twoToOne`). -/
example : memory.load target ptr 1 1 = some ([MByte.uninit, MByte.uninit].take 1) :=
  load_prefix target memory ptr 2 1 1 [.uninit, .uninit]
    (by decide) (by decide) (by decide) rfl

/-- Planted (law `ConcreteSpec.Core.Mem.load_prefix`, case `twoToOne`, subject `ConcreteSpec.Core.Mem.Memory.load`, mutant `ConcreteSpec.Core.Mem.Audit.badLoadPrefix`). -/
example : (2 : Nat) ≠ 0 ∧ (1 : Nat) ≠ 0 ∧ 1 ≤ 2 ∧
    badLoadPrefix target memory ptr 2 1 = some [.uninit, .uninit] ∧
    ¬ (badLoadPrefix target memory ptr 1 1 = some ([MByte.uninit, MByte.uninit].take 1)) := by
  decide

-- 33. free_alloc_ne_existing
/-- Instance (law `ConcreteSpec.Core.Mem.free_alloc_ne_existing`, case `reuseAddr`). -/
example : match freedMemory.alloc target ⟨0⟩ 40 8 1 with
    | .ok (new, _) => new ≠ ⟨0⟩
    | .error _ => True :=
  free_alloc_ne_existing target memory freedMemory memory_wf ⟨0⟩ ⟨0⟩ initBlock rfl rfl
    ⟨0⟩ 40 8 1

/-- Planted (law `ConcreteSpec.Core.Mem.free_alloc_ne_existing`, case `reuseAddr`, subject `ConcreteSpec.Core.Mem.Memory.alloc`, mutant `ConcreteSpec.Core.Mem.Audit.badAllocReuseZero`). -/
example : memory.WellFormed target ∧ memory.free ⟨0⟩ = some freedMemory ∧
    memory.block ⟨0⟩ = some initBlock ∧
    ¬ (match badAllocReuseZero target freedMemory ⟨0⟩ 40 8 1 with
      | .ok (new, _) => new ≠ ⟨0⟩
      | .error _ => True) :=
  ⟨memory_wf, rfl, rfl, fun h => h rfl⟩


def skipFree : Memory → MemBlockId → Option Memory := fun m _ => some m
def corruptExpose : Memory → Ptr → Option Memory := fun _ _ => some badEmpty
def corruptStoreGeometry : Target → Memory → Ptr → List MByte → Nat → Option Memory :=
  fun _ m _ _ _ => some (m.modify ⟨0⟩ fun x => { x with base := x.base + 1 })
def corruptExposeGeometry : Memory → Ptr → Option Memory :=
  fun m _ => some (m.modify ⟨0⟩ fun x => { x with size := x.size + 1 })
def wrongReconstruction : Memory → AddrSpace → Nat → Ptr :=
  fun _ s a => ⟨s, a, some ⟨1⟩⟩

/-- Instance (law `ConcreteSpec.Core.Mem.eqAddr_separation`, case `provenance`). -/
example : Ptr.eqAddr ptr rawPtr = false ↔ ptr.space ≠ rawPtr.space ∨
    ptr.addr < rawPtr.addr ∨ rawPtr.addr < ptr.addr := eqAddr_separation ptr rawPtr
/-- Planted (law `ConcreteSpec.Core.Mem.eqAddr_separation`, case `provenance`, subject `ConcreteSpec.Core.Mem.Ptr.eqAddr`, mutant `ConcreteSpec.Core.Mem.Audit.badEqAddr`). -/
example : ¬ (badEqAddr ptr rawPtr = false ↔ ptr.space ≠ rawPtr.space ∨
    ptr.addr < rawPtr.addr ∨ rawPtr.addr < ptr.addr) := by decide
def alwaysEqual : Ptr → Ptr → Bool := fun _ _ => true
/-- Instance (law `ConcreteSpec.Core.Mem.eqAddr_separation`, case `address`). -/
example : Ptr.eqAddr ptr other = false ↔ ptr.space ≠ other.space ∨
    ptr.addr < other.addr ∨ other.addr < ptr.addr := eqAddr_separation ptr other
/-- Planted (law `ConcreteSpec.Core.Mem.eqAddr_separation`, case `address`, subject `ConcreteSpec.Core.Mem.Ptr.eqAddr`, mutant `ConcreteSpec.Core.Mem.Audit.alwaysEqual`). -/
example : ¬ (alwaysEqual ptr other = false ↔ ptr.space ≠ other.space ∨
    ptr.addr < other.addr ∨ other.addr < ptr.addr) := by decide

/-- Instance (law `ConcreteSpec.Core.Mem.free_retains`, case `live`). -/
example : match memory.free ⟨0⟩ with | none => True | some m' => ∃ x, memory.block ⟨0⟩ = some x ∧ m'.block ⟨0⟩ = some { x with live := false, exposed := false } ∧ m'.next = memory.next :=
  free_retains memory ⟨0⟩
/-- Planted (law `ConcreteSpec.Core.Mem.free_retains`, case `live`, subject `ConcreteSpec.Core.Mem.Memory.free`, mutant `ConcreteSpec.Core.Mem.Audit.skipFree`). -/
example : ¬ (match skipFree memory ⟨0⟩ with | none => True | some m' => ∃ x, memory.block ⟨0⟩ = some x ∧ m'.block ⟨0⟩ = some { x with live := false, exposed := false } ∧ m'.next = memory.next) := by
  intro h
  obtain ⟨x, hx, hh, _⟩ := h
  have he : x = initBlock := Option.some.inj hx.symm
  subst x
  have hf := congrArg (fun z => z.map Block.live) hh
  cases hf

/-- Instance (law `ConcreteSpec.Core.Mem.expose_frame`, case `live`). -/
example : match memory.expose ptr with | none => True | some m' => ∃ b x, ptr.prov = some b ∧ memory.block b = some x ∧ m'.block b = some { x with exposed := true } ∧ m'.next = memory.next ∧ ∀ c, c ≠ b → m'.block c = memory.block c :=
  expose_frame memory ptr
/-- Planted (law `ConcreteSpec.Core.Mem.expose_frame`, case `live`, subject `ConcreteSpec.Core.Mem.Memory.expose`, mutant `ConcreteSpec.Core.Mem.Audit.corruptExpose`). -/
example : ¬ (match corruptExpose memory ptr with | none => True | some m' => ∃ b x, ptr.prov = some b ∧ memory.block b = some x ∧ m'.block b = some { x with exposed := true } ∧ m'.next = memory.next ∧ ∀ c, c ≠ b → m'.block c = memory.block c) := by
  intro h
  obtain ⟨b, x, hp, hx, _, hn, _⟩ := h
  have : (0 : Nat) = 1 := hn
  cases this

/-- Instance (law `ConcreteSpec.Core.Mem.store_geometry`, case `live`). -/
example : match memory.store target ptr [.init 7 none] 1 with | none => True | some m' => ∀ b, (m'.block b).map Block.geometry = (memory.block b).map Block.geometry :=
  store_geometry target memory ptr [.init 7 none] 1
/-- Planted (law `ConcreteSpec.Core.Mem.store_geometry`, case `live`, subject `ConcreteSpec.Core.Mem.Memory.store`, mutant `ConcreteSpec.Core.Mem.Audit.corruptStoreGeometry`). -/
example : ¬ (match corruptStoreGeometry target memory ptr [.init 7 none] 1 with | none => True | some m' => ∀ b, (m'.block b).map Block.geometry = (memory.block b).map Block.geometry) := by
  intro h
  have hh := h ⟨0⟩
  revert hh
  decide

/-- Instance (law `ConcreteSpec.Core.Mem.expose_geometry`, case `live`). -/
example : match memory.expose ptr with | none => True | some m' => ∀ b, (m'.block b).map Block.geometry = (memory.block b).map Block.geometry :=
  expose_geometry memory ptr
/-- Planted (law `ConcreteSpec.Core.Mem.expose_geometry`, case `live`, subject `ConcreteSpec.Core.Mem.Memory.expose`, mutant `ConcreteSpec.Core.Mem.Audit.corruptExposeGeometry`). -/
example : ¬ (match corruptExposeGeometry memory ptr with | none => True | some m' => ∀ b, (m'.block b).map Block.geometry = (memory.block b).map Block.geometry) := by
  intro h
  have hh := h ⟨0⟩
  revert hh
  decide

/-- Instance (law `ConcreteSpec.Core.Mem.fromExposed_sound`, case `interior`). -/
example : ∀ b, (exposedMemory.fromExposed ⟨0⟩ 42).prov = some b →
    ∃ x, exposedMemory.block b = some x ∧ x.Candidate ⟨0⟩ = true ∧
      ((x.base ≤ 42 ∧ 42 < x.base + x.size) ∨
       (42 = x.base + x.size ∧ ∀ e ∈ exposedMemory.blocks, e.2.Candidate ⟨0⟩ = true →
         ¬ (e.2.base ≤ 42 ∧ 42 < e.2.base + e.2.size))) := fromExposed_sound target exposedMemory exposedMemory_wf ⟨0⟩ 42
/-- Planted (law `ConcreteSpec.Core.Mem.fromExposed_sound`, case `interior`, subject `ConcreteSpec.Core.Mem.Memory.fromExposed`, mutant `ConcreteSpec.Core.Mem.Audit.wrongReconstruction`). -/
example : exposedMemory.WellFormed target ∧ ¬ (∀ b, (wrongReconstruction exposedMemory ⟨0⟩ 42).prov = some b →
    ∃ x, exposedMemory.block b = some x ∧ x.Candidate ⟨0⟩ = true ∧
      ((x.base ≤ 42 ∧ 42 < x.base + x.size) ∨
       (42 = x.base + x.size ∧ ∀ e ∈ exposedMemory.blocks, e.2.Candidate ⟨0⟩ = true →
         ¬ (e.2.base ≤ 42 ∧ 42 < e.2.base + e.2.size)))) := by
  refine ⟨exposedMemory_wf, ?_⟩
  intro h
  obtain ⟨x, hx, _⟩ := h ⟨1⟩ rfl
  cases hx

/-- Accepted (rule C4). Framing observes initialized bytes in a second live allocation. -/
example : frameMemory.load target frameOther 2 1 = some [.init 9 none, .init 10 none] ∧
    frameStoredMemory.load target frameOther 2 1 = some [.init 9 none, .init 10 none] := by decide

/-- Instance (law `ConcreteSpec.Core.Mem.eqAddr_separation`, case `space`). -/
example : Ptr.eqAddr ptr ⟨⟨1⟩, 40, none⟩ = false ↔ ptr.space ≠ (⟨1⟩ : AddrSpace) ∨
    ptr.addr < 40 ∨ 40 < ptr.addr := eqAddr_separation ptr ⟨⟨1⟩, 40, none⟩
/-- Planted (law `ConcreteSpec.Core.Mem.eqAddr_separation`, case `space`, subject `ConcreteSpec.Core.Mem.Ptr.eqAddr`, mutant `ConcreteSpec.Core.Mem.Audit.alwaysEqual`). -/
example : ¬ (alwaysEqual ptr ⟨⟨1⟩, 40, none⟩ = false ↔ ptr.space ≠ (⟨1⟩ : AddrSpace) ∨
    ptr.addr < 40 ∨ 40 < ptr.addr) := by decide

end Audit

end ConcreteSpec.Core.Mem
