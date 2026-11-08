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
    decodePtr tgt (encodePtr tgt p hpv) = some p := by
  have hp : 0 < tgt.ptrSize := by have := ht.1; have := ht.2.1; omega
  have hl : (ordered tgt.byteOrder (leBytes tgt.ptrSize p.addr)).length > 0 := by simpa using hp
  cases hs : ordered tgt.byteOrder (leBytes tgt.ptrSize p.addr) with
  | nil => simp [hs] at hl
  | cons v vs =>
    have he : encodePtr tgt p hpv = .init v (some (p, 0)) ::
        (vs.zipIdx 1).map (fun (v, i) => .init v (some (p, i))) := by
      simp [encodePtr, hs, List.zipIdx_cons]
    rw [he]
    simp only [decodePtr]
    rw [← he]
    simp [hpv]

@[simp] theorem block_modify (m : Memory) (b c : MemBlockId) (f : Block → Block) :
    (m.modify b f).block c = if c = b then (m.block b).map f else m.block c := by
  exact AList.lookup_update b f m.blocks c

/-- Empty memory satisfies every target's memory invariant. -/
theorem empty_wellFormed (tgt : Target) : Memory.empty.WellFormed tgt := by
  constructor <;> simp [Memory.empty, Memory.block, AList.lookup]

private theorem keys_update (xs : AList MemBlockId Block) (b : MemBlockId) (f : Block → Block) :
    List.map Prod.fst (AList.update xs b f) = List.map Prod.fst xs := by
  induction xs with
  | nil => rfl
  | cons e xs ih =>
    simp only [AList.update]
    split <;> simp_all

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

/-- A block update preserving geometry and byte count, and never reviving a dead block,
preserves the whole memory invariant. Exposure and byte contents are otherwise unrestricted. -/
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

/-- An eligible containing or one-past candidate suffices to recover some provenance. -/
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

/-- Completeness at the containing-block priority; no list-order tie-break enters the result. -/
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

/-- Nonempty disjoint blocks in one space cannot share their one-past endpoint either. -/
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

/-- In the absence of any containing candidate, a one-past candidate is recovered exactly. -/
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

/-- If neither priority has a candidate, reconstruction supplies no provenance. -/
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

/-- Every successful access, including a zero-byte one, uses a target-valid pointer. -/
theorem access_valid (tgt : Target) (m : Memory) (p : Ptr) (n a : Nat)
    (r : Option (MemBlockId × Nat)) (h : m.access tgt p n a = some r) : p.Valid tgt := by
  unfold Memory.access at h
  split at h
  · split at h
    next hv => exact hv.2.2
    next => cases h
  · split at h
    next => cases h
    next =>
      split at h
      next => cases h
      next =>
        split at h
        next hv => exact hv.2.2.2.2.2
        next => cases h

/-- Every decoded whole stored pointer is target-valid. Other fragment patterns remain
without pointer meaning in this draft. -/
theorem decodePtr_valid (tgt : Target) (bs : List MByte) (p : Ptr)
    (h : decodePtr tgt bs = some p) : p.Valid tgt := by
  match bs with
  | .init _ (some (q, 0)) :: _ =>
    simp only [decodePtr] at h
    split at h
    next hp =>
      split at h
      next => cases h; exact hp
      next => cases h
    next => cases h
  | [] | .uninit :: _ | .init _ none :: _ | .init _ (some (_, Nat.succ _)) :: _ => cases h

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

/-- Successful byte stores preserve block geometry, liveness and well-formedness. -/
theorem store_wellFormed (tgt : Target) (m m' : Memory) (hw : m.WellFormed tgt)
    (p : Ptr) (bs : List MByte) (a : Nat) (h : m.store tgt p bs a = some m') :
    m'.WellFormed tgt := by
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
      obtain ⟨x, _, hx, _, _, _, hlo, hhi, _, hoff, _⟩ :=
        access_some tgt m p bs.length a b off ha
      apply modify_wellFormed tgt m hw b
      · intro y; exact ⟨rfl, rfl, rfl, rfl⟩
      · intro y hy
        rw [hx] at hy; cases hy
        have := hw.sized b x hx
        simp only [List.length_append, List.length_take, List.length_drop]
        omega
      · intro y hy; exact hy

/-- Successful release preserves the invariant without deleting the identity. -/
theorem free_wellFormed (tgt : Target) (m m' : Memory) (hw : m.WellFormed tgt)
    (b : MemBlockId) (h : m.free b = some m') : m'.WellFormed tgt := by
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

/-- Exposure changes exactly the exposed flag of a live provenance block. -/
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

/-- Conversely every live provenance block can be exposed, preserving all other fields. -/
theorem expose_success (m : Memory) (p : Ptr) (b : MemBlockId) (x : Block)
    (hp : p.prov = some b) (hx : m.block b = some x) (hl : x.live = true) :
    m.expose p = some (m.modify b (fun y => { y with exposed := true })) := by
  simp [Memory.expose, hp, hx, hl]

/-- The exposed block has exactly its old fields except the exposed flag; other blocks and
freshness counter are unchanged. -/
theorem expose_frame (m m' : Memory) (p : Ptr) (h : m.expose p = some m') :
    ∃ b x, p.prov = some b ∧ m.block b = some x ∧
      m'.block b = some { x with exposed := true } ∧ m'.next = m.next ∧
      ∀ c, c ≠ b → m'.block c = m.block c := by
  obtain ⟨b, x, hp, hx, _, rfl⟩ := expose_result m m' p h
  exact ⟨b, x, hp, hx, by simp [hx], rfl, by intro c hc; simp [hc]⟩

/-- Marking a block exposed preserves well-formedness. -/
theorem expose_wellFormed (tgt : Target) (m m' : Memory) (hw : m.WellFormed tgt)
    (p : Ptr) (h : m.expose p = some m') : m'.WellFormed tgt := by
  obtain ⟨b, _, _, _, _, rfl⟩ := expose_result m m' p h
  apply modify_wellFormed tgt m hw b
  · intro y; exact ⟨rfl, rfl, rfl, rfl⟩
  · intro y hy; exact hw.sized b y hy
  · intro y hy; exact hy

/-- Geometry relevant to hybrid projections; neither byte contents nor exposure is cached. -/
def Block.geometry (x : Block) : AddrSpace × Nat × Nat × Nat :=
  (x.space, x.base, x.size, x.align)

/-- Store does not change any block's projection geometry. -/
theorem store_geometry (tgt : Target) (m m' : Memory) (p : Ptr) (bs : List MByte)
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

/-- Exposure also preserves every projection's geometry. -/
theorem expose_geometry (m m' : Memory) (p : Ptr) (h : m.expose p = some m')
    (b : MemBlockId) :
    (m'.block b).map Block.geometry = (m.block b).map Block.geometry := by
  obtain ⟨c, _, _, _, _, rfl⟩ := expose_result m m' p h
  simp only [block_modify]
  split
  · next he =>
      subst b
      cases m.block _ <;> rfl
  · rfl

/-- Reads depend only on their provenance block, independently of every other block. -/
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

/-- A store frames reads using a different allocation identity. -/
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
      obtain ⟨_, hp, _⟩ := access_some tgt m p bs.length a b off ha
      apply load_congr
      intro c hc
      have hcb : c ≠ b := by intro he; subst c; exact hn (hc.trans hp.symm)
      simp [hcb]

/-- Exposure affects provenance reconstruction, but no byte read or access check. -/
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

/-- A successful nonempty read can be shortened to any nonempty prefix. -/
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
      obtain ⟨x, hp, hx, _, hl, hs, hlo, hhi, hal, hoff, hpv⟩ :=
        access_some tgt m p n a b off ha
      simp only [hx, Option.map_some, Option.some.injEq] at h
      subst bs
      have hb : p.addr + k ≤ x.base + x.size := by omega
      simp [Memory.load, Memory.access, hk, hp, hx, hl, hs, hlo, hb, hal, hpv, hoff,
        List.take_take, Nat.min_eq_left hkn]

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
