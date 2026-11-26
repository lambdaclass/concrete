import ConcreteSpec.Spec.Basics
import ConcreteSpec.Spec.Ident
import ConcreteSpec.Spec.Memory
import ConcreteSpec.Spec.Rep
import ConcreteSpec.Spec.Scalar
import ConcreteSpec.Spec.Slice1Syntax
import ConcreteSpec.Spec.Target
import ConcreteSpec.Proofs.Memory

namespace ConcreteSpec.Core.Mem

/-- Representation application. The checked view covers a negative signed value. -/
theorem example_Representation_application_checked_view_covers_negative : (encodeInt linuxX8664 .i16 (-2)).map sliceByte =
    ConcreteSpec.Rep.encodeScalar (.int .i16 (-2)) := encodeInt_rep .i16 (-2)

namespace Audit

def target : Target :=
  { linuxX8664 with addrBits := 8, ptrSize := 2, ptrAlign := 1, maxObject := 250 }
def ptr : Ptr := ⟨⟨0⟩, 40, some ⟨0⟩⟩
def other : Ptr := ⟨⟨0⟩, 41, some ⟨1⟩⟩
def badPtr : Ptr := ⟨⟨0⟩, 256, some ⟨0⟩⟩
def nullPtr : Ptr := ⟨⟨0⟩, 0, none⟩
def rawPtr : Ptr := ⟨⟨0⟩, 40, none⟩
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


/-- Fault: signed octets are interpreted as unsigned. -/
def badDecodeInt (tgt : Target) (t : IntTy) (bs : List MByte) : Option Int :=
  if bs.length = intBytes t then
    (octets bs).map fun os => (leValue (ordered tgt.byteOrder os) : Int)
  else none
/-- The same missing sign interpretation also violates the signed range. -/
def badDecodeIntRange := badDecodeInt
/-- Fault: magnitude replaces the two's-complement pattern. -/
def badEncodeInt (tgt : Target) (t : IntTy) (n : Int) : List MByte :=
  (ordered tgt.byteOrder (leBytes (intBytes t) n.natAbs)).map (.init · none)
def badEmpty : Memory := ⟨[(⟨1⟩, initBlock)], 0⟩

def badAllocId : Target → Memory → AddrSpace → Nat → Nat → Nat →
    Except AllocError (BlockId × Memory) :=
  fun _ m s base size align =>
    .ok (⟨m.next + 1⟩, ⟨(⟨m.next + 1⟩, candidate s base size align) :: m.blocks, m.next + 1⟩)

def badAllocEmpty : Target → Memory → AddrSpace → Nat → Nat → Nat →
    Except AllocError (BlockId × Memory) :=
  fun _ m _ _ _ _ => .ok (⟨m.next⟩, m)

def badAllocReuseZero : Target → Memory → AddrSpace → Nat → Nat → Nat →
    Except AllocError (BlockId × Memory) :=
  fun _ m _ _ _ _ => .ok (⟨0⟩, m)

/-- Deliberately wrong allocator for planted checks: skip exactly one geometry obligation.
0 skips power-of-two alignment, 1 skips base alignment, 2 skips the address boundary,
3 skips maxObject, 4 skips the non-null base check, 5 skips live overlap,
6 skips address-space membership. -/
def mutatedAlloc (skip : Nat) (tgt : Target) (m : Memory) (s : AddrSpace)
    (base size align : Nat) : Except AllocError (BlockId × Memory) :=
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
    Except AllocError (BlockId × Memory) := mutatedAlloc 0
def mutatedAllocAlign : Target → Memory → AddrSpace → Nat → Nat → Nat →
    Except AllocError (BlockId × Memory) := mutatedAlloc 1
def mutatedAllocBound : Target → Memory → AddrSpace → Nat → Nat → Nat →
    Except AllocError (BlockId × Memory) := mutatedAlloc 2
def mutatedAllocMaxObj : Target → Memory → AddrSpace → Nat → Nat → Nat →
    Except AllocError (BlockId × Memory) := mutatedAlloc 3
def mutatedAllocNull : Target → Memory → AddrSpace → Nat → Nat → Nat →
    Except AllocError (BlockId × Memory) := mutatedAlloc 4
def mutatedAllocOverlap : Target → Memory → AddrSpace → Nat → Nat → Nat →
    Except AllocError (BlockId × Memory) := mutatedAlloc 5
def mutatedAllocSpace : Target → Memory → AddrSpace → Nat → Nat → Nat →
    Except AllocError (BlockId × Memory) := mutatedAlloc 6

def badLoadNone : Target → Memory → Ptr → Nat → Nat → Option (List MByte) :=
  fun tgt m p n a => if n < 2 then none else m.load tgt p n a
/-- Fault: a load serializes the block table, including exposure metadata, rather than its slice. -/
def badLoadDiff : Target → Memory → Ptr → Nat → Nat → Option (List MByte) :=
  fun _ m _ _ _ => some (m.blocks.flatMap fun e =>
    .init (if e.2.exposed then 1 else 0) none :: e.2.bytes)
def badLoadPrefix : Target → Memory → Ptr → Nat → Nat → Option (List MByte) :=
  fun tgt m p sz a => if sz % 2 = 0 then m.load tgt p sz a else none
def badStoreNone : Target → Memory → Ptr → List MByte → Nat → Option Memory :=
  fun tgt m p bs a => if bs.length < 2 then none else m.store tgt p bs a
def badStoreDup : Target → Memory → Ptr → List MByte → Nat → Option Memory :=
  fun tgt m p bs a => (m.store tgt p bs a).map fun m' =>
    m'.modify ⟨1⟩ fun x => { x with bytes := bs ++ x.bytes }
def badStoreWf : Target → Memory → Ptr → List MByte → Nat → Option Memory :=
  fun tgt m p bs a => (m.store tgt p bs a).map fun m' => { m' with next := 0 }
def badAccessSome : Target → Memory → Ptr → Nat → Nat → Option (Option (BlockId × Nat)) :=
  fun tgt m p _ a => m.access tgt p 0 a
def badAccessPair : Target → Memory → Ptr → Nat → Nat → Option (Option (BlockId × Nat)) :=
  fun _ m p _ a => do
    let b ← p.prov
    let x ← m.block b
    if x.live = true ∧ x.space = p.space ∧ p.addr % a = 0 then
      some (some (b, p.addr - x.base)) else none
def badAccessNone : Target → Memory → Ptr → Nat → Nat → Option (Option (BlockId × Nat)) :=
  fun tgt m p n a => m.access tgt p (n + 2 ^ tgt.addrBits) a
def badEqAddr : Ptr → Ptr → Bool := fun p q => p == q
def badFromExposedNone : Memory → AddrSpace → Nat → Ptr := fun _ s addr => ⟨s, addr, none⟩
/-- Fault: attach the first stored block without checking its address range. -/
def badFromExposedSome : Memory → AddrSpace → Nat → Ptr :=
  fun m s addr => ⟨s, addr, (m.blocks.head?).map Prod.fst⟩
def badFreeWf : Memory → BlockId → Option Memory :=
  fun m b => (m.free b).map fun m' => { m' with next := 0 }
/-- Fault: exposure wrongly requires the block to have been exposed already. -/
def badExposeNone (m : Memory) (p : Ptr) : Option Memory := do
  let b ← p.prov
  let x ← m.block b
  if x.exposed then m.expose p else none
def badExposeWf : Memory → Ptr → Option Memory :=
  fun m p => (m.expose p).map fun m' => { m' with next := 0 }

/-- Helper (inversion). The out-of-range block identity in badEmpty violates the freshness invariant. -/
private theorem not_badEmpty_wf : ¬ badEmpty.WellFormed target := by
  intro hw
  have h := hw.fresh ⟨1⟩ initBlock rfl
  cases h

/-- Helper (congruence). The audit fixture memory satisfies the target memory invariant. -/
private theorem memory_wf : memory.WellFormed target :=
  alloc_wellFormed target Memory.empty (empty_wellFormed target) ⟨0⟩ 40 8 1

/-- Helper (congruence). The exposed audit fixture memory satisfies the target memory invariant. -/
private theorem exposedMemory_wf : exposedMemory.WellFormed target :=
  expose_wellFormed target memory memory_wf ptr

-- 1. decodeInt_encodeInt
/-- Instance (law `ConcreteSpec.Core.Mem.decodeInt_encodeInt`, case `i8`). -/
theorem decodeInt_encodeInt_example_i8 : decodeInt target .i8 (encodeInt target .i8 (-5)) = some (-5) :=
  decodeInt_encodeInt target .i8 (-5) (by decide)

/-- Planted (law `ConcreteSpec.Core.Mem.decodeInt_encodeInt`, case `i8`, subject `ConcreteSpec.Core.Mem.decodeInt`, mutant `ConcreteSpec.Core.Mem.Audit.badDecodeInt`). -/
theorem decodeInt_encodeInt_planted_i8_badDecodeInt : IntTy.i8.InRange (-5) ∧
    ¬ (badDecodeInt target .i8 (encodeInt target .i8 (-5)) = some (-5)) := by decide

/-- Planted (law `ConcreteSpec.Core.Mem.decodeInt_encodeInt`, case `i8`, subject `ConcreteSpec.Core.Mem.encodeInt`, mutant `ConcreteSpec.Core.Mem.Audit.badEncodeInt`). -/
theorem decodeInt_encodeInt_planted_i8_badEncodeInt : IntTy.i8.InRange (-5) ∧
    ¬ (decodeInt target .i8 (badEncodeInt target .i8 (-5)) = some (-5)) := by decide

-- 2. decodeInt_inRange
/-- Instance (law `ConcreteSpec.Core.Mem.decodeInt_inRange`, case `i8`). -/
theorem decodeInt_inRange_example_i8 : match decodeInt target .i8 [.init 251 none] with
    | some n => IntTy.i8.InRange n
    | none => True :=
  decodeInt_inRange target .i8 [.init 251 none]

/-- Planted (law `ConcreteSpec.Core.Mem.decodeInt_inRange`, case `i8`, subject `ConcreteSpec.Core.Mem.decodeInt`, mutant `ConcreteSpec.Core.Mem.Audit.badDecodeIntRange`). -/
theorem decodeInt_inRange_planted_i8_badDecodeIntRange : ¬ (match badDecodeIntRange target .i8 [.init 251 none] with
    | some n => IntTy.i8.InRange n
    | none => True) := by
  change ¬ IntTy.i8.InRange 251
  decide

-- 7. empty_wellFormed
/-- Instance (law `ConcreteSpec.Core.Mem.empty_wellFormed`, case `default`). -/
theorem empty_wellFormed_example_default : Memory.empty.WellFormed target :=
  empty_wellFormed target

/-- Planted (law `ConcreteSpec.Core.Mem.empty_wellFormed`, case `default`, subject `ConcreteSpec.Core.Mem.Memory.empty`, mutant `ConcreteSpec.Core.Mem.Audit.badEmpty`). -/
theorem empty_wellFormed_planted_default_badEmpty : ¬ badEmpty.WellFormed target := not_badEmpty_wf

-- 8. alloc_fresh
/-- Instance (law `ConcreteSpec.Core.Mem.alloc_fresh`, case `first`). -/
theorem alloc_fresh_example_first : match Memory.empty.alloc target ⟨0⟩ 40 8 1 with
    | .ok (b, _) => b.id = Memory.empty.next ∧ Memory.empty.block b = none
    | .error _ => True :=
  alloc_fresh target Memory.empty (empty_wellFormed target) ⟨0⟩ 40 8 1

/-- Planted (law `ConcreteSpec.Core.Mem.alloc_fresh`, case `first`, subject `ConcreteSpec.Core.Mem.Memory.alloc`, mutant `ConcreteSpec.Core.Mem.Audit.badAllocId`). -/
theorem alloc_fresh_planted_first_badAllocId : Memory.empty.WellFormed target ∧
    ¬ (match badAllocId target Memory.empty ⟨0⟩ 40 8 1 with
      | .ok (b, _) => b.id = Memory.empty.next ∧ Memory.empty.block b = none
      | .error _ => True) :=
  ⟨empty_wellFormed target, by rintro ⟨h, _⟩; cases h⟩

-- 9. alloc_block
/-- Instance (law `ConcreteSpec.Core.Mem.alloc_block`, case `first`). -/
theorem alloc_block_example_first : match Memory.empty.alloc target ⟨0⟩ 40 8 1 with
    | .ok (b, m') => m'.block b = some (candidate ⟨0⟩ 40 8 1)
    | .error _ => True :=
  alloc_block target Memory.empty ⟨0⟩ 40 8 1

/-- Planted (law `ConcreteSpec.Core.Mem.alloc_block`, case `first`, subject `ConcreteSpec.Core.Mem.Memory.alloc`, mutant `ConcreteSpec.Core.Mem.Audit.badAllocEmpty`). -/
theorem alloc_block_planted_first_badAllocEmpty : ¬ (match badAllocEmpty target Memory.empty ⟨0⟩ 40 8 1 with
    | .ok (b, m') => m'.block b = some (candidate ⟨0⟩ 40 8 1)
    | .error _ => True) := by
  intro h; cases h

-- 10. alloc_failure (all 7 failure branches)
/-- Instance (law `ConcreteSpec.Core.Mem.alloc_failure`, case `pow2`). -/
theorem alloc_failure_example_pow2 : Memory.empty.alloc target ⟨0⟩ 42 2 3 = .error .unavailable :=
  alloc_failure target Memory.empty ⟨0⟩ 42 2 3 (.inl (by decide))

/-- Planted (law `ConcreteSpec.Core.Mem.alloc_failure`, case `pow2`, subject `ConcreteSpec.Core.Mem.Memory.alloc`, mutant `ConcreteSpec.Core.Mem.Audit.mutatedAllocPow2`). -/
theorem alloc_failure_planted_pow2_mutatedAllocPow2 : (¬ IsPow2 3 ∨ 42 % 3 ≠ 0 ∨ 2 ^ target.addrBits - 1 < 42 + 2 ∨
      target.maxObject < 2 ∨ (0 < 2 ∧ 42 = target.null) ∨
      (∃ e ∈ Memory.empty.blocks, e.2.live = true ∧ e.2.Overlaps (candidate ⟨0⟩ 42 2 3)) ∨
      (⟨0⟩ : AddrSpace) ∉ target.spaces) ∧
    ¬ (mutatedAllocPow2 target Memory.empty ⟨0⟩ 42 2 3 = .error .unavailable) :=
  ⟨.inl (by decide), by intro h; cases h⟩

/-- Instance (law `ConcreteSpec.Core.Mem.alloc_failure`, case `align`). -/
theorem alloc_failure_example_align : Memory.empty.alloc target ⟨0⟩ 41 2 2 = .error .unavailable :=
  alloc_failure target Memory.empty ⟨0⟩ 41 2 2 (.inr (.inl (by decide)))

/-- Planted (law `ConcreteSpec.Core.Mem.alloc_failure`, case `align`, subject `ConcreteSpec.Core.Mem.Memory.alloc`, mutant `ConcreteSpec.Core.Mem.Audit.mutatedAllocAlign`). -/
theorem alloc_failure_planted_align_mutatedAllocAlign : (¬ IsPow2 2 ∨ 41 % 2 ≠ 0 ∨ 2 ^ target.addrBits - 1 < 41 + 2 ∨
      target.maxObject < 2 ∨ (0 < 2 ∧ 41 = target.null) ∨
      (∃ e ∈ Memory.empty.blocks, e.2.live = true ∧ e.2.Overlaps (candidate ⟨0⟩ 41 2 2)) ∨
      (⟨0⟩ : AddrSpace) ∉ target.spaces) ∧
    ¬ (mutatedAllocAlign target Memory.empty ⟨0⟩ 41 2 2 = .error .unavailable) :=
  ⟨.inr (.inl (by decide)), by intro h; cases h⟩

/-- Instance (law `ConcreteSpec.Core.Mem.alloc_failure`, case `bound`). -/
theorem alloc_failure_example_bound : Memory.empty.alloc target ⟨0⟩ 254 2 1 = .error .unavailable :=
  alloc_failure target Memory.empty ⟨0⟩ 254 2 1 (.inr (.inr (.inl (by decide))))

/-- Planted (law `ConcreteSpec.Core.Mem.alloc_failure`, case `bound`, subject `ConcreteSpec.Core.Mem.Memory.alloc`, mutant `ConcreteSpec.Core.Mem.Audit.mutatedAllocBound`). -/
theorem alloc_failure_planted_bound_mutatedAllocBound : (¬ IsPow2 1 ∨ 254 % 1 ≠ 0 ∨ 2 ^ target.addrBits - 1 < 254 + 2 ∨
      target.maxObject < 2 ∨ (0 < 2 ∧ 254 = target.null) ∨
      (∃ e ∈ Memory.empty.blocks, e.2.live = true ∧ e.2.Overlaps (candidate ⟨0⟩ 254 2 1)) ∨
      (⟨0⟩ : AddrSpace) ∉ target.spaces) ∧
    ¬ (mutatedAllocBound target Memory.empty ⟨0⟩ 254 2 1 = .error .unavailable) :=
  ⟨.inr (.inr (.inl (by decide))), by intro h; cases h⟩

/-- Instance (law `ConcreteSpec.Core.Mem.alloc_failure`, case `maxObject`). -/
theorem alloc_failure_example_maxObject : Memory.empty.alloc target ⟨0⟩ 1 251 1 = .error .unavailable :=
  alloc_failure target Memory.empty ⟨0⟩ 1 251 1 (.inr (.inr (.inr (.inl (by decide)))))

/-- Planted (law `ConcreteSpec.Core.Mem.alloc_failure`, case `maxObject`, subject `ConcreteSpec.Core.Mem.Memory.alloc`, mutant `ConcreteSpec.Core.Mem.Audit.mutatedAllocMaxObj`). -/
theorem alloc_failure_planted_maxObject_mutatedAllocMaxObj : (¬ IsPow2 1 ∨ 1 % 1 ≠ 0 ∨ 2 ^ target.addrBits - 1 < 1 + 251 ∨
      target.maxObject < 251 ∨ (0 < 251 ∧ 1 = target.null) ∨
      (∃ e ∈ Memory.empty.blocks, e.2.live = true ∧ e.2.Overlaps (candidate ⟨0⟩ 1 251 1)) ∨
      (⟨0⟩ : AddrSpace) ∉ target.spaces) ∧
    ¬ (mutatedAllocMaxObj target Memory.empty ⟨0⟩ 1 251 1 = .error .unavailable) :=
  ⟨.inr (.inr (.inr (.inl (by decide)))), by intro h; cases h⟩

/-- Instance (law `ConcreteSpec.Core.Mem.alloc_failure`, case `null`). -/
theorem alloc_failure_example_null : Memory.empty.alloc target ⟨0⟩ 0 2 1 = .error .unavailable :=
  alloc_failure target Memory.empty ⟨0⟩ 0 2 1 (.inr (.inr (.inr (.inr (.inl (by decide))))))

/-- Planted (law `ConcreteSpec.Core.Mem.alloc_failure`, case `null`, subject `ConcreteSpec.Core.Mem.Memory.alloc`, mutant `ConcreteSpec.Core.Mem.Audit.mutatedAllocNull`). -/
theorem alloc_failure_planted_null_mutatedAllocNull : (¬ IsPow2 1 ∨ 0 % 1 ≠ 0 ∨ 2 ^ target.addrBits - 1 < 0 + 2 ∨
      target.maxObject < 2 ∨ (0 < 2 ∧ 0 = target.null) ∨
      (∃ e ∈ Memory.empty.blocks, e.2.live = true ∧ e.2.Overlaps (candidate ⟨0⟩ 0 2 1)) ∨
      (⟨0⟩ : AddrSpace) ∉ target.spaces) ∧
    ¬ (mutatedAllocNull target Memory.empty ⟨0⟩ 0 2 1 = .error .unavailable) :=
  ⟨.inr (.inr (.inr (.inr (.inl (by decide))))), by intro h; cases h⟩

/-- Instance (law `ConcreteSpec.Core.Mem.alloc_failure`, case `overlap`). -/
theorem alloc_failure_example_overlap : memory.alloc target ⟨0⟩ 44 2 1 = .error .unavailable :=
  alloc_failure target memory ⟨0⟩ 44 2 1 (.inr (.inr (.inr (.inr (.inr (.inl (by decide)))))))

/-- Planted (law `ConcreteSpec.Core.Mem.alloc_failure`, case `overlap`, subject `ConcreteSpec.Core.Mem.Memory.alloc`, mutant `ConcreteSpec.Core.Mem.Audit.mutatedAllocOverlap`). -/
theorem alloc_failure_planted_overlap_mutatedAllocOverlap : (¬ IsPow2 1 ∨ 44 % 1 ≠ 0 ∨ 2 ^ target.addrBits - 1 < 44 + 2 ∨
      target.maxObject < 2 ∨ (0 < 2 ∧ 44 = target.null) ∨
      (∃ e ∈ memory.blocks, e.2.live = true ∧ e.2.Overlaps (candidate ⟨0⟩ 44 2 1)) ∨
      (⟨0⟩ : AddrSpace) ∉ target.spaces) ∧
    ¬ (mutatedAllocOverlap target memory ⟨0⟩ 44 2 1 = .error .unavailable) :=
  ⟨.inr (.inr (.inr (.inr (.inr (.inl (by decide)))))), by intro h; cases h⟩

/-- Instance (law `ConcreteSpec.Core.Mem.alloc_failure`, case `space`). -/
theorem alloc_failure_example_space : Memory.empty.alloc target ⟨1⟩ 40 2 1 = .error .unavailable :=
  alloc_failure target Memory.empty ⟨1⟩ 40 2 1 (.inr (.inr (.inr (.inr (.inr (.inr (by decide)))))))

/-- Planted (law `ConcreteSpec.Core.Mem.alloc_failure`, case `space`, subject `ConcreteSpec.Core.Mem.Memory.alloc`, mutant `ConcreteSpec.Core.Mem.Audit.mutatedAllocSpace`). -/
theorem alloc_failure_planted_space_mutatedAllocSpace : (¬ IsPow2 1 ∨ 40 % 1 ≠ 0 ∨ 2 ^ target.addrBits - 1 < 40 + 2 ∨
      target.maxObject < 2 ∨ (0 < 2 ∧ 40 = target.null) ∨
      (∃ e ∈ Memory.empty.blocks, e.2.live = true ∧ e.2.Overlaps (candidate ⟨1⟩ 40 2 1)) ∨
      (⟨1⟩ : AddrSpace) ∉ target.spaces) ∧
    ¬ (mutatedAllocSpace target Memory.empty ⟨1⟩ 40 2 1 = .error .unavailable) :=
  ⟨.inr (.inr (.inr (.inr (.inr (.inr (by decide)))))), by intro h; cases h⟩

-- 11. access_zero
/-- Instance (law `ConcreteSpec.Core.Mem.access_zero`, case `alignedNonNull`). Accepted: zero-size load/store need no provenance, and the empty store preserves the complete memory. -/
theorem access_zero_example_alignedNonNull : (Memory.empty.load target rawPtr 0 2 = some [] ∧
      Memory.empty.store target rawPtr [] 2 = some Memory.empty) ↔
    rawPtr.addr % 2 = 0 ∧ rawPtr.addr ≠ target.null ∧ rawPtr.Valid target :=
  access_zero target Memory.empty rawPtr 2

/-- Planted (law `ConcreteSpec.Core.Mem.access_zero`, case `alignedNonNull`, subject `ConcreteSpec.Core.Mem.Memory.load`, mutant `ConcreteSpec.Core.Mem.Audit.badLoadNone`). -/
theorem access_zero_planted_alignedNonNull_badLoadNone : ¬ ((badLoadNone target Memory.empty rawPtr 0 2 = some [] ∧
      Memory.empty.store target rawPtr [] 2 = some Memory.empty) ↔
    rawPtr.addr % 2 = 0 ∧ rawPtr.addr ≠ target.null ∧ rawPtr.Valid target) := by
  intro h
  have hh := (h.mpr (by decide)).1
  cases hh

/-- Planted (law `ConcreteSpec.Core.Mem.access_zero`, case `alignedNonNull`, subject `ConcreteSpec.Core.Mem.Memory.store`, mutant `ConcreteSpec.Core.Mem.Audit.badStoreNone`). -/
theorem access_zero_planted_alignedNonNull_badStoreNone : ¬ ((Memory.empty.load target rawPtr 0 2 = some [] ∧
      badStoreNone target Memory.empty rawPtr [] 2 = some Memory.empty) ↔
    rawPtr.addr % 2 = 0 ∧ rawPtr.addr ≠ target.null ∧ rawPtr.Valid target) := by
  intro h
  have hh := (h.mpr (by decide)).2
  cases hh

-- 12. access_no_provenance
/-- Instance (law `ConcreteSpec.Core.Mem.access_no_provenance`, case `nonzero`). -/
theorem access_no_provenance_example_nonzero : memory.access target rawPtr 1 1 = none :=
  access_no_provenance target memory rawPtr 1 1 (by decide) rfl

/-- Planted (law `ConcreteSpec.Core.Mem.access_no_provenance`, case `nonzero`, subject `ConcreteSpec.Core.Mem.Memory.access`, mutant `ConcreteSpec.Core.Mem.Audit.badAccessSome`). -/
theorem access_no_provenance_planted_nonzero_badAccessSome : (1 : Nat) ≠ 0 ∧ rawPtr.prov = none ∧
    ¬ (badAccessSome target memory rawPtr 1 1 = none) := by decide

-- 13. access_after_free
/-- Instance (law `ConcreteSpec.Core.Mem.access_after_free`, case `freed`). -/
theorem access_after_free_example_freed : freedMemory.access target ptr 1 1 = none :=
  access_after_free target memory freedMemory ⟨0⟩ ptr 1 1 rfl rfl (by decide)

/-- Planted (law `ConcreteSpec.Core.Mem.access_after_free`, case `freed`, subject `ConcreteSpec.Core.Mem.Memory.access`, mutant `ConcreteSpec.Core.Mem.Audit.badAccessSome`). -/
theorem access_after_free_planted_freed_badAccessSome : memory.free ⟨0⟩ = some freedMemory ∧ ptr.prov = some ⟨0⟩ ∧ (1 : Nat) ≠ 0 ∧
    ¬ (badAccessSome target freedMemory ptr 1 1 = none) :=
  ⟨rfl, rfl, by decide, by decide⟩

-- 14. eqAddr_provenance
/-- Representation check. Canonical computation. -/
theorem example_Representation_check_Canonical_computation : Ptr.eqAddr ⟨⟨0⟩, 40, some ⟨0⟩⟩ ⟨⟨0⟩, 40, none⟩ = true :=
  eqAddr_provenance ⟨0⟩ 40 (some ⟨0⟩) none

/-- Representation check. Canonical computation. -/
theorem example_Representation_check_Canonical_computation_2 : ¬ (badEqAddr ⟨⟨0⟩, 40, some ⟨0⟩⟩ ⟨⟨0⟩, 40, none⟩ = true) := by decide

-- 15. alloc_ne_existing
/-- Instance (law `ConcreteSpec.Core.Mem.alloc_ne_existing`, case `second`). -/
theorem alloc_ne_existing_example_second : match memory.alloc target ⟨0⟩ 60 8 1 with
    | .ok (b, _) => b ≠ ⟨0⟩
    | .error _ => True :=
  alloc_ne_existing target memory memory_wf ⟨0⟩ 60 8 1 ⟨0⟩ initBlock rfl

/-- Planted (law `ConcreteSpec.Core.Mem.alloc_ne_existing`, case `second`, subject `ConcreteSpec.Core.Mem.Memory.alloc`, mutant `ConcreteSpec.Core.Mem.Audit.badAllocReuseZero`). -/
theorem alloc_ne_existing_planted_second_badAllocReuseZero : memory.WellFormed target ∧ memory.block ⟨0⟩ = some initBlock ∧
    ¬ (match badAllocReuseZero target memory ⟨0⟩ 60 8 1 with
      | .ok (b, _) => b ≠ ⟨0⟩
      | .error _ => True) :=
  ⟨memory_wf, rfl, fun h => h rfl⟩

-- 16. alloc_wellFormed
/-- Instance (law `ConcreteSpec.Core.Mem.alloc_wellFormed`, case `first`). -/
theorem alloc_wellFormed_example_first : match Memory.empty.alloc target ⟨0⟩ 40 8 1 with
    | .ok (_, m') => m'.WellFormed target
    | .error _ => True :=
  alloc_wellFormed target Memory.empty (empty_wellFormed target) ⟨0⟩ 40 8 1

/-- Planted (law `ConcreteSpec.Core.Mem.alloc_wellFormed`, case `first`, subject `ConcreteSpec.Core.Mem.Memory.alloc`, mutant `ConcreteSpec.Core.Mem.Audit.badAllocId`). -/
theorem alloc_wellFormed_planted_first_badAllocId : Memory.empty.WellFormed target ∧
    ¬ (match badAllocId target Memory.empty ⟨0⟩ 40 8 1 with
      | .ok (_, m') => m'.WellFormed target
      | .error _ => True) := by
  refine ⟨empty_wellFormed target, ?_⟩
  intro hw
  have h : (1 : Nat) < 1 := hw.fresh ⟨1⟩ initBlock rfl
  exact (by decide : ¬ ((1 : Nat) < 1)) h

-- 17. fromExposed_contains
/-- Instance (law `ConcreteSpec.Core.Mem.fromExposed_contains`, case `interior`). -/
theorem fromExposed_contains_example_interior : (exposedMemory.fromExposed ⟨0⟩ 42).prov = some ⟨0⟩ :=
  fromExposed_contains target exposedMemory exposedMemory_wf ⟨0⟩ 42 ⟨0⟩ exposedBlock
    rfl rfl (by decide)

/-- Planted (law `ConcreteSpec.Core.Mem.fromExposed_contains`, case `interior`, subject `ConcreteSpec.Core.Mem.Memory.fromExposed`, mutant `ConcreteSpec.Core.Mem.Audit.badFromExposedNone`). -/
theorem fromExposed_contains_planted_interior_badFromExposedNone : exposedMemory.WellFormed target ∧ exposedMemory.block ⟨0⟩ = some exposedBlock ∧
    exposedBlock.Candidate ⟨0⟩ = true ∧
    (exposedBlock.base ≤ 42 ∧ 42 < exposedBlock.base + exposedBlock.size) ∧
    ¬ ((badFromExposedNone exposedMemory ⟨0⟩ 42).prov = some ⟨0⟩) :=
  ⟨exposedMemory_wf, rfl, rfl, by decide, by decide⟩

-- 18. fromExposed_onePast
/-- Instance (law `ConcreteSpec.Core.Mem.fromExposed_onePast`, case `boundary`). -/
theorem fromExposed_onePast_example_boundary : (exposedMemory.fromExposed ⟨0⟩ 48).prov = some ⟨0⟩ :=
  fromExposed_onePast target exposedMemory exposedMemory_wf ⟨0⟩ 48 ⟨0⟩ exposedBlock
    rfl rfl rfl (by decide)

/-- Planted (law `ConcreteSpec.Core.Mem.fromExposed_onePast`, case `boundary`, subject `ConcreteSpec.Core.Mem.Memory.fromExposed`, mutant `ConcreteSpec.Core.Mem.Audit.badFromExposedNone`). -/
theorem fromExposed_onePast_planted_boundary_badFromExposedNone : exposedMemory.WellFormed target ∧ exposedMemory.block ⟨0⟩ = some exposedBlock ∧
    exposedBlock.Candidate ⟨0⟩ = true ∧ 48 = exposedBlock.base + exposedBlock.size ∧
    (∀ e ∈ exposedMemory.blocks, e.2.Candidate ⟨0⟩ = true →
      ¬ (e.2.base ≤ 48 ∧ 48 < e.2.base + e.2.size)) ∧
    ¬ ((badFromExposedNone exposedMemory ⟨0⟩ 48).prov = some ⟨0⟩) :=
  ⟨exposedMemory_wf, rfl, rfl, rfl, by decide, by decide⟩

-- 19. fromExposed_none
/-- Instance (law `ConcreteSpec.Core.Mem.fromExposed_none`, case `outside`). -/
theorem fromExposed_none_example_outside : (exposedMemory.fromExposed ⟨0⟩ 100).prov = none :=
  fromExposed_none target exposedMemory exposedMemory_wf ⟨0⟩ 100 (by decide)

/-- Planted (law `ConcreteSpec.Core.Mem.fromExposed_none`, case `outside`, subject `ConcreteSpec.Core.Mem.Memory.fromExposed`, mutant `ConcreteSpec.Core.Mem.Audit.badFromExposedSome`). -/
theorem fromExposed_none_planted_outside_badFromExposedSome : exposedMemory.WellFormed target ∧
    (∀ e ∈ exposedMemory.blocks, e.2.Candidate ⟨0⟩ = true →
      ¬ (e.2.base ≤ 100 ∧ 100 < e.2.base + e.2.size) ∧
        100 ≠ e.2.base + e.2.size) ∧
    ¬ ((badFromExposedSome exposedMemory ⟨0⟩ 100).prov = none) :=
  ⟨exposedMemory_wf, by decide, by decide⟩

-- 20. access_valid
/-- Instance (law `ConcreteSpec.Core.Mem.access_valid`, case `nonempty`). An out-of-range closed pointer cannot yield a successful nonempty access. -/
theorem access_valid_example_nonempty : match memory.access target badPtr 2 1 with
    | some _ => badPtr.Valid target
    | none => True :=
  access_valid target memory memory_wf 2 1 (by decide) badPtr

/-- Accepted (rule C4). Successful nonempty access remains covered at a valid closed pointer. -/
theorem example_Accepted_rule_C4_Successful_nonempty_access : (memory.access target ptr 2 1 = some (some (⟨0⟩, 0))) ∧
    (match memory.access target ptr 2 1 with
    | some _ => ptr.Valid target
    | none => True) :=
  ⟨rfl, access_valid target memory memory_wf 2 1 (by decide) ptr⟩

/-- Planted (law `ConcreteSpec.Core.Mem.access_valid`, case `nonempty`, subject `ConcreteSpec.Core.Mem.Memory.access`, mutant `ConcreteSpec.Core.Mem.Audit.badAccessPair`). -/
theorem access_valid_planted_nonempty_badAccessPair : memory.WellFormed target ∧ (2 : Nat) ≠ 0 ∧
    ¬ (match badAccessPair target memory badPtr 2 1 with
      | some _ => badPtr.Valid target
      | none => True) :=
  ⟨memory_wf, by decide, by
    change ¬ badPtr.Valid target
    decide⟩

-- 21. access_success
/-- Instance (law `ConcreteSpec.Core.Mem.access_success`, case `interior`). Independent provenance, liveness, alignment and bounds obligations hold for an interior two-byte access. -/
theorem access_success_example_interior : memory.access target ptr 2 1 = some (some (⟨0⟩, ptr.addr - initBlock.base)) :=
  access_success target memory ptr 2 1 ⟨0⟩ initBlock (by decide) rfl rfl rfl rfl
    (by decide) (by decide) (by decide)

/-- Planted (law `ConcreteSpec.Core.Mem.access_success`, case `interior`, subject `ConcreteSpec.Core.Mem.Memory.access`, mutant `ConcreteSpec.Core.Mem.Audit.badAccessNone`). -/
theorem access_success_planted_interior_badAccessNone : (2 : Nat) ≠ 0 ∧ ptr.prov = some ⟨0⟩ ∧ memory.block ⟨0⟩ = some initBlock ∧
    initBlock.live = true ∧ initBlock.space = ptr.space ∧ initBlock.base ≤ ptr.addr ∧
    ptr.addr + 2 ≤ initBlock.base + initBlock.size ∧ ptr.addr % 1 = 0 ∧
    ¬ (badAccessNone target memory ptr 2 1 = some (some (⟨0⟩, ptr.addr - initBlock.base))) := by
  decide

-- 22. store_load
/-- Instance (law `ConcreteSpec.Core.Mem.store_load`, case `oneByte`). -/
theorem store_load_example_oneByte : storedMemory.load target ptr ([MByte.init 7 none]).length 1 =
    some [MByte.init 7 none] :=
  store_load target memory storedMemory memory_wf ptr [.init 7 none] 1 rfl

/-- Planted (law `ConcreteSpec.Core.Mem.store_load`, case `oneByte`, subject `ConcreteSpec.Core.Mem.Memory.load`, mutant `ConcreteSpec.Core.Mem.Audit.badLoadNone`). -/
theorem store_load_planted_oneByte_badLoadNone : memory.WellFormed target ∧
    memory.store target ptr [.init 7 none] 1 = some storedMemory ∧
    ¬ (badLoadNone target storedMemory ptr ([MByte.init 7 none]).length 1 =
      some [MByte.init 7 none]) :=
  ⟨memory_wf, rfl, by decide⟩

-- 23. store_other_block
/-- Instance (law `ConcreteSpec.Core.Mem.store_other_block`, case `otherBlock`). -/
theorem store_other_block_example_otherBlock : (frameMemory.store target ptr [.init 7 none] 1).bind (fun m' => m'.block ⟨1⟩) =
    frameMemory.block ⟨1⟩ :=
  store_other_block target frameMemory ptr [.init 7 none] 1 ⟨0⟩ 0 ⟨1⟩ rfl (by decide)

/-- Planted (law `ConcreteSpec.Core.Mem.store_other_block`, case `otherBlock`, subject `ConcreteSpec.Core.Mem.Memory.store`, mutant `ConcreteSpec.Core.Mem.Audit.badStoreDup`). -/
theorem store_other_block_planted_otherBlock_badStoreDup : frameMemory.access target ptr ([MByte.init 7 none]).length 1 = some (some (⟨0⟩, 0)) ∧
    (⟨1⟩ : BlockId) ≠ ⟨0⟩ ∧
    ¬ ((badStoreDup target frameMemory ptr [.init 7 none] 1).bind (fun m' => m'.block ⟨1⟩) =
      frameMemory.block ⟨1⟩) :=
  ⟨rfl, by decide, by intro h; cases h⟩

-- 24. store_same_block_frame
/-- Instance (law `ConcreteSpec.Core.Mem.store_same_block_frame`, case `afterSlice`). -/
theorem store_same_block_frame_example_afterSlice : ((memory.store target ptr [.init 7 none] 1).bind fun m' => m'.block ⟨0⟩).map
      (fun y => y.bytes[2]?) = some (initBlock.bytes[2]?) :=
  store_same_block_frame target memory memory_wf ptr [.init 7 none] 1 ⟨0⟩ 0 2 initBlock
    rfl rfl (.inr (by decide))

/-- Planted (law `ConcreteSpec.Core.Mem.store_same_block_frame`, case `afterSlice`, subject `ConcreteSpec.Core.Mem.Memory.store`, mutant `ConcreteSpec.Core.Mem.Audit.badStoreNone`). -/
theorem store_same_block_frame_planted_afterSlice_badStoreNone : memory.WellFormed target ∧
    memory.access target ptr ([MByte.init 7 none]).length 1 = some (some (⟨0⟩, 0)) ∧
    memory.block ⟨0⟩ = some initBlock ∧ (2 < 0 ∨ 0 + ([MByte.init 7 none]).length ≤ 2) ∧
    ¬ (((badStoreNone target memory ptr [.init 7 none] 1).bind fun m' => m'.block ⟨0⟩).map
      (fun y => y.bytes[2]?) = some (initBlock.bytes[2]?)) :=
  ⟨memory_wf, rfl, rfl, .inr (by decide), by decide⟩

-- 25. alloc_initialized
/-- Instance (law `ConcreteSpec.Core.Mem.alloc_initialized`, case `first`). -/
theorem alloc_initialized_example_first : match Memory.empty.alloc target ⟨0⟩ 40 8 1 with
    | .ok (b, m') => ∃ x, m'.block b = some x ∧ x.live = true ∧ x.exposed = false ∧
        x.bytes = List.replicate 8 .uninit
    | .error _ => True :=
  alloc_initialized target Memory.empty ⟨0⟩ 40 8 1

/-- Planted (law `ConcreteSpec.Core.Mem.alloc_initialized`, case `first`, subject `ConcreteSpec.Core.Mem.Memory.alloc`, mutant `ConcreteSpec.Core.Mem.Audit.badAllocEmpty`). -/
theorem alloc_initialized_planted_first_badAllocEmpty : ¬ (match badAllocEmpty target Memory.empty ⟨0⟩ 40 8 1 with
    | .ok (b, m') => ∃ x, m'.block b = some x ∧ x.live = true ∧ x.exposed = false ∧
        x.bytes = List.replicate 8 .uninit
    | .error _ => True) := by
  rintro ⟨x, hx, _⟩
  cases hx

-- 26. store_wellFormed
/-- Instance (law `ConcreteSpec.Core.Mem.store_wellFormed`, case `oneByte`). -/
theorem store_wellFormed_example_oneByte : match memory.store target ptr [.init 7 none] 1 with
    | some m' => m'.WellFormed target
    | none => True :=
  store_wellFormed target memory memory_wf ptr [.init 7 none] 1

/-- Planted (law `ConcreteSpec.Core.Mem.store_wellFormed`, case `oneByte`, subject `ConcreteSpec.Core.Mem.Memory.store`, mutant `ConcreteSpec.Core.Mem.Audit.badStoreWf`). -/
theorem store_wellFormed_planted_oneByte_badStoreWf : memory.WellFormed target ∧
    ¬ (match badStoreWf target memory ptr [.init 7 none] 1 with
      | some m' => m'.WellFormed target
      | none => True) :=
  ⟨memory_wf, by
    intro hw
    have h : (0 : Nat) < 0 := hw.fresh ⟨0⟩ storedBlock rfl
    exact (Nat.lt_irrefl 0) h⟩

-- 27. free_wellFormed
/-- Instance (law `ConcreteSpec.Core.Mem.free_wellFormed`, case `live`). -/
theorem free_wellFormed_example_live : match memory.free ⟨0⟩ with
    | some m' => m'.WellFormed target
    | none => True :=
  free_wellFormed target memory memory_wf ⟨0⟩

/-- Planted (law `ConcreteSpec.Core.Mem.free_wellFormed`, case `live`, subject `ConcreteSpec.Core.Mem.Memory.free`, mutant `ConcreteSpec.Core.Mem.Audit.badFreeWf`). -/
theorem free_wellFormed_planted_live_badFreeWf : memory.WellFormed target ∧
    ¬ (match badFreeWf memory ⟨0⟩ with
      | some m' => m'.WellFormed target
      | none => True) :=
  ⟨memory_wf, by
    intro hw
    have h : (0 : Nat) < 0 := hw.fresh ⟨0⟩ freedBlock rfl
    exact (Nat.lt_irrefl 0) h⟩

-- 28. expose_success
/-- Instance (law `ConcreteSpec.Core.Mem.expose_success`, case `live`). -/
theorem expose_success_example_live : memory.expose ptr = some (memory.modify ⟨0⟩ (fun y => { y with exposed := true })) :=
  expose_success memory ptr ⟨0⟩ initBlock rfl rfl rfl

/-- Planted (law `ConcreteSpec.Core.Mem.expose_success`, case `live`, subject `ConcreteSpec.Core.Mem.Memory.expose`, mutant `ConcreteSpec.Core.Mem.Audit.badExposeNone`). -/
theorem expose_success_planted_live_badExposeNone : ptr.prov = some ⟨0⟩ ∧ memory.block ⟨0⟩ = some initBlock ∧ initBlock.live = true ∧
    ¬ (badExposeNone memory ptr =
      some (memory.modify ⟨0⟩ (fun y => { y with exposed := true }))) :=
  ⟨rfl, rfl, rfl, by intro h; cases h⟩

-- 29. expose_wellFormed
/-- Instance (law `ConcreteSpec.Core.Mem.expose_wellFormed`, case `live`). -/
theorem expose_wellFormed_example_live : match memory.expose ptr with
    | some m' => m'.WellFormed target
    | none => True :=
  expose_wellFormed target memory memory_wf ptr

/-- Planted (law `ConcreteSpec.Core.Mem.expose_wellFormed`, case `live`, subject `ConcreteSpec.Core.Mem.Memory.expose`, mutant `ConcreteSpec.Core.Mem.Audit.badExposeWf`). -/
theorem expose_wellFormed_planted_live_badExposeWf : memory.WellFormed target ∧
    ¬ (match badExposeWf memory ptr with
      | some m' => m'.WellFormed target
      | none => True) :=
  ⟨memory_wf, by
    intro hw
    have h : (0 : Nat) < 0 := hw.fresh ⟨0⟩ exposedBlock rfl
    exact (Nat.lt_irrefl 0) h⟩

-- 30. store_load_other
/-- Instance (law `ConcreteSpec.Core.Mem.store_load_other`, case `disjointProv`). -/
theorem store_load_other_example_disjointProv : frameStoredMemory.load target frameOther 1 1 = frameMemory.load target frameOther 1 1 :=
  store_load_other target frameMemory frameStoredMemory ptr frameOther [.init 7 none] 1 1 1 rfl (by decide)

/-- Planted (law `ConcreteSpec.Core.Mem.store_load_other`, case `disjointProv`, subject `ConcreteSpec.Core.Mem.Memory.load`, mutant `ConcreteSpec.Core.Mem.Audit.badLoadDiff`). -/
theorem store_load_other_planted_disjointProv_badLoadDiff : frameMemory.store target ptr [.init 7 none] 1 = some frameStoredMemory ∧
    frameOther.prov ≠ ptr.prov ∧
    ¬ (badLoadDiff target frameStoredMemory frameOther 1 1 = badLoadDiff target frameMemory frameOther 1 1) :=
  ⟨rfl, by decide, by decide⟩

-- 31. expose_load
/-- Instance (law `ConcreteSpec.Core.Mem.expose_load`, case `samePtr`). -/
theorem expose_load_example_samePtr : exposedMemory.load target ptr 1 1 = memory.load target ptr 1 1 :=
  expose_load target memory exposedMemory ptr ptr 1 1 rfl

/-- Planted (law `ConcreteSpec.Core.Mem.expose_load`, case `samePtr`, subject `ConcreteSpec.Core.Mem.Memory.load`, mutant `ConcreteSpec.Core.Mem.Audit.badLoadDiff`). -/
theorem expose_load_planted_samePtr_badLoadDiff : memory.expose ptr = some exposedMemory ∧
    ¬ (badLoadDiff target exposedMemory ptr 1 1 = badLoadDiff target memory ptr 1 1) :=
  ⟨rfl, by decide⟩

-- 32. load_prefix
/-- Instance (law `ConcreteSpec.Core.Mem.load_prefix`, case `twoToOne`). -/
theorem load_prefix_example_twoToOne : memory.load target ptr 1 1 = some ([MByte.uninit, MByte.uninit].take 1) :=
  load_prefix target memory ptr 2 1 1 [.uninit, .uninit]
    (by decide) (by decide) (by decide) rfl

/-- Planted (law `ConcreteSpec.Core.Mem.load_prefix`, case `twoToOne`, subject `ConcreteSpec.Core.Mem.Memory.load`, mutant `ConcreteSpec.Core.Mem.Audit.badLoadPrefix`). -/
theorem load_prefix_planted_twoToOne_badLoadPrefix : (2 : Nat) ≠ 0 ∧ (1 : Nat) ≠ 0 ∧ 1 ≤ 2 ∧
    badLoadPrefix target memory ptr 2 1 = some [.uninit, .uninit] ∧
    ¬ (badLoadPrefix target memory ptr 1 1 = some ([MByte.uninit, MByte.uninit].take 1)) := by
  decide

-- 33. free_alloc_ne_existing
/-- Instance (law `ConcreteSpec.Core.Mem.free_alloc_ne_existing`, case `reuseAddr`). -/
theorem free_alloc_ne_existing_example_reuseAddr : match freedMemory.alloc target ⟨0⟩ 40 8 1 with
    | .ok (new, _) => new ≠ ⟨0⟩
    | .error _ => True :=
  free_alloc_ne_existing target memory freedMemory memory_wf ⟨0⟩ ⟨0⟩ initBlock rfl rfl
    ⟨0⟩ 40 8 1

/-- Planted (law `ConcreteSpec.Core.Mem.free_alloc_ne_existing`, case `reuseAddr`, subject `ConcreteSpec.Core.Mem.Memory.alloc`, mutant `ConcreteSpec.Core.Mem.Audit.badAllocReuseZero`). -/
theorem free_alloc_ne_existing_planted_reuseAddr_badAllocReuseZero : memory.WellFormed target ∧ memory.free ⟨0⟩ = some freedMemory ∧
    memory.block ⟨0⟩ = some initBlock ∧
    ¬ (match badAllocReuseZero target freedMemory ⟨0⟩ 40 8 1 with
      | .ok (new, _) => new ≠ ⟨0⟩
      | .error _ => True) :=
  ⟨memory_wf, rfl, rfl, fun h => h rfl⟩


def skipFree : Memory → BlockId → Option Memory := fun m _ => some m
def corruptExpose : Memory → Ptr → Option Memory :=
  fun m p => (m.expose p).map fun m' => { m' with next := 0 }
def corruptStoreGeometry : Target → Memory → Ptr → List MByte → Nat → Option Memory :=
  fun _ m _ _ _ => some (m.modify ⟨0⟩ fun x => { x with base := x.base + 1 })
def corruptExposeGeometry : Memory → Ptr → Option Memory :=
  fun m _ => some (m.modify ⟨0⟩ fun x => { x with size := x.size + 1 })
def wrongReconstruction : Memory → AddrSpace → Nat → Ptr :=
  fun m s a => ⟨s, a, some ⟨m.next⟩⟩

/-- Instance (law `ConcreteSpec.Core.Mem.eqAddr_separation`, case `provenance`). -/
theorem eqAddr_separation_example_provenance : Ptr.eqAddr ptr rawPtr = false ↔ ptr.space ≠ rawPtr.space ∨
    ptr.addr < rawPtr.addr ∨ rawPtr.addr < ptr.addr := eqAddr_separation ptr rawPtr
/-- Planted (law `ConcreteSpec.Core.Mem.eqAddr_separation`, case `provenance`, subject `ConcreteSpec.Core.Mem.Ptr.eqAddr`, mutant `ConcreteSpec.Core.Mem.Audit.badEqAddr`). -/
theorem eqAddr_separation_planted_provenance_badEqAddr : ¬ (badEqAddr ptr rawPtr = false ↔ ptr.space ≠ rawPtr.space ∨
    ptr.addr < rawPtr.addr ∨ rawPtr.addr < ptr.addr) := by decide
/-- Fault: same address space is mistaken for equal addresses. -/
def alwaysEqual : Ptr → Ptr → Bool := fun p q => decide (p.space = q.space)

/-- Fault: numeric equality forgets address-space identity. -/
def ignoreSpace : Ptr → Ptr → Bool := fun p q => decide (p.addr = q.addr)
/-- Instance (law `ConcreteSpec.Core.Mem.eqAddr_separation`, case `address`). -/
theorem eqAddr_separation_example_address : Ptr.eqAddr ptr other = false ↔ ptr.space ≠ other.space ∨
    ptr.addr < other.addr ∨ other.addr < ptr.addr := eqAddr_separation ptr other
/-- Planted (law `ConcreteSpec.Core.Mem.eqAddr_separation`, case `address`, subject `ConcreteSpec.Core.Mem.Ptr.eqAddr`, mutant `ConcreteSpec.Core.Mem.Audit.alwaysEqual`). -/
theorem eqAddr_separation_planted_address_alwaysEqual : ¬ (alwaysEqual ptr other = false ↔ ptr.space ≠ other.space ∨
    ptr.addr < other.addr ∨ other.addr < ptr.addr) := by decide

/-- Instance (law `ConcreteSpec.Core.Mem.free_retains`, case `live`). -/
theorem free_retains_example_live : match memory.free ⟨0⟩ with | none => True | some m' => ∃ x, memory.block ⟨0⟩ = some x ∧ m'.block ⟨0⟩ = some { x with live := false, exposed := false } ∧ m'.next = memory.next :=
  free_retains memory ⟨0⟩
/-- Planted (law `ConcreteSpec.Core.Mem.free_retains`, case `live`, subject `ConcreteSpec.Core.Mem.Memory.free`, mutant `ConcreteSpec.Core.Mem.Audit.skipFree`). -/
theorem free_retains_planted_live_skipFree : ¬ (match skipFree memory ⟨0⟩ with | none => True | some m' => ∃ x, memory.block ⟨0⟩ = some x ∧ m'.block ⟨0⟩ = some { x with live := false, exposed := false } ∧ m'.next = memory.next) := by
  intro h
  obtain ⟨x, hx, hh, _⟩ := h
  have he : x = initBlock := Option.some.inj hx.symm
  subst x
  have hf := congrArg (fun z => z.map Block.live) hh
  cases hf

/-- Instance (law `ConcreteSpec.Core.Mem.expose_frame`, case `live`). -/
theorem expose_frame_example_live : match memory.expose ptr with | none => True | some m' => ∃ b x, ptr.prov = some b ∧ memory.block b = some x ∧ m'.block b = some { x with exposed := true } ∧ m'.next = memory.next ∧ ∀ c, c ≠ b → m'.block c = memory.block c :=
  expose_frame memory ptr
/-- Planted (law `ConcreteSpec.Core.Mem.expose_frame`, case `live`, subject `ConcreteSpec.Core.Mem.Memory.expose`, mutant `ConcreteSpec.Core.Mem.Audit.corruptExpose`). -/
theorem expose_frame_planted_live_corruptExpose : ¬ (match corruptExpose memory ptr with | none => True | some m' => ∃ b x, ptr.prov = some b ∧ memory.block b = some x ∧ m'.block b = some { x with exposed := true } ∧ m'.next = memory.next ∧ ∀ c, c ≠ b → m'.block c = memory.block c) := by
  intro h
  obtain ⟨b, x, hp, hx, _, hn, _⟩ := h
  have : (0 : Nat) = 1 := hn
  cases this

/-- Instance (law `ConcreteSpec.Core.Mem.store_geometry`, case `live`). -/
theorem store_geometry_example_live : match memory.store target ptr [.init 7 none] 1 with | none => True | some m' => ∀ b, (m'.block b).map Block.geometry = (memory.block b).map Block.geometry :=
  store_geometry target memory ptr [.init 7 none] 1
/-- Planted (law `ConcreteSpec.Core.Mem.store_geometry`, case `live`, subject `ConcreteSpec.Core.Mem.Memory.store`, mutant `ConcreteSpec.Core.Mem.Audit.corruptStoreGeometry`). -/
theorem store_geometry_planted_live_corruptStoreGeometry : ¬ (match corruptStoreGeometry target memory ptr [.init 7 none] 1 with | none => True | some m' => ∀ b, (m'.block b).map Block.geometry = (memory.block b).map Block.geometry) := by
  intro h
  have hh := h ⟨0⟩
  revert hh
  decide

/-- Instance (law `ConcreteSpec.Core.Mem.expose_geometry`, case `live`). -/
theorem expose_geometry_example_live : match memory.expose ptr with | none => True | some m' => ∀ b, (m'.block b).map Block.geometry = (memory.block b).map Block.geometry :=
  expose_geometry memory ptr
/-- Planted (law `ConcreteSpec.Core.Mem.expose_geometry`, case `live`, subject `ConcreteSpec.Core.Mem.Memory.expose`, mutant `ConcreteSpec.Core.Mem.Audit.corruptExposeGeometry`). -/
theorem expose_geometry_planted_live_corruptExposeGeometry : ¬ (match corruptExposeGeometry memory ptr with | none => True | some m' => ∀ b, (m'.block b).map Block.geometry = (memory.block b).map Block.geometry) := by
  intro h
  have hh := h ⟨0⟩
  revert hh
  decide

/-- Instance (law `ConcreteSpec.Core.Mem.fromExposed_sound`, case `interior`). -/
theorem fromExposed_sound_example_interior : match (exposedMemory.fromExposed ⟨0⟩ 42).prov with
    | none => True
    | some b => ∃ x, exposedMemory.block b = some x ∧ x.Candidate ⟨0⟩ = true ∧
        ((x.base ≤ 42 ∧ 42 < x.base + x.size) ∨
         (42 = x.base + x.size ∧ ∀ e ∈ exposedMemory.blocks, e.2.Candidate ⟨0⟩ = true →
           ¬ (e.2.base ≤ 42 ∧ 42 < e.2.base + e.2.size))) :=
  fromExposed_sound target exposedMemory exposedMemory_wf ⟨0⟩ 42
/-- Planted (law `ConcreteSpec.Core.Mem.fromExposed_sound`, case `interior`, subject `ConcreteSpec.Core.Mem.Memory.fromExposed`, mutant `ConcreteSpec.Core.Mem.Audit.wrongReconstruction`). -/
theorem fromExposed_sound_planted_interior_wrongReconstruction : exposedMemory.WellFormed target ∧
    ¬ (match (wrongReconstruction exposedMemory ⟨0⟩ 42).prov with
      | none => True
      | some b => ∃ x, exposedMemory.block b = some x ∧ x.Candidate ⟨0⟩ = true ∧
          ((x.base ≤ 42 ∧ 42 < x.base + x.size) ∨
           (42 = x.base + x.size ∧ ∀ e ∈ exposedMemory.blocks, e.2.Candidate ⟨0⟩ = true →
             ¬ (e.2.base ≤ 42 ∧ 42 < e.2.base + e.2.size)))) := by
  refine ⟨exposedMemory_wf, ?_⟩
  intro h
  obtain ⟨x, hx, _⟩ := h
  cases hx

/-- Accepted (rule C4). Framing observes initialized bytes in a second live allocation. -/
theorem example_Accepted_rule_C4_Framing_observes_initialized : frameMemory.load target frameOther 2 1 = some [.init 9 none, .init 10 none] ∧
    frameStoredMemory.load target frameOther 2 1 = some [.init 9 none, .init 10 none] := by decide

/-- Instance (law `ConcreteSpec.Core.Mem.eqAddr_separation`, case `space`). -/
theorem eqAddr_separation_example_space : Ptr.eqAddr ptr ⟨⟨1⟩, 40, none⟩ = false ↔ ptr.space ≠ (⟨1⟩ : AddrSpace) ∨
    ptr.addr < 40 ∨ 40 < ptr.addr := eqAddr_separation ptr ⟨⟨1⟩, 40, none⟩
/-- Planted (law `ConcreteSpec.Core.Mem.eqAddr_separation`, case `space`, subject `ConcreteSpec.Core.Mem.Ptr.eqAddr`, mutant `ConcreteSpec.Core.Mem.Audit.ignoreSpace`). -/
theorem eqAddr_separation_planted_space_ignoreSpace : ¬ (ignoreSpace ptr ⟨⟨1⟩, 40, none⟩ = false ↔ ptr.space ≠ (⟨1⟩ : AddrSpace) ∨
    ptr.addr < 40 ∨ 40 < ptr.addr) := by decide

/-! The following controls delete one guard at a time. Their other obligations are
satisfied, so each refusal observes its named rule rather than an unrelated failure. -/

/-- Fault injector for access: omit just liveness, space, lower bound, upper bound or alignment. -/
def accessSkipping (skip : Nat) (m : Memory) (p : Ptr) (n a : Nat) :
    Option (Option (BlockId × Nat)) := do
  let b ← p.prov
  let x ← m.block b
  if (skip = 0 ∨ x.live = true) ∧ (skip = 1 ∨ x.space = p.space) ∧
      (skip = 2 ∨ x.base ≤ p.addr) ∧ (skip = 3 ∨ p.addr + n ≤ x.base + x.size) ∧
      (skip = 4 ∨ p.addr % a = 0) then some (some (b, p.addr - x.base)) else none

/-- Released memory fails only liveness here. -/
theorem example_Released_memory_fails_only_liveness_here : freedMemory.access target ptr 1 1 = none ∧
    accessSkipping 0 freedMemory ptr 1 1 = some (some (⟨0⟩, 0)) := by decide

/-- A pointer in a second admitted space fails the allocation's space check alone. -/
theorem example_pointer_in_second_admitted_space_fails : let twoSpaces := { target with spaces := [⟨0⟩, ⟨1⟩] }
    let p := { ptr with space := ⟨1⟩ }
    twoSpaces.WellFormed ∧ p.Valid twoSpaces ∧ memory.WellFormed twoSpaces ∧
    memory.access twoSpaces p 1 1 = none ∧
    accessSkipping 1 memory p 1 1 = some (some (⟨0⟩, 0)) := by
  refine ⟨by decide, by decide, ?_, by decide, by decide⟩
  exact alloc_wellFormed { target with spaces := [⟨0⟩, ⟨1⟩] } Memory.empty
    (empty_wellFormed _) ⟨0⟩ 40 8 1

/-- A byte before the base fails only the lower bound. -/
theorem example_byte_before_base_fails_only_lower : memory.access target { ptr with addr := 39 } 1 1 = none ∧
    accessSkipping 2 memory { ptr with addr := 39 } 1 1 = some (some (⟨0⟩, 0)) := by decide

/-- The one-past pointer is not a one-byte access. -/
theorem example_one_past_pointer_is_not_one : memory.access target { ptr with addr := 48 } 1 1 = none ∧
    accessSkipping 3 memory { ptr with addr := 48 } 1 1 = some (some (⟨0⟩, 8)) := by decide

/-- Interior and in bounds, but not aligned to two bytes. -/
theorem example_Interior_and_in_bounds_but_not : memory.access target { ptr with addr := 41 } 1 2 = none ∧
    accessSkipping 4 memory { ptr with addr := 41 } 1 2 = some (some (⟨0⟩, 1)) := by decide

/-- Fault: reverse reconstruction priority by trying one-past before containing. -/
def onePastFirst (m : Memory) (space : AddrSpace) (addr : Nat) : Ptr :=
  match m.blocks.find? (fun e => e.2.Candidate space && decide (addr = e.2.base + e.2.size)) with
  | some e => ⟨space, addr, some e.1⟩
  | none => m.fromExposed space addr

/-- Adjacent exposed blocks make the priority observable, independent of list order. -/
def adjacent : Memory :=
  ⟨[(⟨1⟩, { candidate ⟨0⟩ 48 8 1 with exposed := true }),
    (⟨0⟩, { candidate ⟨0⟩ 40 8 1 with exposed := true })], 2⟩

theorem example_adjacent_WellFormed_target : adjacent.WellFormed target ∧
    (adjacent.fromExposed ⟨0⟩ 48).prov = some ⟨1⟩ ∧
    (onePastFirst adjacent ⟨0⟩ 48).prov = some ⟨0⟩ := by
  refine ⟨?_, by decide, by decide⟩
  exact expose_wellFormed target _
    (alloc_wellFormed target exposedMemory exposedMemory_wf ⟨0⟩ 48 8 1)
    ⟨⟨0⟩, 48, some ⟨1⟩⟩

/-- Fault: reconstruct without requiring exposure. -/
def ignoreExposure (m : Memory) (space : AddrSpace) (addr : Nat) : Ptr :=
  let exposed : Memory := { m with blocks := List.map (fun (b, x) => (b, { x with exposed := true })) m.blocks }
  exposed.fromExposed space addr

theorem example_memory_fromExposed_prov : (memory.fromExposed ⟨0⟩ 42).prov = none ∧
    (ignoreExposure memory ⟨0⟩ 42).prov = some ⟨0⟩ := by decide

/-- Fault: forgetting the nonempty condition admits a zero-sized block at its one-past address. -/
def includeEmpty (m : Memory) (space : AddrSpace) (addr : Nat) : Ptr :=
  match m.blocks.find? (fun e => e.2.live && e.2.exposed && decide (e.2.space = space) &&
      decide (addr = e.2.base + e.2.size)) with
  | some e => ⟨space, addr, some e.1⟩
  | none => m.fromExposed space addr

theorem example_m_Memory : let m : Memory := ⟨[(⟨0⟩, { candidate ⟨0⟩ 40 0 1 with exposed := true })], 1⟩
    m.WellFormed target ∧ (m.fromExposed ⟨0⟩ 40).prov = none ∧
    (includeEmpty m ⟨0⟩ 40).prov = some ⟨0⟩ := by
  refine ⟨?_, by decide, by decide⟩
  exact expose_wellFormed target _
    (alloc_wellFormed target Memory.empty (empty_wellFormed target) ⟨0⟩ 40 0 1)
    ⟨⟨0⟩, 40, some ⟨0⟩⟩

end Audit

end ConcreteSpec.Core.Mem
