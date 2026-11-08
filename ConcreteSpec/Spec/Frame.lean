import ConcreteSpec.Spec.Hybrid

/-!
# Logical footprints, call frames and ABI contracts (foundation C10)

PROPOSED: not normative until the owner freezes Core. Part of `ConcreteSpec.Spec`; see
`ConcreteSpec/README.md`. This module states interfaces and laws, not a concrete target ABI.

A logical footprint names byte ranges by allocation identity, not by numeric address, together with
structured places. `Footprint.Preserves` is the frame law: every byte of an old block outside the
footprint and every place outside it is exactly unchanged. A fresh allocation has no old bytes to
preserve; its fresh identity makes it disjoint from every range in an old block. Byte contents include
initialization and pointer fragments, so preserving a byte preserves all three.

Sequential composition unions footprints. A C1 call contract supplies the callee footprint and its
frame law. C1 return frames restore the caller's locals; shared hybrid state is not rolled back.

The ABI interface relates typed arguments and results to registers and stack slots, and records frame
size/alignment, return locations, clobbers, preserved machine state and final-artifact stack
accounting. Foreign arguments are FFI-safe, never enums. `kept` consumes an owned allocation and
produces a linear `Kept<H>` receipt which only the paired extern consumes. No source frame clause,
snapshot construct, extern syntax, packed-access semantics or target-specific ABI table is introduced.
-/
namespace ConcreteSpec.Core.Frame
open ConcreteSpec.Core.Mem
open ConcreteSpec.Core.Hybrid

/-- One logical byte range: an allocation identity and offsets within that allocation. -/
structure ByteRange where
  block : MemBlockId
  offset : Nat
  extent : Nat
  deriving DecidableEq, Repr

/-- Byte `i` of block `b` belongs to the range. A zero-extent range contains no bytes. -/
def ByteRange.Contains (r : ByteRange) (b : MemBlockId) (i : Nat) : Prop :=
  r.block = b ∧ r.offset ≤ i ∧ i < r.offset + r.extent

instance (r : ByteRange) (b : MemBlockId) (i : Nat) : Decidable (r.Contains b i) := by
  unfold ByteRange.Contains; infer_instance

/-- Two ranges are disjoint when their allocations differ, either is empty, or their offsets do not
overlap. Numeric address reuse cannot make ranges in different allocations overlap. -/
def ByteRange.Disjoint (r s : ByteRange) : Prop :=
  r.block ≠ s.block ∨ r.extent = 0 ∨ s.extent = 0 ∨
    r.offset + r.extent ≤ s.offset ∨ s.offset + s.extent ≤ r.offset

instance (r s : ByteRange) : Decidable (r.Disjoint s) := by
  unfold ByteRange.Disjoint; infer_instance

/-- The complete byte footprint of a block of `size` bytes. Lifecycle operations such as `free` and
`expose` use this to name the block whose metadata they may change. -/
def fullBlock (b : MemBlockId) (size : Nat) : ByteRange := ⟨b, 0, size⟩

/-- An operation's permitted effects on hybrid state. Byte ranges and places are sets represented by
lists; membership, not list order or duplication, is semantic. -/
structure Footprint where
  bytes : List ByteRange
  places : List Place
  deriving DecidableEq, Repr

/-- The empty footprint. -/
def Footprint.empty : Footprint := ⟨[], []⟩

/-- Only one byte range is touched. -/
def Footprint.byte (r : ByteRange) : Footprint := ⟨[r], []⟩

/-- Only one structured place is touched. -/
def Footprint.place (p : Place) : Footprint := ⟨[], [p]⟩

/-- The combined permission of two operations run in sequence. -/
def Footprint.union (f g : Footprint) : Footprint := ⟨f.bytes ++ g.bytes, f.places ++ g.places⟩

/-- This byte is outside every range in the footprint. -/
def Footprint.ByteOutside (f : Footprint) (b : MemBlockId) (i : Nat) : Prop :=
  ∀ r ∈ f.bytes, ¬ r.Contains b i

/-- This place is outside the footprint. -/
def Footprint.PlaceOutside (f : Footprint) (p : Place) : Prop := p ∉ f.places

instance (f : Footprint) (b : MemBlockId) (i : Nat) : Decidable (f.ByteOutside b i) := by
  unfold Footprint.ByteOutside; infer_instance

instance (f : Footprint) (p : Place) : Decidable (f.PlaceOutside p) := by
  unfold Footprint.PlaceOutside; infer_instance

/-- The frame law for one operation. Only bytes of blocks that existed before the operation are
quantified: allocation creates bytes rather than changing old ones. The whole `MByte` is compared,
including initialization and provenance fragments. Place cells are compared exactly. -/
def Footprint.Preserves (before after : State) (f : Footprint) : Prop :=
  (∀ b i x, before.memory.block b = some x → f.ByteOutside b i →
    (after.memory.block b).map (fun y => y.bytes[i]?) = some (x.bytes[i]?)) ∧
  (∀ p, f.PlaceOutside p → after.places p = before.places p)

/-- The footprint of a successful access. A zero-byte access touches no allocation range. -/
def accessFootprint : Option (Option (MemBlockId × Nat)) → Nat → Footprint
  | some (some (b, off)), n => .byte ⟨b, off, n⟩
  | _, _ => .empty

/-- A complete C1 call of `f`: execution begins at its body with its scope above a `.ret` frame and
finishes at that frame's continuation with the result bound into the restored caller locals. The
`Steps` premise includes argument-independent callee execution and all of its cleanup. -/
def C1Call (O : Exec.Operands) (f : FnId) (P : Exec.Program O)
    (entry exit : Exec.Config O) : Prop :=
  ∃ (fn : Exec.Fn O) (dst : Option Var) (caller : AList Var O.Val)
    (k : List (Exec.Frame O)) (tr : List O.Event) (result : O.Val),
    AList.lookup P.fns f = some fn ∧
    entry.ctrl = .exec fn.body ∧
    entry.frames = .scope fn.block (fn.params.map Prod.fst) [] :: .ret dst caller :: k ∧
    exit.ctrl = .done ∧ exit.locals = Exec.bindDst dst result caller ∧ exit.frames = k ∧
    Exec.Steps P entry tr exit

/-- The footprint contract of one C1 callee. `project` exposes the hybrid part of the operand
instance's shared state; ownership instrumentation need not have a footprint. Every complete call of
`f` must satisfy the stated frame law. -/
structure C1CallContract (O : Exec.Operands) (f : FnId) (P : Exec.Program O)
    (project : O.Store → State) where
  footprint : Footprint
  frame : ∀ {entry exit}, C1Call O f P entry exit →
    footprint.Preserves (project entry.store) (project exit.store)

/-! ### Examples -/

/-- Accepted: adjacent ranges are disjoint, and a byte inside a range is contained in it. -/
example : (⟨⟨0⟩, 4, 4⟩ : ByteRange).Disjoint ⟨⟨0⟩, 8, 2⟩ ∧
    (⟨⟨0⟩, 4, 4⟩ : ByteRange).Contains ⟨0⟩ 5 := by decide

/-- Accepted: equal numeric offsets in different allocation identities are disjoint. -/
example : (⟨⟨0⟩, 4, 4⟩ : ByteRange).Disjoint ⟨⟨1⟩, 4, 4⟩ := by decide

/-- Refused: overlapping ranges in one allocation are not disjoint; nor does a range contain its
one-past offset. -/
example : ¬ (⟨⟨0⟩, 4, 4⟩ : ByteRange).Disjoint ⟨⟨0⟩, 7, 2⟩ ∧
    ¬ (⟨⟨0⟩, 4, 4⟩ : ByteRange).Contains ⟨0⟩ 8 := by decide

/-- Accepted: a zero-extent range is disjoint from every range, including itself. -/
example : (⟨⟨0⟩, 4, 0⟩ : ByteRange).Disjoint ⟨⟨0⟩, 4, 0⟩ := by decide

end ConcreteSpec.Core.Frame
