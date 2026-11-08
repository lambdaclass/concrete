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

/-- Accepted: returning without a destination restores exactly the caller's association list. -/
example {V : Type} (v : V) (caller : AList Var V) : Exec.bindDst none v caller = caller := rfl

/-- Refused: a loop exit cannot cross a call's `.ret` frame; only a returned value restores its
caller. -/
example {O : Exec.Operands} (dst : Option Var) (caller callee : AList Var O.Val)
    (st : O.Store) (l : LoopId) (k : List (Exec.Frame O)) :
    Exec.stepExit callee st (.brk l) (.ret dst caller :: k) = none := rfl

/-! ## ABI signature interface -/

namespace ABI

/-- A machine word location for one typed value: a register or a logical stack slot. A value may
use several locations, in the order given by its binding. -/
inductive Location (Reg : Type) where
  | register (r : Reg)
  | stack (slot : Nat)
  deriving DecidableEq, Repr

/-- The machine state visible to the ABI interface. Stack slots below `stackPointer` are the
caller's preserved stack; argument and result areas at or above it need not be preserved. -/
structure Machine (Reg Word : Type) where
  regs : Reg → Word
  slots : Nat → Word
  stackPointer : Nat

/-- Read one location in a machine state. -/
def Location.read {Reg Word : Type} (st : Machine Reg Word) : Location Reg → Word
  | .register r => st.regs r
  | .stack slot => st.slots slot

/-- The ABI locations and logical type of one argument or result. -/
structure Binding (Ty Reg : Type) where
  ty : Ty
  locations : List (Location Reg)

/-- The encoded words of a typed value are exactly the words in its locations. -/
def Binding.Holds {Ty Val Reg Word : Type} (encode : Ty → Val → List Word)
    (b : Binding Ty Reg) (v : Val) (st : Machine Reg Word) : Prop :=
  encode b.ty v = b.locations.map (Location.read st)

/-- Every argument has its declared type and location binding; too few or too many arguments do not
satisfy the relation. -/
def ArgumentsHold {Ty Val Reg Word : Type} (encode : Ty → Val → List Word) :
    List (Binding Ty Reg) → List Val → Machine Reg Word → Prop
  | [], [], _ => True
  | b :: bs, v :: vs, st => b.Holds encode v st ∧ ArgumentsHold encode bs vs st
  | _, _, _ => False

/-- An explicit target convention. `stackBound` is the complete callee contribution from final
artifact evidence, including its frame, spills, cleanup and nested calls; it is not a source-local
estimate. `result` is the return convention. -/
structure Signature (Ty Val Reg Word : Type) where
  encode : Ty → Val → List Word
  arguments : List (Binding Ty Reg)
  result : Binding Ty Reg
  frameSize : Nat
  frameAlign : Nat
  calleeSaved : List Reg
  clobbers : List Reg
  stackBound : Nat

/-- The callee-saved registers, the caller's stack below its stack pointer, and the stack pointer
itself are exactly restored. -/
def Machine.Preserved {Reg Word : Type} (before after : Machine Reg Word) (saved : List Reg) : Prop :=
  (∀ r ∈ saved, after.regs r = before.regs r) ∧
  (∀ slot, slot < before.stackPointer → after.slots slot = before.slots slot) ∧
  after.stackPointer = before.stackPointer

/-- Every register outside the clobber set has the same value after return. -/
def Machine.RegisterFrame {Reg Word : Type} (before after : Machine Reg Word)
    (clobbers : List Reg) : Prop :=
  ∀ r, r ∉ clobbers → after.regs r = before.regs r

/-- A signature's structural laws: positive power-of-two alignment, frame size rounded to it, the
frame included in the callee stack bound, and callee-saved registers disjoint from clobbers. -/
def Signature.WellFormed {Ty Val Reg Word : Type} (sig : Signature Ty Val Reg Word) : Prop :=
  IsPow2 sig.frameAlign ∧ sig.frameSize % sig.frameAlign = 0 ∧
    sig.frameSize ≤ sig.stackBound ∧ ∀ r, r ∈ sig.calleeSaved → r ∉ sig.clobbers

/-- A returning ABI call: typed arguments occupy their entry locations, the typed result occupies
its return locations, preserved machine state is restored exactly, and only clobbers may change
registers. -/
structure Signature.Call {Ty Val Reg Word : Type} (sig : Signature Ty Val Reg Word)
    (before after : Machine Reg Word) (args : List Val) (result : Val) : Prop where
  arguments : ArgumentsHold sig.encode sig.arguments args before
  result : sig.result.Holds sig.encode result after
  preserved : Machine.Preserved before after sig.calleeSaved
  clobbers : Machine.RegisterFrame before after sig.clobbers

/-- The stack-accounting handoff of one call: the caller's frame plus the callee's final-artifact
bound. Heap accounting is separate. -/
def callStack {Ty Val Reg Word : Type} (callerFrame : Nat) (sig : Signature Ty Val Reg Word) : Nat :=
  callerFrame + sig.stackBound

/-- Whole-artifact stack accounting adds the launcher/runtime contribution to the call handoff. -/
def artifactStack {Ty Val Reg Word : Type} (launcherRuntime callerFrame : Nat)
    (sig : Signature Ty Val Reg Word) : Nat :=
  launcherRuntime + callStack callerFrame sig

/-! ### Foreign validity and retention (Q3.12) -/

/-- Types with a direct foreign representation: integers, floats, bool, Unit, raw pointers to values,
and C-layout structs of such values. There is no reference or enum constructor, so neither can be
smuggled behind a raw pointer or inside a C struct. -/
inductive FFIType where
  | int
  | float
  | bool
  | unit
  | rawPointer (pointee : FFIType)
  | cStruct (fields : List FFIType)
  deriving Repr

/-- The logical types at an extern boundary. Owned `H` and its receipt are retention interface
values, not direct C representations. `enum` is present only so the validity rule can refuse it. -/
inductive ExternType (H : Type) where
  | ffi (t : FFIType)
  | owned (h : H)
  | receipt (h : H)
  | enum

/-- Extern retention: call-only by default, or `kept` for an owned allocation. -/
inductive Retention where
  | callOnly
  | kept
  deriving DecidableEq, Repr

/-- One extern parameter's logical type and retention marker. -/
structure ExternParam (H : Type) where
  ty : ExternType H
  retention : Retention

/-- Valid ordinary extern parameters are call-only direct FFI values, or `kept` owned allocations.
Enums, kept raw pointers, receipts as fresh inputs, and unmarked owned allocations are refused. A
receipt accepted by the paired completion is checked separately by `KeptPair.WellFormed`. -/
def ExternParam.Valid {H : Type} : ExternParam H → Prop
  | ⟨.ffi _, .callOnly⟩ => True
  | ⟨.owned _, .kept⟩ => True
  | _ => False

instance {H : Type} (p : ExternParam H) : Decidable p.Valid := by
  cases p with
  | mk ty retention =>
    cases ty <;> cases retention <;> unfold ExternParam.Valid <;> infer_instance

/-- Valid extern results are direct FFI values, an owned allocation returned by completion, or its
linear receipt returned by submission. An enum result is refused. -/
def ExternResult.Valid {H : Type} : ExternType H → Prop
  | .enum => False
  | _ => True

instance {H : Type} (t : ExternType H) : Decidable (ExternResult.Valid t) := by
  cases t <;> unfold ExternResult.Valid <;> infer_instance

/-- The two typed extern identities paired for one kept handle `H`. -/
structure KeptPair (F H : Type) where
  submit : F
  complete : F
  handle : H

/-- The signature-pairing law of Q3.12: submission takes `kept h: H` and returns `Kept<H>`;
completion takes exactly that receipt and returns `H`. The four descriptors are the declaration's
actual parameter/result types, so this is an interface validity check, not just a naming convention. -/
def KeptPair.WellFormed {F H : Type} (pair : KeptPair F H)
    (submitParam : ExternParam H) (submitResult completeParam completeResult : ExternType H) : Prop :=
  submitParam = ⟨.owned pair.handle, .kept⟩ ∧ submitResult = .receipt pair.handle ∧
    completeParam = .receipt pair.handle ∧ completeResult = .owned pair.handle

/-- The ownership state of one handle: the Concrete owner has it, or foreign code retains it and
Concrete has only the linear `Kept<H>` receipt. There is no discard transition for a receipt. -/
inductive RetentionState (H : Type) where
  | owned (h : H)
  | kept (h : H)

/-- A foreign call, carrying the handle whose retention it changes. -/
inductive ForeignEvent (F H : Type) where
  | call (f : F) (h : H)

/-- The complete retention transition relation. Submission consumes `H` and returns `Kept<H>`;
only the paired completion consumes that receipt and returns the same `H`. No other call changes
retention. The receipt keeps the allocator dependency in `H` and has no operations of its own. -/
def KeptStep {F H : Type} (pair : KeptPair F H) :
    RetentionState H → ForeignEvent F H → RetentionState H → Prop
  | .owned h, .call f h', .kept h'' =>
    f = pair.submit ∧ h = pair.handle ∧ h' = h ∧ h'' = h
  | .kept h, .call f h', .owned h'' =>
    f = pair.complete ∧ h = pair.handle ∧ h' = h ∧ h'' = h
  | _, _, _ => False

instance {F H : Type} [DecidableEq F] [DecidableEq H] (pair : KeptPair F H)
    (before : RetentionState H) (event : ForeignEvent F H) (after : RetentionState H) :
    Decidable (KeptStep pair before event after) := by
  cases before <;> cases event <;> cases after <;> unfold KeptStep <;> infer_instance

/-! ### Launcher entry and exit (Q8.5–Q8.6) -/

/-- The launcher contract binds an exact retained artifact closure, entry inputs, exit observations
and the final artifact's stack bound. These are relations, not a concrete launcher implementation. -/
structure Launcher (Artifact Input Result Observation : Type) where
  retainedClosure : Artifact → Prop
  entry : Artifact → Input → Prop
  exit : Artifact → Result → Observation → Prop
  stackBound : Artifact → Nat

/-- Startup either refuses an inadequate reservation with status 125 or admits the input. -/
inductive Startup (Input : Type) where
  | refused125
  | entered (input : Input)
  deriving DecidableEq, Repr

/-- The decided stack-budget admission rule. -/
def stackAdmission {Input : Type} (bound reserved : Nat) (input : Input) : Startup Input :=
  if reserved < bound then .refused125 else .entered input

/-- A launched run carries the exact artifact closure and entry/exit relations, with a reservation
at least the final artifact bound. -/
structure Launcher.Run {Artifact Input Result Observation : Type}
    (l : Launcher Artifact Input Result Observation) (artifact : Artifact) (reserved : Nat)
    (input : Input) (result : Result) (observation : Observation) : Prop where
  closure : l.retainedClosure artifact
  entry : l.entry artifact input
  exit : l.exit artifact result observation
  adequateStack : l.stackBound artifact ≤ reserved

/-! ### ABI examples -/

/-- Accepted: two typed arguments are read from register 0 and stack slot 0 respectively. -/
example : ArgumentsHold (fun (_ : Nat) (v : Nat) => [v])
    [⟨0, [.register 0]⟩, ⟨1, [.stack 0]⟩] [11, 12]
    ⟨fun _ => 11, fun _ => 12, 1⟩ := by
  exact ⟨rfl, rfl, trivial⟩

/-- Refused: a value different from the word in its bound stack slot does not satisfy the argument
relation. -/
example : ¬ ArgumentsHold (fun (_ : Nat) (v : Nat) => [v])
    [⟨0, [.register 0]⟩, ⟨1, [.stack 0]⟩] [11, 99]
    ⟨fun _ => 11, fun _ => 12, 1⟩ := by
  intro h
  have h' := h.2.1
  simp [Binding.Holds, Location.read] at h'

/-- Accepted: a call-only C pointer and a kept owned allocation are valid extern parameters. -/
example : (⟨.ffi (.rawPointer .int), .callOnly⟩ : ExternParam Nat).Valid ∧
    (⟨.owned 7, .kept⟩ : ExternParam Nat).Valid := by decide

/-- Refused: enums never cross an extern boundary, and a raw pointer cannot be marked `kept`. -/
example : ¬ (⟨.enum, .callOnly⟩ : ExternParam Nat).Valid ∧
    ¬ (⟨.ffi (.rawPointer .int), .kept⟩ : ExternParam Nat).Valid ∧
    ¬ (ExternResult.Valid (.enum : ExternType Nat)) := by decide

/-- Accepted: the paired declarations have exactly the owned/receipt types of the same handle. -/
example : (⟨0, 1, 7⟩ : KeptPair Nat Nat).WellFormed
    ⟨.owned 7, .kept⟩ (.receipt 7) (.receipt 7) (.owned 7) := by
  exact ⟨rfl, rfl, rfl, rfl⟩

/-- Refused: a completion for a different handle's receipt does not satisfy the pairing law. -/
example : ¬ (⟨0, 1, 7⟩ : KeptPair Nat Nat).WellFormed
    ⟨.owned 7, .kept⟩ (.receipt 7) (.receipt 8) (.owned 7) := by
  intro h
  have hc := h.2.2.1
  cases hc

/-- Accepted: submitting handle 7 to the paired extern turns ownership into its receipt. -/
example : KeptStep ⟨0, 1, 7⟩ (.owned 7) (.call 0 7) (.kept 7) := by decide

/-- Refused: the submitting extern cannot consume its own receipt; only completion can. -/
example : ¬ KeptStep ⟨0, 1, 7⟩ (.kept 7) (.call 0 7) (.owned 7) := by decide

/-- Accepted: an adequate reservation admits startup. -/
example : stackAdmission 64 64 7 = .entered 7 := by decide

/-- Refused: a smaller reservation refuses startup with status 125. -/
example : stackAdmission 64 32 7 = .refused125 := by decide

end ABI

end ConcreteSpec.Core.Frame
