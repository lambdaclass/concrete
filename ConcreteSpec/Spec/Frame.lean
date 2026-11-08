import ConcreteSpec.Spec.Hybrid

/-!
# Logical footprints, call frames and ABI contracts (foundation C10)

PROPOSED: not normative until the owner freezes Core. Part of `ConcreteSpec.Spec`; see
`ConcreteSpec/README.md`. This module states interfaces and laws, not a concrete target ABI.

A logical footprint names byte ranges by allocation identity, not by numeric address, together with
block metadata and structured places as separate permissions. `Footprint.Preserves` is the frame
law: every byte of an old block outside its byte ranges, all metadata of every block outside its
block set, and every place outside it are exactly unchanged.
A fresh allocation has no old bytes to preserve; its fresh identity makes it disjoint from every
range in an old block. Byte contents include initialization and pointer fragments, so preserving a
byte preserves all three.

Sequential composition unions footprints. A C1 call contract supplies the callee footprint and its
frame law for the complete call, from argument evaluation and entry instrumentation through cleanup,
leave instrumentation and result binding. C1 return frames restore the caller's locals; shared hybrid
state is not rolled back.

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

/-- The complete byte footprint of a block of `size` bytes. -/
def fullBlock (b : MemBlockId) (size : Nat) : ByteRange := ⟨b, 0, size⟩

/-- All block fields except its byte contents. -/
def blockMetadata (x : Block) : AddrSpace × Nat × Nat × Nat × Bool × Bool :=
  (x.space, x.base, x.size, x.align, x.live, x.exposed)

/-- An operation's permitted effects on hybrid state. Byte ranges, block metadata and places are
sets represented by lists; membership, not list order or duplication, is semantic. A byte write does
not claim permission to change its block's metadata. -/
structure Footprint where
  bytes : List ByteRange
  blocks : List MemBlockId
  places : List Place
  deriving DecidableEq, Repr

/-- The empty footprint. -/
def Footprint.empty : Footprint := ⟨[], [], []⟩

/-- Only one byte range is touched; every block's metadata remains outside the footprint. -/
def Footprint.byte (r : ByteRange) : Footprint := ⟨[r], [], []⟩

/-- Only the metadata of one allocation may change, including for a zero-sized allocation. -/
def Footprint.block (b : MemBlockId) : Footprint := ⟨[], [b], []⟩

/-- Only one structured place is touched. -/
def Footprint.place (p : Place) : Footprint := ⟨[], [], [p]⟩

/-- The combined permission of two operations run in sequence. -/
def Footprint.union (f g : Footprint) : Footprint :=
  ⟨f.bytes ++ g.bytes, f.blocks ++ g.blocks, f.places ++ g.places⟩

/-- This byte is outside every range in the footprint. -/
def Footprint.ByteOutside (f : Footprint) (b : MemBlockId) (i : Nat) : Prop :=
  ∀ r ∈ f.bytes, ¬ r.Contains b i

/-- This block's metadata is outside the footprint, independently of its byte ranges. -/
def Footprint.BlockOutside (f : Footprint) (b : MemBlockId) : Prop := b ∉ f.blocks

/-- This place is outside the footprint. -/
def Footprint.PlaceOutside (f : Footprint) (p : Place) : Prop := p ∉ f.places

instance (f : Footprint) (b : MemBlockId) (i : Nat) : Decidable (f.ByteOutside b i) := by
  unfold Footprint.ByteOutside; infer_instance

instance (f : Footprint) (b : MemBlockId) : Decidable (f.BlockOutside b) := by
  unfold Footprint.BlockOutside; infer_instance

instance (f : Footprint) (p : Place) : Decidable (f.PlaceOutside p) := by
  unfold Footprint.PlaceOutside; infer_instance

/-- The frame law for one operation. Only bytes of blocks that existed before the operation are
quantified: allocation creates bytes rather than changing old ones. The whole `MByte` is compared,
including initialization and provenance fragments. For every block outside the metadata footprint,
its space, base, size, alignment, liveness and exposure are exactly unchanged, even when one of its
byte ranges is written. Place cells are compared exactly. -/
def Footprint.Preserves (before after : State) (f : Footprint) : Prop :=
  (∀ b i x, before.memory.block b = some x → f.ByteOutside b i →
    (after.memory.block b).map (fun y => y.bytes[i]?) = some (x.bytes[i]?)) ∧
  (∀ b x, before.memory.block b = some x → f.BlockOutside b →
    (after.memory.block b).map blockMetadata = some (blockMetadata x)) ∧
  (∀ p, f.PlaceOutside p → after.places p = before.places p)

/-- The footprint of a successful access. A zero-byte access touches no allocation range. -/
def accessFootprint : Option (Option (MemBlockId × Nat)) → Nat → Footprint
  | some (some (b, off)), n => .byte ⟨b, off, n⟩
  | _, _ => .empty

/-- A complete C1 call of `f`, from its `.call` statement to the caller continuation. The `Steps`
premise reaches this call's own `.ret` frame, either at an explicit return or at normal Unit
fall-through. The final silent step performs `.leave`, result binding and restoration. A matching
intermediate configuration therefore cannot stand in for the outer return. -/
def C1Call (O : Exec.Operands) (f : FnId) (P : Exec.Program O)
    (entry exit : Exec.Config O) : Prop :=
  ∃ (fn : Exec.Fn O) (dst : Option Var) (caller : AList Var O.Val)
    (k : List (Exec.Frame O)) (args : List O.Expr) (site : SiteId) (vs : List O.Val)
    (st stArgs stEnter stReturn : O.Store) (callee : AList Var O.Val)
    (tr : List O.Event) (result : O.Val),
    AList.lookup P.fns f = some fn ∧
    entry = ⟨.exec (.call dst f args site), caller, st, k⟩ ∧
    Exec.evalArgs O caller st args = .ok (vs, stArgs) ∧
    fn.params.length = vs.length ∧
    O.life (.enter (fn.params.map Prod.fst)) stArgs = some stEnter ∧
    ((Exec.Steps P entry tr
        ⟨.exiting (.ret result), callee, stReturn, .ret dst caller :: k⟩ ∧
      Exec.step P ⟨.exiting (.ret result), callee, stReturn, .ret dst caller :: k⟩ =
        some (.next exit)) ∨
     (result = O.unitVal ∧ Exec.Steps P entry tr
        ⟨.done, callee, stReturn, .ret dst caller :: k⟩ ∧
      Exec.step P ⟨.done, callee, stReturn, .ret dst caller :: k⟩ = some (.next exit)))

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

/-- Accepted: a nonempty byte footprint still leaves the written block's metadata framed; a
lifecycle footprint frames other blocks, and a union frames a block absent from both components. -/
example : (Footprint.byte ⟨⟨0⟩, 4, 4⟩).BlockOutside ⟨0⟩ ∧
    (Footprint.block ⟨0⟩).BlockOutside ⟨1⟩ ∧
    (Footprint.union (.byte ⟨⟨0⟩, 4, 4⟩) (.block ⟨1⟩)).BlockOutside ⟨0⟩ := by decide

/-- Refused: a lifecycle footprint claims its block's metadata even though it writes no bytes, and
a union cannot frame metadata claimed by either component. -/
example : ¬ (Footprint.block ⟨0⟩).BlockOutside ⟨0⟩ ∧
    ¬ (Footprint.union (.block ⟨0⟩) .empty).BlockOutside ⟨0⟩ := by decide

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

/-- An abstract call-transition interface, independent of the ABI contract. -/
abbrev CallTransition (_Ty Val Reg Word : Type) :=
  Machine Reg Word → Machine Reg Word → List Val → Val → Prop

/-- Conformance is a specification to prove about a transition, never execution evidence. -/
def TransitionConforms {Ty Val Reg Word : Type} (sig : Signature Ty Val Reg Word)
    (transition : CallTransition Ty Val Reg Word) : Prop :=
  ∀ before after args result, transition before after args result → sig.Call before after args result

/-- Small machine instructions for an executable, target-independent example. Frame entry and
exit are explicit operations; writes carry their destinations rather than frame-law proofs. -/
inductive ToyInstruction where
  | register (r word : Nat)
  | slot (offset word : Nat)
  | reserve (bytes : Nat)
  | release (bytes : Nat)

/-- Execute instructions without checking any ABI contract. Stack writes address the current
stack pointer plus their offset, so a write after reservation targets the callee's frame. -/
def runToy : List ToyInstruction → Machine Nat Nat → Machine Nat Nat
  | [], st => st
  | instruction :: rest, st =>
    let next := match instruction with
      | .register r word => { st with regs := fun q => if q = r then word else st.regs q }
      | .slot offset word =>
        { st with slots := fun q => if q = st.stackPointer + offset then word else st.slots q }
      | .reserve bytes => { st with stackPointer := st.stackPointer + bytes }
      | .release bytes => { st with stackPointer := st.stackPointer - bytes }
    runToy rest next

/-- This toy convention passes one natural word in register 0 and returns one in register 1.
It is an example implementation, not the linux-x86_64 ABI table. -/
def toySignature : Signature Unit Nat Nat Nat :=
  ⟨fun _ v => [v], [⟨(), [.register 0]⟩], ⟨(), [.register 1]⟩,
    8, 8, [0], [1], 8⟩

/-- Pack the toy input by executing a register write. -/
def toyInput (base : Machine Nat Nat) (arg : Nat) : Machine Nat Nat :=
  runToy [.register 0 arg] base

/-- A real toy callee reserves a frame, writes its return value and a private spill, then
releases the frame. No instruction tests restoration or result validity. -/
def toyBody (result : Nat) : List ToyInstruction :=
  [.reserve 8, .register 1 result, .slot 0 99, .release 8]

/-- Independent execution relation for the toy convention. The witnesses describe input
packing and actual instructions; they contain none of the ABI equalities being proved. -/
def toyTransition : CallTransition Unit Nat Nat Nat := fun before after args result =>
  ∃ base arg, before = toyInput base arg ∧ args = [arg] ∧
    after = runToy (toyBody result) before

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

/-- Extern retention: call-only by default, or `kept` for an owned allocation. Call-only pointers
permit reads, and writes only from a mutable borrow or owned allocation, during the call only;
borrow-derived pointers are never `kept`. That foreign code does not access call-only memory after
return, or kept memory before its paired completion returns, is a named trusted-base premise of
Q3.12, not a theorem proved by this transition interface. -/
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

/-- The two distinct extern identities paired for one tracked handle instance. `H` is the handle
identity type; `handle` is the instance tracked by this single-handle retention trace. -/
structure KeptPair (F H : Type) where
  submit : F
  complete : F
  handle : H

/-- Valid extern results are direct FFI values, the paired owned allocation returned by its
completion, or its linear receipt returned by its submission. Checking the extern identity prevents
an unrelated declaration from manufacturing either retention value. An enum result is refused. -/
def ExternResult.Valid {F H : Type} (pair : KeptPair F H) (f : F) : ExternType H → Prop
  | .ffi _ => True
  | .owned h => f = pair.complete ∧ h = pair.handle
  | .receipt h => f = pair.submit ∧ h = pair.handle
  | .enum => False

instance {F H : Type} [DecidableEq F] [DecidableEq H] (pair : KeptPair F H) (f : F)
    (t : ExternType H) : Decidable (ExternResult.Valid pair f t) := by
  cases t <;> unfold ExternResult.Valid <;> infer_instance

/-- The signature-pairing law of Q3.12: distinct submission and completion take `kept h: H` and its
receipt respectively; only submission returns `Kept<H>` and only completion returns `H`. The four
descriptors are the declaration's actual parameter/result types, so this is an interface validity
check, not just a naming convention. -/
def KeptPair.WellFormed {F H : Type} (pair : KeptPair F H)
    (submitParam : ExternParam H) (submitResult completeParam completeResult : ExternType H) : Prop :=
  pair.submit ≠ pair.complete ∧ submitParam = ⟨.owned pair.handle, .kept⟩ ∧
    submitResult = .receipt pair.handle ∧ completeParam = .receipt pair.handle ∧
    completeResult = .owned pair.handle

/-- The ownership state of one handle: the Concrete owner has it, or foreign code retains it and
Concrete has only the linear `Kept<H>` receipt. There is no discard transition for a receipt. -/
inductive RetentionState (H : Type) where
  | owned (h : H)
  | kept (h : H)

/-- A foreign call, carrying the handle whose retention it changes. -/
inductive ForeignEvent (F H : Type) where
  | call (f : F) (h : H)

/-- The complete retention transition relation. Submission consumes `H` and returns `Kept<H>`;
only the distinct paired completion consumes that receipt and returns the same `H`. No other call
changes retention. The receipt keeps the allocator dependency in `H` and has no operations of its
own. -/
def KeptStep {F H : Type} (pair : KeptPair F H) :
    RetentionState H → ForeignEvent F H → RetentionState H → Prop
  | .owned h, .call f h', .kept h'' =>
    f = pair.submit ∧ h = pair.handle ∧ h' = h ∧ h'' = h
  | .kept h, .call f h', .owned h'' =>
    f = pair.complete ∧ pair.submit ≠ pair.complete ∧ h = pair.handle ∧ h' = h ∧ h'' = h
  | _, _, _ => False

instance {F H : Type} [DecidableEq F] [DecidableEq H] (pair : KeptPair F H)
    (before : RetentionState H) (event : ForeignEvent F H) (after : RetentionState H) :
    Decidable (KeptStep pair before event after) := by
  cases before <;> cases event <;> cases after <;> unfold KeptStep <;> infer_instance

/-- Check declarations by destructuring their types and comparing handle identities. The result
is an extracted protocol, with no proof fields and no call to a specification predicate. -/
def checkKeptPair {F H : Type} [DecidableEq F] [DecidableEq H]
    (submit complete : F) (sp : ExternParam H) (sr cp cr : ExternType H) :
    Option (KeptPair F H) :=
  match sp, sr, cp, cr with
  | ⟨.owned h, .kept⟩, .receipt a, .receipt b, .owned c =>
    if submit = complete then none
    else if h = a ∧ h = b ∧ h = c then some ⟨submit, complete, h⟩ else none
  | _, _, _, _ => none

/-- Retention instructions are ownership transfers, not assertions of `KeptStep`. -/
inductive RetentionInstruction where
  | submit
  | complete
  deriving DecidableEq

/-- Execute a transfer and emit the foreign call it requires. Endpoint dispatch comes from the
instruction; the input token supplies the transferred handle. There is no validity re-check. -/
def transferKept {F H : Type} (pair : KeptPair F H) :
    RetentionInstruction → RetentionState H → Option (ForeignEvent F H × RetentionState H)
  | .submit, .owned h => some (.call pair.submit h, .kept h)
  | .complete, .kept h => some (.call pair.complete h, .owned h)
  | _, _ => none

/-- The returned transfer, if any, meets the retention contract. This is a specification over
an independently executable transfer implementation. -/
def TransferCorrect {F H : Type} (pair : KeptPair F H) (before : RetentionState H)
    (out : Option (ForeignEvent F H × RetentionState H)) : Prop :=
  ∀ event after, out = some (event, after) → KeptStep pair before event after

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
    ¬ (ExternResult.Valid ⟨0, 1, 7⟩ 0 (.enum : ExternType Nat)) := by decide

/-- Refused: an unrelated extern can return neither the paired receipt nor the paired owned handle. -/
example : ¬ ExternResult.Valid (⟨0, 1, 7⟩ : KeptPair Nat Nat) 2 (.receipt 7) ∧
    ¬ ExternResult.Valid (⟨0, 1, 7⟩ : KeptPair Nat Nat) 2 (.owned 7) := by decide

/-- Accepted: the paired declarations have exactly the owned/receipt types of the same handle. -/
example : (⟨0, 1, 7⟩ : KeptPair Nat Nat).WellFormed
    ⟨.owned 7, .kept⟩ (.receipt 7) (.receipt 7) (.owned 7) := by
  exact ⟨by decide, rfl, rfl, rfl, rfl⟩

/-- Refused: ordinary FFI results cannot stand in for the submission receipt or completion's owned
handle, even though each is individually a valid extern result. -/
example : ¬ (⟨0, 1, 7⟩ : KeptPair Nat Nat).WellFormed
    ⟨.owned 7, .kept⟩ (.ffi .int) (.receipt 7) (.owned 7) ∧
    ¬ (⟨0, 1, 7⟩ : KeptPair Nat Nat).WellFormed
      ⟨.owned 7, .kept⟩ (.receipt 7) (.receipt 7) (.ffi .int) := by
  constructor <;> intro h
  · have hr := h.2.2.1
    cases hr
  · have hr := h.2.2.2.2
    cases hr

/-- Refused: a completion for a different handle's receipt does not satisfy the pairing law. -/
example : ¬ (⟨0, 1, 7⟩ : KeptPair Nat Nat).WellFormed
    ⟨.owned 7, .kept⟩ (.receipt 7) (.receipt 8) (.owned 7) := by
  intro h
  have hc := h.2.2.2.1
  cases hc

/-- Refused: submission and completion cannot be the same extern identity. -/
example : ¬ (⟨0, 0, 7⟩ : KeptPair Nat Nat).WellFormed
    ⟨.owned 7, .kept⟩ (.receipt 7) (.receipt 7) (.owned 7) := by
  intro h
  exact h.1 rfl

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
