import ConcreteSpec.Proofs.Memory
import ConcreteSpec.Spec.Basics
import ConcreteSpec.Spec.Hybrid
import ConcreteSpec.Spec.Ident
import ConcreteSpec.Spec.Memory
import ConcreteSpec.Spec.Scalar
import ConcreteSpec.Spec.Target
import ConcreteSpec.Proofs.Hybrid

namespace ConcreteSpec.Core.Hybrid
open ConcreteSpec.Core.Mem

/-- Instance (law `encode_refuses_unadmitted`). All held scalar representations refuse,
including both Boolean values; no empty byte list stands in for missing authority. -/
theorem encode_refuses_unadmitted_example (tgt : Target) (b : Bool) (c : Char) :
    encode tgt (.bool b) = none ∧ encode tgt (.char c) = none ∧ encode tgt .unit = none := by
  simp [encode, Value.Storable, Value.ty, CVal.ty]

/-- Instance. The public query retains the admitted integer encoding. -/
theorem example_public_query_retains_admitted_integer_encoding : encode linuxX8664 (.int .u8 7) = some [.init 7 none] := by decide

/-- Planted encoder: return an apparent empty representation when admission fails. -/
def sentinelEncode (tgt : Target) (v : Value) : Option (List MByte) :=
  if hv : v.Storable then some (encodeAdmitted tgt v hv) else some []

/-- Planted (law `encode_refuses_unadmitted`, case `heldBoolean`,
subject `ConcreteSpec.Core.Hybrid.encode`, mutant `ConcreteSpec.Core.Hybrid.sentinelEncode`).
The empty-sentinel implementation violates refusal while preserving admitted integer encoding. -/
theorem encode_refuses_unadmitted_planted_heldBoolean_sentinelEncode : ¬ Value.Storable (.bool true) ∧
    sentinelEncode linuxX8664 (.bool true) ≠ none := by decide

namespace Audit

def range : Range := ⟨⟨⟨0⟩, 40, some ⟨0⟩⟩, 3, 1⟩
def memory : Memory := ⟨[(⟨0⟩, candidate ⟨0⟩ 40 3 1)], 1⟩
def before : State := ⟨[(⟨0⟩, ⟨(.int .u8), .materialized range⟩)], memory⟩
def after : State :=
  { before with memory := memory.modify ⟨0⟩ fun x =>
      { x with bytes := [.init 1 none, .uninit, .uninit] } }
def exposedAfter : State :=
  { before with memory := memory.modify ⟨0⟩ fun x =>
      { x with exposed := true } }

def structState : State := ⟨[(⟨0⟩, ⟨(.int .u8), .structured (.int .u8 1)⟩)], Memory.empty⟩
def matRange : Range := ⟨⟨⟨0⟩, 40, some ⟨0⟩⟩, 1, 1⟩
def matBlock : Block := { candidate ⟨0⟩ 40 1 1 with bytes := [.init 1 none] }
def matMemory : Memory := ⟨[(⟨0⟩, matBlock)], 1⟩
def matState : State := ({ structState with memory := matMemory }).set ⟨0⟩ ⟨(.int .u8), .materialized matRange⟩
def matExposedBlock : Block := { matBlock with exposed := true }
def matExposedState : State :=
  ⟨[(⟨0⟩, ⟨(.int .u8), .materialized matRange⟩), (⟨0⟩, ⟨(.int .u8), .structured (.int .u8 1)⟩)],
    ⟨[(⟨0⟩, matExposedBlock)], 1⟩⟩

/-- Mutate only the allocation counter; bytes, projections and all other geometry are retained. -/
def forgetFreshness (st : State) : State :=
  { st with memory := { st.memory with next := 0 } }

/-- Wrong relation: compare integer bytes with the successor of the requested value. -/
def badRep (tgt : Target) (t : Ty) (bs : List MByte) (v : Value) : Prop :=
  match v with
  | .int it n => Rep tgt t bs (.int it (n + 1))
  | _ => Rep tgt t bs v
/-- Wrong codec: an off-by-one integer payload. -/
def badEncode (tgt : Target) (v : Value) (hv : v.Storable) : List MByte :=
  match v with
  | .int t n => encodeInt tgt t (n + 1)
  | v => encodeAdmitted tgt v hv
/-- Wrong decoder: erase the decoded type by returning unit on success. -/
def badDecodeUnit (tgt : Target) (t : Ty) (bs : List MByte) : Option Value :=
  (decode tgt t bs).map (fun _ => .unit)
/-- Wrong reader: dispatch only to the byte-storage branch, losing structured values. -/
def badReadNone (tgt : Target) (st : State) (p : Place) : Option Value := do
  let c ← st.places p
  match c.storage with
  | .structured _ => none
  | .materialized _ => st.read tgt p
/-- Wrong reader: dispatch only to structured storage, losing materialized values. -/
def badReadStructOnly (tgt : Target) (st : State) (p : Place) : Option Value := do
  let c ← st.places p
  match c.storage with
  | .structured _ => st.read tgt p
  | .materialized _ => none
/-- Wrong materializer: decode an existing mapping and allocate it again instead of reusing it.
The first structured allocation is unchanged; only the reuse branch is faulty. -/
def badMaterializeReallocate (tgt : Target) (st : State) (p : Place) (s : AddrSpace)
    (base align : Nat) (pad : List MByte) : Option (Range × State) := do
  let c ← st.places p
  match c.storage with
  | .structured _ => st.materialize tgt p s base align pad
  | .materialized _ =>
    let v ← st.read tgt p
    (st.set p { c with storage := .structured v }).materialize tgt p s base align pad
/-- Wrong materializer: allocate the bytes but forget to publish the place's new storage form. -/
def badMaterializeEmpty (tgt : Target) (st : State) (p : Place) (s : AddrSpace)
    (base align : Nat) (pad : List MByte) : Option (Range × State) :=
  (st.materialize tgt p s base align pad).map fun (r, out) =>
    (r, { out with cells := st.cells })
def badMaterializeWf : Target → State → Place → AddrSpace → Nat → Nat → List MByte →
    Option (Range × State) :=
  fun tgt st p s base align pad =>
    (st.materialize tgt p s base align pad).map fun (r, out) => (r, forgetFreshness out)
/-- Wrong writer: clear the trailing bytes of the containing block after an active write. -/
def badWriteClobber (tgt : Target) (st : State) (p : Place) (v : Value) : Option State := do
  if hv : v.Storable then
    let out ← st.write tgt p v
    let c ← out.places p
    match c.storage with
    | .structured _ => some out
    | .materialized r =>
      let b ← r.ptr.prov
      pure { out with memory := out.memory.modify b fun x =>
        { x with bytes := x.bytes.take (encodeAdmitted tgt v hv).length ++
            List.replicate (x.size - (encodeAdmitted tgt v hv).length) (.init 0 none) } }
  else none
def badWriteWf : Target → State → Place → Value → Option State := fun tgt st p v => (st.write tgt p v).map forgetFreshness
def badRawWriteWf : Target → State → Ptr → List MByte → Nat → Option State :=
  fun tgt st p bs a => (st.rawWrite tgt p bs a).map forgetFreshness
def badExposeStateWf : Target → State → Place → AddrSpace → Nat → Nat → List MByte →
    Option (Range × State) :=
  fun tgt st p s base align pad =>
    (st.expose tgt p s base align pad).map fun (r, out) => (r, forgetFreshness out)

/-- Helper. A stored block cannot have an identity below a reset allocation counter. -/
private theorem not_reset_wf (st : State) (x : Block)
    (hx : st.memory.block ⟨0⟩ = some x) (hn : st.memory.next = 0) :
    ¬ st.WellFormed linuxX8664 := by
  intro hw
  have h := hw.memory.fresh ⟨0⟩ x hx
  rw [hn] at h
  exact Nat.not_lt_zero _ h

/-- Helper (congruence). The materialized audit fixture state satisfies the hybrid invariant. -/
private theorem before_wf : before.WellFormed linuxX8664 := by
  refine ⟨alloc_wellFormed linuxX8664 Memory.empty (empty_wellFormed linuxX8664)
    ⟨0⟩ 40 3 1, ?_⟩
  intro p c hc
  simp only [State.places, before, AList.lookup] at hc
  split at hc
  next =>
    cases hc
    exact ⟨⟨0⟩, candidate ⟨0⟩ 40 3 1, rfl, rfl, rfl, rfl, rfl, rfl, by decide⟩
  next => cases hc

/-- Helper (congruence). The structured audit fixture state satisfies the hybrid invariant. -/
private theorem structState_wf : structState.WellFormed linuxX8664 := by
  refine ⟨empty_wellFormed linuxX8664, ?_⟩
  intro p c hc
  simp only [State.places, structState, AList.lookup] at hc
  split at hc
  next => cases hc; trivial
  next => cases hc

-- 1. rep_roundtrip
/-- Instance (law `ConcreteSpec.Core.Hybrid.rep_roundtrip`, case `u8One`). -/
theorem rep_roundtrip_example_u8One : Rep linuxX8664 (Value.ty (.int .u8 1)) (encodeAdmitted linuxX8664 (.int .u8 1) (by decide)) (.int .u8 1) :=
  rep_roundtrip linuxX8664 (.int .u8 1) (by decide)

/-- Planted (law `ConcreteSpec.Core.Hybrid.rep_roundtrip`, case `u8One`, subject `ConcreteSpec.Core.Hybrid.Rep`, mutant `ConcreteSpec.Core.Hybrid.Audit.badRep`). -/
theorem rep_roundtrip_planted_u8One_badRep : Value.Storable (.int .u8 1) ∧
    ¬ badRep linuxX8664 (Value.ty (.int .u8 1)) (encodeAdmitted linuxX8664 (.int .u8 1) (by decide)) (.int .u8 1) :=
  ⟨by decide, by intro h; cases h⟩

/-- Planted (law `ConcreteSpec.Core.Hybrid.rep_roundtrip`, case `u8One`, subject `ConcreteSpec.Core.Hybrid.encodeAdmitted`, mutant `ConcreteSpec.Core.Hybrid.Audit.badEncode`). -/
theorem rep_roundtrip_planted_u8One_badEncode : Value.Storable (.int .u8 1) ∧
    ¬ Rep linuxX8664 (Value.ty (.int .u8 1)) (badEncode linuxX8664 (.int .u8 1) (by decide)) (.int .u8 1) :=
  ⟨by decide, by intro h; cases h⟩

-- 2. decode_valid
/-- Instance (law `ConcreteSpec.Core.Hybrid.decode_valid`, case `u8One`). -/
theorem decode_valid_example_u8One : match decode linuxX8664 (.int .u8) [.init 1 none] with
    | some v => v.ty = (.int .u8) ∧ v.Storable
    | none => True :=
  decode_valid linuxX8664 (.int .u8) [.init 1 none]

/-- Planted (law `ConcreteSpec.Core.Hybrid.decode_valid`, case `u8One`, subject `ConcreteSpec.Core.Hybrid.decode`, mutant `ConcreteSpec.Core.Hybrid.Audit.badDecodeUnit`). -/
theorem decode_valid_planted_u8One_badDecodeUnit : ¬ (match badDecodeUnit linuxX8664 (.int .u8) [.init 1 none] with
    | some v => v.ty = (.int .u8) ∧ v.Storable
    | none => True) := by
  change ¬ (CTy.unit = CTy.int IntTy.u8 ∧ Value.Storable .unit)
  decide

-- 3. read_structured
/-- Instance (law `ConcreteSpec.Core.Hybrid.read_structured`, case `u8One`). -/
theorem read_structured_example_u8One : structState.read linuxX8664 ⟨0⟩ = some (.int .u8 1) :=
  read_structured linuxX8664 structState ⟨0⟩ (.int .u8) (.int .u8 1) rfl rfl (by decide)

/-- Planted (law `ConcreteSpec.Core.Hybrid.read_structured`, case `u8One`, subject `ConcreteSpec.Core.Hybrid.State.read`, mutant `ConcreteSpec.Core.Hybrid.Audit.badReadNone`). -/
theorem read_structured_planted_u8One_badReadNone : structState.places ⟨0⟩ = some ⟨(.int .u8), .structured (.int .u8 1)⟩ ∧
    Value.ty (.int .u8 1) = (.int .u8) ∧ Value.Storable (.int .u8 1) ∧
    ¬ (badReadNone linuxX8664 structState ⟨0⟩ = some (.int .u8 1)) :=
  ⟨rfl, rfl, by decide, by decide⟩

-- 4. materialize_reuse
/-- Instance (law `ConcreteSpec.Core.Hybrid.materialize_reuse`, case `alreadyMat`). -/
theorem materialize_reuse_example_alreadyMat : before.materialize linuxX8664 ⟨0⟩ ⟨0⟩ 60 1 [] = some (range, before) :=
  materialize_reuse linuxX8664 before ⟨0⟩ (.int .u8) range rfl ⟨0⟩ 60 1 []

/-- Planted (law `ConcreteSpec.Core.Hybrid.materialize_reuse`, case `alreadyMat`, subject `ConcreteSpec.Core.Hybrid.State.materialize`, mutant `ConcreteSpec.Core.Hybrid.Audit.badMaterializeReallocate`). -/
theorem materialize_reuse_planted_alreadyMat_badMaterializeReallocate : matState.places ⟨0⟩ = some ⟨(.int .u8), .materialized matRange⟩ ∧
    ¬ (badMaterializeReallocate linuxX8664 matState ⟨0⟩ ⟨0⟩ 60 1 [] = some (matRange, matState)) :=
  ⟨rfl, by intro h; cases h⟩

-- 5. write_read
/-- Instance (law `ConcreteSpec.Core.Hybrid.write_read`, case `u8One`). -/
theorem write_read_example_u8One : after.read linuxX8664 ⟨0⟩ = some (.int .u8 1) :=
  write_read linuxX8664 before after before_wf.memory ⟨0⟩ (.int .u8 1) rfl

/-- Planted (law `ConcreteSpec.Core.Hybrid.write_read`, case `u8One`, subject `ConcreteSpec.Core.Hybrid.State.read`, mutant `ConcreteSpec.Core.Hybrid.Audit.badReadStructOnly`). -/
theorem write_read_planted_u8One_badReadStructOnly : before.memory.WellFormed linuxX8664 ∧
    before.write linuxX8664 ⟨0⟩ (.int .u8 1) = some after ∧
    ¬ (badReadStructOnly linuxX8664 after ⟨0⟩ = some (.int .u8 1)) :=
  ⟨before_wf.memory, rfl, by decide⟩

-- 6. typed_store_frame
/-- Instance (law `ConcreteSpec.Core.Hybrid.typed_store_frame`, case `trailingPad`). Accepted: a real typed State.write updates the integer and preserves uninitialized trailing bytes in a well-formed state. -/
theorem typed_store_frame_example_trailingPad : match before.write linuxX8664 ⟨0⟩ (.int .u8 1) with
    | some st' => (st'.memory.block ⟨0⟩).map (fun y => y.bytes[1]?) =
        some ((candidate ⟨0⟩ 40 3 1).bytes[1]?)
    | none => True :=
  typed_store_frame linuxX8664 before before_wf ⟨0⟩ (.int .u8) range (.int .u8 1) ⟨0⟩ 0 1
    (candidate ⟨0⟩ 40 3 1) rfl 1 rfl rfl rfl (.inr (by decide))

/-- Planted (law `ConcreteSpec.Core.Hybrid.typed_store_frame`, case `trailingPad`, subject `ConcreteSpec.Core.Hybrid.State.write`, mutant `ConcreteSpec.Core.Hybrid.Audit.badWriteClobber`). A typed write mutated to initialize trailing padding to zero violates the frame at byte 1 of the same well-formed state. -/
theorem typed_store_frame_planted_trailingPad_badWriteClobber : before.WellFormed linuxX8664 ∧
    before.places ⟨0⟩ = some ⟨(.int .u8), .materialized range⟩ ∧
    before.memory.access linuxX8664 range.ptr 1 range.align = some (some (⟨0⟩, 0)) ∧
    before.memory.block ⟨0⟩ = some (candidate ⟨0⟩ 40 3 1) ∧
    (1 < 0 ∨ 0 + 1 ≤ 1) ∧
    ¬ (match badWriteClobber linuxX8664 before ⟨0⟩ (.int .u8 1) with
      | some st' => (st'.memory.block ⟨0⟩).map (fun y => y.bytes[1]?) =
          some ((candidate ⟨0⟩ 40 3 1).bytes[1]?)
      | none => True) :=
  ⟨before_wf, rfl, rfl, rfl, .inr (by decide), by intro h; cases h⟩

-- 7. materialize_projection
/-- Instance (law `ConcreteSpec.Core.Hybrid.materialize_projection`, case `firstMat`). -/
theorem materialize_projection_example_firstMat : match structState.materialize linuxX8664 ⟨0⟩ ⟨0⟩ 40 1 [] with
    | some (r, st') => ∃ t, st'.places ⟨0⟩ = some ⟨t, .materialized r⟩
    | none => True :=
  materialize_projection linuxX8664 structState ⟨0⟩ ⟨0⟩ 40 1 []

/-- Planted (law `ConcreteSpec.Core.Hybrid.materialize_projection`, case `firstMat`, subject `ConcreteSpec.Core.Hybrid.State.materialize`, mutant `ConcreteSpec.Core.Hybrid.Audit.badMaterializeEmpty`). -/
theorem materialize_projection_planted_firstMat_badMaterializeEmpty : ¬ (match badMaterializeEmpty linuxX8664 structState ⟨0⟩ ⟨0⟩ 40 1 [] with
    | some (r, st') => ∃ t, st'.places ⟨0⟩ = some ⟨t, .materialized r⟩
    | none => True) := by
  rintro ⟨t, ht⟩
  cases ht

-- 8. materialize_idempotent
/-- Instance (law `ConcreteSpec.Core.Hybrid.materialize_idempotent`, case `secondCall`). -/
theorem materialize_idempotent_example_secondCall : before.materialize linuxX8664 ⟨0⟩ ⟨0⟩ 60 1 [] = some (range, before) :=
  materialize_idempotent linuxX8664 before before ⟨0⟩ ⟨0⟩ ⟨0⟩ 40 1 60 1 [] [] range rfl

/-- Instance (law `ConcreteSpec.Core.Hybrid.materialize_idempotent`, case `firstAllocation`). The first call allocates and initializes storage; a different second request reuses it. -/
theorem materialize_idempotent_example_firstAllocation : matState.materialize linuxX8664 ⟨0⟩ ⟨0⟩ 60 1 [] = some (matRange, matState) :=
  materialize_idempotent linuxX8664 structState matState ⟨0⟩ ⟨0⟩ ⟨0⟩
    40 1 60 1 [] [] matRange rfl
/-- Planted (law `ConcreteSpec.Core.Hybrid.materialize_idempotent`, case `firstAllocation`, subject `ConcreteSpec.Core.Hybrid.State.materialize`, mutant `ConcreteSpec.Core.Hybrid.Audit.badMaterializeReallocate`). -/
theorem materialize_idempotent_planted_firstAllocation_badMaterializeReallocate : badMaterializeReallocate linuxX8664 structState ⟨0⟩ ⟨0⟩ 40 1 [] = some (matRange, matState) ∧
    ¬ (badMaterializeReallocate linuxX8664 matState ⟨0⟩ ⟨0⟩ 60 1 [] = some (matRange, matState)) :=
  ⟨rfl, by intro h; cases h⟩

-- 9. write_wellFormed
/-- Instance (law `ConcreteSpec.Core.Hybrid.write_wellFormed`, case `u8One`). -/
theorem write_wellFormed_example_u8One : match before.write linuxX8664 ⟨0⟩ (.int .u8 1) with
    | some st' => st'.WellFormed linuxX8664
    | none => True :=
  write_wellFormed linuxX8664 before before_wf ⟨0⟩ (.int .u8 1)

/-- Planted (law `ConcreteSpec.Core.Hybrid.write_wellFormed`, case `u8One`, subject `ConcreteSpec.Core.Hybrid.State.write`, mutant `ConcreteSpec.Core.Hybrid.Audit.badWriteWf`). -/
theorem write_wellFormed_planted_u8One_badWriteWf : before.WellFormed linuxX8664 ∧
    ¬ (match badWriteWf linuxX8664 before ⟨0⟩ (.int .u8 1) with
      | some st' => st'.WellFormed linuxX8664
      | none => True) :=
  ⟨before_wf, not_reset_wf _ _ rfl rfl⟩

-- 10. rawWrite_wellFormed
/-- Instance (law `ConcreteSpec.Core.Hybrid.rawWrite_wellFormed`, case `oneByte`). -/
theorem rawWrite_wellFormed_example_oneByte : match before.rawWrite linuxX8664 range.ptr [.init 1 none] 1 with
    | some st' => st'.WellFormed linuxX8664
    | none => True :=
  rawWrite_wellFormed linuxX8664 before before_wf range.ptr [.init 1 none] 1

/-- Planted (law `ConcreteSpec.Core.Hybrid.rawWrite_wellFormed`, case `oneByte`, subject `ConcreteSpec.Core.Hybrid.State.rawWrite`, mutant `ConcreteSpec.Core.Hybrid.Audit.badRawWriteWf`). -/
theorem rawWrite_wellFormed_planted_oneByte_badRawWriteWf : before.WellFormed linuxX8664 ∧
    ¬ (match badRawWriteWf linuxX8664 before range.ptr [.init 1 none] 1 with
      | some st' => st'.WellFormed linuxX8664
      | none => True) :=
  ⟨before_wf, not_reset_wf _ _ rfl rfl⟩

-- 11. materialize_wellFormed
/-- Instance (law `ConcreteSpec.Core.Hybrid.materialize_wellFormed`, case `firstMat`). -/
theorem materialize_wellFormed_example_firstMat : match structState.materialize linuxX8664 ⟨0⟩ ⟨0⟩ 40 1 [] with
    | some (_, st') => st'.WellFormed linuxX8664
    | none => True :=
  materialize_wellFormed linuxX8664 structState structState_wf ⟨0⟩ ⟨0⟩ 40 1 []

/-- Planted (law `ConcreteSpec.Core.Hybrid.materialize_wellFormed`, case `firstMat`, subject `ConcreteSpec.Core.Hybrid.State.materialize`, mutant `ConcreteSpec.Core.Hybrid.Audit.badMaterializeWf`). -/
theorem materialize_wellFormed_planted_firstMat_badMaterializeWf : structState.WellFormed linuxX8664 ∧
    ¬ (match badMaterializeWf linuxX8664 structState ⟨0⟩ ⟨0⟩ 40 1 [] with
      | some (_, st') => st'.WellFormed linuxX8664
      | none => True) :=
  ⟨structState_wf, not_reset_wf _ _ rfl rfl⟩

-- 12. expose_wellFormed
/-- Instance (law `ConcreteSpec.Core.Hybrid.expose_wellFormed`, case `firstExpose`). -/
theorem expose_wellFormed_example_firstExpose : match structState.expose linuxX8664 ⟨0⟩ ⟨0⟩ 40 1 [] with
    | some (_, st') => st'.WellFormed linuxX8664
    | none => True :=
  expose_wellFormed linuxX8664 structState structState_wf ⟨0⟩ ⟨0⟩ 40 1 []

/-- Planted (law `ConcreteSpec.Core.Hybrid.expose_wellFormed`, case `firstExpose`, subject `ConcreteSpec.Core.Hybrid.State.expose`, mutant `ConcreteSpec.Core.Hybrid.Audit.badExposeStateWf`). -/
theorem expose_wellFormed_planted_firstExpose_badExposeStateWf : structState.WellFormed linuxX8664 ∧
    ¬ (match badExposeStateWf linuxX8664 structState ⟨0⟩ ⟨0⟩ 40 1 [] with
      | some (_, st') => st'.WellFormed linuxX8664
      | none => True) :=
  ⟨structState_wf, not_reset_wf _ _ rfl rfl⟩

namespace Frame

/-- An existing materialized integer at address 80, independent of the place under test. -/
def observedStart : State := ⟨[(⟨1⟩, ⟨(.int .u8), .structured (.int .u8 1)⟩)], Memory.empty⟩
def observedRange : Range := ⟨⟨⟨0⟩, 80, some ⟨0⟩⟩, 1, 1⟩
def observed : State :=
  (observedStart.materialize linuxX8664 ⟨1⟩ ⟨0⟩ 80 1 []).getD (observedRange, observedStart) |>.2
def structState : State := observed.set ⟨0⟩ ⟨(.int .u8), .structured (.int .u8 0)⟩
def matRange : Range := ⟨⟨⟨0⟩, 40, some ⟨1⟩⟩, 1, 1⟩
def matState : State :=
  (structState.materialize linuxX8664 ⟨0⟩ ⟨0⟩ 40 1 []).getD (matRange, structState) |>.2
def matExposedState : State :=
  (structState.expose linuxX8664 ⟨0⟩ ⟨0⟩ 40 1 []).getD (matRange, structState) |>.2
def before : State := matState
def after : State := (before.write linuxX8664 ⟨0⟩ (.int .u8 1)).getD before

/-- Helper (congruence). Closed structured fixture has no memory projections yet. -/
private theorem observedStart_wf : observedStart.WellFormed linuxX8664 := by
  refine ⟨empty_wellFormed linuxX8664, ?_⟩
  intro p c hc
  simp only [State.places, observedStart, AList.lookup] at hc
  split at hc
  next => cases hc; trivial
  next => cases hc
/-- Helper (congruence). Derived from a successful first materialization, retaining its invariant. -/
private theorem observed_wf : observed.WellFormed linuxX8664 :=
  materialize_wellFormed linuxX8664 observedStart observedStart_wf ⟨1⟩ ⟨0⟩ 80 1 []
/-- Helper (congruence). Adding a structured cell does not disturb the existing projection. -/
private theorem structState_wf : structState.WellFormed linuxX8664 :=
  set_wellFormed linuxX8664 observed ⟨0⟩ ⟨(.int .u8), .structured (.int .u8 0)⟩ observed_wf trivial
/-- Helper (congruence). Both allocations and their materialized projections satisfy the invariant. -/
private theorem before_wf : before.WellFormed linuxX8664 :=
  materialize_wellFormed linuxX8664 structState structState_wf ⟨0⟩ ⟨0⟩ 40 1 []
/-- Helper (computation). Both existing places name distinct live block identities. -/
private theorem before_sep : before.Separated ⟨0⟩ ⟨1⟩ := by
  intro c d r s hc hd hr hs
  have hc' : c = ⟨(.int .u8), .materialized matRange⟩ := Option.some.inj hc.symm
  have hd' : d = ⟨(.int .u8), .materialized observedRange⟩ := Option.some.inj hd.symm
  subst c; subst d
  cases hr; cases hs
  decide
/-- Helper (computation). The live intervals 40..41 and 80..81 are disjoint. -/
private theorem before_disjoint : before.RangesDisjoint ⟨0⟩ ⟨1⟩ := by
  intro c d r s hc hd hr hs
  have hc' : c = ⟨(.int .u8), .materialized matRange⟩ := Option.some.inj hc.symm
  have hd' : d = ⟨(.int .u8), .materialized observedRange⟩ := Option.some.inj hd.symm
  subst c; subst d
  cases hr; cases hs
  exact .inr (.inr (.inr (.inl (by decide))))

/-- Wrong projection lookup: use the newest materialized entry for every byte read.
Structured entries are skipped, so materializing an unrelated cell can redirect a read. -/
def badReadDiff (tgt : Target) (st : State) (q : Place) : Option Value :=
  match st.cells with
  | (_, ⟨t, .materialized r⟩) :: _ => do
    let n ← sizeof t
    let bs ← st.memory.load tgt r.ptr n r.align
    decode tgt t bs
  | _ => st.read tgt q

abbrev badReadEmptyDiff := badReadDiff

/-- Instance (laws `write_frame`, `materialize_frame`, `expose_frame`). The observed place decodes successfully before and after each transition. -/
theorem example_laws_write_frame_materialize_frame_expose_frame_observed_place : before.read linuxX8664 ⟨1⟩ = some (.int .u8 1) ∧
    after.read linuxX8664 ⟨1⟩ = some (.int .u8 1) ∧
    structState.read linuxX8664 ⟨1⟩ = some (.int .u8 1) ∧
    matExposedState.read linuxX8664 ⟨1⟩ = some (.int .u8 1) := by decide

-- 13. write_frame
/-- Instance (law `ConcreteSpec.Core.Hybrid.write_frame`, case `otherPlace`). -/
theorem write_frame_example_otherPlace : after.read linuxX8664 ⟨1⟩ = before.read linuxX8664 ⟨1⟩ :=
  write_frame linuxX8664 before after ⟨0⟩ ⟨1⟩ (.int .u8 1) (by decide) before_sep rfl

/-- Planted (law `ConcreteSpec.Core.Hybrid.write_frame`, case `otherPlace`, subject `ConcreteSpec.Core.Hybrid.State.read`, mutant `ConcreteSpec.Core.Hybrid.Audit.Frame.badReadDiff`). -/
theorem write_frame_planted_otherPlace_badReadDiff : (⟨1⟩ : Place) ≠ ⟨0⟩ ∧ before.Separated ⟨0⟩ ⟨1⟩ ∧
    before.write linuxX8664 ⟨0⟩ (.int .u8 1) = some after ∧
    ¬ (badReadDiff linuxX8664 after ⟨1⟩ = badReadDiff linuxX8664 before ⟨1⟩) :=
  ⟨by decide, before_sep, rfl, by decide⟩

-- 14. write_frame_disjoint
/-- Instance (law `ConcreteSpec.Core.Hybrid.write_frame_disjoint`, case `otherPlace`). -/
theorem write_frame_disjoint_example_otherPlace : after.read linuxX8664 ⟨1⟩ = before.read linuxX8664 ⟨1⟩ :=
  write_frame_disjoint linuxX8664 before after before_wf ⟨0⟩ ⟨1⟩ (.int .u8 1)
    (by decide) before_disjoint rfl

/-- Planted (law `ConcreteSpec.Core.Hybrid.write_frame_disjoint`, case `otherPlace`, subject `ConcreteSpec.Core.Hybrid.State.read`, mutant `ConcreteSpec.Core.Hybrid.Audit.Frame.badReadDiff`). -/
theorem write_frame_disjoint_planted_otherPlace_badReadDiff : before.WellFormed linuxX8664 ∧ (⟨1⟩ : Place) ≠ ⟨0⟩ ∧
    before.RangesDisjoint ⟨0⟩ ⟨1⟩ ∧
    before.write linuxX8664 ⟨0⟩ (.int .u8 1) = some after ∧
    ¬ (badReadDiff linuxX8664 after ⟨1⟩ = badReadDiff linuxX8664 before ⟨1⟩) :=
  ⟨before_wf, by decide, before_disjoint, rfl, by decide⟩

-- 15. materialize_frame
/-- Instance (law `ConcreteSpec.Core.Hybrid.materialize_frame`, case `otherPlace`). -/
theorem materialize_frame_example_otherPlace : matState.read linuxX8664 ⟨1⟩ = structState.read linuxX8664 ⟨1⟩ :=
  materialize_frame linuxX8664 structState matState structState_wf ⟨0⟩ ⟨1⟩
    (by decide) ⟨0⟩ 40 1 [] matRange rfl

/-- Planted (law `ConcreteSpec.Core.Hybrid.materialize_frame`, case `otherPlace`, subject `ConcreteSpec.Core.Hybrid.State.read`, mutant `ConcreteSpec.Core.Hybrid.Audit.Frame.badReadEmptyDiff`). -/
theorem materialize_frame_planted_otherPlace_badReadEmptyDiff : structState.WellFormed linuxX8664 ∧ (⟨1⟩ : Place) ≠ ⟨0⟩ ∧
    structState.materialize linuxX8664 ⟨0⟩ ⟨0⟩ 40 1 [] = some (matRange, matState) ∧
    ¬ (badReadEmptyDiff linuxX8664 matState ⟨1⟩ =
      badReadEmptyDiff linuxX8664 structState ⟨1⟩) :=
  ⟨structState_wf, by decide, rfl, by decide⟩

-- 16. expose_frame
/-- Instance (law `ConcreteSpec.Core.Hybrid.expose_frame`, case `otherPlace`). -/
theorem expose_frame_example_otherPlace : matExposedState.read linuxX8664 ⟨1⟩ = structState.read linuxX8664 ⟨1⟩ :=
  expose_frame linuxX8664 structState matExposedState structState_wf ⟨0⟩ ⟨1⟩
    (by decide) ⟨0⟩ 40 1 [] matRange rfl

/-- Planted (law `ConcreteSpec.Core.Hybrid.expose_frame`, case `otherPlace`, subject `ConcreteSpec.Core.Hybrid.State.read`, mutant `ConcreteSpec.Core.Hybrid.Audit.Frame.badReadEmptyDiff`). -/
theorem expose_frame_planted_otherPlace_badReadEmptyDiff : structState.WellFormed linuxX8664 ∧ (⟨1⟩ : Place) ≠ ⟨0⟩ ∧
    structState.expose linuxX8664 ⟨0⟩ ⟨0⟩ 40 1 [] = some (matRange, matExposedState) ∧
    ¬ (badReadEmptyDiff linuxX8664 matExposedState ⟨1⟩ =
      badReadEmptyDiff linuxX8664 structState ⟨1⟩) :=
  ⟨structState_wf, by decide, rfl, by decide⟩

end Frame

-- 17. materialize_read
/-- Instance (law `ConcreteSpec.Core.Hybrid.materialize_read`, case `firstMat`). -/
theorem materialize_read_example_firstMat : matState.read linuxX8664 ⟨0⟩ = some (.int .u8 1) :=
  materialize_read linuxX8664 structState matState structState_wf.memory ⟨0⟩ (.int .u8 1)
    ⟨0⟩ 40 1 [] matRange rfl rfl

/-- Planted (law `ConcreteSpec.Core.Hybrid.materialize_read`, case `firstMat`, subject `ConcreteSpec.Core.Hybrid.State.read`, mutant `ConcreteSpec.Core.Hybrid.Audit.badReadStructOnly`). -/
theorem materialize_read_planted_firstMat_badReadStructOnly : structState.memory.WellFormed linuxX8664 ∧
    badReadStructOnly linuxX8664 structState ⟨0⟩ = some (.int .u8 1) ∧
    structState.materialize linuxX8664 ⟨0⟩ ⟨0⟩ 40 1 [] = some (matRange, matState) ∧
    ¬ (badReadStructOnly linuxX8664 matState ⟨0⟩ = some (.int .u8 1)) :=
  ⟨structState_wf.memory, rfl, rfl, by decide⟩

/-- Successful raw overwrite of the already materialized integer. -/
def rawAfter : State :=
  (matState.rawWrite linuxX8664 matRange.ptr [.init 2 none] 1).getD matState

/-- Wrong rule: keep reading the old value after a raw overwrite. -/
def staleRead (tgt : Target) (st : State) (p : Place) : Option Value :=
  match st.cells.find? (fun e => e.1 == p && match e.2.storage with
    | .structured _ => true
    | .materialized _ => false) with
  | some (_, ⟨_, .structured v⟩) => some v
  | _ => st.read tgt p

/-- Instance (law `ConcreteSpec.Core.Hybrid.rawWrite_read`, case `overwrite`). -/
theorem rawWrite_read_example_overwrite : rawAfter.read linuxX8664 ⟨0⟩ = decode linuxX8664 (.int .u8) [.init 2 none] :=
  rawWrite_read linuxX8664 matState rawAfter
    (materialize_wellFormed linuxX8664 structState structState_wf ⟨0⟩ ⟨0⟩ 40 1 []).memory
    ⟨0⟩ (.int .u8) matRange [.init 2 none] rfl rfl rfl

/-- Planted (law `ConcreteSpec.Core.Hybrid.rawWrite_read`, case `overwrite`). The same
well-formed state, projection, byte count and successful raw write reject a stale typed read. -/
theorem rawWrite_read_planted_overwrite : matState.memory.WellFormed linuxX8664 ∧
    matState.places ⟨0⟩ = some ⟨.int .u8, .materialized matRange⟩ ∧
    (sizeof (.int .u8) = some [MByte.init 2 none].length) ∧
    matState.rawWrite linuxX8664 matRange.ptr [.init 2 none] 1 = some rawAfter ∧
    ¬ (staleRead linuxX8664 rawAfter ⟨0⟩ = decode linuxX8664 (.int .u8) [.init 2 none]) :=
  ⟨(materialize_wellFormed linuxX8664 structState structState_wf ⟨0⟩ ⟨0⟩ 40 1 []).memory,
    rfl, rfl, rfl, by decide⟩

end Audit

end ConcreteSpec.Core.Hybrid
