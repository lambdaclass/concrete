import ConcreteSpec.Spec.Scalar
import ConcreteSpec.Spec.Target
import ConcreteSpec.Spec.Memory
import ConcreteSpec.Spec.Hybrid

namespace ConcreteSpec.Core.Hybrid
open ConcreteSpec.Core.Mem

/-- Instance. The encoding hold does not hold the decided one-byte Boolean layout. -/
theorem example_encoding_hold_does_not_hold_decided : sizeof .bool = some 1 := rfl

/-- Instance. Missing layout authority refuses a query, rather than claiming zero bytes. -/
theorem example_Missing_layout_authority_refuses_query_rather : sizeof .char = none ∧ sizeof .unit = none := ⟨rfl, rfl⟩

/-- Instance: an integer is readable before taking its raw address. -/
theorem example_integer_is_readable_before_taking_its : (State.mk [(⟨0⟩, ⟨.int .u8, .structured (.int .u8 7)⟩)] Memory.empty).read
    linuxX8664 ⟨0⟩ = some (.int .u8 7) := by decide

/-- Instance: Boolean byte storage is outside this piece while its encoding is held. -/
theorem example_Boolean_byte_storage_is_outside_this : (State.mk [(⟨0⟩, ⟨.bool, .structured (.bool true)⟩)] Memory.empty).read
    linuxX8664 ⟨0⟩ = none := by decide

/-- Instance (law `materialize_read`). First materialization preserves a signed integer
on a big-endian profile, independently of the trailing byte's initialization. -/
theorem materialize_read_example : (do
    let st : State := ⟨[(⟨0⟩, ⟨.int .i16, .structured (.int .i16 (-37))⟩)], Memory.empty⟩
    let (_, out) ← st.materialize { linuxX8664 with byteOrder := .big } ⟨0⟩ ⟨0⟩ 4096 2 [.uninit]
    pure (out.read { linuxX8664 with byteOrder := .big } ⟨0⟩)) =
      some (some (.int .i16 (-37))) := by decide

/-- Instance. Sharing the scalar universe does not infer unit byte layout. -/
theorem example_Sharing_scalar_universe_does_not_infer : (State.mk [(⟨0⟩, ⟨.unit, .structured .unit⟩)] Memory.empty).read
    linuxX8664 ⟨0⟩ = none := by decide

/-- Instance (laws `typed_store_frame`, `rawWrite_read`). A typed write preserves both
initialized and uninitialized trailing bytes. A raw overwrite with an uninitialized active
byte then refuses a typed value; there is no old-value fallback. -/
theorem example_laws_typed_store_frame_rawWrite_read_typed_write_preserves : (do
    let st : State := ⟨[(⟨0⟩, ⟨.int .u8, .structured (.int .u8 1)⟩)], Memory.empty⟩
    let (r, st) ← st.materialize linuxX8664 ⟨0⟩ ⟨0⟩ 4096 1 [.uninit, .init 73 none]
    let st ← st.write linuxX8664 ⟨0⟩ (.int .u8 2)
    let typed := st.read linuxX8664 ⟨0⟩
    let padding := st.memory.load linuxX8664 { r.ptr with addr := 4097 } 2 1
    let st ← st.rawWrite linuxX8664 r.ptr [.uninit] 1
    pure (typed, padding, st.read linuxX8664 ⟨0⟩)) =
      some (some (.int .u8 2), some [.uninit, .init 73 none], none) := by decide

/-- Instance (law `materialize_reuse`). Later raw address-taking keeps the first mapping
and padding even when proposed addresses, alignment and trailing bytes differ. -/
theorem materialize_reuse_example : (do
    let st : State := ⟨[(⟨0⟩, ⟨.int .u8, .structured (.int .u8 1)⟩)], Memory.empty⟩
    let (r, st) ← st.materialize linuxX8664 ⟨0⟩ ⟨0⟩ 4096 1 [.init 73 none]
    let (r', st) ← st.materialize linuxX8664 ⟨0⟩ ⟨0⟩ 8192 8 [.init 99 none]
    pure (r.ptr == r'.ptr, st.memory.load linuxX8664 r'.ptr 2 1)) =
      some (true, some [.init 1 none, .init 73 none]) := by decide

end ConcreteSpec.Core.Hybrid
