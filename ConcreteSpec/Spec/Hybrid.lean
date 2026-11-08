import ConcreteSpec.Spec.Memory
import ConcreteSpec.Spec.Exec

/-!
# Hybrid typed places and byte memory (foundation C4, question 2)

PROPOSED: not normative until the owner freezes Core. Q3.11 joins structured storage and bytes
by `Rep`. A cell is either structured or materialized, never both: there is no typed cache to
invalidate. Materialized reads decode the current bytes, and failure supplies no value. Write
permission and ownership remain premises of C5, as for `Memory.store`.

This module covers scalar integers and bools. The containing storage may include trailing
padding, supplied explicitly once at materialization (the C8 oracle interface is later).
Typed writes touch only the active scalar prefix, preserving padding, initialization and
fragments outside it. Struct field layout, enums, pointers in structs and the C8 padding oracle
are left out. Pointer decoding stays exactly as in Memory; no new pointer meaning is chosen.

`State` is an `Exec.Operands.Store` shape: instantiate `Store := State`, use `read`/`write` in
operand evaluation and `rawWrite` for authorized primitive writes; turn `none` into `Res.stuck`.
C5 supplies lifecycle ownership and permissions; C6 supplies primitive contracts. No change to
Exec is needed. Local variable identities and place identities are separate; the operand instance
must resolve a local's place before using these operations.
-/
namespace ConcreteSpec.Core.Hybrid
open ConcreteSpec.Core.Mem

/-- The scalar types represented in this slice. -/
inductive Ty where
  | int (t : IntTy)
  | bool
  deriving DecidableEq, Repr

/-- Scalar values. Validity is explicit, so an out-of-range integer cannot be safely stored. -/
inductive Value where
  | int (t : IntTy) (n : Int)
  | bool (b : Bool)
  deriving DecidableEq, Repr

/-- The scalar's type. -/
def Value.ty : Value → Ty
  | .int t _ => .int t
  | .bool _ => .bool

/-- Integer values are in their declared range. -/
def Value.Valid : Value → Prop
  | .int t n => t.InRange n
  | .bool _ => True

instance (v : Value) : Decidable v.Valid := by
  cases v <;> dsimp [Value.Valid] <;> infer_instance

/-- Active byte count, a pure layout query: no state is read or materialized. -/
def sizeof : Ty → Nat
  | .int t => intBytes t
  | .bool => 1

/-- Alignment is a read-only layout input; querying it never materializes a place. -/
def alignof (layoutAlign : Ty → Nat) (t : Ty) : Nat := layoutAlign t

/-- Active scalar bytes, with no padding choices. -/
def encode (tgt : Target) : Value → List MByte
  | .int t n => encodeInt tgt t n
  | .bool b => encodeBool b

/-- Partial decoding of the current active bytes. -/
def decode (tgt : Target) : Ty → List MByte → Option Value
  | .int t, bs => (decodeInt tgt t bs).map (.int t)
  | .bool, bs => (decodeBool bs).map .bool

/-- The representation relation; padding is outside the active slice passed to decoding. -/
def Rep (tgt : Target) (t : Ty) (bs : List MByte) (v : Value) : Prop :=
  decode tgt t bs = some v

/-- Stable projection of a materialized place to a block/range; `extent` includes padding. -/
structure Range where
  ptr : Ptr
  extent : Nat
  align : Nat
  deriving Repr

/-- A single authoritative storage form. -/
inductive Storage where
  | structured (v : Value)
  | materialized (r : Range)
  deriving Repr

/-- A typed place, with its storage form. -/
structure Cell where
  ty : Ty
  storage : Storage
  deriving Repr

/-- Fresh logical place identities, independent of numeric addresses. -/
structure Place where
  id : Nat
  deriving DecidableEq, Repr

/-- The hybrid store. Functions model the finite place map abstractly; all updates below replace
one entry. Byte memory is the sole authority for materialized places. -/
structure State where
  places : Place → Option Cell
  memory : Memory

/-- Replace a logical place without changing other entries. -/
def State.set (st : State) (p : Place) (c : Cell) : State :=
  { st with places := fun q => if q = p then some c else st.places q }

/-- Safe typed read. Materialized storage is decoded on every read. -/
def State.read (tgt : Target) (st : State) (p : Place) : Option Value := do
  let c ← st.places p
  match c.storage with
  | .structured v => if v.ty = c.ty ∧ v.Valid then some v else none
  | .materialized r =>
    let bs ← st.memory.load tgt r.ptr (sizeof c.ty) r.align
    decode tgt c.ty bs

/-- Safe typed write. No padding argument exists: materialized writes encode only active bytes.
The caller supplies ownership and write permission (C5). -/
def State.write (tgt : Target) (st : State) (p : Place) (v : Value) : Option State := do
  let c ← st.places p
  if v.ty = c.ty ∧ v.Valid then
    match c.storage with
    | .structured _ => some (st.set p { c with storage := .structured v })
    | .materialized r =>
      let m ← st.memory.store tgt r.ptr (encode tgt v) r.align
      some { st with memory := m }
  else none

/-- First raw address-taking materializes containing storage. The allocator address, alignment
and initial padding are explicit inputs. Existing materialized storage is reused, even if those
inputs differ. Layout queries and safe borrowing have no call to this operation. -/
def State.materialize (tgt : Target) (st : State) (p : Place) (space : AddrSpace)
    (base align : Nat) (padding : List MByte) : Option (Range × State) := do
  let c ← st.places p
  match c.storage with
  | .materialized r => some (r, st)
  | .structured v =>
    if v.ty = c.ty ∧ v.Valid then
      let bs := encode tgt v ++ padding
      let (b, m) ← (st.memory.alloc tgt space base bs.length align).toOption
      let r : Range := ⟨⟨space, base, some b⟩, bs.length, align⟩
      let m ← m.store tgt r.ptr bs align
      some (r, ({ st with memory := m }).set p { c with storage := .materialized r })
    else none

/-- Exposing a place first materializes it, then records its block as exposed. -/
def State.expose (tgt : Target) (st : State) (p : Place) (space : AddrSpace)
    (base align : Nat) (padding : List MByte) : Option (Range × State) := do
  let (r, st) ← st.materialize tgt p space base align padding
  let m ← st.memory.expose r.ptr
  some (r, { st with memory := m })

/-- Raw writes update the only byte store. There are no materialized typed caches to refresh. -/
def State.rawWrite (tgt : Target) (st : State) (p : Ptr) (bs : List MByte) (align : Nat) :
    Option State := do
  let m ← st.memory.store tgt p bs align
  some { st with memory := m }

/-- Accepted: a structured bool is read without allocating byte storage. -/
example : (State.mk (fun _ => some ⟨.bool, .structured (.bool true)⟩) Memory.empty).read
    linuxX8664 ⟨0⟩ = some (.bool true) := by decide

/-- Refused: invalid bytes at a materialized bool do not fall back to a structured value. -/
example : decode linuxX8664 .bool [.init 2 none] = none := by decide

/-- Accepted: materialize once with uninitialized and initialized padding, rewrite the bool,
then observe exactly the original padding. Refused: a raw write of byte 2 leaves no bool value. -/
example : (do
    let st : State := ⟨fun p => if p = ⟨0⟩ then some ⟨.bool, .structured (.bool true)⟩ else none,
      Memory.empty⟩
    let (r, st) ← st.materialize linuxX8664 ⟨0⟩ ⟨0⟩ 4096 1 [.uninit, .init 73 none]
    let st ← st.write linuxX8664 ⟨0⟩ (.bool false)
    let typed := st.read linuxX8664 ⟨0⟩
    let padding := st.memory.load linuxX8664 { r.ptr with addr := 4097 } 2 1
    let st ← st.rawWrite linuxX8664 r.ptr [.init 2 none] 1
    pure (typed, padding, st.read linuxX8664 ⟨0⟩)) =
      some (some (.bool false), some [.uninit, .init 73 none], none) := by decide

/-- Accepted: another raw address-taking reuses the first mapping and first padding, even
when given different proposed storage and padding. -/
example : (do
    let st : State := ⟨fun _ => some ⟨.bool, .structured (.bool true)⟩, Memory.empty⟩
    let (r, st) ← st.materialize linuxX8664 ⟨0⟩ ⟨0⟩ 4096 1 [.init 73 none]
    let (r', st) ← st.materialize linuxX8664 ⟨0⟩ ⟨0⟩ 8192 8 [.init 99 none]
    pure (r.ptr == r'.ptr, st.memory.load linuxX8664 r'.ptr 2 1)) =
      some (true, some [.init 1 none, .init 73 none]) := by decide

end ConcreteSpec.Core.Hybrid
