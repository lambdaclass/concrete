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

Pointer-typed places await the whole-pointer decoding owner decision; their sizeof will take
the target.

`State` is an `Exec.Operands.Store` shape: instantiate `Store := State`, use `read`/`write` in
operand evaluation and `rawWrite` for authorized primitive writes; turn `none` into `Res.stuck`.
C5 supplies lifecycle ownership and permissions; C6 supplies the request/response interface. No change to
Exec is needed. Local variable identities and place identities are separate; the operand instance
must resolve a local's place before using these operations.
-/
namespace ConcreteSpec.Core.Hybrid
open ConcreteSpec.Core.Mem

/-- The common scalar type universe; this byte-storage slice admits int/bool only. -/
abbrev Ty := CTy

/-- Values share the constant subset's scalar model, defined once in Scalar. -/
abbrev Value := CVal

abbrev Value.ty (v : Value) : Ty := CVal.ty v
abbrev Value.Valid (v : Value) : Prop := CVal.Valid v

/-- Byte storage in this slice supports integers and bools. Unit has no representation here;
sharing the common value universe does not add unit layout or materialization rules. -/
def Value.Storable (v : Value) : Prop := v.Valid ∧ v.ty ≠ .unit

instance (v : Value) : Decidable v.Storable := by
  cases v <;> dsimp [Value.Storable, Value.Valid, Value.ty, CVal.Valid, CVal.ty] <;> infer_instance

/-- Active byte count for supported int/bool types, a pure layout query. The zero for
unsupported unit is a sentinel, not a unit layout rule; unit storage is refused. -/
def sizeof : Ty → Nat
  | .int t => intBytes t
  | .bool => 1
  | .unit => 0

/-- Alignment is a read-only layout input; querying it never materializes a place. -/
def alignof (layoutAlign : Ty → Nat) (t : Ty) : Nat := layoutAlign t

/-- Active scalar bytes, with no padding choices. -/
def encode (tgt : Target) : Value → List MByte
  | .int t n => encodeInt tgt t n
  | .bool b => encodeBool b
  | .unit => []

/-- Partial decoding of the current active bytes. -/
def decode (tgt : Target) : Ty → List MByte → Option Value
  | .int t, bs => (decodeInt tgt t bs).map (.int t)
  | .bool, bs => (decodeBool bs).map .bool
  | .unit, _ => none

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

/-- The hybrid store uses the common finite association list. Byte memory is the sole
authority for materialized places. -/
structure State where
  cells : AList Place Cell
  memory : Memory

/-- Lookup in the finite place map. -/
def State.places (st : State) (p : Place) : Option Cell := AList.lookup st.cells p

/-- A projection names a whole containing block, with room for the active scalar bytes.
This slice materializes whole scalar cells; field subranges belong to the struct-layout slice. -/
def Cell.Projected (m : Memory) (c : Cell) : Prop :=
  match c.storage with
  | .structured _ => True
  | .materialized r => ∃ b x, r.ptr.prov = some b ∧ m.block b = some x ∧
      r.ptr.space = x.space ∧ r.ptr.addr = x.base ∧ r.extent = x.size ∧
      r.align = x.align ∧ sizeof c.ty ≤ r.extent

/-- Byte memory is well-formed and every materialized scalar has a valid containing projection. -/
structure State.WellFormed (tgt : Target) (st : State) : Prop where
  memory : st.memory.WellFormed tgt
  projected : ∀ p c, st.places p = some c → c.Projected st.memory

/-- For this whole-cell slice, separated projections use distinct allocation identities.
This is stronger than numeric non-overlap and permits address reuse across dead blocks. -/
def State.Separated (st : State) (p q : Place) : Prop :=
  ∀ c d r s, st.places p = some c → st.places q = some d →
    c.storage = .materialized r → d.storage = .materialized s → r.ptr.prov ≠ s.ptr.prov

/-- Replace a logical place without changing other entries. -/
def State.set (st : State) (p : Place) (c : Cell) : State :=
  { st with cells := (p, c) :: st.cells }

@[simp] theorem State.places_set (st : State) (p q : Place) (c : Cell) :
    (st.set p c).places q = if p = q then some c else st.places q := rfl

@[simp] theorem State.places_memory (st : State) (m : Memory) (p : Place) :
    ({ st with memory := m } : State).places p = st.places p := rfl

@[simp] theorem State.memory_set (st : State) (p : Place) (c : Cell) :
    (st.set p c).memory = st.memory := rfl

/-- Safe typed read. Materialized storage is decoded on every read. -/
def State.read (tgt : Target) (st : State) (p : Place) : Option Value := do
  let c ← st.places p
  match c.storage with
  | .structured v => if v.ty = c.ty ∧ v.Storable then some v else none
  | .materialized r =>
    let bs ← st.memory.load tgt r.ptr (sizeof c.ty) r.align
    decode tgt c.ty bs

/-- Safe typed write. No padding argument exists: materialized writes encode only active bytes.
The caller supplies ownership and write permission (C5). -/
def State.write (tgt : Target) (st : State) (p : Place) (v : Value) : Option State := do
  let c ← st.places p
  if v.ty = c.ty ∧ v.Storable then
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
    if v.ty = c.ty ∧ v.Storable then
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
example : (State.mk [(⟨0⟩, ⟨.bool, .structured (.bool true)⟩)] Memory.empty).read
    linuxX8664 ⟨0⟩ = some (.bool true) := by decide

/-- Accepted: first materialization preserves an integer's value, including its byte order. -/
example : (do
    let st : State := ⟨[(⟨0⟩, ⟨.int .i16, .structured (.int .i16 (-37))⟩)], Memory.empty⟩
    let (_, st) ← st.materialize { linuxX8664 with byteOrder := .big } ⟨0⟩ ⟨0⟩ 4096 2 [.uninit]
    pure (st.read { linuxX8664 with byteOrder := .big } ⟨0⟩)) =
      some (some (.int .i16 (-37))) := by decide

/-- Refused: sharing the scalar universe with constants does not admit unit storage. -/
example : (State.mk [(⟨0⟩, ⟨.unit, .structured .unit⟩)] Memory.empty).read
    linuxX8664 ⟨0⟩ = none := by decide

/-- Refused: invalid bytes at a materialized bool do not fall back to a structured value. -/
example : decode linuxX8664 .bool [.init 2 none] = none := by decide

/-- Accepted: materialize once with uninitialized and initialized padding, rewrite the bool,
then observe exactly the original padding. Refused: a raw write of byte 2 leaves no bool value. -/
example : (do
    let st : State := ⟨[(⟨0⟩, ⟨.bool, .structured (.bool true)⟩)],
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
    let st : State := ⟨[(⟨0⟩, ⟨.bool, .structured (.bool true)⟩)], Memory.empty⟩
    let (r, st) ← st.materialize linuxX8664 ⟨0⟩ ⟨0⟩ 4096 1 [.init 73 none]
    let (r', st) ← st.materialize linuxX8664 ⟨0⟩ ⟨0⟩ 8192 8 [.init 99 none]
    pure (r.ptr == r'.ptr, st.memory.load linuxX8664 r'.ptr 2 1)) =
      some (true, some [.init 1 none, .init 73 none]) := by decide

end ConcreteSpec.Core.Hybrid
