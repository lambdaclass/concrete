import ConcreteSpec.Spec.Memory
import ConcreteSpec.Spec.Exec
import ConcreteSpec.Spec.Rep

/-!
# Hybrid typed places and byte memory (foundation C4, question 2)

PROPOSED: not normative until the owner freezes Core. Q3.11 joins structured storage and bytes
by `Rep`. A cell is either structured or materialized, never both: there is no typed cache to
invalidate. Materialized reads decode the current bytes, and failure supplies no value. Write
permission and ownership remain premises of C5, as for `Memory.store`.

This piece covers integer cells only. Boolean encoding is held (ISS-0069), as are
struct layout and whole-pointer representation (ISS-0070; F12/F33 owner hold).
Character and unit layouts are held under ISS-0076; neither has a representation here.
Encoding refuses unadmitted values, and the underlying byte encoder requires a `Storable`
admission proof. Padding is explicit uninterpreted storage, not a field-layout algorithm.

Authority in `docs/decisions.md`: #foundation-q2 approves structured places with byte/provenance
blocks and materialization on raw address-taking or exposure; #decided-2026-11-05-core,
Q3.11 joins them by representation. Q3.7 supplies initialized-byte access and write premises;
Q3.9 supplies exposure; #foundation-q3 and #od126 supply target byte order and integer widths.
No historical reviewer receipt supplies authority. The proof module checks agreement with the
existing integer representation in Spec/Rep on its fixed little-endian profile.

Held independently: ISS-0064 (U+FEFF/bare CR), ISS-0065 (queued partial items and constant
restrictions), ISS-0068 (paths), ISS-0071 (character preconditions), ISS-0072 (wiping),
ISS-0073 (startup/ABI). None supplies a definition here.

`State` is an `Exec.Operands.Store` shape: instantiate `Store := State`, use `read`/`write` in
operand evaluation and `rawWrite` for authorized primitive writes; turn `none` into `Res.stuck`.
C5 supplies lifecycle ownership and permissions; C6 supplies the request/response interface. No change to
Exec is needed. Local variable identities and place identities are separate; the operand instance
must resolve a local's place before using these operations.
-/
namespace ConcreteSpec.Core.Hybrid
open ConcreteSpec.Core.Mem

/-- The shared scalar type universe; this byte-storage slice admits integers only. -/
abbrev Ty := CTy

/-- Values share the constant subset's scalar model, defined once in Scalar. -/
abbrev Value := CVal

abbrev Value.ty (v : Value) : Ty := CVal.ty v
abbrev Value.Valid (v : Value) : Prop := CVal.Valid v

/-- Byte storage in this slice supports integers. Boolean encoding (ISS-0069) and character/unit layouts (ISS-0076) are held;
sharing the common value universe does not add unit layout or materialization rules. -/
def Value.Storable (v : Value) : Prop := v.Valid ∧ v.ty ≠ .unit ∧ v.ty ≠ .bool ∧ v.ty ≠ .char

instance (v : Value) : Decidable v.Storable := by
  cases v <;> dsimp [Value.Storable, Value.Valid, Value.ty, CVal.Valid, CVal.ty] <;> infer_instance

/-- Adapter to the existing slice representation's integer names. No layout is chosen here. -/
def sliceIntTy : IntTy → ConcreteSpec.IntTy
  | .i8 => .i8 | .i16 => .i16 | .i32 => .i32 | .i64 => .i64
  | .u8 => .u8 | .u16 => .u16 | .u32 => .u32 | .u64 => .u64

/-- Partial layout query, delegating to the existing Rep scalar layout. Boolean size is
settled by OD126 even though its byte encoding is held (ISS-0069). Character and unit
layout lack admitted authority (ISS-0076); refusal is distinct from a zero-byte layout. -/
def sizeof : Ty → Option Nat
  | .int t => some (ConcreteSpec.Rep.scalarSize (.int (sliceIntTy t)))
  | .bool => some (ConcreteSpec.Rep.scalarSize .bool)
  | .unit | .char => none

/-- Instance. The encoding hold does not hold the decided one-byte Boolean layout. -/
example : sizeof .bool = some 1 := rfl

/-- Instance. Missing layout authority refuses a query, rather than claiming zero bytes. -/
example : sizeof .char = none ∧ sizeof .unit = none := ⟨rfl, rfl⟩

/-- Alignment is a read-only layout input; querying it never materializes a place. -/
def alignof (layoutAlign : Ty → Nat) (t : Ty) : Nat := layoutAlign t

/-- Active scalar bytes, restricted to admitted values, with no padding choices.
A held representation cannot supply the required admission proof. -/
def encodeAdmitted (tgt : Target) (v : Value) (h : v.Storable) : List MByte :=
  match v with
  | .int t n => encodeInt tgt t n
  | .bool _ => False.elim (h.2.2.1 rfl)
  | .unit => False.elim (h.2.1 rfl)
  | .char _ => False.elim (h.2.2.2 rfl)

/-- Partial encoding, with the same explicit refusal shape as `sizeof`.
Only admitted values reach the restricted encoder; refusal supplies no representation bytes. -/
def encode (tgt : Target) (v : Value) : Option (List MByte) :=
  if hv : v.Storable then some (encodeAdmitted tgt v hv) else none

/-- Partial decoding of the current active bytes. -/
def decode (tgt : Target) : Ty → List MByte → Option Value
  | .int t, bs => (decodeInt tgt t bs).map (.int t)
  | .bool, _ => none
  | .unit, _ | .char, _ => none

/-- The representation relation; padding is outside the active slice passed to decoding. -/
def Rep (tgt : Target) (t : Ty) (bs : List MByte) (v : Value) : Prop :=
  decode tgt t bs = some v

/-- Forget fragment bookkeeping when comparing integer bytes with Spec/Rep.
Integers carry no provenance (Q3.7); uninitialized bytes stay uninitialized. -/
def sliceByte : MByte → ConcreteSpec.Rep.MByte
  | .uninit => .undef
  | .init b _ => .byte b

/-- Stable projection of a materialized place to a block/range; `extent` includes padding. -/
structure Range where
  ptr : Ptr
  extent : Nat
  align : Nat
  deriving Repr

/-- Occupied byte intervals are disjoint when either is empty, they are in different spaces,
or one interval
ends before the other starts. Endpoints are excluded, so adjacent ranges are disjoint. -/
def Range.Disjoint (r s : Range) : Prop :=
  r.extent = 0 ∨ s.extent = 0 ∨ r.ptr.space ≠ s.ptr.space ∨ r.ptr.addr + r.extent ≤ s.ptr.addr ∨
    s.ptr.addr + s.extent ≤ r.ptr.addr

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
      r.align = x.align ∧ ∃ n, sizeof c.ty = some n ∧ n ≤ r.extent

/-- Byte memory is well-formed and every materialized scalar has a valid containing projection. -/
structure State.WellFormed (tgt : Target) (st : State) : Prop where
  memory : st.memory.WellFormed tgt
  projected : ∀ p c, st.places p = some c → c.Projected st.memory

/-- For this whole-cell slice, separated projections use distinct allocation identities.
This is stronger than numeric non-overlap and permits address reuse across dead blocks. -/
def State.Separated (st : State) (p q : Place) : Prop :=
  ∀ c d r s, st.places p = some c → st.places q = some d →
    c.storage = .materialized r → d.storage = .materialized s → r.ptr.prov ≠ s.ptr.prov

/-- The precise byte-range frame premise. Structured cells impose no byte-range obligation. -/
def State.RangesDisjoint (st : State) (p q : Place) : Prop :=
  ∀ c d r s, st.places p = some c → st.places q = some d →
    c.storage = .materialized r → d.storage = .materialized s → r.Disjoint s

/-- Replace a logical place without changing other entries. -/
def State.set (st : State) (p : Place) (c : Cell) : State :=
  { st with cells := (p, c) :: st.cells }

/-- Representation lemma. Place lookup after State.set reduces to association-list cons. -/
@[simp] theorem State.places_set (st : State) (p q : Place) (c : Cell) :
    (st.set p c).places q = if p = q then some c else st.places q := rfl

/-- Representation lemma. Updating the byte-memory field preserves place lookup. -/
@[simp] theorem State.places_memory (st : State) (m : Memory) (p : Place) :
    ({ st with memory := m } : State).places p = st.places p := rfl

/-- Representation lemma. Updating a logical place preserves the byte-memory field. -/
@[simp] theorem State.memory_set (st : State) (p : Place) (c : Cell) :
    (st.set p c).memory = st.memory := rfl

/-- Safe typed read. Materialized storage is decoded on every read. -/
def State.read (tgt : Target) (st : State) (p : Place) : Option Value := do
  let c ← st.places p
  match c.storage with
  | .structured v => if v.ty = c.ty ∧ v.Storable then some v else none
  | .materialized r =>
    let n ← sizeof c.ty
    let bs ← st.memory.load tgt r.ptr n r.align
    decode tgt c.ty bs

/-- Safe typed write. No padding argument exists: materialized writes encode only active bytes.
The caller supplies ownership and write permission (C5). -/
def State.write (tgt : Target) (st : State) (p : Place) (v : Value) : Option State := do
  let c ← st.places p
  if hv : v.ty = c.ty ∧ v.Storable then
    match c.storage with
    | .structured _ => some (st.set p { c with storage := .structured v })
    | .materialized r =>
      let m ← st.memory.store tgt r.ptr (encodeAdmitted tgt v hv.2) r.align
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
    if hv : v.ty = c.ty ∧ v.Storable then
      let bs := encodeAdmitted tgt v hv.2 ++ padding
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

/-- Instance: an integer is readable before taking its raw address. -/
example : (State.mk [(⟨0⟩, ⟨.int .u8, .structured (.int .u8 7)⟩)] Memory.empty).read
    linuxX8664 ⟨0⟩ = some (.int .u8 7) := by decide

/-- Instance: Boolean byte storage is outside this piece while its encoding is held. -/
example : (State.mk [(⟨0⟩, ⟨.bool, .structured (.bool true)⟩)] Memory.empty).read
    linuxX8664 ⟨0⟩ = none := by decide

/-- Instance (law `materialize_read`). First materialization preserves a signed integer
on a big-endian profile, independently of the trailing byte's initialization. -/
example : (do
    let st : State := ⟨[(⟨0⟩, ⟨.int .i16, .structured (.int .i16 (-37))⟩)], Memory.empty⟩
    let (_, out) ← st.materialize { linuxX8664 with byteOrder := .big } ⟨0⟩ ⟨0⟩ 4096 2 [.uninit]
    pure (out.read { linuxX8664 with byteOrder := .big } ⟨0⟩)) =
      some (some (.int .i16 (-37))) := by decide

/-- Instance. Sharing the scalar universe does not infer unit byte layout. -/
example : (State.mk [(⟨0⟩, ⟨.unit, .structured .unit⟩)] Memory.empty).read
    linuxX8664 ⟨0⟩ = none := by decide

/-- Instance (laws `typed_store_frame`, `rawWrite_read`). A typed write preserves both
initialized and uninitialized trailing bytes. A raw overwrite with an uninitialized active
byte then refuses a typed value; there is no old-value fallback. -/
example : (do
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
example : (do
    let st : State := ⟨[(⟨0⟩, ⟨.int .u8, .structured (.int .u8 1)⟩)], Memory.empty⟩
    let (r, st) ← st.materialize linuxX8664 ⟨0⟩ ⟨0⟩ 4096 1 [.init 73 none]
    let (r', st) ← st.materialize linuxX8664 ⟨0⟩ ⟨0⟩ 8192 8 [.init 99 none]
    pure (r.ptr == r'.ptr, st.memory.load linuxX8664 r'.ptr 2 1)) =
      some (true, some [.init 1 none, .init 73 none]) := by decide

end ConcreteSpec.Core.Hybrid
