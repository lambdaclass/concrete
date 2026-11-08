import ConcreteSpec.Spec.Target
import ConcreteSpec.Spec.Scalar

/-!
# Byte memory: blocks, provenance, access validity and scalar representation

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). Part of `ConcreteSpec.Spec`;
see `ConcreteSpec/README.md`. Foundation contract C4, first part: the raw store (rows Q3.7–Q3.10)
and the byte representation of scalars (Q3.11's `Rep`, for integers and `bool`). The hybrid state
(structured places with a partial projection to these blocks, materialization, padding, and typed
writes to materialized storage) is the second part and is not here.

**Blocks** (Q3.7). Memory is a set of blocks. A block has a fresh identity that is never reused,
even when its addresses are (Q3.9: a stale pointer keeps its old block, and every access through
it fails liveness), an address space, a base address, a size, an alignment, whether it is live,
whether it has been exposed, and its bytes. Every block satisfies `base + size ≤ 2^addrBits − 1`, so
its one-past address is an address (Q3.10, generalized to the profile's address width by the
owner's question 3 of 2026-11-08). Live blocks never overlap in occupied bytes (Q3.10: a collision
means overlapping occupied bytes; a zero-sized block occupies none, Q3.8).

**Bytes.** A byte is uninitialized, or an initialized octet; an initialized byte may in addition
be a fragment of a stored pointer, carrying that pointer and the byte's index in it (proposal C4:
pointer fragments carry provenance separately from the numeric byte). There is no poison value.

**Pointers** (Q3.7). A pointer is an address space, an address and a provenance: the block it was
derived from, or none. Integers carry no provenance. `expose` records a block as exposed;
`fromExposed` gives an integer the provenance of the live, exposed, nonempty block containing it,
else of the one it is one past, else none (Q3.9). Live blocks do not overlap, so at most one block
qualifies at each priority (`Proofs/Memory.lean`), and no tie-break is added. Pointer equality
compares addresses only and grants nothing (Q3.10).

**Access validity** (Q3.7). A nonzero access needs provenance, a live block, bounds, alignment;
a read also needs initialized bytes (checked when the bytes are decoded), and a write needs write
permission, which is authority (C5) and is a premise here, not a check. All accesses require a
target-valid pointer. A zero-byte access additionally needs only alignment and a non-null address. A violated premise is a failed proof obligation of the
`Unsafe` code that made the access: here, the operation has no result (unsafe stuckness), never a
fabricated value.

**Allocation** (Q1.8–Q1.11, Q3.10). `alloc` takes the address the allocator chose (an environment
choice, C8) and either creates a fresh block there, uninitialized, or fails with an ordinary
failure that leaves memory unchanged; an address that would overlap a live block, misalign, or not
fit in a target space is a failure, not a trap. The allocator's own availability laws are its contract (slices
19–23), not this module's.

**Scalar representation** (OD126, TYPE-19). An integer of type `t` occupies `bits / 8` bytes in the
profile's byte order, holding its two's-complement pattern (`toBits`); a `bool` is one byte, `0` or
`1`. Decoding is partial: an uninitialized byte, or a `bool` byte other than `0` and `1`, has no
value. An integer decoded from bytes that are pointer fragments is the integer the octets spell,
without provenance (integers carry none, Q3.7). Bytes other than exactly one whole stored
pointer have no pointer value: decoding fails (owner decision 2026-11-08, item 20).
-/

namespace ConcreteSpec.Core.Mem
open ConcreteSpec.Core

/-- A memory block's identity (Q3.7): fresh at allocation, never reused. -/
structure MemBlockId where
  id : Nat
  deriving DecidableEq, Repr

/-- A pointer (Q3.7): an address in a space, and its provenance. -/
structure Ptr where
  space : AddrSpace
  addr : Nat
  prov : Option MemBlockId
  deriving DecidableEq, Repr

/-- A pointer's numeric address and space belong to this target. Provenance is checked
separately by nonempty accesses. -/
def Ptr.Valid (tgt : Target) (p : Ptr) : Prop :=
  p.addr < 2 ^ tgt.addrBits ∧ p.space ∈ tgt.spaces

instance (tgt : Target) (p : Ptr) : Decidable (p.Valid tgt) := by
  unfold Ptr.Valid; infer_instance

/-- A byte of memory. An initialized byte is an octet, and may also be byte `index` of a stored
pointer `ptr`. -/
inductive MByte where
  | uninit
  | init (v : Fin 256) (frag : Option (Ptr × Nat))
  deriving DecidableEq, Repr

/-- A block. `bytes` has `size` entries, the byte at address `base + i` being entry `i`. -/
structure Block where
  space : AddrSpace
  base : Nat
  size : Nat
  align : Nat
  live : Bool
  exposed : Bool
  bytes : List MByte
  deriving DecidableEq, Repr

/-- The memory: its blocks by identity, and the next fresh identity. -/
structure Memory where
  blocks : AList MemBlockId Block
  next : Nat
  deriving Repr

/-- The empty memory. -/
def Memory.empty : Memory := ⟨[], 0⟩

/-- The block of an identity. -/
def Memory.block (m : Memory) (b : MemBlockId) : Option Block := AList.lookup m.blocks b

/-- Change the block of an identity. -/
def Memory.modify (m : Memory) (b : MemBlockId) (f : Block → Block) : Memory :=
  { m with blocks := AList.update m.blocks b f }

/-- Two blocks' occupied bytes overlap: both nonempty, in one space, with intersecting ranges. -/
def Block.Overlaps (x y : Block) : Prop :=
  x.space = y.space ∧ 0 < x.size ∧ 0 < y.size ∧ x.base < y.base + y.size ∧ y.base < x.base + x.size

instance (x y : Block) : Decidable (x.Overlaps y) := by unfold Block.Overlaps; infer_instance

/-- What every memory satisfies: identities below `next` (so `next` is fresh), each block's
bytes as many as its size, its space among the target spaces, its one-past address an address
of the target, its base aligned, and no
two live blocks overlapping. -/
structure Memory.WellFormed (tgt : Target) (m : Memory) : Prop where
  /-- Every stored entry agrees with lookup; shadowed contradictory blocks are forbidden. -/
  canonical : ∀ e ∈ m.blocks, m.block e.1 = some e.2
  /-- A block identity has exactly one entry, so first-entry updates preserve canonicality. -/
  unique : (List.map Prod.fst m.blocks).Nodup
  fresh : ∀ b x, m.block b = some x → b.id < m.next
  sized : ∀ b x, m.block b = some x → x.bytes.length = x.size
  fits : ∀ b x, m.block b = some x → x.base + x.size ≤ 2 ^ tgt.addrBits - 1
  spaces : ∀ b x, m.block b = some x → x.space ∈ tgt.spaces
  aligned : ∀ b x, m.block b = some x → IsPow2 x.align ∧ x.base % x.align = 0
  disjoint : ∀ b x c y, m.block b = some x → m.block c = some y → b ≠ c → x.live = true →
    y.live = true → ¬ x.Overlaps y

/-! ## Allocation and release -/

/-- Why an allocation failed: an ordinary result (Q1.9), never a trap. The allocator's typed
failure returns the rejected request; this is the memory's side of it. -/
inductive AllocError where
  /-- The chosen address does not hold the block: misaligned, overlapping a live block, or not
  fitting in the address space. -/
  | unavailable
  deriving DecidableEq, Repr

/-- The block `alloc` would create at `base`, if it fits there. -/
def candidate (space : AddrSpace) (base size align : Nat) : Block :=
  ⟨space, base, size, align, true, false, List.replicate size .uninit⟩

/-- The chosen address can hold a new block: aligned, its one-past address an address, not null
when nonempty, and overlapping no live block. -/
def Memory.Fits (tgt : Target) (m : Memory) (x : Block) : Prop :=
  IsPow2 x.align ∧ x.base % x.align = 0 ∧ x.base + x.size ≤ 2 ^ tgt.addrBits - 1 ∧
    x.size ≤ tgt.maxObject ∧ (0 < x.size → x.base ≠ tgt.null) ∧
    (∀ e ∈ m.blocks, e.2.live = true → ¬ e.2.Overlaps x) ∧ x.space ∈ tgt.spaces

instance (tgt : Target) (m : Memory) (x : Block) : Decidable (m.Fits tgt x) := by
  unfold Memory.Fits; infer_instance

/-- Allocate `size` bytes aligned to `align` in `space`, at the address `base` the allocator chose:
a fresh, live, unexposed block of uninitialized bytes, or a failure that leaves memory as it was. -/
def Memory.alloc (tgt : Target) (m : Memory) (space : AddrSpace) (base size align : Nat) :
    Except AllocError (MemBlockId × Memory) :=
  let x := candidate space base size align
  if m.Fits tgt x then .ok (⟨m.next⟩, ⟨(⟨m.next⟩, x) :: m.blocks, m.next + 1⟩)
  else .error .unavailable

/-- Release a live block: it is no longer live, nor exposed (Q3.9), and its identity is kept, so it
is never reused. `none`: the block is not live (a double release); the caller's contract excludes
it. -/
def Memory.free (m : Memory) (b : MemBlockId) : Option Memory :=
  match m.block b with
  | some x => if x.live then some (m.modify b fun x => { x with live := false, exposed := false }) else none
  | none => none

/-! ## Access validity, reads and writes -/

/-- A target-valid pointer `p` may access `n` bytes aligned to `a`: for `n = 0`, an aligned non-null address
(Q3.7's exception); otherwise a live block of `p`'s provenance, in `p`'s space, holding all `n`
bytes, with `p` aligned. The block and the offset of the first byte, or `none`. -/
def Memory.access (tgt : Target) (m : Memory) (p : Ptr) (n a : Nat) : Option (Option (MemBlockId × Nat)) :=
  if n = 0 then
    if p.addr % a = 0 ∧ p.addr ≠ tgt.null ∧ p.Valid tgt then some none else none
  else
    match p.prov with
    | none => none
    | some b => match m.block b with
      | none => none
      | some x =>
        if x.live = true ∧ x.space = p.space ∧ x.base ≤ p.addr ∧ p.addr + n ≤ x.base + x.size ∧
            p.addr % a = 0
        then some (some (b, p.addr - x.base)) else none

/-- Read `n` bytes through `p` (aligned to `a`): the bytes, which may be uninitialized (decoding
decides whether they hold a value), or `none` when the access is not valid. -/
def Memory.load (tgt : Target) (m : Memory) (p : Ptr) (n a : Nat) : Option (List MByte) :=
  match m.access tgt p n a with
  | some none => some []
  | some (some (b, off)) => (m.block b).map fun x => (x.bytes.drop off).take n
  | none => none

/-- Write bytes `bs` through `p` (aligned to `a`): the memory with those bytes replaced, or `none`
when the access is not valid. Write permission is a premise (authority, C5), not checked here. -/
def Memory.store (tgt : Target) (m : Memory) (p : Ptr) (bs : List MByte) (a : Nat) :
    Option Memory :=
  match m.access tgt p bs.length a with
  | some none => some m
  | some (some (b, off)) => some (m.modify b fun x =>
      { x with bytes := x.bytes.take off ++ bs ++ x.bytes.drop (off + bs.length) })
  | none => none

/-! ## Exposure and integer round trips (Q3.9) -/

/-- Expose the block of `p`: it becomes a candidate for `fromExposed`. `none` when `p` has no
live block. (Refusing a borrow-derived pointer, OD64, is the authority check of C5.) -/
def Memory.expose (m : Memory) (p : Ptr) : Option Memory :=
  match p.prov with
  | none => none
  | some b => match m.block b with
    | some x => if x.live then some (m.modify b fun x => { x with exposed := true }) else none
    | none => none

/-- A block `from_exposed` may give an address to: live, exposed, nonempty, in the space. -/
def Block.Candidate (space : AddrSpace) (x : Block) : Bool :=
  x.live && x.exposed && decide (0 < x.size) && decide (x.space = space)

/-- `from_exposed` (Q3.9): the provenance for address `addr` in `space`. First the candidate
block containing `addr`; else the candidate block `addr` is one past; else none. -/
def Memory.fromExposed (m : Memory) (space : AddrSpace) (addr : Nat) : Ptr :=
  let cands := m.blocks.filter fun e => e.2.Candidate space
  match cands.find? (fun e => decide (e.2.base ≤ addr ∧ addr < e.2.base + e.2.size)) with
  | some e => ⟨space, addr, some e.1⟩
  | none => match cands.find? (fun e => decide (addr = e.2.base + e.2.size)) with
    | some e => ⟨space, addr, some e.1⟩
    | none => ⟨space, addr, none⟩

/-- Pointer equality compares addresses (and spaces) only (Q3.10). -/
def Ptr.eqAddr (p q : Ptr) : Bool := decide (p.space = q.space ∧ p.addr = q.addr)

/-! ## Scalar representation -/

/-- The `k` low octets of `n`, least significant first. -/
def leBytes : Nat → Nat → List (Fin 256)
  | 0, _ => []
  | k + 1, n => ⟨n % 256, Nat.mod_lt _ (by decide)⟩ :: leBytes k (n / 256)

/-- The number little-endian octets spell. -/
def leValue : List (Fin 256) → Nat
  | [] => 0
  | b :: bs => b.val + 256 * leValue bs

/-- Octets in the profile's byte order. -/
def ordered (o : ByteOrder) (bs : List (Fin 256)) : List (Fin 256) :=
  match o with
  | .little => bs
  | .big => bs.reverse

/-- The number of bytes an integer type occupies. -/
def intBytes (t : IntTy) : Nat := t.bits / 8

/-- The bytes of integer `n` of type `t`: its two's-complement pattern, in the profile's order, as
plain initialized bytes. -/
def encodeInt (tgt : Target) (t : IntTy) (n : Int) : List MByte :=
  (ordered tgt.byteOrder (leBytes (intBytes t) (toBits t n))).map (.init · none)

/-- The octets of initialized bytes; `none` if one is uninitialized. Fragments give their octet:
an integer has no provenance. -/
def octets : List MByte → Option (List (Fin 256))
  | [] => some []
  | .uninit :: _ => none
  | .init v _ :: bs => (octets bs).map (v :: ·)

/-- The integer of type `t` the bytes hold: exactly `intBytes t` initialized bytes, read in the
profile's order. -/
def decodeInt (tgt : Target) (t : IntTy) (bs : List MByte) : Option Int :=
  if bs.length = intBytes t then
    (octets bs).map fun os => ofBits t (leValue (ordered tgt.byteOrder os))
  else none

/-- The byte of a `bool`. -/
def encodeBool (b : Bool) : List MByte := [.init ⟨boolToBits b, by cases b <;> decide⟩ none]

/-- The `bool` a byte holds: an initialized `0` or `1`. -/
def decodeBool : List MByte → Option Bool
  | [.init v _] => boolOfBits v.val
  | _ => none

/-- The bytes of a stored pointer: its address in the profile's order and pointer width, each byte
a fragment of the pointer. Encoding is total; decoding checks target validity. -/
def encodePtr (tgt : Target) (p : Ptr) : List MByte :=
  ((ordered tgt.byteOrder (leBytes tgt.ptrSize p.addr)).zipIdx).map fun (v, i) => .init v (some (p, i))

/-- The pointer the bytes hold, when they are exactly one stored pointer's fragments, in order. Any
other contents have no pointer meaning (owner decision 2026-11-08, item 20).
The exact encoding comparison checks length, all fragment identities and indices, and octets;
the numeric decode independently checks the address spelled by those octets. -/
def decodePtr (tgt : Target) (bs : List MByte) : Option Ptr :=
  match bs with
  | .init _ (some (p, 0)) :: _ =>
    if p.Valid tgt then if bs = encodePtr tgt p then
      match octets bs with
      | some os => if leValue (ordered tgt.byteOrder os) = p.addr then some p else none
      | none => none
    else none else none
  | _ => none

/-! ### Examples -/

/-- The target of the examples. -/
abbrev tgt : Target := linuxX8664

/-- A well-formed small profile used to check the address boundary. -/
private def tinyTarget : Target :=
  { tgt with addrBits := 8, ptrSize := 1, ptrAlign := 1, maxObject := 255 }

example : tinyTarget.WellFormed := by decide

/-- Refused: even a zero-byte access requires an address and space of the target. -/
example : Memory.empty.access tinyTarget ⟨⟨0⟩, 256, none⟩ 0 1 = none ∧
    Memory.empty.access tgt ⟨⟨99⟩, 8, none⟩ 0 1 = none ∧
    decodePtr tinyTarget
      [.init 0 (some (⟨⟨0⟩, 256, none⟩, 0))] = none ∧
    (Memory.empty.alloc tgt ⟨99⟩ 4096 1 1).toOption = none := by decide

/-- A `u16` `0x1234` is stored little-endian on the x86-64 profile, and reads back. -/
example : encodeInt tgt .u16 0x1234 = [.init 0x34 none, .init 0x12 none] := by decide

example : decodeInt tgt .i16 (encodeInt tgt .i16 (-2)) = some (-2) := by decide

/-- On a big-endian profile the same value is stored the other way round. -/
example : encodeInt { tgt with byteOrder := .big } .u16 0x1234 = [.init 0x12 none, .init 0x34 none] :=
  by decide

/-- An uninitialized byte has no value; byte `2` is no `bool`. -/
example : decodeInt tgt .u8 [.uninit] = none ∧ decodeBool [.init 2 none] = none ∧
    decodeBool (encodeBool true) = some true := by decide

/-- A fresh block is uninitialized: reading it gives bytes, but no integer. -/
example : (do
    let (b, m) ← (Memory.empty.alloc tgt ⟨0⟩ 4096 4 4).toOption
    let bs ← m.load tgt ⟨⟨0⟩, 4096, some b⟩ 4 4
    pure (decodeInt tgt .u32 bs)) = some none := by decide

/-- A write then a read through the same pointer gives the value back; a read past the block, or
through a pointer without provenance, is not a valid access. -/
example : (do
    let (b, m) ← (Memory.empty.alloc tgt ⟨0⟩ 4096 4 4).toOption
    let p : Ptr := ⟨⟨0⟩, 4096, some b⟩
    let m ← m.store tgt p (encodeInt tgt .u32 7) 4
    let bs ← m.load tgt p 4 4
    pure (decodeInt tgt .u32 bs, m.load tgt ⟨⟨0⟩, 4098, some b⟩ 4 2,
      m.load tgt ⟨⟨0⟩, 4096, none⟩ 4 4)) = some (some 7, none, none) := by decide

/-- After release, the old pointer reaches nothing, and the next block has a new identity even at
the same address. A zero-byte access needs a target-valid, aligned non-null address. -/
example : (do
    let (b, m) ← (Memory.empty.alloc tgt ⟨0⟩ 4096 4 4).toOption
    let m ← m.free b
    let (c, m) ← (m.alloc tgt ⟨0⟩ 4096 4 4).toOption
    pure (decide (b = c), m.load tgt ⟨⟨0⟩, 4096, some b⟩ 4 4, m.load tgt ⟨⟨0⟩, 8, none⟩ 0 4))
    = some (false, none, some []) := by decide

/-- Allocation that would overlap a live block fails, as an ordinary result. -/
example : (do
    let (_, m) ← (Memory.empty.alloc tgt ⟨0⟩ 4096 16 8).toOption
    pure (m.alloc tgt ⟨0⟩ 4100 4 4 |>.toOption.isNone)) = some true := by decide

/-- `from_exposed`: an address inside an exposed block gets its provenance; one past it, too, when
no exposed block contains it; an address of an unexposed block gets none. -/
example : (do
    let (b, m) ← (Memory.empty.alloc tgt ⟨0⟩ 4096 16 8).toOption
    let (_, m) ← (m.alloc tgt ⟨0⟩ 8192 16 8).toOption
    let m ← m.expose ⟨⟨0⟩, 4096, some b⟩
    pure ((m.fromExposed ⟨0⟩ 4100).prov, (m.fromExposed ⟨0⟩ 4112).prov,
      (m.fromExposed ⟨0⟩ 8192).prov)) = some (some ⟨0⟩, some ⟨0⟩, none) := by decide

/-- A stored pointer reads back with its provenance; the same bytes read as an integer give its
address, with no provenance. -/
example : (do
    let (b, m) ← (Memory.empty.alloc tgt ⟨0⟩ 4096 16 8).toOption
    let p : Ptr := ⟨⟨0⟩, 4100, some b⟩
    let m ← m.store tgt ⟨⟨0⟩, 4104, some b⟩ (encodePtr tgt p) 8
    let bs ← m.load tgt ⟨⟨0⟩, 4104, some b⟩ 8 8
    pure (decodePtr tgt bs == some p, decodeInt tgt .u64 bs)) = some (true, some 4100) := by
  decide

end ConcreteSpec.Core.Mem
