import ConcreteSpec.Spec.Ident

/-!
# Target profiles

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). Part of `ConcreteSpec.Spec`;
see `ConcreteSpec/README.md`. Foundation contract C3 (docs/proposals/FOUNDATION.md), step 3 of the
spec plan (docs/proposals/SPEC_UNIFICATION.md, section 6).

A *target profile* is the read-only description of the machine a program is compiled for. Every
rule that depends on the machine reads it as a parameter; no rule assumes a 64-bit machine. The
profile keeps apart facts that a 64-bit desktop happens to make equal: how wide an address is, how
many bytes a stored pointer takes and how it is aligned, how wide a stored length is, the byte
order, and which address spaces exist. None of them is inferred from another, or from the width of
the CPU's arithmetic (owner decision 2026-11-08, question 3: read-only profiles; this generalizes
rows Q3.7, Q3.9 and Q3.10 and OD126's profile without changing the existing one).

The *meaning* of a length or an index does not depend on the profile: it is a `u64` everywhere
(row Q3.5, kept by question 7). A profile's `lenBits` is only the width a length is *stored* in on
that target; a width narrower than 64 is allowed only where a proof shows that every length the
program can have fits (question 7, option (c)), and that proof is not part of the profile.

There is one profile today, `linuxX8664`, the existing 64-bit little-endian target of row Q7.1,
unchanged. Another target is another profile (OD126), and must satisfy `Target.WellFormed`.
-/

namespace ConcreteSpec.Core

/-- The order in which a multi-byte value's bytes are stored. -/
inductive ByteOrder where
  | little
  | big
  deriving DecidableEq, Repr

/-- An address space of a target: data memory, or for example a separate program memory. Raw
pointers and accesses name their space explicitly; there is no silent conversion between spaces
(FOUNDATION C3). -/
structure AddrSpace where
  id : Nat
  deriving DecidableEq, Repr

/-- `n` is a positive power of two. -/
def IsPow2 (n : Nat) : Prop := 0 < n ∧ n &&& (n - 1) = 0

instance (n : Nat) : Decidable (IsPow2 n) := by unfold IsPow2; infer_instance

/-- A target profile. Every field is read-only: rules read it and never change it.

- `addrBits`: the width of an address, in bits.
- `ptrSize`, `ptrAlign`: the bytes a stored pointer occupies and its alignment.
- `lenBits`: the width, in bits, a length or index is stored in (its meaning stays `u64`, Q3.5).
- `byteOrder`: the order of a stored value's bytes.
- `spaces`: the address spaces the target has; `dataSpace` is the one ordinary values live in.
- `null`: the distinguished null address.
- `maxObject`: the largest number of bytes one object may occupy. -/
structure Target where
  addrBits : Nat
  ptrSize : Nat
  ptrAlign : Nat
  lenBits : Nat
  byteOrder : ByteOrder
  spaces : List AddrSpace
  dataSpace : AddrSpace
  null : Nat
  maxObject : Nat
  deriving DecidableEq, Repr

/-- What every profile must satisfy.

- An address has at least one bit, and a stored pointer has room for all of them.
- A stored pointer's alignment is a power of two, and its size is a multiple of it (the layout law
  that a size is a multiple of its alignment, FOUNDATION C3).
- A stored pointer fits in one object.
- A stored length has between 1 and 64 bits, since its meaning is a `u64` (Q3.5).
- The data space is one of the target's spaces.
- The null address is an address.
- No object is as large as the address range: `maxObject` is below `2 ^ addrBits`. This is only
  the profile's size bound; that a placed object's one-past address is an address, row Q3.10's
  `base + size ≤ 2^64 − 1` generalized to `2 ^ addrBits − 1` (question 3), is an obligation on each
  block's base and size, stated with memory (C4), not here. -/
def Target.WellFormed (t : Target) : Prop :=
  0 < t.addrBits ∧ t.addrBits ≤ 8 * t.ptrSize ∧ IsPow2 t.ptrAlign ∧ t.ptrSize % t.ptrAlign = 0 ∧
  t.ptrSize ≤ t.maxObject ∧
  0 < t.lenBits ∧ t.lenBits ≤ 64 ∧ t.dataSpace ∈ t.spaces ∧
  t.null < 2 ^ t.addrBits ∧ t.maxObject < 2 ^ t.addrBits

instance (t : Target) : Decidable t.WellFormed := by unfold Target.WellFormed; infer_instance

/-- The existing target, linux-x86_64-hosted (row Q7.1): 64-bit addresses, 8-byte pointers aligned
to 8, 64-bit lengths, little-endian, one data space, null at address 0, and objects of at most
`2^64 − 1` bytes (row Q3.10). -/
def linuxX8664 : Target where
  addrBits := 64
  ptrSize := 8
  ptrAlign := 8
  lenBits := 64
  byteOrder := .little
  spaces := [⟨0⟩]
  dataSpace := ⟨0⟩
  null := 0
  maxObject := 2 ^ 64 - 1

/-! ### Examples -/

/-- The existing profile is well formed. -/
example : linuxX8664.WellFormed := by decide

/-- A profile whose pointers have no room for its addresses is not: 32-bit addresses in a 2-byte
pointer. -/
example : ¬ ({ linuxX8664 with
    addrBits := 32
    ptrSize := 2
    ptrAlign := 2
    maxObject := 2 ^ 16 } : Target).WellFormed := by decide

/-- Nor is one whose pointer alignment is not a power of two. -/
example : ¬ ({ linuxX8664 with ptrAlign := 6 } : Target).WellFormed := by decide

/-- Nor one whose pointer size is not a multiple of its alignment. -/
example : ¬ ({ linuxX8664 with ptrAlign := 16 } : Target).WellFormed := by decide

/-- Nor one whose objects are too small to hold a pointer. -/
example : ¬ ({ linuxX8664 with maxObject := 4 } : Target).WellFormed := by decide

/-- Nor one whose objects may be as large as the whole address range. -/
example : ¬ ({ linuxX8664 with maxObject := 2 ^ 64 } : Target).WellFormed := by decide

/-- A small target with 16-bit addresses and 16-bit stored lengths is well formed: nothing assumes
64 bits. -/
example : ({ linuxX8664 with
    addrBits := 16
    ptrSize := 2
    ptrAlign := 2
    lenBits := 16
    maxObject := 2 ^ 15 } : Target).WellFormed := by decide

end ConcreteSpec.Core
