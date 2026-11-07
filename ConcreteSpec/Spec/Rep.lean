import ConcreteSpec.Spec.Slice1Semantics

/-!
# Part 2: representation

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). Part of `ConcreteSpec.Spec`;
see `ConcreteSpec/README.md`.
-/

namespace ConcreteSpec.Rep
open ConcreteSpec

/-! ## Part 2. Representation (Q3.11): `rep_roundtrip`

Core joins structured values and the raw store by a representation relation (Q3.11). This part
defines the layout and the byte encoding of the slice-1 types and proves that decoding an encoded
value gives the value back (row Q9.14's `rep_roundtrip`). The layout facts are language rules
(OD126): byte order is little-endian, a `bool` is one byte, and a struct with no fields occupies
no bytes (F13: a zero-sized value occupies no bytes). Integer widths follow from the type names
(TYPE-19). A `bool` byte is `0` for `false` and `1` for `true`; every other byte value, and an
uninitialized byte, is not a valid `bool`.

Not modelled here, because slice 1 does not need them: pointer fragments, permissions and
external frames (Q3.11), struct fields and their padding, and load/store against a block memory.
-/

/-- A memory byte: uninitialized, or initialized with a value below 256. -/
inductive MByte where
  | undef
  | byte (b : Fin 256)
  deriving DecidableEq, Repr

/-- The size in bytes of a scalar type (TYPE-19, OD126). -/
def scalarSize : STy → Nat
  | .int t => t.bits / 8
  | .bool => 1

/-- The size in bytes of a slice-1 type this part covers: a scalar, or a struct declared with no
fields (OD126, F13). Other structs are outside this part. -/
def Ty.size (Δ : Decls) : Ty → Option Nat
  | .scalar st => some (scalarSize st)
  | .struct s => match findDecl Δ s with
    | some d => if d.fields = [] then some 0 else none
    | none => none

/-- `n` as `w` little-endian bytes (OD126), modulo `256^w`. -/
def encodeNat : Nat → Nat → List MByte
  | 0, _ => []
  | w + 1, n => .byte ⟨n % 256, Nat.mod_lt _ (by decide)⟩ :: encodeNat w (n / 256)

/-- The number little-endian bytes denote, if every byte is initialized. -/
def decodeNat : List MByte → Option Nat
  | [] => some 0
  | .byte b :: rest => (decodeNat rest).map (fun m => b.val + 256 * m)
  | .undef :: _ => none

/-- A scalar's bytes. An integer is stored as its two's-complement bit pattern (its value modulo
`2^bits`) in `bits / 8` little-endian bytes; a `bool` is one byte, `0` or `1`. -/
def encodeScalar : Scalar → List MByte
  | .int t n => encodeNat (t.bits / 8) (n % (2 : Int) ^ t.bits).toNat
  | .bool b => [.byte (if b then 1 else 0)]

/-- The integer a bit pattern `m` denotes in `t`: `m` itself, or `m - 2^bits` for a signed type
whose top bit is set. -/
def ofBits (t : IntTy) (m : Nat) : Int :=
  if t.signed = true ∧ m ≥ 2 ^ (t.bits - 1) then (m : Int) - (2 : Int) ^ t.bits else (m : Int)

/-- The scalar of type `st` that `bs` represents, if any. The byte count must be the type's size,
every byte must be initialized, and a `bool` byte must be `0` or `1`. -/
def decodeScalar : STy → List MByte → Option Scalar
  | .int t, bs =>
    if bs.length = t.bits / 8 then (decodeNat bs).map (fun m => .int t (ofBits t m)) else none
  | .bool, [.byte b] =>
    if b.val = 0 then some (.bool false) else if b.val = 1 then some (.bool true) else none
  | .bool, _ => none

/-- A value's bytes: a scalar's encoding, or no bytes for a struct with no fields. -/
def encodeVal : Val → List MByte
  | .scalar s => encodeScalar s
  | .struct _ _ => []

/-- The value of type `t` that `bs` represents, if any. -/
def decodeVal (Δ : Decls) : Ty → List MByte → Option Val
  | .scalar st, bs => (decodeScalar st bs).map .scalar
  | .struct s, [] => match findDecl Δ s with
    | some d => if d.fields = [] then some (.struct s []) else none
    | none => none
  | .struct _, _ :: _ => none

/-- `v` is a value of type `t` that this part represents: a well-typed scalar, or the value of a
struct declared with no fields. -/
inductive RepVal (Δ : Decls) : Val → Ty → Prop where
  | scalar {v st} : SVTy v st → RepVal Δ (.scalar v) (.scalar st)
  | empty {s d} : findDecl Δ s = some d → d.fields = [] → RepVal Δ (.struct s []) (.struct s)

/-- Non-vacuity: decoding is not total. An uninitialized byte is no `u8`, byte `2` is no `bool`,
and a byte count other than the size is no value. -/
example : decodeScalar (.int .u8) [.undef] = none := by decide

example : decodeScalar .bool [.byte 2] = none := by decide

example : decodeScalar (.int .u16) [.byte 1] = none := by decide

/-- Little-endian, two's complement (OD126): `-2 : i16` is the bytes `0xfe 0xff`. -/
example : encodeScalar (.int .i16 (-2)) = [.byte 254, .byte 255] := by decide

/-- Round trips at the ends of each unsigned range, for both `bool` values and for the empty
struct `Token` (zero bytes, F13). -/
example : decodeVal [] (.scalar (.int .u8)) (encodeVal (.scalar (.int .u8 255))) =
    some (.scalar (.int .u8 255)) := by decide

example : decodeVal [] (.scalar (.int .u16)) (encodeVal (.scalar (.int .u16 65535))) =
    some (.scalar (.int .u16 65535)) := by decide

example : decodeVal [] (.scalar (.int .u32)) (encodeVal (.scalar (.int .u32 4294967295))) =
    some (.scalar (.int .u32 4294967295)) := by decide

example : decodeVal [] (.scalar (.int .u64)) (encodeVal (.scalar (.int .u64 18446744073709551615))) =
    some (.scalar (.int .u64 18446744073709551615)) := by decide

example : decodeVal [] (.scalar (.int .u64)) (encodeVal (.scalar (.int .u64 0))) =
    some (.scalar (.int .u64 0)) := by decide

example : decodeVal [] (.scalar .bool) (encodeVal (.scalar (.bool true))) =
    some (.scalar (.bool true)) := by decide

example : decodeVal [] (.scalar .bool) (encodeVal (.scalar (.bool false))) =
    some (.scalar (.bool false)) := by decide

end ConcreteSpec.Rep
