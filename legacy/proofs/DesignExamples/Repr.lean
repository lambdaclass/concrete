/-!
# Design example 4: typed values in byte-and-provenance memory

VERIFIED_PIPELINE.md gives Core "structured values, places and allocation identities for safe
operations; a specified byte-and-provenance memory component for raw operations and FFI,
connected to typed values by a representation relation", and gives Memory CFG "CompCert
block–offset memory (bytes, pointer fragments, undef, permissions)" with layout from "the
single `Layout` authority". S7 asks for "a representative typed-value/raw-memory abstraction
lemma" before Core freezes. This file is that lemma in miniature:

* memory bytes are concrete bytes, **pointer fragments** carrying provenance, or **undef**;
* `encode`/`decode` relate typed values to byte images for `u8`, `bool`, `i32`, `i64`, `u64`
  and pointers, little-endian (the first profile is AArch64/macOS), two's complement;
* a struct of scalars is laid out with the field offsets, alignment and tail padding of
  `Concrete/Check/Layout.lean` (`tySize`/`tyAlign`/`alignUp`); padding bytes are undef;
* memory is a list of blocks (allocation identities) of bytes; `load`/`store` check alignment
  and bounds.

Proved for every well-typed value, not just examples: `decode_encode` (scalars),
`decodeStruct_encodeStruct` (every struct of scalar fields) and `load_store` (block memory).
The examples then pin the decisions those theorems leave open: typed decoding ignores padding,
pointer bytes are not integers, integer bytes are not pointers, a torn pointer is no pointer,
invalid `bool` bytes and uninitialized bytes decode to nothing.

This is a design model, not Core: it imports nothing from the compiler.
-/

namespace DesignExamples.Repr

inductive Scalar where
  | u8
  | bool
  | i32
  | i64
  | u64
  | ptr
  deriving Repr, DecidableEq

/-- Sizes and alignments as `Layout.tySize`/`Layout.tyAlign` give them today. -/
def Scalar.size : Scalar → Nat
  | .u8 | .bool => 1
  | .i32 => 4
  | .i64 | .u64 | .ptr => 8

def Scalar.align (t : Scalar) : Nat := t.size

inductive Val where
  | int (n : Int)
  | bool (b : Bool)
  /-- a pointer is a block (allocation identity) and an offset: provenance, not an address. -/
  | ptr (blk off : Nat)
  deriving Repr, DecidableEq

inductive MByte where
  /-- a concrete byte; `encode` only produces values below 256. -/
  | byte (b : Nat)
  /-- the `idx`-th of the 8 bytes of a pointer to `blk + off`. -/
  | frag (blk off idx : Nat)
  /-- uninitialized or padding. -/
  | undef
  deriving Repr, DecidableEq

/-- Well-typedness: the value is in range for the type. -/
def wt : Scalar → Val → Bool
  | .u8, .int n => decide (0 ≤ n ∧ n < 256)
  | .i32, .int n => decide (-2147483648 ≤ n ∧ n < 2147483648)
  | .i64, .int n => decide (-9223372036854775808 ≤ n ∧ n < 9223372036854775808)
  | .u64, .int n => decide (0 ≤ n ∧ n < 18446744073709551616)
  | .bool, .bool _ => true
  | .ptr, .ptr _ _ => true
  | _, _ => false

/-! ## Scalars -/

/-- `k` little-endian bytes of `n`. -/
def le : Nat → Nat → List MByte
  | _, 0 => []
  | n, k + 1 => .byte (n % 256) :: le (n / 256) k

def unle : List MByte → Option Nat
  | [] => some 0
  | .byte b :: bs => if b < 256 then (unle bs).map (fun u => b + 256 * u) else none
  | _ :: _ => none

def ptrBytes (blk off : Nat) : List MByte := (List.range 8).map (.frag blk off)

/-- Two's complement bits of `n` in a `w`-byte field. -/
def twos (w : Nat) (n : Int) : Nat := if n < 0 then (n + (256 ^ w : Nat)).toNat else n.toNat

def encode : Scalar → Val → List MByte
  | .u8, .int n => le n.toNat 1
  | .u64, .int n => le n.toNat 8
  | .i32, .int n => le (twos 4 n) 4
  | .i64, .int n => le (twos 8 n) 8
  | .bool, .bool b => [.byte (if b then 1 else 0)]
  | .ptr, .ptr blk off => ptrBytes blk off
  | _, _ => []

/-- Read `w` bytes as an unsigned or two's-complement integer. Any fragment or undef byte
makes the read fail: a pointer's bytes are not an integer (design question R3). -/
def decodeInt (signed : Bool) (w : Nat) (bs : List MByte) : Option Val :=
  if bs.length = w then
    match unle bs with
    | some u =>
      some (if signed && decide (u ≥ 256 ^ w / 2) then .int ((u : Int) - (256 ^ w : Nat))
            else .int u)
    | none => none
  else none

def decode : Scalar → List MByte → Option Val
  | .u8, bs => decodeInt false 1 bs
  | .u64, bs => decodeInt false 8 bs
  | .i32, bs => decodeInt true 4 bs
  | .i64, bs => decodeInt true 8 bs
  | .bool, [.byte 0] => some (.bool false)
  | .bool, [.byte 1] => some (.bool true)
  | .bool, _ => none   -- any other byte is not a bool (design question R4)
  | .ptr, bs =>
    match bs with
    | .frag blk off 0 :: _ => if bs = ptrBytes blk off then some (.ptr blk off) else none
    | _ => none

theorem le_length (n k : Nat) : (le n k).length = k := by
  induction k generalizing n with
  | zero => rfl
  | succ k ih => simp [le, ih]

theorem unle_le (n k : Nat) : unle (le n k) = some (n % 256 ^ k) := by
  induction k generalizing n with
  | zero => simp [le, unle, Nat.mod_one]
  | succ k ih =>
    simp only [le, unle, ih, Nat.mod_lt _ (by decide : 256 > 0), ite_true, Option.map_some]
    rw [Nat.pow_succ, Nat.mul_comm (256 ^ k) 256, Nat.mod_mul]


theorem encode_length (t : Scalar) (v : Val) (h : wt t v = true) :
    (encode t v).length = t.size := by
  cases t <;> cases v <;> simp_all [wt, encode, le_length, ptrBytes, Scalar.size]

theorem decodeInt_le_of_lt (signed : Bool) (w x : Nat) (hx : x < 256 ^ w) :
    decodeInt signed w (le x w) =
      some (if signed && decide (x ≥ 256 ^ w / 2) then .int ((x : Int) - (256 ^ w : Nat))
            else .int x) := by
  simp only [decodeInt, le_length, unle_le, Nat.mod_eq_of_lt hx, ite_true]

/-- **Scalar round trip**: decoding the encoding of a well-typed value returns it. -/
theorem decode_encode (t : Scalar) (v : Val) (h : wt t v = true) :
    decode t (encode t v) = some v := by
  cases t <;> cases v <;> simp only [wt, Bool.false_eq_true] at h
  case bool.bool b => cases b <;> rfl
  case ptr.ptr blk off => simp [encode, decode, ptrBytes, List.range, List.range.loop]
  case u8.int n =>
    simp only [decide_eq_true_eq] at h
    simp only [encode, decode]
    rw [decodeInt_le_of_lt _ _ _ (by simp; omega)]
    simp; omega
  case u64.int n =>
    simp only [decide_eq_true_eq] at h
    simp only [encode, decode]
    rw [decodeInt_le_of_lt _ _ _ (by simp; omega)]
    simp; omega
  case i32.int n =>
    simp only [decide_eq_true_eq] at h
    simp only [encode, decode]
    rw [decodeInt_le_of_lt _ _ _ (by simp [twos]; split <;> omega)]
    simp only [twos]
    by_cases hn : n < 0 <;> simp [hn] <;> split <;> (congr 1; omega)
  case i64.int n =>
    simp only [decide_eq_true_eq] at h
    simp only [encode, decode]
    rw [decodeInt_le_of_lt _ _ _ (by simp [twos]; split <;> omega)]
    simp only [twos]
    by_cases hn : n < 0 <;> simp [hn] <;> split <;> (congr 1; omega)

/-! ## Structs of scalars, laid out as `Layout.lean` does -/

/-- Round up to a multiple of `a`. -/
def alignUp (n a : Nat) : Nat := n + (a - n % a) % a

/-- `Layout.alignUp` as written today, for the cross-check below. -/
def layoutAlignUp (n a : Nat) : Nat := if a ≤ 1 then n else ((n + a - 1) / a) * a

/-- The two agree on every offset below 256 and every scalar alignment. -/
theorem alignUp_agrees_with_Layout :
    ∀ n, n < 256 → ∀ a ∈ [1, 2, 4, 8], alignUp n a = layoutAlignUp n a := by decide +kernel

theorem le_alignUp (n a : Nat) : n ≤ alignUp n a := by simp [alignUp]

def pad (k : Nat) : List MByte := List.replicate k .undef

def fieldsEnd : List Scalar → Nat → Nat
  | [], off => off
  | f :: fs, off => fieldsEnd fs (alignUp off f.align + f.size)

def offsets : List Scalar → Nat → List Nat
  | [], _ => []
  | f :: fs, off => alignUp off f.align :: offsets fs (alignUp off f.align + f.size)

def structAlign (fs : List Scalar) : Nat := fs.foldr (fun f a => max f.align a) 1

def structSize (fs : List Scalar) : Nat := alignUp (fieldsEnd fs 0) (structAlign fs)

/-- Each field at its aligned offset; the bytes skipped to reach it are undef padding. -/
def encFields : List Scalar → List Val → Nat → List MByte
  | f :: fs, v :: vs, off =>
    let o := alignUp off f.align
    pad (o - off) ++ encode f v ++ encFields fs vs (o + f.size)
  | _, _, _ => []

/-- The struct's byte image: fields, then undef tail padding up to the struct size. -/
def encodeStruct (fs : List Scalar) (vs : List Val) : List MByte :=
  let body := encFields fs vs 0
  body ++ pad (structSize fs - body.length)

/-- Decoding reads each field at its offset and ignores padding. -/
def decFields : List Scalar → List MByte → Nat → Option (List Val)
  | [], _, _ => some []
  | f :: fs, bs, off =>
    let o := alignUp off f.align
    let bs1 := bs.drop (o - off)
    match decode f (bs1.take f.size), decFields fs (bs1.drop f.size) (o + f.size) with
    | some v, some vs => some (v :: vs)
    | _, _ => none

def decodeStruct (fs : List Scalar) (bs : List MByte) : Option (List Val) :=
  if bs.length = structSize fs then decFields fs bs 0 else none

/-- Field-wise well-typedness. -/
def wtFields : List Scalar → List Val → Bool
  | [], [] => true
  | f :: fs, v :: vs => wt f v && wtFields fs vs
  | _, _ => false

theorem le_fieldsEnd (fs : List Scalar) (off : Nat) : off ≤ fieldsEnd fs off := by
  induction fs generalizing off with
  | nil => simp [fieldsEnd]
  | cons f fs ih =>
    simp only [fieldsEnd]
    have := le_alignUp off f.align
    have := ih (alignUp off f.align + f.size)
    omega

theorem encFields_length (fs : List Scalar) (vs : List Val) (off : Nat)
    (h : wtFields fs vs = true) : (encFields fs vs off).length + off = fieldsEnd fs off := by
  induction fs generalizing vs off with
  | nil => cases vs <;> simp_all [encFields, fieldsEnd, wtFields]
  | cons f fs ih =>
    cases vs with
    | nil => simp [wtFields] at h
    | cons v vs =>
      simp only [wtFields, Bool.and_eq_true] at h
      simp only [encFields, fieldsEnd, List.length_append, pad, List.length_replicate,
        encode_length f v h.1]
      have := le_alignUp off f.align
      have := ih vs (alignUp off f.align + f.size) h.2
      omega

theorem decFields_encFields (fs : List Scalar) (vs : List Val) (off : Nat) (tail : List MByte)
    (h : wtFields fs vs = true) : decFields fs (encFields fs vs off ++ tail) off = some vs := by
  induction fs generalizing vs off with
  | nil => cases vs <;> simp_all [decFields, wtFields]
  | cons f fs ih =>
    cases vs with
    | nil => simp [wtFields] at h
    | cons v vs =>
      simp only [wtFields, Bool.and_eq_true] at h
      simp only [encFields, decFields, List.append_assoc]
      rw [List.drop_left' (by simp [pad]), List.take_left' (encode_length f v h.1),
        List.drop_left' (encode_length f v h.1), decode_encode f v h.1,
        ih vs _ h.2]

/-- **Struct round trip**, for every struct of scalar fields and every well-typed value. -/
theorem decodeStruct_encodeStruct (fs : List Scalar) (vs : List Val)
    (h : wtFields fs vs = true) : decodeStruct fs (encodeStruct fs vs) = some vs := by
  have hl := encFields_length fs vs 0 h
  have hs : fieldsEnd fs 0 ≤ structSize fs := le_alignUp _ _
  simp only [decodeStruct, encodeStruct, List.length_append, pad, List.length_replicate]
  split
  · exact decFields_encFields fs vs 0 _ h
  · omega

/-! ## Block–offset memory -/

/-- Blocks are allocation identities; each holds its bytes. Freed blocks are absent. -/
abbrev Mem := List (Nat × List MByte)

def getBlk (b : Nat) : Mem → Option (List MByte)
  | [] => none
  | (b', c) :: rest => if b' = b then some c else getBlk b rest

def setBlk (b : Nat) (c : List MByte) (m : Mem) : Mem := (b, c) :: m

/-- A fresh allocation is uninitialized. -/
def alloc (m : Mem) (b n : Nat) : Mem := (b, pad n) :: m

def load (t : Scalar) (m : Mem) (b off : Nat) : Option Val :=
  if off % t.align ≠ 0 then none        -- misaligned: no value (design question R6)
  else match getBlk b m with
    | none => none                        -- freed or never allocated
    | some c => if off + t.size ≤ c.length then decode t ((c.drop off).take t.size) else none

def store (t : Scalar) (v : Val) (m : Mem) (b off : Nat) : Option Mem :=
  if off % t.align ≠ 0 || !wt t v then none
  else match getBlk b m with
    | none => none
    | some c =>
      if off + t.size ≤ c.length then
        some (setBlk b (c.take off ++ encode t v ++ c.drop (off + t.size)) m)
      else none

/-- **Load after store** returns the stored value, for every well-typed scalar. -/
theorem load_store (t : Scalar) (v : Val) (m m' : Mem) (b off : Nat)
    (h : store t v m b off = some m') : load t m' b off = some v := by
  unfold store at h
  split at h
  · contradiction
  · rename_i hc
    simp only [Bool.or_eq_true, decide_eq_true_eq, Bool.not_eq_true', not_or] at hc
    split at h
    · contradiction
    · rename_i c _
      split at h
      · rename_i hb
        simp only [Option.some.injEq] at h
        subst h
        have hlen := encode_length t v (by simpa using hc.2)
        have hoff : (c.take off).length = off := by simp; omega
        simp only [load, hc.1, setBlk, getBlk, ite_false, ite_true, List.append_assoc]
        split
        · rw [List.drop_left' hoff, List.take_left' hlen, decode_encode t v (by simpa using hc.2)]
        · rename_i hn; simp [hlen] at hn; omega
      · contradiction

/-! ## Examples: the decisions the theorems leave open -/

/-- `struct S { a: u8, b: i64, c: bool, d: i32 }` -/
def S : List Scalar := [.u8, .i64, .bool, .i32]
def sVal : List Val := [.int 7, .int (-2), .bool true, .int (-1)]

/-- Offsets 0, 8, 16, 20; size 24; alignment 8 — what `Layout.lean` computes for `S`. -/
theorem S_layout : offsets S 0 = [0, 8, 16, 20] ∧ structSize S = 24 ∧ structAlign S = 8 := by
  decide

theorem S_bytes :
    encodeStruct S sVal =
      [.byte 7] ++ pad 7 ++
      [.byte 254, .byte 255, .byte 255, .byte 255, .byte 255, .byte 255, .byte 255, .byte 255] ++
      [.byte 1] ++ pad 3 ++ [.byte 255, .byte 255, .byte 255, .byte 255] := by decide

/-- Typed struct decoding ignores padding: this alternate image (concrete bytes, a pointer
fragment and undef in the padding) decodes to the same value, so the representation relation is
many-to-one (design question R2). Raw byte loads of the padding still see what is there. -/
theorem S_padding_unobservable :
    decodeStruct S
      ([.byte 7] ++ List.replicate 7 (.byte 0xAA) ++
       [.byte 254, .byte 255, .byte 255, .byte 255, .byte 255, .byte 255, .byte 255, .byte 255] ++
       [.byte 1, .frag 3 0 5, .byte 0, .undef] ++ [.byte 255, .byte 255, .byte 255, .byte 255])
      = some sVal := by decide

/-- Reading a padding byte as a `u8` yields nothing: padding is undef. -/
theorem S_padding_undef : decode .u8 (((encodeStruct S sVal).drop 1).take 1) = none := by decide

/-- A pointer's bytes are not an integer, and an integer's bytes are not a pointer:
pointer ↔ integer conversion must be an explicit operation (design question R3). -/
theorem ptr_bytes_not_int : decode .i64 (encode .ptr (.ptr 3 16)) = none := by decide
theorem int_bytes_not_ptr : decode .ptr (encode .u64 (.int 3)) = none := by decide

/-- A torn pointer (one fragment overwritten by a byte store) is no pointer. -/
theorem torn_pointer :
    (do
      let m1 ← store .ptr (.ptr 3 16) (alloc [] 1 8) 1 0
      let m2 ← store .u8 (.int 0) m1 1 2
      pure (load .ptr m2 1 0)) = some none := by decide

/-- `bool` has two valid bytes; any other byte is not a `bool` (design question R4). -/
theorem bool_invalid_byte : decode .bool [.byte 2] = none := by decide

/-- Fresh memory is uninitialized; reading it yields nothing. -/
theorem fresh_is_undef : load .i32 (alloc [] 1 8) 1 4 = none := by decide

/-- Misaligned and out-of-bounds accesses yield nothing; an aligned in-bounds store then load
round-trips (a concrete instance of `load_store`). -/
theorem access_rules :
    store .i32 (.int 1) (alloc [] 1 8) 1 2 = none ∧
    store .i32 (.int 1) (alloc [] 1 8) 1 8 = none ∧
    ((store .i32 (.int (-5)) (alloc [] 1 8) 1 4).bind (load .i32 · 1 4)) = some (.int (-5)) := by
  decide

end DesignExamples.Repr
