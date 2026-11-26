import Concrete.Artifact.Sha256

/-!
# Provisional artifact byte encoding (A13)

A frame is ASCII decimal byte length (no leading zeroes, except zero), ASCII colon, then
exactly that many bytes. A record starts with the literal bytes `Concrete/0-dev:` and a
framed record kind, followed by framed fields in schema order through end of input.
The caller owns the schema and supplies canonical field bytes, including UTF-8 where the
schema calls for text. Raw byte fields are preserved verbatim.

The digest preimage is the same framing with the reserved domain `digest`, a fixed role label,
a canonical decimal slot number, and the exact payload bytes. Consumers assign slots
by schema within each role, starting at zero. They must check the expected role and slot
from the schema. A payload path or its displayed representation is never a surrogate for
the payload bytes.

D4/E3 remains open pending a recorded unanimous verdict on the exact plan-v4 step.
The format and digest API are provisional; no certificate claim depends on digest equality.
The first replay consumer must supply schema-specific limits and check canonical decoding.
A future local-variable codec must satisfy ISS-0060 for both encoding and decoding.
-/

namespace Concrete.Artifact

/-- Versioned record namespaces. Receipt and certificate field schemas belong to their producers. -/
inductive RecordKind where
  | certificate | receipt | facts
  deriving DecidableEq, Repr

def RecordKind.label : RecordKind → String
  | .certificate => "certificate"
  | .receipt => "receipt"
  | .facts => "facts"

def RecordKind.all : List RecordKind := [.certificate, .receipt, .facts]

theorem RecordKind.mem_all (kind : RecordKind) : kind ∈ all := by
  cases kind <;> decide

private def frameHeader (size : Nat) : ByteArray :=
  size.repr.toByteArray.push 0x3a

private def appendFrame (out bytes : ByteArray) : ByteArray :=
  out ++ frameHeader bytes.size ++ bytes

private def frame (bytes : ByteArray) : ByteArray :=
  frameHeader bytes.size ++ bytes

private def appendFrames (out : ByteArray) (fields : List ByteArray) : ByteArray :=
  fields.foldl appendFrame out

/-- Encode a sequence of fields, including a sequence nested inside another field. -/
def encodeFrames (fields : List ByteArray) : ByteArray :=
  appendFrames ByteArray.empty fields

private def encodeDomain (domain : String) (fields : List ByteArray) : ByteArray :=
  appendFrames "Concrete/0-dev:".toByteArray (domain.toByteArray :: fields)

/-- Encode ordered fields with byte lengths independent of locale and host endianness. -/
def encode (kind : RecordKind) (fields : List ByteArray) : ByteArray :=
  encodeDomain kind.label fields

/-- Domains for payloads referenced by pre-link evidence and the single-build receipt. -/
inductive PayloadRole where
  | source | compiler | spec | bir
  | llvmModule | lcbSource | lcbJob | assembly | backendInput | toolInput
  | runtime | siteTable | target | loweringPath | tool
  | executable | certificate | facts
  deriving DecidableEq, Repr

def PayloadRole.label : PayloadRole → String
  | .source => "source"
  | .compiler => "compiler"
  | .spec => "spec"
  | .bir => "bir"
  | .llvmModule => "llvm-module"
  | .lcbSource => "lcb-source"
  | .lcbJob => "lcb-job"
  | .assembly => "assembly"
  | .backendInput => "backend-input"
  | .toolInput => "tool-input"
  | .runtime => "runtime"
  | .siteTable => "site-table"
  | .target => "target"
  | .loweringPath => "lowering-path"
  | .tool => "tool"
  | .executable => "executable"
  | .certificate => "certificate"
  | .facts => "facts"

def PayloadRole.all : List PayloadRole :=
  [.source, .compiler, .spec, .bir, .llvmModule, .lcbSource, .lcbJob, .assembly,
   .backendInput, .toolInput, .runtime, .siteTable, .target, .loweringPath, .tool,
   .executable, .certificate, .facts]

theorem PayloadRole.mem_all (role : PayloadRole) : role ∈ all := by
  cases role <;> decide

/-- The byte sequence hashed for a payload in its schema-assigned role and slot. -/
def digestInput (role : PayloadRole) (slot : Nat) (payload : ByteArray) : ByteArray :=
  encodeDomain "digest" [role.label.toByteArray, slot.repr.toByteArray, payload]

private theorem decimal_injective {a b : Nat} (h : a.repr.toByteArray = b.repr.toByteArray) :
    a = b := by
  have hs := congrArg String.toList (String.toByteArray_inj.mp h)
  rw [Nat.toList_repr, Nat.toList_repr] at hs
  simpa only [Nat.ofDigitChars_ten_toDigits] using
    congrArg (fun digits => Nat.ofDigitChars 10 digits 0) hs

private theorem decimal_no_colon (n : Nat) (b : UInt8)
    (hb : b ∈ n.repr.toByteArray.data.toList) : (b != 0x3a) = true := by
  rw [← String.utf8Encode_toList, Nat.toList_repr] at hb
  simp only [List.utf8Encode, List.toList_data_toByteArray] at hb
  obtain ⟨c, hc, hb⟩ := List.mem_flatMap.mp hb
  have hd := Char.isDigit_iff_toNat.mp (Nat.isDigit_of_mem_toDigits (by decide) (by decide) hc)
  have hlt : c.toNat < 128 := by
    change 48 ≤ c.toNat ∧ c.toNat ≤ 57 at hd
    omega
  have henc : String.utf8EncodeChar c = [c.toUInt8] := by
    have ha : c.val.toNat ≤ 127 := show c.toNat ≤ 127 by omega
    simp only [String.utf8EncodeChar, ha, ↓reduceIte]
    rfl
  rw [henc, List.mem_singleton] at hb
  subst b
  apply bne_iff_ne.mpr
  intro he
  have hn := congrArg UInt8.toNat he
  change c.toNat % 256 = 58 at hn
  change 48 ≤ c.toNat ∧ c.toNat ≤ 57 at hd
  omega

private theorem frame_prefix (bytes rest : ByteArray) :
    (frame bytes ++ rest).data.toList.takeWhile (· != 0x3a) = bytes.size.repr.toByteArray.data.toList := by
  change ((bytes.size.repr.toByteArray.push 0x3a ++ bytes ++ rest).data.toList).takeWhile
    (· != 0x3a) = _
  simp only [ByteArray.toList_data_append, ByteArray.data_push, Array.toList_push,
    List.append_assoc, List.singleton_append]
  rw [List.takeWhile_append_of_pos (fun b hb => decimal_no_colon _ b hb)]
  simp

private theorem frame_cancel {a b tailA tailB : ByteArray}
    (h : frame a ++ tailA = frame b ++ tailB) : a = b ∧ tailA = tailB := by
  have hp := congrArg (fun bytes : ByteArray => bytes.data.toList.takeWhile (· != 0x3a)) h
  rw [frame_prefix, frame_prefix] at hp
  have hs : a.size = b.size := decimal_injective (by
    simpa [ByteArray.ext_iff] using hp)
  simp only [frame, hs, ByteArray.append_assoc] at h
  exact (ByteArray.append_eq_append_iff_of_size_eq_left hs).mp
    ((ByteArray.append_right_inj _).mp h)

private theorem appendFrames_prefix (out : ByteArray) (fields : List ByteArray) :
    appendFrames out fields = out ++ encodeFrames fields := by
  induction fields generalizing out with
  | nil => simp [appendFrames, encodeFrames]
  | cons bytes fields ih =>
    change appendFrames (appendFrame out bytes) fields =
      out ++ appendFrames (appendFrame ByteArray.empty bytes) fields
    rw [ih (appendFrame out bytes), ih (appendFrame ByteArray.empty bytes)]
    simp [appendFrame, ByteArray.append_assoc]

private theorem encodeFrames_cons (bytes : ByteArray) (fields : List ByteArray) :
    encodeFrames (bytes :: fields) = frame bytes ++ encodeFrames fields := by
  change appendFrames (appendFrame ByteArray.empty bytes) fields = _
  rw [appendFrames_prefix]
  simp [appendFrame, frame]

/-- A framed sequence uniquely determines the number, boundaries and bytes of its fields. -/
theorem encodeFrames_injective {xs ys : List ByteArray}
    (h : encodeFrames xs = encodeFrames ys) : xs = ys := by
  have nonempty (a : ByteArray) (tail : List ByteArray) :
      encodeFrames (a :: tail) ≠ encodeFrames [] := by
    intro he
    have hs := congrArg ByteArray.size he
    simp [encodeFrames_cons, frame, frameHeader, show encodeFrames [] = ByteArray.empty from rfl] at hs
  induction xs generalizing ys with
  | nil =>
    cases ys with
    | nil => rfl
    | cons b bs => exact (nonempty b bs h.symm).elim
  | cons a as ih =>
    cases ys with
    | nil => exact (nonempty a as h).elim
    | cons b bs =>
      simp only [encodeFrames_cons] at h
      obtain ⟨rfl, ht⟩ := frame_cancel h
      rw [ih ht]

private theorem eq_of_mapped_eq {α : Type} {xs : List α} {f : α → String}
    (hd : (xs.map f).Nodup) {a b : α} (ha : a ∈ xs) (hb : b ∈ xs) (he : f a = f b) :
    a = b := by
  obtain ⟨i, hi, hia⟩ := List.mem_iff_getElem.mp ha
  obtain ⟨j, hj, hjb⟩ := List.mem_iff_getElem.mp hb
  have hij : i = j := (hd.getElem_inj (hi := by simpa) (hj := by simpa)).mp (by
    simpa [hia, hjb] using he)
  subst j
  exact hia.symm.trans hjb

private theorem domain_fields_eq {a b : String} {xs ys : List ByteArray}
    (h : encodeDomain a xs = encodeDomain b ys) : a = b ∧ xs = ys := by
  unfold encodeDomain at h
  rw [appendFrames_prefix, appendFrames_prefix, ByteArray.append_right_inj] at h
  have he := encodeFrames_injective h
  simpa [String.toByteArray_inj] using he

/-- An encoded record uniquely determines its kind and framed fields. -/
theorem encode_injective {a b : RecordKind} {xs ys : List ByteArray}
    (h : encode a xs = encode b ys) : a = b ∧ xs = ys := by
  obtain ⟨hk, hf⟩ := domain_fields_eq h
  exact ⟨eq_of_mapped_eq (by decide) (RecordKind.mem_all a) (RecordKind.mem_all b) hk, hf⟩

/-- Digest preimages uniquely determine their role, slot and payload bytes. -/
theorem digestInput_injective {a b : PayloadRole} {i j : Nat} {x y : ByteArray}
    (h : digestInput a i x = digestInput b j y) : a = b ∧ i = j ∧ x = y := by
  have hf := (domain_fields_eq h).2
  simp only [List.cons.injEq, and_true] at hf
  exact ⟨eq_of_mapped_eq (by decide) (PayloadRole.mem_all a) (PayloadRole.mem_all b)
    (String.toByteArray_inj.mp hf.1), decimal_injective hf.2.1, hf.2.2⟩

/-- Every record domain is disjoint from the digest domain. -/
theorem encode_ne_digestInput (kind : RecordKind) (fields : List ByteArray)
    (role : PayloadRole) (slot : Nat) (payload : ByteArray) :
    encode kind fields ≠ digestInput role slot payload := by
  intro h
  have hk := (domain_fields_eq h).1
  cases kind <;> exact (by decide : RecordKind.label _ ≠ "digest") hk

/-- A SHA-256 value in a specific role and slot. The indices come from the caller's schema. -/
structure Digest (role : PayloadRole) (slot : Nat) where
  bytes : ByteArray
  size_eq : bytes.size = 32

/-- Validate a raw digest read at the schema-assigned role and slot. -/
def Digest.ofBytes (role : PayloadRole) (slot : Nat) (bytes : ByteArray) :
    Option (Digest role slot) :=
  if h : bytes.size = 32 then some ⟨bytes, h⟩ else none

theorem Digest.ofBytes_eq_none_iff (role : PayloadRole) (slot : Nat) (bytes : ByteArray) :
    ofBytes role slot bytes = none ↔ bytes.size ≠ 32 := by
  unfold ofBytes
  split <;> simp_all

/-- Full SHA-256 over the encoded preimage. Driver evaluation uses the Lean runtime. -/
def digest (role : PayloadRole) (slot : Nat) (payload : ByteArray) : Option (Digest role slot) :=
  (Sha256.hash (digestInput role slot payload)).bind (Digest.ofBytes role slot)

/-- Compare against a separately obtained digest in the same domain. -/
def matchesDigest (role : PayloadRole) (slot : Nat) (payload : ByteArray)
    (expected : Digest role slot) : Bool :=
  match digest role slot payload with
  | some actual => actual.bytes.data == expected.bytes.data
  | none => false

theorem matchesDigest_of_none {role : PayloadRole} {slot : Nat}
    {payload : ByteArray} (expected : Digest role slot) (h : digest role slot payload = none) :
    matchesDigest role slot payload expected = false := by
  simp [matchesDigest, h]

theorem matchesDigest_of_rejected_length {role : PayloadRole} {slot : Nat}
    {payload : ByteArray} (expected : Digest role slot)
    (h : (digestInput role slot payload).size ≥ 2^61) :
    matchesDigest role slot payload expected = false := by
  apply matchesDigest_of_none
  simp [digest, Sha256.hash_eq_none_iff.mpr h]

end Concrete.Artifact
