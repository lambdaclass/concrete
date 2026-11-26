import Concrete.Artifact.Encoding

open Concrete.Artifact

private def hex (value : ByteArray) : String :=
  let digits := "0123456789abcdef".toList.toArray
  String.ofList (value.toList.flatMap fun byte =>
    [digits[byte.toNat / 16]!, digits[byte.toNat % 16]!])

private def checkBytes (label : String) (actual : ByteArray) (expected : String) : IO Unit := do
  let encoded := hex actual
  unless encoded == expected do
    throw (IO.userError s!"{label}: expected {expected}, got {encoded}")
  IO.println encoded

private def requireDigest (value : Option ByteArray) : IO ByteArray :=
  match value with
  | some bytes => pure bytes
  | none => throw (IO.userError "SHA-256 refused test input")

-- The transport boundary rejects raw bytes and context mismatches at elaboration.
example (_bytes : ByteArray) (_role : PayloadRole) (_slot : Nat) : True := by
  fail_if_success have _ := RecordKind.digest
  fail_if_success have _ := matchesDigest _role _slot _bytes _bytes
  trivial

example (_bytes : ByteArray) (_expected : Digest .source 0) : True := by
  fail_if_success have _ := matchesDigest .tool 0 _bytes _expected
  fail_if_success have _ := matchesDigest .source 1 _bytes _expected
  trivial

private def check (condition : Bool) (label : String) : IO Unit := do
  unless condition do
    throw (IO.userError label)

def main : IO Unit := do
  let payload : ByteArray := ⟨#[0, 58, 255, 13, 10]⟩
  let replacement : ByteArray := ⟨#[0, 58, 254, 13, 10]⟩
  check (RecordKind.all.map RecordKind.label == ["certificate", "receipt", "facts"])
    "record label inventory changed"
  check (PayloadRole.all.map PayloadRole.label ==
    ["source", "compiler", "spec", "bir", "llvm-module", "lcb-source", "lcb-job",
     "assembly", "backend-input", "tool-input", "runtime", "site-table", "target",
     "lowering-path", "tool", "executable", "certificate", "facts"])
    "payload label inventory changed"
  for kind in RecordKind.all do
    for other in RecordKind.all do
      if kind ≠ other then
        check ((encode kind [payload]).data != (encode other [payload]).data)
          s!"colliding record kinds {repr kind} and {repr other}"
    let fields := [PayloadRole.source.label.toUTF8, "12".toUTF8, payload]
    check ((encode kind fields).data != (digestInput .source 12 payload).data)
      s!"record kind {repr kind} overlaps the digest domain"
  check ((encodeFrames ["a".toUTF8, "12:b".toUTF8]).data !=
      (encodeFrames ["a12:b".toUTF8]).data) "split and joined fields collide"
  check ((encodeFrames ["a".toUTF8, "12:b".toUTF8]).data !=
      (encodeFrames ["a12:".toUTF8, "b".toUTF8]).data) "field boundaries collide"
  check ((encodeFrames [ByteArray.empty]).data != (encodeFrames []).data)
    "empty field disappeared"
  checkBytes "nested frames" (encodeFrames [encodeFrames [ByteArray.empty, ":".toUTF8]])
    "353a303a313a3a"
  checkBytes "receipt" (encode .receipt [ByteArray.empty, "é".toUTF8, payload])
    "436f6e63726574652f302d6465763a373a72656365697074303a323ac3a9353a003aff0d0a"
  checkBytes "empty facts" (encode .facts [])
    "436f6e63726574652f302d6465763a353a6661637473"
  checkBytes "digest preimage" (digestInput .source 12 payload)
    "436f6e63726574652f302d6465763a363a646967657374363a736f75726365323a3132353a003aff0d0a"
  let some sourceDigest := digest .source 12 payload
    | throw (IO.userError "SHA-256 refused source vector")
  checkBytes "source digest" sourceDigest.bytes
    "5d443ee518a7f11ad347da6202200909b985dfb1fe74d23069803e3c5b495438"
  checkBytes "SHA-256 empty" (← requireDigest (Sha256.hash ByteArray.empty))
    "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
  checkBytes "SHA-256 abc" (← requireDigest (Sha256.hash "abc".toUTF8))
    "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
  let twoBlocks := "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq".toUTF8
  checkBytes "SHA-256 padding crosses block" (← requireDigest (Sha256.hash twoBlocks))
    "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"
  -- Independent SHA-256 vectors with high-bit bytes in every word lane.
  for (size, expected) in [
      (55, "14f503abd5f96b9fd1c6f01fd453f08b23358b17631d5a7d8ad0203b701f055a"),
      (63, "ff50f8e2a19c463f596f4c69fa7421ef505e1dea5602fd517a62243b297de483"),
      (64, "c39e13bbb05726a3c0747d3ca54c27e3f86bc10a1d3754cd031bd1ca7256c8ed"),
      (119, "0f7b2e13e4bfc6c6ff690113666b5d32328d962a25d8d42a47733fd5a5e54d51"),
      (120, "68fe4f26b94bc04bcbc1bf163566dba1e1d732f4151514f9ffc0533eec6506a9")] do
    let message : ByteArray := ⟨(Array.range size).map fun i => (128 + i).toUInt8⟩
    checkBytes s!"SHA-256 length {size}" (← requireDigest (Sha256.hash message)) expected
  for size in [0, 31, 33, 64] do
    let malformed : ByteArray := ⟨Array.replicate size 0x30⟩
    check (Digest.ofBytes .source 0 malformed).isNone s!"accepted {size}-byte digest"
  for role in PayloadRole.all do
    let some original := digest role 0 payload
      | throw (IO.userError s!"SHA-256 refused role {repr role}")
    let some expected := Digest.ofBytes role 0 original.bytes
      | throw (IO.userError s!"digest parser refused role {repr role}")
    check (matchesDigest role 0 payload expected) s!"positive control failed in role {repr role}"
    check (!matchesDigest role 0 replacement expected) s!"byte substitution in role {repr role}"
    let some moved := Digest.ofBytes role 1 original.bytes
      | throw (IO.userError "slot substitution control constructed an invalid digest length")
    check (!matchesDigest role 1 payload moved) s!"slot substitution in role {repr role}"
    for other in PayloadRole.all do
      if other ≠ role then
        let some relabeled := Digest.ofBytes other 0 original.bytes
          | throw (IO.userError "role substitution control constructed an invalid digest length")
        check (!matchesDigest other 0 payload relabeled)
          s!"domain substitution from {repr role} to {repr other}"
    IO.println s!"{role.label}: {hex expected.bytes}"
