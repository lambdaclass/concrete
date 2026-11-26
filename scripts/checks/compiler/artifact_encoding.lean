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

private def roles : List PayloadRole :=
  [.source, .compiler, .spec, .bir, .llvmModule, .lcbSource, .lcbJob, .assembly,
   .backendInput, .toolInput, .runtime, .siteTable, .target, .loweringPath, .tool,
   .executable, .certificate, .facts]

def main : IO Unit := do
  let payload : ByteArray := ⟨#[0, 58, 255, 13, 10]⟩
  let replacement : ByteArray := ⟨#[0, 58, 254, 13, 10]⟩
  checkBytes "receipt" (encode .receipt [ByteArray.empty, "é".toUTF8, payload])
    "436f6e63726574652f302d6465763a373a72656365697074303a323ac3a9353a003aff0d0a"
  checkBytes "empty facts" (encode .facts [])
    "436f6e63726574652f302d6465763a353a6661637473"
  checkBytes "digest preimage" (digestInput .source 12 payload)
    "436f6e63726574652f302d6465763a363a646967657374363a736f75726365323a3132353a003aff0d0a"
  checkBytes "source digest" (← requireDigest (digest .source 12 payload))
    "5d443ee518a7f11ad347da6202200909b985dfb1fe74d23069803e3c5b495438"
  checkBytes "SHA-256 empty" (← requireDigest (Sha256.hash ByteArray.empty))
    "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
  checkBytes "SHA-256 abc" (← requireDigest (Sha256.hash "abc".toUTF8))
    "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
  let twoBlocks := "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq".toUTF8
  checkBytes "SHA-256 padding crosses block" (← requireDigest (Sha256.hash twoBlocks))
    "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"
  for role in roles do
    let expected ← requireDigest (digest role 0 payload)
    unless matchesDigest role 0 payload expected do
      throw (IO.userError s!"positive control failed in role {repr role}")
    if matchesDigest role 0 replacement expected then
      throw (IO.userError s!"byte substitution accepted in role {repr role}")
    if matchesDigest role 1 payload expected then
      throw (IO.userError s!"slot substitution accepted in role {repr role}")
    for other in roles do
      if other ≠ role && matchesDigest other 0 payload expected then
        throw (IO.userError s!"domain substitution accepted from {repr role} to {repr other}")
