import Concrete.Artifact.Sha256

/-!
# Provisional artifact byte encoding (A13)

A frame is ASCII decimal byte length (no leading zeroes, except zero), ASCII colon, then
exactly that many bytes. A record starts with the literal bytes `Concrete/0-dev:` and a
framed record kind, followed by framed fields in schema order through end of input.
The caller owns the schema and supplies canonical field bytes, including UTF-8 where the
schema calls for text. Raw byte fields are preserved verbatim.

The digest preimage is the same record encoding with kind `digest`, a fixed role label,
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
  | certificate | receipt | facts | digest
  deriving DecidableEq, Repr

private def RecordKind.label : RecordKind → String
  | .certificate => "certificate"
  | .receipt => "receipt"
  | .facts => "facts"
  | .digest => "digest"

private def appendFrame (out bytes : ByteArray) : ByteArray :=
  (out ++ (toString bytes.size).toUTF8).push 0x3a ++ bytes

/-- Encode ordered fields once, with byte lengths independent of locale and host endianness. -/
def encode (kind : RecordKind) (fields : List ByteArray) : ByteArray :=
  fields.foldl appendFrame (appendFrame "Concrete/0-dev:".toUTF8 kind.label.toUTF8)

/-- Domains for payloads referenced by pre-link evidence and the single-build receipt. -/
inductive PayloadRole where
  | source | compiler | spec | bir
  | llvmModule | lcbSource | lcbJob | assembly | backendInput | toolInput
  | runtime | siteTable | target | loweringPath | tool
  | executable | certificate | facts
  deriving DecidableEq, Repr

private def PayloadRole.label : PayloadRole → String
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

/-- The byte sequence hashed for a payload in its schema-assigned role and slot. -/
def digestInput (role : PayloadRole) (slot : Nat) (payload : ByteArray) : ByteArray :=
  encode .digest [role.label.toUTF8, (toString slot).toUTF8, payload]

/-- Full SHA-256 over the encoded preimage. Driver evaluation uses the Lean runtime. -/
def digest (role : PayloadRole) (slot : Nat) (payload : ByteArray) : Option ByteArray :=
  Sha256.hash (digestInput role slot payload)

/-- Compare against a separately obtained digest. A failed hash computation fails closed. -/
def matchesDigest (role : PayloadRole) (slot : Nat) (payload expected : ByteArray) : Bool :=
  match digest role slot payload with
  | some actual => actual.data == expected.data
  | none => false

end Concrete.Artifact
