import Concrete.Refusal
import ConcreteSpec.Spec.Manifest

/-! Pure validation of the configuration envelope. -/

namespace Concrete.Driver.Project
open ConcreteSpec.Project

def manifestByteLimit : Nat := Manifest.byteLimit

def manifestDepthLimit : Nat := Manifest.depthLimit

/-- Validate the byte envelope before any decoder can inspect it. -/
def manifestText (bytes : ByteArray) : Except ManifestRefusal String := do
  if ¬ Manifest.SizeAccepted bytes.size then throw .size
  let some value := String.fromUTF8? bytes | throw .encoding
  return value

/-- Check the bounded syntax supported by the configuration boundary. -/
def parseManifest (text : String) : Except ManifestRefusal Unit :=
  if ¬ Manifest.SizeAccepted text.utf8ByteSize then .error .size
  else (Manifest.validate manifestDepthLimit text).mapError fun
    | .malformed => .malformed
    | .depth => .depth

end Concrete.Driver.Project
