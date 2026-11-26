import ConcreteSpec.Spec.Manifest

namespace Concrete
open ConcreteSpec.Project

inductive ManifestRefusal where
  | read (detail : String)
  | size
  | encoding
  | depth
  | malformed
  | schemaPending
  deriving DecidableEq, Repr

/-- Driver diagnostics for the pre-admission boundary. -/
def ManifestRefusal.code : ManifestRefusal → String
  | .read _ => "MANIFEST-READ"
  | .size => "MANIFEST-SIZE"
  | .encoding => "MANIFEST-ENCODING"
  | .depth => "MANIFEST-DEPTH"
  | .malformed => "MANIFEST-SYNTAX"
  | .schemaPending => "MANIFEST-SCHEMA"

def ManifestRefusal.ruleId : ManifestRefusal → Option ConcreteSpec.Core.RuleId
  | .size => some Manifest.sizeRule
  | .encoding => some Manifest.encodingRule
  | .depth => some Manifest.depthRule
  | .malformed => some Manifest.syntaxRule
  | .read _ | .schemaPending => none

def ManifestRefusal.message : ManifestRefusal → String
  | .read detail => s!"cannot read Concrete.toml ({detail})"
  | .size => s!"Concrete.toml exceeds {Manifest.byteLimit} bytes"
  | .encoding => "Concrete.toml must be UTF-8"
  | .depth => s!"Concrete.toml exceeds the nesting limit of {Manifest.depthLimit}"
  | .malformed => "Concrete.toml is malformed"
  | .schemaPending => "Concrete.toml requires an approved manifest schema"


inductive Refusal where
  | syntaxError (message : String)
  | unsupported (message : String)
  | checker (message : String)
  | lexerDigitLimit (offset limit : Nat)
  | backendResource (backend resource : String) (measured limit : Nat)
  | backendProfile (backend reason : String)
  | toolFailure (reason : String)
  | manifest (refusal : ManifestRefusal)
  | usageError (message : String)
  | internal (message : String)
  deriving DecidableEq, Repr

def Refusal.code : Refusal → String
  | .syntaxError _ => "SYNTAX"
  | .unsupported _ => "SLICE-0"
  | .checker _ => "CORE-CHECK"
  | .lexerDigitLimit .. => "RESOURCE"
  | .backendResource .. => "BACKEND-LIMIT"
  | .backendProfile .. => "BACKEND-PROFILE"
  | .toolFailure _ => "TOOL-FAILURE"
  | .manifest refusal => refusal.code
  | .usageError _ => "USAGE"
  | .internal _ => "INTERNAL"

def Refusal.exitCode : Refusal → UInt32
  | .syntaxError _ | .unsupported _ | .checker _ | .manifest .. => 1
  | .lexerDigitLimit .. | .backendResource .. | .backendProfile .. | .toolFailure _ => 2
  | .usageError _ => 3
  | .internal _ => 4

-- Manifest rule IDs name declarations in a fixed namespace. Their identifiers
-- need no Lean pretty-printer escaping at this diagnostic boundary.
private def manifestRuleName : Lean.Name → String
  | .anonymous => ""
  | .str .anonymous segment => segment
  | .str parent segment => manifestRuleName parent ++ "." ++ segment
  | .num parent index => manifestRuleName parent ++ "." ++ toString index

def Refusal.message : Refusal → String
  | .syntaxError msg => msg
  | .unsupported msg => "not supported in slice 0: " ++ msg
  | .checker msg => "checker refusal: " ++ msg
  | .lexerDigitLimit offset limit =>
    s!"integer digit run at byte {offset} exceeds limit {limit}"
  | .backendResource backend resource measured limit =>
    s!"incomplete ({backend} backend: {resource}: {measured} exceeds limit {limit})"
  | .backendProfile backend reason => s!"incomplete ({backend} backend: {reason})"
  | .toolFailure reason => s!"incomplete ({reason})"
  | .manifest refusal =>
    match refusal.ruleId with
    | none => refusal.message
    | some rule => s!"{refusal.message} [rule {manifestRuleName rule.name}@{rule.rev}]"
  | .usageError msg => msg
  | .internal msg => "compiler failure: " ++ msg

end Concrete
