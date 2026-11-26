import ConcreteSpec.Spec.Rules
import ConcreteSpec.Spec.Manifest

namespace Concrete
open ConcreteSpec.Project
open ConcreteSpec.Core

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

/-- Manifest diagnostics cite the exact registered rule for the refusal. -/
private def ManifestRefusal.registeredRule : ManifestRefusal → Option RegisteredRule
  | .size => some ⟨Manifest.sizeRule, by decide⟩
  | .encoding => some ⟨Manifest.encodingRule, by decide⟩
  | .depth => some ⟨Manifest.depthRule, by decide⟩
  | .malformed => some ⟨Manifest.syntaxRule, by decide⟩
  | .read _ | .schemaPending => none

def ManifestRefusal.ruleId (refusal : ManifestRefusal) : Option RuleId :=
  refusal.registeredRule.map Subtype.val

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
    | some rule => s!"{refusal.message} [rule {rule.name}@{rule.rev}]"
  | .usageError msg => msg
  | .internal msg => "compiler failure: " ++ msg

/-- The rule citation carries a validated name and revision and matches the refusal. -/
structure Diagnostic where
  private mk ::
  refusal : Refusal
  rule : RegisteredRule

private def Refusal.ruleCategory : Refusal → Option RuleRefusalCategory
  | .syntaxError _ => some .malformed
  | .unsupported _ => some .unsupported
  | .checker _ => some .checker
  | .lexerDigitLimit .. | .backendResource .. => some .resource
  | .internal _ => some .internal
  | .backendProfile .. | .toolFailure _ | .manifest _ | .usageError _ => none

private def Refusal.withRule (refusal : Refusal) (rule : RegisteredRule) : Option Diagnostic :=
  let compatible : Bool := match refusal with
    | .manifest detail => detail.ruleId == some rule.val
    | _ => match refusal.ruleCategory with
      | some category => rule.val.refusalCategory == some category
      | none => false
  if compatible then some ⟨refusal, rule⟩ else none

/-- Generate a diagnostic from the current revision of a rule declaration. -/
def Refusal.diagnostic (refusal : Refusal) (rule : Lean.Name) : Option Diagnostic :=
  (referenceRule rule).bind refusal.withRule

/-- Check a diagnostic citation from a transport reader against the current registry. -/
def Refusal.diagnosticAt (refusal : Refusal) (rule : RuleId) : Option Diagnostic :=
  (validateRule rule).bind refusal.withRule

end Concrete
