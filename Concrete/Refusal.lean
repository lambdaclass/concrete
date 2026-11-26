import ConcreteSpec.Spec.Rules

namespace Concrete

inductive Refusal where
  | syntaxError (message : String)
  | unsupported (message : String)
  | checker (message : String)
  | lexerDigitLimit (offset limit : Nat)
  | backendResource (backend resource : String) (measured limit : Nat)
  | backendProfile (backend reason : String)
  | toolFailure (reason : String)
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
  | .usageError _ => "USAGE"
  | .internal _ => "INTERNAL"

def Refusal.exitCode : Refusal → UInt32
  | .syntaxError _ | .unsupported _ | .checker _ => 1
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
  | .usageError msg => msg
  | .internal msg => "compiler failure: " ++ msg

open ConcreteSpec.Core

/-- The rule citation carries a validated name and revision and matches the refusal category. -/
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
  | .backendProfile .. | .toolFailure _ | .usageError _ => none

private def Refusal.withRule (refusal : Refusal) (rule : RegisteredRule) : Option Diagnostic := do
  let category ← refusal.ruleCategory
  if rule.val.refusalCategory = some category then some ⟨refusal, rule⟩ else none

/-- Generate a diagnostic from the current revision of a rule declaration. -/
def Refusal.diagnostic (refusal : Refusal) (rule : Lean.Name) : Option Diagnostic :=
  (referenceRule rule).bind refusal.withRule

/-- Check a diagnostic citation from a transport reader against the current registry. -/
def Refusal.diagnosticAt (refusal : Refusal) (rule : RuleId) : Option Diagnostic :=
  (validateRule rule).bind refusal.withRule

end Concrete
