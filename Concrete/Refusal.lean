import ConcreteSpec.Spec.Rules

namespace Concrete

inductive Refusal where
  | syntaxError (message : String)
  | unsupported (message : String)
  | checker (message : String)
  | backendResource (backend resource : String) (measured limit : Nat)
  | internal (message : String)
  deriving DecidableEq, Repr

def Refusal.code : Refusal → String
  | .syntaxError _ => "SYNTAX"
  | .unsupported _ => "SLICE-0"
  | .checker _ => "CORE-CHECK"
  | .backendResource .. | .internal _ => "INTERNAL"

def Refusal.message : Refusal → String
  | .syntaxError msg => msg
  | .unsupported msg => "not supported in slice 0: " ++ msg
  | .checker msg => "checker refusal: " ++ msg
  | .backendResource backend resource measured limit =>
    s!"{backend} backend resource refusal: {resource}: {measured} exceeds limit {limit}"
  | .internal msg => "compiler failure: " ++ msg

/-- The rule citation for a diagnostic is generated from the declaration name. Transport
readers that already have a name and revision use `Refusal.diagnosticAt` to reject unknown references. -/
structure Diagnostic where
  refusal : Refusal
  rule : ConcreteSpec.Core.RegisteredRule

def Refusal.diagnostic (refusal : Refusal) (rule : Lean.Name) : Option Diagnostic :=
  (ConcreteSpec.Core.referenceRule rule).map fun id => ⟨refusal, id⟩

/-- Check a diagnostic citation from a transport reader against the current registry. -/
def Refusal.diagnosticAt (refusal : Refusal) (rule : ConcreteSpec.Core.RuleId) :
    Option Diagnostic :=
  (ConcreteSpec.Core.validateRule rule).map fun id => ⟨refusal, id⟩

end Concrete
