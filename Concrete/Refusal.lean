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


/-- A parser issue determines its registered rule independently of its wording. -/
inductive ParserIssue where
  | unexpectedToken | unexpectedEnd | trailingTokens
  | outsideProduction | nonDecimal
  | internal (detail : String)
  deriving DecidableEq, Repr

structure ParserError where
  issue : ParserIssue
  span : ConcreteSpec.Surface0.Span
  deriving DecidableEq, Repr

def ParserError.rule (error : ParserError) : RegisteredRule :=
  match error.issue with
  | .unexpectedToken | .unexpectedEnd | .trailingTokens => ⟨⟨``Slice0Rules.Parsed, 1⟩, by decide⟩
  | .outsideProduction => ⟨⟨``Slice0Rules.AdmittedProduction, 2⟩, by decide⟩
  | .nonDecimal => ⟨⟨``Slice0Rules.DecimalLiteral, 1⟩, by decide⟩
  | .internal _ => ⟨⟨``Slice0Rules.Predictive, 1⟩, by decide⟩

inductive Refusal where
  | parser (error : ParserError)
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
  | .parser ⟨.unexpectedToken, _⟩ | .parser ⟨.unexpectedEnd, _⟩ | .parser ⟨.trailingTokens, _⟩
  | .syntaxError _ => "SYNTAX"
  | .parser ⟨.outsideProduction, _⟩ | .parser ⟨.nonDecimal, _⟩ => "SLICE-0"
  | .parser ⟨.internal _, _⟩ => "INTERNAL"
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
  | .parser ⟨.internal _, _⟩ => 4
  | .parser _ | .syntaxError _ | .unsupported _ | .checker _ | .manifest .. => 1
  | .lexerDigitLimit .. | .backendResource .. | .backendProfile .. | .toolFailure _ => 2
  | .usageError _ => 3
  | .internal _ => 4

def Refusal.message : Refusal → String
  | .parser ⟨.unexpectedToken, span⟩ => s!"unexpected token at byte {span.start}"
  | .parser ⟨.unexpectedEnd, _⟩ => "unexpected end of file"
  | .parser ⟨.trailingTokens, _⟩ => "trailing tokens"
  | .parser ⟨.outsideProduction, _⟩ => "not supported in slice 0: program uses a form outside the slice-0 fragment"
  | .parser ⟨.nonDecimal, span⟩ => s!"not supported in slice 0: non-decimal integer at byte {span.start}"
  | .parser ⟨.internal detail, _⟩ => "compiler failure: " ++ detail
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
  | .parser _ | .backendProfile .. | .toolFailure _ | .manifest _ | .usageError _ => none

private def Refusal.withRule (refusal : Refusal) (rule : RegisteredRule) : Option Diagnostic :=
  let compatible : Bool := match refusal with
    | .parser error => error.rule.val == rule.val
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

/-- A transported parser citation succeeds exactly for the rule selected at the refusal site. -/
theorem Refusal.parser_diagnosticAt_iff (error : ParserError) (rule : RuleId) :
    ((Refusal.parser error).diagnosticAt rule).isSome = true ↔ rule = error.rule.val := by
  by_cases h : rule = error.rule.val
  · subst rule
    simp [diagnosticAt, validateRule, error.rule.property, withRule]
  · simp [diagnosticAt, validateRule, withRule, Ne.symm h, h]

end Concrete
