import ConcreteSpec.StageInterface
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


open ConcreteSpec.StageInterface (RefusalCause ParserIssue Surface0Issue)

private def RefusalCause.code : RefusalCause → String
  | .lexical (.digitLimit _) _ => "RESOURCE"
  | .lexical .. => "SYNTAX"
  | .parser (.internal _) _ => "INTERNAL"
  | .parser .outsideProduction _ | .parser .nonDecimal _
  | .surface (.unexpectedEntry _) | .surface (.wildcard _) => "SLICE-0"
  | .parser .. => "SYNTAX"
  | .surface _ | .core .. => "CORE-CHECK"

private def RefusalCause.message : RefusalCause → String
  | .lexical (.encoding reason) span => s!"invalid source at byte {span.start}: {repr reason}"
  | .lexical (.digitLimit limit) span => s!"integer digit run at byte {span.start} exceeds limit {limit}"
  | .lexical issue span =>
    let detail := match issue with
      | .integer => "malformed integer"
      | .string => "malformed string"
      | .byte => "malformed byte literal"
      | .comment => "forbidden comment character"
      | _ => "unexpected character"
    s!"{detail} at byte {span.start}"
  | .parser .unexpectedToken span => s!"unexpected token at byte {span.start}"
  | .parser .unexpectedEnd _ => "unexpected end of file"
  | .parser .trailingTokens _ => "trailing tokens"
  | .parser .outsideProduction _ => "not supported in slice 0: program uses a form outside the slice-0 fragment"
  | .parser .nonDecimal span => s!"not supported in slice 0: non-decimal integer at byte {span.start}"
  | .parser (.internal _) _ => "compiler failure: invalid parser transition"
  | .surface (.unexpectedEntry span) => s!"expected the entry point 'main' at byte {span.start}"
  | .surface (.wildcard span) => s!"wildcard binding at byte {span.start}"
  | .surface (.fallthrough _) => "checker refusal: reachable end of a u8 function (STMT-9)"
  | .surface (.unreachable span) =>
    s!"checker refusal: unreachable code (STMT-14) at {span.line}:{span.column}, bytes {span.start}..{span.stop}"
  | .surface (.integerSuffix span) => s!"checker refusal: integer suffix in an expected-type position at byte {span.start}"
  | .surface (.unknownLocal name span) => s!"checker refusal: unknown local '{name}' at byte {span.start}"
  | .surface (.duplicateBinder name span) => s!"checker refusal: duplicate binding '{name}' at byte {span.start}"
  | .surface (.reservedBinder name span) => s!"checker refusal: reserved binding '{name}' at byte {span.start}"
  | .core .. => "checker refusal: Core check rejected the program"

instance : Repr RefusalCause where
  reprPrec cause prec := reprPrec (cause.rule, cause.primary) prec

inductive Refusal where
  | source (cause : RefusalCause)
  | path (reason : ConcreteSpec.Core.SourcePath.Reason) (offset : Nat)
  | missingLocation (reason : ConcreteSpec.Admission.Refusal)
  | backendResource (backend resource : String) (measured limit : Nat)
  | backendProfile (backend reason : String)
  | toolFailure (reason : String)
  | manifest (refusal : ManifestRefusal)
  | usageError (message : String)
  | internal (message : String)
  deriving DecidableEq

def Refusal.code : Refusal → String
  | .source cause => RefusalCause.code cause
  | .path .. => "SYNTAX"
  | .missingLocation _ => "INTERNAL"
  | .backendResource .. => "BACKEND-LIMIT"
  | .backendProfile .. => "BACKEND-PROFILE"
  | .toolFailure _ => "TOOL-FAILURE"
  | .manifest refusal => refusal.code
  | .usageError _ => "USAGE"
  | .internal _ => "INTERNAL"

def Refusal.exitCode : Refusal → UInt32
  | .source (.parser (.internal _) _) | .missingLocation _ => 4
  | .source (.lexical (.digitLimit _) _) => 2
  | .source _ | .path .. | .manifest .. => 1
  | .backendResource .. | .backendProfile .. | .toolFailure _ => 2
  | .usageError _ => 3
  | .internal _ => 4

def Refusal.message : Refusal → String
  | .source cause => RefusalCause.message cause
  | .path .. => "expected a canonical project-relative source path"
  | .missingLocation _ => "compiler failure: missing source location for Core refusal"
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

instance : Repr Refusal where
  reprPrec refusal prec := reprPrec refusal.message prec

private def Refusal.citation : Refusal → Option RuleId
  | .source cause => some cause.rule
  | .manifest detail => detail.ruleId
  | .path reason _ => some reason.ruleId
  | _ => none

/-- A transported citation must equal the producer's exact rule and revision. -/
def Refusal.diagnosticAt (refusal : Refusal) (rule : RuleId) : Option Diagnostic := do
  let registered ← validateRule rule
  if refusal.citation = some rule then return ⟨refusal, registered⟩ else none

def Refusal.diagnostic (refusal : Refusal) (name : Lean.Name) : Option Diagnostic := do
  let registered ← referenceRule name
  refusal.diagnosticAt registered.val

theorem Refusal.source_diagnosticAt_iff (cause : RefusalCause) (rule : RuleId) :
    ((Refusal.source cause).diagnosticAt rule).isSome = true ↔ rule = cause.rule := by
  by_cases h : rule = cause.rule
  · subst rule
    simp [diagnosticAt, validateRule, cause.citation.property, ConcreteSpec.StageInterface.RefusalCause.rule,
      citation, bind, Option.bind]
  · simp [diagnosticAt, validateRule, citation, Ne.symm h, h, bind, Option.bind]
    split <;> rfl

end Concrete
