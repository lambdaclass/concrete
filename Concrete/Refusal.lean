import ConcreteSpec.StageInterface
import ConcreteSpec.Spec.Manifest

namespace Concrete
open ConcreteSpec.Project
open ConcreteSpec.Core

inductive ManifestRefusal where
  | read (detail : String)
  | size
  | encoding (span : Manifest.ByteSpan)
  | invalid (error : Manifest.Error)
  | schemaPending
  deriving DecidableEq, Repr

/-- Driver diagnostics for the pre-admission boundary. -/
def ManifestRefusal.code : ManifestRefusal → String
  | .read _ => "MANIFEST-READ"
  | .size => "MANIFEST-SIZE"
  | .encoding _ => "MANIFEST-ENCODING"
  | .invalid (.depth _) => "MANIFEST-DEPTH"
  | .invalid (.malformed _) => "MANIFEST-SYNTAX"
  | .schemaPending => "MANIFEST-SCHEMA"

/-- Manifest diagnostics cite the exact registered rule for the refusal. -/
private def ManifestRefusal.reference : ManifestRefusal → Option (RegisteredRule × Manifest.ByteSpan)
  | .size => some (⟨Manifest.sizeRule, by decide⟩, ⟨Manifest.byteLimit, Manifest.byteLimit + 1⟩)
  | .encoding span => some (⟨Manifest.encodingRule, by decide⟩, span)
  | .invalid (.depth span) => some (⟨Manifest.depthRule, by decide⟩, span)
  | .invalid (.malformed span) => some (⟨Manifest.syntaxRule, by decide⟩, span)
  | .read _ | .schemaPending => none

def ManifestRefusal.ruleId (refusal : ManifestRefusal) : Option RuleId :=
  refusal.reference.map (·.1.val)

def ManifestRefusal.message : ManifestRefusal → String
  | .read detail => s!"cannot read Concrete.toml ({detail})"
  | .size => s!"Concrete.toml exceeds {Manifest.byteLimit} bytes"
  | .encoding _ => "Concrete.toml must be UTF-8"
  | .invalid (.depth _) => s!"Concrete.toml exceeds the nesting limit of {Manifest.depthLimit}"
  | .invalid (.malformed _) => "Concrete.toml is malformed"
  | .schemaPending => "Concrete.toml requires an approved manifest schema"


open ConcreteSpec.StageInterface (RefusalCause ParserIssue Surface0Issue ParseRefusal
  MalformedCause UnsupportedCause FileSpan)

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
  | .lexical (.digitLimit limit) span => s!"integer digit at byte {span.start} exceeds limit {limit}"
  | .lexical issue span =>
    let detail := match issue with
      | .integer .leadingZero => "leading zero in decimal integer"
      | .integer (.invalidSuffix suffix) => s!"invalid integer suffix '{ConcreteSpec.Surface0.scalarText suffix}'"
      | .integer (.missingDigits radix) => s!"missing digits in {repr radix} integer"
      | .string .unterminated => "unterminated string"
      | .string (.invalidEscape c) => s!"invalid string escape ({c})"
      | .string (.rawForbidden c) => s!"forbidden string character ({c})"
      | .byte .unterminated => "unterminated byte literal"
      | .byte (.invalidEscape c) => s!"invalid byte escape ({c})"
      | .byte (.invalidRaw c) => s!"forbidden byte character ({c})"
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
  | path (failure : SourcePath.Refusal)
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
  | .manifest refusal => refusal.message
  | .usageError msg => msg
  | .internal msg => "compiler failure: " ++ msg

/-- The rule citation carries a validated name and revision and matches the refusal. -/
structure Diagnostic where
  private mk ::
  refusal : Refusal
  rule : RegisteredRule

instance : Repr Refusal where
  reprPrec refusal prec := reprPrec refusal.message prec

private inductive DiagnosticLocation where
  | file (span : ConcreteSpec.StageInterface.FileSpan)
  | path (start stop : Nat)
  | manifest (span : Manifest.ByteSpan)

private def Refusal.reference : Refusal → Option (RegisteredRule × DiagnosticLocation)
  | .source cause => some (cause.citation, .file cause.primary)
  | .path failure =>
    let reason := failure.violation.reason
    let loc := failure.violation.location
    let rule : RegisteredRule := ⟨reason.ruleId, by cases reason <;> decide⟩
    some (rule, .path loc.offset (min (loc.offset + 1) loc.stop))
  | .manifest detail => detail.reference.map fun (rule, span) => (rule, .manifest span)
  | .missingLocation _ | .backendResource .. | .backendProfile .. | .toolFailure _
  | .usageError _ | .internal _ => none

/-- The public renderer consumes the same citation as diagnostic validation. -/
def Refusal.render (r : Refusal) : String := Id.run do
  let detail := if r.code == "SYNTAX" then s!"{r.code.toLower} error: {r.message}" else r.message
  let base := s!"{r.code}: {detail}"
  let some (rule, location) := r.reference | return base
  let whereAt := match location with
    | .file s => s!", file {s.file.index}, bytes {s.startOffset}..{s.endOffset}"
    | .path start stop => s!", path, bytes {start}..{stop}"
    | .manifest span => s!", Concrete.toml, bytes {span.start}..{span.stop}"
  return s!"{base} [rule {rule.val.name}@{rule.val.rev}{whereAt}]"

/-- A transported citation must equal the producer's exact rule and revision. -/
def Refusal.diagnosticAt (refusal : Refusal) (rule : RuleId) : Option Diagnostic := do
  let (registered, _) ← refusal.reference
  if registered.val = rule then return ⟨refusal, registered⟩ else none

def Refusal.diagnostic (refusal : Refusal) (name : Lean.Name) : Option Diagnostic := do
  let registered ← referenceRule name
  refusal.diagnosticAt registered.val

theorem Refusal.source_diagnosticAt_iff (cause : RefusalCause) (rule : RuleId) :
    ((Refusal.source cause).diagnosticAt rule).isSome = true ↔ rule = cause.rule := by
  change (if cause.rule = rule then some ⟨.source cause, cause.citation⟩ else (none : Option Diagnostic)).isSome =
    true ↔ rule = cause.rule
  cases decEq cause.rule rule with
  | isTrue same =>
    rw [ite_eq_left same]
    exact ⟨fun _ => same.symm, fun _ => rfl⟩
  | isFalse same =>
    rw [ite_eq_right same]
    exact ⟨nofun, fun h => (same h.symm).elim⟩

/-- Project a grammar-level `ParseRefusal` into the shared `RefusalCause` carrier.
Stream-coordinate violations (`ParseRefusal.invalidStream`) precede grammar
execution and have no `ParserIssue`. -/
def RefusalCause.ofParseRefusal? (refusal : ParseRefusal) : Option RefusalCause :=
  refusal.toParserIssue?.map fun issue => .parser issue refusal.span.surface

private def locationRuleLabel : SourcePathSpan.LocationRefusal → String
  | .unknownFile => "ConcreteSpec.Core.SourcePathSpan.SlotResolves@1"
  | .reversedRange => "ConcreteSpec.Core.SourcePathSpan.OrderedOffsets@2"
  | .pastEnd _ => "ConcreteSpec.Core.SourcePathSpan.EndInSource@2"
  | .wrongLine _ => "ConcreteSpec.Core.SourcePathSpan.LineMatches@2"
  | .wrongColumn _ => "ConcreteSpec.Core.SourcePathSpan.ColumnMatches@2"

/-- Transport any typed `ParseRefusal` from `ConcreteSpec.StageInterface` into
the driver's single `Refusal` renderer (`Refusal.render`), retaining the exact
rule revision (`Parsed@1`, `AdmittedProduction@3`, `DecimalLiteral@1`) and span. -/
def Refusal.ofParseRefusal : ParseRefusal → Refusal
  | .invalidStream span reason =>
    .internal s!"invalid token stream location at file {span.file.index}, bytes {span.startOffset}..{span.endOffset} [rule {locationRuleLabel reason}]"
  | .malformed _ .unexpectedToken span => .source (.parser .unexpectedToken span.surface)
  | .malformed _ .unexpectedEnd span => .source (.parser .unexpectedEnd span.surface)
  | .malformed _ .trailingTokens span => .source (.parser .trailingTokens span.surface)
  | .unsupported _ (.production _) span _ => .source (.parser .outsideProduction span.surface)
  | .unsupported _ (.nonDecimal _) span _ => .source (.parser .nonDecimal span.surface)

theorem RefusalCause.ofParseRefusal?_primary {refusal : ParseRefusal} {cause : RefusalCause}
    (h : RefusalCause.ofParseRefusal? refusal = some cause) :
    cause.primary = refusal.span := by
  unfold RefusalCause.ofParseRefusal? at h
  cases hi : refusal.toParserIssue? with
  | none => rw [hi] at h; cases h
  | some issue =>
    rw [hi] at h
    cases Option.some.inj h
    cases refusal.span
    rfl

theorem RefusalCause.ofParseRefusal?_malformed (rule : RegisteredRule) (cause : MalformedCause)
    (span : FileSpan) :
    ∃ issue,
      RefusalCause.ofParseRefusal? (.malformed rule cause span) = some (.parser issue span.surface) ∧
      (RefusalCause.parser issue span.surface).rule = ⟨``Slice0Rules.Parsed, 1⟩ ∧
      (RefusalCause.parser issue span.surface).primary = span := by
  cases cause with
  | unexpectedToken => exact ⟨.unexpectedToken, rfl, rfl, by cases span; rfl⟩
  | unexpectedEnd => exact ⟨.unexpectedEnd, rfl, rfl, by cases span; rfl⟩
  | trailingTokens => exact ⟨.trailingTokens, rfl, rfl, by cases span; rfl⟩

theorem RefusalCause.ofParseRefusal?_unsupported_production (rule : RegisteredRule)
    (id : ConcreteSpec.GrammarData.ProductionId) (span : FileSpan)
    (required : ConcreteSpec.StageInterface.Slice) :
    RefusalCause.ofParseRefusal? (.unsupported rule (.production id) span required) =
        some (.parser .outsideProduction span.surface) ∧
      (RefusalCause.parser .outsideProduction span.surface).rule = ⟨``Slice0Rules.AdmittedProduction, 3⟩ ∧
      (RefusalCause.parser .outsideProduction span.surface).primary = span :=
  ⟨rfl, rfl, by cases span; rfl⟩

theorem RefusalCause.ofParseRefusal?_unsupported_nonDecimal (rule : RegisteredRule)
    (radix : ConcreteSpec.Surface0.Radix) (span : FileSpan)
    (required : ConcreteSpec.StageInterface.Slice) :
    RefusalCause.ofParseRefusal? (.unsupported rule (.nonDecimal radix) span required) =
        some (.parser .nonDecimal span.surface) ∧
      (RefusalCause.parser .nonDecimal span.surface).rule = ⟨``Slice0Rules.DecimalLiteral, 1⟩ ∧
      (RefusalCause.parser .nonDecimal span.surface).primary = span :=
  ⟨rfl, rfl, by cases span; rfl⟩

theorem Refusal.ofParseRefusal_eq_source {refusal : ParseRefusal} {cause : RefusalCause}
    (h : RefusalCause.ofParseRefusal? refusal = some cause) :
    Refusal.ofParseRefusal refusal = .source cause := by
  cases refusal with
  | invalidStream _ _ => cases h
  | malformed _ c _ =>
    cases c <;> cases h <;> rfl
  | unsupported _ c _ _ =>
    cases c <;> cases h <;> rfl

theorem Refusal.ofParseRefusal_diagnosticAt_iff {refusal : ParseRefusal} {cause : RefusalCause}
    (h : RefusalCause.ofParseRefusal? refusal = some cause) (rule : RuleId) :
    ((Refusal.ofParseRefusal refusal).diagnosticAt rule).isSome = true ↔ rule = cause.rule := by
  rw [Refusal.ofParseRefusal_eq_source h]
  exact Refusal.source_diagnosticAt_iff cause rule

end Concrete
