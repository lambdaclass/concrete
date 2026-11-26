import ConcreteSpec.Spec.Hybrid
import ConcreteSpec.Spec.Own
import ConcreteSpec.Spec.Surface0

/-! Spec-owned data and signatures at the compiler boundary. -/
namespace ConcreteSpec.StageInterface

open Core Core.SourcePathSpan

abbrev Slice := Fin 22

structure FileId where
  index : Nat
  deriving DecidableEq, Repr

/-- Coordinates use Core.Span's half-open byte range and 1-based line and column. -/
structure FileSpan where
  file : FileId
  startOffset : Nat
  endOffset : Nat
  line : Nat
  col : Nat
  deriving DecidableEq, Repr

def FileSpan.Binds (span : FileSpan) (project : Project) (specSpan : Span) : Prop :=
  ∃ file, project.files[span.file.index]? = some file ∧
    SourcePathSpan.Reference.Binds project specSpan file ∧ specSpan.LocationOk file.source ∧
    span.startOffset = specSpan.startOffset ∧ span.endOffset = specSpan.endOffset ∧
    span.line = specSpan.line ∧ span.col = specSpan.col

structure Source where
  project : Project
  entryFile : FileId
  slice : Slice

structure Located (A : Type) where
  span : FileSpan
  value : A

structure TokenStream where
  source : Source
  tokens : Array (Located ConcreteSpec.Surface0.TokenValue)
  /-- Trivia preceding each token, followed by the EOF trivia. -/
  trivia : Array (Array FileSpan)

structure ParserView where
  source : Source
  tokens : Array (Located ConcreteSpec.Surface0.TokenValue)

def TokenStream.parserView (stream : TokenStream) : ParserView :=
  ⟨stream.source, stream.tokens⟩

/-- AST is the result of the selected typed start action of the shared grammar.
Later slices supply larger action families through the same handoff. -/
structure Parsed (Ast : Type) where
  source : Source
  tree : Ast

inductive ParseRefusal where
  | malformed (rule : RuleId) (atSpan : FileSpan)
  | unsupported (rule : RuleId) (atSpan : FileSpan) (required : Slice)

structure Origin where
  operation : SiteId
  lexical : FileSpan
  evidenceKey : StableKey
  deriving DecidableEq, Repr

structure ResourceRefusal where
  rule : RuleId
  measured : Nat
  limit : Nat

structure CheckerRefusal where
  rule : RuleId
  primary : FileSpan
  related : List (Name × FileSpan)
  obligation : Option ObligationId

/-- The operand family is supplied by ConcreteSpec. Control, calls and scoped lifetime use Exec. -/
structure CoreLanguage where
  operands : Exec.Operands
  hybridStore : operands.Store = Hybrid.State
  Declaration : Type
  permit : AList Var Own.Descriptor → AList Var operands.Val → operands.Expr →
    Own.State → Option Own.State

abbrev CoreLanguage.instrumented (language : CoreLanguage)
    (ownership : AList Var Own.Descriptor) : Exec.Operands :=
  Own.instrumentOperands language.operands (AList.lookup ownership) (language.permit ownership)

/-- The early fragment uses the common scalar vocabulary and nominal identities.
Declaration shape, Copy and fragment eligibility are checked by the spec. -/
inductive EarlyTy where
  | scalar (ty : CTy)
  | nominal (decl : DeclId)
  deriving DecidableEq

inductive EarlyValue where
  | scalar (value : CVal)
  | empty (decl : DeclId)

inductive EarlyOperand where
  | literal (value : CVal)
  | copy (variableId : Var)
  | move (variableId : Var)

inductive EarlyExpr where
  | read (value : EarlyOperand)
  | add (ty : IntTy) (left right : EarlyOperand)
  | constructEmpty (decl : DeclId)

inductive EarlyPrim where
  | destructureEmpty (decl : DeclId)

abbrev EarlyDeclaration := Unit

structure EarlyCore (language : CoreLanguage) where
  values : language.operands.Val = EarlyValue
  types : language.operands.Ty = EarlyTy
  expressions : language.operands.Expr = EarlyExpr
  primitives : language.operands.Prim = EarlyPrim
  declarations : language.Declaration = EarlyDeclaration

structure Payload (language : CoreLanguage) where
  ownership : AList Var Own.Descriptor
  body : Exec.Program (language.instrumented ownership)
  entry : FnId
  origins : AList SiteId Origin
  declarations : AList DeclId language.Declaration

abbrev Payload.machine {language : CoreLanguage} (payload : Payload language) : Exec.Operands :=
  language.instrumented payload.ownership

structure CoreInput (language : CoreLanguage) where
  project : Project
  slice : Slice
  payload : Payload language

def CoreInput.LocationsWF {language : CoreLanguage} (input : CoreInput language) : Prop :=
  (List.map Prod.fst input.payload.origins).Nodup ∧
  (List.map (fun (entry : SiteId × Origin) => entry.2.evidenceKey) input.payload.origins).Nodup ∧
  ∀ (site : SiteId) (origin : Origin), (site, origin) ∈ input.payload.origins →
    site = origin.operation ∧ ∃ span, origin.lexical.Binds input.project span

/-- A discharge identifies an exact operation, ordered typed operands and obligation.
The enclosing set belongs to the exact Core payload and its checking context. -/
structure Discharge (language : CoreLanguage) where
  operation : SiteId
  key : StableKey
  obligation : ObligationId
  operandTypes : List language.operands.Ty
  operands : List language.operands.Expr

/-- An operation can have several independent conditions, such as divisor nonzero and
the signed division overflow guard. Each obligation appears once. -/
abbrev Discharges (language : CoreLanguage) := AList SiteId (List (Discharge language))

def Discharges.Contains {language : CoreLanguage} (discharges : Discharges language)
    (site : SiteId) (d : Discharge language) : Prop :=
  ∃ group, (site, group) ∈ discharges ∧ d ∈ group

structure Admitted (language : CoreLanguage) where
  core : CoreInput language
  discharges : Discharges language

/-- The spec owns the fragment, independent statics, acceptance, and operation safety judgments.
The checker is the single executable traversal. -/
structure Admission (language : CoreLanguage) where
  fragment : Slice → Payload language → Prop
  statics : CoreInput language → Prop
  accepted : CoreInput language → Prop
  rejects : CoreInput language → CheckerRefusal → Prop
  initial : (input : CoreInput language) → Exec.Config input.payload.machine → Prop
  matchesOperation : CoreInput language → Discharge language → Prop
  atOperation : (input : CoreInput language) → Exec.Config input.payload.machine → SiteId → Prop
  holds : (input : CoreInput language) → Discharge language → Exec.Config input.payload.machine → Prop
  check : CoreInput language → Except CheckerRefusal (Discharges language)

def Admission.ValidDischarges {language : CoreLanguage} (spec : Admission language)
    (input : CoreInput language) (discharges : Discharges language) : Prop :=
  (List.map Prod.fst discharges).Nodup ∧
  (∀ site group, (site, group) ∈ discharges →
    group ≠ [] ∧ (group.map Discharge.obligation).Nodup) ∧
  ∀ (site : SiteId) (d : Discharge language), discharges.Contains site d →
    site = d.operation ∧ d.obligation.site = site ∧ spec.matchesOperation input d ∧
    (∃ origin, (site, origin) ∈ input.payload.origins ∧ origin.evidenceKey = d.key) ∧
    ∀ start state events, spec.initial input start →
      Exec.Steps input.payload.body start events state → spec.atOperation input state site →
      spec.holds input d state

def Admission.Sound {language : CoreLanguage} (spec : Admission language) : Prop :=
  ∀ input, input.LocationsWF → spec.fragment input.slice input.payload →
    (spec.accepted input ↔ ∃ discharges, spec.check input = .ok discharges) ∧
    (∀ discharges, spec.check input = .ok discharges →
      spec.statics input ∧ spec.ValidDischarges input discharges) ∧
    (∀ refusal, spec.check input = .error refusal → spec.rejects input refusal)

/-- Layout is a spec function at the resolved target. A zero extent still has logical lifetime. -/
structure Layout where
  size : Nat
  align : Nat
  deriving DecidableEq, Repr

abbrev LayoutOf (Ty : Type) := Target → Ty → Option Layout

end ConcreteSpec.StageInterface
