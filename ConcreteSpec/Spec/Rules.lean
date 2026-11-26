import ConcreteSpec.Spec.Accounting
import ConcreteSpec.Spec.Const
import ConcreteSpec.Spec.Ident
import ConcreteSpec.Spec.Own
import ConcreteSpec.Spec.Slice0Rules
import ConcreteSpec.Spec.Slice1Checker

/-!
# Diagnostic rule registry

A diagnostic cites the Lean declaration that states its rule and that rule's own revision
(`RuleId`, defined in `Ident`). This is the complete registry declaration.
Later pieces register rules when their definitions and evidence land, importing their owning
modules here. An entry records identity, not approval or a second definition of the rule.

`scripts/spec/rules.sh` reads entries and documentation from the built Lean environment. It
requires a declared decision-row citation and separate elaborated accepted and refused proofs
for each entry. Source-path and source-path span rules cite R-0709 and an existing decision
anchor. Example links use Lean declaration names, the identities used by the site.
-/

namespace ConcreteSpec.Core

/-- Acceptance evidence applies the owning rule's judgment to a concrete input.
The exporter recognizes this proposition's elaborated type, without unfolding the wrapper. -/
def RuleAccepted {Input : Sort _} (rule : Input → Prop) (input : Input) : Prop :=
  rule input

/-- Refusal evidence proves the negation of the same judgment on a concrete input. -/
def RuleRefused {Input : Sort _} (rule : Input → Prop) (input : Input) : Prop :=
  ¬ rule input

/-- A named conformance theorem linked to its owning rule. Its proposition supplies the verdict.
An inspectable example reduces, with default transparency, to closed constructor/literal data.
Traverse every argument of every constructor application, including parameters and dictionaries,
without slicing: skip proofs (terms whose type is a proposition); recursively check every other
argument. Data terms must reduce to constructor/literal trees. Type terms in parameters or indices
must reduce to closed sort/inductive type expressions, with every argument checked by this same
rule. Runtime fields whose type is a sort, including Prop and Type fields, are refused outright.
Free variables, metavariables, stuck terms, unreduced families and exhausted bounds are refused.
Top-level proof, proposition and type inputs are refused. The kernel and trust gates own proofs;
proof bodies do not identify the example. -/
structure RuleExample where
  rule : Lean.Name
  declaration : Lean.Name

/-- Refusal categories that can cite a slice-0 rule. -/
inductive RuleRefusalCategory where
  | malformed | unsupported | checker | resource | internal
  deriving DecidableEq

/-- Slice 0's required rule identities and the categories that may cite them.
Proof links live in the evidence inventory. -/
def slice0RuleEntries : List (RuleId × RuleRefusalCategory) :=
  [⟨⟨``Slice0Rules.Lexed, 1⟩, .malformed⟩,
   ⟨⟨``Slice0Rules.Predictive, 1⟩, .internal⟩,
   ⟨⟨``Slice0Rules.Parsed, 1⟩, .malformed⟩,
   ⟨⟨``Slice0Rules.EntryName, 1⟩, .unsupported⟩,
   ⟨⟨``Slice0Rules.Returns, 1⟩, .checker⟩,
   ⟨⟨``Slice0Rules.ReachableBody, 1⟩, .checker⟩,
   ⟨⟨``Slice0Rules.ExpectedTypeLiteral, 1⟩, .checker⟩,
   ⟨⟨``Slice0Rules.LocalBound, 1⟩, .checker⟩,
   ⟨⟨``Slice0Rules.FreshLocal, 1⟩, .checker⟩,
   ⟨⟨``Slice0Rules.UnreservedLocal, 1⟩, .checker⟩,
   ⟨⟨``Slice0Rules.NamedBinding, 1⟩, .unsupported⟩,
   ⟨⟨``Slice0Rules.DecimalLiteral, 1⟩, .unsupported⟩,
   ⟨⟨``Slice0Rules.AdmittedProduction, 1⟩, .unsupported⟩,
   ⟨⟨``Slice0Rules.LiteralTyped, 1⟩, .checker⟩,
   ⟨⟨``Slice0Rules.Admitted, 1⟩, .checker⟩,
   ⟨⟨``Slice0Rules.LocalsUsed, 1⟩, .checker⟩,
   ⟨⟨``Slice0Rules.AdditionFits, 1⟩, .checker⟩,
   ⟨⟨``Slice0Rules.WithinLimit, 1⟩, .resource⟩]

def slice0Rules : List RuleId := slice0RuleEntries.map Prod.fst

/-- The refusal category of an exact slice-0 rule reference. -/
def RuleId.refusalCategory (rule : RuleId) : Option RuleRefusalCategory :=
  (slice0RuleEntries.find? fun entry => entry.1 == rule).map Prod.snd

/-- Diagnostic rule identities, in specification order. Each judgment delegates to its owning
definition; registration adds no language policy. -/
def registeredRules : List RuleId :=
  [⟨``ConstsPermitted, 1⟩, ⟨``Own.OwnerPermitted, 1⟩, ⟨``Own.LoanPermitted, 1⟩,
   ⟨``Own.EndPermitted, 1⟩, ⟨``Own.MovePermitted, 1⟩,
   ⟨``Own.DropPermitted, 1⟩, ⟨``Own.LeavePermitted, 1⟩,
   ⟨``Leak.Account.WF, 1⟩, ⟨``Leak.ResetConserved, 1⟩, ⟨``Leak.SignedNetSound, 1⟩,
   ⟨``Leak.ByteBoundSound, 1⟩, ⟨``Leak.ByteBoundSoundOptional, 1⟩, ⟨``Leak.ReportSound, 1⟩,
   ⟨``AbortReady, 1⟩, ⟨``ProgramPermitted, 1⟩,
   ⟨``Leak.FiniteActionPermitted, 1⟩, ⟨``Leak.FiniteSequencePermitted, 1⟩] ++ SourcePath.Reason.inOrder.map SourcePath.Reason.ruleId ++
  [SourcePath.Project.Reason.caseCollision, .fileDirectoryConflict, .duplicate].map
    SourcePath.Project.Reason.ruleId ++
  [⟨``SourcePathSpan.FilePresent, 2⟩, ⟨``SourcePathSpan.OrderedOffsets, 2⟩,
   ⟨``SourcePathSpan.EndInSource, 2⟩, ⟨``SourcePathSpan.LineMatches, 2⟩,
   ⟨``SourcePathSpan.ColumnMatches, 2⟩] ++ slice0Rules

/-- A rule reference whose exact name and revision occur in the registry. -/
abbrev RegisteredRule := { rule : RuleId // rule ∈ registeredRules }

/-- Check an incoming reference, including its revision. -/
def validateRule (rule : RuleId) : Option RegisteredRule :=
  if h : rule ∈ registeredRules then some ⟨rule, h⟩ else none

/-- Generate a reference for the current revision of a declaration. -/
def referenceRule (name : Lean.Name) : Option RegisteredRule :=
  (registeredRules.find? (fun rule => rule.name == name)).bind validateRule

/-- Rule references required for the slice's conformance coverage, in registry order.
An unregistered slice has no coverage claim. -/
def rulesForSlice (slice : SliceId) : Option (List RegisteredRule) :=
  if slice.n.val = 0 then
    some (slice0Rules.attach.map fun entry =>
      ⟨entry.val, List.mem_append_right _ entry.property⟩)
  else none

/-- Missing conformance coverage in registry order. Coverage is revision-specific. -/
def missingRules (slice : SliceId) (exercised : List RegisteredRule) :
    Option (List RegisteredRule) :=
  (rulesForSlice slice).map fun rules =>
    rules.filter fun rule => !(exercised.any fun hit => hit.val == rule.val)

end ConcreteSpec.Core
