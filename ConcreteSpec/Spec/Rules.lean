import ConcreteSpec.Spec.Accounting
import ConcreteSpec.Spec.Const
import ConcreteSpec.Spec.Ident
import ConcreteSpec.Spec.Own
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
  [⟨``SourcePathSpan.FilePresent, 1⟩, ⟨``SourcePathSpan.OrderedOffsets, 1⟩,
   ⟨``SourcePathSpan.EndInSource, 1⟩, ⟨``SourcePathSpan.LineMatches, 1⟩,
   ⟨``SourcePathSpan.ColumnMatches, 1⟩]

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


end ConcreteSpec.Core
