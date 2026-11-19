import ConcreteSpec.Spec.Accounting
import ConcreteSpec.Spec.Const
import ConcreteSpec.Spec.Ident
import ConcreteSpec.Spec.Own

/-!
# Diagnostic rule registry

A diagnostic cites the Lean declaration that states its rule and that rule's own revision
(`RuleId`, defined in `Ident`). This is the complete registry declaration.
Later pieces register rules when their definitions and evidence land, importing their owning
modules here. An entry records identity, not approval or a second definition of the rule.

`scripts/spec/rules.sh` reads entries and documentation from the built Lean environment. It
requires a declared decision-row citation and separate elaborated accepted and refused proofs
for each entry. Example links use Lean declaration names, the identities used by the site.
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
   ⟨``Leak.FiniteActionPermitted, 1⟩, ⟨``Leak.FiniteSequencePermitted, 1⟩] ++ SourcePath.Reason.inOrder.map SourcePath.Reason.ruleId

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

namespace ConstRegistry

/-- Instance: a populated dependency environment supports arithmetic in a later constant. -/
theorem accepted : RuleAccepted ConstsPermitted
    (3, [⟨ConstDemo.cid "BASE", .int .u8, .int .u8 2⟩,
         ⟨ConstDemo.cid "NEXT", .int .u8,
           .plain .add (.ref (ConstDemo.cid "BASE")) (.int .u8 1)⟩]) := by
  unfold RuleAccepted ConstsPermitted
  decide

/-- Instance: the same dependency setup refuses overflow in an unused initializer. -/
theorem refused : RuleRefused ConstsPermitted
    (3, [⟨ConstDemo.cid "BASE", .int .u8, .int .u8 255⟩,
         ⟨ConstDemo.cid "NEXT", .int .u8,
           .plain .add (.ref (ConstDemo.cid "BASE")) (.int .u8 1)⟩]) := by
  unfold RuleRefused ConstsPermitted
  decide

end ConstRegistry

namespace Own.Registry

/-- Instance: shared loans permit owner reads. -/
theorem ownerAccepted : RuleAccepted OwnerPermitted (Examples.shared, ⟨0⟩, .shared) := by
  unfold RuleAccepted OwnerPermitted
  decide
/-- Instance: the very same populated ledger refuses owner writes. -/
theorem ownerRefused : RuleRefused OwnerPermitted (Examples.shared, ⟨0⟩, .mutable) := by
  unfold RuleRefused OwnerPermitted
  decide
/-- Instance: the mutable child can access its root. -/
theorem loanAccepted : RuleAccepted LoanPermitted (Examples.child, ⟨1⟩, .mutable) := by
  unfold RuleAccepted LoanPermitted
  decide
/-- Instance: that child suspends even reads through the parent. -/
theorem loanRefused : RuleRefused LoanPermitted (Examples.child, ⟨0⟩, .shared) := by
  unfold RuleRefused LoanPermitted
  decide
/-- Instance: the child may end. -/
theorem endAccepted : RuleAccepted EndPermitted (Examples.child, ⟨1⟩) := by
  unfold RuleAccepted EndPermitted
  decide
/-- Instance: its still-live parent cannot end first. -/
theorem endRefused : RuleRefused EndPermitted (Examples.child, ⟨0⟩) := by
  unfold RuleRefused EndPermitted
  decide
/-- Instance: the initial live owner may move. -/
theorem moveAccepted : RuleAccepted MovePermitted (Examples.initial, ⟨0⟩) := by
  unfold RuleAccepted MovePermitted
  decide
/-- Instance: a loan of that same owner blocks moving. -/
theorem moveRefused : RuleRefused MovePermitted (Examples.shared, ⟨0⟩) := by
  unfold RuleRefused MovePermitted
  decide
/-- Instance: a consumed owner's storage can end. -/
theorem dropAccepted : RuleAccepted DropPermitted
    ([Examples.x], (move Examples.initial ⟨0⟩).getD Examples.initial) := by
  unfold RuleAccepted DropPermitted
  decide
/-- Instance: the identical live linear owner cannot be discarded. -/
theorem dropRefused : RuleRefused DropPermitted ([Examples.x], Examples.initial) := by
  unfold RuleRefused DropPermitted
  decide
/-- Instance: function exit succeeds after explicit consumption. -/
theorem leaveAccepted : RuleAccepted LeavePermitted
    ((move Examples.initial ⟨0⟩).getD Examples.initial) := by
  unfold RuleAccepted LeavePermitted
  decide
/-- Instance: function exit refuses an undischarged obligation. -/
theorem leaveRefused : RuleRefused LeavePermitted Examples.initial := by
  unfold RuleRefused LeavePermitted
  decide

end Own.Registry

namespace Leak.Registry

/-- Closed populated accounting input for registry conformance. -/
def accountA : Account := ⟨100, 5, 12, 9⟩
/-- Closed memory with two independently populated allocator lineages. -/
def twoLineages : List (LineageId × Account) :=
  [(⟨0⟩, accountA), (⟨1⟩, ⟨7, 2, 2, 2⟩)]
/-- Closed function-entry account. -/
def reportEntry : Account := ⟨100, 2, 100, 2⟩
/-- Closed function-exit account. -/
def reportExit : Account := ⟨108, 7, 100, 10⟩
/-- Independently stated bounds for the closed report example. -/
def establishedReport : LineageReport :=
  ⟨⟨0⟩, ⟨8, .boundEstablished⟩, ⟨5, 5, .boundEstablished⟩,
    ⟨8, .boundEstablished⟩, ⟨12, .boundEstablished⟩⟩

/-- Instance: populated history bounds live bytes. -/
theorem wfAccepted : RuleAccepted Account.WF accountA := by
  simp only [RuleAccepted, Account.WF]
  decide
/-- Instance: a local peak below live bytes is refused. -/
theorem wfRefused : RuleRefused Account.WF { accountA with functionPeak := 4 } := by
  simp only [RuleRefused, Account.WF]
  decide
/-- Instance: reset records its actual outflow. -/
theorem resetAccepted : RuleAccepted ResetConserved (accountA, accountA.reset, 5) := by
  simp only [RuleAccepted, ResetConserved, ResetConservation]
  decide
/-- Instance: an incorrect outflow cannot certify the same reset. -/
theorem resetRefused : RuleRefused ResetConserved (accountA, accountA.reset, 4) := by
  simp only [RuleRefused, ResetConserved, ResetConservation]
  decide
/-- Instance: an established bound covers the actual bytes. -/
theorem boundAccepted : RuleAccepted ByteBoundSound (10, ⟨12, .boundEstablished⟩) := by
  simp [RuleAccepted, ByteBoundSound, Bound.Sound]
/-- Instance: an established underestimate is refused. -/
theorem boundRefused : RuleRefused ByteBoundSound (10, ⟨9, .boundEstablished⟩) := by
  simp [RuleRefused, ByteBoundSound, Bound.Sound]
/-- Instance: a defined allocator footprint supports evidence. -/
theorem optionalAccepted : RuleAccepted ByteBoundSoundOptional (some 10, ⟨12, .boundEstablished⟩) := by
  simp [RuleAccepted, ByteBoundSoundOptional, Bound.SoundOptional]
/-- Instance: an absent footprint cannot be established. -/
theorem optionalRefused : RuleRefused ByteBoundSoundOptional (none, ⟨12, .boundEstablished⟩) := by
  simp [RuleRefused, ByteBoundSoundOptional, Bound.SoundOptional]
/-- Instance: a report distinguishes function-local peak 8 from historical peak 100. -/
theorem reportAccepted : RuleAccepted ReportSound (⟨0⟩, reportEntry, reportExit, some 10, establishedReport) := by
  simp [RuleAccepted, ReportSound, LineageReport.Sound, Bound.Sound, SignedInterval.Sound,
    Bound.SoundOptional, establishedReport, reportEntry, reportExit]
/-- Instance: correct numbers cannot be attributed to another lineage. -/
theorem reportRefused : RuleRefused ReportSound (⟨1⟩, reportEntry, reportExit, some 10, establishedReport) := by
  simp [RuleRefused, ReportSound, LineageReport.Sound, establishedReport]

/-- Instance: a populated lineage admits an allocation without affecting another lineage. -/
theorem actionAccepted : RuleAccepted FiniteActionPermitted (twoLineages, .alloc ⟨0⟩ 8) := rfl
/-- Instance: the same lineage refuses an excessive free. -/
theorem actionRefused : RuleRefused FiniteActionPermitted (twoLineages, .free ⟨0⟩ 6) := by
  simp [RuleRefused, FiniteActionPermitted, accountMemory, twoLineages, accountA, ActionPermitted, apply, Memory.set]
/-- Instance: allocation, free and reset compose through the production fold. -/
theorem sequenceAccepted :
    RuleAccepted FiniteSequencePermitted
      (twoLineages, [.alloc ⟨0⟩ 8, .free ⟨0⟩ 3, .reset ⟨0⟩]) := rfl
/-- Instance: a bad middle free refuses the whole sequence even though its first action succeeds. -/
theorem sequenceRefused : RuleRefused FiniteSequencePermitted (twoLineages,
    [.alloc ⟨0⟩ 8, .free ⟨0⟩ 14, .reset ⟨0⟩]) := by
  change ¬ (false = true)
  decide

/-- Instance: a negative net change lies inside an established signed interval. -/
theorem signedAccepted : RuleAccepted SignedNetSound (-3, ⟨-4, -2, .boundEstablished⟩) := by
  simp [RuleAccepted, SignedNetSound, SignedInterval.Sound]
/-- Instance: changing only the lower endpoint excludes the same negative net change. -/
theorem signedRefused : RuleRefused SignedNetSound (-3, ⟨-2, -2, .boundEstablished⟩) := by
  simp [RuleRefused, SignedNetSound, SignedInterval.Sound]


end Leak.Registry

namespace SourcePath.Registry

/-- Mixed-case directories and internal repeated dots survive local admission. -/
def mixedCaseInput : SourceText := SourceText.ofBytes [83, 114, 99, 47, 65, 46, 46, 98]

/-- Instance: the accepted byte input for the Nonempty check. -/
theorem emptyAccepted : RuleAccepted Nonempty mixedCaseInput := by
  unfold RuleAccepted Nonempty
  decide

def emptyRefusedInput : SourceText := SourceText.ofBytes []
/-- Instance: the refused byte input for the Nonempty check. -/
theorem emptyRefused : RuleRefused Nonempty emptyRefusedInput := by
  unfold RuleRefused Nonempty
  decide

/-- Instance: the accepted byte input for the Relative check. -/
theorem absoluteAccepted : RuleAccepted Relative mixedCaseInput := by
  unfold RuleAccepted Relative
  decide

def absoluteRefusedInput : SourceText := SourceText.ofBytes [47, 65]
/-- Instance: the refused byte input for the Relative check. -/
theorem absoluteRefused : RuleRefused Relative absoluteRefusedInput := by
  unfold RuleRefused Relative
  decide

/-- Instance: the accepted byte input for the NoDrivePrefix check. -/
theorem driveAccepted : RuleAccepted NoDrivePrefix mixedCaseInput := by
  unfold RuleAccepted NoDrivePrefix
  decide

def driveRefusedInput : SourceText := SourceText.ofBytes [67, 58]
/-- Instance: the refused byte input for the NoDrivePrefix check. -/
theorem driveRefused : RuleRefused NoDrivePrefix driveRefusedInput := by
  unfold RuleRefused NoDrivePrefix
  decide

/-- Instance: the accepted byte input for the NoTraversal check. -/
theorem traversalAccepted : RuleAccepted NoTraversal mixedCaseInput := by
  unfold RuleAccepted NoTraversal
  decide

def traversalRefusedInput : SourceText := SourceText.ofBytes [65, 47, 46, 46]
/-- Instance: the refused byte input for the NoTraversal check. -/
theorem traversalRefused : RuleRefused NoTraversal traversalRefusedInput := by
  unfold RuleRefused NoTraversal
  decide

/-- Instance: the accepted byte input for the NoSingleDot check. -/
theorem singleDotAccepted : RuleAccepted NoSingleDot mixedCaseInput := by
  unfold RuleAccepted NoSingleDot
  decide

def singleDotRefusedInput : SourceText := SourceText.ofBytes [65, 47, 46]
/-- Instance: the refused byte input for the NoSingleDot check. -/
theorem singleDotRefused : RuleRefused NoSingleDot singleDotRefusedInput := by
  unfold RuleRefused NoSingleDot
  decide

/-- Instance: the accepted byte input for the NonemptyComponents check. -/
theorem emptyComponentAccepted : RuleAccepted NonemptyComponents mixedCaseInput := by
  unfold RuleAccepted NonemptyComponents
  decide

def emptyComponentRefusedInput : SourceText := SourceText.ofBytes [65, 47]
/-- Instance: the refused byte input for the NonemptyComponents check. -/
theorem emptyComponentRefused : RuleRefused NonemptyComponents emptyComponentRefusedInput := by
  unfold RuleRefused NonemptyComponents
  decide

/-- Instance: the accepted byte input for the Ascii check. -/
theorem nonAsciiAccepted : RuleAccepted Ascii mixedCaseInput := by
  unfold RuleAccepted Ascii
  decide

def nonAsciiRefusedInput : SourceText := SourceText.ofBytes [65, 128]
/-- Instance: the refused byte input for the Ascii check. -/
theorem nonAsciiRefused : RuleRefused Ascii nonAsciiRefusedInput := by
  unfold RuleRefused Ascii
  decide

/-- Instance: the accepted byte input for the SlashSeparator check. -/
theorem backslashAccepted : RuleAccepted SlashSeparator mixedCaseInput := by
  unfold RuleAccepted SlashSeparator
  decide

def backslashRefusedInput : SourceText := SourceText.ofBytes [65, 92, 66]
/-- Instance: the refused byte input for the SlashSeparator check. -/
theorem backslashRefused : RuleRefused SlashSeparator backslashRefusedInput := by
  unfold RuleRefused SlashSeparator
  decide

/-- Instance: the accepted byte input for the NoControls check. -/
theorem controlAccepted : RuleAccepted NoControls mixedCaseInput := by
  unfold RuleAccepted NoControls
  decide

def controlRefusedInput : SourceText := SourceText.ofBytes [65, 127]
/-- Instance: the refused byte input for the NoControls check. -/
theorem controlRefused : RuleRefused NoControls controlRefusedInput := by
  unfold RuleRefused NoControls
  decide

/-- Instance: the accepted byte input for the NoSpaces check. -/
theorem spaceAccepted : RuleAccepted NoSpaces mixedCaseInput := by
  unfold RuleAccepted NoSpaces
  decide

def spaceRefusedInput : SourceText := SourceText.ofBytes [65, 32]
/-- Instance: the refused byte input for the NoSpaces check. -/
theorem spaceRefused : RuleRefused NoSpaces spaceRefusedInput := by
  unfold RuleRefused NoSpaces
  decide

/-- Instance: the accepted byte input for the NoLeadingDot check. -/
theorem leadingDotAccepted : RuleAccepted NoLeadingDot mixedCaseInput := by
  unfold RuleAccepted NoLeadingDot
  decide

def leadingDotRefusedInput : SourceText := SourceText.ofBytes [46, 65]
/-- Instance: the refused byte input for the NoLeadingDot check. -/
theorem leadingDotRefused : RuleRefused NoLeadingDot leadingDotRefusedInput := by
  unfold RuleRefused NoLeadingDot
  decide

/-- Instance: the accepted byte input for the NoTrailingDot check. -/
theorem trailingDotAccepted : RuleAccepted NoTrailingDot mixedCaseInput := by
  unfold RuleAccepted NoTrailingDot
  decide

def trailingDotRefusedInput : SourceText := SourceText.ofBytes [65, 46]
/-- Instance: the refused byte input for the NoTrailingDot check. -/
theorem trailingDotRefused : RuleRefused NoTrailingDot trailingDotRefusedInput := by
  unfold RuleRefused NoTrailingDot
  decide

def reservedAcceptedInput : SourceText := SourceText.ofBytes [67, 79, 77, 48, 46, 67]
/-- Instance: the accepted byte input for the NoReservedStem check. -/
theorem reservedAccepted : RuleAccepted NoReservedStem reservedAcceptedInput := by
  unfold RuleAccepted NoReservedStem
  decide

def reservedRefusedInput : SourceText := SourceText.ofBytes [99, 79, 78, 46, 67]
/-- Instance: the refused byte input for the NoReservedStem check. -/
theorem reservedRefused : RuleRefused NoReservedStem reservedRefusedInput := by
  unfold RuleRefused NoReservedStem
  decide

def componentCapAcceptedInput : SourceText := SourceText.ofBytes (List.replicate 100 65)
/-- Instance: the accepted byte input for the ComponentByteCap check. -/
theorem componentCapAccepted : RuleAccepted ComponentByteCap componentCapAcceptedInput := by
  unfold RuleAccepted ComponentByteCap
  decide

def componentCapRefusedInput : SourceText := SourceText.ofBytes (List.replicate 101 65)
/-- Instance: the refused byte input for the ComponentByteCap check. -/
theorem componentCapRefused : RuleRefused ComponentByteCap componentCapRefusedInput := by
  unfold RuleRefused ComponentByteCap
  decide

def pathCapAcceptedInput : SourceText :=
  ⟨List.replicate 99 65 ++ [47] ++ List.replicate 100 66, by
    intro byte member
    simp only [List.mem_append, List.mem_replicate, List.mem_cons, List.not_mem_nil, or_false] at member
    omega⟩
/-- Instance: the accepted byte input for the PathByteCap check. -/
theorem pathCapAccepted : RuleAccepted PathByteCap pathCapAcceptedInput := by
  unfold RuleAccepted PathByteCap
  apply of_decide_eq_true
  cbv

def pathCapRefusedInput : SourceText :=
  ⟨List.replicate 98 65 ++ [47] ++ List.replicate 100 66 ++ [47, 67], by
    intro byte member
    simp only [List.mem_append, List.mem_replicate, List.mem_cons, List.not_mem_nil, or_false] at member
    omega⟩
/-- Instance: the refused byte input for the PathByteCap check. -/
theorem pathCapRefused : RuleRefused PathByteCap pathCapRefusedInput := by
  unfold RuleRefused PathByteCap
  apply of_decide_eq_true
  cbv

/-- Instance: the accepted byte input for the FilenameCharacters check. -/
theorem disallowedAccepted : RuleAccepted FilenameCharacters mixedCaseInput := by
  unfold RuleAccepted FilenameCharacters
  decide

def disallowedRefusedInput : SourceText := SourceText.ofBytes [65, 47, 66, 58, 67]
/-- Instance: the refused byte input for the FilenameCharacters check. -/
theorem disallowedRefused : RuleRefused FilenameCharacters disallowedRefusedInput := by
  unfold RuleRefused FilenameCharacters
  decide

end SourcePath.Registry

/-- Complete conformance links, populated alongside the owning rules. -/
def registeredRuleExamples : List RuleExample :=
  [⟨``ConstsPermitted, ``ConstRegistry.accepted⟩,
   ⟨``ConstsPermitted, ``ConstRegistry.refused⟩,
   ⟨``Own.OwnerPermitted, ``Own.Registry.ownerAccepted⟩,
   ⟨``Own.OwnerPermitted, ``Own.Registry.ownerRefused⟩,
   ⟨``Own.LoanPermitted, ``Own.Registry.loanAccepted⟩,
   ⟨``Own.LoanPermitted, ``Own.Registry.loanRefused⟩,
   ⟨``Own.EndPermitted, ``Own.Registry.endAccepted⟩,
   ⟨``Own.EndPermitted, ``Own.Registry.endRefused⟩,
   ⟨``Own.MovePermitted, ``Own.Registry.moveAccepted⟩,
   ⟨``Own.MovePermitted, ``Own.Registry.moveRefused⟩,
   ⟨``Own.DropPermitted, ``Own.Registry.dropAccepted⟩,
   ⟨``Own.DropPermitted, ``Own.Registry.dropRefused⟩,
   ⟨``Own.LeavePermitted, ``Own.Registry.leaveAccepted⟩,
   ⟨``Own.LeavePermitted, ``Own.Registry.leaveRefused⟩,
   ⟨``Leak.Account.WF, ``Leak.Registry.wfAccepted⟩,
   ⟨``Leak.Account.WF, ``Leak.Registry.wfRefused⟩,
   ⟨``Leak.ResetConserved, ``Leak.Registry.resetAccepted⟩,
   ⟨``Leak.ResetConserved, ``Leak.Registry.resetRefused⟩,
   ⟨``Leak.SignedNetSound, ``Leak.Registry.signedAccepted⟩,
   ⟨``Leak.SignedNetSound, ``Leak.Registry.signedRefused⟩,
   ⟨``Leak.ByteBoundSound, ``Leak.Registry.boundAccepted⟩,
   ⟨``Leak.ByteBoundSound, ``Leak.Registry.boundRefused⟩,
   ⟨``Leak.ByteBoundSoundOptional, ``Leak.Registry.optionalAccepted⟩,
   ⟨``Leak.ByteBoundSoundOptional, ``Leak.Registry.optionalRefused⟩,
   ⟨``Leak.ReportSound, ``Leak.Registry.reportAccepted⟩,
   ⟨``Leak.ReportSound, ``Leak.Registry.reportRefused⟩,
   ⟨``Leak.FiniteActionPermitted, ``Leak.Registry.actionAccepted⟩,
   ⟨``Leak.FiniteActionPermitted, ``Leak.Registry.actionRefused⟩,
   ⟨``Leak.FiniteSequencePermitted, ``Leak.Registry.sequenceAccepted⟩,
   ⟨``Leak.FiniteSequencePermitted, ``Leak.Registry.sequenceRefused⟩,
   ⟨``SourcePath.Nonempty, ``SourcePath.Registry.emptyAccepted⟩,
   ⟨``SourcePath.Nonempty, ``SourcePath.Registry.emptyRefused⟩,
   ⟨``SourcePath.Relative, ``SourcePath.Registry.absoluteAccepted⟩,
   ⟨``SourcePath.Relative, ``SourcePath.Registry.absoluteRefused⟩,
   ⟨``SourcePath.NoDrivePrefix, ``SourcePath.Registry.driveAccepted⟩,
   ⟨``SourcePath.NoDrivePrefix, ``SourcePath.Registry.driveRefused⟩,
   ⟨``SourcePath.NoTraversal, ``SourcePath.Registry.traversalAccepted⟩,
   ⟨``SourcePath.NoTraversal, ``SourcePath.Registry.traversalRefused⟩,
   ⟨``SourcePath.NoSingleDot, ``SourcePath.Registry.singleDotAccepted⟩,
   ⟨``SourcePath.NoSingleDot, ``SourcePath.Registry.singleDotRefused⟩,
   ⟨``SourcePath.NonemptyComponents, ``SourcePath.Registry.emptyComponentAccepted⟩,
   ⟨``SourcePath.NonemptyComponents, ``SourcePath.Registry.emptyComponentRefused⟩,
   ⟨``SourcePath.Ascii, ``SourcePath.Registry.nonAsciiAccepted⟩,
   ⟨``SourcePath.Ascii, ``SourcePath.Registry.nonAsciiRefused⟩,
   ⟨``SourcePath.SlashSeparator, ``SourcePath.Registry.backslashAccepted⟩,
   ⟨``SourcePath.SlashSeparator, ``SourcePath.Registry.backslashRefused⟩,
   ⟨``SourcePath.NoControls, ``SourcePath.Registry.controlAccepted⟩,
   ⟨``SourcePath.NoControls, ``SourcePath.Registry.controlRefused⟩,
   ⟨``SourcePath.NoSpaces, ``SourcePath.Registry.spaceAccepted⟩,
   ⟨``SourcePath.NoSpaces, ``SourcePath.Registry.spaceRefused⟩,
   ⟨``SourcePath.NoLeadingDot, ``SourcePath.Registry.leadingDotAccepted⟩,
   ⟨``SourcePath.NoLeadingDot, ``SourcePath.Registry.leadingDotRefused⟩,
   ⟨``SourcePath.NoTrailingDot, ``SourcePath.Registry.trailingDotAccepted⟩,
   ⟨``SourcePath.NoTrailingDot, ``SourcePath.Registry.trailingDotRefused⟩,
   ⟨``SourcePath.NoReservedStem, ``SourcePath.Registry.reservedAccepted⟩,
   ⟨``SourcePath.NoReservedStem, ``SourcePath.Registry.reservedRefused⟩,
   ⟨``SourcePath.ComponentByteCap, ``SourcePath.Registry.componentCapAccepted⟩,
   ⟨``SourcePath.ComponentByteCap, ``SourcePath.Registry.componentCapRefused⟩,
   ⟨``SourcePath.PathByteCap, ``SourcePath.Registry.pathCapAccepted⟩,
   ⟨``SourcePath.PathByteCap, ``SourcePath.Registry.pathCapRefused⟩,
   ⟨``SourcePath.FilenameCharacters, ``SourcePath.Registry.disallowedAccepted⟩,
   ⟨``SourcePath.FilenameCharacters, ``SourcePath.Registry.disallowedRefused⟩]

end ConcreteSpec.Core
