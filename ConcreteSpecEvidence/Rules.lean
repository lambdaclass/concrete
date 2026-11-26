import ConcreteSpec.Spec.Basics
import ConcreteSpec.Spec.Hybrid
import ConcreteSpec.Spec.Scalar
import ConcreteSpec.Spec.Slice1Ownership
import ConcreteSpec.Spec.Slice1Syntax
import ConcreteSpecEvidence.ConstExamples
import ConcreteSpecEvidence.OwnExamples
import ConcreteSpec.Spec.Accounting
import ConcreteSpec.Spec.Const
import ConcreteSpec.Spec.Ident
import ConcreteSpec.Spec.Own
import ConcreteSpec.Spec.Slice1Checker
import ConcreteSpec.Spec.Rules

namespace ConcreteSpec.Core

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
  have length : pathCapRefusedInput.bytes.length = 201 := by
    simp only [pathCapRefusedInput, List.length_append, List.length_replicate,
      List.length_cons, List.length_nil]
  unfold RuleRefused PathByteCap Permitted
  simp only [Reference.offsets, length]
  decide

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

namespace SourcePath.Project.Registry

def fileA : SourcePath := ⟨SourceText.ofBytes [97], by decide⟩
def fileB : SourcePath := ⟨SourceText.ofBytes [98], by decide⟩
def upperDirectory : SourcePath := ⟨SourceText.ofBytes [65, 47, 99, 46, 99, 111, 110], by decide⟩
def lowerDirectory : SourcePath := ⟨SourceText.ofBytes [97, 47, 66, 46, 99, 111, 110], by decide⟩

def separateFiles : SourcePath × SourcePath := (fileA, fileB)
def directoryCollision : SourcePath × SourcePath := (lowerDirectory, upperDirectory)
def fileDirectoryConflict : SourcePath × SourcePath := (fileA, lowerDirectory)

/-- Instance: two different root files have no case collision. -/
theorem caseAccepted : RuleAccepted NoCaseCollision separateFiles := by
  intro ⟨n, leftBound, _, different, folded⟩
  change n ≤ 1 at leftBound
  have lengthCases : n = 0 ∨ n = 1 := by omega
  rcases lengthCases with rfl | rfl
  · exact different rfl
  · exact (by decide : [[97]] ≠ [[98]]) folded

/-- Instance: a/B.con and A/c.con conflict at their directory prefixes. -/
theorem caseRefused : RuleRefused NoCaseCollision directoryCollision := by
  intro permitted
  exact permitted ⟨1, by decide, by decide, by decide, by decide⟩

/-- Instance: two different root files have no file/directory conflict. -/
theorem fileDirectoryAccepted : RuleAccepted NoFileDirectoryConflict separateFiles := by
  intro ⟨_, eitherPrefix⟩
  rcases eitherPrefix with isPrefix | isPrefix
  · obtain ⟨suffix, eq⟩ := isPrefix
    change [[97]] ++ suffix = [[98]] at eq
    simp at eq
  · obtain ⟨suffix, eq⟩ := isPrefix
    change [[98]] ++ suffix = [[97]] at eq
    simp at eq

/-- Instance: a cannot be both a file and the directory containing a/B.con. -/
theorem fileDirectoryRefused : RuleRefused NoFileDirectoryConflict fileDirectoryConflict := by
  intro permitted
  exact permitted ⟨by decide, Or.inl ⟨[[66, 46, 99, 111, 110]], rfl⟩⟩

def duplicateFile : SourcePath × SourcePath := (fileA, fileA)

/-- Instance: two different root files are distinct. -/
theorem duplicateAccepted : RuleAccepted NoDuplicate separateFiles := by
  change fileA.raw ≠ fileB.raw
  decide

/-- Instance: two occurrences of the same file are refused. -/
theorem duplicateRefused : RuleRefused NoDuplicate duplicateFile := by
  intro permitted
  exact permitted rfl

end SourcePath.Project.Registry

namespace SourcePathSpan.Registry

/-- The admitted path `other.con` is absent from the singleton tab project. -/
def missingPath : SourcePath :=
  ⟨SourceText.ofBytes [111, 116, 104, 101, 114, 46, 99, 111, 110], by decide⟩

/-- Instance: The tab file is present at its exact path. -/
theorem filePresentAccepted : RuleAccepted FilePresent
    (tabProject, mainSourcePath) := by
  unfold RuleAccepted FilePresent
  decide

/-- Instance: The requested path is absent even though the admitted project contains a file. -/
theorem filePresentRefused : RuleRefused FilePresent
    (tabProject, missingPath) := by
  unfold RuleRefused FilePresent
  decide

/-- Instance: Offsets in ascending order describe a half-open range. -/
theorem offsetsAccepted : RuleAccepted OrderedOffsets
    (2, 3) := by
  unfold RuleAccepted OrderedOffsets
  decide

/-- Instance: Reversed offsets do not describe a range. -/
theorem offsetsRefused : RuleRefused OrderedOffsets
    (3, 2) := by
  unfold RuleRefused OrderedOffsets
  decide

/-- Instance: The end of the tab span is at EOF. -/
theorem endAccepted : RuleAccepted EndInSource
    (tabSource, 3) := by
  unfold RuleAccepted EndInSource
  decide

/-- Instance: The next offset is outside the tab file. -/
theorem endRefused : RuleRefused EndInSource
    (tabSource, 4) := by
  unfold RuleRefused EndInSource
  decide

/-- Instance: The tab span starts on line one. -/
theorem lineAccepted : RuleAccepted LineMatches
    (tabSpan, tabSource) := by
  unfold RuleAccepted LineMatches
  decide

/-- Instance: Line two cannot name the start of the tab span. -/
theorem lineRefused : RuleRefused LineMatches
    ({ tabSpan with line := 2, line_pos := by decide }, tabSource) := by
  unfold RuleRefused LineMatches
  decide

/-- Instance: The column counts the preceding tab as one byte. -/
theorem columnAccepted : RuleAccepted ColumnMatches
    (tabSpan, tabSource) := by
  unfold RuleAccepted ColumnMatches
  decide

/-- Instance: Column two fails to count the preceding tab. -/
theorem columnRefused : RuleRefused ColumnMatches
    ({ tabSpan with col := 2, col_pos := by decide }, tabSource) := by
  unfold RuleRefused ColumnMatches
  decide

end SourcePathSpan.Registry

namespace Slice1Registry

private def sName : Name := ⟨"S"⟩
private def sTy : Ty := .struct sName
private def decls : Decls := [⟨sName, false, []⟩]
private def bindS : Stmt := .let_ (.name ⟨0⟩) sTy (.mk sName [])
private def takeS : Stmt := .letStruct sName [] (.atom (.move ⟨0⟩))
private def ret : Terminator := .ret (.int .u8 0)

private def live (used : Bool) : Ctx :=
  [(⟨0⟩, { ty := sTy, live := true, used, known := none })]

theorem usedAbort : RuleAccepted AbortReady (live true) := by
  unfold RuleAccepted AbortReady
  decide

theorem unusedAbort : RuleRefused AbortReady (live false) := by
  unfold RuleRefused AbortReady
  decide

theorem consumeThenAbort : RuleAccepted ProgramPermitted ⟨decls, [bindS, takeS], .abort ⟨0⟩⟩ := by
  unfold RuleAccepted ProgramPermitted
  decide

theorem consumeThenReturn : RuleAccepted ProgramPermitted ⟨decls, [bindS, takeS], ret⟩ := by
  unfold RuleAccepted ProgramPermitted
  decide

theorem liveLinearAtReturn : RuleRefused ProgramPermitted ⟨decls, [bindS], ret⟩ := by
  unfold RuleRefused ProgramPermitted
  decide

theorem useAfterMove : RuleRefused ProgramPermitted ⟨decls, [bindS, takeS, takeS], ret⟩ := by
  unfold RuleRefused ProgramPermitted
  decide

theorem overflow : RuleRefused ProgramPermitted
    ⟨[], [.let_ .wild (.scalar (.int .u8)) (.add (.int .u8 200) (.int .u8 56))], ret⟩ := by
  unfold RuleRefused ProgramPermitted
  decide

end Slice1Registry

/-- Complete conformance links, populated alongside the owning rules. -/
def registeredRuleExamples : List RuleExample :=
  [⟨``ConstsPermitted, ``ConstRegistry.accepted⟩,
   ⟨``ConstsPermitted, ``ConstRegistry.refused⟩,
   ⟨``AbortReady, ``Slice1Registry.usedAbort⟩,
   ⟨``AbortReady, ``Slice1Registry.unusedAbort⟩,
   ⟨``ProgramPermitted, ``Slice1Registry.consumeThenAbort⟩,
   ⟨``ProgramPermitted, ``Slice1Registry.consumeThenReturn⟩,
   ⟨``ProgramPermitted, ``Slice1Registry.liveLinearAtReturn⟩,
   ⟨``ProgramPermitted, ``Slice1Registry.useAfterMove⟩,
   ⟨``ProgramPermitted, ``Slice1Registry.overflow⟩,
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
   ⟨``SourcePath.FilenameCharacters, ``SourcePath.Registry.disallowedRefused⟩,
   ⟨``SourcePath.Project.NoCaseCollision, ``SourcePath.Project.Registry.caseAccepted⟩,
   ⟨``SourcePath.Project.NoCaseCollision, ``SourcePath.Project.Registry.caseRefused⟩,
   ⟨``SourcePath.Project.NoFileDirectoryConflict, ``SourcePath.Project.Registry.fileDirectoryAccepted⟩,
   ⟨``SourcePath.Project.NoFileDirectoryConflict, ``SourcePath.Project.Registry.fileDirectoryRefused⟩,
   ⟨``SourcePath.Project.NoDuplicate, ``SourcePath.Project.Registry.duplicateAccepted⟩,
   ⟨``SourcePath.Project.NoDuplicate, ``SourcePath.Project.Registry.duplicateRefused⟩,
   ⟨``SourcePathSpan.FilePresent, ``SourcePathSpan.Registry.filePresentAccepted⟩,
   ⟨``SourcePathSpan.FilePresent, ``SourcePathSpan.Registry.filePresentRefused⟩,
   ⟨``SourcePathSpan.OrderedOffsets, ``SourcePathSpan.Registry.offsetsAccepted⟩,
   ⟨``SourcePathSpan.OrderedOffsets, ``SourcePathSpan.Registry.offsetsRefused⟩,
   ⟨``SourcePathSpan.EndInSource, ``SourcePathSpan.Registry.endAccepted⟩,
   ⟨``SourcePathSpan.EndInSource, ``SourcePathSpan.Registry.endRefused⟩,
   ⟨``SourcePathSpan.LineMatches, ``SourcePathSpan.Registry.lineAccepted⟩,
   ⟨``SourcePathSpan.LineMatches, ``SourcePathSpan.Registry.lineRefused⟩,
   ⟨``SourcePathSpan.ColumnMatches, ``SourcePathSpan.Registry.columnAccepted⟩,
   ⟨``SourcePathSpan.ColumnMatches, ``SourcePathSpan.Registry.columnRefused⟩]

end ConcreteSpec.Core
