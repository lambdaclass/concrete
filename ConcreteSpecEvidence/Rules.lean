import ConcreteSpec.Spec.LL1
import ConcreteSpec.Spec.Outcome
import ConcreteSpec.Spec.Slice0
import ConcreteSpec.Spec.Slice0Syntax
import ConcreteSpec.Spec.Slice0Rules
import ConcreteSpec.Spec.Statics
import ConcreteSpec.Spec.Surface0
import ConcreteSpec.Spec.Manifest
import ConcreteSpec.Spec.ProjectPolicy
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

namespace Slice0Rules

open ConcreteSpec.Surface0

private def span : Surface0.Span := ⟨0, 1, 1, 1, ⟨1⟩⟩
private def returned : Statement := .ret (.integer 0 none span) (statementSpan span span)
private def parsedProgram : Surface0.Program := ⟨"main", span, [returned]⟩
private def parsedTokens : List Token :=
  [⟨.fixed .fn, span⟩, ⟨.name "main", span⟩, ⟨.fixed .lparen, span⟩,
   ⟨.fixed .rparen, span⟩, ⟨.fixed .arrow, span⟩, ⟨.fixed .u8, span⟩,
   ⟨.fixed .lbrace, span⟩, ⟨.fixed .return_, span⟩, ⟨.integer 0 .decimal none, span⟩,
   ⟨.fixed .semi, span⟩, ⟨.fixed .rbrace, span⟩]

theorem lexAccepted : RuleAccepted Lexed ([32], []) :=
  ⟨[32], .cons (s := 32) (chunk := [32]) (by decide) .nil, by simp,
    .eof (.whitespace (Or.inl rfl) (.stop [] (by simp) (by simp)))⟩

theorem lexRefused : RuleRefused Lexed ([0], []) := by
  rintro ⟨cs, hd, good, _⟩
  change Utf8.Decodes [0] cs at hd
  generalize he : ([0] : List UInt8) = bytes at hd
  cases hd with
  | nil => cases he
  | @cons c chunk bs cs enc _ =>
    cases chunk with
    | nil => exact enc
    | cons b tail =>
      simp only [List.cons_append, List.cons.injEq] at he
      obtain ⟨rfl, htail⟩ := he
      obtain ⟨rfl, rfl⟩ := List.append_eq_nil_iff.mp htail.symm
      have hc : c = 0 := enc.2
      subst c
      exact (good 0 (by simp)).1 rfl

theorem grammarAccepted : RuleAccepted Predictive grammar0 := grammar0_ll1

theorem grammarRefused : RuleRefused Predictive
    ⟨.program, [(.program, [.t .fn]), (.program, [.t .fn, .t .ident])]⟩ := by
  unfold RuleRefused Predictive
  decide

theorem parseAccepted : RuleAccepted Parsed (parsedTokens, parsedProgram) :=
  Derives.main span span span span span span span span
    (.ret span span (.expr (.integer 0 none span) (.nil _)) .nil)

theorem parseRefused : RuleRefused Parsed ([], parsedProgram) := by
  intro h
  change Derives [] parsedProgram at h
  cases h

theorem entryAccepted : RuleAccepted EntryName "main" := rfl
theorem entryRefused : RuleRefused EntryName "other" := by
  unfold RuleRefused EntryName
  decide

theorem returnsAccepted : RuleAccepted Returns [returned] := True.intro
theorem returnsRefused : RuleRefused Returns [] := id

theorem reachAccepted : RuleAccepted ReachableBody [returned] := True.intro
theorem reachRefused : RuleRefused ReachableBody [returned, returned] := id

theorem suffixAccepted : RuleAccepted ExpectedTypeLiteral none := by
  unfold RuleAccepted ExpectedTypeLiteral
  decide
theorem suffixRefused : RuleRefused ExpectedTypeLiteral (.some .u8) := by
  unfold RuleRefused ExpectedTypeLiteral
  decide

theorem lookupAccepted : RuleAccepted LocalBound ("x", ["x"]) := by
  unfold RuleAccepted LocalBound
  decide
theorem lookupRefused : RuleRefused LocalBound ("x", ["y"]) := by
  unfold RuleRefused LocalBound
  decide

theorem freshAccepted : RuleAccepted FreshLocal ("x", ["y"]) := by
  unfold RuleAccepted FreshLocal
  decide
theorem freshRefused : RuleRefused FreshLocal ("x", ["x"]) := by
  unfold RuleRefused FreshLocal
  decide

theorem binderAvailable : RuleAccepted AvailableBinder "x" := by
  unfold RuleAccepted
  decide
theorem binderUnavailable : RuleRefused AvailableBinder "main" := by
  unfold RuleRefused
  decide

theorem reservedAccepted : RuleAccepted UnreservedLocal "x" := by
  unfold RuleAccepted UnreservedLocal
  decide
theorem reservedRefused : RuleRefused UnreservedLocal "abort" := by
  unfold RuleRefused UnreservedLocal
  decide

theorem namedAccepted : RuleAccepted NamedBinding "x" := by
  unfold RuleAccepted NamedBinding
  decide
theorem namedRefused : RuleRefused NamedBinding "_" := by
  unfold RuleRefused NamedBinding
  decide

theorem radixAccepted : RuleAccepted DecimalLiteral .decimal := by
  unfold RuleAccepted DecimalLiteral
  decide
theorem radixRefused : RuleRefused DecimalLiteral .hexadecimal := by
  unfold RuleRefused DecimalLiteral
  decide

theorem productionAccepted : RuleAccepted AdmittedProduction (.more, [.t .plus, .n .product, .n .more]) := by
  unfold RuleAccepted AdmittedProduction
  decide
theorem productionRefused : RuleRefused AdmittedProduction (.more, [.t .minus, .n .product, .n .more]) := by
  unfold RuleRefused AdmittedProduction
  decide

theorem literalAccepted : RuleAccepted LiteralTyped 255 := .int rfl
theorem literalRefused : RuleRefused LiteralTyped 256 := by
  intro h
  change ConcreteSpec.AtomTy [] (.int .u8 256) (.int .u8) at h
  cases h with
  | int fits => cases fits

theorem admissionAccepted : RuleAccepted Admitted ⟨[], .lit 42⟩ :=
  ⟨by simp [ConcreteSpec.DeclsOk, ConcreteSpec.Slice0.toCore], .ret (.int rfl) (.atom .int) (by simp [ConcreteSpec.Discharged])⟩

theorem admissionRefused : RuleRefused Admitted ⟨[], .lit 256⟩ := by
  intro h
  have body := h.2
  change ConcreteSpec.BodyOk [] [] [] (.ret (.int .u8 256)) at body
  cases body with
  | ret typed _ _ =>
    cases typed with
    | int fits => cases fits

private def unusedLocal : ConcreteSpec.Entry :=
  { ty := .scalar (.int .u8), live := true, used := false, known := some (.int .u8 1) }

private def oneLocal : ConcreteSpec.Ctx := [(ConcreteSpec.Slice0.a₀, unusedLocal)]

theorem localsUsedAccepted : RuleAccepted LocalsUsed (oneLocal, .var ConcreteSpec.Slice0.a₀) := by
  refine ⟨_, .atom (.copy (e := unusedLocal) rfl rfl rfl), ?_⟩
  decide

theorem localsUsedRefused : RuleRefused LocalsUsed (oneLocal, .lit 0) := by
  rintro ⟨_, read, discharged⟩
  cases read with
  | atom read =>
    cases read
    exact (by decide : ¬ ConcreteSpec.Discharged [] oneLocal) discharged

theorem additionAccepted : RuleAccepted AdditionFits (200, 55) := by
  unfold RuleAccepted AdditionFits IntTy.InRange
  decide

theorem additionRefused : RuleRefused AdditionFits (200, 56) := by
  unfold RuleRefused AdditionFits IntTy.InRange
  decide

theorem additionInvalidOperands : RuleRefused AdditionFits (300, -100) := by
  unfold RuleRefused AdditionFits IntTy.InRange
  decide

theorem limitAccepted : RuleAccepted WithinLimit (10, 10) := by
  unfold RuleAccepted WithinLimit
  decide
theorem limitRefused : RuleRefused WithinLimit (11, 10) := by
  unfold RuleRefused WithinLimit
  decide

end Slice0Rules

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

/-- Instance: Slot zero resolves to the file in the singleton project. -/
theorem slotResolvesAccepted : RuleAccepted SlotResolves
    (tabProject, ⟨0⟩) := by
  intro missing
  change tabProject.fileAt ⟨0⟩ = none at missing
  have found : tabProject.fileAt ⟨0⟩ = some (File.load mainSourcePath tabSource) := rfl
  exact Option.some_ne_none _ (found.symm.trans missing)

/-- Instance: Slot one is absent from the singleton project. -/
theorem slotResolvesRefused : RuleRefused SlotResolves
    (tabProject, ⟨1⟩) := by
  intro resolves
  exact resolves rfl

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

theorem freshBinder : RuleAccepted OwnershipRule.FreshBinder (live false, ⟨1⟩) := rfl

theorem duplicateBinder : RuleRefused OwnershipRule.FreshBinder (live false, ⟨0⟩) := nofun

theorem freshPattern : RuleAccepted OwnershipRule.FreshPattern ([], [(⟨"f"⟩, .name ⟨0⟩)]) := by
  exact ⟨by decide, fun _ _ => rfl⟩

theorem reusedPattern : RuleRefused OwnershipRule.FreshPattern (live false, [(⟨"f"⟩, .name ⟨0⟩)]) :=
  fun h => nomatch h.2 ⟨0⟩ (.head _)

theorem moveLinear : RuleAccepted OwnershipRule.ReadAllowed (decls, live false, .move ⟨0⟩) :=
  ⟨_, .move rfl rfl rfl⟩

theorem copyLinear : RuleRefused OwnershipRule.ReadAllowed (decls, live false, .copy ⟨0⟩) := by
  rintro ⟨_, h⟩
  cases h with
  | copy found _ mode => cases Option.some.inj found; contradiction

theorem emptyReturn : RuleAccepted OwnershipRule.ReturnDischarged (decls, []) := fun _ => nofun

theorem liveReturn : RuleRefused OwnershipRule.ReturnDischarged (decls, live true) := by
  intro h
  rcases (h _ (.head _)).1 with copy | moved <;> contradiction

theorem discardByte : RuleAccepted WildcardPermitted (decls, .scalar (.int .u8)) := rfl

theorem discardLinear : RuleRefused WildcardPermitted (decls, sTy) := nofun

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

namespace ProjectRegistry
open ConcreteSpec.Project

-- Fixture-only language-rules identities. The manifest schema assigns their real spelling.
def supported : LanguageRulesVersion := ⟨1, 2⟩

theorem standaloneAccepted : RuleAccepted LanguagePinAccepted (supported, .standalone) := by
  unfold RuleAccepted
  decide

theorem pinAccepted : RuleAccepted LanguagePinAccepted (supported, .project (some supported)) := by
  unfold RuleAccepted
  decide

theorem missingPinRefused : RuleRefused LanguagePinAccepted (supported, .project none) := by
  unfold RuleRefused
  decide

theorem unsupportedPinRefused :
    RuleRefused LanguagePinAccepted (supported, .project (some ⟨2, 0⟩)) := by
  unfold RuleRefused
  decide

def package : PackageId := ⟨⟨"example"⟩, ⟨⟨"0"⟩⟩⟩
def function : FnId := ⟨⟨package, [⟨"entry"⟩], by simp⟩, ⟨"main"⟩⟩

def facts : FunctionFacts :=
  ⟨function, .established false [] .function, .established false [] .function,
    .established false [] .function, .established true [] .function,
    .established 32 [] .function⟩

theorem clauseAccepted : RuleAccepted ClauseAccepted (⟨.noAlloc, []⟩, facts) := by
  unfold RuleAccepted
  decide

theorem allocationRefused : RuleRefused ClauseAccepted
    (⟨.noAlloc, []⟩, { facts with allocates := .established true [] .function }) := by
  unfold RuleRefused
  decide

theorem unknownRefused : RuleRefused ClauseAccepted
    (⟨.noAlloc, []⟩, { facts with allocates := .unknown .function }) := by
  unfold RuleRefused
  decide

def tables : Tables := ⟨some ⟨1, []⟩, some ⟨2, []⟩⟩

theorem versionsAccepted : RuleAccepted TableVersionsAccepted (tables, 1, 2) := by
  unfold RuleAccepted
  decide

theorem policyVersionRefused : RuleRefused TableVersionsAccepted (tables, 2, 2) := by
  unfold RuleRefused
  decide

theorem assumptionsVersionRefused : RuleRefused TableVersionsAccepted (tables, 1, 1) := by
  unfold RuleRefused
  decide


theorem sizeAccepted : RuleAccepted Project.Manifest.SizeAccepted 65536 := by
  unfold RuleAccepted
  decide

theorem sizeRefused : RuleRefused Project.Manifest.SizeAccepted 65537 := by
  unfold RuleRefused
  decide

theorem encodingAccepted : RuleAccepted Project.Manifest.EncodingAccepted (ByteArray.mk #[0xC3, 0xA9]) := by
  unfold RuleAccepted
  decide

theorem encodingRefused : RuleRefused Project.Manifest.EncodingAccepted (ByteArray.mk #[0xFF]) := by
  unfold RuleRefused
  decide

theorem depthAccepted : RuleAccepted Project.Manifest.DepthAccepted "[a.b.c.d.e.f.g.h.i.j.k.l.m.n.o]\nx = true" := by
  unfold RuleAccepted
  cbv <;> decide

theorem depthRefused : RuleRefused Project.Manifest.DepthAccepted "[a.b.c.d.e.f.g.h.i.j.k.l.m.n.o.p]\nx = true" := by
  unfold RuleRefused
  cbv <;> decide

theorem syntaxAccepted : RuleAccepted Project.Manifest.SyntaxAccepted "a = true" := by
  unfold RuleAccepted
  cbv <;> decide

theorem syntaxRefused : RuleRefused Project.Manifest.SyntaxAccepted "a = " := by
  unfold RuleRefused
  cbv <;> decide

end ProjectRegistry

/-- Complete conformance links, populated alongside the owning rules. -/
def registeredRuleExamples : List RuleExample :=
  [⟨``ConstsPermitted, ``ConstRegistry.accepted⟩,
   ⟨``ConstsPermitted, ``ConstRegistry.refused⟩,
   ⟨``OwnershipRule.FreshBinder, ``Slice1Registry.freshBinder⟩,
   ⟨``OwnershipRule.FreshBinder, ``Slice1Registry.duplicateBinder⟩,
   ⟨``OwnershipRule.FreshPattern, ``Slice1Registry.freshPattern⟩,
   ⟨``OwnershipRule.FreshPattern, ``Slice1Registry.reusedPattern⟩,
   ⟨``OwnershipRule.ReadAllowed, ``Slice1Registry.moveLinear⟩,
   ⟨``OwnershipRule.ReadAllowed, ``Slice1Registry.copyLinear⟩,
   ⟨``OwnershipRule.ReturnDischarged, ``Slice1Registry.emptyReturn⟩,
   ⟨``OwnershipRule.ReturnDischarged, ``Slice1Registry.liveReturn⟩,
   ⟨``WildcardPermitted, ``Slice1Registry.discardByte⟩,
   ⟨``WildcardPermitted, ``Slice1Registry.discardLinear⟩,
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
   ⟨``SourcePathSpan.SlotResolves, ``SourcePathSpan.Registry.slotResolvesAccepted⟩,
   ⟨``SourcePathSpan.SlotResolves, ``SourcePathSpan.Registry.slotResolvesRefused⟩,
   ⟨``SourcePathSpan.OrderedOffsets, ``SourcePathSpan.Registry.offsetsAccepted⟩,
   ⟨``SourcePathSpan.OrderedOffsets, ``SourcePathSpan.Registry.offsetsRefused⟩,
   ⟨``SourcePathSpan.EndInSource, ``SourcePathSpan.Registry.endAccepted⟩,
   ⟨``SourcePathSpan.EndInSource, ``SourcePathSpan.Registry.endRefused⟩,
   ⟨``SourcePathSpan.LineMatches, ``SourcePathSpan.Registry.lineAccepted⟩,
   ⟨``SourcePathSpan.LineMatches, ``SourcePathSpan.Registry.lineRefused⟩,
   ⟨``SourcePathSpan.ColumnMatches, ``SourcePathSpan.Registry.columnAccepted⟩,
   ⟨``SourcePathSpan.ColumnMatches, ``SourcePathSpan.Registry.columnRefused⟩,
   ⟨``Project.LanguagePinAccepted, ``ProjectRegistry.standaloneAccepted⟩,
   ⟨``Project.LanguagePinAccepted, ``ProjectRegistry.pinAccepted⟩,
   ⟨``Project.LanguagePinAccepted, ``ProjectRegistry.missingPinRefused⟩,
   ⟨``Project.LanguagePinAccepted, ``ProjectRegistry.unsupportedPinRefused⟩,
   ⟨``Project.ClauseAccepted, ``ProjectRegistry.clauseAccepted⟩,
   ⟨``Project.ClauseAccepted, ``ProjectRegistry.allocationRefused⟩,
   ⟨``Project.ClauseAccepted, ``ProjectRegistry.unknownRefused⟩,
   ⟨``Project.TableVersionsAccepted, ``ProjectRegistry.versionsAccepted⟩,
   ⟨``Project.TableVersionsAccepted, ``ProjectRegistry.policyVersionRefused⟩,
   ⟨``Project.TableVersionsAccepted, ``ProjectRegistry.assumptionsVersionRefused⟩,
   ⟨``Project.Manifest.SizeAccepted, ``ProjectRegistry.sizeAccepted⟩,
   ⟨``Project.Manifest.SizeAccepted, ``ProjectRegistry.sizeRefused⟩,
   ⟨``Project.Manifest.EncodingAccepted, ``ProjectRegistry.encodingAccepted⟩,
   ⟨``Project.Manifest.EncodingAccepted, ``ProjectRegistry.encodingRefused⟩,
   ⟨``Project.Manifest.DepthAccepted, ``ProjectRegistry.depthAccepted⟩,
   ⟨``Project.Manifest.DepthAccepted, ``ProjectRegistry.depthRefused⟩,
   ⟨``Project.Manifest.SyntaxAccepted, ``ProjectRegistry.syntaxAccepted⟩,
   ⟨``Project.Manifest.SyntaxAccepted, ``ProjectRegistry.syntaxRefused⟩] ++
  [⟨``Slice0Rules.Lexed, ``Slice0Rules.lexAccepted⟩,
   ⟨``Slice0Rules.Lexed, ``Slice0Rules.lexRefused⟩,
   ⟨``Slice0Rules.Predictive, ``Slice0Rules.grammarAccepted⟩,
   ⟨``Slice0Rules.Predictive, ``Slice0Rules.grammarRefused⟩,
   ⟨``Slice0Rules.Parsed, ``Slice0Rules.parseAccepted⟩,
   ⟨``Slice0Rules.Parsed, ``Slice0Rules.parseRefused⟩,
   ⟨``Slice0Rules.EntryName, ``Slice0Rules.entryAccepted⟩,
   ⟨``Slice0Rules.EntryName, ``Slice0Rules.entryRefused⟩,
   ⟨``Slice0Rules.Returns, ``Slice0Rules.returnsAccepted⟩,
   ⟨``Slice0Rules.Returns, ``Slice0Rules.returnsRefused⟩,
   ⟨``Slice0Rules.ReachableBody, ``Slice0Rules.reachAccepted⟩,
   ⟨``Slice0Rules.ReachableBody, ``Slice0Rules.reachRefused⟩,
   ⟨``Slice0Rules.ExpectedTypeLiteral, ``Slice0Rules.suffixAccepted⟩,
   ⟨``Slice0Rules.ExpectedTypeLiteral, ``Slice0Rules.suffixRefused⟩,
   ⟨``Slice0Rules.LocalBound, ``Slice0Rules.lookupAccepted⟩,
   ⟨``Slice0Rules.LocalBound, ``Slice0Rules.lookupRefused⟩,
   ⟨``Slice0Rules.FreshLocal, ``Slice0Rules.freshAccepted⟩,
   ⟨``Slice0Rules.FreshLocal, ``Slice0Rules.freshRefused⟩,
   ⟨``Slice0Rules.AvailableBinder, ``Slice0Rules.binderAvailable⟩,
   ⟨``Slice0Rules.AvailableBinder, ``Slice0Rules.binderUnavailable⟩,
   ⟨``Slice0Rules.UnreservedLocal, ``Slice0Rules.reservedAccepted⟩,
   ⟨``Slice0Rules.UnreservedLocal, ``Slice0Rules.reservedRefused⟩,
   ⟨``Slice0Rules.NamedBinding, ``Slice0Rules.namedAccepted⟩,
   ⟨``Slice0Rules.NamedBinding, ``Slice0Rules.namedRefused⟩,
   ⟨``Slice0Rules.DecimalLiteral, ``Slice0Rules.radixAccepted⟩,
   ⟨``Slice0Rules.DecimalLiteral, ``Slice0Rules.radixRefused⟩,
   ⟨``Slice0Rules.AdmittedProduction, ``Slice0Rules.productionAccepted⟩,
   ⟨``Slice0Rules.AdmittedProduction, ``Slice0Rules.productionRefused⟩,
   ⟨``Slice0Rules.LiteralTyped, ``Slice0Rules.literalAccepted⟩,
   ⟨``Slice0Rules.LiteralTyped, ``Slice0Rules.literalRefused⟩,
   ⟨``Slice0Rules.Admitted, ``Slice0Rules.admissionAccepted⟩,
   ⟨``Slice0Rules.Admitted, ``Slice0Rules.admissionRefused⟩,
   ⟨``Slice0Rules.LocalsUsed, ``Slice0Rules.localsUsedAccepted⟩,
   ⟨``Slice0Rules.LocalsUsed, ``Slice0Rules.localsUsedRefused⟩,
   ⟨``Slice0Rules.AdditionFits, ``Slice0Rules.additionAccepted⟩,
   ⟨``Slice0Rules.AdditionFits, ``Slice0Rules.additionRefused⟩,
   ⟨``Slice0Rules.WithinLimit, ``Slice0Rules.limitAccepted⟩,
   ⟨``Slice0Rules.WithinLimit, ``Slice0Rules.limitRefused⟩,
   ⟨``Slice0Rules.AdditionFits, ``Slice0Rules.additionInvalidOperands⟩]

end ConcreteSpec.Core
