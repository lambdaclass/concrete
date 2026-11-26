import ConcreteSpec.Diagnostics.FailedObligation
import ConcreteSpec.Spec.Examples
import ConcreteSpec.Spec.Slice1Checker
import ConcreteSpec.Spec.Rules
import ConcreteSpec.Surface1

namespace ConcreteSpec.FailedObligation
open Examples (u8 x₀ s₂)

private def fieldObligation : Obligation :=
  ⟨.statement 2, ⟨``ProgramPermitted, 1⟩, .overflow, .u8, ⟨200, 200⟩, ⟨100, 100⟩⟩

example : diagnoseClosed Examples.fieldOverflowTrap 1 3 =
    some ⟨fieldObligation, 1, 3, .refutedReplayed ⟨[], 200, 100⟩⟩ := by decide

example : diagnoseClosed Examples.fieldOverflowTrap 1 2 =
    some ⟨fieldObligation, 1, 2, .noWitness 1⟩ := by decide

example : diagnoseClosed Examples.fieldOverflowTrap 1 0 =
    some ⟨fieldObligation, 1, 0, .noWitness 1⟩ := by decide

example : (diagnoseClosed Examples.fieldOverflowTrap 1 3).any
    (fun report => Core.registeredRules.contains report.obligation.rule) = true := by decide

private def outerSpan : Surface0.Span := ⟨24, 42, 2, 7⟩
private def innerSpan : Surface0.Span := ⟨32, 41, 2, 15⟩

-- The outer plus is lexically first. The overflowing right operand is lowered first.
-- Both sites exist, and the overflowing statement's ordinal names the other site.
private def overflowSource : Surface1.SourceProgram :=
  ⟨[], "main", [.bind "_" (.typed u8)
      (.add (.integer 1 none) (.add (.integer 200 none) (.integer 100 none) innerSpan) outerSpan),
    .ret (.integer 42 none)]⟩

example : (Surface1.elaborate overflowSource).map (fun out => (out.sites, out.statementSites)) =
    some ([(⟨0⟩, outerSpan), (⟨1⟩, innerSpan)], [some ⟨1⟩, some ⟨0⟩]) := by decide

private def sourceReport : Report :=
  ⟨{ fieldObligation with location := .source ⟨⟨1⟩, .safety, 0⟩ }, 1, 1,
    .refutedReplayed ⟨[], 200, 100⟩⟩

example : diagnoseSource overflowSource 1 1 = some sourceReport := by decide
example : ¬ Surface1.SourcePermitted overflowSource :=
  diagnoseSource_refused (report := sourceReport) (by decide : diagnoseSource overflowSource 1 1 = _)

private def wrongPrefix : Program := { Examples.overflowDropped with body :=
  (Stmt.let_ .wild u8 (.atom (.bool true))) :: Examples.overflowDropped.body }

private def badLiteralPrefix : Program := { Examples.overflowDropped with body :=
  [.let_ (.name x₀) u8 (.atom (.int .u8 1000)),
   .let_ (.name s₂) u8 (.add (.copy x₀) (.int .u8 (-900))),
   .let_ .wild u8 (.add (.copy s₂) (.int .u8 200))] }

private def wrongTargetType : Program := { Examples.overflowDropped with body :=
  [.let_ .wild (.scalar .bool) (.add (.int .u8 200) (.int .u8 100))] }

private def duplicateTarget : Program := { Examples.overflowDropped with body :=
  [.let_ (.name x₀) u8 (.atom (.int .u8 0)),
   .let_ (.name x₀) u8 (.add (.int .u8 200) (.int .u8 100))] }

private def linearDiscard : Program := { Examples.overflowDropped with body :=
  Examples.wildLinear.body ++ Examples.overflowDropped.body }

private def duplicateDecl : Program := { Examples.overflowDropped with
  decls := Examples.token :: Examples.decls }

-- All of these reach a real overflow in unchecked execution. The overflow is not evidence
-- for the actual checker rejection.
example : [wrongPrefix, badLiteralPrefix, wrongTargetType, duplicateTarget, linearDiscard,
    duplicateDecl].all (fun p => run p 4 == .trap .arithmeticOverflow) = true := by decide

example : [wrongPrefix, badLiteralPrefix, wrongTargetType, duplicateTarget, linearDiscard,
    duplicateDecl, Examples.wrongReturnType].all
    (fun p => !check p && (diagnoseClosed p 1 4).isNone) = true := by decide

example : check Examples.fieldSumFits = true ∧
    diagnoseClosed Examples.fieldSumFits 1 8 = none := by decide

private def openProgram : Program :=
  ⟨Examples.decls, Examples.add100, .ret (.copy s₂)⟩

private def inputReport (onPath : List Int → Bool) (cases : Nat) : Option Report :=
  diagnoseInputs openProgram [(x₀, .u8)] [200, -1, 200, 256] onPath cases 2

example : bodyOkB openProgram.decls Examples.inputCtx openProgram.body openProgram.ending =
    false := by decide

-- The single tested input returns normally with sufficient fuel. An exhausted search on this
-- refused operation therefore cannot be used as an admission result.
example : runBody openProgram.decls 2 (Examples.inputEnv 0) openProgram.body openProgram.ending =
    .exit 100 := by decide

private def openObligation : Obligation :=
  ⟨.statement 0, ⟨``ProgramPermitted, 1⟩, .overflow, .u8, ⟨0, 255⟩, ⟨100, 100⟩⟩

private def exhaustedOpen : Report := ⟨openObligation, 1, 2, .noWitness 1⟩

example : inputReport (fun _ => true) 1 = some exhaustedOpen := by decide

example : ¬ BodyOk openProgram.decls Examples.inputCtx openProgram.body openProgram.ending :=
  (diagnoseInputs_refused
    (by decide : inputReport (fun _ => true) 1 = some exhaustedOpen)).2

example : inputReport (fun _ => true) 2 =
    some ⟨openObligation, 2, 2, .refutedReplayed ⟨[(x₀, 255)], 255, 100⟩⟩ := by decide

example : diagnoseInputs openProgram [(x₀, .u8), (x₀, .u8)] []
    (fun _ => true) 4 2 = none := by decide

example : diagnoseInputs {openProgram with body := Examples.add0} [(x₀, .u8)] []
    (fun _ => true) 2 2 = none := by decide

example : diagnoseInputs {openProgram with body := [.let_ (.name s₂) (.scalar .bool)
    (.add (.copy x₀) (.int .u8 100))]} [(x₀, .u8)] [] (fun _ => true) 2 2 = none := by decide

private def signedProgram : Program :=
  ⟨[], [.let_ .wild (.scalar (.int .i8)) (.add (.copy x₀) (.int .i8 (-1)))], .ret (.int .u8 42)⟩

example : diagnoseInputs signedProgram [(x₀, .i8)] [] (fun _ => true) 1 1 =
    some ⟨{openObligation with type := .i8, left := ⟨-128, 127⟩, right := ⟨-1, -1⟩},
      1, 1, .refutedReplayed ⟨[(x₀, -128)], -128, -1⟩⟩ := by decide

-- The second endpoint overflows but is outside this path. The in-range path constant
-- is then tried and replayed by the reference evaluator.
example : (inputReport (fun xs => xs != [255]) 2).map Report.result =
    some (.noWitness 2) := by decide
example : (inputReport (fun xs => xs != [255]) 3).map Report.result =
    some (.refutedReplayed ⟨[(x₀, 200)], 200, 100⟩) := by decide

private def twoInputProgram : Program :=
  ⟨[], [.let_ (.name s₂) u8 (.add (.copy x₀) (.copy ⟨1⟩))], .ret (.copy s₂)⟩

example : (diagnoseInputs twoInputProgram [(x₀, .u8), (⟨1⟩, .u8)] []
    (fun _ => true) 4 2).map Report.result =
    some (.refutedReplayed ⟨[(x₀, 255), (⟨1⟩, 255)], 255, 255⟩) := by decide

example : ∃ program report,
    diagnoseClosed program 1 2 = some report ∧
    report.result = .noWitness 1 ∧ ¬ Checked program := by
  let report : Report := ⟨fieldObligation, 1, 2, .noWitness 1⟩
  have h : diagnoseClosed Examples.fieldOverflowTrap 1 2 = some report := by decide
  exact ⟨_, report, h, rfl, diagnoseClosed_refused h⟩

example : ∃ out, Surface1.elaborate overflowSource = some out ∧
    ReplayValid out.core [] sourceReport ⟨[], 200, 100⟩ := by
  obtain ⟨out, he, _, _, hr⟩ := diagnoseSource_sound
    (by decide : diagnoseSource overflowSource 1 1 = some sourceReport) rfl
  exact ⟨out, he, hr⟩

example : ∃ (out : Surface1.Elaborated) (index : Nat) (site : Core.SiteId),
    Surface1.elaborate overflowSource = some out ∧
    sourceReport.obligation.location = .source ⟨site, .safety, 0⟩ ∧
    (∃ binder ty left right, out.core.body[index]? = some (Stmt.let_ binder ty (.add left right))) ∧
    out.statementSites[index]? = some (some site) :=
  (diagnoseSource_origin (by decide : diagnoseSource overflowSource 1 1 = some sourceReport)).2

example : let report : Report := ⟨fieldObligation, 1, 3, .refutedReplayed ⟨[], 200, 100⟩⟩
    ReplayValid Examples.fieldOverflowTrap [] report ⟨[], 200, 100⟩ := by
  intro report
  exact (diagnoseClosed_sound
    (by decide : diagnoseClosed Examples.fieldOverflowTrap 1 3 = some report) rfl).2.2.2.2

example : 1 ≤ exhaustedOpen.caseBudget ∧ exhaustedOpen.caseBudget = 1 :=
  diagnoseInputs_exhausted (by decide : inputReport (fun _ => true) 1 = some exhaustedOpen) rfl

example : let report : Report := ⟨fieldObligation, 1, 2, .noWitness 1⟩
    1 ≤ report.caseBudget ∧ report.caseBudget = 1 := by
  intro report
  exact diagnoseClosed_exhausted
    (by decide : diagnoseClosed Examples.fieldOverflowTrap 1 2 = some report) rfl

example : let report : Report := ⟨openObligation, 3, 2, .refutedReplayed ⟨[(x₀, 200)], 200, 100⟩⟩
    ∃ values, (values != [255]) = true ∧ InputsInRange [(x₀, .u8)] values ∧
      ReplayValid openProgram (inputBindings [(x₀, .u8)] values).1 report ⟨[(x₀, 200)], 200, 100⟩ := by
  intro report
  obtain ⟨_, _, _, _, _, _, values, _, hp, hv, _, _, hr⟩ := diagnoseInputs_sound
    (by decide : inputReport (fun xs => xs != [255]) 3 = some report) rfl
  exact ⟨values, hp, hv, hr⟩

example : boundaryValues 0 255 [12, -1, 0, 12, 256] = #[0, 255, 12] := by decide
example : boundaryValues 3 2 [2, 3] = #[] := by decide
example : search [#[]] 3 (fun _ => true) (some : List Int → Option (List Int)) =
    .noWitness 0 := by decide
example : search [#[0, 1], #[2, 3]] 3 (fun input => input == [0, 3]) some =
    .refutedReplayed [0, 3] := by decide
example : search [#[(0 : Int)]] 0 (fun _ => true) some = .noWitness 0 := by decide

example : (SearchResult.refutedReplayed ()).message = "refuted, replayed" := by decide

example : (SearchResult.noWitness 1 : SearchResult Witness).message =
    "no witness within 1 cases" := by decide

end ConcreteSpec.FailedObligation
