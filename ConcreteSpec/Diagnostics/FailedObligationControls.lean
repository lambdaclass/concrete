import ConcreteSpec.Diagnostics.FailedObligation
import ConcreteSpec.Spec.Examples
import ConcreteSpec.Spec.Slice1Checker
import ConcreteSpec.Surface1

namespace ConcreteSpec.FailedObligation
open Examples (u8 x₀ s₂)

private def fieldOrigin (index : Nat) : Option Core.SiteId :=
  if index = 2 then some ⟨7⟩ else none

private def fieldObligation : Obligation :=
  ⟨⟨⟨7⟩, .safety, 0⟩, ⟨``admitted, 1⟩, .overflow, .u8, ⟨200, 200⟩, ⟨100, 100⟩⟩

example : diagnoseClosed Examples.fieldOverflowTrap fieldOrigin 1 3 =
    some ⟨fieldObligation, 1, 3, .refutedReplayed ⟨[], 200, 100⟩⟩ := by decide

example : diagnoseClosed Examples.fieldOverflowTrap fieldOrigin 1 2 =
    some ⟨fieldObligation, 1, 2, .noWitness 1⟩ := by decide

example : diagnoseClosed Examples.fieldOverflowTrap fieldOrigin 1 0 =
    some ⟨fieldObligation, 1, 0, .noWitness 1⟩ := by decide

example : diagnoseClosed Examples.fieldOverflowTrap
    (fun index => if index = 3 then some ⟨7⟩ else none) 1 4 = none := by decide

-- Slice-1 specification elaboration of a source operation. This one-operation control can
-- read the sole lexical site directly. The production origin lookup is owned by 4(d).
private def sourceSpan : Surface0.Span := ⟨32, 41, 2, 15⟩

private def overflowSource : Surface1.SourceProgram :=
  ⟨[], "main", [.bind "_" (.typed u8) (.add (.integer 200 none) (.integer 100 none) sourceSpan),
    .ret (.integer 42 none)]⟩

example : (do
    let out ← Surface1.elaborate overflowSource
    let [(site, span)] := out.sites | none
    let report ← diagnoseClosed out.core (fun index => if index = 0 then some site else none) 1 1
    pure (check out.core, span, report)) =
    some (false, sourceSpan,
      ⟨{ fieldObligation with id := ⟨⟨0⟩, .safety, 0⟩ }, 1, 1,
        .refutedReplayed ⟨[], 200, 100⟩⟩) := by decide

private def origin (index : Nat) : Option Core.SiteId := some ⟨index + 10⟩

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
    (fun p => !check p && (diagnoseClosed p origin 1 4).isNone) = true := by decide

example : check Examples.fieldSumFits = true ∧
    diagnoseClosed Examples.fieldSumFits origin 1 8 = none := by decide

private def openProgram : Program :=
  ⟨Examples.decls, Examples.add100, .ret (.copy s₂)⟩

private def inputReport (onPath : List Int → Bool) (cases : Nat) : Option Report :=
  diagnoseInputs openProgram origin [(x₀, .u8)] [200, -1, 200, 256] onPath cases 2

example : bodyOkB openProgram.decls Examples.inputCtx openProgram.body openProgram.ending =
    false := by decide

-- The single tested input returns normally with sufficient fuel. An exhausted search on this
-- refused operation therefore cannot be used as an admission result.
example : runBody openProgram.decls 2 (Examples.inputEnv 0) openProgram.body openProgram.ending =
    .exit 100 := by decide

example : (inputReport (fun _ => true) 1).map Report.result = some (.noWitness 1) := by decide
example : (inputReport (fun _ => true) 2).map Report.result =
    some (.refutedReplayed ⟨[(0, 255)], 255, 100⟩) := by decide

-- The second endpoint overflows but is outside this path. The in-range path constant
-- is then tried and replayed by the reference evaluator.
example : (inputReport (fun xs => xs != [255]) 2).map Report.result =
    some (.noWitness 2) := by decide
example : (inputReport (fun xs => xs != [255]) 3).map Report.result =
    some (.refutedReplayed ⟨[(0, 200)], 200, 100⟩) := by decide

private def twoInputProgram : Program :=
  ⟨[], [.let_ (.name s₂) u8 (.add (.copy x₀) (.copy ⟨1⟩))], .ret (.copy s₂)⟩

example : (diagnoseInputs twoInputProgram origin [(x₀, .u8), (⟨1⟩, .u8)] []
    (fun _ => true) 4 2).map Report.result =
    some (.refutedReplayed ⟨[(0, 255), (1, 255)], 255, 255⟩) := by decide

example : ∃ program report,
    diagnoseClosed program fieldOrigin 1 2 = some report ∧
    report.result = .noWitness 1 ∧ ¬ Checked program := by
  let report : Report := ⟨fieldObligation, 1, 2, .noWitness 1⟩
  have h : diagnoseClosed Examples.fieldOverflowTrap fieldOrigin 1 2 = some report := by decide
  exact ⟨_, report, h, rfl, diagnoseClosed_refused h⟩

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
