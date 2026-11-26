import ConcreteSpec.Diagnostics.FailedObligation
import ConcreteSpec.Spec.Examples

namespace ConcreteSpec.FailedObligation

-- The closed slice-1 fixture moves and destructures a struct before the selected sum.
example : check Examples.fieldOverflowTrap = false := by decide

example : diagnoseClosedAdd Examples.fieldOverflowTrap ⟨2, .u8⟩ 1 3 =
    ⟨⟨2, .u8⟩, 1, 3, .refutedReplayed ⟨200, 100⟩⟩ := by decide

-- This run cannot reach the obligation. Exhaustion yields no witness.
example : diagnoseClosedAdd Examples.fieldOverflowTrap ⟨2, .u8⟩ 1 2 =
    ⟨⟨2, .u8⟩, 1, 2, .noWitness 1⟩ := by decide

private def smallSum : Program :=
  { Examples.overflowDropped with
    body := [.let_ .wild (.scalar (.int .u8)) (.add (.int .u8 1) (.int .u8 2))] }

example : diagnoseClosedAdd smallSum ⟨0, .u8⟩ 5 1 =
    ⟨⟨0, .u8⟩, 5, 1, .noWitness 1⟩ := by decide

-- A trap in the same exact program at a different statement is no evidence for this addition.
private def earlierTrap : Program :=
  { smallSum with body := Examples.overflowDropped.body ++ smallSum.body }

example : replayClosedAdd earlierTrap ⟨1, .u8⟩ 2 = none := by decide
example : replayClosedAdd Examples.fieldOverflowTrap ⟨2, .i8⟩ 3 = none := by decide

-- Constants outside the type range and duplicates never consume cases. The first boundary
-- is excluded by the path; replay returns no evidence at the second.
private def axes : List (Array Int) := [boundaryValues 0 255 [12, -1, 0, 12, 256]]
private def onPath (input : List Int) : Bool := input != [0]
private def replay (input : List Int) : Option Int :=
  if input == [255] then none else input.head?

example : search axes 2 onPath replay = .noWitness 2 := by decide
example : search axes 3 onPath replay = .refutedReplayed 12 := by decide
example : search axes 0 onPath replay = .noWitness 0 := by decide

example : search [#[0, 1], #[2, 3]] 3 (fun input => input == [0, 3]) some =
    .refutedReplayed [0, 3] := by decide

example : (SearchResult.refutedReplayed ()).message = "refuted, replayed" := by decide

example : (SearchResult.noWitness 1 : SearchResult AddWitness).message =
    "no witness within 1 cases" := by decide

end ConcreteSpec.FailedObligation
