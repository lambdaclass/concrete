import ConcreteSpec.Spec.Slice0Layout
import Concrete.Refusal

open Concrete ConcreteSpec.Core

-- This list names the rules exercised by the slice-0 acceptance and refusal proofs.
-- Resolve the names through the same registry used by diagnostic producers.
private def exercisedNames : List Lean.Name :=
  [``Slice0Rules.Lexed, ``Slice0Rules.Predictive, ``Slice0Rules.Parsed,
   ``Slice0Rules.EntryName, ``Slice0Rules.BodyAccepted, ``Slice0Rules.ReachableBody,
   ``Slice0Rules.LiteralTyped, ``Slice0Rules.Admitted, ``Slice0Rules.AdditionFits,
   ``Slice0Rules.WithinLimit]

private def exercised : List RegisteredRule := exercisedNames.filterMap referenceRule

example : exercised.length = exercisedNames.length := by decide
example : missingRules ⟨0⟩ exercised = some [] := by decide
example : (missingRules ⟨0⟩ exercised.tail).map (List.map RuleId.name) =
    some [``Slice0Rules.Lexed] := by decide
example : missingRules ⟨1⟩ exercised = none := by decide

private def refusal : Refusal := .checker "unreachable code"
example : ((refusal.diagnostic ``Slice0Rules.ReachableBody).map (fun d => d.rule.val.name)) =
    some ``Slice0Rules.ReachableBody := by decide
example : (refusal.diagnostic `unregistered).isNone = true := by decide
example : (validateRule ⟨``Slice0Rules.ReachableBody, 2⟩).isNone = true := by decide

open Slice0Layout in
example : (query linuxX8664 (.integer .u8)).map Layout.byteSize = some 1 := rfl

open Slice0Layout in
example : query linuxX8664 (.integer .u16) = none := rfl

open Slice0Layout in
example :
    let target := { linuxX8664 with maxObject := 0 }
    query target (.integer .u8) = none ∧
      (query target .logical).map Layout.byteSize = some 0 := ⟨rfl, rfl⟩
