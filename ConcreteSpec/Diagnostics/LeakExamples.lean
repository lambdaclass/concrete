import ConcreteSpec.Spec.Leak
import ConcreteSpec.Spec.Own
import ConcreteSpec.Diagnostics.Types

/-! C7 numeric-local fixtures. Spellings belong to diagnostics and never enter a labelling,
release record, operand evaluation or accounting projection. Authority:
docs/decisions.md#decided-2026-11-08-c9-identities (numeric identity). -/
namespace ConcreteSpec.Diagnostics.LeakExamples
open Core Core.Leak

/-- Numeric local used by this diagnostic fixture. -/
def hiddenVar : Var := ⟨0⟩
/-- Concrete local environment, independent of proof fixtures. -/
def hiddenLocals (n : Nat) : AList Var Int := [(hiddenVar, Int.negSucc n)]
/-- Decode the fixture's local payload through the shared numeric lookup. -/
def hiddenOffset (locals : AList Var Int) : Int :=
  match locals.lookup hiddenVar with
  | some (.negSucc n) => Int.ofNat n
  | _ => 0

/-- Two tables rename local zero and give another number its earlier spelling. -/
def beforeNames : LocalNames := ⟨[(hiddenVar, "hidden"), (⟨1⟩, "public")]⟩
def afterNames : LocalNames := ⟨[(hiddenVar, "secret_offset"), (⟨1⟩, "hidden")]⟩

/-- Elaboration keeps source names beside the numeric environment. -/
def fixture : Elaborated (AList Var Int) := ⟨hiddenLocals 2, beforeNames⟩

/-- Instance: rehinting preserves reads of C7's numeric secret local; another number stays distinct. -/
example : AList.lookup beforeNames.entries hiddenVar ≠ AList.lookup afterNames.entries hiddenVar ∧
    AList.lookup beforeNames.entries hiddenVar = AList.lookup afterNames.entries ⟨1⟩ ∧
    hiddenVar ≠ (⟨1⟩ : Var) ∧ hiddenOffset (hiddenLocals 2) = 2 := by decide

/-- Instance: assignment and scope removal use the same numeric identity and shared operations. -/
example : hiddenOffset (AList.update (hiddenLocals 2) ⟨0⟩ (fun _ => -5)) = 4 ∧
    Exec.restrict [⟨1⟩] (hiddenLocals 2 ++ [(⟨1⟩, 7)]) = [(⟨1⟩, 7)] := by decide

/-- Instance: the migrated local zero is also the ownership fixture's zero. Dropping its live linear
obligation is refused, and consuming it permits storage teardown, regardless of either name table. -/
example : hiddenVar = Own.Examples.x ∧
    Own.drop [hiddenVar] Own.Examples.initial = none ∧
    (Own.move Own.Examples.initial ⟨0⟩ >>= Own.drop [hiddenVar]).isSome = true := by decide

end ConcreteSpec.Diagnostics.LeakExamples
