import ConcreteSpec.Spec.Slice0
import ConcreteSpec.Spec.Surface0

/-! Slice-0 rule predicates for conformance and diagnostics. Where a rule already has a
specification judgment, the predicate delegates to that judgment. -/

namespace ConcreteSpec.Core.Slice0Rules

open ConcreteSpec.Surface0

/-- Lexical derivation for slice-0 bytes and tokens. Decision: Q4.2,
docs/decisions.md#recovered-59-30. Existing BOM and CR scope holds are documented in Surface0. -/
def Lexed (input : List UInt8 × List Token) : Prop :=
  Lexes ⟨input.1.toArray⟩ input.2

/-- The grammar's checked LL(1) requirement. Decision: Q8.3,
docs/decisions.md#decided-2026-11-25-freeze-and-grammar. -/
def Predictive (grammar : Concrete.LL1.Grammar Terminal Nonterminal) : Prop :=
  Concrete.LL1.ll1 grammar = true

/-- Payload-level parse derivation. Decision: Q8.3,
docs/decisions.md#decided-2026-11-25-freeze-and-grammar. -/
def Parsed (input : List Token × Surface0.Program) : Prop := Derives input.1 input.2

/-- The entry elaborates under the slice-0 `main` function contract. Decision: Q8.3,
docs/decisions.md#decided-2026-11-25-freeze-and-grammar.
Full source-to-Core correspondence is the independent 4(f) obligation. -/
def EntryName (name : String) : Prop := name = "main"

/-- Elaboration must have a return and must not encounter a statement after it.
Decision: Q8.3, docs/decisions.md#decided-2026-11-23-slice0-and-roadmap-answers. -/
def BodyAccepted (body : List Statement) : Prop := (checkBody body).isOk = true

/-- STMT-14 forbids a statement after the return, independently of STMT-9 fallthrough.
Decision: Q8.3, docs/decisions.md#decided-2026-11-23-slice0-and-roadmap-answers. -/
def ReachableBody (body : List Statement) : Prop :=
  match checkBody body with
  | .error (.unreachable _) => False
  | _ => True

/-- Slice-0 literal typing delegates to the Core atom judgment (ARITH-2).
Decision: Q6.1, docs/decisions.md#decided-project-no-hidden-traps. -/
def LiteralTyped (n : Nat) : Prop :=
  ConcreteSpec.AtomTy [] (.int .u8 n) (.int .u8)

/-- Slice-0 admission delegates to the Core declarative checker contract.
Decision: Q6.1, docs/decisions.md#decided-project-no-hidden-traps. -/
def Admitted (program : ConcreteSpec.Slice0.Program) : Prop :=
  ConcreteSpec.Checked (ConcreteSpec.Slice0.toCore program)

/-- Whether a slice-0 checked addition produces an integer result.
Decision: Q6.1, docs/decisions.md#decided-project-no-hidden-traps. -/
def AdditionFits (operands : Int × Int) : Prop :=
  ConcreteSpec.checkedAdd .u8 operands.1 operands.2 =
    .ok (.scalar (.int .u8 (operands.1 + operands.2)))

/-- Inclusive accounting rule for R-0528 resource limits. The caller supplies a measured
quantity and a limit in the same unit. E7 leaves the numeric scan limit, its unit and refusal
precedence open. This rule also leaves driver enforcement to A14. Decision: R-0528. -/
def WithinLimit (measuredAndLimit : Nat × Nat) : Prop :=
  measuredAndLimit.1 ≤ measuredAndLimit.2

end ConcreteSpec.Core.Slice0Rules
