import ConcreteSpec.Spec.Slice0
import ConcreteSpec.Spec.Surface0
import ConcreteSpec.Spec.Scalar

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

/-- The table interpreter must have remaining work budget, a defined reduction for
the completed row, and exactly one start value on completion. A missing transition
is an implementation failure (Q8.3, docs/decisions.md#decided-2026-11-25-freeze-and-grammar). -/
inductive ParserTransition where
  | advance (remaining : Nat)
  | reduce (actionDefined : Bool)
  | finish (startValues : Nat)
  deriving DecidableEq

/-- The interpreter has a defined transition for the current state (Q8.3,
docs/decisions.md#decided-2026-11-25-freeze-and-grammar). -/
def ParserCanStep : ParserTransition → Prop
  | .advance remaining => 0 < remaining
  | .reduce defined => defined = true
  | .finish values => values = 1

/-- Payload-level parse derivation. Decision: Q8.3,
docs/decisions.md#decided-2026-11-25-freeze-and-grammar. -/
def Parsed (input : List Token × Surface0.Program) : Prop := Derives input.1 input.2

/-- The entry elaborates under the slice-0 `main` function contract. Decision: Q8.3,
docs/decisions.md#decided-2026-11-25-freeze-and-grammar.
Full source-to-Core correspondence is the independent 4(f) obligation. -/
def EntryName (name : String) : Prop := name = "main"

/-- STMT-9 refuses fallthrough independently of STMT-14.
Decision: Q8.3, docs/decisions.md#decided-2026-11-23-slice0-and-roadmap-answers. -/
def Returns (body : List Statement) : Prop :=
  match checkBody body with
  | .error .fallthrough => False
  | _ => True

/-- STMT-14 forbids a statement after the return, independently of STMT-9 fallthrough.
Decision: Q8.3, docs/decisions.md#decided-2026-11-23-slice0-and-roadmap-answers. -/
def ReachableBody (body : List Statement) : Prop :=
  match checkBody body with
  | .error (.unreachable _) => False
  | _ => True

/-- In an expected-type position the literal has no suffix. Every slice-0 integer
expression is in such a position. Decision: OD138, Q4.2,
docs/decisions.md#decided-2026-11-08-language-revisions. -/
def ExpectedTypeLiteral (suffix : Option ConcreteSpec.IntTy) : Prop := suffix = none

/-- A local reference resolves in the bindings already in scope. Decision: Q8.3,
docs/decisions.md#decided-2026-11-25-freeze-and-grammar. -/
def LocalBound (input : String × List String) : Prop := input.1 ∈ input.2

/-- Reserved intrinsic names, including the lossless conversion `widen`. The shared
surface catalog records each spelling and its decision. -/
abbrev intrinsicNames : List String := ConcreteSpec.Surface0.intrinsicNames

/-- A binding may not reuse a local already in scope. Decision: OD67 (SCOPE-1). -/
def FreshLocal (input : String × List String) : Prop := input.1 ∉ input.2

/-- A local may not shadow the entry function, a builtin type or an intrinsic. Decision: OD67, OD153,
docs/decisions.md#decided-2026-11-27-token-ballot. -/
def UnreservedLocal (name : String) : Prop := UnreservedLocalName name

/-- Slice 0 binds a name rather than a wildcard. Decision: OD62, Q8.3,
docs/decisions.md#decided-slice0-surface-2026-11-06. -/
def NamedBinding (name : String) : Prop := name ≠ "_"

/-- Slice-0 integer atoms use decimal syntax. Decision: Q8.3,
docs/decisions.md#decided-slice0-surface-2026-11-06. -/
def DecimalLiteral (radix : Radix) : Prop := radix = .decimal

/-- The refusal grammar marks productions outside the slice-0 fragment. This rule
checks one production; unsupported-form classification requires a complete parse.
Decision: Q8.3, docs/decisions.md#decided-slice0-surface-2026-11-06. -/
def AdmittedProduction
    (production : Nonterminal × List (Concrete.LL1.Sym Terminal Nonterminal)) : Prop :=
  (projectedProductions .shared).lookup production = some false

/-- Slice-0 literal typing delegates to the Core atom judgment (ARITH-2).
Decision: Q6.1, docs/decisions.md#decided-project-no-hidden-traps. -/
def LiteralTyped (n : Nat) : Prop :=
  ConcreteSpec.AtomTy [] (.int .u8 n) (.int .u8)

/-- Slice-0 admission delegates to the Core declarative checker contract.
Decision: Q6.1, docs/decisions.md#decided-project-no-hidden-traps. -/
def Admitted (program : ConcreteSpec.Slice0.Program) : Prop :=
  ConcreteSpec.Checked (ConcreteSpec.Slice0.toCore program)

/-- OWN-5 requires every named local to have been used at return (OD62,
docs/decisions.md#od62). In a slice-0 context all locals are Copy, so after accounting
for the return atom's read, the Core return discharge checks their use. -/
def LocalsUsed (input : ConcreteSpec.Ctx × ConcreteSpec.Slice0.Atom) : Prop :=
  ∃ final, ConcreteSpec.Uses [] input.1 (.atom input.2.toCore) final ∧
    ConcreteSpec.Discharged [] final

/-- Whether a slice-0 checked addition produces an integer result.
Decision: Q6.1, docs/decisions.md#decided-project-no-hidden-traps. -/
def AdditionFits (operands : Int × Int) : Prop :=
  IntTy.u8.InRange operands.1 ∧ IntTy.u8.InRange operands.2 ∧
    checked .u8 .add operands.1 operands.2 = .ok (operands.1 + operands.2)

/-- Inclusive accounting rule for R-0528 resource limits. The caller supplies a measured
quantity and a limit in the same unit. E7 leaves the numeric scan limit, its unit and refusal
precedence open. This rule also leaves driver enforcement to A14. Decision: R-0528. -/
def WithinLimit (measuredAndLimit : Nat × Nat) : Prop :=
  measuredAndLimit.1 ≤ measuredAndLimit.2

end ConcreteSpec.Core.Slice0Rules
