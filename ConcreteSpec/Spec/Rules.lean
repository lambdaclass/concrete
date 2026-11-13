import ConcreteSpec.Spec.Ident

/-!
# Diagnostic rule registry

A diagnostic cites the Lean declaration that states its rule and that rule's own revision
(`RuleId`, defined in `Ident`). This is the complete registry declaration; it starts empty.
Later pieces register rules when their definitions and evidence land, importing their owning
modules here. An entry records identity, not approval or a second definition of the rule.

`scripts/spec/rules.sh` reads entries and documentation from the built Lean environment. It
requires a declared decision-row citation and separate elaborated accepted and refused proofs
for each entry. Example links use Lean declaration names, the identities used by the site.
-/

namespace ConcreteSpec.Core

/-- Diagnostic rule identities, in specification order. No rules are registered by this piece. -/
def registeredRules : List RuleId := []

/-- Acceptance evidence applies the owning rule's judgment to a concrete input.
The exporter recognizes this proposition's elaborated type, without unfolding the wrapper. -/
def RuleAccepted {Input : Sort _} (rule : Input → Prop) (input : Input) : Prop :=
  rule input

/-- Refusal evidence proves the negation of the same judgment on a concrete input. -/
def RuleRefused {Input : Sort _} (rule : Input → Prop) (input : Input) : Prop :=
  ¬ rule input

/-- A named conformance theorem linked to its owning rule. Its proposition supplies the verdict;
there is no separately assigned verdict. Inputs must normalize to closed constructor/literal data,
with no parameters, axioms, opaque computations or unresolved variables. -/
structure RuleExample where
  rule : Lean.Name
  declaration : Lean.Name

/-- Complete conformance links, populated alongside the owning rules. -/
def registeredRuleExamples : List RuleExample := []

end ConcreteSpec.Core
