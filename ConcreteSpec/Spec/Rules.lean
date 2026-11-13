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

/-- A conformance proof's verdict relative to its linked rule. -/
inductive RuleVerdict where
  | accepted
  | refused

/-- A structured link to a named conformance proof. The exporter checks that the proof exists,
has no nonstandard axioms, and mentions its rule in its type or proof dependencies. -/
structure RuleExample where
  rule : Lean.Name
  declaration : Lean.Name
  verdict : RuleVerdict

/-- Complete conformance links, populated alongside the owning rules. -/
def registeredRuleExamples : List RuleExample := []

end ConcreteSpec.Core
