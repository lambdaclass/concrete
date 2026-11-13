import ConcreteSpec.Spec.Ident

/-!
# Diagnostic rule registry

A diagnostic cites the Lean declaration that states its rule and that rule's own revision
(`RuleId`, defined in `Ident`). This is the complete registry declaration; it starts empty.
Later pieces register rules when their definitions and evidence land, importing their owning
modules here. An entry records identity, not approval or a second definition of the rule.

`scripts/spec/rules.sh` reads entries and documentation from the built Lean environment. It
requires a declared decision-row citation and separate accepted and refused example comments
for each entry, and rejects citations of unregistered rules throughout the specification.
-/

namespace ConcreteSpec.Core

/-- Diagnostic rule identities, in specification order. No rules are registered by this piece. -/
def registeredRules : List RuleId := []

end ConcreteSpec.Core
