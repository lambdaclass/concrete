Restore the Lean-derived diagnostic rule registry tooling with an empty complete registry,
reusing `RuleId` and Lean declaration names for structured conformance links. The exporter
checks elaborated theorem evidence and rule dependencies; Python validates its closed schema,
documentation, decision citations and paired coverage. Production and all fixtures run through
Lean before validation, including invalid-proof and wrong-rule controls. The Lean-dependent gate
runs in the Spec CI job, alongside site extraction. Root checks remain independent of the Lean
toolchain, like the source-only import gate.

Conformance verdicts now come from `RuleAccepted rule input` or `RuleRefused rule input`
theorem propositions, with exact owning-rule identity and closed constructor/literal inputs.
Controls reject tautologies, acceptance substituted for refusal, and parameterized inputs;
the production validator path also exercises unreadable input and double-timeout INCOMPLETE.
The site counts registered named conformance theorems as examples and marks their existing
Lean declaration links using the same registry.

Regenerate the proof axiom inventory for the generated `RuleExample` structure declarations.
