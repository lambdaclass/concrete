import ConcreteSpec.Diagnostics.Types

/-!
# Diagnostics ownership

An elaborated program owns its spelling table. Semantic operations consume its `code`;
the required local_identity gate enforces this separation structurally.
-/
