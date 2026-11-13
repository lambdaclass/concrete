import ConcreteSpec.Diagnostics.ExecExamples
import ConcreteSpec.Diagnostics.Types
import ConcreteSpec.Diagnostics.Repr

/-!
# Diagnostics ownership

An elaborated program owns its spelling table. Semantic operations consume its `code`;
the required local_identity gate enforces this separation structurally.
-/
