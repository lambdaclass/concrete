### Shared capability base

Move the existing capability enumeration and list into `ConcreteSpec.Spec.CapabilityBase`,
with no explicit imports. Authority imports the base and preserves the same public names and
meaning. Extend the specification import gate to reject any import from the capability base,
keeping future grant and primitive models independent of the higher authority model.
