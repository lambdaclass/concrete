import ConcreteSpec.Spec.Basics

/-! Per-program diagnostics ownership, outside the semantic specification. -/
namespace ConcreteSpec

/-- Elaboration output for one program or function, together with its diagnostic spellings.
Semantic operations consume `code` only. -/
structure Elaborated (α : Type) where
  code : α
  names : LocalNames


end ConcreteSpec
