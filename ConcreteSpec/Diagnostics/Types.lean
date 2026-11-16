import ConcreteSpec.Spec.Basics

/-! Per-program diagnostics ownership, outside the semantic specification. -/
namespace ConcreteSpec

/-- Source spellings for diagnostics only. For example, `⟨[(⟨7⟩, "count")]⟩` displays
local 7 as `count`. This table belongs to one elaborated program or function, outside its semantic state. -/
structure LocalNames where
  /-- Diagnostic entries; for example, `[(⟨7⟩, "count")]`. -/
  entries : Core.AList Var String

/-- Elaboration output for one program or function, together with its diagnostic spellings.
Semantic operations consume `code` only. For example, `Elaborated Nat` may be
`⟨42, ⟨[(⟨7⟩, "count")]⟩⟩`. -/
structure Elaborated (α : Type) where
  code : α
  names : LocalNames


end ConcreteSpec
