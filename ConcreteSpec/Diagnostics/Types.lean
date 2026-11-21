import ConcreteSpec.Spec.Basics
import ConcreteSpec.Spec.Ident

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

namespace ConcreteSpec.Core.SourcePath

/-- UTF-8 adapter for callers that already have a host string. -/
def parseString (raw : String) : Except Refusal SourcePath :=
  parse (SourceText.ofBytes raw.toByteArray.data.toList)

end ConcreteSpec.Core.SourcePath
