import ConcreteSpec.Spec.Scalar
import ConcreteSpec.Spec.Target

/-! The layout interface for A9(a). Logical places have identity and lifetime even when their
layout requires no storage. The Memory stage owns the transitions on those places. -/

namespace ConcreteSpec.Core.Slice0Layout

inductive Shape where
  | integer (ty : IntTy)
  | logical
  deriving DecidableEq, Repr

inductive Layout where
  | byte
  | logical
  deriving DecidableEq, Repr

def Layout.byteSize : Layout → Nat
  | .byte => 1
  | .logical => 0

/-- Query the slice-0 layout using the selected target's object bound (Q3.10, OD126).
The current integer operations use only a single byte, whose order is independent of endianness.
A logical place has no materialized address or alignment requirement in this interface. -/
def query (target : Target) : Shape → Option Layout
  | .integer .u8 => if 1 ≤ target.maxObject then some .byte else none
  | .integer _ => none
  | .logical => some .logical

theorem integer_layout (target : Target) (h : 1 ≤ target.maxObject) :
    query target (.integer .u8) = some .byte := by simp [query, h]

theorem logical_layout (target : Target) : query target .logical = some .logical := rfl

theorem layout_fits {target : Target} {shape : Shape} {layout : Layout}
    (h : query target shape = some layout) : layout.byteSize ≤ target.maxObject := by
  cases shape with
  | logical =>
    cases h
    exact Nat.zero_le _
  | integer ty =>
    cases ty <;> try contradiction
    simp only [query] at h
    split at h
    · cases h
      assumption
    · contradiction

end ConcreteSpec.Core.Slice0Layout
