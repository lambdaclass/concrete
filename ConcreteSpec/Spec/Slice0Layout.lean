import ConcreteSpec.Spec.Scalar
import ConcreteSpec.Spec.Target

/-! The layout interface for A9(a). Logical places have identity and lifetime even when their
layout requires no storage. The Memory stage owns the transitions on those places. -/

namespace ConcreteSpec.Core.Slice0Layout

inductive Shape where
  | integer (ty : IntTy)
  | logical
  deriving DecidableEq, Repr

/-- Integer alignment is a profile fact (OD126), subject to the storage layout laws.
The current hosted profile uses natural alignment for its integer types. -/
structure Profile where
  target : Target
  wellFormed : target.WellFormed
  intAlign : IntTy → Nat
  intAlignPow2 : ∀ ty, IsPow2 (intAlign ty)
  intSizeAligned : ∀ ty, ty.byteSize % intAlign ty = 0

def linuxX8664Profile : Profile where
  target := linuxX8664
  wellFormed := example_existing_profile_is_well_formed
  intAlign := IntTy.byteSize
  intAlignPow2 := by intro ty; cases ty <;> decide
  intSizeAligned := by intro ty; cases ty <;> decide

/-- The data needed to materialize a stored integer. The index binds byte order and the
object bound to the same target used for memory access. -/
structure Storage (target : Target) where
  byteSize : Nat
  align : Nat
  positive : 0 < byteSize
  alignPow2 : IsPow2 align
  sizeAligned : byteSize % align = 0
  fits : byteSize ≤ target.maxObject

def Storage.byteOrder {target : Target} (_ : Storage target) : ByteOrder := target.byteOrder

inductive Layout (target : Target) where
  | stored (storage : Storage target)
  /-- Zero bytes, no address, and no physical alignment requirement. -/
  | logical

def Layout.byteSize {target : Target} : Layout target → Nat
  | .stored storage => storage.byteSize
  | .logical => 0

/-- A valid target can still refuse an integer wider than its object bound (Q3.10). -/
def query (profile : Profile) : Shape → Option (Layout profile.target)
  | .logical => some .logical
  | .integer ty =>
    if fits : ty.byteSize ≤ profile.target.maxObject then
      some (.stored ⟨ty.byteSize, profile.intAlign ty, by cases ty <;> decide,
        profile.intAlignPow2 ty, profile.intSizeAligned ty, fits⟩)
    else none

theorem logical_layout (profile : Profile) : query profile .logical = some .logical := rfl

theorem layout_fits {target : Target} (layout : Layout target) :
    layout.byteSize ≤ target.maxObject := by
  cases layout with
  | stored storage => exact storage.fits
  | logical => exact Nat.zero_le _

end ConcreteSpec.Core.Slice0Layout
