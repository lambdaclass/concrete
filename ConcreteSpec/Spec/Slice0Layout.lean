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

/-- Changing storage byte order preserves every size, alignment and bound. -/
def Profile.withByteOrder (profile : Profile) (order : ByteOrder) : Profile :=
  { profile with target := {profile.target with byteOrder := order}, wellFormed := profile.wellFormed }

/-- Geometry shared by the layout query and declared byte-storage layouts. -/
structure StorageGeometry (target : Target) (byteSize align : Nat) : Prop where
  alignPow2 : IsPow2 align
  sizeAligned : byteSize % align = 0
  fits : byteSize ≤ target.maxObject

instance (target : Target) (size align : Nat) : Decidable (StorageGeometry target size align) :=
  decidable_of_iff (IsPow2 align ∧ size % align = 0 ∧ size ≤ target.maxObject)
    ⟨fun ⟨p, s, f⟩ => ⟨p, s, f⟩, fun g => ⟨g.alignPow2, g.sizeAligned, g.fits⟩⟩

/-- The data needed to materialize a stored integer. -/
structure Storage (target : Target) where
  byteSize : Nat
  align : Nat
  positive : 0 < byteSize
  geometry : StorageGeometry target byteSize align

def Storage.byteOrder {target : Target} (_ : Storage target) : ByteOrder := target.byteOrder

inductive Layout (target : Target) where
  | stored (storage : Storage target)
  /-- Zero bytes, no address, and no physical alignment requirement. -/
  | logical

def Layout.byteSize {target : Target} : Layout target → Nat
  | .stored storage => storage.byteSize
  | .logical => 0

/-- Native aggregate alignment is the maximum field alignment with identity
one (foundation C3). A logical empty root imposes no alignment on a byte block. -/
def Layout.alignment {target : Target} : Layout target → Nat
  | .stored s => s.align
  | .logical => 1

/-- A valid target can still refuse an integer wider than its object bound (Q3.10). -/
def query (profile : Profile) : Shape → Option (Layout profile.target)
  | .logical => some .logical
  | .integer ty =>
    if fits : ty.byteSize ≤ profile.target.maxObject then
      some (.stored ⟨ty.byteSize, profile.intAlign ty, by cases ty <;> decide,
        ⟨profile.intAlignPow2 ty, profile.intSizeAligned ty, fits⟩⟩)
    else none

theorem logical_layout (profile : Profile) : query profile .logical = some .logical := rfl

theorem layout_fits {target : Target} (layout : Layout target) :
    layout.byteSize ≤ target.maxObject := by
  cases layout with
  | stored storage => exact storage.geometry.fits
  | logical => exact Nat.zero_le _

end ConcreteSpec.Core.Slice0Layout
