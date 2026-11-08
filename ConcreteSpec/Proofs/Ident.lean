import ConcreteSpec.Spec.Ident

/-!
# Laws of typed and persistent identities

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). See `ConcreteSpec/README.md`.
-/

namespace ConcreteSpec.Core

/-- **Lexical renumbering preserves persistent identity.** Every injective renumbering of `SiteId`s
leaves the function-relative stable key of every site unchanged (R-0492). -/
theorem StableSiteKey.stable_renumber (r : SiteRenumbering) (k : StableSiteKey) :
    (k.renumber r).key = k.key := rfl

namespace Vacuity

/-- An enclosing function used by the persistent-key examples. -/
def function : FnId := ⟨⟨["demo"]⟩, "f"⟩

/-- A site whose lexical and persistent identities are deliberately different representations. -/
def site : StableSiteKey := ⟨⟨0⟩, ⟨function, ⟨"body/add/left"⟩⟩⟩

/-- Successor is an injective lexical renumbering. -/
def successor : SiteRenumbering := ⟨Nat.succ, fun _ _ h => Nat.succ.inj h⟩

/-- Non-trivial instance for `StableSiteKey.stable_renumber`: the lexical site changes from zero to
one while its exact stable key is retained. -/
example : (site.renumber successor).site ≠ site.site ∧
    (site.renumber successor).key = site.key := by decide

/-- A planted mutant that renumbers the persistent token together with the lexical site. -/
def renumberAndMutate (r : SiteRenumbering) (k : StableSiteKey) : StableSiteKey :=
  { k.renumber r with key := { k.key with relative := ⟨k.key.relative.token ++ "/renumbered"⟩ } }

/-- Planted counter-example: changing persistent data during renumbering violates the stability law. -/
example : (renumberAndMutate successor site).key ≠ site.key := by decide

/-- Trait instances use the same separate lexical/persistent binding without changing `InstanceId`. -/
example : StableInstanceKey.mk ⟨0⟩ ⟨function, ⟨"instances/0"⟩⟩ ≠
    StableInstanceKey.mk ⟨1⟩ ⟨function, ⟨"instances/0"⟩⟩ := by decide

end Vacuity
end ConcreteSpec.Core
