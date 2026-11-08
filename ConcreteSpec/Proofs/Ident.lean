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

/-- **OD44 refuses a named collision.** If both modules of a cross-package top-level collision are
loaded, the program is not well-formed; the witness retains both module and package identities. -/
theorem LoadedModules.collision_refuses (modules : List ModuleId) (collision : ModuleCollision)
    (h : LoadedModules.HasCollision modules collision) : ¬ LoadedModules.WellFormed modules := by
  intro hwf
  exact collision.differentPackages
    (hwf collision.first h.1 collision.second h.2 collision.sameTopLevel)

namespace Vacuity

/-- An enclosing function used by the persistent-key examples. -/
def function : FnId := FnId.mk (ModuleId.mk ⟨"demo"⟩ ["demo"]) "f"

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

/-- Two modules in one package, with distinct top-level names. -/
def firstModule : ModuleId := ⟨⟨"p1"⟩, ["first"]⟩
def secondModule : ModuleId := ⟨⟨"p1"⟩, ["second"]⟩

/-- A different package defining the first module's top-level name. -/
def collidingModule : ModuleId := ⟨⟨"p2"⟩, ["first"]⟩

def collision : ModuleCollision := ⟨firstModule, collidingModule, rfl, by decide⟩

/-- Accepted: distinct top-level names in one package are well-formed. -/
example : LoadedModules.WellFormed [firstModule, secondModule] := by
  intro a ha b hb htop
  simp only [List.mem_cons, List.not_mem_nil, or_false] at ha hb
  rcases ha with rfl | rfl <;> rcases hb with rfl | rfl <;> rfl

/-- Non-trivial instance for `LoadedModules.collision_refuses`: the collision witness is in the load
set and the real predicate refuses it, naming both packages. -/
example : LoadedModules.HasCollision [firstModule, collidingModule] collision ∧
    ¬ LoadedModules.WellFormed [firstModule, collidingModule] := by
  have hc : LoadedModules.HasCollision [firstModule, collidingModule] collision :=
    ⟨by decide, by decide⟩
  exact ⟨hc, LoadedModules.collision_refuses [firstModule, collidingModule] collision hc⟩

/-- The rejected OD44 option (b): a planted coexistence rule accepts the same collision. -/
def Coexistence (_modules : List ModuleId) : Prop := True

example : ¬ LoadedModules.WellFormed [firstModule, collidingModule] ∧
    Coexistence [firstModule, collidingModule] := by
  have hc : LoadedModules.HasCollision [firstModule, collidingModule] collision :=
    ⟨by decide, by decide⟩
  exact ⟨LoadedModules.collision_refuses [firstModule, collidingModule] collision hc, trivial⟩

end Vacuity
end ConcreteSpec.Core
