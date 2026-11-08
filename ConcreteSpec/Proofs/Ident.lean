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

/-- **Specialization identity is request-site independent.** Equal generic and argument lists name
the same specialization no matter which call sites request it. -/
theorem SpecializationRequest.site_independent {Ty Cap : Type} (first second : SiteId)
    (generic : GenericId) (typeArguments : List Ty) (capabilityArguments : List Cap) :
    (SpecializationRequest.for first generic typeArguments capabilityArguments).specialization =
      (SpecializationRequest.for second generic typeArguments capabilityArguments).specialization := rfl

/-- Different type arguments name different specializations. -/
theorem SpecializationId.ne_of_typeArguments_ne {Ty Cap : Type}
    (first second : SpecializationId Ty Cap) (h : first.typeArguments ≠ second.typeArguments) :
    first ≠ second := by
  intro he
  exact h (congrArg (fun s : SpecializationId Ty Cap => s.typeArguments) he)

/-- Different capability arguments also name different specializations. -/
theorem SpecializationId.ne_of_capabilityArguments_ne {Ty Cap : Type}
    (first second : SpecializationId Ty Cap)
    (h : first.capabilityArguments ≠ second.capabilityArguments) : first ≠ second := by
  intro he
  exact h (congrArg (fun s : SpecializationId Ty Cap => s.capabilityArguments) he)

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

/-- Three specializations of one generic, differing only in one argument list at a time. -/
def numeric : SpecializationId Nat String := ⟨.function function, [0], ["Fs"]⟩
def otherType : SpecializationId Nat String := ⟨.function function, [1], ["Fs"]⟩
def otherCapability : SpecializationId Nat String := ⟨.function function, [0], ["Console"]⟩

/-- Non-trivial instance for `SpecializationRequest.site_independent`: distinct sites request the
same numeric specialization and obtain equal identities. -/
example : (⟨0⟩ : SiteId) ≠ ⟨1⟩ ∧
    (SpecializationRequest.for ⟨0⟩ (.function function) [0] ["Fs"]).specialization =
      (SpecializationRequest.for ⟨1⟩ (.function function) [0] ["Fs"]).specialization := by
  exact ⟨by decide, SpecializationRequest.site_independent ⟨0⟩ ⟨1⟩ (.function function) [0] ["Fs"]⟩

/-- Non-trivial instances for the argument-difference laws. -/
example : numeric ≠ otherType :=
  SpecializationId.ne_of_typeArguments_ne numeric otherType (by decide)

example : numeric ≠ otherCapability :=
  SpecializationId.ne_of_capabilityArguments_ne numeric otherCapability (by decide)

/-- Planted counter-example for site independence: adding the request site to the identity splits
one specialization into two. -/
def withRequestSite {Ty Cap : Type} (site : SiteId) (s : SpecializationId Ty Cap) :=
  (s, site)

example : withRequestSite ⟨0⟩ numeric ≠ withRequestSite ⟨1⟩ numeric := by decide

/-- Planted counter-example for the argument laws: keeping only the generic equates identities that
the real rules distinguish. -/
def onlyGeneric {Ty Cap : Type} (s : SpecializationId Ty Cap) : GenericId := s.generic

example : onlyGeneric numeric = onlyGeneric otherType ∧ numeric ≠ otherType := by
  exact ⟨rfl, SpecializationId.ne_of_typeArguments_ne numeric otherType (by decide)⟩

/-- A source `assume` and a package-level stack assumption with stable, typed identities. -/
def sourceAssume : PremiseId := ⟨.assume, .function ⟨function, ⟨"assume/0"⟩⟩⟩
def stackAssumption : PremiseId := ⟨.stack, .package ⟨"p1"⟩ "defaultStack"⟩

/-- Accepted: both decided premise scopes are represented without bare-string keys. -/
example : sourceAssume.kind = .assume ∧ stackAssumption.kind = .stack := ⟨rfl, rfl⟩

/-- Refused as an identity match: changing the premise kind changes the premise identity. -/
example : sourceAssume ≠ { sourceAssume with kind := .trustedSurface } := by decide

/-- Two facts with the same conclusion but different premise dependencies. -/
def conditional : ConditionalFact String := ⟨"bounded", [sourceAssume]⟩
def unconditional : ConditionalFact String := ⟨"bounded", []⟩

/-- Planted counter-example: comparing only conclusions loses the conditional fact's premise debt. -/
def conclusionOnly (first second : ConditionalFact String) : Prop :=
  first.conclusion = second.conclusion

example : conclusionOnly conditional unconditional ∧ conditional ≠ unconditional := by
  exact ⟨rfl, by decide⟩

end Vacuity
end ConcreteSpec.Core
