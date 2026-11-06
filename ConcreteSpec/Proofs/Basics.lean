import ConcreteSpec.Spec

/-!
# Lemmas about lists keyed by name.

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). See `ConcreteSpec/README.md`.
-/

namespace ConcreteSpec

theorem lookupF_mem {κ α : Type} [DecidableEq κ] {x : κ} {a : α} :
    ∀ {l : List (κ × α)}, lookupF x l = some a → (x, a) ∈ l
  | [], h => by simp [lookupF] at h
  | (y, b) :: rest, h => by
    unfold lookupF at h
    split at h
    · cases h; subst_vars; exact List.Mem.head _
    · exact List.Mem.tail _ (lookupF_mem h)

theorem lookupF_of_mem {κ α : Type} [DecidableEq κ] {x : κ} {a : α} :
    ∀ {l : List (κ × α)}, (x, a) ∈ l → ∃ b, lookupF x l = some b
  | [], h => by cases h
  | (y, b) :: rest, h => by
    unfold lookupF
    split
    · exact ⟨b, rfl⟩
    · cases h with
      | head => contradiction
      | tail _ h => exact lookupF_of_mem h

end ConcreteSpec
