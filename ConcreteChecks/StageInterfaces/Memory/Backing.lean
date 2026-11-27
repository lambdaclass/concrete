import ConcreteChecks.StageInterfaces.Memory.Fixtures

open Concrete
open Concrete.Interfaces
open ConcreteSpec Core Core.SourcePathSpan StageInterface StageGrammar

namespace StageInterfaceControls

theorem emptyBytesWF : (Mem.Memory.empty).WellFormed testProfile.memoryTarget :=
  ⟨nofun, by decide, nofun, nofun, nofun, nofun, nofun, nofun⟩

theorem storedBytesWF (space : AddrSpace) (base : Nat) (fits : backingFits space base)
    (byte : Fin 256 := 42) :
    (backingAfter space base byte).bytes.WellFormed testProfile.memoryTarget := by
  let stored := {Mem.candidate space base 1 1 with bytes := [.init byte none]}
  have lookup : ∀ block value, (backingAfter space base byte).bytes.block block = some value →
      block = ⟨0⟩ ∧ value = stored := by
    intro block value found
    change (if (⟨0⟩ : BlockId) = block then some stored else none) = some value at found
    split at found
    · rename_i eq
      exact ⟨eq.symm, (Option.some.inj found).symm⟩
    · contradiction
  refine ⟨?_, (by simp : [_].Nodup), ?_, ?_, ?_, ?_, ?_, ?_⟩
  · intro entry member
    change entry ∈ [(⟨0⟩, stored)] at member
    cases List.mem_singleton.mp member
    rfl
  · intro block value found
    rcases lookup block value found with ⟨rfl, rfl⟩
    exact Nat.zero_lt_one
  · intro block value found
    rcases lookup block value found with ⟨_, rfl⟩
    rfl
  · intro block value found
    rcases lookup block value found with ⟨_, rfl⟩
    exact fits.2.2.1
  · intro block value found
    rcases lookup block value found with ⟨_, rfl⟩
    exact fits.2.2.2.2.2.2
  · intro block value found
    rcases lookup block value found with ⟨_, rfl⟩
    exact ⟨fits.1, fits.2.1⟩
  · intro b x c y first second distinct _ _
    exact (distinct ((lookup b x first).1.trans (lookup c y second).1.symm)).elim

theorem allocatedBytesWF (space : AddrSpace) (base : Nat) (fits : backingFits space base) :
    (backingBefore space base).bytes.WellFormed testProfile.memoryTarget := by
  -- The allocation has the same geometry and live block as the initialized snapshot.
  let allocated := Mem.candidate space base 1 1
  have lookup : ∀ block value, (backingBefore space base).bytes.block block = some value →
      block = ⟨0⟩ ∧ value = allocated := by
    intro block value found
    change (if (⟨0⟩ : BlockId) = block then some allocated else none) = some value at found
    split at found
    · rename_i eq
      exact ⟨eq.symm, (Option.some.inj found).symm⟩
    · contradiction
  refine ⟨?_, (by simp : [_].Nodup), ?_, ?_, ?_, ?_, ?_, ?_⟩
  · intro entry member
    change entry ∈ [(⟨0⟩, allocated)] at member
    cases List.mem_singleton.mp member
    rfl
  · intro block value found
    rcases lookup block value found with ⟨rfl, rfl⟩
    exact Nat.zero_lt_one
  · intro block value found
    rcases lookup block value found with ⟨_, rfl⟩
    rfl
  · intro block value found
    rcases lookup block value found with ⟨_, rfl⟩
    exact fits.2.2.1
  · intro block value found
    rcases lookup block value found with ⟨_, rfl⟩
    exact fits.2.2.2.2.2.2
  · intro block value found
    rcases lookup block value found with ⟨_, rfl⟩
    exact ⟨fits.1, fits.2.1⟩
  · intro b x c y first second distinct _ _
    exact (distinct ((lookup b x first).1.trans (lookup c y second).1.symm)).elim

end StageInterfaceControls
