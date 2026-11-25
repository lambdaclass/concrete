import Concrete.MIR

namespace Concrete.Memory

/-- Each place lives in its own logical allocation. Slice 0 stores one byte at offset zero. -/
structure Layout where
  size : Nat
  alignment : Nat
  deriving DecidableEq, Repr

def u8Layout : Layout := ⟨1, 1⟩

structure Slot where
  id : ConcreteSpec.Var
  layout : Layout

/-- Store to a complete slot. The local operands of the RHS read their complete slots.
The addition retains its normal and exceptional outcomes. -/
structure Store where
  dest : Slot
  value : MIR.Op

structure Program where
  stores : List Store
  result : Core.Atom

def lower (p : MIR.Program) : Program :=
  ⟨p.body.map (fun stmt => ⟨⟨stmt.dest, u8Layout⟩, stmt.op⟩), p.result⟩

-- The pipeline retains MIR for its artifacts. Borrow its input spine while constructing
-- the output so traversing a shared list does not retain and release each suffix.
private def lowerStores (body : @& List MIR.Assignment) (out : List Store) : List Store :=
  match body with
  | [] => out.reverse
  | stmt :: rest => lowerStores rest (⟨⟨stmt.dest, u8Layout⟩, stmt.op⟩ :: out)

private theorem lowerStores_eq (xs : List MIR.Assignment) (out : List Store) :
    lowerStores xs out =
      out.reverse ++ xs.map (fun stmt => ⟨⟨stmt.dest, u8Layout⟩, stmt.op⟩) := by
  induction xs generalizing out with
  | nil => simp [lowerStores]
  | cons x xs ih => simp [lowerStores, ih, List.append_assoc]

private def lowerBorrowed (p : @& MIR.Program) : Program :=
  ⟨lowerStores p.body [], p.result⟩

@[csimp] private theorem lower_eq_lowerBorrowed : @lower = @lowerBorrowed := by
  funext p
  simp [lower, lowerBorrowed, lowerStores_eq]

private def toMIR (p : Program) : MIR.Program :=
  ⟨p.stores.map (fun store => ⟨store.dest.id, store.value⟩), p.result⟩

theorem code_lower (p : MIR.Program) : toMIR (lower p) = p := by
  cases p
  simp [toMIR, lower, List.map_map, Function.comp_def]

def run (p : Program) : ConcreteSpec.Outcome := MIR.run (toMIR p)

theorem lower_correct (p : MIR.Program) : run (lower p) = MIR.run p := by
  rw [run, code_lower]

/-- The admitted lowering stages preserve the Core outcome through Memory. -/
theorem lower_admitted_correct {source admitted mir} (ha : Admission.check source = .ok admitted)
    (hl : MIR.lower admitted = .ok mir) :
    let p := Core.toCore source.program
    run (lower mir) = Core.run p (p.body.length + 1) :=
  (lower_correct mir).trans (MIR.lower_correct ha hl)

/-- The unchecked lowering stages preserve the Core outcome through Memory. -/
theorem lower_unchecked_correct {source mir} (hl : MIR.lowerUnchecked source = .ok mir) :
    let p := Core.toCore source.program
    run (lower mir) = Core.run p (p.body.length + 1) :=
  (lower_correct mir).trans (MIR.lowerUnchecked_correct hl)

end Concrete.Memory
