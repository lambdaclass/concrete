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

private def toMIR (p : Program) : MIR.Program :=
  ⟨p.stores.map (fun store => ⟨store.dest.id, store.value⟩), p.result⟩

theorem code_lower (p : MIR.Program) : toMIR (lower p) = p := by
  cases p
  simp [toMIR, lower, List.map_map, Function.comp_def]

def run (p : Program) : ConcreteSpec.Outcome := MIR.run (toMIR p)

theorem lower_correct (p : MIR.Program) : run (lower p) = MIR.run p := by
  rw [run, code_lower]

end Concrete.Memory
