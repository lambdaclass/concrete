import ConcreteSpec.Diagnostics.Types
import ConcreteSpec.Spec.Exec

/-! Numeric-local execution fixtures with per-program diagnostic names.
Fresh identities: docs/decisions.md#decided-2026-11-05-core, Q9.1.
Numeric equality is reused unchanged from the landed Basics foundation.
The spelling tables stay outside the semantic module and are never passed to execution.
-/
namespace ConcreteSpec.Diagnostics.ExecExamples
open Core Core.Exec Core.Exec.Demo

/-- Assignment and reads use numbers, even when two names are exchanged. -/
def named : Elaborated (Stmt ops) :=
  ⟨.seq (.bind x (.lit 7) s) (.seq (.assign x (.lit 8) s) (.ret (.var x) s)),
    ⟨[(x, "first"), (y, "second")]⟩⟩

-- Instance: concrete numeric-local execution case.
example : brief (go named.code) = ([], some (.returned 8)) := by decide
-- Instance: concrete numeric-local execution case.
example : brief (go ({ named with names := ⟨[(x, "second"), (y, "first")]⟩ }).code) =
    ([], some (.returned 8)) := by decide

/-- The same displayed name cannot merge distinct numeric locals. -/
-- Instance: concrete numeric-local execution case.
example : brief (go (⟨.seq (.bind x (.lit 7) s)
    (.seq (.bind y (.lit 8) s) (.ret (.var x) s)),
      ⟨[(x, "same"), (y, "same")]⟩⟩ : Elaborated (Stmt ops)).code) =
    ([], some (.returned 7)) := by decide

/-- Scope removal preserves the outer identity; spelling does not save a dead local. -/
-- Instance: concrete numeric-local execution case.
example : brief (go (⟨.seq (.bind x (.lit 7) s)
    (.seq (.block ⟨1⟩ (.bind y (.lit 8) s)) (.ret (.var x) s)),
      ⟨[(x, "same"), (y, "same")]⟩⟩ : Elaborated (Stmt ops)).code) =
    ([], some (.returned 7)) := by decide
-- Instance: concrete numeric-local execution case.
example : isStuck (go (⟨.seq (.block ⟨1⟩ (.bind y (.lit 8) s)) (.ret (.var y) s),
    ⟨[(y, "renamed")]⟩⟩ : Elaborated (Stmt ops)).code) = true := by decide

/-- A move records numeric identity, independently of the displayed name. -/
-- Instance: concrete numeric-local execution case.
example : isStuck (go (⟨.seq (.bind x (.lit 7) s)
    (.seq (.call none logF [.move x] s) (.ret (.var x) s)),
      ⟨[(x, "renamed")]⟩⟩ : Elaborated (Stmt ops)).code) = true := by decide

end ConcreteSpec.Diagnostics.ExecExamples
