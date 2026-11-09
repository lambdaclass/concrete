import ConcreteSpec.Proofs.Checker
import ConcreteSpec.Proofs.Rep
import ConcreteSpec.Proofs.Cleanup
import ConcreteSpec.Proofs.Borrow
import ConcreteSpec.Proofs.Brand

/-!
# The examples of the specification derived from the general theorems (non-vacuity witnesses)

The cleanup traces and the brand refusal that the specification states directly are derived here again from
`defer_cleanup_releases` and `brand_mismatch_refused`, which shows that those theorems apply to
them.

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). See `ConcreteSpec/README.md`.
-/

namespace ConcreteSpec.Examples
open ConcreteSpec

/-- Helper (computation). Closed checker computation establishing the premise for the parametric input example. (approved: REV-base1a-agy-01025713b). -/
theorem add0_ok : BodyOk decls inputCtx add0 (.copy s₂) := bodyOkB_iff.mp (by decide)

/-- Law (subject `ConcreteSpec.runBody`). For every input value the accepted body add0 neither gets stuck nor traps: the input is
bounded only by its type, and that is the evidence that admits the sum. -/
theorem add0_never_stuck (n : Int) (h : IntTy.u8.inRange n = true) (fuel : Nat) (w : Stuck) :
    runBody decls fuel (inputEnv n) add0 (.copy s₂) ≠ .stuck w :=
  runBody_safe add0_ok
    (.cons (fun _ => ⟨_, rfl, .scalar (.int h)⟩) (fun _ hs => by cases hs) (fun _ _ h => by cases h) .nil)
    fuel w

/-- Law (subject `ConcreteSpec.runBody`). -/
theorem add0_never_traps (n : Int) (h : IntTy.u8.inRange n = true) (fuel : Nat) (k : Trap) :
    runBody decls fuel (inputEnv n) add0 (.copy s₂) ≠ .trap k :=
  runBody_ne_trap add0_ok
    (.cons (fun _ => ⟨_, rfl, .scalar (.int h)⟩) (fun _ hs => by cases hs) (fun _ _ h => by cases h) .nil)
    fuel k

namespace Add0Evidence

def stuckBody : Decls → Nat → Env → List Stmt → Atom → Outcome := fun _ _ _ _ _ => .stuck .movedRead
def trapBody : Decls → Nat → Env → List Stmt → Atom → Outcome := fun _ _ _ _ _ => .trap .arithmeticOverflow

/-- Instance (law `ConcreteSpec.Examples.add0_never_stuck`, case `upperBound`). -/
example : runBody decls 2 (inputEnv 255) add0 (.copy s₂) ≠ .stuck .movedRead :=
  add0_never_stuck 255 (by decide) 2 .movedRead

/-- Planted (law `ConcreteSpec.Examples.add0_never_stuck`, case `upperBound`, subject `ConcreteSpec.runBody`, mutant `ConcreteSpec.Examples.Add0Evidence.stuckBody`). -/
example : IntTy.u8.inRange 255 = true ∧
    ¬ (stuckBody decls 2 (inputEnv 255) add0 (.copy s₂) ≠ .stuck .movedRead) :=
  ⟨by decide, by simp [stuckBody]⟩

/-- Instance (law `ConcreteSpec.Examples.add0_never_traps`, case `upperBound`). -/
example : runBody decls 2 (inputEnv 255) add0 (.copy s₂) ≠ .trap .arithmeticOverflow :=
  add0_never_traps 255 (by decide) 2 .arithmeticOverflow

/-- Planted (law `ConcreteSpec.Examples.add0_never_traps`, case `upperBound`, subject `ConcreteSpec.runBody`, mutant `ConcreteSpec.Examples.Add0Evidence.trapBody`). -/
example : IntTy.u8.inRange 255 = true ∧
    ¬ (trapBody decls 2 (inputEnv 255) add0 (.copy s₂) ≠ .trap .arithmeticOverflow) :=
  ⟨by decide, by simp [trapBody]⟩

end Add0Evidence

end ConcreteSpec.Examples

namespace ConcreteSpec.Cleanup

/-- The theorem applied to return: with every call returning, that is the whole trace. -/
example : RunCleanup (fun _ => true) (pending stack)
    [.call desV, .release ⟨7, 2⟩, .call desB, .call relA] .done :=
  ((defer_cleanup_releases (ok := fun _ => true) stack_reg (e := .ret) (by decide) rfl).1
    (by decide) _ _).mpr ⟨rfl, rfl⟩

/-- If destroy(b) traps on return, destroy(v) ran and released its loan first, and
give_up(a) never runs. -/
example : RunCleanup (fun d => d != desB) (pending stack)
    [.call desV, .release ⟨7, 2⟩, .call desB] .trapped :=
  ((defer_cleanup_releases (ok := fun d => d != desB) stack_reg (e := .ret) (by decide) rfl).2.2.1
    [desV] desB [relA] rfl (by decide) (by decide) _ _).mpr ⟨rfl, rfl⟩

/-- If destroy(v) itself traps on return while holding loan 7, the trace stops at that call:
loan 7 is not released by cleanup, and destroy(b) and give_up(a) never run. -/
example : RunCleanup (fun d => d != desV) (pending stack) [.call desV] .trapped :=
  ((defer_cleanup_releases (ok := fun d => d != desV) stack_reg (e := .ret) (by decide) rfl).2.2.1
    [] desV [desB, relA] rfl (by decide) (by decide) _ _).mpr ⟨rfl, rfl⟩

/-- Falling off the block runs destroy(c); continue L2 also runs destroy(d); break L1
(the outer label) also runs destroy(v) and releases its loan; each through the theorem. -/
example : RunCleanup (fun _ => true) [desC] [.call desC] .done :=
  ((defer_cleanup_releases (ok := fun _ => true) stack2_reg (e := .fallthrough) (by decide)
    (fs := [⟨.block, [desC]⟩]) rfl).1 (by decide) _ _).mpr ⟨rfl, rfl⟩

example : RunCleanup (fun _ => true) [desC, desD] [.call desC, .call desD] .done :=
  ((defer_cleanup_releases (ok := fun _ => true) stack2_reg (e := .cont 2) (by decide)
    (fs := [⟨.block, [desC]⟩, ⟨.loopBody 2, [desD]⟩]) rfl).1 (by decide) _ _).mpr ⟨rfl, rfl⟩

example : RunCleanup (fun _ => true) [desC, desD, desV]
    [.call desC, .call desD, .call desV, .release ⟨7, 2⟩] .done :=
  ((defer_cleanup_releases (ok := fun _ => true) stack2_reg (e := .brk 1) (by decide)
    (fs := [⟨.block, [desC]⟩, ⟨.loopBody 2, [desD]⟩, ⟨.loopBody 1, [desV]⟩]) rfl).1
    (by decide) _ _).mpr ⟨rfl, rfl⟩

/-- continue L1 (the outer label) unwinds the same scopes as break L1, then tests L1's
condition again (F7). -/
example : RunCleanup (fun _ => true) [desC, desD, desV]
    [.call desC, .call desD, .call desV, .release ⟨7, 2⟩] .done :=
  ((defer_cleanup_releases (ok := fun _ => true) stack2_reg (e := .cont 1) (by decide)
    (fs := [⟨.block, [desC]⟩, ⟨.loopBody 2, [desD]⟩, ⟨.loopBody 1, [desV]⟩]) rfl).1
    (by decide) _ _).mpr ⟨rfl, rfl⟩

end ConcreteSpec.Cleanup

namespace ConcreteSpec.Brand

/-- The brand-mismatch example of Spec/Brand.lean, derived from brand_mismatch_refused. -/
example : ¬ Ok [10, 11] [(1, .heap 10), (0, .alloc 11)] [.use 1 0] [(1, .heap 10), (0, .alloc 11)] :=
  (brand_mismatch_refused rfl rfl (by decide)).1

end ConcreteSpec.Brand
