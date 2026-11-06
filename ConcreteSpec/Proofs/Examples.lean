import ConcreteSpec.Proofs.Checker
import ConcreteSpec.Proofs.Rep
import ConcreteSpec.Proofs.Cleanup
import ConcreteSpec.Proofs.Borrow
import ConcreteSpec.Proofs.Brand

/-!
# Examples that apply the theorems: the accepted runtime overflow from an input (`add100_safe`), cleanup traces derived from `defer_cleanup_releases`, and a brand refusal derived from `brand_mismatch_refused`.

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). See `ConcreteSpec/README.md`.
-/

namespace ConcreteSpec.Examples
open ConcreteSpec

theorem add100_ok : BodyOk decls inputCtx add100 (.copy s₂) := bodyOkB_iff.mp (by decide)

/-- For every input value the accepted body never gets stuck. -/
theorem add100_safe (n : Int) (h : IntTy.u8.inRange n = true) (fuel : Nat) (w : Stuck) :
    runBody decls fuel (inputEnv n) add100 (.copy s₂) ≠ .stuck w :=
  runBody_safe add100_ok
    (.cons (fun _ => ⟨_, rfl, .scalar (.int h)⟩) (fun _ hs => by cases hs) (fun _ _ h => by cases h) .nil)
    fuel w

end ConcreteSpec.Examples

namespace ConcreteSpec.Cleanup

theorem stack_reg : Reg ownedAfter stack :=
  .enter (k := .block) <|
    .push (k := .loopBody 1) (d := desV)
      (.enter (k := .loopBody 1) <|
        .push (k := .fnBody) (d := desB)
          (.push (k := .fnBody) (d := relA) (.enter (k := .fnBody) .empty)
            rfl (by decide) (by decide) (by decide))
          rfl (by decide) (by decide) (by decide))
      rfl (by decide) (by decide) (by decide)

/-- The theorem applied to `return`: with every call returning, that is the whole trace. -/
example : RunCleanup (fun _ => true) (pending stack)
    [.call desV, .release ⟨7, 2⟩, .call desB, .call relA] .done :=
  ((defer_cleanup_releases (ok := fun _ => true) stack_reg (e := .ret) (by decide) rfl).1
    (by decide) _ _).mpr ⟨rfl, rfl⟩

/-- If `destroy(b)` traps on `return`, `destroy(v)` ran and released its loan first, and
`give_up(a)` never runs. -/
example : RunCleanup (fun d => d != desB) (pending stack)
    [.call desV, .release ⟨7, 2⟩, .call desB] .trapped :=
  ((defer_cleanup_releases (ok := fun d => d != desB) stack_reg (e := .ret) (by decide) rfl).2.2.1
    [desV] desB [relA] rfl (by decide) (by decide) _ _).mpr ⟨rfl, rfl⟩

/-- If `destroy(v)` itself traps on `return` while holding loan 7, the trace stops at that call:
loan 7 is not released by cleanup, and `destroy(b)` and `give_up(a)` never run. -/
example : RunCleanup (fun d => d != desV) (pending stack) [.call desV] .trapped :=
  ((defer_cleanup_releases (ok := fun d => d != desV) stack_reg (e := .ret) (by decide) rfl).2.2.1
    [] desV [desB, relA] rfl (by decide) (by decide) _ _).mpr ⟨rfl, rfl⟩

theorem stack2_reg : Reg ownedAfter2 stack2 :=
  .push (k := .block) (d := desC)
    (.enter (k := .block) <|
      .push (k := .loopBody 2) (d := desD)
        (.enter (k := .loopBody 2) <|
          .push (k := .loopBody 1) (d := desV)
            (.enter (k := .loopBody 1) <|
              .push (k := .fnBody) (d := desB)
                (.push (k := .fnBody) (d := relA) (.enter (k := .fnBody) .empty)
                  rfl (by decide) (by decide) (by decide))
                rfl (by decide) (by decide) (by decide))
            rfl (by decide) (by decide) (by decide))
        rfl (by decide) (by decide) (by decide))
    rfl (by decide) (by decide) (by decide)

/-- Falling off the block runs `destroy(c)`; `continue L2` also runs `destroy(d)`; `break L1`
(the outer label) also runs `destroy(v)` and releases its loan; each through the theorem. -/
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

/-- `continue L1` (the outer label) unwinds the same scopes as `break L1`, then tests `L1`'s
condition again (F7). -/
example : RunCleanup (fun _ => true) [desC, desD, desV]
    [.call desC, .call desD, .call desV, .release ⟨7, 2⟩] .done :=
  ((defer_cleanup_releases (ok := fun _ => true) stack2_reg (e := .cont 1) (by decide)
    (fs := [⟨.block, [desC]⟩, ⟨.loopBody 2, [desD]⟩, ⟨.loopBody 1, [desV]⟩]) rfl).1
    (by decide) _ _).mpr ⟨rfl, rfl⟩

end ConcreteSpec.Cleanup

namespace ConcreteSpec.Brand

/-- A value of brand 10 used through an allocator of brand 11 is refused. -/
example : ¬ Ok [10, 11] [(1, .heap 10), (0, .alloc 11)] [.use 1 0] [(1, .heap 10), (0, .alloc 11)] :=
  (brand_mismatch_refused rfl rfl (by decide)).1

end ConcreteSpec.Brand
