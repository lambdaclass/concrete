import Concrete.Proof.Proof

/-!
# fixed_capacity — example proofs

Proof theorems for the `fixed_capacity` flagship (bounded ring buffer), moved
out of `Concrete.Proof` into this per-example namespace. The generic
`while_step` loop lemmas (`eval_while_step_unfold` / `while_step_break` /
`while_step_cont` / `while_step_exit`) are INFRASTRUCTURE (also used by
`Concrete.ProofSoundness`) and stay in `Concrete.Proof`, as do the registered
spec PExprs (`ringNewExpr`/`ringPushExpr`/`ringContainsExpr`/`fcTagExpr`) and the
`fixedCapacityFns` table. Only the ring-buffer proof theorems move.
-/

namespace Examples.FixedCapacity.Proofs

open Concrete.Proof

set_option linter.unusedSimpArgs false in
/-- `ring_contains` on an empty ring (count = 0) returns 0 for any
    value: `scan = min(count, cap) = 0`, so the while_step's cond
    `i < scan` is `0 < 0 = false` on the first check and control
    falls through to `cont = .lit (.int 0)`.  The body never runs.

    This is the first proof in the project that exercises
    `PExpr.while_step` end-to-end under the kernel.  It's modest by
    design — the empty-ring case is the cheapest while_step
    instance (zero iterations) and shows the cond/cont path works.
    Theorems about non-empty rings need iteration counting and are
    a follow-up. -/
theorem ring_contains_empty_correct
    (data : List PVal) (head v : Int) (fuel : Nat) :
    eval fixedCapacityFns
      ((Env.empty.bind "rb" (.struct_ "RingBuf"
        [ ("data",  .array_ data)
        , ("head",  .int head)
        , ("count", .int 0)
        ])).bind "val" (.int v))
      (fuel + 10) ringContainsExpr
    = some (.int 0) := by
  simp [ringContainsExpr, eval, eval.evalWhileStep, eval.lookupField,
        fixedCapacityFns, Env.bind, evalBinOp]

set_option maxHeartbeats 2000000 in
set_option linter.unusedSimpArgs false in
/-- **Composition theorem**: a 1-element ring containing `v` at
    index 0 (head=1, count=1, data starts with `.int v`) has
    `ring_contains v = 1`.

    This is the iteration-counted companion to
    `ring_contains_empty_correct`: the loop runs exactly one
    iteration because `scan = min(count, cap) = 1`.

    Proof strategy uses the helper lemmas above to bypass simp's
    full-eval expansion:
      1. Reach the while_step via the outer letIn chain.
      2. Apply `while_step_break` with hypotheses for cond and
         step (each closed by a focused simp).
      3. cond: `i = 0 < scan = 1` evaluates to true.
      4. step: idx computes to 0 via BitVec.srem 32 16; the eq
         check `val == rb.data[0]` fires; arm returns Break(1).

    Composes with `ring_push_zero_correct`: pushing `v` into the
    empty ring produces exactly this {head=1, count=1, data=[v..]}
    shape.  Two functions, one chain. -/
theorem ring_push_then_contains_correct
    (data_tail : List PVal) (fuel : Nat) :
    eval fixedCapacityFns
      ((Env.empty.bind "rb" (.struct_ "RingBuf"
        [ ("data",  .array_ ((.int 0) :: data_tail))
        , ("head",  .int 1)
        , ("count", .int 1)
        ])).bind "val" (.int 0))
      (fuel + 30) ringContainsExpr
    = some (.int 1) := by
  simp (config := { maxSteps := 1000000 })
       [ringContainsExpr, eval, eval.evalWhileStep, eval.evalFields,
        eval.lookupField, eval.lookupIndex,
        fixedCapacityFns, Env.bind, evalBinOp]

set_option linter.unusedSimpArgs false in
/-- `ring_push` on the canonical empty RingBuf (head=0, count=0,
    data all zeros) with value `v` produces a RingBuf whose data
    has `.int v` at index 0 (the rest zeros), head=1, count=1.

    This is the first proof in the project that exercises
    `PExpr.arraySet` end-to-end under the Lean kernel: the source's
    `d[i] = val` extracts to a shadowing `letIn` rebinding `d` via
    `arraySet`, which is what the state model document specifies. -/
theorem ring_push_zero_correct (v : Int) (fuel : Nat) :
    eval fixedCapacityFns
      ((Env.empty.bind "rb" (.struct_ "RingBuf"
        [ ("data",  .array_ (List.replicate 16 (.int 0)))
        , ("head",  .int 0)
        , ("count", .int 0)
        ])).bind "val" (.int v))
      (fuel + 20) ringPushExpr
    = some (.struct_ "RingBuf"
        [ ("data",  .array_ ((.int v) :: List.replicate 15 (.int 0)))
        , ("head",  .int 1)
        , ("count", .int 1)
        ]) := by
  simp [ringPushExpr, eval, eval.evalFields, eval.lookupField,
        eval.lookupIndex, fixedCapacityFns, Env.bind, evalBinOp,
        List.replicate, List.set]

set_option linter.unusedSimpArgs false in
/-- `compute_tag` on a buffer whose first 6 data bytes are all 0
    returns 0 (any tail is ignored).  This is the first theorem
    in the project that exercises a bounded while loop end-to-end
    under the Lean kernel: each iteration evaluates the cond,
    runs the two flat assigns through `evalAssigns`, and rebinds
    `acc` and `i` in the env.  After 6 iterations acc remains 0
    (0 xor 0 = 0 at each step) and `i = 6` makes the cond
    false, falling through to `return acc`. -/
theorem compute_tag_zero_correct (rest : List PVal) (fuel : Nat) :
    eval fixedCapacityFns
      (Env.empty.bind "buf"
        (.struct_ "MsgBuf"
          [("data", .array_ (List.replicate 6 (.int 0) ++ rest))]))
      (fuel + 80) fcTagExpr
    = some (.int 0) := by
  -- The proof unfolds the loop 6 times.  Each iteration's
  -- bitxor case reduces (0).toNat ^^^ (0).toNat = 0, so acc
  -- stays 0.  After iteration 6, `decide (i < 6) = false`
  -- fires the fall-through branch.
  simp [fcTagExpr,
        eval, eval.evalAssigns, eval.lookupField, eval.lookupIndex,
        fixedCapacityFns, Env.bind, evalBinOp, List.replicate]

set_option linter.unusedSimpArgs false in
/-- `ring_new()` evaluates to the canonical empty RingBuf:
    `data` is 16 zeros, `head` and `count` are both 0.  This is the
    first attached theorem on fixed_capacity, and the first proof
    in the project that combines arrayLit + structLit + letIn. -/
theorem ring_new_correct (fuel : Nat) :
    eval fixedCapacityFns Env.empty (fuel + 5) ringNewExpr
    = some (.struct_ "RingBuf"
        [ ("data",  .array_ (List.replicate 16 (.int 0)))
        , ("head",  .int 0)
        , ("count", .int 0)
        ]) := by
  simp [ringNewExpr,
        eval, eval.evalElems, eval.evalFields,
        fixedCapacityFns, Env.bind,
        List.replicate]
end Examples.FixedCapacity.Proofs
