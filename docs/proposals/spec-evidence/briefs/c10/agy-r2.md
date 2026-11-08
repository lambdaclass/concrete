1. **`ConcreteSpec/Spec/Frame.lean:338-342` — [major] [REAL DEFECT] `KeptPair.WellFormed` now accepts `.ffi _` as `submitResult` or `completeResult`**
   - **Problem:** In Round 1, `KeptPair.WellFormed` required `submitResult = .receipt pair.handle` and `completeResult = .owned pair.handle`. In Round 2, those two equalities were replaced with `ExternResult.Valid pair pair.submit submitResult` and `ExternResult.Valid pair pair.complete completeResult`. Because `ExternResult.Valid` defines `.ffi _ => True` (`ConcreteSpec/Spec/Frame.lean:325`), `KeptPair.WellFormed (⟨0, 1, 7⟩ : KeptPair Nat Nat) ⟨.owned 7, .kept⟩ (.ffi .int) (.receipt 7) (.ffi .int)` is now provably `True`! That violates Q3.12 and the docstring directly above `KeptPair.WellFormed` ("only submission returns `Kept<H>` and only completion returns `H`").
   - **Concrete fix:** Keep both the exact type requirements (`submitResult = .receipt pair.handle` and `completeResult = .owned pair.handle`) and `ExternResult.Valid` (or require exact equality in `WellFormed` while keeping `ExternResult.Valid` for individual extern result checking):
     ```lean
     def KeptPair.WellFormed {F H : Type} (pair : KeptPair F H)
         (submitParam : ExternParam H) (submitResult completeParam completeResult : ExternType H) : Prop :=
       pair.submit ≠ pair.complete ∧
         submitParam = ⟨.owned pair.handle, .kept⟩ ∧
         submitResult = .receipt pair.handle ∧
         ExternResult.Valid pair pair.submit submitResult ∧
         completeParam = .receipt pair.handle ∧
         completeResult = .owned pair.handle ∧
         ExternResult.Valid pair pair.complete completeResult
     ```
     And add a negative example refusing `.ffi .int` as `submitResult` or `completeResult` in `KeptPair.WellFormed`.

2. **`ConcreteSpec/Spec/Frame.lean:89-90` and `ConcreteSpec/Proofs/Frame.lean:119-128` — [minor] [REAL DEFECT] `Footprint.BlockOutside` conflates nonempty byte ranges with block-metadata footprints, making `store_frame`'s metadata preservation vacuous on the written block**
   - **Problem:** `Footprint.BlockOutside f b` holds only when `∀ r ∈ f.bytes, r.block ≠ b ∨ r.extent = 0`. Consequently, when `store` writes `bs.length > 0` bytes to block `b`, `accessFootprint` is `.byte ⟨b, off, bs.length⟩`, so `(accessFootprint access bs.length).BlockOutside b` is `False`. In `store_frame` (`ConcreteSpec/Proofs/Frame.lean:119-128`), the `c = b` branch of metadata preservation is proved by contradiction (`exact (hne rfl).elim` / `exact (hn he).elim`) rather than showing that `store` preserves `blockMetadata` of the written block `b`! Thus, `store_frame` does not actually guarantee that a nonempty `store` preserves `space`, `base`, `size`, `align`, `live`, and `exposed` of the block being written to.
   - **Concrete fix:** Either track metadata footprints explicitly as a `blocks : List MemBlockId` field on `Footprint` (so `.byte r` has `blocks = []` and `BlockOutside` is `b ∉ f.blocks`, allowing `store_frame`, `write_materialized_frame`, and `rawWrite_frame` to preserve metadata of *all* blocks including `b`), or prove a dedicated lemma in `Proofs/Frame.lean` that `Memory.store` preserves `blockMetadata` for every block (including the target block `b`).

3. **`ConcreteSpec/Spec/Frame.lean:124-135` — [minor] [PREFERENCE] `C1Call` characterizes return solely by `exit.ctrl = .done ∧ exit.frames = k`**
   - **Problem:** Although `C1Call` now starts at `.call dst f args site`, the multi-step transition `Exec.Steps P entry tr exit` is still only constrained at the end by `exit.ctrl = .done ∧ exit.locals = Exec.bindDst dst result caller ∧ exit.frames = k`. If a recursive call inside `f` temporarily produces a configuration whose frame stack happens to equal `k` and locals equal `Exec.bindDst dst result caller` at a statement completion (`.done`), `C1Call` would match before the outer `.ret dst caller` frame is popped. Decomposing `C1Call` as stepping from `entry` to the `.ret dst caller :: k` boundary followed by `step_return_restores_caller` (with `Passes` holding on intermediate frames) makes the call-return matching airtight even for recursive calls.
   - **Concrete fix:** Witness the return step explicitly in `C1Call` (e.g., `Steps` from the post-`enter` state through `callee` body/cleanup to `⟨.exiting (.ret result), calleeLocals, stClean, .ret dst caller :: k⟩` followed by the `.ret` step to `exit`), or require intermediate steps not to pop `k`.

VERDICT: CHANGES
