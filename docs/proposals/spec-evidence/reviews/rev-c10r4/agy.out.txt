### Round-3 Fix Verification

1. **`C1Call` restoration conjuncts & `c1Call_restores_caller` (Fix 1 — Verified):**
   - `exit.ctrl = .done`, `exit.locals = Exec.bindDst dst result caller`, and `exit.frames = k` have been removed from [`C1Call`](file:///ConcreteSpec/Spec/Frame.lean#L127-L144).
   - [`c1Call_restores_caller`](file:///ConcreteSpec/Proofs/Frame.lean#L360-L394) now connects `entry = ⟨.exec (.call dst f args site), caller, st, k⟩` directly to `exit` by unfolding [`Exec.step`](file:///ConcreteSpec/Proofs/Frame.lean#L372-L393) and `Exec.stepExit` on the final step out of the call's `.ret dst caller :: k` frame across both return branches (`O.life .leave` and `Exec.bindLife`).
2. **`C1Call` Unit fall-through (Fix 2 — Verified):**
   - [`C1Call`](file:///ConcreteSpec/Spec/Frame.lean#L138-L144) now takes the disjunction of an explicit return (`⟨.exiting (.ret result), callee, stReturn, .ret dst caller :: k⟩`) and a normal Unit fall-through (`result = O.unitVal` at `⟨.done, callee, stReturn, .ret dst caller :: k⟩`), each followed by its final `Exec.step` popping `.ret dst caller`.
   - Tested by the non-trivial `.skip` body witness [`c1UnitFn`](file:///ConcreteSpec/Proofs/Frame.lean#L448-L469).
3. **`KeptPair.WellFormed` redundancy & `keptPair_results_valid` (Fix 3 — Verified):**
   - The two redundant `ExternResult.Valid` conjuncts are dropped from [`KeptPair.WellFormed`](file:///ConcreteSpec/Spec/Frame.lean#L358-L362).
   - [`keptPair_results_valid`](file:///ConcreteSpec/Proofs/Frame.lean#L601-L609) derives `ExternResult.Valid pair pair.submit submitResult ∧ ExternResult.Valid pair pair.complete completeResult` from `WellFormed`.

---

### Merge Resolution Check (`f5f46bef1..2dfce586`)

- [`ConcreteSpec/README.md`](file:///ConcreteSpec/README.md#L96-L99): Combines the C10 (`Frame`) summary from `fa284eb9a` with the C6 (`Observe`) completion from `07c16bb5f`, updating the remaining slices to `C5 and C7–C9 remain`. Clean.
- [`ConcreteSpec/Spec.lean`](file:///ConcreteSpec/Spec.lean#L61-L64): Lists `Hybrid` (C4), `Observe` (C6), and `Frame` (C10) in sequence. Clean.

---

### Law Vacuity Audit

| Law | Verdict | Non-trivial Instance (Accepted) | Planted Counter-example (Refused) |
| :--- | :--- | :--- | :--- |
| [`return_cleanup_restores_caller`](file:///ConcreteSpec/Proofs/Frame.lean#L354-L355) | **Non-vacuous** | Step from `.exiting (.ret v)` with `O.life .leave = some stLeave` and `bindLife = some stBind` reaches `⟨.done, bindDst dst v caller, stBind, k⟩`. | Fails when `O.life .leave = none`, `bindLife = none`, or top frame is not `.ret dst caller`. |
| [`c1Call_restores_caller`](file:///ConcreteSpec/Proofs/Frame.lean#L360-L394) | **Non-vacuous** | [`c1ExplicitCall`](file:///ConcreteSpec/Proofs/Frame.lean#L425-L446) (explicit `.ret 7`) and [`c1UnitFn`](file:///ConcreteSpec/Proofs/Frame.lean#L450-L469) (`.skip` fall-through) step from `.call` entry to `.done` exit with caller locals and `k = []` restored. | [`c1BadExit`](file:///ConcreteSpec/Proofs/Frame.lean#L471-L493) (`⟨.done, [], (), [.ret none []]⟩`, unpopped `.ret` frame) is refused both for `C1Call` and for the restoration conclusion. |
| [`c1_call_frame`](file:///ConcreteSpec/Proofs/Frame.lean#L496) | **Non-vacuous** | Callee contract over `c1ExplicitCall` preserving caller bytes outside the callee's write footprint while modifying the callee's own block. | Mutating a caller byte outside the declared `writes` footprint fails `FramePreserved`. |
| [`extern_result_rejects_unrelated_retention`](file:///ConcreteSpec/Proofs/Frame.lean#L596) | **Non-vacuous** | `KeptPair ⟨0, 1, 7⟩` accepts `0` for `.receipt 7` and `1` for `.owned 7`. | Function `2` (or handle `8`) is rejected by `ExternResult.Valid` (`¬ ExternResult.Valid ⟨0, 1, 7⟩ 2 (.receipt 7)`). |
| [`keptPair_results_valid`](file:///ConcreteSpec/Proofs/Frame.lean#L601-L609) | **Non-vacuous** | [`⟨0, 1, 7⟩`](file:///ConcreteSpec/Proofs/Frame.lean#L612-L615) with `submitParam = ⟨.owned 7, .kept⟩`, `submitResult = .receipt 7`, `completeParam = .receipt 7`, `completeResult = .owned 7`. | [Mismatched handle `.receipt 8`](file:///ConcreteSpec/Proofs/Frame.lean#L619-L626) fails both `WellFormed` and `ExternResult.Valid ⟨0, 1, 7⟩ 0 (.receipt 8)`. |
| [`kept_only_paired_completion`](file:///ConcreteSpec/Proofs/Frame.lean#L630) | **Non-vacuous** | Transition from `.retained 7` via `complete = 1` with `.receipt 7` restores `.concreteOwned 7`. | Unpaired function or mismatched handle fails the transition out of `.retained`. |
| *(Unchanged Frame/Footprint/ABI/Launcher laws in `Proofs/Frame.lean`)* | **Non-vacuous** | Disjointness, monotonicity, `FramePreserved` composition, ABI layout/passing/marshaling, and `LauncherRun` laws all remain unchanged from round 3 with their existing positive and negative witnesses intact. | Existing planted counter-examples (overlapping ranges, out-of-footprint writes, misaligned/wrong-size ABI slots, unpopped launcher frame) remain intact. |

No defects found.

VERDICT: ACCEPT
rc=0
