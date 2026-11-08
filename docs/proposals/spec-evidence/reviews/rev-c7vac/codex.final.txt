C7 needs changes under the binding vacuity rule. The NI/CT examples have substantive witnesses and counter-examples, but the report proofs include vacuous laws, reset accounting checks part of its claimed invariant before admitting a transition, and most remaining obligations lack planted negative controls.

This was a static, read-only review of `REVIEW.diff` and the corresponding source. I made no changes, ran no builds or gates, and used no network. Proposed controls below have **not** been executed.

**Findings**

1. **ConcreteSpec/Proofs/Leak.lean:523 — major — REAL DEFECT: report soundness is established by copying the actual quantities into the bounds.**

   `established_report_sound` unfolds to three instances of `x ≤ x`, with footprint checking disabled. It accepts arbitrary entry/exit accounts without an execution connecting them. This is precisely a theorem that restates its definition; it establishes no independently derived bound.

   **Concrete fix:** prove soundness of a report constructed independently from a nonempty action sequence or analysis result. For example, start with 100 historical requested bytes, enter with 2 live bytes, allocate 8 and free 3: independently report requested ≤ 8, net change ≤ 5, and local peak ≤ 10. Plant separate underbounds 7, 4, and 9 and prove each report unsound.

2. **ConcreteSpec/Proofs/Leak.lean:540 — major — REAL DEFECT: every soundness obligation is disabled by its status guard.**

   `unestablished_report_sound` holds because each premise `notEstablished = boundEstablished` is false. There is no non-trivial accounting instance that exercises a bound, and no counter-example while those statuses remain fixed.

   The representation of unknown evidence is reasonable; counting this implication as a qualifying soundness law violates the supplied owner rule.

   **Concrete fix:** treat this as a representation lemma, explicitly identified as vacuous rather than soundness evidence. Add an independently established report with mixed statuses, demonstrate that established fields constrain actual values, and plant an understated established field that fails.

3. **ConcreteSpec/Spec/Leak.lean:236; ConcreteSpec/Proofs/Leak.lean:416 — major — REAL DEFECT: reset outflow conservation is enforced by the transition guard.**

   The action supplies `released`; `apply` accepts it only after checking `released = current.live`. `reset_conservation` then uses that exact equality to prove its claimed outflow. A deliberately incorrect reset outflow is excluded before the law can inspect it. `run_conservation` inherits this limitation for reset outflows.

   **Concrete fix:** derive the reset outflow from the transition itself, or prove a separate refinement from independently executed C4 resets to accounting records. Plant a reset implementation or recorded outflow that disagrees with the actual released bytes and demonstrate that the independent conservation obligation fails.

4. **ConcreteSpec/Spec/Leak.lean:324 — major — REAL DEFECT: wipe preservation has neither a non-trivial instance nor a planted counter-example.**

   `WipePreservation` is only an obligation schema. Nothing in this delta exhibits a related source/target pair containing a wipe. An empty relation or universally empty wipe traces satisfy it.

   **Concrete fix:** supply a related pair with at least one nonzero wipe, prove preservation, and plant a related target that drops, changes, or reorders that wipe. Prove the planted transformation fails preservation.

5. **ConcreteSpec/Proofs/Leak.lean:328; ConcreteSpec/Proofs/Exec.lean:530 — major — REAL DEFECT: the required per-law negative controls are largely absent.**

   The tables below identify the gaps individually. Successful evaluation examples and generic proofs do not supply the owner-required planted counter-example for each law. The drop-refusal example covers only one lifecycle refusal path; it does not exercise binding, activation entry, return, primitive continuation refusal, or both loop-test branches.

   **Concrete fix:** add the listed concrete instances and independent negative controls. Keep mathematical helper lemmas distinct from substantive contract evidence; a definition-expansion helper must not substitute for that evidence.

**Audit notation**

- **N:** substantive, non-vacuous instance exists.
- **M:** potentially substantive, but the required instance or planted control is missing.
- **V:** vacuous under the supplied rule.
- **D:** definition-level calculation/helper; it does not independently establish the advertised contract.
- “Proposed” means a review suggestion, **not a planted control present in the delta**.

For negative theorems such as `authorized_release_not_ni`, the counter-case below means a case refuting the defective implementation or overstrong security claim—not a counter-example to a valid Lean theorem.

**C7 security laws and execution witnesses**

All locations in this table are in `ConcreteSpec/Proofs/Leak.lean`.

| Line / law | Verdict | Instance and counter-example |
|---|---|---|
| 159 `allowed_unique` | D / M | Nonempty allowed answers exist in `secureRun` and `leakyRun`. Uniqueness is directly enforced by equality to one answer in `allowed`; no independently permissive answer-contract control is planted. Proposed: allow two distinct output answers and show uniqueness fails. |
| 173 `step_unique` | N / M | Concrete four-step executions exist. Proposed control: two allowed primitive answers producing different labels/successors; absent. |
| 196 `secure_run_unique` | N | `secureRun 1` inhabits its run premise; the four steps are explicitly constructed. `leakyRun 1` has different output/leakage and demonstrates why the same trace characterization cannot cover the leaky entry. |
| 230 `secure_ni` | N | `secureRun 1` and `secureRun 2`: different secrets, matching choices/releases, equal public observations. Planted counter-example: `leakyRun 1` and `leakyRun 2`, established at line 305. |
| 240 `secure_ct` | N | Same secure pair has equal nonempty leakage traces. Planted counter-example: the leaky pair has operands `[1]` versus `[2]`, established at line 315. |
| 248 `secureRun` | N | Explicit terminating execution, including a release, output, and final observation. The leaky execution supplies the negative security comparison. |
| 273 `leakyRun` | N | Explicit terminating execution that outputs and leaks its secret. Secure executions supply the corresponding passing cases. |
| 297 `leaky_low` | D / M | Secrets 1 and 2 satisfy it, but this labelling discards every value and makes `lowStore` universally true. No public-value/store mismatch control exists. |
| 300 `release_authorized` | N | Nonempty release trace at permitted site 0. `Spec/Leak.lean:87` supplies an unauthorized site-1 counter-example. |
| 305 `authorized_release_not_ni` | N | Actual runs with secrets 1 and 2 satisfy every premise and differ publicly. `secure_ni` supplies the passing comparison. |
| 315 `authorized_release_not_ct` | N | Actual runs satisfy every premise and have distinct leakage. `secure_ct` supplies the passing comparison. |

The secure examples do **not** rely on impossible run premises: the explicit `secureRun` constructors resolve that concern. Their fixed-length `Unit` choice traces also do not filter out the exhibited leaky pair.

**Every accounting and reporting theorem**

Here, let `A = ⟨100, 5, 12, 9⟩`, a well-formed account. Suggested mutations are absent unless stated otherwise.

| Location in `Proofs/Leak.lean` | Verdict | Non-trivial instance; negative control |
|---|---|---|
| 328 `Account.alloc_zero` | N / M | `A.alloc 0 = A`. Proposed: zero allocation increments requested bytes. |
| 332 `Memory.set_same` | N / M | Nonconstant memory, setting one lineage to its existing account. Proposed: setter also changes another lineage. |
| 337 `Account.enter_wf` | N / M | `A.enter` has local peak 5 while retaining historical peak 12. Proposed: initialize local peak below live bytes. |
| 340 `Account.alloc_wf` | N / M | Allocate 10 from `A`, producing live/peaks 15. Proposed: leave either peak below 15. No runtime WF guard causes this theorem to pass. |
| 343 `Account.free_wf` | N / M | Free 3 from `A`, leaving live 2. Proposed: free increases live above a peak. |
| 346 `Account.reset_wf` | D / M | Reset sets live to zero, making both inequalities automatic. No independent reset-correctness control exists. |
| 350 `apply_wf` | N / M | Successful nonzero allocation from well-formed memory. Proposed: corrupt a touched or untouched lineage’s peak. |
| 384 `run_wf` | N / M | Allocate 8, free 3, reset 5—the existing sequence is an instance. Proposed: an intermediate action leaves an invalid final account. |
| 396 `apply_alloc_zero` | N / M | Apply zero allocation to nonempty well-formed memory. Proposed: alter another lineage or cumulative history. |
| 402 `free_conservation` | D / M | Free 3 from live 5; stated fields directly expand `Account.free`. Proposed independent control: decrement requested or peak while freeing. No such control is planted. |
| 416 `reset_conservation` | V | Reset live 5 with stated outflow 5. Wrong outflow is rejected by the very equality used in the proof; finding 3. |
| 431 `run_conservation` | N / M, reset qualification | Existing 8/3/5 sequence genuinely exercises allocation/free arithmetic. Reset outflow correctness remains guard-enforced. Proposed controls: lose an allocation, double-count a free, or misrecord reset outflow. |
| 478 `apply_peak_monotone` | N / M | Allocate beyond an existing peak; free/reset retain it. Proposed: lower peak during free/reset. |
| 509 `run_peak_monotone` | N / M | Allocate then free/reset, leaving a historical peak above live bytes. Proposed: clear peak on the final reset. |
| 523 `established_report_sound` | V | Bounds are the actual quantities themselves; finding 1. |
| 532 `empty_function_report_sound` | N / M | Historical requested/peak 100, current live 0, local request/peak bounds 0. This distinguishes local accounting from history. Missing planted report using an understated bound on a nonempty function. |
| 540 `unestablished_report_sound` | V | All antecedents are false; finding 2. |

The prose also promises preservation of **function-local peaks** by free/reset. Those fields are preserved by the definitions, but `free_conservation` and `reset_conservation` omit them from their conclusions. Independent positive and corrupt-function-peak controls are missing.

**Definition-level guarantees and examples in `Spec/Leak.lean`**

| Location / guarantee | Verdict and witness/control audit |
|---|---|
| 38 `Leakage` | Seven distinct constructors represent branches, addresses, allocations, loops, variable-time operands, public lengths, and external effects. Only variable-time operands are exercised. Nonempty witnesses and changed-event controls for the other six forms are missing. |
| 49 `ReleaseSite`; 56 `Release` | Typed site, value, reason, and path are represented. Site mismatch is exercised; type mismatch, changed value, and release-order controls are missing. |
| 70 `Permits`; 78 `Authorized` | **N:** listed site 0 succeeds at line 83; site 1 fails at line 87. A typed-site mismatch control is missing. |
| 74 `ForbidsRelease`; example 91 | **D / M:** the example is exactly `[] = []`. Missing substantive law: every nonempty release trace is unauthorized under the empty policy, with a nonempty permitted-policy contrast. |
| 95 `Labelling`; 101 `publicLocals` | **M:** the demo projects everything to `none`; no mixed public/secret locals example proves selective retention. Proposed: change a public binding and reject equivalence while allowing a secret change. |
| 107 `LowEquivalent` | **M:** differing-secret positive instance exists through `leaky_low`. No public-locals or public-store negative instance. |
| 113 `Instrumentation` | **N / M:** concrete instrumentation emits nonempty observations, leakage, releases, and choices. No control detects reversed accumulation or omitted final observation. |
| 126 `InstrumentedSteps` | **N / M:** explicit four-step runs preserve event order. Missing planted swapped/dropped trace control and an explicit projection back to C1 steps. |
| 137 `Run` | **N / M:** explicit complete secure/leaky runs. Missing refusal of a nonfinal prefix as a complete run. |
| 148 `publicObservations` | **N / M:** secure trace contains output 7 followed by normal termination. Missing dropped/changed-final-observation control. |
| 155 `DelimitedReleaseNI` | **N:** secure and leaky witnesses above exercise the predicate positively and negatively. |
| 164 `ConstantTime` | **N:** secure and leaky witnesses above exercise it positively and negatively. |
| 183 `Account.empty`; 228 `Memory.empty` | **D / M:** zero initialization by definition; no nonzero-lineage initialization control. |
| 187 `Account.WF` | **M:** valid nonempty accounts are possible; no explicit invalid account with live above either peak is planted. |
| 191 `Account.enter` | **N / M:** line 532 distinguishes local from historical peak. Missing live-on-entry nonzero witness and incorrect-entry-peak control. |
| 196 `Account.alloc` | **N / M:** line 306 exercises nonzero allocation. Missing controls for cumulative requests, peak growth, and retaining an already greater peak. |
| 204 `Account.free` | **N / M:** free 3 after allocating 8. Empty-memory overfree is rejected at line 309; history/local-peak corruption controls are absent. |
| 209 `Account.reset`; 214 `Action.reset` | **V for claimed outflow evidence:** nonzero reset exists, but correct recorded outflow is checked before transition; finding 3. |
| 224 `Memory.set` | **M:** setter identity theorem exists. No concrete two-lineage isolation instance/control. |
| 232 `apply` | **N / M:** accepted sequence and rejected overfree exist. Missing misstated-reset refusal example and independent accounting-corruption controls. |
| 240 `run` | **N / M:** nonempty accepted sequence exists. No sequence with a successful prefix followed by a refused action is exhibited. |
| 245 `allocated` | **M:** sequence supplies allocation 8. Missing interleaved two-lineage counting control. |
| 251 `freed` | **M:** sequence supplies explicit free 3. Missing control distinguishing another lineage and reset outflow. |
| 257 `resetBytes` | **M:** sequence supplies reset outflow 5. Missing multiple-reset/two-lineage counting control. |
| 276 `Bound.Sound` | **M:** exact established bounds and disabled unknown bounds are instantiated. Missing established underbound rejection. |
| 281 `Bound.SoundOptional` | **M:** only `none` with unestablished footprint is demonstrated. Missing `some 10` bounded by 12, rejection at 9, and rejection of established evidence for `none`. |
| 298 `LineageReport.Sound` | **M / V evidence:** empty-function example is substantive; generic supporting laws are vacuous. Missing nonempty function and independent failures for each report field/status. |
| Example 306 | **N:** alloc 8/free 3/reset 5 yields requested 8/live 0/peaks 8. Line 309 rejects overfree, but does not challenge each calculated field. |
| Example 309 | **N:** freeing 1 from empty memory is refused; line 306 supplies a successful nonzero free. |
| 315 `WipeEvent`; 324 `WipePreservation` | **M:** no nonempty wipe instance or dropped/changed/reordered wipe counter-example; finding 4. |

**All changed execution laws and examples**

These lifecycle changes are part of the supplied delta and therefore included. Ordinary success/refusal premises are realizable with accepting/refusing operand instances; they are not inherently impossible. However, an explicit premise saying that a callback refused does not independently prove an ownership or information-flow invariant.

| Location in `Proofs/Exec.lean` | Vacuity audit |
|---|---|
| 62 `steps_request` | **N / M:** instantiated by `demo_log_completes` and `demo_bump_completes`. Missing allowed-answer/refused-continuation negative control. |
| 161 `step_return_caller` | **N / M:** accepting demo lifecycle supplies leave/bind success. Missing concrete nonempty caller restoration instance and separately refused leave/result-bind controls. |
| 173 `exit_scope_next` | **N / M:** nonempty defer list starts its newest call. Missing wrong-order successor control. |
| 184 `done_scope_next` | **N / M:** normal scope execution has a concrete deferred-call example at line 719. Missing wrong-order control. |
| 198 `exit_scope_runs_defers` | **N / M:** completing demo calls and accepting drop realize premises. Missing explicit exit instance with nonempty defers and reversed-order control. |
| 211 `done_scope_runs_defers` | **N / M:** line 719 supplies bump then log. Missing planted order/state corruption control. |
| 226 `unwind_runs_plan` | **N / M:** nested scopes with completing calls realize premises. Missing explicit nonempty nested witness/control. |
| 258 `return_reaches_caller` | **N / M:** completing cleanup plus accepting leave/bind realizes premises. Missing explicit nonempty cleanup/caller witness and lost-return-value control. |
| 339 `RunsEach.drop` | **D / M:** successful drop is built into its constructor. Missing independent execution/refusal comparison. |
| 374 `cleans_iff_runs_plan` | **N / M:** equivalence relates two structured representations, rather than assuming its conclusion. Missing concrete nested plan and changed-order/drop control. |
| 425 `steps_from_trapped` | **N / M:** terminal trap supplies a real initial state; proof-only constructor adaptation. Missing planted continuation-after-trap control. |
| 439 `abort_runs_no_cleanup` | **N / M:** abort with pending cleanup realizes a meaningful case. Missing concrete pending-effect witness and planted cleanup-after-abort control in this delta. |
| 494 `step_deterministic` | **N / M:** demo silent/primitive steps realize premises. Missing concrete distinct allowed-answer pair exercising the environment alternative. |
| 522 `no_step_of_none` | **D / M:** follows directly from `Step` requiring a successful `step`; refused drop gives an instance. No independent malformed-transition control. |
| 530 `bind_refused` | **N / M:** possible with successful literal evaluation and refusing bind callback. No concrete instance/control. |
| 537 `assign_refused` | **N / M:** existing local plus refusing bind callback is possible. No concrete instance/control; use an existing local so absent-local refusal does not mask the test. |
| 545 `enter_refused` | **D / M:** `enter` directly maps over the refused callback. No concrete refused-entry example or accepting contrast. |
| 551 `call_refused` | **N / M:** possible after argument success. Missing matched-arity concrete instance, so arity mismatch cannot mask callback propagation. |
| 560 `scope_drop_refused` | **N:** refusing/accepting scope examples at `Spec/Exec.lean:776/781` exercise the path. |
| 567 `exit_drop_refused` | **N / M:** possible for both scope and test frames. Neither explicit structured-exit instance/control is present. |
| 576 `test_drop_refused` | **N / M:** possible after successful evaluation. Missing separate `test = some true` and `some false` instances; `test = none` would mask the intended refusal. |
| 585 `return_refused` | **N / M:** possible for explicit and Unit returns. Missing separate refused-leave and refused-bind cases, each with accepting contrasts. |
| 598 `prim_bind_refused` | **N / M:** possible for a successful primitive answer and refused destination bind. Missing concrete **allowed** answer, refused continuation, and error-answer bypass examples. |
| 614 `continuation_refused` | **V / D:** `cont a = none` is repackaged as “not some.” No machine behavior is established beyond the hypothesis. Use a concrete primitive run/refusal control as substantive evidence. |
| 701 `demo_log_completes` | **N / M:** actual call emits 1 and returns. Missing planted wrong-event control. |
| 709 `demo_bump_completes` | **N / M:** actual call increments counter. Missing planted missing/double increment control. |
| 719 scope example | **N / M:** two nonempty deferred calls exercise state threading and output. Missing reversed order control; use distinguishable effects so reversal is observable. |

Finally, the changed executable examples in `Spec/Exec.lean`:

| Location | Verdict |
|---|---|
| 759 `enter = some start` | **D:** compatibility equation for an always-accepting instance; not an independent entry guarantee. Refused-entry contrast missing. |
| 776 flagged-local scope | **N:** flagged `x` becomes stuck; unflagged `y` at line 781 supplies the passing contrast. |
| 781 unflagged-local scope | **N:** completes normally; flagged `x` supplies the rejection control. |
| 897 unanswered oracle | **N:** reaches `noAnswer`; line 901 distinguishes an answered-but-invalid request. |
| 901 invalid oracle answer | **N:** reaches `badAnswer`; normal demo oracle supplies valid-answer instances. |
| 947 infinite loop | **N / M:** consumes fuel and reports `outOfFuel`. Missing paired terminating-loop control specifically challenging this example’s outcome. |

No unresolved owner decision needs to be chosen to address these findings. The missing Secret typing, backend timing, and physical wiping implementations are explicitly deferred by this contract; I have not treated those deferred implementations as defects in this slice.

VERDICT: CHANGES