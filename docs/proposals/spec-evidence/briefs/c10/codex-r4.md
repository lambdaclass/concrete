The three round-3 edits are real, but C10 still fails the requested vacuity standard. Several unchanged “laws” assume their conclusions through contract fields or merely unfold definitions.

Read-only inspection only: no files changed, git commands, network access, builds, or gates. Witnesses below are source-level analyses, not newly compiled tests.

1. **ConcreteSpec/Proofs/Frame.lean:496 — major — REAL DEFECT: `c1_call_frame` assumes the entire frame law.**  
   Its proof is `contract.frame h`; `C1CallContract.frame` already states exactly the conclusion. A callee that corrupts an unrelated byte cannot refute this theorem: it simply cannot receive the required contract. Thus this establishes no connection between a callee’s execution and its advertised footprint.

   **Concrete fix:** derive the contract from independently defined callee effects or execution evidence, then prove that derivation sound for complete `C1Call` executions. Add an actual modifying callee with an unrelated location preserved, and a planted execution that corrupts that location.

2. **ConcreteSpec/Proofs/Frame.lean:528 — major — REAL DEFECT: ABI restoration and location laws remain assumptions packaged as calls.**  
   `clobbers_only_changed_registers`, `caller_stack_slots_equal`, `stack_pointer_restored`, `typed_arguments_at_entry`, and `typed_result_at_return` directly project `Signature.Call` fields. `callee_saved_registers_equal` uses `h.clobbers`, but its conclusion is also already required by `h.preserved`. A machine execution that corrupts a saved register, stack slot, pointer, or result is excluded by the premise rather than exposed by a law.

   **Concrete fix:** distinguish the desired ABI contract from evidence that an independently described call implements it. Derive these properties from a call transition or artifact certificate whose premises do not contain the resulting equalities. Concrete target conventions may remain deferred; an abstract execution/evidence interface still needs that separation.

3. **ConcreteSpec/Proofs/Frame.lean:568 — major — REAL DEFECT: stack accounting and launcher evidence are definitional.**  
   `frame_size_aligned` and `frame_within_stack_bound` project `WellFormed`; `call_stack_account` unfolds addition; `artifact_stack_account` adds associativity. None connects accounting to stack use. Likewise, `launcher_entry_exit` projects all three conclusions from `Launcher.Run`, and `launcher_run_admitted` assumes reservation adequacy through that record.

   **Concrete fix:** introduce independent layout, stack-use, and launch evidence. Derive alignment, bounds, retained closure, and entry/exit correspondence from it. Keep the current predicates as contract specifications, but do not count their projections as fulfillment of the required laws.

4. **ConcreteSpec/Proofs/Frame.lean:590 — major — REAL DEFECT under the owner’s explicit definition-restatement rule: retention laws still characterize definitions rather than validate independent behavior.**  
   The rejection and transition theorems unfold `ExternResult.Valid` or `KeptStep`, whose branches encode the claimed restrictions verbatim. The new `keptPair_results_valid` removes the redundant premises correctly, but proves compatibility between exact descriptor equalities and a validity predicate that repeats those equalities. Its receipt-8 counterexample falsifies the premise too; it does not independently challenge retention behavior.

   **Concrete fix:** retain declaration validity as a specification, and connect declaration checking and retention execution to it through independently defined operations or traces. Plant incorrect endpoint/handle transitions in that independent layer. Constructor-level examples alone cannot discharge this requirement.

The round-3 fixes assess as follows:

- **Caller restoration: fixed.** The conclusion is no longer stored in `C1Call`. The proof evaluates the actual final return/pop step and shares `dst`, `caller`, and `k` with the entry equality. Successful lifecycle instrumentation is derived from successful stepping.
- **Unit fall-through: fixed at the C1 operational level.** The new branch reaches `.done` over the saved return frame and requires its actual pop, returning `O.unitVal`. Function-result typing remains C9’s obligation, consistently with `Exec.step`.
- **Redundant kept-pair validity conjuncts: removed correctly.** This closes the redundancy finding, although the new theorem does not resolve the broader vacuity issue above.

The two supplied merge resolutions are correct: the README retains C10 and removes completed C6 from the outstanding list; `Spec.lean` retains both `Observe` and `Frame`, including their imports. I found no new choice among owner decisions in these edits.

Every named public law in `Proofs/Frame.lean` is audited below. **D** means definitional/projection-only under the requested strict rule; **N** means it derives a substantive operational or compositional property. The counterexamples describe planted bad conclusions; for D rows, the premise generally rejects the mutation immediately, which is precisely the limitation.

| Law | Verdict | Positive witness; planted counterexample |
|---|---|---|
| `preserves_refl` | D: reflexivity | Populated state unchanged; mutate an outside byte and preservation fails, but this is no longer `st → st`. |
| `byteOutside_union` | D: list-membership decomposition | Byte outside two nonempty ranges; place it inside either range and the premise fails. |
| `blockOutside_union` | D: list-membership decomposition | Block 2 outside metadata sets `{0}`, `{1}`; block 0 fails the premise. |
| `placeOutside_union` | D: list-membership decomposition | Distinct place outside both lists; add it to either list. |
| `blockOutside_block` | D: singleton nonmembership | Block 1 outside block 0’s footprint; choosing block 0 fails the premise. |
| `preserves_seq` | N | Actual byte store composed with identity; planted liveness mutation is rejected by the examples at lines 145–162. |
| `store_frame` | N | Actual nonempty store preserves the second byte and metadata; planted liveness mutation is rejected. |
| `free_frame` | N | Free a live block beside an unrelated block; corrupt the unrelated block’s metadata or any byte. |
| `expose_frame` | N | Expose a live provenance block; corrupt its bytes or another block’s metadata. |
| `alloc_frame` | N | Allocate beside an existing block; corrupt that old block’s bytes or metadata. |
| `set_frame` | N | Replace place `p` while distinct `q` exists; mutate `q` or memory. |
| `write_structured_frame` | N | Successful same-type structured write; mutate a distinct place. |
| `write_materialized_frame` | N | Successful encoded scalar store; corrupt an outside byte or block metadata. |
| `rawWrite_frame` | N | Actual nonempty raw store; planted liveness mutation is rejected by the existing example. |
| `alloc_fresh_disjoint` | N | Allocate beside an existing nonempty range; reuse its allocation identity with overlapping offsets. |
| `step_call_saves_caller` | N | Call with arguments, populated caller locals, and continuation; omit the return frame or save different locals. |
| `step_return_restores_caller` | N | Return into populated caller with destination binding; restore different locals or continuation. |
| `return_cleanup_restores_caller` | N | Return through a scope containing a successful deferred call; skip its cleanup or corrupt the restored caller. |
| `c1Call_restores_caller` | N; round-3 fix succeeds | Existing explicit-return and fall-through calls; existing retained-return-frame exit fails the conclusion. |
| `c1_call_frame` | D | Real call with identity store projection; corrupt outside state and the required contract cannot be supplied. |
| `machine_preserved_trans` | N | Two calls change scratch registers/slots while preserving saved register 0 and slot 0; corrupt either protected location. |
| `machine_registerFrame_union` | N | First call changes register 1, second register 2, both preserve register 0; corrupt register 0. |
| `callee_saved_registers_equal` | D: already in `preserved` | Saved register 0 unchanged while register 1 changes; corrupt register 0 and `Call` fails. |
| `clobbers_only_changed_registers` | D | Change only register 1 with clobbers `{1}`; change register 0 and the premise fails. |
| `caller_stack_slots_equal` | D | SP 1, preserve slot 0 while changing slot 1; corrupt slot 0 and the premise fails. |
| `stack_pointer_restored` | D | SP 1 before/after; return SP 2 and the premise fails. |
| `typed_arguments_at_entry` | D | Existing arguments `[11,12]` in register/stack locations; `[11,99]` fails the stored argument premise. |
| `typed_result_at_return` | D | Result 7 encoded in its return register; register contains 8 and the premise fails. |
| `frame_size_aligned` | D | Size 16, alignment 8; size 17 fails `WellFormed`. |
| `frame_within_stack_bound` | D | Frame 16, bound 32; bound 8 fails `WellFormed`. |
| `call_stack_account` | D | Caller 16 plus bound 32 gives 48; accounting 47 contradicts the definition, with no stack execution involved. |
| `artifact_stack_account` | D | Runtime 8 plus caller 16 plus bound 32 gives 56; accounting 55 contradicts the definition. |
| `extern_result_rejects_unrelated_retention` | D | Pair `(0,1,7)`, extern 2; allowing its receipt requires changing the validity definition itself. |
| `keptPair_results_valid` | D: descriptor compatibility | Existing pair `(0,1,7)`; receipt 8 fails both premise and conclusion. |
| `kept_only_paired_completion` | D | Kept 7, completion 1, owned 7; wrong endpoint/handle is excluded by `KeptStep`. |
| `submit_produces_paired_receipt` | D | Owned 7, submission 0, kept 7; wrong receipt is excluded by `KeptStep`. |
| `kept_receipt_rejects_submission` | D | Submission 0 cannot consume kept 7; the forbidden transition is definitionally false. |
| `kept_receipt_rejects_other_extern` | D | Extern 2 cannot consume kept 7; again definitionally excluded. |
| `stack_under_reservation` | N: computational admission rule | Bound 64, reservation 32 yields refusal 125; planted entry instead fails. |
| `stack_adequate` | N: computational admission rule | Bound/reservation 64 yields entry; planted refusal fails. |
| `launcher_entry_exit` | D | Exact closure/input/output relations supplied in `Run`; mismatched observation makes `Run` unavailable. |
| `launcher_run_admitted` | D: adequacy carried by `Run` | Bound/reservation 64; reservation 32 makes the premise unavailable. |

The small D algebra helpers are useful identities, but under the stated rule they cannot substitute for independent conformance laws. Several N witnesses above also still need explicit compiled positive/negative examples; inspection establishes their feasibility, not CI validation.

VERDICT: CHANGES