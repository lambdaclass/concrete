# C10 N2 audit

Source: clean, read-only C10 checkout at `487085db1fcdf85a20c6310314170bfbcb44dc8f`
(round 5 `4159b7c56` plus its golden). Scope: `ConcreteSpec.Proofs.Frame`.
Gate code: `985bf2fd4`. Outcome: **FAIL**, exit 1; no INCOMPLETE findings.
The scoped source build held a slot through the supplied `lean-build.sh` wrapper.

Reproduce from this branch under `nix develop`, holding the shared build slot:

```
bash scripts/proofs/vacuity.sh --source /home/unbalancedparen/projects/gates-vacuity-c10 --scope ConcreteSpec.Proofs.Frame
```

**NOT FIXED — C10 source approvals.** Every listed Helper still has `PENDING` as
its approval reference. The spec worker must record the actual independent review
reference. No additional writer/reviewer identity tags are required by the final
convention. The gate cannot authenticate an external review reference.

```text
FAIL unresolved-approval ConcreteSpec.Core.Frame.ABI.artifact_stack_account PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.ABI.call_stack_account PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.ABI.callee_saved_registers_equal PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.ABI.caller_stack_slots_equal PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.ABI.checkKeptPair_result PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.ABI.clobbers_only_changed_registers PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.ABI.encodeToyExit_correct PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.ABI.extern_result_rejects_unrelated_retention PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.ABI.frame_size_aligned PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.ABI.frame_within_stack_bound PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.ABI.keptPair_results_valid PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.ABI.kept_only_paired_completion PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.ABI.kept_receipt_rejects_other_extern PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.ABI.kept_receipt_rejects_submission PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.ABI.launcher_entry_exit PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.ABI.launcher_run_admitted PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.ABI.machine_preserved_trans PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.ABI.machine_registerFrame_union PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.ABI.retainChain_complete PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.ABI.retainChain_length PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.ABI.retainChain_membership PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.ABI.stack_adequate PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.ABI.stack_pointer_restored PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.ABI.stack_under_reservation PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.ABI.submit_produces_paired_receipt PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.ABI.toyTransition_conforms PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.ABI.typed_arguments_at_entry PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.ABI.typed_result_at_return PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.alloc_frame PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.alloc_fresh_disjoint PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.blockOutside_block PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.blockOutside_union PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.byteOutside_union PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.c1Call_restores_caller PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.c1_call_frame_of_implementation PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.c1_steps_frame PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.expose_frame PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.free_frame PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.placeOutside_union PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.preserves_covers PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.preserves_refl PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.preserves_seq PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.preserves_trans PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.rawWrite_frame PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.return_cleanup_restores_caller PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.runStoringCall_result PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.set_frame PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.step_call_saves_caller PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.step_return_restores_caller PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.store_frame PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.write_materialized_frame PENDING
FAIL unresolved-approval ConcreteSpec.Core.Frame.write_structured_frame PENDING
FAIL unresolved-approval _private.ConcreteSpec.Proofs.Frame.0.ConcreteSpec.Core.Frame.c1ExplicitCall PENDING
FAIL unresolved-approval _private.ConcreteSpec.Proofs.Frame.0.ConcreteSpec.Core.Frame.metadataStoreWellFormed PENDING
```

**NOT FIXED — unresolved backticked names.** Dot-constructor notation and a local
`unitVal` are not resolved declaration names. Source prose must use actual resolved
names when it uses the convention’s backticks.

```text
FAIL unknown-name ConcreteSpec.Core.Frame.runStoringCall_result .done
FAIL unknown-name ConcreteSpec.Core.Frame.step_call_saves_caller .ret
FAIL unknown-name ConcreteSpec.Core.Frame.step_return_restores_caller .ret
FAIL unknown-name ConcreteSpec.Core.Frame.«⟪example⟫ConcreteSpec/Proofs/Frame.lean:457» .ret
FAIL unknown-name ConcreteSpec.Core.Frame.«⟪example⟫ConcreteSpec/Proofs/Frame.lean:483» .ret
FAIL unknown-name ConcreteSpec.Core.Frame.«⟪example⟫ConcreteSpec/Proofs/Frame.lean:483» unitVal
```

**NOT FIXED — open/partial Instances.** `caller_restoration` and
`ABI.stackSamples_bounded` leave binders hidden behind `CallerRestoration` and
`StackSafe`. `c4_effects_frame` and `c1_call_frame` leave explicit binders. These
examples do not provide all closed arguments and hypothesis proofs. They must
become full applications with concrete arguments; the corresponding Planted
examples then need the premise-preserving counterexample at exactly those arguments.
The current negated universal examples are not accepted as substitutes.

```text
FAIL instance-application ConcreteSpec.Core.Frame.ABI.«⟪example⟫ConcreteSpec/Proofs/Frame.lean:845» 
FAIL instance-application ConcreteSpec.Core.Frame.«⟪example⟫ConcreteSpec/Proofs/Frame.lean:502» 
FAIL instance-application ConcreteSpec.Core.Frame.«⟪example⟫ConcreteSpec/Proofs/Frame.lean:681» 
FAIL instance-application ConcreteSpec.Core.Frame.«⟪example⟫ConcreteSpec/Proofs/Frame.lean:754» 
FAIL missing-instance ConcreteSpec.Core.Frame.ABI.stackSamples_bounded 
FAIL missing-instance ConcreteSpec.Core.Frame.c1_call_frame 
FAIL missing-instance ConcreteSpec.Core.Frame.c4_effects_frame 
FAIL missing-instance ConcreteSpec.Core.Frame.caller_restoration 
FAIL unpaired-planted ConcreteSpec.Core.Frame.ABI.«⟪example⟫ConcreteSpec/Proofs/Frame.lean:854» 
FAIL unpaired-planted ConcreteSpec.Core.Frame.«⟪example⟫ConcreteSpec/Proofs/Frame.lean:511» 
FAIL unpaired-planted ConcreteSpec.Core.Frame.«⟪example⟫ConcreteSpec/Proofs/Frame.lean:696» 
FAIL unpaired-planted ConcreteSpec.Core.Frame.«⟪example⟫ConcreteSpec/Proofs/Frame.lean:760» 
```

The missing-instance and unpaired-planted findings above follow from the four
rejected Instances. Their Planteds cannot be compared with an exact argument vector
until those Instances are repaired. This report does not assert that those Planteds
will pass after repair. No recognized projection-law finding was emitted by v1;
that is not a claim that v1 decides arbitrary semantic vacuity.

The C10 worktree was not edited. These findings go to the spec worker. N2 was not
weakened to make this candidate pass. Migration and required CI promotion remain
owned by R-0219 as described in [README.md](README.md).
