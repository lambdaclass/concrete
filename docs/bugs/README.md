# Bug Ledger

Status: reference

This directory is the stable ledger for concrete, numbered bugs discovered by real use, tests, or review.

Use it for:

- compiler/runtime/codegen bugs with a concrete failure mode
- the current fix status of those bugs
- a pointer to the repro/regression coverage

Do not use it for:

- broad language-design questions
- stdlib maturity gaps
- package/tooling UX issues without a concrete compiler/runtime defect

Those belong in:

- [ROADMAP.md](../../ROADMAP.md)
- [research/workloads/phase-h-findings.md](../../research/workloads/phase-h-findings.md)
- the relevant research note under `research/`

## Filing a bug

A bug record is `docs/bugs/NNN_short_name.md` (the next unused number), an entry in
`scripts/tests/audit_bug_corpus.sh` (`BUG_TEST_MAP` with a fixture, or `SKIP_BUGS` with a rationale
naming the coverage), and a line in the open or fixed list below. Every time a bug is found, ask
whether an underlying problem in the compiler's design produced it, and record the answer: a fix
that repairs one spelling of a recurring cause leaves the next spelling open. The record has these
parts:

```
# Bug NNN — one-line statement of the defect

**Status:** OPEN | FIXED (branch, commit)
**Found:** date, by what (workload, hunt, review, example)
**Severity:** what goes wrong for a user (wrong code, unsound claim, crash, refusal)

## Reproducer        the smallest program or command, its observed and expected result
## Cause             the code that produces it (file, function)
## Fix               (when fixed) what changed, and the permanent regression leg
## Structural cause  the design fact that let this bug exist: which missing model, duplicated
                     authority, name-based join or fail-open default it is an instance of, and
                     the other bugs of that class
## Structural owner  the roadmap task, exit or verified-pipeline stage that removes the cause
                     for the whole class (not only this spelling), or a proposed owner
```

`Structural cause` and `Structural owner` are required for every bug numbered 102 or higher
(`scripts/tests/check_bug_filing_complete.sh` fails without them). The classes found so far, with
their owners, are in [STRUCTURAL_CLASSES.md](STRUCTURAL_CLASSES.md); a new bug either joins a class
there or adds one. Whether a new bug is repaired now or contained until its owner lands follows the
roadmap's defect disposition rule ([execution queue](../../ROADMAP.md#the-current-execution-queue)).

## Fixed Bugs

- [001_cross_module_struct_field_offset.md](001_cross_module_struct_field_offset.md)
- [002_i32_literal_type_mismatch.md](002_i32_literal_type_mismatch.md)
- [003_cross_module_mut_borrow_move.md](003_cross_module_mut_borrow_move.md)
- [004_array_variable_index_assign.md](004_array_variable_index_assign.md)
- [005_enum_field_struct_layout_panic.md](005_enum_field_struct_layout_panic.md)
- [006_cross_module_string_literal_collision.md](006_cross_module_string_literal_collision.md)
- [007_no_print_string_builtin.md](007_no_print_string_builtin.md)
- [008_if_else_expression_aggregate_types.md](008_if_else_expression_aggregate_types.md)
- [009_const_declarations_not_lowered.md](009_const_declarations_not_lowered.md)
- [010_no_string_substr.md](010_no_string_substr.md)
- [011_linear_string_building_in_loops.md](011_linear_string_building_in_loops.md)
- [012_no_standalone_timing_path.md](012_no_standalone_timing_path.md)
- [013_alloca_inside_loops_stack_overflow.md](013_alloca_inside_loops_stack_overflow.md)
- [014_string_literal_in_loop_invalid_ir.md](014_string_literal_in_loop_invalid_ir.md)
- [015_O0_default_distorted_real_workload_performance.md](015_O0_default_distorted_real_workload_performance.md)
- [016_cross_module_generic_monomorphization_link_failure.md](016_cross_module_generic_monomorphization_link_failure.md)
- [017_std_net_socket_constants_are_linux_only.md](017_std_net_socket_constants_are_linux_only.md)
- [018_stack_array_borrow_creates_copy.md](018_stack_array_borrow_creates_copy.md)
- [019_array_struct_field_mutation.md](019_array_struct_field_mutation.md)
- [020_int_match_consume_not_propagated.md](020_int_match_consume_not_propagated.md)
- [021_int_match_disagree_not_checked.md](021_int_match_disagree_not_checked.md)
- [022_submodule_breaks_parent_impl_method_consume.md](022_submodule_breaks_parent_impl_method_consume.md) — found by #35 conlog workload
- [023_shortcircuit_aggregate_phi.md](023_shortcircuit_aggregate_phi.md) — found by #35 conlog workload
- [024_recursive_struct_infinite_size.md](024_recursive_struct_infinite_size.md) — found by panic-to-diagnostic probing; recursive struct → E0583 instead of llvm-as leak
- [025_no_main_linker_error.md](025_no_main_linker_error.md) — no `main` in an executable build → clean `error[link]` instead of an ld leak
- [026_array_repeat_count_hang.md](026_array_repeat_count_hang.md) — huge `[v; N]` repeat count → clean parse diagnostic instead of O(N²) hang/OOM
- [028_reserved_name_collision.md](028_reserved_name_collision.md) — user fn named `user_main`/`__*` → clean `reserved identifier` instead of an LLVM duplicate-symbol leak
- [029_divergent_if_array_addr_after_loop.md](029_divergent_if_array_addr_after_loop.md)
- [030_nonmut_array_write.md](030_nonmut_array_write.md)
- [031_branch_lazy_promotion_uninit.md](031_branch_lazy_promotion_uninit.md) — found by workload 1 (base64_cli)
- [032_multibyte_string_literal_emit.md](032_multibyte_string_literal_emit.md)
- [033_ifexpr_merge_aggregate_phi.md](033_ifexpr_merge_aggregate_phi.md) — found by workload 2 (png_chunks)
- [034_shortcircuit_borrow_promotion.md](034_shortcircuit_borrow_promotion.md) — found by std.cli v1 (031 class, third site)
- [035_fieldoffset_generic_enum_panic.md](035_fieldoffset_generic_enum_panic.md) — layout made program-wide (own-module priority)
- [036_import_dependent_type_metadata.md](036_import_dependent_type_metadata.md) — import closure: type metadata travels with the type
- [037_repr_align_exceeds_declared_type.md](037_repr_align_exceeds_declared_type.md) — repr(align(N>natural)) → E0585 fail-closed; found by the audit-3/3 enum work
- [038_if_merge_promoted_aggregate_clobber.md](038_if_merge_promoted_aggregate_clobber.md) — found by the extended differential fuzzer (string in branch arm); 4th merge-loop instance
- [039_import_alias_program_order_rebind.md](039_import_alias_program_order_rebind.md)
- [040_corecheck_match_binder_first_match_scope.md](040_corecheck_match_binder_first_match_scope.md)
- [041_match_merge_keeps_arm_binders.md](041_match_merge_keeps_arm_binders.md)
- [042_imported_newtype_not_a_type.md](042_imported_newtype_not_a_type.md)
- [043_string_not_nul_terminated_ffi.md](043_string_not_nul_terminated_ffi.md)
- [044_renamed_generic_import_not_monomorphized.md](044_renamed_generic_import_not_monomorphized.md)
- [045_match_binder_shadow_clobber.md](045_match_binder_shadow_clobber.md)
- [046_map_keys_values_duplicate_linear.md](046_map_keys_values_duplicate_linear.md)
- [047_hashmap_insert_duplicate_past_tombstone.md](047_hashmap_insert_duplicate_past_tombstone.md) — fixed by R-0003
- [048_hashmap_find_slot_hang_no_empty_slots.md](048_hashmap_find_slot_hang_no_empty_slots.md) — fixed by R-0003
- [050_mono_indirect_call_hijack.md](050_mono_indirect_call_hijack.md) — fixed by R-0002
- [051_generic_enums_not_monomorphized.md](051_generic_enums_not_monomorphized.md) — fixed by R-0001
- [057_hashmap_builtin_size_undercount.md](057_hashmap_builtin_size_undercount.md) — fixed in the R-0003 slice
- [053_dce_deletes_checked_negation.md](053_dce_deletes_checked_negation.md) — fixed in R-0005: the unary trap inventory moved into `IntArith`, so DCE stops deciding locally whether `-x` can trap
- [056_fnptr_reassign_phi_undefined_register.md](056_fnptr_reassign_phi_undefined_register.md) — fixed in R-0436: a function reference became `SVal.fnRef` and a call target became `SCallee`, retiring both string encodings
- [061_pexpr_conflates_param_application_with_global_call.md](061_pexpr_conflates_param_application_with_global_call.md) — fixed in R-0442: `PExpr.applyVar` + a `FnTable.callables` namespace; filed as latent, but the witness was already load-bearing in three shipped HOF proofs
- [062_proof_dependency_staleness_does_not_propagate.md](062_proof_dependency_staleness_does_not_propagate.md) — fixed in R-0004 slice 3: a reachable non-current dependency downgrades its dependent to `deps_not_current`, at one hop and transitively
- [072_spawn_exec_failure_flushes_parent_buffers.md](072_spawn_exec_failure_flushes_parent_buffers.md) — fixed 2026-10-01: a child whose exec failed called `exit`, flushing stdio buffers copied from the parent, so pending output was written twice; it now calls `_exit`
- [073_generic_type_argument_arity_unchecked.md](073_generic_type_argument_arity_unchecked.md) — fixed 2026-10-01: `Box1<i32, bool>` for a one-parameter struct compiled with the extra argument dropped; Resolve now checks type-argument arity (E0113)
- [074_duplicate_module_names_across_packages.md](074_duplicate_module_names_across_packages.md) — fixed 2026-10-04: two packages defining the same top-level module name failed only in LLVM validation, and report modes merged their declarations; `loadProject` now refuses the collision naming both packages
- [075_policy_assumption_authority_checks_vacuous.md](075_policy_assumption_authority_checks_vacuous.md) — fixed 2026-10-06: `check_policy`/`check_assumptions` scraped only parenthesised report tokens and never saw a capability (and `Net` was not a capability); now read structured facts, validate names, and carry fail/pass controls
- [076_proof_admission_blind_to_dependencies.md](076_proof_admission_blind_to_dependencies.md) — fixed 2026-10-06: proof admission used a program-only call graph, so indirect calls and foreign bindings inside dependencies, and type-parameter dispatch, were admitted as effect-free; now reads the shared assumption summary, with fixture, gate, #guards and mutations
- [077_nested_submodule_relative_call_undefined_symbol.md](077_nested_submodule_relative_call_undefined_symbol.md) — OPEN: a call `util::poke` from `tb.a` into `tb.a.util` lowers to the undefined `@util_poke` (LLVM validation fails); the assumption summary over-resolves the same call
- [078_if_else_both_diverge_open_merge.md](078_if_else_both_diverge_open_merge.md) — FIXED: an if/else (statement or expression) whose arms both diverge left its merge block open, so an enclosing branch used the unreachable block as a phi source and SSA verification rejected a valid program (E0703)
- [079_receipt_for_a_body_the_theorem_is_not_about.md](079_receipt_for_a_body_the_theorem_is_not_about.md) — FIXED: a body was compared with a specification only when its source name keyed a side table, so `fn always_seven() { return 7; }` linked to `check_nonce_correct` was proved and minted a receipt; one subject binding (body and parameters ARE the specification the theorem evaluates) now governs proved, issuance and consumption
- [080_narrowing_cast_modeled_as_identity.md](080_narrowing_cast_modeled_as_identity.md) — FIXED: `PExpr.cast` was the identity, so `x as u8` was 300 in the proof model and 44 in the binary; value-changing casts now extract as `wrapCast` (the runtime's wrap formula), affected proofs show their values fit, and `check_model_diff.sh` compares model and runtime on boundary inputs across the corpus
- [084_enum_arm_binders_bound_by_name_in_model.md](084_enum_arm_binders_bound_by_name_in_model.md) — FIXED: the proof model bound match-arm binders by name (and bound 0 when no field had the binder's name) while Elab, the interpreter and the binary bind by position, so `Box::Full { v } => v` was 0 in the model and `x` in the program; the model now binds positionally and fails rather than fabricate, pinned by `check_model_diff.sh` leg E
- [085_loop_step_reads_pre_iteration_env.md](085_loop_step_reads_pre_iteration_env.md) — FIXED: a `while_step` loop body's model built every `Cont` field from the pre-iteration env and ended a fall-through `if` early, so `x = x + 1; x = x + 1;` stepped by 1 and `seq(7)` was 21 vs 28; assignments now bind sequentially and branches continue with the rest of the body; pinned by `check_model_diff.sh` leg S
- [086_discarded_statement_modeled_as_value.md](086_discarded_statement_modeled_as_value.md) — FIXED: a discarded `id(x);` was modeled as its block's value (`g(5)` 5 vs 7) and a `return` inside a `let` initializer as a value (11 vs 1); only `isValue` trailing expressions are values and non-tail expressions must not escape; pinned by `check_model_diff.sh` leg T
- [101_branch_local_let_captures_continuation.md](101_branch_local_let_captures_continuation.md) — FIXED: a branch-local `let` was nested over the code after its branch (and a loop body's own `let` written back through `Cont`), so `scoped(1)` was 10 in the model vs 1; a block's locals may not be named by its continuation; pinned by `check_model_diff.sh` leg S
- [088_unchecked_default_literal_and_unnormalized_fold.md](088_unchecked_default_literal_and_unnormalized_fold.md) — FIXED: an unhinted literal defaulted to an `Int` with no range check (`let x = 18446744073709551615` was 2^64-1 interpreted, -1 compiled), a literal at a type variable was never range-checked after Mono, and the SSA folder compared unnormalized constants (`0xFFFFFFFFFFFFFFFF as i64 < 0` folded false); literals now fit their defaulted/instantiated type (E0227, both paths) and the folder normalizes through `IntArith.wrapToType`
- [090_match_guard_moves_linear.md](090_match_guard_moves_linear.md) — FIXED: a match guard could move a linear value; when the guard was false, a later arm moved it again (use after free, double free). Guards are read-only (E0300)
- [092_reference_stored_in_aggregate.md](092_reference_stored_in_aggregate.md) — FIXED: a reference stored in a struct field, enum payload or array did not freeze its source, so `let w = W { t: &s }; s.drop(); let W { t } = w;` read freed memory. References are never stored in an aggregate (E0239)
- [093_shadowing_local_escapes_scope_check.md](093_shadowing_local_escapes_scope_check.md) — FIXED: a block local or match binder that shadowed an outer name was skipped by the never-consumed check (E0208), so it leaked; checker bindings now carry an identity and scope/merge checks match by it, not by name
- [094_binding_state_updates_keyed_by_name.md](094_binding_state_updates_keyed_by_name.md) — FIXED: per-binding state updates (consume, use, reassign, `defer`, borrow freeze) marked every entry sharing the name, so a binder shadowing a live linear binding consumed it too and the outer value leaked; state updates now match by binding identity (arm binders may shadow; the outer binding is still owed after the match) and a destructure binder refuses to shadow a live non-Copy binding (E0292)
- [081_call_key_not_bound_to_callee.md](081_call_key_not_bound_to_callee.md) — FIXED: the proof model's `.call "k"` means the theorem table's entry under `k`, but edges were justified by table CONTAINMENT, so `import main.{ check_version as check_class }` borrowed `validate_header_correct`; every call is now bound by key to the callee the elaborator resolved, and the stale-fingerprint warning prints the v2 value to pin
- [083_name_keyed_proof_fallback.md](083_name_keyed_proof_fallback.md) — FIXED: a compiler-side table keyed by function NAME (`Proof.provedFunctions`) attached a claim to any program defining `main.compute_tag` and friends, so `fn compute_tag(…: u8) -> u8` with no proof attributes was reported proved while its binary aborts; the table is deleted, every program it served now links in source with a v2 pin, and `regress_083_no_claim_by_name.con` must report no claim
- [089_ensures_discharged_by_a_link.md](089_ensures_discharged_by_a_link.md) — FIXED: `#[ensures_proof(No.Such.Theorem)]` made a false `#[ensures]` read `proved_by_lean` on contracts, vcs, ledger and audit with no Lean run, and `check-proofs` printed ✓ for any type-checking theorem; one verdict (`ensuresVerdictOf`) now decides every surface, and no link or coverage label discharges a postcondition until Exact property acceptance exists
- [121_enum_llvm_layout_disagrees.md](121_enum_llvm_layout_disagrees.md) — FIXED (branch `bug121-conlog-segv`): a payload-less enum was 8 bytes in LLVM and 4 to the compiler, and an align-8 payload enum 4-aligned in LLVM, so a struct holding one was laid out twice and every aggregate copy moved its fields; `examples/conlog` freed a String pointer rebuilt from padding (SIGSEGV on CI, intermittent on x86_64) and miscounted errors; `check_enum_union_layout.sh` leg 5 now compares LLVM's layout of every emitted type with the compiler's
- [052_array_element_destroy_noop.md](052_array_element_destroy_noop.md) — FIXED 2026-10-18: `Vec<[T; N]>.drop()` skipped element destruction via a synthesized no-op `T_destroy`; Mono now synthesizes per-element glue from the argument type, Check no longer accepts every unmatched type as destroyable (E0241), and an unresolved or destructor-less type is refused (E0814) instead of no-oped
- [065_stack_depth_unboundedness_not_propagated.md](065_stack_depth_unboundedness_not_propagated.md) — FIXED 2026-10-18: per-function unboundedness already propagated to callers (3d9b63a5), but the `Max stack bound:` summary still maximised over the bounded subset, so a recursive program printed `20 bytes (leaf)` and a `max_stack_bytes` / `stack_max_bytes` budget passed it; the summary now reads `unbounded` when any function is, and the policy gate, the assumptions gate and the release bundle read it through one parser (`scripts/tests/lib/stack_bound.sh`) that refuses it. Gated by `check_stack_budget.sh`
- [055_sibling_import_alias_unusable.md](055_sibling_import_alias_unusable.md) — fixed 2026-10-18 with bug 082: a project sibling `import a.{x as y}` emitted an undefined callee
- [082_import_alias_loses_to_same_named_definition.md](082_import_alias_loses_to_same_named_definition.md) — FIXED: `import a.{check_version as check_class}` ran another module's `check_class` on both backends, because the alias spelling went into Core and every consumer resolved it against the whole program. Elab now writes the definition's spelling, and a module binding one name twice (an import and a local function, or two imports) is E0299

## Open Numbered Bugs

- [027_emitssa_quadratic_rendering.md](027_emitssa_quadratic_rendering.md) — EmitSSA renders SSA→LLVM text in O(n²) (perf; large functions slow to codegen)
- [049_reduce_crash_predicate_vacuous.md](049_reduce_crash_predicate_vacuous.md) — `concrete reduce --predicate crash` reduces anything to an empty program (reducer audit 2026-07-18)
- [054_struct_mono_name_collision.md](054_struct_mono_name_collision.md) — user types shadow generated specializations (middle-end audit 2026-07-18)
- [058_proof_by_without_fingerprint_never_stales.md](058_proof_by_without_fingerprint_never_stales.md) — contained: a missing stored fingerprint is now `unbound`, never `proved`; the full subject digest remains R-0004 work (2026-07-25)
- [059_body_fingerprint_omits_signature_and_types.md](059_body_fingerprint_omits_signature_and_types.md) — the body hash drops declared types and never sees the signature; `i32 -> u32` keeps a proof `proved` (R-0004, 2026-07-25)
- [060_contracts_outside_proof_fingerprint.md](060_contracts_outside_proof_fingerprint.md) — `#[ensures]` is outside the hash, so a FALSE postcondition still reports `proved` (R-0004, 2026-07-25)
- [063_cap_inference_defaults_derived_fnptr_to_empty.md](063_cap_inference_defaults_derived_fnptr_to_empty.md) — cap-variable inference records "unknown argument type" as "no capabilities", so a fn pointer read from a struct field, call result or array element cannot reach a `cap C` parameter; misreported as E0220 against a `with()` the program never wrote (R-0470, 2026-07-27)
- [064_query_kind_error_lists_the_rejected_kind.md](064_query_kind_error_lists_the_rejected_kind.md) — every bare semantic `--query KIND` exits 1 with "unknown query kind 'predictable'" and then lists `predictable` first among known kinds; the arity is wrong, not the kind, so no repair is derivable from the message (R-0471, surface audit 2026-07-31)
- [069_aliased_imported_type_unresolvable.md](069_aliased_imported_type_unresolvable.md) — an aliased imported struct/enum was registered under its definition-site spelling, so the importer's local alias did not resolve (FIXED 2026-08-02; **renumbered from 064**, which a second, unrelated bug already held)
- [070_builtin_enum_list_built_twice.md](070_builtin_enum_list_built_twice.md) — `builtinEnumList` built twice and `hasUserResult` computed from different inputs, so Elab and Check held different beliefs about a user `Result` (FIXED 2026-08-02; **renumbered from 065**, which a second, unrelated bug already held)
- [071_cross_package_capability_requirements_unenforced.md](071_cross_package_capability_requirements_unenforced.md) — a capability declared in a dependency did not bind on its caller: a function declaring nothing read `$HOME` through `std.env.get`. Only hardcoded intrinsics were enforced across a package boundary (OPEN, partially repaired 2026-09-21)
- [102_pointer_local_address_cast_miscompiled.md](102_pointer_local_address_cast_miscompiled.md) — `&mut x as *mut *mut T` on a pointer-typed local passes `x`'s value, not its address (silent wrong code; the interpreter refuses the cast, so differential testing misses it). Found by RocksDB slice 1 groundwork, 2026-10-20 (OPEN)
- [103_extern_destroy_replaced_by_empty_body.md](103_extern_destroy_replaced_by_empty_body.md) — Mono's drop-glue synthesizes an empty definition for any unresolved callee ending in `_destroy`, so an `extern fn foo_destroy` never reaches the C library (silent leak). Found by RocksDB slice 1 groundwork, 2026-10-20 (OPEN)
- [104_assigned_reference_outlives_block_local.md](104_assigned_reference_outlives_block_local.md) — `leaked = &h` from an inner block records no borrow, so `h` can be freed (or go out of scope) while the outer reference is still read: use after free in safe code. Found by the V1 design examples (B6). Joins exit 0b.
- [107_shadowed_binding_reference_reads_new_slot.md](107_shadowed_binding_reference_reads_new_slot.md) — `let x = 1; let r = &x; let x = 2; *r` is 1 interpreted and 2 compiled: a shadowing `let` keeps the Core name, so `Lower` stores it into the shadowed binding's slot (silent wrong code on main); the reference-visible form of 087, whose `land2-fix` repair should close it

- [108_bool_cast_trunc.md](108_bool_cast_trunc.md) — OPEN: a `bool` cast to a wider integer is emitted as `trunc i1` (examples-health, 2026-10-20)
- [109_f32_literal_invalid_ir.md](109_f32_literal_invalid_ir.md) — OPEN: an `f32` literal that is not exactly representable is emitted as a decimal LLVM `float` constant (examples-health, 2026-10-20)
- [110_array_value_store.md](110_array_value_store.md) — OPEN: an array value is stored as an aggregate where the SSA value is a pointer (examples-health, 2026-10-20)
- [111_main_params_accepted.md](111_main_params_accepted.md) — OPEN: `main` may declare parameters, and the wrapper calls it with none (examples-health, 2026-10-20)
- [112_user_intrinsic_e0116.md](112_user_intrinsic_e0116.md) — OPEN: the declaration std.mem uses for `sizeof` is rejected (E0116 at 0:0) outside std (examples-health, 2026-10-20)
- [113_submodule_extern_effects_unchecked.md](113_submodule_extern_effects_unchecked.md) — OPEN: R-0484 R3 (every foreign binding declares its effects) is not enforced in nested submodules (examples-health, 2026-10-20)
- [114_submodule_intrinsic_links_c_symbol.md](114_submodule_intrinsic_links_c_symbol.md) — OPEN: an intrinsic declared in a nested submodule and called by path lowers to a C call (examples-health, 2026-10-20)
- [115_vec_get_duplicates_linear.md](115_vec_get_duplicates_linear.md) — OPEN: the builtin `vec_get` returns a non-Copy element by value, duplicating a linear value (examples-health, 2026-10-20)
- [116_vec_push_first_field_through_ref.md](116_vec_push_first_field_through_ref.md) — OPEN: `vec_push` on the first field of a struct reached through a `&mut` parameter calls an undefined `@vec_push` (examples-health, 2026-10-20)
- [117_interp_module_const.md](117_interp_module_const.md) — OPEN: the interpreter does not resolve module-level constants (examples-health, 2026-10-20)
- [118_interp_nested_module_import.md](118_interp_nested_module_import.md) — OPEN: the interpreter cannot call a function imported from a nested module (examples-health, 2026-10-20)
- [119_interp_generic_trait_method.md](119_interp_generic_trait_method.md) — OPEN: the interpreter cannot call a trait method on a type parameter (examples-health, 2026-10-20)
- [120_interp_field_borrow_through_ref.md](120_interp_field_borrow_through_ref.md) — OPEN: the interpreter cannot borrow a field through a reference parameter (examples-health, 2026-10-20)
- [122_builtin_enum_canonical_layout.md](122_builtin_enum_canonical_layout.md) — OPEN: the builtin `Option`/`Result` are one LLVM union sized for the largest instantiation while the compiler lays out each instantiation (and a user enum named `Option`/`Result`) by its own size, so aggregate copies move or overrun fields (bug 121's layout probe, 2026-10-20)
- [123_const_borrow_uninitialized_slot.md](123_const_borrow_uninitialized_slot.md) — OPEN: `const R: &Int = &X;` is accepted (REF-3 names constants) and `&X` lowers to an uninitialized stack slot, so `get(R)` exits 0 where 42 is read (silent wrong code); `land2-fix` refuses uses of `R` but compiles `&X` in a body to the same slot (exit 2i specification, G1, 2026-10-21). Joins exit 0b.
- [124_duplicate_parameter_names.md](124_duplicate_parameter_names.md) — OPEN: `fn f(a: Int, a: Int)` passes the checker and fails in `llvm-as` ("redefinition of argument"), while `--interp` runs it; also on `land2-fix` (exit 2i specification, G3, 2026-10-21). Joins exit 2c; an input to 2i SCOPE-1.
- [127_struct_literal_field_order.md](127_struct_literal_field_order.md) — OPEN: the binary evaluates struct-literal initializers in declaration order (`Lower` reorders them) while `--interp` follows the ratified source order, so `P { b: tick(5), a: tick(6) }` leaves 65 natively and 56 in the interpreter (silent wrong code; semantics probes N1, 2026-10-21). Joins exit 0b.
- [128_interp_int_to_float_identity.md](128_interp_int_to_float_identity.md) — OPEN: the interpreter models `<int> as f64` as the identity, so `(2^53 + 1 as f64) as Int` is 2^53 + 1 under `--interp` and 2^53 natively (silent wrong value in the differential reference; semantics probes N2, 2026-10-21). Joins exit 0b.
- [129_user_fn_captures_libc_symbol.md](129_user_fn_captures_libc_symbol.md) — OPEN: user functions are emitted under bare names, so a safe `fn malloc` becomes the program's allocator and `alloc` calls it; the binary is killed by a signal (SIGABRT on arm64 macOS) (semantics probes N3, 2026-10-21). Joins exit 0b.
- [130_int_as_bool_invalid_ir.md](130_int_as_bool_invalid_ir.md) — OPEN: `x as bool` with `x: Int` passes the checker and fails in `llvm-as` (`bitcast i64 to i1`), and the interpreter refuses it; the reverse of bug 108, refused with E0553 by the unlanded `backend-108-116` (semantics probes N4, 2026-10-21). Joins exit 2c.
- [131_reference_parameter_repointed.md](131_reference_parameter_repointed.md) — OPEN: a reference parameter can be re-pointed at a block-local value (`r = &local`), and the binary reads the dead slot while `--interp` fails; the parameter variant of bug 104, refused with E0238 on `land2-fix` (semantics probes, 2026-10-21). Joins exit 0b.

(045/046 numbers used by the parallel session's fixes — match-binder alpha-rename and keys/values Copy-bounding — before these entries were filed; the keys/values double-free finding from the same audit is tracked there, fixed.)

## Still Open, But Not Numbered Bugs

These are real problems, but they are not all concrete compiler defects. Track them as Phase H findings / roadmap items until they either become fixed or are reduced to a reproducible bug:

- formatting / interpolation
- runtime-oriented collection maturity
- runtime / stack pressure classification

## Confirmed Defects Awaiting Fixture-Backed Numbering

Do not reserve a bug number from prose alone. These findings are owned by
roadmap tasks now and receive stable IDs when their minimal fixtures and bug
documents land:

- **Proof replay is working-directory-sensitive (R-0004).** The HMAC input
  reports 11 verified / 0 failed from the repository root, but 0 / 11
  `theorem_lookup` failures and `Toolchain: unknown` when the same compiler is
  invoked from `examples/hmac_sha256/`. The theorem files exist; replay context
  is the defect.
- **Capability-polymorphic callback inference rejects stored/derived values
  (R-0016).** Passing a function pointer reached through a field, index, or call
  result to a `cap C` parameter fails closed with misleading E0220
  `expected ... with()` instead of `.cannotInferCapVariable`. This is a
  higher-order usability/inference defect, not an authority escalation.
