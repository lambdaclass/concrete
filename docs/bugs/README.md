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

## Open Numbered Bugs

- [027_emitssa_quadratic_rendering.md](027_emitssa_quadratic_rendering.md) — EmitSSA renders SSA→LLVM text in O(n²) (perf; large functions slow to codegen)
- [049_reduce_crash_predicate_vacuous.md](049_reduce_crash_predicate_vacuous.md) — `concrete reduce --predicate crash` reduces anything to an empty program (reducer audit 2026-07-18)
- [052_array_element_destroy_noop.md](052_array_element_destroy_noop.md) — Vec<[T; N]>.drop() skips element destruction via synthesized no-op T_destroy (middle-end audit 2026-07-18)
- [053_dce_deletes_checked_negation.md](053_dce_deletes_checked_negation.md) — discard(-x) at MIN loses the documented trap (middle-end audit 2026-07-18)
- [054_struct_mono_name_collision.md](054_struct_mono_name_collision.md) — user types shadow generated specializations (middle-end audit 2026-07-18)
- [055_sibling_import_alias_unusable.md](055_sibling_import_alias_unusable.md) — project sibling `import a.{x as y}` emits undefined callee (middle-end audit 2026-07-18)
- [056_fnptr_reassign_phi_undefined_register.md](056_fnptr_reassign_phi_undefined_register.md) — reassigning a fn-pointer local across a branch emits a phi over Lower's `@fnref.X` sentinel; valid program refused with E0709 (found building the R-0002 gate, 2026-07-25)
- [058_proof_by_without_fingerprint_never_stales.md](058_proof_by_without_fingerprint_never_stales.md) — contained: a missing stored fingerprint is now `unbound`, never `proved`; the full subject digest remains R-0004 work (2026-07-25)
- [059_body_fingerprint_omits_signature_and_types.md](059_body_fingerprint_omits_signature_and_types.md) — the body hash drops declared types and never sees the signature; `i32 -> u32` keeps a proof `proved` (R-0004, 2026-07-25)
- [060_contracts_outside_proof_fingerprint.md](060_contracts_outside_proof_fingerprint.md) — `#[ensures]` is outside the hash, so a FALSE postcondition still reports `proved` (R-0004, 2026-07-25)
- [061_pexpr_conflates_param_application_with_global_call.md](061_pexpr_conflates_param_application_with_global_call.md) — the proof model spells a parameter application and a global call identically; latent, filed under principle 12 (2026-07-25)

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
