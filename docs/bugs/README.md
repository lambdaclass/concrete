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

## Open Numbered Bugs

- [027_emitssa_quadratic_rendering.md](027_emitssa_quadratic_rendering.md) — EmitSSA renders SSA→LLVM text in O(n²) (perf; large functions slow to codegen)

## Still Open, But Not Numbered Bugs

These are real problems, but they are not all concrete compiler defects. Track them as Phase H findings / roadmap items until they either become fixed or are reduced to a reproducible bug:

- formatting / interpolation
- runtime-oriented collection maturity
- runtime / stack pressure classification
