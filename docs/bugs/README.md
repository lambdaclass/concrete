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

## Open Numbered Bugs

- [018_stack_array_borrow_creates_copy.md](018_stack_array_borrow_creates_copy.md)

## Still Open, But Not Numbered Bugs

These are real problems, but they are not all concrete compiler defects. Track them as Phase H findings / roadmap items until they either become fixed or are reduced to a reproducible bug:

- formatting / interpolation
- runtime-oriented collection maturity
- runtime / stack pressure classification
