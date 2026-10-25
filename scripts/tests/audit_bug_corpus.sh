#!/usr/bin/env bash
# Bug-to-regression corpus audit.
# Checks that every numbered bug in docs/bugs/ has regression test coverage.
#
# Each bug must either:
#   1. Have an entry in BUG_TEST_MAP (bug number -> test file name)
#   2. Be listed in SKIP_BUGS with a justification
#
# THE RECORDS OWN THESE MAPS. Since the ledger migration (scripts/gen/migrate_ledgers.sh) each bug's
# coverage is the `fixtures:` or `coverage:` line of the ledger block in its record,
# docs/bugs/NNN_*.md, and the maps below are built from them at run time by
# `scripts/gen/bug_ledger.py corpus` (never committed, so two branches filing bugs cannot conflict
# here). The inline arrays are the pre-migration form; once any record carries a ledger block they
# must be empty, and an entry left in them fails this audit.
#
# Run as a CI gate or manually:
#   bash scripts/tests/audit_bug_corpus.sh

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
BUGS_DIR="$ROOT_DIR/docs/bugs"
TESTS_DIR="$ROOT_DIR/tests/programs"
RUN_TESTS="$ROOT_DIR/scripts/tests/run_tests.sh"

PASS=0
FAIL=0
SKIP=0

# Explicit mapping: bug number -> regression test file(s) in tests/programs/.
# Multiple files separated by space.
declare -A BUG_TEST_MAP=(
  # Bug 206: refusals that had no diagnostic code (one per new code).
  [206]="error_206_newtype_field_other_than_zero.con error_206_newtype_constructor_arity.con error_206_receiverless_trait_method.con error_206_wrapping_on_non_integers.con error_206_print_without_arguments.con error_206_destructure_in_for_header.con error_206_test_with_parameter.con error_206_integer_requires.con error_206_spec_fn_calls_runtime_code.con"
  # Bug 204: a trait bound on a primitive type argument (E0241, TRAIT-10).
  [204]="error_204_primitive_bound_unchecked.con error_204_primitive_bound_unused.con regress_204_primitive_bound_satisfied.con"
  # Bug 205: a struct containing itself through a generic instantiation (E0583, TYPE-16).
  [205]="error_205_recursive_through_instantiation.con regress_205_instantiations_without_cycle.con"
  [091]="error_trait_impl_caps_exceed_generic.con error_trait_impl_caps_exceed_direct.con error_trait_bound_call_missing_cap.con error_trait_impl_sig_mismatch.con error_trait_method_generic.con trait_impl_caps_declared.con error_091_nested_destroy_caps.con error_091_generic_receiver_moved.con error_091_receiver_through_ref.con trait_generic_receiver_forms.con trait_impl_generic_self_return.con error_091_generic_self_return_mismatch.con trait_destroy_owned_receiver.con error_091_destroy_mut_receiver.con error_091_destroy_through_ref.con error_091_destroy_concrete_through_ref.con error_091_destroy_method_then_destroy.con error_091_mut_receiver_through_ref.con error_091_mut_receiver_through_ref_concrete.con error_091_mut_receiver_through_ref_projection.con error_091_mut_receiver_in_ref_self.con error_091_mut_receiver_immutable.con error_091_mut_receiver_immutable_concrete.con error_091_mut_receiver_borrowed.con trait_mut_receiver_forms.con error_091_destroy_paren_then_destroy.con error_091_destroy_static_borrowed.con error_091_destroy_deref_ref.con error_091_destroy_defname_borrowed.con error_091_receiver_deref_moved.con error_091_mut_receiver_paren_deref.con error_091_mut_receiver_deref_borrow.con error_091_receiver_deref_borrow_moved.con error_091_destroy_deref_if_ref.con error_091_mut_receiver_deref_if_ref.con error_091_destroy_newtype_through_ref.con error_091_mut_receiver_reborrow_shared.con error_091_mut_receiver_paren_borrow_live.con error_091_mut_borrow_explicit_through_ref.con error_091_mut_borrow_explicit_paren_immutable.con error_091_mut_borrow_explicit_projection_through_ref.con error_091_norm_temporaries.con error_091_destroy_defname_deref_arg.con error_091_destroy_defname_paren_arg.con error_091_mut_receiver_paren_borrow_outer.con error_091_mut_receiver_projection_borrow_live.con error_091_deref_ref_by_value.con error_091_paren_call_arg_not_consumed.con error_091_norm_if_projection.con error_091_destroy_deref_borrow_mut.con error_091_match_deref_ref_payload.con error_091_mut_receiver_deref_borrow_provenance.con error_091_borrow_provenance_unknown.con error_091_norm_if_local_binding.con error_091_mut_receiver_ref_to_mut_ref.con error_091_mut_receiver_reborrowed_mut_ref.con error_091_mut_receiver_reassigned_ref.con error_091_norm_if_local_chain.con error_091_ref_repointed_in_branch.con error_091_mut_receiver_paren_borrow_arg.con error_091_mut_receiver_deref_reborrow_arg.con error_091_norm_match_owned_binder.con error_091_newtype_payload_through_ref.con error_091_mut_receiver_deref_field_arg.con trait_destroy_newtype_payload.con error_091_mut_receiver_nested_reborrow_arg.con error_091_mut_receiver_if_reborrow_arg.con error_091_heap_field_through_ref.con trait_destroy_newtype_payload_paren.con error_091_mut_receiver_if_local_reborrow_arg.con error_091_ref_receiver_under_mut_reborrow.con trait_mut_receiver_nested_reborrow.con error_091_norm_heap_deref_payload.con error_091_norm_conditional_receiver.con error_091_norm_deref_read_under_reborrow.con error_091_norm_cast_arg_overlap.con error_091_norm_parent_arg_under_reborrow.con error_091_norm_copied_mut_ref.con error_091_norm_double_deref_heap.con error_091_norm_field_write_under_reborrow.con error_091_norm_borrow_block_under_live_ref.con error_091_norm_paren_heap_load.con error_091_norm_assign_under_shared_ref.con error_091_norm_heap_field_load.con error_091_norm_paren_array_write.con error_091_norm_double_deref_read.con error_091_norm_intrinsic_arg_under_reborrow.con error_091_norm_ref_array_overwrite.con error_091_norm_generic_ref_array_overwrite.con error_091_norm_catchall_binder_through_ref.con error_091_norm_enum_payload_ref.con error_091_norm_generic_field_ref_write.con error_091_norm_heap_ref_field_write.con error_091_norm_match_untraced_ref.con error_091_norm_alias_ref_field.con error_091_norm_nested_ref_binder.con error_091_norm_alias_cast_overlap.con error_091_norm_alias_cast_let.con error_091_norm_intrinsic_ref_result.con error_091_norm_temp_projection_ref.con error_091_norm_generic_nested_ref.con error_091_norm_generic_nested_alias_ref.con"
  [104]="error_104_assigned_ref_heap_block.con error_104_assigned_ref_stack_block.con"
  [099]="regress_099_generic_infer_field_index.con regress_099_generic_infer_call_results.con regress_099_generic_infer_cap_method.con"
  [100]="regress_100_generic_ref_destroy.con"
  [096]="regress_096_type_identity_same_named.con regress_096_imported_type_layout.con regress_096_local_and_imported_same_name.con regress_096_same_named_destroy.con regress_096_nested_same_named_methods.con regress_096_imported_struct_field_binding.con regress_096_method_value_same_named.con error_096_imported_sig_binds_other_type.con error_096_canonical_type_spelling_clash.con regress_096_import_closure_copy.con regress_096_generic_method_param_named_like_type.con error_096_method_spelling_clash.con regress_096_private_method_not_transported.con error_096_import_precedence_binding.con error_096_method_owner_exact.con regress_096_sibling_types_unused.con regress_096_method_owner_same_module.con error_096_chained_alias_signature.con regress_096_transitive_signature_binding.con error_096_qualified_call_other_type.con error_096_alias_signature_other_type.con regress_096_sibling_injected_binding.con error_096_generic_qualified_call_other_type.con error_096_fn_value_other_type.con error_096_enum_payload_binding_other_type.con error_096_const_initializer_other_type.con error_096_trait_dispatch_other_type.con error_096_extern_signature_other_type.con error_096_generic_trait_impl_dispatch_other_type.con regress_096_newtype_field_nominal.con regress_096_alias_typed_const.con error_096_newtype_unwrap_other_type.con regress_096_extern_alias_param.con regress_096_nested_newtype_construct.con regress_096_diverging_initializer.con regress_096_alias_copy_unused.con"
  [097]="regress_097_098_same_named_private_fns.con regress_097_param_named_like_colliding_fn.con regress_097_free_fn_named_like_method.con error_097_canonical_fn_spelling_clash.con regress_097_let_shadows_colliding_fn.con regress_097_colliding_fn_in_const.con"
  [098]="regress_097_098_same_named_private_fns.con error_098_nested_generic_collision.con error_098_nested_collision_same_tree.con"
  [127]="regress_127_struct_literal_source_order.con"
  [111]="error_111_main_params.con error_111_main_params_unit.con error_111_submodule_main_params.con"
  [123]="error_123_const_ref_type.con error_123_const_borrow_in_body.con error_123_const_field_borrow.con error_123_const_receiver_borrow.con error_123_const_index_borrow.con error_123_const_initializer_borrow.con error_123_const_initializer_array_borrow.con error_123_const_initializer_mut_arg.con error_123_const_initializer_elab_error.con"
  [131]="error_131_param_repointed_block_local.con error_131_param_repointed_param.con"
  # Bugs 102 and 126: one place rule for pointer-valued locals (`&x` is x's slot, Lower
  # promotes it) and two dereferences through `&Heap<S>`; 105: EmitSSA stores a reference
  # register as the reference, never as the aggregate it points at; 115: the builtin
  # vec_get / vec_set require a Copy element.
  [102]="regress_102_pointer_local_address.con regress_102_zero_sized_ptr_eq.con regress_102_ptr_eq_through_raw_base.con"
  [126]="regress_126_heap_ref_one_hop.con"
  [105]="regress_105_ref_valued_branch_generic.con error_105_ref_valued_if_arg.con error_105_ref_valued_if_field.con"
  [115]="error_115_vec_get_non_copy.con error_115_vec_get_generic_unbounded.con error_115_vec_set_non_copy.con"
  [124]="error_2i_scope1_duplicate_params.con regress_2i_scope1_duplicate_params_renamed.con"
  [132]="error_132_const_name_collision.con"
  [107]="error_107_shadow_under_live_ref.con error_107_shadow_in_block_under_live_ref.con"
  [073]="error_type_arg_count_extra.con error_type_arg_count_missing.con type_arg_count_ok.con"
  [001]="bug_cross_module_struct_field.con"
  [002]="bug_i32_literal_type.con"
  [003]="bug_cross_module_mut_borrow.con"
  [004]="bug_array_var_index_assign.con"
  [005]="bug_enum_in_struct.con"
  [007]="bug_print_builtins.con"
  [008]="bug_if_expression.con"
  [011]="bug_string_building.con"
  [012]="bug_clock_builtin.con"
  [013]="test_string_literal_in_loop.con"
  [014]="test_string_literal_in_loop.con"
  [016]="bug_cross_module_mut_borrow.con"
  [018]="bug_stack_array_borrow_copy.con"
  [019]="bug_array_struct_field_mutation.con"
  [020]="bug_int_match_consume.con"
  [021]="bug_int_match_disagree.con"
  [022]="submodule_linear_consume/src/main.con"
  [023]="scand_aggregate_in_scope/src/main.con"
  # R-0001: per-instantiation generic-enum mono. Both fixtures are positive
  # (run_ok) — they were E0808 rejections while only containment existed.
  # R-0002: a callee VALUE is never resolved by global name. Single-file witness
  # (42, was 21) + the std.io collision variant as a project fixture.
  [050]="regress_050_indirect_call_shadow.con regress_050_generic_f_std_io/src/main.con"
  [087]="regress_087_let_block_shadow.con regress_087_borrow_shadow_uaf/src/main.con regress_087_unpromoted_sibling.con regress_2i_scope1_087_renamed.con"
  # R-0003: one probe/occupancy slice — overwrite-past-tombstone (047) and the
  # zero-empty-slot missing lookup (048) share the same fixture.
  [047]="regress_047_048_hashmap_probe/src/main.con"
  [048]="regress_047_048_hashmap_probe/src/main.con"
  [051]="regress_generic_enum_051.con adversarial_mono_generic_enum.con"
  # Bug 057: a by-value HashMap must arrive whole (hash_fn for get, cap for drop).
  [057]="regress_057_hashmap_by_value/src/main.con"
  # Bug 090: a match guard moved a linear value a later arm moved again (E0300).
  # Bug 093: a shadowing block local / match binder escaped E0208 (name-keyed scope check).
  [093]="error_093_shadowing_block_local_leak.con error_093_shadowing_match_binder_leak.con regress_093_shadowing_locals_consumed.con"
  # Bug 094: name-keyed state updates let a binder consume the live binding it shadowed (E0209/E0208/E0292).
  [094]="error_094_arm_binder_shadows_live.con error_094_destructure_shadows_live.con error_094_value_binder_shadows_live.con regress_094_shadowing_binders.con regress_094_shadow_state_by_identity.con error_094_borrow_block_shadow_leak.con error_094_mut_ref_param_shadow_leak.con"
  # Bug 092: a reference stored in an aggregate outlived its source (E0239).
  [092]="error_092_ref_struct_field.con error_092_ref_in_option.con error_092_ref_in_array.con error_092_generic_box_ref.con error_092_generic_array_ref.con error_092_newtype_ref.con error_092_alias_newtype_ref.con error_092_alias_struct_field_ref.con regress_092_type_param_shadows_ref_alias.con error_092_const_ref_array.con error_092_phantom_ref_type_arg.con regress_092_refs_flow_down.con"
  # Bug 108: `bool as <int>` was `trunc i1`; `<int> as bool` is refused (E0553).
  [108]="regress_108_bool_cast_widen.con error_108_int_as_bool.con"
  # Bug 109: float constants printed with six decimals (f32 invalid IR, f64 wrong value).
  [109]="regress_109_float_literals.con"
  # Bug 110: an array SSA value (a pointer) stored as if it were the aggregate.
  [110]="regress_110_array_value_store.con"
  # Bug 116: vec_push on a first field through &mut called the undefined @vec_push.
  [116]="regress_116_vec_push_first_field.con"
  # Bug 133: &mut (*p).f borrowed a copy of *p (a write through it was lost).
  [133]="regress_133_deref_field_borrow.con"
  # Bug 184: --interp read all of stdin before main ran (waited for an open pipe to close);
  # both fixtures run under run_tests.sh's r184_leg with stdin held open.
  [184]="regress_184_interp_stdin_unread.con regress_184_interp_stdin_read.con"
  # Bug 191: test programs and std tests wrote fixed /tmp paths (rc 139 for any other user on a
  # shared host); both programs build their path from CONCRETE_TEST_TMPDIR and check fopen
  # (run_tests.sh regress_191 legs), and check_gate_hygiene.sh refuses /tmp literals in .con sources.
  [191]="file_write_read.con file_read_basic.con regress_191_test_scratch.con"
  # Bug 179: concurrent compiles of one source shared the intermediate <input>.ll / <main>.ll;
  # both fixtures are compiled concurrently by run_tests.sh (file mode and project mode).
  [179]="regress_179_concurrent_compile.con regress_179_concurrent_build/src/main.con"
  # Bug 186: trait impl selection was a search (duplicate, overlapping, specialized, orphan impls;
  # a method name from two impls; bound calls by order). E0590 - E0595, plus the two-order positive control.
  [186]="error_186_duplicate_impl.con error_186_duplicate_generic_impl.con error_186_overlap_generic_impl.con error_186_overlap_blanket_impl.con error_186_orphan_impl.con error_186_specialized_impl.con error_186_method_clash.con error_186_inherent_clash.con error_186_ambiguous_bound_method.con error_186_duplicate_primitive_spelling.con error_186_orphan_sibling.con error_186_imported_alias_target.con error_186_builtin_arity.con regress_186_coherent_impls.con regress_186_primitive_impl_spellings.con regress_186_coherent_impls_reversed.con regress_186_visible_targets.con"
  # Bug 175: a match whose arms all diverge left its merge block open (E0708 in a loop).
  # Also four rows of check_divergence_detection.sh.
  [175]="regress_175_match_arms_all_diverge.con"
  # Bug 208: a fn-typed local spelled like an imported function was renamed to it when used as a
  # value (run_tests.sh also compares --interp with the binary).
  [208]="regress_208_fn_local_named_like_import.con"
  # Bug 212: an uppercase constant before an if/while/match head's block is read as a struct
  # literal; the E0001 names that reading (also the while and match forms, and the parenthesized
  # heads in regress_212_parenthesized_const_head.con).
  [212]="error_212_const_in_if_head.con"
  # Bug 214: two leaking returns gave two identical E0208 refusals; each now names its return.
  [214]="error_214_two_returns_leak.con"
  # Bug 228: interpreter loops linear in their iteration count (run_tests.sh: --interp within 20 s).
  [228]="regress_228_interp_loop_env.con"
  # Bug 225: a refused bound names a builtin type and the call's position (run_tests.sh checks lines 12 and 14).
  [225]="error_225_bound_names_builtin_type.con"
  # Bug 224: a failed `let mut` keeps its mutability in recovery (run_tests.sh: E0217 at line 14 only).
  [224]="error_224_failed_let_keeps_mutability.con"
  # Bug 222: an imported enum brings its impls (run_tests.sh also compares --interp with the binary).
  [222]="regress_222_imported_enum_impls.con"
  # Bug 221: a call's explicit type-argument list of the wrong length (E0322).
  [221]="error_221_extra_type_argument.con error_221_type_argument_to_plain_fn.con error_221_method_extra_type_argument.con error_221_short_type_argument_list.con error_221_fn_pointer_type_argument.con error_221_bound_method_type_argument.con regress_221_exact_type_arguments.con"
  # Bug 220: a radix prefix with no digits (E0001).
  [220]="error_220_empty_hex_literal.con error_220_empty_binary_literal.con error_220_empty_octal_literal.con regress_220_radix_literals.con"
  # Bug 218: two declarations of one name in a module (E0299, MOD-3).
  [218]="error_218_duplicate_function.con error_218_duplicate_struct.con error_218_struct_and_enum.con error_218_duplicate_constant.con error_218_duplicate_alias.con regress_218_type_and_value_namespaces.con"
  # Bug 250: native in-loop writes into large aggregates compile in linear time (suite: within 20 s).
  [250]="regress_250_native_array_writes.con"
  # Bug 249: an explicit array literal parses in linear time (suite: a generated literal within 20 s).
  [249]="regress_249_explicit_array_literal.con"
  # Bug 248: interpreter element writes in place (suite: --interp within 20 s).
  [248]="regress_248_interp_array_write.con"
  # Bug 027: large array literals compile natively in linear time (suite: within 20 s).
  [027]="regress_027_large_array_literals.con"
  # Bug 236: an array literal elaborates in linear time (run_tests.sh: --emit-core and --interp within 20 s).
  [236]="regress_236_array_repeat_literal.con"
  # Bug 235: interpreter array reads O(1) (run_tests.sh: --interp within 20 s).
  [235]="regress_235_interp_array_read.con"
  # Bug 234: interpreter calls linear in the call depth (run_tests.sh: --interp within 20 s).
  [234]="regress_234_interp_deep_recursion.con"
  # Bug 231: a bound's trait names resolve (E0125); module traits, Copy and Destroy still admitted.
  [231]="error_231_unknown_bound_trait.con error_231_enclosing_trait_bound.con error_231_trait_param_bound.con error_231_union_param_bound.con error_231_bodiless_decl_bound.con regress_231_bounds_name_module_traits.con"
  # Bug 227: a user enum Result in one module captured the builtin in another (Lower's and Resolve's
  # spelled-name lookups); run_tests.sh compares --interp with the binary.
  [227]="regress_227_nested_builtin_result.con regress_227_toplevel_builtin_result.con regress_227_user_result_methods_beside_builtin.con regress_227_child_result_beside_builtin.con regress_227_imported_field_builtin_result.con regress_227_value_import_named_result.con"
  # Bug 230: a top-level module's known types were program-wide (E0108 now); the positive control
  # keeps imported aliases, the import closure, own types and children's newtypes.
  [230]="error_230_toplevel_foreign_alias_cast.con error_230_parent_names_child_alias.con regress_230_toplevel_type_environment.con"
  # Bug 246: an imported alias's target is expanded through its exporter's aliases.
  [246]="regress_246_imported_alias_chain.con regress_246_imported_alias_chain_across_imports.con regress_246_imported_alias_chain_long.con"
  # Bug 223: an import alias of a type names the declaration (methods, trait impls, values of the
  # defining module); run_tests.sh compares --interp with the binary.
  [223]="regress_223_aliased_struct_methods.con regress_223_aliased_enum.con regress_223_aliased_generic_and_bound.con regress_223_aliased_destroy_impl.con regress_223_aliased_self_result.con regress_223_aliased_cap_params.con error_223_method_core_name_taken.con"
  # Bug 243: an aliased newtype import is the newtype (methods, values, generics, nested trees).
  [243]="regress_243_aliased_newtype_import.con regress_243_aliased_newtype_with_original.con regress_243_aliased_generic_newtype.con regress_243_nested_aliased_newtype.con"
  # Bug 245: two top-level trees' private constants of one spelling, native and --interp.
  [245]="regress_245_toplevel_private_constants.con regress_245_nested_private_constants.con"
  # Bug 233: a public constant is imported by its identity (a trait import is refused with its
  # reason); run_tests.sh compares --interp with the binary and builds the project fixture.
  [233]="regress_233_import_constant.con regress_233_nested_import_constant.con regress_233_import_constant_scope.con regress_233_nested_alias_constant.con regress_233_constant_type_alias.con regress_233_constant_type_imported_alias.con error_233_imported_initializer_calls_function.con error_233_imported_initializer_extern.con error_233_imported_initializer_via_constant.con error_233_import_constant_collides_local.con error_233_import_trait.con regress_233_shared_spelling_import.con"
  # Bug 229: Resolve's module scope refused names Check admits (type/value namespaces, builtin
  # Option, child newtypes).
  [229]="regress_229_value_does_not_hide_type.con regress_229_user_option_does_not_hide_builtin.con regress_229_child_newtype_known.con regress_229_option_named_alias_or_struct.con regress_229_alias_and_enum_literal_precedence.con"
  # Bug 217: Resolve walks every nested submodule (E0108 / E0100 at resolve; the project leg in
  # run_tests.sh builds tests/regressions/bug217_resolve_skips_nested_submodules/project).
  [217]="error_217_nested_cast_unknown_type.con error_217_nested_undeclared_name.con error_217_nested_annotation_unknown_type.con error_217_module_after_main_cast.con error_217_nested_enclosing_alias_cast.con regress_217_nested_submodule_scope.con regress_217_sibling_type_precedence.con regress_217_enclosing_type_not_in_scope.con"
  # Bug 215: an unknown type in a cast or a type-argument list (E0108).
  [215]="error_215_cast_unknown_type.con error_215_turbofish_unknown_type.con error_215_cast_field_spelling.con error_215_const_cast_unknown_type.con"
  # Bug 213: an arm after an unguarded catch-all arm (SSA verifier natively).
  [213]="regress_213_arm_after_binding_arm.con"
  # Bug 211: generic arithmetic instantiated at a non-numeric type (E0501, post-mono).
  [211]="error_211_generic_add_struct.con error_211_generic_mul_enum.con error_211_generic_rhs_struct.con error_211_generic_mixed_width.con regress_211_generic_add_numeric.con"
  # Bug 210: arithmetic on a struct, enum or newtype operand (E0501).
  [210]="error_210_struct_arith.con error_210_enum_arith.con error_210_newtype_arith.con error_210_struct_minus_literal.con error_210_child_module_arith.con error_210_child_module_newtype_arith.con"
  # Bug 202: a project's `let … else` reached Check undesugared (the project loop builds and runs it).
  # Also check_local_identity.sh leg R.
  [202]="regress_202_project_let_else/src/main.con"
  # Bug 185: a raw-pointer comparison through a safe generic escaped E0229 (instances lost `trusted`).
  [185]="error_185_generic_rawptr_eq_safe.con regress_185_generic_rawptr_eq_authority.con"
  # Bug 188: a capability variable entailed every requirement (Unsafe included). Also
  # check_capability_judgment.sh and three mutation families.
  # Bug 189: `trusted impl` conferred trust on every method of the block. Also
  # check_trusted_method.sh's bug 189 section.
  [189]="error_189_trusted_impl_refused.con"
  # Bug 203: one diagnostic code for two unrelated errors (E0420 split; E0421 for append()).
  [203]="error_203_append_unsupported_argument.con error_203_ghost_read_at_runtime.con error_203_print_unsupported_argument.con"
  [188]="error_188_capvar_console.con error_188_capvar_process.con error_188_capvar_unsafe_call.con error_188_capvar_unsafe_op.con error_188_capvar_unsafe_callback.con error_188_capparam_named_unsafe.con error_188_capvar_method_unsafe_callback.con error_188_capvar_impl_receiver.con error_188_bodyless_capparam.con regress_188_capvar_polymorphism.con"
  # Bugs 134 and 140: the comparison rule (E0229): aggregates, linear operands, references,
  # unbounded and aggregate-instantiated generics, raw pointers in safe code; positive control.
  [134]="error_134_enum_eq.con error_134_struct_eq.con"
  [140]="error_140_heap_eq.con error_140_linear_eq.con error_140_ref_eq.con error_140_string_eq.con error_140_generic_unbounded_eq.con error_140_generic_copy_at_struct.con error_140_rawptr_eq_safe.con error_140_generic_self_compare.con regress_140_scalar_comparisons.con"
  # Bug 142: rule (d), no exit inside a call argument, literal element or operand (E0278).
  [142]="error_142_struct_field_return.con error_142_call_arg_return.con error_142_array_elem_return.con error_142_enum_field_return.con error_142_operand_return.con error_142_method_arg_return.con error_142_break_in_arg.con error_142_deref_operand_return.con error_142_for_cond_break.con error_142_assign_index_return.con regress_142_exits_allowed.con"
  # Bug 143: defer rule (a), reserve by place (E0279 / E0206).
  [143]="error_143_defer_borrow_then_free.con error_143_defer_borrow_then_consume.con error_143_defer_observer_before_consumer.con error_143_defer_field_arg.con error_143_defer_call_arg.con error_143_defer_then_use_after_block.con error_143_defer_borrow_only.con error_143_defer_last_in_match_arm.con regress_143_defer_reserve_by_place.con error_143_defer_shadow_refused.con regress_143_defer_sibling_scopes.con"
  # Bug 144: heap interiors linearity-tracked (E0290 / E0287).
  [144]="error_144_heap_field_read_twice.con error_144_free_drops_linear.con error_144_defer_free_drops_linear.con error_144_heap_newtype_unwrap.con regress_144_heap_node_destructure.con"
  # Bug 145: vec_set / vec_free on non-Copy elements (E0291 / E0287).
  [145]="error_145_vec_free_linear.con regress_145_vec_free_copy_param.con regress_145_vec_free_nested_copy_param.con"
  # Bug 146: *h frees its cell (positive control; run_tests.sh also checks the emitted free).
  [146]="regress_146_heap_deref_frees.con regress_146_heap_deref_cast.con error_146_deref_heap_place_write.con error_146_deref_heap_field_read.con"
  # Bug 147: erased proof statements may not move runtime values (E0243).
  [147]="error_147_assert_consumes_heap.con error_147_ghost_let_consumes_heap.con error_147_assert_revives_binding.con regress_147_assert_reads.con regress_147_ghost_consumes_ghost.con"
  # Bug 148: a move in the right operand of && / || (E0244).
  [148]="error_148_and_operand_consumes.con error_148_or_operand_consumes.con regress_148_short_circuit_reads.con"
  # Bug 149: exit paths account for owned values (E0207/E0208/E0210/E0211).
  [149]="error_149_loop_cond_moves_outer.con error_149_return_after_unreached_defer.con error_149_return_in_borrow_block.con error_149_labelled_break_skips.con error_149_labelled_continue_skips.con regress_149_exits_accounted.con regress_149_labelled_exit_runs_defers.con regress_149_return_in_borrow_with_defer.con regress_149_break_in_borrow_with_defer.con error_149_diverging_branches_in_loop.con error_149_loop_cond_in_exiting_branch.con error_149_match_diverge_in_loop.con regress_149_both_branches_return.con regress_149_cond_return_in_borrow.con error_149_continue_in_exit_branch.con regress_149_nested_loop_in_exit_branch.con regress_149_break_value_statement_loop.con error_149_break_value_non_copy.con error_149_break_value_moves_outer.con error_149_loop_revives_consumed.con error_149_rebinding_nested_loop.con regress_149_inner_label_in_exit_branch.con"
  # Bug 194: a reachable end of a function with a result type (E0307).
  [194]="error_194_missing_return_plain.con error_194_missing_return_if.con error_194_missing_return_method.con regress_194_unreachable_end_forms.con"
  # Bug 195: a let ... else whose else block falls through (E0308).
  [195]="error_195_let_else_falls_through_unit.con error_195_let_else_falls_through.con"
  # Bug 196: a match arm whose pattern does not fit the scrutinee (E0309).
  [196]="error_196_other_enum_variant.con error_196_range_over_enum.con error_196_variant_over_int.con error_196_literal_type.con error_196_literal_over_type_param.con error_196_same_name_other_enum.con"
  # Bug 197: a variant pattern without a field list dropped a non-Copy payload (E0288).
  [197]="error_197_variant_pattern_drops_payload.con regress_197_variant_pattern_ignores_copy.con"
  # Bug 165: println/append desugared into spelled helper calls that bound to a same-named user fn.
  [165]="regress_165_generated_helper_call.con"
  # Bug 199: a cast to a type alias panicked the backend.
  [199]="regress_199_cast_to_alias.con"
  # Bug 198 (T-RAWDUP): *p by value of a non-Copy pointee through a raw pointer (E0311).
  [198]="error_rawdup_trusted_deref.con regress_198_read_owned.con"
  [182]="error_d2_borrow_literal.con error_d2_borrow_field_of_temporary.con error_d2_temporary_receiver.con error_d2_mut_borrow_string_literal.con error_d2_call_disjoint_fields.con error_d2_call_field_read.con error_d2_receiver_projection_read.con error_d2_branch_receiver.con error_d2_call_field_read_in_branch.con error_d2_explicit_mut_receiver_read.con regress_d2_roots_and_projections.con"
  [181]="regress_181_borrowed_binders.con error_181_root_reassigned_match.con error_181_root_reassigned_while_let.con error_181_root_reassigned_guard.con error_181_root_reassigned_nested.con error_181_root_reassigned_whole_binder.con error_181_root_reassigned_defer.con"
  # Bug 190: an array bound or passed by value aliased its source in the binary.
  [190]="regress_190_array_value_copy.con"
  [137]="regress_136_137_literal_eval_order.con"
  [136]="regress_136_137_literal_eval_order.con"
  [183]="regress_183_deref_assign_rhs_first.con"
  # Bug 166: an extern fn taken as a value is refused (E0588, D4).
  [166]="error_166_foreign_fn_value_repr_c.con error_166_foreign_fn_value_repr_c_let.con error_166_foreign_fn_value_scalar.con error_166_imported_extern_value.con error_166_extern_value_in_const.con error_166_extern_value_after_scope.con regress_166_wrapper_value.con"
  # Bug 180: project reports ran Mono on the invoked file's modules under a replaced certificate.
  # Also check_pipeline_artifacts.sh legs B, U and D.
  [180]="regress_180_report_scope/src/main.con"
  # Bug 200: a generic type named without its argument list (E0113, TYPE-15).
  [200]="error_200_bare_generic_type.con error_200_bare_generic_signature.con regress_200_generic_type_with_arguments.con regress_200_type_param_shadows_generic.con regress_200_self_receiver_of_generic_impl.con"
  # Bug 193: a struct or enum literal giving a field twice (E0306, TYPE-7).
  [193]="error_193_duplicate_struct_field.con error_193_duplicate_variant_field.con"
  # Bug 192: an integer condition of an if / while / for statement (E0223, TYPE-5).
  [192]="error_192_int_if_condition.con error_192_int_while_condition.con error_192_int_for_condition.con error_192_int_match_guard.con"
  # Bug 130: `x as bool` with an Int x (the bug 108 refusal, E0553).
  [130]="error_130_int_as_bool_int.con"
  [090]="error_090_guard_moves_linear.con error_090_guard_moves_binder.con error_090_guard_defer_moves.con error_090_guard_move_then_reassign.con regress_090_guard_reads.con"
  # Bug 121: enum LLVM types disagreed with tySize/tyAlign, so an enum field moved the
  # fields after it on aggregate copies (conlog SIGSEGV). Class leg: check_enum_union_layout.sh leg 5.
  [121]="regress_bug121_enum_field_layout.con"
  # Bug 113: R-0484 R3 skipped nested submodules (E0116 and E0117 now hold there too).
  [113]="error_113_submodule_extern_undeclared.con error_113_submodule_extern_effectful_pure.con"
  # Bug 103: an extern ending in _destroy was replaced by an empty synthesized body
  # (run_tests.sh codegen section links it with regress_103_extern_destroy.c, rc 42).
  [103]="regress_103_extern_destroy.con"
  # Bug 129: a safe fn malloc became the program's allocator; every Concrete-defined symbol is
  # now mangled apart from C (run_tests.sh also pins the IR: _C.main.malloc, declare @malloc).
  [129]="regress_129_user_fn_named_malloc.con regress_129_extern_beside_same_named_fn.con regress_129_imported_extern_beside_fn.con regress_129_fn_value_extern.con regress_129_runtime_helper_beside_fn.con regress_129_generic_extern.con"
  # Bug 112: an #[intrinsic] declaration was refused as an undeclared foreign binding (E0116).
  [112]="regress_112_user_intrinsic.con"
  # Bug 150: SSA cleanup folded a float self-comparison (x != x) as an integer one.
  [150]="regress_150_float_self_compare.con"
  # Bug 152: string_trim("") returned one byte past the empty source.
  [152]="regress_152_string_trim_empty.con"
  # Bug 151 (contained by E0299): a.b_f and a_b.f flattened to one Core name.
  [151]="error_151_nested_name_flattening.con"
  # Bug 176: a loop body's / for header's shadowing let outlived the loop in Check (run_tests.sh
  # also requires --interp == binary == 42 for the valid fixture).
  [176]="regress_176_loop_shadow_restores_outer.con error_176_while_shadow_type.con error_176_while_shadow_ssa.con error_176_while_shadow_mut.con error_176_for_header_type.con error_176_loop_body_linear_leak.con error_176_loop_body_leak_while_let.con error_176_loop_body_leak_labelled_break.con error_176_loop_body_leak_labelled_break_mid.con"
  # Bug 177: CoreCheck never compared a binder's type with its value's. The defence-in-depth
  # check itself is covered by pipeline-test corecheck/binder-agreement (eight cases planted in
  # Core, all accepted by the previous compiler) and check_corecheck_boundary.sh class 7 (bug
  # 176's program never compiles); this fixture covers the Elab alias-hint sites the check found.
  [177]="regress_177_alias_value_hints.con"
)

# Bugs that don't need a .con regression test (with reason).
declare -A SKIP_BUGS=(
  [219]="FIXED 2026-10-23 -- mutation 31 was anchored on the proof-only cExprToPExpr wrapper and survived. Regression vehicle is the mutation harness (family 31 KILLED), not a tests/programs fixture"
  [252]="FIXED 2026-10-25 -- the Markdown-link check dropped anchors, resolved changelog fragments against their own directory, and was run only by the Makefile. Documentation tooling, no compiler program to reproduce it: the regression vehicle is the link gate's throwaway-tree controls and the selection replay controls over the selector entry point select_gates.sh"
  [237]="FIXED 2026-10-25 -- the impact gate selector counted a script name in a keyed array value (a corpus note in this file) as a call, so a docs-only diff selected the corpus runners. Regression vehicle is the text-is-not-a-call controls over the selector entry point select_gates.sh (selection replay gate)"
  [232]="FIXED 2026-10-24 -- the docs-drift gate pinned the discovery signal's streak as a literal (1 after a quiet batch, 0 after a token batch), so recording a quiet batch failed the gate. Regression vehicle is the docs-drift gate itself (its controls now read the record's streak and expect +1 / +0)"
  [242]="OPEN -- a type in an imported signature is read in the importer's environment by its definition spelling, so the import closure's copy of it hides the importer's own type of that spelling (E0252 on a valid literal); owner R-0492/R-0514 signature types by identity; reproducer tests/regressions/bug242_imported_signature_type_captured/repro.con, found by the probes of bug 223's fix 2026-10-25; pinned as an open-bug reproducer (compile-fails)"
  [226]="OPEN -- a submodule may import a private function or extern of another module (FileSummary.resolveImports, used by Check and Elab for submodules, admits every function), while a top-level module importing it is E0111; fixing it needs a visibility for std's imports of std.libc's non-pub bindings; reproducer tests/regressions/bug226_submodule_imports_private_function/repro.con, found by bug 217's fix 2026-10-24; reproduction pinned by check_example_outputs.sh (open-bug reproducers leg, compiles)"
  [216]="OPEN -- a write into a field, nested element or heap cell of a binding not declared mut is accepted while &mut of the same place is refused (needs the OWN-10 scope decision); reproducer tests/regressions/bug216_field_write_non_mut_binding/repro.con, found by the post-R-0492 differential hunt 2026-10-23; reproduction pinned by check_example_outputs.sh (open-bug reproducers leg, compiles)"
  [244]="FIXED 2026-10-25 -- a killed gate run left its children working the tree and the repository lock held until removed by hand. Regression vehicle is the repository-lock gate's bug-244 legs (a runner sandbox killed by SIGTERM, SIGINT and SIGKILL; the lock library fresh.sh reclaims only a provably empty session group), not a tests/programs fixture"
  [209]="FIXED 2026-10-23 -- a read-only Codex review left a stale index.lock in the caller's worktree. Regression vehicle is check_review_tool.sh (every reviewer call runs with GIT_OPTIONAL_LOCKS=0), not a tests/programs fixture"
  [207]="FIXED 2026-10-23 -- check_binder_refs.sh aborted before its verdict on a failed assertion. Regression vehicle is the mutation harness (families 55-57 KILLED), not a tests/programs fixture"
  [106]="FIXED 2026-10-20 -- model-diff report hang + swallowed gate failures. Regression vehicle is check_model_diff.sh (watchdog/exit legs and the thesis_demo exhausted-row leg), not a tests/programs fixture"
  [006]="string literal collision -- fixed in EmitSSA, covered by cross-module tests"
  [009]="const decls not lowered -- fixed in Lower.lean, no standalone repro needed"
  [010]="missing string_substr -- stdlib addition"
  [015]="O0 default perf -- compiler flag default change, not a .con test"
  [017]="socket constants linux-only -- platform-specific stdlib"
  [024]="recursive-struct infinite size -- covered by check_error_leaks.sh gate corpus"
  [025]="no-main linker leak -- covered by check_error_leaks.sh (no_main, empty_file cases)"
  [026]="huge array-repeat count hang -- covered by check_error_leaks.sh (huge_array case)"
  [178]="FIXED 2026-10-22 -- superlinear match lowering (compile time, not output). Regression vehicle is check_ssa_lowering_scaling.sh (lower-stage heartbeat and instructions:u slope <= 1.3 over 250-2000 arms), not a tests/programs fixture"
  [028]="reserved-name collision -- covered by check_error_leaks.sh (clash_*, extern_argc cases)"
  [030]="FIXED general (not trusted-only) -- error_030_nonmut_array_write.con (run_err E0217); Check arrayIndexAssign mut rule"
  [029]="FIXED both sites -- regress_029_if_merge_array_addr.con + regress_029_loop_exit_array_addr.con in tests/programs (run_ok 42/7)"
  [033]="FIXED -- regress_033_discard_live_string.con (run_ok live-across); Lower ifExpr merge aggregate alloca path"
  [032]="FIXED -- regress_032_multibyte_str_literal.con (run_ok 42); EmitSSA byte-based literal sizes+escapes"
  [031]="FIXED all 3 sites -- regress_031_if_branch_borrow.con + regress_031_ifexpr_branch_borrow.con + regress_031_match_arm_borrow.con (run_ok 101/107/8); Lower prePromoteAddrTaken"
  [034]="FIXED -- regress_034_shortcircuit_borrow_promotion.con (run_ok 133) guards the shape; LOAD-BEARING regression = check_cli_helpers.sh two-positionals leg (pre-fix compiler aborts 134, verified); Lower &&/|| prePromoteAddrTaken"
  [035]="FIXED -- enum_generic_payload_layout/src/main.con (project test, exit 0); lowerModule now receives program-wide struct/enum/newtype defs (own-module priority, additive fill)"
  [036]="FIXED -- import_closure_metadata/src/main.con (project test, exit 0); resolveImports closes over public types reachable through imported signatures"
  [037]="FIXED -- error_repr_align_exceeds_natural.con (run_err E0585); repr_align.con moved to the legal no-op case (run_ok 8); CoreCheck reprAlignExceedsNatural"
  [038]="FIXED -- regress_038_if_merge_promoted_aggregate.con (run_ok qm); Lower merge loops skip ANY promoted var (aggregate included); extended fuzzer is the class gate"
  [049]="OPEN -- reduce --predicate crash is vacuous (parse-only); reduces any program to ~empty, fix pending"
  [052]="FIXED 2026-10-18 -- Vec<[T;N]>.drop() skipped element destruction because tyName is empty for arrays and Mono answered the unresolved T_destroy with an empty no-op. Mono now decides the destructor from the argument type (per-element glue for arrays, nested and generic elements included; no-op only for Copy; E0814 for unresolved or destructor-less types) and Check's destroyable lost its accept-anything catch-all (E0241 for [non-Destroy; N], Heap, HeapArray, &mut). Regression vehicle is a multi-file fixture plus gate legs, not a tests/programs .con: tests/regressions/bug052_vec_array_destroy, asserted by check_collections_copy_only.sh (6 legs: compiled counts 4 8 14 17 21, glue IR calls @Tracked_destroy, compiled and --interp agree on 2 8 9, two refusal controls); all six fail on the pre-fix compiler; see docs/bugs/052"
  [053]="FIXED (R-0005) -- the unary trap inventory moved into IntArith (evalIntUnaryOp / unaryOpCanTrap) so DCE stops deciding locally whether -x can trap; gated by check_trap_inventory.sh (12 checks, both paths at i8/i16/i32/Int) with mutations #25-#27"
  [054]="HALF CLOSED -- the collision now fails closed with E0809 (a specialization whose mangled name is declared, or two instantiations mangling to one name); gated in check_mono_name_collision.sh. Still OPEN: the mangling is forgeable, so a legitimate program spelling Box_Int is refused rather than compiled, and fn symbols are uncovered (R-0007)"
  [067]="FIXED 2026-08-05 — a capped obligation escaped forbid-assume when Rocq/Isabelle are present: the cap is display-only, so an obligation resting on an UNSOUND HYPOTHESIS passes the release gate. Asserted by check_known_wrong_corpus.sh, which fails in CI (and only there — no external provers locally). Authority leak. FIX: the capped list keys on hypDebt (kernel-independent) instead of status, so promotion to proved_by_multi_kernel can no longer erase the cap. Gated by check_known_wrong_corpus.sh, which only fires in CI where Rocq/Isabelle exist; see docs/bugs/067"
  [068]="FIXED 2026-08-06 — a ghost let and a runtime let produced IDENTICAL body evidence, so erasure was invisible to the subject digest and a proof over one stayed valid-looking after a change to the other. Found while scoping proof-context elaboration, not by a gate. FIX: EvidenceStmtV2.letBind carries isGhost, on the same principle as exprStmt's isValue. Gated by check_shadow_body_v2.sh; see docs/bugs/068"
  [066]="FIXED 2026-08-04 -- the interpreter discarded a match guard's environment, so a guard's output and mutations vanished while its VALUE still selected the arm; return values agreed, so only an OUTPUT differential caught it. Gated by check_match_guard_effects.sh (12 checks: succeeding guard, failing guard's effects surviving the fall-through, two failing guards in order, a guard whose pattern never matches, outer-mutation persistence, arm-local mutation NOT leaking, binder-frame restoration with a rebinding control). Mutation-verified on the EFFECTS axis (threading the env only on success fails 3 legs). The binding-LEAK axis is prevented upstream, so its mutation legitimately survives rather than indicating missing coverage: code owner Elab.bindArmVar alpha-renames every match payload binder to a fresh Core name (x -> x.b0, verified with --emit-core), gated by regress_045_match_binder_shadow.con (run_ok 42) in run_tests.sh. If that owner or gate changes, the leak mutation becomes killable and check_match_guard_effects.sh's restoration leg starts doing real work -- a 045/066 coupling, also recorded in the bug doc"
  [070]="FIXED 2026-08-02 (renumbered from 065) -- builtinEnumList built twice AND hasUserResult computed from different inputs (Elab consults imports, Check does not), so the passes hold different beliefs about a user Result; code divergence measured, no wrong-behaviour witness yet; gated by check_builtin_enum_owner.sh (four Result arrival paths: absent, local, imported-unaliased, imported-aliased)"
  [069]="FIXED 2026-08-02 (renumbered from 064) -- aliased imported structs/enums are registered under the importer's local spelling; positive alias fixture compiles and the unaliased E0298 privacy control proves resolution was preserved; gated by check_type_identity.sh via tests/programs/bug_064_aliased_imported_type.con"
  [055]="FIXED 2026-10-18 with bug 082 -- a project sibling-submodule alias is spelled by its definition (a_pick) in Core; regress_082_project_alias_binding/src/main.con (project test, exit 0; previous compiler: undefined @pick at llvm-as)"
  [056]="FIXED (R-0436) -- a function reference became SVal.fnRef and a call target became SCallee.direct/.indirect, retiring both string encodings; gated by check_fnptr_values.sh (17 checks: rebinding across if/loop, phi mixing loaded register with known global, devirtualization preserved, verifier still refusing undefined phi operands AND undefined indirect targets) with mutations #28-#30"
  [058]="OPEN -- #[proof_by] with no #[proof_fingerprint] compares the current fingerprint with ITSELF, so an edited body still reports proved (R-0004 defect 1; reproducer in the bug doc, control case with a fingerprint stales correctly)"
  [059]="FIXED (R-0004 V2 activation 2026-08-17) -- the authoritative subject is proofSubjectDigestV2, which binds CheckedDeclFacts (full typed signature), so i32 -> u32 now stales the claim; LOAD-BEARING regression = check_proof_freshness.sh '059 CLOSED' leg (whole-signature change must report stale)"
  [060]="FIXED (R-0004 V2 activation 2026-08-17) -- contracts are inside the v2 subject, so attaching or changing an #[ensures] moves the digest and the claim no longer reports proved; LOAD-BEARING regression = check_proof_freshness.sh '060 CLOSED' leg (a false postcondition must not report proved)"
  [061]="FIXED (R-0442) -- PExpr gained .applyVar and FnTable gained a callables namespace, so a parameter application and a definition call are different nodes resolved in different namespaces; gated by check_proofcore_callable_identity.sh (29 checks) with mutations #31-#33"
  [071]="FIXED 2026-09-21 (sinks+Alloc) / 2026-09-25 (Unsafe) / 2026-09-25 (prelude receivers) -- a capability declared in a DEPENDENCY did not bind on its caller: a function declaring nothing called std.env.get and the program printed \$HOME. ALL FOUR CALL FORMS now bind, and they are enumerated because a headline that did not name them is exactly how the last one stayed open: imported free function, explicitly imported receiver method (RawCursor::read_u8), associated call (TcpStream::connect), and PRELUDE receiver method (String::drop). The prelude case survived two rounds because importedFnCaps was built by walking m.imports and String needs no import; check_cap_sibling_module.sh asserted the acceptance as a KNOWN HOLE (a passing check) while check_cross_package_caps.sh passed claiming methods bind -- two green gates, incompatible headlines. Public std Unsafe fell 154->21 via per-method trusted. Gated by check_cross_package_caps.sh (18/0) and check_cap_sibling_module.sh (17/0, now a rejection test with a positive control); see docs/bugs/071"
  [063]="FIXED both sites 2026-09-19/2026-09-20 (R-0470) -- cap-variable inference recorded \"unknown argument type\" as \"no capabilities\", so a fn pointer read from a struct field, call result or array element could not reach a cap C parameter; misreported as E0220 against a with() the program never wrote. The authority check already PASSED on the fabricated empty capset and only expectTy stopped it, so relaxing fn-type comparison to subsetting would have turned a rejected-valid-program into an authority hole. FIX: both inference sites yield Option CapSet and contribute nothing when the type is unknown, letting resolveCaps name the variable. TWO SITES -- Check.lean (function calls) and CheckHelpers.lean (methods); the first fix took only one, and std's cap-polymorphic combinators (Set::fold, for_each, with_value) are METHODS, so real callers reach the one that was missed. Gated by check_cap_inference.sh (10/0), which asserts E0242 and the ABSENCE of E0220 on each path -- a gate that only checked \"does it fail\" would pass either way; see docs/bugs/063"
  [064]="OPEN -- a bare semantic --query KIND is rejected as unknown while the message lists that same kind as known (arity is wrong, not spelling). CLI/report diagnostic, not a .con defect: the regression vehicle is the query leg of a gate script (every knownQueryKinds/knownFactKinds member in both bare and argument form, asserting the message never lists the rejected token), not a program fixture. No gate yet"
  [065]="FIXED 2026-10-18 -- the --report stack-depth summary maximised over the BOUNDED subset, so a program with recursion printed a finite Max stack bound (20 bytes (leaf) while main recursed; 0 bytes ((none)) when nothing was bounded), and check_policy.sh (max_stack_bytes), check_assumptions.sh (stack_max_bytes) and capture_release_bundle.sh (evidence.max_stack_bytes) each grepped that number, so a budget PASSED unbounded stack. The summary now reads unbounded when any function is (Report.lean stackSummaryLine) and all three consumers read it through scripts/tests/lib/stack_bound.sh, which refuses it. Regression vehicle is a fixture pair, not a tests/programs .con: tests/regressions/bug065_unbounded_stack/{unbounded,bounded}, run by check_stack_budget.sh (summary unbounded; both budgets refuse naming unboundedness; bundle marks it; bounded control keeps 40 bytes, passes 40, fails 39); reverting the summary fails 6 legs; see docs/bugs/065"
  [072]="FIXED 2026-10-01 -- std.process.spawn's child called exit(127) when exec failed, flushing stdio buffers it had copied from the parent, so pending buffered output was written twice (measured: XX for one byte). Now _exit. Regression vehicle is a std-using package, not a tests/programs .con: tests/regressions/spawn_exit/exec_failure_no_double_flush, run by check_spawn_exit.sh, which asserts the child exited 127 and the file holds exactly X; shown to fail on XX with the fix reverted; see docs/bugs/072"
  [074]="FIXED 2026-10-04 -- two packages each defining top-level module 'util' passed every frontend check, failed only in LLVM validation (invalid redefinition of function), and report modes silently merged the two packages' declarations. loadProject now refuses the collision before resolution, naming both packages. Regression vehicle is a multi-package fixture, not a tests/programs .con: tests/regressions/bug074_duplicate_module_packages, asserted by check_assumption_summary.sh (build and report mode both refused; disabling the refusal restores the LLVM error and fails the gate); see docs/bugs/074"
  [075]="FIXED 2026-10-06 -- check_policy.sh and check_assumptions.sh scraped only parenthesised --report caps tokens while capability headlines print bare, so their authority checks never saw a capability; Net (not a capability) sat in ten policy files. Regression vehicle is the gates themselves: fail/pass controls in both (forbidden Console fails, allowed passes, Std expands, unknown name fails, unavailable facts fail); see docs/bugs/075"
  [076]="FIXED 2026-10-06 -- proof admission used effectOpaqueSet over the program-only call graph, so indirect calls and foreign bindings inside dependencies, and type-parameter dispatch, were admitted as effect-free. Regression is a two-package fixture (tests/regressions/proof_admission) run by check_proof_admission.sh, plus #guard controls in ProofCore; see docs/bugs/076"
  [077]="OPEN -- relative call into a three-level nested submodule lowers to an undefined symbol (@util_poke); reproducer tests/regressions/bug077_nested_submodule_call/repro.con, fix pending; containment (build refuses it, reports keep reach, no R-0484 case depends on it) gated by check_bug077_contained.sh"
  [078]="FIXED 2026-10-10 -- an if/else (statement or expression) whose arms both diverge left its merge block open, so an enclosing branch used the unreachable block as a phi source and SSA verification rejected a valid program (E0703). Regression vehicle is a gate, not a tests/programs .con: check_divergence_detection.sh compiles, runs and compares with --interp four bug 078 shapes plus the nested-divergence row that found it; those five rows fail with E0703 on the previous compiler; see docs/bugs/078"
  [079]="FIXED 2026-10-17 -- a body was compared with a specification only when its source name keyed Concrete.Proof.specs, so a claim borrowing any theorem by naming that theorem's spec was proved and minted a receipt (always_seven + check_nonce_correct on b87152ba). Regression vehicle is a gate: check_subject_binding.sh mints nothing for always_seven, rejects the receipt b87152ba issued for it (scripts/tests/fixtures/bug079/always_seven.b87152ba.store), and refuses the changed-together, attested-wrong-model and permuted-parameter shapes; see docs/bugs/079"
  [080]="FIXED 2026-10-18 -- a narrowing cast was the identity in the proof model (x as u8 = 300 in the model, 44 in the binary); value-changing casts extract as wrapCast. Regression vehicle is a gate: check_model_diff.sh compares the model with the interpreter on boundary inputs across the corpus and pins tests/regressions/bug080_narrow_cast (300 -> 44 on both sides); see docs/bugs/080"
  [084]="FIXED 2026-10-18 -- the proof model bound match-arm binders by name (0 when no field had the binder's name) while Elab/interp/binary bind by position (Box::Full { v } => v: model 0, runtime x; Mk { b, a } => b: model 100). Regression vehicle is a gate: check_model_diff.sh leg E pins tests/regressions/bug084_enum_arm_binding/repro.con (positional values on both sides, binary exits 22); fails 7/10 on the previous compiler; see docs/bugs/084"
  [085]="FIXED 2026-10-18 -- a while_step loop body's model read every variable from the pre-iteration env (x = x + 1; x = x + 1 stepped by 1) and dropped the rest of the body after a fall-through if; assignments now bind sequentially. Regression vehicle is a gate: check_model_diff.sh leg S pins tests/regressions/bug085_loop_step/repro.con (ten loop shapes, each pinned to its runtime value); see docs/bugs/085"
  [086]="FIXED 2026-10-18 -- a discarded expression statement was modeled as its block's value (if x > 0 { id(x); } return 7; modeled as x) and a return nested in a non-tail expression as a value; only isValue trailing expressions are values and non-tail sites must not escape. Regression vehicle is a gate: check_model_diff.sh leg T pins tests/regressions/bug086_discarded_tail/repro.con; see docs/bugs/086"
  [101]="FIXED 2026-10-19 -- a branch- or body-local let was nested over the code after its scope in the proof model (scoped(1): model 10, runtime 1); branches composing with a continuation refuse when their locals reach it. Regression vehicle: tests/regressions/bug085_loop_step/repro.con scoped/reloop + check_model_diff.sh leg S"
  [114]="OPEN -- an intrinsic declared in a nested submodule and called by path lowers to a C call; reproducer tests/regressions/bug114_submodule_intrinsic_links_c_symbol/repro.con, found by the examples-health pass 2026-10-20; reproduction pinned by check_example_outputs.sh (open-bug reproducers leg)"
  [117]="FIXED 2026-10-21 (interpreter-coverage pass) -- the interpreter evaluates a module constant's initializer at the use, in its own frame, and refuses a name two modules define (bug 132). Regression vehicle: run_tests.sh interp-vs-native leg on tests/regressions/bug117_interp_module_const/repro.con; examples/snippets/constants.con promoted to interp=yes"
  [118]="FIXED 2026-10-21 (interpreter-coverage pass) -- the interpreter resolves a call spelling per calling module as the backend does (definitions, the module's import aliases, sibling aliases), refusing every ambiguity. Regression vehicle: run_tests.sh interp-vs-native leg on tests/regressions/bug118_interp_nested_module_import/repro.con; examples/snippets/import.con and examples/project promoted to interp=yes"
  [122]="OPEN -- the builtin Option/Result are emitted as one canonical LLVM union sized for the program's largest instantiation while the compiler lays out each instantiation (and a user enum named Option/Result) by its own size, so aggregate copies move or overrun fields (wrong code; a stack overflow in tests/programs/match_expr_linear.con); reproducer tests/regressions/bug122_builtin_enum_canonical_layout/repro.con, found by bug 121's layout probe 2026-10-20; reproduction pinned by check_example_outputs.sh (open-bug reproducers leg, runs-exit) and check_enum_union_layout.sh leg 5 (KNOWN_122)"
  [135]="FIXED 2026-10-21 -- concrete fmt printed float literals with toString (six decimals), so 0.0000000001 became 0.000000 and the formatted program differed; one exact canonical (shortest, then nearest) round-trip printer (Lexer.floatLiteralString) now serves fmt, --emit-core and --emit-ssa. Regression vehicle is a gate, not a tests/programs fixture: check_concrete_fmt.sh section 8 compiles every corpus float lexeme plus 311 sampled doubles before and after fmt and requires the LLVM f64 bits to equal Python's reading, requires each sampled double's canonical spelling to survive fmt, and requires tests/regressions/fmt_roundtrip/floats.con to keep its lexemes; both legs shown to fail with toString restored; see docs/bugs/135"
  [128]="FIXED 2026-10-21 (interpreter-coverage pass) -- the interpreter has IEEE float values; int -> float rounds to nearest-even exactly, float -> int is the checked conversion. Regression vehicle: run_tests.sh interp-vs-native leg on tests/regressions/bug128_interp_int_to_float_identity/repro.con and tests/codegen/float_semantics.con in check_codegen_differential.sh"
  [119]="FIXED 2026-10-20 (batch 2) -- interp dispatch of a trait method on a type parameter through the trait-impl table. Regression vehicle: run_tests.sh interp-vs-native leg on tests/regressions/bug119_interp_generic_trait_method/repro.con (a regression dir, not a tests/programs fixture)"
  [120]="FIXED 2026-10-20 (batch 2) -- interp projection base of reference type forwards its path. Regression vehicle: run_tests.sh interp-vs-native leg on tests/regressions/bug120_interp_field_borrow_through_ref/repro.con (a regression dir, not a tests/programs fixture)"
  [088]="FIXED 2026-10-18 -- an unhinted integer literal defaulted to Int with NO range obligation (let x = 18446744073709551615: interp 2^64-1, binary -1), a literal at a type variable was never range-checked after Mono (let y: T = 300 at u8: interp 300, binary 44), and the SSA folder compared unnormalized constants (0xFFFFFFFFFFFFFFFF as i64 < 0: interp true, binary false). Regression vehicles: regress_088_const_fold_casts.con (run_ok 1048575; old compiler 1048574) + error_088_unhinted_literal_range.con (run_err) + check_cast_matrix.sh constant-fold and both-paths-reject sections; see docs/bugs/088"
  [081]="FIXED 2026-10-18 -- the proof model calls by table KEY but edges were justified by table containment, so an import alias (check_version as check_class) borrowed validate_header_correct and minted. Regression vehicle is a gate: check_subject_binding.sh legs X refuse and do not mint tests/programs/bug081_alias_key (and its colliding-definition variant) while the unaliased elf_header claims stay proved and minted; see docs/bugs/081"
  [083]="FIXED 2026-10-18 -- resolveSpec fell back to Proof.provedFunctions, a table keyed by qualified NAME with v1 body-string freshness, so a program with no proof attributes defining main.compute_tag (u8, aborting at runtime) was reported proved (origin hardcoded). The table is deleted; every program it served links in source with a v2 pin. Regression fixture tests/programs/regress_083_no_claim_by_name.con (run_tests.sh: 0 proved, 3 unproved, no hardcoded origin); see docs/bugs/083"
  [089]="FIXED 2026-10-18 -- an #[ensures] read proved_by_lean on contracts/vcs/obligation-ledger/audit whenever a link named a theorem (even No.Such.Theorem, or only #[proof_coverage(iff)]), check-proofs printed a check mark for any theorem that type-checked, and a rejected ensures theorem exited 1 under 0 failed while its receipt minted. One verdict (ensuresVerdictOf) decides every surface; nothing discharges a postcondition until Exact property acceptance. Regression vehicle is a gate: check_contract_negatives.sh section bug 089 runs tests/regressions/bug089_ensures_proof (four fixtures x every surface, check-proofs summaries/exit, receipts); see docs/bugs/089"
  [082]="FIXED 2026-10-18 -- an import alias reached Core as the alias spelling and every backend resolved it program-wide, so import a.{check_version as check_class} ran another module's check_class (compiled AND interp); also an unaliased import of a name two top-level modules define, and a project submodule's std import hijacked by a sibling submodule's same-named fn. Elab now spells such calls by their definition (importCallSpellingsFor), and one name has one binding per module (E0299). Regression: tests/codegen/import_alias_binding.con (interp==compiled differential + pinned 77324 in check_codegen_execution.sh; previous compiler 12814) and tests/programs/error_082_import_alias_shadows_local.con / error_082_import_duplicate_binding.con / error_082_import_alias_shadows_method.con / error_082_import_spelling_collision.con / error_082_import_suffix_collision.con (run_err E0299), plus the project test regress_082_project_alias_binding/src/main.con (sibling alias = bug 055, and std.math.abs not hijacked by a sibling submodule's abs); see docs/bugs/082"
  [062]="FIXED (R-0004 slice 3) -- dependency containment: a reachable dependency that is not current downgrades its dependent to deps_not_current, at one hop and transitively; gated by check_proof_freshness.sh with mutations #34-#36"
  [039]="FIXED -- regress_039_import_alias_collision/src/main.con (project test, exit 0); emitSModule puts the module's own bare->qualified import aliases ahead of the program-wide pool"
  [040]="FIXED -- regress_040_match_binder_types.con (run_ok 42); CoreCheck addVar shadows (prepend) + match-arm binders arm-scoped (save/restore)"
  [041]="FIXED -- regress_041_match_binder_states.con (run_ok 42) + error_041_match_leak_still_caught.con (run_err E0208); Check post-match merges rebuild from envBefore (arm binders arm-scoped)"
  [042]="FIXED -- regress_042_import_newtype/src/main.con (project test, exit 0) + std-compiled-coverage numeric leg; Resolve import classifier now registers newtypes + public type aliases"
  [043]="FIXED -- check_envcfg.sh env-override leg + std-compiled-coverage env/fs/io/net legs (Linux CI is the discriminating platform); String::to_cstr + all C-string FFI sites converted"
  [044]="FIXED -- regress_044_renamed_generic_import/src/main.con (project test, exit 0); Mono lookupFn tries all alias orientations + specializes under the resolved def's canonical name"
  [045]="FIXED -- regress_045_match_binder_shadow.con (run_ok 42); Elab alpha-renames match binders (value -> value.bN), retiring the name-scoping class below Elab"
  [046]="FIXED -- error_046_map_values_linear/src/main.con (negative project fixture, must FAIL) + std-compiled-coverage bug-046 legs (E0241 + Copy positive); keys/values/elements Copy-bounded"
)

# The records' coverage lines (see the header). A record-borne entry and a legacy inline entry never
# coexist: the migration moves every inline entry into its record in one step.
LEGACY_ENTRIES=$(( ${#BUG_TEST_MAP[@]} + ${#SKIP_BUGS[@]} ))
if ! LEDGER_ROWS="$(python3 -I "$ROOT_DIR/scripts/gen/bug_ledger.py" corpus --root "$ROOT_DIR")"; then
  echo "FATAL: scripts/gen/bug_ledger.py corpus failed; the bug records' ledger blocks cannot be read" >&2
  exit 2
fi
LEDGER_FAIL=0
if [ -n "$LEDGER_ROWS" ]; then
  if [ "$LEGACY_ENTRIES" -gt 0 ]; then
    echo "  FAIL the bug records carry ledger blocks but this script still holds $LEGACY_ENTRIES inline entries:" \
      "$(printf '%s\n' "${!BUG_TEST_MAP[@]}" "${!SKIP_BUGS[@]}" | sort | tr '\n' ' ')— move each into its record's" \
      "ledger block (fixtures: or coverage:) and delete it here"
    LEDGER_FAIL=1
  fi
  while IFS=$'\t' read -r lnum lkind _lstatus lval; do
    [ -n "$lnum" ] || continue
    case "$lkind" in
      fixtures) BUG_TEST_MAP[$lnum]="$lval" ;;
      coverage) SKIP_BUGS[$lnum]="$lval" ;;
      *) echo "FATAL: bug_ledger.py corpus printed an unknown kind '$lkind' for $lnum" >&2; exit 2 ;;
    esac
  done <<< "$LEDGER_ROWS"
fi

echo "=== Bug-to-Regression Corpus Audit ==="
echo

for bugfile in "$BUGS_DIR"/[0-9][0-9][0-9]_*.md; do
  [[ -f "$bugfile" ]] || continue
  base=$(basename "$bugfile" .md)
  num=${base%%_*}

  # Skip exempted bugs
  if [[ -n "${SKIP_BUGS[$num]:-}" ]]; then
    echo "  skip  $base (${SKIP_BUGS[$num]})"
    SKIP=$((SKIP + 1))
    continue
  fi

  # Check explicit mapping
  if [[ -z "${BUG_TEST_MAP[$num]:-}" ]]; then
    echo "  FAIL  $base -- no entry in BUG_TEST_MAP"
    FAIL=$((FAIL + 1))
    continue
  fi

  issues=""
  for testfile in ${BUG_TEST_MAP[$num]}; do
    # Check test file exists
    if [[ ! -f "$TESTS_DIR/$testfile" ]]; then
      issues="${issues}missing $testfile; "
      continue
    fi
    # Check test is registered in run_tests.sh
    testbase="${testfile%.con}"
    if ! grep -q "$testbase" "$RUN_TESTS" 2>/dev/null; then
      issues="${issues}$testfile not in run_tests.sh; "
    fi
  done

  if [[ -z "$issues" ]]; then
    echo "  ok    $base -> ${BUG_TEST_MAP[$num]}"
    PASS=$((PASS + 1))
  else
    echo "  FAIL  $base -- $issues"
    FAIL=$((FAIL + 1))
  fi
done

echo
echo "=== Reverse check: bug_*.con without a numbered bug ==="

for testfile in "$TESTS_DIR"/bug_*.con; do
  [[ -f "$testfile" ]] || continue
  testbase=$(basename "$testfile")
  # Check if any BUG_TEST_MAP value references this file
  found=false
  for mapped in "${BUG_TEST_MAP[@]}"; do
    for f in $mapped; do
      [[ "$f" == "$testbase" ]] && found=true && break 2
    done
  done
  if ! $found; then
    echo "  warn  $testbase -- not mapped to any numbered bug"
  fi
done

# ============================================================================
# STATUS AGREEMENT: the bug doc and this corpus must not contradict each other.
#
# Bug 063 was recorded three ways at once — the doc said Fixed, this corpus said
# OPEN with "no fixture yet", and the roadmap task still described it as pending
# — and nothing in CI objected. A reader asking "is 063 closed?" got a different
# answer depending on which file they opened, which is the same defect class as
# ABSENCE_IS_NOT_A_FACT.md: a record of a fact that is not tied to the fact.
#
# ONLY THE FIXED/NOT-FIXED BINARY IS ENFORCED, deliberately. The vocabularies do
# not line up beyond that and should not be forced to: bug 058 is CONTAINED in
# its doc and OPEN here, which is an honest pair — contained is not fixed. What
# must never happen is one side reading FIXED while the other does not.
#
# Legacy entries (001-028, 047-057) hold a fixture path rather than a status
# word; they predate the convention and are COUNTED AND REPORTED rather than
# silently skipped, so the coverage of this check stays visible instead of
# looking total.
# ============================================================================
# NO DUPLICATE BUG NUMBERS — in the docs OR in the tables below.
#
# THIS IS THE ONE THAT ACTUALLY BIT. Two unrelated bugs were filed under 064 and
# two more under 065. The tables here are bash ASSOCIATIVE ARRAYS, where a
# repeated key silently OVERWRITES instead of colliding, so one entry of each
# pair simply ceased to exist — and both discarded entries were FIXED bugs, which
# were also missing from the README index. Three records of the same fact and all
# three lost it, with nothing in CI objecting. They are now 069 and 070.
#
# A duplicate is detected by counting SOURCE LINES, not array entries: by the
# time the array exists the evidence is gone, which is exactly why this went
# unnoticed for so long.
echo
echo "=== every bug number is used once ==="
DUP_DOCS="$(ls "$BUGS_DIR"/[0-9][0-9][0-9]_*.md 2>/dev/null | xargs -n1 basename 2>/dev/null | cut -c1-3 | sort | uniq -d || true)"
DUP_KEYS="$(grep -oE '^[[:space:]]*\[[0-9]{3}\]=' "${BASH_SOURCE[0]}" | tr -d ' []=' | sort | uniq -d || true)"
if [ -n "$DUP_DOCS" ]; then
  echo "  FAIL two bug docs share a number: $(echo $DUP_DOCS) — renumber one; the ledger keys by number"
  FAIL=$((FAIL+1))
else
  ok_dup=1
fi
if [ -n "$DUP_KEYS" ]; then
  echo "  FAIL a corpus key is assigned twice: $(echo $DUP_KEYS) — bash keeps the LAST silently, so an entry is being discarded"
  FAIL=$((FAIL+1))
elif [ "${ok_dup:-0}" = "1" ]; then
  echo "  ok   no duplicate numbers in docs or corpus keys"
fi

echo
echo "=== bug doc status agrees with the corpus (FIXED vs not) ==="
if [ -n "$LEDGER_ROWS" ]; then
  # ONE OWNER: since the migration the record's **Status:** line is the only status there is, and
  # bug_ledger.py refuses a coverage line that restates it, so there is no second copy to compare.
  echo "  ok   the record's **Status:** line is the only status (bug_ledger.py refuses a coverage line restating it)"
else
STATUS_CHECKED=0; STATUS_LEGACY=0; STATUS_BAD=0
for doc in "$BUGS_DIR"/[0-9][0-9][0-9]_*.md; do
  [ -e "$doc" ] || continue
  num="$(basename "$doc" | cut -c1-3)"
  # SKIP_BUGS is where the status-bearing rows live (a bug with no fixture of its
  # own, annotated with why); BUG_TEST_MAP holds the legacy fixture paths.
  entry="${SKIP_BUGS[$num]:-${BUG_TEST_MAP[$num]:-}}"
  [ -n "$entry" ] || continue
  # The corpus word, upper-cased. Legacy rows start with a path, not a status.
  cword="$(printf '%s' "$entry" | awk '{print toupper($1)}')"
  case "$cword" in
    FIXED*|OPEN*|HALF*|PARTIAL*|CONTAINED*) : ;;
    *) STATUS_LEGACY=$((STATUS_LEGACY+1)); continue ;;
  esac
  dline="$(grep -m1 '^\*\*Status:\*\*' "$doc" || true)"
  if [ -z "$dline" ]; then
    echo "  FAIL $num: the bug doc has no '**Status:**' line to compare"
    STATUS_BAD=$((STATUS_BAD+1)); continue
  fi
  dword="$(printf '%s' "$dline" | sed 's/^\*\*Status:\*\*[[:space:]]*//' | awk '{print toupper($1)}')"
  STATUS_CHECKED=$((STATUS_CHECKED+1))
  dfixed=no; cfixed=no
  case "$dword" in FIXED*) dfixed=yes ;; esac
  case "$cword" in FIXED*) cfixed=yes ;; esac
  if [ "$dfixed" != "$cfixed" ]; then
    echo "  FAIL $num: doc says '$dword' but the corpus says '$cword' — one of them is wrong"
    STATUS_BAD=$((STATUS_BAD+1))
  fi
done
echo "  checked $STATUS_CHECKED status-bearing entries; $STATUS_LEGACY legacy entries carry a fixture path instead of a status and are not compared"
if [ "$STATUS_CHECKED" -lt 20 ]; then
  echo "  FAIL only $STATUS_CHECKED entries were comparable — the convention is eroding, not holding"
  STATUS_BAD=$((STATUS_BAD+1))
fi
if [ "$STATUS_BAD" -eq 0 ]; then
  echo "  ok   every status-bearing entry agrees with its bug doc"
else
  FAIL=$((FAIL+STATUS_BAD))
fi
fi

FAIL=$((FAIL+LEDGER_FAIL))

echo
echo "=== Results ==="
echo "  pass:    $PASS"
echo "  fail:    $FAIL"
echo "  skip:    $SKIP"

if [[ "$FAIL" -gt 0 ]]; then
  echo
  echo "FAILED: $FAIL bug(s) missing regression coverage."
  echo "Fix: give the bug's record (docs/bugs/NNN_*.md) a ledger block with a fixtures: or coverage: line"
  echo "     (before the ledger migration: an entry in BUG_TEST_MAP or SKIP_BUGS in this script)."
  exit 1
fi

echo
echo "All numbered bugs have regression coverage."
