import ConcreteSpec
import Lean

open Lean Elab Command

namespace SpecLanding

structure Group where
  origin : Name
  declarations : List Name := []
  laws : List Name := []

inductive Scope where
  | original | addition | qualification
  deriving BEq

structure Piece where
  id : String
  scope : String
  groups : List Group
  remaining : List String := []
  scopeKind : Scope := .original
  disposition : Option String := none

private def present (id scope : String) (groups : List Group) : Piece :=
  { id, scope, groups }

private def added (id scope record : String) (groups : List Group) : Piece :=
  { present id s!"{scope} ({record})" groups with scopeKind := .addition }

private def pending (id : String) (records : List String) : Piece :=
  { id, scope := "", groups := [], remaining := records }

private def disposition (id status record : String) : Piece :=
  { id, scope := record, groups := [], disposition := some status }

private def qualifies (id family : String) (groups : List Group := []) : Piece :=
  { id, scope := family, groups, scopeKind := .qualification
    remaining := ["ROADMAP.md R-0700; docs/decisions.md#decided-2026-11-13-proof-labels"] }

private def decls (origin : Name) (names : List Name) : Group :=
  { origin, declarations := names }

private def evidence (origin namespaceName : Name) (names : List Name) : Group :=
  decls origin (names.map (namespaceName ++ ·))

private def laws (origin : Name) (names : List Name) : Group :=
  { origin, laws := names }

-- Name literals quote planned identities without requiring them to resolve here.
private def plan : List Piece := [
  present "F01" "Explicit proof imports and a complete specification root" [
    decls `ConcreteSpec.Proofs.Basics [`ConcreteSpec.Core.AList.lookup_cons]],
  present "F02" "Numeric variables, shared lookup, and separate diagnostics" [
    decls `ConcreteSpec.Spec.Basics [`ConcreteSpec.Var, `ConcreteSpec.Core.AList.lookup],
    decls `ConcreteSpec.Diagnostics.Types [`ConcreteSpec.LocalNames, `ConcreteSpec.Elaborated],
    decls `ConcreteSpec.Proofs.Basics [`ConcreteSpec.Core.AList.lookup_of_mem]],
  present "F03" "Stable identities, target, outcome, and byte coordinates" [
    evidence `ConcreteSpec.Spec.Ident `ConcreteSpec.Core [
      `example_LoadedModules_findCollision_loadedA_loadedB_isNone,
      `example_collision_refuses_entire_load_and_retains, `example_OD44_refuses_one_top_level_module,
      `example_Ordinals_are_per_kind_named_siblings, `example_Entering_named_construct_resets_scope_but,
      `example_Descendant_assignment_retains_pinned_ancestor_and,
      `example_Assigned_tables_attach_every_key_in, `example_Equal_specialization_requests_at_different_sites,
      `example_Known_premise_categories_round_trip_unknown, `example_Two_declarations_named_Pair_in_different,
      `example_same_module_path_in_two_packages, `example_field_belongs_its_owner_Pair_is,
      `example_FieldId_mk_variant_ModuleId_mk, `example_first_and_last_slice,
      `example_tab_advances_one_byte_column_independently, `example_Counting_tab_as_zero_bytes_gives,
      `example_Local_identity_is_numeric_type_from, `example_Places_primitives_and_contract_revisions_have,
      `example_Two_obligations_at_one_site_differ, `example_revised_rule_is_different_rule_identity,
      `example_Before_freeze_version_is_x],
    evidence `ConcreteSpec.Spec.Target `ConcreteSpec.Core [
      `example_existing_profile_is_well_formed, `example_profile_whose_pointers_have_no_room,
      `example_Nor_is_one_whose_pointer_alignment, `example_Nor_one_whose_pointer_size_is,
      `example_Nor_one_whose_objects_are_too, `example_Nor_one_whose_objects_may_be,
      `example_small_target_bit_addresses_and_bit],
    evidence `ConcreteSpec.Spec.Outcome `ConcreteSpec.Core [
      `example_names_are_decided_ones, `example_trap_passes_through_bind_continuation_never,
      `example_Stuckness_propagates_without_being_converted_trap,
      `example_successful_value_continues_normally_while_mapping],
    decls `ConcreteSpec.Spec.Ident [`ConcreteSpec.Core.PlaceId, `ConcreteSpec.Core.PrimId, `ConcreteSpec.Core.ModuleId, `ConcreteSpec.Core.SourceText, `ConcreteSpec.Core.SourceText.lineAt, `ConcreteSpec.Core.SourceText.columnAt],
    decls `ConcreteSpec.Spec.Target [`ConcreteSpec.Core.Target, `ConcreteSpec.Core.Target.WellFormed],
    decls `ConcreteSpec.Spec.Outcome [`ConcreteSpec.Core.Res, `ConcreteSpec.Core.TrapKind]],
  added "F03b" "F03 SourceText follow-on"
    "changelog.d/2026-11-17-spec-f03b-sourcetext.md" [
    evidence `ConcreteSpec.Spec.Ident `ConcreteSpec.Core [
      `example_newline_starts_new_based_byte_column],
    evidence `ConcreteSpec.Proofs.Ident `ConcreteSpec.Core.Vacuity [
      `example_aSource_aTab, `lineAt_zero_example_aSource, `lineAt_zero_planted_aSource_zeroBasedLine,
      `columnAt_zero_example_aSource, `columnAt_zero_planted_aSource_zeroBasedColumn],
    decls `ConcreteSpec.Spec.Ident [`ConcreteSpec.Core.SourceText.ofBytes, `ConcreteSpec.Core.SourceText.toBytes],
    decls `ConcreteSpec.Proofs.Ident [`ConcreteSpec.Core.SourceText.take_toBytes],
    laws `ConcreteSpec.Proofs.Ident [
      `ConcreteSpec.Core.SourceText.columnAt_zero, `ConcreteSpec.Core.SourceText.lineAt_zero]],
  disposition "F03p-design" "folded into F03p"
    "ISSUES.tsv ISS-0068; docs/decisions.md#decided-2026-11-17-proposals-in-lean",
  pending "F03p" ["ISSUES.tsv ISS-0068"],
  present "F04" "Stable-key, namespace, obligation, contract, and byte-coordinate proofs" [
    evidence `ConcreteSpec.Proofs.Ident `ConcreteSpec.Core.Vacuity [
      `lookup_renumber_example_siteTable, `lookup_renumber_planted_siteTable_collapseRenumber,
      `nodup_renumber_example_siteTable, `nodup_renumber_planted_siteTable_collapseRenumber,
      `assignProgramTables_outsideEdit_example_bodiesBefore,
      `assignProgramTables_outsideEdit_planted_bodiesBefore_assignProgramTablesFromSiteOffset,
      `collectKeysIn_sound_example_nestedBody, `collectKeysIn_sound_planted_nestedBody_missingPathAtIn,
      `collectKeysIn_collectionSound_example_nestedBody,
      `collectKeysIn_collectionSound_planted_nestedBody_unprefixedCollect,
      `collectKeysIn_complete_example_nestedBody, `collectKeysIn_complete_planted_nestedBody_shallowCollect,
      `assignFunctionTable_occurrences_example_nestedBody,
      `assignFunctionTable_occurrences_planted_nestedBody_shallowAssign,
      `assignFunctionTable_lookup_example_nestedBody,
      `assignFunctionTable_lookup_planted_nestedBody_shallowAssign,
      `assignFunctionTable_named_scope_example_namedNested,
      `assignFunctionTable_named_scope_planted_namedNested_unresetScopeAssign,
      `assignFunctionTable_rename_named_example_namedNested,
      `assignFunctionTable_rename_named_planted_namedNested_noopRename,
      `assignFunctionTable_rename_named_planted_namedNested_unresetScopeAssign,
      `assignFunctionTable_rename_named_example_namedCall,
      `assignFunctionTable_rename_named_planted_namedCall_noopRename,
      `assignFunctionTable_pin_example_nestedBody, `assignFunctionTable_pin_planted_nestedBody_noopPin,
      `assignFunctionTable_pin_example_unnamedBinding,
      `assignFunctionTable_pin_planted_unnamedBinding_noopPin,
      `key_insert_differentKind_example_unnamedBinding,
      `key_insert_differentKind_planted_unnamedBinding_allKindsOrdinal,
      `key_insert_named_example_unnamedBinding, `key_insert_named_planted_unnamedBinding_countNamedKey,
      `key_rename_other_example_namedBinding, `key_rename_other_planted_namedBinding_demoteRename,
      `key_pin_other_example_unnamedBinding, `key_pin_other_planted_unnamedBinding_labelOnlyOrdinal,
      `example_Helper_computation_not_law_evidence, `example_Helper_computation_not_law_evidence_2,
      `descendant_insert_differentKind_example_nestedBody,
      `descendant_insert_differentKind_planted_nestedBody_allKindsDescendantKey,
      `descendant_insert_named_example_nestedBody,
      `descendant_insert_named_planted_nestedBody_countNamedDescendantKey,
      `pathAt_insert_after_example_nestedBody, `pathAt_insert_after_planted_nestedBody_wholeForestPathAt,
      `pathAt_insert_named_example_nestedBody, `pathAt_insert_named_planted_nestedBody_countNamedPathAt,
      `pathAt_insert_differentKind_example_nestedBody,
      `pathAt_insert_differentKind_planted_nestedBody_allKindsPathAt,
      `example_Regression_not_independent_soundness_evidence,
      `example_Regression_not_independent_soundness_evidence_2, `descendant_rename_other_example_nestedBody,
      `descendant_rename_other_planted_nestedBody_demoteRename, `descendant_pin_other_example_nestedBody,
      `descendant_pin_other_planted_nestedBody_labelOnlyDescendantKey,
      `example_Representation_not_law_evidence, `example_Representation_not_law_evidence_2,
      `canonicalize_complete_example_requestZero, `canonicalize_complete_planted_requestZero_dropFirstEdges,
      `canonicalize_edge_sound_example_edgeZero, `canonicalize_edge_sound_planted_edgeZero_oobEntryAt,
      `canonicalize_share_example_requestZero, `canonicalize_share_planted_requestZero_bySite,
      `canonicalize_distinct_example_requestOtherType, `canonicalize_distinct_example_requestOtherCapability,
      `canonicalize_distinct_planted_requestOtherType_onlyGeneric,
      `canonicalize_distinct_planted_requestOtherCapability_onlyGeneric,
      `canonicalize_nodup_example_requestZero, `canonicalize_nodup_planted_requestZero_bySite,
      `load_refusal_sound_example_collidingTop,
      `load_refusal_sound_planted_collidingTop_fabricatedRefusalLoad,
      `load_refusal_sound_planted_collidingTop_admitAll, `load_refusal_complete_example_collidingOrigin,
      `load_refusal_complete_planted_collidingOrigin_admitAll, `load_admitted_example_firstModule,
      `load_admitted_planted_firstModule_discardLoad, `load_admitted_example_collidingModule,
      `load_admitted_planted_collidingModule_admitAll, `lineAt_append_newline_example_aSource,
      `lineAt_append_newline_planted_aSource_constantLine, `lineAt_append_other_example_aSource,
      `lineAt_append_other_planted_aSource_offsetLine, `columnAt_append_tab_example_aSource,
      `columnAt_append_tab_planted_aSource_tabStopFour, `columnAt_append_newline_example_aTab,
      `columnAt_append_newline_planted_aTab_noResetColumn, `lineAt_append_example_aSource,
      `lineAt_append_planted_aSource_constantLine, `columnAt_append_example_utf8Prefix,
      `columnAt_append_planted_utf8Prefix_scalarColumn, `read_encode_example_stackRecord,
      `read_encode_planted_stackRecord_encodeDroppingKind, `read_unknown_example_unknownRecord,
      `read_unknown_planted_unknownRecord_catchAllRead, `read_iff_example_mismatchedRecord,
      `read_iff_planted_mismatchedRecord_versionIgnoringRead, `read_iff_example_futureVersion,
      `read_iff_planted_futureVersion_versionIgnoringRead,
      `example_Accepted_declaration_and_constant_owners_cover,
      `example_conclusionOnly_conditional_unconditional_conditional_unconditional],
    laws `ConcreteSpec.Proofs.Ident [
      `ConcreteSpec.Core.EncodedPremise.read_encode,
      `ConcreteSpec.Core.EncodedPremise.read_iff,
      `ConcreteSpec.Core.EncodedPremise.read_unknown,
      `ConcreteSpec.Core.KeyConstruct.assignFunctionTable_lookup,
      `ConcreteSpec.Core.KeyConstruct.assignFunctionTable_named_scope,
      `ConcreteSpec.Core.KeyConstruct.assignFunctionTable_occurrences,
      `ConcreteSpec.Core.KeyConstruct.assignFunctionTable_pin,
      `ConcreteSpec.Core.KeyConstruct.assignFunctionTable_rename_named,
      `ConcreteSpec.Core.KeyConstruct.assignProgramTables_outsideEdit,
      `ConcreteSpec.Core.KeyConstruct.collectKeysIn_collectionSound,
      `ConcreteSpec.Core.KeyConstruct.collectKeysIn_complete,
      `ConcreteSpec.Core.KeyConstruct.collectKeysIn_sound,
      `ConcreteSpec.Core.KeyConstruct.descendant_insert_differentKind,
      `ConcreteSpec.Core.KeyConstruct.descendant_insert_named,
      `ConcreteSpec.Core.KeyConstruct.descendant_pin_other,
      `ConcreteSpec.Core.KeyConstruct.descendant_rename_other,
      `ConcreteSpec.Core.KeyConstruct.key_insert_differentKind,
      `ConcreteSpec.Core.KeyConstruct.key_insert_named,
      `ConcreteSpec.Core.KeyConstruct.key_pin_other,
      `ConcreteSpec.Core.KeyConstruct.key_rename_other,
      `ConcreteSpec.Core.KeyConstruct.pathAt_insert_after,
      `ConcreteSpec.Core.KeyConstruct.pathAt_insert_differentKind,
      `ConcreteSpec.Core.KeyConstruct.pathAt_insert_named,
      `ConcreteSpec.Core.LoadedModules.load_admitted,
      `ConcreteSpec.Core.LoadedModules.load_refusal_complete,
      `ConcreteSpec.Core.LoadedModules.load_refusal_sound,
      `ConcreteSpec.Core.SiteKeyTable.lookup_renumber,
      `ConcreteSpec.Core.SiteKeyTable.nodup_renumber,
      `ConcreteSpec.Core.SourceText.columnAt_append,
      `ConcreteSpec.Core.SourceText.columnAt_append_newline,
      `ConcreteSpec.Core.SourceText.columnAt_append_tab,
      `ConcreteSpec.Core.SourceText.lineAt_append,
      `ConcreteSpec.Core.SourceText.lineAt_append_newline,
      `ConcreteSpec.Core.SourceText.lineAt_append_other,
      `ConcreteSpec.Core.SpecializationTable.canonicalize_complete,
      `ConcreteSpec.Core.SpecializationTable.canonicalize_distinct,
      `ConcreteSpec.Core.SpecializationTable.canonicalize_edge_sound,
      `ConcreteSpec.Core.SpecializationTable.canonicalize_nodup,
      `ConcreteSpec.Core.SpecializationTable.canonicalize_share]],
  pending "F04p" ["ISSUES.tsv ISS-0068"],
  added "R" "Rule registry, prerequisite of F06, F13 and F17"
    "changelog.d/2026-11-13-spec-rule-registry.md; ROADMAP.md R-0615" [
    decls `ConcreteSpec.Spec.Rules [`ConcreteSpec.Core.registeredRules,
      `ConcreteSpec.Core.registeredRuleExamples, `ConcreteSpec.Core.RuleAccepted,
      `ConcreteSpec.Core.RuleRefused]],
  present "F05" "Fixed-width arithmetic and exact-term correspondence" [
    evidence `ConcreteSpec.Proofs.Scalar `ConcreteSpec.Core.ScalarEvidence [
      `typed_eval_range_example_literal, `typed_eval_range_planted_literal_wideEval,
      `machine_operands_example_typed_pair, `machine_operands_planted_typed_pair_wideEval,
      `machine_inRange_example_typed_machine, `machine_inRange_planted_typed_machine_wideEval],
    evidence `ConcreteSpec.Proofs.Scalar `ConcreteSpec.Core [
      `checkedNeg_ok_arithmetic_example_signed, `checkedNeg_ok_arithmetic_planted_signed_identityNeg,
      `checkedNeg_error_iff_example_minimum, `checkedNeg_error_iff_planted_minimum_uncheckedNeg],
    evidence `ConcreteSpec.Proofs.Scalar `ConcreteSpec.Core.ScalarLawEvidence [
      `checked_inRange_example_addition, `checked_inRange_planted_addition_carriedChecked,
      `name_injective_example_concrete, `name_injective_planted_concrete_sameName,
      `trapKind_injective_example_concrete, `trapKind_injective_planted_concrete_sameTrap,
      `toBits_lt_example_concrete, `toBits_lt_planted_concrete_unreducedBits,
      `ofBits_inRange_example_concrete, `ofBits_inRange_planted_concrete_offsetOfBits,
      `ofBits_toBits_example_concrete, `ofBits_toBits_planted_concrete_offByOneOfBits,
      `wrap_inRange_example_concrete, `wrap_inRange_planted_concrete_unreducedUnary,
      `wrap_of_inRange_example_concrete, `wrap_of_inRange_planted_concrete_incrementWrap,
      `saturate_inRange_example_concrete, `saturate_inRange_planted_concrete_unreducedUnary,
      `saturate_of_inRange_example_concrete, `saturate_of_inRange_planted_concrete_incrementWrap,
      `bitwise_inRange_example_concrete, `bitwise_inRange_planted_concrete_unreducedBitwise,
      `bitNot_inRange_example_concrete, `bitNot_inRange_planted_concrete_unreducedUnary,
      `wrapping_of_checked_example_concrete, `wrapping_of_checked_planted_concrete_subtractForAll,
      `saturating_of_checked_example_concrete, `saturating_of_checked_planted_concrete_subtractForAll,
      `toBits_ofBits_example_concrete, `toBits_ofBits_planted_concrete_offByOneOfBits,
      `toBits_wrap_example_concrete, `toBits_wrap_planted_concrete_increment,
      `toBits_wrapping_example_concrete, `toBits_wrapping_planted_concrete_subtractForAll,
      `toBits_shl_example_concrete, `toBits_shl_planted_concrete_unboundedBits,
      `toBits_shr_unsigned_example_concrete, `toBits_shr_unsigned_planted_concrete_offsetBits,
      `toBits_bitwise_example_concrete, `toBits_bitwise_planted_concrete_xorForAll,
      `toBits_bitNot_example_concrete, `toBits_bitNot_planted_concrete_incrementWrap,
      `saturate_below_example_concrete, `saturate_below_planted_concrete_incrementWrap,
      `saturate_above_example_concrete, `saturate_above_planted_concrete_incrementWrap,
      `machine_plain_eq_exact_example_concrete, `machine_plain_eq_exact_planted_concrete_corruptExact,
      `eval_toExact_example_concrete, `eval_toExact_planted_concrete_eraseExact,
      `lossless_iff_example_same_width, `lossless_iff_planted_same_width_strictLossless],
    evidence `ConcreteSpec.Proofs.Scalar `ConcreteSpec.Core.ScalarVariableEvidence [
      `example_Reading_numeric_variable_does_not_merge, `example_Assignment_uses_shared_map_other_number,
      `example_Scope_removal_erases_only_chosen_number],
    evidence `ConcreteSpec.Proofs.Scalar `ConcreteSpec.Core.CharEvidence [
      `charFromCodePoint_map_example_surrogate,
      `charFromCodePoint_map_planted_surrogate_replacementConstructor,
      `charFromCodePoint_roundtrip_example_ascii, `charFromCodePoint_roundtrip_planted_ascii_nextConstructor,
      `charFromAscii_map_example_high_byte, `charFromAscii_map_planted_high_byte_latin1Constructor],
    evidence `ConcreteSpec.Spec.Scalar `ConcreteSpec.Core [
      `example_IntTy_all_map_IntTy_bits, `example_IntTy_i8_min_IntTy_i8,
      `example_i16_is_bit_pattern_xfffe_and, `example_Overflow_is_error_value_checked_operation,
      `example_plain_u8_add_trap_overflow, `example_checked_u8_add_ok,
      `example_Division_truncates_toward_zero_remainder_takes, `example_zero_divisor_and_signed_MIN_and,
      `example_checked_i8_div_error_overflow, `example_Shifts_count_must_be_below_width,
      `example_checked_u8_shl_x81_ok, `example_checked_i8_shr_ok_checked,
      `example_Wrapping_and_saturating_forms_never_fail, `example_Bitwise_operations_act_on_bit_patterns,
      `example_lossless_conversion_is_allowed_only_when,
      `example_try_convert_reports_OutOfRange_value_not_trap, `example_In_contract_machine_u8_addition_and,
      `example_machine_term_is_not_defined_on, `example_Negation_overflows_i8_there_is_no,
      `example_largest_scalar_is_accepted, `example_Surrogates_are_refused_independently_upper_bound,
      `example_non_surrogate_above_Unicode_s_range, `example_ASCII_s_upper_endpoint_is_accepted,
      `example_charFromAscii],
    decls `ConcreteSpec.Spec.Scalar [`ConcreteSpec.Core.IntTy, `ConcreteSpec.Core.checked, `ConcreteSpec.Core.CTerm.eval],
    laws `ConcreteSpec.Proofs.Scalar [
      `ConcreteSpec.Core.ArithmeticError.trapKind_injective,
      `ConcreteSpec.Core.TrapKind.name_injective,
      `ConcreteSpec.Core.bitNot_inRange,
      `ConcreteSpec.Core.bitwise_inRange,
      `ConcreteSpec.Core.charFromAscii_map,
      `ConcreteSpec.Core.charFromCodePoint_map,
      `ConcreteSpec.Core.charFromCodePoint_roundtrip,
      `ConcreteSpec.Core.checkedNeg_error_iff,
      `ConcreteSpec.Core.checkedNeg_ok_arithmetic,
      `ConcreteSpec.Core.checked_inRange,
      `ConcreteSpec.Core.eval_toExact,
      `ConcreteSpec.Core.lossless_iff,
      `ConcreteSpec.Core.machine_inRange,
      `ConcreteSpec.Core.machine_operands,
      `ConcreteSpec.Core.machine_plain_eq_exact,
      `ConcreteSpec.Core.ofBits_inRange,
      `ConcreteSpec.Core.ofBits_toBits,
      `ConcreteSpec.Core.saturate_above,
      `ConcreteSpec.Core.saturate_below,
      `ConcreteSpec.Core.saturate_inRange,
      `ConcreteSpec.Core.saturate_of_inRange,
      `ConcreteSpec.Core.saturating_of_checked,
      `ConcreteSpec.Core.toBits_bitNot,
      `ConcreteSpec.Core.toBits_bitwise,
      `ConcreteSpec.Core.toBits_lt,
      `ConcreteSpec.Core.toBits_ofBits,
      `ConcreteSpec.Core.toBits_shl,
      `ConcreteSpec.Core.toBits_shr_unsigned,
      `ConcreteSpec.Core.toBits_wrap,
      `ConcreteSpec.Core.toBits_wrapping,
      `ConcreteSpec.Core.typed_eval_range,
      `ConcreteSpec.Core.wrap_inRange,
      `ConcreteSpec.Core.wrap_of_inRange,
      `ConcreteSpec.Core.wrapping_of_checked]],
  present "F06" "Constant expressions, admission, evaluation, and registry controls" [
    evidence `ConcreteSpec.Spec.Const `ConcreteSpec.Core.ConstDemo [
      `example_accepted_rule_ConcreteSpec_Core_ConstsAdmitted_wrapping,
      `example_accepted_rule_ConcreteSpec_Core_ConstsAdmitted_plain,
      `example_accepted_division_and_unused_constant_have,
      `example_accepted_rule_ConcreteSpec_Core_ConstsAdmitted_fallible,
      `example_refused_rule_ConcreteSpec_Core_ConstsAdmitted_reports,
      `example_division_refusal_names_both_constant_and,
      `example_refused_rule_ConcreteSpec_Core_ConstsAdmitted_signed,
      `example_refused_rule_ConcreteSpec_Core_ConstsAdmitted_shift,
      `example_refused_rule_ConcreteSpec_Core_ConstsAdmitted_UNUSED,
      `example_refused_rule_ConcreteSpec_Core_ConstsAdmitted_finite,
      `example_same_finite_initializer_succeeds_adequate_fuel,
      `example_refused_rule_ConcreteSpec_Core_ConstsAdmitted_non,
      `example_refused_reference_later_in_already_supplied, `example_refused_literal_out_range_and_operands,
      `example_mixed_integer_widths_are_refused, `example_refused_one_name_declared_twice,
      `example_accepted_rule_ConcreteSpec_Core_ConstsAdmitted_source,
      `example_refused_rule_ConcreteSpec_Core_ConstsAdmitted_elaboration,
      `example_refused_rule_ConcreteSpec_Core_ConstsAdmitted_self,
      `example_malformed_diagnostic_claimed_chain_B_is,
      `example_accepted_rule_ConcreteSpec_Core_ConstsAdmitted_sorted,
      `example_refused_rule_ConcreteSpec_Core_ConstsAdmitted_neither,
      `example_refused_rule_ConcreteSpec_Core_ConstsAdmitted_self_2,
      `example_Self_reference_also_fails_dependency_order,
      `example_Fuel_refusal_carries_identity_constant_including,
      `example_unresolved_reference_is_not_dependency_order,
      `example_evaluation_check_is_independent_this_list, `example_closed_admission_list_accepts_integer_and,
      `example_unit_typed_declaration_is_refused_even, `example_char_typed_declaration_is_refused_same,
      `example_analogous_populated_bool_environment_admits_reference,
      `example_whole_unit_declaration_has_well_typed,
      `example_corresponding_char_declaration_also_isolates_ConstantType,
      `example_bool_integer_conversion_is_held_ISS, `example_diagnostic_names_actual_nested_three_edge,
      `example_exhausted_fuel_and_overflow_are_observably,
      `example_accepted_endpoints_adjacent_signed_and_unsigned,
      `example_all_accepted_arithmetic_endpoints_also_pass, `example_signed_minimum_negation_is_well_typed,
      `example_even_smallest_nonzero_unsigned_negation_overflows,
      `example_signed_minimum_divided_minus_one_overflows,
      `example_sorting_preserves_blocked_cyclic_suffix_which,
      `example_exact_output_distinguishes_stable_ready_selection,
      `example_longer_sorted_chain_satisfies_full_dependency],
    decls `ConcreteSpec.Spec.Const [`ConcreteSpec.Core.CExpr, `ConcreteSpec.Core.CExpr.eval, `ConcreteSpec.Core.ConstsPermitted],
    decls `ConcreteSpec.Spec.Rules [`ConcreteSpec.Core.ConstRegistry.accepted, `ConcreteSpec.Core.ConstRegistry.refused]],
  present "F07" "Constant typing, evaluation, dependency order, and cycle laws" [
    evidence `ConcreteSpec.Proofs.Const `ConcreteSpec.Core [
      `eval_wellTyped_progress_example_div_zero, `eval_wellTyped_progress_planted_div_zero_divisionAsFuel,
      `eval_total_example_wrap, `eval_total_planted_wrap_refuseWrappingAdd,
      `evalConsts_sound_example_division, `evalConsts_sound_planted_division_boolConstValues,
      `typed_evalConsts_refusal_example_unused_div_zero,
      `typed_evalConsts_refusal_planted_unused_div_zero_invalidRefusalEvalConsts,
      `admitted_copy_example_division_type, `typeConsts_refuses_nonCopy_example_mixed_declarations,
      `typeConsts_refuses_nonCopy_planted_mixed_declarations_uncheckedTypeConsts,
      `eval_agrees_example_division_failure, `eval_agrees_planted_division_failure_swallowFailure,
      `eval_agrees_example_division_success, `eval_agrees_planted_division_success_fabricateFailure,
      `semantic_failure_refuses_example_unused_after_prefix,
      `semantic_failure_refuses_planted_unused_after_prefix_skipUnusedEvalConsts,
      `cycle_excludes_dependencyOrder_example_two_constants,
      `cycle_excludes_dependencyOrder_planted_two_constants_unsortedPermOrder,
      `DependencySortingComplete_example_forward_ref,
      `DependencySortingComplete_planted_forward_ref_sourceOrderOnly, `constant_types_example_closed_list,
      `constant_types_planted_unit_admitUnit, `typed_constant_closed_example_division,
      `typed_constant_closed_planted_unit_binding_unitTypeConsts, `exhausted_refuses_example_zero_budget,
      `exhausted_refuses_planted_zero_budget_fuelAsInvalid,
      `example_implementation_tie_break_selects_B_before,
      `planted_both_outputs_satisfy_dependency_contract_so, `ConstOrderIndependent_example_forward_and_ties,
      `example_both_directions_admission_are_populated_and,
      `eval_agrees_planted_forward_failure_swallowFailure,
      `eval_agrees_planted_forward_success_fabricateFailure,
      `semantic_failure_refuses_example_unused_overflow,
      `semantic_failure_refuses_planted_unused_overflow_skipUnusedEvalConsts,
      `constant_types_planted_bool_reverse_intOnlyConstantType, `cycle_chain_real_example_three_cycle,
      `cycle_chain_real_planted_empty_forward_acceptEmptyChain,
      `cycle_chain_real_planted_real_reverse_backwardsChain,
      `cycle_chain_real_planted_backwards_forward_backwardsChain,
      `admitted_copy_planted_noncopy_declaration_uncheckedTypeConsts,
      `eval_total_planted_same_type_range_outOfRangeEval,
      `eval_wellTyped_progress_planted_same_type_range_outOfRangeEval,
      `evalConsts_sound_planted_same_type_range_outOfRangeConstValues,
      `ConstOrderIndependent_example_two_edge_chain,
      `ConstOrderIndependent_planted_typing_forward_adjacentTypeConsts,
      `ConstOrderIndependent_planted_typing_reverse_lateBoolTypeConsts,
      `ConstOrderIndependent_planted_evaluation_forward_adjacentEvalConsts,
      `ConstOrderIndependent_planted_evaluation_reverse_lateBoolEvalConsts,
      `admitted_copy_planted_noncopy_value_unitConstValues,
      `ConstOrderIndependent_planted_binding_value_invertFirstBinding],
    laws `ConcreteSpec.Proofs.Const [
      `ConcreteSpec.Core.ConstOrderIndependent,
      `ConcreteSpec.Core.DependencySortingComplete,
      `ConcreteSpec.Core.admitted_copy,
      `ConcreteSpec.Core.constant_types,
      `ConcreteSpec.Core.cycle_chain_real,
      `ConcreteSpec.Core.cycle_excludes_dependencyOrder,
      `ConcreteSpec.Core.evalConsts_sound,
      `ConcreteSpec.Core.eval_agrees,
      `ConcreteSpec.Core.eval_total,
      `ConcreteSpec.Core.eval_wellTyped_progress,
      `ConcreteSpec.Core.exhausted_refuses,
      `ConcreteSpec.Core.semantic_failure_refuses,
      `ConcreteSpec.Core.typeConsts_refuses_nonCopy,
      `ConcreteSpec.Core.typed_constant_closed,
      `ConcreteSpec.Core.typed_evalConsts_refusal]],
  present "F08" "Parameterized executor, lifecycle transitions, residual state, and fuel" [
    evidence `ConcreteSpec.Spec.Exec `ConcreteSpec.Core.Exec.Demo [
      `example_fn_Fn_ops_vs_List, `example_match_run_O, `example_match_run_O_2,
      `example_go_seq_block_seq_dlog, `example_brief_go_block_seq_dlog, `example_brief_go_block_seq_dlog_2,
      `example_brief_go_block_seq_dlog_3, `example_go_seq_loop_skip_lit, `example_go_seq_bind_x_lit,
      `example_brief_go_call_logF_fail, `example_brief_go_seq_bind_x, `example_isStuck_go_seq_block_bind,
      `example_go_seq_block_seq_defer, `example_brief_go_prim_log_arith, `example_go_prim_log_arith_div,
      `example_brief_go_block_seq_defer, `example_brief_run_prog_env_done, `example_match_run_prog_start_int,
      `example_match_run_prog_st_ok, `example_r_Report_ops_r_events, `example_r_Report_ops_r_events_2,
      `example_go_seq_bind_a_lit, `example_brief_go_seq_call_eatF, `example_brief_go_seq_loop_seq,
      `example_isStuck_go_seq_loop_bind, `example_isOutOfFuel_run_prog_env_start, `example_runFn_O,
      `example_runFn_O_2, `example_runFn_O_3, `example_run_O, `example_run_O_2, `example_run_O_3,
      `example_run_O_4, `example_run_O_5, `example_run_O_6, `example_runFrom_prog_env_exec_skip,
      `example_runFrom_prog_env_trapped_abort, `example_runFrom_prog_env_exec_skip_2,
      `example_runFrom_prog_env_exec_prim, `example_runFrom_prog_env_exec_prim_2, `example_runFrom_prog,
      `example_runFrom_prog_st_ok_st, `example_runFrom_prog_env, `example_run_O_7, `example_run_O_8,
      `example_run_O_9, `example_run_O_10, `example_run_O_11,
      `example_accepting_fallthrough_tears_down_last_activation,
      `example_refusing_fallthrough_is_unsafe_stuck_before,
      `example_accepting_explicit_return_preserves_its_value,
      `example_refusing_explicit_return_cannot_publish_returned,
      `example_independent_fallthrough_rule_exactly_one_leave,
      `example_independent_explicit_return_rule_preserves_returned,
      `example_no_relational_label_answer_or_destination, `example_no_relational_label_answer_or_destination_2],
    decls `ConcreteSpec.Spec.Exec [`ConcreteSpec.Core.Exec.Operands, `ConcreteSpec.Core.Exec.Config, `ConcreteSpec.Core.Exec.step, `ConcreteSpec.Core.Exec.runFrom, `ConcreteSpec.Core.Exec.Report]],
  present "F09a" "Elementary execution and structured exits" [
    evidence `ConcreteSpec.Proofs.Exec `ConcreteSpec.Core.Exec.CoreEvidence [
      `lookup_restrict_instance, `lookup_restrict_planted, `restrict_dom_drops_instance,
      `restrict_dom_drops_planted, `steps_request_instance, `steps_request_planted, `step_ret_instance,
      `step_ret_planted, `step_return_caller_instance, `step_return_caller_planted,
      `two_defers_register_instance, `exit_scope_next_instance, `done_scope_next_instance,
      `exit_scope_runs_defers_instance, `done_scope_runs_defers_instance, `unwind_runs_plan_instance,
      `return_reaches_caller_instance, `break_reaches_loop_instance, `continue_reaches_loop_instance,
      `two_defers_register_planted, `exit_scope_next_planted, `done_scope_next_planted,
      `exit_scope_runs_defers_planted, `done_scope_runs_defers_planted, `unwind_runs_plan_planted,
      `return_reaches_caller_planted, `break_reaches_loop_planted, `continue_reaches_loop_planted,
      `exit_scope_runs_defers_reversed, `done_scope_runs_defers_reversed, `unwind_runs_plan_reversed,
      `return_reaches_caller_reversed, `break_reaches_loop_reversed, `continue_reaches_loop_reversed,
      `two_defers_register_fifo, `exit_scope_next_reversed, `done_scope_next_reversed,
      `step_ret_cleanup_first, `step_ret_cleanup_first_value, `return_reaches_caller_lifecycle_instance,
      `return_reaches_caller_bind_before_leave, `reverse_exit_completes, `reverse_done_completes,
      `outer_first_completes, `exit_scope_teardown_instance, `done_scope_teardown_instance,
      `unwind_runs_plan_teardown_instance, `break_reaches_loop_teardown_instance,
      `continue_reaches_loop_teardown_instance, `return_reaches_caller_teardown_instance,
      `exit_scope_runs_defers_skipRestriction, `exit_scope_runs_defers_skipDrop,
      `done_scope_runs_defers_skipRestriction, `done_scope_runs_defers_skipDrop,
      `unwind_runs_plan_skipRestriction, `unwind_runs_plan_skipDrop, `break_reaches_loop_skipRestriction,
      `break_reaches_loop_skipDrop, `continue_reaches_loop_skipRestriction, `continue_reaches_loop_skipDrop,
      `steps_request_lifecycle_instance, `steps_request_store_instance,
      `two_defers_register_preservation_instance, `exit_scope_next_preservation_instance,
      `done_scope_next_preservation_instance, `exit_scope_runs_defers_effects_instance,
      `done_scope_runs_defers_effects_instance, `unwind_runs_plan_populated_instance,
      `break_reaches_loop_populated_instance, `continue_reaches_loop_populated_instance],
    laws `ConcreteSpec.Proofs.Exec [
      `ConcreteSpec.Core.Exec.break_reaches_loop,
      `ConcreteSpec.Core.Exec.continue_reaches_loop,
      `ConcreteSpec.Core.Exec.done_scope_next,
      `ConcreteSpec.Core.Exec.done_scope_runs_defers,
      `ConcreteSpec.Core.Exec.exit_scope_next,
      `ConcreteSpec.Core.Exec.exit_scope_runs_defers,
      `ConcreteSpec.Core.Exec.lookup_restrict,
      `ConcreteSpec.Core.Exec.restrict_dom_drops,
      `ConcreteSpec.Core.Exec.return_reaches_caller,
      `ConcreteSpec.Core.Exec.step_ret,
      `ConcreteSpec.Core.Exec.step_return_caller,
      `ConcreteSpec.Core.Exec.steps_request,
      `ConcreteSpec.Core.Exec.two_defers_register,
      `ConcreteSpec.Core.Exec.unwind_runs_plan]],
  present "F09b" "Cleanup and saved-caller segmentation" [
    evidence `ConcreteSpec.Proofs.Exec `ConcreteSpec.Core.Exec.CleanupEvidence [
      `example_Helper_application_ConcreteSpec_Core_Exec_cleanup_runs_exact_plan,
      `example_Helper_application_ConcreteSpec_Core_Exec_planWith_calls,
      `planted_certificate_ConcreteSpec_Core_Exec_planWith_calls_Reversal,
      `planted_certificate_ConcreteSpec_Core_Exec_planWith_calls_Omitting,
      `planted_certificate_ConcreteSpec_Core_Exec_cleanup_runs_exact_plan_All,
      `example_Helper_application_ConcreteSpec_Core_Exec_cleanup_runs_exact_plan_2,
      `planted_certificate_ConcreteSpec_Core_Exec_cleanup_runs_exact_plan_Missing,
      `planted_certificate_ConcreteSpec_Core_Exec_cleanup_runs_exact_plan_Missing_2,
      `planted_certificate_ConcreteSpec_Core_Exec_cleanup_runs_exact_plan_injected,
      `example_Helper_application_ConcreteSpec_Core_Exec_boundary_of_performed,
      `planted_certificate_ConcreteSpec_Core_Exec_boundary_of_performed_independent,
      `example_Helper_application_ConcreteSpec_Core_Exec_boundary_scope_calls,
      `planted_certificate_ConcreteSpec_Core_Exec_boundary_scope_calls_Dispatch, `split_return_example,
      `call_return_example, `call_return_planted, `split_return_planted, `protected_step_example,
      `protected_step_planted, `call_protects_example, `call_protects_planted, `boundary_complete_example,
      `raw_cleanup_runs_plan_example, `boundary_complete_planted, `raw_cleanup_runs_plan_planted,
      `register_protected_example, `register_protected_planted, `exit_label_example, `exit_label_planted,
      `step_trapped_example, `step_trapped_planted, `step_trapped_planted_2, `steps_from_trapped_example,
      `steps_from_trapped_example_2, `steps_from_trapped_planted, `steps_from_trapped_planted_2,
      `abort_runs_no_cleanup_example, `abort_runs_no_cleanup_planted, `steps_from_trapped_example_3,
      `evalArgs_first_trap_example, `evalArgs_first_trap_planted, `evalArgs_first_trap_planted_2,
      `evalArgs_first_trap_planted_3, `planted_call_then_drop_reconstruction_s_higher,
      `example_Helper_Positive_state_dependent_call_order, `planted_Reverse_just_registered_calls_counter_read],
    laws `ConcreteSpec.Proofs.Exec [
      `ConcreteSpec.Core.Exec.FirstStepsN.call_return,
      `ConcreteSpec.Core.Exec.FirstStepsN.split_return,
      `ConcreteSpec.Core.Exec.abort_runs_no_cleanup,
      `ConcreteSpec.Core.Exec.boundary_complete,
      `ConcreteSpec.Core.Exec.call_protects,
      `ConcreteSpec.Core.Exec.evalArgs_first_trap,
      `ConcreteSpec.Core.Exec.exit_label,
      `ConcreteSpec.Core.Exec.protected_step,
      `ConcreteSpec.Core.Exec.raw_cleanup_runs_plan,
      `ConcreteSpec.Core.Exec.register_protected,
      `ConcreteSpec.Core.Exec.step_trapped,
      `ConcreteSpec.Core.Exec.steps_from_trapped]],
  present "F09c" "Relational determinism and dispatcher agreement" [
    evidence `ConcreteSpec.Proofs.Exec `ConcreteSpec.Core.Exec.DeterminismEvidence [
      `rules_deterministic_instance, `rules_silent_deterministic_instance, `rules_deterministic_planted,
      `rules_silent_deterministic_planted, `rules_sound_instance, `rules_complete_instance,
      `rules_sound_planted, `rules_complete_planted, `primitive_determinism_instance,
      `primitive_agreement_instance, `stuck_agreement_instance, `refusal_agreement_instance,
      `refused_control_instance, `continuation_refusal_instance],
    laws `ConcreteSpec.Proofs.Exec [
      `ConcreteSpec.Core.Exec.rules_complete,
      `ConcreteSpec.Core.Exec.rules_deterministic,
      `ConcreteSpec.Core.Exec.rules_silent_deterministic,
      `ConcreteSpec.Core.Exec.rules_sound]],
  present "F10" "Lifecycle refusals and destruction ordering" [
    evidence `ConcreteSpec.Proofs.Exec `ConcreteSpec.Core.Exec.LifecycleEvidence [
      `bind_instance, `bind_planted, `assign_instance, `assign_planted, `call_instance, `call_planted,
      `scope_instance, `scope_planted, `exitScope_instance, `exitScope_planted, `exitTest_instance,
      `exitTest_planted, `test_instance, `test_planted, `return_instance, `returnExplicit_planted,
      `returnImplicit_planted, `leave_instance, `leaveReturn_planted, `leaveNormal_planted, `enter_instance,
      `enter_planted, `entry_report_instance, `entry_report_planted, `continuation_instance,
      `continuation_planted, `primitive_bind_instance, `primitive_bind_planted, `primitive_step_instance,
      `primitive_step_planted, `primitive_error_instance, `primitive_error_planted, `bind_accepted,
      `bind_deleted, `assign_accepted, `assign_deleted, `call_accepted, `call_deleted, `scope_accepted,
      `scope_deleted, `exitScope_accepted, `exitScope_deleted, `exitTest_accepted, `exitTest_deleted,
      `test_accepted, `test_deleted, `refused_stop_instance, `refused_stop_planted, `false_test_instance,
      `false_test_planted, `return_accepted, `returnExplicit_deleted, `returnImplicit_deleted,
      `leave_accepted, `leaveNormal_deleted, `leaveReturn_deleted, `enter_accepted, `enter_deleted,
      `primitive_step_accepted, `primitive_deleted, `continuation_accepted, `continuation_deleted,
      `primitive_bind_accepted, `primitive_bind_deleted, `return_destinations_accepted, `false_test_accepted,
      `false_test_deleted],
    evidence `ConcreteSpec.Proofs.Exec `ConcreteSpec.Core.Exec.DestructionEvidence [
      `validator_instance, `builder_instance, `enters_instance, `enters_planted, `builder_deleted,
      `sequence_element_instance, `sequence_element_planted, `sequence_storage_instance,
      `sequence_storage_planted, `map_entry_instance, `map_entry_planted, `map_order_planted,
      `map_storage_instance, `map_storage_planted],
    laws `ConcreteSpec.Proofs.Exec [
      `ConcreteSpec.Core.Exec.assign_refused,
      `ConcreteSpec.Core.Exec.bind_refused,
      `ConcreteSpec.Core.Exec.call_refused,
      `ConcreteSpec.Core.Exec.continuation_refused,
      `ConcreteSpec.Core.Exec.destroyCall_enters,
      `ConcreteSpec.Core.Exec.enter_refused,
      `ConcreteSpec.Core.Exec.entry_refusal_report,
      `ConcreteSpec.Core.Exec.exit_drop_refused,
      `ConcreteSpec.Core.Exec.exit_test_drop_refused,
      `ConcreteSpec.Core.Exec.leave_refused,
      `ConcreteSpec.Core.Exec.mapDestroyOrder_entry,
      `ConcreteSpec.Core.Exec.mapDestroyOrder_storage,
      `ConcreteSpec.Core.Exec.prim_bind_refused,
      `ConcreteSpec.Core.Exec.prim_bind_step,
      `ConcreteSpec.Core.Exec.prim_error_bypasses_bind,
      `ConcreteSpec.Core.Exec.refused_stops,
      `ConcreteSpec.Core.Exec.return_refused,
      `ConcreteSpec.Core.Exec.scope_drop_refused,
      `ConcreteSpec.Core.Exec.seqDestroyOrder_elem,
      `ConcreteSpec.Core.Exec.seqDestroyOrder_storage,
      `ConcreteSpec.Core.Exec.test_drop_refused]],
  present "F11" "Byte memory, provenance, and encoding" [
    evidence `ConcreteSpec.Spec.Memory `ConcreteSpec.Core.Mem [
      `example_tinyTarget_WellFormed, `example_Refused_zero_byte_access_still_needs,
      `example_Refused_aligned_representable_address_in_unknown,
      `example_Refused_allocation_in_unknown_space_otherwise, `example_u16_x1234_is_stored_little_endian,
      `example_decodeInt_tgt_i16_encodeInt_tgt, `example_On_big_endian_profile_same_value,
      `example_uninitialized_byte_has_no_integer_value, `example_fresh_block_is_uninitialized_reading_it,
      `example_write_then_read_through_same_pointer, `example_After_release_old_pointer_reaches_nothing,
      `example_Allocation_would_overlap_live_block_fails,
      `example_from_exposed_address_inside_exposed_block_gets],
    evidence `ConcreteSpec.Proofs.Memory `ConcreteSpec.Core.Mem [
      `example_Representation_application_checked_view_covers_negative],
    evidence `ConcreteSpec.Proofs.Memory `ConcreteSpec.Core.Mem.Audit [
      `decodeInt_encodeInt_example_i8, `decodeInt_encodeInt_planted_i8_badDecodeInt,
      `decodeInt_encodeInt_planted_i8_badEncodeInt, `decodeInt_inRange_example_i8,
      `decodeInt_inRange_planted_i8_badDecodeIntRange, `empty_wellFormed_example_default,
      `empty_wellFormed_planted_default_badEmpty, `alloc_fresh_example_first,
      `alloc_fresh_planted_first_badAllocId, `alloc_block_example_first,
      `alloc_block_planted_first_badAllocEmpty, `alloc_failure_example_pow2,
      `alloc_failure_planted_pow2_mutatedAllocPow2, `alloc_failure_example_align,
      `alloc_failure_planted_align_mutatedAllocAlign, `alloc_failure_example_bound,
      `alloc_failure_planted_bound_mutatedAllocBound, `alloc_failure_example_maxObject,
      `alloc_failure_planted_maxObject_mutatedAllocMaxObj, `alloc_failure_example_null,
      `alloc_failure_planted_null_mutatedAllocNull, `alloc_failure_example_overlap,
      `alloc_failure_planted_overlap_mutatedAllocOverlap, `alloc_failure_example_space,
      `alloc_failure_planted_space_mutatedAllocSpace, `access_zero_example_alignedNonNull,
      `access_zero_planted_alignedNonNull_badLoadNone, `access_zero_planted_alignedNonNull_badStoreNone,
      `access_no_provenance_example_nonzero, `access_no_provenance_planted_nonzero_badAccessSome,
      `access_after_free_example_freed, `access_after_free_planted_freed_badAccessSome,
      `example_Representation_check_Canonical_computation,
      `example_Representation_check_Canonical_computation_2, `alloc_ne_existing_example_second,
      `alloc_ne_existing_planted_second_badAllocReuseZero, `alloc_wellFormed_example_first,
      `alloc_wellFormed_planted_first_badAllocId, `fromExposed_contains_example_interior,
      `fromExposed_contains_planted_interior_badFromExposedNone, `fromExposed_onePast_example_boundary,
      `fromExposed_onePast_planted_boundary_badFromExposedNone, `fromExposed_none_example_outside,
      `fromExposed_none_planted_outside_badFromExposedSome, `access_valid_example_nonempty,
      `example_Accepted_rule_C4_Successful_nonempty_access, `access_valid_planted_nonempty_badAccessPair,
      `access_success_example_interior, `access_success_planted_interior_badAccessNone,
      `store_load_example_oneByte, `store_load_planted_oneByte_badLoadNone,
      `store_other_block_example_otherBlock, `store_other_block_planted_otherBlock_badStoreDup,
      `store_same_block_frame_example_afterSlice, `store_same_block_frame_planted_afterSlice_badStoreNone,
      `alloc_initialized_example_first, `alloc_initialized_planted_first_badAllocEmpty,
      `store_wellFormed_example_oneByte, `store_wellFormed_planted_oneByte_badStoreWf,
      `free_wellFormed_example_live, `free_wellFormed_planted_live_badFreeWf, `expose_success_example_live,
      `expose_success_planted_live_badExposeNone, `expose_wellFormed_example_live,
      `expose_wellFormed_planted_live_badExposeWf, `store_load_other_example_disjointProv,
      `store_load_other_planted_disjointProv_badLoadDiff, `expose_load_example_samePtr,
      `expose_load_planted_samePtr_badLoadDiff, `load_prefix_example_twoToOne,
      `load_prefix_planted_twoToOne_badLoadPrefix, `free_alloc_ne_existing_example_reuseAddr,
      `free_alloc_ne_existing_planted_reuseAddr_badAllocReuseZero, `eqAddr_separation_example_provenance,
      `eqAddr_separation_planted_provenance_badEqAddr, `eqAddr_separation_example_address,
      `eqAddr_separation_planted_address_alwaysEqual, `free_retains_example_live,
      `free_retains_planted_live_skipFree, `expose_frame_example_live,
      `expose_frame_planted_live_corruptExpose, `store_geometry_example_live,
      `store_geometry_planted_live_corruptStoreGeometry, `expose_geometry_example_live,
      `expose_geometry_planted_live_corruptExposeGeometry, `fromExposed_sound_example_interior,
      `fromExposed_sound_planted_interior_wrongReconstruction,
      `example_Accepted_rule_C4_Framing_observes_initialized, `eqAddr_separation_example_space,
      `eqAddr_separation_planted_space_ignoreSpace, `example_Released_memory_fails_only_liveness_here,
      `example_pointer_in_second_admitted_space_fails, `example_byte_before_base_fails_only_lower,
      `example_one_past_pointer_is_not_one, `example_Interior_and_in_bounds_but_not,
      `example_adjacent_WellFormed_target, `example_memory_fromExposed_prov, `example_m_Memory],
    decls `ConcreteSpec.Spec.Memory [`ConcreteSpec.Core.Mem.Memory,
      `ConcreteSpec.Core.Mem.Memory.alloc, `ConcreteSpec.Core.Mem.Memory.fromExposed,
      `ConcreteSpec.Core.Mem.decodeInt],
    laws `ConcreteSpec.Proofs.Memory [
      `ConcreteSpec.Core.Mem.access_after_free,
      `ConcreteSpec.Core.Mem.access_no_provenance,
      `ConcreteSpec.Core.Mem.access_success,
      `ConcreteSpec.Core.Mem.access_valid,
      `ConcreteSpec.Core.Mem.access_zero,
      `ConcreteSpec.Core.Mem.alloc_block,
      `ConcreteSpec.Core.Mem.alloc_failure,
      `ConcreteSpec.Core.Mem.alloc_fresh,
      `ConcreteSpec.Core.Mem.alloc_initialized,
      `ConcreteSpec.Core.Mem.alloc_ne_existing,
      `ConcreteSpec.Core.Mem.alloc_wellFormed,
      `ConcreteSpec.Core.Mem.decodeInt_encodeInt,
      `ConcreteSpec.Core.Mem.decodeInt_inRange,
      `ConcreteSpec.Core.Mem.empty_wellFormed,
      `ConcreteSpec.Core.Mem.eqAddr_separation,
      `ConcreteSpec.Core.Mem.expose_frame,
      `ConcreteSpec.Core.Mem.expose_geometry,
      `ConcreteSpec.Core.Mem.expose_load,
      `ConcreteSpec.Core.Mem.expose_success,
      `ConcreteSpec.Core.Mem.expose_wellFormed,
      `ConcreteSpec.Core.Mem.free_alloc_ne_existing,
      `ConcreteSpec.Core.Mem.free_retains,
      `ConcreteSpec.Core.Mem.free_wellFormed,
      `ConcreteSpec.Core.Mem.fromExposed_contains,
      `ConcreteSpec.Core.Mem.fromExposed_none,
      `ConcreteSpec.Core.Mem.fromExposed_onePast,
      `ConcreteSpec.Core.Mem.fromExposed_sound,
      `ConcreteSpec.Core.Mem.load_prefix,
      `ConcreteSpec.Core.Mem.store_geometry,
      `ConcreteSpec.Core.Mem.store_load,
      `ConcreteSpec.Core.Mem.store_load_other,
      `ConcreteSpec.Core.Mem.store_other_block,
      `ConcreteSpec.Core.Mem.store_same_block_frame,
      `ConcreteSpec.Core.Mem.store_wellFormed]],
  present "F12" "Integer hybrid storage and preservation" [
    evidence `ConcreteSpec.Spec.Hybrid `ConcreteSpec.Core.Hybrid [
      `example_encoding_hold_does_not_hold_decided, `example_Missing_layout_authority_refuses_query_rather,
      `example_integer_is_readable_before_taking_its, `example_Boolean_byte_storage_is_outside_this,
      `materialize_read_example, `example_Sharing_scalar_universe_does_not_infer,
      `example_laws_typed_store_frame_rawWrite_read_typed_write_preserves, `materialize_reuse_example],
    evidence `ConcreteSpec.Proofs.Hybrid `ConcreteSpec.Core.Hybrid [
      `encode_refuses_unadmitted_example, `example_public_query_retains_admitted_integer_encoding,
      `encode_refuses_unadmitted_planted_heldBoolean_sentinelEncode],
    evidence `ConcreteSpec.Proofs.Hybrid `ConcreteSpec.Core.Hybrid.Audit [
      `rep_roundtrip_example_u8One, `rep_roundtrip_planted_u8One_badRep,
      `rep_roundtrip_planted_u8One_badEncode, `decode_valid_example_u8One,
      `decode_valid_planted_u8One_badDecodeUnit, `read_structured_example_u8One,
      `read_structured_planted_u8One_badReadNone, `materialize_reuse_example_alreadyMat,
      `materialize_reuse_planted_alreadyMat_badMaterializeReallocate, `write_read_example_u8One,
      `write_read_planted_u8One_badReadStructOnly, `typed_store_frame_example_trailingPad,
      `typed_store_frame_planted_trailingPad_badWriteClobber, `materialize_projection_example_firstMat,
      `materialize_projection_planted_firstMat_badMaterializeEmpty,
      `materialize_idempotent_example_secondCall, `materialize_idempotent_example_firstAllocation,
      `materialize_idempotent_planted_firstAllocation_badMaterializeReallocate,
      `write_wellFormed_example_u8One, `write_wellFormed_planted_u8One_badWriteWf,
      `rawWrite_wellFormed_example_oneByte, `rawWrite_wellFormed_planted_oneByte_badRawWriteWf,
      `materialize_wellFormed_example_firstMat, `materialize_wellFormed_planted_firstMat_badMaterializeWf,
      `expose_wellFormed_example_firstExpose, `expose_wellFormed_planted_firstExpose_badExposeStateWf,
      `materialize_read_example_firstMat, `materialize_read_planted_firstMat_badReadStructOnly,
      `rawWrite_read_example_overwrite, `rawWrite_read_planted_overwrite],
    evidence `ConcreteSpec.Proofs.Hybrid `ConcreteSpec.Core.Hybrid.Audit.Frame [
      `example_laws_write_frame_materialize_frame_expose_frame_observed_place,
      `write_frame_example_otherPlace, `write_frame_planted_otherPlace_badReadDiff,
      `write_frame_disjoint_example_otherPlace, `write_frame_disjoint_planted_otherPlace_badReadDiff,
      `materialize_frame_example_otherPlace, `materialize_frame_planted_otherPlace_badReadEmptyDiff,
      `expose_frame_example_otherPlace, `expose_frame_planted_otherPlace_badReadEmptyDiff],
    decls `ConcreteSpec.Spec.Hybrid [`ConcreteSpec.Core.Hybrid.State, `ConcreteSpec.Core.Hybrid.encode, `ConcreteSpec.Core.Hybrid.decode],
    laws `ConcreteSpec.Proofs.Hybrid [
      `ConcreteSpec.Core.Hybrid.decode_valid,
      `ConcreteSpec.Core.Hybrid.encode_refuses_unadmitted,
      `ConcreteSpec.Core.Hybrid.expose_frame,
      `ConcreteSpec.Core.Hybrid.expose_wellFormed,
      `ConcreteSpec.Core.Hybrid.materialize_frame,
      `ConcreteSpec.Core.Hybrid.materialize_idempotent,
      `ConcreteSpec.Core.Hybrid.materialize_projection,
      `ConcreteSpec.Core.Hybrid.materialize_read,
      `ConcreteSpec.Core.Hybrid.materialize_reuse,
      `ConcreteSpec.Core.Hybrid.materialize_wellFormed,
      `ConcreteSpec.Core.Hybrid.rawWrite_read,
      `ConcreteSpec.Core.Hybrid.rawWrite_wellFormed,
      `ConcreteSpec.Core.Hybrid.read_structured,
      `ConcreteSpec.Core.Hybrid.rep_roundtrip,
      `ConcreteSpec.Core.Hybrid.typed_store_frame,
      `ConcreteSpec.Core.Hybrid.write_frame,
      `ConcreteSpec.Core.Hybrid.write_frame_disjoint,
      `ConcreteSpec.Core.Hybrid.write_read,
      `ConcreteSpec.Core.Hybrid.write_wellFormed]],
  present "F13" "Ownership state, lending, and rule registry" [
    evidence `ConcreteSpec.Spec.Own `ConcreteSpec.Core.Own.Examples [
      `example_Accepted_rule_ConcreteSpec_Core_Own_ownerAccess,
      `example_Refused_rule_ConcreteSpec_Core_Own_ownerAccess,
      `example_Refused_shared_loans_also_prevent_moving,
      `example_Accepted_shared_loans_disjoint_or_overlapping, `example_Refused_disjoint_indices_do_not_bypass,
      `example_Refused_exclusive_loan_blocks_even_owner,
      `example_Accepted_rule_ConcreteSpec_Core_Own_loanAccess,
      `example_Refused_rule_ConcreteSpec_Core_Own_loanAccess,
      `example_Refused_shared_lending_cannot_upgrade_mutable,
      `example_Refused_shared_child_freezes_writes_through,
      `example_Accepted_rule_ConcreteSpec_Core_Own_endLoan,
      `example_Refused_rule_ConcreteSpec_Core_Own_endLoan, `example_Refused_moving_twice_or_reading_moved,
      `example_Refused_leaving_live_linear_owner_would,
      `example_Accepted_downward_lending_preserves_root_leaving,
      `example_Refused_root_loan_cannot_leave_scope, `example_Accepted_identities_are_not_reused_even,
      `example_Refused_logical_token_has_linear_obligation,
      `example_Accepted_explicitly_consuming_logical_unit_obligation,
      `example_Refused_actual_C1_run_gets_stuck,
      `example_unsafe_execution_refutes_unconditional_equality_final,
      `example_erased_execution_does_not_refuse_use, `example_refusalCases_all_e_own,
      `planted_six_separate_omission_mutants_all_accept, `example_Accepted_C1_run_lends_reborrows_ends,
      `example_Refused_existing_owner_loan_or_reborrow, `example_existing_child_is_not_pending_argument,
      `example_Refused_incomplete_locals_table_omits_active, `example_Accepted_real_C1_call_lends_mutable,
      `example_OD23_refuses_second_operand_itself_before, `example_correctly_shaped_actual_call_stops_at,
      `planted_bypassing_second_operand_access_check_defeats,
      `example_direct_owner_lending_cannot_bypass_populated, `example_immediate_direct_owner_lending_is_live,
      `example_deferred_registration_leaves_owner_reads_available,
      `planted_refusing_all_owner_arguments_fails_required,
      `example_assignment_after_consumption_uses_same_numeric,
      `example_numeric_identity_removes_renamed_local_s, `example_moving_twice_refuses_even_no_loan,
      `example_final_function_boundary_refuses_still_live,
      `example_explicit_consumption_permits_same_final_boundary,
      `example_outer_obligation_is_consumed_Refusal_retains,
      `planted_skipped_scope_admission_reaches_function_exit,
      `example_real_scope_exit_instrumentation_refuses_live, `example_ending_just_loan_permits_scope_teardown,
      `example_downward_only_lending_rejects_issued_but],
    decls `ConcreteSpec.Spec.Own [`ConcreteSpec.Core.Own.State, `ConcreteSpec.Core.Own.WellFormed, `ConcreteSpec.Core.Own.OwnerPermitted],
    evidence `ConcreteSpec.Spec.Rules `ConcreteSpec.Core.Own.Registry [
      `ownerAccepted, `ownerRefused, `loanAccepted, `loanRefused,
      `endAccepted, `endRefused, `moveAccepted, `moveRefused,
      `dropAccepted, `dropRefused, `leaveAccepted, `leaveRefused]],
  present "F14" "Ownership preservation, activation and local lifetime proofs" [
    evidence `ConcreteSpec.Proofs.Own `ConcreteSpec.Core.Own.PrimitiveWitnesses [
      `example_Positive_preservation_progress_instances_use_live, `example_ownership_calculation,
      `example_ownership_calculation_2, `example_ownership_calculation_3, `example_ownership_calculation_4,
      `planted_Malformed_move_access_observations_destroy_owned, `planted_ownership_calculation,
      `planted_malformed_borrow_reuses_supply_malformed_reborrow, `planted_ownership_calculation_2,
      `planted_Negated_success_each_operational_permission_violated, `planted_ownership_calculation_3,
      `planted_ownership_calculation_4, `planted_ownership_calculation_5, `planted_ownership_calculation_6,
      `planted_ownership_calculation_7],
    evidence `ConcreteSpec.Proofs.Own `ConcreteSpec.Core.Own.LifecycleWitnesses [
      `example_Each_lifecycle_loan_ending_law_has, `example_ownership_calculation,
      `example_ownership_calculation_2, `example_ownership_calculation_3, `example_ownership_calculation_4,
      `example_ownership_calculation_5, `example_ownership_calculation_6, `example_ownership_calculation_7,
      `planted_Malformed_reinitialization_drop_leave_observations_lose,
      `planted_malformed_bind_reuses_place_identity,
      `planted_entry_adoption_mutation_assigns_unissued_activation,
      `planted_endLoan_endLocals_mutation_removes_parent_before,
      `planted_Progress_counterexamples_live_linear_replacement_child, `planted_ownership_calculation,
      `planted_ownership_calculation_2, `planted_ownership_calculation_3],
    evidence `ConcreteSpec.Proofs.Own `ConcreteSpec.Core.Own.CallWitnesses [
      `example_ownership_calculation, `example_ownership_calculation_2, `example_ownership_calculation_3,
      `example_ownership_calculation_4, `example_ownership_calculation_5,
      `planted_Malformed_bookkeeping_observations_duplicate_parameters_and, `planted_ownership_calculation,
      `planted_leave_mutation_discard_loans_but_retain],
    evidence `ConcreteSpec.Proofs.Own `ConcreteSpec.Core.Own.RoundThreeWitnesses [
      `example_Observation_projected_owner_loan_issues_identity,
      `planted_Rejection_observation_Wrong_projection_falsifies_existential,
      `example_Observation_child_retains_its_parent_s, `planted_Rejection_observation_Removing_child_s_parent,
      `example_Observation_Ending_child_preserves_nonzero_supply,
      `planted_Rejection_observation_mutated_end_decrements_supply,
      `example_Observation_departing_local_had_live_loan,
      `planted_Rejection_observation_Exit_retaining_local_s,
      `planted_Rejection_observation_Exit_retaining_departing_slot,
      `example_Observation_child_marked_during_argument_evaluation,
      `planted_Rejection_observation_Adopting_normal_caller_loan,
      `planted_Rejection_observation_untyped_parameter_refuses_entry,
      `example_Observation_typed_fresh_parameter_enters_over,
      `example_Observation_Lending_downward_after_entry_targets,
      `planted_Refused_activation_cannot_lend_upward_into,
      `planted_Rejection_observation_former_arbitrary_destination_rule,
      `planted_Rejection_observation_rule_issues_wrong_birth,
      `planted_Rejection_observation_Failing_append_requested_suffix,
      `example_Observation_Reborrowing_retains_original_parent_record,
      `planted_Rejection_observation_Removing_parent_now_falsifies],
    evidence `ConcreteSpec.Proofs.Own `ConcreteSpec.Core.Own.DirectArgumentWitnesses [
      `example_Observation_direct_mutable_argument_coexists_unrelated,
      `planted_Rejection_observation_mutated_direct_loan_reuses,
      `example_Observation_Direct_lending_has_no_caller,
      `planted_Rejection_observation_child_caller_loan_cannot,
      `example_Observation_Argument_evaluation_freezes_both_reads,
      `planted_Rejection_observation_Removing_loan_prematurely_fails,
      `example_Observation_Entry_adopts_direct_root_and, `planted_Rejection_observation_return_never_ends_its,
      `example_Observation_original_caller_loan_is_unchanged,
      `planted_Rejection_observation_Ending_every_loan_would,
      `example_Observation_Registering_deferred_call_keeps_live,
      `planted_Rejection_observation_Ending_call_loan_at, `example_Accepted_real_C1_direct_owner_call,
      `example_Accepted_deferred_direct_lending_uses_late,
      `example_Observation_call_restores_two_caller_owners,
      `planted_Rejection_observation_Keeping_call_loan_forever],
    evidence `ConcreteSpec.Proofs.Own `ConcreteSpec.Core.Own.LiveDestinationWitnesses [
      `example_Observation_mutable_loan_and_child_target,
      `planted_Rejection_observation_Targeting_inactive_but_previously,
      `example_Observation_pending_direct_root_becomes_loan,
      `planted_Rejection_observation_Adoption_into_inactive_destination,
      `example_Observation_Return_retains_caller_loan_and,
      `planted_Rejection_observation_Popping_callee_without_ending],
    evidence `ConcreteSpec.Proofs.Own `ConcreteSpec.Core.Own.PreservationEvidence [
      `move_preserves_example_initial, `move_preserves_planted_initial_moveDuplicateSlot,
      `reinitialize_preserves_example_moved, `reinitialize_preserves_planted_moved_reinitializeUnissuedPlace,
      `borrow_preserves_example_shared, `borrow_preserves_planted_shared_borrowNoAdvanceSupply,
      `reborrowTo_preserves_example_exclusive, `reborrowTo_preserves_planted_exclusive_reborrowToDropParent,
      `reborrow_preserves_example_exclusive, `reborrow_preserves_planted_exclusive_reborrowDropAncestors,
      `endLoan_preserves_example_child, `endLoan_preserves_planted_child_endLoanRemoveParent,
      `example_Computation_observation, `planted_Computation_observation, `example_Computation_observation_2,
      `planted_Computation_observation_2, `bind_preserves_example_shared,
      `bind_preserves_planted_shared_bindNoAdvancePlace, `drop_preserves_example_moved,
      `drop_preserves_planted_moved_dropLocalOnly, `enter_preserves_example_shared,
      `enter_preserves_planted_shared_enterDuplicateFrameId,
      `example_endLocals_preserves_also_covers_refusal_while_caller,
      `example_successful_cleanup_preserves_populated_caller_ledger,
      `planted_endLocalsAllowChildren_violates_endLocals_preserves_ending_parent_before,
      `leave_preserves_example_localLoan, `leave_preserves_planted_localLoan_leaveKeepSlots,
      `enterDown_preserves_example_exclusive, `enterDown_preserves_planted_exclusive_enterDownReuseParentId,
      `life_preserves_example_shared, `life_preserves_planted_shared_lifeBindUnindexed,
      `adoptDown_preserves_example_adoptionInput,
      `adoptDown_preserves_planted_adoptionInput_adoptDownUnissuedActivation,
      `passDown_preserves_example_exclusive, `passDown_preserves_planted_exclusive_passDownClearParent,
      `passDown_call_preserves_example_exclusive,
      `passDown_call_preserves_planted_exclusive_passDownOmitPending, `callLife_preserves_example_waiting,
      `callLife_preserves_planted_waiting_callLifeUnissuedActivation,
      `callLife_call_preserves_example_waiting, `callLife_call_preserves_planted_waiting_callLifeKeepPending,
      `passOwner_preserves_example_caller, `passOwner_preserves_planted_caller_passOwnerClearCallRoot,
      `passOwner_call_preserves_example_caller, `passOwner_call_preserves_planted_caller_passOwnerOmitPending],
    evidence `ConcreteSpec.Proofs.Own `ConcreteSpec.Core.Own.LifetimeEvidence [
      `example_leave_no_local_loan_also_covers_refusal_when_caller, `example_exit_ends_live_local_loan_and,
      `planted_leaveWithLoans_violates_leave_no_local_loan_omitting_only_root,
      `leave_no_activation_slot_example_localLoan, `leave_no_activation_slot_planted_localLoan_leaveKeepSlots,
      `passOwner_freezes_example_caller, `passOwner_freezes_planted_caller_passOwnerNoFreeze,
      `example_leave_retains_caller_preserves_this_loan_across_successful,
      `planted_leaveAllLoans_changes_only_cleanup_selection_and, `leave_no_activation_loan_example_own,
      `leave_no_activation_loan_planted_own_leaveWithoutEnding, `reborrowTo_live_example_exclusive,
      `reborrowTo_live_planted_exclusive_reborrowToDead, `adoptDown_live_example_adoptionInput,
      `adoptDown_live_planted_adoptionInput_adoptDownUnissuedActivation, `leave_live_example_own,
      `leave_live_planted_own_leaveWithoutEnding, `planted_ownership_calculation,
      `planted_ownership_calculation_2, `planted_issued_but_inactive_activation_cannot_retain,
      `planted_callee_cannot_index_caller_s_Copy, `example_Cleanup_removes_every_alias_removed_place,
      `planted_same_omitted_root_check_adds_transition],
    evidence `ConcreteSpec.Proofs.Own `ConcreteSpec.Core.Own.FinalEvidence [
      `enter_progress_example_shared, `enter_progress_planted_shared_enterRefuse,
      `directOwner_roundtrip_example_caller, `directOwner_roundtrip_planted_caller_callLifeLeaked,
      `directOwner_roundtrip_planted_caller_passOwnerWrongSupply,
      `example_roundtrip_mutant_succeeds_through_passOwner_enter],
    evidence `ConcreteSpec.Proofs.Own `ConcreteSpec.Core.Own.SuccessfulInstances [
      `example_preservation_and_lifetime_fixtures_actually_take],
    evidence `ConcreteSpec.Proofs.Own `ConcreteSpec.Core.Own.RoundTwoEvidence [
      `example_nonzero_target_is_restored_and_other, `planted_each_wrong_selector_fails_same_populated,
      `example_shared_direct_argument_freezes_owner_writes, `example_return_ends_populated_callRoot_loan_not,
      `planted_callRoot_survives_omitted_cleanup_and_targets,
      `example_issued_but_departed_activation_refuses_reborrowing,
      `planted_same_destination_and_arguments_strand_child],
    evidence `ConcreteSpec.Proofs.Own `ConcreteSpec.Core.Own.PendingEvidence [
      `passOwner_call_preserves_example_first, `passOwner_call_preserves_planted_first_passOwnerDuplicate,
      `passDown_call_preserves_example_down, `passDown_call_preserves_planted_down_passDownDuplicate,
      `passDown_call_preserves_example_down_2, `passOwner_call_preserves_example_first_2,
      `example_both_shared_argument_paths_accept_second, `callLife_call_preserves_example_exiting,
      `callLife_call_preserves_planted_exiting_callLifePending],
    evidence `ConcreteSpec.Proofs.Basics `ConcreteSpec.Core [
      `example_refusal_imposes_no_postcondition_even_impossible,
      `example_successful_result_must_satisfy_its_actual,
      `planted_refusal_case_distinguishes_generic_policy_from,
      `planted_wrong_successful_value_distinguishes_policy_from,
      `example_equivalence_holds_in_both_directions_at],
    decls `ConcreteSpec.Spec.Basics [`ConcreteSpec.Core.ResultSatisfies],
    laws `ConcreteSpec.Proofs.Own [
      `ConcreteSpec.Core.Own.adoptDown_live,
      `ConcreteSpec.Core.Own.adoptDown_preserves,
      `ConcreteSpec.Core.Own.bind_preserves,
      `ConcreteSpec.Core.Own.borrow_preserves,
      `ConcreteSpec.Core.Own.callLife_call_preserves,
      `ConcreteSpec.Core.Own.callLife_preserves,
      `ConcreteSpec.Core.Own.directOwner_roundtrip,
      `ConcreteSpec.Core.Own.drop_preserves,
      `ConcreteSpec.Core.Own.endLoan_preserves,
      `ConcreteSpec.Core.Own.endLocals_preserves,
      `ConcreteSpec.Core.Own.enterDown_preserves,
      `ConcreteSpec.Core.Own.enter_preserves,
      `ConcreteSpec.Core.Own.enter_progress,
      `ConcreteSpec.Core.Own.leave_live,
      `ConcreteSpec.Core.Own.leave_no_activation_loan,
      `ConcreteSpec.Core.Own.leave_no_activation_slot,
      `ConcreteSpec.Core.Own.leave_no_local_loan,
      `ConcreteSpec.Core.Own.leave_preserves,
      `ConcreteSpec.Core.Own.leave_retains_caller,
      `ConcreteSpec.Core.Own.life_preserves,
      `ConcreteSpec.Core.Own.move_preserves,
      `ConcreteSpec.Core.Own.passDown_call_preserves,
      `ConcreteSpec.Core.Own.passDown_preserves,
      `ConcreteSpec.Core.Own.passOwner_call_preserves,
      `ConcreteSpec.Core.Own.passOwner_freezes,
      `ConcreteSpec.Core.Own.passOwner_preserves,
      `ConcreteSpec.Core.Own.reborrowTo_live,
      `ConcreteSpec.Core.Own.reborrowTo_preserves,
      `ConcreteSpec.Core.Own.reborrow_preserves,
      `ConcreteSpec.Core.Own.reinitialize_preserves]],
  present "F15" "Execution and argument erasure" [
    evidence `ConcreteSpec.Proofs.OwnErasureEvidence `ConcreteSpec.Core.Own.ExecErasureWitnesses [
      `example_Moving_initialized_owner_in_instrumented_operand,
      `example_trapping_operand_in_instrumented_instance_traps,
      `example_Left_right_instrumented_argument_evaluation_erases,
      `example_argument_list_traps_after_move_traps,
      `example_Condition_testing_agrees_between_instrumented_and,
      `example_Binding_fresh_variable_updates_ownership_frame,
      `example_allowed_primitive_answer_satisfies_erased_primitive, `requestErasure,
      `example_Both_silent_and_primitive_event_single, `example_multi_step_instrumented_run_emitting_two,
      `example_Non_stuck_fuel_indexed_runs_returned, `example_full_instrumented_run_primitive_events_store,
      `example_On_checker_accepted_configurations_acceptedMoveConfig_and,
      `example_For_both_non_stuck_acceptedMoveConfig_and,
      `example_For_checker_accepted_programs_instrumented_run, `step_next_erasure_planted,
      `step_request_erasure_planted, `Step_erasure_example, `Step_erasure_planted, `Steps_erasure_example,
      `Steps_erasure_planted, `runFrom_erasure_example, `runFrom_erasure_planted, `run_erasure_example,
      `run_erasure_planted, `step_stuck_iff_of_accepted_example, `step_stuck_iff_of_accepted_planted,
      `runFrom_erasure_of_accepted_example, `runFrom_erasure_of_accepted_planted,
      `run_stuck_iff_of_accepted_example, `run_stuck_iff_of_accepted_planted],
    evidence `ConcreteSpec.Proofs.OwnErasureEvidence `ConcreteSpec.Core.Own.ArgumentEvidence [
      `evalArgs_ok_erasure_example, `evalArgs_ok_erasure_planted, `evalArgs_trap_erasure_planted],
    evidence `ConcreteSpec.Proofs.OwnErasureEvidence `ConcreteSpec.Core.Own.PopulatedErasureEvidence [
      `example_successful_operand_argument_erasure_exercises_live, `evalArgs_trap_erasure_example,
      `example_silent_erasure_succeeds_on_populated_locals,
      `example_primitive_erasure_exposes_same_argument_state,
      `example_relational_single_step_and_finite_erasure, `example_forward_fuel_and_whole_run_erasure,
      `example_accepted_converse_runs_in_both_directions, `step_stuck_iff_of_accepted_planted,
      `evalArgs_trap_erasure_planted, `step_next_erasure_planted],
    evidence `ConcreteSpec.Proofs.OwnErasureEvidence `ConcreteSpec.Core.Own.AddedAnswerEvidence [
      `run_stuck_iff_of_accepted_example, `runFrom_erasure_of_accepted_planted],
    evidence `ConcreteSpec.Proofs.OwnErasureEvidence `ConcreteSpec.Core.Own.RefusalEvidence [
      `eval_refusal_example, `eval_refusal_planted_shared, `evalArgs_refusal_example,
      `evalArgs_refusal_planted, `life_refusal_example, `life_refusal_planted_shared,
      `step_eval_refusal_example, `step_eval_refusal_planted, `step_life_refusal_example,
      `step_life_refusal_planted, `evalArgs_stuck_iff_of_permitted_example,
      `evalArgs_trap_iff_of_permitted_example, `evalArgs_erasure_of_permitted_planted,
      `evalArgs_erasure_of_permitted_planted_2, `evalArgs_stuck_iff_of_permitted_planted,
      `evalArgs_trap_iff_of_permitted_planted, `evalArgs_erasure_of_permitted_example,
      `evalArgs_stuck_iff_of_permitted_example_2, `evalArgs_erasure_of_permitted_planted_3,
      `example_Helper_observation_permission_rejects_poisoned_state,
      `evalArgs_stuck_iff_of_permitted_planted_2, `evalArgs_trap_iff_of_permitted_example_2,
      `evalArgs_trap_iff_of_permitted_planted_2, `evalArgs_ok_erasure_planted, `step_next_erasure_planted,
      `runFrom_erasure_planted, `run_erasure_planted_shared, `evalArgs_stuck_iff_of_permitted_planted_3,
      `evalArgs_trap_iff_of_permitted_planted_3],
    evidence `ConcreteSpec.Proofs.OwnErasureEvidence `ConcreteSpec.Core.Own.RelationalConverseEvidence [
      `Step_converse_of_accepted_example, `Step_converse_of_accepted_planted,
      `Steps_converse_of_accepted_example, `Steps_converse_of_accepted_planted,
      `Step_converse_of_accepted_example_2, `runFrom_erasure_of_accepted_planted],
    decls `ConcreteSpec.Proofs.OwnErasureEvidence [`ConcreteSpec.Core.Own.ExecErasureWitnesses.bindReadErasure, `ConcreteSpec.Core.Own.ExecErasureWitnesses.unconditionalRunConverse_refuted],
    laws `ConcreteSpec.Proofs.OwnErasure [
      `ConcreteSpec.Core.Own.Step_converse_of_accepted,
      `ConcreteSpec.Core.Own.Step_erasure,
      `ConcreteSpec.Core.Own.Steps_converse_of_accepted,
      `ConcreteSpec.Core.Own.Steps_erasure,
      `ConcreteSpec.Core.Own.evalArgs_erasure_of_permitted,
      `ConcreteSpec.Core.Own.evalArgs_ok_erasure,
      `ConcreteSpec.Core.Own.evalArgs_refusal,
      `ConcreteSpec.Core.Own.evalArgs_stuck_iff_of_permitted,
      `ConcreteSpec.Core.Own.evalArgs_trap_erasure,
      `ConcreteSpec.Core.Own.evalArgs_trap_iff_of_permitted,
      `ConcreteSpec.Core.Own.eval_refusal,
      `ConcreteSpec.Core.Own.life_refusal,
      `ConcreteSpec.Core.Own.runFrom_erasure,
      `ConcreteSpec.Core.Own.runFrom_erasure_of_accepted,
      `ConcreteSpec.Core.Own.run_erasure,
      `ConcreteSpec.Core.Own.run_stuck_iff_of_accepted,
      `ConcreteSpec.Core.Own.step_eval_refusal,
      `ConcreteSpec.Core.Own.step_life_refusal,
      `ConcreteSpec.Core.Own.step_next_erasure,
      `ConcreteSpec.Core.Own.step_request_erasure,
      `ConcreteSpec.Core.Own.step_stuck_iff_of_accepted]],
  pending "F16" ["ROADMAP.md R-0611"],
  present "CAP" "Shared capability type and enumeration" [
    decls `ConcreteSpec.Spec.CapabilityBase [`ConcreteSpec.Authority.Cap, `ConcreteSpec.Authority.Cap.all]],
  pending "F17" ["ROADMAP.md R-0611", "ISSUES.tsv ISS-0081"],
  pending "F18" ["ROADMAP.md R-0615", "ISSUES.tsv ISS-0049", "docs/decisions.md#decided-2026-11-17-proposals-in-lean (ISS-0050 writer brief)"],
  pending "F19" ["ROADMAP.md R-0612", "ISSUES.tsv ISS-0084"],
  pending "F20" ["ROADMAP.md R-0612", "ISSUES.tsv ISS-0084"],
  pending "F20p" ["ISSUES.tsv ISS-0068", "ROADMAP.md R-0612"],
  pending "F21" ["ROADMAP.md R-0612"],
  pending "F22" ["ROADMAP.md R-0612", "ISSUES.tsv ISS-0068"],
  pending "F23" ["ROADMAP.md R-0612", "ISSUES.tsv ISS-0084"],
  pending "F24a" ["ROADMAP.md R-0614"],
  pending "F24b" ["ROADMAP.md R-0614"],
  pending "F25a" ["ROADMAP.md R-0614"],
  pending "F25b" ["ROADMAP.md R-0614"],
  pending "F26" ["ROADMAP.md R-0613"],
  added "F26a" "F26 accounting slice in C7"
    "changelog.d/2026-11-15-spec-f26a-accounting.md; ROADMAP.md R-0613" [
    evidence `ConcreteSpec.Spec.Accounting `ConcreteSpec.Core.Leak [
      `example_execution_check_evaluating_run_on_allocate, `example_refusal_check_showing_apply_returns_none],
    evidence `ConcreteSpec.Proofs.Accounting `ConcreteSpec.Core.Leak [
      `empty_memory_has_well_formed_account,
      `peak_below_live_is_invalid,
      `setting_account_preserves_other_lineage,
      `bad_peak_account_is_invalid,
      `run_refuses_excessive_free,
      `free_can_release_entry_bytes,
      `allocation_totals_by_lineage,
      `free_totals_by_lineage,
      `reset_totals_by_lineage,
      `bound_accepts_overestimate,
      `bound_refuses_underestimate,
      `optional_bound_accepts_established_fact,
      `optional_bound_refuses_underestimate,
      `optional_bound_refuses_missing_fact,
      `report_refuses_wrong_lineage],
    evidence `ConcreteSpec.Proofs.Accounting `ConcreteSpec.Core.Leak.Audit [
      `alloc_zero_example_accountA, `alloc_zero_planted_accountA_allocInventsByte,
      `same_account_write_preserves_memory,
      `corrupting_setter_changes_memory,
      `function_entry_peak_example_freeAfterEntry,
      `function_entry_peak_planted_freeAfterEntry_enterLowersPeak, `apply_alloc_zero_example_memoryA,
      `apply_alloc_zero_planted_memoryA_applyCorruptsOther, `free_conservation_example_memoryA,
      `reset_conservation_example_memoryA, `reset_conservation_planted_memoryA_applyBadReset,
      `run_wf_example_conservationActions, `run_conservation_example_conservationActions,
      `run_peak_monotone_example_conservationActions, `run_peak_exact_example_conservationActions,
      `report_certified_example_established, `report_certified_example_mixedStatus,
      `report_certified_example_emptyFunction,
      `observed_peak_for_populated_and_empty_runs, `free_keeps_live,
      `run_wf_planted, `conservation_planted, `monotone_planted, `exact_peak_planted, `report_planted,
      `report_certified_planted_absolutePeak, `report_certified_planted_reversedNet,
      `report_certified_planted_inventedFootprint, `run_historical_peak_exact_example_populatedHistory,
      `historical_exact_planted, `free_permitted_iff_example_ownedAndExcessive,
      `free_permitted_iff_planted_admitsExcessiveOutflow, `free_permitted_iff_planted_refusesOwnedOutflow,
      `free_permitted_iff_example_doubleFree, `run_conservation_planted_doubleFreeAdmitted,
      `run_conservation_planted_extraFreedByte, `run_conservation_planted_extraResetByte,
      `no_allocation_reports_zero_peak, `net_interval_permits_entry_release,
      `upper_only_check_accepts_excessive_release],
    decls `ConcreteSpec.Spec.Accounting [`ConcreteSpec.Core.Leak.Account, `ConcreteSpec.Core.Leak.run, `ConcreteSpec.Core.Leak.LineageReport.Sound],
    evidence `ConcreteSpec.Spec.Rules `ConcreteSpec.Core.Leak.Registry [
      `wfAccepted, `wfRefused, `resetAccepted, `resetRefused,
      `boundAccepted, `boundRefused, `optionalAccepted, `optionalRefused,
      `reportAccepted, `reportRefused, `actionAccepted, `actionRefused,
      `sequenceAccepted, `sequenceRefused, `signedAccepted, `signedRefused],
    laws `ConcreteSpec.Proofs.Accounting [
      `ConcreteSpec.Core.Leak.Account.alloc_zero,
      `ConcreteSpec.Core.Leak.apply_alloc_zero,
      `ConcreteSpec.Core.Leak.free_conservation,
      `ConcreteSpec.Core.Leak.free_permitted_iff,
      `ConcreteSpec.Core.Leak.function_entry_peak,
      `ConcreteSpec.Core.Leak.report_certified,
      `ConcreteSpec.Core.Leak.reset_conservation,
      `ConcreteSpec.Core.Leak.run_conservation,
      `ConcreteSpec.Core.Leak.run_historical_peak_exact,
      `ConcreteSpec.Core.Leak.run_peak_exact,
      `ConcreteSpec.Core.Leak.run_peak_monotone,
      `ConcreteSpec.Core.Leak.run_wf]],
  disposition "F27" "deferred by owner"
    "ISSUES.tsv ISS-0072; docs/decisions.md#decided-2026-11-16-secret-wiping",
  pending "F28" ["ROADMAP.md R-0616", "ISSUES.tsv ISS-0073"],
  pending "F29" ["ROADMAP.md R-0616", "ISSUES.tsv ISS-0073"],
  pending "F29p" ["ROADMAP.md R-0616", "ISSUES.tsv ISS-0068"],
  pending "F30" ["ROADMAP.md R-0608 and R-0571"],
  pending "F31" ["ROADMAP.md R-0608 and R-0571", "ISSUES.tsv ISS-0069"],
  pending "F32" ["ROADMAP.md R-0574 and R-0571"],
  pending "F33" ["ROADMAP.md R-0571 and R-0609", "ISSUES.tsv ISS-0069, ISS-0070 and ISS-0076"],
  pending "F34" ["ROADMAP.md R-0607 and R-0571"],
  pending "F35" ["ROADMAP.md R-0607, R-0612 and R-0571"],
  pending "F36" ["ROADMAP.md R-0611 and R-0571"],
  pending "F37" ["ROADMAP.md R-0611 and R-0571"],
  pending "F38" ["ROADMAP.md R-0607 and R-0571"],
  disposition "F39-design" "folded into F39"
    "ROADMAP.md R-0611; docs/decisions.md#decided-2026-11-17-proposals-in-lean",
  pending "F39" ["ROADMAP.md R-0611 and R-0571"],
  pending "F40" ["ROADMAP.md R-0571 and R-0555"],
  qualifies "M01" "Basics",
  qualifies "M02" "Authority",
  qualifies "M03" "Borrow",
  qualifies "M04" "Brand",
  qualifies "M05" "Checker",
  qualifies "M06" "Cleanup",
  qualifies "M07" "Examples",
  qualifies "M08" "Observe",
  qualifies "M09" "Rep" [laws `ConcreteSpec.Proofs.Rep [`ConcreteSpec.Rep.rep_roundtrip]],
  qualifies "M10" "Slice0",
  qualifies "M11" "Slice1",
  qualifies "M12" "Surface0"]


private def origin? (env : Environment) (name : Name) : Option Name :=
  if let some i := env.getModuleIdxFor? name then
    env.header.moduleNames[i.toNat]?
  else if env.contains name then
    some env.header.mainModule
  else
    none

private def isLawDoc (doc : String) : Bool :=
  let doc := doc.trimAscii.toString
  doc.startsWith "Law" && ((doc.drop 3).toString.toList.head?).all
    (fun c => c.isWhitespace || ":.(".contains c)

private def readLaws (env : Environment) (inScope : Name → Bool) :
    CommandElabM (NameMap Name) := do
  let mut found := {}
  for (name, info) in env.constants do
    let .thmInfo _ := info | continue
    if (privateToUserName name).isInternal then continue
    let some origin := origin? env name | continue
    if !inScope origin then continue
    if let some doc ← findDocString? env name then
      if isLawDoc doc && (← liftCoreM (findDeclarationRanges? name)).isSome then
        found := found.insert name origin
  return found

structure Result where
  originalLanded : Nat := 0
  additionLanded : Nat := 0
  qualificationLanded : Nat := 0
  errors : Array String := #[]
  rows : Array String := #[]

private def inventory (env : Environment) (observedLaws : NameMap Name)
    (plan : List Piece) (integrityErrors : Array String := #[]) : Result := Id.run do
  let mut result : Result := { errors := integrityErrors }
  let mut seen : NameSet := {}
  let mut seenLaws : NameSet := {}
  let mut ids : Std.HashSet String := {}
  for piece in plan do
    let mut errors := #[]
    if ids.contains piece.id then
      errors := errors.push s!"duplicate piece {piece.id}"
    ids := ids.insert piece.id
    let claimsLanding := piece.remaining.isEmpty && piece.disposition.isNone
    if claimsLanding && piece.groups.isEmpty then
      errors := errors.push s!"{piece.id}: a landed piece needs evidence"
    for group in piece.groups do
      if group.declarations.isEmpty && group.laws.isEmpty then
        errors := errors.push s!"{piece.id}: empty evidence group"
      if (env.getModuleIdx? group.origin).isNone && group.origin != env.header.mainModule then
        errors := errors.push s!"{piece.id}: missing module {group.origin}"
      for name in group.declarations ++ group.laws do
        if seen.contains name then
          errors := errors.push s!"{piece.id}: duplicate planned declaration {name}"
        seen := seen.insert name
        let expectsLaw := group.laws.contains name
        if expectsLaw then seenLaws := seenLaws.insert name
        match origin? env name with
        | none => errors := errors.push s!"{piece.id}: missing declaration {name} in {group.origin}"
        | some actual =>
          if actual != group.origin then
            errors := errors.push s!"{piece.id}: {name} is in {actual}, expected {group.origin}"
          else if expectsLaw && !observedLaws.contains name then
            errors := errors.push s!"{piece.id}: {name} is not an authored Law theorem"
    let landed := errors.isEmpty && claimsLanding
    let status :=
      if !errors.isEmpty then "incomplete"
      else if landed then "landed"
      else piece.disposition.getD (if piece.groups.isEmpty then "pending" else "partial")
    let scope := if piece.scope.isEmpty then "" else s!" — {piece.scope}"
    result := {
      originalLanded := result.originalLanded +
        (if landed && piece.scopeKind == .original then 1 else 0)
      additionLanded := result.additionLanded +
        (if landed && piece.scopeKind == .addition then 1 else 0)
      qualificationLanded := result.qualificationLanded +
        (if landed && piece.scopeKind == .qualification then 1 else 0)
      errors := result.errors ++ errors
      rows := (result.rows.push s!"{piece.id}: {status}{scope}") ++
        (piece.remaining.map (s!"  remaining: {·}")).toArray
    }
  let unplanned := observedLaws.toList.filterMap fun (name, origin) =>
    if seenLaws.contains name then none else some s!"unplanned Law {name} ({origin})"
  return { result with errors := result.errors ++ unplanned.toArray.qsort (· < ·) }

-- Guard the reconciled totals without a second list of rows.
private def checkPlanSize (plan : List Piece) : Array String := Id.run do
  let mut errors := #[]
  for (kind, label, expected) in [
      (Scope.original, "original spec plan", 51), (.addition, "recorded additions", 3),
      (.qualification, "theorem qualification", 12)] do
    let actual := (plan.filter (·.scopeKind == kind)).length
    if actual != expected then
      errors := errors.push s!"{label}: expected {expected} rows, found {actual}"
  return errors

private def checkInventory (env : Environment) (observedLaws : NameMap Name) (plan : List Piece)
    (rootErrors : Array String) : Result :=
  inventory env observedLaws plan (rootErrors ++ checkPlanSize plan)

private def rootErrors (env : Environment) (modules : Array Name) : Array String :=
  (modules.filterMap fun name =>
    if (env.getModuleIdx? name).isNone then
      some s!"spec module outside the root import closure: {name}"
    else none).qsort (· < ·)

private def checkRoot (env : Environment) : IO (Array String) := do
  let files ← (System.FilePath.mk "ConcreteSpec").walkDir
  let modules := files.filterMap fun file =>
    if file.extension == some "lean" then
      some ((file.withExtension "").components.foldl Name.str .anonymous)
    else none
  return rootErrors env modules

namespace Control

theorem retained : True := trivial

/-- Law: inventory control. -/
theorem newName : True := trivial

/-- Law: this tagged definition must be excluded. -/
def nonTheorem : Bool := true

def verify (env : Environment) (specLaws : NameMap Name) : CommandElabM Unit := do
  let origin := env.header.mainModule
  let observed ← readLaws env (· == origin)
  let good := present "test" "" [laws origin [`SpecLanding.Control.newName]]
  let check (pieces : List Piece) := (inventory env observed pieces).errors
  let hasError (pieces : List Piece) (message : String) : CommandElabM Unit := do
    unless (check pieces).contains message do
      throwError "inventory control missed {message}"
  unless (check [good]).isEmpty do
    throwError "inventory control: updated plan did not resolve the rename"
  hasError [present "test" "" [laws origin [`SpecLanding.Control.oldName]]]
    s!"test: missing declaration SpecLanding.Control.oldName in {origin}"
  hasError [present "test" "" [decls origin [`SpecLanding.Control.absent]]]
    s!"test: missing declaration SpecLanding.Control.absent in {origin}"
  hasError [present "test" "" [decls origin [`SpecLanding.Control.newName]]]
    s!"unplanned Law SpecLanding.Control.newName ({origin})"
  for name in [`SpecLanding.Control.retained, `SpecLanding.Control.nonTheorem] do
    hasError [present "test" "" [laws origin [name]]]
      s!"test: {name} is not an authored Law theorem"
  hasError [present "test" "" [decls origin []]] "test: empty evidence group"
  hasError [present "test" "" []] "test: a landed piece needs evidence"
  hasError [good, good] "duplicate piece test"
  hasError [good, { good with id := "other" }] "other: duplicate planned declaration SpecLanding.Control.newName"
  hasError [present "test" "" [laws `ConcreteSpec.Proofs.Basics [`SpecLanding.Control.newName]]]
    s!"test: SpecLanding.Control.newName is in {origin}, expected ConcreteSpec.Proofs.Basics"
  hasError [present "test" "" [laws `MissingModule [`SpecLanding.Control.newName]]]
    "test: missing module MissingModule"
  let removed := checkInventory env specLaws (plan.filter (·.id != "F04p")) #[]
  unless removed.errors == #["original spec plan: expected 51 rows, found 50"] do
    throwError "inventory control: removing a planned row was accepted"
  let orphan := rootErrors env #[`ConcreteSpec.Spec.Basics, `ConcreteSpec.PlantedOrphan]
  let checked := inventory env {} [] orphan
  unless checked.errors == #["spec module outside the root import closure: ConcreteSpec.PlantedOrphan"] do
    throwError "inventory control: root integrity depends on the plan rows"
  IO.println "inventory controls passed"

end Control

#eval show CommandElabM Unit from do
  let env ← getEnv
  let observed ← readLaws env (fun origin => origin.getRoot == `ConcreteSpec)
  let result := checkInventory env observed plan (← checkRoot env)
  for row in result.rows ++ result.errors do
    IO.println row
  if !result.errors.isEmpty then throwError "spec landing inventory failed"
  Control.verify env observed
  let (qualifications, spec) := plan.partition (·.scopeKind == .qualification)
  let (original, additions) := spec.partition (·.scopeKind == .original)
  let dispositionCount := (original.filter (·.disposition.isSome)).length
  let landed := result.originalLanded + result.additionLanded
  IO.println s!"spec landing inventory: {landed}/{spec.length} spec pieces landed"
  IO.println s!"  original plan: {result.originalLanded}/{original.length} landed; recorded additions: {result.additionLanded}/{additions.length} landed"
  IO.println s!"  active spec scope: {landed}/{spec.length - dispositionCount} landed ({dispositionCount} historical rows folded or deferred)"
  IO.println s!"theorem qualification: {result.qualificationLanded}/{qualifications.length} rows landed"

end SpecLanding
