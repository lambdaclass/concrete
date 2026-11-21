import ConcreteSpec
import Concrete
import Lean

open Lean Elab Command

namespace ConcreteSpec.Core.SourcePathSpan

def inventoryHelper : Nat := 0

end ConcreteSpec.Core.SourcePathSpan

namespace ConcreteSpec.Core.Observe
/-- Law: the environment fixture preserves zero. -/
theorem EnvironmentEvidence.law : 0 + 0 = (0 : Nat) := rfl
/-- Planted: the environment fixture detects adding one. -/
theorem EnvironmentEvidence.planted : 0 + 1 ≠ (0 : Nat) := by decide
def OutputEvidence.helper : Nat := 0
/-- Law: the output fixture preserves zero. -/
theorem OutputEvidence.law : 0 + 0 = (0 : Nat) := rfl
/-- Planted: the output fixture detects adding one. -/
theorem OutputEvidence.planted : 0 + 1 ≠ (0 : Nat) := by decide
end ConcreteSpec.Core.Observe

namespace SpecLanding

structure Group where
  origin : Name
  declarations : List Name := []
  laws : List Name := []

inductive Scope where
  | original | addition | qualification
  deriving BEq

inductive PieceId where
  | F01 | F02 | F03 | F03b | «F03p-design» | F03p | F04 | F04p
  | R | F05 | F06 | F07 | F08 | F09a | F09b | F09c
  | F10 | F11 | F12 | F13 | F14 | F15 | F16 | CAP
  | F17 | F18 | F19 | F20 | F20p | F21 | F22 | F23
  | F24a | F24b | F25a | F25b | F26 | F26a | F27 | F28
  | F29 | F29p | F30 | F31 | F32 | F33 | F34 | F35
  | F36 | F37 | F38 | «F39-design» | F39 | F40 | M01 | M02
  | M03 | M04 | M05 | M06 | M07 | M08 | M09 | M10
  | M11 | M12
  deriving BEq, Hashable, ToExpr

instance : ToString PieceId where
  toString id := (toExpr id).constName!.getString!

private def scopeKind : PieceId → Scope
  | .F03b | .R | .F26a => .addition
  | .M01 | .M02 | .M03 | .M04 | .M05 | .M06
  | .M07 | .M08 | .M09 | .M10 | .M11 | .M12 => .qualification
  | .F01 | .F02 | .F03 | .«F03p-design» | .F03p | .F04 | .F04p
  | .F05 | .F06 | .F07 | .F08 | .F09a | .F09b | .F09c | .F10
  | .F11 | .F12 | .F13 | .F14 | .F15 | .F16 | .CAP | .F17 | .F18
  | .F19 | .F20 | .F20p | .F21 | .F22 | .F23 | .F24a | .F24b
  | .F25a | .F25b | .F26 | .F27 | .F28 | .F29 | .F29p | .F30
  | .F31 | .F32 | .F33 | .F34 | .F35 | .F36 | .F37 | .F38
  | .«F39-design» | .F39 | .F40 => .original

structure Piece where
  id : PieceId
  scope : String
  groups : List Group := []
  remaining : List String := []
  disposition : Option String := none

private def present (id : PieceId) (scope : String) : Piece :=
  { id, scope }

private def added (id : PieceId) (scope record : String) : Piece :=
  present id s!"{scope} ({record})"

private def pending (id : PieceId) (records : List String) : Piece :=
  { id, scope := "", remaining := records }

private def disposition (id : PieceId) (status record : String) : Piece :=
  { id, scope := record, disposition := some status }

private def qualifies (id : PieceId) (family : String) : Piece :=
  { id, scope := family
    remaining := ["ROADMAP.md R-0700; docs/decisions.md#decided-2026-11-13-proof-labels"] }

private def decls (origin : Name) (names : List Name) : Group :=
  { origin, declarations := names }

private def evidence (origin namespaceName : Name) (names : List Name) : Group :=
  decls origin (names.map (namespaceName ++ ·))

private def laws (origin : Name) (names : List Name) : Group :=
  { origin, laws := names }


structure EvidenceNamespace where
  origin : Name
  namespaceName : Name
  isProof : Bool := false
  ruleOrigin : Option Name := none

structure RuleRequirement where
  name : Name
  ruleOrigin : Name

structure Completion where
  named : List Group := []
  requiredSpec : List Group := [] -- Resolve planned spec declarations when the piece claims landing.
  namespaces : List EvidenceNamespace := []
  localRules : List RuleRequirement := []
  completionRules : List RuleRequirement := []

private def specNamespace (origin namespaceName : Name) : EvidenceNamespace :=
  { origin := `ConcreteSpec ++ origin, namespaceName := `ConcreteSpec ++ namespaceName
    isProof := (`Proofs).isPrefixOf origin }

private def registryNamespace (namespaceName ruleOrigin : Name) : EvidenceNamespace :=
  { origin := `ConcreteSpec.Spec.Rules, namespaceName := `ConcreteSpec ++ namespaceName
    ruleOrigin := some (`ConcreteSpec ++ ruleOrigin) }

private def namedEvidence (groups : List Group) : Completion :=
  { named := groups }

private def reservedEvidence (namespaces : List EvidenceNamespace) : Completion :=
  { namespaces }

-- These requirements belong to the piece, independently of its status.
-- Future namespaces reserve names for the new evidence, including in existing
-- modules. Quote planned identities even when they do not yet resolve.
-- F30 also cites compiler evidence, whose origin must be reachable from Concrete.
private def completion : PieceId → Completion
  | .F01 => namedEvidence [
    decls `ConcreteSpec.Proofs.Basics [`ConcreteSpec.Core.AList.lookup_cons]]
  | .F02 => namedEvidence [
    decls `ConcreteSpec.Spec.Basics [`ConcreteSpec.Var, `ConcreteSpec.Core.AList.lookup],
    decls `ConcreteSpec.Diagnostics.Types [`ConcreteSpec.LocalNames, `ConcreteSpec.Elaborated],
    decls `ConcreteSpec.Proofs.Basics [`ConcreteSpec.Core.AList.lookup_of_mem]]
  | .F03 => namedEvidence [
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
    decls `ConcreteSpec.Spec.Outcome [`ConcreteSpec.Core.Res, `ConcreteSpec.Core.TrapKind]]
  | .F03b => namedEvidence [
    evidence `ConcreteSpec.Spec.Ident `ConcreteSpec.Core [
      `example_newline_starts_new_based_byte_column],
    evidence `ConcreteSpec.Proofs.Ident `ConcreteSpec.Core.Vacuity [
      `example_aSource_aTab, `lineAt_zero_example_aSource, `lineAt_zero_planted_aSource_zeroBasedLine,
      `columnAt_zero_example_aSource, `columnAt_zero_planted_aSource_zeroBasedColumn],
    decls `ConcreteSpec.Spec.Ident [`ConcreteSpec.Core.SourceText.ofBytes, `ConcreteSpec.Core.SourceText.toBytes],
    decls `ConcreteSpec.Proofs.Ident [`ConcreteSpec.Core.SourceText.take_toBytes],
    laws `ConcreteSpec.Proofs.Ident [
      `ConcreteSpec.Core.SourceText.columnAt_zero, `ConcreteSpec.Core.SourceText.lineAt_zero]]
  | .F03p =>
    let rule (name : Name) : RuleRequirement := ⟨name, `ConcreteSpec.Spec.Ident⟩
    { named := [
    evidence `ConcreteSpec.Spec.Ident `ConcreteSpec.Core [
      `SourcePath, `SourcePath.parse, `SourcePath.render, `Span, `Span.LocationOk,
      `mainSourcePath, `tabSpan]],
      localRules := [`Nonempty, `Relative, `NoDrivePrefix, `NoTraversal, `NoSingleDot,
      `NonemptyComponents, `Ascii, `SlashSeparator, `NoControls, `NoSpaces, `NoLeadingDot,
      `NoTrailingDot, `NoReservedStem, `ComponentByteCap, `PathByteCap, `FilenameCharacters].map
        (fun name => rule (`ConcreteSpec.Core.SourcePath ++ name))
      -- Planned identities for the project-wide checks in R-0709.
      completionRules := [`ConcreteSpec.Core.SourcePath.Project.NoCaseCollision,
      `ConcreteSpec.Core.SourcePath.Project.NoFileDirectoryPrefixConflict].map rule }
  | .F04 => namedEvidence [
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
      `ConcreteSpec.Core.SpecializationTable.canonicalize_share]]
  | .F04p =>
    { named := [
    laws `ConcreteSpec.Proofs.Ident [
      `ConcreteSpec.Core.SourcePath.byte_probe_reference,
      `ConcreteSpec.Core.SourcePath.component_probe_reference,
      `ConcreteSpec.Core.SourcePath.permitted_reference_iff,
      `ConcreteSpec.Core.SourcePath.parse_iff,
      `ConcreteSpec.Core.SourcePath.admitted_iff,
      `ConcreteSpec.Core.SourcePath.admission_rule_iff,
      `ConcreteSpec.Core.SourcePath.admission_empty_iff,
      `ConcreteSpec.Core.SourcePath.admission_absolute_iff,
      `ConcreteSpec.Core.SourcePath.admission_drive_iff,
      `ConcreteSpec.Core.SourcePath.admission_traversal_iff,
      `ConcreteSpec.Core.SourcePath.admission_singleDot_iff,
      `ConcreteSpec.Core.SourcePath.admission_emptyComponent_iff,
      `ConcreteSpec.Core.SourcePath.admission_nonAscii_iff,
      `ConcreteSpec.Core.SourcePath.admission_backslash_iff,
      `ConcreteSpec.Core.SourcePath.admission_control_iff,
      `ConcreteSpec.Core.SourcePath.admission_space_iff,
      `ConcreteSpec.Core.SourcePath.admission_leadingDot_iff,
      `ConcreteSpec.Core.SourcePath.admission_trailingDot_iff,
      `ConcreteSpec.Core.SourcePath.admission_reserved_iff,
      `ConcreteSpec.Core.SourcePath.admission_componentCap_iff,
      `ConcreteSpec.Core.SourcePath.admission_pathCap_iff,
      `ConcreteSpec.Core.SourcePath.admission_disallowed_iff,
      `ConcreteSpec.Core.SourcePath.parse_refusal_iff,
      `ConcreteSpec.Core.SourcePath.parse_refusal_sound,
      `ConcreteSpec.Core.SourcePath.parse_render,
      `ConcreteSpec.Core.SourcePath.renderBytes_injective,
      `ConcreteSpec.Core.SourcePath.priority_reference,
      `ConcreteSpec.Core.SourcePath.firstViolation_order,
      `ConcreteSpec.Core.SourcePath.parse_refusal_order,
      `ConcreteSpec.Core.SourcePath.pathCap_iff,
      `ConcreteSpec.Core.SourcePath.componentCap_iff,
      `ConcreteSpec.Core.SourcePath.path_bytes_le,
      `ConcreteSpec.Core.SourcePath.component_bytes_le,
      `ConcreteSpec.Core.SourcePath.component_cap_offset,
      `ConcreteSpec.Core.SourcePath.components_exact,
      `ConcreteSpec.Core.SourcePath.component_slice,
      `ConcreteSpec.Core.SourcePath.component_cap_byte,
      `ConcreteSpec.Core.SourcePath.parse_refusal_location,
      `ConcreteSpec.Core.SourcePath.ascii_bytes,
      `ConcreteSpec.Core.SourcePath.render_exact,
      `ConcreteSpec.Core.SourcePath.render_injective,
      `ConcreteSpec.Core.SourcePath.parseString_render,
      `ConcreteSpec.Core.SourcePath.LocalControls.admittedBytes_iff],
    evidence `ConcreteSpec.Proofs.Ident `ConcreteSpec.Core [
      `Span.pathOk, `Span.locationOk_iff, `tabSpan_location, `tabSpan_otherSource,
      `tabSpan_badCoordinates, `SourcePath.LocalControls.foldedRenderingChangesIdentity]],
      namespaces := [specNamespace `Proofs.Ident `Core.SourcePathSpan] }
  | .R => namedEvidence [
    decls `ConcreteSpec.Spec.Rules [`ConcreteSpec.Core.registeredRules,
      `ConcreteSpec.Core.registeredRuleExamples, `ConcreteSpec.Core.RuleAccepted,
      `ConcreteSpec.Core.RuleRefused]]
  | .F05 => namedEvidence [
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
      `ConcreteSpec.Core.wrapping_of_checked]]
  | .F06 =>
    { named := [
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
    decls `ConcreteSpec.Spec.Const [`ConcreteSpec.Core.CExpr, `ConcreteSpec.Core.CExpr.eval]],
      completionRules := [⟨`ConcreteSpec.Core.ConstsPermitted, `ConcreteSpec.Spec.Const⟩] }
  | .F07 => namedEvidence [
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
      `ConcreteSpec.Core.typed_evalConsts_refusal]]
  | .F08 => namedEvidence [
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
    decls `ConcreteSpec.Spec.Exec [`ConcreteSpec.Core.Exec.Operands, `ConcreteSpec.Core.Exec.Config, `ConcreteSpec.Core.Exec.step, `ConcreteSpec.Core.Exec.runFrom, `ConcreteSpec.Core.Exec.Report]]
  | .F09a => namedEvidence [
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
      `ConcreteSpec.Core.Exec.unwind_runs_plan]]
  | .F09b => namedEvidence [
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
      `ConcreteSpec.Core.Exec.steps_from_trapped]]
  | .F09c => namedEvidence [
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
      `ConcreteSpec.Core.Exec.rules_sound]]
  | .F10 => namedEvidence [
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
      `ConcreteSpec.Core.Exec.test_drop_refused]]
  | .F11 => namedEvidence [
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
      `ConcreteSpec.Core.Mem.store_wellFormed]]
  | .F12 => namedEvidence [
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
      `ConcreteSpec.Core.Hybrid.write_wellFormed]]
  | .F13 =>
    { named := [
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
    decls `ConcreteSpec.Spec.Own [`ConcreteSpec.Core.Own.State, `ConcreteSpec.Core.Own.WellFormed]],
      completionRules := [`OwnerPermitted, `LoanPermitted, `EndPermitted, `MovePermitted,
        `DropPermitted, `LeavePermitted].map
          (fun name => ⟨`ConcreteSpec.Core.Own ++ name, `ConcreteSpec.Spec.Own⟩) }
  | .F14 => namedEvidence [
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
      `ConcreteSpec.Core.Own.reinitialize_preserves]]
  | .F15 => namedEvidence [
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
      `ConcreteSpec.Core.Own.step_stuck_iff_of_accepted]]
  | .F16 => reservedEvidence [
    specNamespace `Proofs.Own `Core.Own.LoanHistory]
  | .CAP => namedEvidence [
    decls `ConcreteSpec.Spec.CapabilityBase [`ConcreteSpec.Authority.Cap, `ConcreteSpec.Authority.Cap.all]]
  | .F17 => reservedEvidence [
    specNamespace `Spec.Grant `Core.Grants,
    specNamespace `Proofs.Grant `Core.Grants,
    registryNamespace `Core.Grants.Registry `Spec.Grant]
  | .F18 => reservedEvidence [
    specNamespace `Spec.Prim `Core.Prim,
    specNamespace `Proofs.Prim `Core.Prim]
  | .F19 => { requiredSpec := [
    evidence `ConcreteSpec.Spec.Observe `ConcreteSpec.Core.Observe [
      `Observation, `FullObservation, `ExecAdapter, `reportObservation, `launchObservation]] }
  | .F20 => reservedEvidence [
    specNamespace `Proofs.Observe2 `Core.Observe.CodecLaunch]
  | .F20p => reservedEvidence [
    specNamespace `Proofs.Observe2 `Core.Observe.LoaderPathPolicy]
  | .F21 => reservedEvidence [
    specNamespace `Proofs.ObserveEnvironment `Core.Observe.EnvironmentEvidence,
    specNamespace `Proofs.ObserveOutput `Core.Observe.OutputEvidence]
  | .F22 => reservedEvidence [
    specNamespace `Proofs.ObserveSource `Core.Observe.SourceEvidence,
    specNamespace `Proofs.ObserveEvidence `Core.Observe.ProducerEvidence,
    specNamespace `Proofs.ObserveTransform `Core.Observe.TransformEvidence]
  | .F23 => reservedEvidence [
    specNamespace `Proofs.ObserveReport `Core.Observe.ReportEvidence]
  | .F24a => reservedEvidence [
    specNamespace `Spec.Adequacy `Core.Exec.FiniteQueries,
    specNamespace `Proofs.Adequacy `Core.Exec.FiniteQueries]
  | .F24b => reservedEvidence [
    specNamespace `Proofs.Adequacy `Core.Exec.FuelAndUniqueness]
  | .F25a => reservedEvidence [
    specNamespace `Proofs.Adequacy `Core.Exec.RunProjection]
  | .F25b => reservedEvidence [
    specNamespace `Proofs.Adequacy `Core.Exec.TerminationAndBlocking]
  | .F26 => reservedEvidence [
    specNamespace `Spec.Leak `Core.Leak.Release,
    specNamespace `Proofs.Leak `Core.Leak.Noninterference]
  | .F26a =>
    { named := [
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
      completionRules := [`Account.WF, `ResetConserved, `ByteBoundSound, `ByteBoundSoundOptional,
        `ReportSound, `FiniteActionPermitted, `FiniteSequencePermitted, `SignedNetSound].map
          (fun name => ⟨`ConcreteSpec.Core.Leak ++ name, `ConcreteSpec.Spec.Accounting⟩) }
  | .F28 => { requiredSpec := [
    evidence `ConcreteSpec.Spec.Frame `ConcreteSpec.Core.Frame [
      `C1Frames, `Machine.RegisterFrame, `callStack, `Launcher, `ProjectStartup]] }
  | .F29 => reservedEvidence [
    specNamespace `Proofs.Frame `Core.Frame.BoundaryEvidence]
  | .F29p => reservedEvidence [
    specNamespace `Proofs.Frame `Core.Frame.SourcePolicy]
  | .F30 => namedEvidence [
    evidence `Concrete.BIR `Concrete.BIR.ScalarBridge [
      `fromCore_toCore, `toCore_fromCore, `bits, `signed, `min, `max, `inRange,
      `checked, `wrap, `toBits, `ofBits, `intValue_valid],
    decls `Concrete.BIR.Progress [`Concrete.BIR.hasTy_int_core],
    decls `Concrete.Backend.LCB [`Concrete.Backend.LCB.Proof.enc_int_core]]
  | .F31 => { requiredSpec := [
    evidence `ConcreteSpec.Spec.Slice1Syntax `ConcreteSpec.ScalarMigration [`intType],
    evidence `ConcreteSpec.Spec.Statics `ConcreteSpec.ScalarMigration [`literalType],
    evidence `ConcreteSpec.Spec.Slice1Semantics `ConcreteSpec.ScalarMigration [`checkedAdd],
    evidence `ConcreteSpec.Spec.Slice1Ownership `ConcreteSpec.ScalarMigration [`atomBounds],
    evidence `ConcreteSpec.Spec.Slice1Checker `ConcreteSpec.ScalarMigration [`checkLiteral],
    evidence `ConcreteSpec.Spec.Slice0 `ConcreteSpec.ScalarMigration [`literalToCore],
    evidence `ConcreteSpec.Spec.Rep `ConcreteSpec.Rep.ScalarMigration [`decodeScalar],
    evidence `ConcreteSpec.Spec.Examples `ConcreteSpec.Examples.ScalarMigration [`literalTooBig]] }
  | .F32 => reservedEvidence [
    specNamespace `Proofs.Checker `CheckerMigration]
  | .F33 => reservedEvidence [
    specNamespace `Spec.Rep `Rep.CheckedView,
    specNamespace `Proofs.Rep `Rep.CheckedView]
  | .F34 => reservedEvidence [
    specNamespace `Proofs.Slice1 `Slice1CoreCorrespondence]
  | .F35 => reservedEvidence [
    specNamespace `Spec.Slice1Semantics `ExecObservationMigration,
    specNamespace `Spec.Slice1Checker `ExecObservationMigration,
    specNamespace `Spec.Observation `ExecObservationMigration,
    specNamespace `Spec.Slice0 `ExecObservationMigration,
    specNamespace `Spec.Examples `ExecObservationMigration,
    specNamespace `Proofs.Slice1 `ExecObservationMigration,
    specNamespace `Proofs.Checker `ExecObservationMigration,
    specNamespace `Proofs.Observe `ExecObservationMigration,
    specNamespace `Proofs.Slice0 `ExecObservationMigration,
    specNamespace `Proofs.Examples `ExecObservationMigration]
  | .F36 => reservedEvidence [
    specNamespace `Spec.Borrow `Borrow.CheckedView,
    specNamespace `Proofs.Borrow `Borrow.CheckedView,
    specNamespace `Spec.Surface0 `Surface0.BorrowMigration,
    specNamespace `Proofs.Surface0 `Surface0.BorrowMigration]
  | .F37 => reservedEvidence [
    specNamespace `Spec.Brand `Brand.CheckedView,
    specNamespace `Proofs.Brand `Brand.CheckedView]
  | .F38 => reservedEvidence [
    specNamespace `Spec.Cleanup `Cleanup.CheckedDependencies,
    specNamespace `Proofs.Cleanup `Cleanup.CheckedDependencies]
  | .F39 => reservedEvidence [
    specNamespace `Spec.Authority `Authority.GrantExecution,
    specNamespace `Proofs.Authority `Authority.GrantExecution]
  | .F40 => reservedEvidence [
    specNamespace `Spec.Examples `Examples.IntegratedFragments,
    specNamespace `Proofs.Examples `Examples.IntegratedFragments]
  | .M01 => reservedEvidence [
    specNamespace `Proofs.Basics `Qualification.Basics]
  | .M02 => reservedEvidence [
    specNamespace `Proofs.Authority `Qualification.Authority]
  | .M03 => reservedEvidence [
    specNamespace `Proofs.Borrow `Qualification.Borrow]
  | .M04 => reservedEvidence [
    specNamespace `Proofs.Brand `Qualification.Brand]
  | .M05 => reservedEvidence [
    specNamespace `Proofs.Checker `Qualification.Checker]
  | .M06 => reservedEvidence [
    specNamespace `Proofs.Cleanup `Qualification.Cleanup,
    specNamespace `Spec.Cleanup `Qualification.Cleanup]
  | .M07 => reservedEvidence [
    specNamespace `Proofs.Examples `Qualification.Examples]
  | .M08 => reservedEvidence [
    specNamespace `Proofs.Observe `Qualification.Observe]
  | .M09 =>
    { named := [laws `ConcreteSpec.Proofs.Rep [`ConcreteSpec.Rep.rep_roundtrip]]
      namespaces := [
      specNamespace `Proofs.Rep `Qualification.Rep] }
  | .M10 => reservedEvidence [
    specNamespace `Proofs.Slice0 `Qualification.Slice0]
  | .M11 => reservedEvidence [
    specNamespace `Proofs.Slice1 `Qualification.Slice1]
  | .M12 => reservedEvidence [
    specNamespace `Proofs.Surface0 `Qualification.Surface0]
  | .«F03p-design» | .«F39-design» | .F27 => {}

private def plan : List Piece := [
  present .F01 "Explicit proof imports and a complete specification root",
  present .F02 "Numeric variables, shared lookup, and separate diagnostics",
  present .F03 "Stable identities, target, outcome, and byte coordinates",
  added .F03b "F03 SourceText follow-on"
    "changelog.d/2026-11-17-spec-f03b-sourcetext.md",
  disposition .«F03p-design» "design proposals never land on main and stay on WIP branches as writer briefs. The Lean spec carries the normative decisions for F03p"
    "ISSUES.tsv ISS-0068; docs/decisions.md#decided-2026-11-17-proposals-in-lean",
  pending .F03p ["ROADMAP.md R-0709; ISSUES.tsv ISS-0068 (project-wide admission)"],
  present .F04 "Stable-key, namespace, obligation, contract, and byte-coordinate proofs",
  pending .F04p ["ROADMAP.md R-0709; ISSUES.tsv ISS-0068 (dependent project-wide proofs)"],
  added .R "Rule registry, prerequisite of F06, F13 and F17"
    "changelog.d/2026-11-13-spec-rule-registry.md; ROADMAP.md R-0615",
  present .F05 "Fixed-width arithmetic and exact-term correspondence",
  present .F06 "Constant expressions, admission, evaluation, and registry controls",
  present .F07 "Constant typing, evaluation, dependency order, and cycle laws",
  present .F08 "Parameterized executor, lifecycle transitions, residual state, and fuel",
  present .F09a "Elementary execution and structured exits",
  present .F09b "Cleanup and saved-caller segmentation",
  present .F09c "Relational determinism and dispatcher agreement",
  present .F10 "Lifecycle refusals and destruction ordering",
  present .F11 "Byte memory, provenance, and encoding",
  present .F12 "Integer hybrid storage and preservation",
  present .F13 "Ownership state, lending, and rule registry",
  present .F14 "Ownership preservation, activation and local lifetime proofs",
  present .F15 "Execution and argument erasure",
  pending .F16 ["ROADMAP.md R-0611"],
  present .CAP "Shared capability type and enumeration",
  pending .F17 ["ROADMAP.md R-0611", "ISSUES.tsv ISS-0081"],
  pending .F18 ["ROADMAP.md R-0615", "ISSUES.tsv ISS-0049", "docs/decisions.md#decided-2026-11-17-proposals-in-lean (ISS-0050 writer brief)"],
  pending .F19 ["ROADMAP.md R-0612", "ISSUES.tsv ISS-0084"],
  pending .F20 ["ROADMAP.md R-0612", "ISSUES.tsv ISS-0084"],
  pending .F20p ["ISSUES.tsv ISS-0068", "ROADMAP.md R-0612"],
  pending .F21 ["ROADMAP.md R-0612"],
  pending .F22 ["ROADMAP.md R-0612", "ISSUES.tsv ISS-0068"],
  pending .F23 ["ROADMAP.md R-0612", "ISSUES.tsv ISS-0084"],
  pending .F24a ["ROADMAP.md R-0614"],
  pending .F24b ["ROADMAP.md R-0614"],
  pending .F25a ["ROADMAP.md R-0614"],
  pending .F25b ["ROADMAP.md R-0614"],
  pending .F26 ["ROADMAP.md R-0613"],
  added .F26a "F26 accounting slice in C7"
    "changelog.d/2026-11-15-spec-f26a-accounting.md; ROADMAP.md R-0613",
  disposition .F27 "wiping is out of the spec and only the owner can reopen it"
    "ISSUES.tsv ISS-0072; docs/decisions.md#decided-2026-11-16-secret-wiping",
  pending .F28 ["ROADMAP.md R-0616", "ISSUES.tsv ISS-0073"],
  pending .F29 ["ROADMAP.md R-0616", "ISSUES.tsv ISS-0073"],
  pending .F29p ["ROADMAP.md R-0616", "ISSUES.tsv ISS-0068"],
  present .F30 "scalar bridge representation proofs (changelog.d/2026-11-16-spec-f30-scalar-bridge.md)",
  pending .F31 ["ROADMAP.md R-0608 and R-0571", "ISSUES.tsv ISS-0069"],
  pending .F32 ["ROADMAP.md R-0574 and R-0571"],
  pending .F33 ["ROADMAP.md R-0571 and R-0609", "ISSUES.tsv ISS-0069, ISS-0070 and ISS-0076"],
  pending .F34 ["ROADMAP.md R-0607 and R-0571"],
  pending .F35 ["ROADMAP.md R-0607, R-0612 and R-0571"],
  pending .F36 ["ROADMAP.md R-0611 and R-0571"],
  pending .F37 ["ROADMAP.md R-0611 and R-0571"],
  pending .F38 ["ROADMAP.md R-0607 and R-0571"],
  disposition .«F39-design» "design proposals never land on main and stay on WIP branches as writer briefs. The Lean spec carries the normative decisions for F39"
    "ROADMAP.md R-0611; docs/decisions.md#decided-2026-11-17-proposals-in-lean",
  pending .F39 ["ROADMAP.md R-0611 and R-0571"],
  pending .F40 ["ROADMAP.md R-0571 and R-0555"],
  qualifies .M01 "Basics",
  qualifies .M02 "Authority",
  qualifies .M03 "Borrow",
  qualifies .M04 "Brand",
  qualifies .M05 "Checker",
  qualifies .M06 "Cleanup",
  qualifies .M07 "Examples",
  qualifies .M08 "Observe",
  qualifies .M09 "Rep",
  qualifies .M10 "Slice0",
  qualifies .M11 "Slice1",
  qualifies .M12 "Surface0"]

private abbrev occursOnce (pieces : List Piece) (id : PieceId) : Prop :=
  (pieces.map (·.id)).count id = 1

private theorem planCoversEveryId (id : PieceId) : occursOnce plan id := by
  cases id <;> decide

-- Follow each root's own import edges in the shared environment.
private def importClosure (env : Environment) (root : Name) : NameSet := Id.run do
  let mut visited : NameSet := {}
  let mut pending := #[root]
  while !pending.isEmpty do
    let name := pending.back!
    pending := pending.pop
    if visited.contains name then continue
    let some idx := env.getModuleIdx? name | continue
    visited := visited.insert name
    for dependency in env.header.moduleData[idx.toNat]!.imports do
      pending := pending.push dependency.module
  return visited

private def evidenceInScope (specModules compilerModules : NameSet) (origin : Name) : Bool :=
  match origin.getRoot with
  | `ConcreteSpec => specModules.contains origin
  | `Concrete => compilerModules.contains origin
  | _ => false

private def origin? (env : Environment) (name : Name) : Option Name :=
  if let some i := env.getModuleIdxFor? name then
    env.header.moduleNames[i.toNat]?
  else if env.contains name then
    some env.header.mainModule
  else
    none

private def hasDocTag (tag doc : String) : Bool :=
  let doc := doc.trimAscii.toString
  doc.startsWith tag && ((doc.drop tag.length).toString.toList.head?).all
    (fun c => c.isWhitespace || ":.(".contains c)

private def isAuthored (env : Environment) (name : Name) : Bool := Id.run do
  if (privateToUserName name).isInternal then return false
  -- Do not use inherited ranges, such as the parent range of a generated recursor.
  let some ranges := declRangeExt.find? (level := .exported) env name <|>
      declRangeExt.find? (level := .server) env name | return false
  -- Derived declarations can have a command range with no distinct name selection.
  return ranges.selectionRange.pos != ranges.range.pos ||
    ranges.selectionRange.endPos != ranges.range.endPos

structure EvidenceMetadata where
  laws : NameMap Name := {}
  planted : NameMap Name := {}

private def readEvidence (env : Environment) (inScope : Name → Bool) :
    CommandElabM EvidenceMetadata := do
  let mut found : EvidenceMetadata := {}
  for (name, info) in env.constants do
    let .thmInfo _ := info | continue
    let some origin := origin? env name | continue
    if !inScope origin || !isAuthored env name then continue
    let doc := (← findDocString? env name).getD ""
    if hasDocTag "Law" doc then
      found := { found with laws := found.laws.insert name origin }
    -- Match the site's Planted doc tag and its underscore-delimited name convention.
    let .str _ last := privateToUserName name | continue
    if hasDocTag "Planted" doc || (last.splitOn "_").contains "planted" then
      found := { found with planted := found.planted.insert name origin }
  return found

structure Result where
  originalLanded : Nat := 0
  additionLanded : Nat := 0
  qualificationLanded : Nat := 0
  errors : Array String := #[]
  rows : Array String := #[]

private def EvidenceNamespace.matches (ns : EvidenceNamespace) (origin name : Name) : Bool :=
  ns.origin == origin && ns.namespaceName.isPrefixOf name

structure Registry where
  rules : List ConcreteSpec.Core.RuleId := ConcreteSpec.Core.registeredRules
  examples : List ConcreteSpec.Core.RuleExample := ConcreteSpec.Core.registeredRuleExamples

private def readRegistry (env : Environment) (specModules : NameSet) : CommandElabM Registry := do
  -- The rule gate also checks that the linked input reduces to closed concrete data.
  let inScope (name : Name) := (origin? env name).any specModules.contains
  let rules := ConcreteSpec.Core.registeredRules.filter (fun rule => inScope rule.name)
  let examples ← ConcreteSpec.Core.registeredRuleExamples.filterM fun link => do
    if !inScope link.declaration then return false
    let axioms ← collectAxioms link.declaration
    return axioms.all fun name => [`propext, `Classical.choice, `Quot.sound].contains name
  return { rules, examples }

-- Inspect the theorem independently of the registry so deleting its link cannot hide its kind.
private def exampleType? (env : Environment) (name : Name) : Option Expr := do
  let .thmInfo info ← env.find? name | none
  let wrapper ← info.type.getAppFn.constName?
  if wrapper == `ConcreteSpec.Core.RuleAccepted || wrapper == `ConcreteSpec.Core.RuleRefused then
    some info.type
  else none

private def exampleRule? (type : Expr) : Option Name := do
  let #[_, rule, _] := type.getAppArgs | none
  let .const name _ := rule | none
  some name

private def exampleWrapper (env : Environment) (link : ConcreteSpec.Core.RuleExample) : Option Name := do
  let name := link.declaration
  if origin? env name != some `ConcreteSpec.Spec.Rules || !isAuthored env name then none else
    let type ← exampleType? env name
    if exampleRule? type == some link.rule then type.getAppFn.constName? else none

private def Registry.hasExample (registry : Registry) (env : Environment)
    (rule wrapper : Name) (cited : Name → Bool) : Bool :=
  registry.examples.any fun link =>
    link.rule == rule && cited link.declaration && exampleWrapper env link == some wrapper

private def Registry.uniqueRule? (registry : Registry) (name : Name) :
    Option ConcreteSpec.Core.RuleId :=
  match registry.rules.filter (·.name == name) with
  | [entry] => some entry
  | _ => none

private def Registry.errors (registry : Registry) (env : Environment) (piece : PieceId)
    (rules : List RuleRequirement) (cited : Name → Bool := fun _ => true) : Array String := Id.run do
  let mut errors := #[]
  for rule in rules do
    match registry.uniqueRule? rule.name with
    | none =>
      errors := errors.push s!"{piece}: missing unique registered rule {rule.name}"
    | some entry =>
      unless rule.ruleOrigin != `ConcreteSpec.Spec.Rules &&
          origin? env rule.name == some rule.ruleOrigin && isAuthored env rule.name do
        errors := errors.push s!"{piece}: registered rule {rule.name} must be authored in {rule.ruleOrigin}"
      unless entry.rev >= 1 do
        errors := errors.push s!"{piece}: registered rule {rule.name} has revision below 1"
    unless registry.hasExample env rule.name `ConcreteSpec.Core.RuleAccepted cited do
      errors := errors.push s!"{piece}: missing registered acceptance example for {rule.name}"
    unless registry.hasExample env rule.name `ConcreteSpec.Core.RuleRefused cited do
      errors := errors.push s!"{piece}: missing registered refusal example for {rule.name}"
  return errors

private def EvidenceNamespace.cites (ns : EvidenceNamespace) (groups : List Group) (name : Name) : Bool :=
  groups.any fun group =>
    ns.matches group.origin name && (group.declarations ++ group.laws).contains name

private def Registry.coversReservation (registry : Registry) (env : Environment)
    (piece : PieceId) (ns : EvidenceNamespace) (ruleOrigin : Name) (groups : List Group) : Bool :=
  registry.examples.any fun link =>
    ns.cites groups link.declaration &&
      (registry.errors env piece [⟨link.rule, ruleOrigin⟩] (ns.cites groups)).isEmpty

private def Registry.citationErrors (registry : Registry) (env : Environment)
    (piece : PieceId) (required : Completion) (origin name : Name) : Array String := Id.run do
  let type := exampleType? env name
  let rule := match type with
    | some type => exampleRule? type
    | none =>
      if registry.rules.any (·.name == name) || registry.examples.any (·.rule == name) then
        some name
      else none
  let some rule := rule
    | return if type.isSome then #[s!"{piece}: {name} must cite a named rule"] else #[]
  if type.isSome && !registry.examples.any (fun link =>
      link.declaration == name && link.rule == rule && (exampleWrapper env link).isSome) then
    return #[s!"{piece}: {name} is missing its registered example link for {rule}"]
  if let some requirement := (required.localRules ++ required.completionRules).find? (·.name == rule) then
    return registry.errors env piece [requirement]
  if let some ns := required.namespaces.find? (fun ns => ns.matches origin name && ns.ruleOrigin.isSome) then
    if let some ruleOrigin := ns.ruleOrigin then
      return registry.errors env piece [⟨rule, ruleOrigin⟩]
  return #[s!"{piece}: {name} requires a registered-rule requirement for {rule}"]

private def reservationErrors (env : Environment) (pieces : List Piece)
    (requirements : PieceId → Completion := completion) : Array String := Id.run do
  let mut errors := #[]
  let mut namespaces : NameMap (List (PieceId × EvidenceNamespace × List Name)) := {}
  let mut ruleOwners : NameMap PieceId := {}
  for piece in pieces do
    let required := requirements piece.id
    for ns in required.namespaces do
      let sameModule := (namespaces.find? ns.origin).getD []
      for (owner, other, _) in sameModule do
        if ns.namespaceName.isPrefixOf other.namespaceName ||
            other.namespaceName.isPrefixOf ns.namespaceName then
          errors := errors.push s!"{piece.id}: overlapping completion reservation with {owner} under {ns.namespaceName} in {ns.origin}"
      let cited := (required.named ++ required.requiredSpec ++ piece.groups).flatMap fun group =>
        if group.origin == ns.origin then group.declarations ++ group.laws else []
      namespaces := namespaces.insert ns.origin ((piece.id, ns, cited) :: sameModule)
    for rule in required.localRules ++ required.completionRules do
      if let some owner := ruleOwners.find? rule.name then
        errors := errors.push s!"{piece.id}: overlapping registry reservation for {rule.name} with {owner}"
      ruleOwners := ruleOwners.insert rule.name piece.id
  for (name, _) in env.constants do
    let some origin := origin? env name | continue
    let some reserved := namespaces.find? origin | continue
    for (owner, ns, cited) in reserved do
      if ns.namespaceName.isPrefixOf name && !cited.contains name && isAuthored env name then
        errors := errors.push s!"{owner}: uncited declaration {name} in completion reservation"
  return errors.qsort (· < ·)

private def inventory (env : Environment) (inScope : Name → Bool) (observed : EvidenceMetadata)
    (plan : List Piece) (integrityErrors : Array String := #[])
    (requirements : PieceId → Completion := completion) (registry : Registry := {}) : Result := Id.run do
  let mut result : Result := { errors := integrityErrors }
  let mut seen : NameSet := {}
  let mut seenLaws : NameSet := {}
  let mut ids : Std.HashSet PieceId := {}
  for piece in plan do
    let mut errors := #[]
    if ids.contains piece.id then
      errors := errors.push s!"duplicate piece {piece.id}"
    ids := ids.insert piece.id
    let claimsLanding := piece.remaining.isEmpty && piece.disposition.isNone
    let required := requirements piece.id
    let groups := required.named ++ piece.groups ++
      (if claimsLanding then required.requiredSpec else [])
    if claimsLanding && groups.isEmpty then
      errors := errors.push s!"{piece.id}: a landed piece needs evidence"
    errors := errors ++ registry.errors env piece.id required.localRules
    for ns in required.namespaces do
      if ns.origin == `ConcreteSpec.Spec.Rules && ns.ruleOrigin.isNone then
        errors := errors.push s!"{piece.id}: missing rule-defining module for registry reservation under {ns.namespaceName}"
    if claimsLanding then
      errors := errors ++ registry.errors env piece.id required.completionRules
      for ns in required.namespaces do
        unless piece.groups.any (fun group =>
            (group.declarations ++ group.laws).any (ns.matches group.origin)) do
          errors := errors.push s!"{piece.id}: missing completion evidence under {ns.namespaceName} in {ns.origin}"
        if let some ruleOrigin := ns.ruleOrigin then
          if !registry.coversReservation env piece.id ns ruleOrigin piece.groups then
            errors := errors.push s!"{piece.id}: missing registered rule authored in {ruleOrigin} with cited acceptance and refusal under {ns.namespaceName} in {ns.origin}"
        if ns.isProof then
          let citesLaw (group : Group) (name : Name) :=
            observed.laws.find? name == some group.origin
          let citesReserved (names : Group → List Name) (classified : NameMap Name) :=
            piece.groups.any fun group => (names group).any fun name =>
              ns.matches group.origin name && classified.find? name == some group.origin
          let namedLaw := piece.id == .M09 &&
            required.named.any (fun group => group.laws.any (citesLaw group))
          unless namedLaw || citesReserved (·.laws) observed.laws do
            errors := errors.push s!"{piece.id}: missing reserved Law theorem under {ns.namespaceName} in {ns.origin}"
          unless citesReserved (·.declarations) observed.planted do
            errors := errors.push s!"{piece.id}: missing reserved planted control under {ns.namespaceName} in {ns.origin}"
    for group in piece.groups do
      for name in group.declarations ++ group.laws do
        unless required.namespaces.any (fun ns => ns.matches group.origin name) do
          errors := errors.push s!"{piece.id}: {name} in {group.origin} is outside its completion evidence"
    for group in groups do
      if group.declarations.isEmpty && group.laws.isEmpty then
        errors := errors.push s!"{piece.id}: empty evidence group"
      if (env.getModuleIdx? group.origin).isNone && group.origin != env.header.mainModule then
        errors := errors.push s!"{piece.id}: missing module {group.origin}"
      else if !inScope group.origin then
        errors := errors.push s!"{piece.id}: evidence module outside its root import closure: {group.origin}"
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
          else if !isAuthored env name then
            errors := errors.push s!"{piece.id}: {name} is not an authored declaration"
          else if expectsLaw && !observed.laws.contains name then
            errors := errors.push s!"{piece.id}: {name} is not an authored Law theorem"
          else
            errors := errors ++ registry.citationErrors env piece.id required group.origin name
    let landed := errors.isEmpty && claimsLanding
    let status :=
      if !errors.isEmpty then "incomplete"
      else if landed then "landed"
      else piece.disposition.getD (if groups.isEmpty then "pending" else "partial")
    let scope := if piece.scope.isEmpty then "" else s!" — {piece.scope}"
    result := {
      originalLanded := result.originalLanded +
        (if landed && scopeKind piece.id == .original then 1 else 0)
      additionLanded := result.additionLanded +
        (if landed && scopeKind piece.id == .addition then 1 else 0)
      qualificationLanded := result.qualificationLanded +
        (if landed && scopeKind piece.id == .qualification then 1 else 0)
      errors := result.errors ++ errors
      rows := (result.rows.push s!"{piece.id}: {status}{scope}") ++
        (piece.remaining.map (s!"  remaining: {·}")).toArray
    }
  let unplanned := observed.laws.toList.filterMap fun (name, origin) =>
    if seenLaws.contains name then none else some s!"unplanned Law {name} ({origin})"
  return { result with errors := result.errors ++ unplanned.toArray.qsort (· < ·) }

private def rootErrors (specModules : NameSet) (modules : Array Name) : Array String :=
  (modules.filterMap fun name =>
    if !specModules.contains name then
      some s!"spec module outside the root import closure: {name}"
    else none).qsort (· < ·)

private def checkRoot (specModules : NameSet) : IO (Array String) := do
  let files ← (System.FilePath.mk "ConcreteSpec").walkDir
  let modules := files.filterMap fun file =>
    if file.extension == some "lean" then
      some ((file.withExtension "").components.foldl Name.str .anonymous)
    else none
  return rootErrors specModules modules

namespace Control

theorem retained : True := trivial

def rulesAuthored (value : Bool) : Prop := value = true

theorem rulesAuthoredAccepted : ConcreteSpec.Core.RuleAccepted rulesAuthored true := rfl

theorem rulesAuthoredRefused : ConcreteSpec.Core.RuleRefused rulesAuthored false :=
  Bool.noConfusion

/-- Law: inventory control. -/
theorem newName (n : Nat) : n + 0 = n := Nat.add_zero n

/-- Planted: increment in place of adding zero. -/
theorem wrongIncrement : 0 + 1 ≠ (0 : Nat) := by decide

/-- Law: this tagged definition must be excluded. -/
def nonTheorem : Bool := true

/-- Planted: this tagged definition must be excluded. -/
def nonTheoremControl : Bool := false

-- Planted mutations fail the same coverage obligation as the production plan.
example : ¬ occursOnce (plan.filter (·.id != .F04p)) .F04p := by decide

private def substituted := plan.map fun piece =>
  if piece.id == .F39 then { piece with id := .F40 } else piece

example : substituted.length = plan.length ∧
    ¬ occursOnce substituted .F39 ∧ ¬ occursOnce substituted .F40 := by decide

def verify (env : Environment) (specModules compilerModules : NameSet) (specEvidence : EvidenceMetadata) (baseline : Registry) :
    CommandElabM Unit := do
  let origin := env.header.mainModule
  let inScope (name : Name) := name == origin || evidenceInScope specModules compilerModules name
  let fixtures ← readEvidence env (· == origin)
  let observed := { fixtures with laws := (fixtures.laws.erase `ConcreteSpec.Core.Observe.EnvironmentEvidence.law).erase `ConcreteSpec.Core.Observe.OutputEvidence.law }
  let claim (groups : List Group) := { present .F01 "" with groups }
  let good := claim [
    laws origin [`SpecLanding.Control.newName], decls origin [`SpecLanding.Control.wrongIncrement]]
  let check (pieces : List Piece) := (inventory env inScope observed pieces #[]
    (fun _ => { namespaces := [⟨origin, `SpecLanding.Control, true, none⟩] })).errors
  let hasError (pieces : List Piece) (message : String) : CommandElabM Unit := do
    unless (check pieces).any (·.startsWith message) do
      throwError "inventory control missed {message}"
  unless (check [good]).isEmpty do
    throwError "inventory control: updated plan did not resolve the rename"
  hasError [claim [laws origin [`SpecLanding.Control.oldName]]]
    s!"F01: missing declaration SpecLanding.Control.oldName in {origin}"
  hasError [claim [decls origin [`SpecLanding.Control.absent]]]
    s!"F01: missing declaration SpecLanding.Control.absent in {origin}"
  hasError [claim [decls origin [`SpecLanding.Control.newName]]]
    s!"unplanned Law SpecLanding.Control.newName ({origin})"
  for name in [`SpecLanding.Control.retained, `SpecLanding.Control.nonTheorem] do
    hasError [claim [laws origin [name]]]
      s!"F01: {name} is not an authored Law theorem"
  for name in [`ConcreteSpec.Var.mk.inj, `ConcreteSpec.Var.recOn, `ConcreteSpec.instDecidableEqVar] do
    hasError [claim [decls `ConcreteSpec.Spec.Basics [name]]]
      s!"F01: {name} is not an authored declaration"
  hasError [claim [decls origin []]] "F01: empty evidence group"
  hasError [claim []] "F01: a landed piece needs evidence"
  hasError [good, good] "duplicate piece F01"
  hasError [good, { good with id := .F02 }] "F02: duplicate planned declaration SpecLanding.Control.newName"
  hasError [claim [laws `ConcreteSpec.Proofs.Basics [`SpecLanding.Control.newName]]]
    s!"F01: SpecLanding.Control.newName is in {origin}, expected ConcreteSpec.Proofs.Basics"
  hasError [claim [laws `MissingModule [`SpecLanding.Control.newName]]]
    "F01: missing module MissingModule"
  let rejectsBorrowedEvidence (id : PieceId) (moduleName name : Name) : CommandElabM Unit := do
    unless origin? env name == some moduleName && isAuthored env name do
      throwError "inventory control: {name} must be authored in {moduleName}"
    let falseLanding := plan.map fun piece =>
      if piece.id == id then
        { piece with remaining := [], groups := [decls moduleName [name]] }
      else piece
    let checked := inventory env inScope specEvidence falseLanding #[] completion baseline
    unless checked.errors.contains
        s!"{id}: {name} in {moduleName} is outside its completion evidence" &&
        !checked.rows.any (·.startsWith s!"{id}: landed") do
      throwError "inventory control: borrowed evidence {name} was accepted for {id}"
  rejectsBorrowedEvidence .F39 `ConcreteSpec.Spec.Basics `ConcreteSpec.Core.AList.erase
  rejectsBorrowedEvidence .F39 `ConcreteSpec.Spec.Authority `ConcreteSpec.Authority.console
  rejectsBorrowedEvidence .F39 `ConcreteSpec.Spec.Authority `ConcreteSpec.Authority.ProgOk
  rejectsBorrowedEvidence .F17 `ConcreteSpec.Spec.Rules `ConcreteSpec.Core.Leak.Registry.accountA
  rejectsBorrowedEvidence .F03p `ConcreteSpec.Spec.Ident `ConcreteSpec.Core.ContractId
  let helper := `ConcreteSpec.Core.SourcePathSpan.inventoryHelper
  let helperOnly := plan.map fun piece =>
    if piece.id == .F04p then
      { piece with remaining := [], groups := [decls origin [helper]] }
    else piece
  -- Use F04p's actual reservation with this fixture's module as its origin.
  let withFixtureOrigin (id : PieceId) :=
    let required := completion id
    if id == .F04p then
      { required with namespaces := required.namespaces.map (fun ns => { ns with origin }) }
    else required
  let checked := inventory env inScope specEvidence helperOnly #[] withFixtureOrigin baseline
  unless checked.errors.size == 2 &&
      checked.errors.any (·.startsWith "F04p: missing reserved Law theorem") &&
      checked.errors.any (·.startsWith "F04p: missing reserved planted control") &&
      !checked.rows.any (·.startsWith "F04p: landed") do
    throwError "inventory control: helper-only F04p claim was not refused for missing Law and control"
  hasError [claim [laws origin [`SpecLanding.Control.newName]]]
    "F01: missing reserved planted control"
  hasError [claim [decls origin [`SpecLanding.Control.retained, `SpecLanding.Control.wrongIncrement]]]
    "F01: missing reserved Law theorem"
  hasError [claim [laws origin [`SpecLanding.Control.newName],
      decls origin [`SpecLanding.Control.nonTheoremControl]]]
    "F01: missing reserved planted control"
  let land (id : PieceId) (groups : List Group := []) : List Piece :=
    [{ present id "" with groups }]
  let constRule := `ConcreteSpec.Core.ConstsPermitted
  let withoutConstRule := { baseline with
    rules := baseline.rules.filter (·.name != constRule)
    examples := baseline.examples.filter (·.rule != constRule) }
  let checked := inventory env inScope {} (land .F06) #[] completion withoutConstRule
  unless checked.errors.contains s!"F06: missing unique registered rule {constRule}" &&
      checked.errors.contains s!"F06: missing registered acceptance example for {constRule}" &&
      checked.errors.contains s!"F06: missing registered refusal example for {constRule}" &&
      checked.originalLanded == 0 do
    throwError "inventory control: F06 landed after removing the rule registration and both links"
  let plainCitation (name moduleName : Name) (_ : PieceId) :=
    namedEvidence [decls moduleName [name]]
  let checked := inventory env inScope {} (land .F06) #[]
    (plainCitation constRule `ConcreteSpec.Spec.Const) baseline
  unless checked.errors == #[s!"F06: {constRule} requires a registered-rule requirement for {constRule}"] &&
      checked.originalLanded == 0 do
    throwError "inventory control: F06 accepted a rule as a plain declaration"
  for name in [`ConcreteSpec.Core.ConstRegistry.accepted, `ConcreteSpec.Core.ConstRegistry.refused] do
    let fixture := plainCitation name `ConcreteSpec.Spec.Rules
    let checked := inventory env inScope {} (land .F06) #[] fixture withoutConstRule
    unless checked.errors == #[s!"F06: {name} is missing its registered example link for {constRule}"] &&
        checked.originalLanded == 0 do
      throwError "inventory control: F06 accepted an unlinked rule theorem: {name}"
    let checked := inventory env inScope {} (land .F06) #[] fixture baseline
    unless checked.errors == #[s!"F06: {name} requires a registered-rule requirement for {constRule}"] &&
        checked.originalLanded == 0 do
      throwError "inventory control: F06 accepted a rule theorem as a plain declaration: {name}"
  let localSourcePath (id : PieceId) :=
    let required := completion id
    if id == .F03p then { required with completionRules := [] } else required
  let sourcePathClaim := land .F03p
  let checked := inventory env inScope {} sourcePathClaim #[] localSourcePath baseline
  unless checked.errors.isEmpty && checked.originalLanded == 1 do
    throwError "inventory control: local F03p evidence was refused: {checked.errors}"
  for requirement in (completion .F03p).localRules do
    let rule := requirement.name
    let withoutRule := { baseline with rules := baseline.rules.filter (·.name != rule) }
    let checked := inventory env inScope {} sourcePathClaim #[] localSourcePath withoutRule
    unless checked.errors.contains s!"F03p: missing unique registered rule {rule}" &&
        checked.originalLanded == 0 do
      throwError "inventory control: F03p landed without registry entry for {rule}"
    for (wrapper, verdict) in [(`ConcreteSpec.Core.RuleAccepted, "acceptance"),
        (`ConcreteSpec.Core.RuleRefused, "refusal")] do
      let withoutExample := { baseline with examples := baseline.examples.filter fun link =>
        !(link.rule == rule && exampleWrapper env link == some wrapper) }
      let checked := inventory env inScope {} sourcePathClaim #[] localSourcePath withoutExample
      unless checked.errors.contains s!"F03p: missing registered {verdict} example for {rule}" &&
          checked.originalLanded == 0 do
        throwError "inventory control: F03p landed without registered {verdict} for {rule}"
  let misplaced := `ConcreteSpec.Core.RuleAccepted
  let wrongModule := { baseline with rules := ⟨misplaced, 1⟩ :: baseline.rules }
  unless (wrongModule.errors env .F03p [⟨misplaced, `ConcreteSpec.Spec.Ident⟩]).contains
      s!"F03p: registered rule {misplaced} must be authored in ConcreteSpec.Spec.Ident" do
    throwError "inventory control: accepted a rule defined in Spec.Rules"
  let rule := `ConcreteSpec.Core.SourcePath.Nonempty
  let zeroRevision := { baseline with rules := baseline.rules.map fun entry =>
    if entry.name == rule then { entry with rev := 0 } else entry }
  let checked := inventory env inScope {} sourcePathClaim #[] localSourcePath zeroRevision
  unless checked.errors == #[s!"F03p: registered rule {rule} has revision below 1"] &&
      checked.originalLanded == 0 do
    throwError "inventory control: F03p landed with revision zero"
  let localOnly := { baseline with rules := baseline.rules.filter (fun rule =>
    !(completion .F03p).completionRules.any (·.name == rule.name)) }
  let checked := inventory env inScope {} sourcePathClaim #[] completion localOnly
  unless checked.originalLanded == 0 && checked.errors.any (·.startsWith "F03p: missing unique registered rule ConcreteSpec.Core.SourcePath.Project.") do
    throwError "inventory control: full F03p landed with local rules alone"
  let missingSpan (id : PieceId) :=
    let required := localSourcePath id
    { required with named := required.named.map fun group =>
      { group with declarations := group.declarations.map fun name =>
        if name == `ConcreteSpec.Core.Span then `ConcreteSpec.Core.missingSpan else name } }
  let checked := inventory env inScope {} sourcePathClaim #[] missingSpan baseline
  unless checked.errors.contains
      "F03p: missing declaration ConcreteSpec.Core.missingSpan in ConcreteSpec.Spec.Ident" &&
      checked.originalLanded == 0 do
    throwError "inventory control: F03p landed without its named spec declarations"
  let outputFixture (id : PieceId) :=
    let required := completion id
    { required with namespaces := required.namespaces.map fun ns => { ns with origin } }
  let environment := [
    laws origin [`ConcreteSpec.Core.Observe.EnvironmentEvidence.law],
    decls origin [`ConcreteSpec.Core.Observe.EnvironmentEvidence.planted]]
  let outputHelper := decls origin [`ConcreteSpec.Core.Observe.OutputEvidence.helper]
  let output := [
    laws origin [`ConcreteSpec.Core.Observe.OutputEvidence.law],
    decls origin [`ConcreteSpec.Core.Observe.OutputEvidence.planted]]
  let outputEvidence := { fixtures with laws := fixtures.laws.erase `SpecLanding.Control.newName }
  let checked := inventory env inScope outputEvidence (land .F21 (environment ++ output)) #[] outputFixture
  unless checked.errors.isEmpty && checked.originalLanded == 1 do
    throwError "inventory control: complete F21 proof reservations were refused: {checked.errors}"
  let checked := inventory env inScope outputEvidence (land .F21 (environment ++ [outputHelper])) #[] outputFixture
  unless checked.originalLanded == 0 &&
      checked.errors.any (·.startsWith "F21: missing reserved Law theorem under ConcreteSpec.Core.Observe.OutputEvidence") &&
      checked.errors.any (·.startsWith "F21: missing reserved planted control under ConcreteSpec.Core.Observe.OutputEvidence") do
    throwError "inventory control: F21 landed with only an output helper"
  let qualificationFixture (id : PieceId) :=
    let required := completion id
    if id == .M09 then
      { required with namespaces := required.namespaces.map fun ns =>
        { ns with origin, namespaceName := `SpecLanding.Control } }
    else required
  let repLaw := `ConcreteSpec.Rep.rep_roundtrip
  let some repOrigin := specEvidence.laws.find? repLaw
    | throwError "inventory control: M09's named Law is missing"
  let qualifiedEvidence := { observed with laws := ({} : NameMap Name).insert repLaw repOrigin }
  let checked := inventory env inScope qualifiedEvidence
    (land .M09 [decls origin [`SpecLanding.Control.wrongIncrement]]) #[] qualificationFixture
  unless checked.errors.isEmpty && checked.qualificationLanded == 1 do
    throwError "inventory control: M09 with an existing Law and reserved control did not land: {checked.errors}"
  let checked := inventory env inScope qualifiedEvidence
    (land .M09 [decls origin [`SpecLanding.Control.retained]]) #[] qualificationFixture
  unless checked.errors.size == 1 &&
      checked.errors.any (·.startsWith "M09: missing reserved planted control") &&
      checked.qualificationLanded == 0 do
    throwError "inventory control: M09 without a reserved planted control was accepted: {checked.errors}"
  let registryOnly := { present .F17 "" with groups := [decls origin [`SpecLanding.Control.retained]] }
  let needsSpecAndRegistry (id : PieceId) :=
    { completion id with namespaces := ⟨origin, `SpecLanding.Control, false, none⟩ :: (completion id).namespaces }
  let incomplete := inventory env inScope {} [registryOnly] #[] needsSpecAndRegistry
  unless incomplete.errors.contains
      "F17: missing completion evidence under ConcreteSpec.Core.Grants in ConcreteSpec.Spec.Grant" do
    throwError "inventory control: incomplete namespace coverage was accepted"
  -- Use live accounting examples to exercise the registry reservation without inventing grant rules.
  let registryFixture (ruleOrigin : Name) (_ : PieceId) := reservedEvidence [
    registryNamespace `Core.Leak.Registry ruleOrigin]
  let registryGroup := evidence `ConcreteSpec.Spec.Rules `ConcreteSpec.Core.Leak.Registry
  let checkRegistry (ruleOrigin : Name) (names : List Name) (registry : Registry := baseline) :=
    inventory env inScope {} (land .F17 [registryGroup names]) #[] (registryFixture ruleOrigin) registry
  let missingPair (ruleOrigin : Name) :=
    s!"F17: missing registered rule authored in {ruleOrigin} with cited acceptance and refusal under ConcreteSpec.Core.Leak.Registry in ConcreteSpec.Spec.Rules"
  let pair := [`accountA, `wfAccepted, `wfRefused]
  let checked := checkRegistry `Spec.Accounting pair
  unless checked.errors.isEmpty && checked.originalLanded == 1 do
    throwError "inventory control: cited registry example pair was refused: {checked.errors}"
  let checked := checkRegistry `Spec.Grant pair
  unless checked.errors.contains (missingPair `ConcreteSpec.Spec.Grant) && checked.originalLanded == 0 do
    throwError "inventory control: accepted cited accounting examples for a grant rule: {checked.errors}"
  for names in [[`accountA], [`wfAccepted, `resetRefused], [`wfAccepted], [`wfRefused]] do
    let checked := checkRegistry `Spec.Accounting names
    unless checked.errors.contains (missingPair `ConcreteSpec.Spec.Accounting) &&
        checked.originalLanded == 0 do
      throwError "inventory control: accepted a registry reservation without a cited example pair: {names}"
  let rule := `ConcreteSpec.Core.Leak.Account.WF
  let zeroRevision := { baseline with rules := baseline.rules.map fun entry =>
    if entry.name == rule then { entry with rev := 0 } else entry }
  let duplicate := { baseline with rules := ⟨rule, 1⟩ :: baseline.rules }
  for (label, registry) in [("revision zero", zeroRevision), ("duplicate registration", duplicate)] do
    let checked := checkRegistry `Spec.Accounting pair registry
    unless checked.errors.contains (missingPair `ConcreteSpec.Spec.Accounting) &&
        checked.originalLanded == 0 do
      throwError "inventory control: accepted a registry reservation with {label}"
  -- Give the authored fixture and its valid example pair the Rules module as their origin.
  let rulesModule := `ConcreteSpec.Spec.Rules
  let rulesEnv := env.setMainModule rulesModule
  let rule := `SpecLanding.Control.rulesAuthored
  let accepted := `SpecLanding.Control.rulesAuthoredAccepted
  let refused := `SpecLanding.Control.rulesAuthoredRefused
  let registry : Registry := { rules := [⟨rule, 1⟩], examples := [⟨rule, accepted⟩, ⟨rule, refused⟩] }
  let ns : EvidenceNamespace := ⟨rulesModule, `SpecLanding.Control, false, some rulesModule⟩
  unless origin? rulesEnv rule == some rulesModule && isAuthored rulesEnv rule &&
      registry.hasExample rulesEnv rule `ConcreteSpec.Core.RuleAccepted (· == accepted) &&
      registry.hasExample rulesEnv rule `ConcreteSpec.Core.RuleRefused (· == refused) do
    throwError "inventory control: Rules-authored reservation fixture failed to resolve"
  let checked := inventory rulesEnv inScope {} (land .F17 [decls rulesModule [accepted, refused]]) #[]
    (fun _ => reservedEvidence [ns]) registry
  unless checked.errors.any (·.startsWith "F17: missing registered rule authored in ConcreteSpec.Spec.Rules") &&
      checked.originalLanded == 0 do
    throwError "inventory control: accepted a registry reservation for a Rules-authored rule"
  let reservedFixture (_ : PieceId) : Completion :=
    { namespaces := [⟨origin, `ConcreteSpec.Core.Observe.OutputEvidence, true, none⟩] }
  let checked := reservationErrors env [present .F21 "", present .F22 ""] reservedFixture
  unless checked.any (·.startsWith "F22: overlapping completion reservation with F21") &&
      checked.contains "F21: uncited declaration ConcreteSpec.Core.Observe.OutputEvidence.helper in completion reservation" do
    throwError "inventory control: overlapping reservation or pre-existing helper was accepted"
  let orphan := rootErrors specModules #[`ConcreteSpec.Spec.Basics, `ConcreteSpec.PlantedOrphan]
  let checked := inventory env inScope {} [] orphan
  unless checked.errors == #["spec module outside the root import closure: ConcreteSpec.PlantedOrphan"] do
    throwError "inventory control: root integrity depends on the plan rows"
  -- A reduced spec root leaves Scalar reachable through the compiler alone.
  let reducedSpec := importClosure env `ConcreteSpec.Spec.Basics
  let compilerOnly := `ConcreteSpec.Spec.Scalar
  unless compilerModules.contains compilerOnly &&
      rootErrors reducedSpec #[compilerOnly] ==
        #[s!"spec module outside the root import closure: {compilerOnly}"] do
    throwError "inventory control: compiler imports masked a spec module outside the spec root"
  let checked := inventory env (evidenceInScope reducedSpec compilerModules) {} (land .F06) #[]
    (plainCitation `ConcreteSpec.Core.IntTy compilerOnly)
  unless checked.errors == #[s!"F06: evidence module outside its root import closure: {compilerOnly}"] &&
      checked.originalLanded == 0 do
    throwError "inventory control: accepted spec evidence reachable only through the compiler"
  IO.println "inventory controls passed"

end Control

#eval show CommandElabM Unit from do
  let env ← getEnv
  let specModules := importClosure env `ConcreteSpec
  let compilerModules := importClosure env `Concrete
  let inSpec (origin : Name) := origin.getRoot == `ConcreteSpec && specModules.contains origin
  let observed ← readEvidence env inSpec
  let registry ← readRegistry env specModules
  let result := inventory env (evidenceInScope specModules compilerModules) observed plan
    ((← checkRoot specModules) ++ reservationErrors env plan)
    completion registry
  for row in result.rows ++ result.errors do
    IO.println row
  if !result.errors.isEmpty then throwError "spec landing inventory failed"
  Control.verify env specModules compilerModules observed registry
  let (qualifications, spec) := plan.partition (fun piece => scopeKind piece.id == .qualification)
  let (original, additions) := spec.partition (fun piece => scopeKind piece.id == .original)
  let dispositionCount := (spec.filter (·.disposition.isSome)).length
  let landed := result.originalLanded + result.additionLanded
  IO.println s!"spec landing inventory: {landed}/{spec.length} spec pieces landed"
  IO.println s!"  original plan: {result.originalLanded}/{original.length} landed; recorded additions: {result.additionLanded}/{additions.length} landed"
  IO.println s!"  active spec scope: {landed}/{spec.length - dispositionCount} landed ({dispositionCount} historical rows outside active scope)"
  IO.println s!"theorem qualification: {result.qualificationLanded}/{qualifications.length} rows landed"

end SpecLanding
