import ConcreteSpec
import Lean

open Lean Elab Command

namespace SpecLanding

structure Group where
  origin : Name
  declarations : List Name := []
  laws : List Name := []

structure Piece where
  id : String
  scope : String
  groups : List Group
  remaining : List String := []

private def present (id scope : String) (groups : List Group) : Piece :=
  ⟨id, scope, groups, []⟩

private def pending (id scope : String) : Piece :=
  ⟨id, scope, [], [scope]⟩

private def decls (origin : Name) (names : List Name) : Group :=
  { origin, declarations := names }

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
    decls `ConcreteSpec.Spec.Ident [`ConcreteSpec.Core.PlaceId, `ConcreteSpec.Core.PrimId, `ConcreteSpec.Core.ModuleId, `ConcreteSpec.Core.SourceText, `ConcreteSpec.Core.SourceText.lineAt, `ConcreteSpec.Core.SourceText.columnAt],
    decls `ConcreteSpec.Spec.Target [`ConcreteSpec.Core.Target, `ConcreteSpec.Core.Target.WellFormed],
    decls `ConcreteSpec.Spec.Outcome [`ConcreteSpec.Core.Res, `ConcreteSpec.Core.TrapKind]],
  present "F03b" "Nominal source bytes and byte-list round trip" [
    decls `ConcreteSpec.Spec.Ident [`ConcreteSpec.Core.SourceText.ofBytes, `ConcreteSpec.Core.SourceText.toBytes],
    decls `ConcreteSpec.Proofs.Ident [`ConcreteSpec.Core.SourceText.take_toBytes]],
  pending "F03p-design" "Record the authorized project-relative path and dependent Span design",
  pending "F03p" "Adapt SourcePath, Span, and their named examples to the authorized policy",
  present "F04" "Stable-key, namespace, obligation, contract, and byte-coordinate proofs" [
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
      `ConcreteSpec.Core.KeyConstruct.collectKeysIn_complete._unary,
      `ConcreteSpec.Core.KeyConstruct.collectKeysIn_sound,
      `ConcreteSpec.Core.KeyConstruct.collectKeysIn_sound._unary,
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
      `ConcreteSpec.Core.SourceText.columnAt_zero,
      `ConcreteSpec.Core.SourceText.lineAt_append,
      `ConcreteSpec.Core.SourceText.lineAt_append_newline,
      `ConcreteSpec.Core.SourceText.lineAt_append_other,
      `ConcreteSpec.Core.SourceText.lineAt_zero,
      `ConcreteSpec.Core.SpecializationTable.canonicalize_complete,
      `ConcreteSpec.Core.SpecializationTable.canonicalize_distinct,
      `ConcreteSpec.Core.SpecializationTable.canonicalize_edge_sound,
      `ConcreteSpec.Core.SpecializationTable.canonicalize_nodup,
      `ConcreteSpec.Core.SpecializationTable.canonicalize_share]],
  pending "F04p" "Path and Span laws and controls",
  present "R" "Rule registry interface and named acceptance and refusal claims" [
    decls `ConcreteSpec.Spec.Rules [`ConcreteSpec.Core.registeredRules,
      `ConcreteSpec.Core.registeredRuleExamples, `ConcreteSpec.Core.RuleAccepted,
      `ConcreteSpec.Core.RuleRefused]],
  present "F05" "Fixed-width arithmetic and exact-term correspondence" [
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
    decls `ConcreteSpec.Spec.Const [`ConcreteSpec.Core.CExpr, `ConcreteSpec.Core.CExpr.eval, `ConcreteSpec.Core.ConstsPermitted],
    decls `ConcreteSpec.Spec.Rules [`ConcreteSpec.Core.ConstRegistry.accepted, `ConcreteSpec.Core.ConstRegistry.refused]],
  present "F07" "Constant typing, evaluation, dependency order, and cycle laws" [
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
    decls `ConcreteSpec.Spec.Exec [`ConcreteSpec.Core.Exec.Operands, `ConcreteSpec.Core.Exec.Config, `ConcreteSpec.Core.Exec.step, `ConcreteSpec.Core.Exec.runFrom, `ConcreteSpec.Core.Exec.Report]],
  present "F09a" "Elementary execution and structured exits" [
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
    laws `ConcreteSpec.Proofs.Exec [
      `ConcreteSpec.Core.Exec.rules_complete,
      `ConcreteSpec.Core.Exec.rules_deterministic,
      `ConcreteSpec.Core.Exec.rules_silent_deterministic,
      `ConcreteSpec.Core.Exec.rules_sound]],
  present "F10" "Lifecycle refusals and destruction ordering" [
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
    decls `ConcreteSpec.Spec.Own [`ConcreteSpec.Core.Own.State, `ConcreteSpec.Core.Own.WellFormed, `ConcreteSpec.Core.Own.OwnerPermitted],
    decls `ConcreteSpec.Spec.Rules [`ConcreteSpec.Core.Own.Registry.ownerAccepted, `ConcreteSpec.Core.Own.Registry.endRefused]],
  present "F14" "Ownership preservation, activation and local lifetime proofs" [
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
  pending "F16" "Downward protocol, deferred lifetime, move lifecycle, and loan-history evidence",
  present "CAP" "Shared capability type and enumeration" [
    decls `ConcreteSpec.Spec.CapabilityBase [`ConcreteSpec.Authority.Cap, `ConcreteSpec.Authority.Cap.all]],
  pending "F17" "Grant model and proofs over the shared capability base",
  pending "F18" "Primitive contracts, ghost execution, and repaired obligation search",
  pending "F19" "Integrated observation model, including the ISS-0048 repair",
  pending "F20" "Observe2 evidence for the admitted policy, with loader character controls tracked in F20p",
  pending "F20p" "Loader character controls and their supporting declarations",
  pending "F21" "Observation environment and output proofs",
  pending "F22" "Observation source, evidence, and transformation proofs",
  pending "F23" "Observation report proofs",
  pending "F24a" "Adequacy model and finite-run soundness, with query and trace controls",
  pending "F24b" "Fuel extension and infinite-execution uniqueness with controls",
  pending "F25a" "Event-prefix movement and finite-or-infinite internal events",
  pending "F25b" "Remaining adequacy declarations and controls for group A4",
  pending "F26" "Remaining non-wiping leakage and release model, proofs, and controls",
  present "F26a" "Memory accounting, trajectory peaks, and signed net intervals" [
    decls `ConcreteSpec.Spec.Accounting [`ConcreteSpec.Core.Leak.Account, `ConcreteSpec.Core.Leak.run, `ConcreteSpec.Core.Leak.LineageReport.Sound],
    decls `ConcreteSpec.Spec.Rules [`ConcreteSpec.Core.Leak.Registry.sequenceAccepted, `ConcreteSpec.Core.Leak.Registry.sequenceRefused],
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
  pending "F27" "Wiping declarations and controls under an authorized policy",
  pending "F28" "Frame and footprint model with explicit launch state",
  pending "F29" "Frame proofs and controls for the admitted policy",
  pending "F29p" "End-to-end source-policy controls and support",
  pending "F30" "Scalar bridge and progress preparation for the compiler",
  pending "F31" "Atomic scalar migration through slices, representations, and compiler consumers",
  pending "F32" "Integrated checker proofs and controls",
  pending "F33" "Checked representation views and proofs",
  pending "F34" "Adapted slice proofs and typed slice/Core correspondence",
  pending "F35" "Atomic execution and observation switch with proof and consumer migrations",
  pending "F36" "Checked borrow views and affected proofs and consumers",
  pending "F37" "Checked brand views and adapted proofs",
  pending "F38" "Checked cleanup dependency analysis and proof migration",
  pending "F39-design" "Record the design of bounded Authority views",
  pending "F39" "Checked authority grant and execution views with full proofs",
  pending "F40" "Cross-fragment examples and root reconciliation; retire obsolete bridges",
  pending "M01" "Qualify the existing Basics theorem family with classifications and evidence",
  pending "M02" "Qualify the existing Authority theorem family with classifications and evidence",
  pending "M03" "Qualify the existing Borrow theorem family with classifications and evidence",
  pending "M04" "Qualify the existing Brand theorem family with classifications and evidence",
  pending "M05" "Qualify the existing Checker theorem family with classifications and evidence",
  pending "M06" "Qualify the existing Cleanup theorem family with classifications and evidence",
  pending "M07" "Qualify the existing Examples theorem family with classifications and evidence",
  pending "M08" "Qualify the existing Observe theorem family with classifications and evidence",
  pending "M09" "Qualify the existing Rep theorem family with classifications and evidence",
  pending "M10" "Qualify the existing Slice0 theorem family with classifications and evidence",
  pending "M11" "Qualify the existing Slice1 theorem family with classifications and evidence",
  pending "M12" "Qualify the existing Surface0 theorem family with classifications and evidence"]


private def origin? (env : Environment) (name : Name) : Option Name :=
  if let some i := env.getModuleIdxFor? name then
    env.header.moduleNames[i.toNat]?
  else if env.contains name then
    some env.header.mainModule
  else
    none

private def readLaws (env : Environment) (inScope : Name → Bool) :
    IO (NameMap Name) := do
  let mut found := {}
  let modules := env.header.moduleNames
  for (name, _) in env.constants do
    let origin := match env.getModuleIdxFor? name with
      | some idx => modules[idx.toNat]!
      | none => env.header.mainModule
    if !inScope origin then continue
    if let some doc ← findDocString? env name then
      if doc == "Law" || doc.startsWith "Law " then
        found := found.insert name origin
  return found

structure Result where
  landed : Nat := 0
  errors : Array String := #[]
  rows : Array String := #[]
  unplanned : Array String := #[]

private def inventory (env : Environment) (observedLaws : NameMap Name)
    (plan : List Piece) (pieceErrors : String → Array String := fun _ => #[]) : Result := Id.run do
  let mut result : Result := {}
  let mut seen : NameSet := {}
  let mut ids : Std.HashSet String := {}
  for piece in plan do
    let mut errors := pieceErrors piece.id
    if ids.contains piece.id then
      errors := errors.push s!"duplicate piece {piece.id}"
    ids := ids.insert piece.id
    if piece.groups.isEmpty && piece.remaining.isEmpty then
      errors := errors.push s!"{piece.id}: a landed piece needs evidence"
    for group in piece.groups do
      if (env.getModuleIdx? group.origin).isNone && group.origin != env.header.mainModule then
        errors := errors.push s!"{piece.id}: missing module {group.origin}"
      for name in group.declarations ++ group.laws do
        if seen.contains name then
          errors := errors.push s!"{piece.id}: duplicate planned declaration {name}"
        seen := seen.insert name
        match origin? env name with
        | none => errors := errors.push s!"{piece.id}: missing declaration {name} in {group.origin}"
        | some actual =>
          if actual != group.origin then
            errors := errors.push s!"{piece.id}: {name} is in {actual}, expected {group.origin}"
          else if group.laws.contains name && !observedLaws.contains name then
            errors := errors.push s!"{piece.id}: {name} lost its Law tag"
    let landed := errors.isEmpty && piece.remaining.isEmpty
    let status := if landed then "landed" else if !errors.isEmpty then "incomplete" else "pending"
    let scope := if piece.groups.isEmpty then "" else s!" — {piece.scope}"
    result := {
      result with
      landed := result.landed + (if landed then 1 else 0)
      errors := result.errors ++ errors
      rows := (result.rows.push s!"{piece.id}: {status}{scope}") ++
        (piece.remaining.map (s!"  remaining: {·}")).toArray
    }
  for (name, origin) in observedLaws.toList do
    if !seen.contains name then
      result := { result with unplanned := result.unplanned.push s!"unplanned Law {name} ({origin})" }
  return result

-- F01 brought the proof root and Basics import into shape. Include that
-- property in its status instead of measuring import commands as source units.
private def checkRoot (env : Environment) : IO (Array String) := do
  let mut missing := #[]
  for file in ← (System.FilePath.mk "ConcreteSpec").walkDir do
    if file.extension != some "lean" then continue
    let moduleName := (file.withExtension "").components.foldl Name.str .anonymous
    if (env.getModuleIdx? moduleName).isNone then
      missing := missing.push s!"F01: spec module outside the root import closure: {moduleName}"
  if let some i := env.getModuleIdx? `ConcreteSpec.Proofs.Basics then
    let imports := env.header.moduleData[i.toNat]!.imports.map (·.module)
    if imports.filter (fun m => m.getRoot == `ConcreteSpec) != #[`ConcreteSpec.Spec.Basics] then
      missing := missing.push "F01: Proofs.Basics must import Spec.Basics as its sole spec dependency"
  return missing.qsort (· < ·)

namespace Control

theorem retained : True := trivial

/-- Law (inventory control). -/
theorem newName : True := trivial

def verify (env : Environment) : IO Unit := do
  let origin := env.header.mainModule
  let observed ← readLaws env (· == origin)
  let check := fun group => inventory env observed [present "test" "planted control" [group]]
  let extra := check (decls origin [`SpecLanding.Control.retained])
  unless extra.errors.isEmpty && extra.landed == 1 &&
      extra.unplanned == #[s!"unplanned Law SpecLanding.Control.newName ({origin})"] do
    throw <| IO.userError "inventory control: extra elaborated Law went unreported"
  let missing := check (decls origin [`SpecLanding.Control.absent])
  unless missing.landed == 0 && missing.errors ==
      #[s!"test: missing declaration SpecLanding.Control.absent in {origin}"] do
    throw <| IO.userError "inventory control: missing declaration was accepted"
  let stale := check (laws origin [`SpecLanding.Control.oldName])
  let updated := check (laws origin [`SpecLanding.Control.newName])
  unless stale.landed == 0 && stale.errors ==
      #[s!"test: missing declaration SpecLanding.Control.oldName in {origin}"] &&
      updated.errors.isEmpty && updated.landed == 1 && updated.unplanned.isEmpty do
    throw <| IO.userError "inventory control: updating the plan did not resolve the rename"
  IO.println "inventory controls: missing declaration, unplanned Law, and rename passed"

end Control

#eval show CommandElabM Unit from do
  let env ← getEnv
  Control.verify env
  let observed ← readLaws env (fun origin => origin.getRoot == `ConcreteSpec)
  let rootErrors ← checkRoot env
  let result := inventory env observed plan (fun id => if id == "F01" then rootErrors else #[])
  for row in result.rows ++ result.unplanned ++ result.errors do
    IO.println row
  if !result.errors.isEmpty then throwError "spec landing inventory failed"
  IO.println s!"spec landing inventory: {result.landed}/{plan.length} pieces landed"

end SpecLanding
