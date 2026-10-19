#!/usr/bin/env bash
# Bug-to-regression corpus audit.
# Checks that every numbered bug in docs/bugs/ has regression test coverage.
#
# Each bug must either:
#   1. Have an entry in BUG_TEST_MAP below (bug number -> test file name)
#   2. Be listed in SKIP_BUGS with a justification
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
  [091]="error_trait_impl_caps_exceed_generic.con error_trait_impl_caps_exceed_direct.con error_trait_bound_call_missing_cap.con error_trait_impl_sig_mismatch.con error_trait_method_generic.con trait_impl_caps_declared.con"
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
  [087]="regress_087_let_block_shadow.con regress_087_borrow_shadow_uaf/src/main.con"
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
  [090]="error_090_guard_moves_linear.con error_090_guard_moves_binder.con error_090_guard_defer_moves.con error_090_guard_move_then_reassign.con regress_090_guard_reads.con"
  # Bug 121: enum LLVM types disagreed with tySize/tyAlign, so an enum field moved the
  # fields after it on aggregate copies (conlog SIGSEGV). Class leg: check_enum_union_layout.sh leg 5.
  [121]="regress_bug121_enum_field_layout.con"
)

# Bugs that don't need a .con regression test (with reason).
declare -A SKIP_BUGS=(
  [006]="string literal collision -- fixed in EmitSSA, covered by cross-module tests"
  [009]="const decls not lowered -- fixed in Lower.lean, no standalone repro needed"
  [010]="missing string_substr -- stdlib addition"
  [015]="O0 default perf -- compiler flag default change, not a .con test"
  [017]="socket constants linux-only -- platform-specific stdlib"
  [024]="recursive-struct infinite size -- covered by check_error_leaks.sh gate corpus"
  [025]="no-main linker leak -- covered by check_error_leaks.sh (no_main, empty_file cases)"
  [026]="huge array-repeat count hang -- covered by check_error_leaks.sh (huge_array case)"
  [027]="EmitSSA O(n^2) rendering -- OPEN perf item; no .con regression (codegen perf, not correctness)"
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
  [102]="OPEN -- &mut x as *mut *mut T on a pointer-typed local passes x's value, not its address (silent wrong code; the interpreter refuses the cast). Found by the RocksDB slice 1 groundwork. Regression vehicle lands with the fix under queue exit 0b: the docs/bugs/102 program as a permanent fixture, plus interpreter support for the cast so differential gates cover it"
  [103]="OPEN -- Mono drop glue synthesizes an empty definition for any unresolved callee ending in _destroy, so an extern fn foo_destroy never reaches the C library (silent leak). Found by the RocksDB slice 1 groundwork. Regression vehicle lands with the fix under queue exit 0b: the docs/bugs/103 program checked by --emit-llvm (extern stays a declare) and a linked C stub counting the call; check_rocksdb_slice1.sh also fails on it"
  [104]="OPEN -- a reference assigned from an inner block into an outer reference variable records no borrow (Check .assign never sets borrowedFrom), so the source is freed or leaves scope while the reference is read: use after free in safe code. Found by the V1 design examples (B6). Reproducers in docs/bugs/104 (heap: exits 0 instead of 77; stack: exits 30, interpreter refuses); they become permanent rejection fixtures with the fix under queue exit 0b. The bug 092 fix covers aggregates only; batch 2 (land2-fix, bug 091 normalized place rule) rejects both reproducers with E0238"
  [108]="OPEN -- a bool cast to a wider integer is emitted as trunc i1; reproducer tests/regressions/bug108_bool_cast_trunc/repro.con, found by the examples-health pass 2026-10-20; reproduction pinned by check_example_outputs.sh (open-bug reproducers leg)"
  [109]="OPEN -- an f32 literal that is not exactly representable is emitted as a decimal LLVM float constant; reproducer tests/regressions/bug109_f32_literal_invalid_ir/repro.con, found by the examples-health pass 2026-10-20; reproduction pinned by check_example_outputs.sh (open-bug reproducers leg)"
  [110]="OPEN -- an array value is stored as an aggregate where the SSA value is a pointer; reproducer tests/regressions/bug110_array_value_store/repro.con, found by the examples-health pass 2026-10-20; reproduction pinned by check_example_outputs.sh (open-bug reproducers leg)"
  [111]="OPEN -- main may declare parameters, and the wrapper calls it with none; reproducer tests/regressions/bug111_main_params_accepted/repro.con, found by the examples-health pass 2026-10-20; reproduction pinned by check_example_outputs.sh (open-bug reproducers leg)"
  [112]="OPEN -- the declaration std.mem uses for sizeof is rejected (E0116 at 0:0) outside std; reproducer tests/regressions/bug112_user_intrinsic_e0116/repro.con, found by the examples-health pass 2026-10-20; reproduction pinned by check_example_outputs.sh (open-bug reproducers leg)"
  [113]="OPEN -- R-0484 R3 (every foreign binding declares its effects) is not enforced in nested submodules; reproducer tests/regressions/bug113_submodule_extern_effects_unchecked/repro.con, found by the examples-health pass 2026-10-20; reproduction pinned by check_example_outputs.sh (open-bug reproducers leg)"
  [114]="OPEN -- an intrinsic declared in a nested submodule and called by path lowers to a C call; reproducer tests/regressions/bug114_submodule_intrinsic_links_c_symbol/repro.con, found by the examples-health pass 2026-10-20; reproduction pinned by check_example_outputs.sh (open-bug reproducers leg)"
  [115]="OPEN -- the builtin vec_get returns a non-Copy element by value, duplicating a linear value; reproducer tests/regressions/bug115_vec_get_duplicates_linear/repro.con, found by the examples-health pass 2026-10-20; reproduction pinned by check_example_outputs.sh (open-bug reproducers leg)"
  [116]="OPEN -- vec_push on the first field of a struct reached through a &mut parameter calls an undefined @vec_push; reproducer tests/regressions/bug116_vec_push_first_field_through_ref/repro.con, found by the examples-health pass 2026-10-20; reproduction pinned by check_example_outputs.sh (open-bug reproducers leg)"
  [117]="OPEN -- the interpreter does not resolve module-level constants; reproducer tests/regressions/bug117_interp_module_const/repro.con, found by the examples-health pass 2026-10-20; reproduction pinned by check_example_outputs.sh (open-bug reproducers leg)"
  [118]="OPEN -- the interpreter cannot call a function imported from a nested module; reproducer tests/regressions/bug118_interp_nested_module_import/repro.con, found by the examples-health pass 2026-10-20; reproduction pinned by check_example_outputs.sh (open-bug reproducers leg)"
  [119]="OPEN -- the interpreter cannot call a trait method on a type parameter; reproducer tests/regressions/bug119_interp_generic_trait_method/repro.con, found by the examples-health pass 2026-10-20; reproduction pinned by check_example_outputs.sh (open-bug reproducers leg)"
  [120]="OPEN -- the interpreter cannot borrow a field through a reference parameter; reproducer tests/regressions/bug120_interp_field_borrow_through_ref/repro.con, found by the examples-health pass 2026-10-20; reproduction pinned by check_example_outputs.sh (open-bug reproducers leg)"
  [122]="OPEN -- the builtin Option/Result are emitted as one canonical LLVM union sized for the program's largest instantiation while the compiler lays out each instantiation (and a user enum named Option/Result) by its own size, so aggregate copies move or overrun fields (wrong code; a stack overflow in tests/programs/match_expr_linear.con); reproducer tests/regressions/bug122_builtin_enum_canonical_layout/repro.con, found by bug 121's layout probe 2026-10-20; reproduction pinned by check_example_outputs.sh (open-bug reproducers leg, runs-exit) and check_enum_union_layout.sh leg 5 (KNOWN_122)"
  [123]="OPEN -- a module constant of bare reference type is accepted (tyHoldsRefInAggregateV looks through the outer reference) and &X lowers to an uninitialized stack slot (addrOfLocal), so get(R) exits 0 where 42 is read; reproducer tests/regressions/bug123_const_borrow_uninitialized_slot/repro.con, found by the exit 2i specification (G1) 2026-10-21; reproduction pinned by check_example_outputs.sh (open-bug reproducers leg, wrong-exit:42); rejection fixture error_2i_ref3_const_bare_ref.con lands with the fix under queue exit 0b"
  [124]="OPEN -- duplicate parameter names pass checkFn and fail in llvm-as (redefinition of argument) while --interp runs the program; reproducer tests/regressions/bug124_duplicate_parameter_names/repro.con, found by the exit 2i specification (G3) 2026-10-21; reproduction pinned by check_example_outputs.sh (open-bug reproducers leg); the rejection fixture lands with SCOPE-1 (E0301, R-0500) under queue exit 2c"
  [127]="OPEN -- struct-literal initializers are evaluated in declaration order by the binary (Lower's .structLit reorders fields to the definition before lowering each initializer) and in the ratified source order by --interp, so P { b: tick(5), a: tick(6) } exits 65 natively and prints 56 interpreted; reproducer tests/regressions/bug127_struct_literal_field_order/repro.con, found by the semantics probes (N1) 2026-10-21; reproduction pinned by check_example_outputs.sh (open-bug reproducers leg, runs-exit:65); the run fixture and a native leg for check_evidence_build.sh's order gate land with the fix under queue exit 0b"
  [128]="OPEN -- the interpreter's evalCast treats an integer-to-float cast as the identity (wrapToType at f64 returns the integer), so (2^53+1 as f64) as Int is 2^53+1 under --interp and 2^53 natively; reproducer tests/regressions/bug128_interp_int_to_float_identity/repro.con, found by the semantics probes (N2) 2026-10-21; reproduction pinned by check_example_outputs.sh (open-bug reproducers leg, interp-prints:3); the differential fixture lands with the fix under queue exit 0b (owner R-0493)"
  [129]="OPEN -- user functions are emitted under their bare source names (EmitSSA llvmFnName), so a safe fn malloc becomes the program's malloc and alloc calls it; the binary is killed by a signal (SIGABRT, status 134 on arm64 macOS); reproducer tests/regressions/bug129_user_fn_captures_libc_symbol/repro.con, found by the semantics probes (N3) 2026-10-21; reproduction pinned by check_example_outputs.sh (open-bug reproducers leg, signals); the fixture lands with the fix (symbol mangling, R-0492) or its containment under queue exit 0b"
  [130]="OPEN -- CoreCheck admits <int> as bool and the backend emits bitcast i64 to i1, which llvm-as rejects, while --interp fails (if condition is not a boolean); reproducer tests/regressions/bug130_int_as_bool_invalid_ir/repro.con, found by the semantics probes (N4) 2026-10-21; reproduction pinned by check_example_outputs.sh (open-bug reproducers leg, compile-fails); the unlanded backend-108-116 refuses it with E0553 and the rejection fixture lands with it under queue exit 2c"
  [131]="OPEN -- a reference parameter is a mutable binding and r = &local in an inner block records no provenance, so the binary reads a dead slot while --interp fails (ref path base 'local' not in env); the parameter variant of 104; reproducer tests/regressions/bug131_reference_parameter_repointed/repro.con, found by the semantics probes 2026-10-21; reproduction pinned by check_example_outputs.sh (open-bug reproducers leg, interp-fails); land2-fix refuses it with E0238 and the rejection fixture lands with batch 2 under queue exit 0b"
  [107]="OPEN -- a reference to a shadowed binding reads the shadowing binding's storage in the binary (let x = 1; let r = &x; let x = 2; *r: interp 1, compiled 2): a shadowing let keeps its Core name and Lower keys slots by name. Found by the batch 4 Codex review. Same Elab/Lower defect as 087 (batch 2's bindLocal repair on land2-fix should close it); regression vehicle (binary vs --interp on both reproducers in docs/bugs/107) lands with that repair under queue exit 0b"
  [088]="FIXED 2026-10-18 -- an unhinted integer literal defaulted to Int with NO range obligation (let x = 18446744073709551615: interp 2^64-1, binary -1), a literal at a type variable was never range-checked after Mono (let y: T = 300 at u8: interp 300, binary 44), and the SSA folder compared unnormalized constants (0xFFFFFFFFFFFFFFFF as i64 < 0: interp true, binary false). Regression vehicles: regress_088_const_fold_casts.con (run_ok 1048575; old compiler 1048574) + error_088_unhinted_literal_range.con (run_err) + check_cast_matrix.sh constant-fold and both-paths-reject sections; see docs/bugs/088"
  [081]="FIXED 2026-10-18 -- the proof model calls by table KEY but edges were justified by table containment, so an import alias (check_version as check_class) borrowed validate_header_correct and minted. Regression vehicle is a gate: check_subject_binding.sh legs X refuse and do not mint tests/programs/bug081_alias_key (and its colliding-definition variant) while the unaliased elf_header claims stay proved and minted; see docs/bugs/081"
  [083]="FIXED 2026-10-18 -- resolveSpec fell back to Proof.provedFunctions, a table keyed by qualified NAME with v1 body-string freshness, so a program with no proof attributes defining main.compute_tag (u8, aborting at runtime) was reported proved (origin hardcoded). The table is deleted; every program it served links in source with a v2 pin. Regression fixture tests/programs/regress_083_no_claim_by_name.con (run_tests.sh: 0 proved, 3 unproved, no hardcoded origin); see docs/bugs/083"
  [089]="FIXED 2026-10-18 -- an #[ensures] read proved_by_lean on contracts/vcs/obligation-ledger/audit whenever a link named a theorem (even No.Such.Theorem, or only #[proof_coverage(iff)]), check-proofs printed a check mark for any theorem that type-checked, and a rejected ensures theorem exited 1 under 0 failed while its receipt minted. One verdict (ensuresVerdictOf) decides every surface; nothing discharges a postcondition until Exact property acceptance. Regression vehicle is a gate: check_contract_negatives.sh section bug 089 runs tests/regressions/bug089_ensures_proof (four fixtures x every surface, check-proofs summaries/exit, receipts); see docs/bugs/089"
  [082]="FIXED 2026-10-18 -- an import alias reached Core as the alias spelling and every backend resolved it program-wide, so import a.{check_version as check_class} ran another module's check_class (compiled AND interp); also an unaliased import of a name two top-level modules define, and a project submodule's std import hijacked by a sibling submodule's same-named fn. Elab now spells such calls by their definition (importCallSpellingsFor), and one name has one binding per module (E0299). Regression: tests/codegen/import_alias_binding.con (interp==compiled differential + pinned 77324 in check_codegen_execution.sh; previous compiler 12814) and tests/programs/error_082_import_alias_shadows_local.con / error_082_import_duplicate_binding.con / error_082_import_alias_shadows_method.con / error_082_import_spelling_collision.con (run_err E0299), plus the project test regress_082_project_alias_binding/src/main.con (sibling alias = bug 055, and std.math.abs not hijacked by a sibling submodule's abs); see docs/bugs/082"
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

echo
echo "=== Results ==="
echo "  pass:    $PASS"
echo "  fail:    $FAIL"
echo "  skip:    $SKIP"

if [[ "$FAIL" -gt 0 ]]; then
  echo
  echo "FAILED: $FAIL bug(s) missing regression coverage."
  echo "Fix: add entry to BUG_TEST_MAP or SKIP_BUGS in this script."
  exit 1
fi

echo
echo "All numbered bugs have regression coverage."
