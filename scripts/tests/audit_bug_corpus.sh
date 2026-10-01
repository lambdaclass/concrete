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
  # R-0003: one probe/occupancy slice — overwrite-past-tombstone (047) and the
  # zero-empty-slot missing lookup (048) share the same fixture.
  [047]="regress_047_048_hashmap_probe/src/main.con"
  [048]="regress_047_048_hashmap_probe/src/main.con"
  [051]="regress_generic_enum_051.con adversarial_mono_generic_enum.con"
  # Bug 057: a by-value HashMap must arrive whole (hash_fn for get, cap for drop).
  [057]="regress_057_hashmap_by_value/src/main.con"
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
  [052]="OPEN -- T_destroy no-op for arrays; Vec<[T;N]>.drop() skips element destruction (arrdrop cell-count repro)"
  [053]="FIXED (R-0005) -- the unary trap inventory moved into IntArith (evalIntUnaryOp / unaryOpCanTrap) so DCE stops deciding locally whether -x can trap; gated by check_trap_inventory.sh (12 checks, both paths at i8/i16/i32/Int) with mutations #25-#27"
  [054]="HALF CLOSED -- the collision now fails closed with E0809 (a specialization whose mangled name is declared, or two instantiations mangling to one name); gated in check_mono_name_collision.sh. Still OPEN: the mangling is forgeable, so a legitimate program spelling Box_Int is refused rather than compiled, and fn symbols are uncovered (R-0007)"
  [067]="FIXED 2026-08-05 — a capped obligation escaped forbid-assume when Rocq/Isabelle are present: the cap is display-only, so an obligation resting on an UNSOUND HYPOTHESIS passes the release gate. Asserted by check_known_wrong_corpus.sh, which fails in CI (and only there — no external provers locally). Authority leak. FIX: the capped list keys on hypDebt (kernel-independent) instead of status, so promotion to proved_by_multi_kernel can no longer erase the cap. Gated by check_known_wrong_corpus.sh, which only fires in CI where Rocq/Isabelle exist; see docs/bugs/067"
  [068]="FIXED 2026-08-06 — a ghost let and a runtime let produced IDENTICAL body evidence, so erasure was invisible to the subject digest and a proof over one stayed valid-looking after a change to the other. Found while scoping proof-context elaboration, not by a gate. FIX: EvidenceStmtV2.letBind carries isGhost, on the same principle as exprStmt's isValue. Gated by check_shadow_body_v2.sh; see docs/bugs/068"
  [066]="FIXED 2026-08-04 -- the interpreter discarded a match guard's environment, so a guard's output and mutations vanished while its VALUE still selected the arm; return values agreed, so only an OUTPUT differential caught it. Gated by check_match_guard_effects.sh (12 checks: succeeding guard, failing guard's effects surviving the fall-through, two failing guards in order, a guard whose pattern never matches, outer-mutation persistence, arm-local mutation NOT leaking, binder-frame restoration with a rebinding control). Mutation-verified on the EFFECTS axis (threading the env only on success fails 3 legs). The binding-LEAK axis is prevented upstream, so its mutation legitimately survives rather than indicating missing coverage: code owner Elab.bindArmVar alpha-renames every match payload binder to a fresh Core name (x -> x.b0, verified with --emit-core), gated by regress_045_match_binder_shadow.con (run_ok 42) in run_tests.sh. If that owner or gate changes, the leak mutation becomes killable and check_match_guard_effects.sh's restoration leg starts doing real work -- a 045/066 coupling, also recorded in the bug doc"
  [070]="FIXED 2026-08-02 (renumbered from 065) -- builtinEnumList built twice AND hasUserResult computed from different inputs (Elab consults imports, Check does not), so the passes hold different beliefs about a user Result; code divergence measured, no wrong-behaviour witness yet; gated by check_builtin_enum_owner.sh (four Result arrival paths: absent, local, imported-unaliased, imported-aliased)"
  [069]="FIXED 2026-08-02 (renumbered from 064) -- aliased imported structs/enums are registered under the importer's local spelling; positive alias fixture compiles and the unaliased E0298 privacy control proves resolution was preserved; gated by check_type_identity.sh via tests/programs/bug_064_aliased_imported_type.con"
  [055]="OPEN -- project sibling import alias emits undefined callee (one044); fully-qualified form works"
  [056]="FIXED (R-0436) -- a function reference became SVal.fnRef and a call target became SCallee.direct/.indirect, retiring both string encodings; gated by check_fnptr_values.sh (17 checks: rebinding across if/loop, phi mixing loaded register with known global, devirtualization preserved, verifier still refusing undefined phi operands AND undefined indirect targets) with mutations #28-#30"
  [058]="OPEN -- #[proof_by] with no #[proof_fingerprint] compares the current fingerprint with ITSELF, so an edited body still reports proved (R-0004 defect 1; reproducer in the bug doc, control case with a fingerprint stales correctly)"
  [059]="FIXED (R-0004 V2 activation 2026-08-17) -- the authoritative subject is proofSubjectDigestV2, which binds CheckedDeclFacts (full typed signature), so i32 -> u32 now stales the claim; LOAD-BEARING regression = check_proof_freshness.sh '059 CLOSED' leg (whole-signature change must report stale)"
  [060]="FIXED (R-0004 V2 activation 2026-08-17) -- contracts are inside the v2 subject, so attaching or changing an #[ensures] moves the digest and the claim no longer reports proved; LOAD-BEARING regression = check_proof_freshness.sh '060 CLOSED' leg (a false postcondition must not report proved)"
  [061]="FIXED (R-0442) -- PExpr gained .applyVar and FnTable gained a callables namespace, so a parameter application and a definition call are different nodes resolved in different namespaces; gated by check_proofcore_callable_identity.sh (29 checks) with mutations #31-#33"
  [071]="FIXED 2026-09-21 (sinks+Alloc) / 2026-09-25 (Unsafe) / 2026-09-25 (prelude receivers) -- a capability declared in a DEPENDENCY did not bind on its caller: a function declaring nothing called std.env.get and the program printed \$HOME. ALL FOUR CALL FORMS now bind, and they are enumerated because a headline that did not name them is exactly how the last one stayed open: imported free function, explicitly imported receiver method (RawCursor::read_u8), associated call (TcpStream::connect), and PRELUDE receiver method (String::drop). The prelude case survived two rounds because importedFnCaps was built by walking m.imports and String needs no import; check_cap_sibling_module.sh asserted the acceptance as a KNOWN HOLE (a passing check) while check_cross_package_caps.sh passed claiming methods bind -- two green gates, incompatible headlines. Public std Unsafe fell 154->21 via per-method trusted. Gated by check_cross_package_caps.sh (18/0) and check_cap_sibling_module.sh (17/0, now a rejection test with a positive control); see docs/bugs/071"
  [063]="FIXED both sites 2026-09-19/2026-09-20 (R-0470) -- cap-variable inference recorded \"unknown argument type\" as \"no capabilities\", so a fn pointer read from a struct field, call result or array element could not reach a cap C parameter; misreported as E0220 against a with() the program never wrote. The authority check already PASSED on the fabricated empty capset and only expectTy stopped it, so relaxing fn-type comparison to subsetting would have turned a rejected-valid-program into an authority hole. FIX: both inference sites yield Option CapSet and contribute nothing when the type is unknown, letting resolveCaps name the variable. TWO SITES -- Check.lean (function calls) and CheckHelpers.lean (methods); the first fix took only one, and std's cap-polymorphic combinators (Set::fold, for_each, with_value) are METHODS, so real callers reach the one that was missed. Gated by check_cap_inference.sh (10/0), which asserts E0242 and the ABSENCE of E0220 on each path -- a gate that only checked \"does it fail\" would pass either way; see docs/bugs/063"
  [064]="OPEN -- a bare semantic --query KIND is rejected as unknown while the message lists that same kind as known (arity is wrong, not spelling). CLI/report diagnostic, not a .con defect: the regression vehicle is the query leg of a gate script (every knownQueryKinds/knownFactKinds member in both bare and argument form, asserting the message never lists the rejected token), not a program fixture. No gate yet"
  [065]="OPEN -- --report stack-depth drops recursive callees from each caller's call graph, so a bounded caller of unbounded code reports a finite stack and the summary prints it as the program maximum. NOT report-only: that max is greped by check_policy.sh (max_stack_bytes) and check_assumptions.sh (stack_max_bytes) and published as evidence.max_stack_bytes by capture_release_bundle.sh, and the -z fail-closed branch cannot fire because a finite max is always printed. Not live only because all five budgeted projects (crypto_verify, parse_validate, fixed_capacity, hmac_sha256, constant_time_tag) are recursion-free. Regression vehicle is check_stack_depth.sh (does not exist yet) plus a recursive budgeted negative project the policy gate must REFUSE, not a tests/programs .con"
  [072]="FIXED 2026-10-01 -- std.process.spawn's child called exit(127) when exec failed, flushing stdio buffers it had copied from the parent, so pending buffered output was written twice (measured: XX for one byte). Now _exit. Regression vehicle is a std-using package, not a tests/programs .con: tests/regressions/spawn_exit/exec_failure_no_double_flush, run by check_spawn_exit.sh, which asserts the child exited 127 and the file holds exactly X; shown to fail on XX with the fix reverted; see docs/bugs/072"
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
