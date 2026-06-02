# Testing

Status: stable reference

This document describes the test architecture, coverage matrix, determinism policy, and execution model for Concrete.

## Test Architecture

The test system has four layers, ordered by cost:

| Layer | Tool | Cost | What it catches |
|-------|------|------|-----------------|
| **Pass-level** | `Concrete/PipelineTest.lean` (Lean executable) | <1s, no I/O | Parse errors, type errors, elaboration bugs, monomorphization bugs, SSA verify/cleanup invariants, emit correctness |
| **Artifact** | `run_tests.sh --report`, `--codegen` | ~1s each, no clang | Report output regressions, SSA structure, LLVM IR shape, codegen differentials |
| **End-to-end** | `run_tests.sh` positive/negative | ~0.5s each, needs clang | Full compile-and-run behavior, runtime correctness |
| **Stress/integration** | `run_tests.sh` integration tests | ~1s each, needs clang | Multi-feature interactions, deep call chains, realistic programs |

### Pass-Level Lean Tests

`Concrete/PipelineTest.lean` exercises individual compiler passes directly on in-memory source strings. No subprocess, no clang, no file I/O.

Current coverage (32 tests):
- **Parse (4)**: valid programs parse, malformed input rejected
- **Frontend (8)**: parse→check→elaborate on structs/enums/traits/generics, type errors and undefined vars rejected
- **Monomorphize (2)**: generic and trait programs monomorphize
- **SSA Lowering (2)**: `lowerModule` produces functions with blocks
- **SSA Verify (3)**: `ssaVerifyProgram` accepts valid SSA from simple/enum/generic programs
- **SSA Cleanup (2)**: `ssaCleanupProgram` runs without crash, double-cleanup is idempotent
- **SSA Emit (2)**: `emitSSAProgram` produces LLVM IR, test mode works
- **Full pipeline (5)**: source → LLVM IR for 5 program shapes
- **Layout/ABI (4)**: scalar sizes/alignments, builtin type sizes, repr(C) struct layout with field offsets, pass-by-pointer decisions

Run: `lake build pipeline-test && .lake/build/bin/pipeline-test`

### Artifact Tests

Report and codegen tests consume cached compiler output without invoking clang.

- **Report tests (~70 assertions)**: content checks across all 8 `--report` modes (caps, unsafe, layout, interface, mono, alloc, authority, proof)
- **Codegen differential tests**: SSA optimization verification (constant folding, strength reduction), codegen structure (GEP offsets, enum tags, aggregate promotion), cross-representation consistency (packed struct, enum payload, Core→SSA agreement)

Compiler output is cached by `(file, flags)` key. Multi-assertion report tests reuse a single compilation (26/57 cache hits per fast run).

### End-to-End Tests

Compile-and-run tests in `lean_tests/`:
- **Positive (~230)**: compile, run, check exit code matches expected value
- **Negative (~180)**: compile, expect specific error message in stderr
- **Abort (1)**: compile, run, expect crash
- **Test flag (4)**: `--test` mode with pass/fail/mixed/submodule programs
- **O2 (~90)**: same programs compiled with `-O2`, check same results
- **Cross-target (~25)**: IR verified to compile for x86_64 via `clang --target`
- **Performance (1)**: regression check against saved baseline (>20% warning)

### Stdlib Tests

238 `#[test]` functions across all stdlib modules, compiled through the real compiler path. Module-targeted testing via `--stdlib-module <name>`.

15 collection modules verified for test presence and correctness.

### Integration / Stress Tests

Named real-program corpus (13 tests):
- `integration_text_processing.con` — string/parsing pipeline
- `integration_data_structures.con` — struct/enum data flow
- `integration_error_handling.con` — Result/Option error chains
- `integration_collection_pipeline.con` — multi-collection pipeline with Vec, generics, enums, allocation patterns
- `integration_generic_pipeline.con` (~150 lines) — 5-layer borrow chain, trait dispatch, complex enum matching
- `integration_state_machine.con` (~170 lines) — 4-state × 5-command nested match, struct construction in match arms
- `integration_compiler_stress.con` (~200 lines) — deep generic instantiation, multi-trait dispatch, nested enum matching
- `integration_multi_module.con` + `helper.con` — cross-module types, traits, and enum matching
- `integration_recursive_structures.con` (~200 lines) — recursive expression evaluation, stack-based computation
- `integration_multi_file_calculator.con` (~200 lines) — 3-module RPN evaluator with trait dispatch
- `integration_type_registry.con` (~248 lines) — 3-module catalog with validation/metrics
- `integration_pipeline_processor.con` (~223 lines) — 4-module data transformation
- `integration_stress_workload.con` (~280 lines) — 4-module bytecode interpreter with 11-variant enum

## Coverage Matrix

### By failure mode

| Failure mode | Test layer | Test count | Key tests |
|-------------|-----------|-----------|-----------|
| Parser crash/hang | Fuzz | 500 iter | `test_parser_fuzz.sh` |
| Parse rejection | Pass-level + E2E | 4 + ~20 | `parse/*`, `error_resolve_*` |
| Name resolution | Pass-level + E2E | 2 + ~10 | `frontend/*`, `error_resolve_*`, `module_*` |
| Type/capability errors | Pass-level + E2E | 8 + ~60 | `frontend/*`, `error_type_*`, `error_cap_*`, `error_borrow_*` |
| Linearity violations | E2E | ~15 | `error_unconsumed*`, `error_use_after_*`, `error_linear_*`, `linear_*` |
| Elaboration/trait bugs | Pass-level + E2E | 2 + ~20 | `mono/*`, `trait_*`, `generic_*`, `error_trait_*` |
| Lowering invariants | Pass-level + E2E | 5 + ~30 | `lower/*`, `verify/*`, `struct_*`, `enum_*`, `regress_*` |
| SSA verification | Pass-level | 3 | `verify/valid-*` |
| SSA cleanup | Pass-level | 2 | `cleanup/*` |
| Codegen structure | Artifact | ~16 | `codegen_*`, struct/enum GEP/tag checks |
| LLVM emission | Pass-level + Artifact | 2 + ~10 | `emit/*`, cross-representation checks |
| Runtime behavior | E2E | ~560 | All positive run_ok tests |
| -O2 regressions | E2E | ~90 | All computation, struct/enum, linearity, borrow, generic, trait, string, heap, vec, complex, integration, phase3 programs |
| Report accuracy | Artifact | ~90 | `report_integration.con`, `report_*_check.con`, `test_proof_*.con`, phase3 consistency cross-checks |
| ABI / FFI | E2E | 9 | `test_repr_c_*`, `test_fn_ptr_*`, `test_sizeof_*`, `test_ptr_round_trip`, `test_array_bounds`, `phase3_abi_interop` |
| Layout/ABI | Pass-level | 4 | Scalar sizes, builtin sizes, repr(C), pass-by-ptr |
| Stdlib correctness | Stdlib | 238 | All `#[test]` functions |
| Collection integrity | Stdlib | 15 modules | Collection verification section |
| Multi-module | E2E | 22 | `module_*`, `summary_*`, `module_file/` |
| Formatter | Property | 4 | `fmt_parse_roundtrip.con`, golden tests |

### By compiler pass

| Pass | Direct pass-level tests | Owned E2E tests | Total coverage |
|------|------------------------|----------------|---------------|
| `parse` | 4 | ~20 | Strong |
| `resolve` | (via frontend) | ~12 | Moderate |
| `check` | (via frontend) | ~60 | Strong |
| `elab` | (via frontend) | ~20 | Moderate |
| `core_check` | (via frontend) | ~10 | Moderate |
| `mono` | 2 | ~15 | Moderate |
| `lower` | 2 | ~30 | Strong |
| `ssa_verify` | 3 | ~5 | Moderate |
| `ssa_cleanup` | 2 | ~5 | Moderate |
| `emit_ssa` | 2 | ~180 | Strong |
| `layout` | 4 | ~10 | Strong |
| `report` | — | 44 | Strong |
| `format` | — | 4 | Light |

## Dependency-Aware Test Selection

### `--affected` mode

`run_tests.sh --affected` detects changed files via `git diff` and selects only the test sections that exercise those compiler passes.

```bash
./run_tests.sh --affected                     # auto-detect from git diff
./run_tests.sh --affected Concrete/Lower.lean  # explicit file list
```

The mapping from compiler source files to test sections lives in `test_dep_map.toml`. The mapping is conservative — when in doubt, more sections run.

Example mappings:
- `Concrete/Check.lean` → positive, negative, passlevel (type/cap/linearity tests)
- `Concrete/Lower.lean` → positive, codegen, O2, passlevel (lowering + backend tests)
- `Concrete/Report.lean` → report (report output tests only)
- `std/src/*` → stdlib, collection (stdlib tests only)
- Unknown files → full suite (safe fallback)

### Explanation

After an `--affected` run, the summary shows which files triggered which sections:

```
=== Affected mode ===
  changed files: Concrete/Lower.lean,Concrete/SSACleanup.lean
  sections: codegen,O2,passlevel,positive
```

## Structured Test Metadata

### Manifest

`test_manifest.toml` is reference metadata for test cases. It is **not** consumed by `run_tests.sh` directly — test execution is driven by the shell script's section structure. The manifest captures the metadata that shell sections encode implicitly, making it queryable and auditable for documentation and future tooling. Each test entry includes:

- `file` — path relative to repo root
- `category` — semantic category (`unit`, `semantic`, `lowering`, `codegen`, `report`, `integration`, `stress`, `stdlib`, `regression`, `property`, `fuzz`)
- `kind` — execution kind (`run_ok`, `run_err`, `run_abort`, `run_test`, `check_report`, `check_codegen`, `check_O2`, `lean_pass`)
- `passes` — which compiler passes this test exercises
- `profile` — run profile (`fast`, `slow`, `network`)
- `expected` — expected output
- `owner_pass` — primary compiler pass whose correctness this test defends
- `needs_clang` — whether clang is needed
- `multi_module` — whether the test uses `mod X;` file imports

### Dependency Map

`test_dep_map.toml` maps compiler source files to affected test sections and categories. It is parsed by `run_tests.sh --affected` to select which test sections to run based on changed files.

## Determinism and Flakiness Policy

### Rules

1. **Fixed seeds**: all randomized tests use fixed seeds unless deliberately exploring. `test_parser_fuzz.sh` uses `$RANDOM` seeded from iteration count, not wall-clock time.

2. **No wall-clock dependence**: no test depends on absolute time, execution speed, or timing. `std.time` tests check only that monotonic clock moves forward, not that specific durations elapse.

3. **Timeout classes**: tests have three implicit timeout tiers:
   - Fast tests: 10s (most E2E tests)
   - Slow tests: 30s (compilation + complex runtime)
   - Network tests: 60s (TCP round-trip with fork/accept)

4. **Network isolation by default**: `--fast` mode (the default) skips all network tests. Network tests run only under `--full`. The `SKIP_FLAKY_TCP_TEST=1` environment variable provides an additional escape hatch.

5. **Stable temp directory handling**: test artifacts use `$TMPDIR` or `/tmp`, never the working directory. Compiler output cache uses a per-run temp directory cleaned up on exit.

6. **Parallel safety**: all tests are independent. No test reads another test's output or depends on execution order. The test runner uses job-based parallelism (`-j N`) without shared mutable state between test processes.

7. **Quarantine/repair expectations**: if a test becomes flaky:
   - Identify the non-determinism source (timing, file system, network, uninitialized memory)
   - Fix the root cause or move the test to `--full` only
   - Do not delete or skip flaky tests without a tracking comment
   - The `SKIP_FLAKY_TCP_TEST` pattern is the model for temporary quarantine

### Known flakiness risks

- **TCP round-trip test**: depends on `fork()` + local socket bind. Can fail under port contention or slow CI. Quarantined behind `--full` and `SKIP_FLAKY_TCP_TEST`.
- **Parser fuzz**: timeout-based crash detection could theoretically race on very slow machines. The 5s timeout is generous for the parser's workload.

## Compile-Time and Suite-Time Baselines

### Current baselines (as of Phase D hardening)

| Metric | Value |
|--------|-------|
| Pass-level tests | <1s (32 tests, no I/O) |
| Fast suite (`--fast`) | ~25-35s (~1427 tests, parallel) |
| Full suite (`--full`) | ~40-50s (~2596 tests, 1 skipped, includes network, cross-target, perf, consistency, policy) |
| Cache hit rate | 26/57 compilations saved per fast run |
| Compiler build | ~30-45s (`lake build`) |
| lli-accelerated suite | ~12s (when `LLI_PATH` is set) |

### Tracking

Suite time is not yet automatically tracked between runs. The baselines above are manual snapshots. Future work: record timing per section in the summary output and warn on regressions beyond a threshold.

## Failure Isolation

### Artifact preservation

Failed tests automatically save artifacts to `.test-failures/`:
- Timestamped output (stdout + stderr)
- Exact rerun command
- Compiler flags used

Example:
```
$ cat .test-failures/lean_tests_struct_basic_con
# Failure: lean_tests/struct_basic.con
# Time: 2026-03-13 14:22:01
# Rerun: .lake/build/bin/concrete lean_tests/struct_basic.con -o /tmp/test_rerun && /tmp/test_rerun
<compiler output>
```

### Dependency gates

Report test groups use `compile_gate()` to skip downstream assertions when compilation fails. This prevents cascading false failures and makes the first error obvious.

## Execution Modes

| Mode | Sections | Use case |
|------|----------|----------|
| `--fast` (default) | all except network | Daily driver |
| `--full` | everything | Pre-merge |
| `--affected` | auto-detected | After specific compiler changes |
| `--affected FILE` | mapped sections | Targeted rerun |
| `--filter PAT` | all, filtered by path | Iterate on one area |
| `--stdlib` | stdlib + collection | After stdlib changes |
| `--stdlib-module M` | one stdlib module | Iterate on one module |
| `--O2` | O2 regressions | After lowering changes |
| `--codegen` | codegen + O2 | After backend changes |
| `--report` | report | After report changes |
| `--manifest` | none (list only) | List all tests with categories |

## Recommended Verification Flow

### Daily driver (default)

```bash
lake build && ./run_tests.sh
```

Runs `--fast` mode: parallel on all cores, network tests skipped.

### After changing a specific compiler pass

```bash
lake build && ./run_tests.sh --affected
```

Auto-detects changed files and runs only affected test sections.

### Pre-merge (full coverage)

```bash
lake build && ./run_tests.sh --full
```

### Targeted workflows

```bash
./run_tests.sh --filter struct_loop   # iterate on one area
./run_tests.sh --stdlib               # after touching std/src/
./run_tests.sh --stdlib-module map    # iterate on one stdlib module
./run_tests.sh --O2                   # after lowering changes
./run_tests.sh --codegen              # after backend changes
./run_tests.sh --report               # after report changes
./run_tests.sh -j 1                   # debug ordering issues
```

### Other suites

```bash
bash test_ssa.sh                      # SSA-specific backend coverage
bash test_parser_fuzz.sh              # parser crash/hang fuzzing
bash test_fuzz.sh                     # structured fuzz: parser + typecheck + valid (1500+ programs)
bash test_fuzz.sh --valid 1000        # valid program generation only, custom iteration count
bash test_perf.sh --save              # save performance baseline
bash test_perf.sh --compare           # compare against baseline (requires prior --save)
bash test_mutation.sh --list          # list 18 mutations without running
bash test_mutation.sh                 # run all mutations (~15 min, rebuilds compiler per mutation)
bash test_mutation.sh --mutation 5    # run a single mutation
```

## Testing Phases

The test suite has evolved in three phases with distinct goals:

### Phase 1: Feature and Bug Regression Tests (complete)

Goal: one test per feature, one test per bug. Catch regressions in individual language features and compiler passes.

Covers: type system, codegen, capabilities, trusted boundaries, linearity, modules, stdlib, golden tests, pass-level Lean tests. Phase-end snapshot: ~686 tests.

### Phase 2: Edge Cases and Contract-Boundary Tests (complete)

Goal: push individual features toward their edges and test cross-boundary contracts.

Covers: ABI/FFI runtime tests (repr(C), fn pointers, sizeof, arrays), proof boundary assertions (`--report proof` with exact eligibility marking and exclusion reasons), optimization-sensitive codegen tests with O2 variants, cross-module resolution edge cases, parser/type-system edge cases. Phase-end snapshot: ~766 tests.

### Phase 3: System-Level Validation and Regression Discipline (complete)

Goal: prove the compiler holds up under composed, realistic pressure — not just isolated feature tests.

#### 3.1 Large mixed-feature programs (done)

6 programs in the 200-340 line range, each exercising 4+ features:
- `phase3_expression_evaluator.con` — enums (expression tree), match, modules, traits, function pointers
- `phase3_task_scheduler.con` — enums (task state), structs, Vec, while loops, match, capabilities, defer
- `phase3_data_pipeline.con` — modules, generics with trait bounds, function pointers, Vec, linearity
- `phase3_type_checker.con` — enums (types/expressions), match, structs, Vec, modules
- `phase3_state_machine.con` — enums (5 states, 5 events), structs, nested match, modules, trusted
- `phase3_report_consistency.con` — pure functions, capabilities, trusted, repr(C), generics, allocations

All return 42 and are registered as `run_ok` + O2 differential tests.

#### 3.2 Differential testing (done)

~75 O2 differential tests covering all major program categories: computation, struct/enum, linearity, borrows, generics, traits, result/option, string, break/defer, heap, vec, complex programs, integration programs, bug regressions, hardening, and phase3 programs. Each test compiles with `-O2` and verifies identical exit code to `-O0`.

#### 3.3 Property / fuzz testing (done)

`test_fuzz.sh` — three modes, 500 iterations each (1500 total):
- **Parser fuzz**: random tokens, structured programs, deep nesting, many params, long expressions — must not crash
- **Typechecker stress**: type mismatches, undefined variables, missing returns — must not crash
- **Valid program generation**: arithmetic, conditional, loop, struct programs with computed expected values — must compile and produce correct output

#### 3.4 Artifact / report consistency (done)

20 cross-check assertions in `run_tests.sh` verifying consistency across report modes:
- Proof-eligible functions have no capabilities in caps report
- Trusted functions appear in unsafe report AND excluded from proof
- Functions with capabilities appear in authority report, confirmed by caps report
- Generic functions appear in mono report with specializations
- Allocating functions appear in alloc report with correct patterns
- repr(C) structs appear in layout report with correct sizes
- Proof eligible count matches number of pure functions

#### 3.5 Cross-target / ABI validation (done)

`phase3_abi_interop.con` + `phase3_abi_interop.c` — verifies sizeof/offsetof agreement and by-value struct passing between Concrete and C for repr(C) structs (Point, Rect, Aligned64). Linked and run as a custom test in `run_tests.sh`.

Cross-target IR verification: 25 representative programs compiled to LLVM IR, then verified to compile for x86_64 via `clang -S --target=x86_64-unknown-linux-gnu`. Runs in `--full` mode.

By-value struct FFI: small repr(C) structs (≤ 16 bytes) are flattened to integer registers per the ARM64 C ABI. Point (8 bytes) → one i64, Rect (16 bytes) → two i64s.

#### 3.6 Performance regression gates (done)

`test_perf.sh` — measures compile time, runtime (avg of 3), IR line count, and binary size for 7 representative programs. Supports `--save` (save baseline to `.perf-baseline`) and `--compare` (compare against baseline, warn on >20% regression). Requires a `.perf-baseline` file created by `--save`; `--compare` without a baseline is a no-op. Integrated into `run_tests.sh --full` as an informational section (warnings printed but do not fail the suite).

`test_mutation.sh` — mutation testing for the compiler. Applies targeted source mutations one at a time, rebuilds the compiler (`lake build`), and runs the test suite (`run_tests.sh --fast`). If all tests pass after a mutation, the mutation *survived* — indicating a test gap. If any test fails, the mutation was *killed* — tests caught the change. Use `--list` to see all mutations without running, `--mutation N` to run one.

18 mutations across 7 compiler files:
- **Layout.lean** (6): tySize i32 4→8, tyAlign i32 4→1, unit size 0→4, string size 24→16, isPassByPtr string→false, isFFISafe rejects integers
- **Shared.lean** (2): isNumeric rejects floats, isInteger excludes i32
- **Check.lean** (3): disable use-after-move, disable loop-depth linearity, disable scope-exit linearity
- **CoreCheck.lean** (3): disable match exhaustiveness, disable capability check, allow break outside loop
- **Lower.lean** (1): arrayIndex GEP uses wrong type
- **EmitSSA.lean** (1): isReprCStruct always false
- **SSAVerify.lean** (2): disable aggregate phi check, disable phi predecessor check

Each mutation takes 30-60s (rebuild + test). Full run: ~15 minutes. Not part of `run_tests.sh` — run separately as a CI-occasional job.

#### 3.7 Failure-quality testing (done)

5 diagnostic quality tests:
- `phase3_diag_multi_error.con` — 3 independent type errors, all reported (bounded error recovery)
- `phase3_diag_specific_location.con` — error in deeply nested code, correct line reported
- `phase3_diag_no_cascade.con` — single undeclared variable produces <5 errors
- `phase3_diag_hint_quality.con` — capability error includes "hint:" text
- `phase3_diag_type_mismatch.con` — error includes "expected" and "got"

### Phase 4: Memory/Ownership Pressure Tests (complete)

Goal: exercise the checker against the hardest memory/ownership cases — not just isolated error tests but composed patterns that force the checker, docs, and proof boundaries to agree.

#### 4.1 Memory-model pressure (5 tests)

- `pressure_borrow_in_loop.con` — fresh borrow block per loop iteration
- `pressure_interleaved_linear.con` — two linear vars with interleaved borrow/consumption
- `pressure_nested_linear_struct.con` — nested linear struct consumed whole
- `pressure_branch_create_consume.con` — linear vars created/consumed within if/else branches
- `pressure_match_linear_arms.con` — all match arms consuming same pre-existing linear var

#### 4.2 Borrow/aliasing pressure (3 tests)

- `pressure_sequential_mut_ref.con` — sequential deref write, deref read, deref write, then call on borrow-block `&mut T`
- `pressure_param_ref_multiuse.con` — parameter `&mut T` ref passed through multi-function chain
- `pressure_borrow_then_consume.con` — borrow linear struct for inspection, unfreeze, then consume

#### 4.3 Cleanup/leak-boundary pressure (11 tests: 6 positive + 5 negative)

Positive:
- `pressure_defer_nested.con` — two defers in LIFO order
- `pressure_defer_in_loop.con` — defer in function called from loop
- `pressure_defer_with_borrow.con` — defer combined with borrow block
- `pressure_destroy_wrapper.con` — Destroy trait satisfies linearity
- `pressure_linear_helper_consume.con` — multi-hop linear consumption chain
- `pressure_heap_defer_free.con` — heap alloc with deferred free (arrow access while reserved)

Negative (error cases):
- `pressure_err_defer_then_move.con` — move reserved-by-defer variable
- `pressure_err_heap_leak.con` — heap pointer never freed
- `pressure_err_linear_no_destroy.con` — linear struct goes out of scope
- `pressure_err_destroy_then_use.con` — use after destroy consumption
- `pressure_err_branch_leak.con` — consumed in one branch, leaked in other

### Phase 5: Self-Consistency Checks (complete)

Goal: verify that the compiler's internal data structures agree with each other — proof obligations match re-derivation, diagnostics agree with obligation status, extraction results are consistent with eligibility, and fingerprints are coherent across entries and obligations.

`--report consistency` runs `ProofCore.selfCheck` with 15 cross-family invariants:

- **OBL-KNOWN**: every obligation references a known function (entry or excluded)
- **OBL-STATUS**: obligation status agrees with re-derivation via `deriveObligationStatus`
- **PROVED-EXTRACTED**: proved status requires extraction=Some
- **PROVED-FP**: proved status requires matching fingerprint
- **PROVED-SPEC**: proved status requires spec attachment
- **STALE-FP**: stale status requires mismatched fingerprint
- **STALE-SPEC**: stale status requires spec attachment
- **ENTRY-FP**: entry fingerprint matches obligation fingerprint
- **EXTRACT-UNSUP**: extracted=Some implies empty unsupported list
- **BLOCKED-UNSUP**: eligible + extracted=None implies non-empty unsupported list
- **DEP-PROVED**: dependencies only reference proved obligations
- **DUP-NAME**: no duplicate function names across entries and excluded
- **DIAG-STATUS**: diagnostic kinds agree with obligation status
- **ENTRY-OBL**: every entry has a corresponding obligation (no dropped obligations)
- **EXCL-OBL**: every excluded function has a corresponding obligation

All 468 compilable test programs pass with zero violations. Integrated into `--full` mode.

### Phase 6: Verifier Passes (complete)

Goal: catch internal compiler invariant violations before bad state leaks downstream. Three verifier passes implemented in `Concrete/Verify.lean`:

- **Post-Elab verifier** (`verifyNoPlaceholders`): detects `Ty.placeholder` surviving elaboration. Available only via `--report verify` (not wired into pipeline). 14 programs with try/defer expressions retain placeholder types — documented intentional exception, resolved during lowering.
- **Post-Mono verifier** (`verifyPostMono`): hard gate — detects `Ty.typeVar` surviving monomorphization. Blocks compilation. Skips generic definitions (only checks monomorphized copies). Wired into `Pipeline.monomorphize`.
- **LLVM IR validation** (`validateLLVMIR`): runs `llvm-as` on emitted `.ll` files before clang. Gracefully skips if llvm-as not on PATH. Wired into all four compilation paths.

`--report verify` runs both post-Elab and post-Mono verifiers and reports results. All 385 non-error test programs pass with zero verifier errors. Integrated into `--full` mode.

### Phase 7: Debug Bundle (complete)

Goal: capture everything needed to reproduce a compilation failure in a stable directory layout.

`concrete debug-bundle <file.con> [-o dir]` runs the full pipeline, accumulating artifacts at each stage. On failure or success, it writes a bundle containing: `manifest.json` (compiler version, source path, failure stage, artifact flags), `source/` (original source files), `diagnostics.txt`, `core.txt` (Core IR), `ssa.txt` (SSA IR), `llvm.ll` (LLVM IR), `consistency.txt` (ProofCore self-check), and `verify.txt` (post-Elab verifier results). Each artifact is only present if the pipeline reached the stage that produces it.

The capture pipeline tracks 9 stages: parse, resolve, check, elaborate, coreCheck, mono, lower, emit, complete.

### Phase 8: CI Trust Gate (complete)

Goal: run the four correctness contracts automatically in CI so regressions in determinism, self-consistency, terminology, and verifier invariants are caught before merge.

`./scripts/tests/run_tests.sh --trust-gate` runs only these sections:

- **Determinism** — `test_determinism.sh --quick` across 18 report modes, 6 query kinds, 3 IR emit modes, and snapshot comparison
- **Self-consistency** — `--report consistency` on all 468 compilable programs (15 ProofCore invariants)
- **Terminology** — `test_terminology_gate.sh` enforcing canonical proof/obligation status terms
- **Verifier passes** — `--report verify` on all 385 non-error programs
- **Malformed artifacts** — 23 attack tests: truncated/wrong-type/missing-field/duplicate-key snapshots, corrupted/empty/duplicate/empty-fingerprint registries, valid-empty-registry acceptance, no-duplicate-warning check, bad TOML dependencies/policies/sections/missing-package, bundle validation (missing/corrupted/partial manifests, wrong field types, valid bundle), diff missing-file explicit diagnostic
- **Invalid-query diagnostics** — 11 tests: empty query, unknown single/two/three-part kinds, empty segments, too many separators, valid kind/semantic/filter queries still work, error messages list known kinds
- **State desynchronization** — 13 attack tests: registry-vs-fingerprint stale detection, registry-vs-function-identity unknown-function warning, conflicting specs, obligation-diagnostic cross-check, snapshot fact tampering, doctored snapshot drift detection, obligation-traceability identity agreement, stale-proof consistency invariants, snapshot-then-edit drift, trusted/ineligible function proof bypass
- **Bug corpus audit** — `audit_bug_corpus.sh` verifies every numbered bug has a mapped regression test

The `trust-gate` CI job runs in parallel with the main test suite and SSA tests. Also available locally as `make test-trust-gate`.

#### Malformed Artifact Error Policy

Each artifact class has an explicit decision on warning vs hard error:

| Artifact | Missing | Corrupt/Malformed | Schema Issue |
|---|---|---|---|
| **Snapshot JSON** | hard error (exit 1) | hard error (exit 1) | warning (missing fields) |
| **Proof-registry** | silent (no file = no registry) | warning + empty registry | warning (duplicates, empty fingerprint) |
| **Concrete.toml** | hard error (build fails) | warning (bad lines skipped) | warning (missing [package], unknown sections) |
| **Debug bundle** | hard error (validate-bundle exit 1) | hard error (exit 1) | warning (missing fields) |
| **Report mode** | n/a | n/a | hard error (unknown mode) |

The principle: missing optional artifacts (registry, policy) are silent. Present-but-corrupt artifacts always produce a diagnostic. Schema violations that don't prevent operation are warnings; structural failures that prevent meaningful processing are errors.

### Phase 9: Testcase Reducer (complete)

Goal: automatically shrink failing programs to minimal reproduction cases.

`concrete reduce <file.con> --predicate <pred> [-o output] [--verbose]` applies syntax-aware shrinking passes in a fixpoint loop: remove top-level items → remove statements → remove match arms → remove else branches. Repeats until no pass makes progress.

10 predicates covering all pipeline stages: `parse-error`, `resolve-error`, `check-error`, `elab-error`, `core-check-error`, `mono-error`, `lower-error`, `consistency-violation`, `verify-warning`, `crash`. Substring matching via colon syntax (e.g., `check-error:expected Int`). For un-parseable programs, falls back to line-based reduction.

### Phase 10: Uniform Diagnostic Engine (complete)

Goal: every compiler phase emits the same structured `Diagnostic` shape.

All pipeline phases now use `ExceptT Diagnostics` instead of `ExceptT String`: Parser (`throwParse`), Mono (`MonoM`), Lower (`throwLower`), alongside Resolve, Check, Elab, CoreCheck, and SSAVerify which already used structured diagnostics. `liftStringError` eliminated entirely from the codebase. All build warnings fixed (zero-warning build enforced).

### Phase 11: CI/CD Evidence Gates (complete)

Goal: verify proof, predictable, and report correctness in CI.

10 evidence gates added to the trust-gate CI section:
- **Predictable check**: `crypto_verify` must pass, `thesis_demo` must fail (has I/O)
- **Stale-proof check**: no proof-bearing example (`crypto_verify`, `elf_header`) has stale proofs
- **Proof-obligation status**: `thesis_demo` has at least one proved obligation
- **Report artifact generation**: all 18 `--report` modes produce non-empty output
- **Trust-drift check**: consistency and fingerprints pass on all proof-bearing examples

Trust-gate now covers 5 contract sections (determinism, consistency, terminology, verify, evidence) with 952 total checks (before later phases added more).

### Phase 12: Error Context Chains (complete)

Goal: compiler diagnostics accumulate human-readable context as errors propagate up.

`Diagnostic` now carries a `context : List String` field. Helpers: `Diagnostic.addContext`, `Diagnostics.addContext`, `withContext` (monadic), `Except.addContext` (pure). Rendering appends `= while ...` lines. Annotated at: Check (per-module, per-function), Elab (per-module, per-function), Lower (per-function).

### Phase 13: Module/Package Policy Checks (complete)

Goal: make `[policy]` in `Concrete.toml` a first-class compile-error mechanism, not just report-side analysis.

`Concrete/Policy.lean` implements `ProjectPolicy` with three constraint types:
- **`predictable = true`**: enforces the predictable-execution profile (no recursion, alloc, FFI, blocking) as compile errors, reusing `Report.checkPredictableModule`
- **`deny = ["Unsafe", ...]`**: forbids listed capabilities via `CFnDef.capSet.normalize` checking
- **`require-proofs = true`**: requires Lean proofs for all eligible functions, rejecting `.missing`, `.stale`, `.blocked` obligation statuses

`parsePolicy` parses the `[policy]` TOML section. `enforcePolicy` runs after CoreCheck on project modules only (dependencies excluded via `depNames`), returning structured `Diagnostics` with pass="policy". Wired into both `compileBuild` and `compileTests` in `Main.lean`. Evidence gate: `crypto_verify` example builds clean with `predictable = true` and `deny = ["Unsafe"]`.

### Phase 14: Attacker-Style Drift Demo (complete)

Goal: define the thesis threat/accident model and demonstrate Concrete catching authority/resource/proof drift end-to-end.

Threat model defined in `docs/THREAT_MODEL.md` covering 6 categories: proof semantic drift, authority escalation, validation weakening, resource drift, trust boundary erosion, specification mismatch.

Three end-to-end drift demos using `concrete snapshot` + `concrete diff`:
- **`crypto_verify`**: `+` → `-` in tag computation, `>` → `>=` in nonce check — proof drift + validation weakening
- **`elf_header`**: magic byte `127` → `0`, version accepts `0` — proof drift + validation weakening
- **`thesis_demo`**: `+` → `-` in parse_byte, `validate` gains `with(File)` + unbounded `while` — proof drift + authority escalation + resource drift

8 new drift-detection gates in CI evidence section verify: trust weakening detected, `proved → stale` transitions, `is_pure: true → false`, File capability escalation, unbounded loop drift. 34 proof-pressure-set gates verify all 6 obligation states, extraction forms, extraction status/blockers, obligation dependencies/sources, eligibility reasons, effects/evidence consistency, fingerprint/extraction determinism, and Lean theorem stub generation (PExpr definitions, function table, theorem names, eval helpers, excluded function omission, determinism) against `examples/proof_pressure/`. 6 registry integrity adversarial tests validate unknown function rejection, ineligible target rejection, empty proof/spec name rejection, duplicate entry warning, and clean registry boundary. 6 Lean kernel checking tests verify `--report check-proofs` against hardcoded proofs, registry proofs, fake proof names, exit codes, and toolchain reporting. 4 end-to-end Lean attachment tests verify proved/stale consistency across proof-status, obligations, extraction, and check-proofs. 6 stale-proof repair tests cover the full cycle: proved → mutate → stale → update → proved → kernel check. 29 blocked/ineligible pressure tests verify every ineligibility reason (File, Network, Process, Unsafe, Alloc capabilities, direct/mutual recursion, FFI, allocation, blocking I/O, entry point, trusted, combo), every blocked construct (struct literal, match expression, mutable assignment, string literal, if-without-else), registry-targeting-blocked/ineligible errors, consistency invariants, and JSON diagnostic fields. 10 workflow tests verify `concrete build` proof summary line (presence, proved/stale counts, exit code 0 despite stale), `concrete check` output (proof status report, next-steps prioritization, totals, exit code 1 when unresolved, helpful error without project). 10 proof scoping tests verify user-package-only build summary (format and count bounds), check report excludes std.* functions, check totals are user-only, dependency obligations hidden footer, exit code reflects user obligations, next steps reference only user functions, require-proofs passes for ineligible-only user code, and require-proofs catches user-package missing proof. 15 proof workflow tests verify: extraction report status labels (extracted/excluded), PExpr form presence, fingerprint presence, lean-stubs generation (PExpr defs, theorem stubs with sorry, eval helpers, function table, ineligible exclusion), proof-status shows all 5 obligation states, proof-diagnostics failure/repair classes, proof-deps dependency edges, extraction-obligation fingerprint consistency, end-to-end stale repair cycle (proved → stale → repaired), rename detection hints, obligation totals, and diagnostic-codes E0800-E0807. Trust-gate: 1404 checks, now includes 12 interpreter semantic oracle tests (parse_validate exit match, function calls, array loops, structs, enum match, XOR bitwise, unsupported diagnostic, compiled-vs-interpreted comparison with stdout, scope isolation, negative array index, observable contract, match-arm mutation persistence), 10 parse-validate error-flow tests (build, run, predictable pass, effects/evidence, trusted count, allocation, enum compilation, bounded loop, policy, purity), 12 fixed-capacity validation tests (build, run, predictable pass, evidence levels, trust classification, bounded loops, zero alloc, proof eligibility, extraction gaps, policy, capability-free core), 10 service-errors error-propagation tests (build, run, predictable pass, effects/evidence, trusted count, allocation, error code functions, pipeline handlers, policy, purity), 25 stack-depth reporting tests (12 report structure/format + 13 adversarial: deep chain depth=12, wide fan depth=1, mixed recursive/bounded split, large frame >100 bytes, diamond pattern, zero-param minimum frame), 27 malformed-artifact attack tests, 13 state-desynchronization attack tests, 11 invalid-query diagnostic tests, 9 API versioning envelope tests, 7 error code taxonomy tests, 20 policy enforcement adversarial tests (14 project-based + 6 standalone predictable boundary: direct/mutual recursion fail, hidden alloc chain fail, while loop fail, nested match pass, Copy enum chain pass), 15 proof failure taxonomy tests, 15 proof workflow tests, 15 proof evidence bundle tests, 7 proof gate CI tests, 24 proof pressure set inspection gates, 6 registry integrity gates, 6 Lean kernel checking gates, 4 end-to-end Lean attachment gates, 6 stale-proof repair gates, 29 blocked/ineligible pressure gates, 10 workflow tests, 10 proof scoping tests, 10 proof attachment stability tests, 11 proof dependency tests, and bug corpus audit.

### Phase 15: Adversarial Compiler-Hardening Corpus (complete)

Goal: hostile workloads that stress every compiler pass under scale, composition, and edge-case pressure.

64 positive test files across 10 categories, all passing. Plus 16 policy enforcement tests (11 negative + 5 boundary positive, trust-gate) and 10 negative adversarial tests (hostile code the compiler must reject):

**Parser stress (8 tests)**: deep nesting (15 levels), long expressions (50 operators), many locals (30), many params (20), complex match (15 arms), empty bodies, nested parens (16 levels), mixed operator precedence.

**Lowering stress (9 tests)**: 4-level nested struct access, 20-variant enum match, 4-level nested loops, struct create/consume in loops, enum-in-struct, large enum payloads, 10-function call chains, array-of-structs iteration, defer LIFO ordering.

**Monomorphization stress (7 tests)**: generic enum with 3 types, 5-type instantiation fan-out, trait with 3 methods + 2 impls, trait with 3 impls + generic dispatch, generic return struct, recursive generic struct, nested generics.

**Module stress (7 tests)**: 3-level deep module chain, struct/enum across module boundaries, transitive imports, capability propagation across modules, same-name functions across siblings.

**Proof/report stress (5 tests)**: generic function with proof + monomorphization, 10 pure proof-eligible functions, mixed eligibility (pure/capability/trusted), module isolation with qualified calls, report generation with 20 functions.

**Scaling/hostile workloads (9 tests)**: 40-function chains, 25-variant enum, 20-deep struct transform chain, 20-field struct, 5 simultaneous enums, 3-level nested match, array operations, generic struct explosion, 12 sibling modules.

**Error-flow stress (7 tests)**: 10-variant Copy enum exhaustive match, nested result-in-result enum, 5-function error propagation chain, struct payloads in error variants, multiple coexisting error enums with conversion, exhaustive match-all-paths, many-variant enum.

**Stack-depth stress (6 tests)**: 12-function deep call chain (depth=12), 8-leaf wide fan (depth=1, not 8), mixed recursive/non-recursive, large frame (>100 bytes), diamond call pattern (deduplication), zero-param minimum frame (8 bytes).

**Pipeline stress (4 tests)**: 3-stage error conversion with unified AppError, severity classification with is_fatal/is_retriable, partial success with intermediate state preservation, 4-check fan-in first-failure reporting. **Known issue**: `adversarial_pipeline_partial_success.con` crashes lli (LLVM interpreter JIT, LLVM 21.1.8) on the `Partial { state: PartialState, error: PipeErr }` variant layout; native compilation via clang works correctly — this is an lli-only bug, not a Concrete codegen issue.

**Predictable boundary stress (5 tests)**: deeply nested match (pure, passes), Copy enum construct/match chain (passes), direct recursion (fails), mutual recursion (fails), hidden 3-level alloc chain (fails).

**Policy enforcement adversarial tests (19 tests, trust-gate):**

Project-based negative (must be rejected, 11 tests):
- `adversarial_policy_deny_unsafe` — deny=["Unsafe"], code uses Unsafe capability (E0611)
- `adversarial_policy_deny_multi` — deny=["File","Network"], code uses both (E0611)
- `adversarial_policy_predictable_recursion` — predictable=true, recursive function (E0610)
- `adversarial_policy_predictable_alloc` — predictable=true, uses Alloc cap (E0610)
- `adversarial_policy_predictable_ffi` — predictable=true, extern FFI call (E0610)
- `adversarial_policy_predictable_blocking` — predictable=true, uses File cap (E0610)
- `adversarial_policy_combined` — predictable+deny, both E0610 and E0611 (2 assertions)
- `adversarial_policy_require_proofs_missing` — require-proofs=true, eligible function has no proof (E0612)
- `adversarial_policy_require_proofs_stale` — require-proofs=true, stale registry fingerprint (E0612)
- `adversarial_policy_require_proofs_blocked` — require-proofs=true, eligible-but-unextractable function (E0612)

Project-based positive boundary (must pass, 3 tests):
- `adversarial_policy_deny_pass` — deny=["Unsafe"], code only uses Console (allowed)
- `adversarial_policy_predictable_pass` — predictable=true, pure bounded code (allowed)
- `adversarial_policy_empty` — empty [policy] section, no restrictions (allowed)

Standalone predictable boundary (5 tests):
- `adversarial_predict_bound_direct_recursion` — direct recursion fails `--check predictable`
- `adversarial_predict_bound_mutual_recursion` — mutual recursion fails `--check predictable`
- `adversarial_predict_bound_hidden_alloc` — 3-level Alloc chain fails `--check predictable`
- `adversarial_predict_bound_nested_match` — deeply nested pure match passes `--check predictable`
- `adversarial_predict_bound_copy_enum_chain` — Copy enum chain passes `--check predictable`

There is intentionally no `require-proofs=true` passing boundary yet. Today that policy is enforced at whole-program scope, and the hosted stdlib still contains proof-eligible functions without attached proofs. A true passing boundary belongs after a no-std mode or stdlib proof coverage exists.

**Negative adversarial tests (10 tests, hostile code that must be rejected):**

Ownership/borrow abuse (4 tests):
- `adversarial_neg_double_move_branch` — move in one branch, use after if
- `adversarial_neg_borrow_outlives_owner` — consume owner while borrow is active
- `adversarial_neg_mut_ref_alias` — nested mutable borrows of same variable
- `adversarial_neg_move_loop_body` — consume linear value inside loop

Module/visibility abuse (2 tests):
- `adversarial_neg_private_fn_call` — call non-pub function from outside module
- `adversarial_neg_private_struct_field` — use non-pub struct type from outside module

Capability abuse (2 tests):
- `adversarial_neg_cap_escalate_indirect` — pure→pure→needs_file chain (escalation)
- `adversarial_neg_cap_forge` — pure function calls println without Console

Type system abuse (2 tests):
- `adversarial_neg_enum_variant_type_mismatch` — wrong payload type in enum constructor
- `adversarial_neg_return_type_mismatch` — return Bool from Int-typed function

**Fixed compiler bugs (regression tests retained):**
1. **LLVM IR function name mangling collision** — same-name functions in different modules produced duplicate LLVM definitions. Fixed by qualifying colliding names with module path in EmitSSA; collision key uses `f.modulePath` to handle flattened submodules. Regression tests: `adversarial_module_same_name`, `adversarial_module_many_siblings`, `adversarial_module_struct_across`, `adversarial_module_enum_across`.
2. **Generic Copy struct core-check failure** — `struct Copy Box<T>` rejected because field type `.named "T"` wasn't recognized as a type parameter. Fixed by checking field type names against struct `typeParams` in CoreCheck, plus post-mono Copy validation in Verify.lean that rejects `Box<NonCopy>` instantiations. Regression tests: `adversarial_mono_generic_return_struct`, `adversarial_mono_nested_generics`, `adversarial_mono_recursive_generic_struct`, `adversarial_mono_generic_enum`, `adversarial_scale_generic_explosion`, `error_copy_generic_non_copy_instantiation` (negative).
3. **Top-level main alongside inline modules** — `pub fn main()` after `mod` blocks was silently dropped by the parser. Fixed by parsing remaining top-level items as a sibling "main" module. Regression tests: `adversarial_proof_module_isolation`, `adversarial_scale_many_modules`.
