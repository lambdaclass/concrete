# Testing

Status: stable reference

This document describes the test architecture, coverage matrix, determinism policy, and execution model for Concrete.

## Test Architecture

The test system has four layers, ordered by cost:

| Layer | Tool | Cost | What it catches |
|-------|------|------|-----------------|
| **Pass-level** | `PipelineTest.lean` (Lean executable) | <1s, no I/O | Parse errors, type errors, elaboration bugs, monomorphization bugs, SSA verify/cleanup invariants, emit correctness |
| **Artifact** | `run_tests.sh --report`, `--codegen` | ~1s each, no clang | Report output regressions, SSA structure, LLVM IR shape, codegen differentials |
| **End-to-end** | `run_tests.sh` positive/negative | ~0.5s each, needs clang | Full compile-and-run behavior, runtime correctness |
| **Stress/integration** | `run_tests.sh` integration tests | ~1s each, needs clang | Multi-feature interactions, deep call chains, realistic programs |

### Pass-Level Lean Tests

`PipelineTest.lean` exercises individual compiler passes directly on in-memory source strings. No subprocess, no clang, no file I/O.

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
- **Negative (~170)**: compile, expect specific error message in stderr
- **Abort (1)**: compile, run, expect crash
- **Test flag (4)**: `--test` mode with pass/fail/mixed/submodule programs
- **O2 (~90)**: same programs compiled with `-O2`, check same results
- **Cross-target (~25)**: IR verified to compile for x86_64 via `clang --target`
- **Performance (1)**: regression check against saved baseline (>20% warning)

### Stdlib Tests

184 `#[test]` functions across all stdlib modules, compiled through the real compiler path. Module-targeted testing via `--stdlib-module <name>`.

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
| Runtime behavior | E2E | ~180 | All positive run_ok tests |
| -O2 regressions | E2E | ~90 | All computation, struct/enum, linearity, borrow, generic, trait, string, heap, vec, complex, integration, phase3 programs |
| Report accuracy | Artifact | ~90 | `report_integration.con`, `report_*_check.con`, `test_proof_*.con`, phase3 consistency cross-checks |
| ABI / FFI | E2E | 9 | `test_repr_c_*`, `test_fn_ptr_*`, `test_sizeof_*`, `test_ptr_round_trip`, `test_array_bounds`, `phase3_abi_interop` |
| Layout/ABI | Pass-level | 4 | Scalar sizes, builtin sizes, repr(C), pass-by-ptr |
| Stdlib correctness | Stdlib | 184 | All `#[test]` functions |
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
| Fast suite (`--fast`) | ~25-35s (~890 tests, parallel) |
| Full suite (`--full`) | ~40-50s (~911 tests, 2 skipped, includes network, cross-target, perf) |
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
