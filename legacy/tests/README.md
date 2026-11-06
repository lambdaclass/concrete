# Tests

Concrete keeps test inputs, fixtures, and runner-facing data under `tests/`.

Layout:

- `programs/`: Concrete source programs used by the shell test runner
  (`scripts/tests/run_tests.sh`), including the negative `error_*`/
  `pressure_err_*` fixtures and `patterns/` / `adversarial/` subfamilies
- `codegen/`: compile-run-assert and interp-vs-compiled differential fixtures
  (`check_codegen_execution.sh`, `check_codegen_differential.sh`)
- `golden/`: expected output baselines for `--emit-core`, `--emit-ssa`, and
  formatter output (`test_golden.sh`; sources in `golden/src/`)
- `known_holes/`: fixtures pinning KNOWN_HOLES entries (open holes stay
  reproducible; closed holes stay closed)
- `oracle/`: differential-oracle vectors (`test_oracle.sh`)
- `fixtures/`: metadata used by the runner and docs (`test_dep_map.toml`,
  `test_manifest.toml`) plus binary fixtures (deliberate `.bin` ELF headers)
- `invalid_programs/`: ORPHANED front-end-negative fixtures — they were driven
  by a Rust-era test harness (removed 2026-07-02; the language was previously
  implemented in Rust). The cases are mostly covered by `programs/error_*`;
  before deleting them, diff the coverage and fold anything unique into
  `programs/` or a gate.

Related entrypoints live outside this directory:

- shell runners and per-feature gates: [`../scripts/tests/`](../scripts/tests/README.md)
- shared gate harness: `../scripts/tests/lib/gate.sh`
- Lean pass-level executable: `Concrete/PipelineTest.lean`

This split keeps test data under `tests/`, test runners under
`scripts/tests/`, and compiler/pass unit tests in Lean next to the compiler.
