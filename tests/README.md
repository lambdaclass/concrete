# Tests

Concrete keeps test inputs, fixtures, and runner-facing data under `tests/`.

Layout:

- `programs/`: Concrete source programs used by the shell test runner
- `invalid_programs/`: focused invalid-input fixtures for parser/front-end Rust tests
- `golden/`: expected output snapshots for `--emit-core`, `--emit-ssa`, and formatter output
- `fixtures/`: metadata used by the runner and docs (`test_dep_map.toml`, `test_manifest.toml`)

Related entrypoints live outside this directory:

- shell runners: [`../scripts/tests/`](../scripts/tests/README.md)
- Lean pass-level executable: `Concrete/PipelineTest.lean`
- Rust integration tests: `tests/*.rs`

This split keeps:

- test data under `tests/`
- test runners under `scripts/tests/`
- compiler/pass unit tests in Rust/Lean next to their respective toolchains
