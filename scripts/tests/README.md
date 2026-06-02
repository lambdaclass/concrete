# Test Scripts

This directory contains shell entrypoints for the non-Rust, non-Lean parts of the test system.

Main entrypoints:

- `run_tests.sh`: primary regression runner
- `test_golden.sh`: golden-file verification for `--emit-core`, `--emit-ssa`, and `--fmt`
- `update_golden.sh`: refresh golden baselines
- `test_ssa.sh`: SSA-focused backend checks
- `test_parser_fuzz.sh`: parser fuzzing
- `test_fuzz.sh`: broader structured fuzzing
- `test_perf.sh`: performance-baseline capture and comparison
- `test_mutation.sh`: mutation-testing harness

Supporting helpers:

- `phase3_diagnostic_checks.sh`
- `phase3_report_consistency_checks.sh`

All scripts assume the repository root as their working directory and normalize to it internally.
