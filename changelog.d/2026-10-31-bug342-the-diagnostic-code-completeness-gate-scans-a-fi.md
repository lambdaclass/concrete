### Bug 342: the diagnostic-code completeness gate scans a fixed file list, so eleven emitted codes (E0900 among them) have no ledger row and no spec classification (2026-10-31)

Filed, not fixed: eleven emitted diagnostic codes (E0613 - E0617, E0811 - E0815, E0900) have no row in `--report diagnostic-codes`, because `check_diagnostic_codes_complete.sh` scans a fixed module list; the spec's S6 therefore never classifies them. The E0900 reproducer is pinned by `check_example_outputs.sh` (open-bug reproducers leg)
([bug 342](docs/bugs/342_the_diagnostic_code_completeness_gate_scans_a_fi.md)).
