### Bug 342: the diagnostic-code completeness gate scans a fixed file list, so eleven emitted codes (E0900 among them) have no ledger row and no spec classification (2026-10-31)

The diagnostic-code ledger is now checked against every compiler module: eleven codes the compiler
emitted had no row in `--report diagnostic-codes` (E0613 - E0617, E0811 - E0815, E0900), because
`check_diagnostic_codes_complete.sh` scanned a fixed module list. They have rows and spec
classifications now. A proven runtime-safety violation reports one code per case: E0900 an index
out of bounds (MEM-9, the refusal half of MEM-6@1), E0901 a zero divisor and E0902 a proven overflow
(ARITH-13). The spec gate's S6 refuses a code a language stage reports when it maps to no rule
([bug 342](docs/bugs/342_the_diagnostic_code_completeness_gate_scans_a_fi.md)).
