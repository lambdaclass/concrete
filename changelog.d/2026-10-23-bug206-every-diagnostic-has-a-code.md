### Every Diagnostic Has A Code: Bug 206 (2026-10-23)

Over thirty compiler refusals had no diagnostic code: twenty Check errors built from a bare message,
eleven diagnostics written without one, and every module-loading failure. Each now names the code of the rule it enforces, with
new codes E0122 – E0124 and E0312 – E0321 and three new rules (MOD-4, module files; PAT-8, an
enum destructuring `let` is a block statement; STMT-10, test functions). `Diagnostic.code` no longer has a default, so a code-less
diagnostic does not typecheck, and `check_diagnostic_codes_present.sh` and the `run_err` output
check in `run_tests.sh` keep it that way.
