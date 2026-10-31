# Bug 342 — the diagnostic-code completeness gate scans a fixed file list, so eleven emitted codes (E0900 among them) have no ledger row and no spec classification

**Status:** FIXED (branch `wip/bug-342-code-ledger`): the completeness gate scans every compiler module, the eleven codes have ledger rows and classifications, E0900 is split into one code per case, and S6 refuses a language-stage code that maps to no rule
**Found:** 2026-10-31, by a probe while answering the 2026-10-30 spec quality audit (MEM-6 could not be given E0900 as its code, because S6 reads only ledger codes and E0900 is not one); confirmed by Codex and Grok
**Severity:** silent, in tooling only: eleven diagnostic codes the compiler emits are missing from `--report diagnostic-codes`, the ledger that claims to list every one, while the gate that enforces it passes; the spec's code classification (S6) and coverage checks never see them. No compiler output changes

<!-- bug-ledger
summary: the diagnostic-code completeness gate scans a fixed file list, so eleven emitted codes (E0900 among them) have no ledger row and no spec classification
coverage: check_diagnostic_codes_complete.sh (whole-tree scan; controls N1 a code from an unlisted module, N2 a removed ledger row, N3 a commented code); check_spec.sh S6 (control N6c) and S4 (MEM-9/index-proven-out-of-bounds E0900, ARITH-13/divisor-proven-zero E0901); check_proven_violation_enforcement.sh and check_obligation_redteam.sh pin E0900 and E0901
class: 11
silent: yes
ruling: Outside the spec: tooling — the diagnostic-code ledger and check_diagnostic_codes_complete.sh; S6 classifies ledger rows only
-->

## Fix

- `scripts/tests/check_diagnostic_codes_complete.sh` reads every `"E<4 digits>"` literal in every
  module under `Concrete/` (comment lines aside; `Report.lean` is the ledger itself), so no list of
  emitters can fall behind. Its negative controls plant a code in a new module, delete a ledger row
  and plant a code in a comment.
- `Concrete/Report/Report.lean` has rows for E0613 - E0617 (policy), E0811 - E0815 (proof, mono)
  and E0900 - E0902, classified in `docs/spec/codes.tsv` (tooling, internal, rule).
- `provenViolationDiagnostics` reports one code per case: E0900 an index proven out of bounds
  (MEM-9), E0901 a divisor proven zero and E0902 an operation proven to overflow (ARITH-13). The
  refusal clause of MEM-6@1 moved to MEM-9, so MEM-6@2 is the run-time guarantee alone.
- S6 (`scripts/spec/spec.py`) refuses a code a language stage (parse, resolve, check, elab,
  core-check, runtime-safety) reports unless it is a rule code, retired, or internal with a reason
  in `codes.tsv`'s fourth column (control N6c).
- No program reaches E0902 today (the ARITH-13 rationale; `coverage_allow.tsv` says why).

## Reproducer

An index the compiler proves out of bounds (`a[5]` on a `[u8; 3]`) was refused with E0900, a code the ledger did not list; the reproducer was removed with the fix, and MEM-9's reject example is the same program.

## Observed

The compiler prints `error[runtime-safety]: (E0900) proven runtime-safety violation: a[5] is always out of bounds for array size 3`, and `--report diagnostic-codes` has no E0900 row. A whole-tree scan on main `71e3e804` (git grep -ohE '(=> |code := |loadError )"E[0-9]{4}"' -- 'Concrete/*.lean' | grep -oE 'E[0-9]{4}' | sort -u, then for each code `grep -qE "entryR? \"$c\"" Concrete/Report/Report.lean`) finds 258 emitted codes, 11 of them with no ledger row: E0613 – E0617 (`Concrete/Check/Policy.lean`), E0811 – E0813 (`Concrete/Proof/ProofCore.lean`), E0814 – E0815 (`Concrete/IR/Mono.lean`) and E0900 (`Concrete/Report/ReportObligations.lean`). `check_diagnostic_codes_complete.sh` passes.

## Expected

No language rule; the tooling contract: `--report diagnostic-codes` is the ledger of every diagnostic the compiler can emit, and `check_diagnostic_codes_complete.sh` says a new diagnostic cannot land without its ledger row. The spec gate's S6 (`scripts/tests/check_spec.sh`) classifies every ledger code in `docs/spec/codes.tsv`, so a code outside the ledger escapes the spec's coverage checks too (MEM-6 states the E0900 refusal but cannot name its code).

## Cause

`scripts/tests/check_diagnostic_codes_complete.sh` scans a fixed list of modules (CheckError, CoreCheck, Verify, Resolve, Elab, Check, Project, FileSummary, SSAVerify); the emitters in `Policy.lean`, `ProofCore.lean`, `Mono.lean` and `ReportObligations.lean` are outside it, and nothing adds their codes to the `entry` list in `Concrete/Report/Report.lean`.

## Structural cause

Class 11 ([STRUCTURAL_CLASSES.md](STRUCTURAL_CLASSES.md)): absence read as a fact. A module missing from a hand-kept allowlist is read as emitting no codes, and the ledger is a hand-kept list rather than one derived from the emitters, so S6 inherits the omission.

## Structural owner

One source for diagnostic codes: the ledger derived from (or checked against) a whole-tree scan of every emitting module, so a code cannot be emitted without a row (ROADMAP 13m, the registry's first slice, extended to every module).

## Containment while open

`check_example_outputs.sh` (open-bug reproducers leg) pins the E0900 refusal. No program's behaviour depends on the ledger, so there is no user workaround to give; the repair is to scan every module and add the eleven rows (each classified in `docs/spec/codes.tsv`).
