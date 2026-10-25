# Bug 203 — E0420 named two unrelated errors

<!-- bug-ledger
summary: an `append()` / `print()` argument of an unsupported type was refused with no code by Check, and Elab's copy used E0420, the ghost-read code (CON-4), so neither identified the rule; both are now E0421 (TYPE-10). Pinned by `error_203_append_unsupported_argument.con`, `error_203_print_unsupported_argument.con` and `error_203_ghost_read_at_runtime.con`. Found while migrating the specification's contracts chapter
fixtures: error_203_append_unsupported_argument.con error_203_ghost_read_at_runtime.con error_203_print_unsupported_argument.con
class: 12 — one diagnostic code, E0420, for two unrelated errors
silent: no
-->

**Status:** FIXED (branch `wip/bug203`, based on `main` `1b2dba60` with `wip/train6` `13bc2527` merged)
**Found:** 2026-10-23, while migrating the language specification's contracts chapter: CON-4's
code E0420 (a ghost binding read by runtime code) was also the code of an `append()` argument of
an unsupported type, so the specification's one-code-one-rule mapping (S6) could not tag it
**Severity:** a diagnostic-identity defect, not a soundness one. Both programs were refused, but
the `append()` / `print()` refusal carried no code at all from Check, and Elab's copy of it the
ghost-read code, so neither said which rule refused the program.

## Reproducer

```con pseudocode
fn main() with(Alloc) -> Int {
    let mut out: String = "";
    append(&mut out, 1.5);
    drop_string(out);
    return 0;
}
```

Expected: a code of its own (TYPE-10, builtin signatures). Observed on `main` `1b2dba60`: Check
refuses it with no code (`throwCheckMsg`), and the same refusal in Elab's `append()` desugaring
spelled E0420, the code of a `ghost let` read at run time (CON-4). `print(1.5)` is the same.

## Cause

`Concrete/Check/Check.lean`, the `print`/`println` and `append` intercepts, built the refusal from
a bare message (`throwCheckMsg`, which sets no code), and `Concrete/Elab/Elab.lean`'s `append()`
desugaring built it inline with `code := "E0420"`, the code `ElabError.ghostInRuntime` already
used. Codes are chosen by hand at each site, and nothing checked that a code names one error.

## Fix

Check reports `CheckError.builtinArgUnsupported`, code E0421, for `append`, `print` and `println`;
Elab's copy (defense in depth behind Check) uses E0421 too. E0421 has a diagnostic-codes ledger
entry, a `codes.tsv` row and a place in TYPE-10's `Codes`. E0420 stays the ghost read. Fixtures:
`error_203_append_unsupported_argument.con` and `error_203_print_unsupported_argument.con`
(E0421) and `error_203_ghost_read_at_runtime.con` (E0420), in `run_tests.sh`. Eighteen other
Check refusals still use `throwCheckMsg` and carry no code; they are filed separately.

## Structural cause

Class 12, one fact kept in two places, read the other way: one code stood for two facts. A
diagnostic code is spelled at its site rather than derived from the error it reports.

## Structural owner

The language specification's code mapping (`docs/spec`, S6: every code belongs to one rule), with
codes derived from error constructors (`ElabError.code`) rather than spelled inline.
