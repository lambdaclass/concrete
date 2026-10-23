# Bug 203 — E0420 named two unrelated errors

**Status:** FIXED (branch `wip/bug203`, based on `main` `1b2dba60`)
**Found:** 2026-10-23, while migrating the language specification's contracts chapter: CON-4's
code E0420 (a ghost binding read by runtime code) was also the code of an `append()` argument of
an unsupported type, so the specification's one-code-one-rule mapping (S6) could not tag it
**Severity:** a diagnostic-identity defect, not a soundness one. Both programs were refused, but
the code did not say which rule refused them, so the rule tag and the specification link of an
E0420 diagnostic could be wrong.

## Reproducer

```con pseudocode
fn main() with(Alloc) -> Int {
    let mut out: String = "";
    append(&mut out, 1.5);
    drop_string(out);
    return 0;
}
```

Expected: a code of its own (TYPE-10, builtin signatures). Observed on `main` `1b2dba60`: E0420,
the code of `ghost let` read at run time (CON-4).

## Cause

`Concrete/Elab/Elab.lean`: the `append()` desugaring builds its diagnostic inline and spelled
`code := "E0420"`, the code `ElabError.ghostInRuntime` already used. Codes are chosen by hand at
each site, and nothing checked that a code is used by one error only.

## Fix

The `append()` refusal is E0421, with a diagnostic-codes ledger entry, a `codes.tsv` row and a
place in TYPE-10's `Codes`. E0420 stays the ghost read. Fixtures:
`error_203_append_unsupported_argument.con` (E0421) and `error_203_ghost_read_at_runtime.con`
(E0420), in `run_tests.sh`.

## Structural cause

Class 12, one fact kept in two places, read the other way: one code stood for two facts. A
diagnostic code is spelled at its site rather than derived from the error it reports.

## Structural owner

The language specification's code mapping (`docs/spec`, S6: every code belongs to one rule), with
codes derived from error constructors (`ElabError.code`) rather than spelled inline.
