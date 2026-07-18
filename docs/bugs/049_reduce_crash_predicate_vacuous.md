# Bug 049: `concrete reduce --predicate crash` is vacuous

**Status:** Open
**Discovered:** 2026-07-18, reducer audit (demonstrated:
`concrete reduce tests/programs/fib.con --predicate crash` reduces a working
program to a single newline and reports success).

## Symptom

`--predicate crash` is documented (CLI usage text) as "crash = parse succeeds
but anything after can fail" and is the obvious choice for crash-class bugs.
In `Concrete/Report/Reduce.lean:76-80` the predicate's `evalPredicate`
returns `true` for ANY candidate that *parses* — nothing after parsing is
ever run. Every deletion that stays parseable is accepted, so the reducer
converges on the empty program, which cannot exhibit any crash. The output
is worthless and the user is told it is a valid minimization.

## Root cause

The crash predicate cannot be evaluated in-process safely: a candidate that
crashes the pipeline would abort the reduction itself (no isolation), so the
check was stubbed to parse-only — silently downgrading an unsound predicate
into the shipped CLI.

## Candidate fix

Either evaluate the crash against a sandboxed subprocess per candidate
(compile+run with timeout, crash = nonzero/signal on the compiled artifact),
or remove `--predicate crash` from the CLI until that exists. Related
weakening documented in the same audit: substring-less stage predicates
(`check-error` etc.) accept candidates failing for a *different* reason —
at minimum the reducer should print a warning when the final error class
differs from the original's; and multi-module programs currently get zero
reduction silently (every shrink pass is a no-op on `| _ =>` module lists).
