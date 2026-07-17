# Bug 038: if-merge clobbered promoted-aggregate mutations

**Status:** Fixed (2026-07-16)
**Fixed in:** `Concrete/IR/Lower.lean` — the statement-if, if-expr, and match
merge loops now skip reconciliation for ANY promoted variable (aggregate
included): the promoted alloca is the single source of truth, written through
by every arm. Previously only promoted scalars skipped; promoted aggregates
were reconciled from the var-table snapshot — the stale pre-if value — and
`setVar` stored it back over the promoted alloca, clobbering the arms' writes.
**Regression test:** `tests/programs/regress_038_if_merge_promoted_aggregate.con`
(prints "qm"); extended differential fuzzer (`fuzz_differential.py` now
generates aggregate value-ifs, method borrows in both branch arms, and a
non-Copy String) is the class gate.
**Discovered:** 2026-07-16, by the audit-2026-07-16 fuzzer grammar extension
(seed 3, depth 4) — the grammar previously could not generate
strings/non-Copy, which is why the bug was invisible to the nightly fuzz.

## Symptom

```con
let mut str0: String = "q";
if true { string_push_char(&mut str0, 109); } else { }
println(&str0);   -- compiled: "q" (mutation lost); interp: "qm" (correct)
```

## Root cause

`str0` is pre-promoted (bug 031) into a stable alloca because its address is
taken in the arm; the `string_push_char` correctly wrote through that alloca.
But the statement-if merge loop only skipped reconciliation for promoted
SCALARS ("memory-backed, single source of truth" — the C9 fix). Promoted
aggregates went through the aggregate merge path, which reads the var-table
snapshots — for a promoted var those hold the STALE pre-promotion value
(the `"q"` literal) — and the resulting merge value was stored back into the
promoted alloca, erasing the mutation. Fourth instance of the merge-loop
family (029/031/033/034); the family fix (unify the four branch sites) is
ROADMAP 1a.

## Note

Same commit: `string_length` in the interpreter returned codepoints while the
backend returns bytes (drift on non-ASCII), and the interpreter grew
`string_push_char`/`string_append` — both prerequisites of the fuzzer
extension that found this bug.
