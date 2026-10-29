# Bug 249 — the parser appends an explicit array literal's elements one by one (quadratic)

<!-- bug-ledger
summary: the parser appended each element of an explicit array literal to the list built so far, so `[e1, …, en]` took time n² to parse (40000 elements: 7 s, now 0.03 s). Pushed onto an `Array` and converted once. Pinned by `regress_249_explicit_array_literal.con` and a generated 2 x 100000-element literal. Found measuring bug 027. Class 17.
fixtures: regress_249_explicit_array_literal.con
class: 17 — the parser appended an explicit array literal's elements one at a time
silent: no
ruling: Outside the spec: performance — the parser appended explicit array-literal elements one at a time, quadratic
-->

**Status:** FIXED (branch `wip/perf-quadratics`)
**Found:** 2026-10-25, measuring bug 027: an explicit literal of n elements took longer than
`[v; n]` in every mode, and the extra time was all in the parser.
**Severity:** loud (performance). Every explicit array literal (`[e1, e2, …]`) cost the square of
its length to parse, for the binary, `--interp` and every report.

Confirmed real by Codex review, 2026-10-25: ROADMAP exit 2o requires near-linear parsing; repeated prefix copying made explicit literals quadratic. The small fixture checks semantics; generated literals check scaling.

## Reproducer

A literal of 40000 explicit elements (`let a: [i64; 40000] = [7, 7, …];`): parsing alone took
7.0 s (20000: 1.15 s); fixed 0.03 s (160000: 0.11 s). `regress_249_explicit_array_literal.con`
pins the shapes the parser must keep (element order, a trailing comma, nested and computed
elements); `run_tests.sh` also parses (`--emit-core`) and compiles a generated literal of 2 x 100000
elements within 20 s.

## Cause

`Parser.lean`, the `.lbracket` case of `parsePrimary`: after the first element, each element was
appended to the list built so far (`elems := elems ++ [e]`), copying it every time. The repeat form
`[v; n]` had been fixed to `List.replicate` by bug 026; the comma form had not.

## Fix

The elements after the first are pushed onto an `Array` and the list is built once.

## Structural cause

Class 17 ("pass state in association lists and per-query recomputation"): a list grown by append
in a loop, the shape the append ratchet counts but cannot find in code that predates it; bug 236
was the same literal's next stage.

## Structural owner

R-0422 (compiler throughput queue, complexity envelopes).
