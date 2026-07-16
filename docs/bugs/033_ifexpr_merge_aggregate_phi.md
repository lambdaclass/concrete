# Bug 033: value-`if` merge phied live aggregate variables — E0714

**Status:** Fixed (2026-07-16)
**Fixed in:** Lower.lean ifExpr merge loop — live-variable reconciliation now
uses the statement-if's aggregate path (entry alloca + insertStoreBeforeTerm
per branch + load; arrays rebind the alloca ADDRESS per the bug-029 rule)
instead of an unconditional phi. SSAVerify's E0714 (no aggregate phis) was
correctly refusing the bad IR — front-end accepted, verifier caught it, so
this was a compile-time rejection, not silent wrong code.
**Regression test:** `tests/programs/regress_033_discard_live_string.con`
(prints "live-across").
**Discovered:** 2026-07-16, Phase 7 workload 2 (png_chunks) — `discard(r.is_ok())`
with a live String parameter in scope.

## Symptom

Any `discard(e)` — which desugars (slice 5) to a unit-valued value-`if`
(`if true { e; }`) — with a live String/struct/array variable in scope failed
to compile: `error[ssa-verify]: (E0714) ... phi %ifphi.N has aggregate type
... — use alloca+store instead`. Same for any hand-written value-`if` with a
live aggregate reassigned across it.

## Root cause

Two merge loops implement live-variable reconciliation after a conditional:
the statement-if's (`.ifElse`) grew the aggregate alloca path (bug 029) and
the pre-promotion rule (bug 031); the value-if's (`.ifExpr`) kept the
original phi-everything shape. The discard() desugar made the value-if path
reachable from a much wider class of programs than hand-written value-ifs.

## Note

This is the THIRD divergence between the two merge loops (029: arrays,
031: pre-promotion, 033: aggregate phi). They should be one function —
recorded as a refactor candidate for the Lower structured-builder arc
(ROADMAP #5 continuation), pulled the next time either loop changes.
