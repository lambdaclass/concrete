# Bug 044: Renamed import of a generic fn was never monomorphized (undefined symbol)

**Status:** Fixed (2026-07-18)
**Fixed in:** Mono.lean — `lookupFn` tries EVERY alias entry for a name
(one extra alias hop each) instead of first-match-only, and both generic
branches specialize under the RESOLVED def's canonical name
(`monoNameFor fnDef.name …`), so a renamed call shares the same
specialization as direct calls.
**Regression test:** `tests/programs/regress_044_renamed_generic_import/`
(project test, exit 0: grow/dealloc roundtrip through renamed aliases).
**Discovered:** 2026-07-18 dogfooding bug 043 (a scratch `dealloc as
io_dealloc` rename failed to link); queued as ROADMAP 1a defect #1, fixed
same day.

## Repro

```text
import std.alloc.{dealloc as dd};
dd::<u8>(p);
// llvm-as: error: use of undefined value '@dealloc'
```

Non-generic renames worked; only generic ones broke.

## Root cause

A renamed import contributes MULTIPLE alias entries in different
orientations — `("dd","dealloc")` from resolveImports and
`("dd","alloc_dealloc")` from the nested-path alias section — into one
flat list, while Mono's `fnMap` is keyed by bare def names. `lookupFn`
took the FIRST `("dd", X)` entry only; when that X was the
module-qualified spelling, `fnMap.get X` missed, `lookupFn` returned
`none`, and the call was "extern or unknown, leave as-is". EmitSSA's
alias resolution then rewrote `dd → dealloc` — the bare GENERIC name,
which is never emitted (only `alloc_dealloc_for_*` specializations
exist) → undefined symbol at llvm-as time. Fail-closed (link error), not
wrong-code.

Same identity-by-name family as bugs 039–041: a flat first-match name
table with entries of mixed orientation.

## Secondary hardening in the same fix

Specializations are now created under the canonical def name even when
reached through an alias — previously a successful alias resolution would
have created a duplicate `dd_for_u8` spec alongside `dealloc_for_u8`.
