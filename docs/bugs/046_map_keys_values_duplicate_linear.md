# Bug 046: HashMap keys()/values() and HashSet elements() duplicated linear payloads

**Status:** Fixed (2026-07-18)
**Fixed in:** std — `keys` moved behind `K: Copy`, `values` behind
`V: Copy`, `HashSet::elements` behind `K: Copy`. The scoped/borrowing APIs
(`keys_for_each`, `values_for_each`, folds, `with_value`) remain available
for linear element types. A consuming `into_keys(self)` (which must also
destroy the unreturned side) is pull-gated, not built speculatively.
**Regression test:** `tests/programs/error_046_map_values_linear/`
(negative project fixture — run_tests now asserts `error_*` projects FAIL
to build) + check_std_compiled_coverage.sh bug-046 legs (E0241 rejection
for linear payloads AND the Copy-typed positive roundtrip).
**Discovered:** 2026-07-18, external stdlib review; verified severe by
runtime repro before queueing.

## Repro (pre-fix: checker-accepted, runtime double-free)

```text
let mut m: HashMap<u64, String> = HashMap::new(hash_u64, eq_u64);
discard(m.insert(1, "linear-heap-value").is_none());
let vs: Vec<String> = m.values();   // copies *val_ptr — SECOND owner
vs.drop();                          // destroys the String
m.drop();                           // destroys it AGAIN → rc 133 (malloc abort)
```

## Root cause

`keys`/`values` lived in the UNCONSTRAINED `trusted impl<K, V>` block and
copied `*key_ptr`/`*val_ptr` into a fresh Vec while the map retained its
copy — two owners of one allocation for any non-Copy payload. The
`V: Copy` discipline already existed in the same file (the `get` reader
block); these two methods simply predated/escaped it, and
`HashSet::elements` inherited the defect through `inner.keys()`.
Depending on destruction order this is a double-free, use-after-free, or
aliased mutation — all checker-accepted. H18's keyed-remove/drop-glue
work covered destruction paths but not this extraction path.

## Why no workload hit it

Workloads keyed maps with Copy types (u64) or read via `get`/`for_each`;
`values()` over a linear payload had zero consumers outside std's own
Copy-typed tests — the same "public surface with no real consumer" class
the compiled-coverage gate hunts, here at the TYPE-INSTANTIATION axis the
per-module gate cannot see (its fixtures also used Copy payloads).

## Related (same review, verified separately)

- Manifest generator misparses signatures (single regex) — queued as the
  next slice; a fact gate over wrong facts is worse than no gate.
- `NonZeroU32(0)` constructs in SAFE cross-module code, bypassing
  `try_new` — smart-constructor invariants are advisory until struct/
  newtype construction rights exist. Pre-freeze language requirement,
  tracked in ROADMAP (pointer-bearing forgery is NOT possible in safe
  code — the Unsafe-gated cast barrier held under repro).
