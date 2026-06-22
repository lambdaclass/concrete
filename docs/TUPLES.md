# Tuples — deliberate no-tuples decision (V1)

Status: DECIDED — no anonymous tuples in V1. ROADMAP Phase 6 #5 ("tuple types,
or a deliberate no-tuples decision (record which)"). Gated by
`scripts/tests/check_no_tuples.sh`.
Date: 2026-06-22

## Decision

Concrete V1 does **not** have anonymous tuple types, tuple literals, or
positional tuple indexing (`(i32, i32)`, `(1, 2)`, `t.0`-as-tuple-index). The
**named struct** is the one product type. A function that wants to return two
values returns a small `struct Copy` with named fields.

```
// Not Concrete:
fn divmod(a: i32, b: i32) -> (i32, i32) { (a / b, a % b) }

// Concrete — a named result struct:
struct Copy DivMod { quotient: i32, remainder: i32 }
fn divmod(a: i32, b: i32) -> DivMod { DivMod { quotient: a / b, remainder: a % b } }
```

## Why

- **Evidence and reports stay legible.** Field *names* flow into layout reports,
  obligation messages, backend contracts, and proofs. `result.remainder` is
  self-describing; `result.1` is not. Concrete's whole bet is visible evidence,
  and positional fields erase the names that make evidence readable.
- **One product type, not two.** Anonymous tuples and named structs overlap
  almost entirely; carrying both doubles the surface (patterns, layout, codegen,
  formatting, docs) for a convenience that named structs already cover.
- **Consistency with the rest of the language.** Concrete favors explicit, named
  constructs everywhere (no implicit conversions, named capabilities, named
  fields). Tuples are the odd anonymous-positional exception.
- **Workload-driven.** No current example or workload needs anonymous tuples;
  the few multiple-return cases are clearer as named result structs. Adding
  tuples now would be a symmetry-driven feature, not a workload-driven one
  (cf. the const-generics decision, `docs/CONST_GENERICS_V1.md`).

## What this means concretely

- Tuple **type** syntax (`(A, B)`), tuple **literals** (`(a, b)`), and tuple
  **indexing** are rejected at parse time (clean parse errors, not silent
  acceptance). `t.0` remains the representation-extraction path for **newtypes**
  only (`Port(u16)` → `port.0 : u16`), which is a named single-field wrapper, not
  a tuple.
- The replacement is always a `struct` (use `struct Copy` for a small by-value
  result).

## If the verdict ever flips

Anonymous tuples would only be reconsidered if a real workload shows named
structs are genuinely insufficient (e.g. heavy generic plumbing where naming
every intermediate pair is untenable). Until then this is a closed decision, not
an open hole — tuple syntax stays a clear "use a struct" parse error.
