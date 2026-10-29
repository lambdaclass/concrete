# Bug 005: Enum Fields Inside Structs Can Panic Layout Computation

<!-- bug-ledger
summary: Enum Fields Inside Structs Can Panic Layout Computation
fixtures: bug_enum_in_struct.con
ruling: Spec gap: PASS — a program every rule accepts is compiled: no stage after Check refuses it, crashes on it or emits invalid LLVM IR for it
-->

**Status:** Fixed
**Discovered:** 2026-03-15
**Discovered in:** `examples/policy_engine/main.con` (early Phase H)
**Regression test:** `lean_tests/bug_enum_in_struct.con`

## Symptom

The compiler could panic while lowering or laying out a struct that contains enum-typed fields, especially when that struct was stored inside `Vec`-style collections or otherwise forced through layout computation.

The immediate trigger was a `Rule` struct containing `Action` and `Verdict` enum fields. `Layout.tyAlign` could not compute alignment for the enum field inside the named struct and crashed instead of producing a normal compiler error or a correct layout.

## Reproduction Shape

```con
enum Copy Color {
    Red {},
    Green {},
    Blue {},
}

struct Copy Rule {
    color: Color,
    priority: i32,
}

// This shape, stored in Vec<Rule>, triggered the layout panic.
```

## Fix

Enum fields in structs now work correctly, including in `Vec<Rule>` and similar containers. The layout engine handles enum alignment properly.

## Regression Test

`lean_tests/bug_enum_in_struct.con` covers:
- Struct with multiple enum fields (`Color`, `Verdict`) plus `i32`
- Pattern matching on enum fields via helper functions
- `Vec<Rule>` push/get roundtrip — the collection-oriented pressure that originally triggered the bug
