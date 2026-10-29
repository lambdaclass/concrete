# Bug 193 — a struct or enum literal could give a field twice, and the consumers chose differently

<!-- bug-ledger
summary: a struct or enum literal could give a field twice (`P { n: 1, n: 41 }`); the checker type-checked the first occurrence, the binary stored the last (41) and `--interp` used the first (1). Silent. Found by the Codex review of TYPE-7; refused with E0306; pinned by `error_193_duplicate_{struct,variant}_field.con`
fixtures: error_193_duplicate_struct_field.con error_193_duplicate_variant_field.con
class: 7 — which occurrence of a repeated literal field counts
silent: yes
exit: 0b
ruling: Violates: TYPE-7@1
-->

**Status:** FIXED (branch `wip/spec-single-source`)
**Found:** 2026-10-23, by the Codex review of the specification's types chapter (TYPE-7 states
"each of its fields exactly once"), then probed on both consumers
**Severity:** silent wrong code in safe code. `P { n: 1, n: 41 }` compiled; the binary read 41 (the
last occurrence) and `--interp` read 1 (the first).

Confirmed real by Codex review, 2026-10-25: docs/spec/types.md TYPE-7 requires each struct field exactly once; duplicate fields bypassed checking and produced different native/interpreter values.

## Reproducer

```con pseudocode
struct Copy P { n: Int }
fn main() -> Int {
    let p: P = P { n: 1, n: 41 };
    return p.n;
}
```

Expected: refused (TYPE-7). Observed on `wip/spec-single-source` `bcd7a019`: the binary exits 41,
`--interp` prints 1. An enum variant literal (`Option::Some { value: 1, value: 2 }`) was accepted
too, but there both consumers used the first occurrence and silently dropped the second.

## Cause

`Concrete/Check/Check.lean`, `checkExprCore`, the `.structLit` and `.enumLit` cases: each declared
field is looked up in the literal with `fields.find?` (the first occurrence is type-checked and
ownership-checked), and the field list is only checked for unknown names, never for repeats. The
parser keeps duplicates. For a struct literal, lowering stored every occurrence in order, so the
last one won in the binary, while the interpreter evaluated every occurrence and then read the
first. For an enum literal, elaboration selected the first occurrence (`Concrete/Elab/Elab.lean`),
so both consumers agreed and the second occurrence vanished. Either way a linear value given in a
later occurrence was never checked.

## Fix

Both literal forms refuse a repeated field with the new code E0306 (`duplicateFieldInLiteral`,
TYPE-7). Regression fixtures: `error_193_duplicate_struct_field.con` and
`error_193_duplicate_variant_field.con` (in `run_tests.sh`), and TYPE-7's reject example
`duplicate-field` in `docs/spec/types.md`, which `check_spec.sh` runs on both consumers.

## Structural cause

Class 7, semantics implemented several times: the checker, elaboration, the interpreter and the
backend each decided which occurrence of a field counts, and for structs they disagreed. It is also class 13 (a documented
rule with no enforcing site): "each field once" was the evident rule of a struct literal, enforced
for patterns (E0304) but not for literals.

## Structural owner

R-0499 (S8: the Core checker owns the literal's well-formedness, and Core keeps one value per
field) and R-0496 (S7: the interpreter becomes the Core semantics, so there is one reading of a
literal). The specification gate's examples on both consumers are the containment.
