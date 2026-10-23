# Bug 193 — a struct or enum literal could give a field twice, and the consumers chose differently

**Status:** FIXED (branch `wip/spec-single-source`)
**Found:** 2026-10-23, by the Codex review of the specification's types chapter (TYPE-7 states
"each of its fields exactly once"), then probed on both consumers
**Severity:** silent wrong code in safe code. `P { n: 1, n: 41 }` compiled; the binary read 41 (the
last occurrence) and `--interp` read 1 (the first).

## Reproducer

```con pseudocode
struct Copy P { n: Int }
fn main() -> Int {
    let p: P = P { n: 1, n: 41 };
    return p.n;
}
```

Expected: refused (TYPE-7). Observed on `wip/spec-single-source` `bcd7a019`: the binary exits 41,
`--interp` prints 1. An enum variant literal (`Option::Some { value: 1, value: 2 }`) took the same
path.

## Cause

`Concrete/Check/Check.lean`, `checkExprCore`, the `.structLit` and `.enumLit` cases: each declared
field is looked up in the literal with `fields.find?` (the first occurrence is type-checked and
ownership-checked), and the field list is only checked for unknown names, never for repeats. The
parser keeps duplicates. Elaboration and lowering then stored every occurrence in order, so the last
one won in the binary, while the interpreter built the value from the first. A linear value given
in the second occurrence was never checked at all.

## Fix

Both literal forms refuse a repeated field with the new code E0306 (`duplicateFieldInLiteral`,
TYPE-7). Regression fixtures: `error_193_duplicate_struct_field.con` and
`error_193_duplicate_variant_field.con` (in `run_tests.sh`), and TYPE-7's reject example
`duplicate-field` in `docs/spec/types.md`, which `check_spec.sh` runs on both consumers.

## Structural cause

Class 7, semantics implemented several times: the checker, the interpreter and the backend each
decided which occurrence of a field counts, and they disagreed. It is also class 13 (a documented
rule with no enforcing site): "each field once" was the evident rule of a struct literal, enforced
for patterns (E0304) but not for literals.

## Structural owner

R-0499 (S8: the Core checker owns the literal's well-formedness, and Core keeps one value per
field) and R-0496 (S7: the interpreter becomes the Core semantics, so there is one reading of a
literal). The specification gate's examples on both consumers are the containment.
