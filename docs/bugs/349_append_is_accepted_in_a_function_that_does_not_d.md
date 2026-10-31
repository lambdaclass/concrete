# Bug 349 — append is accepted in a function that does not declare Alloc, because Elab desugars it into string_append calls that carry no capability

**Status:** OPEN
**Found:** \1, by a hand probe of the builtins catalog of the spec's readability pass 2 (the `append` row), native against `--interp`; confirmed by Codex and Grok
**Severity:** silent under the decided rule: a program MEM-7 refuses is accepted, and both consumers agree on its result

<!-- bug-ledger
summary: append is accepted in a function that does not declare Alloc, because Elab desugars it into string_append calls that carry no capability
coverage: reproducer tests/regressions/bug349_append_is_accepted_in_a_function_that_does_not_d/repro.con, pinned by check_example_outputs.sh (open-bug reproducers leg, runs-exit 42)
class: 6
silent: yes
ruling: Violates: MEM-7@1
-->

## Reproducer

`tests/regressions/bug349_append_is_accepted_in_a_function_that_does_not_d/repro.con`: a function without capabilities that calls `append(&mut s, 7, "cd", true)`, called from a `main` without capabilities.

## Observed

Both consumers exit 42: no diagnostic, though neither function declares `Alloc`. A loop of 1000 `append` calls without `Alloc` also runs on both.

## Expected

[MEM-7@1](../spec/memory.md#MEM-7) (Status: enforced): `append` requires the `Alloc` capability, and a function that calls it without `Alloc` in its `with(...)` set is refused by TRUST-4 (E0520).

## Cause

`IntrinsicId.capability` (Concrete/Resolve/Intrinsic.lean) maps `.append` to `Alloc`, but Elab (`Concrete/Elab/Elab.lean`, the variadic `append` desugaring) rewrites the statement into calls of the runtime builtins `string_append`, `string_append_int`, `string_append_bool` and `string_push_char`, and CoreCheck looks a call's capability up by its callee, which for those is none. Check's `append` case checks the argument types only, so the requirement is checked nowhere.

## Structural cause

Class 6 ([STRUCTURAL_CLASSES.md](STRUCTURAL_CLASSES.md)): a capability fact attached to the source construct (`append`) is not carried across the desugaring that replaces it, so the checker after Elab sees only the callees, which carry none.

## Structural owner

R-0499 (S8 capability containment): capabilities are checked on Core with each desugared call carrying the capability of the construct it came from.

## Containment while open

`check_example_outputs.sh` (open-bug reproducers leg) pins the acceptance (exit 42). Meanwhile `--report caps` under-reports `Alloc` for such functions; whether the `string_append*` builtins themselves should need `Alloc` is a separate open owner question.
