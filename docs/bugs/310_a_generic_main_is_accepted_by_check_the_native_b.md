# Bug 310 — a generic main is accepted by Check: the native build loses its entry point and --interp runs it

**Status:** OPEN
**Found:** 2026-10-28, by the class-13 spec audit (`wip/class13-audit`, `research/process/class13-audit-2026-10/`): one program per clause of each normative rule, native against `--interp`.
**Severity:** loud natively (a `link` error that names no rule), while `--interp` runs a program STMT-7 forbids: the two consumers disagree.

<!-- bug-ledger
compiler: legacy
summary: a generic main is accepted by Check: the native build loses its entry point and --interp runs it
coverage: archived reproducer(s) for bug 310; see Reproducer and Archive disposition below; no required passing regression against the frozen compiler
class: 13
silent: no
ruling: Violates: STMT-7@3
-->

## Reproducer

`tests/regressions/bug310_a_generic_main_is_accepted_by_check_the_native_b/repro.con`: `fn main<T>() -> Int { return 42; }`.

## Observed

On main ca49fada Check accepts the program. The native build fails at `link`: `error[link]: no `main` function found; an executable needs an entry point`, with no code and no rule. `--interp` runs the generic body and prints 42. `fn main<T: Copy>()` behaves the same.

## Expected

[STMT-7@2](../spec/statements.md#STMT-7): the entry point is a module-level `fn main()` with no parameters. A `main` with a type parameter is refused at check with E0310, on both consumers.

## Cause

`Concrete/Check/Check.lean`, `checkModule`'s `mainParamErrs`, refused only value parameters (bug 111). Nothing instantiates a generic `main`, so Mono dropped it and EmitSSA found no entry point, while the interpreter called the generic body directly.

## Structural cause

Class 13: a documented rule with no enforcing site for one of its clauses. STMT-7's site checked
the clause bug 111 hit (value parameters) and not the rest of the signature it names; no gate asked
for one negative program per clause. The class-13 audit found three more in the same pass (the
TYPE-11 destructure, SYN-3 destructure-binder and duplicate-member gaps).

## Structural owner

R-0499 (the Core checker as the one judgment of a well-formed program, entry signature included).
Until then, check_spec.sh S12 (`wip/spec-code-coverage`) maps every rule code to its enforcing
declarations and requires a negative witness for each.

## Historical branch containment (unlanded)

The archived filing branch attempted the following repair: Check refuses a `main` with type parameters (`CheckError.mainIsGeneric`,
E0310, `rule: STMT-7@2`). A capability parameter (`fn main<cap C>()`) is left as it is: both
consumers run it, and whether STMT-7 covers it is a question for the rule's owner, not this fix.

## Archived fix attempt

`mainParamErrs` refuses a `main` that declares a type parameter, after the value-parameter check,
with E0310 and its own message; STMT-7 gains the reject example `generic-main`.

Codex confirmation 2026-10-28: CONFIRMED — `mainParamErrs` ignores type parameters, allowing a generic entry point that native compilation loses while the interpreter executes it (loud natively, silent acceptance in --interp; OD: none).

## Archive disposition

Recorded against `legacy-compiler-final` (`066c9d9b8`); this defect remains open.
Source record: `refs/archive/branches/wip/bug-310`. Any repair or containment described above is an archived attempt, not a landed fix or current schedule.
Historical gate claims above describe the source branch; they do not register these fixtures in current gates. Expected results are the decided expected behaviour (or the explicitly stated spec gap), not required passes against the frozen compiler.

Preserved reproducer files:

- `tests/regressions/bug310_a_generic_main_is_accepted_by_check_the_native_b/error_310_generic_main_bounded.con`
- `tests/regressions/bug310_a_generic_main_is_accepted_by_check_the_native_b/error_310_submodule_generic_main.con`
- `tests/regressions/bug310_a_generic_main_is_accepted_by_check_the_native_b/repro.con`

The original unlanded fix attempt is preserved at `refs/archive/branches/wip/stmt7-generic-main`.

Current ruling: STMT-7@3 retains the no-type-parameters requirement; its later `u8`-only result requirement also rejects this historical `Int` reproducer. The observed entry-point divergence above is the original bug, not a claim that the program satisfies the rest of the current spec.
