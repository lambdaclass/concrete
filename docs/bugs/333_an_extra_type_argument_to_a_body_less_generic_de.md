# Bug 333 — An extra type argument to a body-less generic declaration is ignored natively and fails at run time under --interp

**Status:** OPEN
**Found:** 2026-10-30, by the Codex and Kimi reviews of the spec-gap rules (wip/spec-s6), native against `--interp` on main.
**Severity:** silent: `sizeof::<u8, u32>()` compiles and the binary exits 42, while `--interp` fails at run time with "sizeof without one type argument".

Codex confirmation 2026-10-30: CONFIRMED.

<!-- bug-ledger
summary: An extra type argument to a body-less generic declaration is ignored natively and fails at run time under --interp
coverage: reproducer tests/regressions/bug333_an_extra_type_argument_to_a_body_less_generic_de/repro.con, found by the spec-gap reviews 2026-10-30; reproduction pinned by check_example_outputs.sh (open-bug reproducers leg) and check_spec.sh S4 (TYPE-9's pending example)
class: 13
silent: yes
exit: 0b
ruling: Violates: TYPE-9@2
-->

## Reproducer

`tests/regressions/bug333_an_extra_type_argument_to_a_body_less_generic_de/repro.con`.

## Observed

`sizeof::<u8, u32>()` compiles and the binary exits 42, while `--interp` fails at run time with "sizeof without one type argument".

## Expected

[TYPE-9](../spec/types.md#TYPE-9): the call is refused at check (E0322), on both consumers.

## Cause

`Concrete/Frontend/Parser.lean` keeps no type-parameter list for a body-less declaration, `Concrete/Resolve/FileSummary.lean` marks it `typeParamsUnrecorded`, and `Concrete/Check/Check.lean`'s arity check skips such a callee; `Concrete/IR/Lower.lean` ignores the extra argument and `Concrete/Interp/Interp.lean` refuses it at run time.

## Structural cause

Class 13: a documented rule with an enforcing site that misses one form.

## Structural owner

R-0499 (the Core checker), which owns call arity for every callee, body-less or not.

## Containment while open

The reproducer is pinned by check_example_outputs.sh's open-bug reproducers leg, and the rule's
pending example records main's outcome.
