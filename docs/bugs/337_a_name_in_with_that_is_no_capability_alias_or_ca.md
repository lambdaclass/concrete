# Bug 337 — a name in with(...) that is no capability, alias or capability variable is accepted

**Status:** OPEN
**Found:** 2026-10-30, by absorbing CALLABLE_VALUES_AND_CAPABILITIES and HANDLE_CAPABILITIES into the specification (wip/spec-absorb-3): a probe of names in `with(...)` that are no capability, native against `--interp`, on main cc876322.
**Severity:** silent: a program MOD-1 refuses is accepted, and a misspelt capability (`with(Consle)`) is a capability of its own that grants nothing and is never reported.

<!-- bug-ledger
compiler: legacy
summary: a name in with(...) that is no capability, alias or capability variable is accepted
coverage: archived reproducer(s) for bug 337; see Reproducer and Archive disposition below; no required passing regression against the frozen compiler
class: 13
silent: yes
ruling: Violates: MOD-1@1
-->

## Reproducer

`tests/regressions/bug337_a_name_in_with_that_is_no_capability_alias_or_ca/repro.con`: two functions that declare `with(Foo)`, where `Foo` is no capability, alias or capability variable.

Codex confirmation 2026-10-30: CONFIRMED ("violates MOD-1@1: `Foo` in `with(Foo)` is a used name with no visible
declaration, yet the compiler preserves it as a concrete capability instead of refusing it"), by source inspection
of the sites below.

## Observed

On main cc876322 (built on OVH): the reproducer compiles, the binary exits 42 and `--interp` prints 42. The control
`let x: Foo = 1;` is refused: `error[resolve]: (E0108) unknown type 'Foo'`, tagged MOD-1. A call that needs `Foo`
from a caller that does not list it is refused only by containment (E0520), so a consistent misspelling passes.

## Expected

[MOD-1](../spec/modules.md#MOD-1): every name a program uses resolves to exactly one visible declaration (a builtin
included), and a name that resolves to none is refused. A name in `with(...)` is a used name; the capabilities are
builtins, `cap` aliases and capability parameters are declarations, and `Foo` is none of them.

## Cause

`Concrete/Frontend/Parser.lean`, `parseWithCaps`: collects identifiers, expands `Std`, and returns them as concrete
capabilities (its "Validate" comment has no check); `resolveCapVars` turns declared capability parameters into
variables and leaves every other name concrete. `Concrete/Resolve/Resolve.lean`, `resolveFnBody`: validates parameter
and return types but never `f.capSet`, and function-pointer types ignore their capability set.
`Concrete/Semantics/Capabilities.lean`, `missingCaps`: containment compares spellings, so `{Foo}` covers `{Foo}`.
Only a `cap` alias's right-hand side is checked against `validCaps`, and the call-site `with(Alloc = e)` form.

## Structural cause

Class 13, a documented rule with no enforcing site: MOD-1 covers every used name, but Resolve resolves the names of
types, values and modules and not capability names, which stay strings compared by spelling (class 1's
spelled-name identity, for capabilities).

## Structural owner

Resolve (`Concrete.Pipeline.resolve`): capability references in every signature and nested type resolve against
the visible aliases, the builtin capabilities and the capability parameters in scope, before containment; R-0492
(identities from Resolve on) makes them identities rather than strings.

## Historical branch containment (unlanded)

The reproducer is pinned by check_example_outputs.sh's open-bug reproducers leg (runs-exit 42). Meanwhile a
misspelt capability is caught only where a caller's set lacks it; `--report caps` shows each function's set, so a
review of it finds names that are no capability.

## Archive disposition

Recorded against `legacy-compiler-final` (`066c9d9b8`); this defect remains open.
Source record: `refs/archive/branches/wip/bug-unknown-capability-name`. Any repair or containment described above is an archived attempt, not a landed fix or current schedule.
Historical gate claims above describe the source branch; they do not register these fixtures in current gates. Expected results are the decided expected behaviour (or the explicitly stated spec gap), not required passes against the frozen compiler.

Preserved reproducer files:

- `tests/regressions/bug337_a_name_in_with_that_is_no_capability_alias_or_ca/repro.con`
