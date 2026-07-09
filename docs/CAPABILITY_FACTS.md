# Capability facts — one source of truth

ROADMAP Phase 6.5 #5. Capabilities/effects are the second identity-defining
semantic axis of Concrete (after integer arithmetic, #1). This note records
where capability facts live so no stage re-derives them.

## The base primitives (already single-sourced)

`Frontend/AST.lean` owns the `CapSet` type and its structural operations:
`normalize`, `concreteCaps`, `isEmpty`, `expandAliases`, and the `stdCaps` /
`validCaps` name lists. These were never duplicated.

## The derived facts (centralized in Phase 6.5 #5)

`Concrete/Semantics/Capabilities.lean` is the one place the *derived* capability
facts are defined:

| Fact | Meaning | Was scattered in |
| --- | --- | --- |
| `capsContain caller callee` | superset: does the caller's authority cover the callee's? (a cap variable satisfies anything) | `Resolve/Shared` (now re-exported from there) |
| `capsAllowUnsafeOp inTrusted cs` | authority to perform an `Unsafe` op — `trusted` OR the cap set covers `Unsafe` | open-coded 4× in `CoreCheck` |
| `capSetHasUnsafe cs` | does the set LITERALLY list `Unsafe`? (for report counting — a cap variable is not literal Unsafe) | `Report/ReportInterface.hasUnsafeCap` |
| `externFnRequiredCaps isTrusted` | an untrusted `extern fn` requires `Unsafe`; a trusted one requires nothing | recomputed in `CoreCheck` + twice in `ReportBase` |

The two Unsafe questions are deliberately distinct and must not be conflated:
**authority** (`capsAllowUnsafeOp`, handles `trusted` and cap variables) drives
the CoreCheck raw-pointer/unsafe-cast gates; **literal membership**
(`capSetHasUnsafe`) drives report counts. A cap-polymorphic function has
authority (its variable satisfies) but does not *literally* list Unsafe.

## Why it matters

Every stage now reads the same fact, so the capability answer a diagnostic
gives and the one a report/audit renders are the same by construction — not two
implementations kept in agreement by luck. The duplicated extern-cap fact was
the concrete risk: `CoreCheck` deciding an `extern fn` needs `Unsafe` while a
report's cap-lookup builder computed it separately meant a drift there would
make `--report`/audit disagree with the checker's diagnostic. Now there is one
`externFnRequiredCaps`.

## Rendering

Capability *rendering* is intentionally NOT collapsed: `Frontend/Format`
produces source syntax (`with(File, Network)`) for `concrete fmt`, while
`Report/ReportBase.ppCapSet` produces prose (`File, Network` / `(pure)`) for
human reports. These serve different audiences and read the same underlying
`CapSet` — the *fact* is shared even though the surface text differs.

## Enforcement

`scripts/tests/check_capability_facts.sh` gates the identity-defining cases:
Unsafe-op-without-authority rejected at CoreCheck, authority via `trusted` and
`with(Unsafe)`, the untrusted-extern-requires-Unsafe fact, and the
report⇔checker agreement negative (the function the checker accepts as Unsafe is
exactly the one the report lists; a pure function is listed by neither).
