# Capability facts — one source of truth

ROADMAP Phase 6.5 #4. Capabilities/effects are the second identity-defining
semantic axis of Concrete (after integer arithmetic, #1). This note records
where capability facts live so no stage re-derives them.

## The base primitives (already single-sourced)

`Frontend/AST.lean` owns the `CapSet` type and its structural operations:
`normalize`, `concreteCaps`, `isEmpty`, `expandAliases`, and the `stdCaps` /
`validCaps` name lists. These were never duplicated.

## The derived facts (centralized in Phase 6.5 #4)

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

## The next layer: `CapabilityJudgment` (planned)

The long-term capability axis should mirror `IntArith` and `TypeJudgment`: a
single compiler-internal decision record, not a new user-facing effect system.
Concrete's surface stays explicit and practical:

```con
fn read(path: Path) with(File) -> Result<Bytes, IOError>
fn apply<T, U, cap C>(f: fn(T) with(C) -> U, x: T) with(C) -> U
```

Do **not** add algebraic effects, effect handlers, row-polymorphism syntax,
implicit context, or theoretical effect terminology to the language surface.
The useful lesson from effect-polymorphic languages is only this: one stage
should decide why a computation needs authority, and every other consumer should
read that decision.

`CapabilityJudgment` should return a decision record, not just a `CapSet`:

```text
CapabilityDecision {
  required_caps
  source: direct_call | callback | trusted_wrapper | unsafe_intrinsic | package_import
  callee
  callback_param
  purity
  evidence_class
  diagnostic_reason
  report_payload
}
```

The exact Lean shape may differ, but the ownership rule should not: Check,
CoreCheck, Report, audit, LSP/agent JSON, and package gates consume the same
decision. They must not independently recompute why `File`, `Network`, `Alloc`,
`Unsafe`, or a capability variable is required.

Staged implementation:

1. **Direct calls.** Decide required caps for a normal function call once; use
   that decision for checker accept/reject, diagnostics, `--report caps`, and
   audit output.
2. **Callbacks/callable values.** Preserve the existing callable model. A
   callback typed `fn(T) with(C) -> U` makes the caller/combinator require `C`;
   `CapabilityJudgment` records that propagation and why it happened.
3. **Trusted/Unsafe/package boundaries.** Trusted wrappers, Unsafe intrinsics,
   extern functions, dependency capability budgets, and audit diffs all render
   from the same decision record.

The first gate should include a direct `File`/`Network` call and a red-team where
checker and report output would otherwise disagree. Later gates add a
capability-polymorphic callback, scoped callback, trusted wrapper, Unsafe
intrinsic, and dependency/package boundary.

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
