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

```con pseudocode
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

**Observed pull (R-0484, 2026-10-03).** Check (E0240, per capability) and CoreCheck
(E0520, whole set) each judge call capabilities, and during R-0484 they disagreed about
who owns `Unsafe` in a trusted body; the fix had to be made in both. `decideCall` already
single-sources the predicate. What is still duplicated is the caller authority each pass
feeds it: inference bindings, trusted discharge, cross-package `Unsafe`. The fix is to
share those semantic rules, not to make CoreCheck trust Check's verdict. CoreCheck checks
a different compiler boundary (elaborated, cross-module Core) and can catch
transformation defects, so it keeps validating independently wherever an invariant could
change between the two.

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

## Where the judgment actually binds (2026-09-20)

Two facts about SCOPE, recorded because both were wrong and one was invisible.

`capsContain` **normalizes the caller before testing membership.** It previously
asked whether either side of a union covered the whole requirement, so
`Alloc ∪ Unsafe` did not cover a callee requiring `Alloc, Unsafe`. That made it
disagree with `missingCaps`, which has always normalized, and `decideCall` could
report `satisfied := false` beside `missing := []` — the self-refuting *"requires
Alloc, Unsafe but caller has Alloc + Unsafe"*. One cap set, two readings, in the
module this note exists to prevent that in.

**CoreCheck's signature table spans the whole module tree, not one module.** It
was built from a single module's own functions, so a call into a sibling
submodule found no entry and the `none` was read as "requires nothing". A
capability-free, non-`trusted` function could call a `with(Console)` sibling and
print. Fixed by `collectAllFnSigs`, which records submodule externs under both
the bare and prefixed spellings, since `prefixModuleFnNames` renames functions
but deliberately leaves externs alone. Gated by
`scripts/tests/check_cap_sibling_module.sh`.

**Cross-package calls bind, and the CALL FORMS are enumerated deliberately** — a
headline that did not name them is how the last one stayed open for two rounds:

| form | example | gated by |
|---|---|---|
| imported free function | `std.env.get` | `check_cross_package_caps.sh` |
| explicitly imported receiver method | `RawCursor::read_u8` | `check_cross_package_caps.sh` |
| associated call | `TcpStream::connect` | `check_cross_package_caps.sh` |
| **prelude receiver method** | `String::drop` | `check_cap_sibling_module.sh` |

The prelude form was the last escape. `CModule.importedFnCaps` was built by walking
`m.imports`, and `String` needs no import statement, so its methods never entered
the table — `String::drop` is `with(Alloc)` and a function declaring nothing could
call it. Two gates were green at once, one of them explicitly asserting that
acceptance as a KNOWN HOLE, which is why CI could not tell the difference. Closed
by collecting dependency `implMethodSigs` recursively through `submoduleSummaries`
and keying them by the mangled `<Type>_<method>` spelling the call site emits.

**Superseded 2026-10-01 (R-0484): `trusted` now absorbs an extern call's `Unsafe`**,
as it already absorbed any other callee's `Unsafe` obligation. It absorbs nothing
else: every `extern` declares its effects (`extern fn write(..) with(Console)`,
`with()` for none; undeclared is E0116), `externFnRequiredCaps isTrusted declared`
returns those effects plus `Unsafe` unless the binding is `trusted extern`, and the
declared effects bind every caller, trusted bodies included. The previous rule (an
extern call inside a `trusted` wrapper needed `with(Unsafe)`, pinned by
`error_trusted_extern_needs_unsafe.con` / `error_trusted_no_extern.con`) pushed every
effectful C function into `trusted extern`, callable with no capability at all — the
`console_write` hole. Fixtures now: `trusted_absorbs_extern_unsafe.con`,
`error_trusted_extern_effect_not_absorbed.con`. Design: `HANDLE_CAPABILITIES.md` R2–R3.

## Per-method `trusted`, and the two safety axes (2026-09-25)

`trusted` was only a whole-impl modifier, so the commonest shape in a systems
standard library — **safe to call, audited implementation** — was not expressible
for a method. Licensing a method's raw work meant marking the whole `impl`
trusted, vouching for every method in it. Measured: **67 of 154** public `std`
signatures declaring `Unsafe` perform raw operations in their own body, so this
was most of the library, not a corner.

Two INDEPENDENT axes, neither implying the other:

| declaration | implementation | caller |
|---|---|---|
| `pub fn f()` | checked | owes nothing |
| `pub trusted fn f()` | audited | owes nothing |
| `pub fn f() with(Unsafe)` | checked under the assumption | owes an invariant |
| `pub trusted fn f() with(Unsafe)` | audited | owes an invariant |

- `trusted` — the IMPLEMENTATION is audited; it licenses raw operations in the body.
- `with(Unsafe)` — the CALLER owes an invariant the language cannot establish.

**Trust never erases operational authority.** A `trusted` method declaring
`with(Console)` is still refused to a caller holding none. Trust is about memory
discipline, not permission to reach a sink.

**Trust licenses a raw operation; it does not hide it.** `--report trust-edges`
records `calls-trusted`, `calls-ffi`, `contains-raw-op` and `assumes-unsafe` as
direct edges from checked Core, emitted at the same sites that gate the
operations, so a report over them cannot disagree with the checker. Paths and
closure are derived by consumers, never stored — a stored closure is a second
producer of what the call graph already determines. Today 292 public `std`
functions reach a raw root while 154 declare `Unsafe`: the declarations are
already an incomplete provenance system, off by 138, which is the drift the edges
exist to stop.

Gated by `scripts/tests/check_trusted_method.sh`. The migration experiment that
motivated this — and the measurements behind it — is in
[TWO_AXIS_SAFETY.md](TWO_AXIS_SAFETY.md).

## Enforcement

`scripts/tests/check_capability_facts.sh` gates the identity-defining cases:
Unsafe-op-without-authority rejected at CoreCheck, authority via `trusted` and
`with(Unsafe)`, the untrusted-extern-requires-Unsafe fact, and the
report⇔checker agreement negative (the function the checker accepts as Unsafe is
exactly the one the report lists; a pure function is listed by neither).
