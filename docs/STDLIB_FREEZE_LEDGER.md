# Stdlib Freeze Gap Ledger

Status: evidence log (freeze-ready close-out)

This document is the working ledger for ROADMAP item 67:

> require workload-backed stdlib freeze evidence beyond the tiny canon: at least one string-heavy medium workload and one interpreter/runtime-heavy medium workload should run against the intended stdlib surface, with explicit gap notes showing whether the remaining pain is missing APIs, bad ergonomics, missing patterns, or a deliberate deferral.

For the intended stdlib surface, see [STDLIB.md](STDLIB.md), [FORMATTING_OUTPUT.md](FORMATTING_OUTPUT.md), [RUNTIME_COLLECTIONS.md](RUNTIME_COLLECTIONS.md), [VALIDATED_WRAPPERS.md](VALIDATED_WRAPPERS.md), [LAYOUT_CONTRACT.md](LAYOUT_CONTRACT.md).

Each finding is classified as:
- **Missing API** — a needed function/type does not yet exist.
- **Bad ergonomics** — an API exists, but using it correctly is verbose or error-prone.
- **Missing pattern** — the convention is not established; examples roll their own.
- **Deferred** — deliberately out of scope for the first freeze.
- **Compiler gap** — the design is settled but the implementation has a bug or limitation.

---

## 1. String-Heavy Workload: `examples/grep`

- Size: 207 lines (`examples/grep/src/main.con`).
- Status: compiles, runs, matches literal strings across input lines, emits formatted match reports.
- Stdlib surfaces exercised: `std.string`, `std.fs`, `std.args`, `std.vec`, `print`/`println` variadic.

### Findings

| Tag | Classification | Finding | Resolution path |
|---|---|---|---|
| G-1 | Bad ergonomics | Every log line builds a `String` via 5–7 chained `out.append(&x); out.append_int(n); ...` calls. 11 call sites total (grepped `.append*(`/`println(`). | **Resolved** (2026-04-20): variadic `append(&mut buf, ...)` desugar implemented in Elab, with Check intercept and `IntrinsicId.append` wired. Test: `tests/programs/variadic_append.con`. grep migration remains as a follow-up cleanup (not freeze-blocking). |
| G-2 | Deferred | No format specifiers (padding, width, hex, leading-zero line numbers). grep does not need them today, but a reviewer would expect them for `ls`-style output. | Reconsideration trigger per [FORMATTING_OUTPUT.md](FORMATTING_OUTPUT.md) §6. Not blocking. |
| G-3 | Compiler gap | `String::append` returns `()` — no chaining possible without borrow-checker work. Doc [FORMATTING_OUTPUT.md §4](FORMATTING_OUTPUT.md) records the decision to not pursue chaining now. | Not a bug; a deliberate deferral documented. |
| G-4 | Missing pattern | grep has no local `format_error(msg: &mut String, ...)` helper; error reporting is inlined. [FORMATTING_OUTPUT.md §5](FORMATTING_OUTPUT.md) commits to the buffer-oriented shape, but no canonical example yet demonstrates it. | Add one when parser error reporting is next touched; do not backport as churn. |

### Verdict
String-heavy workload runs end-to-end on today's surface. The frozen direction ([FORMATTING_OUTPUT.md](FORMATTING_OUTPUT.md)) and the shipped variadic `append` desugar resolve the main ergonomic gap (G-1). Remaining work in this area is example cleanup or later extension, not Phase 3 freeze definition.

---

## 2. Interpreter/Runtime-Heavy Workload: `examples/lox`

- Size: 1 183 lines (`examples/lox/src/main.con`), 48 `fn` declarations.
- Status: compiles, runs, executes `test_comprehensive.lox` producing expected output (numbers, booleans, strings, nil).
- Stdlib surfaces exercised: `std.vec`, `std.string`, `std.fs`, `std.args`, manual binding tables.

### Findings

| Tag | Classification | Finding | Resolution path |
|---|---|---|---|
| L-1 | Missing pattern | lox rolls its own `Vec<NumEntry>` / `Vec<Binding>` / tag-indexed object store instead of `HashMap<String, Value>` + `Vec<Frame>`. [RUNTIME_COLLECTIONS.md §3.1](RUNTIME_COLLECTIONS.md) commits to the HashMap+frame shape as the canonical idiom. | lox predates the frozen direction. Rewriting it onto the canonical shape remains useful follow-up evidence, but the freeze decision no longer waits on it because the current drift is example-shape, not missing stdlib API. |
| L-2 | Resolved | `HashMap::get_mut(&mut self, key: &K) -> Option<&mut V>` landed in `std/src/map.con:66`, mirroring `get`. Required for in-place value mutation ([RUNTIME_COLLECTIONS.md §4](RUNTIME_COLLECTIONS.md) "Mutating lookup" row). | Closed. |
| L-3 | Resolved | `HashMap::insert` returns `Option<V>`: `None` on fresh insert, `Some { value: old }` when overwriting an existing key (`std/src/map.con:77`). | Closed. |
| L-4 | Deferred | No priority queue / heap type in stdlib. lox does not need one; no scheduler example forces it yet. | Deferred per [RUNTIME_COLLECTIONS.md §2](RUNTIME_COLLECTIONS.md) "Not in the stable surface". |
| L-5 | Missing pattern | lox has no `Env` / `Frame` struct factored out; scope handling is threaded through free functions (`lox_env_get`, etc.). | Example-shape, not stdlib. Promote to stdlib only if two unrelated examples arrive at the same shape ([RUNTIME_COLLECTIONS.md §6](RUNTIME_COLLECTIONS.md) promotion rules). |
| L-6 | Compiler gap | Large monolithic file (1 183 lines) suggests module boundaries are not ergonomic for runtime-heavy programs. Not a stdlib gap; a module-hygiene gap. | Tracked separately under [VISIBILITY_AND_MODULE_HYGIENE.md](VISIBILITY_AND_MODULE_HYGIENE.md). |

### Verdict
Interpreter workload runs end-to-end. The frozen direction ([RUNTIME_COLLECTIONS.md](RUNTIME_COLLECTIONS.md)) matches the shape a canonical lox *would* have, and `std.map` already exposes the frozen surface (L-2 and L-3 resolved). A canonical-shape lox rewrite (L-1) remains useful follow-up evidence, but it is no longer a freeze blocker because the remaining drift is example-shape rather than a missing stdlib API.

---

## 3. Summary Against the Freeze Checklist

| Domain | Design frozen | Implementation complete | Workload evidence |
|---|---|---|---|
| Formatting / print / append | Yes ([FORMATTING_OUTPUT.md](FORMATTING_OUTPUT.md)) | Complete — `print`/`println` and variadic `append` all wired | `grep` exercises existing surface; `tests/programs/variadic_append.con` covers the new desugar |
| Runtime collections | Yes ([RUNTIME_COLLECTIONS.md](RUNTIME_COLLECTIONS.md)) | Complete — `HashMap::get_mut` and `insert`-returning-`Option<V>` land in `std/src/map.con`; `OrderedMap::get_mut` added in parallel; no missing stdlib API blockers remain | `lox` runs end-to-end on the frozen surface; a canonical `HashMap<String, Value>` + `Vec<Frame>` rewrite remains optional follow-up evidence (L-1) |
| Validated wrappers | Yes ([VALIDATED_WRAPPERS.md](VALIDATED_WRAPPERS.md)) | Complete — native/SSA layout resolves enum-payload newtypes; canonical stdlib wrappers landed (`std.numeric.NonZeroU32`/`NonZeroU64`/`Port`, `std.text.AsciiText`); cross-module newtype identity preserved at the import boundary; instance-method dispatch resolves against the wrapper; CoreCheck cast-validity exempts only the wrap/unwrap rep cast. All four `docs/VALIDATED_WRAPPERS.md §8` gaps closed (no known compiler-level gaps remain) | `tests/programs/newtype_validated.con`, `AsciiText` stdlib tests, `newtype_method_dispatch.con`, `adversarial_module_newtype_across.con`, `error_newtype_cast_*` |
| Layout / ABI contract | Yes ([LAYOUT_CONTRACT.md](LAYOUT_CONTRACT.md)) | Complete as a Phase 3 surface freeze — the layout menu is frozen; richer report/artifact tagging is later hardening work | `pressure_ffi_*` programs exercise existing surface |

No design decision is blocked by a missing workload, and no compiler-level gaps remain against the stdlib surface defined in this ledger. The implementation tasks flagged here have all landed: `HashMap::get_mut` and `insert`-returning-`Option<V>` are in `std/src/map.con`; the newtype layout/codegen path landed 2026-04-24 (`Layout.Ctx.newtypes` resolves named/generic types at the layout boundary); cross-module newtype identity preserved 2026-04-25; instance-method dispatch + narrowed cast exemption 2026-04-25; canonical stdlib wrappers (`NonZeroU32`, `NonZeroU64`, `Port`, `AsciiText`) plus `OrderedMap::get_mut` shipped alongside; variadic `append` (G-1) landed 2026-04-20. Layout-report enrichment, text-boundary hardening, and other artifact-level follow-up now live outside the Phase 3 freeze gate.

---

## 4. Process

- New workload findings are added as numbered rows under the relevant example.
- When a finding is resolved, mark it resolved in place; do not delete it. The ledger is a historical record.
- When a new medium workload enters the pressure set, it gets its own section with the same `classification / finding / resolution` format.
- Each finding that triggers a design revision references back to the design doc being revised, so the revision can be audited.
