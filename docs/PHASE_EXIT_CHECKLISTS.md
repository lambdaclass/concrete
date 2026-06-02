# Phase Exit Checklists

Status: canonical reference

Each phase has a "phase closes when..." list tied to concrete outputs. A phase is not done until every exit criterion has a verifiable artifact. This prevents roadmap drift into never-finished thematic work.

## Phase 1: Predictable Core

**Closes when all of the following are true:**

- [ ] 5+ canonical examples are trust-gate tested. The validation core of each example passes `--check predictable`; an outer hosted `main` / shell may use I/O capabilities (e.g. Console, File) and fail predictable by design — that split is the point.
  - `fixed_capacity` (12 tests, fully predictable), `parse_validate` (10 tests, fully predictable), `service_errors` (10 tests, fully predictable), `thesis_demo` (8+ tests, predictable validation core + hosted shell), `packet` (4 tests, predictable validation core + hosted shell)
- [x] Predictable boundaries explicitly documented
  - `docs/PREDICTABLE_BOUNDARIES.md` (host calls, cleanup, determinism, failure, memory)
  - `docs/PREDICTABLE_FAILURE_DISCIPLINE.md` (allowed/excluded failure modes)
  - `docs/FAILURE_STRATEGY.md` (abort-only, defer, no-leak)
- [x] Stack-depth reporting works end-to-end
  - `--report stack-depth` shows per-function frame size, max call depth, worst-case bound
  - 25 trust-gate stack-depth tests pass
- [x] Bounded-capacity types are practical
  - Copy structs with fixed arrays compile and pass predictable
  - `fixed_capacity` example proves the pattern works
- [x] Error propagation patterns documented with examples
  - `parse_validate` (single-stage, 6 error categories)
  - `service_errors` (multi-stage, 3 error enums + unified ServiceError, all using builtin Result<T, E>)
- [x] Example governance in place
  - `docs/EXAMPLE_INVENTORY.md` (all 20 examples catalogued)
  - `docs/EXAMPLE_LIFECYCLE.md` (promotion levels defined)
  - `docs/EXAMPLE_NO_DUPLICATES.md` (reuse rule)
- [x] Diagnostic UX design documented
  - `docs/DIAGNOSTIC_UX.md` (quality tiers, target format, priority categories)
- [x] Trusted boundary guide written
  - `docs/TRUSTED_BOUNDARY_GUIDE.md` (4 wrapper patterns, audit checklist)
- [x] No-std / freestanding split defined
  - `docs/FREESTANDING_SPLIT.md`, `docs/STANDALONE_VS_PROJECT.md`, and `docs/PROJECT_BOOTSTRAP.md` define the intended boundary and workflows
- [x] Source-level interpreter exists for semantic oracle
  - Item 31 complete: `Concrete/Interp.lean`, CLI `--interp`, covers `parse_validate` (8/8 tests pass, matches compiled binary). 8 trust-gate interp tests.

**Verification command**: `./scripts/tests/run_tests.sh --trust-gate` passes with 0 failures.

**Current status**: 9/10 exit criteria done. Remaining blocker: expand trust-gated canonical-example coverage. Roadmap item count: 18/22 complete, 4 open (32-34, 39).

## Phase 2: Pre-Stdlib Pressure Workloads

**Closes when all of the following are true:**

- [x] Parser/decoder pressure set exists (JSON subset, DNS, HTTP, binary)
  - 5 parsers compile and run: json_subset, http_request, dns_header, dns_packet, binary_endian
  - Gaps: no string type, no byte cursor stdlib, no byte comparison, if/else chains for integer matching
- [x] Ownership-heavy structures tested (tree, arena graph, intrusive list)
  - 9 programs compile and run: tree, ordered_map, arena_graph, intrusive_list, nested/interleaved/helper/match linear patterns, destroy wrapper
  - Gaps: no recursive types (array-backed only), no generics, linear types require explicit destroy
- [x] Borrow/aliasing patterns documented
  - 5 programs compile and run: sequential_mut_ref, borrow_in_loop, borrow_then_consume, param_ref_multiuse, branch_create_consume
  - Gaps: no partial borrows, no iterator pattern (no closures/generics)
- [x] Trusted-wrapper / FFI pressure tested
  - 4 programs compile and run: ffi_libc_wrapper, ffi_checksum, ffi_os_facade, ffi_cabi
  - Gaps: no string passing to C, C struct interop requires manual layout
- [x] Fixed-capacity / no-alloc pressure extended
  - 4 programs compile and run: fixcap_ring_buffer, fixcap_bounded_queue, fixcap_state_machine, fixcap_controller
  - Gaps: no generics, no const generics, manual fixed-point arithmetic
- [x] Cleanup/leak boundary tested
  - 4 run + 5 error-expected programs: defer_nested, defer_in_loop, defer_with_borrow, heap_defer_free + 5 err_ programs correctly rejected
  - Gaps: manual defer ordering verification, no scope-guard abstraction
- [x] All gap findings documented in a "stdlib requirements" list
  - Each category documents gaps in ROADMAP.md items 45-50 and this checklist

**Verification**: all 36 pressure programs compile and run (or correctly fail for error-expected cases). Gap findings documented per category.

**Current status**: 7/7 items done. Phase 2 complete.

## Phase 3: Stdlib and Syntax Freeze

**Closes when all of the following are true:**

- [x] String/text contract defined (UTF-8, owned vs borrowed, no implicit conversions)
  - `docs/STRING_TEXT_CONTRACT.md` (98d2cbc)
- [x] String/text/bytes boundary is explicit at parser and FFI edges
  - Covered in STRING_TEXT_CONTRACT.md sections 3, 6, 10 (98d2cbc)
- [x] Checked indexing and slice/view contract stabilized
  - Contract defined in BYTE_CURSOR_API.md; std.numeric implemented (891d561, 9 tests); Slice/MutSlice checked `get` implemented
- [x] Core stdlib modules implemented: bytes, option/result, slices, basic collections
  - 38 modules exist in std/src/; audit in STDLIB_AUDIT.md; Tier 1 helpers added to option/result/bytes/math (98d2cbc)
- [x] Runtime-oriented collection maturity is demonstrated for interpreter/runtime-style workloads
  - Design frozen: `docs/RUNTIME_COLLECTIONS.md` (2026-04-20). Stdlib surface committed (Vec, HashMap, OrderedMap, OrderedSet, Set, Deque, Bytes, String); environments/intern pools/multimaps stay example-only.
  - Stdlib surface complete: `HashMap::get_mut` and `insert`-returning-`Option<V>` land in `std/src/map.con` (L-2 and L-3 closed); `OrderedMap::get_mut` added in parallel.
  - Runtime-heavy evidence accepted: `lox` runs end-to-end against the frozen surface. The remaining `HashMap<String, Value>` + `Vec<Frame>` rewrite is useful follow-up evidence, but not a Phase 3 freeze blocker.
- [x] Arithmetic policy is explicit in source, reports, and proof boundaries
  - `docs/ARITHMETIC_POLICY.md` (98d2cbc)
- [x] Formatting and text-output ergonomics are good enough for string-heavy real programs without hidden magic
  - Design frozen: `docs/FORMATTING_OUTPUT.md` (2026-04-20). `print` / `println` / `append(&mut buf, ...)` variadic triad; no format strings, no interpolation, no trait-driven formatting.
  - Shipped: variadic `append` desugar in Elab + Check intercept + `IntrinsicId.append`. Test: `tests/programs/variadic_append.con`. grep migration to variadic append is a follow-up cleanup (not freeze-blocking).
- [x] Error ergonomics settled (`?` operator, Result methods, conversion traits)
  - `docs/ERROR_HANDLING_DESIGN.md`; `?` already parsed/lowered; Tier 1 helpers added (98d2cbc)
- [x] Opaque validated wrapper types and fallible conversions settled
  - Design frozen: `docs/VALIDATED_WRAPPERS.md` (2026-04-20). `PascalCase` names, `try_new` / `try_from_<src>` / `.0`, no implicit coercion, module-local error enums.
  - Layout fix landed (2026-04-24): `Layout.Ctx` carries a newtypes list and resolves through `resolveNewtype` at the named/generic boundary, unblocking enum-payload newtypes (`Option<Port>`, `Option<AsciiText>`) on the native/SSA path.
  - Canonical stdlib wrappers shipped: `std.numeric.NonZeroU32`, `std.numeric.NonZeroU64`, `std.numeric.Port`, `std.text.AsciiText`. Demos: `tests/programs/newtype_validated.con` plus 3 `AsciiText` `#[test]` cases.
  - Cross-module newtype import works (2026-04-25): pub newtypes flow through `publicNames`, imported signatures preserve newtype identity (no inner-type erasure at the boundary), inherent impl methods come along, and Layout sees imported newtypes via the elaborated CModule. Demo: `tests/programs/adversarial_module_newtype_across.con` and `tests/programs/summary_import_pub_newtype.con`.
  - Instance-method dispatch on newtypes works (2026-04-25): `p.value()` on `p: Port` resolves against `Port`'s inherent impl. `resolveTypeE` no longer erases newtypes; the newtype constructor and `.0` field access carry the wrapper name through as a representation no-op cast. Demo: `tests/programs/newtype_method_dispatch.con`.
  - Cast-exemption tightened (2026-04-25): the wrap/unwrap rep cast is exempt from the cast-validity table only when the non-newtype side equals the newtype's resolved inner type. Arbitrary `as` casts to/from a newtype (e.g. `p as bool`, `b as Port`) are rejected at E0553. Negative regressions: `tests/programs/error_newtype_cast_to_unrelated.con`, `tests/programs/error_newtype_cast_from_unrelated.con`.
- [x] Enum/static qualification syntax is finalized and documented
  - `Type::Variant` / `Type::method(...)` is the only shipped qualification surface; `#` is gone from the compiler, formatter, stdlib, examples, and canonical docs
- [x] Remaining constructor/pattern ergonomics are settled
  - Field punning (`{ name }` = `{ name: name }`), `_` wildcard pattern, `let...else` destructuring, and irrefutable struct destructuring are implemented (42397c4). `{ .. }` rest-ignore is deferred — not needed for the canonical workloads.
- [x] Visibility rules stable (pub/non-pub at module and struct level)
  - `docs/VISIBILITY_AND_MODULE_HYGIENE.md` (98d2cbc); existing 3-pass enforcement confirmed adequate
- [x] Endian byte APIs exist (read_u16_be, write_u32_le, etc.)
  - `std/src/numeric.con`: ByteCursor with read_u8/u16/u32/u64 BE/LE, ByteWriter with write_u8/u16/u32 BE/LE (891d561)
- [x] Layout/ABI contract surface is explicit about stable vs opaque representations
  - Design frozen: `docs/LAYOUT_CONTRACT.md` (2026-04-20). Four stable repr forms (none/opaque, `#[repr(C)]`, `#[repr(packed)]`, `#[repr(align(N))]`); `#[repr(transparent)]` rejected; report fact set enumerated.
- [x] Module hygiene proven (no accidental namespace pollution)
  - `docs/VISIBILITY_AND_MODULE_HYGIENE.md` Part 2 (98d2cbc); selective imports enforced, no glob imports
- [x] Canonical examples use the intended stdlib surface rather than one-off local substitutes
  - `parse_validate`: uses builtin `Result<Header, ParseError>` (removed custom `ParseResult`)
  - `service_errors`: uses builtin `Result<T, E>` for all 4 stage results (removed `ValidateResult`, `AuthResult`, `RateResult`, `ServiceResult`)
  - `packet`: uses `std.numeric.ByteCursor` for all reads (removed hand-rolled `read_u8`, `read_u16_be`, `read_u32_be`)
- [x] One string-heavy medium workload and one interpreter/runtime-heavy medium workload validate the freeze surface
  - Ledger: `docs/STDLIB_FREEZE_LEDGER.md` (2026-04-20). String-heavy: `examples/grep` (207 lines, G-1..G-4). Runtime-heavy: `examples/lox` (1 183 lines, 48 fns, L-1..L-6). Each finding is classified (missing API / bad ergonomics / missing pattern / deferred / compiler gap) with a resolution path.
- [x] Phase 2 and Phase H stdlib findings are reconciled into a current requirements ledger with ship/defer decisions
  - `docs/STDLIB_AUDIT.md`, `docs/STDLIB_VALIDATION_PLAN.md` (98d2cbc)
- [x] Syntax and stdlib surface frozen — changes require explicit unfreezing
  - `docs/STDLIB_SURFACE_FREEZE.md` (98d2cbc)

**Verification**: `examples/parse_validate/` and `examples/service_errors/` work with stdlib types (not custom Copy enums), one fixed-capacity example uses the checked indexing/slice surface, one string-heavy medium workload such as `grep` or `policy_engine` uses the intended formatting/text APIs, and one interpreter/runtime-heavy workload such as `mal` or `lox` exercises the intended collection/runtime surface.

**Current status**: 19/19 exit criteria done. Phase 3 complete. The shipped stdlib/syntax surface is freeze-ready on today's evidence: `grep` and `lox` cover the medium-workload bar, runtime collections expose the intended `get_mut` / displaced-value `insert` API, and opaque wrappers are fully closed (layout fix + canonical stdlib wrappers + cross-module identity + instance-method dispatch + narrowed cast exemption). A canonical `lox` rewrite onto `HashMap<String, Value>` + `Vec<Frame>` remains useful follow-up evidence, but not a freeze blocker.

## Phase 4: Tooling, Tests, Wrong-Code Corpus

**Closes when all of the following are true:**

- [ ] Formatter exists (`concrete fmt`)
- [ ] Fuzzer exists and has found/fixed at least 5 bugs
- [ ] Wrong-code corpus has 10+ cases with expected vs actual behavior
- [ ] Test reducer/minimizer is functional
- [ ] Test coverage report exists

**Current status**: 0/12 items done.

## Phase 5: Performance, Artifacts, Contract

**Closes when all of the following are true:**

- [ ] Compile-time benchmarks exist with tracking
- [ ] Runtime benchmark suite exists (at least 5 programs)
- [ ] Artifact design documented (binary format, debug info, symbol tables)
- [ ] Backend contract explicit (what SSA guarantees, what optimizations are allowed)

**Current status**: 0/13 items done.

## Phase 6: Release Credibility, Showcase

**Closes when all of the following are true:**

- [ ] Public showcase corpus has 5+ compelling examples
- [ ] Website or landing page exists with honest claims
- [ ] At least one outsider has tried the language and provided feedback
- [ ] All public documentation reviewed for accuracy

**Current status**: 0/23 items done.

## Phase 7: Proof Expansion, Provable Subset

**Closes when all of the following are true:**

- [ ] ProofCore covers structs, pattern matching, and arrays
- [ ] At least 3 functions have Lean-verified proofs that the kernel accepts
- [ ] Proof workflow documented end-to-end (extract → prove → attach → verify)
- [ ] Proof boundary explicitly defined (what can and cannot be proved)

**Current status**: Phase 2 item 1 (proof workflow) done. Remaining items in Phase 7 not started.

## Later phases (8-14)

Exit checklists for phases 8-14 will be defined when earlier phases approach completion. Premature exit criteria for distant phases would drift as the language evolves.

## Updating this document

When a phase exit criterion is met, check the box and add the commit hash or document reference. When all boxes are checked, the phase is closed. Add a closing date and final trust-gate count.
