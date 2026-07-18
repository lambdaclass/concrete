# Concrete Roadmap

This document is the active execution plan. It answers one question: **what
should happen next, in what order?** Historical phase detail lives in
[CHANGELOG.md](CHANGELOG.md); this file keeps only active work, future-relevant
constraints, and deferred tails with a real pull trigger.

## How To Read This Roadmap

### Active Work vs Historical Record

- **Active roadmap:** future work and pull-gated deferred work that can still
  affect Phase 7+ execution.
- **Historical record:** completed Phase 1-6 item bodies, implementation notes,
  and detailed gate transcripts live in [CHANGELOG.md](CHANGELOG.md) and the
  linked design docs.
- **Done items:** do not remain here as full task text. At most, this roadmap
  keeps a short closed marker or a dependency note.

### Definition Of Done

A roadmap item is done only when its behavior is protected by checked evidence:
positive fixtures, negative fixtures, regression fixtures, and a red-team or
mutation-style guard for the failure mode that would be most damaging. If a task
cannot yet have that gate, the item must say why and name the trigger that makes
it testable.

**When a fix addresses a bug CLASS, "done" means a gate that catches every
instance of the class, not only the reported one.** This project has repeatedly
fixed one instance — a decimal parser while `hex`/`bin`/`oct` still trapped, one
snapshot while sibling ledgers still drifted, one fixture main while the corpus
still failed — and had the siblings resurface a CI run later. The fix that holds
is a class-level gate (a grep/lint over the shape, or a corpus check), not a
per-instance patch.

**Large semantic migrations land in a worktree/branch and merge green — never
incrementally on `main`.** A change to a cross-cutting contract (e.g. the
main-return / exit-code model across the whole fixture corpus) done commit-by-
commit on `main` has produced multi-commit red streaks; isolate it, finish it to
green, and merge once.

### Pull-Gated Work

Deferred work stays in the phase where its trigger lives. Do not build machinery
because the roadmap can imagine it; build it when a workload, proof, failing gate,
or public API forces it.

### Current Frontier

The current frontier is **Phase 7: Standard Library And Core APIs**. Phases 1-6E
are closed as active phases. Phase 7's foundation work has landed: the public
stdlib manifest/API facts, `Option`/`Result` helpers, Reader/Writer IO spine,
bytes/text/path boundaries, std.test basics, ordered traversal, base64 workload,
H18 owned-resource collection drop glue, and the first real CLI workload are in
the historical record. The active frontier is the remaining Phase 7 surface:
MAIN_EXIT_MODEL stage 2, stdlib CLI/env/process helpers, unsafe/trusted wrappers
and trap/debug UX, the shipped pure-core proof arc, and pull-gated breadth after
workloads ask for it.

## Closed Foundations: Phases 1-6

### Summary

Phases 1-6 built the proof/evidence core, ordinary compiler pipeline, language
core, daily workflow, compiler-pipeline hardening, observability, grammar cleanup,
and CLI coherence needed before stdlib expansion. Their detailed task bodies have
been moved to [CHANGELOG.md](CHANGELOG.md).

### Historical Record

- Phase 1-2 proof/evidence foundations: see [CHANGELOG.md](CHANGELOG.md).
- Phase 3 obligation/core audit: see
  [docs/PHASE3_OBLIGATION_CORE_AUDIT.md](docs/PHASE3_OBLIGATION_CORE_AUDIT.md).
- Phase 4 compiler ledger audit: see
  [docs/PHASE4_COMPILER_LEDGER_AUDIT.md](docs/PHASE4_COMPILER_LEDGER_AUDIT.md).
- Phase 5 language/core slab and Phase 6/6B/6C/6D/6E completion records: see
  [CHANGELOG.md](CHANGELOG.md).
- Known holes index: [docs/KNOWN_HOLES.md](docs/KNOWN_HOLES.md). Its open
  section is currently empty; remaining work below is hardening, usability, or
  pull-gated extension work rather than known open soundness debt.

### Remaining Pull-Gated Tails From Phases 1-6

Keep these visible because they can affect future phases:

- **6C V1 hardening:** add remaining telemetry fields, pass hashes, and
  incremental-shadow manifest/edit coverage when a workload needs them.
- **ProofCore partial-def reduction:** moved to Phase 14, pulled only by
  preservation proofs.
- **CompilerDB / interned fact store:** moved to Phase 8.5, pulled by the real
  incremental driver and relational fact consumers.
- **Consume-then-break linear restoration:** after value `while...else` removal,
  consume-then-break on an outer linear remains rejected; add a narrow exemption
  only if a workload needs that control-flow shape.
- **Qualified names/import aliases, FFI surface, target-profile roots, target
  constants, lint/vet, doc, bench, trace/debug, eval/inspect, LSP:** future
  usability/tooling work should be pulled by Phase 7+ workloads or the later
  phase that owns the corresponding user surface.
- **Build-output convention:** move generated binaries/IR out of source dirs when
  the next build-UX touch makes it worthwhile.

## Cross-Cutting Decisions

### Linear Queue And Evidence Discipline

The roadmap is linear. Phases are ordered, and items inside a phase are ordered
unless explicitly marked as a constraint or deferred research note. There are no
parallel tracks. Completed work moves to [CHANGELOG.md](CHANGELOG.md); conditional
work moves to the phase where its trigger lives.

Every user-facing claim should have a replay command, checked report, gate, or
proof. Unsupported cases must fail closed and loud. A feature that passes only
because no one tried to break its gate is not done.

### No Semantically Dark Constructs

Every construct is `proved`, `enforced`, `reported`, `assumed`, or `trusted`.
Authority, allocation, trust, runtime failure, byte/text/path boundaries, and
proof class must remain visible in source or audit output. Do not hide them
behind inference-heavy abstractions, implicit conversions, ambient lookup,
metaprogramming, or broad convenience APIs.

### Capability And Authority Visibility

Capabilities are part of the language's audit story. Imports do not grant
authority. Trusted/Unsafe internals may exist, but public wrappers must expose
the domain capability (`File`, `Env`, `Time`, `Network`, `Process`, `Console`,
`Alloc`, `Unsafe`, etc.) in signatures and reports.

### Allocation Visibility

`with(Alloc)` is allocation authority; allocator values name allocator identity.
The Phase 7 allocator decision is two-tier: tier-1 defaults remain simple,
while allocator-specific `*_in` APIs use explicit allocator values when they are
pulled. Do not add an ambient implicit allocator.

### Scoped References, Not User-Managed Lifetimes

Safe references are second-class scoped access paths, not ordinary returned or
stored values. They may flow down into calls, callbacks, and borrow blocks, but
safe APIs should not return `&T` / `&mut T` or hide lifetime relationships in
data structures. This is the Hylo/Val-style value-semantics choice Concrete
adopts: users get in-place mutation, while the compiler manages the short-lived
borrow reasoning locally instead of exposing a Rust-style lifetime language.

API consequence: prefer operation APIs, owned values/views, handles/indices, or
scoped callbacks such as `with_value` / `with_value_mut`. Returning safe
references stays deeply deferred and evidence-gated; raw pointers are the
trusted/Unsafe escape hatch when low-level code must return an address.

### Compiler Pipeline Spine

Phase 6B established semantic truth and validation records; Phase 6C made the
pipeline observable/replayable; Phase 8 validates the bet with real examples;
Phase 8.5 turns the shadow graph into a persistent incremental driver only after
that validation. Later proof/backend/tooling work must not bypass this order:
semantic facts first, observability second, external validation third,
incremental reuse fourth, independently checked evidence after that.

### Incremental / Certificate Trust Split

Cache hits are optimizations, not proofs, and cannot upgrade evidence classes.
Phase 8.5 starts as conservative cache reuse over successful artifacts. Phase 14
adds an independent Core certificate checker for named predicates; Phase 15
extends the statement only through supported backend-translation slices. Parser,
resolver, unproved source-to-Core correspondence, LLVM, linking, runtime, OS,
and hardware remain trusted until separately checked.

### External-Validation Gate

The back half of the roadmap depends on the bet that evidence-carrying source is
worth the discipline. Phase 8 supplies the first serious external-facing
workloads; the external-validation gate decides whether the project proceeds
with Phase 8.5 and the larger Phase 11-19 investment as planned, narrows the
bet, or redesigns the discipline.

### Early Risk Probes

Four verdict-deciding risks get narrow early probes:

- **External credibility:** deterministic `diff-caps` style authority-diff
  artifact before the full Phase 8 trial.
- **Proof cost:** Phase 9 proof-effort telemetry on the first real proof slice.
- **Backend soundness:** narrow Phase 15 translation-validation slice over a
  small supported subset.
- **Pipeline scale:** Phase 6C/Phase 8 shadow invalidation transcript before real
  artifact reuse.

### Floats And Proof Profiles

`f32`/`f64` are usable runtime types but outside `ProvableV1` unless a future
explicit float profile is selected. Float proofs require named IEEE semantics,
backend fast-math restrictions, and an honest trusted/checked primitive layer.

### Embedded / Freestanding Direction

Embedded and freestanding support remain later-phase work. Inline asm is
trusted/Unsafe; MMIO/volatile access needs explicit capabilities; `Device` is
reserved for real freestanding/MMIO code and should not be added before a
consumer exists.

### Target-Conditional Code

Prefer target/profile-selected source roots and modules in `Concrete.toml` over
in-source conditionals. If narrow `cfg` attributes are ever admitted, they must
be audit-visible target/profile facts, not hidden preprocessor state.

### Native Debug Info

DWARF/source-mapped crash traces matter once Concrete emits real binaries. This
is backend/tooling work owned by Phase 15+, not Phase 7 stdlib work.

### Contract / Spec Trust

Specs and contracts carry evidence classes too. The system must avoid vacuous
proofs, impossible preconditions, partial ghost/spec functions, and generic
proofs whose scope is unclear. Proof evidence is relative to the toolchain that
checked it and must detect drift.

### Stdlib Direction

The stdlib should be small, explicit, and auditable: Gleam/Roc-sized coherence,
Hare-style restrained systems modules, Zig-style allocator/authority
explicitness, Rust-style `Option`/`Result` and layering where useful, Ada/SPARK
evidence discipline, Go-style `Reader`/`Writer` simplicity, and Odin-style
pragmatic breadth only after workload pressure.

## Phase 7: Standard Library And Core APIs

Goal: build the small standard library people need before real workloads,
packages, editor tooling, freestanding targets, and release work can be honest.

Done when: a normal C/Rust-style Concrete program can use documented core APIs
for errors, bytes/text/path, collections, formatting/parsing, I/O capabilities,
tests, and oracle helpers, with every public stdlib item carrying an evidence
class and authority/allocation story.

**Excellence exit contract.** Phase 7 is judged on *honesty, not breadth* — the
one stdlib where every public API's authority, allocation, failure, ownership,
and evidence are visible and gated, over a proven pure core. Breadth is copied
only after that holds. Four properties gate the phase, each backed by a check,
not prose:

1. **Capability-complete.** Every hosted operation carries its exact domain
   capability — `fs`→`File`, `env`→`Env`, `time`→`Time`, `process`→`Process`,
   `net`→`Network`, `console`/`io`→`Console`. `trusted` internals may use
   `Unsafe`, but the public wrapper must re-expose the real cap (a `trusted`
   extern requires *no* capability, so authority collapses into `Unsafe` unless
   re-added by hand). The stdlib manifest gate fails if a hosted op is behind
   `Unsafe` alone.
2. **One IO spine.** All output targets a single `io.Writer` (fn-pointer handle,
   authority at acquisition, no `dyn`), and there is one `File` type: `fmt`,
   files, console, `std.test` output, logs/progress, and future sockets are all
   `Writer` producers/consumers. No second sink, no duplicate `Writer`/`File`.
3. **Linearity-real.** Collections destroy live non-`Copy` elements on
   `drop`/`clear`/`remove`/overwrite (not merely reclaim the buffer); no hidden
   `Clone`, no implicit move-out from indexed containers; deterministic map/set
   traversal. Until non-`Copy` element drop is wired, the limitation is recorded,
   not silent.
4. **Self-enforcing + selectively proven.** The five-fact manifest (allocate / consume /
   fail / capability / proof-class), derived from compiler facts and gated
   (the stdlib manifest gate); the strict error rule (`Option` for absence, `Result` for
   recoverable domain/environment failure, trap/abort for invariants / OOM /
   bounds / arithmetic, explicit cross-module wrapping, no hidden `?` conversion
   web); and the stable, workload-pulled pure core (`option`/`result`, selected
   `bytes`/`numeric` helpers, and checked conversion boundaries) carrying Lean
   evidence where the API has stopped moving. Do not try to prove all stdlib
   APIs before workloads validate their usefulness; every API needs an evidence
   class, but only stable central pure APIs should be upgraded to `proved`.

These enforce a three-layer shape: a **pure core** (no capabilities, no
allocation unless explicit), an **alloc layer** (`with(Alloc)`, later explicit
allocator values), and a **hosted authority layer** (every API names its domain
cap). Iteration is internal only (`for_each`/`fold`/`_ctx`, optional
`Continue | Break`, no iterator trait or lazy adapter tower). Hold breadth —
broad crypto/compression/networking/threading and framework-style APIs — until
the four properties hold and a workload pulls it; the pure core is the priority,
not the periphery.

Phase 7 priority order is deliberately narrower than the comparison languages:
make Concrete's small core pleasant and auditable before copying broad
batteries-included breadth. Completed foundation work lives in
[CHANGELOG.md](CHANGELOG.md); the remaining ranked build order is:

0. Stdlib hardening pass H1-H5 — before all remaining breadth. This is
   foundation repair, not new surface; details live below, ahead of the
   collection-API build-out.
1. MAIN_EXIT_MODEL stage 2 (`docs/MAIN_EXIT_MODEL.md`): COMPLETE
   2026-07-17 — the `CONCRETE_ECHO_RESULT` knob is deleted (compiler +
   every harness consumer; check_exit_codes.sh pins the env var IGNORED).
   Remaining tail, deliberately decoupled: retire Int-main (a later
   surface pass narrows the entry signature to `fn main() -> u8 | Unit`)
   and, workload-gated, `main -> Result<Unit, E>` printing the error
   variant name to stderr (Zig-style nominal rendering, no display
   traits).
1a. Refactor queue (review 2026-07-16, evidence-ranked; each item lands as
   its own slice with the battery):
   - Layout panic hygiene: convert `Concrete/Check/Layout.lean` `panic!`
     paths (unknown struct/named type in fieldOffset/tySize) into
     structured internal-error diagnostics — bug 035's fix made them
     unreachable for well-formed programs, which is exactly when a panic
     should become a diagnostic. SCOPED (2026-07-17): ~69 call sites
     (tyAlign 21, tySize 27, isPassByPtr 12, fieldOffset 8, tyToLLVM 2
     incl. EmitSSA), all cascading through pure code — no small slice;
     it is an Except-threading grind that must land green in ONE piece,
     with the full battery as backstop. Dedicated fresh session.
   - RESOLVED as inert (2026-07-17 probe): the match-loop array
     load-binding divergence documented in the merge-loop unification.
     All four post-029 shapes verified correct (post-match &arr borrow,
     in-place element write, by-value call — IR materializes the
     aggregate correctly): every downstream site re-materializes an
     address from the value, so the divergence miscompiles nothing
     reachable. Leave as the BranchMergeRules comment documents; do NOT
     blind-flip without a failing repro (IR churn for no proven gain).
   - DONE: Lower branch-site consolidation (structured-builder slice 2): the
     three merge loops (statement-if, if-expr, match) now call ONE
     `reconcileBranchVars`; the four residual semantic differences
     (statement-if void rule, array-address rebind — the match loop still
     binds loads, unit-phi guard, single-incoming rebind) are explicit
     fields of `BranchMergeRules`, documented as deliberate and not to be
     "simplified" without a full battery. No behavior change (byte-identical
     IR by construction; 1680/0 + 531/0 + fuzz seeds green). The &&/|| site
     keeps its own minimal shape. The fifth merge bug is now one function's
     problem instead of four sites'.
   - DONE (2026-07-17, workload 5): bug 039 — imported bare fn name rebound
     to another module's colliding import (program-wide first-match alias
     pool; `import std.env.{get}` compiled to args_get, segfault). Fixed in
     emitSModule (module-local import aliases shadow the pool);
     regress_039_import_alias_collision project test + check_envcfg.sh
     env-override legs pin it. Residual (pull-gated, in the bug doc): the
     submodule-granularity variant inside ONE top-level module shares a
     flat alias list.
   - Stale stdlib docs refresh: STRING_TEXT_CONTRACT.md (argv/string
     validation claims), BYTE_VIEW.md (`Text::from_raw` vs the shipped
     `try_from_raw`/`_unchecked` split), stdlib/STDLIB_API_REVIEW.md
     (`std.writer` as a separate module; old args.get concerns) — design
     clarity, not code risk.
   Audit additions (external audit 2026-07-16, spot-verified; first three
   landed same-day — see CHANGELOG):
   - DONE: float-literal lexing correctly rounded (Nat mantissa +
     Float.ofScientific; the old per-digit 0.1 accumulation lexed `0.7`
     one ulp off, invisible to the differential oracle because both sides
     shared the lexer). COMPLETED 2026-07-16 second pass: ofScientific
     itself keeps only ~11 guard bits (1-ulp errors on `16.3633343`,
     `932183.9385014`) — final fix is `floatOfDecimalMantissa`, exact
     big-Nat conversion, gated by check_float_literals.sh (8022 cases
     against CPython float(), function + end-to-end lexer path).
   - DONE: pipeline-test in defaultTargets + run_tests fail-closed (32
     pass-level Lean tests were vacuously green for months). Unparseable
     pipeline-test summary is now fail-closed too.
   - DONE: proof-status proved entries carry an explicit trust line
     ("linked + fingerprint-fresh — kernel replay via check-proofs").
   - DONE: intrinsic identity threaded to call sites (Elab lookupFnSig
     guard + Lower definedFns guard; regress_intrinsic_shadowing.con).
     Residual: Check-side divergence/capability classifiers still match
     raw Core names (conservative misclassification only) — #13b.
   - DONE: Option/Result canonical-enum sizing is footprint-aware
     (`alignUp 4 align + size`) and every enum alloca whose payload needs
     8-byte alignment carries an explicit `align 8` (single emitAllocaTy
     choke point; string_to_int shipped the 4-aligned-i64-store bug).
     Byte-array storage kept deliberately: aggregate load/store of a
     partially-initialized union is poison-safe only at byte granularity.
     Gate: check_enum_union_layout.sh + regress_enum_canonical_align.con.
   - DONE: interp/backend string_char_at drift (codepoint-vs-byte
     indexing, OOB 0-vs--1) — interp matches the backend exactly;
     check_string_char_at.sh. Residual CLOSED 2026-07-17: builtin
     semantics single-sourced by DETECTION — signatures were already one
     table (Resolve/BuiltinSigs.lean, Check+Elab);
     check_builtin_semantics.sh pins interp == compiled byte-equal per
     shared builtin (edge-heavy: empty/non-ASCII/OOB/negative/wide) and
     pins the interp-PENDING inventory as an explicit 11-name list so
     additions land on both sides or extend the pin deliberately. A
     body-generation DSL (one spec emitting interp Lean + backend LLVM)
     is recorded as NOT-now: high machinery cost, detection already
     kills the drift class.
   - DONE: evidence-doc drift — five examples/*/assumptions.toml said
     overflow="wrapping" (language traps); files corrected and
     check_assumptions.sh now reads/enforces the arithmetic section
     against --report arithmetic; ASSUMPTION_FILES.md template fixed.
   - DONE: CI — run_ci_gates_local extraction fixed (bare ./scripts
     invocations + arguments preserved, tree-mutating mutation gate
     excluded); retry loops emit ::warning on failed attempts; dark gates
     wired (extra-gates CI job: catches/showcase/wrong-code/reducer/
     release-bundle + the three audit gates) — test_wrong_code was
     already silently red when wired (WC-0005 stale expectation, fixed).
   - DONE: phantom `concrete new` scrubbed from the Resolve hint and the
     book/site project pages (now document Concrete.toml + src/main.con);
     ideas.org carries a SUPERSEDED banner (12b's gate will police).
   - DONE: fuzzer grammar extension landed — aggregate value-ifs, &mut-self
     method borrows in both branch arms, non-Copy String ops. Its first
     campaign found bug 038 (merge clobbered promoted-aggregate writes —
     all three merge loops now skip ANY promoted var) plus interp drift:
     string_length was codepoints not bytes; push_char/append added.
   - Fuzzer residual: heap/alloc/IO remain compiled-only (interp limits);
     trap CLASS not compared.
   Deliberately NOT queued: std.cli abstraction polish (one more workload
   first), ByteCursor/ByteWriter renames (no signal), vec_get/vec_len
   builtin cleanup (deeper than a pass), roadmap compaction (collides with
   parallel work).
2. CLI/env/process helpers for real tools (stdlib APIs, not compiler CLI).
3. Unsafe/trusted boundary wrappers, trap/debug UX, and verified-profile/
   proof-obligation UX.
4. Shipped pure-core proof arc: prove the actual `Option`/`Result`,
    `Bytes`/slice, numeric checked helpers, and checked text/path conversions
    against their documented contracts.
5. Proof-facing formal stdlib models (`formal_vec`, `formal_map`,
    `formal_set`, `bigint`, lemma helpers) once contracts need them.
6. Broad compression/crypto/networking/threading only after workload demand.

The comparison-language bias for remaining breadth is deliberate: copy module
categories from Hare/Zig/Odin/Go, not their API volume. The near-term systems
set is `cli`, `fmt`/`parse`, `io` buffering and memory sinks/sources,
`fs`/`path`/`temp`/`dirs`, `log`, `sort`/`search`, `uri`, `json`, checksums,
and narrow shell helpers (`shlex`/`glob`/`fnmatch`) when a workload pulls them.
Keep broad crypto, compression/archive formats, OS debug formats, DNS/Unix
sockets, full HTTP, threads, SIMD, and platform databases out of the core until
there is a validation workload and an evidence story.

Phase 6E owns the **compiler** command surface (`concrete build/run/test/fmt`,
help, reports, trace/debug aliases, and compatibility). Phase 7's CLI work is
the **stdlib** side: APIs that Concrete programs use to parse their own command
lines (`std.cli`), read process arguments (`std.args`), and build real tools.
Do not duplicate compiler-command cleanup here.

Foundation items already completed in Phase 7:
stdlib gap matrix, module layout, five-fact manifest, method-canonical rule,
Option/Result helpers, conditional Copy, bytes/slice, Unicode/text policy, path
boundary, Reader/Writer, core value rendering, std.test basics, ordered
traversal, BitSet aliases, H18 owned-resource collection drop glue, the base64
workload, MAIN_EXIT_MODEL stage 1, and the 13t error-convention gate. See
[CHANGELOG.md](CHANGELOG.md). H18's residual v1 fence is not scheduled work:
lifting it is pulled by the first infallible non-`Alloc` destructor; raw
`*_unchecked` overwrite escapes remain documented trusted escapes.

**Stdlib hardening pass — do this before more breadth.** The review found the
existing core has correctness gaps that any module built on top of it (URI, JSON,
CLI helpers, logging) would inherit, so harden the foundation before growing it.
No philosophy change; the API shape is good. Each item lands as a *class*-gate
per the Definition Of Done, not a one-instance patch. Frontier order for this
band: (1) this hardening pass → (2) item 22a API snapshot/diff gate current →
(3) real workload 2 → (4) then broad modules (URI, JSON, CLI helpers,
logging/progress).

   - H1. Parser domain failures must not trap. `parse_hex` / `parse_bin` /
     `parse_oct` return `None` on overflow, matching the already-guarded
     `parse_int` (decimal). Gate: huge/overflowing inputs in *every* radix
     return `None` with no trap — one fixture per radix, so the class cannot
     regress to "decimal-only guarded" again.
   - H2. Overflow-safe bounds / capacity arithmetic. Replace additive
     `start + len > total`
     guards (`io.con` `cur + len > cap`, `numeric.con` `off + len`, and the
     `bytes` / path / base64 siblings) with non-overflowing forms
     (`len > total - start` after an ordering check, or a checked add). Include
     allocation-size calculations, not only access checks. Gate: a grep/lint
     that fails if a std API returning `Option`/`Result` uses overflow-prone
     additive guard arithmetic on a length.
   - H3. IO honesty. Preferred answer: file/console read/write/flush/close
     operations represent real OS errors (`ReadFailed`, `WriteFailed`,
     `FlushFailed`, `CloseFailed`) rather than returning a decorative `Result`.
     Only document an ignored error where the platform API genuinely gives no
     recoverable signal or the sink is explicitly trusted. Gate: every hosted IO
     op either surfaces the error in its `Result` or carries a documented
     ignore-rationale the manifest checks.
   - H4. Collection traversal for non-`Copy` values. Move `OrderedMap::fold` /
     `for_each` out of `impl<K, V: Copy>` (`ordered_map.con`) so borrowed
     traversal works for non-`Copy` values — the scoped-callback model already
     passes `&V`, so the `Copy` bound is spurious and blocks the model we want.
     Gate: a fixture folds / `for_each`-es an `OrderedMap` of a non-`Copy` value.
   - H5. Docs / canonical cleanup. Fix the stale `Bytes::to_string` comment
     (`bytes.con`). If base64 `decode` stays permissive on non-canonical pad
     bits, document it as a deliberate sharp edge; otherwise add strict canonical
     pad-bit checks with a reject fixture. (The `with_value_mut` / `modify`
     "parked" drift is already fixed — see item 1d below.)
   - H6. Deterministic capability-fault simulation (pull-gated; the *dynamic*
     complement to H3). H3's manifest gate proves an IO error *can* be surfaced
     (static shape); this proves it *is* surfaced under real failure (dynamic).
     Because every effect flows through an explicit capability/handle value — e.g.
     `Writer`'s `write_fn: fn(...) -> Result<u64, IoError>` — faults are injected by
     swapping the backend at the seam, with no whole-world simulator; the capability
     model makes fault injection cheap by construction, which is why Concrete's
     version is far smaller than Turso-style DST. Deterministic failure schedules
     drive properties static analysis cannot reach: `write_all` never `Ok` after a
     short/failed write; close-fail-after-flush is reported; allocation #N fails;
     `Reader` treats `Ok(0)` as EOF only at the right boundary. Do NOT re-litigate
     already-static properties (capability erasure = manifest gate; exit-status ≠
     stdout = MAIN_EXIT_MODEL). Proof stays separate — the pure core's first line;
     simulation covers only the effectful failure interleavings proofs do not, and
     upgrades no evidence class to `proved`. Pull-gated: pulled when H3 lands or an IO
     bug appears; run against `base64_cli`/`png_chunks`/workload 3, starting at the
     `Writer`/`File` seam and expanding only as each capability (`Alloc`, `Time`)
     gains an injectable backend. Design record:
     [`docs/DETERMINISTIC_SIMULATION.md`](docs/DETERMINISTIC_SIMULATION.md), which
     refines the tier-J+ "deterministic simulation backend" earmark in
     `docs/EXECUTION_MODEL.md`. Gate: `scripts/tests/check_effect_simulation.sh`
     replays a fixed fault schedule and asserts each effect-boundary property.

1. Build the remaining collection APIs across `std.vec`, `std.map`, `std.set`,
   `std.ordered_map`, `std.ordered_set`, `std.deque`, `std.heap`,
   `std.bitset`, and `std.slice`: fixed arrays/slices, `Vec<T>`, maps, sets,
   buffers, parser cursors, and capacity-aware helpers. Add the ordinary APIs
   C/Rust users expect: `contains`, `remove`, `iter` for maps and sets;
   `insert`, `remove`, `retain`, `sort`, `search` for vectors and slices;
   stable ordering helpers for ordered collections. Each API must state whether
   it requires `with(Alloc)`, whether it can fail, and which runtime
   obligations it creates.

   H18 / owned-resource collections is closed; keep the design pointer here
   because every new collection API must preserve it. A collection owns its live
   elements until they are explicitly moved out. `clear`, `drop`, overwrite,
   set/replacement, and compaction paths destroy displaced live elements;
   `pop`, `remove`, and `swap_remove` transfer ownership to the caller.
   Disposal stays forced-explicit (`x.drop()` / `defer x.drop()`), never silent
   scope-end auto-drop. `get -> Option<T>` remains `T: Copy`; non-`Copy` indexed
   access remains scoped callback / borrow / explicit move-out, with no returned
   mutable references. Full H18 mechanism and gates live in CHANGELOG and
   `docs/RUNTIME_COLLECTIONS.md`; only the pull-gated v1 fence remains:
   associated destructor-capability machinery is pulled by the first infallible
   non-`Alloc` destructor, and raw `*_unchecked` overwrite escapes remain
   documented trusted escapes.

   Traversal must be **deterministic but not ordered**:
   `for_each`/`fold` over `std.map`/`std.set` need a fixed hasher and no per-run
   random seed (so the oracle/differential story is stable given the same
   operations), but the visitation order is deliberately **not** a defined key
   order — `HashMap`/`HashSet` are permanently unordered (a map is unordered in
   the proof model, so this costs the evidence story nothing). A defined key
   order is `OrderedMap`/`OrderedSet`'s job; their `for_each`/`fold` APIs have
   landed and must remain ascending-key traversal. Do not add insertion-order
   tracking to the hash collections.
   - 1a. Keep `Clone` and indexed move/swap as **workload-gated value-model
     research**, separate from H1 and not assumed inevitable. If admitted,
     `Clone` is explicit semantic duplication (capability-visible, usually
     `with(Alloc)`, audit-visible, and not silently proof-eligible), not a
     convenience patch for borrowed reads. The sibling move-out primitive for
     indexed containers is `swap(i, new) -> V`, which transfers ownership out
     while preserving the linear one-value-per-slot invariant. Build either only
     after a real workload repeatedly needs owned duplication or indexed
     ownership-out and the existing `Copy` / move / borrow APIs are the wrong
     fit.
   - 1b. Design arena/index safety before any arena-backed structure becomes a
     flagship or stable stdlib API. Array-backed linked structures use indices,
     and a stale index into a removed/reused slot is a logic-level dangling
     pointer that ordinary ownership does not see. V1 answer should compare
     typed index newtypes (`NodeId`, not `u64`), generation-counted slots,
     arena validity invariants, and proof obligations that inject newtype
     invariants as scoped hypotheses. Public stdlib arenas remain workload-
     gated: do not add a generic `Arena<T>` until a parser/graph/runtime
     workload needs it, and never expose raw integer indices as the stable API.
     Any admitted arena-backed container must preserve linear destruction,
     allocator visibility, and stale-index rejection or explicit trust. Add
     `docs/ARENA_INDEX_SAFETY.md`, `examples/arena_indices/{stale_index,generation_checked,typed_node_id}/`,
     and `scripts/tests/check_arena_index_safety.sh`; the gate must prove stale
     index reuse is rejected, trapped, or explicitly trusted, never silently
     treated as ordinary memory-safe access.
   - 1c. Decide explicit-dictionary coherence before binary collection
     operations harden. `HashMap<K, V>` values built with different `hash_fn` /
     `eq_fn`, or ordered maps/sets built with different comparators, have the
     same type but incompatible semantics; `union`, `merge`, `intersection`,
     equality, and bulk transfer across them can silently produce nonsense.
     Choose one v1 rule: canonical registered dictionary per key type,
     dictionary/brand carried in the collection type or value, debug/runtime
     identity checks on binary operations, or a documented sharp edge that
     forbids binary ops across explicitly-different dictionaries. Add
     `docs/COLLECTION_COHERENCE.md`,
     `examples/collection_coherence/{same_dictionary_merge,different_eq_rejected,different_cmp_rejected}/`,
     and `scripts/tests/check_collection_coherence.sh`; the gate must show the
     chosen verdict for incompatible dictionaries and prove the API cannot
     silently combine them.
   - 1d. `with_value_mut` / `modify` are part of the completed foundation (see
     CHANGELOG). Future collection APIs must preserve their `E0293`
     container-not-in-context guard: the callback's `&mut ctx` may not alias the
     `&mut self` container. Gated in `check_callable_values.sh`.
   - 1e. Keep scalar `from(param)` returned references deeply deferred and
     evidence-gated. If ever added, they stay flat and scalar: no `Option`,
     `Result`, structs, arrays, containers, callback contexts, or generic
     wrappers. This is a research escape valve only if real workloads prove
     operation APIs, owned views, and scoped callbacks insufficient.
   - 1f. Pull narrow const generics forward only when fixed-capacity stdlib APIs
     need reusable capacities. `docs/CONST_GENERICS_V1.md` is the closed Phase 5
     design record; implementation is deferred until a real Phase 7 consumer
     appears (`BoundedVec<T, N>`, `RingBuffer<T, N>`, `PacketBuf<N>`, fixed hash
     table, parser scratch buffer, freestanding reusable buffer, or a
     capacity-indexed proof API). When triggered, implement the staged V1 from
     that doc and wire `scripts/tests/check_const_generics_v1.sh` to prove
     distinct capacities specialize separately, layout is capacity-specific,
     runtime-safety obligations name the instantiated size, and unsupported
     comptime/reflection/runtime-bound forms are rejected.
   - 1g. Research **allocator-as-value** after H18 is closed and before
     allocator-backed collections,
     arenas, or freestanding APIs harden. Keep the distinction sharp:
     `with(Alloc)` is permission to allocate; an allocator value names which
     allocator/arena/pool is used. Do not replace capabilities with allocator
     parameters, and do not add an ambient implicit allocator. The research
     note should compare Zig-style explicit allocators, arena/test allocators,
     embedded/freestanding pools, hot-reload/plugin ownership, allocation
     failure policy, and audit output that records both the capability and the
     allocator identity/strategy. If admitted, every allocator-taking API must
     keep allocation authority visible in function headers and reports. This
     item should move ahead of allocator-heavy collection stabilization if the
     validation project needs arenas, test allocators, reload-safe allocation,
     or freestanding pools.
2. Build internal-iteration and builder APIs in proposed `std.iter` and
   `std.builder` after the collection shape is known. This is NOT Rust's
   external-iterator / adapter-tower model: `research/stdlib/iterators.md`
   resolved the v1 design as per-container internal traversal (`for_each`,
   `fold`, context-threaded callbacks, and optional early-exit via an explicit
   `Continue | Break` tag), with no iterator trait, no lazy adapter chain, and
   no cursor/lifetime model. Add known-length reporting and reverse traversal
   (`rev_fold`/`rev_for_each` — today every backwards walk is a manual
   index-decrement loop; extend `docs/ITERATION_PROTOCOL.md` when these land),
   plus byte/text builders and tree/buffer builders inspired by Gleam's
   `BytesTree` and `StringTree`. Do not hide allocation; builder APIs either carry
   `with(Alloc)` or operate over fixed buffers.
2a. Complete the Hare/Zig-shaped IO adapter layer on top of the one
    `std.io.Reader` / `std.io.Writer` spine: buffered reader/writer helpers
    (`bufio` shape), fixed-memory readers/writers (`memio` shape), byte-counting
    and tee/discard adapters, and small copy/drain helpers. These are adapters,
    not a second IO abstraction. Every adapter must preserve the underlying
    capability story, keep allocation explicit (`with(Alloc)` or caller-provided
    buffers), and be covered by `std.test` sink/source oracles.
3. Build numeric helper APIs in `std.numeric`, `std.math`, and `std.mem`:
   checked/wrapping/saturating arithmetic helpers,
   narrowing/conversion helpers, endian conversions, byte/word packing, and
   evidence classes for each helper.
4. Build sorting and searching primitives in proposed `std.sort` and
    `std.search`: comparison conventions, stable/unstable sort decision,
    binary search, min/max helpers, and evidence/oracle tests over edge cases.
5. Build hashing, checksums, and deterministic random helpers in `std.hash`,
    proposed `std.checksum`, and `std.rand`: stable hash APIs for maps/sets,
    non-cryptographic checksums, seeded deterministic RNG for tests/oracles,
    and a clear split from cryptographic randomness. Any OS entropy source is
    hosted-only and capability-visible.
6. Build constant-time helper APIs in proposed `std.ct` only for narrow,
    auditable cases: equality/compare over fixed-size bytes, no secret-dependent
    branches or early exits, source-shape audit evidence, and clear
    machine-level timing assumptions. This belongs to the narrow security
    surface, not broad crypto.
7. Build time and duration helpers in `std.time`: monotonic versus wall-clock
    distinction,
    timestamp formatting/parsing if admitted, timeout helpers, and explicit
    hosted authority for reading the clock.
8. Build formatting and parsing helpers in `std.fmt` and `std.parse`:
    integer/text formatting, simple
    scanners, structured parse results, error-set reports, and oracle-friendly
    output conventions.
9. Build a reusable scanner/parser core in `std.parse` over
    `std.bytes.Bytes` and `std.text.Text`: `peek`, `advance`, `take_while`,
    `consume`, span/position tracking, error reporting, and no hidden
    allocation unless the API carries `with(Alloc)`.
10. Keep `std.base64` as the first byte-format module's completed workload
    precedent, not an open design task. V1 and `examples/base64_cli` have
    landed; future work is only the explicit tail: decide and gate canonical
    padding-bit strictness, add streaming encode/decode only when a workload
    pulls it, and preserve the args -> bytes/text -> parse/errors -> Writer
    oracle path as the model for the next byte-format module.
11. Add `std.uri` parsing/formatting after the byte/text/path split is stable:
    component accessors, percent encoding/decoding, normalization policy, and
    clear distinction between syntax validation and network authority.
12. Add `std.json` as the first structured data module: tokenization,
    string/number handling, error spans, bounded recursion policy, optional
    DOM-like representation only if the allocation story is explicit, and
    oracle tests against a reference implementation.
13. Add a small typed decoding layer in proposed `std.decode` after
    `std.json`: dynamic value decoding, field access, error paths, and examples
    comparable to Gleam's `dynamic/decode`, without broad reflection or hidden
    runtime typing.
14. Add binary serialization helpers in proposed `std.bin`: endian-aware
    reading/writing, fixed-width integers, length-prefixed fields only with
    explicit bounds, byte-span diagnostics, and no hidden allocation.
15. Add semantic-version and config-format helpers in proposed `std.semver`
    and `std.config` if package/build work starts depending on them:
    `SemVer`, INI/TOML-style scanner, and manifest parsing support. Prefer a
    small Hare-like `format/ini`-class module before any broad configuration
    framework; keep TOML/YAML/package manifests workload-gated. These are
    stdlib/package-boundary helpers, not general metaprogramming.
16. Add command-line parser helpers in proposed `std.cli`: flags, positional
    arguments, usage text, typed parse errors, no ambient environment access
    except through `std.args`, and examples that keep authority visible. This is
    for user programs. The `concrete` compiler's own command taxonomy and help
    behavior are Phase 6E, and `std.cli` should learn from that surface without
    coupling to compiler internals.
16a. Add narrow shell/path helper modules only as tool workloads demand them:
     `std.shlex` for shell-word splitting/quoting, `std.glob` / `std.fnmatch`
     for file-pattern matching, and no `wordexp`-style command/variable
     expansion in the core. These APIs are byte/path aware, carry no hidden
     filesystem authority, and must keep expansion separate from matching so a
     pure parser cannot accidentally become a hosted operation.
17. Add simple logging/diagnostic output APIs in proposed `std.log`: levels,
    writers, formatting
    integration, capability requirements, and policy for release builds. Keep
    this small; it is not a tracing framework.
18. Add progress/status output helpers for CLI tools in proposed
    `std.progress` only after `examples/daily/word_count` and
    `examples/base64_cli` need visible progress output. V1
    surface: `ProgressWriter`, `quiet`, `verbose`, `set_total`, `advance`,
    `finish`, terminal detection through an explicit console handle, and no
    ambient terminal authority. Wire
    `scripts/tests/check_stdlib_progress.sh` when the module is admitted.
19. Build capability-scoped console, file, network, process, and time APIs in
    `std.io`, `std.fs`, `std.env`, `std.args`, `std.process`,
    `std.net`, and `std.time`. Authority must be visible in function types and
    audit reports; no API may smuggle ambient authority through a convenience
    wrapper. New output/input APIs target the `std.io.Reader` / `std.io.Writer`
    spine; a separate public `std.writer` sink surface must not reappear.
    Add an explicit **boundary-module pattern**, borrowing the useful part of
    Elm's ports without copying Elm's web-application architecture: authority
    enters through a small named module, the wrapper exposes a narrow safe API,
    and the audit report names the capability, trusted/FFI boundary,
    assumptions, allocation behavior, and evidence class. Concrete examples:
    `std.fs.boundary` wraps ambient filesystem entry points behind directory
    handles; `std.net.boundary` wraps socket creation before pure parsers see
    bytes; `std.libc.boundary` wraps extern calls before safe code sees owned
    values. Add `docs/stdlib/STDLIB_BOUNDARY_MODULES.md`,
    `examples/stdlib_recipes/{fs_boundary,net_boundary,ffi_boundary}/`, and
    `scripts/tests/check_stdlib_boundary_modules.sh`; the gate must prove that
    the public safe wrapper surface has narrower authority than the underlying
    trusted/extern implementation and that the audit transcript shows both
    sides of the boundary.
20. Build handle-relative filesystem APIs in `std.fs` as the preferred
    file/path shape: directory/file handles carry authority; operations are
    relative to handles for `open`, `create`, `read`, `write`, `metadata`,
    `remove`, `rename`, and `list`. Ambient absolute-path helpers must be
    hosted-only convenience wrappers with explicit authority. Temp-file,
    symlink, path-normalization, and TOCTOU behavior must appear in
    `docs/stdlib/STDLIB_GUIDE.md` and `scripts/tests/check_stdlib_fs.sh`.
20a. Add small `std.temp` and `std.dirs` helpers after the handle-relative
     filesystem shape is gated. `std.temp` owns temporary files/directories with
     explicit cleanup (`drop`/`defer`, no implicit scope-end delete) and visible
     `with(File)` authority; `std.dirs` exposes only conservative hosted
     directory discovery needed by tools. Do not add XDG/platform policy sprawl
     until package/build workloads pull it.
21. Build handle-based network surface in `std.net` only as far as the
    validation workloads require: address parsing, socket handle wrappers, HTTP
    header parsing as a pure parser first, and no full HTTP client/server until
    package/workload evidence demands it.
22. Build stdlib test/oracle helpers in `std.test`: expected failures,
    capability-scoped fixtures, temp directories, oracle vector runners,
    interpreter-vs-compiled helpers, and report snapshots. Stdlib tests must
    also serve as runnable API documentation: every public stdlib type/function
    gets a tiny positive usage test and, where meaningful, a negative or edge
    case. Public docs should link to or quote those tests rather than carrying
    stale hand-written examples. This is the Zig stdlib lesson adapted to
    Concrete: usage examples should be executable fixtures, not prose that can
    rot. Add a gate that fails when a new public stdlib symbol lacks a test/doc
    example, when a referenced example no longer compiles, or when a doc claims
    an evidence class/capability/allocation behavior that the test/report does
    not produce.
22a. Add a public stdlib API snapshot/diff immediately after the first real
     workload (`base64_cli`) and before broad URI/JSON/CLI/log/progress breadth.

     Deliverable: `concrete std snapshot --json` (or an equivalent test helper)
     and `scripts/tests/check_stdlib_api_snapshot.sh`.

     Snapshot fields: module path, public names, signatures, `Copy`/linear
     requirements, capabilities, allocation behavior, trap/recoverable-failure
     profile, evidence class, deprecation status, and linked tests/docs.

     Done when adding/removing a public function, changing a signature, widening
     a capability, changing allocation/trap behavior, or changing an evidence
     class fails the gate until the snapshot and docs are updated. This is the
     stdlib-local version of Phase 10's proof/capability diff and Phase 18's
     package API artifacts; it prevents accidental drift while the library is
     still small.
23. Define stdlib error-handling conventions: when APIs return `Result`,
    `Option`, panic/abort, or require a policy gate; how ignored-result
    diagnostics apply; and how accumulating error sets are reported. Split
    recoverable domain/environment failures from fatal invariant failures:
    `Result`/`Option` for user, file, network, parse, and domain errors that a
    caller can handle; abort/trap for OOM, bounds, arithmetic traps, impossible
    invariants, and explicitly unrecoverable runtime failures. Audit output
    should identify which public APIs can recover, which can trap, and which
    require a policy gate. Do not add a second Zig-style error-union mechanism;
    improve the `Result`/`Option` surface instead. Define how a module error
    composes into a caller's error across a boundary (`FsError` -> `ServiceError`)
    WITHOUT Rust's `From`/`Into` trait web: an explicit conversion function or a
    small wrapping variant named at the call site, not an implicit trait
    conversion. State the pattern once so error-heavy code does not each invent
    its own.
23a. Define the canonical **consume / destroy / handoff** conventions for
    linear code. The guide must distinguish explicit cleanup (`destroy(x)` or
    the type's consuming `.drop()`/Destroy verb), ownership transfer by
    by-value call, ownership transfer by return, destructuring into owned
    fields, `defer` as explicit scheduled cleanup, and the forbidden cases
    (bare non-`Copy` statement, `_` over non-`Copy`, `let _`, non-`Copy`
    sub-place projection by value). This is documentation plus examples and
    diagnostics, not automatic `Drop`: no hidden cleanup and no hidden control
    flow. Include a short **Copy and Linear Values** guide: when to mark a type
    `Copy`, why `Option<i32>` is `Copy` but `Option<String>` is linear, how
    value-mode reads move non-`Copy` bindings by default, why `_` ignores only
    `Copy`, how params are owned locals unless they are borrows, and the
    ordinary consume paths (`destroy`, handoff, return, destructure, `defer`).
    This guide is the user-facing explanation of the conditional-`Copy` and
    mode-based-checker refactors; it should be validated by small runnable
    examples and linked from README/site docs.
24. Define stdlib evidence classes per public API: `proved`, `enforced`,
    `reported`, `tested_by_oracle`, `assumed`, or `trusted`. The evidence class
    must appear in docs and audit artifacts, not just implementation comments.
24a. Add proof-facing formal stdlib models only when a proof or contract
    workload pulls them. These are not runtime containers: `std.formal_vec`,
    `std.formal_map`, and `std.formal_set` model mathematical sequences, maps,
    and sets for contracts, loop invariants, and Lean obligations; `std.bigint`
    models unbounded mathematical integers for specs before any runtime
    big-number API exists; `std.rational` stays deferred until exact-ratio
    specs need it. Each formal module must state its erasure/runtime story, its
    evidence class, the Lean artifact it lowers to, and the refinement relation
    to runtime containers such as `Vec`, `HashMap`, `OrderedMap`, and `HashSet`.
24b. Add a **selective shipped pure-core stdlib proof arc**, distinct from the
    pull-gated formal-container work in 24a. This proves stable stdlib code
    users actually call, not separate mathematical containers, and it is not a
    mandate to prove all 414+ public API rows before the API surface has been
    validated by workloads. Scope v1 narrowly:
    `Option`/`Result` helpers (`map`, `and_then`, `map_err`, identity /
    composition where expressible), numeric checked helpers (success/failure
    agreement with documented overflow, divide-by-zero, and narrowing
    behavior), `Bytes`/slice pure helpers (`view`, `cmp`, checked `get`/`set`,
    raw-data preservation, bounds behavior), and checked text/path conversions
    (valid UTF-8 boundary, raw bytes preserved, unchecked constructors named
    `_unchecked`). Non-goals: hosted APIs, allocation behavior, filesystem,
    network, process, clock, console effects, broad collection proofs, and the
    `formal_vec`/`formal_map`/`formal_set` model-container layer. Treat tests,
    oracle vectors, manifest facts, mutation gates, and capability/allocation
    gates as the right evidence for unstable or broad APIs; upgrade an API to
    `proved` only after it is small, pure, central, and workload-pulled. Done
    when a small Lean proof suite is CI-gated, each covered API's manifest/report
    row distinguishes `proved` from `tested`/`enforced`, and a negative fixture
    proves the report cannot keep `proved` after a body/spec drift. This is the
    "proved pure core" part of the Phase 7 excellence contract; do not let it be
    deferred behind breadth modules such as JSON, CLI, or networking, but also
    do not turn it into an exhaustive stdlib-proof sweep before API ergonomics
    have been validated.
25. Add stdlib authority/allocation/runtime-obligation gates so core helpers
    cannot silently widen capabilities, allocation behavior, trusted
    assumptions, or runtime-risk obligations.
26. Add sanitizer/runtime-instrumentation hooks as debug and validation
    surfaces, not proof evidence. Inspired by Odin's sanitizer-facing base
    surface, Concrete should expose named hooks for bounds, overflow,
    use-after-free-like trusted/FFI probes where applicable, allocator checks,
    and generated-code validation. Audit output must classify this as
    `runtime_checked`, `tested`, or `instrumented`, never `proved`.
27. Split hosted versus freestanding-ready stdlib modules at the API level:
    no-alloc/no-OS core modules, allocator-backed modules, hosted OS modules,
    and modules that are explicitly unavailable under freestanding profiles.
    The freestanding target implementation still lands in Phase 16.
28. Record deliberately deferred stdlib families so they do not disappear from
    planning: compression/archive formats, broad crypto beyond the narrow
    hash/HMAC/constant-time story, PEM/ASN.1/X509, MIME helpers beyond the first
    workload, full HTTP client/server, DNS/Unix-socket breadth, dynamic
    libraries, OS debug formats (`ELF`, DWARF, image parsing), atomics, threads,
    SIMD, target/ABI databases, and platform-specific C/POSIX wrappers. Each
    stays package-later,
    backend-later, freestanding-later, or research-later until a workload
    forces it.
29. Add stdlib docs and examples for C/Rust users:
    `docs/stdlib/STDLIB_GUIDE.md` plus `examples/stdlib_recipes/bytes_text`,
    `path_fs`, `result_errors`, `vec_map`, `parser_cursor`, `json_scan`,
    `base64_cli`, `uri_parse`, `checksum`, `deterministic_rand`, `time_log`,
    and `capability_io`. Each recipe must show the exact `std.*` imports,
    capability set, allocation behavior, and expected audit line.
    Add one blessed systems-program shape, inspired by Elm's success at
    teaching a single architecture but adapted to Concrete's domain:
    **read/acquire -> parse -> validate -> transform -> emit/release**. This is
    not a framework and not hidden control flow; it is an example/documentation
    convention for ordinary tools and protocol handlers. The guide must show
    which stages are pure, which carry capabilities, where `Result` flows, where
    runtime obligations attach, and which report proves each claim. Add
    `examples/stdlib_recipes/pipeline_shape/` with a tiny byte-oriented CLI and
    a no-alloc parser variant; both must include the source, expected
    `--report caps`, `--report contracts`, and `--report profile` snippets.
    Include a small **stdlib style-coherence pass** in this item: each module
    must choose and document when an operation is a method versus a free
    function. Do not mix `s.len()`, `string_length(s)`, and
    `string_push_char(&mut s, c)` arbitrarily in the same public surface. The
    rule should serve explicitness and capability visibility: methods are fine
    for operations whose receiver clearly owns/borrows the authority; free
    functions are fine for cross-type operations or functions whose capability
    story would be clearer outside a receiver. Add examples that teach the
    chosen style instead of preserving accidental historical names.
30. Add a stdlib compatibility/oracle corpus under
    `examples/stdlib_compat/`: `fmt_parse_vectors`, `bytes_text_vectors`,
    `path_vectors`, `collection_vectors`, `base64_vectors`, `uri_vectors`,
    `json_vectors`, `semver_vectors`, `sort_search_vectors`,
    `checksum_vectors`, `rand_vectors`, and `cli_io_vectors`. Wire it with
    `scripts/tests/check_stdlib_compat.sh`; every vector must declare exactly
    one mode in `manifest.toml`: `oracle_python`, `oracle_system_tool`,
    `interp_vs_compiled`, `audit_only`, or `negative_expected_failure`.
31. Add real stdlib workload checks before Phase 8 relies on the library.
    Start with `examples/base64_cli`; it is the first
    workload, not just one item in the set. Then broaden to
    `json_validator`, `ini_parser`, `checksum_cli`, `http_headers`,
    `path_normalizer`, and `lru_cache` or `ring_buffer`. Wire them with
    `scripts/tests/check_stdlib_workloads.sh`. Each workload must build, run,
    compare interpreter-vs-compiled output, and report authority/allocation/
    evidence classes. The oracle requirement is explicit per workload:
    `base64_cli` against Python `base64`, `json_validator` against Python
    `json`, `ini_parser` against a checked-in Python reference,
    `checksum_cli` against Python/hashlib or a checked-in reference,
    `http_headers` against checked-in vectors, `path_normalizer` against
    checked-in platform-specific vectors, and `lru_cache`/`ring_buffer` against
    a checked-in reference model.
32. Add a stdlib sentinel/arithmetic audit before broadening hosted APIs. The
    checked-arithmetic flip exposed syscall/sentinel-style code that relied on
    silent wrap (`-1 as unsigned` followed by `+ 1`, size/error sentinels,
    bit-packed flags, checksum/hash arithmetic). Add
    `scripts/tests/check_stdlib_sentinel_arithmetic.sh` or fold the checks into
    `check_stdlib_workloads.sh`: every intentional modular/sentinel operation in
    `std.fs`, `std.process`, `std.net`, `std.io`, hash/checksum code, and binary
    parsers must use explicit `wrapping_*`/bit operations with a comment or a
    report-visible classification; every non-intentional trap must be fixed as a
    real bug, not papered over. The gate should include at least one non-
    arithmetic workload path (text/path/UTF-8, fs/process/env capabilities,
    maps/iterators, formatter/parser round-trip, or proof/report views) so the
    post-arithmetic validation does not overfit to numeric examples.
33. Add the Phase 7 validation project:
    `examples/stdlib_client/` plus `scripts/tests/check_phase6_stdlib.sh`.
    The client must use `std.option`, `std.result`, `std.bytes`, `std.text`,
    `std.path`, `std.vec`, `std.map`, `std.fs`, `std.io`, `std.fmt`,
    `std.parse`, either `std.json` or `std.base64`, deterministic RNG or
    checksums, sanitizer/runtime-instrumentation hooks where available, and
    `std.test`. CI must build, run, test, audit authority/allocation/evidence
    classes, and compare interpreter-vs-compiled behavior.

## Phase 8: Flagship Depth And Examples

Goal: produce examples that outside systems engineers find impressive, not only
internally coherent.

Done when: the showcase set includes a serious security/crypto or protocol
example with proof/evidence strong enough to anchor the public pitch.

HMAC and `constant_time_tag` are complete flagship baselines recorded in the
changelog. This phase maintains the graduated showcase, deepens theorem
coverage where it strengthens public claims, and adds new examples only when
they force a named surface or public claim.

This phase is also the external-credibility probe for the compiler/evidence
pipeline: Phase 6B's `diff-caps` artifact gives the first narrow reviewable
delta, and this phase turns that style of replayable evidence into a non-author
example or workload transcript before the larger package/release machinery
depends on it.

1. Maintain the five graduated flagships and keep their evidence bundles green:
   `parse_validate`, `crypto_verify`, `fixed_capacity`, `constant_time_tag`,
   and `hmac_sha256`.
2. Add stretch theorem for `fixed_capacity`: multi-iteration ring invariant or
   stronger push/search property.
3. Add stretch theorem for `parse_validate`: success-path / failure-completeness
   theorem once proof ergonomics support it.
4. Audit the next stronger real-crypto candidate only if it forces a new public
   claim: Ed25519 verification subset, AEAD, or a post-quantum primitive.
5. Add only the ProofCore surface that candidate forces: shifts, bitand, u32
   compound loops, rotations, byte-to-word packing, and multi-round invariants.
6. Keep `hmac_sha256` as the regression anchor for exact-extraction,
   spec-drift-tied refinement: source perturbations must make the registered
   proof stale, and ProofKit refactors must keep the 11 proof checks green.
7. Keep the paper, website, README, and showcase manifest aligned with HMAC's
   actual claim: exact extracted source refines an independent SHA-256/HMAC
   spec under named assumptions and trusted backend boundaries.
8. Use HMAC-derived proof patterns only after they move into ProofKit or an
   explicit example guide; do not let future flagships copy private
   `Sha256Refine` scaffolding as hidden infrastructure.
9. Graduate one runtime-error-obligation flagship: parser/protocol example with
   no OOB/div-zero/overflow obligations discharged.
10. Graduate one authority/capability flagship: a privilege-separated tool whose
   trusted core cannot touch files/network/processes except through named
   wrappers.
11. Graduate one FFI-wrapper flagship: trusted C boundary, safe pure core,
    explicit assumptions, layout/ABI evidence.
12. Graduate one ownership-heavy resource flagship: explicit cleanup,
    borrow-heavy APIs, no leaks/double-use, and evidence explaining why.
13. Keep the curated showcase balanced: parser/protocol, bounded state,
    crypto/security, authority, FFI/trust, ownership-heavy.
14. Add a Unix-tool/protocol compatibility flagship that demonstrates bugs
    memory safety alone does not catch: byte-preserving I/O, path/OS-string
    handling, handle-relative filesystem authority, exit-code compatibility,
    error behavior compatibility, ignored-result diagnostics, and oracle tests
    against a reference implementation.
15. Add a thin end-to-end credibility slice before the larger workload ladder,
    so skeptical users can replay one compelling artifact before the full
    Phase 5/6/12/13 surface is complete. Target:
    `examples/credibility_slice/packet_window/` or an equivalent no-alloc
    protocol parser/verifier (WebSocket frame decoder, TLS record-header parser,
    DNS packet parser, or HTTP/1 header parser). The slice must demonstrate the
    actual production-readiness claim, not a toy proof: fixed buffers, `Bytes`,
    `ByteView`, arrays, `Result`, pattern matching, explicit capabilities,
    value-model accessors / scoped borrows, no returned references, and bounded
    allocation or no allocation. It must include positive and negative corpus
    fixtures, fuzz/property-style generated cases with persisted
    counterexamples, an oracle or interpreter-vs-compiled check, one
    runtime-safety obligation, one source contract, one Lean-checked proof, one
    `proved_by_kernel_decision` discharge, and one audit/release bundle. Wire
    `scripts/tests/check_credibility_slice.sh`; the gate must compile and run
    the example, replay the proof/evidence checks, emit `--report contracts`,
    `--report obligation-ledger`, `--report caps`, `--report layout`, and
    `--report fingerprints`, persist any counterexample as a regression, and
    produce a README transcript that a non-author can run without understanding
    compiler internals. This is intentionally a vertical slice, not a new
    parallel track; it exists to validate the bet before the later 10k-line
    workload ladder. Do not start this slice until the immediate Phase 6
    blockers that directly affect parser/buffer code are either done or
    explicitly deferred with examples: array-literal element inference (#6),
    match guards / OR patterns / match-on-reference (#5), and `defer`/cleanup
    (#7) if the chosen slice owns resources.
16. Add a graduated real-workload ladder. The goal is to make sure Concrete
    builds real things that can be checked against references, not only tiny
    proof demos. Each workload must name the surface or public claim it forces;
    otherwise it does not belong in this phase. Do not jump straight to multiple
    10k-line ports before the Phase 5 core slab, Phase 7 stdlib, and daily
    workflow can support them; that would mostly test missing ergonomics.
    Each accepted workload, including #35, must leave behind a checked gap
    report (`WORKLOAD_REPORT.md` or manifest fields) naming: missing stdlib
    APIs, painful syntax, ownership/copy friction, error-handling friction,
    proof/contract friction, runtime trap/debug friction, capability/authority
    surprises, cold and representative edit/rebuild latency, predicted Phase 6C
    invalidation fanout, docs/tooling gaps, and which later roadmap item should absorb
    each lesson. A workload is not complete if it only builds; it must either
    feed the linear roadmap or prove that no new item is needed.
    Sequence:
    - **Main compiler repo:** keep tiny proof patterns
      (`examples/proof_patterns/`), evidence-class examples, small real programs
      that gate the compiler, and showcase flagships here. These protect
      compiler/proof correctness and should stay close to the tests.
    - **Medium in-repo real programs after the Phase 5 core slab and Phase 7
      stdlib:** build exactly these first six examples under
      `examples/workloads_medium/`: `mini_toml`, `http_headers`, `tar_index`,
      `bytecode_vm`, `lru_cache`, and `checksum_cli`. Wire
      `scripts/tests/check_medium_workloads.sh`. `mini_toml` forces
      bytes/text/path diagnostics and parse errors; `http_headers` forces
      ignored-result diagnostics and byte-preserving parsing; `tar_index`
      forces path/OS-string boundaries, archive offsets, overflow/cast
      obligations; `bytecode_vm` forces modules, dispatch, bounded loops;
      `lru_cache` forces collections, ownership, frame facts; `checksum_cli`
      forces project model, `concrete test`, and oracle comparison. Tier exits
      only when at least two medium programs build, run, pass
      interpreter-vs-compiled checks, carry full evidence/trust classification,
      and are covered by runtime-obligation audit.
      At least two of these must follow the Phase 7 pipeline recipe
      (`read/acquire -> parse -> validate -> transform -> emit/release`) and
      include a short transcript showing where capabilities enter and where the
      pure core starts. This borrows the teaching value of Elm's one clear app
      shape and Roc's host/application split, while keeping Concrete's
      authority/evidence boundaries explicit.
    - **Separate workload repo later** (`concrete-workloads`): use it for
      larger ports, 2k-10k line
      programs, compatibility suites, external-user programs, benchmark
      workloads, and reference/oracle data that would bloat the compiler repo.
      Do not create this repo until modules/imports, `Concrete.toml`,
      `concrete test`, bytes/text/path, collections, stdlib core APIs, and basic
      diagnostics are usable. The first external-user workload in this repo is the
      external-validation-gate trial from the cross-cutting checkpoint. The repo
      must pin a Concrete compiler/toolchain version in
      `concrete-workloads/Concrete.lock`, include `workloads.toml` with
      `name`, `size_lines`, `source_language`, `oracle`, `evidence_class`, and
      `required_concrete_version`, and be validated by release CI through
      `scripts/tests/check_external_workloads_repo.sh` so it does not silently
      rot as the language changes.
    - **Ported compatibility examples after the daily workflow is stable:**
      `wc`/`cat`/`sha256sum`-style tools, QOI image decoder, base64 library,
      INI parser, tiny glob/regex matcher, arena allocator demo, MMIO-mock
      driver, or protocol codec. Annotate forcing surfaces: glob/regex must
      stay bounded or explicitly outside `PredictableV1`; arena allocator demo
      waits for allocation-profile work that needs it; MMIO-mock driver waits
      for the Phase 16 `with(Device)`/MMIO evidence decision or explicitly
      pulls that decision forward.
    - **10k-line stress ports** only after daily workflow is stable enough that
      the port tests Concrete rather than fighting missing basics. First
      accepted port set: `inih`, `cJSON`, `qoi`, `tiny_regex`, and `tiny_tar`.
      Each port must live under `concrete-workloads/ports/<name>/` and include
      `PORT.toml` with `upstream_url`, `upstream_commit`, `license`,
      `ported_subset`, `line_count`, `unsupported_features`, `oracle_command`,
      `known_trust_boundaries`, and `lessons_for_concrete`. Wire
      `scripts/tests/check_10k_ports.sh`. Tier exits only when at least one
      large port has a pinned reference/oracle suite, interpreter-vs-compiled
      differential coverage, runtime-obligation audit, and release-CI replay.
    Each workload must have a check story: oracle/reference comparison,
    interpreter-vs-compiled differential tests, runtime-obligation audit, and
    explicit evidence/trust classification for what is proved, tested, assumed,
    or trusted.
17. Do not run broad examples cleanup/polish sweeps. Clean examples
    opportunistically when a roadmap task touches them. Improve examples only
    when they serve proof-link migration, `concrete prove` authoring,
    external validation, or a release-facing tutorial. Add an example-refresh
    checkpoint at every phase closure, and after every two substantial
    Phase-6/7 usability increments. The checkpoint is not a broad rewrite; it
    is a small, gate-backed audit that asks whether graduated examples and
    release-facing docs still teach the current language. It must remove stale
    "deferred" language for newly landed features; update examples that should
    now use the preferred surface (`if let` / `while let`, range patterns,
    future guards/OR patterns, value-model collection access, `ByteView`,
    explicit capabilities); keep legacy examples only when they intentionally
    demonstrate the old/low-level form; and record any skipped update with a
    reason. Add `scripts/tests/check_example_refresh.sh` once the first refresh
    has enough concrete assertions; until then, each phase closure commit must
    name the examples/docs it checked and why no refresh was needed. The goal is
    to prevent tutorial/showcase drift without turning every feature into a
    repo-wide churn pass.
18. Upgrade the constant-time flagship from `reported` to `enforced` with a
    secret-dependent-flow checker. Today `constant_time_tag` reports a
    constant-time source shape and leaves machine timing `assumed`; add a
    source/IR information-flow pass that rejects secret-tagged values reaching
    branch conditions, loop bounds, or array indices, so the discipline becomes a
    compiler-enforced structural property, not a reported shape. This needs no
    hardware/timing model and must not claim machine-level timing: it produces
    `enforced` for the source-flow property only, with machine timing still named
    `assumed`. Mark secrets with an explicit annotation (e.g. `#[secret]`); the
    checker reports `enforced` or a counterexample flow path back to source. Add
    `examples/secret_flow/` with a clean constant-time case and negatives for a
    secret-dependent branch, a secret-dependent index, and a secret-dependent
    loop bound. Wire `scripts/tests/check_secret_flow.sh`; the gate must reject
    every negative and must never present source-flow enforcement as timing
    proof.
19. Add the Phase 8 validation artifact: a showcase/workload dashboard that
    proves every flagship and graduated workload has a check story, evidence
    bundle, oracle or reference when appropriate, interpreter-vs-compiled
    coverage, property-test/counterexample-regression coverage where relevant,
    runtime-obligation audit, trust/assumption classification, and release-CI
    replay. The first external-user workload in this dashboard is the
    external-validation-gate trial. Also publish representative cold pipeline
    timings and Phase 6C shadow invalidation traces for no-op, private-body,
    public-interface, proof/contract, policy, and target edits; those traces are
    the workload-derived input to Phase 8.5, not a cache implementation here.
    If the external-validation trial finds manual proof authoring too costly,
    this artifact must also run the exact narrow Phase 9 #14a synthesis probe
    selected by the gate, preserve its transcript and review-cost measurements,
    and record the final GO/NO decision consumed by the Phase 8.5 trigger. It
    may not silently broaden that probe into the general synthesis roadmap.

## Phase 8.5 / 8B: Incremental Compiler Driver And Artifact Reuse

Goal: make unchanged compiler, proof, report, and backend work reusable without
creating a second semantic pipeline or letting stale evidence remain green.

Trigger: start implementation only after the Phase 8 external-validation gate
returns **GO**, Phase 6B has stable identities/dependency/invalidation contracts
and structured validation records, Phase 6C's shadow edit corpus is green, and
at least one medium Phase 8 workload is large enough to choose granularity from
measurements rather than toy programs.

**Conditional phase rule.** The roadmap remains linear. After Phase 8, a GO
verdict executes this phase before Phase 9. A NO verdict must produce the
required decision record and mark Phase 8.5 explicitly
`rejected-by-validation` or `deferred-by-decision` before Phase 9 proceeds; this
slot may not remain silently blocked while later work advances. A later reversal
requires a new forcing workload and recorded decision.

Prerequisites: Phase 6B determinism, one composed pipeline, stable fact IDs,
dependency graph, pass locality/invalidation, and validation-record contract;
Phase 6C telemetry/replay/shadow manifests; Phase 8 workload edit traces. The
general engine must land before Phase 9's proof cache so proof reuse does not
become a second private database.

Non-goals for V1: no per-token/per-expression query graph, no Datalog or
user-facing rule language, no public long-term compatibility promise for the
opaque internal cache encoding, no remote cache, and no evidence upgrade from a
cache hit or hash match. Start serial and conservatively module/function-grained;
refine granularity and add parallelism only after measurements and equivalence
gates justify them.

Done when: clean, incremental-off, incremental-on, and verify/dual-run modes
produce the same facts, diagnostics, obligations, reports, Core/SSA/LLVM
behavior, and native result; the edit matrix proves conservative bounded
invalidation; no-op and leaf edits reuse semantic and object artifacts; cache
corruption/schema/tool drift recompute or fail closed; and no cached partial or
stale artifact can fabricate a validated boundary or stronger evidence class.

Timing of query-shaping (a deliberate decision, not an omission): the
query/`CompilerSession` form below is built HERE, as part of this
validation-gated driver — it is NOT pulled forward as a pre-GO "query monad
seam." Because this whole engine is gated on the external-validation GO,
threading a query monad through the pipeline earlier would be speculative
insurance against a build that may never happen, plus churn against the
still-moving Phase 6B fact graph. The bounded call-site conversion from the
batch/eager pipeline into queries is accepted as part of this build, post-GO.
Do not re-open a "do the seam early" debate: the seam's only payoff is avoiding
that conversion, and that payoff exists only if the bet validates.

0. Freeze every build revision into an immutable `ProjectInputSnapshot` before
   evaluating queries.
   The snapshot owns canonical project/file/module discovery and exact file
   bytes; manifest/lock/policy/profile/target; stdlib and dependency roots;
   configured cwd/path normalization, symlink and case policy; SDK/sysroot and
   runtime/startup inputs; allowlisted environment variables; and compiler,
   checker, solver, clang/LLVM, linker, and other tool identities. Watch events
   only trigger resnapshot/content hashing. Query evaluation may perform no
   ambient filesystem/environment/tool discovery outside declared input queries,
   and an edit during a build belongs to the next revision rather than mutating
   the active snapshot.

   Give source-bearing artifacts two identities where justified: an exact-source
   revision digest for bytes/spans/diagnostics/provenance, and a canonical
   semantic AST/interface digest for semantics-preserving reuse. Reusing a
   semantic result across whitespace/comment/span edits requires a checked
   semantic-hash equivalence rule plus a refreshed revision/span map; span-bearing
   output always depends on the exact revision. Gate changed environment/std
   path, SDK/sysroot/tool binary, symlink/case alias, add/delete/rename, and
   edit-during-build snapshots.

1. Define one typed query/artifact contract and one `CompilerSession` driver.
   Add a closed `QueryKey` family and an explicit typed `CompilerDB` fact/edge
   store owned by the session. Split every result into (a) a canonical hashed
   payload + semantic dependency manifest and (b) non-hashed operational/view
   metadata. Only normalized semantic fields, schema, subject identity, and
   declared dependency digests enter `output_digest`/the dependency root.
   Timings, hit/miss state, cache path/location, absolute/local paths, rendered
   replay commands, redaction/display choices, and other operational metadata do
   not. Exact-revision diagnostics/provenance use their own revision-bound
   digest.

   Keep orthogonal status dimensions: the existing proof/evidence class;
   `pipeline_validation_status` (`unvalidated | compiler_validated`);
   independent `certificate_status`; and `replay_status`. Cache/integrity/
   certificate states may not become new proof-evidence classes. The result
   envelope also carries dependency keys/digests, producer validation-record id,
   fact/diagnostic ids, provenance roots, cacheability, and replay-plan identity.
   Required initial families:

   - project input: snapshot, loaded-file/module graph, manifest/dependencies;
   - file: source/lex/parse/interface-summary/body;
   - module: resolve, desugar, check, elaborate, Core canonicalize, CoreCheck;
   - function or explicit SCC: calls, ownership/capabilities/effects, ProofCore,
     obligations, discharge inputs, and typed report facts;
   - monomorphized instance: stable definition id + canonical type arguments,
     monomorphization and post-Mono verification;
   - codegen unit: lower, raw-SSA verify, SSA cleanup, post-cleanup verify, LLVM
     emission/object from `ValidatedSSAUnit` in V1;
   - project: module graph, reachability, policy aggregation, link plan, and
     release/bundle view.

   If several production stages remain one composite query, its contract must
   name every enclosed stage and verifier so `runFrontend`, desugar,
   canonicalization, cleanup, or post-pass verification cannot disappear from
   the incremental path. Phase 15 schema-bumps codegen inputs from validated SSA
   to `ValidatedBackendIR`; Phase 8.5 does not depend on future BackendIR.

   Expose `--incremental=off|on|verify` (or equivalent internal modes). Strict,
   tolerant, audit, proof, test, report, and codegen operations are demand sets
   over the same stage functions, never alternate pipelines. Formatting,
   redaction, proof-tool, target, profile, and policy inputs invalidate only
   query families that declare them. The eager `off` mode remains the oracle
   and debugging path.

   Add a query-effect firewall: filesystem reads/discovery, environment
   variables, locale/timezone/time, tool/solver discovery and identity, target
   data, policy, and external process results enter computation only through
   declared input/query APIs. Dynamic dependency recording cannot see ambient
   inputs read behind the query engine, so bypassing the firewall is a cache
   correctness bug and fails a mutation gate.

2. Add deterministic indexed compiler data and linear-time builders before
   persisting today's hot whole-program scans.
   Introduce measured `ProgramIndex`/`OrderedIndex`-style structures for modules,
   symbols, types, functions, layouts, obligations, stable-id lookup, reverse
   imports/calls, and monomorphized users. Maps/intern tables may serve lookup;
   canonically ordered arrays/lists own iteration, serialization, and output so
   hash-table order never leaks. Use dense arrays where stable ids permit them
   and builders/reverse accumulation for large strings and collections.
   Index-heavy compiler artifacts must carry table/arena identity in the type
   or validated artifact schema (`FunctionId`, `TypeId`, `BlockId`,
   `ObligationId`, not raw integers). Serialized artifact bytes are hostile
   until decoded and validated: cross-table IDs, stale generation IDs where
   slots can be reused, and out-of-range IDs must be rejected before they become
   semantic facts or cache hits.

   This is profiler-driven, not a blanket ban on `List`: replace hot repeated
   `find?`/`contains`, `acc ++ [x]`, growing-string append, and repeated global
   scans selected by Phase 6C telemetry and bug 027's corpus. Add duplicate-key
   and index-finalization verifiers, plus mutations that reintroduce a
   quadratic builder and that swap two incompatible typed IDs; both must fail
   their gates.

3. Make reusable frontend artifact boundaries operational.
   Add or validate durable `ParsedFile`, split `InterfaceSummary` and body
   summary, `ResolvedModule`, checked module/program artifact, elaborated module,
   and validated Core-module boundaries. Check may no longer return only `Unit`
   if downstream reuse needs its ownership/capability/value-flow decisions; it
   must produce the durable facts or typed artifact that Elab and later queries
   consume. Initially keep imported body digests in the conservative dependency
   envelope anywhere resolution, trait/method lookup, Check, or Elab can still
   inspect bodies. A module may depend on imported **interface-only** digests—and
   therefore survive a dependency-private body edit—only after the interface
   completeness gate proves every consumer uses the split artifact rather than
   a body side channel. Public API identity and implementation-evidence identity
   remain separate. Even then, the unchanged interface permits reuse only for
   importer frontend/interface queries: referenced generic bodies,
   call/effect/proof facts, monomorphized instances, implementation evidence,
   and codegen still depend on the applicable body digest. Gate both an unused
   nongeneric private-body edit whose importer frontend remains a hit and a
   referenced generic-body edit that invalidates the affected mono/object path.

   Strict and tolerant/partial artifacts use different result types. A partial,
   recovered, cancelled, or internal-error artifact cannot satisfy strict
   codegen, proof, policy, package, or release queries. Introduce only boundaries
   demonstrated useful by the workload; `CheckedModule` is a candidate name,
   not permission to add a redundant typed tree.

4. Implement the serial in-memory query engine and conservative
   reverse-dependency invalidation.
   Record every actual dependency read while evaluating a query and maintain
   reverse edges. Initial recomputation units are file, module, function/SCC,
   mono instance, codegen unit, and project as listed above; do not create a
   query per expression merely because an id exists. Call/import/recursive
   cycles are explicit SCC or fixed-point query families, not accidental query
   recursion.

   V1 uses a **complete conservative dependency envelope**. A result may be
   reused only when that whole envelope is unchanged. A digest authenticates
   declared inputs; it cannot detect an omitted semantic dependency. Missing a
   reuse opportunity is acceptable; stale reuse is not. Narrow an envelope only
   after a gate or theorem demonstrates completeness for that query family.

4a. Define deterministic internal codecs before persisting an artifact family.
    Each cacheable type needs a compiler-versioned canonical encoder/decoder,
    explicit ordering and normalization, `decode(encode(x))` semantic equality,
    canonical re-encoding, malformed-input/size-limit fuzzing, and a round-trip
    boundary verifier. Summary hashes from Phase 6C are not a serialization
    format. Internal codecs may invalidate wholesale across compiler versions;
    Phase 18 separately owns stable public package encodings.

5. Add an opaque compiler-versioned local content-addressed store under
   `.build/concrete-cache/` (or an equivalent project-local path).
   Separate the memo/action table from the content store:
   `QueryKey + canonical input/dependency fingerprint -> ResultManifest +
   output digest`, then `output digest -> canonical artifact bytes`. Keys include
   compiler build/internal schema, semantic input/dependency digests,
   invariant-set version, target/data layout, build/test profile, policy only
   where consumed, and relevant proof/backend/tool identity. Add
   atomic writes, locking, checksums, interrupted-write recovery, size/GC policy,
   `concrete cache status --json`, and `concrete clean --cache --dry-run`.

   Serialized cache entries are untrusted bytes for decoding/integrity purposes.
   Loading must follow an explicit `UntrustedCached* ->
   UntrustedDecodedArtifact` API after schema/digest/dependency/subject checks;
   those checks yield only `cache_integrity_checked` bytes bound to the
   **original** producer validation record; they cannot reconstruct
   `ValidatedCore`, `ValidatedSSA`, a proof result, or a release fact. Before
   Phase 14/15, rerun the current
   semantic boundary verifier over every loaded strict Core/SSA artifact before
   constructing its in-process validated token and fresh producer record. If no verifier can reconstruct
   a boundary, that artifact is not safely cacheable for strict codegen, proof,
   policy, package, or release use. This still trusts the compiler's verifier—it
   is not independent certification.

   Treat the project-local store as inside the same user's integrity boundary:
   hashes/checksums detect accidental corruption and substitution relative to a
   trusted action mapping, not a malicious same-user rewrite of both mapping and
   result. Unknown schema/compiler/invariant versions, truncated content,
   conflicting ids, or mismatched dependencies are quarantined and treated as
   ordinary cache misses whenever source inputs allow recomputation. Fail only
   in an explicit artifact-only/offline operation where recomputation is
   impossible. A coordinated malicious rewrite of action mapping, result,
   manifest, and internal hashes is outside the local-cache threat model until
   an authenticated expected action/root exists; tests must not misdescribe
   internal consistency checks as detecting it.

6. Turn proof, obligation, and report work into queries rather than cached
   rendered strings.
   Cache per-function ProofCore extraction, call/SCC classification,
   stable normalized obligations, typed proof inputs, and typed evidence/report
   rows whose schemas already exist. Project
   reports are deterministic views/joins over those typed facts. A cache hit
   preserves the original proof/evidence class and replay provenance; pipeline
   validation remains a separate dimension. It never turns `tested`,
   `solver_trusted`, `assumed`, or `trusted` into proof. Lean, ProofKit, solver,
   synthesis-model, toolchain, spec, theorem, dependency-fact, and policy
   identities affect only the query families that consume them.

   Do not restore `proved_by_lean`, `proved_by_kernel_decision`, or
   `kernel_replayed` from an untrusted cached verdict alone. Cache proof inputs,
   normalized obligations, generated artifacts, exact proof-workspace/import
   dependency digests, and replay plans; replay the
   kernel before emitting the current strong status unless a separately
   authenticated replay receipt is explicitly admitted by policy and named in
   the TCB. Solver/LLM results retain their weaker class and replay provenance.

   Phase 8.5 supplies the query namespace and store; Phase 9 #4a/#15 owns the
   shared proof-artifact schema and activation of reusable Lean/solver verdicts,
   rather than creating a second `.build/concrete-proof-cache/` database. Cache
   only successful strict non-verdict artifacts first. Negative/tolerant
   diagnostics, filesystem discovery, environment-dependent operations, solver
   results, and cancelled work need explicit cache contracts before reuse.

7. Add stable codegen units, an object cache, and deterministic relinking.
   First define `ToolchainIdentityV1` / `BackendInvocationV1`: exact clang/linker
   executable digests (not version strings alone), SDK/sysroot, allowlisted
   environment, target/data layout and ABI, optimization/debug/sanitizer flags,
   runtime/startup objects, and every effective tool flag. These inputs come
   from `ProjectInputSnapshot` and participate in action keys.

   Then add a deterministic `ProgramCodegenPlan` / global ABI-symbol index that
   owns reachability, symbol naming/collisions, whole-program signature/layout
   facts, builtin/helper/type universe, global/string ownership, externs,
   monomorphized-instance ownership, unit imports/exports, and common-versus-test
   partitioning. The current whole-program Mono/SSA/emitter decisions must become
   explicit plan inputs before a module can emit safely in isolation. V1 unit
   input is `ValidatedSSAUnit`; Phase 15 migrates it to validated BackendIR.
   Start with one unit per source module plus stable runtime/test-harness units
   only if the partitionability gate passes. Later subdivision uses deterministic
   stable-id buckets or explicit mono SCCs, never traversal-order chunks.

   The object action key includes validated-SSA unit + codegen-plan digest,
   backend schema, and `BackendInvocationV1`. The link action key includes
   canonically ordered object digests plus linker/startup/runtime identities.
   Cache/materialize the final linked artifact by link key if a retained-output
   warm no-op is expected to skip the linker; otherwise a missing final artifact
   may relink while still reusing every object.

   Gate isolated unit emission, duplicate/orphan mono ownership, missing/
   colliding symbols, common/test partitioning, target/toolchain invalidation,
   corrupt local objects/action mappings, and requested-output materialization.
   A leaf edit recompiles only its measured affected units plus link; a retained
   warm no-op invokes neither clang nor linker. Phase 15 later adds independent
   BackendIR relation checking. Before a native/object validator, an object hit
   remains backend/compiler-trusted and its hash proves identity only relative
   to the trusted local action mapping—not that LLVM produced correct code.

8. Add a long-lived service/watch surface over the same session, then
   deterministic independent-query scheduling.
   The initial engine is serial. Once serial reuse is correct, allow independent
   file/module/codegen-unit queries to execute in parallel with cancellation and
   bounded resources. Serial and parallel **Concrete-owned canonical**
   artifacts/facts must be byte- and schema-equivalent; external LLVM/object
   nondeterminism follows the explicit Phase 15 policy and must not change
   compiler facts or native behavior. Cancellation or a worker crash must not
   commit partial cache state. Canonically sort diagnostics/events whose order
   is otherwise scheduling-dependent. The internal service exists to preserve a
   session across CLI or scripted edits; Phase 19's LSP must wrap this exact
   session rather than a batch `runFrontend` or editor-only cache.

9. Add cache/query observability and a conservative rollout.
   Extend pipeline events with `executed | memory_hit | disk_hit | invalidated |
   uncacheable`, reason, dependency ids, bytes, time, validation-record id, and
   the separate evidence/pipeline-validation/certificate/replay statuses. Roll
   out `shadow -> opt-in -> CI verify/dual-run -> default-on`
   only after each stage's corpus is green. `verify` recomputes selected hits and
   compares canonical results; any false hit disables that query family and
   persists a minimized regression. Audit/why/diff output may explain reuse but
   may not cite reuse as correctness evidence.

10. Add the Phase 8.5 validation artifact.
    Create `examples/incremental_driver/`,
    `tests/perf/compiler_pipeline/incremental_driver/edits.json`,
    `scripts/tests/check_incremental_driver.sh`,
    `scripts/tests/check_codegen_object_cache.sh`,
    `scripts/tests/check_incremental_failure_recovery.sh`, and the umbrella
    `scripts/tests/check_phase8_5_incremental.sh`.

    Cover no-op; whitespace/comment; private leaf body; public
    signature/type/capability/contract; dependency private body versus
    interface; generic body/new instantiation; proof link/theorem/spec; policy;
    target/profile; file add/delete/rename; syntax/type error then repair;
    cancellation; cache truncation/corruption; schema/compiler/checker/tool
    drift; coordinated action-map/artifact/dependency rewrite (recorded as
    outside the unauthenticated local-cache threat model, never falsely
    "detected"); object
    substitution; ambient-input firewall bypass; and serial/parallel modes. Compare
    incremental-off/on/verify facts, diagnostics, obligations, reports,
    Concrete-owned canonical Core/SSA/LLVM and manifests byte-for-byte; compare
    normalized external-object metadata and native behavior unless the pinned
    toolchain is separately proved reproducible. Assert query execution sets,
    named unaffected sentinel modules/functions/objects that must remain hits,
    affected outputs that must change, and maximum query-family execution counts
    per edit—not only wall time or the tautology "declared dependencies rebuilt."
    On the Phase 8 large workload, a warm no-op must execute zero **producer
    recomputation** beyond required decode/integrity/boundary-validation queries,
    and a leaf edit must rebuild only declared reverse
    dependencies within the stated count bound. Publish observed/baseline-derived
    no-op and leaf-edit speedups (20x/5x are investigation targets, not phase
    gates or public claims); Phase 17 owns platform-specific release performance
    budgets.

## Phase 9: Proof Authoring And Automation

Goal: make flagship proofs a repeatable engineering workflow, not a collection
of one-off `simp` scripts.

Done when: new flagship proofs can start from useful generated stubs, standard
lemmas, and actionable failure diagnostics.

This phase owns the proof-cost probe from the roadmap spine: before assuming the
proof discipline scales, measure manual proof effort on real flagships, then
measure review effort for the LLM-guided synthesis loop if manual authoring is
too expensive.

0a. Instrument the flagships with PROOF-EFFORT TELEMETRY before investing in
   automation, so the external-validation gate's "was the proof discipline
   worth the cost?" question has data instead of anecdotes: per proved
   function, record Lean proof lines, tactic depth, solver/`bv_decide` time,
   and the source complexity it covers (loops, branches, width casts). Publish
   a small baseline table for hmac_sha256/constant_time_tag and refresh it per
   flagship. Cheap to collect now; impossible to reconstruct later.
1. Move example Lean proofs physically next to their Concrete examples. Target layout:
   `examples/<name>/src/main.con`, `examples/<name>/proofs/Proofs.lean`,
   `examples/<name>/snapshot/...`. Configure Lake/module discovery so these
   files are importable by the proof checker (for example as
   `Examples.<Name>.Proofs` or an equivalent stable namespace), update
   `Concrete.lean`/proof umbrellas so `check-proofs` can see them, retarget
   source `#[proof_by]` / `#[ensures_proof]` links, and update
   `concrete prove --emit-lean` / `--workspace` to prefer the
   `examples/<name>/proofs/` destination. Update the proof-namespace guard to
   reject new example proof modules under `Concrete/Examples/`. Pilot on one
   small example, then migrate all current example proof modules. Keep registered
   spec PExprs in `Concrete.Proof` until the later `ProofCore` /
   `SpecRegistry` split.
2. Add proof minimization: `concrete prove --minimize <obligation_id>` emits
    the smallest source / ProofCore / Lean slice needed to reproduce a failed
    obligation. Output directory:
    `.build/prove/<function>/<obligation_id>/minimized/` with `source.con`,
    `proofcore.lean`, `replay.lean`, `context.json`, and `README.md`. Wire
    `scripts/tests/check_prove_minimize.sh` with one loop VC, one failed
    postcondition, one stale proof, and one SMT counterexample. The minimized
    artifact must reproduce the same status and stable id without unrelated
    functions.
3. Define and document stable theorem naming conventions in tool output:
    `<fn>_refines_spec`, `<fn>_<obligation>_proved`,
    `<fn>_loop_<name>_preserves`, and
    `<fn>_call_<callee>_discharges_requires`. `concrete prove` should suggest
    these names instead of leaving agents to invent them.
4. Add CI gates for the agent-facing proof surfaces: snapshot representative
    `--json` output, validate schema versioning, ensure generated Lean stubs
    parse/check up to the intended placeholder boundary, assert replay JSON
    reports the same statuses as human replay, and assert proof-check JSON maps
    a failing Lean proof back to the intended obligation id.
4a. Define one proof artifact schema shared by obligations, minimization,
    `--why`, generated stubs, synthesis attempts, repair plans, stale-proof
    reports, proof-cache entries, and replay bundles.

    Deliverable: `docs/PROOF_ARTIFACT_SCHEMA.md` plus a schema-versioned JSON
    fixture directory. Required common fields: `schema_version`,
    `obligation_id`, source fingerprint, source span, policy id, evidence
    class, replay command, tool versions, assumptions/trust deltas, and final
    status.

    Done when `--emit-lean`, `--minimize`, `--why`, `--synthesize`,
    `--repair-plan`, proof-cache status, and stale-proof reports all emit this
    shared envelope, and a gate fails if any command invents a private JSON
    dialect.
5. Add human docs only after the binary path exists:
    `docs/AGENT_PROOF_AUTHORING.md` and an optional repo-root `AGENTS.md`
    should summarize the binary workflow and point to the ProofKit guide, but
    they must not be the source of truth for agents using only an installed
    binary.
5a. Keep AI/agent assurance guidance aligned with the implemented proof surface.
    `docs/SPARK_CLASS_ASSURANCE.md` is the current design-target guide for
    agents: it tells Claude/Codex-style tools which assurance annotations are
    implemented today, which are future-only, and which replay commands must
    validate a claim. When loop invariants, frame/dependency contracts,
    ghost/spec code, or package evidence land, update this guide in the same
    commit as the feature and add at least one agent-facing example of the
    intended suggestion pattern.
6. Add MCP only after the CLI/JSON/stub/workspace surfaces are stable. The MCP
    server should wrap the binary rather than duplicate logic, exposing resources such
    as `concrete://prove/<fn>/obligations`, `concrete://proofkit/lemmas`, and
    `concrete://examples/evidence-classes`, plus tools for `prove_json`,
    `show_obligation`, `emit_lean`, `check`, `replay`, and `check_proofs`.
7. Build reusable proof lemmas for arrays: lookup, update, length, in-bounds,
    OOB stuck behavior.
8. Build reusable lemmas for loop-carried state and `while_step`.
9. Build reusable lemmas for BitVec operations used by flagships.
10. Build reusable lemmas for structs, fields, enum construction, match, Result,
    Option, and bounded-buffer invariants.
11. Upgrade generated proof stubs for real shapes: arrays, structs, enums,
    fixed buffers, Result/Option, loops, source contracts, and refinement
    composition. Stubs should emit spec target, `PExpr` body, FnTable skeleton,
    expected theorem statement, common imports/tactics, and TODO blocks for
    loop invariants. These items enrich what `--emit-lean` produces; they do
    not introduce a second stub generator. Provide a friendly alias such as
    `concrete prove <file> <fn> --stub` only if it is the same artifact path and
    schema as `--emit-lean`, not a second proof surface.
12. Add generated composition scaffolds: FnTable entries, call lemmas, callee
    refinement dependencies, and composed theorem skeletons.
13. Add generated loop-invariant templates for common proof shapes:
    counter loop over array writes, copy loop, fold loop, multi-store loop,
    offset loop, and block-processing loop.
14. Improve failed-proof diagnostics after `--json`, failed artifacts, and
    `--minimize` exist: classify common failures into actionable categories
    such as missing callee theorem, stale source link, missing table entry,
    failed arithmetic bridge, insufficient frame fact, and spec/extraction
    mismatch. Add `concrete prove <file> <fn> --why <obligation_id>` (or an
    equivalent `--show-obligation --why` form) to explain why the obligation
    exists, which source span generated it, which facts are in scope, what
    evidence classes are allowed, and why automation did not close it.
    Diagnostics should point to the already-generated artifact or next action
    instead of introducing another parallel proof surface. Where a failed proof,
    failed contract, or solver counterexample has concrete inputs or a finite
    witness, emit a runnable `.con` counterexample fixture plus a replay command.
    This is the Dafny/SPARK/F* lesson in Concrete's terms: proof failure should
    feel like debugging a minimized program, not reading a wall of theorem-state
    text. The fixture is evidence of failure only; it must never upgrade a claim
    to proof.
14a. Add **LLM-guided proof synthesis, kernel-verified** as a first-class
    proof-authoring workflow, not as prose-only AI help. Command shape:
    `concrete prove --synthesize <module.fn>` (or an equivalent subcommand)
    runs a closed loop: Concrete emits exact obligations; an LLM proposes loop
    invariants, helper lemmas, or Lean proof scripts; Lean's kernel /
    kernel-backed decision procedures check the artifact; failed attempts are
    minimized and fed back into the next attempt; and only replayed
    kernel-checked artifacts may upgrade evidence. LLM output is never
    evidence by itself. Reports must record model/tool identity, attempt count,
    generated assumptions, rejected assumptions, introduced trusted boundaries,
    final evidence class, replay command, and whether the final artifact works
    without the LLM. Design target:
    `docs/PROOF_SYNTHESIS.md`.

    Product model: the human reviews the spec, assumptions, and evidence class;
    the kernel checks the generated proof artifact. Do not design this as
    "trust the generated proof" or "the LLM decides correctness." If the spec
    is weak, vacuous, or wrong, synthesis has only made the wrong theorem cheap;
    Phase 10/11 spec-adequacy, vacuity, and mutation gates are therefore part of
    this feature's safety story.

    Deliverable: a replay bundle, not an LLM transcript. The bundle must contain
    the obligation, generated candidate artifacts, accepted proof/invariant/
    lemma artifacts, rejected attempts with reasons, policy decisions, and a
    `concrete replay` command that validates the final evidence without network
    access or model access.

    Gates: synthesize a loop invariant for a small loop proof; synthesize a
    missing helper lemma; repair a stale proof after source drift; reject a
    plausible but false proof; reject any attempt that introduces a new
    assumption, trusted fact, extern/Unsafe dependency, or solver-trusted fact
    without policy approval; and produce a replay bundle that verifies without
    the LLM or network. Add red-team fixtures where the agent proposes a proof
    of the wrong theorem, a vacuous spec, an unapproved assumption, and a proof
    script that typechecks only after weakening the claim.

    Agent discoverability is part of the feature, not documentation garnish.
    The installed binary must expose a machine-readable feature/command catalog
    (for example `concrete agent features --json` or `concrete help --json`,
    extending the Phase 6E command catalog rather than inventing a second one)
    that tells LLM tools which proof commands exist, what artifacts they emit,
    what evidence classes mean, which replay command validates a result, and
    which actions are forbidden without policy approval. The same facts should
    be reachable through any later MCP server, but the CLI/JSON catalog is the
    source of truth so agents can discover the workflow without reading the
    repository docs.
15. Add proof-result caching once proof artifacts and fingerprints are stable.
    This item is active only when Phase 8.5 completed after a GO verdict; if
    Phase 8.5 was rejected/deferred, proof replay remains uncached rather than
    growing a private workaround. Implement caching as Phase 8.5 query families in the one
    `.build/concrete-cache/` content-addressed store, not as a separate proof
    database. Cache key: compiler/query schema, toolchain version, source and
    typed-Core fingerprints, spec/proof link, obligation id, dependency-fact
    fingerprints, ProofKit version, proof/solver/synthesis engine version, and
    policy mode where it affects the verdict. Stale or dependency-mismatched
    entries become `needs_recheck`, never green. An LLM-synthesized proof is not
    cacheable as evidence until Lean replays it, and a hit preserves the
    recorded evidence class rather than upgrading it.
    Expose
    `concrete prove --cache-status --json`. Wire
    `scripts/tests/check_proof_cache.sh`; the gate must prove cache hits do not
    mask stale source, stale theorem names, changed policies, or changed solver
    trust settings, and that `--incremental=off|on|verify` produces identical
    proof statuses and replay artifacts.
16. Add simple auto-discharge for structural obligations that do not need human
    proof search. V1 shapes: reflexive field projection, tuple/struct
    constructor-destructor round trips, enum tag preservation, fixed-array
    literal length, direct call wrapper, and source-contract metadata erasure.
    Command surface: `concrete prove --auto <function> --json`, reporting
    `auto_closed`, `needs_lean`, or `not_supported` per obligation. Wire
    `scripts/tests/check_structural_auto_discharge.sh`; auto-discharge may only
    emit `proved_by_kernel_decision` or a linked Lean theorem when the kernel
    actually checks the generated proof.
16a. Add operational VC auto-discharge as the next automation tier after
    structural auto-discharge. Today `linear` and `bitvector` obligations route
    to `omega` / `bv_decide`, while `operational` and `refinement` obligations
    route to Lean proof links; that leaves ordinary `#[ensures]` bodies and
    loop operational-preservation steps dependent on hand-written bridge
    theorems.

    **STATUS (2026-06-22): forcing probe RUN — verdict GO.** A fixed mechanical
    tactic over six real hand-proof-shaped VCs closed 4/6 with no bespoke human
    proof step: HMAC/SHA `ch` and `maj` bitvector refinements via `bv_decide`,
    `count_up` loop invariant preservation via `omega`, and a branching
    `validate_version` postcondition after mechanical guard splitting. The two
    misses were crisp V1 engineering gaps, not research walls: `rotr` needs
    generated `Int`/`Nat`/`BitVec` shift-amount cast-normalization side-goals
    discharged by `omega` before the bitvector leaf reaches `bv_decide`, and the
    failing parser case needs automatic guard splitting. The probe gate is
    `scripts/tests/check_operational_vc_auto_discharge.sh` with fixtures under
    `scripts/tests/fixtures/operational_vc_autodischarge/`; it must keep one
    positive file that closes and one boundary file that fails for the expected
    cast/guard gap until the real implementation lands.

    V1 core: symbolically execute a narrow Core/ProofCore fragment, unfold the
    generated eval and named spec, split conjunctions and guards mechanically,
    normalize `Int`/`Nat`/`BitVec` casts, route arithmetic leaves to `omega`,
    route bitvector leaves to `bv_decide`, and report surviving leaves as
    `needs_lean` / `not_supported` rather than false green. Defer whole SHA
    compression rounds, message schedules, heap/alias-heavy code, effectful code,
    and induction-heavy specs until separate evidence shows they fit. Design
    note: `research/proof-evidence/operational-vc-auto-discharge.md`.
16b. Add an automation trust-upgrade firewall for every proof automation path.
    Any auto-discharge, generated proof, solver route, cache hit, replay helper,
    agent suggestion, or future MCP proof tool must emit a replayable artifact,
    name the engine that closed the fact, preserve the evidence class
    (`proved_by_kernel_decision`, `proved_by_lean`, `solver_trusted`,
    `assumed`, `runtime_checked`, `needs_lean`, `not_supported`, etc.), and
    include a negative gate proving unsupported / stale / effectful / trusted /
    out-of-fragment obligations do not become green. This is the guardrail that
    prevents proof automation from becoming a hidden trust upgrade. Wire the
    first version into the structural and operational auto-discharge gates, then
    extend it to proof-result caching, agent-facing proof suggestions, and any
    external solver path.
17. Add a small verified/spec-checked standard proof library for common
    predicates and formal stdlib models: sorted, bounded, no-duplicates,
    fixed-length, prefix, checksum, constant-time source shape, `formal_vec`,
    `formal_map`, `formal_set`, spec-only `bigint`, and the first reusable
    lemma families over those models. Treat these as **formal shadow models**
    for real containers: user proofs reason about the mathematical model, while
    collection implementations prove refinement facts from `Vec`, `HashMap`,
    `OrderedMap`, and `HashSet` to the corresponding formal model.

    Deliverable: one end-to-end refinement slice, for example
    `HashMap<K,V>` operation facts lowering to `formal_map`, with replayable
    Lean artifacts and a user proof that consumes only the formal model facts.

    Done when the user proof does not mention buckets/tombstones/capacity, the
    collection implementation proves the bridge facts, the report names the
    refinement evidence class, and a deliberately false refinement is rejected by
    the checker/kernel. This is the tractability unlock for proving real
    programs that use containers.
18. Add bounded quantified specs for collections, not arbitrary open-ended
    logic. V1 syntax should cover only finite, source-visible domains:
    `forall i in 0..n { P(i) }`, `exists i in 0..n { P(i) }`, and library
    predicates that lower to those bounded forms (`all_bytes_valid`, `bounded`,
    `sorted`, `no_duplicates`, `prefix`, `fixed_length`). These specs must
    lower into ProofCore with explicit bounds, source spans, and generated
    theorem shapes; they may be proved by Lean, by kernel decision procedures
    only for decidable finite fragments, or reported `needs_lean` / `blocked`
    when the fragment is outside automation. Do not add general quantifier
    syntax until a bounded collection fragment has examples and gates. Add
    `examples/quantified_specs/` with `bounded_array`, `all_bytes_valid`,
    `sorted_prefix`, and one rejected unbounded quantifier. Wire
    `scripts/tests/check_quantified_specs.sh`; the gate must prove quantified
    claims are never erased into a vague postcondition and never reported as
    proved unless the generated finite theorem actually checks.
19. Add AI-assisted proof repair only after artifacts, statuses, and replay are
    stable enough to validate suggestions mechanically. The binary surface is
    `concrete prove --repair-plan <obligation_id> --json`; it emits no edited
    source, only candidate next actions with required checks. Required JSON
    fields: `obligation_id`, `failure_class`, `minimal_artifact`,
    `suggested_lemma`, `suggested_imports`, `candidate_tactic`,
    `validation_command`, and `risk`. Wire
    `scripts/tests/check_proof_repair_plan.sh` with missing theorem, stale
    proof, missing frame fact, arithmetic bridge failure, and spec mismatch.
    No repair suggestion may change a proof status until `--check` or
    `check-proofs` verifies it. A convenience spelling such as
    `concrete prove <file> <fn> --repair` may exist only as an alias for the
    same repair-plan artifact; it must not edit source by default and must never
    upgrade evidence without replay.
20. **Frame inference (the proof-scaling cliff).** Every loop/state proof must
   establish not just what an iteration *changes* but what it *preserves* — the
   frame problem (Smallfoot 2006; later Infer; separation logic's frame rule:
   "a proof mentioning only its footprint preserves everything else"). Today
   this is handled *cheaply* and *implicitly*: mutation is functional
   (`List.set` / `Env.bind`), loop invariants are total index-predicates
   (`fun j => if j < m then word j else 0`), and the frame is discharged ONCE as
   the generic `set_in_counter_map` lemma and applied O(1)/iteration via
   `eval_while_count` — so `block_to_words` / `schedule` / `compress` pay no
   per-cell frame cost. This holds only while updates stay single-cell and
   arrays stay non-aliasing. It will NOT scale to scattered/multi-cell updates
   per iteration, multiple aliasing mutable arrays, invariants that are not
   index-predicates, or a future flat mutable-heap / pointer model. Before any
   of those land, design a frame-like annotation or inference pass into
   ProofCore (separation-logic-style footprints, or a `#[frame]`/`modifies`
   clause that auto-derives preservation) so frame conditions never become the
   majority of proof work. Gate: do not build it until a second update shape
   actually forces it (per the operating rules) — the current functional-list
   model gets framing for free.
21. Deferred architecture refactor: split the current `Concrete.Proof` layering
   so registered example specs can move without a cycle, but do not let this
   block the active frontier unless spec ownership or proof authoring starts
   depending on it. Target shape: `Concrete.ProofCore` owns `PExpr`, `PVal`, evaluation,
   `FnTable`, and source-independent semantics; `Concrete.SpecRegistry` owns
   the spec-drift table and imports whichever example spec modules it registers;
   `Concrete.Proof` becomes the generic proof-theorem / compatibility umbrella.
   Only after this split should registered example SPEC PExprs move from
   `Concrete.Proof.*Expr` into `Concrete.Examples.<Ex>.Proofs` or sibling
   `Specs` modules. Preserve the spec-drift tie throughout. Add
   `scripts/tests/check_proof_layering_split.sh`; the gate must prove example
   proof theorems and example spec PExprs live under the example namespace,
   `Concrete.SpecRegistry` still drives spec-drift, `Concrete.ProofCore` has no
   example-owned theorem/spec definitions, `check-proofs` can reach moved
   modules through the umbrella import, and changing a moved example body still
   reports stale/spec-drift rather than silently accepting the old proof.
22. Add the Phase 9 validation artifact: a proof-authoring project that
   exercises `--json`, `--show-obligation`, `--emit-lean`, `--emit-artifacts`,
   `--workspace`, `--check`, `--nearest-lemmas`, `--minimize`, and source-linked
   proof attachment across straight-line, array update, loop copy, fold,
   composition, bounded quantified specs, ghost, stale, missing, partial, and
   repair cases. The gate must typecheck generated stubs, reject any
   `proof-registry.json`, and verify that failing Lean proofs map back to stable
   obligation ids. If Phase 8.5 completed, it must also exercise proof-query
   memory/disk hits, stale dependency invalidation, proof-tool/policy drift,
   corrupt-entry recovery, and cache-off/on/verify equivalence without treating
   a hit as proof evidence. If Phase 8.5 was rejected/deferred, the validation
   artifact runs eagerly and does not create a private proof cache.

## Phase 10: Audit Commands And Review Artifacts

Goal: let a reviewer answer "what can this program do, what is proved, what is
assumed, and what changed?" without reading compiler internals.

Done when: `concrete audit`, semantic diff, and an artifact viewer cover the
five graduated flagships and one package-scale example.

1. Stabilize machine-readable fact schemas for proof status, obligations,
   effects, capabilities, assumptions, policies, snapshots, showcase metadata,
   runtime traps, synthesis attempts, stdlib evidence, and package evidence.
   Keep one shared evidence-class enum and one shared fact vocabulary across
   reports; no report kind may invent private status strings for `proved`,
   `reported`, `trusted`, `assumed`, `runtime_checked`, `tested_by_oracle`,
   `observed_only`, or `stale`. This is the phase where the global **no second
   truth source** rule becomes mechanical: README/site docs, LSP, MCP, agent
   JSON, release bundles, artifact viewers, and package reports must wrap these
   compiler facts or generated snapshots, not restate evidence status by hand.
   Add a drift fixture proving a stale hand-written claim is caught or marked
   explicitly non-authoritative.
   Keep pipeline validation, independent-certificate, and kernel-replay status
   in separate shared dimensions from that evidence-class enum. In particular,
   `compiler_validated`, `certificate_structurally_checked`, a named checked
   relation, and `kernel_replayed` answer different questions and may not be
   collapsed into or used to upgrade a proof/evidence class.
2. Add `concrete audit`: one human-readable plus machine-readable bundle
   covering authority, trust, allocation, proof status, obligations,
   assumptions, policy, snapshots, backend/target assumptions, replay, and the
   proof-story matrix specialized to the audited program.
3. Add `concrete explain <function>`: capabilities, proof status, assumptions,
   obligations, trusted callees, evidence level, and why each status is what it
   is.
4. Add `concrete why <capability>`: explain why a function needs `File`,
   `Network`, `Alloc`, `Unsafe`, etc., including transitive call chains.
5. Add `concrete diff old new`: authority/proof/trust/runtime-obligation diff.
   This is also the first **proof/capability diff for code review** surface.

   Output must include human text and JSON rows for: added/removed capability,
   capability widening/narrowing, new `trusted`/`Unsafe`/extern boundary,
   stale/missing/downgraded proof, new runtime trap site, allocation authority
   change, changed assumption, and changed stdlib evidence class.

   Done when the same JSON drives CI, a GitHub-comment/golden fixture, editor
   display, and agent consumption. This is Concrete's review differentiator:
   evidence changes live where code review happens, not in a separate proof
   report nobody opens. A PR artifact should look like a compact evidence
   changelog: `+ File capability`, `+ trusted function`, `- Unsafe removed`,
   `verify_packet proof became stale`, `bounds obligation discharged`,
   `dependency evidence downgraded`. It must preserve separate evidence classes
   and must not collapse the review into one green badge.
6. Add semantic trust diff gates: capability widening, allocation change,
   trusted boundary addition, stale proof, weakened/missing obligation,
   assumption widening, runtime-obligation change, and stdlib evidence-class
   drift. Add a red-team fixture proving the diff cannot emit a false-clean
   summary when a capability/trust/proof fact changed.
6a. Add early **capability budget files** before full package facts exist.

    Deliverable: a project-local policy file (for example
    `concrete.policy.toml`) with a minimal V1 surface:
    `forbidden_caps = ["Network", "Unsafe"]`, `allowed_caps = ["Alloc"]`, and
    optional `forbid_trusted = true`.

    Done when `concrete check` / `concrete audit` fails a fixture that widens
    beyond the budget, reports the exact function/import that caused the
    widening, and reuses the same fact vocabulary as Phase 18 import fact
    constraints so the policy graduates cleanly to dependency/package
    boundaries.
7. Add `concrete audit --json`: machine-readable audit output for CI,
   dashboards, editor tooling, and release bundles.
7a. Add a **verified profile** command/policy surface that makes "formally
    verifiable code" operational without overclaiming. Candidate spellings:
    `concrete check --profile verified` and/or
    `concrete audit --profile verified`. The profile rejects or fails the
    policy when a selected target contains stale proofs, unresolved proof
    obligations, unapproved `assumed` facts, unapproved `solver_trusted` facts,
    unreviewed `trusted`/extern/`Unsafe` boundaries, unchecked runtime-safety
    obligations, open known holes, or missing replay commands. It must preserve
    evidence classes rather than upgrading them: `tested_by_oracle` remains
    testing, `solver_trusted` remains trusted unless separately replayed in
    Lean, and runtime checks remain runtime checks. Add a gate with one clean
    verified fixture and negatives for stale proof, assumption, SMT-only claim,
    unchecked unsafe, and unresolved runtime obligation.
8. Add an artifact viewer CLI/TUI over facts, obligations, proofs,
   assumptions, release bundles, and diffs. It may show a compact dashboard,
   but it must never collapse different evidence classes into one fake green
   badge: `proved`, `runtime_checked`, `tested_by_oracle`, `assumed`, and
   `trusted` stay distinct on screen and in JSON.
9. Ensure every release bundle includes an evidence replay command.

   Deliverable: `concrete replay <bundle>` rebuilds and rechecks the selected
   tests, gates, proofs, doc snippets, report snapshots, and release facts. It
   emits a summary of memory-safety enforcement, runtime checks, capabilities,
   proofs, trusted boundaries, assumptions, and unsupported facts. It must work
   without network access, without an LLM, and without private source paths.

   Done when the replay gate covers: clean bundle replays green, stale proof
   fails, missing gate fails, changed source fails, schema mismatch fails, and
   an LLM-generated proof bundle replays using only checked artifacts.
10. Make `tested_by_oracle` evidence structured and diffable:
    - add an oracle manifest naming reference, seeds, case count, input model,
      comparison mode, and coverage kind;
    - split cases into boundary, known-vector, random, adversarial, and
      regression buckets;
    - report case counts, seeds, reference identity, comparison mode, and
      `not_proof` evidence level in audit output;
    - save failing cases as reproducible fixtures;
    - optionally cross-check reference, interpreter, and compiled binary;
    - support metamorphic tests where no complete reference exists;
    - flag oracle evidence weakening in `concrete diff` when cases, seeds,
      reference, comparison mode, or boundary coverage shrink.
11. Add property-based contract testing as a cheap counterexample finder, not
    proof. Command surface: `concrete test --contracts --property --json`
    generates inputs satisfying `#[requires]`, executes the function, checks
    `#[ensures]`, runtime obligations, and selected `assert` facts, then shrinks
    failures to a minimal source-level witness. Evidence class:
    `tested_by_property`, always below proof and below solver evidence. Required
    report fields: function, contract id, seed, generator profile, case count,
    shrunk witness, failing postcondition/obligation id, replay command, and
    whether the witness was persisted as a regression. Add
    `examples/property_contracts/` with `clamp`, `bounded_index`,
    `checksum_range`, one precondition-filtered generator, and one deliberately
    false postcondition. Wire `scripts/tests/check_property_contracts.sh`; the
    gate must prove property testing finds and shrinks the false claim without
    ever producing a `proved_*` status.
12. Add counterexample-to-regression persistence for obligation witnesses from
    SMT, property tests, oracle failures, fuzzers, differential mismatches,
    runtime traps, proof failures, and future fuzzed contracts. Command
    surface: `concrete counterexample save <obligation_id> --out
    tests/counterexamples/<name>.con` plus JSON mode. The saved fixture must
    include source inputs, expected failing obligation id, expected status
    (`counterexample`, `tested_by_property_failure`, `oracle_failure`, etc.),
    replay command, and the original tool provenance. Wire
    `scripts/tests/check_counterexample_regressions.sh` with one SMT overflow
    witness, one property-test contract witness, one oracle mismatch, one
    fuzzer/differential mismatch, one runtime-trap witness, and one proof
    failure/minimized obligation witness. The
    gate must fail if a future refactor turns the same counterexample into
    `proved_*` without changing the checked fixture expectation.
12a. Add **observed contract inference from tests** as an explicit non-proof
     on-ramp to specs.

     Deliverable: `concrete infer-contracts <target> --from-tests --json`
     proposes candidate preconditions, postconditions, or invariants from
     passing tests/property cases. Every output fact is marked
     `observed_only` / `suggested_contract`, never `proved`.

     Reports must show sample size, generator profile, counterexamples tried,
     surviving mutants if any, and the command needed to promote the suggestion
     into a checked contract/proof obligation. Gates: infer a useful range
     postcondition from tests; reject or mark weak a vacuous suggestion; prove a
     mutated implementation can invalidate the observed claim; and ensure no
     inferred contract upgrades evidence without a separate proof or
     policy-approved assumption.
13. Add spec provenance and adequacy facts to audit/release bundles: spec name,
    source standard or paper, independent reference if any, test-vector set,
    reviewer, review date, assumptions, and evidence class
    (`spec_trusted`, `spec_reviewed`, `tested_by_oracle`, or future
    `spec_refines_standard`). Do not let a source-to-spec proof imply the spec
    itself is adequate.
14. Add evidence-level monotonicity checks to audit/diff output.
15. Add one AI-audit demo where an agent answers authority/proof/trust
    questions using compiler facts rather than source guesses.
16. Add review checklists generated from facts: what changed, what widened,
    what became trusted, what lost proof, what gained assumptions, and which
    obligations remain open.
17. Add artifact redaction/stability rules so release bundles can be shared
    publicly without leaking local paths, secrets, or machine-specific noise.
18. Keep audit, contracts, obligations, assumptions, policies, manifests, and
    proof-status output on one shared vocabulary. Do not let each artifact grow
    its own mini-language for the same evidence classes.
19. Keep public-facing docs and website copy grounded in the same evidence
    vocabulary. Use `docs/WHY_CONCRETE.md` as the source for a C/Rust-oriented
    "why this exists" page: small systems code, explicit authority, visible
    evidence classes, spec-drift-tied proofs, named trust boundaries, and what
    Concrete deliberately avoids. The website should show the end goal and the
    current honest status, not catchy slogans or one-badge proof claims.
20. [relocated from closed Phase 4 — #42] Add compiler self-audit: `concrete audit
    --compiler` renders the `CompilerLedger` / `ObligationCore` as a reviewable
    bundle (ledger-from-ledger), proving the compiler's own facts are
    audit-visible through the same surface user programs use. When Phase 8.5
    completed, it must consume its typed query/fact results and show cache/query
    provenance separately from evidence. Otherwise it consumes the eager shared
    pipeline facts. Neither branch may recompute a private audit-only compiler
    model, create a private cache, or present a cache hit as validation.
21. Add the Phase 10 validation artifact: one package-scale audit bundle fixture
    with human and JSON output, semantic diff before/after a change, artifact
    viewer smoke test, oracle manifest, property-test manifest, persisted
    counterexample regression, spec-provenance facts, redaction check, replay
    command, and a README showing how a reviewer answers authority, proof,
    trust, assumption, and runtime-obligation questions without reading compiler
    internals. When Phase 8.5 exists, cold, warm, and cache-off audit/diff/why
    output must be identical after normalizing timing/cache-observability fields,
    and corrupt/stale query artifacts must recompute or fail closed; otherwise
    the same fixture runs eagerly with no private caching.

## Phase 11: Proof Status And Trust Gates

Goal: make every green proof/evidence status precise, traceable, and hard to
misread.

Done when: all existing production proof specs are directly and transitively
FnTable-complete, proof dependencies and provenance are visible, assumptions
and trust boundaries have lifecycle reports, and weaker evidence cannot appear
under a stronger badge.

1. Add transitive FnTable completeness: walk registered spec call graphs, not
   only direct call sites, and fail or flag missing callees before theorem
   authors hit confusing `none` evaluations.
2. Add proof dependency tracking: if proof/spec for `f` depends on `g`, drift in
   `g` must affect `f`'s proof/evidence status or surface an explicit
   dependency warning.
3. Add per-obligation proof/evidence status. Function-level status is only a
   summary; padding, block fold, digest serialization, final composition,
   contract clauses, runtime obligations, oracle checks, and assumptions each
   carry their own evidence class.
4. Keep oracle-tested evidence separate from Lean/spec refinement. Oracles are
   implementation sanity and regression evidence, not proof completion.
5. Add proof debugging output for failed/stale proofs: extracted spec, current
   fingerprint, registered fingerprint, expected theorem shape, missing callee
   facts, likely missing lemma class.
6. Add evidence provenance to proof/evidence facts: source file/span, compiler
   commit, theorem name, spec name, policy file, assumption file, tool version,
   and replay command where available.
7. Add tool-version drift checks: proof/evidence facts record the Lean version,
   Concrete compiler commit, ProofKit hash, extraction version, decision
   procedure version, and solver version where relevant. A toolchain bump marks
   affected evidence `needs_recheck` until replayed; old green badges are never
   silently reused across a proof-tool upgrade.
8. Add evidence monotonicity checks: a refactor cannot silently present a weaker
   claim as if it were still stronger (`proved` cannot degrade to `reported`
   while retaining the same badge/summary).
9. Add assumption lifecycle checks: every assumption has an owner, scope,
   rationale, review date, affected claims, and a diff gate when it widens.
10. Add a trust-boundary inventory report: all `trusted`, `Unsafe`, extern,
   backend, runtime, and target assumptions in one machine-readable list.

   V1 deliverable: `concrete audit --trust-boundaries --json` shows the trusted
   surface, the facts each boundary supports, what calls it reaches, and which
   claims depend on it. This is an honest report, not an auto-refactorer.

   A later "trusted-boundary shrinker" may suggest wrappers or proof targets,
   but suggestions must be advisory until a replayed proof/audit diff validates
   the smaller boundary.
11. Add spec-adequacy gates: release policy can require reviewed spec
    provenance for selected claims, forbid unreviewed specs in graduated
    flagships, and show when a theorem is `proved_by_lean` against a
    `spec_trusted` or unreviewed spec.
12. Add vacuity gates to proof status: `proved` summaries must be downgraded or
    blocked when the proof depends on an unsatisfiable precondition,
    contradictory assumptions, unreachable code path, or invariant `false`.
13. Add solver portfolio and cross-solver agreement as a strictly separate
    evidence class, never as kernel evidence. External SMT V1 may start with
    Z3 only, but the trust-gate roadmap must define how to run `z3`, `cvc5`,
    and `bitwuzla` where the fragment applies. Result classes:
    `solver_trusted` for one trusted solver, `solver_cross_checked` when two or
    more independent solvers agree on the same `unsat` result for the same
    normalized query, `solver_disagreement` when they differ, and
    `solver_unavailable` / `solver_timeout` / `solver_unknown` for non-proofs.
    Agreement must record solver names, versions, logic, query hash, timeout,
    and replay commands. A cross-checked solver result is stronger than a
    single solver but still below `proved_by_lean` and
    `proved_by_kernel_decision`. Add `examples/solver_portfolio/` with one
    QF_NIA query, one bitvector query, one unsupported-fragment case, one fake
    disagreement wrapper, and one missing-solver case. Wire
    `scripts/tests/check_solver_portfolio.sh`; the gate must prove no external
    solver result can overwrite kernel evidence and that disagreement blocks
    release claims unless explicitly assumed.
14. Add spec/proof mutation testing to prove evidence is load-bearing. Command
    surface: `concrete mutate-evidence --target <example> --json` creates
    controlled mutants: change a function body under a proof link, weaken or
    delete an `#[ensures]` clause, strengthen an impossible `#[requires]`,
    remove a loop invariant, alter a spec PExpr/table entry, change a theorem
    name, and perturb a trusted assumption. Expected outcomes must be explicit:
    stale, missing, vacuous, partial, failed proof, widened trust, or unchanged
    only when the mutation is semantically irrelevant and justified. Wire
    `scripts/tests/check_evidence_mutation.sh` over `hmac_sha256`,
    `constant_time_tag`, `proof_patterns`, and one contract-negative example.
    The gate must fail if a mutated proof/spec still reports the original green
    evidence class without an allowed explanation. This is evidence about the
    evidence: proofs must constrain the implementation, not merely decorate it.
15. Add proof-corpus migration across toolchain upgrades, the active half of the
    §7 drift story. Detection marks evidence `needs_recheck`; migration must turn
    a bumped corpus green again without hand-walking every obligation. Command
    surface: `concrete prove --recheck-corpus [--json]` re-runs every linked
    proof/evidence check under the current toolchain and triages each into
    `still_proved`, `replayed_clean`, `broke_needs_repair`, or
    `unavailable_dependency`; `concrete prove --recheck <obligation_id>` does one.
    Pin the external Lean proof-library surface the corpus depends on (Lean
    stdlib / Batteries / Mathlib lemmas and tactics actually cited) in a checked
    `proofs/lean-deps.lock` with versions, so a renamed or relocated upstream
    lemma is reported as `unavailable_dependency`, never a silent break. Wire
    `scripts/tests/check_proof_corpus_migration.sh`: it must prove the flagship
    corpus (`hmac_sha256`, `constant_time_tag`) re-greens under a simulated
    toolchain bump, that a removed/renamed pinned lemma surfaces as
    `unavailable_dependency`, and that no `needs_recheck` obligation can reach a
    green badge without an actual kernel re-check.
16. Add an axiom-inventory and clean-checkout proof replay gate. Every
    `proved_by_lean` / `proved_by_kernel_decision` fact must record the Lean
    axioms its theorem transitively depends on (the `#print axioms` walk):
    `propext`, `Classical.choice`, and `Quot.sound` are the expected baseline;
    any `sorryAx`, `ofReduceBool`/`native_decide`, or user-declared axiom must
    downgrade the evidence class or surface as an explicit trusted-boundary
    fact in the §10 inventory — never sit silently under a kernel-evidence
    badge. Pair it with clean-checkout replay: the proof corpus must re-green
    from a fresh checkout in a clean temp directory, proving no green badge
    depends on stale `.lake` build artifacts, uncommitted files, or local
    machine state. Wire `scripts/tests/check_axiom_inventory.sh` (red-team
    case: a spec theorem patched to use `sorry` or a new `axiom` must lose its
    kernel-evidence class) and
    `scripts/tests/check_clean_checkout_replay.sh` (the flagship corpus —
    `hmac_sha256`, `constant_time_tag` — must replay green from a pristine
    copy, and a deliberately corrupted source-vs-artifact mismatch must fail
    loudly, not reuse the stale artifact). Seed Phase 8.5's local cache with a
    stale proof result, wrong dependency root, truncated entry, and valid entry
    from another compiler/toolchain; clean-checkout replay must reject/recompute
    all four before any evidence becomes green.
17. [relocated from closed Phase 3 — #15/#16 tail] Convert `--report proof-status`
    and `concrete prove`'s obligation facts from consistency-gated recompute to
    literal `ObligationCore`-ledger views — the last Phase 3 surfaces that
    recompute (sound today because consistency-gated). They are the
    proof-status / trust surfaces, so they belong in this phase.
18. Add the Phase 11 validation artifact: a trust-gate pressure project that
    includes transitive proof dependencies, stale dependency propagation,
    tool-version drift, proof-corpus migration across a simulated toolchain bump,
    assumption widening, spec-adequacy policy, vacuity downgrade, solver
    portfolio / disagreement handling, evidence mutation testing, axiom
    inventory and clean-checkout replay, weaker-evidence
    monotonicity, and a release gate proving each status cannot be silently
    presented as stronger evidence. Include cache-off/on/verify parity and
    stale/corrupt-cache negatives so the trust gates cover the operational path
    users actually run.

## Phase 12: Provable And Predictable Subsets

Goal: give users a named small subset they can rely on for serious
proof/evidence work.

Done when: the subset family has public names, allowed constructs, rejected
constructs, arithmetic-site policy, runtime-error policy, and compatibility
promises.

1. Define `PredictableV1`: no allocation unless bounded, no FFI unless trusted
   and assumed, no unbounded loops/recursion, explicit failure-path policy.
2. Freeze arithmetic-site semantics for subset claims. This item is reconciled
   with Phase 6 #10 and `docs/ARITHMETIC_POLICY.md`: build/profile names are
   policy bundles, not arithmetic modes, and the same source expression must not
   mean wrap in one profile and checked in another. Ordinary `+ - *` are checked
   in every profile; intentional modular arithmetic is written as
   `wrapping_*`; intentional clamping is written as `saturating_*`. Subset
   reports must classify arithmetic sites as `checked`, `proved`, `runtime-
   checked`, `explicit-wrapping`, or `explicit-saturating`; they must never
   infer an ambient arithmetic mode from `debug`, `release`, `predictable`, or
   `proof`.
3. Carry arithmetic-site facts into diagnostics, reports, assumptions, proof
   obligations, release bundles, and backend contracts. A theorem about checked
   arithmetic is not a theorem about modular arithmetic; a function using
   `wrapping_*` is either proved against the proof model's explicit modular
   operator (today only `wrapping_add@u32 → addw 32`, as SHA-256 uses) or, for
   the not-yet-modeled forms (`wrapping_sub`/`wrapping_mul`, other widths,
   saturating), outside the default proof subset. Add
   `docs/ARITHMETIC_SITE_EVIDENCE.md`,
   `examples/arithmetic_site_evidence/{checked,wrapping,saturating,profile_invariant}/`,
   and `scripts/tests/check_arithmetic_site_evidence.sh`; the gate must prove
   profile-invariance (the same expression has the same semantics under
   different build profiles), explicit classification of every arithmetic site,
   and rejection/downgrade of proof claims that confuse checked and wrapping
   semantics.
4. Define a first runtime failure model: abort, assertion failure, OOM, stack
   overflow, `defer`/cleanup, impossible branches, and what each does to
   proof/resource claims.
5. Define source-level stack-depth versus backend/target stack claims.
6. Define source-level constant-time profile v1:
   no secret-dependent branch, no secret-dependent memory index, fixed loop
   bounds, explicit backend timing assumptions.
7. Define secret/data-sensitivity labels for future security work:
   `public`, `secret`, `timing-sensitive`.
8. Define source-level memory-safety claims precisely: what linearity, borrows,
   cleanup, trusted code, raw pointers, and FFI do and do not guarantee.
8a. Define the **Unsafe island** rule for unchecked operations. Any operation
    that bypasses a safe runtime check, borrow/linearity rule, raw-pointer
    restriction, layout proof, or FFI ownership boundary must be exposed through
    a deliberately small surface requiring `with(Unsafe)` or an explicitly
    named `trusted` boundary. Examples include future `get_unchecked`, raw
    pointer place writes, unchecked casts/conversions, inline asm, and
    unchecked FFI wrappers. There is no global unsafe mode and no silent
    trusted wrapper that hides authority: audit output must show the safe
    wrapper, the underlying trusted/Unsafe operation, and the assumption being
    accepted.

    Deliverable: `docs/UNSAFE_ISLAND.md` plus fixtures for `get_unchecked`-style
    access, raw-pointer place writes, unchecked casts, inline asm placeholder,
    and FFI wrapper assumptions. Add a red-team gate proving safe code cannot
    reach the unchecked operation without the capability or trusted boundary,
    and proving audit output shows both the safe wrapper and the underlying
    trusted/Unsafe operation.
9. Decide the proof class for references and borrows. A function using `&` or
   `&mut` must be classified as one of: value-only/borrow-free,
   proved over read-only references, proved over mutable references with
   explicit frame/modifies obligations, or enforced-only and outside
   `ProvableV1`. Do not let borrow-using code appear proof-eligible through a
   value-only ProofCore model.
10. Define the v1 threat model: adversary, trusted base, proof scope, backend
   scope, side-channel scope, dependency scope, and what remains out of model.
11. Add negative examples for every `ProvableV1` and `PredictableV1` exclusion.
12. Update `CLAIMS_TODAY.md`, README, showcase docs, and release bundles to use
    the frozen subset names consistently.
13. Close the unprofiled-float proof hole before any float proof claim:
    float-typed params/returns/locals/literals/ops are excluded from ProofCore
    extraction unless an explicit float profile is active. Audit output must
    say `float semantics: unprofiled` and `proof eligibility: excluded` rather
    than reporting a float operation as extracted through integer `PBinOp.add`.
14. Define `ProvableFloatV1` as a separate, narrow proof profile:
    IEEE-754 binary32/binary64, round-to-nearest-even, no fast-math, no
    reassociation, no implicit FMA contraction, no ambient rounding-mode
    mutation, and explicit NaN/infinity/subnormal/signed-zero policy.
15. Add ProofCore support for profiled floats only after item 13 is closed:
    `PVal.float32/64`, float `PBinOp` cases carrying width and rounding
    (`fadd`/`fsub`/`fmul`/`fdiv`/`feq`/`flt`/`fle`), interpreter agreement,
    and backend/audit checks that prove/report `fast_math: forbidden`.
16. Classify the first float semantics layer honestly. Until Concrete imports
    or proves a checked IEEE-754 semantics library, primitive float operations
    are `float_semantics_trusted`; proofs over profiled float code are
    refinements to an explicit bit-level IEEE spec under that named trusted
    primitive layer, not `proved_by_lean` from first principles.
17. Add one small `ProvableFloatV1` flagship only after the profile exists:
    a fixed-order `f32`/`f64` kernel such as clamp/normalize, tiny FIR/IIR, PID,
    or dot product. Prove exact IEEE behavior first; real-valued epsilon-bound
    refinement is a later layer.
18. Add the Phase 12 validation artifact: a profile matrix project covering
    `PredictableV1`, `ProvableV1`, unprofiled-float exclusion, profiled-float
    admission, borrow/reference proof-class decisions, constant-time source
    shape, stack/runtime-failure assumptions, and negative examples for every
    exclusion. The gate must prove reports never call excluded code proof
    eligible.
19. Define the SPARK-class contract layer in Concrete's vocabulary, not Ada's:
    frame/read/write contracts, dependency-flow contracts, ghost/spec purity,
    and proof classes for each fact. Candidate surface may include
    `#[reads(...)]`, `#[writes(...)]`, `#[modifies(...)]`, and
    `#[depends(out <- in1, in2)]`, but syntax is not frozen until a flagship or
    external workload pulls it. The design must explain how these facts differ
    from capabilities: capabilities name external authority; frame/dependency
    contracts name memory/state/data influence. Add
    `docs/SPARK_CLASS_ASSURANCE.md` updates, examples with one parser/buffer
    loop and one policy/data-flow function, and a gate proving facts are
    reported as proved/enforced/reported/assumed/trusted rather than prose-only.
20. Define a development-only expectation policy, borrowing Roc's useful
    lightweight `expect` idea but fitting Concrete's evidence model. The goal is
    a clear place for "this should hold during tests/dev" that cannot be
    mistaken for proof, runtime-safety evidence, or a release assumption.
    Candidate surface may be an attribute, a test helper, or a restricted
    `expect` form, but the semantics must be decided before syntax: it is
    allowed in tests/examples and development profiles; it produces
    `tested`/`dev_checked` evidence only; it is excluded from `ProvableV1`
    unless translated into an explicit contract/obligation; and release bundles
    must either omit it, downgrade it, or list it as non-release evidence. Add
    `docs/EXPECTATION_POLICY.md`,
    `examples/expectation_policy/{dev_expect,test_expect,release_reject,contract_replacement}/`,
    and `scripts/tests/check_expectation_policy.sh`; the gate must prove that a
    passing expectation is never reported as `proved` and that a release/high-
    integrity profile cannot silently rely on it.

## Phase 13: Runtime Safety Obligations

Goal: generate SPARK-like obligations for boring runtime failures instead of
relying only on examples and prose.

Done when: parser/security examples can show obligations for bounds, div/mod
zero, overflow profile, casts, and loop bounds with statuses
`proved`, `enforced`, `assumed`, `missing`, or `blocked`.

This phase is the **absence of runtime errors** story, stated honestly. A bounds
check, overflow trap, cast check, or loop-bound assertion may be enforced at
runtime, proved statically, assumed by policy, or still missing; those are
different evidence classes and reports must not collapse them. The SPARK/Ada
lesson to keep is the framing: runtime safety is not just "the program probably
doesn't crash"; it is a set of named obligations with source spans, replay
commands, and visible proof/enforcement status.

0. PROVEN violations are hard errors by default in safe code (established
   baseline; see CHANGELOG). The obligation engine already discharges some obligations to
   `violation` (a compile-time PROOF the access is wrong); safe build/check
   paths now reject those cases with E0900 instead of treating them like
   `unproven`. Constant OOB (`a[5]` on `[i64; 3]`) and literal div-zero
   (`10 / 0`) fail the build; `--report contracts` still renders the
   underlying `VIOLATION` for review. `trusted` / `with(Unsafe)` remains an
   explicit audit-responsibility escape hatch, and `unproven` obligations are
   NOT swept into the hard-error path. Locked by
   `scripts/tests/check_proven_violation_enforcement.sh`, which asserts both
   hard errors, the trusted exemption, and the unproven-control case.
1. Define stable obligation schema v1: id, kind, source span, function,
   expression, dependencies, evidence status, discharging theorem/check/
   assumption, and replay command.
2. Define the user-level error model: `Result`, `Option`, assertion failure,
   abort/panic, recoverable errors, test failures, and how error flow interacts
   with capabilities, proofs, runtime obligations, and audit output.
3. Generate narrowing/invalid-cast obligations.
4. Generate loop bound and variant obligations for bounded loops.
5. Define policy gates for `#[overflow_checked]`: release profiles may require
   overflow obligations for selected functions/packages, while ordinary
   examples remain quiet unless they opt in. Reports must distinguish
   `overflow_checked`, `overflow checking not requested`, and explicit wrapping
   or saturating arithmetic.
6. Generate obligations for panic/abort/assert-as-denial-of-service risks:
   unchecked indexing, unwrap-like operations, explicit abort paths, failed
   assertions, and profile-dependent panic behavior.
7. Generate byte/text/path boundary obligations: invalid UTF-8, lossy
   conversion, OS-string conversion, path normalization assumptions, and
   rejected implicit conversions.
   - 7a. Implement explicit enum discriminant values for FFI/protocol enums
     (`enum Op { Get = 0x01, Set = 0x02 }`). These are currently REJECTED at
     parse time (E0001, "explicit enum discriminant values are not supported
     yet") because the parser previously parsed and silently DISCARDED them,
     assigning positional tags regardless — a semantically dark construct that
     would corrupt any FFI/serialization enum. Implementing means: honor the
     written value in the variant's tag/repr, reject duplicate discriminant
     values (a collision is a bug, not a silent alias — Rust rejects it),
     surface the chosen tag in the layout/ABI report, and tie it to the
     `repr(C)` boundary so a C consumer and a round-trip decoder agree. Until
     then the rejection stands (regression-locked by
     `tests/programs/error_enum_explicit_discriminant.con`). Add
     `examples/enum_discriminants/{protocol_ops,duplicate_rejected}/` and a
     gate proving values are honored at the repr boundary and duplicates are
     rejected.
8. Generate stack/recursion obligations where the profile claims boundedness.
9. Report runtime-error obligations in human and JSON forms.
10. Add policy gates that can require selected runtime-error obligations to be
   proved/enforced before graduation.
11. Add a runtime-error regression corpus: invalid cast, loop-bound violation,
    lossy byte/text conversion, ignored fallible result, unwrap-like failure,
    panic/abort profile mismatch, and release-policy rejection for missing
    `#[overflow_checked]` evidence where required.
12. Add a runtime-error-obligation flagship requirement: one graduated example
    must demonstrate no OOB/div-zero/overflow under a named profile.
13. Add high-quality diagnostics for obligation failures: violated obligation,
    source expression, required evidence, current status, and next action.
14. Add obligation suppression only through explicit assumptions or policy
    waivers, never comments or hidden allowlists.
15. Prove or validate obligation-generation soundness for the first obligation
    kinds through the compiler soundness bridge.
16. Add automatic invariant inference / abstract interpretation as an
    annotation-reduction pass, not as trusted proof. V1 analysis is finite and
    auditable: interval facts, simple relational facts (`i <= n`, `i < len`,
    `0 <= i`), monotone loop counters, constant loop bounds, simple affine
    equalities/inequalities, and fixed-array length facts. It may synthesize
    candidate loop invariants and scoped facts for bounds, div/mod-zero,
    overflow, cast, and loop-variant obligations. Every inferred fact must be
    emitted in the obligation ledger with source span, analysis name, abstract
    domain, dependencies, and a replay command, then independently discharged
    by `omega`, `bv_decide`, or Lean before receiving
    `proved_by_kernel_decision` / `proved_by_lean`. Unchecked inference is not
    evidence. Add status detail `inferred_candidate` for facts proposed by the
    analysis but not yet checked. Add `examples/inferred_invariants/` with
    `array_sum_no_oob`, `copy_loop_bounds`, `ring_index_mod`, `overflow_counter`,
    and negative cases for non-affine updates, alias-sensitive updates, and
    widened bounds. Wire `scripts/tests/check_invariant_inference.sh`; the gate
    must prove inferred facts reduce required user annotations without creating
    false green obligations.
16a. Connect runtime-safety obligations to loop, frame, and dependency facts.
    Bounds/cast/overflow obligations should be dischargeable from explicit loop
    invariants, generated invariant candidates, and future `reads`/`writes`/
    `modifies` facts without duplicating the ledger. The report must show which
    invariant or frame fact discharged each obligation, and missing facts must
    produce actionable diagnostics rather than generic "unproven" output.
17. Add newtype/type invariants as scoped obligation hypotheses after they are
    checked at construction boundaries. This connects validated wrappers to the
    proof/runtime-safety pipeline: a type such as
    `#[invariant(self.0 > 0 && self.0 < 65536)] newtype Port = u16` must have
    the invariant proved/enforced at every constructor or `try_new` admission
    point, then injected as a named hypothesis for obligations in any function
    receiving a `Port`. No invariant may enter scope from a raw cast, trusted
    constructor, FFI boundary, stale proof, or unchecked assumption without a
    ledger entry saying so. Add `docs/NEWTYPE_INVARIANT_OBLIGATIONS.md`,
    `examples/newtype_invariants/{port,nonzero_len,bounded_index,ffi_trusted}/`,
    and `scripts/tests/check_newtype_invariant_obligations.sh`; the gate must
    prove constructor checks create reusable hypotheses, invalid constructors
    are rejected or return `Result`, raw/trusted paths do not silently inject
    facts, and obligation reports name the type invariant source.
18. Add the Phase 13 validation artifact: a runtime-safety corpus covering
    bounds, div/mod-zero, overflow, casts, panic/abort/assert, byte/text/path
    boundaries, stack/recursion, inferred invariant candidates, newtype
    invariant hypotheses, arithmetic-site evidence mismatches, and obligation
    suppression. Each case must show one of `proved`, `enforced`, `assumed`,
    `missing`, or `blocked`, include a negative variant, and run through policy
    gates plus human/JSON reports.

## Phase 14: Compiler Soundness Bridge

Goal: move the flagship-used `Core -> ProofCore` rules from "extracts to the
expected ProofCore shape" toward source-semantics agreement and checked
trust-gate correctness.

Prior-art peer group for this phase: CompCert for proved compiler passes,
CakeML for verified bootstrap discipline, Cogent for linear systems code with
refinement proof artifacts, F*/Low* for obligation-to-SMT-to-extraction
workflow, and Alive2 for SMT-checked LLVM IR equivalence/optimization
validation. Keep these as reference points for the shape of evidence, not as
marketing claims that Concrete already matches them.

Done when: each flagship-used ProofCore construct is classified as
shape-preserved, eval/source-semantics-preserved, or still trusted; proof-report
facts agree with compiler state; a small independent checker re-derives the
named `CoreCertificateV1` predicate from canonical artifacts rather than
trusting producer summaries; and the remaining trusted base is machine-readable.

**Endgame (north star): the compiler becomes a mechanically-verified artifact,
out of the trusted base.** The per-rule discharge below (R-01..R-21), the
`CoreCertificateV1` independent checker, and Phase 15 #18 translation validation
are not the finish line — they are the incremental ladder toward proving each
pass semantics-preserving in Lean against the interpreter reference, so the
compiler leaves the TCB entirely. Concrete is uniquely positioned for this: it
is Lean-hosted, so the compiler's correctness proof and users' program proofs
share ONE kernel — collapsing "trust the prover" and "trust the compiler" into a
single trusted base no other verified-compiler effort (CompCert, CakeML) has.
The 6B fact-centralized IR and interpreter-as-oracle are the runway: each pass
is a clean function over committed facts with a reference semantics to prove
against, so passes should be built PROVABLE-FIRST, not merely consistent. This
names the endpoint so intermediate design choices bend toward it; it does not
schedule a multi-year proof now, and it must not starve the near-term or the
external trial. The complementary rewrite-passes-in-Concrete route is Phase 20
#15; the shorter path is proving the existing Lean-hosted passes here.

1. Add a compiler-soundness rule-status dashboard over R-01..R-21:
   `shape-preserved`, `eval-preserved`, `source-preserved`, `trusted`, or
   `blocked`, with theorem names and source links for each status.
2. Build source semantics for the provable subset as needed by rule discharge,
   not as a speculative full-language semantics.
3. Upgrade extraction-only rules to three-view preservation where practical:
   source Core evaluation, extracted PExpr evaluation, and extraction theorem
   agree.
4. Prioritize the hard eval/source rules in dependency order: direct calls,
   structs/fields, enums/match, arrays, casts, arraySet, flat bounded `while`,
   and `while_step`.
5. Prove selected proof-report facts agree with compiler state: `proved`,
   `stale`, `blocked`, `missing`, `ineligible`, `trusted`.
6. Prove or mechanically validate trust-gate correctness: body fingerprint
   determinism, spec-drift completeness, proof attachment lookup, FnTable
   completeness, eligibility classification.
7. Record a machine-readable trusted computing base for proof/evidence claims:
   Lean kernel, compiler modules, backend/toolchain, runtime/OS/hardware,
   trusted/extern code, and the external Lean proof-library surface the corpus
   cites (Lean stdlib / Batteries / Mathlib lemmas and tactics). Kernel-checked
   library lemmas do not widen the trusted base, but they are a pinned
   version/availability dependency (see Phase 11 proof-corpus migration); the TCB
   record must name them so a proof's replay surface stays reproducible.
8. Prove selected checker/report agreement for authority and purity facts used
   by proof eligibility, so a function cannot be called proof-eligible while
   secretly requiring capabilities.
9. Decide whether deeper source-semantics proofs require a normalized Core
   layer. Add the layer only if the direct rule proofs show repeated semantic
   duplication across at least two forcing examples.
10. Automate dependency-ordered spec/table generation. Function specs and
    PExpr bodies should be collected/generated in dependency order together
    with FnTable completeness and call dependencies, so proof authors do not
    hand-relocate definitions above tables like `shaFns`.
11. Prove or mechanically validate spec/ghost totality reporting: every
    contract-referenced `spec fn` or ghost computation is either backed by
    Lean termination, accepted by a Concrete totality check, or rejected with a
    `totality_obligation_missing` status. A contract may not depend on a
    partial or non-terminating spec expression silently.
12. Define proof preservation across monomorphization and
    capability-polymorphic instantiation. The compiler must report whether a
    theorem proves a specific generated instance (`proved_for_instance`) or a
    generic body (`proved_generic`), and it must prevent one instance proof from
    being presented as proof for every future instantiation.
13. [relocated from closed Phase 4 — #44f tail / #44g] Compiler-correctness
    hardening: (a) the random differential
    generator exists (`scripts/tests/fuzz_differential.py`, Makefile
    `test-fuzz-differential`), covers the through-reference / void-slot shapes,
    value-bearing if/match nesting, loops, enum payloads, and (2026-07-01) the
    full integer width lattice with explicit casts; its taxonomy treats any
    E07xx/panic on a generated well-typed program as a compiler bug
    (LANGUAGE_INVARIANTS #19). Remaining (a) tail: string/linear-value shapes,
    seed rotation in CI (nightly campaign, not just two fixed seeds), and
    auto-minimization of failures. The per-claim differential companion landed
    2026-07-02: `check_cast_matrix.sh` pins every (source width x edge value) ->
    every-width cast against the interpreter, mechanically covering the
    ARITHMETIC_POLICY truncation/reinterpretation claims. And (b) the defense-in-depth
    ref-return lowering fix — a reference-typed return materialized from a ref
    identifier / `&place` emits a spurious extra load. (b) is unreachable from
    source under Option A: reference returns are rejected at the type level for
    *every* function, safe and trusted (H1; Check + `verifyNoReturnedRefs`/E0236),
    so it is dead-path internal-lowering hardening; fixtures must distinguish
    rejected reference returns (safe and trusted alike) from allowed trusted
    raw-pointer (`*const`/`*mut`) returns, which are not reference types.
13a. Add a semantic-darkness audit and red-team gate. The goal is to catch the
    checked-arithmetic class of bug before it repeats: a construct looks
    ordinary in source, but its real behavior depends on width, profile, target,
    allocation, authority, runtime checks, or an outdated proof/interpreter
    model. Add `docs/SEMANTIC_DARKNESS_AUDIT.md` and
    `scripts/tests/check_semantic_darkness.sh`; wire the gate into CI or the
    Phase 14 validation artifact. The audit must cover:
    - **source-semantics matrix** for each core operation: parser/AST,
      checker rule, Core/Elab representation, interpreter behavior, SSA
      cleanup/optimization, LLVM lowering, ProofCore extraction, reports, and
      stdlib examples must agree on the same meaning;
    - **primitive boundary tests** over arithmetic, casts, shifts, div/mod,
      literals, references, arrays, byte/text/path boundaries, runtime aborts,
      target assumptions, and trusted/Unsafe escape hatches;
    - **stdlib authority/allocation audit**: every stdlib API that allocates,
      touches host state, uses trusted/Unsafe internals, depends on target/OS
      behavior, or relies on sentinel/wrapping/truncating behavior must expose
      that fact in source or report output;
    - **proof/runtime consistency fixtures**: interpreter-vs-compiled,
      optimized-vs-unoptimized where available, report-vs-runtime, and
      ProofCore-vs-runtime behavior must agree or be explicitly classified as
      trusted/unsupported;
    - **old-assumption grep**: scan code, tests, docs, and snapshots for stale
      terms such as `wrap`, `wrapping`, `overflow`, `truncate`, `expected
      diverge`, `unbounded Int`, `addw`, `mod 2`, `unsafe`, `trusted`,
      `temporary`, and `current lowering`; every hit must be either current,
      linked to a roadmap item, or removed.
    The first version should seed fixtures from the known high-risk families:
    checked arithmetic / explicit wrapping, float→int cast overflow (H2),
    lossy casts, div/mod-zero, shifts, returned-reference rejection, ByteView
    wrong-buffer guards, HMAC/SHA-256 modular arithmetic, and capability-bearing
    stdlib calls.
    Prior art and comparison targets: CompCert/CakeML/Cogent/F*/Low* for
    source-semantics and proof/evidence pipelines, and Alive2 for backend IR
    equivalence checking. See
    `research/compiler/pipeline-lessons-2026-07.md`.
13b. **Preserve the one source of typing truth through proofs.** Every 2026-07 front-end bug was
    two passes holding different opinions about the same program: Check typed
    literals from the hint while Elab typed them from the sibling operand;
    `typesCompatible` was lenient where SSA-verify was strict (the E0715 class);
    Elab carried a private int-vs-fixed-width re-elaboration Check knew nothing
    about; std skipped Check entirely while Elab/CoreCheck ran (H12). Shared
    predicates (`binOpOperandsAgree`) and LANGUAGE_INVARIANTS #19 gates DETECT
    drift; the architecture still INVITES it — three semi-independent semantic
    judgments over one program. Endgame: one shared source type judgment feeds
    both Check's type-dependent checks and Elab's existing typed Core
    (`CExpr.ty`) stamping, deleting Elab's private re-inference and CoreCheck's
    overlapping rules down to genuine Core-shape validation.

    The completed Phase 6B / 6.5 `TypeJudgment` work is where the architecture
    lands, not this phase: Core `CExpr`
    is already the typed IR, and the type-axis work is to centralize the source
    type judgment so Check and Elab stamp/read the same answer. The Phase 8.5
    `CompilerDB` owns relational, provenance, evidence, dependency, and query
    facts pulled by real consumers. This Phase 14 item proves and
    preserves that architecture. The work here is to delete remaining
    overlapping typing judgments, prove passes preserve typed Core meaning, and
    ensure proof extraction / reports / backend validation consume typed Core
    plus relational/evidence facts rather than reconstructing source-level
    judgments.

    Source identity remains useful for DB-owned relational facts, but it is not
    the type carrier and should not be built just to fix source typing. Typed
    Core is the type carrier. For migrated type families, Check and Elab must
    call the shared judgment. For migrated relational/evidence families, DB
    coverage checks apply to the Phase 8.5 `CompilerDB`. Missing DB facts or
    conflicting DB facts are compiler errors. Unmigrated families may still be
    absent only while their migration flag says so.

    The preservation split is explicit: typed Core is primary for node-local
    type facts; the Phase 8.5 `CompilerDB` is primary for relational facts such as E0293
    container-not-in-context, borrow conflicts, call-site/parameter pass
    agreement, capability/proof dependencies, provenance, evidence, and
    invalidation. A proof obligation may reference both, but it must not create a
    third spelling of the same fact. This is the proof-facing form of the Phase
    6.5 placement rule.

    The boundary chain should be unrepresentable-by-construction:
    `ResolvedProgram -> DesugaredProgram -> Check -> Elab -> CoreProgram ->
    CoreCheck -> ValidatedCore -> Mono ->
    ValidatedMonoCore -> Lower -> SSA -> SSAVerify -> ValidatedSSA`. If a
    downstream stage wants a source type, local value-flow mode, capability
    requirement, pass-agreement decision, or resolved callee, it consumes the
    owning representation: `CExpr.ty` / typed Core for type facts,
    Phase 8.5 `CompilerDB` edges/evidence for relational facts. It may validate
    shape; it may not create a second source-level judgment. Done when Elab's
    re-inference code is deleted, not merely bypassed, and regressions prove the
    literal/defaulting, mixed-width, std-bypass, conditional-Copy, and
    capability/type-drift classes cannot be expressed through the typed
    boundary. Later identity/DB refinement remains available for edge facts, but it
    is not on the critical path for type drift.
13c. Add an independent Core-certificate checker for a narrow, explicit
    validated-Core predicate.

    Define a versioned canonical `CoreCertificateV1` and a small separate
    `concrete-cert` library/executable target. Enforce a dependency firewall: it
    may import the canonical artifact/schema decoder, hash/id datatypes,
    **independent declarative specifications and shared data types**, and checker
    theorems; it may
    not import Parser, Check, Elab, Mono, Lower, Report, CLI, project loader, the
    Phase 8.5 scheduler/cache, producer executable judgments such as
    `TypeJudgment`/Layout/fingerprint helpers, or producer verifier
    implementations. Publish its
    source/binary hash, import inventory, `partial`/unsafe/axiom inventory,
    rule-set version, and size so "small checker" remains measurable.

    V1 independently derives its verdict from the canonical Core artifact and
    small specifications. It checks at least: canonical/schema validity;
    artifact, subject, predecessor, and dependency-root binding; unique stable
    identities and referenced-node/provenance existence; constructor-local type
    consistency; direct-call arity and argument/result agreement; absence of
    unresolved placeholders/type variables where the Core boundary forbids
    them; direct-call capability containment; proof-target/body-fingerprint and
    obligation dependency binding; and the Phase 6B validation-chain shape.
    Proof-target/fingerprint/obligation/provenance checks establish identity and
    binding only; they do not establish extraction correctness, theorem/spec
    adequacy, or source correspondence. It
    must not merely compare a producer-emitted fact table with a second
    producer-emitted summary.

    Ownership/borrow/value-flow claims enter V1 only where the artifact carries
    enough independently checkable decisions and path edges to validate them.
    Checking that an ownership field exists is not ownership-checker soundness.
    Extend the predicate one rule family at a time with a mutation fixture. Give
    every predicate an explicit checker-proof status:
    `checker_trusted` for executable checking without a soundness theorem,
    `checker_soundness_kernel_proved` only when Lean proves a theorem such as
    `checkCoreCertificate a c = true -> CoreCertificateV1.Valid a c`, and
    `kernel_replayed` only for the artifact-specific theorem. An unproved checker
    may not appear under the same badge as a proved checker.

    Emit `certificate_structurally_checked`, `certificate_mismatch`, or
    `certificate_unsupported`; never `certified` or `proved_by_lean` merely
    because the structural checker passed. An artifact-specific preservation
    theorem actually replayed by Lean may separately emit `kernel_replayed`.
    State the remaining TCB precisely: V1 does **not** prove parsing,
    resolution, Check/source-to-Core correspondence, ownership/capability
    soundness beyond named local rules, spec adequacy, ProofCore extraction
    beyond discharged Phase 14 rules, native-code behavior, or dependency
    completeness beyond the conservative Phase 8.5 envelope. Its own TCB still
    includes adequacy of the named predicate, canonical decoder, hash algorithm
    and collision assumption, checker implementation unless covered by the
    stated soundness theorem, Lean compiler/runtime and host execution when
    running a compiled checker, and every imported trusted/axiomatic primitive.
    `kernel_replayed` is reserved for an artifact-specific theorem actually
    checked by the kernel; a theorem about the checker does not make execution
    of the compiled checker kernel-only.

    Bind the checker to the Phase 8.5 query DAG: an **independent consumer** may
    distrust cached canonical bytes and the producing compiler for the named V1
    predicate only after that consumer reruns the checker. A receipt rendered by
    the producer remains producer output until external replay. A verification
    receipt names artifact digest,
    dependency root, checker/rule-set version, checked and unsupported
    predicates, theorem/replay identity where any, and exact trust boundary.

    Gate altered expression types, wrong call signatures/results, missing
    capabilities, unresolved type variables, duplicate ids, missing provenance,
    stale dependency roots, artifact/certificate swapping, fabricated proof
    links, mismatched obligation subjects, malformed/noncanonical encodings,
    unsupported constructs mislabeled checked, and a producer record that
    agrees with itself about a false artifact fact. Break a producer judgment/
    layout/fingerprint helper and prove the checker still rejects the bad
    artifact without importing that helper. Wire
    `scripts/tests/check_core_certificates.sh` and a checker import-firewall gate.
14. Reduce ProofCore partial-def opacity only where proof preservation needs it.
    (Rehomed from Phase 6B: its trigger is here, not there.) ProofCore still
    contains many `partial def` walkers/wrappers, which are opaque to the kernel.
    Do not chase a full rewrite. Add non-partial wrappers or structural recursion
    ONLY for the constructs pulled by the R-rule preservation proofs above, so a
    lifted rule can be reasoned about structurally. Gate each lifted rule with one
    theorem that would fail if the wrapper still delegated to an opaque
    partial-def shape.

15. Add the Phase 14 validation artifact: a compiler-soundness dashboard with
    one witness program per shipped ProofCore construct, one status per
    R-rule, replay commands for proved/mechanically-validated facts, and
    regressions proving report facts (`proved`, `stale`, `blocked`, `missing`,
    `ineligible`, `trusted`) agree with compiler state. Include the
    `CoreCertificateV1` predicate/rule-set version, checker binary/source hash,
    soundness theorem names, independent receipt per artifact, mutation corpus,
    cache-off/on receipt parity, and a machine-readable list of every boundary
    V1 still leaves producer/compiler-trusted.

## Phase 15: Backend, Target, And Stdlib Contracts

Goal: make backend/toolchain/stdlib assumptions explicit, and state exactly
where source-level proof stops.

Done when: SSA, target/toolchain, optimization, ABI/layout, stdlib evidence,
and Phase 8.5 incremental build contracts are explicit enough for release
evidence; the supported SSA-to-BackendIR translation slice is independently
replayable; and every boundary after that slice remains explicitly trusted.

1. Stabilize SSA as the only backend contract.
2. Document target/toolchain model: triple, data layout, linker, runtime/startup,
   libc expectation, clang/llc boundary, sanitizer/coverage hooks.
3. Define optimization policy: allowed optimizations, evidence preservation,
   debug/release behavior, report/codegen validation. Follow the QBE-style
   "small auditable backend contract first" principle: prefer a stable,
   inspectable subset with explicit assumptions over early attempts to match
   LLVM-level optimization coverage.
4. Add native compiled-program debugging support: DWARF/source-map emission,
   source-mapped backtraces for runtime failures, debug-vs-release behavior,
   optimized-code caveats, and diagnostics that distinguish source-level
   runtime checks from trusted backend/OS crashes. Runtime traps inserted by
   Concrete (bounds, arithmetic, cast/profile checks, failed runtime
   obligations) should carry enough source span and check-class information
   that a real application can debug them without reverse-engineering the abort
   site. This is not proof evidence; it is usability for enforced/runtime-
   checked facts.
5. Extend Phase 8.5 clean-build versus incremental-build equivalence across
   backend variants: facts, obligations, diagnostics, reports, BackendIR,
   codegen units/objects, link manifests, and native behavior must agree under
   target, ABI/layout, optimization, sanitizer, debug, and toolchain changes.
6. Add ABI/layout round-trip checks: C headers/stubs, offsets, size, alignment,
   calling conventions.
6a. Design and, if pulled by a workload, implement first-class alignment facts.
   Start from `research/language/ALIGNMENT_FACTS.md`: compare Zig, Rust, Odin, C, C++, and
   SPARK/Ada; distinguish type/layout alignment from object/place alignment and
   pointer/reference/slice alignment facts; decide whether Concrete uses
   declaration attributes, fact/contract syntax, type-like pointer qualifiers, or
   a staged mix. No implementation should start until a forcing workload exists
   (SIMD/autovectorized slices, C ABI over-aligned buffers, freestanding DMA/MMIO,
   allocator contracts, or an optimization/audit gap). If implemented, gates must
   prove stronger alignment satisfies weaker requirements, offset/field/slice
   projections conservatively weaken facts, runtime checked refinement records the
   fact, unchecked assumptions are `unsafe`/trusted and reported, and backend IR
   alignment metadata is emitted only from Concrete facts.
7. Add C/ABI glue validation: generated headers, imported declarations, host
   stubs, symbol names, calling conventions, ownership transfer, capability
   labels, and trust assumptions must round-trip through at least one C
   harness and one Concrete caller.
7a. Add outbound FFI / embeddability as a first-class backend/ABI contract.
    Concrete's most likely early adoption path is not replacing a whole
    process; it is exporting a small verified parser, protocol checker,
    constant-time helper, or evidence-bearing subsystem into an existing
    C/C++/Rust/Zig/Go host. Today the FFI story is mostly inbound
    (Concrete calling libc or trusted host functions). The roadmap must also
    cover Concrete code callable from outside.

    V1 deliverables:
    - `#[export]` or equivalent stable export annotation with explicit symbol
      naming / no-mangle policy;
    - `staticlib` and/or `sharedlib` output mode, with the supported target
      triples named;
    - generated C header for the exported ABI subset;
    - exported-function ABI classification: scalar, pointer, small/large
      aggregate, enum/result, string/bytes/path, ownership transfer, and
      unsupported forms;
    - generated boundary docs that expose capabilities, allocation authority,
      failure mode, trusted/Unsafe dependencies, and evidence class for each
      export;
    - evidence bundle next to the library artifact, so a host build can audit
      what the embedded Concrete component claims.

    Hard rule: exported functions must not hide Concrete's thesis at the C
    boundary. If an export allocates, needs a capability, can fail, depends on a
    trusted wrapper, or has only tested/runtime-checked evidence, the generated
    header/docs/report must say so. Gate: one C harness calls a pure exported
    parser/checker, one harness calls an exported fallible API, and one negative
    fixture proves an unsupported export shape fails with a diagnostic rather
    than silently choosing an ABI.
8. Add a C ABI classification suite, inspired by QBE's explicit ABI contract:
   scalar parameters and returns, small and large structs, arrays, nested
   aggregates, alignment/padding, by-value versus by-reference lowering,
   varargs policy, calling convention labels, symbol names, and ownership/
   capability/trust annotations. Wire `scripts/tests/check_c_abi_matrix.sh`
   with both generated C callers and Concrete callers.
9. Add `BackendIR` as a structured backend contract between `ValidatedSSA` and
   LLVM/native emission. This is useful even if Concrete never ships its own
   native backend: it gives the compiler a typed, inspectable place to preserve
   runtime checks, trap/source-span facts, layout/ABI decisions, target
   constants, trusted/runtime helper calls, and capability/trust labels before
   they disappear into LLVM text or target code. The intended ladder is
   `ValidatedSSA -> BackendIR -> ValidatedBackendIR -> EmitLLVM` first; later
   emitters (native, C, WASM, QBE-style) may consume the same
   `ValidatedBackendIR` only after the contract is stable.

   V1 should cover a narrow subset: integer arithmetic, bools, branches,
   direct calls, returns, checked runtime traps, small loads/stores, source-map
   annotations, and helper calls. Add `concrete inspect --backend-ir`,
   `--emit-backend-ir`, `concrete verify-ir --pass backend-ir`, golden
   backend-IR fixtures, and round-trip checks that source maps, target
   constants, runtime checks, and capability/trust labels survive lowering.
   Gate old path vs BackendIR-mediated LLVM where both exist:
   `interp == old compiled == BackendIR->LLVM compiled`, then retire the old
   direct SSA->LLVM path once parity is established.
10. Add sanitizer-backed generated-code validation for trusted/FFI/layout/
    pointer-heavy examples, plus the stdlib sanitizer/runtime hooks from Phase
    11. Sanitizer findings are `runtime_checked` / `tested` evidence, not proof.
11. Add backend/codegen differential validation where executable oracles exist.
12. Add compiler self-leak/resource soak harness for long-running workflows.
13. Harden stdlib stability and evidence policy from Phase 7: which stdlib functions are
   trusted, proved, enforced, allocation-free, capability-free, or assumption
   carriers.
14. Define stdlib contracts for allocators, I/O handles, directory/file/path
    handles, byte/text/path conversion APIs, and fallible return discipline.
    Each public stdlib function must state allocation behavior, OS authority,
    failure mode, trusted platform assumptions, and evidence class. This item
    also owns the release-facing unsafe/trusted boundary UX: a safe wrapper may
    narrow authority, but reports must still show the underlying trusted,
    extern, raw-pointer, or `with(Unsafe)` operation and the assumption it
    depends on.
15. Add stdlib evidence gates so core helpers cannot silently widen authority,
    allocation, proof assumptions, or runtime-error obligations.
16. Evaluate a normalized mid-level IR only when traceability/backend-contract
    reports expose a concrete gap.
17. Keep QBE/WASM/second backend deferred until evidence attachment,
    optimization policy, backend IR verification, and backend trust boundaries
    are trustworthy. A QBE-style backend experiment is allowed only as a
    measured research branch: it must consume the same backend IR contract,
    emit the same source maps and target assumptions, pass the C ABI matrix for
    its supported subset, and report exactly which claims become backend/
    target-trusted.
18. Add translation validation for codegen as the path out of a fully trusted
    backend. V1 should validate a narrow `ValidatedSSA -> BackendIR ->
    ValidatedBackendIR` subset per compile:
    integer arithmetic, fixed arrays, structs, direct calls, branches, bounded
    loops, runtime checks, capability calls, and source-map annotations. The
    validator compares checked Core / typed IR / SSA facts against BackendIR
    facts and reports one of: `translation_compiler_validated_v1`, `translation_trusted`,
    `translation_blocked`, or `translation_mismatch`. This is not a promise to
    prove the whole LLVM/native stack; it is a per-artifact check that the
    Concrete lowering into the backend contract preserves the facts Concrete
    claims. Add `examples/translation_validation/` with straight-line,
    branch/loop, struct/array, runtime-check, and deliberate mismatch fixtures.
    This is the roadmap's early backend-soundness probe: start with the small
    subset as soon as SSA/backend facts are stable enough, rather than waiting
    for a full native-backend story before learning whether Concrete's evidence
    can cross the backend boundary.
    Prior art: CompCert for proved compilation/pass-correctness discipline and
    Alive2 for SMT-checked LLVM IR equivalence/optimization validation. The
    concrete threat model here is the class differential fuzzing has already
    exposed: source/Core facts can be true while a backend lowering silently
    emits a different native artifact unless that boundary has its own check.
    Wire `scripts/tests/check_translation_validation.sh`; the gate must prove a
    Lean-proved/Core-level claim cannot be presented as native-code evidence
    unless the backend artifact is either translation-validated or explicitly
    backend-trusted in the audit bundle.
18a. Make the Phase 15 translation-validation slice independently replayable.
    Extend the Phase 14 `concrete-cert` target with a versioned canonical
    `BackendTranslationCertificateV1` over hash-bound
    `ValidatedSSA -> BackendIR -> ValidatedBackendIR` artifacts. The checker
    validates the explicit input/output relation; it does not trust
    producer-emitted `preserved` flags or compare two summaries produced by the
    same lowering pass. Its operator, trap, intrinsic/helper, ABI-layout, and
    target specifications must be independent declarative definitions; it may
    not reuse Lower/Emit/Layout decision helpers whose common-mode bug it is
    intended to catch.

    V1 covers integer width/signedness and operator selection, bools, direct
    calls and signatures, returns, branches, fixed-layout structs/arrays,
    required arithmetic/bounds/runtime traps, layout/target constants,
    source-map identities, helper/intrinsic calls, and capability/trust labels
    for the subset admitted by item #18. V1 admission is whole-function: if an
    unsupported operation can influence control flow, memory, calls/arguments,
    or the return value, that entire function is `translation_trusted` or
    `translation_blocked`. Region-level validation is forbidden until explicit
    compositional boundaries and a composition theorem exist. Track call-closure
    separately: a caller may satisfy the local V1 lowering relation while a
    reachable callee remains backend-trusted, so that is not end-to-end validated
    function behavior.

    Bind Phase 8.5 codegen-unit manifests and object/link identities into the
    dependency root with explicit edge status. The independent V1 **relation**
    check stops at `ValidatedBackendIR`: `BackendIR -> object` and
    `object -> binary` are `identity_bound_only` / `backend_trusted`, not
    semantically checked. LLVM optimization, LLVM text-to-object
    translation, object writer, linker, ABI/runtime behavior, and native
    execution remain trusted/tested unless a later validator explicitly covers
    them. Object and binary hashes prove identity, not semantics.

    Mutate signed versus unsigned division, integer width, required overflow or
    bounds trap, branch target, field offset/layout, direct-call target/signature,
    capability/trust label, source-map id, target/profile, codegen-unit owner,
    object/link manifest, and certificate/build binding. Include an unsupported
    instruction mislabeled as validated. Independent replay must reject each
    checked-relation mutation without calling the producer backend. Also combine
    BackendIR A with object B and consistently rehash a fresh internal graph: in
    the absence of an externally expected root the checker must report an
    unvalidated backend edge, not claim semantic mismatch detection. Wire
    `scripts/tests/check_backend_translation_certificates.sh`.
19. Add the Phase 15 validation artifact: a backend/std-lib contract project
    with ABI/layout C round trips, C/ABI glue generation/import checks,
    the C ABI classification matrix, alignment-fact fixtures if Phase 15 #6a is
    pulled into implementation, backend-IR emission/verifier checks,
    producer translation-validation checks, independent
    `BackendTranslationCertificateV1` replay and mutations, sanitizer runs,
    compiled-oracle differential
    tests, native debug/source-map smoke tests, clean-vs-incremental fact
    equivalence, cached-object substitution negatives, and stdlib
    authority/allocation/evidence gates. The artifact must show the exact point
    at which independent checking stops and backend/toolchain trust begins.
20. Pin the supported LLVM/clang version and gate version drift. The backend today
    targets whatever `clang` defaults to on the host (`docs/EXECUTION_MODEL.md`), yet
    the TCB already conditions backend correctness on "LLVM version" — so a floating
    toolchain leaves the trusted boundary a moving, un-audited target and makes native
    output non-reproducible across hosts (`docs/DETERMINISM.md` already disclaims
    binary reproducibility for exactly this reason). Declare one supported LLVM
    version (or a narrow tested range) as part of the backend contract, and make an
    upgrade a deliberate, gated event: on a version bump the wrong-code corpus, the
    backend-contract gate, and IR golden tests must re-pass before the new version is
    declared supported. This is the cheap control that turns the moving boundary into
    a fixed one; the structured `BackendIR` contract (#18) and a future hand-written
    LLVM bitcode emitter (the Zig/Roc decoupling technique) are the heavier long-term
    options, pulled only if version pinning proves insufficient. Wire the check into
    `scripts/tests/check_backend_contracts.sh`: the compiler refuses or warns on an
    unsupported LLVM version, and the drift corpus re-runs on a bump.
    - 20a. LLVM bitcode emitter — the heavy decoupling rung (pull-gated, deliberately
      NOT its own phase). *What it is:* emit LLVM **bitcode** (LLVM IR's stable
      serialized binary format) from a hand-written serializer that never links or
      calls the churning LLVM C++ API. Zig's self-hosted compiler is the reference
      implementation — it writes bitcode in Zig and hands it to whatever LLVM is
      present, making it immune to the C++-API breakage that costs most frontends an
      upgrade tax; Roc reused Zig's bitcode writer wholesale in its 2026 Rust→Zig
      rewrite. Concrete today emits *textual* LLVM IR to `clang`
      (`Concrete/Backend/EmitLLVM.lean`), which already avoids C++-API linkage but
      stays coupled to textual-IR *syntax* drift across LLVM versions (the
      opaque-pointer migration was one such event). A bitcode writer would remove
      that coupling and enable `clang`-free, bit-identical cross-host emission.
      *Why an item and not a phase, and why deferred:* (1) it is a sub-component of the
      backend contract that gates nothing downstream; (2) it is *less* proof-relevant
      than translation validation (#18) — it buys decoupling/reproducibility, not
      correctness — so elevating it above #18 would invert priorities for a
      proof-carrying compiler; (3) the cheap version pin (#20) already fixes the moving
      trusted boundary, and LLVM churn has not been shown intolerable (one migration
      eaten so far), so a phase-sized commitment now would be speculative scheduling.
      *Pull-trigger* — promote to scheduled work when EITHER a freestanding / no-`clang`
      target is pursued (Phase 16), where self-contained emission stops being optional,
      OR LLVM version churn recurs intolerably despite the #20 pin. Same ladder as
      `BackendIR` (#18): `ValidatedSSA → BackendIR → ValidatedBackendIR → EmitLLVM`
      today; bitcode / own-backend later. Prior art: Zig (hand-written bitcode writer,
      self-hosted backend), Roc (reused it in the Zig rewrite).
21. Compiler-writing / verified-self-host language prerequisites (pull-gated
    research). Features a future proof-preserving self-host (CakeML-style, Phase 14)
    or a compiler-in-Concrete workload would need, surfaced by what makes ML/OCaml and
    Zig good compiler hosts. None is scheduled; each is pulled by a real workload and
    must preserve the linear / no-GC / visible-layout thesis — the OCaml GC path is
    deliberately not taken; the non-GC arena (#1g) + typed-index (#1b) + layout path
    is.
    - 21a. Guaranteed tail-call optimization: a tail call lowers to a loop, so
      recursive tree-walks (the dominant compiler shape) run in constant stack. Today
      recursion is a stack-overflow risk the predictable profile avoids; guaranteed
      TCO makes tail recursion predictable-profile-safe rather than banned. Pure
      codegen guarantee — no ownership/capability/proof interaction. Gate: a deep
      tail-form fixture runs in O(1) stack; non-tail recursion is diagnosed, not
      silently stack-unsafe.
    - 21b. Bit-level data layout: arbitrary-width integers (`uN`/`iN`) and packed
      structs/enums with defined bit layout — compact IR nodes, discriminant/flag
      packing. Fits the visible-systems-layout thesis directly. Gate: a packed
      struct's size and field offsets equal the declared bit layout; an over-wide
      store into a `uN` field is rejected or wraps per the declared width, not
      silently. (SOA containers and `std.bigint` are stdlib-level, pulled the same
      way; nested patterns is tracked in `docs/NESTED_PATTERNS.md`.)
22. Keep inbound FFI, outbound FFI, and unsafe-profile work connected but
    distinct. Inbound FFI says how Concrete safely calls host code; outbound FFI
    says how host code safely calls Concrete exports; Phase 15.5 says how the
    trusted/raw-pointer implementation islands remain auditable. Release-facing
    docs must not collapse these into a vague "FFI supported" claim. Each side
    needs its own examples, ABI restrictions, ownership-transfer rules,
    capability/evidence report, and red-team negatives.

## Phase 15.5: Unsafe Concrete And Trusted Systems Profile

Goal: make deliberately unsafe Concrete code boring to write, easy to audit, and
hard to misuse. This phase borrows the useful Zig lesson: low-level programs
need excellent tools for pointers, slices, allocators, debug allocators, and
runtime instrumentation; the answer is not a global unsafe mode or Rust-style
lifetime algebra, but small explicit unsafe islands with visible authority and
replayable checks.

Done when: every raw-pointer, unchecked, trusted, allocator, and FFI escape hatch
has a named safe wrapper pattern, audit/report output shows the underlying
assumption, debug/profile instrumentation can catch representative misuse, and a
validation project proves safe code cannot reach these operations accidentally.

This phase does **not** weaken Concrete's philosophy. Safe Concrete remains
linear, capability-visible, second-class-reference, and no-hidden-cleanup.
Unsafe Concrete is a deliberately marked profile for the code that must cross
those boundaries: runtimes, allocators, FFI, VM/interpreter internals,
freestanding/MMIO, sanitizer probes, and backend/ABI glue. Its evidence class is
`trusted`, `runtime_checked`, `tested_by_oracle`, or `assumed` unless a later
proof actually discharges the exact claim.

Reference languages and reading:
- Zig for pointer/slice taxonomy, allocator-passing, arena/fixed-buffer/debug
  allocators, and the "prefer slices over raw many-item pointers when length is
  known" rule:
  `https://ziglang.org/documentation/master/` and
  `https://zig.guide/standard-library/allocators/`.
- Hare for checked slices by default, unbounded arrays as an explicit escape
  hatch, small Unix-shaped wrappers, and honest docs about what remains manual:
  `https://harelang.org/blog/2022-06-21-safety-features/` and
  `https://harelang.org/blog/2021-02-09-hare-advances-on-c/`.
- Odin for tracking allocators, sanitizer/runtime-instrumentation pragmatism,
  foreign-system ergonomics, layout/endian control, and fine-grained
  bounds-check controls:
  `https://odin-lang.org/docs/overview/` and
  `https://odin-lang.org/docs/faq/`.
- Rust/Rustonomicon as cautionary input: unsafe reference/aliasing validity is
  subtle, pervasive unsafe can become harder to audit than raw pointer code, and
  Concrete should avoid silently upgrading raw pointers into ordinary safe
  references:
  `https://dev-doc.rust-lang.org/beta/nomicon/what-unsafe-does.html` and
  `https://doc.rust-lang.org/stable/reference/behavior-considered-undefined.html`.
- Counterpoint reading, to keep the claims honest:
  `https://rustmagazine.org/issue-3/is-zig-safer-than-unsafe-rust`.

Borrow the techniques, not the philosophy. Concrete must not adopt hidden
authority, implicit allocation, implicit cleanup, lifetime syntax, trait-object
unsafe abstractions, ambient context, or a broad "unsafe mode." Debug allocators,
sanitizers, and runtime checks are `runtime_checked` / `tested` evidence, never
proof. Unsafe ergonomics are allowed only when they make the trusted assumption
more legible in source and reports.

1. Freeze the unsafe-surface taxonomy:
   - safe references remain second-class and may not be returned;
   - raw single pointers, many-item pointers, fixed-size array pointers, and
     slices are distinct source/report concepts;
   - nullable pointers are explicit (`Option<Ptr>`-style), not the default;
   - every raw pointer/slice carries mutability, provenance/trust class,
     alignment fact where known, and optional length/bounds facts where known.
   Add `docs/UNSAFE_CONCRETE.md` and make it point back to Phase 12's
   `UNSAFE_ISLAND.md`, not duplicate it.
   The design note must include a comparison table mapping Zig's `*T`, `[*]T`,
   `[*c]T`, `?*T`, slices, fixed array pointers, Hare unbounded arrays/slices,
   and Odin pointers/slices onto Concrete's chosen surface. Every row must name
   whether bounds/null/alignment/provenance are statically known, runtime-
   checked, trusted, or unavailable.
2. Add safe-wrapper recipes for the common unsafe shapes: checked slice from raw
   pointer + length, bounds-checked indexed access, non-null construction,
   alignment-refined pointer construction, FFI-owned handle wrappers,
   allocator-backed buffers, and raw-place writes. Each recipe must show the
   public safe wrapper, the private trusted/Unsafe operation, the accepted
   assumption, and the report line that exposes it.
3. Add a debug allocator / checked allocator profile inspired by Zig's
   development allocators: allocation/free stack identity where available,
   double-free detection, use-after-free-like sentinel checks where applicable,
   leak reporting for tests, red-zone or canary checks where practical, and
   allocator-name reporting in audit output. This is `runtime_checked`, not
   proof evidence. It must work with Concrete's allocator-as-value direction:
   `with(Alloc)` grants authority; allocator values name strategy/identity.
   Include the Odin tracking-allocator lesson, but reject Odin's implicit
   `context` as hidden authority: Concrete allocator identity is an explicit
   value or report fact, and allocation permission remains visible as
   `with(Alloc)`.
4. Add pointer/slice runtime-instrumentation hooks for unsafe code: null checks,
   bounds checks for trusted slice views, alignment checks, provenance/trust
   labels, lifetime/owner debugging where the runtime can track it, and explicit
   opt-out for trusted performance paths. A checked profile may trap; a release
   profile may erase checks only when reports still say which checks became
   assumptions.
   Include sanitizer opt-out attributes only as named unsafe/trusted facts
   (`no_sanitize_address`-style behavior must appear in audit output), never as
   comments or unreported compiler flags.
5. Add an unsafe-aliasing policy that avoids Rust's hidden reference-UB trap:
   safe references keep Concrete's ordinary borrow/linearity rules; raw pointers
   do not silently become safe references. Any operation that temporarily
   upgrades raw memory to a safe borrow must state its exclusivity, lifetime, and
   frame assumptions in the wrapper contract/report. No "trust me, this raw
   pointer is an ordinary `&mut` now" shortcut.
6. Add allocator-strategy examples: global allocator, arena/bump allocator,
   fixed-buffer allocator, debug allocator, and test allocator. Every example
   must show allocation authority, allocator identity, cleanup path, and failure
   behavior. Do not add ambient allocation.
7. Add VM/interpreter-style pressure test as the validation workload:
   a small bytecode stack, value array, call-frame/upvalue-like structure, or
   mark/sweep-shaped toy heap that deliberately needs raw pointers/slices. The
   workload should compare the safe-indexed version and the unsafe-pointer
   version, then prove the unsafe version's assumptions are visible in reports
   and checked in debug/profile gates.
   The pressure test should deliberately exercise the cases from the Zig/Rust
   unsafe-code debate: stack-top pointer movement, array/slice access, closure
   or upvalue-like indirection, allocator-triggered collection/checking, and a
   path where a safe-index/handle version is simpler but measurably different
   from the unsafe pointer version. The output is not a performance claim; it is
   an ergonomics/evidence stress test.
8. Add FFI/trusted-boundary red-team tests: wrong length, stale pointer,
   null pointer, alignment mismatch, double free, missing capability, hidden
   allocation, and wrapper that forgets to report the underlying trusted call.
   Safe code must fail to reach the operation; unsafe code must appear in
   `concrete audit --trust-boundaries`.
9. Add syntax/ergonomics only after the taxonomy and gates exist. Candidate
   sugar may include pointer-field access or checked slice construction helpers,
   but it must not reintroduce removed `->` syntax as a general language form
   and must not blur raw pointers with safe references. Ergonomics serve audit,
   not the other way around.
   Candidate ergonomics must be rejected if they make a raw pointer look like a
   safe reference, hide a bounds/null/alignment assumption, or make an unchecked
   operation harder to grep.
10. Add the Phase 15.5 validation artifact:
    `examples/unsafe_concrete_vm/` plus
    `scripts/tests/check_unsafe_concrete.sh`. The gate must run the workload in
    normal and checked profiles, compare interpreter-vs-compiled behavior where
    applicable, prove audit output lists every trusted/Unsafe/allocator/FFI
    assumption, and include at least one mutation per unsafe class that the
    checked profile or audit gate catches.

## Phase 16: Freestanding And Embedded Target

Goal: make Concrete's hosted-vs-freestanding boundary explicit enough for
embedded, kernel, and audit-critical targets without destabilizing the hosted
language.

Prerequisite: the hosted stdlib/runtime boundary, allocator story, target model,
and daily workflow in Phases 8, 9, and 12 must be stable. Freestanding is not
a second backend escape hatch; it is a target profile with fewer assumptions.

Trigger: start this phase when at least one serious workload needs no libc,
explicit allocator/runtime setup, or embedded/kernel-style startup, and the
hosted evidence pipeline can already explain what will be removed.

Done when: a freestanding Concrete program can build under a named target
profile, with no ambient hosted stdlib assumptions, explicit allocator/startup
choices, and an audit report naming every remaining target/runtime assumption.

1. Define `hosted` versus `freestanding` target profiles: libc, startup,
   allocator, panic/abort behavior, stack assumptions, I/O availability,
   floating-point assumptions, and supported capabilities.
2. Split stdlib modules by target profile: core no-alloc/no-OS modules,
   allocator-backed modules, hosted OS modules, and explicitly unavailable
   modules.
3. Define freestanding capability policy: no ambient `File`, `Network`,
   `Console`, or `Process`; target-specific capabilities must be declared and
   audited.
4. Define hardware-access primitives and evidence classes before device-driver
   examples: `volatile`/MMIO operations require explicit `with(Mmio)` or
   `with(Device)` capability, inline assembly is `trusted` and requires
   `with(Unsafe)`, and interrupt handlers are an effectful/trusted boundary
   with explicit target assumptions.
5. Add explicit allocator/runtime hooks for freestanding builds, including
   ownership of allocation failure behavior and cleanup expectations.
6. Add linker/startup configuration: entry symbol, no-main mode, target triple,
   data layout, linker script hooks, and section/layout assumptions.
7. Add freestanding diagnostics: reject hosted APIs, hidden allocation, libc
   calls, unsupported target features, and unavailable capabilities.
8. Add one freestanding example: bounded parser, small checksum/hash kernel, or
   fixed-capacity state machine with no allocation and no hosted I/O.
9. Add one embedded-style audit bundle naming all remaining target assumptions:
   stack, interrupt model if any, allocator/runtime hooks, endian/layout, and
   backend/toolchain boundary.
10. Keep WASM, QBE, and additional backends deferred until freestanding target
    profiles prove the current LLVM path is not enough.
11. Add the Phase 16 validation artifact: one freestanding demo project plus an
    MMIO/device-profile mock audit bundle. The demo must build with no hosted
    APIs, name allocator/startup/linker assumptions, reject hidden libc or
    allocation, and report `with(Device)`/`with(Mmio)`/`with(Unsafe)` evidence
    classes without pretending hardware behavior is proved.

## Phase 17: Public Release Bar

Goal: make Concrete understandable and usable by someone who did not build the
compiler.

Done when: a fresh user can install Concrete, run a proof-bearing example,
inspect its audit bundle, independently verify its evidence root offline, and
understand the claim matrix and remaining trust boundary in under ten minutes.

**Language graduation is a gate, not a date.** Concrete can graduate individual
examples before the language graduates. The language reaches **alpha** only
after the source-contract/proof-link path is usable outside flagship hero work;
it reaches **beta/release** only after ordinary project workflow and external
validation are in place.

**Alpha bar (language can be presented as a usable experimental language):**
- At least one non-author writes, proves, or contract-annotates a useful
  Concrete program and reports that ProofKit + contracts + `concrete prove`
  were worth the discipline.
- Source contracts are primary for at least one flagship, with source-linked
  proofs and no JSON dependency for that flagship's main proof surface.
- `concrete prove` guides a user through contract -> obligation -> Lean/omega/
  `bv_decide` evidence -> source link -> audit.
- Small evidence examples exist for every public claim class:
  `proved_by_lean`, `proved_by_kernel_decision`, `tested_by_oracle`,
  `assumed`, `trusted`, `partial`, and `stale`.
- Core language surface blockers are settled or explicitly deferred with
  examples: pattern cleanup, bytes/text/path, iteration, collections,
  ignored-result diagnostics, and capability polymorphism.
- Runtime safety obligations have defined evidence classes for bounds,
  div-zero, overflow, casts, panic/abort, and unchecked conversion behavior.
- README, website, papers, examples, and roadmap agree and do not outclaim
  audit evidence.

**Beta/release bar (language can be released to users beyond the thesis
audience):**
- `concrete fmt`, `concrete test`, and a minimal project model (`Concrete.toml`)
  exist.
- In-source proof links are the only proof path; `proof-registry.json` support
  must remain absent.
- Release bundles have stable schemas, replay commands, assumption/trust
  reports, and proof-link provenance.
- An installed independent `concrete-cert verify-bundle` path can validate a
  release evidence root offline from untrusted bundle bytes, report the precise
  Core/backend predicates it checked, replay kernel evidence where claimed, and
  keep every unsupported/native boundary visibly trusted.
- A verified-profile policy gate exists and is used by every release-facing
  proof/audit claim; public examples cannot bypass it with stale proofs,
  unapproved assumptions, or hidden trusted/Unsafe boundaries.
- The stdlib/runtime boundary is stable enough for daily examples.
- At least one external user completes the first-user tutorial and a useful
  audit/proof workflow without compiler-author intervention.

1. Define first public release criteria: supported subset, required examples,
   required diagnostics, proof workflow, stdlib/project UX, evidence/policy/
   tooling story.
1a. Add the first-user teaching path as a release-bar artifact, not marketing
    copy. Deliverable: `docs/LEARN_CONCRETE.md` plus a checked tutorial
    transcript that covers install/build/run/test, `Copy` vs linear values,
    consume/destroy/handoff, capabilities in function headers, `Result` /
    ignored-result diagnostics, stdlib basics, one runtime trap, one audit
    report, and the five evidence classes. The tutorial must use runnable
    snippets covered by the doc-snippet gate and must not claim proof where the
    compiler reports only testing, runtime checking, assumption, or trust.
1b. Publish a versioned authoritative language reference before any public
    release claim. The tutorial/book is not the reference. The current
    normative material is scattered across grammar, value-flow, execution,
    invariants, and evidence docs; release users need one versioned document
    whose sections define the language surface, not merely explain it.

    Deliverable: `docs/LANGUAGE_REFERENCE_V0.md` (or generated equivalent)
    covering syntax, lexical forms, type/value model, Copy vs linear values,
    explicit destroy/defer, capabilities, references and borrow blocks,
    statements/expressions, modules/imports/visibility, generics/monomorphization,
    trusted/unsafe/raw pointers, runtime checks/traps, main/exit semantics,
    stdlib boundary expectations, and evidence taxonomy. Every section links to
    its machine-checked gate, diagnostic, or "not yet supported" rejection.
    Gate: `scripts/tests/check_language_reference.sh` verifies every construct
    named in the grammar has a reference section, every anti-feature/rejected
    construct has a diagnostic or policy citation, and README/site/book links
    point to this reference as the normative source.
2. Publish a public claim matrix: what Concrete proves, enforces, reports,
   assumes, and trusts.
3. Add release claim freeze: README, `CLAIMS_TODAY.md`, roadmap, showcase
   manifest, and release bundles must agree.
4. Add compatibility policy for proof artifacts and fact schemas.
5. Add compatibility policy for generated contract/VC obligation IDs:
   obligation IDs should stay stable across harmless formatting and local
   refactors, and any unavoidable churn should be reported as artifact churn,
   not hidden under ordinary proof drift.
6. Add public API compatibility checking before release:
   `concrete api-diff old/ new/ --json` compares exported modules, function
   signatures, capabilities, allocation behavior, evidence classes, stdlib API,
   package interface artifacts, and generated docs. This is the Swift
   API-digester lesson adapted to Concrete: API drift is a reportable fact,
   not a release-note afterthought.
7. Define release/showcase evidence policy by class:
   `proved_by_lean` and `proved_by_kernel_decision` are strong evidence;
   `tested_by_oracle` is supporting evidence; `proved_by_smt` /
   `solver_trusted` require explicit policy approval; `open` and unreviewed
   `assumed` are forbidden for release claims. The policy must be executable by
   the Phase 10 verified-profile command, not just written in prose.
8. Add public examples policy: public-facing examples, website copy, README
   snippets, paper examples, and showcase manifests must not outclaim their
   proof status. Active candidates can be shown as active work, but cannot be
   presented as proved or graduated until their bars land.
9. Add public security/soundness disclosure policy: compiler/proof pipeline
   bugs are security-relevant.
   - 9a. Publish `SECURITY.md`: how to report a compiler/proof-system bug
     privately, response-time commitment, advisory format, and severity
     classes — a soundness bug that lets a false claim go green
     (checker hole, extraction bug, stale-evidence upgrade) is treated as
     highest severity even when nothing crashes. The policy must name scope:
     compiler crashes on valid input, silent miscompiles, verifier/proof
     unsoundness, stale-proof upgrades, hidden capability/allocation/trusted
     boundaries, package-evidence forgery, and stdlib/runtime safety holes are
     all security-relevant for Concrete. Advisories enumerate affected claims by
     reading the obligation ledger of affected releases, not by prose
     recollection. Gate: `scripts/tests/check_security_policy.sh` verifies
     `SECURITY.md` exists, is linked from README/site/release bundle docs, and
     names the supported versions/reporting channel before any public release
     target can pass.
   - 9b. Define the proof-revocation procedure BEFORE the first release that
     carries proof claims: when a shipped theorem/evidence class is
     invalidated (proof bug, axiom-inventory violation, solver unsoundness,
     spec error), there must be a mechanical path that (1) republishes the
     release manifest with the affected facts downgraded to their honest
     class (`open` / `needs_recheck` / `trusted`), (2) emits an advisory
     listing every downgraded claim id, and (3) makes the old bundle
     fail verification loudly rather than silently coexist. Red-team gate:
     `scripts/tests/check_proof_revocation.sh` simulates revoking one
     flagship theorem (e.g. remove it from the axiom-inventory allowlist or
     break its fingerprint) and must show the release bundle flips to
     failing with the downgraded claim named — no false green on a revoked
     proof. Phase 17 #15a's independent verifier must reject the old evidence
     root and any cached receipt that still names the revoked theorem.
9c. Add a certification-style assurance bundle profile before any
    SPARK-class comparison claim. The bundle must include the claim matrix,
    authority/capability report, obligation ledger, proof status, runtime-safety
    status, assumptions, trusted boundaries, package/dependency evidence,
    toolchain versions, replay commands, and any SPARK-class flow/frame facts.
    It must also include an agent-readable summary naming which annotations were
    checked and which were only suggested or future-only. This is a release
    artifact over existing facts, not a second evidence system.
10. Publish `THREAT_MODEL.md` and keep it linked from README, release bundles,
   showcase manifests, and assumptions docs.
11. Add first-user workflow CI: install compiler, create/run one example,
   inspect one audit bundle without repo-local assumptions.
12. Improve onboarding, tutorial, and docs around `proved` / `enforced` /
   `reported` / `assumed` / `trusted`.
12a. Resolve string interpolation / formatting as an explicit release-facing
     decision, not an orphaned gap. V1 rejects string interpolation syntax,
     format macros, and trait-object-style `Display` because they hide
     allocation, dispatch, and formatting authority. The supported path is
     `std.fmt` over `std.io.Writer`, with any string-returning helper carrying
     `with(Alloc)` and any future convenience helper pulled by workloads.

     Required cleanup: remove or close the stale `LANGUAGE_GAPS.md` and
     `docs/bugs/README.md` entries that call interpolation a blocker unless a
     concrete workload reopens the decision. If a future workload proves the
     Writer path insufficient, add a new design note that preserves the same
     evidence: visible allocation, no macros, no hidden dynamic dispatch, and a
     gate proving fixed-buffer formatting remains allocation-free.
12b. Refresh or archive stale gap/backlog documents before release. Documents
     such as `docs/LANGUAGE_GAPS.md`, root-level idea piles, and old stdlib
     reviews may be useful historically, but they cannot sit next to the
     roadmap making claims that contradict closed known holes or current
     semantics. Gate: `scripts/tests/check_gap_docs_current.sh` rejects stale
     "true blocker" / "largest gap" claims unless they map to an open roadmap
     item or known-hole id; otherwise the material moves to CHANGELOG/archive
     with a date and status.
13. Add positioning page against Rust, Zig, Lean, SPARK/Ada, Austral, Hylo,
   Cogent, Dafny, F*, Why3.
14. Add migration/adoption playbook: what C/Rust/Zig code moves first, how to
   wrap libraries honestly, what stays outside Concrete.
15. Add release/install distribution matrix: host triples, checksums/signing,
    install paths, supported/deferred channels.
15a. Define the canonical release-evidence DAG and offline independent bundle
    verifier before packaging or benchmarking it.

    Move the common public/verifier codecs here: versioned canonical encoders
    for source/lock inputs, typed-Core artifacts and Phase 14 receipts,
    obligations/proof bundles, SSA/BackendIR artifacts and Phase 15 receipts,
    codegen-unit/object/link manifests, binary identity, policy/profile,
    assumptions/trust facts, checker/toolchain identities, and proof replay
    payloads. Phase 18 #15b later wraps these in package interface/body formats;
    it does not first define encoders required by this release gate.

    Every edge declares its relation and status (`identity_bound_only`,
    `producer_validated`, or a named independently checked relation), and the
    graph schema names mandatory edges. The bundle exposes one computed evidence
    root, but self-consistency is not authenticity: verification requires an
    externally supplied pinned expected root or a policy-accepted signature/
    builder attestation over that root. A root/signature authenticates identity
    and provenance, not semantic correctness.

    Define `concrete-cert verify-bundle <bundle> --expected-root <root>` plus a
    `concrete verify-bundle` alias only if it invokes the same checker. It runs
    without repository/network state, treats bundle bytes as hostile, recomputes
    canonical nodes/edges/root, reruns Phase 14/15 certificate predicates, and
    validates checker/rule-set/toolchain/target/profile/policy binding. Output
    separate dimensions: `internally_consistent`, `root_matches_expected`,
    `provenance_authenticated`, `core_certificate`, `backend_translation`,
    `proof_replay`, `policy`, `assumptions`, and `remaining_trust`—never one
    ambiguous `verified` badge.

    The schema can reject a missing mandatory edge; the Merkle graph cannot
    discover an arbitrary omitted semantic dependency unless completeness is
    separately proved. Proof/Core and SSA/BackendIR substitutions may fail their
    independently checked relations. Object/binary substitutions fail only the
    expected-root/provenance binding; `BackendIR -> object -> binary` remains
    backend-trusted. A fresh internally consistent root over different native
    bytes is not a semantic mismatch.

    Specify a self-contained offline proof-replay payload and runtime: theorem
    sources or checked proof objects, generated workspace/manifest, exact Lean/
    ProofKit dependencies, kernel/tool versions and hashes, and licenses. If a
    platform bundle cannot provide that replay surface, report
    `proof_replay=unavailable`; it may not satisfy the beta/release bar for a
    claim advertised as offline kernel-replayable.

    Gate expected-root mismatch, accepted/bad signature, proof from build A with
    Core from B, SSA/BackendIR substitution, object/binary substitution,
    schema-required edge deletion, declared dependency-root mismatch, revoked
    proof, unsupported relation mislabeled checked, malformed cycles/duplicate
    ids, and missing checker/theorem identity. A byte mutation must produce a
    different root and fail against the pinned expected root (or lose accepted
    authentication), not necessarily fail internal-consistency parsing. Wire
    `scripts/tests/check_verify_bundle.sh`.
16. Add concrete distribution UX:
    `concrete --version --json`, `scripts/release/build_dist.sh`,
    `dist/concrete-<version>-<target>.tar.gz`, `dist/SHA256SUMS`,
    `dist/SIGNATURES` if signing is enabled, `INSTALL.md`,
    `RELEASE_NOTES.md`, and uninstall/upgrade notes. Nix/Homebrew or similar
    channels may be added only if they can be kept reproducible and
    version-pinned. The archive must ship the independently built
    `concrete-cert` verifier, expose `concrete-cert --version --json`, record its
    source/binary/rule-set hashes separately from the producer compiler, and let
    `verify-bundle` run without repository state or network access. It must also
    ship item #15a's pinned proof-replay runtime/payload dependencies—or report
    `proof_replay=unavailable` and fail any release profile that requires offline
    kernel replay. Include every shipped Lean/ProofKit/runtime component in
    checksums, the supply-chain lock, and `THIRD_PARTY.md`.
17. Add release performance budgets:
    `scripts/tests/check_release_performance.sh` must measure compiler startup,
    cold small-project/stdlib builds, Phase 8.5 warm no-op, private-leaf and
    public-interface edits, proof-query/object-cache hits, relink latency,
    `concrete test`, audit/report generation, proof-check latency, and
    independent bundle-verification latency against
    `release/perf-baseline.json`. Done when
    release CI blocks unexplained regressions and prints the regressed command.
18. Add reproducible release artifact hashes:
    `concrete release --manifest --json` must record source tree hash,
    compiler commit/version, schema versions, target triple, build profile,
    dependency lock hash, stdlib hash, canonical query/artifact dependency root,
    Phase 14/15 checker and rule-set identities, typed-Core/BackendIR/codegen-unit
    roots, emitted object/binary identity, release-bundle evidence root, and
    replay command. V1 does not support old release manifests; schema
    mismatches fail loudly with a regeneration command. Wire
    `scripts/tests/check_reproducible_release_hash.sh`; the gate must build the
    same release artifact twice from a clean tree and compare all hashes.
18a. Add a toolchain/dependency supply-chain lock and license inventory before
    any release claim:
    - The Lean toolchain (`lean-toolchain`), every Lake dependency
      (`lake-manifest.json` pinned revisions), the SMT solver binary
      (name + version + hash, per the SMT replay metadata), and clang/LLVM
      versions are part of the evidence chain — a claim is only as
      trustworthy as the unpinned link. The release manifest (#18) must
      record all of them; drift between the lock and the build environment
      fails the release gate, mirroring the Phase 11 #15
      `proofs/lean-deps.lock` recheck trigger.
    - Publish `docs/THIRD_PARTY.md`: every shipped dependency (Lean
      toolchain, Lake packages, solver, runtime libs linked into emitted
      binaries) with license and role; release claims are blocked while any
      shipped dependency's license is unreviewed or incompatible.
    - Red-team gate: `scripts/tests/check_supply_chain_lock.sh` must fail
      when (a) `lake-manifest.json` and the recorded release manifest
      disagree on any pinned revision, (b) a dependency is present in the
      build but absent from `THIRD_PARTY.md`, or (c) the solver version
      recorded in replay metadata differs from the one in the lock.
18b. Add the language/stdlib/artifact deprecation policy (moved up from Phase
    18 — users of a released language need the policy BEFORE the release, not
    after): what stability the first release promises for syntax, stdlib API,
    proof/fact schemas, and obligation ids; how deprecations are announced
    (diagnostic with a replacement, minimum deprecation window measured in
    releases); and what may never break silently (anything that would flip an
    evidence class without `needs_recheck`). The Phase 19 migration tooling
    (`concrete migrate`) implements this policy; the policy itself is a
    release-bar document. Gate: `concrete api-diff` (#6) must classify every
    public-surface change as `compatible`, `deprecated(window)`, or
    `breaking`, and the release gate fails on `breaking` without a recorded
    policy exception.
18c. Add the external-contributor and public-platform surface before release.
    Write `CONTRIBUTING.md` as an operational checklist, not generic etiquette:
    how to add a language feature, which compiler modules usually change, which
    gates to run, which docs must be updated, how to add positive/negative/
    adversarial fixtures, and how to update
    `scripts/tests/example_manifest.txt` in the same commit as any new
    `examples/` `.con` file (this broke CI twice on 2026-06-09/10). Add
    `scripts/tests/check_contributing_contract.sh`; the gate must prove every
    example project has a manifest entry, every roadmap-required gate is named
    in either `Makefile` or CI, and contributor docs mention source changes,
    tests, docs, roadmap/changelog, claims, and release-bundle impacts. Publish
    a public platform-support statement (`docs/PLATFORM_SUPPORT.md`) naming
    Tier 1/Tier 2/unsupported hosts and targets, including Linux/macOS
    x86_64/aarch64 and Windows status. Windows must not remain ambient: either
    it is a named supported target with CI and hosted-capability semantics, or
    it is explicitly "not v1" with a pull trigger (external adopter, workload,
    or release requirement) and a list of blocked surfaces: paths, process,
    env, console, file locking, line endings, and toolchain/LLVM availability.
    Add performance-claim discipline: public copy may not imply "fast systems
    language" unless `concrete bench` and release performance gates publish
    replayable numbers; otherwise docs must say performance claims are not made
    yet.
19. Ship the first narrow public release only after the above are green.
20. [relocated from closed Phase 4] Artifact and docs stability hardening:
    schema-version rejection gates (refuse to silently misread an artifact whose
    schema version differs from the compiler's), source-location privacy /
    redaction modes for emitted diagnostics and artifacts (Phase-4 #38), and
    docs-drift SEMANTIC checks beyond the artifact-existence gate
    (`check_docs_drift.sh`) — e.g. `Status:`/`Verified:` metadata and stale-claim
    marker detection. (Found not mechanically robust as a default during the #44
    work; they belong here, gated for the release bar.)
21. Add the Phase 17 validation artifact:
    `scripts/tests/check_release_candidate.sh` installs the dist archive into a
    clean temp prefix on every supported host, runs `concrete --version --json`,
    builds one example, runs one proof/audit workflow, verifies checksums and
    signatures if enabled, checks `release/perf-baseline.json`, runs
    `concrete api-diff` against the previous public interface snapshot, and
    confirms the bundle contains the claim matrix, threat model, public
    examples policy, replay commands, schemas, assumptions/trust reports,
    `SECURITY.md` (#9a), the supply-chain lock and `THIRD_PARTY.md` (#18a),
    the deprecation policy and a clean `api-diff` classification (#18b), and
    the tutorial transcript from someone who did not build the compiler. It
    must also run the proof-revocation drill (#9b) against the candidate
    bundle, verify the evidence DAG offline through #15a from the installed
    independent checker, and run tamper cases for source/Core/proof/BackendIR/
    object/binary/dependency-edge substitution. The report must distinguish
    Core/proof or SSA/BackendIR relation failures from object/binary failures
    against the expected root/authentication; native semantic edges remain
    explicitly backend-trusted. Release replay begins with a
    clean cache or `--incremental=off`; a local cache cannot be an undeclared
    release input.

## Phase 18: Packages And Dependency Evidence

Goal: let package users inspect proof, trust, capability, and assumption facts
before adopting a dependency.

Done when: packages have manifests, lockfiles, package-aware facts, trust
policies, provenance, independently checked interface/evidence receipts,
Phase 8.5-aware reuse/invalidation, and registry protocol.

Prerequisite: Phase 8.5 completed. If it was rejected/deferred, Phase 18 may
start only after an explicit roadmap amendment replaces these artifact/query
dependencies; package work may not grow a private cache/driver as a workaround.

1. Expand package artifacts only after reports, policies, assumptions,
   interface artifacts, and CI gates prove what packages must carry. The first
   package artifact refactor must define exact files:
   `Concrete.package.json` (manifest summary), `Concrete.lock`,
   `.concrete/interfaces/<module>.json`, `.concrete/facts/<module>.json`,
   `.concrete/evidence/<module>.json`, `.concrete/certificates/<module>.json`,
   optional `.concrete/verifier-inputs/<module>.car`, and
   `.concrete/docs/<module>.json`.
   Each artifact must name compiler version, schema version, package id,
   source hash, interface/body hash, dependency/evidence root, Phase 14/15
   checker/rule-set identities and receipts where applicable, authority budget,
   assumptions, proof/evidence summaries, and replay commands. Add
   `scripts/tests/check_package_artifacts.sh`; the gate must prove package
   consumers read these artifacts rather than source-private side channels.
   This phase is the main 20k+ LOC scale boundary: large Concrete programs need
   package/workspace boundaries, import hygiene, visibility/API stability,
   versioning, generated docs, and evidence summaries before the language can
   claim to support multi-team codebases rather than only single-repo examples.
2. Design and parse package manifest.
3. Add version constraints, dependency resolution, and lockfile.
4. Add workspace and multi-package support.
5. Add package-aware test selection.
6. Split interface artifacts from body artifacts at package/workspace scale.
   Promote the Phase 8.5 internal split and stable ids into versioned public
   package artifacts; do not expose or rename the opaque internal cache format
   as a package protocol.
   Interface artifacts expose public names, types, capabilities, contracts,
   allocation/effect summaries, deprecation/version facts, and evidence classes.
   Body artifacts contain implementation fingerprints, private obligations,
   proof links, emitted IR hashes, and private diagnostics. A dependent package
   may compile against interface artifacts without seeing private bodies, but
   audit/release bundles must still show when a public claim depends on a
   private body proof, trusted boundary, or assumption.
6a. Define the public package codecs and `PackageInterfaceCertificateV1` before
    consuming certificates.
    Canonically encode the package manifest, public interface/API payload,
    body/evidence root, optional verifier input, and certificate/receipt
    envelope. Keep `api_identity` separate from `implementation_evidence_root`:
    a private body edit may preserve exported names/types while invalidating
    capabilities/effects inferred from bodies, proof/trust/allocation/runtime
    summaries, mono/codegen facts, or other implementation evidence.

    `PackageInterfaceCertificateV1` checks package/schema/root binding and,
    where a canonical checked-Core/body verifier input is shipped, derives the
    supported exported facts from that hash-bound artifact plus Phase 14/15
    receipts. It may not validate a summary against a self-authored summary.
    When private verifier input is withheld, public API shape can remain
    structurally checked, but body-derived capability/trust/allocation/evidence
    summaries stay `compiler_validated` with the producer trusted unless backed
    by a separately replayable proof. The package report must expose that split.

    Gate canonical round trips, old schema, interface/body-root swapping,
    private-body edit with stable API identity but changed evidence root, and a
    coordinated tamper that changes interface summary + matching certificate
    while retaining the old body root. Wire
    `scripts/tests/check_package_interface_certificate_schema.sh`.
7. Add proof-aware package artifacts: facts, obligations, proof status, trusted
   assumptions, policy declarations, package-boundary evidence summaries.
   Required statuses: `proved_by_lean`, `proved_by_kernel_decision`,
   `solver_trusted`, `tested_by_oracle`, `enforced`, `assumed`, `trusted`,
   `partial`, `stale`, `vacuous`, `missing`, and `ineligible`. The artifact
   must record whether evidence is package-local, inherited from a dependency,
   or trusted through a boundary.
7a. Verify dependency interface certificates before consuming package facts or
    reusable artifacts.
    A content hash establishes identity, not semantic validity. Run item #6a's
    dedicated package-interface checker and, only over embedded canonical
    verifier inputs of the right type, the Phase 14 Core / applicable Phase 15
    translation checker. Cache the independent receipt by package root +
    checker/rule-set version and bind it into the importing project's Phase 8.5
    dependency graph. Invalid, stale, unsupported-schema, or unsupported-
    predicate certificates trigger a source rebuild when source and policy
    permit it, or a loud policy failure; they never silently become trusted
    facts. Source rebuild first produces `compiler_validated` facts and becomes
    independently checked only after the applicable checker reruns.

    Import authority/evidence constraints may consume independently checked
    interface facts or facts explicitly labeled `compiler_validated` with the
    producing compiler still trusted; they may not erase that distinction. Gate
    a valid package, tampered interface fact, certificate from another
    package/build, interface+certificate consistently changed against an old
    body root, stale private body with unchanged public API but changed evidence
    root, changed public interface, old checker version, withheld verifier input,
    unsupported predicate, proof revocation, and source-rebuild fallback. Wire
    `scripts/tests/check_package_certificates.sh`.
8. Add module/package authority budgets after package graphs are real, and make
   imports fact-checked boundaries. Imports do not grant capabilities; they
   declare and constrain facts about the imported interface. The first concrete
   fact class is authority: support source-level or manifest-level constraints
   such as `import std.parse requires(no File, no Network, no Unsafe)` and
   package-wide budgets such as `allowed = ["Alloc"]`. A dependency capability
   widening must be a build/audit diff and, when constrained, a build failure
   until explicitly accepted. The design must leave room for the same import
   mechanism to constrain allocation (`no Alloc` / bounded allocation), trust
   (`no trusted`, `no extern`, `no Unsafe`), runtime-failure profile, platform
   (`hosted` / `freestanding` / `posix`), arithmetic-site policy, determinism,
   constant-time / secret-flow claims, and supply-chain facts such as source
   verification or license.
   This is the package/dependency analogue of Elm ports and Elm package
   discipline, adapted to Concrete rather than web apps: code may import a
   dependency's values, but it does not inherit ambient authority, hidden host
   effects, or private assumptions. The imported interface artifact is the
   boundary contract. Roc's platform/host separation is also a useful warning:
   host authority must be explicit at the boundary, never a package-local
   convenience that disappears from reports.
   Security threat model: if a dependency is compromised or upgraded to do more
   than it used to do, the importer must be able to fail closed. Examples:
   `import json as j requires(no File, no Network, no Unsafe, no trusted)`;
   `import hmac.compute requires(proved_by_lean, no Unsafe)`; package-wide
   `allowed_caps = ["Alloc"]`; or `requires(freestanding, deterministic,
   license = "MIT OR Apache-2.0")`. The checked fact set—read through item
   #7a's independently-checked-or-explicitly-trusted boundary—must include at
   least:
   public capability set, allocation authority/profile, `trusted`/extern/FFI/
   `Unsafe` use, assumption set, evidence class floor, proof staleness/vacuity,
   runtime-failure policy, arithmetic-site classification, hosted/freestanding/platform facts,
   dependency provenance/source hash, license status, and supply-chain lock
   identity. A change in any constrained fact is not a warning hidden in prose:
   it is either a build failure or an explicit audit diff requiring acceptance.
   Write
   `research/packages-tooling/import-fact-constraints.md`, then add
   `docs/IMPORT_FACT_CONSTRAINTS.md`,
   `examples/package_authority_imports/{rejects_network_widening,accepts_file_without_granting_file,rejects_unsafe_dependency,rejects_alloc_widening,rejects_hosted_dependency,rejects_trusted_widening,rejects_evidence_downgrade,rejects_new_assumption,rejects_license_drift}/`,
   and `scripts/tests/check_import_fact_constraints.sh`; the gate must prove
   imports read summaries from interface artifacts, do not grant authority to the
   importer, compose with package-level budgets, and reject capability/allocation/
   trust/platform/evidence/assumption/supply-chain fact drift until explicitly
   accepted.
   The first report view should be deliberately small and agent-readable:
   `concrete audit dependency <pkg>` or the package-audit bundle must show
   `public capabilities`, `allocation`, `trusted/Unsafe/extern`, `assumptions`,
   `minimum evidence`, `platform`, `license`, and `source hash` in a stable
   table. This report is the thing an AI assistant, reviewer, or CI policy reads
   before accepting a dependency update.
9. Add dependency trust policy: trust widening across boundaries, review and
   inheritance.
10. Add package-level assumption inheritance: dependency assumptions must be
    visible to dependents and release bundles.
11. Add package provenance and publishing model.
12. Add package registry server protocol and trust model.
13. Add API docs publishing for packages and stdlib:
    `concrete doc --format json`, `concrete doc --format html`,
    `docs/api/std/<version>/`, and package docs under
    `docs/api/packages/<name>/<version>/`. Generated docs must carry version,
    module path, capabilities, allocation behavior, evidence class,
    deprecation status, and source links where available. Published docs must
    be reproducible from the package artifact.
14. Add package documentation hosting/export format only after `concrete doc`
    and package artifacts are stable: static HTML, JSON docs, versioned stdlib
    docs, package docs, release-note links, and
    `scripts/tests/check_docs_publish.sh` to prove generated docs are
    reproducible.
15. Design evidence-typed imports before the package fact schema freezes. A
    dependent package should be able to demand an evidence floor at the import
    boundary, for example `import hmac.compute requires(proved_by_lean)` or a
    manifest-level equivalent for all imports from a dependency. The compiler
    must check the requirement against the dependency's interface/evidence
    artifacts and fail the import if the evidence is missing, stale, vacuous,
    downgraded, inherited only through an unaccepted assumption, or weaker than
    the importing package's policy. This is a package-boundary trust-chain
    feature, not a local proof feature: it must define how evidence classes are
    ordered or deliberately non-ordered, how `solver_trusted`/assumed/trusted
    dependencies are named, how proof revocation invalidates dependents, and
    how release bundles explain the imported requirement. Write
    `research/packages/evidence-typed-imports.md` before implementing the
    surface, and keep it consistent with import authority constraints from
    Phase 18 #8 (e.g. `import hmac.compute requires(proved_by_lean, no Unsafe)`).
    Then add `docs/EVIDENCE_TYPED_IMPORTS.md`,
    `examples/package_evidence_imports/{requires_lean,allows_solver_trusted,rejects_stale,rejects_vacuous}/`,
    and `scripts/tests/check_evidence_typed_imports.sh`; the gate must prove
    dependency evidence is read from package artifacts, not source-private side
    channels, and that an evidence downgrade breaks the importing package.
15a. Add package-level SPARK-class assurance summaries once frame/dependency
    contracts exist. Interface artifacts should expose public contract facts,
    read/write/modifies summaries, dependency-flow summaries, ghost/spec
    assumptions, capability requirements, trusted boundaries, and evidence
    class per public function. Package consumers and agents must be able to ask
    "what may this dependency read, write, depend on, assume, trust, or prove?"
    without inspecting private bodies.
15b. Extend content-addressing beyond proof fingerprints for package/evidence
    artifacts.

    Extend Phase 8.5's opaque internal artifact roots into stable, versioned
    **public package** encodings rather than implementing a second hashing/cache
    system. Reuse the canonical typed-Core, obligation/proof, BackendIR,
    certificate, and release-node encoders already required by Phases 9/14/15
    and Phase 17 #15a. This item owns package interface/body/evidence/docs
    wrappers, public schema compatibility, and package-root composition—not the
    first encoding of common verifier inputs.

    The hard part is not hashing; it is deterministic canonical serialization.
    Before package hashes are authoritative, each package wrapper must define a
    stable, versioned, pointer-free canonical encoding: no map iteration order,
    temp paths, generated-name nondeterminism, host-dependent formatting, or
    schema-ambiguous fields. Borrow the Zig InternPool lesson here: artifacts
    that need stable identity must serialize from explicit IDs and normalized
    structure, not from incidental in-memory shape.

    Done when package interface/body/evidence/docs wrappers have versioned
    canonical encoders over the earlier common nodes; identical semantic package
    content hashes identically across repeated runs; a deliberately
    nondeterministic wrapper is caught by a gate; and package cache/dependency
    keys use these roots instead of source paths or human names.
15c. Add optional shared/remote artifact reuse only after the local Phase 8.5
    store and independent package verification are mature.
    Define an authenticated transport/protocol for content-addressed package,
    proof, and codegen artifacts; the server is an untrusted blob store, not a
    compiler authority. V1 permits only fetch-by-digest where that expected
    digest is already authorized by a lockfile, pinned release root, or
    policy-trusted builder attestation. An untrusted remote action-key-to-digest
    mapping is never an authority for semantic reuse. Every download must pass
    size/schema/digest and dependency-root checks, the applicable Phase 14/15
    certificate checker, target/profile/toolchain binding, and package policy
    before entering the local store.

    Remote object, link, and binary outputs may not enter a strict build merely
    because their hashes are internally consistent: the Phase 14/15 independent
    relation stops at BackendIR. Regenerate native outputs locally. A separately
    opt-in trusted-builder mode may accept signed native outputs only as
    `builder_attested` / `backend_trusted`; its signature authenticates producer
    provenance, not object semantics.
    Upload only deterministic canonical artifacts with explicit privacy/
    redaction policy; never upload source, diagnostics, proof attempts, or
    credentials implicitly. A signature authenticates provenance, not
    correctness.

    This item is optional for the first package release and must not complicate
    local builds. Gate hostile/corrupt server responses, replayed old roots,
    cross-target objects, revoked proofs, concurrent upload, offline fallback,
    and local recomputation parity. Include coordinated poisoning where a
    server rewrites the action mapping, artifact, manifest, and every internal
    hash consistently; it must still fail without an independently authorized
    expected digest. Record CI/team speedups, but keep correctness independent
    of network/cache availability.
16. Add the Phase 18 validation artifact: a multi-package workspace project
    with dependency resolution, lockfile, package-aware tests, interface/body
    artifact split, dependency trust policy, assumption inheritance, authority
    budgets, provenance, independently checked dependency certificates,
    evidence-typed imports, published docs, downstream reuse after a dependency
    private-body edit, invalidation after a public-interface edit, and
    release-bundle evidence for every dependency. Include remote-cache hostile
    input/offline fallback if #15c is implemented. Wire it as
    `examples/package_workspace/` plus
    `scripts/tests/check_phase18_packages.sh`.

## Phase 19: Editor And Human Tooling

Goal: make evidence visible where developers work.

Done when: editor/LSP/tooling exposes the same facts as CI and command-line
reports through the Phase 8.5 session/query graph, with bounded edit
invalidation and certificate freshness, without inventing a second truth source.

Prerequisite: Phase 8.5 completed. If it was rejected/deferred, Phase 19 needs a
recorded replacement architecture before starting; an editor-only fact database
or cache remains forbidden.

1. Add artifact viewer integration for proof/evidence facts.
2. Add compiler-as-service / LSP entrypoints after diagnostics and facts are
   structured. The service must host the Phase 8.5 `CompilerSession`; it may not
   wrap batch `runFrontend` as its normal edit path.
2a. Reuse the incremental query/certificate graph as the only editor fact
    engine.
    LSP/editor requests use the same typed query keys, dependency edges, local
    store, invalidation rules, validation records, independently checked package
    receipts, and evidence views as CLI/CI. Tag each response with project
    revision, subject/dependency root, completeness (`strict | partial`),
    freshness, checker/rule-set version where applicable, and cache/query
    provenance kept separate from evidence status. Cancelled or tolerant
    partial computations cannot enter complete caches or answer proof/release
    queries.

    Gate a scripted edit sequence covering comment/span-only, private body,
    public signature/capability/contract, target/profile, policy, proof, and
    dependency-certificate changes. Assert the bounded Phase 8.5 query execution
    set as well as CLI/LSP fact equality; an editor-only database, stale hover,
    or batch whole-project rebuild for a private leaf edit fails the gate.
3. Add hover/type info for capability status, proof status, predictable status,
   assumptions, obligations, and trusted boundaries.
4. Add obligation navigation: jump from source contract/index/mod/loop to the
   generated obligation and discharging theorem.
5. Add refactor support that preserves or updates facts/proofs where possible.
6. Add dependency audit UI for capability, allocation, FFI, trust, evidence,
   predictability, proof-obligation drift.
   The UI must expose the same boundary facts as Phase 18 #8, not a prose
   summary: public capabilities, allocation authority, trusted/Unsafe/extern
   use, assumptions, evidence floor, platform, license, and source hash. This
   is explicitly for humans and AI agents reviewing imports: an editor hover or
   command palette action should answer "what can this dependency do, what does
   it assume, and what evidence does it carry?" without reading private bodies.
7. Add backwards-compatibility regression corpus once public users exist.
8. Language/versioning/deprecation policy: MOVED to Phase 17 #18b — the
   policy must exist before the first public release; only the tooling that
   implements it lives here.
9. Add migration/deprecation tooling after the policy exists (Phase 17 #18b):
   `concrete migrate --check`, `concrete migrate --apply`, diagnostics for
   deprecated syntax/APIs, suggested replacements, edition/version notes where
   needed, and mechanical rewrites only for transformations that preserve
   evidence facts or explicitly mark them `needs_recheck`. The tool must read
   `concrete api-diff --json`, `concrete doc --format json`, and the final
   obligation ledger rather than scraping text.
10. Add API-docs/editor integration: LSP go-to-docs for stdlib/package APIs,
    evidence/capability/deprecation badges in hover, and diagnostic links to
    `concrete doc` output. The LSP payload must reuse the same doc/evidence
    JSON as the CLI.
11. Add a playground or local web runner only after the release subset is
    stable: `concrete playground --local` or a static hosted runner with
    preloaded examples, no hidden claims, visible evidence class, audit output,
    and clear sandbox/timeout/resource assumptions. This is a teaching surface,
    not a second compiler pipeline.
12. [relocated from closed Phase 4 — #11 tail] Route obligation / proof / policy
    facts into the structured `Diagnostic` record / `--diagnostics-json` channel
    so LSP/editor and CI-JSON consumers see them (array-bounds, solver-policy,
    vacuous-contract, stale-proof, …) — without duplicating the
    `ObligationCore`/report model. Includes interpreter structured diagnostics
    (the deferred Phase-4 #18a). Pull when a real consumer (LSP / CI JSON parser)
    needs machine-readable obligation diagnostics; see LANGUAGE_GAPS for the
    frontend-vs-obligation diagnostic split.
13. Add editor and agent diagnostics for SPARK-class assurance facts after the
    facts exist: failed loop invariants, weak variants, missing frame facts,
    over-broad `writes`, unsatisfied `depends`, ghost/spec partiality, package
    evidence downgrades, and runtime-safety obligations that need a frame or
    invariant. The LSP/JSON payload must reuse the obligation/evidence ledger
    and point agents to the validation command; no editor-only proof status.
    Also surface development-only expectations from Phase 12 #20 as
    `dev_checked` / `tested` facts, never as proof. Editor and agent prompts
    should suggest "promote this to a contract/obligation" when release or
    high-integrity policy requires stronger evidence.
13a. Add installed-binary feature discovery for agents and editor tooling.

     Deliverable: extend the Phase 6E `concrete help --json` / command catalog
     with `concrete agent features --json` only if agent-specific fields need a
     separate view. The catalog must describe supported
     commands, accepted inputs, emitted artifacts, schema versions, evidence
     classes, replay commands, policy gates, forbidden actions, and examples of
     valid next steps.

     Rule: this catalog is the source of truth that MCP, LSP, docs, and LLM
     prompts wrap; they must not duplicate a stale list of Concrete features.
     Done when golden JSON snapshots exist and one agent workflow discovers
     proof synthesis, replay, capability diffs, and counterexample saving from
     the installed binary alone.
14. Add the Phase 19 validation artifact:
   `scripts/tests/check_phase19_editor.sh` runs a scripted LSP/editor session
   or golden transcript over one real project, proving hover, diagnostics,
   obligation navigation, proof/evidence facts, dependency audit UI, refactor
   behavior, docs integration, deprecation diagnostics, and playground output
   match CLI facts rather than inventing a second truth source. It also checks
   revision/freshness tags, cancellation safety, minimal incremental
   recomputation, independently checked dependency receipt changes, and
   cache-off/on equivalence.

## Phase 20: Concurrency And Research-Gated Extensions

Goal: keep speculative ideas gated until Concrete's proof/evidence foundation
can contain them honestly.

Done when: each research idea is either pulled into an earlier phase by a
forcing example, explicitly deferred, or rejected.

1. Keep concurrency design-only until the v1 surface is frozen:
   capability lattice, scopes, spawn/join, linear handles, bounded channels,
   result flow, ownership transfer, rejected forms, and report schema. This
   includes the memory-model question Concrete must not answer accidentally:
   atomics, synchronization, shared mutable state, data-race freedom,
   capability-gated thread authority, and proof/evidence classes for concurrent
   code all remain research-gated until a formal model and pressure tests exist.
   Async syntax remains rejected for v1 per `docs/ANTI_FEATURES.md`; the
   positive research direction lives here and in `docs/EXECUTION_MODEL.md`:
   explicit concurrency primitives, visible effects, linear handles, and
   bounded scheduling/failure evidence rather than hidden async lowering or a
   second control-flow semantics.
2. Build concurrency pressure-test sketches and expected reports before
   implementation.
3. Mechanize the v1 concurrency formal model before claiming safety.
4. Implement OS threads/scopes/channels only after the model and reports are
   stable.
5. Research typestate only if a current state-machine/protocol example needs
   it.
6. Research arena allocation after bounded-capacity/allocation-profile work
   exposes a concrete gap.
7. Research exact WCET/cache/pipeline behavior only with a target/hardware
   model.
8. Research binary-format DSLs only if packet/ELF examples show repeated
   parser boilerplate.
9. Research hardware capability mapping after source-level capabilities and
   package policies are stable.
10. Broaden the proof-relevant interpreter toward Miri-style UB checking only
    if the proof-subset interpreter proves valuable.
11. Investigate a sized/indexed ProofCore evaluator only if the current
    fuel-indexed evaluator remains repeated proof debt after HMAC and at least
    one other substantial loop/composition proof. This is ProofCore v2
    research, not a migration commitment; see
    [research/proof-evidence/SIZED_EVALUATOR_INVESTIGATION.md](research/proof-evidence/SIZED_EVALUATOR_INVESTIGATION.md).
12. Research persistent equality/rewrite state after backend contracts,
    semantic diff, and proof/evidence pipeline are stronger.
13. Do not adopt row effects for v1. The default design stays object-capability
    and audit-visible: authority should be obvious in source, not hidden behind
    abstract effect inference. Revisit only as a research note if explicit
    capabilities create a proven, repeated blocker in real programs after the
    stdlib and concurrency pressure tests exist.
14. Research a generational-reference-style dynamic fallback only if a forcing
    workload proves static linearity plus second-class references are too
    restrictive. The target use case is not ordinary ownership; it is a narrow
    escape for liveness/provenance facts around observers, back-references,
    callback-heavy object graphs, or host/FFI handles that Concrete cannot
    prove statically without unacceptable surface complexity. If adopted, the
    runtime check discharges an obligation as `checked_dynamically`, never as
    `proved`, and audit/report output must distinguish it from static
    ownership proof.

    Do not pull this forward without a pressure test and a decision record.
    The research pass must compare Vale-style generational references and
    region/frozen-scope ideas, explicitly reject any vague "zero-cost safety"
    claim, and prove the fallback does not become a hidden borrow checker,
    hidden GC, implicit Drop, or a way to weaken the linear default.
15. Research dogfooding Concrete's evidence model onto the compiler's own TCB.
    This is the logical endpoint of "evidence-carrying artifact applied to the
    compiler": rewrite or mirror selected compiler passes/checkers in Concrete,
    attach contracts, and use Concrete's own linearity/capability/proof pipeline
    to reduce the trusted base of the toolchain itself. This is NOT a near-term
    migration commitment and must not block Phase 7-19 work; it is a research
    bet to keep visible so the project does not stop at verifying user programs
    while leaving the compiler as an opaque trust-me artifact. This rewrite-in-Concrete route is complementary to the Phase 14 verified-compiler endgame; the primary route there is proving the existing Lean-hosted passes semantics-preserving, not re-implementing them.

    This is distinct from the Phase 14/15 independent certificate checker. That
    checker re-derives narrow predicates over compiler artifacts in a small Lean
    target; this research item asks whether selected compiler passes should be
    implemented or mirrored in Concrete itself. Do not delay the independent
    checker while waiting for self-hosting evidence.

    First pressure-test only: choose one narrow pass or checker helper with a
    small state space (for example a value-flow checker slice, a type-policy
    predicate, a report fact transformer, or a simple SSA cleanup rule), specify
    its input/output contract, and compare the Concrete implementation against
    the Lean/compiler implementation on a generated corpus. Done when the
    decision record states whether the approach actually shrinks the TCB, merely
    duplicates it, or creates a second source of truth. Pull forward only if a
    real pass can be checked without weakening the no-second-truth-source rule.
16. Research a Datalog-style / stratified relational **rule layer** only when
    the shipped Phase 8.5 `CompilerDB` has real relational consumers that typed
    queries/maps cannot express cleanly.

    `CompilerDB` facts — pass-agreement edges, borrow conflicts, E0293
    container exclusions, provenance edges, dependency/invalidation, package
    evidence, and proof/evidence queries — are naturally relational. A
    Datalog-like or stratified-facts model is a good design reference for that
    layer: derived facts are explicit, dependency edges are queryable, cycles
    and invalidation have a discipline, and audit/report views can ask
    relational questions without reparsing prose.

    Phase 8.5 already owns the typed acyclic query/cache driver, scheduling, and
    invalidation; do not reopen or replace it here. What remains deferred is a
    general Datalog/stratified derivation layer or user-visible query language.
    The trigger is a real `CompilerDB` relation family whose rules are awkward
    as ordinary typed Lean functions/maps/joins. The first investigation should
    compare a small stratified internal rule layer against those ordinary typed
    functions, reuse Phase 8.5 dependency/certificate machinery, and keep the
    user language unchanged: no user-facing logic language, no implicit effects,
    and no hidden second truth source.

    Done when a design note demonstrates one concrete relation family
    (for example provenance invalidation or package evidence queries), includes
    an acyclicity/stratification story, shows replayable derived-fact output,
    and rejects a cyclic or stale derived fact. Until then, this remains a
    future rule-layer reference, not unfinished incremental-compiler work.
17. Add the Phase 20 validation artifact: one pressure-test sketch, expected
    report, and decision record for every research-gated extension
    (concurrency, atomics/memory model, typestate, arena allocation, WCET,
    binary-format DSLs, hardware capability mapping, Miri-style interpreter,
    sized evaluator, persistent rewrite state, row effects, generational
    dynamic fallback, compiler self-verification, and Datalog-style relational
    facts). No research item graduates unless its forcing example, report shape,
    evidence class, and rejection or pull-forward criteria are recorded.
