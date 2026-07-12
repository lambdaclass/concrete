# Concrete Roadmap

This document is the active execution plan. It answers one question:
**what should happen next, in what order?**

**Current frontier: Phase 6.** Phases 1–5 are done and removed from this active roadmap. Phases 1–2 were folded into
CHANGELOG.md and design docs; Phases 3–4 closed (core-complete) — their detailed
records are
[docs/PHASE3_OBLIGATION_CORE_AUDIT.md](docs/PHASE3_OBLIGATION_CORE_AUDIT.md) and
[docs/PHASE4_COMPILER_LEDGER_AUDIT.md](docs/PHASE4_COMPILER_LEDGER_AUDIT.md), and
their few deferred, no-active-soundness-risk tails were folded into later phases:
proof-status / `prove` literal-ledger views → Phase 11; interpreter-vs-compiled
harness + ref-return hardening → Phase 14; `audit --compiler` self-audit → Phase
10; obligation→structured-`Diagnostic` bridge → Phase 19; schema-version /
source-location-privacy / docs-drift-semantic → Phase 17.

The roadmap is linear. Phases are ordered, and items inside a phase are ordered
unless explicitly marked as a constraint or a deferred research note. Read the
document as one queue:

1. *(done — Phase 3, closed)* consolidate the compiler's obligation pipeline so
   contracts, VCs, runtime safety, asserts, SMT, policy, reports, and proof
   workspaces all read from one typed evidence ledger;
2. *(done — Phase 4, closed)* consolidate the ordinary compiler pipeline: project
   loading, pass boundaries, typed IR, diagnostics, source maps, backend
   contracts, and command plumbing;
3. *(done — Phase 5, closed; const generics deferred to Phase 7 #8f by trigger)*
   broaden the ordinary language surface now that the compiler pipeline supports
   it: modules/imports, project shape, tests, diagnostics, bytes/text/path +
   owned byte/text views, collections;
3b. **(active frontier — Phase 6+)** make Concrete usable as a normal everyday
   language: patterns, iteration, capability polymorphism, daily-workflow UX,
   project ergonomics;
3c. harden the compiler pipeline before broadening the stdlib: centralize
   shared type/policy predicates, make stage contracts explicit, close
   pass-agreement gaps, and add coverage so later features cannot make Check,
   Elab, Mono, CoreCheck, Lower, interpreter, reports, and proofs disagree;
4. validate the bet with one real workload before broadening the platform:
   #35 is the next forcing function after the callable-values core, and it
   should drive the first stdlib/tooling gaps instead of waiting for a complete
   batteries-included library;
5. build the standard library and core APIs as a small coherent runway for that
   workload: Gleam-sized coherence first, Zig-style allocator/authority
   explicitness, Rust/Ada/SPARK-style safety/proof discipline, and Go/Odin
   breadth only after workload pressure;
5a. after the stdlib/workload slab exists and the external-validation gate
   returns **GO**, turn the stable compiler facts into a real incremental
   artifact engine: persistent typed queries, conservative dependency
   invalidation, content-addressed reuse, cached codegen units, and one driver
   shared by build/check/test/prove/audit/LSP;
6. finish remaining proof-authoring cleanup: colocate example Lean proofs with
   their Concrete examples, keep generated proof workspaces source-linked and
   replayable, and leave the deeper `ProofCore` / spec-registry split deferred
   unless it becomes necessary;
7. harden audit / proof-status / trust gates around source contracts,
   spec provenance, evidence classes, tool-version drift, and oracle evidence;
8. close the release-blocking predictable/provable/runtime-safety gaps,
   starting with casts, loop-derived bounds, runtime-safety policy, and the
   remaining profile story after array bounds, div/mod-zero, and
   opt-in overflow obligations;
9. keep later research items later unless a prior gate forces them.

Cross-cutting items that must stay visible while the linear queue advances:

- **Compiler pipeline spine:** Phase 6B establishes semantic truth ("one meaning
  per program") and structured validation records, Phase 6C makes that truth
  observable/replayable and records a shadow query graph, Phase 8 validates the
  bet on external-facing examples, and Phase 8.5 turns the graph into the real
  incremental artifact driver only after a GO verdict. Phase 9 measures and
  reduces proof cost; Phases 10-14 turn evidence into source/proof claims and
  extract the independent Core-certificate checker; Phase 15 extends independent
  checking across the BackendIR translation slice; Phase 17 binds checked
  artifacts into release roots; Phases 18-19 reuse the same graph for packages
  and editor tooling. Do not let later proof/backend/tooling work bypass this
  order: semantic facts first, observability second, external validation third,
  incremental reuse fourth, independently checked evidence after that.
- **Incremental / certificate trust split:** Phase 8.5 cache reuse is initially
  a compiler optimization over strict successful artifacts, guarded by
  conservative over-invalidation and clean-vs-incremental equivalence. A cache
  hit is not a proof and cannot upgrade an evidence class. Phase 14's independent
  checker first makes canonical Core artifacts and their transport/cache
  untrusted for the named `CoreCertificateV1` predicate; Phase 15 extends that
  statement only through the supported BackendIR translation slice. Parser,
  resolver, source-to-Core correspondence outside proved rules, LLVM, linking,
  runtime, OS, and hardware remain explicitly trusted until separately checked.
- **Pattern ergonomics and daily language friction** live in Phase 6 #5 and #7:
  match guards, OR patterns, `if let` / `while let`, match-on-reference,
  the no-tuples decision, struct update, wildcard destructuring, and `defer` /
  cleanup semantics. Anonymous tuples remain a non-goal unless named structs
  fail a real workload.
- **Import/dependency authority constraints** live in Phase 18 #8: imports do
  not grant capability by themselves, and dependencies can be constrained by
  capability budget, trusted/Unsafe use, assumptions, evidence floor, platform,
  source hash, license, and supply-chain identity.
- **SPARK-class assurance** is split by owner phase, not tracked as a side
  project: proof/agent guidance in Phase 9 #5a, flow/frame/dependency contracts
  in Phase 12 #19, runtime-safety use of loop/frame facts in Phase 13, release
  assurance bundles in Phase 17, package evidence summaries in Phase 18 #15a,
  and editor/agent diagnostics in Phase 19 #13.
- **Proof-cost reduction** is Phase 9 work: proof minimization/stubs/diagnostics
  are #2–#15, structural auto-discharge is #16, and operational VC
  auto-discharge is #16a. The operational tier is the key path for reducing
  hand-written Lean bridge theorems on ordinary postconditions and loop steps.
- **AI/agent learnability** must be updated with the feature that creates the
  proof surface: `docs/SPARK_CLASS_ASSURANCE.md` is the stable agent guide, and
  Phase 9 #5a requires an agent-facing example whenever a new assurance
  annotation lands.
- **Package evidence summaries** are Phase 18 #15a, after import constraints
  have real facts to summarize.
- **Semantic-darkness audits** are Phase 14 #13a. The checked-arithmetic flip
  showed the recurring failure mode: one source construct changes meaning, but
  an older assumption survives in SSA cleanup, ProofCore, reports, interpreter
  behavior, or stdlib examples. Future language/stdlib changes must ask whether
  source spelling hides width, profile, target, allocation, authority, runtime,
  or proof-model behavior, and must add a red-team fixture when it does.

Completed work moves to [CHANGELOG.md](CHANGELOG.md). Deferred or conditional
work moves later in the same linear queue. There are no parallel tracks. Inline
`NEXT` notes are allowed only as scoped follow-ups inside a numbered item; they
do not create a second queue.

Linear does not mean invisible. When a long internal phase risks delaying
external credibility, add one narrow replayable vertical artifact inside the
current linear queue. It must not become a side track or product fork; it exists
to validate the bet, expose abstraction mistakes early, and give skeptics
something real to run. The compiler-pipeline transcript
(`examples/compiler_pipeline_probe/`) is part of the closed-Phase-4 deferred tail
(now tracked across Phases 10/14/19); Phase 8 carries the later
`examples/credibility_slice/packet_window/` replayable flagship.

The next four verdict-deciding risks get early, narrow probes rather than
waiting for their full later phases. Each probe must name a minimal scope, an
early trigger, and the later phase that owns the full version:

1. **External credibility:** near-term probe = Phase 6B `diff-caps`, a
   deterministic runnable answer to one skeptical-review question: "what new
   authority does this change grant?" Minimal scope: capability manifest +
   `diff-caps --machine` over one authority-widening change, one no-op refactor,
   and one dependency/callee widening case. Trigger: as soon as
   `CapabilityJudgment` can emit the stable manifest and the determinism gate can
   keep the machine output byte-stable. Full home: Phase 8's non-author
   workload/trial, and later Phase 18 package evidence. Do not wait for the full
   external trial before shipping this narrow artifact.
2. **Proof cost:** near-term probe = Phase 9 proof-effort telemetry on the first
   real flagship/proof slice. Minimal scope: per proved function, record source
   complexity, proof lines, tactic depth, solver time, replay time, and manual
   review notes for one concrete example. Trigger: the first flagship or
   credibility slice that makes a public proof/evidence claim. Full home: Phase
   9 proof authoring/automation, with the LLM-guided synthesis/review loop
   measured next if manual proof authoring is too expensive.
3. **Backend soundness:** near-term probe = a narrow Phase 15 translation
   validation slice, not the full backend phase. Minimal scope:
   `ValidatedSSA -> BackendIR -> ValidatedBackendIR` over integer arithmetic,
   structs/fixed arrays, direct calls, branches, bounded loops, runtime checks,
   and source-map annotations. Trigger: once Phase 6B has stabilized SSA/backend
   facts enough for those constructs. Full home: Phase 15 backend contracts and
   translation validation. The probe exists to learn early whether Concrete's
   source/Core evidence can cross the backend boundary; anything outside the
   slice remains explicitly backend-trusted.
4. **Iterative pipeline scale:** near-term probe = Phase 6C's shadow query and
   invalidation transcript over one Phase 8 workload. Minimal scope: unchanged
   rebuild, comment/span edit, private-body edit, public-interface edit,
   contract/spec edit, policy edit, and target/profile edit; show which
   file/module/function/obligation/codegen facts would be reused or invalidated,
   but do not reuse them yet. Trigger: stable fact IDs, deterministic canonical
   summaries, and the Phase 6B dependency/invalidation contracts. Full home:
   Phase 8.5's persistent incremental artifact driver, and only after the
   external-validation gate says the project is worth scaling.

Every phase must leave behind checked evidence that it works. A phase item is
not complete because code exists; it is complete when the behavior has positive
fixtures, negative fixtures, regression fixtures, and at least one adversarial
or red-team fixture for the failure mode that would be most damaging. Gates
must be named in the item text, wired into CI or the phase validation artifact,
and must prove both success and non-success: no false green, no stale cache, no
silent trust upgrade, no side-channel recomputation, no hidden artifact, and no
user-facing claim without a replay command or checked report. If a task cannot
yet have a red-team gate, the roadmap item must say why and name the first gate
that will make it testable.

The biggest multiplier is not another roadmap item; it is applying this gate
discipline uniformly. Every drift-prevention gate should be mutation-tested
where practical, every unsupported case must fail closed and loud, every
evidence claim should carry a replay command, and every completed semantic
item should name the gate or proof that protects it. A feature that passes only
because no one tried to break its gate is not done. The largest remaining trust
boundary is the backend, so the early narrow translation-validation probe in
Phase 15 is the backend-side version of the same rule: do not let source/Core
evidence become native-code evidence unless the boundary is validated or
explicitly marked trusted.

North star: **systems code with explicit authority, bounded behavior, small
trusted boundaries, and Lean-backed evidence tied to real source code, while
keeping compiler, backend, toolchain, runtime, and target assumptions honest.**

Known holes index: every tracked soundness / dark-construct gap — what it is,
whether it is open or closed, the gate that locks it, and the item here that
fixes it — is consolidated in [docs/KNOWN_HOLES.md](docs/KNOWN_HOLES.md). Keep
it in sync when a hole is added or fixed. **No known holes are currently open.**
H1/H2/H6-H17 and the C-series are closed and gated; the open section of
`docs/KNOWN_HOLES.md` is empty. The broad linearity/value-flow model is enforced
and guarded by `docs/VALUE_FLOW_SPEC.md`, `check_linear_discard.sh`,
`check_linear_conservation.sh`, `check_linear_nested_scope.sh`, the linearity
fuzzer, and pass-agreement gates. Remaining linearity work in this roadmap is
hardening/refactor work, not open soundness debt. The known expressiveness gap is
fail-closed: owned arrays of linear elements have no whole-owner move-out until
array destructure is pulled by a workload (Phase 6 #13p). H12 is closed: user
submodules and the entire `std` subtree are now checked by the full front-end,
the exemption machinery is deleted, and `check_submodule_check_coverage.sh`
pins the all-source rule at zero. Overflow, div/mod-zero, over-width shift,
`MIN` negation, the float→int cast (H2), and array bounds (H8) all abort by
default at runtime (ROADMAP #10 Stage 2.x + H8); `let _ = expr;` is removed, and
`_` may ignore only `Copy` values.

Governing frame: **no semantically dark constructs.** Every language
construct is `proved`, `enforced`, `reported`, `assumed`, or `trusted` —
never a vague middle. "Provable language" is not "everything in
`ProvableV1`"; it is "every construct has a defined proof story across the
five claim classes." `ProvableV1` is the current `proved`-via-value-semantics
cell. Other cells are populated by capability typing, runtime obligations,
assumption files, and declared trust boundaries.

Design bias: **explicitness over cleverness.** Authority, allocation, trust,
runtime failure, byte/text/path boundaries, and proof class must remain visible
in source or audit output. Do not hide them behind inference-heavy abstractions,
implicit conversions, ambient lookup, or broad metaprogramming.

## Cross-cutting decisions and checkpoints (recorded 2026-06-03)

These are decisions/checkpoints, not yet implementation. Each names the
evidence class so no construct stays semantically dark.

### Floats: usable now, provable only under an explicit profile

**Decision.** `f32`/`f64` are allowed as runtime types but are **outside
ProvableV1** unless a function opts into a future float profile. Unprofiled
float arithmetic is excluded from proof eligibility and audit-loud; no proof or
release claim may be made over float arithmetic until a profile exists.

**Provable Float V1 (future, narrow profile — see Phase 12).** A function opts
into a named profile, e.g. `#[float_profile(ieee754_binary64_nearest_even)]`:
- IEEE-754 binary32/binary64; round-to-nearest-even only at first.
- No fast-math; no reassociation; no FMA contraction unless the source names
  `fma`; no ambient rounding-mode mutation.
- Explicit NaN / infinity / subnormal / signed-zero policy.
- **Bit-level semantics first**: model floats as `BitVec 32/64`; add
  `PVal.float32/64` and float `PBinOp` cases (`fadd`/`fsub`/`fmul`/`fdiv`/`feq`/
  `flt`/`fle`) carrying width + rounding; interpreter agreement.
- **Honest trust label.** Lean's `Float` is opaque/native, not bit-level
  reasoning. The primitive IEEE ops start as a **trusted axiom layer**
  (`float_semantics_trusted`) or an imported checked library (Flocq-style)
  until derived. So v1 = "extracted body refines an explicit IEEE-754 bit-spec
  whose primitive ops are named trusted" — not `proved_by_lean` from first
  principles. Backend must prove/report `fast_math: forbidden`,
  `rounding: nearest_even`.
- First flagship: a small deterministic numeric kernel (clamp/normalize, a
  fixed-order dot product, a tiny IIR/FIR or PID), proved exact-IEEE — not
  scientific computing. Real-number ε-bound refinement is a later layer.

### Embedded hardware access: evidence classes (see Phase 16)

Name the evidence class before implementing freestanding/embedded targets:
- **inline asm** — `trusted`, requires `with(Unsafe)`.
- **volatile / MMIO** — explicit capability, e.g. `with(Device)` / `with(Mmio)`;
  reads/writes are audit-visible effects, never silently elided.
  `with(Device)` is reserved as the capability-vocabulary addition for real
  freestanding/MMIO code (registered cap, NOT in `Std`, opt-in/audit-visible
  like `Unsafe`), but it is **not added before that consumer exists** — no
  speculative vocabulary growth (decided 2026-06-13; see
  `research/language/capability-sandboxing.md` §4a). `Thread` is reserved for the
  concurrency model; `Signal` is deferred; `Random` is clarified as authority
  over external entropy.
- **interrupt handlers** — a separate trusted/effectful boundary.

### Native debug info (see Phase 15)

Once Concrete emits real binaries, source-mapped DWARF / crash traces matter for
auditability. Tooling/backend item, not first-release proof-critical.

### Target-conditional code without hidden metaprogramming

Concrete needs target-specific code for hosted/freestanding builds, OS-specific
FFI, and stdlib splitting, but broad in-source conditional compilation would
pull the language toward macro-like configuration. Prefer module/file selection
through `Concrete.toml` target profiles. Add only narrow, audit-visible
`cfg`-style attributes if file/module selection proves insufficient.

Audit output must explain target selection:
- selected source roots/modules,
- excluded source roots/modules,
- target profile and platform reason,
- enabled build profile,
- and any `cfg` attributes if they are ever admitted.

### Contract-VC stability tiers (dependency edge into Phases 5-6)

The risk this names: Phase 8 flagships are what exercise contracts, and they
will keep hitting an un-frozen Phase 5/6 surface — so any VC/contract IR designed
in Phases 1-2 against that surface gets reworked when collections and the
iteration protocol land. The fix is **not** to reorder (that is circular: the
flagships are what stress-test Phases 5-6) but to tag each contract/VC construct by
stability tier and refuse to freeze syntax over the provisional tier. This is
the same discipline as "let the proof teach the syntax" (Phase 1 preamble),
made into an explicit dependency.

- **Frozen-safe** — obligations over integers, booleans, `BitVec`, and
  fixed-size arrays. This is the slab every shipped proof already stands on
  (HMAC, `ct_compare`, the loop VC). Contract/VC syntax here may be stabilized.
- **Provisional** — any obligation quantifying over collections, iterators,
  strings/text, bytes, paths, or capability-polymorphic callees. These depend
  on the **Phase 5 core slab** (modules/imports, minimal project model,
  `concrete test`, core diagnostics, bytes/text/path, and collections). They
  also remain provisional over later iteration and capability-polymorphism
  work. Do not freeze contract syntax or VC shape for those constructs until
  the relevant Phase 5 or Phase 6 item has landed. Treat any such construct as
  "will be reworked," and do not let a flagship bake an iterator/collection
  assumption into the VC shape.

### Spec and contract trust (no dark specs)

Concrete proves source against specs, but that is not enough by itself. The
spec/contract layer must also have evidence classes, or the system can produce
a beautiful proof of the wrong theorem. Apply the same rule one layer higher:
no semantically dark specs, no hidden vacuity, no partial ghost/spec functions,
and no pretending a monomorphic proof covers every future generic
instantiation.

- Specs need provenance and review status: external standard, independent
  reference, test-vector set, reviewer, remaining assumptions, and trust class.
- Contracts need satisfiability/vacuity checks: an impossible precondition or
  unreachable path must not show up as an ordinary proof.
- `spec fn` and ghost computations used in contracts must be pure, erased, and
  total, with a named obligation when totality is not inherited from Lean.
- Generic and capability-polymorphic proofs must state their scope:
  per-instantiation, generic-once, or generic contract with instance-level proof
  artifacts.
- Proof evidence is relative to the toolchain that checked it. Lean,
  `bv_decide`, `omega`, ProofKit, extraction, and compiler-version changes need
  drift detection and recheck status, not silent reuse of old green badges.
- References and borrows need an explicit proof class: value-only/borrow-free,
  read-only-reference proof, mutable-reference proof with frame obligations, or
  enforced-only and outside `ProvableV1`.

### External-validation gate (go / no-go before the back half of Phases 11-19)

This is a **gate, not a note** — promoted out of Phase 17 because validation
that sits downstream of the build-out it is meant to justify is no validation
at all. The central bet of the whole project is "evidence-carrying source is
worth the discipline." Today the only person who has found it worth the cost is
the person who built it, yet Phases 11-19 (packages, editor, freestanding,
release) are a large investment fully predicated on that bet. The research under
`research/` and `thesis-validation/` tests the thesis but is currently orphaned
from the execution plan; this gate wires it in.

It cannot be "before any Phase 5" — there is a chicken-and-egg floor: an outside
user needs *some* slab to write anything real. So the gate is:

1. **The minimum slab is the Phase 5 core slab**: modules/imports, minimal
   project model, `concrete test`, core diagnostics, bytes/text/path, and
   collections.
2. **Build exactly that** — not the full back half. Both pre-trial
   prerequisites are now complete: **conditional Copy for generic
   instantiations** ✅ DONE 2026-07-05 (`Option<T>`/`Result<T,E>`/Copy-marked
   generics are Copy iff all substituted payloads are Copy; `if let` over
   `Option<i32>` is legal for the right reason), and the **H12 std migration**
   (Phase 7 #38b): the trial evaluates enforcement claims against a stdlib
   that is checked like user code.
3. **Run the trial and treat the result as an explicit go / no-go on Phase 8.5
   and the rest of Phases 11-19.** The persistent incremental driver is a large
   investment in scaling the bet; its Phase 6B/6C prerequisites and shadow
   transcript may land before the verdict, but cache activation does not.

The trial should be implemented as the first external-user workload in the
Phase 8 real-workload ladder, after the Phase 5 core slab and Phase 7
stdlib/core-API slab exist, not as a separate validation artifact.

**Pass criterion:** at least one person who is **not** the compiler author
writes, proves, or contract-annotates a useful Concrete program and reports that
the proof discipline (ProofKit + contracts + `concrete prove`) was worth the
cost. Until this passes, the back half of Phases 11-19 is flagged **at-risk**,
not green.

**Scope of the gate.** Phases 8, 9, and 10 (flagships, proof authoring, audit
commands) are NOT gated — they proceed regardless, because they are what the
trial runs on and what a negative result would teach against. Phase 8.5 is
explicitly gated: do not turn the shadow graph into a persistent cache/build
engine unless the trial returns GO. A **fail** verdict does not silently park
Phase 8.5 or Phases 11-19 either: it forces an explicit decision recorded in
this file — change the bet (redesign the discipline that failed), narrow the
audience, or stop — before Phase 8.5 or any Phase 11+ item may start.

If the trial fails because human proof authoring is too expensive, the next
experiment is not "verification failed as a product idea." Run the same
workload through the narrow Phase 9 LLM-guided proof-synthesis **probe**, pulled
forward into this trial, and measure review cost instead of authoring cost; do
not wait for or start the whole Phase 9 queue merely to settle the Phase 8.5
trigger. The non-author reviews the spec, assumptions, and replayed evidence
while the tool/agent searches for invariants, lemmas, and proof scripts under
kernel verification. Record the final GO/NO (or an explicit decision to reject/
defer Phase 8.5) before the Phase 8 slot closes. The gate result should
distinguish "the discipline is not worth it even with synthesis" from "manual
proof authoring is not worth it."

### Stdlib runway checkpoint (recorded 2026-07-07)

The stdlib survey changes the next ordering without changing the language
philosophy. Concrete should not chase a Go/Odin-sized standard library before a
real program exists. The next useful shape is: **#35 validation project first,
then a small coherent stdlib floor, then allocator/proof/tooling hardening
based on what that program actually forced.**

Comparison baseline:
- **Gleam** is the nearest size model for v1 coherence: small, consistent,
  pleasant `Option`/`Result`/collection/text APIs.
- **Zig** is the allocator/authority lesson: explicit allocator values matter
  for arenas, tests, hot reload, plugins, embedded pools, and API stability.
- **Rust** is the layering lesson: keep core/no-alloc, alloc-backed, and hosted
  APIs distinct; collections/errors/IO are the daily-programming floor.
- **Ada/SPARK** is the assurance lesson: containers, strings, IO, big/spec
  numbers, formal containers, and lemma libraries need explicit proof/evidence
  classes.
- **Go/Odin** are breadth references, not the near-term size target: compression,
  full crypto, broad networking, threads, atomics, archive formats, and vendor
  wrappers stay deferred until workload evidence pulls them.
- **Lean** informs proof infrastructure, not ordinary app-stdlib breadth.

Therefore Phase 7 is ranked by app-unblocking value, not by module count:
error ergonomics; allocator-as-value research before allocator-heavy API
hardening; buffered IO/reader/writer; path/FS; string/text/bytes coherence;
collections phase 2; tests-as-docs; CLI/env/process; trap/debug UX; verified
profile/proof UX. Broad crypto/compression/networking remains later unless #35
forces it.

## Phase 6: Language Usability And Daily Workflow

Goal: make Concrete usable as a normal experimental language, independent of
whether a user is writing proofs.

Done when: a new user can format, build, run, test, diagnose, inspect, and debug
small Concrete programs with predictable commands and useful errors.

This phase starts only after the Phase 5 core slab is sufficiently stable. It
keeps grammar/control-flow ergonomics, daily commands, examples, docs, linting,
profiling, and editor-adjacent usability separate from the smaller core-slab
gate.

Fully completed Phase 6 work has been moved to [CHANGELOG.md](CHANGELOG.md).
The active roadmap below contains only deferred, conditional, or not-yet-done
Phase 6 work.

The ownership hardening ratchets below are not separate from Phase 6B's
`OwnershipJudgment` / `ValueFlowJudgment`. Treat 13h, 13i, and 13k as the
practical checker-facing prerequisite slices: first make ownership transitions
go through narrow APIs, then make ownership transfer/overwrite and control-flow
exit modes explicit data, then promote the result into the Phase 6B judgment and
agreement gate. Do not build a large ownership fact database before these local
refactors make the current checker decisions auditable.

13h. **Deferred hardening ratchet — centralize ownership-transfer and overwrite
   policy helpers.** This is valuable checker cleanup, but it is not an open
   soundness blocker after 13c-13g; do it when touching the checker next or
   after the #18/#35 validation frontier. Delete
   scattered per-AST `if ident then consumeVarIfExists` logic in favor of one
   helper for "this expression transfers ownership" and one helper for "this
   place may be overwritten." Assignment, return, `break value`, function
   arguments, method receivers, stores, array/struct literal elements,
   destructuring, `?`, and `defer` must all route through those helpers or
   through the mode-based `checkExpr` entry point. The overwrite helper owns
   the E0219/E0291 family: rebind after consuming old value is legal; live
   non-`Copy` field/index/`&mut` pointee overwrite rejects; trusted/raw-pointer
   uninitialized-slot idioms must be explicit trusted boundaries, not silent
   safe-code exceptions.

13i. **Deferred hardening ratchet — represent control-flow exit modes as data.**
   Branches, match arms,
   loops, `return`, `break value`, `continue`, `abort`, and future `noreturn`
   calls should produce a structured exit mode:
   `fallsThrough | returns | breaks(value?) | continues | diverges`. Linear
   merge rules should consume this object instead of re-deriving reachability
   and ownership state ad hoc in each checker path. This is the follow-through
   from H9/H14/H17 and the H12 std burn-down: diverging arms do not merge,
   return paths must discharge owned locals before leaving their scope, and
   break-with-value consumes the value being transported.

13j. **Deferred hardening ratchet — add an interpreter runtime conservation
   oracle when the interpreter has
   heap support.** Deferred pull-condition: the interpreter currently has no
   heap model (`allocCall` remains in the differential gate's unsupported
   class), so there is no allocation/free balance to observe. When
   interp gains alloc/Heap support, tag every non-`Copy` runtime value with an
   ID, record move/consume/free events, and report copied, leaked, or
   double-consumed IDs at exit behind `--interp-strict`. Wire it into
   `fuzz_differential.py`, `fuzz_linearity.py`, and corpus/nightly runs as the
   dynamic backstop for checker mistakes. Until then, 13c + 13f + the static
   gates are the conservation net.

13k. **Deferred hardening ratchet — stop direct checker environment mutation
   outside ownership APIs.**
   The checker should expose narrow state-transition helpers such as
   `declareVar`, `moveVar`, `copyVar`, `borrowPlace`, `markConsumed`,
   `reserveForDefer`, `restoreScope`, and `mergeBranchStates`; direct mutation
   of `env.vars`, consumed states, borrow/ref lists, or branch snapshots should
   disappear outside those helpers. This is the API-level version of the
   mode-checker refactor: make illegal states hard to construct, make every
   ownership transition auditable in one place, and keep future bug hunts from
   finding another isolated handler that updates the environment differently
   from the rest.

13l. **Deferred hardening ratchet — create one shared type-predicate module for
   Copy, trait bounds, and
   substitution-sensitive facts.** The conditional-Copy and H17 burn-downs
   exposed repeated logic across Check, Elab, CoreCheck, Mono, and post-mono
   verification: instantiated `Copy`, builtin `Option`/`Result` rules,
   type-parameter substitution, caller type params passed through turbofish,
   and trait-bound satisfaction must not be reimplemented per pass. Move these
   predicates into one shared module and require every pass to call it. A new
   type/classification rule lands only with shared positives/negatives proving
   Check, Elab, CoreCheck, Mono, and Verify agree.

13m. **Deferred hardening ratchet — add diagnostic-code coverage gates for
   ownership/type diagnostics.**
   Every `CheckError` / `CoreCheckError` code in the ownership, Copy,
   capability, and value-flow families must have a negative fixture, a
   stable-code assertion, and a hint assertion when the diagnostic has a hint.
   This gate should cover new codes such as E0290/E0291/E0292 and existing
   high-value codes E0205/E0208/E0219/E0220/E0286/E0287/E0288/E0520. The goal
   is not only "the program rejects"; the user-facing reason must be stable and
   useful, and `--report diagnostic-codes` must stay complete.

13n. **Deferred hardening ratchet — systematize pass-agreement gates.** Strengthen LANGUAGE_INVARIANTS #19
   into a reusable gate family: any program accepted by Check/CoreCheck must
   not die later with an internal E07xx, panic, SSA-verify error, lowering
   mismatch, or interpreter/compiler disagreement unless the feature is
   explicitly classified as unsupported. Reuse `fuzz_differential.py`,
   `fuzz_linearity.py`, codegen-differential vectors, and small hand-written
   fixtures. Each new semantic rule should answer: do Check, Elab, CoreCheck,
   Mono, Lower, SSAVerify, interpreter, and reports agree on the same meaning?

13o. **Add explicit `noreturn` / diverging-function facts.** Today the checker
   knows builtins like `abort()` diverge, but ordinary APIs such as
   `libc_exit` cannot communicate "this never returns" without source-level
   workarounds. Add a small explicit surface (`-> never`, `#[noreturn]`, or an
   equivalent trusted declaration fact) that feeds the structured exit-mode
   object from 13i. This is not inference magic: the divergence fact must be
   visible in source/signature/report output, and wrong trusted declarations
   remain trusted-boundary responsibility.

13p. **Add array destructure / linear array move-out only when a workload pulls
   it.** H11 correctly made `[linear; N]` fail closed for by-value element reads:
   without partial moves or array patterns, an owned array of linear elements
   has no way to move every element out. Do not re-open element projection to
   make this convenient. When a real workload needs it, design explicit
   whole-owner array destructuring (`let [a, b] = arr;` or a fixed-size
   equivalent) and gate that every element moves exactly once while partial
   element move-out remains rejected.

13q. **Deferred hardening ratchet — add checker mutation tests proving the
   linearity gates are load-bearing.**
   Mutate or patch-disable key checker rules in CI-local/nightly mode and prove
   the corresponding gates fail: assignment RHS consume, `break value` consume,
   live-linear shadowing rejection, non-`Copy` overwrite rejection, H11
   non-`Copy` sub-place read rejection, param consume obligations, conditional
   Copy demotion, must-use fallibility before Copy, and branch/arm merge
   agreement. This is separate from proof/evidence mutation testing: it proves
   the checker and linearity gates would catch the exact bug class if it came
   back.

13r. **Doc-snippet compile gate (from the Zig-at-100k-lines lessons,
   2026-07-07).** Every ```con code block in README/site/docs/*.md must either
   COMPILE (or compile-and-check for reject examples marked as such) or carry
   an explicit `pseudocode`/`illustrative` marker. Zig's real-world pain was
   stale online examples drifting from an evolving language; Concrete avoids it
   early by making doc snippets first-class evidence — a gate extracts fenced
   `con` blocks and runs each through the compiler, failing on an unmarked
   block that no longer compiles. Cheap, high-leverage, and directly serves
   the no-dark-constructs / honest-docs discipline.

   Deliverable: `scripts/tests/check_doc_snippets.sh` plus a small fenced-block
   convention:
   - runnable examples use a plain `con` fence;
   - expected rejects use a `con reject:E02xx` fence;
   - illustrative-only snippets use a `con pseudocode` fence;
   - evidence expectation: a nearby `<!-- concrete-evidence: ... -->` block
     naming expected diagnostics, capability facts, proof status, or audit
     facts.

   Done when the gate fails on an unmarked broken snippet, fails when an
   evidence expectation drifts, and passes on one positive, one expected-reject,
   and one explicitly-pseudocode example. Slice-friendly: start with docs/ that
   already contain runnable snippets. HIGHEST-PRIORITY of these four.

13s. **Allocator-as-value research — BEFORE Phase 7 collection APIs harden
   (2026-07-07).** `with(Alloc)` is the AUTHORITY ("may allocate"); it does not
   say WHICH allocator owns the memory. Arenas, tests, embedded, hot-reload,
   plugins, and the #35 validation project all want allocator IDENTITY, not
   just permission (the Zig hot-reload story is the sharpest argument: memory
   must outlive a reloaded library, which needs a named allocator the caller
   controls). Research an explicit allocator value threaded alongside the
   capability — e.g. `fn parse(input: &Text, alloc: Allocator) with(Alloc) ->
   Result<Ast, ParseError>` — under the linear/H17 rules (an `Allocator` is a
   borrow or a Copy handle, never a hidden global). TIMING IS THE POINT:
   retrofitting an allocator parameter into already-shipped collection
   signatures is an H12/H17-scale burn-down, so decide the model before Phase 7
   #3+ harden the APIs.

   Deliverable: `research/stdlib/allocator-as-value.md` with a GO/NO-GO
   decision, example signatures for `Vec`, `String`, parsers, arena allocation,
   test allocators, and hot-reload-owned memory, plus the report fields needed
   to show both `with(Alloc)` authority and allocator identity. Gate is a
   design-review fixture, not implementation: examples must show that the
   allocator value is explicit, linear/borrow-safe, and never a hidden global.

13t. **Recoverable-vs-fatal error convention doc (2026-07-07).** Write the
   normative split so stdlib `Result` usage stays consistent and does not drift
   into Zig-style everything-bubbles noise: `Result` for DOMAIN / user /
   environment failures (parse error, file-not-found, connection refused);
   `abort`/trap for INVARIANT failures, OOM, bounds, and arithmetic traps
   (already the shipped stance — checked arithmetic, array bounds, E0286
   forcing acknowledgment); and the audit/report surface must NAME which
   failures a function treats as recoverable vs fatal. Mostly codifies existing
   behavior, but writing it down is what prevents API-by-API drift.

   Deliverable: `docs/ERROR_CONVENTIONS.md` plus stdlib examples for parse
   failure, file-not-found, OOM/trap, bounds/trap, and ignored `Result`. Done
   when at least one audit/report fixture shows a public API classified as
   recoverable, one as fatal/trapping, and one as policy-gated. Feeds the Phase
   7 std ergonomics work and the #35 error surfaces.

13u. **Trap / debug runtime UX — shaped by #35 (2026-07-07).** Runtime traps
   (bounds, overflow, div-by-zero, OOM, checked-cast) currently abort without a
   source span. Make them useful: a source span on the trap where the emitter
   can carry one, a `concrete run --trace` mode, stable runtime-failure reports,
   and debug builds tuned for inspectability. This is real usability work best
   scoped AFTER #35 tells us which traps actually hurt in a real workload — file
   now, prioritize by evidence.

   Deliverable: a runtime-failure record with trap kind, source span when
   available, function, operation, target profile, and replay command.
   Acceptance gate: one bounds trap, one arithmetic trap, one OOM/fatal fixture
   if available, and one trace-mode run must produce stable human and JSON
   output. Lowest-urgency of these four; pull it forward only if the validation
   project makes trap opacity painful.

13v. **Build-output convention (repo hygiene, found by #35, 2026-07-07).**
   `concrete build` emits an extensionless binary into the project root named
   after the package, which globs cannot cleanly ignore — so `.gitignore`
   hand-enumerates ~42 per-example binary paths and every new project example
   needs another line (conlog just hit this). Give `concrete build` a
   conventional output directory (e.g. `target/` or a `build/`-style dir,
   already covered by the existing `build/` ignore) so one ignore rule
   suffices and built binaries never sit in source dirs. Small tooling change;
   also lets a `make clean` sweep the stray `out/`, `*.con.ll`, and example
   binaries currently littering the tree. NOT a folder reorg — the repo layout
   is otherwise healthy and path-coupled to manifests/gates, so leave it.
   Done when a new example builds without adding a `.gitignore` entry, `make
   clean` removes generated binaries/IR, and existing run/test scripts find the
   new output path through one helper rather than hard-coded source-dir binaries.

2a. Add qualified name access and import aliases for module hygiene. Phase 5
   closed the core modules/imports/visibility surface, but daily use still has
   a namespace gap: if two imported modules export the same public name, the
   caller must manually rename one because calls resolve through unqualified
   names. Design the v1 surface for qualified paths (choose `mod::name`,
   `mod.name`, or another syntax that does not conflict with method calls or
   enum variants), import aliases (`import long.module as m` or equivalent),
   and diagnostics for ambiguous unqualified names. Add
   `examples/module_qualification/{ambiguous_exports,qualified_call,import_alias}/`
   and `scripts/tests/check_qualified_names.sh`; the gate must prove private
   names remain inaccessible, qualified public names resolve without local
   renaming, import aliases do not grant visibility, and ambiguous unqualified
   imports produce a clear diagnostic. This item is name hygiene, not a package
   security boundary, but the syntax must compose with Phase 18 import fact
   constraints such as `import dep.mod requires(no Network, no Unsafe,
   proved_by_lean)`: aliases never grant authority, and a dependency capability
   or evidence widening must remain a build/audit fact rather than being hidden
   by the alias.
8. Define the FFI language surface: `extern` syntax, layout restrictions,
    ABI/calling convention annotations, ownership crossing the boundary,
    capability/trust requirements, and what cannot be expressed safely.
9. [DECIDED — 2026-06-27; docs/FFI_GLUE.md, impl deferred] C/ABI glue-generation
    UX: the eventual shape is a narrow `concrete ffi`/`bindgen` path emitting/
    importing AUDITABLE glue that carries ABI/layout assumptions, ownership
    boundary rules, capability/trust labels, source spans, and reproducible
    output. Blocked on the FFI language surface (#8) — not built before #8 is
    settled; implemented against it then, with an `examples/ffi_glue/` + gate.
12. Define handle-relative filesystem APIs as the preferred capability shape:
    directory/file handles are capabilities; privileged code should operate
    relative to opened handles rather than repeated ambient path lookup. The
    design must address TOCTOU risks, path normalization, symlinks, temp files,
    and byte-preserving OS boundary behavior.
14. Track accumulating error sets for `Result`-heavy code, without adopting row
    effects. V1 is report-only: `concrete audit --report error-sets` and
    `concrete inspect --error-sets --json` over existing enum-returning
    functions. Add `examples/error_sets/protocol_pipeline/` with
    `ParseErr`, `IoErr`, and `AuthErr`, plus
    `scripts/tests/check_error_sets.sh`. The gate must prove call composition
    accumulates variants, dead variants are not reported, ignored-result
    diagnostics still fire, and no syntax resembling row polymorphism is
    accepted.
15. [DECIDED — 2026-06-27; docs/UNITS_OF_MEASURE.md] Units-of-measure /
    dimensional annotations: evaluated → **deferred, no units in v1.** It is a
    type-system feature with proof/report implications (unit erasure must be an
    audit fact, not silent), not a localized add; cost/benefit does not favor it
    ahead of the stdlib/tooling work. Today's discipline carries most of the
    benefit at zero cost: encode units in names (`len_bytes`, `timeout_ms`) per
    STYLE.md, and checked arithmetic removes the silent-corruption failure mode.
    Kept research/workload-gated: if a real workload keeps hitting unit bugs, the
    first step is a report-only prototype (`examples/units_probe/` + gate),
    annotations optional and erased only after audit records the conversion.
18. ✅ **CORE DONE 2026-07-06 — callable values without closures.**
    Shipped (commits 3f25e185 + 8588c8e9, CI green): E0293
    (`conflictingCallBorrows`) is the container-not-in-context hard rule —
    path-based overlap within one call, auto-borrowed receiver + single-hop
    `borrowedFrom` aliases included; `with_value_mut`/`modify` on HashMap and
    OrderedMap, `Vec::with_at_mut`; `for_each_with` (shared `&Ctx`) and
    `for_each_ctx` (mutable `&mut Ctx`, reborrowed per call) on Vec + HashMap.
    H1 tail CLOSED — no ref-returning accessor anywhere in std; the model is
    scoped-read + scoped-in-place-mutation + move-out. The three-mode
    discipline held exactly as designed: `&Ctx` many-call read-only, `&mut Ctx`
    many-call reborrow (H17 makes it an ordinary borrow — no consume
    obligation, no move-out), consuming one-shot. Gated by
    `check_callable_values.sh` (21 rows).
    DEFERRED TAIL (not blockers for #35): consuming one-shot combinators + more
    collection HOFs (workload-pulled), `with(f)` capability-ELISION (§6 — admit
    only if signature noise becomes real friction), `proved_for_instance`
    EVIDENCE artifacts (§8 — fits proof-automation, not blocking), first-class
    stored `BoundFn` (only if a storage workload needs it). `from(param)`/view
    structs remain deep research escape valves.
    Original landing plan (for history):
    - Finish or revert any partial E0293 / same-call-borrow checker work before
      adding APIs. A half-landed aliasing policy is worse than no policy.
    - Add explicit context-threading combinators for `&Ctx` and `&mut Ctx`
      callbacks over the collection surfaces that need them; consuming
      one-shot contexts land only if a workload needs them.
      Linear rule: borrowed contexts are not moved into a hidden environment.
      `&Ctx` is many-call read-only, `&mut Ctx` is many-call exclusive
      reborrow-per-invocation, and consuming `Ctx` is one-shot unless `Ctx` is
      `Copy`. A callback over `&mut Ctx` may mutate through the context but may
      not move a linear field out of the context unless the API is explicitly
      one-shot/consuming.
    - Gate callable capture/conservation: callback function pointers capture
      nothing; every context is an ordinary parameter whose ownership is checked
      by the existing value-flow machinery; callback capsets remain in the
      `fn(...) with(C) -> R` type and must be required at each call site.
    - Add the scoped collection callback gate: `with_value_mut` / `modify`
      only after a container-not-in-context check proves the mutable
      receiver/context aliasing invariant, including the method receiver and
      all callback/context arguments.
    - Add first-class stored `BoundFn` values only after a storage workload
      needs them.
    Keep the roadmap focused on this unfinished tail. `with(f)` capset-elision
    is admitted only if signature noise becomes real friction. `from(param)` and
    view structs are **deep research escape valves**, not planned language work:
    they come forward only if operation APIs, owned views, and scoped callbacks
    fail a real workload badly enough to justify returned-reference provenance.
    Negative ergonomics decision: **do not add local no-capture functions**.
    They add locality but no expressive power, and nested functions create a
    capture-by-expectation footgun because they look like they should see
    enclosing locals. Use top-level helpers, explicit context structs, and
    ordinary function pointers instead. Reopen only with a concrete workload and
    a separate design note proving the capture/type-parameter/mangling story is
    worth the added surface.
20. Design user-facing testing framework UX before `std.test` hardens:
    test discovery (`#[test]` versus naming convention), expected failures,
    capability-scoped fixtures, temp files without ambient authority, oracle
    tests, interpreter-vs-compiled tests, proof-status interaction, and how test
    failures appear in `concrete audit`. The user-facing output must include
    `concrete test --json` with a stable event stream comparable to Go's
    `test2json`: discovered, started, passed, failed, skipped, expected-failed,
    oracle-compared, policy-blocked, and proof-status events.
    - 26a. The testing-framework UX must leave room for contract/property
      fuzzing — the actual implementation is Phase 10 #11 (`concrete test
      --contracts --property`, evidence class `tested_by_property`), not a
      second command here. Concretely: the `concrete test` event stream and
      `--json` schema must be able to carry property/fuzz cases (seed,
      generator profile, case count, shrunk witness, evidence class) without a
      schema break when Phase 10 #11 lands. Do NOT add a separate `concrete
      test --fuzz` surface; this item only reserves the UX space.
21. Add `concrete lint` / `concrete vet` before public examples grow:
    semantic warnings that are not type errors, but still matter for audit and
    release policy. This is Concrete's Clippy-like layer, but evidence-aware
    rather than style-only. V1 lints: ignored fallible results, suspicious
    capability widening, overly broad `with(Alloc, File, Network)` signatures,
    manual cleanup patterns where explicit `defer` would be safer, unreachable
    or vacuous contracts, redundant runtime checks, likely path/bytes/text
    confusion, unstable public API use, weak proof claims, and release-policy
    warnings. Every lint must have a diagnostic code, source span,
    machine-readable JSON payload, and an explicit allow/deny policy. In strict
    profiles a lint is either fixed, explicitly allowed by policy with a reason,
    or a build failure; Concrete should not grow a C/C++-style pile of ignored
    warnings.
22. Add debug/trace mode: `concrete run --trace`, interpreter step traces, Core /
    lowered-IR dumps, source spans in runtime errors, and stable replay commands
    for report/debug failures.
23. Add developer profiling/coverage commands for Concrete programs:
    `concrete run --profile`, `concrete test --coverage`, and
    `concrete trace --json`. These are ordinary debugging surfaces, not proof
    evidence; audit output must label them `tested/profiled`, never `proved`.
24. Add interactive evidence commands for low-ceremony feedback without a live
    mutable REPL: evaluate a function with concrete inputs, inspect Core and
    ProofCore for one function, show the current generated obligation, and
    replay a failing proof/debug report. Target commands include
    `concrete eval`, `concrete inspect --core`, `concrete inspect --proofcore`,
    `concrete prove --show-obligation`, and `concrete run --trace`.
25. Add basic LSP/editor diagnostics early: parse/type errors, capability
    summaries, hover for inferred types, and jump-to-definition. Deeper
    proof/evidence LSP features remain in the later editor phase.
26. [DECIDED — 2026-06-27; docs/TARGET_CONDITIONAL.md] Target-conditional code
    selection: prefer profile/target-selected source roots and modules in
    `Concrete.toml` (built on the existing `[profile]` section) over in-source
    conditionals — which file compiled for which target is a build/audit fact,
    not preprocessor state. A narrow `cfg`-style attribute is deferred, not
    forbidden: if a workload needs it, it must be LL(1)-safe, small,
    target/profile-only, and reported in audit. Until then there is no `cfg`.
    Implementation of profile-keyed source roots lands when cross-platform stdlib
    work needs it.
27. [DECIDED — 2026-06-27; docs/TARGET_CONSTANTS.md, impl deferred] Compiler-known
    target constants (`CONCRETE_OS`, `CONCRETE_ARCH`, `CONCRETE_ENDIAN`,
    `CONCRETE_TARGET_PROFILE`, `CONCRETE_BUILD_PROFILE`, `CONCRETE_TOOLCHAIN_VERSION`):
    decided — they will be exposed as typed, resolved AUDIT FACTS (never hidden
    preprocessor state), recorded in `concrete inspect --resolved --json` +
    `--report audit` + release bundles. Implementation deferred until the
    target-profile machinery needs them (alongside #26's profile-selected source
    roots + cross-platform stdlib). Acceptance bar when it lands:
    `examples/target_constants/` + `scripts/tests/check_target_constants.sh`
    proving constants appear in the resolved-fact layer and a target-dependent
    branch is labelled `target_selected`, not erased.
28. Normalize the CLI around predictable verbs:
    `concrete build`, `concrete run`, `concrete test`, `concrete fmt`,
    `concrete lint`, `concrete vet`, `concrete audit`, `concrete prove`,
    `concrete eval`, `concrete inspect`, `concrete doc`, `concrete bench`,
    `concrete trace`, and `concrete clean`. `concrete fmt` must be a real
    subcommand with `--check`, `--write`, stdin/stdout behavior, and stable exit
    codes, not only a legacy `--fmt` flag. Add `concrete new` / `concrete init`
    for the project bootstrap path documented in `docs/PROJECT_BOOTSTRAP.md`.
    Every public command, every missing subcommand, and top-level
    `concrete --help` / `concrete help <cmd>` must fail or succeed through the
    CLI contract rather than falling through to "open this as a file". Extend
    `scripts/tests/check_cli_contract.sh` with help, init/new, fmt check/write,
    unknown command, and missing-file cases.
28a. Add a fast check-only inner loop before broad stdlib growth:
     `concrete check` parses, resolves, type-checks, enforces linearity,
     checks capabilities, validates ignored-result/must-use rules, and runs
     cheap contract/profile eligibility checks without invoking LLVM, linking,
     or executing the program. This is the Rust `cargo check` lesson adapted to
     Concrete: most daily feedback should not wait for backend codegen. The
     command must share the same frontend/checker/CoreCheck path as production
     builds, emit the same diagnostics/facts, and support `--json` for editors
     and agents. Gate with a project where `concrete check` catches a type
     error, a capability error, a linearity error, and an ignored-result error;
     and with a project where `concrete check` succeeds while `concrete build`
     still fails later on a deliberate backend-only fixture.
29. Add `concrete doc`: generate basic API/reference docs from source,
    capabilities, modules, and public comments without depending on proof
    infrastructure. Outputs must include `concrete doc --format json` and
    `concrete doc --format html`, so editor tooling and published docs reuse
    the same source.
30. Add a first-user tutorial path for C/Rust developers that does not start
    with proofs: install, hello world, values and fixed arrays, ownership,
    borrows, capabilities, explicit errors, tests, compiled debugging, audit,
    then proof-bearing examples. The tone should be "ordinary systems code with
    visible evidence," not proof-assistant ceremony.
31. Add useful non-proof examples: a small CLI tool, a protocol decoder, a
    bounded cache, and a capability-scoped file/console program. Use exact
    examples `examples/daily/word_count`, `examples/daily/packet_decoder`,
    `examples/daily/lru_cache`, and `examples/daily/file_summary`. Wire
    `scripts/tests/check_daily_examples.sh` to build, run, compare
    interpreter-vs-compiled output, assert `--report effects`, assert
    `--report audit`, and pin one negative fixture per example under
    `examples/daily/*/catches/`.
32. Add basic benchmarking UX: `concrete bench` runs small benchmarks, compares
    interpreter versus compiled performance, emits `--json`, and detects
    obvious generated-code regressions. This is separate from the compiler
    performance-budget gates (a closed-Phase-4 deferral, now folded into
    Phase 17's artifact/stability hardening).
34. Add cross-platform build sanity for the supported host set: macOS and Linux
    first, with CI coverage, reproducible commands, and documented toolchain
    expectations.
34a. Process: CI health must be CHECKED before calling a branch green — twice
    now (the dead-YAML episode greened 2026-06-24, and again 2026-06-27 to
    2026-07-01) CI stayed red for 14-15 pushes with nobody noticing, because
    local fast-suite green was treated as done. Finalization ritual after any
    push: `gh run list --workflow CI --limit 1` (or the repo dashboard) and
    confirm the latest run's conclusion; a red run is a stop-the-line item
    before the next feature push. Landed 2026-07-02: `make test-ci-gates`
    (`run_ci_gates_local.sh`) runs every workflow gate locally, extracted from
    the workflow file so the list cannot drift; and the monolithic proof-gate
    CI job was split into three parallel jobs (proof/VC/obligations,
    language-surface, compiler-infra) so a red run reports all failures at
    once instead of one per 46-minute cycle. Remaining: a scheduled job that
    notifies on consecutive red runs. Lesson (2026-07-05, false-red nightly
    plus false-green SSA job): avoid `pipefail` + `echo "$big" | grep -q` on
    large captured output — `grep -q` exits at the first match and SIGPIPEs
    the producer when the output outgrows the runner's pipe buffer (use
    `grep -q ... <<<"$big"`); and every gate script must exit nonzero from
    its own internal FAIL count, or CI reports it green regardless.
34b. Shared gate harness: `scripts/tests/lib/gate.sh` (ok/no counters,
    `rejects`/`accepts`/`agree`/`agree_both` helpers, tmpdir lifecycle) so new
    gates stop hand-rolling the same bash and fixtures stay greppable —
    three stale-fixture CI failures on 2026-07-01/02 hid inside per-gate
    heredocs. `check_cast_matrix.sh` is the first consumer; new gates should
    source it, and existing gates migrate opportunistically when touched.
35. Add the Phase 6 validation project:
    a small C/Rust-style CLI using the
    Phase 5 core slab plus daily workflow (`Concrete.toml`, modules/imports,
    `concrete test`, bytes/text/path and collection decisions, narrow const
    generics, pattern ergonomics, callable/capability callback decisions,
    diagnostics, formatting, docs, lint/vet, benchmark/profile/coverage smoke
    tests, and trace/debug commands). CI must build, run, test, format-check,
    lint, audit, record compiler-known target constants, and compare
    interpreter-vs-compiled behavior on macOS and Linux. It validates the
    language/tooling slab, not the full stdlib. Include at least one
    reload/plugin-shaped or sandbox-shaped boundary if it can stay small:
    a module loaded through an explicit capability/API surface, or a
    WASM-like/sandbox placeholder that proves Concrete's authority reports can
    explain "what this extension may touch." Native arbitrary-code loading stays
    trusted; sandboxed or constrained extensions must expose their authority in
    headers and audit output. Preferred first workload after the stdlib survey:
    a log analyzer / structured text-processing CLI, because it stresses
    errors, buffered IO, bytes/text boundaries, path/FS, maps, scoped
    callbacks, output formatting, and an optional filter/plugin-shaped
    authority boundary without demanding broad networking or crypto.
37. Add a docs/profile synchronization audit before verified-profile work.
    `docs/PROFILES.md`, README/site copy, guarantee docs, and profile reports
    must agree with current implementation: known holes open section empty,
    checked array bounds, checked arithmetic policy, conditional `Copy`, linear
    params, std fully front-end checked, proof limitations, and what remains
    trusted/reported rather than proved. Gate with `check_docs_drift.sh` or a
    new `check_profile_docs.sh`; include negatives for stale "array bounds not
    covered", stale "integer overflow wraps silently", stale H11/open-hole
    status, and any wording that upgrades tests/runtime checks/SMT to formal
    proof. Extend the same gate into a doc-snippet compile check: every Concrete
    code block in README/site/docs either compiles under an explicit fixture
    harness or is marked `pseudocode`/`text` with a reason. This item is a
    prerequisite for pulling the verified profile forward.

## Phase 6B / 6.5: Compiler Pipeline Refactor And Invariant Hardening

Goal: make the compiler pipeline have one opinion about each program before
Phase 7 expands the stdlib and daily workload surface.

Done when: shared type/policy/evaluation/effect decisions live in one place,
every major stage states and checks its invariants, new syntax cannot bypass
walker coverage, production/verification/test paths use the same composed
pipeline unless explicitly justified, and pipeline debugging/reporting can
explain where a fact was introduced or lost. Cacheable stage boundaries also
emit structured producer validation records and a versioned chain contract,
without claiming independent certification.

Design rule for this phase: borrow the useful workflow lessons from Zig, Rust,
Odin, SPARK, and Lean, but not their hidden ambiguity. Concrete should keep
Zig's small-toolchain feel and explicit allocator lesson, Rust's fast feedback
loop and review tooling discipline, SPARK's runtime-error proof framing, and
Lean's kernel-checked replay discipline. It should not import Zig-style lazy
checking of unused code, Rust-style implicit `Drop`, broad compile-time
metaprogramming that hides generated behavior, or C/C++ warning culture. Every
pipeline refactor below is judged by that standard: fewer second truth sources,
faster local feedback, stronger evidence replay, and no new dark semantics.

This phase is for refactors pulled by real bug classes, not aesthetic file
splits. A task belongs here only if it prevents "Check says one thing, another
stage says another" bugs, catches a missed constructor/stage interaction, or
makes pipeline failures reproducible.

The item numbers in this phase are ordered within the phase and may be
renumbered when the phase is reorganized. Keep cross-references updated in the
same change, and use subsection headings to make the dependency order clear.
At this point the Phase 6B / 6C plan is frozen unless implementation exposes a
real missing invariant. Item #14a is the one recorded exception: planning the
incremental/certifying path exposed that a stage token plus a `Unit`-returning
verifier is not yet a portable validation record, so the record/chain contract
must exist before later phases cache those artifacts. The execution frontier is
still: finish
`CopyJudgment` / `InstantiationJudgment` first, because `Copy` feeds ownership;
then build `OwnershipJudgment` / `ValueFlowJudgment`, because it is the
memory-safety axis; then ship `diff-caps` as the first external-facing
credibility artifact once the internal semantic axes stop moving.

**North star ([docs/PIPELINE_ARCHITECTURE.md](docs/PIPELINE_ARCHITECTURE.md)):**
every item below is an instance of one principle — *each program has exactly one
meaning, committed once by the stage that owns it, read-only for every stage
after*. Two axes: (1) **one reference semantics** — the interpreter IS the
language's operational meaning, and the folder/backend are derived from or
checked against it, so the oracle is true by construction, not by fuzzing
(`IntArith`, feature matrices, counterexample/replay gates); and (2) **facts
committed once** — every semantic fact (type, cap,
ownership/value-flow, arithmetic policy, evidence class, resolved identity) is
stamped once and never re-derived downstream. The key escalation this phase drives is from
*gates that DETECT drift* to *types that FORBID it*: shared predicates and
pass-agreement gates catch disagreement after the fact, but certificate-carrying
IR makes re-derivation unrepresentable. That is the
evidence-carrying-artifact thesis applied to the compiler's own IR. Long-term
architecture wins over short-term compatibility in this phase: if a clean
typed/certified boundary requires broad breakage, take the breakage and land it
behind gates rather than enshrine a weak bridge as permanent architecture.

Quality bar for this phase: finishing `IntArith` alone is not "the best
possible pipeline"; it is the first load-bearing semantic axis. The pipeline can
make that claim only when every major semantic axis has the same discipline:
one reference truth, one owning stage, no hidden alternate pipeline, facts
carried forward instead of re-derived, backend lowering derived from compiler
facts rather than parallel policy code, typed report/evidence rows, and
mutation-tested gates proving the checks are load-bearing. Until then, public
language claims should say "pipeline hardening in progress" rather than imply
the compiler architecture is finished.

**Correctness-evidence rule.** `interp == compiled` is an agreement check, not
a proof that the shared meaning is correct. Each runtime-facing semantic axis
must therefore name its source of correctness: a small reference semantics or
policy module (`IntArith`, `TypeJudgment`, future `OwnershipJudgment` /
`TotalityJudgment`), an external/profiled spec where the language deliberately
inherits one (UTF-8/string policy, target ABI, future float profile), adversarial
red-team fixtures for plausible shared wrong assumptions, and eventually proof
obligations for preservation-critical subsets. Differential equality catches
drift between implementations; reference specs, external policies, and proofs
are what prevent two implementations from agreeing on the same wrong rule.

**Judgment-module contract.** `IntArith`, `TypeJudgment`,
`CapabilityJudgment`, `CopyJudgment` / `InstantiationJudgment`,
`OwnershipJudgment` / `ValueFlowJudgment`, and `TotalityJudgment` are not
ordinary helper modules. They
are the pipeline's named semantic decision points.
Every judgment module must be pure and deterministic, return a decision record
rather than a bare `Bool`/`Ty`/`CapSet`, be the only implementation of that
decision, feed diagnostics/reports/audit rows from the same record, name the
owning stage and downstream consumers, state what is intentionally out of scope,
and land with a red-team agreement gate proving old divergent consumers cannot
disagree. Each judgment must also be consumed by or checked against the
interpreter/oracle when the fact affects runtime behavior; prove completeness
when deleting a prior implementation (the new judgment covers the old behavior
matrix, not merely the cases where consumers already agree); and obey the
fact-home rule (node-local structural facts become typed-IR fields, relational
or cross-node/cross-stage facts live in `CompilerDB` when that DB is pulled). If
a later pass recomputes a judgment fact instead of consuming the record or typed
IR field, that is a pipeline bug.

### Semantic Judgment Axes

These items give each remaining semantic axis one owner. Completed arithmetic
(`IntArith`) and source typing (`TypeJudgment`) are recorded in
[CHANGELOG.md](CHANGELOG.md); the active work here is Copy/instantiation,
ownership/value-flow, capabilities, and the relations that cannot live on one
typed node.

1. Promote Copy / trait-bound / instantiation policy into
   `CopyJudgment` / `InstantiationJudgment`.
   The predicate centralization work landed the first slice (`isCopy`,
   conditional Copy after substitution, type substitution/type-var matching),
   but the long-term target is a real judgment axis, not a bag of shared
   booleans. Conditional `Copy`, trait-bound satisfaction, generic
   substitution, type-var matching, turbofish/caller type params, post-mono
   Copy demotion, and generated specialization facts are one semantic decision
   family and must not be reimplemented across Check, Elab/Core, Mono,
   CoreCheck, Verify, reports, or generated-name logic.

   **Landed 2026-07-10 — the boolean `isCopy` recursion is now single-sourced.**
   There were TWO implementations of "is this type Copy?": the monadic front-end
   `CheckHelpers.isCopyType` (over `StructDef`/`EnumDef`/`NewtypeDef`) and the
   pure `Layout.isCopyTyCore` (over `CStructDef`/`CEnumDef`, shared by Mono/
   Verify/CoreCheck). They drifted on `typeVar` (bounds vs a fixed flag) and
   `newtype` (recurse vs absent), kept in agreement only by ordering luck (Check
   gates unbounded type vars first; newtypes resolve before the Core stages run).
   Both now delegate to ONE recursion, `Layout.isCopyTyGeneric`, parameterized by
   lookups (`lookupAgg` / `lookupNewtype` / `typeVarIsCopy`) so each
   representation feeds its own data — the primitive set, the newtype recursion,
   and the conditional-aggregate field check exist once. Gate:
   `scripts/tests/check_copy_judgment.sh` (mutation-tested: breaking the newtype
   recursion fails the newtype-over-Copy case).

   **Landed 2026-07-11 — type-arg inference (`unifyTypes`) is single-sourced.**
   The first `InstantiationJudgment` slice: `unifyTypes` (structural matching of a
   parameter type against an argument type to bind generic type args) was likewise
   defined TWICE — `Elab.lean` (private) and `CheckHelpers.lean` — two copies of
   one algorithm. Differential probing confirmed Check and Elab infer/reject
   generic calls identically today, so this was a behavior-neutral dedup into
   `Shared.unifyTypes` that removes the latent drift; a turbofish-generic agree
   row was added to `check_type_agreement.sh`.

   REMAINING: promote the boolean `isCopy` to the full decision RECORD below
   (conditional + concrete forms, provenance, failing field, trait bounds), and
   the rest of `InstantiationJudgment` (turbofish/caller type params, post-mono
   specialization facts). A known adjacent usability gap surfaced during probing:
   generic inference does not flow the return/let-context type into type-arg
   inference (`let v: i32 = id(42)` needs `id::<i32>(42)`); consistent across
   interp/compiled, so a feature gap, not drift.

   The judgment record should model both stages of knowledge: a **conditional**
   form (`Copy` modulo requirements such as `{T : Copy}`) and a **concrete**
   post-substitution form (`Option<i32>` is `Copy`; `Option<File>` is linear).
   Include at least: the original type, substituted concrete type when known,
   instantiation site, conditional requirements, concrete `is_copy`, why it is
   `Copy` (`primitive`, `declared_copy`, `conditional_aggregate`,
   `type_param_bound`, etc.), the first failing field/payload when it is not
   `Copy`, satisfied or failing trait bounds, any caller/turbofish type
   arguments used, diagnostic reason, ownership/linearity consequence, and
   report/audit payload. Consumers should read that record; they should not ask
   their own version of "is this Copy?" after substitution. This judgment feeds
   the ownership axis directly: `Copy` decides whether a value may duplicate,
   whether `_`/statement discard is legal, and whether a specialization is
   linear or copyable.

   Gate with **consistency under refinement**, not naive stage equality:
   applying the pre-mono conditional judgment to a concrete instantiation must
   equal the post-mono concrete judgment. Cover generic `Option<T>` /
   `Result<T,E>`, user generic structs/enums, nested generic fields, trait
   bounds through turbofish/caller type params, arrays, newtypes, post-mono
   demotion cases, non-Copy payloads, missing bounds, and one red-team where
   Check accepts but Mono/CoreCheck/Verify would otherwise instantiate the
   condition differently. Sequence after the core `TypeJudgment` work (you need
   the type before asking whether it is `Copy`) and treat it as a peer of
   `CapabilityJudgment`, not a distant report/database item.

2. Promote ownership / value-flow into
   `OwnershipJudgment` / `ValueFlowJudgment`.
   Concrete's defining promise is linear
   ownership, but the Phase 6B / 6.5 judgment list currently names arithmetic, type,
   Copy/instantiation, and capability before the ownership decision itself. The
   value-flow spec and H9-H17 work made Check much more disciplined, but they
   are still framed mainly as "the checker rejects bad programs." That is not
   enough for the long-term pipeline: ownership facts are consumed by Check,
   Lower/codegen, the interpreter, reports, and eventually proof extraction.
   If those consumers re-derive "move vs copy vs borrow vs drop" separately,
   Concrete keeps the same drift shape that `TypeJudgment`, `IntArith`, and
   `CopyJudgment` were created to eliminate.

   The judgment should own the use-site/value-flow decision record, not just a
   boolean. Include at least: source place/expression, input type, `Copy`
   decision from `CopyJudgment`, use mode (`copy`, `move`, `borrow`, `mut_borrow`,
   `reborrow`, `consume`, `place_write`, `init`, `overwrite`, `discard`,
   `diverge`), whether the site is a last use, liveness before/after the site,
   scope-exit destroy/drop obligations, control-flow-edge obligations (`return`,
   `break`, loop exit, match/if arm, diverging branch), rejection reason,
   diagnostic payload, and report/audit payload. For aggregate/destructure/
   assignment/return/callback cases, the record should also name the destination
   or pass-agreement fact it depends on.

   Consumers:
   - **Check** uses the record to reject use-after-move, double consume, live
     overwrite, illegal discard, illegal non-Copy projection, and borrow
     conflicts.
   - **Lower** uses the same record, or a plan derived from it, to choose
     move/copy/borrow lowering and to insert `Destroy`/drop operations on the
     exact live-linear paths. Lower must not run a parallel liveness/drop
     analysis that can disagree with Check without an agreement gate.
   - **Interpreter** models consumption from the same decision family so the
     differential oracle agrees on ownership-sensitive programs.
   - **Reports/audit/proofs** explain why a value was copied, moved, borrowed,
     destroyed, rejected, or considered live on a path.

   First slice: audit the current ownership consumers. Identify every place
   Check, Lower, the interpreter, and reports decide move-vs-copy, last-use, drop
   placement, or discard independently. Turn that into a small ownership
   agreement matrix before building a giant database: H14/H15/H17-style
   break-value consume, live overwrite, param consume, reborrowed callback
   context, divergent branch, loop exit, match arms, `_` discard, assignment
   overwrite, aggregate construction, and non-Copy projection.

   Second slice: refactor the checker around a narrow ownership-transition API.
   Direct mutation of `env.vars`, consumed states, borrow/ref lists, or branch
   snapshots should disappear outside helpers such as `declareVar`, `moveVar`,
   `copyVar`, `borrowPlace`, `markConsumed`, `reserveForDefer`, `restoreScope`,
   and `mergeBranchStates`. This is the local, low-risk prerequisite for a
   judgment: make every current ownership transition auditable before exporting
   decisions to later stages.

   Third slice: centralize ownership-transfer and overwrite policy. One helper
   should answer "this expression transfers ownership" for args, returns,
   `break value`, assignment RHS, array/struct literals, destructuring, stores,
   `?`, and `defer`; another helper should answer "this place may be
   overwritten" for rebinds, fields, indices, `&mut` pointees, and trusted
   uninitialized-slot boundaries. These helpers own the E0219/E0291 family and
   should replace scattered per-AST `if ident then consumeVarIfExists` logic.

   Fourth slice: represent control-flow exit modes as data. Branches, match
   arms, loops, `return`, `break value`, `continue`, `abort`, and future
   `noreturn` calls should produce `fallsThrough | returns | breaks(value?) |
   continues | diverges`; linear merge rules should consume that value instead
   of re-deriving reachability and ownership state in each path.

   Fifth slice: define the decision-record shape and central computation for
   the cases already covered by the value-flow spec. It is acceptable for the
   first implementation to be "shared computation + agreement gate" rather than
   a fully committed per-node record, but the roadmap target is stronger:
   Check-owned value-flow decisions become facts that later stages consume or
   derive mechanically, not policy they rediscover.

   Gate: promote the linearity fuzzer into an ownership agreement gate:
   `Check ownership decision == Lower move/drop plan == interpreter consumption
   behavior` over the matrix above. Red-team by disabling one drop edge,
   treating a non-Copy value as Copy, treating a moved value as live, or letting
   Lower insert a destroy on a path Check considered already consumed. The gate
   must fail with a minimized `.con` fixture and a replay command. This item is
   a prerequisite for claiming the pipeline has first-class fact discipline:
   without it, the less central axes are cleaner than Concrete's own linearity
   model.

3. Centralize capability checking and callback propagation into
   `CapabilityJudgment` after the type-axis core is stable.
   Capabilities are as central to Concrete as arithmetic and source typing, but
   this must stay practical: **do not add algebraic effects, effect handlers,
   row-polymorphism syntax, implicit context, or theoretical effect-system
   vocabulary to the user surface.** Concrete keeps explicit authority headers:
   `fn f(...) with(File, Network) -> T`. The improvement is internal:
   `CapabilityJudgment` becomes the capability-axis sibling of `IntArith` and
   `TypeJudgment`, producing one decision record for why a call/expression needs
   authority and how that fact should be diagnosed, reported, audited, and
   surfaced to tools.

   The decision record should include at least: required caps, source of the
   requirement (`direct_call`, `callback`, `trusted_wrapper`,
   `unsafe_intrinsic`, `package_import`), callee/callback parameter identity
   where relevant, purity (`with()` / empty cap set), evidence class,
   diagnostic reason, and report/audit payload. Check, CoreCheck, Report, audit,
   LSP/agent JSON, and package gates must consume this one decision instead of
   recomputing capability containment, callback cap propagation, Unsafe/trusted
   policy, or human report rows independently.

   First slice: direct calls. **Landed 2026-07-11 (`9042b3e3`).** The satisfaction
   decision was already `capsContain` (#5), but the direct-call *decision record* —
   which caps are missing, rendered per-cap (E0240) by Check and whole-set (E0520)
   by CoreCheck — was open-coded per surface. `Capabilities.decideCall`
   (`{required, callerHas, satisfied, missing}`) + `Capabilities.missingCaps` now
   own it; Check (both cap-polymorphic sites), CoreCheck, and reports read the one
   record. Gate `check_capability_judgment.sh` (mutation-tested). DEFERRED to the
   callback slice: the fn-pointer anti-smuggling check (Check.lean:589) is a
   stricter policy (caller must hold the callee's cap *variables*), not the
   direct-call decision.

   Reports slice — **Landed 2026-07-11 (`e4a9dcc0`).** The report/audit surfaces
   recomputed capability *membership* ("does this callee declare cap X") inline at
   four sites (`ReportInterface`, two `Report.lean` why-traces, `ReportObligations`
   Unsafe count). `Capabilities.capSetHas (cs) (cap)` now owns that fact and
   `capSetHasUnsafe` delegates to it; the four sites route through it, so report
   provenance and the checker read the ONE membership fact. Covered by
   `check_capability_facts.sh`, now backed by `capSetHas`.

   Second slice: capability-polymorphic callbacks and callable values. Existing
   callable rules stay; `CapabilityJudgment` centralizes the practical decision:
   "this callback requires `C`, therefore the combinator/caller requires `C`."
   Cover `for_each_with`, `for_each_ctx`, scoped callbacks, and callable values.

   Third slice: trusted/Unsafe/package boundaries. Trusted wrappers, Unsafe
   intrinsics, externs, dependency/package capability budgets, and audit diffs
   should all render from the same capability decision record.

   `diff-caps` slice — capability-surface diff as an acceptance gate (the
   agent-review headline for this axis, and a consumer that makes the whole
   capability machinery visible rather than internal plumbing).
   `CapabilityJudgment` already computes a function's required caps transitively;
   emit that as a first-class **capability manifest** — the transitively-closed
   required-cap set per function/module/package — and add
   `concrete diff-caps <old> <new>` comparing two manifests (two commits, a PR,
   or an old-vs-new dependency version) to report the AUTHORITY DELTA: what caps
   the change newly grants. The reviewer of AI-authored code asks "what can this
   now do that it couldn't?", answered without line-by-line review. The contract
   IS the gate and is load-bearing (byte-stable, never reworded casually): exit
   `0` = no authority expansion, `1` = authority expanded (a human reviews the
   delta), `2` = usage/parse error; `--machine` emits a deterministic
   single-line, schema-versioned JSON verdict (`concrete.diff-caps/1`) with
   identical exit codes; on error stdout is empty and the diagnostic goes to
   stderr; scoped to the declared/inferred capability surface ONLY (no
   other-axis claim, and it says so). Depends on a determinism gate — the
   manifest and machine JSON must be byte-deterministic across runs or the
   contract is worthless; this is the concrete consumer that makes a determinism
   gate load-bearing rather than abstract hygiene. Gate: a change adding a
   transitive `File`/`Network` cap -> exit `1` with the delta in both human and
   machine output; a no-op refactor -> exit `0`; a dependency bump that widens a
   transitive cap -> exit `1` (ties to Phase 18 budgets); identical inputs ->
   byte-identical machine JSON across two runs; a parse error -> exit `2`, empty
   stdout. Prior art: Garnet's `diff-caps` (Island-Dev-Crew/garnet) — the
   three-value exit-code contract and narrow-scope discipline; see
   `research/compiler/pipeline-lessons-2026-07.md`.

   The capability manifest is itself a stable, schema-versioned, deterministic
   artifact — not merely an internal input to `diff-caps`. It is defined and
   emitted HERE, at its owning stage (`CapabilityJudgment` + the determinism
   gate), because it is the atom every later evidence feature consumes: Phase 18
   dependency capability budgets, package evidence summaries, supply-chain
   review, and signed release bundles all assemble from it. Defining the schema
   at the judgment that owns the fact — rather than retrofitting a manifest
   format in Phase 18 — is the "commit the fact once, at its owner" discipline
   applied across the phase boundary.

   Generalize to `diff-evidence` only when pulled (Phase 11/18, agent-review
   workload). `diff-caps` is the first instance of a family: the same
   manifest + delta + three-value-exit-code + byte-stable/`--machine` contract,
   applied one evidence axis at a time, each an HONEST SEPARATE delta (never a
   muddy "changed"): cap-diff (this slice); trust-diff (change newly requires
   `Unsafe`/`trusted` or widens the TCB — Phase 12/14 trust facts); proof-diff
   (a function lost its proof or dropped `proved -> stale/missing/assumed` —
   Phase 11 proof-status drift); evidence-class-diff (any claim's evidence class
   weakened — Phase 10/17 evidence ledger). Each reads committed decision
   records / the evidence ledger, never recomputed. The full agent-review
   question — "what new authority AND what weaker evidence does this change
   introduce?" — is recompute-don't-trust applied to the delta, a strictly
   stronger acceptance story than a caps-only diff. Do not build the other axes
   before their evidence sources are stable.

   Practical purity/discard slice: once `CapabilityJudgment` can say an
   expression is known **pure and trap-free/total** (`with()` / empty authority,
   no mutation, no checked-arithmetic trap, no narrowing trap, no bounds trap,
   no runtime obligation that can fail) and returns a non-`Unit` value, make
   `expr;` over that expression an error with an explicit acknowledgement
   escape (`discard(expr)` or equivalent). This is not row-effect theory and not
   a user-facing effect system; it is the same concrete discard discipline
   already used for fallible and non-`Copy` values. A total pure non-`Unit`
   result discarded as a statement is almost always a lost computation.
   Potentially trapping pure expressions are excluded until
   `TotalityJudgment` proves they are total; their traps are observable runtime
   behavior.

   Gate with an ordinary `File`/`Network` call, a capability-polymorphic
   callable, a scoped callback, a trusted wrapper, an Unsafe intrinsic, a
   dependency/package boundary, a pure non-`Unit` discarded statement, and one
   negative where report output would otherwise disagree with the checker
   diagnostic. The accepted design is the useful Flix/Koka lesson — one
   capability/effect decision — without importing their user-facing effect
   systems.


### Runtime And Oracle Contracts

These items make the interpreter, runtime order, traps, drops, unsupported
runtime families, and deterministic artifacts explicit contracts instead of
assumptions scattered across fuzzers and reports.

4. Add an interpreter reference-semantics contract.
   The interpreter is the compiler's executable oracle, so its boundary must be
   explicit instead of implicit in scattered differential tests. Define what the
   interpreter is authoritative for (source expression evaluation, integer
   semantics through `IntArith`, ownership consumption once
   `OwnershipJudgment` lands, capability/trap behavior where supported, standard
   control-flow behavior, and observable runtime errors), what it intentionally
   does not model yet (for example unsupported allocation/runtime families,
   host I/O, platform-specific backend behavior, or currently unsupported
   literal/runtime forms), and how unsupported cases fail loud rather than
   silently escaping the oracle.

   Every judgment whose decision affects runtime behavior must either be
   consumed by the interpreter or explicitly marked outside the interpreter
   contract with a reason and a coverage replacement. This is the permanent home
   for the "interp tie" clause in the judgment-module contract: `IntArith`,
   `TypeJudgment` decisions that affect runtime width/traps,
   `CapabilityJudgment` runtime traps, `OwnershipJudgment` consumption, and
   future intrinsic/runtime judgments must name their interpreter relationship.

   Done when `docs/INTERPRETER_CONTRACT.md` (or equivalent) lists supported and
   unsupported feature families, `check_differential_positions.sh` / feature
   matrix rows point at that contract, unsupported interpreter cases have stable
   diagnostics, and a gate fails if a new runtime-affecting judgment is added
   without either interpreter consumption or an explicit out-of-contract row.

   This contract must state the evidence ladder explicitly: differential parity
   means the interpreter and compiled artifact agree, but correctness needs an
   owning semantic definition. For every supported runtime family, name the
   reference policy the interpreter implements or checks against (`IntArith` for
   integers, the string/UTF-8 policy for string builtins, `OwnershipJudgment` for
   consumption/drop behavior, `CapabilityJudgment` for authority traps, and
   future float profiles for floating point). A supported family with no named
   policy is not fully specified; a family with no interpreter support must be
   unsupported-loud and listed in the feature matrix. Red-team rows should target
   shared-wrong assumptions, not only interp-vs-compiled disagreement.

   Trap-parity as recorded evidence (Garnet's "cross-OS trap parity recorded as
   evidence, not asserted"). The contract must cover not just which features the
   interpreter models but that **traps agree across interp, compiled, and each
   backend/target** — capability traps, checked-arithmetic traps, bounds traps,
   cast/narrowing traps, and unsafe/trusted traps — and that this agreement is a
   first-class evidence class surfaced in `--report`, not an implicit property of
   scattered `both_trap` gate rows. Generalize the arithmetic `both_trap` row
   into a trap-parity matrix (trap kind x interp / compiled / target) whose
   result is recorded, so "the oracle and the artifact trap identically here" is
   stated evidence a reviewer or agent can read, not luck the fuzzer happens to
   catch. A divergence (one side traps, another does not) is a hard failure; any
   genuinely target-specific trap difference must be an explicit, reported target
   assumption, never a silent one.

5. Add interpreter-vs-compiled coverage by feature and classify unsupported
   oracle families.
   The differential fuzzer and corpus sweep are necessary but not sufficient.
   Maintain a feature matrix: every language construct has at least one
   interp-vs-compiled test, fuzzer generator, or explicit compile-only/proof-only
   rationale. Gate that a new constructor cannot be added without a coverage row.

   The corpus sweep found the current frontier: remaining divergences are loud
   interpreter limitations, not silent wrong-code. Treat that as an oracle-coverage
   backlog, not an excuse to stop checking. First classify unsupported cases by
   family (for example `Vec` / heap allocation, floats, host I/O, or specific
   builtins), record each family in the interpreter contract, and require stable
   unsupported diagnostics. Then implement the highest-leverage family when it
   unlocks meaningful differential coverage. If `Vec` / heap dominates, it is
   the natural first implementation target because it also unlocks the future
   ownership runtime-conservation oracle. Do not rush floats before their
   profile/proof policy is ready.

   Done when the feature matrix can say, for each family, `supported and compared`,
   `unsupported and fails loud`, or `compile-only/proof-only with rationale`; the
   largest unsupported family is quantified; and adding support for a family adds
   permanent interp-vs-compiled rows rather than ad hoc probes.

   Current oracle-growth backlog from the corpus sweep, after string and core
   `Vec` support: function pointers/callables in the interpreter are the next
   small high-leverage slice (`undefined variable 'double'` / `add_op`-style
   failures in phase3/integration programs) because they unblock callback-heavy
   programs and exercise capability/callable semantics. Small finishers after
   that are `vec_pop` and `string_to_int`, both of which need Option/Result enum
   result modeling. `alloc`/`malloc` and the heap model remain larger work and
   should be pulled when ownership runtime-conservation needs it. Floats stay
   last among these oracle gaps unless a workload forces them, because they need
   the named float profile/proof policy before the interpreter can claim more
   than tested parity.

6. Add an evaluation-order, trap, and drop sequencing contract.
   Concrete needs one owner for "what happens when" just as much as it needs one
   owner for types or capabilities. Evaluation order decides when arguments,
   receivers, array/struct elements, conditions, branch values, match
   scrutinees, assignments, returns, callbacks, and `defer`/cleanup paths run.
   Trap/drop sequencing decides whether a checked-arithmetic trap fires before a
   destroy, whether a bounds trap prevents later side effects, whether a
   capability check happens before a callback body, and where linear cleanup runs
   on normal, return, break, and diverging paths. Interp, Check, Lower, and the
   backend must not each carry a private order model.

   This contract should be practical, not theoretical effect sequencing. It
   should state left-to-right / receiver-before-args / condition-before-branch /
   scrutinee-before-arm / element-order rules where Concrete commits to them,
   and state any intentionally unspecified order explicitly. It should connect
   to `OwnershipJudgment` for drops/destroys, `IntArith` for arithmetic traps,
   `CapabilityJudgment` for authority checks, and the interpreter contract for
   executable behavior.

   Gate with fixtures that observe order through traps or safe capabilities:
   argument order, receiver vs argument order, array/struct literal element
   order, if/match condition/scrutinee before selected branch/arm, short-circuit
   behavior if supported, trap-before-drop, drop-on-return, drop-on-break,
   no-drop-after-diverge, and callback authority before callback body. A red-team
   lowering mutation that moves a trap, drop, or capability check across another
   observable event must fail.

7. Build `TotalityJudgment` / the totality fact.
   Totality is the foundational fact that turns several deferred checks from
   hand-wavy policy into a shared decision. It should answer: can this
   expression/function complete normally without trapping, diverging, failing a
   runtime obligation, or relying on hidden cleanup/control-flow behavior? This
   is stricter than purity: a checked add, narrowing cast, bounds/index access,
   or failing runtime obligation may be pure but still not total because its trap
   is observable.

   Inputs: trap facts from `IntArith` and typed Core, context-width decisions from
   completed `TypeJudgment`, ownership/drop/diverge facts from
   `OwnershipJudgment` / `ValueFlowJudgment`, authority/purity facts from
   `CapabilityJudgment`, and evaluation/drop/trap order from item #6. The record
   should include at least: subject expression/function, total vs non-total,
   first failing reason (`may_trap`, `may_diverge`, `may_fail_runtime_obligation`,
   `unknown_cleanup`, `unsupported_interp_family`, etc.), source span, dependent
   facts, diagnostic/report payload, and evidence class.

   Consumers: the pure non-`Unit` discard rule in `CapabilityJudgment` (only
   total pure values are accidental lost computations), the class-6
   second-class-reference boundary verifier (borrow escape checks need to know
   which paths complete and which diverge/trap), report/audit explanations, and
   proof eligibility. A program should not be called "pure discardable",
   "safe reference-return impossible", or "predictable/provable" by
   re-deriving totality at each call site.

   Gate with total and non-total rows: literal arithmetic that cannot trap,
   checked arithmetic that can trap, narrowing casts, bounds/index operations,
   match/if branches with one trapping arm, loops with known divergence, function
   calls with runtime obligations, and a class-6 borrow-verifier fixture whose
   result changes if a diverging/trapping path is incorrectly treated as normal
   completion. Red-team by forcing a trapping expression to total and proving
   pure-discard or borrow-boundary gates fail.

8. Add a pipeline determinism gate.
   Content-addressed artifacts, proof replay, package evidence, deterministic
   reports, and future `CompilerDB` caching all assume the compiler is
   deterministic for the same source, compiler version, target/profile, and
   dependency set. Phase 6B / 6.5 should make that assumption explicit rather than
   leaving it as a Phase 18/19 surprise. The gate does not need to claim
   byte-identical native binaries when LLVM/clang introduce outside
   nondeterminism, but Concrete-owned artifacts must be stable.

   Cover report modes, diagnostics JSON, emitted Core/SSA/LLVM text where
   Concrete controls ordering, proof obligation fingerprints, capability/evidence
   rows, pass-output hashes, generated names, source spans, and any snapshot or
   replay bundle fields. Canonical serialization is the real work: map/set
   iteration, temp paths, timestamps, generated-name counters, absolute paths,
   host locale/timezone, and filesystem traversal order must not leak into
   hashed or reviewable artifacts unless explicitly excluded and labeled
   nondeterministic.

   Done when `scripts/tests/test_determinism.sh` (or a Phase 6B-specific gate)
   runs the same program twice in clean temp dirs and compares canonical outputs,
   includes at least one generic/mono case, one capability/report case, one proof
   obligation case, one generated-name case, and one failure diagnostic case, and
   mutation-testing proves that introducing map-iteration-order or temp-path
   output makes the gate fail. This is the floor under content addressing,
   replay artifacts, and future query/cache invalidation.

### Stage Boundaries And Fact Infrastructure

These items keep the pipeline itself honest: each stage has a contract, facts
have stable homes, and production/audit/proof/test entry points do not grow
hidden alternate pipelines.

9. Add stage contracts and pass-agreement assertions.
   `docs/PIPELINE_CONTRACTS.md` plus `scripts/tests/check_pipeline_contracts.sh`
   establishes that user-triggerable violations are caught at the first
   responsible boundary instead of leaking to Lower, LLVM, the linker, runtime,
   or Lean panics. Keep extending this gate whenever a new boundary rule is
   introduced. The long-term form is `concrete inspect --pipeline-contracts
   --json`, but the V1 gate is already the phase's contract backstop.

10. Add a judgment-contract conformance-audit gate.
   The prose judgment-module contract above should become a small mechanical
   gate so existing and future judgment axes do not drift back into
   helper-module habits. For every judgment named in this phase
   (`CopyJudgment`, `InstantiationJudgment`, `OwnershipJudgment`,
   `CapabilityJudgment`, `TotalityJudgment`, plus completed `IntArith` /
   `TypeJudgment`), the gate should audit the actual code/docs/test surface and
   require a decision-record type or explicit typed-IR field, an owning stage,
   named downstream consumers, a report/audit consumer or explicit
   non-reportable reason, an interpreter relationship or out-of-contract row,
   and a red-team agreement/completeness gate.

   This is not a new semantic axis; it is the meta-contract that keeps the axes
   uniform. Done when `check_judgment_contracts.sh` (or equivalent) fails if an
   existing judgment lacks one of the required contract clauses, or if adding a
   new `*Judgment` mention to the roadmap/docs/code omits a decision record,
   consumer list, interpreter relationship, or gate.

11. Add stable interned fact IDs when `CompilerDB` is pulled by real relational
   facts.
   This item depends on the completed `TypeJudgment` / typed Core
   architecture recorded in the changelog, but it is NOT a prerequisite for the
   type-axis fix. Do not build a separate ID scheme for source expression types:
   typed Core already carries those facts. When pass-agreement edges, borrow
   conflicts, E0293 path-pair facts, provenance, replay, package evidence, or
   editor/agent fact queries need stable identity, introduce deterministic
   internal identities for resolved names, type constructors, generic
   instantiations, capability sets, target facts, proof obligations, report
   facts, diagnostics, generated names, source nodes, Core nodes, SSA values,
   backend operations, and source spans after desugaring. Human output should
   still show source names; caches, pass hashes, evidence bundles, and replay
   artifacts should use stable IDs.

   Done when the same source + compiler version + target/profile produces
   stable IDs, unrelated edits preserve IDs where possible, generated names use
   the same hygiene source, and a gate proves reports/agents can join facts by
   ID without reparsing human strings.

12. Add a lightweight compiler fact dependency graph.
   Do not build the full rustc-style demand-driven query engine yet, but record
   dependency edges as first-class trace/replay data:
   `source -> parse -> resolve -> type -> ownership -> caps -> mono ->
   corecheck -> lower/ssa -> obligations -> reports -> backend`. Every cached
   or replayable fact must name the source/fact/version/profile inputs it
   depends on. If a fact cannot name its dependencies, it is not eligible for
   cache reuse or evidence replay.

   Done when `concrete trace-pipeline --json` can show why a proof obligation,
   capability fact, diagnostic, or backend artifact changed, and a replay gate
   marks dependent facts stale when an upstream source/fact hash changes. Phase
   6C exercises this graph in cache-free shadow mode; Phase 8.5, after the
   external GO verdict, is the only owner of persistent reuse and scheduling.

13. Add pass locality and mutation rules.
   Borrow the MLIR lesson: each pass declares what IR level it may inspect, what
   it may mutate, what facts it may write, and what prior stage contract makes
   its assumptions legal. A report/audit pass may not recompute semantic facts;
   a lowering pass may not mutate source-level facts; a function-local pass may
   not inspect sibling functions unless explicitly declared; a proof extraction
   pass may not silently invent checker facts.

   Done when the pipeline-contract gate includes at least one red-team fixture
   for illegal cross-level read, illegal fact write, hidden sibling inspection,
   and report-side semantic recomputation.

14. Add analysis preservation and invalidation contracts.
   Once facts are cached or reused, every pass must state which facts it reads,
   writes, preserves, and invalidates. This is the guardrail that prevents
   Phase 8.5 incremental checking, proof caching, package facts, or editor facts
   from becoming a stale second truth source.

   Done when a pass that changes Core invalidates dependent CoreCheck, proof,
   report, and backend facts; a pass that only changes source spans preserves
   type/capability facts but invalidates source-linked diagnostics; and a gate
   proves stale facts cannot remain green after an invalidating pass.

14a. Define the structured validation-record and certificate-chain contract
    before compiler artifacts become persistently cacheable.

    Today a boundary such as `ValidatedCore` means "the in-process checker ran,"
    while the underlying checker returns `Unit` and the wrapper carries no
    machine-readable account of what was checked. Replace that shape
    incrementally with versioned **producer validation records**. At minimum a
    record names the canonical subject/artifact id and digest, input and
    dependency ids/digests, compiler/schema/target/profile identity, verifier
    and invariant-set version, producer-claimed checked/unsupported predicates,
    diagnostics, source/provenance roots, referenced evidence classes, and
    replay command. The **consumer boundary**, not the producer, owns the
    required predicate set for an invariant-set version: omitting a required
    predicate cannot still construct the boundary. CoreCheck,
    post-Mono verification, SSA verification, and later BackendIR verification
    should return or attach these records rather than only `Unit`.

    Terminology is load-bearing: a producer record from the same compiler is
    in pipeline state `compiler_validated`; that is not an evidence class, may
    not satisfy a proof/package/verified-profile policy, is not independently
    certified, and does not make the compiler untrusted. Reserve
    `certificate_structurally_checked` for a
    predicate recomputed by Phase 14's independent checker,
    `translation_validated_v1` for Phase 15's checked input/output relation, and
    `kernel_replayed` for an artifact-specific theorem actually replayed by
    Lean's kernel. Never emit an unqualified `certified` status. Unsupported
    regions remain `unsupported`, `translation_trusted`, or `trusted`.

    Define the chain shape now so Phase 8.5 can serialize/cache without inventing
    a cache-only truth source:
    `source/input roots -> typed Core + relational facts -> obligations/proofs ->
    mono/SSA -> BackendIR -> emitted/link artifact`. Each edge names the exact
    dependency relation, invalidation owner, and relation status:
    `identity_bound_only`, `producer_validated`, or the specific independently
    checked predicate/relation. A digest proves identity only for dependencies
    present in the chain; merely naming an edge does not validate its claimed
    relation, prove dependency completeness, or prove semantic correctness.

    Deliverable: `docs/COMPILER_CERTIFICATES.md`, versioned record types, and
    `scripts/tests/check_validation_records.sh`. Gate missing invariant rows,
    subject/certificate swapping, changed dependency hashes, duplicate ids,
    unsupported predicates mislabeled as checked, and a downstream artifact
    whose validation record does not bind the exact upstream digest. Include a
    coordinated-forgery negative where artifact, producer record, digests, and
    dependency list agree internally while encoding a false semantic claim;
    the result must remain only producer/compiler-trusted rather than acquiring
    an independent status. Canonical
    cross-process serialization and cache activation remain Phase 8.5;
    independent checking remains Phase 14/15.

### Value Flow, Builtins, Reports, And Pipeline Composition

These items connect semantic facts to lowering, intrinsics, reports, and the
single composed compiler pipeline.

15. Define Concrete's result-location / destination-passing model.
    Adapt Zig's result-location idea to Concrete's linear value model. Write the
    Concrete-specific contract for value context, place context, borrow context,
    callArg context, destination context, return destination, aggregate literal
    destination, destructure destination, assignment destination, and scoped
    callback destination. This should explain how struct/array literals move
    fields/elements into destinations, how returns move into caller-visible
    storage, why by-value projection of a non-Copy subplace is rejected, and how
    parameter-directed pass agreement interacts with borrows and reborrows.

    This model should feed `OwnershipJudgment` / `ValueFlowJudgment`, not be
    rediscovered during Lower: aggregate construction, returns, destructures,
    assignments, and scoped callbacks should carry destination/pass facts from
    Check through Elab into Core or into future `CompilerDB` edge facts when
    they are relational. Done when `docs/VALUE_FLOW_SPEC.md` or a new
    destination-passing note names the modes, the checker/lowerer tests include
    aggregate construction, return, destructure, assignment, borrow, callArg,
    and scoped callback rows, and at least one old H10/H11-style conservation
    bug is expressible as a violated destination/ownership contract.

16. Add a single builtin/intrinsic registry.
    Builtins and intrinsics must not be separately described in signatures,
    checker behavior, elab/lower special cases, interpreter behavior, report
    facts, stdlib assumptions, and Unsafe/trusted policy. A registry row should
    define source spelling, type/signature, required capabilities, safety/trust
    class, lowering behavior, interpreter behavior, runtime-trap behavior, and
    report/evidence facts.

    Gate with one arithmetic helper, one bounds/trap helper, one allocation or
    collection builtin, one unsafe/trusted intrinsic, and one stdlib-facing
    builtin proving every consumer reads the same registry facts. The first
    slice may be an intrinsic coverage/matrix gate; the end state is a registry,
    not a grep-based allowlist.

17. Move report/evidence output toward `CompilerDB` fact views.
    Internal facts should be typed records until the final renderer, not strings
    assembled at each report site. V1 target: diagnostic-code facts, capability
    facts, runtime-trap facts, Copy/linear facts, trusted/Unsafe facts, proof
    status facts, and package/import facts. Reports, diagnostics, audit, replay,
    PR diffs, editor hovers, and agent JSON should render the same typed records
    / `CompilerDB` views rather than recompute meaning from strings.

    Gate that a new diagnostic/evidence fact cannot be emitted without a schema
    row and report entry, and add one negative where a stale hand-written report
    string disagrees with the compiler fact.

18. Eliminate hidden second pipelines.
    Production compile, tolerant diagnostics, verify gates, fuzzers, audit,
    proof extraction, and tests must call the same composed pass functions or
    declare a checked reason for diverging. The specific risk is a verifier
    checking a path production does not use, or a fuzzer exercising a different
    mono/lower path than shipped code.

    Deliverable: a single pipeline composition layer exposing strict, tolerant,
    audit, proof, and gate modes as configured variants over the same stages.
    Gate with one fixture that would fail if `runVerifyGates` or the fuzzer
    re-runs mono/lower differently from production.

### Coverage, Fuzzing, And Frontend Preservation

These items prevent new syntax, constructors, parser forms, generic surfaces, or
desugar rewrites from bypassing the semantic contracts above.

19. Enforce the name-resolution / qualified-name boundary.
    Resolve should be the only stage that decides what a source name means.
    Later stages may consume resolved IDs / qualified names but must not
    reconstruct lookup from bare strings. This matters for modules, aliases,
    methods, traits, generated mono names, report attribution, package facts,
    and stable fact IDs.

    Gate with ambiguous imports, import aliases, methods with the same bare
    name, generated mono names, and report/audit attribution proving later
    stages use resolved identity.

20. Unify tree-walker coverage across AST/Core/SSA consumers.
    There are recursive walkers in Check, Verify, Mono, ProofCore, Report,
    Lower, interpreter, and tests. Every new constructor risks one walker
    missing it. Keep the constructor-coverage manifest/gate proving every
    language/Core constructor is handled by checker, elab, mono, proof
    extraction, report/audit, interpreter, lowering, value-flow spec, and
    differential/oracle coverage or an explicit compile-only/proof-only
    rationale.

21. Strengthen CoreCheck into the hard "no invalid Core proceeds" boundary.
    CoreCheck should catch frontend/checker/elab/mono misses before Lower. Add
    or verify checks for unresolved generic/typevar leakage after mono, illegal
    Copy specialization, mixed-width binops, illegal non-Copy value-flow residue,
    unresolved capability-polymorphic calls, unsupported unsafe/trusted ops
    without facts, and user-triggerable layout/type holes that can become Lower
    panics. Gate with one accepted Core fixture per valid class and one rejected
    fixture per invalid class.

22. Add the third fuzzer: generics / monomorphization / type-policy drift.
    Concrete already has value/runtime fuzzing (`fuzz_differential`) and
    ownership/conservation fuzzing (`fuzz_linearity`). Add
    `scripts/fuzz_generics.py` (or equivalent) for user structs/enums,
    `Option<T>` / `Result<T,E>`, conditional `Copy`, trait bounds, turbofish
    calls, nested generic fields, arrays/newtypes, method impls, multiple
    instantiations, and mono-name collisions.

    The oracle is phase agreement first, not program output: Check, Elab/Core,
    Mono, CoreCheck, Verify, reports, and generated names must agree on Copy,
    trait-bound satisfaction, substitution, capability requirements, and whether
    a specialization is Copy or demoted to linear. Negative variants should omit
    bounds, instantiate `Copy` containers with linear payloads, shadow type
    variables, collide mono names, or use caller type params through turbofish.
    Done when the fuzzer rediscovers a seeded historical bug and saves
    disagreements as minimized `.con` fixtures naming the first disagreeing
    phase.

23. Add parser/AST grammar-conformance fuzzing.
    The LL(1) checker proves grammar shape, but it is not a parser/AST stress
    net. Generate valid grammar-shaped programs, expected rejects, deeply nested
    but bounded expressions, pattern/control-flow combinations, module/import
    shapes, generic syntax, and statement/expression boundary cases. The oracle:
    parser accepts the valid corpus, rejects the invalid corpus with stable
    diagnostics, preserves source spans for representative nodes, and produces
    AST forms whose feature rows are covered by item #5. When a formatter or
    printer exists, extend this to parse -> print/normalize -> parse round-trip.

24. Add a desugar-preservation contract.
    Desugar is a semantic compiler stage, not an invisible parser cleanup pass.
    It rewrites surface forms such as `for`, `if let` / `while let`, scoped
    blocks, trailing-value blocks, `defer`, destructuring sugar, and future
    convenience syntax into the core source forms that Check and Elab consume.
    If a desugared program behaves differently from the equivalent hand-written
    core form, every later stage can be perfectly correct and still compile the
    wrong program. This contract makes that stage auditable.

    The contract should state, per desugaring: the source construct, the target
    core form, the evaluation order it preserves, how source spans/provenance are
    assigned to synthetic nodes, how ownership/drop/trap sequencing is preserved,
    and which facts later stages are allowed to assume came from desugar rather
    than user syntax. Synthetic nodes must be visible to diagnostics/reports as
    compiler-generated provenance, not as a second source program, and they must
    not create authority, ownership movement, or trap behavior that the source
    construct did not specify.

    Gate with paired fixtures: each surface construct and its hand-written core
    expansion must agree under Check, interpreter, compiled output, source-span
    diagnostics, ownership/drop behavior, and capability/trap behavior where
    applicable. Include at least one red-team per high-risk family: `defer`
    cleanup order, destructuring of non-`Copy` values, `if let` / `while let`
    pattern failure, loop `break` values, trailing-value blocks, and source spans
    on synthetic failures. A mutation that changes desugar order, drops
    provenance, or introduces an extra move/drop/trap must fail.

### Diagnostics, Spans, Names, And Failure Boundaries

These items make failures reviewable and keep compiler-internal artifacts from
leaking as user-facing truth.

25. Define diagnostic code ownership by phase.
    The diagnostic ledger completeness gate prevents missing codes; now each
    phase/category should own a code range or category (parse, check, corecheck,
    mono, lower, proof, report, runtime trap), with source, hint, JSON payload,
    and report behavior documented. Gate that a new diagnostic code declares
    owner/category and appears in the ledger.

26. Add a pipeline diagnostic-quality contract.
    Every user-facing pipeline diagnostic must have a phase owner, stable code,
    primary source span, human message, machine-readable JSON fields, and a
    clear reason why that phase owns the rejection. When an actionable recovery
    exists, include a hint; when the failure came from a fuzzer/gate/
    counterexample, include a replay command. Internal generated names,
    sentinels, LLVM/linker text, or Lean panic details must not leak into the
    main user message unless explicitly marked compiler-internal context.

    Done when a diagnostic-quality gate samples parse, check, mono/corecheck,
    lower/SSA, emit/backend, proof/report, and runtime-obligation failures in
    human and JSON output, and a red-team fixture fails when an error lacks a
    source span, code, hint, owner, or JSON field.

27. Add source-span preservation audits across the pipeline.
    After lexer/parser/check/elab/mono/lower, diagnostics, reports, traps, and
    proof obligations should still point to original source. Gate representative
    constructs: submodules, generic instantiations, desugared `if let`/`while
    let`, method calls, match arms, lowered runtime traps, and generated proof
    obligations. A fixture with a stale main-file span must fail.

28. Encapsulate generated names and stringly internal sentinels.
    Internal names such as `__last_expr`, `__destr_*`, mono suffixes, generated
    temporaries, and lowered helper names must be produced through one hygienic
    naming API. Mono, elab, lower, proof extraction, and reports should use the
    same API. Gate that user-defined names cannot collide with generated names,
    and that generated names round-trip through reports/source maps without
    becoming user-visible authority or value-flow facts.

29. Define the panic-to-diagnostic boundary.
    `panic!` in layout/type/lowering paths is acceptable only for compiler
    invariant violations, not user-triggerable programs. Add a gate that runs
    malformed/internal-edge programs through parser/check/elab/mono/corecheck/
    lower and asserts user-triggerable cases return diagnostics rather than
    Lean panics. Include hostile malformed inputs that previously leaked to
    backend/linker errors, exhausted recursion, or consumed excessive compile
    memory/time before reporting a diagnostic. Any remaining `panic!` must be
    documented as unreachable after a named prior phase contract.

### Targeted Structural Follow-Ups And Exit Artifact

These are deliberately scoped follow-ups: only pull them when the earlier gates
or proof work show that the structure is now the limiting factor.

30. Extract a structured Lower/SSA control-flow builder if branch/loop/phi logic
    remains ad hoc.
    Historical bugs clustered around branch/loop/snapshot reconciliation. The
    extracted builder should own if/match result values, loop headers/exits, phi
    construction, scope-local variable filtering, internal trailing-value
    handling, and defer cleanup boundaries. Acceptance gate: previous nested
    match, loop-after-loop-branch, `__last_expr`, defer, and value-block
    fixtures remain green, and a mutation that drops scope filtering or trap
    preservation is caught.

31. Reduce ProofCore partial-def opacity only where proof preservation needs it.
    ProofCore still contains many `partial def` walkers/wrappers. Do not chase a
    full rewrite here. Add non-partial wrappers or structural recursion only for
    constructs pulled by Phase 12/14 preservation proofs. Gate each lifted rule
    with one theorem that would fail if the wrapper delegated to an opaque
    partial-def shape.

32. Add the Phase 6B / 6.5 validation artifact.
    `scripts/tests/check_pipeline_refactor_contract.sh` runs a small
    compiler-pipeline corpus exercising arithmetic reference semantics,
    central policy predicates, `CopyJudgment` / `InstantiationJudgment`,
    `OwnershipJudgment` / `ValueFlowJudgment`, `TotalityJudgment`,
    judgment-contract enforcement, stage contracts, capability facts, stable fact
    IDs, fact dependencies, pass locality, invalidation, structured producer
    validation records, the versioned certificate-chain contract,
    certificate-carrying IR, destination-passing/value flow, intrinsic registry,
    `CompilerDB`,
    no-hidden-second-pipeline checks, walker coverage, CoreCheck boundary
    failures, feature matrices, source spans, generated-name hygiene,
    diagnostic quality and panic-to-diagnostic behavior. The artifact must prove
    these refactors are behavior preserving for accepted programs and fail closed
    for malformed or unsupported programs.

## Phase 6C / 6.c: Pipeline Observability, Replay, And Scaling

Goal: make the already-unified pipeline inspectable, replayable, and scalable
under real workloads without keeping Phase 6B open-ended.

Done when: compiler-owned telemetry, scaling checks, trace output, minimized
counterexamples, gate mutation tests, pass-output replay artifacts, and a
cache-free shadow query/invalidation manifest exist as tooling around the
pipeline. Phase 6C must not add new semantic truth sources or reuse compiled
results; it observes and replays facts owned by Phase 6B and later phases.

This phase was split out of Phase 6B because these tools are valuable but do not
gate Phase 6B's core criterion: one opinion per program. Keep the minimal
Phase 6B fact-provenance requirement there (explain where a fact was introduced
or lost); put the heavier trace/replay/scaling machinery here.

1. Add pass timing, memory, and IR-size telemetry.
    This is not a public performance claim. Record per-pass timing, peak memory
    / RSS when available, allocation or output-buffer size if available,
    module/function counts, AST/Core/SSA node counts, mono instantiation count,
    obligation count, runtime-trap count, and report size in a JSON trace. Gate
    only schema stability, absence of private paths, and graceful absence of
    platform-specific memory fields; benchmarks/performance claims remain Phase
    17 release work.

2. Add an anti-superlinear compiler complexity guard.
    Telemetry tells us what happened; this gate fails when a compiler pass
    quietly becomes quadratic or worse on ordinary generated programs. Build a
    scaling corpus that generates the same program family at multiple sizes:
    large array literals/repeats, many functions, many blocks, many mono
    instantiations, wide match/if chains, and large emitted modules. Compare
    slope/ratio across sizes, not absolute wall-clock alone, and include peak
    memory/RSS or output-buffer growth. Done when the gate catches a
    deliberately reintroduced quadratic renderer/collector, an intentionally
    excessive memory-growth variant, and the bug 027 family.

3. Add `concrete trace-pipeline --json`.
    Dump per-stage summaries: modules, functions, diagnostics, capabilities,
    obligations, mono instances, CoreCheck status, SSA blocks, runtime traps,
    trusted/Unsafe facts, dependency edges, fact IDs, invalidation decisions,
    and replay commands for failures. Gate with one clean program and one
    failing program where the trace names the first phase that introduced,
    preserved, invalidated, or rejected the relevant fact.

4. Add counterexample-first pipeline debugging.
    Any pipeline panic, proof failure, fuzzer mismatch, optimizer trap issue,
    backend leak, linker leak, or stage-contract failure should reduce to a
    minimized `.con` fixture plus a replay command. Reuse the existing reducer
    where possible. Gate with one panic-to-diagnostic failure, one
    interp-vs-compiled mismatch, one fold/trap-preservation failure, and one
    backend/linker leak proving the counterexample can be saved as a regression.

5. Add mutation testing for the pipeline gates.
    Mutate or patch-disable one representative rule in each major family and
    prove the corresponding gate fails: CoreCheck type rule, Copy predicate,
    arithmetic trap preservation, capability requirement, walker constructor
    handling, source-span stamping, generated-name hygiene, diagnostic-quality
    contract, fact invalidation, and report/evidence schema row. This proves the
    pipeline gates are load-bearing instead of decorative.

6. Add round-trip/replay artifacts for pass outputs.
    For a selected program, users and agents should ask what each stage
    received, what it emitted, what facts changed, and what command replays that
    claim. V1 artifact: per-stage summary hashes and optional minimized dumps
    for AST, Core, post-mono Core, CoreCheck facts, SSA, obligations, traps, and
    report facts. Same source, compiler version, target triple, profile, and
    dependency set must produce stable pass-output hashes unless a field is
    explicitly marked nondeterministic and excluded from the hash.

7. Add a cache-free incremental shadow manifest and edit corpus.
   `concrete trace-pipeline --incremental-shadow <prior-manifest> --json` (or an
   equivalent internal test surface) records the future typed query key/kind and
   owner, semantic input digest, dependency keys/digests, output digest,
   cacheability reason, validation-record id, and
   `would_reuse | would_invalidate` verdict. It must still recompute the fresh
   batch result every time; Phase 6C stores manifests and hashes, never reusable
   compiler artifacts. Compare every predicted hit with the recomputed digest;
   a false hit is a hard failure and a missing dependency edge must be
   minimizable to the responsible edit.

   Cover no-op, comment/span-only, private-body, public signature/type,
   capability/contract/spec, generic instantiation, target/profile/policy,
   file add/delete/rename, dependency-interface, and error-then-repair edits.
   The first model may conservatively over-invalidate. It may become finer only
   when a gate demonstrates dependency completeness for that query family.
   Mutate away one import, call, proof, policy, and target dependency edge and
   prove the shadow comparison catches every false reuse. Wire
   `scripts/tests/check_incremental_shadow.sh`.

8. Add the Phase 6C validation artifact.
   `scripts/tests/check_phase6c_observability.sh` runs telemetry, complexity
   slopes, trace/replay, minimization, mutation testing, deterministic pass
   hashes, and the shadow edit corpus over a small project plus one Phase 8
   workload. It publishes the cold baseline, predicted invalidation sets, and
   query-family census that Phase 8.5 must use; it does not enable caching.


## Phase 7: Standard Library And Core APIs

Goal: build the small standard library people need before real workloads,
packages, editor tooling, freestanding targets, and release work can be honest.

Done when: a normal C/Rust-style Concrete program can use documented core APIs
for errors, bytes/text/path, collections, formatting/parsing, I/O capabilities,
tests, and oracle helpers, with every public stdlib item carrying an evidence
class and authority/allocation story.

Phase 7 priority order is deliberately narrower than the comparison languages:
make Concrete's small core pleasant and auditable before copying broad
batteries-included breadth. The ranked build order is:

1. `Option`/`Result` ergonomics and recoverable-vs-fatal conventions.
2. Allocator-as-value research before allocator-heavy collection APIs harden.
3. Buffered `Reader`/`Writer` and file/byte-stream IO.
4. `Path`/filesystem APIs.
5. `String`/`Text`/`Bytes` coherence.
6. Collections phase 2 on the callable-values surface.
7. `std.test` tests-as-docs and doc-snippet gates.
8. CLI/env/process helpers for real tools.
9. Unsafe/trusted boundary wrappers, trap/debug UX, and verified-profile/
   proof-obligation UX.
10. Proof-facing formal stdlib models (`formal_vec`, `formal_map`,
    `formal_set`, `bigint`, lemma helpers) once contracts need them.
11. Broad compression/crypto/networking/threading only after workload demand.

1. Define the stdlib gap matrix against Gleam, Zig, Rust, Go, Odin, Ada/SPARK,
   Lean, and Roc before expanding APIs. Concrete should not copy any one
   surface. It should make a Gleam/Roc-sized core pleasant, keep Zig-style
   allocation/authority explicit, preserve Rust-style core/alloc/hosted
   layering where useful, copy Ada/SPARK's evidence-class discipline, use Lean
   as proof-infrastructure guidance, and treat Go/Odin as breadth references
   for later, not as v1 size targets. Record which modules are core-now,
   hosted-now, freestanding-later, package-later, or research-later. The matrix
   must name the obvious external references so they are not forgotten:
   Gleam's `List`/`Dict`/`Set`/`String`/`BitArray`/`BytesTree`/`StringTree`/
   `Uri`/`Dynamic.Decode`; Roc's `Str`/`List`/`Dict`/`Set`/`Iter`/numeric
   builtins; Zig's `ArrayList`/maps/sets/sort/fmt/json/base64/Uri/fs/process/
   time/random/hash/crypto/log/testing/allocators/atomics/threads/compress/
   archive/target/OS/debug-format surface; Rust's `core`/`alloc`/`std`,
   `Option`/`Result`, slices, `Vec`, maps, `io`, `fs`, `path`, and `time`;
   Go's `bufio`/`io`/`os`/`path`/`encoding`/`testing`/`crypto`/`compress`
   breadth; Odin's base/core/vendor split and data-oriented package layout;
   Ada's predefined strings, containers, IO, numerics, and command-line
   units; SPARK's formal/functional containers, big numbers, IO restrictions,
   and lemma libraries; and Lean's proof/library organization.
2. Define stdlib module layout and naming: core/prelude, bytes/text/path,
   collections, numeric helpers, formatting/parsing, console/file/network/
   process/time, test/oracle helpers, and target-profile-specific modules.
   Start from this explicit module checklist so none of the Concrete names or
   comparison-driven additions disappear:
   - Existing core/result modules: `std.option`, `std.result`.
   - Existing numeric/memory modules: `std.numeric`, `std.math`, `std.mem`,
     `std.ptr`, `std.alloc`.
   - Existing byte/text/path modules: `std.bytes`, `std.slice`, `std.string`,
     `std.text`, `std.ascii`, `std.path`.
   - Existing format/parse/data modules: `std.fmt`, `std.parse`, `std.hex`,
     `std.sha256`, `std.hash`, `std.rand`, `std.time`.
   - Existing collection modules: `std.vec`, `std.map`, `std.set`,
     `std.ordered_map`, `std.ordered_set`, `std.deque`, `std.heap`,
     `std.bitset`.
   - Existing hosted/capability modules: `std.io`, `std.writer`, `std.fs`,
     `std.env`, `std.args`, `std.process`, `std.net`, `std.libc`.
   - Existing test module: `std.test`.
   - Proposed iterator/builder modules: `std.iter`, `std.builder`.
   - Proposed collection helper modules: `std.sort`, `std.search`.
   - Proposed data/encoding modules: `std.checksum`, `std.base64`,
     `std.uri`, `std.json`, `std.decode`, `std.bin`.
   - Proposed package/config modules: `std.semver`, `std.config`.
   - Proposed CLI/user-output modules: `std.cli`, `std.log`, `std.progress`.
   - Proposed security/text-policy modules: `std.ct`, `std.unicode`.
3. Stabilize `std.option` and `std.result`: `Option<T>`, `Result<T, E>`,
   construction, matching helpers,
   fallible chaining, ignored-result behavior, test helpers, and audit facts
   for fallible returns. ✅ CONDITIONAL COPY landed 2026-07-05: `Option<T>` /
   `Result<T, E>` / Copy-marked generic structs are Copy iff every substituted
   field/payload is Copy (`isCopyType` evaluates instantiations; Mono demotes
   non-qualifying specializations to linear instead of erroring; `if let` /
   `while let` on `Option<Copy>` is legal for the right reason — the linear
   `_` rule unbent). Fixtures: `conditional_copy.con`,
   `error_conditional_copy_{option,result}.con`. Include the small ergonomic floor before real workloads
   work around the canonical error types: `map_err`, `and_then`, `unwrap_or`,
   `ok_or`, and any `unwrap_or_else`/callback form that remains explicit about
   control flow and capabilities. If `?` gains conversion behavior, its
   lowering and reports must remain explicit: no hidden allocation, no hidden
   capability gain, and no silent conversion of one error set into another.
3a. ✅ **DONE (2026-07-05) — conditional `Copy` for generic containers without
   weakening linearity.** `Option<T>` is `Copy` iff `T` is `Copy`; `Result<T, E>`
   is `Copy` iff both `T` and `E` are `Copy`; Copy-marked generic structs are
   demoted to linear for non-qualifying instantiations. This is a convenience
   and correctness-of-classification item, not an affine escape hatch:
   non-`Copy` values still cannot be silently discarded, and `_` may ignore a
   generic value only when the instantiated whole type is actually `Copy`.
   Gates include positives for `Option<i32>` / `Result<i32, i32>` Copy behavior
   and negatives for `Option<String>`, `Result<String, E>`, and nested
   resource-owning payloads. Future generic containers must state their
   conditional-`Copy` rule structurally and preserve the invariant that no
   `Copy` value contains a non-`Copy` owner.
4. Build raw-data APIs in `std.bytes` and `std.slice`: `Bytes`, byte slices,
   fixed buffers, parser cursors,
   byte-preserving formatting, and no implicit UTF-8 or lossy conversions.
5. Build validated text APIs in `std.text`, `std.string`, and `std.ascii`:
   `Text`, `String`, UTF-8 validation, slicing/indexing rules, formatting,
   parse helpers, diagnostics, and explicit conversion from/to raw bytes.
   Decide the `std.bytes.Bytes` / `std.text.Text` / `std.string.String` split
   before `std.json`, `std.uri`, `std.path`, or `std.net` examples depend on
   it.
6. Define the Unicode policy in proposed `std.unicode` before text-heavy APIs
   grow: validation, scalar/value iteration, normalization stance, case
   folding stance, width/display policy, and which pieces are core-now versus
   package-later. Do not silently normalize or case-fold text.
7. Build path and OS-string APIs in `std.path`: `Path`, `OsString`,
   byte-preserving platform
   boundaries, normalization assumptions, symlink/TOCTOU notes, and explicit
   failure modes.
8. Build collection APIs across `std.vec`, `std.map`, `std.set`,
   `std.ordered_map`, `std.ordered_set`, `std.deque`, `std.heap`,
   `std.bitset`, and `std.slice`: fixed arrays/slices, `Vec<T>`, maps, sets,
   buffers, parser cursors, and capacity-aware helpers. Add the ordinary APIs
   C/Rust users expect: `contains`, `remove`, `iter` for maps and sets;
   `insert`, `remove`, `retain`, `sort`, `search` for vectors and slices;
   stable ordering helpers for ordered collections. Each API must state whether
   it requires `with(Alloc)`, whether it can fail, and which runtime
   obligations it creates.
   - 8a. Keep `Clone` and indexed move/swap as **workload-gated value-model
     research**, separate from H1 and not assumed inevitable. If admitted,
     `Clone` is explicit semantic duplication (capability-visible, usually
     `with(Alloc)`, audit-visible, and not silently proof-eligible), not a
     convenience patch for borrowed reads. The sibling move-out primitive for
     indexed containers is `swap(i, new) -> V`, which transfers ownership out
     while preserving the linear one-value-per-slot invariant. Build either only
     after a real workload repeatedly needs owned duplication or indexed
     ownership-out and the existing `Copy` / move / borrow APIs are the wrong
     fit.
   - 8b. Design arena/index safety before any arena-backed structure becomes a
     flagship or stable stdlib API. Array-backed linked structures use indices,
     and a stale index into a removed/reused slot is a logic-level dangling
     pointer that ordinary ownership does not see. V1 answer should compare
     typed index newtypes (`NodeId`, not `u64`), generation-counted slots,
     arena validity invariants, and proof obligations that inject newtype
     invariants as scoped hypotheses. Add
     `docs/ARENA_INDEX_SAFETY.md`, `examples/arena_indices/{stale_index,generation_checked,typed_node_id}/`,
     and `scripts/tests/check_arena_index_safety.sh`; the gate must prove stale
     index reuse is rejected, trapped, or explicitly trusted, never silently
     treated as ordinary memory-safe access.
   - 8c. Decide explicit-dictionary coherence before binary collection
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
   - 8d. Keep `with_value_mut` / `modify` parked, not scheduled. Mutable scoped
     callbacks have a receiver/context aliasing hazard (`&mut self` plus a
     context that can reach the same container); ship them only if a workload
     requires single-element mutable borrowed access and a
     container-not-in-context gate enforces the invariant.
   - 8e. Keep scalar `from(param)` returned references deeply deferred and
     evidence-gated. If ever added, they stay flat and scalar: no `Option`,
     `Result`, structs, arrays, containers, callback contexts, or generic
     wrappers. This is a research escape valve only if real workloads prove
     operation APIs, owned views, and scoped callbacks insufficient.
   - 8f. Pull narrow const generics forward only when fixed-capacity stdlib APIs
     need reusable capacities. `docs/CONST_GENERICS_V1.md` is the closed Phase 5
     design record; implementation is deferred until a real Phase 7 consumer
     appears (`BoundedVec<T, N>`, `RingBuffer<T, N>`, `PacketBuf<N>`, fixed hash
     table, parser scratch buffer, freestanding reusable buffer, or a
     capacity-indexed proof API). When triggered, implement the staged V1 from
     that doc and wire `scripts/tests/check_const_generics_v1.sh` to prove
     distinct capacities specialize separately, layout is capacity-specific,
     runtime-safety obligations name the instantiated size, and unsupported
     comptime/reflection/runtime-bound forms are rejected.
   - 8g. Research **allocator-as-value** before allocator-backed collections,
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
9. Build iterator and builder APIs in proposed `std.iter` and `std.builder`
   after the collection shape is known: `Iter<T>`-style adapters,
   `fold`/`map`/`filter`/`take`/`drop`, known-length reporting, REVERSE
   traversal (`rev_fold`/`rev_for_each` — today every backwards walk is a
   manual index-decrement loop; extend `docs/ITERATION_PROTOCOL.md` when these
   land), byte/text builders, and tree/buffer builders inspired by Gleam's
   `BytesTree` and `StringTree`. Do not hide allocation; builder APIs either carry
   `with(Alloc)` or operate over fixed buffers.
10. Build numeric helper APIs in `std.numeric`, `std.math`, and `std.mem`:
   checked/wrapping/saturating arithmetic helpers,
   narrowing/conversion helpers, endian conversions, byte/word packing, and
   evidence classes for each helper.
11. Build sorting and searching primitives in proposed `std.sort` and
    `std.search`: comparison conventions, stable/unstable sort decision,
    binary search, min/max helpers, and evidence/oracle tests over edge cases.
12. Build hashing, checksums, and deterministic random helpers in `std.hash`,
    proposed `std.checksum`, and `std.rand`: stable hash APIs for maps/sets,
    non-cryptographic checksums, seeded deterministic RNG for tests/oracles,
    and a clear split from cryptographic randomness. Any OS entropy source is
    hosted-only and capability-visible.
13. Build constant-time helper APIs in proposed `std.ct` only for narrow,
    auditable cases: equality/compare over fixed-size bytes, no secret-dependent
    branches or early exits, source-shape audit evidence, and clear
    machine-level timing assumptions. This belongs to the narrow security
    surface, not broad crypto.
14. Build time and duration helpers in `std.time`: monotonic versus wall-clock
    distinction,
    timestamp formatting/parsing if admitted, timeout helpers, and explicit
    hosted authority for reading the clock.
15. Build formatting and parsing helpers in `std.fmt` and `std.parse`:
    integer/text formatting, simple
    scanners, structured parse results, error-set reports, and oracle-friendly
    output conventions.
16. Build a reusable scanner/parser core in `std.parse` over
    `std.bytes.Bytes` and `std.text.Text`: `peek`, `advance`, `take_while`,
    `consume`, span/position tracking, error reporting, and no hidden
    allocation unless the API carries `with(Alloc)`.
17. Add `std.base64` as the first byte-format module: encode/decode, streaming
    encode/decode over fixed buffers and allocation-backed `Vec<u8>`, strict
    error reporting, RFC 4648 vectors, invalid-padding negatives, oracle
    comparison, and evidence classification. Streaming encode/decode is
    explicitly deferred to package/workload pressure, not hidden in v1.
18. Add `std.uri` parsing/formatting after the byte/text/path split is stable:
    component accessors, percent encoding/decoding, normalization policy, and
    clear distinction between syntax validation and network authority.
19. Add `std.json` as the first structured data module: tokenization,
    string/number handling, error spans, bounded recursion policy, optional
    DOM-like representation only if the allocation story is explicit, and
    oracle tests against a reference implementation.
20. Add a small typed decoding layer in proposed `std.decode` after
    `std.json`: dynamic value decoding, field access, error paths, and examples
    comparable to Gleam's `dynamic/decode`, without broad reflection or hidden
    runtime typing.
21. Add binary serialization helpers in proposed `std.bin`: endian-aware
    reading/writing, fixed-width integers, length-prefixed fields only with
    explicit bounds, byte-span diagnostics, and no hidden allocation.
22. Add semantic-version and config-format helpers in proposed `std.semver`
    and `std.config` if package/build work starts depending on them:
    `SemVer`, INI/TOML-style scanner, and manifest parsing support. These are
    stdlib/package-boundary helpers, not general metaprogramming.
23. Add command-line parser helpers in proposed `std.cli`: flags, positional
    arguments, usage text, typed parse errors, no ambient environment access
    except through `std.args`, and examples that keep authority visible.
24. Add simple logging/diagnostic output APIs in proposed `std.log`: levels,
    writers, formatting
    integration, capability requirements, and policy for release builds. Keep
    this small; it is not a tracing framework.
25. Add progress/status output helpers for CLI tools in proposed
    `std.progress` only after `examples/daily/word_count` and
    `examples/stdlib_workloads/base64_cli` need visible progress output. V1
    surface: `ProgressWriter`, `quiet`, `verbose`, `set_total`, `advance`,
    `finish`, terminal detection through an explicit console handle, and no
    ambient terminal authority. Wire
    `scripts/tests/check_stdlib_progress.sh` when the module is admitted.
26. Build capability-scoped console, file, network, process, and time APIs in
    `std.io`, `std.writer`, `std.fs`, `std.env`, `std.args`, `std.process`,
    `std.net`, and `std.time`. Authority must be visible in function types and
    audit reports; no API may smuggle ambient authority through a convenience
    wrapper.
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
27. Build handle-relative filesystem APIs in `std.fs` as the preferred
    file/path shape: directory/file handles carry authority; operations are
    relative to handles for `open`, `create`, `read`, `write`, `metadata`,
    `remove`, `rename`, and `list`. Ambient absolute-path helpers must be
    hosted-only convenience wrappers with explicit authority. Temp-file,
    symlink, path-normalization, and TOCTOU behavior must appear in
    `docs/stdlib/STDLIB_GUIDE.md` and `scripts/tests/check_stdlib_fs.sh`.
28. Build handle-based network surface in `std.net` only as far as the
    validation workloads require: address parsing, socket handle wrappers, HTTP
    header parsing as a pure parser first, and no full HTTP client/server until
    package/workload evidence demands it.
29. Build stdlib test/oracle helpers in `std.test`: expected failures,
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
29a. Add a public stdlib API snapshot/diff before the surface freezes.

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
30. Define stdlib error-handling conventions: when APIs return `Result`,
    `Option`, panic/abort, or require a policy gate; how ignored-result
    diagnostics apply; and how accumulating error sets are reported. Split
    recoverable domain/environment failures from fatal invariant failures:
    `Result`/`Option` for user, file, network, parse, and domain errors that a
    caller can handle; abort/trap for OOM, bounds, arithmetic traps, impossible
    invariants, and explicitly unrecoverable runtime failures. Audit output
    should identify which public APIs can recover, which can trap, and which
    require a policy gate. Do not add a second Zig-style error-union mechanism;
    improve the `Result`/`Option` surface instead.
30a. Define the canonical **consume / destroy / handoff** conventions for
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
31. Define stdlib evidence classes per public API: `proved`, `enforced`,
    `reported`, `tested_by_oracle`, `assumed`, or `trusted`. The evidence class
    must appear in docs and audit artifacts, not just implementation comments.
31a. Add proof-facing formal stdlib models only when a proof or contract
    workload pulls them. These are not runtime containers: `std.formal_vec`,
    `std.formal_map`, and `std.formal_set` model mathematical sequences, maps,
    and sets for contracts, loop invariants, and Lean obligations; `std.bigint`
    models unbounded mathematical integers for specs before any runtime
    big-number API exists; `std.rational` stays deferred until exact-ratio
    specs need it. Each formal module must state its erasure/runtime story, its
    evidence class, the Lean artifact it lowers to, and the refinement relation
    to runtime containers such as `Vec`, `HashMap`, `OrderedMap`, and `HashSet`.
32. Add stdlib authority/allocation/runtime-obligation gates so core helpers
    cannot silently widen capabilities, allocation behavior, trusted
    assumptions, or runtime-risk obligations.
33. Add sanitizer/runtime-instrumentation hooks as debug and validation
    surfaces, not proof evidence. Inspired by Odin's sanitizer-facing base
    surface, Concrete should expose named hooks for bounds, overflow,
    use-after-free-like trusted/FFI probes where applicable, allocator checks,
    and generated-code validation. Audit output must classify this as
    `runtime_checked`, `tested`, or `instrumented`, never `proved`.
34. Split hosted versus freestanding-ready stdlib modules at the API level:
    no-alloc/no-OS core modules, allocator-backed modules, hosted OS modules,
    and modules that are explicitly unavailable under freestanding profiles.
    The freestanding target implementation still lands in Phase 16.
35. Record deliberately deferred stdlib families so they do not disappear from
    planning: compression/archive formats, broad crypto beyond the narrow
    hash/HMAC/constant-time story, full HTTP client/server, dynamic libraries,
    OS debug formats, atomics, threads, SIMD, target/ABI databases, and
    platform-specific C/POSIX wrappers. Each stays package-later,
    backend-later, freestanding-later, or research-later until a workload
    forces it.
36. Add stdlib docs and examples for C/Rust users:
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
37. Add a stdlib compatibility/oracle corpus under
    `examples/stdlib_compat/`: `fmt_parse_vectors`, `bytes_text_vectors`,
    `path_vectors`, `collection_vectors`, `base64_vectors`, `uri_vectors`,
    `json_vectors`, `semver_vectors`, `sort_search_vectors`,
    `checksum_vectors`, `rand_vectors`, and `cli_io_vectors`. Wire it with
    `scripts/tests/check_stdlib_compat.sh`; every vector must declare exactly
    one mode in `manifest.toml`: `oracle_python`, `oracle_system_tool`,
    `interp_vs_compiled`, `audit_only`, or `negative_expected_failure`.
38. Add real stdlib workload checks before Phase 8 relies on the library:
    `examples/stdlib_workloads/base64_cli`,
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
38b. ✅ **DONE (2026-07-02) — H12 CLOSED.** std migrated fully under the
    front-end checker in three same-day tranches (384 violations → 0) and the
    exemption machinery was deleted; std is checked like any other code and
    `check_submodule_check_coverage.sh` pins it at zero. The burn-down forced
    seven checker fixes (divergence merges, return-path leaks, generic
    field-assign, consume-then-exit, store-conservation, linear rebind,
    outermost-binding merges) and the std API decisions recorded in
    KNOWN_HOLES H12 (closed) and docs/OWNERSHIP_MODEL.md. This completed the
    pre-trial requirement that the stdlib not undercut Concrete's enforcement
    claims: std is now checked by the same front-end rules as user code.
38a. Add a stdlib sentinel/arithmetic audit before broadening hosted APIs. The
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
39. Add the Phase 7 validation project:
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

1. Define one typed query/artifact contract and one `CompilerSession` driver.
   Add a closed `QueryKey` family and typed result envelope containing subject
   identity, canonical output digest, dependency keys/digests, validation-record
   identity, diagnostics/fact ids, provenance, cacheability, evidence class,
   replay command, and dependency-root digest. Required initial families:

   - file: source/lex/parse/interface-summary/body;
   - module: resolve/check/elaborate/CoreCheck;
   - function or explicit SCC: calls, ownership/capabilities/effects, ProofCore,
     obligations, discharge inputs, and typed report facts;
   - monomorphized instance: stable definition id + canonical type arguments;
   - codegen unit: lower/SSA verify/backend emission/object;
   - project: module graph, reachability, policy aggregation, link plan, and
     release/bundle view.

   Expose `--incremental=off|on|verify` (or equivalent internal modes). Strict,
   tolerant, audit, proof, test, report, and codegen operations are demand sets
   over the same stage functions, never alternate pipelines. Formatting,
   redaction, proof-tool, target, profile, and policy inputs invalidate only
   query families that declare them. The eager `off` mode remains the oracle
   and debugging path.

2. Add deterministic indexed compiler data and linear-time builders before
   persisting today's hot whole-program scans.
   Introduce measured `ProgramIndex`/`OrderedIndex`-style structures for modules,
   symbols, types, functions, layouts, obligations, stable-id lookup, reverse
   imports/calls, and monomorphized users. Maps/intern tables may serve lookup;
   canonically ordered arrays/lists own iteration, serialization, and output so
   hash-table order never leaks. Use dense arrays where stable ids permit them
   and builders/reverse accumulation for large strings and collections.

   This is profiler-driven, not a blanket ban on `List`: replace hot repeated
   `find?`/`contains`, `acc ++ [x]`, growing-string append, and repeated global
   scans selected by Phase 6C telemetry and bug 027's corpus. Add duplicate-key
   and index-finalization verifiers, plus a mutation that reintroduces a
   quadratic builder and must fail the scaling gate.

3. Make reusable frontend artifact boundaries operational.
   Add or validate durable `ParsedFile`, split `InterfaceSummary` and body
   summary, `ResolvedModule`, checked module/program artifact, elaborated module,
   and validated Core-module boundaries. Check may no longer return only `Unit`
   if downstream reuse needs its ownership/capability/value-flow decisions; it
   must produce the durable facts or typed artifact that Elab and later queries
   consume. A module query depends on its own semantic body plus imported
   **interface** digests, so a dependency's private-body edit does not invalidate
   importers.

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

5. Add an opaque compiler-versioned local content-addressed store under
   `.build/concrete-cache/` (or an equivalent project-local path).
   Keys include compiler build/internal schema, semantic input/dependency
   digests, invariant-set version, target/data layout, build/test profile,
   policy only where consumed, and relevant proof/backend/tool identity. Add
   atomic writes, locking, checksums, interrupted-write recovery, size/GC policy,
   `concrete cache status --json`, and `concrete clean --cache --dry-run`.

   Serialized cache entries are untrusted bytes. Loading must follow an explicit
   `UntrustedCached* -> decode/schema/digest/dependency/subject validation ->
   compiler_validated artifact` API; deserialization cannot directly construct
   `ValidatedCore`, `ValidatedSSA`, a proof result, or a release fact. Before
   Phase 14 this still trusts the producing compiler's validation record—it is
   not independent certification. Unknown schema/compiler/invariant versions,
   truncated content, conflicting ids, or mismatched dependencies cause clean
   recomputation or a loud diagnostic, never a green hit.

6. Turn proof, obligation, and report work into queries rather than cached
   rendered strings.
   Cache per-function ProofCore extraction, call/SCC classification,
   obligations, typed evidence/report rows, and discharge attempts. Project
   reports are deterministic views/joins over those typed facts. A cache hit
   preserves the original evidence class and replay provenance; it never turns
   `tested`, `solver_trusted`, `assumed`, `trusted`, or `compiler_validated`
   into proof. Lean, ProofKit, solver, synthesis-model, toolchain, spec,
   theorem, dependency-fact, and policy identities affect only the query
   families that consume them.

   Fold Phase 9 #15 into this store/query namespace rather than creating a
   second `.build/concrete-proof-cache/` database. Cache only successful strict
   artifacts first. Negative/tolerant diagnostics, filesystem discovery,
   environment-dependent operations, solver results, and cancelled work need
   explicit cache contracts before reuse.

7. Add stable codegen units, an object cache, and deterministic relinking.
   V1 uses one codegen unit per source module plus stable runtime and test-harness
   units; if a measured large module needs subdivision, use deterministic
   stable-id buckets or explicit mono SCCs, never traversal-order chunks. Give
   every monomorphized instance one stable owner. Each unit emits a manifest of
   imports, exports, layouts, helpers, globals, source-map identities, trust
   labels, and mono instances so collision/missing-symbol checks happen before
   linking.

   The object key includes validated SSA/BackendIR unit digest, backend schema,
   target triple/data layout, optimization/debug/sanitizer profile, runtime ABI,
   and clang/LLVM identity and flags. The link key includes canonically ordered
   object digests plus linker/startup/runtime identities. Gate that a leaf edit
   rebuilds only its declared reverse dependencies plus link, a warm no-op with
   retained outputs invokes neither clang nor linker, target/toolchain changes invalidate every affected
   object, build/test share ordinary units but not harness units, and corrupt or
   substituted objects fail closed. Phase 15 later adds independent translation
   checking; an object-cache hit before then remains backend/compiler-trusted.

8. Add a long-lived service/watch surface over the same session, then
   deterministic independent-query scheduling.
   The initial engine is serial. Once serial reuse is correct, allow independent
   file/module/codegen-unit queries to execute in parallel with cancellation and
   bounded resources. Serial and parallel **Concrete-owned canonical**
   artifacts/facts must be byte- and schema-equivalent; external LLVM/object
   nondeterminism follows the explicit Phase 15 policy and must not change
   compiler facts or native behavior. Cancellation or a worker crash must not
   commit partial cache state. The internal service exists to preserve a session across CLI or
   scripted edits; Phase 19's LSP must wrap this exact session rather than a
   batch `runFrontend` or editor-only cache.

9. Add cache/query observability and a conservative rollout.
   Extend pipeline events with `executed | memory_hit | disk_hit | invalidated |
   uncacheable`, reason, dependency ids, bytes, time, validation-record id, and
   evidence class. Roll out `shadow -> opt-in -> CI verify/dual-run -> default-on`
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
    drift; object substitution; and serial/parallel modes. Compare
    incremental-off/on/verify facts, diagnostics, obligations, reports,
    Core/SSA/LLVM, native behavior, and query execution sets—not only wall time.
    On the Phase 8 large workload, a warm no-op must execute zero semantic or
    backend queries and a leaf edit must rebuild only declared reverse
    dependencies. Record relative latency (target probes: at least 20x no-op and
    5x leaf-edit versus cold median) without turning those probes into a public
    release claim; Phase 17 owns release performance budgets.

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
    (for example `concrete agent features --json` or `concrete help --json`)
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
   obligation ids. It must also exercise Phase 8.5 proof-query memory/disk hits,
   stale dependency invalidation, proof-tool/policy drift, corrupt-entry
   recovery, and cache-off/on/verify equivalence without treating a hit as
   proof evidence.

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
    audit-visible through the same surface user programs use. It must consume
    Phase 8.5 typed query/fact results and show cache/query provenance separately
    from evidence; it may not recompute a private audit-only compiler model or
    present a cache hit as validation.
21. Add the Phase 10 validation artifact: one package-scale audit bundle fixture
    with human and JSON output, semantic diff before/after a change, artifact
    viewer smoke test, oracle manifest, property-test manifest, persisted
    counterexample regression, spec-provenance facts, redaction check, replay
    command, and a README showing how a reviewer answers authority, proof,
    trust, assumption, and runtime-obligation questions without reading compiler
    internals. Cold, warm, and cache-off audit/diff/why output must be identical
    after normalizing timing/cache-observability fields, and corrupt/stale query
    artifacts must recompute or fail closed.

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

0. [DONE 2026-06-11] PROVEN violations are hard errors by default in safe
   code. The obligation engine already discharges some obligations to
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
    hardening: (a) ✅ largely DONE and still growing — the random differential
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
    safe code (reference returns are rejected at the type level — H1), so it is
    internal-lowering hardening; fixtures must distinguish rejected safe returns,
    allowed trusted raw-pointer returns, and a correct internal ref-valued return.
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
    may import the canonical artifact/schema decoder, hash/id layer, small
    semantic specification/judgment definitions, and checker theorems; it may
    not import Parser, Check, Elab, Mono, Lower, Report, CLI, project loader, the
    Phase 8.5 scheduler/cache, or producer verifier implementations. Publish its
    source/binary hash, import inventory, `partial`/unsafe/axiom inventory,
    rule-set version, and size so "small checker" remains measurable.

    V1 independently derives its verdict from the canonical Core artifact and
    small specifications. It checks at least: canonical/schema validity;
    artifact, subject, predecessor, and dependency-root binding; unique stable
    identities and referenced-node/provenance existence; constructor-local type
    consistency; direct-call arity and argument/result agreement; absence of
    unresolved placeholders/type variables where the Core boundary forbids
    them; direct-call capability containment; proof-target/body-fingerprint and
    obligation dependency binding; and the Phase 6B validation-chain shape. It
    must not merely compare a producer-emitted fact table with a second
    producer-emitted summary.

    Ownership/borrow/value-flow claims enter V1 only where the artifact carries
    enough independently checkable decisions and path edges to validate them.
    Checking that an ownership field exists is not ownership-checker soundness.
    Extend the predicate one rule family at a time with a mutation fixture and,
    where practical, a bounded soundness theorem such as
    `checkCoreCertificate a c = true -> CoreCertificateV1.Valid a c`.

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

    Bind the checker to the Phase 8.5 query DAG: cached canonical bytes and the
    producing compiler are untrusted for the named V1 predicate only after this
    checker recomputes it. A verification receipt names artifact digest,
    dependency root, checker/rule-set version, checked and unsupported
    predicates, theorem/replay identity where any, and exact trust boundary.

    Gate altered expression types, wrong call signatures/results, missing
    capabilities, unresolved type variables, duplicate ids, missing provenance,
    stale dependency roots, artifact/certificate swapping, fabricated proof
    links, mismatched obligation subjects, malformed/noncanonical encodings,
    unsupported constructs mislabeled checked, and a producer record that
    agrees with itself about a false artifact fact. Wire
    `scripts/tests/check_core_certificates.sh` and a checker import-firewall gate.
14. Add the Phase 14 validation artifact: a compiler-soundness dashboard with
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
   Start from `docs/ALIGNMENT_FACTS.md`: compare Zig, Rust, Odin, C, C++, and
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
    facts and reports one of: `translation_validated_v1`, `translation_trusted`,
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
    same lowering pass.

    V1 covers integer width/signedness and operator selection, bools, direct
    calls and signatures, returns, branches, fixed-layout structs/arrays,
    required arithmetic/bounds/runtime traps, layout/target constants,
    source-map identities, helper/intrinsic calls, and capability/trust labels
    for the subset admitted by item #18. Unsupported operations make only the
    affected function/region `translation_trusted` or `translation_blocked`;
    they may not be hidden under a module-wide validated badge.

    Bind Phase 8.5 codegen-unit manifests and object/link identities into the
    dependency root so the release graph cannot combine checked BackendIR from
    one build with objects or a link plan from another. The independent semantic
    check stops at `ValidatedBackendIR`: LLVM optimization, LLVM text-to-object
    translation, object writer, linker, ABI/runtime behavior, and native
    execution remain trusted/tested unless a later validator explicitly covers
    them. Object and binary hashes prove identity, not semantics.

    Mutate signed versus unsigned division, integer width, required overflow or
    bounds trap, branch target, field offset/layout, direct-call target/signature,
    capability/trust label, source-map id, target/profile, codegen-unit owner,
    object/link manifest, and certificate/build binding. Include an unsupported
    instruction mislabeled as validated. Independent replay must reject each
    mutation without calling the producer backend. Wire
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
     highest severity even when nothing crashes. Advisories enumerate
     affected claims by reading the obligation ledger of affected releases,
     not by prose recollection.
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
     proof. Phase 17 #18d's independent verifier must reject the old evidence
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
13. Add positioning page against Rust, Zig, Lean, SPARK/Ada, Austral, Hylo,
   Cogent, Dafny, F*, Why3.
14. Add migration/adoption playbook: what C/Rust/Zig code moves first, how to
   wrap libraries honestly, what stays outside Concrete.
15. Add release/install distribution matrix: host triples, checksums/signing,
    install paths, supported/deferred channels.
16. Add concrete distribution UX:
    `concrete --version --json`, `scripts/release/build_dist.sh`,
    `dist/concrete-<version>-<target>.tar.gz`, `dist/SHA256SUMS`,
    `dist/SIGNATURES` if signing is enabled, `INSTALL.md`,
    `RELEASE_NOTES.md`, and uninstall/upgrade notes. Nix/Homebrew or similar
    channels may be added only if they can be kept reproducible and
    version-pinned. The archive must ship the independently built
    `concrete-cert` verifier, expose `concrete-cert --version --json`, record its
    source/binary/rule-set hashes separately from the producer compiler, and let
    `verify-bundle` run without repository state or network access.
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
    x86_64/aarch64 and Windows status. Add performance-claim discipline:
    public copy may not imply "fast systems language" unless `concrete bench`
    and release performance gates publish replayable numbers; otherwise docs
    must say performance claims are not made yet.
18d. Add a Merkle-rooted release evidence DAG and offline independent bundle
    verification.

    Every release bundle carries a canonical graph whose nodes include source
    and lock inputs, compiler/query schemas, typed-Core artifacts and Phase 14
    receipts, obligation/proof artifacts, SSA/BackendIR artifacts and Phase 15
    translation receipts, codegen-unit/object/link manifests, emitted binary
    identity, policy/profile, assumptions/trust facts, checker identities, and
    toolchain identity. Every edge names the declared dependency relation and
    the bundle exposes one evidence root. A signature may authenticate that root;
    it is not correctness evidence.

    Ship the narrow independent command `concrete-cert verify-bundle <bundle>`
    with a `concrete verify-bundle` convenience alias only if the alias invokes
    the same checker. It operates without repository state or network access,
    treats the bundle as hostile input, recomputes canonical node/edge/root
    digests, reruns Phase 14/15 certificate predicates, validates checker and
    rule-set/toolchain/target/profile/policy binding, and invokes the pinned offline
    Lean replay path for facts labeled `kernel_replayed` or `proved_by_lean`.
    Producer reports and cached verification receipts are inputs to compare, not
    verdicts to trust.

    Output separate dimensions—`bundle_integrity`, `core_certificate`,
    `backend_translation`, `proof_replay`, `policy`, `assumptions`, and
    `remaining_trust`—rather than one ambiguous `verified` badge. The root binds
    exact artifacts and checked relationships; it does not make unchecked LLVM,
    object generation, linking, runtime, OS, hardware, trusted/Unsafe/FFI code,
    or spec adequacy correct. Default structural verification and the stricter
    release/verified-profile policy are separate modes.

    Reject a proof from build A combined with Core from B; BackendIR, object, or
    binary substitution; a deleted/changed dependency edge; a stale Phase 8.5
    cache artifact under a new root; schema/checker/toolchain/target/profile/
    policy mismatch; a revoked proof; an unsupported region reported validated;
    malformed graph cycles/duplicate ids; and a missing checker or theorem
    identity. Wire `scripts/tests/check_verify_bundle.sh`; a deliberately
    modified byte in every node family must either change the root and fail or
    be explicitly excluded as nondeterministic/non-evidentiary metadata.
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
    bundle, verify the evidence DAG offline through #18d from the installed
    independent checker, and run tamper cases for source/Core/proof/BackendIR/
    object/binary/dependency-edge substitution. Release replay begins with a
    clean cache or `--incremental=off`; a local cache cannot be an undeclared
    release input.

## Phase 18: Packages And Dependency Evidence

Goal: let package users inspect proof, trust, capability, and assumption facts
before adopting a dependency.

Done when: packages have manifests, lockfiles, package-aware facts, trust
policies, provenance, independently checked interface/evidence receipts,
Phase 8.5-aware reuse/invalidation, and registry protocol.

1. Expand package artifacts only after reports, policies, assumptions,
   interface artifacts, and CI gates prove what packages must carry. The first
   package artifact refactor must define exact files:
   `Concrete.package.json` (manifest summary), `Concrete.lock`,
   `.concrete/interfaces/<module>.json`, `.concrete/facts/<module>.json`,
   `.concrete/evidence/<module>.json`, `.concrete/certificates/<module>.json`,
   and `.concrete/docs/<module>.json`.
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
7. Add proof-aware package artifacts: facts, obligations, proof status, trusted
   assumptions, policy declarations, package-boundary evidence summaries.
   Required statuses: `proved_by_lean`, `proved_by_kernel_decision`,
   `solver_trusted`, `tested_by_oracle`, `enforced`, `assumed`, `trusted`,
   `partial`, `stale`, `vacuous`, `missing`, and `ineligible`. The artifact
   must record whether evidence is package-local, inherited from a dependency,
   or trusted through a boundary.
7a. Verify dependency interface certificates before consuming package facts or
    reusable artifacts.
    A content hash establishes identity, not semantic validity. Run the Phase 14
    Core checker and any applicable Phase 15 translation checker over the
    dependency's canonical interface/evidence artifacts, cache the independent
    receipt by bundle root + checker/rule-set version, and bind the receipt into
    the importing project's Phase 8.5 dependency graph. Invalid, stale,
    unsupported-schema, or unsupported-predicate certificates trigger a source
    rebuild when source and policy permit it, or a loud policy failure; they
    never silently become trusted facts.

    Import authority/evidence constraints may consume independently checked
    interface facts or facts explicitly labeled `compiler_validated` with the
    producing compiler still trusted; they may not erase that distinction. Gate
    a valid package, tampered interface fact,
    certificate from another package/build, stale private body with unchanged
    public interface, changed public interface, old checker version, unsupported
    predicate, proof revocation, and source-rebuild fallback. Wire
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
    system. Concrete already content-addresses proof attachments through
    `proof_fingerprint`. The natural Phase 18/19 extension is structural hashes
    for obligations, typed-Core fragments, proof bundles, package interface/body
    evidence, and replay artifacts, so caches and dependency evidence are keyed
    by content rather than names or paths. This supports the incremental
    verification cache, package evidence reuse, registry-retirement work, and
    editor/agent fact lookup without trusting mutable labels.

    The hard part is not hashing; it is deterministic canonical serialization.
    Before these hashes are authoritative, each artifact kind must define a
    stable, versioned, pointer-free canonical encoding: no map iteration order,
    temp paths, generated-name nondeterminism, host-dependent formatting, or
    schema-ambiguous fields. Borrow the Zig InternPool lesson here: artifacts
    that need stable identity must serialize from explicit IDs and normalized
    structure, not from incidental in-memory shape.

    Done when at least obligations, typed-Core fragments, proof bundles, and
    package evidence artifacts have versioned canonical encoders; identical
    semantic content hashes identically across repeated runs; a deliberately
    nondeterministic encoder is caught by a gate; and package/proof cache keys
    use these content hashes instead of source paths or human names.
15c. Add optional shared/remote artifact reuse only after the local Phase 8.5
    store and independent package verification are mature.
    Define an authenticated transport/protocol for content-addressed package,
    proof, and codegen artifacts; the server is an untrusted blob store, not a
    compiler authority. Every download must pass size/schema/digest and
    dependency-root checks, the applicable Phase 14/15 certificate checker, target/profile/
    toolchain binding, and package policy before entering the local store.
    Upload only deterministic canonical artifacts with explicit privacy/
    redaction policy; never upload source, diagnostics, proof attempts, or
    credentials implicitly. A signature authenticates provenance, not
    correctness.

    This item is optional for the first package release and must not complicate
    local builds. Gate hostile/corrupt server responses, cache poisoning,
    replayed old roots, cross-target objects, revoked proofs, concurrent upload,
    offline fallback, and local recomputation parity. Record CI/team speedups,
    but keep correctness independent of network/cache availability.
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

     Deliverable: `concrete help --json` and/or
     `concrete agent features --json`. The catalog must describe supported
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
    [docs/SIZED_EVALUATOR_INVESTIGATION.md](docs/SIZED_EVALUATOR_INVESTIGATION.md).
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
