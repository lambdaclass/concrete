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
4. build the standard library and core APIs before relying on real workloads,
   packages, editor tooling, freestanding targets, or release examples;
5. validate the bet with flagships, real workloads, and at least one external
   user before the large ecosystem/release/editor build-out;
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

North star: **systems code with explicit authority, bounded behavior, small
trusted boundaries, and Lean-backed evidence tied to real source code, while
keeping compiler, backend, toolchain, runtime, and target assumptions honest.**

Known holes index: every tracked soundness / dark-construct gap — what it is,
whether it is open or closed, the gate that locks it, and the item here that
fixes it — is consolidated in [docs/KNOWN_HOLES.md](docs/KNOWN_HOLES.md). Keep
it in sync when a hole is added or fixed. **Open soundness hole: H6** (wildcard /
discarded expression bypasses linearity). The arithmetic/cast/bounds holes are
closed: H2 (float→int cast overflow) closed 2026-06-26; H8 (array indexing) closed
2026-06-28 — raw `a[i]`/`a[i] = v` now traps at runtime on out-of-bounds, matching
the interpreter. Overflow, div/mod-zero, over-width shift, `MIN` negation, the
float→int cast, and array bounds all abort by default (ROADMAP #10 Stage 2.x +
H8). (H7 is an invalid-SSA codegen bug — a valid program wrongly rejected at
compile time — not a runtime soundness hole.)

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
2. **Build exactly that** — not the full back half.
3. **Run the trial and treat the result as an explicit go / no-go on the rest
   of Phases 11-19.**

The trial should be implemented as the first external-user workload in the
Phase 8 real-workload ladder, after the Phase 5 core slab and Phase 7
stdlib/core-API slab exist, not as a separate validation artifact.

**Pass criterion:** at least one person who is **not** the compiler author
writes, proves, or contract-annotates a useful Concrete program and reports that
the proof discipline (ProofKit + contracts + `concrete prove`) was worth the
cost. Until this passes, the back half of Phases 11-19 is flagged **at-risk**,
not green.

**Scope of the gate.** Phases 8-10 (flagships, proof authoring, audit commands)
are NOT gated — they proceed regardless, because they are what the trial runs
on and what a negative result would teach against. A **fail** verdict does not
silently park Phases 11-19 either: it forces an explicit decision recorded in
this file — change the bet (redesign the discipline that failed), narrow the
audience, or stop — before any Phase 11+ item may start.

## Phase 6: Language Usability And Daily Workflow

Goal: make Concrete usable as a normal experimental language, independent of
whether a user is writing proofs.

Done when: a new user can format, build, run, test, diagnose, inspect, and debug
small Concrete programs with predictable commands and useful errors.

This phase starts only after the Phase 5 core slab is sufficiently stable. It
keeps grammar/control-flow ergonomics, daily commands, examples, docs, linting,
profiling, and editor-adjacent usability separate from the smaller core-slab
gate.

### Completed in Phase 6 (folded out of the active list)

Fully done + gated; kept here as a one-line index. See CHANGELOG.md and the
linked docs/gates for detail. Decisions that are *deferred with a pull-condition*
(e.g. #9, #15, #26, #27) stay in the active list below — only fully-shipped items
are folded out.

- **#1 `concrete fmt`** — subcommand, semantics-preserving (fingerprints
  byte-identical across format). `check_concrete_fmt.sh`.
- **#2 grammar** — canonical `grammar/concrete.ebnf` (LL(1), 3 checkers) +
  `docs/GRAMMAR.md`.
- **#3 type aliases** — transparent, deeply expanded; `docs/TYPE_ALIASES.md`,
  `check_type_alias.sh`.
- **#4 loop control** — `break`/`continue`/labels/while-expr value + linear
  cleanup; `docs/LOOP_CONTROL.md`, `check_loop_control.sh`.
- **#5 pattern ergonomics** — ranges, `if let`/`while let`, guards, OR-patterns,
  match-on-`&T`, struct update `..base`, `_` in destructuring. No anonymous
  tuples and nested patterns deferred (workload-gated). `docs/PATTERN_ERGONOMICS.md`.
- **#6 numeric model** — de-facto integer model locked; out-of-range literals
  rejected (E0227). Literal suffixes deferred. `check_numeric_literals.sh`.
- **#7 `defer`** — LIFO, runs on every exit path; block-form rejected.
  `docs/DEFER.md`, `check_defer.sh`.
- **#10 build profiles + checked arithmetic** — `--profile`, full checked
  `+ - * / % << >>` with trapping, `wrapping_*`/`saturating_*`, float→int H2
  closed, `--report arithmetic`. `docs/ARITHMETIC_POLICY.md` + the arith gates.
- **#11 macro stance** — permanent decision: no v1 macros. `docs/MACRO_STANCE.md`,
  `check_no_macros.sh`.
- **#13 ignored-result diagnostics** — discarding a `Result`/`Option` statement
  is E0286 unless acknowledged with `let _ = …;`; `let _` does not silence a
  `Destroy` resource. `docs/IGNORED_RESULT.md`, `check_ignored_result.sh`.
- **#16 style guide** — `docs/STYLE.md` (advisory; mechanical layout owned by
  `concrete fmt`).
- **#17 iteration protocol** — blessed traversal forms, no `Iterator` trait;
  `docs/ITERATION_PROTOCOL.md`.
- **#19 stdlib handoff** — required surfaces stable/provisional, none blocked;
  `docs/STDLIB_HANDOFF.md`, `check_stdlib_handoff.sh`.
- **#33 memory model** — user-facing narrative; "no uninitialized reads" is a
  grammar-level guarantee. `docs/MEMORY_MODEL.md`, `check_memory_model.sh`.
- **#35a semantic-darkness / red-team gate** — `check_phase6_redteam.sh`.
- **#35b differential oracle bug-hunt** — six interp-vs-compiled fixes for
  mutable place borrows, value expressions, `while` values, `else if` value
  parsing, and negative bitwise ops; `tests/oracle/vectors.txt`.
- **#36 statement-vs-trailing-expression** — `isValue` on `Stmt.expr`/`CStmt.expr`;
  `docs/STATEMENT_EXPRESSION_MODEL.md`.

13a. **URGENT regression fix before the next Phase 6 feature: wildcard/discard
   must not bypass linearity.** The Phase 6 #13 ignored-result implementation
   made `let _ = expr;` an acknowledgement form, but the checker currently skips
   registering `_` bindings for any non-`Destroy` type. That creates a linearity
   escape hatch: `let _ = LinearStruct { ... };` compiles even though the same
   value bound to a name is E0208, and `let _ = Result<Resource, E>` drops the
   outer result without proving/consuming the payload. Tracked as
   `docs/KNOWN_HOLES.md` **H6**. Broader probe (2026-06-27) found the root class
   is **missing nested-scope exit checking**: a bare expression statement like
   `LinearStruct { ... };` compiles because `Stmt.expr` only rejects
   `Result`/`Option`; `if`/`if`-expression branches can create branch-local
   linear values that disappear when the checker restores the pre-branch env;
   match/enum destructuring arms do not scope-check arm-local linear fields
   introduced by variant bindings or skipped by `_`; enum `let ... else`
   destructuring over a linear payload can bind-and-drop the payload; `return`
   inside a nested branch can skip checking locals created earlier in that branch;
   `?` propagation inside a nested branch can do the same;
   and returned linear values in statement position (`make_resource();`,
   `receiver.method_returning_resource();`, `Type::make_resource();`) compile.
   Deferred calls are another discard site: `defer risky_result();` and
   `defer make_resource();` currently ignore the call result. Required fix:
   - Define one checker helper for "may be explicitly discarded" and use it for
     `let _ =`, `Stmt.expr` discard, match arm wildcard/value bindings, and
     destructuring desugar output; Copy values are discardable, but linear values
     must be consumed, returned, or reserved exactly as MEMORY_GUARANTEES says.
   - Refactor checker block handling so every statement list is checked as a
     lexical scope with a known entry environment, exit mode, and local-variable
     set; before an env is restored or a branch result is merged, all locals
     introduced inside that block/arm must pass `checkScopeExit` unless the block
     exits by a path that transfers ownership (`return`, typed `break`, etc.) and
     that path has checked the same local set. Do not rely on the final
     function-level `checkScopeExit` to catch locals from branches/arms whose envs
     are thrown away.
   - Reject or force explicit handling for `Result<T, E>` / `Option<T>` when `T`
     or `E` can carry a linear value; do not treat the outer enum as a proof that
     payload ownership disappeared.
   - Add red-team fixtures to `scripts/tests/check_ignored_result.sh` or a new
     linear-discard gate: ordinary `let _ = Linear`, bare `Linear;`, returned
     linear temporary in statement position, branch-local linear in `if` /
     `if`-expression including return/`?` propagation paths, empty/copy-only `else` branches,
     enum `E::A { _ }` over a linear payload, enum arm `E::A { x }` fallthrough
     with `x` unconsumed, enum `let E::A { x } = e else { ... };` with `x`
     unconsumed, calls/methods/static methods returning linear values in statement
     position, deferred calls returning `Result`/`Option` or linear values, and
     `Result<Resource, E>` acknowledgement must all fail. Keep the existing
     loop/break/continue/borrow-block positives green; probes show those paths
     already reject locals correctly.
   - Register E0286 in `--report diagnostic-codes` while touching the diagnostic
     surface.
   - Update `docs/IGNORED_RESULT.md`, `docs/MEMORY_GUARANTEES.md`, and
     `docs/KNOWN_HOLES.md` when fixed; the closing gate must prove the previous
     false-green `check_ignored_result.sh` coverage hole is sealed.

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
18. Finish only the remaining callable-values implementation work that real
    workloads pull. The design checkpoint is DONE and recorded in
    `docs/CALLABLE_VALUES_AND_CAPABILITIES.md` plus the changelog; H1 is closed
    by subtraction; immutable scoped reads and the capability-polymorphic HOF
    surface have shipped. Keep the roadmap focused on the unfinished tail:
    `with(f)` capset-elision if signature noise becomes real friction,
    `with_value_mut` / `modify` only after a container-not-in-context gate proves
    the mutable receiver/context aliasing invariant, and first-class stored
    `BoundFn` values only after a storage workload needs them. `from(param)` and
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
    semantic warnings that are not type errors, including ignored fallible
    results, suspicious capabilities, unreachable contracts, redundant
    runtime checks, likely path/bytes/text confusion, unstable public API use,
    and release-policy warnings. Every lint must have a diagnostic code,
    source span, machine-readable JSON payload, and an explicit allow/deny
    policy.
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
35. Add the Phase 6 validation project: a small C/Rust-style CLI using the
    Phase 5 core slab plus daily workflow (`Concrete.toml`, modules/imports,
    `concrete test`, bytes/text/path and collection decisions, narrow const
    generics, pattern ergonomics, callable/capability callback decisions,
    diagnostics, formatting, docs, lint/vet, benchmark/profile/coverage smoke
    tests, and trace/debug commands). CI must build, run, test, format-check,
    lint, audit, record compiler-known target constants, and compare
    interpreter-vs-compiled behavior on macOS and Linux. It validates the
    language/tooling slab, not the full stdlib.

## Phase 7: Standard Library And Core APIs

Goal: build the small standard library people need before real workloads,
packages, editor tooling, freestanding targets, and release work can be honest.

Done when: a normal C/Rust-style Concrete program can use documented core APIs
for errors, bytes/text/path, collections, formatting/parsing, I/O capabilities,
tests, and oracle helpers, with every public stdlib item carrying an evidence
class and authority/allocation story.

1. Define the stdlib gap matrix against Gleam, Roc, and Zig before expanding
   APIs. Concrete should not copy Zig's full surface; it should make a
   Gleam/Roc-sized core pleasant, Zig-style authority explicit, and Concrete's
   evidence ledger visible. Record which modules are core-now, hosted-now,
   freestanding-later, package-later, or research-later. The matrix must name
   the obvious external references so they are not forgotten: Gleam's
   `List`/`Dict`/`Set`/`String`/`BitArray`/`BytesTree`/`StringTree`/`Uri`/
   `Dynamic.Decode`; Roc's `Str`/`List`/`Dict`/`Set`/`Iter`/numeric builtins;
   and Zig's `ArrayList`/maps/sets/sort/fmt/json/base64/Uri/fs/process/time/
   random/hash/crypto/log/testing/allocators/atomics/threads/compress/archive/
   target/OS/debug-format surface.
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
   for fallible returns.
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
9. Build iterator and builder APIs in proposed `std.iter` and `std.builder`
   after the collection shape is known: `Iter<T>`-style adapters,
   `fold`/`map`/`filter`/`take`/`drop`, known-length reporting, byte/text
   builders, and tree/buffer builders inspired by Gleam's `BytesTree` and
   `StringTree`. Do not hide allocation; builder APIs either carry
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
    values. Add `docs/STDLIB_BOUNDARY_MODULES.md`,
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
    `docs/STDLIB_GUIDE.md` and `scripts/tests/check_stdlib_fs.sh`.
28. Build handle-based network surface in `std.net` only as far as the
    validation workloads require: address parsing, socket handle wrappers, HTTP
    header parsing as a pure parser first, and no full HTTP client/server until
    package/workload evidence demands it.
29. Build stdlib test/oracle helpers in `std.test`: expected failures,
    capability-scoped fixtures, temp directories, oracle vector runners,
    interpreter-vs-compiled helpers, and report snapshots.
30. Define stdlib error-handling conventions: when APIs return `Result`,
    `Option`, panic/abort, or require a policy gate; how ignored-result
    diagnostics apply; and how accumulating error sets are reported.
31. Define stdlib evidence classes per public API: `proved`, `enforced`,
    `reported`, `tested_by_oracle`, `assumed`, or `trusted`. The evidence class
    must appear in docs and audit artifacts, not just implementation comments.
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
    `docs/STDLIB_GUIDE.md` plus `examples/stdlib_recipes/bytes_text`,
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
    external-validation-gate trial.

## Phase 9: Proof Authoring And Automation

Goal: make flagship proofs a repeatable engineering workflow, not a collection
of one-off `simp` scripts.

Done when: new flagship proofs can start from useful generated stubs, standard
lemmas, and actionable failure diagnostics.

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
    not introduce a second stub generator.
12. Add generated composition scaffolds: FnTable entries, call lemmas, callee
    refinement dependencies, and composed theorem skeletons.
13. Add generated loop-invariant templates for common proof shapes:
    counter loop over array writes, copy loop, fold loop, multi-store loop,
    offset loop, and block-processing loop.
14. Improve failed-proof diagnostics after `--json`, failed artifacts, and
    `--minimize` exist: classify common failures into actionable categories
    such as missing callee theorem, stale source link, missing table entry,
    failed arithmetic bridge, insufficient frame fact, and spec/extraction
    mismatch. Diagnostics should point to the already-generated artifact or
    next action instead of introducing another parallel proof surface.
15. Add proof-result caching once proof artifacts and fingerprints are stable.
    Cache key: toolchain version, source fingerprint, spec/proof link,
    obligation id, ProofKit version, backend engine version, and policy mode.
    Store under `.build/concrete-proof-cache/` and expose
    `concrete prove --cache-status --json`. Wire
    `scripts/tests/check_proof_cache.sh`; the gate must prove cache hits do not
    mask stale source, stale theorem names, changed policies, or changed solver
    trust settings.
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
    predicates: sorted, bounded, no-duplicates, fixed-length, prefix, checksum,
    constant-time source shape.
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
    `check-proofs` verifies it.
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
   obligation ids.

## Phase 10: Audit Commands And Review Artifacts

Goal: let a reviewer answer "what can this program do, what is proved, what is
assumed, and what changed?" without reading compiler internals.

Done when: `concrete audit`, semantic diff, and an artifact viewer cover the
five graduated flagships and one package-scale example.

1. Stabilize machine-readable fact schemas for proof status, obligations,
   effects, capabilities, assumptions, policies, snapshots, and showcase
   metadata.
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
6. Add semantic trust diff gates: capability widening, allocation change,
   trusted boundary addition, stale proof, weakened/missing obligation,
   assumption widening.
7. Add `concrete audit --json`: machine-readable audit output for CI,
   dashboards, editor tooling, and release bundles.
8. Add an artifact viewer CLI/TUI over facts, obligations, proofs,
   assumptions, release bundles, and diffs.
9. Ensure every release bundle includes an evidence replay command.
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
    SMT, property tests, oracle failures, and future fuzzed contracts. Command
    surface: `concrete counterexample save <obligation_id> --out
    tests/counterexamples/<name>.con` plus JSON mode. The saved fixture must
    include source inputs, expected failing obligation id, expected status
    (`counterexample`, `tested_by_property_failure`, `oracle_failure`, etc.),
    replay command, and the original tool provenance. Wire
    `scripts/tests/check_counterexample_regressions.sh` with one SMT overflow
    witness, one property-test contract witness, and one oracle mismatch. The
    gate must fail if a future refactor turns the same counterexample into
    `proved_*` without changing the checked fixture expectation.
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
    audit-visible through the same surface user programs use.
21. Add the Phase 10 validation artifact: one package-scale audit bundle fixture
    with human and JSON output, semantic diff before/after a change, artifact
    viewer smoke test, oracle manifest, property-test manifest, persisted
    counterexample regression, spec-provenance facts, redaction check, replay
    command, and a README showing how a reviewer answers authority, proof,
    trust, assumption, and runtime-obligation questions without reading compiler
    internals.

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
    loudly, not reuse the stale artifact).
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
    presented as stronger evidence.

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

Done when: each flagship-used ProofCore construct is classified as
shape-preserved, eval/source-semantics-preserved, or still trusted; proof-report
facts agree with compiler state; and the remaining trusted base is
machine-readable.

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
    hardening: (a) a full interpreter-vs-compiled differential harness with a
    random program GENERATOR over the through-reference / void-slot bug-prone
    shapes (extends `check_codegen_differential.sh`; needs interpreter structured
    diagnostics, the deferred Phase-4 #18a); and (b) the defense-in-depth
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
14. Add the Phase 14 validation artifact: a compiler-soundness dashboard with
    one witness program per shipped ProofCore construct, one status per
    R-rule, replay commands for proved/mechanically-validated facts, and
    regressions proving report facts (`proved`, `stale`, `blocked`, `missing`,
    `ineligible`, `trusted`) agree with compiler state.

## Phase 15: Backend, Target, And Stdlib Contracts

Goal: make backend/toolchain/stdlib assumptions explicit, and state exactly
where source-level proof stops.

Done when: SSA, target/toolchain, optimization, ABI/layout, stdlib evidence,
and incremental build contracts are explicit enough for release evidence.

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
   runtime checks from trusted backend/OS crashes.
5. Add clean-build versus incremental-build equivalence checks: facts,
   obligations, diagnostics, reports, and codegen must agree.
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
9. Add backend IR emission and verification as a release-facing backend
   contract: `concrete inspect --backend-ir`, `--emit-backend-ir`,
   `concrete verify-ir --pass backend-ir`, golden backend-IR fixtures, and
   round-trip checks that source maps, target constants, runtime checks, and
   capability/trust labels survive lowering.
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
    failure mode, trusted platform assumptions, and evidence class.
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
    backend. V1 should validate a narrow backend-IR subset per compile:
    integer arithmetic, fixed arrays, structs, direct calls, branches, bounded
    loops, runtime checks, capability calls, and source-map annotations. The
    validator compares checked Core / typed IR facts against emitted backend IR
    facts and reports one of: `translation_validated`, `translation_trusted`,
    `translation_blocked`, or `translation_mismatch`. This is not a promise to
    prove the whole LLVM/native stack; it is a per-artifact check that the
    Concrete lowering into the backend contract preserves the facts Concrete
    claims. Add `examples/translation_validation/` with straight-line,
    branch/loop, struct/array, runtime-check, and deliberate mismatch fixtures.
    Wire `scripts/tests/check_translation_validation.sh`; the gate must prove a
    Lean-proved/Core-level claim cannot be presented as native-code evidence
    unless the backend artifact is either translation-validated or explicitly
    backend-trusted in the audit bundle.
19. Add the Phase 15 validation artifact: a backend/std-lib contract project
    with ABI/layout C round trips, C/ABI glue generation/import checks,
    the C ABI classification matrix, alignment-fact fixtures if Phase 15 #6a is
    pulled into implementation, backend-IR emission/verifier checks,
    translation-validation checks, sanitizer runs, compiled-oracle differential
    tests, native debug/source-map smoke tests, clean-vs-incremental fact
    equivalence, and stdlib authority/allocation/evidence gates.

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
inspect its audit bundle, and understand the claim matrix in under ten minutes.

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
- The stdlib/runtime boundary is stable enough for daily examples.
- At least one external user completes the first-user tutorial and a useful
  audit/proof workflow without compiler-author intervention.

1. Define first public release criteria: supported subset, required examples,
   required diagnostics, proof workflow, stdlib/project UX, evidence/policy/
   tooling story.
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
   `assumed` are forbidden for release claims.
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
     proof.
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
13. Add positioning page against Rust, Zig, Lean, SPARK/Ada, Austral, Dafny,
   F*, Why3.
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
    version-pinned.
17. Add release performance budgets:
    `scripts/tests/check_release_performance.sh` must measure compiler startup,
    small-project build, stdlib build, `concrete test`, audit/report generation,
    and proof-check latency against `release/perf-baseline.json`. Done when
    release CI blocks unexplained regressions and prints the regressed command.
18. Add reproducible release artifact hashes:
    `concrete release --manifest --json` must record source tree hash,
    compiler commit/version, schema versions, target triple, build profile,
    dependency lock hash, stdlib hash, emitted binary hash, release-bundle hash,
    and replay command. V1 does not support old release manifests; schema
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
    bundle.

## Phase 18: Packages And Dependency Evidence

Goal: let package users inspect proof, trust, capability, and assumption facts
before adopting a dependency.

Done when: packages have manifests, lockfiles, package-aware facts, trust
policies, provenance, and registry protocol.

1. Expand package artifacts only after reports, policies, assumptions,
   interface artifacts, and CI gates prove what packages must carry. The first
   package artifact refactor must define exact files:
   `Concrete.package.json` (manifest summary), `Concrete.lock`,
   `.concrete/interfaces/<module>.json`, `.concrete/facts/<module>.json`,
   `.concrete/evidence/<module>.json`, and `.concrete/docs/<module>.json`.
   Each artifact must name compiler version, schema version, package id,
   source hash, interface hash, dependency hashes, authority budget,
   assumptions, proof/evidence summaries, and replay commands. Add
   `scripts/tests/check_package_artifacts.sh`; the gate must prove package
   consumers read these artifacts rather than source-private side channels.
2. Design and parse package manifest.
3. Add version constraints, dependency resolution, and lockfile.
4. Add workspace and multi-package support.
5. Add package-aware test selection.
6. Split interface artifacts from body artifacts at package/workspace scale.
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
   license = "MIT OR Apache-2.0")`. The checked fact set must include at least:
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
16. Add the Phase 18 validation artifact: a multi-package workspace project
    with dependency resolution, lockfile, package-aware tests, interface/body
    artifact split, dependency trust policy, assumption inheritance, authority
    budgets, provenance, evidence-typed imports, published docs, and
    release-bundle evidence for every dependency. Wire it as
    `examples/package_workspace/` plus
    `scripts/tests/check_phase17_packages.sh`.

## Phase 19: Editor And Human Tooling

Goal: make evidence visible where developers work.

Done when: editor/LSP/tooling exposes the same facts as CI and command-line
reports without inventing a second truth source.

1. Add artifact viewer integration for proof/evidence facts.
2. Add compiler-as-service / LSP entrypoints after diagnostics and facts are
   structured.
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
14. Add the Phase 19 validation artifact:
   `scripts/tests/check_phase18_editor.sh` runs a scripted LSP/editor session
   or golden transcript over one real project, proving hover, diagnostics,
   obligation navigation, proof/evidence facts, dependency audit UI, refactor
   behavior, docs integration, deprecation diagnostics, and playground output
   match CLI facts rather than inventing a second truth source.

## Phase 20: Concurrency And Research-Gated Extensions

Goal: keep speculative ideas gated until Concrete's proof/evidence foundation
can contain them honestly.

Done when: each research idea is either pulled into an earlier phase by a
forcing example, explicitly deferred, or rejected.

1. Keep concurrency design-only until the v1 surface is frozen:
   capability lattice, scopes, spawn/join, linear handles, bounded channels,
   result flow, ownership transfer, rejected forms, and report schema.
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
14. Add the Phase 20 validation artifact: one pressure-test sketch, expected
    report, and decision record for every research-gated extension
    (concurrency, typestate, arena allocation, WCET, binary-format DSLs,
    hardware capability mapping, Miri-style interpreter, sized evaluator,
    persistent rewrite state, and row effects). No research item graduates
    unless its forcing example, report shape, evidence class, and rejection or
    pull-forward criteria are recorded.
