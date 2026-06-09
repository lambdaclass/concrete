# Concrete Roadmap

This document is the active execution plan. It answers one question:
**what should happen next, in what order?**

The roadmap is linear. Phases are ordered, and items inside a phase are ordered
unless explicitly marked as a constraint or a deferred research note. Read the
document as one queue:

1. consolidate the compiler's obligation pipeline so contracts, VCs, runtime
   safety, asserts, SMT, policy, reports, and proof workspaces all read from
   one typed evidence ledger;
2. consolidate the ordinary compiler pipeline: project loading, pass
   boundaries, typed IR, diagnostics, source maps, backend contracts, and
   command plumbing;
3. broaden the ordinary language surface immediately after the compiler
   pipeline can support it: patterns, bytes/text/path, collections, iteration,
   capability polymorphism, tests, project ergonomics, and daily workflow;
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

Completed work moves to [CHANGELOG.md](CHANGELOG.md). Deferred or conditional
work moves later in the same linear queue. There are no parallel tracks. Inline
`NEXT` notes are allowed only as scoped follow-ups inside a numbered item; they
do not create a second queue.

Linear does not mean invisible. When a long internal phase risks delaying
external credibility, add one narrow replayable vertical artifact inside the
current linear queue. It must not become a side track or product fork; it exists
to validate the bet, expose abstraction mistakes early, and give skeptics
something real to run. Phase 4 may use `examples/compiler_pipeline_probe/` as
that transcript for the compiler pipeline; Phase 7 carries the later
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

**Provable Float V1 (future, narrow profile — see Phase 11).** A function opts
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

### Embedded hardware access: evidence classes (see Phase 15)

Name the evidence class before implementing freestanding/embedded targets:
- **inline asm** — `trusted`, requires `with(Unsafe)`.
- **volatile / MMIO** — explicit capability, e.g. `with(Device)` / `with(Mmio)`;
  reads/writes are audit-visible effects, never silently elided.
- **interrupt handlers** — a separate trusted/effectful boundary.

### Native debug info (see Phase 14)

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

### Contract-VC stability tiers (dependency edge into Phase 5)

The risk this names: Phase 7 flagships are what exercise contracts, and they
will keep hitting an un-frozen Phase 5 surface — so any VC/contract IR designed
in Phases 1-2 against that surface gets reworked when collections and the
iteration protocol land. The fix is **not** to reorder (that is circular: the
flagships are what stress-test Phase 5) but to tag each contract/VC construct by
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
  the relevant Phase 5 item has landed. Treat any such construct as "will be
  reworked," and do not let a flagship bake an iterator/collection assumption
  into the VC shape.

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

### External-validation gate (go / no-go before the back half of Phases 10-18)

This is a **gate, not a note** — promoted out of Phase 16 because validation
that sits downstream of the build-out it is meant to justify is no validation
at all. The central bet of the whole project is "evidence-carrying source is
worth the discipline." Today the only person who has found it worth the cost is
the person who built it, yet Phases 10-18 (packages, editor, freestanding,
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
   of Phases 10-18.**

The trial should be implemented as the first external-user workload in the
Phase 7 real-workload ladder, after the Phase 5 core slab and Phase 6
stdlib/core-API slab exist, not as a separate validation artifact.

**Pass criterion:** at least one person who is **not** the compiler author
writes, proves, or contract-annotates a useful Concrete program and reports that
the proof discipline (ProofKit + contracts + `concrete prove`) was worth the
cost. Until this passes, the back half of Phases 10-18 is flagged **at-risk**,
not green.

**Scope of the gate.** Phases 7-9 (flagships, proof authoring, audit commands)
are NOT gated — they proceed regardless, because they are what the trial runs
on and what a negative result would teach against. A **fail** verdict does not
silently park Phases 10-18 either: it forces an explicit decision recorded in
this file — change the bet (redesign the discipline that failed), narrow the
audience, or stop — before any Phase 10+ item may start.

## Phase 3: ObligationCore Pipeline Consolidation

Goal: make every proof, contract, runtime-safety, assertion, SMT, policy, audit,
and proof-authoring surface read from one typed obligation ledger instead of
parallel report-specific walkers.

Design reference: [docs/OBLIGATION_CORE.md](docs/OBLIGATION_CORE.md).

Done when: call-site preconditions, bounds, div/mod, overflow, assertions,
loop VCs, contract clauses, proof links, SMT queries, counterexamples, policies,
audit reports, JSON, and `concrete prove --workspace` all agree because they
consume the same typed `ObligationCore` records and evidence classifications.

Execution order: migrate obligation families first, then expression lowering
and backend adapters, then policies/reports/prove workspaces as temporary
views/parity gates, then perform the full model merge and delete the duplicate
models/walkers. Do not call #14, #15, or #16 architecturally complete merely
because a consistency gate passes; they are only complete when #18 has made
`ObligationCore` the hub and the old side-channel model is gone. Do not move a
consumer before the family records it needs exist in the ledger.

Risk note on the remaining items (#8-#18): the migration is currently
half-done, and a half-migrated ledger is itself a dual-truth-source /
false-green risk. Assert, vacuity, and assume obligations are still computed
by report-side walkers outside the ledger (`Report.lean` `vacuityGoals` /
`collectAssertAssumeS` around lines 1053/1075, discharged directly in
`Main.lean` ~659-661), so today one surface can classify an assert/vacuity
fact differently from a future ledger view with no gate catching it. Until
#8-#18 complete, every new report surface added on top of this state widens
the window where two live truth sources can disagree — finish the migration
before growing new report surface, and treat any interim addition as needing
an explicit parity gate per #17.

1. Define `ObligationCore` schema v1: stable id, source span, function,
   obligation kind, expression shape, typed variables, scoped hypotheses,
   conclusion, semantic profile, dependencies, allowed engines, status,
   evidence class, replay command, counterexample, policy impact, and
   originating source construct.
2. Define the single evidence/status vocabulary used by the ledger:
   `proved_by_lean`, `proved_by_kernel_decision`, `proved_by_lean_replay`,
   `solver_trusted`, `tested_by_oracle`, `runtime_checked`, `enforced`,
   `assumed`, `trusted`, `partial`, `stale`, `vacuous`, `missing`,
   `unproven`, `counterexample`, `unknown`, `timeout`, `solver_error`, and
   `ineligible`. Reports may summarize these statuses, but may not invent a
   second vocabulary.
3. Build one scoped context collector for all obligation kinds. It must thread
   function `#[requires]`, branch guards and negated fall-through facts, loop
   invariants, already-proved assertions, local constants, ghost bindings, and
   let substitutions; it must drop stale hypotheses after assignments using
   one shared invalidation rule.
4. Migrate call-site precondition obligations to `ObligationCore` first. This
   is the smallest forcing case because the current behavior already works and
   has positive/negative fixtures.
5. Migrate array-bounds obligations to the same collector and ledger. Keep the
   existing constant, `omega`, unproven, and violation classifications exactly
   stable.
6. Migrate div/mod nonzero obligations and sound division/modulo lowering.
   Preserve the non-negative-dividend guard that prevents Lean floor-division
   semantics from being confused with Concrete truncating division.
7. Migrate opt-in overflow obligations, including interval/bv-discharge and
   nonlinear SMT routing. Preserve the rule that external SMT may only touch
   VCs the kernel tiers left unproved.
8. Migrate `assert` and `assume`: `assert` becomes an obligation with scoped
   hypotheses; `assume` becomes an audit-loud assumption fact and policy input,
   never a proof.
9. Migrate loop obligations O1-O5, including loop invariant initialization,
   preservation, variant/decrease, and the split status for arithmetic-closed
   but operationally-unproved obligations.
10. Migrate `#[requires]`, `#[ensures]`, `#[invariant]`, and `#[variant]`
    clauses into the ledger, including invalid expression, impure call,
    vacuity, partial proof, and source-linked proof statuses.
11. Migrate proof-link freshness, `#[proof_fingerprint]`, spec-drift, missing
    theorem, blocked proof, and ineligible extraction facts into the same
    ledger instead of keeping proof-status as a separate model.
12. Unify expression lowering through one typed obligation expression layer:
    human rendering, Lean proposition rendering, SMT-LIB rendering,
    counterexample source-variable mapping, and JSON serialization must lower
    from the same structure. Add `scripts/tests/check_obligation_lowering.sh`
    with round-trip cases for linear arithmetic, bitvectors, sound division,
    nonlinear SMT terms, branch/path facts, and invalid expressions.
13. Add backend-owned discharge adapters over `ObligationCore`: constant fold,
    `omega`, `bv_decide`, linked Lean theorem, Lean replay, external SMT,
    oracle/test evidence, runtime enforcement, assumption, and trust boundary.
    Each adapter may only produce its declared evidence class. Add
    `scripts/tests/check_obligation_discharge_adapters.sh` to prove a solver
    adapter cannot emit kernel evidence, a runtime check cannot emit proof
    evidence, and a Lean/kernel adapter cannot depend on external solver state.
14. Make policies consume the final ledger instead of recomputing facts:
    forbid-assume, forbid-vacuous, solver-evidence policy, stale-proof policy,
    runtime-safety requirements, trusted-boundary policy, and release gates.
    Add `scripts/tests/check_obligation_policy_views.sh` with one fixture per
    policy decision and one negative case proving stale/vacuous/solver-trusted
    evidence is rejected when policy says so. A temporary consistency gate is
    allowed during migration, but the item is not complete until policy
    enforcement reads `ObligationCore` records directly and the old
    side-channel qualifier collection is deleted.
15. Make reports consume the ledger: `--report contracts`, `--report vcs`,
    `--report proof-status`, `--report check-proofs`, audit bundles, release
    bundles, JSON, snapshots, and evidence corpus gates become views over the
    same records. Add `scripts/tests/check_obligation_report_views.sh` to assert
    the same stable ids and statuses appear in every report surface. A
    consistency gate proves parity during migration; it does not replace the
    full renderer refactor. This item is complete only when report renderers
    read the hub model, not `Report.VC` / proof-status side structures.
16. Make `concrete prove` consume the ledger: `--json`, `--show-obligation`,
    `--emit-lean`, `--emit-artifacts`, `--workspace`, `--check`, `--replay`,
    `--nearest-lemmas`, and future `--minimize` should not reconstruct
    obligation context independently. Add
    `scripts/tests/check_obligation_prove_views.sh` to compare prove JSON,
    workspace files, emitted Lean, replay commands, and check output against the
    ledger for the same obligation ids. This item is complete only when the
    prove surface obtains obligation context, statuses, replay commands,
    nearest-lemma hints, and check targets from `ObligationCore`, not from a
    private prove/report reconstruction path.
17. Add a migration parity gate after each migrated obligation family:
    compare old and new human reports, JSON, policy behavior, stable ids,
    counterexamples, solver provenance, and proof-workspace output on the
    existing corpus before deleting the old path.
18. Collapse the duplicate obligation models so there is ONE truth source, then
    delete the old report/prove/policy-specific models and walkers.
    `ObligationCore` is today a lossy
    projection of `Report.VC` (it drops `smtHash`/`smtQuery`/`solver`/
    `dischargeMode`/`leanReplay`, and never carried the contract/proof-link
    presentation fields), so consumers still read `VC`/`ProofCore.Obligation`
    directly and `ObligationCore` is a leaf, not the hub. Making it the hub is a
    prerequisite for #14/#15/#16 being real consumers rather than consistency gates,
    so this is staged, each sub-step verified byte-identical before the next:
    - 18a. Widen `ObligationCore.Obligation` to a SUPERSET of the VC surface
      (solver provenance, discharge mode, replay) and make `ofVC` lossless. Pure
      addition — no output change.
    - 18b. Make `--report vcs` render from `ObligationCore` (not `VC`),
      byte-identical — the first real report-as-view over the hub.
    - 18c. Make the audit VC summary and the obligation JSON consume the hub,
      byte-identical.
    - 18d. Fold the `ProofCore.Obligation` proof-status surface into the hub and
      reduce `Report.VC` to a view/alias (or delete it). No obligation family may
      keep two live truth sources. Add
      `scripts/tests/check_no_duplicate_obligation_walkers.sh` to fail on
      reintroduced family-specific collectors or report-side recomputation.
    - 18e. Delete compatibility shims that allow reports, policies, or prove to
      bypass the hub: no `collectVCs`-only report path, no policy-side
      `compute*Quals` side channel, no prove-side obligation reconstruction, and
      no proof-status-only table that is not projected from the hub.
    - 18f. Add a negative source guard:
      `scripts/tests/check_obligation_single_truth_source.sh` must fail if new
      code introduces `Report.VC` as a storage model, a second proof-status
      obligation record, or a family-specific scoped walker outside the
      approved collector/backend adapters.
    The presentation-rich reports (`--report contracts`, `--report proof-status`)
    convert to literal hub consumers only once their fields live in the model;
    until then the #15 consistency gate holds them to the ledger.
19. Add the Phase 3 validation artifact: one fixture project that exercises
    every migrated obligation kind and proves the ledger is the only truth
    source by checking contracts, VCs, proof status, audit, policy, JSON,
    workspace, replay, and release-bundle output for the same stable ids. Use
    `examples/obligation_core_probe/` and
    `scripts/tests/check_phase3_obligation_core.sh` as the final umbrella gate.

## Phase 4: CompilerLedger Pipeline And Typed IR

Goal: make the ordinary compiler pipeline a typed, replayable fact pipeline.
Project loading, parsing, resolution, canonicalization, type checking,
ownership, capabilities, lowering, diagnostics, source maps, interpreter,
backend, and command plumbing should produce named artifacts and write their
facts into one `CompilerLedger` / `ProjectFacts` structure. Reports, editor
tooling, crash bundles, release bundles, cache keys, and backend validation
must render those facts instead of recomputing them.

Design reference: [docs/COMPILER_PIPELINE.md](docs/COMPILER_PIPELINE.md).
Research reference:
[docs/COMPILER_PIPELINE_RESEARCH.md](docs/COMPILER_PIPELINE_RESEARCH.md).

Done when: every user-facing command loads the same typed project context; each
pass emits a typed artifact with id, input ids, output ids, consumed facts,
produced facts, diagnostics, source maps, timing, replay command, and verified
invariants; all compiler facts live in `CompilerLedger`; diagnostics share one
schema; interpreter and compiled execution can be compared through one harness;
and backend/target assumptions are explicit before Phase 5 language usability
work depends on them.

1. Define `ProjectContext`: source roots, modules, entry points, tests,
   policies, assumptions, target profile, build profile, oracle manifests,
   source maps, toolchain identity, and command mode. All commands must load
   this once instead of growing command-specific project discovery.
   This is the Gleam-style product lesson: one coherent toolchain surface, not
   separate mini-compilers behind `build`, `test`, `fmt`, `audit`, and `prove`.
2. Define `CompilerLedger` / `ProjectFacts`, the non-proof compiler fact store.
   It must contain names, modules, imports, types, ownership facts, capability
   facts, diagnostics, source maps, pass artifacts, pass timings, target facts,
   backend assumptions, emitted files, cache/dependency facts, and links to the
   `ObligationCore` ledger. Required API shape: `recordArtifact`,
   `recordDiagnostic`, `recordFact`, `recordDependency`, `recordTiming`,
   `recordSourceMap`, and `recordReplayCommand`. Add
   `scripts/tests/check_compiler_ledger.sh`; the gate must prove `build`,
   `test`, `audit`, `prove`, `inspect`, and `doc` read the same project facts
   rather than constructing command-local fact stores.
3. Split the frontend into named pass outputs: parsed AST, resolved AST,
   canonical IR, typed surface IR, ownership-checked IR,
   capability-checked IR, contract/ghost/assert metadata, lowered Core, and
   backend IR. Each pass must state what facts it consumes and produces, and
   must write its artifact id into `CompilerLedger`.
4. Define a `ResolvedAST` representation with stable resolved names,
   module-qualified references, proof/spec names, source spans, and import
   provenance. Later passes should not redo textual name lookup.
5. Add an explicit canonicalization pass between resolution and type checking.
   It should remove surface-only syntax before the rest of the compiler sees
   it: normalize patterns, desugar `if let`/`while let` once they exist,
   normalize attributes, resolve field puns, make early-return and fall-through
   edges explicit, and keep source-span provenance for diagnostics and reports.
6. Define a multi-level IR policy: no pass may erase a fact until every
   downstream consumer has either consumed it or copied it into a typed fact
   table. Capabilities, source spans, ownership facts, failure points, and
   target assumptions must not disappear during lowering.
7. Define a `TypedIR` representation after type checking but before proof/
   obligation lowering. It should carry expression types, lvalue/rvalue
   classification, array/struct/enum shapes, control-flow form, and source
   spans.
8. Attach ownership and capability facts once, in the checked IR, rather than
   rediscovering them in reports. Reports may render these facts; they should
   not re-infer them from raw syntax.
9. Define one diagnostic schema and renderer: diagnostic code, severity, source
   span, message, reason, help/next action, related spans, command context, and
   optional machine-readable payload. Parser, resolver, type, ownership,
   capability, policy, runtime-obligation, and backend diagnostics must use it.
10. Treat diagnostics as compiler data, not formatted strings. Passes should
   emit structured diagnostics first; human text, JSON, LSP output, tests, and
   release bundles should render the same diagnostic records.
11. Add rich Elm/Gleam-style diagnostic rendering as a checked compiler
    feature, not an incidental formatting pass. The renderer must support:
    source snippets with underlines, primary and secondary spans, related-span
    notes, reason/help/next-action sections, expected-vs-actual facts, policy
    or evidence context when relevant, and stable diagnostic codes. It must
    render from the same structured diagnostic records used by JSON/LSP, not
    from hand-formatted strings. Add `examples/diagnostics_rich/` with exact
    fixtures for parser error, unknown name, type mismatch, ownership/linearity
    error, missing capability, array-bounds obligation failure, solver-policy
    rejection, vacuous contract, and stale proof. Wire
    `scripts/tests/check_rich_diagnostics.sh`; the gate must assert human and
    JSON output agree on code, severity, spans, related spans, reason, help,
    and evidence/policy payload, and that redaction/source-location privacy
    modes still apply.
12. Add error-tolerant partial facts for tooling and reports: parser,
    resolver, typechecker, ownership, and capability passes should be able to
    produce partial artifacts containing explicit `invalid` / `unknown`
    placeholders where safe. This is the LSP/product lesson from Gleam, Dafny,
    Lean, Rust, and Swift: one bad expression should not erase unrelated
    diagnostics, formatting, docs, hover, import facts, or audit context. Any
    partial fact must be labelled as partial and must never feed codegen,
    proof, policy, or release claims as if it were complete.
    Staged:
    - 12a. Cross-pass tolerant *diagnostics* driver (`runFrontendDiagnostics`)
      that returns only `Diagnostics` + a `partial` flag — structurally
      incapable of producing `ValidatedCore`, so it can never feed
      codegen/proof/policy. When name resolution fails it still runs the
      typechecker on the (always structurally complete, side-table-resolved)
      program, so a bad reference in one function no longer erases type
      diagnostics elsewhere. `partial` is set whenever a pass was skipped.
      [DONE]
    - 12b. Run ownership + capability (elaborate→coreCheck) diagnostics on the
      tolerant path too, guarded so unresolved/ill-typed input never reaches them
      (only when resolve AND check are clean, matching the strict precondition).
      Cascade suppression drops a later-pass diagnostic that echoes an earlier
      pass's diagnostic at the identical span. [DONE — folded into 12a]
    - 12c. Parser error recovery: resync at top-level declaration boundaries so a
      single bad declaration yields its own diagnostic and the well-formed
      declarations after it still parse and flow downstream (resolve/check), with
      recovered parse errors collected in the threaded parser state. The strict
      `parse` now reports every recovered error, not just the first.
      [DONE — decl-level; finer statement-level recovery within a body is future]
    - 12d. Explicit `unknown`/`invalid` placeholder nodes for the typed IR so
      downstream tooling (hover, docs, fmt) can render partial facts, each
      marked partial.
13. Preserve source maps through every lowering boundary: AST -> TypedIR,
   TypedIR -> Core, Core -> backend IR, generated C/LLVM/native debug info,
   runtime failures, audit facts, and proof/obligation artifacts.
   Staged:
   - 13a. AST→Core function-granularity: `CFnDef.declSpan` carries the function
     declaration span across elaboration (which previously dropped all spans —
     Core/SSA had zero span fields). Core-check diagnostics (capability, etc.)
     now point at the offending function instead of being location-less.
     `check_source_maps.sh`. [DONE]
   - 13b. Carry `declSpan` Core→SSA→emitted-symbol so backend diagnostics,
     generated C/LLVM, and debug info can name the source function. `SFnDef`
     gains `declSpan`; `lowerFn` carries it (and mono preserves it through
     specialization); the SSA dump (`--emit-ssa`) names each function's source
     line (`; source: divide @ line 4`). `check_source_maps.sh`. [DONE — SSA
     dump; LLVM DWARF debug info is the heavier follow-on, with 13d's finer
     spans.]
   - 13c. Attach the originating function/decl span to proof obligations and
     audit facts (ObligationCore), so witnesses/counterexamples cite source.
     Split by source of the function:
     - 13c1. Project-code obligations cite source (file, line). Verified and
       gated (`check_source_maps.sh`: `main.divide → file:4`). [DONE]
     - 13c2. Dependency/stdlib obligation source locations. [DEFERRED] Two
       blockers: `buildFnLocMap` stamps the entry-point path as the file for
       every module (so a dependency function's span line is paired with the
       wrong file), and the obligation-naming origin of stdlib-qualified names
       (e.g. `sha256.bsig0`) is not yet pinned — main.con neither declares nor
       imports those modules. Needs per-module source-file tracking (a loader
       change) plus obligation-origin investigation. NOT bodged with suffix
       matching: a wrong file in an audit tool is worse than an honest unknown.
   - 13d. Expression/statement-granularity spans in Core (the invasive step):
     thread spans onto Core nodes or a node→span side table for precise
     within-function obligation and runtime-failure locations.
14. Normalize command plumbing for `build`, `run`, `test`, `audit`, `prove`,
   `inspect`, `fmt`, `doc`, and `clean`: shared project loading, shared target/
   policy/assumption loading, shared diagnostics, shared output conventions,
   and shared exit-code taxonomy.
   Staged:
   - 14a. Shared project-root prologue (`withProjectRoot`) — one canonical
     "no Concrete.toml" diagnostic and one exit code for every project-scoped
     command (build/run/test/check/`--report compiler-ledger` deduped from 5
     copies). Shared exit-code taxonomy (`ExitCode`) is the single source for
     both the exit values AND the generated `EXIT CODES` help block, so codes
     and docs cannot drift; `prove` routes through the named constants.
     `check_cli_plumbing.sh`. [DONE]
   - 14b. Shared command-arg parsing: `Cli.hasFlag` / `Cli.flagValue` give one
     definition of a boolean and a valued flag, replacing scattered inline
     `args.contains` / `dropWhile` (esp. the ~15-flag prove block). The valued-
     flag `--`-guard (so `--out --json` does not capture `--json`) is now applied
     uniformly — several sites previously omitted it. `check_cli_plumbing.sh`.
     [DONE]
   - 14c. Route `audit`/`prove`/single-file reports through the same project
     loading + diagnostics path as build/test, so a file in a project sees its
     policy/assumptions/deps uniformly. Split:
     - 14c1. Dependency resolution: `compileAndReport` already routes through
       `loadProject` in project mode, so audit/prove on a stdlib-using project
       file resolve deps exactly as build/test. Verified and gated
       (`check_cli_plumbing.sh`). [DONE]
     - 14c2. Reuse `ctx.registry`/`ctx.pc`/`ctx.policyLocMap` from `loadProject`
       instead of recomputing them, and reflect the project `[policy]`/
       assumptions in audit/prove. [DEFERRED] Risky: the report path computes
       these over the FULL user package with a different `file` stamp
       (`inputPath` vs `mainPath`, which feeds the 13c2 file-attribution
       behavior), and whether audit should ENFORCE policy is a design choice —
       not a mechanical dedup. Needs care, not a forced swap.
15. Add a golden CLI behavior matrix before broad command growth:
    `scripts/tests/check_cli_contract.sh` must cover every public command's
    exit code, stdout/stderr split, `--json` behavior, quiet/verbose behavior,
    artifact output location, missing-file behavior, malformed-input behavior,
    and policy-failure behavior. Fixtures must include `build`, `run`, `test`,
    `audit`, `prove`, `inspect`, `fmt`, `doc`, `clean`, and one unknown command.
    Done when command UX is a checked contract, not an accidental consequence
    of each command's implementation.
    Staged:
    - 15a. First slice (existing commands): `check_cli_contract.sh` pins build,
      run, test, check, prove, `--report`, `--version`, no-args, unknown
      command, missing `Concrete.toml`, and malformed input — exit code,
      stdout/stderr split, `--json` well-formedness, and artifact location.
      Exposed and fixed an uncaught-exception on unknown command / missing input
      (now a clean `ExitCode.usage` error). Future commands (inspect/fmt/doc/
      clean) are asserted as NOT-YET (clean failure), not invented. [DONE]
    - 15b. Extend the matrix to inspect/fmt/doc/clean and policy-failure
      behavior once those command surfaces exist (depends on later phases).
16. Define the compiler-internal API boundary before LSP, MCP, package tooling,
    or editor integrations import random modules. V1 boundary:
    `ProjectContext` loading, `CompilerLedger` queries, diagnostic rendering,
    pass inspection, artifact lookup, `ObligationCore` queries, and release
    bundle capture. Add `docs/COMPILER_API.md` and
    `scripts/tests/check_compiler_api_boundary.sh`; the gate must fail if
    editor/package/tooling code reaches into parser/checker/report internals
    instead of the boundary modules.
    Staged:
    - 16a. Boundary defined + guarded. `docs/COMPILER_API.md` names the V1
      allowlist (`Pipeline`, `CompilerLedger`, `ObligationCore`, `Diagnostic`,
      `DebugBundle`; ProjectContext loading is CLI-only for now). The gate scans
      consumer roots (editor/tools/integrations/lsp/mcp/plugins) for any `.lean`
      importing a non-allowlisted `Concrete.*` module or the bare umbrella, and
      self-tests against a good/bad fixture pair so it can never become a no-op.
      Doc ↔ gate allowlists are asserted to agree. `check_compiler_api_boundary.sh`.
      [DONE — guard first; routing real consumers through it is incremental]
    - 16b. Migrate `ProjectContext` loading out of `Main.lean` into a boundary
      module so consumers can load a project without the CLI; then add it to the
      allowlist. [DONE] `Concrete.Project` now holds `ProjectContext`,
      `loadProject`, `findProjectRoot`, and the dependency/TOML/module-resolution
      helpers (moved in four verified cuts: path/IO leaves → module resolution →
      TOML/deps/registry → ProjectContext/loadProject, building after each).
      Added to the boundary allowlist; a consumer probe imports ONLY
      `Concrete.Project` and loads a project at runtime (PROBE-OK), gated by
      `check_compiler_api_boundary.sh`. Main keeps only CLI glue.
17. Define the backend contract boundary: integer overflow profile, division
   semantics, layout/ABI, panic/assert behavior, optimization assumptions,
   target triple/data layout, libc/runtime assumptions, and what is trusted.
   Staged:
   - 17a. Make the contract VISIBLE: `--report backend-contracts [--json]`.
     `Concrete.Backend` is the single source for target triple / data layout /
     runtime functions / the contract clauses; the emitter (`EmitSSA`) and the
     report both read it, so the documented contract cannot drift from what is
     emitted. Clauses are honest about which guarantees are proof-linked (e.g.
     div-by-zero is a proof obligation, UB at runtime if undischarged) vs runtime.
     Program-derived trusted boundaries (trusted fns + externs) are listed.
     `check_backend_contracts.sh` (incl. a no-drift report-vs-emitted check). [DONE]
   - 17b. Expand the gate to a fixture matrix: arithmetic/division, structs/enums/
     layout, assert/panic path, capability/runtime call. Each asserts the report is
     well-formed, covers all topics, and stays drift-free (report target == emitted)
     regardless of program shape. The unknown/unsupported-target negative is NOT-YET:
     there is no `--target` selection surface, so the contract honestly advertises a
     single target and the gate asserts we are not silently multi-target (rather than
     inventing a target to reject). `check_backend_contracts.sh` 14/0. [DONE]
   - 17c. Surface backend assumptions in audit + release bundles, beside the
     proof/evidence facts.
   - 17d. Tighten actual backend checks (overflow/division/layout first, since
     those affect proof/runtime claims).
18. Add an interpreter-vs-compiled differential harness for ordinary language
    development. Every new executable language feature should be able to run
    through `interpret result == compiled result` where deterministic and
    target-independent.
19. Add pass inspection commands for compiler developers and users:
    `concrete inspect --ast`, `--resolved`, `--typed`, `--core`,
    `--backend-ir`, `--ledger`, with stable redaction of local paths and
    deterministic ordering. `--ledger` must render the same `CompilerLedger`
    records that reports and release bundles consume.
20. Add pass verifier gates, inspired by Swift's SIL verifier, Lean's IR
    checker, Zig's AIR/codegen bookkeeping checks, and Go's SSA validation:
    `concrete verify-ir --pass parsed|resolved|typed|core|backend-ir` must
    check structural invariants for each representation. Examples: resolved
    names must point at existing declarations; typed expressions must carry
    types; ownership facts must not mention dead locals; capabilities must be
    attached before lowering; Core must contain no source-only contract/ghost
    syntax; backend IR must preserve source maps and target assumptions. Wire
    this into `scripts/tests/check_ir_verifiers.sh`.
21. Add structured compiler pipeline events, borrowing the useful part of
    Dafny's pipeline events and Gleam's build telemetry:
    `concrete build --events --json` emits start/finish/fail events for
    project-load, parse, resolve, canonicalize, typecheck, ownership,
    capability, obligation collection, proof/evidence reports, codegen, link,
    and release-bundle capture. Events must include pass name, input artifact
    ids, output artifact ids, source counts, timing, diagnostic count, and
    command context.
22. Add crash/repro bundles for compiler bugs, inspired by Zig's crash context
    reports and Rust's ICE discipline: on an internal compiler failure,
    Concrete writes `.build/concrete-crash/<id>/` with `command.txt`,
    `toolchain.json`, `project.json`, redacted source inputs, last successful
    pass, current function/module/obligation id if known, structured
    diagnostics so far, and a replay command. User errors must never produce
    crash bundles; crash bundles are for compiler bugs only.
23. Add crash triage taxonomy for internal compiler failures. Crash bundles
    must classify `parser_ice`, `resolver_ice`, `canonicalizer_ice`,
    `typechecker_ice`, `ownership_ice`, `capability_ice`, `lowering_ice`,
    `obligation_ice`, `backend_ice`, `report_ice`, and `prove_ice`.
    Add `scripts/tests/check_crash_triage.sh` with one synthetic internal-error
    trigger per broad area where feasible. User errors must still be ordinary
    diagnostics, never crash categories.
24. Define canonical interned identities for names, types, literals, layouts,
    and target facts before broad caching or package artifacts. This is the
    Zig `InternPool` / Rust stable-id lesson adapted to Concrete: reports,
    fingerprints, obligation ids, package interfaces, incremental facts, and
    proof artifacts should use stable canonical ids instead of re-rendered
    strings where possible.
25. Define a query/dependency model for compiler facts before implementing
    broad caching. Name facts such as `parse(file)`, `resolve(module)`,
    `typecheck(function)`, `capabilities(function)`, `typed_ir(function)`,
    `core(function)`, `obligations(function)`, `audit_facts(function)`,
    `codegen(function)`, and `release_bundle(project)`.
    The shape should be query-first, Salsa/rust-analyzer style, but Concrete
    should not take a cache dependency until invalidation and diagnostics are
    stable.
26. Define incremental artifact dependencies: which source files/functions
    affect which diagnostics, facts, obligations, proof checks, generated code,
    inspect output, and release-bundle entries. Do not implement caching broadly
    until the dependency model is named and validated.
27. Eliminate hidden global compiler state before adding broad parallelism,
    caching, LSP, or package builds. All mutable compiler facts must be owned by
    `ProjectContext`, `CompilerLedger`, `ObligationCore`, an explicit pass
    artifact, or a clearly named cache object with declared invalidation. Add
    `scripts/tests/check_no_hidden_compiler_state.sh`; the gate must fail on
    new module-level mutable state, command-local fact stores, ad hoc global
    caches, or pass-local side tables that are consumed by later commands
    without being recorded in the ledger.
28. Add deterministic pipeline replay and serial/parallel equivalence before
    broad incremental compilation. The same project must produce the same
    diagnostics, artifact ids, source maps, ledger facts, obligation ids,
    emitted backend IR, and release-bundle facts under serial execution,
    parallel pass scheduling where enabled, clean builds, and replay from
    retained artifacts. Add `scripts/tests/check_pipeline_determinism.sh` over
    `examples/compiler_pipeline_probe/`; the gate must compare
    `--events --json`, `inspect --ledger`, `inspect --backend-ir`,
    `--json-errors`, and release-bundle summaries across repeated runs.
29. Add schema-version rejection gates for every machine-readable artifact
    while Concrete is moving fast. V1 stance: no old-schema compatibility and
    no migration promise before release. Every JSON/fact artifact must carry
    `schema_version`; a mismatched version must fail with a clear diagnostic
    naming the artifact kind, expected version, found version, and regeneration
    command. Covered artifacts: `CompilerLedger`, `ObligationCore`,
    diagnostics, pass artifacts, release bundles, audit bundles, proof
    workspaces, package artifacts, and backend IR manifests. Add
    `scripts/tests/check_schema_version_rejection.sh`.
30. Add compiler performance instrumentation before broad feature growth:
    `concrete build --timings --json` and `concrete report performance --json`
    must report parse/resolve/typecheck/ownership/capability/lowering/codegen/
    report/prove timings, peak memory if available, source-file/function counts,
    stdlib compile time, and toolchain identity. Performance data is a compiler
    fact, not a printed side channel.
31. Add compiler performance regression budgets:
    `scripts/tests/check_compiler_performance.sh` compares
    `tests/perf/small_project`, `tests/perf/stdlib_imports`,
    `tests/perf/proof_report`, and `tests/perf/codegen_loop` against committed
    JSON baselines. Done when CI fails on unexplained budget regressions and
    the report says which pass regressed.
32. Add correctness-gated compiler speed work, kept in its own fixture area.
    All compilation-speed experiments must live under
    `tests/perf/compiler_pipeline/` until promoted, and no speed change may land
    without both measurement and correctness evidence. Required sub-gates:
    `scripts/tests/check_compiler_perf_report.sh` proves
    `concrete build --timings --json` and `concrete report performance --json`
    are stable compiler-ledger views; `check_incremental_cache_correctness.sh`
    proves cache keys include source hashes, toolchain id, target/build profile,
    command mode, pass schema version, dependency facts, and obligation/proof
    fingerprints, and that stale inputs never reuse old facts;
    `check_parallel_determinism.sh` proves serial and parallel pass execution
    produce byte-identical diagnostics, artifact ids, source maps, ledger facts,
    obligation ids, backend IR, and release-bundle summaries;
    `check_lazy_artifacts.sh` proves expensive JSON/Lean/workspace/audit/source
    map artifacts are emitted only when requested or by an explicit gate; and
    `check_data_layout_parity.sh` proves any data-oriented table/SoA prototype
    is byte-identical at every public surface before measuring speed or memory.
    V1 candidates for measured table layouts are exactly diagnostics, tokens,
    typed/Core/backend IR instructions, `CompilerLedger` facts,
    `ObligationCore` views, symbol tables, and source maps. Do not introduce a
    general Zig-style reflection/comptime facility for this; Concrete may add an
    explicit checked table/layout abstraction only after the performance report
    proves a concrete structure is hot. Done when the separate fixture folder
    contains clean-build, incremental, stale-cache, parallel, lazy-artifact, and
    one narrow data-layout experiment, each with a committed before/after timing
    and a correctness assertion.
33. Add compiler fuzzing as a standing gate:
    `scripts/fuzz/parser`, `scripts/fuzz/resolver`,
    `scripts/fuzz/typecheck`, `scripts/fuzz/ownership`,
    `scripts/fuzz/formatter`, `scripts/fuzz/lowering`, and
    `scripts/fuzz/obligations`. Done when `make test-fuzz` runs a bounded CI
    budget and proves crashes, parser panics, malformed JSON, and false
    `proved_*` statuses are rejected.
34. Add fuzz minimization and fixture promotion:
    `scripts/fuzz/minimize` writes reduced repros into
    `tests/fuzz_regressions/<area>/<name>.con` with an expected diagnostic or
    honest non-proof snapshot. Done when every promoted repro is run by CI and
    no fuzzer-only failure stays outside the checked-in corpus for a release.
35. Document pass invariants and failure boundaries: what each pass guarantees,
    which errors are recoverable for reporting, which errors stop compilation,
    and which assumptions are trusted.
36. Add a compiler-pipeline regression corpus: malformed modules, ambiguous
    names, type errors, ownership errors, capability errors, source-map
    preservation, interpreter/codegen mismatch, backend assumption reporting,
    canonicalization edge cases, dependency invalidation, and deterministic
    `inspect` output.
37. Add metamorphic compiler tests. Equivalent source changes must preserve the
    relevant facts: local variable renaming, independent declaration ordering,
    whitespace/comments, equivalent parentheses, harmless block splitting,
    import ordering where semantics are unchanged, and equivalent literal
    spelling. Wire `scripts/tests/check_metamorphic_compiler.sh`; the gate must
    compare diagnostics, resolved ids where expected, typed/Core/backend facts,
    obligations, release-bundle summaries, and compiled output, while allowing
    source spans and source hashes to differ where they honestly should.
38. Add source-location privacy modes, borrowing the useful part of Odin's
    source-location controls but making them audit-visible:
    `[build] source-location-mode = "normal" | "filename" | "obfuscated" |
    "none"` in `Concrete.toml`, plus `--source-location-mode <mode>` for
    one-off commands. The mode must affect human diagnostics,
    `--json-errors`, crash bundles, audit bundles, release bundles, generated
    C/LLVM/native debug info, proof workspaces, and emitted pass artifacts
    consistently. Add `scripts/tests/check_source_location_modes.sh` with
    `tests/programs/source_locations/{normal,filename,obfuscated,none}.con`.
    Done when local CI still sees full spans under `normal`, release bundles
    record `source_location_mode`, and redacted artifacts never pretend that
    redaction is proof or evidence.
39. Add JSON diagnostic parity as a named gate, not just a renderer option:
    `concrete build --json-errors` or the equivalent command mode must emit the
    same diagnostic codes, spans, reasons, related spans, next actions, and
    payloads as the human renderer. Wire `scripts/tests/check_json_diagnostics.sh`
    with parser, resolver, type, ownership, capability, policy, backend, and
    internal-error fixtures.
40. Add artifact-retention and emitted-pass files for debugging, inspired by
    Odin's keep-temp-files workflow and QBE's printable IL discipline:
    `--keep-artifacts`, `--emit-ast`, `--emit-resolved`, `--emit-typed-ir`,
    `--emit-core`, `--emit-backend-ir`, `--emit-asm`, and
    `--emit-link-command`. Artifacts must live under `.build/concrete-artifacts/`
    by default, obey source-location privacy mode, and include
    `artifact-manifest.json` with `command`, `toolchain`, `target`,
    `build_profile`, `source_location_mode`, `pass_ids`, `input_hashes`,
    `output_files`, and `replay_command`. Add
    `scripts/tests/check_emit_artifacts.sh` with
    `examples/compiler_pipeline_probe/` and one negative case proving
    `--emit-backend-ir` is unavailable before backend IR exists rather than
    silently emitting stale output.
41. Add artifact garbage-collection policy and `concrete clean` modes before
    retained artifacts, proof caches, crash bundles, and package artifacts grow
    without bound. Required modes: `concrete clean --build`, `--artifacts`,
    `--proof-cache`, `--crash-bundles`, `--all`, and `--dry-run --json`.
    Default clean must remove ordinary build outputs but preserve crash bundles
    unless explicitly requested. Wire `scripts/tests/check_clean_artifacts.sh`
    to prove no source, proof, release bundle, or manually retained crash repro
    is deleted accidentally.
42. Add compiler self-audit: `concrete audit --compiler` renders the
    `CompilerLedger` itself. Required output: passes run, artifact ids,
    diagnostics count, source-location privacy mode, target/toolchain identity,
    solver/tool versions, cache/dependency facts, replay commands, backend
    assumptions, emitted files, and links to the `ObligationCore` ledger. Wire
    `scripts/tests/check_compiler_self_audit.sh`; the gate must prove the
    self-audit is generated from `CompilerLedger`, not from text scraping.
43. Keep the backend IR printable, verifier-checked, and regression-testable
    directly. This is the QBE lesson adapted to Concrete: even if LLVM remains
    the backend, Concrete's own backend contract should be a stable emitted
    artifact with a verifier, not an opaque stream of generated code. V1 must
    cover exactly these backend constructs: integer arithmetic, fixed arrays,
    structs, enums, direct calls, bounded loops, branches, runtime checks,
    capability calls, and source-map annotations. Add
    backend-IR golden tests under `tests/backend_ir/`:
    `arith.con`, `calls.con`, `if_loop.con`, `structs.con`, `arrays.con`,
    `runtime_checks.con`, `capabilities.con`, `source_maps.con`, and
    `target_constants.con`. Wire `scripts/tests/check_backend_ir.sh` to run
    `concrete inspect --backend-ir`, `concrete verify-ir --pass backend-ir`,
    and a compiled execution check for each fixture.
44. Add the Phase 4 validation artifact:
    `examples/compiler_pipeline_probe/` plus
    `scripts/tests/check_phase4_pipeline.sh`. The fixture must run
    `concrete build`, `run`, `test`, `fmt --check`, `inspect --ast`,
    `inspect --resolved`, `inspect --typed`, `inspect --core`,
    `inspect --backend-ir`, `verify-ir --pass typed`,
    `verify-ir --pass backend-ir`, `build --events --json`,
    `build --json-errors`, `build --keep-artifacts`,
    `inspect --ledger`, `audit --compiler`, `report performance --json`,
    `clean --dry-run --json`, and `audit`; compare interpreter-vs-compiled
    output; assert source-map spans and source-location privacy modes survive;
    assert dependency facts are stable; assert every pass artifact has input
    ids, output ids, diagnostics, facts consumed/produced, timing, replay
    command, and verifier status; assert rich human diagnostics and JSON/LSP
    diagnostics come from the same records; assert schema-version mismatches
    reject clearly; assert metamorphic variants preserve the right facts; assert
    crash/repro bundles are emitted only for deliberate internal compiler
    failures; and run the fuzz-regression fixtures without relying on
    proof-specific machinery.

## Phase 5: Language Usability And Daily Workflow

Goal: make Concrete usable as a normal experimental language, independent of
whether a user is writing proofs.

Done when: a new user can format, build, run, test, diagnose, inspect, and
debug small Concrete programs with predictable commands and useful errors.

The first six items are the **Phase 5 core slab**. Build them before the
external-validation trial, medium real-workload examples, or any contract/VC
syntax that depends on collections, bytes/text/path, or project layout. The
rest of Phase 5 stays after that slab in the same linear queue.

1. Stabilize modules and imports before packages grow: module names, file
   layout, visibility, import resolution, cycle diagnostics, and generated
   interface summaries.
2. Add a minimal project model before full packages: `Concrete.toml` fields for
   name, entry points, tests, policies, assumptions, source roots, build
   profiles, target profiles, oracle manifests, and evidence gates. The file
   must make authority, assumptions, runtime-check policy, and proof policy
   visible; it must not become an ambient hidden configuration channel.
3. Add `concrete test`: discover and run user tests, example tests,
   expected-failure tests, interpreter-vs-compiled differential tests,
   snapshot tests, oracle tests, and policy/assumption gates through one
   command.
4. Improve diagnostics for parser, resolver, type checker, ownership, linearity,
   capability, unsupported-construct, and codegen/interpreter mismatch errors:
   every diagnostic has a source span, reason, and next action.
5. Define strings, bytes, paths, and OS strings: `Bytes` for raw data, `Text`
   for validated UTF-8, and `Path`/`OsString` for OS-native boundaries. Specify
   literals, ownership, slicing, indexing, formatting, conversions, parser/JSON
   interaction, diagnostics, and test output. No implicit lossy conversion.
6. Define the collections story: fixed arrays, slices, dynamic `Vec`, maps,
   buffers, parser cursors, and which collections require `Alloc` or other
   capabilities.
   - 6a. Decide narrow const generics before fixed-capacity collections become
     stdlib APIs. This is load-bearing for the no-allocation story, not just an
     expressiveness feature: `BoundedVec<T, N>`, `RingBuffer<T, N>`,
     `PacketBuf<N>`, fixed hash tables, parser scratch buffers, and embedded
     queues must not require one monomorphic type per capacity. V1 is limited
     to integer literals and constant expressions that the compiler can
     evaluate without general comptime execution; no type reflection, no
     generated methods, no arbitrary compile-time programs. Monomorphization
     must record the concrete `N` in the compiler ledger, layout report,
     obligation ids, proof/evidence artifacts, and backend contracts. Add
     `docs/CONST_GENERICS_V1.md`, `examples/const_generics/{bounded_vec,ring_buffer,packet_buf}/`,
     and `scripts/tests/check_const_generics_v1.sh`; the gate must prove
     distinct capacities specialize separately, layout is capacity-specific,
     runtime-safety obligations name the instantiated size, and unsupported
     non-integer/comptime-reflection forms are rejected.
7. Add `concrete fmt`: stable formatting for source files, examples, docs
   snippets, and generated fixtures. Formatting must not churn semantic
   fingerprints.
8. Add `docs/GRAMMAR.md`: LL(1) grammar, reserved keywords, attribute syntax,
   contract syntax, `ghost`/`assert`/`assume`, iteration syntax, and negative
   parser fixtures. This is a syntax reference, not a language-design
   committee.
9. Add plain type aliases before the larger stdlib/examples slab:
   `type Digest = [u8; 32]`, `type Tag = [u8; 16]`, etc. Aliases must be
   transparent to layout, extraction, and proof unless explicitly declared as a
   future opaque/newtype form. This is an ordinary readability feature, not a
   proof abstraction.
10. Decide user-facing loop control before broad parser/service examples:
    `break`, `continue`, and whether labeled loops exist. The decision must
    state how each interacts with bounded-loop analysis, cleanup/defer,
    contracts, and runtime-safety obligations. Write
    `docs/LOOP_CONTROL.md`, add fixtures under `tests/programs/loop_control/`,
    and wire `scripts/tests/check_loop_control.sh`. If `break`/`continue` are
    admitted, the gate must cover single loop, nested loop, early cleanup,
    invariant preservation, variant/decrease, and runtime-safety facts. If they
    are deferred, the gate must pin the rejection diagnostics and show the
    explicit-state alternative in `examples/daily/packet_decoder`.
11. Close the match/pattern ergonomics gap before broad `Result`/`Option` and
   protocol-decoder work. This is one compound usability block: algebraic data
   types are already in the language, so the pattern language must be
   expressive enough to use them without stacks of boilerplate matches.

   **Current baseline:** `_` wildcard *match arms*
   (`tests/programs/wildcard_pattern.con`); integer/value patterns (`litArm`)
   and variable patterns (`varArm`); `let`-destructuring including `let … else`
   (`letDestructure` with `elseBody`); struct destructuring
   (`let Struct { fields } = expr;`, `letStructDestructure`).

   **Still open (each: implement, or explicitly defer with examples):**
   - match guards — `Pattern if condition => …`
   - OR patterns — `A | B => …`
   - integer range patterns — `0x30..=0x39 => …`, exclusive/open ranges only
     if the grammar and exhaustiveness checker can explain them clearly
   - `if let`
   - `while let`
   - nested patterns
   - match-on-reference ergonomics for enums and structs behind `&T`/`&mut T`:
     decide whether auto-deref in patterns exists, where it stops, and how
     mutable-borrow frame facts are reported
   - tuple types, or a deliberate no-tuples decision (record which)
   - struct update syntax — `Struct { field: x, ..base }`
   - `_` wildcard *inside destructuring bindings* (distinct from the `_` match
     arm, which is done) — still deferred.

   **Suggested order:** integer range patterns and match-on-`&T` first (they
   immediately improve decoders, parsers, and interpreter-shaped workloads),
   then `if let` / `let … else` documentation and examples (they exercise the
   existing destructuring with the least new machinery), then OR patterns or
   struct update — whichever hurts more in practice (parser/service code tends
   to want OR patterns; SHA-style state updates tend to want `..base`). Add
   `examples/patterns/{byte_ranges,ref_enum_match,parser_token_match}/` and
   extend `scripts/tests/check_pattern_ergonomics.sh`; the gate must cover
   range exhaustiveness, unreachable-arm diagnostics, reference-pattern
   behavior, and negative ambiguity cases.
12. Define numeric literal and cast rules: suffixes, inference/default integer
   type, signed/unsigned comparisons, narrowing, widening, checked/proved/
   wrapping overflow profiles, and diagnostics for ambiguous or lossy casts.
13. Define resource cleanup semantics: `defer`, drop/cleanup ordering,
    early-return cleanup, failure during cleanup, move-after-defer behavior, and
    linear-value interaction.
14. Define the FFI language surface: `extern` syntax, layout restrictions,
    ABI/calling convention annotations, ownership crossing the boundary,
    capability/trust requirements, and what cannot be expressed safely.
15. Define C/ABI glue-generation UX before FFI examples grow: whether Concrete
    emits C headers, imports declarations from C headers, generates host stubs,
    or exposes a narrow `concrete ffi`/`concrete bindgen` command. Generated
    glue must carry layout/ABI assumptions, ownership boundary rules,
    capability/trust labels, source spans, and reproducible output.
16. Define language-visible build profiles: debug/release, overflow checks,
    assertions, runtime checks, optimization assumptions, and proof/audit
    compatibility.
17. State the macro/metaprogramming stance for v1: no unrestricted macro
    system. Allow only controlled, audited compile-time generation /
    derive-like helpers for boring repeated artifacts such as equality,
    debug/display, serializers/parsers, proof stubs, contract boilerplate, and
    small table generation. Generated code must preserve source spans and
    evidence/audit traceability.
18. Define handle-relative filesystem APIs as the preferred capability shape:
    directory/file handles are capabilities; privileged code should operate
    relative to opened handles rather than repeated ambient path lookup. The
    design must address TOCTOU risks, path normalization, symlinks, temp files,
    and byte-preserving OS boundary behavior.
19. Add ignored-result diagnostics for fallible APIs: discarding `Result`,
    `Option`, or runtime-check results is a warning/error unless explicitly
    acknowledged with `_ = ...`, `ignore(...)`, or a policy-approved pattern.
20. Track accumulating error sets for `Result`-heavy code, without adopting row
    effects. V1 is report-only: `concrete audit --report error-sets` and
    `concrete inspect --error-sets --json` over existing enum-returning
    functions. Add `examples/error_sets/protocol_pipeline/` with
    `ParseErr`, `IoErr`, and `AuthErr`, plus
    `scripts/tests/check_error_sets.sh`. The gate must prove call composition
    accumulates variants, dead variants are not reported, ignored-result
    diagnostics still fire, and no syntax resembling row polymorphism is
    accepted.
21. Evaluate units-of-measure / dimensional annotations for common systems
    mistakes by shipping a report-only prototype first. Add
    `examples/units_probe/` with bytes-vs-bits, milliseconds-vs-seconds,
    block-count-vs-byte-offset, protocol-length, and memory-size cases. Add
    `scripts/tests/check_units_probe.sh` and `docs/UNITS_OF_MEASURE.md`. V1
    annotations are optional and erased only after audit records the
    conversion; no implicit unit conversion, no dependent numeric refinements,
    and no proof status may depend on unit erasure unless a contract/VC names
    the conversion explicitly.
22. Add source style guidance alongside `concrete fmt`: idiomatic layout for
    functions, modules, contracts, matches, error handling, examples, and
    proof-bearing code.
23. Decide the v1 iteration protocol before broad stdlib work. Evaluate and
    document the replacement for closures/trait-object iterators in
    `docs/ITERATION_PROTOCOL.md`:
    index-based `for i in 0..len { xs[i] }`, explicit cursor/iterator structs
    with `next() -> Option<T>`, and monomorphized `for_each`-style helpers. The
    decision must cover `Vec`, slices, maps, parser cursors, and interpreter
    workloads, and must explain how authority and allocation remain visible.
    Add `examples/iteration_protocol/{slice_cursor,vec_iter,map_iter,parser_cursor}/`
    and `scripts/tests/check_iteration_protocol.sh`; the gate must report
    allocation/capability behavior for each iteration form and prove bounded
    loops stay visible to runtime-safety obligations.
24. Decide explicit callable values and capability-polymorphic callbacks as one
    model before adding `map`/`fold`/`for_each` families, structured
    concurrency, or callback-heavy stdlib APIs. This replaces the narrower
    "capability polymorphism only" question with the full design knot:
    bare function pointers, bound callback values, explicit context structs,
    callback mutability/linearity modes, and capability polymorphism must be
    specified together.

    The design must avoid a combinatorial split like `map`, `map_file`,
    `map_alloc`; the expected use-site shape remains explicit capability-set
    polymorphism such as
    `fn map<T, U, C>(xs, f: fn(T) with(C) -> U) with(C) -> ...`, grounded in
    `research/language/capability-polymorphism.md`. The doc must compare two
    bound-callback representations explicitly:
    - struct-level capability parameters, e.g. `BoundFn<Ctx, A, R, Caps>`;
    - Concrete-shaped use-site binding, where the capset lives on the inner fn
      type (`call: fn(&Ctx, A) with(C) -> R`) and `C` is bound by the caller's
      ordinary function-level capability polymorphism.

    The design must specify all three callable modes, without hidden captures:
    shared context (`fn(&Ctx, A) -> R`), mutable context (`fn(&mut Ctx, A) ->
    R`), and consuming context / `FnOnce` shape (`fn(Ctx, A) -> R` or an
    explicit one-shot wrapper). It must say when the bound value is copyable,
    movable, linear, or consumed; how reborrowing of `&mut Ctx` works; what
    happens when `Ctx` holds linear resources; how `for x in ...` desugars
    against this model; and how bound callbacks render in capability,
    allocation, audit, and release reports.

    The proof story is part of the same item: a call through a bound callback
    must have a named obligation/evidence shape, and the design must choose
    whether proofs are per monomorphized instance, generic once, or generic
    contracts with instance-level proof artifacts. Audit output must
    distinguish `proved_for_instance` from any future `proved_generic` class.
    Add `docs/CALLABLE_VALUES_AND_CAPABILITIES.md`,
    `examples/callbacks/{bare_fn,bound_shared,bound_mut,bound_once,cap_polymorphic_map}/`,
    and `scripts/tests/check_callable_values.sh`; the gate must prove hidden
    captures are rejected, callback capabilities are not erased, context
    resources remain visible, the three consumption modes behave differently,
    and proof/evidence reports name the instantiated callable shape.
    - 24a (design gate — prerequisite, not implementation). The widened design
      doc (`docs/CALLABLE_VALUES_AND_CAPABILITIES.md`) must exist BEFORE the
      Phase 6 stdlib iteration/HOF surface lands. Today's stdlib
      higher-order functions pin a fixed capability set in the fn-pointer type
      (e.g. `HashMap::fold`'s `f: fn(A, &K, &V) -> A` in `std/src/map.con`),
      which forces the combinatorial split above and does not give explicit
      standing to stateful callbacks. The smuggling hole this item originally
      named — the checker's fn-pointer call path did not require the caller to
      hold the fn type's declared capability set, so a function with no
      `with(...)` could accept `f: fn(i32) with(Network) -> i32` and call it —
      is CLOSED (2026-06-09): calling through a function pointer now requires
      the fn type's capability set in both Check and CoreCheck, locked by
      `adversarial_neg_cap_fnptr_smuggle.con` (rejected, E0240) and
      `cap_fnptr_declared.con` (positive) in the main suite. The design doc
      still owes the callable-values model before any `map`/`fold`/`for_each`
      family hardens. Red-team gate:
      `scripts/tests/check_capability_polymorphism_design.sh` must fail if the
      stdlib gains new HOF surface without the widened design doc, and must
      keep the smuggling fixture rejected.
25. Define the stdlib handoff contract for Phase 6. Phase 5 decides the
    language surfaces the stdlib depends on — modules/imports, project model,
    tests, diagnostics, bytes/text/path types, collections, narrow const
    generics for fixed-capacity APIs, iteration, explicit callable values,
    capability-polymorphic callbacks, build profiles, and CLI verbs — but the actual
    library APIs are built in the dedicated stdlib phase. Write
    `docs/STDLIB_HANDOFF.md` and gate it with
    `scripts/tests/check_stdlib_handoff.sh`; the gate must assert each required
    Phase 5 surface has a status of `stable_for_stdlib`, `provisional_with_gate`,
    or `blocked`, and Phase 6 may not start while any required surface is
    `blocked`.
26. Design user-facing testing framework UX before `std.test` hardens:
    test discovery (`#[test]` versus naming convention), expected failures,
    capability-scoped fixtures, temp files without ambient authority, oracle
    tests, interpreter-vs-compiled tests, proof-status interaction, and how test
    failures appear in `concrete audit`. The user-facing output must include
    `concrete test --json` with a stable event stream comparable to Go's
    `test2json`: discovered, started, passed, failed, skipped, expected-failed,
    oracle-compared, policy-blocked, and proof-status events.
27. Add `concrete lint` / `concrete vet` before public examples grow:
    semantic warnings that are not type errors, including ignored fallible
    results, suspicious capabilities, unreachable contracts, redundant
    runtime checks, likely path/bytes/text confusion, unstable public API use,
    and release-policy warnings. Every lint must have a diagnostic code,
    source span, machine-readable JSON payload, and an explicit allow/deny
    policy.
28. Add debug/trace mode: `concrete run --trace`, interpreter step traces, Core /
    lowered-IR dumps, source spans in runtime errors, and stable replay commands
    for report/debug failures.
29. Add developer profiling/coverage commands for Concrete programs:
    `concrete run --profile`, `concrete test --coverage`, and
    `concrete trace --json`. These are ordinary debugging surfaces, not proof
    evidence; audit output must label them `tested/profiled`, never `proved`.
30. Add interactive evidence commands for low-ceremony feedback without a live
    mutable REPL: evaluate a function with concrete inputs, inspect Core and
    ProofCore for one function, show the current generated obligation, and
    replay a failing proof/debug report. Target commands include
    `concrete eval`, `concrete inspect --core`, `concrete inspect --proofcore`,
    `concrete prove --show-obligation`, and `concrete run --trace`.
31. Add basic LSP/editor diagnostics early: parse/type errors, capability
    summaries, hover for inferred types, and jump-to-definition. Deeper
    proof/evidence LSP features remain in the later editor phase.
32. Decide target-conditional code selection before freestanding and
    cross-platform stdlib work harden. Prefer profile-selected source roots and
    modules in `Concrete.toml`; if narrow `cfg` attributes are added later, they
    must be LL(1)-safe, small, target/profile-only, and reported in audit.
33. Define compiler-known target constants as ordinary, audit-visible compile
    facts, not hidden preprocessor state. Inspired by Odin's builtin target
    constants, Concrete should expose names such as `CONCRETE_OS`,
    `CONCRETE_ARCH`, `CONCRETE_ENDIAN`, `CONCRETE_TARGET_PROFILE`,
    `CONCRETE_BUILD_PROFILE`, and `CONCRETE_TOOLCHAIN_VERSION` only through the
    resolved/typed fact layer. Reports and release bundles must record which
    constants affected compiled source, proofs, obligations, or stdlib module
    selection. Add `examples/target_constants/` and
    `scripts/tests/check_target_constants.sh`; the gate must prove constants
    appear in `concrete inspect --resolved --json`, `--report audit`, and the
    release bundle, and that a target-dependent branch is labelled
    `target_selected` rather than erased silently.
34. Normalize the CLI around predictable verbs:
    `concrete build`, `concrete run`, `concrete test`, `concrete fmt`,
    `concrete lint`, `concrete vet`, `concrete audit`, `concrete prove`,
    `concrete eval`, `concrete inspect`, `concrete doc`, `concrete bench`,
    `concrete trace`, and `concrete clean`.
35. Add `concrete doc`: generate basic API/reference docs from source,
    capabilities, modules, and public comments without depending on proof
    infrastructure. Outputs must include `concrete doc --format json` and
    `concrete doc --format html`, so editor tooling and published docs reuse
    the same source.
36. Add a first-user tutorial path for C/Rust developers that does not start
    with proofs: install, hello world, values and fixed arrays, ownership,
    borrows, capabilities, explicit errors, tests, compiled debugging, audit,
    then proof-bearing examples. The tone should be "ordinary systems code with
    visible evidence," not proof-assistant ceremony.
37. Add useful non-proof examples: a small CLI tool, a protocol decoder, a
    bounded cache, and a capability-scoped file/console program. Use exact
    examples `examples/daily/word_count`, `examples/daily/packet_decoder`,
    `examples/daily/lru_cache`, and `examples/daily/file_summary`. Wire
    `scripts/tests/check_daily_examples.sh` to build, run, compare
    interpreter-vs-compiled output, assert `--report effects`, assert
    `--report audit`, and pin one negative fixture per example under
    `examples/daily/*/catches/`.
38. Add basic benchmarking UX: `concrete bench` runs small benchmarks, compares
    interpreter versus compiled performance, emits `--json`, and detects
    obvious generated-code regressions. This is separate from compiler
    performance budgets in Phase 4.
39. Document the memory model for ordinary users: move/copy/drop behavior,
    cleanup, borrows, linear values, trusted/Unsafe escape hatches, definite
    assignment, and what is rejected. State the invariant explicitly: safe
    Concrete has no uninitialized reads by construction; trusted/FFI memory may
    carry explicit assumptions.
40. Add cross-platform build sanity for the supported host set: macOS and Linux
    first, with CI coverage, reproducible commands, and documented toolchain
    expectations.
41. Add the Phase 5 validation project: a small C/Rust-style CLI using the core
    slab plus daily workflow (`Concrete.toml`, modules/imports,
    `concrete test`, bytes/text/path and collection decisions, narrow const
    generics, pattern ergonomics, callable/capability callback decisions,
    diagnostics, formatting, docs, lint/vet, benchmark/profile/coverage smoke
    tests, and trace/debug commands). CI must build, run, test, format-check,
    lint, audit, record compiler-known target constants, and compare
    interpreter-vs-compiled behavior on macOS and Linux. It validates the
    language/tooling slab, not the full stdlib.

## Phase 6: Standard Library And Core APIs

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
    The freestanding target implementation still lands in Phase 15.
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
37. Add a stdlib compatibility/oracle corpus under
    `examples/stdlib_compat/`: `fmt_parse_vectors`, `bytes_text_vectors`,
    `path_vectors`, `collection_vectors`, `base64_vectors`, `uri_vectors`,
    `json_vectors`, `semver_vectors`, `sort_search_vectors`,
    `checksum_vectors`, `rand_vectors`, and `cli_io_vectors`. Wire it with
    `scripts/tests/check_stdlib_compat.sh`; every vector must declare exactly
    one mode in `manifest.toml`: `oracle_python`, `oracle_system_tool`,
    `interp_vs_compiled`, `audit_only`, or `negative_expected_failure`.
38. Add real stdlib workload checks before Phase 7 relies on the library:
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
39. Add the Phase 6 validation project:
    `examples/stdlib_client/` plus `scripts/tests/check_phase6_stdlib.sh`.
    The client must use `std.option`, `std.result`, `std.bytes`, `std.text`,
    `std.path`, `std.vec`, `std.map`, `std.fs`, `std.io`, `std.fmt`,
    `std.parse`, either `std.json` or `std.base64`, deterministic RNG or
    checksums, sanitizer/runtime-instrumentation hooks where available, and
    `std.test`. CI must build, run, test, audit authority/allocation/evidence
    classes, and compare interpreter-vs-compiled behavior.

## Phase 7: Flagship Depth And Examples

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
    Phase 5/11/12 surface is complete. Target:
    `examples/credibility_slice/packet_window/` or an equivalent protocol-like
    example that exercises all of: explicit capabilities, one runtime-safety
    obligation, one source contract, one Lean-checked proof, one
    `proved_by_kernel_decision` discharge, one SMT counterexample or
    solver-trusted residue, one property-test counterexample, one oracle or
    interpreter-vs-compiled check, and one audit/release bundle. Wire
    `scripts/tests/check_credibility_slice.sh`; the gate must compile and run
    the example, replay the proof/evidence checks, persist any counterexample
    as a regression, and produce a README transcript that a non-author can run
    without understanding compiler internals. This is intentionally a vertical
    slice, not a new parallel track; it exists to validate the bet before the
    later 10k-line workload ladder.
16. Add a graduated real-workload ladder. The goal is to make sure Concrete
    builds real things that can be checked against references, not only tiny
    proof demos. Each workload must name the surface or public claim it forces;
    otherwise it does not belong in this phase. Do not jump straight to multiple
    10k-line ports before the Phase 5 core slab, Phase 6 stdlib, and daily
    workflow can support them; that would mostly test missing ergonomics.
    Sequence:
    - **Main compiler repo:** keep tiny proof patterns
      (`examples/proof_patterns/`), evidence-class examples, small real programs
      that gate the compiler, and showcase flagships here. These protect
      compiler/proof correctness and should stay close to the tests.
    - **Medium in-repo real programs after the Phase 5 core slab and Phase 6
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
      for the Phase 15 `with(Device)`/MMIO evidence decision or explicitly
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
    external validation, or a release-facing tutorial.
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
19. Add the Phase 7 validation artifact: a showcase/workload dashboard that
    proves every flagship and graduated workload has a check story, evidence
    bundle, oracle or reference when appropriate, interpreter-vs-compiled
    coverage, property-test/counterexample-regression coverage where relevant,
    runtime-obligation audit, trust/assumption classification, and release-CI
    replay. The first external-user workload in this dashboard is the
    external-validation-gate trial.

## Phase 8: Proof Authoring And Automation

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
   block Phase 5 unless spec ownership or proof authoring starts depending on
   it. Target shape: `Concrete.ProofCore` owns `PExpr`, `PVal`, evaluation,
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
22. Add the Phase 8 validation artifact: a proof-authoring project that
   exercises `--json`, `--show-obligation`, `--emit-lean`, `--emit-artifacts`,
   `--workspace`, `--check`, `--nearest-lemmas`, `--minimize`, and source-linked
   proof attachment across straight-line, array update, loop copy, fold,
   composition, bounded quantified specs, ghost, stale, missing, partial, and
   repair cases. The gate must typecheck generated stubs, reject any
   `proof-registry.json`, and verify that failing Lean proofs map back to stable
   obligation ids.

## Phase 9: Audit Commands And Review Artifacts

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
20. Add the Phase 9 validation artifact: one package-scale audit bundle fixture
    with human and JSON output, semantic diff before/after a change, artifact
    viewer smoke test, oracle manifest, property-test manifest, persisted
    counterexample regression, spec-provenance facts, redaction check, replay
    command, and a README showing how a reviewer answers authority, proof,
    trust, assumption, and runtime-obligation questions without reading compiler
    internals.

## Phase 10: Proof Status And Trust Gates

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
17. Add the Phase 10 validation artifact: a trust-gate pressure project that
    includes transitive proof dependencies, stale dependency propagation,
    tool-version drift, proof-corpus migration across a simulated toolchain bump,
    assumption widening, spec-adequacy policy, vacuity downgrade, solver
    portfolio / disagreement handling, evidence mutation testing, axiom
    inventory and clean-checkout replay, weaker-evidence
    monotonicity, and a release gate proving each status cannot be silently
    presented as stronger evidence.

## Phase 11: Provable And Predictable Subsets

Goal: give users a named small subset they can rely on for serious
proof/evidence work.

Done when: the subset family has public names, allowed constructs, rejected
constructs, arithmetic profiles, runtime-error policy, and compatibility
promises.

1. Define `PredictableV1`: no allocation unless bounded, no FFI unless trusted
   and assumed, no unbounded loops/recursion, explicit failure-path policy.
2. Freeze the first arithmetic profiles:
   wrapping, checked, and proved/no-overflow.
3. Make arithmetic profiles explicit named semantics, not inherited backend
   habit. The default profile decision must be source-visible and
   project-visible: e.g. debug may trap/check, release may require
   obligation-or-wrap by policy, and intentionally wrapping arithmetic must be
   written or inferred only through a named profile. A theorem about wrapping
   code is not a theorem about trapping/checked code; the active arithmetic
   profile is part of the spec semantics for any proved function. Carry
   arithmetic profile choices into diagnostics, reports, assumptions, proof
   obligations, release bundles, and backend contracts. Add
   `docs/ARITHMETIC_PROFILES_V1.md`, `examples/arithmetic_profiles/{wrap,checked,proved_no_overflow,profile_mismatch}/`,
   and `scripts/tests/check_arithmetic_profiles.sh`; the gate must prove the
   same source expression is classified differently under different named
   profiles, that proof/evidence artifacts record the profile, and that a
   profile mismatch cannot be presented as compatible evidence.
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
18. Add the Phase 11 validation artifact: a profile matrix project covering
    `PredictableV1`, `ProvableV1`, unprofiled-float exclusion, profiled-float
    admission, borrow/reference proof-class decisions, constant-time source
    shape, stack/runtime-failure assumptions, and negative examples for every
    exclusion. The gate must prove reports never call excluded code proof
    eligible.

## Phase 12: Runtime Safety Obligations

Goal: generate SPARK-like obligations for boring runtime failures instead of
relying only on examples and prose.

Done when: parser/security examples can show obligations for bounds, div/mod
zero, overflow profile, casts, and loop bounds with statuses
`proved`, `enforced`, `assumed`, `missing`, or `blocked`.

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
18. Add the Phase 12 validation artifact: a runtime-safety corpus covering
    bounds, div/mod-zero, overflow, casts, panic/abort/assert, byte/text/path
    boundaries, stack/recursion, inferred invariant candidates, newtype
    invariant hypotheses, arithmetic-profile mismatches, and obligation
    suppression. Each case must show one of `proved`, `enforced`, `assumed`,
    `missing`, or `blocked`, include a negative variant, and run through policy
    gates plus human/JSON reports.

## Phase 13: Compiler Soundness Bridge

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
   version/availability dependency (see Phase 10 proof-corpus migration); the TCB
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
13. Add the Phase 13 validation artifact: a compiler-soundness dashboard with
    one witness program per shipped ProofCore construct, one status per
    R-rule, replay commands for proved/mechanically-validated facts, and
    regressions proving report facts (`proved`, `stale`, `blocked`, `missing`,
    `ineligible`, `trusted`) agree with compiler state.

## Phase 14: Backend, Target, And Stdlib Contracts

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
13. Harden stdlib stability and evidence policy from Phase 6: which stdlib functions are
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
19. Add the Phase 14 validation artifact: a backend/std-lib contract project
    with ABI/layout C round trips, C/ABI glue generation/import checks,
    the C ABI classification matrix, backend-IR emission/verifier checks,
    translation-validation checks, sanitizer runs, compiled-oracle differential
    tests, native debug/source-map smoke tests, clean-vs-incremental fact
    equivalence, and stdlib authority/allocation/evidence gates.

## Phase 15: Freestanding And Embedded Target

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
11. Add the Phase 15 validation artifact: one freestanding demo project plus an
    MMIO/device-profile mock audit bundle. The demo must build with no hosted
    APIs, name allocator/startup/linker assumptions, reject hidden libc or
    allocation, and report `with(Device)`/`with(Mmio)`/`with(Unsafe)` evidence
    classes without pretending hardware behavior is proved.

## Phase 16: Public Release Bar

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
      fails the release gate, mirroring the Phase 10 #15
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
    evidence class without `needs_recheck`). The Phase 18 migration tooling
    (`concrete migrate`) implements this policy; the policy itself is a
    release-bar document. Gate: `concrete api-diff` (#6) must classify every
    public-surface change as `compatible`, `deprecated(window)`, or
    `breaking`, and the release gate fails on `breaking` without a recorded
    policy exception.
19. Ship the first narrow public release only after the above are green.
20. Add the Phase 16 validation artifact:
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

## Phase 17: Packages And Dependency Evidence

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
8. Add module/package authority budgets after package graphs are real.
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
    surface, then add `docs/EVIDENCE_TYPED_IMPORTS.md`,
    `examples/package_evidence_imports/{requires_lean,allows_solver_trusted,rejects_stale,rejects_vacuous}/`,
    and `scripts/tests/check_evidence_typed_imports.sh`; the gate must prove
    dependency evidence is read from package artifacts, not source-private side
    channels, and that an evidence downgrade breaks the importing package.
16. Add the Phase 17 validation artifact: a multi-package workspace project
    with dependency resolution, lockfile, package-aware tests, interface/body
    artifact split, dependency trust policy, assumption inheritance, authority
    budgets, provenance, evidence-typed imports, published docs, and
    release-bundle evidence for every dependency. Wire it as
    `examples/package_workspace/` plus
    `scripts/tests/check_phase17_packages.sh`.

## Phase 18: Editor And Human Tooling

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
7. Add backwards-compatibility regression corpus once public users exist.
8. Language/versioning/deprecation policy: MOVED to Phase 16 #18b — the
   policy must exist before the first public release; only the tooling that
   implements it lives here.
9. Add migration/deprecation tooling after the policy exists (Phase 16 #18b):
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
12. Add the Phase 18 validation artifact:
   `scripts/tests/check_phase18_editor.sh` runs a scripted LSP/editor session
   or golden transcript over one real project, proving hover, diagnostics,
   obligation navigation, proof/evidence facts, dependency audit UI, refactor
   behavior, docs integration, deprecation diagnostics, and playground output
   match CLI facts rather than inventing a second truth source.

## Phase 19: Concurrency And Research-Gated Extensions

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
14. Add the Phase 19 validation artifact: one pressure-test sketch, expected
    report, and decision record for every research-gated extension
    (concurrency, typestate, arena allocation, WCET, binary-format DSLs,
    hardware capability mapping, Miri-style interpreter, sized evaluator,
    persistent rewrite state, and row effects). No research item graduates
    unless its forcing example, report shape, evidence class, and rejection or
    pull-forward criteria are recorded.
