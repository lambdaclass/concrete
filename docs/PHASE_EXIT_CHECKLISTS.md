# Phase Exit Checklists

Status: canonical reference — **re-keyed 2026-06-09 to the current ROADMAP
phase structure.** The roadmap was restructured in 2026-06 (obligation/compiler
pipeline consolidation moved ahead of language/stdlib broadening); this file
previously used the pre-restructure phase numbering, so its "Phase N" headings
disagreed with ROADMAP.md and its status lines had gone stale (e.g. the old
"Phase 4: Tooling" claimed 0/12 done while the formatter, fuzzers, wrong-code
corpus, and `concrete reduce` all shipped). The historical checklists are
preserved at the bottom as completed-milestone records.

Each phase has a "phase closes when..." list tied to concrete outputs. A phase
is not done until every exit criterion has a verifiable artifact. This prevents
roadmap drift into never-finished thematic work.

Rule of authority: **ROADMAP.md owns the item lists; this file owns the exit
bar.** A phase's exit criteria here must name the phase's validation artifact
(the umbrella gate script) rather than restate every item — restating items is
how this file went stale last time.

## Completed phases (current numbering)

- **Phase 1: Source contracts** — closed 2026-06-06. Items and evidence in
  [CHANGELOG.md](../CHANGELOG.md) ("Phase 1 source contracts completed").
  Umbrella gate: `scripts/tests/check_phase1_contracts.sh` (still wired in CI;
  closure is re-verified, not historical).
- **Phase 2: VC and SMT core** — closed 2026-06-07. Items and evidence in
  [CHANGELOG.md](../CHANGELOG.md) ("Phase 2 VC and SMT core completed").
  Umbrella gates: `make test-phase2-vc`, `make test-smt-replay`,
  `make test-smt-redteam`.

## Phase 3: ObligationCore Pipeline Consolidation — CLOSED (core-complete, 2026-06-20)

Audited and closed; see [PHASE3_OBLIGATION_CORE_AUDIT.md](PHASE3_OBLIGATION_CORE_AUDIT.md)
and the ROADMAP Phase 3 CLOSED banner. The hub is the single truth source for
policy, `--report vcs`, `--report obligation-ledger`, obligation JSON, the audit
VC summary, and `--report contracts`; records unified, `ofVC` lossless; families
#4–#11 (incl. assert/assume/vacuity, #8) in the ledger; discharge-adapter firewall
kernel-checked; validation artifact `check_phase3_obligation_core.sh` present.

**Deferred (consistency-gated, sound today — not active risk, do not block
closure):** convert `--report proof-status` and `concrete prove`'s obligation
facts from consistency-gated recompute to literal ledger views (#15/#16 tail).

## Phase 4: CompilerLedger Pipeline And Typed IR — CLOSED (core-complete, 2026-06-20)

Audited and closed; see [PHASE4_COMPILER_LEDGER_AUDIT.md](PHASE4_COMPILER_LEDGER_AUDIT.md)
and the ROADMAP Phase 4 CLOSED banner. Core #1–#17 done and gated; the audit
verified **no active soundness or dual-truth-source risk** in the open tail.

**Deferred (workload-gated #18–#45, build only when a real workload pulls it):**
obligation/proof/policy facts → structured `Diagnostic`/JSON (#11 bridge);
interpreter-vs-compiled differential harness; `inspect`/`verify-ir`/`--events`/
`clean`/`audit --compiler`; schema-version gates; perf budgets / fuzzing /
minimization; source-location privacy modes; backend-IR as a stable artifact;
docs-drift *semantic* checks (beyond the artifact-existence gate); and the
defense-in-depth ref-return lowering fix (#44g, unreachable from safe code). The
#45 validation artifact (`examples/compiler_pipeline_probe/`) is part of the
deferred tail.

## Phases 5+

Exit checklists for Phases 5 and later are defined when the prior phase
approaches completion — premature exit criteria for distant phases drift as
the language evolves. Each phase's final roadmap item is its validation
artifact; that artifact becomes the exit bar here when the phase opens.

Standing exit requirements for every phase (from the roadmap preamble):
positive, negative, regression, and at least one adversarial/red-team fixture
per item; gates named in the item text and wired into CI; no false green, no
stale cache, no silent trust upgrade, no side-channel recomputation, no hidden
artifact, no user-facing claim without a replay command.

## Updating this document

When a phase exit criterion is met, check the box and add the commit hash or
gate name. When all boxes are checked, the phase is closed: add a closing date,
move the item list to CHANGELOG.md, and collapse the section here to a
two-line entry under "Completed phases".

---

# Archived: pre-restructure milestone checklists

The sections below are the original checklists under the OLD phase numbering
(before the 2026-06 roadmap restructure). They record real shipped milestones
— the work now underpins current Phases 5/6 prerequisites — and are kept for
provenance. Do not update them; they are historical.

## Milestone: Predictable Core (old "Phase 1")

Closed on the evidence below except one criterion (trust-gated canonical
example coverage, old items 32-34/39) which was folded into the current
roadmap's flagship/example work.

- Predictable boundaries documented: `docs/PREDICTABLE_BOUNDARIES.md`,
  `docs/PREDICTABLE_FAILURE_DISCIPLINE.md`, `docs/FAILURE_STRATEGY.md`
- Stack-depth reporting end-to-end (`--report stack-depth`, 25 trust-gate tests)
- Bounded-capacity types practical (`fixed_capacity`)
- Error propagation documented (`parse_validate`, `service_errors`)
- Example governance (`EXAMPLE_INVENTORY.md`, `EXAMPLE_LIFECYCLE.md`,
  `EXAMPLE_NO_DUPLICATES.md`)
- Diagnostic UX design (`docs/DIAGNOSTIC_UX.md`)
- Trusted boundary guide (`docs/TRUSTED_BOUNDARY_GUIDE.md`)
- Freestanding split defined (`docs/FREESTANDING_SPLIT.md`,
  `docs/STANDALONE_VS_PROJECT.md`, `docs/PROJECT_BOOTSTRAP.md`)
- Source-level interpreter as semantic oracle (`Concrete/Interp.lean`,
  `--interp`)

Verification: `./scripts/tests/run_tests.sh --trust-gate`.

## Milestone: Pre-Stdlib Pressure Workloads (old "Phase 2") — complete

All 36 pressure programs compile and run (or correctly fail for
error-expected cases): parser/decoder set (json_subset, http_request,
dns_header, dns_packet, binary_endian), ownership-heavy structures (tree,
ordered_map, arena_graph, intrusive_list, linear patterns), borrow/aliasing
patterns, trusted-wrapper/FFI pressure, fixed-capacity/no-alloc pressure,
cleanup/leak boundary. Gap findings were reconciled into the stdlib
requirements ledger (`docs/stdlib/STDLIB_AUDIT.md`).

## Milestone: Stdlib and Syntax Freeze (old "Phase 3") — complete (19/19)

The shipped stdlib/syntax surface froze on this evidence (2026-04-20 through
2026-04-25): string/text contract, byte-cursor API, checked indexing, 38 std
modules, runtime collections (`docs/RUNTIME_COLLECTIONS.md`), arithmetic
policy, formatting triad (no format strings), error ergonomics (`?`, Result
helpers), validated wrappers/newtypes (layout fix, cross-module identity,
method dispatch, cast exemption narrowed), `Type::Variant` qualification,
field punning + `let...else` + destructuring, visibility rules, endian APIs,
layout/ABI contract (`docs/LAYOUT_CONTRACT.md`), module hygiene, canonical
examples on the intended surface, and the `grep`/`lox` medium-workload
validation (`docs/stdlib/STDLIB_FREEZE_LEDGER.md`). Changes to the frozen surface
require explicit unfreezing per `docs/stdlib/STDLIB_SURFACE_FREEZE.md`.

## Superseded sections

The old "Phase 4: Tooling, Tests, Wrong-Code Corpus", "Phase 5: Performance,
Artifacts, Contract", "Phase 6: Release Credibility, Showcase", and "Phase 7:
Proof Expansion, Provable Subset" checklists are superseded by the current
ROADMAP's Phase 4 (pipeline + tooling items #30-#43), Phase 7 (flagships),
Phase 8 (proof authoring), and Phase 16 (public release bar). Their stale
status lines ("0/12 done") predated the formatter, fuzzers, wrong-code corpus,
and reducer that have since shipped; consult ROADMAP.md and CHANGELOG.md for
current truth.
