# Documentation Guide

Status: stable reference

This directory holds the stable reference docs for Concrete's implementation and language model.

## Authority And Ownership

The repository has many documents because it preserves design rationale as well as current
contracts. They do not all have equal authority. When two files disagree, use this order:

1. `docs/verification/CLAIMS_TODAY.md` for shipped user-visible guarantees.
2. Normative architecture/policy documents named below for stable design contracts.
3. The canonical trust-boundary/status block in `ROADMAP.md` for active implementation state.
4. `CHANGELOG.md` for landed historical milestones.
5. `research/` for dated, non-normative investigation.

No other document may introduce a competing "current status" dashboard. Detailed audit and phase
documents must link to their canonical owner and identify themselves as normative, explanatory, or
historical. A roadmap task owns sequencing; a design document owns semantics; neither duplicates
the other.

### Canonical owners by topic

| Topic | Canonical owner |
|---|---|
| shipped verification claims | [CLAIMS_TODAY.md](verification/CLAIMS_TODAY.md) |
| product verification commitment | [VERIFICATION_CHARTER.md](verification/VERIFICATION_CHARTER.md) |
| evidence objects and trust boundaries | [EVIDENCE_ARCHITECTURE.md](verification/EVIDENCE_ARCHITECTURE.md) |
| canonical proposition language | [VERIFICATION_IR.md](verification/VERIFICATION_IR.md) |
| proof status vocabulary | [VERIFICATION_STATUS.md](verification/VERIFICATION_STATUS.md) |
| threats, hostile artifacts and advisories | [THREAT_MODEL.md](verification/THREAT_MODEL.md) |
| compiler stages and ownership | [ARCHITECTURE.md](compiler/ARCHITECTURE.md), [PASSES.md](compiler/PASSES.md) |
| language invariants and value model | [LANGUAGE_INVARIANTS.md](language/LANGUAGE_INVARIANTS.md), [VALUE_MODEL.md](language/VALUE_MODEL.md) |
| contracts and verification conditions | [CONTRACTS_AND_VCS.md](verification/CONTRACTS_AND_VCS.md) |
| active implementation order/status | [../ROADMAP.md](../ROADMAP.md) |
| completed changes | [../CHANGELOG.md](../CHANGELOG.md) |

This table is an ownership map, not another summary of each document's contents.

Use these files as the primary reference once a design has moved out of exploration:

- [IDENTITY.md](project/IDENTITY.md) — what Concrete is optimizing for, where it intends to differentiate, and what it is not trying to be
- [WHY_CONCRETE.md](project/WHY_CONCRETE.md) — a direct overview for C and Rust developers: why Concrete exists, what it makes visible, and what it deliberately avoids
- [PRINCIPLES.md](project/PRINCIPLES.md) — the stable language principles: minimalist systems programming, no GC, linear/resource-aware ownership, explicit boundaries, predictable execution, and Lean-backed proof direction
- [INFLUENCES.md](project/INFLUENCES.md) — which external language ideas Concrete copies, adapts, or rejects
- [SAFETY.md](language/SAFETY.md) — capabilities, `trusted`, `Unsafe`, proof boundary, and high-integrity direction
- [HANDLE_CAPABILITIES.md](language/HANDLE_CAPABILITIES.md) — R-0484 design: `with(...)` as the complete list of external authority, handle capabilities (`Writer<C>`), `trusted` and the foreign boundary
- [PROFILES.md](platform/PROFILES.md) — named user-facing profiles: `safe`, `predictable`, `provable`, and the long-term `high-integrity` direction
- [CLAIMS_TODAY.md](verification/CLAIMS_TODAY.md) — the short public claim surface: what Concrete claims today, what it does not, and what remains trusted
- [VERIFICATION_CHARTER.md](verification/VERIFICATION_CHARTER.md) — the product-level verification commitment and its limits
- [EVIDENCE_ARCHITECTURE.md](verification/EVIDENCE_ARCHITECTURE.md) — the normative target model separating correspondence, validity, closure, replay receipts, and policy decisions
- [VERIFICATION_IR.md](verification/VERIFICATION_IR.md) — the target small canonical proposition language and its extension rules
- [PROOF_STORY_MATRIX.md](verification/PROOF_STORY_MATRIX.md) — per-construct proof/evidence story; the "no semantically dark constructs" inventory
- [TRUSTED_COMPUTING_BASE.md](verification/TRUSTED_COMPUTING_BASE.md) — explicit trusted-computing-base accounting for Concrete's strongest current claims
- [THREAT_MODEL.md](verification/THREAT_MODEL.md) — adversarial and accidental trust-weakening classes, including hostile evidence and checker advisories
- [PREDICTABLE_BOUNDARIES.md](platform/PREDICTABLE_BOUNDARIES.md) — host calls, cleanup paths, determinism sources, failure paths, and memory/UB boundaries for predictable/proved code
- [FAILURE_STRATEGY.md](language/FAILURE_STRATEGY.md) — abort-only decision, cleanup guarantees, FFI failure consequences, and proof-backed code failure assumptions
- [PREDICTABLE_FAILURE_DISCIPLINE.md](platform/PREDICTABLE_FAILURE_DISCIPLINE.md) — allowed and excluded failure forms for predictable-profile code
- [EXECUTION_MODEL.md](language/EXECUTION_MODEL.md) — runtime boundary, allocation model, FFI/runtime contract, and execution profiles
- [ARCHITECTURE.md](compiler/ARCHITECTURE.md) — compiler pipeline, artifact flow, pass boundaries, and architecture phase reference
- [PROOF_AUDIT_PIPELINE.md](verification/PROOF_AUDIT_PIPELINE.md) — target proof/audit pipeline: CheckedProgram, Core, normalized Core, ProofCore, obligations, shared facts, and pass contracts
- [PASSES.md](compiler/PASSES.md) — pass-by-pass contracts, ownership boundaries, and what each phase may assume
- [ABI.md](platform/ABI.md) — layout, enum representation, FFI-safety, and ABI boundary rules
- [DIAGNOSTICS.md](compiler/DIAGNOSTICS.md) — diagnostics model, current status, and staged diagnostics work
- [FFI.md](platform/FFI.md) — externs, raw pointers, and the `Unsafe` boundary
- [DESIGN_POLICY.md](project/DESIGN_POLICY.md) — feature-admission criteria and design filter
- [DECISIONS.md](project/DECISIONS.md) — first-class "no" and "not yet" decisions
- [LANGUAGE_SHAPE.md](language/LANGUAGE_SHAPE.md) — long-term structural commitments for the language
- [PROOF_WORKFLOW.md](verification/PROOF_WORKFLOW.md) — proof-authoring and maintenance workflow: choose, generate, write, attach, verify, diagnose, repair, refactor
- [PROOF_SYNTHESIS.md](verification/PROOF_SYNTHESIS.md) — LLM-guided proof synthesis target: AI searches for invariants/lemmas/proofs, Lean replay decides evidence
- [SPARK_CLASS_ASSURANCE.md](verification/SPARK_CLASS_ASSURANCE.md) — SPARK-class assurance target and AI-facing guide for contracts, loop invariants, frame/dependency facts, ghost/spec code, and package evidence
- [PROVABLE_V1.md](verification/PROVABLE_V1.md) — the first named, release-facing provable subset contract
- [PROVABLE_SUBSET.md](verification/PROVABLE_SUBSET.md) — current proof-eligible subset and its boundary
- [LANGUAGE_INVARIANTS.md](language/LANGUAGE_INVARIANTS.md) — the language rules that must hold across every phase
- [STDLIB.md](stdlib/STDLIB.md) — current stable stdlib direction and module priorities
- [TESTING.md](compiler/TESTING.md) — current test structure and what each suite is for
- [VALUE_MODEL.md](language/VALUE_MODEL.md) — value, borrow, ownership, and resource-model rules

The `book/` subdirectory is for tutorial-style and user-facing structured documentation.

## How To Read The Docs

- Read [../README.md](../README.md) first for project overview, current status, and build/test instructions.
- Read [VC_GENERATOR_DESIGN.md](verification/VC_GENERATOR_DESIGN.md) for the planned replacement of the four
  hand-written obligation walkers with a weakest-precondition calculus — the layer that produced
  every discovery defect found this year.
- Read [../ROADMAP.md](../ROADMAP.md) for active and future work. Start with its two
  orientation sections: **"Design honesty: this is largely Why3's architecture"** (what this
  design shares with the state of the art, what is better here, and the one layer that is worse)
  and **"North star"** (what "prove every kind of code in Lean + Rocq + Isabelle" decomposes
  into, and the two limits no amount of work removes).
- Read [../CHANGELOG.md](../CHANGELOG.md) for completed milestones.
- Read [../research/thesis-validation/core-thesis.md](../research/thesis-validation/core-thesis.md) and [../research/thesis-validation/objective-matrix.md](../research/thesis-validation/objective-matrix.md) if you want the current long-term thesis and the flagship validation examples.
- Read [PASSES.md](compiler/PASSES.md) early if you want to understand where compiler responsibility lives today.
- Use this `docs/` directory for stable reference material, not exploratory design notes.

## Scope Boundary

- `docs/` = stable reference and implementation contracts
- `research/` = exploratory notes, possible future work, and design investigations

If a topic is still being explored or debated, it belongs in `research/` first. Once it becomes a stable project rule or compiler boundary, it should move into `docs/`.

## Where New Docs Go

- stable rule/reference -> `docs/`
- active plan/sequencing -> `ROADMAP.md`
- landed milestone/history -> `CHANGELOG.md`
- exploratory note -> `research/`

## Document Lifecycle

Every new or materially rewritten document starts with a status line chosen from:

- `Status: normative contract` — current rules that code and gates must obey;
- `Status: stable reference` — explanatory current material whose normative owner is linked;
- `Status: active design` — a ratified design not yet fully implemented; current state remains in
  `ROADMAP.md`;
- `Status: historical` — retained rationale or audit record, never current status;
- `Status: exploratory` — belongs in `research/`, not `docs/`.

Promotion is a move, not a relabel in place: when research becomes a rule, create or move the
normative form into `docs/`, link the old research note to it, and leave the research note dated.
When a stable document becomes historical, move it under `docs/archive/` only after link checking
and add a replacement pointer. Do not reorganize files merely for aesthetics while active tasks
cite them.

## Physical Layout

Normative and stable documents are grouped by the boundary they describe. Reorganization remains
staged for future additions rather than allowing the root to become flat again:

1. establish canonical owners and status headers;
2. detect duplicate current-status claims and broken links;
3. move one topic family at a time, updating every reference in the same commit;
4. keep redirects/replacement pointers for externally cited paths where practical;
5. run documentation drift and link gates after each family.

Current families:

- `docs/language/` — language/value/ownership/contracts semantics;
- `docs/compiler/` — pipeline, passes, diagnostics and backend boundaries;
- `docs/verification/` — charter, evidence, VIR, proof workflow and trust model;
- `docs/platform/` — ABI, FFI, targets, runtime and freestanding boundaries;
- `docs/project/` — project identity, policy, decisions, examples and authoring conventions;
- `docs/archive/` — dated, non-authoritative execution history retained for traceability;
- existing `docs/stdlib/`, `docs/bugs/`, `docs/book/`, and `docs/NOTES/` remain distinct.

The canonical-owner table above remains the authority layer. Do not create compatibility copies
that can drift; update inbound repository links when a file moves, and use a pointer only when an
external durable path genuinely requires one.
