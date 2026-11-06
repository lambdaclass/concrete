research record, from archived branch wip/cleanup-docs-verification-plan; schedules nothing

# Consolidating docs/verification Into One Verification Specification

Status: Research (proposal, 2026-10-29). Measured on `origin/main` at `71df9c25`. Normative owner
once adopted: [R-0554](../../docs/archive/roadmap-2026-11/ROADMAP_TASKS.md#task-r-0554) item 5. This note moves,
deletes and edits no document; every policy choice it needs is listed under "Decisions for the
owner" and none is made here.

`docs/verification/` holds 59 files: 49 Markdown documents, 6 TSV data files, two Markdown records
under `identity_migrations/` and two text files under `checker-acceptance/`. Together they state
what Concrete's evidence means, what it trusts, how obligations arise and how proofs attach. No
document owns any one of those facts. The same claim appears in up to six documents, with
different wording, and several of them disagree (see "Overlaps and contradictions"). The language
went through the same process in October: [docs/spec/](../../docs/spec/README.md) became the one
written statement of the language, with versioned rule IDs, examples that run, and a gate
(`scripts/tests/check_spec.sh`, driven by `scripts/spec/spec.py`). This note proposes the same
treatment for verification. The result is one normative verification specification in the
docs/spec format, a rationale and design-record layer under it, registers that stay as data with
their gates, and an archive.

## 1. How the language specification is built

The parts this proposal reuses, from [docs/spec/README.md](../../docs/spec/README.md):

- **Chapters and prefixes.** `spec.py` reads the chapter inventory from README §1. A row has the
  form `` | `name.md` | `PREFIX-` | ``, matched by `^\| `([a-z]+)\.md` \| `([A-Z]+)-` \|`
  (`scripts/spec/spec.py:189-191`). Chapter names are lower-case letters only and prefixes are
  upper-case letters only, so `trusted_base.md` or `KRN2-` cannot be used.
- **Entries.** An anchor, a heading, one normative sentence `**ID@V.**`, then the fields `Kind`,
  `Uses`, `Codes`, `Status`, `Enforced by`, `Semantics`, `Obligation` and `Evidence`, a
  non-normative `**Rationale.**`, examples, and optional conformance fixtures (README §2.1).
- **Checks.** S1 format, S2 registry (`ids.tsv`, append-only, with a sentence hash per version),
  S3 history against the base, S4 examples run natively and under `--interp`, S5 coverage,
  S6 the diagnostic-code classification in `codes.tsv`, S7 sites (`spec: ID` comments at the
  `Enforced by:` declarations), S8 tracking IDs, S9 manifests, S10 the generated site,
  S11 no normative sentence restated outside docs/spec, S12 negative witnesses, and S13 one
  specification (`one_spec.py`, which requires every file under docs/language to be listed in
  `unabsorbed.tsv` and lets that list only shrink).
- **Existing links to verification.** `Evidence: trusted: <ID>` must name an ID that occurs in
  `docs/verification/TRUSTED_COMPUTING_BASE.md` (`spec.py:905-907`, a substring test). `codes.tsv`
  classifies the 14 proof and policy codes E0600, E0601, E0610–E0612, E0800–E0807 and E0810 as
  `tooling`, meaning "not language". [contracts.md](../../docs/spec/contracts.md) (CON-) and
  [trusted.md](../../docs/spec/trusted.md) (TRUST-) already send readers to verification
  documents: contracts.md:8-12 names CONTRACTS_AND_VCS.md and EVIDENCE_CLASSES.md as "the
  verification documentation" for how obligations are discharged and reported, and TRUST-2's
  rationale (trusted.md:66-69) points at TRUSTED_COMPUTING_BASE.md for the auditable trusted base.

## 2. Inventory

Roles used below:

- **normative**: the document says it binds (contract, canonical, normative), or a gate or the
  compiler treats it as the authority;
- **target**: a binding direction for work not yet shipped;
- **register**: per-row data that a gate parses or that must cover a code inventory;
- **guide**: how-to material for users;
- **record**: a dated decision, audit or closure record;
- **reference**: a description of the current system with no binding claim;
- **generated / data**: produced by a tool or frozen as a gate baseline.

"Readers" lists the scripts, workflows and Lean or Main sources that name the file. **(parsed)**
marks readers that grep or parse the file's content; the others mention it only in comments or
output strings. Sizes are bytes at `71df9c25`. KH is short for KNOWN_HOLES.md.

### 2.1 Markdown documents (49)

| document | bytes | states | role | readers |
|---|---:|---|---|---|
| [README.md](../../docs/verification/README.md) | 536 | index: read CLAIMS_TODAY, then the charter, EVIDENCE_ARCHITECTURE and VERIFICATION_IR. It names 4 of the 49 documents | reference (index) | none specific |
| [ASSUMPTION_FILES.md](../../docs/verification/ASSUMPTION_FILES.md) | 6,771 | `assumptions.toml` schema v1, the enforced fields, widening and drift | normative ("contract") | `check_assumptions.sh` (comment, line 9) |
| [AXIOMS.md](../../docs/verification/AXIOMS.md) | 13,498 | proof-layer trust: tier 0 kernel allowlist (`propext`, `Classical.choice`, `Quot.sound`), tier 1 native trust (`Lean.ofReduceBool`, `Lean.trustCompiler`, six theorems, four owners; the gate's legacy classifier also lists `Lean.ofReduceNat`, `check_axiom_inventory.sh:31`), tier 2 forbidden, re-check by con-leche and nanoda | normative (gate-enforced) | **(parsed)** `check_axiom_inventory.sh:114,123` (theorem names, allowlist); **(parsed)** `check_external_checker.sh:199` (axiom owners); `check_docs_drift.sh:83` (present-tense list); comments in `check_bv_certificates.sh`, `check_evidence_algebra.sh`, `check_checker_pin_freshness.sh`, `bv_capture_solver.sh`, `axiom_native_trust.txt`, `axiom_native_owners.txt`, `Concrete/Report/Evidence.lean:28`, `lean_action_ci.yml:2553`, `Makefile:141` |
| [CLAIMS_TODAY.md](../../docs/verification/CLAIMS_TODAY.md) | 21,264 | the public claim surface: enforced, proof-backed, reported and trusted claims, non-claims, TCB summary | normative ("canonical public reference"; "remains the authoritative statement", line 14) | **(parsed)** `check_docs_drift.sh:72,100` (paths and `--report` kinds must exist) |
| [CLAIM_TAXONOMY.md](../../docs/verification/CLAIM_TAXONOMY.md) | 17,085 | five composite labels (enforced, proved, reported, trusted assumption, backend/target assumption), their mapping to the report, CLI and JSON, and the forbidden conflations | normative for the current schema (the header defers to R-0440) | none |
| [CONTRACTS_AND_VCS.md](../../docs/verification/CONTRACTS_AND_VCS.md) | 35,266 | claim, obligation, evidence and audit architecture, contract syntax, contract IR, a 13-row evidence-class table, the SMT pipeline | target ("design proposal") but listed as present-tense | **(parsed)** `check_docs_drift.sh:81` |
| [EFFECT_PROOF_BOUNDARIES.md](../../docs/verification/EFFECT_PROOF_BOUNDARIES.md) | 25,065 | effect categories and their proof status, "the five proof eligibility gates", evidence dimensions (R-0484), boundary interactions | normative ("canonical reference") | none |
| [EVIDENCE_ARCHITECTURE.md](../../docs/verification/EVIDENCE_ARCHITECTURE.md) | 16,466 | the governing rule, five evidence objects (correspondence, validity, closure, replay receipt, policy decision), typed identities, authorship modes, independence, revocation, degradation | target ("normative design direction") | comments: `check_dependency_edges.sh:4047`, `check_one_producer.sh:77,159`, `Concrete/Proof/{ClassificationTable,Correspondence,DependencyEdge,ProofCore}.lean` |
| [EVIDENCE_CLASSES.md](../../docs/verification/EVIDENCE_CLASSES.md) | 6,952 | a catalog of five classes (proved, enforced, reported, assumed, trusted) plus tested and runtime safety, each with an audit line and an example | reference (no Status line) | none |
| [EVIDENCE_PRODUCER_MATRIX.md](../../docs/verification/EVIDENCE_PRODUCER_MATRIX.md) | 10,941 | every Expr, Stmt and pattern constructor classified for the evidence producer (identity, binding, gap code), plus one RATIFIED decision on evaluation order | register (no Status line) | **(parsed)** `check_evidence_tree.sh:342-360` (each Stmt constructor, the word "undecided"); **(parsed)** `check_convergence_inventory.sh:177,203` (the RATIFIED line); comments in `check_evidence_build.sh:130`, `Concrete/Proof/EvidenceBuild.lean:111` |
| [EXTERNAL_CHECKER.md](../../docs/verification/EXTERNAL_CHECKER.md) | 9,750 | how con-leche and nanoda run, pins and their freshness, dated bump evaluations | normative operation (runbook) plus records | `check_external_checker.sh:45`, `check_checker_pin_freshness.sh:9,89` (a warning text names it), `lean_action_ci.yml:481` |
| [GUARANTEE_STATEMENT.md](../../docs/verification/GUARANTEE_STATEMENT.md) | 12,173 | safe-code guarantees, proof-backed guarantees, what trusted code invalidates, backend assumptions, "four guarantee tiers" | normative ("the single precise statement", line 3) | none |
| [IDENTITY_MIGRATION_PLAN.md](../../docs/verification/IDENTITY_MIGRATION_PLAN.md) | 18,804 | R-0482 atomic migration plan (approved and implemented 2026-10-13) | record | comments: `check_identity_separation.sh:2`, `scripts/tools/identity_migration_map.sh:2` |
| [IDENTITY_SEPARATION.md](../../docs/verification/IDENTITY_SEPARATION.md) | 12,860 | ratified identity definitions D1–D7 (ScopeOrigin, PackageScopeIdentity, PackageArtifactIdentity, ResolutionContextIdentity, DefinitionIdentity, claim dependency root) and the measured baseline | normative definitions plus record | comments: `Concrete/Proof/DefinitionIdentity.lean:89`, `check_dependency_edges.sh` |
| [INTERPRETER_TRUST.md](../../docs/verification/INTERPRETER_TRUST.md) | 11,584 | the interpreter's trust boundary, executable subset, exclusions, arithmetic and failure | reference ("current reference") | comment: `Concrete/Interp/Interp.lean:20` |
| [KNOWN_HOLES.md](../../docs/verification/KNOWN_HOLES.md) | 80,893 | H- and C-series soundness gaps with OPEN or CLOSED status, a trust ledger (T1), stale "open design decisions", how to use the file | register (status authority) | **(parsed)** `check_hole_status_consistency.sh:28` (`### H<n>.` headings with OPEN or CLOSED); **(parsed)** `check_submodule_check_coverage.sh:166` (`### H12`); `check_docs_drift.sh:73,101`; comments in `check_known_wrong_corpus.sh`, `check_checker_pin_freshness.sh`, `Concrete/Report/{Evidence,Report}.lean` |
| [OBLIGATION_CORE.md](../../docs/verification/OBLIGATION_CORE.md) | 8,484 | the ObligationCore record, obligation kinds, scoped context, discharge adapters, the evidence ledger, the migration rule | normative ("canonical", per `ObligationCore.lean:9`; no Status line) | comment: `Concrete/Proof/ObligationCore.lean:9` |
| [PHASE3_OBLIGATION_CORE_AUDIT.md](../../docs/verification/PHASE3_OBLIGATION_CORE_AUDIT.md) | 11,363 | item-by-item audit of old Phase 3, dated 2026-06-15 | record | **(parsed)** `check_docs_drift.sh:74,102` |
| [PHASE4_COMPILER_LEDGER_AUDIT.md](../../docs/verification/PHASE4_COMPILER_LEDGER_AUDIT.md) | 11,100 | item-by-item audit of old Phase 4, dated 2026-06-17 | record | **(parsed)** `check_docs_drift.sh:75,103` |
| [PROOFKIT_GUIDE.md](../../docs/verification/PROOFKIT_GUIDE.md) | 11,804 | how to prove a function with ProofKit (HMAC flagship), proof shapes, modules, what not to do | guide (no Status line) | none |
| [PROOF_AUDIT_PIPELINE.md](../../docs/verification/PROOF_AUDIT_PIPELINE.md) | 12,941 | the target proof and audit pipeline (CheckedProgram, ProofCore, fact layer), open decisions | target ("not next implementation step") | none |
| [PROOF_CONTRACT.md](../../docs/verification/PROOF_CONTRACT.md) | 17,177 | what a proof attachment means: the artifact, implementation selection, typed dependency edges, the `proved` state, integrity states, reliance, a decision table | normative ("canonical reference") | none |
| [PROOF_EXTRACTION_GAPS.md](../../docs/verification/PROOF_EXTRACTION_GAPS.md) | 6,121 | extractor refusals found by workload proofs, 2026-10-21 and 2026-10-23 | record ("findings record") | none |
| [PROOF_LADDER.md](../../docs/verification/PROOF_LADDER.md) | 15,888 | the reusable proof layer, the discharge ladder (cheapest trust first), `assume` as a trapdoor, shipped against planned | mixed ("partly shipped, partly design") | comment: `Concrete/Proof/Sha256Spec.lean:9` |
| [PROOF_OBLIGATIONS_REGISTER.md](../../docs/verification/PROOF_OBLIGATIONS_REGISTER.md) | 37,273 | per-rule register of ProofCore extraction rules R-01 … R-28 and trust gates G-01 … G-06, with assumptions and the future theorem for each | register ("contract document") | comments: `Concrete/Proof/{Proof,ProofCore,ProofSoundness}.lean` cite rows R-01, R-02, R-16 and R-17 |
| [PROOF_PRESSURE_SET.md](../../docs/verification/PROOF_PRESSURE_SET.md) | 3,981 | the six-function fixture `examples/proof_pressure` and the state each function exercises | reference (fixture description) | none |
| [PROOF_SEMANTICS_BOUNDARY.md](../../docs/verification/PROOF_SEMANTICS_BOUNDARY.md) | 10,687 | language semantics against proof semantics: overlap, fixed-width arithmetic, excluded surface, how to read `proved`, the extraction bridge | normative ("canonical reference") | none |
| [PROOF_STATE_MODEL.md](../../docs/verification/PROOF_STATE_MODEL.md) | 14,539 | how PExpr models environment, arrays, structs, loop state and failure (functional updates, step functions, `none` as stuck) | target in its header ("design proposal"), but the Lean sources cite it as the encoding | comments: `Concrete/Proof/Proof.lean:267,2497`, `ProofCore.lean:1112,1205,1357`, `ProofSoundness.lean:567` |
| [PROOF_STORY_MATRIX.md](../../docs/verification/PROOF_STORY_MATRIX.md) | 9,133 | per-construct claim class and evidence ("no semantically dark constructs"), with six classes (adds `open`) | register ("stable reference") | output string: `Concrete/Report/Report.lean:6076` |
| [PROOF_SYNTHESIS.md](../../docs/verification/PROOF_SYNTHESIS.md) | 8,077 | LLM-guided proof synthesis: "LLM output is search, not evidence" | target ("old Phase 9 product bet") | none |
| [PROOF_THEOREM_SHAPES.md](../../docs/verification/PROOF_THEOREM_SHAPES.md) | 7,419 | theorem categories, naming rules, property forms, fuel convention, stub readability | guide ("active reference") | none |
| [PROOF_WORKFLOW.md](../../docs/verification/PROOF_WORKFLOW.md) | 21,556 | the proof workflow: eligibility, stubs, attachment attributes, verification, diagnostics, stale repair, refactors, commands | guide with normative parts ("canonical reference") | output string: `Concrete/Report/Report.lean:4118` |
| [PROVABLE_SUBSET.md](../../docs/verification/PROVABLE_SUBSET.md) | 6,289 | where the subset sits in the pipeline and why it has its shape; says it does not duplicate the allowlist | rationale ("standing architecture reference") | `scripts/gen/site_reference.py:32` (published as a site reference page) |
| [PROVABLE_V1.md](../../docs/verification/PROVABLE_V1.md) | 14,309 | the ProvableV1 contract: eligibility, supported types, expressions and state, failure model, attachment requirements, compatibility rule | normative ("v1 contract", the allowlist) | output strings: `Concrete/Report/Report.lean:1402,1446,6076`; comment `ProofCore.lean:1097`; `tests/showcase/manifest.toml:55` |
| [PROVER_NEUTRAL_OBLIGATIONS.md](../../docs/verification/PROVER_NEUTRAL_OBLIGATIONS.md) | 40,664 | "THE NORMATIVE RULE" for the prover-neutral layer, trust boundaries, evidence vocabulary, host roles, the as-built audit, the multi-kernel badge decision of 2026-07-31 | normative ("normative architecture") | **(parsed)** `check_docs_drift.sh:86`; comment `Concrete/Semantics/TermIR.lean:25` |
| [PURE_CORE_PROOF_ARC.md](../../docs/verification/PURE_CORE_PROOF_ARC.md) | 14,028 | the pure-core arc contract: evidence classes, definition of done, slices 1–3, the selective-proving rule, where proofs live | record with normative rules | comment: `check_purecore_proofs.sh:2`, `Concrete/Proof/Proof.lean:3033,3122` |
| [R-0208-closure.md](../../docs/verification/R-0208-closure.md) | 7,692 | closure record for R-0208 (checker identity, the Lean #14576 fire drill) | record | comment: `check_lean14576_reproducer.sh:13` |
| [SMT_SOUNDNESS.md](../../docs/verification/SMT_SOUNDNESS.md) | 9,380 | the trusted solver binary, encoding assumptions, unsupported theories, replayed fragments, `solver_checked` against `solver_replayed`, how a solver bug affects each class | normative (no Status line; present-tense listed) | **(parsed)** `check_docs_drift.sh:82`; comment `Concrete/Proof/ObligationCore.lean:57` |
| [SPARK_CLASS_ASSURANCE.md](../../docs/verification/SPARK_CLASS_ASSURANCE.md) | 8,862 | the target annotation families (frames, information flow, ghost code) and coding rules for agents | target ("design target / agent guide") | none |
| [THREAT_MODEL.md](../../docs/verification/THREAT_MODEL.md) | 11,240 | attachment attacks and ten threat categories, each mapped to a mechanism, plus non-goals | normative ("stable reference") | none |
| [TRUSTED_BOUNDARY_GUIDE.md](../../docs/verification/TRUSTED_BOUNDARY_GUIDE.md) | 7,966 | the three-way trust split, four wrapper patterns, an audit checklist | guide ("canonical reference") | none |
| [TRUSTED_COMPUTING_BASE.md](../../docs/verification/TRUSTED_COMPUTING_BASE.md) | 17,527 | TCB layers (checker, Lean kernel, attachment, backend, runtime, trusted and foreign code), per-profile TCB, what multi-kernel agreement removes | normative ("canonical reference") | **(parsed)** `scripts/spec/spec.py:906` (S7 `trusted:` IDs); **(parsed)** `check_docs_drift.sh:84` |
| [VC_BRIDGE_REGISTER.md](../../docs/verification/VC_BRIDGE_REGISTER.md) | 20,363 | the Core-to-obligation lowering, one row per obligation family, with a faithfulness status | register ("contract document") | **(parsed)** `check_vc_bridge_register.sh:25,149` (`` ### `<family>Obligations` `` rows); comments `Concrete/Semantics/IntArith.lean:391,457,503`, `Concrete/Interp/Interp.lean:1115`, `check_hole_status_consistency.sh`, `check_known_wrong_corpus.sh` |
| [VC_GENERATOR_DESIGN.md](../../docs/verification/VC_GENERATOR_DESIGN.md) | 8,062 | design of a weakest-precondition VC generator (one traversal, one rule table) | target ("design, not implemented") | comment: `Concrete/Report/VCGen.lean:4` ("Implements the structure in …") |
| [VERIFICATION_CHARTER.md](../../docs/verification/VERIFICATION_CHARTER.md) | 10,846 | product goal, product invariants, "Evidence is a vector, not a ladder", practical limits | target ("product direction") | none |
| [VERIFICATION_IR.md](../../docs/verification/VERIFICATION_IR.md) | 8,331 | VIR as the promoted TermIR: identities, canonical encoding, certificate profiles, extension admission | target ("normative design direction") | none |
| [VERIFICATION_STATUS.md](../../docs/verification/VERIFICATION_STATUS.md) | 11,571 | capability map for main: what is available, missing and experimental, with explicit non-claims; last reviewed 2026-08-07 | reference ("canonical capability map") | comment: `check_hole_status_consistency.sh` |
| [VERIFY_GATES.md](../../docs/verification/VERIFY_GATES.md) | 7,371 | the compiler's self-check boundaries (post-elab, post-mono, post-lower, post-cleanup), documented exceptions, the never-delete rule | normative ("contract") | comments: `Main.lean:3545`, `Concrete/Pipeline/Pipeline.lean:263`, `test_verify_gates.sh:10` |
| [WRONG_CODE_CORPUS.md](../../docs/verification/WRONG_CODE_CORPUS.md) | 7,597 | what qualifies for `tests/wrong-code/manifest.toml`, the case format, the minimization rule, the lifecycle | normative ("contract") | comment: `test_wrong_code.sh:8`; `tests/wrong-code/` READMEs |

### 2.2 Data files and subdirectories (10)

| file | bytes | contents | role | readers |
|---|---:|---|---|---|
| `claim-inventory-v4.28.0.tsv` | 11,897 | claim inventory frozen under Lean 4.28.0 (R-0208) | record (superseded data) | no script; cited by R-0208-closure.md and docs/archive |
| `claim-inventory-v4.34.1.tsv` | 18,181 | the frozen claim inventory under Lean 4.34.1 | data (gate baseline) | **(parsed)** `check_claim_inventory.sh:21`, `scripts/tools/checker_acceptance_evidence.sh:24`; string `Concrete/Proof/CheckerPolicy.lean:99` |
| `identity-raw-baseline-pre-r0482.tsv` | 52,728 | raw identity digests at `61b2a261` | data (gate baseline) | **(parsed)** `check_identity_rows.sh:59`, `check_identity_separation.sh:355`, `scripts/tools/identity_rows.sh` |
| `identity-rows-pre-r0482.tsv` | 52,326 | normalized frozen identity rows | generated (`identity_rows.sh`), gate baseline | **(parsed)** `check_identity_rows.sh:29` |
| `identity-rows-amendments.tsv` | 13,259 | one amendment per changed frozen row | data (append-only) | **(parsed)** `check_identity_rows.sh`, `check_identity_freeze.sh:73,119`, `check_identity_separation.sh:378`, `identity_migration_map.sh` |
| `identity-migration-map-r0482.tsv` | 60,065 | the old-to-new identity map | generated (`identity_migration_map.sh`), gate baseline | **(parsed)** `check_identity_separation.sh:354` |
| `checker-acceptance/lean-4.34.1-arm64-apple-darwin.txt` | 790 | evidence for registering the macOS checker build | record | string in `Concrete/Proof/CheckerPolicy.lean` (evidence field) |
| `checker-acceptance/lean-4.34.1-x86_64-unknown-linux-gnu.txt` | 1,759 | the same for Linux (CI run 38272808890) | record | string `Concrete/Proof/CheckerPolicy.lean:118` |
| `identity_migrations/2026-10-04_r0484_f1_f9.md` | 2,808 | identity migration caused by a std change (R-0484 F1, F9) | record | comment `Concrete/Proof/Proof.lean:1939` |
| `identity_migrations/2026-10-06_r0484_f6.md` | 3,243 | the same for R-0484 F6 | record | comment `Concrete/Proof/Proof.lean:1943` |

Other things that depend on these paths: `.github/workflows/book.yml` and `site_reference.py`
publish PROVABLE_SUBSET.md as a site page. [docs/README.md](../../docs/README.md) is the real
index of the directory. `tests/wrong-code/`, `tests/oracle/README.md`, about a dozen
`examples/*/README.md` and `assumptions.toml` files, and at least 28 Markdown files outside the
directory (counting only the literal path `docs/verification/`; relative `../verification/` links
come on top) point into it. No golden output under `tests/` contains the paths that `Report.lean` prints.

### 2.3 Summary by role

- **Normative** (14): ASSUMPTION_FILES, AXIOMS, CLAIMS_TODAY, CLAIM_TAXONOMY,
  EFFECT_PROOF_BOUNDARIES, GUARANTEE_STATEMENT, OBLIGATION_CORE, PROOF_CONTRACT,
  PROOF_SEMANTICS_BOUNDARY, PROVABLE_V1, SMT_SOUNDNESS, THREAT_MODEL, TRUSTED_COMPUTING_BASE and
  WRONG_CODE_CORPUS, plus the normative parts of IDENTITY_SEPARATION, PROVER_NEUTRAL_OBLIGATIONS,
  VERIFY_GATES and PURE_CORE_PROOF_ARC.
- **Target** (9): CONTRACTS_AND_VCS, EVIDENCE_ARCHITECTURE, PROOF_AUDIT_PIPELINE,
  PROOF_STATE_MODEL (by its header), PROOF_SYNTHESIS, SPARK_CLASS_ASSURANCE, VC_GENERATOR_DESIGN,
  VERIFICATION_CHARTER and VERIFICATION_IR. PROOF_LADDER is half target.
- **Registers** (5): KNOWN_HOLES, VC_BRIDGE_REGISTER, PROOF_OBLIGATIONS_REGISTER,
  EVIDENCE_PRODUCER_MATRIX and PROOF_STORY_MATRIX, plus the six TSVs.
- **Guides** (4): PROOFKIT_GUIDE, PROOF_WORKFLOW, PROOF_THEOREM_SHAPES and TRUSTED_BOUNDARY_GUIDE.
- **Records** (9 Markdown files plus 3 others): PHASE3 and PHASE4 audits, R-0208-closure,
  IDENTITY_MIGRATION_PLAN, PROOF_EXTRACTION_GAPS, the two `identity_migrations/` files,
  EXTERNAL_CHECKER's dated bump evaluations, PURE_CORE_PROOF_ARC's slice logs,
  `claim-inventory-v4.28.0.tsv` and `checker-acceptance/`.
- **References** (6): README, EVIDENCE_CLASSES, INTERPRETER_TRUST, PROOF_PRESSURE_SET,
  PROVABLE_SUBSET and VERIFICATION_STATUS.

Six documents carry no `Status:` line: AXIOMS, EVIDENCE_CLASSES, EVIDENCE_PRODUCER_MATRIX,
OBLIGATION_CORE, PROOFKIT_GUIDE and SMT_SOUNDNESS. PURE_CORE_PROOF_ARC writes it as
`**Status:**`. The Status lines of eighteen documents call them canonical, normative or a
contract, and two of them claim to be the one public statement of current guarantees (§3, C1).

## 3. Overlaps and contradictions

### 3.1 Contradictions (the documents disagree)

- **C1. Two documents each claim to be the one statement of current guarantees.**
  CLAIMS_TODAY.md:14 says it "remains the authoritative statement of what Concrete supports
  today". GUARANTEE_STATEMENT.md:3 calls itself "the single precise statement of what Concrete
  guarantees", and neither defers to the other. VERIFICATION_STATUS.md:3 ("canonical capability
  map for `main`") is not a third rival: CLAIMS_TODAY.md:19-20 names it as the cross-axis
  capability map, and it defers to CLAIMS_TODAY on line 14. It overlaps, though, because it
  restates the same claims on its own axes. [docs/README.md](../../docs/README.md) line 31 lists
  VERIFICATION_STATUS as "proof status vocabulary", which is not what it contains.
- **C2. Evidence is ranked in one document and explicitly not ranked in others.**
  GUARANTEE_STATEMENT.md:152-182 defines "four guarantee tiers" and says "each tier strictly
  contains the ones above it". VERIFICATION_CHARTER.md:159 ("Evidence is a vector, not a ladder"),
  CLAIM_TAXONOMY.md:3-6 ("must not present the current labels as a universal evidence ordering")
  and the R-0440 decision note in PROOF_LADDER.md:8-12 all reject a total order.
- **C3. Several "class" lists, each presented as the taxonomy.** Some of these lists classify
  different things: report labels, construct-inventory states, target evidence and guarantee
  tiers. That is the problem. No document says which list is which kind of thing, and the ones
  that do classify the same thing disagree.
  - CLAIM_TAXONOMY.md:19-134: enforced, proved, reported, trusted assumption, backend/target
    assumption.
  - EVIDENCE_CLASSES.md:3-4: proved, enforced, reported, assumed, trusted (and "tested" at :70).
  - PROOF_STORY_MATRIX.md:25-34: those five plus `open`.
  - PROVABLE_V1.md:21-31: the same five, but "assumed" covers "an explicit assumption file *or
    trusted boundary*" (:30), which overlaps its own `trusted` row and differs from
    PROOF_STORY_MATRIX.md:31 ("assumptions/policy/release evidence").
  - CONTRACTS_AND_VCS.md:110-131: thirteen classes.
  - GUARANTEE_STATEMENT.md:152: four tiers.
  - TRUSTED_BOUNDARY_GUIDE.md:11-15: an "evidence level" called `trusted-assumption`.

  CLAIM_TAXONOMY, EVIDENCE_CLASSES and PROVABLE_V1 all describe the current report schema, and
  they describe it in incompatible terms.
- **C4. Evidence-class names that the compiler does not emit.** CONTRACTS_AND_VCS.md:117-118 and
  :617-618 define `proved_by_smt_replayed_in_lean`, `proved_by_trusted_solver` and
  `enforced_by_checker`. The Lean sources emit `solver_trusted`, `proved_by_lean_replay`,
  `solver_checked`, `solver_replayed` and `tested_by_oracle` (grep of string literals in
  `Concrete/`), and never those three. CONTRACTS_AND_VCS is in `check_docs_drift.sh`'s
  present-tense list (:81), but that gate checks only paths, so the names pass.
- **C5. `solver_checked` has two meanings.** In SMT_SOUNDNESS.md:87-91 it means corroboration by
  an independent decision procedure (Rocq's `nia`), so "the solver leaves the TCB". In
  PURE_CORE_PROOF_ARC.md:22-25 it means SMT-only "HELPER evidence", which is what SMT_SOUNDNESS
  and the compiler call `solver_trusted`.
- **C6. Are the profile gates mandatory or advisory?** EFFECT_PROOF_BOUNDARIES.md:139-153 and
  GUARANTEE_STATEMENT.md:87 say recursion, allocation, FFI and blocking form an *advisory* gate 5,
  and that "a function that passes gates 1–4 is extracted". PROVABLE_V1.md:36-49,
  PROOF_WORKFLOW.md:68-82 and CLAIMS_TODAY.md:96-103 list no recursion and no allocation as
  required. The code agrees with the second group. `CFnDef.isProofEligible`
  (`Concrete/Proof/ProofCore.lean:1574-1578`) holds only the source gates, but the eligibility
  assessment (`ProofCore.lean:3278-3289`) computes `profileReasons` (recursion, unbounded loops,
  allocation, FFI, floating point, blocking I/O) and sets `eligible := passesSource &&
  passesProfile`. The "advisory" documents are therefore wrong about the shipped compiler.
  Floating point, which is a profile reason in the code, is in none of the lists.
- **C7. Is authority-free the same as pure?** GUARANTEE_STATEMENT.md:82 lists "**Pure** — empty
  capability set (no effects)", and :34-40 says "`capSet.isEmpty` = pure". PROVABLE_V1.md:42 and
  PROVABLE_SUBSET.md:58-59 say authority-free "is not by itself a purity/totality claim".
- **C8. Stale scope.** GUARANTEE_STATEMENT.md:159 gives the tier-1 scope as "integer/boolean
  arithmetic, comparisons, let bindings, if/then/else, non-recursive calls", and :196 lists
  structs, enums and match as future. PROVABLE_SUBSET.md:58-63 calls "no aggregates" and the
  other blanket rules obsolete and lists structs, enums, matches, arrays and bounded loops as
  admitted.
- **C9. Headers that disagree with their bodies or with the code.**
  - VC_GENERATOR_DESIGN.md:3 says "design, not implemented", while
    `Concrete/Report/VCGen.lean:4,20` "implements the structure" as a shadow slice behind
    `--report vcgen-diff`.
  - PURE_CORE_PROOF_ARC.md:3 says "Slice 1 not started", while :83 says slice 1 is COMPLETE and
    slices 2 and 3 follow.
  - PROOF_STATE_MODEL.md:3 says "design proposal", while the Lean sources cite its §2 and §4 as
    the encoding (`ProofCore.lean:1205,1357`, `ProofSoundness.lean:567`).
  - PROOF_PRESSURE_SET.md:16-20,47-48 describes "registry" fingerprints. Its own :9-10 and
    PROOF_WORKFLOW §4 say proofs attach through in-source links.
- **C10. KNOWN_HOLES keeps a stale decision list.** KNOWN_HOLES.md:1186-1223, "Open design
  decisions that gate the old Phase 5/6/7 freeze", lists items it also marks DONE (callable
  values, ByteView) or deferred, with old phase numbers. Open decisions are now owned by the owner
  decisions table of [ROADMAP.md](../../docs/archive/roadmap-2026-11/ROADMAP.md#owner-decisions) and by the OPEN entries of
  [docs/spec/decisions.md](../../docs/spec/decisions.md).
- **C11. The proof status set.** This is the largest current-state conflict.
  CLAIM_TAXONOMY.md:270-285 declares "the seven canonical proof/obligation status terms" (proved,
  unbound, stale, missing, blocked, ineligible, trusted). The compiler also emits `needs_recheck`,
  `deps_not_current`, `correspondence_unjustified` and `spec_unbound` (string literals in
  `Concrete/Proof/ProofCore.lean`; the `ObligationStatus` constructors start at :1889).
  PROOF_CONTRACT.md:230 still labels `needs_recheck` an "R-0004 target".
- **C12. Diagnostic codes outside the classification.** `Concrete/Check/Policy.lean` emits
  E0613–E0617 (vacuity, assumptions, hypothesis debt, solver evidence, two-kernel policy, from
  :229 on), and `ProofCore.lean` emits E0811–E0813 for the newer proof states. None of them has a
  row in `docs/spec/codes.tsv`. So either `--report diagnostic-codes` does not list them, or S6
  would fail. Either way, no document owns their meaning.
- **C13. Out-of-bounds behaviour.** PROOF_STATE_MODEL.md:197-206 says source-level out-of-bounds
  access "is undefined behavior (panics in debug builds; UB in release)". MEM-6 in
  [docs/spec/memory.md](../../docs/spec/memory.md) makes it a checked abort.
- **C14. Dependency-edge vocabularies.** PROOF_CONTRACT.md:102-114 has four edges (`contract`,
  `body`, `trusted`, `missing`). EVIDENCE_ARCHITECTURE.md:150-163 has six (`contract`, `model`,
  `implementation`, `trusted`, `missing`, `unclassified`). `Concrete/Proof/DependencyEdge.lean`
  has five (`contract`, `body`, `trusted`, `missing`, `unclassified`).
- **C15. H23.** TRUSTED_COMPUTING_BASE.md:292-301 still calls the hypothesis set "currently
  unsound" ("Until R-0461 composes status …"). KNOWN_HOLES.md:413 marks H23 CLOSED 2026-08-03
  (R-0461).
- **C16. PROVABLE_V1 contradicts itself on widths.** Lines 85-93 support `div`, `bitand` and
  wrapping add at u32 or i32 only. Line 115 says that since 2026-10-23 they work at every integer
  width, and that `shl` works at every width. The "Excluded" list after it still excludes shifts
  and `bitand` at widths other than `u32`.

### 3.2 Overlaps (one fact in several places)

| fact | stated in |
|---|---|
| proof eligibility gates | EFFECT_PROOF_BOUNDARIES §3, PROVABLE_V1 "Function Eligibility", PROVABLE_SUBSET "Stable Eligibility Boundary", PROOF_WORKFLOW §1, CLAIMS_TODAY §2, GUARANTEE_STATEMENT §2 |
| what `proved` means and does not mean | CLAIMS_TODAY §2, PROOF_CONTRACT §2 and §6, PROOF_SEMANTICS_BOUNDARY §5, CLAIM_TAXONOMY class 2 and §6, PROVABLE_V1 "Non-Claims", GUARANTEE_STATEMENT §2 |
| the claim and evidence vocabulary | CLAIM_TAXONOMY, EVIDENCE_CLASSES, PROOF_STORY_MATRIX, PROVABLE_V1, CONTRACTS_AND_VCS, PURE_CORE_PROOF_ARC, PROVER_NEUTRAL_OBLIGATIONS "Evidence vocabulary", EVIDENCE_ARCHITECTURE "Five objects" (C3) |
| `bv_decide` native-code trust | AXIOMS tier 1, KNOWN_HOLES H20 (OPEN, :527) **and** T1 (a "trust ledger, not a hole", :1170), TCB §2, CLAIMS_TODAY §2, PROOF_LADDER |
| backend and target assumptions | TCB §4-5, GUARANTEE_STATEMENT §4, PROOF_SEMANTICS_BOUNDARY §7, CLAIM_TAXONOMY class 5, CLAIMS_TODAY §7 |
| the meaning of multi-kernel agreement | PROVER_NEUTRAL "DECIDED (2026-07-31)", TCB "What multi-kernel agreement does and does not remove", VC_BRIDGE_REGISTER, CLAIMS_TODAY "Independent-kernel evidence", KNOWN_HOLES |
| SMT trust and replay | SMT_SOUNDNESS, CONTRACTS_AND_VCS (SMT sections), PROOF_LADDER (discharge ladder), PURE_CORE_PROOF_ARC, KNOWN_HOLES H21 |
| the obligation record and ledger | OBLIGATION_CORE, CONTRACTS_AND_VCS "Obligation Model", PROVER_NEUTRAL "The record", PHASE3 audit, VC_BRIDGE_REGISTER |
| proof attachment, fingerprints and staleness | PROOF_CONTRACT, PROOF_WORKFLOW §4-7, CLAIMS_TODAY §3, PROVABLE_V1 "Proof Attachment Requirements", PROVABLE_SUBSET "Attachment Integrity", THREAT_MODEL §1 and §7 |
| the trusted three-way split | TRUSTED_BOUNDARY_GUIDE, GUARANTEE_STATEMENT §3, EFFECT_PROOF_BOUNDARIES "Trusted Code"; language rules TRUST-4 and TRUST-5 in [docs/spec/trusted.md](../../docs/spec/trusted.md) |
| safe-code memory, capability and control-flow guarantees | GUARANTEE_STATEMENT §1 and CLAIMS_TODAY §1 paraphrase rules owned by the OWN-, TRUST- and STMT- chapters of docs/spec. S11 catches only verbatim copies |
| struct-literal evaluation order | EVIDENCE_PRODUCER_MATRIX.md:124 (RATIFIED, grepped by `check_convergence_inventory.sh`) and STMT-4 in [docs/spec/statements.md](../../docs/spec/statements.md) |
| pipeline self-check boundaries | VERIFY_GATES "Named boundaries" and PASS-5 to PASS-11 of [docs/spec/pipeline.md](../../docs/spec/pipeline.md) (`verifyPostMono` is an `Enforced by:` at pipeline.md:232) |
| the interpreter's role | INTERPRETER_TRUST and the spec README's consumer legs (§2.2), plus VERIFIED_PIPELINE S7 |

Two smaller collisions:

- PROOF_OBLIGATIONS_REGISTER numbers its rows `R-01` … `R-28` (its trust gates are `G-01` …
  `G-06`). That is the roadmap's `R-NNNN`
  namespace, and the Lean comments that cite "R-01 and R-02" read like roadmap tasks.
- `scripts/tools/claim_inventory.sh:13` documents its usage as writing
  `docs/verification/claim-inventory.tsv`, a file that does not exist; the frozen copies carry the
  checker version in their names.

## 4. Target structure

### 4.1 Where it lives

The proposal is to add verification chapters to `docs/spec/`, as a second part of the one
specification, and not to start a sibling tree. The reasons:

- **One tool and one registry.** `spec.py` already parses any chapter listed in README §1. Using
  it gives one `ids.tsv`, IDs that are unique across both parts, the S3 history rules, S8
  tracking, S10 site generation and S11 restatement scanning with no second implementation.
- **One code classification.** S6 needs every diagnostic code in exactly one row of `codes.tsv`.
  The 14 `tooling` codes are proof and policy codes (E0800 "stale proof", E0810 "proof link
  unbound", E0611 "denied capability used" and so on). They become `rule` codes once a
  verification entry names them. A sibling tree would need a second `codes.tsv` and would break
  the "exactly one row" property.
- **Existing links stay valid.** `Evidence: trusted: <ID>` and CON-/TRUST- already point into
  verification. Inside one tree those links become rule-to-rule `Uses:` references.

The cost is that README's scope sentence ("the one written statement of the Concrete language")
widens to "the language and its evidence". That is owner decision V1. The alternative, a sibling
`docs/spec-verification/` read by `spec.py --spec`, works only with a shared `ids.tsv` and
`codes.tsv`, which leaves the same tree in practice.

### 4.2 Chapters

Chapter names are lower-case letters only and prefixes upper-case letters only (the inventory
regex, §1). `trusted.md`/`TRUST-` is taken, which is why the TCB chapter is `trustbase.md`.

| chapter | prefix | what it owns | fed by (normative parts) | becomes rationale or design record | gates whose fact it owns |
|---|---|---|---|---|---|
| `claims.md` | `CLAIM-` | the vocabulary: the composite labels as emitted today (definitions), the forbidden conflations, which words a report may use for which evidence, no semantically dark construct, evidence as separate dimensions | CLAIM_TAXONOMY §1, §6 and §7; EVIDENCE_CLASSES; PROOF_STORY_MATRIX "Claim Classes"; PROVABLE_V1 "Claim Classes"; the vocabulary in CONTRACTS_AND_VCS and PROVER_NEUTRAL; VERIFICATION_CHARTER "Product invariants" | VERIFICATION_CHARTER (whole), CLAIM_TAXONOMY §2-5 (mappings, later generated) | `check_evidence_algebra.sh` (named by the entries as `Enforced by:`) |
| `provable.md` | `PROV-` | proof eligibility, the admitted ProofCore surface (the ProvableV1 allowlist), the failure model, the compatibility rule, how the proof model differs from the language | PROVABLE_V1; EFFECT_PROOF_BOUNDARIES §2-3 and §4.1-4.2 (the shipped admission model: summary-based admission refusals, and failed admission still kept under replay and drift maintenance); PROOF_SEMANTICS_BOUNDARY §2-6; the decisions in PROOF_STATE_MODEL, minus its out-of-bounds UB text (C13), which MEM-6 already governs | PROVABLE_SUBSET, PROOF_STATE_MODEL, EFFECT_PROOF_BOUNDARIES §8; PROOF_EXTRACTION_GAPS as a record | showcase ProvableV1 conformance; codes E0802, E0803, E0804 and E0612 (see §4.3 on codes) |
| `attachment.md` | `ATT-` | in-source proof links, fingerprints, coverage, freshness and the integrity states (stale, missing, unbound), typed dependency edges, the identity definitions | PROOF_CONTRACT; PROOF_WORKFLOW §4-7 (normative sentences only); the compiler's full status set (C11) and edge set (C14); PROOF_THEOREM_SHAPES' required shapes and naming rules ("must", line 5); IDENTITY_SEPARATION §3 (D1–D7 as `ATT-D` definitions); THREAT_MODEL §1 and §7 | IDENTITY_SEPARATION §1-2 and §4 (record), PROOF_WORKFLOW (stays a guide) | `check_dependency_edges.sh`, `check_one_producer.sh`, `check_identity_*`; codes E0800, E0801, E0805–E0807, E0810 and E0811–E0813 (see §4.3 on codes) |
| `obligations.md` | `OBL-` | the obligation record and kinds, the scoped context, the runtime-safety families, discharge adapters, Core-to-obligation faithfulness, prover-neutral propositions (TermIR, VIR as pending) | OBLIGATION_CORE; PROVER_NEUTRAL "THE NORMATIVE RULE", "The record", "Composition", "Evidence vocabulary" and its fail-closed driver requirements; the shipped parts of CONTRACTS_AND_VCS; VERIFICATION_IR "Purpose and ownership", "VIR v1 boundary" and "Semantics" (pending entries) | CONTRACTS_AND_VCS (design record), VC_GENERATOR_DESIGN, PROOF_AUDIT_PIPELINE, PROVER_NEUTRAL "As-built versus as-specified" | `check_vc_bridge_register.sh` (the register stays, §4.4) |
| `solvers.md` | `SMT-` | the external-solver firewall, the meaning of `solver_trusted`, `solver_checked` and `solver_replayed`, encoding assumptions, unsupported theories, the discharge order | SMT_SOUNDNESS; PROOF_LADDER "discharge ladder" and "`assume` is a controlled trapdoor"; PURE_CORE_PROOF_ARC "Evidence classes" | PROOF_LADDER (rest) | `check_smt_path.sh`, `check_smt_negatives.sh`, `check_smt_policy.sh`, `check_smt_replay.sh`; codes E0615 and E0616 (solver evidence) |
| `kernels.md` | `KRN-` | the kernel axiom allowlist, declared native trust (theorems and owners), forbidden axioms, independent re-check, checker identity, pin freshness and acceptance, what a multi-kernel badge means | AXIOMS; EXTERNAL_CHECKER "Pins and their freshness"; PROVER_NEUTRAL "DECIDED (2026-07-31)" and "The trust boundaries" (kernel side); TCB §2 | AXIOMS measurements, EXTERNAL_CHECKER bump evaluations (records), R-0208-closure | `check_axiom_inventory.sh`, `check_external_checker.sh`, `check_checker_pin_freshness.sh`, `check_claim_inventory.sh`, `check_bv_certificates.sh`, `check_lean14576_reproducer.sh`; code E0617 (two-kernel policy) |
| `receipts.md` | `RCPT-` | evidence objects as separate facts (correspondence, validity, closure, replay receipt, policy decision), authorship modes, independence, revocation and advisories, degradation | EVIDENCE_ARCHITECTURE (pending entries tracked by R-0440 and related tasks); VERIFICATION_IR "Identities and canonical encoding", "Certificate profiles" and "Extension admission"; THREAT_MODEL §8-9 | EVIDENCE_ARCHITECTURE prose, PROOF_SYNTHESIS (see V7) | none yet (pending) |
| `trustbase.md` | `TCB-` | the trusted base as named assumptions with stable IDs (`TCB-n`), per-profile TCBs, the interpreter's trust boundary, assumption files | TRUSTED_COMPUTING_BASE (with C15 corrected); GUARANTEE_STATEMENT §3-4; PROOF_SEMANTICS_BOUNDARY §7; INTERPRETER_TRUST §1, §3 and §4; ASSUMPTION_FILES (schema and enforced fields); TRUSTED_BOUNDARY_GUIDE's three-way split and wrapper rules ("canonical"); PROVER_NEUTRAL "The trust boundaries" (non-kernel side) | TCB "Why This Exists", INTERPRETER_TRUST §5, TRUSTED_BOUNDARY_GUIDE (stays a guide) | `check_assumptions.sh`; spec.py S7's `trusted:` lookup; codes E0613 and E0614 (vacuity, assumptions) |
| `registers.md` | `REG-` | the rules every register obeys: a hole is tracked, gated and disclosed; status headings; every obligation family has a VC-bridge row; every constructor is classified in the producer matrix; wrong-code admission and lifecycle; frozen baselines change only by amendment | KNOWN_HOLES "governing rule" and "How to use this file"; THREAT_MODEL §10 (the required non-vacuous controls); VC_BRIDGE_REGISTER "Why" and "What faithful means"; WRONG_CODE_CORPUS; EVIDENCE_PRODUCER_MATRIX columns; IDENTITY_MIGRATION_PLAN §6 (amendment rule) | the registers themselves stay as data (§4.4) | `check_hole_status_consistency.sh`, `check_known_wrong_corpus.sh`, `check_vc_bridge_register.sh`, `check_evidence_tree.sh`, `test_wrong_code.sh`, `check_identity_rows.sh`, `check_identity_freeze.sh` |

Two more moves stay inside the existing language spec:

- **VERIFY_GATES** states compiler pipeline boundaries, not evidence. Its normative sentences (the
  boundaries, the severities, the never-delete rule) belong to `pipeline.md`'s PASS- entries, which
  already name `verifyPostMono`. The spec agent owns that chapter (R-0554 assigns docs/spec to
  `wip/spec`), so this is a hand-off, not a slice of this plan.
- **Language-level guarantees** in GUARANTEE_STATEMENT §1, CLAIMS_TODAY §1 and
  EVIDENCE_PRODUCER_MATRIX's RATIFIED section are replaced by citations of OWN-, TRUST-, STMT-4 and
  MEM-. The verification chapters cite language rules and do not restate them.

THREAT_MODEL's remaining categories (§2-6) describe which mechanism catches which threat. They
become the rationale of the chapters that own those mechanisms, and each "Required defenses" line
becomes an entry in the owning chapter. S11 detects only verbatim restatements
(`spec.py:497-516`), so it cannot show that a migration lost a sentence or kept a paraphrase.
Each absorbing slice therefore commits a claim map: every normative sentence of the absorbed
document, each with the entry, rationale, register row or open decision it went to. Review checks
the map.

### 4.3 What the format needs from `spec.py`

The entry format carries over unchanged: an anchor, a heading, one sentence, the fields, a
rationale and examples. Verification rules are often enforced by a gate script or a report rather
than by a compile-time diagnostic, so five extensions are needed. Each lands with a negative
control in README §3.1.

1. **Gate sites (S7).** `Enforced by:` may name `scripts/tests/<gate>.sh`. The gate must contain a
   `spec: <ID>` comment, just as `std/src/*.con#Name` sites do today (`spec.py:816-838`). This is
   how every gate in the table above becomes a site.
2. **A `policy` kind (S1, S5).** A policy entry is a rule about evidence, admission or a register
   whose violation fails a gate or produces a report verdict, not a compile diagnostic. S5
   coverage applies to an `enforced` or `partial` policy entry. It needs at least one
   `Enforced by:` gate, that gate must be in `scripts/tests/ci_gate_inventory.txt` so it actually
   runs in CI, and it must have a negative control (a self-test or planted defect) named in the
   entry's fixtures table. A `pending` policy entry needs only a resolvable tracking ID (S8), as
   pending entries do today. When it turns `enforced`, the gate and control requirement applies in
   the same change.
3. **Trusted IDs (S7).** `Evidence: trusted: <ID>` must name a live `TCB-` entry. Today it only
   has to occur somewhere in the text of TRUSTED_COMPUTING_BASE.md. This lands with `trustbase.md`
   and is stricter than the current substring test.
4. **Proof and policy codes (S1, S6).** Today only a `refusal` may name codes
   (`spec.py:358-361`), stages come from a fixed compiler-stage set (`spec.py:24`, with no `proof`
   or `policy`), and S6 requires the compiler ledger's `rules` field to equal the spec's mapping
   (`spec.py:794-805`). A `coverage_allow.tsv` row does not waive S6. Many proof codes are
   warnings or informational report diagnostics (`ProofCore.lean:2140-2160`), not refusals.
   Moving E06xx and E08xx from `tooling` to `rule` therefore needs all three of the following, or
   the codes stay `tooling` and the entries cite them in prose only:
   - the stages `proof` and `policy`;
   - a rule that a `policy` entry may name a non-blocking diagnostic code;
   - the compiler tagging those diagnostics with `rule: ID@v` (V9).

   First, C12 must be resolved: E0613–E0617 and E0811–E0813 are emitted but unclassified, so they
   need ledger rows before any of this.
5. **Report examples (S4), later.** An example of the form
   `con accept report=<kind> expect=<field>:<value>` compiles a program, runs `--report <kind>`
   and compares one JSON field. It is needed only for entries whose evidence is a report state,
   such as stale, unbound or `solver_trusted`. Until it exists, those entries are `policy` entries
   enforced by their existing gates. Examples that need Lean run on CI, as S4's Lean leg does
   today.

S13 extends to `docs/verification/`. `one_spec.py` gains a second list,
`docs/spec/verification_docs.tsv`, with columns `path`, `disposition` and `chapters`. The
disposition is one of:

- `absorb`: the row must disappear, and the file with it, as in `unabsorbed.tsv`;
- `register`, `guide`, `record`, `runbook`, `public` (CLAIMS_TODAY) or `index` (README): the row
  and file stay, the Markdown file's `Status:` line must name that role, and S11 keeps it free of
  normative sentences;
- `data`: a TSV or text file read by a gate or named by a Lean string. It has no Status line, and
  the row names its reader.

Slice 1 adds the role words to the Status lines of every retained file in the same commit that
introduces the check, so the check never sees a retained file without one.

A new file under docs/verification is refused unless it adds a row with a non-`absorb`
disposition. An `absorb` row can never be added, so the normative part only shrinks.
`check_docs_drift.sh` already runs `one_spec.py` and S11, so nothing new is wired into CI.

### 4.4 What stays in docs/verification

Once the absorb rows are gone, the directory holds only roles that are not specification:

- **Registers**: KNOWN_HOLES, VC_BRIDGE_REGISTER, PROOF_OBLIGATIONS_REGISTER,
  EVIDENCE_PRODUCER_MATRIX, PROOF_STORY_MATRIX and the TSV baselines. Their row formats are what
  the gates parse. `check_hole_status_consistency.sh` needs `### H<n>.` headings and at least one
  CLOSED hole, and `check_submodule_check_coverage.sh` needs `### H12`, so closed holes cannot be
  moved out of KNOWN_HOLES without changing those gates.
- **Guides**: PROOF_WORKFLOW, PROOFKIT_GUIDE, PROOF_THEOREM_SHAPES and TRUSTED_BOUNDARY_GUIDE.
  They cite rule IDs and may later move into the book (R-0554 item 2).
- **Runbook**: EXTERNAL_CHECKER's operational sections.
- **Public view**: CLAIMS_TODAY, which first absorbs GUARANTEE_STATEMENT and VERIFICATION_STATUS
  (V6). It can later be generated by `spec.py claims` from entry statuses and OPEN holes, as
  `rules.md` is generated today.
- **Records** move to `docs/archive/verification/`. S11 and `check_docs_drift.sh` already exclude
  `docs/archive/`. Data files keep their paths when Lean string literals name them:
  `checker-acceptance/*.txt` and `claim-inventory-v4.34.1.tsv` in `CheckerPolicy.lean`. Moving
  those changes compiler sources and `Concrete/BuildIdentity.lean`, so it needs its own compiler
  slice, and nothing is gained by it.

### 4.5 How the gates that read these documents move

| reader | today | after |
|---|---|---|
| `check_axiom_inventory.sh:114,123`, `check_external_checker.sh:199` | grep AXIOMS.md for theorem names, owners and the allowlist | the kernel allowlist is hard-coded twice today (`ALLOWLIST` in `check_axiom_inventory.sh:27`, `STANDARD_AXIOMS` in `check_external_checker.sh:66`, with the legacy `NATIVE_AXIOMS` at :31). The slice first moves it to one data file beside `axiom_native_trust.txt` and `axiom_native_owners.txt`, read by both gates. `spec.py`'s S9, which today knows only the `exit_2i` manifest, gains a generic "table generated from a data file" manifest. `kernels.md`'s tables are then generated from the three files and S9 keeps them byte-equal, so the three greps become one equality check. All of this lands in the commit that absorbs AXIOMS |
| `spec.py:906` | substring of TRUSTED_COMPUTING_BASE.md | a live `TCB-` entry (§4.3, item 3) |
| `check_docs_drift.sh:71-104` (`PRESENT_DOCS`, `REPORT_DOCS`) | 9 named verification documents | a glob over `docs/verification/*.md` minus `record` rows, plus `docs/spec/*.md` for `--report` kinds. Archived audits leave the list in the slice that archives them |
| `check_hole_status_consistency.sh`, `check_submodule_check_coverage.sh` (and `check_known_wrong_corpus.sh`, whose block headings `check_hole_status_consistency.sh:59-105` parses; it names KNOWN_HOLES only in comments) | KNOWN_HOLES headings | unchanged; KNOWN_HOLES stays a register, and the REG- entries name these gates as sites |
| `check_vc_bridge_register.sh` | VC_BRIDGE_REGISTER rows | unchanged; site of a REG- entry |
| `check_evidence_tree.sh:342` | EVIDENCE_PRODUCER_MATRIX constructors | unchanged; site of a REG- entry |
| `check_convergence_inventory.sh:177,203` | the RATIFIED source-order line | requires the exact versioned rule: `STMT-4@1` is `live` in `docs/spec/ids.tsv`, and STMT-4's normative sentence in `statements.md` still contains "literal elements and fields in the order written". A new STMT-4 version fails the gate until someone checks it. The matrix section becomes a citation of STMT-4@1 |
| `check_claim_inventory.sh`, `checker_acceptance_evidence.sh`, `check_identity_*.sh`, `identity_*.sh` | TSV paths | unchanged (data stays put) |
| `site_reference.py:32`, `book.yml` | publish PROVABLE_SUBSET.md | entry removed when `provable.md` lands, because S10 publishes the chapter. `check_site_generated.sh` must stay green |
| `Report.lean:1446,4118,6076` | print `docs/verification/PROVABLE_V1.md`, `PROOF_WORKFLOW.md` and `PROOF_STORY_MATRIX.md` | print the `provable.md` anchor in the slice that absorbs PROVABLE_V1. That is a compiler change and needs CI. PROOF_WORKFLOW and PROOF_STORY_MATRIX keep their paths |
| comments in `Concrete/**/*.lean`, `Main.lean:3545`, `Pipeline.lean:263`, `lean_action_ci.yml`, `Makefile:141` | cite document paths | rewritten to rule IDs in the slice that absorbs each document. Lean comment edits rebuild the compiler, so those slices run on CI |

## 5. Migration order

Each slice keeps every live claim: each normative sentence becomes an entry, rationale, citation
or register row, or is listed in the slice's commit message as a contradiction that waits for an
owner decision. Each slice fixes every inbound link, including anchors (`check_doc_links.sh`
checks anchors) and the root-relative links in `changelog.d/`, and keeps `check_spec.sh`,
`check_docs_drift.sh` and `check_doc_links.sh` green. No slice changes a stated policy, except
by correcting a document to what the shipped compiler does (C6, C9, C11, C14, C15 and C16 are
of that kind; the note cites the code for each). A contradiction that needs a policy choice stays
in its documents, listed in the slice's claim map, until the owner answers. It cannot be written
as an `open` entry: S1 accepts `Status: open ODn` only for a real owner decision of the ROADMAP
table (`spec.py:349-353`). The V-decisions below become open entries only after the owner admits
them to that table.

0. **This note.** Research only.
1. **Tooling.** `spec.py` gains gate sites, the `policy` kind and its coverage rule, and negative
   controls for both. `one_spec.py` gains `verification_docs.tsv` with every current file
   classified, and its own controls. Every retained file gets its role word in its Status line in
   the same commit. README §1 gains the part-two inventory rows (chapters
   planned, none written). There are no document moves. Needs `check_spec.sh` on CI (S7 resolves
   Lean names).
2. **Status lines and the archive.** Add the missing `Status:` lines (six documents), correct
   the four stale headers of C9 so they state what the body and the code show (facts, not
   policy), and move PHASE3 and PHASE4 audits, IDENTITY_MIGRATION_PLAN, R-0208-closure,
   `identity_migrations/` and `claim-inventory-v4.28.0.tsv` to `docs/archive/verification/`.
   Remove the audits from `PRESENT_DOCS` and `REPORT_DOCS` in the same commit and fix their 3 to 5
   inbound mentions each, including the comments in `check_identity_separation.sh:2`,
   `identity_migration_map.sh:2`, `check_lean14576_reproducer.sh:13` and `Proof.lean:1939,1943`
   (Lean comment edits, so CI). Fix docs/README.md line 31.
3. **`kernels.md` (KRN-).** The smallest and best-gated area, and the pilot. It absorbs AXIOMS'
   policy, EXTERNAL_CHECKER's pin rules and the multi-kernel badge decision. It generates the
   native-trust tables and switches the two grepping gates to the data files. AXIOMS.md becomes a
   design record that keeps the measurements, or is absorbed whole if nothing but rationale is
   left. 44 mentions to fix.
4. **`trustbase.md` (TCB-).** Assumption IDs, the stricter S7 `trusted:` lookup, the
   INTERPRETER_TRUST boundary and the ASSUMPTION_FILES schema rules. TRUSTED_BOUNDARY_GUIDE and
   GUARANTEE_STATEMENT §3-4 then cite TCB- IDs.
5. **`registers.md` (REG-).** The register rules, with the existing gates as sites. Registers
   gain a citation of their REG- entry (their Status lines were done in slice 1). PROOF_OBLIGATIONS_REGISTER
   rows are renamed only if V10 says so.
6. **`attachment.md` (ATT-).** PROOF_CONTRACT, the identity definitions and the normative
   sentences of PROOF_WORKFLOW. It states the compiler's status and edge sets (C11, C14), and
   PROOF_CONTRACT and CLAIM_TAXONOMY are corrected to match them. Proof codes are reclassified to
   `rule` only if V9 approves the rule tags and §4.3 item 4 has landed. Otherwise they stay
   `tooling`.
7. **`solvers.md` (SMT-) and `obligations.md` (OBL-).** SMT_SOUNDNESS, OBLIGATION_CORE, the
   shipped parts of CONTRACTS_AND_VCS and the PROVER_NEUTRAL rule. The C4 names (CONTRACTS_AND_VCS
   becomes a design record, so its names stop being present-tense) and the C5 meaning wait for V4.
8. **`provable.md` (PROV-).** PROVABLE_V1, the eligibility rules and the proof-semantics boundary.
   `Report.lean`'s strings and `site_reference.py` change here (compiler slice on CI). C6 is corrected
   to the code; C7 and C8 need V3. PROVABLE_V1's width rows are reconciled with line 115
   (C16).
9. **`claims.md` (CLAIM-).** The vocabulary, after V2. CLAIM_TAXONOMY, EVIDENCE_CLASSES and the
   class tables in PROOF_STORY_MATRIX and PROVABLE_V1 then cite the chapter.
10. **`receipts.md` (RCPT-).** EVIDENCE_ARCHITECTURE and VERIFICATION_IR as pending entries,
    only if V5 says target material is written as pending rules.
11. **Public surface.** CLAIMS_TODAY absorbs GUARANTEE_STATEMENT and VERIFICATION_STATUS, if V6
    approves. Later, `spec.py claims` generates it.
12. **Close.** Every `absorb` row is gone. docs/verification/README becomes an index by role,
    `PRESENT_DOCS` becomes a glob, and R-0554 item 5 closes.

Slices 3 to 10 are independent of each other apart from `Uses:` references and can be reordered.
Slice 1 must come first. Slice 2 can run alongside slice 1.

## 6. Decisions for the owner

Each of these changes a stated policy, the standing of a document, or compiler output. The note
recommends an answer for each but decides none.

- **V1. Where the verification specification lives.** As part two of `docs/spec/`, sharing
  `ids.tsv`, `codes.tsv` and `spec.py`, or as a sibling tree. Recommended: part two. This widens
  README's scope sentence and crosses the R-0554 split between the spec agent (docs/spec) and the
  cleanup agent (docs/verification).
- **V2. The normative claim vocabulary (C2, C3).** Recommended: the labels the compiler emits
  today, as definitions, with the R-0440 dimensions as pending entries. GUARANTEE_STATEMENT's
  "strictly contains" tier ordering is withdrawn in favour of the charter's "vector, not a ladder".
  Also to decide: whether backend/target assumption is its own class (CLAIM_TAXONOMY) or part of
  `trusted` (the other three lists), and whether `open` and `tested` are classes.
- **V3. Proof eligibility wording (C7, C8).** Whether "pure" may be used for authority-free. C6
  needs no decision, because the code requires the profile gates and the advisory documents are
  corrected to it. Whether floating point stays a profile reason is an existing code choice that
  the spec only records.
- **V4. The SMT class names (C4, C5).** Recommended: SMT_SOUNDNESS's meanings, which match the
  code. PURE_CORE_PROOF_ARC's "`solver_checked` … HELPER" rule is restated as `solver_trusted`.
  That rewords a pinned arc rule, so it needs the owner.
- **V5. Target material as pending rules.** Whether EVIDENCE_ARCHITECTURE, VERIFICATION_IR and
  the target parts of PROVER_NEUTRAL become `pending` entries, each a commitment with a tracking
  ID that S3 never lets disappear, or stay rationale only.
- **V6. One public claim page.** Whether GUARANTEE_STATEMENT and VERIFICATION_STATUS merge into
  CLAIMS_TODAY, and whether CLAIMS_TODAY is later generated.
- **V7. Product-bet documents.** Whether PROOF_SYNTHESIS and SPARK_CLASS_ASSURANCE ("design
  target", the latter also "an AI-facing coding guide") move to `research/` (ungated), stay as
  design records, or become pending entries.
- **V8. KNOWN_HOLES' stale decision list (C10) and the H20/T1 duplication.** Whether to delete
  :1186-1223, since open decisions now live in the ROADMAP table and `decisions.md`, and whether
  native-code trust is a hole (H20, OPEN) or a trust-ledger entry (T1). Today it is both.
- **V9. Rule tags on proof and policy diagnostics.** Whether E06xx and E08xx diagnostics carry
  `rule: ID@v` as language diagnostics do. This changes compiler output and goldens. It is a
  prerequisite for classifying those codes as `rule` (§4.3, item 4). Also: what owns the
  unclassified E0613–E0617 and E0811–E0813 (C12).
- **V10. Register row IDs.** Whether PROOF_OBLIGATIONS_REGISTER's `R-01` … rows are renamed
  (for example `PX-01`) so they do not read as roadmap tasks. That touches Lean comments.
- **V11. VERIFY_GATES.** Whether its never-delete rule and boundary severities (post-elab is a
  warning that never blocks) become PASS- entries in `pipeline.md`, which the spec agent owns.

## 7. Method

Read at `71df9c25`: the header, Status line and section headings of every file under
docs/verification; the sections cited in §3; docs/spec/README.md, statements.md, contracts.md
and trusted.md; `scripts/spec/spec.py`, `scripts/spec/one_spec.py` and
`scripts/tests/check_spec.sh`. Readers were found with
`grep -rn "verification/" scripts .github Makefile Main.lean` and by grepping each file name across
`scripts`, `Concrete`, `Main.lean`, `.github` and `Makefile`. Mention counts are name occurrences
across the repository's Markdown, Lean, shell, Python, TOML, Concrete and YAML files. The
evidence-class names were checked against string literals in `Concrete/` and `Main.lean`. Nothing
was built or run except the light document gates.
