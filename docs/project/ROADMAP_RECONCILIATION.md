# Roadmap Reconciliation (2026-10-19)

Status: historical record — read by `scripts/tests/check_roadmap_coverage.sh`.
Date: 2026-10-19

> **Vocabulary after the same day's rename:** the seven execution *stages* named in this record are the
> roadmap's **Phases 1 – 7**, and a task's **Stage:** line is now its **Phase:** line. The topical
> sections it calls "Phase 7" to "Phase 20" are now *areas* (`## Area: …`); the roadmap's
> [Where every area fits](ROADMAP_TASKS.md#where-every-area-fits) table maps each old phase number to its area.
> Since the same day's backlog split, tasks that no active or next phase schedules (their stubs included)
> live in [ROADMAP_BACKLOG.md](ROADMAP_BACKLOG.md); "the roadmap" below means ROADMAP.md and that file.
> Since the 2026-10-28 compaction the task bodies live in the [task register](ROADMAP_TASKS.md), so "the
> roadmap" also includes it, and the binding dispositions below are in
> [this record](#binding-reconciliation-dispositions).
> The record below is kept as written.

The 2026-10-19 reconciliation mapped every task heading and every open-work prose item in
[ROADMAP.md](../../ROADMAP.md) onto the seven stages, the
[verified-compiler plan](../compiler/VERIFIED_PIPELINE.md) and the Lean Compiler Backend decision
(R-0517). It was produced from four slice audits of ROADMAP.md at commit `7b134645`; the line numbers
below refer to that revision. Nothing was deleted. The historical sections it inventoried were later
moved verbatim to [ROADMAP_HISTORY.md](../archive/ROADMAP_HISTORY.md), and the bodies of done,
superseded, merged, retired and delegated tasks to
[ROADMAP_RETIRED_TASKS.md](../archive/ROADMAP_RETIRED_TASKS.md). Each such task keeps a stub in the
roadmap with its **Disposition:** line and a link to its body; each live owner lists what it
inherited under **Inherited obligations**. The acceptance obligations,
regression cases and controls of a superseded task survive under its named owner unless its row
records why they were retired. The roadmap's current queue and live task records own scheduling and
acceptance; this file schedules nothing. The binding architectural dispositions stay in the roadmap's
*Reconciliation record* section.

## Task dispositions

Every task that is not simply live: its disposition, the owner(s) that replace it, and the reason.
Live tasks carry only their **Stage:** line (and a **Reconciliation** note where the audit changed
their scope).

| task | disposition | owner(s) | reason |
|---|---|---|---|
| R-0004 | done | — | done — closed 2026-08-20; the body is retained for its explicit follow-ups and historical boundary; physical move to the archive under R-0481. |
| R-0007 | superseded | R-0492, R-0510 | superseded by R-0492/R-0510: merge, preserving function-symbol and metamorphic controls. |
| R-0008 | superseded | R-0492 | superseded by R-0492: merge reproducer and alias permutations into identity migration. |
| R-0019 | merged | R-0031 | duplicate of R-0031: merge with explicit pull counts and safety requirements. |
| R-0025 | merged | R-0047, R-0020 | duplicate of R-0047/R-0020: merge; no separate umbrella implementation task. |
| R-0027 | merged | R-0061 | duplicate of R-0061: merge. |
| R-0028 | merged | R-0060 | duplicate of R-0060: merge. |
| R-0029 | merged | R-0065 | duplicate of R-0065: merge deferral policy; retain narrow stage-3 module owners. |
| R-0051 | merged | R-0014 | duplicate of R-0014: merge wrappers, documentation and boundary fixtures. |
| R-0059 | merged | R-0021, R-0440 | duplicate of R-0021/R-0440: merge; replace single-class taxonomy with orthogonal facts. |
| R-0062 | merged | R-0021, R-0056 | duplicate of R-0021/R-0056: merge behavioral regression gates. |
| R-0072 | retired | R-0517 | retired — LCB pins QBE 1.3 in its own upstream lock; Concrete pins the LCB version under R-0517. |
| R-0073 | superseded | R-0497, R-0503 | superseded by R-0497/R-0503: merge into payload-bound validated BIR artifacts. |
| R-0074 | superseded | R-0503, R-0517 | superseded — merge into validated BIR (R-0503) and R-0517; the closed `llvm`/`qbe` backend set becomes the LLVM and LCB-native profiles. |
| R-0076 | superseded | R-0502 | superseded by R-0502: merge into single proved Layout authority. |
| R-0077 | superseded | R-0503, R-0509 | superseded by R-0503/R-0509: merge with `rt` contracts, support inventory and trust accounting. |
| R-0079 | superseded | R-0493, R-0501, R-0503 | superseded by R-0493/R-0501/03: merge into typed operations and pass obligations. |
| R-0080 | merged | R-0118 | duplicate of R-0118: merge backend/profile dimensions into one inventory. |
| R-0081 | merged | R-0505, R-0517 | merged — emitter independence between the LLVM adapter and the LCB adapter, and shared-helper disclosure, move to R-0505/R-0517. |
| R-0082 | superseded | R-0497, R-0501, R-0503 | superseded by VP S5–S13: retire old SSA chain; use CheckedCore→CheckedMonoCore→MIR→BIR. |
| R-0083 | retired | R-0511, R-0517 | retired — there is no Concrete QBE backend to select; the no-silent-fallback rule moves to R-0511/R-0517. |
| R-0084 | delegated | R-0517 | delegated — a typed QBE-style IR and printer are LCB's input layer; Concrete's adapter constructs LCB-IR under R-0517. |
| R-0085 | merged | R-0076 | duplicate of R-0076: merge through R-0076 into R-0502. |
| R-0086 | superseded | R-0511, R-0517 | superseded — its semantic vectors become adapter test vectors for V1 (R-0511/R-0517); later widths, casts, memory and floats follow the V-slices. |
| R-0087 | superseded | R-0512 | superseded by VP V7/R-0512: merge with total float model and profile-specific trust labels. |
| R-0088 | superseded | R-0502, R-0505, R-0517 | superseded — aggregate/memory ABI realization moves to R-0502 (layout) and the adapter boundary (R-0505/R-0517) across V2–V6. |
| R-0089 | superseded | R-0505, R-0517 | superseded — call/FFI/ABI matrices apply at the LCB adapter boundary (R-0505/R-0517); LCB's own ABI coverage (its P07) is an external dependency. |
| R-0090 | superseded | R-0503 | superseded by R-0503: retire preferred-C-runtime prescription; implement Concrete `rt`, named libc FFI. |
| R-0091 | merged | R-0075, R-0517 | merged — native test-mode entry generation joins R-0075's driver and the LCB-native profile (R-0517). |
| R-0092 | superseded | R-0491 | superseded — the three-way differential is Core semantics / LLVM profile / LCB-native under R-0491. |
| R-0093 | merged | R-0092 | duplicate of R-0092: merge; labels are hypotheses, not proof of faulty component. |
| R-0094 | superseded | R-0491 | superseded by R-0491: merge; retain hand/reference/metamorphic controls. |
| R-0095 | superseded | R-0491, R-0118 | superseded by R-0491/R-0118: merge manifests and minimum completed comparisons. |
| R-0096 | superseded | R-0513, R-0491 | superseded by R-0513/R-0491: merge; remove mandatory QBE availability. |
| R-0097 | superseded | R-0511, R-0512 | superseded by R-0511/R-0512: merge profile admission; retain no silent fallback. |
| R-0098 | merged | R-0137 | duplicate of R-0137: merge profile-aware workload matrix and owned exclusions. |
| R-0099 | retired | R-0517, R-0505 | retired — graduation of the native path is R-0517/R-0505's LCB-native profile; no QBE graduation exists. |
| R-0100 | superseded | R-0305 | superseded — Wasm/Cranelift no longer wait for QBE; the Phase 15.25 entrance criteria (R-0305) consume validated BIR. |
| R-0101 | delegated | R-0517 | delegated — legality verification of the QBE-style IR is LCB's (its F04/V02); Concrete validates adapter output under R-0517. |
| R-0102 | superseded | R-0506, R-0517 | superseded — the operation gap ledger becomes typed pass obligations (R-0506) and the adapter's obligations (R-0517). |
| R-0103 | superseded | R-0492, R-0505 | superseded by R-0492/R-0505: merge; general separate compilation arrives in V6. |
| R-0104 | merged | R-0146, R-0517 | merged — deterministic emission and toolchain identity join R-0146; the LCB version and upstream lock are pinned by R-0517. |
| R-0105 | superseded | R-0491, R-0504 | superseded by R-0491/R-0504: merge debug/release and sanitizer controls. |
| R-0106 | merged | R-0009, R-0491 | duplicate of R-0009/R-0491: merge new backend predicates into shared reducer/harness. |
| R-0107 | merged | R-0075 | duplicate of R-0075: merge bounded external-tool execution contract. |
| R-0108 | merged | R-0416, R-0515 | duplicate of R-0416/R-0515: merge profile-labelled budgets and measurements. |
| R-0109 | superseded | R-0503, R-0146 | superseded by R-0503/R-0146: merge; remove assumption helpers must be C. |
| R-0110 | merged | R-0075, R-0333 | merged — profile naming and evidence attribution join R-0075/R-0333; the profiles are `llvm` and the LCB-native `verified` profile. |
| R-0111 | superseded | R-0511, R-0137 | superseded by R-0511/R-0137: merge reusable exits; retire required QBE showcase. |
| R-0112 | superseded | R-0512 | superseded by R-0512: merge; retirement requires compatibility and rollback, not parity alone. |
| R-0114 | superseded | R-0492 | superseded by R-0492: merge; distinguish compilation-local IDs from persistent evidence identity. |
| R-0116 | superseded | R-0497, R-0510, R-0514 | superseded by R-0497/R-0510/R-0514: merge; use payload binding and new IR chain. |
| R-0123 | superseded | R-0495, R-0512 | superseded by R-0495/R-0512: retire new legacy-model semantics; add admitted Core features/proof lemmas instead. |
| R-0131 | merged | R-0119 | duplicate of R-0119: merge as portfolio acceptance criterion. |
| R-0171 | superseded | R-0494 | superseded by R-0494: merge into R-0494; preserve route-specific attacks. |
| R-0176 | superseded | R-0495, R-0512 | superseded by R-0495/R-0512: retire proposed legacy architecture; retain ownership goals in R-0151. |
| R-0185 | merged | R-0179 | duplicate of R-0179: merge into R-0179. |
| R-0194 | merged | R-0209 | duplicate of R-0209: merge into R-0209. |
| R-0198 | merged | R-0178 | duplicate of R-0178: merge schema work into R-0178; verdict enforcement into R-0494. |
| R-0202 | superseded | R-0495, R-0512 | superseded by R-0495/R-0512: merge dependency-closure requirement into Core migration; maintain legacy checks until retirement. |
| R-0204 | superseded | R-0494 | superseded by R-0494: merge into R-0494. |
| R-0205 | superseded | R-0494 | superseded by R-0494: merge into R-0494’s evidence-class controls. |
| R-0206 | merged | R-0166 | duplicate of R-0166: merge into R-0166. |
| R-0208 | done | — | done — closed 2026-10-13 at `2b570c5a`; the body is retained for its accepted limits, now owned by R-0532. |
| R-0209 | superseded | R-0494 | superseded by R-0494: merge into R-0494; retain audit/diff tests. |
| R-0211 | superseded | R-0509 | superseded by R-0509/S16: merge into R-0509; retain audit command acceptance. |
| R-0218 | superseded | R-0494 | superseded by R-0494: merge into R-0494. |
| R-0221 | superseded | R-0493 | superseded by R-0493/S6: merge semantic ownership into R-0493. |
| R-0223 | superseded | R-0496 | superseded by R-0496/S7: merge into R-0496’s semantic contract. |
| R-0227 | superseded | R-0499, R-0507 | superseded by R-0499/R-0507: merge into checker rules and soundness. |
| R-0228 | superseded | R-0499 | superseded by R-0499/S8 and V6: merge enforcement into R-0499; raw-operation extensions into V6. |
| R-0229 | superseded | R-0495 | superseded by R-0495 and V2–V4: merge into slice admission/claim coverage. |
| R-0230 | superseded | R-0509 | superseded by R-0509/S16: merge into R-0509 and slice profiles. |
| R-0231 | superseded | R-0511, R-0512 | superseded by R-0511/R-0512: merge into each slice’s admission controls. |
| R-0233 | done | — | done — the unprofiled-float exclusion shipped (`ProofCore.lean`, `Report.lean`); profiled floats remain V7 (R-0496/R-0495). |
| R-0234 | superseded | R-0496 | superseded by R-0496/S7, V7: merge into V7 float-profile work. |
| R-0235 | superseded | R-0496, R-0495 | superseded by R-0496/R-0495: retire PVal/PBinOp expansion; implement Core float semantics. |
| R-0236 | superseded | R-0506, R-0509 | superseded by R-0506/R-0509: merge axiom classification and inventory. |
| R-0238 | superseded | R-0511, R-0512 | superseded by R-0511/R-0512: merge into cumulative slice matrices; retain orthogonal policies. |
| R-0241 | merged | R-0155, R-0178 | duplicate of R-0155/R-0178: merge obligation payload into those canonical schemas. |
| R-0249 | merged | R-0178, R-0179 | duplicate of R-0178/R-0179: merge into R-0178/R-0179. |
| R-0250 | merged | R-0186 | duplicate of R-0186: merge into R-0186. |
| R-0253 | merged | R-0166 | duplicate of R-0166: merge into R-0166. |
| R-0255 | merged | R-0460 | duplicate of R-0460: merge into R-0460. |
| R-0260 | superseded | R-0506 | superseded by R-0506/S12a: merge into pass/validator obligation ledger; preserve legacy historical view. |
| R-0261 | merged | R-0496, R-0460 | duplicate of R-0496/R-0460: merge; already explicitly reconciled. |
| R-0262 | superseded | R-0495, R-0512 | superseded by R-0495/R-0512: retire blanket legacy bridge; retain only migration-pulled bridges. |
| R-0263 | superseded | R-0511, R-0512 | superseded by R-0511/R-0512: merge into V1–V7 slice ordering. |
| R-0264 | superseded | R-0494, R-0509 | superseded by R-0494/R-0509: merge implementation into R-0494, soundness proof into S16. |
| R-0265 | superseded | R-0495, R-0509 | superseded by R-0495/R-0509: merge into statement/binding/aggregation soundness. |
| R-0266 | superseded | R-0509 | superseded by R-0509/S16: merge; distinguish trusted assumptions from ordinary proof dependencies. |
| R-0267 | superseded | R-0499, R-0507, R-0495 | superseded by R-0499/R-0507/R-0495: merge; capabilities and purity remain distinct. |
| R-0268 | superseded | R-0496 | superseded by S5–S8: retire undecided architecture question; plan has chosen the chain. |
| R-0269 | superseded | R-0495 | superseded by R-0495: merge composition needs into R-0164; retire PExpr table architecture. |
| R-0271 | superseded | R-0510 | superseded by R-0510/S8a, V5: merge into R-0510, retaining explicit law-evidence acceptance. |
| R-0273 | superseded | R-0491, R-0498 | superseded by R-0491/R-0498: merge; map every listed family to a retained control. |
| R-0274 | superseded | R-0499, R-0514 | superseded by R-0499/R-0514: retire source-checker/CompilerDB authority prescription. |
| R-0276 | superseded | R-0506, R-0496 | superseded by R-0506/R-0496: retire general legacy rewrite; retain migration-needed wrappers only. |
| R-0277 | superseded | R-0511, R-0512 | superseded by R-0511/R-0512: merge into each slice’s cumulative acceptance. |
| R-0278 | superseded | R-0503 | Superseded by R-0503/S11: merge into R-0503; BIR replaces this architecture. |
| R-0280 | merged | R-0421 | Duplicate of R-0421: merge into R-0421; connect certificate obligations to R-0504. |
| R-0288 | superseded | R-0503 | Superseded by R-0503/S11: merge inspection, verification and parity requirements into R-0503. |
| R-0290 | merged | R-0491 | Duplicate of R-0491: merge into R-0491, retaining backend/profile matrix. |
| R-0295 | superseded | R-0501 | Superseded by R-0501/S10: retire workload gate; MIR is already an architectural decision. |
| R-0296 | retired | R-0517, R-0505 | retired — see the disposition in the task body. |
| R-0297 | delegated | R-0517, R-0505 | delegated — see the disposition in the task body. |
| R-0348 | merged | R-0349 | merged — the formatting decision is settled; residual documentation cleanup moves to R-0349. |
| R-0378 | merged | R-0377 | Duplicate of R-0377 in export scope: merge export requirements into R-0377; retain hosting as deferred increment. |
| R-0391 | merged | R-0429 | Duplicate of R-0429: merge into R-0429; intake through R-0431. |
| R-0408 | superseded | R-0496 | Superseded in core scope by R-0496/S7: merge required instrumentation into R-0496; separately specify any extra research. |
| R-0409 | superseded | R-0496, R-0512 | Superseded by R-0496/R-0512: retire legacy redesign; carry demonstrated proof debt into Core proof engineering. |
| R-0434 | superseded | R-0502 | superseded by R-0502: merge into layout certificates and Memory CFG verification. |
| R-0435 | superseded | R-0500 | superseded by R-0500: merge migration, renamed-pattern syntax and controls. |
| R-0437 | superseded | R-0514, R-0510 | superseded by R-0514/R-0510: merge declaration and specialization checks; preserve distinct diagnostics. |
| R-0447 | merged | R-0481 | duplicate of R-0481: merge unique operational controls; retire conflicting directory blueprint. |
| R-0456 | superseded | R-0496 | superseded by R-0496 for proposed PExpr port: retire PExpr-port design; reconsider only a measured Core-metatheory proposal. |
| R-0464 | superseded | R-0493, R-0495, R-0460 | superseded by R-0493/R-0495/R-0460: merge; archive completed H24 fix and retire SSA-first prescription. |
| R-0465 | done | — | done for its enumerated wiring; the remaining universal-verdict-authority requirement merges into R-0494. |
| R-0478 | merged | R-0481, R-0474 | duplicate of R-0481 / R-0474: merge each half into its actual owner. |
| R-0483 | done | — | done — closed 2026-10-09 at `4f005715`; the body is retained for its accepted coordinate/provenance limit. |
| R-0484 | done | — | done — closed 2026-10-08 at `245cd51c`; the body is retained for its accepted limits; the typed-descriptor second step is R-0530. |
| R-0467 | done | — | done — moved to CHANGELOG.md 2026-10-19: the multi-kernel job runs on pushes to `main` (`lean_action_ci.yml`, `multi-kernel` job). |
| R-0470 | done | — | done — moved to CHANGELOG.md 2026-10-19: both capability-inference sites fixed and gated (`check_cap_inference.sh`). |

## Prose open-work dispositions

Every open-work prose item the four slice audits found (line numbers refer to commit `7b134645`; A =
non-phase sections, B = Phases 7 – 8.5, C = Phases 9 – 14, D = Phases 15 – 20). *live* items keep
their owner or become the named new task; *duplicate* and *superseded* items name the owner that
now carries the requirement; *done* items are closure records; *historical* items schedule nothing.

| slice | item | status | stage | disposition |
|---|---|---|---|---|
| A | “Trustworthy `proved`” 354 | done-but-not-moved for R-0004’s recorded scope | 1 | Retire closure prose to changelog; retain current repairs under R-0494/R-0495 |
| A | “Typed evidence and revocable policy” 355 | duplicate of R-0440 | 5, 7 | Merge into R-0440 |
| A | “Typed compositional contracts” 356 | duplicate of R-0473/R-0474 | 6 | Merge into those owners |
| A | “Total contract language” 357 | live | 6 | **R-0526** (owner) — Turn prose into a bounded task under the totality owner |
| A | “One faithful verification pipeline” 358 | duplicate of R-0450/R-0460 | 2, 4 | Merge into those owners; retain H19 separately |
| A | “Portable multi-kernel evidence” 359 | duplicate of R-0448/R-0454 | 5 | Merge into those owners |
| A | “Practical proof authoring” 360 | live | 2, 4 | Keep; attach bounded increments to Core proof engineering |
| A | “Auditable safety and resource guarantees” 361 | duplicate of R-0445 | 4, 7 | Merge into R-0445 |
| A | “Independently checkable compiler/evidence artifacts” 362 | duplicate of R-0275/R-0353 | 2, 5 | Merge; distinguish V1 consumer from complete cross-profile consumer |
| A | “Non-vacuous assurance operations” 363 | duplicate of R-0513/R-0476 | 1 | Merge into those owners |
| A | “Usable systems platform” 364 | live | 3; after-1.0/research | Keep, but split freestanding from the near-term platform exit |
| A | “Proof-aware local packages” 365 | duplicate of R-0285/R-0440 | 3, 7 | Merge bounded local packaging and later evidence separately |
| A | “Narrow defensible release” 366 | duplicate of R-0333 | 7 | Merge; distinguish preview from verification-alpha requirements |
| A | “Ecosystem packages and editor experience” 367 | live | 3, 7 | Split editor checkpoints from later ecosystem work |
| A | “Relational/concurrent/research extensions” 368 | live | after-1.0/research | Keep |
| A | Dependency-chain item 1, 383–384 | done-but-not-moved for `670ec85a` | 1 | Retire that checkpoint; retain exact-candidate requalification |
| A | Dependency-chain item 2, 385 | done-but-not-moved | 1 | Retire; closure explicitly recorded at 478–480 |
| A | Lifecycle separation, 386–388, 408–414 | duplicate of R-0482/R-0440 | 1, 5, 7 | Merge into their canonical status blocks |
| A | Canonical task/VIR freeze, 389–392, 421–425 | duplicate of R-0450/R-0454 | 5 | Merge; preserve independent-consumer-before-freeze condition |
| A | Typed contracts, 393–394 | duplicate of R-0473/R-0474 | 6 | Merge |
| A | Two-state records/arrays and frames, 394–395 | live | 6 | **R-0524** (owner) — Turn prose into an explicitly owned task |
| A | Ghost locals/parameters, 395 | live | 6 | **R-0524** (owner) — Turn prose into a task with erasure/admissibility exits |
| A | Decreases and total functions, 395–396 | live | 6 | **R-0526** (owner) — Turn prose into a task |
| A | Specification library, 396 | live | 6 | **R-0060** (owner) — Turn prose into a task |
| A | Declaration-isolated measurement, 397 | duplicate of R-0473 | 3, 6 | Merge |
| A | Shared assertion library, 397, 504–509 | duplicate of R-0481/R-0513 | 1 | Merge with a checked migration inventory |
| A | R-0353 Layer 0, 398 | duplicate of R-0353 | 2, 5 | Merge; separate minimal V1 scope |
| A | Shadow comparison and hostile review, 399–400 | duplicate of R-0353 | 5 | Merge |
| A | Proof-aware interfaces, 399 | duplicate of R-0440/R-0482 | 5, 7 | Merge |
| A | Heap/resource/relational growth, 401 | superseded by V2–V7 for compiler semantics | 4; after-1.0/research | Split compiler support from richer user verification |
| A | Receipt transport, 426–430 | duplicate of R-0440 | 5, 7 | Merge; keep signing distinct from replay validity |
| A | Architecture preparation 1, 437–444 | duplicate of R-0480/R-0481 | 1, 5 | Merge as a decision inventory |
| A | Architecture preparation 2, 445–447 | live | 1 | Merge into R-0480 |
| A | Architecture preparation 3, 448–453 | duplicate of R-0480 | 1 | Merge |
| A | Architecture preparation 4, 454–460 | duplicate of R-0481 | 1, 5 | Merge |
| A | Architecture preparation 5, 461–465 | live | 2, 5 | Merge into R-0481; require named reviewer and commit |
| A | Architecture preparation 6, 466–468 | live | 1, 5 | Keep as acceptance, not a separate implementation |
| A | Bounded migration sequence, 470–476 | superseded by current queue and R-0512 | 1, 2, 3, 4, 5, 6 | Retire ordering; preserve per-consumer migration controls |
| A | “native-trust receipts stay withheld,” 480 | live | 5 | **R-0532** (owner) — Merge into R-0506/R-0440 with explicit trust labels |
| A | Graduation contract, 487–502 | live | 1, 2, 3, 4, 5, 6, 7 | Keep as standing acceptance requirements |
| A | Cross-phase forcing artifact, 574–579, 587–589 | duplicate of R-0486/R-0119 | 3, 7 | Merge into selected flagships; avoid a fourth flagship |
| A | Constant-time boundary, 581–585 | live | after-1.0/research | Keep |
| A | Historical compaction, 593–607 | duplicate of R-0480/R-0481 | 1 | Merge; apply to this slice |
| A | Schema ownership, 610–614 | live | 1, 2, 3, 4, 5, 6, 7 | Keep as acceptance |
| A | Evidence debt / first Register A row, 640–658 | live | 2, 4 | Keep, with explicit allowances for required V1 evidence delivery |
| A | Cross-document consistency trigger, 669–686 | duplicate of R-0480/R-0481 | 1 | Merge; trigger has plainly fired |
| A | Claims vocabulary consistency, 683–684 | live | 1 | Merge into R-0481 |
| A | Register versus generator meaning, 684–686 | duplicate of R-0460/R-0498 | 1, 4 | Merge |
| A | Spike/kill criteria, 696–705 | live | 1, 2, 3, 4, 5, 6, 7 | Keep as process requirement |
| A | “Re-qualification for the next integration,” 734–741 | live | 1 | Merge into R-0482/R-0513; remove obsolete fixed count |
| A | “Shard balancing,” 741–742 | live | 1 | **R-0519** (owner) — Turn prose into a performance-pulled task |
| A | Six invalid experiments, 767–772 | done-but-not-moved | 1 | Retire; later run records all six killed |
| A | Two campaign survivors, 764–773 | done-but-not-moved | 1 | Retire; closure at `964073e7` recorded |
| A | Reconciliation matrix, 787–790 | live | 1 | Merge into R-0513; require explicit closure evidence |
| A | Campaign timing, 792–794 | live | 1 | Merge into R-0513 |
| A | Content-bound snapshots, 794–796 | live | 1 | Merge into R-0513 |
| A | Isolated workers, 796–798 | live | 1 | Merge into R-0513 |
| A | Old 95-family exit, 800–811 | done-but-not-moved | 1 | Retire historical exit; current queue owns next qualification |
| A | Harness checkpoint headline, 814–819 | historical | 1 | Retire to archive; not current status |
| A | H-1, 834 | live | 1 | **R-0518** (owner) — Turn prose into a harness task under R-0513 |
| A | H-2, 835 | live | 1 | Merge into R-0513; verify supervisor path, not just driver exit |
| A | H-3, 836 | done-but-not-moved | 1 | Retire closure text |
| A | H-4, 837 | live | 1 | **R-0518** (owner) — Turn prose into a task |
| A | H-5, 838 | done-but-not-moved | 1 | Retire closure text |
| A | H-6, 839 | live | 1 | Merge into R-0513 |
| A | H-7, 840 | live | 1 | **R-0518** (owner) — Turn prose into a task; reconcile missed pre-qualification deadline |
| A | H-9, 841 | live | 1 | **R-0518** (owner) — Turn prose into a task |
| A | Ten V1 links / intrinsic casts, 878–881 | superseded by R-0495/R-0512 migration | 4 | Merge into claim inventory; maintain current fail-closed behavior |
| A | Package identity overbinding, 908–926 | duplicate of R-0482 | 1 | Merge into R-0482’s current state |
| A | Executable-byte provenance, 928–933 | live | 2, 5 | **R-0521** (owner) — Turn prose into a task; minimal V1 provenance cannot wait for full stage 5 |
| A | “No qualifying current-inventory result,” 948–954 | historical | 1 | Retire; contradicted by 723–732 |
| A | Delete `bodyIdentityUses`, 990–1007; 1670–1671 | done-but-not-moved | 1 | Retire |
| A | Producer-refusal table: for-loop, 1024, 1040–1055 | done-but-not-moved | 1 | Retire |
| A | Producer-refusal table: array literal and cast types, 1025, 1028, 1057–1082 | done-but-not-moved for recorded slice | 1 | Retire; keep later intrinsic-cast gap separately |
| A | Producer-refusal table: assert/assume, 1026, 1029, 1103–1121 | done-but-not-moved | 1 | Retire |
| A | Producer-refusal table: imported callee identity, 1027, 1123–1131 | done-but-not-moved for 18 recorded cases | 1 | Retire |
| A | Producer-refusal table: self/if/discard/field place, 1030–1033, 1084–1095 | done-but-not-moved | 1 | Retire |
| A | Function-type capability identity, 1073–1075, 1276–1279 | superseded by S5/S8a and R-0495 | 2, 4 | Merge; require V5 coverage |
| A | Five incomplete generic callees, 1136–1138, 1306–1309 | superseded by R-0492/R-0510 | 2, 4 | Merge |
| A | Four unresolved mangled methods, 1139, 1306–1309 | superseded by R-0492 | 2 | Merge |
| A | Two trait-method refusals / `TraitId`, 1140–1144, 1273–1275, 1306–1309, 1344 | superseded by R-0492/R-0510 | 2, 4 | Merge |
| A | `alloc/free` argument omission, 1149–1152, 1486–1489 | superseded by R-0495/R-0512 | 4 | Merge as a mandatory V3 negative control |
| A | Dependency graph/root, 1182–1196 | done-but-not-moved for R-0004 scope | 1 | Retire; 860–864 records completed closure |
| A | Edge kinds and assumption propagation, 1213–1216 | done-but-not-moved for R-0004 scope | 1 | Retire |
| A | Whole-table reachability/content, 1215–1257 | done-but-not-moved | 1 | Retire stale “missing” text; 1225–1244 records landing |
| A | Freshness, 1258 | done-but-not-moved | 1 | Retire; 883–889 records closure |
| A | Fingerprint migration, 1259–1260 | done-but-not-moved for completed population | 1 | Retire; retain explicit unsupported-link inventory |
| A | Receipts, 1261 | done-but-not-moved | 1 | Retire |
| A | Ninth proof table, 1262, 1328–1342 | live | 6 | Keep as queue row 14; remove obsolete R-0004 blocker |
| A | Struct initializer evaluation order, 1267–1272, 1313–1326, 1337–1340 | done-but-not-moved as decision | 1 | Retire old blocker; R-0500 retains semantic enforcement |
| A | Elaborated-theorem edge classification, 1346–1351 | historical | 5 | Keep only as a constraint in dependency/evidence owner |
| A | Structured subject-facts emission, 1376–1385 | live | 1, 5 | Merge into R-0494/R-0481 |
| A | Branch-health first exercise, 1426–1427 | historical | 1 | Retire dated assertion; require current workflow qualification |
| A | Property rather than count gates, 1429–1439 | duplicate of R-0513 | 1 | Merge |
| A | Pre-push runtime, 1449–1455 | duplicate of R-0513 | 1 | Merge |
| A | Mutation anchor/error handling, 1457–1474 | done-but-not-moved | 1 | Retire |
| A | Cancelled CI reported red, 1476–1481 | done-but-not-moved | 1 | Retire; 1387–1391 records fix |
| A | Aliased imported method E0264, 1128–1129, 1490–1493 | live | 3 | Merge into R-0008 with an exact alias-method fixture |
| A | Raw `lookupVar` binder positions, 1495–1497 | superseded by R-0492 | 2 | Retire alternative representation; retain prohibition |
| A | Invalid versus infrastructure scoring, 1502–1523 | done-but-not-moved | 1 | Retire |
| A | Divergence-detection invalid, 1523–1526, 1542–1545 | done-but-not-moved | 1 | Retire; later result records kill |
| A | Six-hour campaign limit, 1526–1528, 1536, 1543–1545 | done-but-not-moved | 1 | Retire; sharded completion recorded |
| A | Mutation 52 survives, 1539–1541 | live | 1 | **R-0519** (owner) — Turn prose into a task; do not infer closure from a different campaign |
| A | Full mutation-harness worktree support, 1546–1554 | live | 1 | **R-0519** (owner) — Turn prose into a task; early refusal is not support |
| A | Content-based CI gate inventory, 1555–1558 | duplicate of R-0513 | 1 | Merge |
| A | Safe std identity refresh, 1559–1564 | live | 1, 5 | Merge into R-0482 |
| A | Branch full-CI dispatch/watch, 1566–1568 | live | 1 | Merge into R-0513 |
| A | `knownEffectfulForeignSymbols`, 1568–1570 | live | 1, 4 | Merge into R-0498; connect to external-call contracts |
| A | Cross-kernel proposition check, 1630–1632 | duplicate of R-0454 | 5 | Merge; subject digest alone is insufficient |
| A | Multi-kernel status graduation, 1633–1636 | duplicate of R-0448 | 5 | Merge |
| A | Stale `kernel_disagreement`, 1637–1639 | live | 5 | Merge into R-0494/R-0440 |
| A | Kernel-set receipt binding, 1640–1642 | duplicate of R-0440 | 5 | Merge |
| A | Missing `solver_error`, 1643–1646; 1582 | live | 3, 5 | **R-0520** (owner) — Turn prose into a task; restore the three strict assertions |
| A | Pinned spike epochs, 1647–1648 | historical | 1 | Retire merge-specific instruction |
| A | Stale R-0004 “still open” list, 1670–1673 | historical | 1 | Retire; individual surviving gaps are listed above |
| A | Call/package-closed reasoning, 1698–1703 | duplicate of R-0473/R-0474/R-0440 | 6, 7 | Merge |
| A | Wider state, termination and resources, 1701–1703 | live | 4, 6 | Keep as coverage map; separate compiler semantics from user contracts |
| A | Relational contracts, 1702–1703 | live | after-1.0/research | Keep |
| A | Portable artifacts/publishing, 1704–1707 | duplicate of R-0353/R-0440 | 5, 7 | Merge |
| A | Graduated family faithfulness, 1708–1712 | duplicate of R-0460/R-0512 | 2, 4 | Merge |
| A | Module/property policy and hole budgets, 1716–1719 | duplicate of R-0440 | 7 | Merge |
| A | Charter order: fail-visible accounting, 1726 | duplicate of R-0491/R-0498 | 1 | Merge |
| A | Charter order: typed IR, 1727–1729 | duplicate of R-0450/R-0460 | 2, 4 | Merge |
| A | Charter order: composition/state, 1730–1731 | duplicate of R-0473/R-0474 | 6 | Merge |
| A | Charter order: loop preservation/totality, 1732–1734 | live | 4, 6 | Merge Core loop binding into R-0495; retain totality task |
| A | Charter order: enums/state/memory, 1735 | superseded by V2–V7 for runtime semantics | 4, 6 | Split runtime support and specification-language extension |
| A | Charter order: resource/release policy, 1736 | duplicate of R-0445/R-0440 | 4, 7 | Merge |
| A | Charter order: paired execution, 1737 | live | after-1.0/research | Keep |
| A | Charter order: package/replay last, 1738 | superseded by V1 evidence delivery | 2, 5, 7 | Retire “last” dependency; preserve later package scope |
| A | Useful-change workload, 1745–1751 | duplicate of R-0486 | 3, 6, 7 | Merge |
| A | One-module adoption, 1755–1756 | duplicate of R-0285/R-0287 | 3 | Merge |
| A | One forcing domain, 1757–1759 | duplicate of R-0136 | 3; after-1.0/research | Merge source-flow work; keep relational proof research-gated |
| A | One feedback surface, 1760–1761 | duplicate of R-0494/R-0515 | 3 | Merge |
| A | Package completion test, 1763–1766 | duplicate of R-0353/R-0440 | 7 | Merge |
| A | Docs convergence 1, 1779–1780 | duplicate of R-0480 | 1 | Merge |
| A | Docs convergence 2, 1781 | duplicate of R-0480 | 1 | Merge |
| A | Docs convergence 3, 1782–1783 | duplicate of R-0481 | 1, 5 | Merge |
| A | Docs convergence 4, 1784–1785 | duplicate of R-0480 | 1 | Merge |
| A | Docs convergence 5, 1786–1788 | duplicate of R-0481 | 5 | Merge |
| A | Docs convergence 6, 1789–1791 | duplicate of R-0477/R-0480 | 1, 4 | Merge; retire obsolete branch-specific dates |
| A | Manual graduation review, 1793–1795 | live | 1, 2, 3, 4, 5, 6, 7 | Keep |
| A | Safe library abstractions, 1817 | done-but-not-moved for R-0483 | 3 | Retire completed scope; keep R-0485 review clarity separately |
| A | Authority/effects, 1818 | done-but-not-moved for R-0484 | 1 | Retire completed scope; checker proof still belongs to S8/S14 |
| A | Compositional semantics, 1819 | duplicate of R-0473/R-0474/R-0477 | 6 | Merge |
| A | Tuples/destructuring decision, 1820 | duplicate of R-0515 | 3 | Keep decision checkpoint |
| A | Bounded milestone: admitted subset/purity, 1827–1831 | duplicate of R-0473 | 6 | Merge |
| A | Bounded milestone: call composition/frame, 1832–1833 | duplicate of R-0474 | 6 | Merge with two-state owner |
| A | Bounded milestone: explicit evidence/gaps, 1834–1836 | duplicate of R-0495/R-0460 | 4, 6 | Merge |
| A | Bounded milestone: interface stability, 1837–1839 | duplicate of R-0482/R-0440 | 5, 6 | Merge |
| A | Bounded milestone: costs/controls, 1840–1841 | duplicate of R-0150/R-0486 | 3, 6 | Merge |
| A | Bounded milestone: non-author repair/replay, 1842–1847 | duplicate of R-0486 | 6, 7 | Merge |
| A | Actionable failure feedback, 1849–1852 | duplicate of R-0137/R-0466/R-0515 | 3 | Merge |
| A | General heap/concurrency/relational/resource proofs, 1854–1858 | live | after-1.0/research | Keep; compiler heap safety still belongs to stage 4 |
| A | Attestation migration question, 1868–1869 | duplicate of R-0482 | 1, 5 | Merge |
| A | Writer proof/report eligibility, 1869–1871 | live | 1, 4 | Merge into R-0494/R-0495; do not inherit blanket R-0484 closure |
| A | Explicit-error diagnostics/defer/API improvements, 1873–1875 | duplicate of R-0485/R-0515 | 3 | Merge |
| A | Semantic correspondence before backend breadth, 1876–1881 | live | 2, 4, 6 | Keep; native verified backend is required foundation, not optional breadth |
| A | Audit-tool outputs, 1882–1885 | duplicate of R-0494/R-0440 | 3, 5 | Merge |
| A | Function-plus-context workload evaluation, 1886–1892 | duplicate of R-0285/R-0490 | 3 | Merge |
| A | Canonical attestation ordering, 1901, 2271–2278 | done-but-not-moved | 1 | Retire; generator now sorts under `LC_ALL=C` and names order gate |
| A | Diff-to-gate mapping, 1902, 2285–2288 | done-but-not-moved | 1 | Retire old absence claim; R-0513 owns trustworthy tiering |
| A | Linux TCP fixtures / false flaky label, 1903–1907 | done-but-not-moved | 3 | Retire |
| A | Regenerate all derived artifacts after identity change, 2279–2284 | live | 1 | Merge into safe identity-refresh task |
| A | Clean-tree campaign golden, 2289–2292 | live | 1 | Merge into R-0513 acceptance |
| A | H24, 1913, 2080–2097, 2162–2169, 2183–2194 | done-but-not-moved | 1 | Retire “open” text; closure explicitly stated at 1918 |
| A | H19/Register A, 1914, 1923–1929, 2131–2132, 2209–2210, 2225–2226, 2448–2451, 3075–3079, 3108–3109, 3124–3125, 3384–3392 | duplicate of R-0460 | 2, 4 | Merge all current status into R-0460 |
| A | H20, 1915, 2211–2212, 2782, 2790, 2902–2903, 3110–3111 | live | 1; after-1.0/research | **R-0538** (owner) — Merge immediate classification into R-0506; task the trust-reduction research |
| A | H21, 1916, 2213–2215, 2775–2776, 3112–3114 | duplicate of R-0451 | after-1.0/research | Keep upstream-gated; preserve honest trust class |
| A | Lean renderer agreement, 1931–1939, 2120–2123 | duplicate of R-0450 | 4 | Merge |
| A | SMT renderer agreement, 2120–2123, 3340–3342 | done-but-not-moved for recorded arithmetic fragment | 4 | Retire completed fragment; retain explicit scope |
| A | Every-surface assumption enforcement, 1945–1951 | duplicate of R-0494 | 1 | Merge; retain R-0461 regressions |
| A | Missing-family inventory, 1965 | duplicate of R-0498/R-0118 | 1, 4 | Merge |
| A | Repository nightly reachability, 1969–1974, 2113–2119 | duplicate of R-0468 | 1 | Merge; distinguish dispatch success from scheduled reachability |
| A | Multi-kernel composition opt-in, 1964, 2237–2242, 3136–3141 | duplicate of R-0467 | 1, 5 | Merge into qualification tier |
| A | Signal/cleanup operational warning, 2002–2005 | duplicate of R-0513 | 1 | Merge into harness lifecycle tests |
| A | R-0004 remaining slices, 2041–2045 | historical | 1 | Retire |
| A | Two “counts as proved” predicates, 2056–2065 | duplicate of R-0494 | 1 | Merge |
| A | R-0461/H23, 2067–2078 | done-but-not-moved | 1 | Retire closure text; keep fixtures |
| A | Conditional evidence vocabulary, 2099–2105 | duplicate of R-0458/R-0440 | 5 | Merge |
| A | Neutral obligation identity, 2106–2112 | duplicate of R-0454 | 5 | Merge; retire “no subjectDigest exists” |
| A | Binary claim fuzzing, 2124–2128 | duplicate of R-0491/R-0462 | 1, 4 | Merge into prevention harness |
| A | IR absorbs four drivers, 2129–2130, 3336–3339 | duplicate of R-0450/R-0455 | 4 | Merge |
| A | Non-arithmetic meaningful claims, 2133–2135 | duplicate of R-0459 | 4, 7 | Merge; obey Core-claim migration |
| A | Farkas/certificate probe, 2136–2140 | duplicate of R-0463 | 5 | Keep timeboxed |
| A | Multi-kernel graduation, 2141–2142 | duplicate of R-0448 | 5 | Merge |
| A | Second-host metatheory, 2143–2144 | duplicate of R-0456 | after-1.0/research | Keep pull-gated |
| A | Realizations, 2143–2144, 2306, 2514, 2577 | duplicate of R-0449 | after-1.0/research | Keep; do not equate this with all of H19 |
| A | Diagnostics measurement and repairs, 2153–2174 | duplicate of R-0466/R-0471 | 3 | Merge into R-0515 checkpoints |
| A | Capability inference, 2174–2176 | done-but-not-moved | 1 | Retire; R-0470 records both sites done |
| A | Recursive stack budget honesty, 2177–2192 | duplicate of R-0469 | 4 | Merge |
| A | SMT datatype certificate ceiling, 2217–2221, 3084–3086, 3116–3120 | live | after-1.0/research | Keep named unsupported certificate profile |
| A | Register B remaining rows, 2227, 2498, 3080–3081, 3126 | duplicate of R-0455 | 4 | Merge |
| A | Register C “no tracking” residue, 2228–2232, 3127–3131 | historical | 1 | Retire stale sentence |
| A | Discovery completeness, 2233–2235, 3273–3280 | done-but-not-moved for narrow predicates | 4 | Retire completion text; preserve stated antecedents |
| A | Core WP generator, 2332–2344, 2430–2432, 2496, 2566 | duplicate of R-0477/R-0460 | 4, 6 | Merge; require theorem, not “Core implies sound” |
| A | H27 typed contracts, 2382–2389, 2416–2422 | duplicate of R-0473 | 6 | Merge |
| A | H25 capture-safe substitution, 2390–2395 | duplicate of R-0474 | 2, 6 | Merge IDs early; contract substitution proof in stage 6 |
| A | Fixture/std coupling, 2404–2408 | duplicate of R-0475 | 1, 3 | Merge; snapshot refresh did not fix coupling |
| A | 285 unmeasured contract files, 2409–2412 | duplicate of R-0473 | 3, 6 | Merge; recompute denominator |
| A | Remaining negative-control sweep, 2423–2426, 2438–2441, 3071–3074, 3394–3405 | duplicate of R-0513/R-0498 | 1 | Merge into one current inventory; preserve migration-gate deprioritization |
| A | `old`/two-state postconditions, 2433–2437, 2502, 2507, 2517–2529, 2567–2568 | live | 6 | **R-0524** (owner) — Turn prose into one owned task |
| A | Bitwise tier, 2442–2444, 2708, 2854–2856 | live | 4 | Merge into R-0459/R-0495 after Core statement readiness |
| A | Decreases/recursion, 2445–2447, 2500, 2573, 2618, 2631, 2712 | live | 6 | **R-0526** (owner) — Turn prose into a task |
| A | MIN negation trap VC, 2455–2456, 3090 | duplicate of R-0460/R-0493 | 1, 4 | Merge semantics and VC obligations separately |
| A | Float→int trap VC, 2455–2456, 3090 | duplicate of R-0460/R-0493 | 1, 4 | Merge |
| A | Real non-arithmetic corpus coverage, 2457–2458, 2905–2992 | duplicate of R-0459/R-0486 | 4, 7 | Merge; retain anti-tautology controls |
| A | Nonlinear tier/Mathlib decision, 2459–2460, 2707, 2763–2776, 2894–2898 | live | after-1.0/research | Keep workload-gated; retire “two kernels is inherently misleading”; owner **R-0448** (recorded when the section was archived) |
| A | Unannotated-let types, 2461, 3091–3093, 3306–3308 | superseded by R-0495 Core claims | 4 | Merge with exact coverage controls |
| A | Bare-name FFI closure, 2462–2463, 3094–3097 | superseded by R-0492/R-0514 | 2 | Merge |
| A | Stack report versus enforcement, 2464, 2635, 3100 | duplicate of R-0445/R-0469 | 4, 7 | Merge |
| A | Report-layer partial definitions, 2465, 3101, 3261–3272, 3369–3371 | superseded by R-0506/S12a | 2, 4 | Merge; discard stale 17/6/5 counts |
| A | Isabelle builder batch/single split, 2466, 3102 | live | 5 | **R-0522** (owner) — Turn prose into a bounded task |
| A | Loop invariants listed absent, 2501, 2574–2575 | historical | 6 | Retire “absent”; keep typed resolution/consumption below |
| A | Ghost variables/state, 2503 | live | 6 | **R-0524** (owner) — Merge into explicit ghost-state task |
| A | Spec enums, 2504, 2576, 2616 | live | 4, 6 | Merge into typed specification/Core claim growth |
| A | Spec pattern matching, 2505, 2576 | live | 6 | Merge with spec-enum task |
| A | Result/error-path contracts, 2506 | live | 6 | Merge into R-0473/R-0488 |
| A | Theory modules/refinement, 2508 | live | 6 | Merge into specification-library task |
| A | int/list/map/set/bitvector library, 2509, 2569–2572 | live | 6 | **R-0060** (owner) — Turn prose into one library task with scoped modules |
| A | Typed theorem-replay producer, 2511 | superseded by R-0494/S16 | 1, 5 | Merge |
| A | “No production receipts,” 2512 | historical | 1 | Retire; contradicted by R-0004 closure |
| A | Proof-session history, 2513 | live | 5 | Merge into R-0440; distinct from receipts/cache |
| A | Discovery register demotion, 2558–2562 | live | 4 | Merge into R-0460 retirement criteria; retain completeness obligation |
| A | Heap/separation logic, 2578, 2619 | live | after-1.0/research | Keep; V3 heap safety is not deferred |
| A | Typed loops/loop state, 2617 | duplicate of R-0495/R-0474 | 4, 6 | Merge |
| A | Generic proof scope, 2620 | duplicate of R-0271 | 4, 6 | Merge |
| A | Function pointers, 2621 | superseded by V6/V7 and S8 | 4 | Merge |
| A | Closures, 2621 | superseded by plan’s explicit closure exclusion | after-1.0/research | Retire as committed work |
| A | Capability “sound by construction,” 2622 | superseded by R-0499/R-0507 | 2, 4 | Replace with staged implementation/proof status |
| A | General liveness, 2632 | live | after-1.0/research | Keep |
| A | Single-return functional-correctness ceiling, 2633 | superseded by R-0495/R-0496 | 2, 4 | Merge |
| A | Relational/hyperproperties, 2634 | live | after-1.0/research | Keep |
| A | Time/resource bounds, 2635 | live | 4; after-1.0/research | Split required resource premises from richer cost proofs |
| A | Evidence vector, 2639–2643 | duplicate of R-0458/R-0440 | 5 | Merge |
| A | Formal target-printer correspondence, 2660–2663 | live | after-1.0/research | Keep optional proof-backend strengthening |
| A | “Next rungs 6+7,” 2751–2756; original assessment 2881–2889 | done-but-not-moved for recorded struct/array scope | 4 | Retire; enum support remains separately open |
| A | Isabelle bitwise syntax/type blocker, 2778–2790, 2813–2826, 2899–2903, 2978–2982 | done-but-not-moved | 4 | Retire blocker; solved at 2828–2852 |
| A | Recursive syntax, 3020 | live | 6 | **R-0526** (owner) — Merge into termination task |
| A | Recursive VC family, 3012–3018, 3020 | live | 6 | **R-0526** (owner) — Merge into termination task |
| A | Recursive extraction, 3020–3021 | live | 6 | **R-0526** (owner) — Merge into Core proof-authoring growth |
| A | Induction scripts, 3021–3022 | live | 6 | **R-0526** (owner) — Merge into proof-authoring owner |
| A | Mutual-recursion measure, 3034–3035 | live | 6 | **R-0526** (owner) — Merge into termination task |
| A | Symbolic stack bounds, 3035–3037 | live | 6; after-1.0/research | Merge into resource-contract owner |
| A | Div/shift/overflow gap reporting, 3066–3070 | duplicate of R-0495/R-0472 | 1, 4 | Merge; maintain fail-visible reporting during migration |
| A | Non-arithmetic demo, 3082–3086 | done-but-not-moved | 4 | Retire demo request; meaningful corpus coverage remains |
| A | Direct extraction eligibility, 3098–3099, 3243–3244 | superseded by R-0495/R-0512 | 4 | Merge into complete claim-dependency admission |
| A | Unclassified `fixed_capacity` drop, 3104, 3350 | live | 1, 4 | **R-0523** (owner) — Turn prose into a diagnostic/coverage task |
| A | Predictable-loop/indirect-call/stack/FFI fixes, 3203–3225 | done-but-not-moved | 1 | Retire historical fix descriptions |
| A | Transitive recursion admission, 3232–3250 | done-but-not-moved | 1 | Retire superseded direct-admission proposal |
| A | Register B row 2, 3254–3260 | duplicate of R-0455 | 4 | Merge; retain precise mathematical obligation |
| A | Discovery antecedent gap, 3273–3277 | live | 4 | Merge into R-0460/R-0118 |
| A | Parenthesized/field/nested bounds, 3281–3294 | done-but-not-moved | 1 | Retire; retain regressions |
| A | Scoped shift/overflow types, 3295–3305 | done-but-not-moved | 1 | Retire |
| A | Scoped bounds and for-init, 3309–3324 | done-but-not-moved | 1 | Retire |
| A | Unannotated/nested arrays and bounds gap reporting, 3325–3332 | done-but-not-moved | 1 | Retire |
| A | Register B row 3, 3333–3335 | duplicate of R-0455 | 4 | Merge |
| A | Collapse printers, 3340–3342 | duplicate of R-0450/R-0455 | 4 | Merge; do not erase independent target obligations |
| A | Model casts, 3343–3349 | done-but-not-moved for recorded model | 1 | Retire historical completion; R-0493 handles current width-pair semantics |
| A | Printer/evaluator totalization, 3351–3368 | done-but-not-moved | 2 | Retire |
| A | Binary fuzz widening, 3375–3378; 3103 | duplicate of R-0491/R-0462 | 1, 4 | Merge |
| A | Zero claimed-function fuzz population, 3379–3382 | duplicate of R-0491/R-0513 | 1 | Merge; require positive inventory and completed minima |
| A | Call-site substitution theorem, 3386–3389 | duplicate of R-0460/R-0474 | 4, 6 | Merge |
| A | Totality gate divergence fixture, 3406–3409 | live | 1 | Merge into R-0498/R-0513 |
| A | LLVM JIT fallback, 3183–3185, 3417–3421 | done-but-not-moved | 3 | Retire environmental history |
| A | 6C telemetry, 3429–3430 | live | 3 | Merge into incremental-driver owners |
| A | ProofCore partial reduction, 3431–3432 | superseded by R-0506/S12a | 2, 4 | Merge |
| A | CompilerDB/interned facts, 3433–3434 | duplicate of R-0139 | 3 | Merge |
| A | Consume-then-break exemption, 3435–3437 | live | after-1.0/research | Keep workload-gated; require S8 rule/proof impact |
| A | Qualified names/import aliases, 3438 | duplicate of R-0008/R-0492 | 2, 3 | Merge |
| A | FFI surface, 3438 | duplicate of R-0285 | 3 | Merge |
| A | Target roots/constants, 3438–3441 | live | 3, 4 | Merge into target/profile owner |
| A | lint/vet/doc/bench, 3439–3441 | duplicate of R-0515 | 3 | Merge bounded checkpoints |
| A | trace/debug/eval/inspect/LSP, 3439–3441 | duplicate of R-0515 | 3 | Merge |
| A | Build-output root, 3442–3445 | duplicate of R-0447 | 3 | Merge |
| A | “File order…no parallel tracks,” 3451–3458 | superseded by seven-stage queue | 1 | Retire |
| A | Allocator-specific APIs, 3481–3484 | live | 3 | Keep |
| A | Returning/storing safe references, 3488–3498 | superseded by R-0500’s strict rule | 1; after-1.0/research | Keep prohibition; record reopening only as research |
| A | QBE-first pipeline spine, 3502–3514 | superseded by R-0511/R-0514/R-0515 | 2, 3 | Retire sequencing |
| A | Refactor A: typed boundaries, 3526–3578 | superseded by R-0497/S5/S8a | 2 | Merge; replace checked-source ladder with payload-bound Core chain |
| A | Refactor B: observable pass envelope, 3580–3623 | duplicate of R-0115 | 2, 3 | Merge; align with formal pass obligations |
| A | Refactor C: stable indexed facts, 3625–3665 | duplicate of R-0115/R-0492 | 2, 3 | Merge |
| A | Refactor D: transform once/render many, 3667–3698 | duplicate of R-0494/R-0115 | 1, 3 | Merge |
| A | Refactor E: legalization ownership, 3700–3727 | duplicate of R-0493/R-0498/R-0503 | 1, 4 | Merge; generate from current IR semantics |
| A | Refactor F: resolver panics, 3739 | duplicate of R-0024 | 3 | Merge |
| A | Refactor F: specialization panics, 3740 | duplicate of R-0510 | 2, 4 | Merge |
| A | Refactor F: Core lookup panics, 3741 | duplicate of R-0497/R-0499 | 2 | Merge |
| A | Refactor F: lowering panics, 3742 | duplicate of R-0501 | 2, 4 | Merge |
| A | Refactor F: SSA/cleanup assumptions, 3743 | duplicate of R-0503/R-0504 | 2, 4 | Merge |
| A | Refactor F: layout panics, 3731–3734, 3744 | duplicate of R-0502/R-0024 | 2, 4 | Merge; remove QBE prerequisite |
| A | Refactor F: emitter/runtime panics, 3745 | duplicate of R-0503/R-0505 | 2, 4 | Merge |
| A | Refactor F: report unknown constructors, 3746 | duplicate of R-0118/R-0494 | 1, 3 | Merge |
| A | Refactor F controls, 3759–3766 | live | 2, 3 | Merge into R-0024/R-0498 |
| A | Refactor G: cleanup rule registry, 3768–3789 | superseded by R-0504/S12 | 2, 4 | Merge; require semantic certificates/validator obligations |
| A | Refactor G controls, 3791–3802 | duplicate of R-0504 | 2, 4 | Merge; byte parity alone is insufficient |
| A | Refactor H: unified driver, 3804–3846 | duplicate of R-0139/R-0497 | 2, 3 | Merge; update backend and checked-artifact chain |
| A | Refactor I: transformation origins, 3848–3878 | duplicate of R-0114/R-0515 | 2, 3 | Merge |
| A | Refactor J: constructor matrix, 3880–3903 | duplicate of R-0118 | 1, 4 | Merge; include new IRs and native backend |
| A | Phase 7.5 scheduling, 3911–3917 | superseded by S10–S13/V1 | 2, 4 | Retire compulsory chain; reconcile QBE tasks individually |
| A | Phase 8 scheduling, 3918–3924 | superseded by R-0492/R-0497/R-0514 | 2 | Merge remaining interface/matrix work into R-0117/R-0118 |
| A | Incremental consumers, 3925–3928 | duplicate of preceding refactor rows | 3 | Retire duplicate list after merging |
| A | Native backend only after QBE/validation, 3929–3930 | superseded by R-0505/R-0511 | 2 | Retire |
| A | Refactor acceptance, 3932–3950 | live | 2, 3, 4 | Keep after replacing QBE-specific completion condition |
| A | No MIR/allocator/object writer without forcing case, 3952–3956 | superseded by R-0511 | 2 | Retire; verified-native V1 is the forcing case |
| A | Incremental/certificate trust split, 3958–3965 | live | 2, 3, 5 | Keep invariant; update obsolete phase sequencing |
| A | External-validation stop/go gate, 3967–3973 | superseded by R-0515’s continuing pilots | 3 | Retire as global blocker; keep measured product decisions |
| A | Early probe: authority diff, 3979–3980 | duplicate of R-0182/R-0183 | 3, 5 | Merge |
| A | Early probe: proof cost, 3981 | duplicate of R-0150 | 2, 3 | Merge |
| A | Early probe: translation validation, 3982–3983 | duplicate of R-0299/R-0300 | 4 | Merge |
| A | Early probe: invalidation scale, 3984–3985 | duplicate of R-0137 | 3 | Merge |
| A | Float profile, 3987–3991 | superseded by V7/S7 profile requirements | 4 | Merge; no silent unproved float admission |
| A | Freestanding/MMIO/asm, 3993–3998 | live | after-1.0/research | Keep |
| A | Target-selected roots / possible cfg, 4000–4004 | live | 3, 4 | Keep; task any admitted cfg extension |
| A | Native debug info, 4006–4009 | superseded by R-0515 V1 checkpoints | 3 | Merge; remove “Phase 15+ only” delay |
| A | Contract/spec trust, 4011–4016 | duplicate of R-0473/R-0271/R-0506 | 1, 6 | Merge |
| A | Small workload-driven std, 4018–4024 | duplicate of R-0515/R-0488 | 3 | Keep direction; use std coverage inventory |
| A | Enum construction rights, 4038–4043 | duplicate of R-0011 | 3, 4 | Merge |
| A | `BytesRaw` ownership split, 4044–4047 | duplicate of R-0018 | 3 | Merge; reconcile borrowed representation with second-class references |
| A | One Reader/Writer spine, 4048–4050 | duplicate of R-0015 | 3 | Merge |
| A | UTF-8 boundary validation, 4051–4053 | duplicate of R-0013 | 3 | Merge |
| A | Checked C-string conversion, 4054–4058 | duplicate of R-0013 | 3 | Merge |
| A | Hosted adapters absorb Unsafe, 4059–4062 | duplicate of R-0014 | 1, 3 | Merge; preserve only-Unsafe absorption |
| A | Linear-conservation assertion fixture, 4065–4068 | duplicate of R-0012/R-0499 | 2, 3 | Merge |
| A | Trusted raw dereference duplication, 4069–4074 | duplicate of R-0014 | 1, 4 | Merge; current safety repair precedes V6 proof |
| A | Backend-neutral LayoutFacts, 4077–4078 | superseded by R-0502 | 2, 4 | Merge |
| A | Typed emitter fragments / bug 027, 4079–4082 | live | 3, 4 | Merge semantic emission into R-0503; retain profiling-triggered optimization |
| A | Layout diagnostics sequence, 4083–4086 | superseded by R-0502/R-0503 | 2, 4 | Retire ordering |
| A | Stable semantic IDs / bug 054, 4092–4100 | duplicate of R-0492/R-0007 | 1, 2 | Merge |
| A | Compiler-derived interfaces, 4101–4103 | duplicate of R-0117 | 3 | Merge |
| A | Typed pass boundaries, 4104 | superseded by R-0497/R-0510 | 2 | Merge |
| A | Immutable indexed facts, 4105–4106 | duplicate of R-0115 | 2, 3 | Merge |
| A | Generated coverage matrix, 4107 | duplicate of R-0118 | 1, 4 | Merge |
| A | R-0114–R-0118 exclusive order, 4109–4111 | superseded by current queue | 2, 3 | Retire |
| A | Safe slice I/O, 4113–4116 | duplicate of R-0016 | 3 | Merge; preserve owner/provenance and non-escape rules |
| A | “Corresponding local/CI gate work,” 4119–4120 | live | 1, 3 | Replace with explicit task IDs and acceptance links |
| B | “Capability-complete” · 4138 | duplicate of R-0021/R-0484 | 1, 3 | Merge; correct obsolete trusted-extern semantics |
| B | “One IO spine” · 4145 | duplicate of R-0015/R-0033 | 3 | Merge; replace acquisition-only authority with handle-use requirements |
| B | “Linearity-real” · 4149 | duplicate of R-0031/R-0006 | 1, 3 | Merge; preserve returned ownership on removal |
| B | “Self-enforcing + selectively proven” · 4154 | duplicate of R-0021/R-0057/R-0061 | 3, 4 | Merge as release-scoped std acceptance |
| B | Three layers/internal iteration/breadth hold · 4165 | duplicate of R-0064/R-0032/R-0065 | 3; after-1.0/research | Merge as policy, not separate queue |
| B | Phase 7A correction rule · 4174–4195 | live | 1, 3 | Keep; no symmetry-driven redesign |
| B | “Global Task Sequence” steps 1–7 · 4206–4223 | superseded by top queue | 1, 2, 3, 4, 5, 6, 7 | Retire sequence; task obligations remain inventoried above |
| B | Historical trust-boundary table · 4255–4281 | historical | 1 | Archive under R-0004’s explicit historical boundary |
| B | Pending conversions/no-manifest tables · 4354–4409 | historical | 1 | Archive; preserve no-authority exclusions in replay documentation |
| B | “WHAT IS STILL OPEN” · 4429 | historical | 1, 6 | Archive completed parts; ninth table remains explicit stage-6 follow-up |
| B | Atomic-transition entrance conditions · 4463–4473 | historical | 1 | Archive as closure evidence; transfer applicable controls under R-0512 |
| B | “REMAINING for the atomic join transition” · 4515 | historical | 1 | Retire stale open wording |
| B | Missing dispatch misroute mutation · 4531 | historical | 1 | Archive dated gap; replacement controls belong to R-0491/R-0512 |
| B | “ninth proof table” · 4536 | live | 6 | **R-0525** (owner) — Merge with explicit ninth-table checkpoint at 5134–5136 |
| B | “Open boundary: report and process disposition” · 4544 | historical | 1 | Archive; current cross-surface authority belongs to R-0494 |
| B | “Open boundary: exact refusal-path controls” · 4571 | historical | 1 | Archive; preserve attack classes during migration |
| B | Closed manifest provenance/completeness · 4587, 4615 | done (not yet moved) | 1 | Move to closure history |
| B | “Open boundary: theorem-to-call-edge correspondence” · 4636 | historical | 1 | Archive under R-0004 closure; contract precision remains R-0473/74 |
| B | Package-2 caveats / “what that leaves” · 4794–4809 | historical | 1, 6 | Archive measurements; merge live contract/table follow-ups |
| B | “Cache legs remain deferred” · 4830 | duplicate of R-0145/R-0155 | 3, 4, 5 | Merge; never fabricate a test for a nonexistent cache |
| B | Receipt `importsId` limitation · 4955 | live | 2, 5 | **R-0521** (owner) — Merge into canonical task and VP S16 retained-artifact binding |
| B | Proof-library/workspace binding · 4959 | live | 2, 5 | **R-0521** (owner) — Merge into canonical task/R-0353; retain exact dependency controls |
| B | Cache-disabled/corrupted bullet · 4968 | duplicate of R-0145 | 3 | Merge with cache authority parity |
| B | Library package cannot carry evidence · 4972 | historical | 1 | Archive dated diagnosis; current cross-package remainder is R-0482 P1 |
| B | Package-3 eight-step sequence · 4993–5047 | historical | 1 | Archive shipped steps; retain contract/table/cache exceptions at current owners |
| B | Five-way migration · 5085 | duplicate of R-0482 | 1 | Merge qualification remainder; migration itself recorded landed |
| B | “Compiler provenance hardening” · 5093 | live | 2, 5 | **R-0521** (owner) — Turn prose into a bounded task; minimal V1 binding cannot wait for stage 5 |
| B | “Canonical production VerificationTask” · 5097 | live | 2, 5 | **R-0521** (owner) — Turn prose into a task with V1 and cross-profile exits |
| B | “R-0353/schema conformance” · 5100 | duplicate of R-0353 | 2, 5 | Merge minimal V1 consumer versus full stage-5 product distinction |
| B | “Model-faithfulness evidence” · 5104 | superseded by R-0495/R-0508 | 2, 3, 4 | Retire generic legacy-model expansion; preserve explicit abstraction relations where needed |
| B | “Package-level evidence composition” · 5106 | duplicate of R-0482/R-0440 | 7 | Merge |
| B | “Typed PolicyDecision” · 5108 | duplicate of R-0440 | 7 | Merge |
| B | “Axiom and assumption inventories” · 5110 | duplicate of R-0506/R-0509 | 1, 2, 3, 4 | Merge; immediate axiom policy precedes productization |
| B | “Evidence revocation and rollback” · 5112 | duplicate of R-0343/R-0430 | 5, 7 | Merge; retain receipt-store rollback explicitly |
| B | “Signed transport without authority inflation” · 5114 | live | 5, 7 | Merge into R-0353/package transport |
| B | “Schema lifecycle and migration” · 5116 | duplicate of R-0338/R-0361 | 5, 7 | Merge |
| B | “Source-to-VIR bridge audit” · 5120 | superseded by R-0460/R-0495/VP S15 | 2, 3, 4 | Merge distinct source→Core and Core→VC obligations |
| B | “Toolchain-upgrade migration” · 5122 | duplicate of R-0208/R-0429 | 1, 7 | Merge continuing upgrade procedure; do not reopen closed drill |
| B | “Reproducible release artifacts” · 5124 | duplicate of R-0356/R-0357 | 2, 7 | Merge minimal slice retention and release packaging |
| B | “FFI, Unsafe, runtime, backend boundaries” · 5126 | duplicate of R-0505/R-0509 | 2, 3, 4 | Merge |
| B | “Platform qualification” · 5128 | live | 5, 7 | Merge into R-0353/R-0333 acceptance |
| B | “Exact contract witnesses” · 5132 | duplicate of R-0473/R-0474 | 6 | Merge |
| B | “Narrow two-state mutation” · 5134 | live | 6 | **R-0524** (owner) — Turn prose into a bounded task linked to R-0473/74/R-0175 |
| B | “Only afterward … ninth table” · 5135 | live | 6 | **R-0525** (owner) — Turn prose into a task; retarget to Core migration, preserving no-authority controls |
| B | “Checked total functions / decreases” · 5137 | live | 6 | **R-0526** (owner) — Turn prose checkpoint into explicit owned exit; link R-0060 |
| B | “Specification adequacy and vacuity” · 5139 | duplicate of R-0212/R-0213 | 5, 6 | Merge without implying general decidability |
| B | “Relational properties” · 5141 | live | after-1.0/research | **R-0527** (owner) — Turn into scoped research task; separate ordinary refinement already required by VP |
| B | “Closed-world library evolution” · 5142 | live | 4, 7 | **R-0537** (owner) — Turn prose into owned compatibility increment; respect open imported callbacks |
| B | “Parser and decoder fuzzing” · 5145 | live | 3, 5, 7 | Merge per-format coverage into R-0143/R-0353/package codecs |
| B | “Resource limits and denial-of-service” · 5146 | live | 3, 5 | **R-0528** (owner) — Turn prose into cross-command limits task with explicit owners |
| B | “Atomic evidence persistence” · 5148 | live | 5 | **R-0529** (owner) — Turn prose into evidence-store task; do not conflate with R-0144’s cache |
| B | “Gate ownership and reachability” · 5150 | duplicate of R-0481/R-0513 | 1 | Merge |
| B | “Long-running stability and nondeterminism” · 5152 | live | 3, 7 | Merge into R-0149 and release qualification |
| B | “Performance baselines” · 5154 | duplicate of R-0416/R-0515 | 3, 5 | Merge; include proof-engineering budgets from VP |
| B | “ProofCache” · 5156 | duplicate of R-0145/R-0155 | 3, 4, 5 | Merge |
| B | “External-user dependency-upgrade workflow” · 5161 | duplicate of R-0486 | 7 | Merge shared scenario |
| B | “IDE/CI evidence lenses … ergonomics” · 5164 | duplicate of R-0515/R-0119 | 3, 7 | Merge; do not defer basic editor to schema freeze |
| B | “Human-readable evidence explanations” · 5169 | duplicate of R-0182/R-0183 | 5 | Merge |
| B | “Evidence-diff format” · 5171 | duplicate of R-0182/R-0183 | 5 | Merge |
| B | “Independent implementation and security review” · 5173 | duplicate of R-0353/VP S16 | 2, 5 | Merge explicit reviewer and qualification exits |
| B | “Release assurance bundles” · 5174 | duplicate of R-0188/R-0333 | 5, 7 | Merge |
| B | “Privacy and disclosure policy” · 5176 | live | 5, 7 | Merge into R-0361 with concrete export/redaction controls |
| B | “Ecosystem compatibility policy” · 5178 | duplicate of R-0440/R-0429 | 7 | Merge |
| B | “Second proof-producing kernel” · 5181 | duplicate of R-0448 | after-1.0/research | Keep research gate; distinct from con-leche/nanoda rechecking Lean |
| B | “Verified … compiler bridges” · 5183 | superseded by R-0511/R-0512 | 2, 3, 4 | Remove research-only classification |
| B | “Certificate-producing automation” · 5184 | live | 6; after-1.0/research | Merge into existing automation/VIR certificate owners |
| B | H19 · 5192 | duplicate of R-0460 | 2, 3, 4 | Keep explicit semantic-edge obligation |
| B | H20 · 5194 | live | 1, 2, 3, 4, 5 | **R-0538** (owner) — Merge axiom/execution accounting into R-0506/R-0509 |
| B | H21 · 5196 | live | after-1.0/research | Keep trusted label; not a universal blocker for bounded verified slices |
| B | “Philosophical-core gaps” · 5201 | superseded by VP S9/S15/S16 | 2, 3, 4 | Replace blanket verification prohibition with profile/theorem-boundary labels |
| B | “Scheduled critical-path work” · 5206 | historical | 3, 7 | Retire obsolete tooling-after-schema sequence |
| B | “Typestate” · 5210 | live | after-1.0/research | Keep forcing-workload gate |
| B | “Strict post-R-0004 critical path” · 5222–5241 | superseded by top queue | 1, 2, 3, 4, 5, 6, 7 | Retire competing order; preserve compatibility obligations |
| B | Historical “remaining work” · 5311–5408 | historical | 1, 6 | Archive; live table/cache/contract items already separately listed |
| B | Historical slices table · 5440–5465 | historical | 1 | Archive; keep closure record only |
| B | Binder-frame remainder · 5467–5498 | historical | 1 | Archive; transfer invariants to R-0492/R-0495 |
| B | Generator residual lists · 5732–5737, 5797–5809 | historical | 1, 4 | Archive; unsupported specializations stay explicit in migration inventory |
| B | “Positive specialization … blocked” · 5848, 5893 | superseded by R-0510/R-0495 | 4 | Merge admitted-instance proof coverage; no new PExpr expansion |
| B | Classification/receipt “open design question” · 5922–6009 | historical | 1 | Archive R-0004 snapshot |
| B | “What remains … sequencing decision” · 6050 | historical | 1 | Retire stale blocker |
| B | Identity spikes and unmeasured use sites · 6090–6277 | historical | 1 | Archive; current architecture is Resolve identities, not parallel identity experiments |
| B | General binder stack / ConstId remainder · 6404–6411 | historical | 1, 2 | Archive dated snapshot; merge live class controls into R-0492/R-0495 |
| B | Historical closure list/snapshot · 6545–6610 | historical | 1 | Archive; do not reopen from obsolete “NOT STARTED” |
| B | Multi-kernel H25 · 6644 | duplicate of R-0474 | 6 | Merge |
| B | Multi-kernel `old` ceiling · 6648 | duplicate of two-state checkpoint | 6 | **R-0524** (owner) — Merge |
| B | Nonlinear rung 3 · 6652 | duplicate of H21/R-0448 | after-1.0/research | Merge |
| B | Bitvector rung 4 · 6654 | duplicate of H20/R-0506 | 1, 5 | Merge |
| B | Induction/decreases rung 8 · 6655 | duplicate of totality checkpoint | 6 | **R-0526** (owner) — Merge |
| B | Full correctness rung 9 · 6657 | live | after-1.0/research | Retire as a schedulable task; replace only with bounded properties/workloads |
| B | Register A/B remainder · 6674 | duplicate of R-0460 / VP obligations | 2, 3, 4 | Keep exact registers, not aggregate “ladder” progress |
| B | Malformed/unterminated block helper · 6702 | live | 1 | Keep named limitation in R-0476; implement only with a reproducer |
| B | Contract-call logical admission · 6721–6729 | duplicate of R-0473 | 6 | Merge; soundness containment before affected releases |
| B | R-0480 Phase A · 6782–6791 | live | 1 | Keep all four requirements under R-0480 |
| B | R-0480 Phase B · 6793–6802 | live | 1 | Keep; link checking alone does not close authority audit |
| B | R-0480 Phase C · 6804–6814 | done (not yet moved) | 1 | Archive completed move; retain transaction rule |
| B | R-0481 journal · 6932–6948 | live | 3 | Keep one pilot and temporal-authority gates |
| B | R-0481 generated artifacts · 6955–6965 | live | 1, 2, 3, 4, 5 | Keep five artifact families under one producer map |
| B | R-0481 shared assertions · 6982–7001 | duplicate of R-0476/R-0478 | 1 | Merge into R-0481/R-0513 implementation |
| B | R-0481 mutation registry · 7003 | live | 1 | Keep; never count registered-only controls as executed |
| B | R-0481 corpus manifest · 7009–7032 | live | 3 | Keep one denominator and stable IDs |
| B | R-0481 layer firewall · 7034–7074 | superseded by R-0494/VP S9/S16 | 1, 2, 3, 4, 5 | Merge rules; correct Check-before-Core diagram |
| B | R-0481 Evidence ownership · 7076–7095 | live | 2, 3, 4, 5 | Keep incremental moves only after semantic ownership |
| B | R-0481 VIR promotion · 7097–7106 | duplicate of R-0450 | 2, 3, 4, 5 | Merge |
| B | R-0481 sequencing step 5 · 7120 | historical | 1 | Retire completed prerequisite; current owner is R-0482 qualification |
| B | R-0481 standalone consumer · 7128 | duplicate of R-0353 | 2, 5 | Merge |
| B | R-0481 roadmap compaction · 7131 | live | 1 | Keep; this audit supplies concrete input |
| B | R-0482 remaining P1 · 7212 | live | 1 | Keep current queue owner |
| B | R-0482 exact property acceptance · 7293–7314 | superseded by R-0495 | 1, 2 | Merge controls through status/issuance/consumption/replay |
| B | R-0482 total-model admission · 7316 | live | 6 | Merge into R-0473/totality; distinguish behavior models from total contract functions |
| B | R-0482 relation algebra/cardinality · 7277–7291, 7341–7360 | live | 2, 3, 4, 5, 6 | Merge Core ClaimSpec relation checking; retire legacy-only attachment expansion |
| B | R-0482 authoring modes · 7362–7385 | live | 5, 6 | Keep behind S2/S9 proof-surface freeze |
| B | R-0482 differential PFnDef correspondence · 7407 | superseded by R-0491/R-0495 | 1, 2, 3, 4 | Keep maintenance comparisons; retire new legacy correspondence product |
| B | R-0482 exported contract · 7324–7339 | live | 6, 7 | Merge into typed contract and package-interface owners |
| B | R-0482 three dependency strengths · 7415 | live | 5, 6, 7 | Keep semantically justified edges; adapt model edge to Core/abstraction identity |
| B | R-0482 partial evidence linker · 7430–7452 | duplicate of R-0440/package tasks | 7 | Merge all refusal classes |
| B | R-0482 receipt fields · 7454–7472 | live | 2, 5 | Merge canonical task; avoid competing constructors |
| B | R-0482 attacks 1–25 · 7474–7505 | live | 1, 2, 3, 4, 5, 6, 7 | Keep as distributed acceptance matrix; each attack follows its owning increment |
| B | R-0479 remaining vocabulary sweep · 7619–7626 | live | 1 | Keep beyond already-clean status/edge vocabularies |
| B | R-0479 bottom-value exception · 7643 | live | 1 | Keep explicit justified allowlist under R-0494 migration |
| B | R-0476 uncounted shapes · 7752 | live | 1 | Keep four audit classes; malformed-block case remains conditional |
| B | R-0476 central freshness build · 7760 | duplicate of R-0513 | 1 | Merge |
| B | R-0473 operand widths · 7792 | live | 1, 6 | Keep typed-contract requirement |
| B | R-0473 bool arithmetic · 7793 | live | 1, 6 | Keep |
| B | R-0473 result type · 7794 | live | 1, 6 | Keep |
| B | R-0473 loop scope · 7795 | live | 1, 6 | Merge identity/loop-binding controls into R-0492/R-0495 |
| B | R-0473 spec/total/runtime/trusted distinction · 7798 | live | 6 | Keep; capability-free is not total |
| B | R-0473 isolated declarations · 7807, 8281–8290 | live | 3, 6 | Keep bounded diagnostics/contract-analysis mode |
| B | R-0473 embedded Slice-6 work · 7823–8235 | historical | 1 | Archive under R-0004 closure; exact contract witness is the live exception |
| B | R-0473 BindingId steps 1–3 · 8249–8254 | superseded by R-0492 | 2 | Merge; IDs originate at Resolve, not a second contract elaborator |
| B | R-0475 origin-labelled report · 8382–8401 | live | 3 | Keep; derive origin during project assembly |
| B | R-0472 MIN negation · 8417 | live | 2, 3, 4 | Merge into R-0460 and typed arithmetic operation coverage |
| B | R-0472 float→int trap VC · 8418 | live | 4 | Merge into V7/R-0460 |
| B | R-0472 bitwise tier · 8427 | live | 4 | Merge Core ProofKit/bitvector owner; no required extra proof kernel |
| B | R-0472 enum tier · 8434 | live | 4 | Merge V2/Core proof libraries |
| B | R-0472 recursion/decreases · 8444 | live | 6 | **R-0526** (owner) — Merge totality owner; a linear measure VC alone is not full recursion soundness |
| B | R-0472 loop properties · 8453 | live | 4, 5, 6 | Merge exact Core loop binding and contract owners; retire blanket “no invariants” |
| B | R-0472 heap/pointers · 8456 | superseded by VP S7/S11/V3–V6 | 2, 3, 4 | Merge compiler memory model; keep richer logic separately scoped |
| B | R-0472 constant time · 8457 | live | after-1.0/research | Merge relational research; not ordinary safety VCs |
| B | R-0472 print fidelity · 8463 | live | 5; after-1.0/research | Merge R-0450 validation; retire categorical “never proved” claim |
| B | R-0471 query arity · 8549 | live | 3 | Keep class-level CLI test |
| B | R-0471 retired syntax · 8560 | live | 3 | Keep |
| B | R-0471 candidate hints · 8568 | live | 3 | Keep valid-remedy tests |
| B | R-0450 Lean rendering witness · 8715–8726 | live | 2, 3, 4, 5 | Keep within canonical goal/rendering migration |
| B | R-0010 `.audit_me/` · 8831 | live | 1 | Keep bounded inventory; no generated binary promotion |
| B | R-0010 publication state machine · 8843 | duplicate of R-0439/R-0447 | 1 | Merge operational owner; not a compiler bug state |
| B | R-0439 named worktree · 8870–8877 | live | 1 | Keep disposition inventory; never wholesale-merge superseded implementation |
| B | R-0435 systematic rename probe · 8961–8973 | duplicate of R-0500 | 1 | Merge into language migration evidence |
| B | R-0440 raw digest migration · 9007 | live | 1, 2, 3, 4, 5 | Merge identity/artifact owners; preserve compile-fail swaps |
| B | R-0440 authorship modes · 9021 | duplicate of R-0482 modes | 5, 6 | Merge single design after proof-surface freeze |
| B | R-0440 residual ladder prose · 9045 | live | 1, 5 | Keep corpus-wide terminology repair |
| B | R-0447 ownership map · 9477 | duplicate of R-0481 | 1, 2, 3 | Merge unique manifest requirement |
| B | R-0447 dependency ratchet · 9484 | duplicate of R-0481 | 2, 3 | Merge and update pipeline direction |
| B | R-0447 manifest tests/routing/commands · 9492 | live | 1 | Merge into R-0481/R-0513; preserve exact-command parity |
| B | R-0447 corpus/site homes · 9506 | duplicate of R-0481/R-0135 | 3 | Merge; adjudicate conflicting proof/document homes first |
| B | R-0447 diagnostic catalog · 9518 | live | 3 | Merge into R-0137/R-0494 decomposition |
| B | R-0447 artifact hygiene · 9524 | live | 1, 2, 3 | Merge isolation now into R-0513; broader hygiene incremental |
| B | R-0447 publication invariants · 9557 | live | 1 | Merge R-0439 operational acceptance |
| B | R-0444 slice 1 · 9615 | live | 3 | Keep |
| B | R-0444 slice 2 · 9624 | live | 3 | Keep |
| B | R-0444 proof handoff · 9631 | duplicate of R-0170/R-0244 | 4, 5, 6 | Merge, grounded in R-0460/Core semantics |
| B | R-0444 resource handoff · 9650 | duplicate of R-0445 | 3 | Merge |
| B | R-0445 two falsification probes · 9726 | live | 3 | Keep prerequisite experiments |
| B | R-0011 visibility bullets · 9738–9760 | live | 3, 4 | Keep all seven under one construction-rights migration |
| B | R-0012 combinator bullets · 9775–9789 | live | 3, 4 | Keep all five under R-0012 |
| B | R-0013 UTF-8 · 9795 | live | 1, 3 | Keep confirmed correction |
| B | R-0013 C strings · 9800 | live | 1, 3 | Keep complete source inventory |
| B | R-0013 external results · 9806 | live | 1, 3 | Keep concrete witnesses and successful controls |
| B | R-0013 env/args · 9813 | live | 3 | Keep |
| B | R-0013 random bounds · 9818 | live | 1, 3 | Keep documented contract; recoverable variant separately pulled |
| B | F7/F8 · 9825 | live | 1, 3 | Keep repair in R-0013, dynamic validation in R-0030 |
| B | `io.read_all` future reconsideration · 9848 | live, conditional | 3 | Keep trigger; do not misclassify accepted contract as defect |
| B | R-0014 public-safety manifest · 9875 | duplicate of R-0021 | 3 | Merge |
| B | R-0014 bug-046 dereference path · 9886 | live | 1, 2, 3, 4 | Merge checker containment into R-0514 and raw semantics into V6 |
| B | R-0014 raw signature audit · 9899 | live | 3 | Keep with R-0016/R-0021 |
| B | R-0015 legacy discrepancies · 9945 | live | 3 | Keep explicit subcases; confirmed errors remain R-0013 priority |
| B | R-0016 derived capability inference · 9964 | done (not yet moved) | 1 | Retire prerequisite; R-0470 records both paths fixed |
| B | R-0016 escapable slices · 9984 | live | after-1.0/research | Keep excluded from current second-class-reference profile |
| B | R-0017 net · 10017 | live | 3 | Keep |
| B | R-0017 fs/io · 10021 | duplicate of R-0013/R-0015 | 3 | Merge |
| B | R-0017 env · 10024 | duplicate of R-0013 | 3 | Merge |
| B | R-0017 args · 10027 | duplicate of R-0013 | 3 | Merge |
| B | R-0017 time · 10030 | duplicate of R-0038 | 3 | Merge |
| B | R-0017 process · 10033 | duplicate of R-0020.1 | 3 | Merge |
| B | R-0017 rand · 10035 | duplicate of R-0013/R-0036 | 3 | Merge |
| B | BytesRaw split · 10071 | live | 1, 3 | Keep immediate ownership audit under R-0018 |
| B | Collection bullet list · 10094–10109 | duplicate of R-0031 | 3 | Merge every listed requirement |
| B | Naming/coherence ledger · 10111 | live | 3 | Merge into R-0031/R-0066 |
| B | R-0020.1 · 10125 | live | 3 | Keep workload gate; do not recreate removed public fork |
| B | R-0020.2 · 10128 | duplicate of R-0038 | 3 | Merge |
| B | R-0020.3 · 10131 | duplicate of R-0036 | 3 | Merge |
| B | R-0020.4 · 10136 | duplicate of R-0039/R-0040 | 3 | Merge |
| B | R-0020.5 · 10139 | duplicate of R-0039/R-0030 | 3 | Merge |
| B | R-0020.6 · 10141 | duplicate of R-0055 | 3 | Merge |
| B | R-0020.7 · 10143 | live | 3 | Keep under HMAC/std API owner; streaming separately pulled |
| B | R-0020.8 · 10147 | duplicate of R-0047 | 3 | Merge |
| B | R-0020.9 · 10152 | duplicate of R-0012/R-0055 | 3 | Merge; replace undefined “A3” label with R-ID |
| B | R-0020.10 · 10156 | live | 1, 2, 3 | Merge into R-0034/R-0036 and `IntArith` controls |
| B | R-0020.11 · 10162 | live | 3 | Keep five separate demand records; no blanket polish release |
| B | R-0438 temporary missing rows · 10207 | live | 1 | Keep stopgap only until generated inventory replaces it |
| B | R-0438 one-time truth repair · 10248 | live | 1 | Keep; normative interpreter target must now be shared Core semantics |
| B | Phase-7 completion bullets · 10286–10308 | duplicate of listed task owners | 3 | Keep as bounded correction checklist, not full catalog release gate |
| B | Phase-7 trigger exception · 10310 | live | 3 | Keep; replace undefined S2/S4–S7 labels with R-IDs |
| B | Layout panic hygiene · 10325 | superseded by R-0502 | 2, 3 | Merge after layout-owner split |
| B | Bug-039 residual · 10339 | superseded by R-0492 | 1, 2 | Merge; reproduced correctness bugs should not await workload demand |
| B | No public Slice constructor · 10343 | duplicate of R-0016 | 3 | Merge |
| B | Bug-027 rendering · 10349 | duplicate of R-0140 | 3 | Merge measured builder repair |
| B | Fuzzer residuals · 10351 | duplicate of R-0491 | 1, 2, 3, 4 | Merge scope-specific minima; account for unobservable binary trap kinds |
| B | Raw-name classifiers · 10354 | superseded by R-0492/R-0514 | 1, 2 | Merge |
| B | Non-finite float literals · 10358 | live | 3, 4 | Keep bounded diagnostic/emitter repair |
| B | Silent-default sweep · 10365 | live | 1, 2 | Merge into typed artifacts/layout; retain all three controls |
| B | Bug-043 C-string lint · 10372 | duplicate of R-0013 | 1, 3 | Merge source inventory |
| B | Destructor capability fence · 10459 | live | 3, 4 | Keep first-consumer trigger and ownership proof obligations |
| B | R-0031.1a Clone/indexed swap · 10473 | live | after-1.0/research | Keep separate research decisions |
| B | R-0031.1b arenas · 10483 | live | after-1.0/research | Keep before stable arena admission |
| B | R-0031.1c dictionaries · 10498 | live | 3 | Keep before binary collection operations stabilize |
| B | R-0031.1d alias guard · 10512 | live, continuing | 1, 2, 3, 4 | Keep gate; integrate declarative borrow proof |
| B | R-0031.1e returned references · 10516 | live | after-1.0/research | Keep outside V1–V7 until separately specified |
| B | R-0031.1f const generics · 10521 | live | after-1.0/research | Keep; admitted feature must enter maintained V7 inventory/versioned scope |
| B | R-0031.1g allocator values · 10531 | live | after-1.0/research | Keep before allocator-heavy stabilization |
| B | R-0031.1h behavioral laws · 10544 | live | 3, 6 | Keep R-0031 API example and R-0271 proof meaning |
| B | R-0060 rational theory · 10761 | live | after-1.0/research | Keep separately workload-pulled |
| B | R-0065 deferred families · 10803 | live | after-1.0/research | Keep every family explicitly excluded until admitted; do not infer completion |
| B | R-0483 “remaining … owner-bound results” · 10967 | done (not yet moved) | 3 | Retire; BoundView closure explicitly resolves it |
| B | R-0483 failed migration/investigation · 10974–11111 | historical | 1, 3 | Archive; retain accepted coordinate limit at 10907 |
| B | R-0484 descriptor second step · 11147, 11304–11306 | live | 3, 4 | **R-0530** (owner) — Turn into separately identifiable task after closed R-0484 |
| B | R-0484 dependency/query limit · 11142 | live, accepted limit | 3 | Keep limitation; expansion requires a scoped task, not reopening closure |
| B | R-0484 bug-077 containment · 11145 | live | 3 | Keep bug owner and valid-program restoration separately from containment |
| B | R-0484 old indirect opacity analysis · 11151 | live | 2, 3 | Merge retirement into R-0514/R-0494 after parity |
| B | R-0484 campaign “not qualified” · 11154 | historical | 1 | Archive chronology; retain exact later qualifying SHA, not inherited release qualification |
| B | R10 remaining bullets · 11185–11196 | done (not yet moved) | 1 | Retire stale open wording; closure at 11130 covers them |
| B | Proof-admission consolidation/docs audit · 11216–11229 | done (not yet moved) | 1 | Archive under recorded closure |
| B | F7/F8 follow-up · 11347 | duplicate of R-0013/R-0030 | 1, 3 | Merge |
| B | Historical “still open” · 11396 | historical | 1 | Archive |
| B | Historical bug-071/classify-82 remainder · 11533–11552 | historical | 1 | Archive under R-0484 closure |
| B | Historical dependency-opacity remainder · 11599–11619 | historical | 1 | Archive; current accepted coverage limits remain explicit |
| B | Historical “next transition” · 11666 | historical | 1 | Archive |
| B | R-0488.1 mutation audit · 11766 | live | 1, 3 | Keep; merge discovered violations into immediate repairs |
| B | R-0488.2 library pressure · 11774 | live | 3 | Keep bounded workload |
| B | R-0488 partial-progress failure · 11781 | live | 3, 6 | Keep controlled failure and no-duplicate-retry example |
| B | R-0488.3 footprints · 11789 | duplicate of R-0175/R-0473/74 | 6 | Merge |
| B | R-0488.4 upgrade diffs · 11796 | duplicate of R-0182/83/84/R-0372 | 5, 6, 7 | Merge |
| B | R-0488.5 failure explanations · 11802 | duplicate of R-0137/R-0466 | 3 | Merge |
| B | QBE phase goal/done · 11817–11834 | superseded by VP V1–V7/top queue | 2, 3, 4 | Retire mandatory QBE prerequisite; retain optional research scope |
| B | QBE product contract bullets · 11838–11852 | duplicate of R-0074/75/80/83/110 | 3; after-1.0/research | Merge general requirements; QBE-specific work conditional |
| B | Preflight order · 11862 | superseded by VP slice delivery | 2 | Retire competing backend sequence |
| B | Zig correspondence / “defer target MIR” · 11873–11888 | superseded by R-0505 | 2, 3, 4 | Retire; owned Machine IR/encoding now required |
| B | Thin executable slices · 11890 | duplicate of R-0511/R-0512 | 2, 3, 4 | Merge delivery discipline |
| B | `main -> 42` milestone · 11900 | superseded by R-0511 V1 | 2 | Merge useful smoke test, not replacement for V1 theorem exit |
| B | Expansion ladder · 11907–11911 | superseded by VP V1–V7 | 2, 3, 4 | Map families to admitted slices; no monolithic scalar scope |
| B | LLVM byte-parity rule · 11913 | live | 2, 3, 4 | Keep as migration check, supplemented by compatibility/theorem requirements |
| B | QBE “first three slices” kill criterion · 12243 | live | after-1.0/research | Keep if experiment retained; transfer general checkpoint discipline |
| B | Phase-8 three-role contract · 12253 | duplicate of R-0119 | 7 | Merge |
| B | Architecture “after QBE” · 12271 | superseded by R-0492/R-0497/R-0514 | 1, 2 | Retire dependency outright |
| B | HMAC role · 12352 | duplicate of R-0119/R-0124 | 3, 4, 5, 6, 7 | Keep |
| B | Secure-update role · 12353 | duplicate of R-0119/R-0120 | 3, 4, 5, 6, 7 | Keep all listed properties with bounded theorem scope |
| B | Protocol role · 12358 | duplicate of R-0119/R-0121 | 3, 4, 5, 6, 7 | Keep |
| B | Demote four old flagships · 12362 | live | 3 | Merge corpus disposition into R-0135/R-0481 |
| B | Flagship graduation sequencing · 12367 | live | 7 | Keep public graduation gate; permit stage-3 workload implementation |
| B | Main-repo workload tier · 12492 | duplicate of R-0134/R-0481 | 3 | Merge corpus classification |
| B | Medium tier · 12497 | live | 3 | Keep selection gate; do not instantiate all candidates |
| B | Separate workload repository · 12520 | live | 3 | Keep conditional setup; reconcile R-0481’s “no second repository” wording |
| B | Compatibility-port candidates · 12534 | live | 3; after-1.0/research | Keep named triggers; no automatic acceptance of all candidates |
| B | Stress-port tier · 12542 | live | 3 | Merge priority with R-0490; avoid parallel arbitrary large-port commitment |
| B | Performance pull triggers · 12579 | duplicate of R-0416/phase 15.75 | 3 | Keep |
| B | Example-refresh three candidates · 12598 | live | 3 | Keep three explicit judgment items under R-0135 |
| B | Human trial · 12632 | live | 3 | Keep with R-0515 recurring pilots |
| B | “Primary trial is model” amendment · 12642 | superseded by R-0515 | 3 | Retire priority claim; retain complementary R-0466 measurement |
| B | Minimum diagnostic bar · 12658 | live | 3 | Keep four classes with spans and valid-edit controls |
| B | R-0445 human session · 12688 | live | 3 | Keep accounting rule |
| B | Conditional synthesis probe · 12692 | live | 3; after-1.0/research | Keep bounded trigger; remove its authority over editor delivery |
| B | Capability-firewall workload · 12700 | live | 3, 4 | Merge into R-0443 forcing workload |
| B | WAL store workload · 12702 | live | 3, 6 | Keep candidate; requires explicit storage/crash assumptions |
| B | Regex pair · 12704 | live | 3, 4, 5, 6 | Merge R-0244 complexity workload |
| B | Editor buffer workload · 12706 | live | 3, 4 | Merge R-0006/V3–V4 validation |
| B | Job-runner workload · 12708 | live | 3 | Keep only when this surface needs pressure |
| B | DNS pointer-chasing · 12709 | duplicate of R-0244/R-0444 | 3, 4, 5, 6 | Merge |
| B | Incremental GO trigger · 12719 | superseded by stage-3/R-0515 commitment | 3 | Keep measurement prerequisites; replace late all-or-nothing GO gate |
| B | “Roadmap remains linear” · 12730 | superseded by top queue | 3 | Retire phase-number scheduling rule |
| B | Engine before proof cache · 12737 | live | 3 | Keep dependency for cache activation, not for proof development |
| B | No pre-GO session seam · 12757 | superseded by R-0515/R-0385 | 3 | Replace with bounded editor-enabling session checkpoint |
| B | Incremental equivalence exit · 12750 | duplicate of R-0149 | 3 | Merge |
| B | R-0139 seven query families · 12813–12823 | live | 3 | Keep all families; update artifact chain to current checked IRs |
| B | Future BackendIR migration · 12828, 13008 | superseded by R-0503/VP V1 | 2, 3 | Retire old phase assumption; support actual profile-specific artifacts |
| B | Local-cache threat model · 12951–12961 | live, accepted boundary | 3 | Keep explicit limitation; verified profile needs authentic expected roots/revalidation |
| B | Proof-cache authority · 12977–12990 | live | 3, 4, 5 | Keep; one Verdict authority decides current status |
| B | Native/object cache limit · 13024–13027 | live | 3, 4 | Keep LLVM label; add native certificate/image correspondence before verified reuse |
| B | Parallel/cancellation rule · 13033–13042 | live | 3 | Keep |
| B | Validation-artifact placement · 13061 | duplicate of R-0481 corpus policy | 3 | Merge into tests/workload home |
| B | 20×/5× targets · 13084–13090 | live | 3 | Keep measured baselines/count bounds; do not promote numeric targets to gates |
| C | Phase 9 “proof-cost probe”, 13100–13103 | duplicate of R-0150 | 3 | merge into R-0150 |
| C | “First shared consumer: R-0486”, 13111–13117 | live | 3, 5 | keep under R-0150 |
| C | “cost” / “coverage”, 13121–13128 | live | 3, 4 | keep both in R-0150 |
| C | “Publish a small table…retain comparable history”, 13133–13136 | live | 3, 5 | keep under R-0150/R-0438 |
| C | “Keep registered spec PExprs…until…split”, 13155–13156 | superseded by R-0512 | 4 | retire split dependency; retain compatibility during migration |
| C | “when…features land, update this guide”, 13213–13216 | live | 4, 6, 7 | keep as R-0157 exit |
| C | “fixture…replay command”, 13257–13262 | duplicate of R-0191 | 3, 4 | merge persistence into R-0191 |
| C | Synthesis “replay bundle”, 13286–13290 | live | after-1.0/research | keep under R-0167 |
| C | Synthesis gates/red-team fixtures, 13292–13299 | live | after-1.0/research | keep under R-0167; reuse R-0494/R-0213 |
| C | “Agent discoverability”, 13301–13310 | live | 3, 4 | merge into existing Phase 6E catalog owner; do not wait for synthesis |
| C | Cache activation/one-store conditions, 13315–13331 | live | after-1.0/research | keep under R-0168 |
| C | R-0170 “completed forcing probe”, 13350–13351 | historical | 4 | retire from open-work narrative |
| C | “active V1 gaps”, 13351–13354 | live | 4 | keep as explicit R-0170 increments |
| C | “Defer whole SHA…heap…induction”, 13360–13363 | live | after-1.0/research | keep as deferred scope |
| C | “Loop-bound…named discharge class”, 13365–13368 | duplicate of R-0244 | 6 | merge into R-0244 acceptance |
| C | “first reuse-economics customer”, 13381–13387 | live | 4, 6 | keep R-0170/R-0444 joint exit |
| C | Formal-container “end-to-end refinement slice”, 13408–13415 | live | 6 | keep under R-0172 |
| C | Frame inference “before…second update shape”, 13454–13462 | live | 6 | keep; separate early semantic frame proofs |
| C | Phase 10 “three…flagships and one package”, 13505–13506 | duplicate of R-0201 | 5, 7 | merge into R-0201 |
| C | “Absorb…R-0445…resource-fact schema”, 13512–13516 | live | 5 | keep under R-0178 |
| C | “direct…transitively reachable trap kinds”, 13518–13523 | live | 4, 5 | keep under R-0178 |
| C | “no second truth source”, 13525–13538 | superseded by R-0494 | 1, 5 | merge authority checks into R-0494 |
| C | Diff “First increment”, 13561–13569 | live | 5 | keep under R-0182 |
| C | Diff gate “First increment”, 13589–13594 | live | 5 | keep under R-0183 |
| C | “Selected-function equivalence follow-on”, 13596–13602 | live | 6 | **R-0531** (owner) — turn prose into a separately bounded task |
| C | “canonical reachability witness”, 13619–13626 | live | 3, 4 | keep under R-0184 |
| C | R-0441 “list repair is complete”, 13663–13664 | done-but-not-moved | 3 | retire completed half |
| C | R-0441 “Decide one rule and gate it”, 13655–13671 | live | 3, 7 | keep under R-0441 |
| C | Oracle manifest/buckets/report/failure persistence, 13713–13719 | live | 3, 5 | keep under R-0189 |
| C | Oracle cross-check/metamorphic/weakening diff, 13720–13723 | live | 3, 5 | keep optional cross-check distinct from required diff |
| C | Phase 11 “FnTable-complete”, 13816–13819 | superseded by R-0495/R-0512 | 2, 4 | retire FnTable-based phase exit |
| C | Checker-advisory register, 13849–13855 | done-but-not-moved | 1 | move with R-0208 closure |
| C | Lean 4.28 fire drill and seven acceptance bullets, 13857–13879 | done-but-not-moved | 1 | move with R-0208 closure |
| C | “nothing implemented” scope and boundary bullets, 13881–13904 | historical | 1 | retire stale prospective wording |
| C | Five “Design decisions”, 13911–13930 | historical | 1 | retain concise decisions, move implementation diary |
| C | “decision 1…open…decisions 3–4…open”, 13946–13948 | historical | 1 | retire; later paragraphs close them |
| C | Identity limits, 13948–13950 | live | 2, 5 | merge explicit limits into R-0509; task any promised reduction |
| C | Native-trust policy question, 13966–13968 | historical | 1 | retire question; decision at 13995–13997 answers it |
| C | Trusted core modules / nonce limits, 13972–13973 | live | 2, 5 | keep in R-0509 assumption inventory |
| C | Limited policy rollback, 13986–13989 | live | 5, 7 | **R-0532** (owner) — turn prose into threat-model-specific task or accepted limitation |
| C | “Open, and owned by the user”, 13990–13993 | historical | 1 | retire; choices recorded at 13995–13998 |
| C | “Linux stays unaccepted”, 14012–14014 | historical | 1 | retire; 14023–14032 records closure |
| C | “R-0208 closes only when…”, 14022 | done-but-not-moved | 1 | move with closure evidence |
| C | “native-trust receipts (withheld)”, 14033 | live | 5 | **R-0532** (owner) — turn prose into task linked to R-0506/R-0440 |
| C | “limited rollback protection”, 14033 | duplicate of 13986–13989 | 5, 7 | **R-0532** (owner) — merge into that residual |
| C | “correlated not authenticated…self-report”, 14033–14034 | live | 2, 5 | merge into R-0509/R-0353 |
| C | “Fragment-scoped narrowing”, 14036–14038 | live | 5; after-1.0/research | **R-0532** (owner) — turn prose into task; broad refusal remains safe fallback |
| C | “trusted-boundary shrinker”, 14057–14059 | live | after-1.0/research | **R-0533** (owner) — turn prose into optional task |
| C | SAT microsat→MiniSat staging, 14093–14112 | live | after-1.0/research | keep under R-0451 |
| C | “axiom-inventory…clean checkout…cache” bullets, 14147–14165 | live | 1, 2 | merge axiom work into R-0506; retain replay/cache controls |
| C | R-0458 “Partially landed”, 14187–14195 | live | 5 | keep only the stated remainder |
| C | R-0458 “keep” / “attest”, 14197–14201 | live | 5; after-1.0/research | merge driver mechanics into R-0455 |
| C | Phase 12 “subset family…public names”, 14215–14217 | superseded by R-0511/R-0512 | 2, 4 | merge into slice admission manifests |
| C | Float witness “epsilon-bound…later”, 14337–14339 | live | after-1.0/research | **R-0534** (owner) — turn prose into deferred task |
| C | R-0459 “Exhaustiveness”, 14388–14400 | live | 4, 6 | keep; separate language match checking from user protocol completeness |
| C | R-0459 “Relational (2-safety)”, 14401–14406 | live | after-1.0/research | **R-0527** (owner) — turn prose into distinct task |
| C | Bitvector xor/and/or/shift prerequisite, 14408–14412 | duplicate of R-0450 | 2, 4 | merge into R-0450 |
| C | “Also add `proved_by_hand`”, 14414–14416 | live | after-1.0/research | **R-0535** (owner) — turn prose into policy task; no kernel-proof authority |
| C | Phase 13 “absence of runtime errors”, 14423–14433 | live | 2, 4 | keep; clarify runtime traps do not prove absence of errors |
| C | R-0244 auto-discharge/call-graph composition, 14460–14468 | live | 6 | keep under R-0244 |
| C | DNS compression-pointer forcing case, 14470–14474 | live | 6 | keep under R-0244/R-0444 |
| C | “7a. Implement explicit enum discriminant values”, 14490–14504 | live | 3, 4 | **R-0536** (owner) — turn prose into a task |
| C | Inflate then conditional deflate, 14579–14583 | live | 6; after-1.0/research | keep separate bounded increments under R-0452 |
| C | Inflate proof-ratio and trusted-island conditions, 14594–14608 | live | 6 | keep; specify a measurable pass criterion |
| C | “Closes H23, which is live”, 14616–14621 | historical | 1 | retire false current-status wording |
| C | Hypothesis origins/status cap/gate, 14633–14668 | done-but-not-moved for H23 | 1 | archive fix; retain regression controls |
| C | “Widen beyond loops”, 14670–14671 | live | 1, 2, 4 | merge into R-0494/R-0460 |
| C | R-0462 “continuous check…missing”, 14678–14684 | done-but-not-moved for initial harness | 1 | retire “missing”; expand through R-0491 |
| C | R-0463 extraction probe/kill criterion, 14717–14732 | live | after-1.0/research | keep |
| C | H24 div/shift defects and register repair, 14746–14768 | done-but-not-moved | 1 | archive; preserve totality/injectivity controls |
| C | Delete stale-hypothesis analyses via SSA, 14770–14800 | superseded by R-0492/R-0495/R-0460 | 2, 4 | merge goal; retire blanket SSA/deletion prescription |
| C | “every syntactic analysis…total…unsafe default”, 14802–14805 | live | 1, 2 | merge into R-0506/R-0498 |
| C | R-0450/R-0454/SSA sequencing bullets, 14816–14829 | superseded by R-0495/R-0512 | 2, 4 | replace with Core migration dependencies |
| C | R-0465 assumption-union/adapter pieces, 14847–14860 | done-but-not-moved | 1 | archive with existing gate |
| C | Closed/refused exclusivity, 14866–14872 | done-but-not-moved | 1 | archive with `KernelCell` gate |
| C | Three consumers/recomputed release verdict, 14873–14880 | superseded by R-0494 | 1 | merge remaining universal-authority requirement |
| C | R-0467 three scheduling options, 14902–14911 | done-but-not-moved | 1 | retire alternatives; main-push option implemented |
| C | H22 nightly closure condition, 14927–14931 | historical | 1 | retire obsolete closure concern; preserve scheduling fix |
| C | “Nobody knows…approximately never”, 14932–14935 | historical | 1 | retire stale claim; later qualified campaigns exist |
| C | Repository pin / merge coverage, 14937–14939 | live | 1 | merge into R-0513 |
| C | R-0460 “four rows…Zero”, 14946–14950 | live, stale count | 2, 4 | keep; update to five rows/four partial proofs |
| C | “Sketch both [bounds/div]”, 14957–14962 | historical | 2 | retire; both semantic halves already exist |
| C | Phase 14 Core→ProofCore goal/exit, 14982–14997 | superseded by R-0511/R-0512 | 2, 4 | replace with cumulative Core/native proof exits |
| C | “compiler…out of the trusted base”, 14999–15015 | superseded by R-0508/R-0509 | 2, 4 | merge; remove unconditional execution-TCB claim |
| C | R-0261 parenthesized old objective, 15025 | historical | 2 | retire explicitly superseded objective |
| C | Generic behavioral-law acceptance, 15075–15084 | live | 4, 6 | keep under R-0510/R-0271 acceptance |
| C | Differential shape/operator expansion, 15090–15097 | live | 1, 4 | merge into R-0491 |
| C | Rotating seeds/minimization, 15098–15099 | live | 1, 4 | merge into R-0491/R-0513 |
| C | Dead-path reference-return extra load, 15100–15104 | superseded by S8 reference prohibition | 2 | retire feature repair; retain rejection and raw-pointer positive controls |
| C | Metamorphic alpha/hostile-name oracle, 15105–15117 | live | 1, 2 | merge into R-0491/R-0492 |
| C | Semantic matrix, 15122–15125 | duplicate of R-0491/R-0498 | 1, 4 | merge |
| C | Primitive boundary tests, 15126–15128 | duplicate of R-0491 | 1, 4 | merge |
| C | Std authority/allocation audit, 15129–15132 | duplicate of R-0509 | 3, 4 | merge |
| C | Proof/runtime consistency fixtures, 15133–15136 | duplicate of R-0491 | 1, 4 | merge |
| C | “old-assumption grep”, 15137–15141 | duplicate of R-0498 | 1 | merge; use maintained manifest, not only keyword matching |
| C | R-0274 old authority/boundary chain, 15160–15206 | superseded by R-0499/R-0514 | 2 | retire architecture, retain typed/payload-bound invariants |
| C | Certificate predicate/proof levels/negative gates, 15224–15282 | live | 2, 4, 5 | keep under R-0275 |
| C | R-0443 “Falsification probe first”, 15326–15329 | live | 4 | keep as R-0443 entry criterion |
| C | R-0454 “No subjectDigest…cost zero”, 15353–15358 | historical | 2, 5 | retire stale timing claim; use R-0482/R-0512 inventory |
| C | R-0454 three encoding invariants, 15366–15373 | live | 2, 5 | merge into R-0450/R-0492 canonicalization controls |
| C | “AST→SSA…digest-neutral”, 15375–15381 | live requirement, invalid justification | 2, 4 | replace with explicit versioned proposition migration |
| C | R-0455 dropped ops/symbols/driver cloning, 15397–15420 | live, partly implemented | 2, 4 | merge remaining IR work into R-0450; archive landed factoring |
| C | Register B transform proofs, 15422–15430 | live | 2, 4 | keep under R-0455 |
| C | “Collapse N printers”, 15466–15469 | live | after-1.0/research | keep workload/coverage-gated |
| C | “Keep…print validation…instance count”, 15470–15471 | live | 1, 2, 4 | keep under R-0455/R-0513 |
| C | “deleting the validation layer”, 15479–15483 | historical | 2, 4 | retire contradictory paragraph |
| C | R-0456 “meta-theory…or defer”, 15512–15516 | live | after-1.0/research | keep as research decision, targeting Core |
| C | R-0456 adversarial conformance/dependencies, 15518–15522 | live | after-1.0/research | keep only if renewed Core proposal passes probe |
| D | “Done when: SSA…” · 15529–15532 | Superseded by S10–S15 | 2, 4 | Merge into R-0301 with profile-specific boundaries |
| D | “Trigger the production half” · 15558–15571 | Live | 3 | Keep under R-0281, workload-triggered |
| D | “No implementation…until a forcing workload” · 15592–15598 | Live | after-1.0/research | Keep under R-0284; do not defer basic memory alignment |
| D | “First executable slice” 1 · 15624–15628 | Live | 3 | Keep under R-0285/R-0364/R-0365 |
| D | Same list, 2 · 15629–15633 | Live | 3 | Keep under R-0285 |
| D | Same list, 3 · 15634–15637 | Live | 3 | Keep under R-0285 |
| D | Same list, 4 · 15638–15643 | Live | 3 | Keep under R-0285; share pilot records |
| D | “Automate a fresh-consumer CI case” · 15645–15651 | Live | 3 | Keep under R-0285; reuse in R-0352/R-0354 |
| D | “Report the scope as each slice becomes usable” · 15653–15655 | Live | 3 | Keep under R-0285 |
| D | “Failure and resource contract” · 15657–15679 | Live | 3, 6 | Keep under R-0285/R-0488 |
| D | “Reuse supported upstream…annotations” · 15683–15686 | Live | 3 | Keep under R-0285 |
| D | “Store missing facts…external metadata” · 15687–15691 | Live | 3 | Keep under R-0285 |
| D | “Generate only the selected public API” · 15692–15695 | Live | 3 | Keep under R-0285 |
| D | “Validate generated ABI claims” · 15696–15698 | Live | 3 | Keep under R-0283/R-0287 |
| D | “Diagnostics identify the exact missing fact” · 15699–15703 | Live | 3 | Keep under R-0285 |
| D | “Implement the metadata subset needed” · 15705–15708 | Live | 3 | Keep under R-0285 |
| D | “Ease-of-use acceptance” · 15710–15738 | Live | 3 | Keep under R-0285 |
| D | “Binding contract: C headers” · 15742–15747 | Live | 3 | Keep under R-0285 |
| D | “A narrow typed Rust bridge” · 15748–15752 | Live | 3 | Keep under R-0285 |
| D | “Zig packages…” · 15753–15759 | Live | 3 | Keep under R-0285 |
| D | “Generated safe APIs…” · 15760–15764 | Live | 3 | Keep under R-0285 |
| D | “Start the shared ABI subset…” · 15765–15768 | Live | 3 | Keep under R-0285/R-0287 |
| D | “Networking adapters…” · 15769–15772 | Live | 3 | Keep; verified admission waits for V6 environment contracts |
| D | “Generated headers…round-trip” · 15773–15778 | Live | 3 | Keep under R-0287 |
| D | “Integrate transitive libraries…” · 15779–15784 | Live | 3 | Keep under R-0364/R-0365 |
| D | “Acceptance workload” · 15786–15800 | Live | 3 | Keep under R-0285 |
| D | “Each language/library gap…” · 15828–15835 | Live | 3 | Keep under R-0490/R-0515 |
| D | “Full functional proofs are a subsequent increment” · 15843–15846 | Live | 4, 6 | Turn prose into bounded R-0490 increments with owners/exits |
| D | “Silver is a later, separate systems pressure test” · 15847–15848 | Live | after-1.0/research | Turn prose into a deferred workload record under R-0415 |
| D | “R-18 – R-20 relabelled shape-only” · 15995 | Live | 1 | Keep under R-0498 |
| D | “historical 092/095…rejection fixtures” · 16028–16031 | Live | 1, 2 | Keep under R-0499/R-0491 |
| D | “minimal independently checked…V1 claim” · 16203–16205 | Live | 2 | Keep under R-0509/R-0353 |
| D | “legacy implementation…maintenance only” · 16271–16287 | Live | 1, 4 | Keep under R-0512 |
| D | “Before the first pilot” · 16363–16366 | Live | 3 | Merge into R-0346/R-0347; R-0515 coordinates |
| D | “Now…External pilot” · 16367–16372 | Live | 3 | Keep under R-0515 |
| D | “Diagnostics quality bar” · 16373–16379 | Live | 3 | Keep under R-0515 and diagnostic owners |
| D | “V1 tooling checkpoints” · 16380–16386 | Live | 3 | Merge into R-0385–R-0388/R-0281/R-0355 |
| D | “Before public release” · 16387–16390 | Live | 3 | Merge into R-0358/R-0359; retain R-0515 coverage inventory |
| D | “Expand std and the tutorial into a book” · 16391–16392 | Live | after-1.0/research | Keep under R-0515/R-0347 |
| D | “dated decision…deferral…reopening trigger” · 16393–16394 | Live | 3; after-1.0/research | Keep under R-0399/R-0515 |
| D | “Exit (per checkpoint)” · 16396–16410 | Live | 3 | Keep under R-0515; checkpoints do not close whole owner tasks |
| D | “Nightly builds publish automatically” · 16419–16428 | Live | 3 | Keep under R-0516 |
| D | “V1 deliverables: export annotation” · 16442–16443 | Live | 3 | Keep under R-0286 |
| D | “staticlib and/or sharedlib” · 16444–16445 | Live | 3 | Keep static library first; dynamic verified support remains excluded |
| D | “generated C header” · 16446 | Live | 3 | Keep under R-0286 |
| D | “exported-function ABI classification” · 16447–16449 | Live | 3 | Keep under R-0286/R-0287 |
| D | “generated boundary docs” · 16450–16452 | Live | 3 | Keep under R-0286 |
| D | “evidence bundle next to the library” · 16453–16454 | Live | 3, 5 | Keep versioned experimental evidence before freeze |
| D | “V1 should cover a narrow subset” · 16478–16486 | Superseded by R-0503 | 2, 4 | Merge tooling into R-0503; replace parity-only retirement criterion |
| D | “Small owned backend investigation” · 16543–16558 | Superseded in delivery scope by R-0505 | 2; after-1.0/research | Merge native delivery into R-0505; research into R-0413 |
| D | “Actual emitted LLVM increment” · 16593–16613 | Live | 2, 4 | Keep under R-0299 |
| D | “V1 admission is whole-function” · 16635–16642 | Live | 2, 4 | Keep under R-0300 |
| D | “independent V1 relation check stops…” · 16644–16662 | Live for legacy LLVM relation | 2, 4 | Keep scoped to that relation; not universal native boundary |
| D | “Emitted-LLVM replay increment” · 16664–16683 | Live | 2, 4, 5 | Keep under R-0300 |
| D | “20a. LLVM bitcode emitter” · 16704–16728 | Live | after-1.0/research | Keep gated; remove automatic freestanding/no-clang necessity |
| D | “21a. Guaranteed tail-call optimization” · 16733–16739 | Live | after-1.0/research | Turn prose into task if pulled; require cleanup/resource/refinement obligations |
| D | “21b. Bit-level data layout” · 16740–16745 | Live | after-1.0/research | Turn prose into task if pulled; connect R-0502 certificates |
| D | “SOA containers and std.bigint” · 16745–16746 | Live | after-1.0/research | Turn prose into bounded stdlib tasks when activated |
| D | “preferred backend after QBE” · 16754–16756 | Superseded by native-profile plan | after-1.0/research | Retire mandatory ordering |
| D | Wasm entrance: “QBE’s documented subset” · 16764–16766 | Superseded by R-0503/R-0505 | after-1.0/research | Retire QBE prerequisite |
| D | Wasm entrance: “ValidatedBackendIR” · 16767–16769 | Superseded by R-0503 | after-1.0/research | Merge as validated-BIR prerequisite |
| D | Wasm entrance: “named…workload” · 16770–16771 | Live | after-1.0/research | Keep |
| D | Wasm entrance: “preferable to Cranelift” · 16772–16774 | Live | after-1.0/research | Keep decision record; no implied commitment to either |
| D | “Browser…component model…GC…Wasm64…threads…SIMD…exceptions” · 16780 | Live | after-1.0/research | Keep separate admission entries under R-0305 |
| D | Wasm “Done when” · 16810–16816 | Live | after-1.0/research | Keep; remove mandatory four-way/QBE coverage |
| D | Unsafe “Done when” · 16827–16830 | Live | 4 | Merge into R-0321 acceptance |
| D | Taxonomy: “safe references…second-class” · 16875 | Live | 1, 2 | Merge normative rule into R-0500/R-0499 |
| D | Taxonomy: distinct pointer/slice shapes · 16876–16877 | Live | 4 | Keep under R-0312 |
| D | Taxonomy: “nullable pointers…explicit” · 16878 | Live | 4 | Keep under R-0312 |
| D | Taxonomy: “mutability, provenance…alignment…bounds” · 16879–16887 | Live | 2, 4 | Keep under R-0312/R-0502 |
| D | “sanitizer opt-out attributes” · 16905–16907 | Live | 4 | Keep under R-0315 |
| D | “closure or upvalue-like indirection” · 16930–16935 | Live | 4 | Keep under R-0318 with explicit non-language-feature scope |
| D | Performance “Trigger” · 16968–16972 | Live | 3 | Keep; permit R-0515’s earlier bounded baselines |
| D | Performance “Done when” · 16974–16978 | Live | 3 | Merge into R-0424 |
| D | “complexity_guarded may carry…” · 16992–16999 | Live | 3, 6 | Keep under R-0417; no duplicate cost semantics |
| D | “correlation experiment first” · 17001–17005 | Live | after-1.0/research | Keep conditional under R-0417 |
| D | Freestanding prerequisite/trigger · 17101–17107 | Live | after-1.0/research | Keep; express prerequisites as task exits |
| D | Freestanding “Done when” · 17109–17111 | Live | after-1.0/research | Merge into R-0332 |
| D | littlefs “Definition of done” · 17181–17187 | Live | after-1.0/research | Keep under R-0453 |
| D | littlefs “Claim discipline” · 17189–17194 | Live | after-1.0/research | Keep under R-0453; do not imply relational proof |
| D | Phase 17 “under ten minutes” · 17201–17203 | Live | 3, 5, 7 | Merge measured scoped exit into R-0333/R-0346 |
| D | Alpha: “At least one non-author…” · 17217–17219 | Live | 7 | Keep under R-0333; early pilots do not substitute |
| D | Alpha: “Diagnostics teach…” · 17220–17225 | Live | 3, 7 | Merge into R-0515 plus alpha acceptance |
| D | Alpha: “Source contracts are primary…” · 17226–17227 | Live | 6, 7 | Keep under R-0333; reconcile 0.9 scheduling |
| D | Alpha: “concrete prove guides…” · 17228–17229 | Live | 6, 7 | Keep explicit alpha owner mapping |
| D | Alpha: “every public claim class” · 17230–17232 | Live | 3, 5, 7 | Merge into R-0334/R-0336 |
| D | Alpha: “Core language surface blockers…” · 17233–17235 | Live | 3, 4 | Keep per-feature dispositions under R-0333 |
| D | Alpha: “Runtime safety obligations…” · 17236–17237 | Live | 1, 2, 4 | Merge into Core semantics and public claim matrix |
| D | Alpha: “README…agree” · 17238–17239 | Duplicate of R-0337 | 3, 7 | Merge into R-0337 candidate checks |
| D | Beta: “fmt…test…Concrete.toml” · 17243–17244 | Live | 3 | Keep under R-0333 with actual owner IDs |
| D | Beta: “minimal local package/evidence foundation” · 17245–17248 | Live | 3, 5, 7 | Keep; clarify which schema freeze requires which prototype |
| D | Beta: “minimal editor path” · 17249–17251 | Live | 3 | Merge into R-0515/R-0385–R-0388 |
| D | Beta: “proof-registry.json…remain absent” · 17252–17253 | Historical | 1, 4 | Retire as open work; retain permanent negative regression |
| D | Beta: “Release bundles have stable schemas…” · 17254–17255 | Live | 5 | Merge into R-0353/R-0338 |
| D | Beta: “installed independent…verify-bundle” · 17256–17259 | Live | 5 | Keep R-0353/R-0354 acceptance |
| D | Beta: “verified-profile policy gate” · 17260–17262 | Live | 5, 7 | Merge into R-0341/R-0440 |
| D | Beta: “stdlib/runtime boundary…daily examples” · 17263 | Live | 3 | Merge into R-0292/R-0515 |
| D | Beta: “external user…without…intervention” · 17264–17265 | Live | 7 | Keep separate from assisted quarterly pilots |
| D | “9a. Publish SECURITY.md” · 17377–17390 | Live | 3 | Keep under R-0343 before preview |
| D | “9b. proof-revocation procedure” · 17391–17404 | Live | 2, 5 | Keep; preserve historical replay separately |
| D | “9c. upstream checker/kernel…advisories” · 17405–17413 | Live | 1, 2, 5 | Keep under R-0343/R-0353 |
| D | “Required cleanup” interpolation blockers · 17438–17443 | Live | 3 | Merge into R-0349; decision itself is historical |
| D | Verifier “concrete-cert-core” · 17483–17486 | Live | 2, 5 | Keep under R-0353 |
| D | Verifier “concrete-cert” · 17487–17488 | Live | 2, 5 | Keep under R-0353 |
| D | Verifier “kernel adapters” · 17489–17490 | Live | 2, 5 | Keep; do not make every prover a V1 prerequisite |
| D | “Envelope/accounting” · 17497–17502 | Live | 2, 5 | Keep under R-0353 |
| D | “Certificate profiles” · 17503–17507 | Live | 2, 5 | Keep under R-0353 |
| D | “Translation assurance” · 17508–17513 | Live | 2, 4, 5 | Keep; add native transport as distinct relation |
| D | “Linear arithmetic follows R-0463” · 17517–17521 | Live | 5 | Keep under R-0463, consumed by R-0353 |
| D | “Boolean/bit-vector evidence” · 17522–17525 | Live | 5 | Keep under R-0451/R-0353 |
| D | “Nonlinear evidence remains solver_trusted” · 17526–17527 | Live | 5 | Keep named limitation |
| D | “Record independence…separate axes” · 17529–17533 | Live | 2, 5 | Keep under R-0353 |
| D | “Build sequence…freeze…ship Layer 0” · 17535–17540 | Superseded by queue rows 9–11 | 5 | Retire ordering; experimental encodings and consumer precede freeze |
| D | “From the first checker commit…” · 17542–17545 | Live | 2, 5 | Keep under R-0353 |
| D | “canonical binary format” · 17547–17550 | Live | 2, 5 | Keep under R-0353 |
| D | Kernel incident: “every replay receipt binds…” · 17558–17559 | Live | 2, 5 | Keep under R-0353 |
| D | Kernel incident: “accepted, vulnerable and revoked…” · 17560–17562 | Live | 2, 5 | Keep under R-0353 |
| D | Kernel incident: “advisory status…external” · 17563–17565 | Live | 2, 5 | Keep under R-0353 |
| D | Kernel incident: “high-assurance…independent replay” · 17566–17567 | Live | 2, 5 | Keep |
| D | Kernel incident: “checker independence…lineage” · 17568–17570 | Live | 2, 5 | Keep |
| D | Kernel incident: “every future…reproducer” · 17571–17573 | Live | 1, 2, 5 | Keep |
| D | Kernel incident: “re-executed…isolated process” · 17574–17575 | Live | 2, 5 | Keep |
| D | Kernel incident: “revocation enumerates…” · 17576–17577 | Live | 5, 7 | Keep |
| D | “Fragment-scoped revocation…” · 17579–17583 | Live | 5 | Keep optional optimization gated |
| D | “Move the common…codecs here” · 17591–17597 | Live | 2, 5 | Keep under R-0353; update IR names |
| D | “Every edge declares its relation…” · 17599–17605 | Live | 2, 5 | Keep under R-0353 |
| D | “Define…verify-bundle” / result vectors · 17607–17621 | Live | 2, 5 | Keep under R-0353 |
| D | “BackendIR → object → binary remains…trusted” · 17623–17629 | Superseded by S13/S15 for native profile | 2, 4, 5 | Keep only for uncovered LLVM/legacy edges |
| D | “self-contained offline proof-replay payload” · 17631–17636 | Live | 2, 5 | Keep under R-0353/R-0354 |
| D | “Gate expected-root mismatch…” · 17638–17645 | Live | 2, 5 | Keep under R-0353 |
| D | “Also gate omitted/duplicated/surplus…” · 17647–17653 | Live | 2, 5 | Keep under R-0353 |
| D | “Authority progression” · 17655–17658 | Live | 2, 5 | Keep with separate minimal-V1 and cross-profile scopes |
| D | Supply-chain bullet: toolchain and dependency locks · 17704–17711 | Live | 2, 3 | Keep under R-0357 |
| D | Supply-chain bullet: “THIRD_PARTY.md” · 17712–17715 | Live | 3 | Keep under R-0357 |
| D | Supply-chain bullet: “Red-team gate” · 17716–17720 | Live | 3 | Keep under R-0357 |
| D | “Windows must not remain ambient” · 17742–17748 | Live | 3 | Keep under R-0359 |
| D | “Governance” · 17753–17757 | Live | 3, 7 | Keep under R-0359 |
| D | “starts only after…public release” · 17801–17804 | Superseded by release readiness requirements | 3 | Retire start restriction; operations continue after release |
| D | Stewardship “Done when” · 17806–17810 | Live | 3, 7 | Merge into R-0433 readiness/operating checkpoints |
| D | Packages “Done when…registry protocol” · 17912–17914 | Live | 7; after-1.0/research | Keep full-phase exit separate from beta local foundation |
| D | Packages “Phase 8.5 completed” · 17916–17918 | Superseded in breadth by local-foundation decision | 3 | Replace with exact required graph/query checkpoints |
| D | “local foundation before beta” · 17920–17927 | Live | 3, 5, 7 | Keep under R-0383/R-0333 |
| D | “Integrated package management” · 17929–17942 | Live | 3 | Keep under R-0364–R-0367 |
| D | “interop checkpoint pulls forward…” · 17944–17955 | Live | 3 | Keep; explicit exception to whole-Phase-8.5 prerequisite |
| D | “Proof-authoring edges” · 17991–17999 | Live | 5, 7 | Keep under R-0364 |
| D | “exact application/binding manifest examples” · 18001–18005 | Live | 3 | Keep under R-0364 |
| D | “--locked…--offline…--frozen” · 18019–18025 | Live | 3 | Keep under R-0365 |
| D | “one workspace lock…multiple host/target” · 18027–18031 | Live | 3 | Keep under R-0365 |
| D | “private verifier input is withheld” · 18075–18078 | Live | 7 | Keep under R-0369 |
| D | Import constraints: “leave room…” · 18120–18125 | Live | 7; after-1.0/research | Keep under R-0372/R-0440; no premature claims |
| D | “checked fact set…at least” · 18138–18155 | Live | 7 | Keep under R-0372 |
| D | “first report view…small and agent-readable” · 18156–18161 | Live | 7 | Keep under R-0372/R-0390 |
| D | “Remote…native outputs…Regenerate locally” · 18251–18256 | Superseded for validated native images by S13/S15 | after-1.0/research | Keep conservative fallback; permit exact proved/checkable profile only |
| D | “Upload only deterministic…privacy” · 18257–18260 | Live | after-1.0/research | Keep under R-0382 |
| D | Editor “Done when” · 18280–18282 | Live | 3 | Merge into R-0398 |
| D | Editor “Phase 8.5 completed” · 18284–18286 | Live, overbroad dependency | 3 | Keep architecture requirement; enumerate minimal prerequisite exits |
| D | “Minimal pre-release slice…” · 18288–18294 | Live | 3, 7; after-1.0/research | Merge into R-0515/R-0398 checkpoints |
| D | “Also surface development-only expectations” · 18374–18377 | Live | 6 | Keep under R-0396 |
| D | Phase 20 “Done when” · 18412–18413 | Live | after-1.0/research | Merge into R-0415 |
| D | “atomics…shared mutable state…” · 18419–18429 | Live | after-1.0/research | Keep under R-0399–R-0402 |
| D | “Verona…Pony” design survey · 18431–18443 | Live | after-1.0/research | Keep under R-0399; survey is not a runtime commitment |
| D | “trigger fired…combinator core…R-0444” · 18483 | Superseded by R-0444 | 3, 4 | Retire from R-0406 scope; retain historical cross-reference |
| D | “generation layer proper…remains gated” · 18483 | Live | after-1.0/research | Keep under R-0406 |
| D | “First pressure-test only” · 18533–18540 | Live | after-1.0/research | Keep under R-0413 |
| D | “Native backend comparison” · 18542–18552 | Live | after-1.0/research | Keep under R-0413; R-0505 owns first backend |
| D | “What remains deferred…Datalog” · 18566–18580 | Live | after-1.0/research | Keep under R-0414 |
| D | “R-0005 removed…the recurrence mechanism” · 18590–18594 | Historical | 1 | Retire from open-work accounting; keep provenance pointer |
| D | “generated form…did not build” · 18596–18602 | Live | 1, 2 | Keep under R-0446 |
| D | “Two consumers…fuzz generation” · 18597–18599 | Live | 1 | Keep under R-0446/R-0491 |
| D | “and the capability matrix” · 18599–18606 | Live | 1, 2 | Keep under R-0446 |
| D | “original framing…superseded” · 18618–18626 | Historical; superseded by R-0484 | 1 | Retire original design; preserve explanatory reference |
| D | “Potential authority per function” · 18628–18634 | Live | 3, 4 | Keep under R-0487; containment remains R-0499/R-0510 |
| D | “Instantiation-level reports” · 18635–18638 | Live | 3, 4 | Keep under R-0487 |
| D | “Negative questions only under completeness” · 18639–18644 | Live | 3, 4 | Keep under R-0487 |
| D | “Out of scope…mutation, termination…inputs” · 18645–18647 | Live under R-0488/R-0473/R-0474 | 3, 6 | Keep owner cross-references; do not duplicate |
| D | R-0487 “Exit” · 18649–18656 | Live | 3, 4 | Keep |


<!-- Moved from ROADMAP.md's "Reconciliation record" on 2026-10-28; still binding. -->

## Binding reconciliation dispositions

The following dispositions replace conflicting implementation directions in the named tasks.
Existing regression cases and acceptance obligations survive unless explicitly retired with a reason.

- R-0114 merges into R-0492/R-0514; R-0116 merges into R-0497/R-0510/R-0514.
- R-0123, R-0163 and R-0164 target checked Core, generated ClaimSpec statements and
  Core-based composition under R-0495/R-0512. They add no new PExpr/FnTable mechanism.
- R-0176's forward ProofCore architecture is retired. R-0512 owns any isolation needed
  to preserve historical replay.
- R-0202, R-0262 and R-0269 retain legacy correctness maintenance only. New dependency
  closure and statement-generation work belongs to R-0495/R-0509; a PExpr bridge is
  permitted only as an explicitly proved R-0512 migration bridge.
- R-0268's architecture decision is settled by R-0496.
- R-0274 preserves a single authoritative checker over Core under R-0499/R-0514.
  Source diagnostics and elaboration do not replace its typing, ownership or capability rules.
- R-0278/R-0288 supply the validated BIR contract under R-0503, not an additional
  competing IR. R-0295's decision is settled by R-0501/R-0502.
- R-0464 retains the shared trap-definition and fail-closed analysis requirements.
  R-0460 owns VC generation proved sound against Core. Any SSA-based analysis needs
  an explicit proved relation to Core and must account for stores, aliases and calls.
- R-0408/R-0409 evaluate instrumentation and proof-engineering improvements to normative
  Core semantics under R-0496; they do not establish another language evaluator.
- R-0446's generated trap/fuzzer/consumer coverage is an increment of R-0491/R-0493.
- R-0297 is delegated to the Lean Compiler Backend; R-0505 integrates LCB and builds no Concrete
  machine backend; R-0113 records that LCB is Concrete's QBE path (R-0517).

R-0460, R-0299/R-0300 and R-0353 are not redundant with compiler correctness: VC soundness,
LLVM-profile validation and independent evidence consumption retain distinct obligations.
