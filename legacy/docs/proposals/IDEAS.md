**PROPOSED, awaiting owner decision (2026-11-06); reviewed by Codex, agy, Kimi.**

# Ideas from the previous compiler: forgotten or at risk (2026-11-06)

**Scope.** This covers ideas, features, design lessons and assets in `legacy/` (origin/main `d20e3ab333`), checked against the new design:
- `docs/decisions.md`
- `docs/PHILOSOPHY.md`
- `ROADMAP.md`
- `README.md`
- ConcreteSpec on `origin/wip/spec-root`
- `docs/archive/roadmap-2026-11/task_disposition.tsv`

Each old task in the disposition table is mapped to a new task in its `destination` column. Compile-time checks and reports are covered in `../legacy-checks/INVENTORY.md` and are not repeated here.

**Status values:**
- **CARRIED:** in a decided rule or the spec.
- **PLANNED:** a live task, directly or through a disposition destination.
- **DEFERRED:** a disposition trigger, with no live task.
- **RETAINED-ONLY:** a RETENTION row exists, but there is no plan.
- **FORGOTTEN:** no trace anywhere.
- **DROPPED-ON-PURPOSE:** dropped by a cited decision.

## Ranked: forgotten or at risk (v2, after Codex and Kimi review)

| # | Idea (legacy source) | Status | Recommendation |
|---|---|---|---|
| 1 | **Facts query and audit at source level** (`legacy/research/compiler/semantic-query-interface.md`; R-0179–R-0185 → R-0201) | PLANNED **in phase F**, which conflicts with the owner's "from the start" (2026-11-06). The query isn't yet on main; the recording batch is in flight. | **Roadmap:** split R-0201. Facts query in B0/B1 (per function: capabilities, allocation, termination, may-abort, stack, trust, evidence, each with a source location). Keep audit, diff and trust-diff in F. |
| 2 | **Policy declaration mechanism.** Legacy `Concrete.toml [policy]`, `POLICY_FILES.md`; capability budgets (R-0184, R-0186) | PLANNED **in phase F** (R-0201). The format, scope and composition across dependencies are undecided. | **Owner decision before B1:** one policy file, with scoping and composition, over the same facts. Predictable, termination and capability budgets all use it. |
| 3 | **Capability and fact preservation across indirect calls, monomorphization and packages** (structural class 6) | At risk: the spec covers direct calls only | **Spec, A2i/A3:** preservation statement. **Phase B:** a theorem. The facts query reports through these boundaries. |
| 4 | **Bounded execution, split.** (a) Termination, depth, stack and abort facts: CARRIED by the decision (Q11–Q14) and R-0574. (b) Quantitative step, time and allocation-count bounds (`legacy/research/stdlib/allocation-budgets.md`; R-0405, R-0415, R-0445): DEFERRED. The decision says "bounded" gives **no execution-time deadline**. | (b) DEFERRED | **Owner decision:** whether the predictable policy needs allocation-count and step certificates (R-0445), and when. |
| 5 | **Frames and dependency-flow facts** (reads/writes/modifies, `depends`; `legacy/docs/verification/SPARK_CLASS_ASSURANCE.md`) | PLANNED: R-0524 frames (E); R-0239 → R-0473; package summaries R-0380 → R-0383 (G). Missing from v1 of this list. | Keep. Add "what memory it writes" and "what its outputs depend on" to the facts-query schema now, as "not established" until E. |
| 6 | **Obligation sufficiency, spec adequacy and vacuity** (`legacy/docs/verification/PROVER_NEUTRAL_OBLIGATIONS.md`) | PLANNED: R-0460 retained (E); R-0193 and R-0213 → R-0219 (F). Missing from v1. | Keep. Vacuity (a proof that holds only under an unsatisfiable precondition) should block a "proved" fact from the first proof slice, not only from F. |
| 7 | **Exact source locations on every fact and obligation** (`examples/source_maps`; spec O4) | At risk: Core spans are absent and O4 is open | **Spec/compiler:** carry spans from slice 0 into CheckedCore and the facts schema. |
| 8 | **Fixed-capacity, non-allocating std** (`examples/fixed_capacity`) | RETAINED-ONLY. The std/examples migration is *open* (RETENTION.tsv); no predictable std subset is planned. | **Roadmap:** a predictable std subset in C, fixed-capacity containers first. |
| 9 | **I/O driver for the Lean reference evaluator** (`VERIFIED_PIPELINE.md`) | Implicit in R-0491/R-0496; no explicit task | **Roadmap:** name it inside R-0491 by B1. Independent expected outcomes for I/O programs need it. |
| 10 | **Independent expected outcomes / golden-oracle rule** (`GOLDEN_ORACLES.md`, bug 121) | PLANNED: R-0094 → R-0491; R-0425. v1 wrongly said FORGOTTEN. | Keep. Make sure R-0491's gate refuses expectations recorded from the compiler. |
| 11 | **Constant-time / secret flow** (`examples/constant_time_tag`; R-0136, R-0225, R-0527) | DEFERRED | **Owner:** make the first crypto port the trigger. |
| 12 | **Error conventions** (`ERROR_CONVENTIONS.md`) | PLANNED: R-0057 → R-0071 (C). Signature normalization (R-0017) is DEFERRED. | Keep. |
| 13 | **Historical replay vs current acceptance and revocation** (`EVIDENCE_ARCHITECTURE.md`, "Replay receipt") | PLANNED: R-0430 → R-0353 (F) | Keep. Certificates must not treat "was valid" as "is accepted". |
| 14 | **Debug info on the LLVM path** | PLANNED only for LCB (R-0517) | **Roadmap, C:** unverified, with a no-codegen-change check. Low priority. |
| 16 | **Declared assumptions file** (`legacy/docs/verification/ASSUMPTION_FILES.md`: target, heap, stack, FFI and trusted-surface assumptions, with a diff gate) | Probably FORGOTTEN: no disposition row found by grep (agy) | **Spec/roadmap:** a project assumptions view, derived from facts where possible and declared only where it can't be, beside the policy file (#2). |
| 17 | **Contract-strength labels** (point / one-direction / iff / full contract; `legacy/tests/showcase/manifest.toml`, `EVIDENCE_ARCHITECTURE.md`) | Probably FORGOTTEN: no row found | **Facts query:** report the strength of each proved claim, so that a single-input proof never reads as a universal contract. |
| 18 | **Paired "Concrete catches this" negative fixtures per flagship** (`legacy/tests/showcase` catches/) | RETAINED-ONLY (showcase) | **Roadmap:** every port in C/D ships its rejected-violation twins. |
| 15 | **Freestanding / embedded; deterministic fault simulation** (R-0322/R-0324 deferred; R-0030 → R-0071) | DEFERRED / PLANNED | Leave. Name the first embedded workload as the trigger. |

## Other ideas, by status

**CARRIED:**
- **Identity, not spellings** (structural classes 1, 2, 3, 9, 10, 18): rule 13; Var identities in the spec; certificates.
- **One semantics** (class 7): the Lean evaluator is the reference interpreter.
- **Every fact once** (class 12): rule 5.
- **Rules with an enforcing site** (class 13): Lean is the spec, with rule-indexed conformance.
- **Ownership along control-flow paths** (class 15): the spec.
- **Absence as a fact** (class 11): rule 12.
- **Validated wrappers / construction rights:** TYPE-11 and OD123.
- **Ghost state:** OD81.
- **Loop invariants and variants:** OD82, plus `#[variant]` on functions.
- **Assumptions:** `assume` is tracked (OD80); named premises.
- **Claim classes** (enforced / reported / proved / trusted): rule 12.
- **Spec drift / proof-to-source binding** (`*_drifted` examples): certificates bound to exact bytes (R-0552); proofs re-checked every build.
- **Deterministic compiler output:** reproducible builds at B1 (decided 2026-11-06).
- **Main/exit model:** STMT-7/OD24, with exits 125 and 134 in the spec Observation part.
- **UTF-8 strings:** OD127 and Q4.2.
- **Constructor-coverage / walker checklist:** Lean exhaustive matching plus the totality audit (R-0553).

**PLANNED:**
- **Threat model:** R-0230 → R-0509.
- **Stdlib authority gates:** R-0062 → R-0556.
- **Newtype invariants as hypotheses:** R-0258 → R-0259.
- **Panic and abort as denial-of-service obligations:** R-0246 → R-0259.
- **Debug or checked allocator:** R-0314 → R-0321.
- **std.fmt / std.parse:** R-0039 → R-0071.
- **Floats:** R-0087 → R-0512; R-0234 → R-0496.
- **Compiled-binary fuzzing against safety claims:** R-0462 → R-0259.
- **Sanitizer hooks:** R-0063 → R-0301.
- **Arithmetic-site facts:** R-0222 → R-0493.
- **Core trap inventory:** R-0446 → R-0118.
- **Reduction and minimization:** R-0009 / R-0106.
- **Fuzzing:** R-0272.
- **Performance evidence:** R-0424.
- **LSP and editor:** R-0385 → R-0398.
- **Incremental builds:** R-0149.
- **Browser playground:** R-0394, deferred → R-0569 new site.
- **Native-library linking and C/Rust/Zig interop:** R-0285 / R-0301.
- **Diagnostics UX:** R-0075, R-0253.
- **Error conventions:** R-0057 → R-0071 (item 12).

**DEFERRED:**
- Concurrency: R-0399–R-0401.
- Constant-time: #11.
- Step/time bounds: #4(b).
- Freestanding: #15.
- Multi-kernel replay: R-0448, deferred with a trigger (R-0522 only retired the duplicate Isabelle builder paths; correction from agy).
- Error-signature normalization: R-0017 (error conventions themselves are PLANNED, item 12).
- Development-only `expect`: R-0240.
- Binary-format DSLs: R-0406.

**RETAINED-ONLY:**
- Fixed-capacity std: #8.
- The research and paper directories.
- `tests/showcase`.
- The 60 examples (migrated as a corpus; real-program ports follow phase C/D).

**DROPPED-ON-PURPOSE:**
- An SMT-based contract checker for admission: no SMT for admission (no-hidden-traps decision). SMT-checked *lowering* stays as proof-order item 7.
- Profiles as separate admission modes: OD124, profiles kept as a refusal chapter; replaced by policies over facts.
- The transformation-soundness register: legacy ProofCore, retired (R-0262/R-0276).
- Broad `#[cfg]`: P17b.

## Summary

Counts are omitted (corrected 2026-11-06): the ranked table and the status lists above are the
record. No idea is known to be FORGOTTEN, but ranked #16 (the declared-assumptions file) and #17
(contract-strength labels) are probably forgotten (no row found by grep). Two ideas had no home: a
policy file format, and preservation across indirect calls, specialization and packages. Four
PLANNED ideas sat too late: the facts query, policy, the frames schema and vacuity.

## Bug structural classes not yet structurally prevented

- **Class 6** (capability facts across dispatch, specialization and packages): #3.
- **Class 14** (trusted std and FFI with no checked contract): unsafe contracts are decided (Q3.7–Q3.12) but not yet in the spec. Their facts must surface in the query.
- **Class 16** (artifacts at paths no run owns) and **class 17** (pass state in association lists and per-query recomputation, i.e. scaling): process and performance. R-0550 covers scaling; class 16 needs the certificate/artifact binding (R-0552).
- **Class 19** (a mention read as a use): ANF Core with explicit copy/move should prevent it. This needs a conformance example per binding form.

## Reviewer verdicts

- **Codex: AGREE-WITH-CHANGES.**
  - The golden rule is PLANNED (R-0094 → R-0491); error conventions are partly PLANNED (R-0057 → R-0071); #8 must be split (R-0169/R-0174 were miscited); "std migrated" is wrong.
  - It added frames and dependency flow, allocation-count budgets, and replay vs acceptance.
  - Its top 5: bounded-execution and quantitative gaps; policy; preservation across boundaries; frames; fixed-capacity std.
- **Kimi: AGREE-WITH-CHANGES.**
  - The same corrections, plus: policy is PLANNED late (R-0184/R-0186 → R-0201 F), and the counts didn't reconcile.
  - It added the facts query being in phase F, obligation sufficiency and vacuity, and frames.
  - Its top 5: day-one source-located facts query; one policy mechanism; preservation across boundaries; sufficiency and vacuity; independent outcomes with an I/O-capable Lean evaluator.
- **agy: AGREE-WITH-CHANGES.** Its first run was auto-denied shell access in headless mode; the retry used read tools only.
  - Multi-kernel replay was miscited (R-0522 → R-0448, deferred); the counts were off.
  - It added the assumptions file, contract-strength labels, paired negative fixtures, and receipt vs policy decision.
  - Its top 5: day-one facts query plus policy pulled into B; whole-program stack bound; spec obligations O1/O3/O5; assumptions file plus strength labels; step bounds plus fixed-capacity std.

I checked every cited row myself against `task_disposition.tsv` and ROADMAP.md; all the corrections hold. Rows #16 and #17 are "no row found by grep", not proven absent.


