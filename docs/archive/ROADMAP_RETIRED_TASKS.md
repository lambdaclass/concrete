# Retired Roadmap Task Bodies

Status: archive — the bodies of done, superseded, merged, retired and delegated tasks, moved out of
[ROADMAP.md](../../ROADMAP.md).
Source: `ROADMAP.md` at commit `a159c489` (2026-10-19), moved verbatim; only heading levels and relative
links were adjusted for this location.

Each task keeps a stub in the roadmap (ROADMAP.md, or since 2026-10-19 the
[roadmap backlog](../project/ROADMAP_BACKLOG.md) when its phase is not active or next) — its `### Task R-NNNN` heading, **Phase:** (called **Stage:**
when these bodies were moved), **Disposition:**
and **Objective:** lines, and a link here. This file declares no task: each section below is headed
by the bare task ID, so every ID is still declared exactly once, in the roadmap or its backlog.
`scripts/tests/check_roadmap_coverage.sh` checks that every section here has its stub and every stub
links to its section. The [reconciliation record](../project/ROADMAP_RECONCILIATION.md) names each
task's owner; each owner lists what it inherited under **Inherited obligations**. Nothing here
schedules work: obligations bind through their owners, not through this file.

| task | disposition | owner(s) |
|---|---|---|
| [R-0004](#r-0004) | done | — |
| [R-0478](#r-0478) | merged | R-0481, R-0474 |
| [R-0007](#r-0007) | superseded | R-0492, R-0510 |
| [R-0434](#r-0434) | superseded | R-0502 |
| [R-0435](#r-0435) | superseded | R-0500 |
| [R-0437](#r-0437) | superseded | R-0514, R-0510 |
| [R-0447](#r-0447) | merged | R-0481 |
| [R-0019](#r-0019) | merged | R-0031 |
| [R-0029](#r-0029) | merged | R-0065 |
| [R-0051](#r-0051) | merged | R-0014 |
| [R-0483](#r-0483) | done | — |
| [R-0484](#r-0484) | done | — |
| [R-0072](#r-0072) | retired | R-0517 |
| [R-0074](#r-0074) | superseded | R-0503, R-0517 |
| [R-0076](#r-0076) | superseded | R-0502 |
| [R-0079](#r-0079) | superseded | R-0493, R-0501, R-0503 |
| [R-0082](#r-0082) | superseded | R-0497, R-0501, R-0503 |
| [R-0083](#r-0083) | retired | R-0511, R-0517 |
| [R-0086](#r-0086) | superseded | R-0511, R-0517 |
| [R-0087](#r-0087) | superseded | R-0512 |
| [R-0089](#r-0089) | superseded | R-0505, R-0517 |
| [R-0092](#r-0092) | superseded | R-0491 |
| [R-0093](#r-0093) | merged | R-0092 |
| [R-0097](#r-0097) | superseded | R-0511, R-0512 |
| [R-0098](#r-0098) | merged | R-0137 |
| [R-0099](#r-0099) | retired | R-0517, R-0505 |
| [R-0100](#r-0100) | superseded | R-0305 |
| [R-0101](#r-0101) | delegated | R-0517 |
| [R-0105](#r-0105) | superseded | R-0491, R-0504 |
| [R-0107](#r-0107) | merged | R-0075 |
| [R-0108](#r-0108) | merged | R-0416, R-0515 |
| [R-0110](#r-0110) | merged | R-0075, R-0333 |
| [R-0111](#r-0111) | superseded | R-0511, R-0137 |
| [R-0112](#r-0112) | superseded | R-0512 |
| [R-0114](#r-0114) | superseded | R-0492 |
| [R-0116](#r-0116) | superseded | R-0497, R-0510, R-0514 |
| [R-0171](#r-0171) | superseded | R-0494 |
| [R-0176](#r-0176) | superseded | R-0495, R-0512 |
| [R-0208](#r-0208) | done | — |
| [R-0211](#r-0211) | superseded | R-0509 |
| [R-0221](#r-0221) | superseded | R-0493 |
| [R-0228](#r-0228) | superseded | R-0499 |
| [R-0234](#r-0234) | superseded | R-0496 |
| [R-0235](#r-0235) | superseded | R-0496, R-0495 |
| [R-0464](#r-0464) | superseded | R-0493, R-0495, R-0460 |
| [R-0465](#r-0465) | done | — |
| [R-0260](#r-0260) | superseded | R-0506 |
| [R-0262](#r-0262) | superseded | R-0495, R-0512 |
| [R-0266](#r-0266) | superseded | R-0509 |
| [R-0271](#r-0271) | superseded | R-0510 |
| [R-0273](#r-0273) | superseded | R-0491, R-0498 |
| [R-0274](#r-0274) | superseded | R-0499, R-0514 |
| [R-0276](#r-0276) | superseded | R-0506, R-0496 |
| [R-0456](#r-0456) | superseded | R-0496 |
| [R-0288](#r-0288) | superseded | R-0503 |
| [R-0296](#r-0296) | retired | R-0517, R-0505 |
| [R-0297](#r-0297) | delegated | R-0517, R-0505 |
| [R-0348](#r-0348) | merged | R-0349 |

## R-0004

_Stub: [ROADMAP.md](../project/ROADMAP_TASKS.md#task-r-0004) · disposition: done · owner(s): —_

> **Historical boundary:** the detailed R-0004 execution material below is retained for design
> rationale, rejected hypotheses, dated measurements, and audit traceability. Its `ACTIVE`,
> `PENDING`, `NOT STARTED`, “remaining,” and “cannot close” statements describe the moment in which
> they were written and are not current status. The explicitly titled **Post-R-0004 milestones and
> longer-term research** subsection remains current future work. See
> [R-0004 execution history](R-0004_EXECUTION_HISTORY.md) for the condensed chronology.

### Historical execution map — trust boundaries, not percentage complete

**Status authority at the time:** this was the execution summary while R-0004 was active. It is now
superseded by the dated closure record above. Do not describe the closed task as a fraction such as
"6.5/8" or as a percentage: those figures combine implemented helpers, shadow measurements and
authoritative guarantees.

Every boundary advances through the same states and may not skip one:

```text
designed -> implemented -> negatively controlled -> mutation-killed
         -> shadow-measured -> authoritative -> adversarially closed
```

| trust boundary | current state | authority consequence |
|---|---|---|
| proof-subject identity | **implemented, mutation-controlled, schema frozen** | V2 exists but remains shadow until replay/migration |
| exact migration inventory | **closed: 44/44 stored links accounted** | inventory is authoritative; migrated verdicts are not |
| classification discovery | **implemented and mandatory-gated** | both proof-attachment paths are enumerated |
| classification freshness | **implemented and mandatory-gated** | checked-in rows are compared with the live Lean environment |
| classification structural validation | **implemented and mutation-controlled** | malformed, ambiguous and duplicate rows cannot classify |
| table/body provenance | **implemented and mutation-killed** | stored provenance is recomputed from the actual `PFnDef.body` |
| implementation-manifest provenance | **implemented and mutation-killed** | authoritative construction computes digests internally; raw-digest construction is closed on the gated path |
| implementation-manifest completeness | **implemented and corpus-measured** | exact expected/row/refusal sets replace `filterMap`; 64 expected, 63 rows and 1 named refusal are visible |
| closed correspondence join | **implemented and synthetically controlled** | `CorrespondenceResult` retains matched, missing, ambiguous, surplus and malformed sets; this is not yet corpus or verdict authority |
| production correspondence population | **CLOSED 2026-08-15.** `correspondenceInputOf` assembles real compiler edges, validated classification rows and scoped table membership per subject, and `applyCorrespondenceAuthority` CONSUMES the result: a claim may be `proved` only if every edge in its closure is justified and the closure computes. (This row read "OPEN — critical semantic boundary" until 2026-08-16, well after the assembly and the consumption both landed.) | — |
| table-resolution inventory | **PARTIAL: 8/9 referenced table identities resolve** | `combineFns` is the single named external-table refusal; resolution is not silently treated as absence |
| dependency-root computation | **CONSUMED as of 2026-08-15, and currently redundant.** Keyed on `DefinitionIdentity`; an unkeyable edge or a callee without a node REFUSES the closure. Measured 2026-08-16: 13 subject roots refuse, none of them `proved`, so requiring a computable closure downgrades nobody correspondence does not already refuse. The redundancy is PINNED. (The earlier "62/64, must not affect production status" figure predates both the identity migration and the authority pass.) | keep the redundancy measured; a divergence means the root has become load-bearing |
| implementation-manifest accounting | **shadow-measured: 64 expected, 63 rows, 1 named refusal** (`extracted-missing` in `examples/proof_pressure/src/main.con`, so that file is correctly not usable). The old `filterMap` returned that file's 4 rows as an apparently complete manifest — the incompleteness predates the fix; only its visibility changed | manifest MUST NOT affect production status yet |
| report/CLI disposition integrity | **OPEN — two concrete acceptance defects found 2026-08-13**: `--report impl-manifest` prints its payload but falls through to `Unknown report type` and exits 1; a failed obligation lookup now renders the honest marker `no_obligation_record` but the prove exit-code catch-all maps it to success | no report or proof-facing CLI result is authoritative until its payload, stderr and process disposition agree and are gated together |
| classification-refusal reachability | **PARTIAL**: `absent`, `ambiguous (count)` and `malformed` are represented, but committed controls reach only the production `absent` path; duplicate fixtures reproduce a local `filter`/`none` approximation rather than exercising `validatedRowOf`'s ambiguity result | a named refusal receives completion credit only after the exact production lookup and fail-closed consumer path are negatively controlled |
| ~~edge sources disagree = coverage fail-open~~ **RETRACTED 2026-08-13, diagnosed backwards** | I recorded that 9 of 64 subjects rooted over a smaller edge set, called it a coverage fail-open, and predicted root coverage would fall to ~53/64 once fixed. **All of that was wrong.** `shadow edgeKinds` was a STUB: it derived callees from the evidence body then assigned the kind as `if isTrusted then "trusted" else "unclassified"`, consulting no classification table, so every non-trusted edge read `unclassified` regardless of the hand-back. The root was correct and the REPORT was stale — and since `dependencyRootMaterial` refuses any non-current edge, a subject with edges that roots has current edges by construction. **Root coverage remains 62/64 and was never inflated.** Fixed by making both lines read one source (`dependencyNodesOf`); the gate now asserts the invariant (no subject roots while reporting a non-current edge, 0 observed) plus its non-vacuity (12 non-current edges exist) | one producer for edge kinds; a hand-coded second opinion cannot drift from the real one if there is no second opinion |
| real edge-kind distribution | **28 `body`, 11 `unclassified`, 1 `missing`** across 40 edges and 11 edge-bearing subjects (53 of 64 subjects have no outgoing edge). Measured 2026-08-13 through the corrected line. **Supersedes the earlier "all 40 unclassified, zero body" figure, which was read off the stub** | correspondence CAN be demonstrated on this corpus — 28 classified `body` edges exist, so a correspondence gate would not be vacuous. The earlier claim that it would be is withdrawn |
| **per-edge correspondence maps to `DependencyClosure`, NOT `SourceCorrespondence`** | Corrected 2026-08-13 by reading `docs/verification/EVIDENCE_ARCHITECTURE.md` rather than inferring from the commit subject: my first reading assumed the ratified `SourceCorrespondence` object superseded the `EdgeJustification` design. It does not. `SourceCorrespondence` is proposition↔source/Core behaviour; `DependencyClosure` is "every compiler edge, theorem witness, contract hypothesis, table entry, implementation, assumption and trusted boundary … present, unique, current, and consumed exactly once", with missing/surplus/duplicate/ambiguous/unclassified/mismatched as NAMED refusals never discarded by `filterMap` or a first-match lookup. **The slice-6 work is the implementation of `DependencyClosure`, not something replaced by the new architecture** | build `DependencyClosure`, not a parallel `EdgeJustification`. The architecture also retroactively endorses the two fixes already landed: `filterMap` removal and the first-match join both appear in its list of forbidden shapes |
| DependencyClosure refusal vocabulary | **implemented: 6/6 named, gate-derived** | missing, duplicate, ambiguous, unclassified, mismatched and surplus remain distinct; production reachability and consumption are still pending |
| ~~the compiler cannot check table membership per edge~~ **RESOLVED 2026-08-13 — the blocker was not one** | Recorded for weeks as needing the generator to `evalExpr` each `FnTable` and cross entry evidence over as data. The premise was half wrong: the hand-back really does carry only a name and a whole-table digest, **but the compiler IS a Lean program and these tables are ordinary definitions inside it**, so the values were already linked in. Only a name→value dispatch was missing. `Concrete/Proof/TableResolve.lean` provides it; `tableContainsCallee` answers the question per edge. Measured: `proofFns` 2 entries, `cryptoFns` 4, `elfFns` 5; membership true for a held callee, false for an absent one, and a named refusal for an unlinked table. **8 of the 9 tables the hand-back names are dispatched; the exception is `Examples.ProofPatterns.Proofs.combineFns`, defined under `proofs/` outside the compiler build, which correctly refuses.** The dispatch-coverage gate found a real omission while being written — `FnTable.empty` — which would have reported a vacuously-empty membership as an unresolvable dependency | no `evalExpr`, no generator change, no schema change. Correspondence can now be fed from real tables |
| ~~correspondence measured on the real corpus 2026-08-13~~ **HISTORICAL — superseded 2026-08-16** | **6 of 11 edge-bearing subjects fully correspond** (22 matched edges); the other 5 have `missing` edges — a callee in no table their theorem names. **Total surplus 0**, and that is a real assertion: deriving one witness per TABLE ENTRY produced surplus on 5 subjects, because a table legitimately holds implementations the caller never reaches. Witnesses are derived per REQUESTED EDGE the table covers, so nonzero surplus means that regression returned. 53 of 64 subjects have no outgoing edge. Pinned SEPARATELY from root coverage (62/64): rooting says the closure computes, corresponding says every edge in it has exactly one validated justification | reach 11/11, then integrate |
| **`combineFns` CANNOT be dispatched — dependency cycle, measured 2026-08-13** | `proofs/` is a separate `lean_lib` (`srcDir = "proofs"`) whose modules `import Concrete.Proof.Proof`. The compiler linking `combineFns` would require `Concrete` to import `proofs`, which is a cycle. **So the named refusal is correct and permanent for the dispatch route** — not a gap to close by extending the list. **But the fix is a much smaller version of the original hand-back idea than the one that was abandoned:** the classification generator already elaborates those theorems, so a generator module importing `Examples.ProofPatterns.Proofs` can reference `combineFns` DIRECTLY and emit its entry evidence as data — the same name→value trick as the dispatch, one level up, and again with no `evalExpr`. **Entry evidence in the hand-back is therefore needed ONLY for tables the compiler cannot link, not for all of them:** in-compiler tables resolve exactly through the dispatch (8 of 9), and only out-of-build tables carry data. That is the whole remaining cost of taking correspondence 6/11 → 9/11 | generate entry evidence for out-of-build tables only; keep the dispatch for the rest |
| **entry-derived table identity — landed 2026-08-14** | `entryTableDigest name rows` binds the table's own identity plus, per entry, the callable identity, source-body schema and scope, and body digest; canonically ordered so REORDERING IS EQUIVALENT while add/remove/duplicate/alter are not. Generator and compiler call the SAME function. **It is SINGLE-SOURCE RECOMPUTATION, not an independent check** (corrected 2026-08-14 — generator and compiler run the same implementation). It establishes canonical-encoding consistency, the binding between name/entries/digest, and detection of corruption, stale rows and copied digests. It does NOT validate the formula, the generator, or that external entries describe real bodies — nothing in that path reads a body. A consistently-altered entry list and digest verify structurally BY DESIGN, pinned by a probe whose value is in continuing to pass. Real independence would need a standalone verifier implementing this canonical format separately. **Agreement does NOT upgrade evidence:** `combineFns` verifies and stays `generatorAsserted`, pinned by its own probe. Eight controls plus mutation `external-table-digest-checked` (changes the stored digest alone), KILLED | the reviewed mutation list is COMPLETE: `dispatch-routes-to-correct-table` (points `cryptoFns` at `elfFns` — resolves with plausible material, so nothing looks absent) and `external-stays-asserted` both KILLED 2026-08-14 |
| ~~correspondence 9/11 as of 2026-08-14~~ **HISTORICAL — superseded 2026-08-16 (see the Package 2 closure section)** | Was 8/11. The `calls.dbl` `«unresolved»` identity is FIXED at its earliest cause: `ProofCoreExcluded` carried no `callableId` although `cid` was in scope at both construction sites, so a TRUSTED helper — excluded from proof entries but still a real callable — had no identity, became `«unresolved»`, and its edge typed `missing`. The `trusted` branch was also unreachable for exactly the functions it existed for, since the check sat inside the `some cid` arm. Field retained; `idOf` consults both populations, entries first. A trusted edge is now witnessed by the DECLARED TRUST BOUNDARY rather than by table membership (asking a classification row to justify declared trust is the wrong producer). **Roots UNCHANGED at 62/64** — that subject's refusal moved to "depends on 'calls.dbl', which has no node", owned by node construction, not correspondence. Non-current edges 12 → 11, no `missing` edge remains. Mutation `excluded-identity-retained` KILLED. **ONE remains.** `fixed_capacity.validate_message` turned out NOT to be a correspondence failure: it has no `#[proof_by]` and no hand-back row — an eligible function with 11 edges and NO LINKED PROOF. It makes no claim, so there is nothing to justify, and counting it as a failure reported "nobody proved this" as "this proof is unsound". Now reported `no claim` and excluded from the denominator: **correspondence is 9/10 claiming subjects, plus 1 no-claim.** The exemption is narrow — only the TOTAL absence of a theorem qualifies; a subject WITH a theorem whose classification is unusable still reports `usable=no`, guarded by a control asserting at least one such subject exists. `main.validate_header` is that subject and the single remaining failure | **AUDITED 2026-08-14: the refusal is CORRECT and 10/10 needs a FIXTURE repair, not compiler work.** `proof_pressure`'s `validate_header` calls `check_nonce` and carries `#[proof_by(Examples.ElfHeader.Proofs.validate_header_correct)]`. That theorem's table `elfFns` holds check_class/check_data/check_magic/check_version/validate_header — measured, and no `check_nonce`. The theorem is about a DIFFERENT function sharing the qualified name `main.validate_header`, while the fixture's own header documents this one as intended-proved. A name-based attachment that passes every name-based check and fails an identity-and-membership one — the exact class slice 6 exists to surface. Pinned as a known-correct refusal whose failure message distinguishes "fixture repaired" from "join loosened" |
| **DECIDED and TYPE LANDED 2026-08-14 — proof-table membership and correspondence are program/package scoped** | `Concrete/Proof/DefinitionIdentity.lean`: `{packageIdentity, moduleIdentity, declarationIdentity, implementationIdentity}`, private constructor, `sameDefinition` comparing ALL FOUR. `CallableId` remains the compiler-local name; evidence boundaries take the stronger identity. Cross-program collision controls exist, with a positive control and empty-component refusals. `legacyNameOnly` is the `needs_recheck` disposition for name-only evidence. **NOT production: nothing consumes it — table entries and requested edges still carry `CallableId`, so the join still matches on the weaker identity** | **STEPS 1-2 LANDED 2026-08-14:** `PackageIdentity` is a private, non-empty type refusing location-dependent components; `packageIdentityOf` derives it from declared material (section-aware manifest read — a `name` under `[dependencies]` refuses rather than being adopted) plus module and dependency inventories, never from a path; `ProjectContext.packageIdentity : Except PackageIdentityRefusal PackageIdentity` is computed once in `loadProject` and threaded. **RESIDUAL COLLISION, not closed:** the manifest has no origin/namespace field (measured: only `name` 29× and `version` 22× across the corpus), so two UNRELATED packages sharing name, version, dependency names and module names still collide — closing it needs a declared origin in `Concrete.toml`, a manifest surface change. **This residual collision does NOT block R-0004:** current corpus controls detect any actual collision, and the general origin/namespace decision is deferred to **R-0482** because it is a manifest-surface trust-model change, not a soundness hole in the evidence producer. **STILL UNCONSUMED: `extractProofCore` does not take it, `DefinitionIdentity` is unbuilt at the boundary, the join still matches `CallableId`.** **STEP 4a LANDED 2026-08-14: `DefinitionIdentity` is CONSTRUCTED at the extraction boundary — 64/64 subjects scoped.** `ProofCoreEntry.definitionIdentity`, built where the package identity and the authoritative digest are both in scope (`extractModule` threads the package identity). Implementation component is `implementationDigest`, NOT the V1 body digest, so a signature/capability/generic/contract change is not the same definition. `Except`, so an entry without an authoritative digest carries a refusal. **MEASURED: 64/64 subjects scoped (none refused); `v1:user:main.validate_header` is ONE `CallableId` rendering and TWO distinct scoped identities — the motivating collision separated on real data.** Corpus figures stated precisely: 64 subjects, 52 distinct `CallableId` renderings, 53 distinct scoped identities — the identity separates ONE additional pair, and that pair is the `validate_header` collision; other repeated renderings are subjects whose packages and implementations genuinely agree. **ATTESTATION DESIGN RATIFIED AND STEPS 1-2 LANDED 2026-08-14.** `PFnDef` stays a model and gains nothing it cannot derive. `AttestedPFnDef` (private ctor) binds a model to a GENERATED reference; `FnTable.ofAttested` refuses a missing/malformed attestation (`needs_recheck`) and refuses duplicate attestations, while PERMITTING two same-named models attesting different implementations — refusing that would reimpose the `CallableId` collapse inside the new type. `--report generated-implementations` emits the author surface from compiler facts as `Except`-typed opaque symbols (`of?` validates, constructor stays private, so a bad reference stays a refusal); it also reports `unscoped=N` so a missing reference is a stated gap rather than a silently shorter file. Verified end-to-end against the real emitter: generated output compiles, two selected symbols build a table, refused and duplicated attestations are rejected. **Matching implementation identity does NOT prove model faithfulness** — that remains a separate claim, documented on the type. **MEASURED BLOCKER FOR THE ATOMIC TRANSITION, 2026-08-14: proof tables are NOT per-package, so "attest the table" is not available.** Table→package cardinality across the corpus: `Concrete.Proof.FnTable.empty` serves **8** packages (weakened_postcondition, array_update, fold, ghost, loop_copy, stale_missing_partial, straight_line, workspace); `Examples.ProofPatterns.Proofs.combineFns` serves **3** (composition, composition_trusted_helper, composition_unlinked_helper); `cryptoFns` serves **2** (crypto_verify, proof_pressure); `elfFns` serves **2** (elf_header, proof_pressure); `fixedCapacityFns` and `parseValidateFns` serve 1 each.

**Correction to the dated cardinality in the preceding row:** the `#[proof_by]`-only pass counted eight `FnTable.empty` packages. The complete attachment union measures **13 rows across 10 consuming fixtures and 10 scoped package identities**. Also, corpus controls show only that today's identities do not collide; they cannot detect two packages whose computed identity bytes are identical. R-0004's guarantee is therefore exact selection relative to declared/content identity, while globally unique publisher origin remains R-0482's scope.

**Consequence, and it is a real risk to the 9/10 figure.** Attestation is per-ENTRY, so a table serving several packages needs one attested entry per (model, package) pair — which `FnTable.ofAttested` already permits, since it refuses only DUPLICATE identities and two entries may share a model while attesting different implementations. But if the 3 `combineFns` packages are converted with a single attestation, at most one of the three `calls.combine` subjects can correspond and **the current 9/10 would fall to 7/10**. The conversion must therefore emit one attested entry per package that legitimately reuses a model, and the `elfFns`/`cryptoFns` overlap with `proof_pressure` must be left refusing, since that overlap IS the known misattachment.

**This also explains why `FnTable.empty` is the highest-risk site**: **10 fixtures and 10 scoped package identities** share it, so a single attestation would silently strip scoped evidence from nine of them.

**DECISION DISSOLVED 2026-08-14 — `ofAttested` is now TOTAL and no option is needed.** The `Option` existed only because the constructor refused at the DEFINITION site, which is what threatened 103 theorem statements. Refusal moved to the EVIDENCE boundary: a table with a missing or duplicated attestation still evaluates (a model is all evaluation needs) and `scopedEntryEvidence` refuses it with `attestationIncomplete` / `duplicateDefinition` / `legacyUnattested`. Costs nothing the three options cost — no theorem statement changes, no back door, no total `DefinitionIdentity` constructor, and the private constructor stays TYPE-enforced rather than gate-enforced. Attestation failures are COUNTED (`attestationFailures`), since a dropped failure is indistinguishable from an unattested entry, and `attestationIncomplete` is checked BEFORE `legacyUnattested` so a table someone did attest is not reported as unattested. ~~Superseded analysis:~~ (the cost was NOT the 29 literals `FnTable.ofAttested` returns `Option FnTable`, so converting `def proofFns : FnTable` changes its TYPE, and **103 theorem/eval sites depend on these eight tables**: proofFns 11, proofFnsExt 11, cryptoFns 18, elfFns 19, fixedCapacityFns 12, parseValidateFns 15, ctTagFns 9, combineFns 8. Threading an `Option` through kernel-checked theorem statements is not a mechanical edit — it changes what the proofs state.

Three options, with what each costs:

**(a) Corpus threads the `Option`.** Honest, no new surface. Cost: 103 sites, and proof STATEMENTS change, so every affected theorem is re-checked rather than merely re-elaborated.

**(b) Tables written as `{ entries := …, attested := … }` directly.** `attested` is a plain field, and a `DefinitionIdentity` still cannot be forged (private ctor, `of?` validates), so authors could select generated symbols. But generated symbols are `Except`, so this needs a total way to get a bare identity — i.e. a fallback, which is a back door.

**(c) `DefinitionIdentity.ofGenerated`, gated to the generated file only.** Exposes a total constructor and enforces the boundary with a GATE rather than the type — precisely the discipline `check_one_producer.sh` already applies (it asserts exactly which files may reference a producer, code-only via `lib/code_refs.sh`). Cost: the boundary becomes gate-enforced instead of type-enforced, which is weaker in kind but enforced in practice and leaves the 103 theorem statements untouched.

**Superseded recommendation: (c).** This appeared to be the only option that did not change proof
statements, but it accepted a gate-enforced boundary only because refusal had been placed at the
wrong layer. The total `ofAttested` design above preserves the private constructor and makes this
generated-only constructor unnecessary. Do not implement it.

**CONVERSION PATH LANDED 2026-08-14: `FnTable.withAttestations`.** `ofAttested` cannot convert the hand-written tables — they set `globals` for dispatch and carry `@[simp] … .globals = … := rfl` lemmas the proofs rely on, and `ofAttested` nulls `globals`. `withAttestations` is a structure update, so type, behaviour and the `rfl` lemmas are untouched. `scopedEntryEvidence` also now refuses `attestedModelNotInTable`, since `attested` and `entries` are separate arrays and nothing structural stopped a table attesting a model it does not hold.

**TWO MEASUREMENTS THAT MUST PRECEDE THE CONVERSION — both found by attempting it.** (1) Generating references for `proofFns` resolved to `examples/thesis_demo/src/main_drifted.con`, a DRIFT fixture whose digests are deliberately stale; converting against it would bake drifted identities into the table and look correct. (2) `proofFns` never appeared in the table→package cardinality map, because that map followed only `#[proof_by]`-linked theorems while `proofFns` is used by in-repo theorems. **So the conversion needs a per-table source-of-truth mapping covering BOTH attachment paths, and it must exclude drift fixtures** — the same union defect `scripts/gen/classifications.lean` was built to fix, recurring one layer down.

**GENERATOR JOINS COMPILER FACTS AND RECONCILES 2026-08-14 — `scripts/gen/attestation_manifest.sh` exits 0.** Identity comes from `--report attestation-join` (compiler-produced, same facts as the `ProofCore` boundary); the script ORCHESTRATES and does not derive. An earlier version derived package identity from directory layout, which would have been a second producer of implementation identity at the moment R-0004 eliminates them. Package scope comes from the CONSUMING SUBJECT, so an in-repo theorem needs no package of its own — that dissolved the "proofFns has no package" problem rather than solving it.

**Typed reconciliation:** 25 fixtures scanned, 1 excluded as drift, 86 subject rows = 0 without identity + 38 without a table + 48 manifest-backed table rows; 0 duplicates, 0 surplus, 0 package collapses. Every input row ends mapped, excluded or refused, and a mismatch REFUSES. **Reuse resolves with no special case: `FnTable.empty` accounts for 13 rows across 10 consuming fixtures and 10 distinct scoped package identities.**

**PACKAGE COLLAPSE FOUND AND CLOSED.** `contentRoot` derived from module NAMES, so `composition` and `composition_trusted_helper` — different programs, same declared name and module inventory — collapsed to one package identity. Both producers now bind module CONTENT (`packageIdentityOf` from `allSrcMap`, `syntheticForModules` from the file source), digested and sorted by content rather than path so ordering cannot reintroduce checkout dependence. Fixing only the project path was insufficient: these fixtures compile through the STANDALONE path, and the second producer had the same defect. Drift is now caught by BOTH package and implementation components.

**A refusal I manufactured, corrected:** `HmacSha256.Proofs.shaFns` was reported SURPLUS because the table-name character class excluded DIGITS. A refusal produced by the checker rather than found in the data is worse than none, since it reads as a finding.

**GATED AND MUTATION-COVERED 2026-08-14.** `check_attestation_manifest.sh` (11/0, `make test-attestation-manifest`) pins the manifest on the real corpus with EXACT values — a rising mapping count is not automatically good, since it can mean a drift fixture stopped being excluded. Asserts: generator completes without refusing; every subject row accounted for exactly once (86 = 0 + 38 + 48); duplicates/collapses/surplus each zero; drift excluded BY NAME; reuse represented; the `proof_pressure` misattachment still visible.

**A THIRD UNDERCOUNT FROM THE SAME DEFECT, found while writing the gate:** I asserted `FnTable.empty` maps to 8 packages, from the `#[proof_by]`-only map. It is **10** — `evidence_classes/proved_by_lean` and `hmac_sha256` reach it through in-repo theorems. The gate now also asserts the stronger property: 10 consuming fixtures ↔ 10 distinct package identities, ONE-TO-ONE, which would have caught the package collapse directly rather than through a downstream symptom.

**Mutations KILLED:** `manifest-drift-classified` (breaks the header match, letting a drifted implementation be attested — the hazard that actually occurred) and `package-identity-binds-content` (reverts `contentRoot` to names). The second was first written INVALID — it left a binding unused and failed on a lint — and the harness reported that rather than counting it as coverage.

**RECOMPUTATION AND MUTATIONS LANDED 2026-08-15.** The gate now asserts the INVARIANT first — every consuming fixture yields exactly one scoped package identity, none shared — with 10 pinned separately and explicitly labelled as today's corpus denominator rather than architectural truth. Recomputation added: a second derivation must be byte-identical, since the manifest is derived rather than stored and nondeterminism would leave it well-formed while attesting different identities per run.

**Five manifest/provenance mutations KILLED:** `manifest-drift-classified`, `package-identity-binds-content`, `manifest-no-silent-filtering`, `classification-union-both-populations`, `manifest-surplus-path-live`.

**TWO MUTATIONS SURVIVED FIRST AND WERE RETARGETED — both survivals were findings.** Dropping `ensures_proof` from the fixture scan could never kill, because ZERO fixtures use it without `proof_by`; the union of the two populations actually happens in `scripts/gen/classifications.lean`, so the mutation moved there. Deleting the surplus check survived because surplus is 0 in this corpus — removing a check with no live case is undetectable — so the mutation now breaks the check's INPUT to force the path to execute.

**A REAL CONSEQUENCE CAUGHT BY A GATE:** `FnTable` gaining `attested`/`attestationFailures` changed its TYPE, so theorems mentioning it re-elaborated and their table digests moved (`proofFns` 6fe095a9… → e41b73d6…). CLASSIFICATION-FRESHNESS went red and the table was regenerated; theorem artifact digests were unchanged, which is the expected shape.

**PACKAGE 1 COMPLETE 2026-08-15 — the manifest is finished and frozen.** ATTESTATION-MANIFEST 13/0, seven mutation families killed: `manifest-drift-classified`, `package-identity-binds-content`, `manifest-no-silent-filtering`, `classification-union-both-populations`, `manifest-surplus-path-live`, `manifest-duplicate-path-live`, `manifest-key-includes-package`. Reuse POSITIVE CONTROL in place — a reused model is accepted across **13 attestation rows spread over 10 consuming fixtures and 10 scoped package identities**, so the refusals are not holding because everything is refused.

**A CHECK THAT HAD GONE SILENTLY INERT, found by a survived mutation.** The conflict detection reconstructed a key string and grepped for it; when the key's field set changed the grep matched nothing and the check reported NO CONFLICTS — answering "no problems" after it had stopped looking. Rewritten to group with awk over the whole key in one pass, so there is nothing to fall out of alignment. Three of the seven mutations survived on first write, and every survival was a finding about the check rather than a bad target: `ensures_proof` selects no unique fixture, surplus has no live corpus case, and the conflict key was reconstructed rather than grouped.

**PACKAGE 2 — the authority transition, CLOSED 2026-08-16.** This is where authoritative completion actually moved: apply attestations via `withAttestations`, atomically replace the join with `DefinitionIdentity`, delete the `CallableId` path with no fallback, repair `proof_pressure` only after the scoped collision controls pass, reach honest 10/10, require correspondence for root usability, and wire verdict composition. Re-run the exact 86-subject/48-manifest-backed-row denominator before and after conversion.

**STEP 1 LANDED 2026-08-15 — `cryptoFns` is the first table bound to its source implementations.** Attestations applied WITHOUT touching the join: `withAttestations` adds evidence material only, nothing reads it yet, and correspondence stays 9/10 + 1 no-claim. FIVE attestations for FOUR models — `check_nonce` is attested TWICE, once per consuming package (`crypto_verify` and `proof_pressure`). That is not a duplicate; the identities differ in their package component, and collapsing them to one is exactly what scoped identity exists to prevent. Verified: `scopedEntryEvidence cryptoFns` returns 5 entries. References are GENERATED, never transcribed — `scripts/gen/attestation_refs.sh` emits 48 opaque typed symbols from the manifest, one per (table, package, declaration), and the table site selects them.

**A PROOF BROKE, AND THAT WAS THE POINT OF DOING THIS FIRST.** `CryptoVerify/Proofs.lean:101` failed with unsolved goals: the corpus reduces through tables with `simp only [eval, cryptoFns_globals, …]`, and a structure update blocks that reduction. The claim that `withAttestations` preserves behaviour was true of the VALUES and false of the REDUCTION. Fixed with `@[simp]` projection lemmas (`withAttestations_globals`, `withAttestations_entries`, both `rfl`), so proofs whose statements never changed still elaborate. Had this been found during the atomic join flip it would have been entangled with the flip instead of isolated here — which is the argument for applying attestations as a separate step.

**Two consequences, named rather than absorbed.** (1) The classification table went stale, because attesting changes the table's VALUE and its digest moves; regenerated, with theorem artifact digests unchanged, the expected shape. (2) The corpus theorem split moved 166 → 168 body, which is exactly the two projection lemmas: they mention `FnTable` so the classifier counts them, and they are `rfl` projections asserting nothing about any implementation. Pin updated with that reason.

**AN EMPTY TABLE IS VACUOUSLY ATTESTED, NOT LEGACY** — found while preparing the application. `FnTable.empty` has NO ENTRIES, so there is nothing to attest and membership is empty for every callee: a complete answer, not a missing one. `scopedEntryEvidence` was treating it as `legacyUnattested`, which would have refused every subject naming it — and TEN packages share that single constant, which is also why it cannot be attested per-package (one shared `def` cannot carry ten different scoped identities). **The "thirteen packages" in the step-1 commit messages and the source comment was a miscount corrected 2026-08-15: 13 is the number of attestation ROWS, spread over 10 fixtures and 10 distinct scoped package identities. The argument is unaffected — 10 > 1 — but the gate has always measured both separately (`EMPTY_PKGS` 10, rows 13), so the prose was the only thing that conflated them.** The emptiness test is ordered BEFORE the attestation test, and the distinction is the point: a table with entries and no attestations is LEGACY (it describes definitions but cannot say which), while a table with neither is EMPTY (it describes none). Both directions gated, so the exemption is about emptiness rather than about missing attestations.

**Invariants held across step 1:** manifest 86 = 0 + 38 + 48, reuse represented as **13 attestation rows / 10 consuming fixtures / 10 scoped package identities**, correspondence 9/10 + 1 no-claim, globals still dispatch, `cryptoFns` entries unchanged (4). Gates: build clean, DEPENDENCY-EDGES 273/0, ATTESTATION-MANIFEST 13/0, CLASSIFICATION-FRESHNESS 2/0, SUBJECT-FACTS 85/0, IMPL-MANIFEST-GATE 8/0, ONE-PRODUCER 12/0, DOCS-DRIFT 3/0.

**STEP 2 LANDED 2026-08-15 — `elfFns` attested, and every table now reconciles against the manifest.** FIVE attestations for five models. The manifest offers a SIXTH row that is deliberately NOT selected: `elfFns_d7eed943_validate_header` would bind this table's `validateHeaderFn` to `proof_pressure`'s `main.validate_header`, which calls `check_nonce` — a function `elfFns` does not hold. That link IS the known misattachment, and attestation BINDS without checking faithfulness, so selecting the reference would make the table describe a function it does not describe with nothing downstream to catch it.

**THE GAP THAT STEP 1 LEFT: a hand-selected subset is how a conversion under-attests while looking finished.** The table site selects generated symbols by hand, so it can select fewer than the manifest offers — and the result is indistinguishable from a complete conversion: the table reports `attested`, `scopedEntryEvidence` returns rows, every probe passes, and the definitions nobody selected are simply not described. `cryptoFns` was verified by hand in step 1 and NOTHING pinned it. `check_attestation_manifest.sh` now joins the two producers per table: **manifest rows = attestations selected + NAMED exclusions**, each declared exclusion must match a real manifest row (so a stale exclusion cannot mask a gap), unconverted tables must report ZERO rather than a partial set that reads as progress, and the converted set is stated exactly. Mutation `attestation-conversion-complete` drops one of `elfFns`'s five and is **KILLED** by that reconciliation line specifically — verified by applying it by hand and reading the failure: "6 manifest rows, 4 attested, 1 named exclusion(s)".

**A PROOF THAT DELTA-UNFOLDED ITS TABLE.** `validate_header_correct` passed `elfFns` to `simp_all`, so the table's DEFINITION entered its proof term and attesting moved the THEOREM's artifact digest (30dfd7f1… → eafd26fa…), not just the table's. That is the invariant step 1 claimed and this proof was quietly violating. Removed `elfFns` from both simp sets; the proof still closes through the `@[simp] elfFns_globals` projection. **MEASURED, not asserted:** with the table attested the theorem digest is `6dbf4f0a…`, and with the table reverted to unattested it is `6dbf4f0a…` — identical, so the proof term is now independent of the table's evidence material. It was the only proof in the corpus with this shape at the time; `fixedCapacityFns` had the same one, which Package 2 step 3 isolated and closed independently below.

**Corpus split 168 → 167, named:** `Concrete.Proof.elfFns.eq_1`, an equation lemma Lean generates ON DEMAND for a def used by name in a simp set. Nothing forces it into existence now, so the classifier no longer sees it. Nothing was proved less — and it is worth knowing that a pinned count moves for reasons that are not always about the corpus.

**CONVERSION STATE, from the reconciliation itself:** converted = `cryptoFns` (5/5), `elfFns` (5 + 1 named exclusion of 6), `fixedCapacityFns` (4/4); vacuously attested = `FnTable.empty` (13 rows, no entries, nothing to bind); PENDING = `ctTagFns` (3), `parseValidateFns` (3), `combineFns` (7), `shaFns` (7) — all reporting zero and refusing as legacy, which is honest. `proofFns`, `proofFnsExt` and `pureCoreFns` are in NO manifest row at all — **measured 2026-08-15, and the reason is not the one the earlier note implies.** A row needs a fixture SUBJECT that claims the theorem; `examples/thesis_demo/`, whose functions these tables model, carries no `#[proof_by]` or `#[ensures_proof]` annotation whatsoever, so nothing links a subject to them. (The earlier note that generating references for `proofFns` "resolved to `main_drifted.con`" described a different derivation path and should not be read as today's cause.) **Their atomic-flip disposition is now explicit:** they remain evaluable mathematical model tables, but are evidence-ineligible; `scopedEntryEvidence` must return `legacyUnattested`, they cannot justify a body edge or contribute to a dependency root, and no `CallableId` fallback may admit them. A future real subject mapping may move one into the manifest and require a normal attestation conversion.

**Invariants held across step 2:** manifest 86 = 0 + 38 + 48, converted set `cryptoFns`/`elfFns`, correspondence 9/10 + 1 no-claim, `proof_pressure` still refuses with matched=0 missing=1, globals still dispatch. Gates: build clean, DEPENDENCY-EDGES 276/0, ATTESTATION-MANIFEST 24/0, CLASSIFICATION-FRESHNESS 2/0, SUBJECT-FACTS 85/0, IMPL-MANIFEST-GATE 8/0, ONE-PRODUCER 12/0, DOCS-DRIFT 3/0.

**STEP 3 LANDED 2026-08-15 — `fixedCapacityFns` attested, and the predicted delta-unfolding hazard closed in isolation.** Four manifest rows reconcile to four attestations, one consuming package, no reuse and no exclusion. Attesting initially moved only `ring_push_then_contains_correct` (`dc9b2275…` → `0d57d381…`): it was the one proof that passed the bare table to `simp`; the other three already reduced through `fixedCapacityFns_globals` and did not move. Repointing that simp set at the projection lemma preserved the statement and proof. **Measured both ways after the fix:** the theorem digest is `38317276…` with the table attested and `38317276…` with it reverted, while the table digest moves `861f2ef2…` → `fb9fb7ea…`. Mutation `attestation-conversion-complete-fixedcapacity` drops one selected reference and is killed by this table's own reconciliation.

**Corpus split 167 → 166, named:** `Concrete.Proof.fixedCapacityFns.eq_1` is no longer minted. `cryptoFns.eq_1`, `parseValidateFns.eq_1` and `pureCoreFns.eq_1` remain because `by decide` completeness examples in `ProofSoundness.lean` reduce those definitions. A simp set naming a table and a decision procedure reducing it can both mint equation lemmas, but only the former had put evidence material into a corpus theorem proof term.

**Invariants held across step 3:** manifest 86 = 0 + 38 + 48, converted set `cryptoFns`/`elfFns`/`fixedCapacityFns`, correspondence 9/10 + 1 no-claim, entries 4, globals dispatch in both directions, and `scopedEntryEvidence fixedCapacityFns` returns 4/4. Gates: build clean, DEPENDENCY-EDGES 278/0, ATTESTATION-MANIFEST 24/0, CLASSIFICATION-FRESHNESS 2/0, SUBJECT-FACTS 85/0, GATE-MUTATION-COVERAGE 49 families.

**STEP 4 LANDED 2026-08-15 — `parseValidateFns` attested, and the manifest population is NOT the join's population.** THREE attestations for EIGHT entries, and the shortfall is not an omission at the table site: the manifest emits a row per SUBJECT — a declaration some fixture links a proof to — while correspondence asks about the CALLEES of a subject's body. `parse_header` and `validate_header_fields` correspond TODAY (matched=6 and matched=5, usable=yes) over callees — `compute_checksum`, `validate_checksum`, `validate_msg_type`, `validate_payload_len`, `validate_total_len` — that carry no proof link of their own, so no manifest row and no generated reference exists for any of them. The `CallableId` join matches those edges on NAME; **a scoped join requiring an attested entry per callee would stop matching them.**

**MEASURED ACROSS THE CORPUS, not inferred from one table: 14 of 34 distinct (fixture, body-edge callee) pairs have no manifest row** — `hmac_sha256` 8, `parse_validate` 5, `composition_unlinked_helper` 1. Pinned in `check_attestation_manifest.sh` with that breakdown plus a non-vacuity guard on the callee extraction, so the gap cannot grow unnoticed and closing it is visible. **Whether the manifest population should be extended to callees is a flip-design question and is NOT decided here** — but entrance-gate condition 1 ("pending conversion is zero") is necessary and not sufficient: every table can be converted while 14 body edges remain unattestable.

**A CLAIM OF MINE THAT WAS FALSE, corrected.** Step 1 reported "theorem artifact digests unchanged" for the `cryptoFns` conversion. Its own commit shows `verify_message_composed_correct` moved 30dfd7f1-style — precisely 2b1b47d5… → 9d62ecbf… — because `CryptoVerify/Proofs.lean` named `cryptoFns` in a `simp_all` set on a CONTINUATION line, which my line-anchored grep never matched. That is also why step 2 said `elfFns` was the only proof in the corpus with this shape: it was not. Repointed at `cryptoFns_globals`, so the theorem digest is now dbbca262… whether the table is attested (fa03c0e8…) or not (111e0779…) — the step-1 hazard is closed retroactively. The same fix applied to `parseValidateFns` (`validate_header_fields_success`, independent at 071c8a81…).

**THE GREP IS REPLACED BY AN INVARIANT, because it was wrong twice.** A `simp` set naming a table def makes Lean mint that def's equation lemma and lets the table's definitional shape into the proof term; the lemma's PRESENCE is that dependency. `check_dependency_edges.sh` now asserts that no converted table has an `eq_1`/`eq_def`, with `pureCoreFns` — delta-unfolded by `proofs/Examples/PureCore/Proofs.lean` and not manifest-backed — as the live positive control proving the check can see one. The earlier attribution of surviving equation lemmas to the `by decide` completeness examples in `ProofSoundness.lean` was also wrong: `parseValidateFns.eq_1` vanished when its simp site was repointed while its `decide` examples were untouched. Corpus split 166 → 164 accordingly.

**Invariants held across step 4:** manifest 86 = 0 + 38 + 48, correspondence 9/10 + 1 no-claim, entries 8, globals still dispatch, `scopedEntryEvidence parseValidateFns` 3/3. Mutation `attestation-conversion-complete-parsevalidate` KILLED — the reconciliation is against the MANIFEST, not the entry count, so 3 rows vs 2 attested still fails. Gates: build clean, DEPENDENCY-EDGES 281/0, ATTESTATION-MANIFEST 27/0, CLASSIFICATION-FRESHNESS 2/0, GATE-MUTATION-COVERAGE 50 families.

**STEP 5 LANDED 2026-08-15 — `ctTagFns` attested, and a drifted implementation refused.** ONE model, THREE manifest rows, TWO attested. `ctCompareFn` is bound to `constant_time_tag` and to the `demo` module of `evidence_classes/partial_contract`, whose bodies are identical and whose implementation identities differ because contracts are part of the implementation — legitimate reuse, and the sharpest available control that scoped identity has not collapsed to the model (`scopedEntryEvidence ctTagFns` returns 2, not 1). Theorem digests unchanged: those proofs already reduce through `ctTagFns_globals`.

**THE THIRD ROW WOULD HAVE BOUND THE MODEL TO A DRIFTED IMPLEMENTATION.** `examples/evidence_classes/stale_proof` links the same theorem while its body starts `diff` at 1 instead of 0, and the compiler already says so: `--report proof-status` reports SPEC DRIFT, "the theorem is about a different function than the source". Nothing downstream would have caught it — `scopedEntryEvidence` recomputes the digest of the MODEL's body, never the implementation's — so a conversion driven mechanically off the manifest would have baked a drifted identity into the table and looked correct.

**AND THE MANIFEST OFFERS THAT ROW.** Its drift exclusion greps fixture headers for `DRIFTED variant`; this fixture's header says "the body has DRIFTED" and does not match. Prose is a second, weaker producer of a fact the compiler computes. **Not repaired in the manifest — package 1 is frozen and its classifier is a design decision — but the conversion path no longer consumes the miss:** the gate re-derives the drift verdict from the compiler and requires every row from a spec-drifted fixture to be OMITTED at its table site. Mutation `attestation-never-binds-drifted-impl` swaps a legitimate reference for the drifted one (rather than adding it, so the counts still reconcile and only the drift check can fire) — KILLED, verified by hand to fail on exactly that assertion.

**TWO DEFECTS IN MY OWN CHECKS, both found while writing them, both the class this work exists to catch.** (1) The drift scan reported "no fixture reports spec drift" while the same grep matched by hand: `if binary … | grep -q` is wrong under `set -o pipefail`, because the compiler exits non-zero on exactly the fixtures being looked for, so the branch was never taken — a check answering "nothing found" because it stopped looking. (2) The first version asked only whether a row was DECLARED in the exclusion list, which a table could satisfy while also selecting the reference. Declared and omitted are different facts; the site probe now emits what each table actually BOUND and the check reads that.

**A THIRD, in the same hour: the converted-set filter was `grep -vE 'attested=0 '` over the whole site probe**, which worked only because every line began with `SITE`. The same commit added one `BOUND` line per attestation and the comparison broke. `b2a3c5c6` claims ATTESTATION-MANIFEST 30/0; the gate was RED at 29/1 when it was written, because I read a narrowed slice of the output and not the total. Corrected in `a885738e`.

**Invariants held across step 5:** manifest 86 = 0 + 38 + 48, correspondence 9/10 + 1 no-claim, entries 1, one model yields TWO scoped entries. Gates: build clean, DEPENDENCY-EDGES 282/0, ATTESTATION-MANIFEST 31/0, CLASSIFICATION-FRESHNESS 2/0, GATE-MUTATION-COVERAGE 51 families.

**PACKAGE 2 CLOSED 2026-08-16.** Steps 6-11 landed across the authority transition and the sections
below preserve that record. Read this paragraph first, because several statements further down were
written while the work was ahead rather than behind: **every manifest-backed table is converted
(pending = 0); the evidence join, table membership and dependency roots are keyed on
`DefinitionIdentity` with no name-keyed fallback; the `proof_pressure` misattachment is contained;
and scoped correspondence plus computable roots are consumed by production verdicts through the
composed authority pass.** Package 3 owns replay-backed receipts, migration, reproducibility and
adversarial closure; those remaining boundaries do not reopen Package 2.

**STEP 6 — THE DEPENDENCY POPULATION.** The manifest carries two populations separated by `binding_role`. Subject rows are declarations a fixture links a proof to; dependency rows are the requested BODY-edge callees of those subjects, keyed by (consuming subject, table, callee) and produced by the compiler. Keying by (fixture, callee) is too weak and hides the exact defect this work exists to catch — `check_nonce` has a good identity under `cryptoFns` while the theorem claiming `validate_header` requested `elfFns`. The subject section stays byte-identical and `table_attestations` stays subject-only. References are generated from both attestation sections and deduped on (table, package, module, declaration, implementation).

**STEP 7 — MEMBERSHIP PROVENANCE, AND REMOVING THE IGNORANCE FIRST.** "Attested" was one word for three claims. It now carries whether model presence was checked against a COMPILER-LINKED table whose body digests the compiler recomputed, against GENERATOR-ASSERTED rows, or not at all. And `shaFns` stopped being unreadable rather than being labelled unknown: the classification generator's external-table list named `combineFns` and stopped, so adding `shaFns` made ten hmac requests answerable. `dependency_model_unresolved` is 0 on this corpus as a result, and the unreadable-table path is controlled synthetically rather than by a case that no longer exists.

**STEP 8 — EVERY REFERENCE LOAD-BEARING, AND THE LAST TWO TABLES.** `parseValidateFns` binds all eight of its entries, five of them from dependency rows. The reconciliation runs over the DISTINCT REFERENCE SET per table, not over subject rows, and separates three dispositions: BOUND, NAMED EXCLUSION (the table holds that model and deliberately does not bind it), and NO MODEL (nothing could be bound — `combineFns` models `inc` and `dbl`, never `combine` itself). A named exclusion that merely restates a structural fact is refused, so a real misattachment cannot hide among them. `combineFns` and `shaFns` are converted, selecting from `Concrete.Proof.GeneratedAttestations` like every in-build site; **pending conversion is ZERO**, 28 selected + 2 excluded + 13 vacuous + 5 no-model + 0 pending = 48 (later 47, see the repair).

**A CORRECTION THAT SUPERSEDES PART OF STEP 9, recorded here because it changes what the flip is credited with.** Step 9 below says the flip "caught a real cross-program substitution" on `elf_header/main_drifted.con`. **It had not.** That file sat beside `main.con` in one package and both resolve to module `main`, so the project loader picked `main.con` and the drifted bodies were NEVER ANALYZED — reports printed the drifted file's line numbers with the real file's semantics, and its `check_magic` carried the real program's implementation digest rather than its own `b0 == 0` body. The refusal was an artifact of package identity being synthesized from one file's TEXT while another file's BODIES were analyzed.

Both drift fixtures now live in their own packages (`elf_header_drifted`, `crypto_verify_drifted`) and are analyzed as themselves for the first time: each reports exactly the two stale functions its header always claimed. **The cross-program discrimination is real and now measured for the right reason** — the drifted package's `validate_header` corresponds 0/4 while the real one corresponds 4/4 — but the drifted package is caught in production by STALENESS, which is a more specific fact and wins ordering, so the authority pass never sees it.

**STEP 9 — THE ATOMIC FLIP, AND WHAT IT CAUGHT.** `RequestedEdge`, `WitnessTarget`, `EdgeWitness`, `CorrespondenceInput`, both witness refusals and table membership migrated together — a mixed period is exactly what the contract forbids. `RequestedEdge` keeps a `CallableId` as `label`, for diagnosis, never compared. The name-keyed `tableContainsCallee` was renamed `tableHoldsModelNamed` and confined to the manifest's bootstrap question; a gate asserts the two questions genuinely DISAGREE on the corpus, so a silent fallback could not go unnoticed. **`examples/elf_header_drifted/src/main.con` — a different program declaring the same functions — had all four of its body edges justified by `elfFns` under the name join, and none under the scoped one.** Corpus correspondence moved 9/10 → 8/10 for that reason, which is one more program correctly refused. Two things had to be built for it to be honest: excluded records now carry a scoped identity from the same producer, and out-of-build tables' scoped membership crosses as data (`externalScopedEntries`).

**STEP 10 — DEPENDENCY ROOTS, the boundary the flip initially missed.** Found in review: `DepNode` was still keyed on `CallableId`, so a root traversed a closure keyed by source names. `DepNode.id` and its edges are now `DefinitionIdentity`, ordering and serialization use `.canonical`, and an unkeyable edge REFUSES the root per node in the closure. The old code synthesized a fake `CallableId.ofUser "«unresolved»"` so a refusal could name its target; that is impossible for a `DefinitionIdentity`, which is the better outcome — the edge goes to `DepNode.unscoped` and the refusal names it. Measured: 77 of 91 subjects root, 14 refuse.

**THE `proof_pressure` REPAIR IS CONTAINMENT, NOT CLOSURE, and the distinction matters.** Its `validate_header` claimed `Examples.ElfHeader.Proofs.validate_header_correct`, a theorem about a different function that shares the qualified name. The claim was DELETED, because no theorem proves that body and attaching the nearest plausible one is the defect being repaired. That is honest, and it shrinks the denominator rather than supplying a valid replacement proof: **the closed end state for that function is a theorem about the body it actually has.** Consequences, each pinned: subject accounting 86 = 0 + 39 + 47; dependency requests 41 = 41 + 0 (the single `tableModelMissing` refusal existed only because of the misattachment, and its path is now controlled synthetically); correspondence 8 of 9 claiming subjects; the `elfFns` named exclusion removed because the manifest no longer offers that row.

**TWO REVIEW FINDINGS FIXED, both the same class — a scoped path paired with an unscoped helper, neither with a live case on this corpus.** Resolver refusals were collected through the unscoped resolver while witnesses came from the scoped one, so a table whose ATTESTATIONS were broken could resolve unscoped, produce no refusal, and leave a subject `usable` with one of its named tables unexamined. And the dependency-row lookup searched only `pc.entries`, so an excluded callee that HAS an identity was reported `calleeWithoutIdentity` — a refusal manufactured by the checker rather than found in the data.

**STEP 11 — CORRESPONDENCE BECAME AUTHORITATIVE 2026-08-15, and it was not vacuous.** `applyCorrespondenceAuthority` runs on the assembled `ProofCore`, so every consumer sees verdicts that already survived their dependency justification rather than each renderer applying its own. A claim may be `proved` only if every edge in its closure is witnessed exactly once by an entry naming that exact implementation.

**IT DISCRIMINATES BETWEEN TWO PROGRAMS THAT SHARE A THEOREM, and the corpus has both sides.** `elf_header/main.con`'s `main.validate_header` is `proved` and STAYS proved — four outgoing body edges, each witnessed by an `elfFns` entry attested to that exact implementation. `main_drifted.con`'s `main.validate_header`, same declaration name and same linked theorem and same table, is downgraded to `correspondence_unjustified`. Measured by building with and without the pass: that fixture reported FIVE `proved` before and FOUR after. **Corpus-wide at the time: 37 proved, exactly one closure unjustified** — the pass is selective, not blunt, and both directions are gated. (SUPERSEDED 2026-08-16: the fixture-package split and the identity fix moved this to **35 proved / 0 unjustified in `examples/`**, with the live control now `tests/programs/proof_decode_header.con`.)

**TWO CORRECTIONS TO THIS RECORD, both from one bad measurement.** The first version of this entry said the pass "changed no verdict on today's corpus" and would be exercised only by controls, because a survey pattern matched `-- proof link unbound` and `-- proof stale` but not `-- proved` — no space after "proof" — and read "nothing is proved". The corpus holds 37 proved verdicts. So the pass is not vacuous, and its POSITIVE direction has a live case the earlier wording implied it did not.

`correspondence_unjustified` is its own status through every surface — canonical string, currency predicate, diagnostics, report rendering, traceability evidence, obligation ledger kind, release policy — and is NOT current for dependents, so an unjustified closure cannot become the foundation of a second one. `require-proofs` has an explicit arm, because the subject is fresh and the callees are current, which is exactly the shape a catch-all waves through.

**STEP 12 — ROOTS BECAME A CONSUMED DIMENSION, and wiring them found a defect immediately.** `applyCorrespondenceAuthority` now requires both: every edge justified AND the closure computable. `composition_trusted_helper`'s `calls.combine` corresponds perfectly and could not root, because its edge to the TRUSTED helper `calls.dbl` resolved to an identity with no NODE — `dependencyNodesOf` mapped over entries only. Correspondence learned to consult both populations when this defect hit it; the root layer never did. **TRUSTED** exclusions now get nodes (digest from their fingerprint, since an excluded definition has no proof subject) and are leaves, because a definition with no theorem has nothing to type its edges. **Corrected 2026-08-16 after review:** the first version gave nodes to ALL excluded definitions, which made closures computable over callees excluded for reasons carrying no evidence at all — an ineligible callee is simply unprovable, and a closure over it must refuse rather than serialize. The restriction costs nothing measured (13 root refusals either way) and is asserted by a probe that CALLS `dependencyNodesOf` with one trusted and one ineligible exclusion, rather than by reasoning about `isCurrentForDependents`. Roots: 13 refusing.

**THE ROOT CONJUNCT IS CURRENTLY REDUNDANT, AND THAT IS PINNED.** Measured before and after: identical verdicts either way (37/1 when first measured, 35/0 after the fixture-package split), because every subject whose root refuses is already not `proved` or already refused by correspondence. It is consumed anyway — it checks duplicate identities, absent digests and unkeyable edges, which correspondence never asks about — and the redundancy is asserted so a corpus that starts exercising it gets looked at.

**A SECOND VERDICT MOVED, in a place the census could not see.** `tests/programs/proof_decode_header.con` is outside `examples/`, so the corpus survey missed it. Its `decode_header` is now `correspondence_unjustified`: the hardcoded proof link names `proofFnsExt`, a table with ZERO canonical entries, which can describe no definitions and therefore cannot state that the theorem is about THIS program's `parse_byte`. Same class of gap as the drifted program, arriving from the other direction — an unmigrated table rather than a substituted implementation. **`tests/programs/` is deliberately outside the attested population**: compiler test inputs, not evidence-corpus fixtures. Closing it would be a population change, not a fix.

**PACKAGE 2 IS CLOSED 2026-08-16.** Delivered: the identity boundary (evidence join, table membership and dependency roots all keyed on `DefinitionIdentity`, no name-keyed fallback), the correspondence authority, the root dimension, and one package-identity producer across every CLI path. Measured at closure: 35 proved / 0 unjustified in `examples/`, 13 subject roots refusing with none of them proved, 86 = 0 + 39 + 47 subject rows, 41 dependency requests all attested.

**WHAT IS STILL OPEN, stated plainly:** the structural receipt binds root/artifact/trust material,
but no production kernel replay mints or authorizes one and the corpus has not been migrated. Exact
44-link migration/V2 activation, repository-root and fresh-checkout replay
or explicit deferral, the externally blocked ninth table, and Slice 8 red-teaming are Package 3. The
35/91 coverage baseline and local path-independent material are closed. The root conjunct is consumed
but redundant on this corpus; `proof_pressure.validate_header` is contained rather than closed (a
false claim deleted, not a valid proof supplied); and the authority pass's only live corpus control
lives in `tests/programs/`, which is deliberately outside the attested population.

**CONVERSION LEDGER (authoritative owner: `scripts/tests/check_attestation_manifest.sh`).** The roadmap no longer maintains independent counts for table conversion. A table is exactly one of: converted, vacuously complete, pending, no manifest row, or externally blocked. The gate's `attestationSites` list is the sole source of build-visible sites.

| table | manifest rows | `FnTable.entries` | status | notes |
|---|---|---|---|---|
| `Concrete.Proof.FnTable.empty` | 13 refs | 0 | vacuously complete | 10 consuming fixtures / 10 scoped package identities; nothing to bind |
| `Concrete.Proof.proofFns` | 0 | 2 | no manifest row | no fixture subject claims these theorems |
| `Concrete.Proof.proofFnsExt` | 0 | 0 | no manifest row | no fixture subject claims these theorems |
| `Concrete.Proof.pureCoreFns` | 0 | 0 | no manifest row | no fixture subject claims these theorems |
| `Concrete.Proof.cryptoFns` | 5 refs | 4 | converted | `check_nonce` attested twice (two consuming packages) |
| `Concrete.Proof.elfFns` | 5 refs | 5 | converted | all five bound. The `proof_pressure` exclusion is GONE — that fixture's misattached `#[proof_by]` was deleted, so the manifest no longer offers the row |
| `Concrete.Proof.parseValidateFns` | 8 refs | 8 | converted | all eight bound: three from subject rows, five from DEPENDENCY rows |
| `Concrete.Proof.fixedCapacityFns` | 4 refs | 4 | converted | same proof-term independence fix as `elfFns`: `ring_push_then_contains_correct` no longer delta-unfolds the table |
| `Concrete.Proof.ctTagFns` | 3 refs | 1 | converted | 2 attestations of ONE model in two packages + 1 named exclusion: `evidence_classes/stale_proof` is a DRIFTED implementation the compiler reports as spec drift |
| `Examples.ProofPatterns.Proofs.combineFns` | 8 refs | 2 | **converted** | 5 bound + 3 with no model in this table (`calls.combine` is the subject, not a callee its proof unfolds). Selects from `Concrete.Proof.GeneratedAttestations` like every in-build site — no second producer under `proofs/` |
| `Examples.HmacSha256.Proofs.shaFns` | 17 refs | 16 | **converted** | 15 bound + 2 with no model; `rotr` is the reverse case — a model no request references, which is not a gap |

Denominators (measured by the gate, not maintained separately in prose):
- **11** build-visible attestation sites (`attestationSites`).
- **41** dependency requests = 41 attested + 0 refused, keyed by (consumer, table, callee).
- **63** generated references, deduped from 88 attestation rows across both populations.
- **48** manifest-backed table rows (`table_attestations`), accounting for the 86 subject rows. Conversion accounting is now COMPUTED BY THE GATE from the rows, not maintained here: after `ctTagFns` it is **19 selected + 2 named exclusions + 13 vacuous-empty + 14 pending = 48**. (Two prose figures were wrong before the check existed: `14 + 1 + 13 + 20` was recorded after `parseValidateFns` when the measurement was `17 + 1 + 13 + 17`. That is why the four are derived rather than written.) At atomic entrance it must be **32 selected + 2 named exclusions + 13 vacuous-empty + 0 pending = 48**.
- **42** proof-table entries (sum of `FnTable.entries.size` across the 11 sites).
- **29** proof-model literals (`PFnDef`) remain mathematical models by design. This is an inventory, not atomic-flip edit work: exact identity lives in companion attestations and at evidence-consumer boundaries.

**ATOMIC-FLIP ENTRANCE GATE.** Before the `CallableId` join is replaced by `DefinitionIdentity`, the following must hold:
1. Pending conversion is **zero**. Every nonempty manifest-backed table is in the converted set and reconciles as manifest rows = attestations selected + named exclusions.
2. The exact manifest snapshot is unchanged: **86 subject rows = 0 no-identity + 38 without table + 48 manifest-backed**, with the conversion split exactly **32 selected + 2 named exclusions + 13 vacuous-empty + 0 pending**, reconciled by the gate rather than restated.
3. No-manifest tables (`proofFns`, `proofFnsExt`, `pureCoreFns`) remain explicitly evidence-ineligible: they evaluate, but `legacyUnattested` prevents them from justifying an edge or entering a root, with no name-keyed fallback. A positive control keeps their evaluation usable; a negative control proves they cannot supply evidence.
4. Correspondence and roots are unmoved from their measured state: **35 proved / 0 unjustified** across the fixture corpus, 13 subject roots refusing and none of them `proved`. (This read "9/10 claiming subjects, roots 62/64" until 2026-08-16; both figures predated the identity migration, the fixture-package split and the authority pass.)
5. Scoped package/implementation collision controls pass (no shared package identity, no duplicate scoped definitions).
6. A dedicated executable completion assertion checks items 1–3 and is mutation-killed by restoring any pending table, partially selecting a table, or admitting a no-manifest table. The ordinary `check_attestation_manifest.sh` progress reconciliation is deliberately green while pending tables honestly report zero, so its current success alone is NOT the entrance signal.
7. The ordinary attestation-manifest, dependency-edge, classification-freshness, subject-facts, implementation-manifest and one-producer gates remain green.
8. **Every compiler-produced DEPENDENCY REQUEST ends in an exact attestation or a named refusal**, reconciled as `dependency_requests = dependency_attestations + dependency_refusals` (today 42 = 41 + 1). Conditions 1–2 are about SUBJECT rows and are necessary but not sufficient: every table can be converted while body edges remain unattestable. The request denominator is the AUTHORITY here; the "34 fixture/callee pairs" figure is a useful tripwire and nothing more, because keying by (fixture, callee) hides a callee that is a proof-linked subject under one table and an unattestable edge under another — which is exactly `check_nonce`.
9. **Removing any one dependency attestation makes the scoped correspondence join REFUSE.** A reference set that is present but not load-bearing would satisfy condition 8 without carrying it.
10. **The 86/48 subject denominator is unchanged by the new population.** Dependency rows must not inflate the subject accounting.
11. **No converted table has an equation lemma** (`check_dependency_edges.sh`), so no proof term depends on a table's definitional shape. This stays part of EVERY conversion, not just the ones where a digest happened to move.

**MANIFEST POPULATION — DECIDED 2026-08-15, and it is the design blocker `parseValidateFns` exposed.** The manifest carries TWO populations, explicitly separated by a ROLE field so a dependency row can never masquerade as a proof-linked subject:

| population | contents | accounting |
|---|---|---|
| **subject rows** (`binding_role=subject`) | a declaration some fixture links a proof to | the FROZEN 86/48 accounting, byte-identical output |
| **dependency rows** (`binding_role=dependency`) | one request per (consuming subject, table, requested body-edge callee), each ending in an exact attestation or a named refusal | separate counters: `dependency_requests` 42 = `dependency_attestations` 41 + `dependency_refusals` 1, plus duplicate/conflict/without-identity, all 0 |

**LANDED 2026-08-15.** `--report attestation-join` emits both populations; `[dependency-attestations]` and `[dependency-refusals]` are separate manifest sections; references are generated from both and deduped on (table, package, module, declaration, implementation) — 89 rows collapse to 64 symbols. **The earlier "14 uncovered of 34" figure is superseded and was measured with a weak key:** it counted (fixture, callee) pairs in shell, which both re-derived a compiler fact and read 14 where the (consumer, table, callee) answer is 17. That shell measurement is deleted rather than kept alongside.

**The dimensions stay separate.** Exact selection of an implementation is not proof linkage, is not model faithfulness, and is not root availability. Concretely: giving `composition_unlinked_helper`'s callee an exact scoped reference must NOT make its missing proof node usable — that fixture exists to have an unlinked helper, and a dependency reference says "this is exactly which implementation the edge points at", never "this edge is justified". **Measured, not asserted:** `calls.dbl` IS attested and `proof_pressure/validate_header` still reports `matched=0 missing=1`, both gated.

**AND A TABLE THAT CANNOT BE READ IS NOT A TABLE THAT REFUSES.** `tableContainsCallee` answers `false` only for a resolvable table. `elfFns` resolves and holds no `check_nonce` model, so that request is a named `tableModelMissing` refusal. `shaFns` is out-of-build with no external rows and cannot tell, so its requests are attested — refusing on the compiler's own reach would manufacture a refusal from the data's absence, and the identity claim is about the CALLEE either way.

**`combineFns` and `shaFns` are NOT to be converted until this population exists.** They are the two out-of-build tables and carry 14 of the remaining rows, and `shaFns` alone carries 8 of the 14 uncovered callee pairs; converting them first would mean converting against the population that is about to change. When they are converted they select from `Concrete.Proof.GeneratedAttestations` exactly as the in-build sites do — `proofs/` already imports `Concrete.Proof.Proof`, so the dependency direction is valid, and **no second generated file or producer may be created under `proofs/`**, which would reintroduce the two-producer defect R-0004 exists to remove.

**THEN PACKAGE 3 — durability:** replay-backed receipts, 44-link migration, coverage baseline, reproducibility, Slice 8.

### Superseded Package-1 investigation

The following material is preserved as history. It records the investigation that produced the frozen manifest; the conditions it lists are now satisfied or superseded by the ledger above.

~~Earlier status:~~ generator started deriving mappings from BOTH populations and classifies drift by the fixture's own header (not filename), so a renamed drift fixture stays classified. Three drift fixtures found (`crypto_verify`, `elf_header`, `thesis_demo`), each emitted as `EXCLUDED` WITH the theorem that reached it. Distinguishes classification rows from EXTERNAL table rows — conflating them is how `combineFns`, a table, first appeared in a list of theorems. The three in-repo theorems my earlier map missed are now mapped, and the known misattachment is visible: `proof_pressure` maps to BOTH `cryptoFns` and `elfFns`.

~~**THREE ACCEPTANCE CONDITIONS NOT YET MET**, and the run passing does not imply them:~~
- ~~**The denominator does not RECONCILE.** It reports `source_linked_theorems = 31` against 43 unique source-linked names; the gap is theorems in `.con` files outside any directory holding a `Concrete.toml`. Reporting two numbers is not reconciling them.~~ Now reconciled: every subject row is accounted for exactly once.
- ~~**In-repo theorems have no package.** `proofFns`, `proofFnsExt` and `FnTable.empty` map only to in-repo theorems, so no source package is determined. The manifest states this rather than guessing, but those tables cannot be converted until it is resolved — and `FnTable.empty` is the 8-package table, so it is the highest-risk one.~~ The manifest now states this explicitly: `proofFns`, `proofFnsExt`, and `pureCoreFns` have no manifest row because no fixture subject claims them; `FnTable.empty` is shared by **10 consuming fixtures and 10 scoped package identities**.
- ~~**Only empty-population refusals exist.** Duplicate, ambiguous, conflicting and surplus mappings are not refused, no mutation control exists, and identities are not yet recomputed against current compiler facts.~~ Now refused and mutation-controlled.

~~**ORIGINAL ACCEPTANCE CONDITIONS, ratified 2026-08-14 (unchanged, still governing):**~~ These are now the operational invariants of the frozen manifest and are enforced by `check_attestation_manifest.sh`. They require an exact manifest keyed by TABLE and SCOPED IMPLEMENTATION, derived from BOTH attachment populations, that:
- states a source mapping for every evidence-bearing table;
- represents every legitimate (model, package, implementation) reuse — `FnTable.empty` serves **10 fixtures/packages**, `combineFns` 3;
- classifies drift fixtures as named exclusions/refusals;
- refuses on missing, duplicate, ambiguous, conflicting and surplus mappings;
- never selects a mapping because a callable name matches;
- recomputes generated identity against current compiler facts;
- fails the corpus gate if either attachment population is removed;
- is mutation-killed by substituting `main_drifted.con`, another package, or another same-named implementation;
- carries a self-declared denominator reconciled against both source inventories;
- and preserves table entries, ordering, `globals` and existing theorem statements byte-for-byte where applicable.

**REMAINING for the atomic join transition:** satisfy the entrance gate above, convert the 29 proof-model sites, migrate `RequestedEdge`/`EdgeWitness`/table membership/`DepNode` to `DefinitionIdentity` in the SAME change, delete the `CallableId` join with no fallback, then the eight scope/attestation mutations, the cross-project fixture, `proof_pressure` repair, and 10/10. ~~Earlier note:~~ step 4b is a DIFFERENT SHAPE than "migrate five types together" — measured 2026-08-14.

**`PFnDef` cannot supply a `DefinitionIdentity`, and cannot derive one.** It carries `identity : PFnIdentity` (a `CallableId`) and `sourceBodyDigest` — the V1 BODY digest, which is precisely the digest ruled insufficient for the implementation component. No package identity, no implementation identity. 29 literals exist (27 in `Concrete/Proof/Proof.lean`, 2 in `proofs/`).

**And a proof-model entry cannot COMPUTE an implementation digest.** `implementationDigest` needs `CheckedDeclFacts` + `CompleteEvidenceBodyV2` — COMPILER facts about the real function (typed signature, capabilities, generics, contracts). A `PFnDef` is a `PExpr` proof model; it has none of that material and never will.

**So the design is ATTESTATION plus RECOMPUTATION, not derivation.** A table entry must RECORD the compiler's authoritative implementation digest and its package/module/declaration scope, and the compiler must recompute the implementation digest from its own facts and COMPARE. A mismatch means the table describes a different implementation — which is exactly the exact-identity join, and it makes table entries `generatorAsserted` for that component in the same sense external rows already are for body digests, with the compiler's recomputation as the check.

**Superseded consequence for sequencing:** an earlier design expected all 29 `PFnDef` literals to change. `withAttestations` dissolved that requirement: the literals remain mathematical models, and generated implementation references sit beside them. The atomic change instead migrates `RequestedEdge`, `EdgeWitness`, table membership and `DepNode` evidence equality to `DefinitionIdentity` together, after every manifest-backed table is converted; no mixed old/new join is permitted.

The “STEP 3” label in the historical row below belongs to the earlier identity-design sequence; it is not Package 2 step 3 (`fixedCapacityFns`) recorded above.

| **STEP 3 LANDED 2026-08-14: `PackageIdentity` is REQUIRED at the ProofCore boundary.** `ProofCore.packageIdentity` is a required field, `extractProofCore` takes it non-defaulted, and `extractProofCore?` returns `Except ProofIdentityRefusal ProofCore` for contexts that cannot supply one. **EIGHT call sites, not six** — my inventory was wrong twice; the type error found `Main.lean:2928`, `2945`, `3412` after I had recorded only `454` and `1518`. One project path (`Resolve/Project.lean`, manifest-derived, computation moved above the `pc` binding and the duplicate deleted; a manifest with no declared name now REFUSES THE LOAD) and seven standalone paths using `PackageIdentity.syntheticForModules`, one producer for all of them. `DebugBundle` maps a refusal to `proofCore := none`, which its type already modelled — I nearly added a `mkUnidentifiedForDiagnosticsOnly` back door to keep the field populated and reverted it before it compiled, since it would have scoped every unidentified compilation by one shared value. `Reduce` answers "not violated" on refusal. **Measured unchanged: correspondence 9/10, roots 62/64, manifest 64/63/1 — requiring the identity moved no verdict.** ~~Stale inventory~~ (measured 2026-08-14 — the split is 1 vs 4, not 6 uniform sites.** `Concrete/Resolve/Project.lean:391` is the ONLY site inside `loadProject`, so it is the only one with manifest material; note `packageIdentity` is currently computed at the return statement and must move ABOVE the `pc` binding. The other four are standalone single-file paths with no manifest in scope and therefore need `PackageIdentity.synthetic` over canonical module/content material: `Main.lean:454`, `Main.lean:1518`, `Concrete/Report/DebugBundle.lean:276`, `Concrete/Report/Reduce.lean:140` (this last one passes `[] []` for locMap and registry, so it is the most degenerate context and the best test of whether synthetic identity is genuinely derivable there). Required shape: `extractProofCore` takes a non-optional `PackageIdentity`; `extractProofCore?` takes the `Except` and returns `Except ProofIdentityRefusal ProofCore`. Original contract, still governing the remaining steps:  do NOT add `packageIdentity : String := ""`. A default lets missed call sites compile and turns an integration omission into a late refusal. Introduce a private, non-empty `PackageIdentity`; compute it once in project loading and require it at all six `extractProofCore` call sites. A genuinely unavailable identity is a typed extraction refusal, not an empty string. Standalone files receive an explicit deterministic synthetic identity derived from canonical package/module content, never a checkout path or generic `main`. The identity binds declared package name plus a stable namespace/origin when available and a canonical module/content root, since a name alone is not globally unique. Then migrate table entries, requested edges, witnesses, roots and receipts together to `DefinitionIdentity`; forbid mixed old/new joins; convert legacy/name-only evidence to `needs_recheck`; mutation-test removal of package and implementation scope separately; only then repair `proof_pressure` and require correspondence for root usability |
| ~~DECISION NEEDED — cross-program identity scoping~~ | `CallableId` carries `defModule` + `declName` but no program/workspace identity, so `v1:user:main.validate_header` denotes different functions in `elf_header` and `proof_pressure`. Within one compilation that is unambiguous; the Lean-side proof TABLES are shared across fixtures, so a table describing one program's `main.check_magic` could match another program's edge to `main.check_magic`. **No such false justification exists in the corpus today** — the observed case refuses because the callee is absent from the table — but the collision is structurally possible and it is a trust-model question, not a bounded implementation choice. **DEFERRED to R-0482; it does NOT gate R-0004.** R-0004 guarantees exact selection relative to declared/content identity, not globally unique publisher origin; no refusal can distinguish two inputs whose identity bytes are identical. Current controls establish that today's corpus has no such collision. R-0482 owns the manifest-surface origin/namespace decision | decide in R-0482 whether proof tables are program-scoped, and if so bind a program/workspace identity into `CallableId` or into table identity |
| ~~correspondence 8/11 as of 2026-08-13~~ | Was 6/11 before out-of-build entries crossed as data. Three remain, each owned by a different layer: `fixed_capacity.validate_message` (theorem unclassified — upstream of correspondence), one `calls.combine` fixture (UNRESOLVED callee in the compiler graph, not a table problem — my classification had put all three `calls.combine` subjects under the `combineFns` cause and two of three was right), `main.validate_header` (its theorem names a table lacking the callee). Surplus 0 throughout. Kept distinct from roots 62/64 | 11/11 for defensible reasons, then root usability, then verdict composition |
| ~~the 5 non-corresponding subjects, classified 2026-08-13~~ | **Superseded by 2026-08-14 measurements.** This classification drove the `combineFns`/`fixed_capacity.validate_message`/`main.validate_header` breakdown. By 2026-08-14 the `calls.dbl` unresolved-identity fix and the `no claim` reclassification moved correspondence to **9/10 claiming subjects + 1 no-claim**, with `main.validate_header` correctly refusing because its theorem names `elfFns` while the implementation lives in `cryptoFns`. Kept as history; do not use it as the current diagnosis | historical only |
| **dispatch is security-relevant inventory — 4 of 5 controls in place** | HAS: one canonical owner (`TableResolve.lean`), completeness against every hand-back name (8/9, gated), duplicate-name refusal (gated), and **stable table identity verification** (`entryTableDigest` landed 2026-08-14 — generator and compiler call the same canonical function over entry identities and body digests). **MISSING: a mutation proving a removed/misrouted entry makes the subject refuse.** The dispatch itself is correct; the remaining gap is a negative control that deliberately routes a table to the wrong callee and checks that correspondence reports it | register the misroute mutation |
| the two denominators | **the same 64, agreeing FILE BY FILE across all 20 sources.** Both derive from `pc.entries`, so this is a divergence tripwire rather than independent confirmation — its value is catching a later filter on either side, which totals alone would miss (one file gaining an identity while another loses one keeps the sum at 64) | — |
| dependency-aware verdict composition | **LANDED 2026-08-15.** The prohibition was conditional on the authority-transition gate, and that gate (`make test-atomic-flip-entrance`, 13/0) passes. One composed pass downgrades an unjustified or uncomputable closure to `correspondence_unjustified`, propagates to callers, recomputes proved-callees and regenerates diagnostics. | keep INV PROVED-ROOTS green; it is the invariant the root conjunct enforces |
| replay receipts and corpus migration | **SHIPPED 2026-08-17** | historical prerequisite satisfied before closure |
| adversarial closure | **SHIPPED 24/0** | Slice 8 and the independent packet completed before closure |
| ninth proof table | **DEFERRED past Slice 8 2026-08-16, and OFF R-0004's closure list** | It could not remain both frozen and mandatory: the required extraction changes proof-model expressiveness (`packet.decode_header` holds a `let mut cur: ByteCursor` and mutates it ~10 times, so value-state extraction means threading cursor state through `PExpr`, which has no representation for a call returning a value alongside a new state), and the dependency order freezes evidence-affecting changes until Slice 8 closes. The three `proofFnsExt` entries are marked unsupported and the model extension is deferred. **That is only honest because the entries carry no authority, and `check_table_migration.sh` MEASURES it** (133/0): `proofFnsExt` has empty scoped membership, so it describes no definitions and justifies nothing; and no fingerprinted claim in the corpus depends on it. Two independent legs, because either alone could hold for the wrong reason. The extraction tripwire remains and distinguishes three outcomes, so an ABSENT `decode_header` fails as a lost subject rather than reading as "it became extractable". |

The legacy slices map to outcomes only: slices 1–3 cover the landed containment foundation;
slice 4 covers proof-table, receipt-envelope and environment plumbing; slice 5 covers subject
identity; slice 6 covers dependency correspondence and roots; slice 7 covers replay, receipts and
migration; slice 8 covers adversarial closure. Slice numbers are traceability labels, not a second
progress model.

### Open boundary: report and process disposition integrity

**Status:** OPEN. This is part of the authority transition, not CLI polish. A structured payload,
stderr diagnostic and process exit code are three projections of one disposition; disagreement
lets automation accept a result that the report itself says was not computed, or lets a gate pass
by reading partial stdout from a failed producer.

**Required repairs and controls:**

- every recognized `--report` branch returns explicitly after producing its payload; in
  particular `impl-manifest` must not fall through to the unknown-report branch;
- corpus/report gates capture the producer's exit status and require the expected value before
  parsing stdout. `2>/dev/null || true` is forbidden on an evidence-producing command unless the
  test is explicitly asserting failure and separately validates the failure class;
- `no_obligation_record`, `not_computed`, malformed input and every other out-of-domain marker map
  to an explicit non-success process disposition. A catch-all must never turn a new refusal into
  `ExitCode.ok`;
- text, JSON, artifact emission and process status are checked from the same negative fixture, so
  one projection cannot be corrected while another remains fail-open;
- unknown report names still fail, providing the over-acceptance control for recognized reports.

**Acceptance:** `--report impl-manifest` emits exactly one manifest result, no unknown-report
diagnostic, and exits 0; deleting its explicit return is mutation-killed. A prove request whose
obligation lookup has no record emits the typed refusal in text/JSON/artifact forms and exits
nonzero; mapping that refusal through the success catch-all is mutation-killed. No gate can become
green from payload text produced by a command whose exit status it discarded.

### Open boundary: exact refusal-path controls

**Status:** PARTIAL. Representation and fail-closed behavior are separate claims. Adding a named
constructor proves neither that production can reach it nor that its consumer preserves the
refusal. Synthetic code that merely resembles the lookup is not a control for the production path.

**Required shape:** extract the pure lookup over supplied raw rows (for example
`validatedRowIn rows theorem`) and make `validatedRowOf` only the private generated-table wrapper.
Tests then inject absent, one malformed, identical duplicates and conflicting duplicates through
that exact lookup, assert the precise refusal and ambiguity count, and assert the downstream edge
remains `unclassified`. Include a valid singleton as the over-rejection control.

**Acceptance:** removing duplicate detection, discarding the ambiguity count, first-match lookup,
or mapping any refusal to a current edge is killed through the production consumer. A refusal set
is not marked closed merely because a local test reconstructed its expected branching logic.

### Closed foundation: implementation-manifest provenance

**Status:** IMPLEMENTED, MUTATION-KILLED, AND CORPUS-EXERCISED.

The former risk was a public format-only constructor accepting a syntactically valid invented
digest. The authoritative path now constructs complete inputs and computes its digests internally;
the acceptance and mutation criteria below remain as permanent regression requirements.

**Required implementation:**

- construct complete implementation inputs from one `ProofCoreEntry`, so callable identity,
  checked declaration facts and validated evidence body cannot be cross-paired;
- compute source-body and implementation digests internally through the single canonical
  `BodyIdentity` / `Digest` path;
- remove every public raw-digest construction route from proof-relevant consumers;
- keep implementation identity distinct from proof-subject identity: selected specification and
  claim scope belong only to the latter.

**Acceptance:** forged but well-formed digests are unrepresentable; cross-paired identity/facts/body
refuse; incomplete facts and invalid bodies refuse; a real complete `ProofCoreEntry` succeeds;
the frozen V2 subject and V1 golden remain byte-identical.

**Required mutations:** replace an implementation digest with 32 zeroes; pair one callable with
another callable's facts or body; remove body provenance. Each must prevent binding through the
real consumer path.

**Result:** unblocked self-denominating manifest completeness and body-edge correspondence.

### Closed foundation: implementation-manifest completeness

**Status:** IMPLEMENTED AND MEASURED ON REAL INPUT.

The former `filterMap` risk is closed: the result retains its expected denominator, rows and named
refusals. The real corpus reports 64 expected identities, 63 rows and one `extracted-missing`
refusal; the incomplete source is unusable rather than silently narrowed.

**Required implementation:** return a result with the exact expected eligible identity set,
success rows and named refusals. A manifest usable for correspondence exists only when expected
identities equal row identities, refusals are empty and identities are unique.

**Acceptance:** every eligible callable produces exactly one row or one named refusal; no refusal
is dropped; discovery order does not change canonical material; adding an eligible incomplete
entry prevents a usable manifest rather than shrinking it.

**Required mutations:** delete one produced row; introduce one incomplete eligible entry; duplicate
an identity; return rows in another order. The first three refuse and the last remains equivalent.

**Result:** unblocked exact per-edge correspondence.

### Open boundary: theorem-to-call-edge correspondence

**Status:** JOIN IMPLEMENTED AND SYNTHETICALLY CONTROLLED; REAL-SUBJECT POPULATION AND PRODUCTION
CONSUMPTION OPEN — critical path.

**Risk:** applying one theorem-level `contract` or `body` label to every callee can under-bind the
dependencies the theorem actually uses. Quantifying over a table is necessary for some contract
proofs but does not establish a corresponding contract hypothesis for a particular callee.

`Concrete/Proof/Correspondence.lean` now owns the closed join. `correspond` consumes requested edges
that include callee identity—not bare edge kinds—and candidate witnesses, while
`CorrespondenceResult` retains matched, missing, ambiguous, surplus and malformed material. All six
DependencyClosure refusal classes are named and the synthetic join controls pass. A permutation of
correctly identified same-kind witnesses is deliberately accepted; changing identity or kind is
the meaningful swap and refuses.

**Required production wiring:** for every real proved subject, construct requested edges from the
compiler graph; obtain the validated theorem classification; resolve every named table through the
closed table dispatch; derive candidate witnesses; run `correspond`; retain every result set in the
subject's dependency closure; and publish a separate real-corpus correspondence denominator. Do
not count the synthetic join gate as corpus correspondence.

**Surplus scope:** surplus means a candidate supplied to this closed correspondence operation that
matches no requested edge or witness slot. It does not mean an unrelated row in the global
classification table or an unused entry of a whole table. Compute it only after identity validation
and the exact join; never pre-filter unknown candidates. A dynamic whole-table witness is consumed
once for the dynamic edge and does not create per-entry surplus.

**Table-resolution inventory:** `Concrete/Proof/TableResolve.lean` resolves ordinary Lean table
definitions already linked into the compiler. Eight of nine table identities referenced by the
hand-back resolve. `Examples.ProofPatterns.Proofs.combineFns`, defined under `proofs/` outside the
compiler build, is the single named refusal. This replaces the disproved plan to add `evalExpr`, a
generator change and per-entry hand-back schema. Resolution must remain a closed, unique inventory:
unknown and duplicate names refuse, table identity/digest must agree, and deleting or misrouting a
dispatch entry must make the corresponding production subject refuse.

**Acceptance:** every actual call edge has exactly one validated justification; every theorem
witness is consumed exactly once or reported; dynamic table access binds the whole table; direct,
transitive and recursive edges participate; `calls.combine` refuses until justified and the
corpus reaches 64/64 only for semantic reasons.

**Required mutations:** remove, swap, duplicate or weaken a witness; change a callee identity;
change table membership; misattach a contract witness; omit a compiler edge. Each must make the
corresponding root refuse.

**Identity-boundary migration gate:** completion requires all five evidence boundaries—table
entries, requested edges, witnesses, dependency roots and receipts—to carry `DefinitionIdentity`.
No proof-relevant equality may remain `CallableId`-only, no adapter may synthesize package scope
from a display string, and no mixed legacy/V2 join is permitted. Legacy name-only rows produce the
typed `legacyNameOnly -> needs_recheck` disposition. The real cross-package collision fixture must
fail under the old identity, refuse under legacy evidence, and close only under the correctly scoped
definition; removing either package scope or implementation scope is mutation-killed independently.

**R-0004 closure boundary — exact selection, not model faithfulness:** R-0004 finishes the current
transition without absorbing the complete model/interface language of R-0482. Proof authors select
an opaque generated `ImplementationReference`; they never write a package identity, implementation
digest, or display-name attachment. `PFnDef` remains a mathematical proof model and contains no
source facts it cannot derive. The compiler recomputes the authoritative `DefinitionIdentity` and
requires exact agreement with the selected reference. Missing, stale, malformed, duplicated,
ambiguous, forged and legacy name-only references refuse or become `needs_recheck` according to
their typed disposition. Successful recomputation preserves provenance—`generatorAsserted` does
not become `compilerLinked` merely because two uses of the canonical formula agree.

R-0004 MUST retain these five results separately through reports, roots and receipts:

```text
implementation_selection   exact | stale | missing | ambiguous | legacy
model_correspondence       proved | checked | assumed | missing | stale | ambiguous |
                           unsupported
logical_validity           certificate_replayed | kernel_replayed | solver_trusted |
                           assumed | missing | invalid
dependency_closure         complete | incomplete | stale | ambiguous | unclassified
policy_acceptance          accepted | rejected | needs_recheck
```

An exact implementation selection secures the endpoint of a correspondence claim; it does not
prove the arrow from model to implementation. R-0004 may close with model correspondence explicitly
`assumed` or `missing` where that is the honest current state. It may not derive correspondence from
a matching digest, a generated symbol, a matching name, kernel validity of a theorem over PExpr, or
complete dependency closure. R-0482 owns the richer relation/evidence system.

**Resolution refusals are closure data, not report decoration:** zero/ambiguous/malformed external
rows, entry-provenance failures and table-digest mismatches must enter the typed
`CorrespondenceResult` consumed by roots and receipts. Rediscovering and appending them to a shadow
report is useful diagnosis but not authority. Dropping a resolver refusal while leaving ordinary
missing-edge handling intact must be killed by the production consumer.

**External-row controls use the production validator:** local list-length checks and
digest-equals-itself probes do not count. Inject zero rows, one valid row, identical duplicates,
conflicting duplicates, empty module/declaration identities, malformed body digests, duplicate
callable identities, and a self-consistent malformed entry/digest pair through the same pure
exact-one validator called by `entryEvidenceWithProvenance`. Include the valid singleton as the
over-rejection control.

**Dynamic-table boundary:** a dynamic requested edge carries the expected table identity and
canonical table digest; a `wholeTable` witness matches only when both agree. Until that exact match
is implemented and negatively controlled, dynamic correspondence is unsupported rather than
closed—`dynamic = true` alone must never accept an arbitrary whole-table witness.

**Trusted-boundary evidence:** a trusted witness binds the exact trusted callable/definition,
declaration or source identity, trust classification and assumption/trust digest. A diagnostic
string such as `declared-trusted-boundary` is not identity. Trust propagates monotonically into the
root and receipt, and removing or substituting the declaration is mutation-killed.

**Unblocks:** the authority-transition gate.

### Authority transition: dependency-aware verdicts

**Status:** PACKAGE 2 DISCHARGED 2026-08-16 — production `proved` now depends on correspondence and
on a computable closure. The list below is the original gate; its one `[~]` records conservative
contract witnessing whose precision decision belonged to Package 3 rather than to a skipped Package 2
authority boundary. **That decision was taken on 2026-08-16:** precise contract witnesses move to the
typed-contract milestone and leave R-0004's closure list, with a corpus tripwire in
`check_dependency_edges.sh`. The `[~]` therefore stays `[~]` permanently for R-0004 — it is a
conservative binding by design, not an unfinished one.

- [x] implementation-manifest provenance is closed by construction;
- [x] the manifest denominator is exact and every refusal is named;
- [x] every compiler-produced call edge has exactly one validated justification — for every subject
      that is `proved`; a subject whose closure lacks one is downgraded to
      `correspondence_unjustified` by the composed authority pass;
- [x] no theorem-derived witness, table binding or compiler edge remains unmatched (surplus,
      ambiguous, missing and malformed are each retained and each block usability);
- [x] body witnesses bind exact implementation identities — the join key is `DefinitionIdentity`,
      all four components, with no name-keyed fallback anywhere in the evidence path;
- [~] contract witnesses bind exact callee contracts/hypotheses — **CONSERVATIVELY SATISFIED, PRECISION
      PENDING, and the distinction is deliberate.** A contract edge is witnessed today exactly as a
      body edge is: from scoped table membership, so it is justified only when the table holds that
      exact IMPLEMENTATION. That is strictly STRONGER than the contract question and therefore
      fail-closed — it can refuse a claim that is genuinely justified, never accept one that is not.
      The precise form (bind the callee's CONTRACT identity, so a body change within the contract
      does not invalidate the claim while a contract change does) is not implemented.
      **The corpus has ZERO contract edges** — 45 body, 1 trusted, 18 unclassified — because edge
      kind comes from the CALLER's theorem and none of the three contract-classified theorems is
      linked to a function with callees. So this cannot be closed end-to-end here: implementing the
      precise form would be unfalsifiable on this corpus. What IS controlled is the rule that keeps
      the two apart — a contract witness cannot answer a body request; its edge falls to `missing`
      and the witness is separately named `malformed`;
- [x] trust and assumptions propagate monotonically through the full closure — measured on
      `composition_deep_trust` (`outer -> middle -> leaf`), where `outer` mentions no trusted
      function in its own body and still discloses `calls.leaf`. Mutation
      `trust-propagates-transitively` narrows the computation to direct callees and is KILLED. The
      disclosure also moved OUT of the `proved` branch: a boundary is a property of the closure, not
      of the verdict, and hiding it until a claim turns green meant an author fixed a fingerprint and
      discovered an assumption that was always there;
- [x] zero proved-subject root refusals, asserted as compiler invariant `PROVED-ROOTS` rather than by
      correlating report surfaces. **NOT 64/64 root coverage**: 13 subject roots refuse, every one of
      them on a subject that is not `proved`, and the redundancy that makes this hold is pinned;
- [x] collision, substitution, omission, ambiguity and misattachment mutations are killed by the
      production consumer — plus `authority-consumes-correspondence`, which reverts the pass itself;
- [x] ProofCore and Report consume the same validated root (`dependencyNodesOf` is a registered
      single producer, checked by `check_one_producer.sh`);
- [x] no legacy name-keyed, advisory or root-optional path can emit `proved`;
- [x] the production authority consumer has a live refusal control in
      `tests/programs/proof_decode_header.con`, whose table holds zero entries. The earlier claimed
      `main_drifted` movement was invalidated by the fixture-package/identity correction above and is
      not counted as a control.

**Standing Package 2 caveats carried into Package 3, not hidden reopeners:** the root conjunct is
consumed but redundant on the current corpus; `proof_pressure.validate_header` is contained by
removing its false claim rather than closed with a theorem for its actual body; and the authority
pass's only live refusal control is outside the attested fixture population. Slice 7 must preserve
the positive path, and Slice 8 must supply hostile production-consumer coverage inside the
authoritative evidence population.

**What that leaves, as of 2026-08-18.** Replay-authorized receipt issuance and consumption are
SHIPPED: the production replay path is the only minting authority, receipts are issued, stored and
re-checked, and a stored receipt is a distinct untrusted type with no minting path. Contract-witness
precision is DEFERRED to the typed-contract milestone with a corpus tripwire, and the ninth table is
DEFERRED past Slice 8 having been measured to carry no authority — both are off this closure list.

Slice 8 and the independent non-author packet are SHIPPED at 24/0. The packet found and closed a real
authority hole: kernel acceptance established that a theorem was true but issuance never established
that the theorem described the selected subject. Canonical theorem-to-specification binding now
refuses sibling and cross-program retargeting in both issuance and consumption. What remains is the
complete closure-validation bar below, not another evidence feature.

**[shipped 2026-08-17] Clean-checkout reproducibility and compiler provenance**
(`make test-clean-checkout`, 11/0). A real `git clone` into a fresh directory yields identical
evidence; invocation from repository and project roots agrees; and a receipt minted in one checkout
is consumed as current against the same source in the other checkout. Compiler provenance is a
portable build-time, project-standard 128-bit source identity embedded in the binary, with freshness
checked against both the working tree and committed sources (`BUILD-IDENTITY-FRESHNESS`, 8/0). This
names the source build rather than the checkout at report time and works on supported macOS without
`/proc/self/exe` or GNU `sha256sum`. Named limit: patching the executable after build does not move the
embedded value; portable final-executable or signed-build-manifest binding is post-R-0004 hardening.

**The gate immediately found a real location-dependence.** `dirOf "src"` is `""`, so
`findProjectRoot` walked from `src` to the FILESYSTEM ROOT and never checked the current directory:
running `concrete src/main.con` from inside a project found no project, fell back to standalone
analysis with a synthetic package identity, and reported correspondence 0/4 where the same file named
from the repository root reported 4/4. Fixed by canonicalising the walk — which then broke
path-derived scoping until the input was canonicalised at the comparison too.

**Cache legs remain deferred**, not skipped: `ProofCache` is not implemented, so cold/warm/missing/
corrupt-cache tests would pass vacuously. They are a ProofCache graduation condition.

**[shipped 2026-08-17] Atomic V1 -> V2 activation.** The live freshness verdict now compares the V2
subject digest. **The corpus census is UNCHANGED** — 35 proved, 5 stale, 4 dependency-not-current,
15 unbound, 15 ineligible, 32 no-proof, 9 trusted, 1 blocked, identical before and after — which is
the whole point of migrating first: activation without migration turns 43 links `needs_recheck` in a
single commit, measured rather than assumed. `make test-v2-migration-plan` 11/0 now asserts
COMPLETION (nothing left to migrate) and is the regression leg if a v1 value ever reappears.

Atomic because it cannot be otherwise: storing v2 values while the comparison is still v1 marks every
migrated link stale, so the rewrite, the deriver flip, the regenerated attestation manifest and the
regenerated classification table are one change. Editing any `.con` file moves its `PackageIdentity`
— which binds module content — and therefore every `DefinitionIdentity` in that package.

`ObligationStatus.needsRecheck` existed and had never been produced; it is now what a v1 record reads
as. `elf_header_drifted` still demonstrates genuine v2 staleness because its two links were pinned to
the REAL pre-drift digests, obtained by reverting the drift, measuring, and restoring it.

Two design errors of mine surfaced during activation and are fixed: migration required kernel
acceptance (it should not — a fingerprint records WHICH BODY, not whether the proof is valid), and
staleness had TWO producers, so flipping only the status deriver left the registry validator
comparing a v2 value against a v1 fingerprint and emitting five spurious warnings on a clean fixture.

**The link count is 44**, counted from CODE and reconciled by `make test-v2-migration-plan` (9/0):
44 stored fingerprints = 43 plan rows + 1 unreachable (`contract_negatives/invalid_attribute`, whose
purpose is an invalid attribute), and 43 rows = 33 migrate + 6 replay-refused + 4 stale-with-no-
honest-value. An intermediate correction to "43" was itself wrong — it came from a grep matching one
attribute spelling — and the first census read 52 because fixture COMMENTS discuss the attribute.
That is the fourth prose-for-code confusion this repo has caught in a measurement, and it is why the
figure now comes from a gate rather than from this sentence.

### Package 3 — durable replay authority and migration

**User-visible exit story.** An external user can receive a Concrete package and its evidence,
replay it independently, upgrade or replace a dependency, and see exactly which claims remain
current, which became non-current, why, and which trust/assumptions remain. No friendly verdict may
depend on compiler display text, cache presence, an absent completion marker, or an unreplayed
artifact.

**Authority stages used by this package.** The general ownership decision appears near the top of
this roadmap; Package 3 applies it as follows:

| object | status in Package 3 | authority rule |
|---|---|---|
| `VerificationTask` | **[ratified], canonical production construction pending** | immutable exact subject/contract-state/rule-set envelope whose proposition is canonical VIR; internal `TermIR` is not its public schema |
| `ProofAttempt` | **[shipped 2026-08-16]** | `Concrete.Proof.replay` is the one typed theorem-replay execution; every refusal is a returned value, and rendering plus exit-code projection are consumers outside it. `make test-replay-producer`, 26/0 |
| `ProofSession` | **[ratified], post-authority UX** | mutable attempt history and obsolete-state tracking; not a receipt and not an R-0004 UI requirement |
| `ReplayReceipt` | **[shipped 2026-08-16] production** | minted only from a `SuccessfulReplay` plus complete closure/environment material — `unchecked facts -> receipt` does not typecheck. Issued, stored and re-checked on consumption; a stored receipt is a distinct untrusted type with no minting path. 12/0 and 14/0 |
| `PolicyDecision` | **[ratified], full typed implementation owned by R-0440** | fresh source/correspondence/dependency/advisory facts plus receipt plus policy; a stored receipt alone is never current status |
| `ProofCache` | **[ratified] disposable — NOT IMPLEMENTED, and off R-0004's required path** | performance only; a hit, miss, corrupt entry, or disabled cache cannot mint or upgrade evidence. No proof cache exists in the codebase, so cache-corruption tests would pass vacuously today; they are retained as a ProofCache GRADUATION CONDITION rather than as an R-0004 exit criterion |

**Progress already closed 2026-08-16:**

- **[shipped] Receipt field coverage:** subject, theorem artifact, dependency root, table material,
  trust/assumptions, schema and environment are present; root, artifact and trust participate in
  `isCurrentAgainst`. This closes structural binding only, not replay issuance.
- **[shipped] Project/path-independent material** (`make test-evidence-reproducibility`, 5/0): a
  different absolute path and directory name produce byte-identical attestation join, dependency
  roots and correspondence; a one-byte content mutation is the non-vacuity control.
- **[shipped] Eligibility baseline** (`make test-coverage-baseline`, 5/0): **35 of 91 eligible claims
  proved**; 31 no-proof, 15 unbound, 5 stale, 4 dependency-not-current, 1 blocked and 0 unjustified;
  15 ineligible and 9 trusted are excluded rather than counted as proof failures. The report says it
  does not run the kernel, so this is not replay coverage.
- **[shipped] One typed kernel-replay producer** (`make test-replay-producer`, 26/0):
  `ReplayRequest -> IO (Except ReplayRefusal ReplayResult)`. Typed inputs, every failure class
  returned rather than printed, environment retained with the verdict, no evidence side effects. An
  empty request REFUSES rather than returning a vacuous success, because `List.all` is true over an
  empty list. `findLakeWorkspace` moved out of `Main.lean` with it: one answer to "where does this
  replay".
- **[shipped] Replay-gated minting.** `unchecked facts -> ProofEvidenceReceipt` no longer typechecks.
  `ReplayResult` and `SuccessfulReplay` have private constructors and single producers, and
  `ProofEvidenceReceipt.mint` is total and takes the token; the artifact and toolchain come FROM the
  run rather than from parameters a caller chooses. Validation split into pure `ReceiptMaterial.of?`.
  Ten mutation families, all KILLED. **This is authority, not issuance:** nothing in the production
  pipeline mints yet, so no stored receipt affects any status.

- **[shipped] Production receipt issuance** (`make test-receipt-issuance`, 12/0): `--report receipts`
  mints from real kernel replay — `elf_header` 5 issued / 0 withheld — and derives none of its four
  inputs itself. Every withholding names its cause. **Gated on the composed authority verdict**, which
  it was not on the day it landed: the drift fixture received four receipts, two for `stale` claims
  whose bodies had changed since their proofs were linked. Now 2 issued / 3 withheld there.
  Receipts are stored (`--out`) and re-checked on consumption (`--report proof-status --receipts`),
  so this is evidence in use rather than issuance alone.

- **[shipped] The V1 -> V2 migration dry run** (`make test-v2-migration-plan`, 9/0). Reconciles the
  whole population, and refuses to manufacture freshness: a STALE link is pinned to a body that no
  longer exists, so no honest v2 value can be written for it and it becomes `needs_recheck` on
  activation. The refusal is per-claim — the drift fixture's undrifted claims still migrate.
- **[shipped] Receipt storage and consumption** (`make test-receipt-consumption`, 14/0). Receipts
  also bind the PROOF LIBRARY by content digest of its Lean sources, closing the environment
  authority gap: the same theorem names replayed against a different library previously produced a
  byte-identical receipt. The vertical
  slice closes: `--report receipts --out <path>` writes, `--report proof-status --receipts <path>`
  reads back and RE-CHECKS. `StoredReceipt` is a deliberately different type from
  `ProofEvidenceReceipt` with no minting path, so a receipt read from a file cannot become evidence —
  it can only agree or disagree with material computed fresh. The storage key is untrusted: swapping
  two receipts under each other's names moves exactly those two. Decoding refuses rather than
  repairs (missing, duplicate, unknown, unreadable-schema, malformed), and one bad record neither
  discards the rest nor vanishes. **Three Slice 8 attack classes are closed at the decode boundary**
  (partial decoding, schema confusion, receipt/claim substitution).

**Defects found and fixed while doing the above:**

- **`generalFailure` was unreachable dead code.** `--report check-proofs` computed it AFTER a
  fallback that marked every target failed, so a file that did not compile was reported as every
  theorem individually "not found" — the same misdiagnosis the workspace-resolution fix exists to
  prevent. The `env_failure` JSON status and the transcript disclosure had never been reachable.
  Fixed; such targets are now `notAttempted`, and a positive control exists only because `imports`
  is a request field.
- **Backticks inside double-quoted gate messages are command substitution.** Found twice in one day;
  a failure message could silently lose the text a reader needs. Two remaining instances escaped.
- **Every receipt disagreed with itself the instant it was written.** `s!"{n}"` renders a `Name` in
  Lean's display form, so a component containing dots serialized as `«Concrete.Proof.elfFns»` and
  decoded back WITH the guillemets — the whole store read NOT current. The unit round-trip probe used
  table `A`, too simple to expose it; the real corpus caught it on the first end-to-end run.
- **A gate mis-diagnosed an unreadable file as a stale one.** Four NUL bytes inside string literals in
  `ClassificationTable.lean` made `grep` treat it as binary; `-o` returned nothing, and
  `check_classification_freshness.sh` reported "STALE classification table" for a table that was
  current. It guarded the generator's side of the comparison for emptiness and not the checked-in
  side. Lean compiled the file without complaint — a NUL inside a string literal is legal. Both sides
  guarded now; every tracked `.lean`/`.sh`/`.con`/`.md`/`.toml` swept for NUL bytes, none remain.

**Two limits of the issued receipts, recorded rather than papered over:**

- `importsId` is meant to bind the transitive import surface. The compiler cannot compute the Lean
  import closure, so it binds `classificationSurfaceDigest` — every classified theorem, its artifact
  digest and its table digests. A proof that changes which ambient lemma it invokes, without moving
  its own artifact digest or its tables, moves nothing there.
- **Nothing in a receipt identifies which proof library was replayed.** The surface digest is compiled
  into the binary, so replaying against a workspace whose theorems differ would move no field.
  Refusing a caller-resolved workspace is what currently stands in for that missing binding, and it
  is a mitigation, not a binding.

**Vertical slice status:** successful replay producing a receipt, stale/substituted artifact
refusing, trusted closure retaining its named boundary, and `proof-status` consuming fresh facts plus
the receipt are all closed and gated. The fifth bullet is below.

**A vertical-slice bullet that cannot be tested because its subject does not exist:** "cache
disabled/corrupted without changing authority". There is no proof cache in the codebase — `ProofCache`
is a ratified design decision, not an implementation — so any such test would pass vacuously today.

**Found, NOT fixed, and newly scoped — a library package cannot carry proof evidence:**

`loadProject` hardcodes `src/main.con` as the entry point, so a package whose entry is `src/lib.con`
cannot be loaded as a project at all. Scoped identity comes from the package, so every subject in
such a package carries a source name only, and after the Package 2 authority transition
correspondence and dependency roots refuse for all of them. `std` is exactly that shape, and the
workaround its gate uses — copying `src/` elsewhere and compiling the loose tree — DESTROYS the
package identity that evidence now depends on.

**Consequence, measured:** `scripts/tests/check_purecore_proofs.sh` is RED at 19/20, with 9 std proof
links reading "dependency closure has no validated per-edge justification" and drift coverage at 2
of an expected 11. Verified against a build of the tree WITHOUT this session's changes, so it is a
standing consequence of the Package 2 authority transition and not a regression from replay work.
The gate is deliberately left red rather than adjusted to the demoted numbers.

An entry-point fallback to `src/lib.con` was implemented and REVERTED: it let the project load but
module-to-file scoping still resolved zero functions, converting a loud, correct error into a silent
vacuous success — worse than the error. The real fix is module-to-file scoping for library layouts.
This gate is also not in `run_tests.sh`, which is why the demotion went unmeasured; the 204 gates in
`scripts/tests/` are not all reachable from the fast suite.

**Current critical path, in order:**

1. **[shipped 2026-08-16] Extract one typed theorem-replay producer.** Replace the inline, print-and-return
   invocation in `compileAndReport` with the equivalent of
   `ReplayRequest -> IO (Except ReplayRefusal ReplayResult)`. It takes typed inputs rather than the
   CLI's JSON/rendering parameter, returns every failure class, and retains theorem/artifact/checker/
   import material. `check-proofs`, receipt issuance, migration, and tests consume it; rendering,
   diagnostics and exit codes stay outside. The producer has no receipt side effects: it returns a
   successful replay value or a typed refusal, and the next authority stage decides whether a
   receipt can be minted. Kernel replay already works (5 verified / 0 failed on `elf_header`) and
   already keeps `unboundChecked` out of `verified`. **`prove --replay` is not a shortcut**: it
   discharges VC obligations and does not replay linked Lean theorems.
2. **[shipped 2026-08-16, authority only — issuance still pending] Make successful replay the only receipt-minting authority.** Invalid construction is
   unrepresentable: unchecked facts cannot become a receipt; only `SuccessfulReplay` plus a complete
   `VerificationTask`, dependency/trust material and production-derived environment may mint one.
   Production mutations remove replay, convert failure to success, omit root/artifact/trust, or
   accept an interrupted run; the status consumer, not a helper probe, must kill each.
3. **[shipped] Slice 7a — one vertical production receipt.** Successful replay producing a receipt,
   stale/substituted artifact refusal, transitive trusted-boundary retention, and `proof-status`
   consuming fresh facts plus the stored receipt are closed and gated. Receipts are stored, read back,
   and re-checked rather than believed. Cache-disabled/corrupt-cache authority agreement is pending on
   a real `ProofCache` implementation; the control is vacuous today and tracked as design debt.
4. **[deferred to R-0473/R-0474] Exact contract-witness precision.** Package 2's implementation-scoped
   witness is conservative and safe. The smallest real production fixture that distinguishes contract
   from body dependence, and exact `ContractIdentity` plus imported hypotheses, are deferred to the
   typed-contract milestone. “Blocked on a corpus case” is not a durable third state. Full `old`,
   frame/`modifies`, rich state models and the total contract language remain outside Package 3.
5. **[DEFERRED past Slice 8 on 2026-08-16 — off R-0004's closure list; see the ninth-table row above for the measured no-authority proof]** Complete the ninth table. Either
   land mutable-borrow ProofCore extraction using the narrowest sound value-state model, migrate
   `proofFnsExt`'s three entries, and replace its tripwire with positive extraction/evaluation/kernel-
   replay correspondence; or explicitly classify those three links as `unsupported`/`needs_recheck`,
   remove their ability to provide authority, and defer broader memory semantics. Unsupported reborrow
   shapes refuse rather than forcing a general heap calculus into R-0004. Other Package 3 work
   continues while the prerequisite is blocked, but formal closure does not wait indefinitely for an
   unrelated language-semantics blocker.
6. **[shipped] Slice 7b — exact 44-link migration and atomic V2 activation.** The dry-run manifest
   was produced, every eligible link was replayed, receipts were issued only from successful replay,
   legacy/incomplete evidence was mapped to `needs_recheck`, every V1/V2 difference was explained,
   and the authoritative subject digest flipped to V2 without a mixed fallback or copied hashes. The
   exact 44-link denominator remains authoritative.
7. **[shipped 2026-08-17] Slice 7c — complete reproducibility and coverage.** The 35/91 baseline and
   the 5/0 path-independence controls are preserved, and `make test-clean-checkout` (11/0) adds what
   they did not cover: a real `git clone` reproducing the working tree's evidence, repository-root
   versus project-root agreement, and — the control that was missing — a receipt minted in one
   checkout consumed against another checkout's sources at a different absolute path. An unbuilt
   checkout issues nothing rather than issuing unverified receipts. Cache legs stay deferred to
   ProofCache graduation, since `ProofCache` does not exist and the tests would pass vacuously.
8. **[shipped 24/0] Slice 8 — adversarial closure of the production consumer.** It attacks omitted, stale,
   forged, duplicate, ambiguous and cross-package evidence; contract-hypothesis deletion/widening;
   trust deletion/laundering; opaque or unreplayed theorems; copied receipts; partial/defaulted
   decoding; schema and policy/receipt confusion; dependency/environment changes; cache-as-evidence;
   contradictory/vacuous proof contexts; implementation material unused by the proof; and receipt
   persistence failures — partial/truncated writes, non-atomic installation, rollback, duplicate IDs,
   oversized/noncanonical decoding, and crash between replay and completion. Keep production weakening
   mutations, permanent hostile fixtures, one end-to-end multi-module project, semantic proof-
   dependency/smoke diagnostics where available, and an independent non-author attack packet. Every
   successful attack becomes a permanent named regression.

> **R-0004 closes when Slice 8 completes, no hostile control can make a non-current claim appear
> current, and valid replayed claims remain current under benign checkout, path, cache, and
> dependency-preserving changes.** The current subject, exact claim and dependency root, explicit
> trust/assumptions and environment, and successful replay receipt must all survive the production
> consumer together. "No hostile control can succeed" is necessary but not sufficient: a path that
> refuses every claim is not evidence that the path accepts valid claims.

**[historical] Closure validation bar satisfied on 2026-08-20.** R-0004 did not close on the fast
suite or on individually green evidence gates. Resolve every failure found by the complete gate
population, whether newly introduced or pre-existing; "pre-existing" establishes attribution, not
acceptability. Add an attestation-manifest freshness owner that regenerates from compiler facts,
rejects an empty/incomplete population, and pairs the narrow tracked-generated-file allowance with a
real freshness comparison. Builds, generators, mutation harnesses and evidence gates share one
repository-wide exclusive lock so a concurrent rewrite/rebuild cannot contaminate the verdict. The
final run must be serial and uncontaminated, reconcile discovered gates with executed gates, write a
machine-readable `completed=1` summary, report zero failures and unexplained skips, and finish at a
clean pushed HEAD. Those conditions were satisfied by the protected 205/205 run recorded above.

**Product hand-off after R-0004:** R-0004 makes the evidence path honest and replay-independent of
producer state: a receipt can be re-verified from the stored artifact, the exact subject, and the
public task schema. R-0353 provides a separately implemented consumer for that evidence; it is
deliberately not another R-0004 slice. R-0353 makes the evidence independently consumable without
trusting Concrete's compiler or report pipeline. Prototype codec/root work may begin in shadow, but
no verifier result becomes authoritative until R-0004's replay, migration and adversarial
boundaries close.

## R-0478

_Stub: [ROADMAP.md](../project/ROADMAP_TASKS.md#task-r-0478) · disposition: merged · owner(s): R-0481, R-0474_

**Scheduling: opportunistic.** No queue position and nothing blocks on it. Do either half when
already working in the affected file; the `substExpr` rename is the one with a deadline, since it
must land before R-0474 retires the unsound function or that commit becomes ambiguous.

**1. Gate-harness duplication: 163 of 183 gates define their own `ok()`.** A shared library
exists (`scripts/tests/lib/selfprint.sh`) and 28 gates use it. The rest carry private copies of
`ok`/`no`/`assert_contains`/`assert_absent`/`assert_json`, which is why the vacuity fix had to be
applied to two `assert_absent` definitions separately — and why a third copy could be sitting
somewhere unfound. `check_gate_vacuity.sh` can only scan for *shapes* precisely because there is
no single definition to fix.

Consolidating is mechanical but must not be a flag-day rewrite: move the helpers into the shared
lib with identical semantics, migrate gates in batches, and keep each batch's before/after
PASS/FAIL counts identical. A gate whose count changes during a "mechanical" migration is a gate
whose assertions were not what its author thought.

**2. `substExpr` names two unrelated operations.** `Concrete/IR/Mono.lean:43` substitutes TYPES
(`sub : Ty → Ty` over `CExpr`, private); `Concrete/Report/BoolKernel.lean:800` substitutes
EXPRESSIONS by name (`env : List (String × Expr)`, public, and the H25 capture hazard). During
the H25 investigation the first thing needed was establishing which one the containment applied
to — a grep for `substExpr` returns both, and only one is unsound. Rename the type one to
`substTyInExpr` (or the expression one to `instantiateSpecBody`) before R-0474 replaces the
second, so the commit that retires name-based substitution cannot be read as touching the other.

**Why these are one task:** both are cases where the codebase relies on a reader noticing that two
similarly-named or similarly-shaped things are different. That is the same failure mode as the
comment claiming a gate that did not exist — the structure asserts a relationship nothing
enforces.

## R-0007

_Stub: [ROADMAP.md](../project/ROADMAP_TASKS.md#task-r-0007) · disposition: superseded · owner(s): R-0492, R-0510_

   The TYPE-name half of "first fail closed" already landed with R-0001 (E0809:
   a specialization whose mangled name is declared, or two instantiations
   mangling to one name, are refused; gated in
   `scripts/tests/check_mono_name_collision.sh`). It had to, because
   per-instantiation enum mono made forged ENUM specialization names reachable
   for the first time. What remains here is the rest: the FUNCTION-symbol half
   (fn specializations use a different mangler and are not covered), the
   injective unforgeable encoding that would let a program spelling
   `Box_Int` compile instead of being refused, semantic identity separate from
   link/display symbols, and by-identity layout lookup with diagnosing
   missing-field access.

   This task also turns PRINCIPLES #12 into an adversarial metamorphic gate.
   A strict, capture-avoiding alpha rename of source binders must preserve
   acceptance, observable behavior, and semantic artifacts modulo honest
   display names and source spans. A second hostile-spelling leg deliberately
   pressures generated mangles, builtin/intrinsic names, import aliases, and
   local/global collisions. That leg may preserve behavior or fail closed with
   a phase-appropriate collision diagnostic; it may never silently change
   behavior or trigger an internal error. Random fresh-name substitution is
   insufficient because bugs 044/050/051/054 were collision-sensitive.
   Lowering-only sentinels such as `@fnref.*` are exercised by IR mutation
   fixtures, not treated as legal source identifiers. Land the semantic
   contract and fixed historical/adversarial legs here; R-0272 later scales
   the same oracle across generated and corpus programs rather than defining a
   second identity-testing semantics.

## R-0434

_Stub: [ROADMAP.md](../project/ROADMAP_TASKS.md#task-r-0434) · disposition: superseded · owner(s): R-0502_

   The gate for R-0001 asserts this structurally for its own programs (distinct
   declarations, differing footprints) and mutation #19 proves those programs are
   discriminating, but a NEW way of desynchronizing a declaration from a write —
   another aggregate kind, a second backend, a future layout change — would not be
   caught by fixtures that predate it.

   Add a post-Lower/SSA check that walks payload GEP + store chains rooted at an
   alloca of known type and rejects any write whose offset plus size exceeds the
   declared footprint, keying on layout identity rather than on type NAME. Report
   it as a compiler-internal containment failure (the E0808 class) so a codegen
   bug fails closed instead of corrupting memory. Gate it with programs that are
   correct today, plus mutations that shrink a declaration or inflate an offset,
   and require each mutation to be caught by the verifier rather than only by a
   value mismatch. This is the criterion-7 remainder recorded in
   `docs/bugs/051_generic_enums_not_monomorphized.md`; it hardens a closed defect
   rather than fixing an open one, so it follows the confirmed defect queue.

## R-0435

_Stub: [ROADMAP.md](../project/ROADMAP_TASKS.md#task-r-0435) · disposition: superseded · owner(s): R-0500_

**Decision status:** ratified 2026-07-25; implementation pending. This reopens
the same R-0435 task after the earlier same-day “shadowing stays legal” decision
was superseded. The changelog preserves that history; reopening the task is not
reuse of its stable ID.

The language rule is:

> A new local value binding may not reuse the spelling of another local value
> binding visible along its lexical scope chain.

This applies to parameters, `let` declarations, pattern binders, loop binders,
and borrow-block value/region binders. It rejects both an enclosing-scope
collision and a same-scope consuming spelling such as
`let s = transform(s)`. It is an auditability and human-identity rule, not a
substitute for compiler-owned identity: Elab alpha-renaming remains mandatory
defense in depth.

Assignment is not binding introduction. `acc = f(acc, x)` remains legal for a
`mut` linear place when evaluation consumes the old value before installing the
new one; overwriting a still-live linear value remains E0219. The normative
slogan is “one lexical name, one binding; at most one live resource in a linear
place,” not “one binding, one resource.” A declaration inside a loop denotes
one lexical binding even though it executes each iteration, and equal binder
spellings in sibling match arms remain legal because neither binding is visible
from the other.

The rule is scoped to the lexical value-binding namespace. Field labels, type
names, traits, and modules occupy separate namespaces. Whether a local may
shadow a top-level function, import alias, or intrinsic is a separate namespace
decision and must not be smuggled into this migration.

Slice 1 is the prerequisite pattern-binder renaming form
(`Variant { value: inner }` or an equivalent LL(1) spelling), shared by
`match`, `if let`, `while let`, and destructuring `let`. Without it, nested
`Option`/`Result` patterns are not expressible without shadowing because both
payloads are named `value`. Slice 2 adds one stable diagnostic for forbidden
lexical reuse and migrates the existing corpus. Slice 3 adds the inventory and
mutation gates.

Gate renamed nested patterns in every supported pattern position; rejection of
parameter/`let`/pattern/loop/borrow collisions; rejection of
`let s = transform(s)`; positive assignment-rebind, per-iteration declaration,
and sibling-arm cases; interpreter/native agreement; formatter round-trip; and
mutations that omit each binder kind or accidentally reject assignment rebind.
Land the cross-cutting migration in a worktree and merge once green.

   Falsification probe first: rename the same-name binders in ONE std file
   and review the result. If the renamed form is visibly worse (`s1`/`s2`/
   `s3` noise), narrow the ban before the migration, not after.

   Probe result (2026-07-30, preliminary hand-sample, ~20 sites across
   std option/result/deque/map + envcfg/kvstore/conlog/error_conventions):
   the pervasive `Variant { value }` idiom is arm-local and immediately
   consumed (exempt); the one nested case found (envcfg) is sibling-arm
   exempt under the ratified rule; the live consuming-rebind shape
   `let s = transform(s)` did not appear. Zero forced renames in the
   sample — preliminary evidence to proceed as ratified. A systematic
   pass over the ~25 sites DECISIONS cites should confirm before the
   migration lands.

## R-0437

_Stub: [ROADMAP.md](../project/ROADMAP_TASKS.md#task-r-0437) · disposition: superseded · owner(s): R-0514, R-0510_

Verified 2026-07-25 against the built compiler: `fn ident<T: Nonsense>(x: T) -> T`
instantiated at `T = i64` compiles, runs, and returns its argument — the
fictional bound constrains nothing. `fn never_called<T: Cpoy>(…)` compiles
because bounds are only ever validated at an instantiation site. The same
fictional bound at a user struct *is* caught (E0241), so the hole is exactly the
primitive and never-instantiated paths.

Cause: `Concrete/Check/CheckHelpers.lean` `checkTraitBounds` special-cases
`"Copy"` and `"Destroy"`, treats every other bound name as a lookup into
`env.traitImpls`, and ends with `| _ => pure () -- primitive types, skip
non-Copy bound checking`. There is no set of legal bound names, so a bound
cannot be misspelled — only silently ineffective. This is the C3 class
("unknown attributes silently ignored", closed 2026-06-10) resurfacing on the
generic-bound surface, and it is a semantically dark construct under the
governing rule: it looks meaningful and is ignored.

The rule is two-stage, because Concrete has real `trait` declarations
(`grammar/concrete.ebnf:62`, `env.traits` / `m.traitDefs`, plus
`builtinDestroyTrait`) and a generic may legitimately name a trait that has no
implementation yet in this compilation context:

1. **At the generic declaration** — the bound name must resolve to an in-scope
   `TraitDef` or a builtin bound (`Copy`, `Destroy`). This is the stage that is
   missing today, and it is what refuses `Nonsense` and `Cpoy` whether or not
   the generic is ever instantiated.
2. **At the instantiation** — the concrete type must have a matching valid
   `TraitImpl`. This stage already exists (E0241) and needs only the primitive
   fallthrough closed.

Keying stage 1 on registered *impls* would be wrong: a declared-but-unimplemented
trait would be misclassified as an unknown bound. Diagnose "no such trait" and
"trait declared but not implemented for this type" as distinct errors.

The impact is false assurance rather than unsoundness — bodies are still checked
against declared bounds and linearity still catches double use — so this is a
containment-of-claims fix, not a wrong-code fix, and it sits here rather than
among the miscompiles.

Gate: the three programs above as fixtures (fictional bound at a primitive arg,
typo'd bound on an uninstantiated generic, the E0241 struct case as positive
control), plus a positive fixture for a declared-but-unimplemented trait used as
a bound on an uninstantiated generic (must compile — this is the case the
impl-keyed rule would wrongly reject), cross-module and renamed-trait-import
resolution, a legal-bound-name inventory that fails when a new builtin bound is
recognized in code without an entry, and a mutation restoring the primitive
fallthrough.
Record the bound vocabulary normatively — `Copy` is described in
`docs/language/VALUE_MODEL.md`, `Destroy` in `docs/language/RUNTIME_COLLECTIONS.md`, and nothing
states the whole set or what satisfies it, while `docs/project/DECISIONS.md` records
"no trait objects" without mentioning that `impl Trait for Type` and `T: Trait`
bounds exist and are load-bearing across the stdlib. Name bounds as a required
section of the Phase 17 language reference.

## R-0447

_Stub: [ROADMAP.md](../project/ROADMAP_TASKS.md#task-r-0447) · disposition: merged · owner(s): R-0481_

This is an ownership contract and no-new-debt migration, not a repository-wide
rename commit. The compiler is comparatively small; the pressure is in the
surrounding apparatus—flat test scripts, fixtures spread across tests/examples/
bug notes/audit directories, example-specific proofs separated from their
programs, normative text mirrored into the site, and generated artifacts beside
source. Moving all of it at once would create path churn while R-0442/R-0004
still change semantic identities. Leaving it unconstrained until Phase 8 would
make every stdlib and workload task add more debt.

Measured 2026-07-28 migration baselines make the hotspots concrete:
`scripts/tests/run_tests.sh` is 10,970 lines, `scripts/tests/` has 198
top-level files, `tests/programs/` has 819 top-level tracked `.con` fixtures,
`Concrete/Report/Report.lean` is 4,474 lines, `docs/` has 126 top-level
Markdown files, and the site has 16 named reference mirrors plus its index.
The clean tracked corpus has no `.ll` fixture at `tests/programs/` top level;
LLVM files observed there are generated worktree debt, not source fixtures.
Use these numbers as a ratchet baseline, not as a reason to split files or
directories mechanically.

**Target ownership layout:** `config/repository-layout.toml` will become the
canonical machine-checked definition of this tree, and
`docs/REPOSITORY_LAYOUT.md` its human explanation. These names describe the
repository architecture we want, not merely examples:

```text
.github/
  workflows/                hosted CI entrypoints only
.githooks/                  thin local policy entrypoints
config/
  repository-layout.toml    canonical ownership and dependency manifest

Concrete/
  Frontend/                 syntax, parser, source spans
  Resolve/                  modules, imports, semantic identities
  Check/                    types, ownership, capabilities
  Core/                     checked typed program
  Mono/                     instance collection and specialization
  SSA/                      lowering, verification, cleanup
  Semantics/                arithmetic, traps, layout, effects
  Interpreter/              executable second semantics / differential oracle
  Backend/
    LLVM/
    QBE/
  Proof/
    Core/
    Extract/
    Replay/
    Receipt/
  ProofKit/                 reusable proof definitions and lemmas
  Evidence/                 claims, obligations, provenance
  Report/                   rendering only
  Driver/                   CLI, projects, build/run/test orchestration

std/src/
  pure/                     authority-free, allocation-free core
  alloc/                    allocation-owning data structures and algorithms
  hosted/                   filesystem, network, process, clock, console

tests/
  language/
  ownership/
  semantics/
  mono/
  ssa/
  backends/
  proof/
  evidence/
  stdlib/
  workloads/
  bugs/
    <bug-id>/
      case.toml
      fixtures/
      controls/
      expected/
  shared/
    harnesses/
    oracles/

examples/
  tutorials/
  showcase/
  workloads/
  evidence/
  known-holes/
  <category>/<name>/
    example.toml
    src/
    proofs/
    fixtures/
    expected/
    README.md

docs/
  reference/                normative current language
  design/                   ratified design and decisions
  evidence/                 claim and evidence model
  stdlib/
  bugs/
  guides/
  historical/

research/
  active/
  archived/

grammar/                    canonical standalone grammar artifacts
editor/
  grammar/                  syntax package generated/checked against grammar/
  lsp/                      editor protocol implementation when pulled

paper/                      thesis/paper source and bibliography

site/
  content/                  site-only prose; no normative reference copies
  templates/
  static/

scripts/
  ci/                       thin CI composition
  tests/
    runners/                small semantic-area runners
    generators/             manifests → inventories/matrices
  reduce/                   failure predicates and minimization helpers
  release/                  distribution/replay tooling
  dev/                      local developer commands

bench/
  manifests/                comparable workload and measurement definitions
  inputs/                   pinned small benchmark inputs
  references/               pinned comparable reference implementations
  baselines/                reviewed provenance-bearing baseline ledgers

release/
  manifests/                supported targets and bundle contents
  baselines/                tracked release budgets
  policy/                   signing, reproducibility, and publication policy

.build/
  artifacts/
  reports/
  proofs/
  reductions/
  tests/
  site/
  benchmarks/
  releases/

dist/                       generated publication output only
```

Ownership is stricter than spelling. `Semantics` owns reusable semantic facts;
`Report` may render them and `Driver` may schedule them, but neither may
recompute them. Compiler regressions belong to `tests/bugs/`, never to a
teaching category. An example owns its example-specific `proofs/`, `fixtures/`,
`expected/`, and replay material beneath its own directory even though those
children are not repeated in the compact tree above. The site is a generated
consumer of `docs/` and is not a second documentation root.

Root files are limited to repository entrypoints and tool configuration
(`README`, `ROADMAP`, `CHANGELOG`, contribution/security/license policy,
`Makefile`, Lean/Nix configuration, and equivalent canonical manifests).
There is no durable top-level `proofs/` in the target: reusable material lives
in `Concrete/ProofKit`, compiler proof machinery in `Concrete/Proof`, and
example-specific theorems with their example. There is no `.audit_me/` in the
target. The Rust-era `bench/` contents are deleted or replaced by
R-0416–R-0419's evidence-bearing shape above; preserving the directory name
does not preserve dead machinery. `dist/` is the single release-publication
exception to the ordinary `.build/` rule: it is generated, ignored, reproducible
from `.build/releases/`, and contains no canonical source or evidence record.

Physical moves to this tree occur only with their owner migration and must
preserve stable imports, test IDs, history, and links. Where the current module
loader couples an import name to a path—especially under `std/src/`—land the
namespace-stability rule before the move; until then the ownership manifest
must expose the target pure/alloc/hosted structure over the legacy path. A
different final directory name requires a ratified amendment to
`docs/REPOSITORY_LAYOUT.md`, not an ad hoc local convention.

**Concrete implementation contract:** use
`config/repository-layout.toml` as the one machine-readable source for
top-level owners, compiler layers and allowed import edges, test areas,
example/document/research classes, generated roots, and temporary migration
exceptions. `docs/REPOSITORY_LAYOUT.md` is its checked human explanation, not a
parallel list. Every exception carries `id`, `owner`, `reason`, `introduced`,
`removal_task`, and the exact path/import pattern; the gate rejects ownerless
or removal-task-free exceptions. Add
`scripts/tests/check_repository_layout.sh` as the umbrella gate.

The test-case source schema is `tests/<area>/<case>/case.toml` (or
`tests/bugs/<bug-id>/case.toml` for a numbered defect) with required fields:
stable `id`, `owner`, `area`, `kind`, fixture paths, positive/negative controls,
expected compile/exit/stdout/stderr/trap or report observation,
interpreter/LLVM/QBE applicability, target/profile matrix, CI tier, mutation or
class-gate expectation, and bug/roadmap/claim IDs. Unsupported matrix legs name
an explicit reason; absence never means “not run.” Generate
`.build/tests/registry.json` and `.build/reports/test-inventory.md` from those
manifests. `scripts/tests/runners/<area>.sh` consumes the generated registry and
remains independently runnable; CI and the legacy aggregate invoke those area
runners rather than re-encoding fixture lists. The first pilot must migrate one
identity-sensitive area with a positive control, negative/fail-closed case,
interpreter-vs-compiled observation, and mutation kill, proving the schema
handles the hardest existing test shape.

Path-to-job routing comes from the same ownership records. A source path names
its owning component and consumer jobs; a gate names its owner, CI tier, and
hosted job. Generate the pre-push selection consumed by `.githooks/pre-push`
from those records, while `run_ci_gates_local.sh --job` continues to execute a
hosted job's own declared gate set. The generator must reject an unowned path,
an ownerless gate, an unknown job, and disagreement between local-hook and
hosted-CI membership. Central consumers such as `Main.lean` declare every
affected job explicitly. A broad `Concrete/*` or filename-substring catch-all
may be a temporary enumerated fallback, but may not silently override exact
ownership or count as complete coverage.

The same records own the COMMAND, not only membership. Local execution and
hosted CI consume a generated command registry with a stable gate ID, argv,
working-directory policy, timeout/resource policy, and liveness assertion.
Do not recover commands by scraping workflow YAML with a shell regular
expression: the local runner once truncated the trust gate at `2>&1`, produced
an invalid `--trust-gate 2>` command, and counted that permanent parser failure
as a red tree while never executing the named gate. Generation validates every
command syntactically before a run, but syntax alone is containment—a
syntactically valid truncation must also fail the per-gate start/end or expected
assertion-count liveness check. Gate the redirection, pipeline, multiline,
environment, and timeout command shapes, and require exact local/hosted command
identity except for declared platform adapters.

This is forced by four measured misses in the hand-written router:
`check_operational_vc_auto_discharge.sh` was invisible to Proof changes,
`example_manifest.txt` was omitted for new examples, the release-bundle proof
status consumer lived in another job, and `Main.lean` reached every report
surface while routing only to hygiene. Each was caught by hosted CI after the
local hook selected too little. Gate the class, not those four spellings: remove
one generated owner/job edge and prove the hook-selection parity gate turns
red.

Every example carries `example.toml` with stable `id`, lifecycle class
(`tutorial`, `showcase`, `workload`, `evidence`, or `known_hole`), owner,
status, entrypoints, supported targets/profiles, gates, evidence/claim IDs, and
its proof/fixture paths. While an item remains under `examples/`, all five lifecycle classes use the same
`examples/<category>/<name>/{src,proofs,fixtures,expected,README.md}` project
shape; absent optional children are allowed and alternative shapes within that tree are not. The
corpus manifest may move a whole regression/workload item to its actual owning home.
`examples/evidence/hmac_sha256` is the proof-colocation migration control.
Reusable theorems may move from an example into `Concrete/ProofKit` only with a
second consumer and a gate for both. A regression extracted from an example
receives a case manifest under `tests/bugs/` before the example copy is
removed.

That common shape describes items that remain in `examples/`; it is not an admission rule that
keeps every historical test program there. The corpus-manifest migration classifies each existing
directory and moves regression-only, hostile, performance and bulk-workload material to its owning
test/workload home before removal. Public `showcase` admission is capped by the three-flagship
portfolio below. A new showcase must replace or deepen an existing portfolio role rather than
silently creating a fourth.

Documentation and research classification lives in the ownership manifest with
`path`, `class`, `owner`, `status`, and `canonical_source`. Normative reference
records must name their backing gate/fixture; historical and archived records
cannot satisfy a current claim. The site build generates its reference input
from canonical `docs/` records into `.build/site/`; it fails if an authored
site file has the same canonical ID. Research status is exactly `active` or
`archived`, and every active record names a roadmap/decision owner.

Generate the Lean dependency graph as
`.build/reports/lean-import-graph.{json,dot}` and check it with
`scripts/tests/check_import_architecture.sh`. The target DAG is the compiler
sequence shown above plus these consumer rules: `Semantics` is the canonical
shared fact owner; interpreter/backends/proofs consume typed Core/SSA and
Semantics; Evidence consumes typed facts and proof/replay results; Report
consumes facts/evidence; Driver may orchestrate every layer. Reverse imports,
Report/Driver semantic derivations, and direct backend-to-frontend recovery are
forbidden. Baseline edges that cannot yet move are enumerated exceptions tied
to R-0114–R-0118; no wildcard exception or baseline growth is permitted.

Artifact enforcement snapshots a clean tracked worktree, runs representative
build, test, proof, reduction, site, benchmark, and release commands, and fails
on a new output outside `.build/` or the release-only `dist/` projection.
Ignoring a leaked artifact is not a passing fix. Session/worktree metadata
lives outside the repository; minimized reproducers are either temporary
`.build/reductions/` outputs or promoted manifest-backed bug cases.

Source-mutating mutation tests run in disposable worktrees under
`.build/tests/`, never in the developer's active source tree. Until every
mutation suite migrates, the compatibility harness takes a per-worktree
exclusive lock, refuses staged or unstaged dirty target files, stores backups
under a unique directory without flattening path names, restores on normal
exit/INT/TERM, compares every target hash before releasing the lock, and fails
on any stray mutation marker or inexact restoration. Shell traps are only
containment: a foreground build may defer them, so they do not satisfy the
steady-state isolation requirement.

Every legacy-area migration is one reviewable ownership transaction: add and
validate its source manifests, run old and generated selection in comparison,
move the owned files, update imports/links/gates, remove the old registry or
copy, and decrement the published baseline in the same slice. A compatibility
shim is permitted only to preserve a public import or command while its named
owner task lands; it has a removal task and cannot become a second permanent
path. Do not leave “temporary” duplicate fixtures, proofs, documentation, or
semantic catalogs after the comparison passes.

Land these slices in order:

1. **One canonical ownership map.** Add
   `config/repository-layout.toml`,
   `docs/REPOSITORY_LAYOUT.md`, and the umbrella gate named above. The manifest
   classifies compiler layers, test areas, example lifecycle, proof ownership,
   normative documentation, research status, and generated-artifact roots.
   Every new file must have one owner and one lifecycle; the manifest is an
   index into existing semantic facts, not a second compiler database.
2. **Compiler dependency ratchet.** Record and gate the intended direction:
   frontend → resolve → check → typed Core → mono → SSA → backend, with
   Semantics as the canonical owner of arithmetic/traps/layout/effects,
   Proof/Evidence consuming typed artifacts, Report rendering existing facts,
   and Driver orchestrating without deriving semantics. Generate the import
   graph from Lean modules. Baseline current exceptions, forbid new ones, and
   let R-0114–R-0118 remove the baseline as identities, `ProgramFacts`, typed
   pass results, and consumer matrices land.
3. **Manifest-driven test ownership.** Define one small case manifest carrying
   area owner, fixture/control, expected observation, interpreter/backend
   matrix, CI tier, mutation/class gate, and bug/roadmap ID. Pilot one semantic
   area end to end, extract its block from `run_tests.sh`, and generate its
   registry. The legacy script becomes a shrinking dispatcher and may not grow.
   New flat `check_*.sh` files or fixtures without a manifest fail; legacy
   tests migrate when their owning task touches them. Do not replace the flat
   directory with one universal mega-runner—keep small area runners over one
   generated registry. Fixture prefixes such as `adversarial_`, `regress_`,
   and `error_` are migration evidence for area ownership, not a request for a
   one-shot directory rename. Generate pre-push path-to-job routing from the
   same ownership records, generate the local/hosted command registry rather
   than scraping workflow text, and prove selection, command, and liveness
   parity; do not create a second routing manifest.
4. **Canonical corpus homes.** User-facing programs are classified as
   tutorial, showcase, workload, evidence example, or known-hole example.
   Compiler regressions and bug witnesses live under a checked test-corpus
   owner, not among teaching examples. Example-specific proofs live with the
   example; reusable proof infrastructure remains compiler/ProofKit-owned.
   Normative language text is authored once under `docs/`; site pages consume
   or generate from it rather than maintaining hand-edited semantic mirrors.
   Make `site/content/reference/` a generated projection of the canonical docs
   (or links to them) so a site build cannot depend on a second hand-edited
   copy. Research records carry active/archived status and a roadmap/decision
   owner. Do not physically regroup the 126 docs before R-0438's generated
   claim/index view exists; organize discovery by semantic ownership first.
5. **Hot-file authority split.** Extract the stable diagnostic-code catalog
   from `Report.lean` into one canonical diagnostic data/module owner that the
   defining passes validate against and reports consume. `Report` renders
   diagnostics; it does not independently own their identity or wording.
   Split other renderers only when their semantic owner or compile/merge
   pressure requires it, not to hit a line-count target.
6. **Artifact hygiene.** Generated binaries, LLVM/QBE text, reports, proof
   replay bundles, reductions, and test outputs go under the canonical
   project-local artifact root. `.audit_me/` is emptied through R-0010's
   promote-or-delete decision; worktree/session state is ignored or stored
   outside the repository. Move every source-mutating mutation harness into a
   disposable worktree; the locked backup/restore harness is only an interim
   migration aid.

The migration is ratcheted, not big-bang: publish counts for unclassified
files, flat test scripts, wrong-layer imports, regression-shaped examples,
detached example proofs, normative mirrors, and generated source-tree
artifacts; every touched area must reduce or preserve each count, never increase
it. Directory moves happen in the semantic task that establishes the owner so
history remains reviewable and active worktrees do not receive path-only
conflicts. The stdlib's pure/alloc/hosted layering is made visible in the
ownership manifest first; physical `std/src` subdirectories require a
namespace-stable module rule and measured navigation benefit rather than being
ratified by this task.

Gate an unclassified new test/example/research record, a wrong-layer Lean
import, a report or driver module deriving a semantic fact privately, a
regression fixture added as a tutorial, a second normative site copy, and a
generated `.ll`/binary outside the artifact root. Gate growth of
`run_tests.sh`, a second independently maintained E-code catalog, an unowned
path/gate, a central consumer missing one declared job, and local/hosted job
membership divergence. Each must fail with the owning directory/task and
remedy. R-0010 owns bug-corpus promotion, R-0438 owns generated semantic/claim
inventories, R-0114–R-0118 own compiler-layer migration, R-0135 owns example
refresh, and R-0151 owns proof colocation; this task supplies their shared
layout contract and prevents parallel registries. Dead performance harnesses
remain owned by R-0416–R-0419; this task does not delete tracked or untracked
research notes merely because they are at repository root.

**Worktree and publication invariants.** The operating rules in
`docs/project/CONCURRENT_WORK.md` are part of this ownership contract, not advisory
prose. One writer owns one worktree; verification runs from a clean tree at an
exact commit; the verified SHA must equal the push target immediately before
publication; hosted CI is selected by the full 40-character SHA and `push`
event; and an empty CI result means UNVERIFIED, never success. Publication code
must wait on the actual lock/PID/remote ref it owns rather than infer state from
process-name matching. Add executable controls for each invariant so a warning
cannot be ignored and an adjacent-but-wrong object cannot satisfy the check.

**Completion state for R-0447:** close this task only when:

- every new file class has a machine-readable owner and lifecycle;
- a new test can be registered through the case manifest without growing
  `run_tests.sh`, and one semantic area demonstrates fixture, control,
  interpreter/backend matrix, CI selection, mutation, and local replay end to
  end;
- pre-push routing is generated from the ownership records, every path and gate
  is owned, central consumers fan out explicitly, and local/hosted job
  membership, command identity, and liveness agree without workflow-text
  scraping;
- concurrent work and publication are fail-closed: one writer per worktree,
  clean exact-SHA verification, verified SHA equal to the push target, CI queried
  by full SHA plus `push` event, and an empty CI result rejected as unverified;
- generated output has one enforced artifact root and a clean build leaves
  source directories unchanged;
- source-mutating mutation suites execute in disposable worktrees and cannot
  modify the developer's active source tree;
- site reference content cannot drift from its canonical document;
- the generated Lean import graph rejects every new wrong-direction edge;
- examples, regressions, example proofs, reusable proof infrastructure,
  normative documents, and research records have distinct checked
  classifications;
- `Report` consumes the canonical diagnostic catalog instead of owning a
  second statement of diagnostic identity; and
- the measured legacy-debt baselines are published and no tracked count can
  increase silently.

R-0447 establishes that contract, one complete pilot, and the no-new-debt
ratchets; it does not wait for every legacy file to move. The owner tasks above
complete the incremental migration. At that later steady state,
`run_tests.sh` is a small dispatcher or gone, test fixtures are navigable by
semantic area and bug owner, regression witnesses no longer masquerade as
teaching examples, and an example's program/proofs/fixtures/replay data are
colocated while reusable proof machinery stays central. Compiler imports
follow the declared pipeline with the exception baseline eliminated; reports
and drivers contain no private semantic implementations; normative text has
one authored copy; and ordinary builds leave the source tree unchanged.

The usability criterion is deliberately human-sized: a contributor can
determine where an artifact belongs, who owns its semantics, how it is tested,
and whether it is generated without searching the whole repository or editing
a global hand-maintained registry.

## R-0019

_Stub: [ROADMAP.md](../project/ROADMAP_TASKS.md#task-r-0019) · disposition: merged · owner(s): R-0031_

Then:

- expose slice views first; remove ordinary raw element pointers;
- make allocation timing consistent or documented (`HashMap::new` currently
  allocates while `OrderedMap::new` is lazy);
- keep HashMap/HashSet storage unsafety internal, so callers need `Alloc` but
  not `Unsafe`;
- gate tombstone/full-table probing, duplicate replacement, non-Copy key/value
  destruction, clear/reuse, and growth under adversarial hashes;
- define BitSet's logical-length versus capacity contract for set/unset/test,
  union/intersection, out-of-range bits, and clear/reuse; keep storage `Unsafe`
  internal and make allocation the only ordinary caller-visible effect;
- add `Vec` insert/stable-remove/retain/sort/search and an ownership-aware map
  entry/upsert API only when independent workloads reach the pull threshold;
- keep unordered traversal explicitly unspecified and ordered traversal
  ascending; do not promise insertion order accidentally; and
- do not add iterator traits, lazy adapters, or reference-returning entry APIs
  until the reference model can enforce them.

Keep a semantic naming/coherence ledger rather than a mechanical rename sweep:
process name stuttering; duplicate BitSet verbs (`set/unset/test` versus
`insert/remove/contains`); BitSet logical length versus population count;
unchecked endian readers; empty `with()`; bool versus three-way comparator
contracts; and insert return shapes. Rename only after documenting whether the
difference carries ownership, ordering, replacement, or failure information—
for example HashMap returning a displaced value is not equivalent to Set
reporting whether membership changed.

## R-0029

_Stub: [ROADMAP_BACKLOG.md](../project/ROADMAP_BACKLOG.md#task-r-0029) · disposition: merged · owner(s): R-0065_

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

## R-0051

_Stub: [ROADMAP.md](../project/ROADMAP_TASKS.md#task-r-0051) · disposition: merged · owner(s): R-0014_

 Add an explicit **boundary-module pattern**, borrowing the useful part of
 Elm's ports without copying Elm's web-application architecture: authority
 enters through a small named module, the wrapper exposes a narrow safe API,
 and the audit report names the capability, trusted/FFI boundary,
 assumptions, allocation behavior, and evidence class. Concrete boundary patterns:
 `std.fs.boundary` wraps ambient filesystem entry points behind directory
 handles; `std.net.boundary` wraps socket creation before pure parsers see
 bytes; `std.libc.boundary` wraps extern calls before safe code sees owned
 values. Add `docs/stdlib/STDLIB_BOUNDARY_MODULES.md`, checked snippets backed by
 `tests/fixtures/stdlib_boundary/{fs_boundary,net_boundary,ffi_boundary}/` (or the corpus
 manifest's canonical fixture home), and
 `scripts/tests/check_stdlib_boundary_modules.sh`; the gate must prove that
 the public safe wrapper surface has narrower authority than the underlying
 trusted/extern implementation and that the audit transcript shows both
 sides of the boundary.

## R-0483

_Stub: [ROADMAP.md](../project/ROADMAP_TASKS.md#task-r-0483) · disposition: done · owner(s): —_

**Earlier status (2026-09-16): REPAIRED AND GATED.** The three properties are fixed by
structure rather than by checks, the attestation migration is done and validated, and
`check_view_lifetime.sh` (13/0) runs in both the fast local suite and CI.

| property | before | after |
|---|---|---|
| lifetime | `ByteCursor`/`Text` stored a captured `*const u8` | `ByteCursor` stores `off`/`len`/`pos` and takes the buffer on every access, re-deriving bounds from the buffer it is GIVEN |
| identity | `describes` compared a length and read the wrong buffer silently | brand removed; a view is coordinates and says so, with `fits` naming a bounds test |
| validity | `try_text` validated once into a borrowed view | `to_text` COPIES into a `Text` that owns immutable storage |

`Text` is now linear rather than `Copy`, which is the visible price of the guarantee:
validation that survives mutation needs storage the source cannot reach. Raw access did
not disappear but stopped being invisible — `RawCursor` carries `with(Unsafe)` on
construction and on every read, so the caller obligation appears in the caller's type.
`ByteCursor` and `ByteView` need no `trusted impl` at all any more, because with no
stored pointer and no brand nothing in them crosses a trust boundary.

**The migrated parser is `examples/packet`,** and it is the clearest evidence. Its
header claimed `compute_checksum`, `decode_header` and `extract_payload` were PURE while
they took `*const u8` and read through `ByteCursor::from_raw`; the claim was a comment,
not a checked fact. All three now report `(pure)` truthfully, the example contains no
`trusted` function at all, and its **predictable** profile still reports exactly one
failing function (`main`, for blocking I/O) — 1 failed / 13 passed, unchanged. Keeping
that property forced `extract_payload` to write into a caller-supplied buffer rather
than return an owned one: returning `Bytes` would have needed `Alloc` and cost the
example the predictability it exists to demonstrate. That is the measured allocation
cost of the repair, and it was paid by design rather than absorbed.

**Attestation migration: regeneration sufficed; the full R-0482 migration was not
required.** Mapping was built on full scoped rows, not the collapsed
module/declaration summary. 63 rows collapse to 57 on `(table, module, declaration,
implementation)` because six rows differ only by package — the legitimate many-to-one
case — so packages were paired by identical definition set, with any package present
unchanged on both sides pinned to itself. That yields 21/21 packages mapped and a total
symbol rename of 42 entries, each verified to exist in the regenerated file before
being applied. 38 references were rewritten across `Concrete/Proof/Proof.lean` and
`proofs/Examples/HmacSha256/Proofs.lean`; no old symbol survives anywhere.

Evidence was checked after, not assumed: `crypto_verify` is back to **4 proved, 0
stale, 0 closure-unjustified** and `elf_header` to **5 proved, 0 stale, 0
closure-unjustified**, matching their pre-change baselines. No stale proof was retained
to make anything green — implementation identities were confirmed unchanged for every
mapped row, which is precisely why this was a rename and not an evidence transition.
R-0208's boundary is untouched: nothing new was minted.

**Gating.** `check_view_lifetime.sh` asserts the replacement API positively (a valid
borrowed read still returns its byte; a validated `Text` keeps its bytes after the
source mutates), asserts the runtime refusal that cannot be a compile error (a read
against an emptied buffer returns `Err`), asserts the `Unsafe` boundary still bites
(E0521), and asserts each historical reproduction now fails FOR THE STATED REASON
rather than merely failing. That last distinction is the point: a compile error caused
only by deleting an API proves the old spelling is gone and nothing else, so the
positive controls carry the weight. Five further tests in `std/src/numeric.con` cover
the replacement directly. Stdlib 313/0, suite 1713/0.

**Remaining at 2026-09-16 (resolved 2026-10-09 by `BoundView`, above):** `ByteView::of_cursor` still produces a view
whose coordinates are meaningful only against the buffer the cursor was reading, which
the coordinate contract permits but does not make obvious at a call site. Owner-bound
parsed results — where substitution is unrepresentable rather than merely
out-of-contract — remain future work and are the right answer whenever identity
matters.

**Classification (2026-09-15, checked directly).** The extension to `Text` and
`ByteView` is done, and it separates two defect classes from one sound design:

| API | stored shape | destroy-then-read | class |
|---|---|---|---|
| `ByteCursor::from_bytes(&Bytes)` | `*const u8` + len | accepted (rc 0) | representation |
| `Text::from_string(&String)` | `*const u8` + len | accepted (rc 0) | representation |
| `ByteView::cursor(&self, &Bytes)` | coordinates | accepted (rc 0) | conversion |
| `ByteView::try_text(&self, &Bytes)` | coordinates | accepted (rc 0) | conversion |
| `ByteView::byte(&self, &Bytes, i)` | coordinates | **rejected, E0205** | lifetime-sound |

`ByteView` stores `off`/`len`/`buf_len` and no pointer, so the representation is
already correct; `cursor` and `try_text` extract a raw pointer and return a value of
the representation class, giving the advantage back. `ByteView::byte` demands the
buffer on every access, so destroy-then-read cannot be written — the owner is gone
and the call does not typecheck. That row is the positive control for the repair
direction: the intended design is demonstrated to work inside this stdlib today, not
merely proposed. Accept/reject is a compile-time observation and is the reliable
signal; the freed byte remains undefined and is not an oracle.

**That exclusion of the raw constructors is RETRACTED (2026-09-15).** An earlier
revision here put `ByteCursor::from_raw` and the `Text` raw constructors out of scope on
the grounds that they take a pointer the caller already had to obtain. Holding a pointer
is not permission to dereference it later, and the two obligations are separate.
Checked, and the call sites settle it:

```
pub fn from_raw(data: *const u8, len: u64) -> ByteCursor     // no capability
pub fn read_u8(&mut self) -> Result<u8, CursorError>          // no capability

examples/packet/src/main.con
  fn compute_checksum(data: *const u8) -> u16 { ... }         // plain fn
  fn decode_header(data: *const u8, len: u64) -> DecodeResult // plain fn
```

Neither caller is `trusted`, neither declares `with(Unsafe)`, and the file has no
module-level grant. So an ordinary function takes a raw pointer, builds a cursor and
dereferences through it with no obligation anywhere in its signature — while a
user-written struct doing the same by hand is refused with E0521 (`unsafe_boundary`).
The `trusted impl` permits the implementation's operations; it does not surface the
caller's. The comment above `compute_checksum` reads "Uses ByteCursor — no raw pointer
dereferences in this function", which is the laundering stated in the source.

These constructors therefore belong in the audit, and the reachable-without-`Unsafe`
dereference path is also R-0484 evidence: it is authority laundering through a trusted
wrapper, not only a lifetime question. The stack-array sites in `numeric.con`'s own
tests remain legitimate use of a raw constructor; that was never the issue.

**Migration dependency — 2026-09-15 attempt, then reverted; assessment below owns the
current sequencing.** A brand carrying buffer token and mutation generation was implemented end
to end: a `gen` counter on `Bytes` bumped by every mutator, `ByteView` and `ByteCursor`
branded with `(raw_ptr, gen)`, and `describes` checking identity and validity before
bounds. It works — the same-length wrong-buffer fixture flipped from returning `b`'s
byte to being refused, the valid-access control still returned 65, and `std` typechecked
clean.

It does not land as-is. Changing `std` renames every symbol in
`GeneratedAttestations.lean`, breaking the hand-written proof links naming them, and
with the old attestations still in place `crypto_verify` and `elf_header` fell from 4
proved to 2 proved plus 2 closure-unjustified. Reverting restored 4 proved and 1707/0.

**What the churn actually is, measured in an isolated worktree by adding ONE unused
function to `std` and changing nothing else.** Scope identity, implementation identity,
generated names and added entries are measured separately:

| question | answer |
|---|---|
| does package scope identity move for existing definitions? | **yes — 39 of 53** |
| do existing definitions' IMPLEMENTATION identities move? | **no — 0 of 53, byte-identical** |
| do their generated symbol NAMES move? | yes — the same 39, because the name embeds the package digest |
| does the file merely gain an entry? | no — 63 definitions before and after, none added or removed |

(Keyed by `(module, declaration)`, which collapses 63 definitions into 53 unique keys
where a declaration appears in more than one package.)

So an added, unreferenced `std` definition moves dependents' *scope* identity while
every existing implementation identity stays exactly as it was. The measured implementation
bytes did not change; this alone does not establish that scope-bound evidence and
dependency attachments remain current. The immediate build failure is a name — 23
hand-written references across `Concrete/Proof/Proof.lean`,
`proofs/Examples/HmacSha256/Proofs.lean` and `proofs/Examples/ProofPatterns/Proofs.lean`
stop resolving, and `lake build` fails on unknown identifiers.

**That is a migration dependency, not a bar on the safety repair.** Row 0 permits safety
repairs, and stable implementation identities make a mechanical reference migration
plausible. Its totality must still be checked against the full scoped rows: the
module/declaration summary above collapses legitimate package distinctions. The open
question belongs to the R-0482 atomic identity migration: whether regenerating those references preserves honest, fail-closed evidence
without introducing a new authoritative evidence transition — which is what R-0208 (the
Lean #14576 fire drill) constrains. An earlier revision of this entry called the repair "blocked behind"
R-0208 and the R-0482 migration; that overstated it and is corrected here.

Unaffected either way: fixtures, classification, R-0484, and any repair confined to
files outside the attestation surface.

**Lifetime and identity fail separately, and step 3 was right to name them apart.**
Two further results close the gaps this task listed as open:

- **Identity is not checked at all.** `describes` compares `buf.len() != self.buf_len`
  and nothing else, so a *different* buffer of the same length passes the brand.
  `ByteView::byte` over a view of `a`, read against an equal-length `b`, returns `b`'s
  byte (exit 91). The accessor demands an owner, not the right one — so the
  lifetime-sound row above is not a validated accessor, and the repair needs buffer
  identity, not only a buffer argument. `examples/byte_view/wrong_buffer` tested only
  the wrong-LENGTH case and reported that all unsafe uses were rejected; its message
  now states what it checked.
- **Lifetime is not only about `destroy`.** A `ByteCursor` held across ordinary
  `push`es that outgrow capacity is accepted with no consumption anywhere and the
  owner alive throughout: growth reallocates and the captured pointer goes stale. Any
  repair scoped to explicit destruction would miss this.

- **Validity does not survive mutation either.** `try_text` checks UTF-8 once and
  nothing revalidates, so mutating the source under a validated `Text` leaves it
  yielding `0xFF` — again with no destruction and the owner alive. A validated `Text`
  is a claim about the past, not a standing property.

**Step 3's three-way split is therefore measured, not anticipated.** Lifetime,
identity and validity fail independently; only lifetime holds anywhere today, and only
in `ByteView::byte`. A repair addressing lifetime alone would leave the two SILENT
failures standing — a wrong-buffer read and an invalid `Text` both present as ordinary
successful calls, where the lifetime defect at least corrupts something observable.

1. Preserved and replayed: the ByteCursor pair, the Unsafe boundary probe, and seven
   further fixtures live in `tests/regressions/view_lifetime/`, covering all three
   properties. They are retained inputs, not yet wired gates; wire them with the
   repair. The investigation this step asked for is complete.
2. For confirmed failures, prefer pointer-free stored views and cursor positions with an explicit buffer
   borrow on each read. Use enforceable scoped callbacks where needed, or an owning
   representation when retention is required. A `Copy` raw pointer inside a struct
   must not bypass the safe-reference escape rule.
3. Specify bounds, buffer identity and content validity separately. Length equality
   is not identity, and a prior UTF-8 validation does not survive arbitrary source
   mutation. State whether a handle is reusable coordinates or tied to a particular
   buffer/content version; enforce the chosen contract without implying branding
   where only bounds are checked.
4. Migrate one bounded binary parser and affected public stdlib APIs/examples. Keep
   malformed input recoverable and test early returns, cleanup, buffer replacement,
   mutation, same-length wrong buffers and boundary arithmetic.

**Exit:** ordinary callers cannot retain dangling access through these APIs; positive
zero-copy parsing remains usable; invalid lifetime/content transitions are rejected
or safely revalidated; class-level negative controls cover all public constructors
and access paths. Preserve the safe-order control and require the unsafe-order case
to be rejected or made safe by the repaired API; never accept a particular freed byte
as success. Keep the raw-pointer cast/dereference negative controls so library repairs
cannot silently weaken the Unsafe boundary. If an investigated path already rejects,
record the enforcing rule instead of redesigning it. Report remaining trusted operations explicitly. Record allocation,
copying and checked-access costs and retire the unsafe safe-callable predecessors.
R-0486 reuses this parser after the contract prerequisites; this task does not claim
heap proofs or emitted-binary correctness.

## R-0484

_Stub: [ROADMAP.md](../project/ROADMAP_TASKS.md#task-r-0484) · disposition: done · owner(s): —_

The record below is the pre-closure inventory, kept for its commit trail.

**Earlier status (2026-10-05): core checkpoint on main; follow-up integration pending.**
The first implementation merged at `a7c9cf1c`: encoding A, capability parameters on
structs, mandatory extern effect declarations (E0116/E0117), the `trusted` reversal,
and `Writer<C>`/`Reader<C>`. Main is at roadmap cleanup `8960c97d` at this inventory.
The checkpoint's approved full-campaign timeout exception does not certify the campaign
or waive a later release's requirements; qualification retains its separate queue owner.

| increment | recorded state; not a claim of integrated completion |
|---|---|
| F1/F9 construction and fork repair | Pushed as `r0484-finish` at `a084d91e`: only `spawn` constructs `Child`; public `process_fork`/`ForkResult` removed. Branch health passed; [full CI run 37354702330](https://github.com/unbalancedparentheses/concrete2/actions/runs/37354702330) is still running at this inventory. Supported spawn runtime assumptions remain explicit. |
| Shared assumption summaries | `b14aaba3`, followed by bug 074 and canonical manifestless package identity in `873476d7`; carried on the qualification branch, not yet on main. |
| Conclusion qualification | `f3510e04` supplies shared qualification and explicit unloaded-call gaps. This is an intermediate R10 increment, not complete assumption coverage. |
| Authority terminology and JSON API v2 | Committed as `50bfc385`: purity claims removed from these authority reports; schema and consumers migrate without an `is_pure` alias. Integration and final validation remain pending. |
| Combined tree | Local `r0484-integrated` at merge `37496123` combines F1/F9 and qualification. Additional corrections are uncommitted; that SHA is not the final validation candidate. Record the final committed SHA, identity and CI run after those corrections land. |

A branch gate or F1/F9-only CI result cannot stand in for combined-tree validation.
On the final integrated commit run the selected gates, both suite forms, identity
freshness, campaign golden and mutation anchors in a clean clone, then full CI under
the explicitly recorded campaign policy. Explain identity migrations by source/package
mapping and account for every changed snapshot; regeneration alone is not evidence.
Only record a follow-up as merged after main contains its validated commit.

**R10 remaining closure work:**

- Report trusted memory-safety boundaries as a named assumption category, with the
  responsible function, absorbed obligation and dependent claims. A count alone is
  insufficient; preserve provenance and package identity.
- Represent coverage of the audited raw-descriptor restrictions and construction/caller
  paths. Encoding A's lack of typed conversion syntax does not establish that there
  are no descriptor assumptions. Unimplemented coverage remains explicit, not an
  empty category presented as checked.
- Make capability explanations follow dependency summaries and name the dependency
  callee/binding instead of stopping at an unknown node. Preserve unresolved direct
  and indirect calls as gaps, including in text, JSON and query consumers.

**Report meaning and schema migration:** declared capability allowances, reachable
assumptions and coverage are separate facts. An over-declared `with(Console)` header
continues to display Console; “no reached foreign binding” establishes no narrower
authority result, because primitives or other operations can supply authority. R-0487
owns over-declaration analysis. “No external authority” is not purity, non-mutation,
termination or proof eligibility; retain the `&mut` mutation acceptance example.
Without sufficient analysis, describe only the declared set or explicit incomplete
coverage. Empty foreign assumptions never imply complete coverage.

Slice 5 includes the v1 → v2 migration in
[COMPILER_API.md](../compiler/COMPILER_API.md): authority facts replace `is_pure`
with `no_external_authority`, and declaration-only query facts use
`no_declared_capability`. Producer and `concrete diff` share the version constant;
consumers reject v1, absent versions and unsupported future versions before comparing
facts, with a regeneration diagnostic and a valid-v2 acceptance control. No misleading
purity alias is retained. Audit snapshots, query/schema fixtures and documentation
must agree. This is branch implementation until integration is validated and merged.

**Proof-admission consolidation: separate change, required before R-0484 closes.**
After the combined reporting/FFI checkpoint, make admission consume the shared
summary's reachability, assumptions and coverage. Admission retains its own eligibility
judgment; it must not derive eligibility from a headline, an empty authority set or a
renderer. Retain an old/new corpus verdict comparison and explain every delta. Missing
dependencies, unresolved direct/indirect calls and dropped assumption edges must not
silently gain admission; retain valid acceptance controls as well as rejection and
mutation controls. Publish intermediate checkpoints with R-0484 open until this and
the remaining R10/documentation criteria pass. This uses the existing R-0484 owner
and does not introduce a second analysis pipeline or reorder the queue.

The final docs/examples and authority audit, including disposition of the four external
doc pages not yet located, remain required. F7/F8 repairs stay with R-0013 and validation
with R-0030. Typed-descriptor extensions remain a separately gated second step.

(Earlier status, 2026-09-29: design decided; reports were repaired and the admission repair
went live 2026-09-26, and both remain as the conservative backstop.)

**THE DECISION (2026-09-29, refined 2026-09-30): `with(...)` is the complete list of a
function's external authority.** The goal it serves: reading a function's header tells you
what external authority it can use, in one place, however it came by that authority. It
does not say which file is touched, what is mutated through arguments, whether the
function terminates, or which inputs influence which outputs — those need separate
contracts. The canonical statement is
[docs/language/HANDLE_CAPABILITIES.md](../language/HANDLE_CAPABILITIES.md) (rules R1–R10);
the summary below is the original three rules plus the 2026-09-30 additions.

1. **Handles carry their capability in their type.** `Writer<C>` and `Reader<C>`, with `C`
   a capability parameter (the existing `cap C` mechanism, extended from functions to
   structs). `console_writer() with(Console) -> Writer<Console>`; a fixed-buffer writer
   can be `Writer<{}>`, while a growable-buffer writer requires `Writer<Alloc>`.
2. **Using a handle requires its capability.** `Writer<C>::write` requires `with(C)`, so a
   helper reads `fn print_bytes<cap C>(w: &Writer<C>, b: &Bytes) with(C)`. At each call `C`
   is instantiated, and the ordinary rule — a caller's `with(...)` covers its callees' —
   applies unchanged. This is the callable-values rule (a `fn() with(Console)` value
   requires `Console` to call) applied to the one type that escaped it: a `Writer` is a
   function pointer plus a context.
3. **`trusted` absorbs `Unsafe` and nothing else.** A trusted body vouches for memory
   safety; it never hides an effect. `console_write`/`console_err_write` violate this today,
   and so do the effectful `trusted extern` declarations in `std/src/libc.con` (`write`,
   `read`, `fopen`, `socket`, `send`, `recv`, `getenv`, `exit`, …), which require nothing and
   are how a trusted body performs I/O undeclared. `docs/platform/FFI.md` already reserves
   `trusted extern` for side-effect-free foreign functions; std does not follow it.
4. **(2026-09-30) `trusted` also absorbs the `Unsafe` of calling a plain `extern`.** This
   reverses the rule stated in `SAFETY.md`, `FFI.md:109` and `CAPABILITY_FACTS.md:142` and
   pinned by `error_trusted_extern_needs_unsafe.con`/`error_trusted_no_extern.con`, which
   change with the implementation. Without it, `Unsafe` would spread into every `Writer`
   user.
5. **(2026-09-30) Every foreign binding declares its effects; an undeclared binding is
   refused.** There is no "unknown" state. `trusted extern` requires being safe for every
   argument the types permit, with no undeclared effects, so `memcpy`/`memcmp` become
   plain `extern` behind trusted wrappers. Foreign declarations and descriptor
   conversions are audited assumptions, listed in reports with their site, responsible
   boundary and dependent claims.

**Style rule:** concrete by default (`&Writer<Console>` with `with(Console)`); generic `C`
only where a function is genuinely used with more than one kind of handle — std, and helpers
tested against an in-memory writer. `with(C)` reads as "exactly what the caller hands me",
which is a precise bound, not a vague one.

**Alternatives rejected, and why.** A broad `IO` capability says "does some I/O through
something" — vague, duplicative of the parameter, and forces in-memory helpers to claim I/O
they do not perform. Authority-by-possession (R-0487's original plan: headers stay silent,
reports infer `performs`) fails the one-place test: the header of `print_bytes` would still
be empty and only a tool could say it writes.

**Measured cost (2026-09-29).** 14 of 4,767 functions in the repository take a `Writer` or
`Reader` outside `io.con` (6 in `std/src/fmt.con`, 8 in `examples/hexdump`, `cli_tool`,
`png_chunks`, `base64_cli`); no struct holds one as a field. The requirement does not
cascade: to call a handle-using function you must pass a handle, which you either received
(so you are in the chain and gain `C`) or created (so you already declare the concrete
capability, because constructors require it). Everything above the creator is unchanged.
The cost grows where handles are stored in long-lived structs (`App<C>`), one capability
name per method.

**Slices (revised 2026-09-30):** (1) design doc — drafted as
[docs/language/HANDLE_CAPABILITIES.md](../language/HANDLE_CAPABILITIES.md); encoding A
selected for the first implementation (B a later option); construction/caller audit
taken 2026-09-30 in
[HANDLE_CAPABILITIES_AUDIT.md](../language/HANDLE_CAPABILITIES_AUDIT.md); classification
decisions D1-D4 settled 2026-10-01, findings F1-F10 are the slice 3 work list (F10 fixed
early as bug 072); per-handle `close` arguments recorded in R6 — slice 1 complete; (2) compiler:
effect declarations on externs, symbol aliasing, rules 4–5, capability parameters on
`StructDef` and struct types through parse, resolve, check, monomorphization and reports,
dependency-summary transport, and the assumptions section; (3) std FFI migration;
(4) migrate `Writer`/`Reader` and the 14 consumers, inverting `check_effect_opacity.sh`'s
pinned assertion that `print_bytes` is admitted; (5) correct the four pages (Spec, Why
Concrete Exists, Can I prove Concrete programs in Lean?, Nutrition Labels) and `README.md`,
restating an empty capability set as "no external authority". **Second step,** after the
hole is closed: typed bindings and descriptors, and defined behaviour for binding
`dup`/`dup2`/`fdopen` and for cross-classification aliasing.

**Completion criteria (expanded 2026-10-02; planned, not claims of implementation):**

- **Docs and examples move with behavior.** Update reference semantics in the same
  change as compiler rules, and migrate examples with std API changes so CI continues
  to compile and exercise them. Slice 5 finishes with a consistency pass over README,
  the four pages above, tutorials, generated API documentation and report snapshots.
  Include before/after migration examples for extern declarations, trusted wrappers,
  `Writer<C>`/`Reader<C>` and their callers, with remedies for the new diagnostics.
  Reconcile implemented versus planned status in this roadmap, the design and audit.
  Use “no undeclared external authority”; explain parameter mutation, termination,
  proof eligibility and foreign assumptions separately. Include the API-v2 migration,
  incompatible/missing-version rejection, and renamed report/query/diff expectations
  described above; do not label branch-only changes shipped.
- **Final authority audit.** Reconcile the binding inventory against the migrated std:
  every binding is classified or removed, every remaining descriptor restriction has
  named callers and ownership evidence, and every finding F1–F10 has a fixed or
  explicitly owned follow-up disposition. The known-symbol check is an omission
  detector, not proof that foreign declarations are honest. Assumptions retain their
  source, responsible boundary and dependent claims through package summaries.
- **Cross-package and trust controls.** Imported struct capabilities and extern effects
  obey the same rules as local declarations. Cover valid propagation, wrong kind/count,
  missing authority through helpers/methods/fields, and assignment of an effectful
  callback to an empty-capability handle. Trusted wrappers absorb only `Unsafe`;
  ordinary callers of plain externs still require it. Reversing the trusted rule must
  not create an intermediate release accepting undeclared foreign effects.
- **Code generation and ABI.** Capability arguments add no runtime storage and do not
  change layout. Preserve them until semantic checking and report attribution finish.
  Verify multiple capability instantiations have correct behavior and collision-free
  symbols without prescribing whether generated code is shared. Exercise multiple
  Concrete bindings to one C symbol through direct calls and function references,
  including across packages; incompatible foreign ABI signatures are diagnosed.
- **Validation evidence.** Record the exact committed tree and compiler identity for
  successful checks; validate imports and linking from a clean consumer. Retain both
  accepting and rejecting controls, and the design's required causal mutation tests.
  Identity regeneration must account for expected changes, not merely refresh pins.

**Owned follow-ups and boundaries:** R-0484 slice 3 owns resolution of F9's fork
ownership/runtime-profile contract: either constrain the API or state its supported
scope and audited preconditions, with regression coverage for supported behavior.
F10's bug 072 repair does not close F9. R-0013 owns F7/F8 error-honesty repairs (ignored
write/close results and unchecked `ftell` failure — its item 3, the external-return-value
audit, already names `ftell`/`fwrite`/`fclose`), validated through R-0030's fault injection;
these remain separate from the capability repair. R-0484's second step owns typed
descriptors and any future `dup`/`dup2`/`fdopen` support. Before those APIs are admitted,
define replacement, reuse, alias ownership, close responsibility and classification
preservation, with rejection/control cases. That step is workload-gated and is not
required to finish the first implementation. R-0488 remains the unscheduled usability
follow-up; this completion pass neither adds tasks nor changes the execution queue.

**Exit:** `print_bytes` declares `with(C)` and its call site reports `Console`; a function
with an empty `with(...)` cannot exercise undeclared external authority through a handle,
a trusted body or a dependency, subject to the named foreign assumptions. A gate carries
a positive control (a `Writer<{}>` helper stays capability-free) and a negative one
(a handle-using helper without `with(C)` is refused). The completion criteria above are
satisfied and outstanding follow-ups have explicit dispositions rather than being
counted as implemented guarantees.

**History before the decision (2026-09-26).** The remaining work was then framed as the
`requires`/`carries`/`performs` model; that framing is superseded by the decision above and
R-0487 is revised accordingly.

**THE DECISION THAT UNBLOCKED IT (2026-09-26).** The admission rule sat inert for a
release because enabling it moved three `pureCoreFns` links out of drift coverage
(11 → 8) and out of `replayTargetsOf`, and `check_purecore_proofs.sh` argues replay must
keep asking about pending claims "or the migration cannot clear them". That read as the
rule being too expensive. It was a **coupling**: obligation status was derived from the
admission predicate through a parameter merely *named* `eligible`, so refusing admission
also withdrew the obligation, and withdrawing the obligation removed the function from
maintenance. One predicate was answering four separate questions — *extractable*,
*admissible*, *replayable*, *proved* — which are now carried apart (see
`EFFECT_PROOF_BOUNDARIES.md` §4.1).

With status derived from *extractable* and admission carried beside it, the rule is on:
drift coverage is **11**, registered claims remain replay targets, extraction is
untouched (a refused function still produces subject facts, so higher-order programs do
not become dependency-free by being refused), and `check_purecore_proofs.sh` is 38/0.

**The rule that came out of it, and it generalises past this task:** an admission failure
must not erase an artifact from maintenance. Otherwise reclassifying an effect silently
removes claims from replay and drift detection while every report still reads green.

The report wording was the same defect in prose — *"is eligible for proof"* rendered
directly above `admission: REFUSED`, where `eligible` had come to mean "extractable"
internally but reads as "admissible". Now `obligation: extractable` / `registered proof:
none` / `admission: REFUSED — <reason>`, with the consequence sentence conditioned on
whether an artifact actually exists. `check_effect_opacity.sh` 18/0, two mutations
pinning both directions of the recoupling.

**WHAT IS STILL OPEN, and it is the substantive half.** This is the conservative repair:
`print_bytes` becomes *opaque* rather than *reporting that it performs output*. "Not
known pure" is a refusal to claim, not a fact about behaviour. The full model needs the
three-way split — **requires** (ambient authority the caller supplies) / **carries**
(authority travelling inside values) / **performs** (operational effects) — and the
`carries` leg is blocked on a language gap: `Writer` dispatches through a bare
`fn(*mut u8, *const u8, u64) -> Result<u64, IoError>` pointer, and `capParams` exists only
on functions (`AST.lean:355`), not on `StructDef`. So `Writer<E>` is not expressible and
call-graph inference cannot resolve where a handle's authority came from. (Superseded 2026-09-29: the
decision below adopts `Writer<C>` parameterising *required* authority, overturning the
earlier rejection; see R-0487's revision.) Inference-first, with
declarations as checked assertions rather than the primary input; `performs(no File, no
Network)` is harder than `performs(Output)`, because proving absence needs a completeness
the `unknown` state denies.

**ENFORCEMENT HOLE FOUND AND CLOSED 2026-09-20 — the header was not binding at all
across a sibling submodule.** Costing the trusted-consistency obligation named below
started by asking what `trusted` actually suppresses. It suppresses only the `Unsafe`
requirement on raw-pointer ops. The probe written to confirm that instead showed
something worse, and it had nothing to do with `trusted`: **a function declaring no
capabilities, not trusted, could call a `with(Console)` function in a sibling submodule
and print.** The program compiled clean and ran. The same miss dropped `Unsafe` from an
`extern` reached across a submodule — the one capability the language promises `extern`
costs — while a call to that same extern from inside its own module was refused correctly
the whole time.

CoreCheck built its signature table from one module's own functions, so a sibling call
found no entry and the lookup's `none` was read as "requires nothing" rather than "I do
not know what this requires" — the `ABSENCE_IS_NOT_A_FACT.md` pattern, in the capability
checker itself. The `collectAllStructs`/`collectAllEnums` pair on the lines directly above
already threaded types across the whole module tree; signatures were never given the same
treatment. Fixed by `collectAllFnSigs`, recording submodule externs under BOTH spellings
(`prefixModuleFnNames` renames functions but deliberately leaves externs alone, since they
name real C symbols, and the call site still emits the prefixed form — the first fix
closed the function path and left the extern path open, which only a separate program
showed).

**`capsContain` also under-approximated a union caller**, a separate pre-existing defect the
repair exposed. It asked whether EITHER side covered the WHOLE requirement, so
`Alloc ∪ Unsafe` did not cover `Alloc, Unsafe`. It therefore disagreed with `missingCaps`,
which has always normalized, and `decideCall` could report `satisfied := false` beside
`missing := []` — rendering the self-refuting *"requires Alloc, Unsafe but caller has
Alloc + Unsafe"*. One cap set, two readings, in the module whose stated purpose is that
this cannot happen.

**What was deliberately NOT changed, and this is the part worth remembering.** Closing the
hole made it tempting to let `trusted` confer `Unsafe` on extern calls, the way it already
does for raw-pointer ops. Two fixtures older than this repair say otherwise —
`error_trusted_extern_needs_unsafe.con` and `error_trusted_no_extern.con`, the second named
for the rule. Uniformity was the wrong instinct: the two questions look alike and are not,
and a checker repair is not the place to retire a tested language decision. The correct
mechanism already existed and std simply was not using it — `trusted extern fn`, which
`externFnRequiredCaps` types as requiring nothing, and which `examples/elf_header` has used
from the start. 54 libc/alloc externs are now explicitly `trusted extern`, which is also
the honest form: the vouch was always being made, it was being made silently. Without it
`String::eq` calls `memcmp`, and every string comparison in every user program would have
had to declare `with(Unsafe)`.

**The cost, measured across four waves.** 321 std signatures now declare what their bodies
require, 84 of them public. Each wave was found by iterating the checker to a fixpoint, so
every function states the union of what its own call sites require and nothing more. Not
one was a design problem: `println` really does deallocate a `String`, the `test.con`
assertion helpers really do build them, 131 of std's own test functions really do call
those helpers. All 100 non-std packages check clean without modification.

Each wave is a lesson about scope of measurement, not about the fix:

1. A corpus sweep over package entry points said "zero cascade" and was *right about what
   it measured*. std's own `#[test]` functions are only compiled in single-file report
   mode, which that sweep never entered.
2. `E0240` (the cap-polymorphic path in `Check.lean`) words its diagnostic differently from
   `E0520` (`CoreCheck.lean`). A sweep grepping only the latter reported clean three times
   with a `vec.con` site still open.
3. Filenames and function names containing digits (`base64.con`, `sha256.con`,
   `hex_encode_u32`) fell outside an `[a-z_]` character class and were silently skipped —
   the fixpoint reported "0 updated" while 40 errors stood.
4. Package-entry-point checking and single-file `--report` mode do not compile the same
   set. Neither reached `check_std_compiled_coverage.sh`'s per-module probes, which is
   where the parser bug above finally showed.

**A PARSER bug surfaced only because the requirement finally bound**, and it is the
sharpest instance of the absence rule yet: nothing was computed wrong. The body-less `fn`
branch carried `isPublic` onto the declaration and dropped `isTrusted`, so
`pub trusted fn sizeof<T>() -> u64;` reached `externFnRequiredCaps` as UNtrusted and was
charged `Unsafe`. The modifier was parsed correctly and simply not recorded; the consumer
then read the structure default `isTrusted := false` as an authorial decision. A default is
only safe where the absence of a value and the value itself are the same fact, and "not
trusted" is a claim. `check_std_compiled_coverage.sh` compiles one probe per std module
under the full `Std` set — which by construction excludes `Unsafe` — and the `mem` probe
stopped compiling. `sizeof`/`alignof` are now `pub trusted fn`: the compiler answers them
at compile time from a type, so charging `Unsafe` for a size query was an artifact of the
spelling, not a fact about the operation. Paired fixtures `bodyless_trusted` (costs
nothing) and `bodyless_untrusted` (still costs `Unsafe`, so the modifier is effective
rather than meaningless) are CHECK-ONLY — `#[intrinsic]` resolves by the compiler's own
name table, so a renamed copy has no symbol to link.

**The attestation identities moved and were MIGRATED, not re-attested.** std signature
changes change package-scope identity: 7 of 21 scopes produced new digests, 42 generated
references were renamed, and 38 references across `Proof.lean` and
`proofs/Examples/HmacSha256/Proofs.lean` were rewritten. Old and new scopes were matched by
the SET of `(module, function, body digest)` each contains — body digests are unchanged
because no function body changed — and each of the 7 matched exactly one candidate, so
nothing was paired by name similarity or by guesswork. Eight entries share a
`(module, fn, body)` key across packages (`crypto_verify` and `crypto_verify_drifted` hold
the same `check_nonce`), which is why the mapping is built per SCOPE from content sets
rather than per NAME. `crypto_verify` returns to 4 proved / 36 facts and `elf_header` to
5 proved, both 0 stale and 0 closure-unjustified — the R-0483 baseline, and
`check_dependency_edges.sh` returns to 317/0 after briefly reading 32 proved / 3
unjustified while the references were stale. No proof was re-established and no claim
widened; R-0208 is untouched.

The migration had to be run TWICE, which is the durable lesson: any later std source edit
moves package-scope identity again, so the regeneration is not a step you do once at the
start. The mutation-anchor table is the easiest part to forget, because it embeds
attestation identities verbatim and a stale anchor fails open — six families were counted
as covered while testing nothing until `check_mutation_anchors.sh` named them.

**THE THIRD HOLE WAS BIGGER THAN "METHODS", AND IS NOW PARTIALLY CLOSED (bug 071).**
Sizing it showed the boundary is the PACKAGE, not method dispatch: a function declaring
NOTHING called `std.env.get` — `with(Env, Alloc, Unsafe)` — and the program printed
`$HOME`. Filesystem, clock, network and process exit the same. `std.io.println` was the
only refusal, and not for a good reason: it is an INTRINSIC with a hardcoded capability,
so **the enforced set across a package boundary was exactly the hardcoded intrinsic
table, and every capability a dependency actually declared was decorative to consumers.**

Any claim in this document that a capability requirement stays visible at every call
site was, until 2026-09-21, true only WITHIN a package and for those intrinsics.

**Stage 1 landed:** `CModule.importedFnCaps` carries each imported callable's requirement
keyed by the local call spelling (`get` is declared in ten std modules, so a bare-name
table would cross-attach); Elab joins the import list to the dependency `FileSummary`;
CoreCheck consults it on the same `decideCall` path as local and sibling calls. Sinks and
`Alloc` now bind, for free functions and methods alike. Corpus cost: 12 declarations,
every one honest — including a function that read a file without declaring `File`.
`check_cross_package_caps.sh` 16/0, with two compiler mutations (sever the transport;
empty the payload while leaving it wired).

**Stage 1 is deliberately incomplete and bug 071 stays OPEN.** `Unsafe` does not yet bind
across a package: enforcing it gave 91 diagnostics over 32 of 95 packages, **82 missing
only `Unsafe`** with the caller already holding the full `Std` set — which is defined as
every capability except `Unsafe`, so `Std` could not open a file or push to a `Vec`.
That exception is not acceptable as permanent semantics (identical declarations would
mean different things depending on the caller's package); it is pinned in the gate so
removing it fails loudly. The end state: every declared capability binds uniformly, a
safe public abstraction terminates `Unsafe` at a small explicit trusted leaf rather than
re-exporting it, genuinely unsafe APIs keep it, and evidence still records the dependency
on the trusted boundary. Remaining: classify the 82, apply the three treatments, remove
the exception, re-run the corpus.
`tests/regressions/cap_sibling_module/known_hole_cross_package_method/` checks clean today
and `check_cap_sibling_module.sh` asserts that it does, so the day it closes the gate fails
and says so. It is also why the 84 public std declarations above cost consumers nothing
yet, and closing it is expected to be the larger change — it reaches the resolve-side
method signature path rather than one signature table.

`check_cap_sibling_module.sh` (14/0) pins both closed negatives, a positive control that
must BUILD AND RUN, the trusted/`Unsafe` decision and its fixtures, the body-less
declaration pair, the union normalization, and the open hole.

This is a correction to the *checker*, not to the evidence semantics: it makes `with(...)`
mean what it always claimed for required authority. The three-way separation
(requires / carries / performs) below is untouched, and no proof or receipt rides on it.

`CFnDef.isProofEligible` required `f.capSet.isEmpty` and read it as "pure". It means
"nothing was declared". The rule that fixes it — refuse any function that can REACH an
indirect call, transitively, via a least fixpoint over the proof call graph — is
implemented, correct, and switched off at a single line
(`EligibilityEntry.admissible`), for a reason worth stating rather than working around.

**What stopped it.** Enabling it refuses 188 of std's 890 functions, because every
higher-order function is opaque while `trusted` can hide effects behind a
capability-free fn type. That part is right and costs no proof: std has two proved links,
both base64, both unaffected, and the affected claims were already non-proved. But three
`pureCoreFns` links move from `needs_recheck` to `ineligible`, which drops them out of
drift coverage (11 to 8) and out of `replayTargetsOf` entirely — and
`check_purecore_proofs.sh` argues replay must keep asking about pending claims "or the
migration cannot clear them". Whether an INADMISSIBLE claim should still be replayed is a
question about evidence semantics, not about effect-freedom, and enabling this rule
answers it by accident. That is the decision to take deliberately.

**The separation it established still stands and is the durable result.** `eligible`
decides extraction; `admissible` decides admission. A first attempt folded opacity into
`eligible` and removed higher-order functions from the evidence surface, which
`check_shadow_body_v2.sh` caught — "a function used as a VALUE is an edge" exists so a
higher-order program does not look dependency-free. Trading a false purity claim for a
missing dependency edge relocates an R-0004 gap rather than closing one.

When admission is enabled, the diagnostic already names its reason, and does so only when
opacity CHANGES the verdict — listing it beside an unrelated refusal (`main` is excluded
for being the entry point) would present a fact that contributed nothing as a cause.

`check_effect_opacity.sh` (9/0) asserts the live half and pins the inert half, so
enabling admission fails the gate and forces the replay question to be answered in the
same commit.

**Reports reconciled 2026-09-18.** `--report caps` and `--report effects` printed
`(pure)` for any empty capability set, so admission refused what the reports still called
pure. Both now render `(effects unknown: reaches an indirect call)`, and the purity totals
count a claim rather than empty capability sets. The estimated blast radius of ~42
assertion sites was wrong by a wide margin: only functions that actually reach an indirect
call change, and almost every `(pure)` assertion in the corpus is about a genuinely pure
function. `examples/packet`'s parsing core is unchanged, which is the control that the
rule refuses narrowly rather than broadly.

**Still open, and measured more precisely 2026-09-18.** The analysis sees exactly ONE
module for a std-using program: `--report caps` on `examples/base64_cli` lists `module
base64_cli` and nothing else. `std`'s `Writer::write` and `write_raw` are therefore not
missing from the call GRAPH — they are absent from the compilation unit entirely, so a
call into `std` resolves to a name with no node and nothing propagates.
`base64_cli.print_bytes`, the instance this task came from, is still admitted and still
reported pure; `check_effect_opacity.sh` asserts that, so closing the gap fails loudly.

That makes the fix larger than "put dependency modules in the call graph". Opacity is a
fact ABOUT A DEPENDENCY, established where that dependency was compiled, and it has to
reach the consumer the way other dependency facts do. Two shapes, and choosing between
them is a product decision rather than an implementation detail:

1. **Load dependency cores into the analysis.** Direct, but it makes the consumer
   recompute a fact the dependency already established — the second-producer problem this
   repository keeps paying for elsewhere.
2. **Ship opacity as a per-definition dependency fact,** alongside the attestation
   material that already crosses this boundary. One producer, and it composes with the
   package-evidence work; but it widens the dependency fact surface, which is R-0440 and
   R-0482 territory and inherits their prerequisites.

Neither is attempted here.

**THE MECHANISM, located 2026-09-16 — and it is not what the shape suggests.** A
`Writer` dispatches through stored function pointers, so the obvious hypothesis is that
indirect calls lose effects. That hypothesis is WRONG, and a probe refutes it: function
pointer types carry capabilities, and storing a `with(Console)` function in a
capability-free field is rejected outright.

```
E0220: type mismatch in field 'f' of struct 'Sink': expected fn() -> i64,
       found fn() with(Console) -> i64
```

The erasure happens one step earlier, at the `trusted` boundary. Three functions in
`std/src/io.con` issue the same syscall and disagree about saying so:

| function | performs | declares |
|---|---|---|
| `println` (L323) | `libc_write(1, ...)` | `with(Console)` |
| `console_write` (L154) | `libc_write(1, ...)` | nothing |
| `console_err_write` (L173) | `libc_write(2, ...)` | nothing |

`trusted` permits a function to perform an effect without declaring it, and nothing
checks a trusted function's declared capabilities against what its body does. The
undeclared sinks are therefore *typed* capability-free, which is exactly what the
`write_fn` field requires; every call through the handle is capability-free; and a
helper that calls `w.write(b)` reports `(pure)`. The constructor's `with(Console)` is
real and does check authority-to-acquire — the effect of USING the handle is what
disappears.

So the three facts this task must separate are not merely unreported, they diverge at a
known point: acquisition authority is checked at the constructor, argument-supplied
authority is invisible in the header, and the operational effect is erased where a
trusted body declines to name it. The smallest candidate change is therefore a
consistency obligation on `trusted` — a trusted function's declared capabilities must
cover the effects its body performs — rather than new machinery. `println` already
satisfies it; `console_write` and `console_err_write` do not. That candidate needs
costing against every trusted wrapper before adoption, and it is stated here as the
measured lead, not a decision.

**Evidence boundary:** this establishes incorrect reporting and eligibility
classification, not that an effect-ignoring theorem can reach an authoritative verdict.
The measurement did not exercise extraction, proof registration or receipt issuance
with an attached claim. Later checks may refuse it. No false proof or receipt is
demonstrated by the absence of a registered proof.

**Next transition:** trace this helper through extraction, proof attachment, dependency
closure, receipt issuance/consumption and policy. Record the exact accepting/refusing
rule at each boundary and retain the `usage`/`print_bytes` contrast as a paired regression
across all four reports. In isolated negative fixtures, test whether the effect can be
omitted from a claim that reaches a production verdict. Preserve a genuinely effect-free
positive control, and distinguish a report-only correction from any required containment
in proof admission. R-0208 still governs authoritative evidence transitions.

The repaired contract must distinguish no declared capability requirements from purity.
Purity accounts for effects through resource handles, callback/context forwarding and
trusted wrappers; unknown effects cannot establish affirmative purity or satisfy an
effect-free proof-admission requirement. Use the measured consumer paths to choose the
smallest implementation change, rather than inventing a parallel effect/evidence system.

Preserve resource handles as scoped authority. Specify and represent separately the
authority required to acquire a resource, authority supplied through arguments, and
effects invocation may perform. Decide how the existing capability judgment and
interface summaries carry those facts, including callback/context forwarding,
trusted wrappers, destruction and unknown indirect targets. Reconcile the Phase 7
capability-completeness and authority-at-acquisition criteria under that decision.

**Exit:** a capability-free helper accepting a `Writer` cannot be classified as
effect-free merely from its header; file/console sinks, callbacks and cleanup have
consistent checker/report/policy answers. Positive and negative controls expose
authority laundering and conservative unknown effects. Proof/totality eligibility
uses the relevant semantic facts, not an empty-capset shortcut. Update the capability,
safety, identity and public-claims docs together without introducing a rival evidence
schema or claiming unrestricted least-authority enforcement.

## R-0072

_Stub: [ROADMAP_BACKLOG.md](../project/ROADMAP_BACKLOG.md#task-r-0072) · disposition: retired · owner(s): R-0517_

CI must cover a clean checkout and reject an unsupported QBE version with a
clear diagnostic; updating the pin requires the quick matrix plus scheduled
corpus and an attached toolchain-diff record. Check in a host/target
capability matrix discovered from the pinned binary and verified by compile/
assemble/link probes, including `x86_64-linux -> amd64_sysv` and each macOS
runner's real Apple target. Cross targets may be emit/assemble-only when no
declared runner exists; executable rows require a named host or emulator.
Do not infer support from an OS or CPU-family label alone.

## R-0074

_Stub: [ROADMAP.md](../project/ROADMAP_TASKS.md#task-r-0074) · disposition: superseded · owner(s): R-0503, R-0517_

 `{backend, target, codegen_input_hash, primary_text, auxiliary_files,
 runtime_helpers, link_inputs, diagnostics, toolchain_facts}`. Use a closed
 `BackendKind` sum (`llvm | qbe`) and ordinary Lean functions, not a dynamic
 plugin system or effectful callback framework. Keep pure emission separate
 from external validation, assembly, and linking.

## R-0076

_Stub: [ROADMAP.md](../project/ROADMAP_TASKS.md#task-r-0076) · disposition: superseded · owner(s): R-0502_

 Sequencing dependency: perform this ownership split before the queued
 Layout panic-to-diagnostic conversion, then convert the panic paths in the
 target-neutral `LayoutFacts` owner and LLVM-specific renderer where they
 actually remain. Gate the split with byte-identical LLVM IR first so the
 hygiene slice does not move and rewrite the same functions twice.

## R-0079

_Stub: [ROADMAP.md](../project/ROADMAP_TASKS.md#task-r-0079) · disposition: superseded · owner(s): R-0493, R-0501, R-0503_

 for every `SInst`/terminator, state whether Concrete SSA already expresses
 the exact semantics, a shared target-neutral expansion is required, or the
 backend must select an independent sequence. Shared legalization is allowed
 only for language semantics that must be identical (checked arithmetic,
 normalized shift counts, trap classification, aggregate copy intent);
 instruction selection and target realization remain independent so the
 oracle does not correlate backend bugs. Every legalization carries source
 span and semantic-family identity into emitted artifacts.

## R-0082

_Stub: [ROADMAP.md](../project/ROADMAP_TASKS.md#task-r-0082) · disposition: superseded · owner(s): R-0497, R-0501, R-0503_

`source -> Core -> Mono -> ValidatedSSA -> SSACleanup`, followed by sibling
LLVM and QBE emitters. QBE must consume `SModule` / `SInst` (or a minimal
target-neutral view over them) and must never translate emitted LLVM IR or
reuse LLVM instruction-selection text. Share semantic facts such as integer
width/signedness, size/alignment, symbol identity, builtin identity, and
entry-point policy; keep instruction selection, aggregate materialization,
ABI spelling, wrappers, globals, and runtime-helper emission independent.

## R-0083

_Stub: [ROADMAP_BACKLOG.md](../project/ROADMAP_BACKLOG.md#task-r-0083) · disposition: retired · owner(s): R-0511, R-0517_

`--backend llvm|qbe`, `--emit-qbe`, and the corresponding project-build
setting. LLVM remains the default throughout Phase 7.5. Reject an unknown or
unavailable backend with a structured diagnostic that names the missing
tool/target; never fall back to LLVM after the user selects QBE.

## R-0086

_Stub: [ROADMAP.md](../project/ROADMAP_TASKS.md#task-r-0086) · disposition: superseded · owner(s): R-0511, R-0517_

Gate negative, wide, shift, division/remainder, overflow, NaN, signed-zero,
and exit-code edge cases rather than accepting scalar happy paths alone.

## R-0087

_Stub: [ROADMAP_BACKLOG.md](../project/ROADMAP_BACKLOG.md#task-r-0087) · disposition: superseded · owner(s): R-0512_

 Concrete's finite/range/trap policy must be emitted as explicit guards and
 runtime trap edges before QBE conversion instructions—raw QBE `stosi`,
 `stoui`, `dtosi`, or `dtoui` behavior is not Concrete's checked-cast
 semantics. Until this slice lands, every float family is an explicit
 `qbe_pending` capability row rather than silently absent from the subset.

## R-0089

_Stub: [ROADMAP_BACKLOG.md](../project/ROADMAP_BACKLOG.md#task-r-0089) · disposition: superseded · owner(s): R-0505, R-0517_

The ABI matrix must cover 1–8-byte and 9–16-byte aggregates, mixed
integer/float classes, register exhaustion with stack fallback, aggregates
larger than two eightbytes (`MEMORY` class), unaligned/opaque aggregates,
and caller-provided storage for memory-class returns. Do not encode a false
“greater than 16 bytes is unsupported” rule: QBE's supported ABI paths must
be tested; only a shape that the pinned target/toolchain actually cannot
realize receives a named `qbe_pending` row or runtime-mediated boundary.

## R-0092

_Stub: [ROADMAP.md](../project/ROADMAP_TASKS.md#task-r-0092) · disposition: superseded · owner(s): R-0491_

 `{compile_class, stdout_bytes, stderr_bytes, exit_status, runtime_class}`.
 Normalize only declared platform noise; never normalize program output,
 trap kind, or exit status merely to make paths agree.

## R-0093

_Stub: [ROADMAP.md](../project/ROADMAP_TASKS.md#task-r-0093) · disposition: merged · owner(s): R-0092_

 - interpreter == QBE != LLVM: `llvm_backend_mismatch`;
 - interpreter == LLVM != QBE: `qbe_backend_mismatch`;
 - LLVM == QBE != interpreter: `interpreter_or_shared_lowering_mismatch`;
 - all three differ: `semantic_split`;
 - both compiled paths agree with an external/hand oracle but the
   interpreter differs: `interpreter_mismatch`;
 - both compiled paths agree with each other and disagree with the external/
   hand oracle: `shared_lowering_or_spec_mismatch`.
 Save sources, Core, cleaned SSA, LLVM IR, QBE IL, tool versions, commands,
 observations, and classification in the wrong-code/debug bundle.

## R-0097

_Stub: [ROADMAP.md](../project/ROADMAP_TASKS.md#task-r-0097) · disposition: superseded · owner(s): R-0511, R-0512_

 `concrete agent features --json`, help, audit output, and documentation list
 supported targets/features and `qbe_pending` cases. A program outside the
 subset receives a diagnostic; per-function or per-instruction fallback to
 LLVM is forbidden.

## R-0098

_Stub: [ROADMAP.md](../project/ROADMAP_TASKS.md#task-r-0098) · disposition: merged · owner(s): R-0137_

 exclusions require a named missing feature and roadmap owner. Record bugs
 found by the backend/differential suite in the bug corpus with the disagreement classification
 that exposed them.

## R-0099

_Stub: [ROADMAP_BACKLOG.md](../project/ROADMAP_BACKLOG.md#task-r-0099) · disposition: retired · owner(s): R-0517, R-0505_

 Until then `--backend qbe` is explicitly experimental and its native result
 remains backend-trusted/test evidence.

## R-0100

_Stub: [ROADMAP_BACKLOG.md](../project/ROADMAP_BACKLOG.md#task-r-0100) · disposition: superseded · owner(s): R-0305_

 This is not an implementation commitment until those criteria pass.
 Cranelift remains the alternative if a forcing workload instead requires
 fast native/JIT compilation or a broader production backend. C emission,
 libgccjit, MLIR, and direct machine code remain comparison options, not
 parallel commitments. No additional execution path starts without a bug
 class or workload that LLVM plus QBE cannot pressure effectively.

## R-0101

_Stub: [ROADMAP_BACKLOG.md](../project/ROADMAP_BACKLOG.md#task-r-0101) · disposition: delegated · owner(s): R-0517_

 a compiler-owned invalid artifact.

## R-0105

_Stub: [ROADMAP.md](../project/ROADMAP_TASKS.md#task-r-0105) · disposition: superseded · owner(s): R-0491, R-0504_

 Classify within-backend disagreement separately as
 `llvm_optimization_mismatch` or `qbe_toolchain_mismatch`; retain pre/post
 optimization artifacts where available. Include sanitizer/instrumented LLVM
 runs as corroborating evidence, while keeping sanitizer output out of the
 program-output comparison channel.

## R-0107

_Stub: [ROADMAP.md](../project/ROADMAP_TASKS.md#task-r-0107) · disposition: merged · owner(s): R-0075_

 `ssa_to_qbe`, `qbe_verify`, `qbe_codegen`, `assemble`, `link`, `runtime`, or
 `observation_compare`; capture exit code/signal/timeout/resource-limit and a
 bounded stderr excerpt for each external process. Add time, output-size,
 memory, and recursion limits so malformed generated IL or fuzz cases cannot
 hang the suite, and distinguish timeout/OOM/tool crash from semantic
 disagreement.

## R-0108

_Stub: [ROADMAP.md](../project/ROADMAP_TASKS.md#task-r-0108) · disposition: merged · owner(s): R-0416, R-0515_

 Phase 7.5 fails on unbounded or accidental regressions, not on a fixed
 QBE-versus-LLVM speed ratio. Performance results are benchmark facts only;
 they cannot choose a backend silently or weaken semantic/evidence gates.

## R-0110

_Stub: [ROADMAP.md](../project/ROADMAP_TASKS.md#task-r-0110) · disposition: merged · owner(s): R-0075, R-0333_

 reserve backend names and project-manifest schema, define `.qbe` artifact
 naming without colliding with Concrete's own SSA dumps, keep LLVM as the
 stable default, and snapshot help/JSON/diagnostic output. Removing or
 changing the experimental backend requires a roadmap/changelog decision,
 but no package may claim generic native support from a QBE-only test. Audit
 bundles must state the exact backend used; examples and documentation must
 not show QBE-derived native evidence as LLVM-derived or backend-independent.

## R-0111

_Stub: [ROADMAP.md](../project/ROADMAP_TASKS.md#task-r-0111) · disposition: superseded · owner(s): R-0511, R-0137_

 `examples/qbe_backend_validation/` plus
 `scripts/tests/check_phase7_5_qbe.sh`. It must exercise every supported
 semantic-family manifest row; demonstrate each disagreement classifier
 with controlled mutations; validate deterministic emission, clean-checkout
 tool discovery, reducer replay, debug-bundle completeness, unsupported-
 feature diagnostics, no-fallback behavior, target/ABI facts, and quick-CI
 wiring; and publish a machine-readable coverage/pending inventory. Closure
 requires zero unexplained mismatches, successful normal `build`/`run`/`test`
 use on the declared subset, and either a recorded real defect found by the
 differential suite or successful injected-fault detection for every owned
 fault class. All pending rows need an owner and Phase 8/15 disposition.

## R-0112

_Stub: [ROADMAP.md](../project/ROADMAP_TASKS.md#task-r-0112) · disposition: superseded · owner(s): R-0512_

 the compatibility window must have a named removal commit and mutation-
 sensitive parity gate.

## R-0114

_Stub: [ROADMAP.md](../project/ROADMAP_TASKS.md#task-r-0114) · disposition: superseded · owner(s): R-0492_


Acceptance: IDs originate once at the owning boundary, survive rename/re-export
and semantics-preserving moves as specified, change when their semantic owner
changes, appear in diagnostics/debug/proof artifacts, and are mutation-tested
against first-match/name-collision regressions. Migrate one consumer at a time
and delete each superseded text-keyed lookup in the same slice.

## R-0116

_Stub: [ROADMAP.md](../project/ROADMAP_TASKS.md#task-r-0116) · disposition: superseded · owner(s): R-0497, R-0510, R-0514_


Acceptance: migrate one production edge per green slice, preserve diagnostics
and artifacts, add compile-time negative fixtures for illegal handoffs, expose
no unwrap/backdoor constructor, and delete the parallel raw-list entry point as
each boundary lands. Do not introduce a universal pass monad.

## R-0171

_Stub: [ROADMAP.md](../project/ROADMAP_TASKS.md#task-r-0171) · disposition: superseded · owner(s): R-0494_

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

## R-0176

_Stub: [ROADMAP_BACKLOG.md](../project/ROADMAP_BACKLOG.md#task-r-0176) · disposition: superseded · owner(s): R-0495, R-0512_

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

## R-0208

_Stub: [ROADMAP.md](../project/ROADMAP_TASKS.md#task-r-0208) · disposition: done · owner(s): —_

**Lean #14576 fire drill:** the repository's currently pinned `leanprover/lean4:v4.28.0` predates
the upstream fix and must be treated as not accepted for new high-assurance claims until an exact
fixed toolchain is selected and the retained upstream reproducer is tested. Upgrade, rebuild and
replay the affected corpus; prove that the toolchain identity changes; prove that a synthetic or
historical receipt bound to the old identity becomes `needs_recheck`/rejected; and prove that only a
fresh replay under an accepted checker can restore the corresponding evidence dimension. Since no
R-0004 receipt is authoritative yet, this is also a rehearsal of migration and revocation—not a
claim that old production receipts already exist. Never bless proof goldens merely to clear the
upgrade.

Fire-drill acceptance is operational, not “the new toolchain builds”:

- retain the exact upstream exploit and an isolated historical affected toolchain; never run the
  hostile project through an ambient developer environment;
- inventory every Lean-backed claim before upgrading, then replay the same denominator afterward;
- classify each proof/build/golden change and forbid unexplained snapshot regeneration;
- show old checker-bound logical validity downgrades while independently established subject,
  correspondence, and dependency facts remain separately visible;
- show an old synthetic receipt cannot regain acceptance by editing a version string, advisory
  entry, feature footprint, or report output;
- require fresh replay under a confirmed accepted checker/rule set to restore acceptance; and
- publish the exact affected claims and remaining trust rather than saying the whole compiler was
  either safe or unsafe.

**Scope before implementation (tightened 2026-10-10; nothing implemented).** The drill
splits into two slices: (1) revocation and receipt acceptance, (2) the actual upgrade with
an explained replay of the same claim denominator. Slice 1 must first define the trust
boundary for receipt acceptance, because today there is none to revoke against:

- **What exists.** `resolveChecker` (`Concrete/Proof/Replay.lean`) names the checker by the
  text of the workspace's `lean-toolchain` file, not by the process that replays (`lake env
  lean`). `toolchainIdOf` (`Concrete/Proof/Receipt.lean`) is an unkeyed `shortHash` of public
  strings, a stored receipt is unauthenticated, and `storedDispositionFor`
  (`Concrete/Proof/Issue.lean`) yields only `current`/`notCurrent` by digest equality. Anyone
  who can edit a receipt's identity can recompute that hash, so reading the real checker's
  identity fixes the file/process mismatch but authenticates nothing.
- **Checker identity.** Identify the process that actually performs replay, including its
  executable and build identity (resolved binary, version, githash), and bind that.
- **Why a consumer trusts a receipt.** Choose and state one basis, per consumer: protected
  local provenance, an authenticated issuer, or fresh replay by the consumer. A receipt whose
  basis does not hold is not accepted, whatever its contents say.
- **Who controls advisory policy.** An editable register cannot protect against its own
  replacement. Name the independently trusted policy source and specify how missing,
  altered or rolled-back policy fails closed.
- **Tests.** Synthetic identities exercise policy transitions (affected → `needs_recheck`,
  forged identity, edited or rolled-back policy). "Fresh replay under an accepted kernel
  restores acceptance" is reserved for a real upgraded-kernel integration test, not
  simulated.

The upstream issue shows the affected kernel accepting an axiom-free proof of `False`
through checked declarations. That removes the assurance the affected checker provided; it
does not show that any of this repository's theorems is false, and reports must say the
former, not the latter. Queue order: after mutation qualification.

**Design decisions (2026-10-10; implementation after mutation qualification).** A read of the
replay path found that replay emits `#check` against `.olean`s an earlier `lake build` produced, so
the kernel accepted each theorem at build time, not in the replay process; that the nix devshell
takes Lean from nixpkgs and ignores `lean-toolchain` while CI's elan honours it; and that the
`needsRecheck` disposition is never produced. Decided:

1. **Fresh replay includes an actual kernel re-check.** Looking a declaration up in an existing
   `.olean` is insufficient; the accepted checker validates the retained proof and its required
   dependency closure.
2. **Trust basis: fresh consumer replay.** Per-user signing keys are deferred; a signature
   authenticates an issuer's statement, not that the proof was checked correctly.
3. **Advisory policy is a compiled module**, one policy source versioned with the consumer. Its
   identity is recorded in each policy decision, separately from immutable receipts. Sequence
   numbers alone do not prevent rollback: either an independently enforced minimum policy version
   exists, or the rollback guarantee is stated as limited.
4. **A register change triggers policy re-evaluation, not invalidation.** Historical replay facts
   are preserved; recheck is required where an advisory affects the recorded checker, and
   unrelated receipts can remain accepted.
5. **Checker identity binds the actual executable and build.** A nonce-tagged identity line
   printed by the replay process correlates output with that process; it is not authentication.

**Slice 1a landed on branch `r0208-checker-identity` (2026-10-11; decision 5).** The replay process
reports its own `Lean.versionString`, `Lean.githash`, platform and executable behind a per-run
nonce, plus the native images it has mapped (`/proc/self/maps` on Linux, `lsof` on itself on
macOS). The build digest hashes those LOADED images (`shasum -a 256`), keyed by file name so an
identical build at another path is the same checker; OS-owned images are named, not hashed.
Measured on macOS: the old sibling-library rule hashed `libLake_shared`, which the checker never
loads, and missed GMP and libuv, which it does. `toolchainIdOf` moved to V2 and binds
`CheckerIdentity.canonical`; `lean-toolchain` is now shown only as the declared pin. Missing,
duplicated or incomplete self-reports (no line, two lines, no image list, executable or
`libleanshared` absent, duplicate image names) give no identity, and no identity mints nothing
(`checker_unidentified`) and compares current against nothing. `check_checker_identity.sh` (14
legs) holds: a lean-toolchain-only change keeps the identity; one byte appended to the loaded
`libleanshared` changes it and makes every receipt non-current; edited self-reports are a different
checker; a foreign-nonce line is ignored. Cost: a status check went from 0.65 s to 1.7 s (one
probe process per operation, no cache). **What 'current' still does not mean:** reports say so
explicitly — not a fresh kernel re-check (decision 1, open) and not advisory acceptance (decisions
3-4, open). Stated limits: OS libraries named not hashed, core `.olean` data not bound, the
identity is correlated by nonce and not authenticated, and files are hashed after the run
(time-of-check gap). Seven mutation families cover the new refusals and bindings (inventory 95 →
102), each verified red by hand before registration; `check_checker_identity.sh` also refuses two
loaded images sharing a file name, with the second image a real file so only that rule can refuse.
**Slice 1a published and qualified:** `c8731588` on origin (CI 38163033211), 102-family campaign
38163069750 `qualified=1` (102/102, 8.7 runner-hours, slowest shard 55.5 min).

**Slice 1b on branch `r0208-kernel-recheck` (decision 1).** Measured first: replay emitted
`#check @thm` against `.olean`s an earlier build produced, and an ILL-TYPED theorem added with the
kernel check skipped (`bogus : 1 = 2 := Eq.refl 1`) was "found" by that lookup; `leanchecker`
refused it. The replay process now re-checks each theorem itself: it collects the workspace
constants the theorem reaches (types, values, recursor rules, each inductive's constructors and
recursors) and replays them with `Lean.Environment.replay'` — the routine `leanchecker` uses — into
an environment of only the toolchain modules they touch, in the same process whose identity the
receipt binds. Per constant, not per module: a per-module first version refused genuine
`proof_patterns` theorems because an imported module also holds an unrelated `bv_decide` lemma.
One nonce-tagged verdict per theorem (`rechecked`, `native`, `failed`, `not run`); minting requires
`rechecked`. Theorems resting on `Lean.ofReduceBool`/`trustCompiler` are reported native-trust and
never minted — none mints today (all HMAC claims are already withheld), and whether such a claim
may carry a native-trust-qualified receipt is an open policy question. `check_kernel_recheck.sh`
(9 legs) holds: the ill-typed theorem is found by the lookup but refused by the re-check; genuine
theorems in another module and in the same module re-check and mint; a `bv_decide` theorem is
native-trust; missing, duplicated or foreign-nonce verdicts mint nothing. Cost ~0.3 s per theorem.
Limits: toolchain modules are trusted via the checker identity, not replayed (`--fresh`-style replay
of core measured at 88 s per closure); verdict lines are nonce-correlated, not authenticated.
**Advisory policy on branch `r0208-policy` (decisions 3-4).** receiptV2 carries the compiler and
checker identity in the clear, and decoding requires both to re-hash to the bound `toolchain` id,
so editing the readable identity to dodge an advisory is refused (`identity_not_bound`); receiptV1
stores read as written under another envelope. `Concrete/Proof/CheckerPolicy.lean` is the
register, compiled into the consumer: explicitly accepted `(checker, version)` pairs and advisories
keyed by exact versions with status `accepted | affected | revoked | unknown`, a `policyVersion`
and a `policyIdentity` digest over its canonical rendering. Decisions are made at read time from
(receipt checker, register) and reported BESIDE currency, naming the deciding register: revoked >
affected > accepted, and a checker neither accepted nor named (or unparseable) is `unknown`, never
accepted. A register change therefore re-evaluates and never invalidates. First entry: `lean4#14576`
affects `4.28.0`; no checker is accepted yet, so every current receipt reads `checker policy:
affected (lean4#14576)` — the fire drill's "not accepted for new high-assurance claims", stated as
a loss of the affected checker's assurance, not as any theorem being false. Rollback protection is
LIMITED and stated: `check_checker_policy.sh` refuses a compiled `policyVersion` below the
separately owned `scripts/tests/fixtures/checker_policy_minimum`; reverting both together is not
refused. The gate (14 legs) exercises transitions on synthetic identities and registers only.
**Open, and owned by the user:** whether a native-trust (`bv_decide`) claim may carry a
native-trust-qualified receipt; which exact fixed toolchain to accept (slice 2 proper: upgrade,
rebuild, replay the same denominator, explain every delta, and show only fresh re-check under the
accepted checker restores acceptance).

**Lean v4.34.1 upgrade on branch `r0208-upgrade` (2026-10-12; user decisions recorded).** Decisions:
native-trust receipts are allowed but stay withheld until native evaluation is recorded end to end and
can never satisfy a kernel-only policy; target v4.34.1, accepting a checker only per MEASURED BUILD
after the upgrade, hostile controls and fresh replay pass; keep Nix, local and CI aligned. Done:
`lean-toolchain` and the flake (official release archive, GitHub sha256 per platform) — Nix and elan
now load byte-identical images on macOS (build `137d3402…`). Source adaptations change no statement:
deprecated `if_`/`dif_` lemma names (identical statements), `ensures` as a binder keyword, stricter
type checking after `simp … at`, `backward.do.legacy` on the two very large `do` definitions (the new
elaborator never finished `compileAndReport`: bisected, cumulative), and two HMAC lemmas whose
`bv_decide` goal 4.34.1 abstracted (`BitVec.ofNatClamp`) now close by `simp`. Found by the upgrade:
4.34.1's `bv_decide` emits per-proof `._native.bv_decide.ax_*` axioms instead of `ofReduceBool`, which
the re-check's deny-list missed (and a theorem on a DECLARED axiom re-checked even on 4.28); axioms are
now allow-listed to `propext`/`Classical.choice`/`Quot.sound`, and `check_axiom_inventory.sh`
classifies the generated native axioms like `ofReduceBool`. Evidence: the frozen 71-claim inventory
(`docs/verification/claim-inventory-v4.28.0.tsv`) replays identically on 4.34.1; the classification
table's 43 rows keep edge, tables, trust and specs, with only proof-term digests moving (rendered types
identical across versions). Policy v2 accepts one measured build (macOS aarch64); a rebuild, another
commit or another platform reads `unknown`, and Linux stays unaccepted until its CI-measured digest
passes the same controls (`scripts/tools/checker_acceptance_evidence.sh`, artifact
`checker-acceptance-<os>`). Consumers re-check every stored theorem before reporting acceptance, so a
fully consistent forged receipt is accepted only by that re-check, never by its bytes; reports state a
stored receipt is unauthenticated. Mutation inventory 115.
**Campaign on `9204684d` (38272837944): UNQUALIFIED.** `consumer-acceptance-requires-recheck` SURVIVED in
Linux shard 3 — no Linux build is registered, so acceptance was unobservable there. Repaired on
`r0208-closure`: the rule is a pure `Proof.acceptedNow` tested on synthetic inputs on every platform,
production use is covered by a separate family (`report-uses-acceptedNow`, inventory 116) killed on a
registered build, and the Linux build is registered in the same commit with self-contained evidence.
R-0208 closes only when the final commit passes CI and its complete campaign qualifies.

## R-0211

_Stub: [ROADMAP.md](../project/ROADMAP_TASKS.md#task-r-0211) · disposition: superseded · owner(s): R-0509_


V1 deliverable: `concrete audit --trust-boundaries --json` shows the trusted
surface, the facts each boundary supports, what calls it reaches, and which
claims depend on it. This is an honest report, not an auto-refactorer.

A later "trusted-boundary shrinker" may suggest wrappers or proof targets,
but suggestions must be advisory until a replayed proof/audit diff validates
the smaller boundary.

## R-0221

_Stub: [ROADMAP.md](../project/ROADMAP_TASKS.md#task-r-0221) · disposition: superseded · owner(s): R-0493_

`proof`.

## R-0228

_Stub: [ROADMAP.md](../project/ROADMAP_TASKS.md#task-r-0228) · disposition: superseded · owner(s): R-0499_

 accepted.

 Deliverable: `docs/UNSAFE_ISLAND.md` plus fixtures for `get_unchecked`-style
 access, raw-pointer place writes, unchecked casts, inline asm placeholder,
 and FFI wrapper assumptions. Add a red-team gate proving safe code cannot
 reach the unchecked operation without the capability or trusted boundary,
 and proving audit output shows both the safe wrapper and the underlying
 trusted/Unsafe operation.

## R-0234

_Stub: [ROADMAP_BACKLOG.md](../project/ROADMAP_BACKLOG.md#task-r-0234) · disposition: superseded · owner(s): R-0496_

 IEEE-754 binary32/binary64, round-to-nearest-even, no fast-math, no
 reassociation, no implicit FMA contraction, no ambient rounding-mode
 mutation, and explicit NaN/infinity/subnormal/signed-zero policy.

## R-0235

_Stub: [ROADMAP_BACKLOG.md](../project/ROADMAP_BACKLOG.md#task-r-0235) · disposition: superseded · owner(s): R-0496, R-0495_

 `PVal.float32/64`, float `PBinOp` cases carrying width and rounding
 (`fadd`/`fsub`/`fmul`/`fdiv`/`feq`/`flt`/`fle`), interpreter agreement,
 and backend/audit checks that prove/report `fast_math: forbidden`.

## R-0464

_Stub: [ROADMAP.md](../project/ROADMAP_TASKS.md#task-r-0464) · disposition: superseded · owner(s): R-0493, R-0495, R-0460_

**The order here is deliberate and was corrected 2026-07-31.** Moving obligation
generation to SSA fixes *staleness* (facts about a mutable `i`), which is real. It does
nothing about *rule duplication*, and on its own it merely relocates a second copy of the
trap rules onto a new representation. The single definition is the primary objective; the
representation change is the secondary one.

The evidence that this was the live problem, not a theoretical one — H24, reproduced in
`examples/trap_semantics_gap/` and CLOSED 2026-08-03 by the single-definition fix this task
describes (`IntArith.TrapCondition` plus the totality lock tying it to families). `IntArith` already IS the single source: it makes `trap` a
first-class result and defines checked division as trapping on divide-by-zero, signed
`MIN / -1`, and shift out of range. Interp, EmitSSA, SSAVerify, SSACleanup and
TypeJudgment consume it. Obligation generation imports it for range constants and then
states its own weaker conditions, so:

- `a / b` reports `div_nonzero → proved_by_kernel_decision` under `b ≠ 0` and the binary
  **aborts** on `(i32::MIN, -1)` — `divObligations` emits only `divisor ≠ 0` (insufficient);
- `a << b` generates **no obligation at all** and the binary **aborts** on `(1, 40)` —
  there is no shift family (inapplicable).

So the concrete work is: derive each family's trap condition FROM `IntArith` rather than
restating it, which closes both gaps at once and makes the next trap rule added to
`IntArith` propagate to obligations by construction instead of by someone remembering.
`VC_BRIDGE_REGISTER.md`'s div row must be corrected (its "Emits" is provably insufficient)
and a shift row added.

Note the gate-coverage lesson: `check_vc_bridge_register.sh` asserts every family
*generator* has a row, so it cannot detect a **missing family** — there is no generator to
notice. Registering families against the trap definition rather than against the existing
generators is what makes that detectable.

Second objective, and make every syntactic analysis feeding a proof total with an
unsafe-by-default fallback:

The hypothesis walker threads facts forward over MUTABLE surface syntax, so the name `i`
denotes different values at different program points. That forces `dropStaleHyps` to
*delete* every fact mentioning a just-assigned variable, driven by `assignedScalarsS` — a
syntactic mutation analysis whose last case is `| _ => []`, i.e. an unrecognised statement
is assumed to assign nothing. Fail-OPEN, exactly where being wrong is unsound, and any
statement form added to the AST later is silently misclassified.

In SSA each assignment binds a fresh name, so a fact about `i₁` cannot be invalidated by
`i₂ = i₁ + 1`. The entire staleness problem is an artifact of the input representation.
This task therefore **deletes** `dropStaleHyps`, `assignedScalarsS` and all reasoning about
what a store, a call, or a nested loop can invalidate, rather than hardening them — the
same "make the bad state unrepresentable" move Register C made for status composition.

Two further wins. Path conditions become branch predicates on CFG edges, which fixes a
known incompleteness: in `if a != 0 && b / a > 1` the divisor obligation inside the
condition currently gets no `a != 0` hypothesis, because only the whole condition is added
to the then-branch. And the shipped binary is compiled FROM SSA, so obligations move
closer to the artifact that runs instead of describing a representation that does not.

Costs, named because they are real: obligations mention `i₁` and need a mapping back to
source names for diagnostics; a loop `#[invariant]` must be placed at the header and refer
to the φ version — which is precisely where H23 lived, so it needs care rather than
assumed ease; and an obligation mentioning a φ needs the invariant or a case split, the
one genuinely new piece of work.

Choose the SSA stage deliberately: generate BEFORE `SSACleanup`, so optimisation changes
do not invalidate every stored claim, and make the cleanup→binary step its own register
row rather than folding it in silently.

Separately and cheaply, as the general rule this instance illustrates: every syntactic
analysis feeding a proof must be total over its input type with an unsafe default —
unknown construct invalidates everything in scope, or refuses to emit obligations there.
Gate the exhaustiveness.

**Relationship to R-0450 and R-0454 — read before scheduling.** These are three stages of
one pipeline and they are easy to mistake for overlapping work:

```
  program  --[R-0464: which representation?]-->  obligation
           --[R-0454: how is it identified?]-->  digest
           --[R-0450: how is it said?]------->   prover syntax
```

- **R-0450 does not subsume this.** It unifies the *lowering* (obligation → prover
  syntax); R-0464 changes the *input* obligations are generated from. Different stages,
  neither one's IR fixes the other's problem. But sequencing matters: the SSA generator
  should emit INTO R-0450's typed IR, so doing R-0464 first means writing the generator
  against today's ad-hoc lowerings and migrating it later. Prefer R-0450 first, or accept
  the rework knowingly.
- **R-0454 must land first, and must alpha-normalize.** Obligations generated from SSA
  mention `i₁` where AST-generated ones mention `i`. If variable names are in the digest
  basis, this task invalidates every stored digest. The invariant is now recorded in
  R-0454; this is the task that would otherwise pay for its absence.
- **Register A rows are re-anchored, not invalidated.** The rows in
  `VC_BRIDGE_REGISTER.md` state *if the flat goal holds, the runtime property holds*. The
  goal's subject changes representation here, so each row's "Assumes" clause must be
  restated against SSA — cheaper before R-0460 discharges rows than after.

## R-0465

_Stub: [ROADMAP.md](../project/ROADMAP_TASKS.md#task-r-0465) · disposition: done · owner(s): —_

`DischargeAdapter` is the right shape and is genuinely well built: one choke point, a
declared `actsOn` precondition, a declared `allowed` output set, and compile-time
`example`s proving untrusted backends cannot emit a proof class. But it governs one axis —
*who may claim what* — and evidence has two. The other is *what the claim rests on*, and
it had no framework at all until Register C. H23 lived entirely in the second axis, which
is why a well-designed firewall did not catch it: the fold correctly refuses to upgrade an
`assumed` VC, but a VC proved under an unestablished invariant carries
`proved_by_kernel_decision` and sails straight through.

Two pieces:

1. **Adapters declare assumption propagation**, defaulting to UNION and never to reset —
   same choke point, one more field, with a compile-time example per adapter alongside the
   existing `proofClasses` ones.
2. **Make the multi-kernel path a real adapter.** `foldMultiKernelResults` assigns its
   status by direct record update; there is no `multiKernelAdapter` in
   `dischargeAdapters`, no `allowed` list constraining `proved_by_two_kernels` /
   `proved_by_multi_kernel` / `kernel_disagreement`, and no compile-time example covering
   them. The newest and strongest classes in the system are the only ones outside the
   firewall built to govern exactly that. It has a hand-rolled `actsOn`, so it is not
   unsound today — but the protection is convention where everything around it is
   structure, and that distinction is the point of the firewall.

Register C (2026-07-31) supplies the representation this needs; this task is the wiring
that makes the guarantee structural at every backend rather than at the surfaces alone.

**Two more instances of the same shape, from the 2026-08-01 external review.** Neither is
reachable today; both are invariants held by convention where the type could hold them,
which is precisely this task's subject.

- **`foldMultiKernelResults` does not enforce that the closed and refused ID lists are
  disjoint.** Given `rocqClosed = ["k"]` and `rocqRefused = ["k"]`, both receipts are
  written but the `if closed … else if refused` chain passes only `.closed` to
  `multiKernelVerdict`, so the result is `proved_by_two_kernels` beside a stored refusal.
  Not reachable now — both sets are derived from one verdict list, and a verdict is either
  closed or refused — but the bare `List String × List String` API cannot say so. Passing
  the verdict list, or a type that makes the two mutually exclusive, removes the question.
- **The three consumers share the final function, not one typed input.** The report and the
  release policy each supply `leanClosed` independently; the ledger fold infers it from an
  existing status and applies its own extra guard. They agree today — verified on identical
  inputs, and a claimed divergence in that review turned out to compare different ones —
  but agreement rests on three call sites staying in step rather than on a shared value.
  Relatedly, `computeBelowTwoKernels` recomputes the evidence instead of reading the folded
  ledger, so the release gate and the stored artifact can be built from separate prover
  runs whose transient results differ.

## R-0260

_Stub: [ROADMAP.md](../project/ROADMAP_TASKS.md#task-r-0260) · disposition: superseded · owner(s): R-0506_

`shape-preserved`, `eval-preserved`, `source-preserved`, `trusted`, or
`blocked`, with theorem names and source links for each status.

## R-0262

_Stub: [ROADMAP_BACKLOG.md](../project/ROADMAP_BACKLOG.md#task-r-0262) · disposition: superseded · owner(s): R-0495, R-0512_

source Core evaluation, extracted PExpr evaluation, and extraction theorem
agree.

## R-0266

_Stub: [ROADMAP.md](../project/ROADMAP_TASKS.md#task-r-0266) · disposition: superseded · owner(s): R-0509_

Lean kernel, compiler modules, backend/toolchain, runtime/OS/hardware,
trusted/extern code, and the external Lean proof-library surface the corpus
cites (Lean stdlib / Batteries / Mathlib lemmas and tactics). Kernel-checked
library lemmas do not widen the trusted base, but they are a pinned
version/availability dependency (see Phase 11 proof-corpus migration); the TCB
record must name them so a proof's replay surface stays reproducible.

## R-0271

_Stub: [ROADMAP_BACKLOG.md](../project/ROADMAP_BACKLOG.md#task-r-0271) · disposition: superseded · owner(s): R-0510_

**Behavioral-law acceptance (planned 2026-10-18):** use R-0031's equality/hash
pilot to show that a generic proof imports explicit semantic laws as well as
method signatures. Bind each law to the exact callable/dictionary, proposition,
assumptions and instance identities through the existing evidence machinery.
Retain a valid instantiation, a substituted inconsistent implementation, a missing
law, and an explicit assumed law. The latter must propagate trust, while missing
evidence cannot silently become an assumption. An instance theorem cannot justify
other instances or a generic verdict. Keep testing, conditional proofs and
discharged law obligations distinct, and require no implicit trait resolution or
mandatory proofs for ordinary generic code.

## R-0273

_Stub: [ROADMAP.md](../project/ROADMAP_TASKS.md#task-r-0273) · disposition: superseded · owner(s): R-0491, R-0498_

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

## R-0274

_Stub: [ROADMAP.md](../project/ROADMAP_TASKS.md#task-r-0274) · disposition: superseded · owner(s): R-0499, R-0514_

 `typesCompatible` was lenient where SSA-verify was strict (the E0715 class);
 Elab carried a private int-vs-fixed-width re-elaboration Check knew nothing
 about; std skipped Check entirely while Elab/CoreCheck ran (H12). Shared
 predicates (`binOpOperandsAgree`) and LANGUAGE_INVARIANTS rule 19 gates DETECT
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
 facts pulled by real consumers. This task proves and
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

## R-0276

_Stub: [ROADMAP.md](../project/ROADMAP_TASKS.md#task-r-0276) · disposition: superseded · owner(s): R-0506, R-0496_

 (Rehomed from Phase 6B: its trigger is here, not there.) ProofCore still
 contains many `partial def` walkers/wrappers, which are opaque to the kernel.
 Do not chase a full rewrite. Add non-partial wrappers or structural recursion
 ONLY for the constructs pulled by the R-rule preservation proofs above, so a
 lifted rule can be reasoned about structurally. Gate each lifted rule with one
 theorem that would fail if the wrapper still delegated to an opaque
 partial-def shape.

## R-0456

_Stub: [ROADMAP_BACKLOG.md](../project/ROADMAP_BACKLOG.md#task-r-0456) · disposition: superseded · owner(s): R-0496_

The honest constraint, and it is permanent: **no single kernel can bridge two kernels** —
no checker sees both. So relating Lean's `eval` to another host's cannot be a proof. It
is a spec plus a conformance suite of `(neutral program, neutral input) -> expected PVal`
vectors that every host's `eval` must reproduce, and the resulting cross-semantics step is
`tested_by_oracle`. The composition rule folds that in explicitly rather than laundering
it as kernel evidence.

Isabelle/HOL is the target, and the port is tractable by design rather than by luck:
`PExpr`/`PVal` are simple inductive datatypes with no dependent types, partiality is
`Option`, and recursion is fuel-structural, which is `primrec` on `nat` — the easiest case
in a logic with no built-in general recursion.

**DE-PRIORITISED by measurement — read this before scheduling it.** This task was
originally justified partly on non-arithmetic proof ergonomics: port `eval` so another
host can prove refinement and functional-correctness properties. That justification does
not survive the deep-vs-shallow measurement recorded in
[docs/verification/PROVER_NEUTRAL_OBLIGATIONS.md](../verification/PROVER_NEUTRAL_OBLIGATIONS.md): per-function
properties are *cheaper* proved against the SHALLOW extraction, because symbolic fuel in
the deep embedding forces a case split per AST level before anything computes, and that
cost grows with expression depth. Shallow extraction to another host is a **printer** —
R-0455's factoring — not an `eval` port.

So porting `eval` buys exactly one thing: **meta-theory in that host**, i.e. letting
Isabelle check the preservation and transformation-soundness theorems itself. That is a
real benefit and a much narrower one than "Isabelle can prove non-arithmetic properties",
which the printer already delivers. Schedule this on meta-theory grounds or defer it;
do not schedule it to unlock per-function proofs.

Named risk: the `eval` port is where a subtle semantic divergence could silently weaken a
two-host claim, so the conformance suite must be adversarial rather than illustrative.

Depends on R-0454 for the neutral encoding and on R-0455 for the shared-versus-per-host
split, without which this arrives as a second extractor and the factoring never happens.

## R-0288

_Stub: [ROADMAP.md](../project/ROADMAP_TASKS.md#task-r-0288) · disposition: superseded · owner(s): R-0503_


V1 should cover a narrow subset: integer arithmetic, bools, branches,
direct calls, returns, checked runtime traps, small loads/stores, source-map
annotations, and helper calls. Add `concrete inspect --backend-ir`,
`--emit-backend-ir`, `concrete verify-ir --pass backend-ir`, golden
backend-IR fixtures, and round-trip checks that source maps, target
constants, runtime checks, and capability/trust labels survive lowering.
Gate old path vs BackendIR-mediated LLVM where both exist:
`interp == old compiled == BackendIR->LLVM compiled`, then retire the old
direct SSA->LLVM path once parity is established.

## R-0296

_Stub: [ROADMAP_BACKLOG.md](../project/ROADMAP_BACKLOG.md#task-r-0296) · disposition: retired · owner(s): R-0517, R-0505_

 Production QBE support must consume `ValidatedBackendIR`, emit the same source maps and
 target assumptions as other supported backends, pass the full C ABI and
 target/toolchain matrices for its declared subset, integrate Phase 8.5
 incremental artifacts, and report exactly which claims are translation-
 validated versus backend/target-trusted. Keep WASM, Cranelift, and any
 additional backend deferred until a forcing workload or uncovered bug class
 justifies another independent path.

## R-0297

_Stub: [ROADMAP_BACKLOG.md](../project/ROADMAP_BACKLOG.md#task-r-0297) · disposition: delegated · owner(s): R-0517, R-0505_

  7.5 immutable per-function job/deterministic merge contract, but keep MIR
  architecture-specific rather than creating a lowest-common-denominator
  universal machine IR. Require behavior coverage and target/ABI matrices
  comparable to the LLVM/QBE gates before any native backend becomes a
  default for any build mode.

**Small owned backend investigation (planned 2026-10-18):** compare a Lean 4
implementation with a later Concrete implementation under R-0413; record the
tradeoffs in [the Sparkle/native-backend analysis](../../research/compiler/sparkle-emitted-code-validation.md)
and [QBE implementation research](../../research/compiler/qbe-in-concrete.md).
Recommend Lean 4 first so executable target semantics and preservation theorems
can be developed with the existing compiler. Begin under R-0505's schedule, with one
explicit ISA/ABI/object-format target, integer/boolean functions and simple
instruction selection with explicit stack locations. A full QBE reimplementation,
optimizing register allocator and target matrix are not entrance requirements.
State which lowering edges are proved, checked or trusted; include the actual
assembly/object encoding and remaining assembler/linker/runtime assumptions.
Compare against external QBE/LLVM and an independent semantic oracle. Agreement
is testing, not a preservation theorem. Keep the existing workload gate and
record the new independent bug detection or reduced trust that justifies a third
backend's maintenance cost. Neither host language makes the output correct by
itself, and this investigation does not block R-0299/R-0300 or interop delivery.

## R-0348

_Stub: [ROADMAP.md](../project/ROADMAP_TASKS.md#task-r-0348) · disposition: merged · owner(s): R-0349_


  Required cleanup: remove or close the stale `LANGUAGE_GAPS.md` and
  `docs/bugs/README.md` entries that call interpolation a blocker unless a
  concrete workload reopens the decision. If a future workload proves the
  Writer path insufficient, add a new design note that preserves the same
  evidence: visible allocation, no macros, no hidden dynamic dispatch, and a
  gate proving fixed-buffer formatting remains allocation-free.
