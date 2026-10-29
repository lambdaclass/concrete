# Proof.lean and ProofCore.lean: where they overlap, and a structure for both (2026-10)

Status: research note, read-only audit, 2026-10-29, measured at origin/main `36d22567`. The owner
does not want proof churn now, so nothing here is to be implemented yet. This note maps the two
largest modules of the proof layer:
- `Concrete/Proof/Proof.lean`, 3472 lines;
- `Concrete/Proof/ProofCore.lean`, 4363 lines.

It records what each holds, where they overlap and what cannot move. It ends with a proposed
structure for when proof work resumes.

## The short answer

The two files are not two copies of one thing.

`Proof.lean` (namespace `Concrete.Proof`) is the proof model and its corpus:
- the pure expression language (`PExpr`, `PVal`, `PBinOp`);
- its evaluation semantics (`evalBinOp`, `eval`);
- function tables and attestation (`FnTable`, `AttestedPFnDef`);
- canonical renderings for digests;
- the flagship and stdlib models, with the proof-ladder lemmas.

`ProofCore.lean` (namespace `Concrete`) is a compiler pass. It turns checked Core into that
language and decides proof eligibility, the registry, obligations and diagnostics.

`ProofCore` imports `Proof`, and nothing imports the other way, so the dependency is a clean layer.
The overlap is real only in three places (see below).

## What each file holds, in order

### `Concrete/Proof/Proof.lean`

| lines | section | contents |
|---|---|---|
| 35–418 | the pure expression language | `PBinOp` (40), `PVal` (173), `PMatchPat`, `PExpr` (211); `SourceBodyDigest` (328–339); `PFnIdentity`, `PFnDef` (359), `SpecDecl` (391), `IdentifiedPFnDef` |
| 419–477 | environments and tables | `Env`, `FnTable` (attested, entries, globals, callables, schema version), `FnTable.empty` and its simp lemmas |
| 478–572 | attested entries | `AttestedPFnDef`, `FnTable.withAttestations`, `AttestedPFnDef.of`, `FnTable.ofAttested`, `FnTable.ofGlobals` |
| 573–858 | canonical rendering and the table root | `pbinOpCanonical`, `pvalCanonical`, `pexprCanonical` (643); evidence predicates (`entriesSorted`, `allIdentified`, `hasDuplicateIds`, `dispatchResolves`, `isEvidenceBearing`); `FnTable.root` (831) |
| 859–916 | the dual comparison | `FnTable.lookupById`, `withCallables`, `Env.empty` / `Env.bind` |
| 917–1454 | evaluation semantics | `wrapInt`, `satClamp`, `evalBinOp` (950–1243), `bindArgs`, `eval` (1252–1454) |
| 1455–1549 | embedded programs, basic lemmas | `proofFns`, `eval_lit` … `eval_mul_lits` |
| 1550–1813 | legacy packet demos | `parse_byte`, `check_length`, `validate`, `decode_header` (`proofFnsExt`) |
| 1814–2637 | flagship models and theorems | `cryptoFns`, `elfFns`, `parseValidateFns`, `fixedCapacityFns`, the while-step lemmas |
| 2646–2740 | constant-time tag | `ctTagFns` |
| 2741–2979 | SHA-256 and HMAC | the specification `PExpr`s (about 60 small helpers) |
| 2980–3029 | spec-drift fixture | `driftTestSpec`, `drift_test_theorem` |
| 3030–3219 | pure-core stdlib specifications | Option, Result, bytes, numeric, base64; `pureCoreFns` |
| 3220–3454 | the proof ladder | `lookupIndex_set_*`, `eval_while_*`, `eval_fuel_succ` / `eval_fuel_le`, `evalAssigns_fuel_le`, `eval_while_count` |
| 3455–3472 | specification parameters | eleven `SpecDecl`s |

### `Concrete/Proof/ProofCore.lean`

| lines | section | contents |
|---|---|---|
| 44–206 | shared analyses | defers, raw-pointer operations, extern names, allocation calls |
| 207–480 | call graph | `buildCallGraph`, effect opacity, `tarjanSCC`, `classifyRecursion` |
| 481–597 | loop boundedness | `LoopBound`, `classifyLoops` |
| 598–670 | body fingerprint V1 | `fingerprintExpr` over `CExpr` (frozen), `bodyFingerprint` |
| 671–1567 | Core to `PExpr` extraction | `binOpToPBinOp`, `castToPExpr`, escape analysis, `pexprMentions` (837), `cExprToPExprImpl` (931), `cStmtsToPExprKImpl` (1058), `identifyUnsupported` (1412–1567) |
| 1568–1771 | eligibility and admission | `isProofEligible`, `AdmissionRefusal`, `EligibilityEntry` |
| 1772–1874 | registry and specification attachment | `ProofRegistryEntry`, `FunctionIdentity`, `SpecIdentity`, `SpecAttachment` |
| 1875–2089 | the obligation model | `ObligationStatus`, `Obligation` |
| 2090–2241 | diagnostics | `ProofDiagnosticKind`, `ProofDiagnostic` |
| 2242–2367 | the artifact | `ProofCoreEntry` (the extracted `PExpr`, fingerprint, definition identity, subject digest), `ProofCore` |
| 2368–3234 | registry validation and identity glue | `RegistryIssue`, `proofSubjectDigestV2` (2451), implementation manifests, `dependencyNodesOf`, `pexprCallKeys` (2701), subject binding, `validateRegistry` |
| 3235–3964 | pipeline orchestration | `assessEligibility`, `extractModule`, `generateObligations`, `generateDiagnostics`, `extractProofCore` |
| 3965–4027 | printing and accessors | `summary`, `findEntry` |
| 4028–4363 | self-checks and transforms | `selfCheck` (4041–4288), `scopeToUser`, `relocate`, table-resolution refusals |

## Who imports them

- Only `Proof`:
  - `Proof/BodyIdentity` and `Proof/DependencyEdge`;
  - every `ProofKit` module;
  - all 13 `proofs/Examples/*/Proofs.lean`;
  - two fixtures of the operational VC auto-discharge gate.
- Only `ProofCore`: `Proof/DependencyEdges`, `Proof/Issue`, `Report/ModelDiff`, `Report/DebugBundle`,
  `Report/Reduce`, `Check/Policy`.
- Both: `Proof/ProofSoundness`, `Report/ReportInterface`, `ReportVC`, `ReportBase`, and the report
  modules (`Effects` and `ObligationDiscovery` once the report split lands).
- By name, not by import:
  - `scripts/tools/identity_rows.sh` reads `Proof.lean` by path;
  - `scripts/gen/classifications.lean` finds the `SpecDecl` and `PFnDef` constants by reflection;
  - `Concrete/Proof/ClassificationTable.lean` (40–42, 284–288) and `TableResolve.lean` (73–74) hold
    fully qualified names such as `"Concrete.Proof.proofFns"` as strings.

## Where they overlap

| concept | in `Proof.lean` | in `ProofCore.lean` | verdict |
|---|---|---|---|
| the `PExpr` language | defines it | builds it from Core | producer and consumer; correct split |
| binary-operator semantics | `evalBinOp` | `binOpToPBinOp` emits only what `evalBinOp` supports (comments at 680, 702) | a coupled contract, proved in `ProofSoundness`; not a copy |
| body digests | `pexprCanonical`, `SourceBodyDigest`, `FnTable.root` (over the model) | `bodyFingerprint` (V1, over `CExpr`), `proofSubjectDigestV2`, and a call to `sourceBodyDigestV1Of` (3009) | three different subjects, but **two "body" digests side by side**: the frozen V1 `CExpr` fingerprint still drives `ProofRegistryEntry.bodyFingerprint` and staleness next to `sourceBodyDigestV1Of` / V2. The most likely place for an identity mix-up |
| `PExpr` traversals | `eval`, `pexprCanonical` | `pexprMentions` (837), `pexprCallKeys` (2701) | the same *pattern* duplicated about eight times across the repository (also `BodyIdentity` four times, `ProofSoundness`, the report's `renderPExpr` and `renderPExprAsLean`). There is no shared fold, so each new `PExpr` constructor is a set of hand edits that `check_constructor_coverage.sh` has to police |
| function tables and identity | `FnTable`, `PFnIdentity`, `AttestedPFnDef`, `lookupById` | `ProofCoreEntry.definitionIdentity`, `FunctionIdentity`, `SpecIdentity`, table-resolution refusals | producer and consumer (model side and source side); `FunctionIdentity` / `SpecIdentity` overlap in concept with `DefinitionIdentity` |
| attestation and subject binding | `AttestedPFnDef`, `withAttestations` | `SubjectBinding`, `specIsStale`, `validateRegistry` | two halves of one "model and source agree" story, split across the files |
| "canonical" rendering | encodings for digests | `ObligationStatus.canonical`, diagnostic codes, registry-issue text | same word, different role (digest bytes and report text) |
| eligibility | described in `Proof.lean`'s header only | all of it | none |

## What cannot move without proof churn

Every `proofs/` file imports `Proof.lean` alone, and they use, by name:
- **the model:** `PExpr`, `PVal`, `PBinOp`, `PFnDef`, `SpecDecl`, `Env`, `FnTable`, `FnTable.root`,
  `FnTable.withAttestations`, `AttestedPFnDef.of`;
- **the semantics:** `eval` (including `eval.evalArgs` in simp sets), `evalBinOp`, `bindArgs`,
  `wrapInt` and its lemmas;
- **the ladder:** `eval_fuel_le`, `eval_while_count`, `eval_while_step_unfold`, the while-step
  lemmas;
- **every flagship model, by name:** for example `CryptoVerify/Proofs.lean:32` simp-unfolds them.

No `ProofCore` declaration appears in `proofs/`. Beyond `proofs/`, the qualified names hard-coded
in `ClassificationTable`, `TableResolve` and `scripts/tests/proof_namespace_allowlist.txt` pin the
`Concrete.Proof` namespace and the declaration names.

So declarations may move between files, but not be renamed or change namespace. Lean resolves
`Concrete.Proof.X` wherever it is declared, as long as the importing module still imports it.

## Stale or dead-looking code

These come from whole-word greps, so generic names are approximate; check each before deleting.

- **`Proof.lean`:**
  - the tombstone comment at 2638–2645 for the removed `provedFunctions`;
  - the legacy packet demos (1455–1813), which no `proofs/` file uses. `ClassificationTable` and
    `TableResolve` reference only three theorems and the two tables, as strings, and gates read them
    too. The rest is file-local.
  - basic lemmas (`eval_lit`, `eval_add_lits`, …) and several model helpers (`ctCompareFn`,
    `fcTagFn`, the `ring*Fn`, `verifyMessageFn`, `parseHeaderFn`, the `evalComputeTag` wrappers) that
    nothing outside the file names.
- **Not dead:** the eleven `SpecDecl`s. No Lean code names them, but
  `scripts/gen/classifications.lean` reads them by reflection.
- **`ProofCore.lean`:** `FunctionIdentity`, `SpecIdentity`, `SpecSource`, `ExclusionKind`,
  `ObligationStatus.allCanonical`, `ProofCore.findExcluded`, `registryIssuesToDiagnostics`,
  `classifyIneligible` and `admissionRefusalsOf` are not referenced outside the file.
  `correspondenceInputOf` says "SHADOW. Nothing consumes this."; one other file mentions its name.

The dead-code item of the cleanup (a Lean-side unused-declaration finder) is the right tool to
confirm these, with reflection and proof use treated as live.

## Proposed structure (for when proof work resumes)

The rule is the report split's: move declarations verbatim, keep their names and namespaces, and
leave the old module as an umbrella that imports the new ones, so no importer changes. Under that
rule no proof changes; the proofs only need to keep building.

`Proof.lean` would become six modules:

| module | contents | lines |
|---|---|---|
| `Proof/Syntax.lean` | `PBinOp`, `PVal`, `PMatchPat`, `PExpr` | ~290 |
| `Proof/Model.lean` | `SourceBodyDigest`, `PFnIdentity`, `PFnDef`, `SpecDecl`, `IdentifiedPFnDef` | ~90 |
| `Proof/Table.lean` | `Env`, `FnTable`, attestation, canonical rendering, `FnTable.root`; it does not need the semantics | ~500 |
| `Proof/Semantics.lean` | `wrapInt`, `evalBinOp`, `eval`, `bindArgs` | ~540 |
| `Proof/Ladder.lean` | the basic, while-step and ladder lemmas | ~350 |
| `Proof/Models/*.lean` | the flagship and stdlib model corpus, one module per example family | ~1700 |

`Proof.lean` stays as the umbrella, since `proofs/` imports it.

`ProofCore.lean` would become six modules:

| module | contents | lines |
|---|---|---|
| `Proof/CoreAnalysis.lean` | helpers, call graph and recursion, loop boundedness, the V1 fingerprint | ~625 |
| `Proof/Extract.lean` | Core to `PExpr`; needs only `Proof/Syntax` and `CExpr` | ~900 |
| `Proof/Eligibility.lean` | eligibility and admission | ~200 |
| `Proof/Records.lean` | registry, obligation and diagnostic types; close to `Issue` and `ObligationCore` | ~470 |
| `Proof/Binding.lean` | registry validation, subject binding, identity glue | ~870 |
| `ProofCore.lean` | the pipeline, the artifact, self-checks and transforms | ~1250 |

`ProofSoundness` would then import `Proof/Extract` and `Proof/Semantics`, which is the theorem it
states, instead of the whole pass.

## Three findings worth acting on before any split

1. **Two body digests side by side.** The frozen V1 `CExpr` fingerprint still decides registry
   staleness while V1/V2 source-body digests exist next to it (R-0004's territory). Which one
   decides is the kind of fact that should have one owner. The `DependencyEdge` and `BodyIdentity`
   work already tracks the identity side.
2. **No shared `PExpr` fold.** About eight exhaustive hand-written walkers. A single
   `PExpr.foldM`, or a `Children` view, would let `pexprMentions`, `pexprCallKeys`,
   `pexprCanonical` and the renderers stop restating the constructor list. This is a structural
   class (one fact kept in several places), not a style point.
3. **Dead legacy demos.** Removing `Proof.lean` 1455–1813 is churn the owner has deferred; the
   dead-code ratchet can hold them in its baseline until then.
