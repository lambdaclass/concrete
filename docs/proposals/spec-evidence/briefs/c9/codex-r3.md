Changes required under the binding vacuity rule. The terminal-state and drive counterexample repairs are substantive; the identity and evidence laws still contain construction-only guarantees.

Read-only inspection only: no files modified, network access, builds, or gates. Validation remains with the coordinator.

Findings:

1. **ConcreteSpec/Proofs/Ident.lean:13 — major — REAL DEFECT.** `stable_renumber` merely projects the field that `renumber` explicitly leaves untouched. Injectivity is unused. The successor witness changes a number, but never exercises stable-key assignment; even an assignment based entirely on lexical numbering escapes this theorem.

   **Fix:** specify a key-assignment interface over identifiable program occurrences and prove equivariance when the program’s sites are renumbered. Keep token contents abstract. Plant an assignment that derives its token from the lexical number and show the same law fails.

2. **ConcreteSpec/Proofs/Ident.lean:26 — major — REAL DEFECT.** `site_independent` is constructor projection. The argument-difference laws at lines 32 and 39 are record-equality consequences. These establish the record’s shape, but under the owner’s explicit rule they do not constitute non-vacuous specialization identity laws. No specialization lookup or sharing operation participates.

   **Fix:** define specialization resolution/interning against these keys and prove that requests from distinct sites resolve to the same specialization, while different arguments resolve distinctly. Test site-keyed and argument-dropping implementations. The current `onlyGeneric` example also covers only type arguments; provide the capability counterpart.

3. **ConcreteSpec/Proofs/Ident.lean:47 — major — REAL DEFECT.** `location_determines_line_col` assumes both spans’ coordinates equal the source-derived coordinates, then substitutes those equalities. Its wrong-column witness is excluded by that very premise. It therefore fails the requested vacuity criterion.

   Moreover, `LocationOk` is not required by `SiteTable.Valid` in **ConcreteSpec/Spec/Observe.lean:117**. A relative-path span with contradictory offsets and coordinates remains a valid table entry.

   **Fix:** connect source bytes to the actual span-production interface and prove its output coordinates against an independent byte-scanning relation. Require source consistency at the site-table validation boundary. Include newline, multibyte UTF-8, tab, EOF, and incorrect-coordinate mutants.

4. **ConcreteSpec/Proofs/Prim.lean:129 — major — REAL DEFECT.** `discharge_fact` transports an existing proof across the proposition equality supplied by `Discharges`; neither context equality nor obligation identity participates. The planted mismatch at line 497 changes the identity while retaining the same true proposition. It cannot falsify the theorem’s conclusion—even a consumer ignoring identity could still return that proposition’s proof.

   The original reflexive `usable_current` also remains at line 124.

   **Fix:** specify an actual evidence-consumption operation bound to an authoritative obligation lookup. Prove successful consumption establishes the proposition retrieved for the requested identity and context; prove mismatches cannot produce successful consumption. Plant consumers omitting identity, context, and proposition binding, and demonstrate failures of those respective properties.

5. **ConcreteSpec/Proofs/Ident.lean:18 — major — REAL DEFECT.** `collision_refuses` contradicts the pairwise condition defining `WellFormed`. It does not establish load-time refusal or a diagnostic naming both packages. Retaining identities in a supplied collision witness is insufficient to establish either behavior. Under the binding rule, this is another definition-restating law.

   **Fix:** define a load-validation result carrying collision diagnostics, then prove refusal and diagnostic contents against an independently stated collision relation. The coexistence mutant must violate that validator’s law.

6. **ConcreteSpec/Proofs/Observe2.lean:385 — minor — REAL DEFECT.** The planted counterexample defeats the *old full-span claim*, not the revised rendered-field injectivity law. Thus the revised law lacks its required planted failing implementation.

   **Fix:** supply a renderer mutant omitting or ambiguously encoding a rendered field, with distinct rendered tuples that collide. Retain the range example as justification for the theorem’s revised scope.

The law audit below distinguishes substantive behavioral laws from the construction-only laws prohibited by this review’s rule.

| Law | Vacuity verdict and witness |
|---|---|
| `StableSiteKey.stable_renumber` | **Fails:** successor changes the lexical site; persistent key is copied unchanged. Mutant exists, but no assignment is tested. |
| `LoadedModules.collision_refuses` | **Fails:** `p1/first` and `p2/first` instantiate the definition’s forbidden pair; coexistence is compared against that predicate, not loading. |
| `SpecializationRequest.site_independent` | **Fails:** sites 0 and 1 construct identical specialization subrecords. |
| `SpecializationId.ne_of_typeArguments_ne` | **Fails:** `[0]` versus `[1]` is record discrimination. Generic-only mutant supplied. |
| `SpecializationId.ne_of_capabilityArguments_ne` | **Fails:** `Fs` versus `Console` is record discrimination. Capability-specific mutant missing. |
| `Span.location_determines_line_col` | **Fails:** tab spans share an assumed source-derived coordinate; wrong-column span is excluded by `LocationOk`. |
| `SafetyEvidence.usable_current` | **Fails:** context equality reflexivity remains. |
| `SafetyEvidence.discharge_fact` | **Fails the requested substantive-consumption criterion:** arithmetic proof transport; identity mismatch does not falsify its conclusion. |
| Five evidence mismatch laws | **Construction-only:** revised examples now correctly change precisely the named field and preserve the others. They repair the counterexamples, but still test record equality rather than consumption. |
| `Ghost.runStmts_of_not_normal` | **Original guard-restating limitation remains.** The returned-state example is inhabited; the stronger terminal law supplies the useful behavioral statement. |
| `Ghost.runStmts_terminal` | **Pass:** returned and trapped states preserve the complete report despite trailing `abort`; restarting mutant changes locals and ending. |
| `Ghost.drive_steps` | **Pass:** primitive execution supplies a steps derivation; fabricated-event report has no corresponding derivation. |
| `Ghost.drive_steps_result` | **Pass:** same execution without auxiliary report premise; fabricated-store report is unreachable. |
| `Ghost.ghost_erasure_run` | **Pass:** mixed ghost operations and concrete `skip`; deleting concrete `skip` changes remaining fuel. |
| `Ghost.ghost_erasure_ending` | **Pass as a corollary:** mixed execution preserves observations; deleting concrete `abort` changes ending. |
| `PrimRegistry.answerOk_iff` witness | Existing declared-failure witness remains. New two-type examples substantively expose removal of the failure-type equality check. |
| `trapLine_injective` | **Substantive decoding law; counterexample requirement incomplete:** distinct ranges witness the revised scope, but the planted failure targets the previous statement. |
| Changed decoder round-trip lemmas | **Substantive:** parsing reconstructs rendered fields through delimiter and decimal decoding; no invariant-checking guard makes the round trip automatic. |

The unchanged renaming, rehinting, registry, and ghost-erasure laws are not newly introduced by this diff. The amended witnesses do not reveal a new regression in them.

The schema separates the requested concepts sensibly: obligation site/kind/discriminator, package-qualified module identity, specialization identity versus request edges, and typed premise dependencies. I found no encoded choice for either opaque token’s contents, nor an extra undecided premise category. OD44 coexistence appears only as an explicitly rejected mutant. I would nevertheless withhold schema-freeze approval until the assignment, resolution, consumption, and source-validation interfaces above make these fields operationally accountable.

On the three owner questions:

- **`FunctionRelativeKey.token`:** recommend a structured syntactic path, with explicitly defined handling of generated and unnamed constructs. Per-kind ordinals need a stated edit-stability contract because inserting an earlier peer can renumber them. Keep the representation abstract until the owner answers.
- **`PackageId.token`:** name and version is a reasonable proposal, but first decide whether equal name/version from different origins denotes the same package or requires refusal. That determines whether the proposal is a complete identity. Do not encode it yet.
- **Future premise categories:** recommend new constructors only after a decided row authorizes the category, with an explicit schema-version and unknown-category refusal policy. The current closed enumeration is consistent with that direction.

The supplied merge resolution is correct: both `Prim` and `Observe2` proof imports survive, and both `Prim` and `Observe` specification imports survive. I found no dropped side in those hunks.

VERDICT: CHANGES