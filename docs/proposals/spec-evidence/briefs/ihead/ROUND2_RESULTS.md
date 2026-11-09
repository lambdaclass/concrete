# Round 2 integration result

Final head: `3b54f736db89cc49c5582b8b8caf9de882dec885`.
All four exact requested heads are ancestors. No pushes, reviewers or git notes.
BRIEF.md and COMMON.md were absent. No edits to decisions.md or ROADMAP.md.

## Merges and resolutions

1. `eecb00e8b56f7adddfd743e8046a02d14a583765`: C9 `971eb025d`.
   Only conflict: scripts/proofs/axioms.golden; integration side retained verbatim.
2. `50dd56ca23d97eb3b82ec445067da8454aeb5926`: C10 `871db4e19`.
   Frame.storingFn and the private Frame c1FnId/c1UnitFnId conflicted because round 1
   had migrated their empty module identities. Kept C10's own package `frame`, version `0`,
   module `["frame"]` identities. Kept round 1's C4 result-indexed proof applications.
   Golden conflict: integration side retained verbatim.
3. `51ea5c7e2e46a65831338a6b9c39c49373f215d0`: C6 `f2321db98`.
   ConcreteSpec.lean conflict: union of Own/Grant/Frame and all six new observation modules.
   Golden conflict: integration side retained verbatim.
   C9/C6 declaration overlaps auto-merged: SourceTable, SiteTable.Valid, ProjectSources,
   SourceLoadRefusal, SourceLoadResult, LoadedProject and all loader operations/specifications,
   trapLineChars/trapLine and source loader proofs retain C9's forms. Ident is identical to C9.
   C6 supplies the rendered-field trapLine_injective, observation state, executable environments,
   source-span producers and adapter/report proofs. C9's byte-range counterexample survives.
   ExecAdapter admits a project through C9's loader and projects its site invariant;
   there is no second SiteTable.Valid or string PathOk.

   Final-store overlap: C6 adds the actual store to Exec.Ending.final/runFrom. Migrated
   Own.eraseEnding to erase that store, Own fixtures/pattern matches, Exec's planted mutant,
   and Adequacy's instrumented evaluator, Conclusion, ReportedQueries, completeness/soundness,
   termination evidence and examples. Termination existentially quantifies the final store,
   rather than requiring it to equal the initial store. Prim/Ghost's corresponding source-head
   migration was preserved. No execution or language rule was replaced.

   C6's stronger WellFormed.blocked exposed two false old fixture helper claims:
   C1 Demo.env returns none on malformed primitive shapes while the adapters classify every
   request as internal. Adequacy demos now explicitly use observationEnv, a total fixture
   oracle preserving each original answer and returning an error answer for otherwise
   unanswered malformed requests. observationEnv_of_answer proves original answers survive.
   The normative C1 oracle is unchanged. The adapters now supply state and an empty project
   admitted by the loader. Conclusion.final_config is a store-independent inversion helper.

4. `3b54f736db89cc49c5582b8b8caf9de882dec885`: base migration `9e48fdb06`.
   No conflicts. Its changes concern base Proofs/{Authority,Basics,Borrow,Brand,Checker,Cleanup,
   Examples,Observe,Rep,Slice0,Slice1}; Exec and Scalar are untouched. Base Proofs/Observe is
   the earlier ConcreteSpec namespace, distinct from C6's Core.Observe observation model.
   No semantic conflict requiring a third design. The golden's 31-line automatic base merge
   was retained; it was neither hand-edited nor regenerated. Full regeneration remains the
   coordinator's blocked owner-decision task. Its empty-axiom TSV fields produce expected
   trailing-tab diagnostics in git diff --check; no generated field was stripped.

## Approvals reset

All reset approvals are now PENDING (conservatively including changed proof/example blocks):

- ConcreteSpec/Proofs/Adequacy.lean: runFrom_complete
- ConcreteSpec/Proofs/Adequacy.lean: runFrom_complete_le
- ConcreteSpec/Proofs/Adequacy.lean: runFrom_final_sound
- ConcreteSpec/Proofs/Adequacy.lean: spin_move_01
- ConcreteSpec/Proofs/Adequacy.lean: spin_move_12
- ConcreteSpec/Proofs/Adequacy.lean: spin_move_23
- ConcreteSpec/Proofs/Adequacy.lean: spin_move_34
- ConcreteSpec/Proofs/Adequacy.lean: spin_move_40
- ConcreteSpec/Proofs/Adequacy.lean: spin_move
- ConcreteSpec/Proofs/Adequacy.lean: spin_internal_silent_divergence
- ConcreteSpec/Proofs/Adequacy.lean: event_move_01
- ConcreteSpec/Proofs/Adequacy.lean: event_move_12
- ConcreteSpec/Proofs/Adequacy.lean: event_move_23
- ConcreteSpec/Proofs/Adequacy.lean: event_move_40
- ConcreteSpec/Proofs/Adequacy.lean: event_move_34
- ConcreteSpec/Proofs/Adequacy.lean: event_move
- ConcreteSpec/Proofs/Adequacy.lean: erasingWellFormed
- ConcreteSpec/Proofs/Adequacy.lean: keepingWellFormed
- ConcreteSpec/Proofs/Adequacy.lean: erasing_silent_divergence
- ConcreteSpec/Proofs/Adequacy.lean: keeping_infinite_interaction
- ConcreteSpec/Proofs/Adequacy.lean: logThenSpin_move
- ConcreteSpec/Proofs/Adequacy.lean: terminalPath

New helpers `Conclusion.final_config`, `Demo.observationEnv`, and
`Demo.observationEnv_of_answer` are PENDING. All source-head approvals otherwise retained.

## Validation

Each merge's complete candidate tree passed the mandated wrapper build before commit;
failed C6 integration builds were repaired before committing. C6 and base also passed
post-commit builds. Exact post-commit builds of C9 eecb00e8b and C10 50dd56ca2 also passed (58 jobs each).
C6 51ea5c7e2 and final base head 3b54f736d passed 64-job post-commit builds.
Final 3b54f736d rules: PASS=32 FAIL=0, exit 0.
Final 3b54f736d trust ban: PASS=23 FAIL=0, exit 0; 62 source modules,
17,355 constants and all 1,894 examples checked.
Logs are preserved under _review/ROUND2-*.log.
All build commands use the owner's 1200-second limit, overriding ROUND2's 1800 seconds.
ISSUES.tsv has no duplicate IDs; no unmerged paths; working tree clean.

## QUESTIONS

No new owner decision is needed; QUESTIONS2.md is not required. QUESTIONS.md is historical
round-one context, resolved by the accepted C6 head. Golden regeneration remains deferred.
