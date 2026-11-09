# N2 round-2 review disposition

Owner: **R-0219**. This is a structural checker repair; no ConcreteSpec or ROADMAP
file is changed and no OD decision is taken. Reviews of record belong to the
coordinator with a different model. Reviewers must do the best long-term thing
rather than cut corners.

The structural cause was loss of declaration identity and proof/application
context during normalization, together with incomplete source provenance and
module selection. The checker now shares protected semantic comparison, bounded
proof traversal, source-aware resolution, and the single theorem population.
There is no per-law exemption or special C10 acceptance path.

| Review finding | Status | Structural repair and regression evidence |
| --- | --- | --- |
| 1: defeq erases subject/mutant; Not/Ne rejected | FIXED | Expand dependent wrappers while preserving every named semantic boundary simultaneously, then abstract subject/mutant constants at their actual levels. Preserve argument vectors while allowing Not/Ne and their aliases. `dependent-mutant-positive`, `dependent-mutant-fresh-argument`, `dependent-mutant-erased-boundary`, `wrapper-fresh-argument`, `wrapper-erased-mutant`, `wrapper-erased-instance`, `inequality-positive`, `negation-positive`, `negation-alias-positive`, `partial-negation`. |
| 2: definitionally equal mutant | FIXED | Check semantic definitional inequality as well as kind/type/name. `defeq-mutant` requires exactly `mutant-defeq`. |
| 3: unfolded hypotheses / definitional tautologies | FIXED | Compare hypothesis types with the conclusion and equality/iff sides with each other, independently of proof wrappers. `unfolded-hypothesis`, `wrapped-rfl`. |
| 4: cases/obtain/match, metadata, id and empty constructors | FIXED | Strip the complete application spine, recognize one-constructor recursor projections, raw-delta transparent wrappers and constructor proof fields. `obtain`, `cases`, `structure-cases`, `id`, `true-intro`, and the original projection controls. |
| 5: unmetered whnfR and budget=0 control | FIXED | Raw delta unfolds one definition at a time; one shared counter covers proof traversal, including branches. No whnfR chain is charged as one step. `positive-budget-chain` uses budget 3 and deeper wrappers and requires exactly `INCOMPLETE projection-budget`. |
| 6: false-elimination helpers and non-proof arguments | FIXED | Bounded transitive traversal with visited declarations follows only hypothesis proof arguments and proof subarguments. `helper-false`, `false-cases`, `helper-nomatch`, `empty-elimination`, `pempty-elimination`; `non-proof-decidable-argument` passes. |
| 7: single-module scope / omitted Obligations | FIXED | Repeated/comma-separated scopes compile one dependency union; each selected module must contribute authored evidence or an Obligation. Both C10 modules are audited and all five Obligations reported. `scoped-obligation-only` passes. |
| 8: unannotated examples, private leakage, namespace resolution | FIXED | Select only N2-annotated examples; restrict private names by module; walk ancestors and recover namespace/open context with Lean's parser. Controls cover annotated-only selection, private modules, ancestor namespaces, open/only/renaming/hiding, section boundaries, nested command opens, ambiguous opens and explicit root qualification. |
| 9: proof defs, missing provenance, empty docs, closure gap, LexError | FIXED | All authored declarations carrying proof bodies, including def and opaque, require classification. The shared rewriter reserves its generated-example marker in authored identifiers, preventing proof definitions from forging that provenance. Missing provenance is INCOMPLETE; matcher/equation metadata and parent ranges identify generated declarations. Unclassified evidence targets fail regardless of doc emptiness. Unscoped disk/import mismatch and lexical errors fail closed. `unclassified-def`, `unclassified-opaque`, `opaque-helper-positive`, `forged-example-provenance`, `example-marker-literals-positive`, `missing-provenance`, `imported-matcher-provenance`, `unclassified`, `closure-gap`, `lexical-error`. |
| 10: delimiters, reasons, Obligation syntax, duplicate fields | FIXED | Require closing delimiters, anchor reasons to the actual class header, validate full slice/row syntax, and consume exactly the evidence fields. Diagnostic-bearing example identities cannot collapse silently into a set wildcard. `missing-close`, `approval-missing-close`, `helper-reason-anchor`, `obligation-format`, `duplicate-field`. |
| 11: rules controls-only claim and modes | FIXED | Controls-only reports SPEC-RULES-CONTROLS, never specification-audit success; default rule behavior and JSONL remain compatible. Both rule entry points are executable. 31 exact rules controls pass. |
| 12: reusable workflow / required job mismatch | FIXED | One normal landing-record-gated Proof vacuity job follows proof-axioms/trust setup. Required-check identity is exact. Structural scopes accumulate in that job at contract promotion; the current unmigrated state is explicit. The separate workflow is manual-only. |

## Carry and C10

The result-wrapper shape objection was a gate false positive. Subject protection
now distinguishes a match on an operation result from a remaining open telescope,
and wrapper controls include invalid plants for a vacuous outcome branch. The
`toyBody_stack_model` membership discharge is still flagged with the exact
`instance-false-discharge` diagnostic. Every one of the previous 13 unknown names
is classified in [C10-audit.md](C10-audit.md): four gate false positives, nine real
source diagnostics. Both C10 source revisions report all five Obligations and no
INCOMPLETE result.

**NOT FIXED in this gate branch:** real source approvals and source evidence
findings listed in that audit. The source worktree is explicitly read-only; the
spec writer must repair or classify its evidence under the FINAL convention.
**Not yet promoted:** migrated structural CI scope, owned by R-0219. The owning
[README](README.md#required-check-transition) specifies the cumulative promotion
command. No whole-spec pass or grandfather list is introduced.

## Provenance regression confirmed during repair

At `33f4f00f6`, adding `def «⟪example⟫spoof» : True := trivial` to the positive
fixture incorrectly returned N2 PASS: the authored proof could forge the spelling
used by retained examples. The shared rewrite now rejects that reserved marker in
source identifiers before generating any retained names. Strings and comments
still pass. This establishes the generator's provenance boundary without a second
population list or a C10 exception. Its exact diagnostic is
`FAIL example-provenance [anonymous]`.

A second comparison regression used `mutant n := S 0`: expanding the mutant as
an ordinary dependency of S let `mutant 999` pass in place of the Instance's
`mutant 2`. The shared expansion routine now preserves every named subject and
mutant simultaneously. Its positive dependent-mutant control passes; fresh-argument
and erased-boundary controls require exactly `planted-proposition`.

An explicit-root regression likewise confirmed that `_root_.S` could resolve to a
local S after the root declaration was absent. Native resolution now receives the
original rooted identifier; the missing-root control requires `unknown-name`
and its associated `planted-subject` finding, while a real rooted name passes.

## Commits

- `7c2729f9a934f9b4dd8770cbb582bad92acf7540`: N2: align cumulative vacuity CI with landing check identities
- `bcfb993a9c62c92bc3ed20152e59b2b9ad51b0df`: N2: preserve semantic identities and audit proof context structurally
- `33f4f00f69e48e499b33ef0b1eeaeaf51149f7ca`: N2: classify all authored proof bodies including opaque declarations
- `b48189fbe7fb9bbf403a7d7e98e6c12059503fe1`: N2: authenticate retained examples at the shared rewrite boundary
- `51e46255c6f69ed2855cb30e7bf3bed6be413ae3`: N2: preserve dependent subjects and mutants as simultaneous boundaries
- `16fb6551f4dfc9f922c0fdcc3a4862722688af10`: N2: honor root-qualified references without local fallback

## Validation record

Final checker: `16fb6551f4dfc9f922c0fdcc3a4862722688af10`. All Lean elaboration held the supplied build slot,
and validation commands were bounded. These are focused checks; full suites,
full-spec builds and independent reviews remain with the coordinator on concrete-ci-2.

| Check | Exact checker/input commit | Result |
| --- | --- | --- |
| Complete vacuity controls | `16fb6551f4dfc9f922c0fdcc3a4862722688af10` | **91 PASS, 0 FAIL**; [record](audits/N2-controls.txt) |
| Six shared population views and registered-rule JSONL, byte for byte; unchanged axiom/trust/rule checker sources | `16fb6551f4dfc9f922c0fdcc3a4862722688af10` | **10 PASS**; [record](audits/N2-compatibility.txt) |
| C10 worktree with Spec.Frame/Proofs.Frame union | `16fb6551f4dfc9f922c0fdcc3a4862722688af10` | FAIL, 87 findings, 0 INCOMPLETE, 5 Obligations |
| C10 adc0656fc with the same union | `16fb6551f4dfc9f922c0fdcc3a4862722688af10` | FAIL, 98 findings, 0 INCOMPLETE, 5 Obligations |
| workflow_yaml | `16fb6551f4dfc9f922c0fdcc3a4862722688af10` | **3 PASS, 0 FAIL** |
| landing_script | `16fb6551f4dfc9f922c0fdcc3a4862722688af10` | **238 PASS, 0 FAIL** |
| rules controls only | `16fb6551f4dfc9f922c0fdcc3a4862722688af10` | **31 PASS, 0 FAIL**; SPEC-RULES-CONTROLS, no specification-audit claim |

[Machine-readable C10 validation](audits/C10-validation.json) binds both source
revisions, scopes, diagnostic counts and Obligation names to the checker SHA.
The 13 earlier unknown names are individually classified in the C10 audit;
4 were gate false positives and 9 remain real source diagnostics. The requested
toyBody membership discharge remains flagged, and wrapper-shape false positives
are repaired without weakening planted argument checks.

Documentation/link checks run after the audit commit and are reported with that
commit in the handoff. ConcreteSpec and ROADMAP are unchanged, and the C10 worktree
remains clean. No pushes or reviews of this work were run.
