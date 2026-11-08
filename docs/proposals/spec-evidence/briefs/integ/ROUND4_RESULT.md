# Round 4 result

Final code commit: `ed98a8476a1464d37d7645dbc00047c8755b8128`. Only `ConcreteSpec/Spec/Adequacy.lean` and
`ConcreteSpec/Proofs/Adequacy.lean` changed. No push, history rewrite, hook bypass, protected
module edit, golden edit, or language-rule change was made.

## Findings and design

- **Codex 1 / agy 3, oracle preservation:** `Move.toQueryOracle_answer` is now an exact iff,
  including successor queries. It is explicitly an inversion helper, not execution evidence.
  `runFromQ_request_correspondence` instead starts from nonfinality and an issued request,
  and proves the independent `RequestResolution` oracle/contract relation for the evaluator's
  report. Unanswered, invalid-answer, successful-answer, and refused-continuation instances
  are closed applications. The wrong-history evaluator resets occurrences before querying;
  at the exact repeated-request instance it substitutes an earlier answer and violates the
  correspondence while both issuance premises remain true. A fabricated-final mutant is
  rejected separately. No answer or successful transition is a premise of this law.
- **Codex 2, vacuous termination evidence:** `CallDomain` fixes function and calls outside the
  certificate. `ConditionalTermination` proves coverage of every declared call, subject to
  explicitly named trusted premises. `TerminationEvidence` discharges those premises and
  requires an actual domain inhabitant; there is no certificate-local admission filter.
  `terminatingEvidence` certifies an actual entry into `boomFn`, ending in a trapped final
  outcome (termination does not require success). Empty domains, missing functions, refused
  entry, and an admitted spinning function all have negative examples. The exported fuel
  extension law has a closed real certificate instance and a mutant that preserves the
  certificate budget (10) but fails at the law's larger budget (11).
- **Codex 3 / agy audit disagreement, round trips:** Codex's classification is correct because
  both conversions preserve exactly the same `states` function. The round trips and their
  existential packaging are Representation lemmas, with approvals PENDING. Independent
  behavioral evidence is `InfiniteExecution.evaluator_prefix`, proved by induction over actual
  transitions, then packaged by `toFuelLimit`. Its real logging-prefix instance rejects a
  history-forgetting evaluator. Reconstruction still uses `move_of_fuel_succ`; a sequence
  skipping every second position fails its one-step compatibility obligation.
- **agy 1 / Codex assessment difference, observations:** canonical matching witnesses are
  useful existence helpers, but the exported finite and infinite laws now quantify over
  **every** matching C6 stream. `MatchesProjectedHistory.sameObservation` composes mutual
  prefix coverage through the actual projected histories. The matcher mutation `uncheckedHistory`
  leaves finite/unbounded classification hypotheses true while breaking universal soundness.
  A constantly emitting stream refutes its finite claim; an all-silent stream refutes its
  infinite claim. This strengthens the statement itself as agy requested.
- **agy 2, interleaving:** added the empty-history helper and public `runQ_queryHistoryValid`.
  `runFromQ_interleaved_site` relates two same-site queries at arbitrary ordered positions by
  the exact per-site count in the half-open interval. `runFromQ_interleaved_other_sites`
  specializes this to occurrence + 1 when all intervening sites differ. The actual three-query
  example revisits site 0 after site 1. `sharedSiteRun` preserves both endpoint-query premises,
  but rewrites the intervening site and fails the count law and history-validity law.

No semantic repair is staged. Review approval of exemptions remains outstanding, as explicitly
required by the FINAL convention. No independent reviewer was run during this writer turn;
this report does not claim approval of the pending exemptions or an automated N2-kernel audit.

## Verification

At `ed98a8476a1464d37d7645dbc00047c8755b8128`:

- `nix develop -c timeout 1200 lake build ConcreteSpec`: PASS (47 jobs).
- `nix develop -c timeout 600 bash scripts/spec/rules.sh`: PASS=32 FAIL=0.
- `nix develop -c timeout 900 bash scripts/proofs/trust_ban.sh`: PASS=23 FAIL=0; 8,432 declarations and 335 examples checked.
- `git diff --check`: PASS.
- Source classification audit: 96 theorems, exactly one class tag each;
  10 Laws, all with tagged closed Instance/Planted pairs. This is a source-cardinality
  audit; Lean checked their terms. It is not the proposed full FINAL projection gate.
- Protected modules and axioms golden compare unchanged against `fcb18d674`.
- Forbidden heavy gates were not run.

Workflow correction: `b27323c9c` was mistakenly committed before inspecting its failed build
(the request unfolding helper had one unclosed reflexive equality). `c39dce34d` immediately
fixed it and passed the build. History was preserved. Every later slice was committed only
after its successful build; the final exact commit has also been built explicitly.

## Commits

```text
b27323c9c spec: relate arbitrary projected streams and interval query counts
c39dce34d spec: close request computation equality
43a2f5df0 spec: separate conditional termination from inhabited domain certificates
0e0c79795 spec: exercise usable termination evidence and reject vacuous certificates
ae69d2192 spec: prove evaluator request resolution without assumed answers
b9c18fa1b spec: add closed request and universal observation mutation controls
3f1ae5ed8 spec: reject wrong-history oracle execution and exercise interleaved requests
88a2029f1 spec: classify adequacy proofs and add premise-preserving interval mutation
6f8741f8f spec: make pointwise evaluator adequacy the infinite correspondence evidence
78e62b1a4 spec: authenticate core adequacy and termination laws with closed mutants
6349df47e spec: specialize interleaved counts and reject skipped-step reconstruction
ed98a8476 spec: explain adequacy classifications and independent domain coverage
```

## Laws, statements, and exact witnesses

Each linked example is in the proof module. Each Instance is a full closed application of the
named law; each Planted includes the mutated hypotheses at that instance's arguments and the
negated mutated conclusion. Additional untagged concrete controls exercise auxiliary helpers
and the termination interface's coverage obligations.

### `runFromQ_queryHistoryValid`

[Declaration](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:84). Law (subject `runFromQ`).
**Execution-level occurrence invariant.** Every query reported by an actual instrumented run
counts exactly the earlier queries at its own site, including terminal unanswered queries.

- [Instance at line 2255](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:2255): Instance (law `runFromQ_queryHistoryValid`, case `threeRequestConfig`).
- [Planted at line 2260](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:2260): Planted (law `runFromQ_queryHistoryValid`, case `threeRequestConfig`, subject `runFromQ`, mutant `sharedSiteRun`).

### `runFromQ_interleaved_site`

[Declaration](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:143). Law (subject `runFromQ`).
Between two issued queries at the same site, the occurrence increases by exactly the number
of requests at that site in the intervening half-open interval; other sites contribute zero.

- [Instance at line 2136](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:2136): Instance (law `runFromQ_interleaved_site`, case `threeRequestConfig`).
- [Planted at line 2151](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:2151): Planted (law `runFromQ_interleaved_site`, case `threeRequestConfig`, subject `runFromQ`, mutant `sharedSiteRun`).

### `runFromQ_toReport`

[Declaration](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:237). Law (subject `runFromQ`).
Erasing query instrumentation gives C1's original driver when the query-level oracle is C1's
oracle lifted to ignore occurrence numbers.

- [Instance at line 2167](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:2167): Instance (law `runFromQ_toReport`, case `terminalConfig`).
- [Planted at line 2172](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:2172): Planted (law `runFromQ_toReport`, case `terminalConfig`, subject `runFromQ`, mutant `droppedAnswerRun`).

### `runFromQ_request_correspondence`

[Declaration](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:328). Law (subject `runFromQ`).
An actually issued request is answered at its exact event and query
history. The report obeys that answer, the contract, and the partial continuation, in every branch.

- [Instance at line 1947](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:1947): Instance (law `runFromQ_request_correspondence`, case `terminalConfig`).
- [Instance at line 1954](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:1954): Instance (law `runFromQ_request_correspondence`, case `badAnswerOracle`).
- [Instance at line 1961](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:1961): Instance (law `runFromQ_request_correspondence`, case `env`).
- [Instance at line 2065](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:2065): Instance (law `runFromQ_request_correspondence`, case `occurrenceSensitiveOracle`).
- [Instance at line 2104](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:2104): Instance (law `runFromQ_request_correspondence`, case `refusingBindConfig`).
- [Planted at line 1974](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:1974): Planted (law `runFromQ_request_correspondence`, case `terminalConfig`, subject `runFromQ`, mutant `fabricatedFinalRun`). The issuance premises remain true.
- [Planted at line 2073](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:2073): Planted (law `runFromQ_request_correspondence`, case `occurrenceSensitiveOracle`, subject `runFromQ`, mutant `wrongHistoryRun`). Resetting the occurrence substitutes the first answer for an unanswered repeated request. Both issuance premises are unchanged.

### `runFromQ_sound`

[Declaration](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:512). Law (subject `runFromQ`).
**Soundness.** Every instrumented report has a finite oracle-checked path to a position that
justifies its ending. `ReportedQueries` is indexed by that justification and identifies the
reported history exactly: the final position's history, plus the authenticated terminal query for
a request-stopping ending.

- [Instance at line 2205](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:2205): Instance (law `runFromQ_sound`, case `terminalConfig`).
- [Planted at line 2222](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:2222): Planted (law `runFromQ_sound`, case `terminalConfig`, subject `runFromQ`, mutant `fabricatedFinalRun`).

### `runFromQ_complete`

[Declaration](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:626). Law (subject `runFromQ`).
**Completeness.** Exactly `n` units of fuel suffice for an `n`-step oracle-checked execution to
a final configuration; the report carries the path's complete event and query histories.

- [Instance at line 2190](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:2190): Instance (law `runFromQ_complete`, case `terminalConfig`).
- [Planted at line 2197](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:2197): Planted (law `runFromQ_complete`, case `terminalConfig`, subject `runFromQ`, mutant `droppedAnswerRun`).

### `InfiniteExecution.evaluator_prefix`

[Declaration](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:910). Law (subject `runQ`).
Every position of a genuine infinite execution is exactly the
corresponding evaluator prefix, including its events, queries, and residual configuration.

- [Instance at line 2239](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:2239): Instance (law `InfiniteExecution.evaluator_prefix`, case `event_infinite`).
- [Planted at line 2246](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:2246): Planted (law `InfiniteExecution.evaluator_prefix`, case `event_infinite`, subject `runQ`, mutant `forgetfulRunQ`).

### `finite_projected_observation`

[Declaration](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:1203). Law (subject `MatchesProjectedHistory`).
A finite projected history has a matching C6 stream, and every matching stream has the
same finite-divergence observation. Soundness does not depend on choosing the canonical stream.

- [Instance at line 1994](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:1994): Instance (law `finite_projected_observation`, case `erasingAdapter`).
- [Planted at line 2030](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:2030): Planted (law `finite_projected_observation`, case `erasingAdapter`, subject `MatchesProjectedHistory`, mutant `uncheckedHistory`).

### `infinite_projected_observation`

[Declaration](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:1216). Law (subject `MatchesProjectedHistory`).
An unbounded projected history has a matching C6 stream, and every matching stream has
the same infinite trace. Soundness preserves the trace under arbitrary silent-step scheduling.

- [Instance at line 2002](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:2002): Instance (law `infinite_projected_observation`, case `keepingAdapter`).
- [Planted at line 2040](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:2040): Planted (law `infinite_projected_observation`, case `keepingAdapter`, subject `MatchesProjectedHistory`, mutant `uncheckedHistory`).

### `TerminationEvidence.all_allowed_runs`

[Declaration](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:1322). Law (subject `run`).
Every admitted call has a successful lookup and entry and reaches a final outcome. The same
outcome is reported at every fuel budget above the certificate's witness budget.

- [Instance at line 2276](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:2276): Instance (law `TerminationEvidence.all_allowed_runs`, case `terminatingEvidence`).
- [Planted at line 2282](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:2282): Planted (law `TerminationEvidence.all_allowed_runs`, case `terminatingEvidence`, subject `run`, mutant `earlyStopRun`).

## Every exemption left PENDING (86)

None is writer-approved. Each declaration below contains its class and reason in the source.

- [issue_occurrence](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:34): Helper (projection), approved: PENDING.
- [issue_next_occurrence](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:41): Helper (arithmetic), approved: PENDING.
- [answer_deterministic](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:50): Helper (congruence), approved: PENDING.
- [answer_toQueryOracle](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:57): Helper (projection), approved: PENDING.
- [QueryHistoryValid.append](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:63): Helper (arithmetic), approved: PENDING.
- [QueryHistoryValid.nil](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:118): Helper (computation), approved: PENDING.
- [runQ_queryHistoryValid](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:124): Helper (projection), approved: PENDING.
- [occurrenceAt_take_interval](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:131): Helper (arithmetic), approved: PENDING.
- [occurrenceAt_take_no_other](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:157): Helper (arithmetic), approved: PENDING.
- [runFromQ_interleaved_other_sites](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:186): Helper (arithmetic), approved: PENDING.
- [runFromQ_consecutive_same_site](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:203): Helper (arithmetic), approved: PENDING.
- [runFromQ_consecutive_other_site](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:221): Helper (arithmetic), approved: PENDING.
- [Move.toQueryOracle_answer](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:280): Helper (projection), approved: PENDING.
- [runFromQ_request](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:309): Helper (computation), approved: PENDING.
- [runFromQ_queries_from](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:350): Helper (computation), approved: PENDING.
- [runFromQ_queries_prefix](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:384): Helper (computation), approved: PENDING.
- [runFromQ_queries_prefix_le](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:424): Helper (computation), approved: PENDING.
- [runFromQ_query_fuel_independent](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:441): Helper (computation), approved: PENDING.
- [Move.toStep](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:461): Helper (projection), approved: PENDING.
- [Moves.toSteps](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:473): Helper (projection), approved: PENDING.
- [final?_none_of_step](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:487): Helper (computation), approved: PENDING.
- [runFrom_sound](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:597): Helper (projection), approved: PENDING.
- [runFrom_events_from](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:616): Helper (computation), approved: PENDING.
- [runFrom_complete](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:645): Helper (projection), approved: PENDING.
- [runFrom_mono](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:656): Helper (computation), approved: PENDING.
- [runFrom_complete_le](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:703): Helper (computation), approved: PENDING.
- [runFrom_trace_prefix_le](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:718): Helper (computation), approved: PENDING.
- [runFrom_trace_prefix](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:759): Helper (computation), approved: PENDING.
- [runFrom_final_sound](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:766): Helper (computation), approved: PENDING.
- [runFromQ_add_of_outOfFuel](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:783): Helper (computation), approved: PENDING.
- [eq_of_runFromQ_zero_out](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:831): Helper (computation), approved: PENDING.
- [Move.runFromQ_one](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:846): Helper (computation), approved: PENDING.
- [Move.not_final](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:855): Helper (computation), approved: PENDING.
- [move_of_fuel_succ](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:863): Helper (computation), approved: PENDING.
- [InfiniteExecution.not_final](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:903): Helper (projection), approved: PENDING.
- [infinite_iff_fuelLimit](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:940): Representation lemma, approved: PENDING.
- [InfiniteExecution.toFuelLimit_toInfiniteExecution](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:946): Representation lemma, approved: PENDING.
- [FuelLimit.toInfiniteExecution_toFuelLimit](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:952): Representation lemma, approved: PENDING.
- [fuelLimit_states_unique](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:957): Helper (computation), approved: PENDING.
- [FuelLimit.unique](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:978): Helper (computation), approved: PENDING.
- [InfiniteExecution.unique](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:989): Helper (computation), approved: PENDING.
- [Move.events_prefix](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:1000): Helper (projection), approved: PENDING.
- [InfiniteExecution.events_prefix](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:1009): Helper (arithmetic), approved: PENDING.
- [InfiniteExecution.projected_prefix](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:1023): Helper (arithmetic), approved: PENDING.
- [getElem?_eq_of_prefix](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:1032): Helper (arithmetic), approved: PENDING.
- [visiblePrefix_finiteDivergenceSteps](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:1040): Helper (computation), approved: PENDING.
- [visiblePrefix_emitTrace](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:1053): Helper (computation), approved: PENDING.
- [eventually_constant_of_bounded](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:1063): Helper (arithmetic), approved: PENDING.
- [MatchesProjectedHistory.sameObservation](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:1089): Helper (computation), approved: PENDING.
- [finite_projected_canonical](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:1108): Helper (computation), approved: PENDING.
- [infinite_projected_canonical](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:1151): Helper (computation), approved: PENDING.
- [finite_or_infinite_events](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:1230): Helper (arithmetic), approved: PENDING.
- [finite_not_infinite_events](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:1245): Helper (arithmetic), approved: PENDING.
- [silentDivergence_not_infiniteInteraction](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:1254): Helper (arithmetic), approved: PENDING.
- [internal_finite_or_infinite_events](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:1265): Helper (arithmetic), approved: PENDING.
- [not_infinite_of_final](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:1278): Helper (computation), approved: PENDING.
- [not_infinite_of_noAnswer](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:1293): Helper (computation), approved: PENDING.
- [endlesslyBlocked_of_noAnswer](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:1308): Helper (computation), approved: PENDING.
- [TerminationEvidence.excludes_infinite](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:1340): Helper (projection), approved: PENDING.
- [spin_move_01](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:1408): Helper (computation), approved: PENDING.
- [spin_move_12](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:1410): Helper (computation), approved: PENDING.
- [spin_move_23](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:1412): Helper (computation), approved: PENDING.
- [spin_move_34](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:1414): Helper (computation), approved: PENDING.
- [spin_move_40](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:1416): Helper (computation), approved: PENDING.
- [spin_move](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:1420): Helper (computation), approved: PENDING.
- [spin_internal_silent_divergence](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:1443): Helper (computation), approved: PENDING.
- [event_move_01](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:1482): Helper (computation), approved: PENDING.
- [event_move_12](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:1486): Helper (computation), approved: PENDING.
- [event_move_23](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:1490): Helper (computation), approved: PENDING.
- [event_move_40](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:1494): Helper (computation), approved: PENDING.
- [event_move_34](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:1500): Helper (computation), approved: PENDING.
- [event_move](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:1524): Helper (computation), approved: PENDING.
- [erasing_project](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:1565): Helper (computation), approved: PENDING.
- [keeping_project_rawEvents](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:1571): Helper (computation), approved: PENDING.
- [erasingWellFormed](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:1582): Helper (computation), approved: PENDING.
- [erasing_silent_divergence](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:1592): Helper (computation), approved: PENDING.
- [erasing_internal_infinite](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:1597): Helper (computation), approved: PENDING.
- [keeping_infinite_interaction](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:1603): Helper (computation), approved: PENDING.
- [bumpedMove](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:1714): Helper (computation), approved: PENDING.
- [terminalRequest](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:1942): Helper (computation), approved: PENDING.
- [erasedFinite](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:1986): Helper (computation), approved: PENDING.
- [keptInfinite](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:1989): Helper (computation), approved: PENDING.
- [alwaysEmit_not_finite](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:2018): Helper (computation), approved: PENDING.
- [interleaved_execution](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:2125): Helper (arithmetic), approved: PENDING.
- [terminalPath](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:2186): Helper (computation), approved: PENDING.
- [unanswered_no_move](/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/writers/integ/ConcreteSpec/Proofs/Adequacy.lean:2215): Helper (inversion), approved: PENDING.

## QUESTIONS

None.
