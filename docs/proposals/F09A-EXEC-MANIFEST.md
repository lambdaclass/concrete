# F09a: complete E1 command-group manifest

Source: `origin/wip/spec-integration-head` at `aa44140a3`,
`ConcreteSpec/Proofs/Exec.lean`; local export supplied by the coordinator.
Selection is by complete documented Lean commands and their dependency closure,
not literal line slicing. All commands retain namespace `ConcreteSpec.Core.Exec`;
`O` and `V` remain implicit parameters. The request helper remains private.
The source's unrelated namespace openings and section commentary do not migrate.
No command from the excluded assumed-yes branch is used.

## Root commands and transitive support

The table is the entire E1 root population plus its two completion helpers.
Every declaration is in [the proof module](../../ConcreteSpec/Proofs/Exec.lean).
All historical `APPR-*` suffixes are retired, not evidence. Composition statements
remain representation lemmas; unfolding statements remain Helpers. Law labels
follow the [accepted vocabulary](../decisions.md#decided-2026-11-13-proof-labels).

| Source and destination declaration | Tag | Recorded authority | Disposition |
|---|---|---|---|
| `Steps.single` | Representation lemma | [authority](../decisions.md#decided-2026-11-05-core) | Retain complete command |
| `Steps.trans` | Representation lemma | [authority](../decisions.md#decided-2026-11-05-core) | Retain complete command |
| `steps_next` | Representation lemma | [authority](../decisions.md#decided-2026-11-05-core) | Retain complete command |
| `steps_request_inv` | Representation lemma | [authority](../decisions.md#decided-2026-11-05-core) | Retain complete command |
| `steps_request` | Law | [authority](../decisions.md#foundation-q16) | Retain complete command |
| `demo_log_completes` | Helper | [authority](../decisions.md#decided-2026-11-05-core) | Repair function-end boundary |
| `demo_bump_completes` | Helper | [authority](../decisions.md#decided-2026-11-05-core) | Repair function-end boundary |
| `step_seq` | Helper | [authority](../decisions.md#decided-2026-11-05-core) | Retain complete command |
| `step_done_seq` | Helper | [authority](../decisions.md#decided-2026-11-05-core) | Retain complete command |
| `step_exit_seq` | Helper | [authority](../decisions.md#decided-2026-11-05-core) | Retain complete command |
| `step_block` | Helper | [authority](../decisions.md#decided-2026-11-05-core) | Retain complete command |
| `lookup_restrict` | Law | [authority](../decisions.md#od2) | Retain complete command |
| `restrict_dom_drops` | Law | [authority](../decisions.md#od2) | Retain complete command |
| `register_scope` | Helper | [authority](../decisions.md#decided-2026-11-05-core) | Retain complete command |
| `register_seq` | Helper | [authority](../decisions.md#decided-2026-11-05-core) | Retain complete command |
| `two_defers_register` | Law | [authority](../decisions.md#decided-2026-11-05-core) | Retain complete command |
| `step_ret` | Law | [authority](../decisions.md#od2) | Retain complete command |
| `step_break_loop` | Helper | [authority](../decisions.md#decided-2026-11-05-core) | Retain complete command |
| `step_continue_loop` | Helper | [authority](../decisions.md#decided-2026-11-05-core) | Retain complete command |
| `step_return_caller` | Law | [authority](../decisions.md#decided-2026-11-05-core) | Repair function-end boundary |
| `exit_scope_next` | Law | [authority](../decisions.md#decided-2026-11-05-core) | Retain complete command |
| `done_scope_next` | Law | [authority](../decisions.md#decided-2026-11-05-core) | Retain complete command |
| `exit_scope_runs_defers` | Law | [authority](../decisions.md#decided-2026-11-05-core) | Retain complete command |
| `done_scope_runs_defers` | Law | [authority](../decisions.md#decided-2026-11-05-core) | Retain complete command |
| `unwind_runs_plan` | Law | [authority](../decisions.md#decided-2026-11-05-core) | Retain complete command |
| `plan_scopes` | Helper | [authority](../decisions.md#decided-2026-11-05-core) | Retain complete command |
| `return_reaches_caller` | Law | [authority](../decisions.md#decided-2026-11-05-core) | Repair function-end boundary |
| `break_reaches_loop` | Law | [authority](../decisions.md#decided-2026-11-05-core) | Retain complete command |
| `continue_reaches_loop` | Law | [authority](../decisions.md#decided-2026-11-05-core) | Retain complete command |

Cleanup/return laws additionally require admitted lifecycle transitions under the
[lifecycle refusal answer](../decisions.md#decided-2026-11-13-lifecycle-refusal).
`return_reaches_caller` explicitly includes `functionEnd` in its starting frames;
its prefix cannot cross that boundary because `Passes` excludes it. The caller
step binds only; it must not repeat leave. Both completion helpers take arbitrary
caller locals, stores and stacks and cross the same boundary on ordinary calls.
No theorem is stated against the removed driver or a historical execution report.

## Corresponding CoreEvidence command closure

For every Law in the table, the source's complete positive anonymous examples
are replaced by named `<law>_instance` commands in `CoreEvidence`. The two lookup
examples are combined in one conjunction, preserving both branches. Concrete
return applications gain the required `functionEnd` frame. Positive cleanup,
return, break and continue examples retain actual deferred log/bump completion.
The move-return example additionally records the pre-cleanup instrumentation.

Every corresponding anonymous negative example is replaced by named
`<law>_planted`. `lookup_restrict` retains both drop-all and keep-all domain-filter mutations;
`restrict_dom_drops` retains keep-all. The remaining source controls depended on
`ReflOnlySteps`, `stuckStep` or `Refusal.StuckStep`: these are retired for this
piece because they disable unrelated rules too. The replacements delete exactly
one dispatcher rule, leaving operands and the original lifecycle and completion
premises unchanged. `MissingRule`, `hasRegistered`, `missingStep`, `MutStep` and
`MutSteps` are Planted; `missing_blocked`, `missing_trace` and `missing_next` are
Helpers. This relation is proof-only and has no oracle/fuel recursion.

Support `d1`, `d2`, `afterBumpLog`, `logAns` is retained as concrete fixtures.
`passes_single_scope` is retained as a private Helper. `realCalls` and
`realCleanup` factor repeated actual completion witnesses. The source's other
CoreEvidence mutants (trap, argument, planWith, destruction) belong to later
groups and are not dependencies of E1. No source theorem or anonymous E1 example
is silently omitted; each family above includes its complete applications and
controls. No historical approvals qualify the replacement controls.

## Holds and scope

No E1 law requires an unresolved owner choice. Later E2–E4 proofs stay in their
planned pieces. E1 holds no additional language rule and creates no issue ID.
Known policy holds remain: path policy ISS-0068, Boolean encoding ISS-0069,
pointer bytes ISS-0070, SPARK character preconditions ISS-0071, wiping ISS-0072,
and startup/ABI ISS-0073. U+FEFF scope, bare CR, constant restrictions, struct
layout/whole-pointer representation and queued partial decisions are outside
E1 and remain unencoded; no new finding about them is asserted by this piece.
The demo integer truth test remains an instance interface only.
Initial-entry reporting and external-effect rollback details explicitly left open
by the lifecycle record acquire no new theorem or policy here.
