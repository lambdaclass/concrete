Gate A round 13, P1: nested controls preserve structured failure state and durable evidence.

Structural cause: control subshells isolated verdict variables and placed regression bundles
under their caller's disposable scratch. Structural owner: the shared spec_diff candidate
lifecycle and control acknowledgment protocol (Concrete/README.md, differential gates).

Reproduction at 477b0048: the new
`Orchestration.test_nested_verifier_extra_mismatch_then_replay_timeout` invokes the production
nested bundle verifier with an emission plant, an additional evaluator mismatch, then an
exact planted replay-timeout diagnostic. Before the fix it returned 3 instead of 1 and the
outer cleanup removed the nested evidence. The fixed test requires exit 1, both exact failure
diagnostics, the exact timeout diagnostic and surviving evidence outside the deleted scratch.
A second control verifies that successful emission verification acknowledges its exact planted
record while an additional evaluator mismatch remains FAIL.

All control bundle destinations were audited: worker-journal, nested bundle verification,
overflow/LCB timeout probes and report-mismatch now store evidence outside scratch. Immutable
snapshot references cross nested executions in a shared ledger; only a completed control's
exact declared records can be acknowledged. Unverified records remain FAIL over INCOMPLETE.
No language decision or roadmap/issue ID allocated.
