# C7 security round 2

Merged the accounting round-2 base without changing its validator, controls or accounting law.
Retained ISS-0082 and ISS-0083. Security authority now cites the recorded owner entry
in docs/decisions.md#foundation-q1 for the C7 interface/law boundary and
docs/decisions.md#decided-project-infoflow-2026-11-06 for NI, CT and public lengths.
Removed memory-accounting citations from security declarations; inherited approvals supply no authority.

The specification root explicitly imports Leak, ReleaseCalls and LeakData after their dependencies.
The proof root explicitly imports all three security proof modules. Compound extraction mutants
omit one child traversal each, preserving the other child; their per-case Planted examples now
select distinct left/right faults. Added a two-edge positive reachability Instance and a Planted
relation that retains reflexivity/direct edges but loses chaining. Instrumentation erasure has
positive Instances in both directions. Registry execution fixtures are named, and security registry
and call-extraction proofs have no lines longer than 120 characters.

All sixteen carried Laws and their authorities, classification policy and excluded scope remain
listed in `2026-11-15-spec-f26b-security.md`. New controls are Instance or Planted, not additional
Law evidence. No held rule is encoded: ISS-0064/0065 (source scope and partial recovery), ISS-0068
(path policy), ISS-0069 (Boolean encoding), ISS-0070 (pointer bytes/representation and struct layout),
ISS-0071 (SPARK preconditions), ISS-0072 (wiping), ISS-0073 (startup/ABI), ISS-0080 (constant
extensions) and ISS-0083 (unestablished recovered currentness) retain their owners.
The assumed-yes branch and proposals supply no independent authority.

The axiom inventory is regenerated from the combined built environment, retaining the accounting
and security theorem populations exactly. The harness owns the merge commit and publication.
