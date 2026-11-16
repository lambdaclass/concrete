### F16 round 4: output guarantees and non-vacuous call progress

Strengthen the downward Law's output to retain the original root, append the exact
requested projection and identify the parent relationship. Strengthen adoption's
output with the child's callee activation, the exact transfer table (preserving
root, projection, mode and ancestry), and preservation of every other caller
loan; shared/mutable and same-root parent/ancestor cases remain admitted.

Add a companion return-progress Law for both conditional restoration Laws. Its
premises describe a newest-first, callee-closed loan graph, discharged linear
locals and no surviving caller loan into callee storage. A list-induction Helper
proves arbitrary finite unwinding succeeds; progress is not a premise. Reuse O1's
removed-loan Helper rather than copy its proof. Add argument-time plain-lending
progress and conflicting-parent suspension for both modes, with arbitrary pending
arguments, and an exact empty-pending call roundtrip Law preserving caller state
apart from advancing loan/activation supplies.

Kernel-checked Planted controls reject retained caller activation, unrelated-loan
removal, discarded projection suffix, relending-return refusal, reversed local
unwinding and mutable-argument refusal. Four additional one-rule attempts cover future activation, replacement of the
parent path, shared-return refusal, and clearing the projection during adoption.
The last survived the intermediate outputs and motivated the exact transfer table;
all four are now rejected.
The mutable-argument control also rejects the roundtrip composition. Positive
return witnesses include local descendants and shared call arguments.

Complete execution control metadata and wrap long lines. The O3 inventory is
18 Law, 14 Helper, 36 Instance, 51 Planted. Authorities remain the approved C5
boundary, Core Q3.2–Q3.4, recovered D3 C5 Q1–Q2, OD2/OD23 and lifecycle refusal;
no owner decision is chosen and no new issue identifier is allocated.
