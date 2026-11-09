# N2 phase-B implementation brief

Owner: R-0219. Status: NOT MET; implementation incomplete.

## PHASE B EXIT CRITERION

The exact historical C9-new and C6-current controls (whole replay scopes, not selected roots) must COMPLETE under the unchanged budgets: 2,000,000 heartbeats, projection 4,096, discharge work 20,000, with the measured cost reported for each (exact source/checker SHA, heartbeats, discharge work units, worklist/SCC/query counts, projection visits) and with their historical diagnostics unchanged. If either control does not complete, phase B STOPS: report the measured cut-short point and return the design for revision. Never raise a budget, never add a staged exception, never record a partial completion as a pass. All negative controls keep failing with their exact diagnostics and the F3b repro passes; otherwise phase B also stops.

## Implementation state

The accepted design remains [N2-discharge-solver.md](N2-discharge-solver.md).
The representation slice is committed at c64d542d5. The solver and consumer
switch are unfinished. No new-solver whole-scope cost has been measured and no
measured cut-short point is claimed. This is not a completed
phase-B outcome. See [the implementation record](N2-round6b.md).

Slice 2 stopped at the certificate dependency-validation specification gap
recorded in [the precise counterexample](N2-slice2-certificate-blocker.md).
Kernel checking and excluding supplied local assumptions alone do not validate
global supplier dependencies of a context refutation. The note's soundness
requirement forbids such dependencies; its constructive validation protocol
needs clarification. No callable solver or consumer switch was added, and no
new-solver cost or measured budget exhaustion is claimed.
