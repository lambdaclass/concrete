# N2 round 6 phase B: lead verification, implementation outstanding

Owner: R-0219. Independent reviewers must **do the best long-term thing rather
than cut corners**. The coordinator runs all reviews of record; none was run by
the writer. The supplied phase-B brief at
`/home/unbalancedparen/projects/gates-briefs/N2-fix6b.md` already contains the
following fixed criterion verbatim:

> The exact historical C9-new and C6-current controls (whole replay scopes, not selected roots) must COMPLETE under the unchanged budgets: 2,000,000 heartbeats, projection 4,096, discharge work 20,000, with the measured cost reported for each (exact source/checker SHA, heartbeats, discharge work units, worklist/SCC/query counts, projection visits) and with their historical diagnostics unchanged. If either control does not complete, phase B STOPS: report the measured cut-short point and return the design for revision. Never raise a budget, never add a staged exception, never record a partial completion as a pass. All negative controls keep failing with their exact diagnostics and the F3b repro passes; otherwise phase B also stops.

## Status: NOT MET

The accepted solver has **not been implemented**. This handoff supplies only the
independent CI setup fix and baseline lead reproductions. It does not establish
a design defect, an implementation completion, or a completion-bound failure for
a new solver. No claim that the accepted design was experimentally rejected is
made. There is no measured new-solver cut-short point to report.

| required whole replay | source SHA | checker | heartbeats | discharge work | worklist/SCC/query counts | projection visits | completion |
| --- | --- | --- | --- | --- | --- | --- | --- |
| C9-new | 3e22e6417cd4c84d658fd7d4601d11ad0b160454 | not implemented | not measured | not measured | not measured | not measured | NOT ESTABLISHED |
| C6-current | 6477c65a79aa715c60b32a4b025de204533c65b7 | not implemented | not measured | not measured | not measured | not measured | NOT ESTABLISHED |

Whole replay measurements were not run in this handoff. The phase-A census and
prior incomplete runs are not substituted for those measurements. All budgets
and analysis algorithms remain unchanged; there is no staged acceptance exception.

## Leads and focused evidence

- F1 **NOT REPRODUCED** for the named shapes with the baseline consumer at
  25907fb41b1e1ec0417507d2f81d6716f3a5bf7d. The existing 26 discharge controls
  passed, including False.elim, absurd, nomatch, Empty/PEmpty, supplied joint
  contradictions, context-cache isolation, and the complete F3b repro. Six more
  controls passed: Subtype/PSigma/PProd proof suppliers each require the exact
  named instance-false-discharge diagnostic; finite nonempty collection proofs
  using decide, simp, and explicit cases each require zero findings. The accepted
  treatment of unsupplied impossible lambda domains remains unchanged.
- F2 **FIXED** by 852664bf78490016a9de5e74cf7a751b41a66ba9. One local composite
  action installs Nix and pinned Lean, preserving the PR install retry. All three
  N2 entry points (PR, manual, daily) use it before entering the gates shell.
  Workflow/action YAML parsing and setup ordering checks passed. Actual hosted
  action execution was not run locally.
- 51eca9ecf8e3e39dc4328ae85774cbb37ef46216 adds the six controls to the normal
  inventory and the focused discharge runner, with the recorded baseline results.

See [the focused transcript](audits/N2-round6b-leads.txt). Checks used the
inherited cached Nix development PATH and Lean 4.34.1. Python syntax and
`git diff --check` also passed. No full suite, whole-tree build, push, review
tooling, ConcreteSpec change, ROADMAP change, ID allocation or OD decision.

The remaining structural work is exactly the accepted
[N2 discharge solver](N2-discharge-solver.md): symbolic guarded transfer graphs,
actual supplied provenance, application-specific queries, checked branch
certificates, SCC safety invariants, and bounded complete historical validation.
The owning README still records this unresolved R-0219 completion blocker.

## Continued implementation: representation only

Commit c64d542d5 adds the first implementation slice: hash-consed expression
identities, a separate origin DAG, typed input substitutions, guard and
certificate records, application-specific query keys, transfer nodes, raw
declaration templates, and a shared worklist state with the unchanged 20,000
allowance. The graph compiler retains formal expressions instead of applying
actual arguments to a declaration body. Graphs are scoped to one immutable
environment and Instance.

This slice does **not** implement the solver, SCC invariants, certificate
reconstruction, query interpretation, or consumer switch. The existing
`falseDischarge` remains the sole active implementation. These missing pieces
are implementation work, not an experimentally established flaw in the accepted
design. This continuation does not satisfy phase B and does not invoke its
measurement-based stop condition. No historical solver measurements were run;
the cost table above remains unestablished. No budget was changed.

Focused validation: all 32 discharge controls passed, including exact negative
diagnostics and the full F3b repro, with the new representation disconnected
from verdicts. The bounded runner recovered after its initial 180-second
wall-clock allowance, completing under its 360-second retry; the internal
analysis budgets were unchanged. After the graph-construction additions, the
consumer elaborated successfully in a separate bounded compile. `git diff
--check` passed. These checks establish representation elaboration and retained
baseline behavior only; they supply no new-solver soundness or completion evidence.
