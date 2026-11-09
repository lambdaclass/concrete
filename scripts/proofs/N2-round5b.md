# N2 round 5b investigation — NOT FIXED

For the coordinator and independent reviewers: **do the best long-term thing
rather than cut corners**. The requested structural summary implementation was
not completed. No code from the unsuccessful experiment is retained.

## Disposition

Round-5 item 4 remains **NOT FIXED**. The exact historical C9 control still
exhausts the unchanged 2,000,000-heartbeat bound. Replay and promotion remain
blocked. This handoff records an unsuccessful investigation, not a completed
repair or an accepted staging substitute.

The checker is byte-for-byte the checker at ec9b5350dc8f4faaa29b426fe76030c3bbc7a683:
SHA-256 `c479e1a9bad3c18bcdb0d93ec85e151dd3932d924fc6044e9e9e793d529c2c3a`.
Every existing positive, negative, historical fixture, projection bound and
analysis budget is unchanged. No ConcreteSpec or ROADMAP edits, OD decision,
ISS/R allocation, push, rewrite, review tooling or full suite was performed.

## Rejected experiment

An uncommitted implementation retained declaration binder templates, computed
recursive conservative effect summaries with a bounded reverse worklist, and
instantiated actual arguments before residual context-sensitive traversal. Clean
recursive components required a completed worklist. Effectful components retained
the original traversal. No alpha-closure verdict cache was used.

A diagnostic probe certified Nat.le_trans without repeated instantiation:
247 work units, 26 declaration summaries, 221 template nodes. This observation
was insufficient to establish the requested structural repair. The complete C9
control still exhausted 2,000,000 heartbeats. Its last diagnostic sample recorded
at least 1,400 application visits, 6,029 shared work units and 210 summaries;
these are lower bounds from an instrumented, unfinished run, not completed costs.
The trace included repeated congrArg and well-founded-fix machinery under growing
hypothetical contexts.

More decisively, the unchanged focused controls returned **25 PASS, 1 FAIL**:
`reachable-joint-helper-negative` lost its required
`FAIL instance-false-discharge` on the instance and instead reported only
`FAIL missing-planted ConcreteSpecControl.growth bad/ConcreteSpecControl.S`.
The expected diagnostic remained unchanged. This was an unacceptable false pass
in the experiment, so the entire implementation was discarded before committing.
The subsequent constructor-field variation also failed C9 completion and was
discarded. None of these experiment results certifies the restored checker.

The outstanding design is still the recursive supplier-provenance and
branch-constraint summary solver specified in [README.md](README.md), owned by
R-0219. A coarse clean/effectful effect summary with residual unfolding did not
satisfy it. No new containment or migration exception was introduced.

## C9 before/after figures

The unmodified pre-round checker and the retained final checker are identical:
both exact controls exhaust **2,000,000 heartbeats**, with no completed work
figure. A separate temporary diagnostic consumer incremented an IO.Ref at each
uncached discharge visit and sampled cumulative user heartbeats every 100 visits.
It changed no budget, key, branch, supplier or expected diagnostic. Its last
sample was **13,500 visits at 1,987,088 heartbeats**, followed by the same
2,000,000-heartbeat INCOMPLETE at isDefEq. These are unfinished-run lower bounds,
not a completed C9 cost. See [work diagnostic](audits/N2-round5b-c9-work-diagnostic.txt).
The instrumentation was confined to a temporary consumer and is not committed
as checker code. The rejected experiment's work units include summary work and
are not directly comparable to these original traversal visits.

## Fresh validation of the restored checker

Results and command identity are recorded in the round-5b audit files. These are
focused writer checks, not complete c1/c2 audits or a historical replay PASS.

- Exact C9: **FAIL control, INCOMPLETE checker**, unchanged heartbeat exhaustion
  at isDefEq; see [restored C9](audits/N2-round5b-c9-restored.txt).
- Focused discharge controls: **26 PASS, 0 FAIL**, including the complete F3b
  repro and reachable-joint-helper-negative; see
  [restored controls](audits/N2-round5b-controls-restored.txt).
- C2 projection: **PASS**, 49 visits at both 64 and 4,096; see
  [projection checks](audits/N2-round5b-c2-projection-restored.txt).
- c1 exact-source probes: **PASS**, both logSegment and boundary_of_performed
  retain falseDischarge=false; see [c1 probes](audits/N2-round5b-c1-helpers-restored.txt).
- c2 exact-source probe: **PASS**, acyclic_of_dependencyOrder retains
  falseDischarge=false; see [c2 probe](audits/N2-round5b-c2-helpers-restored.txt).
- Historical fixture provenance: **PASS**, all 10 excerpts byte-for-byte.
- Documentation links: **PASS**, 12 controls, 1,131 links, 894 anchors.
- git diff --check: **PASS**.

The checks used the inherited cached Nix environment, including pinned Lean
4.34.1. Attempts to create a fresh offline default shell and gates shell failed
on unavailable Nix store dependencies; no network fetch or heavy Lean build was
performed. All commands were externally bounded.

## Coordinator server requirements

The final checker still has the known C9 blocker. Do not interpret absent findings
in an unfinished report as clean evidence. The required complete checks remain
those in [the round-5 handoff](N2-round5.md#required-coordinator-server-runs):

1. Full controls: `nix develop -c python3 scripts/proofs/vacuity_driver.py --controls`.
2. Exact historical replay: `nix develop -c python3 scripts/proofs/vacuity_fixtures.py --replay NEW_DIR`.
3. Fresh exact c1 ccca4abaa and c2 9cb0ef7fe archives, each audited with
   `vacuity_driver.py --source ARCHIVE_DIR` and all seventeen scope flags:
   Authority, Basics, Borrow, Brand, Checker, Cleanup, Const, Examples, Exec,
   Hybrid, Memory, Observe2, Observe, Rep, Scalar, Slice0, Slice1, each prefixed
   `ConcreteSpec.Proofs.`.

Run on concrete-ci-2 under the coordinator heavy-slot policy and external bounds;
record the exact final commit and source hashes, costs, complete reports and
terminal verdicts. Promotion requires replay PASS, zero instance-false-discharge
findings on c1 and c2, and no INCOMPLETE. Those requirements are not established
by this round. Server results queued at ec9b5350d were not relayed during the
investigation.
