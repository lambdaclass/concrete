# R-0004 Execution History

Status: historical

This document is a non-authoritative chronology of R-0004. Current work and sequencing live in
[ROADMAP.md](../../ROADMAP.md); shipped user-visible claims live in
[CLAIMS_TODAY.md](../verification/CLAIMS_TODAY.md); completed milestones live in
[CHANGELOG.md](../../CHANGELOG.md).

## Closure

R-0004 closed on 2026-08-20. Its authorizing artifact came from the protected serial run at
`a839d519`:

```text
completed=1  mode=closure  jobs=1
discovered=205  executed=205  passed=205  failed=0
```

The run reported no integrity refusal, finished on a clean tree, and matched the pushed HEAD. The
closure commit was `0c2ac6b1`.

The closed product boundary is: a friendly `proved` verdict requires a current exact subject,
complete dependency root, explicit trust/assumptions/environment, and a receipt that only successful
kernel replay can mint. Stored receipt bytes are untrusted input and are compared with facts
recomputed from the current program.

## Package chronology

### Packages 1 and 2 — exact authority

- Replaced name-keyed evidence joins with exact `DefinitionIdentity` selection.
- Made correspondence and computable dependency roots part of the production verdict.
- Preserved contract, body, trusted and missing dependency-edge kinds.
- Added transitive trust propagation and named refusal sets.
- Fixed stale/dead controls whose failure modes had been indistinguishable from success.
- Deferred exact contract witnesses to typed contracts because the corpus contained zero contract
  edges; the current implementation-bound witness is conservative and fail-safe.

### Package 3 — replay, receipts, migration and attack

- Extracted one typed replay producer returning `Except ReplayRefusal ReplayResult`.
- Made `SuccessfulReplay` the only authority capable of minting a receipt.
- Shipped issuance, storage and fresh consumption; `StoredReceipt` has no minting path.
- Bound subject, theorem artifact, dependency root, trust boundaries and replay-derived Lean import
  source digests.
- Activated V2 subject identity atomically and preserved the corpus census without manufactured
  values.
- Demonstrated cross-checkout and path-independent receipt consumption.
- Permanently retained Slice 8 attacks, including the independent theorem-retargeting finding that
  a kernel-valid theorem need not be about the selected subject.
- Kept the ninth table fail-closed and authority-free pending a narrow mutable-state model.

## Corrections that changed the meaning of “green”

R-0004 repeatedly found checks whose failure looked like success:

- renamed grep targets made tripwires inert;
- `grep -q` behind a large producer raced under `pipefail` via SIGPIPE;
- an absent suite summary was read as a clean result;
- gates existed but were unreachable from CI;
- a syntax error sat after an unconditional exit, so dead assertions never parsed;
- a “serial” run still fanned out internally;
- killed mutation processes could leave tracked compiler sources modified;
- an empty discovered gate set could report `PASS=0 FAIL=0` and exit successfully.

The closure runner therefore gained enforced serial mode, complete discovery/execution accounting,
machine-readable completion, preserved failing output, start/end integrity reconciliation, and
disposable mutation workspaces.

## Named post-closure limitations

These were deliberately not hidden inside R-0004:

- package identity over-binds dependency/content facts; the measured std edit moves 7 of 21 package
  identities even though the failure mode is demotion rather than false authority;
- build identity names the source build, not post-build executable bytes;
- exact contract identity waits for the typed-contract substrate;
- the three ninth-table links remain unsupported and unable to provide authority;
- `ProofCache` does not exist and remains a performance-pulled optimization.

## Post-closure mutation qualification

The mutation campaign is stronger assurance work, not a second R-0004 closure definition. The
harness shipped at `898d9a7b` requires a green pristine baseline, pristine build, exact unique
anchor, authenticated gate completion, and causal restore/rebuild/reapply confirmation.

An earlier attempt stopped before its baseline and retained only
`run_did_not_reach_reconciliation`. The complete diagnostic campaign against `898d9a7b` subsequently
executed and reported all 81 families: 73 were killed by their named gates with red-green-red causal
confirmation, 6 experiments were invalid, 2 survived, and none failed to apply. The preserved files
are `mutation-campaign-summary.898d9a7b` and `campaign_full.898d9a7b.log`.

The artifact's `completed=0` is a historical schema defect, not an assertion that the run stopped:
the old producer made zero unresolved families a condition of completion. It also recorded
`head_changed` because the control checkout moved to documentation commit `89a2bcb1` during the run;
the immutable tested workspace remained bound to `898d9a7b`. The dispositions are therefore useful
diagnosis but the campaign is not qualifying evidence.

Commit `98dee5e3` split the facts: `completed` now means every selected unit reported and the driver
reached reconciliation; `qualified` additionally requires a full campaign, intact integrity and
every family killed by its declared attributable route. It also records exact disposition counters
and retains failure logs. A clean pushed-HEAD campaign still needs 81/81 kills, zero invalid,
survived or could-not-apply results, `completed=1`, and `qualified=1`.

## Where the detailed record remains

The original trust-boundary tables, eight-slice plan, rejected designs, intermediate denominators,
and dated corrections remain under `Task R-0004` in the roadmap for commit-level archaeology. They
are explicitly historical and cannot override the current execution state. Future compaction may
move that material here verbatim once all inbound links and generated references are inventoried.
