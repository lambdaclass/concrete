# Scheduled tiers and freshness (R-0676)

Authority: [R-0676](../../../ROADMAP.md#r-0676) and the
[tier decision](../../../docs/decisions.md#decided-2026-11-08-gate-tiers), including
“Tier and scheduled-run rules” and “Design and build order”. These anchors are
currently supplied by `origin/wip/record-chain-fix`, not this branch's main base.
The scheduling section of `gates-briefs/prop-codex2.md` is the implementation brief.
No language or spec decision is made here.

## Explicit integration dependencies — do not enable as qualified yet

This branch supplies the execution/record/observation protocol, not the campaigns.
It depends on ISS-0029 (`origin/wip/repro-ci2`, 688e10a86421feb48ab4f26f2b4dd2955a18aff4)
for `scripts/checks/lib/repro_manifest_compare.py:read_inventory` and that module's
single `ci2_runners.txt` binding. It imports the parser rather than copying its
inventory or using a runner-name pattern. Repository/runner registration remains
the trust boundary described in that branch's `docs/SELF_HOSTED_RUNNERS.md`.
The referenced tip has no reusable exact-SHA dispatch helper: selection is a
main-only schedule-specific entry point; it does not replace the repro CI leg.
The coordinator must reconcile X4's shared dispatch interface with ISS-0029 before
qualification; a second independent trusted-runner implementation is forbidden.

B/N5 (`origin/wip/gates-manifest`, c2245c99ab7e9255e6a25898386804d6621f2bb8)
provides `tests/manifest.tsv`, a **fixture** inventory. It does not declare gate
tiers, commands or windows. No fixture list is copied here and no campaign list
is invented. The single source of gate selection must be G1 (R-0674), with G2/G3
(R-0675) evidence/cost contracts. Until that producer lands, the executor records
INCOMPLETE, and the observer cannot report PASS. The proposed adapter boundary is:

```
python3 scripts/checks/lib/gate_manifest.py scheduled --tier daily|weekly
```

It emits one JSON array of rows with `id`, `tier`, `command` (argv),
`budget_seconds`, `tools` (consumed identities), `seed`, and `controls`.
Each control has `id`, `command` (argv), `exit_code` (nonzero) and `diagnostic`
(the complete, exact combined stdout/stderr after stripping surrounding whitespace).
G1 must validate completeness, capability availability and tier placement before
export; the scheduler refuses empty tiers and absent controls. This is an adapter
contract awaiting agreement with G1, **not a claim that its exporter exists**.
The registry must also own freshness windows; the protocol's current 36-hour daily
and eight-day weekly recovery windows are initial operational defaults requiring
integration into that registry before qualification. Prior failed attempts remain
visible; recovery never rewrites their occurrence.

The executor also requires the root `scripts/checks/lib/heavy_slots.sh` interface
`heavy_slot_acquire` / `heavy_slot_release`. The frozen legacy helper is not run.
The root helper must coordinate two slots **across runner Unix users and with
landing/repro/manual work**, not merely within a checkout or user home. Its shared
directory and permissions must be provisioned by the coordinator if absent; this
branch changes no host settings. Until available, execution is INCOMPLETE. R-0676
owns this integration; do not substitute a private scheduler semaphore.

## Execution and evidence

Only schedule and main-ref manual dispatch reach the host-labelled worker.
A GitHub-hosted selector resolves main once; replay requires a full 40-digit SHA
that is an ancestor of that resolved main. No PR or arbitrary ref is selected.
Trusted main supplies the controller; the selected revision runs in a fresh
`mktemp` private clone under Nix, the shared heavy slot and `bounded_run`.
The controller revision is recorded separately from the selected gate revision.
Commands, controls, consumed tool identities, seeds, wall duration, exit status and
logs are indexed per attempt. Future raw samples and minimized reproducers belong
in the result-index directory alongside logs and must be registered by G1's schema.

The note refs are `refs/notes/concrete-gates-daily` and
`refs/notes/concrete-gates-weekly`. Each exact-SHA note is JSON Lines with immutable
`run_id/run_attempt` identities. Publication fetches current history, rejects a
duplicate attempt, appends, and uses ordinary fast-forward pushes with remote SHA
readback. Concurrent publication retries from the newly fetched history, never a
force push. Only the generated workflow publishes; local development does not push.
A note retains complete structured metadata beyond artifact retention. Artifact
upload runs even after failure, and publication requires the upload's artifact URL.
Logs expire after 90 days under current GitHub retention: **long-term log/sample
archival is still required by R-0676**, with a coordinator-selected durable store.
No secret or storage setting is invented here.

PASS requires all gates and their exact-diagnostic controls. A failure or unavailable
observation remains FAIL or INCOMPLETE. Cancelled/skipped/queued-too-long/absent
attempts are MISSING; they never become PASS. A worker killed before writing a note
is an absent MISSING observation. Recording that cancellation's occurrence in notes,
including a worker that never starts, still needs a GitHub-hosted completion
collector using Actions' job provenance (R-0676); worker-owned metadata alone cannot
attest jobs that never ran. No successful older note at another SHA qualifies main.

The observer runs every four hours on `ubuntu-24.04` and is called from normal CI.
It reads main and note refs independently of the host and exits nonzero for
STALE/MISSING/FAIL/INCOMPLETE, reporting each tier. It is not added to
`required-checks.txt`: defects block dependent work, not every unrelated landing.
If the watchdog schedule is missed, normal CI invokes the same observation.
Explicit observer-occurrence history and missed-observer controls remain R-0676
integration work, rather than treating a current observation as proof of past ones.

## Focused verification

Inside `nix develop .#gates`:

```
bash scripts/checks/lib/bounded.sh 20 python3 scripts/checks/scheduled/test_protocol.py
bash scripts/checks/lib/bounded.sh 30 bash scripts/checks/workflow_yaml.sh
```

Controls assert full diagnostics for stale, absent, wrong-SHA, cancelled, replay
collision, forged host, missing artifact/control and fail-then-pass disagreement.
These are protocol controls, not evidence that actual campaign/collector integration
has run. Qualification requires coordinator CI2 checks after the dependencies above
land; do not run heavy campaigns locally or weaken campaigns to make them cheap.
