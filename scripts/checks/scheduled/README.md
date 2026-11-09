# Scheduled tiers and freshness (R-0676)

Authority: [R-0676](https://github.com/unbalancedparentheses/concrete2/blob/81cfa508683fde7d2363284053dbe4405dd34ca3/ROADMAP.md#r-0676) and the
[tier decision](https://github.com/unbalancedparentheses/concrete2/blob/81cfa508683fde7d2363284053dbe4405dd34ca3/docs/decisions.md#decided-2026-11-08-gate-tiers), including
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
visible; recovery never rewrites their occurrence. The latest fresh attempt recovers
operational MISSING/INCOMPLETE on its SHA; FAIL remains permanent. Newer main
coverage recovers older operational gaps, while malformed evidence always surfaces.

The executor uses `scripts/checks/lib/heavy_slots.sh`, the shared Linux host-wide
`heavy_slot_acquire` / `heavy_slot_release` API. Landing, reproducibility and manual
heavy work must source this same helper; no private pools are permitted. Acquisition
is nonblocking: two occupied slots report INCOMPLETE, leaving the coordinator to
queue a new attempt. Nested acquisition is refused. File descriptors hold locks for
the process lifetime, so cancellation releases them without deleting lock files.

The coordinator provisions `/var/lib/concrete/heavy-slots` once: a root-owned,
non-writable directory containing exactly the persistent regular files `slot-1` and
`slot-2`, owned by root and group-writable by the shared runner group (mode 0660).
Every runner Unix user and manual heavy-work user must belong to that group. Never
unlink/replace these files while workers run. `CONCRETE_HEAVY_SLOT_DIR` is a host
configuration override, shared by **all** heavy-work entry points on that host,
not a per-checkout escape hatch. Missing provisioning, inaccessible files, or an
occupied pool produces an exact INCOMPLETE diagnostic; the helper creates nothing.
This branch makes no host settings changes. R-0676 owns adopting this helper in
ISS-0029 and the landing host entry points; the coordinator must verify that
integration and provision the pool before live scheduled qualification.

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
`run_id/run_attempt` identities. Publication fetches current history and appends under
`--force-with-lease=<ref>:<fetched-sha>` compare-and-swap. An identical existing
attempt is an idempotent retry; a different payload at that identity is rejected
with `replay would overwrite attempt`. Remote readback handles a dropped response
without losing a successful update. No local development command publishes notes.
A note retains structured metadata beyond artifact retention. Metadata has its own
small artifact so collection does not download a potentially enormous log archive.
Missing log uploads make an otherwise successful execution INCOMPLETE.
Logs expire after 90 days under current GitHub retention: **long-term log/sample
archival is still required by R-0676**, with a coordinator-selected durable store.
No secret or storage setting is invented here.

PASS requires all gates and their exact-diagnostic controls. Status/conclusion and
gate evidence agree in both directions: gate FAIL takes precedence over INCOMPLETE;
PASS needs nonempty passing gates and controls; INCOMPLETE needs incomplete evidence.
Cancelled/skipped/queued-too-long/absent attempts are MISSING, allowing empty gates,
no artifact and no assigned worker. A later PASS never erases a FAIL on that SHA,
even after the freshness window expires. A late completion cannot overwrite an
already recorded overdue attempt; replay uses a new Actions run attempt.

Workers have read-only contents tokens, with checkout credential persistence off.
They never publish. `gate_publish.yml` runs on GitHub-hosted runners after workflow
completion (including whole-workflow cancellation), and hourly for queued/unstarted
attempts older than 26 hours. It checks out only the trusted publisher's default-branch
revision, never the selected historical SHA, and never executes artifact contents.
It obtains job status/conclusion and runner name from Actions' attempt-specific
Jobs API, then applies ISS-0029's inventory for the observing repository. Worker
runner/repository assertions cannot establish provenance. Selection comes from the
hosted selector's Actions job log, with one exact `SCHEDULE_SELECTION SHA TIER`
diagnostic; no worker-supplied selection qualifies. If selection never completed,
the trusted workflow run title identifies the cron tier on the workflow event SHA;
rejected manual selections never annotate main. Active execution is never treated
as an overdue queue. Queued observations use observation time; a recorded overdue
attempt remains immutable and subsequent collection is idempotent. Completed worker
executions without an index are INCOMPLETE and partial logs remain uploadable. Collector
API errors produce a failing INCOMPLETE collector observation; absent inventory
still permits nonqualifying MISSING/dependency-INCOMPLETE publication, while missing
notes remain visible to the independent observer. HTTP redirects strip credentials.
The publisher reserves time for setup and cleanup: its two 300-second bounded
collection attempts fit the 20-minute job, and exhausted attempts explicitly report
INCOMPLETE. The execution watchdog streams partial logs; a crashed/timed-out worker
without an index is reconstructed as INCOMPLETE by the trusted collector.

`gate_freshness.yml` runs every four hours, on pushes to main, and on manual dispatch.
It reports exact-SHA defects across main's first-parent history, with FAIL precedence,
and separately reports fresh ancestor coverage, stale evidence, or absent history.
Ancestor coverage is operational freshness, never qualification of a different SHA.
The workflow exits nonzero and surfaces defects through its Actions run, logs and
job summary. **It is independent of PR and landing CI**, absent from
`lean_action_ci.yml` and `required-checks.txt`; it cannot block unrelated candidates
or change the train's required-job classification. Main pushes provide prompt
visibility without adding a required candidate check. A scheduled failure requires
immediate triage and a structural repair/regression pinned to its exact SHA; dependent
work stops. It never becomes a blanket failed check on unrelated landings.

All git subprocesses discard inherited `GIT_*` variables. The executor reads the
checked-out HEAD back and records it separately from the trusted controller revision.
Slot and bounded libraries come from the trusted controller ROOT. Exit 125 is reserved
for controller incompleteness (slot acquisition or bounded double timeout); a gate
that completes with exit 124 remains a failure. Per-gate exceptions preserve FAIL and
allow later gates to execute. Inventory/setup errors write indexed INCOMPLETE evidence.

Remaining qualification work belongs to **R-0676**, coordinated with R-0674/R-0675:
shared X4 dispatch agreement with ISS-0029 (whose referenced branch supplies no
reusable dispatcher), manifest-owned freshness windows and agreed exporter schema,
shared cross-user host slots, durable logs/samples, and observer-occurrence history
with missed-observer controls. No substitute gate list, private semaphore or hosting
setting is introduced. The allocated **R-0690 — Scheduled evidence integration
and watchdog qualification** owns live qualification of the watchdog on the real schedule: land these producer interfaces, validate real cancellation,
queue and remote-note retry scenarios on CI2/GitHub, archive artifacts durably, and
prove an absent observer is detected independently. This is qualification work, not
a claim that fixture-only tests have exercised a real campaign.

No GitHub settings, secrets, runner registrations or branch protection changes are
required by this patch. Its publisher requests job-scoped `contents: write` and
`actions: read` in workflow YAML; the coordinator must verify repository policy allows
those scopes. Host slot-directory provisioning and durable archival remain explicit
operational dependencies; this branch does not make those changes.

## Focused verification

Inside `nix develop .#gates`:

```
bash scripts/checks/scheduled_gates.sh
bash scripts/checks/lib/bounded.sh 30 bash scripts/checks/workflow_yaml.sh
```

Controls assert full diagnostics for stale, absent, wrong-SHA, cancelled, replay
collision, forged host, missing artifact/control and fail-then-pass disagreement.
The root runner requires `scheduled_gates.sh`, which runs protocol, executor,
collector, publication and history-observer controls through the bounded runner.
These are controller controls, not evidence that actual campaign/collector integration
has run. Qualification requires coordinator CI2 checks after the dependencies above
land; do not run heavy campaigns locally or weaken campaigns to make them cheap.
