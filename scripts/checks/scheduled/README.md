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
single `ci2_runners.txt` binding. `scripts/checks/lib/ci2_binding.py` is the sole
adapter: `read_inventory(lib=None)` imports that authoritative parser, and
`verify(repository, environment, name, runners=None)` checks exact membership.
`protocol.py` delegates to it; Gate C must use this same helper (its CLI verifies
standard `GITHUB_REPOSITORY`, `RUNNER_ENVIRONMENT`, `RUNNER_NAME`). Neither the
ISS-0029 parser nor its authoritative inventory exists on this base. The exact
absence diagnostic is `INCOMPLETE dependency: ISS-0029 runner binding unavailable`,
even if an isolated `ci2_runners.txt` appears. This is a deliberate dependency,
not a second parser or a fallback that weakens provenance. Repository/runner registration remains
the trust boundary described in that branch's `docs/SELF_HOSTED_RUNNERS.md`.
The referenced tip has no reusable exact-SHA dispatch helper: selection is a
main-only schedule-specific entry point; it does not replace the repro CI leg.
The coordinator must reconcile X4's shared dispatch interface with ISS-0029 before
qualification; a second independent trusted-runner implementation is forbidden.

## Gate registry (G1)

`scripts/checks/gates.json` is the sole declarative owner of gate IDs, tiers,
argv commands, budgets in seconds, runner classes, output artifacts, tools, seeds,
controls, workflow bindings and daily/weekly freshness windows. JSON schema version
1 has `schema`, `windows` and `gates`. `tests/manifest.tsv` on B/N5 is a separate
program/expected-outcome inventory; it owns no gate tiers.

The single reader/validator is `scripts/checks/lib/gate_manifest.py`:
`load(path=REGISTRY)` returns a validated document; `tier_rows(tier, data=None)` and
`scheduled_rows(tier, data=None)` return matching rows with a selected `tier`;
`check_workflow(data=None, root=ROOT, workflow=None)` checks CI and root inventory
coverage. CLI: `validate`, `check-workflow`, `tier --tier T`,
`scheduled --tier daily|weekly`, `execute --id ID --tier T`.
Unknown fields, duplicate IDs/bindings, unsafe artifact paths, invalid budgets,
scheduled runner mismatches and missing scheduled controls are rejected generically.
No campaign count or specific gate belongs in the validator.

Every existing CI command and root check is registered. CI calls `execute` by ID;
commands and budgets do not appear a second time in YAML. Job timeouts are generated
as `ceil(sum(registered step budgets) * 3 / 60) + 10` minutes (two bounded attempts
plus setup/cleanup reserve), and checked for exact drift. When adding rows, update
those generated job timeout values. `gates_registry.sh`, required by `run.sh`,
checks workflow invocation, tier, budget, runner and root inventory completeness.
Controls for row removal, literal/duplicate YAML commands, misrouted tiers, unknown
fields, budgets and runner drift run through `scheduled_gates.sh`.

Rows contain `id`, `tiers`, `command` (argv), `budget_seconds`, `runner_class`
(`ci`, `github-hosted`, `native-matrix`, `concrete-ci-2`), `artifacts`, `tools`,
`seed`, `controls`, and `workflow` (`null` or `job`/`step` binding). Each control
has `id`, `command`, `exit_code` (nonzero, excluding reserved 125), and `diagnostic`
(the complete exact combined output after stripping surrounding whitespace).
Each artifact declares `path` and `sha_file`; scheduled outputs require a SHA file.
No daily/weekly campaigns exist on this base: those tiers remain INCOMPLETE with
`INCOMPLETE dependency: tier has no registered gates` until campaign owners add rows.
Gate C must add its own PR/qualification/daily declarations here and CI bindings;
it must not introduce a separate registry or add tiers to the fixture inventory.

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
logs are indexed per attempt. Registered raw report trees are copied under
`gate-N-artifacts/` in the result index before the private checkout is removed,
including partial outputs from incomplete runs. Each regular file is indexed with
its relative path, byte count and SHA-256. Symlinks and special files are refused.
The artifact's `sha_file` names a file inside the declared artifact containing only
the selected full SHA (with an optional final newline). Missing required reports
fail with `FAIL scheduled artifact: missing PATH`; a mismatched binding fails with
`FAIL scheduled artifact: selected SHA mismatch PATH`. Partial reports remain in
the upload on failure. CI upload steps obtain artifact paths from the registry
executor's `artifact_paths` step output; literal YAML paths are drift too.

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

## Scheduled-gate environment and outcome contract

Both gate commands and control commands receive the same generic environment:
`SELECTED_SHA` (selected full 40-digit commit), `SELECTED_TIER` (`daily` or `weekly`),
`SCHEDULED_ATTEMPT_ID` (`GITHUB_RUN_ID/GITHUB_RUN_ATTEMPT`), `GITHUB_REPOSITORY`,
`RUNNER_NAME`, and `RUNNER_ENVIRONMENT`. Runner identity uses the real Actions
values; no missing value is fabricated. Before executing any code, the controller
requires `RUNNER_ENVIRONMENT=self-hosted` and exact inventory membership.
No gate-specific environment variables are supplied by the scheduler.

The trusted wrapper additionally supplies `SCHEDULED_OUTCOME_FILE` (a private JSONL
append stream for that invocation) and `SCHEDULED_OUTCOME_HELPER` (the trusted
`gate_outcome.py`). Emit a semantic failure with
`python3 "$SCHEDULED_OUTCOME_HELPER" FAIL "DIAGNOSTIC"`; emit gate-level availability
loss with `INCOMPLETE` instead. Events contain `status`, `diagnostic`, and `sha`.
Only the gate's top-level code emits a gate outcome. `bounded.sh` records local
watchdog state only: check `bounded_is_incomplete "$rc"` and explicitly emit
INCOMPLETE when an unrecovered timeout prevents the gate's observation. Expected
timeout controls and recovered inner retries must not emit gate outcomes.
Each outer watchdog attempt has a private outcome stream, discarded on timeout.
Non-timeout failures must emit FAIL before continuing after an error; otherwise a
bare nonzero exit is FAIL when no INCOMPLETE event exists.
Explicit FAIL takes precedence over every INCOMPLETE event. A mismatched event SHA
is FAIL with `FAIL scheduled outcome: selected SHA mismatch`. The wrapper transports
INCOMPLETE as reserved exit 125; an unmarked gate exit 124 or 125 remains FAIL.
Malformed streams fail with `FAIL scheduled outcome: invalid event stream`.
The stream is separate from stdout/stderr: log text cannot impersonate an outcome.

All git subprocesses discard inherited `GIT_*` variables. The executor reads the
checked-out HEAD back and records it separately from the trusted controller revision.
The selected registry is data parsed by the trusted reader; no historical exporter
is executed. Slot, bounded and outcome libraries come from trusted ROOT. Per-gate
exceptions preserve FAIL and allow later gates to execute. Inventory/setup errors
write indexed INCOMPLETE evidence.

### Gate C integration instructions (verbatim handoff)

Add scaling PR, qualification and daily rows and determinism rows to
`scripts/checks/gates.json`; give different commands/budgets distinct IDs, bind CI
rows by workflow job/step, and declare report directories with a `sha_file` inside
each report. Use `SELECTED_TIER` and `SELECTED_SHA`, `SCHEDULED_ATTEMPT_ID`, and the
standard `GITHUB_REPOSITORY`, `RUNNER_ENVIRONMENT`, `RUNNER_NAME`; remove scheduler
coupling to `CONCRETE_SCALING_*`. Use `ci2_binding.py` for runner verification.
Write the selected full SHA into each declared `sha_file`. Use the shared
`bounded.sh` and emit structured FAIL for genuine failures through
`SCHEDULED_OUTCOME_HELPER`; propagate INCOMPLETE for build, measurement and planted
control timeouts. Keep exact-diagnostic controls in the registry. Route CI commands
through `gate_manifest.py execute` and upload its `artifact_paths` output; update
the derived job timeouts and run `gates_registry.sh`. Do not add a second manifest,
runner parser, semaphore or gate-specific assertion to the generic validator.

Remaining qualification work belongs to **R-0676**, coordinated with R-0674/R-0675:
shared X4 dispatch agreement with ISS-0029 (whose referenced branch supplies no
reusable dispatcher), live adoption/provisioning of the shared cross-user host
slots, durable logs/samples, and observer-occurrence history
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

Partial report trees are preserved even before their SHA binding is written. Their
artifact entry has `binding_status=INCOMPLETE`, `sha=null`, and an exact missing
binding diagnostic; only verified bindings carry `sha`. Missing bindings cannot
upgrade an INCOMPLETE gate to FAIL. Present mismatched bindings and unsafe paths
remain FAIL. A successful command with an unfinished binding is INCOMPLETE.

A child of a heavy-slot holder reuses its exported `CONCRETE_HEAVY_SLOT_FD`,
validated against the provisioned pool's inodes. The watchdog explicitly preserves
that descriptor across its subprocess boundary. Release closes only the caller's
copy, so a child cannot release its parent's lock. Invalid inheritance fails
closed rather than consuming another host slot.


## Explicit identity and hermetic controls

The structural owner is R-0676's scheduled evidence protocol. CLI entry points
snapshot the process environment once and pass it down explicitly. Validation
requires the target repository, full SHA, tier, runner inventory and expected
attempt as arguments; `attempt=None` explicitly denotes a history containing
multiple attempts. Repository membership in an inventory does not select the
target repository. The collector obtains the expected attempt from trusted Actions
metadata. Publication and observation pass the target independently of note data.
No library validator reads GitHub or scheduler environment variables.

Execution receives a supplied environment mapping; gate environments override its
selected SHA, tier and attempt with the selected inputs. API credentials, Git
subprocess environments, registry output paths and structured-outcome SHA/path
also arrive through arguments. `ci2_binding.py` continues to use only the shared
ISS-0029 inventory adapter, with no alternative identity source.

Tests own their fixture identities and subprocess environments. The scheduled gate
runs the entire lightweight suite twice in separate processes, once without any
`GITHUB_*`, `SELECTED_*`, `SCHEDULED_*` or `RUNNER_*` keys and once with foreign
values for every known or ambient key in those namespaces, plus unknown keys.
Both runs must pass and return identical per-test outcomes. The control itself
has a planted result-drift test with an exact diagnostic. Each run retains the
original 45-second suite budget; the enclosing 100-second bound allows both runs
and reporting. These checks exercise controller behavior, without running heavy
campaigns or replacing R-0690's live qualification.


## Registry qualification of retained evidence

The structural cause of ISS-0042 was treating shape-valid worker metadata as a
complete campaign attestation. R-0676 owns the structural repair: `validate` now
reads `scripts/checks/gates.json` from the selected full commit with trusted
`gate_manifest` code. Neither the controller working copy nor a newer registry
can substitute for that revision. No scheduled gate inventory is duplicated in
the protocol.

Collection, publication (including existing history), and observation all use
this validator. It returns a copy retaining the worker's `status`, gates, controls,
logs and partial artifact manifests, with a recomputed `qualification_status` and
`qualification` diagnostic list. A worker PASS alone is insufficient: qualifying
PASS requires `qualification_status=PASS` and no diagnostics. Incoming cached
qualification fields are always replaced. Operational scheduler/Actions failures
are record metadata (`execution_error`), never invented registry gates.

Qualification requires the exact tier gate IDs, exact registered command, exact
control IDs and every passing control's exact registered diagnostic. Interrupted
or failed controls retain their observed diagnostics and cannot qualify. Each
declared artifact must appear at its registry-derived retained path and carry the
selected SHA and PASS binding status. Its file manifest must contain the declared
`sha_file` with the digest and size of that exact full SHA, optionally followed by
a newline; malformed, duplicate or escaping file entries fail qualification.
The executor applies the same binding-byte contract before preserving reports.

Missing gates, controls, artifacts or bindings yield INCOMPLETE qualification;
substitutions, invented inventories, wrong commands/diagnostics and wrong SHA
bindings yield FAIL qualification. Both retain exact diagnostics in published
notes, without converting partial evidence into success or discarding it. A
later complete attempt can recover an incomplete attempt; a substitution FAIL
remains visible permanently. Shape/provenance contradictions still reject invalid
records. Absence/cancellation continues to record MISSING without campaign data.

The lightweight controls commit a hermetic five-control registry and report
contract, then remove or substitute each required component through collection,
real publication serialization, and observation. They also prove selected-commit
registry identity, partial recovery and cached-verdict recomputation. Production
executor report fixtures must qualify under their own committed registry. These
controls remain in both clean and hostile environment runs; campaign-scale live
qualification remains with R-0690 on concrete-ci-2.

Git isolation and observation recovery (R-0676): the structural cause of caller
corruption was repository-making fixtures inheriting hook-local Git variables.
The shell entry uses `git_sandbox`; Python Git subprocesses use the shared
`protocol.clean_env` owner, including fixture helpers. The protocol captures its
Git environment once at entry, retaining the gate shell PATH and removing Git
and scheduler identity namespaces. The Python sandbox audit in
`sandbox_isolation.sh` checks repository-making scripts at every depth. The
hostile suite points Git directory, work tree, index and object storage at a
committed canary and compares every file byte afterwards; a planted index change
must produce the exact canary-change diagnostic.

Observation uses recomputed typed qualification outcomes (`status`, `diagnostic`,
`operational`), never diagnostic wording to decide recovery. Missing registry
evidence is operational only for a producer-recorded INCOMPLETE attempt; missing
evidence in a claimed PASS and an unavailable registry remain nonrecoverable
qualification defects. A newer qualified ancestor or tip PASS covers older
operational INCOMPLETE/MISSING occurrences. Every registry substitution FAIL is
permanent, regardless of age or later PASS, and observation diagnostics include
the exact tier and SHA. The owning protocol and hermetic controls enforce this
contract in the cheap PR tier; full campaigns retain their scheduled tiers.
