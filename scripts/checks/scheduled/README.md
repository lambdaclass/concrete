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
It reports exact-SHA defects across every eligible ancestor of main, with FAIL precedence,
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
corruption ([ISS-0043](../../../ISSUES.tsv)) was repository-making fixtures
inheriting hook-local Git variables.
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


## Shared eligible main history (R-0676)

The structural cause of the replay/observation mismatch was independent ancestry
selection: replay accepted merged second-parent commits while observation skipped
them. `history.py:main_history` now owns the eligible set (all commits reachable
from `refs/remotes/origin/main`). The replay CLI and production observer both use
that definition; the workflow contains no independent ancestry predicate.
Observation reads every annotated eligible ancestor and preserves every permanent
SHA-pinned FAIL regardless of age or a newer PASS. First-parent order is used only
to select operational freshness coverage and recover older operational
INCOMPLETE/MISSING occurrences. A side-branch PASS cannot establish that coverage.

The cheap PR control builds a real merge graph and real daily/weekly notes, checks
shared replay authorization of the second parent, and invokes the production
observer entry point. A second-parent FAIL plus fresh tip PASS must return exactly
`FAIL <tier> <replayed-sha>: recorded attempt` for both tiers and exit nonzero.
It runs in both clean and hostile environments. Registry/inventory dependencies
use the suite's explicit fixtures; no campaign is run locally. The scheduler sweep
found no other independently defined ancestry set: execution uses the accepted
exact SHA, collection transports trusted selection (or the scheduled main head),
and publication stores exact-SHA notes without applying another history filter.
R-0690 retains the live CI2 replay/collection/publication qualification obligation.


## One owner per scheduler fact

Design audit of d6254f31967d (round 12). This is an implementation contract,
not a claim that the residuals below are fixed. Prefer the best long-term design;
never trade structural ownership for speed, a smaller diff, or fewer review rounds.
The structural cause is that callers still assemble parts of execution context and
some derived facts independently. R-0676 owns the controller repair; R-0690 owns
live qualification and operational dependencies. If a separate tracking row is
needed, send ISS-XXXX and its closure condition to the coordinator for allocation;
writers do not allocate IDs or edit ROADMAP.md. No OD is selected.

Citation convention: bare Python/shell filenames below are relative to
`scripts/checks/scheduled/`; `lib/` means `scripts/checks/lib/`; `checks/` means
`scripts/checks/`; `workflows/` means `.github/workflows/`. Each citation names an
existing line inspected in this tree. Proposed helpers and controls have no line
citation because they do not exist. “MISSING” means no control proves the entire
single-owner property; narrower controls are identified separately. Consumers in
this audit include production adapters, workflow boundaries and test runners;
tests using fixture data are controls, not alternate production authorities.

### Ownership and residual ledger

| Fact | Single owner (current or required) | Consumers today | Duplicate or ambient read today | Control and residual |
|---|---|---|---|---|
| Eligible main set, resolved tip, topological and first-parent orders | `history.py:10` (`MAIN` at `history.py:7`) | `history.py:19`, `observe.py:51`, replay adapter `workflows/gate_schedule.yml:51` | No second ancestry predicate; `observe.py:53` separately assembles presentation order. Scheduled selection uses event SHA at `workflows/gate_schedule.yml:46`, deliberately distinct from replay's current-main tip. | `test_observe.py:109` checks real merge authorization and exact FAIL output; `test_observe.py:162` checks freshness scope. S11a P1 is covered by the step 2 EOF control; P2 still skips it on direct execution. Return a named history value with explicit ordering roles. |
| Repository, main ref, selected SHA/tier, controller and checkout identity | Trusted selection adapter `workflows/gate_schedule.yml:34`; history authority above; verification `run.py:102` | `run.py:88`, `collect.py:51`, `collect.py:67`, `protocol.py:160`, `publish.py:7`, `observe.py:50` | CLI snapshots at `run.py:180`, `collect.py:177`, `publish.py:45`, `observe.py:47`, `history.py:29`; The former Git import snapshot is removed; `process_adapter.py` captures the explicit invocation. Main literals at `collect.py:108`, `collect.py:149` and workflow checkout/ref guards are independently maintained. | `test_execution.py:45`, `test_collect.py:120`, `test_protocol.py:159`; MISSING shared identity/context ownership control. Residual: construct explicit trusted invocation identity once, retain event SHA versus history tip distinction. |
| Attempt/occurrence identity, immutable closure, event/job state | Authoritative Actions reconstruction `collect.py:67`; append invariant `protocol.py:221` | `run.py:91`, `collect.py:106`, `publish.py:23`, `protocol.py:174`, `protocol.py:244` | Occurrence is cron/replay plus run ID at `run.py:94`, event plus run ID at `collect.py:73`; collector overrides worker identity intentionally but the occurrence meanings differ. | `test_collect.py:79`, `test_publish.py:17`, `test_protocol.py:166`; MISSING occurrence parity control. Residual: name execution identity versus schedule occurrence and use one encoding for each. |
| Child environment, tool resolution, credentials and host configuration | Process-context owner: `lib/scheduled_process.py` | `protocol.py:40`, `run.py:83`, `publish.py:30`, `publish.py:34`, `hermetic_suite.py:17`, `lib/gate_manifest.py:159` | Named environment allowlist and explicit publisher role; PATH is a declared resolved tool-stage input, HOME is private, loader/shell startup settings are dropped. | `ProcessContext` real boundary probe and exact context/credential plants; explicit publisher capability tested through Python, CLI and shell. |
| stdin (S11a P1), stdout/stderr, other inherited descriptors | Required same process-context owner; current git launch `protocol.py:40` | `run.py:28`, `publish.py:30`, `publish.py:34`, `hermetic_suite.py:79`, shell/watchdog boundaries below | stdin now defaults to EOF, with explicit note-body payload; `test_observe.py:123` calls stdin-reading mktree. Scheduler calls override generic supervision with the validated slot descriptor tuple; unrelated descriptors close. | `test_process.ProcessStdin` proves EOF with an open never-closing pipe and the exact planted-inheritance refusal; `hermetic_suite.child` checks EOF at the actual child entry. The common `ProcessContext` probe detects fd 9 inheritance with an exact isolated-helper plant. Existing slot propagation controls `test_slot.py:98`, `test_slot.py:114`, `test_slot.py:119` cover intentional lease inheritance only. Residual: explicit EOF or payload, explicit streams, validated slot capability only. |
| cwd, filesystem roots, temporary storage, HOME/git configuration, umask | Required same process-context owner; trusted root `run.py:16`, registry root `protocol.py:17` | `run.py:99`, `run.py:109`, `run.py:134`, `protocol.py:40`, `publish.py:30`, `hermetic_suite.py:61` | Helper declares cwd, private HOME/config/temp and umask 077. Git system/global config and local hooks are disabled; trusted publisher checkout-local transport remains declared. | `ProcessContext`: exact cwd/HOME/umask/temp plants, hostile Git config/local-hook test and retained publisher transport. |
| Locale, timezone, wall clock and duration clock | Required invocation clock/context owner; UTC parser `protocol.py:44` | `run.py:18`, `run.py:129`, `collect.py:48`, `collect.py:116`, `collect.py:150`, `collect.py:164`, `observe.py:69`, `protocol.py:228` | One injected operation observation is shared by runner/collector/observer consumers; completion derives from monotonic elapsed time. Child locale/TZ are C/UTC; process specs carry absolute monotonic deadlines. | `ProcessClock` exact second-read and raw-read controls, remaining-deadline/expiry proof; `ProcessContext` exact locale/TZ plants; collector sweep tests shared observation. |
| Registry schema, gate IDs, tiers, argv, tools/seeds, controls, artifacts, freshness windows | `checks/gates.json:1`, sole parser `lib/gate_manifest.py:39` | `lib/gate_manifest.py:91`, `lib/gate_manifest.py:104`, `lib/gate_manifest.py:150`, `run.py:119`, `protocol.py:16`, `protocol.py:62`, `protocol.py:77`, `observe.py:54` | Gate inventory not duplicated. Current-controller WINDOWS versus selected-SHA gate registry is implicit; daily/weekly enumeration is repeated at `workflows/gate_freshness.yml:24`, `collect.py:62`, `lib/gate_manifest.py:96`. Tools/seeds are copied as declarations at `run.py:125`, not measured tool identity. | `test_registry.py:17`, `test_registry.py:62`, `test_registry_evidence.py:179`; MISSING complete schema/tier/window provenance control. Residual: explicit current policy versus historical evidence, generate adapters from owner, preserve real tool/seed attestation obligation. |
| Typed outcomes, transport codes, qualification, FAIL permanence and precedence | Event interpreter `lib/gate_outcome.py:22`; evidence authority `protocol.py:160`, `protocol.py:228` | `with_slot.sh:16`, `run.py:137`, `run.py:148`, `run.py:172`, `collect.py:92`, `publish.py:9`, `observe.py:22` | Reduction of PASS/FAIL/INCOMPLETE repeated in run and protocol; transport 125 is interpreted separately from semantic status by design but has no shared typed adapter. | `test_protocol.py:108`, `test_protocol.py:125`, `test_protocol.py:135`, `test_slot.py:79`, `test_slot.py:84`; MISSING common reducer equivalence control. Residual: shared typed reduction with explicit transport conversion; observation retains every permanent FAIL. |
| Note refs, remote identity, publication lease, retry/idempotency | Lease algorithm `publish.py:7`; required shared note-ref constructor | `publish.py:10`, `protocol.py:253`, `observe.py:56`, `workflows/gate_freshness.yml:25` | Note prefix repeated in all four; origin remote/config is ambient transport configuration. | `test_publish.py:23`, `test_publish.py:29`, `test_publish.py:35`; MISSING ref/remote ownership control. Residual: one constructor and explicit remote capability, retain fetched-state lease and remote readback. |
| Budgets, retries, grace, cancellation and process lifetime | Gate budget `checks/gates.json:7`; watchdog `lib/bounded_exec.py:65`, retry policy `lib/bounded.sh:82` | `lib/gate_manifest.py:140`, `lib/gate_manifest.py:159`, `run.py:21`, `run.py:135`, `run.py:145`, `with_slot.sh:22`, `protocol.py:42`, `publish.py:11`, `publish.py:35`, `hermetic_suite.py:80`, `collect.py:34` | Formula/reserves repeated in run and manifest; fixed workflow limits at `workflows/gate_schedule.yml:61`, `workflows/gate_schedule.yml:89`, `workflows/gate_publish.yml:16`, `workflows/gate_publish.yml:41`, `workflows/gate_freshness.yml:13`, `workflows/gate_freshness.yml:32`, `checks/scheduled_gates.sh:9`. The former `run.py:28` watchdog is removed; all scheduler lifecycle calls use `bounded_exec.supervise`. | `test_registry.py:50`, `test_slot.py:48`, `test_workflows.py:16`; MISSING whole deadline composition control. Residual: one budget composition API, bounded cleanup, one process-tree supervisor; never shorten real campaigns to fit PR. |
| Runner membership, host slots and resource capability | Binding adapter `lib/ci2_binding.py:11`, verifier `lib/ci2_binding.py:22`; host lease `lib/heavy_slots.sh:4` | `protocol.py:56`, `run.py:106`, `protocol.py:160`, `collect.py:158`, `publish.py:48`, `observe.py:69`, `with_slot.sh:19`, `lib/bounded_exec.py:91` | Workflow labels are routing rather than proof; slot directory override/fd enter via ambient environment; inventory dependency remains unavailable on this base. | `test_execution.py:30`, `test_protocol.py:145`, `test_slot.py:52`, `test_slot.py:60`, `test_slot.py:119`. Residual: explicit validated host capability, actual shared provisioning and ISS-0029 integration under R-0690. |
| API provenance, selection log, pagination/size bounds, authentication and redirects | `collect.py:19`, `collect.py:22`, `collect.py:29`, `collect.py:39`, `collect.py:51` | `collect.py:106`, `collect.py:141`, `collect.py:67` | Proxy/CA/network configuration is ambient urllib input; schedule tier fallback depends on independently authored workflow title at `workflows/gate_schedule.yml:2`. | `test_collect.py:21`, `test_collect.py:29`, `test_collect.py:98`, `test_collect.py:120`; MISSING explicit network context/title derivation control. Residual: declared HTTPS transport policy and shared selection encoding; API data remains an external observation. |
| Artifact binding, report bytes/digests, index/log retention and durable evidence | Preservation `run.py:44`, qualification `protocol.py:72`, archive bounds `collect.py:97` | `run.py:139`, `run.py:161`, `collect.py:120`, `collect.py:131`, `collect.py:139`, `protocol.py:214`; uploads `workflows/gate_schedule.yml:92` | Retention 90 repeated at `workflows/gate_schedule.yml:99` and `workflows/gate_schedule.yml:107`; stored digest and declared tool metadata alone do not establish durable independent attestation. | `test_registry_evidence.py:121`, `test_registry_evidence.py:129`, `test_registry_evidence.py:168`. Residual: durable archive and live partial/cancelled upload qualification under R-0690; shared retention policy. |
| Test inventory, IDs, skips and results for every runner (S11a P2) | Required single inventory enumerator; current discovery `hermetic_suite.py:53` | `hermetic_suite.py:79`, `hermetic_suite.py:86`, `checks/scheduled_gates.sh:9`, direct main blocks `test_observe.py:81`, `test_protocol.py:156`, other test module main blocks | Direct execution exits before later classes at `test_observe.py:84`, `test_observe.py:109`, `test_observe.py:162`, `test_protocol.py:159`; clean/hostile comparison sees discovery only. Results recorder `hermetic_suite.py:39` records successes/skips, child exit records suite failure. | `test_hermetic.py:7` detects result drift; MISSING direct/discovery inventory parity. Residual: one canonical complete ID inventory consumed by each runner, compare IDs and outcomes without fixed counts; remove premature mains as part of that structural repair. |
| Trigger cadence, queue age, observer coverage and required-check independence | Cadence `workflows/gate_schedule.yml:3`, `workflows/gate_publish.yml:7`, `workflows/gate_freshness.yml:3`; queue policy `collect.py:20` | Selector `workflows/gate_schedule.yml:43`, collector `collect.py:116`, `collect.py:150`, `collect.py:164`, freshness `observe.py:13` | Cron-to-tier mapping repeated in workflow title and selector; no persisted independent observer-occurrence ledger. | `test_collect.py:73`, `test_collect.py:79`; MISSING missed-observer control. Residual: one schedule policy with generated adapters and independent occurrence monitoring under R-0690, outside unrelated PR/landing qualification. |

### Process-boundary contract and bypass census

The enforcement helper is `lib/scheduled_process.py`; `Invocation` owns the
materialized process specification and `bounded_exec.supervise` owns lifecycle.
`process_adapter.py` and `lib/scheduled_process.sh` project Python and shell calls
into that same specification. The census migration is implemented; the broader
context fields are materialized by the same helper (step 3 controls below).
It owns a validated process specification: resolved argv/tool, explicit cwd, complete
allowlisted env, private HOME/git configuration/temp root, fixed umask, locale and
TZ, stdin EOF by default or explicit bytes, stdout/stderr sinks, closed unrelated
fds and an explicit validated slot-fd exception, deadline/retry/grace and cancellation
policy. No other caller state is inherited. Identity, clock and network capabilities
are explicit invocation data; a subprocess cannot literally be insulated from the
kernel clock or network by an env filter. The helper must declare those dependencies
and tests must inject/mediate them. Nix's resolved environment is an explicit stage
output with a recorded tool identity, not permission to reuse the controller env.
Use the existing bounded supervisor for process-tree lifecycle through this helper;
do not add a third watchdog. Shell adapters submit the same specification through
its CLI. Necessary lease descriptors are explicitly transferred capabilities, not
an exception allowing arbitrary fd inheritance.

All census launches now enter the helper. The census below records the original
locations and separates
production launch sites from tests so migration cannot stop at the observed mktree.
Shell utility children and command substitutions inherit the wrapper's already
established context; wrappers must enter through the helper, and nested execution
must retain or narrow that context, never reconstruct it from ambient state.

| Boundary / all current call sites | Explicit inputs required |
|---|---|
| Git reader `protocol.py:41` (used by history, registry, observer and publication) | Repository cwd/-C, argv, git/config/transport policy, EOF stdin, captured streams, read budget; remove import-time fallback. |
| Private clone/checkout `run.py:109`, `run.py:110` through `run.py:28` | Source/destination, selected SHA, private cwd/config/home, EOF, setup budget and log sinks. |
| Nix gate/control `run.py:134`, `run.py:143` through `run.py:28` | Selected checkout, registry argv/budget, identity and outcome paths, resolved tool env, EOF, log/capture sinks, validated host-slot capability. |
| Note write `publish.py:30`, push `publish.py:34` | Explicit note body only for write; EOF for push; trusted cwd, remote/auth/config, ref/lease, output sinks and deadline. |
| Hermetic child `hermetic_suite.py:79` | Explicit interpreter, complete inventory, destination, clean/hostile context, EOF, isolated cwd/home/temp, captured streams, child budget. |
| Registry executor `lib/gate_manifest.py:159`; watchdog `lib/bounded_exec.py:99`; shell dispatcher `lib/bounded.sh:99`, `lib/bounded.sh:108`, `lib/bounded.sh:110` | Fully materialized process spec including stream routing and only the validated slot fd. Preserve generic bounded-run users' contracts; scheduler adapters must explicitly select the hermetic policy. |
| Gate wrapper `with_slot.sh:15`, outcome interpreter `with_slot.sh:16`, retry wrapper `with_slot.sh:22` | Gate argv/outcome stream/SHA, scoped env, EOF, checkout cwd, logs, lease and deadline; utilities at `with_slot.sh:4`, `with_slot.sh:11`, `with_slot.sh:14`, `with_slot.sh:20` use that same declared context. |
| Workflow controller launches `workflows/gate_schedule.yml:51`, `workflows/gate_schedule.yml:89`; publication git/config/collector `workflows/gate_publish.yml:38`, `workflows/gate_publish.yml:39`, `workflows/gate_publish.yml:41`; freshness git/observer `workflows/gate_freshness.yml:26`, `workflows/gate_freshness.yml:27`, `workflows/gate_freshness.yml:32`; suite `checks/scheduled_gates.sh:9` | Trusted checkout/context, event/selection inputs, scoped token only where needed, explicit EOF, output/summary paths, operation budget. Workflow actions remain external checkout/upload service boundaries, not hermetic subprocess claims. |
| Direct test subprocesses `test_slot.py:33`, `test_slot.py:53`, `test_slot.py:70`, `test_slot.py:75`, `test_slot.py:110`, `test_hermetic.py:38`, `test_workflows.py:30` | Fixture-owned spec; hostile state only as the planted input. `test_registry_evidence.py:57` saves the real run function for an interceptor, not an additional launch. Test gate shell payloads in `test_integration.py:143`, `test_integration.py:150`, `test_integration.py:157` exercise nested supervision through the same contract. |

### Ordered structural implementation and exact controls

Steps 1–2 and step 3 process fields are implemented and controlled below.
Step 3 is implemented; steps 4–6 remain MISSING. Each
must assert the expected exit/status and complete diagnostic, not just any failure.
Use isolated copies for planted regressions and compare canonical IDs, not counts.
The listed FAIL strings are required future control diagnostics, not existing output.

1. Introduce invocation context and the single process-spec helper, backed by the
   existing bounded supervisor. Migrate **all** boundaries in the census before
   calling process isolation complete. Add a source/adapter coverage control: plant
   a direct subprocess launch outside the helper; require exit 1 and exactly
   `FAIL scheduled process boundary: unmanaged launch`. Validate required spec fields;
   omit cwd and require exit 1 and `FAIL scheduled process specification: missing cwd`.
2. Prove inherited-state closure by role. For S11a P1 keep a writer end of a pipe
   open with no data until after the test completes (no timed EOF and no shell
   pipeline wait). Run a child that reads stdin, including real empty-tree mktree,
   through every adapter. It must see EOF promptly. Plant stdin inheritance in an
   isolated helper copy; a separate short supervisor must identify the blocked
   read, terminate/reap the whole child group, and emit exit 1 with exactly
   `FAIL scheduled process stdin: inherited open pipe`. An unrelated timeout is
   INCOMPLETE, never this planted PASS. Keep mktree as a stdin-sensitive probe;
   merely replacing it with write-tree would conceal the class.
3. Extend that same boundary probe across env, cwd, HOME/config/hooks, umask,
   locale/TZ, temp root, descriptors and credential scoping. Plant one undeclared
   key `SCHEDULER_FOREIGN`, cwd, HOME, umask, locale, timezone, temp root or fd 9;
   each detector exits 1 with exactly `FAIL scheduled process context: FIELD`,
   substituting respectively `env.SCHEDULER_FOREIGN`, `cwd`, `HOME`, `umask`,
   `locale`, `timezone`, `temp`, `fd.9`. Assert a validated slot survives and a
   forged slot still gives the existing exact invalid-descriptor diagnostic.
   Plant publisher token exposure in gate env and require exit 1 with exactly
   `FAIL scheduled process context: credential scope`. Inject one observation time
   and monotonic deadline; plant a second wall-clock read and require exit 1 with
   exactly `FAIL scheduled clock: undeclared read`. Test private git config while
   retaining declared publisher transport; no global config dependence is allowed.
4. Establish one inventory enumerator and common execution/reporting adapter for
   direct modules and hermetic discovery. Move main dispatch after definitions,
   and compare normalized complete test IDs (including skips) per module, then
   results in clean/hostile contexts. For S11a P2 plant a main dispatch before
   MergeHistory in an isolated module copy; require exit 1 with exactly
   `FAIL scheduled inventory: direct/discovery mismatch test_observe`. Repeat for
   ExplicitIdentity with exactly
   `FAIL scheduled inventory: direct/discovery mismatch test_protocol`. The parity
   check must observe what direct execution actually ran, not only import the
   file. A failed module is a separate failure; zero tests or matching truncated
   inventories cannot pass. Existing clean/hostile result and canary controls stay.
5. Materialize shared history/identity, note-ref, occurrence, schedule/tier and
   budget policy values and use them in every adapter, with generated workflow
   projections checked against owners. Preserve independent verification of
   untrusted evidence; do not replace it with trust in worker-supplied cached facts.
   Plant one divergent consumer per fact and require exit 1 with exactly
   `FAIL scheduled ownership: FACT`, with FACT one of `history`, `identity`,
   `note-ref`, `occurrence`, `schedule`, `tier`, `window`, `budget`, `retention`.
   Retain the real merge graph's exact per-tier recorded-attempt FAIL assertions.
   Exercise deadline composition with a fake monotonic clock, not long sleeps.
6. Share typed outcome reduction across execution/qualification adapters; keep
   authoritative recomputation and irreversible FAIL. Plant a reducer that lets
   INCOMPLETE or a later PASS mask FAIL; require exit 1 with exactly
   `FAIL scheduled outcome precedence: FAIL masked`. Keep exact registry-control
   diagnostics, wrong-SHA artifact controls and lease collision/readback controls.
   Add tool/seed attestation substitution and require exit 1 with exactly
   `FAIL scheduled provenance: tool/seed mismatch`. Plant redirected credential
   forwarding and require exit 1 with exactly
   `FAIL scheduled transport: redirected credential`. Plant missing observer
   occurrence and require exit 1 with exactly
   `FAIL scheduled observer: missed occurrence` using an independent observer.
7. Coordinator runs reviews with the instruction to do the best long-term thing
   rather than cut corners, then CI2/GitHub qualification under R-0690: cancellation,
   occupied host slots, queued attempts, leases with dropped responses, replay,
   durable archive and missed-observer detection. Archive corruption must produce
   exactly `FAIL scheduled archive: digest mismatch` and exit 1. Cheap PR controls
   cover the same contracts with deterministic fixtures; real full campaigns and
   expensive qualification remain daily/weekly, never weakened to fit a PR budget.

This round changes documentation only. No new control is claimed as executed and
no residual is silently accepted as qualification. The coordinator owns subsequent
implementation/review scheduling and any new issue/roadmap allocation.


### Round 13, implementation slice 1: steps 1–2

R-0676 owns this structural boundary repair; R-0690 retains CI2 qualification.
No roadmap IDs or owner decisions were allocated. The census includes production,
workflow, shell, registry and test adapters. Generic `bounded.sh` retains its stdin
contract; scheduler shell adapters explicitly select `scheduled_process.sh`.
The helper resolves argv and materializes cwd/environment/stream policy once per
invocation. Its stdin is an empty regular file by default, or explicit payload
bytes for note writes. The import-time Git environment snapshot is gone.

| Control | Status | Exact observed contract |
|---|---|---|
| Source/adapter census and isolated direct-launch plant | IMPLEMENTED | `ProcessCoverage.test_census`, `test_unmanaged_launch`, `test_unmanaged_shell_adapter`: exit 1, stderr exactly `FAIL scheduled process boundary: unmanaged launch\n`, empty stdout. |
| Required process specification field | IMPLEMENTED | `ProcessCoverage.test_missing_cwd`: exit 1, stderr exactly `FAIL scheduled process specification: missing cwd\n`, empty stdout. |
| EOF through Python, CLI, shell, slot, registry and real Git mktree adapters | IMPLEMENTED | `ProcessStdin.test_eof_through_adapters`: writer held open through all checks, no timed EOF; real empty-tree `mktree` retained. Hermetic child entry also reads EOF. |
| Isolated helper stdin-inheritance plant | IMPLEMENTED | `ProcessStdin.test_production_and_planted_inheritance`: production exit 0 with empty streams; plant exit 1, stderr exactly `FAIL scheduled process stdin: inherited open pipe\n`, empty stdout. Independent short supervisor identifies the exact pipe inode and blocked-read kernel wait channel before cancelling/reaping the group; writer closes only afterward. |
| Unrelated timeout | IMPLEMENTED | `ProcessStdin.test_unrelated_timeout_is_incomplete`: exit 124, stderr exactly `INCOMPLETE scheduled process stdin: unproven timeout\n`, empty stdout. Missing Linux `/proc` proof is likewise INCOMPLETE, never a planted-control pass. |
| Context fields/credential isolation (step 3) | IMPLEMENTED | `ProcessContext`: common child probe through Python, CLI, shell, slot and registry; isolated helper plants each exit 1 with empty stdout and exactly `FAIL scheduled process context: FIELD\n` for `env.SCHEDULER_FOREIGN`, `cwd`, `HOME`, `umask`, `locale`, `timezone`, `temp`, `fd.9`, `credential scope`. Existing `Slot` positive/forged descriptor controls remain. |
| Private Git config and publisher transport (step 3) | IMPLEMENTED | `ProcessContext.test_private_git_config_hooks_and_publisher_transport`: hostile global config ignored, local hooks cannot run, declared checkout-local remote and HTTP extraheader retained; real `ls-remote` uses the declared local fixture transport despite a hostile global URL rewrite. Explicit publisher role alone carries GH_TOKEN; gate selection removes tokens. |
| Single observation and absolute deadline (step 3) | IMPLEMENTED | `ProcessClock.test_single_observation_and_second_read_plant`: production exit 0, empty streams; isolated second-read plant exit 1, empty stdout, stderr exactly `FAIL scheduled clock: undeclared read\n`. `test_no_unmediated_wall_clock_reads` rejects raw wall reads outside the owner with that same diagnostic. `test_deadline_does_not_restart_at_execution` proves only remaining monotonic budget is used and expired work never launches. `Collector.test_sweep_injects_one_observation_into_every_record` proves sweep propagation without recapture. |
| Direct/discovery inventory parity (step 4) | MISSING | Premature main dispatch remains for its owning slice. |
| Shared derived policy values (step 5) | MISSING | Accepted ownership controls remain required. |
| Common outcome reducer (step 6) | MISSING | Accepted equivalence controls remain required. |

The short stdin proof uses Linux wait-channel evidence because the required live
scheduler host is CI2. A host without that evidence returns INCOMPLETE. CI2 must
qualify the helper with real Nix campaigns, slot transfer, cancellation and
partial-artifact retention; the light PR controls do not substitute for those runs.


### Round 14, implementation slice 2: step 3

The same R-0676 process owner now filters a named environment allowlist by role,
creates private HOME/config/temp roots for each execution, fixes umask to 077 and
locale/TZ to C/UTC, and closes unrelated descriptors. Only an inode-checked,
lock-validated host slot transfers; generic bounded users retain their existing
policy. The publisher shell call explicitly declares its role; gates never receive
publisher tokens. Git ignores system/global configuration and disables local hooks
while retaining the trusted checkout's declared local transport (including the
Actions checkout HTTP extraheader). No global Git identity/config is required.
The resolved PATH remains an explicit tool-stage input, including test shims and
Nix's selected tools; loader and shell startup variables are not inherited.
Each context plant runs an isolated helper copy and is judged by the common real
child probe, with exact return code and complete stderr checked. Steps 4–6 have
not been started. CI2 qualification remains owned by R-0690.


Clock capability is owned by `scheduled_process.operation_clock`: an operation
captures one UTC observation and passes the clock to its consumers. Runner
completion advances that observation by monotonic elapsed time; collector age,
cutoff and overdue records and all observer tiers use the same observation.
Process specifications carry an absolute monotonic deadline; execution consumes
only its remaining budget and cannot restart it after setup or child creation.
`ProcessClock.test_supervisor_charges_startup_to_declared_deadline` proves that
startup consumes the same declared deadline in the existing lifecycle supervisor. The wall source
refuses a second read, and source coverage rejects undeclared raw wall reads in
production scheduler modules. This mediates controller dependencies; it is not a
kernel clock/network sandbox for arbitrary gate programs.

CI2 still needs the real Nix gate/control path under the named runtime allowlist,
validated slot transfer, cancellation/reaping and partial-artifact retention, plus
trusted publisher transport and the scheduled tier campaigns. Run those against
the exact final slice SHA; the focused PR probes do not replace full daily/weekly
campaigns. No gate budget or tier has been reduced.


### Round 16: structural repair of slices 1–2 (R-0676)

The S14 failure and S13a findings are confirmed. The structural cause of
ISS-0043 was duplicated enforcement: a caller could supply a locally named
"clean" dictionary while bypassing the shared process specification. The process
owner now applies the one sanitizer unconditionally. `protocol.clean_env` is a
re-export of that same function. `process_coverage.check_python_launches` forbids
raw Python launches (including variable argv and aliases) throughout the scheduled
package, tests included. The Python leg of `sandbox_isolation.sh` projects that
complete Python/shell/workflow census, including **every** scheduled module;
it does not maintain a second scheduled
Git policy. Its existing non-scheduled repository-maker audit remains in place.
The shell/workflow control checks substitutions, conditionals, loops, braces,
pipelines, conjunctions and disjunctions. Planted launches fail with exactly
`FAIL scheduled process boundary: unmanaged launch`. Direct supervisor access is
limited to the independent stdin control and the named lifecycle regression test.

`Invocation.capture`, the Python adapter and the real CLI validate raw required
fields before conversion. There is no ambient cwd/environment fallback in the
scheduled specification. Operation call sites declare their repository, stage
environment and budget. The history fixture injects a repository dependency rather
than changing the test process's cwd. Resolved tool paths preserve executable
basenames, which matters for multicall binaries in the Nix userland.

`bounded_exec` owns one authenticated Unix-socket supervisor per invocation tree.
Nested runners are clients; only the owner launches process groups. Requests carry
parent identity, deadlines, environment and transferred stream/slot descriptors.
The owner registers the request before a descendant can launch, rejects children
of cancelled requests, terminates every registered descendant group with TERM
then bounded KILL, waits for leaders, and on Linux adopts/reaps orphaned group
members. Cancellation cannot kill an inner supervisor before it propagates: there
is no inner supervisor. The private `BOUNDED_TREE_*` capability is explicitly
carried through the process specification; it is not a gate configuration input.
Slot descriptor numbers are remapped after transfer while retaining their open
file descriptions. Tests cancel and time out the real slot adapter, verify both
gate PIDs disappear, and acquire both pool locks afterward. The independent stdin
control deliberately establishes its own short supervision boundary.

`bounded.sh` alone owns retries, outcome reset, incomplete/runner-failed status
and recovered/incomplete logs. `scheduled_process.sh` selects the single-attempt
runner, whose status protocol is `exit N` / `timeout`; it contains no retry loop.
Per-attempt structured outcome files live in the process owner's private context,
so the slot adapter no longer wraps a gate in redundant watchdogs. Focused tests
exercise recovered retries (including replay of an explicitly declared stdin file),
double timeouts and runner failure with exact status,
log and diagnostic assertions.

The open pipe is attached directly to each CLI/shell/slot adapter under test.
The proof program forks a same-group member; cleanup checks both members, not just
the leader. A generic stdin-inheriting adapter must fail with exactly
`FAIL scheduled process stdin: inherited open pipe`, while unrelated timeouts
remain exactly `INCOMPLETE scheduled process stdin: unproven timeout`.
The writer stays open until cleanup completes. The sandbox live probe counts
Linux tasks (threads) for RLIMIT_NPROC, retaining the same 512 additional-task
allowance on shared hosts.

Reproduction evidence against the rejected baseline: an outer 0.5-second timeout left the recorded
gate PID alive and slot-1 locked (F1); the generic stdin-inheriting bounded adapter
returned `0, EOF` through the old EOF probe (F2); omitted cwd/env reached execution
without a specification error (F3); removing the managed wrappers from workflow
substitutions/conditionals and planting `then "$@"` still passed the census (F4);
the old stdin control returned its exact planted-failure diagnostic while a same-group fork remained an unreaped zombie, verified under a separate subreaper that cleaned it up (F5);
a missing executable left no outcome or log through the scheduled retry adapter
(F6). All six findings are accepted; none is dismissed as a budget problem.

R-0676 owns these repairs. R-0690 still owns CI2 qualification with real Nix
campaigns, cancellation and partial artifacts at the final commit SHA. No gate
budget/tier was reduced and no step 4 work is included.
