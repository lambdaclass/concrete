# Gate C execution identity

The command in each `scripts/checks/gates.json` row selects `SELECTED_TIER` explicitly.
That tier is the only execution-mode selector; ambient event, ref and runner values
are authorization inputs, never mode selectors. `execution_policy.py` is pure policy:

- `pr`: GitHub-hosted, `pull_request`, `refs/pull/N/merge`; deterministic coverage only.
- `qualification`: self-hosted, `push` or `workflow_dispatch`, main/ci/wip branch.
- `daily`: self-hosted, `schedule` or `workflow_dispatch`, main/ci/wip branch.

Every mode binds the checkout and reports to the selected full SHA. Qualification
and daily always verify ISS-0029's authoritative ci2 inventory, including smoke runs;
there is no fallback inventory. An unavailable binding remains INCOMPLETE.
Entry points snapshot external identity and pass it to `campaign.sh`; libraries
must not read GITHUB/SELECTED/SCHEDULED/RUNNER variables themselves.

`scaling.sh --check-execution-policy` checks the checkout and execution policy only:
it never builds, measures, or produces qualification evidence. `mode_controls.py`
uses this entry point with realistic positive and negative contexts and exact
status/diagnostic assertions. Full runs additionally perform ci2 binding before
building. `hermetic_controls.py` runs all cheap C controls with clean and hostile
CI identities and compares their evidence; `workflow_yaml.sh` runs this in PRs.
The full scaling and native determinism campaigns retain their registry tiers.

Scheduled evidence qualification is owned by R-0676's public
`scripts/checks/scheduled/protocol.py` validator (ISS-0042). C campaigns emit raw
reports and lifecycle outcomes; the shared scheduler preserves them, and collection,
publication and observation validate against the selected commit's registry.
C has no independent scheduled-evidence parser or qualification verdict.
Determinism currently has PR/qualification rows only; scaling-daily is C's
scheduled entry. `scheduled_evidence_controls.py` constructs synthetic metadata
from the actual committed daily registry and exercises public validation,
collection, append and observation with exact diagnostics for substituted gates,
invented/missing controls, wrong diagnostics, missing reports and wrong commands.
These cheap controls run in clean and hostile environments via `workflow_yaml.sh`;
they never run campaigns or publish evidence. Shared protocol tests additionally
exercise publication and artifact binding. Full campaigns retain their existing tiers.

## Real scaling qualification

The registry already exposes one bounded command per mode; invoke its executor,
never copy a row's campaign command into another script. On concrete-ci-2, in the
exact candidate checkout and the real runner service environment, run:

```sh
nix develop --command python3 scripts/checks/lib/gate_manifest.py execute --id scaling-qualification --tier qualification
```

`SELECTED_SHA` must be the full candidate commit; `GITHUB_EVENT_NAME` must be
`workflow_dispatch` (or `push`), `GITHUB_REF` its authorized main/ci/wip ref,
`GITHUB_REPOSITORY` the real repository, `RUNNER_ENVIRONMENT=self-hosted`, and
`RUNNER_NAME` the actual inventoried runner service name. Preserve the real run
ID/attempt as `SCHEDULED_ATTEMPT_ID`. Do not substitute a fixture inventory or
invent runner identity for an SSH shell. The registry owns the watchdog budget
and produces `scaling-qualification-report/` with SHA, provenance, raw samples,
analysis and control diagnostics. Exit zero from policy probes or PR coverage
is not qualification. Preserve the report even on INCOMPLETE or FAIL.

A real `CI` workflow_dispatch at the candidate wip ref checks out `github.sha`,
routes Scaling to concrete-ci-2, invokes this same registry entry and uploads
its declared artifact paths. It runs fresh measurements; it does not accept a
previous PR result as qualification. After landing, dispatch `gate_schedule.yml`
on main with `sha=<full main-history SHA>` and `tier=daily`: its trusted controller
selects the committed `scaling-daily` registry row, runs it on the inventoried
host and collects the reports and exact-diagnostic controls through R-0676's
shared protocol. For a direct daily campaign (raw reports only), the registered
command is:

```sh
nix develop --command python3 scripts/checks/lib/gate_manifest.py execute --id scaling-daily --tier daily
```

Qualification remains **INCOMPLETE** in this checkout: ISS-0029's authoritative
`scripts/checks/lib/repro_manifest_compare.py` inventory API is absent. Both the
campaign and scheduler refuse with `INCOMPLETE dependency: ISS-0029 runner binding
unavailable`; merely running on concrete-ci-2 does not satisfy that dependency.
The structural completion is to land ISS-0029's authoritative parser and provisioned
inventory, shared by all consumers, then run the real commands above. The
coordinator must name/file the ISS-0029 runner-binding roadmap row in its sweep
before calling this staged work complete; no R ID for that dependency is present
in this checkout. R-0676 owns scheduled evidence integration, not a replacement
inventory. No real qualification or daily measurement has been run here.
