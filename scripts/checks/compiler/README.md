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
