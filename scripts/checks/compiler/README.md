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
