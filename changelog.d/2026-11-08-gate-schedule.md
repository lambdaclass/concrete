Scheduled tiers: add exact-SHA append-only attempt protocol, diagnostic controls,
main-only concrete-ci-2 scheduling and independent GitHub-hosted freshness observation,
and wire Gate C's `SCALING_TIER=daily` backend scaling and depth probes into the daily
tier via `scripts/checks/lib/gate_manifest.py`.
Uses the shared `scripts/checks/lib/ci2_runners.txt` binding (and ISS-0029's
`repro_manifest_compare.py` once merged) with R-0674/R-0675's G1/G2/G3 registry
contract; no duplicate gate list. Live execution on `concrete-ci-2` still requires the
shared cross-user two-heavy-slot helper (`R-0676`). Hosted completion/overdue collection uses Actions job provenance and an isolated
write token; freshness remains independent of landing checks. FAIL cannot age out.
R-0676 and R-0690 still own
shared X4 pre-landing dispatch integration with ISS-0029, registry windows, and durable log/sample archival;
see scripts/checks/scheduled/README.md for the complete design and qualification limits.
