Scheduled tiers: add exact-SHA append-only attempt protocol, diagnostic controls,
main-only concrete-ci-2 scheduling and independent GitHub-hosted freshness observation.
Depends explicitly on ISS-0029 (688e10a8) runner binding and R-0674/R-0675's G1/G2/G3
registry; no duplicate gate list. Execution remains INCOMPLETE until the registry and
shared cross-user two-heavy-slot helper are available. Hosted completion/overdue collection uses Actions job provenance and an isolated
write token; freshness remains independent of landing checks. FAIL cannot age out.
R-0676 still owns
shared X4 dispatch integration, registry windows, and durable log/sample archival;
see scripts/checks/scheduled/README.md for the complete design and qualification limits.
