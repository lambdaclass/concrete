### Shared issue and bug reservations (ISS-0034; R-0542, R-0561)

One root allocator reserves ISS and bug IDs on origin with atomic Git ref creation, immutable
identity metadata and exact remote read-back. Bug reservations share the frozen allocator's
namespace, preserving occupied legacy numbers. Issues gain a stable title independent of evidence.
The root checks now refuse unreserved IDs, duplicate records, reservation mismatches and independent
uses on origin main/wip branches. Temporary-repository controls cover exclusion across clones,
branch inheritance, offline refusal, legacy import and the real pre-push reservation tier.

Migration is explicit: the operator resolves the ISS-0017 collision (acceptance-shim to ISS-0033;
spec-* retains ISS-0017; lexer to ISS-0032), reviews a backfill plan and publishes its reservations
in one atomic transaction. No reservation refs are pushed by the writer; the live gate refuses
until backfill and branch reconciliation finish. See docs/ISSUE_IDS.md for the operator commands.

The backfill dry run also refused independent ISS-0018 introductions on five advertised wip heads;
the operator must reconcile those before publication. No collision exemption is installed.

Round 2: identity survives rebases, stacked branches, detached trains and evidence edits.
Enforce reservations on introduced/modified records relative to origin/main; audit all remote
drafts separately. Backfill and verify reservation refs before landing enforcement (exact operator
order in docs/ISSUE_IDS.md). Handle remote-only legacy objects, deleted files and malformed bug
records; cover both metadata namespaces and mixed hook tiers, Python sandbox makers, and issue
ledger append conflicts. No reservation refs were published by the writer.
