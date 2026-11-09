### Shared issue and bug reservations (ISS-0034; R-0542, R-0561)

One root allocator reserves ISS and bug IDs on origin with atomic Git ref creation, immutable
identity metadata and exact remote read-back. Bug reservations share the frozen allocator's
namespace, preserving occupied legacy numbers. Issues gain a stable title independent of evidence.
The root checks validate reservation refs and every record in origin/main, HEAD and the working
tree. A separate audit checks unrelated wip branches. Temporary-repository controls cover exclusion across clones,
branch inheritance, offline refusal, legacy import and the real pre-push reservation tier.

Migration is explicit: the operator resolves the conflicts computed in the current backfill
plan, reviews that plan and publishes its reservations in one atomic transaction. No reservation refs are pushed by the writer; the live gate refuses
until backfill and branch reconciliation finish. See docs/ISSUE_IDS.md for the operator commands.

The backfill dry run also refused independent ISS-0018 introductions on five advertised wip heads;
the operator must reconcile those before publication. No collision exemption is installed.

Round 2: identity survives rebases, stacked branches, detached trains and evidence edits.
Enforce reservations on all records in origin/main, HEAD and the working tree; audit unrelated
remote drafts separately. Backfill and verify reservation refs before landing enforcement (exact operator
order in docs/ISSUE_IDS.md). Handle remote-only legacy objects, deleted files and malformed bug
records; cover both metadata namespaces and mixed hook tiers, Python sandbox makers, and issue
ledger append conflicts. No reservation refs were published by the writer.

Round 5: the same allocator owns roadmap R-NNNN subjects under refs/reserved/roadmap/NNNN.
One roadmap-row parser serves reserve, check, backfill and audit. Every main, HEAD and working-tree
ISS, BUG and R identity requires its exact reservation; collisions name the reservation ref or
main row, and unreadable authoritative inputs are INCOMPLETE. Controls cover stale ISS-0020 and
R-row reuse before and after integration, concurrent R allocation, metadata, bootstrap and valid
identity transfer. Authoritative backfill covers all 146 roadmap rows on main without requiring
unrelated drafts to migrate together; the separate whole-inventory audit retains their conflicts.
No reservation refs were published.

Round 6: wording is mutable for every ISS, BUG and R record. Identity follows the reserved
number and authored introduction provenance, preserved across rebase/cherry-pick. Independent
introductions fail on stale branches and integrated candidates even with matching wording.
Backfill compares provenance, never current titles; the authoritative main history remains the
migration baseline. No reservation refs were published.


Round 7: identity is the introducing commit OID, with a separate worktree child identity.
One presence-transition definition covers every reader, including two-step bug creation,
restoration after deletion and merge inheritance. History parsing is independent of user diff
configuration. Plans display introducing OIDs and first-record content and compute branch-specific
coordinator instructions; copied introductions must reconcile onto shared history. Seven new
adversarial controls fail on round 6 and pass after the fix. The whole inventory has 24 conflicts;
the authoritative plan has none. The live reservation gate still blocks activation pending legacy
backfill. No reservation refs were published.
