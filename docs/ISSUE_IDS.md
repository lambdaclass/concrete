# Issue and bug IDs

The root allocator is `python3 -I scripts/ids/allocate.py`. Every new ISS and bug ID goes through
it; never choose the next number from a file. This is the structural repair for ISS-0034,
owned by R-0542 (one source for ledger identities) and R-0561 (the new bug ledger).
The frozen `legacy/scripts/gen/new_bug.sh` is historical evidence, never a filing entry point.

From the writer's own `wip/*` branch:

```sh
python3 -I scripts/ids/allocate.py reserve iss 'Short immutable title'
python3 -I scripts/ids/allocate.py reserve bug 'Short immutable title'
```

The returned ID is held on origin before it is printed. Copy the exact title into the issue's
`title` column or the bug record's `# Bug NNN: Title` heading. Evidence, status and closure conditions
can change independently. Keep the reservation branch name as provenance; descendants inherit
records through Git ancestry, and after landing main provides that ancestry even if the original
branch is deleted. Do not reuse an abandoned ID, rename an identity or transfer its reservation.
A genuinely different issue needs a fresh ID. `--number N` is for a coordinator-assigned number;
it still requires an unused number and wins the same atomic reservation as automatic allocation.

ISS and bug numbers are typed identities: `ISS-0034` and `BUG-034` are different IDs. Both use the
same allocator and protocol. ISS refs are `refs/reserved/iss/NNNN`. Bug refs remain
`refs/bug-reservations/NNN`, the *same namespace* used by the frozen allocator: a legacy reservation
and a root reservation therefore compete for exactly the same ref, never parallel authorities.
The automatic bug sequence skips every existing bug record and every reservation, including
unused legacy reservations; gaps are never reused. New bug records belong at `docs/bugs/NNN_*.md`
when the ledger returns; frozen records stay at `legacy/docs/bugs/NNN_*.md` unchanged.

## Guarantee and failure handling

A reservation is a parentless empty-tree commit with a JSON message recording schema, typed ID,
immutable title, branch, holder email and host, UTC time and a random nonce. No code is published.
Creation uses `--force-with-lease=<ref>:` (expected absent). Git's ref transaction allows exactly
one concurrent creator for a given ID. The loser refuses and must rerun from a fresh snapshot;
it never files under the losing number. The pre-push metadata tier validates this shape, refuses
deletions and refuses replacing a schema reservation. An administrator who bypasses client hooks
can still change refs; protect these namespaces on the authoritative server against deletion and
forced update. This protocol assumes origin honours Git's leases and atomic ref transactions.

Every push is followed by an exact remote OID read-back. If the response was lost but the ref is
ours, allocation succeeds. If another writer owns it, allocation fails. If origin cannot be read,
the outcome is unknown: **do not file or retry with a different number**. Read the reservation
ref and inspect its JSON metadata when origin returns; retain the orphan commit OID from the Git
object store if reconciling an interrupted invocation. No rollback deletes a reservation. Filing
and reservation are separate: validate filing inputs first; a failed filing leaves the ID held,
and the same branch can finish the record later without allocating again (the failure lesson of
legacy bug 300). There is no offline mode or stale-fetch escape hatch.

The gate `scripts/checks/id_reservations.sh` runs the controls and then validates the work tree
and a fresh snapshot of origin main, all `wip/*` heads and reservation refs. It refuses duplicates
within either ledger, absent reservations, mismatched titles and independent branch introductions
of the same ID (even with identical titles). Shared ancestry permits inherited records. Historical
branches lacking a title column derive their immutable title from the first introduction in Git
history, rather than mutable current evidence. Main/CI integration must inherit each reservation's
owning branch or main; detached candidates whose ancestry cannot be established are refused.
Network failure fails the check. Like any read-only gate, its verdict describes the fetched
snapshot: it does not lock future branch updates. Reservations provide exclusion at allocation;
the gate detects manual violations when they become visible. It is included automatically by
`run.sh` and named in that runner's required set. Fetches use a private temporary bare repository,
so neither caller refs nor notes change. No legacy code or gate runs.

## Operator migration (no refs are pushed by planning)

First have every writer stop hand allocation and publish the coordinated renumberings:
acceptance-shim's ISS-0017 becomes **ISS-0033**, spec-* keeps ISS-0017, and the lexer collision
becomes **ISS-0032**. Fix or retire stale wip snapshots that still introduce conflicting identities;
the gate deliberately includes every advertised wip head. These are planned resolutions, never
an exemption in the gate or an automatic rewrite of somebody else's branch.

After the harness pushes this branch, from its full clone:

```sh
nix develop --command python3 -I scripts/ids/allocate.py backfill --output /path/to/backfill.json
# Review updates, provenance, legacy_oid imports, and conflicts in backfill.json.
# If the command exits nonzero, correct the conflicting branches and regenerate; do not publish.
nix develop --command python3 -I scripts/ids/allocate.py publish-plan /path/to/backfill.json
nix develop --command bash scripts/checks/id_reservations.sh
nix develop --command bash scripts/checks/issues.sh
```

Use a private output path of your choosing. The plan covers every ID in main, origin wip heads,
and this work tree (including the coordinator-assigned ISS-0034), plus unused legacy bug
reservations. Inspect that ISS-0001 through ISS-0034 are all represented by an existing reservation
or a planned update; missing historical IDs must be recovered from their owning writer, never
invented. A conflict writes a diagnostic plan but exits nonzero and publication refuses that plan.
The known ISS-0017 resolution is recorded explicitly in every plan. Historical holders of a legacy
reservation remain auditable through its original commit OID (`legacy_oid`) in the imported JSON;
backfill metadata identifies the operator performing the import, not the original filer.

`publish-plan` refuses a changed origin head set and uses one `git push --atomic` with exact leases
for every new ref and legacy conversion. Origin without atomic-push support is refused. Conversion
of an old reservation requires its exact previous OID; schema reservations are immutable. Read-back
checks every ref, so a failed/ambiguous transaction is never inferred successful from a log message.
A successful plan is idempotent while heads remain unchanged. An observed partial publication
requires operator reconciliation from the retained plan; it is not a pass. Regenerate after any
head or reservation changes. The writer of this change has pushed no reservations; migration is
an explicit operator step, and until it completes the live gate is expected to refuse the tree.
This is operational rollout of the full structural design, not a temporary allocator.
