# Project identity reservations

`python3 -I scripts/ids/allocate.py` is the shared online allocator for ISS, BUG and roadmap R identities
(ISS-0034; structural owners R-0542 and R-0561). Never infer the next number from local files.
The frozen legacy allocator is not a filing entry point.

```sh
python3 -I scripts/ids/allocate.py reserve iss 'Short immutable title'
python3 -I scripts/ids/allocate.py reserve bug 'Short immutable title'
python3 -I scripts/ids/allocate.py reserve roadmap 'Short immutable subject'
```

Run from the writer's local worktree branch (`NAME` or `wip/NAME`). The shared
`scripts/ids/branch.py` policy used by `worktree-new.sh` records it as `wip/NAME`. Copy the returned ID and exact reserved title into the
`ISSUES.tsv` title column, `# Bug NNN: Title` heading, or the second (subject) cell of a
`| R-NNNN | Subject | ... |` row in `ROADMAP.md`. Writers never pick IDs. If a coordinator must
allocate for a writer, leave an uncommitted `ISS-XXXX` or `R-XXXX` placeholder and report it;
a placeholder is never a valid record or a passing gate state. The reservation binds ID, title and
reserving branch. The branch is immutable provenance, not an ancestry requirement: derived,
stacked, rebased, cherry-picked and detached train checkouts all carry the same identity. Status,
evidence, acceptance obligations and closure fields may evolve. Roadmap subjects are immutable
identity metadata, including Markdown spelling. The shared `scripts/ids/roadmap.py` parser owns
all reads of task rows for allocation, snapshots, working trees, backfill and audit: the first
cell declares the ID, the second declares its subject, and prose/dependency references do not
declare identities. Duplicate IDs, malformed numeric IDs and empty subjects are refused. A different title for the same ID is a collision; a
separate issue needs a new reservation. `--number N` permits a coordinator-assigned unused number
through the same atomic protocol, without overwriting anything.

ISS refs use `refs/reserved/iss/NNNN`; roadmap refs use `refs/reserved/roadmap/NNNN`; bugs use `refs/bug-reservations/NNN`, sharing the historical
bug namespace. Unused legacy reservations remain burned. New bug files use
`docs/bugs/NNN_description.md`; frozen files remain under `legacy/docs/bugs/`. Numeric filenames
with fewer than three digits or a missing underscore, missing/mismatched Bug headings, and
duplicate bug numbers across either directory are refused. Documentation files are not records.

## Reservation protocol and recovery

A reservation is a parentless empty-tree commit with schema-1 JSON recording ID, immutable title,
reserving branch, holder, timestamp and nonce. Creation uses an expected-absent exact ref lease;
one concurrent creator wins. Every publication uses an atomic push and exact leases, followed by
an exact remote OID read-back for every ref. Hooks refuse deletion, malformed metadata and schema
reservation replacement. Protect these namespaces against deletion and forced update on origin:
client hooks cannot constrain an administrator who bypasses them. Origin must support atomic
pushes and honour leases.

Before pushing, the allocator prints and saves a recovery receipt under
`.git/id-reservation-attempts/` containing old/new OIDs and complete commit bytes. If transport
fails but every remote ref matches, publication succeeded. Partial publication is refused. If
origin cannot be read, the outcome is unknown: preserve the receipt, do not file or reserve a
replacement, and reconcile the exact OIDs when origin returns. Never delete a reservation to
recover. Failed filing leaves the identity held so the same record can be completed later.

Legacy conversions retain `legacy_oid`, require its exact lease, and fetch the remote old object
without changing caller refs or FETCH_HEAD when the pre-push verifier needs it. Schema-1
reservations cannot be converted again. Planning and checking fetch into a private bare repository;
they never update caller refs or notes. Temporary Git operations clear repository-locating variables, including when invoked from a
hook, while preserving operator transport configuration. Private snapshots disable hooks and
signing and inherit transport/authentication settings from the caller, including repo-local settings. Origin must be reachable.

## Enforcement and remote audit

`allocate.py check` validates every reservation ref as schema-1, every record on fresh
origin/main (including the frozen bug ledger), every record committed in HEAD, and every record
in the working tree independently, for all three kinds. All must have reservations with exact titles/subjects. Collision diagnostics name the exact
reservation ref or origin/main file and row ID. An unreadable origin/main or reservation object
is INCOMPLETE (exit 2), never PASS; a proven identity violation is refused (exit 1). Local main
cannot replace the origin/main snapshot, and unstaged edits cannot mask committed records.
Legacy bug reservation commits are accepted only as backfill inputs, never as passing gate state.

The tools enforce rollout order: backfill and metadata publication work before enforcement lands;
the enforcement branch cannot pass its landing gate until origin/main and its own complete records
are reserved. Main's old tools keep operating during preparation. After activation there are no
unreserved historical-ID exemptions. Deleted files are absent only from the tree being checked.

The normal gate does not judge unrelated wip branches. `reserve` takes the next number above origin/main records and reservation refs. Unreserved
wip drafts have no numbering authority and cannot exhaust or claim numbers. Malformed main
records refuse allocation; unrelated drafts are recorded as audit errors without crashing.
The authoritative reservation namespace provides mutual exclusion. A stale unreserved wip branch
therefore cannot stop another branch's checks or reservations.

`allocate.py audit-wip` separately checks all advertised main/wip records against reservations.
`backfill --output PLAN` produces a nonpublishing operator plan for that same inventory plus the
current tree and unused legacy reservations. `--scope authoritative` instead plans origin/main
and the current worktree, the same records the activation gate requires. This is the bootstrap
path: stale unrelated drafts need not migrate on the same day. It never exempts any main, HEAD
or worktree record from the live gate. The default whole-inventory plan and `audit-wip` expose
draft conflicts separately; each writer reconciles its draft before passing its own gate. Local
main cannot replace origin/main in either plan scope. Explicit titles define identity. For pre-title
rows, backfill recovers first-introduction titles from history. Distinct remaining titles require
operator reconciliation; date/area signatures and majority votes never establish identity.
The local branch replaces its remote inventory entry rather than being counted twice.
Detached allocation or backfill requires `id.branch` or exactly one matching advertised wip HEAD;
an unattributable or ambiguous HEAD is refused, never silently omitted.
Main is preferred for initial provenance; existing reservation provenance never
changes. The five ISS-0018 copies represent one evolving issue, not five introductions.

## Exact operator rollout order

1. Have writers stop hand allocation. Use the updated tools and hook from this branch in a full
   clone **before enabling enforcement on main**. Planning and audit are read-only. Coordinate the
   known renumberings: acceptance-shim ISS-0017 becomes ISS-0033, spec-* retains ISS-0017, lexer
   becomes ISS-0032. Reconcile differing explicit titles; do not retire a rebased head merely for
   lacking shared introduction ancestry.
2. Produce the authoritative backfill plan with the commands below. Inspect every conflict, title, provenance,
   old OID and legacy import. Resolve conflicts with the owning writers, then regenerate until
   backfill exits 0. Inspect that every ISS, BUG and R record on origin/main and this branch is represented by a
   matching reservation or update, including all 146 current main roadmap rows. No planning command publishes refs.
3. From that updated clone, the operator explicitly runs `publish-plan`. Its metadata-aware hook
   handles bootstrap without running source gates. This is the first publication of reservation
   refs, **before this enforcement branch lands**. Publication refuses a changed advertised head
   set, uses atomic exact leases and verifies all remote OIDs. Preserve the plan and receipts.
4. Run the separate whole-remote audit and the branch gates. Record and reconcile conflicting
   active heads before declaring the whole draft inventory migrated. This separate audit may
   fail while the authoritative bootstrap and a clean branch gate pass. Old draft heads may remain unreserved
   without blocking other branches, but all records on a branch must be reserved before it can pass a landing
   gate. Regenerate plans after remote head changes.
5. Only after backfill and verification, land this enforcement branch through the normal train.
   Thereafter every identity in main, HEAD and the working tree is checked on every run. Writers reserve
   before filing; run the independent audit periodically to discover stale/manual draft records.

```sh
nix develop .#gates --command python3 -I scripts/ids/allocate.py backfill --scope authoritative --output /private/path/ids.json
# Inspect the plan; stop on a nonzero exit. Nothing above has published anything.
nix develop .#gates --command python3 -I scripts/ids/allocate.py publish-plan /private/path/ids.json
nix develop .#gates --command python3 -I scripts/ids/allocate.py audit-wip
nix develop .#gates --command bash scripts/checks/id_reservations.sh
nix develop .#gates --command bash scripts/checks/issues.sh
```

A successful unchanged plan is idempotent. A conflicting plan cannot publish. No reservation refs
were pushed by the round-2 writer; bootstrap publication remains an explicit operator action.

Round-2 read-only inventory on 2026-11-08: backfill exited 0 with 357 planned reservations and
zero conflicts, including the five ISS-0018 heads. This plan was not published and must be
regenerated if the advertised heads change.

ISSUES.tsv conflicts in landing trains use a three-way merge by ID with the same schema validator
as `issues.sh`. Independent row changes are preserved, competing changes are refused, and the
result is sorted and validated before staging. Comments are merged as a separate document value.

Round-5 read-only inventory on 2026-11-09 at origin/main
`9cddd589182910be056ea9a0be4cd2963663a82f`: 146 roadmap rows, 18 issue rows and
323 bug records. The authoritative plan exited 0: 488 planned reservations, zero conflicts,
including all 146 main R rows. The whole-inventory plan exited 1: 495 planned reservations
and 82 conflicts (81 differing R subjects on historical drafts and one malformed draft issue
ledger). These are evidence for coordinator reconciliation, never automatic renumberings or
title substitutions. Plans are read-only and must be regenerated against current advertised
heads before publication. No round-5 backfill was published.
