# Project identity reservations

`python3 -I scripts/ids/allocate.py` is the shared online allocator for ISS, BUG and roadmap R identities
(ISS-0034; structural owners R-0542 and R-0561). Never infer the next number from local files.
The frozen legacy allocator is not a filing entry point.

```sh
python3 -I scripts/ids/allocate.py reserve iss 'Short initial title'
python3 -I scripts/ids/allocate.py reserve bug 'Short initial title'
python3 -I scripts/ids/allocate.py reserve roadmap 'Short initial subject'
```

Run from the writer's local worktree branch (`NAME` or `wip/NAME`). The shared
`scripts/ids/branch.py` policy used by `worktree-new.sh` records it as `wip/NAME`. Copy the returned ID and exact reserved title into the
`ISSUES.tsv` title column, `# Bug NNN: Title` heading, or the second (subject) cell of a
`| R-NNNN | Subject | ... |` row in `ROADMAP.md`. Writers never pick IDs. If a coordinator must
allocate for a writer, leave an uncommitted `ISS-XXXX` or `R-XXXX` placeholder and report it;
a placeholder is never a valid record or a passing gate state. The reservation records the ID,
initial title and reserving branch as immutable provenance. The current title, subject, status,
evidence, acceptance obligations and closure fields may evolve. All three record kinds use
introduction provenance rather than wording to distinguish edits from new items.
The shared `scripts/ids/roadmap.py` parser owns all reads of task rows: the first cell declares
the ID, the second its current subject; prose/dependency references do not declare identities.
Duplicate IDs, malformed numeric IDs and empty subjects are refused. A
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
in the working tree independently, for all three kinds. All must have schema-1 reservations and consistent introduction provenance. Collision diagnostics name the exact
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
main cannot replace origin/main in either plan scope. Initial titles are preserved in reservation
metadata and never constrain later wording. First-introduction provenance establishes record identity;
date/area signatures and majority votes do not.
The local branch replaces its remote inventory entry rather than being counted twice.
Detached allocation or backfill requires `id.branch` or exactly one matching advertised wip HEAD;
an unattributable or ambiguous HEAD is refused, never silently omitted.
Main is preferred for initial provenance; existing reservation provenance never
changes. The five ISS-0018 copies represent one evolving issue, not five introductions.

## Exact operator rollout order

1. Have writers stop hand allocation. Use the updated tools and hook from this branch in a full
   clone **before enabling enforcement on main**. Planning and audit are read-only. Coordinate the
   known renumberings: acceptance-shim ISS-0017 becomes ISS-0033, spec-* retains ISS-0017, lexer
   becomes ISS-0032. Reconcile independent introductions; rewording an inherited row needs no reconciliation.
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
ledger). Round 6 supersedes those title-based conflicts with introduction-provenance checks. Plans are read-only and must be regenerated against current advertised
heads before publication. No round-5 backfill was published.

## Edits and independent additions

An introduction is a state transition: the child contains an ID that no parent supplies,
or restores an ID along a parent lineage that previously contained and then deleted it.
A normal merge may inherit an ID from another parent when the missing parent has never
contained it. Restoring it after deletion is a new introduction even when another merge
parent still contains it. `introduced_keys` owns this definition for every reader.
Historical presence uses one tolerant reader for ISS rows, roadmap declarations and bug
record paths, including a bug path created before its heading. Current main, HEAD and
working tree still undergo full current-schema validation.

Identity is the introducing commit OID. First-introduction record content is diagnostic
evidence, never an equality key. Copying a commit through rebase, cherry-pick or `commit -C`
creates another introduction OID; identical authorship, message or payload cannot collapse
it into main's introduction. A copy must reconcile onto main's actual introduction history.
The history reader uses commit and tree output without parsing patch paths, so diff prefixes,
renames, colors and external diff configuration cannot change identity. It examines every
reachable introduction and actual merge parent, so selecting one row during a merge cannot
erase independent introductions.

The working tree is a child state of HEAD with a distinct `worktree:<HEAD OID>` introduction
identity for additions absent from HEAD. Both validation and backfill use that child history;
a deletion committed in HEAD followed by an unstaged re-add is a new introduction.

For a number with introductions on origin/main, a branch must inherit at least one of those
introductions and carry no independent introduction. Changing the subject or moving the row to
a different line keeps the identity. A branch based before main introduced ISS-0020 that adds
its own ISS-0020 has a different introduction and fails, even with identical wording. A merged
candidate carrying both introductions fails too. These rules apply equally to ISS, BUG and R.
Main's existing introduction history is the baseline; this migration does not retrospectively
split historical main records.

For a reserved number absent from main's introduction history, a new addition belongs to the
reserving branch. Other branches may inherit an introduction already present on that owner's
advertised head. A detached candidate can inherit this provenance but cannot claim a new
addition merely by changing its text. Without a reservation, a backfill plan refuses multiple
independent introductions across branches, even if their titles match. A reservation's title
remains the original metadata; it is never rewritten to match the current row.

No extra row-position rule is needed: each kind has its own number namespace, and each current
ledger refuses duplicate IDs. Physical line numbers, sorting and legitimate bug-file moves do
not change identity. A delete followed by a later re-add is a new introduction and is checked
as such. Rebased, cherry-picked and squashed copies must reconcile with the actual
introducing OID when main already owns the number. Matching text alone cannot establish identity.

## Round-6 validation and inventory (2026-11-09)

At `56019fb18f137a74c2c4d391877113f881a600df`, with origin/main
`9cddd589182910be056ea9a0be4cd2963663a82f` and 115 advertised main/wip heads,
the whole-inventory plan exited 1: **578 planned reservations, one conflict**. All 81
round-5 roadmap wording conflicts are gone, as are the rebased ISS-0018 ambiguities.
The authoritative plan exited 0: **488 planned reservations, zero conflicts**.
Neither plan was published.

The remaining conflict is **ISS-0015 on `wip/sandbox-isolation-r2`**. Its introduction is
`d11ebc6829a70d3485feb6189f39500b0e462383`; main introduced the record through
`0d1a1b84a665b36cd413a9f7d560d9b993a9c9a8`, whose message describes squashing that
branch. The squash changed the authored timestamp and message, so the original authored
provenance was lost. Matching wording cannot repair that missing provenance. This stale
branch needs reconciliation against main's existing record; it is not a roadmap subject
conflict and does not block the authoritative bootstrap. No automatic renumbering or
wording constraint is applied.

All checks below ran bounded inside `nix develop .#gates` with
`PYTHONDONTWRITEBYTECODE=1`, against the commit above:

| check | exit status |
| --- | --- |
| all reservation controls (including rounds 3–6) | 0 |
| `issues.sh` | 0 |
| `review_tool.sh` (temporary repositories and mock reviewers) | 0 |
| `sandbox_isolation.sh` | 0 |
| full `run.sh` | 1 |

The full suite's only failed check was `id_reservations.sh`: its controls exited 0,
then its live check exited 1 because `refs/bug-reservations/200` still contains legacy
metadata requiring backfill. All other root checks passed. This is the enforced rollout
block; it is not a passing full suite. No source-gate exemption was added.

Before/after controls reproduced all four agy findings against `c3a3f7aa2` (exit 1)
and pass in the current controls (exit 0). Round-6 controls also fail against
`458658b4e` (exit 1), reproducing rejection of legitimate inherited wording changes.
They cover independent ISS and R additions on stale branches and integrated candidates,
identical-text reuse, two unreserved introductions, rewording inherited records,
`cherry-pick -x` provenance, bug-file moves and examples that must not declare identities.
The example-row control separately fails against `36f759526` and passes after the fix.
No reviewers were invoked, and no caller notes or reservation refs were modified.


## Round-7 reproduction, validation and inventory (2026-11-09)

The round-6 findings were **CONFIRMED** first at `4d42c0f2c3f286271ebb5a9190cf38eda284b728`
in temporary repositories. `round7_controls.py` exited 1, with seven failing controls:

| finding | baseline evidence | control after fix |
| --- | --- | --- |
| 1: working-tree re-add | check exited 0 after committed deletion and unstaged replacement | `worktree_readd` rejects it |
| 1: working-tree collision | backfill exited 0 with zero conflicts for two introductions | `worktree_collision` requires one conflict |
| 2: copied restoration | check exited 0 after deletion and cherry-pick of the original introduction | `cherry_readd` rejects it |
| 2: merge restoration | check exited 0 when another parent retained the deleted ID | `merge_readd` rejects it |
| 2: two-step bug | check exited 0 after path-first, heading-second introduction and merge | `two_step_bug` rejects it |
| 3: hostile diff config | merged local-main collision passed check, exit 0 | `hostile_config` rejects collision and accepts inherited edits |
| 4: diagnostics | plan omitted both introducing OIDs and gave hardcoded ISS-0017 advice | `diagnostics` requires OIDs, branches and computed reconciliation/reservation instructions |

The hostile global configuration enables `diff.noprefix`, `diff.mnemonicPrefix`, copy
rename detection, always-on color and an external diff that exits unsuccessfully.
The full earlier control set is retained. The earlier cherry-pick control now requires
refusal of the distinct OID, consistent with the requested identity rule.
Round-3 controls against `c3a3f7aa2` exited 1, reproducing all four earlier findings:
authoritative/main/HEAD coverage, worktree branch policy, structured TSV merge and metadata
validation. Those controls pass on the round-7 implementation; their fixes were already in HEAD.

Code validation is recorded against `86db602e4` (the code commit preceding this documentation).
Every command ran bounded inside `nix develop .#gates` with `PYTHONDONTWRITEBYTECODE=1`.

| check | exit status |
| --- | --- |
| all reservation controls, rounds 3–7 | 0 |
| `issues.sh` | 0 |
| `review_tool.sh` (mock reviewers in temporary repositories) | 0 |
| `sandbox_isolation.sh` | 0 |
| full `run.sh` | 1 |

The full suite's only remaining failed check is `id_reservations.sh`: its controls exit 0,
but its live check exits 1 because legacy bug reservation metadata still requires backfill.
This is an activation block, not a passing full suite. An initial run at `a9b303624` also
failed the structural sandbox scan for the new control's missing explicit entry call;
`86db602e4` adds that call, and the sandbox gate passes.

The whole-inventory plan exits 1: **555 planned reservations, 24 conflicts**, across 115
advertised main/wip heads. The authoritative plan exits 0: **488 planned reservations,
zero conflicts**. Main was `9cddd589182910be056ea9a0be4cd2963663a82f`.
Neither plan was published. The increase from round 6's one conflict is the deliberate
consequence of commit-OID identity: copied introductions no longer collapse by content.

Each conflict is explained below. Families identify the actual introducing OIDs and
branches; the table lists every conflicted ID separately.

- **A:** main and its inheritors use `33374704fb9f4ce94a07359204d29de6bb332223`;
  `wip/landing-audit-2026-11-06` uses `c7cf9471a8655e9f4265a31b79c5448ae14eacc0`.
  First records match exactly. Reconcile the old landed-copy branch onto main or retire
  its stale ref; matching content does not authorize treating two OIDs as one.
- **B:** main uses `0d1a1b84a665b36cd413a9f7d560d9b993a9c9a8`;
  `wip/sandbox-isolation-r2` uses `d11ebc6829a70d3485feb6189f39500b0e462383`.
  Main's commit describes the squash of this branch. Reconcile or retire that pre-squash
  ref; this is the already-landed sandbox issue, not an instruction to renumber it.
- **C:** `wip/record-chain-fix`, `wip/record-signoffs-r` and `wip/roadmap-sweep-r`
  use `d323abb79fd6e74ba8d6a1b3c3f47d6e4f3a065f`; `wip/record-signoffs` and
  `wip/roadmap-sweep` use `a13c363b57a52b21be1fa1e0661d0ebff90a9d67`.
  These first records match exactly but have distinct OIDs. The coordinator must choose
  the shared history and reconcile dependent drafts onto it, or retire superseded copies.
- **D:** `wip/record-chain-fix` and `wip/roadmap-sweep-r` use
  `4334a2b0970d49a381defbe5fd73a3430e5c7abb`; `wip/roadmap-sweep` uses
  `95d808c792c1cbf6e440e25e813a4d3370ba5c93`. These first records also match
  exactly. Reconcile the drafts onto one introduction history or retire superseded copies.

If comparison establishes that any pair actually represents distinct work, the coordinator
must reserve a new ID and renumber that work before retrying backfill. The plan contains
first-record content, full OIDs, branch attribution and per-ID resolution instructions.
No content-based equivalence or automatic retirement/renumbering is applied.

| conflicted ID | introduction family and cause |
| --- | --- |
| ISS-0012 | A: distinct introducing OIDs for the copied record |
| ISS-0013 | A: distinct introducing OIDs for the copied record |
| ISS-0014 | A: distinct introducing OIDs for the copied record |
| ISS-0015 | B: distinct introducing OIDs for the copied record |
| ISS-0018 | C: distinct introducing OIDs for the copied record |
| R-0628 | C: distinct introducing OIDs for the copied record |
| R-0629 | C: distinct introducing OIDs for the copied record |
| R-0630 | C: distinct introducing OIDs for the copied record |
| R-0631 | C: distinct introducing OIDs for the copied record |
| R-0632 | D: distinct introducing OIDs for the copied record |
| R-0633 | D: distinct introducing OIDs for the copied record |
| R-0634 | D: distinct introducing OIDs for the copied record |
| R-0635 | D: distinct introducing OIDs for the copied record |
| R-0636 | D: distinct introducing OIDs for the copied record |
| R-0637 | D: distinct introducing OIDs for the copied record |
| R-0638 | D: distinct introducing OIDs for the copied record |
| R-0639 | D: distinct introducing OIDs for the copied record |
| R-0640 | D: distinct introducing OIDs for the copied record |
| R-0641 | D: distinct introducing OIDs for the copied record |
| R-0642 | D: distinct introducing OIDs for the copied record |
| R-0643 | D: distinct introducing OIDs for the copied record |
| R-0644 | D: distinct introducing OIDs for the copied record |
| R-0645 | D: distinct introducing OIDs for the copied record |
| R-0646 | D: distinct introducing OIDs for the copied record |

No reviewer was invoked, no backfill was published, and no caller/remote reservation refs
or git notes were created or modified. No issue rows, owner decisions or roadmap rows were edited.
