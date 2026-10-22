# Landing trains

Status: operational procedure (ROADMAP R-0539, measurement R-0547)

Ready branches land together as one **train**: one integrated candidate, one CI run, and
bisection by member only when that run is red. `scripts/land/train.sh` does what an orchestrator
used to do by hand, and refuses the mistakes that hand-run trains made on 2026-10-22.

## The cycle

```sh
scripts/land/train.sh build  <name> <ref>...   # train/<name> = origin/main + each ref, --no-ff, in order
scripts/land/train.sh check  <name>            # fast surface + impact-selected gates (+ remote full set)
scripts/land/train.sh ci     <name>            # push ci/<name>, dispatch CI, record the run id
scripts/land/train.sh watch  <name>            # poll to a verdict; failing jobs' log tails
scripts/land/train.sh land   <name>            # verify, then push the exact SHA to origin and lambdaclass
scripts/land/report.sh                         # median time to land, per-stage medians
```

`--dry-run` before the subcommand prints pushes, dispatches and the remote run instead of doing
them. `land --dry-run` still performs every verification, so it is the way to ask "would this land?".
`train.sh status <name>` prints the train's state and log.

### build

- Creates `train/<name>` from `origin/main` (after a fetch) and merges each ref with `--no-ff`.
  The merge message names the member and its SHA. A ref that is already in main is skipped.
- `--force` rebuilds an existing train. That is the normal response to a moved main.
- Conflicts are resolved mechanically only in three kinds of file
  (`scripts/land/resolve_conflicts.py`):

  | kind | files | resolution |
  |---|---|---|
  | append-only ledgers | `CHANGELOG.md`, `scripts/tests/audit_bug_corpus.sh`, `docs/bugs/discovery.tsv`, `docs/project/candidates.tsv` | union of both sides' lines, only when both sides inserted lines (the diff3 base of every hunk is empty); in the corpus, only map entries inside a `declare -A` map |
  | whole generated files | `Concrete/BuildIdentity.lean` | regenerated on the train |
  | generated blocks | any file whose every conflict hunk is inside a `BEGIN GENERATED ... (scripts/gen/<generator>; ...)` block whose generator the train will run | regenerated on the train |

  A generated view is resolved only when its generator is in the regeneration plan that runs after
  the last merge. A generator that merely exists is not enough. Any other conflict, including prose
  next to a generated block and two edits of one ledger line, aborts that merge. The script
  names the files and exits 3. The train keeps the members merged before it.
- After every merge, `resolve_conflicts.py corpus-keys` refuses a duplicate key in the corpus maps,
  and a bug listed in both `BUG_TEST_MAP` and `SKIP_BUGS`. bash keeps the last of two equal keys
  without a word, and a union merge left stale OPEN entries for fixed bugs on 2026-10-22.
- After the last merge the generated views are regenerated once:
  `scripts/gen/refresh_generated.sh` when it exists (branch `ledgers`, R-0542), otherwise
  `bug_dashboard.py --write`, `candidate_table.py --write` and `build_identity.sh`, each when present.
  Any change becomes one `regenerate generated views` commit. A generator's refusal stops the build.
- After every git operation the script checks for `MERGE_HEAD`, `CHERRY_PICK_HEAD`, `REVERT_HEAD`
  and a rebase in progress, and stops if one exists. It never continues past a stopped operation.
  A dirty tree is refused before anything starts.

### check

- Runs the pre-push hook's own selection (the R-0022 fast surface gates plus the touched areas)
  over exactly the range the `ci` push will present: `train/<name>` at `base..head` on `ci/<name>`.
  The hook therefore writes its validation record for that range, and the `ci` push reuses it
  instead of running the gates again.
- If `scripts/tests/select_gates.sh` (R-0513) exists and the hook does not already call it, its
  output is run as well. The interface is `select_gates.sh --base <sha> --head <sha>`, printing one
  gate command per line, which runs through `run_ci_gates_local.sh --first`.
- If `~/bin/concrete-remote` exists, the full gate set runs remotely at the same time
  (`concrete-remote full-gates <sha> <branch>`, R-0541). Its exit status counts toward `check`.
  `--no-remote` skips it.
- Runs inside `nix develop` unless already in the dev shell. `LAND_NO_NIX=1` disables this.

### ci

- Refuses unless `check` passed at this exact HEAD. Each `check` run first withdraws the previous
  pass, so a failed re-check leaves no pass behind. `--skip-check` overrides that, and the log
  records the check as `skipped`.
- Pushes in two steps:
  1. `ci/<name>` is reset to the train's base. The base is main's own commit and has already been
     published, so this push skips the hook.
  2. The train is pushed on top with the hook running. Its range is `base..head`, so the record
     written by `check` is reused.

  Without the first step, a new branch has no remote base and the hook would select gates by
  diffing the whole tree.
- Dispatches `gh workflow run CI -R unbalancedparentheses/concrete2 --ref ci/<name>`. It finds the
  run whose head SHA is the train's, records the run id and prints the run URL.

### watch

Polls until the run completes. A timeout is reported as `incomplete`, never as a result. The
command then lists every job that did not succeed, by tier, and prints the log tail of each failed
job (`gh api repos/<repo>/actions/jobs/<id>/logs`).

A run at another SHA, or a run with no landing-tier jobs, is no verdict: `watch` exits 1 and logs
`refused`.

**Campaign-only failure.** The campaign tier is the mutation campaign, the fuzz campaign and the
proof replay (`LAND_CAMPAIGN_JOBS`). When it is the only part that failed, `watch` says so and
exits 0: the campaign gates stage (c) qualification, not merges. The failure is still printed and
logged.

### land

`land` takes the same repository-wide lock as `push-both.sh` first, then checks every condition below
before anything is pushed:

- the CI run's `headSha` is the local HEAD, and the run is completed;
- every landing-tier job concluded `success`, and a run with no landing-tier jobs is refused;
- `origin/main`, read with `ls-remote`, is still the train's base;
- the mirror's main is an ancestor of the train, so no half-landing happens.

If main moved, the command exits 4 and prints the rebuild command. The result described a
candidate that no longer integrates main, so the train is rebuilt, checked and run again.

The push sequence:

1. Pushes the exact SHA to `origin main` through the hook, which reuses the `check` record. The
   push carries `--force-with-lease=refs/heads/main:<base>`, so it succeeds only while main is still
   the base, even against a publisher that does not take the lock.
2. Pushes the same SHA to `lambdaclass main`, with a lease on the value read before the first
   push. Both remotes are required (the owner's decision of 2026-10-22, which supersedes the
   2026-07-31 "stop pushing to lambdaclass" note in `push-both.sh`): a missing `lambdaclass` remote
   is refused before anything is pushed. Following `push-both.sh` policy, this push runs with the hook
   skipped, because the identical commit was just accepted by the primary's hook. If it fails after
   the primary landed, the command says so and exits non-zero.
3. Checks both remotes with `ls-remote`.

A remote named `fork` is refused as primary or mirror. `--no-mirror` (or `LAND_MIRROR=`) lands on
origin only. That is a deliberate exception, and the landing is not complete until the same SHA
reaches `lambdaclass`. `push-both.sh` follows the same rule: it mirrors to `lambdaclass` by default.

### bisect

```sh
scripts/land/train.sh bisect <name> --test '<command that fails on the red train>'
scripts/land/train.sh bisect <name> --ci        # each subset through check, ci and watch
```

Halves the member list until one member remains. Each subset is its own train (`<name>-b<k>`),
built from the members' recorded SHAs. Its result is logged against that train's head. The suspect
is then built alone. If it is green alone, the failure is an interaction between members and the
report says so. The command prints the rebuild without the culprit. A member removed by bisection
keeps the failure attributed to it.

## Where the record lives (R-0547)

Every subcommand appends one row to the train's log:

- the time in UTC;
- the train, stage, base and head;
- the members, as `ref@sha`;
- the run id, result and duration;
- a note.

Results use the promotion pipeline's vocabulary:

- `passed`, `failed` and `skipped`;
- `incomplete` for no verdict;
- `refused` and `conflict` for a refusal by the script.

**Decision: the log is kept local, one file per train, in the git common directory**
(`<git-common-dir>/concrete-landing/log/<train>.tsv`). Train state lives next to it, in
`<train>/train.env`. The reasons:

- **Committing it to the train changes the candidate.** A row appended to the train after CI ran
  would land a SHA that CI never validated. The `land` row cannot exist before the land happens.
- **One shared TSV conflicts.** Every train would append to the same file, which recreates the
  ledger conflicts R-0542 removes. One file per train cannot conflict.
- **The common directory is shared by every worktree.** Any worktree can watch or land a train
  built in another, and `report.sh` sees all of them.

To publish the record, `report.sh --export docs/project/landing_log/` copies the per-train files
into the tree for a documentation-only commit. Per-train files never conflict on rebase. That
directory is what R-0547's generated median, next to the candidate table, will read.

## Configuration

| variable | default | meaning |
|---|---|---|
| `LAND_REPO` | `unbalancedparentheses/concrete2` | repository for `gh` |
| `LAND_REMOTE` | `origin` | primary remote |
| `LAND_MIRROR` | `lambdaclass` | required mirror, pushed after the primary; a missing remote is refused; empty (or `--no-mirror`) skips it deliberately |
| `LAND_CAMPAIGN_JOBS` | `^(Gate mutation\|Weekly clean-checkout proof replay\|Nightly differential fuzz)` | job names in the campaign tier |
| `LAND_STATE_DIR`, `LAND_LOG_DIR` | under the git common directory | state and log location |
| `LAND_NO_NIX` | unset | `1` never re-enters `nix develop` |
| `LAND_GH`, `LAND_HOOK` | `gh`, `.githooks/pre-push` | test seams |

## Tests

`scripts/tests/check_landing_script.sh` runs in CI's build-free grammar job. It drives the real
script against a throwaway repository with bare-repo remotes and a fake `gh`. It covers:

- every resolution route;
- every refusal: a real conflict, prose next to a generated block, a duplicate corpus key, a merge
  or rebase already in progress, a dirty tree, an existing train, a head-SHA mismatch, a red
  landing job, a moved main and a remote named `fork`;
- the check → ci → watch → land pipeline, with both dry-runs;
- bisection;
- the report.
