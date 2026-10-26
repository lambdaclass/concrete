# Test Scripts

This directory contains shell entrypoints for the non-Rust, non-Lean parts of the test system.

Main entrypoints:

- `run_tests.sh`: primary regression runner
- `test_golden.sh`: golden-file verification for `--emit-core`, `--emit-ssa`, and `--fmt`
- `update_golden.sh`: refresh golden baselines
- `test_ssa.sh`: SSA-focused backend checks
- `test_parser_fuzz.sh`: parser fuzzing
- `test_fuzz.sh`: broader structured fuzzing
- `test_perf.sh`: performance-baseline capture and comparison
- `test_mutation.sh`: mutation-testing harness

Supporting helpers:

- `phase3_diagnostic_checks.sh`
- `phase3_report_consistency_checks.sh`
- `lib/bounded.sh`: time budgets. A timeout is INCOMPLETE, never a failure and never a pass: the
  command is re-run once alone with a larger budget, and a second timeout is reported as incomplete
  (the gate still exits nonzero). Use `bounded_run` for per-test budgets, or `bounded_watchdog SECS`
  and `"${TO[@]}" CMD` for single bounded commands with `bounded_gate_status && [ "$FAIL" -eq 0 ]`
  as the gate's last line; do not call `timeout` directly in a new gate (ROADMAP "Promotion
  pipeline", result vocabulary).

All scripts assume the repository root as their working directory and normalize to it internally.

Gates run only inside the flake's shells: `nix develop .#gates` (what CI uses; no Lean, so elan's
stays on PATH) or `nix develop`. Every gate's first statement sources `lib/gate_env.sh`, which
refuses without the `CONCRETE_GATES_SHELL=1` marker those shells set; `check_gate_hygiene.sh`
enforces that for every `check_*`, `test_*` and `run_*` script and every script a workflow runs.

## Shared ledgers: one owner per fact

Facts that many branches record live in files only their owner edits. The shared views are
generated from them, and `check_ledgers.sh` (run by `check_docs_drift.sh`) fails when a view is
hand-edited or stale.

- **To file a bug, write its record**, starting from `scripts/gen/new_bug.sh "<title>" [--class N]
  [--silent yes|no]`, which reserves the number on origin (`refs/bug-reservations/NNN`, race-safe
  across branches, worktrees and machines) and scaffolds the record, a reproducer and a fragment
  (`check_new_bug.sh` tests it). `docs/bugs/NNN_*.md` holds the bug's status and, in its
  `bug-ledger` block, its summary, its coverage (`fixtures:` or `coverage:`), its structural
  classes, whether it is silent and its exits ([docs/bugs/README.md](../../docs/bugs/README.md),
  "Filing a bug"). `audit_bug_corpus.sh` and `check_bug_filing_complete.sh` read the coverage from
  the records at run time (`scripts/gen/bug_ledger.py corpus`). Do not add entries to the maps in
  `audit_bug_corpus.sh`.
- **To log a change, add a `changelog.d/` fragment** ([changelog.d/README.md](../../changelog.d/README.md)).
  Never edit CHANGELOG.md; fragments are folded into it on main.
- **To add a CI gate, add its command to `ci_gate_inventory.txt`** in the commit that adds it to the
  workflow, once that file exists (sorted, one command per line). `run_ci_gates_local.sh` compares
  it with the workflow, by content.
- **Never edit a generated view**: the bug lists in `docs/bugs/README.md`, the bugs column of
  `docs/bugs/STRUCTURAL_CLASSES.md`'s table, ROADMAP's generated exit bug spans and dashboards,
  and `Concrete/BuildIdentity.lean`. After a rebase that conflicts in one of them, run
  `bash scripts/gen/refresh_generated.sh` and `git add` the result.

`scripts/gen/migrate_ledgers.sh` performs the one-time move from the hand-maintained views to the
records. Until it has run, the generators report "not migrated" and the legacy maps stay in use.

Follow-ups, not yet done:

- `Concrete/BuildIdentity.lean` stays committed. It is embedded at build time and receipts bind
  it, so a branch binary must carry its own sources' identity; regenerating it only at landing
  would leave every branch build stale. Deriving it inside the build would need a Lake pre-build
  step (a `lakefile.lean`). Until then a conflict in it is resolved by `refresh_generated.sh`.
- `docs/bugs/discovery.tsv` (when present) still lists bugs per probe batch, so two branches
  filing bugs from one batch edit one row. The record's `silent:` line is the owner of silent/loud
  and the ledger checks the two agree; moving batch membership into the record is the next step.
- `changelog.py --fold` orders newest first within one fold only; merging by date across folds
  is not implemented.
- The migration keeps one README line per bug when a bug was listed twice and NOTEs the other;
  the lander reconciles those by hand.
