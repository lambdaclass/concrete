# Gate Profile

Status: measurement record (R-0546 step 1), taken 2026-10-22

This file records where CI time goes, why the slowest gates are slow, and what the build and
compile caches changed. It is a measurement at named SHAs, not a live dashboard. Re-measure before
you cite a number for a different commit.

## Sources

- **Run 39775852735**: `main` at `713778e2`. All required jobs, push-triggered.
- **Run 39764647243**: `ci/validation-improvements` at `62775698`. It also ran the scheduled jobs:
  mutation shards, nightly fuzz, weekly clean-checkout replay and multi-kernel.

Step durations come from `gh run view <id> --json jobs`, with each step's `startedAt` and
`completedAt` at one-second resolution. They were cross-checked against the per-line timestamps in
`gh api repos/unbalancedparentheses/concrete2/actions/jobs/<id>/logs`. The mutation shards (24
jobs of 10 to 85 min) are left out of the per-gate table, because each shard is a campaign rather
than a gate.

## Critical path

On `main` (713778e2), the jobs ran for these wall times:

| Job | Wall time | Steps |
|---|---|---|
| Language-surface gates (Phase 5-6) | **80.3 min** | 173 |
| Proof evidence gate | 54.3 min | 52 |
| Build + main tests (macOS) | 10.7 min | 12 |
| Multi-kernel evidence | 9.7 min | 10 |
| Campaign gates (mint-batch accounting) | 9.3 min | 13 |
| Extra gates (dark-gate wiring) | 9.2 min | 23 |
| Trust gate | 5.5 min | 14 |
| Every other job | 5 min or less | |

The Language-surface job is the critical path. It runs 170 gates serially in one job.

## The 25 slowest gates

Times are in minutes. "main" is run 39775852735 and "branch" is run 39764647243. A dash means the
step did not run in that run (scheduled-only).

| # | main | branch | Job | Gate (workflow step → command) |
|---|---|---|---|---|
| 1 | – | 16.9 | Nightly fuzz | Fuzz campaign → `fuzz_differential.py --n 2000 --depth 3` + `--n 500 --depth 4` |
| 2 | 13.3 | 14.7 | Proof evidence | Phase 3 capstone → `check_phase3_obligation_core.sh` |
| 3 | 10.8 | 10.7 | Language-surface | Checker acceptance evidence → `scripts/tools/checker_acceptance_evidence.sh` |
| 4 | 8.1 | 9.0 | Proof evidence | Phase 2 VC/discharge umbrella → `check_phase2_vc.sh` |
| 5 | 8.7 | 8.4 | Campaign (mint-batch) | `check_mint_batch_accounting.sh` |
| 6 | 7.5 | 7.4 | Language-surface | Claim inventory → `check_claim_inventory.sh` |
| 7 | 6.3 | 7.1 | Proof evidence | VC schema v1 → `check_vc_schema.sh` |
| 8 | 4.5 | 5.9 | Multi-kernel | `check_multi_kernel.sh` (under `nix develop .#provers`) |
| 9 | 4.7 | 5.9 | Multi-kernel | Build the compiler in the provers shell → `lake build` |
| 10 | 5.3 | 5.8 | Proof evidence | Reports-as-views → `check_obligation_report_views.sh` |
| 11 | – | 5.8 | Multi-kernel | Multi-kernel mutation coverage (scheduled, 2 rebuilds) |
| 12 | 5.0 | 4.9 | Trust gate | `run_tests.sh --trust-gate` |
| 13 | 4.9 | 4.6 | Language-surface | Phase 6C observability capstone → `check_phase6c_observability.sh` |
| 14 | 4.1 | 4.7 | Extra gates | Showcase corpus → `check_showcase.sh` |
| 15 | 3.8 | 4.2 | Proof evidence | ObligationCore ledger → `check_obligation_core.sh` |
| 16 | 4.0 | 3.9 | Language-surface | Replay producer → `check_replay_producer.sh` |
| 17 | 3.7 | 3.7 | Language-surface | Slice 8 adversarial → `check_slice8_adversarial.sh` |
| 18 | 3.6 | 3.4 | Main test suite | `run_tests.sh` (fast) |
| 19 | 3.3 | 3.4 | Language-surface | Table migration → `check_table_migration.sh` |
| 20 | 2.2 | 3.4 | Examples | Example outputs → `check_example_outputs.sh` (valgrind) |
| 21 | – | 3.4 | Clean-checkout replay | Install Lean (lean-action, caches disabled) |
| 22 | 3.0 | 3.1 | Language-surface | Identity rows → `check_identity_rows.sh` |
| 23 | 3.1 | 3.0 | Language-surface | Receipt consumption → `check_receipt_consumption.sh` |
| 24 | 2.8 | 3.0 | Proof evidence | Phase 1 contracts umbrella → `check_phase1_contracts.sh` |
| 25 | 2.0 | 3.0 | Campaign (summary) | `check_campaign_summary_golden.sh` |

Just below the table, at 2.0 to 2.9 min each: `run_fast_surface_gates.sh --mutate`,
`check_contract_negatives.sh`, `check_artifact_fuzz.sh`, `check_attestation_manifest.sh`,
`check_vcgen_diff.sh`, `check_external_checker.sh` and `check_obligation_prove_views.sh`.

## Why the top 10 are slow

### 1. `check_phase3_obligation_core.sh` (13.3 min): an umbrella re-running sub-gates CI already ran

Lines 66-70 re-run five sub-gates: `obligation_core`, `report_views`, `policy_views`,
`prove_views` and `discharge_adapters`. All five ran as their own steps in the same job between
12:36 and 12:52. In the log, this loop took 12.2 of the 13.3 min (12:53:29 → 13:05:40). The
gate's own probes take about 67 s. Those probes compile `examples/hmac_sha256` about 9 times, with
`--report obligation-ledger --json` three times on identical input. Two of the nested sub-gates,
`report_views` and `prove_views`, also each re-run `check_snapshots.sh` (about 75 s).

### 2. `checker_acceptance_evidence.sh` (10.8 min): duplicate of steps run minutes earlier

This script re-runs four gates that just ran as separate Language-surface steps:
`check_checker_identity` (1.4 min), `check_kernel_recheck`, `check_checker_policy` and
`check_axiom_inventory`. It then runs the full `claim_inventory.sh` replay (about 7.5 min), which
`check_claim_inventory.sh` had just run. These runs measure the real checker on purpose, so
caching them is not appropriate. The duplication itself is the waste.

### 3. `check_phase2_vc.sh` (8.1 min): pure umbrella

It runs nine sub-gates, and all nine ran as standalone steps in the same job. Most of its cost is
`check_vc_schema.sh` running a second time (see #7).

### 4. `check_mint_batch_accounting.sh` (8.7 min): the same 4,359-line gate run 9 times

It runs `check_dependency_edges.sh` once per self-test environment flag, and each run takes 45 to
85 s. Each run recompiles about 31 proof-carrying examples × 4 reports (`cc_prefetch`, about 128
compiles) and repeats the Lean probe batches. The flags affect only the mint-batch section, so
about 128 × 9 compiles are identical across runs.

### 5. `check_claim_inventory.sh` (7.5 min): a serial kernel replay over 59 sources

For each of the 59 sources, it runs `--report check-proofs --json` and `--report receipts`. Each
one starts `lake env lean` importing the Examples proof library. That makes about 118 compiler
runs and 120+ Lean processes, at about 7.6 s per source, all in serial. The replay is the
evidence, so caching it is not appropriate. Parallelizing it (the rows are already `LC_ALL=C sort`ed)
would take it to about 2.5 min.

### 6 (#7 in the table). `check_vc_schema.sh` (6.3 min, and again inside #3): one compile per assertion

The `check_json` helper (line 22) recompiles on every assertion. That is 20 identical runs of
`concrete examples/hmac_sha256/src/main.con --report vcs --json` at about 18.6 s each. Each VC
report starts three `lake env lean` discharge batches. HMAC has goals the batch cannot close, so
the compiler falls back to one Lean process per goal (Main.lean around lines 700-760). **This is
the cache's best case.**

### 7. `check_multi_kernel.sh` (4.5 min) plus its `lake build` step (4.7 min)

About 2 min of the build step is downloading the nix closure. The other 2.6 min is a cold build of
all 242 Lean jobs, because this job does not restore the `.lake` cache the other jobs use. The gate
itself is dominated by external prover runs (coqc per obligation, `isabelle build`). Two of its
invocations repeat with identical flags.

### 8. `check_obligation_report_views.sh` (5.3 min, and again inside #1): the same report repeated

It runs nine HMAC compiles. The loop at lines 69-70 runs `--report audit` four times on identical
input, once per class. It then re-runs `check_snapshots.sh`, which CI also runs on its own.

### 9. `run_tests.sh --trust-gate` (5.0 min): serial loops over the whole corpus

The consistency and verify sections each loop serially over all 1,100 `tests/programs/*.con`
(49 s and 25 s). The evidence, policy and project sections add about 30 to 50 s each. There is no
duplication with the main suite, which runs in fast mode.

### 10. `check_phase6c_observability.sh` (4.9 min): two isolated mutation harness runs

Sample #5 alone takes 4m47s. `FAMILY=5` and `FAMILY=6` each `cp -a` the whole checkout including
`.lake`, run a pristine `lake build` in the copy, and run baseline gates before mutating. The
header comment says these families "need no rebuild", which is no longer true.

## Cross-cutting causes

1. **Umbrella duplication.** Four of the top 10 (#1, #2, #3 and #10) mostly re-run gates that
   CI already runs as separate steps. On top of that, `check_snapshots.sh` runs five times per CI
   run. Removing the nested re-runs, or turning each umbrella into "the named steps passed", is
   worth about 35 CI-minutes per run with no loss of coverage. The coverage is already there,
   once.
2. **Identical compiles of fixed inputs.** #4, #6, #8 and the probes in #1 are dominated by the
   same pure report run repeatedly with an unchanged compiler. Each run is slow because VC
   discharge starts Lean (18 to 19 s per HMAC report). The shared compile cache below removes
   these runs both within and across gates and worktrees.
3. **The per-goal Lean fallback** in VC discharge (Main.lean, `kernelDischarge*`) runs one Lean
   process per goal when a batch fails. Parallelizing the fallback, or caching discharge results
   per goal text inside the compiler, would speed up every HMAC report on every path.
4. **Serial loops** (#5, #9, nightly fuzz). These are plain parallelization candidates.

## Shared compile cache (R-0546 step 2)

`scripts/tests/lib/compile_cache.sh` provides `ccache_run "$COMPILER" <file.con> --report <kind>
[--json]`. It is shared by every gate and every worktree under `~/.cache/concrete-compile`.

**What the key covers.** A stale result is ruled out by construction: the key covers everything
the invocation can read, and any invocation it cannot cover runs uncached.

- **The compiler:** the sha256 of the binary's bytes, hashed on every call (never by path or
  mtime), plus the tree's BuildIdentity.
- **The invocation:** the exact argv, the input's absolute path and the working directory.
- **The input's closure:** the project closure, found the way `findProjectRoot` finds it (every
  file under the nearest `Concrete.toml`, or under the input's directory when there is no
  manifest). Each file is hashed separately.
- **What the compiler loads:** the resolved std tree and `proofs/`.
- **The Lean workspace and environment:** `lean-toolchain`, `lakefile.toml` and
  `lake-manifest.json` (VC discharge runs `lake env lean` in the cwd), plus `PATH` and the resolved
  path of every tool the compiler can spawn.

**What is cached.** Only the allowlisted pure reports `vcs`, `obligation-ledger` and `audit` are
cached. Each was checked to be deterministic run-to-run on HMAC.

**What is never cached.** `-o` builds, `--interp`, `check-proofs`, receipts and `prove` (a fresh
nonce, and the replay is the evidence), model-diff, traces, and projects with `[dependencies]` or
`[native]`. A gate that tests determinism calls the compiler directly.
`CONCRETE_COMPILE_CACHE=0` disables the cache, and `=refresh` recomputes.

**Controls.** `check_compile_cache.sh` checks the following:

- a hit is byte-identical (stdout, stderr and status) to a fresh compile, including a failing one;
- editing the input, another project file, a std file or the compiler's bytes each invalidates
  the entry;
- cwd and argv are separate keys;
- the shapes the key cannot cover bypass the cache.

**Known limit.** A Lean discharge that fails for a reason outside the key, for example the
process being killed under memory pressure, is stored like any other result. If a cached report
looks wrong, re-run with `CONCRETE_COMPILE_CACHE=refresh`.

### Measured effect

Adopted by `check_vc_schema.sh`, `check_obligation_report_views.sh` and
`check_phase3_obligation_core.sh`. The measurements ran on the same 18-core laptop, at `25eb35fd`
(original gates) and `a8a6323e` (cached gates). The compiler was identical, built from the same
Lean sources. Each gate ran sequentially, while the machine had background load from other
sessions (load average 7 to 25, recorded per run).

MEASURED_TABLE

## Lean build seeding (R-0546 step 3)

**What a fresh build costs.** A fresh checkout builds all 242 Lean jobs. Measured at `25eb35fd`:
**52 s wall, 255 s user CPU** on the 18-core laptop. On CI it is about 2.6 min (#9 above).

**How seeding works.** `scripts/lake-seed.sh` copies another checkout's `.lake` copy-on-write
(APFS `cp -c`, or Linux `--reflink`), then runs `lake build`. Measured: **0.2 s to copy and 0.6 to
1.1 s for the verifying `lake build`**, about a 50× speed-up for a new worktree.
`scripts/worktree-new.sh` seeds by default (`CONCRETE_LAKE_SEED=0` opts out).

**Why a seed cannot go stale.** Lake decides what to rebuild from content hashes in each
artifact's `.trace` file, not from mtimes. The negative control:

1. A seed built at `25eb35fd` was copied into a checkout of `8f40d45b`.
2. Every seeded file was then touched to be newer than the sources.
3. `lake build` still rebuilt the 146 modules whose sources differ (57 s).
4. The binary embeds `8f40d45b`'s BuildIdentity (`ee30a7dc…`), and the seed's (`5a0afcdd…`) does
   not appear in it.

A seed from any commit, including one with uncommitted edits, therefore only saves work. Because
the verifying build always runs, the seed is never a substitute for building.

**Not done: a shared read-only `.lake`.** Lake writes traces, logs and the binary into
`.lake/build`, so a shared read-only build directory cannot serve as a live build directory.
Copy-on-write seeding gives the same speed-up without sharing a mutable directory between
worktrees.
