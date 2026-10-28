# Self-hosted CI runners

The CI workflow (`.github/workflows/lean_action_ci.yml`) and the branch-health workflow can run
their Ubuntu jobs on our own servers instead of GitHub-hosted `ubuntu-latest`. This page covers what
the switch does, how to set up a server, and the security rules for a **public** repository.

## What the switch does

Every Ubuntu job's `runs-on` is:

```yaml
runs-on: ${{ (github.event_name != 'pull_request' && vars.CONCRETE_SELF_HOSTED == 'true') && fromJSON('["self-hosted","linux","x64","concrete"]') || 'ubuntu-latest' }}
```

- The repository variable `CONCRETE_SELF_HOSTED` routes the **landing tier** (build, test suites,
  trust, proof and evidence jobs, the language-surface jobs, multi-kernel) and branch health.
- `CONCRETE_SELF_HOSTED_CAMPAIGN` routes the **campaign tier** (mutation plan, the 24 mutation
  shards, the aggregate, the fuzz campaign, the proof replay) on its own, and the two light jobs of
  the nightly workflow (`nightly.yml`: the dispatch and the report, which need `gh` and `python3`). Leave it unset
  unless the servers can absorb 24 shards of up to ~85 minutes each. Labels give no priority, so a
  campaign on self-hosted runners would queue landing runs behind it.
  The tiers are listed in [PROMOTION_PIPELINE.md](PROMOTION_PIPELINE.md#ci-tiers).
- An unset variable, or any value other than `true`, means GitHub-hosted. That is the default.
- **A `pull_request` event never runs on a self-hosted runner**, whatever the variables say. See
  the security section for why this guard alone is not enough.
- `Build + main tests (macOS)` stays on `macos-latest`. The Pages workflow (`book.yml`) stays on
  GitHub-hosted runners.

**Two pools.** CI's jobs ask for the label `concrete`; branch health asks for `concrete-light`, a
separate pool of small runners (two on concrete-ci-2, `LIGHT_RUNNERS` in the dotfiles host file).
Branch health runs on every push to every branch, about 500 runs a day, and on the `concrete` pool
those runs took 20-92 runner-minutes out of each train's window (measured 2026-10-24). Labels give
no priority, so the only way to keep a train from queueing behind them is to not share the pool.
A `concrete-light` runner carries only that label, so CI never lands on it either.

A job routed to self-hosted waits until a runner with **all four** labels `self-hosted`, `linux`,
`x64` and its pool label (`concrete`, or `concrete-light` for branch health) is idle. If no runner is online, it waits (up to 24 hours) rather than falling
back to GitHub-hosted. So turn the variable off before taking the servers down.

### Switching it on and off

```sh
# on (landing tier)
gh variable set CONCRETE_SELF_HOSTED --body true -R unbalancedparentheses/concrete2
# off (back to GitHub-hosted)
gh variable delete CONCRETE_SELF_HOSTED -R unbalancedparentheses/concrete2
# campaign tier, separately (normally left off)
gh variable set CONCRETE_SELF_HOSTED_CAMPAIGN --body true -R unbalancedparentheses/concrete2
```

The variable is read when a run starts. Runs already queued keep the routing they started with.

**lambdaclass/concrete** is a separate repository. Variables and repository-level runners do not
carry over: it stays on GitHub-hosted runners until it gets its own runners (registered against
`https://github.com/lambdaclass/concrete`, or through an org runner group that grants that repo the
`concrete` label) and its own `CONCRETE_SELF_HOSTED` variable. The nightly campaign (`nightly.yml`,
which dispatches CI with `campaign=true` on `main`) runs there, so `CONCRETE_SELF_HOSTED_CAMPAIGN`
matters there for the nightly campaign. On concrete2 the nightly runs only when the variable
`CONCRETE_NIGHTLY_QUALIFICATION` is `true`; otherwise the campaign variable only affects campaign
runs you dispatch with `-f campaign=true`.

## Security rules (public repository)

GitHub's own guidance is that self-hosted runners should not be used with public repositories,
because a pull request can run arbitrary code on them. We use them anyway, under these rules:

1. **No `pull_request` event runs on a self-hosted runner.** The `runs-on` expression enforces this
   for the workflow as written. But a pull request runs the workflow file **from the PR branch**, so
   a fork PR can rewrite `runs-on` to `[self-hosted]` and the guard is gone. The guard prevents
   accidents. It does not stop an attacker. Rules 2 and 3 do.
2. **Require approval for all outside collaborators.** In Settings, Actions, General, "Approval for
   running fork pull request workflows from contributors", choose "Require approval for all outside
   collaborators". Before approving a fork PR's run, read its diff to `.github/workflows/`. A
   changed `runs-on`, a new job, or a new trigger is a reason not to approve.
3. **Ephemeral runners in throwaway VMs or containers are recommended.** `./config.sh ...
   --ephemeral` only makes the runner take one job and then unregister. **It does not erase
   anything:** files, surviving processes, cron entries, a modified `~/.bashrc` or toolchain all
   stay on the machine. Isolation comes from destroying the VM or container after each job and
   starting a fresh one from a clean image, which registers a new ephemeral runner. Automate that
   with actions-runner-controller, or with a supervisor **outside** the job's VM or container that
   calls `gh api -X POST repos/unbalancedparentheses/concrete2/actions/runners/registration-token`.
   The supervisor's admin token must never be readable from inside a job. Persistent runners (or
   `--ephemeral` re-registered on the same machine, which is the same thing) are acceptable only
   with rule 2, the cleanup hook below and a dedicated low-privilege user, and with the
   understanding that one job can leave state for the next.
4. **Dedicated, unprivileged users and hosts.** Use one Unix user per runner, with no SSH keys,
   cloud credentials or other repositories' secrets readable, and **no sudo**. Don't run other
   services on the host.
5. The triggers that do reach self-hosted runners are `push` to `main`, `push` to other branches
   (branch health), `workflow_dispatch` and `schedule`. All of them need write access to the
   repository. The nightly report job is a `workflow_run` of CI that runs only for a
   `workflow_dispatch` run on `main` (never for a pull request's run) and checks out the default
   branch, so no pull request's code reaches it.

## Server setup

### Host

- Ubuntu 24.04 x86-64, the same image family as `ubuntu-latest`. The `.lake/build` and `~/.elan`
  caches are keyed by `runner.os`/`runner.arch` and shared with GitHub-hosted runs. Lake re-checks
  its traces, so a mismatched cache rebuilds instead of being trusted. But a different distribution
  makes every self-hosted build cold.
- **One runner per ~4 cores**, with at least 8 GB RAM and 30 GB disk each. The Lean build and the
  heavier gates are memory-bound, and a runner that swaps is slower than a GitHub-hosted one. For
  example, a 16-core, 64 GB server takes 4 runners.

### Packages to preinstall

The workflow installs `clang` (and `valgrind` for the examples job) with `sudo apt-get` on
GitHub-hosted runners. On a self-hosted runner it does so **only when the tool is missing**, and the
runner users have no sudo (see below). So preinstall everything:

```sh
sudo apt-get update
sudo apt-get install -y git curl ca-certificates build-essential clang valgrind \
  python3 python3-pip python3-yaml xxd perl tar zstd ripgrep coreutils gh
# Rust (rustc + cargo), per runner user, like the stable toolchain on GitHub-hosted images:
sudo -iu runner1 sh -c 'curl --proto =https --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y --profile minimal'
```

- `rustc` is used by the LL(1) grammar job. `cargo` builds nanoda for the external-checker job
  whenever `~/.cache/concrete-ext` has no build for the pinned SHA. Ubuntu's `rustc` package does
  not pull in `cargo`, so use rustup (or install both `rustc` and `cargo`), and check it once by
  running the external-checker job with an empty `~/.cache/concrete-ext`. Rustup puts its tools in
  `~/.cargo/bin`, so install it before `./config.sh`, or add that directory to the runner's `.path`.
- `python3-yaml` is used by the workflow, example and qualification gates, `xxd` and `curl` by
  the workload differentials, and `zstd` by `actions/cache`. `gh` (GitHub CLI; Ubuntu 24.04 ships
  it as the `gh` package) is used by the nightly workflow's dispatch and report jobs, which run on
  these runners under `CONCRETE_SELF_HOSTED_CAMPAIGN`.
- **Lean (elan) is not preinstalled.** `leanprover/lean-action` installs elan into `~/.elan` on
  first use, and the toolchain stays there: the runner user's home persists between jobs. The
  `~/.elan` cache step runs on GitHub-hosted runners only. On these runners it re-downloaded a
  2.3 GB archive of toolchains the home already held, 2-4 minutes per job and about a third of all
  runner time in a train (measured 2026-10-23). A toolchain bump is installed once per runner by
  lean-action (and the 4.35 leg by its `elan toolchain install` step). With one user per runner,
  each runner has its own `~/.elan`, so concurrent jobs never race on one toolchain directory.
- **Nix** is needed by exactly one job, `Multi-kernel evidence (Rocq + Isabelle)`, which runs
  `nix develop .#provers`. Install multi-user Nix (daemon) **before** configuring the runner, so
  `/nix/var/nix/profiles/default/bin` is on the PATH the runner records in its `.path` file. Enable
  flakes in `/etc/nix/nix.conf` (`experimental-features = nix-command flakes`).
  `cachix/install-nix-action` sees an existing `nix` and skips its own install, so its
  `extra_nix_config` is **not** applied: the system config must carry the flakes setting. The
  provers closure is several GB; a persistent `/nix/store` makes this job much faster than on
  GitHub-hosted runners.
- **Do not install** `z3`, `cvc5`, `coqc`, `isabelle` or `lli` system-wide. Several gates
  treat those as skip-if-absent, so installing them makes a self-hosted run execute legs that the
  GitHub-hosted reference run skips, and the two runs no longer agree. The provers come from the
  Nix shell in the one job that uses them.
- **No sudo.** On a self-hosted runner, the `Install clang` steps run `sudo apt-get` only when
  the tool is missing. Without sudo, that step fails and names the problem, which is the intended
  result: install the package on the host. Do **not** grant passwordless `sudo apt-get`.
  Unrestricted apt arguments give root (apt hooks, `-o` options, local packages), and that defeats
  the per-runner user boundary.

### Installing a runner

Do this once per runner, each in its own user (`runner1`, `runner2`, ...) and its own directory:

```sh
sudo useradd -m -s /bin/bash runner1
sudo -iu runner1
mkdir actions-runner && cd actions-runner
# Take the current version and checksum from Settings, Actions, Runners, New self-hosted runner.
curl -o actions-runner.tar.gz -L https://github.com/actions/runner/releases/download/v<VERSION>/actions-runner-linux-x64-<VERSION>.tar.gz
tar xzf actions-runner.tar.gz
# The token is shown on that settings page (or: gh api -X POST repos/unbalancedparentheses/concrete2/actions/runners/registration-token)
./config.sh --url https://github.com/unbalancedparentheses/concrete2 --token <TOKEN> \
  --name "$(hostname)-runner1" --labels concrete --unattended   # add --ephemeral (recommended)
exit
cd /home/runner1/actions-runner
sudo ./svc.sh install runner1
sudo ./svc.sh start
```

`self-hosted`, `linux` and `x64` are added automatically, and `--labels concrete` adds the fourth
label the workflow asks for.

### State between jobs (persistent runners)

A GitHub-hosted job starts on a fresh VM. A self-hosted one does not. Here is what the workflow
already handles, and what the host has to handle.

- **Workspace.** `actions/checkout@v4` (default `clean: true`) runs `git clean -ffdx` and
  `git reset --hard` on the existing checkout. That removes `.lake/`, `.mutation-evidence/`,
  leftovers of a cancelled mutation shard and any tracked file a gate mutated, before the cache
  restores `.lake/build`. The proof replay's "no build caches" property holds for the same
  reason.
- **`$RUNNER_TEMP`** is emptied by the runner for every job. The workflow's own temporary files
  live there: the LL(1) checker binaries, the trust-gate log, the thesis-demo reports, the example
  outputs, the checker acceptance evidence and the mutation shard records.
- **`/tmp` is NOT cleaned, and many gates write fixed `/tmp/...` paths.** Two runners on one host
  would overwrite each other's files, and a stale file from an earlier job could be read by a later
  one. Give every runner service a private `/tmp`, and empty it when each job starts:

  ```sh
  # Private /tmp per runner service (systemd unit created by svc.sh)
  sudo systemctl edit "$(cat /home/runner1/actions-runner/.service)"
  #   [Service]
  #   PrivateTmp=true
  sudo systemctl restart "$(cat /home/runner1/actions-runner/.service)"
  ```

  ```sh
  # /home/runner1/job-started.sh  (chmod +x), wired up in the runner's .env file:
  #   ACTIONS_RUNNER_HOOK_JOB_STARTED=/home/runner1/job-started.sh
  #!/bin/sh
  # /tmp is private to this runner service (PrivateTmp), so emptying it touches no other process.
  find /tmp -mindepth 1 -maxdepth 1 -user "$(id -u)" -exec rm -rf {} + 2>/dev/null || true
  ```

  An ephemeral runner in a fresh container or VM needs neither.
- **`$HOME` caches** persist: `~/.elan` (the toolchain, keyed by `lean-toolchain`) and
  `~/.cache/concrete-ext`. In that cache, each tool is built into a directory named by its pinned
  SHA and checked against it, and lean4export, which is rebuilt with this repository's toolchain,
  also has that toolchain in its directory name. So a toolchain bump builds a new exporter instead of
  reusing one that cannot read the new `.olean` files. Both caches are keyed by everything their
  builds depend on, so keeping them between jobs makes runs faster without affecting correctness.
  They live in the user's HOME, so each runner has its own. Old directories are never deleted; prune
  `~/.cache/concrete-ext` now and then.
- **Processes.** The runner kills a job's process tree when the job ends. A gate that leaves a
  daemon behind would survive on a persistent runner. That is one more reason to prefer ephemeral
  runners.

### Checking it works

1. Settings, Actions, Runners lists each runner as Idle, with labels
   `self-hosted, Linux, X64, concrete`.
2. `gh variable set CONCRETE_SELF_HOSTED --body true -R unbalancedparentheses/concrete2`.
3. `gh workflow run CI --ref main -R unbalancedparentheses/concrete2` (landing tier only), then
   check in the run's job pages that "Runner name" is one of yours and that `Install clang` printed
   `self-hosted runner, using preinstalled clang`.
4. To turn it off, delete the variable. Queued jobs that are still waiting for a self-hosted
   runner have to be cancelled and re-run.

## Build cache (R-0546)

Every job, worktree and agent used to build the compiler from nothing. Two caches now start that
build warm. **Neither one skips it.** `lake build` still runs and re-checks every module's trace,
and the BuildIdentity freshness gate still runs.

### Lean: `.lake/build` seeds

`scripts/dev/seed-lake.sh` copies an existing `.lake/build` into a checkout that has none. It
**refuses** (exit 3, nothing copied) unless the source matches on the **seed key**:

- the platform (`uname -s`/`-m`);
- `lean-toolchain`;
- the sha256 of `lake-manifest.json`;
- the sha256 of the **lean executable itself**.

The executable is in the key because lake's traces record only the Lean version and commit. Elan's
official archive and the devshell's autoPatchelf'd Linux copy would otherwise accept each other's
`.olean` files, and a module kernel-checked by one binary would be credited to the other (R-0208).
For a checkout, the script reads the binary from the checkout's own lake traces. Traces from other
Lean versions are ignored, because lake rebuilds those modules anyway.

Sources and `lakefile.toml` are not in the key. A seed from other sources is the useful case, and
lake mixes each module's sources and options into its trace. A source hash only chooses between
cache entries.

| Where | Seed source | How |
|---|---|---|
| Self-hosted CI runner | `/var/cache/concrete-lake/<key>/{<srchash>,latest}` | workflow step `Seed .lake from the server build cache (self-hosted only)` |
| GitHub-hosted runner | `actions/cache` (`.lake/build`, exact source key) and lean-action's `.lake` cache (previous commit) | unchanged |
| New worktree (laptop or server) | the main checkout's `.lake/build` | `bash scripts/dev/seed-lake.sh` (inside `nix develop`), then `lake build` |

- **Every same-version module must name its executable.** A trace from this Lean version that
  does not name the binary that compiled it refuses the whole seed, because it may be a fetched
  artifact or an unknown log format. The check assumes that the binary at a recorded path has not
  been replaced in place. That holds for Nix store paths, which are immutable, and for elan
  toolchains, which use one directory per release.
- **A consistent snapshot.** Lake takes no lock that the seeder could share. A checkout source must
  therefore be quiet: nothing in it may be written from 10 s before the copy
  (`CONCRETE_SEED_QUIET`) until the copy ends. A `lake build` that is running, or has just
  finished, in the main checkout refuses the seed. Provenance is checked again on the copy itself.
  So is the order of writes: an `.olean` or `.ilean` newer than its trace means the build was
  killed or suspended mid-module, and that refuses the seed too. One risk remains and is accepted:
  checkout files edited by hand with matching timestamps are not detected. CI seeds only from
  published entries.
  Published cache entries need neither check. Each one is moved into place whole, after a completed
  build, and never written again. One seeder runs per checkout at a time (`.lake/seed.lock`).
- **CI checks against `~/.elan`'s lean** (`--elan`). That is the binary lean-action builds with,
  whatever else is on `PATH`.
- **Copies, never hardlinks.** Lake rewrites trace files in place, so a hardlink would write through
  into the seed. macOS uses APFS clones (`cp -c`, instant, no extra space). The server uses a plain
  copy, about 1 s for 600 MB.
- In CI, a seeded job sets `use-github-cache: false` on lean-action. Otherwise lean-action would
  restore an older commit's `.lake` over the seed. An exact-hit of the `.lake/build`
  `actions/cache` step comes first. The seed then refuses to overwrite it, which is the right
  outcome, because the exact hit is the better start.
- `--build [args]` seeds and then always `exec`s `lake build args`. The exit status is the build's.
- `scripts/tests/check_build_cache.sh` gates the key, the refusals, the copy-not-link rule,
  publish/consume, and the workflow wiring.

**The cache producer on concrete-ci-1.** The system user `lakecache` (no login, no sudo, home
`/var/lib/concrete-lake`, its own elan) owns `/var/cache/concrete-lake`. That directory is
world-readable and writable only by `lakecache`. `concrete-lake-refresh.timer` runs every 10
minutes and starts `concrete-lake-refresh.service`, which runs at `Nice=15`, `CPUWeight=20` and
`IOWeight=20`, with `ProtectSystem=strict` and `ProtectHome=true`. The service:

1. fetches `origin/main` into `/var/lib/concrete-lake/main`, a persistent clone, so the build is
   incremental;
2. runs `/usr/local/lib/concrete-lake/seed-lake.sh --publish /var/cache/concrete-lake`, which runs
   `lake build` and publishes only if it succeeds.

The service keeps three entries per key and updates `latest` atomically. The installed copy of
`seed-lake.sh` must be updated by hand when the key format changes. The key starts with
`format=concrete-lake-seed-v1`, so a format change makes the old entries miss instead of match
wrongly.

**Why only `lakecache` writes.** Lake trusts a module's stored trace and `.hash` files. A job that
could write the seed could plant an `.olean` file whose trace still matches its sources, and later
jobs would use it without rebuilding. Runner users (`gha1..6`) and `dev` can only read the cache.
The producer builds only `origin/main`, which takes write access to change.

### Nix: binary cache

All local users share one `/nix/store` through nix-daemon. On this host the store already acts as
the cache, so the useful steps are to realize the devshell closures and to protect them from garbage
collection:

- `/nix/var/nix/gcroots/concrete/devshell-default` (116 paths) and `devshell-provers`
  (11.4 GB, Rocq, Isabelle, cvc5), built from `origin/main`'s `flake.lock`;
- `/nix/var/nix/gcroots/concrete/nix-serve-ng` (the server binary).

`concrete-nix-serve.service` serves that store as a **signed** binary cache. It uses nix-serve-ng
from the repository's pinned nixpkgs.

- **Choice of nix-serve-ng.** It serves the live store directly, with no push step, database or
  second copy, and signs narinfos on the fly. Attic adds a server, a database and an upload step,
  and earns that only with several machines that push.
- **Exposure.** It listens on `127.0.0.1:5050` only. The ufw rules are unchanged (OpenSSH only).
  It runs as `DynamicUser`, and the signing key reaches it through systemd `LoadCredential`.
- **Keys.** The secret key is `/etc/nix/concrete-ci-1-cache.sec` (root, 0600). The public key is
  `concrete-ci-1-cache-1:vbcfJrrGDnfqalZ9Tmf0ZsvWY5+UCM77+uHM5aVoyMA=`.
- **Not a substituter on concrete-ci-1 itself.** A local substituter that serves the same store
  can never supply a missing path. It would only add a failed lookup per miss and a new way to fail.
  `/etc/nix/nix.conf` is therefore unchanged, and cache.nixos.org remains the only substituter. A
  second server on the same LAN would add `http://<lan-ip>:5050` (after a ufw rule for that
  interface only) as `extra-substituters`, plus the public key in `extra-trusted-public-keys`.
- **GitHub-hosted runners** never reach it. They keep cache.nixos.org and `actions/cache`. No port
  is opened.
- **The laptop.** The laptop is aarch64-darwin and the server is x86_64-linux, so the server's store
  has nothing a macOS devshell can use. It helps only a Linux builder or VM on the laptop. If one is
  needed, use a tunnel, never a public port:

  ```sh
  ssh -i ~/.ssh/concrete_ci -N -L 5050:127.0.0.1:5050 ubuntu@62.210.216.169 &
  nix build ... --extra-substituters http://127.0.0.1:5050 \
    --extra-trusted-public-keys 'concrete-ci-1-cache-1:vbcfJrrGDnfqalZ9Tmf0ZsvWY5+UCM77+uHM5aVoyMA='
  ```

  `--extra-substituters` is honored only for a trusted user, or when it is listed in the laptop's
  `trusted-substituters`.

### Measured (2026-10-22)

| Build | Time | Modules rebuilt |
|---|---|---|
| Server, cold `lake build` (elan Lean, origin/main) | 105 s | 239/242 |
| Server, seeded, same sources | 1 s copy + 0.2 s | 0 |
| Server, seeded from main, building the previous main commit or wip/ci-tiering | 1 s + 28 s | 9 |
| Laptop, seeded (APFS clone, 0 s) from a stale main checkout, building wip/ci-tiering | 85 s (load ~25) | 178 |
| GitHub-hosted `Build` job, lean-action prior-commit cache (run 39775852735) | 240 s | 148 |
| GitHub-hosted downstream jobs, exact `.lake/build` hit | 20–35 s setup | 0 |

A seed is only as warm as its source is recent. The server seed tracks `origin/main` within 10
minutes, so a self-hosted job pays only for its own diff. A laptop worktree seeded from a stale main
checkout pays for everything that differs.
