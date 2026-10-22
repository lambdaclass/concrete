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
  shards, the aggregate, the fuzz campaign, the weekly proof replay) on its own. Leave it unset
  unless the servers can absorb 24 shards of up to ~85 minutes each. Labels give no priority, so a
  campaign on self-hosted runners would queue landing runs behind it.
  The tiers are listed in ROADMAP.md, "Promotion pipeline", "CI tiers".
- An unset variable, or any value other than `true`, means GitHub-hosted. That is the default.
- **A `pull_request` event never runs on a self-hosted runner**, whatever the variables say. See
  the security section for why this guard alone is not enough.
- `Build + main tests (macOS)` stays on `macos-latest`. The Pages workflow (`book.yml`) stays on
  GitHub-hosted runners.

A job routed to self-hosted waits until a runner with **all four** labels `self-hosted`, `linux`,
`x64` and `concrete` is idle. If no runner is online, it waits (up to 24 hours) rather than falling
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
`concrete` label) and its own `CONCRETE_SELF_HOSTED` variable. The campaign crons run only there
(`github.repository == 'lambdaclass/concrete'`), so `CONCRETE_SELF_HOSTED_CAMPAIGN` matters there
for the nightly campaign. On concrete2 it only affects campaign runs you dispatch with
`-f campaign=true`.

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
   repository.

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
  python3 python3-pip python3-yaml xxd perl tar zstd ripgrep coreutils
# Rust (rustc + cargo), per runner user, like the stable toolchain on GitHub-hosted images:
sudo -iu runner1 sh -c 'curl --proto =https --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y --profile minimal'
```

- `rustc` is used by the LL(1) grammar job. `cargo` builds nanoda for the external-checker job
  whenever `~/.cache/concrete-ext` has no build for the pinned SHA. Ubuntu's `rustc` package does
  not pull in `cargo`, so use rustup (or install both `rustc` and `cargo`), and check it once by
  running the external-checker job with an empty `~/.cache/concrete-ext`. Rustup puts its tools in
  `~/.cargo/bin`, so install it before `./config.sh`, or add that directory to the runner's `.path`.
- `python3-yaml` is used by the workflow, example and qualification gates, `xxd` and `curl` by
  the workload differentials, and `zstd` by `actions/cache`.
- **Lean (elan) is not preinstalled.** `leanprover/lean-action` installs elan into `~/.elan` on
  first use, and the `~/.elan` cache step restores the pinned toolchain. With one user per runner,
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
  restores `.lake/build`. The weekly proof replay's "no build caches" property holds for the same
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
  `~/.cache/concrete-ext` (lean4export, con-leche and nanoda, each built into a directory named by
  its pinned SHA and checked against it). Both are content-addressed, so a persistent copy is a
  speed-up, not a correctness risk. They are per user, so per runner.
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
