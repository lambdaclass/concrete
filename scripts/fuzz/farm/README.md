# Fuzz farm

A continuous bug-finding loop on the CI server, concrete-ci-2 (a Hetzner AX102). It runs the
repository's differential and linearity generators around the clock against the newest build of
`origin/main`, and turns what they find into deduplicated, minimized, reproducible findings. Nothing here commits to
the repository. `promote.sh` prepares a reproducer, and an agent or a human files it.

| file | role |
|---|---|
| `farm.py` | engine: campaign workers, oracles, classification, minimizer, dedup store, re-check, summary, status JSON, promote |
| `farm.sh` | driver: `run` (campaign loop + load monitor), `track` (fetch/build/snapshot/re-check), `status` |
| `install-farm.sh` | root installer: `fuzz` user, clone, systemd slice/service/timer, config |
| `promote.sh` | `promote.sh <signature> [--repo DIR --write]`: a `tests/regressions/` fixture plus the `scripts/gen/new_bug.sh` command |
| `predicate.sh` | external predicate that lets the in-repo AST reducer (`concrete reduce`) minimize with the farm's oracle |

The gate is `scripts/tests/check_fuzz_farm.sh`. It is build-free and drives the engine with
the fake compiler and generators under `tests/fixtures/fuzz_farm/`.

## Generators and oracles

The generators are loaded by file path from a snapshot of main, so generator fixes land with
the language rules they track. The engine and the oracles are this directory's own code.

| generator | source | oracle |
|---|---|---|
| `diff` | `fuzz_differential.py` `Gen(rng, depth).program()` | `--interp` vs the compiled binary |
| `lin` | `fuzz_linearity.py` `gen_cases(rng, steps)` | ground truth: must compile / must reject with a linearity code |
| `mut` | a `diff` program with 1 to 3 token edits on one line | the compiler must not crash or hang |

Each batch draws a 63-bit batch seed and each case a 63-bit seed from it. Both are logged in
`state/seeds.log` with the generator, depth and compiler SHA, and every finding records its
seed. `farm.py repro --generator G --seed S --depth D --gen-dir <snapshot>/gen` regenerates
the program.

## Classes

| class | meaning | notify |
|---|---|---|
| `silent` | both sides ran and disagree: value vs value, or value vs trap | yes (`error`) |
| `crash` | `PANIC`, uncaught exception, stack overflow, a signal, or an E07xx SSA-verify refusal, in the compiler or the interpreter | yes (`warn`) |
| `unsound` | the checker accepted a program that the linearity oracle says must be rejected | yes (`error`) |
| `timeout` | one side hangs. It is re-run once at 4x the budget (60 s compile, 10 s run, 30 s interp) first | no |
| `refusal` | a program the generator believes valid is rejected. This fails closed and has the lowest priority | no |

A crash is keyed without its generator, so the same panic reached by `diff` and `mut` is one
finding.

## Findings pipeline

1. Pre-signature = class + oracle + normalized shape. Paths, positions, numbers, quoted names
   and generator variable names are erased. The shape is the diagnostic or the diff shape
   (which output lines differ, value/trap sides).
2. A repeat of a known pre-signature only bumps the count (the last 50 seeds are kept). Every
   25th repeat is re-minimized, up to 5 distinct structures per pre-signature.
3. A new pre-signature is minimized while the pre-signature holds. `diff` findings go through
   `concrete reduce` (AST passes, via `predicate.sh`), then a line and intra-line delta
   reducer: it drops statements, takes one `if` arm, drops a `% B` or an operand, lowers
   literals to 1, and drops single tokens. `lin` unsoundness uses pair minimization: the bug
   program must stay accepted AND its consume-once twin over the same chain must still
   compile, so the terminal stays a real violation. Steps collapse by renaming `NEW` to `OLD`.
   The budget is 900 s or 600 oracle runs per finding.
4. Signature = class + `-` + sha256(pre-signature + structure hash of the minimized program)[:12].
   The structure hash erases literals and renames identifiers by first appearance.
5. Store: `/home/fuzz/findings/<sig>/{repro.con, minimized.con, meta.json, log}` plus the
   index `findings/findings.json`.
6. Re-check: when the track job builds a new main, every open finding's `minimized.con` (then
   `repro.con`) is re-classified. If neither reproduces the pre-signature, the finding is
   marked `fixed-on-main <sha>`. A fixed signature that shows up again is reopened.
7. Generator drift: a non-`mut` batch of at least 20 cases with more than 30 % refusals raises
   an alert in `state/drift.json` and the status JSON. Only one refusal per such batch is filed,
   so drift does not flood the store with "bugs". A clean batch clears the alert.

Reporting:

- `scripts/land/notify.sh <level> <title> <body>` is called for each NEW silent, crash or
  unsound signature when main's snapshot has it. Channels come from the fuzz user's
  `~/.config/concrete/notify.env`.
- The daily summary is written to `/home/fuzz/reports/summary-YYYY-MM-DD.md`.
- The status page reads `/srv/fuzz-farm/fuzz-status.json` (world-readable, schema
  `fuzz-farm-status/1`). It holds per-class `open`, `fixed`, `new_today` and `occurrences`,
  plus totals, today's case counts per generator and outcome, drift alerts, pause state and the
  compiler SHA.

## Resource policy

The farm must never slow CI (the GitHub runner services in `system.slice`) or the gate host
(`dev` over SSH, `user.slice`).

`install-farm.sh`'s defaults, in the table below, were sized for the first farm host,
concrete-ci-1 (64 threads, 251 GiB; deleted 2026-10-23). The farm now runs on concrete-ci-2
(32 threads, 125 GiB), provisioned from the owner's private server configuration with the same
policy scaled to half the threads: `CPUQuota` 2400 % (75 % of 32 threads), `MemoryHigh` /
`MemoryMax` 24G / 32G, 16 workers, and a pause when non-farm CPU use is above 8 cores or load1 is
above 48. Re-running `install-farm.sh` unchanged on that host would restore the larger values.

| knob | value | why |
|---|---|---|
| slice | `fuzz.slice` | everything (campaigns and builds) is accounted together |
| `CPUWeight` / `IOWeight` | 1 / 1 | the minimum. Under contention the farm gets about 1/100 of a default-weight peer |
| `CPUSchedulingPolicy` | `idle` (SCHED_IDLE), plus `Nice=19` | it only runs on otherwise idle CPU |
| `IOSchedulingClass` | `idle` | |
| `CPUQuota` | 4800 % in production (75 % of 64 threads); 800 % in smoke mode | hard ceiling even on an idle box |
| `MemoryHigh` / `MemoryMax` / swap | 56G / 64G / 0 | reclaim pressure first, then OOM inside the slice only |
| `TasksMax` | 8192 | |
| workers | 40 in production, 4 in smoke mode | each worker is mostly one compiler process plus clang |
| pause rule | pause when non-farm CPU use is above 16 cores (25 %; busy time from `/proc/stat` minus the slice's `cpu.stat`), or load1 is above 96 (a backstop only: load1 counts the farm's own workers). Resume after 4 consecutive 15 s samples under 8 cores | campaigns and minimizers sleep. An in-flight case finishes, which takes seconds |

Stop and start:

```sh
sudo systemctl stop fuzz-farm.service fuzz-farm-track.timer            # stop now
sudo systemctl disable --now fuzz-farm.service fuzz-farm-track.timer   # stop and keep off across reboots
sudo systemctl enable --now fuzz-farm-track.timer fuzz-farm.service    # start (production config)
sudo bash scripts/fuzz/farm/install-farm.sh --smoke 10                 # re-run a 10 min smoke
journalctl -u fuzz-farm -f ; sudo -u fuzz /opt/fuzz-farm/farm.sh status
```
