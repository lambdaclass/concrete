# External Checker Operation

Status: stable reference

Every theorem in Concrete's Lean sources is re-checked outside the Lean kernel by
[con-leche](https://github.com/leanprover/con-leche) and [nanoda](https://github.com/ammkrn/nanoda_lib)
(`scripts/tests/check_external_checker.sh`, CI job `External proof re-check`). What that re-check
establishes, and what it does not, is in [AXIOMS.md](AXIOMS.md#re-checked-outside-the-lean-kernel-con-leche-nanoda).
This page covers how the checkers are run and kept current, because both are trust anchors
(the trusted base in [VERIFIED_PIPELINE.md](../compiler/VERIFIED_PIPELINE.md), R-0208's checker identity)
and an independent checker protects us only while it is current.

## Why currency matters

- **Kernel bug [lean4#14576](https://github.com/leanprover/lean4/issues/14576)**: the official kernel
  accepted an axiom-free proof of `False` (wrong-structure projections; phantom parameters of nested
  inductives escaping type checking). A week-old nanoda also accepted it, because of a separate
  projection bug; two unrelated bugs were needed to get past both. The
  [postmortem](https://leodemoura.github.io/blog/2026-8-1-postmortem-for-kernel-soundness-bug-14576/)'s
  lesson is that independent checking works only when the kernel and the checker are both current.
  The project's own reproducer is `scripts/tests/check_lean14576_reproducer.sh`.
- **[lean4#15288](https://github.com/leanprover/lean4/pull/15288)** (Lean 4.35.0): fixed undefined
  behaviour when an object whose single-threaded reference count had overflowed is shared between
  threads — its children were not marked thread-shared, so other threads could update their counts
  non-atomically and free them while in use. The PR says the official kernel is single-threaded in
  its default configuration but that "other Lean-based checkers such as con-leche might be affected".

## con-leche runs single-threaded

con-leche's default worker count is the machine's hardware thread count (`--jobs=<n>`, `Main.lean`).
The gate passes `--jobs=1` at every call (`CONLECHE_FLAGS`), and its leg P fails if any con-leche
invocation bypasses those flags. At `--jobs=1` the parse and install phases run on the main thread
alone and the check phase on one worker thread; no object is shared between concurrently running
threads.

This is defence in depth, not the only protection: con-leche is built with its own `lean-toolchain`,
`v4.35.0-rc3` at both `dd695ca` and `c26f793`, and that release candidate already carries the #15288
fix as the backport [lean4#15306](https://github.com/leanprover/lean4/pull/15306) (it is in the
`v4.35.0-rc1...v4.35.0-rc3` range). Single-threading removes the class even if a future pin is built
with a toolchain that lacks it, or a related sharing bug remains.

nanoda's `num_threads: 4` (the nanoda config in the gate) is unaffected: nanoda is Rust and does not
use the Lean runtime #15288 fixed. It stays at four threads, as the Lean Kernel Arena runs it.

**Cost.** Measured 2026-10-22 on the Hetzner dev server (32 hardware threads) over the gate's own
exports at `8621d95a` (16175 declarations in the standard export), median of three runs, con-leche
`dd695ca`; `c26f793` is within noise of it:

| con-leche run | `--jobs=1` | `--jobs=4` | default (32 workers) |
|---|---|---|---|
| standard export, accepted | 11.2 s | 3.4 s | 1.3 s |
| swapped-proof control, rejected | 8.1 s | 2.7 s | 1.3 s |
| native export, declined | 1.9 s | 0.6 s | 0.5 s |
| **all three** | **about 21 s** | about 7 s | about 3 s |
| peak resident set | 160–170 MB | 245–335 MB | 300–520 MB |

Single-threading adds about 18 s to a gate whose wall time is about 75–80 s (most of it the
population, export and statement-fidelity legs), and it uses about a third of the memory. The
slowdown is kept. Should it matter, the time can be recovered by checking independent export
shards in parallel processes, each single-threaded, rather than by threads within one process.

## Pins and their freshness

`scripts/tests/external_checker.pins` holds lean4export, con-leche and nanoda revisions. The rule for
choosing them is in [AXIOMS.md](AXIOMS.md#re-checked-outside-the-lean-kernel-con-leche-nanoda)
(*Updating the pins*): prefer the revisions the [Lean Kernel Arena](https://arena.lean-lang.org/)
evaluated, since its soundness suite is the external evidence that a revision rejects what it should.

`scripts/tests/check_checker_pin_freshness.sh` compares the con-leche and nanoda pins with the
revisions behind the Arena's **published** results. Each checker's page,
`https://arena.lean-lang.org/checker/<name>/`, links the checker definition at the exact
lean-kernel-arena commit the results came from (`…/blob/<sha>/checkers/<name>.yaml`); that file's
`rev:` is the evaluated revision. If the page cannot be read, the checker repository's
default-branch head (`git ls-remote <url> HEAD`) is the comparison, and the output says so. The
upstream head and the revision the Arena's master branch names for its next round are printed for
information.

| tier | where | stale pin | no network |
|---|---|---|---|
| landing | step in the `External proof re-check` job; `run_ci_gates_local.sh` | warning (`::warning::` annotation), exit 0 | `SKIP`, exit 0 |
| campaign | job `Checker pin freshness (campaign finding)`, nightly cron / `campaign=true` dispatch | **finding**, exit 1 | finding, exit 1 |

An upstream commit never fails a merge: landings must stay reproducible from their own inputs. In
the campaign tier a stale pin is a finding, so stage (c) qualification (which requires every
workflow job) is withheld until the pin is bumped or the finding is answered. The campaign tier
treats an unreadable source as a finding too, because a check that silently skipped every night
would never report anything.

lean4export is not an Arena checker; it is pinned to the lean4export tag for this repository's
`lean-toolchain`, and the gate's export-metadata leg fails if they disagree.

## Bump evaluation, 2026-10-22

**Bumped con-leche `dd695ca` → `c26f793`**, the revision behind the Arena's published results
(lean-kernel-arena@`8bbe2534`: soundness 69 right, 0 wrong, 3 declined; completeness 131 right,
0 wrong, 6 declined). The eight commits between them are con-leche task #353 (the official kernel's
reduction order: arguments last to first, fuel and stack, `eagerReduce`) and #354 (check fuel
effectively unbounded).

- **Build.** The gate builds con-leche with its own `lean-toolchain`, which is `v4.35.0-rc3` at both
  revisions; `c26f793` builds that way. It does **not** build with this repository's Lean 4.34.1,
  and neither does `dd695ca` (both use `Array.markLinear` / `Std.HashMap.markLinear`, which 4.34.1
  lacks). That is expected: con-leche reads exports from older toolchains, and the pins never built
  it with ours. lean4export, which must read our `.olean` files, is the tool built with 4.34.1.
- **Gate.** `check_external_checker.sh` with `c26f793` and `--jobs=1`: 22 passed, 0 failed
  (16175 declarations accepted; swap rejected as a type mismatch at the receiver; native export
  declined with exit 2).
- **Checker identity.** R-0208's identity, kernel re-check and policy gates
  (`check_checker_identity.sh`, `check_kernel_recheck.sh`, `check_checker_policy.sh`) bind the Lean
  kernel replay process and its measured build, not con-leche or nanoda: none reads
  `external_checker.pins`, so no identity pin, register row or claim-inventory row changes. They are
  re-run on the bump commit. The R-0208 closure record is unchanged: its qualification was against
  the Lean kernel's checker identity, and the external re-check is a separate CI job whose verdict
  is re-earned on every run.
- **nanoda** stays at `e0d76f5`, which matches the Arena's published results. The Arena's master
  branch already names newer revisions of both checkers (con-leche `ae1f319`, nanoda `998e0a8`) for
  its round in progress; the freshness check will report them once that round's results are
  published.

The tracked hole is [KNOWN_HOLES.md](KNOWN_HOLES.md) H29.
