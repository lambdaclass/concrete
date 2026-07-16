# Deterministic Capability Simulation (design record)

**Status:** design record, pull-gated. Not scheduled work. Refines the tier-J+
"deterministic simulation backend" earmark in
[EXECUTION_MODEL.md](EXECUTION_MODEL.md); the pull-trigger is ROADMAP Phase 7 item
H6 (attached to H3, IO honesty). Written so the approach is not re-derived or
mis-scoped later. Companion to [TESTING.md](TESTING.md).

## Scope: capability-seam fault injection, NOT full-world DST

Turso/Antithesis-style Deterministic Simulation Testing simulates the *entire*
world — a deterministic scheduler over disk, network, clock, and threads, usually
at hypervisor or syscall-interposition level — because in a typical systems
program effects are diffuse (syscalls happen everywhere). Concrete does **not**
need that, and should not build it.

Concrete funnels every hosted effect through an **explicit, first-class capability
/ handle value**. The `io.Writer` sink, for example, is a function-pointer handle:

```
write_fn: fn(*mut u8, *const u8, u64) -> Result<u64, IoError>
```

So the fault-injection point is obvious and narrow: **swap the backend at the
capability seam.** A test installs a `Writer` whose `write_fn` returns a short
count, or an allocator whose Nth request fails, or a clock that advances on a
fixed schedule. No world-simulator, no syscall interposition. The capability model
makes fault injection cheap *by construction* — that is why Concrete's version is
much smaller than Turso's, and the smallness is structural, not merely prudent.

## What it covers — and what it must NOT re-litigate

This methodology exists for the **dynamic failure interleavings** that Concrete's
other correctness lines structurally cannot reach:

| Correctness line | Covers |
| --- | --- |
| Proofs (pure core) | `option`/`result`/`bytes`/`numeric` semantics vs spec |
| Static gates | capability visibility (manifest), exhaustiveness, MAIN_EXIT status |
| Differential (interp vs compiled) | success-path behavioral equivalence |
| **Capability simulation (this doc)** | **effectful failure interleavings** |

Do **not** rebuild statically-guaranteed properties dynamically. "No trusted
wrapper erases `with(File)`/`with(Alloc)`" is the stdlib manifest gate; "exit
status ≠ stdout" is MAIN_EXIT_MODEL. Simulation is only for what those cannot
prove: what happens *when the effect fails*.

## Failure schedules

A schedule is a deterministic, replayable script over one capability's operations,
e.g.: *first write succeeds partially (returns a short count); second write fails;
allocation #3 fails; close fails after flush succeeds; read returns EOF after N
bytes.* Determinism is a prerequisite (replay + shrink), and Concrete already
values deterministic artifacts (see [DETERMINISM.md](DETERMINISM.md)) — this reuses
that discipline rather than adding a new source of nondeterminism.

## Effect-boundary properties (initial set)

- `write_all` never reports `Ok` after a short or failed write.
- `Reader` treats `Ok(0)` as EOF only at the correct boundary.
- close-fail-after-flush-success is reported, not swallowed.
- An allocation failure surfaces as the declared failure, never a silent partial.
- `drop`/`destroy` never hides capability use (composes with H18 drop-glue).

These are the *dynamic* counterpart to H3: H3's manifest gate proves an `IoError`
variant *can* be surfaced (the vocabulary — `ReadFailed`/`WriteFailed`/
`FlushFailed`/`CloseFailed` — already exists in `std/src/io.con`); simulation
proves it *is* surfaced when the operation actually fails.

## Relationship to proof

Proof stays the fast, precise first line for the pure core. Simulation is for
hosted effects and failure interleavings the proof model does not yet cover — it
does not replace proof and does not upgrade any evidence class to `proved`. The two
are complementary lines, not competitors.

## Phased coverage — seam-by-seam, workload-pulled

Start where the injectable seam already exists (`Writer`/`File`, function-pointer
backend) and expand only as each capability grows a swappable backend (`Alloc`,
`Time`, later `Network`/`Process`). Run against real workloads — `base64_cli`,
`png_chunks`, then workload 3 — not abstract infrastructure built ahead of demand.
The pull-trigger is H3 landing or an IO bug; absent that, this stays a design
record.

## Prior art

Turso/limbo and the Antithesis platform (full-world DST); FoundationDB's simulation
testing (the origin of the technique). Concrete deliberately adopts the *idea*
(deterministic fault injection for effectful code) at a fraction of the scope,
anchored to the capability seam.
