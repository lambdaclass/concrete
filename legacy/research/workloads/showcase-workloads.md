# Showcase Workloads

Status: Open

This note collects the kinds of real programs Concrete should eventually be able to implement well, not only toy examples or micro-benchmarks.

These workloads matter because they pressure the language and compiler in ways small tests do not:

- runtime and allocation behavior
- stdlib completeness and coherence
- formatter and diagnostics quality on real code
- backend performance and stability
- report and audit usefulness on nontrivial programs
- later, proof and specification workflows on real executable code

## Why Keep A Showcase List

Concrete should not only ask "can this feature exist?" It should also ask:

- what real programs should justify the language?
- what workloads should become stress tests for the compiler and stdlib?
- what examples should demonstrate that Concrete is good for explicit low-level work rather than only for small demos?

That is especially important for a language whose identity is auditability, explicit authority, and proof-friendly low-level programming.

## Artificial Life Simulation

One especially interesting showcase target is an implementation in the spirit of Rabrg's [`artificial-life`](https://github.com/Rabrg/artificial-life) reproduction of *Computational Life: How Well-formed, Self-replicating Programs Emerge from Simple Interaction*.

The workload shape is:

- a `240 x 135` grid
- each cell contains a 64-instruction Brainfuck-like program
- neighboring programs are randomly paired
- their instruction tapes are concatenated
- the combined program is executed for a bounded number of steps
- the tapes are split back apart afterward
- programs can loop and mutate program tapes, including themselves
- self-replicating programs can emerge and spread across the grid

## Why This Workload Fits Concrete

This is a strong Concrete showcase because it pressures several important areas at once:

- explicit buffers and mutation
- low-level data representation
- collections and grid storage
- performance-sensitive tight loops
- deterministic or semi-deterministic simulation structure
- formatter, diagnostics, and tooling quality on a real program
- later, execution-cost analysis or proof work on bounded pieces of the simulation

It is also just interesting enough to be worth reading and running, which matters for a showcase program.

## What It Would Test

An implementation like this would be a good milestone once the relevant phases are mature enough:

- **Phase C**: stdlib/testing/tooling quality
- **Phase D**: backend stability and artifact/report usefulness
- **Phase E**: runtime/execution clarity
- later, selected proof and audit experiments over bounded components

This is not immediate compiler work. It is a target workload that helps keep the project honest about what "real low-level programming" should mean.

## Criteria For A Good Showcase Workload

A showcase workload is especially valuable if it:

- is interesting enough that people will actually run it
- stresses real language/compiler boundaries
- is large enough to find integration bugs
- is small enough to remain understandable
- can later support report, audit, or proof experiments

Concrete should gradually accumulate a small set of these, not a giant benchmark zoo.
