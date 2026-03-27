# Proof And Evidence Carrying Artifacts

**Status:** Open

This note explores a stronger long-term artifact story for Concrete: compiler outputs that do not stop at binaries, IR, or human-readable reports, but can also carry proof- and review-relevant evidence.

## Why This Matters

Concrete's identity is not only "compile low-level code."

It is also:

- explain what the program is allowed to do
- explain where trust boundaries are crossed
- explain what was allocated, destroyed, monomorphized, and lowered
- eventually connect those facts to proofs

If the compiler can produce durable evidence artifacts, then:

- audit output becomes more operationally useful
- proofs become easier to tie back to the exact code and build
- reproducibility and review become stronger

## What Counts As Evidence

Examples of evidence-bearing outputs include:

- capability/authority summaries with why-traces
- `Unsafe` / `trusted` boundary summaries
- allocation and cleanup summaries
- layout/ABI facts
- monomorphization facts
- hashes or identities tying reports to source and build state
- later, references to proof artifacts or proof obligations

## Why This Is Different From "Just Reports"

Reports are already useful, but they are often treated as human-facing terminal output.

The stronger direction is:

- reports as stable artifacts
- artifacts tied to source/build identity
- artifacts that tooling, CI, review workflows, and later proof workflows can all consume

That is the point where Concrete starts to look less like "a compiler with some nice flags" and more like a trust-oriented build system.

## A Concrete Long-Term Shape

The eventual shape could look something like:

- source
- compiler artifacts
- audit reports
- build identity / hashes
- proof-oriented exports for selected functions
- optional references to proofs or proof obligations

all tied together clearly enough that a reviewer can answer:

- what code was built?
- what authority did it require?
- what trust boundaries did it cross?
- what proof artifacts, if any, correspond to it?

## Why This Fits Concrete

This idea reinforces several existing project directions:

- audit mode
- validated Core as a proof boundary
- reusable pipeline artifacts
- reproducible builds
- certification-style traceability

It is not a separate philosophical branch. It is where those threads converge.

## Relation To The Roadmap

This idea primarily lives across:

- **Phase D**: reusable pipeline artifacts, proof-oriented exports, formalization
- **Phase I**: operational evidence, traceability, reproducibility, review workflows

It also benefits from:

- **Phase C** report productization
- **Phase F** stronger authority/trust reporting

## What Not To Do

The project should avoid treating this as:

- a giant new proof framework before the simpler Lean workflow exists
- a replacement for readable human-facing reports
- a parallel metadata system disconnected from compiler facts

The right direction is:

- one set of compiler facts
- multiple consumers: human reports, tooling, CI, audits, and later proofs
