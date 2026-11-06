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
- proof / spec / result registries loaded by the compiler or review tooling
- optional references to proofs or proof obligations

all tied together clearly enough that a reviewer can answer:

- what code was built?
- what authority did it require?
- what trust boundaries did it cross?
- what proof artifacts, if any, correspond to it?

## Proof-Aware Package Artifacts

The natural later extension is that packages do not just ship source and a
binary, but also ship evidence artifacts.

The useful package-level artifact set would eventually include:

- machine-readable fact snapshots
- predictable/profile results
- proof status
- obligation status
- trusted assumptions
- policy declarations or authority budgets
- artifact/schema version
- identities or fingerprints tying all of the above to the checked code

This matters because Concrete's trust story becomes much stronger once
dependency review can ask:

- what authority does this package require?
- what is only reported versus enforced versus proved?
- what trusted assumptions does this package rely on?
- what changed between package versions?

That is much stronger than treating proof/evidence as a local compiler side
channel.

## Avoid Hardcoded Proof Evidence

Hardcoded proof registries inside the compiler are acceptable only for early
thesis validation.

The durable direction is external, reproducible proof evidence:

- function identity
- source/build identity
- checked-Core or ProofCore identity
- body fingerprint
- spec identity
- obligation identity
- proof-result identity
- trusted assumptions, if any

Compiler reports can then consume the artifact and say `proved` without making
the compiler source code itself the proof database.

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
- later package/workspace artifact discipline

## What Not To Do

The project should avoid treating this as:

- a giant new proof framework before the simpler Lean workflow exists
- a replacement for readable human-facing reports
- a parallel metadata system disconnected from compiler facts

The right direction is:

- one set of compiler facts
- multiple consumers: human reports, tooling, CI, audits, and later proofs
