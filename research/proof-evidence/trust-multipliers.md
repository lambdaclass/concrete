# Trust Multipliers

**Status:** Open
**Affects:** Runtime model, reports, proofs, packaging, operational evidence
**Date:** 2026-03-14

## Purpose

This note collects a small set of ideas that could make Concrete unusually strong as an audit-oriented, proof-friendly low-level language.

These ideas are not random feature requests.

They reinforce existing Concrete directions:

- explicit capabilities and trust boundaries
- reusable compiler artifacts
- proof-oriented validated Core
- audit/report outputs
- reproducible, reviewable builds

## The Main Ideas

### 1. Proof-backed authority reports

Concrete already has capability reports and "why" traces.

The stronger direction is to connect those reports to:

- validated Core facts
- proof eligibility
- trusted/foreign assumptions that remain unproved
- later, selected proof artifacts for specific functions

The goal is not "prove everything now." The goal is:

- make authority reports stronger than plain compiler narration
- show what was mechanically checked
- show what still rests on trusted or foreign boundaries

This fits best in:

- **Phase F**: capability and safety productization
- later **Phase I**: evidence and traceability

### 2. Authority budgets as build contracts

Concrete can go beyond reporting what authority a function or package uses.

It can also let a package, subsystem, or binary declare what authority it is allowed to require at all.

Examples:

- a parser package may require no ambient authority
- a CLI package may require `File` but not `Network`
- a high-integrity binary may reject `Process` entirely

This turns authority review into enforceable project policy instead of only inspection after the fact.

This fits best in:

- **Phase F**: authority wrappers, aliases, safety ergonomics
- **Phase H**: package/subsystem authority budgets

### 3. Verified FFI envelopes

`extern fn` is one of the riskiest trust boundaries in a low-level language.

Concrete could strengthen that boundary by moving from "raw extern declarations" toward explicit FFI envelopes that describe:

- ABI assumptions
- layout assumptions
- ownership transfer
- destruction expectations
- capability requirements
- trusted/unsafe justification

The goal is not to prove all foreign code.

The goal is to make the boundary around foreign code explicit, auditable, and eventually more checkable.

This fits best in:

- **Phase E**: runtime/FFI boundary and ABI maturity
- later **Phase F**: visibility of trusted/unsafe/foreign edges

### 4. Structural boundedness reports

Before trying full execution-cost analysis, Concrete could report structural boundedness facts such as:

- recursion present or absent
- dynamic allocation present or absent
- loop nesting depth
- reachable FFI calls
- blocking I/O reachability
- unbounded collection growth
- dynamic dispatch absence/presence

This is a natural extension of the current report system.

It is cheaper and more honest than pretending to have precise WCET or exact cost models too early.

This fits best in:

- **Phase E**: execution profiles and analyzable runtime constraints
- later report expansion work

### 5. Reproducible trust bundles

Concrete could eventually emit a review bundle tying together:

- source hash
- compiler commit/version
- target and build options
- reports
- proof references
- ABI/layout assumptions
- emitted artifact identities

This would let audits, CI, and high-integrity review workflows operate on one coherent package of evidence rather than scattered outputs.

This fits best in:

- **Phase I**: operational maturity, evidence, and traceability

### 6. Capability sandbox profiles

Concrete's capability model could become stronger through explicit profiles such as:

- `no_alloc`
- bounded allocation
- `no_ffi`
- restricted `trusted`
- no ambient authority

The key is that these would be compiler-enforced profiles, not just style advice.

This fits best in:

- **Phase E**: execution restrictions and high-integrity execution profiles
- **Phase F**: high-integrity safety profile

### 7. Serious showcase workloads

Concrete should have a small set of real showcase programs that pressure:

- runtime boundaries
- collections and buffers
- multi-module behavior
- report usefulness
- later, proof and evidence workflows

The best showcase workloads are not only "big examples."

They are recognizable, stress real boundaries, and remain understandable enough to audit.

This fits best in:

- later **Phase E** / **Phase I**
- long-horizon workload and demonstration planning

### 8. Machine-readable reports and report-first review workflows

Human-readable reports are not enough if Concrete is meant to support CI, review tooling, certification-style workflows, or durable audit artifacts.

The stronger direction is:

- stable machine-readable report outputs
- review commands or CI modes that evaluate authority, allocation, layout, trusted, FFI, and proof-facing facts together
- policy failures based on those compiler facts rather than ad hoc scripts

This fits best in:

- **Phase L**: operational report surfaces and review workflows
- later **Phase O** for any stricter evidence-gated policy layers that still need research

### 9. Trust-drift diffing

Once packages, reports, and evidence outputs are more stable, Concrete should be able to answer review questions like:

- what new authority did this package gain?
- where did allocation appear or disappear?
- what FFI layout or trusted boundary changed?

This is a natural extension of evidence bundles and machine-readable reports, but important enough to call out directly.

This fits best in:

- **Phase J**: authority-aware package/dependency policy direction
- **Phase L**: maintained package/release trust-drift diffing

## Why These Ideas Belong Together

These ideas form a coherent stack:

1. capability sandbox profiles restrict what code may do
2. authority budgets restrict what subsystems may require
3. verified FFI envelopes make foreign boundaries explicit
4. structural boundedness reports make execution risk visible
5. proof-backed authority reports strengthen trust claims
6. reproducible trust bundles package the evidence into something reviewable
7. machine-readable reports and review workflows make the evidence consumable
8. trust-drift diffing makes version-to-version review practical

That stack is more aligned with Concrete than a random collection of new language features.

## Relationship To Existing Notes

This note depends on or extends:

- [authority-budgets.md](authority-budgets.md)
- [capability-sandboxing.md](capability-sandboxing.md)
- [execution-cost.md](execution-cost.md)
- [proof-evidence-artifacts.md](proof-evidence-artifacts.md)
- [showcase-workloads.md](showcase-workloads.md)
- [high-integrity-profile.md](high-integrity-profile.md)

## Recommended Roadmap Placement

- **Phase E**: verified FFI envelopes, capability sandbox profiles, structural boundedness reports
- **Phase F**: proof-backed authority reports, stronger authority ergonomics, wrapper/alias patterns
- **Phase H**: package/subsystem authority budgets
- **Phase I**: reproducible trust bundles and proof-facing evidence packaging
- **Phase J**: authority-aware package policy and early trust-drift direction
- **Phase L**: machine-readable reports, report-first review workflows, and maintained trust-drift diffing
- **Later / long horizon**: showcase workloads as end-to-end demonstrations and stress targets

## What Not To Do

- do not turn these into a giant parallel metadata system disconnected from compiler facts
- do not promise full proofs or full WCET too early
- do not add new language surface when report/tooling/artifact work can carry the same value more cleanly
- do not scatter this work across phases without keeping the runtime/safety/package/evidence sequencing coherent
