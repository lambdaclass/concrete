# Evidence Bundles And Review Workflows

Status: open

Concrete's biggest unrealized differentiator may not be another language feature.
It may be turning reports, artifacts, policies, and proofs into one visible review workflow.

## Core Claim

Concrete already has most of the raw pieces:

- capability / authority reports
- allocation and layout reports
- trusted / FFI boundary reporting
- explicit artifact boundaries
- proof-oriented architecture
- high-integrity profile direction

The missing leap is packaging these into a coherent evidence product.

## Evidence Bundle Shape

A long-term evidence bundle could include:

- source hash or source identity
- compiler version
- target / build profile
- artifact IDs
- authority summary
- allocation summary
- layout / ABI summary
- trusted / FFI boundary summary
- proof references or proof-subject references
- machine-readable report outputs

The point is to let a reviewer answer:

- what code was built?
- what authority did it require?
- where did it allocate?
- where did it cross trust or foreign boundaries?
- what changed relative to the previous version?
- what proof-facing evidence, if any, corresponds to it?

## Report-First Review Workflow

Concrete could become unusually strong if review is not just “read the code,” but:

- run a review command or CI workflow under a profile
- inspect authority / alloc / layout / trusted / FFI / proof surfaces together
- fail builds on policy violations

This is not a second semantic system. It is a stronger consumer workflow over compiler facts that already exist or are planned.

## Trust Drift Diffing

Once packages and artifacts are more mature, Concrete should be able to show review-relevant drift such as:

- this dependency gained `Network`
- this release now allocates in these functions
- this FFI type layout changed
- this trusted boundary expanded

This is especially strong for supply-chain and certification-style review.

## Machine-Readable Reports

Human-readable reports are necessary, but not sufficient.

Concrete should eventually emit stable machine-readable outputs for at least:

- capabilities / authority
- allocation
- layout / ABI
- proof eligibility
- trusted / FFI boundaries

That would let CI, IDEs, policy tooling, audit dashboards, and certification workflows consume the same compiler facts.

## Verified FFI Envelopes

Concrete already points toward stronger FFI structure. This deserves continued emphasis because it fits the evidence story well:

- foreign boundaries are explicit
- review surfaces can include ABI/layout facts
- policy workflows can reason about where FFI enters the system

## What This Is Not

This is not a call for:

- more type-system cleverness
- richer effect inference
- macros or metaprogramming
- broad abstraction growth

Those are much more likely to blur the language identity than to strengthen it.

## Roadmap Implication

The main long-term opportunity is:

- not “add one more language feature”
- but “turn reports, artifacts, policies, and proofs into one operational evidence workflow”

That should become increasingly explicit in the formalization, package, and operational-maturity phases.

## Explicit Roadmap Placement

- **Phase I**: proof-carrying audit artifacts and the first proof-facing review outputs
- **Phase J**: authority budgets as package/subsystem build contracts
- **Phase K**: flagship showcase workloads and public review narratives
- **Phase L**: machine-readable reports, report-first review workflows, reproducible trust bundles, and trust-drift diffing
- **Phase O**: any stronger evidence-gated features that still need research before they become stable commitments
