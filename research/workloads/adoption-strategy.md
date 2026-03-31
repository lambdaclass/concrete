# Adoption Strategy

Status: exploratory

This note covers the missing layer between "the compiler/language is architecturally strong" and "people can quickly understand why to use it, try it, and remember it."

Concrete already has a strong internal story:

- explicit authority and trust boundaries
- ownership and linearity
- audit/report outputs
- a compiler written in Lean 4
- a credible long-term proof direction

That is not yet the same thing as user pull.

To create user pull, the project needs a deliberate adoption story:

- a clear signature-domain strategy
- memorable public examples
- smooth first-use and onboarding
- a stable/experimental surface users can understand quickly
- clear positioning against adjacent systems languages
- explicit non-goals so the language does not blur itself into a generic pitch

## Why This Matters

Technically coherent languages still fail when:

- nobody can tell what they are especially good for
- the first experience is too rough
- the only proof of seriousness is the compiler internals
- project identity is understandable only after reading many internal docs

Concrete should not optimize for broad popularity through feature growth.
It should optimize for a sharper kind of adoption:

- the right users understand the value quickly
- the first serious examples are memorable
- the language feels intentional and coherent
- the trust/audit story is visible without reading the compiler

This is not a marketing layer on top of the language.
It is a product-clarity layer around the language's real design.

## The Adoption Problem

Today, Concrete is strongest at:

- auditability
- explicit authority
- explicit trust boundaries
- proof-friendly compiler structure

But potential users still need to answer a simpler question:

Why should I try this instead of Rust, Zig, C, or Austral?

The answer cannot only be:

- "the compiler is in Lean"
- "the roadmap is rigorous"
- "the language is proof-friendly"

It also needs visible product pull.

## Signature Domains

Concrete should choose one or two primary domains where it wants to be unusually strong.

Good candidates:

- high-integrity systems components
- audit-heavy infrastructure/tools
- security-sensitive low-level utilities
- deterministic service components
- mission-critical policy and control kernels

These domains fit Concrete's current direction because they reward:

- explicit authority
- explicit resource ownership
- traceable trust boundaries
- explainable reports
- later proof/evidence integration

Concrete should avoid trying to position itself as a general "better systems language for everything."

That would blur the identity.

### Domain Selection Criteria

A good signature domain should reward most of Concrete's existing strengths at once:

- explicit authority
- explicit ownership/resource boundaries
- audit/report value
- trust-boundary visibility
- later proof/evidence leverage

A bad signature domain would mostly reward things Concrete is not currently optimizing for:

- broad ecosystem/network effects
- maximal throughput benchmarking alone
- heavy metaprogramming
- broad portability promises before the target/package story is mature

### Working Domain Shortlist

The current best candidates look like:

1. audit-heavy infrastructure and systems utilities
2. high-integrity components with explicit runtime/authority restrictions
3. small mission-critical policy/control components inside larger systems

These are better than "general systems programming" because they give Concrete a sharper answer to:

why this language?

### Mission-Critical Examples

If Concrete earns its full roadmap, the most compelling mission-critical use cases are likely to be small, high-consequence components rather than entire giant platforms. Examples include:

- secure boot, update, and attestation verifiers
- cryptographic control-plane or key-use policy engines
- spacecraft or satellite command gatekeepers
- industrial control safety interlocks
- medical-device policy kernels
- privileged installer or deployment verification helpers
- audited wrappers around critical C libraries, device APIs, or firmware interfaces
- cross-domain or high-assurance data-release policy engines

These are attractive because they reward several Concrete strengths at once:

- narrow authority
- visible trust boundaries
- explicit runtime/resource behavior
- reviewability
- later proof/evidence leverage

They also keep the language honest about scope: Concrete is more likely to justify itself as the language for small critical kernels than as the language for every surrounding subsystem.

## Showcase Strategy

Concrete needs public examples that are:

- larger than toy snippets
- smaller than full production products
- memorable enough to communicate identity

The best showcase set is probably:

1. one serious systems utility
2. one audit-heavy/report-heavy example
3. one unusual or memorable workload

These are not only tests.
They are public demonstrations of what the language is for.

Examples:

- capability-audited file/network tool
- bounded/high-integrity data-processing component
- the artificial-life showcase already discussed elsewhere

### Showcase Selection Rules

A showcase should ideally demonstrate several Concrete-specific strengths at once:

- capability reporting
- trust-boundary visibility
- meaningful ownership/resource flow
- a real stdlib/runtime story
- enough size to feel serious

It should not be selected only because it compiles.

### Recommended Showcase Portfolio

The strongest first portfolio is probably:

1. **Audit Utility**
   A systems tool where the report story matters visibly.

2. **High-Integrity Example**
   A bounded/restricted component where the execution/safety profile matters.

3. **Memorable Stress/Identity Example**
   Something unusual enough to be remembered, such as the artificial-life direction.

This gives Concrete:

- one practical example
- one assurance-oriented example
- one identity-forming example

## First-Use Experience

A strong language still loses adoption if the first-use loop is rough.

Concrete should treat the following as product surfaces:

- install/build instructions
- starter project template
- small idiomatic examples
- report UX
- editor support
- how quickly users can compile, run, and inspect something real

The first-use loop should answer:

- how do I start a project?
- how do I add modules and later packages?
- how do I run tests?
- how do I inspect capabilities/layout/unsafe boundaries?
- what is idiomatic Concrete?

### First-Use Standard

The first serious user path should be short:

1. build or install the compiler
2. create or open a small starter project
3. run it
4. run tests
5. inspect at least one report
6. understand what parts of the experience are stable vs evolving

If that path is confusing, adoption will lag even if the architecture is good.

## Public Stability Surface

Adoption is much easier when users know what is stable.

Concrete should explicitly separate:

- stable enough to rely on now
- implemented but still evolving
- experimental
- intentionally deferred

This matters especially for:

- reports
- package/project model
- runtime expectations
- FFI/ABI guarantees
- proof/evidence surfaces

Without this, every user has to infer stability from commit history.

### Stability Surface Categories

The project should probably standardize on a small set of labels:

- **Stable enough to build on**
- **Implemented but evolving**
- **Experimental**
- **Research only / not committed**

This should apply consistently across:

- language surface
- reports
- runtime expectations
- package/project model
- proof/evidence surfaces
- long-term concurrency work

## Positioning

Concrete should explain its position relative to nearby languages directly.

Likely shape:

- compared to Rust: smaller surface, more explicit trust/authority story, less ecosystem breadth
- compared to Zig/C: stronger ownership/effect/audit story, less "just write whatever low-level code you want"
- compared to Austral: similar explicitness spirit, but with stronger Lean/proof/compiler-evidence direction

Good positioning is not about claiming to beat everything.
It is about saying where Concrete is strongest and what tradeoffs it makes on purpose.

### Positioning Rules

Good positioning for Concrete should:

- name adjacent languages honestly
- acknowledge where they are stronger
- explain the tradeoff instead of denying it
- return to Concrete's actual strengths: auditability, authority/trust visibility, proof-friendly structure, and explicit runtime semantics

Bad positioning would sound like:

- "Concrete is just a better Rust"
- "Concrete can do everything but more safely"
- "the Lean implementation automatically makes everything trustworthy"

## Practical Adoption Deliverables

The first useful adoption push probably needs:

1. a one-page "why Concrete" document for the right users
2. 2-3 serious public examples
3. a first-project template
4. a clear stable vs experimental surface
5. a short "Concrete vs adjacent languages" positioning doc

### Phase-Level Success Signals

This phase is succeeding when a technically serious new user can:

- identify Concrete's target domains quickly
- run one public showcase without reading internal compiler docs
- understand what reports are for and see one useful report output
- understand what is stable vs evolving
- explain why Concrete exists without reducing it to "compiler in Lean"

### What Should Not Count As Adoption Progress

These should not be mistaken for real progress:

- adding many examples with no clear signature use
- broadening the pitch until it sounds like every other systems language
- counting internal regression tests as public showcases
- claiming stability without naming boundaries
- adding features mainly to make demos look broader

## Relationship To The Roadmap

This note maps to the roadmap's adoption/product phase.

It should sit after package/dependency semantics become clearer, because users need a coherent project model before adoption work becomes believable.

It should sit before full operational maturity, because successful adoption pressure should help shape what the operational surfaces actually need to support.

It should also stay before the long-term concurrency maturity phase.
The first convincing Concrete story should not depend on the project already solving the entire concurrency design space.

## What This Note Is Not

This note is not a call for:

- broad marketing work
- feature growth for popularity
- flattening Concrete into a generic mainstream systems language
- papering over unstable areas by pretending they are settled

## Explicit Non-Goals

Concrete should not use this phase to chase:

- ecosystem-size competition
- generic benchmark-driven positioning
- feature-count comparison as a primary public metric
- premature promises about portability, self-hosting, or concurrency breadth
- showcase programs that hide the language's real tradeoffs

The point is sharper:

make the language's real strengths legible, usable, and memorable.
