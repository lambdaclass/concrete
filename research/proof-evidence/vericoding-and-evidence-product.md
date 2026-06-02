# Vericoding And Evidence Product

Status: open

This note records what Concrete should learn from the recent "vericoding" wave without copying its hype or its weakest assumptions.

The strongest lesson is not "LLMs can write verified code."
The strongest lesson is that verification becomes commercially real only when it looks like a product:

- human intent
- formal artifact
- machine check
- archived evidence

Concrete already has the beginnings of that stack:

- extracted ProofCore
- obligations
- fingerprints
- proof registry attachments
- proof status facts
- semantic diff / trust-drift direction
- explicit trusted / backend / target boundaries

The competitive question is not whether Concrete can imitate NL-to-Dafny demos.
It is whether Concrete can become the best system for **reviewable, artifact-backed, drift-aware verification of real systems code**.

## What The Vericoding Pitch Gets Right

The recent vericoding pitch is right about a few things:

1. Code generation is not the bottleneck anymore.
   Review, verification, and trust are the bottleneck.

2. "Tested" and "proved" are different categories.
   A serious system should say which one it has.

3. Verification has to feel operational.
   Build / check / prove / archive / audit must feel like one workflow, not a research appendix.

4. Evidence artifacts matter.
   A claim that cannot be archived, reviewed, diffed, and rechecked is weaker than it looks.

Concrete should keep all of those lessons.

## What Concrete Should Not Copy

Concrete should not win by claiming:

- natural language in, verified code out
- automatic formal specification from prose as the primary story
- cryptographic wrapping as a substitute for proof-UX quality
- broad correctness claims before the artifact and trust boundaries are mature

Those are attractive slogans, but they are not Concrete's strongest position.

Concrete's stronger position is:

- explicit capability and trust boundaries
- artifact-first evidence
- proof status with stale/missing/blocked/ineligible distinctions
- semantic diff and trust-drift review
- honest backend/toolchain/target assumptions
- Lean-backed proof attachments for selected user-code properties

## The Competitive Position Concrete Should Aim For

Concrete should aim to be the best system at:

1. proving small but real systems functions
2. showing exactly what was proved
3. showing exactly what changed
4. keeping trusted assumptions visible
5. turning verification artifacts into normal review and CI surfaces

That is a stronger niche than "AI writes verified programs from English."

It is also more compatible with Concrete's design:

- LL(1) parser
- small core language
- explicit authority
- explicit trusted boundaries
- artifact-backed reports
- Lean-attached proofs rather than solver-only claims

## Product Requirements To Win

### 1. Spec Quality Must Be Visible

Concrete should not stop at:

- proved
- stale
- blocked
- missing
- ineligible

It also needs a way to communicate when a proof-bearing surface is too weak to be meaningful.

Examples:

- a theorem only proves a tiny helper fact while the public docs imply a broader safety claim
- a function is "proved" against a narrow spec that leaves critical behavior unconstrained
- a proof attachment exists, but the surrounding review surface hides what the spec excludes

Concrete should eventually report:

- proof status
- claim scope
- claim strength / proof surface strength
- trusted assumptions that the claim depends on

### 2. Evidence Bundles Must Be First-Class

Concrete already has the ingredients for stronger evidence packaging.

The long-term product should include a stable evidence bundle containing at least:

- source identity
- function identity
- extracted ProofCore
- obligation identity and status
- theorem identifier
- fingerprint
- proof registry attachment
- diagnostics
- toolchain identity
- backend / target assumptions that matter for the claim

That bundle should be:

- reviewable
- archivable
- diffable
- attachable to CI and releases

### 3. Claim Scope Must Be Explicit

Every strong proof/evidence claim should answer:

- which function or artifact does this refer to?
- which property is proved?
- which assumptions are required?
- which boundaries are excluded?
- does this speak about source semantics, ProofCore semantics, enforced checker behavior, or compiled behavior under assumptions?

This is where many systems become blurry.
Concrete should be unusually explicit here.

### 4. Verification Must Be A Review Workflow

Concrete should treat these as one workflow:

- report
- query
- snapshot
- diff
- proof status
- stale detection
- proof attachment review
- trust widening review

The strongest version is not "you can prove code once."
It is "you can review what changed in trust, proof, and authority across revisions."

### 5. Outsider UX Must Be A Release Bar

The right standard is not "the compiler team understands the statuses."
It is:

- another engineer can read the output and understand it
- stale/blocked/missing states are actionable
- a reviewer can tell what the strongest honest claim is

If Concrete does this well, it will look operationally stronger than systems that technically verify more but explain less.

## What Concrete Should Change Or Add

### Add Or Strengthen In The Roadmap

1. Spec quality / underspecification reporting
   The system should eventually flag proof-bearing surfaces that are formally checked but too weak to justify the surrounding claim.

2. Stable evidence bundle
   The proof/debug/report bundle should become a stable public artifact, not only an internal debug aid.

3. Claim-strength taxonomy
   Concrete should distinguish:
   - proved property
   - enforced checker property
   - analysis-reported fact
   - trusted assumption
   - weak / underspecified proof surface

4. Proof / attachment / theorem diff workflow
   Semantic diff should grow beyond "fact drift" into proof-target drift, theorem attachment drift, and claim-scope drift.

5. Release evidence contract
   Flagship examples should ship with the evidence artifacts and assumptions needed for third-party review.

6. AI-assisted proof work only after artifact maturity
   AI may help write or repair proofs, but Concrete should never present "AI produced something proof-like" as a trust anchor.

## A Better Competitive Story Than "AI Wrote Verified Code"

Concrete should say something closer to:

> Concrete makes selected systems-code properties mechanically checkable, explicitly scoped, drift-aware, and reviewable through stable artifacts.

That is less flashy than the strongest vericoding slogans.
It is also more defensible.

## What "Top Notch" Would Look Like

Concrete becomes top notch here if it can show all of the following on one real example:

1. a real Concrete function with a Lean-backed property
2. a visible extracted proof target
3. a visible theorem attachment
4. a visible fingerprint and stale-detection path
5. a visible trust/assumption boundary
6. a diff that shows exactly what changed
7. a bundle a third party can archive and review

That is a stronger demonstration than most current "verified coding" stories.

## Non-Goals For Now

Concrete should explicitly avoid:

- pretending natural-language-to-spec is solved
- expanding proof claims faster than the proof workflow matures
- replacing proof workflow quality with cryptographic wrappers
- broadening the surface language just to look more like verification-first languages

The right path is:

- narrow
- explicit
- artifact-first
- honest
- reviewable

## Related Notes

- [Verification Product Model](./verification-product-model.md)
- [Proof Evidence Artifacts](./proof-evidence-artifacts.md)
- [Evidence Review Workflows](./evidence-review-workflows.md)
- [Proof UX And Authoring Loop](./proof-ux-and-authoring-loop.md)
- [Trust Multipliers](./trust-multipliers.md)
