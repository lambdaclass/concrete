# High-Integrity Profile For Concrete

**Status:** Open

This note sketches what an explicit high-integrity or provable profile for Concrete could mean.

The goal is not to create a different language. The goal is to define a stricter way to write Concrete for code that must be easier to audit, analyze, constrain, and eventually prove.

## Why This Matters

Concrete already has strong ingredients for higher-assurance code:

- explicit capabilities
- explicit `Unsafe`
- explicit `trusted` boundaries
- explicit allocation
- explicit FFI
- a proof-oriented compiler architecture centered on validated Core

Those pieces are valuable, but they do not yet add up to a clearly defined stricter mode for critical code.

A high-integrity profile would give the project:

- a place where safety, runtime restrictions, and proof goals line up
- a stronger story for critical systems without forcing all Concrete code into maximum restriction
- a cleaner bridge between ordinary systems programming and review-heavy or certification-heavy use

## What It Is

The profile should be understood as:

- **same language**
- **stricter rules**
- **explicit compiler/tooling awareness**

In other words:

- normal Concrete remains practical and expressive
- high-integrity Concrete is Concrete under tighter, explicit restrictions

## What It Might Restrict

The exact shape is future work, but the most likely restrictions are:

- no `Unsafe`, or only tightly scoped/approved wrappers
- no unrestricted FFI
- no dynamic allocation, or only bounded-allocation profiles
- no ambient authority growth
- a more analyzable concurrency model than "threads are available, therefore they are allowed"
- stronger reporting, traceability, and evidence requirements

This is intentionally broader than "add more proof annotations." The high-integrity profile is about operational analyzability as much as formal reasoning.

## Normal Concrete vs High-Integrity Concrete

The simplest mental model is:

- **normal Concrete**: full practical low-level language
- **high-integrity Concrete**: the same language under explicit restrictions for code that must be easier to trust

That is similar in spirit to SPARK's relationship to Ada, without implying that Concrete should copy Ada's surface design or prover model.

## What The Profile Should Require

For the profile to be real rather than rhetorical, it should eventually come with:

1. **Compiler-recognized restrictions**
   - not only style guidance
   - actual profile-aware checks

2. **Profile-aware reports**
   - why this code qualifies
   - where it crosses or fails the stricter rules

3. **Project/package visibility**
   - profile membership should be visible at more than function scope

4. **A clear relation to the proof story**
   - selected-function Lean proofs should fit naturally inside the profile
   - the profile should reduce proof-hostile features, not add them

## What The Profile Should Not Be

It should not be:

- a giant contract system by default
- a second language
- a justification for uncontrolled feature growth plus "safe mode" later
- a vague marketing term without compiler and report consequences

## How It Relates To The Roadmap

This idea is split across several later phases:

- **Phase E**: execution restrictions, bounded/no-allocation modes, analyzable concurrency constraints
- **Phase F**: stricter authority, `Unsafe`, `trusted`, and FFI boundaries
- **Phase G**: an explicit critical/provable subset as a language-design commitment
- **Phase I**: evidence, traceability, and operational review workflows

So the high-integrity profile is not one isolated feature. It is a cross-phase synthesis of runtime restrictions, safety restrictions, language discipline, and evidence.

## Why This Fits Concrete

This direction fits Concrete better than a large contract system as a first move because it preserves the project's strongest current ideas:

- explicitness over hidden machinery
- smaller trusted surfaces
- analyzability over convenience
- low-level control without pretending everything is universally safe

If Concrete succeeds here, it would offer something unusual:

- a practical low-level language for ordinary work
- and a clearly stricter profile for code that must be easier to audit, constrain, and prove
