# Semantic Diff And Trust-Drift Tooling

Status: open

This note turns an existing long-term idea into a more concrete tool direction:

- Concrete should eventually diff semantics and trust-relevant artifacts, not only source text

This is not a new language feature.
It is an operational tool over compiler facts that already exist or are planned.

## Why This Fits Concrete

Concrete already wants to expose:

- authority requirements
- trusted and foreign boundaries
- allocation behavior
- layout and ABI facts
- proof-facing evidence
- package and artifact identity

A semantic diff tool would let users compare those facts across:

- two commits
- two releases
- two package versions
- two evidence bundles

That is unusually aligned with Concrete's audit-first identity.

## The Core Idea

Instead of only:

- `git diff`
- changelog summaries
- manual inspection of reports

Concrete should eventually support something like:

```text
concrete diff old new --trust-impact
```

with output shaped around review questions such as:

- what new authority did this component gain?
- what trusted boundaries were added or removed?
- where did allocation appear or disappear?
- what layout or ABI facts changed?
- what proof obligations, proof references, or proof status changed?

## Good First Targets

The first version should stay narrow.

It should compare:

- authority/capability requirements
- allocation summaries
- trusted / `Unsafe` / FFI boundary summaries
- layout / ABI summaries

These are already close to the existing report surfaces.

## Later Targets

Later versions could also compare:

- proof eligibility
- explicit proof obligations
- proof-session status
- package-level authority budgets
- dependency trust drift
- evidence-bundle contents

## Why This Is Better Than A New Language Feature

This idea strengthens Concrete without changing the source language.

It:

- builds on existing compiler facts
- reinforces auditability
- helps CI and release review
- supports certification-style workflows later

It does not:

- add syntax
- add inference
- add hidden semantics
- weaken explicitness

That makes it exactly the kind of multiplier Concrete should prefer.

## Architectural Requirements

This tool depends on:

- stable artifact identity
- machine-readable report outputs
- deterministic report subjects
- package/release identity
- eventually, stable proof-facing subject identity

So the diff tool should be treated as a consumer of the artifact/report system, not as a parallel ad hoc analysis path.

## Review-Oriented Output

The tool should optimize for review questions, not raw data dumps.

Example categories:

- authority growth or reduction
- trusted-boundary expansion or removal
- allocation drift
- layout drift
- proof-status drift

The goal is not "show every AST change."
The goal is "show the changes that matter for trust, review, and compatibility."

## What Not To Do

Concrete should avoid:

- turning this into a generic semantic refactoring engine
- diffing facts that do not yet have stable identity rules
- adding new source-language features just to support the diff
- presenting low-level compiler noise instead of review-relevant drift

## Roadmap Placement

This fits best in:

- **Phase J**:
  - package identity and graph discipline
  - authority-aware package policy
- **Phase L**:
  - machine-readable reports
  - report-first review workflows
  - maintained trust-drift diffing

## Philosophy Check

This idea is a strong fit for Concrete because it:

- strengthens auditability
- strengthens explainability
- strengthens evidence workflows
- reuses compiler facts
- avoids language-surface drift

If it ever starts demanding new syntax, new inference, or a second semantic system, it is going in the wrong direction.
