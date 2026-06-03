# Graded Modal Types

**Status:** Research only — not a Concrete v1 feature
**Affects:** type system, capabilities, contracts, audit reports, synthesis
**Date:** 2026-06-03

## Summary

Graded modal types attach a quantity to a type-level modality. Instead of only
saying "this value is linear" or "this function needs `File`", the type can
say how many times a resource is used, or with what budget:

```text
uses key exactly 1 time
reads buffer at most N times
allocates at most K cells
calls capability C zero times
```

This is an active research area, and it is useful background for Concrete
because Concrete already cares about authority, allocation, proof status, and
bounded behavior. In a sufficiently mature system, quantitative resource facts
could support stronger static checking and even synthesis: a user states a
resource budget, and the compiler/search constructs code that satisfies it.

That is not the right move for Concrete now.

## Why It Is Tempting

Grading could express properties Concrete eventually wants to report or prove:

- a key is consumed exactly once;
- a callback is invoked at most once;
- a loop performs exactly `N` reads;
- a parser allocates at most `K` bytes;
- a function never uses `File`, `Network`, or `Unsafe`;
- a bounded data structure preserves a capacity budget.

The connection to synthesis is also real. If the type carries a resource
budget, search can be constrained by the budget. This is powerful in languages
where the type system is the main specification surface.

## Why It Does Not Fit Concrete Now

Concrete's current design is deliberately less abstract:

- authority is explicit through capabilities;
- ownership is explicit through linear values;
- proof claims live in source contracts and audit artifacts;
- obligations are discharged by Lean, kernel decision procedures, runtime
  checks, or explicit assumptions;
- trust classes are visible in reports.

Graded modal types would add a new type-system layer before the current
evidence pipeline has finished becoming usable. That is the wrong order. It
risks making ordinary code harder to read and making authority/resource facts
less obvious to reviewers.

Concrete's current bottleneck is not "the type system cannot express enough."
It is:

- generated contract VCs are still maturing;
- `ghost` state is not landed yet;
- `concrete prove <function>` does not exist yet;
- runtime safety obligations are still early;
- external validation by a non-author has not happened yet.

Adding graded modalities before those pieces are stable would be abstraction
before evidence.

## Concrete-Compatible Direction

If this idea ever becomes useful, the Concrete version should start as audit
and contract data, not as a core type-system feature.

Possible first surface:

```text
allocations <= N
calls(f) <= K
reads(buf) <= N
uses(File) == 0
consumes(resource) == 1
```

These should first appear as:

1. compiler reports;
2. source contracts / VCs;
3. runtime or oracle checks where needed;
4. only later, if repeatedly forced, a typed annotation.

That keeps the project aligned with its main rule: evidence should be local,
automatic where possible, and auditable by class.

## What Not To Add

- Do not add general modal-type syntax to Concrete v1.
- Do not add graded capabilities as hidden inferred effects.
- Do not replace object-capability signatures with graded effect rows.
- Do not make resource budgets implicit in type inference before they are
  visible in audit output.
- Do not add synthesis as a language promise before proof/contract authoring is
  usable by non-authors.

## Revisit Trigger

Revisit graded modalities only if all of these are true:

1. source contracts and `ghost` state are in regular use;
2. `concrete prove <function>` can generate useful proof stubs;
3. runtime/resource obligations are reported per function;
4. at least two real programs repeatedly need quantitative resource claims that
   are awkward as contracts or reports;
5. the proposed notation keeps authority and resource use obvious at the call
   site and in `concrete audit`.

Until then, the right Concrete feature is **quantitative resource/evidence
reporting**, not graded modal types.

## Relationship To Existing Design

This note does not change:

- the no-row-effects decision;
- the object-capability model;
- linear ownership;
- source contracts;
- ProofKit / VC generation;
- audit evidence classes.

It records a research influence and a deliberate non-adoption decision for now.
