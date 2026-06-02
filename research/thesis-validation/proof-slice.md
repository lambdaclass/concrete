# Proof Slice

**Status:** Open

This note defines the first proof target for thesis validation.

The goal is not "prove arbitrary Concrete programs." The goal is to prove a meaningful restricted fragment over selected user-facing functions and selected report claims.

## Why A Slice Matters

If Concrete tries to jump straight to "formal verification of any function," the effort will sprawl and the result will be hard to explain.

A proof slice keeps the problem credible:

1. choose a narrow fragment
2. prove useful claims
3. connect those claims back to source and reports

## What To Prove First

The first proof targets should likely be:

1. small user-facing pure functions in the provable subset
2. parser and validator safety properties on bounded systems cores
3. selected report properties such as allocation absence or simple authority facts
4. traceability from source function to extracted proof object

This is enough to show the thesis without claiming universal proof coverage.

The strongest next step after the first helper-level proof is:

1. one real packet-decoder parser-core theorem
2. one visible `proved` entry in the effects report
3. one quick proof-maintenance check after a refactor

## Candidate Scope

Likely restrictions for the first slice:

1. no FFI
2. no trusted regions
3. empty or tightly restricted capability set
4. simple structured control flow
5. proof-friendly extracted Core representation

## What The Demo Should Show

Concrete should be able to demonstrate:

1. a user-facing function
2. its extracted proof representation
3. a Lean theorem over that function
4. a clear statement of what layer the theorem is about

The point is evidence with traceability, not theorem-count inflation.
