# Thesis Validation

**Status:** Open

This note defines the main experimental track for Concrete after real-program validation.

The project has already shown that the language can express and compile serious programs. The next question is whether Concrete's deepest ideas are real enough to justify the project:

1. capability-visible architecture
2. bounded / predictable execution
3. proof-backed evidence tied to the compiler pipeline

This is not the same as "make the language broadly comfortable first." It is the phase where Concrete tests the ideas that would make it distinctive.

## What This Phase Is Trying To Prove

Concrete should be able to demonstrate, in implementation rather than aspiration, that:

1. operational restrictions can be enforced, not merely documented
2. execution-relevant facts can be surfaced mechanically
3. a restricted execution profile can be made precise
4. proof-backed evidence can attach to selected user-facing code and compiler reports
5. determinism, cleanup cost, and host-call opacity are visible enough that "predictable" does not hide obvious blind spots
6. the claimed reports, checks, and proofs survive adversarial examples designed to break or confuse them

## Main Workstreams

The thesis-validation track breaks into four workstreams:

1. `NoAlloc` and bounded-allocation foundations
2. operational/trust effect reporting
3. first predictable-execution profile
4. proof-backed evidence on a restricted fragment
5. validation of determinism, cleanup cost, and host-call classification
6. adversarial validation of every thesis-level claim

## What Success Looks Like

This phase is successful when Concrete can point to a small but credible demonstration set showing:

1. functions rejected for violating `NoAlloc`
2. compiler reports identifying blocking, FFI, trusted use, recursion, unknown loop bounds, and allocation class
3. a predictable-execution profile that rejects code structurally outside the profile
4. selected user-facing functions with proof-backed claims connected to the actual compiler pipeline
5. explicit answers on four thesis-level invariants:
   - whether lowering/codegen can silently introduce allocation
   - whether any analyzed path can escape through indirect calls
   - what boundedness guarantees apply on failure paths
   - where LLVM/backend timing assumptions begin
6. explicit answers on four practical predictability questions:
   - whether a function is deterministic with respect to time, randomness, and iteration order
   - whether cleanup work is bounded on success paths
   - whether cleanup work is bounded on failure paths
   - whether host calls are classified as blocking, timing-opaque, or trust-boundary crossings
7. adversarial suites for the main thesis claims:
   - predictable-profile gates
   - effects/evidence reporting
   - proof/report linkage
   - flagship-example regressions

## Relationship To Other Notes

This note is an umbrella for the thesis-validation slice.

Supporting notes:

1. [noalloc-enforcement.md](noalloc-enforcement.md)
2. [boundedness-reports.md](boundedness-reports.md)
3. [proof-slice.md](proof-slice.md)
4. [validation-examples.md](validation-examples.md)

Related broader notes:

1. [../predictable-execution/predictable-execution.md](../predictable-execution/predictable-execution.md)
2. [../predictable-execution/effect-taxonomy.md](../predictable-execution/effect-taxonomy.md)
3. [../compiler/backend-traceability.md](../compiler/backend-traceability.md)
4. [../proof-evidence/proving-concrete-functions-in-lean.md](../proof-evidence/proving-concrete-functions-in-lean.md)

## Bottom Line

Concrete now needs to validate its thesis, not just its syntax and stdlib surface.

That means:

1. enforce one real operational restriction
2. report one real class of execution uncertainty
3. define one real restricted profile
4. prove one real class of claims over real user-facing code
5. show that the proofs and reports remain usable after at least one real refactor
6. show that the compiler resists targeted counterexamples intended to confuse or bypass the thesis claims
