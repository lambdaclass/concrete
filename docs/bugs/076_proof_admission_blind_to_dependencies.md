# Bug 076 — proof admission could not see into dependencies, or through a type parameter

**Status:** FIXED 2026-10-06 (branch `r0484-admission`)
**Found:** 2026-10-06, while moving proof admission onto the shared assumption summary
(R-0484 R10). The old/new verdict comparison showed functions the old rule admitted that the
summary, and the reports built on it, already called incomplete.
**Severity:** a false admission. A function could count as EFFECT-FREE proof coverage while it
could reach an indirect call, or a foreign binding whose declared effects are only assumed. In
the corpus no affected function carried a registered proof, so no admitted proof was overstated.

## Mechanism

Admission refused a function only if it could reach an indirect call, according to
`effectOpaqueSet`, a fixpoint over ProofCore's call graph. That graph is built from the PROGRAM's
modules, so:

- an indirect call inside a dependency was invisible: `factlib.apply`, and std's `Writer::flush`
  and `Writer::close` calling through the writer's fn pointers;
- a foreign binding was never an admission fact. A function that reached `std.libc.memcmp`, or a
  fixture's `extern fn abs with()` through a `trusted` wrapper, was admitted, because it made no
  indirect call;
- a method call on a type parameter (`T_describe`) names no definition and was no edge at all,
  so a generic body's call to a per-instantiation impl looked like it called nothing.

The assumption summary built for the reports did not have these blind spots: it is built over
the program and its loaded dependencies, and it records reached bindings. So the reports and
admission disagreed about the same functions.

## Fixed

`extractProofCore` takes the assumption summary as a required input, from the same constructor
the reports use (`Assumptions.forProgram`). It admits an extractable function only when exactly
one summary covers it, that summary has no unresolved edge, and it reaches no foreign binding.
Each refusal is typed (`AdmissionRefusal`) and names its site. The summary classifies a
type-parameter method call as its own gap kind. An excluded obligation states
`admissible: false` instead of defaulting to true, and a reached key whose facts entry is
missing still refuses rather than disappearing. Corpus comparison:
`docs/verification/EFFECT_PROOF_BOUNDARIES.md` §4.2. Forty-three verdicts changed, all from
admitted to refused.

## Regression

`tests/regressions/proof_admission`, gated by `check_proof_admission.sh`. Three functions stay
admitted. A foreign binding reached through a trusted wrapper, an indirect call inside the
dependency, the same call reached transitively, and a type-parameter dispatch are all refused,
and refusal does not stop extraction. Admission must agree with the published summary facts for
every eligible function. Build-time `#guard`s in `ProofCore` cover the judgment. Six mutation
families in `test_mutation.sh` (86–91) remove one input each.
