# N2 slice 2: certificate dependency validation needs specification

Owner: R-0219. Implementation stopped before adding a solver or changing the
consumer. Independent reviewers: **do the best long-term thing rather than cut
corners**. The coordinator runs reviews of record; the writer ran none.

The construction rule in [N2-discharge-solver.md](N2-discharge-solver.md), lines
79–85, reconstructs a certificate without supplied proof assumptions, checks it
in Lean, and checks its permitted assumptions after substitution. This does not
specify how to validate the certificate's **global proof dependencies**. The
soundness requirement at lines 145–148 requires a certificate from permitted
hypothetical assumptions only. That requirement rules out the certificate below;
the missing part is a constructive validation protocol, not a proposed change
to the semantic policy or a counterexample to that soundness requirement.

## Exact counterexample

The standalone meta-level fixture is
[N2-Certificate-Origin-Repro.lean](fixtures/N2-Certificate-Origin-Repro.lean).
It imports Lean outside ConcreteSpec and contains:

```lean
axiom impossible : False
theorem injectedFalse : False := impossible
theorem reachableDischarge (_k : Nat) : 2 = 2 := False.elim impossible
```

The term under analysis is
`fun (_k : Nat) => (False.elim impossible : 2 = 2)`.
Its body is reachable; its unsupplied Nat binder is consistent. The opaque
False supplier must be rejected under the note's lines 22–27. No hypothetical
assumption can independently certify this body's unreachability.

Before registering `injectedFalse` as a simp theorem, the same context-refutation
procedure as the existing consumer finds no certificate in the empty local
context. After `attribute [local simp] injectedFalse`, it produces:

```text
of_eq_true injectedFalse._simp_1 : False
```

This proof has no metavariables or local assumptions and passes `Lean.Meta.check`.
Its global dependency `injectedFalse := impossible` is the supplied contradiction.
Kernel checking, checking the certificate's type, and checking its free local
assumptions do not detect that dependency. This is a syntactic-provenance issue,
not a request to test the consistency of the whole environment.

## Required design clarification

Specify how certificate global dependencies acquire origins, how their transfer
obligations are validated, and when the certificate may enter the checked cache.
The check must be non-circular: the certificate being validated cannot itself
exclude the obligation that detects its supplied impossible producer. Checking
only the original term's suppliers or the certificate's free variables is
insufficient. The clarification must preserve the existing joint-hypothetical
and F3b positives, reject this supplied dependency, share the original worklist
and budgets, and report unfinished validation as named INCOMPLETE. A library
trust list or an exception for simp-generated proofs would violate the note.

The long-term requirement is already the note's soundness condition; its
certificate dependency-validation protocol needs to be made explicit and
independently reviewed before implementing this slice. No OD answer is chosen
or requested here. No ID or roadmap row was allocated; R-0219 remains the owner.

## Focused evidence and limits

Run in the inherited Nix development environment, Lean 4.34.1:

```sh
bash scripts/checks/lib/bounded.sh 60 lean scripts/proofs/fixtures/N2-Certificate-Origin-Repro.lean
```

The fixture asserts the exact generated proof expression, False type, empty
local context, absence of metavariables, baseline certificate absence, and exact
body of the supplying theorem. The transcript is in
[the focused audit](audits/N2-slice2-certificate-origin.txt).

This is a design counterexample probe, not a new-solver control PASS. The solver
has not been implemented, the 32 discharge cases have not been exercised against
a new solver, and no whole-scope replay or completion-bound measurement was run.
It does not trigger the measured C9/C6 exhaustion criterion or establish any
new-solver cost. Slice 3 must not switch the consumer on this evidence.
