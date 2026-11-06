import ConcreteSpec.Spec.Slice1Syntax
import ConcreteSpec.Spec.Statics
import ConcreteSpec.Spec.Slice1Ownership
import ConcreteSpec.Spec.Slice1Semantics
import ConcreteSpec.Spec.Slice1Checker
import ConcreteSpec.Spec.Examples
import ConcreteSpec.Spec.Slice0
import ConcreteSpec.Spec.Rep
import ConcreteSpec.Spec.Cleanup
import ConcreteSpec.Spec.Borrow
import ConcreteSpec.Spec.Brand
import ConcreteSpec.Spec.Authority
import ConcreteSpec.Spec.Observation

/-!
# The Concrete specification

**PROPOSED: not normative until the owner freezes Core (ROADMAP A2c).** The owner provisionally
approved the Core design on 2026-11-05 (ROADMAP A2a; decision rows Q1.1–Q9.19 in
docs/decisions.md). `ConcreteSpec.Spec` is the one specification library (row Q9.15, split
into modules by owner decision 2026-11-06): every rule is in its modules under
`ConcreteSpec/Spec/`, as Lean definitions with plain-English doc comments, each followed by example
programs that it accepts or refuses and what they do. This file imports them all. It becomes normative when the owner freezes Core (A2c, row
Q9.19). Until then its examples are not conformance claims for any compiler, and any finding that
would change an approved row goes back to the owner.

How to read it. Read this header, then the modules in the order the README gives
(Slice1Syntax, Statics, Slice1Ownership, Slice1Semantics, Slice1Checker, Examples, Slice0, Rep,
Cleanup, Borrow, Brand, Authority, Observation); you never need the proofs to know what a rule says.
The proofs are in `ConcreteSpec/Proofs/` and say why the rules hang together
(an accepted program never gets stuck, the checker agrees with the rules, and the design theorems).
`ConcreteSpec/README.md` is the reading guide and lists what is not yet formalized.

The examples are decided by running the definitions: `check p = true` means the checker accepts
`p`, and `check_iff` (Proofs/Checker.lean) proves that this is exactly the rule `Checked p`;
`run p 10 = .exit 42` means `p` exits with 42.

The decisions are cited by their row ids (Q1.1 … Q9.19, P1 … P23, in docs/decisions.md) and by the
recorded rule ids of the archived prose specification, legacy/docs/spec (ODnn, TYPE-n, OWN-n, …).

The file has seven parts. Each is a small Core fragment with its own judgments, defined only as
far as its theorems need (R4 F8). Unifying them into one Core is A2i work (R-0571).

1. **Slice 1** (ROADMAP phase B): scalars, `let`, structs, moves and the use-after-move refusal,
   checked `+`, `return`, in Core's binding form (Q9.1), with evidence-required admission (no
   hidden traps, decision of 2026-11-06: an operation is accepted only with checked evidence that
   it cannot fail; Q6.1–Q6.6 as revised). It ends with the slice-0 fragment, the first subset the compiler
   accepts (`ConcreteSpec.Slice0`).
2. **Representation** (Q3.11): the layout and byte encoding of the slice-1 types.
3. **Structured cleanup** (Q9.8, Q2.1, Q2.7, Q1.5): `defer`, frames and exits.
4. **Scoped borrows** (Q3.3, Q3.4, Q9.5, REF-11), with range subviews (Q4.7).
5. **Allocator brands** (Q1.1–Q1.5, Q1.13).
6. **Authority**: capabilities and grants (the authority rules, Q5.1–Q5.4, Q7.11).
7. **Observations**: what the parent process sees, the exit status and traps (Q7.10, Q8.5).

Only Lean core is imported (Q9.16). No proof uses `native_decide` or `sorry` (Q9.17).

What slice 1 contains and how its Core is shaped (A-normal form, fresh identities, explicit
`copy`/`move`, statically known values) is in the header of `Spec/Slice1Syntax.lean`.
-/
