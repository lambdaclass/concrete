import ConcreteSpec.Spec.Basics
import ConcreteSpec.Spec.Ident
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
import ConcreteSpec.Spec.Target
import ConcreteSpec.Spec.Outcome
import ConcreteSpec.Spec.Exec
import ConcreteSpec.Spec.Observe
import ConcreteSpec.Spec.Adequacy
import ConcreteSpec.Spec.Scalar
import ConcreteSpec.Spec.Const
import ConcreteSpec.Spec.Memory
import ConcreteSpec.Spec.Hybrid
import ConcreteSpec.Spec.Rules

/-!
# The Concrete specification

**PROPOSED: not normative until the owner freezes Core (ROADMAP A2c).** The owner provisionally
approved the Core design on 2026-11-05 (ROADMAP A2a; decision rows Q1.1–Q9.19 in
docs/decisions.md). `ConcreteSpec.Spec` is the one specification library (row Q9.15, split
into modules by owner decision 2026-11-06): every rule is in its modules under
`ConcreteSpec/Spec/`, as Lean definitions with plain-English doc comments, each followed by example
programs that it accepts or refuses and what they do. This file imports them all. It becomes
normative when the owner freezes Core (A2c, row Q9.19). Until then its examples are not
conformance claims for any compiler, and any finding that would change an approved row goes back
to the owner.

How to read it. Read this header, then the modules in the order this file imports them; you never
need the proofs to know what a rule says.
The proofs are in `ConcreteSpec/Proofs/` and say why the rules hang together
(an accepted program never gets stuck, the checker agrees with the rules, and the design theorems).
`ConcreteSpec/README.md` is the reading guide and lists what remains open.

The examples are decided by running the definitions: `check p = true` means the checker accepts
`p`, and `check_iff` (Proofs/Checker.lean) proves that this is exactly the rule `Checked p`;
`run p 10 = .exit 42` means `p` exits with 42.

The decisions are cited by their row ids (Q1.1 … Q9.19, P1 … P23, in docs/decisions.md) and by the
recorded rule ids of the archived prose specification, legacy/docs/spec (ODnn, TYPE-n, OWN-n, …).

The parts are separate Core fragments, each with its own judgments, defined only as far as its
theorems need (R4 F8). Unifying them into one Core is A2i work (R-0571).

The foundation (owner decision 2026-11-08, "ok all recommended": the shared interfaces and their
laws, written, proved and tested before the freeze) is in the namespace `ConcreteSpec.Core`, from
`Target` on: `Target` (C3, target profiles), `Outcome` (trap kinds and the ways a run stops),
`Exec` (C1, execution, calls and cleanup), `Scalar` (C2, scalars and arithmetic), `Const` (C2,
the constant initializer subset), `Memory` (C4, byte memory and scalar representation),
`Hybrid` (C4, structured places and stable materialized ranges) and `Observe` (C6, observations
and the environment). Each module's header says which contract it states and what it leaves to a
later slice.

Only Lean core is imported (Q9.16). No proof uses `native_decide` or `sorry` (Q9.17).

What slice 1 contains and how its Core is shaped (A-normal form, fresh identities, explicit
`copy`/`move`, statically known values) is in the header of `Spec/Slice1Syntax.lean`.
-/
