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
far as its theorems need (R4 F8). Unifying them into one Core is work for A3 (R-0560).

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

## Slice 1

What slice 1 contains:

* the scalar types: the eight integer types (TYPE-19) and `bool`;
* `let` with a type, a struct literal, and a struct-destructuring `let` (PAT-5);
* struct declarations, `Copy` or not (TYPE-14), with scalar fields; the slice's central case is
  an empty struct without `Copy`, moved and then consumed by a destructuring `let`;
* the use-after-move refusal (OWN-1), consumption before exit (OWN-2) and used bindings (OWN-5);
* one checked operation, integer `+` (ARITH-3), which traps on overflow;
* evidence-required admission (no hidden traps, decision of 2026-11-06, Q1): `+` is accepted only
  when the facts the checker keeps (known values, Q6.2, and type bounds) prove the sum fits; every
  other `+` is refused, so an accepted program never traps (`checked_no_implicit_traps`). This
  subsumes the refusal of proven violations (Q6.1, Q6.4; ARITH-13, OD118);
* one function, `main`, with no parameters, whose body is a list of `let` statements followed by
  `return a` with `a : u8` (STMT-7, STMT-9).

Core's binding form (Q9.1):

* **A-normal form.** Every operand is an atom: a literal, `copy x` or `move x`. `x + y` is an
  expression whose operands are atoms; a nested surface expression becomes a sequence of `let`s
  of temporaries, and `return` takes an atom.
* **Fresh identities.** A local is a `Var`, an identity (a source-name hint and a number) that is
  bound at most once. Surface names, shadowing and the namespace rules (SCOPE-1, SCOPE-2) are
  elaboration rules that map names to identities; Core only requires each identity to be fresh.
* **Explicit moves and copies.** Reading a local is written `copy x` (its type is Copy) or
  `move x` (its type is linear). There is no read whose effect depends on the type. One read form
  per type (owner-confirmed 2026-11-05): `copy` is allowed only on a Copy type and `move` only on a
  linear one; `move` of a Copy value and `copy` of a linear value are both refused.

What slice 1 is not:

* It is not a parser or an elaborator. Programs are given as Core abstract syntax. How surface
  literals receive a type (ARITH-2, TYPE-4), how an unannotated `let` gets its type, how names
  become identities and how nested expressions become A-normal form are elaboration rules for a
  later file, which must be proved to produce this syntax.
* Nothing outside slice 1 is modelled in this part: no calls, no branches, no loops, no
  references, no heap, no capabilities, no `defer`. Struct fields are scalars only. Parts 3–7
  model the pieces of those that their theorems need.

The judgments (`HasType`, `UseAtom`, `Uses`, `StmtOk`, `BodyOk`, `Checked`) are inductive
relations, independent of the executable checker (`check`); `check_iff` proves the two agree. The
semantics is a total, fuel-indexed evaluator (`run`) whose outcome separates exit, trap, stuck and
running out of fuel (STMT-1). `Exec` is the fuel-free form of the same statement-level evaluator:
adequacy and determinism are about fuel, not an independent cross-check of expression evaluation.

Statically known values (Q6.2, as amended by the owner on 2026-11-05). The checker tracks an
abstract value for each binding: unknown, a known scalar, or a struct whose fields each carry
their own fact. A value is known when it comes from the fixed table: a literal, an immutable `let`
of a known value, or a checked operation on known operands. Constructing a struct records each
field's fact independently; an immutable binding keeps the facts; destructuring hands each field's
fact to its binder; an unknown field stays unknown without discarding its known siblings; and an
ownership transfer (`move`) keeps the facts for the receiving binding while still consuming the
source. So `let Pair { a: x, b: y } = Pair { a: 200, b: 100 }; x + y` is refused exactly as
`let a = 200; let b = 100; a + b` is (`fieldOverflowTrap`, `overflowTrap`). The exclusions stay:
nothing is tracked through mutable state (slice 1 has none), along branches or paths, through
user-function calls, or through calls the table does not name. Slice 1 judges Core, where every
`let` is immutable and a binding's origin is not recorded, so a temporary the elaborator introduces
gets the same fact as a source binding of the same expression. The remaining exclusion, that
acceptance of a source program does not depend on how the elaborator names its subexpressions, is
an obligation on the source-to-Core elaboration relation, which slice 1 does not define; it is not
proved here. In slice 1 struct fields are scalars, so the
rule's recursive form (a struct field holding a struct, nested patterns, tuples) has only its
one-level instance here. An input is not known, only bounded by its type: `add100` and
`partialUnknownRefused` trap for some input values and are refused (no hidden traps, decision of
2026-11-06), and `add0`, whose sum fits for every input, is accepted. The table has more entries (module constants, the
six named wrapping/saturating operations, lossless `as`) that slice 1 has no syntax for. The
refusal applies to all written code, including trusted and unreachable code (Q6.4); slice 1 has
neither, so its examples show only reachable positions and a discarded one (`overflowDropped`).
-/
