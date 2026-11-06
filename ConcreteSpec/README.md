# Reading the Concrete specification

**Status: PROPOSED.** This is the specification of the new compiler, written in Lean (decision
rows Q9.15, P3: Lean is the specification). It is not normative until the owner freezes Core at
stage A2c (row Q9.19). Before that, the owner approved the Core design provisionally (A2a,
2026-11-05), and A2b formalized it here and checked the four design theorems (row Q9.14).

## What to read, in order

1. **[`Spec.lean`](Spec.lean)** is the specification: one file (row Q9.15) with every rule as a
   Lean definition, each with a plain-English doc comment, followed by example programs. The
   examples give the surface program in a doc comment, its Core form, and what the rules say about
   it: accepted (`check p = true`) with its outcome (`run p 10 = .exit 42`), or refused
   (`check p = false`). You can read it without reading any proof.
   - The header says what the file covers and how slice 1's Core is shaped (A-normal form, fresh
     identities, explicit `copy`/`move`, statically known values).
   - **Part 1, slice 1**: syntax, typing, ownership and known values, the evaluator, the checker,
     then the conformance examples (`ConcreteSpec.Examples`).
   - **Part 2, representation** (Q3.11): byte layout and encoding.
   - **Part 3, structured cleanup** (Q9.8): `defer`, frames, exits.
   - **Part 4, scoped borrows** (Q3.3, Q3.4, Q9.5, REF-11).
   - **Part 5, allocator brands** (Q1.1–Q1.5, Q1.13).
2. **[`Proofs/`](Proofs/)** says why the rules hang together. Read it only to check a claim.

   | File | Main results |
   |---|---|
   | `Proofs/Slice1.lean` | `checked_never_stuck` (an accepted program never gets stuck), `checked_exits_or_traps`, `knownViolation_traps` (a refused known violation really would trap), fuel properties of `run` |
   | `Proofs/Checker.lean` | `check_iff`: the executable checker accepts exactly the programs the rules accept |
   | `Proofs/Rep.lean` | `rep_roundtrip` (design theorem 1) |
   | `Proofs/Cleanup.lean` | `defer_cleanup_releases` (design theorem 2) |
   | `Proofs/Borrow.lean` | `open_mut_freezes_root` (design theorem 3), `shared_open_grants_no_write` |
   | `Proofs/Brand.lean` | `reset_fresh` (design theorem 4), the brand refusals |
   | `Proofs/Examples.lean` | `add100_safe` (an accepted program that traps only for some inputs is safe for every input), and the cleanup traces and brand refusal of `Spec.lean` derived again from the general theorems (they apply, non-vacuously) |
   | `Proofs/Basics.lean` | list lemmas |

The decisions are cited by row id (Q1.1 … Q9.19, P1 … P23, in
[docs/spec/decisions.md](../docs/spec/decisions.md)) and by rule id (ODnn, TYPE-n, OWN-n, …).

## Rules of the file

- Only Lean core is imported, no Mathlib (Q9.16). No `sorry`, no `native_decide` (Q9.17), and
  `autoImplicit` is off, so a misspelled name is an error, never a silently bound variable.
- Axioms: only Lean's standard `propext`, `Quot.sound` and `Classical.choice`, the last in one
  place: `rep_roundtrip` reaches it through the core lemma `Nat.mod_mul`. The evaluator `run` and
  the checker `check` use `propext` alone, so both stay executable and the examples are decided by
  running them.
- The judgments (inductive relations) are the normative form; the checker is proved to agree with
  them (`check_iff`).
- Build: `lake build ConcreteSpec`. The external checker re-checks every theorem.

## Not yet formalized

The five parts are separate Core fragments, each defined only as far as its design theorem needs
(R4 F8); unifying them into one Core is A3 work (R-0560). What the specification does not yet
state, as named open obligations:

- **Elaboration** from surface syntax to Core: how literals get a type, how names become
  identities, how nested expressions become A-normal form, and that acceptance of a source
  program does not depend on how the elaborator names temporaries.
- **The lexer, model-translation and native-image obligations.**
- **Instrumentation erasure** (Q9.10): the ownership instrumentation (`Slot.moved`, the `used`,
  `live`, `known` and `kfields` fields of `Entry`) is stated as intent, with no erasure definition
  or theorem yet.
- **Trap observations** (Q7.10, Q8.5): a trap's source location and message, and the mapping of
  exit values and traps to the process exit code (trap exit 134); `Trap` is an abstract outcome.
- **Target observations** (the launcher contract): every result here is about the model's abstract
  outcomes; a claim about an executable needs a transport to the approved launcher contract.
- **Capabilities** (Q5, Q2.7, the authority rules): the cleanup calls of part 3 and the allocator
  operations of part 5 need capabilities, and no part models capability checking.
- **Part 3** abstracts each deferred call to its identity, dependencies, loans and whether it
  traps; it does not model the statements between registrations or evaluating a `return` value
  before cleanup.
- **Part 4** does not model the range clause and `else` of Q4.7, element access, or the no-escape
  rules.
- **Part 5** abstracts the condition of `while`, and does not prove that a checked program passes
  every ghost brand check (the erasure correspondence of Q1.1).
