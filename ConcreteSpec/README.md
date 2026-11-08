# Reading the Concrete specification

**Status: PROPOSED.** This is the specification of the new compiler, written in Lean (decision
rows Q9.15, P3: Lean is the specification). It is not normative until the owner freezes Core at
stage A2c (row Q9.19). Before that, the owner approved the Core design provisionally (A2a,
2026-11-05), and A2b formalized it here and checked the four design theorems (row Q9.14).

## How to read it

[`Spec.lean`](Spec.lean) is the specification library (row Q9.15; split into modules by owner
decision 2026-11-06). It imports the modules under [`Spec/`](Spec/) in reading order, and each
module's header says what it covers. Every rule is a Lean definition with a plain-English doc
comment, followed by example programs: the surface program in a doc comment, its Core form, and
what the rules say about it: accepted (`check p = true`) with its outcome (`run p 10 = .exit 42`),
or refused (`check p = false`). You can read it without reading any proof.

[`Proofs/`](Proofs/) says why the rules hang together. Read it only to check a claim.

**The owner view.** `scripts/spec/lean_site.sh [OUT]` (after `lake build ConcreteSpec`) generates
a static HTML view into `_build/spec-site/`: every module rendered from its source (doc comments as
prose, rules and examples as code with linked names), each declaration with what it uses and which
proofs are about it, and a page of all theorems with their statements. It is generated from the
sources and the elaborated environment only, and fails on a broken link.

The decisions are cited by row id (Q1.1 … Q9.19, P1 … P23, in
[docs/decisions.md](../docs/decisions.md)) and by rule id (ODnn, TYPE-n, OWN-n, …, in the
archived prose specification [legacy/docs/spec](../legacy/docs/spec)).

## Rules of the file

- Only Lean core is imported, no Mathlib (Q9.16). No `sorry`, no `native_decide` (Q9.17), and
  `autoImplicit` is off, so a misspelled name is an error, never a silently bound variable.
- Axioms: only Lean's standard `propext`, `Quot.sound` and `Classical.choice`. The evaluator
  `run`, the checker `check` and the other executable definitions use `propext` alone, so they
  stay executable and the examples are decided by running them; `Classical.choice` appears only
  in proofs (`rep_roundtrip` through the core lemma `Nat.mod_mul`, and the range proofs).
- These rules are checked in CI by [scripts/proofs](../scripts/proofs): the per-theorem axiom
  inventory against its golden list, the transitive ban on `sorry` and `native_decide`, a fresh
  kernel replay with `leanchecker`, and an independent re-check of the exported theorems by
  con-leche and nanoda.
- Every rule a diagnostic can name is registered in [`Spec/Rules.lean`](Spec/Rules.lean) by its
  Lean name and revision. The rule gate ([scripts/spec/rules.sh](../scripts/spec/rules.sh), run by
  CI's Spec build) requires each registered rule to cite its decision row and to have an example
  it accepts and one it refuses, and every rule citation to name a registered rule.
- The judgments (inductive relations) are the normative form; the checker is proved to agree with
  them (`check_iff`).
- Build: `lake build ConcreteSpec`. The external checker re-checks every theorem.

## What remains open

The parts are separate Core fragments, each defined only as far as its theorems need (R4 F8);
unifying them into one Core is A2i work (R-0571).

Open obligations, by name:

- **O1 Elaboration** from surface syntax to Core: how literals get a type, how names become
  identities, how nested expressions and grouping become A-normal form, source positions, and that
  acceptance of a source program does not depend on how the elaborator names temporaries.
- **O2 The lexer, model-translation and native-image obligations.**
- **O3 Instrumentation erasure** (Q9.10): the ownership instrumentation (`Slot.moved`, the
  `used`, `live`, `known` and `kfields` fields of `Entry`) is stated as intent, with no erasure
  definition or theorem yet. (Brand erasure, part 5, is done.)
- **O4 Trap diagnostics** (Q7.10, OD101): the bytes of the trap line, including the trap's source
  location (needs O1's positions), and of the startup refusal.
- **O5 Target observations** (the launcher contract): every result here is about the model's
  outcomes; a claim about an executable needs a transport to the approved launcher contract.
- **O6 Launch validation** (part 6): `ProgOk` assumes a valid launch: instance limits, the target
  offering each parameter type, and the launcher's grant minting are not modelled; nor are
  authority rule 11, foreign calls, trait dispatch, generic methods and generated cleanup.
- **O7 Capabilities in the other parts**: the cleanup calls of part 3 and the allocator
  operations of part 5 need capabilities; part 6 checks capabilities in its own fragment only
  (unification, A2i).
- **O8 Part 3** abstracts each deferred call to its identity, dependencies, loans and whether it
  traps; it does not model the statements between registrations or evaluating a `return` value
  before cleanup.
- **O9 Part 4**: the run-time fragment (`vrun`) and the access rules (`Ok`) are separate
  fragments, not yet joined in one soundness theorem (A2i); the run covers array sources (text
  views share the index arithmetic, and their validity is `text_view_utf8`); views of views,
  writes through text views, and text whose length is known while its bytes are not (treated as
  unknown) are not modelled; the frame theorem covers bodies that access only their view. That
  compiled code performs the same range check is a compiler-slice obligation (O5).
- **O10 Part 5** abstracts the condition of `while`: loop test number `k` takes the outcome
  `σ k`, and the results hold for every `σ`.
- **O11 Foundation contracts** (owner decision 2026-11-08, C1–C10): written so far are C3
  (`Target`), C1 (`Exec`: the machine, its step, cleanup order, traps; parametric in the operand
  language) and C2 (`Scalar`, `Const`). C4 (`Memory`, `Hybrid`) now covers byte memory, scalar
  representation and structured/materialized places, with trailing padding preserved by typed
  writes; struct field layouts, enums, pointers in structs and the C8 padding oracle remain out
  of its scalar slice. The old slice-1 `Spec/Rep.lean` still has its own `MByte` and integer
  encoding over the old type universe. The slice-1-to-Core integration slice must port that
  layer to `Mem.MByte`/`Mem.encodeInt` and retire the old encoders while preserving its proofs;
  C4 has not completed that port. Core constants and Hybrid share `CTy`/`CVal` from `Scalar`.
  C5–C10 remain. C1 leaves to later slices the source-level
  evaluation orders (elaboration, O1), counted loops (slice 4), and ownership of moved values
  (C5), the split of a request from its response (C6) and the oracle's query record (C8); its
  storage can use C4's hybrid state; C5 must still supply ownership instrumentation. Lifecycle
  instrumentation may refuse an event (`Option.none`), making execution stuck, including initial
  activation and primitive result binding; cleanup laws require successful lifecycle events. C2
  leaves floats (slice 26), and two owner questions: whether a failing operation may appear in a constant
  initializer, and whether the source may name a constant written later in the file.
