# Reading the Concrete specification

**Status: PROPOSED.** This is the specification of the new compiler, written in Lean (decision
rows Q9.15, P3: Lean is the specification). It is not normative until the owner freezes Core at
stage A2c (row Q9.19). Before that, the owner approved the Core design provisionally (A2a,
2026-11-05), and A2b formalized it here and checked the four design theorems (row Q9.14).

## What to read, in order

1. **[`Spec.lean`](Spec.lean)** is the specification library (row Q9.15; split into modules by
   owner decision 2026-11-06). Its header says what the specification covers and how slice 1's
   Core is shaped (A-normal form, fresh identities, explicit `copy`/`move`, statically known
   values), and it imports the modules below. Every rule is a Lean definition with a plain-English
   doc comment, followed by example programs: the surface program in a doc comment, its Core form,
   and what the rules say about it: accepted (`check p = true`) with its outcome
   (`run p 10 = .exit 42`), or refused (`check p = false`). You can read it without reading any
   proof. Read the modules in this order:

   | Module | Contents |
   |---|---|
   | [`Spec/Slice1Syntax.lean`](Spec/Slice1Syntax.lean) | Part 1, slice 1: types, integer ranges, Core syntax |
   | [`Spec/Statics.lean`](Spec/Statics.lean) | slice-1 typing |
   | [`Spec/Slice1Ownership.lean`](Spec/Slice1Ownership.lean) | ownership, known values (Q6.2), static refusal, acceptance (`Checked`) |
   | [`Spec/Slice1Semantics.lean`](Spec/Slice1Semantics.lean) | values, the evaluator `run`, `Exec`, value typing |
   | [`Spec/Slice1Checker.lean`](Spec/Slice1Checker.lean) | the executable checker `check` |
   | [`Spec/Examples.lean`](Spec/Examples.lean) | slice-1 conformance examples |
   | [`Spec/Slice0.lean`](Spec/Slice0.lean) | the slice-0 fragment the compiler targets first (`u8` literals, immutable `let`, checked `+`, `main -> u8`), embedded into slice 1 by `toCore` |
   | [`Spec/Rep.lean`](Spec/Rep.lean) | Part 2, representation (Q3.11): byte layout and encoding |
   | [`Spec/Cleanup.lean`](Spec/Cleanup.lean) | Part 3, structured cleanup (Q9.8): `defer`, frames, exits |
   | [`Spec/Borrow.lean`](Spec/Borrow.lean) | Part 4, scoped borrows (Q3.3, Q3.4, Q9.5, REF-11) and range subviews (Q4.7): `borrow [mut] v from a until b as s { … } [else { … }]`, no escape, element access at run time (`vrun`), UTF-8 text views |
   | [`Spec/Brand.lean`](Spec/Brand.lean) | Part 5, allocator brands (Q1.1–Q1.5, Q1.13), the ghost run and the erased run |
   | [`Spec/Authority.lean`](Spec/Authority.lean) | Part 6, authority (Q5.1–Q5.4, Q7.11): capabilities, grants, Console |
   | [`Spec/Observation.lean`](Spec/Observation.lean) | Part 7, observations (Q7.10, Q8.5): startup refusal 125, the returned code, trap 134 |

2. **[`Proofs/`](Proofs/)** says why the rules hang together. Read it only to check a claim.

   | File | Main results |
   |---|---|
   | `Proofs/Slice1.lean` | `checked_never_stuck` (an accepted program never gets stuck), `checked_no_implicit_traps` (an accepted program never traps), `checked_exits`, fuel properties of `run` |
   | `Proofs/Checker.lean` | `check_iff`: the executable checker accepts exactly the programs the rules accept |
   | `Proofs/Slice0.lean` | `slice0_exits` (an accepted slice-0 program exits with a `u8` status), `slice0_never_traps`, `toCore_injective` |
   | `Proofs/Rep.lean` | `rep_roundtrip` (design theorem 1) |
   | `Proofs/Cleanup.lean` | `defer_cleanup_releases` (design theorem 2) |
   | `Proofs/Borrow.lean` | `open_mut_freezes_root` (design theorem 3), `shared_open_grants_no_write`; ranges: `range_never_traps` (a range accepted without `else` opens), `enter_in_bounds`, `range_open_mut_freezes_root`; no escape: `view_not_moved`, `view_not_used_after`, `root_restored`; element access: `view_in_source`, `view_get`, `view_set`, `viewsOk_enter`, `viewsOk_put`; mutation: `view_write_exact`, `range_block_frame`; text: `text_view_utf8`, `utf8_split` |
   | `Proofs/Brand.lean` | `reset_fresh` (design theorem 4), the brand refusals, `checked_passes_brand_checks` (a checked program never fails a ghost brand check), `brand_erasure` (Q1.1: its run with brands erased does exactly what its ghost run does), `ghost_run` |
   | `Proofs/Authority.lean` | `authority_contained` (every operation a run performs uses a grant `main` was launched with, of the right type, under a capability `main` declares, for every fuel), `sub_iff`, `equiv_iff` |
   | `Proofs/Observe.lean` | `checked_observation`, `checked_observable` (an accepted program's observation and status), `refused_start` |
   | `Proofs/Examples.lean` | the cleanup traces and brand refusal of the specification derived again from the general theorems (they apply, non-vacuously) |
   | `Proofs/Basics.lean` | list lemmas |

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
- The judgments (inductive relations) are the normative form; the checker is proved to agree with
  them (`check_iff`).
- Build: `lake build ConcreteSpec`. The external checker re-checks every theorem.

## Done at A2c, and what remains open

The seven parts are separate Core fragments, each defined only as far as its theorems need
(R4 F8); unifying them into one Core is A3 work (R-0560).

Formalized at A2c, each with rules, examples and proofs:

- **Capability checking** (part 6): capability sets and their semantic equality (Q5.3),
  substitution of capability parameters at a call, grant parameters, Console for the standard
  streams (Q7.11), the refusals (missing capability, missing grant, `Unsafe` outside trusted code,
  `Device`), and `authority_contained`.
- **Brand erasure** (Q1.1, part 5): the erased run (no brands, no brand checks), and
  `brand_erasure`: for a checked program it does what the ghost run does, which never fails a
  brand check (`checked_passes_brand_checks`), for every outcome of the loop tests.
- **Range subviews** (Q4.7, part 4): the range test for arrays and UTF-8 text, `else`, and the
  evidence a range without `else` needs (no hidden traps: known valid, so it never traps).
  Element access `v[i]` reaches exactly source element `a + i`, inside `[a, b)` and the source, or
  traps (`view_in_source`, `view_get`, `view_set`); a write changes exactly that element
  (`view_write_exact`), and a block that accesses only its view changes nothing outside
  `[a, b)` (`range_block_frame`). No escape: a view never moves (so is never returned, stored,
  passed or captured), is never named after its block, and the source's root is back with
  exactly its earlier entries, freeze released (`view_not_moved`, `view_not_used_after`,
  `root_restored`). A valid text range of well-formed UTF-8 is well formed (`text_view_utf8`).
- **Trap and exit observations** (Q7.10, Q8.5, part 7): startup refusal 125, the returned code,
  trap 134, and what each observation records.
- **The slice-0 fragment** (part 1): a Core syntax the compiler can target, embedded into slice 1.

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
  (unification, A3).
- **O8 Part 3** abstracts each deferred call to its identity, dependencies, loans and whether it
  traps; it does not model the statements between registrations or evaluating a `return` value
  before cleanup.
- **O9 Part 4**: the run-time fragment (`vrun`) and the access rules (`Ok`) are separate
  fragments, not yet joined in one soundness theorem (A3); the run covers array sources (text
  views share the index arithmetic, and their validity is `text_view_utf8`); views of views,
  writes through text views, and text whose length is known while its bytes are not (treated as
  unknown) are not modelled; the frame theorem covers bodies that access only their view. That
  compiled code performs the same range check is a compiler-slice obligation (O5).
- **O10 Part 5** abstracts the condition of `while`: loop test number `k` takes the outcome
  `σ k`, and the results hold for every `σ`.
