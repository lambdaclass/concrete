# Slice 1: writer brief (draft, not launched)

Status: a draft brief for the writers of [slice 1](../../ROADMAP.md#vertical-slices) (queue row
B1). Nothing here is a decision.

**Launch conditions.** No piece of this brief launches until all of the following hold. Section 6
gives the order of work once they hold.
- Slice 0 is on main.
- The lexer (`wip/slice0-lexer`) is on main.
- The spelling dependencies in §4 are clear: every row marked *pending* or *open question* has the
  owner's answer.
- The owner has answered the scope question in §1.
- The foundation is written and frozen ([foundation first](../decisions.md#decided-2026-11-07-foundation-first):
  "Before slice 1 builds on it, the shared foundation is fully written in the Lean spec and
  frozen"). ROADMAP orders A2c, the Core freeze, before B1.

**Unlanded sources.** This brief cites two branches that are not on main:
- `wip/record-language` at `1547734f8`, owner-signed on 2026-11-08 and not yet landed. It
  defines R-0625 and the 2026-11-08 language decisions, cited below as "item N".
- `wip/slice0` at `ea5181b5`, which carries the slice-0 compiler.

If either changes before landing, this brief is re-checked against the landed text.

## 1. Scope

Slice 1 is decision row [Q8.2](../decisions.md#q8-2-revision), confirmed by the
[vertical slices](../decisions.md#decided-project-vertical-slices-2026-11-06). The ROADMAP slice
table lists exactly these items:

1. an empty non-`Copy` struct;
2. its literal;
3. a move;
4. the use-after-move refusal;
5. a struct-destructuring `let`;
6. one checked operation;
7. a return;
8. explicit `abort()` as the runtime trap example, with the refused `200 + 56` as its
   known-violation control;
9. the component and bound launcher (Q8.4, Q8.5);
10. the predictable policy (R-0576);
11. the `Concrete.toml` `[policy]` and `[assumptions]` tables (R-0589);
12. the Lean I/O driver infrastructure (R-0586).

Slice 1 also meets the slice shape: Lean judgments with accepted, refused and trapping conformance
examples; every stage, from source to BIR and to both backends, proved or checked by a proved
validator; a composed theorem with the LLVM premises and the LCB premises separate; facts and
diagnostics for the new constructs; and full observations compared in CI. It is cumulative on
slice 0. LP0 (R-0572) follows it.

**Out of scope**, by the slice table:
- integer widths other than `u8` (slice 2);
- `bool` (slice 3);
- structs with named fields, declared `Copy`, field-wise known values (slice 9);
- branches, loops and calls;
- capabilities beyond what the launcher and the I/O driver need.

**The spec is wider than the slice.** The current slice-1 Core in `ConcreteSpec`
(`Spec/Slice1Syntax.lean`) covers more than slice 1 does:
- all eight integer types and `bool`;
- structs with scalar fields;
- declared-`Copy` structs;
- field-wise known values.

This is a spec design question for the owner, and the answer is a launch condition. The two
options, each needing its own proof:
- **(a) A compiler-subset predicate.** The compiler admits a subset defined by an explicit predicate
  over that Core, the way slice 0 is embedded by `toCore`. A proof shows that the predicate is
  closed under the passes.
- **(b) Narrowing the spec.** The spec is narrowed to the slice. This is a spec change with its own
  review.

Recommendation: (a). It follows the pattern slice 0 already uses (`toCore`). It also keeps the
rules that slices 2, 3 and 9 need, which (b) would delete and later re-add. The spec stays the one
source, and the compiler's subset is stated in it rather than being a second, narrower copy.

## 2. The programs it must build

Each program is a conformance example that names its rule, its source and its outcome: an exit
status, a refusal with its Lean rule ID, or a trap kind (R-0425). Each runs on the Lean semantics
and on both native builds.

1. **The component (accepted).** It declares an empty linear struct and builds one value of it. It
   moves the value to a second local, consumes it with a destructuring `let`, and returns a `u8`
   that a checked `+` computes with evidence. Expected: exit with that value.
2. **Use after move (refused).** Like program 1, except that it destructures the moved-from local.
   Expected: refused under `OWN-1`.
3. **Not consumed (refused).** The linear value is not consumed before `return`. Expected: refused
   under `OWN-2`.
4. **Known violation (refused).** A checked `+` of `200` and `56` in `u8`. Expected: refused (Q6.1,
   OD118). The field-wise variant of
   [Q6.2](../decisions.md#decided-2026-11-05-q6-2-fields) needs named fields and belongs to
   slice 9.
5. **Explicit trap (accepted, traps).** A program reaches `abort()`. Expected: the trap kind
   `abort` (item 4) and the launcher's trap observation (Q8.5). `checked_no_implicit_traps` still
   holds; `checked_exits` no longer holds as stated (§3.2).
6. **The predictable policy.** With `[policy]` turned on in `Concrete.toml`:
   - program 1 is accepted;
   - two refusal controls are refused, each naming the function, the fact and its source: a planted
     violation and a planted unknown fact (R-0576 requires both).

   Every slice-1 program establishes the policy's facts (no allocation, no blocking, no foreign
   calls, no loops), so no slice-1 source program can violate the policy. Both controls are
   therefore planted, as R-0576's controls are. The fact must be one the
   policy alone refuses, not a stack bound, because an unknown stack bound is refused from slice 0
   regardless of policy ([stack bounds](../decisions.md#decided-2026-11-07-stack-randomness)).
7. **Assumptions.** A change to `[assumptions]` without a matching record fails the
   assumption-diff check, and an unknown section, key or version is refused (R-0589).
8. **I/O driver.** The Lean evaluator runs one program through the driver, with an expected outcome
   that does not depend on the compiler (R-0586, infrastructure only).

The spec's examples (`ConcreteSpec/Spec/Examples.lean`) show programs 1–3 in an illustrative
surface. That text is not a spelling choice, and several of its forms are superseded by the
2026-11-08 decisions (§4).

## 3. Dependencies

### 3.1 On main before launch
- Slice 0 (`ea5181b5`), landed through the train and green on main.
- The lexer (`7e8c926c`), landed after slice 0.
- `wip/record-language` (`1547734f8`).
- The frozen foundation (A2c).

### 3.2 Spec contracts (`ConcreteSpec`, PROPOSED until A2c)

**Slice-1 Core, used as it stands:**
- `Spec/Slice1Syntax.lean`: A-normal form, fresh `Var` identities, and Core's explicit `copy x` and
  `move x` reads (Q9.1).
- `Spec/Slice1Ownership.lean`, `Spec/Slice1Checker.lean` and `Proofs/Checker.lean`: the inductive
  judgments and the executable `check`, related by `check_iff` (R-0499).
- `Spec/Slice1Semantics.lean` and `Proofs/Slice1.lean`: the total, fuel-indexed `run`, with
  `checked_never_stuck` and `checked_no_implicit_traps`.
- `Spec/Observation.lean` and `Proofs/Observe.lean`: what the launcher shows the parent process.
- `Spec/Slice0.lean` and `Proofs/Slice0.lean`: `toCore`, which must stay valid.

**Foundation contracts it builds on**, which must be frozen first (FOUNDATION.md):
- C1, execution, calls and cleanup (R-0607): the step relation's `abort` result, where "abort runs
  no cleanup".
- C2, scalars, failure and arithmetic (R-0608): the trap kind `abort`.
- C3, target, layout and representation (R-0609): it restates `rep_roundtrip` and deletes `Rep`.
  Slice 1's layout and byte encoding, and the Memory CFG (R-0580), target C3, not
  `Spec/Rep.lean`.
- C4, byte memory and value validity: the byte-memory round-trip and decode-validity laws.
- C6, observations and environment: the launcher and the I/O driver.
- C9, identities, primitive contracts, evidence and ghost erasure.

**The `abort()` gap.** Slice-1 Core has no `abort()`; `Proofs/Slice1.lean` says slice 1 has none.
That text was written before 2026-11-06, when `abort()` moved into slice 1. Before compiler work, a
spec change (cap 5) must:
- add `abort` to the syntax, the semantics and the checker;
- revise `checked_exits`, which today proves that every accepted program exits, and the
  trap-freedom proof that supports it;
- restate `checked_no_implicit_traps` so that its only traps are explicit ones;
- update the documentation of all three.

### 3.3 Lexer and grammar
- **Already in the lexer** (`Concrete/Lex.lean` and `Spec/Surface0.lean` at `7e8c926c`): UTF-8
  decoding and source checking, with soundness and completeness; identifier start and continue;
  `//` line comments.
- **Still open in the lexer:**
  - whitespace (space, tab, LF and CR, item 30, decided): `7e8c926c` introduces no whitespace
    rules yet;
  - the first-error-offset property (a TODO).
- **New tokens:** the struct-declaration keyword, and however `abort` lexes, as a reserved word or
  as an identifier resolved against the builtin table. Both depend on §4.
- **The grammar:** the parser waits for the written grammar (R-0622), its Lean `Grammar` judgment
  (Q8.3) and `ll1_sound`. On `wip/record-language`, R-0622 depends on R-0625.

### 3.4 Roadmap tasks with a slice-1 increment (queue row B1)

R-0425, R-0499, R-0492, R-0493, R-0514, R-0501, R-0503, R-0497, R-0504, R-0507, R-0521, R-0552,
R-0528, R-0479, R-0118, R-0573, R-0576, R-0577, R-0579, R-0580, R-0582, R-0586, R-0588, R-0589,
R-0599 and R-0600.

**Inconsistency to resolve, not silently.** B1 lists R-0600, the always-aborts fact, but R-0600's
own row says it is "due with the functions-and-calls slice" (slice 5). The may-abort fact is at
slice 3. This brief schedules neither for slice 1. The decisions writer reconciles R-0600's
placement.

## 4. Surface spellings it needs

The table lists every surface form slice 1's programs use. The statuses mean:
- **decided**: a decided row fixes the exact text.
- **pending (R-0625)**: the 2026-11-08 decisions make the spellings of items 6–29 and 40–53
  illustrative and send them to the spelling round (items 55 and 59).
- **open question**: no decided row or R-0625 item names it, so it goes to the owner.

This brief chooses none of them.

| # | form | used by | status | source |
|---|---|---|---|---|
| 1 | `fn main() -> u8 { … }` | every program | pending (R-0625) | item 18's result arrow is among items 6–29, whose spellings are illustrative. Slice 0 implements this text at `ea5181b5`, and would change with the round's answer |
| 2 | `let x: u8 = e;` | every program | pending (R-0625) | item 18 (written types), as row 1 |
| 2a | a `let` of struct type: `let t: Token = Token {};` and `let u: Token = t;` in the spec | programs 1–3 | pending (R-0625) | item 18; the type is a struct name |
| 3 | reads of `u8` (Copy) locals as a plain name | programs 1 and 4 | decided by the slice-0 surface | slice 0 implements them; Core's `copy x` is the elaborated form (Q9.1) |
| 4 | decimal literals, leading zero refused | every program | decided | slice-0 surface row; typed suffixes are not needed (item 41, pending) |
| 5 | checked `a + b`, parentheses, `return a;` | every program | decided | slice 0; ARITH-3 |
| 6 | `//` comments; whitespace (space, tab, LF, CR); ASCII case-sensitive identifiers | every program | decided | items 30 and 31 (grammar items, decided as written). The lexer does not implement whitespace yet (§3.3) |
| 7 | struct declaration: the keyword and the empty field list (`struct Token {}` in the spec) | programs 1–3, 5 and 6 | pending (R-0625) | empty structs are kept (item 50, which is among items 40–53, so its text is illustrative). The declaration text comes with R-0622, after R-0625 |
| 8 | empty struct literal (`Token {}` in the spec) | programs 1–3, 5 and 6 | pending (R-0625) | item 8 (constructors write `field: value`) is illustrative. The empty case's text is in the round |
| 9 | empty destructuring pattern (`let Token {} = u;` in the spec) | programs 1 and 2 | pending (R-0625) | item 9 (patterns write `field: binder`) is illustrative |
| 10 | the type written on a destructuring `let` | programs 1 and 2 | pending (R-0625) | item 18 requires a written type on every local; item 55 leaves the destructuring typing rule to the round |
| 11 | surface read of a linear local (a plain `t` in the spec's examples, `move t` in Core) | programs 1 and 2 | open question | Q9.1 fixes Core's reads, not the source text. Item 12 is the heap move-out, a different form. The owner decides whether this goes into R-0625 |
| 12 | `abort()` as written | program 5 | decided | Q8.2's revision and the slice table write `abort()`; the trap kind `abort` is item 4 |
| 13 | `abort`'s place in the closed builtin inventory | program 5 | pending (R-0625) | item 59 (OPEN-4, the closed builtin inventory) |
| 13a | `abort`'s signature (its result type) | program 5 | open question | item 59 promises exhaustive signatures only for the view, constant-time, text, container and branded APIs; `abort` is not named there |
| 14 | where `abort()` may stand in a slice-1 body: as a statement, or as a value under `return` | program 5 | open question | slice-1 bodies are `let`s followed by `return`, and the Unit statement rule (OD29) arrives at slice 3 |
| 15 | the struct keyword and slice 1's other words in the reserved-word table | lexer | pending (R-0625) | item 40 fixes some reservations, and the revision of OD90 keeps `Copy` reserved. The full table is part of R-0622, after R-0625 |
| 16 | `Concrete.toml` table names `[policy]` and `[assumptions]` | programs 6 and 7 | decided | typed and versioned tables, with unknown keys and versions refused ([row](../decisions.md#decided-project-concrete-toml)) |
| 16a | their key names and values, the version field, and how the predictable policy is turned on | programs 6 and 7 | open question | not decided. This is project configuration, not R-0625 language syntax, so it goes to the owner separately |
| 17 | rule IDs in diagnostics and facts (for example `StmtOk.let_.admitted@2`) | all | decided | derived from the Lean rule's name and its revision ([row](../decisions.md#decided-project-diagnostics-2026-11-06)) |

## 5. Gates (the definition of done)

All slice-0 legs stay green on the cumulative program set. Slice 1 adds the following.

**Conformance (R-0425).** Every program in §2 passes the Lean runner and both native runners, and
every refusal matches its rule ID.

**Correspondence and soundness (R-0499, R-0507).**
- `check_iff` holds on the extended Core.
- The positive and rule-breaking mutation controls pass.
- Type and ownership soundness hold over the executable semantics, `abort` included.
- `checked_no_implicit_traps` holds as revised in §3.2.

**Lowering (R-0501, R-0580, R-0579).**
- Each lowering that slice 1 uses has its preservation proof and its output-invariant proof. That
  covers struct values, destructuring, moves and the explicit trap, through the Memory CFG under
  one layout authority built on C3.
- The BIR → LCB-IR adapter proof covers the slice-1 subset.

**Certificates (R-0582, R-0552).** Per-pass certificates with per-edge witnesses replace slice 0's
whole-compiler `decide`. A planted wrong witness at any edge is rejected.

**Native builds (R-0503, R-0578, R-0577).**
- Both backends build, the LLVM path labelled unverified with its premises in their own list.
- Full observations are compared with each other and with the expected outcome.
- Each native trace is checked as a refinement of Core's behaviour
  ([question 17](../decisions.md#foundation-q17)).
- Every slice-1 example builds reproducibly.
- Stack bounds, AddressSanitizer runs and binary size are covered, as in slice 0.

**Facts and diagnostics (R-0575, R-0576, R-0479, R-0599).**
- The facts report covers structs, moves, `abort` and policy coverage.
- Diagnostics are records in the frozen, versioned JSON schema.
- Failures are never turned into facts.
- Replayed counterexamples are produced where they apply.

**Proof strength (R-0585).**
- Vacuity checks and contract-strength labels apply from B1.
- Planted vacuous proofs and planted point-only proofs are caught.

**Provenance (R-0521).** Minimal provenance and the canonical verification task accompany slice 1's
authoritative claims: compiler, source, theorem and executable digest.

**Identity, payload and artifact controls.**
- R-0492: alias, symbol, binding and forgery controls.
- R-0514: substituting a different Core payload is refused.
- R-0497: forgery, mutation and bypass compile-fail controls.
- R-0504: substitutions and wrong artifacts are rejected.
- R-0528: each input class has a limit and a hostile fixture.
- R-0479: diagnostics name the valid next action.
- R-0118: a missing coverage row or a synthetic constructor fails the gate.

These are B1 obligations, not existing slice-0 legs.

**Policy and assumptions (R-0576, R-0589).** Program 6's planted violation and planted unknown fact
are both refused, each from a policy-only fact. The assumption-diff check fails on an unrecorded change.

**Launcher (Q8.5).** The pinned launcher ABI and trap mapping match the Lean observation for
program 5.

**Fuzzer (R-0573).** The differential fuzzer grows by one feature (empty linear structs and moves),
and the adversarial interaction programs are added.

**Proof integrity.** `scripts/proofs/axioms.sh` passes, with new golden lines justified line by
line. The trust ban, the trusted-base gate and the external checker pass.

**Composed theorem.** It covers slice 1 under named premises, with the LLVM and LCB premises kept
separate.

## 6. Writers, reviews and order

**Who writes.** Assignment follows the owner's writers rule of 2026-11-08, relayed by the
coordinator ("can you have kimi, codex and agy implement things and we just review it"; "the lexer
and everything after it: writers implement, Codex for the Lean/compiler code, agy for grammar test
cases").
- Codex writes the Lean spec changes and the compiler code.
- agy writes the grammar conformance cases.
- Kimi takes the independent pieces that touch no other writer's tree, such as the I/O driver
  infrastructure, and the `Concrete.toml` checks once row 16 is answered.
- Claude briefs, reviews, gates and lands through the train.
- The two non-writing models review each piece, and caps follow the diff class (compiler and spec:
  5).

AGENTS.md's "Who works on what" (Claude on the spec, slice 0 and the front end) predates this rule.
The decisions writer reconciles the two texts.

**Order once the launch conditions hold:**
1. The spec change for `abort()` and the revised exit theorems (§3.2), shaped by the owner's answer
   to §1.
2. Lowering and backend extensions from Core abstract syntax: struct values, moves, destructuring
   and the explicit trap.
3. Facts, diagnostics, policy, assumptions and the launcher.
4. The grammar cases, the parser and elaboration to Core with its correspondence proof (after R-0622
   is approved).
5. The composed theorem, certificates and the CI legs, then qualification (B2).
