# Slice 1: writer brief (draft, not launched)

Status: a draft brief for the writers of [slice 1](../../ROADMAP.md#vertical-slices) (queue row
B1). Nothing here is a decision. No implementation starts until three things hold: slice 0 is on
main, the lexer (`wip/slice0-lexer`) is on main, and every spelling marked *pending* below has the
owner's answer.

**Unlanded sources.** This brief cites two branches that are not on main:
- `wip/record-language` at `1547734f8`, which the owner signed off on 2026-11-08 and which has
  not landed yet. It defines R-0625 and the 2026-11-08 language decisions.
- `wip/slice0` at `ea5181b5`, which carries the slice-0 compiler and its CI legs.

If either branch changes before it lands, re-check this brief against the landed text.

## 1. Scope

Slice 1 is decision row [Q8.2](../decisions.md#q8-2-revision), confirmed by the
[vertical slices](../decisions.md#decided-project-vertical-slices-2026-11-06). The ROADMAP slice
table lists what it adds:

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

Slice 1 also meets the slice shape:
- Its rules are Lean judgments, with accepted, refused and trapping conformance examples.
- Every stage, from source to BIR and to both backends (LLVM and the LCB), is proved, or is
  checked by a proved validator.
- The composed theorem keeps the LLVM premises and the LCB premises separate.
- The facts report and the diagnostics cover the new constructs.
- CI compares the full observations from the Lean semantics and from both backends.

Slice 1 is cumulative on slice 0. It extends slice 0's constructs and does not reintroduce them.
LP0 (R-0572) follows slice 1 before any growth.

Out of scope:
- integer widths other than `u8`;
- `bool`, branches, loops and calls;
- capabilities beyond what the launcher and the I/O driver need;
- every construct of slice 2 and later.

## 2. The programs it must build

Every program below exists as a conformance example. Each example names:
- its rule;
- its source;
- its outcome: an exit status, a refusal with its Lean rule ID, or a trap kind (R-0425).

Each program runs through the Lean semantics and through both native builds.

1. **The component (accepted).** It declares an empty linear struct and builds one value of it. It
   moves the value to a second local, consumes it with a destructuring `let`, and returns a `u8`
   that a checked `+` computes. The `+` must have evidence. Expected: exit with that value.
2. **Use after move (refused).** Like program 1, except that it destructures the moved-from local.
   Expected: refused under `OWN-1`, with the diagnostic record naming the rule.
3. **Not consumed (refused).** The linear value is never consumed before `return`. Expected:
   refused under `OWN-2`.
4. **Copy struct (accepted).** An empty `Copy` struct is read twice. This control shows that the
   linearity rules key on `Copy` (TYPE-14). Its declaration spelling is pending; see §4.
5. **Known violation (refused).** A program whose checked `+` adds `200` and `56` in `u8` is
   refused (Q6.1, OD118), together with the field-wise variant from
   [Q6.2](../decisions.md#decided-2026-11-05-q6-2-fields). The variant's typed source form waits
   for R-0625.
6. **Explicit trap (accepted, traps).** A program reaches `abort()`. Expected: the trap kind
   `abort` (decision item 4) and the launcher's trap observation (Q8.5). This trap is explicit, so
   `checked_no_implicit_traps` still holds.
7. **The predictable policy.** Two programs with `[policy]` turned on in `Concrete.toml`:
   - program 1, accepted;
   - a program whose policy fact is unknown or incomplete, refused naming the function, the fact
     and its source (R-0576).

   Slice 1 has no allocation, blocking or foreign calls, so the refusal case is built from an
   unknown fact, such as a missing stack bound where the policy demands one.
8. **Assumptions.** A change to `[assumptions]` with no matching record fails the
   assumption-diff check. An unknown section, key or version is refused (R-0589).
9. **I/O driver.** The Lean evaluator runs one program through the driver, with an expected outcome
   that does not depend on the compiler (R-0586, infrastructure only).

The current spec examples (`ConcreteSpec/Spec/Examples.lean`: `token`, `unit0`, `pair`,
`notConsumed`, `fieldOverflowTrap` and the rest) show these programs in an illustrative surface.
Their surface text is not a spelling choice. Several of their forms are superseded by the
2026-11-08 language decisions (see §4).

## 3. Dependencies

### 3.1 On main before launch
- Slice 0 (`wip/slice0` at `ea5181b5`, owner-signed), landed through the train and green on main.
- The lexer (`wip/slice0-lexer` at `7e8c926c`, branch CI green), landed after slice 0.
- `wip/record-language` (`1547734f8`, owner-signed). It defines R-0625 and the 2026-11-08 language
  decisions.

### 3.2 Spec contracts slice 1 builds on (`ConcreteSpec`, PROPOSED until A2c)
- **`Spec/Slice1Syntax.lean`.** The Core forms: A-normal form, fresh `Var` identities, and explicit
  `copy x` and `move x` reads (Q9.1).
- **`Spec/Slice1Ownership.lean`, `Spec/Slice1Checker.lean` and `Proofs/Checker.lean`.**
  - the inductive judgments `HasType`, `UseAtom`, `Uses`, `StmtOk`, `BodyOk` and `Checked`;
  - the executable `check`, with `check_iff`, the correspondence R-0499 needs.
- **`Spec/Slice1Semantics.lean` and `Proofs/Slice1.lean`.**
  - the total, fuel-indexed `run`, whose outcomes are exit, trap, stuck and out of fuel;
  - `checked_never_stuck`, `checked_no_implicit_traps` and `checked_exits`.
- **`Spec/Rep.lean` and `Proofs/Rep.lean`.** The layout and byte encoding of the slice-1 types,
  with `rep_roundtrip`, for the values-to-bytes boundary and the Memory CFG (R-0580).
- **`Spec/Observation.lean` and `Proofs/Observe.lean`.** What the launcher shows the parent
  process.
- **`Spec/Slice0.lean` and `Proofs/Slice0.lean`.** `toCore`, the embedding of slice 0 into slice 1,
  which must stay valid as slice 1 grows.
- **Foundation contracts that must hold first** (the
  [foundation-first decision](../decisions.md#decided-2026-11-07-foundation-first); FOUNDATION.md):
  - C2, scalars, failure and arithmetic, which defines the trap kind `abort` (R-0608);
  - C1, execution, calls and cleanup, which gives the meaning of an explicit trap;
  - C9, identities, primitive contracts, evidence and ghost erasure;
  - C6, observations and environment, for the launcher and the I/O driver.

**Missing from the spec today.** Slice-1 Core does not yet have `abort()`. `Proofs/Slice1.lean`
says that slice 1 has no explicit `abort()`, a statement written before the 2026-11-06 move of
`abort()` into slice 1. Before the compiler work, a spec change must add it to the syntax, the
semantics, the checker and `checked_no_implicit_traps`'s statement. That change has a compiler and
spec cap of 5.

### 3.3 Lexer and grammar pieces
- **Already in the lexer** (`Concrete/Lex.lean` and `ConcreteSpec/Spec/Surface0.lean` at
  `7e8c926c`):
  - UTF-8 decoding and source checking (`decodeUtf8` and `checkSource`, with soundness and
    completeness);
  - identifier start and continue;
  - `//` line comments.
- **New for slice 1 in the lexer:** the struct-declaration keyword, `,` (for structs with fields,
  as in the `pair` example), and whatever `abort` lexes as: a reserved word or an ordinary
  identifier resolved against the builtin table. That is part of the closed builtin inventory,
  which is pending (§4).
- **Still open in the lexer:** the first-error-offset property, a TODO at `7e8c926c`.
- **The grammar.** The parser waits for the written grammar (R-0622): the owner approves it, it is
  encoded as the Lean `Grammar` judgment (Q8.3), and `ll1_sound` holds. On `wip/record-language`,
  R-0622 depends on R-0625. Slice 1's parser therefore waits for R-0625 as well.

### 3.4 Roadmap tasks with a slice-1 increment (queue row B1)

R-0425, R-0499, R-0492, R-0493, R-0514, R-0501, R-0503, R-0497, R-0504, R-0507, R-0521, R-0552,
R-0528, R-0479, R-0118, R-0573, R-0576, R-0577, R-0579, R-0580, R-0582, R-0586, R-0588, R-0589,
R-0599 and R-0600.

## 4. Surface spellings it needs

The table lists every surface form slice 1's programs use.
- **decided**: the row fixes the exact text, so the parser can implement it.
- **pending (R-0625)**: the 2026-11-08 language decisions name the form as illustrative or leave
  it to the spelling round (items 55 and 59).
- **open question**: no decided row or R-0625 item names it, so it goes to the owner.

This brief chooses none of them.

| # | form | used by | status | source |
|---|---|---|---|---|
| 1 | `fn main() -> u8 { … }` | every program | decided | slice-0 surface ([row](../decisions.md#decided-slice0-surface-2026-11-06)); result arrow, item 18 |
| 2 | `let x: u8 = e;` (typed immutable `let`) | every program | decided | slice 0; item 18 (every local has a written type) |
| 3 | decimal literals, leading zero refused | every program | decided | slice-0 surface row; typed suffixes such as `200u8` are not needed when the `let` carries the type (their grammar is item 41, pending) |
| 4 | checked `a + b` | programs 1 and 5 | decided | slice 0; ARITH-3 |
| 5 | `return a;` | every program | decided | slice 0 |
| 6 | parentheses for grouping | inherited | decided | slice 0 |
| 7 | `//` line comments, ASCII case-sensitive identifiers | every program | decided | items 30 and 31 |
| 8 | struct declaration: the keyword and the empty field list (`struct Token {}` in the spec) | programs 1–7 | pending (R-0625) | empty structs are kept (item 50, decided). The declaration's exact text is not in a decided row; the full grammar comes from R-0622, after R-0625 |
| 9 | struct with fields (`struct Pair { a: u8, b: u8 }` in the spec), with commas and an optional trailing comma | program 5 (field-wise variant) | pending (R-0625) | commas mandatory and trailing comma optional are decided (item 43); the declaration form is pending, as in row 8 |
| 10 | marking a struct `Copy` (`struct Copy Unit0 {}` in the spec) | program 4 | open question | TYPE-14 decides that `Copy` is declared, not how it is written; neither item 40's reserved words nor item 59's list names it |
| 11 | struct literal: empty (`Token {}`) and with fields (`field: value`) | programs 1–7 | pending (R-0625) | item 8 (constructors always write `field: value`) is among items 6–29, illustrative until the spelling round |
| 12 | destructuring pattern (`let Token {} = u;`, `field: binder`) | programs 1, 2 and 5 | pending (R-0625) | item 9 (patterns always write `field: binder`) is illustrative; the treatment of ignored fields is kept |
| 13 | the type written on a destructuring `let` | programs 1, 2 and 5 | pending (R-0625) | item 18 requires a written type on every local and leaves the typing rule for destructuring to item 55 |
| 14 | surface read of a linear local (a plain `t` in the spec's examples, `move t` in Core) | programs 1 and 2 | open question | Q9.1 fixes Core's `copy x` and `move x`, not whether source text writes them. Item 12's move-out is the heap move-out, a different form. The owner decides whether this belongs in R-0625 |
| 15 | `abort()`: the call's text, its place in the builtin inventory and its result type | program 6 | pending (R-0625) | the name `abort` and the trap kind are decided (Q8.2 revision, item 4). The closed builtin inventory and its signatures are item 59 (OPEN-4) |
| 16 | where `abort()` may appear in a slice-1 body: as a statement, or as a value under `return` | program 6 | open question | slice 1 bodies are `let`s followed by `return`. The Unit statement rule (OD29) arrives at slice 3. How a bottom-typed call fits slice 1's grammar is not decided |
| 17 | the struct keyword and other words of slice 1 in the reserved-word table | lexer | pending (R-0625) | item 40 fixes some reservations (`spec`, `by`, `kept`, `Self`, `while`, `Entropy`). The full keyword table comes with the written grammar (R-0622) |
| 18 | `Concrete.toml` `[policy]` and `[assumptions]`, their key names and versioning, and how the predictable policy is turned on | programs 7 and 8 | open question | the tables are decided as typed and versioned, with unknown keys or versions refused ([row](../decisions.md#decided-project-concrete-toml)). The key names and the version field's text are not decided. This is project configuration, not language syntax: it is not in R-0625 and goes to the owner separately |
| 19 | the facts-report and diagnostics names for the new constructs (rule IDs such as `StmtOk.let_.admitted@2`) | all | decided | rule IDs are the Lean rule's name plus its revision ([row](../decisions.md#decided-project-diagnostics-2026-11-06)), so they come from the spec, not from a spelling choice |

Rows 8–17 block the parser, through R-0622. Row 18 blocks only programs 7 and 8; Lean judgments
and the lowering work for programs 1–6 do not depend on it.

## 5. Gates (the definition of done)

Every slice-0 CI leg stays green on the cumulative program set. Slice 1 adds the following.

**Conformance (R-0425):** every program in §2, accepted, refused or trapping, passes the Lean
runner and both native runners, and each refusal matches its Lean rule ID.

**Spec-to-checker correspondence (R-0499, R-0507):**
- `check_iff` holds on the extended Core;
- the positive and rule-breaking mutation controls pass;
- type and ownership soundness hold over the executable semantics, `abort` included;
- `checked_no_implicit_traps` keeps its meaning: the only traps an accepted program reaches are
  explicit.

**Lowering (R-0501, R-0580):**
- every lowering slice 1 uses has a preservation proof: struct values, destructuring, moves, the
  explicit trap, and their layout through the Memory CFG under one layout authority;
- the BIR → LCB-IR adapter proof is extended to the slice-1 subset (R-0579).

**Native builds:**
- both backends, with the unverified LLVM path labelled and its premises listed separately
  (R-0503);
- reproducible builds of every slice-1 example (R-0577);
- stack bounds, AddressSanitizer runs and binary size, as in slice 0.

**Facts and diagnostics:**
- the facts report covers structs, moves, `abort` (the may-abort and always-aborts facts, R-0600)
  and policy coverage (R-0575, R-0576);
- every diagnostic is one record in the frozen, versioned JSON schema, and failures are never
  turned into facts (R-0479);
- replayed counterexamples for "not established" are produced where applicable (R-0599).

**Policy and assumptions:** the predictable-policy refusal controls pass, and the assumption-diff
check fails on an unrecorded change (R-0576, R-0589).

**The launcher:** the pinned launcher ABI and trap mapping (Q8.5) match the Lean observation for
program 6.

**The fuzzer:** the differential fuzzer grows by one feature (structs and moves), and the
adversarial interaction programs are added (R-0573).

**Proof integrity:**
- `scripts/proofs/axioms.sh`, with any new golden lines justified line by line;
- the trust ban;
- the trusted-base gate;
- the external checker.

**The composed theorem:** it covers slice 1 under named premises, with the LLVM and LCB premises
kept separate.

## 6. Writers and reviews

The work follows the [writers rule](../../AGENTS.md):
- Codex writes the Lean spec additions and the compiler code.
- agy writes the grammar conformance cases, accepted and refused.
- Kimi takes the independent pieces where they do not overlap another writer's tree. Candidates
  are the I/O driver infrastructure and the `Concrete.toml` checks, once row 18 is answered.
- Each writer works in a fresh full clone on its own `wip/` branch, under bounded, resource-limited
  runs.
- The other two models review each piece, and Claude reviews and gates it. Pieces land only
  through the train.
- Caps follow the diff class: compiler and spec pieces have a cap of 5.

Suggested order, each step a reviewable piece:
1. The spec change for `abort()` (§3.2). No spelling is needed, because Core is abstract syntax.
2. Lowering and backend extensions for the slice-1 Core: struct values, moves, destructuring and
   the explicit trap. These are driven from Core abstract syntax, so they can begin before the
   parser.
3. The facts, diagnostics, policy and launcher pieces.
4. After R-0625 and R-0622 are approved: the grammar judgment cases, then the parser and the
   elaboration to Core, with its correspondence proof.
5. The composed theorem and the CI legs, then qualification (B2).

Steps 1–3 depend only on landed slice 0 and the spec. Step 4 is the only one that waits for the
spelling round. The coordinator decides when each step launches.
