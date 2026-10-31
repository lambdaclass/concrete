# Proposals: three structural changes to the specification

Status: proposal (for the project owner, 2026-10-30). Nothing here is decided or implemented. They come
from the spec quality audit of 2026-10-30 (Codex and Grok over `docs/spec` at `d21a4cfd`), which found
the grammar out of step with the rule entries, rules that mix what is checked with what runs, and
examples that serve as both documentation and conformance tests. Each proposal states the change, what
it fixes, what it costs, and what it leaves open. They are independent; any subset can be taken.

Numbers below are from the specification on 2026-10-30: 17 chapters, 289 entries, about 390 examples
(211 accept, 175 reject, 6 abort), and a grammar of 61 productions in 417 lines of
`grammar/concrete.ebnf`, checked for LL(1) by `scripts/tests/check_grammar_ll1.sh` with eight recorded
conflicts in `grammar/ll1_known_conflicts.tsv`.

## 1. Each grammar production lives in its rule entry; the EBNF is generated

**Change.** Every production moves into the spec entry that owns the construct (a `struct` declaration
into the types or traits entry that defines it, `for i from a until b` into STMT-6, and so on), in a
fenced `ebnf` block. `grammar/concrete.ebnf` becomes a generated file, assembled from those blocks in
chapter order by `spec.py`, and checked byte for byte like `SPEC.md` (S9). The LL(1) checkers read the
generated file unchanged. A production with no owning entry is a gate failure, as is an entry whose
example uses a form no production derives (the parser already reports which productions an example
exercises, so the check is mechanical).

**Fixes.** The audit's grammar findings are all one kind: an entry states a form the grammar lacks
(qualified calls `m::f()`, `import a.{f}`, `pub` fields, value `if`/`match`, bodyless declarations), or
the grammar keeps a removed form (C-style `for`, `break <expr>`, `E#V`). Today nothing ties the two.
With the production inside the entry, changing a rule changes its production in the same diff, and
OPEN-77 (the resync) stops being a separate standing task.

**Cost.** One-time: about 3 – 5 days. Placing 61 productions (some shared, such as `expr` and `type`,
which get a home in the syntax chapter), the generator and its byte check (about 150 lines of
`spec.py`, modelled on `single`), the "every production has an owner" and "every example is derivable"
checks, and the resync of the productions the audit found missing or stale, which has to happen anyway
(OPEN-77) and is most of the work. Ongoing: none beyond today's; an entry edit that changes syntax
edits one more block. Risk: the shared non-terminals need one clear owner, or ownership becomes
arbitrary; the proposal gives them to SYN-1.

**Leaves open.** Whether the EBNF or the entries' sentences are normative while they disagree (the
audit's open question on grammar status, raised as an OPEN entry). After this change they cannot
disagree for long, but the question needs an answer for the transition.

## 2. Each rule separates validation from execution

**Change.** Following the WebAssembly specification, an entry whose rule has both a static and a
dynamic side states them as two labelled parts: **Validation** (when a program is accepted: the
refusals, their codes and stages) and **Execution** (what an accepted program does: values, traps,
order of effects). Entries that are only one of the two say so. The entry format in
`docs/spec/MAINTAINERS.md` §2.1 gains the two optional labels, and S1 checks that a `reject` example
belongs to a validation part and an `accept` or `abort` example to an execution part.

**Fixes.** Several audit findings are a rule whose sentence mixes the two and leaves one side
implicit: STMT-11 ("remaining locals are destroyed": a static obligation or a run-time action?),
the counted loop (bounds evaluated once at run time; their type checked statically), contracts
(which values an `ensures` may read, versus when it is evaluated relative to deferred calls),
`[e; N]` (accepted for which `e`, evaluated how many times). Splitting makes each side's gap visible
and gives the Lean semantics (R-0496, R-0499) a direct target: validation maps to the checker judgment,
execution to Core's executable semantics.

**Cost.** About 4 – 6 days to restate the roughly 120 entries that have both sides (the others gain
a one-word label), with no change of meaning: the restatement is a version bump per entry (S2), so the
registry grows by about 120 superseded rows and the compiler's rule tags change version for those
entries (a mechanical edit of `Concrete/Report/Diagnostic.lean` and the `spec:` comments). Every
restated sentence needs review, since splitting a sentence is where meaning slips; budget one Codex
and one Grok pass per chapter. Ongoing: new entries are written in the split form.

**Leaves open.** Whether a refusal that only exists to forbid a run-time behaviour (for example a
trap the checker can prove) is stated under validation or execution; the proposal says validation,
with the execution part naming the trap it prevents.

## 3. A separate conformance suite with typed assertions

**Change.** The examples stay in the entries as illustrations, and a separate suite,
`tests/conformance/`, holds the conformance programs, each one file with a header naming its rule and
one assertion kind: `assert_return` (exit status and output; today's `accept`), `assert_trap` (the
trap kind; today's `abort`), `assert_invalid` (code and stage; today's `reject`), and
`assert_malformed` (a parse refusal, today a `reject` at stage `parse`). The suite is the
specification's test of an implementation: a second implementation, or the Lean model, runs it without
reading the chapters. Each entry lists its conformance files; S4 runs the suite instead of the inline
examples, and an entry with no conformance file fails as an entry with no example does today.

**Fixes.** The audit found examples doing two jobs badly: illustrations carrying test scaffolding (the
`main` migration to `u8` wraps 272 examples in a comparison), pending examples whose `now=` pins
current behaviour inside normative text, placeholder codes (`code=new`, `code=none`) in a document
meant to read as the language, and long fixture inventories in rationales. With a suite, an
illustration can be minimal, pending pins and fixture lists move to the suite and the registry, and
the suite can grow to the edge cases (one per clause, as the class-13 audit did by hand) without
lengthening the chapters.

**Cost.** About 3 – 4 days for the move: a converter from today's fenced examples to suite files (the
attributes map one to one), the runner change in `spec.py` (about 100 lines; the compile-and-compare
core is reused), the registry rows for suite files, and site changes so a chapter links its tests.
Then a choice: keep both the inline example and the suite file for the 390 current examples (no
editorial work, some duplication), or trim the inline examples to illustrations over time. Ongoing:
two places to touch when a rule changes (the entry and its tests), which S4's "every entry has a
conformance file" check keeps honest.

**Leaves open.** Whether `tests/rules/` and `tests/programs/` (the compiler's own corpus, which S12
already reads as witnesses) merge into the suite or stay the implementation's tests.
