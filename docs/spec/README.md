# The Concrete Language Specification

**Status:** the written language specification, started 2026-10-23. It is normative wherever Core's semantics in Lean does not yet define a rule (see "Authority" below).

`docs/spec/` is the one written statement of the Concrete language. It has one chapter per
area, and every rule and every definition in it has a stable, versioned ID. Other documents, the
website and the compiler cite a rule by its ID and do not restate it. The compiler, the
interpreter and the test corpus are checked against this directory by
`scripts/tests/check_spec.sh`.

This follows Austral's discipline: the language should fit in one document that a reader can
audit.

**Authority.** The normative definition of the language is Core's semantics in Lean: Core's
executable semantics and the checker judgments on Core (R-0496 and R-0499 in
[VERIFIED_PIPELINE.md](../compiler/VERIFIED_PIPELINE.md)). `docs/spec/` is the checked, readable
view of that definition. It is not a second definition. Each entry links the Lean definition it
describes (`Semantics:`). Where that definition exists, it governs, and the entry's sentence and
examples are checked against it. Where it does not exist yet, the sentence is a provisional
statement and the missing link is tracked debt, which closes as the Core semantics lands. The
link is optional today and becomes required area by area (§2.1, S7). Once an entry has a link,
it never loses it (S3).

Authority moves one chapter at a time (TIGHTENING_DECISIONS D11). When an area's chapter is in
this directory and checked, the chapter replaces the area's old prose home, and the old document
becomes a design record (§6). The rules' Lean definitions then govern, and a disagreement
between them and the chapter fails the gate, because every example runs against the
implementation. Until an area's chapter migrates, its old prose home stays normative.

This README defines the format, the gate and the migration plan. It states no language rules.

## 1. Layout

```
docs/spec/
  README.md   this file: format, gate, migration plan (not part of the language)
  <chapter>.md  one chapter per area: references.md, scope.md, patterns.md, ffi.md, ...
  ids.tsv     registry of every rule, definition and example ID ever allocated (append-only)
  codes.tsv   classification of every diagnostic code in the compiler's ledger
```

The spec is one document with many chapters. A chapter covers one area of the language, and the
website publishes one page per chapter (§4). The chapter inventory is the authority for which
areas exist (§8).

| chapter | ID prefix | state |
|---|---|---|
| `references.md` | `REF-` | pilot |
| `scope.md` | `SCOPE-` | pilot |
| `patterns.md` | `PAT-` | migrated: PAT-1 to PAT-8 (renamed binders, exhaustiveness, arm shapes, guards, destructuring `let`, `if let` / `while let`, ranges and alternatives, where an enum destructuring `let` may appear) |
| `ffi.md` | `FFI-` | migrated: FFI-1 to FFI-5 (foreign function values, Layout authority, extern declarations, layout attributes, symbol names) |
| `ownership.md` | `OWN-` | migrated: OWN-D1, OWN-1 to OWN-15 (linearity, discards, overwrites, mutability, `defer`, destruction, value flow per constructor) |
| `pipeline.md` | `PASS-` | migrated: PASS-1 to PASS-11 (specialization, binder modes, the native-profile theorem, the trusted LLVM profile, one boundary entry per stage) |
| `types.md` | `TYPE-` | migrated: TYPE-1 to TYPE-16 (callable values, dispatch, owned `Text`, typing of expressions and operators, aggregates, calls, builtins, construction rights, aliases, kinds, generic arguments, finite size) |
| `memory.md` | `MEM-` | migrated: MEM-1 to MEM-6 (two-layer memory and quasi-concrete addresses pending, initialization, field access, heap ownership, array bounds) |
| `trusted.md` | `TRUST-` | migrated: TRUST-1 to TRUST-7 (pointer operations, undefined behaviour, references from raw pointers, capabilities, `trusted`, trusted modules, moving out of raw memory); handle capabilities and callable-value capabilities still in their documents |
| `statements.md` | `STMT-` | migrated: STMT-1 to STMT-10 (outcomes, abort only, single-threaded, evaluation order, exits in operands, loops, `main`, value blocks, returning on every path, test functions) |
| `arithmetic.md` | `ARITH-` | migrated: ARITH-1 to ARITH-6 (floats, literals, checked arithmetic, named operations, conversions, comparisons) |
| `contracts.md` | `CON-` | migrated: CON-1 to CON-4 (partial correctness, contract claims, the specification judgment pending, ghost code) |
| `modules.md` | `MOD-` | migrated: MOD-1 to MOD-4 (name resolution, visibility, one binding per name, module files) |
| `syntax.md` | `SYN-` | migrated: SYN-1 to SYN-3 (the grammar, operator precedence, reserved spellings) |
| `traits.md` | `TRAIT-` | migrated: TRAIT-D1, TRAIT-1 to TRAIT-10 (coherence: one impl per trait and type, no overlap, instance locality, impl targets, one method name per type, unambiguous bound calls, order independence; declared traits, impl conformance, satisfied bounds) |
| `decisions.md` | `OPEN-` | open: OPEN-1 to OPEN-22 (the owner decisions OD1 to OD22 of ROADMAP exit 2n, stated as questions with no answer encoded) |

**pilot**: the chapter is written in this format and checked by the gate. **stub**: the chapter
holds only the rules the pilot needed, and the rest of its area migrates later.

A chapter for a new area, such as `traits.md` (`TRAIT-`, trait coherence), uses this format
from its first commit.

## 2. Format

### 2.1 Entries

An **entry** is a rule or a definition. Its ID is `<PREFIX>-<n>`, where `<PREFIX>` is the
chapter's prefix, written in upper case. A definition's number has a `D` in front of it
(`REF-D1`). An entry consists of the following, in this order:

```
<a id="REF-3"></a>
### REF-3. No reference is stored

**REF-3@1.** <exactly one sentence>

- Kind: refusal
- Uses: REF-D1, REF-D2
- Codes: E0239 (check, post-mono)
- Status: enforced
- Enforced by: Concrete.verifyNoStoredRefs, Concrete.checkModule
- Semantics: none
- Obligation: INV-REF (R-0500, V1)
- Evidence: tested

**Rationale.** <prose, not normative>

<examples, §2.2>

**Conformance fixtures.**   (optional)

| fixture | expected | pair | modes |
|---|---|---|---|
```

1. **The anchor line** `<a id="<ID>"></a>`. It gives the same `#REF-3` link target on GitHub,
   in a local preview and on the website.
2. **The heading** `### <ID>. <title>`.
3. **The normative sentence.** It is one paragraph that starts with `**<ID>@<version>.**` and
   holds exactly one sentence. Inline code spans and the abbreviations `e.g.` and `i.e.` are
   ignored when sentences are counted. Any other `.`, `!` or `?` followed by a space and a
   capital letter starts a second sentence, which is refused. The version is a positive
   integer. It increases whenever the sentence's meaning changes, and the compiler's
   diagnostics carry the same versioned tag (`rule: REF-3@1`).
4. **The fields.** Each field is one bullet. The fields appear in the order shown and none
   repeats. An unknown field is refused. Values that are lists are separated by `, `.
   - `Kind:` one of `refusal`, `guarantee`, `definition` or `preservation`:
     - A **refusal** rejects a breaking program with a diagnostic.
     - A **guarantee** is a semantic property with no diagnostic of its own.
     - A **definition** fixes a term that rules use. It is as normative as a rule.
     - An **open** entry states a language question that only the project owner decides (the
       owner decisions OD1 – OD22 of ROADMAP exit 2n). It encodes no answer: it has no codes, no
       examples and no `Enforced by:` names, its `Status:` is `open ODn` and its `Evidence:` is
       `open`. Its rationale names the rules it bears on and what current main does, which stays
       provisional until the owner answers; the answer turns it into the rules it decides.
     - A **preservation** statement is one pipeline stage's contract: the relation between its
       input and its output (for example, Mono's output refines its input, with binder modes
       unchanged), stated against the stage boundaries of
       [PIPELINE_CONTRACTS.md](../compiler/PIPELINE_CONTRACTS.md) and the pass interface of
       VERIFIED_PIPELINE ("Passes"). Its `Enforced by:` names the pass or its validator. Its
       examples are accepted programs whose result must be the same on every consumer.
   - `Uses:` the definitions the sentence depends on, or `none`. When a definition's version
     increases, every entry that uses it must increase its own version in the same change.
   - `Codes:` for a refusal, one or more `E<4 digits> (<stage>, ...)`. A pending refusal whose
     code is not allocated yet writes `new (<stage>)`. The stages are pass tags
     of `error[<pass>]`, from the vocabulary of
     [PIPELINE_CONTRACTS.md](../compiler/PIPELINE_CONTRACTS.md): `parse`, `resolve`, `check`,
     `elab`, `mono`, `post-mono`, `core-check`. Guarantees and definitions have `none`.
   - `Status:` one of `enforced`, `partial <tracking>` (the rule holds for its non-pending
     examples, and the pending ones are tracked) or `pending <tracking>` (decided, not
     implemented). A tracking ID is a roadmap item (`R-0496`) or a bug (`bug-181`).
   - `Enforced by:` the Lean declarations of today's compiler that implement the entry, as
     fully qualified names, or, for a library contract, a std declaration written
     `std/src/<file>.con#<Name>` (whose `//` comment block carries the `spec:` comment). It is
     required unless the status is `pending`. A definition names
     the Lean predicate it denotes, such as `Concrete.tyContainsRef`.
   - `Semantics:` the declarations in Core's semantics or checker judgments that define the
     entry, or `none` while they do not exist. This is the normative link (see "Authority"
     above). `Enforced by:` names the implementation and `Semantics:` names the definition;
     they are the same declaration only when the implementation is the definition.
   - `Obligation:` what the verified pipeline must prove for this entry, given as an
     invariant or roadmap ID, or `none`.
   - `Evidence:` exactly one of `tested`, `proved: <Lean theorem>` or `trusted: <ID>`. These
     three are kept distinct:
     - `tested` means the examples and fixtures pass.
     - `proved` names a kernel-checked theorem. The gate checks that the declaration is a
       theorem and that it depends on no axiom beyond Lean's standard three.
     - `trusted` names an assumption ID recorded in
       [TRUSTED_COMPUTING_BASE.md](../verification/TRUSTED_COMPUTING_BASE.md).

     Passing examples never make an entry `proved`. The gate checks a `proved:` name's kind and
     axioms; that the theorem's statement establishes the entry is a reviewed claim. A
     definition's evidence is `by-users`: it is exercised only through the entries that use
     it, and S1 refuses a definition no entry uses.
5. `**Rationale.**` explains the entry. It is not normative.
6. **Examples** (§2.2).
7. **Conformance fixtures** (optional). Each row names a file under `tests/`, and the
   `expected`, `pair` and `modes` columns follow the contract of `check_rules_2i.sh`. The rows
   are the entry's part of a generated acceptance manifest (§5).

Text outside entries, such as chapter introductions, is explanatory and states no rule. A
relative link in a chapter must resolve to a file in the repository.

### 2.2 Examples are tests

An example is a fenced block. Its info string is `con`, followed by space-separated attributes:

```
info     := "con" SP verdict (SP attr)*
verdict  := "accept" | "reject" | "abort"
attr     := "name=" NAME                 required; [a-z0-9-]+, unique within the entry
          | "exit=" N                    accept only, required; 0 <= N <= 125
          | "code=" CODE                 reject only, required; E<4 digits>, or "new" if pending
          | "stage=" STAGE               reject only, required
          | "pair=" NAME                  reject only, required; the accept example of the same
                                          entry that is this program's rewrite
          | "pending=" TRACKING
          | "now=" OUTCOME               required if and only if pending
          | "now-interp=" OUTCOME        optional, only when pending
          | "trap=" TEXT                  abort only, required: the interpreter's run-time error names it
          | "interp-exempt=" NAME        accept only; a reason slug, such as no-heap-in-interp
          | "project=" NAME              compile as a package of that name against this checkout's std
                                          (`concrete build`, CONCRETE_STD pinned); needs interp-exempt
OUTCOME  := "accept:" N | "reject:" CODE ":" STAGE | "fail:" TEXT   (TEXT: no spaces; `_` reads as a space)
```

An unknown or repeated attribute is refused. Every example is a complete program. Its ID is
`<entry>/<name>` (`REF-6/let-binds-ref`).

Examples run on every available **consumer**: the native binary, `--interp`, and Core's
executable semantics once it exists. VERIFIED_PIPELINE S7 makes the interpreter into that
semantics plus an IO driver, and from then on the third leg runs every example of an entry whose
`Semantics:` is set. Agreement between consumers is necessary and not sufficient, because two
consumers can implement the same wrong rule. Every leg is therefore compared with the outcome the
example states, never only with the other legs.

- **accept**: the program builds natively and the binary exits `N`. `--interp` exits 0 and
  prints `N` as its last line. `interp-exempt` drops the interpreter leg, and the gate prints
  every exemption. A third leg runs the Lean semantics (decision B17), for accept and abort
  examples that are not pending and not project examples: `--report model-diff` evaluates every
  function of the program that extracts to the ProofCore model (`Concrete.Proof.eval`, scalar
  signatures today) on boundary inputs, with the model and with the interpreter. A `MISMATCH` row
  (both produced a value and they differ) or a failing report fails the example; rows where one side
  produced no value (`partial`, `model_stuck`, `ERROR`, `exhausted`) compare nothing and do not. A
  program with no extracted function has no third leg; the gate prints how many examples it compared
  and on how many agreeing inputs.
- **abort**: the program compiles and traps at run time: the binary is killed by `SIGABRT` (the
  abort helpers of checked operations) and `--interp` stops with a run-time error, not a
  diagnostic that names the trap given by `trap=`. It takes no `exit`, `code` or `stage`.
- **reject**: the first error reported by the native compile, and the first error reported
  by `--interp`, are each `error[<stage>]: (<code>)`, and no lower layer leaked (`llvm-as`,
  the system linker, a panic). In a preservation entry (a stage boundary), the code may belong
  to another chapter's rule, so it need not be among the entry's `Codes` and no rule tag is
  required, and `code=none` is allowed for a `link` diagnostic that has no code yet.
  `interp-exempt=` on a reject example drops the interpreter leg, for a stage the interpreter
  does not have. The code must be one of the entry's
  `Codes`, the stage one of that code's stages, and the diagnostic must carry
  `rule: <ID>@<version>` for this entry.
- **pending**: the entry is decided and not implemented. The example is still run, and
  `now=` records what the compiler does today: `accept:N`, `reject:CODE:STAGE` or
  `fail:TEXT` (the consumer stops with an error whose output contains `TEXT`). `now-interp=` records a different outcome for the
  interpreter. The gate fails if the example behaves as its verdict says, because the marker is
  stale and must be removed. It also fails if the example behaves differently from `now`, which
  covers a crash, a timeout, an unrelated diagnostic and a change in one consumer. Only an exact
  match with `now` counts as PENDING. PENDING is reported separately and never counts as a pass.
  For `code=new` the verdict holds as soon as both consumers refuse the program at the stated
  stage with any code that `now` does not name; the gate then fails and asks for the code to be
  allocated and written into the entry.
- `con pseudocode` marks an illustration that is not compiled. It is allowed only outside
  entries.

## 3. The gate: `scripts/tests/check_spec.sh`

The gate runs on the built compiler and is registered as its own step in CI's `language-gates`
job. Its summary line is
`SPEC: PASS=<n> FAIL=<n> PENDING=<n> SKIPPED=<n> (entries <e>, examples <x>)`.

It fails closed in three ways. It refuses to run when it extracts zero entries or zero
examples. A setup failure (no compiler, Lean unavailable, no history base) is a failure, not a
skip. A skip is allowed only through an explicit environment variable, never when `CI` is set,
and it is counted in SKIPPED.

| check | what must hold |
|---|---|
| S1 format | Every chapter parses under §2. IDs are unique across chapters, and each ID's prefix is its chapter's. Each entry has one sentence and every field. Every `Uses` names a live definition. A refusal names at least one code, and other kinds name none. Every relative link resolves. |
| S2 registry | `ids.tsv` has one row per allocated key: an entry version (`REF-3@1`) or an example (`REF-3/stored-field`). Columns: `key`, `state` (`live`, `pending`, `superseded` or `retired:<reason>`) and, for an entry version, the SHA-256 of its normalized sentence (`-` for an example). The `live` and `pending` rows equal what the chapters contain, state for state. A sentence whose hash differs from its version's row needs a new version row. |
| S3 history | Against a base revision: no `ids.tsv` row is removed, a hash never changes, and a row's state changes only along `pending → live`, `live → superseded` (only when a newer version of the same entry exists) and `pending` or `live → retired:<reason>`. A `superseded` or `retired` row is final, so an ID is never reused. `codes.tsv` gains no `unmigrated` row. When a definition gains a version, every entry that uses it gains one in the same change. The base is `$SPEC_BASE`, or else the merge base with `origin/main`, or `HEAD~1` when that merge base is `HEAD`. |
| S4 examples | Every example behaves as §2.2 says, on both consumers, and an accepted example's extracted functions agree between the Lean model and the interpreter. |
| S5 coverage | An enforced or partial refusal has at least one non-pending reject example. Every reject example's `pair` names an accept example of the same entry, and the pair of a non-pending reject example is itself non-pending, so every refusal shows the program it refuses next to the rewrite it accepts. A pending refusal has at least one reject example. A guarantee that is not pending has at least one non-pending accept example that runs on both consumers. One exception, named by the check: a library contract whose `Enforced by:` names only std declarations may be covered by a native project example, since the interpreter runs single files. |
| S6 codes | Every code in `--report diagnostic-codes` has exactly one row in `codes.tsv`. The class is `rule` (named by an entry), `unmigrated:<chapter>`, `internal` (a compiler invariant failure, such as `ssa-verify` or `lower`), `tooling` (proof, policy and evidence tooling, not language) or `retired`. Class `rule` holds exactly for the codes some entry names. For each code, the ledger's `rules` field equals the set of `<ID>@<version>` of the entries that name it. The spec owns that mapping, and the compiler's copy is checked against it. |
| S7 sites | Each name in `Enforced by:` and `Semantics:` resolves (a generated Lean file, run with `lake env lean`). The text of its declaration, from its doc comment to the next top-level declaration, contains a `spec: <ID>` comment. Every `spec: <ID>` comment in `Concrete/**/*.lean` names a live entry. Each `proved:` name is a theorem with standard axioms only. Each `trusted:` ID occurs in TRUSTED_COMPUTING_BASE.md. |
| S8 tracking | Every tracking ID exists: an `R-` ID in ROADMAP.md or its backlog, a `bug-` ID as a record in `docs/bugs/`. |
| S9 manifests | Every generated manifest (§5) is byte-equal to its generator's output. |
| S10 site | The site generator (§4) writes one page per chapter with every entry's anchor and the rule index, `scripts/gen/site_reference.py` writes the reference pages, and `zola build` succeeds on a scratch copy of `site/`. `check_site_generated.sh` keeps both sections generated: no tracked copy, every site build runs both generators first, and every site link resolves. Without `zola` the check fails, unless `SPEC_SKIP_SITE=1` is set outside CI, which counts it as SKIPPED. CI installs the pinned Zola that `book.yml` uses. |
| S11 restatement | No normative sentence occurs in a Markdown file outside `docs/spec/` (§6). |
| S12 witnesses | `rule_sites.tsv` is byte-equal to `spec.py inventory`: one row per (refusal, code) with its stages and `Enforced by:` declarations. Every row of an enforced or partial refusal has a negative witness: a spec reject example, or a test program that is a negative leg of a structured runner (the first argument of a `run_err` invocation in `run_tests.sh`, or a reject row of a chapter's conformance table), on which the compiler exits with a failure status, without crashing or leaking a lower layer, and reports that code tagged with that rule (`rule: ID@n`); an untagged diagnostic witnesses only a code that one rule alone names. A row with none is named in `coverage_allow.tsv` with a reason (`od:ODn`, `R-NNNN`, `bug-NNN` or `unreachable:<why>`), and an allow row whose code gained a witness fails, so the list only shrinks. S12 is evidence at the level of a rule's code, not of each `Enforced by:` declaration: deleting the only site that reports a rule's code makes S12 fail, while a rule with several sites for one code needs the mutation harness to show that each is load-bearing. |
| S13 one specification | `docs/spec` is the one normative statement of the language. Every Markdown file under `docs/language` is listed in `unabsorbed.tsv` with the chapters that will absorb it; a file not listed is refused, and S3 refuses a row added to the list. Absorbing a document moves its rules into entries and its useful rationale under them, moves tracking material to ROADMAP or KNOWN_HOLES, and deletes the file and its row in one commit. |

Code-to-rule mapping (S6), behaviour (S4) and coverage (S5) are mechanical. The association
in `Enforced by:` and the placement of a `spec:` comment (S7) are reviewed claims: the gate
proves only that the named declaration exists and carries the comment, not that it enforces
the rule.

### 3.1 Negative controls

The gate first requires that the real spec passes. Each control then plants one defect in a
scratch copy, confirms the copy really changed, and requires the check to fail with that
control's own defect message. Failing for some other reason does not satisfy the control.

| control | defect planted |
|---|---|
| S1 | a duplicate ID; a two-sentence rule; an unknown attribute; a `Uses` that names a missing definition |
| S2 | an example deleted from a chapter while still registered; a sentence edited without a version bump |
| S3 | a registry row deleted relative to the base; an `unmigrated` row added |
| S4 | a wrong exit code; a wrong code; a wrong stage; a missing rule tag; a pending example that already holds; a pending example whose `now=` is wrong |
| S5 | a refusal whose only accept example is pending |
| S6 | a ledger code with no row in `codes.tsv` (an orphan code); a rule-class code no entry names |
| S7 | an `Enforced by:` name that does not resolve; a declaration without its `spec:` comment; a `spec:` comment that names an unknown ID |
| S8 | an unknown tracking ID |
| S9 | a hand edit to a generated manifest |
| S10 | a chapter missing from the generated site |
| S11 | a normative sentence copied into another doc |
| S12 | a rule code with no witness; an allow row for a witnessed code; a hand edit to `rule_sites.tsv` |
| S13 | a new document under `docs/language` |

## 4. Website

The website's language specification is generated from `docs/spec/` at build time, with no
copies. `python3 scripts/spec/spec.py site --out site/content/spec` writes `_index.md` and one
page per chapter. On each page it:

- adds Zola front matter;
- keeps the anchors;
- labels each example ("accepted, exits 42", "rejected: E0302 at check", "pending: bug-182");
- rewrites relative repository links to absolute links into the repository;

and it writes `rules.md`, the rule index: every entry with its kind, status and codes.

The reference section is generated the same way: `scripts/gen/site_reference.py --out
site/content/reference` writes one page per document it lists (ABI, architecture, safety, the value
model and so on), from the document itself, with links to other listed documents and to spec
chapters rewritten to site links. Both directories are gitignored and contain only that output.
`make docs-site`, `make docs-serve` and `.github/workflows/book.yml` run both generators before
`zola build`, and `check_site_generated.sh` refuses a tracked copy, a build that skips a generator
and a site link to a page that does not exist. The reference pages were hand copies until
2026-10-23, and every one had drifted from its document.

## 5. One source for the acceptance manifests

`tests/rules/exit_2i.tsv` is generated from the conformance tables of `references.md`,
`scope.md` and `patterns.md`, by `python3 scripts/spec/spec.py manifest exit_2i`. The
manifest's rule column is the entry ID, so the old `G4` rows become `PAT-1`. The file stays
committed for its readers, and S9 refuses a hand edit.

`check_rules_2i.sh` keeps its row checks: the rule tag, the spec evidence, the rewrite, the
ledger, binary against `--interp`, the model sidecar, and the formatter leg for PAT-1. It keeps
its structural requirements too: reject and run pairs, a model-checked SCOPE-3 row, and
binary, interp and fmt for PAT-1. It now reads its code-to-rule table from
`spec.py codes`, where it used to keep a copy.

The compiler's `spec` evidence on a rule-tagged diagnostic is the rule's anchor
(`docs/spec/references.md#REF-3`). It is computed from the tag, replacing the single constant
`ruleSpecRef`.

## 6. Other documents cite rules and do not restate them

A document outside `docs/spec/` cites a rule as `REF-3` and links to its anchor. It may
explain a rule, record its history or argue for it, but it does not repeat the normative
sentence.

S11 enforces the mechanical part of this. It normalizes each sentence and each document the
same way: Markdown emphasis, link targets and code ticks are removed, whitespace is collapsed
and the text is lower-cased. Every `*.md` in the repository is scanned, except `docs/spec/`,
the generated `site/content/spec/` (checked by S10 instead) and `docs/archive/`. A document
that contains a normalized sentence fails.

`check_docs_drift.sh` also runs S11 by itself, since S11 needs no compiler. A documentation-only
change therefore fails fast. Paraphrases are not caught. Turning a migrated area's paraphrases
into citations is part of that area's migration (§8).

[MEMORY_SEMANTICS §15](../language/MEMORY_SEMANTICS.md) was the normative text for REF and
SCOPE until this pilot. It is now their design record, keeping the probes, the status history
and the gap analysis (§15.7). It says at its top that `references.md` and `scope.md` govern.

## 7. Existing checks this builds on

Each fact has one owner. The new gate extends the existing checks rather than running beside
them.

| existing | relation |
|---|---|
| `check_rules_2i.sh` and `tests/rules/exit_2i.tsv` | The manifest is generated (§5), and the gate's code-to-rule table comes from the spec. |
| `check_diagnostic_codes_complete.sh` | It owns one fact: every code in the literal `=> "E…"` mappings of `CheckError.lean`, `CoreCheck.lean`, `Resolve.lean` and `Elab.lean` has a ledger row (widened 2026-10-23, which added E0112 – E0120 and E0420 to the ledger). S6 owns the next fact: every ledger row is classified, and rule rows match the spec. The two chain. |
| `check_pipeline_contracts.sh` and `PIPELINE_CONTRACTS.md` | Migrated (step 0): the boundary contracts are `pipeline.md`'s PASS-5 to PASS-11, their fixtures are its examples, the leak check is part of S4, and the gate is retired. PIPELINE_CONTRACTS.md is the design record. |
| `VALUE_FLOW_SPEC.md` and `check_value_flow_spec.sh` | Migrated (step 1): the per-constructor tables are OWN-15 in `ownership.md`. `check_value_flow_spec.sh` stays the owner of constructor coverage and now reads `ownership.md`; it is a fast source gate, so it stays out of `check_spec.sh`, which needs a compiler. VALUE_FLOW_SPEC.md was absorbed into OWN-15's rationale and deleted (S13). |
| `check_constructor_coverage.sh` | It checks that every compiler walker handles every constructor. That is a fact about the compiler, not the language, so it stays separate. |
| `check_doc_snippets.sh` (13r: `con`, `con reject:E…`, `con pseudocode`) | It owns snippets in other documents, at the bar of `--emit-core`. Spec examples are stricter: they run, compare both consumers and pin the stage. `docs/spec/` is outside its glob. |
| `check_docs_drift.sh` | It runs S11 (§6). |

## 8. Coverage inventory and migration plan

Every area of the language has a planned chapter. A migration step does the following:

- moves the area's normative statements into the chapter as entries, with examples and
  `spec:` sites;
- reduces the replaced document to a design record that cites IDs;
- deletes the matching hand copy under `site/content/`;
- reclassifies the area's codes in `codes.tsv` from `unmigrated:<chapter>` to `rule`.

Each step lands with the gate green. The steps run in this order:

0. **pipeline.md** (`PASS-`, migrated 2026-10-23): the boundary contracts of
   PIPELINE_CONTRACTS.md, one preservation entry per stage (Elab, Mono, CoreCheck, Lower, SSA),
   each with its relation and its evidence (D12: every pass states what it preserves). Generic
   substitution and binder modes are first (PASS-1 and PASS-2, in the pilot). `check_pipeline_contracts.sh`'s fixtures become the
   entries' reject examples, and PIPELINE_CONTRACTS.md becomes their design record.
1. **ownership.md** (`OWN-`, migrated 2026-10-23): linearity, consumption, Copy and Destroy, discards, defer.
   It replaces the normative parts of OWNERSHIP_MODEL, VALUE_FLOW_SPEC (the constructor table
   and its gate), LANGUAGE_INVARIANTS §2, IGNORED_RESULT and DEFER. The decided defer and
   exit-order rules (TIGHTENING_DECISIONS §4.5) enter as pending.
2. **types.md** (`TYPE-`, migrated 2026-10-23): type formation, aliases, generics and const generics, newtypes,
   construction rights. It replaces TYPE_ALIASES, CONST_GENERICS_V1, CONSTRUCTION_RIGHTS,
   VALIDATED_WRAPPERS and the type sections of VALUE_MODEL. D5 (a bare `T` is Any) enters as
   enforced. D6 (Copy is explicit) entered as pending and was enforced on 2026-10-23 (option (a), TYPE-14@2).
3. **patterns.md** (`PAT-`, migrated 2026-10-23): exhaustiveness, nested and destructuring
   patterns, guards, `if let` and `while let`. It replaces PATTERN_DESTRUCTURING and
   PATTERN_ERGONOMICS; the deferred decisions of NESTED_PATTERNS and TUPLES were absorbed into the patterns and types chapters ("Absent by decision").
4. **arithmetic.md** (`ARITH-`, migrated 2026-10-23): integer and float semantics, overflow, checked, wrapping and
   saturating operations, casts. It replaces ARITHMETIC_POLICY and NUMERIC_MODEL. T-ARITH and
   T-CAST enter as pending.
5. **statements.md** (`STMT-`, migrated 2026-10-23): statements and expressions, value blocks, loops and loop
   control, evaluation order, `main` and the exit status. It replaces
   STATEMENT_EXPRESSION_MODEL, LOOP_CONTROL (absorbed into STMT-6 and deleted) and the language half of MAIN_EXIT_MODEL;
   EXECUTION_MODEL stays the runtime and platform description. T-ORDER's violations (bugs 136,
   137 and 183) were fixed in landing train 6, so STMT-4 is enforced.
5a. **memory.md** (`MEM-`, migrated 2026-10-23): allocations, `Heap<T>`, what safe code may
   observe. It replaces MEMORY_MODEL, MEMORY_GUARANTEES and the remaining sections of
   MEMORY_SEMANTICS.
6. **ffi.md** (`FFI-`) and **trusted.md** (`TRUST-`), migrated 2026-10-23 except handle and callable-value capabilities: the C boundary,
   raw pointers, `trusted`, `Unsafe`, capabilities and handle capabilities. They replace the
   normative parts of platform/FFI, SAFETY, TWO_AXIS_SAFETY, CAPABILITY_FACTS,
   HANDLE_CAPABILITIES and CALLABLE_VALUES_AND_CAPABILITIES. T-RAWDUP enters enforced (TRUST-7); D8 (global state) and D10
   (the `trusted mod` marker) enter as pending.
7. **contracts.md** (`CON-`, migrated 2026-10-23): `requires`, `ensures`, invariants, ghost code, the specification
   judgment. It replaces the language half of CONTRACTS_GUIDE. T-H27 enters as pending.
8. **modules.md** (`MOD-`, migrated 2026-10-23): modules, imports, visibility, name resolution and namespaces
   (SCOPE-4's open question). It replaces VISIBILITY_AND_MODULE_HYGIENE. D9 (one file per module,
   with generated interfaces) enters as pending.
9. **traits.md** (`TRAIT-`, migrated 2026-10-23): traits, impls, bounds and coherence (D7). The trait-coherence
   branch may start this chapter at any point. It does not wait for its place in this order.
10. **syntax.md** (`SYN-`, migrated 2026-10-23): the surface grammar. `grammar/concrete.ebnf` and its LL(1) checks stay the
    owners of the productions (GRAMMAR.md and MACRO_STANCE.md were absorbed into the chapter and deleted), and the chapter cites productions by name rather than copying
    them. Its entries cover what the grammar cannot say, such as the precedence and
    trailing-value rules.

LANGUAGE_GAPS and KNOWN_HOLES are not migrated, because they record open problems, not
rules. A gap or hole that a rule will close is that rule's `partial` or `pending` tracking ID,
and the record cites the rule's ID.

The migration is complete when no `unmigrated` row remains in `codes.tsv` and every row of
the inventory above is a chapter in the pilot state. From then on S3 refuses any new
`unmigrated` row. That an area is covered at all is a property of this inventory, which is
reviewed. Diagnostics alone cannot show that a semantics without a diagnostic has a rule.
