# CLI question audit: what the legacy compiler could answer, and whether the new design can

PROPOSAL, 2026-11-08. It decides nothing and changes no decision or roadmap row. It was reviewed
once each by Codex, Kimi and agy (the prompt and raw outputs are in `cli-question-audit/`, starting with [the prompt](cli-question-audit/prompt.md)), and
their corrections are merged below.

**The owner's goal (verbatim):** "is that we can answer all of this in the future and we plan ahead
correctly".

**Sources.** Legacy (origin/main): `legacy/Main.lean` (dispatch 3920–4490, `compileAndReport`
1656–3575, `compileAndCheck` 3577, `compileAndQuery` 651, help and usage 56–90, the prove agent help
3789–3870), `legacy/Concrete/Report/Report.lean` (`queryFacts` 5924, `knownQueryKinds` and
`knownFactKinds` 4871–4878), `legacy/Concrete/Check/Policy.lean`,
`legacy/docs/project/POLICY_FILES.md`. New: `docs/decisions.md`, `ROADMAP.md`,
`docs/proposals/{FOUNDATION,IDEAS,MERGED,SPARK,SPEC_UNIFICATION}.md` and
`docs/archive/roadmap-2026-11/task_disposition.tsv` on origin/main; `docs/proposals/SLICE1_BRIEF.md`
(origin/wip/slice1-brief); `Concrete/README.md`, `Concrete/BIR.lean` and `Concrete/Premises.lean`
(origin/wip/slice0); `ConcreteSpec/Spec/Ident.lean` (origin/wip/spec-core); the STD proposals
(origin/wip/std-placement); the 2026-11-08 language decisions (LANG-1108).

**Classes.** (a) covered: a row, task or slice answers it. (b) deliberately dropped: a decision
removes it. (c) MISSING: nothing answers it. (d) partial: what is missing is said, including work
that is deferred with a trigger.

**Architecture columns.** For every question not dropped: what the compiler must record from the
start so that the question can be answered later without retrofitting; the earliest slice where
that data must exist (usually the slice where the IR holding it is created); and whether the current
design provides it: OK, or a flag U*n* (collected under [urgent findings](#urgent-findings)).

## Table

### A. Authority, effects and resources

| # | Legacy surface | Plain-words question | Class | New-design anchor or gap | Must record from the start | Earliest slice | Prereq |
|---|---|---|---|---|---|---|---|
| 1 | `--report caps` | Which capabilities does each function need? | a | R-0575 (field from slice 0; real at slice 6) | capability set per `FnId` from the `with(...)` judgment, with each use's `SiteId` | 6 (field 0) | OK (C5, C9) |
| 2 | `--report authority` | For each capability, which functions use it, directly or transitively? | a | R-0575 composition and module rollups | call edges: caller `FnId`, callee, call `SiteId` | 5 | OK (C1, C9 summaries) |
| 3 | `--query why-capability:FN:CAP` | Why does this function need capability C, through which call path? | a | R-0201 `why` (phase F, ROADMAP line 559), over the R-0575 drill-down | per call edge, the capabilities it contributes | 5–6 | OK |
| 4 | `--report effects` | What may this function do (authority, allocation, recursion, loops, FFI, trust, evidence)? | a | R-0575, slice 0 | one typed fact record per `FnId` | 0 | OK |
| 5 | `--report alloc` | Which functions allocate, free or defer cleanup? | a | R-0575 allocation; R-0595 heap facts at slice 19 | allocation sites with their `LineageId` (19); cleanup sites (`defer`, `Destroy`) (14) | 14 / 19 (fields 0) | OK (C4, C7) |
| 6 | `--report unsafe` (with native libraries) | Which functions are trusted, extern or use raw memory, which native libraries are linked, and what do they assume? | a | R-0575 trusted, foreign and opaque boundaries; slices 24–25; R-0509 | boundary kind per `FnId`; premise identities; link name and library per extern | 24 (field 0) | U12 |
| 7 | `--report assumptions` | Which assumptions does each function's conclusion rest on, dependencies included? | a | R-0575 `conditional(as)`; R-0589 assumption → facts | premise identities on every conditional fact | 0 | U12 |
| 8 | `--report trust-edges` (project mode) | Which functions directly reach a trusted or foreign root, across packages? | d | R-0575 drill-down; R-0201 trust-diff (F); R-0588 and R-0383 for package boundaries; no direct-edge view | direct call edges as stored data, keyed by package-qualified identities | 5 (packages 17/G) | U3 |
| 9 | `--report arithmetic` | For each arithmetic site: proved safe, checked, wrapping or saturating? | a | R-0493; R-0575 implicit trap sites (slice 0) | `SiteId` per operation and its discharge-token kind | 0 | OK |
| 10 | `--report recursion` | Which functions are recursive, in which cycles? | a | R-0575 recursion depth; slice 5 (OD82, per recursive component) | call graph with its strongly connected components | 5 | OK |
| 11 | `--report stack-depth` | How much stack can this use, and is there a bound? | a | R-0575 stack from slice 0 (final artifact, both backends); proven bounds at slice 5 | frame slots per BIR function; final-artifact frame sizes per backend | 0 | OK |
| 12 | `--check predictable`, `--query predictable:FN`, policy `predictable` | Does this function or program meet the predictable profile? | a | R-0576, slice 1 | policy result per function over fact identities | 1 | OK |
| 13 | policy keys `no_alloc`, `no_unsafe`, `no_trusted`, `no_externs`, `deny`, `forbidden_capabilities`, `allowed_capabilities`, `max_stack_bytes` | Does the program stay within its declared authority and resource budget? | d | the mechanism is decided (`Concrete.toml` `[policy]`, R-0589, R-0576; the stack budget from slice 0); capability budgets are R-0184 → R-0201 (F); the key names are open (SLICE1_BRIEF row 16a) | policies read fact identities; unknown never satisfies | 1 | OK |
| 14 | policy `require-proofs` | Is every obligation of every function proved (a proof floor)? | d | safety obligations are mandatory at admission (no-hidden-traps row); a floor for user contracts is no named policy (R-0219, R-0201, F) | status per obligation, by obligation identity | 8 | U1 |
| 15 | policy `forbid-assume` | Does any claim rest on `assume` or an unproved hypothesis? | a | R-0575 `conditional` carries transitive `assume` debt, and a policy may refuse it; `assume` never admits safety (LANG-1108 item 24) | each `assume` as a premise identity | 8 | U12 |
| 16 | policies `require-two-kernels`, `solver-evidence`, `solver-assumption` | Must obligations be closed by two kernels; may solver results count? | d | deferred with a trigger: R-0448 (multi-kernel), R-0214 (solver evidence as a separate class); only kernel-checked proofs count for admission | evidence identity names the checker (C9) | F | OK |
| 17 | `--report profile [--profile N]` | Which build profile or policy bundle applies, and what does it change? | a | replaced by policies: the facts query reports which policy covers each function (R-0576); OD124 keeps profiles as refusal bundles with no behaviour change | — | 1 | OK |

### B. Program structure and representation

| # | Legacy surface | Plain-words question | Class | New-design anchor or gap | Must record from the start | Earliest slice | Prereq |
|---|---|---|---|---|---|---|---|
| 18 | `--report layout` | What are the size, alignment and field offsets of each type on this target? | d | R-0502 one layout authority (Memory CFG, C3; the default layout is decided); no report or query names it | a layout table keyed by the concrete type identity (scalars, declarations, specialization arguments) and the target profile, produced by Memory CFG | 0 (scalars), 9 (structs) | OK (data) |
| 19 | `--report interface` | What is this module's public API? | a | R-0556, slice 17 | — | 17 | OK |
| 20 | `--report mono` | Which generic instances exist, and which calls requested each? | d | R-0510, R-0588, Mono Core at slice 11; no report | specialization identity = (generic, type and capability arguments); request edges from each call site to the instance, recorded separately | 11 | U4 |
| 21 | `--report traceability`, `--query traceability[:FN]` | Which IR code came from which source function and site? | d | the slice-0 design says every MIR, Memory CFG and BIR instruction carries its origin (Concrete/README.md); no query | an origin edge per instruction in every IR, Mono included | 0 | U13, U4 |
| 22 | `--report backend-contracts` | What does the code generator guarantee for this program (ABI, target, runtime, arithmetic)? | a | R-0581 BIR contract; R-0503; R-0301; the trusted-base statement in facts (slice 0) | — | 0 | OK |
| 23 | `--report generated-implementations` | What code did the compiler generate (cleanup, glue, temporaries), and from what? | d | slice-0 origin edges for generated temporaries; R-0512 (F); no report; the generating rule is not recorded | an origin edge "generated by rule R for site S" on every generated item | 0 | U11 |

### C. Proofs and evidence

| # | Legacy surface | Plain-words question | Class | New-design anchor or gap | Must record from the start | Earliest slice | Prereq |
|---|---|---|---|---|---|---|---|
| 24 | `--report proof-status`, `proof`, `--query proof:FN`, the `concrete check` summary | What is each function's proof status? | a | R-0575 proof status per obligation; `#[proof(path)]` at slice 8 with one discharge edge per obligation identity (decisions.md, slice-8 proof surface) | obligation identity per site, the `TheoremId`, the status | 0 (proofs 8) | U1 |
| 25 | `--report obligations`, `obligation-ledger`, `vcs` | What must be proved, where, of which kind, under which hypotheses? | d | R-0575 lists obligations with site and status; generated obligation schemas precede `Admission` (SPEC_UNIFICATION module order); exporting each obligation's statement as a Lean proposition is not scheduled before R-0177 (E) | each obligation a typed record: identity, kind, site, hypotheses, conclusion | 0 | U1, U9 |
| 26 | `--report eligibility` | Which functions are in the provable subset, and why not the others? | a | by design: an accepted function is in its slice; an unsupported form gets "not supported in this slice" (slice-0 surface row; `Rule.fragment` in SPEC_UNIFICATION) | refusal kind distinguishing unsupported from invalid | 0 | OK |
| 27 | `--report extraction`, the model part of `lean-stubs` | Which functions have an extracted Lean model, and which were excluded? | b | no extraction step: Core is the Lean semantics and the compiler is Lean (Lean-is-the-spec and proof-infrastructure rows) | — | — | — |
| 28 | `--report model-diff` | Does the proof model compute what the binary computes? | a | the composed theorem; the classified differential R-0491; the per-native-trace refinement check (foundation question 17) | — | 0 | OK |
| 29 | `--report check-proofs`, `prove --check` | Does the kernel accept every linked proof? | a | `#[proof(path)]` at slice 8, re-checked each build; R-0509 replay | `TheoremId` resolved to a Lean declaration (spec-core Ident) | 8 | OK |
| 30 | `--report fingerprints`, `body-bytes`, the stale statuses | Is a proof still about the current body? | a | a different mechanism: a `#[proof]` theorem must equal a current generated obligation or refer to the function's current semantic identity, and a stale one is refused (decisions.md, roles (a)–(d)); certificates bind exact source bytes (R-0552) | the theorem refers to the function's Core definition by identity | 8 | U10 |
| 31 | `--report proof-deps` | Which contracts, theorems and assumptions does this proof rely on? | d | R-0509 axioms per claim; R-0575 conditional premises; the C9 typed evidence graph; no proof-dependency view | evidence-graph edges theorem → contract, theorem or premise | 5 (call evidence), 8 | OK (C9) |
| 32 | `--report proof-diagnostics`, `prove --show-obligation`, `--nearest-lemmas`, `--emit-lean`, `lean-stubs`, `--emit-artifacts`, `--workspace`, `--emit-link`, `--out`, `--out-dir`, `--force`, `--stdout` | Why is this obligation not proved, what exactly should I write, and where may the tool write it? | d | R-0177 proof authoring (E); R-0479 next action; R-0599 replayed counterexamples (B1); the proof surface arrives at slice 8, before R-0177 | the obligation statement as a Lean proposition | 8 | U9 |
| 33 | `prove --json`, `--capabilities`, `--schema`, `--help=agent` | What can this compiler do for an agent, machine-readably? | d | the schema is R-0593 and R-0178; next actions are R-0479; no feature-discovery surface | — | 0 | OK |
| 34 | `prove --replay`, `--emit-lean-replay` | Can I re-run the proof check myself? | a | a replay command in every facts answer (R-0575); R-0509; independent re-check (con-leche, nanoda) as a slice-0 follow-up | — | 0 | OK |
| 35 | `--report receipts`, `proof-status --receipts`, `--out` | Has this exact claim been kernel-checked, and can that be stored and re-verified offline? | a | R-0552 certificates from slice 0; R-0353 receipts (F) | — | 0 | OK |
| 36 | `--report attestation-join` | Which claims are attested by which checker runs? | a | R-0353, R-0509 | checker identity and version in each certificate | 0 | OK |
| 37 | `--report proof-bundle` | Export all the evidence for an independent consumer | a | R-0353 (F) | — | F | OK |
| 38 | `--report subject-facts`, `impl-manifest` | Which definition and which implementation is each theorem about? | d | the subject is bound by the `#[proof]` roles at slice 8 and by C9 ("safety evidence bound to typed identities"); R-0482's separate model, implementation, proposition and contract identities come in phase E | the binding theorem ↔ function and obligation identity | 8 | U10 |
| 39 | `--report package-identity` | Which package is this, and what does it define? | d | the build identity is in facts (slice 0); package identity is R-0383 (G) | a package component in module identity | 17 | U3 |
| 40 | `--report migration`, `migration-manifest` | (the legacy fingerprint V1 → V2 migration) | b | R-0512: no authority from legacy evidence | — | — | — |
| 41 | `--report contracts` | Which contracts does each function have, and are they valid and discharged? | a | slice 5 `#[requires]` and `#[ensures]`; R-0473 (E); status in facts | — | 5 | OK |
| 42 | `--report vcgen-diff`, `term-ir` | (legacy-internal differentials of its VC generator) | b | the new checker is proved sound (R-0574, R-0499); there is no string VC layer | — | — | — |
| 43 | `--report bridge-check` | Could a "proved" obligation be refuted by a concrete input? | a | checker soundness (R-0574, R-0499); R-0599; the fuzzer R-0272 | — | 0 | OK |
| 44 | `--report core-semantics-diff` | Do two independent evaluators agree on this program? | a | R-0491 classified differential (Lean semantics, native, legacy oracle); question 17. The Rocq-extraction second evaluator is not carried | — | 0 | OK |
| 45 | `--report lowering-agreement`, `bool-kernel`, `multi-kernel`, `--rocq`, `--isabelle`, `--all-provers`, `--require-two-kernels` | Do several independent proof kernels agree? | d | deferred with a trigger: R-0448 (owner pull only), R-0449; admission counts only kernel-checked Lean proofs | evidence identity names the checker (C9) | F | OK |
| 46 | `--smt`, `--emit-smt`, `--smt-timeout-ms`, `--smt --replay`, `--report solver-cert` | What does an SMT solver say, and can a kernel corroborate it? | d | SMT for admission is dropped (no-hidden-traps row, IDEAS); a separate solver evidence class is deferred with a trigger (R-0214, R-0451) | — | F | OK |
| 47 | `--report artifact-fuzz` | Does the compiled binary honour the safety claims on valid inputs? | a | R-0259 (E); R-0272 | — | E | OK |
| 48 | `--report consistency`, `--report verify` | Are the compiler's internal invariants intact for this program? | a | by construction: per-pass proofs or validators (R-0583, R-0504), the per-compilation certificate (R-0552), totality (R-0553) | — | 0 | OK |
| 49 | `--report audit`, `concrete audit`, `--query audit:FN`, `--query evidence:FN` | One summary of authority, trust, allocation, proofs and obligations | a | the facts query from slice 0 (R-0575); `audit`, `explain` and `why` are R-0201 (F) | — | 0 | OK |

### D. Diagnostics, schemas and exit codes

| # | Legacy surface | Plain-words question | Class | New-design anchor or gap | Must record from the start | Earliest slice | Prereq |
|---|---|---|---|---|---|---|---|
| 50 | `--diagnostics-json`, `--report diagnostics-json` | What are the errors, machine-readably? | a | R-0593 `--format=json`, slice 0 | — | 0 | OK |
| 51 | the error-tolerant front end (its result carries a `partial` completeness flag) | What are all the problems in this file, not only the first? | d | `CheckResult.refused` carries a non-empty list of refusals (SPEC_UNIFICATION, the checker result); whether the lexer, parser and elaborator recover and keep reporting after a failure is undecided | a result type able to carry several refusals and a completeness mark | 0 | U6 |
| 52 | `--report schema` | What is the machine schema of facts and diagnostics? | a | R-0593, R-0178 | — | 0 | OK |
| 53 | `--report diagnostic-codes` | What does each diagnostic mean? | a | rule IDs are Lean names with a revision; the retired-ID registry (R-0603); the browsable owner view (R-0555) | — | 0 | OK |
| 54 | `--query KIND`, `KIND:FN`, `fn:FN` | Show all facts of one kind, or all facts of one function | d | `concrete facts FILE [FN]` selects by function (R-0575); no selection by fact kind is named | fact kind as a typed field | 0 | OK |
| 55 | exit codes (`staleEvidence`, `obligationsMissing`, usage, proof-check and internal failures) | Can CI tell from the exit code why a command failed? | d | R-0479 keeps refusals through exit codes; the policy result is satisfied, violated or incomplete; no exit-code taxonomy maps refused, incomplete, internal failure and usage | — | 0 | OK |
| 56 | `diff` exit codes (1 weakened, 2 incomparable) | Did a fact weaken, or are the two snapshots not comparable? | d | R-0201 diff (F) and R-0178's compatibility policy; no exit contract | schema and semantics versions in every snapshot | 0 | OK |

### E. Pipeline introspection and debugging

| # | Legacy surface | Plain-words question | Class | New-design anchor or gap | Must record from the start | Earliest slice | Prereq |
|---|---|---|---|---|---|---|---|
| 57 | `--emit-core`, `--emit-ssa`, `--emit-ssa-unverified`, `--emit-llvm` | What does my program look like at IR stage X? | d | the slice-0 `Output` keeps every intermediate (toks, ast, p, sites, tokens, mir, mem, bir, ll, lcb), and the `.ll` file is written under `.lake/concrete/`; no command, and no text printer for Core, MIR, Memory CFG or BIR in the slice-0 design | a deterministic printer per IR | 0 | U8 |
| 58 | `--trace-pipeline`, `concrete trace` | Which stage first refused my program, and what did each stage produce? | c | no row; the stage registry and each stage's `Except Refusal` exist | each refusal records its stage as well as its rule | 0 | U5 |
| 59 | `--emit-trace-json` | How long and how big is each stage? | d | R-0550 records per-stage time and memory from slice 0 (report-only); no command exposes it | — | 0 | OK |
| 60 | `debug-bundle`, `validate-bundle` | Give me everything needed to reproduce this compiler failure; is a bundle complete? | d | R-0503 carries R-0075 and R-0107 (retained artifacts; compiler versus toolchain defects); reproducible builds (R-0577) and the build identity make a bundle cheap; no format or command; R-0281 native debugging is deferred | source closure, `Concrete.toml`, build identity, failing stage, intermediates | 0 | U5 |
| 61 | `reduce --predicate` (built-in, message-qualified and external predicates) | What is the smallest program that still fails the same way? | a | R-0272 with R-0009's obligations (bounded execution, preserved failure class) | a failure class of stage and rule for the built-in predicates; external predicates are scripts and need nothing more | 0 | U5 |
| 62 | `diff old new [--json]`, `snapshot` | Did any fact or claim weaken between two versions? | a | R-0201 diff (F); R-0589 assumption diff (slice 1); R-0556 interface diff (17); a snapshot is the facts JSON (R-0575) | edit-stable keys for functions, sites and obligations | 0 | U2 |
| 63 | `--report compiler-ledger` | What did this build read, produce and depend on (modules, imports, artifacts, digests, timings, cache)? | d | R-0497 artifacts bound to payloads; R-0577 build identity; R-0550 timings; R-0528 import closure; R-0149 (G); no single build record or query | one build record per exact SHA: input and output digests, per-stage time | 0 | OK |

### F. Daily commands

| # | Legacy surface | Plain-words question | Class | New-design anchor or gap | Must record from the start | Earliest slice | Prereq |
|---|---|---|---|---|---|---|---|
| 64 | `build`, `-o`, project mode | Build this file or project | a | slice 0 `concrete build FILE -o OUT`; `Concrete.toml` from slice 1 | — | 0 | OK |
| 65 | `run [-- args]` | What does this program do when I run it, with arguments? | c | no `run` command; native executables exist from slice 0 and the launcher from slice 1, while argument access is unplaced (ROADMAP, "grep at 18: command-line input gap") | — | 0 (arguments 18) | OK |
| 66 | `--interp [-- args]` | What does the reference semantics say this program does, without native code? | d | the Lean evaluator R-0496; the I/O driver R-0586 (infrastructure at slice 1, every I/O example from 16); no user command | — | 1 | OK |
| 67 | `test [--module]`, `--test` | Run the tests | a | test runner at slice 8, modules at slice 17 | — | 8 | OK |
| 68 | `fmt`, `--fmt` | Format this source | a | slice 17 | — | 17 | OK |
| 69 | `fmt --check`, `--write`, `--stdin` | Is this file already formatted (a CI gate); format in place; format editor input | d | slice 17 names a formatter only; check, write and stdin modes are not named | — | 17 | OK |
| 70 | `check` | Does the project pass every check, and are all proofs current, without building? | d | `concrete facts` and the policies; no check-only command and no exit contract for `facts` | — | 0 | OK |
| 71 | `--version` | Which compiler is this? | a | the build identity (R-0577) | — | 0 | OK |
| 72 | `help`, `--help`, `-h`, `<cmd> --help` | Which commands and flags exist? | c | no help surface is named | — | 0 | OK |
| 73 | `--query` for the editor | Hover, go-to and live facts in an editor | a | R-0398 (G), over the shared session | — | G | OK |

## Counts

73 rows. (a) covered: 40. (b) deliberately dropped: 3 (rows 27, 40, 42; the admission half of row
46 is also dropped). (c) MISSING: 3 (rows 58, 65, 72). (d) partial: 27, of which rows 16, 45 and 46
are deferred with a trigger.

## Questions the new design adds (legacy had none)

A closed status per fact with coverage (established, conditional, refuted, not established, not
applicable); the final-artifact stack bound per backend, refused over the `Concrete.toml` budget;
the trusted-base statement; the build identity and reproducible builds; the full-observation
comparison of two backends; kernel-checked per-compilation certificates; termination evidence or
the responsible `#[may_not_terminate]` permission and its call path; heap byte bounds per allocator
lineage (R-0595); frames and output-to-input dependencies (`#[depends]`, LANG-1108 item 28); vacuity
and contract-strength labels (R-0585); replayed counterexamples (R-0599); the derived "always
aborts" fact (R-0600); unused permissions as facts; secret release sites, noninterference and
constant time (slice 7); assumption diffs (R-0589); `spec_version` and per-rule revisions; "not
supported in this slice" refusals; named assurance properties per module (R-0606).

<a id="urgent-findings"></a>
## Urgent findings: prerequisites the current implementation does not yet provide

The design (C9, R-0492, R-0575) already requires most of these. What is missing is their
implementation in the identity types and the slice-0 code, which are the source of the facts
schema, and that schema is frozen from the first front-end output. Each is therefore due before
that freeze or at the slice named.

- **U1. No obligation identity type.** C9 requires distinct obligation identities
  ([FOUNDATION.md](FOUNDATION.md#c9-identities-primitive-contracts-evidence-and-ghost-erasure)), and
  the `#[proof]` roles record one discharge edge per obligation identity; `ConcreteSpec/Spec/Ident.lean`
  on wip/spec-core has `SiteId` and `TheoremId` and no obligation identity. Due before the schema
  freeze.
- **U2. No edit-stable key.** C9 asks for stable source sites and R-0492 for a persistent evidence
  identity, but `SiteId` is a number minted per compilation (Ident, slice-0 `BIR.lean`) and
  `InstanceId` is "numbered by elaboration". Diffs, assumption diffs, policy baselines and proof
  binding across edits need a persistent key, for example the function identity plus an obligation
  key relative to the function. Due before the schema freeze.
- **U3. No package in identities.** `ModuleId` is a path from the package root and `FnId` is module
  plus name, so two packages with the same module path collide (legacy bug 074). C9 does not mention
  packages; R-0383 is phase G. A package component in the identity types is cheapest now.
- **U4. No specialization identity.** Ident's `InstanceId` is a trait instance numbered by
  elaboration. Mono Core (slice 11) and R-0588 need a specialization keyed by the generic and its
  type and capability arguments, with request edges from each call site recorded separately, so
  that many callers share one instance.
- **U5. Refusals do not name their stage.** SPEC_UNIFICATION's `Refusal` carries a rule (lexical,
  grammar, fragment or admission), a kind, a subject and `blockedBy`. A refusal or failure after
  admission (lowering, the adapter, a toolchain step) has no rule kind, and no record names the
  stage. Trace, reduction predicates and debug bundles need it. Due at B0 with R-0593.
- **U6. Recovery after a front-end failure.** The checker already returns every refusal; whether the
  lexer, parser and elaborator recover and keep reporting is undecided.
- **U7. Two span shapes.** The spec `Span` is a point (path, line, column); slice-0 `BIR.Span` is a
  byte range without a path, beside `Loc`. Facts and diagnostics need one type, and a range is
  likely needed for diagnostics. Ident's comment calls the column unit an owner question, but it was
  decided on 2026-11-08 (foundation question 10: bytes, a tab counts one byte), so that comment is
  stale.
- **U8. No IR printers** besides the LLVM text and the LCB render (minor).
- **U9. Obligation statements are not exported** as Lean propositions, with stubs, before R-0177
  (phase E), yet users write `#[proof]` against them from slice 8. The schemas exist in the design;
  only their export is late.
- **U10. Subject binding must be implemented by slice 8.** The design provides it (the `#[proof]`
  roles, C9); R-0482's four-identity split is a later refinement (legacy bug 079's class).
- **U11. Generated code does not record its generating rule.** Slice 0 already generates
  temporaries, so the origin record should carry the rule from slice 0 (minor).
- **U12. Premises have no typed identity.** Slice-0 `Premises.lean` names premises by strings
  (`"P_lex"`, …) and Ident has no premise or assumption type, while conditional facts, declared
  `Concrete.toml` assumptions (slice 1) and `assume` (slice 8) all key on one.
- **U13. Instruction provenance is promised but not yet implemented.** The slice-0 README says
  every MIR, Memory CFG and BIR instruction carries its origin; `BIR.lean` records origins only in
  the trap-site table. Reconcile before slice 0 is qualified.

## Gaps and proposed slices

One rule applies to every row: **a new command or flag is an owner question** (the slice-0 surface
decision named `concrete build` and `concrete facts` only), while **the record or field behind it
is scheduling** inside an existing task.

| Gap | Rows | Proposed slice | Owner question or scheduling |
|---|---|---|---|
| obligation identity, edit-stable keys and typed premise identity in Ident and the schema (U1, U2, U12) | 6, 7, 14, 15, 24, 25, 62 | A2i (C9, R-0615), before the slice-0 schema freeze | scheduling |
| package component in identities (U3) | 8, 39 | A2i (C9) | scheduling |
| specialization identity and request edges (U4) | 20, 21 | A2i (C9), used at 11 | scheduling |
| instruction provenance and the generating rule (U13, U11) | 21, 23 | B0 | scheduling |
| stage on every refusal and failure record (U5) | 58, 60, 61 | B0 (R-0593) | scheduling; `concrete trace` is an owner question |
| front-end recovery after a failure (U6) | 51 | B0 / B1 | owner question |
| one span type; fix Ident's stale column comment (U7) | every located fact | A2i / B0 | scheduling |
| IR printers (U8) and an emit command | 57 | B0 | printers: scheduling; command: owner question |
| obligation statements and stubs before proofs (U9; an R-0177 increment) | 25, 32 | slice 8 | scheduling |
| subject binding implemented with `#[proof]` (U10) | 30, 38 | slice 8 | scheduling |
| `why`, trust-edge, layout, mono, traceability, proof-dependency and fact-kind views | 3, 8, 18, 20, 21, 31, 54 | as each construct lands (5, 9, 11); `why` stays R-0201 | data: scheduling; new commands or flags: owner question |
| exit-code taxonomy for `build`, `facts`, `diff` and a check mode | 55, 56, 70 | B0 | owner question |
| `run`, `interp` and `help` commands | 65, 66, 72 | `help` at B0; `interp` at 1 or 16; `run` at 1 (arguments at 18) | owner question |
| formatter check, write and stdin modes | 69 | 17 | owner question |
| debug bundle and build record | 60, 63 | B0 / B1 under R-0503 | format: scheduling; commands: owner question |
| agent feature discovery | 33 | B0, with the schema | owner question (a command) |
| proof floor policy for user contracts | 14 | slice 8 | owner question (a policy key) |

## Reviewer verdicts

All three: **AGREE-WITH-CHANGES**. Every correction below was checked against the sources and merged,
except where noted.

- **Codex.** Rows 16, 45 and 46 are deferred, not dropped (merged). Row 17 is a replacement, not a
  removal (merged as (a)). U1, U2 and U10 are implementation gaps that the design already requires
  (merged: the section now says so). Row 18 needs a concrete type key (merged). U4 should separate
  identity from request edges (merged). `partial` is a returned flag, not a CLI flag (merged). Row 55
  is partial (merged). The `diff` exit contract (merged as row 56). External reduction predicates
  (merged into row 61). BIR records origins only for trap sites (verified, merged as U13). U7's
  column unit is an owner question: not merged, because foundation question 10 decided it on
  2026-11-08.
- **Kimi.** Rows 16, 45 and 46 deferred (merged). Row 3 is covered by R-0201 (merged). The checker
  result already carries a list of refusals (verified in SPEC_UNIFICATION, merged into row 51 and
  U6). U9 narrowed to export (merged). U10 is provided by the design (merged). U4's identity
  (merged). Row 23 starts at slice 0 (merged). Row 5 splits cleanup (14) from allocation (19)
  (merged). Row 55 partial (merged). Help commands (merged as row 72). `fmt --check` (merged as row
  69). Discovery and bundle commands are owner questions (merged as the general rule).
- **agy.** Rows 54, 55 and 59 are partial (merged). Row 30 is partial: not merged, because the
  `#[proof]` roles already refuse a stale theorem; the residual is U10. Row 65's slice (merged).
  `fmt` and `prove` flags (merged into rows 69 and 32). No premise identity in Ident (verified,
  merged as U12). U7's column unit: not merged, as for Codex. New commands or flags for views are
  owner questions (merged as the general rule).
