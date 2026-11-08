# CLI question audit: what the legacy compiler could answer, and whether the new design can

DRAFT for review. Proposal only: it decides nothing and changes no decision or roadmap row.

**Owner's goal (verbatim):** "is that we can answer all of this in the future and we plan ahead correctly".

**Sources.** Legacy: `legacy/Main.lean` (dispatch 3920–4490; `compileAndReport` 1656–3575;
`compileAndCheck` 3577; `compileAndQuery` 651), `legacy/Concrete/Report/Report.lean` (`queryFacts`
5924, `knownQueryKinds`/`knownFactKinds` 4871–4878), `legacy/Concrete/Check/Policy.lean`,
`legacy/docs/project/POLICY_FILES.md`, `legacy/docs/compiler/COMPILER_API.md`. New: `docs/decisions.md`,
`ROADMAP.md`, `docs/proposals/{FOUNDATION,IDEAS,MERGED,SPARK}.md` on origin/main;
`docs/proposals/SLICE1_BRIEF.md` (origin/wip/slice1-brief); `Concrete/README.md` and
`Concrete/BIR.lean` (origin/wip/slice0); `ConcreteSpec/Spec/Ident.lean` (origin/wip/spec-core);
STD_* proposals (origin/wip/std-placement); the 2026-11-08 language decisions (LANG-1108).

**Classes.** (a) covered: a row, task or slice answers it. (b) deliberately dropped: a decision
removes it. (c) MISSING: nothing answers it. (d) partial: what is missing is said.

**Architecture column.** For every question not dropped: what the compiler must record from the
start so the question can be answered later without retrofitting; the earliest slice where that
data must exist (usually where the IR holding it is created); and whether the current design
provides it: OK, or FLAG (not provided by the slice-0 design, the slice-1 brief, C1–C10 or the
spec-core identities; flags are collected under "Urgent findings").

## Table

### A. Authority, effects, resources

| # | Legacy surface | Plain-words question | Class | New-design anchor or gap | Must record from the start | Earliest slice | Prereq |
|---|---|---|---|---|---|---|---|
| 1 | `--report caps` | Which capabilities does each function need? | a | R-0575 (field from slice 0; real at slice 6) | capability set per `FnId` from the `with(...)` judgment, with each use's `SiteId` | 6 (field 0) | OK (C5, C9) |
| 2 | `--report authority` | For each capability, which functions use it, directly or transitively? | a | R-0575 composition and module rollups | call edges (caller `FnId`, callee, call `SiteId`) | 5 | OK (C1, C9 summaries) |
| 3 | `--query why-capability:FN:CAP` | Why does this function need capability C: through which call path? | d | facts give the set and drill-down (R-0575); the `why` command is R-0201, phase F | per call edge, the capabilities it contributes | 5/6 | OK |
| 4 | `--report effects` | What may this function do (authority, allocation, recursion, loops, FFI, trust, evidence)? | a | R-0575, slice 0 | one typed fact record per `FnId` | 0 | OK |
| 5 | `--report alloc` | Which functions allocate, free or defer cleanup? | a | R-0575 allocation; R-0595 heap facts at slice 19 | allocation sites with `LineageId`; cleanup sites | 19 (field 0) | OK (C4, C7) |
| 6 | `--report unsafe` (with native libraries) | Which functions are trusted, extern or use raw memory, which native libraries are linked, and what do they assume? | a | R-0575 trusted/foreign/opaque; slices 24–25; R-0509 | boundary kind per `FnId`, named premise ids, link-name and library per extern | 24 (field 0) | OK |
| 7 | `--report assumptions` | Which assumptions does each function's conclusion rest on, dependencies included? | a | R-0575 `conditional(as)`; R-0589 assumption → facts | premise ids on every conditional fact | 0 | OK |
| 8 | `--report trust-edges` (project mode) | Which functions directly reach a trusted or foreign root, across packages? | d | R-0575 drill-down; R-0201 trust-diff (F); R-0588/R-0383 package boundaries | direct call edges as stored data, keyed by package-qualified identities | 5 (packages 17/G) | FLAG U3 |
| 9 | `--report arithmetic` | For each arithmetic site: proved safe, checked, wrapping or saturating? | a | R-0493; R-0575 implicit trap sites (slice 0) | `SiteId` per operation plus its discharge-token kind | 0 | OK |
| 10 | `--report recursion` | Which functions are recursive, in which cycles? | a | R-0575 recursion depth; slice 5 (OD82 per component) | call graph with SCCs | 5 | OK |
| 11 | `--report stack-depth` | How much stack can this use; is there a bound? | a | R-0575 stack from slice 0 (final artifact, both backends); proven bounds slice 5 | frame slots per BIR function; final-artifact frame sizes per backend | 0 | OK |
| 12 | `--check predictable`, `--query predictable:FN`, policy `predictable` | Does this function or program meet the predictable profile? | a | R-0576, slice 1 | policy result per function over fact ids | 1 | OK |
| 13 | policy keys `no_alloc`, `no_unsafe`, `no_trusted`, `no_externs`, `deny`, `forbidden_capabilities`, `allowed_capabilities`, `max_stack_bytes` | Does the program stay within its declared authority and resource budget? | d | mechanism decided (`Concrete.toml` `[policy]`, R-0589, R-0576; stack budget slice 0); capability budgets R-0184 → R-0201 (F); key names open (SLICE1_BRIEF row 16a) | policies read fact ids; unknown never satisfies | 1 | OK |
| 14 | policy `require-proofs` | Is every obligation of every function proved (a proof floor)? | d | safety obligations are mandatory at admission (no-hidden-traps); a floor for user contracts is not a named policy (R-0219, R-0201, F) | status per obligation | 8 | FLAG U1 |
| 15 | policy `forbid-assume` | Does any claim rest on `assume` or an unproved hypothesis? | a | R-0575 `conditional` carries transitive `assume` debt; a policy may refuse; `assume` never admits safety (LANG-1108 item 24) | each `assume` as a premise id | 8 | OK |
| 16 | policies `require-two-kernels`, `solver-evidence`, `solver-assumption` | Must obligations be closed by two kernels; may solver results count? | b | only kernel-checked proofs count (proof-infrastructure row); no SMT for admission (no-hidden-traps row); R-0448 and R-0214 deferred | — | — | — |
| 17 | `--report profile [--profile N]` | Which build profile is in effect and what does it change? | b | OD124: profiles are not behaviour modes; the replacement question "which policies cover this function" is R-0576 | — | — | — |

### B. Program structure and representation

| # | Legacy surface | Plain-words question | Class | New-design anchor or gap | Must record from the start | Earliest slice | Prereq |
|---|---|---|---|---|---|---|---|
| 18 | `--report layout` | What are the size, alignment and field offsets of each type on this target? | d | R-0502 one layout authority (Memory CFG, C3; default layout decided); no report or query names it | a layout table keyed by `DeclId` and target profile, produced by Memory CFG | 9 (scalars 0) | OK (data); surface missing |
| 19 | `--report interface` | What is this module's public API? | a | R-0556, slice 17 | — | 17 | OK |
| 20 | `--report mono` | Which generic instances exist, and which call requested each? | d | R-0510, R-0588, Mono Core at slice 11; no report | specialization identity = (generic id, type and capability arguments) plus requesting `SiteId` | 11 | FLAG U4 |
| 21 | `--report traceability`, `--query traceability[:FN]` | Which IR code came from which source function and site? | d | slice 0: every MIR, Memory CFG and BIR instruction carries its origin (Concrete/README.md); no query | origin edge per instruction in every IR, Mono included | 0 | OK (Mono: U4) |
| 22 | `--report backend-contracts` | What does the code generator guarantee for this program (ABI, target, runtime, arithmetic)? | a | R-0581 BIR contract; R-0503; R-0301; trusted-base statement in facts (slice 0) | — | 0 | OK |
| 23 | `--report generated-implementations` | What code did the compiler generate (cleanup, glue, temporaries), and from what? | d | slice-0 origin edges for generated temporaries; R-0512 (F); no report; the generating rule is not recorded | origin edge "generated by rule R for site S" on every generated item | 1 (14 for `defer`/`Destroy`) | FLAG U11 (minor) |

### C. Proofs and evidence

| # | Legacy surface | Plain-words question | Class | New-design anchor or gap | Must record from the start | Earliest slice | Prereq |
|---|---|---|---|---|---|---|---|
| 24 | `--report proof-status`, `proof`, `--query proof:FN`, `concrete check` summary | What is each function's proof status? | a | R-0575 proof status per obligation; `#[proof(path)]` at slice 8 | obligation identity per site, the `TheoremId`, the status | 0 (proofs 8) | FLAG U1 |
| 25 | `--report obligations`, `obligation-ledger`, `vcs` | What must be proved, where, of which kind, under which hypotheses? | d | R-0575 lists obligations with site and status; each obligation's statement (hypotheses and conclusion as a Lean proposition) is not named before R-0177/R-0495 (E) | each obligation a typed record: id, kind, site, hypotheses, conclusion | 0 (octagon contexts 3) | FLAG U1, U9 |
| 26 | `--report eligibility` | Which functions are in the provable subset, and why not the others? | a | by design: an accepted function is in its slice; an unsupported form gets "not supported in this slice" (slice-0 surface row) | refusal record distinguishing unsupported from invalid | 0 | OK |
| 27 | `--report extraction`, `lean-stubs` (model part) | Which functions have a Lean model, which were excluded? | b | no extraction step: Core is the Lean semantics and the compiler is Lean (Lean-is-the-spec and proof-infrastructure rows) | — | — | — |
| 28 | `--report model-diff` | Does the proof model compute what the binary computes? | a | composed theorem; classified differential R-0491; per-native-trace refinement check (foundation question 17) | — | 0 | OK |
| 29 | `--report check-proofs`, `prove --check` | Does the kernel accept every linked proof? | a | `#[proof(path)]` slice 8; proofs re-checked each build; R-0509 replay | `TheoremId` resolved to a Lean declaration (spec-core Ident) | 8 | OK |
| 30 | `--report fingerprints`, `body-bytes`, stale statuses | Is a proof still about the current body? | a | different mechanism: certificates bind exact source bytes (R-0552); proofs re-checked each build | the theorem refers to the function's Core definition by identity | 8 | OK (see U10) |
| 31 | `--report proof-deps` | Which contracts, theorems and assumptions does this proof rely on? | d | R-0509 axioms per claim; R-0575 conditional premises; C9 typed evidence graph; no proof-dependency query | evidence-graph edges theorem → contract/theorem/premise | 5 (call evidence) / 8 | OK (C9) |
| 32 | `--report proof-diagnostics`, `prove --show-obligation`, `--nearest-lemmas`, `--emit-lean`, `lean-stubs`, `--emit-artifacts`, `--workspace`, `--emit-link` | Why is this obligation not proved, and what exactly should I write? | d | R-0177 proof authoring (E); R-0479 next action; R-0599 replayed counterexamples (B1); the proof surface arrives at slice 8, before R-0177 | obligation statement as a Lean proposition (U9) | 8 | FLAG U9 (scheduling) |
| 33 | `prove --json`, `--capabilities`, `--schema`, `--help=agent` | What can this compiler do for an agent, machine-readably? | d | schema R-0593/R-0178; next actions R-0479; no feature-discovery surface | — | 0 | OK |
| 34 | `prove --replay`, `--emit-lean-replay` | Can I re-run the proof check myself? | a | replay command in every facts answer (R-0575); R-0509; independent re-check (con-leche, nanoda) as a slice-0 follow-up | — | 0 | OK |
| 35 | `--report receipts`, `proof-status --receipts`, `--out` | Has this exact claim been kernel-checked, and can that be stored and re-verified offline? | a | R-0552 certificates from slice 0; R-0353 receipts (F) | — | 0 | OK |
| 36 | `--report attestation-join` | Which claims are attested by which checker runs? | a | R-0353, R-0509 | checker identity and version in each certificate | 0 | OK |
| 37 | `--report proof-bundle` | Export all the evidence for an independent consumer | a | R-0353 (F) | — | F | OK |
| 38 | `--report subject-facts`, `impl-manifest` | Which definition and which implementation is each theorem about? | d | `TheoremId` (Ident); R-0482 separate model/implementation/proposition/contract identities is phase E, after `#[proof]` at slice 8 | subject binding theorem ↔ `FnId`/obligation | 8 | FLAG U10 (scheduling) |
| 39 | `--report package-identity` | Which package is this, and what does it define? | d | build identity in facts (slice 0); package identity R-0383 (G) | package component in module identity | 17 | FLAG U3 |
| 40 | `--report migration`, `migration-manifest` | (legacy fingerprint V1→V2 migration) | b | R-0512: no authority from legacy evidence | — | — | — |
| 41 | `--report contracts` | Which contracts does each function have, and are they valid and discharged? | a | slice 5 `#[requires]`/`#[ensures]`; R-0473 (E); status in facts | — | 5 | OK |
| 42 | `--report vcgen-diff`, `term-ir` | (legacy-internal differentials of its VC generator) | b | the new checker is proved sound (R-0574, R-0499); no string VC layer | — | — | — |
| 43 | `--report bridge-check` | Could a "proved" obligation be refuted by a concrete input? | a | checker soundness proof (R-0574, R-0499); R-0599; fuzzer R-0272 | — | 0 | OK |
| 44 | `--report core-semantics-diff` | Do two independent evaluators agree on this program? | a | R-0491 classified differential (Lean semantics, native, legacy oracle); question 17 (the Rocq-extraction second evaluator is not carried) | — | 0 | OK |
| 45 | `--report lowering-agreement`, `bool-kernel`, `multi-kernel`, `--rocq`, `--isabelle`, `--all-provers`, `--require-two-kernels` | Do several independent proof kernels agree? | b | only kernel-checked Lean proofs count; R-0448 deferred, owner pull only | — | — | — |
| 46 | `--smt`, `--emit-smt`, `--smt-timeout-ms`, `--smt --replay`, `--report solver-cert` | What does an SMT solver say, and can a kernel corroborate it? | b | no SMT for admission (no-hidden-traps row); R-0214, R-0451 deferred | — | — | — |
| 47 | `--report artifact-fuzz` | Does the compiled binary honour the safety claims on valid inputs? | a | R-0259 (E); R-0272 | — | E | OK |
| 48 | `--report consistency`, `--report verify` | Are the compiler's internal invariants intact for this program? | a | by construction: per-pass proofs or validators (R-0583, R-0504), per-compilation certificate (R-0552), totality (R-0553) | — | 0 | OK |
| 49 | `--report audit`, `concrete audit`, `--query audit:FN`, `--query evidence:FN` | One summary of authority, trust, allocation, proofs and obligations | a | the facts query from slice 0 (R-0575); `audit`, `explain`, `why` R-0201 (F) | — | 0 | OK |

### D. Diagnostics and schemas

| # | Legacy surface | Plain-words question | Class | New-design anchor or gap | Must record from the start | Earliest slice | Prereq |
|---|---|---|---|---|---|---|---|
| 50 | `--diagnostics-json`, `--report diagnostics-json` | What are the errors, machine-readably? | a | R-0593 `--format=json`, slice 0 | — | 0 | OK |
| 51 | error-tolerant front end (`partial` flag) | What are all the problems in this file, not only the first? | c | no row; the checker result type is fixed in A2i before the freeze (R-0592, "one refusal constructor per premise"); nothing says whether one run reports several refusals | a result type able to carry several refusals with their sites | A2i (before the freeze) | FLAG U6 |
| 52 | `--report schema` | What is the machine schema of facts and diagnostics? | a | R-0593, R-0178 | — | 0 | OK |
| 53 | `--report diagnostic-codes` | What does each diagnostic mean? | a | rule IDs are Lean names with a revision; retired-ID registry (R-0603); browsable owner view (R-0555) | — | 0 | OK |
| 54 | `--query KIND`, `KIND:FN`, `fn:FN` | Show all facts of one kind, or all facts of one function | a | `concrete facts FILE [FN]` (R-0575); filtering by kind is a view over the same record | — | 0 | OK |
| 55 | exit codes (`staleEvidence`, `obligationsMissing`, usage) | Can CI tell from the exit code why a command failed? | a | R-0479 (refusals survive through exit codes); policy result satisfied/violated/incomplete | — | 0 | OK |

### E. Pipeline introspection and debugging

| # | Legacy surface | Plain-words question | Class | New-design anchor or gap | Must record from the start | Earliest slice | Prereq |
|---|---|---|---|---|---|---|---|
| 56 | `--emit-core`, `--emit-ssa`, `--emit-ssa-unverified`, `--emit-llvm` | What does my program look like at IR stage X? | d | slice-0 `Output` keeps every intermediate (toks, ast, p, sites, tokens, mir, mem, bir, ll, lcb); `.ll` written under `.lake/concrete/`; no command, and no text printers for Core, MIR, Memory CFG or BIR in the slice-0 design | a deterministic printer per IR | 0 | FLAG U8 (minor) |
| 57 | `--trace-pipeline`, `concrete trace` | Which stage first refused my program, and what did each stage produce? | c | no row; the stage registry and each stage's `Except Refusal` exist | each refusal records its stage (registry id) as well as its rule id | 0 | FLAG U5 |
| 58 | `--emit-trace-json` | How long and how big is each stage? | a | R-0550 per-stage time and memory from slice 0 (report-only; command unnamed) | — | 0 | OK |
| 59 | `debug-bundle`, `validate-bundle` | Give me everything needed to reproduce this compiler failure; is a bundle complete? | d | R-0503 carries R-0075/R-0107 (retained artifacts; compiler vs toolchain defects); reproducible builds (R-0577) and the build identity make a bundle cheap; no format or command; R-0281 native debugging deferred | bundle = source closure, `Concrete.toml`, build identity, failing stage, intermediates | 0 | OK (needs U5) |
| 60 | `reduce --predicate` | What is the smallest program that still fails the same way? | a | R-0272 with R-0009's obligations (bounded execution, preserved failure class) | stable failure class = stage + rule id | 0 | needs U5 |
| 61 | `diff old new [--json]`, `snapshot` | Did any fact or claim weaken between two versions? | a | R-0201 diff (F); R-0589 assumption diff (slice 1); R-0556 interface diff (17); a snapshot is the facts JSON (R-0575) | edit-stable keys for functions, sites and obligations | 0 | FLAG U2 |
| 62 | `--report compiler-ledger` | What did this build read, produce and depend on (modules, imports, artifacts, digests, timings, cache)? | d | R-0497 artifacts bound to payloads; R-0577 build identity; R-0550 timings; R-0528 import closure; R-0149 (G); no single build record or query | one build record per exact SHA: input and output digests, per-stage time | 0 | OK |

### F. Daily commands

| # | Legacy surface | Plain-words question | Class | New-design anchor or gap | Must record from the start | Earliest slice | Prereq |
|---|---|---|---|---|---|---|---|
| 63 | `build`, `-o`, project mode | Build this file or project | a | slice 0 `concrete build FILE -o OUT`; `Concrete.toml` from slice 1 | — | 0 | OK |
| 64 | `run [-- args]` | What does this program do when I run it with arguments? | c | no `run` command; argument access is unplaced (ROADMAP "grep at 18: command-line input gap") | — | 18 | OK |
| 65 | `--interp [-- args]` | What does the reference semantics say this program does, without native code? | d | Lean evaluator R-0496; I/O driver R-0586 (infrastructure slice 1, full at 16); no user command | — | 1 | OK |
| 66 | `test [--module]`, `--test` | Run the tests | a | test runner slice 8, modules slice 17 | — | 8 | OK |
| 67 | `fmt`, `--fmt` | Format this source | a | slice 17 | — | 17 | OK |
| 68 | `check` | Does the project pass every check and are all proofs current, without building? | d | `concrete facts` plus policies; no check-only command and no exit-code contract for `facts` | — | 0 | OK |
| 69 | `--version` | Which compiler is this? | a | build identity (R-0577) | — | 0 | OK |
| 70 | `--query` for the editor | Hover, go-to and live facts in an editor | a | R-0398 (G) over the shared session | — | G | OK |

## Questions the new design adds (legacy had none)

Closed status per fact with coverage (established, conditional, refuted, not established, not
applicable); final-artifact stack bound per backend with the `Concrete.toml` budget; the trusted-base
statement; build identity and reproducible builds; full-observation comparison of two backends;
kernel-checked per-compilation certificates; termination evidence or the responsible
`#[may_not_terminate]` permission and its call path; heap byte bounds per allocator lineage
(R-0595); frames and output-to-input dependencies (`#[depends]`, LANG-1108 item 28); vacuity and
contract-strength labels (R-0585); replayed counterexamples (R-0599); the derived "always aborts"
fact (R-0600); unused permissions as facts; secret release sites, noninterference and constant time
(slice 7); assumption diffs (R-0589); `spec_version` and per-rule revisions; "not supported in this
slice" refusals; named assurance properties per module (R-0606).

## Urgent findings: prerequisites the current design does not provide

- **U1. No obligation identity.** C9 lists obligations among the typed identities, but
  `ConcreteSpec/Spec/Ident.lean` (wip/spec-core) has `SiteId` and `TheoremId` and no obligation
  identity. Facts, proof status, policies and diffs key on it; the facts schema is frozen from
  slice 0.
- **U2. No edit-stable key.** `SiteId` is a number minted per compilation (slice-0 `BIR.lean`, Ident);
  `InstanceId` is "numbered by elaboration". Diffs, assumption diffs, policy baselines and proof
  binding across edits need a persistent key (for example `FnId` plus an obligation key relative to
  the function). R-0492 promises "persistent evidence identity" but the schema freezes first.
- **U3. No package in identities.** `ModuleId` is a path from the package root; `FnId` is module plus
  name. Two packages with the same module path collide (legacy bug 074). Packages are R-0383 (G),
  modules slice 17, but the identity types and the schema are frozen earlier.
- **U4. No specialization identity.** Mono Core arrives at slice 11; Ident's `InstanceId` is a trait
  instance numbered by elaboration. Facts through specialization (R-0588) need (generic, arguments,
  requesting site).
- **U5. Refusals do not name their stage.** R-0593's record carries rule IDs; trace, reduction
  predicates and debug bundles need the stage, and backend or adapter refusals have no Lean rule.
- **U6. One refusal or all of them.** The checker result type is fixed before the freeze (R-0592);
  whether a run reports every refusal is undecided. This is an owner question.
- **U7. Two span shapes.** Spec-core `Span` is a point (path, line, column); slice-0 `BIR.Span` is a
  byte range without a path, plus `Loc`. Facts and diagnostics need one range type before the schema
  freezes.
- **U8. No IR printers** besides LLVM text and the LCB render (minor).
- **U9. Obligation statements are not exported** as Lean propositions before R-0177 (E), but users
  write `#[proof]` at slice 8 (scheduling).
- **U10. Subject binding** (R-0482, E) comes after `#[proof]` at slice 8 (scheduling; legacy bug 079).
- **U11. Generated code does not record its generating rule** (minor).

## Gaps and proposed slices

| Gap | Rows | Proposed slice | Owner question or scheduling |
|---|---|---|---|
| obligation identity and edit-stable keys in the identity types and the facts schema (U1, U2) | 14, 24, 25, 61 | A2i (C9) before the slice-0 schema freeze | scheduling (inside C9's existing scope) |
| package component in identities (U3) | 8, 39 | A2i (C9) | scheduling |
| specialization identity (U4) | 20, 21 | A2i (C9), used at 11 | scheduling |
| stage on every refusal record (U5); `concrete trace` | 57, 59, 60 | B0 (R-0593) | the command is an owner question (CLI surface); the record field is scheduling |
| several refusals per run (U6) | 51 | A2i (R-0592) | owner question |
| one span type (U7) | all located facts | A2i / B0 | scheduling |
| IR printers and an emit command (U8) | 56 | B0 | command: owner question |
| obligation statements and stubs before proofs (U9, R-0177 increment) | 25, 32 | slice 8 | scheduling |
| subject binding before proofs (U10, R-0482 increment) | 38 | slice 8 | scheduling |
| `why` call paths, trust edges, layout, mono and traceability queries | 3, 8, 18, 20, 21 | facts views as each construct lands (5, 9, 11) | scheduling |
| run and interpret commands; check-only command and exit codes | 64, 65, 68 | `interp` at 1 or 16, `run` at 18; check at B0 | owner question (command set) |
| debug bundle and build record | 59, 62 | B0/B1 under R-0503 | scheduling |
| generated-code origin rule (U11) | 23 | 1 | scheduling |
| agent feature discovery | 33 | B0 with the schema | scheduling |
| proof floor policy for user contracts | 14 | slice 8 | owner question (policy key) |
