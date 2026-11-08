# CLI owner questions from the CLI question audit

PROPOSAL, 2026-11-08, for the owner. It decides nothing. These are the six owner questions in
[CLI_QUESTION_AUDIT.md](CLI_QUESTION_AUDIT.md#gaps-and-proposed-slices). Each one has options and
Claude's recommendation. Codex, Kimi and agy each reviewed them once, independently; their picks are
at the end, and their raw answers are in `cli-question-audit/` ([prompt](cli-question-audit/owner-prompt.md)).

**Rows every question rests on.**
- The slice-0 surface (decisions.md, `decided-slice0-surface-2026-11-06`) names two commands,
  `concrete build` and `concrete facts`.
- Diagnostics (`decided-project-diagnostics-2026-11-06`) are one typed record, rendered as human
  text or JSON lines (`--format=human|json`). There are no warnings. A fix is applied automatically
  only when a lemma proves it preserves meaning.
- R-0479: refusals are never turned into facts, they survive through results, text, JSON and exit
  codes, and diagnostics name the valid next action.
- The facts design (`decided-project-facts-report`): every fact has a closed status, an unknown fact
  never satisfies a policy, and a policy's result is satisfied, violated or incomplete, where only
  satisfied passes.
- The tools row (`decided-2026-11-07-tools`): the test runner comes at slice 8, the formatter at 17
  and the LSP later.
- Philosophy: one form per situation, the most transparent long-term option, and verbosity is fine
  when it is clearer. Most code is written by LLMs, so the CLI must be easy for an agent to drive
  and to check.
- The program's own exit statuses are decided and are separate from the compiler's: the program
  exits 0–255, 125 on a startup refusal and 134 on a trap (Q7.10, the trap line).

## Q1. Does the front end keep reporting after its first failure? (audit U6)

**Legacy.** `--diagnostics-json` ran an error-tolerant front end (Phase 4 #12a). A pass that failed
did not hide the diagnostics of passes that could still run, and the result carried a `partial`
flag. **New.** The checker returns `refused (rs : List Refusal)`, every refusal it finds, in a
deterministic order (SPEC_UNIFICATION, the checker result). For the lexer, parser and elaborator,
nothing is decided.

- **(a)** Stop at the first lexer, parser or elaboration refusal. The checker still reports all of
  its own refusals.
- **(b)** Recover in a defined way inside each stage: the lexer skips to the next token, the parser
  resynchronizes at a statement or item boundary, and elaboration continues over the items that
  resolved. Every refusal is reported, and the result is marked incomplete when a later stage could
  not run. Recovery is part of each stage's specification, so it is proved like the rest of the
  stage.
- **(c)** Like (a), but run every independent unit (file, later module) separately, so one broken
  file does not hide another's refusals. There is no recovery inside a unit.
- **(d)** Recover only in an unverified diagnostics-only path, as legacy did, with the proved path
  stopping at the first refusal.

**Recommendation: (c) now, and (b) for the parser when a workload shows that one refusal per file
is too slow to fix.** (c) gives agents and people every independent refusal without making
recovery part of the proved front end. (d) is a second front end, which the one-form rule refuses.
(b) adds proof work to the LL(1) parser now for a convenience.

## Q2. An exit-code scheme for `build`, `facts` and `diff`, and a check-only mode

**Legacy.** One taxonomy (`legacy/Main.lean` 7–29): 0 success, 1 usage, 2 obligations missing,
3 stale evidence, 4 proof-check failure, 5 solver failure, 6 internal error. It was not applied
everywhere: `concrete check` exited 1 on any issue, and `diff` exited 1 on a weakened fact and 2 on
incomparable snapshots. **New.** R-0479 and the policy results above; no numbers are decided.

- **(a)** One small taxonomy for every command, from the typed result: 0 accepted or satisfied,
  1 refused (a program refusal, a violated policy, a weakened fact in `diff`), 2 incomplete (an
  unknown fact, an analysis limit, an incomparable snapshot or schema version), 3 usage, 4 internal
  or toolchain failure. The JSON record gives the detail. It is generated from one Lean definition,
  as legacy's help block was.
- **(b)** A finer taxonomy like legacy's, one code per cause (refused, policy violated, proof
  missing, proof rejected, incomplete, usage, internal, toolchain).
- **(c)** 0 or 1 only; the JSON record gives every reason.
- **Check-only mode.** (i) `concrete facts` gets the taxonomy and is the check, with no separate
  command; (ii) `concrete check` runs admission, the facts and the policies without code
  generation; (iii) `concrete build --no-emit`.

**Recommendation: (a) with (i).** (a) keeps the decided three-valued policy result (satisfied,
violated, incomplete) visible to CI without parsing, and keeps internal failures apart from
refusals. (b) duplicates in numbers what the record already says. (c) loses "incomplete", which the
facts design says must never read as a pass. For the check, (i) avoids a second command that does
what `facts` already does. Artifact facts such as the stack bound need a build, so `facts` reports
them "not established" when no artifact exists.

## Q3. The `trace`, `emit`, `run`, `interp` and `help` commands

**Legacy.** `--trace-pipeline` named the first failing stage; `--emit-core`, `--emit-ssa` and
`--emit-llvm` printed an IR; `run [-- args]` built and ran; `--interp [-- args]` ran the reference
interpreter; `--help`, `help` and `-h` printed grouped help. **New.** Slice 0's `Output` keeps every
intermediate, the Lean evaluator is the reference interpreter (R-0496, the I/O driver R-0586), and
none of these commands is named.

- **(a)** All five as subcommands: `concrete trace`, `concrete emit --stage <core|mir|mem|bir|llvm|lcb>`,
  `concrete run`, `concrete interp`, `concrete help`.
- **(b)** Fold the inspection into the two commands that exist: `concrete build --emit <stage>` and
  `concrete facts --trace`; add `concrete run` and `concrete help`. The evaluator runs only in CI's
  differential.
- **(c)** Add `help` now and nothing else until a slice needs it; `trace` and `emit` stay developer
  scripts.
- **(d)** Like (a), but `interp` is `concrete run --evaluator`, one command with two engines.
- **(e)** (a) without `run`: `trace`, `emit`, `interp` and `help`, each scheduled with its data
  (`help` first, `emit` with the IR printers, `interp` with the evaluator's I/O driver), and `run`
  decided at slice 1, when the launcher exists.

Every option except (c) extends the decided two-command slice-0 surface, so each is an owner
decision.

**Recommendation: (e).** Each answers one question and maps to one data source
(the stage registry, one IR, the evaluator, the command table), which is the most transparent.
(d) merges two different meanings (the native build and the Lean semantics) under one name. (b)
hides a question behind a flag on a command that answers another. `run` adds nothing that
`build` plus executing the file does not do.

## Q4. The formatter's `--check`, `--write` and `--stdin` modes (slice 17)

**Legacy.** `concrete fmt FILE` printed to stdout, `--check` exited nonzero when the file was not
formatted, `--write` rewrote the file, and `--stdin` formatted standard input. A file is formatted
when formatting its parse gives the same bytes back.

- **(a)** All three, with legacy's meaning and the Q2 exit codes.
- **(b)** `--check` and `--write` only; editors pipe a temporary file.
- **(c)** `--check` only, as a CI gate; the formatter otherwise prints to stdout.
- **(d)** A canonical form that is part of admission: an unformatted file is refused.

**Recommendation: (a).** `--check` is the CI gate, `--write` is the daily use and `--stdin` is what
editors and agents need without touching files. (d) makes layout a language rule, which no decided
row asks for and which would refuse correct programs for whitespace.

## Q5. Agent feature discovery

**Legacy.** `concrete prove --capabilities` printed a JSON object of supported features, obligation
kinds, evidence classes and the schema version; `--schema` printed the JSON schema; `--help=agent`
printed a workflow guide. **New.** The JSON schema is generated from the Lean types (R-0593); every
output carries `spec_version`; nothing lists features.

- **(a)** `concrete schema` prints the generated JSON schema of diagnostics and facts, and
  `concrete capabilities` prints the supported slice, the commands, the fact kinds, the rule IDs and
  the versions, all generated from the Lean registries.
- **(b)** Only `concrete schema`; feature support is read from the schema and the slice number.
- **(c)** Nothing on the CLI; the schema and the rule registry are published files.
- **(d)** (a) plus a workflow guide for agents, like `--help=agent`.

**Recommendation: (a).** An agent should learn what this exact compiler supports from the compiler,
generated from the same registries, so the answer cannot drift. (d)'s guide is prose that goes
stale; the next actions in each diagnostic (R-0479) already say what to do.

## Q6. A proof-floor policy for user contracts (from slice 8)

**Legacy.** Policy `require-proofs` refused a project with any eligible function whose proof was
missing, stale or blocked. **New.** Safety obligations cannot be left open: admission refuses them
(no hidden traps). A user's own contract (an `#[ensures]` or a stated property) may be "not
established" and is then reported as a fact; no policy key requires it.

- **(a)** A policy key in `Concrete.toml` `[policy]`, for example `require-contract-proofs`, scoped
  like the predictable policy (project or module, tighten only, transitive to dependencies): every
  stated contract in scope must be established; conditional on named premises passes only if the
  policy lists them.
- **(b)** No key: an unproved `#[ensures]` is refused at admission, as safety obligations are.
- **(c)** No key: the facts report the gap and CI checks the JSON itself.
- **(d)** (a), plus a strength floor: a point-instance proof does not satisfy it (R-0585 strength
  labels).

**Recommendation: (d).** It is a policy over existing facts, as the facts and Concrete.toml rows
require, and it uses the strength labels so that a single-input proof cannot pass a floor meant for
a contract. (b) changes what a contract means, and an unproved contract is legitimate during
development. (c) makes every project re-implement the same check outside the compiler.

## Reviewer picks

One independent round, 2026-11-08 (Codex in a git-archive copy of origin/main, Kimi read-only,
agy with the context inlined). Raw answers: [Codex](cli-question-audit/owner-codex.md),
[Kimi](cli-question-audit/owner-kimi.md), [agy](cli-question-audit/owner-agy.md).

| Question | Claude | Codex | Kimi | agy |
|---|---|---|---|---|
| Q1 front-end recovery | c (b later) | c, runner-up b | c, runner-up b | c, runner-up b |
| Q2 exit codes and check mode | a+i | a+i, runner-up a+iii | a+i, runner-up a+iii | a+i, runner-up a+ii |
| Q3 trace, emit, run, interp, help | e | a, runner-up d | e, runner-up c | c, runner-up b |
| Q4 formatter modes | a | a, runner-up c | a, runner-up b | a, runner-up b |
| Q5 agent discovery | a | a, runner-up d | a, runner-up d | a, runner-up b |
| Q6 proof floor | d | d, runner-up a | d, runner-up a | d, runner-up a |

Q1, Q2, Q4, Q5 and Q6 are unanimous. Q3 splits: Codex wants all five commands, Kimi agrees with the
recommendation, and agy wants only `help` until a slice needs more, because the slice-0 surface
names two commands. (e) was missing from the reviewed draft's options; Kimi, Codex and agy each
pointed that out, and it is added above.

**Points the reviewers raised, for the owner's answer:**
- Q1 (Kimi, Codex): `CheckResult` cannot carry refusals and "incomplete" together, so (b) or (c)
  needs a coverage mark for skipped stages and dependent units, as legacy's `partial` flag did.
- Q2 (Codex, Kimi, agy): an unknown fact makes a command incomplete only when a policy requires it;
  the precedence of mixed outcomes needs a rule; and `facts` on a tree with no built artifact makes
  artifact facts, such as the stack bound, incomplete. Artifact reuse needs a rule.
- Q4 (Codex, Kimi): combined modes (`--check --write`, `--stdin --check`) should be refused as
  usage; legacy silently chose one.
- Q5 (Codex, Kimi): a capability listing must distinguish recognized rules from implemented
  features and supported targets. A workflow guide is allowed if it only teaches and links, or is
  generated from the command and next-action registries.
- Q6 (Codex): the floor must require evidence for the exact contract, state its scope and refuse a
  vacuous discharge (R-0585), not only check a strength label.
- agy's claim that `spec_version` was replaced by per-rule revisions is not right: the runtime row
  keeps one `spec_version` header in every output (decisions.md, `decided-project-runtime-2026-11-06`);
  the revision replaced only the version inside each rule ID.
