# Owner questions: second three-model review (2026-11-08)

One independent, read-only round by Codex, Kimi and agy (Gemini) on three revised Claude
recommendations (R3, R14, R18). The reviewers had seen the earlier versions (L2, CLI Q3, foundation
Q1). This page decides nothing; the owner decides.

- Context: origin/main at f65b91217; `docs/proposals/OWNER_QUESTIONS_REVIEW.md` and
  `owner-questions-review/` from origin/wip/owner-questions-review (11d9e6ee2);
  `docs/proposals/CLI_OWNER_QUESTIONS.md` and `CLI_QUESTION_AUDIT.md` from origin/wip/cli-question-audit
  (a0e436767); the owner record `LANGUAGE_DECISIONS_2026-11-08.md` (items 1-59); `PENDING_RECORDS.md`
  (the foundation Q1 queue entry).
- Codex (`codex exec -s danger-full-access`) and Kimi (`kimi -p`, no --yolo/--auto) read the files in
  a git-archive copy; both copies were compared with the original afterwards and are identical. agy
  answered from the same question text with the context inlined (full `PHILOSOPHY.md`, the 2026-11-08
  record, `docs/decisions.md` excerpts incl. lines 100-320, the slice-0 surface row, the refusal list,
  OD8/OD42/OD116/OD118/OD137/OD99, the foundation Q1 entry, the earlier review page and the CLI
  questions).
- Raw outputs: `owner-questions-review-2/` (`prompt.txt`, `prompt-agy.txt`, `codex.out`, `kimi.out`,
  `agy.out`). Kimi's output includes its reasoning trace; Codex's includes its tool trace, with the
  answer at the end. All three exited 0 within the 1800 s bound.

## Per reviewer

- R3 Codex: AGREE (changes its earlier pick); breaks none if recorded as item 16's migration to u8 keeping OD137's ASCII rule; notes a Rule 2 tension the owner should approve expressly; wants exact lexical rules (malformed quotes refused).
- R3 Kimi: AGREE; breaks none (items 16, 33, 49 hold); wants negative controls for `''`, `'ab'`, unterminated, non-ASCII, escaped quote.
- R3 agy: AGREE; breaks none, says dropping `'a'` would instead contradict items 16 and 33; wants a stated rule that any `'` forming neither a literal nor a label is one malformed token.
- R14 Codex: AGREE; `help` now extends the decided slice-0 surface (`build`, `facts`), so it needs an explicit owner revision, and each later command its own authorization.
- R14 Kimi: AGREE; same: the slice-0 surface row is revised by adding `help`; generate `help`/`trace`/`emit` from the command/stage registries and IR printers.
- R14 agy: AGREE; breaks none once approved as an extension of the slice-0 surface per P1/P23; notes `run` would mix compiler and program exit codes.
- R18 Codex: DISAGREE as written; "the same operation set as runtime code" would admit user calls, loops and effects, against Q6.6, item 26's closed total initializer subset and P17b; better: admit fallible arithmetic within the closed initializer subset, reusing runtime operation definitions.
- R18 Kimi: AGREE only under a scope: one shared primitive-operation semantics inside item 26's closed total grammar (no user calls, loops, allocation, capabilities or effects); otherwise breaks item 26, Q6.6 and possibly P17b.
- R18 agy: AGREE provided "ordinary operations" means primitive operations and Copy constructors over acyclic constants (no user calls or loops); better: state that call-free, loop-free, acyclic restriction in the rule text.

R18 evidence question ("the kernel-checked evaluation is the evidence"), all three: sound
conditionally. The kernel must check that the specified evaluator returns success (Codex, Kimi:
not merely that an evaluator ran); Q6.4/TYPE-22 holds because every initializer, used or not, is
evaluated and a failure refuses it; Q6.6 holds only if the initializer fragment makes an
evaluator-resource outcome impossible (acyclic, call-free, loop-free; out-of-fuel is neither success
nor proven failure); P17b holds only if constant evaluation stays inside that closed fragment.
Codex and Kimi also note the explicit TYPE-22 timing revision (each use to compile time, item 26)
is still owed.

## Agreement

| Item | Codex | Kimi | agy | Agreement |
|---|---|---|---|---|
| R3 `'a'` ASCII u8 literal | agree | agree | agree | unanimous agree |
| R14 help now; emit/trace/interp later; no run | agree | agree | agree | unanimous agree; Codex and Kimi: adding `help` revises the slice-0 surface row, needs owner revision |
| R18 fallible ops in const initializers | disagree (as written) | agree (scoped) | agree (scoped) | 2-1 agree; all three in substance: fallible primitive ops yes, but only inside a closed, call-free, loop-free, effect-free initializer grammar, not "the same as runtime code" |

Raw ANSWER lines:
- Codex: `ANSWER: R3=agree; R14=agree; R18=disagree`
- Kimi: `ANSWER: R3=agree; R14=agree; R18=agree`
- agy: `ANSWER: R3=agree; R14=agree; R18=agree`
