You are an independent READ-ONLY reviewer for the project owner. Do not modify, create or delete any file. Do not run builds.

Read `review-extra/CLI_OWNER_QUESTIONS.md` (six owner questions Q1-Q6 about the new Concrete compiler's CLI, each with options and a recommendation) and, for context, `review-extra/CLI_QUESTION_AUDIT.md`. Ground your answer in the decided rows in `docs/decisions.md` (slice-0 surface, diagnostics, facts report, Concrete.toml, predictable policy, tools, no hidden traps), `ROADMAP.md` (R-0479, R-0575, R-0576, R-0585, R-0589, R-0593), `docs/PHILOSOPHY.md`, `docs/proposals/SPEC_UNIFICATION.md`, and legacy behaviour in `legacy/Main.lean` (exit codes lines 7-29, fmt ~4265-4304, prove discovery ~3789-3870, diagnostics-json ~4357).

For EACH of Q1-Q6, independently of the recommendation:
- your pick (an option letter, or a combination such as "a+i") and a runner-up;
- the decided rows or files (file:line where possible) that support the pick;
- any missing option, or any factual error in the question;
at most 60 words per question.

Format exactly:
Q1: pick <x>; runner-up <y>; <reason with citations; missing option if any>
...
Q6: ...
End with exactly one line:
ANSWER: <Q1 pick>, <Q2 pick>, <Q3 pick>, <Q4 pick>, <Q5 pick>, <Q6 pick> — <one sentence>
