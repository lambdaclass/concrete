# Where The Bugs Are: Bug Areas, October 2026

Status: Research (dated measurement, 2026-10-26). Normative owner of the method:
[docs/bugs/README.md, Bug areas](../../docs/bugs/README.md#bug-areas). Current numbers: run
`python3 scripts/gen/bug_ledger.py report` or read [docs/bugs/AREAS.md](../../docs/bugs/AREAS.md).

Measured on branch `wip/bug-area-research` at `9c889929` (on main `ea21a2e8`): 270 bug records
(001 – 299 with gaps), 10 open. The area of a bug is **where its fix lives**, derived from the
commits that fixed it, not where the defect showed: a miscompile contained by a new checker
refusal counts as a Check bug.

## How certain the areas are

| basis | bugs | meaning |
|---|---:|---|
| commits | 161 | the bug's own fix commits |
| named | 37 | commits shared with other bugs, split by the files the record names |
| shared | 34 | shared commits the record does not split (ambiguous) |
| record | 10 | the commit that filed or closed the record (old bugs; ambiguous) |
| override | 23 | an `area:` line in the record, each with the record's evidence |
| unfixed | 5 | open, no code fix yet |

44 bugs (16 %) are ambiguous. The 23 overrides cover bugs whose fix commit named another bug,
renumbered bugs (069 and 070 were 064 and 065), commits that only recorded a fix made elsewhere, and
records that spell only a helper's file. Counts by stage below move by a few bugs either way within
that margin; the ranking does not.

## Which stages and modules

| stage | bugs | silent | top modules |
|---|---:|---:|---|
| Check | 72 | 39 | Check 33, CoreCheck 13, CheckHelpers 9, Verify 5 |
| Lower | 34 | 10 | Lower 33 |
| Resolve | 26 | 4 | Resolve 16, FileSummary 6 |
| Elab | 25 | 9 | Elab 24 |
| Proof | 19 | 8 | ProofCore 11 |
| Interp | 18 | 4 | Interp 18 |
| Parse | 11 | 3 | Parser 7 |
| EmitSSA | 10 | 1 | EmitSSA 10 |
| Mono, Std, EmitLLVM, Report, CLI, SSA, Tooling, Gates | 4 – 8 each | | |

- Five files hold nearly half of all bugs (124 of 270): `Check.lean` (33), `Lower.lean` (33),
  `Elab.lean` (24), `Interp.lean` (18) and `Resolve.lean` (16).
- The checker is the largest area by far (27 % of bugs) and the most silent: 39 of its 72 bugs were
  silent wrong code or unsound acceptance. Most of those are fixes that make the checker refuse a
  program the back end miscompiled, so "Check" here largely means "the source rules had no
  enforcing site" (class 13 is its largest class, 11 bugs in `Check.lean`).
- After Check, silent bugs concentrate in Lower (10), Elab (9) and Proof (8; 42 % of Proof's bugs
  are silent: evidence attached to the wrong subject). Resolve, Mono and EmitSSA bugs are mostly
  loud.

## Which classes recur where

The recurring area and class pairs (`report`, last table), largest first:

- **Check × class 13** (documented rules with no enforcing site): 11 in `Check.lean`, 4 in `CoreCheck`,
  3 in `CheckHelpers`, 4 in `Resolve`. The largest single recurrence.
- **Lower × class 8** (storage, places and control flow decided ad hoc in Lower): 10, plus 12 older
  Lower bugs with no class that read as the same shape (aggregate phis, divergent merges, field
  writes through references).
- **Interp × class 7** (semantics implemented several times): 10 — the interpreter disagreeing with
  the binary.
- **Elab × class 1** (declaration identity by spelled name): 9; class 1 spreads over ten stages
  (36 bugs), the widest class.
- **Resolve × class 12** (one fact kept in two places): 8, plus 3 in FileSummary.
- **Check × class 15** (ownership state along control-flow paths): 7, all in Check.
- **Class 17** (superlinear pass state, 43 bugs) is in every stage from Parse to EmitSSA: Check 10,
  Lower 7, Parse 6, Elab 4, Interp 4. It is a property of how passes are written, not of one pass.

## Trends

- Discovery is dominated by October 2026: 199 of 270 records were filed that month (18 in March,
  44 in July). That is the probing campaign (hunts, reviews, spec and semantics passes, the R-0550
  scaling gate), not a rise in defects per change; per-month counts measure probing effort first.
- Silent bugs appear only from October in this table because records before bug 079 mostly have no
  `silent:` line (78 records lack one): silent counts before that are a lower bound.
- How found (from `discovery.tsv`, bugs 079 on): reviews found 37 bugs, 16 of them in Check;
  development work 62; hunts 24, spread over Proof, Check, Elab and Resolve; spec and semantics
  passes found almost only Check bugs (16 of 20).
- Time to fix: most records were filed with their fix (the same commit), so the mean is near zero for
  every stage. Among bugs filed open, the slowest are EmitSSA (mean 36 days, bug 027's quadratic
  rendering), Mono (18) and Elab (12); Resolve, Lower and Interp bugs filed open were fixed within
  a day on average.

## What it suggests

- The structural owners with the most bugs behind them are Check's rule-enforcement sites (class 13),
  Lower's storage and control-flow model (class 8), and the duplicated interpreter semantics
  (class 7). Each is concentrated in one file, so each owner's fix is local.
- Class 17 is not owned by any one stage; a per-pass scaling gate (R-0550) is the right owner shape.
- Open bugs are few (10) and spread out; the backlog is not a stage problem.

## Caveats

- An area is derived from commits by heuristics (subject naming, record-named files, line weights).
  Shared multi-bug commits and old bugs filed with their fix are the weak spots; the report counts
  them as ambiguous.
- Line weights favour large mechanical changes over small precise ones; the record-named files
  correct most of that.
- Stage totals depend on `docs/bugs/area_map.tsv` (for example, `Concrete/Semantics/` maps to
  Check and `Concrete/Report/VCGen.lean` to Proof).
