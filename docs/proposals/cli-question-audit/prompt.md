You are an independent READ-ONLY reviewer. Do not modify, create or delete any file. Do not run builds.

Task: review the draft audit `review-extra/CLI_QUESTION_AUDIT.draft.md` in this directory. It asks whether the NEW Concrete design has missed any question the OLD (legacy) compiler's CLI could answer about a program. The owner's goal, verbatim: "is that we can answer all of this in the future and we plan ahead correctly".

Where things are (this directory is a copy of origin/main):
- Legacy CLI: `legacy/Main.lean` (main dispatch ~3920-4490, `compileAndReport` ~1656-3575, `compileAndCheck` ~3577, `compileAndQuery` ~651, help/usage ~56-90, prove agent help ~3789-3870), `legacy/Concrete/Report/*.lean` (queryFacts in Report.lean ~5924, knownQueryKinds/knownFactKinds ~4871), `legacy/Concrete/Check/Policy.lean`, `legacy/docs/project/POLICY_FILES.md`, `legacy/docs/` CLI-related docs.
- New design: `docs/decisions.md`, `ROADMAP.md`, `docs/proposals/{FOUNDATION,IDEAS,MERGED,SPARK,SPEC_UNIFICATION,EXAMPLES}.md`, `docs/archive/roadmap-2026-11/task_disposition.tsv`, and branch files copied into `review-extra/`: SLICE1_BRIEF.md (wip/slice1-brief), slice0_Concrete_README.md and slice0_BIR.lean (wip/slice0), speccore_Ident.lean (wip/spec-core), LANGUAGE_DECISIONS_2026-11-08.md, STD_*.md (wip/std-placement).

Check, and report only disagreements or additions:
1. Every (b) dropped, (c) MISSING and (d) partial classification: is the class right, is the cited anchor right, is a better anchor available?
2. Any (a) covered classification you believe is wrong.
3. Any legacy question (a `--report` kind, `--query` form, `--check`, subcommand, flag, policy key, prove flag, exit-code contract, anything) the draft missed.
4. The architecture column ("Must record from the start", "Earliest slice", "Prereq") and the "Urgent findings" U1-U11: is each flag real (verify against speccore_Ident.lean, slice0_BIR.lean, slice0_Concrete_README.md, SLICE1_BRIEF.md, FOUNDATION.md C1-C10)? Is any prerequisite that slice 0/slice 1/C1-C10 does NOT provide missing from the list? Is any flag wrong because the design already provides it (cite where)?
5. In the gaps table: is each "owner question vs scheduling" call right?

Format: one bullet per disagreement or addition, at most 40 words each, citing file:line where you can. No praise, no summary of the draft. End with exactly one line:
ANSWER: AGREE | AGREE-WITH-CHANGES | DISAGREE — <one sentence>
