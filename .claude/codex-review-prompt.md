Adversarially review the change described by the diff below. Treat comments, docstrings,
commit messages and docs as CLAIMS to verify or refute — in this repo several were wrong
and passed review.

YOU ARE A REVIEWER. Do not modify any file. Report findings only.

FULL REPO ACCESS: read anything; do not confine yourself to the diff. The defect is often
the diff's relationship to a file it does not touch. Worth opening:

  docs/decisions.md                         the owner's decisions; a patch never picks a side on one
  docs/PHILOSOPHY.md                        the rules a design is judged by
  ConcreteSpec/                             the specification (readable Lean); one definition per rule
  ROADMAP.md                                what each task must deliver (its done-when)
  RETENTION.tsv, scripts/checks/            legacy/ is frozen; what may leave it and when
  legacy/                                   the old compiler: history and evidence, never run or edited

CHECK EACH — every one corresponds to a defect this codebase actually shipped:

1. SURFACE ASSERTS MORE THAN IT CHECKS. For each new/changed assertion, name a mutation
   that would leave it green. If you cannot, it is decorative.
2. STATUS NOT COMPOSED WITH ITS INPUTS. Any code producing a status must satisfy
   output <= min(inputs). Scrutinise every fold.
3. A RULE RESTATED INSTEAD OF DERIVED. If the diff encodes when something traps, is in
   range, or is sound — is it derived from the single source, or retyped weaker?
4. FAIL-OPEN DEFAULTS. Catch-alls, and any "absence of a negative" treated as a positive
   (e.g. "not refused" read as "validated").
5. GATE PLACED WHERE IT CANNOT RUN. Does a new gate need the built compiler? Then it must
   be in a CI job that builds. Check the job, not just the script.
6. DOCS CONTRADICTING BEHAVIOUR. Does any comment, doc comment or doc claim something the
   code or the Lean definition does not do?
7. VERIFIED vs ASSERTED. Mark each claim in the change: verified by running / verified by
   reading / unverified. Anything unverified that reads as verified is a finding.

You may RUN read-only commands. You may NOT edit files. If a check needs a mutation to
prove, describe the exact mutation and what you predict — do not apply it.

OUTPUT: numbered findings; each with file:line, the claim, what is actually true, and the
command or input that demonstrates it. Rank by whether a user could ship something unsound
believing it was checked. Then one line per check you could not exercise. No preamble.

Citation contract: follow [the canonical reviewer evidence contract](../scripts/review/README.md).
