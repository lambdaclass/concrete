### A proposal for server-side protection of main (2026-11-08)

`docs/proposals/MAIN_PROTECTION.md` drafts GitHub rulesets for main on lambdaclass/concrete and a
reversible cutover: activate the replacements, disable the old pull-request ruleset, test a train
landing, then remove the old ruleset. It distinguishes GitHub's `success`/`skipped`/`neutral`
required-check semantics from train.sh's exact-run, success-only and base checks; omits the no-op
`ci/*` Landing record job from the server requirements; and records the limits of the GitHub Actions
app binding. It also covers class-wide deploy-key bypass, the narrower GitHub App alternative,
credential and admin trust, read-only audits, and rollback. Nothing is applied: the owner applies or
approves the settings.
