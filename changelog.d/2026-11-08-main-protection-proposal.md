### A proposal for server-side protection of main (2026-11-08)

`docs/proposals/MAIN_PROTECTION.md` drafts the GitHub rulesets that would make the server the
boundary for main on lambdaclass/concrete: the required checks on the exact commit and no rewrite
for everyone, and updates only from the landing clone's deploy key. It records the settings read on
2026-11-08 (an admin bypass lets any admin push main) and the exact `gh api` commands. Nothing is
applied: the owner applies or approves them.
