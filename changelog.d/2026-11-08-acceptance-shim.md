### Acceptance lets the root checks push to their own fixtures (ISS-0017) (2026-11-08)

`scripts/land/acceptance.sh` failed every move candidate at its check leg. Its git shim refused any
push that named main, wherever it went, and since ISS-0015 the train's impact leg runs the
repository hook, whose root checks (`landing_bot.sh`, `landing_script.sh`, `review_tool.sh`) push
main to throwaway local fixture repositories. The shim now lives in
`scripts/land/acceptance_shim.sh` and judges the destination by repository identity (its absolute
git common directory, protected remotes resolved when the shim is made). A push that could move
main passes only to an existing local repository that is neither real remote. A missing
repository, a relative, remote or non-repository URL, an insteadOf rewrite and a shell or quoted
alias fail closed, and plain aliases (from config, `-c` or the environment) are expanded and
judged. "Could move main" now also covers `@`, `:`, globs, `*/main` and a push with no refspec.
`landing_script.sh` drives the shim with 35 refused spellings and 14 allowed fixture pushes and
commands. A script that runs the real git by absolute path is outside the shim; acceptance's
pre-push guard and its before/after read of main still stop it.
