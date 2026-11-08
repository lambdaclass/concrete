### Acceptance lets the root checks push to their own fixtures (ISS-0017) (2026-11-08)

`scripts/land/acceptance.sh` failed every move candidate at its check leg. Its git shim refused any
push that named main, wherever it went, and since ISS-0015 the train's impact leg runs the
repository hook, whose root checks (`landing_bot.sh`, `landing_script.sh`, `review_tool.sh`) push
main to throwaway local fixture repositories. The shim now lives in
`scripts/land/acceptance_shim.sh` and judges the destination by repository identity (its absolute
git common directory). Protected remotes are resolved when the shim is made, in every local
spelling git accepts (a path, `file://`, `file://localhost`, `~`, relative to the clone top), and a
protected local URL that names no repository stops the shim from being made. A push that could
move main passes only to an existing local repository that is neither real remote. Git's global
options and push's options are parsed from fixed lists, spelled in full; an unknown, abbreviated
or bundled option is refused. A missing repository, a relative, remote or non-repository URL, an
insteadOf rewrite, submodule recursion and a shell or quoted alias fail closed, and plain aliases
(from config, `-c`, `--config-env` or the environment) are expanded and judged. "Could move main"
now also covers `--branches`, `--prune`, `@`, `:`, globs, `*/main` and a push with no refspec.
`landing_script.sh` drives the shim with 45 refused invocations, three more protected URL
spellings, two refused shim creations and 18 allowed fixture pushes and commands. A script that
runs the real git by absolute path is outside the shim; acceptance's pre-push guard and its
before/after read of main still stop it.
