### Acceptance lets the root checks push to their own fixtures (ISS-0017) (2026-11-08)

`scripts/land/acceptance.sh` failed every move candidate at its check leg. Its git shim refused any
push that named main, wherever it went, and since ISS-0015 the train's impact leg runs the
repository hook, whose root checks (`landing_bot.sh`, `landing_script.sh`, `review_tool.sh`) push
main to throwaway local fixture repositories. The shim now lives in
`scripts/land/acceptance_shim.sh` and judges the destination: a push that could move main is
refused on either real remote however it is spelled (a remote name, the URL, `file://`, `--repo`,
a second push URL), and also when the repository is missing, its URL is not an existing local path,
or an insteadOf rewrite is in effect; a push to an existing local fixture goes through. "Could move
main" now also covers `@`, `:`, globs, `*/main` and a push with no refspec. `landing_script.sh`
drives the shim with every refused spelling and the allowed fixture pushes.
