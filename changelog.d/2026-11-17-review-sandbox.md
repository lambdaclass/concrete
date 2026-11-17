ISS-0086 / R-0708: Run Codex reviews in disposable, verified archives of the
reviewed commit with owner-authorized danger-full-access. Require successful
repository-read command evidence before accepting verdicts; execution failures
record no note. Preserve each exact prompt, raw event stream and final output.
Keep GIT_OPTIONAL_LOCKS=0 as defense in depth; no caller index is exposed.
Only review.sh invokes Codex for reviews under scripts/; it uses invoke.sh.
