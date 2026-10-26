### Bug 283: gate assertions can fail on SIGPIPE (filed, open)

`printf '%s' "$OUT" | grep -q PAT` under `pipefail` can report a FAIL for a pattern that is present:
grep exits on its first match, the producer dies of SIGPIPE, and the pipeline takes its status. Seen
in CI on `check_transform_register.sh`; the shape occurs in about a hundred gate scripts. The planned
fix matches with here-strings and widens the hygiene scan from elaborator producers to the shape.
