### Bug 283: gate assertions can fail on SIGPIPE (filed, open)

`printf '%s' "$OUT" | grep -q PAT` under `pipefail` can invert a match: grep exits on its first match,
the producer dies of SIGPIPE, and the pipeline takes its status, so a positive leg reports FAIL for a
present pattern and a negated leg reports ok for forbidden text that is there (silent). Seen
in CI on `check_transform_register.sh`; the shape occurs in about a hundred gate scripts. The planned
fix matches with here-strings and widens the hygiene scan from elaborator producers to the shape.
