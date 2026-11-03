### Fixed: the landing train's review leg could never run

`scripts/land/train.sh check` and `scripts/land/ready.sh` call the reviewer as `review.sh --base <b> --head <h>`,
but `review.sh` had no `--head` option and refused with "unknown argument: --head". Every review leg
therefore failed at once (seen on the queue-reorder train, 2026-11-03). `review.sh` now accepts `--head` and
refuses to run unless it names the checkout's HEAD, because the review always reads HEAD's tree. Its
`--help` prints the whole header instead of a fixed line range.

Structural cause: the landing gates never ran the real `review.sh` the way its callers do
(`check_landing_bot.sh` replaces the reviewer with `true`/`false` through `LAND_REVIEW_CMD`, and
`check_landing_script.sh` does not exercise the review leg). Owner: `check_review_tool.sh`,
which now exercises the callers' `--base --head` form against the real script, including the refusal.

### Fixed: no train could land while the checker-pin campaign job existed

`train.sh watch` reads a skipped landing-tier job as "no verdict". Its campaign-tier list (`CAMPAIGN_RE`) predated
the nightly job "Checker pin freshness (campaign finding)", which is skipped on every ordinary run, so every
train's watch ended "landing tier is NOT green (incomplete)" on a green run (queue-reorder, run 41658068237).
The default now names that job. Structural cause: the list was hand-kept beside the workflow with nothing
comparing them. Owner: `check_landing_script.sh`, which now derives the campaign set from
`lean_action_ci.yml` (an `if:` on `inputs.campaign`, closed under `needs:`) and fails on any difference in
either direction.
