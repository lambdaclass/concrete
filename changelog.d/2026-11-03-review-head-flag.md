### Fixed: the landing train's review leg could never run

`scripts/land/train.sh check` and `scripts/land/ready.sh` call the reviewer as `review.sh --base <b> --head <h>`,
but `review.sh` had no `--head` option and refused with "unknown argument: --head". Every review leg
therefore failed at once (seen on the queue-reorder train, 2026-11-03). `review.sh` now accepts `--head` and
refuses to run unless it names the checkout's HEAD, because the review always reads HEAD's tree. Its
`--help` prints the whole header instead of a fixed line range.

Structural cause: the landing gate (`check_landing_script.sh`) replaces the reviewer with a stub through
`LAND_REVIEW_CMD`, so nothing ever ran the real `review.sh` the way its callers do. Owner: `check_review_tool.sh`,
which now exercises the callers' `--base --head` form against the real script, including the refusal.
