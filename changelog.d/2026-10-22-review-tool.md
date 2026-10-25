### One Review Procedure With Delta Rounds (R-0544, 2026-10-22)

Reviews had taken 8 to 34 rounds per branch, each re-reading the whole growing diff, and the
reviewer refused whole rounds on large ownership and memory diffs. `scripts/review/review.sh` is
now the one procedure: round 1 reviews the full diff from the merge-base, later rounds only the
delta since the previous round; large diffs are split into pieces by file group; a refused piece is
retried once; a round cap (default 5) stops the loop; and the verdict is recorded as a git note
(`refs/notes/concrete-review`) on the reviewed commit, which `scripts/review/verify.sh` and the
landing tools read. [REVIEW.md](docs/project/REVIEW.md) describes it, and `check_review_tool.sh`
(CI) tests it.
