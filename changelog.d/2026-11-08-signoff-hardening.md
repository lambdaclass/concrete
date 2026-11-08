- Owner sign-off verification is hardened against ambiguous ref-selected prefixes, shape-only hook
  admission, parented records, recorder-synthesised review targets and trailing invalid verdicts
  (ISS-0023, ISS-0024, ISS-0025, ISS-0026 and ISS-0027). Requests now carry their own exact
  `reviewed:` binding, prefixes resolve only against objects, and the pre-push notes tier verifies
  sign-off and review-evidence contents.
- Owner item 1 adds writer provenance and reviewer substitution: the branch writer never reviews it,
  the remaining required reviewers must ACCEPT with demonstrated repository access, and legacy
  records without writer keys retain their original verification rule.
- The owner rounds rule adds locally built `refs/review-evidence/<sha>` commits for manually run
  review rounds. Their per-round requests, raw outputs and hashed manifests count toward the cap only
  with complete readable repository evidence; diff-only and failed reviews do not count.
