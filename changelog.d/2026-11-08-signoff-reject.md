### Owner sign-off accepts completed REJECT reviews (2026-11-08)

`signoff.sh verify` now accepts an owner's exact-SHA sign-off at the review cap when every reviewer
completed a review, even when a last verdict is REJECT, and reports every verdict. Missing reviewers
and verdict-less outputs remain refusals; landing controls cover both behaviors (ISS-0020).
