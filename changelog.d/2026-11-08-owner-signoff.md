### The train accepts an owner sign-off at a member's exact SHA (2026-11-08)

When a branch reaches its review-round cap, the owner may approve one exact commit after Codex, Kimi
and agy have all reviewed it (R-0618). `scripts/land/signoff.sh record` stores the approval as
`refs/owner-signoff/<sha>`, with `record.txt` and a separate hashed request and output for each
reviewer. A structured `reviewed:` line plus a commit name in each request body binds the review to
the signed SHA, and verdicts come only from the output, closing F1 (binding by any transcript
mention) and F2 (acceptance supplied by the request). `train.sh`'s review provenance accepts, for a
member with no clean review note, a sign-off that `signoff.sh verify` finds valid for that exact SHA
(the member used the review cap of its diff's class: docs 2 rounds, scripts 3, compiler and spec 5),
and then requires the train head to be exactly the clean merge of its members onto its base. A
sign-off for another commit, a changed verdict file, the review of another commit, cap evidence that
is not a well-formed review.sh note, a missing reviewer, a reviewer output ending in REJECT, a quote
that names no commit, a scripts or mixed docs-and-compiler member after 2 rounds, and a head beyond
the clean merge are each refused (`scripts/checks/landing_script.sh`). The pre-push hook takes a
sign-off push in its no-source-tree tier only when the object is a seven-file record.
