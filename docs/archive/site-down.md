# Website takedown record (R-0565, decision row P9)

The owner decided on 2026-11-05 that the website goes down at the move and is rebuilt from scratch
with the new compiler (R-0569); no frozen playground is served in between
([decision](../spec/decisions.md#decided-project-site)). The move stops the site build. Unpublishing
the live sites is a separate manual step, because it is visible outside the repository: it runs only
after the owner confirms the moment, and this record is then updated to `status: done` with the date.

decision: P9 (owner, 2026-11-05)
build: book.yml stops with the move (archived under legacy/.github/workflows/)
deployment: https://unbalancedparentheses.github.io/concrete2/ and https://lambdaclass.github.io/concrete/ (GitHub Pages, workflow builds)
unpublish: gh api -X DELETE repos/unbalancedparentheses/concrete2/pages; gh api -X DELETE repos/lambdaclass/concrete/pages
confirmation: the owner confirmed the moment on 2026-11-05 (P9: taken down with no frozen browser bundle)
unbalancedparentheses: unpublished 2026-11-05 (Pages DELETE; the URL answers 404); the site workflow is disabled
lambdaclass: the Pages DELETE was refused 2026-11-05 (HTTP 422, organisation policy); the site workflow is disabled, but the URL still serves the last deploy
remaining: a lambdaclass organisation admin unpublishes lambdaclass/concrete in Settings > Pages (ISS-0004)
status: partial

After the unpublish step, check that both URLs answer 404, then set `status: done YYYY-MM-DD`. The
old site's sources stay archived under `legacy/site/` with their `RETENTION.tsv` row.
