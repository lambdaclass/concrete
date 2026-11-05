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
confirmation: the owner confirms the moment before the unpublish step runs; nothing runs it automatically
status: pending

After the unpublish step, check that both URLs answer 404, then set `status: done YYYY-MM-DD`. The
old site's sources stay archived under `legacy/site/` with their `RETENTION.tsv` row.
