# Changelog fragments

Status: reference

To log a change, add one file here; never edit CHANGELOG.md by hand. Each branch adds its own
file, so two branches never edit the same lines and a rebase cannot conflict here.

- Name: `YYYY-MM-DD-<slug>.md`, the date and a lowercase slug (the branch or topic), for example
  `2026-10-22-bug175-match-merge.md`.
- First line: the entry's `### ` heading, exactly as it will appear in CHANGELOG.md. A
  `### Completed Task R-NNNN` heading retires a roadmap task, as it does in CHANGELOG.md.
- Body: ordinary Markdown, with no `#` or `##` headings. One fragment may hold several entries.

`python3 scripts/gen/changelog.py --fold` moves the pending fragments into CHANGELOG.md, newest
first, and deletes them. It runs on main after a landing. Landings are serialized, so a fold never
conflicts. Until then, `--print` shows the changelog with the pending fragments folded in, and the
roadmap gates read Completed Task headings from that view.

`check_docs_drift.sh` fails on a malformed fragment, and on a hand edit of CHANGELOG.md: the file
carries a stamp, and only `--fold` and `--restamp` write it. Use `--restamp` only to accept a
deliberate correction to an old entry.
