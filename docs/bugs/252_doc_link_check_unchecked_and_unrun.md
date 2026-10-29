# Bug 252 — the Markdown-link check ignored anchors, misresolved changelog fragments, and nothing ran it

<!-- bug-ledger
summary: the Markdown-link check dropped every `#anchor`, so links to renamed or moved headings passed; it resolved `changelog.d/` fragment links against `changelog.d/` although fragments fold into the root CHANGELOG.md, so three correct links read as broken; and only the Makefile ran it, so neither CI nor the pre-push selection did. Anchors are checked against GitHub heading slugs and explicit ids, fragments resolve against the root, the check is a workflow step that every diff selects, and gate registration enumerates Python gates. Pinned by the link gate's throwaway-tree controls and the selection controls. Class 11.
coverage: the Markdown-link check dropped anchors, resolved changelog fragments against their own directory, and was run only by the Makefile. Documentation tooling, no compiler program to reproduce it: the regression vehicle is the link gate's throwaway-tree controls and the selection replay controls over the selector entry point select_gates.sh (2026-10-25)
class: 11 — a link check that skipped every anchor and that no workflow step ran passed as coverage
silent: no
ruling: Violates: unrecorded
-->

**Status:** FIXED (branch `wip/doc-links-gate`)
**Found:** 2026-10-25, by the orchestrator's review of documentation links on main `032d8e12`: three
links in `research/meta/bend-roc-lessons.md` named roadmap headings that had been renamed or moved to
`docs/project/ROADMAP_BACKLOG.md` (repaired on `wip/bendroc-links`, `a4c4d8eb`), and the link check,
run by hand, reported three correct changelog fragment links as broken.
**Severity:** documentation only. A link to a renamed or moved heading lands at the top of the
target file instead of the section it cites, and nothing said so; correct links in pending
changelog fragments read as broken, so the check's one visible result was wrong.

Confirmed real by Codex review, 2026-10-25: Fragment resolution contradicted changelog.d/README.md and CI omitted the gate; anchor checking extends the old checker’s explicitly path-only scope.

## Reproducer

On main `032d8e12`, run `python3 scripts/tests/check_doc_links.py`. Observed: `FAIL=3`, all three in
`changelog.d/` (`docs/project/AGENTS.md`, `research/meta/bend-roc-lessons.md`,
`ROADMAP.md#open-research-questions-bend-and-roc`), each a correct root-relative link; and the three
stale anchors in `research/meta/bend-roc-lessons.md` (`ROADMAP.md#phase-9-proof-authoring-and-automation`,
`#task-r-0404`, `#task-r-0399`) pass. Then `grep -rn check_doc_links .github scripts .githooks`:
nothing; only the Makefile names it. Expected: the stale anchors fail, the fragment links pass, and CI
and the pre-push selection run the check.

## Cause

`scripts/tests/check_doc_links.py`:

- `target.split("#", 1)[0]` kept the path and dropped the anchor, so `file.md#anything` passed when
  `file.md` existed, and a same-file `#anchor` was skipped as if external;
- every target resolved against `path.parent`, but `scripts/gen/changelog.py --fold` moves a
  fragment's text verbatim into the root `CHANGELOG.md`, so fragment links are written against the
  root (the folded file's place) and resolved against `changelog.d/` they are wrong;
- it was reached only by two Makefile targets: no workflow step, so neither CI nor the pre-push hook
  (which runs what the impact selector picks from the workflow's commands) ever ran it, and the gate
  registration rule did not see it because it enumerated shell gates only. `ROADMAP_HISTORY`
  records it as running "on every docs-drift run", true only of `make test-docs-drift`.

## Fix

The checker is `scripts/tests/lib/doc_links.py` (with a `--root` seam) under a shell gate,
`scripts/tests/check_doc_links.sh`, which is a workflow step in the build-free `grammar` job:

- an anchor into a Markdown file must be one of its GitHub anchors: a heading's slug as
  github-slugger computes it (rendered text, lowercased, punctuation removed, spaces to hyphens,
  `-1`, `-2` for repeated headings; headings inside code fences are not headings), or an explicit
  `<a id>`/`name`, which `docs/spec` uses; same-file anchors are checked too. Anchors into other
  files (`Foo.lean#L10`) and directories are not checked;
- a link in a `changelog.d/` fragment, and a link with a leading `/`, resolve against the root
  (`changelog.d/README.md` states the convention);
- an anchor into CHANGELOG.md, and a fragment's own `#anchor`, is checked against CHANGELOG.md with
  the pending fragments folded in by `scripts/gen/changelog.py`'s own fold;
- fenced code, code spans (also across lines) and HTML comments hold no links, headings or ids
  (GitHub renders none of them), and both links of a nested badge `[![badge](img)](target)` are
  read; the old pattern read neither.

The gate registration rule enumerates `scripts/tests/check_*.py` as well as shell gates, so a
Python gate no workflow step reaches is reported unreachable (checked by adding an unreferenced
`check_*.py`: FAIL). The impact selector has a list of gates any non-empty diff selects (`EVERY_CHANGE` in
`scripts/tests/lib/gates_for_diff.py`): any path can be a link target and any heading an anchor, so
a docs-only edit, a renamed heading and a deleted file can each break the gate. Selecting it maps no
file, so a path no rule maps still selects every gate.

Regression legs: the gate's own controls on throwaway trees (a missing heading, a duplicate suffix
past the last repeat, a heading inside a fence or an HTML comment, an id inside a code span, a
missing image inside a link, a missing file, a fragment link written relative to `changelog.d/`, a
fragment anchor fail; slugs with punctuation, code, emphasis and a literal underscore, a duplicate
suffix, an explicit id, case, same-file, root-relative and sibling-fragment links pass) and two selection controls (a docs-only
and a research-note diff select the link gate). Repaired links: none beyond `a4c4d8eb`'s three; with
anchors checked the tree has no other broken link (2604 links, 523 anchors).

## Structural cause

Class 11 (absence read as a fact): a check that never runs looks exactly like one that passes, and a
check that silently skips part of its input (the anchor) reports the skipped part as checked. Both
halves of this bug were invisible for that reason; the registration rule exists for the first half
and enumerated only shell gates. Bugs 075 and 106 are the same shape for policy and report gates.

## Structural owner

R-0513 (exits 2d and 2e, gates): every gate is a workflow command, which the registration rule and
the impact selector both read; a whole-tree input is declared to the selector (`EVERY_CHANGE`,
`ALWAYS_IF`) rather than left to the path a gate happens to name.
