# Brief B: one test format (manifest-driven tests) — branch test-manifest

You are working in the full clone /home/unbalancedparen/projects/gates-test-manifest, branch
`test-manifest`, based on origin/wip/slice0 (ea5181b5; slice 0 is not on main yet). Read
docs/project/AGENTS.md, CLAUDE.md and Concrete/README.md (sections "Layout", "Gates", "Reproducible
builds") first. Do NOT push, do NOT touch git remotes, do NOT run `git rebase/reset/clean/stash`.
Commit your work on the branch (small commits, clear messages). Do not run heavy jobs: no full
`lake build` of the whole tree, no native gate runs; at most `lake build Concrete` once if you need
to typecheck Lean you changed, bounded: `timeout 1800`. Every command you run must be bounded with
`timeout`. Write no files outside this clone except under /tmp via mktemp.

## Goal (owner-approved)

"One test format: each test is one program with its expected outcome (output, exit code, or
diagnostic rule ID) declared in a single manifest; one runner; one owner per test. Migrate the new
compiler's existing slice-0 tests into it."

Today the only compiler tests are the hand-written BIR examples in `Concrete/BIR.lean`,
`namespace Examples`, `def all : List (String × Program × Outcome)`, driven by
`scripts/checks/compiler/slice0_bir.lean` (emitter: `list`, `emit NAME DIR TARGET`, `loop`,
`invalid`) and enumerated by three gates: `slice0_backends.sh` (via `emit_examples` in
`scripts/checks/lib/slice0_common.sh`), `slice0_artifacts.sh`, and `slice0_repro.sh` (its own
`slice0_bir.lean list > names`). The expected outcome lives in Lean (`Examples.all`).

## What to build

1. `tests/manifest.tsv` — THE one manifest. A `#` comment header that states the format, then a
   tab-separated header row `id	program	expect	slice`, then one row per test:
   - `id`: unique, `[a-z0-9_]+`.
   - `program`: `bir:<example name>` (a program of `Concrete.BIR.Examples`) for today's tests;
     the format also accepts `con:<path under tests/>` (a source file; none exist yet: the front
     end is not built) — document it, validate it, but add no `.con` file.
   - `expect`: exactly one of `exit <0..255>`, `trap <kind> site <n>` (kind is the Lean
     `ConcreteSpec.Trap` constructor name the site table carries, e.g. `arithmeticOverflow`; n the
     site index), `refuse <RULE-ID>` (for source tests once the checker exists; validated
     syntactically now). Nothing else.
   - `slice`: the slice that owns the test (`slice0` for all current rows).
   Migrate every entry of `Examples.all` into a row (same name as id).
2. The expectation has ONE home, the manifest. Change `Examples.all` to `List (String × Program)`
   (programs only) and move every expected outcome into the manifest. The emitter's `emit` takes
   the expectation from the manifest (e.g. `emit NAME DIR TARGET EXPECT...`), parses it, and keeps
   every current check: the BIR run must equal it, LCB's evaluator must return the same word, and
   for a trap the site table entry's kind must equal the manifest's kind. If anything else in
   Concrete/ or ConcreteSpec/ reads `Examples.all`'s outcomes (search), keep those proofs working
   without restating the outcomes in a second list; if that is impossible, stop and say why in your
   final message.
3. One shared reader: `scripts/checks/lib/test_manifest.py` (python3 -I, stdlib only) that parses
   and validates the manifest (header, column count, unique ids, the three expect forms, slice
   syntax, `con:` path exists and lies under tests/, no `con:` path listed twice) and prints rows
   on stdout for shell consumers (e.g. `rows [--kind bir]` prints `id<TAB>program<TAB>expect`).
   Fail closed with a message naming the line on any defect.
4. One runner: rename `scripts/checks/compiler/slice0_backends.sh` to
   `scripts/checks/compiler/tests.sh` (git mv; keep all its controls and its judge controls). It
   enumerates the manifest, not the emitter. It also checks that the set of `bir:` names in the
   manifest equals the emitter's `list` exactly (both directions; a program with no row, or a row
   with no program, fails). Rename the CI job accordingly ("Tests (x86-64)", "Tests
   (aarch64-macos)") in .github/workflows/lean_action_ci.yml, update required-checks.txt, and add
   rows to required-checks-transition.tsv for the old job names, following that file's header and
   scripts/land/required_checks.py.
5. `slice0_artifacts.sh` and `slice0_repro.sh` enumerate tests through the same reader (the repro
   gate keeps comparing its two sides' inventories). Remove `emit_examples` if nothing else uses it.
6. A root check `scripts/checks/test_manifest.sh` (cheap: no build, no Lean; run.sh runs every
   `scripts/checks/*.sh`): runs the reader's validation, and requires every file under `tests/`
   except `tests/manifest.tsv` and `tests/golden/` to be a `con:` program of exactly one row (and,
   when one exists, a `.con` file not listed fails). Add it to run.sh's REQUIRED list. Look at the
   existing root checks for conventions (bounded commands, gate_env.sh, mktemp scratch, no fixed
   /tmp paths, `check_gate_hygiene`-style rules, the root inventory scripts/checks/root_inventory.txt).
7. Planted-failure controls — each must fail FOR THE PLANTED REASON (grep the expected message,
   not only a non-zero exit), the way slice0_repro.sh's `control` does:
   - root check, on a private copy of the manifest/tests tree in mktemp: a duplicate id; an
     unlisted `tests/slice0/x.con`; a row whose `con:` file is missing; a malformed expect
     (`exit 256`, `trap foo`); a wrong column count.
   - runner (tests.sh): a manifest copy with one wrong expectation (e.g. `answer` -> `exit 41`)
     must fail at the emitter with the expectation mismatch; a manifest copy missing a row must
     fail the inventory check; a row naming a nonexistent program must fail. Let the runner take
     the manifest path from an environment variable (default tests/manifest.tsv) so controls can
     point it at a copy; the controls themselves run inside tests.sh, cheaply (emit only, no
     native build, where possible).
8. Docs: update Concrete/README.md wherever it says tests are `*.con` + `*.expect` files or names
   slice0_backends.sh (the layout table, gate 4, the gates list), to the manifest. Keep the
   README's style (plain, short sentences). Add a changelog.d fragment (see changelog.d/README.md).
   Update scripts/checks/root_inventory.txt only if the root check requires it.

## Constraints

- Follow the house style of the existing scripts (comments at the top stating what the gate checks
  and its controls; `set -euo pipefail`; bounded_run for every timed command in compiler gates).
- No language or spec changes; do not edit ConcreteSpec/ beyond what (2) strictly requires.
- Run `bash scripts/checks/test_manifest.sh` and `bash -n` / `shellcheck` (if present) on every
  script you touch; run `python3 -I scripts/checks/lib/test_manifest.py` self-checks. Do not run
  the native gates; I run them on the CI server.
- Final message: list of commits, what you ran and its result, and any open question.
