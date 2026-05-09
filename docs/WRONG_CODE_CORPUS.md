# Wrong-code regression corpus

Status: contract. Operative file: `tests/wrong-code/manifest.toml`.

This corpus is the named, durable home for every compiler bug that
ever silently shipped wrong behavior — miscompiles, codegen
divergences, verifier-trigger crashers, fact / report mismatches, and
proof-evidence regressions. Roadmap reference: Phase D items D.16
(named regression corpus) and D.14 (reducer/minimizer workflow).

The corpus is a **registry**, not a relocation. Existing repros under
`tests/programs/bug_*.con` and `tests/programs/adversarial/<area>/`
stay where they are; the corpus references them by path so existing
test-runner wiring keeps working. New cases that have no other home
land under `tests/wrong-code/cases/<id>/`.

## What qualifies

A case belongs in the corpus when **at some point the compiler agreed
with a program in a way that was wrong**. The categories are:

| Category | Meaning |
|---|---|
| `miscompile` | Source program compiles cleanly, the compiled binary produces wrong output (different from `--interp`, different from the language spec, or different across optimization levels). |
| `codegen-divergence` | Output of compiled binary differs between `lli`, native, debug vs release, etc., for a program that should be deterministic. |
| `verifier-trigger` | A valid program triggers an SSA-verify / CoreCheck / lint failure that should not have fired. |
| `fact-report-mismatch` | `--report alloc` / `caps` / `layout` / `proof` / etc. contradicts the actual compiler state, or two reports contradict each other. |
| `proof-evidence-regression` | A previously-valid proof artifact, attachment, or evidence claim becomes invalid without a corresponding source change. |
| `crash` | Compiler crashes or panics on a valid input, or fails to produce a clean diagnostic on an invalid input. |
| `error-regression` | Compiler accepts a program that should be rejected, or rejects with the wrong error code / message. |

Things that are *not* wrong-code regressions:

- Feature requests, missing language constructs, performance bugs.
- Programs the compiler correctly rejects.
- Style / doc nits, unused imports, formatter output drift.
- Tests that exercise correct behavior (those belong in `tests/programs/`).

## Per-case format

Each case gets a stable identifier `WC-NNNN` and an entry in the
manifest. The manifest is the operative file; the per-case notes file
is a prose companion.

### Manifest entry (`tests/wrong-code/manifest.toml`)

```toml
[[case]]
id          = "WC-0003"
category    = "miscompile"
status      = "fixed"           # fixed | open
repro       = "tests/programs/bug_field_assign_narrow_field.con"
kind        = "runtime"         # runtime | compile-error | crash | report
expected    = "100"             # for runtime: the trimmed stdout the binary must produce
discovered  = "66d8736"         # commit (or commit-range / branch) that surfaced it
fixed       = "66d8736"         # commit that fixed it; absent if status = open
notes       = "tests/wrong-code/cases/WC-0003.md"
```

Field semantics:

- `repro`: path to the smallest input that triggers the bug. Existing
  `tests/programs/bug_*.con` and `tests/programs/adversarial/<area>/`
  files are first-class; the manifest just *registers* them.
- `kind`:
  - `runtime` — compile + run native binary, compare trimmed stdout
    to `expected`.
  - `compile-error` — `expected` is the error code (e.g. `E0703`); the
    compiler must reject with that code.
  - `crash` — compile/run is expected to fail with a non-zero exit. A
    `crash_signature` field can pin a substring that must appear in
    stderr.
  - `report` — `expected_report` (path) must match `concrete <repro>
    --report <kind>` output.

### Per-case notes (`tests/wrong-code/cases/WC-NNNN.md`)

Free-form, but the conventional skeleton is:

```markdown
# WC-NNNN — <one-line title>

**Category:** <category>
**Status:** <fixed|open>
**Discovered:** <commit-or-context>
**Fixed:** <commit-or-empty>

## Symptom
What went wrong, observably. The minimal user-visible bad behavior.

## Root cause
What the compiler was actually doing wrong, by which pass / file / fn.

## Fix
Where the fix lives in the tree, and the one-sentence "why this works"
explanation.

## Minimization
What was reduced from. If the repro is already minimal, say so.

## How to verify it stays fixed
Either: "the manifest entry runs in `make test-wrong-code`," or a
specific manual reproduction recipe.
```

## Minimization rule

Every repro must be **the smallest input that still triggers the
bug**. If a case is larger than minimal, the notes file must explain
why (typically: a feature interaction that requires several constructs
together, or a layout-sensitive case that needs a specific struct
shape).

This is not "shortest file" cargo-culting. The point is that a future
reader can read the repro end-to-end in one sitting and know exactly
which language features the bug needs to express itself. A 30-line
repro that mixes five unrelated features is bigger than a 50-line
repro that uses only the relevant ones.

When a case lands as `open`, expand the notes file with a "what
reduction was attempted" paragraph so a follow-up minimization pass is
possible without re-deriving the work.

## Artifact bundle

Each case directory under `tests/wrong-code/cases/<id>/` is the
artifact bundle. Required:

- The notes file (`WC-NNNN.md` directly under `cases/`).

Optional, when relevant:

- `dump/` — IR / SSA / report dumps captured at discovery time. Use
  when the bug is in a pass and the dump is what made it visible.
- `original.con` — the un-reduced program, when reduction was
  non-trivial.
- `flags.txt` — non-default compiler flags required to reproduce.

Repros listed in the manifest may live elsewhere in the tree; the
artifact bundle is what travels with the *case*, not the repro file
itself.

## CI entry point

```sh
make test-wrong-code              # runs every fixed case; fails on regression
bash scripts/tests/test_wrong_code.sh --include-open  # also runs open cases (expected to fail loudly)
```

The script:

1. Reads `tests/wrong-code/manifest.toml`.
2. For each `status = "fixed"` entry, runs the case according to its
   `kind` and asserts `expected` matches.
3. If `--include-open` is passed, also runs `status = "open"` entries
   and reports their pass/fail without affecting the run's exit code,
   so a fix is detected automatically the next time CI runs.
4. Verifies internal manifest invariants: every `repro` path exists,
   every `notes` path exists, every fixed case has a `fixed` commit.

A case that flips from `open` to `fixed` is a routine manifest update,
not a separate workflow.

## Lifecycle

- A new bug triggers a manifest entry **the day it's found**, even if
  the fix lands the same hour. The entry exists so the regression
  cannot silently come back.
- Cases are never deleted. A case that the compiler stops being able
  to compile (because of a deliberate language change) is documented
  in the notes file with `status = "fixed"` plus a `superseded`
  paragraph, but stays in the manifest as historical evidence.
- The corpus grows monotonically. If it shrinks, that's a signal to
  audit.

## See also

- `tests/wrong-code/README.md` — operations quickstart
- `tests/wrong-code/manifest.toml` — the registry
- `tests/oracle/README.md` — the differential-harness sibling for
  positive value-return semantics; mismatches surfaced there land
  here when triaged
- ROADMAP Phase D.14 (reducer/minimizer workflow) and D.16 (named
  regression corpus)
