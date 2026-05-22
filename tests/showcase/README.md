# Phase 7 showcase

The curated set of graduated flagships. Each entry has met every bar
in its own AUDIT.md and is enforced by drift-detecting CI gates.

This directory is the **registry**, not the implementation. The
implementations live under `examples/<name>/`; the manifest
references them.

## Files

- `manifest.toml` — the registry. One entry per graduated flagship.
- `README.md` — this file.

## Adding a flagship

1. The example must show **10 of 10 bars met** in its
   `<example>/AUDIT.md`. The bars are documented in
   `examples/parse_validate/AUDIT.md` (the template that
   established them).
2. The example must carry, at minimum:
   - One Lean-backed theorem in `Concrete/Proof.lean`, registered
     in `src/proof-registry.json`.
   - An `assumptions.toml` declaring the trust surface (enforced
     by `make test-assumptions`).
   - A `[policy]` section in `Concrete.toml` (enforced by
     `make test-policy`).
   - A `catches/` directory with at least one negative case
     (enforced by `make test-catches`).
   - A `snapshot/` baseline (enforced by `make test-snapshots`).
   - An `oracle/` differential test against an independent
     reference (or a stronger Lean equivalence proof).
   - An honest `README.md` and `AUDIT.md`.
   - A capturable release evidence bundle
     (`scripts/tests/capture_release_bundle.sh`).
3. Add a `[[flagship]]` entry to `manifest.toml` with all 10 bars
   marked `true`.

## Removing a flagship

Removing is allowed only with a roadmap note explaining why
(language churn, scope change, replacement by a stronger example).
In that case the entry moves to an `[[archived]]` section, not
deleted — the audit trail of "this was once a flagship" is part of
the project's honesty discipline.

## CI

`make test-showcase` walks every entry, runs its declared gates,
and asserts the release bundle still captures cleanly. Drift in
any gate fails the run.

The showcase is the strictest standing CI surface: it composes
every other drift-enforced gate the project has built into one
walk.

## Current entries

| Example | Graduated | Thesis role |
|---|---|---|
| parse_validate | 2026-05-22 | First pull-through pilot to graduate. Capability-pure validation core with one Lean composition theorem and a drift-enforced negative pair. |

## See also

- `examples/parse_validate/AUDIT.md` — the canonical 10-bar audit.
- `ROADMAP.md` Phase 7 — the showcase contract.
- `docs/RELEASE_BUNDLE.md` — release evidence bundles per flagship.
