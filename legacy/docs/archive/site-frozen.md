# The frozen site deployment (ROADMAP R-0565)

Status: record; incomplete until every value below is real.

The legacy/ move stops `.github/workflows/book.yml`, so the site stays at its last deployment until
the new site builds from the new tree. That deployment must be the frozen compiler's, labelled as
such, with its browser bundle identified and checked. `scripts/land/legacy_move.sh` (prerequisite
P4) reads the `key: value` lines below and refuses the move while any value is a placeholder.

tag: legacy-compiler-final
commit: 066c9d9b8836a2b176aa37f6c8313195a4b4ee29
bundle_sha256: pending
deployment: pending
label: pending
browser_parity: pending
loader_ui: pending
redeploy: pending

What is known now (2026-11-04), not yet the record above:

- The live deployment, <https://unbalancedparentheses.github.io/concrete2/>, was built by the `site`
  workflow from main at `b78fff1a5`, not from the tag. Between the tag and `b78fff1a5`, `git diff`
  shows no change under `Concrete/`, `Main.lean`, `Browser.lean`, `Concrete.lean`, `std/`,
  `scripts/browser/`, `site/`, `lakefile.toml` or `lean-toolchain`, so its compiler sources are the
  tag's; its bundle has not been hashed and the browser checks have not been run against it.
- Filling the record needs: a browser bundle built from the tag and its sha256; a parity run of the
  bundle against the native oracle (`scripts/oracle/run.sh`) and the loader and UI checks, each
  passing; the deployment labelled as the frozen compiler for the language before the 2026-10-29 to
  2026-11-01 decisions; and a written redeploy procedure. Deploying is outward-facing and waits for
  the owner (ISSUES.tsv ISS-0004).
