# Scripts

Repository scripts live here instead of the project root.

- [`tests/`](./tests/README.md) contains test runners and test-maintenance helpers.
- `check_ll1.*` contains grammar-check helpers for the parser.

Rule of thumb:

- put reusable developer scripts in `scripts/`
- put test entrypoints in `scripts/tests/`
- keep the repo root for project entrypoints and major top-level directories only
