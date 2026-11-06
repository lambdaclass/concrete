#!/usr/bin/env bash
# promote.sh <signature> [--repo <checkout>] [--write]
# Prepare a repo-ready reproducer for one farm finding: a fixture under
# tests/regressions/fuzz_<class>_<hash>/repro.con (the minimized program with a header that
# states the expected and observed behaviour, the seed and the compiler SHA), plus the
# scripts/gen/new_bug.sh command that files it. Prints by default; --write (with --repo)
# writes the fixture into that checkout. It never commits: an agent or human does that.
set -uo pipefail
exec python3 "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/farm.py" promote "$@"
