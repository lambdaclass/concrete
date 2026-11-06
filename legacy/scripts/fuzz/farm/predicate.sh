#!/usr/bin/env bash
# External predicate for `concrete reduce` (external:<this> <meta.json>): exit 0 iff the
# candidate (appended last by the reducer) still shows the finding's class and shape.
set -uo pipefail
exec python3 "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/farm.py" predicate "$@"
