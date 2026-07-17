#!/usr/bin/env bash
# Float-literal correctly-rounded gate (audit 2026-07-16).
#
# Float literals must lex to the correctly-rounded nearest binary64 value
# (ties to even), matching strtod/Python float() exactly. History: the lexer
# first accumulated `acc + digit * 0.1^k`, compounding rounding error per
# digit (`0.7` was one ulp off); the first fix used Float.ofScientific, which
# keeps only ~11 guard bits after division and still mis-rounds rare literals
# (`16.3633343`, `932183.9385014`). `Concrete.floatOfDecimalMantissa` does the
# conversion with exact big-Nat arithmetic (all sticky bits kept).
#
# The corpus runner checks BOTH the exported conversion function and the
# end-to-end lexer path (tokenize → floatLit) against CPython float() over
# fixed regression cases, subnormal/overflow edges, and 8000 LCG cases.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
[ -x "$ROOT_DIR/.lake/build/bin/concrete" ] || { echo "error: build first" >&2; exit 2; }

python3 "$ROOT_DIR/scripts/tests/float_literal_check.py" || exit 1
