#!/usr/bin/env bash
# Returned-reference provenance known-hole gate.
#
# This gate tracks the current unsound public API shape without pretending it is
# fixed. It does two things:
#   1. Reproduces the known hole: aggregate-wrapped returned refs compile today.
#   2. Freezes the public stdlib aggregate-ref return surface against a baseline,
#      so no new `Option<&T>` / `Option<&mut T>` / alias-equivalent public safe
#      API lands before scalar `from(param)` returned references are designed.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

MAP_HOLE="examples/known_holes/returned_ref_provenance_map"
VEC_HOLE="examples/known_holes/returned_ref_provenance_vec"

echo "=== known hole reproduces: aggregate-wrapped returned refs compile today ==="
if (cd "$MAP_HOLE" && "$COMPILER" build >/dev/null 2>&1); then
  ok "HashMap::get_mut saved through Option<&mut V> still compiles (KNOWN HOLE)"
else
  no "known HashMap returned-ref hole no longer compiles; update this gate to expected-reject"
fi
rm -f "$MAP_HOLE/main" "$MAP_HOLE/returned_ref_provenance_map"

if (cd "$VEC_HOLE" && "$COMPILER" build >/dev/null 2>&1); then
  ok "Vec::get saved through Option<&T> still compiles (KNOWN HOLE)"
else
  no "known aggregate returned-ref hole no longer compiles; update this gate to expected-reject"
fi
rm -f "$VEC_HOLE/main" "$VEC_HOLE/returned_ref_provenance_vec"

echo "=== public stdlib aggregate-ref return surface is frozen ==="
CURRENT="$(mktemp)"
python3 - <<'PY' > "$CURRENT"
from pathlib import Path
import re

struct = None
aliases = set()
items = []

for path in sorted(Path("std/src").glob("*.con")):
    struct = None
    for raw in path.read_text().splitlines():
        line = raw.strip()
        m = re.match(r"pub\s+struct\s+(?:Copy\s+)?([A-Za-z_][A-Za-z0-9_]*)", line)
        if m:
            struct = m.group(1)
        ma = re.match(r"pub\s+type\s+([A-Za-z_][A-Za-z0-9_]*)\s*=\s*(.*)", line)
        if ma and "&" in ma.group(2) and re.search(r"\b(Option|Result)\s*<", ma.group(2)):
            aliases.add(ma.group(1))
        mf = re.match(r"pub\s+fn\s+([A-Za-z_][A-Za-z0-9_]*)\s*\(.*", line)
        if not mf or "->" not in line:
            continue
        ret = line.split("->", 1)[1]
        aggregate_ref = ("&" in ret and re.search(r"\b(Option|Result)\s*<", ret)) or any(re.search(r"\b" + re.escape(a) + r"\b", ret) for a in aliases)
        if aggregate_ref:
            owner = struct or "<module>"
            items.append(f"{path}:{owner}.{mf.group(1)}: {line}")

for item in sorted(items):
    print(item)
PY

if diff -u scripts/tests/returned_ref_aggregate_baseline.txt "$CURRENT" >/dev/null; then
  ok "no new public stdlib APIs returning aggregate-wrapped refs beyond baseline"
else
  no "public stdlib aggregate-ref return surface changed before from(param) design:"
  diff -u scripts/tests/returned_ref_aggregate_baseline.txt "$CURRENT" | sed 's/^/       /'
fi
rm -f "$CURRENT"

echo "=== scalar returned refs are not part of the aggregate-ref freeze ==="
if printf '%s\n' 'pub fn scalar_ref(x: &i32) -> &i32 {' | grep -Eq -- '->.*\b(Option|Result)\s*<.*&'; then
  no "scanner would reject a bare scalar returned ref"
else
  ok "bare scalar -> &T is not rejected by this aggregate-ref freeze"
fi

echo ""
echo "RETURNED-REF-PROVENANCE: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
