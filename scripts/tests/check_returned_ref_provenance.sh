#!/usr/bin/env bash
# Returned-reference provenance gate (H1), danger-staged (ROADMAP Phase 6 #8a).
#
# Mutable half (the use-after-realloc WRITE vector) is WITHDRAWN: no safe public
# API returns `Option<&mut V>` / a `&mut` into storage. The gate enforces that
# no such definition reappears, and that `update(k, fn(V) -> V)` replaced it.
#
# Immutable half (`get -> Option<&V>`, `peek`/`min`/`max`/`min_key`/`max_key`)
# is CONTAINED: still an unsound read-stale shape, but kept until V1.1 scoped
# callbacks (`with_value`) replace it. The gate freezes that surface against a
# baseline (no NEW immutable aggregate-ref API) and reproduces the immutable
# hole so it is not forgotten.

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

echo "=== mutable half withdrawn: no safe public API returns Option<&mut> ==="
if grep -rn "pub fn.*-> *Option<&mut" std/src/*.con >/dev/null 2>&1; then
  no "a public API still returns Option<&mut ...> — mutable aggregate-ref hole reopened:"
  grep -rn "pub fn.*-> *Option<&mut" std/src/*.con | sed 's/^/       /'
else
  ok "no public Option<&mut ...> accessor in std (get_mut withdrawn)"
fi
if grep -q "pub fn update(&mut self, key: &K, f: fn(V) -> V) -> bool" std/src/map.con; then
  ok "HashMap::update(k, fn(V) -> V) replacement present"
else
  no "HashMap::update replacement missing"
fi

echo "=== immutable half still reproduces (contained, not yet fixed) ==="
for h in "$MAP_HOLE" "$VEC_HOLE"; do
  if (cd "$h" && "$COMPILER" build >/dev/null 2>&1); then
    ok "$(basename "$h") builds (immutable Option<&T> read hole — KNOWN, contained)"
  else
    no "$(basename "$h") no longer builds — if the immutable accessor was withdrawn, update this gate"
  fi
  rm -f "$h/main" "$h/$(basename "$h")"
done

echo "=== immutable aggregate-ref surface frozen against baseline ==="
CURRENT="$(mktemp)"
python3 - <<'PY' > "$CURRENT"
from pathlib import Path
import re
for path in sorted(Path("std/src").glob("*.con")):
    struct=None; aliases=set()
    for raw in path.read_text().splitlines():
        line=raw.strip()
        m=re.match(r"pub\s+struct\s+(?:Copy\s+)?([A-Za-z_][A-Za-z0-9_]*)", line)
        if m: struct=m.group(1)
        ma=re.match(r"pub\s+type\s+([A-Za-z_][A-Za-z0-9_]*)\s*=\s*(.*)", line)
        if ma and "&" in ma.group(2) and re.search(r"\b(Option|Result)\s*<", ma.group(2)):
            aliases.add(ma.group(1))
        mf=re.match(r"pub\s+fn\s+([A-Za-z_][A-Za-z0-9_]*)\s*\(.*", line)
        if not mf or "->" not in line: continue
        ret=line.split("->",1)[1]
        agg=("&" in ret and re.search(r"\b(Option|Result)\s*<",ret)) or any(re.search(r"\b"+re.escape(a)+r"\b",ret) for a in aliases)
        if agg: print(f"{path}:{struct or '<module>'}.{mf.group(1)}: {line}")
PY
if diff -u scripts/tests/returned_ref_aggregate_baseline.txt "$CURRENT" >/dev/null; then
  ok "no new immutable aggregate-ref API beyond the (post-withdrawal) baseline"
else
  no "immutable aggregate-ref surface changed — update baseline only with a recorded reason:"
  diff -u scripts/tests/returned_ref_aggregate_baseline.txt "$CURRENT" | sed 's/^/       /'
fi
rm -f "$CURRENT"

echo "=== bare scalar -> &T is NOT the banned shape (positive) ==="
if printf '%s\n' 'pub fn scalar_ref(x: &i32) -> &i32 {' | grep -Eq -- '->.*\b(Option|Result)\s*<.*&'; then
  no "scanner would reject a bare scalar returned ref"
else
  ok "bare scalar -> &T is not flagged"
fi

echo ""
echo "RETURNED-REF-PROVENANCE: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
