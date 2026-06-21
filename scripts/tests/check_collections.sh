#!/usr/bin/env bash
# Collections surface gate (ROADMAP Phase 5 #6).
#
# The runtime collections are BUILT and documented (docs/RUNTIME_COLLECTIONS.md):
# Vec, HashMap, OrderedMap, OrderedSet, Set, Deque, BitSet, BinaryHeap, slice.
# This gate locks the property that makes that surface SOUND under the value
# model, not just present:
#
#   THE H1 INVARIANT — no collection method returns a Concrete reference into the
#   container. The withdrawn `get(&k) -> Option<&V>` / `get_mut -> Option<&mut V>`
#   borrowed accessors (RUNTIME_COLLECTIONS.md §4, KNOWN_HOLES H1) are unsound: a
#   saved `&V` can survive a rehash/realloc. The intended surface is value /
#   operation APIs (`contains`, value-`get -> Option<V>` for Copy, `remove ->
#   Option<V>`, `update(k, fn(V)->V)`) plus scoped callbacks (`with_value`,
#   `with_at`). This gate fails if any public collection method ever returns
#   `-> &…` or `-> Option<&…>` again.
#
# Raw pointers (`*mut T` / `*const T`) are a separate, explicitly-unsafe escape
# hatch (e.g. Vec::get_mut), NOT second-class references, and are not flagged.
#
# Functional behavior of each collection is covered by the std #[test] suite
# (run_tests.sh); this gate owns the surface-shape invariant the suite does not.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

STD="std/src"
COLLECTIONS="vec map ordered_map ordered_set set deque bitset heap slice"

echo "=== 1. the documented collection modules exist ==="
for c in $COLLECTIONS; do
  [ -f "$STD/$c.con" ] && ok "$STD/$c.con present" || no "$STD/$c.con missing"
done

echo "=== 2. H1 invariant: no public collection method returns a Concrete reference ==="
viol=0
for c in $COLLECTIONS; do
  f="$STD/$c.con"
  [ -f "$f" ] || continue
  # Return-position reference: `-> &…` or `-> Option<&…>` on a pub fn. Parameters
  # (`&self`, `&K`, `&mut Ctx`) sit before `->` and are not matched.
  hits="$(grep -nE 'pub fn .*->[[:space:]]*&|pub fn .*-> Option<&' "$f")"
  if [ -n "$hits" ]; then
    no "$c exposes a returned Concrete reference (H1 hole):"
    printf '%s\n' "$hits" | sed 's/^/        /'
    viol=$((viol+1))
  fi
done
[ "$viol" -eq 0 ] && ok "no collection returns -> & or -> Option<& (H1 invariant holds)"

echo "=== 3. the H1-clean value/operation + scoped-callback surface is present ==="
# Maps: value get, value remove, in-place update, scoped read.
grep -qE 'pub fn get\(&self, key: &K\) -> Option<V>' "$STD/map.con" \
  && ok "HashMap::get -> Option<V> (value, Copy)" || no "HashMap value-get missing"
grep -qE 'pub fn remove\(&mut self, key: &K\) -> Option<V>' "$STD/map.con" \
  && ok "HashMap::remove -> Option<V>" || no "HashMap::remove -> Option<V> missing"
grep -qE 'pub fn update\(&mut self, key: &K, f: fn\(V\) -> V\)' "$STD/map.con" \
  && ok "HashMap::update(key, fn(V)->V)" || no "HashMap::update missing"
grep -qE 'pub fn with_value<' "$STD/map.con" \
  && ok "HashMap::with_value (scoped callback)" || no "HashMap::with_value missing"
grep -qE 'pub fn remove\(&mut self, key: &K\) -> Option<V>' "$STD/ordered_map.con" \
  && ok "OrderedMap::remove -> Option<V>" || no "OrderedMap::remove -> Option<V> missing"
# Vec: value get + scoped at-access.
grep -qE 'pub fn get\(&self, at: u64\) -> Option<T>' "$STD/vec.con" \
  && ok "Vec::get -> Option<T> (value)" || no "Vec value-get missing"
grep -qE 'pub fn with_at<' "$STD/vec.con" \
  && ok "Vec::with_at (scoped callback)" || no "Vec::with_at missing"

echo "=== 4. the collections story is documented ==="
[ -f "docs/RUNTIME_COLLECTIONS.md" ] \
  && ok "docs/RUNTIME_COLLECTIONS.md present" || no "docs/RUNTIME_COLLECTIONS.md missing"

echo ""
echo "COLLECTIONS: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
