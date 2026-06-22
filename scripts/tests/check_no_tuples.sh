#!/usr/bin/env bash
# No-tuples decision gate (ROADMAP Phase 6 #5; docs/TUPLES.md).
#
# Concrete V1 deliberately has no anonymous tuples — named structs are the one
# product type. This gate pins that the tuple SYNTAX stays cleanly rejected at
# parse time (a clear "use a struct" error, never silently half-accepted), and
# that the named-struct replacement compiles. It also guards that `.0` extraction
# on a NEWTYPE still works (that is a named single-field wrapper, not a tuple).

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
C="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first ($C missing)" >&2; exit 2; }

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

# rejected <label> <source>
rejected(){ printf '%s' "$2" > "$TMP/t.con"
  if "$C" "$TMP/t.con" -o "$TMP/t.bin" >"$TMP/t.out" 2>&1; then
    no "$1: tuple syntax was ACCEPTED (should be rejected)"
  else
    ok "$1: rejected ($(grep -oE '\([A-Z0-9]+\)' "$TMP/t.out" | head -1))"
  fi; }
# accepted <label> <source>
accepted(){ printf '%s' "$2" > "$TMP/t.con"
  if "$C" "$TMP/t.con" -o "$TMP/t.bin" >"$TMP/t.out" 2>&1; then ok "$1: compiles"
  else no "$1: expected to compile"; sed 's/^/        /' "$TMP/t.out" | head -3; fi; }

echo "=== tuple syntax stays rejected (use a struct) ==="
rejected "tuple type annotation" 'mod m { fn f() -> i32 { let t: (i32, i32) = (1, 2); return 0; } }'
rejected "tuple literal"         'mod m { fn f() -> i32 { let t = (1, 2); return 0; } }'
rejected "tuple return type"     'mod m { fn f() -> (i32, i32) { return (1, 2); } }'

echo "=== the named-struct replacement compiles ==="
accepted "named result struct" 'mod m { struct Copy DivMod { q: i32, r: i32 } fn dm(a: i32, b: i32) -> DivMod { return DivMod { q: a / b, r: a % b }; } fn main() -> Int { let d: DivMod = dm(7, 3); return (d.q * 10 + d.r) as Int; } }'

echo "=== newtype .0 extraction (a named wrapper, not a tuple) still works ==="
accepted "newtype .0" 'mod m { newtype Tag = i32; fn main() -> Int { let t: Tag = Tag(5); return t.0 as Int; } }'

echo ""
echo "NO-TUPLES: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
