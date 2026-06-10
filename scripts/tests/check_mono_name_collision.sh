#!/usr/bin/env bash
# Monomorphization name-collision known-hole gate.
#
# Mono mangles a specialization by the HEAD constructor of the type argument,
# discarding nested type arguments. So `tag<Hold<Pair<i64>>>` and
# `tag<Hold<Pair<bool>>>` both mangle to one `tag_for_Pair` over one
# `%Hold_Pair` LLVM type — but those two types have different layouts (inner
# Pair<i64> is 16 bytes, Pair<bool> is 2 bytes). One function body + one struct
# type for two distinct-layout types is a silent miscompile (ABI corruption the
# moment a field is touched or the value is passed by-value).
#
# This gate tracks the hole without pretending it is fixed:
#   1. Reproduces it: the colliding program still builds.
#   2. Proves the collision: two distinct inner layouts (%Pair_Int and
#      %Pair_Bool) exist, yet exactly ONE `define @tag_for_Pair` is emitted.
# When mangling keys on the full type, two distinct specializations will be
# emitted; flip the merged-define assertion to expect 2 at that point.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

F="examples/known_holes/mono_name_collision/src/main.con"

echo "=== known hole reproduces: colliding generic program still builds ==="
if "$COMPILER" "$F" >/dev/null 2>&1; then
  ok "Hold<Pair<i64>> + Hold<Pair<bool>> program builds (KNOWN HOLE)"
else
  no "colliding program no longer builds — flip this gate to expected-reject"
fi
rm -f examples/known_holes/mono_name_collision/src/main

SSA="$("$COMPILER" "$F" --emit-ssa 2>&1)"

echo "=== two distinct inner layouts exist ==="
if printf '%s' "$SSA" | grep -q "%Pair_Int" && printf '%s' "$SSA" | grep -q "%Pair_Bool"; then
  ok "both %Pair_Int (16B) and %Pair_Bool (2B) inner types are present"
else
  no "expected both Pair_Int and Pair_Bool inner types in the SSA"
fi

echo "=== the collision: one merged function body for two distinct types ==="
DEFS="$(printf '%s' "$SSA" | grep -cE "^define i64 @tag_for_")"
if [ "$DEFS" = "1" ]; then
  ok "exactly one @tag_for_Pair emitted for two distinct Hold<Pair<…>> types (the bug)"
elif [ "$DEFS" -ge 2 ]; then
  no "two+ tag specializations emitted — collision appears FIXED; flip this gate"
else
  no "no tag specialization emitted — detection regressed (got $DEFS)"
fi

echo ""
echo "MONO-NAME-COLLISION: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
