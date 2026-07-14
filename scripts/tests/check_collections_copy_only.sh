#!/usr/bin/env bash
# KNOWN_HOLES H18 gate: collections do not destroy non-Copy elements — pinned.
#
# Pins the DISCLOSED status quo until the explicit-destruction design lands
# (drop_with/clear_with/remove_with family): (a) the leak is real and
# demonstrable (a non-Copy element enters a Vec; vec.drop() compiles without
# consuming it — the linear checker cannot see through the container); (b) std
# does not quietly grow a hidden per-element Drop path while the hole is open
# (the fix must arrive as the visible *_with API family, flipping this gate
# DELIBERATELY).

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
C=".lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first" >&2; exit 2; }
TMP=$(mktemp -d); trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# (a) the demonstrating fixture: non-Copy element swallowed by Vec.drop
cat > "$TMP/h18.con" <<'CON'
mod m {
    struct Owner { fd: i64 }
    fn main() with(Alloc) -> Int {
        let mut v: Vec<Owner> = vec_new::<Owner>();
        vec_push::<Owner>(&mut v, Owner { fd: 3 });
        vec_free::<Owner>(v);   // H18: the live Owner inside is LEAKED, not destroyed
        return 0;
    }
}
CON
if "$C" "$TMP/h18.con" -o "$TMP/h18.bin" >/dev/null 2>&1; then
  ok "H18 demonstrated: Vec<non-Copy>.drop() compiles and leaks the element (disclosed)"
else
  no "H18 fixture no longer compiles — if explicit destruction landed, flip this gate deliberately"
fi

# (b) no hidden per-element Drop sneaks into container drops
if grep -nE "element.*drop\(\)|value.*\.drop\(\).*loop" std/src/vec.con >/dev/null 2>&1; then
  no "vec.drop appears to have grown per-element destruction — land it as the *_with family + update H18"
else
  ok "no hidden per-element Drop in Vec (the fix must be the visible *_with family)"
fi
grep -q "drop_with\|clear_with\|remove_with" std/src/vec.con \
  && no "*_with family present — H18 may be closable; update KNOWN_HOLES + this gate" \
  || ok "*_with destruction family not yet shipped (H18 open as disclosed)"

echo
echo "COLLECTIONS-COPY-ONLY (H18): PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
