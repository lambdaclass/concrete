#!/usr/bin/env bash
# Type-alias gate (ROADMAP Phase 6 #3).
#
# Type aliases are TRANSPARENT aliases (not nominal newtypes): `type Id = i32`
# names the same type i32, interchangeable everywhere, with no new layout/proof
# identity. This gate locks that behavior:
#
#   1. TRANSPARENT — aliases work over arrays, structs, generic instantiations,
#      and in function signatures; an alias value and the underlying value are
#      mutually substitutable; chains (B=A=i32) and nested aliases ([E;N]) fully
#      expand.
#   2. NO NEW IDENTITY — a program written with an alias and the same program
#      written with the underlying type produce byte-identical `--report layout`
#      and `--report fingerprints`. (So aliasing cannot shift proof/evidence
#      identity — the property #3 requires.)
#   3. BAD FORMS REJECTED — an alias to an unknown type is rejected at the
#      declaration (E0108); a recursive alias, direct or mutual, is rejected with
#      a dedicated diagnostic (E0112) rather than a confusing downstream error.
#
# Fixtures: tests/programs/type_alias/. See docs/TYPE_ALIASES.md.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
source "$ROOT_DIR/scripts/tests/lib/selfprint.sh"
cd "$ROOT_DIR"
C="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first ($C missing)" >&2; exit 2; }
D="tests/programs/type_alias"
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

run_expect(){ local n="$1" e="$2"
  gate_selfprint_wrap "$D/$n.con" "$TMP/$n.w.con"
  if ! "$C" "$TMP/$n.w.con" -o "$TMP/$n.bin" >"$TMP/$n.err" 2>&1; then
    no "$n: expected to compile"; sed 's/^/        /' "$TMP/$n.err" | head -3; return; fi
  local g; g="$("$TMP/$n.bin" 2>/dev/null)"
  [ "$g" = "$e" ] && ok "$n -> $g" || no "$n: got '$g' want '$e'"; }

reject_with(){ local n="$1" c="$2"
  local out; out="$("$C" "$D/$n.con" -o "$TMP/$n.bin" 2>&1)"
  if grep <<<"$out" -qE 'error\['; then
    grep <<<"$out" -q "($c)" && ok "$n rejected with $c" \
      || { no "$n rejected, wrong code"; grep <<<"$out" -oE '\([A-Z0-9]+\)' | head -1 | sed 's/^/        got: /'; }
  else no "$n: expected rejection ($c), compiled"; fi; }

echo "=== 1. transparency: arrays / structs / fns / chains / nested ==="
run_expect alias_array_struct_fn 7
run_expect alias_transparency 12
run_expect alias_chain_and_nested 16

echo "=== 2. no new identity: alias vs underlying produce identical layout + fingerprints ==="
cat > "$TMP/alias.con" <<'EOF'
mod m { type Id = i32; struct Copy S { a: Id, b: i32 }
  fn f(x: Id) -> Id { return x + 1; }
  fn main() -> Int { let s: S = S { a: 1, b: 2 }; return (s.a + f(s.b)) as Int; } }
EOF
cat > "$TMP/raw.con" <<'EOF'
mod m { struct Copy S { a: i32, b: i32 }
  fn f(x: i32) -> i32 { return x + 1; }
  fn main() -> Int { let s: S = S { a: 1, b: 2 }; return (s.a + f(s.b)) as Int; } }
EOF
"$C" "$TMP/alias.con" --report layout 2>/dev/null > "$TMP/la.txt"
"$C" "$TMP/raw.con"   --report layout 2>/dev/null > "$TMP/lr.txt"
{ [ -s "$TMP/la.txt" ] && cmp -s "$TMP/la.txt" "$TMP/lr.txt"; } \
  && ok "layout identical (alias creates no new layout identity)" \
  || no "layout differs between alias and underlying"
"$C" "$TMP/alias.con" --report fingerprints 2>/dev/null > "$TMP/fa.txt"
"$C" "$TMP/raw.con"   --report fingerprints 2>/dev/null > "$TMP/fr.txt"
{ [ -s "$TMP/fa.txt" ] && cmp -s "$TMP/fa.txt" "$TMP/fr.txt"; } \
  && ok "fingerprints identical (alias creates no new proof identity)" \
  || no "fingerprints differ between alias and underlying"

echo "=== 3. bad forms rejected ==="
reject_with neg_unknown_target E0108
reject_with neg_recursive_alias E0112
reject_with neg_mutually_recursive_alias E0112

echo ""
echo "TYPE-ALIAS: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
