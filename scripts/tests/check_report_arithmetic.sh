#!/usr/bin/env bash
export CONCRETE_ECHO_RESULT=1  # MAIN_EXIT_MODEL stage 1: legacy echoed-result mode until fixtures migrate (stage 2 deletes this)
# `--report arithmetic` gate — ROADMAP #10 §3.2 / §9.1 (the arithmetic audit
# surface). Every arithmetic site must classify as exactly one of:
#   runtime-checked | proved | explicit-wrapping | explicit-saturating
# and non-arithmetic operators (comparisons, logical, pure bitwise & | ^) must
# NOT be counted. `proved` is currently always 0 (the proof model assumes
# no-overflow rather than discharging overflow obligations) — pinned here so a
# future overflow-proof integration is a deliberate, visible change.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
C="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first ($C missing)" >&2; exit 2; }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# want_total <file> <substring>  — the report's Totals line must contain substring.
totals(){ local n="$1" want="$2"
  local line; line="$("$C" "$TMP/$n.con" --report arithmetic 2>&1 | grep '^Totals:')"
  if grep <<<"$line" -qF "$want"; then ok "$n: Totals has '$want'"
  else no "$n: Totals='$line' missing '$want'"; fi; }

echo "=== every spelling classifies into the right class ==="
cat > "$TMP/all.con" <<'EOF'
fn f(a: u32, b: u32) -> u32 {
    let c1: u32 = a + b;
    let c2: u32 = a - b;
    let c3: u32 = a * b;
    let c4: u32 = a / b;
    let c5: u32 = a % b;
    let c6: u32 = a << b;
    let c7: u32 = a >> b;
    let w1: u32 = wrapping_add(a, b);
    let w2: u32 = wrapping_sub(a, b);
    let w3: u32 = wrapping_mul(a, b);
    let s1: u32 = saturating_add(a, b);
    let s2: u32 = saturating_sub(a, b);
    let s3: u32 = saturating_mul(a, b);
    return c1;
}
fn n(x: i32) -> i32 { return -x; }
fn fc(x: f64) -> i32 { return x as i32; }
pub fn main() -> Int { return 0; }
EOF
# 7 checked binops + unary neg + float-cast = 9 runtime-checked; 3 wrapping; 3 saturating; 0 proved
totals all "9 runtime-checked"
totals all "0 proved"
totals all "3 explicit-wrapping"
totals all "3 explicit-saturating"
totals all "15 arithmetic sites"

echo "=== non-arithmetic operators are NOT counted ==="
cat > "$TMP/none.con" <<'EOF'
fn f(a: u32, b: u32, p: bool, q: bool) -> bool {
    let lt: bool = a < b;
    let eq: bool = a == b;
    let ba: u32 = a & b;
    let bo: u32 = a | b;
    let bx: u32 = a ^ b;
    let an: bool = p && q;
    return lt;
}
pub fn main() -> Int { return 0; }
EOF
totals none "0 arithmetic sites"

echo "=== float + - * are NOT integer-checked sites (IEEE, not classified) ==="
cat > "$TMP/flt.con" <<'EOF'
fn f(a: f64, b: f64) -> f64 { return a + b * a - b; }
pub fn main() -> Int { return 0; }
EOF
totals flt "0 arithmetic sites"

echo "=== wrapping migration shows up: hmac_sha256 has explicit-wrapping sites ==="
hm="$("$C" examples/hmac_sha256/src/main.con --report arithmetic 2>&1 | grep '^Totals:')"
wc="$(grep <<<"$hm" -oE '[0-9]+ explicit-wrapping' | grep -oE '^[0-9]+')"
if [ "${wc:-0}" -ge 20 ]; then ok "hmac_sha256: $wc explicit-wrapping sites (mod-2^32 migration visible)"
else no "hmac_sha256: expected >=20 explicit-wrapping, got '${wc:-0}' ($hm)"; fi

echo ""
echo "REPORT-ARITHMETIC: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
