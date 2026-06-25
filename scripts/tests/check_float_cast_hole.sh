#!/usr/bin/env bash
# Known-hole gate: float -> int cast overflow is UNCHECKED (docs/KNOWN_HOLES.md H2).
#
# `f as iN` / `f as uN` lowers to a raw LLVM `fptosi`/`fptoui`. When the float is
# outside the integer type's range the result is LLVM poison — the compiled
# binary silently produces garbage instead of trapping. This is the one
# remaining "semantically dark" arithmetic construct after the ROADMAP #10
# checked-integer flip; it survives because (a) it is a separate subsystem from
# integer `+ - * / % << >>`, and (b) the interpreter does not support float
# literals yet, so there is no interp==compiled oracle to verify a fix against.
#
# This gate is a DISCLOSURE PIN, not a pass/fail of correctness: it asserts the
# CURRENT (held, buggy) behavior so the hole cannot silently change shape. When
# float->int casts are made checked (or saturating), this gate WILL fail —
# that is the signal to update KNOWN_HOLES.md H2, move it to CLOSED, and replace
# the assertions below with a real interp==compiled trap/agreement check.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
C="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first ($C missing)" >&2; exit 2; }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

echo "=== in-range float->int cast is correct (not part of the hole) ==="
cat > "$TMP/ok.con" <<'EOF'
fn main() -> i64 { let f: f64 = 100.5; let x: i32 = f as i32; return x as i64; }
EOF
"$C" "$TMP/ok.con" -o "$TMP/ok.bin" >/dev/null 2>&1 && [ "$("$TMP/ok.bin")" = "100" ] \
  && ok "100.5 as i32 == 100 (truncates toward zero)" \
  || no "in-range float->int cast wrong"

echo "=== HELD HOLE: out-of-range float->int does NOT trap (raw fptosi) ==="
cat > "$TMP/ovf.con" <<'EOF'
fn main() -> i64 { let f: f64 = 9999999999.0; let x: i32 = f as i32; return x as i64; }
EOF
if "$C" "$TMP/ovf.con" -o "$TMP/ovf.bin" >/dev/null 2>&1; then
  out="$("$TMP/ovf.bin" 2>/dev/null)"; rc=$?
  # Hole present: the binary runs to success (rc 0) with an out-of-range/garbage
  # value, instead of aborting. If this ever traps, the hole is fixed → update H2.
  if [ "$rc" -eq 0 ] && [ "$out" != "" ] && [ "$out" != "2147483647" ]; then
    ok "out-of-range cast still UNCHECKED (got '$out', no trap) — H2 hole intact"
  else
    no "behavior CHANGED (out='$out' rc=$rc) — float->int cast may be fixed; update docs/KNOWN_HOLES.md H2 and replace this gate with a real trap/agreement check"
  fi
else
  no "ovf fixture failed to compile"
fi

# Pin the mechanism: the lowering is still a raw fptosi with no range guard.
if "$C" "$TMP/ovf.con" --emit-llvm 2>/dev/null | grep -qE "fptosi|fptoui"; then
  ok "lowering still emits raw fptosi/fptoui (no range-check/abort guard)"
else
  no "no fptosi/fptoui in lowering — cast path changed; revisit H2"
fi

echo ""
echo "FLOAT-CAST-HOLE: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
