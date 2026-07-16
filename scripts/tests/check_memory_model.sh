#!/usr/bin/env bash
# Memory-model gate — ROADMAP Phase 6 #33 (docs/MEMORY_MODEL.md).
#
# Backs the headline invariant the user-facing memory model claims: safe
# Concrete has NO UNINITIALIZED READS BY CONSTRUCTION. This is a grammar-level
# guarantee — a `let` binding cannot be declared without a value — so the gate
# pins that an initializer-less declaration is rejected, while ordinary
# declare-with-value and mutate-after-init keep working. (The full ownership /
# borrow / move rules are covered by check_collections / trust-gate / the
# match+borrow red-team; this gate is specifically the definite-initialization
# invariant the doc promises.)

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
source "$ROOT_DIR/scripts/tests/lib/selfprint.sh"
cd "$ROOT_DIR"
C="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first ($C missing)" >&2; exit 2; }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
write(){ printf '%s\n' "$2" > "$TMP/$1.con"; }

# reject: must fail to compile (parse/check error).
reject(){ local n="$1"
  if "$C" "$TMP/$n.con" -o "$TMP/$n.bin" >"$TMP/$n.e" 2>&1; then
    no "$n: expected rejection, compiled"; else ok "$n: rejected (no uninitialized binding)"; fi; }
# accept_run: must compile, run, and print $want.
accept_run(){ local n="$1" want="$2"
  gate_selfprint_wrap "$TMP/$n.con" "$TMP/$n.w.con"; mv "$TMP/$n.w.con" "$TMP/$n.con"
  if ! "$C" "$TMP/$n.con" -o "$TMP/$n.bin" >"$TMP/$n.e" 2>&1; then
    no "$n: expected to compile"; sed 's/^/        /' "$TMP/$n.e" | head -3; return; fi
  local out; out="$("$TMP/$n.bin" 2>/dev/null)"
  [ "$out" = "$want" ] && ok "$n: runs, = $want" || no "$n: got '$out' want '$want'"; }

echo "=== no uninitialized reads by construction (the #33 invariant) ==="
# A declaration with no initializer is not deferred-init; it does not exist.
write uninit      'fn main() -> Int { let x: Int; return x; }'
write uninit_mut  'fn main() -> Int { let mut x: Int; x = 5; return x; }'
write uninit_cond 'fn f(c: bool) -> Int { let mut x: Int; if c { x = 1; } return x; } pub fn main() -> Int { return 0; }'
reject uninit
reject uninit_mut
reject uninit_cond

echo "=== declare-with-value and mutate-after-init still work ==="
write decl    'fn main() -> Int { let x: Int = 7; return x; }'
write mutate  'fn main() -> Int { let mut x: Int = 1; x = 41; return x + 1; }'
accept_run decl 7
accept_run mutate 42

echo "=== the doc and its references exist ==="
for d in docs/MEMORY_MODEL.md docs/MEMORY_SEMANTICS.md docs/VALUE_MODEL.md docs/MEMORY_GUARANTEES.md; do
  [ -f "$d" ] && ok "doc present: $d" || no "missing doc: $d"
done
grep -q "no uninitialized reads" docs/MEMORY_SEMANTICS.md && ok "MEMORY_SEMANTICS states the invariant" \
  || no "MEMORY_SEMANTICS missing the definite-initialization invariant"

echo ""
echo "MEMORY-MODEL: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
