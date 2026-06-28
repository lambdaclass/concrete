#!/usr/bin/env bash
# Linear-discard gate — KNOWN_HOLES H6 (silent discard of a non-Copy value).
#
# A non-Copy (linear) value must be consumed; silently dropping one is an error.
# This gate locks the discard sites that were previously silent:
#   - a bare statement expression `make();` / `Token { .. };` is rejected (E0287);
#   - a discarded linear CALL result in statement position is rejected (E0287);
#   - a linear value declared inside an `if`/`else` branch and not consumed is
#     rejected at the branch merge (E0208);
#   - a matched payload binding left unconsumed in an arm is rejected (E0208);
#   - a deferred call whose return is a linear value is rejected (E0287);
# and the legitimate forms still compile:
#   - `let _ = make();` is the explicit acknowledgement (discard on purpose);
#   - `free(box);` is the free-and-drop idiom (free IS the consumption);
#   - a consumed value (passed on / returned / destructured) compiles.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
C="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first ($C missing)" >&2; exit 2; }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

HDR='mod m {
  struct Token { x: i32 }
  enum E { A { t: Token }, B {} }
  fn make() -> Token { return Token { x: 1 }; }
  fn sink(t: Token) -> Int { return t.x as Int; }
  fn boxed() with(Alloc) -> Heap<Token> { return alloc(Token { x: 1 }); }'

# reject <label> <code> <body>
reject(){ printf '%s\n  %s\n}' "$HDR" "$3" > "$TMP/t.con"
  local out; out="$("$C" "$TMP/t.con" -o "$TMP/t.bin" 2>&1)"
  if echo "$out" | grep -q "($2)"; then ok "$1 rejected ($2)"
  else no "$1: expected $2, got '$(echo "$out" | grep -oE '\([A-Z0-9]+\)' | head -1)'"; fi; }

# accept <label> <body>
accept(){ printf '%s\n  %s\n}' "$HDR" "$2" > "$TMP/t.con"
  if "$C" "$TMP/t.con" -o "$TMP/t.bin" >"$TMP/t.err" 2>&1; then ok "$1 compiles"
  else no "$1: expected to compile"; sed 's/^/        /' "$TMP/t.err" | head -2; fi; }

echo "=== silent linear discards are rejected ==="
reject "bare make();"        E0287 'fn main() -> Int { make(); return 0; }'
reject "bare Token { .. };"  E0287 'fn main() -> Int { Token { x: 1 }; return 0; }'
reject "branch-local in if"  E0208 'fn main() -> Int { if true { let r: Token = make(); } return 0; }'
reject "match-arm payload"   E0208 'fn f(e: E) -> Int { match e { E::A { t } => { }, E::B {} => { } } return 0; } fn main() -> Int { return f(E::B {}); }'
reject "defer linear call"   E0287 'fn main() -> Int { defer make(); return 0; }'

echo "=== legitimate forms still compile ==="
accept "let _ = make()"      'fn main() -> Int { let _ = make(); return 0; }'
accept "free(box)"           'fn main() with(Alloc) -> Int { let b: Heap<Token> = boxed(); free(b); return 0; }'
accept "consumed (passed)"   'fn main() -> Int { let t: Token = make(); return sink(t); }'
accept "match-arm consumed"  'fn f(e: E) -> Int { match e { E::A { t } => { return sink(t); }, E::B {} => { } } return 0; } fn main() -> Int { return f(E::B {}); }'
accept "match-arm wildcard"  'fn f(e: E) -> Int { match e { E::A { _ } => { }, E::B {} => { } } return 0; } fn main() -> Int { return f(E::B {}); }'
accept "i32 discard (Copy)"  'fn noise(n: i32) -> i32 { return n + 1; } fn main() -> Int { noise(5); return 0; }'

echo ""
echo "LINEAR-DISCARD: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
