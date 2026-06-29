#!/usr/bin/env bash
# Linear-discard gate — KNOWN_HOLES H6 (silent discard of a non-Copy value).
#
# Two rules, locked here:
#   1. A non-Copy value discarded as a bare statement / deferred call is rejected
#      (E0287) — `make_resource();`, `Token { .. };`, `defer make();`.
#   2. `_` is only a pattern wildcard, never a discard device: `let _ = e;` is
#      removed (E0289), and a `_` that would silently consume a value transitively
#      owning a resource — a wildcard arm or a `_` payload field — is rejected
#      (E0288), at every site.
# Legitimate forms still compile:
#   - `match e { _ => {} }` ignores a NON-resource value (the ack escape);
#   - `free(box);` is the free-and-drop idiom (free IS the consumption);
#   - a consumed value (passed on / returned / destructured-and-destroyed) compiles;
#   - discarding a Copy value as a bare statement compiles.

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
  struct File { fd: i32 }
  impl Destroy for File { fn destroy(self) { } }
  enum Data { A { v: i32 }, B {} }
  enum Res { Has { f: File }, Empty {} }
  fn make() -> Token { return Token { x: 1 }; }
  fn sink(t: Token) -> Int { return t.x as Int; }
  fn maybe() -> Option<i32> { return Option::<i32>::None {}; }
  fn boxed() with(Alloc) -> Heap<Token> { return alloc(Token { x: 1 }); }
  fn openr() -> Res { return Res::Has { f: File { fd: 3 } }; }'

reject(){ printf '%s\n  %s\n}' "$HDR" "$3" > "$TMP/t.con"
  local out; out="$("$C" "$TMP/t.con" -o "$TMP/t.bin" 2>&1)"
  if echo "$out" | grep -q "($2)"; then ok "$1 rejected ($2)"
  else no "$1: expected $2, got '$(echo "$out" | grep -oE '\([A-Z0-9]+\)' | head -1)'"; fi; }

accept(){ printf '%s\n  %s\n}' "$HDR" "$2" > "$TMP/t.con"
  if "$C" "$TMP/t.con" -o "$TMP/t.bin" >"$TMP/t.err" 2>&1; then ok "$1 compiles"
  else no "$1: expected to compile"; sed 's/^/        /' "$TMP/t.err" | head -2; fi; }

echo "=== bare/deferred non-Copy discards rejected (E0287) ==="
reject "bare make();"       E0287 'fn main() -> Int { make(); return 0; }'
reject "bare Token { .. };" E0287 'fn main() -> Int { Token { x: 1 }; return 0; }'
reject "defer linear call"  E0287 'fn main() -> Int { defer make(); return 0; }'

echo "=== let _ removed (E0289) ==="
reject "let _ = make();"    E0289 'fn main() -> Int { let _ = make(); return 0; }'
reject "let _ = 5; (Copy)"  E0289 'fn main() -> Int { let _ = 5; return 0; }'

echo "=== \`_\` cannot consume a resource owner, at any site (E0288) ==="
reject "wildcard arm/resource"    E0288 'fn f(r: Res) -> Int { match r { _ => { } } return 0; } fn main() -> Int { return f(openr()); }'
reject "wildcard payload/resource" E0288 'fn f(r: Res) -> Int { match r { Res::Has { _ } => { }, Res::Empty {} => { } } return 0; } fn main() -> Int { return f(openr()); }'

echo "=== legitimate forms still compile ==="
accept "match opt { _ => {} } (non-resource)" 'fn main() -> Int { let o: Option<i32> = maybe(); match o { _ => { } } return 0; }'
accept "wildcard payload (non-resource)"      'fn f(d: Data) -> Int { match d { Data::A { _ } => { }, Data::B {} => { } } return 0; } fn main() -> Int { return f(Data::B {}); }'
accept "free(box)"          'fn main() with(Alloc) -> Int { let b: Heap<Token> = boxed(); free(b); return 0; }'
accept "consumed (passed)"  'fn main() -> Int { let t: Token = make(); return sink(t); }'
accept "i32 discard (Copy)" 'fn noise(n: i32) -> i32 { return n + 1; } fn main() -> Int { noise(5); return 0; }'

echo ""
echo "LINEAR-DISCARD: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
