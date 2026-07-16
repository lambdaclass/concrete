#!/usr/bin/env bash
# Linear-discard gate — KNOWN_HOLES H6 (silent discard of a non-Copy value).
#
# Two rules, locked here (Concrete is LINEAR — a non-Copy value must be used exactly
# once and can never silently disappear):
#   1. A non-Copy value discarded as a bare statement / deferred call is rejected
#      (E0287) — `make_resource();`, `Token { .. };`, `defer make();`.
#   2. `_` may ignore ONLY a Copy value: `let _ = e;` is removed (E0289), and a `_`
#      that would drop a NON-COPY value — a wildcard arm `match e { _ => {} }` or a
#      `_` payload field — is rejected (E0288), at every site. To get rid of a
#      non-Copy value, account for it: destructure exhaustively (a `_` on a *Copy*
#      payload is fine) and consume/hand off the parts.
# Legitimate forms still compile:
#   - exhaustive `match e { V1 { _ } => {}, V2 => {} }` where the payloads are Copy;
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
  impl Destroy for File { fn destroy(self) { let File { fd } = self; } }
  struct Wrap { f: File }
  struct Pair { f: File, g: File }
  enum Data { A { v: i32 }, B {} }
  enum Res { Has { f: File }, Empty {} }
  fn make() -> Token { return Token { x: 1 }; }
  fn sink(t: Token) -> Int { let Token { x } = t; return x as Int; }
  fn make_file(n: i32) -> File { return File { fd: n }; }
  fn make_wrap() -> Wrap { return Wrap { f: make_file(1) }; }
  fn sink_file(f: File) -> Int { destroy(f); return 0; }
  fn maybe() -> Option<i32> { return Option::<i32>::None {}; }
  fn boxed() with(Alloc) -> Heap<Token> { return alloc(Token { x: 1 }); }
  fn openr() -> Res { return Res::Has { f: File { fd: 3 } }; }'

reject(){ printf '%s\n  %s\n}' "$HDR" "$3" > "$TMP/t.con"
  local out; out="$("$C" "$TMP/t.con" -o "$TMP/t.bin" 2>&1)"
  if grep <<<"$out" -q "($2)"; then ok "$1 rejected ($2)"
  else no "$1: expected $2, got '$(grep <<<"$out" -oE '\([A-Z0-9]+\)' | head -1)'"; fi; }

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

echo "=== \`_\` may ignore only Copy — dropping a non-Copy value is rejected (E0288) ==="
reject "wildcard arm/resource"       E0288 'fn f(r: Res) -> Int { match r { _ => { } } return 0; } fn main() -> Int { return f(openr()); }'
reject "wildcard payload/resource"   E0288 'fn f(r: Res) -> Int { match r { Res::Has { _ } => { }, Res::Empty {} => { } } return 0; } fn main() -> Int { return f(openr()); }'
# Conditional Copy (2026-07-05): Option<i32> IS Copy now, so the non-Copy row
# needs a linear payload; the Option<i32> wildcard moved to the accept rows.
reject "wildcard arm/Option over linear payload" E0288 'fn main() -> Int { let o: Option<File> = Option::<File>::Some { value: make_file(1) }; match o { _ => { } } return 0; }'
reject "value wildcard/Heap (non-enum non-Copy)" E0288 'fn main() with(Alloc) -> Int { let h: Heap<Token> = boxed(); match h { _ => { } } return 0; }'

echo "=== non-enum value patterns move non-Copy scrutinees ==="
reject "value pattern then original reuse" E0205 'fn main() with(Alloc) -> Int { let h: Heap<Token> = boxed(); match h { y => { free(y); } } free(h); return 0; }'
reject "value pattern binding unconsumed" E0208 'fn main() with(Alloc) -> Int { let h: Heap<Token> = boxed(); match h { y => { } } return 0; }'
reject "array literal then element reuse" E0205 'fn main() -> Int { let a: File = make_file(1); let b: File = make_file(2); let arr: [File; 2] = [a, b]; destroy(a); destroy(b); return 0; }'
reject "partial linear struct destructure" E0252 'fn main() -> Int { let p: Pair = Pair { f: make_file(1), g: make_file(2) }; let Pair { f } = p; return sink_file(f); }'

echo "=== legitimate forms still compile ==="
accept "exhaustive, Copy payloads ignored" 'fn main() -> Int { let o: Option<i32> = maybe(); match o { Option::Some { _ } => { }, Option::None => { } } return 0; }'
accept "wildcard arm/Option<i32> (conditionally Copy)" 'fn main() -> Int { let o: Option<i32> = maybe(); match o { _ => { } } return 0; }'
accept "wildcard payload (Copy field)"     'fn f(d: Data) -> Int { match d { Data::A { _ } => { }, Data::B {} => { } } return 0; } fn main() -> Int { return f(Data::B {}); }'
accept "value pattern consumes Heap once" 'fn main() with(Alloc) -> Int { let h: Heap<Token> = boxed(); match h { y => { free(y); } } return 0; }'
accept "linear struct destructure moves field" 'fn main() -> Int { let w: Wrap = make_wrap(); let Wrap { f } = w; return sink_file(f); }'
# H17 consequence: an owned [linear; N] cannot be discharged until array
# destructure lands (KNOWN_HOLES H11 note) — asserts the honest E0208.
reject "array of linears is undischargeable" E0208 'fn main() -> Int { let a: File = make_file(1); let b: File = make_file(2); let arr: [File; 2] = [a, b]; return 0; }'
accept "free(box)"          'fn main() with(Alloc) -> Int { let b: Heap<Token> = boxed(); free(b); return 0; }'
accept "consumed (passed)"  'fn main() -> Int { let t: Token = make(); return sink(t); }'
accept "i32 discard (Copy)" 'fn noise(n: i32) -> i32 { return n + 1; } fn main() -> Int { noise(5); return 0; }'

echo "=== H16: shadowing a still-live linear binding is rejected (would leak) ==="
reject "let f=..; let f=..; (live shadow)" E0292 'fn main() -> Int { let f: File = make_file(1); let f: File = make_file(2); return sink_file(f); }'
accept "let s = transform(s) (consumed first)" 'fn grow(f: File) -> File { return f; } fn main() -> Int { let f: File = make_file(1); let f: File = grow(f); return sink_file(f); }'

echo "=== H17: params (incl. by-value self) are owned locals — must be consumed ==="
reject "fn drop_it(f: File) {} (silent-drop escape)" E0208 'fn drop_it(f: File) -> Int { return 0; } fn main() -> Int { return drop_it(make_file(1)); }'
reject "param read but never consumed"               E0208 'fn peek_leak(f: File) -> Int { return f.fd as Int; } fn main() -> Int { return peek_leak(make_file(1)); }'
reject "by-value self silently dropped"              E0208 'fn main() -> Int { let f: File = make_file(1); return sink_file(f); } impl File { fn vanish(self) -> Int { return 0; } }'
accept "param consumed via destructure"              'fn use_f(f: File) -> Int { let File { fd } = f; return fd as Int; } fn main() -> Int { return use_f(make_file(1)); }'
accept "&self / &mut self borrows carry no obligation" 'impl File { fn get(&self) -> i32 { return self.fd; } fn bump(&mut self) { self.fd = self.fd + 1; } } fn main() -> Int { let mut f: File = make_file(1); let v: i32 = f.get(); f.bump(); return (v as Int) + sink_file(f); }'

echo ""
echo "LINEAR-DISCARD: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
