#!/usr/bin/env bash
export CONCRETE_ECHO_RESULT=1  # MAIN_EXIT_MODEL stage 1: legacy echoed-result mode until fixtures migrate (stage 2 deletes this)
# Nested-scope linearity gate — KNOWN_HOLES H9 (a NAMED linear value bound in a
# nested if/else branch or a matched payload, left unconsumed, used to leak: the
# branch/arm merge dropped it before the function-level scope-exit check ran).
#
# Locked here:
#   1. A linear value declared in an if-branch / else-branch / match arm and not
#      consumed before the block exits is rejected (E0208) — including on a
#      `return`/`break` path (control leaves the scope, so the resource leaks).
#   2. `let g = f;` over a linear `f` MOVES it (use-after-move is E0205; a moved-
#      from value left unconsumed is still flagged). This is what lets
#      `let local = payload;` transfer ownership instead of stranding the payload.
#   3. EXEMPT: a block whose end is unreachable — a non-terminating `while true {}`
#      (no break) or an `abort()` — may leave a resource live (a server accept loop).
# Legitimate forms still compile:
#   - a branch/arm local consumed before the block exits (passed on / destroy());
#   - a payload moved into a local and then consumed;
#   - a `return value;` that returns (consumes) the payload;
#   - a value that owns no resource (a Copy view) freely dropped.

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
  struct File { fd: i32 }
  impl Destroy for File { fn destroy(self) { let File { fd } = self; } }
  struct Copy ViewPair { a: i32, b: i32 }
  enum Res { Has { f: File }, Empty {} }
  fn make() -> File { return File { fd: 3 }; }
  fn openr() -> Res { return Res::Has { f: File { fd: 3 } }; }
  fn sink(f: File) -> Int { let File { fd } = f; return fd as Int; }
  fn peek(f: &File) -> Int { return f.fd as Int; }'

reject(){ printf '%s\n  %s\n}' "$HDR" "$3" > "$TMP/t.con"
  local out; out="$("$C" "$TMP/t.con" -o "$TMP/t.bin" 2>&1)"
  if grep <<<"$out" -q "($2)"; then ok "$1 rejected ($2)"
  else no "$1: expected $2, got '$(grep <<<"$out" -oE '\([A-Z0-9]+\)' | head -1)'"; fi; }

accept(){ printf '%s\n  %s\n}' "$HDR" "$2" > "$TMP/t.con"
  if "$C" "$TMP/t.con" -o "$TMP/t.bin" >"$TMP/t.err" 2>&1; then ok "$1 compiles"
  else no "$1: expected to compile"; sed 's/^/        /' "$TMP/t.err" | head -3; fi; }

echo "=== nested-scope linear leaks rejected (E0208) ==="
reject "then-branch local"   E0208 'fn main() -> Int { let c: Bool = true; if c { let r: File = make(); } return 0; }'
reject "else-branch local"   E0208 'fn main() -> Int { let c: Bool = true; if c { } else { let r: File = make(); } return 0; }'
reject "match payload"        E0208 'fn f(r: Res) -> Int { match r { Res::Has { f } => { }, Res::Empty {} => { } } return 0; } fn main() -> Int { return f(openr()); }'
reject "leak on branch return" E0208 'fn main() -> Int { let c: Bool = true; if c { let r: File = make(); return 1; } return 0; }'
reject "leak on arm return"    E0208 'fn f(r: Res) -> Int { match r { Res::Has { f } => { return 1; }, Res::Empty {} => { } } return 0; } fn main() -> Int { return f(openr()); }'

echo "=== move-through-let (E0205 use-after-move) ==="
reject "use after let-move"   E0205 'fn main() -> Int { let f: File = make(); let g: File = f; sink(g); return sink(f); }'

echo "=== legitimate nested forms still compile ==="
accept "consumed in then"          'fn main() -> Int { let c: Bool = true; if c { let r: File = make(); sink(r); } return 0; }'
accept "payload moved + consumed"  'fn f(r: Res) -> Int { match r { Res::Has { f } => { let g: File = f; sink(g); }, Res::Empty {} => { } } return 0; } fn main() -> Int { return f(openr()); }'
accept "return consumes payload"   'fn f(r: Res) -> File { match r { Res::Has { f } => { return f; }, Res::Empty {} => { return make(); } } } fn main() -> Int { return sink(f(openr())); }'
accept "while-true server loop"    'fn srv() with() { let l: File = make(); while true { peek(&l); } } fn main() -> Int { return 0; }'
accept "Copy view dropped in arm"  'fn f(r: Res) -> Int { match r { Res::Has { f } => { let v: ViewPair = ViewPair { a: 1, b: 2 }; sink(f); }, Res::Empty {} => { } } return 0; } fn main() -> Int { return f(openr()); }'

echo ""
echo "LINEAR-NESTED-SCOPE: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
