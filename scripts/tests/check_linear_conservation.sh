#!/usr/bin/env bash
# Linear-conservation gate — the systematic backstop for "a linear value silently
# duplicated or dropped through a value-flow site the main linearity path missed"
# (the class that produced the array-literal duplication bug and the struct-
# destructure over-rejection). Concrete is LINEAR: a non-Copy value flows to EXACTLY
# ONE place. This gate walks every site a value can flow INTO and asserts, for a
# linear resource `File`:
#
#   MOVE     — after the value flows into the site, reusing it is use-after-move (E0205)
#   NO-LEAK  — a site that receives it but never consumes it onward errors (E0208)
#   LEGIT    — the correctly-consumed form compiles
#
# Sites covered: let-binding, array-literal element, struct-literal field, struct
# destructure (source moved + fields become owned), function argument, return, match
# scrutinee. The `_`/bare-discard/branch-local sites are locked by
# check_linear_discard.sh and check_linear_nested_scope.sh; this gate is the
# move/duplication half. Add a row here whenever a new value-flow form is introduced.

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
  impl Destroy for File { fn destroy(self) { } }
  struct Wrap { f: File }
  struct Pair { a: File, b: File }
  enum E { A { f: File }, B {} }
  fn mk() -> File { return File { fd: 3 }; }
  fn mkW() -> Wrap { return Wrap { f: mk() }; }
  fn sink(f: File) -> Int { return f.fd as Int; }
  fn take2(x: [File; 2]) -> Int { return x[0].fd as Int; }
  fn takeW(w: Wrap) -> Int { let Wrap { f } = w; return sink(f); }'

reject(){ printf '%s\n  %s\n}' "$HDR" "$3" > "$TMP/t.con"
  local out; out="$("$C" "$TMP/t.con" -o "$TMP/t.bin" 2>&1)"
  if echo "$out" | grep -q "($2)"; then ok "$1 → $2"
  else no "$1: expected $2, got '$(echo "$out" | grep -oE '\([A-Z0-9]+\)' | head -1)'"; fi; }

accept(){ printf '%s\n  %s\n}' "$HDR" "$2" > "$TMP/t.con"
  if "$C" "$TMP/t.con" -o "$TMP/t.bin" >"$TMP/t.err" 2>&1; then ok "$1 compiles"
  else no "$1: expected to compile"; sed 's/^/        /' "$TMP/t.err" | head -2; fi; }

echo "=== let-binding moves the source ==="
reject "let g=f; reuse f"       E0205 'fn main() -> Int { let f: File = mk(); let g: File = f; sink(g); return sink(f); }'
accept "let g=f; consume g"     'fn main() -> Int { let f: File = mk(); let g: File = f; return sink(g); }'

echo "=== array-literal moves each element (the duplication bug) ==="
reject "[a,b]; reuse a"         E0205 'fn main() -> Int { let a: File = mk(); let b: File = mk(); let x: [File; 2] = [a, b]; a.destroy(); b.destroy(); return take2(x); }'
reject "[a,b] then array leaks" E0208 'fn main() -> Int { let a: File = mk(); let b: File = mk(); let x: [File; 2] = [a, b]; return 0; }'
accept "[a,b] moved into array, consumed" 'fn main() -> Int { let a: File = mk(); let b: File = mk(); let x: [File; 2] = [a, b]; return take2(x); }'

echo "=== struct-literal field moves ==="
reject "Wrap{f:x}; reuse x"     E0205 'fn main() -> Int { let x: File = mk(); let w: Wrap = Wrap { f: x }; sink(x); return takeW(w); }'
accept "Wrap{f:x} consumed"     'fn main() -> Int { let x: File = mk(); let w: Wrap = Wrap { f: x }; return takeW(w); }'

echo "=== struct destructure: source moved, fields become owned ==="
accept "destructure + consume field" 'fn main() -> Int { let w: Wrap = mkW(); let Wrap { f } = w; return sink(f); }'
reject "destructure field unconsumed" E0208 'fn main() -> Int { let w: Wrap = mkW(); let Wrap { f } = w; return 0; }'
reject "reuse source after destructure" E0205 'fn main() -> Int { let w: Wrap = mkW(); let Wrap { f } = w; sink(f); return takeW(w); }'

echo "=== function argument / return move ==="
reject "arg moved; reuse"       E0205 'fn main() -> Int { let f: File = mk(); sink(f); return sink(f); }'
reject "return moved; reuse"    E0205 'fn id(f: File) -> File { let g: File = f; sink(g); return f; } fn main() -> Int { return sink(id(mk())); }'

echo "=== match scrutinee moves ==="
reject "matched then reuse scrutinee" E0205 'fn main() -> Int { let e: E = E::B {}; match e { E::A { f } => { sink(f); }, E::B {} => { } } match e { E::A { f } => { return sink(f); }, E::B {} => { return 0; } } }'

echo "=== let-else over a non-Copy enum stays rejected (linear: use full match) ==="
reject "let-else over resource enum" E0288 'fn main() -> Int { let e: E = E::B {}; let E::A { f } = e else { return 0; }; return sink(f); }'

echo ""
echo "LINEAR-CONSERVATION: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
