#!/usr/bin/env bash
# R-0001 / bug 051 containment gate.
#
# User-defined generic enums are not yet monomorphized (monoStructsInProgram
# handles only structs). EmitSSA emits ONE LLVM type per enum name from the
# first instantiation while Lower writes each instantiation's real payload —
# so instantiations of different sizes silently corrupt the stack. Until real
# per-instantiation enum mono lands, Mono rejects any generic USER-enum
# instantiation fail-closed with E0808. Builtin Option/Result are safe
# (EmitSSA gives them a program-wide alignment-aware canonical union) and must
# NOT be caught. This gate pins all three.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER="${COMPILER:-.lake/build/bin/concrete}"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# rejected_e0808 <name> <source>
rejected_e0808() {
  local name="$1" src="$2" f="$TMP/$1.con"
  printf '%s\n' "$src" > "$f"
  local out rc; out="$("$COMPILER" "$f" -o "$TMP/$1.out" 2>&1)"; rc=$?
  # Require the E0808 mono diagnostic SPECIFICALLY — a parser/checker/linker
  # error or a crash must NOT count as containment. Also require a clean
  # non-zero rejection (not a segfault-class signal >128).
  if grep -q "(E0808)" <<<"$out" && grep -qi "error\[mono\]" <<<"$out" \
     && [ "$rc" -ne 0 ] && [ "$rc" -lt 128 ]; then
    ok "$name rejected with E0808 (error[mono], rc=$rc)"
  else
    no "$name NOT cleanly rejected with E0808 (rc=$rc) — got: $(head -1 <<<"$out")"
  fi
}
# accepted <name> <source>
accepted() {
  local name="$1" src="$2" f="$TMP/$1.con"
  printf '%s\n' "$src" > "$f"
  if "$COMPILER" "$f" -o "$TMP/$1.out" >/dev/null 2>&1; then ok "$name compiles"
  else no "$name failed to compile: $("$COMPILER" "$f" -o "$TMP/$1.out" 2>&1 | head -1)"; fi
}

echo "=== user generic enums are rejected fail-closed (E0808) ==="
rejected_e0808 mixed_sizes 'mod m {
  enum Wrap<T> { W { v: T }, N }
  pub fn main() -> i64 {
    let a: Wrap<i64> = Wrap::<i64>::W { v: 3 };
    let b: Wrap<i32> = Wrap::<i32>::W { v: 40 };
    let mut r: i64 = 0;
    match a { Wrap::W { v } => { r = r + v; }, Wrap::N => {} }
    match b { Wrap::W { v } => { r = r + (v as i64); }, Wrap::N => {} }
    return r;
  }
}'
rejected_e0808 single_inst 'mod m {
  enum Box<T> { B { v: T } }
  pub fn main() -> i64 { let a: Box<i64> = Box::<i64>::B { v: 5 }; match a { Box::B { v } => { return v; } } }
}'

echo "=== builtin Option/Result are NOT caught (they have a canonical union) ==="
accepted builtin_option 'mod m {
  pub fn main() -> i64 {
    let a: Option<i64> = Option::<i64>::Some { value: 7 };
    let b: Option<i32> = Option::<i32>::Some { value: 5 };
    let mut r: i64 = 0;
    match a { Option::Some { value } => { r = r + value; }, Option::None => {} }
    match b { Option::Some { value } => { r = r + (value as i64); }, Option::None => {} }
    return r;
  }
}'
accepted nongeneric_enum 'mod m {
  enum Color { Red, Green, Blue }
  pub fn main() -> i64 { let c: Color = Color::Green; match c { Color::Red => { return 1; }, Color::Green => { return 2; }, Color::Blue => { return 3; } } }
}'

echo
echo "GENERIC-ENUM-CONTAINMENT: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
