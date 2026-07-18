#!/usr/bin/env bash
# 0b construction rights gate: representation is private-by-default across
# modules (docs/CONSTRUCTION_RIGHTS.md). Pins every rejection + the pub
# escape. Verified soundness holes: NonZeroU32(0) bypass, and cross-module
# Bytes field read/write/literal forgery.
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"; cd "$ROOT_DIR"
C="$ROOT_DIR/.lake/build/bin/concrete"; [ -x "$C" ] || { echo "build first" >&2; exit 2; }
TMP=$(mktemp -d); trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
# rej <label> <expected-code> <body>: a project whose build must FAIL with the code
rej(){ local label="$1" code="$2"; mkdir -p "$TMP/p/src"; printf '[package]\nname="p"\nversion="0.1.0"\n' > "$TMP/p/Concrete.toml"; cat > "$TMP/p/src/main.con"
  if ( cd "$TMP/p" && "$C" build ) > "$TMP/p.log" 2>&1; then no "$label (COMPILED — hole open)"; else grep -q "$code" "$TMP/p.log" && ok "$label -> $code" || no "$label (wrong code: $(grep -m1 error "$TMP/p.log"))"; fi
  rm -rf "$TMP/p"; }
acc(){ local label="$1"; mkdir -p "$TMP/a/src"; printf '[package]\nname="a"\nversion="0.1.0"\n' > "$TMP/a/Concrete.toml"; cat > "$TMP/a/src/main.con"
  ( cd "$TMP/a" && "$C" build ) >"$TMP/a.log" 2>&1 && ok "$label (accepted)" || no "$label (rejected: $(grep -m1 error "$TMP/a.log"))"; rm -rf "$TMP/a"; }

rej "newtype construct cross-module" E0296 <<'EOF'
mod main { import std.numeric.{NonZeroU32};
  fn main() with(Std) -> u8 { let z: NonZeroU32 = NonZeroU32(0); discard(z); return 0; } }
EOF
rej "struct field READ cross-module" E0298 <<'EOF'
mod main { import std.bytes.{Bytes};
  fn main() with(Std) -> u8 { let mut b: Bytes = Bytes::new(); b.push(1); let n: u64 = b.len; discard(n); b.drop(); return 0; } }
EOF
rej "struct field WRITE cross-module" E0298 <<'EOF'
mod main { import std.bytes.{Bytes};
  fn main() with(Std) -> u8 { let mut b: Bytes = Bytes::new(); b.push(1); b.len = 999; b.drop(); return 0; } }
EOF
rej "struct LITERAL cross-module (private field)" E0297 <<'EOF'
mod main { import std.bytes.{Bytes};
  fn main() with(Std) -> u8 { let f: Bytes = Bytes { ptr: 0 as *mut u8, len: 9, cap: 9 }; f.drop(); return 0; } }
EOF
acc "public accessor path (b.len())" <<'EOF'
mod main { import std.bytes.{Bytes};
  fn main() with(Std) -> u8 { let mut b: Bytes = Bytes::new(); b.push(1); let n: u64 = b.len(); discard(n); b.drop(); if n == 1 { return 0; } return 1; } }
EOF
acc "newtype public constructor (try_new)" <<'EOF'
mod main { import std.numeric.{NonZeroU32};
  fn main() with(Std) -> u8 { match NonZeroU32::try_new(5) { Option::Some { value } => { discard(value); return 0; }, Option::None => { return 1; }, } } }
EOF
acc "pub fields cross-module (construct + read)" <<'EOF'
mod geom { pub struct Copy P { pub x: Int, pub y: Int } }
mod main { import geom.{P};
  fn main() -> Int { let p: P = P { x: 3, y: 4 }; return p.x + p.y; } }
EOF
echo; echo "CONSTRUCTION-RIGHTS: PASS=$PASS FAIL=$FAIL"; [ "$FAIL" -eq 0 ]
