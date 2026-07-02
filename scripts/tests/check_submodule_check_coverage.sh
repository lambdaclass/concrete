#!/usr/bin/env bash
# Submodule front-end-checking gate (KNOWN_HOLES H12 — CLOSED 2026-07-02).
#
# Submodule function BODIES — every `mod x;` file in a project and inline
# `mod x { … }` nests — were historically NEVER front-end checked: only their
# signatures were consumed, so type errors, IMMUTABLE ASSIGNMENTS, and
# LINEARITY violations in them compiled silently (CoreCheck's coarser
# Core-level rules were the only net). Fixed: checkProgram recurses into
# EVERY submodule (std included — the 384-violation burn-down completed and
# the exemption machinery was deleted). This gate locks the enforcement
# matrix and asserts std itself stays clean under the full front-end.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
TMPDIR=$(mktemp -d)
trap 'rm -rf "$TMPDIR"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# mkproj <helper-body>: build a two-file project whose sub-file has the body.
mkproj(){ local dir="$1" helper="$2"
  mkdir -p "$dir/src"
  printf '[package]\nname = "subcheck"\n' > "$dir/Concrete.toml"
  cat > "$dir/src/main.con" <<'EOF'
mod subcheck {
    mod helper;
    import helper.{addx};
    pub fn main() -> u32 { return addx(1); }
}
EOF
  printf '%s' "$helper" > "$dir/src/helper.con"; }

# rejects <label> <dir> <code>: project build must fail with the given code.
rejects(){ local label="$1" dir="$2" code="$3"
  local OUT; OUT="$(cd "$dir" && "$COMPILER" build 2>&1)"
  if [ $? -ne 0 ] && printf '%s' "$OUT" | grep -q "($code)"; then ok "$label"
  else no "$label (want $code; got: $(printf '%s' "$OUT" | head -1))"; fi; }

echo "=== user submodule bodies are front-end checked ==="

mkproj "$TMPDIR/imm" 'mod helper {
    pub fn addx(a: u32) -> u32 {
        let c: u32 = 5;
        c = c + 1;
        return a + c;
    }
}'
rejects "immutable assignment in sub-file (E0217)" "$TMPDIR/imm" "E0217"
# #24a: the diagnostic must NAME the sub-file (and quote ITS source line),
# not the main file the build started from.
OUT="$(cd "$TMPDIR/imm" && "$COMPILER" build 2>&1)"
if printf '%s' "$OUT" | grep -q "helper.con:4" && printf '%s' "$OUT" | grep -qF 'c = c + 1'; then
  ok "sub-file diagnostics name the sub-file with its own snippet (#24a)"
else
  no "sub-file diagnostics misattributed (got: $(printf '%s' "$OUT" | head -1))"
fi

mkproj "$TMPDIR/ty" 'mod helper {
    pub fn addx(a: u32) -> u32 {
        let b: bool = a;
        return a;
    }
}'
rejects "type mismatch in sub-file (E0220)" "$TMPDIR/ty" "E0220"

mkproj "$TMPDIR/lin" 'mod helper {
    pub fn addx(a: u32) -> u32 {
        let s: String = "leak me";
        return a;
    }
}'
rejects "linear leak in sub-file (E0208)" "$TMPDIR/lin" "E0208"

mkproj "$TMPDIR/uam" 'mod helper {
    pub fn addx(a: u32) -> u32 {
        let s: String = "moved";
        drop_string(s);
        drop_string(s);
        return a;
    }
}'
rejects "use-after-move in sub-file (E0205)" "$TMPDIR/uam" "E0205"

mkproj "$TMPDIR/mw" 'mod helper {
    pub fn addx(a: u32) -> u32 {
        let mut b: i8 = 1;
        let mut c: i32 = 2;
        if b < c { return a; }
        return a;
    }
}'
rejects "mixed-width binop in sub-file (E0228)" "$TMPDIR/mw" "E0228"

mkproj "$TMPDIR/cap" 'mod helper {
    pub fn addx(a: u32) -> u32 {
        let t: i64 = clock_monotonic_ns();
        return a + ((t % 2) as u32);
    }
}'
rejects "missing capability in sub-file (E0520)" "$TMPDIR/cap" "E0520"

mkproj "$TMPDIR/okc" 'mod helper {
    pub fn addx(a: u32) -> u32 {
        let mut c: u32 = 5;
        c = c + 1;
        return a + c;
    }
}'
if (cd "$TMPDIR/okc" && "$COMPILER" build >/dev/null 2>&1); then
  ok "valid sub-file still builds"
else
  no "valid sub-file failed to build"
fi

# Sibling-submodule TYPE reference still checks (positive case): helper uses a
# struct defined in a sibling sub-file, mirroring Elab's sibling-type injection.
mkdir -p "$TMPDIR/sib/src"
printf '[package]\nname = "subcheck"\n' > "$TMPDIR/sib/Concrete.toml"
cat > "$TMPDIR/sib/src/main.con" <<'EOF'
mod subcheck {
    mod shapes;
    mod helper;
    import helper.{area};
    pub fn main() -> u32 { return area(); }
}
EOF
cat > "$TMPDIR/sib/src/shapes.con" <<'EOF'
mod shapes {
    pub struct Copy Rect { w: u32, h: u32 }
}
EOF
cat > "$TMPDIR/sib/src/helper.con" <<'EOF'
mod helper {
    pub fn area() -> u32 {
        let r: Rect = Rect { w: 3, h: 4 };
        return r.w * r.h;
    }
}
EOF
if (cd "$TMPDIR/sib" && "$COMPILER" build >/dev/null 2>&1); then
  ok "sibling-submodule type reference checks and builds"
else
  no "sibling-submodule type reference failed"
fi

echo "=== std stays fully front-end checked (H12 closed; no exemption returns) ==="

if grep -q 'stdMigratedSubmodules' Concrete/Check.lean; then
  no "exemption machinery is BACK in Check.lean (H12 regression)"
else
  ok "no std exemption machinery in Check.lean"
fi
STDERRS="$("$COMPILER" std/src/lib.con --test 2>&1 | grep -cE 'error\[')"
if [ "$STDERRS" = "0" ]; then
  ok "std carries zero front-end violations under the full checker"
else
  no "std has $STDERRS front-end violations (was zero when H12 closed)"
fi
if grep -q '^### H12' docs/KNOWN_HOLES.md; then
  ok "H12 recorded in KNOWN_HOLES.md"
else
  no "H12 entry missing from KNOWN_HOLES.md"
fi

echo
echo "check_submodule_check_coverage: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
