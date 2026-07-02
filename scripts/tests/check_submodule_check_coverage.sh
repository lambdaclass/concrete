#!/usr/bin/env bash
# Submodule front-end-checking gate (KNOWN_HOLES H12).
#
# Submodule function BODIES — every `mod x;` file in a project and inline
# `mod x { … }` nests — were historically NEVER front-end checked: only their
# signatures were consumed, so type errors, IMMUTABLE ASSIGNMENTS, and
# LINEARITY violations in them compiled silently (CoreCheck's coarser
# Core-level rules were the only net). Fixed for user code: checkProgram now
# recurses into submodules mirroring Elab's context.
#
# The `std` subtree is EXEMPT for now (H12, OPEN): its bodies carry ~384
# never-checked violations and possibly checker-unsupported shapes; migrating
# it is tracked burn-down work. This gate (a) locks user-submodule
# enforcement, (b) keeps the exemption HONEST — it must stay disclosed in
# KNOWN_HOLES and visible in Check.lean until the migration removes it.

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

echo "=== the std exemption stays tracked and disclosed (H12) ==="

if grep -q 'KNOWN_HOLES H12' Concrete/Check.lean; then
  ok "std exemption is marked H12 in Check.lean"
else
  no "std exemption marker missing from Check.lean (removed? then close H12 and delete this check)"
fi
# The burn-down list only GROWS: every migrated module must stay migrated.
MIGRATED="alloc ascii bitset bytes env fs hash libc math mem ordered_set ptr rand sha256 string test writer"
MISSING=""
for m in $MIGRATED; do
  grep -q "\"$m\"" Concrete/Check.lean || MISSING="$MISSING $m"
done
if [ -z "$MISSING" ]; then
  ok "all $(echo $MIGRATED | wc -w | tr -d ' ') migrated std modules stay on the burn-down list"
else
  no "migrated std modules REMOVED from stdMigratedSubmodules:$MISSING (regression — the list only grows)"
fi
if grep -q '^### H12' docs/KNOWN_HOLES.md; then
  ok "H12 disclosed in KNOWN_HOLES.md"
else
  no "H12 entry missing from KNOWN_HOLES.md"
fi

echo
echo "check_submodule_check_coverage: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
