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

echo "=== the std exemption stays tracked and disclosed (H12) ==="

if grep -q 'KNOWN_HOLES H12' Concrete/Check.lean; then
  ok "std exemption is marked H12 in Check.lean"
else
  no "std exemption marker missing from Check.lean (removed? then close H12 and delete this check)"
fi
if grep -q '^### H12' docs/KNOWN_HOLES.md; then
  ok "H12 disclosed in KNOWN_HOLES.md"
else
  no "H12 entry missing from KNOWN_HOLES.md"
fi

echo
echo "check_submodule_check_coverage: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
