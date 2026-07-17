#!/usr/bin/env bash
# Trailing-value gate for if/match in value blocks (STATEMENT_EXPRESSION_MODEL
# follow-up): inside a VALUE block (if-expression branch, while-else, match arm
# block), a trailing `match` or `if/else` whose branches end with values IS the
# block's value. Previously they always parsed as statements, so the branch
# typed as `()` (confusing E0224) or the nested branch failed to parse at all
# ("expected ';', got }") — together ~60% of differential-fuzzer rejects.
#
# A trailing `if`/`match` whose arms/branches are ALL statements stays a
# statement (its lowering has no value merge; flipping it changed the SSA of
# existing programs — the integrity-example E0714 regression this gate pins).

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
source "$ROOT_DIR/scripts/tests/lib/selfprint.sh"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
TMPDIR=$(mktemp -d)
trap 'rm -rf "$TMPDIR"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

agree(){ local label="$1" F="$2" want="$3"
  local I C
  I="$("$COMPILER" "$F" --interp 2>&1)"
  gate_selfprint_wrap "$F" "$F.w.con"
  "$COMPILER" "$F.w.con" -o "$F.bin" >/dev/null 2>&1 && C="$("$F.bin")" || C="<compile/run failed>"
  if [ "$I" = "$want" ] && [ "$C" = "$want" ]; then ok "$label"
  else no "$label (want $want, interp=$I compiled=$C)"; fi; }

cat > "$TMPDIR/m_in_if.con" <<'EOF'
mod m {
    fn main() -> Int {
        let mut x: i32 = 1;
        let v: i32 = if x > 0 { match x { 0 => 5, _ => 6 } } else { 0 };
        return v as Int;
    }
}
EOF
agree "trailing match in if-expression branch" "$TMPDIR/m_in_if.con" "6"

cat > "$TMPDIR/if_in_if.con" <<'EOF'
mod m {
    fn main() -> Int {
        let mut x: i32 = 1;
        let v: i32 = if x > 0 { if x > 5 { 10 } else { 20 } } else { 0 };
        return v as Int;
    }
}
EOF
agree "trailing nested if-expression" "$TMPDIR/if_in_if.con" "20"

cat > "$TMPDIR/chain.con" <<'EOF'
mod m {
    fn main() -> Int {
        let mut x: i32 = 7;
        let v: i32 = if x > 10 { 1 } else if x > 5 { 2 } else { 3 };
        let w: i32 = if x > 10 { if x > 20 { 4 } else { 5 } } else if x > 5 { if x > 6 { 6 } else { 7 } } else { 8 };
        return ((v * 10) + w) as Int;
    }
}
EOF
agree "else-if chains as values (nested)" "$TMPDIR/chain.con" "26"

cat > "$TMPDIR/m_in_arm.con" <<'EOF'
mod m {
    fn main() -> Int {
        let mut x: i32 = 2;
        let v: i32 = match x { 2 => { match x { 2 => 42, _ => 1 } }, _ => 0 };
        return v as Int;
    }
}
EOF
agree "trailing match in match-arm block" "$TMPDIR/m_in_arm.con" "42"

cat > "$TMPDIR/stmt_if.con" <<'EOF'
mod m {
    fn main() -> Int {
        let mut x: i32 = 1;
        if x > 0 { x = x + 1; } else { x = x - 1; }
        if x > 0 { x = x + 10; }
        let v: i32 = if x > 0 { match x { 12 => 100, _ => 50 } } else { 0 };
        return v as Int;
    }
}
EOF
agree "statement ifs stay statements alongside value forms" "$TMPDIR/stmt_if.con" "100"

# Statement-if TRAILING an arm block, with all-statement branches: must remain a
# statement (no value merge in SSA), so aggregates in scope never get phi'd —
# pins the E0714 regression shape from examples/integrity.
cat > "$TMPDIR/trail_stmt_if.con" <<'EOF'
mod m {
    fn main() -> Int {
        let mut x: i32 = 1;
        let mut acc: i32 = 0;
        match x {
            1 => {
                if x > 0 { acc = acc + 7; } else { acc = acc - 7; }
            },
            _ => { acc = 99; }
        }
        return acc as Int;
    }
}
EOF
agree "all-statement trailing if in arm stays a statement" "$TMPDIR/trail_stmt_if.con" "7"

echo
echo "check_trailing_value_blocks: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
