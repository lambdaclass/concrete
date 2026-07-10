#!/usr/bin/env bash
# Check/Elab source-type AGREEMENT gate (Phase 6.5 #9 — TypeJudgment).
#
# The E0228/E0715 bug class was Check and Elab each running their own source-type
# inference and disagreeing. Both now route the type decision through the one
# shared judgment (Concrete/Semantics/TypeJudgment.lean): intLit/floatLit typing,
# and — the case this gate targets — binop operand ordering (isFlexibleLit +
# binOpOperandOrder). Elab's old bottom-up re-elaborate repair (which special-
# cased nested literal trees like `(8 + 5) + x`) was DELETED in favor of Check's
# top-down rule: type the concrete operand first, hint the whole flexible literal
# tree with its type.
#
# This gate is the red-team for that deletion. If Check accepted one type and
# Elab stamped a different CExpr.ty, one of three things happens: check-accept
# with a codegen mismatch (E0715 at SSA-verify), a compile failure, or a compiled
# binary whose value diverges from the interpreter. So `agree` (interp ==
# compiled == expected) is a per-node agreement proxy: it cannot pass unless the
# two front-end passes typed every node in the flexible tree identically.
#
# LOAD-BEARING by construction: every `agree` case takes the concrete width from
# a SIBLING operand, NOT from the enclosing let/return annotation (the binop's
# own hint is `Int` or absent). A hint would flow top-down and mask a broken
# operand order; a sibling-only width does not. Mutate binOpOperandOrder to
# always return `.lhsFirst` and rebuild — the flexible tree types `.int` while
# the sibling is a fixed width, and the compiled binary fails SSA-verify (E0715),
# failing this gate. That is exactly the drift the shared judgment forbids.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
TMPDIR=$(mktemp -d)
trap 'rm -rf "$TMPDIR"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# agree <label> <file> <expected>: check-accepts, and interp == compiled == want.
agree(){ local label="$1" F="$2" want="$3"
  local I C
  I="$("$COMPILER" "$F" --interp 2>&1)"
  "$COMPILER" "$F" -o "$F.bin" >/dev/null 2>&1 && C="$("$F.bin")" || C="<compile/run failed>"
  if [ "$I" = "$want" ] && [ "$C" = "$want" ]; then ok "$label"
  else no "$label (want $want, interp=$I compiled=$C)"; fi; }

# rejects <label> <file> <code>: check REJECTS with the given diagnostic code
# (both passes agree it is ill-typed, rather than one accepting it).
rejects(){ local label="$1" F="$2" code="$3"
  local OUT; OUT="$("$COMPILER" "$F" --interp 2>&1)"
  if [ $? -ne 0 ] && printf '%s' "$OUT" | grep -q "$code"; then ok "$label"
  else no "$label (want $code, got: $(printf '%s' "$OUT" | head -1))"; fi; }

echo "=== flexible literal tree adopts the SIBLING's width (no rescuing hint) ==="

# Array index: the index expression's hint is Int, but the concrete width comes
# from the sibling `i: i32`. The flexible `(1 + 1)` must adopt i32 from i, not Int
# from the index hint. THIS is the case Elab's deleted repair handled; it is
# discriminating because no let annotation supplies the width.
cat > "$TMPDIR/idx.con" <<'EOF'
mod m {
    fn main() -> Int {
        let i: i32 = 2;
        let a: [i32; 8] = [0,1,2,3,4,5,6,7];
        let v: i32 = a[((1 + 1) + i) as Int];
        return v as Int;
    }
}
EOF
agree "a[((1 + 1) + i) as Int]  [rhsFirst, sibling width]" "$TMPDIR/idx.con" "4"

# Mirror: flexible tree on the RHS, concrete `i` on the left (lhsFirst path).
cat > "$TMPDIR/idxm.con" <<'EOF'
mod m {
    fn main() -> Int {
        let i: i32 = 2;
        let a: [i32; 8] = [0,1,2,3,4,5,6,7];
        let v: i32 = a[(i + (1 + 1)) as Int];
        return v as Int;
    }
}
EOF
agree "a[(i + (1 + 1)) as Int]  [lhsFirst, sibling width]" "$TMPDIR/idxm.con" "4"

# Comparison in `if`: no width hint at all; the flexible `(8 + 5)` must adopt i32
# from `x`, and the whole comparison type i32 < i32.
cat > "$TMPDIR/cmp.con" <<'EOF'
mod m {
    fn main() -> Int {
        let x: i32 = 10;
        let y: i32 = 13;
        if (8 + 5) + x > y { return 1; } else { return 0; }
    }
}
EOF
agree "(8 + 5) + x > y  [comparison, no hint]" "$TMPDIR/cmp.con" "1"

# Deep nesting where the only concrete anchor is the sibling `x` (the outer hint
# from `as Int` is Int, which must NOT win). isFlexibleLit must recurse the whole
# tree so it is hinted i32 from x.
cat > "$TMPDIR/deep.con" <<'EOF'
mod m {
    fn main() -> Int {
        let x: i32 = 100;
        return (((1 + 2) + 3) + x) as Int;
    }
}
EOF
agree "(((1 + 2) + 3) + x) as Int  [deep, sibling width]" "$TMPDIR/deep.con" "106"

# Subtraction inside the flexible tree (binOp of two literals is flexible), width
# from the sibling.
cat > "$TMPDIR/sub.con" <<'EOF'
mod m {
    fn main() -> Int {
        let x: i32 = 100;
        return ((0 - 4) + x) as Int;
    }
}
EOF
agree "((0 - 4) + x) as Int  [subtraction in tree]" "$TMPDIR/sub.con" "96"

echo "=== both-flexible tree types under the enclosing hint (sanity) ==="

cat > "$TMPDIR/both.con" <<'EOF'
mod m {
    fn main() -> Int {
        let r: i32 = (10 + 2) * (3 + 1);
        return r as Int;
    }
}
EOF
agree "(10 + 2) * (3 + 1)  [both flexible]" "$TMPDIR/both.con" "48"

echo "=== genuine width mismatch is rejected by BOTH passes (E0228) ==="

# An `Int` VALUE (not a flexible literal) + i32 is a real mismatch: it must be
# rejected, not silently unified. `a` is an ident, so isFlexibleLit is false and
# no hint rescues it — exactly the residue Check's line-122 rejection covers.
cat > "$TMPDIR/mismatch.con" <<'EOF'
mod m {
    fn main() -> Int {
        let a: Int = 5;
        let b: i32 = 3;
        let c = a + b;
        return c as Int;
    }
}
EOF
rejects "Int value + i32  [real mismatch]" "$TMPDIR/mismatch.con" "E0228"

echo
echo "check_type_agreement: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
