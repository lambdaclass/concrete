#!/usr/bin/env bash
export CONCRETE_ECHO_RESULT=1  # MAIN_EXIT_MODEL stage 1: legacy echoed-result mode until fixtures migrate (stage 2 deletes this)
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
  if [ $? -ne 0 ] && grep -q <<<"$OUT" "$code"; then ok "$label"
  else no "$label (want $code, got: $(printf '%s' "$OUT" | head -1))"; fi; }

# both_trap <label> <file>: interp AND the compiled binary both trap on a
# checked-overflow (interp reports "overflow" and exits non-zero; the binary
# exits non-zero). This is the load-bearing signal for control-flow branch
# typing: if the branch value is typed at the RESULT width (e.g. i32), an
# overflowing branch traps in both. Under the old drift, Elab typed the branch
# Int (i64) while stamping the node i32, so interp returned the un-truncated i64
# value (no trap) while the compiled binary silently truncated — a divergence,
# not a shared trap.
both_trap(){ local label="$1" F="$2"
  local IOUT IRC CRC
  IOUT="$("$COMPILER" "$F" --interp 2>&1)"; IRC=$?
  # Run the binary inside command substitution so the shell's job-control signal
  # notice ("Abort trap: 6") is absorbed rather than printed to the gate log.
  if "$COMPILER" "$F" -o "$F.bin" >/dev/null 2>&1; then local _o; _o="$("$F.bin" 2>&1)"; CRC=$?; else CRC=200; fi
  if [ $IRC -ne 0 ] && grep -qi <<<"$IOUT" "overflow" && [ $CRC -ne 0 ]; then ok "$label"
  else no "$label (interp rc=$IRC compiled rc=$CRC; interp: $(printf '%s' "$IOUT" | head -1))"; fi; }

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

echo "=== if/match branches adopt the result width (control-flow family) ==="

# Clean sanity: a flexible binop in each branch types at the result width i32.
cat > "$TMPDIR/ifval.con" <<'EOF'
mod m { fn main() -> Int {
    let c: Bool = true;
    let x: i32 = if c { 40 + 2 } else { 0 };
    return x as Int;
} }
EOF
agree "if c { 40 + 2 } else { 0 } : i32" "$TMPDIR/ifval.con" "42"

cat > "$TMPDIR/mtval.con" <<'EOF'
mod m { fn main() -> Int {
    let i: i32 = 0;
    let x: i32 = match i { 0 => 40 + 2, _ => 0 };
    return x as Int;
} }
EOF
agree "match i { 0 => 40 + 2, _ => 0 } : i32" "$TMPDIR/mtval.con" "42"

# Discriminating: an i32-overflowing branch must trap in BOTH interp and compiled
# (the branch is typed i32, so 2e9 + 2e9 overflows). Under the old drift the
# branch was Int (i64), interp returned 4000000000 (no trap) and the compiled
# binary silently truncated — a divergence this case forbids.
cat > "$TMPDIR/ifovf.con" <<'EOF'
mod m { fn main() -> Int {
    let c: Bool = true;
    let x: i32 = if c { 2000000000 + 2000000000 } else { 0 };
    return x as Int;
} }
EOF
both_trap "if-branch i32 overflow traps in both (was 4e9 vs -294967296)" "$TMPDIR/ifovf.con"

cat > "$TMPDIR/mtovf.con" <<'EOF'
mod m { fn main() -> Int {
    let i: i32 = 0;
    let x: i32 = match i { 0 => 2000000000 + 2000000000, _ => 0 };
    return x as Int;
} }
EOF
both_trap "match-arm i32 overflow traps in both" "$TMPDIR/mtovf.con"

echo "=== families that flow a context type already agree (regression guard) ==="

# Call argument: a flexible binop arg adopts the PARAMETER width (i32). Both Check
# and Elab flow the param type as the arg hint, so this already agrees; the row
# guards against a future change dropping that hint (which would re-open the
# interp-vs-compiled drift, caught here as a non-shared trap).
cat > "$TMPDIR/callarg.con" <<'EOF'
mod m {
    fn takes(v: i32) -> i32 { return v; }
    fn main() -> Int { return takes(2000000000 + 2000000000) as Int; }
}
EOF
both_trap "call arg adopts param width i32 (overflow traps in both)" "$TMPDIR/callarg.con"

cat > "$TMPDIR/callclean.con" <<'EOF'
mod m {
    fn takes(v: i32) -> i32 { return v; }
    fn main() -> Int { return takes(40 + 2) as Int; }
}
EOF
agree "call arg flexible binop : i32" "$TMPDIR/callclean.con" "42"

# Array literal element: a flexible binop element adopts the array element width.
cat > "$TMPDIR/arr.con" <<'EOF'
mod m { fn main() -> Int {
    let a: [i32; 2] = [2000000000 + 2000000000, 0];
    return a[0] as Int;
} }
EOF
both_trap "array-literal element adopts element width i32 (overflow traps in both)" "$TMPDIR/arr.con"

cat > "$TMPDIR/arrclean.con" <<'EOF'
mod m { fn main() -> Int {
    let a: [i32; 2] = [40 + 2, 0];
    return a[0] as Int;
} }
EOF
agree "array-literal element flexible binop : i32" "$TMPDIR/arrclean.con" "42"

echo "=== generic instantiation: Check and Elab infer the same type args ==="

# Type-arg inference is single-sourced (Shared.unifyTypes, InstantiationJudgment
# axis). A turbofish generic call must produce the same value under interp and
# compiled — if Check and Elab inferred/stamped different type args the compiled
# specialization would diverge or fail.
cat > "$TMPDIR/gen.con" <<'EOF'
mod m {
    fn id<T>(x: T) -> T { return x; }
    fn main() -> Int {
        let v: i32 = id::<i32>(42);
        return v as Int;
    }
}
EOF
agree "id::<i32>(42) turbofish generic" "$TMPDIR/gen.con" "42"

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
