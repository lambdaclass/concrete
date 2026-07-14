#!/usr/bin/env bash
# SSA-verify behavior-preservation gate.
#
# SSAVerify is a TRUST pass — it decides whether lowered SSA is well-formed. Any
# change to its dominator / predecessor / use-def machinery (e.g. the Gauss-Seidel
# dominator convergence fix, or the future idom/CHK near-linear rewrite) must
# preserve VERDICTS: a valid program still compiles (verify accepts) and produces
# the same runtime value on the interpreter and the compiled binary; and a program
# that violates SSA form is still REJECTED. A faster dominator calc that accepts or
# rejects a different set of programs is a soundness regression disguised as a
# speedup — the complexity guard proves it's fast; only THIS gate proves it's still
# correct. It is a hard merge blocker for SSAVerify changes.
#
# The corpus stresses exactly the CFG shapes dominator computation depends on:
# deep if/else chains, wide match, nested loops with breaks/continues, diamonds,
# and value-bearing control flow (where phi-operand dominance matters).

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
emit(){ printf '%s\n' "$2" > "$TMPDIR/$1.con"; }

# accepts_val <label> <name> <expected>: verify ACCEPTS the SSA, and interp ==
# compiled == <expected> (verdict = accept, and codegen stays correct).
accepts_val(){ local label="$1" F="$TMPDIR/$2.con" want="$3" I C
  I="$("$COMPILER" "$F" --interp 2>&1)"
  if "$COMPILER" "$F" -o "$F.bin" >/dev/null 2>&1; then C="$("$F.bin" 2>&1)"; else C="<verify-rejected-or-compile-fail>"; fi
  if [ "$I" = "$want" ] && [ "$C" = "$want" ]; then ok "$label (accept, => $want)"
  else no "$label (want $want; interp=$(printf '%s' "$I"|head -1) compiled=$(printf '%s' "$C"|head -1))"; fi; }

echo "=== SSAVerify still ACCEPTS valid complex CFGs, codegen correct ==="

# Deep if/else chain — a long dominator chain (the near-worst case for dominators).
emit ifchain 'mod m { fn main() -> Int { let mut a: Int = 0;
  if a==0 { a=1; } else { a=2; }
  if a==1 { a=a+10; } else { a=a+20; }
  if a==11 { a=a+100; } else { a=a+200; }
  if a>50 { a=a+1000; } else { a=a+2000; }
  return a; } }'
accepts_val "deep if/else chain" ifchain "1111"

# Wide match — many blocks + a wide merge phi (the shape that exposed the cubic).
emit widematch 'mod m { fn main() -> Int { let x: Int = 7; let r: Int = match x {
  0 => 0, 1 => 10, 2 => 20, 3 => 30, 4 => 40, 5 => 50, 6 => 60, 7 => 70, 8 => 80, _ => 99 };
  return r; } }'
accepts_val "wide match merge phi" widematch "70"

# Nested loops with break/continue + a post-loop use (loop-header/exit dominance).
emit loops 'mod m { fn main() -> Int { let mut sum: Int = 0; let mut i: Int = 0;
  while i < 5 { i = i + 1; let mut j: Int = 0;
    while j < 5 { j = j + 1; if j == 3 { continue; } sum = sum + 1; } }
  return sum; } }'
accepts_val "nested loops break/continue" loops "20"

# Diamond CFG feeding a value-bearing if (phi-operand dominance across the merge).
emit diamond 'mod m { fn main() -> Int { let c: Bool = true; let d: Bool = false;
  let x: Int = if c { if d { 1 } else { 2 } } else { 3 };
  let y: Int = if x == 2 { x + 40 } else { 0 };
  return y; } }'
accepts_val "diamond into value-if (phi dominance)" diamond "42"

# Loop result via break (statement form; value while-expr removed in 6D#2) —
# still exercises the loop-header phis + the result slot.
emit whileval 'mod m { fn main() -> Int { let mut i: Int = 0; let mut r: Int = -1;
  while i < 100 { i = i + 1; if i == 9 { r = i * 5; break; } }
  return r; } }'
accepts_val "loop result via break (header phis)" whileval "45"

echo
echo "check_ssa_verify_agreement: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
