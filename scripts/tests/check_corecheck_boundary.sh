#!/usr/bin/env bash
# CoreCheck / pre-Lower boundary gate (Phase 6.5 #4).
#
# The pipeline is Check -> Elab -> CoreCheck -> Mono (+ verifyPostMono) -> Lower.
# Lower and the backend assume their input is free of frontend/mono/type-policy
# residue. This gate is the END-TO-END proof of that boundary: for each residue
# class, a user-writable program that exhibits it must be REJECTED before a
# binary is produced (i.e. `concrete -o` fails). If any class ever reaches Lower
# and compiles, that is a boundary hole — a miscompile risk — and fails here.
#
# Where each class is currently enforced (the audit — see docs/COMPILER_BOUNDARY.md):
#   1. unresolved type vars after mono   -> verifyPostMono (verifyNoTypeVars)   [post-mono boundary]
#   2. illegal Copy specialization       -> verifyPostMono (verifyCopyFields)   [post-mono boundary]
#   3. mixed-width binops (E0228/E0715)  -> Check                                [front-end; not re-asserted post-mono]
#   4. capability misuse                 -> CoreCheck (core-check)               [Core boundary]
#   5. unsafe op without capability      -> CoreCheck (core-check)               [Core boundary]
#   6. returning a second-class reference -> Check                               [front-end]
# Classes 4/5 are enforced at the CoreCheck boundary; 3/6 are caught once in
# Check. Migrating 3/6 to explicit post-mono re-assertion (defense-in-depth) is
# the staged follow-up. This gate guarantees the OBSERVABLE property regardless
# of which stage rejects: no residue class compiles to a binary.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
TMPDIR=$(mktemp -d)
trap 'rm -rf "$TMPDIR"' EXIT
PASS=0; FAIL=0
emit(){ printf '%s\n' "$2" > "$TMPDIR/$1.con"; }

# rejected <label> <name>: `concrete -o` must FAIL (residue never reaches a binary).
rejected(){ local label="$1" F="$TMPDIR/$2.con"
  local O RC; O="$("$COMPILER" "$F" -o "$F.bin" 2>&1)"; RC=$?
  if [ $RC -ne 0 ]; then
    local where; where="$(printf '%s' "$O" | grep -oE 'error\[[a-z-]+\]' | head -1)"
    echo "  ok   $label (rejected ${where:-error})"; PASS=$((PASS+1))
  else echo "  FAIL $label — REACHED LOWER and compiled (boundary hole!)"; FAIL=$((FAIL+1)); fi; }

echo "=== residue classes must be rejected before Lower ==="

emit mixedwidth 'mod m { fn main() -> Int { let a: i8 = 1; let b: i32 = 2; let c = a + b; return 0; } }'
rejected "class 3: mixed-width binop (i8 + i32)" mixedwidth

emit copyspec 'mod m { struct Copy Box<T> { v: T } fn main() -> Int { let b: Box<String> = Box::<String> { v: "x" }; return 0; } }'
rejected "class 2: illegal Copy specialization (Box<String>)" copyspec

emit unsafeptr 'mod m { fn main() -> Int { let x: i32 = 5; let p: *const i32 = &x as *const i32; let v: i32 = *p; return v as Int; } }'
rejected "class 5: unsafe raw-ptr deref in a safe fn" unsafeptr

emit capmisuse 'mod m { fn needs() with(File) -> Int { return 0; } fn main() -> Int { return needs(); } }'
rejected "class 4: calling a File-capability fn from a pure fn" capmisuse

emit retref 'mod m { fn bad(x: i32) -> &i32 { return &x; } fn main() -> Int { return 0; } }'
rejected "class 6: returning a second-class reference" retref

echo
echo "check_corecheck_boundary: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
