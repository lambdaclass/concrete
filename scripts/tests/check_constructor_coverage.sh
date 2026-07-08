#!/usr/bin/env bash
# Constructor-coverage gate (pipeline refactor #3).
#
# The compiler has many independent recursive walkers (checker, elaborator,
# monomorphizer, core-checker, proof extraction, interpreter, lowering,
# formatter). Every new AST/Core expression constructor risks ONE of those
# walkers silently not handling it — a wildcard `| _ =>` swallows it, and the
# miss only surfaces as a runtime wrong-answer much later. This is the
# compiler-pipeline analogue of the feature-interaction checklist.
#
# This gate makes "every constructor is handled by every walker that must handle
# it" a mechanically-checked invariant:
#
#   - the surface `Expr` constructors are extracted from Frontend/AST.lean and
#     each must appear explicitly (`.ctor`) in every FRONTEND pass;
#   - the core `CExpr` constructors are extracted from Elab/Core.lean and each
#     must appear explicitly in every CORE pass.
#
# The constructor lists are read from the source, not hard-coded, so adding a
# constructor and forgetting to teach a walker about it FAILS this gate. The
# four surface-only forms (methodCall / staticMethodCall / arrowAccess / paren)
# are desugared in Resolve/Elab and correctly do not exist as CExpr — that is
# why the two matrices are checked against different constructor lists.
#
# A pass "handles" a constructor iff the token `.<ctor>` occurs in its file.
# Today every cell is covered; this gate locks that in. It greps source only
# (no compiler build required) and lives in the grammar CI job.

set -euo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

PASS=0
FAIL=0
ok() { echo "  ok   $1"; PASS=$((PASS+1)); }
no() { echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# Extract the `| ctorName` heads of an inductive block from a Lean file.
# $1 = file, $2 = inductive name.
ctors_of() {
  awk -v want="inductive $2 where" '
    $0 ~ want {f=1; next}
    f && /^(inductive|structure|def|abbrev|end|namespace)/ {exit}
    f && /^[[:space:]]*\|/ {print}
  ' "$1" | grep -oE "\| [a-z][A-Za-z0-9_]+" | sed 's/| //' | sort -u
}

# Assert every constructor in $3.. appears in file $1 (pass label $2).
covers() {
  local file="$1" label="$2"; shift 2
  local missing=""
  for c in "$@"; do
    grep -qE "\.${c}([^A-Za-z0-9_]|\$)" "$file" || missing="$missing $c"
  done
  if [ -z "$missing" ]; then ok "$label handles all ${#} constructors"
  else no "$label is MISSING:$missing  (add an explicit arm in $file)"; fi
}

mapfile -t EXPR_CTORS  < <(ctors_of Concrete/Frontend/AST.lean Expr)
mapfile -t CEXPR_CTORS < <(ctors_of Concrete/Elab/Core.lean CExpr)

echo "=== surface Expr (${#EXPR_CTORS[@]} constructors) handled by every frontend pass ==="
[ "${#EXPR_CTORS[@]}" -ge 20 ] || no "extracted only ${#EXPR_CTORS[@]} Expr constructors — parser broken?"
covers Concrete/Check/Check.lean     "checker (Check)"     "${EXPR_CTORS[@]}"
covers Concrete/Elab/Elab.lean       "elaborator (Elab)"   "${EXPR_CTORS[@]}"
covers Concrete/Frontend/Format.lean "formatter (Format)"  "${EXPR_CTORS[@]}"

echo ""
echo "=== core CExpr (${#CEXPR_CTORS[@]} constructors) handled by every core pass ==="
[ "${#CEXPR_CTORS[@]}" -ge 20 ] || no "extracted only ${#CEXPR_CTORS[@]} CExpr constructors — parser broken?"
covers Concrete/IR/Lower.lean         "lowering (Lower)"       "${CEXPR_CTORS[@]}"
covers Concrete/IR/Mono.lean          "mono (Mono)"            "${CEXPR_CTORS[@]}"
covers Concrete/Check/CoreCheck.lean  "core-check (CoreCheck)" "${CEXPR_CTORS[@]}"
covers Concrete/Interp/Interp.lean    "interpreter (Interp)"   "${CEXPR_CTORS[@]}"
covers Concrete/Proof/ProofCore.lean  "proof extraction"       "${CEXPR_CTORS[@]}"

echo ""
echo "CONSTRUCTOR-COVERAGE: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
