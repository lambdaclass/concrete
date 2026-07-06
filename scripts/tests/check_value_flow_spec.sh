#!/usr/bin/env bash
# Value-flow constructor-coverage gate (ROADMAP 13c / 13e prevention program).
#
# Every surface AST constructor (Expr / MatchArm / Stmt in Concrete/AST.lean)
# must have a row in docs/VALUE_FLOW_SPEC.md declaring its value-flow behavior
# (creates / moves / borrows / copies / overwrites / rejects) and naming the
# gate that locks it. The H13-H17 sweep existed because value flow is
# distributed across syntax handlers and new forms kept landing without a
# conservation story — `break (value)` had no row anywhere, so its missing
# consume hid for weeks. This gate makes that impossible: a new constructor
# fails CI until the spec has its row (and the row's gate has a fixture).
#
# Extraction is deliberately dumb (grep-level) so it cannot drift from the
# source: constructor names are the `| name` alternatives between each
# `inductive X where` header and the next `inductive`/`end`.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
AST="Concrete/AST.lean"
SPEC="docs/VALUE_FLOW_SPEC.md"
[ -f "$AST" ] || { echo "error: $AST missing" >&2; exit 2; }
[ -f "$SPEC" ] || { echo "error: $SPEC missing — write the value-flow spec (ROADMAP 13c)" >&2; exit 1; }

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# Extract constructor names of one inductive from AST.lean.
constructors() {
  awk -v target="$1" '
    $0 ~ "^inductive " target " where" { grab=1; next }
    grab && /^inductive |^end$|^def / { grab=0 }
    grab {
      # match: | name (args) — strip comments first
      line=$0; sub(/--.*/, "", line)
      while (match(line, /\|[[:space:]]*[A-Za-z_][A-Za-z0-9_]*/)) {
        piece=substr(line, RSTART, RLENGTH)
        sub(/\|[[:space:]]*/, "", piece)
        print piece
        line=substr(line, RSTART+RLENGTH)
      }
    }' "$AST"
}

check_inductive() {
  local ind="$1"
  local names; names="$(constructors "$ind")"
  if [ -z "$names" ]; then no "$ind: extracted no constructors (parser drift?)"; return; fi
  local missing=0
  while IFS= read -r c; do
    # a row mentions the constructor as `name` (backtick-quoted)
    if ! grep -q "\`$c\`" "$SPEC"; then
      no "$ind.$c has no row in docs/VALUE_FLOW_SPEC.md — declare its value flow before landing"
      missing=1
    fi
  done <<< "$names"
  if [ "$missing" -eq 0 ]; then
    ok "$ind: all $(wc -l <<< "$names" | tr -d ' ') constructors have value-flow rows"
  fi
}

echo "=== every surface AST constructor has a value-flow row ==="
check_inductive Expr
check_inductive MatchArm
check_inductive Stmt

echo ""
echo "VALUE-FLOW-SPEC: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
