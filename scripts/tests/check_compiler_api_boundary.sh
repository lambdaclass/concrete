#!/usr/bin/env bash
# Compiler-internal API boundary gate (ROADMAP Phase 4 #16).
#
# External consumers (editor/LSP, MCP, package tooling, integrations) must depend
# on the small stable BOUNDARY surface, not reach into compiler internals (parser,
# checker, elaborator, report/obligation reconstruction, codegen). This gate scans
# the consumer roots for `.lean` files importing anything outside the boundary
# allowlist and fails if found. It also self-tests against a good/bad fixture pair
# so it can never silently become a no-op (no false greens).
#
# The compiler itself (Main.lean, the Concrete.* modules) is unrestricted — the
# rule applies only to consumer roots.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# The V1 boundary allowlist — must agree with docs/COMPILER_API.md.
BOUNDARY=("Concrete.Project" "Concrete.Pipeline" "Concrete.CompilerLedger" "Concrete.ObligationCore" "Concrete.Diagnostic" "Concrete.DebugBundle")
CONSUMER_ROOTS=("editor" "tools" "integrations" "lsp" "mcp" "plugins")
DOC="docs/COMPILER_API.md"

is_boundary(){ local m="$1"; for b in "${BOUNDARY[@]}"; do [ "$m" = "$b" ] && return 0; done; return 1; }

# scan_file <path> → prints each forbidden Concrete import found (empty = clean).
scan_file(){
  local f="$1"
  # `import Concrete` (bare umbrella) or `import Concrete.X` where X not in allowlist.
  grep -oE "^[[:space:]]*import[[:space:]]+Concrete(\.[A-Za-z0-9_]+)*" "$f" 2>/dev/null \
    | sed -E 's/^[[:space:]]*import[[:space:]]+//' | while read -r mod; do
      if [ "$mod" = "Concrete" ]; then echo "$mod"; elif ! is_boundary "$mod"; then echo "$mod"; fi
    done
}

echo "=== the boundary doc exists and names every allowlisted module ==="
[ -f "$DOC" ] && ok "$DOC present" || no "$DOC missing"
docok=1
for b in "${BOUNDARY[@]}"; do grep -q "\`$b\`" "$DOC" || { docok=0; echo "       doc missing $b"; }; done
[ "$docok" = "1" ] && ok "doc lists every boundary module (doc ↔ gate agree)" || no "doc/gate boundary mismatch"

echo "=== every boundary module exists as a real Concrete module ==="
bmok=1
for b in "${BOUNDARY[@]}"; do test -f "${b//.//}.lean" || { bmok=0; echo "       missing ${b//.//}.lean"; }; done
[ "$bmok" = "1" ] && ok "all boundary modules resolve to files" || no "a boundary module file is missing"

echo "=== consumer roots import only the boundary (no internals, no bare umbrella) ==="
violations=0; scanned=0
for root in "${CONSUMER_ROOTS[@]}"; do
  [ -d "$root" ] || continue
  while IFS= read -r f; do
    scanned=$((scanned+1))
    bad="$(scan_file "$f")"
    if [ -n "$bad" ]; then
      violations=$((violations+1))
      echo "       VIOLATION $f imports: $(printf '%s ' $bad)"
    fi
  done < <(find "$root" -name '*.lean' 2>/dev/null)
done
[ "$violations" = "0" ] && ok "no consumer-root file reaches past the boundary (scanned $scanned .lean files)" \
  || no "$violations consumer file(s) import compiler internals"

echo "=== self-test: the scanner actually detects violations (no false green) ==="
FIX="$(mktemp -d)"
printf 'import Concrete.CompilerLedger\nimport Concrete.Diagnostic\n' > "$FIX/good.lean"
printf 'import Concrete.Parser\nimport Concrete.Report\n'             > "$FIX/bad_internal.lean"
printf 'import Concrete\n'                                            > "$FIX/bad_umbrella.lean"
[ -z "$(scan_file "$FIX/good.lean")" ] && ok "a boundary-only consumer passes the scanner" \
  || no "scanner false-positives on a boundary-only import"
[ -n "$(scan_file "$FIX/bad_internal.lean")" ] && ok "scanner flags an internal import (Parser/Report)" \
  || no "scanner FAILED to flag an internal import — gate is a no-op"
[ -n "$(scan_file "$FIX/bad_umbrella.lean")" ] && ok "scanner flags the bare umbrella import" \
  || no "scanner FAILED to flag the bare umbrella import"

echo "=== a consumer loads a project THROUGH the boundary (Concrete.Project only) ==="
PROBE="scripts/tests/fixtures/api_boundary/load_probe.lean"
[ -f "$PROBE" ] || no "load probe fixture missing"
# static: the probe imports only boundary modules (no internals, no umbrella).
[ -z "$(scan_file "$PROBE")" ] && ok "load_probe imports only boundary modules" \
  || no "load_probe reaches past the boundary: $(scan_file "$PROBE")"
# the boundary actually exposes the loading API.
grep -qE "def loadProject" Concrete/Resolve/Project.lean && grep -qE "partial def findProjectRoot" Concrete/Resolve/Project.lean \
  && grep -qE "structure ProjectContext" Concrete/Resolve/Project.lean \
  && ok "Concrete.Project exposes findProjectRoot / loadProject / ProjectContext" \
  || no "boundary module missing the project-loading API"
# dynamic: compile + run the probe so we prove it actually loads a project (only
# when a Lean toolchain is available — never a false fail on a bare runner).
if command -v lake >/dev/null 2>&1; then
  if lake env lean --run "$PROBE" 2>/dev/null | grep -q "PROBE-OK:"; then
    ok "the probe loads a project at runtime via the boundary (PROBE-OK)"
  else
    no "the probe failed to load a project through the boundary"
  fi
else
  echo "  --   (lake unavailable; skipped the runtime load-probe)"
fi

echo ""
echo "API-BOUNDARY: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
