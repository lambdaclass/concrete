#!/usr/bin/env bash
# Phase 6 13r: doc-snippet compile gate — doc snippets are first-class evidence.
#
# Every fenced ```con / ```concrete block in README.md + docs/*.md must either:
#   - COMPILE            plain `con` / `concrete` fence (front-end bar: --emit-core
#                        — snippets need not define main/link)
#   - REJECT as declared  `con reject:E0xxx` fence (compiler must emit that code)
#   - be marked exempt    `con pseudocode` (illustrative/fragment; not compiled)
# An unmarked block that no longer compiles FAILS the gate (the Zig stale-docs
# lesson: docs drift unless they are executed).

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
TMP=$(mktemp -d); trap 'rm -rf "$TMP"' EXIT

# Extract every con/concrete block into $TMP/NNN.meta + $TMP/NNN.con
python3 - "$TMP" README.md docs/*.md <<'PY'
import sys, re, os
tmp = sys.argv[1]; n = 0
for path in sys.argv[2:]:
    lines = open(path, encoding='utf-8').read().split('\n')
    i = 0
    while i < len(lines):
        m = re.match(r'^```(con|concrete)(\s+(.*))?$', lines[i])
        if not m:
            i += 1; continue
        tag = (m.group(3) or '').strip()
        body, j = [], i + 1
        while j < len(lines) and not lines[j].startswith('```'):
            body.append(lines[j]); j += 1
        n += 1
        open(f"{tmp}/{n:03d}.con", 'w').write('\n'.join(body) + '\n')
        open(f"{tmp}/{n:03d}.meta", 'w').write(f"{path}:{i+1}\t{tag}\n")
        i = j + 1
print(n)
PY

PASS=0; FAIL=0; SKIP=0
for meta in "$TMP"/*.meta; do
  b="${meta%.meta}"
  loc="$(cut -f1 "$meta")"; tag="$(cut -f2 "$meta")"
  case "$tag" in
    pseudocode*) SKIP=$((SKIP+1)); continue ;;
    reject:*)
      code="${tag#reject:}"
      out="$("$COMPILER" "$b.con" --emit-core 2>&1 >/dev/null || true)"
      if grep -q <<<"$out" "$code"; then PASS=$((PASS+1));
      else echo "  FAIL $loc — expected reject $code, got: $(printf '%s' "$out" | head -1)"; FAIL=$((FAIL+1)); fi ;;
    *)
      if "$COMPILER" "$b.con" --emit-core >/dev/null 2>&1; then PASS=$((PASS+1));
      else echo "  FAIL $loc — unmarked snippet no longer compiles (mark \`con pseudocode\` if illustrative)"; FAIL=$((FAIL+1)); fi ;;
  esac
done

echo
echo "DOC-SNIPPETS: PASS=$PASS FAIL=$FAIL SKIPPED=$SKIP"
[ "$FAIL" -eq 0 ]
