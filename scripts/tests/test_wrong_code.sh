#!/usr/bin/env bash
# Phase D wrong-code regression corpus runner.
#
# Reads tests/wrong-code/manifest.toml and asserts every fixed case
# still behaves as expected. Open cases are reported but do not affect
# the run's exit code unless --include-open --strict is passed.
#
# Contract: docs/WRONG_CODE_CORPUS.md

set -uo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

COMPILER=".lake/build/bin/concrete"
MANIFEST="tests/wrong-code/manifest.toml"

INCLUDE_OPEN=0
STRICT_OPEN=0
while [ $# -gt 0 ]; do
  case "$1" in
    --include-open) INCLUDE_OPEN=1; shift ;;
    --strict)       STRICT_OPEN=1; shift ;;
    -h|--help)
      cat <<USAGE
Usage: $0 [--include-open] [--strict]

  --include-open   also probe status="open" cases (default: skip)
  --strict         (with --include-open) treat unexpected open-case
                   results as run failures, not just informational
USAGE
      exit 0
      ;;
    *) echo "unknown flag: $1" >&2; exit 2 ;;
  esac
done

if [ ! -x "$COMPILER" ]; then
  echo "error: compiler not found at $COMPILER. Run 'make build' first." >&2
  exit 2
fi
if [ ! -f "$MANIFEST" ]; then
  echo "error: manifest $MANIFEST missing." >&2
  exit 2
fi

TMPDIR=$(mktemp -d)
trap 'rm -rf "$TMPDIR"' EXIT

# Parse the manifest into a flat record stream. We avoid pulling in a
# TOML library so the script has no extra deps; the manifest format is
# small and stable enough that this regex parse is reliable.
python3 - "$MANIFEST" > "$TMPDIR/cases.tsv" <<'PYEOF'
import re, sys
src = open(sys.argv[1]).read()
# Split on case-table headers; first chunk is preamble.
chunks = re.split(r'^\[\[case\]\]\s*$', src, flags=re.MULTILINE)[1:]

def unescape(s):
    # Minimal TOML basic-string escape handling (\n, \t, \\, \").
    return (s.replace('\\\\', '\x00')
             .replace('\\n', '\n')
             .replace('\\t', '\t')
             .replace('\\"', '"')
             .replace('\x00', '\\'))

for c in chunks:
    fields = {}
    for m in re.finditer(r'^\s*(\w+)\s*=\s*"((?:\\.|[^"\\])*)"\s*$', c, flags=re.MULTILINE):
        fields[m.group(1)] = unescape(m.group(2))
    cols = [
        fields.get('id', ''),
        fields.get('category', ''),
        fields.get('status', ''),
        fields.get('repro', ''),
        fields.get('kind', ''),
        fields.get('expected', ''),
        fields.get('notes', ''),
    ]
    # TSV cannot carry literal tabs/newlines in fields; encode as escapes
    # for transport and decode in bash via printf '%b'.
    cols = [c.replace('\\', '\\\\').replace('\t', '\\t').replace('\n', '\\n') for c in cols]
    print('\t'.join(cols))
PYEOF

PASS=0
FAIL=0
SKIP_OPEN=0
OPEN_PASS=0
OPEN_FAIL=0
ANY_FAIL=0

run_runtime_case() {
  local id="$1" repro="$2" expected="$3" status="$4"
  local bin="$TMPDIR/$id"
  if ! "$COMPILER" "$repro" -o "$bin" >"$TMPDIR/$id.cerr" 2>&1; then
    if [ "$status" = "fixed" ]; then
      echo "FAIL $id ($repro) — compilation failed"
      sed 's/^/  /' < "$TMPDIR/$id.cerr" | head -3
      FAIL=$((FAIL + 1))
      return
    else
      echo "open-fail $id ($repro) — compile-error in runtime case (open)"
      OPEN_FAIL=$((OPEN_FAIL + 1))
      return
    fi
  fi
  local actual
  actual=$("$bin" 2>/dev/null) || true
  actual=$(printf '%s' "$actual" | sed -e 's/[[:space:]]*$//')
  if [ "$actual" = "$expected" ]; then
    if [ "$status" = "fixed" ]; then
      PASS=$((PASS + 1))
      echo "  ok   $id ($repro) → $expected"
    else
      OPEN_PASS=$((OPEN_PASS + 1))
      echo "  open-PASS $id ($repro) → $expected (open case unexpectedly passes; consider flipping to fixed)"
    fi
  else
    if [ "$status" = "fixed" ]; then
      echo "FAIL $id ($repro) — expected '$expected' got '$actual'"
      FAIL=$((FAIL + 1))
    else
      echo "  open $id ($repro) — still failing as expected (got '$actual')"
      OPEN_PASS=$((OPEN_PASS + 1))
    fi
  fi
}

run_compile_error_case() {
  local id="$1" repro="$2" expected="$3" status="$4"
  local bin="$TMPDIR/$id"
  local cerr="$TMPDIR/$id.cerr"
  "$COMPILER" "$repro" -o "$bin" > "$cerr" 2>&1 && rc=0 || rc=$?
  # We expect a compile error; check that the error code substring is present.
  if grep -q "$expected" "$cerr"; then
    if [ "$status" = "fixed" ]; then
      echo "  ok   $id ($repro) → expected $expected raised"
      PASS=$((PASS + 1))
    else
      echo "  open $id ($repro) → still raises $expected as expected"
      OPEN_PASS=$((OPEN_PASS + 1))
    fi
  else
    if [ "$status" = "fixed" ]; then
      echo "FAIL $id ($repro) — expected $expected, compiler exit=$rc"
      sed 's/^/  /' < "$cerr" | head -3
      FAIL=$((FAIL + 1))
    else
      # The expected error is no longer raised — the compiler accepts the
      # program (or rejects with a different code). This means the bug
      # may have been silently fixed.
      echo "  open-PASS $id ($repro) — expected $expected but compiler exit=$rc (silent fix? flip to fixed)"
      OPEN_PASS=$((OPEN_PASS + 1))
    fi
  fi
}

while IFS=$'\t' read -r id category status repro kind expected notes; do
  [ -z "$id" ] && continue
  # Decode escapes that the python parser added so newlines / tabs in
  # `expected` survive the TSV transport.
  expected=$(printf '%b' "$expected")

  # Manifest invariants
  if [ ! -e "$repro" ]; then
    echo "FAIL $id — repro path missing: $repro"
    FAIL=$((FAIL + 1))
    continue
  fi
  if [ -n "$notes" ] && [ ! -e "$notes" ]; then
    echo "FAIL $id — notes path missing: $notes"
    FAIL=$((FAIL + 1))
    continue
  fi

  if [ "$status" != "fixed" ] && [ "$INCLUDE_OPEN" -ne 1 ]; then
    SKIP_OPEN=$((SKIP_OPEN + 1))
    continue
  fi

  case "$kind" in
    runtime)        run_runtime_case "$id" "$repro" "$expected" "$status" ;;
    compile-error)  run_compile_error_case "$id" "$repro" "$expected" "$status" ;;
    *)
      echo "FAIL $id — unsupported kind: $kind"
      FAIL=$((FAIL + 1))
      ;;
  esac
done < "$TMPDIR/cases.tsv"

echo ""
if [ "$INCLUDE_OPEN" -eq 1 ]; then
  echo "WRONG-CODE: PASS=$PASS  FAIL=$FAIL  OPEN_PASS=$OPEN_PASS  OPEN_FAIL=$OPEN_FAIL"
else
  echo "WRONG-CODE: PASS=$PASS  FAIL=$FAIL  OPEN_SKIPPED=$SKIP_OPEN"
fi

if [ "$FAIL" -gt 0 ]; then
  exit 1
fi
if [ "$STRICT_OPEN" -eq 1 ] && [ "$OPEN_FAIL" -gt 0 ]; then
  exit 1
fi
exit 0
