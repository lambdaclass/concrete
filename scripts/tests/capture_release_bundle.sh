#!/usr/bin/env bash
# Release evidence bundle (Phase 7/8 surface).
#
# Captures everything needed to reproduce and audit a SUCCESSFUL
# example: source, full report set, proof status, assumption file,
# policy file, AUDIT, CATCHES, README, snapshots, compiler version.
# Sibling to scripts/tests/capture_wrong_code_bundle.sh — that one
# is for failing programs; this one is for showcase / release
# evidence.
#
# Usage:
#   capture_release_bundle.sh <example-dir> [-o <out-dir>]
#
# Default output: out/release/<example-name>/

set -uo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

COMPILER=".lake/build/bin/concrete"

usage() {
  cat <<'USAGE'
Usage: capture_release_bundle.sh <example-dir> [-o <out-dir>]

  <example-dir>  e.g. examples/parse_validate
  -o <out-dir>   Defaults to out/release/<basename>

Captures source, full report set, proof-registry, assumption/policy
files, AUDIT/CATCHES/README, snapshot baselines, and compiler
version into a stable layout suitable for release evidence review.

Contract: docs/RELEASE_BUNDLE.md
USAGE
}

if [ $# -lt 1 ]; then usage; exit 1; fi
case "$1" in -h|--help) usage; exit 0 ;; esac

EXAMPLE_DIR="$1"; shift
OUT=""
while [ $# -gt 0 ]; do
  case "$1" in
    -o)        OUT="$2"; shift 2 ;;
    -h|--help) usage; exit 0 ;;
    *) echo "unknown flag: $1" >&2; exit 2 ;;
  esac
done

[ -d "$EXAMPLE_DIR" ] || { echo "error: $EXAMPLE_DIR not a directory" >&2; exit 2; }
[ -x "$COMPILER" ] || { echo "error: $COMPILER missing. Run 'make build'." >&2; exit 2; }

EXAMPLE_NAME=$(basename "$EXAMPLE_DIR")
OUT="${OUT:-out/release/$EXAMPLE_NAME}"
SOURCE="$EXAMPLE_DIR/src/main.con"

[ -f "$SOURCE" ] || { echo "error: $SOURCE not found" >&2; exit 2; }

mkdir -p "$OUT/source" "$OUT/reports" "$OUT/snapshots" "$OUT/catches"

# Verify the example actually compiles and runs cleanly. A release
# bundle must not be captured on a broken example.
TMP_BIN=$(mktemp)
trap 'rm -f "$TMP_BIN"' EXIT
if ! "$COMPILER" "$SOURCE" -o "$TMP_BIN" >/dev/null 2>&1; then
  echo "error: $SOURCE does not compile cleanly. Refusing to capture a release bundle." >&2
  exit 1
fi
RUNTIME_STDOUT=$("$TMP_BIN" 2>/dev/null || true)

# --- Source files (canonical name + original basename) ---
SRC_BASE=$(basename "$SOURCE")
cp "$SOURCE" "$OUT/source/program.con"
[ "$SRC_BASE" != "program.con" ] && cp "$SOURCE" "$OUT/source/$SRC_BASE"

# --- Project config (Concrete.toml, assumptions.toml, proof-registry) ---
for f in Concrete.toml assumptions.toml; do
  if [ -f "$EXAMPLE_DIR/$f" ]; then
    cp "$EXAMPLE_DIR/$f" "$OUT/$f"
  fi
done
if [ -f "$EXAMPLE_DIR/src/proof-registry.json" ]; then
  cp "$EXAMPLE_DIR/src/proof-registry.json" "$OUT/source/proof-registry.json"
fi

# --- Narrative docs (AUDIT, CATCHES, README) ---
for f in AUDIT.md CATCHES.md README.md; do
  if [ -f "$EXAMPLE_DIR/$f" ]; then
    cp "$EXAMPLE_DIR/$f" "$OUT/$f"
  fi
done

# --- Negative pair (catches/*) ---
if [ -d "$EXAMPLE_DIR/catches" ]; then
  for case in "$EXAMPLE_DIR/catches"/*.con; do
    [ -f "$case" ] && cp "$case" "$OUT/catches/$(basename "$case")"
  done
fi

# --- Full report set (every report kind that runs cleanly) ---
for kind in caps unsafe layout interface alloc mono authority proof \
            eligibility proof-status obligations stack-depth fingerprints \
            effects recursion consistency verify contracts vcs audit; do
  out_file="$OUT/reports/$kind.txt"
  if "$COMPILER" "$SOURCE" --report "$kind" > "$out_file" 2>&1; then
    :
  else
    rm -f "$out_file"
  fi
done

# --- Snapshot baselines (if present) ---
if [ -d "$EXAMPLE_DIR/snapshot" ]; then
  for snap in "$EXAMPLE_DIR/snapshot"/*.txt; do
    [ -f "$snap" ] && cp "$snap" "$OUT/snapshots/$(basename "$snap")"
  done
fi

# --- Runtime evidence: stdout of the example ---
echo "$RUNTIME_STDOUT" > "$OUT/runtime-stdout.txt"

# --- Compiler identity ---
"$COMPILER" --version > "$OUT/compiler-version.txt" 2>&1 || echo "unknown" > "$OUT/compiler-version.txt"

# --- Manifest ---
PROVED_COUNT=$(grep -c '✓' "$OUT/reports/proof-status.txt" 2>/dev/null || echo 0)
STACK_MAX=$(grep -oE 'Max stack bound:\s*[0-9]+' "$OUT/reports/stack-depth.txt" 2>/dev/null | grep -oE '[0-9]+' | head -1 || echo "?")
HAS_ASSUMPTIONS=$([ -f "$OUT/assumptions.toml" ] && echo true || echo false)
HAS_POLICY=$(grep -q '^\[policy\]' "$OUT/Concrete.toml" 2>/dev/null && echo true || echo false)
HAS_CATCHES=$([ -d "$OUT/catches" ] && [ -n "$(ls -A "$OUT/catches" 2>/dev/null)" ] && echo true || echo false)
HAS_SNAPSHOTS=$([ -d "$OUT/snapshots" ] && [ -n "$(ls -A "$OUT/snapshots" 2>/dev/null)" ] && echo true || echo false)

cat > "$OUT/manifest.json" <<EOF
{
  "version": 1,
  "example": "$EXAMPLE_NAME",
  "source": "source/program.con",
  "runtime_stdout": "$(echo "$RUNTIME_STDOUT" | head -1 | tr -d '"')",
  "compiler": "$(cat "$OUT/compiler-version.txt" | head -1 | tr -d '"')",
  "evidence": {
    "proved_functions": $PROVED_COUNT,
    "max_stack_bytes": $STACK_MAX,
    "has_assumptions": $HAS_ASSUMPTIONS,
    "has_policy": $HAS_POLICY,
    "has_negative_pair": $HAS_CATCHES,
    "has_snapshots": $HAS_SNAPSHOTS
  },
  "captured_with": "scripts/tests/capture_release_bundle.sh"
}
EOF

echo "Release bundle written: $OUT"
echo "  proved functions: $PROVED_COUNT"
echo "  max stack: $STACK_MAX bytes"
echo "  assumptions: $HAS_ASSUMPTIONS  policy: $HAS_POLICY  catches: $HAS_CATCHES  snapshots: $HAS_SNAPSHOTS"
ls "$OUT" | sed 's/^/  /'
