#!/usr/bin/env bash
# Capture a wrong-code corpus bundle from a single failing source.
#
# Wraps `concrete debug-bundle` to produce the structural artifacts
# (source/, diagnostics, IR dumps where they ran cleanly), then layers
# on the corpus-specific files: command.txt, predicate.txt,
# compiler-version.txt, stdout.txt, stderr.txt, and any reports that
# run cleanly.
#
# Contract: docs/BUG_BUNDLE.md
#
# Usage:
#   capture_wrong_code_bundle.sh <source.con> --case WC-NNNN --predicate <KIND>:<ARG> [-o out/<dir>]
#
# Default output: out/<CASE>/ (relative to repo root).

set -uo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

COMPILER=".lake/build/bin/concrete"

usage() {
  cat <<'USAGE'
Usage: capture_wrong_code_bundle.sh <source.con> --case <ID> --predicate <KIND>:<ARG> [-o <out>]

Required:
  --case <ID>         e.g. WC-0023 — recorded in manifest and used as
                      default output directory name.
  --predicate <P>     The predicate that surfaced the bug. Recorded
                      verbatim in predicate.txt; format matches the
                      reducer (error-code:E0708, runtime-output:42,
                      oracle-mismatch, report-contains:caps:Alloc).

Output: out/<CASE>/ unless -o overrides.
USAGE
}

if [ $# -lt 1 ]; then usage; exit 1; fi
case "$1" in -h|--help) usage; exit 0 ;; esac

SOURCE="$1"; shift
CASE_ID=""
PREDICATE=""
OUT=""
while [ $# -gt 0 ]; do
  case "$1" in
    --case)      CASE_ID="$2"; shift 2 ;;
    --predicate) PREDICATE="$2"; shift 2 ;;
    -o)          OUT="$2"; shift 2 ;;
    -h|--help)   usage; exit 0 ;;
    *) echo "unknown flag: $1" >&2; exit 2 ;;
  esac
done

if [ -z "$CASE_ID" ];   then echo "error: --case is required" >&2; exit 2; fi
if [ -z "$PREDICATE" ]; then echo "error: --predicate is required" >&2; exit 2; fi
if [ ! -f "$SOURCE" ];  then echo "error: source not found: $SOURCE" >&2; exit 2; fi
if [ ! -x "$COMPILER" ]; then
  echo "error: compiler not found at $COMPILER. Run 'make build' first." >&2
  exit 2
fi

OUT="${OUT:-out/$CASE_ID}"
mkdir -p "$OUT"

# Step 1: run `concrete debug-bundle` for the structural skeleton.
"$COMPILER" debug-bundle "$SOURCE" -o "$OUT" >/dev/null 2>&1 || true

# Step 2: corpus-specific overlay files.
SRC_BASENAME=$(basename "$SOURCE")
mkdir -p "$OUT/source"
# debug-bundle already copied the source. Normalize the canonical
# filename so consumers can rely on `source/program.con`.
if [ -f "$OUT/source/$SRC_BASENAME" ] && [ "$SRC_BASENAME" != "program.con" ]; then
  cp "$OUT/source/$SRC_BASENAME" "$OUT/source/program.con"
elif [ ! -f "$OUT/source/program.con" ]; then
  cp "$SOURCE" "$OUT/source/program.con"
fi

# command.txt — the invocation that surfaced the bug.
cat > "$OUT/command.txt" <<EOF
$COMPILER "$SOURCE" -o /tmp/bug_bin
EOF

# predicate.txt — verbatim predicate used to identify the bug.
echo "$PREDICATE" > "$OUT/predicate.txt"

# compiler-version.txt — pinned version banner.
"$COMPILER" --version > "$OUT/compiler-version.txt" 2>&1 || echo "unknown" > "$OUT/compiler-version.txt"

# stdout.txt / stderr.txt — re-run the compile and capture both streams
# separately. Distinguish from diagnostics.txt, which is the Lean-side
# rendered diagnostics list (already produced by debug-bundle when
# something failed).
"$COMPILER" "$SOURCE" -o "$OUT/.bug_bin_tmp" > "$OUT/stdout.txt" 2> "$OUT/stderr.txt" || true
rm -f "$OUT/.bug_bin_tmp"

# ssa-unverified.txt — useful when failed_at == "lower" with a verifier
# rejection. Always attempt; remove if the dump is empty / the command
# errored without output.
SSA_UNVERIFIED=$("$COMPILER" "$SOURCE" --emit-ssa-unverified 2>/dev/null || true)
if [ -n "$SSA_UNVERIFIED" ]; then
  printf '%s\n' "$SSA_UNVERIFIED" > "$OUT/ssa-unverified.txt"
fi

# reports/ — try each report kind; include ones that run cleanly (exit 0
# AND produce output). Reports that error are skipped silently.
REPORT_DIR="$OUT/reports"
mkdir -p "$REPORT_DIR"
for kind in caps unsafe layout alloc authority effects recursion stack-depth fingerprints; do
  REPORT=$("$COMPILER" "$SOURCE" --report "$kind" 2>/dev/null)
  rc=$?
  if [ $rc -eq 0 ] && [ -n "$REPORT" ]; then
    printf '%s\n' "$REPORT" > "$REPORT_DIR/$kind.txt"
  fi
done
# If no reports landed, drop the directory rather than ship an empty bundle.
if [ -z "$(ls -A "$REPORT_DIR" 2>/dev/null)" ]; then
  rmdir "$REPORT_DIR"
fi

# bundle.json — corpus-specific metadata (sibling to the existing
# manifest.json, which captures the structural pipeline state).
LINE_COUNT=$(wc -l < "$SOURCE" | tr -d ' ')
cat > "$OUT/bundle.json" <<EOF
{
  "case_id": "$CASE_ID",
  "predicate": "$PREDICATE",
  "source_basename": "$SRC_BASENAME",
  "source_lines": $LINE_COUNT,
  "captured_with": "scripts/tests/capture_wrong_code_bundle.sh"
}
EOF

echo "Bundle written: $OUT"
ls "$OUT" | sort | sed 's/^/  /'
