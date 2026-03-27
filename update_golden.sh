#!/bin/bash
# Regenerate golden test baselines for --emit-core, --emit-ssa, and --fmt
set -e

COMPILER=".lake/build/bin/concrete"
SRC_DIR="golden_tests/src"
CORE_DIR="golden_tests/core"
SSA_DIR="golden_tests/ssa"
FMT_DIR="golden_tests/fmt"

if [ ! -x "$COMPILER" ]; then
  echo "Error: compiler not found at $COMPILER. Run 'lake build' first."
  exit 1
fi

mkdir -p "$CORE_DIR" "$SSA_DIR" "$FMT_DIR"

count=0
for src in "$SRC_DIR"/*.con; do
  name=$(basename "$src" .con)
  echo "Updating: $name"

  # Generate Core IR baseline
  if "$COMPILER" "$src" --emit-core > "$CORE_DIR/$name.expected" 2>&1; then
    : # success
  else
    echo "  WARN: --emit-core failed for $name (saved error output)"
  fi

  # Generate SSA IR baseline
  if "$COMPILER" "$src" --emit-ssa > "$SSA_DIR/$name.expected" 2>&1; then
    : # success
  else
    echo "  WARN: --emit-ssa failed for $name (saved error output)"
  fi

  # Generate Formatter baseline (only for fmt_* sources)
  case "$name" in
    fmt_*)
      if "$COMPILER" "$src" --fmt > "$FMT_DIR/$name.expected" 2>&1; then
        : # success
      else
        echo "  WARN: --fmt failed for $name (saved error output)"
      fi
      ;;
  esac

  count=$((count + 1))
done

echo "Updated $count golden test baselines."
