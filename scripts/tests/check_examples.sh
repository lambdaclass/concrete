#!/usr/bin/env bash
export CONCRETE_ECHO_RESULT=1  # MAIN_EXIT_MODEL stage 1: legacy echoed-result mode until fixtures migrate (stage 2 deletes this)
# Example coverage gate.
#
# Drives scripts/tests/example_manifest.txt, the single source of truth for
# what every .con file under examples/ is expected to do:
#
#   compile        — standalone compile must succeed
#   project-build  — `concrete build` in the owning project dir must succeed
#   expected-error — standalone compile must FAIL (diagnostics example)
#   skip           — not checked here (pressure example or covered by a
#                    dedicated gate; see the manifest header)
#
# Coverage rule: a .con file under examples/ that is not in the manifest is a
# FAILURE — new examples cannot silently go unchecked. A manifest entry whose
# file no longer exists is also a failure.
#
# Compiled binaries are left in $EXAMPLES_OUT_DIR (default: a temp dir) so a
# follow-up smoke step can run them.

set -uo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

COMPILER=".lake/build/bin/concrete"
MANIFEST="scripts/tests/example_manifest.txt"

if [ ! -x "$COMPILER" ]; then
  echo "error: compiler not found at $COMPILER. Run 'lake build' first." >&2
  exit 2
fi
if [ ! -f "$MANIFEST" ]; then
  echo "error: manifest not found at $MANIFEST" >&2
  exit 2
fi

OUT_DIR="${EXAMPLES_OUT_DIR:-}"
if [ -z "$OUT_DIR" ]; then
  OUT_DIR=$(mktemp -d)
  trap 'rm -rf "$OUT_DIR"' EXIT
else
  mkdir -p "$OUT_DIR"
fi

PASS=0
FAIL=0
SKIP=0
errors=""

ok() { PASS=$((PASS + 1)); }
no() {
  FAIL=$((FAIL + 1))
  errors="$errors\n  FAIL $1"
  echo "FAIL: $1"
}

# --- 1. Coverage: every .con under examples/ is in the manifest, and ---
# --- every manifest path still exists on disk.                       ---
manifest_paths=$(grep -v '^[[:space:]]*#' "$MANIFEST" | grep -v '^[[:space:]]*$' | awk '{print $1}' | sort)
disk_paths=$(find examples -name '*.con' | sort)

unlisted=$(comm -23 <(echo "$disk_paths") <(echo "$manifest_paths"))
if [ -n "$unlisted" ]; then
  while IFS= read -r f; do
    no "$f is not listed in $MANIFEST (add it with its expected outcome)"
  done <<< "$unlisted"
else
  ok
fi

missing=$(comm -13 <(echo "$disk_paths") <(echo "$manifest_paths"))
if [ -n "$missing" ]; then
  while IFS= read -r f; do
    no "$f is listed in $MANIFEST but does not exist on disk"
  done <<< "$missing"
else
  ok
fi

dups=$(echo "$manifest_paths" | uniq -d)
if [ -n "$dups" ]; then
  while IFS= read -r f; do
    no "$f appears more than once in $MANIFEST"
  done <<< "$dups"
else
  ok
fi

# --- 2. Drive each manifest entry. project-build dirs are built once. ---
built_projects=""

# Find the nearest ancestor directory (within examples/) with a Concrete.toml.
project_root_of() {
  local dir
  dir=$(dirname "$1")
  while [ "$dir" != "." ] && [ "$dir" != "/" ]; do
    if [ -f "$dir/Concrete.toml" ]; then
      echo "$dir"
      return 0
    fi
    dir=$(dirname "$dir")
  done
  return 1
}

while read -r src outcome; do
  case "$outcome" in
    compile)
      out_name=$(echo "$src" | tr '/.' '__')
      if "$COMPILER" "$src" -o "$OUT_DIR/$out_name" > /dev/null 2>&1; then
        ok
      else
        no "$src (expected to compile)"
        "$COMPILER" "$src" -o "$OUT_DIR/$out_name" 2>&1 | head -5
      fi
      ;;
    project-build)
      proj=$(project_root_of "$src") || { no "$src (project-build, but no Concrete.toml ancestor)"; continue; }
      case " $built_projects " in
        *" $proj "*) continue ;;
      esac
      built_projects="$built_projects $proj"
      if (cd "$proj" && "$ROOT_DIR/$COMPILER" build > /dev/null 2>&1); then
        ok
      else
        no "$proj (project expected to build)"
        (cd "$proj" && "$ROOT_DIR/$COMPILER" build 2>&1 | head -5)
      fi
      ;;
    expected-error)
      if "$COMPILER" "$src" -o "$OUT_DIR/should_not_exist" > /dev/null 2>&1; then
        no "$src (expected a compile error, but it compiled)"
      else
        ok
      fi
      ;;
    skip)
      SKIP=$((SKIP + 1))
      ;;
    *)
      no "$src has unknown outcome '$outcome' in $MANIFEST"
      ;;
  esac
done < <(grep -v '^[[:space:]]*#' "$MANIFEST" | grep -v '^[[:space:]]*$')

echo ""
echo "=== Example coverage: $PASS passed, $FAIL failed, $SKIP skipped ==="
if [ "$FAIL" -gt 0 ]; then
  echo -e "Failures:$errors"
  exit 1
fi
