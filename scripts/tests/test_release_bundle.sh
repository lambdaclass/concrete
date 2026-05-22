#!/usr/bin/env bash
# Smoke test for the release-bundle capture script.
#
# Captures a bundle on parse_validate (the pilot) and asserts every
# required-always file is present plus the appropriate
# stage-conditional files. Does not capture bundles for every
# example; that's opt-in per Phase 7 showcase manifest entry.

set -uo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

PASS=0
FAIL=0
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

OUT="$TMP/parse_validate"

check_file() {
  local label="$1" path="$2"
  if [ -e "$path" ]; then
    echo "  ok   $label"
    PASS=$((PASS + 1))
  else
    echo "  FAIL $label missing: $path"
    FAIL=$((FAIL + 1))
  fi
}

check_contains() {
  local label="$1" path="$2" needle="$3"
  if [ ! -e "$path" ]; then
    echo "  FAIL $label missing: $path"
    FAIL=$((FAIL + 1))
    return
  fi
  if grep -qF "$needle" "$path"; then
    echo "  ok   $label contains '$needle'"
    PASS=$((PASS + 1))
  else
    echo "  FAIL $label does not contain '$needle'"
    FAIL=$((FAIL + 1))
  fi
}

# --- Capture on parse_validate (the pilot) ---
echo "=== capture parse_validate ==="
bash scripts/tests/capture_release_bundle.sh examples/parse_validate -o "$OUT" >/dev/null 2>&1

# Always-included files
for f in manifest.json compiler-version.txt runtime-stdout.txt \
         source/program.con source/main.con Concrete.toml; do
  check_file "$f" "$OUT/$f"
done

# Stage-conditional files (parse_validate has all of these)
for f in assumptions.toml AUDIT.md CATCHES.md README.md \
         source/proof-registry.json; do
  check_file "$f" "$OUT/$f"
done

# Reports present
for kind in caps alloc unsafe stack-depth effects proof-status \
            eligibility verify; do
  check_file "reports/$kind.txt" "$OUT/reports/$kind.txt"
done

# Snapshots present
check_file "snapshots/caps.txt" "$OUT/snapshots/caps.txt"
check_file "snapshots/proof-status.txt" "$OUT/snapshots/proof-status.txt"

# Negative pair
check_file "catches/01_authority_widening.con" "$OUT/catches/01_authority_widening.con"

# Manifest correctness
check_contains "manifest.json names example" "$OUT/manifest.json" '"example": "parse_validate"'
check_contains "manifest.json reports proved_functions" "$OUT/manifest.json" '"proved_functions": 2'
check_contains "manifest.json reports has_policy" "$OUT/manifest.json" '"has_policy": true'
check_contains "manifest.json reports has_assumptions" "$OUT/manifest.json" '"has_assumptions": true'
check_contains "manifest.json reports has_negative_pair" "$OUT/manifest.json" '"has_negative_pair": true'

# Compiler version banner is real
check_contains "compiler-version.txt mentions concrete" "$OUT/compiler-version.txt" "concrete"

# Refuse-if-broken contract
echo ""
echo "=== refuse on broken source ==="
mkdir -p "$TMP/broken/src"
echo "this is not concrete syntax" > "$TMP/broken/src/main.con"
if bash scripts/tests/capture_release_bundle.sh "$TMP/broken" -o "$TMP/broken-out" >/dev/null 2>&1; then
  echo "  FAIL refuse-if-broken: capture succeeded on broken source"
  FAIL=$((FAIL + 1))
else
  echo "  ok   refuse-if-broken: capture refused on broken source"
  PASS=$((PASS + 1))
fi

echo ""
echo "RELEASE-BUNDLE: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -gt 0 ] && exit 1 || exit 0
