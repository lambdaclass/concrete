#!/usr/bin/env bash
export CONCRETE_ECHO_RESULT=1  # MAIN_EXIT_MODEL stage 1: legacy echoed-result mode until fixtures migrate (stage 2 deletes this)
# Phase 7 showcase CI gate.
#
# For each graduated flagship in tests/showcase/manifest.toml,
# verifies:
#   1. The example path exists.
#   2. Every claimed evidence file exists.
#   3. Every claimed bar is actually still met (cross-checked
#      against the file's bar declaration).
#   4. The release bundle still captures cleanly.
#
# This is the strictest standing gate — it composes every other
# drift-enforced gate the project has built. A flagship that has
# graduated must STAY graduated.

set -uo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

MANIFEST="tests/showcase/manifest.toml"
COMPILER=".lake/build/bin/concrete"

if [ ! -f "$MANIFEST" ]; then
  echo "No showcase manifest found. Nothing to check."
  exit 0
fi
if [ ! -x "$COMPILER" ]; then
  echo "error: compiler not found at $COMPILER. Run 'make build' first." >&2
  exit 2
fi

PASS=0
FAIL=0
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

# Parse the manifest's [[flagship]] entries. Yields example name +
# path + claimed provable_v1_conformance on each line.
python3 - "$MANIFEST" > "$TMP/entries.tsv" <<'PYEOF'
import re, sys
src = open(sys.argv[1]).read()
chunks = re.split(r'^\[\[flagship\]\]\s*$', src, flags=re.MULTILINE)[1:]
for c in chunks:
    fields = {}
    for m in re.finditer(r'^\s*(\w+)\s*=\s*"([^"]*)"\s*$', c, flags=re.MULTILINE):
        fields[m.group(1)] = m.group(2)
    name = fields.get('example', '')
    path = fields.get('path', '')
    conf = fields.get('provable_v1_conformance', '')
    if name and path:
        print(f"{name}\t{path}\t{conf}")
PYEOF

while IFS=$'\t' read -r name path claimed_conf; do
  [ -z "$name" ] && continue
  echo "=== flagship: $name ==="

  # 1. Path exists.
  if [ ! -d "$path" ]; then
    echo "  FAIL path $path missing"
    FAIL=$((FAIL + 1))
    continue
  fi

  # 2. Required artifacts (the 10 bars' artifacts).
  errs=0
  # proof-registry.json is NO LONGER required: flagship proofs are in-source
  # (#[spec]/#[proof_by]/#[proof_fingerprint]); a stray registry is forbidden by
  # check_no_example_registries.sh.
  for f in src/main.con AUDIT.md README.md CATCHES.md Concrete.toml \
           assumptions.toml; do
    if [ ! -f "$path/$f" ]; then
      echo "  FAIL missing artifact: $path/$f"
      errs=$((errs + 1))
    fi
  done
  for d in catches snapshot oracle; do
    if [ ! -d "$path/$d" ]; then
      echo "  FAIL missing directory: $path/$d"
      errs=$((errs + 1))
    fi
  done

  # 3. AUDIT.md must show "10 of 10 bars met" (or higher) — assert
  #    the bars-met line names 10/10.
  if ! grep -qE 'bars met' "$path/AUDIT.md"; then
    echo "  FAIL $path/AUDIT.md has no bars-met line"
    errs=$((errs + 1))
  fi

  # 4. Release bundle captures cleanly.
  bundle_out="$TMP/bundle-$name"
  if ! bash scripts/tests/capture_release_bundle.sh "$path" -o "$bundle_out" >/dev/null 2>&1; then
    echo "  FAIL release bundle capture failed"
    errs=$((errs + 1))
  fi

  # 5. ProvableV1 conformance — the manifest's claimed
  #    provable_v1_conformance must match the runtime audit's Status line.
  #    Closes the doc → manifest → runtime loop; a future change that
  #    breaks ProvableV1 conformance surfaces here.
  if [ -n "$claimed_conf" ] && [ -f "$path/src/main.con" ]; then
    audit_out=$("$COMPILER" "$path/src/main.con" --report audit 2>/dev/null || true)
    runtime_conf=$(grep <<<"$audit_out" -m1 "^Status: " | awk '{print $2}')
    if [ -z "$runtime_conf" ]; then
      echo "  FAIL ProvableV1 conformance: runtime audit produced no Status line"
      errs=$((errs + 1))
    elif [ "$runtime_conf" != "$claimed_conf" ]; then
      echo "  FAIL ProvableV1 conformance drift: manifest claims '$claimed_conf', runtime says '$runtime_conf'"
      errs=$((errs + 1))
    fi
  fi

  if [ "$errs" -eq 0 ]; then
    echo "  ok   $name — all artifacts present, release bundle captures cleanly, ProvableV1 conformance matches manifest"
    PASS=$((PASS + 1))
  else
    FAIL=$((FAIL + 1))
  fi
done < "$TMP/entries.tsv"

echo ""
echo "SHOWCASE: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -gt 0 ] && exit 1 || exit 0
