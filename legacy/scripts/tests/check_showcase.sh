#!/usr/bin/env bash
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
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/fresh.sh"
require_fresh_binary || exit 1

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

# EACH FLAGSHIP IS CHECKED IN ITS OWN PROCESS, CONCURRENTLY (R-0546). A flagship's checks (release
# bundle capture, the audit report) are independent of every other flagship's, and in series they made
# this a 5-minute gate. check_flagship writes its report to its own file and its verdict (0 pass,
# 1 fail) to another; the reports are printed and the verdicts counted in manifest order, and a
# flagship that left no verdict is counted as a failure.
check_flagship() { # index name path claimed_conf
  local name="$2" path="$3" claimed_conf="$4" errs=0 f d bundle_out audit_out runtime_conf
  echo "=== flagship: $name ==="

  # 1. Path exists.
  if [ ! -d "$path" ]; then
    echo "  FAIL path $path missing"
    return 1
  fi

  # 2. Required artifacts (the 10 bars' artifacts).
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
    return 0
  fi
  return 1
}

N=0
while IFS=$'\t' read -r name path claimed_conf; do
  [ -z "$name" ] && continue
  ( rc=0; check_flagship "$N" "$name" "$path" "$claimed_conf" > "$TMP/report-$N" 2>&1 || rc=$?
    echo "$rc" > "$TMP/verdict-$N" ) &
  N=$((N + 1))
done < "$TMP/entries.tsv"
wait
for (( i=0; i<N; i++ )); do
  cat "$TMP/report-$i" 2>/dev/null
  case "$(cat "$TMP/verdict-$i" 2>/dev/null)" in
    0) PASS=$((PASS + 1)) ;;
    1) FAIL=$((FAIL + 1)) ;;
    *) echo "  FAIL flagship #$i left no verdict (its check died before recording one)"; FAIL=$((FAIL + 1)) ;;
  esac
done

echo ""
echo "SHOWCASE: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -gt 0 ] && exit 1 || exit 0
