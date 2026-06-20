#!/usr/bin/env bash
# Project-model gate (ROADMAP Phase 5 #2).
#
# Locks the Concrete.toml project-model STRUCTURE and validation so the manifest
# cannot drift into an ambient hidden configuration channel:
#   - a valid [package] with a name builds with no structural warnings;
#   - a missing [package] / missing name is warned;
#   - an UNRECOGNIZED section is warned (the anti-ambient-config guard — new config
#     surface cannot be added silently);
#   - the entry point is src/main.con, and its absence is a clean error.
# [policy] ENFORCEMENT is gated separately by check_policy.sh; this gate covers the
# manifest structure itself. See docs/POLICY_FILES.md, docs/PROJECT_BOOTSTRAP.md,
# docs/STANDALONE_VS_PROJECT.md.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
mkdir -p "$TMP/p/src"
printf 'mod p { fn main() -> i64 { return 0; } }\n' > "$TMP/p/src/main.con"
setup_toml(){ printf '%s' "$1" > "$TMP/p/Concrete.toml"; }
build_out(){ ( cd "$TMP/p" && "$COMPILER" build 2>&1 ); }

echo "=== 1. valid manifest builds with no structural warning ==="
setup_toml '[package]
name = "p"
version = "0.1.0"
'
out="$(build_out)"
printf '%s' "$out" | grep -qE "Built" && ! printf '%s' "$out" | grep -qiE "warning: Concrete.toml" \
  && ok "valid [package]+name builds, no structural warning" \
  || no "valid manifest warned or failed to build"

echo "=== 2. unrecognized section is warned (anti-ambient-config) ==="
setup_toml '[package]
name = "p"
version = "0.1.0"
[mystery]
secret = true
'
printf '%s' "$(build_out)" | grep -qiE "unrecognized section '\[mystery\]'" \
  && ok "unknown section warned — no silent hidden config" \
  || no "unknown section was NOT warned (ambient-config guard missing)"

echo "=== 3. missing [package] is warned ==="
setup_toml '[dependencies]
'
printf '%s' "$(build_out)" | grep -qiE "missing \[package\] section" \
  && ok "missing [package] warned" || no "missing [package] not warned"

echo "=== 4. missing name is warned ==="
setup_toml '[package]
version = "0.1.0"
'
printf '%s' "$(build_out)" | grep -qiE "\[package\] missing 'name'" \
  && ok "missing name warned" || no "missing name not warned"

echo "=== 5. entry point is src/main.con; absence is a clean error ==="
setup_toml '[package]
name = "p"
version = "0.1.0"
'
mv "$TMP/p/src/main.con" "$TMP/p/src/other.con"
out="$(build_out)"
if printf '%s' "$out" | grep -qiE "src/main.con|entry point" && ! printf '%s' "$out" | grep -qE "Built"; then
  ok "missing src/main.con is a clean error naming the entry point"
else
  no "missing entry point not reported cleanly"
fi
mv "$TMP/p/src/other.con" "$TMP/p/src/main.con"

echo ""
echo "PROJECT-MODEL: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
