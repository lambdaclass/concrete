#!/usr/bin/env bash
# Build-profiles gate — ROADMAP #10, Stage 1 (mechanism only).
#
# Stage 1 makes the active build profile visible; it has NO codegen/semantic
# effect. This gate pins the mechanism:
#   - a default profile is reported when nothing selects one,
#   - a `--profile` CLI flag wins,
#   - a `[profile]` in Concrete.toml is honored,
#   - CLI overrides the manifest,
#   - an unknown profile (CLI or manifest) is rejected,
#   - the report states arithmetic is checked SEMANTICS but the current LOWERING
#     still wraps until Stage 2 — so the report never implies a guarantee the
#     backend does not yet provide, and profiles are not arithmetic modes.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
C="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first ($C missing)" >&2; exit 2; }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; sed 's/^/        /' "$2" 2>/dev/null | head -4; FAIL=$((FAIL+1)); }

# Standalone file (no project) for default/CLI cases.
echo 'pub fn main() -> Int { return 0; }' > "$TMP/solo.con"

# A project with a [profile] in its manifest.
mkdir -p "$TMP/proj/src"
cat > "$TMP/proj/Concrete.toml" <<'EOF'
[package]
name = "demo"

[profile]
name = "predictable"
EOF
echo 'pub fn main() -> Int { return 0; }' > "$TMP/proj/src/main.con"

# A project whose manifest names an invalid profile.
mkdir -p "$TMP/badproj/src"
cat > "$TMP/badproj/Concrete.toml" <<'EOF'
[package]
name = "bad"

[profile]
name = "nonsense"
EOF
echo 'pub fn main() -> Int { return 0; }' > "$TMP/badproj/src/main.con"

echo "=== build profiles (Stage 1: mechanism + reporting only) ==="

# 1. default profile appears
"$C" "$TMP/solo.con" --report profile >"$TMP/o1" 2>&1
if grep -qE "profile: +debug" "$TMP/o1" && grep -qE "source: +default" "$TMP/o1"; then
  ok "default profile reported (debug, source=default)"; else no "default profile not reported" "$TMP/o1"; fi

# 2. CLI override wins
"$C" "$TMP/solo.con" --report profile --profile release >"$TMP/o2" 2>&1
if grep -qE "profile: +release" "$TMP/o2" && grep -qE "source: +--profile" "$TMP/o2"; then
  ok "CLI --profile overrides (release, source=cli)"; else no "CLI override not honored" "$TMP/o2"; fi

# 3. manifest profile works
"$C" "$TMP/proj/src/main.con" --report profile >"$TMP/o3" 2>&1
if grep -qE "profile: +predictable" "$TMP/o3" && grep -qE "source: +\[profile\] in Concrete.toml" "$TMP/o3"; then
  ok "manifest [profile] honored (predictable, source=manifest)"; else no "manifest profile not honored" "$TMP/o3"; fi

# 4a. CLI overrides manifest
"$C" "$TMP/proj/src/main.con" --report profile --profile release >"$TMP/o4" 2>&1
if grep -qE "profile: +release" "$TMP/o4"; then
  ok "CLI overrides manifest profile"; else no "CLI did not override manifest" "$TMP/o4"; fi

# 4b. invalid CLI profile rejected (non-zero exit, no report)
if "$C" "$TMP/solo.con" --report profile --profile bogus >"$TMP/o5" 2>&1; then
  no "invalid CLI profile was ACCEPTED" "$TMP/o5"
else
  grep -q "unknown profile 'bogus'" "$TMP/o5" && ok "invalid CLI profile rejected" || no "invalid CLI profile: wrong error" "$TMP/o5"
fi

# 4c. invalid manifest profile rejected
if "$C" "$TMP/badproj/src/main.con" --report profile >"$TMP/o6" 2>&1; then
  no "invalid manifest profile was ACCEPTED" "$TMP/o6"
else
  grep -q "'nonsense' is unknown" "$TMP/o6" && ok "invalid manifest profile rejected" || no "invalid manifest profile: wrong error" "$TMP/o6"
fi

# 5. report states checked SEMANTICS but current wrapping LOWERING gap
"$C" "$TMP/solo.con" --report profile >"$TMP/o7" 2>&1
if grep -q "checked semantics" "$TMP/o7" \
   && grep -qi "CURRENT LOWERING GAP" "$TMP/o7" \
   && grep -qi "still lowers to silent" "$TMP/o7" \
   && grep -qi "Stage 2" "$TMP/o7"; then
  ok "report states checked-semantics + current-wrapping-gap (Stage 2)"
else no "report missing the checked-semantics/wrapping-gap statement" "$TMP/o7"; fi

# Profiles are not arithmetic modes: the arithmetic line is identical across
# profiles (semantics are profile-invariant).
A_DEBUG="$("$C" "$TMP/solo.con" --report profile 2>&1 | grep -A1 'arithmetic:')"
A_REL="$("$C" "$TMP/solo.con" --report profile --profile release 2>&1 | grep -A1 'arithmetic:')"
if [ "$A_DEBUG" = "$A_REL" ]; then
  ok "arithmetic line is profile-invariant (debug == release)"
else no "arithmetic differs across profiles — profiles must NOT be arithmetic modes" /dev/null; fi

echo ""
echo "BUILD-PROFILES: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
