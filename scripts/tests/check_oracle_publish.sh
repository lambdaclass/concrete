#!/usr/bin/env bash
# THE ORACLE IS PUBLISHED AT THE LOCKED COMMIT, OR NOT AT ALL (P13; ROADMAP R-0564).
#
# scripts/oracle/publish.sh puts the frozen compiler on a public release. Its tag must name the
# locked commit: a release created without --target is tagged at the default branch's head, which
# would publish today's tree under the oracle's name. This gate runs publish.sh against a fake `gh`
# and a fake `curl` (no network, nothing published) on a scratch copy of scripts/oracle with a test
# lock, and holds it to: a new release is created with --target at the lock's commit; an existing
# release whose tag resolves elsewhere is refused before any upload; a release that already holds
# the asset is only re-verified; an artifact with the wrong digest or name, a lock whose commit is not
# a full id, a gh that tags the wrong commit and a published asset that downloads to other bytes are
# all refused. Each refusal is a negative control with its expected reason.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR" || exit 2
[ -f scripts/oracle/publish.sh ] || { echo "FATAL: scripts/oracle/publish.sh missing" >&2; exit 2; }
TMP="$(mktemp -d)" || exit 2
trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

LOCKED=1111111111111111111111111111111111111111
OTHER=2222222222222222222222222222222222222222

# A scratch oracle directory: the real scripts, a test lock naming a test artifact.
mkdir -p "$TMP/oracle" "$TMP/bin" "$TMP/art"
cp scripts/oracle/publish.sh scripts/oracle/lib.sh "$TMP/oracle/"
ART="$TMP/art/oracle-test-tag-x86_64-linux.tar.gz"
printf 'the test artifact\n' > "$ART"
SHA="$(sha256sum "$ART" | cut -d' ' -f1)"
write_lock() {  # write_lock COMMIT
  cat > "$TMP/oracle/ORACLE.lock" <<EOF
tag=test-tag
commit=$1
platform=x86_64-linux
artifact_sha256=$SHA
release_repo=example/repo
release_tag=oracle-test-tag
artifact_url=
EOF
}

# The fake gh keeps the release in $STATE: exists, tag_commit, asset (a copy of the uploaded file),
# create_target (what --target said), uploads (count). FAKE_GH_TAG_AT=head makes create ignore
# --target and tag OTHER, the bug this gate exists for.
cat > "$TMP/bin/gh" <<'EOF'
#!/usr/bin/env bash
set -uo pipefail
S="$STATE"
case "$1 $2" in
  "release view")
    [ -f "$S/exists" ] || exit 1
    if printf '%s\n' "$@" | grep -qx -- --json; then [ -f "$S/asset" ] && cat "$S/asset_name"; fi
    exit 0 ;;
  "release create")
    t=""; while [ $# -gt 0 ]; do [ "$1" = --target ] && t="$2"; shift; done
    echo "$t" > "$S/create_target"; touch "$S/exists"
    if [ "${FAKE_GH_TAG_AT:-}" = head ]; then echo "$OTHER" > "$S/tag_commit"
    else echo "${t:-$OTHER}" > "$S/tag_commit"; fi
    exit 0 ;;
  "release upload")
    cp "$4" "$S/asset"; basename "$4" > "$S/asset_name"
    echo $(( $(cat "$S/uploads" 2>/dev/null || echo 0) + 1 )) > "$S/uploads"; exit 0 ;;
  "api repos/example/repo/commits/tags/oracle-test-tag")
    [ -f "$S/tag_commit" ] && cat "$S/tag_commit" || exit 1; exit 0 ;;
esac
echo "fake gh: unexpected: $*" >&2; exit 9
EOF
# The fake curl serves the stored asset (or other bytes when FAKE_CURL_TAMPER is set).
cat > "$TMP/bin/curl" <<'EOF'
#!/usr/bin/env bash
out=""; while [ $# -gt 0 ]; do [ "$1" = -o ] && out="$2"; shift; done
[ -f "$STATE/asset" ] || exit 22
if [ -n "${FAKE_CURL_TAMPER:-}" ]; then echo tampered > "$out"; else cp "$STATE/asset" "$out"; fi
EOF
chmod +x "$TMP/bin/gh" "$TMP/bin/curl"

run=0
publish() {  # publish ARTIFACT [ENV=VAL...]: fresh $STATE prepared by the caller; output in $OUT, status in $RC
  local a="$1"; shift
  OUT="$(env PATH="$TMP/bin:$PATH" STATE="$STATE" OTHER="$OTHER" "$@" bash "$TMP/oracle/publish.sh" "$a" 2>&1)"; RC=$?
}
fresh() { run=$((run+1)); STATE="$TMP/state$run"; mkdir -p "$STATE"; }
uploads() { cat "$STATE/uploads" 2>/dev/null || echo 0; }

echo "=== oracle publish: release at the locked commit ==="
write_lock "$LOCKED"

fresh; publish "$ART"
if [ "$RC" = 0 ] && [ "$(cat "$STATE/create_target" 2>/dev/null)" = "$LOCKED" ] \
   && [ "$(cat "$STATE/tag_commit")" = "$LOCKED" ] && [ "$(uploads)" = 1 ]; then
  ok "a new release is created with --target at the lock's commit, uploaded once and verified"
else no "new release (rc=$RC target=$(cat "$STATE/create_target" 2>/dev/null) uploads=$(uploads)): $OUT"; fi

fresh; touch "$STATE/exists"; echo "$LOCKED" > "$STATE/tag_commit"; cp "$ART" "$STATE/asset"
basename "$ART" > "$STATE/asset_name"; publish "$ART"
if [ "$RC" = 0 ] && [ ! -f "$STATE/create_target" ] && [ "$(uploads)" = 0 ] && grep -q "already holds" <<<"$OUT"; then
  ok "an existing release at the locked commit holding the asset is only re-verified"
else no "re-verify (rc=$RC uploads=$(uploads)): $OUT"; fi

fresh; touch "$STATE/exists"; echo "$LOCKED" > "$STATE/tag_commit"; publish "$ART"
if [ "$RC" = 0 ] && [ ! -f "$STATE/create_target" ] && [ "$(uploads)" = 1 ]; then
  ok "an existing release at the locked commit without the asset gets it uploaded"
else no "existing release, no asset (rc=$RC uploads=$(uploads)): $OUT"; fi

echo "=== negative controls ==="
fresh; touch "$STATE/exists"; echo "$OTHER" > "$STATE/tag_commit"; publish "$ART"
if [ "$RC" = 1 ] && [ "$(uploads)" = 0 ] && grep -q "is at commit $OTHER, the lock says $LOCKED" <<<"$OUT"; then
  ok "an existing release tagged at another commit is refused before any upload"
else no "existing release at another commit (rc=$RC uploads=$(uploads)): $OUT"; fi

fresh; publish "$ART" FAKE_GH_TAG_AT=head
if [ "$RC" = 1 ] && [ "$(uploads)" = 0 ] && grep -q "new release oracle-test-tag on example/repo is at commit $OTHER" <<<"$OUT"; then
  ok "a release whose new tag lands on another commit is refused before any upload"
else no "gh tags the wrong commit (rc=$RC uploads=$(uploads)): $OUT"; fi

fresh; publish "$ART" FAKE_CURL_TAMPER=1
if [ "$RC" = 1 ] && grep -q "does not download to $SHA" <<<"$OUT"; then
  ok "a published asset that downloads to other bytes is refused"
else no "tampered download (rc=$RC): $OUT"; fi

fresh; BAD="$TMP/art/wrong/$(basename "$ART")"; mkdir -p "$(dirname "$BAD")"; echo other > "$BAD"
publish "$BAD"
if [ "$RC" = 1 ] && [ ! -f "$STATE/exists" ] && grep -q "the lock says $SHA" <<<"$OUT"; then
  ok "an artifact whose sha256 is not the lock's is refused before anything is created"
else no "wrong digest (rc=$RC): $OUT"; fi

fresh; RENAMED="$TMP/art/renamed.tar.gz"; cp "$ART" "$RENAMED"; publish "$RENAMED"
if [ "$RC" = 1 ] && [ ! -f "$STATE/exists" ] && grep -q "must be named $(basename "$ART")" <<<"$OUT"; then
  ok "an artifact not named after the lock is refused before anything is created"
else no "wrong name (rc=$RC): $OUT"; fi

write_lock "066c9d9b8"; fresh; publish "$ART"
if [ "$RC" = 1 ] && [ ! -f "$STATE/exists" ] && grep -q "is not a full commit id" <<<"$OUT"; then
  ok "a lock whose commit is not a full id is refused before anything is created"
else no "abbreviated lock commit (rc=$RC): $OUT"; fi

echo
echo "check_oracle_publish: $PASS passed, $FAIL failed"
[ "$FAIL" = 0 ]
