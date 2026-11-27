#!/usr/bin/env bash
# PINNED LAKE DEPENDENCY CACHE VERIFICATION AND RETRY CONTROLS (ISS-0130).
#
# Exercises scripts/checks/lib/fetch_lake_packages.sh end to end on local bare
# repositories and a PATH-injected git stub, with no network access:
#   - cold fetch of a pinned non-HEAD revision;
#   - offline cache hit after the remote disappears;
#   - revision mismatch after restore -> package deleted and refetched;
#   - planted .lake/build inside a restored dependency -> deleted and refetched clean;
#   - modified tracked file and stray unlisted package -> cleaned/refetched;
#   - transient HTTP 403 failures -> bounded retry succeeds on attempt 3;
#   - persistent HTTP 403 failures -> bounded retry fails closed after 3 attempts;
#   - remote refusing direct SHA-1 fetch -> falls back to advertised refs;
#   - stalled git fetch -> per-attempt timeout kills and fails closed after bounded retries;
#   - malformed/unsafe lake-manifest.json inputs -> refused before touching disk.
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
HELPER="$ROOT_DIR/scripts/checks/lib/fetch_lake_packages.sh"
[ -f "$HELPER" ] || { echo "FATAL: $HELPER missing" >&2; exit 2; }
source "$ROOT_DIR/scripts/checks/lib/git_sandbox.sh" || exit 2

TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
git_sandbox "$TMP/gitconfig" || exit 2
cat >"$GIT_CONFIG_GLOBAL" <<'EOF'
[user]
	name = lake-packages control
	email = control@example.invalid
[init]
	defaultBranch = main
[advice]
	detachedHead = false
EOF

PASS=0
FAIL=0
ok() { echo "  ok   $1"; PASS=$((PASS + 1)); }
no() { echo "  FAIL $1"; FAIL=$((FAIL + 1)); }

# Build a local bare dependency repository with two distinct commits (REV1 < REV2).
DEP_SRC="$TMP/dep-src"
DEP_BARE="$TMP/dep.git"
git init -q "$DEP_SRC"
printf 'name = "dummy-dep"\n[[lean_lib]]\nname = "DummyDep"\n' >"$DEP_SRC/lakefile.toml"
printf 'def dummyVal : Nat := 1\n' >"$DEP_SRC/DummyDep.lean"
cp "$ROOT_DIR/lean-toolchain" "$DEP_SRC/"
git -C "$DEP_SRC" add lakefile.toml DummyDep.lean lean-toolchain
git -C "$DEP_SRC" commit -qm "rev1"
REV1="$(git -C "$DEP_SRC" rev-parse HEAD)"

printf 'def dummyVal : Nat := 2\n' >"$DEP_SRC/DummyDep.lean"
git -C "$DEP_SRC" add DummyDep.lean
git -C "$DEP_SRC" commit -qm "rev2"
REV2="$(git -C "$DEP_SRC" rev-parse HEAD)"

git clone -q --bare "$DEP_SRC" "$DEP_BARE"

write_project() {
  local dir="$1"
  local url="$2"
  local rev="$3"
  rm -rf "$dir"
  mkdir -p "$dir"
  cp "$ROOT_DIR/lean-toolchain" "$dir/"
  cat >"$dir/lake-manifest.json" <<EOF
{"version": "1.3.0",
 "packagesDir": ".lake/packages",
 "packages":
 [{"url": "$url",
   "type": "git",
   "subDir": null,
   "scope": "",
   "rev": "$rev",
   "name": "«dummy-dep»",
   "manifestFile": "lake-manifest.json",
   "inputRev": "$rev",
   "inherited": false,
   "configFile": "lakefile.toml"}],
 "name": "control",
 "lakeDir": ".lake",
 "fixedToolchain": false}
EOF
}

REAL_GIT="$(command -v git)"
STUB_BIN="$TMP/stub-bin"
mkdir -p "$STUB_BIN"
cat >"$STUB_BIN/git" <<EOF
#!/usr/bin/env bash
set -euo pipefail
REAL_GIT="$REAL_GIT"
is_fetch=0
for arg in "\$@"; do
  if [ "\$arg" = "fetch" ]; then
    is_fetch=1
    break
  fi
done
if [ "\$is_fetch" = 1 ]; then
  if [ -n "\${STUB_COUNT_FILE:-}" ]; then
    count=0
    [ ! -f "\$STUB_COUNT_FILE" ] || count="\$(cat "\$STUB_COUNT_FILE")"
    count=\$((count + 1))
    printf '%s\n' "\$count" >"\$STUB_COUNT_FILE"
  else
    count=1
  fi
  if [ "\${STUB_MODE:-}" = "forbid-sha" ]; then
    for arg in "\$@"; do
      if [[ "\$arg" =~ ^[0-9a-f]{40}\$ ]]; then
        echo "fatal: remote error: upload-pack: not our ref \$arg" >&2
        exit 128
      fi
    done
  elif [ "\${STUB_MODE:-}" = "timeout" ]; then
    sleep 10
    exit 0
  elif [ "\${STUB_FAIL_UNTIL:-0}" -ge "\$count" ]; then
    echo "fatal: unable to access 'https://github.com/unbalancedparentheses/lean-compiler-backend/': The requested URL returned error: 403" >&2
    exit 128
  fi
fi
exec "\$REAL_GIT" "\$@"
EOF
chmod +x "$STUB_BIN/git"

echo "=== cold fetch and offline cache hit ==="
PROJ="$TMP/proj-cold"
write_project "$PROJ" "$DEP_BARE" "$REV1"
if (cd "$PROJ" && LAKE_FETCH_BACKOFF_BASE=0 bash "$HELPER") >"$TMP/cold.log" 2>&1 \
   && [ "$(git -C "$PROJ/.lake/packages/dummy-dep" rev-parse HEAD)" = "$REV1" ] \
   && [ -z "$(git -C "$PROJ/.lake/packages/dummy-dep" status --porcelain --untracked-files=all --ignored=matching)" ] \
   && [ ! -e "$PROJ/.lake/packages/dummy-dep/.lake" ] \
   && [ -z "$(git -C "$PROJ/.lake/packages/dummy-dep" config --get remote.origin.promisor || true)" ]; then
  ok "cold fetch materializes exact pinned non-HEAD revision ($REV1) cleanly"
else
  cat "$TMP/cold.log" >&2
  no "cold fetch failed"
fi

# Delete the remote temporarily to prove a valid cache hit requires zero network access.
mv "$DEP_BARE" "$TMP/dep-offline.git"
if (cd "$PROJ" && LAKE_FETCH_BACKOFF_BASE=0 bash "$HELPER") >"$TMP/hit.log" 2>&1 \
   && has_match -F "cache verified at $REV1" <"$TMP/hit.log"; then
  ok "valid restored package passes verification completely offline"
else
  cat "$TMP/hit.log" >&2
  no "offline cache hit failed"
fi
mv "$TMP/dep-offline.git" "$DEP_BARE"

echo "=== post-restore mismatch, build-output, dirty-tree and stray-package controls ==="
# 1. Wrong checked-out revision (REV2 instead of pinned REV1).
git -C "$PROJ/.lake/packages/dummy-dep" fetch -q --no-tags origin
git -C "$PROJ/.lake/packages/dummy-dep" checkout -q --detach "$REV2"
[ "$(git -C "$PROJ/.lake/packages/dummy-dep" rev-parse HEAD)" = "$REV2" ]
if (cd "$PROJ" && LAKE_FETCH_BACKOFF_BASE=0 bash "$HELPER") >"$TMP/mismatch.log" 2>&1 \
   && has_match -F "HEAD is $REV2, manifest pins $REV1" <"$TMP/mismatch.log" \
   && [ "$(git -C "$PROJ/.lake/packages/dummy-dep" rev-parse HEAD)" = "$REV1" ]; then
  ok "revision mismatch after restore deletes package and refetches pinned revision"
else
  cat "$TMP/mismatch.log" >&2
  no "revision mismatch control failed"
fi

# 2. Planted dependency .lake/build output inside restored package.
mkdir -p "$PROJ/.lake/packages/dummy-dep/.lake/build/lib/lean"
printf 'stale\n' >"$PROJ/.lake/packages/dummy-dep/.lake/build/lib/lean/DummyDep.olean"
if (cd "$PROJ" && LAKE_FETCH_BACKOFF_BASE=0 bash "$HELPER") >"$TMP/depbuild.log" 2>&1 \
   && has_match -F "unexpected .lake directory inside dependency" <"$TMP/depbuild.log" \
   && [ ! -e "$PROJ/.lake/packages/dummy-dep/.lake" ] \
   && [ "$(git -C "$PROJ/.lake/packages/dummy-dep" rev-parse HEAD)" = "$REV1" ]; then
  ok "restored dependency containing .lake/build is deleted and refetched source-only"
else
  cat "$TMP/depbuild.log" >&2
  no "planted dependency .lake/build control failed"
fi

# 3. Modified tracked file + stray unlisted package directory.
printf '%s\n' '-- tampered' >>"$PROJ/.lake/packages/dummy-dep/DummyDep.lean"
mkdir -p "$PROJ/.lake/packages/stale-unlisted"
printf 'orphan\n' >"$PROJ/.lake/packages/stale-unlisted/file"
if (cd "$PROJ" && LAKE_FETCH_BACKOFF_BASE=0 bash "$HELPER") >"$TMP/dirty.log" 2>&1 \
   && has_match -F "pruning unexpected entry .lake/packages/stale-unlisted" <"$TMP/dirty.log" \
   && has_match -F "dirty, untracked or ignored files present" <"$TMP/dirty.log" \
   && [ ! -e "$PROJ/.lake/packages/stale-unlisted" ] \
   && [ -z "$(git -C "$PROJ/.lake/packages/dummy-dep" status --porcelain --untracked-files=all --ignored=matching)" ]; then
  ok "dirty package is refetched cleanly and stray unlisted packages are pruned"
else
  cat "$TMP/dirty.log" >&2
  no "dirty-tree / stray-package control failed"
fi

# 3b. Wrong origin URL after restore -> deleted and refetched from manifest URL.
git -C "$PROJ/.lake/packages/dummy-dep" remote set-url origin "https://example.invalid/wrong-repo"
if (cd "$PROJ" && LAKE_FETCH_BACKOFF_BASE=0 bash "$HELPER") >"$TMP/origin.log" 2>&1 \
   && has_match -F "origin URL is https://example.invalid/wrong-repo, manifest pins $DEP_BARE" <"$TMP/origin.log" \
   && [ "$(git -C "$PROJ/.lake/packages/dummy-dep" remote get-url origin)" = "$DEP_BARE" ] \
   && [ "$(git -C "$PROJ/.lake/packages/dummy-dep" rev-parse HEAD)" = "$REV1" ]; then
  ok "origin URL mismatch after restore deletes package and refetches from manifest URL"
else
  cat "$TMP/origin.log" >&2
  no "origin URL mismatch control failed"
fi

# 3c. Corrupt .git metadata after restore -> deleted and refetched cleanly.
printf 'not-a-valid-ref\n' >"$PROJ/.lake/packages/dummy-dep/.git/HEAD"
if (cd "$PROJ" && LAKE_FETCH_BACKOFF_BASE=0 bash "$HELPER") >"$TMP/corrupt.log" 2>&1 \
   && has_match -F "unreadable HEAD commit" <"$TMP/corrupt.log" \
   && [ "$(git -C "$PROJ/.lake/packages/dummy-dep" rev-parse HEAD)" = "$REV1" ]; then
  ok "corrupt restored .git metadata is deleted and refetched cleanly"
else
  cat "$TMP/corrupt.log" >&2
  no "corrupt .git control failed"
fi

# 3d. Nested .lake inside a restored dependency subdirectory -> deleted and refetched clean.
mkdir -p "$PROJ/.lake/packages/dummy-dep/subdir/.lake/build"
printf 'stale\n' >"$PROJ/.lake/packages/dummy-dep/subdir/.lake/build/x.olean"
if (cd "$PROJ" && LAKE_FETCH_BACKOFF_BASE=0 bash "$HELPER") >"$TMP/nested-lake.log" 2>&1 \
   && has_match -F "unexpected nested .lake path" <"$TMP/nested-lake.log" \
   && [ ! -e "$PROJ/.lake/packages/dummy-dep/subdir" ] \
   && [ "$(git -C "$PROJ/.lake/packages/dummy-dep" rev-parse HEAD)" = "$REV1" ]; then
  ok "nested .lake directory inside restored dependency is deleted and refetched clean"
else
  cat "$TMP/nested-lake.log" >&2
  no "nested .lake control failed"
fi

echo "=== bounded retry, fallback and timeout controls ==="
# 4. Transient HTTP 403 on attempts 1 and 2, succeeds on attempt 3.
PROJ_RETRY="$TMP/proj-retry"
write_project "$PROJ_RETRY" "$DEP_BARE" "$REV1"
COUNT_FILE="$TMP/retry.count"
rm -f "$COUNT_FILE"
if (cd "$PROJ_RETRY" && PATH="$STUB_BIN:$PATH" STUB_COUNT_FILE="$COUNT_FILE" STUB_FAIL_UNTIL=2 \
    LAKE_FETCH_ATTEMPTS=3 LAKE_FETCH_BACKOFF_BASE=0 bash "$HELPER") >"$TMP/retry.log" 2>&1 \
   && [ "$(cat "$COUNT_FILE")" = "3" ] \
   && has_match -F "attempt 3/3" <"$TMP/retry.log" \
   && [ "$(git -C "$PROJ_RETRY/.lake/packages/dummy-dep" rev-parse HEAD)" = "$REV1" ]; then
  ok "transient HTTP 403 failures recover within 3 bounded attempts"
else
  cat "$TMP/retry.log" >&2
  no "transient retry recovery control failed"
fi

# 5. Persistent HTTP 403 fails closed after exactly 3 attempts with no partial package left.
PROJ_FAIL="$TMP/proj-fail"
write_project "$PROJ_FAIL" "$DEP_BARE" "$REV1"
COUNT_FILE="$TMP/fail.count"
rm -f "$COUNT_FILE"
rc=0
(cd "$PROJ_FAIL" && PATH="$STUB_BIN:$PATH" STUB_COUNT_FILE="$COUNT_FILE" STUB_FAIL_UNTIL=99 \
  LAKE_FETCH_ATTEMPTS=3 LAKE_FETCH_BACKOFF_BASE=0 bash "$HELPER") >"$TMP/fail.log" 2>&1 || rc=$?
if [ "$rc" -ne 0 ] && [ "$(cat "$COUNT_FILE")" = "3" ] \
   && has_match -F "failed to fetch $REV1 from $DEP_BARE after 3 attempt(s)" <"$TMP/fail.log" \
   && [ ! -e "$PROJ_FAIL/.lake/packages/dummy-dep" ] \
   && [ -z "$(find "$PROJ_FAIL/.lake/packages" -mindepth 1 -maxdepth 1 -print)" ]; then
  ok "persistent HTTP 403 fails closed after 3 attempts with no leftover partial state"
else
  cat "$TMP/fail.log" >&2
  no "persistent failure control failed (rc=$rc, count=$(cat "$COUNT_FILE" 2>/dev/null || echo none))"
fi

# 6. Remote refusing raw SHA-1 wants falls back to advertised refs.
PROJ_FB="$TMP/proj-fallback"
write_project "$PROJ_FB" "$DEP_BARE" "$REV1"
if (cd "$PROJ_FB" && PATH="$STUB_BIN:$PATH" STUB_MODE=forbid-sha \
    LAKE_FETCH_ATTEMPTS=1 LAKE_FETCH_BACKOFF_BASE=0 bash "$HELPER") >"$TMP/fallback.log" 2>&1 \
   && [ "$(git -C "$PROJ_FB/.lake/packages/dummy-dep" rev-parse HEAD)" = "$REV1" ]; then
  ok "remote refusing direct SHA-1 fetch falls back to advertised refs cleanly"
else
  cat "$TMP/fallback.log" >&2
  no "SHA-1 fallback control failed"
fi

# 7. Stalled git fetch hits per-attempt timeout, retries up to bound, and fails closed.
PROJ_TO="$TMP/proj-timeout"
write_project "$PROJ_TO" "$DEP_BARE" "$REV1"
COUNT_FILE="$TMP/timeout.count"
rm -f "$COUNT_FILE"
rc=0
(cd "$PROJ_TO" && PATH="$STUB_BIN:$PATH" STUB_COUNT_FILE="$COUNT_FILE" STUB_MODE=timeout \
  LAKE_FETCH_ATTEMPTS=2 LAKE_FETCH_BACKOFF_BASE=0 LAKE_FETCH_TIMEOUT=1 bash "$HELPER") >"$TMP/timeout.log" 2>&1 || rc=$?
if [ "$rc" -ne 0 ] && [ "$(cat "$COUNT_FILE")" = "2" ] \
   && has_match -F "fetch timed out after 1s" <"$TMP/timeout.log" \
   && [ ! -e "$PROJ_TO/.lake/packages/dummy-dep" ]; then
  ok "stalled git fetch is killed by per-attempt timeout and fails closed after bounded retries"
else
  cat "$TMP/timeout.log" >&2
  no "timeout control failed (rc=$rc, count=$(cat "$COUNT_FILE" 2>/dev/null || echo none))"
fi

echo "=== manifest integrity controls ==="
expect_manifest_refusal() {
  local label="$1"
  local expected="$2"
  local json_body="$3"
  local dir="$TMP/bad-$label"
  local rc=0
  mkdir -p "$dir"
  cp "$ROOT_DIR/lean-toolchain" "$dir/"
  printf '%s\n' "$json_body" >"$dir/lake-manifest.json"
  (cd "$dir" && LAKE_FETCH_BACKOFF_BASE=0 bash "$HELPER") >"$TMP/bad-$label.log" 2>&1 || rc=$?
  if [ "$rc" -ne 0 ] && has_match -F "$expected" <"$TMP/bad-$label.log"; then
    ok "manifest control rejects $label"
  else
    cat "$TMP/bad-$label.log" >&2
    no "manifest control did not reject $label (rc=$rc)"
  fi
}

expect_manifest_refusal "non-hex-rev" "40-char hex commit" \
  '{"packagesDir":".lake/packages","packages":[{"type":"git","name":"dep","url":"https://example.invalid/dep","rev":"main"}]}'
expect_manifest_refusal "path-traversal-name" "unsafe package name" \
  '{"packagesDir":".lake/packages","packages":[{"type":"git","name":"../escape","url":"https://example.invalid/dep","rev":"0123456789abcdef0123456789abcdef01234567"}]}'
expect_manifest_refusal "bad-packages-dir" "expected packagesDir" \
  '{"packagesDir":".lake/build","packages":[]}'
expect_manifest_refusal "non-git-type" "unsupported type" \
  '{"packagesDir":".lake/packages","packages":[{"type":"path","name":"dep","url":"../dep","rev":"0123456789abcdef0123456789abcdef01234567"}]}'

echo
echo "lake_packages: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
