#!/usr/bin/env bash
# PINNED LAKE DEPENDENCY CACHE VERIFICATION AND RETRY CONTROLS (ISS-0130).
#
# Exercises scripts/checks/lib/fetch_lake_packages.sh end to end on local bare
# repositories and a PATH-injected git stub, with no network access:
#   - cold fetch of a pinned non-HEAD revision;
#   - offline cache hit after the remote disappears (discarding untrusted .git metadata);
#   - revision mismatch after restore -> package deleted and refetched;
#   - planted .lake/build and nested .lake inside restored dependency -> deleted and refetched;
#   - unreadable directory hiding a traversal error -> invalidates, cleans and refetches;
#   - modified tracked file (including hidden behind assume-unchanged and skip-worktree)
#     and stray unlisted package -> detected without trusting git status, deleted and refetched;
#   - tampered loose object in restored .git/objects -> detected by strict fsck and refetched;
#   - configured Git mirror (url.<mirror>.insteadOf) -> fetches and verifies offline against
#     stored literal manifest origin URL;
#   - scratch-workspace object seeding (trust_ban.sh pattern) -> never symlinks or inherits
#     local backend edits, extra commits or .lake/build outputs from caller's packages;
#   - transient HTTP 403 failures -> bounded retry succeeds on attempt 3;
#   - persistent HTTP 403 failures -> bounded retry fails closed after 3 attempts;
#   - remote refusing direct SHA-1 fetch -> falls back to advertised refs;
#   - stalled git fetch, slow primary+fallback exceeding one shared deadline, and slow
#     checkout -> single per-attempt deadline kills and fails closed after bounded retries;
#   - malformed/unsafe lake-manifest.json inputs -> refused before touching disk.
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
HELPER="$ROOT_DIR/scripts/checks/lib/fetch_lake_packages.sh"
[ -f "$HELPER" ] || { echo "FATAL: $HELPER missing" >&2; exit 2; }
source "$ROOT_DIR/scripts/checks/lib/git_sandbox.sh" || exit 2

TMP="$(mktemp -d)"
cleanup_tmp() {
  find "$TMP" -type d -exec chmod u+rwx {} + 2>/dev/null || true
  rm -rf "$TMP"
}
trap cleanup_tmp EXIT
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

# Build a local bare dependency repository with three distinct commits (REV0 < REV1 < REV2)
# so pinned REV1 is both non-root (has a parent commit, exercising shallow depth-1 handling)
# and non-HEAD (REV2 is HEAD).
DEP_SRC="$TMP/dep-src"
DEP_BARE="$TMP/dep.git"
git init -q "$DEP_SRC"
printf 'name = "dummy-dep"\n[[lean_lib]]\nname = "DummyDep"\n' >"$DEP_SRC/lakefile.toml"
printf 'def dummyVal : Nat := 0\n' >"$DEP_SRC/DummyDep.lean"
cp "$ROOT_DIR/lean-toolchain" "$DEP_SRC/"
git -C "$DEP_SRC" add lakefile.toml DummyDep.lean lean-toolchain
git -C "$DEP_SRC" commit -qm "rev0"

printf 'def dummyVal : Nat := 1\n' >"$DEP_SRC/DummyDep.lean"
git -C "$DEP_SRC" add DummyDep.lean
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
is_checkout=0
for arg in "\$@"; do
  if [ "\$arg" = "fetch" ]; then
    is_fetch=1
    break
  elif [ "\$arg" = "checkout" ]; then
    is_checkout=1
    break
  fi
done
if [ "\$is_checkout" = 1 ]; then
  if [ -n "\${STUB_CHECKOUT_COUNT_FILE:-}" ]; then
    co_count=0
    [ ! -f "\$STUB_CHECKOUT_COUNT_FILE" ] || co_count="\$(cat "\$STUB_CHECKOUT_COUNT_FILE")"
    co_count=\$((co_count + 1))
    printf '%s\n' "\$co_count" >"\$STUB_CHECKOUT_COUNT_FILE"
  fi
  if [ "\${STUB_MODE:-}" = "slow-checkout" ]; then
    sleep 3
    exit 0
  elif [ "\${STUB_MODE:-}" = "slow-fetch-and-checkout" ]; then
    sleep 0.65
  fi
fi
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
  elif [ "\${STUB_MODE:-}" = "slow-fallback" ]; then
    sleep 0.7
    for arg in "\$@"; do
      if [[ "\$arg" =~ ^[0-9a-f]{40}\$ ]]; then
        echo "fatal: remote error: upload-pack: not our ref \$arg" >&2
        exit 128
      fi
    done
  elif [ "\${STUB_MODE:-}" = "slow-fetch-and-checkout" ]; then
    sleep 0.65
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

# Plant an untrusted hook and assume-unchanged flag on an UNMODIFIED file, then delete the remote
# to prove a valid cache hit works offline AND discards restored .git metadata by materializing fresh.
mkdir -p "$PROJ/.lake/packages/dummy-dep/.git/hooks"
printf '#!/bin/sh\nexit 99\n' >"$PROJ/.lake/packages/dummy-dep/.git/hooks/post-checkout"
chmod +x "$PROJ/.lake/packages/dummy-dep/.git/hooks/post-checkout"
git -C "$PROJ/.lake/packages/dummy-dep" update-index --assume-unchanged DummyDep.lean
mv "$DEP_BARE" "$TMP/dep-offline.git"
if (cd "$PROJ" && LAKE_FETCH_BACKOFF_BASE=0 bash "$HELPER") >"$TMP/hit.log" 2>&1 \
   && has_match -F "cache verified at $REV1" <"$TMP/hit.log" \
   && [ ! -e "$PROJ/.lake/packages/dummy-dep/.git/hooks/post-checkout" ] \
   && [ -z "$(git -C "$PROJ/.lake/packages/dummy-dep" ls-files -v | grep '^[a-z]' || true)" ]; then
  ok "valid restored package passes verification offline and replaces restored .git metadata cleanly"
else
  cat "$TMP/hit.log" >&2
  no "offline cache hit failed"
fi
mv "$TMP/dep-offline.git" "$DEP_BARE"

echo "=== post-restore mismatch, tamper, build-output, traversal and stray-package controls ==="
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
   && has_match -F "working tree differs from pinned commit" <"$TMP/dirty.log" \
   && [ ! -e "$PROJ/.lake/packages/stale-unlisted" ] \
   && [ "$(cat "$PROJ/.lake/packages/dummy-dep/DummyDep.lean")" = "def dummyVal : Nat := 1" ]; then
  ok "dirty package is refetched cleanly and stray unlisted packages are pruned"
else
  cat "$TMP/dirty.log" >&2
  no "dirty-tree / stray-package control failed"
fi

# 3a. Finding 1 attack: modified tracked Lean file hidden behind assume-unchanged (git status is empty!).
printf '%s\n' 'def tampered : Nat := 999' >>"$PROJ/.lake/packages/dummy-dep/DummyDep.lean"
git -C "$PROJ/.lake/packages/dummy-dep" update-index --assume-unchanged DummyDep.lean
if [ -n "$(git -C "$PROJ/.lake/packages/dummy-dep" status --porcelain --untracked-files=all --ignored=matching)" ]; then
  no "assume-unchanged setup failed: git status unexpectedly reported the edit"
elif (cd "$PROJ" && LAKE_FETCH_BACKOFF_BASE=0 bash "$HELPER") >"$TMP/assume-unchanged.log" 2>&1 \
     && has_match -F "working tree differs from pinned commit (DummyDep.lean)" <"$TMP/assume-unchanged.log" \
     && [ "$(cat "$PROJ/.lake/packages/dummy-dep/DummyDep.lean")" = "def dummyVal : Nat := 1" ]; then
  ok "modified Lean file hidden by assume-unchanged is detected and refetched cleanly"
else
  cat "$TMP/assume-unchanged.log" >&2
  no "assume-unchanged tamper control failed"
fi

# 3a2. Finding 1 variant: modified tracked Lean file hidden behind skip-worktree (git status is empty!).
printf '%s\n' 'def tamperedSkip : Nat := 888' >>"$PROJ/.lake/packages/dummy-dep/DummyDep.lean"
git -C "$PROJ/.lake/packages/dummy-dep" update-index --skip-worktree DummyDep.lean
if [ -n "$(git -C "$PROJ/.lake/packages/dummy-dep" status --porcelain --untracked-files=all --ignored=matching)" ]; then
  no "skip-worktree setup failed: git status unexpectedly reported the edit"
elif (cd "$PROJ" && LAKE_FETCH_BACKOFF_BASE=0 bash "$HELPER") >"$TMP/skip-worktree.log" 2>&1 \
     && has_match -F "working tree differs from pinned commit (DummyDep.lean)" <"$TMP/skip-worktree.log" \
     && [ "$(cat "$PROJ/.lake/packages/dummy-dep/DummyDep.lean")" = "def dummyVal : Nat := 1" ]; then
  ok "modified Lean file hidden by skip-worktree is detected and refetched cleanly"
else
  cat "$TMP/skip-worktree.log" >&2
  no "skip-worktree tamper control failed"
fi

# 3a3. Corrupt object in restored .git/objects -> detected and refetched cleanly.
BLOB_OID="$(git -C "$PROJ/.lake/packages/dummy-dep" rev-parse "${REV1}:DummyDep.lean")"
BLOB_PATH="$PROJ/.lake/packages/dummy-dep/.git/objects/${BLOB_OID:0:2}/${BLOB_OID:2}"
chmod u+w "$BLOB_PATH"
printf 'corrupted-blob-bytes' >"$BLOB_PATH"
if (cd "$PROJ" && LAKE_FETCH_BACKOFF_BASE=0 bash "$HELPER") >"$TMP/corrupt-obj.log" 2>&1 \
   && has_match -F "restored package failed verification" <"$TMP/corrupt-obj.log" \
   && [ "$(cat "$PROJ/.lake/packages/dummy-dep/DummyDep.lean")" = "def dummyVal : Nat := 1" ]; then
  ok "corrupt blob object in restored .git/objects is detected and refetched cleanly"
else
  cat "$TMP/corrupt-obj.log" >&2
  no "corrupt object control failed"
fi

# 3b. Wrong origin URL after restore -> deleted and refetched from manifest URL.
git -C "$PROJ/.lake/packages/dummy-dep" remote set-url origin "https://example.invalid/wrong-repo"
if (cd "$PROJ" && LAKE_FETCH_BACKOFF_BASE=0 bash "$HELPER") >"$TMP/origin.log" 2>&1 \
   && has_match -F "origin URL is https://example.invalid/wrong-repo, manifest pins $DEP_BARE" <"$TMP/origin.log" \
   && [ "$(git -C "$PROJ/.lake/packages/dummy-dep" config --local --get remote.origin.url)" = "$DEP_BARE" ] \
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

# 3e. Non-blocking 2: unreadable directory hiding nested .lake and read-only/unreadable files ->
# traversal error invalidates, directory-only chmod removes it, and helper refetches clean.
mkdir -p "$PROJ/.lake/packages/dummy-dep/locked/.lake/build"
printf 'hidden\n' >"$PROJ/.lake/packages/dummy-dep/locked/.lake/build/x.olean"
chmod 0444 "$PROJ/.lake/packages/dummy-dep/locked/.lake/build/x.olean"
printf 'locked\n' >"$PROJ/.lake/packages/dummy-dep/locked/zero.txt"
chmod 0000 "$PROJ/.lake/packages/dummy-dep/locked/zero.txt"
chmod 000 "$PROJ/.lake/packages/dummy-dep/locked"
if (cd "$PROJ" && LAKE_FETCH_BACKOFF_BASE=0 bash "$HELPER") >"$TMP/unreadable-dir.log" 2>&1 \
   && has_match -F "filesystem traversal error" <"$TMP/unreadable-dir.log" \
   && [ ! -e "$PROJ/.lake/packages/dummy-dep/locked" ] \
   && [ "$(git -C "$PROJ/.lake/packages/dummy-dep" rev-parse HEAD)" = "$REV1" ]; then
  ok "unreadable directory with read-only files invalidates restored package and refetches clean"
else
  chmod 700 "$PROJ/.lake/packages/dummy-dep/locked" 2>/dev/null || true
  cat "$TMP/unreadable-dir.log" >&2
  no "unreadable directory traversal control failed"
fi

echo "=== Finding 3 (round 2): hard-linked shared object permissions are never mutated ==="
SHARED_MIRROR="$TMP/shared-mirror.git"
git clone -q --bare "$DEP_SRC" "$SHARED_MIRROR"
COMMIT_OBJ_REL="objects/${REV1:0:2}/${REV1:2}"
MIRROR_COMMIT_OBJ="$SHARED_MIRROR/$COMMIT_OBJ_REL"
chmod 0444 "$MIRROR_COMMIT_OBJ"

PROJ_HL_HIT="$TMP/proj-hardlink-hit"
write_project "$PROJ_HL_HIT" "$SHARED_MIRROR" "$REV1"
mkdir -p "$PROJ_HL_HIT/.lake/packages"
git clone -q "$SHARED_MIRROR" "$PROJ_HL_HIT/.lake/packages/dummy-dep"
git -C "$PROJ_HL_HIT/.lake/packages/dummy-dep" checkout -q --detach "$REV1"
CLONE_COMMIT_OBJ="$PROJ_HL_HIT/.lake/packages/dummy-dep/.git/$COMMIT_OBJ_REL"
BEFORE_INO="$(stat -c '%i' "$MIRROR_COMMIT_OBJ")"
CLONE_INO="$(stat -c '%i' "$CLONE_COMMIT_OBJ")"
BEFORE_MODE="$(stat -c '%a' "$MIRROR_COMMIT_OBJ")"
if [ "$BEFORE_INO" != "$CLONE_INO" ] || [ "$BEFORE_MODE" != "444" ]; then
  no "hardlink hit setup failed: mirror_ino=$BEFORE_INO clone_ino=$CLONE_INO mode=$BEFORE_MODE"
elif (cd "$PROJ_HL_HIT" && LAKE_FETCH_BACKOFF_BASE=0 bash "$HELPER") >"$TMP/hl-hit.log" 2>&1 \
     && has_match -F "cache verified at $REV1" <"$TMP/hl-hit.log" \
     && [ "$(stat -c '%a' "$MIRROR_COMMIT_OBJ")" = "444" ] \
     && [ "$(stat -c '%i' "$PROJ_HL_HIT/.lake/packages/dummy-dep/.git/$COMMIT_OBJ_REL")" != "$BEFORE_INO" ]; then
  ok "cache hit cleanup of hard-linked local clone preserves shared mirror object mode 444"
else
  cat "$TMP/hl-hit.log" >&2
  no "hard-linked cache hit mutated shared object mode (now $(stat -c '%a' "$MIRROR_COMMIT_OBJ" 2>/dev/null || echo missing))"
fi

PROJ_HL_MISS="$TMP/proj-hardlink-miss"
write_project "$PROJ_HL_MISS" "$SHARED_MIRROR" "$REV1"
mkdir -p "$PROJ_HL_MISS/.lake/packages"
git clone -q "$SHARED_MIRROR" "$PROJ_HL_MISS/.lake/packages/dummy-dep"
git -C "$PROJ_HL_MISS/.lake/packages/dummy-dep" checkout -q --detach "$REV1"
printf '%s\n' '-- dirty' >>"$PROJ_HL_MISS/.lake/packages/dummy-dep/DummyDep.lean"
CLONE_MISS_OBJ="$PROJ_HL_MISS/.lake/packages/dummy-dep/.git/$COMMIT_OBJ_REL"
if [ "$(stat -c '%i' "$CLONE_MISS_OBJ")" != "$BEFORE_INO" ] || [ "$(stat -c '%a' "$MIRROR_COMMIT_OBJ")" != "444" ]; then
  no "hardlink miss setup failed"
elif (cd "$PROJ_HL_MISS" && LAKE_FETCH_BACKOFF_BASE=0 bash "$HELPER") >"$TMP/hl-miss.log" 2>&1 \
     && has_match -F "restored package failed verification" <"$TMP/hl-miss.log" \
     && [ "$(stat -c '%a' "$MIRROR_COMMIT_OBJ")" = "444" ] \
     && [ "$(stat -c '%i' "$PROJ_HL_MISS/.lake/packages/dummy-dep/.git/$COMMIT_OBJ_REL")" != "$BEFORE_INO" ]; then
  ok "refetch cleanup of dirty hard-linked local clone preserves shared mirror object mode 444"
else
  cat "$TMP/hl-miss.log" >&2
  no "hard-linked refetch cleanup mutated shared object mode (now $(stat -c '%a' "$MIRROR_COMMIT_OBJ" 2>/dev/null || echo missing))"
fi

echo "=== Finding 3: configured Git mirror (url.*.insteadOf) controls ==="
MIRROR_ROOT="$TMP/mirror"
mkdir -p "$MIRROR_ROOT"
git clone -q --bare "$DEP_SRC" "$MIRROR_ROOT/dummy-dep.git"
MIRROR_CFG="$TMP/gitconfig-mirror"
cat >"$MIRROR_CFG" <<EOF
[user]
	name = lake-packages mirror control
	email = control@example.invalid
[init]
	defaultBranch = main
[advice]
	detachedHead = false
[url "$MIRROR_ROOT/"]
	insteadOf = https://example.invalid/upstream/
EOF
PROJ_MIRROR="$TMP/proj-mirror"
MANIFEST_URL="https://example.invalid/upstream/dummy-dep.git"
write_project "$PROJ_MIRROR" "$MANIFEST_URL" "$REV1"
if (cd "$PROJ_MIRROR" && GIT_CONFIG_GLOBAL="$MIRROR_CFG" LAKE_FETCH_ATTEMPTS=1 LAKE_FETCH_BACKOFF_BASE=0 bash "$HELPER") >"$TMP/mirror-fetch.log" 2>&1 \
   && [ "$(GIT_CONFIG_GLOBAL="$MIRROR_CFG" git -C "$PROJ_MIRROR/.lake/packages/dummy-dep" remote get-url origin)" = "$MIRROR_ROOT/dummy-dep.git" ] \
   && [ "$(git config --file "$PROJ_MIRROR/.lake/packages/dummy-dep/.git/config" --get remote.origin.url)" = "$MANIFEST_URL" ] \
   && [ "$(git -C "$PROJ_MIRROR/.lake/packages/dummy-dep" rev-parse HEAD)" = "$REV1" ]; then
  ok "configured Git mirror (insteadOf) fetches and verifies stored literal manifest URL"
else
  cat "$TMP/mirror-fetch.log" >&2
  no "Git mirror fetch control failed"
fi

# Prove offline cache hit still succeeds under the mirror gitconfig once the mirror disappears.
mv "$MIRROR_ROOT/dummy-dep.git" "$MIRROR_ROOT/dummy-dep-offline.git"
if (cd "$PROJ_MIRROR" && GIT_CONFIG_GLOBAL="$MIRROR_CFG" LAKE_FETCH_ATTEMPTS=1 LAKE_FETCH_BACKOFF_BASE=0 bash "$HELPER") >"$TMP/mirror-hit.log" 2>&1 \
   && has_match -F "cache verified at $REV1" <"$TMP/mirror-hit.log"; then
  ok "offline cache hit succeeds under configured Git mirror (insteadOf)"
else
  cat "$TMP/mirror-hit.log" >&2
  no "Git mirror offline cache hit control failed"
fi
mv "$MIRROR_ROOT/dummy-dep-offline.git" "$MIRROR_ROOT/dummy-dep.git"

echo "=== Finding 2: independent scratch-workspace object seeding controls ==="
# Simulate a developer checkout whose .lake/packages/dummy-dep has a local added lemma,
# prebuilt .lake/build outputs, AND a local HEAD commit on top of pinned REV1.
CALLER_WS="$TMP/caller-ws"
write_project "$CALLER_WS" "$DEP_BARE" "$REV1"
(cd "$CALLER_WS" && LAKE_FETCH_BACKOFF_BASE=0 bash "$HELPER") >"$TMP/caller-init.log" 2>&1
git -C "$CALLER_WS/.lake/packages/dummy-dep" fetch -q --no-tags origin
git -C "$CALLER_WS/.lake/packages/dummy-dep" checkout -q --detach "$REV2"
printf 'def localAddedLemma : Nat := 42\n' >>"$CALLER_WS/.lake/packages/dummy-dep/DummyDep.lean"
mkdir -p "$CALLER_WS/.lake/packages/dummy-dep/.lake/build/lib/lean"
printf 'local-olean\n' >"$CALLER_WS/.lake/packages/dummy-dep/.lake/build/lib/lean/DummyDep.olean"

SCRATCH_WS="$TMP/scratch-ws"
write_project "$SCRATCH_WS" "$DEP_BARE" "$REV1"
mv "$DEP_BARE" "$TMP/dep-offline-seed.git"
if (cd "$SCRATCH_WS" && LAKE_SEED_PACKAGES_DIR="$CALLER_WS/.lake/packages" LAKE_FETCH_BACKOFF_BASE=0 bash "$HELPER") >"$TMP/scratch-seed.log" 2>&1 \
   && [ ! -L "$SCRATCH_WS/.lake/packages" ] \
   && [ ! -L "$SCRATCH_WS/.lake/packages/dummy-dep" ] \
   && [ "$(git -C "$SCRATCH_WS/.lake/packages/dummy-dep" rev-parse HEAD)" = "$REV1" ] \
   && [ ! -e "$SCRATCH_WS/.lake/packages/dummy-dep/.lake" ] \
   && ! has_match -F "localAddedLemma" <"$SCRATCH_WS/.lake/packages/dummy-dep/DummyDep.lean" \
   && [ "$(cat "$SCRATCH_WS/.lake/packages/dummy-dep/DummyDep.lean")" = "def dummyVal : Nat := 1" ]; then
  ok "scratch workspace gets independent verified pinned checkout without caller edits, extra commits or .lake/build"
else
  cat "$TMP/scratch-seed.log" >&2
  no "scratch workspace isolation control failed"
fi
mv "$TMP/dep-offline-seed.git" "$DEP_BARE"

# If the caller's object store lacks the pinned commit, seeding falls back cleanly to fetching from manifest URL.
CALLER_WRONG="$TMP/caller-wrong"
write_project "$CALLER_WRONG" "$DEP_BARE" "$REV2"
(cd "$CALLER_WRONG" && LAKE_FETCH_BACKOFF_BASE=0 bash "$HELPER") >"$TMP/caller-wrong.log" 2>&1
SCRATCH_FB="$TMP/scratch-seed-fb"
write_project "$SCRATCH_FB" "$DEP_BARE" "$REV1"
if (cd "$SCRATCH_FB" && LAKE_SEED_PACKAGES_DIR="$CALLER_WRONG/.lake/packages" LAKE_FETCH_BACKOFF_BASE=0 bash "$HELPER") >"$TMP/scratch-seed-fb.log" 2>&1 \
   && has_match -F "seed object verification failed" <"$TMP/scratch-seed-fb.log" \
   && [ "$(git -C "$SCRATCH_FB/.lake/packages/dummy-dep" rev-parse HEAD)" = "$REV1" ]; then
  ok "scratch workspace falls back to verified remote fetch when caller seed lacks pinned commit"
else
  cat "$TMP/scratch-seed-fb.log" >&2
  no "scratch seed fallback control failed"
fi

# Structural control on scripts/proofs/trust_ban.sh: must use fetch_lake_packages.sh,
# never symlink .lake/packages, and never hide set -e inside an `if (` subshell.
if has_match -F "fetch_lake_packages.sh" <"$ROOT_DIR/scripts/proofs/trust_ban.sh" \
   && has_match -F "build_verified_scratch" <"$ROOT_DIR/scripts/proofs/trust_ban.sh" \
   && ! has_match -E 'ln -s.*\.lake/packages' <"$ROOT_DIR/scripts/proofs/trust_ban.sh" \
   && ! has_match -E '^[[:space:]]*if[[:space:]]*\([[:space:]]*$' <"$ROOT_DIR/scripts/proofs/trust_ban.sh"; then
  ok "trust_ban.sh populates its scratch workspace via fail-closed verified wrapper without symlinking or if-subshells"
else
  no "trust_ban.sh violates scratch-workspace fail-closed structural invariants"
fi

echo "=== bounded retry, fallback and single per-attempt deadline controls ==="
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

# 7b. Non-blocking 1: slow primary refusal (0.7s) + slow fallback (0.7s) share ONE 1s deadline.
PROJ_SHARED_TO="$TMP/proj-shared-timeout"
write_project "$PROJ_SHARED_TO" "$DEP_BARE" "$REV1"
rc=0
(cd "$PROJ_SHARED_TO" && PATH="$STUB_BIN:$PATH" STUB_MODE=slow-fallback \
  LAKE_FETCH_ATTEMPTS=1 LAKE_FETCH_BACKOFF_BASE=0 LAKE_FETCH_TIMEOUT=1 bash "$HELPER") >"$TMP/shared-timeout.log" 2>&1 || rc=$?
if [ "$rc" -ne 0 ] \
   && has_match -F "fetch timed out after 1s" <"$TMP/shared-timeout.log" \
   && [ ! -e "$PROJ_SHARED_TO/.lake/packages/dummy-dep" ]; then
  ok "primary fetch refusal and fallback fetch share one single per-attempt deadline"
else
  cat "$TMP/shared-timeout.log" >&2
  no "shared per-attempt deadline control failed (rc=$rc)"
fi

# 7c. Non-blocking 1 (round 2): stalled materialization checkout runs inside the single deadline
# (and only the verified materialization checkout runs — no redundant raw checkout).
PROJ_CO_TO="$TMP/proj-checkout-timeout"
write_project "$PROJ_CO_TO" "$DEP_BARE" "$REV1"
CO_COUNT_FILE="$TMP/co-timeout.count"
rm -f "$CO_COUNT_FILE"
rc=0
(cd "$PROJ_CO_TO" && PATH="$STUB_BIN:$PATH" STUB_MODE=slow-checkout STUB_CHECKOUT_COUNT_FILE="$CO_COUNT_FILE" \
  LAKE_FETCH_ATTEMPTS=1 LAKE_FETCH_BACKOFF_BASE=0 LAKE_FETCH_TIMEOUT=1 bash "$HELPER") >"$TMP/co-timeout.log" 2>&1 || rc=$?
if [ "$rc" -ne 0 ] \
   && [ "$(cat "$CO_COUNT_FILE" 2>/dev/null || echo 0)" = "1" ] \
   && has_match -F "fetch timed out after 1s" <"$TMP/co-timeout.log" \
   && [ ! -e "$PROJ_CO_TO/.lake/packages/dummy-dep" ]; then
  ok "stalled materialization checkout is killed by the same single per-attempt deadline"
else
  cat "$TMP/co-timeout.log" >&2
  no "materialization checkout deadline control failed (rc=$rc, co_count=$(cat "$CO_COUNT_FILE" 2>/dev/null || echo 0))"
fi

# 7d. Non-blocking 1 (round 2): combined fetch (0.65s) + materialization checkout (0.65s)
# shares ONE 1s per-attempt deadline and times out cleanly.
PROJ_COMBINED_TO="$TMP/proj-combined-timeout"
write_project "$PROJ_COMBINED_TO" "$DEP_BARE" "$REV1"
CO_COUNT_FILE="$TMP/combined-co.count"
rm -f "$CO_COUNT_FILE"
rc=0
(cd "$PROJ_COMBINED_TO" && PATH="$STUB_BIN:$PATH" STUB_MODE=slow-fetch-and-checkout STUB_CHECKOUT_COUNT_FILE="$CO_COUNT_FILE" \
  LAKE_FETCH_ATTEMPTS=1 LAKE_FETCH_BACKOFF_BASE=0 LAKE_FETCH_TIMEOUT=1 bash "$HELPER") >"$TMP/combined-timeout.log" 2>&1 || rc=$?
if [ "$rc" -ne 0 ] \
   && [ "$(cat "$CO_COUNT_FILE" 2>/dev/null || echo 0)" = "1" ] \
   && has_match -F "fetch timed out after 1s" <"$TMP/combined-timeout.log" \
   && [ ! -e "$PROJ_COMBINED_TO/.lake/packages/dummy-dep" ]; then
  ok "fetch and final materialization checkout share one single per-attempt deadline"
else
  cat "$TMP/combined-timeout.log" >&2
  no "combined fetch + materialization deadline control failed (rc=$rc, co_count=$(cat "$CO_COUNT_FILE" 2>/dev/null || echo 0))"
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
