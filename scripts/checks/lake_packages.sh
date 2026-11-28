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
#   - active Lake 4.35 package overrides (.lake/package-overrides.json and
#     LAKE_PKG_URL_MAP) -> rejected fail-closed;
#   - manifest subDir/configFile carrying and resolved-path containment inside the
#     verified checkout (including symlink-escape refusal);
#   - literal directory-entry pruning and exact final membership (catching newline-
#     suffixed directory names);
#   - failing git cleanliness queries (git exiting non-zero with empty stdout) ->
#     detected through one git_ok helper and verified to fail the gate;
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

# Single git query helper used everywhere in this gate: requires git to exit 0
# before its stdout (GIT_OUT) is ever inspected.
GIT_OUT=""
git_ok() {
  local out=""
  GIT_OUT=""
  out="$(git "$@")" || return 1
  GIT_OUT="$out"
}

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
git_ok -C "$DEP_SRC" rev-parse HEAD
REV1="$GIT_OUT"

printf 'def dummyVal : Nat := 2\n' >"$DEP_SRC/DummyDep.lean"
git -C "$DEP_SRC" add DummyDep.lean
git -C "$DEP_SRC" commit -qm "rev2"
git_ok -C "$DEP_SRC" rev-parse HEAD
REV2="$GIT_OUT"

git clone -q --bare "$DEP_SRC" "$DEP_BARE"

write_project() {
  local dir="$1"
  local url="$2"
  local rev="$3"
  local sub_dir_json="${4:-null}"
  local config_file_json="${5:-\"lakefile.toml\"}"
  rm -rf "$dir"
  mkdir -p "$dir"
  cp "$ROOT_DIR/lean-toolchain" "$dir/"
  cat >"$dir/lake-manifest.json" <<EOF
{"version": "1.3.0",
 "packagesDir": ".lake/packages",
 "packages":
 [{"url": "$url",
   "type": "git",
   "subDir": $sub_dir_json,
   "scope": "",
   "rev": "$rev",
   "name": "«dummy-dep»",
   "manifestFile": "lake-manifest.json",
   "inputRev": "$rev",
   "inherited": false,
   "configFile": $config_file_json}],
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
  if [ -n "\${STUB_PLANT_STRAY_DIR:-}" ]; then
    mkdir -p "\$STUB_PLANT_STRAY_DIR"
    printf 'planted-during-fetch\n' >"\$STUB_PLANT_STRAY_DIR/marker"
  fi
fi
exec "\$REAL_GIT" "\$@"
EOF
chmod +x "$STUB_BIN/git"

echo "=== cold fetch and offline cache hit ==="
PROJ="$TMP/proj-cold"
write_project "$PROJ" "$DEP_BARE" "$REV1"
if (cd "$PROJ" && LAKE_FETCH_BACKOFF_BASE=0 bash "$HELPER") >"$TMP/cold.log" 2>&1 \
   && git_ok -C "$PROJ/.lake/packages/dummy-dep" rev-parse HEAD \
   && [ "$GIT_OUT" = "$REV1" ] \
   && git_ok -C "$PROJ/.lake/packages/dummy-dep" status --porcelain --untracked-files=all --ignored=matching \
   && [ -z "$GIT_OUT" ] \
   && [ ! -e "$PROJ/.lake/packages/dummy-dep/.lake" ] \
   && git_ok -C "$PROJ/.lake/packages/dummy-dep" config --local --list \
   && ! has_match -E '^(remote\.origin\.promisor|extensions\.partialclone)=' <<<"$GIT_OUT"; then
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
   && git_ok -C "$PROJ/.lake/packages/dummy-dep" ls-files -v \
   && ! has_match -E '^[a-z]' <<<"$GIT_OUT"; then
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
git_ok -C "$PROJ/.lake/packages/dummy-dep" rev-parse HEAD && [ "$GIT_OUT" = "$REV2" ]
if (cd "$PROJ" && LAKE_FETCH_BACKOFF_BASE=0 bash "$HELPER") >"$TMP/mismatch.log" 2>&1 \
   && has_match -F "HEAD is $REV2, manifest pins $REV1" <"$TMP/mismatch.log" \
   && git_ok -C "$PROJ/.lake/packages/dummy-dep" rev-parse HEAD \
   && [ "$GIT_OUT" = "$REV1" ]; then
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
   && git_ok -C "$PROJ/.lake/packages/dummy-dep" rev-parse HEAD \
   && [ "$GIT_OUT" = "$REV1" ]; then
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
if ! git_ok -C "$PROJ/.lake/packages/dummy-dep" status --porcelain --untracked-files=all --ignored=matching; then
  no "assume-unchanged setup failed: git status exited non-zero"
elif [ -n "$GIT_OUT" ]; then
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
if ! git_ok -C "$PROJ/.lake/packages/dummy-dep" status --porcelain --untracked-files=all --ignored=matching; then
  no "skip-worktree setup failed: git status exited non-zero"
elif [ -n "$GIT_OUT" ]; then
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
git_ok -C "$PROJ/.lake/packages/dummy-dep" rev-parse "${REV1}:DummyDep.lean"
BLOB_OID="$GIT_OUT"
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
   && git_ok -C "$PROJ/.lake/packages/dummy-dep" config --local --get remote.origin.url \
   && [ "$GIT_OUT" = "$DEP_BARE" ] \
   && git_ok -C "$PROJ/.lake/packages/dummy-dep" rev-parse HEAD \
   && [ "$GIT_OUT" = "$REV1" ]; then
  ok "origin URL mismatch after restore deletes package and refetches from manifest URL"
else
  cat "$TMP/origin.log" >&2
  no "origin URL mismatch control failed"
fi

# 3c. Corrupt .git metadata after restore -> deleted and refetched cleanly.
printf 'not-a-valid-ref\n' >"$PROJ/.lake/packages/dummy-dep/.git/HEAD"
if (cd "$PROJ" && LAKE_FETCH_BACKOFF_BASE=0 bash "$HELPER") >"$TMP/corrupt.log" 2>&1 \
   && has_match -F "unreadable HEAD commit" <"$TMP/corrupt.log" \
   && git_ok -C "$PROJ/.lake/packages/dummy-dep" rev-parse HEAD \
   && [ "$GIT_OUT" = "$REV1" ]; then
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
   && git_ok -C "$PROJ/.lake/packages/dummy-dep" rev-parse HEAD \
   && [ "$GIT_OUT" = "$REV1" ]; then
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
   && git_ok -C "$PROJ/.lake/packages/dummy-dep" rev-parse HEAD \
   && [ "$GIT_OUT" = "$REV1" ]; then
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
   && (GIT_CONFIG_GLOBAL="$MIRROR_CFG" git_ok -C "$PROJ_MIRROR/.lake/packages/dummy-dep" remote get-url origin && [ "$GIT_OUT" = "$MIRROR_ROOT/dummy-dep.git" ]) \
   && git_ok config --file "$PROJ_MIRROR/.lake/packages/dummy-dep/.git/config" --get remote.origin.url \
   && [ "$GIT_OUT" = "$MANIFEST_URL" ] \
   && git_ok -C "$PROJ_MIRROR/.lake/packages/dummy-dep" rev-parse HEAD \
   && [ "$GIT_OUT" = "$REV1" ]; then
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
   && git_ok -C "$SCRATCH_WS/.lake/packages/dummy-dep" rev-parse HEAD \
   && [ "$GIT_OUT" = "$REV1" ] \
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
   && git_ok -C "$SCRATCH_FB/.lake/packages/dummy-dep" rev-parse HEAD \
   && [ "$GIT_OUT" = "$REV1" ]; then
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
   && git_ok -C "$PROJ_RETRY/.lake/packages/dummy-dep" rev-parse HEAD \
   && [ "$GIT_OUT" = "$REV1" ]; then
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
   && find "$PROJ_FAIL/.lake/packages" -mindepth 1 -maxdepth 1 -print0 >"$TMP/fail-entries.nul" \
   && [ ! -s "$TMP/fail-entries.nul" ]; then
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
   && git_ok -C "$PROJ_FB/.lake/packages/dummy-dep" rev-parse HEAD \
   && [ "$GIT_OUT" = "$REV1" ]; then
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
expect_manifest_refusal "bad-lake-dir" "expected lakeDir" \
  '{"lakeDir":".other","packagesDir":".lake/packages","packages":[]}'
expect_manifest_refusal "subdir-traversal" "unsafe subDir" \
  '{"packagesDir":".lake/packages","packages":[{"type":"git","name":"dep","url":"https://example.invalid/dep","rev":"0123456789abcdef0123456789abcdef01234567","subDir":"../outside"}]}'
expect_manifest_refusal "subdir-absolute" "unsafe subDir" \
  '{"packagesDir":".lake/packages","packages":[{"type":"git","name":"dep","url":"https://example.invalid/dep","rev":"0123456789abcdef0123456789abcdef01234567","subDir":"/tmp/outside"}]}'
expect_manifest_refusal "configfile-traversal" "unsafe configFile" \
  '{"packagesDir":".lake/packages","packages":[{"type":"git","name":"dep","url":"https://example.invalid/dep","rev":"0123456789abcdef0123456789abcdef01234567","configFile":"../outside.toml"}]}'
expect_manifest_refusal "configfile-absolute" "unsafe configFile" \
  '{"packagesDir":".lake/packages","packages":[{"type":"git","name":"dep","url":"https://example.invalid/dep","rev":"0123456789abcdef0123456789abcdef01234567","configFile":"/tmp/outside.toml"}]}'
expect_manifest_refusal "configfile-bad-ext" "unsupported configFile extension" \
  '{"packagesDir":".lake/packages","packages":[{"type":"git","name":"dep","url":"https://example.invalid/dep","rev":"0123456789abcdef0123456789abcdef01234567","configFile":"lakefile.py"}]}'

echo "=== Finding 2 (closing round): Lake 4.35 override rejection and subDir/configFile containment ==="
# Active workspace override file (.lake/package-overrides.json) must be rejected even when
# lake-manifest.json and .lake/packages/dummy-dep are otherwise clean.
PROJ_OVR="$TMP/proj-overrides"
write_project "$PROJ_OVR" "$DEP_BARE" "$REV1"
(cd "$PROJ_OVR" && LAKE_FETCH_BACKOFF_BASE=0 bash "$HELPER") >"$TMP/ovr-init.log" 2>&1
LOCAL_OVR_DEP="$TMP/local-override-dep"
mkdir -p "$LOCAL_OVR_DEP"
printf 'name = "dummy-dep"\n[[lean_lib]]\nname = "DummyDep"\n' >"$LOCAL_OVR_DEP/lakefile.toml"
printf 'def dummyVal : Nat := 999\n' >"$LOCAL_OVR_DEP/DummyDep.lean"
cat >"$PROJ_OVR/.lake/package-overrides.json" <<EOF
{"schemaVersion": "1.3.0",
 "packages":
 [{"type": "path",
   "name": "«dummy-dep»",
   "dir": "$LOCAL_OVR_DEP",
   "inherited": false,
   "configFile": "lakefile.toml"}]}
EOF
rc=0
(cd "$PROJ_OVR" && LAKE_FETCH_BACKOFF_BASE=0 bash "$HELPER") >"$TMP/ovr-file.log" 2>&1 || rc=$?
if [ "$rc" -ne 0 ] && has_match -F "refusing active Lake package override file .lake/package-overrides.json" <"$TMP/ovr-file.log"; then
  ok "active .lake/package-overrides.json is rejected fail-closed"
else
  cat "$TMP/ovr-file.log" >&2
  no "active .lake/package-overrides.json was not rejected (rc=$rc)"
fi

# Symlink at .lake/package-overrides.json (even dangling) must also be rejected.
rm -f "$PROJ_OVR/.lake/package-overrides.json"
ln -s "$TMP/no-such-overrides.json" "$PROJ_OVR/.lake/package-overrides.json"
rc=0
(cd "$PROJ_OVR" && LAKE_FETCH_BACKOFF_BASE=0 bash "$HELPER") >"$TMP/ovr-symlink.log" 2>&1 || rc=$?
if [ "$rc" -ne 0 ] && has_match -F "refusing active Lake package override file .lake/package-overrides.json" <"$TMP/ovr-symlink.log"; then
  ok "symlink at .lake/package-overrides.json is rejected fail-closed"
else
  cat "$TMP/ovr-symlink.log" >&2
  no "symlink at .lake/package-overrides.json was not rejected (rc=$rc)"
fi

# Removing .lake/package-overrides.json restores clean verification.
rm -f "$PROJ_OVR/.lake/package-overrides.json"
if (cd "$PROJ_OVR" && LAKE_FETCH_BACKOFF_BASE=0 bash "$HELPER") >"$TMP/ovr-clean.log" 2>&1 \
   && has_match -F "cache verified at $REV1" <"$TMP/ovr-clean.log"; then
  ok "removing .lake/package-overrides.json restores verified cache hit"
else
  cat "$TMP/ovr-clean.log" >&2
  no "removing .lake/package-overrides.json did not restore clean verification"
fi

# Active LAKE_PKG_URL_MAP environment override (Lake/Config/Env.lean) must be rejected,
# while empty `{}` (exported by `lake env` when unset) is allowed.
rc=0
(cd "$PROJ_OVR" && LAKE_PKG_URL_MAP='{"dummy-dep":"/tmp/evil-dep.git"}' LAKE_FETCH_BACKOFF_BASE=0 bash "$HELPER") >"$TMP/ovr-urlmap.log" 2>&1 || rc=$?
if [ "$rc" -ne 0 ] && has_match -F "refusing active Lake package URL override via LAKE_PKG_URL_MAP" <"$TMP/ovr-urlmap.log"; then
  ok "active LAKE_PKG_URL_MAP environment override is rejected fail-closed"
else
  cat "$TMP/ovr-urlmap.log" >&2
  no "active LAKE_PKG_URL_MAP was not rejected (rc=$rc)"
fi
if (cd "$PROJ_OVR" && LAKE_PKG_URL_MAP='{}' LAKE_FETCH_BACKOFF_BASE=0 bash "$HELPER") >"$TMP/ovr-urlmap-empty.log" 2>&1 \
   && has_match -F "cache verified at $REV1" <"$TMP/ovr-urlmap-empty.log"; then
  ok "empty LAKE_PKG_URL_MAP={} (as exported by lake env) passes verification"
else
  cat "$TMP/ovr-urlmap-empty.log" >&2
  no "empty LAKE_PKG_URL_MAP={} was unexpectedly rejected"
fi

# Build a dependency repository with a valid subDir + custom configFile, plus commits
# with symlink escapes for subDir and configFile pointing to an outside target.
OUTSIDE_PKG="$TMP/outside-pkg"
mkdir -p "$OUTSIDE_PKG"
printf 'name = "outside"\n[[lean_lib]]\nname = "Outside"\n' >"$OUTSIDE_PKG/lakefile.toml"
printf 'def outsideVal : Nat := 777\n' >"$OUTSIDE_PKG/Outside.lean"

SUB_SRC="$TMP/sub-src"
SUB_BARE="$TMP/sub.git"
git init -q "$SUB_SRC"
mkdir -p "$SUB_SRC/pkgs/nested"
printf 'name = "dummy-dep"\n[[lean_lib]]\nname = "NestedDep"\n' >"$SUB_SRC/pkgs/nested/custom.toml"
printf 'name = "dummy-dep"\n[[lean_lib]]\nname = "NestedDep"\n' >"$SUB_SRC/pkgs/nested/lakefile.toml"
printf 'def nestedVal : Nat := 1\n' >"$SUB_SRC/pkgs/nested/NestedDep.lean"
cp "$ROOT_DIR/lean-toolchain" "$SUB_SRC/"
git -C "$SUB_SRC" add lean-toolchain pkgs/nested/custom.toml pkgs/nested/lakefile.toml pkgs/nested/NestedDep.lean
git -C "$SUB_SRC" commit -qm "valid-subdir"
git_ok -C "$SUB_SRC" rev-parse HEAD
REV_SUB_OK="$GIT_OUT"

ln -s "$OUTSIDE_PKG" "$SUB_SRC/pkg-link"
git -C "$SUB_SRC" add pkg-link
git -C "$SUB_SRC" commit -qm "subdir-symlink-escape"
git_ok -C "$SUB_SRC" rev-parse HEAD
REV_SUB_ESCAPE="$GIT_OUT"

mkdir -p "$SUB_SRC/cfg-escape"
# Use a relative symlink so the escape resolves from inside .lake/packages/dummy-dep/cfg-escape/
# out to $TMP/outside-pkg/lakefile.toml, matching real-world git repo symlink attacks.
ln -s "../../../../../outside-pkg/lakefile.toml" "$SUB_SRC/cfg-escape/lakefile.toml"
git -C "$SUB_SRC" add cfg-escape/lakefile.toml
git -C "$SUB_SRC" commit -qm "config-symlink-escape"
git_ok -C "$SUB_SRC" rev-parse HEAD
REV_CFG_ESCAPE="$GIT_OUT"

git clone -q --bare "$SUB_SRC" "$SUB_BARE"

# Valid subDir + custom configFile passes cold fetch and offline cache hit.
PROJ_SUB_OK="$TMP/proj-subdir-ok"
write_project "$PROJ_SUB_OK" "$SUB_BARE" "$REV_SUB_OK" '"pkgs/nested"' '"custom.toml"'
if (cd "$PROJ_SUB_OK" && LAKE_FETCH_ATTEMPTS=1 LAKE_FETCH_BACKOFF_BASE=0 bash "$HELPER") >"$TMP/sub-ok.log" 2>&1 \
   && git_ok -C "$PROJ_SUB_OK/.lake/packages/dummy-dep" rev-parse HEAD \
   && [ "$GIT_OUT" = "$REV_SUB_OK" ]; then
  ok "valid manifest subDir and custom configFile pass verification"
else
  cat "$TMP/sub-ok.log" >&2
  no "valid manifest subDir + custom configFile control failed"
fi

# Valid subDir + null configFile (Lake default "lakefile" -> lakefile.toml) passes.
PROJ_SUB_DEF="$TMP/proj-subdir-default-cfg"
write_project "$PROJ_SUB_DEF" "$SUB_BARE" "$REV_SUB_OK" '"pkgs/nested"' 'null'
if (cd "$PROJ_SUB_DEF" && LAKE_FETCH_ATTEMPTS=1 LAKE_FETCH_BACKOFF_BASE=0 bash "$HELPER") >"$TMP/sub-def.log" 2>&1 \
   && git_ok -C "$PROJ_SUB_DEF/.lake/packages/dummy-dep" rev-parse HEAD \
   && [ "$GIT_OUT" = "$REV_SUB_OK" ]; then
  ok "valid manifest subDir with default null configFile resolves lakefile.toml cleanly"
else
  cat "$TMP/sub-def.log" >&2
  no "valid manifest subDir + default configFile control failed"
fi

# Symlink subDir escaping verified checkout must fail closed with no package left on disk.
PROJ_SUB_ESC="$TMP/proj-subdir-escape"
write_project "$PROJ_SUB_ESC" "$SUB_BARE" "$REV_SUB_ESCAPE" '"pkg-link"' '"lakefile.toml"'
rc=0
(cd "$PROJ_SUB_ESC" && LAKE_FETCH_ATTEMPTS=1 LAKE_FETCH_BACKOFF_BASE=0 bash "$HELPER") >"$TMP/sub-esc.log" 2>&1 || rc=$?
if [ "$rc" -ne 0 ] \
   && has_match -F "resolved package path for subDir 'pkg-link' escapes verified checkout" <"$TMP/sub-esc.log" \
   && [ ! -e "$PROJ_SUB_ESC/.lake/packages/dummy-dep" ]; then
  ok "subDir symlink escaping verified checkout fails closed with no package left on disk"
else
  cat "$TMP/sub-esc.log" >&2
  no "subDir symlink escape control did not fail closed (rc=$rc)"
fi

# Symlink configFile escaping verified checkout must fail closed with no package left on disk.
PROJ_CFG_ESC="$TMP/proj-cfg-escape"
write_project "$PROJ_CFG_ESC" "$SUB_BARE" "$REV_CFG_ESCAPE" '"cfg-escape"' '"lakefile.toml"'
rc=0
(cd "$PROJ_CFG_ESC" && LAKE_FETCH_ATTEMPTS=1 LAKE_FETCH_BACKOFF_BASE=0 bash "$HELPER") >"$TMP/cfg-esc.log" 2>&1 || rc=$?
if [ "$rc" -ne 0 ] \
   && has_match -F "resolved config path for 'lakefile.toml' escapes verified checkout" <"$TMP/cfg-esc.log" \
   && [ ! -e "$PROJ_CFG_ESC/.lake/packages/dummy-dep" ]; then
  ok "configFile symlink escaping verified checkout fails closed with no package left on disk"
else
  cat "$TMP/cfg-esc.log" >&2
  no "configFile symlink escape control did not fail closed (rc=$rc)"
fi

# Missing subDir and missing configFile inside verified checkout must fail closed.
PROJ_SUB_MISS="$TMP/proj-subdir-missing"
write_project "$PROJ_SUB_MISS" "$SUB_BARE" "$REV_SUB_OK" '"no-such-subdir"' '"lakefile.toml"'
rc=0
(cd "$PROJ_SUB_MISS" && LAKE_FETCH_ATTEMPTS=1 LAKE_FETCH_BACKOFF_BASE=0 bash "$HELPER") >"$TMP/sub-miss.log" 2>&1 || rc=$?
if [ "$rc" -ne 0 ] \
   && has_match -F "package subDir 'no-such-subdir' not found in verified checkout" <"$TMP/sub-miss.log" \
   && [ ! -e "$PROJ_SUB_MISS/.lake/packages/dummy-dep" ]; then
  ok "missing manifest subDir in verified checkout fails closed"
else
  cat "$TMP/sub-miss.log" >&2
  no "missing manifest subDir control did not fail closed (rc=$rc)"
fi

PROJ_CFG_MISS="$TMP/proj-cfg-missing"
write_project "$PROJ_CFG_MISS" "$SUB_BARE" "$REV_SUB_OK" '"pkgs/nested"' '"missing.toml"'
rc=0
(cd "$PROJ_CFG_MISS" && LAKE_FETCH_ATTEMPTS=1 LAKE_FETCH_BACKOFF_BASE=0 bash "$HELPER") >"$TMP/cfg-miss.log" 2>&1 || rc=$?
if [ "$rc" -ne 0 ] \
   && has_match -F "configFile 'missing.toml' not found in verified package directory" <"$TMP/cfg-miss.log" \
   && [ ! -e "$PROJ_CFG_MISS/.lake/packages/dummy-dep" ]; then
  ok "missing manifest configFile in verified checkout fails closed"
else
  cat "$TMP/cfg-miss.log" >&2
  no "missing manifest configFile control did not fail closed (rc=$rc)"
fi

echo "=== Non-blocking (closing round): literal directory entries with trailing newline ==="
# 1. Pre-existing directory named "dummy-dep\n" must be pruned (never matched as "dummy-dep").
NL_STRAY="$PROJ/.lake/packages/"$'dummy-dep\n'
mkdir -p "$NL_STRAY"
printf 'sneaky\n' >"$NL_STRAY/file"
if (cd "$PROJ" && LAKE_FETCH_BACKOFF_BASE=0 bash "$HELPER") >"$TMP/nl-prune.log" 2>&1 \
   && has_match -F "pruning unexpected entry" <"$TMP/nl-prune.log" \
   && [ ! -e "$NL_STRAY" ] \
   && python3 -I - "$PROJ/.lake/packages" <<'PY'; then
import os, sys
actual = sorted(e.name for e in os.scandir(sys.argv[1]))
sys.exit(0 if actual == ["dummy-dep"] else 1)
PY
  ok "unlisted directory named with trailing newline is pruned and exact literal membership holds"
else
  cat "$TMP/nl-prune.log" >&2
  no "newline-suffixed directory pruning control failed"
fi

# 2. Final post-condition exact-membership control: if "dummy-dep\n" appears after initial
# pruning (during fetch), verify_manifest_packages_present must fail closed.
PROJ_NL_POST="$TMP/proj-nl-post"
write_project "$PROJ_NL_POST" "$DEP_BARE" "$REV1"
NL_POST_STRAY="$PROJ_NL_POST/.lake/packages/"$'dummy-dep\n'
rc=0
(cd "$PROJ_NL_POST" && PATH="$STUB_BIN:$PATH" STUB_PLANT_STRAY_DIR="$NL_POST_STRAY" \
  LAKE_FETCH_ATTEMPTS=1 LAKE_FETCH_BACKOFF_BASE=0 bash "$HELPER") >"$TMP/nl-post.log" 2>&1 || rc=$?
if [ "$rc" -ne 0 ] \
   && has_match -F "post-condition failed: unexpected entries in .lake/packages" <"$TMP/nl-post.log" \
   && has_match -F "dummy-dep\\n" <"$TMP/nl-post.log"; then
  ok "post-condition exact membership check rejects newline-suffixed unlisted directory"
else
  cat "$TMP/nl-post.log" >&2
  no "post-condition newline membership control did not fail closed (rc=$rc)"
fi

if [ "${LAKE_PACKAGES_NO_SELF_CONTROL:-0}" != "1" ]; then
  echo "=== Finding 1 (closing round): non-zero git cleanliness query fails the gate ==="
  FAIL_STATUS_BIN="$TMP/fail-status-bin"
  mkdir -p "$FAIL_STATUS_BIN"
  cat >"$FAIL_STATUS_BIN/git" <<EOF
#!/usr/bin/env bash
set -euo pipefail
REAL_GIT="$REAL_GIT"
args=("\$@")
i=0
subcmd=""
while [ "\$i" -lt "\${#args[@]}" ]; do
  tok="\${args[\$i]}"
  case "\$tok" in
    -C|-c|--git-dir|--work-tree|--namespace|--config-env)
      i=\$((i + 2))
      continue
      ;;
    --*)
      i=\$((i + 1))
      continue
      ;;
    -*)
      i=\$((i + 1))
      continue
      ;;
    *)
      subcmd="\$tok"
      break
      ;;
  esac
done
if [ "\$subcmd" = "status" ]; then
  exit 2
fi
exec "\$REAL_GIT" "\$@"
EOF
  chmod +x "$FAIL_STATUS_BIN/git"

  # Direct unit control on git_ok itself.
  if (PATH="$FAIL_STATUS_BIN:$PATH" git_ok -C "$DEP_SRC" status --porcelain); then
    no "git_ok unexpectedly succeeded when git status exited 2 with empty stdout"
  else
    ok "git_ok returns non-zero when git status exits 2 with empty stdout"
  fi

  # End-to-end gate control reproducing Codex's attack: only git status exits 2 with empty stdout;
  # the gate MUST fail all three git-status controls and exit non-zero.
  rc=0
  PATH="$FAIL_STATUS_BIN:$PATH" LAKE_PACKAGES_NO_SELF_CONTROL=1 \
    bash "$ROOT_DIR/scripts/checks/lake_packages.sh" >"$TMP/git-status-fail.log" 2>&1 || rc=$?
  if [ "$rc" -ne 0 ] \
     && has_match -F "FAIL cold fetch failed" <"$TMP/git-status-fail.log" \
     && has_match -F "FAIL assume-unchanged setup failed: git status exited non-zero" <"$TMP/git-status-fail.log" \
     && has_match -F "FAIL skip-worktree setup failed: git status exited non-zero" <"$TMP/git-status-fail.log" \
     && has_match -E '^lake_packages: PASS=[0-9]+ FAIL=3$' <"$TMP/git-status-fail.log"; then
    ok "gate fails closed (FAIL=3, rc=$rc) when git status exits 2 with empty stdout"
  else
    tail -30 "$TMP/git-status-fail.log" >&2
    no "failing git status did not fail all three gate controls as expected (rc=$rc)"
  fi
fi

echo
echo "lake_packages: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
