#!/usr/bin/env bash
# Restore verification and bounded-retry fetch for pinned Lake dependency sources.
#
# Reads lake-manifest.json and lean-toolchain from the current working directory.
# Never trusts restored .git metadata, .git/index flags (such as assume-unchanged
# or skip-worktree), git status, or rewritten remote URLs:
#   - verifies stored remote.origin.url in .git/config directly (ignoring url.*.insteadOf
#     rewrites so configured mirrors work) and refuses partial-clone promisor configs;
#   - scans the restored tree for .lake directories without suppressing traversal errors;
#   - resolves stored HEAD without executing untrusted repo config or hooks;
#   - transfers commit <rev> and its tree/blob closure into a fresh trusted git repo
#     with strict object verification and fsck, materializes a fresh checkout, verifies
#     the tree hash against <rev>^{tree} in an independent index, compares the restored
#     working tree byte-for-byte against the fresh checkout, and replaces the restored
#     directory with the fresh verified checkout;
#   - deletes and refetches any missing, mismatched, dirty, unreadable or corrupt package
#     with bounded retries, exponential backoff and one single deadline per attempt.
#
# Environment configuration:
#   LAKE_FETCH_ATTEMPTS      max fetch attempts per package (default: 3)
#   LAKE_FETCH_BACKOFF_BASE  initial retry delay in seconds, doubled per retry (default: 10)
#   LAKE_FETCH_TIMEOUT       single per-attempt deadline in seconds across fetch,
#                            fallback fetch and checkout (default: 180)
#   LAKE_SEED_PACKAGES_DIR   optional directory of existing package repos used ONLY as
#                            an untrusted object-store seed when a package is absent
#                            (only verified commit <rev> objects are extracted into a
#                            fresh checkout; working-tree edits and build outputs are
#                            never inherited)
set -euo pipefail

HELPER_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
BOUNDED_EXEC="$HELPER_ROOT/scripts/checks/lib/bounded_exec.py"
source "$HELPER_ROOT/scripts/checks/lib/git_sandbox.sh" || exit 2
git_sandbox || exit 2

MANIFEST="lake-manifest.json"
TOOLCHAIN="lean-toolchain"
ATTEMPTS="${LAKE_FETCH_ATTEMPTS:-3}"
BACKOFF_BASE="${LAKE_FETCH_BACKOFF_BASE:-10}"
FETCH_TIMEOUT="${LAKE_FETCH_TIMEOUT:-180}"
SEED_PACKAGES_DIR="${LAKE_SEED_PACKAGES_DIR:-}"

if [[ ! "$ATTEMPTS" =~ ^[1-9][0-9]*$ ]]; then
  echo "error: lake-packages: LAKE_FETCH_ATTEMPTS must be a positive integer (got '$ATTEMPTS')" >&2
  exit 2
fi
if [[ ! "$BACKOFF_BASE" =~ ^[0-9]+$ ]]; then
  echo "error: lake-packages: LAKE_FETCH_BACKOFF_BASE must be a non-negative integer (got '$BACKOFF_BASE')" >&2
  exit 2
fi
if ! python3 -I - "$FETCH_TIMEOUT" <<'PY'; then
import math, sys
try:
    v = float(sys.argv[1])
except ValueError:
    sys.exit(1)
sys.exit(0 if math.isfinite(v) and v > 0 else 1)
PY
  echo "error: lake-packages: LAKE_FETCH_TIMEOUT must be a positive finite number (got '$FETCH_TIMEOUT')" >&2
  exit 2
fi

if [ ! -f "$TOOLCHAIN" ] || [ -L "$TOOLCHAIN" ] || [ ! -s "$TOOLCHAIN" ]; then
  echo "error: lake-packages: $TOOLCHAIN must exist as a non-empty regular file" >&2
  exit 2
fi
if [ ! -f "$MANIFEST" ] || [ -L "$MANIFEST" ]; then
  echo "error: lake-packages: $MANIFEST must exist as a regular file" >&2
  exit 2
fi
if [ -L ".lake" ]; then
  echo "error: lake-packages: refusing symlink at .lake" >&2
  exit 1
fi

safe_rm_rf() {
  local target="$1"
  [ -n "$target" ] || return 0
  python3 -I - "$target" <<'PY'
import os
import shutil
import stat
import sys

path = sys.argv[1]
try:
    st = os.lstat(path)
except FileNotFoundError:
    sys.exit(0)

if stat.S_ISLNK(st.st_mode) or not stat.S_ISDIR(st.st_mode):
    os.unlink(path)
    sys.exit(0)

def unlock_tree(root):
    try:
        os.chmod(root, 0o700)
    except OSError:
        pass
    try:
        entries = list(os.scandir(root))
    except OSError:
        return
    for entry in entries:
        try:
            est = entry.stat(follow_symlinks=False)
        except OSError:
            continue
        if stat.S_ISDIR(est.st_mode) and not stat.S_ISLNK(est.st_mode):
            unlock_tree(entry.path)
        elif stat.S_ISREG(est.st_mode):
            try:
                os.chmod(entry.path, 0o600)
            except OSError:
                pass

unlock_tree(path)
shutil.rmtree(path)
PY
}

TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/lake-packages.XXXXXX")"
CURRENT_STAGE=""
CURRENT_FRESH=""
cleanup() {
  [ -z "${CURRENT_FRESH:-}" ] || safe_rm_rf "$CURRENT_FRESH" || true
  [ -z "${CURRENT_STAGE:-}" ] || safe_rm_rf "$CURRENT_STAGE" || true
  [ -z "${TMP_DIR:-}" ] || safe_rm_rf "$TMP_DIR" || true
}
trap cleanup EXIT

MANIFEST_ROWS="$TMP_DIR/manifest.tsv"
python3 -I - "$MANIFEST" >"$MANIFEST_ROWS" <<'PY'
import json
import re
import sys
from pathlib import Path

manifest_path = Path(sys.argv[1])
try:
    data = json.loads(manifest_path.read_text(encoding="utf-8"))
except (OSError, UnicodeDecodeError, json.JSONDecodeError) as exc:
    sys.exit(f"error: lake-packages: cannot parse {manifest_path}: {exc}")

if not isinstance(data, dict):
    sys.exit(f"error: lake-packages: {manifest_path} must be a JSON object")

packages_dir = data.get("packagesDir")
if packages_dir != ".lake/packages":
    sys.exit(
        f"error: lake-packages: expected packagesDir '.lake/packages', got {packages_dir!r}"
    )

packages = data.get("packages")
if not isinstance(packages, list):
    sys.exit(f"error: lake-packages: {manifest_path} packages field must be a list")

seen = set()
for index, entry in enumerate(packages):
    if not isinstance(entry, dict):
        sys.exit(f"error: lake-packages: package entry {index} must be an object")
    pkg_type = entry.get("type")
    if pkg_type != "git":
        sys.exit(f"error: lake-packages: package entry {index} has unsupported type {pkg_type!r}")
    raw_name = entry.get("name")
    if not isinstance(raw_name, str) or not raw_name:
        sys.exit(f"error: lake-packages: package entry {index} missing name")
    name = raw_name[1:-1] if raw_name.startswith("«") and raw_name.endswith("»") else raw_name
    if (
        not name
        or name in {".", ".."}
        or name.startswith(".")
        or any(ch in name for ch in "/\\\t\r\n\0«»")
    ):
        sys.exit(f"error: lake-packages: unsafe package name {raw_name!r}")
    if name in seen:
        sys.exit(f"error: lake-packages: duplicate package name {name!r}")
    seen.add(name)

    url = entry.get("url")
    if (
        not isinstance(url, str)
        or not url
        or url.startswith("-")
        or any(ch in url for ch in "\t\r\n\0")
    ):
        sys.exit(f"error: lake-packages: package {name} has unsafe url {url!r}")

    rev = entry.get("rev")
    if not isinstance(rev, str) or not re.fullmatch(r"[0-9a-f]{40}", rev):
        sys.exit(
            f"error: lake-packages: package {name} rev must be a 40-char hex commit (got {rev!r})"
        )

    print(f"{name}\t{url}\t{rev}")
PY

PACKAGES_DIR=".lake/packages"
if [ -L "$PACKAGES_DIR" ] || { [ -e "$PACKAGES_DIR" ] && [ ! -d "$PACKAGES_DIR" ]; }; then
  echo "warn lake-packages: removing non-directory $PACKAGES_DIR" >&2
  safe_rm_rf "$PACKAGES_DIR"
fi
mkdir -p "$PACKAGES_DIR"

# Prune any unexpected or leftover entries under .lake/packages not listed in lake-manifest.json.
declare -A EXPECTED_PKGS=()
while IFS=$'\t' read -r name _url _rev; do
  [ -n "$name" ] || continue
  EXPECTED_PKGS["$name"]=1
done <"$MANIFEST_ROWS"

shopt -s nullglob dotglob
for entry in "$PACKAGES_DIR"/*; do
  base="$(basename "$entry")"
  if [ -z "${EXPECTED_PKGS[$base]+x}" ]; then
    echo "warn lake-packages: pruning unexpected entry $entry" >&2
    safe_rm_rf "$entry"
  fi
done
shopt -u nullglob dotglob

# Strictly scan DIR for any top-level or nested .lake path without suppressing traversal errors.
scan_no_lake_dirs() {
  local dir="$1"
  python3 -I - "$dir" <<'PY'
import os
import stat
import sys

root = sys.argv[1]
try:
    st = os.lstat(root)
except OSError as exc:
    print(f"cannot inspect package directory: {exc}")
    sys.exit(1)
if stat.S_ISLNK(st.st_mode) or not stat.S_ISDIR(st.st_mode):
    print("missing or symlink directory")
    sys.exit(1)

git_path = os.path.join(root, ".git")
try:
    git_st = os.lstat(git_path)
except OSError:
    print("missing or non-directory .git")
    sys.exit(1)
if stat.S_ISLNK(git_st.st_mode) or not stat.S_ISDIR(git_st.st_mode):
    print("missing or non-directory .git")
    sys.exit(1)

top_lake = os.path.join(root, ".lake")
try:
    os.lstat(top_lake)
    print("unexpected .lake directory inside dependency")
    sys.exit(1)
except FileNotFoundError:
    pass
except OSError as exc:
    print(f"filesystem traversal error at {top_lake}: {exc}")
    sys.exit(1)

def walk_dir(current, is_root):
    try:
        entries = list(os.scandir(current))
    except OSError as exc:
        print(f"filesystem traversal error at {current}: {exc}")
        sys.exit(1)
    for entry in entries:
        if is_root and entry.name == ".git":
            continue
        if entry.name == ".lake":
            print(f"unexpected nested .lake path: {entry.path}")
            sys.exit(1)
        try:
            est = entry.stat(follow_symlinks=False)
        except OSError as exc:
            print(f"filesystem traversal error at {entry.path}: {exc}")
            sys.exit(1)
        if stat.S_ISDIR(est.st_mode) and not stat.S_ISLNK(est.st_mode):
            walk_dir(entry.path, False)

walk_dir(root, True)
PY
}

# Verify literal stored origin URL and absence of partial-clone promisor config directly
# from DIR/.git/config without applying caller url.*.insteadOf rewrites.
verify_stored_config() {
  local dir="$1"
  local url="$2"
  local cfg="$dir/.git/config"
  local remote_urls=""
  local promisor=""
  local partial=""
  local rc=0

  if [ ! -f "$cfg" ] || [ -L "$cfg" ]; then
    echo "missing or symlink .git/config"
    return 1
  fi

  remote_urls="$(GIT_CONFIG_NOSYSTEM=1 GIT_CONFIG_GLOBAL=/dev/null git config --file "$cfg" --get-all remote.origin.url 2>/dev/null)" || rc=$?
  if [ "$rc" -eq 1 ]; then
    echo "missing origin remote"
    return 1
  elif [ "$rc" -ne 0 ]; then
    echo "unreadable git config"
    return 1
  fi
  if [ "$remote_urls" != "$url" ]; then
    echo "origin URL is $(printf '%s' "$remote_urls" | tr '\n' ','), manifest pins $url"
    return 1
  fi

  rc=0
  promisor="$(GIT_CONFIG_NOSYSTEM=1 GIT_CONFIG_GLOBAL=/dev/null git config --file "$cfg" --get-all remote.origin.promisor 2>/dev/null)" || rc=$?
  if [ "$rc" -eq 0 ] && [ -n "$promisor" ]; then
    echo "partial-clone promisor remote is not allowed ($promisor)"
    return 1
  elif [ "$rc" -gt 1 ]; then
    echo "unreadable git config"
    return 1
  fi

  rc=0
  partial="$(GIT_CONFIG_NOSYSTEM=1 GIT_CONFIG_GLOBAL=/dev/null git config --file "$cfg" --get-all extensions.partialclone 2>/dev/null)" || rc=$?
  if [ "$rc" -eq 0 ] && [ -n "$partial" ]; then
    echo "partial-clone promisor remote is not allowed ($partial)"
    return 1
  elif [ "$rc" -gt 1 ]; then
    echo "unreadable git config"
    return 1
  fi
  return 0
}

# Resolve DIR/.git/HEAD directly from disk without executing DIR/.git/config or hooks.
resolve_stored_head() {
  local dir="$1"
  python3 -I - "$dir" <<'PY'
import os
import re
import stat
import sys

git_dir = os.path.join(sys.argv[1], ".git")
head_path = os.path.join(git_dir, "HEAD")

def read_regular_text(path):
    st = os.lstat(path)
    if stat.S_ISLNK(st.st_mode) or not stat.S_ISREG(st.st_mode):
        raise ValueError("not a regular file")
    with open(path, "r", encoding="utf-8") as fh:
        return fh.read().strip()

def check_no_symlink_components(base, rel):
    cur = base
    for part in rel.split("/"):
        cur = os.path.join(cur, part)
        st = os.lstat(cur)
        if stat.S_ISLNK(st.st_mode):
            raise ValueError("symlink in ref path")
    return cur

HEX_RE = re.compile(r"^[0-9a-f]{40}$")

try:
    head = read_regular_text(head_path)
except Exception:
    sys.exit(1)

if HEX_RE.fullmatch(head):
    print(head)
    sys.exit(0)

if head.startswith("ref: "):
    ref = head[5:].strip()
    if (
        not ref.startswith("refs/")
        or ".." in ref.split("/")
        or "//" in ref
        or ref.endswith("/")
        or any(ch in ref for ch in "\\\0\r\n")
    ):
        sys.exit(1)
    try:
        ref_path = check_no_symlink_components(git_dir, ref)
        val = read_regular_text(ref_path)
        if HEX_RE.fullmatch(val):
            print(val)
            sys.exit(0)
    except FileNotFoundError:
        pass
    except Exception:
        sys.exit(1)

    packed = os.path.join(git_dir, "packed-refs")
    try:
        content = read_regular_text(packed)
    except Exception:
        sys.exit(1)
    for line in content.splitlines():
        if not line or line.startswith("#") or line.startswith("^"):
            continue
        parts = line.split(" ", 1)
        if len(parts) == 2 and parts[1] == ref and HEX_RE.fullmatch(parts[0]):
            print(parts[0])
            sys.exit(0)

sys.exit(1)
PY
}

# Materialize a completely fresh, self-contained git repository at FRESH_DIR from verified
# commit REV objects located in SRC_OBJECTS_DIR, configure literal manifest origin URL,
# check out REV into a fresh index, and verify that the materialized working tree hashes
# to REV^{tree} in an independent index and contains no .lake directory.
materialize_verified_checkout() {
  local src_objects_dir="$1"
  local fresh_dir="$2"
  local url="$3"
  local rev="$4"
  local src_objects_abs=""
  local obj_list="$TMP_DIR/objects.list"
  local pack_file="$TMP_DIR/objects.pack"
  local verify_idx="$TMP_DIR/verify-tree.idx"
  local obj_type=""
  local parents=""
  local expected_tree=""
  local actual_tree=""
  local scan_err=""
  local cfg_err=""

  if [ ! -d "$src_objects_dir" ] || [ -L "$src_objects_dir" ]; then
    echo "missing or symlink .git/objects"
    return 1
  fi
  if [ -e "$src_objects_dir/info/alternates" ] || [ -L "$src_objects_dir/info/alternates" ]; then
    echo "unexpected git alternates file in package object store"
    return 1
  fi
  if ! src_objects_abs="$(cd "$src_objects_dir" && pwd -P)"; then
    echo "unreadable .git/objects"
    return 1
  fi

  if ! GIT_CONFIG_NOSYSTEM=1 GIT_CONFIG_GLOBAL=/dev/null \
      git init -q --template="" "$fresh_dir" >/dev/null 2>&1; then
    echo "could not initialize fresh staging repository"
    return 1
  fi

  obj_type="$(GIT_CONFIG_NOSYSTEM=1 GIT_CONFIG_GLOBAL=/dev/null \
      GIT_ALTERNATE_OBJECT_DIRECTORIES="$src_objects_abs" \
      git --no-replace-objects -C "$fresh_dir" cat-file -t "$rev" 2>/dev/null || true)"
  if [ "$obj_type" != "commit" ]; then
    echo "pinned commit $rev missing or invalid in object store"
    return 1
  fi

  # We transfer only depth-1 objects (commit $rev plus its tree/blob closure), so if
  # $rev has parent commits, mark $rev shallow BEFORE strict unpack and fsck.
  parents="$(GIT_CONFIG_NOSYSTEM=1 GIT_CONFIG_GLOBAL=/dev/null \
      GIT_ALTERNATE_OBJECT_DIRECTORIES="$src_objects_abs" \
      git --no-replace-objects -C "$fresh_dir" cat-file -p "$rev" \
      | awk '/^parent / {print $2} /^$/ {exit}')"
  if [ -n "$parents" ]; then
    printf '%s\n' "$rev" >"$fresh_dir/.git/shallow"
  fi

  rm -f "$obj_list" "$pack_file" "$verify_idx"
  if ! {
    printf '%s\n' "$rev"
    GIT_CONFIG_NOSYSTEM=1 GIT_CONFIG_GLOBAL=/dev/null \
      GIT_ALTERNATE_OBJECT_DIRECTORIES="$src_objects_abs" \
      git --no-replace-objects -C "$fresh_dir" rev-list --objects --no-object-names "${rev}^{tree}"
  } >"$obj_list" 2>/dev/null; then
    echo "incomplete or corrupt tree/blob objects for pinned commit $rev"
    return 1
  fi

  if ! GIT_CONFIG_NOSYSTEM=1 GIT_CONFIG_GLOBAL=/dev/null \
      GIT_ALTERNATE_OBJECT_DIRECTORIES="$src_objects_abs" \
      git --no-replace-objects -C "$fresh_dir" pack-objects --stdout -q <"$obj_list" >"$pack_file" 2>/dev/null; then
    echo "failed to read objects for pinned commit $rev"
    return 1
  fi

  if ! GIT_CONFIG_NOSYSTEM=1 GIT_CONFIG_GLOBAL=/dev/null \
      git --no-replace-objects -C "$fresh_dir" unpack-objects --strict -q <"$pack_file" >/dev/null 2>&1; then
    echo "corrupt or unverifiable objects for pinned commit $rev"
    return 1
  fi
  rm -f "$obj_list" "$pack_file"

  if ! GIT_CONFIG_NOSYSTEM=1 GIT_CONFIG_GLOBAL=/dev/null \
      git --no-replace-objects -C "$fresh_dir" fsck --full --no-dangling >/dev/null 2>&1; then
    echo "object store failed git fsck for pinned commit $rev"
    return 1
  fi

  if ! GIT_CONFIG_NOSYSTEM=1 GIT_CONFIG_GLOBAL=/dev/null \
      git -C "$fresh_dir" remote add origin "$url" >/dev/null 2>&1; then
    echo "could not record origin URL in fresh checkout"
    return 1
  fi

  if ! GIT_CONFIG_NOSYSTEM=1 GIT_CONFIG_GLOBAL=/dev/null \
      git --no-replace-objects -C "$fresh_dir" \
        -c core.hooksPath=/dev/null -c core.autocrlf=false -c core.eol=lf \
        -c core.symlinks=true -c advice.detachedHead=false \
        checkout -q --force --detach "$rev" >/dev/null 2>&1; then
    echo "failed to materialize checkout of pinned commit $rev"
    return 1
  fi

  if ! GIT_CONFIG_NOSYSTEM=1 GIT_CONFIG_GLOBAL=/dev/null \
      git --no-replace-objects -C "$fresh_dir" \
        -c core.hooksPath=/dev/null clean -ffdxq >/dev/null 2>&1; then
    echo "failed to clean fresh checkout of pinned commit $rev"
    return 1
  fi

  if ! expected_tree="$(GIT_CONFIG_NOSYSTEM=1 GIT_CONFIG_GLOBAL=/dev/null \
      git --no-replace-objects -C "$fresh_dir" rev-parse --verify --quiet "${rev}^{tree}" 2>/dev/null)"; then
    echo "unreadable pinned tree for $rev"
    return 1
  fi

  rm -f "$verify_idx"
  if ! GIT_CONFIG_NOSYSTEM=1 GIT_CONFIG_GLOBAL=/dev/null GIT_INDEX_FILE="$verify_idx" \
      git --no-replace-objects --git-dir="$fresh_dir/.git" --work-tree="$fresh_dir" \
        -c core.autocrlf=false -c core.safecrlf=false -c core.symlinks=true \
        -c core.filemode=true -c core.excludesFile=/dev/null -c core.attributesFile=/dev/null \
        add -A --force >/dev/null 2>&1; then
    rm -f "$verify_idx"
    echo "failed to index fresh checkout for tree verification"
    return 1
  fi

  if ! actual_tree="$(GIT_CONFIG_NOSYSTEM=1 GIT_CONFIG_GLOBAL=/dev/null GIT_INDEX_FILE="$verify_idx" \
      git --no-replace-objects --git-dir="$fresh_dir/.git" --work-tree="$fresh_dir" \
        write-tree 2>/dev/null)"; then
    rm -f "$verify_idx"
    echo "failed to compute tree hash of fresh checkout"
    return 1
  fi
  rm -f "$verify_idx"

  if [ "$actual_tree" != "$expected_tree" ]; then
    echo "materialized tree $actual_tree does not match pinned tree $expected_tree"
    return 1
  fi

  if ! scan_err="$(scan_no_lake_dirs "$fresh_dir")"; then
    echo "$scan_err"
    return 1
  fi
  if ! cfg_err="$(verify_stored_config "$fresh_dir" "$url")"; then
    echo "$cfg_err"
    return 1
  fi
  return 0
}

# Compare RESTORED_DIR's working tree (excluding top-level .git) against freshly
# materialized FRESH_DIR byte-for-byte, ignoring restored .git/index completely.
compare_worktrees() {
  local restored_dir="$1"
  local fresh_dir="$2"
  python3 -I - "$restored_dir" "$fresh_dir" <<'PY'
import hashlib
import os
import stat
import sys

restored_root = sys.argv[1]
fresh_root = sys.argv[2]

def snapshot_tree(root):
    entries = {}
    def walk(current, rel_prefix, is_root):
        try:
            items = list(os.scandir(current))
        except OSError as exc:
            print(f"filesystem traversal error at {current}: {exc}")
            sys.exit(1)
        for item in items:
            if is_root and item.name == ".git":
                continue
            rel = item.name if not rel_prefix else f"{rel_prefix}/{item.name}"
            try:
                st = item.stat(follow_symlinks=False)
            except OSError as exc:
                print(f"filesystem traversal error at {item.path}: {exc}")
                sys.exit(1)
            mode = st.st_mode
            if stat.S_ISLNK(mode):
                try:
                    target = os.readlink(item.path)
                except OSError as exc:
                    print(f"filesystem traversal error at {item.path}: {exc}")
                    sys.exit(1)
                entries[rel] = ("symlink", target)
            elif stat.S_ISDIR(mode):
                entries[rel] = ("dir",)
                walk(item.path, rel, False)
            elif stat.S_ISREG(mode):
                h = hashlib.sha256()
                try:
                    with open(item.path, "rb") as fh:
                        while True:
                            chunk = fh.read(65536)
                            if not chunk:
                                break
                            h.update(chunk)
                except OSError as exc:
                    print(f"filesystem traversal error at {item.path}: {exc}")
                    sys.exit(1)
                is_exec = bool(mode & 0o100)
                entries[rel] = ("file", is_exec, h.hexdigest())
            else:
                print(f"unsupported special file in working tree: {rel}")
                sys.exit(1)
    walk(root, "", True)
    return entries

restored = snapshot_tree(restored_root)
fresh = snapshot_tree(fresh_root)

extra = sorted(set(restored) - set(fresh))
if extra:
    print(f"dirty, untracked or ignored files present ({extra[0]})")
    sys.exit(1)

missing = sorted(set(fresh) - set(restored))
if missing:
    print(f"working tree missing pinned file ({missing[0]})")
    sys.exit(1)

for rel in sorted(fresh):
    if restored[rel] != fresh[rel]:
        print(f"working tree differs from pinned commit ({rel})")
        sys.exit(1)
PY
}

# Verify restored DIR without trusting its .git/index, hooks, or rewritten URLs, and
# materialize a clean replacement repository at FRESH_OUT on success.
verify_restored_package() {
  local dir="$1"
  local url="$2"
  local rev="$3"
  local fresh_dir="$4"
  local reason=""
  local head_rev=""

  if ! reason="$(scan_no_lake_dirs "$dir")"; then
    echo "$reason"
    return 1
  fi
  if ! reason="$(verify_stored_config "$dir" "$url")"; then
    echo "$reason"
    return 1
  fi
  if ! head_rev="$(resolve_stored_head "$dir")"; then
    echo "unreadable HEAD commit"
    return 1
  fi
  if [ "$head_rev" != "$rev" ]; then
    echo "HEAD is $head_rev, manifest pins $rev"
    return 1
  fi
  if ! reason="$(materialize_verified_checkout "$dir/.git/objects" "$fresh_dir" "$url" "$rev")"; then
    echo "$reason"
    return 1
  fi
  if ! reason="$(compare_worktrees "$dir" "$fresh_dir")"; then
    echo "$reason"
    return 1
  fi
  return 0
}

run_bounded_attempt() {
  local err_file="$1"
  local status_file="$2"
  shift 2
  rm -f "$status_file"
  if ! GIT_TERMINAL_PROMPT=0 python3 -I "$BOUNDED_EXEC" \
      --timeout "$FETCH_TIMEOUT" --kill-after 10 --status "$status_file" \
      -- "$@" >"$err_file" 2>&1; then
    echo "runner-failed" >"$status_file"
  fi
}

# Run one complete fetch attempt (init, remote add, primary fetch, optional raw-SHA
# fallback fetch, and checkout) under ONE single deadline (FETCH_TIMEOUT).
fetch_once() {
  local stage="$1"
  local url="$2"
  local rev="$3"
  local err_file="$4"
  local status_file="$5"
  local primary_err="$TMP_DIR/primary-fetch.err"
  local outcome=""

  rm -f "$primary_err"
  run_bounded_attempt "$err_file" "$status_file" \
    bash --noprofile --norc -euo pipefail -c '
      stage="$1"
      url="$2"
      rev="$3"
      primary_err="$4"
      git init -q --template="" "$stage"
      git -C "$stage" remote add origin "$url"
      if ! git -C "$stage" fetch -q --depth 1 --no-tags origin "$rev" 2>"$primary_err"; then
        cat "$primary_err" >&2
        if grep -qiE "not our ref|unadvertised object|couldn'\''t find remote ref|no such remote ref" "$primary_err"; then
          git -C "$stage" fetch -q --no-tags origin
        else
          exit 1
        fi
      fi
      git -C "$stage" -c core.hooksPath=/dev/null -c advice.detachedHead=false checkout -q --force --detach "$rev"
    ' _ "$stage" "$url" "$rev" "$primary_err"

  outcome="$(cat "$status_file" 2>/dev/null || true)"
  if [ "$outcome" != "exit 0" ]; then
    if [ "$outcome" = "timeout" ]; then
      echo "fetch timed out after ${FETCH_TIMEOUT}s" >>"$err_file"
    fi
    return 1
  fi
  return 0
}

fetch_package_with_retry() {
  local name="$1"
  local url="$2"
  local rev="$3"
  local dest="$PACKAGES_DIR/$name"
  local err_file="$TMP_DIR/fetch-$name.err"
  local status_file="$TMP_DIR/fetch-$name.status"
  local attempt=1
  local delay=0
  local shift_exp=0
  local raw_stage=""
  local fresh_stage=""
  local verify_err=""

  while [ "$attempt" -le "$ATTEMPTS" ]; do
    if [ "$attempt" -gt 1 ]; then
      shift_exp=$((attempt - 2))
      if [ "$shift_exp" -gt 10 ]; then
        shift_exp=10
      fi
      delay=$((BACKOFF_BASE * (1 << shift_exp)))
      echo "lake-packages: $name: retrying fetch (attempt $attempt/$ATTEMPTS) after ${delay}s" >&2
      if [ "$delay" -gt 0 ]; then
        sleep "$delay"
      fi
    fi

    raw_stage="$(mktemp -d "$PACKAGES_DIR/.fetch-$name.XXXXXX")"
    CURRENT_STAGE="$raw_stage"
    fresh_stage="$(mktemp -d "$PACKAGES_DIR/.clean-$name.XXXXXX")"
    CURRENT_FRESH="$fresh_stage"

    if fetch_once "$raw_stage" "$url" "$rev" "$err_file" "$status_file"; then
      if verify_err="$(verify_stored_config "$raw_stage" "$url")" \
         && verify_err="$(materialize_verified_checkout "$raw_stage/.git/objects" "$fresh_stage" "$url" "$rev")"; then
        safe_rm_rf "$dest"
        mv "$fresh_stage" "$dest"
        CURRENT_FRESH=""
        safe_rm_rf "$raw_stage"
        CURRENT_STAGE=""
        echo "ok   lake-packages: $name fetched and verified at $rev (attempt $attempt/$ATTEMPTS)"
        return 0
      else
        echo "warn lake-packages: $name: fetched checkout failed verification ($verify_err)" >&2
      fi
    else
      echo "warn lake-packages: $name: fetch attempt $attempt/$ATTEMPTS failed: $(tr '\n' ' ' <"$err_file" | head -c 400)" >&2
    fi

    safe_rm_rf "$fresh_stage"
    CURRENT_FRESH=""
    safe_rm_rf "$raw_stage"
    CURRENT_STAGE=""
    attempt=$((attempt + 1))
  done

  echo "error: lake-packages: $name: failed to fetch $rev from $url after $ATTEMPTS attempt(s)" >&2
  return 1
}

try_seed_package() {
  local name="$1"
  local url="$2"
  local rev="$3"
  local dest="$PACKAGES_DIR/$name"
  local seed_pkg=""
  local seed_real=""
  local dest_real=""
  local fresh_stage=""
  local reason=""

  [ -n "$SEED_PACKAGES_DIR" ] || return 1
  seed_pkg="$SEED_PACKAGES_DIR/$name"
  if [ ! -d "$seed_pkg" ] || [ -L "$seed_pkg" ] || [ ! -d "$seed_pkg/.git" ] || [ -L "$seed_pkg/.git" ] || [ ! -d "$seed_pkg/.git/objects" ] || [ -L "$seed_pkg/.git/objects" ]; then
    return 1
  fi
  seed_real="$(realpath -m "$seed_pkg")"
  dest_real="$(realpath -m "$dest")"
  if [ "$seed_real" = "$dest_real" ]; then
    return 1
  fi

  fresh_stage="$(mktemp -d "$PACKAGES_DIR/.seed-$name.XXXXXX")"
  CURRENT_FRESH="$fresh_stage"
  if reason="$(materialize_verified_checkout "$seed_pkg/.git/objects" "$fresh_stage" "$url" "$rev")"; then
    safe_rm_rf "$dest"
    mv "$fresh_stage" "$dest"
    CURRENT_FRESH=""
    echo "ok   lake-packages: $name materialized from verified seed objects at $rev"
    return 0
  fi
  echo "warn lake-packages: $name: seed object verification failed ($reason); falling back to remote fetch" >&2
  safe_rm_rf "$fresh_stage"
  CURRENT_FRESH=""
  return 1
}

while IFS=$'\t' read -r name url rev; do
  [ -n "$name" ] || continue
  pkg_dir="$PACKAGES_DIR/$name"
  if [ -e "$pkg_dir" ] || [ -L "$pkg_dir" ]; then
    fresh_stage="$(mktemp -d "$PACKAGES_DIR/.verify-$name.XXXXXX")"
    CURRENT_FRESH="$fresh_stage"
    if reason="$(verify_restored_package "$pkg_dir" "$url" "$rev" "$fresh_stage")"; then
      safe_rm_rf "$pkg_dir"
      mv "$fresh_stage" "$pkg_dir"
      CURRENT_FRESH=""
      echo "ok   lake-packages: $name cache verified at $rev"
      continue
    fi
    safe_rm_rf "$fresh_stage"
    CURRENT_FRESH=""
    echo "warn lake-packages: $name: restored package failed verification ($reason); deleting and refetching" >&2
    safe_rm_rf "$pkg_dir"
  else
    if try_seed_package "$name" "$url" "$rev"; then
      continue
    fi
    echo "info lake-packages: $name: not in cache; fetching $rev"
  fi
  fetch_package_with_retry "$name" "$url" "$rev"
done <"$MANIFEST_ROWS"
