#!/usr/bin/env bash
# Restore verification and bounded-retry fetch for pinned Lake dependency sources.
#
# Reads lake-manifest.json and lean-toolchain from the current working directory.
# Never trusts restored .git metadata, .git/index flags (such as assume-unchanged
# or skip-worktree), git status, or rewritten remote URLs:
#   - rejects active Lake 4.35 package overrides (.lake/package-overrides.json and
#     non-empty/invalid LAKE_PKG_URL_MAP) and verifies manifest lakeDir/packagesDir;
#   - carries each manifest entry's subDir and configFile and requires resolved
#     package and configuration paths to stay inside the verified checkout;
#   - prunes unexpected .lake/packages entries and verifies final membership by
#     literal directory names (never stripping trailing newlines);
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
#                            fallback fetch, object verification and checkout
#                            materialization (default: 180)
#   LAKE_SEED_PACKAGES_DIR   optional directory of existing package repos used ONLY as
#                            an untrusted object-store seed when a package is absent
#                            (only verified commit <rev> objects are extracted into a
#                            fresh checkout; working-tree edits and build outputs are
#                            never inherited)
set -euo pipefail

HELPER_SCRIPT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/$(basename "${BASH_SOURCE[0]}")"
HELPER_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
BOUNDED_EXEC="$HELPER_ROOT/scripts/checks/lib/bounded_exec.py"
source "$HELPER_ROOT/scripts/checks/lib/git_sandbox.sh" || exit 2
git_sandbox || exit 2

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

def unlock_dirs(root):
    # Restrict permission repair strictly to directories needed for traversal and deletion.
    # Never chmod regular files: local git clones can share hard-linked object inodes with
    # other repositories/mirrors, and POSIX unlink only requires containing-dir permissions.
    try:
        rst = os.lstat(root)
    except OSError:
        return
    if not stat.S_ISDIR(rst.st_mode) or stat.S_ISLNK(rst.st_mode):
        return
    mode = stat.S_IMODE(rst.st_mode)
    if (mode & 0o700) != 0o700:
        try:
            os.chmod(root, mode | 0o700)
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
            unlock_dirs(entry.path)

unlock_dirs(path)
shutil.rmtree(path)
PY
}

# Helper functions live above the --internal-fetch-attempt dispatch so bounded fetch
# attempts share the exact verification and materialization logic under one deadline.

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

# Reject any active Lake 4.35 package override mechanism in the workspace or environment:
#   - Lake/Config/Workspace.lean: packageOverridesFile := self.lakeDir / "package-overrides.json"
#     (loaded automatically by Lake/Load/Resolve.lean Workspace.materializeDeps);
#   - Lake/Config/Env.lean: LAKE_PKG_URL_MAP overrides Git remote URLs during materialization
#     (note: `lake env` exports LAKE_PKG_URL_MAP={} when empty, so only unset or `{}` is allowed).
check_no_lake_overrides() {
  python3 -I - <<'PY'
import json
import os
import stat
import sys

try:
    st = os.lstat(".lake")
except FileNotFoundError:
    st = None
except OSError as exc:
    sys.exit(f"error: lake-packages: cannot inspect .lake: {exc}")

if st is not None:
    if stat.S_ISLNK(st.st_mode) or not stat.S_ISDIR(st.st_mode):
        sys.exit("error: lake-packages: refusing symlink or non-directory at .lake")

override_path = os.path.join(".lake", "package-overrides.json")
try:
    os.lstat(override_path)
    sys.exit(
        "error: lake-packages: refusing active Lake package override file .lake/package-overrides.json"
    )
except FileNotFoundError:
    pass
except OSError as exc:
    sys.exit(f"error: lake-packages: cannot inspect {override_path}: {exc}")

url_map_raw = os.environ.get("LAKE_PKG_URL_MAP")
if url_map_raw is not None:
    try:
        parsed = json.loads(url_map_raw)
    except json.JSONDecodeError as exc:
        sys.exit(f"error: lake-packages: refusing invalid LAKE_PKG_URL_MAP override: {exc}")
    if not isinstance(parsed, dict) or len(parsed) > 0:
        sys.exit(
            f"error: lake-packages: refusing active Lake package URL override via LAKE_PKG_URL_MAP ({url_map_raw!r})"
        )
PY
}

# Verify that a materialized checkout's manifest subDir, configFile (and optional
# manifestFile when present) resolve strictly inside the verified checkout directory
# and never traverse outside it (including via symlinks) or enter .git.
verify_package_layout() {
  local checkout_dir="$1"
  local sub_dir="$2"
  local config_file="$3"
  local manifest_file="${4:-}"
  python3 -I - "$checkout_dir" "$sub_dir" "$config_file" "$manifest_file" <<'PY'
import os
import stat
import sys

checkout_dir = sys.argv[1]
sub_dir = sys.argv[2]
config_file = sys.argv[3]
manifest_file = sys.argv[4]

try:
    st = os.lstat(checkout_dir)
except OSError as exc:
    print(f"cannot inspect package checkout: {exc}")
    sys.exit(1)
if stat.S_ISLNK(st.st_mode) or not stat.S_ISDIR(st.st_mode):
    print("package checkout is not a real directory")
    sys.exit(1)

root_real = os.path.realpath(checkout_dir)
git_real = os.path.join(root_real, ".git")

def is_within(child_real, parent_real):
    try:
        return os.path.commonpath([parent_real, child_real]) == parent_real
    except ValueError:
        return False

def validate_rel(raw, label, allow_dot=False):
    if not raw or any(ord(ch) < 32 or ord(ch) == 127 for ch in raw):
        print(f"unsafe {label}: {raw!r}")
        sys.exit(1)
    if raw.startswith("/") or "\\" in raw or (len(raw) >= 2 and raw[1] == ":"):
        print(f"unsafe {label}: must be a relative POSIX path ({raw!r})")
        sys.exit(1)
    parts = [p for p in raw.split("/") if p]
    if ".." in parts or ".git" in parts or ".lake" in parts:
        print(f"unsafe {label}: disallowed path component in {raw!r}")
        sys.exit(1)
    if not allow_dot and all(p == "." for p in parts):
        print(f"unsafe {label}: degenerate path {raw!r}")
        sys.exit(1)
    return parts

def resolve_within(base_real, rel_path, label, boundary_real):
    cur = base_real
    for part in [p for p in rel_path.split("/") if p and p != "."]:
        cur = os.path.join(cur, part)
        if not os.path.lexists(cur):
            return None, "missing"
        if not os.path.exists(cur):
            return None, "broken"
        cur_real = os.path.realpath(cur)
        if not is_within(cur_real, root_real) or not is_within(cur_real, boundary_real) or is_within(cur_real, git_real):
            return None, "escape"
        cur = cur_real
    return cur, "ok"

if sub_dir:
    validate_rel(sub_dir, "subDir", allow_dot=True)
    pkg_real, status = resolve_within(root_real, sub_dir, "subDir", root_real)
    if status == "missing":
        print(f"package subDir {sub_dir!r} not found in verified checkout")
        sys.exit(1)
    if status == "broken":
        print(f"package subDir {sub_dir!r} is a broken symlink")
        sys.exit(1)
    if status == "escape":
        print(f"resolved package path for subDir {sub_dir!r} escapes verified checkout")
        sys.exit(1)
    if not os.path.isdir(pkg_real):
        print(f"resolved package path for subDir {sub_dir!r} is not a directory")
        sys.exit(1)
else:
    pkg_real = root_real

cfg_spec = config_file or "lakefile"
validate_rel(cfg_spec, "configFile", allow_dot=False)
if cfg_spec.endswith("/"):
    print(f"unsafe configFile: directory path {cfg_spec!r}")
    sys.exit(1)
base = cfg_spec.rsplit("/", 1)[-1]
if base in {"", ".", ".."}:
    print(f"unsafe configFile: invalid basename in {cfg_spec!r}")
    sys.exit(1)

# Match Lake/Load/Package.lean resolveConfigFile extension handling.
dot_idx = base.rfind(".")
if dot_idx > 0:
    ext = base[dot_idx + 1:]
    if ext not in {"lean", "toml"}:
        print(f"unsupported configFile extension in {cfg_spec!r}")
        sys.exit(1)
    candidates = [cfg_spec]
else:
    candidates = [f"{cfg_spec}.lean", f"{cfg_spec}.toml"]

resolved_cfg = None
for rel_cand in candidates:
    cand_real, status = resolve_within(pkg_real, rel_cand, "configFile", pkg_real)
    if status == "missing":
        continue
    if status == "broken":
        print(f"configFile {rel_cand!r} is a broken symlink")
        sys.exit(1)
    if status == "escape":
        print(f"resolved config path for {rel_cand!r} escapes verified checkout")
        sys.exit(1)
    try:
        cst = os.stat(cand_real)
    except OSError as exc:
        print(f"cannot inspect resolved configFile {rel_cand!r}: {exc}")
        sys.exit(1)
    if not stat.S_ISREG(cst.st_mode):
        print(f"resolved config path for {rel_cand!r} is not a regular file")
        sys.exit(1)
    resolved_cfg = cand_real
    break

if resolved_cfg is None:
    print(f"configFile {cfg_spec!r} not found in verified package directory")
    sys.exit(1)

if manifest_file:
    validate_rel(manifest_file, "manifestFile", allow_dot=False)
    if manifest_file.endswith("/"):
        print(f"unsafe manifestFile: directory path {manifest_file!r}")
        sys.exit(1)
    man_real, status = resolve_within(pkg_real, manifest_file, "manifestFile", pkg_real)
    if status == "broken":
        print(f"manifestFile {manifest_file!r} is a broken symlink")
        sys.exit(1)
    if status == "escape":
        print(f"resolved manifestFile path for {manifest_file!r} escapes verified checkout")
        sys.exit(1)
    if status == "ok":
        try:
            mst = os.stat(man_real)
        except OSError as exc:
            print(f"cannot inspect resolved manifestFile {manifest_file!r}: {exc}")
            sys.exit(1)
        if not stat.S_ISREG(mst.st_mode):
            print(f"resolved manifestFile path for {manifest_file!r} is not a regular file")
            sys.exit(1)
PY
}

# Materialize a completely fresh, self-contained git repository at FRESH_DIR from verified
# commit REV objects located in SRC_OBJECTS_DIR, configure literal manifest origin URL,
# check out REV into a fresh index, and verify that the materialized working tree hashes
# to REV^{tree} in an independent index, contains no .lake directory, and satisfies
# the manifest entry's subDir/configFile containment checks.
materialize_verified_checkout() {
  local src_objects_dir="$1"
  local fresh_dir="$2"
  local url="$3"
  local rev="$4"
  local sub_dir="$5"
  local config_file="$6"
  local manifest_file="${7:-}"
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
  local layout_err=""

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
  if ! layout_err="$(verify_package_layout "$fresh_dir" "$sub_dir" "$config_file" "$manifest_file")"; then
    echo "$layout_err"
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
  local sub_dir="$4"
  local config_file="$5"
  local manifest_file="$6"
  local fresh_dir="$7"
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
  if ! reason="$(materialize_verified_checkout "$dir/.git/objects" "$fresh_dir" "$url" "$rev" "$sub_dir" "$config_file" "$manifest_file")"; then
    echo "$reason"
    return 1
  fi
  if ! reason="$(compare_worktrees "$dir" "$fresh_dir")"; then
    echo "$reason"
    return 1
  fi
  return 0
}

# Internal worker entry point invoked under bounded_exec.py so one single per-attempt
# deadline governs init, remote add, primary fetch, optional raw-SHA fallback fetch,
# stored-config verification, strict object unpack/fsck, and final checkout materialization.
if [ "${1:-}" = "--internal-fetch-attempt" ]; then
  if [ "$#" -ne 9 ]; then
    echo "error: lake-packages: invalid internal attempt arguments" >&2
    exit 2
  fi
  raw_stage="$2"
  fresh_stage="$3"
  url="$4"
  rev="$5"
  sub_dir="$6"
  config_file="$7"
  manifest_file="$8"
  TMP_DIR="$9"
  primary_err="$TMP_DIR/primary-fetch.err"
  rm -f "$primary_err"
  git init -q --template="" "$raw_stage"
  git -C "$raw_stage" remote add origin "$url"
  if ! git -C "$raw_stage" fetch -q --depth 1 --no-tags origin "$rev" 2>"$primary_err"; then
    cat "$primary_err" >&2
    if grep -qiE "not our ref|unadvertised object|couldn't find remote ref|no such remote ref" "$primary_err"; then
      git -C "$raw_stage" fetch -q --no-tags origin
    else
      exit 1
    fi
  fi
  if ! cfg_err="$(verify_stored_config "$raw_stage" "$url")"; then
    echo "fetched checkout failed verification ($cfg_err)" >&2
    exit 1
  fi
  if ! mat_err="$(materialize_verified_checkout "$raw_stage/.git/objects" "$fresh_stage" "$url" "$rev" "$sub_dir" "$config_file" "$manifest_file")"; then
    echo "fetched checkout failed verification ($mat_err)" >&2
    exit 1
  fi
  exit 0
fi

if [ "$#" -ne 0 ]; then
  echo "error: lake-packages: unexpected arguments: $*" >&2
  exit 2
fi

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
check_no_lake_overrides || exit 1

TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/lake-packages.XXXXXX")"
CURRENT_STAGE=""
CURRENT_FRESH=""
cleanup() {
  [ -z "${CURRENT_FRESH:-}" ] || safe_rm_rf "$CURRENT_FRESH" || true
  [ -z "${CURRENT_STAGE:-}" ] || safe_rm_rf "$CURRENT_STAGE" || true
  [ -z "${TMP_DIR:-}" ] || safe_rm_rf "$TMP_DIR" || true
}
trap cleanup EXIT

MANIFEST_JSON="$TMP_DIR/manifest.json"
MANIFEST_RECORDS="$TMP_DIR/manifest.nul"
python3 -I - "$MANIFEST" "$MANIFEST_JSON" "$MANIFEST_RECORDS" <<'PY'
import json
import re
import sys
from pathlib import Path

manifest_path = Path(sys.argv[1])
json_out = Path(sys.argv[2])
nul_out = Path(sys.argv[3])

try:
    data = json.loads(manifest_path.read_text(encoding="utf-8"))
except (OSError, UnicodeDecodeError, json.JSONDecodeError) as exc:
    sys.exit(f"error: lake-packages: cannot parse {manifest_path}: {exc}")

if not isinstance(data, dict):
    sys.exit(f"error: lake-packages: {manifest_path} must be a JSON object")

if "lakeDir" in data and data["lakeDir"] is not None:
    lake_dir = data["lakeDir"]
    if lake_dir != ".lake":
        sys.exit(f"error: lake-packages: expected lakeDir '.lake', got {lake_dir!r}")

packages_dir = data.get("packagesDir")
if packages_dir != ".lake/packages":
    sys.exit(
        f"error: lake-packages: expected packagesDir '.lake/packages', got {packages_dir!r}"
    )

packages = data.get("packages")
if not isinstance(packages, list):
    sys.exit(f"error: lake-packages: {manifest_path} packages field must be a list")

def validate_rel_subpath(raw, field_name, pkg_name, *, allow_dot=False):
    if not isinstance(raw, str) or not raw:
        sys.exit(f"error: lake-packages: package {pkg_name} has unsafe {field_name} {raw!r}")
    if any(ord(ch) < 32 or ord(ch) == 127 for ch in raw):
        sys.exit(f"error: lake-packages: package {pkg_name} has unsafe {field_name} {raw!r}")
    if raw.startswith("/") or "\\" in raw or (len(raw) >= 2 and raw[1] == ":"):
        sys.exit(
            f"error: lake-packages: package {pkg_name} has unsafe {field_name} {raw!r} (must be relative POSIX path)"
        )
    parts = [p for p in raw.split("/") if p]
    if ".." in parts or ".git" in parts or ".lake" in parts:
        sys.exit(f"error: lake-packages: package {pkg_name} has unsafe {field_name} {raw!r}")
    if not allow_dot and all(p == "." for p in parts):
        sys.exit(f"error: lake-packages: package {pkg_name} has unsafe {field_name} {raw!r}")
    return raw

seen = set()
parsed_entries = []
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

    raw_sub = entry.get("subDir")
    if raw_sub is None:
        sub_dir = ""
    else:
        sub_dir = validate_rel_subpath(raw_sub, "subDir", name, allow_dot=True)

    raw_cfg = entry.get("configFile")
    if raw_cfg is None:
        config_file = "lakefile"
    else:
        config_file = validate_rel_subpath(raw_cfg, "configFile", name, allow_dot=False)
        if config_file.endswith("/"):
            sys.exit(f"error: lake-packages: package {name} has unsafe configFile {raw_cfg!r}")
        base = config_file.rsplit("/", 1)[-1]
        if base in {"", ".", ".."}:
            sys.exit(f"error: lake-packages: package {name} has unsafe configFile {raw_cfg!r}")
        dot_idx = base.rfind(".")
        if dot_idx > 0:
            ext = base[dot_idx + 1:]
            if ext not in {"lean", "toml"}:
                sys.exit(
                    f"error: lake-packages: package {name} has unsupported configFile extension ({raw_cfg!r})"
                )

    raw_man = entry.get("manifestFile")
    if raw_man is None:
        manifest_file = ""
    else:
        manifest_file = validate_rel_subpath(raw_man, "manifestFile", name, allow_dot=False)
        if manifest_file.endswith("/"):
            sys.exit(f"error: lake-packages: package {name} has unsafe manifestFile {raw_man!r}")

    parsed_entries.append({
        "name": name,
        "url": url,
        "rev": rev,
        "subDir": sub_dir,
        "configFile": config_file,
        "manifestFile": manifest_file,
    })

json_out.write_text(json.dumps(parsed_entries), encoding="utf-8")
with open(nul_out, "wb") as fh:
    for item in parsed_entries:
        for field in ("name", "url", "rev", "subDir", "configFile", "manifestFile"):
            fh.write(item[field].encode("utf-8"))
            fh.write(b"\0")
PY

PACKAGES_DIR=".lake/packages"
if [ -L "$PACKAGES_DIR" ] || { [ -e "$PACKAGES_DIR" ] && [ ! -d "$PACKAGES_DIR" ]; }; then
  echo "warn lake-packages: removing non-directory $PACKAGES_DIR" >&2
  safe_rm_rf "$PACKAGES_DIR"
fi
mkdir -p "$PACKAGES_DIR"

# Prune any unexpected or leftover entries under .lake/packages by literal directory
# entry name (via find -print0 / exact comparison, never stripping trailing newlines).
prune_unexpected_entries() {
  local packages_dir="$1"
  local manifest_json="$2"
  local entries_file="$TMP_DIR/pkg-entries.nul"
  local entry=""
  local base=""
  local display=""
  if ! find "$packages_dir" -mindepth 1 -maxdepth 1 -print0 >"$entries_file"; then
    echo "error: lake-packages: cannot enumerate $packages_dir" >&2
    return 1
  fi
  while IFS= read -r -d '' entry; do
    base="${entry#"$packages_dir"/}"
    if ! python3 -I - "$manifest_json" "$base" <<'PY'; then
import json
import sys
from pathlib import Path

expected = {item["name"] for item in json.loads(Path(sys.argv[1]).read_text(encoding="utf-8"))}
sys.exit(0 if sys.argv[2] in expected else 1)
PY
      display="$(python3 -I - "$packages_dir" "$base" <<'PY'
import sys
pkg_dir, name = sys.argv[1], sys.argv[2]
if any(ord(ch) < 32 or ord(ch) == 127 for ch in name):
    print(f"{pkg_dir}/{name!r}")
else:
    print(f"{pkg_dir}/{name}")
PY
)"
      echo "warn lake-packages: pruning unexpected entry $display" >&2
      safe_rm_rf "$entry"
    fi
  done <"$entries_file"
  rm -f "$entries_file"
}
prune_unexpected_entries "$PACKAGES_DIR" "$MANIFEST_JSON" || exit 1

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

# Run one complete fetch, object-verification and checkout-materialization attempt
# under ONE single deadline (FETCH_TIMEOUT).
fetch_once() {
  local raw_stage="$1"
  local fresh_stage="$2"
  local url="$3"
  local rev="$4"
  local sub_dir="$5"
  local config_file="$6"
  local manifest_file="$7"
  local attempt_tmp="$8"
  local err_file="$9"
  local status_file="${10}"
  local outcome=""

  run_bounded_attempt "$err_file" "$status_file" \
    bash --noprofile --norc "$HELPER_SCRIPT" \
    --internal-fetch-attempt "$raw_stage" "$fresh_stage" "$url" "$rev" "$sub_dir" "$config_file" "$manifest_file" "$attempt_tmp"

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
  local sub_dir="$4"
  local config_file="$5"
  local manifest_file="$6"
  local dest="$PACKAGES_DIR/$name"
  local err_file="$TMP_DIR/fetch-$name.err"
  local status_file="$TMP_DIR/fetch-$name.status"
  local attempt=1
  local delay=0
  local shift_exp=0
  local raw_stage=""
  local fresh_stage=""
  local attempt_tmp=""

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
    attempt_tmp="$(mktemp -d "$TMP_DIR/attempt-$name.XXXXXX")"

    if fetch_once "$raw_stage" "$fresh_stage" "$url" "$rev" "$sub_dir" "$config_file" "$manifest_file" "$attempt_tmp" "$err_file" "$status_file"; then
      safe_rm_rf "$dest"
      mv "$fresh_stage" "$dest"
      CURRENT_FRESH=""
      safe_rm_rf "$raw_stage"
      CURRENT_STAGE=""
      safe_rm_rf "$attempt_tmp"
      echo "ok   lake-packages: $name fetched and verified at $rev (attempt $attempt/$ATTEMPTS)"
      return 0
    else
      echo "warn lake-packages: $name: fetch attempt $attempt/$ATTEMPTS failed: $(tr '\n' ' ' <"$err_file" | head -c 400)" >&2
    fi

    safe_rm_rf "$attempt_tmp"
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
  local sub_dir="$4"
  local config_file="$5"
  local manifest_file="$6"
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
  if reason="$(materialize_verified_checkout "$seed_pkg/.git/objects" "$fresh_stage" "$url" "$rev" "$sub_dir" "$config_file" "$manifest_file")"; then
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

# Final fail-closed post-condition: assert no active Lake overrides, exact literal
# membership in .lake/packages matching lake-manifest.json, and every manifest package
# materialized on disk at its exact pinned revision with no .lake subdirectory and with
# resolved subDir/configFile paths staying inside the verified checkout.
verify_manifest_packages_present() {
  local manifest_json="$1"
  local records_file="$2"
  local pkg_name=""
  local pkg_url=""
  local pkg_rev=""
  local pkg_sub=""
  local pkg_cfg=""
  local pkg_man=""
  local pkg_dir=""
  local head_rev=""
  local err_msg=""

  check_no_lake_overrides || return 1
  if [ ! -d ".lake" ] || [ -L ".lake" ]; then
    echo "error: lake-packages: post-condition failed: missing or symlinked .lake" >&2
    return 1
  fi
  if [ ! -d "$PACKAGES_DIR" ] || [ -L "$PACKAGES_DIR" ]; then
    echo "error: lake-packages: post-condition failed: missing or symlinked $PACKAGES_DIR" >&2
    return 1
  fi

  # Verify exact literal membership of .lake/packages against manifest entries.
  python3 -I - "$PACKAGES_DIR" "$manifest_json" <<'PY' || return 1
import json
import os
import sys
from pathlib import Path

packages_dir = sys.argv[1]
manifest_entries = json.loads(Path(sys.argv[2]).read_text(encoding="utf-8"))
expected = {item["name"] for item in manifest_entries}
try:
    actual = {item.name for item in os.scandir(packages_dir)}
except OSError as exc:
    sys.exit(f"error: lake-packages: post-condition failed: cannot list {packages_dir}: {exc}")

extra = sorted(actual - expected)
if extra:
    sys.exit(
        f"error: lake-packages: post-condition failed: unexpected entries in {packages_dir}: {extra!r}"
    )
missing = sorted(expected - actual)
if missing:
    sys.exit(
        f"error: lake-packages: post-condition failed: missing packages in {packages_dir}: {missing!r}"
    )
PY

  while IFS= read -r -d '' pkg_name \
     && IFS= read -r -d '' pkg_url \
     && IFS= read -r -d '' pkg_rev \
     && IFS= read -r -d '' pkg_sub \
     && IFS= read -r -d '' pkg_cfg \
     && IFS= read -r -d '' pkg_man; do
    [ -n "$pkg_name" ] || continue
    pkg_dir="$PACKAGES_DIR/$pkg_name"
    if [ ! -d "$pkg_dir" ] || [ -L "$pkg_dir" ] || [ ! -d "$pkg_dir/.git" ] || [ -L "$pkg_dir/.git" ]; then
      echo "error: lake-packages: post-condition failed: missing verified checkout at $pkg_dir" >&2
      return 1
    fi
    if ! err_msg="$(scan_no_lake_dirs "$pkg_dir")"; then
      echo "error: lake-packages: post-condition failed: $pkg_dir: $err_msg" >&2
      return 1
    fi
    head_rev="$(resolve_stored_head "$pkg_dir")" || {
      echo "error: lake-packages: post-condition failed: unreadable HEAD in $pkg_dir" >&2
      return 1
    }
    if [ "$head_rev" != "$pkg_rev" ]; then
      echo "error: lake-packages: post-condition failed: $pkg_dir HEAD is $head_rev, expected $pkg_rev" >&2
      return 1
    fi
    if ! err_msg="$(verify_package_layout "$pkg_dir" "$pkg_sub" "$pkg_cfg" "$pkg_man")"; then
      echo "error: lake-packages: post-condition failed: $pkg_dir: $err_msg" >&2
      return 1
    fi
  done <"$records_file"
  return 0
}

while IFS= read -r -d '' name \
   && IFS= read -r -d '' url \
   && IFS= read -r -d '' rev \
   && IFS= read -r -d '' sub_dir \
   && IFS= read -r -d '' config_file \
   && IFS= read -r -d '' manifest_file; do
  [ -n "$name" ] || continue
  pkg_dir="$PACKAGES_DIR/$name"
  if [ -e "$pkg_dir" ] || [ -L "$pkg_dir" ]; then
    fresh_stage="$(mktemp -d "$PACKAGES_DIR/.verify-$name.XXXXXX")"
    CURRENT_FRESH="$fresh_stage"
    if reason="$(verify_restored_package "$pkg_dir" "$url" "$rev" "$sub_dir" "$config_file" "$manifest_file" "$fresh_stage")"; then
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
    if try_seed_package "$name" "$url" "$rev" "$sub_dir" "$config_file" "$manifest_file"; then
      continue
    fi
    echo "info lake-packages: $name: not in cache; fetching $rev"
  fi
  fetch_package_with_retry "$name" "$url" "$rev" "$sub_dir" "$config_file" "$manifest_file"
done <"$MANIFEST_RECORDS"

verify_manifest_packages_present "$MANIFEST_JSON" "$MANIFEST_RECORDS" || exit 1
