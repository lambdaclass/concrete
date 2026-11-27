#!/usr/bin/env bash
# Restore verification and bounded-retry fetch for pinned Lake dependency sources.
#
# Reads lake-manifest.json and lean-toolchain from the current working directory.
# Verifies that .lake/packages/<dep> contains ONLY a clean git checkout at the
# exact pinned commit rev and manifest URL, with no .lake build directory, no
# untracked/ignored artifacts and no partial-clone promisor remote. Any missing,
# mismatched, dirty or corrupt package is deleted and refetched with bounded
# retries and exponential backoff.
#
# Environment configuration:
#   LAKE_FETCH_ATTEMPTS      max fetch attempts per package (default: 3)
#   LAKE_FETCH_BACKOFF_BASE  initial retry delay in seconds, doubled per retry (default: 10)
#   LAKE_FETCH_TIMEOUT       per-attempt git fetch timeout in seconds (default: 180)
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

TMP_DIR="$(mktemp -d "${TMPDIR:-/tmp}/lake-packages.XXXXXX")"
CURRENT_STAGE=""
cleanup() {
  [ -z "${CURRENT_STAGE:-}" ] || rm -rf "$CURRENT_STAGE"
  [ -z "${TMP_DIR:-}" ] || rm -rf "$TMP_DIR"
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
  rm -rf "$PACKAGES_DIR"
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
    rm -rf "$entry"
  fi
done
shopt -u nullglob dotglob

# Verify that DIR is a clean, offline-complete git checkout of URL at exact commit REV
# with no .lake build directory and no untracked, modified or ignored files.
verify_package_dir() {
  local dir="$1"
  local url="$2"
  local rev="$3"
  local head_rev=""
  local remote_url=""
  local promisor=""
  local status=""
  local nested_lake=""

  if [ ! -d "$dir" ] || [ -L "$dir" ]; then
    echo "missing or symlink directory"
    return 1
  fi
  if [ ! -d "$dir/.git" ] || [ -L "$dir/.git" ]; then
    echo "missing or non-directory .git"
    return 1
  fi
  if [ -e "$dir/.lake" ] || [ -L "$dir/.lake" ]; then
    echo "unexpected .lake directory inside dependency"
    return 1
  fi
  nested_lake="$(find "$dir" -path "$dir/.git" -prune -o -name .lake -print -quit 2>/dev/null || true)"
  if [ -n "$nested_lake" ]; then
    echo "unexpected nested .lake path: $nested_lake"
    return 1
  fi
  promisor="$(git -C "$dir" config --get remote.origin.promisor 2>/dev/null || true)"
  if [ -n "$promisor" ]; then
    echo "partial-clone promisor remote is not allowed ($promisor)"
    return 1
  fi
  if ! head_rev="$(git -C "$dir" rev-parse --verify --quiet "HEAD^{commit}" 2>/dev/null)"; then
    echo "unreadable HEAD commit"
    return 1
  fi
  if [ "$head_rev" != "$rev" ]; then
    echo "HEAD is $head_rev, manifest pins $rev"
    return 1
  fi
  if ! remote_url="$(git -C "$dir" remote get-url origin 2>/dev/null)"; then
    echo "missing origin remote"
    return 1
  fi
  if [ "$remote_url" != "$url" ]; then
    echo "origin URL is $remote_url, manifest pins $url"
    return 1
  fi
  if ! status="$(git -C "$dir" status --porcelain --untracked-files=all --ignored=matching 2>/dev/null)"; then
    echo "git status failed"
    return 1
  fi
  if [ -n "$status" ]; then
    echo "dirty, untracked or ignored files present"
    return 1
  fi
  return 0
}

run_bounded_git() {
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

fetch_once() {
  local stage="$1"
  local url="$2"
  local rev="$3"
  local err_file="$4"
  local status_file="$5"
  local outcome=""

  if ! git init -q "$stage" >"$err_file" 2>&1; then
    return 1
  fi
  if ! git -C "$stage" remote add origin "$url" >>"$err_file" 2>&1; then
    return 1
  fi

  run_bounded_git "$err_file" "$status_file" git -C "$stage" fetch -q --depth 1 --no-tags origin "$rev"
  outcome="$(cat "$status_file" 2>/dev/null || true)"
  if [ "$outcome" != "exit 0" ]; then
    if [ "$outcome" = "timeout" ]; then
      echo "fetch timed out after ${FETCH_TIMEOUT}s" >>"$err_file"
      return 1
    fi
    # Fall back to fetching advertised refs only if the server refuses direct SHA-1 wants.
    if grep -qiE 'not our ref|unadvertised object|couldn'\''t find remote ref|no such remote ref' "$err_file"; then
      run_bounded_git "$err_file" "$status_file" git -C "$stage" fetch -q --no-tags origin
      outcome="$(cat "$status_file" 2>/dev/null || true)"
      if [ "$outcome" != "exit 0" ]; then
        if [ "$outcome" = "timeout" ]; then
          echo "fallback fetch timed out after ${FETCH_TIMEOUT}s" >>"$err_file"
        fi
        return 1
      fi
    else
      return 1
    fi
  fi

  if ! GIT_TERMINAL_PROMPT=0 git -C "$stage" -c advice.detachedHead=false checkout -q --detach "$rev" >>"$err_file" 2>&1; then
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
  local stage=""
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

    stage="$(mktemp -d "$PACKAGES_DIR/.fetch-$name.XXXXXX")"
    CURRENT_STAGE="$stage"
    if fetch_once "$stage" "$url" "$rev" "$err_file" "$status_file"; then
      if verify_err="$(verify_package_dir "$stage" "$url" "$rev")"; then
        rm -rf "$dest"
        mv "$stage" "$dest"
        CURRENT_STAGE=""
        if verify_err="$(verify_package_dir "$dest" "$url" "$rev")"; then
          echo "ok   lake-packages: $name fetched and verified at $rev (attempt $attempt/$ATTEMPTS)"
          return 0
        fi
        echo "warn lake-packages: $name: post-move verification failed ($verify_err)" >&2
        rm -rf "$dest"
      else
        echo "warn lake-packages: $name: fetched checkout failed verification ($verify_err)" >&2
      fi
    else
      echo "warn lake-packages: $name: fetch attempt $attempt/$ATTEMPTS failed: $(tr '\n' ' ' <"$err_file" | head -c 400)" >&2
    fi

    rm -rf "$stage"
    CURRENT_STAGE=""
    attempt=$((attempt + 1))
  done

  echo "error: lake-packages: $name: failed to fetch $rev from $url after $ATTEMPTS attempt(s)" >&2
  return 1
}

while IFS=$'\t' read -r name url rev; do
  [ -n "$name" ] || continue
  pkg_dir="$PACKAGES_DIR/$name"
  if [ -e "$pkg_dir" ] || [ -L "$pkg_dir" ]; then
    if reason="$(verify_package_dir "$pkg_dir" "$url" "$rev")"; then
      echo "ok   lake-packages: $name cache verified at $rev"
      continue
    fi
    echo "warn lake-packages: $name: restored package failed verification ($reason); deleting and refetching" >&2
    rm -rf "$pkg_dir"
  else
    echo "info lake-packages: $name: not in cache; fetching $rev"
  fi
  fetch_package_with_retry "$name" "$url" "$rev"
done <"$MANIFEST_ROWS"
