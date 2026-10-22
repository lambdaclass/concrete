#!/usr/bin/env bash
# SHARED COMPILE-OUTPUT CACHE (R-0546), for gates that run the same analysis report on the same
# input again and again with the same compiler.
#
#   ccache_run "$COMPILER" <file.con> --report <kind> [--json]
#
# behaves like running the command: the same stdout, the same stderr, the same exit status. On a
# hit it replays a stored result instead. Measured on CI (713778e2): check_vc_schema.sh runs
# `concrete examples/hmac_sha256/src/main.con --report vcs --json` 20 times at ~18.6 s each, because
# every VC report starts `lake env lean` discharge batches; check_obligation_report_views.sh runs
# `--report audit` four times on the same input.
#
# A STALE RESULT MUST BE IMPOSSIBLE, so the key is everything the invocation can read, and anything
# the key cannot cover is NOT CACHED (it runs uncached, transparently):
#   * the compiler: sha256 of the binary's bytes (hashed on every call, never by path or mtime) and
#     the BuildIdentity of the tree it sits in;
#   * the exact argument vector, the input's absolute path and the working directory (reports name
#     paths, and `lake env lean` runs in the cwd's workspace);
#   * the input's PROJECT CLOSURE: every file under the nearest enclosing Concrete.toml directory,
#     found the way the compiler finds it (Resolve/Project.lean findProjectRoot). Without a manifest,
#     every file under the input's directory. A project with [dependencies] or [native], a closure
#     outside the repository, or one that would be the whole repository is not cached;
#   * the std library the binary resolves (CONCRETE_STD if it holds src/lib.con, else <repo>/std);
#   * the proof sources (<repo>/proofs) and the Lean workspace of the cwd (lean-toolchain,
#     lakefile.toml, lake-manifest.json), because VC discharge runs `lake env lean` there;
#   * PATH and the resolved paths of every external tool the compiler can spawn (lake, lean, z3,
#     coqc, isabelle, clang, cc, pkg-config, llvm-as, git), and CONCRETE_STD.
# ONLY ALLOWLISTED REPORTS are cached (CCACHE_REPORTS): pure analyses whose output is a function of
# those inputs. Never cached: `-o` builds, --interp, --test, check-proofs / receipts / prove (a fresh
# nonce and a kernel replay that IS the evidence), model-diff (CONCRETE_MODEL_DIFF_PERTURB),
# traces with timings, --version (git state). A gate that tests the compiler's own determinism
# must call the compiler directly, never through this function; CONCRETE_COMPILE_CACHE=0 disables
# the cache everywhere, and =refresh recomputes and overwrites every entry it touches.
#
# Results are shared by every gate and every worktree under ${CONCRETE_COMPILE_CACHE_DIR:-
# $HOME/.cache/concrete-compile}. Entries are written to a private temporary and published by
# rename, so a reader sees a complete result or none. A crash (status >= 128) is never stored.

CCACHE_DIR="${CONCRETE_COMPILE_CACHE_DIR:-$HOME/.cache/concrete-compile}"
CCACHE_REPORTS="${CCACHE_REPORTS:-vcs obligation-ledger audit}"
CCACHE_HITS=0
CCACHE_MISSES=0
CCACHE_BYPASS=0
CCACHE_LAST=""       # hit | miss | bypass:<reason> — for gates and controls

_cc_sha() { # stdin -> sha256 hex
  if command -v sha256sum >/dev/null 2>&1; then sha256sum | cut -d' ' -f1
  else shasum -a 256 | cut -d' ' -f1; fi
}
_cc_sha_file() {
  if command -v sha256sum >/dev/null 2>&1; then sha256sum "$1" | cut -d' ' -f1
  else shasum -a 256 "$1" | cut -d' ' -f1; fi
}
# Content digest of a directory tree: every regular file's relative path and bytes, in a fixed order.
# Symlinks are refused (a link can point anywhere), so a tree containing one is not cacheable.
_cc_tree() {
  local d="$1"
  [ -d "$d" ] || { echo "absent"; return 0; }
  if [ -n "$(find "$d" -type l -print -quit 2>/dev/null)" ]; then return 1; fi
  # One digest PER FILE, then a digest of that list: concatenating contents would let two trees
  # that split the same bytes differently between files collide.
  local hasher="sha256sum"
  command -v sha256sum >/dev/null 2>&1 || hasher="shasum -a 256"
  ( cd "$d" && { find . -type f -print0 | LC_ALL=C sort -z | xargs -0 $hasher /dev/null; } 2>/dev/null \
      | _cc_sha )
}

# Decide eligibility and compute the key into CCACHE_KEY. Returns 1 (with CCACHE_LAST=bypass:...)
# when the invocation must run uncached. Called in the CURRENT shell, never in `$( )`, so the
# bypass reason it records is visible to the caller.
_cc_key() {
  local comp="$1"; shift
  local file="" kind="" json=0 a
  CCACHE_LAST=""; CCACHE_KEY=""
  [ "${CONCRETE_COMPILE_CACHE:-1}" != "0" ] || { CCACHE_LAST="bypass:disabled"; return 1; }
  [ -z "${CONCRETE_MODEL_DIFF_PERTURB:-}" ] || { CCACHE_LAST="bypass:perturb"; return 1; }
  # The argument shape is exact: <file.con> --report <kind> [--json]. Anything else is not a
  # recognised pure analysis.
  [ "$#" -eq 3 ] || [ "$#" -eq 4 ] || { CCACHE_LAST="bypass:shape"; return 1; }
  file="$1"
  case "$file" in *.con) ;; *) CCACHE_LAST="bypass:shape"; return 1 ;; esac
  [ "$2" = "--report" ] || { CCACHE_LAST="bypass:shape"; return 1; }
  kind="$3"
  if [ "$#" -eq 4 ]; then [ "$4" = "--json" ] || { CCACHE_LAST="bypass:shape"; return 1; }; json=1; fi
  case " $CCACHE_REPORTS " in *" $kind "*) ;; *) CCACHE_LAST="bypass:report($kind)"; return 1 ;; esac
  [ -f "$file" ] && [ -x "$comp" ] || { CCACHE_LAST="bypass:missing"; return 1; }

  local comp_abs repo file_abs dir proj std t
  comp_abs="$(cd "$(dirname "$comp")" && pwd -P)/$(basename "$comp")"
  repo="$(cd "$(dirname "$comp_abs")/../../.." 2>/dev/null && pwd -P)"   # .lake/build/bin -> repo
  [ -n "$repo" ] && [ -f "$repo/lakefile.toml" ] || { CCACHE_LAST="bypass:not-a-repo-binary"; return 1; }
  file_abs="$(cd "$(dirname "$file")" && pwd -P)/$(basename "$file")"
  # Project root, as findProjectRoot finds it: the nearest ancestor holding Concrete.toml.
  dir="$(dirname "$file_abs")"; proj=""
  while :; do
    if [ -e "$dir/Concrete.toml" ]; then proj="$dir"; break; fi
    [ "$dir" = "/" ] && break
    dir="$(dirname "$dir")"
  done
  if [ -n "$proj" ]; then
    case "$proj/" in "$repo"/?*) ;; *) CCACHE_LAST="bypass:project-outside-repo"; return 1 ;; esac
    if grep -qE '^[[:space:]]*\[(dependencies|native)' "$proj/Concrete.toml" 2>/dev/null; then
      CCACHE_LAST="bypass:project-deps"; return 1
    fi
  else
    proj="$(dirname "$file_abs")"
    case "$proj/" in "$repo"/?*) ;; *) CCACHE_LAST="bypass:input-outside-repo"; return 1 ;; esac
  fi
  std="$repo/std"
  if [ -n "${CONCRETE_STD:-}" ] && [ -f "$CONCRETE_STD/src/lib.con" ]; then std="$CONCRETE_STD"; fi

  local k_comp k_proj k_std k_proofs k_ws tools tl
  k_comp="$(_cc_sha_file "$comp_abs")" || { CCACHE_LAST="bypass:hash"; return 1; }
  k_proj="$(_cc_tree "$proj")" || { CCACHE_LAST="bypass:symlink"; return 1; }
  k_std="$(_cc_tree "$std")" || { CCACHE_LAST="bypass:symlink"; return 1; }
  k_proofs="$(_cc_tree "$repo/proofs")" || { CCACHE_LAST="bypass:symlink"; return 1; }
  k_ws="$(for t in lean-toolchain lakefile.toml lakefile.lean lake-manifest.json; do
            printf '%s:' "$t"; if [ -f "$t" ]; then _cc_sha_file "$t"; else echo absent; fi; done)"
  tools=""
  for tl in lake lean z3 coqc isabelle clang cc pkg-config llvm-as git; do
    tools="$tools $tl=$(command -v "$tl" 2>/dev/null || echo none)"
  done
  CCACHE_KEY="$({
    echo "concrete-compile-cache-v1"
    echo "compiler_sha256=$k_comp"
    echo "build_identity=$(grep -oE 'buildIdentity : String := "[0-9a-f]+"' "$repo/Concrete/BuildIdentity.lean" 2>/dev/null | grep -oE '[0-9a-f]{32}' | head -1)"
    echo "kind=$kind json=$json"
    printf 'argv='; printf '%s\0' "$@"; echo
    echo "input=$file_abs"
    echo "cwd=$(pwd -P)"
    echo "project=$proj:$k_proj"
    echo "std=$std:$k_std"
    echo "proofs=$k_proofs"
    echo "workspace=$k_ws"
    echo "tools=$tools"
    echo "path=$PATH"
    echo "concrete_std=${CONCRETE_STD:-}"
  } | _cc_sha)"
  [ "${#CCACHE_KEY}" = 64 ] || { CCACHE_LAST="bypass:hash"; return 1; }
}

ccache_run() {
  local comp="$1"; shift
  local key ent rc
  if ! _cc_key "$comp" "$@"; then
    CCACHE_BYPASS=$((CCACHE_BYPASS+1))
    "$comp" "$@"; return $?
  fi
  key="$CCACHE_KEY"
  ent="$CCACHE_DIR/${key:0:2}/$key"
  if [ "${CONCRETE_COMPILE_CACHE:-1}" != "refresh" ] && [ -f "$ent/rc" ]; then
    rc="$(cat "$ent/rc" 2>/dev/null)"
    case "$rc" in ''|*[!0-9]*) ;; *)
      cat "$ent/out"; cat "$ent/err" >&2
      CCACHE_HITS=$((CCACHE_HITS+1)); CCACHE_LAST="hit"
      return "$rc" ;;
    esac
  fi
  local tmp
  mkdir -p "$CCACHE_DIR/${key:0:2}" 2>/dev/null
  tmp="$(mktemp -d "$CCACHE_DIR/${key:0:2}/.tmp.XXXXXX" 2>/dev/null)" || {
    CCACHE_BYPASS=$((CCACHE_BYPASS+1)); CCACHE_LAST="bypass:unwritable"; "$comp" "$@"; return $?; }
  "$comp" "$@" >"$tmp/out" 2>"$tmp/err"; rc=$?
  cat "$tmp/out"; cat "$tmp/err" >&2
  CCACHE_MISSES=$((CCACHE_MISSES+1)); CCACHE_LAST="miss"
  if [ "$rc" -lt 128 ]; then
    echo "$rc" > "$tmp/rc"
    rm -rf "$ent" 2>/dev/null
    mv "$tmp" "$ent" 2>/dev/null || rm -rf "$tmp"
  else
    rm -rf "$tmp"
  fi
  return "$rc"
}
