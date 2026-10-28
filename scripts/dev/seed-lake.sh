#!/usr/bin/env bash
# SEED A CHECKOUT'S .lake/build FROM AN EXISTING BUILD, SO `lake build` STARTS WARM (R-0546 part 2).
#
# A fresh worktree, a fresh CI workspace and a fresh agent clone all rebuild the compiler from
# nothing (about 4 minutes). Most of that work was already done by another checkout of nearly the
# same sources. This script copies that build in. It NEVER decides that the copy is up to date:
# `lake build` still runs afterwards and re-checks every module's trace, rebuilding whatever differs.
# Seeding changes how long the build takes, not whether it happens.
#
# WHAT MUST MATCH. Lake's per-module trace mixes in the Lean VERSION ("Lean 4.34.1, commit ...") but
# not the lean EXECUTABLE. Two binaries of one release (elan's official archive, and the devshell's
# autoPatchelf'd copy on Linux) would therefore accept each other's .olean files, and a module
# kernel-checked by one would be credited to the other (R-0208: the checker identity is the
# executable that ran). So the seed key binds:
#
#   platform (uname -s/-m), lean-toolchain, sha256(lake-manifest.json),
#   sha256(the lean executable itself, resolved through `lean --print-prefix`)
#
# and a source whose key differs is REFUSED (exit 3), leaving a cold build. Sources are NOT part of
# the key: a seed from different sources is exactly the useful case, and lake handles it. Neither
# is lakefile.toml: lake mixes each module's options into its trace, and a branch that adds a
# library would otherwise never get a warm start.
# The source hash (Concrete/, proofs/, Concrete.lean, Main.lean) only picks the best entry in a
# cache root (an exact source match first, else the newest entry for the key).
#
# SOURCES OF A SEED
#   default            the main checkout of this repository (the git common dir's work tree)
#   --from DIR         another checkout (DIR/.lake/build) or one published cache entry (DIR/build)
#   --cache-root ROOT  a published cache: ROOT/<key>/<srchash>/ , ROOT/<key>/latest -> <srchash>
# For a checkout, the lean executable that built it is read from its own lake traces (every
# `.> LEAN_PATH=... <lean> ...` log line); all of them must name a binary whose hash equals ours.
#
# USAGE
#   seed-lake.sh [--from DIR | --cache-root ROOT] [--force] [--build [lake args...]]
#   seed-lake.sh --print-key            key, source hash and the key preimage for this checkout
#   seed-lake.sh --publish ROOT         (cache producer) run `lake build`, then publish .lake/build
#   --checkout DIR                      act on DIR instead of the checkout holding this script
#   --elan                              check against ~/.elan's lean only (CI: what lean-action runs)
#
# RESIDUAL RISK (accepted, documented): seeding from a CHECKOUT trusts that its traces describe its
# artifacts. The quiet window, the artifact-newer-than-trace check and the provenance re-check on the
# copy catch a running or interrupted build; a checkout whose files were edited by hand with
# matching timestamps is not detected. CI seeds only from published entries, which are immutable.
#
# EXIT  0 seeded (or published) | 3 refused: no usable seed, nothing changed | 2 usage/internal error
# With --build, the exit status is lake build's, whatever seeding did.
#
# Environment: LEAN_BIN overrides the lean executable whose identity is checked (default: the one
# `lean --print-prefix` names, falling back to ~/.elan/bin/lean). CONCRETE_LAKE_KEEP (default 3)
# is how many entries per key --publish keeps.
set -uo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

die()    { echo "seed-lake: error: $*" >&2; exit 2; }
refuse() { echo "seed-lake: not seeding: $*" >&2; echo "seed-lake: lake build will start cold" >&2; exit 3; }
say()    { echo "seed-lake: $*" >&2; }

sha256_file() {
  if command -v sha256sum >/dev/null 2>&1; then sha256sum "$1" | cut -d' ' -f1
  else shasum -a 256 "$1" | cut -d' ' -f1; fi
}
sha256_stdin() {
  if command -v sha256sum >/dev/null 2>&1; then sha256sum | cut -d' ' -f1
  else shasum -a 256 | cut -d' ' -f1; fi
}

# The lean executable this checkout will build with.
# --elan: resolve through ~/.elan/bin/lean only, the toolchain lean-action builds with, whatever
# else is on PATH.
our_lean_bin() {
  if [ -n "${LEAN_BIN:-}" ]; then printf '%s\n' "$LEAN_BIN"; return; fi
  local lean="" prefix
  if [ "$ELAN_ONLY" = 1 ]; then [ -x "$HOME/.elan/bin/lean" ] && lean="$HOME/.elan/bin/lean"
  elif command -v lean >/dev/null 2>&1; then lean="lean"
  elif [ -x "$HOME/.elan/bin/lean" ]; then lean="$HOME/.elan/bin/lean"; fi
  [ -n "$lean" ] || return 1
  prefix="$(cd "$ROOT_DIR" && "$lean" --print-prefix 2>/dev/null)" || return 1
  [ -x "$prefix/bin/lean" ] || return 1
  printf '%s\n' "$prefix/bin/lean"
}

# key_preimage <checkout-dir> <lean-binary>: the lines the key is a hash of.
key_preimage() {
  local dir="$1" bin="$2" f
  for f in lean-toolchain lake-manifest.json; do
    [ -f "$dir/$f" ] || { echo "missing $dir/$f" >&2; return 1; }
  done
  [ -f "$bin" ] || { echo "missing lean binary $bin" >&2; return 1; }
  printf 'format=concrete-lake-seed-v1\n'
  printf 'platform=%s-%s\n' "$(uname -s)" "$(uname -m)"
  printf 'lean-toolchain=%s\n' "$(tr -d '[:space:]' < "$dir/lean-toolchain")"
  printf 'lake-manifest.json=%s\n' "$(sha256_file "$dir/lake-manifest.json")"
  printf 'lean-binary=%s\n' "$(sha256_file "$bin")"
}
key_of_preimage() { sha256_stdin | cut -c1-32; }

# Hash of the Lean sources lake compiles (tracked files, working-tree contents).
source_hash() {
  local dir="$1"
  ( cd "$dir" || exit 1
    git ls-files -z -- Concrete.lean Main.lean Browser.lean 'Concrete/*.lean' 'proofs/*.lean' \
      | LC_ALL=C sort -z \
      | while IFS= read -r -d '' f; do
          [ -f "$f" ] && printf '%s %s\n' "$(sha256_file "$f")" "$f"
        done ) | sha256_stdin | cut -c1-32
}

# lean_commit <lean-binary>: the commit the binary reports (lake's traces record this, not the bytes).
lean_commit() { "$1" --version 2>/dev/null | grep -oE 'commit [0-9a-f]+' | awk 'NR==1{print $2}'; }

# trace_lean_bins <build-dir> <commit>: the distinct lean executables named in the lake traces of
# modules built by Lean <commit>. Traces from another Lean version are left out: lake rebuilds those
# modules itself (the version is in every trace), and a checkout keeps the traces of modules that no
# longer exist, from whatever toolchain last built them.
# EVERY such trace must name its executable: one that does not (an artifact fetched rather than
# compiled, an unknown log format) prints UNKNOWN:<trace>, and callers refuse the whole build.
trace_lean_bins() {
  local build="$1" commit="$2" t line
  [ -n "$commit" ] || return 0
  find "$build/lib" -name '*.trace' -type f -exec grep -lF "commit $commit" {} + 2>/dev/null \
    | while IFS= read -r t; do
        line="$(grep -ohE '\.> LEAN_PATH=[^ ]* [^ "]*/bin/lean ' "$t" | awk 'NR==1{print $3}')"
        if [ -n "$line" ]; then printf '%s\n' "$line"; else printf 'UNKNOWN:%s\n' "$t"; fi
      done | LC_ALL=C sort -u
}

# check_trace_bins <build-dir> <our-binary>: every same-version module was compiled by a binary
# with our binary's bytes. Prints the reason and returns 1 otherwise.
# ASSUMPTION: the binary at a recorded path still has the bytes it had when it compiled. That holds
# for Nix store paths (immutable) and for elan toolchains (one directory per release, official
# archive); a hand-replaced executable at the same path would defeat this check.
check_trace_bins() {
  local build="$1" bin="$2" ours b n=0
  ours="$(sha256_file "$bin")"
  while IFS= read -r b; do
    [ -n "$b" ] || continue
    n=$((n+1))
    case "$b" in UNKNOWN:*) echo "${b#UNKNOWN:} does not name the lean that built it"; return 1 ;; esac
    [ -f "$b" ] || { echo "$build was built by $b, which is not readable here"; return 1; }
    [ "$(sha256_file "$b")" = "$ours" ] || { echo "$build was built by a different lean executable ($b) than $bin"; return 1; }
  done < <(trace_lean_bins "$build" "$(lean_commit "$bin")")
  [ $n -gt 0 ] || { echo "$build has no module trace from this Lean version"; return 1; }
  # AN INTERRUPTED MODULE. Lake writes a module's trace after the lean process that wrote its
  # .olean/.ilean exits. A build killed or suspended in between leaves an artifact NEWER than its
  # trace, and the old trace would vouch for the new artifact. Refuse any such pair.
  local t a
  while IFS= read -r t; do
    for a in "${t%.trace}.olean" "${t%.trace}.ilean"; do
      if [ -e "$a" ] && [ "$a" -nt "$t" ]; then echo "$a is newer than its trace (an interrupted build)"; return 1; fi
    done
  done < <(find "$build/lib" -name '*.trace' -type f 2>/dev/null)
}

# copy_tree <src> <dst>: copy-on-write where the file system can, a plain copy otherwise. Never
# hardlinks: lake rewrites trace and hash files in place, which would write through into the seed.
copy_tree() {
  local src="$1" dst="$2"
  if [ "$(uname -s)" = Darwin ]; then
    cp -c -R -p "$src" "$dst" 2>/dev/null || { rm -rf "$dst"; cp -R -p "$src" "$dst"; }
  else
    cp -a --reflink=auto "$src" "$dst"
  fi
}

MODE=seed FROM="" CACHE_ROOT="" FORCE=0 BUILD=0 PUBLISH_ROOT="" ELAN_ONLY=0
BUILD_ARGS=()
while [ $# -gt 0 ]; do
  case "$1" in
    --from)       [ $# -ge 2 ] || die "--from needs a directory"; FROM="$2"; shift 2 ;;
    --cache-root) [ $# -ge 2 ] || die "--cache-root needs a directory"; CACHE_ROOT="$2"; shift 2 ;;
    --force)      FORCE=1; shift ;;
    --elan)       ELAN_ONLY=1; shift ;;
    --build)      BUILD=1; shift; BUILD_ARGS=("$@"); break ;;
    --checkout)   [ $# -ge 2 ] || die "--checkout needs a directory"; ROOT_DIR="$(cd "$2" && pwd)" || die "no such checkout: $2"; shift 2 ;;
    --print-key)  MODE=print-key; shift ;;
    --publish)    [ $# -ge 2 ] || die "--publish needs a cache root"; MODE=publish; PUBLISH_ROOT="$2"; shift 2 ;;
    -h|--help)    sed -n '2,45p' "$0"; exit 0 ;;
    *)            die "unknown argument: $1 (see --help)" ;;
  esac
done
[ -n "$FROM" ] && [ -n "$CACHE_ROOT" ] && die "--from and --cache-root are exclusive"
cd "$ROOT_DIR" || die "cannot enter $ROOT_DIR"

# With --build the build ALWAYS runs, whatever seeding concluded. This is the only exit path of
# --build, so no outcome of seeding can stand in for a build.
run_build_after() {
  ( "$@" ); local rc=$?
  [ $rc -eq 0 ] || [ $rc -eq 3 ] || say "seeding failed (exit $rc); building cold"
  say "running: lake build ${BUILD_ARGS[*]:-}"
  # ${a[@]+...}: an empty array is "unbound" under set -u in bash 3.2 (macOS /bin/bash).
  exec lake build ${BUILD_ARGS[@]+"${BUILD_ARGS[@]}"}
}

do_print_key() {
  local bin pre
  bin="$(our_lean_bin)" || die "no lean found (set LEAN_BIN, or run inside nix develop / with elan)"
  pre="$(key_preimage "$ROOT_DIR" "$bin")" || die "cannot compute the key"
  echo "key=$(printf '%s\n' "$pre" | key_of_preimage)"
  echo "source-hash=$(source_hash "$ROOT_DIR")"
  echo "lean=$bin"
  printf '%s\n' "$pre"
}

do_seed() {
  local bin ours ourkey src="" srcpre srckey dst="$ROOT_DIR/.lake/build" kind="" entry srch from_checkout=0
  bin="$(our_lean_bin)" || refuse "no lean executable found to check the seed against (set LEAN_BIN)"
  ours="$(key_preimage "$ROOT_DIR" "$bin")" || refuse "cannot compute this checkout's key"
  ourkey="$(printf '%s\n' "$ours" | key_of_preimage)"

  if [ -e "$dst" ] && [ "$FORCE" != 1 ]; then
    refuse "$dst already exists (use --force to replace it)"
  fi

  if [ -n "$CACHE_ROOT" ]; then
    [ -d "$CACHE_ROOT/$ourkey" ] || refuse "no cache entry for key $ourkey under $CACHE_ROOT"
    srch="$(source_hash "$ROOT_DIR")"
    if [ -d "$CACHE_ROOT/$ourkey/$srch/build" ]; then entry="$CACHE_ROOT/$ourkey/$srch"; kind="exact sources"
    elif [ -d "$CACHE_ROOT/$ourkey/latest/build" ]; then
      entry="$(cd "$CACHE_ROOT/$ourkey/latest" && pwd -P)"; kind="newest entry"
    else refuse "cache key $ourkey has no usable entry"; fi
    FROM="$entry"
  fi

  if [ -z "$FROM" ]; then
    local common
    common="$(git -C "$ROOT_DIR" rev-parse --path-format=absolute --git-common-dir 2>/dev/null)" \
      || refuse "not a git checkout, and no --from given"
    FROM="$(dirname "$common")"
    kind="main checkout"
  fi
  FROM="$(cd "$FROM" 2>/dev/null && pwd -P)" || refuse "seed source does not exist"
  [ "$FROM" != "$(cd "$ROOT_DIR" && pwd -P)" ] || refuse "the seed source is this checkout"

  if [ -f "$FROM/seed.key" ] && [ -d "$FROM/build" ]; then
    # A published entry: its producer recorded the preimage it was built under.
    src="$FROM/build"; kind="${kind:-cache entry}"
    srcpre="$(cat "$FROM/seed.key")"
  elif [ -d "$FROM/.lake/build/lib" ]; then
    # A checkout: re-derive its key from its own files and from the lean its traces name.
    src="$FROM/.lake/build"; kind="${kind:-checkout}"; from_checkout=1
    local why
    why="$(check_trace_bins "$src" "$bin")" || refuse "$why"
    srcpre="$(key_preimage "$FROM" "$bin")" || refuse "cannot compute the key of $FROM"
  else
    refuse "$FROM has neither .lake/build nor a published build"
  fi

  srckey="$(printf '%s\n' "$srcpre" | key_of_preimage)"
  if [ "$srckey" != "$ourkey" ]; then
    say "seed key differs:"
    diff <(printf '%s\n' "$srcpre") <(printf '%s\n' "$ours") | sed 's/^/seed-lake:   /' >&2
    refuse "toolchain, manifest, platform or lean executable differ from $FROM"
  fi

  mkdir -p "$ROOT_DIR/.lake" || die "cannot create .lake"
  # One seeder at a time per checkout; the existence check is repeated under the lock.
  SEED_LOCK="$ROOT_DIR/.lake/seed.lock"
  mkdir "$SEED_LOCK" 2>/dev/null || { SEED_LOCK=""; refuse "another seed-lake is running here ($ROOT_DIR/.lake/seed.lock)"; }
  local tmp="$ROOT_DIR/.lake/build.seeding.$$" stamp="$ROOT_DIR/.lake/seed.stamp.$$"
  SEED_TMP="$tmp" SEED_STAMP="$stamp"
  trap 'rm -rf "${SEED_TMP:-}" "${SEED_STAMP:-}" "${SEED_LOCK:-}"' EXIT
  if [ -e "$dst" ] && [ "$FORCE" != 1 ]; then refuse "$dst already exists (use --force to replace it)"; fi
  rm -rf "$tmp"
  local t0=$SECONDS
  # A CONSISTENT SNAPSHOT. A build running in the source could pair an old trace with a new .olean.
  # Lake takes no lock we could share, so the source must be QUIET: nothing in it modified from
  # $quiet seconds before the copy until the copy ends. Lake writes a module's trace right after the
  # lean process that wrote its .olean exits, so a build in progress always shows up as a recent
  # write. Published cache entries need none of this: they are moved into place whole, after a
  # completed build, and never written again.
  local quiet="${CONCRETE_SEED_QUIET:-10}"
  if [ "$from_checkout" = 1 ]; then
    : > "$stamp"
    perl -e '$t = time - shift; utime $t, $t, shift' "$quiet" "$stamp" || die "cannot backdate $stamp"
    if [ -z "$(find "$src" -newer "$stamp" -print 2>/dev/null | awk 'NR==1')" ]; then
      copy_tree "$src" "$tmp" || { rm -rf "$tmp" "$stamp"; die "copy from $src failed"; }
    fi
    if [ -n "$(find "$src" -newer "$stamp" -print 2>/dev/null | awk 'NR==1')" ]; then
      rm -rf "$tmp" "$stamp"; refuse "$src was written to within ${quiet}s of or during the copy (a build is running there)"
    fi
    rm -f "$stamp"
  else
    copy_tree "$src" "$tmp" || { rm -rf "$tmp"; die "copy from $src failed"; }
  fi
  # Provenance is re-checked on the COPY: what was validated is exactly what will be used.
  if [ "$from_checkout" = 1 ]; then
    local why2
    why2="$(check_trace_bins "$tmp" "$bin")" || { rm -rf "$tmp"; refuse "$why2"; }
  fi
  chmod -R u+w "$tmp" 2>/dev/null || true
  rm -rf "$dst" && mv "$tmp" "$dst" || die "cannot move the seed into place"
  { echo "source=$src"; echo "kind=$kind"; echo "key=$ourkey"; echo "seeded=$(date -u +%Y-%m-%dT%H:%M:%SZ)"
    echo "note=provenance only; nothing reads this as a freshness signal, lake build re-checks every trace"
  } > "$ROOT_DIR/.lake/seed-provenance"
  say "seeded .lake/build from $src ($kind, key $ourkey) in $((SECONDS - t0))s"
  say "this is a warm START, not a build: run lake build (it re-checks every module)"
}

do_publish() {
  local root="$PUBLISH_ROOT" bin pre key srch dest tmp keep="${CONCRETE_LAKE_KEEP:-3}"
  umask 022   # the cache is world-readable and writable by its producer only
  bin="$(our_lean_bin)" || die "no lean found"
  pre="$(key_preimage "$ROOT_DIR" "$bin")" || die "cannot compute the key"
  key="$(printf '%s\n' "$pre" | key_of_preimage)"
  srch="$(source_hash "$ROOT_DIR")"
  # Publish only what a successful build in THIS process produced.
  lake build || die "lake build failed; nothing published"
  local why
  why="$(check_trace_bins "$ROOT_DIR/.lake/build" "$bin")" || die "not publishing: $why"
  mkdir -p "$root/$key" || die "cannot create $root/$key"
  dest="$root/$key/$srch"
  if [ -d "$dest/build" ]; then say "already published: $dest"; else
    tmp="$root/$key/.tmp.$srch.$$"
    rm -rf "$tmp"; mkdir -p "$tmp"
    copy_tree "$ROOT_DIR/.lake/build" "$tmp/build" || { rm -rf "$tmp"; die "copy failed"; }
    printf '%s\n' "$pre" > "$tmp/seed.key"
    { echo "commit=$(git rev-parse HEAD 2>/dev/null || echo unknown)"
      echo "published=$(date -u +%Y-%m-%dT%H:%M:%SZ)"; echo "lean=$bin"; } > "$tmp/seed.meta"
    chmod -R a+rX,go-w "$tmp"
    mv "$tmp" "$dest" || die "cannot move $tmp into place"
    say "published $dest"
  fi
  ln -sfn "$srch" "$root/$key/.latest.$$" && mv -fT "$root/$key/.latest.$$" "$root/$key/latest" 2>/dev/null \
    || { rm -f "$root/$key/latest"; mv -f "$root/$key/.latest.$$" "$root/$key/latest"; }
  # Keep the newest $keep entries for this key (never the one just published).
  local old
  ls -1dt "$root/$key"/*/ 2>/dev/null | awk -v k="$keep" 'NR>k' | while IFS= read -r old; do
    old="${old%/}"
    case "$(basename "$old")" in latest|"$srch") continue ;; esac
    [ -L "$old" ] || rm -rf "$old"
  done
}

case "$MODE" in
  print-key) do_print_key ;;
  publish)   do_publish ;;
  seed)
    if [ "$BUILD" = 1 ]; then run_build_after do_seed; else do_seed; fi ;;
esac
