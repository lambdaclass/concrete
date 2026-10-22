#!/usr/bin/env bash
# A PRIVATE COPY OF THIS CHECKOUT, for gates that must perturb or populate the tree (R-0546).
#
# In a full pass the gates run JOBS-wide in ONE checkout, and several passes share a host. A gate that
# edits a tracked file (even to restore it a moment later), adds a module under Concrete/, or creates
# probe directories inside the checkout is visible to every concurrent gate: a .lean edit makes every
# concurrent `require_fresh_binary` rebuild the shared compiler from it, a probe module changes the
# proof library every concurrent receipt check binds, and transient files make a concurrent `cp -a` of
# the checkout fail when they vanish under it. Such a gate works in a private copy instead.
#
# The copy is a `git clone --shared` of this checkout at HEAD (its own .git; objects shared), with the
# WORKING-TREE bytes of every tracked file overlaid and working-tree/staged deletions applied, so it is
# this tree rather than HEAD; and with this checkout's .lake seeded copy-on-write and verified by
# `lake build` (scripts/lake-seed.sh), so its compiler is this compiler. Untracked files are not
# copied. The copy takes its own repository lock.

# private_checkout <parent-dir> -> prints the copy's root on stdout; nonzero (and a reason in
# <parent-dir>/private-checkout.log) when a faithful copy could not be made.
private_checkout() {
  local base="$1" root tree log
  root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)" || return 1
  tree="$base/tree"; log="$base/private-checkout.log"
  : > "$log"
  git clone --quiet --shared --no-checkout "$root" "$tree" >>"$log" 2>&1 \
    || { echo "git clone failed" >> "$log"; return 1; }
  git -C "$tree" checkout --quiet --detach "$(git -C "$root" rev-parse HEAD)" >>"$log" 2>&1 \
    || { echo "git checkout failed" >> "$log"; return 1; }
  # THE INDEX TOO, so staged state reads the same in the copy (a staged addition is staged, a staged
  # change reverted in the working tree is still a change). Its blobs are reachable: the clone borrows
  # this repository's object store.
  local idx
  idx="$(git -C "$root" rev-parse --path-format=absolute --git-path index 2>/dev/null)" \
    && [ -f "$idx" ] && cp "$idx" "$tree/.git/index" \
    || { echo "could not copy the index" >> "$log"; return 1; }
  # Overlay the working-tree bytes of every tracked file (one tar stream: xargs could split it into
  # several archives and the reader stops at the first)...
  ( cd "$root" && git ls-files -z | while IFS= read -r -d '' f; do [ -e "$f" ] && printf '%s\0' "$f"; done \
      | tar --null -T - -cf - ) | tar -xf - -C "$tree" \
    || { echo "tar overlay failed" >> "$log"; return 1; }
  # ...and apply deletions relative to HEAD, staged or not, so HEAD's copy is not resurrected.
  { ( cd "$root" && git diff --name-only -z --no-renames --diff-filter=D HEAD )
    ( cd "$root" && git ls-files -z --deleted ); } | while IFS= read -r -d '' f; do
    rm -f "$tree/$f" || { echo "could not apply deletion of $f" >> "$log"; exit 1; }
  done || return 1
  if [ -d "$root/.lake" ]; then
    ( cd "$tree" && bash "$root/scripts/lake-seed.sh" "$root" ) >>"$log" 2>&1 \
      || { echo "lake seed failed" >> "$log"; return 1; }
  fi
  printf '%s' "$tree"
}

# reexec_in_private_checkout "$0" "$@"
#   Called at the top of a gate. Unless already inside one, makes a private checkout and re-executes
#   the SAME gate script from it (so every relative path, ROOT_DIR and the workspace the compiler
#   resolves are the copy's), removes the copy afterwards and exits with the gate's status. A copy that
#   cannot be made is a refusal (exit 2), never a pass.
reexec_in_private_checkout() {
  local self="$1"; shift
  [ "${CONCRETE_IN_PRIVATE_CHECKOUT:-0}" = "1" ] && return 0
  local root rel base tree rc
  root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)" || exit 2
  # The gate's path inside the repository, whatever directory it was invoked from: every caller lives
  # in scripts/tests, so a path that does not resolve under the root falls back to that.
  rel="${self#"$root"/}"
  case "$rel" in /*) rel="scripts/tests/$(basename "$self")" ;; esac
  [ -f "$root/$rel" ] || rel="scripts/tests/$(basename "$self")"
  base="$(mktemp -d)" || { echo "FATAL: mktemp failed; no private checkout" >&2; exit 2; }
  if ! tree="$(private_checkout "$base")"; then
    echo "FATAL: could not make a private checkout for $(basename "$self") — nothing was checked:" >&2
    sed 's/^/    /' "$base/private-checkout.log" | tail -8 >&2
    rm -rf "$base"; exit 2
  fi
  rc=0
  ( cd "$tree" && unset CONCRETE_GATE_LOCK && CONCRETE_IN_PRIVATE_CHECKOUT=1 bash "$tree/$rel" "$@" ) || rc=$?
  rm -rf "$base"
  exit "$rc"
}
