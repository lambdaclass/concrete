#!/usr/bin/env bash
# Emit `Concrete/BuildIdentity.lean` — the identity of the compiler BUILD, embedded in the binary.
#
# WHY EMBEDDED RATHER THAN COMPUTED AT RUN TIME. A receipt must name the compiler that produced it.
# Three earlier attempts named something else:
#
#   `git status --porcelain`  — repository state at REPORT time. Any untracked file invalidated every
#                               receipt, and a binary built dirty then cleaned reported clean.
#   `git rev-parse HEAD`      — the checkout at REPORT time. A binary built from commit A claims B the
#                               moment the checkout moves, and a binary built from uncommitted sources
#                               claims whatever happens to be checked out.
#   `sha256sum /proc/self/exe`— the running executable, which is the right OBJECT, but neither
#                               `/proc/self/exe` nor GNU `sha256sum` exists on macOS, an active CI
#                               platform. Receipt issuance simply refused there.
#
# A digest computed AT BUILD TIME over the compiler's own sources travels with the binary. It is
# portable, costs nothing at run time, and is a property of the artifact rather than of the tree the
# artifact is later pointed at. Uncommitted sources are covered for free — the generator digests the
# working tree, not a commit.
#
# WHAT IT DOES NOT COVER, stated because a receipt that overclaims is worse than one that binds less:
# a binary PATCHED AFTER BUILD keeps the constant its sources produced. This identifies the build, not
# the bytes on disk. Detecting post-build tampering needs an executable digest, which needs a portable
# in-process hash of a 178 MB file — not available today.
#
# STALENESS IS A GATE FAILURE, not a silent wrong answer: `check_build_identity_freshness.sh`
# re-derives this and fails on disagreement, the same discipline `ClassificationTable` is held to.
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

OUT="Concrete/BuildIdentity.lean"

# ---------------------------------------------------------------------------
# THE COMPILER-SOURCE INVENTORY, declared rather than discovered.
#
# ROOTS are the trees and files whose content defines a compiler build. Naming them explicitly is
# the point: a bare `find` over whatever happens to be present answers a slightly different question
# each time the tree changes, and nothing ever says so. A root that should be here and is not is a
# digest that is honest about a subset and silent about the rest.
INVENTORY_ROOTS=(
  "Concrete"        # the compiler itself
  "Main.lean"       # the driver
  "lakefile.toml"   # build configuration: dependencies and targets
  "lean-toolchain"  # the Lean version the artifact is built against
)

# NAMED EXCLUSIONS, each with a reason. Excluding by a name stated here rather than by a filter that
# happens to skip something: the previous form was `grep -v 'Concrete/BuildIdentity.lean'` inline,
# which is indistinguishable from an accident and would silently stop excluding if the path moved.
declare -A INVENTORY_EXCLUSIONS=(
  ["Concrete/BuildIdentity.lean"]="this generator's own output; including it makes the value depend on itself and it never reaches a fixed point"
)

# The resolved inventory: every `*.lean` under the roots, plus the named non-Lean roots, minus the
# named exclusions. Emitted sorted so the digest is a function of content and not of directory order.
inventory_files() {
  {
    for r in "${INVENTORY_ROOTS[@]}"; do
      if [ -d "$r" ]; then
        find "$r" -name '*.lean' -type f 2>/dev/null
      elif [ -f "$r" ]; then
        printf '%s\n' "$r"
      fi
    done
  } | LC_ALL=C sort -u | while IFS= read -r f; do
    [ -n "${INVENTORY_EXCLUSIONS[$f]:-}" ] && continue
    printf '%s\n' "$f"
  done
}

digest_input() {
  inventory_files | while IFS= read -r f; do
    [ -f "$f" ] || continue
    printf '%s\n' "$f"
    cat "$f"
  done
}

# Hashed with the same tool the rest of the repo uses for content digests, so the value is a
# project-standard 128-bit digest rather than a bespoke one. `sha256sum` here is a BUILD-TIME
# dependency only — nothing at run time invokes it, which is the portability point.
if command -v sha256sum >/dev/null 2>&1; then
  DIGEST="$(digest_input | sha256sum | cut -c1-32)"
elif command -v shasum >/dev/null 2>&1; then
  DIGEST="$(digest_input | shasum -a 256 | cut -c1-32)"
else
  echo "FATAL: no sha256sum or shasum available to compute the build identity" >&2
  exit 1
fi

# THE INVENTORY ITSELF IS FINGERPRINTED, separately from the content. The content digest moves when
# a file's TEXT changes; it also moves when a file is added or removed, but indistinguishably. These
# two values make the difference visible: a changed count or list digest with unchanged files means
# the inventory moved, which is a different event from an edit and is the one that silently shrinks
# coverage. `check_build_identity_freshness.sh` re-derives both.
INV_COUNT="$(inventory_files | grep -c . || true)"
if command -v sha256sum >/dev/null 2>&1; then
  INV_DIGEST="$(inventory_files | sha256sum | cut -c1-32)"
else
  INV_DIGEST="$(inventory_files | shasum -a 256 | cut -c1-32)"
fi

# A COLLAPSED INVENTORY MUST NOT PRODUCE A DIGEST. If the roots stopped matching anything, the
# content hash would still be a well-formed 32 hex characters — of nothing — and would look exactly
# like a healthy identity. The floor is far below the real count so it catches collapse, not drift.
if [ "${INV_COUNT:-0}" -lt 50 ]; then
  echo "FATAL: the compiler-source inventory resolved to ${INV_COUNT} files, which cannot be right." >&2
  echo "       Refusing to emit a build identity over a collapsed inventory — it would be a" >&2
  echo "       well-formed digest of almost nothing." >&2
  exit 1
fi

if [ "${#DIGEST}" -ne 32 ]; then
  echo "FATAL: build identity digest is ${#DIGEST} chars, expected 32" >&2
  exit 1
fi

cat > "$OUT" <<LEAN
/-!
# Generated compiler build identity — do not edit

Emitted by \`scripts/gen/build_identity.sh\` from the compiler's own sources. A receipt binds this to
name the compiler that produced it.

EMBEDDED AT BUILD TIME on purpose. Identities derived from the repository at REPORT time —
\`git status\`, \`git rev-parse HEAD\` — describe the tree the binary is later pointed at rather than
the binary, and are wrong in both directions: a binary built from one commit claims another once the
checkout moves, and a binary built from uncommitted sources claims whatever is checked out. This
value travels with the artifact.

A run-time digest of the executable would be strictly stronger and is not portable: neither
\`/proc/self/exe\` nor GNU \`sha256sum\` exists on macOS, which is an active CI platform, and hashing
a 178 MB binary in-process is not viable with the in-repo SHA-256. **Known limit: a binary patched
after build keeps the constant its sources produced.** This identifies the BUILD, not the bytes.

Staleness is a gate failure rather than a silent wrong answer:
\`check_build_identity_freshness.sh\` re-derives this and fails on disagreement.
-/

namespace Concrete

/-- 128-bit digest over the compiler's own sources at build time. -/
def buildIdentity : String := "${DIGEST}"

/-- How many source files the identity above was computed over.

    Carried so that a SHRINKING inventory is detectable. The content digest moves when a file is
    removed, but it moves the same way it does for an ordinary edit — this value distinguishes the
    two, and a digest computed over fewer files is a weaker claim wearing the same shape. -/
def buildIdentitySourceCount : Nat := ${INV_COUNT}

/-- Digest of the inventory's FILE LIST, independent of file contents.

    Moves only when files are added, removed, or renamed. Together with the count, this makes
    "the inventory changed" distinguishable from "a source changed". -/
def buildIdentityInventoryDigest : String := "${INV_DIGEST}"

end Concrete
LEAN
echo "wrote $OUT (build identity ${DIGEST}; ${INV_COUNT} sources, inventory ${INV_DIGEST})"
