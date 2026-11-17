#!/usr/bin/env bash
# Single Codex review runner. Arguments: root sha prompt piece output log.
# The private archive is disposable, not a security boundary for arbitrary code.
set -euo pipefail
root="$1" sha="$2" prompt="$3" piece="$4" out="$5" log="$6"
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
private="$(mktemp -d)"
trap 'rm -rf "$private"' EXIT
trap 'exit 2' HUP INT TERM
mkdir "$private/tree"
git -C "$root" archive --format=tar "$sha" >"$private/tree.tar"
tar -xf "$private/tree.tar" -C "$private/tree"
# Check every path, byte and executable bit, including symlink targets. Fail closed
# on archive export-ignore/export-subst and unsupported gitlinks rather than review
# a tree different from the commit.
python3 "$here/read_evidence.py" tree "$root" "$sha" "$private/tree" >"$private/probe"
{
  cat "$prompt"
  printf '\nThis review is read-only. Do not modify files. The working directory is a private copy of commit %s, with no Git metadata.\n' "$sha"
  printf 'Before reviewing, run this repository-read probe and leave its output in the command transcript:\n'
  sed -n '1p' "$private/probe"
} >"$private/prompt"
final_prompt="$(cat "$private/prompt")"
printf '%s' "$final_prompt" >"$prompt"
# Keep optional locks disabled as defense in depth for git commands; the archive
# has no index, so bug 209 no longer depends on this environment setting.
rc=0
(for variable in $(git -C "$root" rev-parse --local-env-vars); do unset "$variable"; done
 cd "$private/tree"
 GIT_OPTIONAL_LOCKS=0 "${REVIEW_CODEX_BIN:-codex}" exec --sandbox danger-full-access \
 --skip-git-repo-check --json --ephemeral --color never -C "$private/tree" -o "$out" \
 "$final_prompt" <"$piece") >"$log" 2>&1 || rc=$?
[ "$rc" -eq 0 ] || exit 2
python3 "$here/read_evidence.py" events "$private/probe" "$log" "$out"
