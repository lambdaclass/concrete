#!/usr/bin/env bash
# Single Codex review runner. Arguments: root sha prompt piece output log.
# The private archive is disposable, not a security boundary for arbitrary code.
set -euo pipefail
root="$1" sha="$2" prompt="$3" piece="$4" out="$5" log="$6"
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
bin="$(command -v "${REVIEW_CODEX_BIN:-codex}")"
case "$bin" in /*) ;; *) bin="$PWD/$bin" ;; esac
case "$out" in /*) ;; *) out="$PWD/$out" ;; esac
private="$(mktemp -d)"
private="$(cd "$private" && pwd -P)"
child=
cancel() {
  trap '' HUP INT TERM
  if [ -n "$child" ]; then
    kill -TERM -- "-$child" 2>/dev/null || true
    for _ in {1..20}; do
      kill -0 -- "-$child" 2>/dev/null || break
      sleep 0.05
    done
    kill -KILL -- "-$child" 2>/dev/null || true
    wait "$child" 2>/dev/null || true
  fi
  exit 2
}
trap 'rm -rf "$private"' EXIT
trap cancel HUP INT TERM
mkdir "$private/tree" "$private/tmp"
git -C "$root" archive --format=tar "$sha" | tar -xf - -C "$private/tree"
# Check every path, byte and executable bit, including symlink targets. Fail closed
# on archive export-ignore/export-subst and unsupported gitlinks rather than review
# a tree different from the commit.
python3 -I "$here/read_evidence.py" tree "$root" "$sha" "$private/tree" >"$private/probe"
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
environment=("PATH=$PATH" "TMPDIR=$private/tmp" GIT_OPTIONAL_LOCKS=0)
for variable in HOME CODEX_HOME TERM LANG LC_ALL SSL_CERT_FILE SSL_CERT_DIR LEAN_NUM_THREADS; do
  if [[ -v $variable ]]; then environment+=("$variable=${!variable}"); fi
done
cancelled=0
trap 'cancelled=1' HUP INT TERM
set -m
(
  cd "$private/tree"
  exec env -i "${environment[@]}" "$bin" exec --sandbox danger-full-access \
    --skip-git-repo-check --json --ephemeral --color never -C "$private/tree" -o "$out" "$final_prompt"
) <"$piece" >"$log" &
child=$!
trap cancel HUP INT TERM
[ "$cancelled" = 0 ] || cancel
wait "$child" || rc=$?
child=
python3 -I "$here/read_evidence.py" events "$private/probe" "$log" "$out" "$rc"
