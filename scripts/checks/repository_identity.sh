#!/usr/bin/env bash
# Live endpoints and remote operations share one repository identity definition.
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh"
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
source "$ROOT_DIR/scripts/checks/lib/git_sandbox.sh"
git_sandbox
python3 -I "$ROOT_DIR/scripts/checks/lib/repository_identity.py" "$ROOT_DIR"
# A regenerated move must retain the guard and every consumer changed by this migration.
python3 -I - "$ROOT_DIR" <<'PYOVERLAY'
import pathlib
import shlex
import sys
root = pathlib.Path(sys.argv[1])
text = (root / 'scripts/land/legacy_move.sh').read_text()
overlay = set(shlex.split(text.split('OVERLAY=(', 1)[1].split(')\n', 1)[0], comments=True))
required = {
    '.githooks/pre-push', '.claude/commands/land.md', 'docs/proposals/SPEC_UNIFICATION.md',
    'scripts/checks/workflow_yaml.sh', 'scripts/checks/lib/repository_identity.py',
    'scripts/checks/repository_identity.sh', 'scripts/land/move_prereqs.py',
    'scripts/land/train.sh', 'scripts/land/ready.sh', 'scripts/land/notes.sh',
    'scripts/land/acceptance.sh', 'scripts/land/gates_transport_ssh.sh',
    'scripts/review/review.sh', 'scripts/review/verify.sh',
}
assert not required - overlay, f'move overlay omits {sorted(required - overlay)}'
assert all((root / path).is_file() for path in required)
PYOVERLAY
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
# No installed hooks and no network: invoke the repository hook directly with gates skipped.
git init -q "$TMP/repo"
git -C "$TMP/repo" config core.hooksPath "$TMP/nohooks"
git -C "$TMP/repo" remote add origin "$TMP/local.git"
cd "$TMP/repo"
old_owner=unbalancedparentheses
mixed_owner=UnbalancedParentheses
urls=(
  "https://github.com/$old_owner/concrete2.git"
  "https://github.com/$mixed_owner/Concrete2.git"
  "https://github.com/$old_owner/concrete2.git/"
  "https://github.com/$old_owner/concrete2/"
  "ssh://git@github.com:22/$old_owner/concrete2.git"
  "git@github-alias:$old_owner/concrete2.git"
  "https://$old_owner.github.io/concrete2/"
  "https://api.github.com/repos/$old_owner/concrete2/releases"
)
for url in "${urls[@]}"; do
  if out="$(CONCRETE_SKIP_GATES=1 bash "$ROOT_DIR/.githooks/pre-push" origin "$url" </dev/null 2>&1)"; then
    echo "repository_identity: hook accepted $url" >&2; exit 1
  fi
  [[ "$out" == *"archived repository destination refused"* ]] || { echo "$out" >&2; exit 1; }
  # The very same forms must be found in maintained content.
  printf '%s\n' "$url" > README.md
  if python3 -I "$ROOT_DIR/scripts/checks/lib/repository_identity.py" . >/dev/null; then
    echo "repository_identity: scanner accepted $url" >&2; exit 1
  fi
  rm README.md
  # A stale fetch URL or an extra push URL is refused before any network access.
  git remote set-url origin "$url"
  if python3 -I "$ROOT_DIR/scripts/checks/lib/repository_identity.py" --remote origin 2>/dev/null; then exit 1; fi
  for tool in "$ROOT_DIR/scripts/land/ready.sh" "$ROOT_DIR/scripts/land/notes.sh"; do
    if out="$(bash "$tool" wip/test 2>&1)"; then
      echo "repository_identity: $tool accepted stale origin" >&2; exit 1
    fi
    [[ "$out" == *"archived repository destination refused"* ]] || { echo "$out" >&2; exit 1; }
  done
  git remote set-url origin "$TMP/local.git"
  git config --add remote.origin.pushurl "$url"
  if python3 -I "$ROOT_DIR/scripts/checks/lib/repository_identity.py" --remote origin 2>/dev/null; then exit 1; fi
  git config --unset-all remote.origin.pushurl
done
for name in fork lambdaclass; do
  if CONCRETE_SKIP_GATES=1 bash "$ROOT_DIR/.githooks/pre-push" "$name" "$TMP/local.git" </dev/null 2>/dev/null; then
    echo "repository_identity: hook accepted remote $name" >&2; exit 1
  fi
done
for url in "$TMP/local.git" https://github.com/lambdaclass/concrete.git ssh://git@github.com:22/lambdaclass/concrete.git git@github-alias:lambdaclass/concrete.git; do
  git remote set-url origin "$url"
  CONCRETE_SKIP_GATES=1 bash "$ROOT_DIR/.githooks/pre-push" origin "$url" </dev/null >/dev/null 2>&1
done
echo 'repository_identity: hook, fetch, push, content and remote-name controls pass'
