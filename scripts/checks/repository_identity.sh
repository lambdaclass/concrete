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
    'scripts/land/acceptance.sh', 'scripts/land/gates_transport_ssh.sh', 'scripts/land/run-at-ref.sh',
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
# A real local bundle ensures stale-origin probes cannot pass merely on a missing input.
git -C "$TMP/repo" -c user.name=Test -c user.email=test@example.com commit --allow-empty -qm fixture
git -C "$TMP/repo" bundle create "$TMP/input.bundle" HEAD
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
  if out="$(RUN_AT_REF_REPO="$TMP/repo" RUN_AT_REF_SOURCE="$TMP/input.bundle" RUN_AT_REF_RUNS="$TMP/runs" \
      bash "$ROOT_DIR/scripts/land/run-at-ref.sh" HEAD true 2>&1)"; then
    echo "repository_identity: runner accepted stale origin with local bundle" >&2; exit 1
  fi
  [[ "$out" == *"archived repository destination refused"* ]] || { echo "$out" >&2; exit 1; }
  [[ ! -e "$TMP/runs" ]] || { echo 'runner touched run state before refusal' >&2; exit 1; }
  git remote set-url origin "$TMP/local.git"
  git config --add remote.origin.pushurl "$url"
  if python3 -I "$ROOT_DIR/scripts/checks/lib/repository_identity.py" --remote origin 2>/dev/null; then exit 1; fi
  git config --unset-all remote.origin.pushurl
done
git remote set-url origin "$TMP/local.git"
for name in fork lambdaclass; do
  git remote add "$name" "$TMP/repo"
  if out="$(RUN_AT_REF_REPO="$TMP/repo" RUN_AT_REF_SOURCE="$name" RUN_AT_REF_RUNS="$TMP/runs" \
      bash "$ROOT_DIR/scripts/land/run-at-ref.sh" HEAD true 2>&1)"; then
    echo "repository_identity: runner accepted remote $name" >&2; exit 1
  fi
  [[ "$out" == *"only origin is permitted"* ]] || { echo "$out" >&2; exit 1; }
  [[ ! -e "$TMP/runs" ]] || { echo 'runner touched run state before refusal' >&2; exit 1; }
  if CONCRETE_SKIP_GATES=1 bash "$ROOT_DIR/.githooks/pre-push" "$name" "$TMP/local.git" </dev/null 2>/dev/null; then
    echo "repository_identity: hook accepted remote $name" >&2; exit 1
  fi
done
for url in "$TMP/local.git" https://github.com/lambdaclass/concrete.git ssh://git@github.com:22/lambdaclass/concrete.git git@github-alias:lambdaclass/concrete.git; do
  git remote set-url origin "$url"
  CONCRETE_SKIP_GATES=1 bash "$ROOT_DIR/.githooks/pre-push" origin "$url" </dev/null >/dev/null 2>&1
done
# Both supported local transports and origin run the exact expected commit.
git remote set-url origin "$TMP/repo"
expected="$(git rev-parse HEAD)"
cd "$TMP"
for source in origin "$TMP/repo" "$TMP/input.bundle" input.bundle; do
  out="$(RUN_AT_REF_REPO="$TMP/repo" RUN_AT_REF_SOURCE="$source" RUN_AT_REF_RUNS="$TMP/runs" RUN_AT_REF_NO_NIX=1 \
    bash "$ROOT_DIR/scripts/land/run-at-ref.sh" HEAD 'git rev-parse HEAD')"
  [[ "$out" == *"sha=$expected"* && "$out" == *"exit=0"* ]] || { echo "$out" >&2; exit 1; }
done
echo 'repository_identity: hook, runner, fetch, push, content and remote-name controls pass'
