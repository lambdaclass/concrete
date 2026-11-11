#!/usr/bin/env bash
# Exercise bundle transport and script self-location without network or reviewers.
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh"
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
source "$ROOT_DIR/scripts/checks/lib/git_sandbox.sh"
git_sandbox
# Source classification is offline, including the one supported network URL.
python3 -I - "$ROOT_DIR" <<'PYSOURCE'
import importlib.util
import pathlib
import shlex
import sys
root = pathlib.Path(sys.argv[1])
spec = importlib.util.spec_from_file_location('bundle_source', root / 'scripts/land/bundle_source.py')
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)
url = 'https://github.com/lambdaclass/concrete.git'
assert module.fetch_source(url, '.') == url
# The migration must preserve each maintained overlay entry, including transport helpers.
text = (root / 'scripts/land/legacy_move.sh').read_text()
overlay = set(shlex.split(text.split('OVERLAY=(', 1)[1].split(')\n', 1)[0], comments=True))
required = {'scripts/land/run-at-ref.sh', 'scripts/land/bundle_source.py',
            'scripts/checks/landing_sources.sh', 'scripts/checks/archived_references.sh',
            'scripts/checks/lib/archived_references.py'}
assert required <= overlay
assert all((root / path).is_file() for path in overlay), 'missing migration overlay file'
PYSOURCE
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
# Disposable fixture: no installed hooks and no network.
git init -q "$TMP/repo"
git -C "$TMP/repo" config core.hooksPath "$TMP/nohooks"
git -C "$TMP/repo" remote add origin "$TMP/local.git"
# A real bundle exercises the runner without network access.
git -C "$TMP/repo" -c user.name=Test -c user.email=test@example.com commit --allow-empty -qm fixture
git -C "$TMP/repo" bundle create "$TMP/input.bundle" HEAD
BOUNDED_PARENT="$TMP" source "$ROOT_DIR/scripts/checks/lib/bounded.sh"
cd "$TMP/repo"
# A nested caller must still reach ready.sh's dirty-tree stop before any fetch.
mkdir caller
relative_tools="$(python3 -I -c 'import os,sys; print(os.path.relpath(sys.argv[1], sys.argv[2]))' "$ROOT_DIR/scripts" "$TMP/repo/caller")"
printf dirty > tracked
git add tracked
if out="$(cd caller && bounded_run 15 -- bash "$relative_tools/land/ready.sh" wip/test 2>&1)"; then
  echo 'landing_sources: ready accepted dirty fixture' >&2; exit 1
fi
[[ "$out" == *"the work tree has uncommitted changes"* ]] || { echo "$out" >&2; exit 1; }
git reset -q -- tracked
rm tracked
expected="$(git rev-parse HEAD)"
cd "$TMP"
mkdir 'caller@host:dir'
cp input.bundle 'caller@host:dir/input.bundle'
for source in "$TMP/input.bundle" input.bundle 'caller@host:dir/input.bundle'; do
  out="$(RUN_AT_REF_REPO=repo RUN_AT_REF_SOURCE="$source" RUN_AT_REF_RUNS=relative-runs RUN_AT_REF_NO_NIX=1 \
    bounded_run 15 -- bash "$ROOT_DIR/scripts/land/run-at-ref.sh" HEAD 'git rev-parse HEAD')"
  [[ "$out" == *"sha=$expected"* && "$out" == *"exit=0"* ]] || { echo "$out" >&2; exit 1; }
done
[[ -z "$(git -C repo for-each-ref refs/run-at-ref/)" ]]
[[ -z "$(find relative-runs -mindepth 1 -type d -print)" ]]
ln -s input.bundle linked.bundle
ln -s "$TMP" linked-dir
printf invalid > invalid.bundle
# Each refusal checks its diagnostic, and must precede run-state creation.
for spec in 'repo|local clones and remote names are refused' \
    "$TMP/repo|local clones and remote names are refused" \
    'https://example.com/archive.git|local clones and remote names are refused' \
    'linked.bundle|symlink fetch source refused' 'linked-dir/input.bundle|symlink fetch source refused' \
    'invalid.bundle|invalid git bundle' 'origin|local clones and remote names are refused' \
    'https://example.com/other.git|local clones and remote names are refused'; do
  source="${spec%%|*}"; reason="${spec#*|}"
  if out="$(RUN_AT_REF_REPO=repo RUN_AT_REF_SOURCE="$source" RUN_AT_REF_RUNS=refused-runs RUN_AT_REF_NO_NIX=1 \
      bounded_run 15 -- bash "$ROOT_DIR/scripts/land/run-at-ref.sh" HEAD true 2>&1)"; then
    echo "landing_sources: accepted $source" >&2; exit 1
  fi
  [[ "$out" == *"$reason"* && ! -e refused-runs ]] || { echo "$out" >&2; exit 1; }
done
if out="$(RUN_AT_REF_REPO=repo RUN_AT_REF_SOURCE=input.bundle RUN_AT_REF_RUNS=refused-runs \
    bounded_run 15 -- bash "$ROOT_DIR/scripts/land/run-at-ref.sh" refs/heads/missing true 2>&1)"; then exit 1; fi
[[ "$out" == *"requested ref is not advertised"* && ! -e refused-runs ]]
# These statements run after their entry points change directory. Exercise the
# actual source statements in isolation: no reviewer, proof build or note writes.
for spec in 'land/train.sh:usage()' 'review/review.sh:usage()' 'proofs/external.sh:DIRECT='; do
  file="${spec%%:*}"; prefix="${spec#*:}"
  statement="$(python3 -I -c 'import pathlib,sys; print(next(line for line in pathlib.Path(sys.argv[1]).read_text().splitlines() if line.startswith(sys.argv[2])))' "$ROOT_DIR/scripts/$file" "$prefix")"
  probe='SELF_DIR="$(cd "$(dirname "$0")" && pwd)"; SCRIPT_DIR="$SELF_DIR"; cd "$1"; '
  case "$prefix" in
    'usage()') probe+="$statement"$'\nusage'; expected=2
      [ "$file" != review/review.sh ] || expected=0 ;;
    *) probe+="$statement; [ -z \"\$DIRECT\" ]"; expected=0 ;;
  esac
  rc=0
  out="$(cd "$ROOT_DIR/scripts" && bounded_run 15 -- bash -c "$probe" "$file" "$ROOT_DIR" 2>&1)" || rc=$?
  [ "$rc" = "$expected" ] && [[ "$out" != *'No such file'* ]] || { echo "$file: $out" >&2; exit 1; }
  if [ "$prefix" = 'usage()' ]; then
    [[ "$out" == *"${file##*/}"* ]] || { echo "$file: empty help" >&2; exit 1; }
  fi
done
bounded_gate_status
echo "landing_sources: bundle and self-location controls pass"
