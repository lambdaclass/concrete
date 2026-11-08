#!/usr/bin/env bash
# Markdown-link gate: every local link in the repository's Markdown names a path that exists and,
# into a Markdown file, a heading or explicit id that exists (scripts/checks/lib/doc_links.py holds
# the rules: GitHub slugs, duplicate-heading suffixes, root-relative changelog.d/ fragments).
#
# WHY A GATE AND NOT A MAKE TARGET. The checker used to be a Makefile target only, reached by
# nothing in CI or the pre-push selection, and it checked the path of `file.md#anchor` and dropped
# the anchor. Links to headings renamed or moved to another file passed, and it resolved changelog.d/
# fragments against changelog.d/ although they are folded into the root CHANGELOG.md, so a correct
# fragment link read as broken. Nobody saw either, because nothing ran it (bug 252).
#
# The controls below run the same checker on throwaway trees, so a checker that accepts everything,
# skips anchors, or resolves fragments against the wrong directory fails here before the real run.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
CHECK="scripts/checks/lib/doc_links.py"
[ -f "$CHECK" ] || { echo "FATAL: $CHECK missing" >&2; exit 2; }

PASS=0; FAIL=0; INCOMPLETE=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
inc(){ echo "  INCOMPLETE $1"; INCOMPLETE=$((INCOMPLETE+1)); }
TMP="$(mktemp -d)" || { echo "FATAL: mktemp failed" >&2; exit 2; }
trap 'rm -rf "$TMP"' EXIT
BOUNDED_PARENT="$TMP"
source "$ROOT_DIR/scripts/checks/lib/bounded.sh"

# tree NAME: a fresh control tree with a target document carrying the anchors the controls use.
tree() {
  rm -rf "${TMP:?}/$1"; mkdir -p "$TMP/$1/docs" "$TMP/$1/changelog.d"
  cat > "$TMP/$1/docs/TARGET.md" <<'EOF'
# Target

## Phase 9: the `ledger` & its *views*

## Repeated

## Repeated

## A literal _ and __init__ and **bold**

<a id="REF-3"></a>

`<a id="in-code"></a>`

<!--
## Commented out
-->

```
## Not a heading
```
EOF
  printf '# Readme\n' > "$TMP/$1/README.md"
}
# expect NAME RC PATTERN DESCRIPTION: the checker on tree NAME exits RC and prints PATTERN.
expect() {
  local out rc
  out="$(bounded_run -i /dev/null 30 -- python3 -I "$CHECK" --root "$TMP/$1" 2>&1)"; rc=$?
  if bounded_is_incomplete "$rc"; then inc "$4: the checker timed out twice"; return; fi
  if [ "$rc" -eq "$2" ] && grep -qE -- "$3" <<<"$out"; then ok "$4"
  else no "$4 (exit $rc, wanted $2 and /$3/): $(awk 'NR<=3' <<<"$out" | tr '\n' ' ')"; fi
}

echo "=== controls: the checker on throwaway trees ==="
tree good
cat > "$TMP/good/docs/LINKS.md" <<'EOF'
[slug](TARGET.md#phase-9-the-ledger--its-views) [dup](TARGET.md#repeated-1) [id](TARGET.md#REF-3)
[case](TARGET.md#Repeated) [self](#local) [up](../README.md) [root](/README.md)
## Local
```
[inside a fence](MISSING.md)
```
`[inside a span](MISSING.md)` and `a span that
[crosses a line](MISSING.md)`
[literal](TARGET.md#a-literal-_-and-init-and-bold)
EOF
printf '### Entry\n\nSee [the target](docs/TARGET.md#repeated), [readme](README.md) and [the next](#second-entry).\n' \
  > "$TMP/good/changelog.d/2099-01-01-control.md"
printf '### Second Entry\n\nBody.\n' > "$TMP/good/changelog.d/2099-01-02-other.md"
printf '# Changelog\n\n## Major Milestones\n\n### Old Entry\n' > "$TMP/good/CHANGELOG.md"
expect good 0 'DOC-LINKS: PASS' "positive control: slugs (literal underscore, emphasis), duplicate suffix, explicit id, code skipped, root-relative fragment links and a sibling fragment's heading pass"

tree anchor
printf '[gone](TARGET.md#phase-8-renamed)\n' > "$TMP/anchor/docs/LINKS.md"
expect anchor 1 'no anchor #phase-8-renamed in docs/TARGET.md' "a link to a missing heading fails"

tree dup
printf '[third](TARGET.md#repeated-2)\n' > "$TMP/dup/docs/LINKS.md"
expect dup 1 'no anchor #repeated-2' "a duplicate suffix past the last repeated heading fails"

tree fenced
printf '[fenced](TARGET.md#not-a-heading)\n' > "$TMP/fenced/docs/LINKS.md"
expect fenced 1 'no anchor #not-a-heading' "a heading inside a code fence is not an anchor"

tree commented
printf '[ghost](TARGET.md#commented-out)\n' > "$TMP/commented/docs/LINKS.md"
expect commented 1 'no anchor #commented-out' "a heading inside an HTML comment is not an anchor"

tree codeid
printf '[ghost](TARGET.md#in-code)\n' > "$TMP/codeid/docs/LINKS.md"
expect codeid 1 'no anchor #in-code' "an id written inside a code span is not an anchor"

tree badge
printf '[![badge](NOPE.svg)](TARGET.md)\n' > "$TMP/badge/docs/LINKS.md"
expect badge 1 'missing local link NOPE.svg' "an image inside a link's text is checked"

tree missing
printf '[gone](NOPE.md)\n' > "$TMP/missing/docs/LINKS.md"
expect missing 1 'missing local link NOPE.md' "a link to a missing file fails"

tree outside
printf '[rel](docs/TARGET.md)\n' > "$TMP/outside/docs/LINKS.md"
expect outside 1 'missing local link docs/TARGET.md' "outside changelog.d/ a link resolves against its own directory"

tree fragrel
printf '### Entry\n\n[as if nested](../docs/TARGET.md)\n' > "$TMP/fragrel/changelog.d/2099-01-01-control.md"
expect fragrel 1 'missing local link ../docs/TARGET.md' "a changelog.d/ fragment resolves against the root, not changelog.d/"

tree fraganchor
printf '### Entry\n\n[renamed](docs/TARGET.md#gone)\n' > "$TMP/fraganchor/changelog.d/2099-01-01-control.md"
expect fraganchor 1 'no anchor #gone in docs/TARGET.md' "a fragment's anchor is checked too"

echo "=== controls: swept files and discovery failures ==="
# These controls need a real index and real legacy trees. Keep every repository variable from the
# caller out of the throwaway repository, including when this gate runs under a linked-worktree hook.
source "$ROOT_DIR/scripts/checks/lib/git_sandbox.sh" || exit 2
: > "$TMP/gitconfig"
git_sandbox "$TMP/gitconfig" || exit 2
LOCK_REL="$(python3 -I -c 'import sys; sys.path.insert(0, sys.argv[1]); import legacy_lock; print(legacy_lock.LOCK_PATH)' \
  "$ROOT_DIR/scripts/checks/lib")" || exit 2
tree repo
mkdir -p "$TMP/repo/legacy"
printf '# Frozen\n' > "$TMP/repo/legacy/FROZEN.md"
git init -q "$TMP/repo" && git -C "$TMP/repo" add -A \
  && git -C "$TMP/repo" -c user.name=t -c user.email=t@t commit -qm frozen \
  || { echo "FATAL: swept-file control repository" >&2; exit 2; }
BASE="$(git -C "$TMP/repo" rev-parse HEAD)" || exit 2
FROZEN="$(git -C "$TMP/repo" rev-parse HEAD:legacy)" || exit 2
mkdir -p "$TMP/repo/$(dirname "$LOCK_REL")"
printf 'base %s\ntree %s\n' "$BASE" "$FROZEN" > "$TMP/repo/$LOCK_REL"
printf '[target](/docs/TARGET.md#target)\n' > "$TMP/repo/legacy/SWEPT.md"
git -C "$TMP/repo" add -A \
  && git -C "$TMP/repo" -c user.name=t -c user.email=t@t commit -qm sweep \
  || { echo "FATAL: swept-file control commit" >&2; exit 2; }
expect repo 0 'DOC-LINKS: PASS CHECKED=1 ANCHORS=1' "a swept document with a valid link passes"
printf '[gone](/docs/MISSING.md)\n' > "$TMP/repo/legacy/SWEPT.md"
expect repo 1 'legacy/SWEPT[.]md:1: missing local link /docs/MISSING[.]md' "a swept document with a broken link fails"
printf '[target](/docs/TARGET.md#target)\n' > "$TMP/repo/legacy/SWEPT.md"

tree missing_tree
git init -q "$TMP/missing_tree" || { echo "FATAL: missing-tree control repository" >&2; exit 2; }
mkdir -p "$TMP/missing_tree/$(dirname "$LOCK_REL")"
printf 'base %040d\n' 0 > "$TMP/missing_tree/$LOCK_REL"
expect missing_tree 1 'legacy[.]lock:2: missing: line 2 must be .tree <tree>.' "a lock without its tree row is malformed and fails"

# Selectively fail one discovery step while every other Git command runs normally. The exact
# diagnostic must name that step; neither an empty swept set nor an rglob fallback is acceptable.
mkdir "$TMP/bin"
REAL_GIT="$(command -v git)"
export REAL_GIT DOC_LINKS_CONTROL_TMP="$TMP"
cat > "$TMP/bin/git" <<'STUB'
#!/usr/bin/env bash
cmd="$1"
[ "$cmd" != -C ] || cmd="$3"
spec="${!#}"
if [ "$cmd" = ls-tree ] && [ -e "$DOC_LINKS_CONTROL_TMP/fail-frozen" ] && [ "$spec" != HEAD:legacy ]; then
  exit 2
fi
if [ "$cmd" = ls-tree ] && [ -e "$DOC_LINKS_CONTROL_TMP/fail-head" ] && [ "$spec" = HEAD:legacy ]; then
  exit 2
fi
if [ "$cmd" = ls-files ] && [ -e "$DOC_LINKS_CONTROL_TMP/fail-ls-files" ]; then
  exit 2
fi
exec "$REAL_GIT" "$@"
STUB
chmod +x "$TMP/bin/git"
touch "$TMP/fail-frozen"
PATH="$TMP/bin:$PATH" expect repo 1 '^FAIL frozen legacy tree lookup failed: git ls-tree exited 2$' "a failing frozen-tree lookup fails with its exact diagnostic"
rm "$TMP/fail-frozen"
touch "$TMP/fail-head"
PATH="$TMP/bin:$PATH" expect repo 1 '^FAIL HEAD:legacy lookup failed: git ls-tree exited 2$' "a failing HEAD:legacy lookup fails with its exact diagnostic"
rm "$TMP/fail-head"
touch "$TMP/fail-ls-files"
PATH="$TMP/bin:$PATH" expect repo 1 '^FAIL Markdown file listing failed: git ls-files exited 2$' "a failing Markdown file listing fails instead of falling back to rglob"
rm "$TMP/fail-ls-files"

echo "=== the repository ==="
out="$(bounded_run -i /dev/null 60 -- python3 -I "$CHECK" 2>&1)"; rc=$?
if bounded_is_incomplete "$rc"; then inc "the repository run timed out twice"
elif [ "$rc" -eq 0 ]; then ok "$(grep -m1 'DOC-LINKS' <<<"$out")"
else
  grep '^FAIL' <<<"$out"
  no "$(grep -m1 'DOC-LINKS' <<<"$out" || echo "the checker exited $rc")"
fi

bounded_summary
echo ""
echo "DOC-LINKS-GATE: PASS=$PASS FAIL=$FAIL INCOMPLETE=$INCOMPLETE"
[ "$FAIL" -eq 0 ] && [ "$INCOMPLETE" -eq 0 ]
