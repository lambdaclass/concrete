#!/usr/bin/env bash
# Markdown-link gate: every local link in the repository's Markdown names a path that exists and,
# into a Markdown file, a heading or explicit id that exists (scripts/tests/lib/doc_links.py holds
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
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
CHECK="scripts/tests/lib/doc_links.py"
[ -f "$CHECK" ] || { echo "FATAL: $CHECK missing" >&2; exit 2; }

PASS=0; FAIL=0; INCOMPLETE=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
inc(){ echo "  INCOMPLETE $1"; INCOMPLETE=$((INCOMPLETE+1)); }
TMP="$(mktemp -d)" || { echo "FATAL: mktemp failed" >&2; exit 2; }
trap 'rm -rf "$TMP"' EXIT
BOUNDED_PARENT="$TMP"
source "$ROOT_DIR/scripts/tests/lib/bounded.sh"

# tree NAME: a fresh control tree with a target document carrying the anchors the controls use.
tree() {
  rm -rf "${TMP:?}/$1"; mkdir -p "$TMP/$1/docs" "$TMP/$1/changelog.d"
  cat > "$TMP/$1/docs/TARGET.md" <<'EOF'
# Target

## Phase 9: the `ledger` & its *views*

## Repeated

## Repeated

<a id="REF-3"></a>

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
`[inside a span](MISSING.md)`
EOF
printf '### Entry\n\nSee [the target](docs/TARGET.md#repeated) and [readme](README.md).\n' \
  > "$TMP/good/changelog.d/2099-01-01-control.md"
expect good 0 'DOC-LINKS: PASS' "positive control: slugs, duplicate suffix, explicit id, code skipped, root-relative fragment links pass"

tree anchor
printf '[gone](TARGET.md#phase-8-renamed)\n' > "$TMP/anchor/docs/LINKS.md"
expect anchor 1 'no anchor #phase-8-renamed in docs/TARGET.md' "a link to a missing heading fails"

tree dup
printf '[third](TARGET.md#repeated-2)\n' > "$TMP/dup/docs/LINKS.md"
expect dup 1 'no anchor #repeated-2' "a duplicate suffix past the last repeated heading fails"

tree fenced
printf '[fenced](TARGET.md#not-a-heading)\n' > "$TMP/fenced/docs/LINKS.md"
expect fenced 1 'no anchor #not-a-heading' "a heading inside a code fence is not an anchor"

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
