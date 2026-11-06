#!/usr/bin/env bash
# The website shows the repository's documents, never copies of them (decision B18).
#
# The specification pages (scripts/spec/spec.py site) and the reference pages
# (scripts/gen/site_reference.py) are generated at build time from docs/, so they cannot be stale.
# The reference pages used to be tracked hand copies, and every one had drifted from its source.
# This gate holds the arrangement:
#   A. no page of a generated section is tracked (a tracked copy is a second owner of the text);
#   B. the deploy workflow and `make docs-site` / `docs-serve` run both generators before zola;
#   C. the generators run and produce every page, and every `@/spec/` and `@/reference/` link in
#      the tracked site content and templates names a page that exists after generation;
#   D. zola builds the generated site (when zola is on PATH; check_spec S10 requires it where it is).
# Each check has a negative control on a copy.

set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
GENERATED_DIRS="site/content/spec site/content/reference"

tracked_copies() {  # $1 = file listing (one path per line); prints tracked pages of generated sections
  local d; for d in $GENERATED_DIRS; do grep -E "^$d/" "$1" || true; done
}
runs_generators() {  # $1 = file; 0 when both generators run before the zola build in it
  python3 -I - "$1" <<'PY'
import re, sys
# comment lines (YAML / shell / make) do not run, so they are dropped before looking
s = "\n".join(l for l in open(sys.argv[1], encoding="utf-8").read().split("\n") if not l.lstrip().startswith("#"))
ok = True
for m in re.finditer(r"zola[^\n]*--root site (build|serve)", s):
    before = s[:m.start()]
    seg = before[max(before.rfind("\n\n"), before.rfind("- name:"), 0):]
    ok &= "spec/spec.py site --out site/content/spec" in seg and "site_reference.py --out site/content/reference" in seg
sys.exit(0 if ok and re.search(r"zola[^\n]*--root site", s) else 1)
PY
}
dangling_links() {  # $1 = site dir with generated pages; prints links to pages that do not exist
  python3 -I - "$1" <<'PY'
import glob, os, re, sys
d = sys.argv[1]
for p in sorted(glob.glob(d + "/content/**/*.md", recursive=True) + glob.glob(d + "/templates/**/*.html", recursive=True)):
    for m in re.finditer(r"@/((?:spec|reference)/[A-Za-z0-9_.-]+\.md)", open(p, encoding="utf-8").read()):
        if not os.path.exists(os.path.join(d, "content", m.group(1))): print(f"{p}: @/{m.group(1)}")
PY
}

echo "=== A. generated sections are not tracked ==="
git ls-files > "$TMP/ls.txt"
c="$(tracked_copies "$TMP/ls.txt")"
[ -z "$c" ] && ok "no tracked page under $GENERATED_DIRS" || { no "tracked copies of generated pages:"; echo "$c" | sed 's/^/      /'; }
printf 'site/content/reference/SAFETY.md\n' > "$TMP/ls2.txt"
[ -n "$(tracked_copies "$TMP/ls2.txt")" ] && ok "control A: a tracked reference copy is detected" || no "control A: a tracked reference copy was not detected"

echo "=== B. every site build generates first ==="
for f in .github/workflows/book.yml Makefile; do
  runs_generators "$f" && ok "$f runs both generators before zola" || no "$f builds the site without generating its pages"
done
sed 's|python3 scripts/gen/site_reference.py --out site/content/reference||' .github/workflows/book.yml > "$TMP/book.yml"
runs_generators "$TMP/book.yml" && no "control B1: a build step without the reference generator passed" || ok "control B1: a build step without the reference generator is detected"
sed 's|python3 scripts/gen/site_reference.py --out site/content/reference|# python3 scripts/gen/site_reference.py --out site/content/reference|' .github/workflows/book.yml > "$TMP/book2.yml"
runs_generators "$TMP/book2.yml" && no "control B2: a commented-out generator passed" || ok "control B2: a commented-out generator is detected"

echo "=== C. the generators produce every page, and every site link resolves ==="
cp -R site "$TMP/site"; rm -rf "$TMP/site/content/spec" "$TMP/site/content/reference"
if python3 scripts/spec/spec.py site --out "$TMP/site/content/spec" > /dev/null \
   && python3 scripts/gen/site_reference.py --out "$TMP/site/content/reference" > /dev/null; then
  ok "both generators run"
  n="$(python3 scripts/gen/site_reference.py --list | wc -l | tr -d ' ')"
  m="$(grep -l 'GENERATED from docs/.* by scripts/gen/site_reference.py' "$TMP/site/content/reference/"*.md | wc -l | tr -d ' ')"
  [ "$n" = "$m" ] && [ "$n" -gt 0 ] && ok "$n reference pages, each generated from its document" || no "reference pages: $m generated of $n listed"
  [ -f "$TMP/site/content/spec/rules.md" ] && ok "the rule index page is generated" || no "no rule index page"
  dl="$(dangling_links "$TMP/site")"
  [ -z "$dl" ] && ok "every @/spec/ and @/reference/ link resolves" || { no "dangling site links:"; echo "$dl" | sed 's/^/      /'; }
  printf '\n[x](@/reference/NO_SUCH_PAGE.md)\n' >> "$TMP/site/content/guide/intro.md"
  [ -n "$(dangling_links "$TMP/site")" ] && ok "control C: a link to a page that does not exist is detected" || no "control C: a dangling link was not detected"
  sed -i.bak '$d' "$TMP/site/content/guide/intro.md"; sed -i.bak '$d' "$TMP/site/content/guide/intro.md"; rm -f "$TMP/site/content/guide/intro.md.bak"
else
  no "a generator failed"
fi

echo "=== D. zola builds the generated site ==="
# Where zola is installed (the devshell, CI's spec job) the build is required by check_spec S10,
# which generates both sections too; this gate runs in jobs without zola and then skips it.
if command -v zola > /dev/null 2>&1; then
  if zola --root "$TMP/site" build --output-dir "$TMP/public" > "$TMP/zola.out" 2>&1; then ok "zola builds the site"
  else no "zola build failed: $(tail -3 "$TMP/zola.out" | tr '\n' ' ')"; fi
else echo "  skip zola build (zola not on PATH; check_spec S10 builds the site where it is)"; fi

echo ""
echo "SITE-GENERATED: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
