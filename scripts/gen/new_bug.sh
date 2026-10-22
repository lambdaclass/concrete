#!/usr/bin/env bash
# File a new bug: allocate its number race-safely, write the record, the reproducer skeleton and the
# changelog fragment, regenerate the ledger views, and print what the author still has to fill in.
#
#   scripts/gen/new_bug.sh "<title>" [--class N] [--silent yes|no]
#   scripts/gen/new_bug.sh --number-only "<title>"     allocate and reserve a number, write nothing
#
# NUMBER. The next number is one above the highest bug number seen anywhere another writer could
# have used one:
#   * every branch of the shared remote (main, wip/*, ci/* and the rest) and every local branch,
#     after a fetch: the docs/bugs/NNN_*.md names in each tree;
#   * every worktree of this repository, on disk (untracked records count);
#   * every reservation on the remote: refs/bug-reservations/NNN.
# Gaps are never reused (a skipped number may have been retired or be in flight somewhere unseen).
#
# RESERVATION (the race-safe step). Seeing a free number is not owning it: two agents can see the
# same maximum. The number is owned once `refs/bug-reservations/NNN` is CREATED on the remote, with
# `--force-with-lease=refs/bug-reservations/NNN:` (expected value: absent). The server creates a ref
# atomically, so of two concurrent pushes for one number exactly one succeeds; the other is rejected
# and moves to the next number. Each reservation is a parentless, empty-tree commit whose message
# names the title, the user, the host and the time, so `git ls-remote origin 'refs/bug-reservations/*'`
# lists who holds what. A reservation publishes no code, and the pre-push hook accepts exactly that
# shape on that namespace (its reservation tier) and nothing else there.
#
# FILES (the tree must be migrated to the ledger: scripts/gen/migrate_ledgers.sh):
#   docs/bugs/NNN_<slug>.md                      the record, with its bug-ledger block
#   tests/regressions/bugNNN_<slug>/repro.con    the reproducer skeleton
#   changelog.d/<date>-bugNNN-<slug>.md          the changelog fragment
# then `scripts/gen/bug_ledger.py --write` (and bug_dashboard.py when present) regenerates the views.
# Every placeholder carries the token FILL-IN(new_bug.sh); check_bug_filing_complete.sh refuses a
# record, reproducer or fragment that still holds one, so a draft can be backed up on wip/* but
# cannot land.
#
# ENVIRONMENT: CONCRETE_BUG_REMOTE (default origin; `fork` is refused), NEW_BUG_NO_FETCH=1 (skip the
# fetch; tests), NEW_BUG_DATE (YYYY-MM-DD; tests). Exit 0 filed, 1 refused, 2 usage.
set -euo pipefail

die() { echo "new_bug: $*" >&2; exit 1; }
usage() { sed -n '2,7p' "$0" >&2; exit 2; }

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT"
REMOTE="${CONCRETE_BUG_REMOTE:-origin}"
TODAY="${NEW_BUG_DATE:-$(date +%Y-%m-%d)}"
TOKEN="FILL-IN(new_bug.sh)"

TITLE=""; CLASS=""; SILENT=""; NUMBER_ONLY=0
while [ $# -gt 0 ]; do
  case "$1" in
    --class) [ $# -ge 2 ] || usage; CLASS="$2"; shift 2 ;;
    --silent) [ $# -ge 2 ] || usage; SILENT="$2"; shift 2 ;;
    --number-only) NUMBER_ONLY=1; shift ;;
    -h|--help) usage ;;
    -*) echo "new_bug: unknown option $1" >&2; usage ;;
    *) [ -z "$TITLE" ] || usage; TITLE="$1"; shift ;;
  esac
done
[ -n "$TITLE" ] || usage
case "$REMOTE" in fork) die "the remote 'fork' is divergent and never used for coordination" ;; esac
git remote get-url "$REMOTE" >/dev/null 2>&1 || die "no remote '$REMOTE' (set CONCRETE_BUG_REMOTE)"
[[ "$TODAY" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}$ ]] || die "NEW_BUG_DATE must be YYYY-MM-DD"
[ -z "$CLASS" ] || [[ "$CLASS" =~ ^[1-9][0-9]*$ ]] || die "--class takes a class number from docs/bugs/STRUCTURAL_CLASSES.md"
case "$SILENT" in ""|yes|no) ;; *) die "--silent takes yes or no" ;; esac

no_op_in_progress() {
  local p
  for p in MERGE_HEAD CHERRY_PICK_HEAD REVERT_HEAD rebase-merge rebase-apply; do
    [ ! -e "$(git rev-parse --git-path "$p")" ] || die "a git operation is in progress ($p); finish or abort it first"
  done
}
no_op_in_progress

# ----------------------------------------------------------------------------- validate up front
# Everything that could refuse runs before a number is reserved, so a refusal never burns one.
if [ "$NUMBER_ONLY" = 0 ]; then
  [ -n "$(python3 -I scripts/gen/bug_ledger.py corpus)" ] \
    || die "this tree is not migrated to the bug ledger (scripts/gen/migrate_ledgers.sh), so there is no ledger block to write; file by hand (docs/bugs/README.md, Filing a bug) or use --number-only"
  python3 -I scripts/gen/bug_ledger.py --check >/dev/null \
    || die "the ledger views are stale before the new bug is added; run python3 scripts/gen/bug_ledger.py --write first"
  python3 -I scripts/gen/changelog.py --check >/dev/null \
    || die "changelog.d/ or CHANGELOG.md is already malformed (above); repair it first"
fi
python3 -I - "$ROOT" "$TITLE" "$CLASS" <<'PY' || exit 1
import os, sys
root, title, cls = sys.argv[1], sys.argv[2], sys.argv[3]
sys.path.insert(0, os.path.join(root, "scripts", "gen"))
import bug_ledger as b
def refuse(m):
    print(f"new_bug: {m}", file=sys.stderr); sys.exit(1)
if "\n" in title or "\t" in title or "-->" in title or not title.strip():
    refuse("the title is one line with no tab and no '-->'")
if "changelog stamp:" in title:
    refuse("the title holds 'changelog stamp:', which a changelog fragment may not carry")
m = b.STATUS_UPPER.search(title) or b.STATUS_LEAD.match(title)
if m:
    refuse(f"the title states a status ('{m.group(0)}'); the record's **Status:** line is the only status a bug has")
if cls:
    p = os.path.join(root, "docs", "bugs", "STRUCTURAL_CLASSES.md")
    _, blk = b.splice(b.read(p), b.CLASS_BEGIN, b.CLASS_END, "", p)
    if cls not in [n for n, _, _ in b.class_rows(blk, p)]:
        refuse(f"class {cls} is not a row of STRUCTURAL_CLASSES.md's summary table (join a class there, or add one first)")
PY
SLUG="$(printf '%s' "$TITLE" | tr '[:upper:]' '[:lower:]' | sed -E 's/[^a-z0-9]+/_/g; s/^_+//; s/_+$//' | cut -c1-48 | sed -E 's/_+$//')"
[ -n "$SLUG" ] || die "the title has no letters or digits to make a file name from"

# ----------------------------------------------------------------------------- allocate
if [ "${NEW_BUG_NO_FETCH:-0}" != 1 ]; then
  # Every branch head, whatever the configured refspec (a single-branch clone fetches only one).
  git fetch --quiet --prune "$REMOTE" "+refs/heads/*:refs/remotes/$REMOTE/*" \
    || die "git fetch $REMOTE failed; the allocation needs the remote's branches"
  no_op_in_progress
fi
seen="$(mktemp)"; trap 'rm -f "$seen"' EXIT
record_nums() { sed -nE 's#^(.*/)?([0-9]{3})_[^/]*\.md$#\2#p'; }
while IFS= read -r ref; do
  git ls-tree --name-only "$ref" docs/bugs/ 2>/dev/null | record_nums >> "$seen" || true
done < <(git for-each-ref --format='%(refname)' "refs/remotes/$REMOTE" refs/heads)
while IFS= read -r wt; do
  [ -d "$wt/docs/bugs" ] || continue
  find "$wt/docs/bugs" -maxdepth 1 -name '[0-9][0-9][0-9]_*.md' | record_nums >> "$seen"
done < <(git worktree list --porcelain | sed -n 's/^worktree //p')
reservations="$(git ls-remote "$REMOTE" 'refs/bug-reservations/*')" || die "git ls-remote $REMOTE failed; cannot read the reservations"
printf '%s\n' "$reservations" | sed -nE 's#.*refs/bug-reservations/([0-9]+)$#\1#p' >> "$seen"
max="$(awk '/^[0-9]+$/ { n = $0 + 0; if (n > m) m = n } END { print m + 0 }' "$seen")"
n=$((max + 1))

# A reservation push is gated by the pre-push hook like any push. A hook without the reservation
# tier would treat it as an unknown ref and run its fallback gates on whatever is checked out, so
# such a hook is refused here rather than bypassed.
hooks="$(git config core.hooksPath || true)"
if [ -n "$hooks" ]; then case "$hooks" in /*) ;; *) hooks="$(git rev-parse --show-toplevel)/$hooks" ;; esac
else hooks="$(git rev-parse --git-path hooks)"; fi
if [ -x "$hooks/pre-push" ] && ! grep -q 'refs/bug-reservations/' "$hooks/pre-push"; then
  die "the active pre-push hook ($hooks/pre-push) has no reservation tier, so it would run gates for a reservation push; refresh it from main (git show origin/main:.githooks/pre-push > '$hooks/pre-push')"
fi

empty_tree="$(git hash-object -t tree /dev/null)"
who="$(git config user.email || echo unknown)@$(hostname -s 2>/dev/null || echo host)"
tries=0
while :; do
  tries=$((tries + 1)); [ "$tries" -le 50 ] || die "could not reserve a number after 50 attempts"
  num="$(printf '%03d' "$n")"
  [ "${#num}" -eq 3 ] || die "bug numbers past 999 need a wider record name (docs/bugs/NNN_*.md)"
  # The nonce makes every attempt a distinct commit: two identical requests in one second would
  # otherwise build one SHA, and pushing a SHA the remote already holds "succeeds" for both.
  nonce="$(od -An -N8 -tx1 /dev/urandom | tr -d ' \n')"
  c="$(git commit-tree "$empty_tree" -m "bug $num reserved: $TITLE" -m "by $who at $(date -u +%Y-%m-%dT%H:%M:%SZ), nonce $nonce")"
  if git push --quiet --force-with-lease="refs/bug-reservations/$num:" "$REMOTE" "$c:refs/bug-reservations/$num" 2>/dev/null; then
    break
  fi
  # Rejected: taken by a concurrent reservation (try the next number), or the push itself failed.
  git ls-remote --exit-code "$REMOTE" "refs/bug-reservations/$num" >/dev/null 2>&1 \
    || die "the reservation push for bug $num failed and the ref does not exist; check access to '$REMOTE'"
  echo "new_bug: bug $num was reserved concurrently; trying the next number" >&2
  n=$((n + 1))
done
no_op_in_progress
echo "new_bug: reserved bug $num on $REMOTE (refs/bug-reservations/$num)"
if [ "$NUMBER_ONLY" = 1 ]; then
  echo "$num"
  exit 0
fi

# ----------------------------------------------------------------------------- write
REC="docs/bugs/${num}_${SLUG}.md"
REPRO_DIR="tests/regressions/bug${num}_${SLUG}"
REPRO="$REPRO_DIR/repro.con"
FRAG="changelog.d/${TODAY}-bug${num}-$(printf '%s' "$SLUG" | tr '_' '-').md"
for f in "$REC" "$REPRO" "$FRAG"; do [ ! -e "$f" ] || die "$f already exists"; done
mkdir -p "$REPRO_DIR"
{
  printf '# Bug %s — %s\n\n' "$num" "$TITLE"
  printf '**Status:** OPEN\n'
  printf '**Found:** %s, %s by what (workload, probe, review, example)\n' "$TODAY" "$TOKEN"
  printf '**Severity:** %s what goes wrong for a user (silent wrong code, unsound claim, crash, refusal)\n\n' "$TOKEN"
  printf '<!-- bug-ledger\nsummary: %s\n' "$TITLE"
  printf 'coverage: reproducer %s, pinned by %s the gate leg that pins it (for example check_example_outputs.sh, open-bug reproducers leg)\n' "$REPRO" "$TOKEN"
  [ -z "$CLASS" ] || printf 'class: %s\n' "$CLASS"
  [ -z "$SILENT" ] || printf 'silent: %s\n' "$SILENT"
  printf -- '-->\n\n'
  printf '## Reproducer\n\n`%s`: %s the smallest program that shows the defect.\n\n' "$REPRO" "$TOKEN"
  printf '## Observed\n\n%s what the binary and `--interp` do (exit status, output, diagnostic).\n\n' "$TOKEN"
  printf '## Expected\n\n%s what the normative rule says, with a link to it.\n\n' "$TOKEN"
  printf '## Cause\n\n%s the code that produces it (file, function).\n\n' "$TOKEN"
  printf '## Structural cause\n\n%s the design fact that let this bug exist: which class of\n' "$TOKEN"
  printf '[STRUCTURAL_CLASSES.md](STRUCTURAL_CLASSES.md) it is an instance of (or the new class it adds),\n'
  printf 'and the other bugs of that class.\n\n'
  printf '## Structural owner\n\n%s the roadmap task, exit or verified-pipeline stage that removes the\n' "$TOKEN"
  printf 'cause for the whole class, not only this spelling.\n\n'
  printf '## Containment while open\n\n%s the gate leg that pins the current behaviour, and what a user\n' "$TOKEN"
  printf 'does meanwhile. A silent defect is stopped at once by repair or fail-closed containment\n'
  printf '([defect disposition rule](../../ROADMAP.md#the-current-execution-queue)).\n'
} > "$REC"
{
  printf '// Bug %s: %s\n' "$num" "$TITLE"
  printf '// %s replace this body with the smallest program that shows the defect, and state\n' "$TOKEN"
  printf '// here what it does (Observed) and what it should do (Expected).\n'
  printf 'fn main() -> Int {\n    return 0;\n}\n'
} > "$REPRO"
{
  printf '### Bug %s: %s (%s)\n\n' "$num" "$TITLE" "$TODAY"
  printf '%s what changed for a user, the regression leg, and the structural owner\n' "$TOKEN"
  printf '([bug %s](docs/bugs/%s)).\n' "$num" "$(basename "$REC")"
} > "$FRAG"

if ! python3 -I scripts/gen/bug_ledger.py --write >/dev/null || ! python3 -I scripts/gen/bug_ledger.py --check >/dev/null \
   || ! python3 -I scripts/gen/changelog.py --check >/dev/null; then
  rm -f "$REC" "$REPRO" "$FRAG"; rmdir "$REPRO_DIR" 2>/dev/null || true
  python3 -I scripts/gen/bug_ledger.py --write >/dev/null 2>&1 || true
  die "the ledger or changelog refused the new files (above); they were removed. Bug $num stays reserved; pass it to whoever files next or leave the gap"
fi
if [ -f scripts/gen/bug_dashboard.py ]; then python3 -I scripts/gen/bug_dashboard.py --write >/dev/null; fi

echo "new_bug: filed bug $num"
echo "  record      $REC"
echo "  reproducer  $REPRO"
echo "  changelog   $FRAG"
echo "  views       regenerated (python3 scripts/gen/bug_ledger.py --write)"
echo
echo "Still to fill in (every $TOKEN; check_bug_filing_complete.sh refuses them on landing):"
grep -n -F "$TOKEN" "$REC" "$REPRO" "$FRAG" | sed -E 's/^([^:]+:[0-9]+):.*FILL-IN\(new_bug\.sh\) ?/  \1  /'
[ -n "$CLASS" ]  || echo "  $REC  no class: add 'class: N' to the ledger block (docs/bugs/STRUCTURAL_CLASSES.md), or add a class"
[ -n "$SILENT" ] || echo "  $REC  no silent: add 'silent: yes' (wrong code, unsound claim) or 'silent: no' to the ledger block"
echo "Then: pin the reproducer in a gate leg, rerun python3 scripts/gen/bug_ledger.py --write, and commit all four."
