#!/usr/bin/env bash
# File a new bug: allocate its number race-safely, write the record, the reproducer skeleton, the
# changelog fragment and its discovery attribution, regenerate the views, and print what is left.
#
#   scripts/gen/new_bug.sh "<title>" --silent yes|no --batch <text> [--class N]
#   scripts/gen/new_bug.sh "<title>" --silent yes|no --new-batch <kind> --probe <text> --effort <size> [--class N]
#   scripts/gen/new_bug.sh --number NNN "<title>" ...  file under NNN, already reserved and unfiled
#   scripts/gen/new_bug.sh --number-only "<title>"     allocate and reserve a number, write nothing
#
# DISCOVERY (bug 300). Every bug from 079 on is attributed to one probe batch of
# docs/bugs/discovery.tsv, and bug_dashboard.py refuses a record without one. The row names the
# number, which only exists once it is reserved, so the scaffold writes the attribution itself:
# --batch adds the bug to the one existing row whose probe cell contains <text>; --new-batch adds a
# row dated today with that kind, probe and effort. Either one, and --silent (the row's silent cell
# and the record's silent: line must agree), is required when the tree has the discovery record and
# the dashboard, and is checked before a number is reserved. A filing that still fails after the
# reservation removes everything it wrote, takes the bug back out of discovery.tsv, hands the number back as
# `reserved` (see RESERVATION) and prints the --number command that files the same bug under it.
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
# The message's first line also names the number's state: `bug NNN reserved: <title>` is held and
# free to file (--number-only makes it; a failed filing hands its number back to it), and
# `bug NNN filing: <title>` is taken by a filing run (a full run reserves straight into it; --number
# moves a `reserved` number to it with a lease on the commit it read, so one of two claims wins).
#
# FILES (the tree must be migrated to the ledger: scripts/gen/migrate_ledgers.sh):
#   docs/bugs/NNN_<slug>.md                      the record, with its bug-ledger block
#   tests/regressions/bugNNN_<slug>/repro.con    the reproducer skeleton
#   changelog.d/<date>-bugNNN-<slug>.md          the changelog fragment
#   docs/bugs/discovery.tsv                      the bug added to its batch's row (see DISCOVERY)
# then `scripts/gen/bug_ledger.py --write` (and bug_dashboard.py when present) regenerates the views.
# Every placeholder carries the token FILL-IN(new_bug.sh); check_bug_filing_complete.sh refuses a
# record, reproducer or fragment that still holds one, so a draft can be backed up on wip/* but
# cannot land.
#
# ONE FILING PER WORKTREE AT A TIME. A full filing rewrites shared files of the worktree
# (discovery.tsv, the generated views), so from its first check to its last write or rollback it
# holds a lock, the directory `git rev-parse --git-path new_bug.lock` with the holder's pid; a second
# run waits for it, then refuses. A lock left by a killed run is never broken automatically: the
# refusal names the pid, and the lock is removed by hand once no new_bug.sh runs. Every rewrite of
# discovery.tsv goes through a temporary file renamed into place, so a failed write leaves it whole.
#
# ENVIRONMENT: CONCRETE_BUG_REMOTE (default origin; `fork` is refused), NEW_BUG_NO_FETCH=1 (skip the
# fetch; tests), NEW_BUG_DATE (YYYY-MM-DD; tests), NEW_BUG_LOCK_WAIT (seconds to wait for the
# worktree's filing lock, default 300). Exit 0 filed, 1 refused, 2 usage.
set -euo pipefail

die() { echo "new_bug: $*" >&2; exit 1; }
usage() { sed -n '2,19p' "$0" >&2; exit 2; }

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT"
REMOTE="${CONCRETE_BUG_REMOTE:-origin}"
TODAY="${NEW_BUG_DATE:-$(date +%Y-%m-%d)}"
TOKEN="FILL-IN(new_bug.sh)"

TITLE=""; CLASS=""; SILENT=""; NUMBER_ONLY=0; NUMBER=""
BATCH=""; NEW_KIND=""; NEW_PROBE=""; NEW_EFFORT=""; HAVE_BATCH=0; HAVE_NEW=0; HAVE_PROBE=0; HAVE_EFFORT=0
ARGS=("$@")
while [ $# -gt 0 ]; do
  case "$1" in
    --class) [ $# -ge 2 ] || usage; CLASS="$2"; shift 2 ;;
    --silent) [ $# -ge 2 ] || usage; SILENT="$2"; shift 2 ;;
    --batch) [ $# -ge 2 ] || usage; BATCH="$2"; HAVE_BATCH=1; shift 2 ;;
    --new-batch) [ $# -ge 2 ] || usage; NEW_KIND="$2"; HAVE_NEW=1; shift 2 ;;
    --probe) [ $# -ge 2 ] || usage; NEW_PROBE="$2"; HAVE_PROBE=1; shift 2 ;;
    --effort) [ $# -ge 2 ] || usage; NEW_EFFORT="$2"; HAVE_EFFORT=1; shift 2 ;;
    --number) [ $# -ge 2 ] || usage; NUMBER="$2"; shift 2 ;;
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
[ -z "$NUMBER" ] || [[ "$NUMBER" =~ ^[0-9]{3}$ ]] || die "--number takes a three-digit bug number"
[ "$NUMBER_ONLY" = 0 ] || [ -z "$NUMBER" ] || die "--number files under a number already held; --number-only reserves a new one"
DISC="docs/bugs/discovery.tsv"
TRACKED=0; if [ -f "$DISC" ] && [ -f scripts/gen/bug_dashboard.py ]; then TRACKED=1; fi
if [ "$NUMBER_ONLY" = 1 ]; then
  [ "$HAVE_BATCH$HAVE_NEW$HAVE_PROBE$HAVE_EFFORT" = 0000 ] || die "--number-only writes nothing, so it takes no batch"
else
  [ "$HAVE_BATCH" = 0 ] || [ "$HAVE_NEW" = 0 ] || die "--batch and --new-batch both given: a bug belongs to exactly one batch"
  [ "$HAVE_NEW" = 1 ] || [ "$HAVE_PROBE$HAVE_EFFORT" = 00 ] || die "--probe and --effort describe the row of a --new-batch"
  if [ "$TRACKED" = 1 ]; then
    [ "$HAVE_BATCH$HAVE_NEW" != 00 ] || die "no discovery batch given. Every bug from 079 on is attributed to the probe batch that found it ($DISC), and that row must name the number this run reserves, so the scaffold writes it: pass --batch <text from the probe cell of an existing row>, or --new-batch <kind> --probe <what was probed> --effort <size> for a new row dated $TODAY"
    [ -n "$SILENT" ] || die "--silent yes|no is required: the record's silent: line and the discovery row's silent cell must agree"
    [ "$HAVE_NEW" = 0 ] || { [ "$HAVE_PROBE" = 1 ] && [ "$HAVE_EFFORT" = 1 ]; } || die "--new-batch needs --probe and --effort"
  else
    [ "$HAVE_BATCH$HAVE_NEW" = 00 ] || die "this tree has no discovery record ($DISC with scripts/gen/bug_dashboard.py), so --batch and --new-batch have no row to write"
  fi
fi

no_op_in_progress() {
  local p
  for p in MERGE_HEAD CHERRY_PICK_HEAD REVERT_HEAD rebase-merge rebase-apply; do
    [ ! -e "$(git rev-parse --git-path "$p")" ] || die "a git operation is in progress ($p); finish or abort it first"
  done
}
no_op_in_progress

# The discovery attribution (bug 300). `check` validates --batch or --new-batch against the record
# and its rules (bug_dashboard.py's own constants); `apply NNN` adds NNN to the matched row, or
# inserts the new row in date order, with NNN in the silent cell when --silent yes; `revert NNN`
# undoes only that edit (NNN out of every row, and the --new-batch row it inserted), so a rollback
# never clobbers another filing's row written meanwhile, as restoring a whole copy would.
discovery() {
  python3 -I - "$ROOT" "$DISC" "$1" "${2:-}" "$SILENT" "$HAVE_BATCH" "$BATCH" "$NEW_KIND" "$NEW_PROBE" "$NEW_EFFORT" "$TODAY" <<'PY'
import os, sys
root, rel, mode, num, silent, have_batch, batch, kind, probe, effort, today = sys.argv[1:12]
sys.path.insert(0, os.path.join(root, "scripts", "gen"))
import bug_dashboard as d
def refuse(m):
    print(f"new_bug: {m}", file=sys.stderr); sys.exit(1)
path = os.path.join(root, rel)
lines = open(path, encoding="utf-8").read().split("\n")
data = [i for i, ln in enumerate(lines) if ln and not ln.startswith("#")]
if not data or lines[data[0]].split("\t") != d.COLS:
    refuse(f"{rel} has no header row {' '.join(d.COLS)}")
rows, col = data[1:], {c: i for i, c in enumerate(d.COLS)}
def cell(i, c):
    cs = lines[i].split("\t")
    return cs[col[c]].strip() if len(cs) == len(d.COLS) else ""
def save():
    tmp = f"{path}.new_bug.{os.getpid()}"
    try:
        with open(tmp, "w", encoding="utf-8") as fh:
            fh.write("\n".join(lines))
        os.replace(tmp, path)
    except OSError as e:
        try:
            os.unlink(tmp)
        except OSError:
            pass
        refuse(f"could not rewrite {rel}: {e}")
if mode == "revert":
    def drop(v):
        rest = [b for b in v.strip().split(",") if b and b != num]
        return ",".join(rest) or "-"
    mine = "\t".join([today, kind, probe, effort, "-", "-"])
    keep = []
    for i, ln in enumerate(lines):
        if i in rows and len(ln.split("\t")) == len(d.COLS):
            cs = ln.split("\t")
            if num in cs[col["bugs"]].split(",") or num in cs[col["silent"]].split(","):
                cs[col["bugs"]], cs[col["silent"]] = drop(cs[col["bugs"]]), drop(cs[col["silent"]])
                ln = "\t".join(cs)
                if have_batch != "1" and ln == mine:
                    continue
        keep.append(ln)
    lines = keep
    save()
    sys.exit(0)
target = None
if have_batch == "1":
    if not batch.strip():
        refuse("--batch takes text from the probe cell of the batch that found the bug")
    hits = [i for i in rows if batch in cell(i, "probe")]
    if len(hits) != 1:
        shown = "".join(f"\n  {cell(i, 'date')}  {cell(i, 'probe')[:90]}" for i in hits[:5])
        refuse(f"--batch '{batch}' matches {len(hits)} rows of {rel}, not one{':' if hits else ''}{shown}\n"
               "give text only the batch that found the bug has, or add its row with --new-batch")
    target = hits[0]
else:
    for name, v in (("--new-batch", kind), ("--probe", probe), ("--effort", effort)):
        if not v.strip() or any(c in v for c in "\t\n\r"):
            refuse(f"{name} takes one non-empty line with no tab")
    if kind not in d.COUNTING | d.ATTRIBUTION:
        refuse(f"--new-batch kind '{kind}' is not one of {', '.join(sorted(d.COUNTING | d.ATTRIBUTION))}")
    tm = d.CLASS_TAG.match(probe)
    if tm and kind not in d.COUNTING:
        refuse(f"a 'class N: ' probe tag marks a counting batch, not a {kind} row")
    if tm and int(tm.group(1)) not in {c[0] for c in d.load_classes(root)}:
        refuse(f"the probe is tagged class {tm.group(1)}, which STRUCTURAL_CLASSES.md does not have")
    if kind in d.COUNTING and today >= d.EFFORT_REQUIRED_FROM and not d.EFFORT.match(effort):
        refuse(f"a counting batch from {d.EFFORT_REQUIRED_FROM} on records its effort as "
               f"'<N> hours|programs|probes|examples|cases', got '{effort}'")
if mode == "check":
    sys.exit(0)
def add(v):
    return num if v.strip() == "-" else v + "," + num
if target is not None:
    cs = lines[target].split("\t")
    cs[col["bugs"]] = add(cs[col["bugs"]])
    if silent == "yes":
        cs[col["silent"]] = add(cs[col["silent"]])
    lines[target] = "\t".join(cs)
else:
    pos = data[0]
    for i in rows:
        if cell(i, "date") <= today:
            pos = i
    lines.insert(pos + 1, "\t".join([today, kind, probe, effort, num, num if silent == "yes" else "-"]))
save()
PY
}

# ----------------------------------------------------------------------------- validate up front
# Everything that could refuse runs before a number is reserved, so a refusal never burns one.
LOCK=""
unlock() { [ -z "$LOCK" ] || rm -rf "$LOCK"; LOCK=""; }
if [ "$NUMBER_ONLY" = 0 ]; then
  wait_s="${NEW_BUG_LOCK_WAIT:-300}"; [[ "$wait_s" =~ ^[0-9]+$ ]] || die "NEW_BUG_LOCK_WAIT takes a number of seconds"
  lk="$(git rev-parse --git-path new_bug.lock)"; waited=0
  until mkdir "$lk" 2>/dev/null; do
    if [ "$waited" -ge "$wait_s" ]; then
      die "another filing holds this worktree's lock $lk (pid $(cat "$lk/pid" 2>/dev/null || echo unknown)); if no new_bug.sh is running, remove it (rm -r '$lk') and run again"
    fi
    [ "$waited" -gt 0 ] || echo "new_bug: waiting for another filing in this worktree ($lk)" >&2
    sleep 1; waited=$((waited + 1))
  done
  LOCK="$lk"; trap unlock EXIT
  echo $$ > "$LOCK/pid"
  [ -n "$(python3 -I scripts/gen/bug_ledger.py corpus)" ] \
    || die "this tree is not migrated to the bug ledger (scripts/gen/migrate_ledgers.sh), so there is no ledger block to write; file by hand (docs/bugs/README.md, Filing a bug) or use --number-only"
  python3 -I scripts/gen/bug_ledger.py --check >/dev/null \
    || die "the ledger views are stale before the new bug is added; run python3 scripts/gen/bug_ledger.py --write first"
  python3 -I scripts/gen/changelog.py --check >/dev/null \
    || die "changelog.d/ or CHANGELOG.md is already malformed (above); repair it first"
  if [ "$TRACKED" = 1 ]; then
    python3 -I scripts/gen/bug_dashboard.py --check >/dev/null \
      || die "the bug dashboard is stale or refuses this tree before the new bug is added (above); repair it, then run python3 scripts/gen/bug_dashboard.py --write"
    discovery check || exit 1
  fi
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
# On any failure after the first file is written (a refused record, a failed write), every file this
# run created is removed, its discovery.tsv edit is undone, and the views are regenerated without it, so a
# failed filing leaves the tree as it found it. The reserved number stays reserved, and the message
# names the --number command that files the same bug under it.
WRITING=0; HELD=0; CLAIM=""; REC=""; REPRO=""; REPRO_DIR=""; FRAG=""; DISC_EDIT=0
cleanup() {
  local rc=$? resume
  rm -f "${seen:-}"
  if [ "$rc" -ne 0 ] && [ "$WRITING" = 1 ]; then
    rm -f "$REC" "$REPRO" "$FRAG"; [ -z "$REPRO_DIR" ] || rmdir "$REPRO_DIR" 2>/dev/null || true
    [ "$DISC_EDIT" = 0 ] || discovery revert "$num" || echo "new_bug: could not take bug $num back out of $DISC; remove it by hand" >&2
    python3 -I scripts/gen/bug_ledger.py --write >/dev/null 2>&1 || true
    [ "$TRACKED" = 0 ] || python3 -I scripts/gen/bug_dashboard.py --write >/dev/null 2>&1 || true
    echo "new_bug: filing failed; removed the files it had written and restored the views" >&2
  fi
  if [ "$rc" -ne 0 ] && [ "$HELD" = 1 ]; then
    if release; then
      resume="scripts/gen/new_bug.sh"; [ -n "$NUMBER" ] || resume="$resume --number $num"
      resume="$resume$(printf ' %q' "${ARGS[@]}")"
      echo "new_bug: bug $num stays reserved and unfiled; once the cause above is repaired, file it with" >&2
      echo "  $resume" >&2
    else
      echo "new_bug: bug $num stays reserved and unfiled, and could not be handed back for --number (refs/bug-reservations/$num still reads 'filing'); leave the gap" >&2
    fi
  fi
  unlock
}
seen="$(mktemp)"; trap cleanup EXIT
record_nums() { sed -nE 's#^(.*/)?([0-9]{3})_[^/]*\.md$#\2#p'; }
while IFS= read -r ref; do
  git ls-tree --name-only "$ref" docs/bugs/ 2>/dev/null | record_nums >> "$seen" || true
done < <(git for-each-ref --format='%(refname)' "refs/remotes/$REMOTE" refs/heads)
while IFS= read -r wt; do
  [ -d "$wt/docs/bugs" ] || continue
  find "$wt/docs/bugs" -maxdepth 1 -name '[0-9][0-9][0-9]_*.md' | record_nums >> "$seen"
done < <(git worktree list --porcelain | sed -n 's/^worktree //p')
reservations="$(git ls-remote "$REMOTE" 'refs/bug-reservations/*')" || die "git ls-remote $REMOTE failed; cannot read the reservations"

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
# res_commit STATE [NOTE]: a reservation commit for $num. The nonce makes every one distinct: two
# identical requests in one second would otherwise build one SHA, and pushing a SHA the remote
# already holds "succeeds" for both.
res_commit() {
  local nonce; nonce="$(od -An -N8 -tx1 /dev/urandom | tr -d ' \n')"
  git commit-tree "$empty_tree" -m "bug $num $1: $TITLE" -m "by $who at $(date -u +%Y-%m-%dT%H:%M:%SZ), nonce $nonce${2:+, $2}"
}
# release: hand a number this run claimed for filing back as `reserved`, with a lease on the claim, so
# a later --number run can take it.
release() {
  local c; c="$(res_commit reserved "released after a failed filing")" || return 1
  git push --quiet --force-with-lease="refs/bug-reservations/$num:$CLAIM" "$REMOTE" "$c:refs/bug-reservations/$num" 2>/dev/null
}

if [ -n "$NUMBER" ]; then
  # --number: claim a `reserved` number (from --number-only, or released by a failed filing) that this
  # user holds and no branch or worktree has a record for, by moving its ref to a `filing` commit
  # with a lease on the reservation it read: of two runs claiming one number, exactly one wins.
  num="$NUMBER"
  ! grep -qx "$num" "$seen" || die "bug $num already has a record on a branch or in a worktree; a filed number is never filed again"
  rsha="$(printf '%s\n' "$reservations" | awk -v r="refs/bug-reservations/$num" '$2 == r { print $1 }')"
  [ -n "$rsha" ] || die "bug $num is not reserved on $REMOTE (refs/bug-reservations/$num); omit --number to reserve a new number"
  git fetch --quiet --no-write-fetch-head "$REMOTE" "refs/bug-reservations/$num" \
    || die "could not fetch refs/bug-reservations/$num from $REMOTE"
  rsub="$(git log -1 --format=%s "$rsha")" && rbody="$(git log -1 --format=%b "$rsha")" \
    || die "could not read the reservation commit of bug $num"
  case "$rsub" in
    "bug $num reserved: "*) ;;
    "bug $num filing: "*) die "bug $num is being filed, or was filed, by another run (refs/bug-reservations/$num reads '$rsub'); a held number is filed once" ;;
    *) die "refs/bug-reservations/$num ('$rsub') is not a reservation new_bug.sh can file under" ;;
  esac
  rby="$(printf '%s\n' "$rbody" | sed -n 's/^by \(.*\) at .*/\1/p')"
  case "$rby" in "$(git config user.email || echo unknown)@"*) ;;
    *) die "bug $num was reserved by '${rby:-unknown}', not by this user; file under a number you reserved" ;; esac
  c="$(res_commit filing "claimed with --number")"
  git push --quiet --force-with-lease="refs/bug-reservations/$num:$rsha" "$REMOTE" "$c:refs/bug-reservations/$num" 2>/dev/null \
    || die "could not claim bug $num: another run claimed it first, or the push failed"
  CLAIM="$c"; HELD=1
  no_op_in_progress
  echo "new_bug: claimed bug $num on $REMOTE for filing (refs/bug-reservations/$num)"
else
  printf '%s\n' "$reservations" | sed -nE 's#.*refs/bug-reservations/([0-9]+)$#\1#p' >> "$seen"
  max="$(awk '/^[0-9]+$/ { n = $0 + 0; if (n > m) m = n } END { print m + 0 }' "$seen")"
  n=$((max + 1))
  # --number-only leaves the number `reserved`; a full filing creates it already `filing`.
  state=filing; [ "$NUMBER_ONLY" = 0 ] || state=reserved
  tries=0
  while :; do
    tries=$((tries + 1)); [ "$tries" -le 50 ] || die "could not reserve a number after 50 attempts"
    num="$(printf '%03d' "$n")"
    [ "${#num}" -eq 3 ] || die "bug numbers past 999 need a wider record name (docs/bugs/NNN_*.md)"
    c="$(res_commit "$state")"
    if git push --quiet --force-with-lease="refs/bug-reservations/$num:" "$REMOTE" "$c:refs/bug-reservations/$num" 2>/dev/null; then
      break
    fi
    # Rejected: taken by a concurrent reservation (try the next number), or the push itself failed.
    git ls-remote --exit-code "$REMOTE" "refs/bug-reservations/$num" >/dev/null 2>&1 \
      || die "the reservation push for bug $num failed and the ref does not exist; check access to '$REMOTE'"
    echo "new_bug: bug $num was reserved concurrently; trying the next number" >&2
    n=$((n + 1))
  done
  [ "$NUMBER_ONLY" = 1 ] || { CLAIM="$c"; HELD=1; }
  no_op_in_progress
  echo "new_bug: reserved bug $num on $REMOTE (refs/bug-reservations/$num)"
  if [ "$NUMBER_ONLY" = 1 ]; then
    echo "$num"
    exit 0
  fi
fi

# ----------------------------------------------------------------------------- write
REC="docs/bugs/${num}_${SLUG}.md"
REPRO_DIR="tests/regressions/bug${num}_${SLUG}"
REPRO="$REPRO_DIR/repro.con"
FRAG="changelog.d/${TODAY}-bug${num}-$(printf '%s' "$SLUG" | tr '_' '-').md"
# -L as well as -e: a dangling symlink is not "absent", and the rollback must never delete a path
# this run did not create.
for f in "$REC" "$REPRO" "$FRAG" "$REPRO_DIR"; do
  if [ -e "$f" ] || [ -L "$f" ]; then die "$f already exists"; fi
done
WRITING=1
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
if [ "$TRACKED" = 1 ]; then
  DISC_EDIT=1   # armed first: revert is a no-op when apply wrote nothing
  discovery apply "$num" || die "could not add bug $num to $DISC (above)"
fi

if ! python3 -I scripts/gen/bug_ledger.py --write >/dev/null || ! python3 -I scripts/gen/bug_ledger.py --check >/dev/null \
   || ! python3 -I scripts/gen/changelog.py --check >/dev/null; then
  die "the ledger or changelog refused the new files (above)"
fi
if [ -f scripts/gen/bug_dashboard.py ]; then
  python3 -I scripts/gen/bug_dashboard.py --write >/dev/null || die "the bug dashboard refused the new files (above)"
fi
WRITING=0; HELD=0

echo "new_bug: filed bug $num"
echo "  record      $REC"
echo "  reproducer  $REPRO"
echo "  changelog   $FRAG"
if [ "$TRACKED" = 1 ]; then
  if [ "$HAVE_BATCH" = 1 ]; then echo "  discovery   $DISC: added to the batch matching '$BATCH'"
  else echo "  discovery   $DISC: new $NEW_KIND batch dated $TODAY"; fi
fi
echo "  views       regenerated (python3 scripts/gen/bug_ledger.py --write)"
echo
echo "Still to fill in (every $TOKEN; check_bug_filing_complete.sh refuses them on landing):"
grep -n -F "$TOKEN" "$REC" "$REPRO" "$FRAG" | sed -E 's/^([^:]+:[0-9]+):.*FILL-IN\(new_bug\.sh\) ?/  \1  /'
[ -n "$CLASS" ]  || echo "  $REC  no class: add 'class: N' to the ledger block (docs/bugs/STRUCTURAL_CLASSES.md), or add a class"
[ -n "$SILENT" ] || echo "  $REC  no silent: add 'silent: yes' (wrong code, unsound claim) or 'silent: no' to the ledger block"
echo "Then: pin the reproducer in a gate leg, rerun python3 scripts/gen/bug_ledger.py --write, and commit them all."
