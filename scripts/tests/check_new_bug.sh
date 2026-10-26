#!/usr/bin/env bash
# The bug scaffold, scripts/gen/new_bug.sh, in a scratch repository with its own bare "origin".
#
# Legs:
#   1. allocation sees every place another writer could have used a number: a wip/* branch and a
#      ci/* branch on the remote, a local-only branch, an untracked record in another worktree, and
#      a reservation ref; gaps are not reused;
#   2. race safety: concurrent allocations against one remote return distinct numbers, each with its
#      reservation ref;
#   3. the full scaffold writes the record, reproducer and fragment, the generated views agree with
#      the records afterwards (bug_ledger.py --check), the record lands in the README list and in its
#      class row, and check_bug_filing_complete.sh refuses the unfilled placeholders until they are
#      filled;
#   4. refusals burn no number: a title stating a status, an unknown class, an unmigrated tree, and
#      a pre-push hook without the reservation tier;
#   5. the pre-push hook's reservation tier accepts a parentless empty-tree commit on
#      refs/bug-reservations/NNN and refuses anything else there;
#   6. discovery attribution (bug 300): the scaffold writes the bug's docs/bugs/discovery.tsv row
#      itself (--batch joins an existing row, --new-batch adds one), so bug_dashboard.py accepts the
#      filing; a filing with no batch, an ambiguous or unknown batch, or a malformed new row is
#      refused before a number is reserved; a failure after the reservation restores discovery.tsv
#      and prints a --number command that files the same bug under the number it holds, without a
#      second reservation; --number refuses a number that is not reserved, is another user's, is
#      being filed by another run, or already has a record, and of two concurrent --number runs on
#      one held number exactly one files it; the commands the fuzz farm and CI triage print are
#      accepted as printed.
#
# Copies the ledger inputs the way check_ledgers.sh does (migrating the copy when this tree is not
# migrated yet), so it needs no compiler build. Runs in seconds.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
# A pre-push hook exports GIT_DIR and friends; the scratch repositories must not inherit them.
unset $(git rev-parse --local-env-vars) 2>/dev/null || true
PASS=0; FAIL=0
ok() { echo "  ok   $1"; PASS=$((PASS+1)); }
no() { echo "  FAIL $1"; FAIL=$((FAIL+1)); }
TMP="$(mktemp -d)" || { echo "FATAL: mktemp -d failed" >&2; exit 2; }
case "$TMP" in /?*/?*) [ -d "$TMP" ] || { echo "FATAL: mktemp gave no directory" >&2; exit 2; } ;;
  *) echo "FATAL: unusable temporary directory '$TMP'" >&2; exit 2 ;; esac
trap 'rm -rf "$TMP"' EXIT
export GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_NOSYSTEM=1 NEW_BUG_DATE=2026-01-02
G() { git -C "$W" "$@"; }

# ----------------------------------------------------------------------------- the scratch repo
W="$TMP/work"; O="$TMP/origin.git"
mkdir -p "$W/docs" "$W/scripts/tests" "$W/scripts/gen" "$W/.github/workflows" "$W/tests" "$W/.githooks"
cp -R docs/bugs "$W/docs/bugs"
cp ROADMAP.md CHANGELOG.md "$W/"
cp -R changelog.d "$W/changelog.d"
# bug_dashboard.py too: without it the scaffold skips the discovery attribution and the dashboard,
# which is how bug 300 (a filing the dashboard refused after the reservation) went unseen here.
cp scripts/gen/bug_ledger.py scripts/gen/bug_dashboard.py scripts/gen/changelog.py scripts/gen/migrate_ledgers.py \
   scripts/gen/new_bug.sh "$W/scripts/gen/"
cp scripts/tests/audit_bug_corpus.sh scripts/tests/check_bug_filing_complete.sh \
   scripts/tests/run_tests.sh scripts/tests/run_ci_gates_local.sh "$W/scripts/tests/"
# The copied gates start by sourcing the gate-shell guard from beside themselves.
mkdir -p "$W/scripts/tests/lib" && cp scripts/tests/lib/gate_env.sh scripts/tests/lib/has_match.sh "$W/scripts/tests/lib/"
[ -f scripts/tests/ci_gate_inventory.txt ] && cp scripts/tests/ci_gate_inventory.txt "$W/scripts/tests/"
cp .github/workflows/lean_action_ci.yml "$W/.github/workflows/"
cp .githooks/pre-push "$W/.githooks/"
ln -s "$ROOT_DIR/tests/programs" "$W/tests/programs"
cp -R tests/regressions "$W/tests/regressions"
RAW="$TMP/raw"; cp -RP "$W" "$RAW"   # the unmigrated copy, for the refusal leg
if [ -z "$(python3 -I scripts/gen/bug_ledger.py corpus)" ]; then
  MIGRATED_HERE=0
  { python3 -I "$W/scripts/gen/migrate_ledgers.py" "$W" \
      && python3 -I "$W/scripts/gen/bug_ledger.py" --write --root "$W" \
      && python3 -I "$W/scripts/gen/changelog.py" --stamp-initial --root "$W"; } > "$TMP/migrate.log" 2>&1 \
    || { no "the scratch copy could not be migrated (log below)"; sed 's/^/      /' "$TMP/migrate.log" | awk 'NR<=30'; }
else MIGRATED_HERE=1; fi
git init -q -b main "$W" && G config user.email gate@example.invalid && G config user.name gate \
  && G add -A && G commit -q -m base && git init -q --bare "$O" && G remote add origin "$O" \
  && G push -q origin main || { echo "FATAL: could not build the scratch repository" >&2; exit 2; }
# A single-branch refspec, as a `clone --single-branch` has: the scaffold must fetch every head anyway.
G config remote.origin.fetch '+refs/heads/main:refs/remotes/origin/main'
NB="$W/scripts/gen/new_bug.sh"
# Every full filing names its discovery batch (bug 300); refusal legs pass one so they reach the
# refusal they are about.
BN=(--new-batch development --probe "gate control batch" --effort "1 probe")
BA=(--silent no "${BN[@]}")
DISC="$W/docs/bugs/discovery.tsv"
dash_ok() { python3 -I "$W/scripts/gen/bug_dashboard.py" --check --root "$W" >/dev/null 2>&1; }
M="$(ls "$W/docs/bugs" | sed -nE 's/^([0-9]{3})_.*/\1/p' | sort -n | tail -1)"; M=$((10#$M))
num_of() { awk 'NF { last = $0 } END { print last }' "$1"; }
alloc() { # expected number, name: run --number-only and compare
  local want; want="$(printf '%03d' "$1")"
  if bash "$NB" --number-only "alloc control $2" > "$TMP/alloc.out" 2>&1 && [ "$(num_of "$TMP/alloc.out")" = "$want" ] \
     && git -C "$W" ls-remote --exit-code origin "refs/bug-reservations/$want" >/dev/null; then
    ok "allocation $2: bug $want, reserved on the remote"
  else no "allocation $2: wanted $want, got: $(tr '\n' ' ' < "$TMP/alloc.out")"; fi
}
on_branch() { # branch, number: commit a record for <number> on <branch> from main
  G checkout -q -b "$1" main && printf '# Bug %03d — x\n\n**Status:** OPEN\n' "$2" > "$W/docs/bugs/$(printf '%03d' "$2")_other.md" \
    && G add -A && G commit -q -m "bug $2" && G checkout -q main
}

echo "=== 1. allocation sees every writer (highest bug on main: $(printf '%03d' $M)) ==="
alloc $((M + 1)) "on a fresh remote"
on_branch wip/other $((M + 5)) && G push -q origin wip/other && G branch -q -D wip/other
alloc $((M + 6)) "past a record on a remote wip/* branch the configured refspec does not fetch"
alloc $((M + 7)) "past the reservation just made"
on_branch ci/cand $((M + 20)) && G push -q origin ci/cand && G branch -q -D ci/cand
alloc $((M + 21)) "past a record on a remote ci/* branch"
on_branch localonly $((M + 25))
alloc $((M + 26)) "past a record on an unpushed local branch"
G worktree add -q --detach "$TMP/wt2" main && printf 'x\n' > "$TMP/wt2/docs/bugs/$(printf '%03d' $((M + 30)))_untracked.md"
alloc $((M + 31)) "past an untracked record in another worktree"
H="$(G commit-tree "$(G hash-object -t tree /dev/null)" -m held)"
G push -q origin "$H:refs/bug-reservations/$(printf '%03d' $((M + 40)))"
alloc $((M + 41)) "past a reservation held by another writer"
G branch -q -D localonly; G worktree remove --force "$TMP/wt2"

echo "=== 2. concurrent allocations get distinct numbers ==="
pids=""
# One title and frozen commit dates for all six, so identical requests build identical commits unless
# each carries its own nonce. The interleaving is up to the scheduler: this leg can catch a collision,
# it cannot force one on every run.
for i in 1 2 3 4 5 6; do GIT_AUTHOR_DATE='2026-01-02T00:00:00Z' GIT_COMMITTER_DATE='2026-01-02T00:00:00Z' \
  bash "$NB" --number-only "race" > "$TMP/race.$i" 2>&1 & pids="$pids $!"; done
rc_all=0; for p in $pids; do wait "$p" || rc_all=1; done
got="$(for i in 1 2 3 4 5 6; do num_of "$TMP/race.$i"; done | sort)"
held=0; for x in $got; do git -C "$W" ls-remote --exit-code origin "refs/bug-reservations/$x" >/dev/null && held=$((held+1)); done
if [ "$rc_all" = 0 ] && [ "$(printf '%s\n' "$got" | grep -cE '^[0-9]{3}$')" = 6 ] \
   && [ "$(printf '%s\n' "$got" | sort -u | grep -c .)" = 6 ] && [ "$held" = 6 ]; then
  ok "six concurrent allocations returned six distinct reserved numbers ($(echo $got))"
else no "concurrent allocations collided or failed: $(echo $got) (exit status $rc_all, $held reserved)"; cat "$TMP"/race.* | awk 'NR<=12' | sed 's/^/      /'; fi

echo "=== 3. the scaffold files a bug and the generated views agree ==="
before="$(git -C "$W" ls-remote origin 'refs/bug-reservations/*' | wc -l | tr -d ' ')"
NEXT="$(printf '%03d' $((M + 41 + 6 + 1)))"
if bash "$NB" "Scaffold control: a placeholder defect" --class 1 --silent yes \
     --new-batch hunt --probe "scaffold control hunt" --effort "12 programs" > "$TMP/file.out" 2>&1; then
  ok "new_bug.sh filed a bug"
else no "new_bug.sh failed:"; sed 's/^/      /' "$TMP/file.out" | awk 'NR<=15'; fi
REC="$(ls "$W/docs/bugs/${NEXT}_"*.md 2>/dev/null | head -1)"
if [ -n "$REC" ] && grep -q '^<!-- bug-ledger' "$REC" && grep -q '^\*\*Status:\*\* OPEN' "$REC"; then
  ok "the record is docs/bugs/$(basename "$REC"), with a ledger block and an OPEN status"
else no "no record docs/bugs/${NEXT}_*.md with a ledger block"; fi
missing=""
for sec in Reproducer Observed Expected Cause "Structural cause" "Structural owner" "Containment while open"; do
  grep -q "^## $sec\$" "${REC:-/dev/null}" || missing="$missing '$sec'"
done
[ -z "$missing" ] && ok "the record has every section" || no "the record lacks sections:$missing"
grep -qx 'class: 1' "${REC:-/dev/null}" && grep -qx 'silent: yes' "${REC:-/dev/null}" \
  && ok "the ledger block carries the requested class and silent values" || no "the ledger block lost --class or --silent"
[ -f "$W/tests/regressions/bug${NEXT}_scaffold_control_a_placeholder_defect/repro.con" ] \
  && ok "the reproducer skeleton exists" || no "no reproducer skeleton under tests/regressions/bug${NEXT}_*"
FRAG="$(ls "$W/changelog.d/2026-01-02-bug${NEXT}-"*.md 2>/dev/null | head -1)"
[ -n "$FRAG" ] && ok "the changelog fragment is changelog.d/$(basename "$FRAG")" || no "no changelog fragment for bug $NEXT"
if python3 -I "$W/scripts/gen/bug_ledger.py" --check --root "$W" >/dev/null 2>&1 \
   && python3 -I "$W/scripts/gen/changelog.py" --check --root "$W" >/dev/null 2>&1; then
  ok "the generated views and the changelog agree with the records after filing"
else no "the generated views or the changelog disagree with the records after filing"; fi
if [ "$(awk -F'\t' -v n="$NEXT" '$2 == "hunt" && $3 == "scaffold control hunt" && $4 == "12 programs" && $5 == n && $6 == n' "$DISC" | wc -l | tr -d ' ')" = 1 ] \
   && dash_ok; then
  ok "the scaffold wrote the new batch's discovery row (bug $NEXT, silent) and the dashboard accepts the filing"
else no "no discovery row 'hunt / scaffold control hunt / 12 programs / $NEXT / $NEXT', or bug_dashboard.py --check refuses the filing"; fi
grep -qF "[$(basename "${REC:-x}")]" "$W/docs/bugs/README.md" && ok "the README's generated list names the new bug" \
  || no "the README's generated list does not name the new bug"
grep -qE "^\| 1\. [^|]*\|[^|]*\b$NEXT\b" "$W/docs/bugs/STRUCTURAL_CLASSES.md" && ok "class 1's row lists the new bug" \
  || no "class 1's row in STRUCTURAL_CLASSES.md does not list $NEXT"
after="$(git -C "$W" ls-remote origin 'refs/bug-reservations/*' | wc -l | tr -d ' ')"
[ "$after" = $((before + 1)) ] && ok "filing reserved exactly one number" || no "filing reserved $((after - before)) numbers"
grep -q 'Still to fill in' "$TMP/file.out" && grep -qF "$(basename "${REC:-x}")" "$TMP/file.out" \
  && ok "the scaffold prints what is left to fill in" || no "the scaffold printed no fill-in list"
if (cd "$W" && bash scripts/tests/check_bug_filing_complete.sh) > "$TMP/bfc.out" 2>&1; then
  no "check_bug_filing_complete.sh passed a scaffold whose placeholders are unfilled"
elif grep -q 'unfilled new_bug.sh placeholders' "$TMP/bfc.out"; then
  ok "check_bug_filing_complete.sh refuses the unfilled placeholders"
else no "check_bug_filing_complete.sh failed, but not on the placeholders:"; grep FAIL "$TMP/bfc.out" | sed 's/^/      /'; fi
# Fill one output at a time: each must still be refused while any other holds a placeholder.
fill() { [ -f "$1" ] && sed -i.bak 's/FILL-IN(new_bug.sh)/filled:/g' "$1" && rm -f "$1.bak"; }
fill "$REC"
(cd "$W" && bash scripts/tests/check_bug_filing_complete.sh) > "$TMP/bfc1.out" 2>&1 \
  && no "check_bug_filing_complete.sh ignores placeholders left in the reproducer and the fragment" \
  || { grep -q 'unfilled.*repro.con' "$TMP/bfc1.out" && grep -q "unfilled.*$(basename "${FRAG:-x}")" "$TMP/bfc1.out" \
       && ok "a filled record still fails while the reproducer and fragment hold placeholders" \
       || no "the placeholder leg did not name the reproducer and the fragment"; }
fill "$W/tests/regressions/bug${NEXT}_scaffold_control_a_placeholder_defect/repro.con"; fill "${FRAG:-/nonexistent}"
printf '\nFILL-IN(new_bug.sh) left in the record only\n' >> "${REC:-/dev/null}"
(cd "$W" && bash scripts/tests/check_bug_filing_complete.sh) > "$TMP/bfc3.out" 2>&1 \
  && no "check_bug_filing_complete.sh ignores a placeholder left in the record alone" \
  || { grep -q "unfilled.*$(basename "${REC:-x}")" "$TMP/bfc3.out" && ok "a placeholder left in the record alone is refused" \
       || no "a record-only placeholder failed the gate for another reason"; }
[ -n "$REC" ] && grep -v 'left in the record only' "$REC" > "$TMP/rec" && cat "$TMP/rec" > "$REC"
if (cd "$W" && bash scripts/tests/check_bug_filing_complete.sh) > "$TMP/bfc2.out" 2>&1; then
  ok "once filled, the new bug passes check_bug_filing_complete.sh"
else no "the filled scaffold fails check_bug_filing_complete.sh:"; grep FAIL "$TMP/bfc2.out" | sed 's/^/      /'; fi

echo "=== 4. refusals burn no number ==="
refuse() { # name, expected stderr substring, command...
  local name="$1" want="$2"; shift 2
  local b a; b="$(git -C "$W" ls-remote origin 'refs/bug-reservations/*' | wc -l)"
  if "$@" > "$TMP/ref.out" 2>&1; then no "$name: accepted"
  elif grep -qF -- "$want" "$TMP/ref.out"; then
    a="$(git -C "$W" ls-remote origin 'refs/bug-reservations/*' | wc -l)"
    [ "$a" = "$b" ] && ok "$name: refused, no number reserved" || no "$name: refused, but a number was reserved"
  else no "$name: refused for another reason: $(tr '\n' ' ' < "$TMP/ref.out" | cut -c1-200)"; fi
}
refuse "a title stating a status" "states a status" bash "$NB" "FIXED crash in the lexer" "${BA[@]}"
refuse "a leading status word" "states a status" bash "$NB" "open handle leak" "${BA[@]}"
refuse "an unknown class" "is not a row" bash "$NB" "some defect" --class 999 "${BA[@]}"
refuse "a bad --silent" "yes or no" bash "$NB" "some defect" "${BN[@]}" --silent maybe
refuse "a title a changelog fragment cannot carry" "changelog stamp:" bash "$NB" "parser mishandles changelog stamp: text" "${BA[@]}"
printf '# not a fragment heading\n' > "$W/changelog.d/2026-01-01-broken.md"
refuse "an already-malformed changelog.d" "already malformed" bash "$NB" "some defect" "${BA[@]}"
rm -f "$W/changelog.d/2026-01-01-broken.md"
# A failure after the files are written (here: the fragment cannot be created) removes all of them and
# leaves the views as they were. Root ignores the permission bits, so the leg reports itself skipped.
recs_before="$(ls "$W/docs/bugs" | sort)"; regs_before="$(ls "$W/tests/regressions" | sort)"
chmod 555 "$W/changelog.d"
if [ -w "$W/changelog.d" ]; then
  echo "  skip a failed write rolls back (the directory stays writable for this user)"
elif bash "$NB" "rollback control defect" "${BA[@]}" > "$TMP/rb.out" 2>&1; then
  no "filing succeeded although changelog.d/ was read-only"
elif [ "$(ls "$W/docs/bugs" | sort)" = "$recs_before" ] && [ "$(ls "$W/tests/regressions" | sort)" = "$regs_before" ] \
     && python3 -I "$W/scripts/gen/bug_ledger.py" --check --root "$W" >/dev/null 2>&1 && grep -q 'removed the files' "$TMP/rb.out"; then
  ok "a failed write after the record exists removes every file it wrote and leaves the views consistent"
else no "a failed filing left files or stale views behind: $(tr '\n' ' ' < "$TMP/rb.out" | cut -c1-200)"; fi
chmod 755 "$W/changelog.d"
# A dangling symlink where the reproducer directory would go is an existing path: refused by the
# scaffold's own existence check (no generator reads that path), and left in place.
nx="$(git -C "$W" ls-remote origin 'refs/bug-reservations/*' | sed -nE 's#.*/([0-9]+)$#\1#p' | sort -n | tail -1)"
nx="$(printf '%03d' $((10#$nx + 1)))"
SL="$W/tests/regressions/bug${nx}_symlink_control"
ln -s "$TMP/missing/dir" "$SL"
if bash "$NB" "symlink control" "${BA[@]}" > "$TMP/sl.out" 2>&1; then no "filing over a dangling symlink succeeded"
elif [ -L "$SL" ] && grep -q "bug${nx}_symlink_control already exists" "$TMP/sl.out" \
     && ! ls "$W/docs/bugs/${nx}_"* >/dev/null 2>&1; then
  ok "a dangling symlink at an output path is refused by the existence check and left untouched"
else no "a dangling symlink at the reproducer path was not refused cleanly: $(tr '\n' ' ' < "$TMP/sl.out" | cut -c1-200)"; fi
rm -f "$SL"
if [ "$MIGRATED_HERE" = 0 ]; then
  git init -q -b main "$RAW" && git -C "$RAW" remote add origin "$O"
  refuse "an unmigrated tree" "not migrated" bash "$RAW/scripts/gen/new_bug.sh" "some defect" "${BA[@]}"
fi
mkdir -p "$TMP/oldhooks" && printf '#!/bin/sh\nexit 0\n' > "$TMP/oldhooks/pre-push" && chmod +x "$TMP/oldhooks/pre-push"
G config core.hooksPath "$TMP/oldhooks"
refuse "a pre-push hook without the reservation tier" "no reservation tier" bash "$NB" --number-only "hook control"
G config core.hooksPath .githooks
if bash "$NB" --number-only "through the real hook" > "$TMP/hook.out" 2>&1; then
  ok "a reservation passes through the repository's own pre-push hook"
else no "a reservation through .githooks/pre-push failed: $(tr '\n' ' ' < "$TMP/hook.out" | cut -c1-200)"; fi
G config --unset core.hooksPath

echo "=== 5. the pre-push reservation tier accepts only an empty orphan commit ==="
Z=0000000000000000000000000000000000000000
E="$(G commit-tree "$(G hash-object -t tree /dev/null)" -m r)"
T="$(G rev-parse HEAD)"                      # a real tree, with history
P="$(G commit-tree "$(G hash-object -t tree /dev/null)" -p "$T" -m r)"   # empty tree, with a parent
hook() { (cd "$W" && printf '%s\n' "$1" | bash .githooks/pre-push origin "$O") > "$TMP/hook.log" 2>&1; }
hook "refs/x $E refs/bug-reservations/900 $Z" && grep -q 'no gates' "$TMP/hook.log" \
  && ok "hook: an empty orphan commit on refs/bug-reservations/NNN passes without gates" \
  || no "hook: the reservation shape was not accepted: $(tr '\n' ' ' < "$TMP/hook.log" | cut -c1-200)"
hook "refs/x $T refs/bug-reservations/901 $Z" && no "hook: a commit carrying a tree was accepted as a reservation" \
  || { grep -q 'reservation tier' "$TMP/hook.log" && ok "hook: a commit with content on a reservation ref is refused" \
       || no "hook: content on a reservation ref failed for another reason"; }
hook "refs/x $P refs/bug-reservations/902 $Z" && no "hook: an empty commit with a parent was accepted as a reservation" \
  || { grep -q 'reservation tier' "$TMP/hook.log" && ok "hook: a reservation commit with history is refused" \
       || no "hook: a parented reservation failed for another reason"; }
hook "refs/x $E refs/bug-reservations/notanumber $Z" && no "hook: a non-numeric reservation ref was accepted" \
  || { grep -q 'reservation tier' "$TMP/hook.log" && ok "hook: a non-numeric reservation ref is refused" \
       || no "hook: a non-numeric reservation ref failed for another reason"; }
hook "refs/x $(G hash-object -t tree /dev/null) refs/bug-reservations/903 $Z" && no "hook: a bare empty tree object was accepted as a reservation" \
  || { grep -q 'reservation tier' "$TMP/hook.log" && ok "hook: a non-commit object on a reservation ref is refused" \
       || no "hook: a tree object on a reservation ref failed for another reason"; }
hook "refs/x $E refs/bug-reservations/904 $Z"$'\n'"refs/heads/w $T refs/heads/wip/w $Z" \
  && grep -q 'wip/\* backup push' "$TMP/hook.log" && ! grep -q 'no gates' "$TMP/hook.log" \
  && ok "hook: a reservation pushed together with a branch does not exempt the branch (it takes the branch's tier)" \
  || no "hook: a mixed reservation + wip push was not handled by the wip tier: $(tr '\n' ' ' < "$TMP/hook.log" | cut -c1-200)"

echo "=== 6. the discovery attribution (bug 300) ==="
refuse "a filing that names no discovery batch" "no discovery batch given" bash "$NB" "unattributed defect" --silent no
refuse "a batch without --silent" "--silent yes|no is required" bash "$NB" "unattributed defect" "${BN[@]}"
refuse "--batch text no row has" "matches 0 rows" bash "$NB" "x defect" --silent no --batch "no batch has this probe text"
refuse "--batch text several rows have" "not one" bash "$NB" "x defect" --silent no --batch "e"
refuse "--batch and --new-batch together" "both given" bash "$NB" "x defect" --silent no --batch "scaffold control hunt" "${BN[@]}"
refuse "--new-batch with an unknown kind" "is not one of" bash "$NB" "x defect" --silent no --new-batch probing --probe p --effort "1 probe"
refuse "--new-batch without --effort" "needs --probe and --effort" bash "$NB" "x defect" --silent no --new-batch hunt --probe p
refuse "a counting batch from 2026-10-22 on without an effort size" "records its effort" \
  env NEW_BUG_DATE=2026-10-26 bash "$NB" "x defect" --silent no --new-batch hunt --probe p --effort unrecorded
refuse "a class tag on an attribution-only batch" "marks a counting batch" bash "$NB" "x defect" --silent no --new-batch review --probe "class 1: p" --effort "1 probe"
cp "$W/ROADMAP.md" "$TMP/roadmap.bak"
awk '{ print } /BEGIN GENERATED: bug dashboard/ { print "a hand edit" }' "$TMP/roadmap.bak" > "$W/ROADMAP.md"
refuse "a stale bug dashboard" "stale or refuses" bash "$NB" "x defect" "${BA[@]}"
cat "$TMP/roadmap.bak" > "$W/ROADMAP.md"
# --batch joins an existing row: the number goes in its bugs cell, and in its silent cell only when silent.
if bash "$NB" "Joined control defect" --silent no --batch "scaffold control hunt" > "$TMP/join.out" 2>&1; then
  J="$(sed -nE 's/^new_bug: filed bug ([0-9]{3})$/\1/p' "$TMP/join.out")"
  if [ -n "$J" ] && [ "$(awk -F'\t' -v b="$NEXT,$J" -v s="$NEXT" '$3 == "scaffold control hunt" && $5 == b && $6 == s' "$DISC" | wc -l | tr -d ' ')" = 1 ] && dash_ok; then
    ok "--batch added loud bug $J to the existing row's bugs cell only, and the dashboard accepts it"
  else no "--batch did not add bug ${J:-?} to the row 'scaffold control hunt' as '$NEXT,${J:-?}' (silent '$NEXT'), or the dashboard refuses it"; fi
else no "filing with --batch failed: $(tr '\n' ' ' < "$TMP/join.out" | cut -c1-240)"; fi
# A failure after the reservation (here the dashboard cannot write ROADMAP.md): every file and the
# discovery row are rolled back, and the printed --number command files the same bug under the held
# number with no second reservation. Root ignores the permission bits, so the leg reports itself skipped.
disc_before="$(cksum < "$DISC")"
chmod 444 "$W/ROADMAP.md"
if [ -w "$W/ROADMAP.md" ]; then
  echo "  skip a failure after the reservation rolls back and resumes (ROADMAP.md stays writable for this user)"
  chmod 644 "$W/ROADMAP.md"
elif bash "$NB" "Resume control defect" --silent yes --batch "scaffold control hunt" > "$TMP/rs.out" 2>&1; then
  chmod 644 "$W/ROADMAP.md"; no "filing succeeded although ROADMAP.md was read-only"
else
  chmod 644 "$W/ROADMAP.md"
  R="$(sed -nE 's/^new_bug: bug ([0-9]{3}) stays reserved and unfiled.*/\1/p' "$TMP/rs.out")"
  CMD="$(sed -nE 's/^  (scripts\/gen\/new_bug\.sh --number .*)$/\1/p' "$TMP/rs.out")"
  if [ -n "$R" ] && [ "$(cksum < "$DISC")" = "$disc_before" ] && ! ls "$W/docs/bugs/${R}_"* >/dev/null 2>&1 \
     && git -C "$W" ls-remote --exit-code origin "refs/bug-reservations/$R" >/dev/null && [ -n "$CMD" ] \
     && python3 -I "$W/scripts/gen/bug_ledger.py" --check --root "$W" >/dev/null 2>&1; then
    ok "a failure after reserving bug $R removed its files, restored discovery.tsv and the views, and printed the --number command"
  else no "a failure after the reservation left the tree changed or printed no --number command: $(tr '\n' ' ' < "$TMP/rs.out" | cut -c1-300)"; fi
  b="$(git -C "$W" ls-remote origin 'refs/bug-reservations/*' | wc -l)"
  if [ -n "$CMD" ] && (cd "$W" && eval "bash $CMD") > "$TMP/rs2.out" 2>&1; then
    a="$(git -C "$W" ls-remote origin 'refs/bug-reservations/*' | wc -l)"
    if [ "$a" = "$b" ] && ls "$W/docs/bugs/${R}_resume_control_defect.md" >/dev/null 2>&1 \
       && [ "$(awk -F'\t' -v r="$R" '$3 == "scaffold control hunt" && ("," $5 ",") ~ ("," r ",") && ("," $6 ",") ~ ("," r ",")' "$DISC" | wc -l | tr -d ' ')" = 1 ] \
       && dash_ok; then
      ok "the printed --number command filed bug $R under its held reservation (no new number), silent in its row"
    else no "the --number rerun did not file bug ${R:-?} cleanly (reservations $b -> $a): $(tr '\n' ' ' < "$TMP/rs2.out" | cut -c1-240)"; fi
  else no "the printed --number command failed: $(tr '\n' ' ' < "$TMP/rs2.out" 2>/dev/null | cut -c1-240)"; fi
fi
refuse "--number for a number nobody reserved" "is not reserved" bash "$NB" --number 990 "x defect" "${BA[@]}"
refuse "--number for a number that has a record" "already has a record" bash "$NB" --number "$NEXT" "x defect" "${BA[@]}"
refuse "--number for a reservation new_bug.sh did not make" "not a reservation new_bug.sh can file under" \
  bash "$NB" --number "$(printf '%03d' $((M + 40)))" "x defect" "${BA[@]}"
# Hand-made reservations at fresh numbers: another user's held number, and one a run is filing.
res_at() { # number, state, owner
  local c; c="$(G commit-tree "$(G hash-object -t tree /dev/null)" -m "bug $1 $2: control" -m "by $3@host at 2026-01-02T00:00:00Z, nonce $1$2")"
  G push -q origin "$c:refs/bug-reservations/$1"
}
OTHER=970; FILING=971; RACE=972
res_at $OTHER reserved other@example.invalid && res_at $FILING filing gate@example.invalid && res_at $RACE reserved gate@example.invalid
refuse "--number for another user's reservation" "not by this user" bash "$NB" --number $OTHER "x defect" "${BA[@]}"
refuse "--number for a number another run is filing" "is being filed" bash "$NB" --number $FILING "x defect" "${BA[@]}"
# Two runs claiming one held number, from two clones: the lease lets exactly one file it. As in leg 2
# the interleaving is the scheduler's: without the lease this leg failed 2 runs of 3 (it can catch a
# double claim, it cannot force one). The 'is being filed' refusal above is the deterministic half.
git clone -q -b main "$O" "$TMP/c2" && git -C "$TMP/c2" config user.email gate@example.invalid && git -C "$TMP/c2" config user.name gate
(cd "$W" && bash "$NB" --number $RACE "Race control A" "${BA[@]}") > "$TMP/raceA.out" 2>&1 & pa=$!
(cd "$TMP/c2" && bash scripts/gen/new_bug.sh --number $RACE "Race control B" "${BA[@]}") > "$TMP/raceB.out" 2>&1 & pb=$!
ra=0; rb=0; wait $pa || ra=$?; wait $pb || rb=$?
filed="$(ls "$W/docs/bugs/${RACE}_"* "$TMP/c2/docs/bugs/${RACE}_"* 2>/dev/null | wc -l | tr -d ' ')"
if { [ "$ra" = 0 ] && [ "$rb" != 0 ]; } || { [ "$ra" != 0 ] && [ "$rb" = 0 ]; }; then
  if [ "$filed" = 1 ] && grep -qE 'claimed it first|is being filed' "$TMP/raceA.out" "$TMP/raceB.out"; then
    ok "two concurrent --number runs on one held number: exactly one filed it, the other was refused"
  else no "two concurrent --number runs: $filed records for bug $RACE, or the loser gave another reason: $(cat "$TMP/raceA.out" "$TMP/raceB.out" | tr '\n' ' ' | cut -c1-300)"; fi
else no "two concurrent --number runs on bug $RACE: exit statuses $ra and $rb, not one success: $(cat "$TMP/raceA.out" "$TMP/raceB.out" | tr '\n' ' ' | cut -c1-300)"; fi
# The automatic filing paths print commands that must pass as printed (bug 300): the fuzz farm's
# promote output (pinned byte-exact in its golden) and CI triage's skeleton command, once its SILENT
# placeholder is chosen (the bare placeholder is refused before any reservation: leg 4's bad --silent).
FC="$(sed -n 's/^\(scripts\/gen\/new_bug\.sh .*--new-batch .*\)$/\1/p' "$ROOT_DIR/tests/fixtures/fuzz_farm/promote.golden")"
if [ -n "$FC" ] && (cd "$W" && eval "bash $FC") > "$TMP/farm.out" 2>&1; then
  ok "the fuzz farm's printed new_bug.sh command files a bug as printed"
else no "the fuzz farm's printed new_bug.sh command was refused or not found: $(tr '\n' ' ' < "$TMP/farm.out" 2>/dev/null | cut -c1-240)"; fi
TC="$(python3 -I - "$ROOT_DIR" <<'PY'
import os, re, sys
src = open(os.path.join(sys.argv[1], "scripts", "ci", "triage.py")).read()
m = re.search(r"f'bash (scripts/gen/new_bug\.sh .*?)'\s*\n\s*f'(--new-batch .*?)'", src)
cmd = (m.group(1) + m.group(2)) if m else ""
for k, v in (('{r["gate"]}', "check_x.sh"), ("{first}", "a failing leg"), ('{run.get("databaseId")}', "1"),
             ("--silent SILENT", "--silent no")):
    cmd = cmd.replace(k, v)
print(cmd if "{" not in cmd else "")
PY
)"
if [ -n "$TC" ] && (cd "$W" && eval "bash $TC") > "$TMP/triage.out" 2>&1; then
  ok "CI triage's printed new_bug.sh command files a bug as printed"
else no "CI triage's printed new_bug.sh command was refused or not found ('$TC'): $(tr '\n' ' ' < "$TMP/triage.out" 2>/dev/null | cut -c1-240)"; fi
refuse "--number with --number-only" "--number-only reserves a new one" bash "$NB" --number-only --number 990 "x defect"

echo "NEW-BUG: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ] || exit 1
