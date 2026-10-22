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
#      refs/bug-reservations/NNN and refuses anything else there.
#
# Copies the ledger inputs the way check_ledgers.sh does (migrating the copy when this tree is not
# migrated yet), so it needs no compiler build. Runs in seconds.
set -uo pipefail
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
cp scripts/gen/bug_ledger.py scripts/gen/changelog.py scripts/gen/migrate_ledgers.py scripts/gen/new_bug.sh "$W/scripts/gen/"
cp scripts/tests/audit_bug_corpus.sh scripts/tests/check_bug_filing_complete.sh \
   scripts/tests/run_tests.sh scripts/tests/run_ci_gates_local.sh "$W/scripts/tests/"
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
# One title for all six: identical requests in the same second must still build distinct commits.
for i in 1 2 3 4 5 6; do bash "$NB" --number-only "race" > "$TMP/race.$i" 2>&1 & pids="$pids $!"; done
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
if bash "$NB" "Scaffold control: a placeholder defect" --class 1 --silent yes > "$TMP/file.out" 2>&1; then
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
refuse "a title stating a status" "states a status" bash "$NB" "FIXED crash in the lexer"
refuse "a leading status word" "states a status" bash "$NB" "open handle leak"
refuse "an unknown class" "is not a row" bash "$NB" "some defect" --class 999
refuse "a bad --silent" "yes or no" bash "$NB" "some defect" --silent maybe
refuse "a title a changelog fragment cannot carry" "changelog stamp:" bash "$NB" "parser mishandles changelog stamp: text"
printf '# not a fragment heading\n' > "$W/changelog.d/2026-01-01-broken.md"
refuse "an already-malformed changelog.d" "already malformed" bash "$NB" "some defect"
rm -f "$W/changelog.d/2026-01-01-broken.md"
if [ "$MIGRATED_HERE" = 0 ]; then
  git init -q -b main "$RAW" && git -C "$RAW" remote add origin "$O"
  refuse "an unmigrated tree" "not migrated" bash "$RAW/scripts/gen/new_bug.sh" "some defect"
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

echo "NEW-BUG: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ] || exit 1
