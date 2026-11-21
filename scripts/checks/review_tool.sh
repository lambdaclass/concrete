#!/usr/bin/env bash
# R-0544 — the review tool (scripts/review/review.sh, verify.sh, split_diff.py)
# does what docs/REVIEW.md says it does.
#
# No API key is needed: the reviewer is a fake `codex` that answers according to
# MARK_* markers on the ADDED lines of the diff it receives on stdin, and logs
# every call (prompt kind, stdin size, files) so the legs can check what was sent.
# All git work happens in throwaway repositories with a bare repository as
# `origin`, so the notes push, including the non-fast-forward path, runs for real.
#
# Legs: verdict parsing, note writing and its exact format, notes push and the
# non-fast-forward merge, cumulative clean coverage, findings carryover, splitting (file groups, size limit, hunk split, --pieces N),
# the content-filter refusal retry (once, neutral, smaller, then `blocked`), the
# round cap (distinct exit code, no extra round), verify.sh, and the pre-push
# hook's review-notes tier (a notes-only push runs no gates).

set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
REVIEW="$ROOT_DIR/scripts/review/review.sh"
VERIFY="$ROOT_DIR/scripts/review/verify.sh"
SPLIT="$ROOT_DIR/scripts/review/split_diff.py"

PASS=0
FAIL=0
ok() { echo "  ok   $1"; PASS=$((PASS+1)); }
no() { echo "  FAIL $1"; FAIL=$((FAIL+1)); }
check() { # check <label> <command...>: the command must succeed
  local label="$1"; shift
  if "$@"; then ok "$label"; else no "$label"; fi
}

T="$(mktemp -d)"
trap 'rm -rf "$T"' EXIT

# Isolate git from the caller's repository (a hook exports GIT_DIR; ISS-0015) and from the user's
# config (hooks, signing, identity).
source "$ROOT_DIR/scripts/checks/lib/git_sandbox.sh" || exit 2
git_sandbox "$T/gitconfig" || exit 2
cat >"$GIT_CONFIG_GLOBAL" <<'EOF'
[user]
	name = Review Gate
	email = review-gate@example.invalid
[init]
	defaultBranch = main
[advice]
	detachedHead = false
EOF

# ------------------------------------------------------------------ fake codex
FAKE_LOG="$T/fake"
mkdir -p "$FAKE_LOG" "$T/bin"
export FAKE_LOG
# Bug 209's legs prove review.sh OVERRIDES the caller's setting: the gate runs with optional locks
# explicitly on, so a value inherited from whoever launched it cannot satisfy them.
export GIT_OPTIONAL_LOCKS=1
cat >"$T/bin/codex" <<'EOF'
#!/usr/bin/env bash
# Fake reviewer: `codex exec --sandbox read-only ... -o <file> <prompt>`, diff on stdin.
set -uo pipefail
[ "${1:-}" = exec ] || { echo "fake codex: expected exec" >&2; exit 64; }
shift
out="" prompt="" sandbox=""
while [ $# -gt 0 ]; do
  case "$1" in
    -o|--output-last-message) out="$2"; shift 2 ;;
    -s|--sandbox) sandbox="$2"; shift 2 ;;
    -C|--cd|--color|-m|--model|-c) shift 2 ;;
    --ephemeral|--skip-git-repo-check) shift ;;
    *) prompt="$1"; shift ;;
  esac
done
n=$(( $(ls "$FAKE_LOG" | grep -c '^stdin\.' || true) + 1 ))
cat >"$FAKE_LOG/stdin.$n"
printf '%s' "$prompt" >"$FAKE_LOG/prompt.$n"
kind=standard
grep -q "routine code review" "$FAKE_LOG/prompt.$n" && kind=neutral
files="$(grep '^diff --git ' "$FAKE_LOG/stdin.$n" | sed 's#.* b/##' | sort -u | tr '\n' ',')"
echo "$n $kind sandbox=$sandbox bytes=$(wc -c <"$FAKE_LOG/stdin.$n" | tr -d ' ') files=$files" >>"$FAKE_LOG/calls"
echo "$n optional_locks=${GIT_OPTIONAL_LOCKS:-unset}" >>"$FAKE_LOG/locks"
added="$(grep '^+' "$FAKE_LOG/stdin.$n" || true)"
has() { grep -q "$1" <<<"$added"; }
reply() { printf '%s\n' "$1" >"$out"; }
if has MARK_REFUSE_ALWAYS || { has MARK_REFUSE_STANDARD && [ "$kind" = standard ]; }; then
  reply "I'm sorry, but I can't help with that request because it was flagged by our cybersecurity policy."
elif has MARK_ERROR; then
  echo "stream error: 500" >&2; exit 1
elif has MARK_FINDING; then
  reply "- a.txt:1: the counter is never reset
VERDICT: findings"
elif has MARK_NOVERDICT; then
  reply "I looked at it and it seems fine."
elif has MARK_TRAILING; then
  reply "VERDICT: clean
Also, one more thought."
elif has MARK_BOLD; then
  reply "Nothing to report.
**VERDICT: clean**"
elif has MARK_MENTION_FILTER; then
  reply "The content filter detection and the cybersecurity wording look right.
VERDICT: clean"
else
  reply "Nothing to report.
VERDICT: clean"
fi
EOF
chmod +x "$T/bin/codex"
export REVIEW_CODEX_BIN="$T/bin/codex"

calls() { [ -f "$FAKE_LOG/calls" ] && wc -l <"$FAKE_LOG/calls" | tr -d ' ' || echo 0; }
reset_fake() { rm -f "$FAKE_LOG"/*; }

# ------------------------------------------------------------------ repos
git init -q --bare "$T/origin.git"
git clone -q "$T/origin.git" "$T/w" 2>/dev/null
W="$T/w"
g() { git -C "$W" "$@"; }
commit_file() { # commit_file <path> <content> <message>
  mkdir -p "$(dirname "$W/$1")"
  printf '%s\n' "$2" >"$W/$1"
  g add -A && g commit -qm "$3"
}
commit_file README.md "base" "base"
printf '.review/\n' >"$W/.gitignore"
g add .gitignore && g commit -qm "ignore .review"
g push -q origin main
review() { (cd "$W" && bash "$REVIEW" "$@"); }
note() { g notes --ref=refs/notes/concrete-review-v2 show "${1:-HEAD}" 2>/dev/null; }
field() { note "${2:-HEAD}" | sed -n "/^$/q; s/^$1: //p"; }

echo "=== round 1: full diff, findings, note written and pushed ==="
g checkout -qb feature
commit_file old.txt "MARK_FINDING in the first change" "first change"
review >"$T/out1" 2>&1; rc=$?
check "findings exit 1 (got $rc)" test "$rc" -eq 1
check "one reviewer call" test "$(calls)" -eq 1
check "reviewer runs with --sandbox read-only" grep -q "sandbox=read-only" "$FAKE_LOG/calls"
# Bug 209: every reviewer call runs without git's optional locks, so a reviewer command stopped
# mid-`git status` cannot leave the caller's worktree with a stale index.lock.
check "reviewer runs with GIT_OPTIONAL_LOCKS=0" grep -q " optional_locks=0$" "$FAKE_LOG/locks"
check "no reviewer call ran with optional locks" bash -c '! grep -v " optional_locks=0$" "$1" | has_match .' _ "$FAKE_LOG/locks"
check "round 1 prompt carries the verdict rule" grep -q "^VERDICT: findings$" "$FAKE_LOG/prompt.1"
check "round 1 reviews the full diff from the merge-base" grep -q "files=old.txt,$" "$FAKE_LOG/calls"
R1="$(g rev-parse HEAD)"
BASE="$(g merge-base HEAD origin/main)"
FORMAT_RE='^verdict: (clean|findings|blocked)
rounds: [0-9]+
reviewer: codex
utc: [0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z
base: [0-9a-f]{40}
pieces: [0-9]+
diff-start: [0-9a-f]{40}
findings-note: [0-9a-f]{40}

'
n1="$(note "$R1")"
if [[ "$n1" =~ $FORMAT_RE ]]; then ok "findings note has the expected fields, in order"; else no "note format: $n1"; fi
check "note: verdict findings" test "$(field verdict)" = findings
check "note: rounds 1" test "$(field rounds)" = 1
check "note: base is the merge-base" test "$(field base)" = "$BASE"
check "note: pieces 1" test "$(field pieces)" = 1
check "raw output at .review/<sha>-1.md" test -s "$W/.review/$R1-1.md"
check "findings recorded in the verdict note" \
  bash -c "git -C '$W' notes --ref=refs/notes/concrete-review-v2 show $R1 | has_match 'counter is never reset'"
check "notes ref pushed to origin" test "$(git -C "$T/origin.git" rev-parse refs/notes/concrete-review-v2)" = "$(g rev-parse refs/notes/concrete-review-v2)"
check ".review/ stays untracked" test -z "$(g status --porcelain)"
(cd "$W" && bash "$VERIFY" HEAD >/dev/null); rc=$?
check "verify.sh: findings is not clean (exit 1, got $rc)" test "$rc" -eq 1

echo "=== same HEAD again: nothing to review ==="
review >/dev/null 2>&1; rc=$?
check "re-review of a reviewed HEAD exits 4 (got $rc)" test "$rc" -eq 4
check "and calls no reviewer" test "$(calls)" -eq 1

echo "=== --head: the callers' contract (train.sh, ready.sh pass --base --head) ==="
review --head HEAD >/dev/null 2>&1; rc=$?
check "--head at the checkout's HEAD is accepted (reviewed HEAD exits 4, got $rc)" test "$rc" -eq 4
review --base main --head "$(g rev-parse HEAD)" >/dev/null 2>&1; rc=$?
check "--base --head <sha> as train.sh calls it is accepted (got $rc)" test "$rc" -eq 4
review --head main >"$T/out-head" 2>&1; rc=$?
check "--head other than the checkout refuses (exit 2, got $rc)" test "$rc" -eq 2
check "and says why" grep -q "but the checkout is at" "$T/out-head"
review --head "" >/dev/null 2>&1; rc=$?
check "an empty --head refuses (exit 2, got $rc)" test "$rc" -eq 2
check "and calls no reviewer" test "$(calls)" -eq 1

echo "=== round 2: full review with the previous findings ==="
commit_file old.txt "fixed" "fix the counter"
commit_file new.txt "a second change" "second change"
review >"$T/out2" 2>&1; rc=$?
check "clean exit 0 (got $rc)" test "$rc" -eq 0
check "round 2 includes the full branch changes" grep -q "^2 standard .*files=new.txt,old.txt,$" "$FAKE_LOG/calls"
check "round 2 diff does not resend the round-1 hunk" bash -c "! grep -q '^+MARK_FINDING' '$FAKE_LOG/stdin.2'"
check "round 2 prompt describes full scope" grep -q "complete change of a branch" "$FAKE_LOG/prompt.2"
check "round 2 prompt carries the previous findings" grep -q "counter is never reset" "$FAKE_LOG/prompt.2"
check "note: rounds 2, verdict clean" test "$(field rounds)/$(field verdict)" = "2/clean"
(cd "$W" && bash "$VERIFY" HEAD >"$T/v" 2>&1); rc=$?
check "verify.sh: clean exit 0 (got $rc)" test "$rc" -eq 0
check "verify.sh prints the verdict" grep -q "clean (round 2" "$T/v"
(cd "$W" && bash "$VERIFY" --required-base "$BASE" HEAD >"$T/vr" 2>&1); rc=$?
check "verify.sh accepts a note based at the required base" test "$rc" -eq 0
(cd "$W" && bash "$VERIFY" --required-base "$R1" HEAD >"$T/vr" 2>&1); rc=$?
check "verify.sh accepts a note based before the required base" test "$rc" -eq 0
(cd "$W" && bash "$VERIFY" --required-base missing-required-base HEAD >"$T/vr" 2>&1); rc=$?
check "verify.sh refuses an unknown required base" test "$rc" -eq 2
check "unknown required base has its own diagnostic" grep -q 'required base is not a commit' "$T/vr"
(cd "$W" && bash "$VERIFY" "$R1" >/dev/null 2>&1); rc=$?
check "verify.sh judges the exact commit, not the branch (R1 still 1)" test "$rc" -eq 1
review >/dev/null 2>&1; rc=$?
check "an already-clean HEAD reports clean without a call (exit 0)" test "$rc" -eq 0 -a "$(calls)" -eq 2

echo "=== ISS-0098: clean delta requires the complete predecessor chain ==="
g checkout -qb chain-controls
FULL_SHA="$(g rev-parse HEAD)"
FULL_NOTE="$(note)"
FULL_BLOB="$(g notes --ref=concrete-review-v2 list HEAD)"
commit_file delta.txt delta "first clean delta"
review >"$T/delta" 2>&1; rc=$?
check "verified clean predecessor allows a delta" test "$rc" -eq 0
check "delta prompt identifies the clean start" grep -q "change since commit $FULL_SHA" "$FAKE_LOG/prompt.3"
check "delta contains only the new change" grep -q 'files=delta.txt,$' "$FAKE_LOG/calls"
check "delta note records the exact predecessor commit and blob" \
  test "$(field diff-start)/$(field predecessor-note)" = "$FULL_SHA/$FULL_BLOB"
commit_file delta2.txt delta2 "second clean delta"
review >"$T/delta" 2>&1; rc=$?
check "second clean delta is published" test "$rc" -eq 0
verify_head() { (cd "$W" && bash "$VERIFY" --required-base "$BASE" HEAD); }
verify_head >"$T/chain" 2>&1; rc=$?
check "two clean deltas verify back to the full review" test "$rc" -eq 0

g notes --ref=concrete-review-v2 remove "$FULL_SHA" >/dev/null 2>&1
verify_head >"$T/chain" 2>&1; rc=$?
check "missing indirect predecessor fails (exit $rc)" test "$rc" -eq 2
check "missing predecessor diagnostic identifies the missing commit" grep -q "$FULL_SHA unverified: no note" "$T/chain"

g notes --ref=concrete-review-v2 add -f -m "$(printf '%s\n' "$FULL_NOTE" | sed 's/^utc: .*/utc: 2000-01-01T00:00:00Z/')" "$FULL_SHA" 2>/dev/null
verify_head >"$T/chain" 2>&1; rc=$?
check "changed indirect predecessor fails (exit $rc)" test "$rc" -eq 2
check "changed indirect predecessor fails on blob identity" grep -q "predecessor note changed" "$T/chain"
g notes --ref=concrete-review-v2 add -f -C "$FULL_BLOB" "$FULL_SHA" 2>/dev/null

# Give the head delta a matching reference to a genuine findings verdict so
# rejection must come from the verdict, not just a mismatched blob.
CHAIN_NOTE="$(g notes --ref=concrete-review-v2 list HEAD)"
R1_BLOB="$(g notes --ref=concrete-review-v2 list "$R1")"
g notes --ref=concrete-review-v2 add -f -m "$(note | sed -e "s/^diff-start: .*/diff-start: $R1/" \
  -e "s/^predecessor-note: .*/predecessor-note: $R1_BLOB/")" HEAD 2>/dev/null
verify_head >"$T/chain" 2>&1; rc=$?
check "intact predecessor with findings fails (exit $rc)" test "$rc" -eq 2
check "intact predecessor with findings fails for its verdict" grep -q 'predecessor verdict is findings; expected clean' "$T/chain"
g notes --ref=concrete-review-v2 add -f -C "$CHAIN_NOTE" HEAD 2>/dev/null

echo "=== ISS-0099: findings intake requires matching text ==="
g checkout -qb missing-findings "$R1"
commit_file old.txt fixed "fix after missing findings"
R1_NOTE="$(note "$R1")"
R1_HEADER="${R1_NOTE%%$'\n\n'*}"
g notes --ref=concrete-review-v2 add -f -m "$R1_HEADER" "$R1" 2>/dev/null
reset_fake
REVIEW_PUSH=0 review >"$T/missing-findings" 2>&1; rc=$?
check "findings verdict without text refuses before review (exit $rc)" test "$rc" -eq 2 -a "$(calls)" -eq 0
check "missing findings diagnostic identifies the round" grep -q "invalid review evidence for $R1 round 1: .*missing or mismatched findings text" "$T/missing-findings"
g notes --ref=concrete-review-v2 add -f -m "$R1_HEADER"$'\n\nstale text from a different review' "$R1" 2>/dev/null
REVIEW_PUSH=0 review >"$T/missing-findings" 2>&1; rc=$?
check "findings verdict with mismatched text refuses before review (exit $rc)" test "$rc" -eq 2 -a "$(calls)" -eq 0
g notes --ref=concrete-review-v2 add -f -C "$R1_BLOB" "$R1" 2>/dev/null
g checkout -q feature
reset_fake

echo "=== advanced base: full branch fallback preserves rounds and excludes main ==="
g checkout -qb advanced-base main
commit_file member.txt "member reviewed" "member change"
reset_fake
review --max-rounds 3 >"$T/advanced-first" 2>&1; rc=$?
check "member initially reviewed clean" test "$rc/$(field rounds)" = "0/1"
g checkout -q main
commit_file upstream.txt "MARK_FINDING unrelated main change" "main advances"
# No push: explicit base matches the train's invocation.
g checkout -q advanced-base
g merge -q --no-edit main
commit_file member-new.txt "member added after merging main" "member continues"
reset_fake
review --base main --head HEAD --max-rounds 3 >"$T/advanced-second" 2>&1; rc=$?
check "advanced-base round is clean and remains round 2" test "$rc/$(field rounds)" = "0/2"
check "fallback reviews the complete member contribution" grep -q 'files=member-new.txt,member.txt,$' "$FAKE_LOG/calls"
check "fallback excludes unrelated main changes" bash -c '! grep -q upstream.txt "$1"' _ "$FAKE_LOG/stdin.1"
check "fallback prompt describes full branch scope" grep -q 'complete change of a branch' "$FAKE_LOG/prompt.1"
check "fallback is announced" grep -q 'reviewing full base..HEAD diff (round 2)' "$T/advanced-second"
commit_file member-new.txt "member changed again" "member delta"
reset_fake
review --base main --max-rounds 3 >"$T/advanced-third" 2>&1; rc=$?
check "descendant review resumes delta at round 3" test "$rc/$(field rounds)" = "0/3"
check "resumed delta contains only the new member change" grep -q 'files=member-new.txt,$' "$FAKE_LOG/calls"
commit_file cap.txt "no fourth round" "cap probe"
review --base main --max-rounds 3 >"$T/advanced-cap" 2>&1; rc=$?
check "base advance did not reset the cap" test "$rc" -eq 3
check "cap calls no additional reviewer" test "$(calls)" -eq 1
g checkout -q feature
# Restore the fixture base for the independent legs below.
g branch -f main origin/main
reset_fake

echo "=== ISS-0019: prior review ranges cannot hide branch changes ==="
RANGE_NOTES_TIP="$(g rev-parse refs/notes/concrete-review-v2)"
g checkout -q -b range-controls origin/main
RANGE_BASE="$(g rev-parse HEAD)"
commit_file hidden.txt hidden hidden
RANGE_MID="$(g rev-parse HEAD)"
commit_file visible.txt visible visible
range_note() {
  local verdict="$1"
  local rounds="$2"
  local base="$3"
  local finding_field=""
  if [ "$verdict" != clean ]; then
    local findings='ANCESTOR_OPEN_FINDING'
    finding_field="$(printf '\nfindings-note: %s\n\n%s' "$(printf '%s\n' "$findings" | g hash-object --stdin)" "$findings")"
  fi
  g notes --ref=concrete-review-v2 add -f -m "$(printf 'verdict: %s\nrounds: %s\nreviewer: codex\nutc: 2026-11-11T00:00:00Z\nbase: %s\npieces: 1\ndiff-start: %s%s' "$verdict" "$rounds" "$base" "$base" "$finding_field")" HEAD
}
for verdict in clean findings blocked; do
  range_note "$verdict" 1 "$RANGE_MID"
  reset_fake
  REVIEW_PUSH=0 review --base "$RANGE_BASE" >"$T/range" 2>&1; rc=$?
  check "HEAD $verdict intermediate note triggers full review" test "$rc" -eq 0 -a "$(calls)" -eq 1
  check "HEAD $verdict full review includes hidden change" grep -q '^+hidden$' "$FAKE_LOG/stdin.1"
  check "HEAD $verdict fallback preserves rounds" test "$(field rounds)" = 2
done
range_note clean 1 "$RANGE_MID"
commit_file later.txt later later
reset_fake
REVIEW_PUSH=0 review --base "$RANGE_BASE" >"$T/range" 2>&1; rc=$?
check "ancestor intermediate note triggers full review" test "$rc" -eq 0 -a "$(calls)" -eq 1
check "ancestor fallback includes hidden and new changes" grep -q 'files=hidden.txt,later.txt,visible.txt,' "$FAKE_LOG/calls"
# A train merge may have no tree delta from the inadequately reviewed member.
range_note clean 1 "$RANGE_MID"
MERGE_HEAD="$(printf merge | g commit-tree "$(g rev-parse HEAD^{tree})" -p "$(g rev-parse HEAD)" -p "$RANGE_BASE")"
g reset -q --hard "$MERGE_HEAD"
reset_fake
REVIEW_PUSH=0 review --base "$RANGE_BASE" >"$T/range" 2>&1; rc=$?
check "identical-tree train merge falls back instead of nothing-to-review" test "$rc" -eq 0 -a "$(calls)" -eq 1
check "identical-tree fallback includes unreviewed contribution" grep -q '^+hidden$' "$FAKE_LOG/stdin.1"
range_note clean 5 "$RANGE_MID"
reset_fake
REVIEW_PUSH=0 review --base "$RANGE_BASE" >"$T/range" 2>&1; rc=$?
check "invalid range cannot reset exhausted cap" test "$rc" -eq 3 -a "$(calls)" -eq 0
# Stacked notes must not conceal ancestor caps or unresolved findings.
for mode in capped malformed uncapped merge; do
  g checkout -q -B "stack-$mode" "$RANGE_BASE"
  commit_file ancestor.txt ancestor "ancestor-$mode"
  STACK_B="$(g rev-parse HEAD)"
  rounds=5; [ "$mode" != uncapped ] || rounds=2
  range_note findings "$rounds" "$RANGE_BASE"
  commit_file child.txt child "child-$mode"
  rounds=1; [ "$mode" != malformed ] || rounds=0
  range_note clean "$rounds" "$STACK_B"
  if [ "$mode" = merge ]; then
    # Review evidence on both parents must contribute to the cap.
    g checkout -q -b stack-side "$RANGE_BASE"
    commit_file side.txt side side
    range_note clean 1 "$RANGE_BASE"
    g merge -q --no-ff "stack-$mode" -m merge
  fi
  reset_fake
  REVIEW_PUSH=0 review --base "$RANGE_BASE" >"$T/stack" 2>&1; rc=$?
  if [ "$mode" = uncapped ]; then
    check "stacked narrow clean preserves round 3" test "$rc/$(field rounds)" = 0/3 -a "$(calls)" -eq 1
    check "stacked narrow clean forces full review" grep -q '^+ancestor$' "$FAKE_LOG/stdin.1"
    check "stacked narrow clean carries ancestor findings" grep -q ANCESTOR_OPEN_FINDING "$FAKE_LOG/prompt.1"
  else
    check "stacked $mode cannot bypass ancestor cap" test "$rc" -eq 3 -a "$(calls)" -eq 0
    check "stacked $mode fails for cap reason" grep -q 'cap' "$T/stack"
  fi
done
# A valid first blocked round has no completed delta start, but has valid scope.
g checkout -q -B blocked-first "$RANGE_BASE"
commit_file initial.txt initial initial
range_note blocked 1 "$RANGE_BASE"
reset_fake
REVIEW_PUSH=0 review --base "$RANGE_BASE" >"$T/blocked-first" 2>&1; rc=$?
check "covering first blocked round resumes at round 2" test "$rc/$(field rounds)" = 0/2 -a "$(calls)" -eq 1
check "covering first blocked round has no false range diagnostic" test -z "$(grep 'prior review does not cover' "$T/blocked-first")"
# A blocked round contributes its count, but never advances the delta start.
for mode in narrow covering; do
  g checkout -q -B "blocked-$mode" "$RANGE_BASE"
  commit_file prior.txt prior "prior-$mode"
  range_note clean 2 "$RANGE_BASE"
  commit_file pending.txt pending "pending-$mode"
  BLOCKED_BASE="$(g rev-parse HEAD)"
  commit_file blocked.txt blocked "blocked-$mode"
  nb="$RANGE_BASE"; [ "$mode" != narrow ] || nb="$BLOCKED_BASE"
  range_note blocked 1 "$nb"
  commit_file after.txt after "after-$mode"
  reset_fake
  REVIEW_PUSH=0 review --base "$RANGE_BASE" >"$T/blocked-scope" 2>&1; rc=$?
  check "$mode blocked note preserves maximum round count" test "$rc/$(field rounds)" = 0/3
  check "$mode blocked invokes reviewer once" test "$(calls)" -eq 1
  check "$mode blocked reviewer diff exists" test -f "$FAKE_LOG/stdin.1"
  if [ "$mode" = narrow ]; then
    check "narrow blocked note forces full scope despite older clean" grep -q '^+prior$' "$FAKE_LOG/stdin.1"
  else
    for hunk in pending blocked after; do
      check "covering blocked delta includes $hunk" grep -q "^+$hunk$" "$FAKE_LOG/stdin.1"
    done
    check "covering blocked note retains delta scope" bash -c '[ -f "$1" ] && ! grep -q "^+prior$" "$1"' _ "$FAKE_LOG/stdin.1"
    check "covering blocked note has no false range diagnostic" test -z "$(grep 'prior review does not cover' "$T/blocked-scope")"
  fi
done
# A completed covering review supersedes narrow ancestor scope, including at the cap.
for verdict in clean findings; do
  g checkout -q -B "recovered-$verdict" "$RANGE_BASE"
  commit_file narrow.txt narrow "narrow-$verdict"
  RECOVERY_MID="$(g rev-parse HEAD)"
  commit_file old-scope.txt old-scope "old-scope-$verdict"
  range_note clean 2 "$RECOVERY_MID"
  commit_file covering.txt covering "covering-$verdict"
  range_note "$verdict" 3 "$RANGE_BASE"
  reset_fake
  cap=5; expected=4
  if [ "$verdict" = clean ]; then cap=3; expected=0; fi
  REVIEW_PUSH=0 review --base "$RANGE_BASE" --max-rounds "$cap" >"$T/recovered" 2>&1; rc=$?
  check "covering $verdict supersedes narrow ancestor shortcut" test "$rc" -eq "$expected" -a "$(calls)" -eq 0
  if [ "$verdict" = clean ]; then reason='already reviewed clean'; else reason='HEAD already reviewed in round'; fi
  check "covering $verdict shortcut gives intended reason" grep -q "$reason" "$T/recovered"
  commit_file resumed.txt resumed "resumed-$verdict"
  reset_fake
  REVIEW_PUSH=0 review --base "$RANGE_BASE" >"$T/recovered" 2>&1; rc=$?
  check "covering $verdict descendant calls reviewer once" test "$rc" -eq 0 -a "$(calls)" -eq 1
  check "covering $verdict descendant logs diff" test -f "$FAKE_LOG/stdin.1"
  check "covering $verdict descendant reviews new change" grep -q '^+resumed$' "$FAKE_LOG/stdin.1"
  if [ "$verdict" = clean ]; then
    check "covering clean descendant excludes superseded scope" bash -c '[ -f "$1" ] && ! grep -q "^+old-scope$" "$1"' _ "$FAKE_LOG/stdin.1"
  else
    check "findings descendant reviews full scope" grep -q '^+old-scope$' "$FAKE_LOG/stdin.1"
  fi
done
# Coverage on one merge parent must not hide a narrow note on the other.
g checkout -q -B uncovered-side "$RANGE_BASE"
commit_file side-base.txt side-base side-base
SIDE_BASE="$(g rev-parse HEAD)"
commit_file uncovered.txt uncovered uncovered
range_note clean 2 "$SIDE_BASE"
g checkout -q -B covered-side "$RANGE_BASE"
commit_file covered.txt covered covered
range_note clean 2 "$RANGE_BASE"
g merge -q --no-ff uncovered-side -m uncovered-merge
reset_fake
REVIEW_PUSH=0 review --base "$RANGE_BASE" >"$T/uncovered" 2>&1; rc=$?
check "uncovered merge sibling calls reviewer once" test "$rc" -eq 0 -a "$(calls)" -eq 1
check "uncovered merge sibling gives full-scope reason" grep -q 'prior review does not cover' "$T/uncovered"
check "uncovered merge sibling includes already-covered parent" grep -q '^+covered$' "$FAKE_LOG/stdin.1"
check "uncovered merge sibling includes narrow contribution" grep -q '^+side-base$' "$FAKE_LOG/stdin.1"
g checkout -q range-controls
# A real unrelated commit is syntactically valid but cannot cover the required base.
UNRELATED="$(printf unrelated | g commit-tree "$(g rev-parse HEAD^{tree})")"
range_note clean 1 "$UNRELATED"
(cd "$W" && bash "$VERIFY" --required-base "$RANGE_BASE" HEAD >"$T/vr" 2>&1); rc=$?
check "real non-ancestor note base exits 2 with range reason" test "$rc" -eq 2 -a -n "$(grep 'review range does not cover required base' "$T/vr")"
for args in missing empty flag; do
  case "$args" in
    missing) (cd "$W" && bash "$VERIFY" --required-base >"$T/vr" 2>&1) ;;
    empty) (cd "$W" && bash "$VERIFY" --required-base '' HEAD >"$T/vr" 2>&1) ;;
    flag) (cd "$W" && bash "$VERIFY" --required-base --fetch HEAD >"$T/vr" 2>&1) ;;
  esac; rc=$?
  check "$args required-base argument exits 2 with argument reason" test "$rc" -eq 2 -a -n "$(grep -E 'needs a commit|required base is not a commit' "$T/vr")"
done
# Both endpoints exist, but the path between them is shallowly truncated.
range_note clean 1 "$RANGE_BASE"
g clone -q --no-local --depth 1 --branch range-controls "$W" "$T/shallow"
git -C "$T/shallow" fetch -q --depth 1 "$W" "$RANGE_BASE"
git -C "$T/shallow" fetch -q "$W" refs/notes/concrete-review-v2:refs/notes/concrete-review-v2
(cd "$T/shallow" && bash "$VERIFY" --required-base HEAD HEAD >"$T/vr" 2>&1); rc=$?
check "shallow ancestry fails closed with range reason" test "$rc" -eq 2 -a -n "$(grep 'review range does not cover required base' "$T/vr")"
g checkout -q feature
reset_fake

echo "=== verify.sh: missing and malformed notes ==="
commit_file x.txt "x" "unreviewed"
(cd "$W" && bash "$VERIFY" HEAD >/dev/null 2>&1); rc=$?
check "no note: exit 2 (got $rc)" test "$rc" -eq 2
g notes --ref=refs/notes/concrete-review-v2 add -f -m "verdict: clean" HEAD
(cd "$W" && bash "$VERIFY" HEAD >/dev/null 2>&1); rc=$?
check "malformed note (verdict line alone): exit 2 (got $rc)" test "$rc" -eq 2
B40="$(printf '%040d' 0 | tr 0 a)"
g notes --ref=refs/notes/concrete-review-v2 add -f -m "$(printf 'verdict: clean\nrounds: 1\nreviewer: codex\nbase: %s\npieces: 1\ndiff-start: %s\nutc: 2026-10-22T00:00:00Z' "$BASE" "$BASE")" HEAD
(cd "$W" && bash "$VERIFY" HEAD >"$T/vc" 2>&1); rc=$?
check "a round-1 note with utc last verifies clean (exit $rc)" test "$rc" -eq 0
range_note clean 1 "$B40"
(cd "$W" && bash "$VERIFY" --required-base HEAD HEAD >"$T/vr" 2>&1); rc=$?
check "verify.sh refuses a syntactically valid but unknown note base" test "$rc" -eq 2
check "unknown note base fails for range coverage" grep -q 'review range does not cover required base' "$T/vr"
# The landing bot's keys went with it: a verdict carried across a rebase, or a landing order, is no
# longer a key a review note may hold.
for extra in "carried-from: $B40" "order: 1"; do
  g notes --ref=refs/notes/concrete-review-v2 add -f -m "$(printf 'verdict: clean\nrounds: 2\nreviewer: codex\nbase: %s\npieces: 1\nutc: 2026-10-22T00:00:00Z\n%s' "$B40" "$extra")" HEAD
  out="$(cd "$W" && bash "$VERIFY" HEAD 2>&1)"; rc=$?
  check "a note with '${extra%%:*}' is malformed: exit 2, bad ${extra%%:*} (got $rc)" test "$rc" -eq 2 -a -n "$(printf '%s' "$out" | grep -F "malformed review note (bad ${extra%%:*})")"
done
g notes --ref=refs/notes/concrete-review-v2 add -f -m "$(printf 'verdict: clean\nrounds: 2\nreviewer: codex\nutc: 2026-10-22T00:00:00Z\nbase: %s\npieces: 1\nsurprise: 1' "$B40")" HEAD
(cd "$W" && bash "$VERIFY" HEAD >/dev/null 2>&1); rc=$?
check "an unknown key is malformed: exit 2 (got $rc)" test "$rc" -eq 2
g notes --ref=refs/notes/concrete-review-v2 add -f -m "$(printf 'verdict: findings\nverdict: clean\nrounds: 2\nreviewer: codex\nutc: 2026-10-22T00:00:00Z\nbase: %s\npieces: 1' "$B40")" HEAD
(cd "$W" && bash "$VERIFY" HEAD >/dev/null 2>&1); rc=$?
check "a repeated key is malformed: exit 2 (got $rc)" test "$rc" -eq 2
g notes --ref=refs/notes/concrete-review-v2 remove HEAD 2>/dev/null

# The range tests plant invalid notes. Do not let those fixtures enter later publication tests.
g update-ref refs/notes/concrete-review-v2 "$RANGE_NOTES_TIP"

echo "=== verdict parsing ==="
parse_case() { # parse_case <marker> <want-exit> <want-verdict> <label>
  local marker="$1"
  local want="$2"
  local wantv="$3"
  local label="$4"
  local rc
  reset_fake
  g checkout -q main && g checkout -qb "parse-$marker"
  commit_file "p/$marker.txt" "$marker" "case $marker"
  review >/dev/null 2>&1; rc=$?
  check "$label (exit $rc, verdict $(field verdict))" test "$rc" = "$want" -a "$(field verdict)" = "$wantv"
}
parse_case MARK_BOLD 0 clean "markdown-emphasised verdict line is accepted"
parse_case MARK_TRAILING 5 blocked "text after the verdict line is not a verdict"
parse_case MARK_NOVERDICT 5 blocked "a reply with no verdict line is blocked, not clean"
parse_case MARK_ERROR 5 blocked "a reviewer that exits nonzero is blocked"
parse_case MARK_MENTION_FILTER 0 clean "a real review that mentions content filters is not a refusal"

echo "=== content-filter refusal: one neutral retry on smaller pieces ==="
reset_fake
g checkout -q main && g checkout -qb refuse-once
commit_file Concrete/Own.lean "$(printf 'line %s\n' $(seq 1 40))
MARK_REFUSE_STANDARD" "ownership change"
review >"$T/out3" 2>&1; rc=$?
check "refused then reviewed neutrally: clean (exit $rc)" test "$rc" -eq 0
check "first call standard, retries neutral" \
  bash -c "awk '{print \$2}' '$FAKE_LOG/calls' | tr '\n' ' ' | has_match -E '^standard (neutral )+$'"
first_bytes="$(awk 'NR==1 { sub("bytes=", "", $4); print $4 }' "$FAKE_LOG/calls")"
retry_max="$(awk 'NR>1 { sub("bytes=", "", $4); if ($4 + 0 > m) m = $4 + 0 } END { print m + 0 }' "$FAKE_LOG/calls")"
check "retry pieces are smaller ($retry_max < $first_bytes)" test "$retry_max" -lt "$first_bytes"
check "neutral prompt avoids the project framing" bash -c "! grep -q 'Concrete, a compiler' '$FAKE_LOG/prompt.2'"
check "raw output records the refusal" grep -q ": refused" "$W/.review/$(g rev-parse HEAD)-1.md"

reset_fake
g checkout -q main && g checkout -qb refuse-always
commit_file Concrete/Own.lean "$(printf 'line %s\n' $(seq 1 40))
MARK_REFUSE_ALWAYS" "ownership change"
review >"$T/out4" 2>&1; rc=$?
check "refused twice: blocked, exit 5 (got $rc)" test "$rc" -eq 5
check "note: verdict blocked" test "$(field verdict)" = blocked
check "no loop: one standard call and one retry pass ($(calls) calls)" test "$(calls)" -ge 2 -a "$(calls)" -le 3
check "blocked piece listed in the output" grep -q "blocked" "$T/out4"
reset_fake
review >/dev/null 2>&1; rc=$?
check "a blocked HEAD may be re-run; it counts as round 2" test "$(field rounds)" = 2 -a "$rc" -eq 5
check "the re-run reviews the whole branch again (blocked does not advance the delta)" \
  grep -q "complete change of a branch" "$FAKE_LOG/prompt.1"

echo "=== findings in one piece, another piece blocked ==="
reset_fake
g checkout -q main && g checkout -qb mixed
commit_file a.txt "MARK_FINDING here" "a finding"
commit_file Concrete/Own.lean "MARK_REFUSE_ALWAYS" "a refusal"
review >"$T/out8" 2>&1; rc=$?
check "blocked wins over findings: verdict blocked, exit 5 (got $rc, $(field verdict))" \
  test "$rc" -eq 5 -a "$(field verdict)" = blocked
check "the reviewed piece's findings are still recorded" \
  bash -c "git -C '$W' notes --ref=refs/notes/concrete-review-v2 show HEAD | has_match 'counter is never reset'"
commit_file b.txt "more" "more work"
reset_fake
review >/dev/null 2>&1
check "next round restarts from the base, not after the blocked round" grep -q "complete change of a branch" "$FAKE_LOG/prompt.1"
check "and carries the blocked round's findings forward" grep -q "counter is never reset" "$FAKE_LOG/prompt.1"
# A blocked round that finds nothing new must still hand the old finding on.
commit_file a.txt "MARK_REFUSE_ALWAYS" "rewrite a.txt"
review >/dev/null 2>&1
check "a blocked round with no findings of its own records the carried ones" \
  bash -c "git -C '$W' notes --ref=refs/notes/concrete-review-v2 show HEAD | has_match 'counter is never reset'"
commit_file c.txt "c" "c"
reset_fake
review >/dev/null 2>&1
check "after two blocked rounds in a row the finding still reaches the prompt" \
  grep -q "counter is never reset" "$FAKE_LOG/prompt.1"

echo "=== pre-push hook: a review-notes push runs no gates ==="
HOOK="$ROOT_DIR/.githooks/pre-push"
Z=0000000000000000000000000000000000000000
NOTES_SHA="$(g rev-parse refs/notes/concrete-review-v2)"
hook_out="$(cd "$W" && printf 'refs/notes/concrete-review-v2 %s refs/notes/concrete-review-v2 %s\n' \
  "$NOTES_SHA" "$Z" | bash "$HOOK" 2>&1)"; rc=$?
check "notes-only push: hook exits 0 (got $rc)" test "$rc" -eq 0
check "notes-only push: hook takes the review-notes tier" grep -q "review-notes push" <<<"$hook_out"
# Owner sign-off refs: only a record (parentless, its subject's 40-hex name, exactly the seven files)
# takes the notes tier. Anything else takes the full tier, which a linked worktree refuses before it
# runs anything: that refusal is how these legs see the tier without running the gates.
SO_BLOB="$(printf 'x\n' | g hash-object -w --stdin)"
SO_TREE="$(printf '100644 blob %s\tagy-request.txt\n100644 blob %s\tagy.txt\n100644 blob %s\tcodex-request.txt\n100644 blob %s\tcodex.txt\n100644 blob %s\tkimi-request.txt\n100644 blob %s\tkimi.txt\n100644 blob %s\trecord.txt\n' "$SO_BLOB" "$SO_BLOB" "$SO_BLOB" "$SO_BLOB" "$SO_BLOB" "$SO_BLOB" "$SO_BLOB" | g mktree)"
SO_REC="$(g commit-tree "$SO_TREE" -m record)"; SO_SUBJ="$(g rev-parse HEAD)"
SO_OLD_TREE="$(printf '100644 blob %s\tagy.txt\n100644 blob %s\tcodex.txt\n100644 blob %s\tkimi.txt\n100644 blob %s\trecord.txt\n' "$SO_BLOB" "$SO_BLOB" "$SO_BLOB" "$SO_BLOB" | g mktree)"
SO_OLD_REC="$(g commit-tree "$SO_OLD_TREE" -m old-record)"
g worktree add -q --detach "$T/so-wt" HEAD
so_hook(){ (cd "$T/so-wt" && printf '%s %s %s %s\n' "$1" "$2" "$1" "$Z" | CONCRETE_GATES_SHELL= bash "$HOOK" origin x 2>&1); }
hook_out="$(so_hook "refs/owner-signoff/$SO_SUBJ" "$SO_REC")"; rc=$?
check "a seven-file owner sign-off record takes the review-notes tier (exit $rc)" \
  bash -c "[ $rc -eq 0 ] && grep -q 'review-notes push' <<<'$hook_out'"
hook_out="$(so_hook "refs/owner-signoff/$SO_SUBJ" "$SO_OLD_REC")"; rc=$?
check "the old four-file owner sign-off shape takes the full tier (exit $rc)" \
  bash -c "[ $rc -eq 1 ] && grep -q 'is a linked worktree' <<<'$hook_out'"
hook_out="$(so_hook "refs/owner-signoff/$SO_SUBJ" "$SO_SUBJ")"; rc=$?
check "a source commit pushed as a sign-off takes the full tier (exit $rc)" \
  bash -c "[ $rc -eq 1 ] && grep -q 'is a linked worktree' <<<'$hook_out'"
SO_DTREE="$(printf '100644 blob %s\tsrc.lean\n' "$SO_BLOB" | g mktree)"
SO_DIRS="$(g commit-tree "$(printf '040000 tree %s\tagy-request.txt\n040000 tree %s\tagy.txt\n040000 tree %s\tcodex-request.txt\n040000 tree %s\tcodex.txt\n040000 tree %s\tkimi-request.txt\n040000 tree %s\tkimi.txt\n040000 tree %s\trecord.txt\n' "$SO_DTREE" "$SO_DTREE" "$SO_DTREE" "$SO_DTREE" "$SO_DTREE" "$SO_DTREE" "$SO_DTREE" | g mktree)" -m dirs)"
hook_out="$(so_hook "refs/owner-signoff/$SO_SUBJ" "$SO_DIRS")"; rc=$?
check "seven directories named like a record's files take the full tier (exit $rc)" \
  bash -c "[ $rc -eq 1 ] && grep -q 'is a linked worktree' <<<'$hook_out'"
hook_out="$(so_hook "refs/owner-signoff/not-a-sha" "$SO_REC")"; rc=$?
check "a record under a name that is no SHA takes the full tier (exit $rc)" \
  bash -c "[ $rc -eq 1 ] && grep -q 'is a linked worktree' <<<'$hook_out'"
g worktree remove --force "$T/so-wt"
hook_out="$(cd "$W" && printf 'refs/heads/mixed %s refs/heads/wip/x %s\nrefs/notes/concrete-review-v2 %s refs/notes/concrete-review-v2 %s\n' \
  "$(g rev-parse HEAD)" "$Z" "$NOTES_SHA" "$Z" | bash "$HOOK" 2>&1)"; rc=$?
check "notes alongside a wip push take the wip tier (exit $rc)" \
  bash -c "[ $rc -eq 0 ] && grep -q 'wip/\* backup push' <<<'$hook_out'"

echo "=== splitting by file group and size ==="
reset_fake
g checkout -q main && g checkout -qb split
mkdir -p "$W/Concrete" "$W/tests" "$W/docs" "$W/scripts"
for i in 1 2 3; do printf 'def f%s := %s\n' "$i" "$i" >"$W/Concrete/M$i.lean"; done
printf 'test\n' >"$W/tests/t.con"
printf 'doc\n' >"$W/docs/d.md"
printf 'echo hi\n' >"$W/scripts/s.sh"
seq 1 400 | sed 's/^/big line /' >"$W/Concrete/Big.lean"
g add -A && g commit -qm "spread change"
REVIEW_PIECE_BYTES=2000 review >"$T/out5" 2>&1; rc=$?
check "split review is clean (exit $rc)" test "$rc" -eq 0
K="$(field pieces)"
check "several pieces ($K)" test "$K" -ge 5
check "one call per piece" test "$(calls)" -eq "$K"
mixed=0
while read -r line; do
  files="${line##*files=}"
  groups="$(tr ',' '\n' <<<"$files" | sed '/^$/d' | awk -F/ '{ print $1 }' | sort -u | wc -l | tr -d ' ')"
  [ "$groups" -le 1 ] || mixed=1
done <"$FAKE_LOG/calls"
check "no piece mixes file groups" test "$mixed" -eq 0
over="$(awk '{ sub("bytes=", "", $4); if ($4 + 0 > 2000) print }' "$FAKE_LOG/calls")"
check "every piece within the size limit${over:+: $over}" test -z "$over"
check "the big file is split across pieces" test "$(grep -c 'files=Concrete/Big.lean,$' "$FAKE_LOG/calls")" -ge 2
check "compiler pieces come first" bash -c "head -1 '$FAKE_LOG/calls' | has_match 'files=Concrete/'"
bigpart="$(grep -l '^diff --git a/Concrete/Big.lean' "$FAKE_LOG"/stdin.* | awk 'NR==2')"
check "each split part repeats the file header and starts a hunk" \
  bash -c "head -1 '$bigpart' | has_match '^diff --git' && grep -q '^@@' '$bigpart'"

g diff "$(g merge-base HEAD main)" HEAD >"$T/split.diff"
total="$(wc -c <"$T/split.diff" | tr -d ' ')"
n="$(python3 "$SPLIT" "$T/split.diff" "$T/sp3" $(( (total + 2) / 3 )))"
check "--pieces N sizing: a third of the diff gives about three pieces ($n)" test "$n" -ge 3 -a "$n" -le 6
n1p="$(python3 "$SPLIT" "$T/split.diff" "$T/sp1" "$total")"
check "a limit of the whole diff still separates the four groups ($n1p)" test "$n1p" -eq 4
# Hunk headers are excluded: a hunk cut in parts gets renumbered headers.
cat "$T"/sp3/piece-*.diff | grep -v '^@@' | sort >"$T/sp3.sorted"
grep -v '^@@' "$T/split.diff" | sort >"$T/orig.sorted"
check "split loses no diff line" bash -c "comm -23 '$T/orig.sorted' '$T/sp3.sorted' | has_match . && exit 1 || exit 0"
reset_fake
g checkout -q main && g checkout -qb pieces-n
commit_file tests/a.con "$(seq 1 300)" "tests only"
review --pieces 3 >/dev/null 2>&1
p3="$(field pieces)"
check "--pieces 3 on one file gives three pieces ($p3)" test "$p3" -eq 3
# Each part's @@ header must match its own body, and the parts must continue
# one another: part k+1 starts where part k ended, on both sides.
cat >"$T/hunks.py" <<'EOF'
import re, sys
expect_old = expect_new = None
for path in sys.argv[1:]:
    hunks = [l for l in open(path) if l.startswith("@@")]
    if len(hunks) != 1:
        sys.exit("%s: want one hunk, got %d" % (path, len(hunks)))
    m = re.match(r"@@ -(\d+),(\d+) \+(\d+),(\d+) @@", hunks[0])
    if not m:
        sys.exit("%s: unnumbered header %r" % (path, hunks[0]))
    o, oc, n, nc = map(int, m.groups())
    body = open(path).read().split(hunks[0], 1)[1].splitlines()
    got_o = sum(1 for l in body if l[:1] in (" ", "-"))
    got_n = sum(1 for l in body if l[:1] in (" ", "+"))
    if (got_o, got_n) != (oc, nc):
        sys.exit("%s: header counts %d,%d but body has %d,%d" % (path, oc, nc, got_o, got_n))
    if expect_new is not None and (o, n) != (expect_old, expect_new):
        sys.exit("%s: starts at -%d +%d, previous part ended at -%d +%d" % (path, o, n, expect_old, expect_new))
    expect_old, expect_new = o + oc, n + nc
if expect_new != 301:
    sys.exit("parts end at +%s, want +301 (300 lines)" % expect_new)
EOF
check "split parts carry correct, contiguous @@ headers" \
  python3 "$T/hunks.py" $(ls "$FAKE_LOG"/stdin.* | sort -t. -k2 -n)

echo "=== the round cap ==="
reset_fake
g checkout -q main && g checkout -qb capped
commit_file a.txt "MARK_FINDING one" "c1"
review --max-rounds 2 >/dev/null 2>&1; rc=$?
check "round 1 of 2: findings (exit $rc)" test "$rc" -eq 1
commit_file a.txt "MARK_FINDING two" "c2"
review --max-rounds 2 >"$T/cap" 2>&1; rc=$?
check "round 2 of 2 with findings: cap exit 3 (got $rc)" test "$rc" -eq 3
check "cap message says split or escalate" grep -q "Split the branch" "$T/cap"
check "cap message lists the open findings" grep -q "counter is never reset" "$T/cap"
check "round 2 note was still written" test "$(field rounds)/$(field verdict)" = "2/findings"
before="$(calls)"
commit_file a.txt "fixed" "c3"
review --max-rounds 2 >"$T/cap2" 2>&1; rc=$?
check "no round 3: refused with exit 3 (got $rc)" test "$rc" -eq 3
check "and no reviewer call" test "$(calls)" -eq "$before"
check "no note written on the refused round" bash -c "! git -C '$W' notes --ref=refs/notes/concrete-review-v2 show HEAD >/dev/null 2>&1"

echo "=== notes push: non-fast-forward is fetched, merged and retried ==="
git clone -q "$T/origin.git" "$T/other" 2>/dev/null
git -C "$T/other" fetch -q origin "+refs/notes/concrete-review-v2:refs/notes/concrete-review-v2"
OTHER_SHA="$(git -C "$T/other" rev-parse HEAD)"
git -C "$T/other" notes --ref=refs/notes/concrete-review-v2 add -f -m "verdict: clean
rounds: 1
reviewer: codex
utc: 2026-10-22T00:00:00Z
base: $OTHER_SHA
pieces: 1
diff-start: $OTHER_SHA" "$OTHER_SHA"
git -C "$T/other" push -q origin refs/notes/concrete-review-v2
reset_fake
g checkout -q main && g checkout -qb nonff
commit_file z.txt "z" "z"
# Our local notes ref lacks the other writer's note: the start-of-round sync
# must merge it, or the push below would be a non-fast-forward.
review >"$T/out6" 2>&1; rc=$?
check "review after a concurrent notes push succeeds (exit $rc)" test "$rc" -eq 0
check "origin keeps the other writer's note" \
  bash -c "git -C '$T/origin.git' notes --ref=refs/notes/concrete-review-v2 show $OTHER_SHA | has_match 'verdict: clean'"
check "origin has our note" \
  bash -c "git -C '$T/origin.git' notes --ref=refs/notes/concrete-review-v2 show $(g rev-parse HEAD) | has_match 'verdict: clean'"
# Now force the non-fast-forward at push time: another writer pushes between
# our sync and our push (the fake reviewer does it while it "reviews").
git -C "$T/other" fetch -q origin "+refs/notes/concrete-review-v2:refs/notes/concrete-review-v2"
cat >"$T/bin/codex-race" <<EOF
#!/usr/bin/env bash
git -C "$T/other" notes --ref=refs/notes/concrete-review-v2 add -f -m "verdict: clean
rounds: 2
reviewer: codex
utc: 2026-10-22T00:00:00Z
base: $OTHER_SHA
pieces: 1
diff-start: $OTHER_SHA" "$OTHER_SHA"
git -C "$T/other" push -q origin refs/notes/concrete-review-v2
exec "$T/bin/codex" "\$@"
EOF
chmod +x "$T/bin/codex-race"
reset_fake
commit_file z2.txt "z2" "z2"
REVIEW_CODEX_BIN="$T/bin/codex-race" review >"$T/out7" 2>&1; rc=$?
check "push rejected once, then merged and pushed (exit $rc)" test "$rc" -eq 0
check "the retry path ran" grep -q "rejected; fetching and merging" "$T/out7"
check "origin holds the racing note" \
  bash -c "git -C '$T/origin.git' notes --ref=refs/notes/concrete-review-v2 show $OTHER_SHA | has_match 'rounds: 2'"
check "origin holds our note" \
  bash -c "git -C '$T/origin.git' notes --ref=refs/notes/concrete-review-v2 show $(g rev-parse HEAD) | has_match 'verdict: clean'"

echo "=== notes push: any other refusal stops at once (exit 6) ==="
printf '#!/bin/sh\necho "remote policy says no" >&2\nexit 1\n' >"$T/origin.git/hooks/pre-receive"
chmod +x "$T/origin.git/hooks/pre-receive"
reset_fake
commit_file z3.txt "z3" "z3"
review >"$T/out9" 2>&1; rc=$?
rm -f "$T/origin.git/hooks/pre-receive"
check "a refused (not non-fast-forward) push exits 6 (got $rc)" test "$rc" -eq 6
check "and is not retried as if it were a race" bash -c "! grep -q 'rejected; fetching and merging' '$T/out9'"
check "the note is still recorded locally" test "$(field verdict)" = clean
before="$(calls)"
review >"$T/out9-retry" 2>&1; rc=$?
check "a clean HEAD retries publication without repeating the review (exit $rc)" test "$rc" -eq 0 -a "$(calls)" -eq "$before"
check "the retry published the same note" test "$(git -C "$T/origin.git" notes --ref=concrete-review-v2 list "$(g rev-parse HEAD)")" = "$(g notes --ref=concrete-review-v2 list HEAD)"

echo "=== ISS-0099: rejecting the legacy denial leaves v2 evidence intact ==="
cat >"$T/origin.git/hooks/update" <<'EOF'
#!/bin/sh
if [ "$1" = refs/notes/concrete-review ]; then
  echo "remote policy rejects the legacy denial" >&2
  exit 1
fi
EOF
chmod +x "$T/origin.git/hooks/update"
VERDICT_TIP="$(git -C "$T/origin.git" rev-parse refs/notes/concrete-review-v2)"
g checkout -qb atomic-notes origin/main
commit_file atomic.txt MARK_FINDING atomic-notes
reset_fake
review >"$T/atomic" 2>&1; rc=$?
rm -f "$T/origin.git/hooks/update"
check "publication is refused (exit $rc)" test "$rc" -eq 6
check "remote hook rejected the legacy denial" grep -q 'remote policy rejects the legacy denial' "$T/atomic"
check "remote evidence ref did not advance" test "$(git -C "$T/origin.git" rev-parse refs/notes/concrete-review-v2)" = "$VERDICT_TIP"

echo "=== interrupted findings write cannot leak into an unrelated publication ==="
g checkout -qb interrupted origin/main
commit_file interrupted.txt MARK_NOVERDICT interrupted
review >"$T/blocked-before" 2>&1; rc=$?
check "interrupted rerun starts with a published blocked round" test "$rc" -eq 5
INTERRUPTED_SHA="$(g rev-parse HEAD)"
INTERRUPTED_BLOB="$(g notes --ref=concrete-review-v2 list "$INTERRUPTED_SHA")"
mkdir -p "$T/interrupt/bin"
export REAL_GIT="$(command -v git)"
export INTERRUPT_MARKER="$T/interrupted-write"
cat >"$T/interrupt/bin/git" <<'EOF'
#!/usr/bin/env bash
# In the old implementation this is the live findings write. In the new one
# the findings are part of the note written to the private staging ref.
case "$1:${2:-}:${3:-}" in
  notes:--ref=refs/notes/concrete-review-findings:add|notes:--ref=refs/notes/review-stage-*:add)
    "$REAL_GIT" "$@" || exit $?
    printf 'write completed\n' >"$INTERRUPT_MARKER"
    kill -KILL "$PPID"
    ;;
  *) exec "$REAL_GIT" "$@" ;;
esac
EOF
chmod +x "$T/interrupt/bin/git"
PATH="$T/interrupt/bin:$PATH" TMPDIR="$T/interrupt" review >"$T/interrupted" 2>&1; rc=$?
check "review was killed immediately after writing findings (exit $rc)" test "$rc" -eq 137 -a -s "$INTERRUPT_MARKER"
check "interruption left the canonical local record intact" test "$(g notes --ref=concrete-review-v2 list "$INTERRUPTED_SHA")" = "$INTERRUPTED_BLOB"

g checkout -qb unrelated-after-interrupt origin/main
commit_file unrelated.txt clean unrelated
review >"$T/unrelated" 2>&1; rc=$?
check "unrelated review publishes after the interruption (exit $rc)" test "$rc" -eq 0
REMOTE_INTERRUPTED="$(git -C "$T/origin.git" notes --ref=concrete-review-v2 list "$INTERRUPTED_SHA")"
check "remote retains the exact pre-interruption evidence" test "$REMOTE_INTERRUPTED" = "$INTERRUPTED_BLOB"

# A manually corrupted pending record on that other branch must also fail closed.
g notes --ref=concrete-review-v2 add -f -m "$(note "$INTERRUPTED_SHA" | sed '/^$/q')" "$INTERRUPTED_SHA" 2>/dev/null
VERDICT_TIP="$(git -C "$T/origin.git" rev-parse refs/notes/concrete-review-v2)"
commit_file unrelated.txt 'another clean change' unrelated-again
review >"$T/corrupted" 2>&1; rc=$?
check "unrelated publication refuses an inconsistent local record (exit $rc)" test "$rc" -eq 6
check "the refusal identifies the unrelated corrupt record" grep -q "refusing to publish invalid local evidence for $INTERRUPTED_SHA" "$T/corrupted"
check "invalid pending evidence leaves the remote unchanged" test "$(git -C "$T/origin.git" rev-parse refs/notes/concrete-review-v2)" = "$VERDICT_TIP"
g notes --ref=concrete-review-v2 add -f -C "$INTERRUPTED_BLOB" "$INTERRUPTED_SHA" 2>/dev/null

echo "=== legacy round accounting qualifies for exact-SHA owner sign-off ==="
g checkout -qb legacy-signoff origin/main
commit_file scripts/fixture.sh 'echo change' legacy-signoff
LEGACY_SHA="$(g rev-parse HEAD)"
legacy_note() {
  g notes --ref=concrete-review add -f -m "verdict: $1
rounds: 3
reviewer: codex
utc: 2026-11-21T00:00:00Z
base: $2
pieces: 1" HEAD 2>/dev/null
}
legacy_note findings "$BASE"
g notes --ref=concrete-review-findings add -m 'legacy open finding' HEAD
reset_fake
REVIEW_PUSH=0 review --max-rounds 3 >"$T/legacy-cap" 2>&1; rc=$?
check "legacy round 3 still caps review (exit $rc)" test "$rc" -eq 3 -a "$(calls)" -eq 0
check "legacy open findings survive migration" grep -q 'legacy open finding' "$T/legacy-cap"
printf 'review of %s\n' "$LEGACY_SHA" >"$T/request"
printf 'VERDICT: ACCEPT\n' >"$T/accept"
SIGNOFF="$ROOT_DIR/scripts/land/signoff.sh"
record_approval() {
  (cd "$W" && bash "$SIGNOFF" record "$LEGACY_SHA" --base "$BASE" --rounds-at "$LEGACY_SHA" \
    --date 2026-11-21 --quote "Approve $LEGACY_SHA" \
    --codex-request "$T/request" --codex "$T/accept" \
    --kimi-request "$T/request" --kimi "$T/accept" --agy-request "$T/request" --agy "$T/accept")
}
legacy_note findings "$LEGACY_SHA"
record_approval >"$T/legacy-narrow" 2>&1; rc=$?
check "narrow legacy note cannot qualify an approval (exit $rc)" test "$rc" -eq 1
check "narrow legacy note fails range validation" grep -q 'review range does not cover required base' "$T/legacy-narrow"
legacy_note findings "$BASE"
record_approval >"$T/legacy-approval" 2>&1; rc=$?
check "legacy round 3 lets sign-off record owner approval (exit $rc)" test "$rc" -eq 0
(cd "$W" && bash "$SIGNOFF" verify "$LEGACY_SHA" --base "$BASE") >"$T/legacy-approval" 2>&1; rc=$?
check "the recorded owner approval verifies (exit $rc)" test "$rc" -eq 0
legacy_note clean "$BASE"
(cd "$W" && bash "$VERIFY" --required-base "$BASE" HEAD) >"$T/legacy-coverage" 2>&1; rc=$?
check "legacy clean round 3 is ineligible as clean landing evidence (exit $rc)" test "$rc" -eq 2

echo "=== legacy accounting with an advanced review base ==="
g checkout -qb legacy-advanced-base origin/main || exit 1
commit_file advanced-main.txt clean advanced-main
ADVANCED_BASE="$(g rev-parse HEAD)"
g checkout -qb legacy-advanced origin/main
for legacy_round in 1 2 3; do
  commit_file legacy-advanced.txt "legacy change $legacy_round" "legacy round $legacy_round"
  g notes --ref=concrete-review add -m "verdict: findings
rounds: $legacy_round
reviewer: codex
utc: 2026-11-21T00:00:00Z
base: $ADVANCED_BASE
pieces: 1" HEAD
  g notes --ref=concrete-review-findings add -m "advanced base finding $legacy_round" HEAD
done
reset_fake
REVIEW_PUSH=0 review --max-rounds 3 >"$T/legacy-advanced" 2>&1; rc=$?
check "advanced legacy base does not reset the cap (exit $rc)" test "$rc" -eq 3 -a "$(calls)" -eq 0
for legacy_round in 1 2 3; do
  check "advanced base finding $legacy_round survives" grep -q "advanced base finding $legacy_round" "$T/legacy-advanced"
done

echo "=== partial legacy publication after a later clean round ==="
g checkout -qb legacy-partial origin/main
commit_file partial-a.txt clean partial-findings
legacy_note findings "$BASE"
PARTIAL_SHA="$(g rev-parse HEAD)"
reset_fake
REVIEW_PUSH=0 review --max-rounds 5 >"$T/legacy-missing" 2>&1; rc=$?
check "unresolved missing legacy findings still fail closed (exit $rc)" test "$rc" -eq 2 -a "$(calls)" -eq 0
check "missing text identifies the affected SHA" grep -q "missing or mismatched findings text for $PARTIAL_SHA" "$T/legacy-missing"
commit_file partial-b.txt clean partial-clean
legacy_note clean "$BASE"
# Legacy helper writes round 3. Make the earlier findings round 2.
g notes --ref=concrete-review show "$PARTIAL_SHA" | sed 's/^rounds: 3$/rounds: 2/' >"$T/partial-note"
g notes --ref=concrete-review add -f -F "$T/partial-note" "$PARTIAL_SHA" 2>/dev/null
REVIEW_PUSH=0 review --max-rounds 3 >"$T/legacy-partial-cap" 2>&1; rc=$?
check "superseded missing findings do not bypass the cap (exit $rc)" test "$rc" -eq 3 -a "$(calls)" -eq 0
REVIEW_PUSH=0 review --max-rounds 5 >"$T/legacy-partial" 2>&1; rc=$?
check "migration recovers in round 4 (exit $rc)" test "$rc" -eq 0 -a "$(field rounds)" = 4
check "migration reviews both legacy commits in full" grep -q 'files=partial-a.txt,partial-b.txt,' "$FAKE_LOG/calls"
check "migration records full v2 coverage" test "$(field diff-start)" = "$BASE"

echo "=== capped blocked round retries publication without another review ==="
g checkout -qb capped-blocked-retry origin/main
commit_file capped-blocked.txt MARK_NOVERDICT capped-blocked-retry
printf '#!/bin/sh\ntest "$1" != refs/notes/concrete-review-v2\n' >"$T/origin.git/hooks/update"
chmod +x "$T/origin.git/hooks/update"
review --max-rounds 1 >"$T/blocked-cap-failure" 2>&1; rc=$?
rm -f "$T/origin.git/hooks/update"
check "blocked cap publication reports the refusal (exit $rc)" test "$rc" -eq 6
BLOCKED_SHA="$(g rev-parse HEAD)"
if git -C "$T/origin.git" notes --ref=concrete-review-v2 list "$BLOCKED_SHA" >/dev/null 2>&1; then
  no "blocked evidence remains unpublished"
else
  ok "blocked evidence remains unpublished"
fi
reset_fake
review --max-rounds 1 >"$T/blocked-cap-retry" 2>&1; rc=$?
check "blocked cap retry publishes without another review (exit $rc)" test "$rc" -eq 3 -a "$(calls)" -eq 0
check "blocked cap retry published the exact record" test "$(git -C "$T/origin.git" notes --ref=concrete-review-v2 list "$BLOCKED_SHA")" = "$(g notes --ref=concrete-review-v2 list HEAD)"

echo "=== mixed versions: legacy landing refuses after v2 findings ==="
PRE_MIGRATION=ff462b3180acd6ef8c03566e7e53e2be0ff489f8
mkdir -p "$T/old/scripts/"{review,checks/lib,land}
for file in scripts/review/{review.sh,verify.sh,split_diff.py} scripts/checks/lib/has_match.sh scripts/land/train.sh; do
  git -C "$ROOT_DIR" show "$PRE_MIGRATION:$file" >"$T/old/$file" || exit 1
done
sed -n '/^review_provenance() {/,/^}/p' "$T/old/scripts/land/train.sh" >"$T/old/provenance.sh"
old_review() { (cd "$W" && bash "$T/old/scripts/review/review.sh" --max-rounds 3); }
old_verify() { (cd "$W" && bash "$T/old/scripts/review/verify.sh" --fetch --required-base "$BASE" HEAD); }
old_provenance() (
  cd "$W" || exit 1
  VERIFY="$T/old/scripts/review/verify.sh"
  REMOTE=origin
  state_get_soft() { [ "$2" = base ] && printf '%s\n' "$BASE"; }
  source "$T/old/provenance.sh"
  review_provenance mixed "$MIXED_SHA"
)
g checkout -qb mixed-version origin/main
commit_file mixed.txt MARK_FINDING mixed-version
MIXED_SHA="$(g rev-parse HEAD)"
reset_fake
# Have the old writer publish a clean verdict for this exact object.
cat >"$T/bin/always-clean" <<'EOF'
#!/usr/bin/env bash
while [ "$#" -gt 0 ]; do
  if [ "$1" = -o ]; then output="$2"; shift; fi
  shift
done
cat >/dev/null
printf 'VERDICT: clean\n' >"$output"
EOF
chmod +x "$T/bin/always-clean"
REVIEW_CODEX_BIN="$T/bin/always-clean" old_review >"$T/old-review" 2>&1; rc=$?
check "old reviewer published a clean verdict before cutover (exit $rc)" test "$rc" -eq 0
check "old verifier accepts the initial legacy clean note" old_verify
check "old train accepts the initial legacy clean note" old_provenance
mkdir -p "$T/cutover-hooks"
cp "$HOOK" "$T/cutover-hooks/pre-push"
g config core.hooksPath "$T/cutover-hooks"
review --max-rounds 3 >"$T/mixed-new" 2>&1; rc=$?
g config --unset core.hooksPath
check "new full review publishes findings in round 2 (exit $rc)" test "$rc" -eq 1 -a "$(field rounds)" = 2
MIXED_BLOB="$(g notes --ref=concrete-review-v2 list "$MIXED_SHA")"
reset_fake
REVIEW_CODEX_BIN="$T/bin/always-clean" old_review >"$T/old-rerun" 2>&1; rc=$?
check "old reviewer refuses to publish another clean verdict (exit $rc)" test "$rc" -eq 4
old_verify >"$T/old-verify" 2>&1; rc=$?
check "old verifier refuses landing (exit $rc)" test "$rc" -eq 1
old_provenance >"$T/old-train" 2>&1; rc=$?
check "old train refuses landing (exit $rc)" test "$rc" -eq 1
(cd "$W" && bash "$VERIFY" --fetch --required-base "$BASE" HEAD) >"$T/mixed-verify" 2>&1; rc=$?
check "new verifier still sees findings (exit $rc)" test "$rc" -eq 1
check "older writer left the new evidence byte-for-byte intact" test "$(g notes --ref=concrete-review-v2 list HEAD)" = "$MIXED_BLOB"
# Simulate an already-running old writer with a clean notes commit based on
# the latest legacy tip. A fast-forward on its own would overwrite the denial.
g update-ref refs/notes/cached-old-writer "$(g rev-parse refs/notes/concrete-review)"
g notes --ref=cached-old-writer add -f -m "verdict: clean
rounds: 3
reviewer: codex
utc: 2026-11-21T00:00:00Z
base: $BASE
pieces: 1" HEAD 2>/dev/null
g -c core.hooksPath="$T/cutover-hooks" push origin refs/notes/cached-old-writer:refs/notes/concrete-review >"$T/stale-write" 2>&1; rc=$?
check "installed hook refuses a stale legacy-only fast-forward (exit $rc)" test "$rc" -ne 0
check "hook identifies the legacy-only update" grep -q 'standalone legacy review updates are disabled' "$T/stale-write"
old_provenance >"$T/old-train-after-stale-write" 2>&1; rc=$?
check "old train still refuses after the stale clean write attempt (exit $rc)" test "$rc" -eq 1


echo "=== usage errors ==="
review --pieces zero >/dev/null 2>&1; rc=$?
check "bad --pieces: exit 2 (got $rc)" test "$rc" -eq 2
review --bogus >/dev/null 2>&1; rc=$?
check "unknown flag: exit 2 (got $rc)" test "$rc" -eq 2

echo
echo "check_review_tool: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ] || exit 1
