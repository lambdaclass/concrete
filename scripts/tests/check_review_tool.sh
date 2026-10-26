#!/usr/bin/env bash
# R-0544 — the review tool (scripts/review/review.sh, verify.sh, split_diff.py)
# does what docs/project/REVIEW.md says it does.
#
# No API key is needed: the reviewer is a fake `codex` that answers according to
# MARK_* markers on the ADDED lines of the diff it receives on stdin, and logs
# every call (prompt kind, stdin size, files) so the legs can check what was sent.
# All git work happens in throwaway repositories with a bare repository as
# `origin`, so the notes push, including the non-fast-forward path, runs for real.
#
# Legs: verdict parsing, note writing and its exact format, notes push and the
# non-fast-forward merge, delta selection (round 2 sees only the delta and the
# previous findings), splitting (file groups, size limit, hunk split, --pieces N),
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

# Isolate git from the user's config (hooks, signing, identity).
export GIT_CONFIG_NOSYSTEM=1
export GIT_CONFIG_GLOBAL="$T/gitconfig"
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
note() { g notes --ref=refs/notes/concrete-review show "${1:-HEAD}" 2>/dev/null; }
field() { note "${2:-HEAD}" | sed -n "s/^$1: //p"; }

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
pieces: [0-9]+$'
n1="$(note "$R1")"
if [[ "$n1" =~ $FORMAT_RE ]]; then ok "note has exactly the six fields, in order"; else no "note format: $n1"; fi
check "note: verdict findings" test "$(field verdict)" = findings
check "note: rounds 1" test "$(field rounds)" = 1
check "note: base is the merge-base" test "$(field base)" = "$BASE"
check "note: pieces 1" test "$(field pieces)" = 1
check "raw output at .review/<sha>-1.md" test -s "$W/.review/$R1-1.md"
check "findings recorded under the findings ref" \
  bash -c "git -C '$W' notes --ref=refs/notes/concrete-review-findings show $R1 | has_match 'counter is never reset'"
check "notes ref pushed to origin" test "$(git -C "$T/origin.git" rev-parse refs/notes/concrete-review)" = "$(g rev-parse refs/notes/concrete-review)"
check ".review/ stays untracked" test -z "$(g status --porcelain)"
(cd "$W" && bash "$VERIFY" HEAD >/dev/null); rc=$?
check "verify.sh: findings is not clean (exit 1, got $rc)" test "$rc" -eq 1

echo "=== same HEAD again: nothing to review ==="
review >/dev/null 2>&1; rc=$?
check "re-review of a reviewed HEAD exits 4 (got $rc)" test "$rc" -eq 4
check "and calls no reviewer" test "$(calls)" -eq 1

echo "=== round 2: only the delta, with the previous findings ==="
commit_file old.txt "fixed" "fix the counter"
commit_file new.txt "a second change" "second change"
review >"$T/out2" 2>&1; rc=$?
check "clean exit 0 (got $rc)" test "$rc" -eq 0
check "round 2 diff holds only the delta (new.txt, old.txt fix)" grep -q "^2 standard .*files=new.txt,old.txt,$" "$FAKE_LOG/calls"
check "round 2 diff does not resend the round-1 hunk" bash -c "! grep -q '^+MARK_FINDING' '$FAKE_LOG/stdin.2'"
check "round 2 prompt names the delta start" grep -q "change since commit $R1" "$FAKE_LOG/prompt.2"
check "round 2 prompt carries the previous findings" grep -q "counter is never reset" "$FAKE_LOG/prompt.2"
check "note: rounds 2, verdict clean" test "$(field rounds)/$(field verdict)" = "2/clean"
(cd "$W" && bash "$VERIFY" HEAD >"$T/v" 2>&1); rc=$?
check "verify.sh: clean exit 0 (got $rc)" test "$rc" -eq 0
check "verify.sh prints the verdict" grep -q "clean (round 2" "$T/v"
(cd "$W" && bash "$VERIFY" "$R1" >/dev/null 2>&1); rc=$?
check "verify.sh judges the exact commit, not the branch (R1 still 1)" test "$rc" -eq 1
review >/dev/null 2>&1; rc=$?
check "an already-clean HEAD reports clean without a call (exit 0)" test "$rc" -eq 0 -a "$(calls)" -eq 2

echo "=== verify.sh: missing and malformed notes ==="
commit_file x.txt "x" "unreviewed"
(cd "$W" && bash "$VERIFY" HEAD >/dev/null 2>&1); rc=$?
check "no note: exit 2 (got $rc)" test "$rc" -eq 2
g notes --ref=refs/notes/concrete-review add -f -m "verdict: clean" HEAD
(cd "$W" && bash "$VERIFY" HEAD >/dev/null 2>&1); rc=$?
check "malformed note (verdict line alone): exit 2 (got $rc)" test "$rc" -eq 2
B40="$(printf '%040d' 0 | tr 0 a)"
# The landing bot's carried note (notes.sh put: keys in its own order, utc last) is well formed.
g notes --ref=refs/notes/concrete-review add -f -m "$(printf 'verdict: clean\nrounds: 2\nreviewer: codex\nbase: %s\npieces: 1\ncarried-from: %s\nutc: 2026-10-22T00:00:00Z' "$B40" "$B40")" HEAD
(cd "$W" && bash "$VERIFY" HEAD >"$T/vc" 2>&1); rc=$?
check "the landing bot's carried note verifies clean (exit 0, got $rc)" test "$rc" -eq 0
check "a carried verdict says where it came from" grep -q "carried from $B40" "$T/vc"
g notes --ref=refs/notes/concrete-review add -f -m "$(printf 'verdict: clean\nrounds: 2\nreviewer: codex\nutc: 2026-10-22T00:00:00Z\nbase: %s\npieces: 1\nsurprise: 1' "$B40")" HEAD
(cd "$W" && bash "$VERIFY" HEAD >/dev/null 2>&1); rc=$?
check "an unknown key is malformed: exit 2 (got $rc)" test "$rc" -eq 2
g notes --ref=refs/notes/concrete-review add -f -m "$(printf 'verdict: findings\nverdict: clean\nrounds: 2\nreviewer: codex\nutc: 2026-10-22T00:00:00Z\nbase: %s\npieces: 1' "$B40")" HEAD
(cd "$W" && bash "$VERIFY" HEAD >/dev/null 2>&1); rc=$?
check "a repeated key is malformed: exit 2 (got $rc)" test "$rc" -eq 2
g notes --ref=refs/notes/concrete-review remove HEAD 2>/dev/null

echo "=== verdict parsing ==="
parse_case() { # parse_case <marker> <want-exit> <want-verdict> <label>
  local marker="$1" want="$2" wantv="$3" label="$4" rc
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
  bash -c "git -C '$W' notes --ref=refs/notes/concrete-review-findings show HEAD | has_match 'counter is never reset'"
commit_file b.txt "more" "more work"
reset_fake
review >/dev/null 2>&1
check "next round restarts from the base, not after the blocked round" grep -q "complete change of a branch" "$FAKE_LOG/prompt.1"
check "and carries the blocked round's findings forward" grep -q "counter is never reset" "$FAKE_LOG/prompt.1"
# A blocked round that finds nothing new must still hand the old finding on.
commit_file a.txt "MARK_REFUSE_ALWAYS" "rewrite a.txt"
review >/dev/null 2>&1
check "a blocked round with no findings of its own records the carried ones" \
  bash -c "git -C '$W' notes --ref=refs/notes/concrete-review-findings show HEAD | has_match 'counter is never reset'"
commit_file c.txt "c" "c"
reset_fake
review >/dev/null 2>&1
check "after two blocked rounds in a row the finding still reaches the prompt" \
  grep -q "counter is never reset" "$FAKE_LOG/prompt.1"

echo "=== pre-push hook: a review-notes push runs no gates ==="
HOOK="$ROOT_DIR/.githooks/pre-push"
Z=0000000000000000000000000000000000000000
NOTES_SHA="$(g rev-parse refs/notes/concrete-review)"
hook_out="$(cd "$W" && printf 'refs/notes/concrete-review %s refs/notes/concrete-review %s\nrefs/notes/concrete-review-findings %s refs/notes/concrete-review-findings %s\n' \
  "$NOTES_SHA" "$Z" "$(g rev-parse refs/notes/concrete-review-findings)" "$Z" | bash "$HOOK" 2>&1)"; rc=$?
check "notes-only push: hook exits 0 (got $rc)" test "$rc" -eq 0
check "notes-only push: hook takes the review-notes tier" grep -q "review-notes push" <<<"$hook_out"
hook_out="$(cd "$W" && printf 'refs/heads/mixed %s refs/heads/wip/x %s\nrefs/notes/concrete-review %s refs/notes/concrete-review %s\n' \
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
check "no note written on the refused round" bash -c "! git -C '$W' notes --ref=refs/notes/concrete-review show HEAD >/dev/null 2>&1"

echo "=== notes push: non-fast-forward is fetched, merged and retried ==="
git clone -q "$T/origin.git" "$T/other" 2>/dev/null
git -C "$T/other" fetch -q origin "+refs/notes/concrete-review:refs/notes/concrete-review"
OTHER_SHA="$(git -C "$T/other" rev-parse HEAD)"
git -C "$T/other" notes --ref=refs/notes/concrete-review add -f -m "verdict: clean
rounds: 1
reviewer: codex
utc: 2026-10-22T00:00:00Z
base: $OTHER_SHA
pieces: 1" "$OTHER_SHA"
git -C "$T/other" push -q origin refs/notes/concrete-review
reset_fake
g checkout -q main && g checkout -qb nonff
commit_file z.txt "z" "z"
# Our local notes ref lacks the other writer's note: the start-of-round sync
# must merge it, or the push below would be a non-fast-forward.
review >"$T/out6" 2>&1; rc=$?
check "review after a concurrent notes push succeeds (exit $rc)" test "$rc" -eq 0
check "origin keeps the other writer's note" \
  bash -c "git -C '$T/origin.git' notes --ref=refs/notes/concrete-review show $OTHER_SHA | has_match 'verdict: clean'"
check "origin has our note" \
  bash -c "git -C '$T/origin.git' notes --ref=refs/notes/concrete-review show $(g rev-parse HEAD) | has_match 'verdict: clean'"
# Now force the non-fast-forward at push time: another writer pushes between
# our sync and our push (the fake reviewer does it while it "reviews").
git -C "$T/other" fetch -q origin "+refs/notes/concrete-review:refs/notes/concrete-review"
cat >"$T/bin/codex-race" <<EOF
#!/usr/bin/env bash
git -C "$T/other" notes --ref=refs/notes/concrete-review add -f -m "race note" "$OTHER_SHA"
git -C "$T/other" push -q origin refs/notes/concrete-review
exec "$T/bin/codex" "\$@"
EOF
chmod +x "$T/bin/codex-race"
reset_fake
commit_file z2.txt "z2" "z2"
REVIEW_CODEX_BIN="$T/bin/codex-race" review >"$T/out7" 2>&1; rc=$?
check "push rejected once, then merged and pushed (exit $rc)" test "$rc" -eq 0
check "the retry path ran" grep -q "rejected; fetching and merging" "$T/out7"
check "origin holds the racing note" \
  bash -c "git -C '$T/origin.git' notes --ref=refs/notes/concrete-review show $OTHER_SHA | has_match 'race note'"
check "origin holds our note" \
  bash -c "git -C '$T/origin.git' notes --ref=refs/notes/concrete-review show $(g rev-parse HEAD) | has_match 'verdict: clean'"

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

echo "=== usage errors ==="
review --pieces zero >/dev/null 2>&1; rc=$?
check "bad --pieces: exit 2 (got $rc)" test "$rc" -eq 2
review --bogus >/dev/null 2>&1; rc=$?
check "unknown flag: exit 2 (got $rc)" test "$rc" -eq 2

echo
echo "check_review_tool: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ] || exit 1
