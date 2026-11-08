#!/usr/bin/env bash
# R-0544 — the review tool (scripts/review/review.sh, verify.sh, split_diff.py)
# does what docs/REVIEW.md says it does.
#
# No API key or network is needed: fake Codex, Kimi and agy reviewers answer according to MARK_*
# markers or clean, and log every call (prompt kind, closed stdin, files, archived tree) so the legs
# can check what was sent. All git work happens in throwaway repositories with a bare repository as
# `origin`, so the notes push, including the non-fast-forward path, runs for real.
#
# Legs: verdict and repository-access parsing, exact prompt persistence and hashes, both prompts'
# permanent owner rule, independent reviewer/writer selection and refusal, note writing and its
# format, notes push and the non-fast-forward merge, delta selection (round 2 sees only the delta
# and previous findings), splitting (file groups, size limit, hunk split, --pieces N), the
# content-filter refusal retry (once, neutral, smaller, then `blocked`), the round cap (distinct
# exit code, no extra round), verify.sh including legacy clean-note and required-base refusal, and
# the pre-push hook's review-notes tier (a notes-only push runs no gates).

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
# Fake reviewer: `codex exec -s danger-full-access ... -o <file> <prompt>`, diff in the prompt.
set -uo pipefail
[ "${1:-}" = exec ] || { echo "fake codex: expected exec" >&2; exit 64; }
shift
out="" prompt="" sandbox="" cd="" skip=0
while [ $# -gt 0 ]; do
  case "$1" in
    -o|--output-last-message) out="$2"; shift 2 ;;
    -s|--sandbox) sandbox="$2"; shift 2 ;;
    -C|--cd) cd="$2"; shift 2 ;;
    --color|-m|--model|-c) shift 2 ;;
    --skip-git-repo-check) skip=1; shift ;;
    --ephemeral) shift ;;
    *) prompt="$1"; shift ;;
  esac
done
[ -z "$cd" ] || cd "$cd" || exit 65
n=$(( $(ls "$FAKE_LOG" | grep -c '^stdin\.' || true) + 1 ))
cat >"$FAKE_LOG/actual-stdin.$n"
printf '%s' "$prompt" >"$FAKE_LOG/prompt.$n"
awk 'found { print } /^--- supplied diff: / { found=1 }' "$FAKE_LOG/prompt.$n" >"$FAKE_LOG/stdin.$n"
kind=standard
grep -q "routine code review" "$FAKE_LOG/prompt.$n" && kind=neutral
files="$(grep '^diff --git ' "$FAKE_LOG/stdin.$n" | sed 's#.* b/##' | sort -u | tr '\n' ',')"
archive=no
[ ! -e .git ] && [ "$(cat README.md 2>/dev/null)" = base ] && [ ! -w README.md ] && [ ! -w . ] && archive=yes
echo "$n $kind sandbox=$sandbox bytes=$(wc -c <"$FAKE_LOG/stdin.$n" | tr -d ' ') archive=$archive skip_git_check=$skip stdin_bytes=$(wc -c <"$FAKE_LOG/actual-stdin.$n" | tr -d ' ') files=$files" >>"$FAKE_LOG/calls"
echo "$n optional_locks=${GIT_OPTIONAL_LOCKS:-unset}" >>"$FAKE_LOG/locks"
added="$(grep '^+' "$FAKE_LOG/stdin.$n" || true)"
has() { grep -q "$1" <<<"$added"; }
reply() { printf '%s\n' "$1" >"$out"; }
if has MARK_REFUSE_ALWAYS || { has MARK_REFUSE_STANDARD && [ "$kind" = standard ]; }; then
  reply "I'm sorry, but I can't help with that request because it was flagged by our cybersecurity policy."
elif has MARK_DOUBLE_TIMEOUT; then
  sleep 1; exit 0
elif has MARK_ERROR; then
  echo "stream error: 500" >&2; exit 1
elif has MARK_MUTATE_TREE; then
  chmod u+w README.md 2>/dev/null && printf 'mutated\n' > README.md
  reply "READ: README.md
REPOSITORY: readable
VERDICT: clean"
elif has MARK_REPOSITORY_UNREADABLE; then
  reply "READ: README.md
REPOSITORY: unreadable
VERDICT: clean"
elif has MARK_SANDBOX_FAILURE; then
  reply "READ: README.md
sandbox could not start
REPOSITORY: readable
VERDICT: clean"
elif has MARK_NO_REPOSITORY; then
  reply "READ: README.md
Nothing to report.
VERDICT: clean"
elif has MARK_REPO_WRONG_POS; then
  reply "READ: README.md
REPOSITORY: readable
One more remark after REPOSITORY.
VERDICT: clean"
elif has MARK_NO_READ; then
  reply "Nothing to report.
REPOSITORY: readable
VERDICT: clean"
elif has MARK_BAD_READ; then
  reply "READ: does-not-exist-in-tree.txt
REPOSITORY: readable
VERDICT: clean"
elif has MARK_READ_DIR; then
  reply "READ: p
REPOSITORY: readable
VERDICT: clean"
elif has MARK_READ_ABS; then
  reply "READ: /README.md
REPOSITORY: readable
VERDICT: clean"
elif has MARK_FINDING; then
  reply "- a.txt:1: the counter is never reset
READ: README.md
REPOSITORY: readable
VERDICT: findings"
elif has MARK_NOVERDICT; then
  reply "READ: README.md
REPOSITORY: readable
I looked at it and it seems fine."
elif has MARK_TRAILING; then
  reply "READ: README.md
REPOSITORY: readable
VERDICT: clean
Also, one more thought."
elif has MARK_BOLD; then
  reply "Nothing to report.
**READ: README.md**
**REPOSITORY: readable**
**VERDICT: clean**"
elif has MARK_MENTION_FILTER; then
  reply "The content filter detection and the cybersecurity wording look right.
READ: README.md
REPOSITORY: readable
VERDICT: clean"
else
  reply "Nothing to report.
READ: README.md
REPOSITORY: readable
VERDICT: clean"
fi
EOF
chmod +x "$T/bin/codex"
export REVIEW_CODEX_BIN="$T/bin/codex"

# Kimi and agy have one-argument prompt interfaces and answer on stdout. Their fakes pin the exact
# permission flags, archived cwd and closed stdin; the production result parser is the Codex one.
for reviewer in kimi agy; do
  cat >"$T/bin/$reviewer" <<'EOF'
#!/usr/bin/env bash
set -uo pipefail
reviewer="$(basename "$0")"
if [ "$reviewer" = agy ]; then
  [ "${1:-}" = --dangerously-skip-permissions ] || { echo "fake agy: missing permission flag" >&2; exit 64; }
  shift
fi
[ "${1:-}" = -p ] && [ $# -eq 2 ] || { echo "fake $reviewer: expected -p PROMPT" >&2; exit 64; }
prompt="$2"
n=$(( $(ls "$FAKE_LOG" | grep -c '^stdin\.' || true) + 1 ))
cat >"$FAKE_LOG/actual-stdin.$n"
printf '%s' "$prompt" >"$FAKE_LOG/prompt.$n"
awk 'found { print } /^--- supplied diff: / { found=1 }' "$FAKE_LOG/prompt.$n" >"$FAKE_LOG/stdin.$n"
archive=no
[ ! -e .git ] && [ "$(cat README.md 2>/dev/null)" = base ] && [ ! -w README.md ] && [ ! -w . ] && archive=yes
echo "$n $reviewer archive=$archive stdin_bytes=$(wc -c <"$FAKE_LOG/actual-stdin.$n" | tr -d ' ')" >>"$FAKE_LOG/calls"
echo "$n optional_locks=${GIT_OPTIONAL_LOCKS:-unset}" >>"$FAKE_LOG/locks"
case "$prompt" in
  *MARK_REFUSE_ALWAYS*) printf "I'm sorry, but I can't help with that request because it was flagged by our cybersecurity policy.\n" ;;
  *MARK_DOUBLE_TIMEOUT*) sleep 1; exit 0 ;;
  *MARK_ERROR*) echo "stream error: 500" >&2; exit 1 ;;
  *MARK_REPOSITORY_UNREADABLE*) printf 'READ: README.md\nREPOSITORY: unreadable\nVERDICT: clean\n' ;;
  *MARK_SANDBOX_FAILURE*) printf 'READ: README.md\nbwrap: operation not permitted\nREPOSITORY: readable\nVERDICT: clean\n' ;;
  *MARK_NO_REPOSITORY*) printf 'READ: README.md\nNothing to report.\nVERDICT: clean\n' ;;
  *MARK_REPO_WRONG_POS*) printf 'READ: README.md\nREPOSITORY: readable\nRemark after REPOSITORY.\nVERDICT: clean\n' ;;
  *MARK_NO_READ*) printf 'Nothing to report.\nREPOSITORY: readable\nVERDICT: clean\n' ;;
  *MARK_BAD_READ*) printf 'READ: missing-file.txt\nREPOSITORY: readable\nVERDICT: clean\n' ;;
  *MARK_FINDING*) printf -- '- a.txt:1: the counter is never reset\nREAD: README.md\nREPOSITORY: readable\nVERDICT: findings\n' ;;
  *MARK_NOVERDICT*) printf 'READ: README.md\nREPOSITORY: readable\nI looked at it and it seems fine.\n' ;;
  *MARK_TRAILING*) printf 'READ: README.md\nREPOSITORY: readable\nVERDICT: clean\nTrailing line.\n' ;;
  *) printf 'Nothing to report.\nREAD: README.md\nREPOSITORY: readable\nVERDICT: clean\n' ;;
esac
EOF
  chmod +x "$T/bin/$reviewer"
done
export REVIEW_KIMI_BIN="$T/bin/kimi" REVIEW_AGY_BIN="$T/bin/agy"

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
review() { (cd "$W" && bash "$REVIEW" ${REVIEW_TEST_WRITER:+--writer "$REVIEW_TEST_WRITER"} "$@"); }
stamp_writer() { # stamp_writer <model> [round] [branch]
  local m="$1" r="${2:-1}" b="${3:-feature}" extra=()
  [ "$m" = agy ] && extra=(--agy-diff-sha256 "1111111111111111111111111111111111111111111111111111111111111111")
  python3 -I "$ROOT_DIR/scripts/review/writer_provenance.py" stamp-commit --repo "$W" \
    --model "$m" --round "$r" --branch "$b" \
    --brief-sha256 "2222222222222222222222222222222222222222222222222222222222222222" \
    "${extra[@]}" >/dev/null
}
note() { g notes --ref=refs/notes/concrete-review show "${1:-HEAD}" 2>/dev/null; }
field() { note "${2:-HEAD}" | sed -n "s/^$1: //p"; }
sha256_file() { if command -v sha256sum >/dev/null 2>&1; then sha256sum "$1" | cut -d' ' -f1; else shasum -a 256 "$1" | cut -d' ' -f1; fi; }

echo "=== round 1: full diff, findings, note written and pushed ==="
g checkout -qb feature
commit_file old.txt "MARK_FINDING in the first change" "first change"
review >"$T/out1" 2>&1; rc=$?
check "findings exit 1 (got $rc)" test "$rc" -eq 1
check "one reviewer call" test "$(calls)" -eq 1
check "reviewer has full access only to a read-only archived tree, with stdin closed" \
  grep -q "sandbox=danger-full-access .*archive=yes skip_git_check=1 stdin_bytes=0" "$FAKE_LOG/calls"
# Bug 209: every reviewer call runs without git's optional locks, so a reviewer command stopped
# mid-`git status` cannot leave the caller's worktree with a stale index.lock.
check "reviewer runs with GIT_OPTIONAL_LOCKS=0" grep -q " optional_locks=0$" "$FAKE_LOG/locks"
check "no reviewer call ran with optional locks" bash -c '! grep -v " optional_locks=0$" "$1" | has_match .' _ "$FAKE_LOG/locks"
check "round 1 prompt carries the verdict rule" grep -q "^VERDICT: findings$" "$FAKE_LOG/prompt.1"
check "round 1 prompt requires READ: <path> evidence" grep -q "READ: <path>" "$FAKE_LOG/prompt.1"
check "round 1 prompt requires repository readability evidence" grep -q "REPOSITORY: readable.*REPOSITORY: unreadable" "$FAKE_LOG/prompt.1"
check "the standard prompt carries the permanent owner rule" \
  grep -Fq 'OWNER RULE (2026-11-08, verbatim: "ALWAYS tell the reviewrs to do the best long term thing rather than cuitting corners"): judge and fix by the best long-term design. Never cut corners for speed, a smaller diff or fewer rounds. As a reviewer, report any shortcut, containment patch or corner cut as a real finding and say what the long-term fix is. As a writer, implement the structural fix, not a containment patch; if staging is truly unavoidable, record the long-term design and name its roadmap row.' "$FAKE_LOG/prompt.1"
check "round 1 reviews the full diff from the merge-base" grep -q "files=old.txt,$" "$FAKE_LOG/calls"
R1="$(g rev-parse HEAD)"
BASE="$(g merge-base HEAD origin/main)"
FORMAT_RE='^verdict: (clean|findings|blocked)
rounds: [0-9]+
reviewer: codex
writer: claude
utc: [0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z
base: [0-9a-f]{40}
pieces: [0-9]+$'
n1="$(note "$R1")"
if [[ "$n1" =~ $FORMAT_RE ]]; then ok "note has exactly the seven common fields, in order"; else no "note format: $n1"; fi
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
check "clean note records repository: readable" test "$(field repository)" = readable
R2="$(g rev-parse HEAD)"
P2="$W/.review/$R2-2.prompt.1.txt"
check "the exact prompt is saved next to the raw output" cmp -s "$P2" "$FAKE_LOG/prompt.2"
P2_SHA="$(sha256_file "$P2")"
check "the raw output records the saved prompt's sha256" \
  bash -c "sed -n 's/^prompt sha256: //p' '$W/.review/$R2-2.md' | has_match -x '$P2_SHA'"
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
LEGACY_NOTE="$(printf 'verdict: clean\nrounds: 2\nreviewer: codex\nbase: %s\npieces: 1\nutc: 2026-10-22T00:00:00Z' "$B40")"
g notes --ref=refs/notes/concrete-review add -f -m "$LEGACY_NOTE" HEAD
out="$(cd "$W" && bash "$VERIFY" HEAD 2>&1)"; rc=$?
check "a legacy clean note without repository: readable is not clean (exit 1, got $rc)" test "$rc" -eq 1
check "the legacy-note refusal names the missing key" grep -q "missing repository: readable" <<<"$out"
# Even with repository evidence, a note without writer is legacy, never clean provenance.
g notes --ref=refs/notes/concrete-review add -f -m "$LEGACY_NOTE
repository: readable" HEAD
out="$(cd "$W" && bash "$VERIFY" HEAD 2>&1)"; rc=$?
check "a readable clean note without writer is legacy, not clean (exit 1, got $rc)" test "$rc" -eq 1
check "the legacy-writer refusal names the missing key" grep -q "missing writer (legacy note)" <<<"$out"
# A current note notes.sh put wrote (keys in its own order, writer and repository after utc) verifies
# when its base is an ancestor of HEAD, and is refused when its base is not an ancestor of HEAD.
VALID_BASE="$(g rev-parse main)"
g notes --ref=refs/notes/concrete-review add -f -m "$(printf 'verdict: clean\nrounds: 2\nreviewer: codex\nbase: %s\npieces: 1\nutc: 2026-10-22T00:00:00Z\nwriter: claude\nrepository: readable' "$VALID_BASE")" HEAD
(cd "$W" && bash "$VERIFY" HEAD >"$T/vc" 2>&1); rc=$?
check "a readable independent clean note with utc before writer verifies (exit 0, got $rc)" test "$rc" -eq 0
g notes --ref=refs/notes/concrete-review add -f -m "$LEGACY_NOTE
writer: claude
repository: readable" HEAD
out="$(cd "$W" && bash "$VERIFY" HEAD 2>&1)"; rc=$?
check "a clean note whose base is not an ancestor of HEAD is refused without --required-base (exit 1, got $rc)" test "$rc" -eq 1
check "non-ancestor base refusal names base and commit" grep -q "base $B40 is not an ancestor-or-equal of" <<<"$out"
g notes --ref=refs/notes/concrete-review add -f -m "$LEGACY_NOTE" HEAD
review >"$T/legacy-rerun" 2>&1; rc=$?
check "review.sh re-reviews a legacy clean HEAD instead of short-circuiting (exit $rc)" \
  test "$rc" -eq 0 -a "$(field repository)" = readable -a "$(field writer)" = claude -a "$(field rounds)" = 3
# The landing bot's keys went with it: a verdict carried across a rebase, or a landing order, is no
# longer a key a review note may hold.
for extra in "carried-from: $B40" "order: 1"; do
  g notes --ref=refs/notes/concrete-review add -f -m "$(printf 'verdict: clean\nrounds: 2\nreviewer: codex\nbase: %s\npieces: 1\nutc: 2026-10-22T00:00:00Z\n%s' "$B40" "$extra")" HEAD
  out="$(cd "$W" && bash "$VERIFY" HEAD 2>&1)"; rc=$?
  check "a note with '${extra%%:*}' is malformed: exit 2, bad ${extra%%:*} (got $rc)" test "$rc" -eq 2 -a -n "$(printf '%s' "$out" | grep -F "malformed review note (bad ${extra%%:*})")"
done
g notes --ref=refs/notes/concrete-review add -f -m "$(printf 'verdict: clean\nrounds: 2\nreviewer: codex\nutc: 2026-10-22T00:00:00Z\nbase: %s\npieces: 1\nsurprise: 1' "$B40")" HEAD
(cd "$W" && bash "$VERIFY" HEAD >/dev/null 2>&1); rc=$?
check "an unknown key is malformed: exit 2 (got $rc)" test "$rc" -eq 2
g notes --ref=refs/notes/concrete-review add -f -m "$(printf 'verdict: findings\nverdict: clean\nrounds: 2\nreviewer: codex\nutc: 2026-10-22T00:00:00Z\nbase: %s\npieces: 1' "$B40")" HEAD
(cd "$W" && bash "$VERIFY" HEAD >/dev/null 2>&1); rc=$?
check "a repeated key is malformed: exit 2 (got $rc)" test "$rc" -eq 2
g notes --ref=refs/notes/concrete-review remove HEAD 2>/dev/null

echo "=== note schema transition: old main verify.sh vs new verify.sh ==="
# Obtain main's pre-transition verify.sh from origin/main when available and pre-transition;
# otherwise reconstruct main's 6-key verify.sh from legacy/scripts/review/verify.sh (identical
# except for dropping carried-from|order per 2026-11-08-landing-shrink).
OLD_VERIFY="$T/old-verify.sh"
if git -C "$ROOT_DIR" show origin/main:scripts/review/verify.sh >"$OLD_VERIFY" 2>/dev/null \
   && ! grep -q "writer_provenance" "$OLD_VERIFY"; then
  :
else
  sed -e '/carried-from|order/d' "$ROOT_DIR/legacy/scripts/review/verify.sh" >"$OLD_VERIFY"
fi
chmod +x "$OLD_VERIFY"
TRANS_HEAD="$(g rev-parse HEAD)"
OLD_BOOTSTRAP_NOTE="$(printf 'verdict: clean\nrounds: 1\nreviewer: codex\nutc: 2026-11-08T00:00:00Z\nbase: %s\npieces: 1' "$VALID_BASE")"
g notes --ref=refs/notes/concrete-review add -f -m "$OLD_BOOTSTRAP_NOTE" "$TRANS_HEAD"
(cd "$W" && bash "$OLD_VERIFY" "$TRANS_HEAD" >/dev/null 2>&1); rc=$?
check "transition: main's old verify.sh accepts main's 6-key clean bootstrap note (exit 0, got $rc)" test "$rc" -eq 0
out="$(cd "$W" && bash "$VERIFY" "$TRANS_HEAD" 2>&1)"; rc=$?
check "transition: new verify.sh refuses the 6-key note as clean provenance (exit 1, got $rc)" test "$rc" -eq 1
check "transition: new verify.sh explains the 6-key note lacks repository: readable" grep -q "missing repository: readable" <<<"$out"
NEW_SCHEMA_NOTE="$(printf 'verdict: clean\nrounds: 2\nreviewer: codex\nwriter: claude\nutc: 2026-11-08T00:00:00Z\nbase: %s\npieces: 1\nrepository: readable' "$VALID_BASE")"
g notes --ref=refs/notes/concrete-review add -f -m "$NEW_SCHEMA_NOTE" "$TRANS_HEAD"
(cd "$W" && bash "$VERIFY" "$TRANS_HEAD" >/dev/null 2>&1); rc=$?
check "transition: new verify.sh accepts the 8-key note with writer and repository: readable (exit 0, got $rc)" test "$rc" -eq 0
(cd "$W" && bash "$OLD_VERIFY" "$TRANS_HEAD" >/dev/null 2>&1); rc=$?
check "transition: main's old verify.sh rejects the 8-key note (exit 2, got $rc), why bootstrap uses main's review.sh" test "$rc" -eq 2
g notes --ref=refs/notes/concrete-review remove "$TRANS_HEAD" 2>/dev/null

echo "=== verify.sh and review.sh: required base coverage & delta base-widening prevention (A -> B -> C) ==="
COV_A="$(g rev-parse main)"
g checkout -q main && g checkout -qb coverage-sibling
commit_file sibling.txt "sibling commit S" "S"
COV_S="$(g rev-parse HEAD)"
g checkout -q main && g checkout -qb coverage
commit_file b_only.txt "unreviewed intermediate B" "B"
COV_B="$(g rev-parse HEAD)"
commit_file c_only.txt "reviewed head C" "C"
COV_C="$(g rev-parse HEAD)"
coverage_note() { # base: a current clean note on C whose review began at this base
  g notes --ref=refs/notes/concrete-review add -f -m "$(printf 'verdict: clean\nrounds: 1\nreviewer: codex\nwriter: claude\nutc: 2026-11-08T00:00:00Z\nbase: %s\npieces: 1\nrepository: readable' "$1")" "$COV_C"
}
coverage_note "$COV_S"
out="$(cd "$W" && bash "$VERIFY" "$COV_C" 2>&1)"; rc=$?
check "a note based at sibling commit S (not an ancestor of C) is refused by verify.sh (exit 1, got $rc)" test "$rc" -eq 1
check "sibling-base refusal names S and C" grep -q "base $COV_S is not an ancestor-or-equal of $COV_C" <<<"$out"
coverage_note "$COV_B"
(cd "$W" && bash "$VERIFY" "$COV_C" >/dev/null 2>&1); rc=$?
check "without a coverage requirement the clean note at B verifies (exit 0, got $rc)" test "$rc" -eq 0
out="$(cd "$W" && bash "$VERIFY" --required-base "$COV_A" "$COV_C" 2>&1)"; rc=$?
check "a note based at unreviewed B does not cover required base A (exit 1, got $rc)" test "$rc" -eq 1
check "the coverage refusal names both bases" bash -c "grep -q 'base $COV_B.*required base $COV_A' <<<'$out'"
coverage_note "$COV_A"
(cd "$W" && bash "$VERIFY" --required-base "$COV_A" "$COV_C" >/dev/null 2>&1); rc=$?
check "a note based at main A covers required base A (exit 0, got $rc)" test "$rc" -eq 0
g notes --ref=refs/notes/concrete-review remove "$COV_C" 2>/dev/null

# Delta base-widening reproducer (A -> B -> C): if B carries a prior note (findings or clean) whose
# base is B (so A..B was never reviewed), running review.sh on C with base A must NOT start the delta
# at B and stamp base: A over B..C alone; it must review the full A..C diff.
for b_verdict in findings clean; do
  reset_fake
  b_repo=""
  [ "$b_verdict" = clean ] && b_repo=$'\nrepository: readable'
  g notes --ref=refs/notes/concrete-review add -f -m "$(printf 'verdict: %s\nrounds: 1\nreviewer: codex\nwriter: claude\nutc: 2026-11-08T00:00:00Z\nbase: %s\npieces: 1%s' "$b_verdict" "$COV_B" "$b_repo")" "$COV_B"
  review --base "$COV_A" >"$T/widen-$b_verdict" 2>&1; rc=$?
  check "delta base-widening ($b_verdict on B with base B): review on C against A exits 0 (got $rc)" test "$rc" -eq 0
  check "delta base-widening ($b_verdict on B with base B): reviews full A..C (both b_only.txt and c_only.txt)" \
    grep -q "files=b_only.txt,c_only.txt,$" "$FAKE_LOG/calls"
  check "delta base-widening ($b_verdict on B with base B): prompt states complete change against A, not delta from B" \
    grep -q "complete change of a branch against its base commit $COV_A" "$FAKE_LOG/prompt.1"
  check "delta base-widening ($b_verdict on B with base B): round increments to 2 and note base is A" \
    test "$(field rounds "$COV_C")/$(field base "$COV_C")" = "2/$COV_A"
  g notes --ref=refs/notes/concrete-review remove "$COV_C" 2>/dev/null
done
g notes --ref=refs/notes/concrete-review remove "$COV_B" 2>/dev/null

echo "=== independent reviewers and writer provenance ==="
for reviewer in kimi agy; do
  reset_fake
  g checkout -q main && g checkout -qb "reviewer-$reviewer"
  commit_file "$reviewer.txt" "clean review by $reviewer" "Codex-written branch"
  stamp_writer codex 1 "reviewer-$reviewer"
  review --reviewer "$reviewer" >"$T/reviewer-$reviewer" 2>&1; rc=$?
  check "Codex-stamped branch reviewed clean by $reviewer without --writer flag (exit $rc)" test "$rc" -eq 0
  check "$reviewer note records reviewer $reviewer and derived writer codex" \
    test "$(field reviewer)/$(field writer)" = "$reviewer/codex"
  check "verify accepts the independent $reviewer note and cross-checks writer provenance" \
    bash -c "cd '$W' && bash '$VERIFY' HEAD >/dev/null 2>&1"
  check "$reviewer runs in the read-only archive with stdin closed" grep -q "$reviewer archive=yes stdin_bytes=0" "$FAKE_LOG/calls"
  check "$reviewer runs with optional locks off" grep -q " optional_locks=0$" "$FAKE_LOG/locks"
done
# --writer is an optional assertion checked against derived writer provenance.
reset_fake
g checkout -q main && g checkout -qb writer-assert
commit_file assert.txt "agy-written" "agy commit"
stamp_writer agy 1 writer-assert
REVIEW_TEST_WRITER=codex review --reviewer kimi >"$T/writer-mismatch" 2>&1; rc=$?
check "mismatched --writer assertion is refused (exit 2, got $rc)" test "$rc" -eq 2
check "mismatched --writer error names the derived writer" grep -q "does not match derived writer provenance 'agy'" "$T/writer-mismatch"
before="$(calls)"
review --reviewer agy >"$T/self-review" 2>&1; rc=$?
check "reviewer == derived writer is refused without --writer flag (exit 2, got $rc)" test "$rc" -eq 2
check "self-review refusal calls no reviewer" test "$(calls)" = "$before"
REVIEW_TEST_WRITER=agy review --reviewer kimi >"$T/writer-match" 2>&1; rc=$?
check "matching --writer assertion succeeds (exit $rc)" test "$rc" -eq 0
# Multi-writer branch across rounds (round 1 codex, round 2 agy): reviewer must be none of them.
reset_fake
g checkout -q main && g checkout -qb multi-writer
commit_file mw1.txt "round 1 by codex" "mw1"
stamp_writer codex 1 multi-writer
commit_file mw2.txt "round 2 by agy" "mw2"
stamp_writer agy 2 multi-writer
review --reviewer codex >"$T/mw-codex" 2>&1; rc=$?
check "multi-writer branch (codex,agy) refuses codex as reviewer (exit 2, got $rc)" test "$rc" -eq 2
review --reviewer agy >"$T/mw-agy" 2>&1; rc=$?
check "multi-writer branch (codex,agy) refuses agy as reviewer (exit 2, got $rc)" test "$rc" -eq 2
review --reviewer kimi >"$T/mw-kimi" 2>&1; rc=$?
check "multi-writer branch (codex,agy) accepts kimi as independent reviewer (exit 0, got $rc)" test "$rc" -eq 0
check "multi-writer note records writer: codex,agy" test "$(field writer)" = "codex,agy"
(cd "$W" && bash "$VERIFY" HEAD >/dev/null 2>&1); rc=$?
check "verify.sh accepts clean note on multi-writer branch (exit 0, got $rc)" test "$rc" -eq 0
LOG_BLOB="$(python3 -I "$ROOT_DIR/scripts/review/writer_provenance.py" export-log --repo "$W" --base "$(g merge-base HEAD main)" --head HEAD --write-blob)"
check "export-log --write-blob writes a git blob that verifies multi-writer provenance" \
  python3 -I "$ROOT_DIR/scripts/review/writer_provenance.py" verify --repo "$W" --base "$(g merge-base HEAD main)" --head HEAD --claimed-writer "codex,agy" --reviewer kimi --log-object "$LOG_BLOB"
# Unstamped human/pre-harness commit + stamped codex commit -> derived writer is claude,codex
# (unstamped commits are explicitly attributed to claude, never silently treated as written by nobody).
reset_fake
g checkout -q main && g checkout -qb human-plus-codex
commit_file human.txt "unstamped human commit" "human commit"
commit_file bot.txt "stamped codex commit" "bot commit"
stamp_writer codex 1 human-plus-codex
review --reviewer codex >"$T/hc-codex" 2>&1; rc=$?
check "unstamped+codex branch derives claude,codex and refuses codex reviewer (exit 2, got $rc)" test "$rc" -eq 2
review --reviewer kimi >"$T/hc-kimi" 2>&1; rc=$?
check "unstamped+codex branch reviewed clean by kimi records writer: claude,codex (exit $rc)" \
  test "$rc" -eq 0 -a "$(field writer)" = "claude,codex"
# Negative controls for incomplete or forged writer stamps:
# 1. Writer: trailer alone without a matching writer-log entry in refs/notes/concrete-writer is refused.
reset_fake
g checkout -q main && g checkout -qb trailer-only
commit_file t.txt "trailer only" "$(printf 'forged stamp\n\nWriter: codex')"
review --reviewer kimi >"$T/trailer-only" 2>&1; rc=$?
check "a commit with a Writer: trailer but no author stamp or log entry is refused (exit 2, got $rc)" test "$rc" -eq 2
g checkout -q main && g checkout -qb author-and-trailer-no-log
commit_file t2.txt "no log" "no log"
stamp_writer codex 1 author-and-trailer-no-log
g notes --ref=refs/notes/concrete-writer remove HEAD
review --reviewer kimi >"$T/no-log" 2>&1; rc=$?
check "a commit with writer author and Writer: trailer but no writer-log entry is refused by review.sh (exit 2, got $rc)" test "$rc" -eq 2
check "no-log refusal says a Writer: trailer alone is never enough" grep -q "Writer: trailer alone is never enough" "$T/no-log"
NO_LOG_BASE="$(g merge-base HEAD main)"
g notes --ref=refs/notes/concrete-review add -f -m "$(printf 'verdict: clean\nrounds: 1\nreviewer: kimi\nwriter: codex\nutc: 2026-11-08T00:00:00Z\nbase: %s\npieces: 1\nrepository: readable' "$NO_LOG_BASE")" HEAD
out="$(cd "$W" && bash "$VERIFY" HEAD 2>&1)"; rc=$?
check "verify.sh also refuses a clean note when the commit's writer-log entry is missing (exit 1, got $rc)" test "$rc" -eq 1
check "verify.sh no-log refusal names the missing writer-log entry" grep -q "no matching writer-log entry" <<<"$out"
# 2. agy commit with mismatched agy-diff-sha256 in writer-log is refused.
g checkout -q main && g checkout -qb agy-bad-hash
commit_file abh.txt "agy" "agy"
stamp_writer agy 1 agy-bad-hash
ABH_SHA="$(g rev-parse HEAD)"
g notes --ref=refs/notes/concrete-writer show "$ABH_SHA" | sed 's/^agy-diff-sha256: .*/agy-diff-sha256: 9999999999999999999999999999999999999999999999999999999999999999/' >"$T/bad-agy-log"
g notes --ref=refs/notes/concrete-writer add -f -F "$T/bad-agy-log" "$ABH_SHA"
review --reviewer kimi >"$T/agy-bad-out" 2>&1; rc=$?
check "agy commit whose writer-log agy-diff-sha256 mismatches its trailer is refused (exit 2, got $rc)" test "$rc" -eq 2
# 3. Clean note whose writer: claims claude on a codex-stamped branch is refused by verify.sh.
g checkout -q reviewer-kimi
SELF_BASE="$(g merge-base HEAD main)"
g notes --ref=refs/notes/concrete-review add -f -m "$(printf 'verdict: clean\nrounds: 1\nreviewer: agy\nwriter: claude\nutc: 2026-11-08T00:00:00Z\nbase: %s\npieces: 1\nrepository: readable' "$SELF_BASE")" HEAD
out="$(cd "$W" && bash "$VERIFY" HEAD 2>&1)"; rc=$?
check "verify.sh refuses a clean note whose writer field disagrees with commit provenance (exit 1, got $rc)" test "$rc" -eq 1
check "provenance-mismatch refusal names both claimed and derived writers" grep -q "note writer 'claude' does not match derived writer provenance 'codex'" <<<"$out"
g notes --ref=refs/notes/concrete-review add -f -m "$(printf 'verdict: clean\nrounds: 1\nreviewer: agy\nwriter: agy\nutc: 2026-11-08T00:00:00Z\nbase: %s\npieces: 1\nrepository: readable' "$SELF_BASE")" HEAD
out="$(cd "$W" && bash "$VERIFY" HEAD 2>&1)"; rc=$?
check "verify refuses a clean note whose reviewer equals writer (exit 1, got $rc)" test "$rc" -eq 1
check "self-reviewed-note refusal names the rule" grep -q "agy reviewed its own work" <<<"$out"
g notes --ref=refs/notes/concrete-review add -f -m "$(printf 'verdict: clean\nrounds: 1\nreviewer: agy\nutc: 2026-11-08T00:00:00Z\nbase: %s\npieces: 1\nrepository: readable' "$SELF_BASE")" HEAD
out="$(cd "$W" && bash "$VERIFY" HEAD 2>&1)"; rc=$?
check "verify refuses a clean note without writer as legacy (exit 1, got $rc)" test "$rc" -eq 1
check "missing-writer note refusal says legacy" grep -q "missing writer (legacy note)" <<<"$out"

echo "=== verdict and reviewer-output contract parsing ==="
parse_case() { # parse_case <marker> <want-exit> <want-verdict> <label> [reviewer]
  local marker="$1" want="$2" wantv="$3" label="$4" rev="${5:-codex}" rc tmo="${REVIEW_TIMEOUT:-1500}"
  [ "$marker" = MARK_DOUBLE_TIMEOUT ] && tmo=0.05
  reset_fake
  g checkout -q main && g checkout -qb "parse-$rev-$marker"
  commit_file "p/$marker.txt" "$(printf 'line %s\n' $(seq 1 20))
$marker" "case $marker"
  [ "$rev" = codex ] || stamp_writer codex 1 "parse-$rev-$marker"
  REVIEW_TIMEOUT="$tmo" review --reviewer "$rev" >/dev/null 2>&1; rc=$?
  check "$label (exit $rc, verdict $(field verdict))" test "$rc" = "$want" -a "$(field verdict)" = "$wantv"
}
parse_case MARK_BOLD 0 clean "markdown-emphasised READ/REPOSITORY/VERDICT lines are accepted"
parse_case MARK_TRAILING 5 blocked "text after the verdict line is not a verdict"
parse_case MARK_NOVERDICT 5 blocked "a reply with no verdict line is blocked, not clean"
parse_case MARK_ERROR 5 blocked "a reviewer that exits nonzero is blocked"
parse_case MARK_REPOSITORY_UNREADABLE 5 blocked "an unreadable repository is blocked, never clean"
parse_case MARK_SANDBOX_FAILURE 5 blocked "a sandbox failure text overrides VERDICT: clean"
parse_case MARK_NO_REPOSITORY 5 blocked "a clean verdict without a REPOSITORY line is blocked"
parse_case MARK_REPO_WRONG_POS 5 blocked "REPOSITORY: readable not on the penultimate non-empty line is blocked"
parse_case MARK_NO_READ 5 blocked "a clean verdict without any READ: <path> line is blocked"
parse_case MARK_BAD_READ 5 blocked "a READ: <path> naming a file not in the reviewed commit is blocked"
parse_case MARK_READ_DIR 5 blocked "a READ: <path> naming a directory instead of a file is blocked"
parse_case MARK_READ_ABS 5 blocked "a READ: <path> with an absolute path is blocked"
parse_case MARK_MUTATE_TREE 5 blocked "a reviewer that mutates the read-only archived tree is blocked"
parse_case MARK_MENTION_FILTER 0 clean "a real review that mentions content filters is not a refusal"
# Double timeout under bounded_run (exit 124) must block immediately without halving retry:
# bounded_run makes its 2 standard attempts, and review.sh makes 0 neutral halving-retry calls.
parse_case MARK_DOUBLE_TIMEOUT 5 blocked "a bounded_run double-timeout blocks immediately without halving retry"
check "double-timeout made only bounded_run's 2 standard attempts and 0 neutral halving retries" \
  bash -c "[ '$(calls)' -eq 2 ] && ! grep -q ' neutral ' '$FAKE_LOG/calls'"
# Kimi and agy negative controls across the shared reviewer-output contract.
for r in kimi agy; do
  parse_case MARK_REPOSITORY_UNREADABLE 5 blocked "$r: REPOSITORY: unreadable is blocked" "$r"
  parse_case MARK_NO_REPOSITORY 5 blocked "$r: missing REPOSITORY: readable is blocked" "$r"
  parse_case MARK_REPO_WRONG_POS 5 blocked "$r: non-penultimate REPOSITORY: readable is blocked" "$r"
  parse_case MARK_NO_READ 5 blocked "$r: missing READ: <path> is blocked" "$r"
  parse_case MARK_BAD_READ 5 blocked "$r: nonexistent READ: <path> is blocked" "$r"
  parse_case MARK_ERROR 5 blocked "$r: nonzero exit is blocked" "$r"
  parse_case MARK_DOUBLE_TIMEOUT 5 blocked "$r: double-timeout is blocked in 1 call" "$r"
done

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
check "the neutral prompt carries the permanent owner rule" \
  grep -Fq 'OWNER RULE (2026-11-08, verbatim: "ALWAYS tell the reviewrs to do the best long term thing rather than cuitting corners"): judge and fix by the best long-term design. Never cut corners for speed, a smaller diff or fewer rounds. As a reviewer, report any shortcut, containment patch or corner cut as a real finding and say what the long-term fix is. As a writer, implement the structural fix, not a containment patch; if staging is truly unavoidable, record the long-term design and name its roadmap row.' "$FAKE_LOG/prompt.2"
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
WRITER_NOTES_SHA="$(g rev-parse refs/notes/concrete-writer)"
hook_out="$(cd "$W" && printf 'refs/notes/concrete-review %s refs/notes/concrete-review %s\nrefs/notes/concrete-review-findings %s refs/notes/concrete-review-findings %s\nrefs/notes/concrete-writer %s refs/notes/concrete-writer %s\n' \
  "$NOTES_SHA" "$Z" "$(g rev-parse refs/notes/concrete-review-findings)" "$Z" "$WRITER_NOTES_SHA" "$Z" | bash "$HOOK" 2>&1)"; rc=$?
check "notes-only push (review, findings, writer): hook exits 0 (got $rc)" test "$rc" -eq 0
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
writer: claude
utc: 2026-10-22T00:00:00Z
base: $OTHER_SHA
pieces: 1
repository: readable" "$OTHER_SHA"
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
