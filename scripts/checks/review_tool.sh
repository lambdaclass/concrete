#!/usr/bin/env bash
# R-0544 — the review tool (scripts/review/review.sh, verify.sh, split_diff.py)
# does what docs/REVIEW.md says it does.
#
# No API key is needed: the reviewer is a fake `codex` that answers according to
# MARK_* markers on the ADDED lines of the diff it receives on stdin, and logs
# every call (prompt kind, stdin size, files) so the legs can check what was sent.
# All git work happens in throwaway repositories with a bare repository as
# `origin`. The publication legs exercise real fetches and pushes, including the
# non-fast-forward retry. Other legs keep notes local to their fixture.
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
export REVIEW_GATE_SECRET=must_be_absent
# Bug 209's legs prove review.sh OVERRIDES the caller's setting: the gate runs with optional locks
# explicitly on, so a value inherited from whoever launched it cannot satisfy them.
export GIT_OPTIONAL_LOCKS=1
{
printf '#!/usr/bin/env bash\n'
printf 'FAKE_LOG=%q\nFAKE_VERIFIER=%q\nFAKE_SOURCE=%q\n' "$FAKE_LOG" "$ROOT_DIR/scripts/review/read_evidence.py" "$T/w"
cat <<'EOF'
# Fake reviewer: `codex exec --sandbox danger-full-access ... -o <file> <prompt>`, diff on stdin.
set -uo pipefail
[ "${1:-}" = exec ] || { echo "fake codex: expected exec" >&2; exit 64; }
shift
out="" prompt="" sandbox="" directory="" project_doc_limit=""
while [ $# -gt 0 ]; do
  case "$1" in
    -o|--output-last-message) out="$2"; shift 2 ;;
    -s|--sandbox) sandbox="$2"; shift 2 ;;
    -C|--cd) directory="$2"; shift 2 ;;
    -c) case "$2" in project_doc_max_bytes=*) project_doc_limit="${2#*=}" ;; esac; shift 2 ;;
    --color|-m|--model) shift 2 ;;
    --json|--ephemeral|--skip-git-repo-check) shift ;;
    *) prompt="$1"; shift ;;
  esac
done
n=$(( $(ls "$FAKE_LOG" | grep -c '^stdin\.' || true) + 1 ))
cat >"$FAKE_LOG/stdin.$n"
printf '%s' "$prompt" >"$FAKE_LOG/prompt.$n"
printf '%s\n' "$project_doc_limit" >"$FAKE_LOG/project-doc-limit.$n"
kind=standard
grep -q "routine code review" "$FAKE_LOG/prompt.$n" && kind=neutral
files="$(grep '^diff --git ' "$FAKE_LOG/stdin.$n" | sed 's#.* b/##' | sort -u | tr '\n' ',')"
echo "$n $kind sandbox=$sandbox bytes=$(wc -c <"$FAKE_LOG/stdin.$n" | tr -d ' ') files=$files" >>"$FAKE_LOG/calls"
echo "$n optional_locks=${GIT_OPTIONAL_LOCKS:-unset}" >>"$FAKE_LOG/locks"
printf '%s\n' "${REVIEW_GATE_SECRET-unset}" "${GIT_CONFIG_GLOBAL-unset}" "${TMPDIR:-}" >"$FAKE_LOG/environment.$n"
added="$(grep '^+' "$FAKE_LOG/stdin.$n" || true)"
has() { grep -q "$1" <<<"$added"; }
# Read the actual private tree, execute the requested digest probe, and emit
# the same successful command event schema as codex --json.
printf '%s' "$directory" >"$FAKE_LOG/directory.$n"
reply() { printf '%s\n' "$1" >"$out"; }
if has MARK_REFUSE_ALWAYS; then
  echo '{"type":"turn.failed","error":{"message":"invalid_prompt"}}'
  exit 1
elif has MARK_CLI_REFUSE && [ "$kind" = standard ]; then
  message='Your request was flagged as potentially violating our usage policy. Please try again with a different prompt: https://platform.openai.com/docs/guides/reasoning#advice-on-prompting'
  printf '{"type":"error","message":"%s"}\n{"type":"turn.failed","error":{"message":"%s"}}\n' "$message" "$message"
  touch "$FAKE_LOG/cli-refusal"
  exit 1
elif has MARK_PLAIN_REFUSE && [ "$kind" = standard ]; then
  reply "I'm not able to assist with that request."
  exit 0
elif has MARK_REFUSE_STANDARD && [ "$kind" = standard ]; then
  reply "I can't help with that request."
  exit 0
fi
if has MARK_EXACT; then
  # The exact-tree fixture has one runner-created nonce, added after verification.
  nonce_path=("$directory"/.concrete-review-nonce-*)
  [ "${#nonce_path[@]}" = 1 ] && mv "${nonce_path[0]}" "$TMPDIR/nonce" || exit 2
  python3 -I "$FAKE_VERIFIER" tree "$FAKE_SOURCE" "$(git -C "$FAKE_SOURCE" rev-parse HEAD)" "$directory" || exit 2
  mv "$TMPDIR/nonce" "${nonce_path[0]}" || exit 2
  echo verified >"$FAKE_LOG/identity.$n"
fi
if has MARK_READ_AGENTS; then
  cat "$directory/AGENTS.md" >"$FAKE_LOG/agents.$n" || exit 2
fi
if has MARK_SLEEP; then
  # Create both an ordinary child and a separate session, with an intermediate
  # parent that must be reaped before its detached child can be adopted.
  sleep 9 &
  sleeper=$!
  (
    setsid bash -c 'echo "$$" >"$1"; exec sleep 9' _ "$FAKE_LOG/detached" &
    wait
  ) &
  intermediate=$!
  while [ ! -s "$FAKE_LOG/detached" ]; do sleep 0.01; done
  echo "$$ $sleeper $intermediate $(cat "$FAKE_LOG/detached")" >"$FAKE_LOG/sleeping"
  wait
fi
if has MARK_DISCOVERY; then
  git -C "$directory" rev-parse --show-toplevel >"$FAKE_LOG/discovered" 2>/dev/null
  printf '%s\n' "$?" >"$FAKE_LOG/discovery-status"
fi
if has MARK_WRITE_LINK; then
  printf 'reviewer mutation\n' >"$directory/escape" && touch "$FAKE_LOG/followed-link"
fi
if has MARK_WRITE; then
  printf 'reviewer mutation\n' >"$directory/README.md" && touch "$directory/reviewer-created" && touch "$FAKE_LOG/wrote"
fi
if has MARK_NO_READ; then
  touch "$FAKE_LOG/skipped-read"
else
  probe="$(awk '/run this repository-read probe verbatim/{getline; print}' "$FAKE_LOG/prompt.$n")"
  if has MARK_PRINT_DIGEST; then
    expected="$(git -C "$FAKE_SOURCE" show HEAD:.gitignore | sha256sum)"
    expected="${expected%% *}"
    probe="printf '%s\n' '$expected'"
    touch "$FAKE_LOG/printed-digest"
  fi
  if has MARK_SHIM_PROBE; then
    expected="$(git -C "$FAKE_SOURCE" show HEAD:.gitignore | sha256sum)"
    expected="${expected%% *}"
    mkdir "$TMPDIR/shim"
    printf '#!/bin/sh\nprintf "%%s\\n" "%s"\n' "$expected" >"$TMPDIR/shim/python3"
    chmod +x "$TMPDIR/shim/python3"
    rm "$directory/.gitignore"
    [ "$("$TMPDIR/shim/python3")" = "$expected" ] || exit 2
    digest="$(cd "$directory" && PATH="$TMPDIR/shim:$PATH" bash -c "$probe" 2>"$FAKE_LOG/probe-error")"
    printf '%s\n' "$?" >"$FAKE_LOG/probe-status"
    printf '%s\n' "$probe" >"$FAKE_LOG/probe-command"
    # Even an event claiming success and returning the old content hash must fail.
    digest="$expected"
  else
    digest="$(cd "$directory" && eval "$probe")" || exit 2
  fi
  python3 -I -c '
import json
import shlex
import sys

print(json.dumps({"type": "item.completed", "item": {
    "type": "command_execution", "exit_code": 0, "status": "completed",
    "command": "/bin/bash -lc " + shlex.quote(sys.argv[1]),
    "aggregated_output": sys.argv[2],
}}))
' "$probe" "$digest"
fi
if has MARK_NULL_OUTPUT; then
  echo '{"type":"item.completed","item":{"type":"command_execution","exit_code":0,"status":"completed","command":"unrelated command","aggregated_output":null}}'
fi
if has MARK_BAD_EVENT; then
  echo '[]'
fi
if has MARK_BAD_ITEM; then
  echo '{"type":"item.completed","item":[]}'
fi
if has MARK_UNICODE; then
  printf '{"type":"item.completed","item":{"type":"agent_message","text":"left\342\200\250right"}}\n'
fi
if has MARK_SANDBOX; then
  echo '{"type":"error","message":"execution sandbox could not start"}'
  touch "$FAKE_LOG/reported-error"
fi
if has MARK_MENTION_FILTER; then
  echo '{"type":"item.completed","item":{"type":"command_execution","exit_code":0,"status":"completed","command":"probe report","aggregated_output":"Repository reads failed because the execution sandbox could not start"}}'
  reply "The patch handles sandbox failure and the content filter.
**VERDICT**: clean"
elif has MARK_ERROR; then
  echo "stream error: 500" >&2; exit 1
elif has MARK_FINDING; then
  reply "- a.txt:1: the counter is never reset
**VERDICT**: findings"
elif has MARK_NOVERDICT; then
  reply "I looked at it and it seems fine."
elif has MARK_TRAILING; then
  reply "VERDICT: clean
Also, one more thought."
elif has MARK_BOLD; then
  reply "Nothing to report.
**VERDICT: clean**"
else
  reply "Nothing to report.
VERDICT: clean"
fi
EOF
} >"$T/bin/codex"
chmod +x "$T/bin/codex"
export REVIEW_CODEX_BIN="$T/bin/codex" REVIEW_PUSH=0

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

echo "=== review host contract ==="
reject_args() {
  local diagnostic="$1" rc=0
  shift
  "$@" >"$T/bad-arguments" 2>&1 || rc=$?
  check "$diagnostic (exit $rc)" test "$rc" -eq 2
  check "$diagnostic diagnostic" grep -Fq -- "$diagnostic" "$T/bad-arguments"
}
reject_args 'usage: invoke.sh' /bin/bash "$ROOT_DIR/scripts/review/invoke.sh"
reject_args 'usage: invoke.sh' /bin/bash "$ROOT_DIR/scripts/review/invoke.sh" a b c d e f g
reject_args 'usage: supervise.py' python3 -I "$ROOT_DIR/scripts/review/supervise.py" "$T"
reject_args 'usage: supervise.py' python3 -I "$ROOT_DIR/scripts/review/supervise.py" "$T" BAD -- /bin/sh
reject_args 'usage: read_evidence.py' python3 -I "$ROOT_DIR/scripts/review/read_evidence.py" probe
reject_args 'limit-bytes must be a positive integer' python3 -I "$SPLIT" a b 0
# Exercise the supervisor's non-Linux launch path on Linux CI too.
python3 -I -c 'import runpy, sys; sys.platform = "darwin"; runpy.run_path(sys.argv.pop(1), run_name="__main__")' \
  "$ROOT_DIR/scripts/review/supervise.py" "$T" -- "$(type -P touch)" "$T/unsupported-launched" >"$T/unsupported" 2>&1; rc=$?
check "unsupported supervisor exits before launching a child" test "$rc" -ne 0 -a ! -e "$T/unsupported-launched"
check "unsupported supervisor explains required host facilities" grep -q 'requires Linux with procfs and child subreaping' "$T/unsupported"
python3 -I -c 'import runpy, sys; sys.version_info = (3, 8); runpy.run_path(sys.argv.pop(1), run_name="__main__")' \
  "$ROOT_DIR/scripts/review/supervise.py" --check >"$T/unsupported-python" 2>&1; rc=$?
check "Python preflight rejects 3.8" test "$rc" -ne 0
check "Python preflight gives the minimum version" grep -q 'requires Python 3.9 or newer' "$T/unsupported-python"
if [ "$(uname -s)" != Linux ]; then
  review >"$T/unsupported-review" 2>&1; rc=$?
  check "review driver rejects this host before contacting reviewer" test "$rc" = 2 -a "$(calls)" = 0 -a ! -e "$W/.review"
  check "review driver reports its host requirement" grep -q 'requires Linux with procfs and child subreaping' "$T/unsupported-review"
  echo "check_review_tool: PASS=$PASS FAIL=$FAIL"
  [ "$FAIL" -eq 0 ]
  exit $?
fi

echo "=== round 1: full diff, findings, note written and pushed ==="
g checkout -qb feature
commit_file old.txt "MARK_FINDING in the first change" "first change"
REVIEW_PUSH=1 review >"$T/out1" 2>&1; rc=$?
check "findings exit 1 (got $rc)" test "$rc" -eq 1
check "one reviewer call" test "$(calls)" -eq 1
check "reviewer runs with --sandbox danger-full-access" grep -q "sandbox=danger-full-access" "$FAKE_LOG/calls"
check "reviewer disables automatic project instructions" grep -qx 0 "$FAKE_LOG/project-doc-limit.1"
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
read-evidence: v1
patch-coverage: v1$'
n1="$(note "$R1")"
if [[ "$n1" =~ $FORMAT_RE ]]; then ok "note has exactly the eight fields, in order"; else no "note format: $n1"; fi
check "note: verdict findings" test "$(field verdict)" = findings
check "note: rounds 1" test "$(field rounds)" = 1
check "note: base is the merge-base" test "$(field base)" = "$BASE"
check "note: pieces 1" test "$(field pieces)" = 1
check "raw output at .review/<sha>-1.md" test -s "$W/.review/$R1-1.md"
check "findings recorded under the findings ref" \
  bash -c "git -C '$W' notes --ref=refs/notes/concrete-review-findings show $R1 | has_match 'counter is never reset'"
check "findings note omits the parsed emphasized verdict" bash -c \
  'note="$(git -C "$1" notes --ref=concrete-review-findings show "$2")" && ! grep -q VERDICT <<<"$note"' _ "$W" "$R1"
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
g checkout -q -b range-controls origin/main
RANGE_BASE="$(g rev-parse HEAD)"
commit_file hidden.txt hidden hidden
RANGE_MID="$(g rev-parse HEAD)"
commit_file visible.txt visible visible
range_note() {
  g notes --ref=concrete-review add -f -m "$(printf 'verdict: %s\nrounds: %s\nreviewer: codex\nutc: 2026-11-11T00:00:00Z\nbase: %s\npieces: 1\nread-evidence: v1\npatch-coverage: v1' "$1" "$2" "$3")" HEAD
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
  g notes --ref=concrete-review-findings add -f -m 'ANCESTOR_OPEN_FINDING' HEAD
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
  check "covering $verdict descendant reviews new delta" grep -q '^+resumed$' "$FAKE_LOG/stdin.1"
  check "covering $verdict descendant excludes superseded scope" bash -c '[ -f "$1" ] && ! grep -q "^+old-scope$" "$1"' _ "$FAKE_LOG/stdin.1"
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
git -C "$T/shallow" fetch -q "$W" refs/notes/concrete-review:refs/notes/concrete-review
(cd "$T/shallow" && bash "$VERIFY" --required-base HEAD HEAD >"$T/vr" 2>&1); rc=$?
check "shallow ancestry fails closed with range reason" test "$rc" -eq 2 -a -n "$(grep 'review range does not cover required base' "$T/vr")"
g checkout -q feature
reset_fake

echo "=== verify.sh: missing and malformed notes ==="
commit_file x.txt "x" "unreviewed"
(cd "$W" && bash "$VERIFY" HEAD >/dev/null 2>&1); rc=$?
check "no note: exit 2 (got $rc)" test "$rc" -eq 2
g notes --ref=refs/notes/concrete-review add -f -m "verdict: clean" HEAD
(cd "$W" && bash "$VERIFY" HEAD >/dev/null 2>&1); rc=$?
check "malformed note (verdict line alone): exit 2 (got $rc)" test "$rc" -eq 2
B40="$(printf '%040d' 0 | tr 0 a)"
# Field order does not affect verification.
g notes --ref=refs/notes/concrete-review add -f -m "$(printf 'verdict: clean\nrounds: 2\nreviewer: codex\nbase: %s\npieces: 1\nread-evidence: v1\npatch-coverage: v1\nutc: 2026-10-22T00:00:00Z' "$B40")" HEAD
(cd "$W" && bash "$VERIFY" HEAD >"$T/vc" 2>&1); rc=$?
check "a note with utc last verifies clean (exit 0, got $rc)" test "$rc" -eq 0
(cd "$W" && bash "$VERIFY" --required-base HEAD HEAD >"$T/vr" 2>&1); rc=$?
check "verify.sh refuses a syntactically valid but unknown note base" test "$rc" -eq 2
check "unknown note base fails for range coverage" grep -q 'review range does not cover required base' "$T/vr"
# The landing bot's keys went with it: a verdict carried across a rebase, or a landing order, is no
# longer a key a review note may hold.
for extra in "carried-from: $B40" "order: 1"; do
  g notes --ref=refs/notes/concrete-review add -f -m "$(printf 'verdict: clean\nrounds: 2\nreviewer: codex\nbase: %s\npieces: 1\nread-evidence: v1\npatch-coverage: v1\nutc: 2026-10-22T00:00:00Z\n%s' "$B40" "$extra")" HEAD
  out="$(cd "$W" && bash "$VERIFY" HEAD 2>&1)"; rc=$?
  check "a note with '${extra%%:*}' is malformed: exit 2, bad ${extra%%:*} (got $rc)" test "$rc" -eq 2 -a -n "$(printf '%s' "$out" | grep -F "malformed review note (bad ${extra%%:*})")"
done
g notes --ref=refs/notes/concrete-review add -f -m "$(printf 'verdict: clean\nrounds: 2\nreviewer: codex\nutc: 2026-10-22T00:00:00Z\nbase: %s\npieces: 1\nsurprise: 1' "$B40")" HEAD
(cd "$W" && bash "$VERIFY" HEAD >/dev/null 2>&1); rc=$?
check "an unknown key is malformed: exit 2 (got $rc)" test "$rc" -eq 2
g notes --ref=refs/notes/concrete-review add -f -m "$(printf 'verdict: findings\nverdict: clean\nrounds: 2\nreviewer: codex\nutc: 2026-10-22T00:00:00Z\nbase: %s\npieces: 1\nread-evidence: v1\npatch-coverage: v1' "$B40")" HEAD
(cd "$W" && bash "$VERIFY" HEAD >/dev/null 2>&1); rc=$?
check "a repeated key is malformed: exit 2 (got $rc)" test "$rc" -eq 2
g notes --ref=refs/notes/concrete-review remove HEAD 2>/dev/null

echo "=== historical notes keep round accounting without supplying coverage ==="
reset_fake
g checkout -q main && g checkout -qb historical-note
commit_file historical.txt historical historical
historical_base="$(g rev-parse main)"
g notes --ref=refs/notes/concrete-review add -m "$(printf 'verdict: clean\nrounds: 1\nreviewer: codex\nutc: 2026-10-22T00:00:00Z\nbase: %s\npieces: 1' "$historical_base")" HEAD
(cd "$W" && bash "$VERIFY" --required-base "$historical_base" HEAD) >"$T/historical.verify" 2>&1; rc=$?
check "historical clean note is unqualified" test "$rc" -eq 2
check "historical note diagnostic identifies missing read evidence" grep -q 'predates repository-read evidence' "$T/historical.verify"
commit_file current.txt current current
review >"$T/historical.review" 2>&1; rc=$?
check "review after a historical note succeeds and counts the earlier round" test "$rc" = 0 -a "$(field rounds)" = 2
check "review after a historical note includes the entire branch diff" grep -q 'files=current.txt,historical.txt,$' "$FAKE_LOG/calls"

echo "=== notes from before patch coverage keep their round but force a full diff ==="
reset_fake
g checkout -q main && g checkout -qb old-patch-coverage
commit_file Concrete/Earlier.lean 'def earlier := 1' 'earlier Lean change'
g notes --ref=refs/notes/concrete-review add -m "$(printf 'verdict: clean\nrounds: 1\nreviewer: codex\nutc: 2026-10-22T00:00:00Z\nbase: %s\npieces: 1\nread-evidence: v1' "$historical_base")" HEAD
(cd "$W" && bash "$VERIFY" --required-base "$historical_base" HEAD) >"$T/old-patch.verify" 2>&1; rc=$?
check "old patch coverage cannot qualify as clean" test "$rc" = 2
check "old patch coverage diagnostic identifies the missing version" grep -q 'predates current patch coverage' "$T/old-patch.verify"
commit_file unrelated.txt unrelated 'unrelated change after old review'
review >"$T/old-patch.review" 2>&1; rc=$?
check "a note with read evidence alone still counts toward the round" test "$rc" = 0 -a "$(field rounds)" = 2
check "the next review sends the earlier Lean change" grep -Fxq -- '+def earlier := 1' "$FAKE_LOG/stdin.1"
check "the next review also sends the unrelated change" grep -Fxq -- '+unrelated' "$FAKE_LOG/stdin.2"

echo "=== verdict parsing ==="
parse_case() { # parse_case <marker> <want-exit> <want-verdict> <label>
  local marker="$1" want="$2" wantv="$3" label="$4" rc
  reset_fake
  g checkout -q main && g checkout -qb "parse-$marker"
  commit_file "p/$marker.txt" "$marker" "case $marker"
  review >"$T/parse.log" 2>&1; rc=$?
  check "$label (exit $rc, verdict $(field verdict))" test "$rc" = "$want" -a "$(field verdict)" = "$wantv"
}
parse_case MARK_BOLD 0 clean "markdown-emphasised verdict line is accepted"
parse_case MARK_TRAILING 5 blocked "text after the verdict line is not a verdict"
parse_case MARK_NOVERDICT 5 blocked "a reply with no verdict line is blocked, not clean"
parse_case MARK_ERROR 2 "" "a reviewer that exits nonzero is an error without a note"
parse_case MARK_SANDBOX 2 "" "sandbox failure overrides a clean verdict and successful probe"
check "sandbox error event emitted" test -f "$FAKE_LOG/reported-error"
parse_case MARK_NO_READ 2 "" "missing repository-read evidence is an error"
check "reviewer deliberately skipped read" test -f "$FAKE_LOG/skipped-read"
parse_case MARK_PRINT_DIGEST 2 "" "printing the expected digest without running the read command is rejected"
check "digest-only control executed" test -f "$FAKE_LOG/printed-digest"
parse_case MARK_SHIM_PROBE 2 "" "same probe with missing target and a PATH shim cannot supply evidence"
check "absolute probe fails when its target is missing despite a working shim" \
  grep -q '^FileNotFoundError:.*[.]gitignore' "$FAKE_LOG/probe-error"
check "the exact prompted command was used with the shim on PATH" \
  cmp "$FAKE_LOG/probe-command" <(awk '/run this repository-read probe verbatim/{getline; print}' "$FAKE_LOG/prompt.1")
check "a forged success event with the old content hash cannot produce a note" \
  grep -q 'missing successful repository-read command evidence' "$W/.review/$(g rev-parse HEAD)-1.md"
parse_case MARK_NULL_OUTPUT 0 clean "null output from another command preserves valid read evidence"
parse_case MARK_BAD_EVENT 2 "" "non-object event fails without a note"
check "round report includes malformed event diagnostic" grep -q 'malformed reviewer event on line 2' "$W/.review/$(g rev-parse HEAD)-1.md"
check "review failure surfaces the runner diagnostic on stderr" grep -q 'malformed reviewer event on line 2' "$T/parse.log"
parse_case MARK_BAD_ITEM 2 "" "non-object item fails without a note"
check "round report includes malformed item diagnostic" grep -q 'malformed reviewer item on line 2' "$W/.review/$(g rev-parse HEAD)-1.md"
parse_case MARK_PLAIN_REFUSE 0 clean "plain pre-read refusal reaches a successful neutral retry"
check "plain refusal actually invoked the neutral prompt" grep -q ' neutral ' "$FAKE_LOG/calls"
parse_case MARK_UNICODE 0 clean "literal U+2028 stays inside its JSONL record"
parse_case MARK_CLI_REFUSE 0 clean "CLI usage-policy event reaches a successful neutral retry"
check "CLI refusal event emitted" test -f "$FAKE_LOG/cli-refusal"
check "CLI refusal actually invoked the neutral prompt" grep -q ' neutral ' "$FAKE_LOG/calls"
parse_case MARK_WRITE 0 clean "reviewer writes are confined to the private archive"
check "reviewer completed both writes" test -f "$FAKE_LOG/wrote"
check "caller README unchanged" test "$(cat "$W/README.md")" = base
check "caller has no reviewer-created file" test ! -e "$W/reviewer-created"
check "private archive removed on exit" test ! -e "$(cat "$FAKE_LOG/directory.1")"
check "exact prompt retained" cmp "$FAKE_LOG/prompt.1" "$W/.review/$(g rev-parse HEAD)-1-invocation-1.prompt.txt"

check "reviewer environment excludes caller secret and Git config, scopes temp files" \
  cmp "$FAKE_LOG/environment.1" <(printf '%s\n' unset unset "$(dirname "$(cat "$FAKE_LOG/directory.1")")/tmp")
# Restore the throwaway fixture even when the runner under test is reverted.
g restore README.md
rm -f "$W/reviewer-created"

# A dirty tracked file and an untracked file must never enter the reviewed tree.
reset_fake
g checkout -q main && g checkout -qb exact-tree
commit_file exact.txt 'MARK_EXACT MARK_READ_AGENTS' exact
printf '# Review instructions\n\nPreserve trailing spaces.  \nFinal line without newline' >"$T/expected-agents"
cp "$T/expected-agents" "$W/AGENTS.md"
printf '\000\377binary\n' >"$W/binary"
printf '#!/bin/sh\nexit 0\n' >"$W/executable"
chmod +x "$W/executable"
ln -s exact.txt "$W/link"
mkdir "$W/nested"
ln -s ../link "$W/nested/link"
probe_name=$'!probe\nfile'
printf 'probe path with a newline\n' >"$W/$probe_name"
g add AGENTS.md binary executable link nested/link "$probe_name" && g commit -qm 'archive entry types'
exact_sha="$(g rev-parse HEAD)"
printf dirty >"$W/README.md"
printf dirty >"$W/AGENTS.md"
printf untracked >"$W/untracked"
review >"$T/exact-review.log" 2>&1; rc=$?
check "review invocation sees exact SHA with dirty caller" test "$rc" -eq 0
check "private invocation tree identity verified" test -s "$FAKE_LOG/identity.1"
check "reviewer reads the exact committed AGENTS.md bytes" cmp "$T/expected-agents" "$FAKE_LOG/agents.1"
check "reviewer input names the binary file without dumping its payload" \
  grep -Fxq 'Binary files /dev/null and b/binary differ' "$FAKE_LOG/stdin.1"
# Independently compare archive verifier against the commit, including binary,
# executable and symlink paths, then plant a byte mismatch.
mkdir "$T/exact"
g archive HEAD >"$T/exact.tar"
tar -xf "$T/exact.tar" -C "$T/exact"
check "copy matches committed SHA despite dirty caller" python3 -I "$ROOT_DIR/scripts/review/read_evidence.py" tree "$W" "$(g rev-parse HEAD)" "$T/exact"
printf altered >"$T/exact/README.md"
if python3 -I "$ROOT_DIR/scripts/review/read_evidence.py" tree "$W" "$(g rev-parse HEAD)" "$T/exact" >/dev/null 2>&1; then no "altered archive refused"; else ok "altered archive refused"; fi
g restore AGENTS.md README.md
rm "$W/untracked"
# Resolve relative inputs and the executable in the invoking directory.
mkdir "$T/relative"
printf 'review\n' >"$T/relative/prompt"
printf 'diff\n' >"$T/relative/piece"
(cd "$T/relative" && REVIEW_CODEX_BIN=../bin/codex bash "$ROOT_DIR/scripts/review/invoke.sh" \
  ../w "$(g rev-parse HEAD)" prompt piece output events) >"$T/relative.log" 2>&1; rc=$?
check "relative helper arguments produce caller output and events" test "$rc" = 0 \
  -a -s "$T/relative/output" -a -s "$T/relative/events"
mkdir -p "$W/sub"
commit_file relative.txt relative relative
(cd "$W/sub" && REVIEW_CODEX_BIN=../../bin/codex bash "$REVIEW") >"$T/relative-review.log" 2>&1; rc=$?
check "relative reviewer executable works from caller subdirectory" test "$rc" = 0
reset_fake
printf '#!/bin/sh\ntouch "%s"\n' "$FAKE_LOG/builtin-ran" >"$W/sub/:"
chmod +x "$W/sub/:"
(cd "$W/sub" && REVIEW_CODEX_BIN=: bash "$REVIEW") >"$T/builtin.log" 2>&1; rc=$?
check "builtin name does not resolve to an implicit caller-local executable" test "$rc" = 2 -a ! -e "$FAKE_LOG/builtin-ran"
check "builtin lookup gives the executable diagnostic" grep -q 'reviewer executable not found' "$T/builtin.log"
rm "$W/sub/:"

reset_fake
g checkout -q main && g checkout -qb archive-interpreters
mkdir "$W/launcher-bin" "$T/path-archive"
for interpreter in bash node; do
  # Absolute shebang and builtin marker so the sentinel works even with an empty PATH.
  printf '#!/bin/sh\nprintf ran >"%s"\nexit 99\n' "$FAKE_LOG/archive-$interpreter-ran" \
    >"$W/$interpreter"
  cp "$W/$interpreter" "$W/launcher-bin/$interpreter"
  chmod +x "$W/$interpreter" "$W/launcher-bin/$interpreter"
  printf '#!/usr/bin/env %s\n' "$interpreter" >"$T/bin/reviewer-$interpreter"
  chmod +x "$T/bin/reviewer-$interpreter"
done
g add bash node launcher-bin && g commit -qm 'archive-only PATH interpreters'
path_sha="$(g rev-parse HEAD)"
g archive "$path_sha" >"$T/path-archive.tar"
tar -xf "$T/path-archive.tar" -C "$T/path-archive"
rm "$W/bash" "$W/node" "$W/launcher-bin/bash" "$W/launcher-bin/node"
runner="$ROOT_DIR/scripts/review/invoke.sh"
supervisor="$ROOT_DIR/scripts/review/supervise.py"
python="$(type -P python3)"
for interpreter in bash node; do
  reviewer="$T/bin/reviewer-$interpreter"
  for path_case in empty leading trailing doubled dot relative; do
    case "$path_case" in
      empty) bad_path= ;;
      leading) bad_path=":$PATH" ;;
      trailing) bad_path="$PATH:" ;;
      doubled) bad_path="$T/bin::$PATH" ;;
      dot) bad_path=".:$PATH" ;;
      relative) bad_path="launcher-bin:$PATH" ;;
    esac
    label="$interpreter/$path_case"
    (cd "$W" && PATH="$bad_path" REVIEW_CODEX_BIN="$reviewer" "$REVIEW") >"$T/path-driver.log" 2>&1; rc=$?
    check "$label: driver rejects PATH without recording a note" test "$rc" = 2 -a -z "$(note)"
    check "$label: driver explains the PATH rejection" grep -q 'PATH entries must be absolute' "$T/path-driver.log"
    PATH="$bad_path" "$BASH" "$runner" "$W" "$path_sha" "$T/relative/prompt" "$T/relative/piece" \
      "$T/path-output" "$T/path-events" "$reviewer" "$python" >"$T/path-runner.log" 2>&1; rc=$?
    check "$label: standalone runner rejects PATH" test "$rc" = 2 -a ! -e "$T/path-events"
    check "$label: runner explains the PATH rejection" grep -q 'PATH entries must be absolute' "$T/path-runner.log"
    "$python" -I "$supervisor" "$T/path-archive" "PATH=$bad_path" -- "$reviewer" >"$T/path-supervisor.log" 2>&1; rc=$?
    check "$label: supervisor rejects PATH before launching the env shebang" test "$rc" = 1
    check "$label: supervisor explains the PATH rejection" grep -q 'PATH entries must be absolute' "$T/path-supervisor.log"
    check "$label: archive interpreter never executes" test ! -e "$FAKE_LOG/archive-$interpreter-ran"
  done
done
"$python" -I "$supervisor" "$T/path-archive" -- "$T/bin/reviewer-bash" >"$T/path-supervisor.log" 2>&1; rc=$?
check "supervisor rejects missing PATH" test "$rc" = 1
check "supervisor explains missing PATH rejection" grep -q 'PATH entries must be absolute' "$T/path-supervisor.log"
g restore bash node launcher-bin

echo "=== committed reviewer configuration ==="
for config in .codex/config.toml nested/.codex/rules/default.rules .agents/skills/local/SKILL.md; do
  reset_fake
  g checkout -q main && g checkout -qB committed-reviewer-config
  mkdir -p "$W/${config%/*}"
  printf '# committed reviewer configuration\n' >"$W/$config"
  g add -f -- "$config" && g commit -qm "add $config"
  rm -- "$W/$config"
  review >"$T/reviewer-config.log" 2>&1; rc=$?
  check "$config stops review before launch and leaves no note" test "$rc" = 2 -a "$(calls)" = 0 -a -z "$(note)"
  check "$config rejection identifies the committed path" \
    grep -Fq "committed reviewer configuration is prohibited: $config" "$T/reviewer-config.log"
done

reset_fake
g checkout -q main && g checkout -qb diff-drivers
commit_file .gitattributes 'driver.txt diff=hide' 'configure diff attribute'
commit_file driver.txt original 'driver original'
driver_base="$(g rev-parse HEAD)"
commit_file driver.txt MARK_FINDING 'driver changed'
g config diff.hide.textconv true
g diff "$driver_base" HEAD >"$T/converted"
check "fixture textconv driver hides the edit with an ordinary git diff" test ! -s "$T/converted"
g config diff.hide.command true
g diff --no-textconv "$driver_base" HEAD >"$T/external-diff"
check "fixture external diff driver also hides the edit" test ! -s "$T/external-diff"
review --base "$driver_base" >"$T/raw-diff.log" 2>&1; rc=$?
check "review bypasses both configured drivers and receives the raw edit" test "$rc" = 1 -a "$(calls)" = 1
check "raw review patch contains the hidden line" grep -q '^+MARK_FINDING$' "$FAKE_LOG/stdin.1"
g config --unset diff.hide.textconv
g config --unset diff.hide.command

commit_file proof.lean 'def before := 1' 'original Lean line'
lean_base="$(g rev-parse HEAD)"
printf '*.lean -diff\n' >"$T/lean-attributes"
for source in checkout info global committed; do
  reset_fake
  g checkout -qB lean-diff-attributes "$lean_base"
  commit_file proof.lean 'def after := 2' "changed Lean line ($source)"
  case "$source" in
    checkout) cp "$T/lean-attributes" "$W/.gitattributes" ;;
    info) cp "$T/lean-attributes" "$W/.git/info/attributes" ;;
    global) g config --global core.attributesFile "$T/lean-attributes" ;;
    committed) commit_file .gitattributes '*.lean -diff' 'committed binary attribute' ;;
  esac
  g diff "$lean_base" HEAD -- proof.lean >"$T/ordinary-lean.diff"
  check "$source attribute marks the Lean edit binary in an ordinary diff" \
    grep -Fxq 'Binary files a/proof.lean and b/proof.lean differ' "$T/ordinary-lean.diff"
  review --base "$lean_base" >"$T/lean-review.log" 2>&1; rc=$?
  check "$source attribute permits a complete review invocation" test "$rc" = 0 -a "$(calls)" = 1
  check "$source attribute leaves the old Lean line in reviewer input" \
    grep -Fxq -- '-def before := 1' "$FAKE_LOG/stdin.1"
  check "$source attribute leaves the new Lean line in reviewer input" \
    grep -Fxq -- '+def after := 2' "$FAKE_LOG/stdin.1"
  case "$source" in
    checkout) g restore .gitattributes ;;
    info) rm "$W/.git/info/attributes" ;;
    global) g config --global --unset core.attributesFile ;;
  esac
done

echo "=== review patch preserves Git's record bytes ==="
reset_fake
g checkout -q main && g checkout -qb patch-bytes
mkdir -p "$W/Concrete"
printf 'def lineEnding := 1\r\n' >"$W/Concrete/Patch.lean"
g add Concrete/Patch.lean && g commit -qm 'Lean source with CRLF'
patch_base="$(g rev-parse HEAD)"
printf 'def lineEnding := 1\ndef separator := "left\342\200\250diff --git a/docs/x b/docs/x"\n' >"$W/Concrete/Patch.lean"
g add Concrete/Patch.lean && g commit -qm 'LF and embedded Unicode separator'
g diff --no-color --no-ext-diff --no-textconv -M "$patch_base" HEAD >"$T/expected-patch.diff"
review --base "$patch_base" >"$T/patch-bytes.review" 2>&1; rc=$?
check "embedded diff header stays in its original file piece" test "$rc" = 0 -a "$(calls)" = 1
check "removed Lean line retains its carriage return" grep -Fxq -- $'-def lineEnding := 1\r' "$FAKE_LOG/stdin.1"
check "reviewer stdin matches Git's patch byte for byte" cmp "$T/expected-patch.diff" "$FAKE_LOG/stdin.1"
check "saved invocation patch matches reviewer stdin" \
  cmp "$FAKE_LOG/stdin.1" "$W/.review/$(g rev-parse HEAD)-1-invocation-1.patch.diff"

reset_fake
g checkout -q main && g checkout -qb escaping-link
ln -s "$W/README.md" "$W/escape"
commit_file link-control.txt MARK_WRITE_LINK 'archive escape control'
mkdir "$T/escape-tree"
g archive HEAD | tar -xf - -C "$T/escape-tree"
printf 'control mutation\n' >"$T/escape-tree/escape"
check "following the fixture archive link can write into the caller" grep -q '^control mutation$' "$W/README.md"
g restore README.md
review >"$T/escape.log" 2>&1; rc=$?
check "runner rejects an escaping link before reviewer launch or note" test "$rc" = 2 -a "$(calls)" = 0 -a -z "$(note)"
check "caller remains unchanged when review rejects the link" test "$(cat "$W/README.md")" = base
check "round report identifies the escaping symlink" grep -q 'archive symlink has an absolute target: escape' "$W/.review/$(g rev-parse HEAD)-1.md"

rm "$W/escape"
ln -s . "$W/jump"
ln -s "jump/../../../${W#/}/README.md" "$W/escape"
g add jump escape && g commit -qm 'relative archive escape through an intermediate link'
review >"$T/relative-escape.log" 2>&1; rc=$?
check "relative symlink chain that escapes the tree is rejected before launch" test "$rc" = 2 -a "$(calls)" = 0 -a -z "$(note)"
check "round report identifies the relative escape" grep -q 'archive symlink escapes the private tree: escape' "$W/.review/$(g rev-parse HEAD)-1.md"

verifier="$ROOT_DIR/scripts/review/read_evidence.py"
rm "$T/exact/README.md"
ln "$W/README.md" "$T/exact/README.md"
python3 -I "$verifier" tree "$W" "$exact_sha" "$T/exact" >"$T/hardlink.log" 2>&1; rc=$?
check "archive verifier rejects a hardlink to the caller" test "$rc" -ne 0
check "hardlink rejection identifies the path" grep -q 'archive hardlink or special file: README.md' "$T/hardlink.log"
rm "$T/exact/README.md"
mkfifo "$T/exact/README.md"
python3 -I "$verifier" tree "$W" "$exact_sha" "$T/exact" >"$T/fifo.log" 2>&1; rc=$?
check "archive verifier rejects a FIFO before reading it" test "$rc" -ne 0
check "FIFO rejection identifies the path" grep -q 'archive hardlink or special file: README.md' "$T/fifo.log"

# Inject each file type at extraction so the real runner must reject it before
# launching the reviewer. Use a safe commit so no other archive check can mask it.
reset_fake
g checkout -q main && g checkout -qb archive-file-types
commit_file file-types.txt control 'runner file-type controls'
mkdir "$T/tar-shim"
printf '#!/usr/bin/env bash\nset -e\nreal_tar=%q\ncaller_file=%q\n' "$(type -P tar)" "$W/README.md" >"$T/tar-shim/tar"
cat >>"$T/tar-shim/tar" <<'EOF'
"$real_tar" "$@"
target="${!#}/README.md"
rm "$target"
case "$ARCHIVE_FILE_TYPE" in
  hardlink) ln "$caller_file" "$target" ;;
  fifo) mkfifo "$target" ;;
  *) exit 2 ;;
esac
EOF
chmod +x "$T/tar-shim/tar"
for file_type in hardlink fifo; do
  (cd "$W" && ARCHIVE_FILE_TYPE="$file_type" PATH="$T/tar-shim:$PATH" \
    bash "$ROOT_DIR/scripts/checks/lib/bounded.sh" 10 bash "$REVIEW") >"$T/runner-$file_type.log" 2>&1; rc=$?
  check "runner rejects injected $file_type before reviewer launch or note" test "$rc" = 2 -a "$(calls)" = 0 -a -z "$(note)"
  check "runner reports injected $file_type at the expected path" \
    grep -q 'archive hardlink or special file: README.md' "$T/runner-$file_type.log"
done
check "caller file survives runner hardlink and FIFO controls" test "$(cat "$W/README.md")" = base

# Ignore an installed replacement both when selecting the review diff and
# when creating and independently checking the archived tree.
reset_fake
g checkout -q main && g checkout -qb original-object
commit_file original.txt MARK_EXACT original
original="$(g rev-parse HEAD)"
g checkout -q --detach main
commit_file replacement.txt MARK_FINDING replacement
replacement="$(g rev-parse HEAD)"
g checkout -q original-object
g replace "$original" "$replacement"
check "fixture activates the replacement" g cat-file -e "$original:replacement.txt"
review >"$T/replacement.log" 2>&1; rc=$?
check "review uses the original commit and records a clean note" test "$rc" = 0 -a "$(field verdict)" = clean
check "diff includes the original path only" grep -q 'files=original.txt,$' "$FAKE_LOG/calls"
check "runner archive matches the original commit despite replacement" test -s "$FAKE_LOG/identity.1"
mkdir "$T/replaced-tree"
g archive "$original" | tar -xf - -C "$T/replaced-tree"
check "fixture actually archived the replacement" test -f "$T/replaced-tree/replacement.txt"
if python3 -I "$ROOT_DIR/scripts/review/read_evidence.py" tree "$W" "$original" "$T/replaced-tree" >"$T/replaced.verify" 2>&1; then
  no "independent verification rejects the replacement tree"
else
  check "independent verification rejects the replacement tree" grep -q 'archive file missing or wrong type: original.txt' "$T/replaced.verify"
fi
g replace -d "$original" >/dev/null

reset_fake
g checkout -q exact-tree
commit_file discovery.txt MARK_DISCOVERY discovery
mkdir "$W/caller-tmp"
(cd "$W" && TMPDIR="$W/caller-tmp" bash "$REVIEW") >"$T/discovery.log" 2>&1; rc=$?
check "review completed with TMPDIR inside the caller" test "$rc" = 0
check "Git discovery ran and found no caller repository" grep -qx 128 "$FAKE_LOG/discovery-status"
if [[ "$(cat "$FAKE_LOG/directory.1")/" == "$W/"* ]]; then
  no "private archive allocated outside the caller"
else
  check "private archive allocated outside the caller" test -s "$FAKE_LOG/directory.1"
fi

for attribute in export-ignore export-subst; do
  printf '$Format:%%H$\n' >"$W/target"
  printf 'target %s\n' "$attribute" >"$W/.gitattributes"
  g add target .gitattributes && g commit -qm "$attribute"
  reset_fake
  review >/dev/null 2>&1; rc=$?
  check "$attribute rejects altered archive before invocation" test "$rc" = 2 -a "$(calls)" = 0 -a -z "$(note)"
done
g update-index --add --cacheinfo "160000,$(g rev-parse HEAD),submodule"
g rm -q .gitattributes && g commit -qm gitlink
review >/dev/null 2>&1; rc=$?
check "gitlink rejected before invocation" test "$rc" = 2 -a "$(calls)" = 0 -a -z "$(note)"

reset_fake
g checkout -q main && g checkout -qb cancel
commit_file signal MARK_SLEEP signal
mkdir "$T/cancel-tmp"
(cd "$W" && TMPDIR="$T/cancel-tmp" exec bash "$REVIEW") >"$T/signal.log" 2>&1 &
review_pid=$!
for _ in {1..200}; do [ ! -s "$FAKE_LOG/sleeping" ] || break; sleep 0.025; done
check "cancellation reached reviewer and descendant" test -s "$FAKE_LOG/sleeping"
kill -TERM "$review_pid"
wait "$review_pid"; rc=$?
check "cancelled review returned an error" test "$rc" -ne 0
check "detached process has an independent session" test -s "$FAKE_LOG/detached"
if read -r reviewer descendant intermediate detached <"$FAKE_LOG/sleeping"; then
  for pid in "$reviewer" "$descendant" "$intermediate" "$detached"; do
    if kill -0 "$pid" 2>/dev/null; then no "process $pid terminated"; else ok "process $pid terminated"; fi
  done
fi
check "cancelled review removed both temporary directories" test -z "$(ls -A "$T/cancel-tmp")" -a ! -e "$(cat "$FAKE_LOG/directory.1")"
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
n="$(python3 -I "$SPLIT" "$T/split.diff" "$T/sp3" $(( (total + 2) / 3 )))"
check "--pieces N sizing: a third of the diff gives about three pieces ($n)" test "$n" -ge 3 -a "$n" -le 6
n1p="$(python3 -I "$SPLIT" "$T/split.diff" "$T/sp1" "$total")"
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
  python3 -I "$T/hunks.py" $(ls "$FAKE_LOG"/stdin.* | sort -t. -k2 -n)

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
pieces: 1
read-evidence: v1
patch-coverage: v1" "$OTHER_SHA"
git -C "$T/other" push -q origin refs/notes/concrete-review
reset_fake
g checkout -q main && g checkout -qb nonff
commit_file z.txt "z" "z"
# Our local notes ref lacks the other writer's note: the start-of-round sync
# must merge it, or the push below would be a non-fast-forward.
REVIEW_PUSH=1 review >"$T/out6" 2>&1; rc=$?
check "review after a concurrent notes push succeeds (exit $rc)" test "$rc" -eq 0
check "start-of-round sync avoids a rejected push" bash -c '! grep -q "rejected; fetching and merging" "$1"' _ "$T/out6"
check "origin keeps the other writer's note" \
  bash -c "git -C '$T/origin.git' notes --ref=refs/notes/concrete-review show $OTHER_SHA | has_match 'verdict: clean'"
check "origin has our note" \
  bash -c "git -C '$T/origin.git' notes --ref=refs/notes/concrete-review show $(g rev-parse HEAD) | has_match 'verdict: clean'"
# Now force the non-fast-forward at push time: another writer pushes between
# our sync and our push (the fake reviewer does it while it "reviews").
git -C "$T/other" fetch -q origin "+refs/notes/concrete-review:refs/notes/concrete-review"
cat >"$T/bin/codex-race" <<EOF
#!/usr/bin/env bash
set -e
export GIT_CONFIG_NOSYSTEM=1 GIT_CONFIG_GLOBAL=/dev/null
git -C "$T/other" -c user.name="Race fixture" -c user.email=race@example.invalid notes --ref=refs/notes/concrete-review add -f -m "race note" "$OTHER_SHA"
git -C "$T/other" push -q origin refs/notes/concrete-review
touch "$T/race-pushed"
exec "$T/bin/codex" "\$@"
EOF
chmod +x "$T/bin/codex-race"
reset_fake
commit_file z2.txt "z2" "z2"
REVIEW_CODEX_BIN="$T/bin/codex-race" REVIEW_PUSH=1 review >"$T/out7" 2>&1; rc=$?
check "push rejected once, then merged and pushed (exit $rc)" test "$rc" -eq 0
check "the competing push completed inside the reviewer" test -f "$T/race-pushed"
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
REVIEW_PUSH=1 review >"$T/out9" 2>&1; rc=$?
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
