#!/bin/bash
# R-0544 — one review procedure for every agent. See docs/REVIEW.md.
#
#   scripts/review/review.sh [--base <ref>] [--head <ref>] [--max-rounds 5]
#                            [--pieces auto|N] [--prompt-extra "..."]
#
# --head names the commit the caller means to review (train.sh and ready.sh pass it); it must be
# the checkout's HEAD, or nothing runs (exit 2): the review always reads HEAD's tree.
#
# Runs ONE review round on HEAD and records the result as a git note on HEAD
# under refs/notes/concrete-review, which it then pushes.
#
#   round 1      reviews the full diff base..HEAD (base defaults to the merge-base
#                with origin/main);
#   round N > 1  reviews only the delta since the last reviewed commit (found from
#                the notes on earlier commits of the branch) and passes the previous
#                round's findings to the reviewer. If that commit does not descend
#                from the current base, review the full base..HEAD diff instead:
#                never include changes imported from base. This fallback preserves
#                the branch's round count and cap (it is not a new round 1).
#
# A large diff is split into pieces by file group and size. A piece the reviewer
# refuses is retried once, halved and with a more neutral prompt; after that it is
# `blocked`. The round cap is enforced across invocations: there is no round
# max+1.
#
# Exit codes:
#   0  verdict clean
#   1  verdict findings (fix, commit, run again)
#   2  usage or environment error (nothing recorded)
#   3  round cap reached: split the branch or escalate (open findings are printed)
#   4  nothing to review (HEAD already reviewed, or empty diff)
#   5  verdict blocked (a piece could not be reviewed; see .review/)
#   6  result recorded locally but the notes ref could not be pushed
#
# Environment (mostly for the test gate, scripts/checks/review_tool.sh):
#   REVIEW_CODEX_BIN    reviewer binary (default: codex)
#   REVIEW_MAIN_REF     ref the default base is computed against (default: origin/main)
#   REVIEW_PUSH         1 to fetch/push the notes refs (default: 1)
#   REVIEW_PIECE_BYTES  size limit of one piece in bytes (default: 60000)

set -euo pipefail
# Load the PATH check with builtins only, before any executable lookup.
script=${BASH_SOURCE[0]}
case "$script" in */*) ;; *) script="./$script" ;; esac
source "${script%/*}/runtime.sh" || exit 2
review_require_absolute_path "${PATH-}" || exit 2
export GIT_NO_REPLACE_OBJECTS=1

SCRIPT_DIR="$(CDPATH= cd -- "${script%/*}/" && pwd)"
source "$SCRIPT_DIR/../checks/lib/has_match.sh" || exit 2  # bug 283: never a pipe into grep -q
ROOT="$(git rev-parse --show-toplevel)"

CODEX_BIN="${REVIEW_CODEX_BIN:-codex}"
REMOTE=origin
MAIN_REF="${REVIEW_MAIN_REF:-$REMOTE/main}"
PUSH="${REVIEW_PUSH:-1}"
PIECE_BYTES="${REVIEW_PIECE_BYTES:-60000}"
NOTES_REF="refs/notes/concrete-review"
FINDINGS_REF="refs/notes/concrete-review-findings"
REVIEWER="codex"

BASE=""
MAX_ROUNDS=5
PIECES="auto"
EXTRA=""

usage() { awk 'NR > 1 { if (!/^#/) exit; sub(/^# ?/, ""); print }' "$SCRIPT_DIR/review.sh"; }  # the header, whatever its length
die() { echo "review: $*" >&2; exit 2; }

while [ $# -gt 0 ]; do
  case "$1" in
    --base) [ $# -ge 2 ] || die "--base needs a ref"; BASE="$2"; shift 2 ;;
    --head) [ $# -ge 2 ] && [ -n "$2" ] || die "--head needs a ref"; WANT_HEAD="$2"; shift 2 ;;
    --max-rounds) [ $# -ge 2 ] || die "--max-rounds needs a number"; MAX_ROUNDS="$2"; shift 2 ;;
    --pieces) [ $# -ge 2 ] || die "--pieces needs auto or a number"; PIECES="$2"; shift 2 ;;
    --prompt-extra) [ $# -ge 2 ] || die "--prompt-extra needs text"; EXTRA="$2"; shift 2 ;;
    -h|--help) usage; exit 0 ;;
    *) die "unknown argument: $1 (see --help)" ;;
  esac
done
[[ "$MAX_ROUNDS" =~ ^[1-9][0-9]*$ ]] || die "--max-rounds must be a positive integer"
[[ "$PIECES" == auto || "$PIECES" =~ ^[1-9][0-9]*$ ]] || die "--pieces must be auto or a positive integer"
PYTHON="$(review_executable python3)" || die "python3 executable not found"
"$PYTHON" -I "$SCRIPT_DIR/supervise.py" --check || exit 2
CODEX_BIN="$(review_executable "$CODEX_BIN")" || die "reviewer executable not found: ${REVIEW_CODEX_BIN:-codex}"
cd "$ROOT"
if [ -n "${WANT_HEAD:-}" ]; then
  want="$(git rev-parse -q --verify "$WANT_HEAD^{commit}")" || die "--head $WANT_HEAD is not a commit"
  [ "$want" = "$(git rev-parse HEAD)" ] || die "--head $WANT_HEAD is ${want:0:12}, but the checkout is at $(git rev-parse --short=12 HEAD)"
fi
[[ "$PIECE_BYTES" =~ ^[1-9][0-9]*$ ]] || die "REVIEW_PIECE_BYTES must be a positive integer"

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

runner=
cancel_review() {
  trap '' HUP INT TERM
  if [ -n "$runner" ]; then
    kill -TERM "$runner" 2>/dev/null || true
    wait "$runner" 2>/dev/null || true
  fi
  exit 2
}
trap cancel_review HUP INT TERM

# ---------------------------------------------------------------- notes sync
# Fetch the remote notes refs and merge them into the local ones. On a conflict
# for the same commit the local note wins (`-s ours`): it is the newer result.
sync_notes() {
  [ "$PUSH" = 1 ] || return 0
  local ref tmp
  for ref in "$NOTES_REF" "$FINDINGS_REF"; do
    tmp="refs/notes/review-sync-${ref##*/}"
    if git fetch -q "$REMOTE" "+$ref:$tmp" 2>/dev/null; then
      if git show-ref -q --verify "$ref"; then
        git notes --ref="$ref" merge -q -s ours "$tmp" >/dev/null 2>&1 \
          || { echo "review: could not merge remote $ref" >&2; git update-ref -d "$tmp"; return 1; }
      else
        git update-ref "$ref" "$tmp"
      fi
      git update-ref -d "$tmp"
    fi
  done
}

# Push both notes refs; on a non-fast-forward, fetch + merge and retry.
push_notes() {
  [ "$PUSH" = 1 ] || return 0
  local attempt refs=("$NOTES_REF")
  git show-ref -q --verify "$FINDINGS_REF" && refs+=("$FINDINGS_REF")
  for attempt in 1 2 3; do
    if git push -q "$REMOTE" "${refs[@]}" 2>"$WORK/push.err"; then
      return 0
    fi
    # Only a non-fast-forward is cured by fetch + merge. Anything else (a hook
    # refusal, auth, network) fails at once instead of being repeated.
    if ! grep -qE '\[rejected\].*\((fetch first|non-fast-forward)\)' "$WORK/push.err"; then
      break
    fi
    echo "review: notes push attempt $attempt rejected; fetching and merging" >&2
    sync_notes || true
  done
  cat "$WORK/push.err" >&2
  return 1
}

note_field() { # note_field <text> <key>
  printf '%s\n' "$1" | awk -v k="$2: " 'index($0, k) == 1 && !done { print substr($0, length(k) + 1); done = 1 }'
}

findings_for() { # findings_for <sha> <round>: the recorded findings of that round
  local sha="$1" round="$2"
  if git notes --ref="$FINDINGS_REF" show "$sha" 2>/dev/null; then return 0; fi
  if [ -f ".review/$sha-$round.md" ]; then cat ".review/$sha-$round.md"; return 0; fi
  echo "(findings text not available for $sha round $round)"
}

cap_message() { # cap_message <rounds-done> <findings-text>
  {
    echo "review: round cap reached ($1 of $MAX_ROUNDS rounds without a clean verdict)."
    echo "review: do not run another round. Split the branch into smaller branches, or"
    echo "review: escalate to a human reviewer. Open findings:"
    echo "$2" | sed 's/^/  /'
  } >&2
}

# ---------------------------------------------------------------- state
sync_notes || die "notes sync failed"

HEAD_SHA="$(git rev-parse HEAD)"
if [ -z "$BASE" ]; then
  git rev-parse -q --verify "$MAIN_REF^{commit}" >/dev/null || die "no $MAIN_REF; pass --base"
  BASE_SHA="$(git merge-base HEAD "$MAIN_REF")" || die "no merge-base with $MAIN_REF"
else
  BASE_SHA="$(git rev-parse -q --verify "$BASE^{commit}")" || die "bad --base: $BASE"
fi

if [ -n "$(git status --porcelain --untracked-files=no)" ]; then
  echo "review: warning: uncommitted changes are not reviewed; only HEAD is" >&2
fi

# Latest note on the branch (round count), and the latest note whose round
# actually reviewed its diff (delta start). A `blocked` round reviewed only part
# of its diff, so it does not advance the delta start.
PREV_VALID=0
PREV_SHA="" PREV_ROUNDS=0 PREV_VERDICT="" REVIEWED_SHA="" REVIEWED_ROUNDS=0
INSUFFICIENT_RANGE=0
PREV_FINDINGS=""
COVERING_REVIEWS=""
while read -r c; do
  [ -n "$c" ] || continue
  # Historical notes count toward the cap and preserve findings.
  # Only notes with current read evidence and patch coverage qualify below.
  vrc=0
  bash "$SCRIPT_DIR/verify.sh" --round-history "$c" >/dev/null 2>&1 || vrc=$?
  [ "$vrc" -le 1 ] || continue
  n="$(git notes --ref="$NOTES_REF" show "$c")"
  v="$(note_field "$n" verdict)"
  r="$(note_field "$n" rounds)"
  [ "$r" -le "$PREV_ROUNDS" ] || PREV_ROUNDS="$r"
  vrc=0
  bash "$SCRIPT_DIR/verify.sh" --required-base "$BASE_SHA" "$c" >/dev/null 2>&1 || vrc=$?
  if [ -z "$PREV_SHA" ]; then
    PREV_SHA="$c" PREV_VERDICT="$v"
    if [ "$vrc" -le 1 ]; then PREV_VALID=1; fi
  fi
  # Topological order visits covering descendants first. They supersede both
  # ancestor findings and insufficient scope, but never uncovered merge siblings.
  covered=0
  for descendant in $COVERING_REVIEWS; do
    if git merge-base --is-ancestor "$c" "$descendant"; then covered=1; break; fi
  done
  if [ "$covered" = 0 ] && [ "$vrc" -gt 1 ]; then INSUFFICIENT_RANGE=1; fi
  if [ "$covered" = 0 ] && [ "$v" != clean ]; then
    PREV_FINDINGS="$PREV_FINDINGS
$(findings_for "$c" "$r")"
  fi
  if [ "$vrc" -le 1 ] && { [ "$v" = clean ] || [ "$v" = findings ]; }; then
    COVERING_REVIEWS="$COVERING_REVIEWS $c"
    if [ -z "$REVIEWED_SHA" ]; then
      REVIEWED_SHA="$c" REVIEWED_ROUNDS="$r"
    fi
  fi
done < <(git rev-list --topo-order HEAD "^$BASE_SHA")

if [ "$INSUFFICIENT_RANGE" = 0 ] && [ "$PREV_VALID" = 1 ] && [ "$PREV_SHA" = "$HEAD_SHA" ] && [ "$PREV_VERDICT" != blocked ]; then
  if [ "$PREV_VERDICT" = clean ]; then
    echo "review: HEAD $HEAD_SHA already reviewed clean (round $PREV_ROUNDS)"
    exit 0
  fi
  if [ "$PREV_ROUNDS" -ge "$MAX_ROUNDS" ]; then
    cap_message "$PREV_ROUNDS" "$PREV_FINDINGS"
    exit 3
  fi
  echo "review: HEAD already reviewed in round $PREV_ROUNDS (verdict $PREV_VERDICT)." >&2
  echo "review: commit the fixes, then run again." >&2
  exit 4
fi

ROUND=$((PREV_ROUNDS + 1))
if [ "$ROUND" -gt "$MAX_ROUNDS" ]; then
  cap_message "$PREV_ROUNDS" "$PREV_FINDINGS"
  exit 3
fi

FROM="${REVIEWED_SHA:-$BASE_SHA}"
FULL_REVIEW=0
if [ "$INSUFFICIENT_RANGE" = 1 ] ||
   { [ -n "$REVIEWED_SHA" ] && ! git merge-base --is-ancestor "$BASE_SHA" "$REVIEWED_SHA"; }; then
  # Missing range coverage or an old review predating changes imported from base
  # requires full review. Comparing an old commit directly
  # with HEAD would review base's changes too. Re-review the branch's complete
  # current contribution, including merge resolutions, without resetting rounds.
  FROM="$BASE_SHA"
  FULL_REVIEW=1
  echo "review: prior review does not cover current branch scope; reviewing full base..HEAD diff (round $ROUND)" >&2
fi
# Compare the original objects in a bare repository with no worktree, index,
# attribute source, or inherited Git configuration. The empty template also
# rules out injected info/attributes. Git's built-in binary detection keeps
# binary payloads out of the prompt while still emitting their named changes.
object_format="$(git rev-parse --show-object-format)"
object_directory="$(git rev-parse --path-format=absolute --git-path objects)"
object_alternates="${GIT_ALTERNATE_OBJECT_DIRECTORIES-}"
(
  source "$SCRIPT_DIR/../checks/lib/git_sandbox.sh" || exit 2
  git_sandbox /dev/null || exit 2
  unset GIT_ATTR_SOURCE
  export GIT_ATTR_NOSYSTEM=1
  git init -q --bare --template= --object-format="$object_format" "$WORK/diff.git"
  # Keep the current directory so relative paths in the object alternates still resolve.
  GIT_OBJECT_DIRECTORY="$object_directory" GIT_ALTERNATE_OBJECT_DIRECTORIES="$object_alternates" \
    git --git-dir="$WORK/diff.git" -c core.attributesFile=/dev/null \
      diff --no-color --no-ext-diff --no-textconv -M "$FROM" "$HEAD_SHA"
) >"$WORK/full.diff"
if [ ! -s "$WORK/full.diff" ]; then
  echo "review: nothing to review: no changes between $FROM and HEAD" >&2
  exit 4
fi

# ---------------------------------------------------------------- split
TOTAL="$(wc -c <"$WORK/full.diff" | tr -d ' ')"
split_at() { rm -rf "$WORK/pieces"; "$PYTHON" -I "$SCRIPT_DIR/split_diff.py" "$WORK/full.diff" "$WORK/pieces" "$1"; }
if [ "$PIECES" = auto ]; then
  K="$(split_at "$PIECE_BYTES")"
else
  # --pieces N: start at total/N and grow the limit until at most N pieces come
  # out (repeated file headers add a little). File groups are never merged, so
  # more groups than N still give one piece per group.
  LIMIT=$(( (TOTAL + PIECES - 1) / PIECES ))
  K="$(split_at "$LIMIT")"
  while [ "$K" -gt "$PIECES" ] && [ "$LIMIT" -lt "$TOTAL" ]; do
    LIMIT=$(( LIMIT + LIMIT / 20 + 1 ))
    K="$(split_at "$LIMIT")"
  done
fi

# ---------------------------------------------------------------- prompts
SCOPE_ROUND1="The patch below is the complete change of a branch against its base commit $BASE_SHA."
SCOPE_DELTA="The patch below is only the change since commit $FROM, which was reviewed in round $REVIEWED_ROUNDS. Earlier rounds covered the rest of the branch."
VERDICT_RULE='For each finding write one bullet: `- path:line: what is wrong and why`.
The last line of your reply must be exactly one of these two lines, with nothing after it:
VERDICT: clean
VERDICT: findings
Use `VERDICT: findings` when you report any finding, including an earlier finding that is still open.'

prompt_for() { # prompt_for <standard|neutral> <piece label> <group> <paths>
  local scope="$SCOPE_ROUND1"
  [ -n "$REVIEWED_SHA" ] && [ "$FULL_REVIEW" = 0 ] && scope="$SCOPE_DELTA"
  if [ "$1" = standard ]; then
    cat <<EOF
You are reviewing a patch to Concrete, a compiler for a small systems programming language. The compiler is written in Lean 4; the repository also holds its standard library, test programs, test scripts and documentation. Read repository files in the working directory for context; this is a read-only review.

This is review round $ROUND. $scope
This is $2 (file group: $3; files: $4).

Report correctness findings only: wrong behaviour, logic errors, regressions, unhandled cases, tests that do not check what they claim, and documentation that contradicts the code. Do not report style preferences.

Also answer one question, as a finding when the answer is yes: does this patch pick a side on any owner decision in docs/decisions.md (OD1 - OD157 under "Owner decisions", or an undecided project question), that is, choose, revise, encode, implement, extend or revert what one of them covers without the owner's explicit answer? Only the project owner decides those; a patch that does so must not land. Recording a decision the owner explicitly approved (the recording rule in docs/decisions.md) is permitted and is not picking a side. For such a patch, report as a finding: a recorded answer without its provenance (the owner's answer quoted with its date, or the quoted owner rule that decides it under the recording rule); wording that does not faithfully transcribe that answer; any change beyond its scope; and anything recorded as decided that the owner did not approve.
EOF
  else
    cat <<EOF
Please do a routine code review of the source-code patch below. It belongs to a compiler, a program that translates source code into machine code, and to that compiler's tests and documentation. Treat it as ordinary software engineering: look for logic errors and for behaviour that differs from what the code and its comments say. You may read files in the working directory for context.

This is $2 (files: $4). $scope
Also report as a finding any change that picks a side on an owner decision listed in docs/decisions.md (OD1 - OD157) or an undecided project question without the owner's explicit answer. Writing down a decision the owner explicitly approved is allowed; report it only when it lacks the owner's quoted, dated answer (or the quoted owner rule that decides it under the recording rule in docs/decisions.md), changes that answer's meaning, goes beyond it, or adds something the owner did not approve.
EOF
  fi
  if [ -n "$PREV_FINDINGS" ]; then
    cat <<EOF

The previous round reported the findings below. Check whether this patch resolves each one, and list every one that is still open as a finding.
--- previous findings ---
$PREV_FINDINGS
--- end previous findings ---
EOF
  fi
  [ -n "$EXTRA" ] && printf '\n%s\n' "$EXTRA"
  printf '\n%s\n' "$VERDICT_RULE"
}

LAST_RESULT=""
LAST_BODY=""
LAST_FINDINGS=""
RUN_N=0
# run_one <prompt-kind> <piece-file> <label> <group> <paths>; sets LAST_RESULT to
# clean|findings|refused|noverdict, LAST_BODY to the full reply and LAST_FINDINGS
# to the findings text without the final verdict.
run_one() {
  local kind="$1" piece="$2" label="$3" group="$4" paths="$5" rc=0
  RUN_N=$((RUN_N + 1))
  LAST_FINDINGS=""
  local out="$WORK/out.$RUN_N.txt" log="$WORK/log.$RUN_N.txt"
  : >"$out"
  local prompt="$RAW_DIR/$HEAD_SHA-$ROUND-invocation-$RUN_N.prompt.txt"
  local evidence="$RAW_DIR/$HEAD_SHA-$ROUND-invocation-$RUN_N"
  cp "$piece" "$evidence.patch.diff"
  prompt_for "$kind" "$label" "$group" "$paths" >"$prompt"
  : >"$log"
  local cancelled=0
  trap 'cancelled=1' HUP INT TERM
  bash "$SCRIPT_DIR/invoke.sh" "$ROOT" "$HEAD_SHA" "$prompt" "$evidence.patch.diff" "$out" "$log" "$CODEX_BIN" "$PYTHON" \
    >"$WORK/result.$RUN_N" 2>"$evidence.stderr.txt" &
  runner=$!
  trap cancel_review HUP INT TERM
  [ "$cancelled" = 0 ] || cancel_review
  wait "$runner" || rc=$?
  runner=
  cp "$out" "$evidence.output.txt"
  cp "$log" "$evidence.events.jsonl"
  if [ -s "$evidence.stderr.txt" ]; then
    { echo; echo "### Runner stderr ($label, invocation $RUN_N)"; echo; cat "$evidence.stderr.txt"; } >>"$RAW"
  fi
  if [ "$rc" -eq 3 ]; then
    LAST_RESULT=refused
    if [ -s "$out" ]; then LAST_BODY="$(cat "$out")"; else LAST_BODY="$(cat "$log")"; fi
    return
  elif [ "$rc" -ne 0 ]; then
    { echo; echo "## $label: error (invocation $RUN_N)"; cat "$out" "$log"; } >>"$RAW"
    cat "$evidence.stderr.txt" >&2
    die "reviewer execution or repository-read evidence failed; see $evidence.* (no note recorded)"
  fi
  LAST_BODY="$(cat "$out")"
  IFS= read -r LAST_RESULT <"$WORK/result.$RUN_N"
  if [ "$LAST_RESULT" = findings ]; then
    LAST_FINDINGS="$(tail -n +2 "$WORK/result.$RUN_N")"
  fi
}

RAW_DIR="$ROOT/.review"
mkdir -p "$RAW_DIR"
RAW="$RAW_DIR/$HEAD_SHA-$ROUND.md"
UTC="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
{
  echo "# review of $HEAD_SHA, round $ROUND"
  echo
  echo "reviewer: $REVIEWER; utc: $UTC; base: $BASE_SHA; diff from: $FROM; pieces: $K; diff bytes: $TOTAL"
} >"$RAW"

N_FINDINGS=0 N_BLOCKED=0
FINDINGS_TEXT=""
i=0
while IFS=$'\t' read -r name group bytes paths; do
  i=$((i + 1))
  label="piece $i of $K"
  echo "review: round $ROUND, $label ($group, $bytes bytes)" >&2
  run_one standard "$WORK/pieces/$name" "$label" "$group" "$paths"
  result="$LAST_RESULT" details="$LAST_FINDINGS"
  {
    echo; echo "## $label: $group ($bytes bytes): $result"; echo; echo "files: $paths"; echo
    echo "$LAST_BODY"
  } >>"$RAW"
  if [ "$result" = refused ] || [ "$result" = noverdict ]; then
    # Retry once: smaller pieces (the piece halved) and a more neutral prompt.
    half_dir="$WORK/half-$i"
    half_limit=$(( (bytes + 1) / 2 ))
    "$PYTHON" -I "$SCRIPT_DIR/split_diff.py" "$WORK/pieces/$name" "$half_dir" "$half_limit" --no-groups >/dev/null
    result=clean details=""
    j=0
    while IFS=$'\t' read -r hname _hgroup hbytes hpaths; do
      j=$((j + 1))
      hlabel="part $j of piece $i of $K"
      echo "review: retrying $hlabel with a neutral prompt ($hbytes bytes)" >&2
      run_one neutral "$half_dir/$hname" "$hlabel" "$group" "$hpaths"
      { echo; echo "### retry, $hlabel ($hbytes bytes): $LAST_RESULT"; echo; echo "$LAST_BODY"; } >>"$RAW"
      case "$LAST_RESULT" in
        clean) ;;
        findings) [ "$result" = blocked ] || result=findings; details="$details
$LAST_FINDINGS" ;;
        *) result=blocked ;;
      esac
    done <"$half_dir/manifest.tsv"
  fi
  case "$result" in
    findings)
      N_FINDINGS=$((N_FINDINGS + 1))
      FINDINGS_TEXT="$FINDINGS_TEXT
[$label: $group]
$details"
      ;;
    blocked)
      N_BLOCKED=$((N_BLOCKED + 1))
      FINDINGS_TEXT="$FINDINGS_TEXT
[$label: $group] blocked: the reviewer refused or gave no verdict, also after one neutral retry. Files: $paths
$details"
      ;;
  esac
done <"$WORK/pieces/manifest.tsv"

# `blocked` wins over `findings`: a round with a blocked piece did not review all
# of its diff, and only a `blocked` verdict keeps the next round's delta from
# starting after it. The findings of the pieces that were reviewed are still
# recorded and carried forward.
if [ "$N_BLOCKED" -gt 0 ]; then VERDICT=blocked
elif [ "$N_FINDINGS" -gt 0 ]; then VERDICT=findings
else VERDICT=clean
fi
{ echo; echo "## verdict: $VERDICT ($N_FINDINGS piece(s) with findings, $N_BLOCKED blocked)"; } >>"$RAW"

# ---------------------------------------------------------------- record
# THE NOTE FORMAT IS AN INTERFACE: scripts/review/verify.sh parses it (train.sh's review leg reads
# it through verify.sh). Change it only together with verify.sh.
cat >"$WORK/note" <<EOF
verdict: $VERDICT
rounds: $ROUND
reviewer: $REVIEWER
utc: $UTC
base: $BASE_SHA
pieces: $K
read-evidence: v1
patch-coverage: v1
EOF
git notes --ref="$NOTES_REF" add -f -F "$WORK/note" "$HEAD_SHA"
if [ "$VERDICT" = blocked ] && [ -n "$PREV_FINDINGS" ]; then
  # A blocked round cannot resolve the findings it was given: some code went
  # unreviewed. Record them with this round's own, so a later round (even after
  # several blocked rounds in a row, or a re-run on the same HEAD) still gets them.
  FINDINGS_TEXT="$FINDINGS_TEXT
[carried from earlier rounds; not confirmed resolved, because this round was blocked]
$PREV_FINDINGS"
fi
if [ "$VERDICT" != clean ]; then
  printf '%s\n' "${FINDINGS_TEXT#$'\n'}" >"$WORK/findings"
  git notes --ref="$FINDINGS_REF" add -f -F "$WORK/findings" "$HEAD_SHA"
fi

echo "review: round $ROUND verdict $VERDICT ($K piece(s)); raw output: ${RAW#$ROOT/}"
[ "$VERDICT" = clean ] || printf '%s\n' "${FINDINGS_TEXT#$'\n'}"

if ! push_notes; then
  echo "review: the result is recorded locally but $NOTES_REF could not be pushed to $REMOTE." >&2
  echo "review: push it by hand: git push $REMOTE $NOTES_REF" >&2
  exit 6
fi

case "$VERDICT" in
  clean) exit 0 ;;
  findings|blocked)
    if [ "$ROUND" -ge "$MAX_ROUNDS" ]; then
      cap_message "$ROUND" "${FINDINGS_TEXT#$'\n'}"
      exit 3
    fi
    [ "$VERDICT" = findings ] && exit 1
    exit 5
    ;;
esac
