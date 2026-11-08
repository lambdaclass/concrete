#!/usr/bin/env bash
# R-0544 — one review procedure for every agent. See docs/REVIEW.md.
#
#   scripts/review/review.sh [--writer codex|kimi|agy|claude[,...]] [--reviewer codex|kimi|agy]
#                            [--base <ref>] [--head <ref>] [--max-rounds 5]
#                            [--pieces auto|N] [--prompt-extra "..."]
#
# Writer provenance is derived from the commits in base..HEAD (via scripts/review/writer_provenance.py
# and refs/notes/concrete-writer; unstamped commits have an unknown writer and are refused unless
# backfilled in refs/notes/concrete-writer).
# --writer is optional: when supplied, it is checked as an assertion against the derived writer set.
# --reviewer defaults to codex and must not be any writer of base..HEAD: nobody reviews their own work.
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
#                round's findings to the reviewer.
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
#   5  verdict blocked (a piece could not be reviewed or its repository was unreadable; see .review/)
#   6  result recorded locally but the notes ref could not be pushed
#
# Environment (mostly for the test gate, scripts/checks/review_tool.sh):
#   REVIEW_CODEX_BIN    Codex binary (default: codex)
#   REVIEW_KIMI_BIN     Kimi binary (default: kimi)
#   REVIEW_AGY_BIN      agy binary (default: agy)
#   REVIEW_REMOTE       remote for notes and the default base (default: origin)
#   REVIEW_MAIN_REF     ref the default base is computed against (default: $REVIEW_REMOTE/main)
#   REVIEW_PUSH         1 to fetch/push the notes refs (default: 1)
#   REVIEW_PIECE_BYTES  size limit of one piece in bytes (default: 60000)
#   REVIEW_TIMEOUT      seconds per reviewer attempt before one longer retry (default: 1500)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/../checks/lib/has_match.sh" || exit 2  # bug 283: never a pipe into grep -q
ROOT="$(git rev-parse --show-toplevel)"
cd "$ROOT"

CODEX_BIN="${REVIEW_CODEX_BIN:-codex}"
KIMI_BIN="${REVIEW_KIMI_BIN:-kimi}"
AGY_BIN="${REVIEW_AGY_BIN:-agy}"
REMOTE="${REVIEW_REMOTE:-origin}"
MAIN_REF="${REVIEW_MAIN_REF:-$REMOTE/main}"
PUSH="${REVIEW_PUSH:-1}"
PIECE_BYTES="${REVIEW_PIECE_BYTES:-60000}"
TIMEOUT="${REVIEW_TIMEOUT:-1500}"
NOTES_REF="refs/notes/concrete-review"
FINDINGS_REF="refs/notes/concrete-review-findings"
WRITER_NOTES_REF="refs/notes/concrete-writer"
REVIEWER="codex"
ASSERT_WRITER=""
WRITER=""

BASE=""
MAX_ROUNDS=5
PIECES="auto"
EXTRA=""

usage() { awk 'NR > 1 { if (!/^#/) exit; sub(/^# ?/, ""); print }' "${BASH_SOURCE[0]}"; }  # the header, whatever its length
die() { echo "review: $*" >&2; exit 2; }

valid_writer_csv() {
  [[ "$1" =~ ^(codex|kimi|agy|claude)(,(codex|kimi|agy|claude))*$ ]] || return 1
  local w seen=","
  local IFS=,
  for w in $1; do
    case "$seen" in *",$w,"*) return 1 ;; esac
    seen="$seen$w,"
  done
  return 0
}

while [ $# -gt 0 ]; do
  case "$1" in
    --base) [ $# -ge 2 ] || die "--base needs a ref"; BASE="$2"; shift 2 ;;
    --head) [ $# -ge 2 ] && [ -n "$2" ] || die "--head needs a ref"; WANT_HEAD="$2"; shift 2 ;;
    --max-rounds) [ $# -ge 2 ] || die "--max-rounds needs a number"; MAX_ROUNDS="$2"; shift 2 ;;
    --pieces) [ $# -ge 2 ] || die "--pieces needs auto or a number"; PIECES="$2"; shift 2 ;;
    --prompt-extra) [ $# -ge 2 ] || die "--prompt-extra needs text"; EXTRA="$2"; shift 2 ;;
    --reviewer) [ $# -ge 2 ] || die "--reviewer needs codex, kimi or agy"; REVIEWER="$2"; shift 2 ;;
    --writer) [ $# -ge 2 ] || die "--writer needs codex, kimi, agy or claude"; ASSERT_WRITER="$2"; shift 2 ;;
    -h|--help) usage; exit 0 ;;
    *) die "unknown argument: $1 (see --help)" ;;
  esac
done
case "$REVIEWER" in codex|kimi|agy) ;; *) die "--reviewer must be codex, kimi or agy" ;; esac
if [ -n "$ASSERT_WRITER" ]; then
  valid_writer_csv "$ASSERT_WRITER" || die "--writer must be codex, kimi, agy or claude (or comma-separated unique models)"
  case ",$ASSERT_WRITER," in
    *,"$REVIEWER",*) die "reviewer $REVIEWER cannot review its own work (writer $ASSERT_WRITER)" ;;
  esac
fi
[[ "$MAX_ROUNDS" =~ ^[1-9][0-9]*$ ]] || die "--max-rounds must be a positive integer"
[[ "$PIECES" == auto || "$PIECES" =~ ^[1-9][0-9]*$ ]] || die "--pieces must be auto or a positive integer"
if [ -n "${WANT_HEAD:-}" ]; then
  want="$(git rev-parse -q --verify "$WANT_HEAD^{commit}")" || die "--head $WANT_HEAD is not a commit"
  [ "$want" = "$(git rev-parse HEAD)" ] || die "--head $WANT_HEAD is ${want:0:12}, but the checkout is at $(git rev-parse --short=12 HEAD)"
fi
[[ "$PIECE_BYTES" =~ ^[1-9][0-9]*$ ]] || die "REVIEW_PIECE_BYTES must be a positive integer"
{ [[ "$TIMEOUT" =~ ^[0-9]+(\.[0-9]+)?$ ]] && awk -v s="$TIMEOUT" 'BEGIN { exit !(s + 0 > 0) }'; } \
  || die "REVIEW_TIMEOUT must be a positive number of seconds"
case "$REVIEWER" in codex) REVIEWER_BIN="$CODEX_BIN" ;; kimi) REVIEWER_BIN="$KIMI_BIN" ;; agy) REVIEWER_BIN="$AGY_BIN" ;; esac
command -v "$REVIEWER_BIN" >/dev/null 2>&1 || die "reviewer binary not found: $REVIEWER_BIN"

WORK="$(mktemp -d)"
cleanup() {
  chmod -R u+w "$WORK" 2>/dev/null || true
  rm -rf "$WORK"
}
trap cleanup EXIT
BOUNDED_PARENT="$WORK" source "$SCRIPT_DIR/../checks/lib/bounded.sh" || exit 2
source "$SCRIPT_DIR/collect_output.sh" || exit 2
sha256_file() { if command -v sha256sum >/dev/null 2>&1; then sha256sum "$1" | cut -d' ' -f1; else shasum -a 256 "$1" | cut -d' ' -f1; fi; }

# ---------------------------------------------------------------- notes sync
# Fetch the remote notes refs and merge them into the local ones. On a conflict
# for the same commit the local note wins (`-s ours`): it is the newer result.
sync_notes() {
  [ "$PUSH" = 1 ] || return 0
  local ref tmp ferr
  for ref in "$NOTES_REF" "$FINDINGS_REF" "$WRITER_NOTES_REF"; do
    tmp="refs/notes/review-sync-${ref##*/}"
    if ferr="$(git fetch -q "$REMOTE" "+$ref:$tmp" 2>&1)"; then
      if git show-ref -q --verify "$ref"; then
        git notes --ref="$ref" merge -q -s ours "$tmp" >/dev/null 2>&1 \
          || { echo "review: could not merge remote $ref" >&2; git update-ref -d "$tmp"; return 1; }
      else
        git update-ref "$ref" "$tmp"
      fi
      git update-ref -d "$tmp"
    else
      case "$ferr" in
        *"couldn't find remote ref"*) ;;
        *) echo "review: could not fetch $ref from $REMOTE: ${ferr//$'\n'/ }" >&2; return 1 ;;
      esac
    fi
  done
}

# Push the notes refs; on a non-fast-forward, fetch + merge and retry.
push_notes() {
  [ "$PUSH" = 1 ] || return 0
  local attempt refs=("$NOTES_REF")
  git show-ref -q --verify "$FINDINGS_REF" && refs+=("$FINDINGS_REF")
  git show-ref -q --verify "$WRITER_NOTES_REF" && refs+=("$WRITER_NOTES_REF")
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
  git merge-base --is-ancestor "$BASE_SHA" "$HEAD_SHA" 2>/dev/null \
    || die "--base $BASE ($BASE_SHA) is not an ancestor of HEAD ($HEAD_SHA)"
fi

if ! WRITER="$(python3 -I "$SCRIPT_DIR/writer_provenance.py" derive --repo "$ROOT" --base "$BASE_SHA" --head "$HEAD_SHA" --reviewer "$REVIEWER" ${ASSERT_WRITER:+--assert-writer "$ASSERT_WRITER"} 2>"$WORK/prov.err")"; then
  die "$(cat "$WORK/prov.err")"
fi
case ",$WRITER," in
  *,"$REVIEWER",*) die "reviewer $REVIEWER cannot review its own work (writer $WRITER)" ;;
esac

if [ -n "$(git status --porcelain --untracked-files=no)" ]; then
  echo "review: warning: uncommitted changes are not reviewed; only HEAD is" >&2
fi

# Latest note on the branch (round count), and the latest note whose round
# actually reviewed its diff (delta start). A `blocked` round reviewed only part
# of its diff, so it does not advance the delta start. Neither does a clean or
# findings note with invalid writer provenance or whose base does not cover
# BASE_SHA (never widen base coverage across a delta).
PREV_SHA="" PREV_ROUNDS=0 PREV_VERDICT="" REVIEWED_SHA="" REVIEWED_ROUNDS=0 REVIEWED_VERDICT=""
while read -r c; do
  [ -n "$c" ] || continue
  fields_out="$(bash "$SCRIPT_DIR/verify.sh" --fields "$c" 2>/dev/null)" || continue
  v="$(note_field "$fields_out" verdict)"
  r="$(note_field "$fields_out" rounds)"
  nb="$(note_field "$fields_out" base)"
  nr="$(note_field "$fields_out" reviewer)"
  nw="$(note_field "$fields_out" writer)"
  if [ "$v" = clean ]; then
    if ! bash "$SCRIPT_DIR/verify.sh" --required-base "$BASE_SHA" "$c" >/dev/null 2>&1; then
      v=unverified-clean
    fi
  elif [ "$v" = findings ]; then
    if ! git merge-base --is-ancestor "$nb" "$BASE_SHA" 2>/dev/null \
      || ! git merge-base --is-ancestor "$nb" "$c" 2>/dev/null \
      || ! python3 -I "$SCRIPT_DIR/writer_provenance.py" verify --repo "$ROOT" --base "$nb" --head "$c" --claimed-writer "$nw" --reviewer "$nr" >/dev/null 2>&1; then
      v=unverified-findings
    fi
  fi
  if [ -z "$PREV_SHA" ]; then PREV_SHA="$c" PREV_ROUNDS="$r" PREV_VERDICT="$v"; fi
  if [ "$v" = clean ] || [ "$v" = findings ]; then REVIEWED_SHA="$c" REVIEWED_ROUNDS="$r" REVIEWED_VERDICT="$v"; break; fi
done < <(git rev-list --topo-order HEAD "^$BASE_SHA")

if [ "$PREV_SHA" = "$HEAD_SHA" ]; then
  case "$PREV_VERDICT" in
    clean)
      echo "review: HEAD $HEAD_SHA already reviewed clean (round $PREV_ROUNDS)"
      exit 0
      ;;
    blocked|unverified-clean|unverified-findings) ;;
    *)
      if [ "$PREV_ROUNDS" -ge "$MAX_ROUNDS" ]; then
        cap_message "$PREV_ROUNDS" "$(findings_for "$PREV_SHA" "$PREV_ROUNDS")"
        exit 3
      fi
      echo "review: HEAD already reviewed in round $PREV_ROUNDS (verdict $PREV_VERDICT)." >&2
      echo "review: commit the fixes, then run again." >&2
      exit 4
      ;;
  esac
fi

ROUND=$((PREV_ROUNDS + 1))
if [ "$ROUND" -gt "$MAX_ROUNDS" ]; then
  cap_message "$PREV_ROUNDS" "$(findings_for "$PREV_SHA" "$PREV_ROUNDS")"
  exit 3
fi

FROM="${REVIEWED_SHA:-$BASE_SHA}"
git diff --no-color --no-ext-diff -M "$FROM" "$HEAD_SHA" >"$WORK/full.diff"
if [ ! -s "$WORK/full.diff" ]; then
  echo "review: nothing to review: no changes between $FROM and HEAD" >&2
  exit 4
fi

REVIEW_TREE="$WORK/reviewed-tree"
prepare_review_archive "$ROOT" "$HEAD_SHA" "$REVIEW_TREE" "$WORK/tree.manifest"

# Open findings to carry forward: those of the latest round, and, when that
# round was blocked, also those of the last fully reviewed round (the blocked
# round may not have seen the code they are about).
PREV_FINDINGS=""
if [ -n "$PREV_SHA" ] && { [ "$PREV_VERDICT" = findings ] || [ "$PREV_VERDICT" = unverified-findings ] || [ "$PREV_VERDICT" = blocked ]; }; then
  PREV_FINDINGS="$(findings_for "$PREV_SHA" "$PREV_ROUNDS")"
fi
if [ "$REVIEWED_VERDICT" = findings ] && [ "$REVIEWED_SHA" != "$PREV_SHA" ]; then
  PREV_FINDINGS="$PREV_FINDINGS
$(findings_for "$REVIEWED_SHA" "$REVIEWED_ROUNDS")"
fi

# ---------------------------------------------------------------- split
TOTAL="$(wc -c <"$WORK/full.diff" | tr -d ' ')"
split_at() { rm -rf "$WORK/pieces"; python3 "$SCRIPT_DIR/split_diff.py" "$WORK/full.diff" "$WORK/pieces" "$1"; }
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
For every repository file you opened for context, write a line `READ: <path>` (using its path relative to the repository root; at least one such line is required) before the final two lines.
Your penultimate non-empty line must be exactly `REPOSITORY: readable` or `REPOSITORY: unreadable`. Use readable only after you have successfully opened repository files for context; if the sandbox or any repository read failed, use unreadable.
The last non-empty line of your reply must be exactly one of these two lines, with nothing after it:
VERDICT: clean
VERDICT: findings
Use `VERDICT: findings` when you report any finding, including an earlier finding that is still open.'
OWNER_RULE='OWNER RULE (2026-11-08, verbatim: "ALWAYS tell the reviewrs to do the best long term thing rather than cuitting corners"): judge and fix by the best long-term design. Never cut corners for speed, a smaller diff or fewer rounds. As a reviewer, report any shortcut, containment patch or corner cut as a real finding and say what the long-term fix is. As a writer, implement the structural fix, not a containment patch; if staging is truly unavoidable, record the long-term design and name its roadmap row.'

prompt_for() { # prompt_for <standard|neutral> <piece label> <group> <paths>
  local scope="$SCOPE_ROUND1"
  [ -n "$REVIEWED_SHA" ] && scope="$SCOPE_DELTA"
  if [ "$1" = standard ]; then
    cat <<EOF
You are reviewing a patch to Concrete, a compiler for a small systems programming language. The compiler is written in Lean 4; the repository also holds its standard library, test programs, test scripts and documentation. The working directory is a temporary git-archive copy of the complete repository at reviewed commit $HEAD_SHA. Open any files you need for context; do not change them.

This is review round $ROUND. $scope
This is $2 (file group: $3; files: $4).

Report correctness findings only: wrong behaviour, logic errors, regressions, unhandled cases, tests that do not check what they claim, and documentation that contradicts the code. Do not report style preferences.

Also answer one question, as a finding when the answer is yes: does this patch pick a side on any owner decision in docs/decisions.md (OD1 - OD157 under "Owner decisions", or an undecided project question), that is, choose, revise, encode, implement, extend or revert what one of them covers without the owner's explicit answer? Only the project owner decides those; a patch that does so must not land. Recording a decision the owner explicitly approved (the recording rule in docs/decisions.md) is permitted and is not picking a side. For such a patch, report as a finding: a recorded answer without its provenance (the owner's answer quoted with its date, or the quoted owner rule that decides it under the recording rule); wording that does not faithfully transcribe that answer; any change beyond its scope; and anything recorded as decided that the owner did not approve.
EOF
  else
    cat <<EOF
Please do a routine code review of the source-code patch below. It belongs to a compiler, a program that translates source code into machine code, and to that compiler's tests and documentation. Treat it as ordinary software engineering: look for logic errors and for behaviour that differs from what the code and its comments say. The working directory is a temporary archive of the complete repository at commit $HEAD_SHA; open any files you need for context and do not change them.

This is $2 (files: $4). $scope
Also report as a finding any change that picks a side on an owner decision listed in docs/decisions.md (OD1 - OD157) or an undecided project question without the owner's explicit answer. Writing down a decision the owner explicitly approved is allowed; report it only when it lacks the owner's quoted, dated answer (or the quoted owner rule that decides it under the recording rule in docs/decisions.md), changes that answer's meaning, goes beyond it, or adds something the owner did not approve.
EOF
  fi
  printf '\n%s\n' "$OWNER_RULE"
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

RAW_DIR="$ROOT/.review"
mkdir -p "$RAW_DIR"
RAW="$RAW_DIR/$HEAD_SHA-$ROUND.md"
LAST_RESULT=""
LAST_BODY=""
LAST_PROMPT=""
LAST_PROMPT_SHA=""
RUN_N=0
# run_one <prompt-kind> <piece-file> <label> <group> <paths>; sets LAST_RESULT to
# clean|findings|blocked|refused|noverdict|error and LAST_BODY to the reviewer's message.
run_one() {
  local kind="$1" piece="$2" label="$3" group="$4" paths="$5" rc=0 msg contract_out reason
  RUN_N=$((RUN_N + 1))
  local out="$WORK/out.$RUN_N.txt" log="$WORK/log.$RUN_N.txt" prompt="$WORK/prompt.$RUN_N.txt"
  LAST_PROMPT="$RAW_DIR/$HEAD_SHA-$ROUND.prompt.$RUN_N.txt"
  prompt_for "$kind" "$label" "$group" "$paths" >"$prompt"
  printf '\n--- supplied diff: %s ---\n' "$label" >>"$prompt"
  cat "$piece" >>"$prompt"
  cp "$prompt" "$LAST_PROMPT"
  LAST_PROMPT_SHA="$(sha256_file "$LAST_PROMPT")"
  # Collect only the reviewer's final message into $out (and diagnostic logs into $log) and
  # validate archive integrity, process outcome, final message and log via scripts/review/collect_output.sh.
  collect_reviewer_output "$REVIEWER" "$REVIEW_TREE" "$prompt" "$out" "$log" "$TIMEOUT" "$REVIEWER_BIN" || rc=$?
  msg="$(cat "$out")"
  LAST_BODY="$msg"
  contract_out="$(validate_reviewer_output "$ROOT" "$HEAD_SHA" "$REVIEW_TREE" "$WORK/tree.manifest" "$out" "$log" "clean,findings" "$rc")"
  LAST_RESULT="$(note_field "$contract_out" status)"
  reason="$(note_field "$contract_out" reason)"
  if [ "$(note_field "$contract_out" tree_modified)" = yes ]; then
    LAST_BODY="${msg:+$msg
}($reason; tree restored)"
    return 0
  fi
  if [ "$LAST_RESULT" = error ]; then
    LAST_BODY="$msg
(reviewer exited $rc; last lines of its log:)
$(tail -n 20 "$log")"
  elif [ "$LAST_RESULT" = blocked ] && bounded_is_incomplete "$rc"; then
    LAST_BODY="$msg
($reason; last lines of its log:)
$(tail -n 20 "$log")"
  fi
}

RAW_DIR="$ROOT/.review"
mkdir -p "$RAW_DIR"
RAW="$RAW_DIR/$HEAD_SHA-$ROUND.md"
UTC="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
{
  echo "# review of $HEAD_SHA, round $ROUND"
  echo
  echo "reviewer: $REVIEWER; writer: $WRITER; utc: $UTC; base: $BASE_SHA; diff from: $FROM; pieces: $K; diff bytes: $TOTAL"
} >"$RAW"

N_FINDINGS=0 N_BLOCKED=0
FINDINGS_TEXT=""
i=0
while IFS=$'\t' read -r name group bytes paths; do
  i=$((i + 1))
  label="piece $i of $K"
  echo "review: round $ROUND, $label ($group, $bytes bytes)" >&2
  run_one standard "$WORK/pieces/$name" "$label" "$group" "$paths"
  result="$LAST_RESULT" body="$LAST_BODY"
  {
    echo; echo "## $label: $group ($bytes bytes): $result"; echo; echo "files: $paths"
    echo "prompt: ${LAST_PROMPT#$ROOT/}"; echo "prompt sha256: $LAST_PROMPT_SHA"; echo
    echo "$body"
  } >>"$RAW"
  if [ "$result" = refused ] || [ "$result" = noverdict ] || [ "$result" = error ]; then
    # Retry once: smaller pieces (the piece halved) and a more neutral prompt.
    half_dir="$WORK/half-$i"
    half_limit=$(( (bytes + 1) / 2 ))
    python3 "$SCRIPT_DIR/split_diff.py" "$WORK/pieces/$name" "$half_dir" "$half_limit" --no-groups >/dev/null
    result=clean body=""
    j=0
    while IFS=$'\t' read -r hname _hgroup hbytes hpaths; do
      j=$((j + 1))
      hlabel="part $j of piece $i of $K"
      echo "review: retrying $hlabel with a neutral prompt ($hbytes bytes)" >&2
      run_one neutral "$half_dir/$hname" "$hlabel" "$group" "$hpaths"
      {
        echo; echo "### retry, $hlabel ($hbytes bytes): $LAST_RESULT"
        echo "prompt: ${LAST_PROMPT#$ROOT/}"; echo "prompt sha256: $LAST_PROMPT_SHA"; echo
        echo "$LAST_BODY"
      } >>"$RAW"
      case "$LAST_RESULT" in
        clean) ;;
        findings) [ "$result" = blocked ] || result=findings; body="$body
$LAST_BODY" ;;
        *) result=blocked ;;
      esac
    done <"$half_dir/manifest.tsv"
  fi
  stripped="$(printf '%s\n' "$body" | python3 -I "$SCRIPT_DIR/output_contract.py" --strip)"
  case "$result" in
    findings)
      N_FINDINGS=$((N_FINDINGS + 1))
      FINDINGS_TEXT="$FINDINGS_TEXT
[$label: $group]
$stripped"
      ;;
    blocked)
      N_BLOCKED=$((N_BLOCKED + 1))
      FINDINGS_TEXT="$FINDINGS_TEXT
[$label: $group] blocked: the reviewer could not review this piece or read the repository. Files: $paths
$stripped"
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
writer: $WRITER
utc: $UTC
base: $BASE_SHA
pieces: $K
EOF
if [ "$VERDICT" = clean ]; then echo "repository: readable" >>"$WORK/note"; fi
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
