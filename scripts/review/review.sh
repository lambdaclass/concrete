#!/usr/bin/env bash
# R-0544 — one review procedure for every agent. See docs/REVIEW.md.
#
#   scripts/review/review.sh [--base <ref>] [--head <ref>] [--max-rounds 5]
#                            [--pieces auto|N] [--prompt-extra "..."]
#
# --head names the commit the caller means to review (train.sh and ready.sh pass it); it must be
# the checkout's HEAD, or nothing runs (exit 2): the review always reads HEAD's tree.
#
# Runs ONE review round on HEAD and records the result as a git note on HEAD
# under refs/notes/concrete-review-v2, which it then pushes.
#
#   round 1      reviews the full diff base..HEAD (base defaults to the merge-base
#                with origin/main);
#   round N > 1  uses a delta only from verified clean coverage of the current
#                base. Otherwise it reviews the full base..HEAD diff. Unresolved
#                findings reach the prompt either way, and the round cap carries over.
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
#   6  result recorded locally but the notes could not be published
#
# Environment (mostly for the test gate, scripts/checks/review_tool.sh):
#   REVIEW_CODEX_BIN    reviewer binary (default: codex)
#   REVIEW_MAIN_REF     ref the default base is computed against (default: origin/main)
#   REVIEW_PUSH         1 to fetch/push the notes (default: 1)
#   REVIEW_PIECE_BYTES  size limit of one piece in bytes (default: 60000)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/../checks/lib/has_match.sh" || exit 2  # bug 283: never a pipe into grep -q
ROOT="$(git rev-parse --show-toplevel)"
cd "$ROOT"

CODEX_BIN="${REVIEW_CODEX_BIN:-codex}"
REMOTE=origin
MAIN_REF="${REVIEW_MAIN_REF:-$REMOTE/main}"
PUSH="${REVIEW_PUSH:-1}"
PIECE_BYTES="${REVIEW_PIECE_BYTES:-60000}"
source "$SCRIPT_DIR/refs.sh"
NOTES_REF="$REVIEW_NOTES_REF"
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
if [ -n "${WANT_HEAD:-}" ]; then
  want="$(git rev-parse -q --verify "$WANT_HEAD^{commit}")" || die "--head $WANT_HEAD is not a commit"
  [ "$want" = "$(git rev-parse HEAD)" ] || die "--head $WANT_HEAD is ${want:0:12}, but the checkout is at $(git rev-parse --short=12 HEAD)"
fi
[[ "$PIECE_BYTES" =~ ^[1-9][0-9]*$ ]] || die "REVIEW_PIECE_BYTES must be a positive integer"
command -v "$CODEX_BIN" >/dev/null 2>&1 || die "reviewer binary not found: $CODEX_BIN"

WORK="$(mktemp -d)"
STAGE_REF="refs/notes/review-stage-$$-$RANDOM"
SYNC_REF="refs/notes/review-sync-$$-$RANDOM"
LEGACY_STAGE_REF="refs/notes/review-legacy-stage-$$-$RANDOM"
cleanup() {
  git update-ref -d "$STAGE_REF"
  git update-ref -d "$SYNC_REF"
  git update-ref -d "$LEGACY_STAGE_REF"
  rm -rf "$WORK"
}
trap cleanup EXIT

# ---------------------------------------------------------------- notes sync
# Compare notes by annotated object ID, independent of changes in Git's fanout.
note_entries() {
  git ls-tree -r --format='%(objectname) %(path)' "$1" |
    tr -d / | awk 'length($2) == 40 && $2 ~ /^[0-9a-f]+$/'
}

sync_notes() {
  [ "$PUSH" = 1 ] || return 0
  fetch_review_ref "$REMOTE" "$NOTES_REF" "$SYNC_REF" || return 1
  local remote local_tip common merged
  remote="$(git rev-parse -q --verify "$SYNC_REF")" || return 0
  local_tip="$(git rev-parse -q --verify "$NOTES_REF" || true)"
  if [ -z "$local_tip" ]; then
    git update-ref "$NOTES_REF" "$remote" ""
    return
  fi
  git merge-base --is-ancestor "$remote" "$local_tip" && return 0
  if git merge-base --is-ancestor "$local_tip" "$remote"; then
    git update-ref "$NOTES_REF" "$remote" "$local_tip"
    return
  fi

  git update-ref "$STAGE_REF" "$local_tip" || return 1
  git notes --ref="$STAGE_REF" merge -q -s ours "$SYNC_REF" || return 1
  merged="$(git rev-parse --verify "$STAGE_REF")" || return 1
  # An automatic merge can combine unrelated notes. If both writers changed
  # the same note differently, require a retry against reconciled evidence.
  common="$(git merge-base "$local_tip" "$remote" || true)"
  : >"$WORK/common-notes"
  if [ -n "$common" ]; then note_entries "$common" >"$WORK/common-notes" || return 1; fi
  note_entries "$remote" >"$WORK/remote-notes" || return 1
  note_entries "$merged" >"$WORK/merged-notes" || return 1
  awk '
    FILENAME == ARGV[1] { base[$2] = $1; next }
    FILENAME == ARGV[2] { remote[$2] = $1; next }
                       { merged[$2] = $1 }
    END {
      for (sha in remote)
        if (remote[sha] != base[sha] && merged[sha] != remote[sha]) exit 1
      for (sha in base)
        if (!(sha in remote) && (sha in merged)) exit 1
    }
  ' "$WORK/common-notes" "$WORK/remote-notes" "$WORK/merged-notes" \
    || { echo "review: conflicting local and remote evidence; reconcile the notes before retrying" >&2; return 1; }
  git update-ref "$NOTES_REF" "$merged" "$local_tip"
}

# Validate every new or changed record against the fetched tip before pushing
# the exact staged snapshot. A malformed pending record may be on another branch.
validate_publication() {
  local tip="$1"
  local previous="$2"
  local sha rc
  : >"$WORK/remote-notes"
  if [ -n "$previous" ]; then note_entries "$previous" >"$WORK/remote-notes" || return 1; fi
  note_entries "$tip" >"$WORK/local-notes" || return 1
  awk '
    FILENAME == ARGV[1] { remote[$2] = $1; next }
    { if ($1 != remote[$2]) print $2; delete remote[$2] }
    END { for (sha in remote) exit 1 }
  ' "$WORK/remote-notes" "$WORK/local-notes" >"$WORK/changed-notes" \
    || { echo "review: refusing to delete published review evidence" >&2; return 1; }
  while IFS= read -r sha; do
    rc=0
    bash "$SCRIPT_DIR/verify.sh" --notes-ref "$STAGE_REF" "$sha" >/dev/null || rc=$?
    [ "$rc" -le 1 ] \
      || { echo "review: refusing to publish invalid local evidence for $sha" >&2; return 1; }
  done <"$WORK/changed-notes"
}

# Deny legacy landing on the exact SHA of each changed non-clean record.
# The marker is a valid legacy findings verdict so an old reviewer syncing
# normally also refuses to replace it on the same HEAD. The v2 blob owns the
# actual verdict and findings text.
stage_legacy_denials() {
  local sha note blob
  legacy_previous=""
  while IFS= read -r sha; do
    note="$(git notes --ref="$STAGE_REF" show "$sha")" || return 1
    [ "$(note_field "$note" verdict)" != clean ] || continue
    if [ -z "$legacy_previous" ]; then
      fetch_review_ref "$REMOTE" "$LEGACY_REVIEW_REF" "$LEGACY_STAGE_REF" || return 1
      legacy_previous="$(git rev-parse -q --verify "$LEGACY_STAGE_REF" || echo absent)"
    fi
    blob="$(git notes --ref="$STAGE_REF" list "$sha")" || return 1
    printf 'verdict: findings\nrounds: %s\nreviewer: v2-denial-%s\nutc: %s\nbase: %s\npieces: %s\n' \
      "$(note_field "$note" rounds)" "$blob" \
      "$(note_field "$note" utc)" "$(note_field "$note" base)" \
      "$(note_field "$note" pieces)" >"$WORK/legacy-denial"
    if [ "$(git notes --ref="$LEGACY_STAGE_REF" list "$sha" 2>/dev/null)" = "$(git hash-object "$WORK/legacy-denial")" ]; then
      continue
    fi
    git notes --ref="$LEGACY_STAGE_REF" add -f -F "$WORK/legacy-denial" "$sha" || return 1
  done <"$WORK/changed-notes"
}

# Push the captured notes commit; on a non-fast-forward, fetch + merge and retry.
push_notes() {
  [ "$PUSH" = 1 ] || return 0
  local attempt tip previous legacy_tip legacy_previous
  local -a push_args refspecs
  for attempt in 1 2 3; do
    tip="$(git rev-parse --verify "$NOTES_REF")" || return 1
    previous="$(git rev-parse -q --verify "$SYNC_REF" || true)"
    [ "$tip" != "$previous" ] || return 0
    if ! has_v2_review_tooling HEAD ||
       ! git diff --quiet HEAD -- scripts/review/refs.sh; then
      echo "review: checkout lacks the committed v2 review tooling; update it from origin/main before publishing" >&2
      return 1
    fi
    git update-ref "$STAGE_REF" "$tip" || return 1
    validate_publication "$tip" "$previous" || return 1
    push_args=(-q)
    refspecs=("$tip:$NOTES_REF")
    # Read and stage legacy only when publishing a denial. Clean-only pushes
    # cannot change what a legacy reader sees.
    stage_legacy_denials || return 1
    if [ -n "$legacy_previous" ]; then
      legacy_tip="$(git rev-parse --verify "$LEGACY_STAGE_REF")" || return 1
      if [ "$legacy_tip" != "$legacy_previous" ]; then
        refspecs+=("$legacy_tip:$LEGACY_REVIEW_REF")
        push_args+=(--atomic)
      fi
    fi
    if git push "${push_args[@]}" "$REMOTE" "${refspecs[@]}" 2>"$WORK/push.err"; then
      return 0
    fi
    # Only a non-fast-forward is cured by fetch + merge. Anything else (a hook
    # refusal, auth, network) fails at once instead of being repeated.
    if ! grep -qE '\[rejected\].*\((fetch first|non-fast-forward)\)' "$WORK/push.err"; then
      break
    fi
    echo "review: notes push attempt $attempt rejected; fetching and merging" >&2
    sync_notes || return 1
  done
  cat "$WORK/push.err" >&2
  return 1
}

publish_or_exit() {
  if ! push_notes; then
    echo "review: the result is recorded locally but the review notes could not be pushed to $REMOTE." >&2
    echo "review: resolve the publication error and rerun review.sh to retry." >&2
    exit 6
  fi
}

note_field() { # note_field <text> <key>
  printf '%s\n' "$1" | awk -v k="$2: " 'index($0, k) == 1 && !done { print substr($0, length(k) + 1); done = 1 }'
}

legacy_findings() {
  local sha="$1"
  local verdict="$2"
  local ref="$3"
  local findings
  if ! findings="$(git notes --ref="$ref" show "$sha" 2>/dev/null)"; then
    # Legacy blocked notes may have no text. A findings verdict always needs it.
    [ "$verdict" = blocked ]
    return
  fi
  [[ "$findings" =~ [^[:space:]] ]] || return 1
  printf '%s\n' "$findings"
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
if [ "$PUSH" = 1 ]; then
  fetch_review_ref "$REMOTE" "$LEGACY_REVIEW_REF" "$PUBLISHED_LEGACY_REVIEW_REF" || die "could not read legacy evidence"
  fetch_review_ref "$REMOTE" "$LEGACY_FINDINGS_REF" "$PUBLISHED_LEGACY_FINDINGS_REF" || die "could not read legacy findings"
fi

intake_tip="$(git rev-parse -q --verify "$NOTES_REF" || true)"
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

# Track round counts separately from the latest verified clean delta start.
PREV_VALID=0
PREV_SHA="" PREV_ROUNDS=0 PREV_VERDICT="" REVIEWED_SHA="" REVIEWED_ROUNDS=0 REVIEWED_NOTE=""
INSUFFICIENT_RANGE=0
OPEN_FINDINGS=0
PREV_FINDINGS=""
COVERING_REVIEWS=""
LEGACY_CLEAN_REVIEWS=""
while read -r c; do
  [ -n "$c" ] || continue
  # New evidence takes precedence for the same commit, even when invalid.
  ref="$NOTES_REF"
  if ! blob="$(git notes --ref="$ref" list "$c" 2>/dev/null)"; then
    ref="$PUBLISHED_LEGACY_REVIEW_REF"
    if [ "$PUSH" != 1 ] || ! blob="$(git notes --ref="$ref" list "$c" 2>/dev/null)"; then
      ref="$LEGACY_REVIEW_REF"
      blob="$(git notes --ref="$ref" list "$c" 2>/dev/null)" || continue
    fi
  fi
  n="$(git cat-file blob "$blob")"
  v="$(note_field "$n" verdict)"
  r="$(note_field "$n" rounds)"
  intake_mode=--legacy-accounting
  if [ "$ref" = "$NOTES_REF" ]; then
    intake_mode=--metadata-only
  elif [[ "$(note_field "$n" reviewer)" == v2-denial-* ]]; then
    die "legacy denial for $c has no matching v2 snapshot; fetch the current review refs and retry"
  fi
  intake_rc=0
  bash "$SCRIPT_DIR/verify.sh" "$intake_mode" --notes-ref "$ref" "$c" >"$WORK/intake" || intake_rc=$?
  if [ "$intake_rc" -ne 0 ]; then
    if [ "$intake_rc" -eq 3 ] || { [ "$ref" = "$NOTES_REF" ] && { [ "$v" = findings ] || [ "$v" = blocked ]; }; }; then
      die "invalid review evidence for $c round $r: $(head -n 1 "$WORK/intake")"
    fi
    continue
  fi
  [ "$r" -le "$PREV_ROUNDS" ] || PREV_ROUNDS="$r"
  vrc=2
  if [ "$ref" = "$NOTES_REF" ]; then
    vrc=0
    bash "$SCRIPT_DIR/verify.sh" --required-base "$BASE_SHA" "$c" >/dev/null 2>&1 || vrc=$?
  fi
  [ "$(git notes --ref="$ref" list "$c" 2>/dev/null)" = "$blob" ] \
    || die "review note changed during intake for $c; retry"
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
  if [ "$ref" != "$NOTES_REF" ]; then
    # A later legacy clean round can retire older legacy findings under the
    # historical range rule. It cannot supply v2 coverage or retire v2 findings.
    for entry in $LEGACY_CLEAN_REVIEWS; do
      if [ "${entry#*:}" -gt "$r" ] && git merge-base --is-ancestor "$c" "${entry%:*}"; then
        covered=1
        break
      fi
    done
    if [ "$v" = clean ] && git merge-base --is-ancestor "$(note_field "$n" base)" "$BASE_SHA" 2>/dev/null; then
      LEGACY_CLEAN_REVIEWS="$LEGACY_CLEAN_REVIEWS $c:$r"
    fi
  fi
  if [ "$covered" = 0 ] && [ "$v" != clean ]; then
    if [ "$ref" != "$NOTES_REF" ]; then
      findings_ref="$LEGACY_FINDINGS_REF"
      if [ "$ref" = "$PUBLISHED_LEGACY_REVIEW_REF" ]; then findings_ref="$PUBLISHED_LEGACY_FINDINGS_REF"; fi
      findings="$(legacy_findings "$c" "$v" "$findings_ref")" \
        || die "missing or mismatched findings text for $c round $r"
    else
      findings="${n#*$'\n\n'}"
    fi
    PREV_FINDINGS="$PREV_FINDINGS
$findings"
    if [ "$v" = findings ]; then OPEN_FINDINGS=1; fi
  fi
  if [ "$vrc" -le 1 ] && { [ "$v" = clean ] || [ "$v" = findings ]; }; then
    COVERING_REVIEWS="$COVERING_REVIEWS $c"
    if [ "$v" = clean ] && [ -z "$REVIEWED_SHA" ]; then
      REVIEWED_SHA="$c" REVIEWED_ROUNDS="$r" REVIEWED_NOTE="$blob"
    fi
  fi
done < <(git rev-list --topo-order HEAD "^$BASE_SHA")

if [ "$INSUFFICIENT_RANGE" = 0 ] && [ "$PREV_VALID" = 1 ] && [ "$PREV_SHA" = "$HEAD_SHA" ] && [ "$PREV_VERDICT" != blocked ]; then
  publish_or_exit
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
  if [ -n "$intake_tip" ]; then publish_or_exit; fi
  cap_message "$PREV_ROUNDS" "$PREV_FINDINGS"
  exit 3
fi

FROM="${REVIEWED_SHA:-$BASE_SHA}"
FULL_REVIEW=0
if [ "$INSUFFICIENT_RANGE" = 1 ] || [ "$OPEN_FINDINGS" = 1 ] ||
   { [ -n "$REVIEWED_SHA" ] && ! git merge-base --is-ancestor "$BASE_SHA" "$REVIEWED_SHA"; }; then
  # Missing range coverage or an old review predating changes imported from base
  # requires full review. Comparing an old commit directly
  # with HEAD would review base's changes too. Re-review the branch's complete
  # current contribution, including merge resolutions, without resetting rounds.
  FROM="$BASE_SHA"
  FULL_REVIEW=1
  echo "review: prior review does not cover current branch scope cleanly; reviewing full base..HEAD diff (round $ROUND)" >&2
fi
git diff --no-color --no-ext-diff -M "$FROM" "$HEAD_SHA" >"$WORK/full.diff"
if [ ! -s "$WORK/full.diff" ]; then
  echo "review: nothing to review: no changes between $FROM and HEAD" >&2
  exit 4
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
The last line of your reply must be exactly one of these two lines, with nothing after it:
VERDICT: clean
VERDICT: findings
Use `VERDICT: findings` when you report any finding, including an earlier finding that is still open.'

prompt_for() { # prompt_for <standard|neutral> <piece label> <group> <paths>
  local scope="$SCOPE_ROUND1"
  [ -n "$REVIEWED_SHA" ] && [ "$FULL_REVIEW" = 0 ] && scope="$SCOPE_DELTA"
  if [ "$1" = standard ]; then
    cat <<EOF
You are reviewing a patch to Concrete, a compiler for a small systems programming language. The compiler is written in Lean 4; the repository also holds its standard library, test programs, test scripts and documentation. The repository is checked out read-only in the working directory, and you may open any file in it for context.

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

# A refusal is recognized only when there is no verdict line: a real review that
# merely discusses content filters (as a review of this tool does) still parses.
REFUSAL_RE="(I can.?t|I cannot|I won.?t|I.?m (not able|unable)|unable to) (help|assist|comply|review|continue|provide)|cyber ?security|content (filter|policy)|usage polic|safety (system|filter|policy)|flagged (as|for)|potentially (harmful|malicious)|invalid_prompt|violat(es|ion of) .*polic"

LAST_RESULT=""
LAST_BODY=""
RUN_N=0
# run_one <prompt-kind> <piece-file> <label> <group> <paths>; sets LAST_RESULT to
# clean|findings|refused|noverdict|error and LAST_BODY to the reviewer's message.
run_one() {
  local kind="$1"
  local piece="$2"
  local label="$3"
  local group="$4"
  local paths="$5"
  local rc=0
  local msg
  local last
  RUN_N=$((RUN_N + 1))
  local out="$WORK/out.$RUN_N.txt"
  local log="$WORK/log.$RUN_N.txt"
  : >"$out"
  # GIT_OPTIONAL_LOCKS=0 (bug 209): the reviewer runs git in the caller's worktree, and its sandbox
  # does not stop `git status` from taking .git's index.lock to refresh the index. A command it
  # stops mid-refresh left that lock behind, 0 bytes, and the next commit in the worktree failed.
  # Without optional locks a status reads the index and writes nothing.
  GIT_OPTIONAL_LOCKS=0 "$CODEX_BIN" exec --sandbox read-only --ephemeral --color never -C "$ROOT" -o "$out" \
    "$(prompt_for "$kind" "$label" "$group" "$paths")" <"$piece" >"$log" 2>&1 || rc=$?
  msg="$(cat "$out")"
  [ -n "$msg" ] || [ "$rc" -ne 0 ] || msg="$(cat "$log")"
  LAST_BODY="$msg"
  last="$(printf '%s\n' "$msg" | sed -e 's/[`*]//g' -e 's/[[:space:]]*$//' | awk 'NF { l = $0 } END { print l }')"
  if [ "$rc" -eq 0 ] && [ "$last" = "VERDICT: clean" ]; then LAST_RESULT=clean
  elif [ "$rc" -eq 0 ] && [ "$last" = "VERDICT: findings" ]; then LAST_RESULT=findings
  elif { printf '%s\n' "$msg"; cat "$log"; } | has_match -iE "$REFUSAL_RE"; then LAST_RESULT=refused
  elif [ "$rc" -ne 0 ]; then
    LAST_RESULT=error
    LAST_BODY="$msg
(reviewer exited $rc; last lines of its log:)
$(tail -n 20 "$log")"
  else LAST_RESULT=noverdict
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
  result="$LAST_RESULT" body="$LAST_BODY"
  {
    echo; echo "## $label: $group ($bytes bytes): $result"; echo; echo "files: $paths"; echo
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
      { echo; echo "### retry, $hlabel ($hbytes bytes): $LAST_RESULT"; echo; echo "$LAST_BODY"; } >>"$RAW"
      case "$LAST_RESULT" in
        clean) ;;
        findings) [ "$result" = blocked ] || result=findings; body="$body
$LAST_BODY" ;;
        *) result=blocked ;;
      esac
    done <"$half_dir/manifest.tsv"
  fi
  stripped="$(printf '%s\n' "$body" | grep -vE '^[`*[:space:]]*VERDICT:' || true)"
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
[$label: $group] blocked: the reviewer refused or gave no verdict, also after one neutral retry. Files: $paths
$stripped"
      ;;
  esac
done <"$WORK/pieces/manifest.tsv"

# `blocked` wins over `findings`: a round with a blocked piece did not review all
# of its diff. The findings of the pieces that were reviewed are still
# recorded and carried forward.
if [ "$N_BLOCKED" -gt 0 ]; then VERDICT=blocked
elif [ "$N_FINDINGS" -gt 0 ]; then VERDICT=findings
else VERDICT=clean
fi
{ echo; echo "## verdict: $VERDICT ($N_FINDINGS piece(s) with findings, $N_BLOCKED blocked)"; } >>"$RAW"

# ---------------------------------------------------------------- record
# THE NOTE FORMAT IS AN INTERFACE: scripts/review/verify.sh parses it (train.sh's review leg reads
# it through verify.sh). Change it only together with verify.sh.
if [ "$VERDICT" = blocked ] && [ -n "$PREV_FINDINGS" ]; then
  # A blocked round cannot resolve the findings it was given: some code went
  # unreviewed. Record them with this round's own, so a later round (even after
  # several blocked rounds in a row, or a re-run on the same HEAD) still gets them.
  FINDINGS_TEXT="$FINDINGS_TEXT
[carried from earlier rounds; not confirmed resolved, because this round was blocked]
$PREV_FINDINGS"
fi
if [ "$VERDICT" != clean ]; then
  # Canonicalise once so the hash covers the exact text stored in the note.
  printf '%s\n' "$FINDINGS_TEXT" | git stripspace >"$WORK/findings"
  FINDINGS_NOTE="$(git hash-object -- "$WORK/findings")"
fi
cat >"$WORK/note" <<EOF
verdict: $VERDICT
rounds: $ROUND
reviewer: $REVIEWER
utc: $UTC
base: $BASE_SHA
pieces: $K
diff-start: $FROM
EOF
if [ "$FROM" != "$BASE_SHA" ]; then
  printf 'predecessor-note: %s\n' "$REVIEWED_NOTE" >>"$WORK/note"
fi
if [ "$VERDICT" != clean ]; then
  printf 'findings-note: %s\n\n' "$FINDINGS_NOTE" >>"$WORK/note"
  cat "$WORK/findings" >>"$WORK/note"
fi
if [ -n "$intake_tip" ]; then git update-ref "$STAGE_REF" "$intake_tip"; fi
git notes --ref="$STAGE_REF" add -f -F "$WORK/note" "$HEAD_SHA"
vrc=0
bash "$SCRIPT_DIR/verify.sh" --notes-ref "$STAGE_REF" --required-base "$BASE_SHA" "$HEAD_SHA" >/dev/null || vrc=$?
[ "$vrc" -le 1 ] || die "staged review evidence did not verify"
git update-ref "$NOTES_REF" "$STAGE_REF" "$intake_tip" || die "local review notes changed; retry"

echo "review: round $ROUND verdict $VERDICT ($K piece(s)); raw output: ${RAW#$ROOT/}"
[ "$VERDICT" = clean ] || printf '%s\n' "${FINDINGS_TEXT#$'\n'}"

publish_or_exit

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
