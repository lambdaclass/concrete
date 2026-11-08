#!/usr/bin/env bash
# OWNER SIGN-OFF RECORDS (ROADMAP R-0618; owner, 2026-11-08: "when a branch reaches its review-round
# cap, the owner may approve one exact commit SHA after Codex, Kimi and agy have all reviewed it; the
# approval is recorded"). A sign-off is review provenance for that one commit, as a clean
# refs/notes/concrete-review note is (scripts/review/verify.sh); train.sh accepts either for a member.
#
#   signoff.sh collect <commit> --reviewer codex|kimi|agy --request FILE --out FILE
#                      [--log FILE] [--timeout SECS]
#   signoff.sh record <commit> --base BASE --rounds-at NOTED --date YYYY-MM-DD --quote TEXT
#                     --codex-request FILE --codex FILE --kimi-request FILE --kimi FILE
#                     --agy-request FILE --agy FILE
#   signoff.sh verify <commit> --base BASE
#
# THE RECORD is the ref refs/owner-signoff/<full sha>, a parentless commit whose tree holds exactly
# seven plain files: record.txt and, for each reviewer r in codex, kimi and agy, r-request.txt and
# r.txt. The output is copied byte for byte. The request is copied verbatim after a structured first
# line, `reviewed: <full sha>`, which `record` prepends to attest the commit reviewed. Thus a clone
# that fetches the ref can check what each reviewer was asked and its output separately. record.txt
# is `key: value` lines, each key once:
#   signoff: owner
#   commit: <the 40-hex SHA approved>
#   date: YYYY-MM-DD
#   quote: <the owner's words, verbatim; they name the commit by an unambiguous prefix of 7+ hex digits>
#   rounds-at: <the 40-hex SHA of the commit whose scripts/review/review.sh note records the rounds used>
#   request-codex: codex-request.txt <sha256>
#   verdict-codex: codex.txt <sha256>
#   request-kimi: kimi-request.txt <sha256>
#   verdict-kimi: kimi.txt <sha256>
#   request-agy: agy-request.txt <sha256>
#   verdict-agy: agy.txt <sha256>
# `record` builds the record, verifies it, and only then writes the ref locally; it pushes nothing
# (push the ref to both remotes: the pre-push hook takes a refs/owner-signoff/* record in its
# no-source-tree tier). Only the owner's recorded words go in a quote.
#
# THE CAP (R-0618) comes from the class of the member's diff against its base, classified by path,
# the highest class present winning: docs and roadmap 2, scripts 3, compiler and spec 5; a path in no
# class counts as compiler and spec. A sign-off qualifies only when the review rounds recorded on the
# member's own commits reach that cap: `rounds-at` must be the commit itself or one of its ancestors
# not on BASE, and carry a well-formed review.sh note (scripts/review/verify.sh: clean, findings or
# blocked) whose round is at least the cap, whatever its verdict
# (the cap counts rounds used: the other reviewers' rejections are why a branch reaches it).
#
# `verify` accepts the record only if the ref exists, the tree holds exactly those seven 100644 blobs,
# every key is present once and well formed, `commit` is the commit asked about, the quote names it,
# the cap was reached, and every file's sha256 is the recorded one. Each request's `reviewed:` line
# must equal the signed commit, and its body must name that commit by an unambiguous prefix of 7+
# hex digits; output mentions do not bind a review to a commit. Each output must pass
# `scripts/review/output_contract.py` (at least one valid `READ: <path>` from the signed commit's
# tree, `REPOSITORY: readable` as the penultimate non-empty line, and `VERDICT: ACCEPT` or
# `VERDICT: REJECT` as the last non-empty line); both ACCEPT and REJECT are recorded and reported,
# and an output without a verdict or failing the contract is refused. Request lines never count as
# verdicts. The sign-off judges the exact commit and says nothing about any other commit.
#
# Exit codes: 0 a valid sign-off; 1 a sign-off that does not qualify (the reason is printed);
#             2 no sign-off, or a usage error.
set -euo pipefail
PREFIX=refs/owner-signoff
NOTES_REF=refs/notes/concrete-review
REVIEWERS="codex kimi agy"
REVIEW_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../review" && pwd)"
VERIFY="$REVIEW_DIR/verify.sh"
OUTPUT_CONTRACT="$REVIEW_DIR/output_contract.py"
source "$REVIEW_DIR/collect_output.sh" || exit 2

usage() { awk 'NR > 1 { if (!/^#/) exit; sub(/^# ?/, ""); print }' "${BASH_SOURCE[0]}"; }
die() { echo "signoff: $*" >&2; exit 2; }
sha256() { if command -v sha256sum >/dev/null 2>&1; then sha256sum | cut -d' ' -f1; else shasum -a 256 | cut -d' ' -f1; fi; }

# path_class PATH → docs | scripts | compiler (anything unlisted is compiler: the highest class)
path_class() {
  case "$1" in
    docs/*|changelog.d/*|ROADMAP.md|CHANGELOG.md|README.md|AGENTS.md|CLAUDE.md) echo docs ;;
    scripts/*|.githooks/*|.github/*|.claude/*|required-checks.txt|required-checks-transition.tsv) echo scripts ;;
    *) echo compiler ;;
  esac
}
# names SHA → reads text on stdin; succeeds when a hex word of at least 7 digits in it is an
# unambiguous name of SHA in this repository (git refuses an ambiguous prefix)
names() {
  local w
  for w in $(tr -c '0-9a-f' ' '); do
    [ "${#w}" -ge 7 ] && [ "${1#"$w"}" != "$1" ] && [ "$(git rev-parse -q --verify "$w^{commit}" 2>/dev/null)" = "$1" ] && return 0
  done
  return 1
}
class_cap() { case "$1" in docs) echo 2 ;; scripts) echo 3 ;; *) echo 5 ;; esac; }
# diff_class SHA BASE → the highest class among the paths SHA changes against its merge base with BASE
diff_class() {
  local mb p c best=docs
  mb="$(git merge-base "$2" "$1")" || return 1
  while IFS= read -r p; do
    [ -n "$p" ] || continue
    c="$(path_class "$p")"
    case "$c" in compiler) best=compiler ;; scripts) [ "$best" = compiler ] || best=scripts ;; esac
  done < <(git diff --no-renames --name-only "$mb" "$1")
  echo "$best"
}

cmd_record() {
  [ $# -ge 1 ] || die "record: name the commit"
  local sha date="" quote="" base="" at="" r f q v; sha="$(git rev-parse -q --verify "$1^{commit}")" || die "record: not a commit: $1"; shift
  local codex="" kimi="" agy="" codex_request="" kimi_request="" agy_request=""
  while [ $# -gt 0 ]; do
    [ $# -ge 2 ] || die "record: $1 needs a value"
    case "$1" in
      --date) date="$2" ;; --quote) quote="$2" ;; --base) base="$2" ;; --rounds-at) at="$2" ;;
      --codex) codex="$2" ;; --kimi) kimi="$2" ;; --agy) agy="$2" ;;
      --codex-request) codex_request="$2" ;; --kimi-request) kimi_request="$2" ;; --agy-request) agy_request="$2" ;;
      *) die "record: unknown option $1" ;;
    esac; shift 2
  done
  [[ "$date" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}$ ]] || die "record: --date YYYY-MM-DD is required"
  [ -n "$quote" ] && [[ "$quote" != *$'\n'* ]] || die "record: --quote is required, on one line"
  [ -n "$base" ] || die "record: --base BASE is required"
  at="$(git rev-parse -q --verify "$at^{commit}")" || die "record: --rounds-at must name the noted commit"
  local rec="signoff: owner
commit: $sha
date: $date
quote: $quote
rounds-at: $at" entries=""
  for r in $REVIEWERS; do
    q="${r}_request"; q="${!q}"; [ -f "$q" ] || die "record: --$r-request FILE is required (the verbatim review request)"
    f="${!r}"; [ -f "$f" ] || die "record: --$r FILE is required (the reviewer's verdict output)"
    v="$({ printf 'reviewed: %s\n' "$sha"; cat -- "$q"; } | git hash-object -w --no-filters --stdin)"
    rec="$rec
request-$r: $r-request.txt $({ printf 'reviewed: %s\n' "$sha"; cat -- "$q"; } | sha256)"
    entries="${entries}100644 blob $v	$r-request.txt
"
    v="$(git hash-object -w --no-filters -- "$f")"
    rec="$rec
verdict-$r: $r.txt $(sha256 < "$f")"
    entries="${entries}100644 blob $v	$r.txt
"
  done
  v="$(printf '%s\n' "$rec" | git hash-object -w --no-filters --stdin)"
  local tree commit
  tree="$(printf '%s100644 blob %s\trecord.txt\n' "$entries" "$v" | git mktree)"
  commit="$(git commit-tree "$tree" -m "owner sign-off of $sha ($date)")"
  local out
  out="$( check "$sha" "$commit" "$base" )" || { printf '%s\nsignoff: not recorded (nothing written)\n' "$out" >&2; exit 1; }
  git update-ref "$PREFIX/$sha" "$commit"
  echo "$out"
  echo "signoff: recorded $PREFIX/$sha -> ${commit:0:10}; push it to both remotes"
}

# check SHA RECORD BASE: judge the record commit RECORD as a sign-off of SHA (exit 1 with the reason)
check() {
  local sha="$1" obj="$2" base="$3"
  bad() { echo "$sha owner sign-off does not qualify: $*"; exit 1; }
  [ "$(git cat-file -t "$obj:record.txt" 2>/dev/null)" = blob ] || bad "the record holds no record.txt"
  local rec line k v seen=" " F_signoff="" F_commit="" F_date="" F_quote="" F_rounds_at=""
  local F_request_codex="" F_verdict_codex="" F_request_kimi="" F_verdict_kimi="" F_request_agy="" F_verdict_agy=""
  rec="$(git cat-file blob "$obj:record.txt")"
  while IFS= read -r line; do
    [[ "$line" =~ ^([a-z-]+):\ (.*)$ ]] || bad "record.txt line is not key: value: '$line'"
    k="${BASH_REMATCH[1]}" v="${BASH_REMATCH[2]}"
    case "$seen" in *" $k "*) bad "record.txt repeats $k" ;; esac
    seen="$seen$k "
    case "$k" in signoff|commit|date|quote|rounds-at|request-codex|verdict-codex|request-kimi|verdict-kimi|request-agy|verdict-agy) ;; *) bad "record.txt has the unknown key $k" ;; esac
    printf -v "F_${k//-/_}" '%s' "$v"
  done <<< "$rec"
  for k in signoff commit date quote rounds-at; do case "$seen" in *" $k "*) ;; *) bad "record.txt has no $k" ;; esac; done
  [ "$F_signoff" = owner ] || bad "signoff is '$F_signoff', not owner"
  [ "$F_commit" = "$sha" ] || bad "the record is for $F_commit, not $sha"
  [[ "$F_date" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}$ ]] || bad "date '$F_date' is not YYYY-MM-DD"
  printf '%s' "$F_quote" | names "$sha" \
    || bad "the quote does not name ${sha:0:10} (an unambiguous prefix of at least 7 hex digits): '$F_quote'"
  # The cap: the class of the member's diff, and the rounds review.sh recorded on its own commits.
  local cls cap at rounds
  cls="$(diff_class "$sha" "$base")" || bad "no merge base of ${sha:0:10} and ${base:0:10}"
  cap="$(class_cap "$cls")"
  at="$F_rounds_at"
  [[ "$at" =~ ^[0-9a-f]{40}$ ]] || bad "rounds-at '$at' is not a full commit SHA"
  { git merge-base --is-ancestor "$at" "$sha" 2>/dev/null && ! git merge-base --is-ancestor "$at" "$base" 2>/dev/null; } \
    || bad "rounds-at ${at:0:10} is not one of ${sha:0:10}'s own commits (an ancestor or itself, not on ${base:0:10})"
  # verify.sh --fields validates the note schema as review.sh wrote it (0 any well-formed review.sh
  # note, including findings, blocked and provenance-invalid clean notes; 2 no note or a
  # malformed one) and prints its structured fields. This call needs no --required-base: rounds-at
  # counts rounds used, not landing diff coverage.
  local vout vrc=0
  vout="$(bash "$VERIFY" --fields "$at" 2>&1)" || vrc=$?
  [ "$vrc" = 0 ] || bad "rounds-at ${at:0:10} carries no well-formed review.sh note: ${vout//$'\n'/ }"
  rounds="$(printf '%s\n' "$vout" | sed -n 's/^rounds: //p' | head -1)"
  [ -n "$rounds" ] || bad "rounds-at ${at:0:10}: no round count in '$vout'"
  [ "$rounds" -ge "$cap" ] \
    || bad "the review cap was not reached: a $cls change has cap $cap, and ${at:0:10} records $rounds round(s)"
  local r n entry file hash got reviewed verdict verdicts=""
  for r in $REVIEWERS; do
    n="F_request_$r"; [ -n "${!n}" ] || bad "the record has no $r request"
    n="F_verdict_$r"; [ -n "${!n}" ] || bad "the record has no $r verdict"
  done
  local want="100644 blob agy-request.txt 100644 blob agy.txt 100644 blob codex-request.txt 100644 blob codex.txt 100644 blob kimi-request.txt 100644 blob kimi.txt 100644 blob record.txt " have
  have="$(git ls-tree --format='%(objectmode) %(objecttype) %(path)' "$obj" | LC_ALL=C sort | tr '\n' ' ')"
  [ "$have" = "$want" ] || bad "the record tree is not exactly the seven sign-off files as 100644 blobs"
  for r in $REVIEWERS; do
    n="F_request_$r"; entry="${!n}"
    file="${entry%% *}" hash="${entry#* }"
    [ "$file" = "$r-request.txt" ] && [[ "$hash" =~ ^[0-9a-f]{64}$ ]] || bad "request-$r is '$entry', not '$r-request.txt <sha256>'"
    got="$(git cat-file blob "$obj:$file" | sha256)"
    [ "$got" = "$hash" ] || bad "the sha256 of $file is $got, not the recorded $hash"
    reviewed="$(git cat-file blob "$obj:$file" | sed -n '1p')"
    [[ "$reviewed" =~ ^reviewed:\ [0-9a-f]{40}$ ]] || bad "the $r request's first line is not a 'reviewed: <full 40-hex SHA>' line"
    [ "$reviewed" = "reviewed: $sha" ] || bad "the $r request's reviewed line is '${reviewed#reviewed: }', not $sha"
    git cat-file blob "$obj:$file" | tail -n +2 | names "$sha" \
      || bad "the $r request body does not name ${sha:0:10} (an unambiguous prefix of at least 7 hex digits)"
    n="F_verdict_$r"; entry="${!n}"
    file="${entry%% *}" hash="${entry#* }"
    [ "$file" = "$r.txt" ] && [[ "$hash" =~ ^[0-9a-f]{64}$ ]] || bad "verdict-$r is '$entry', not '$r.txt <sha256>'"
    got="$(git cat-file blob "$obj:$file" | sha256)"
    [ "$got" = "$hash" ] || bad "the sha256 of $file is $got, not the recorded $hash"
    local c_out c_status c_reason
    c_out="$(git cat-file blob "$obj:$file" | python3 -I "$OUTPUT_CONTRACT" --commit "$sha" --verdicts ACCEPT,REJECT)" \
      || bad "the $r output could not be parsed by output_contract.py"
    c_status="$(printf '%s\n' "$c_out" | sed -n 's/^status: //p' | head -1)"
    c_reason="$(printf '%s\n' "$c_out" | sed -n 's/^reason: //p' | head -1)"
    case "$c_status" in
      ACCEPT|REJECT) verdicts="$verdicts${verdicts:+, }$r $c_status" ;;
      noverdict|refused) bad "the $r output has no verdict ($c_reason)" ;;
      *) bad "the $r output fails the reviewer-output contract: $c_reason" ;;
    esac
  done
  echo "$sha signed off by the owner ($F_date; $cls change, cap $cap reached at ${at:0:10} with $rounds round(s); verdicts: $verdicts): \"$F_quote\""
}

cmd_verify() {
  local sha base=""
  [ $# -ge 1 ] || die "usage: signoff.sh verify <commit> --base BASE"
  sha="$(git rev-parse -q --verify "$1^{commit}")" || die "verify: not a commit: $1"; shift
  while [ $# -gt 0 ]; do
    case "$1" in --base) [ $# -ge 2 ] || die "verify: --base needs a value"; base="$2"; shift 2 ;; *) die "verify: unknown option $1" ;; esac
  done
  [ -n "$base" ] || die "verify: --base BASE is required (the cap is the class of the diff against it)"
  base="$(git rev-parse -q --verify "$base^{commit}")" || die "verify: --base is not a commit"
  git rev-parse -q --verify "$PREFIX/$sha^{commit}" >/dev/null || { echo "$sha has no owner sign-off ($PREFIX/$sha)"; exit 2; }
  check "$sha" "$(git rev-parse "$PREFIX/$sha^{commit}")" "$base"
}

cmd_collect() {
  [ $# -ge 1 ] || die "collect: name the commit"
  local sha reviewer="" req="" out="" log="" timeout="${REVIEW_TIMEOUT:-1500}"
  sha="$(git rev-parse -q --verify "$1^{commit}")" || die "collect: not a commit: $1"; shift
  while [ $# -gt 0 ]; do
    [ $# -ge 2 ] || die "collect: $1 needs a value"
    case "$1" in
      --reviewer) reviewer="$2" ;;
      --request) req="$2" ;;
      --out) out="$2" ;;
      --log) log="$2" ;;
      --timeout) timeout="$2" ;;
      *) die "collect: unknown option $1" ;;
    esac
    shift 2
  done
  case "$reviewer" in codex|kimi|agy) ;; *) die "collect: --reviewer must be codex, kimi or agy" ;; esac
  [ -f "$req" ] || die "collect: --request FILE is required"
  [ -n "$out" ] || die "collect: --out FILE is required"
  names "$sha" < "$req" || die "collect: request body does not name ${sha:0:10} (an unambiguous prefix of at least 7 hex digits)"
  local work tree rc=0 c_out c_status c_reason
  work="$(mktemp -d "${TMPDIR:-/tmp}/concrete-signoff-collect.XXXXXX")"
  # shellcheck disable=SC2064
  trap "chmod -R u+w '$work' 2>/dev/null || true; rm -rf '$work'" RETURN
  [ -n "$log" ] || log="$work/$reviewer.log"
  tree="$work/tree"
  mkdir -p "$tree"
  git archive "$sha" | tar -x -C "$tree"
  chmod -R a-w "$tree"
  collect_reviewer_output "$reviewer" "$tree" "$req" "$out" "$log" "$timeout" || rc=$?
  c_out="$(python3 -I "$OUTPUT_CONTRACT" --commit "$sha" --verdicts ACCEPT,REJECT < "$out")" || die "collect: output_contract.py failed"
  c_status="$(printf '%s\n' "$c_out" | sed -n 's/^status: //p' | head -1)"
  c_reason="$(printf '%s\n' "$c_out" | sed -n 's/^reason: //p' | head -1)"
  if [ "$rc" -ne 0 ] || { [ "$c_status" != ACCEPT ] && [ "$c_status" != REJECT ]; }; then
    echo "signoff: collect ($reviewer on ${sha:0:10}) failed: status=$c_status ($c_reason, exit $rc)" >&2
    return 1
  fi
  echo "signoff: collected $reviewer final message for ${sha:0:10}: VERDICT: $c_status -> $out"
}

case "${1:-}" in
  collect) shift; cmd_collect "$@" ;;
  record) shift; cmd_record "$@" ;;
  verify) shift; cmd_verify "$@" ;;
  -h|--help) usage ;;
  *) usage >&2; exit 2 ;;
esac
