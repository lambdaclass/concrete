#!/usr/bin/env bash
# OWNER SIGN-OFF RECORDS (ROADMAP R-0618; owner, 2026-11-08). A sign-off is review provenance for one
# exact commit. Owner item 1 adds writer substitution; the manual-rounds rule adds review evidence.
#
#   signoff.sh record <commit> --base BASE --rounds-at NOTED-OR-EVIDENCE --date YYYY-MM-DD
#                     --quote TEXT [--writer MODEL --writer-evidence EVIDENCE]
#                     --codex-request FILE --codex FILE --kimi-request FILE --kimi FILE
#                     --agy-request FILE --agy FILE
#   signoff.sh verify <commit> (--base BASE | --structural) [--record RECORD]
#   signoff.sh evidence build <commit> (--round N --reviewer MODEL --request FILE --output FILE)...
#   signoff.sh evidence verify <commit> [--record RECORD]
#
# THE RECORD is refs/owner-signoff/<full sha>, a parentless commit holding record.txt and, for each
# required reviewer r, r-request.txt and r.txt, all 100644 blobs and no other files. Requests and
# outputs are copied byte for byte. Each request must itself begin `reviewed: <full sha>`; record
# never adds that line. record.txt is `key: value` lines, each key once:
#   signoff: owner
#   commit: <the 40-hex SHA approved>
#   date: YYYY-MM-DD
#   quote: <the owner's words, verbatim; an unambiguous object prefix of 7+ hex digits names commit>
#   rounds-at: <a review.sh-noted commit, or the review-evidence commit for this sign-off>
#   request-codex: codex-request.txt <sha256>
#   verdict-codex: codex.txt <sha256>
#   request-kimi: kimi-request.txt <sha256>
#   verdict-kimi: kimi.txt <sha256>
#   request-agy: agy-request.txt <sha256>
#   verdict-agy: agy.txt <sha256>
# New records with writer provenance also have `writer: codex|kimi|agy|claude`,
# `writer-evidence: author:<author>|committer:<committer>|trailer:<trailer>` (matched against the
# signed commit), and `absent-<writer>: writer`. A writer in codex, kimi or agy is never a reviewer;
# the other two are required. A claude writer leaves all three required. Each required output must
# end in ACCEPT, contain `REPOSITORY: readable`, no `REPOSITORY: unreadable`, and at least one
# `READ: <file>` line. Records without writer keys are the original seven-file format and retain the
# original three-reviewer ACCEPT-or-REJECT rule without repository-access lines.
#
# `record` builds and verifies the record before writing the ref locally. It pushes nothing.
# `--structural` checks everything except the base-dependent diff class, cap and rounds-at ancestry
# exclusion; the pre-push hook uses it with --record to judge the pushed object, not a local ref.
#
# REVIEW EVIDENCE is refs/review-evidence/<full reviewed sha>, a parentless commit for reviews run
# outside review.sh. For every round N and reviewer r its flat tree holds exactly:
#   round-N-r-request.txt       the exact request, with the same reviewed: binding as above
#   round-N-r.txt               the raw output, ACCEPT or REJECT and full-repository evidence
#   round-N-r-manifest.txt      reviewed, round, reviewer, repository: readable, and both sha256s
# Rounds are contiguous from 1 and have one unchanged reviewer set. `evidence build` constructs and
# verifies a complete commit locally; `evidence verify` checks one. Neither pushes. For a sign-off,
# its reviewer set must equal the required set and refs/review-evidence/<signed sha> must point at
# rounds-at. Its round count replaces the review.sh note's count. A diff-only or failed review does
# not count. Publish review evidence before a sign-off that uses it.
#
# THE CAP comes from the member's diff against BASE: docs and roadmap 2, scripts 3, compiler and spec
# 5; unclassified paths are compiler and spec. A review.sh rounds-at must be the signed commit or its
# ancestor not on BASE and carry a well-formed note (clean, findings or blocked) whose round reaches
# the cap. Review evidence instead binds its reviewed SHA in every manifest and through its ref.
#
# Every request body must name the signed commit by an unambiguous object prefix of 7+ hex digits.
# A body line that says `Review exact commit <other prefix>` contradicts the binding and is refused.
# Prefixes resolve only through `git rev-parse --disambiguate`, never through refs. Each output's
# last VERDICT: line of any value is selected first, then required to be ACCEPT or REJECT. Request
# lines never count as verdicts. The sign-off judges the exact commit and no other.
#
# Exit codes: 0 valid; 1 present but not qualifying (reason printed); 2 absent or usage error.
set -euo pipefail
PREFIX=refs/owner-signoff
EVIDENCE_PREFIX=refs/review-evidence
REVIEWERS="codex kimi agy"
VERIFY="$(cd "$(dirname "${BASH_SOURCE[0]}")/../review" && pwd)/verify.sh"

usage() { awk 'NR > 1 { if (!/^#/) exit; sub(/^# ?/, ""); print }' "${BASH_SOURCE[0]}"; }
die() { echo "signoff: $*" >&2; exit 2; }
sha256() { if command -v sha256sum >/dev/null 2>&1; then sha256sum | cut -d' ' -f1; else shasum -a 256 | cut -d' ' -f1; fi; }

path_class() {
  case "$1" in
    docs/*|changelog.d/*|ROADMAP.md|CHANGELOG.md|README.md|AGENTS.md|CLAUDE.md) echo docs ;;
    scripts/*|.githooks/*|.github/*|.claude/*|required-checks.txt|required-checks-transition.tsv) echo scripts ;;
    *) echo compiler ;;
  esac
}
# names SHA: succeed only when a 7+ hex word is an unambiguous object name for exactly SHA.
names() {
  local w matches
  for w in $(tr -c '0-9a-f' ' '); do
    [ "${#w}" -ge 7 ] && [ "${#w}" -le 40 ] && [ "${1#"$w"}" != "$1" ] || continue
    matches="$(git rev-parse --disambiguate="$w" 2>/dev/null || true)"
    [ "$matches" = "$1" ] && [ "$(git cat-file -t "$1" 2>/dev/null)" = commit ] && return 0
  done
  return 1
}
class_cap() { case "$1" in docs) echo 2 ;; scripts) echo 3 ;; *) echo 5 ;; esac; }
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
required_reviewers() {
  local writer="$1" r
  for r in $REVIEWERS; do [ "$r" = "$writer" ] || printf '%s ' "$r"; done
}
last_verdict() {
  awk '/^[[:space:]]*VERDICT:/ { v=$0 } END { if (v != "") { sub(/^[[:space:]]*VERDICT:[[:space:]]*/, "", v); sub(/[[:space:]]*$/, "", v); print v } }'
}
tree_exact() { # tree_exact OBJECT newline-separated-sorted-files
  local obj="$1" files="$2" f want="" have
  while IFS= read -r f; do [ -n "$f" ] && want="${want}${want:+$'\n'}100644 blob $f"; done <<< "$files"
  have="$(git ls-tree --format='%(objectmode) %(objecttype) %(path)' "$obj" | LC_ALL=C sort)"
  [ "$have" = "$want" ]
}
request_check() { # request_check SHA OBJECT FILE LABEL; FAILURE_HANDLER is bad or ebad in the caller
  local sha="$1" obj="$2" file="$3" label="$4" text body reviewed targets t
  text="$(git cat-file blob "$obj:$file")"
  reviewed="${text%%$'\n'*}"
  [[ "$reviewed" =~ ^reviewed:\ [0-9a-f]{40}$ ]] || "$FAILURE_HANDLER" "the $label request's first line is not a 'reviewed: <full 40-hex SHA>' line"
  [ "$reviewed" = "reviewed: $sha" ] || "$FAILURE_HANDLER" "the $label request's reviewed line is '${reviewed#reviewed: }', not $sha"
  body="${text#*$'\n'}"
  names "$sha" <<< "$body" || "$FAILURE_HANDLER" "the $label request body does not name ${sha:0:10} (an unambiguous object prefix of at least 7 hex digits)"
  targets="$(printf '%s\n' "$body" | sed -n -E 's/.*[Rr]eview exact commit ([0-9a-f]{7,40})([^0-9a-f].*)?$/\1/p')"
  for t in $targets; do
    case "$sha" in "$t"*) ;; *) "$FAILURE_HANDLER" "the $label request names $t, not ${sha:0:10}, as the exact commit under review" ;; esac
  done
}
output_access_check() { # output_access_check TEXT LABEL; FAILURE_HANDLER is bad or ebad in the caller
  local text="$1" label="$2"
  grep -qE '^[[:space:]]*REPOSITORY: readable[[:space:]]*$' <<< "$text" || "$FAILURE_HANDLER" "the $label output has no REPOSITORY: readable line"
  ! grep -qE '^[[:space:]]*REPOSITORY: unreadable[[:space:]]*$' <<< "$text" || "$FAILURE_HANDLER" "the $label output says REPOSITORY: unreadable"
  grep -qE '^[[:space:]]*READ:[[:space:]]*[^[:space:]]' <<< "$text" || "$FAILURE_HANDLER" "the $label output names no file it read (READ: <file>)"
}
writer_evidence_check() { # writer_evidence_check SHA EVIDENCE
  local sha="$1" evidence="$2" want actual
  case "$evidence" in
    author:*) want="${evidence#author:}"; actual="$(git show -s --format='%an <%ae>' "$sha")" ;;
    committer:*) want="${evidence#committer:}"; actual="$(git show -s --format='%cn <%ce>' "$sha")" ;;
    trailer:*) want="${evidence#trailer:}"; actual="$(git show -s --format='%(trailers)' "$sha")"
      grep -Fxq -- "$want" <<< "$actual" || bad "writer-evidence trailer '$want' is not on $sha"
      return 0 ;;
    *) bad "writer-evidence must start author:, committer: or trailer:" ;;
  esac
  [ -n "$want" ] && [ "$actual" = "$want" ] || bad "writer-evidence '$evidence' does not match $sha"
}

cmd_record() {
  [ $# -ge 1 ] || die "record: name the commit"
  local sha date="" quote="" base="" at="" r f q v writer="" writer_evidence="" required
  sha="$(git rev-parse -q --verify "$1^{commit}")" || die "record: not a commit: $1"; shift
  local codex="" kimi="" agy="" codex_request="" kimi_request="" agy_request=""
  while [ $# -gt 0 ]; do
    [ $# -ge 2 ] || die "record: $1 needs a value"
    case "$1" in
      --date) date="$2" ;; --quote) quote="$2" ;; --base) base="$2" ;; --rounds-at) at="$2" ;;
      --writer) writer="$2" ;; --writer-evidence) writer_evidence="$2" ;;
      --codex) codex="$2" ;; --kimi) kimi="$2" ;; --agy) agy="$2" ;;
      --codex-request) codex_request="$2" ;; --kimi-request) kimi_request="$2" ;; --agy-request) agy_request="$2" ;;
      *) die "record: unknown option $1" ;;
    esac; shift 2
  done
  [[ "$date" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}$ ]] || die "record: --date YYYY-MM-DD is required"
  [ -n "$quote" ] && [[ "$quote" != *$'\n'* ]] || die "record: --quote is required, on one line"
  base="$(git rev-parse -q --verify "$base^{commit}")" || die "record: --base is not a commit"
  at="$(git rev-parse -q --verify "$at^{commit}")" || die "record: --rounds-at must name a commit"
  if [ -n "$writer" ] || [ -n "$writer_evidence" ]; then
    case "$writer" in codex|kimi|agy|claude) ;; *) die "record: --writer and --writer-evidence are required together; writer is codex, kimi, agy or claude" ;; esac
    [ -n "$writer_evidence" ] || die "record: --writer-evidence is required with --writer"
  fi
  required="$(required_reviewers "$writer")"
  for r in $REVIEWERS; do
    q="${r}_request"; f="$r"
    case " $required " in *" $r "*) ;; *) [ -z "${!q}" ] && [ -z "${!f}" ] || die "record: writer $writer can never be a reviewer ($r)" ;; esac
  done
  local rec="signoff: owner
commit: $sha
date: $date
quote: $quote
rounds-at: $at" entries=""
  if [ -n "$writer" ]; then rec="$rec
writer: $writer
writer-evidence: $writer_evidence
absent-$writer: writer"; fi
  for r in $required; do
    q="${r}_request"; q="${!q}"; [ -f "$q" ] || die "record: --$r-request FILE is required (the verbatim request, including its reviewed: first line)"
    f="${!r}"; [ -f "$f" ] || die "record: --$r FILE is required (the reviewer's verdict output)"
    v="$(git hash-object -w --no-filters -- "$q")"
    rec="$rec
request-$r: $r-request.txt $(sha256 < "$q")"
    entries="${entries}100644 blob $v	$r-request.txt
"
    v="$(git hash-object -w --no-filters -- "$f")"
    rec="$rec
verdict-$r: $r.txt $(sha256 < "$f")"
    entries="${entries}100644 blob $v	$r.txt
"
  done
  v="$(printf '%s\n' "$rec" | git hash-object -w --no-filters --stdin)"
  local tree commit out
  tree="$(printf '%s100644 blob %s\trecord.txt\n' "$entries" "$v" | git mktree)"
  commit="$(git commit-tree "$tree" -m "owner sign-off of $sha ($date)")"
  out="$(check "$sha" "$commit" "$base")" || { printf '%s\nsignoff: not recorded (nothing written)\n' "$out" >&2; exit 1; }
  git update-ref "$PREFIX/$sha" "$commit"
  echo "$out"
  echo "signoff: recorded $PREFIX/$sha -> ${commit:0:10}; push it to both remotes"
}

# evidence_check SHA OBJECT REQUIRED-REVIEWERS REQUIRE-REF; sets EVIDENCE_ROUNDS.
evidence_check() {
  local sha="$1" obj="$2" required="$3" require_ref="${4:-1}" FAILURE_HANDLER=ebad
  ebad() { echo "$sha review evidence does not qualify: $*"; exit 1; }
  [ "$(git cat-file -t "$obj" 2>/dev/null)" = commit ] || ebad "the evidence object is not a commit"
  [ "$(git rev-list --parents -n 1 "$obj")" = "$obj" ] || ebad "the evidence commit is not parentless"
  if [ "$require_ref" = 1 ]; then
    [ "$(git rev-parse -q --verify "$EVIDENCE_PREFIX/$sha^{commit}" 2>/dev/null)" = "$obj" ] \
      || ebad "$EVIDENCE_PREFIX/$sha does not point at $obj"
  fi
  local manifests file n r line k v seen=" " files="" max=0
  manifests="$(git ls-tree --name-only "$obj" | LC_ALL=C sort)"
  [ -n "$manifests" ] || ebad "the tree is empty"
  while IFS= read -r file; do
    [[ "$file" =~ ^round-([1-9][0-9]*)-(codex|kimi|agy)-manifest\.txt$ ]] || continue
    n="${BASH_REMATCH[1]}" r="${BASH_REMATCH[2]}"
    case "$seen" in *" $n:$r "*) ebad "round $n repeats $r" ;; esac
    seen="$seen$n:$r "
    [ "$n" -le "$max" ] || max="$n"
    local F_reviewed="" F_round="" F_reviewer="" F_repository="" F_request="" F_output="" mseen=" "
    while IFS= read -r line; do
      [[ "$line" =~ ^([a-z-]+):\ (.*)$ ]] || ebad "$file line is not key: value: '$line'"
      k="${BASH_REMATCH[1]}" v="${BASH_REMATCH[2]}"
      case "$mseen" in *" $k "*) ebad "$file repeats $k" ;; esac
      mseen="$mseen$k "
      case "$k" in reviewed|round|reviewer|repository|request|output) ;; *) ebad "$file has unknown key $k" ;; esac
      printf -v "F_${k}" '%s' "$v"
    done <<< "$(git cat-file blob "$obj:$file")"
    for k in reviewed round reviewer repository request output; do case "$mseen" in *" $k "*) ;; *) ebad "$file has no $k" ;; esac; done
    [ "$F_reviewed" = "$sha" ] || ebad "$file reviews $F_reviewed, not $sha"
    [ "$F_round" = "$n" ] && [ "$F_reviewer" = "$r" ] || ebad "$file's round or reviewer does not match its name"
    [ "$F_repository" = readable ] || ebad "$file does not say repository: readable"
    local q="round-$n-$r-request.txt" o="round-$n-$r.txt" entry hash got text verdict
    entry="$F_request"; [ "${entry%% *}" = "$q" ] && [[ "${entry#* }" =~ ^[0-9a-f]{64}$ ]] || ebad "$file request is not '$q <sha256>'"
    hash="${entry#* }"; got="$(git cat-file blob "$obj:$q" | sha256)"; [ "$got" = "$hash" ] || ebad "the sha256 of $q is $got, not $hash"
    request_check "$sha" "$obj" "$q" "round-$n-$r"
    entry="$F_output"; [ "${entry%% *}" = "$o" ] && [[ "${entry#* }" =~ ^[0-9a-f]{64}$ ]] || ebad "$file output is not '$o <sha256>'"
    hash="${entry#* }"; got="$(git cat-file blob "$obj:$o" | sha256)"; [ "$got" = "$hash" ] || ebad "the sha256 of $o is $got, not $hash"
    text="$(git cat-file blob "$obj:$o")"; verdict="$(last_verdict <<< "$text")"
    case "$verdict" in ACCEPT|REJECT) ;; "") ebad "the round-$n-$r output has no verdict" ;; *) ebad "the round-$n-$r output's last verdict is '$verdict', not ACCEPT or REJECT" ;; esac
    output_access_check "$text" "round-$n-$r"
    files="${files}${files:+$'\n'}$file"$'\n'"$q"$'\n'"$o"
  done <<< "$manifests"
  [ "$max" -gt 0 ] || ebad "no round-N-reviewer-manifest.txt files"
  tree_exact "$obj" "$(printf '%s\n' "$files" | LC_ALL=C sort)" || ebad "the tree is not exactly each round and reviewer's request, output and manifest as 100644 blobs"
  local i round_set="" first_set="" canonical=""
  for i in $(seq 1 "$max"); do
    round_set=""
    for r in $REVIEWERS; do case "$seen" in *" $i:$r "*) round_set="$round_set${round_set:+ }$r" ;; esac; done
    [ -n "$round_set" ] || ebad "round $i is missing (rounds must be contiguous from 1)"
    [ -z "$first_set" ] || [ "$round_set" = "$first_set" ] || ebad "the reviewer set changes between rounds"
    first_set="$round_set"
  done
  if [ -n "$required" ]; then
    for r in $required; do canonical="$canonical${canonical:+ }$r"; done
    [ "$first_set" = "$canonical" ] || ebad "reviewers are '$first_set', not the required '$canonical'"
  fi
  EVIDENCE_ROUNDS="$max"
  echo "$sha review evidence: $max round(s), reviewers: $first_set"
}

cmd_evidence() {
  local action="${1:-}"; shift || true
  case "$action" in
    build)
      [ $# -ge 1 ] || die "evidence build: name the reviewed commit"
      local sha; sha="$(git rev-parse -q --verify "$1^{commit}")" || die "evidence build: not a commit: $1"; shift
      local -a ns rs qs os
      local i=-1 n r q o
      while [ $# -gt 0 ]; do
        [ $# -ge 2 ] || die "evidence build: $1 needs a value"
        case "$1" in
          --round) i=$((i+1)); ns[i]="$2"; rs[i]=""; qs[i]=""; os[i]="" ;;
          --reviewer) [ "$i" -ge 0 ] && [ -z "${rs[i]}" ] || die "evidence build: --reviewer follows --round, once"; rs[i]="$2" ;;
          --request) [ "$i" -ge 0 ] && [ -z "${qs[i]}" ] || die "evidence build: --request follows --round, once"; qs[i]="$2" ;;
          --output) [ "$i" -ge 0 ] && [ -z "${os[i]}" ] || die "evidence build: --output follows --round, once"; os[i]="$2" ;;
          *) die "evidence build: unknown option $1" ;;
        esac; shift 2
      done
      [ "$i" -ge 0 ] || die "evidence build: at least one --round group is required"
      local entries="" seen=" " j
      for j in $(seq 0 "$i"); do
        n="${ns[j]}"; r="${rs[j]}"; q="${qs[j]}"; o="${os[j]}"
        [[ "$n" =~ ^[1-9][0-9]*$ ]] || die "evidence build: round '$n' is not a positive integer"
        case "$r" in codex|kimi|agy) ;; *) die "evidence build: reviewer is codex, kimi or agy" ;; esac
        case "$seen" in *" $n:$r "*) die "evidence build: duplicate round $n reviewer $r" ;; esac; seen="$seen$n:$r "
        [ -f "$q" ] && [ -f "$o" ] || die "evidence build: request and output files are required for round $n reviewer $r"
        local qf="round-$n-$r-request.txt" of="round-$n-$r.txt" mf="round-$n-$r-manifest.txt" qb ob mb manifest
        qb="$(git hash-object -w --no-filters -- "$q")"; ob="$(git hash-object -w --no-filters -- "$o")"
        manifest="reviewed: $sha
round: $n
reviewer: $r
repository: readable
request: $qf $(sha256 < "$q")
output: $of $(sha256 < "$o")"
        mb="$(printf '%s\n' "$manifest" | git hash-object -w --no-filters --stdin)"
        entries="${entries}100644 blob $qb	$qf
100644 blob $ob	$of
100644 blob $mb	$mf
"
      done
      local tree commit; tree="$(printf '%s' "$entries" | git mktree)"; commit="$(git commit-tree "$tree" -m "review evidence of $sha")"
      evidence_check "$sha" "$commit" "" 0
      git update-ref "$EVIDENCE_PREFIX/$sha" "$commit"
      echo "signoff: recorded $EVIDENCE_PREFIX/$sha -> ${commit:0:10}; push nothing from this command" ;;
    verify)
      [ $# -ge 1 ] || die "evidence verify: name the reviewed commit"
      local sha record=""; sha="$(git rev-parse -q --verify "$1^{commit}")" || die "evidence verify: not a commit: $1"; shift
      while [ $# -gt 0 ]; do
        case "$1" in --record) [ $# -ge 2 ] || die "evidence verify: --record needs a value"; record="$2"; shift 2 ;; *) die "evidence verify: unknown option $1" ;; esac
      done
      if [ -z "$record" ]; then
        record="$(git rev-parse -q --verify "$EVIDENCE_PREFIX/$sha^{commit}" 2>/dev/null)" || { echo "$sha has no review evidence ($EVIDENCE_PREFIX/$sha)"; exit 2; }
      fi
      evidence_check "$sha" "$record" "" 1 ;;
    *) die "usage: signoff.sh evidence build|verify ..." ;;
  esac
}

# check SHA RECORD BASE: BASE empty means structural mode (all but the base-dependent cap checks).
check() {
  local sha="$1" obj="$2" base="$3" FAILURE_HANDLER=bad
  bad() { echo "$sha owner sign-off does not qualify: $*"; exit 1; }
  [ "$(git cat-file -t "$obj" 2>/dev/null)" = commit ] || bad "the record object is not a commit"
  [ "$(git rev-list --parents -n 1 "$obj")" = "$obj" ] || bad "the record commit is not parentless"
  [ "$(git cat-file -t "$obj:record.txt" 2>/dev/null)" = blob ] || bad "the record holds no record.txt"
  local rec line k v seen=" " F_signoff="" F_commit="" F_date="" F_quote="" F_rounds_at=""
  local F_writer="" F_writer_evidence="" F_absent_codex="" F_absent_kimi="" F_absent_agy="" F_absent_claude=""
  local F_request_codex="" F_verdict_codex="" F_request_kimi="" F_verdict_kimi="" F_request_agy="" F_verdict_agy=""
  rec="$(git cat-file blob "$obj:record.txt")"
  while IFS= read -r line; do
    [[ "$line" =~ ^([a-z-]+):\ (.*)$ ]] || bad "record.txt line is not key: value: '$line'"
    k="${BASH_REMATCH[1]}" v="${BASH_REMATCH[2]}"
    case "$seen" in *" $k "*) bad "record.txt repeats $k" ;; esac
    seen="$seen$k "
    case "$k" in
      signoff|commit|date|quote|rounds-at|request-codex|verdict-codex|request-kimi|verdict-kimi|request-agy|verdict-agy|writer|writer-evidence|absent-codex|absent-kimi|absent-agy|absent-claude) ;;
      *) bad "record.txt has the unknown key $k" ;;
    esac
    printf -v "F_${k//-/_}" '%s' "$v"
  done <<< "$rec"
  for k in signoff commit date quote rounds-at; do case "$seen" in *" $k "*) ;; *) bad "record.txt has no $k" ;; esac; done
  [ "$F_signoff" = owner ] || bad "signoff is '$F_signoff', not owner"
  [ "$F_commit" = "$sha" ] || bad "the record is for $F_commit, not $sha"
  [[ "$F_date" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}$ ]] || bad "date '$F_date' is not YYYY-MM-DD"
  printf '%s' "$F_quote" | names "$sha" || bad "the quote does not name ${sha:0:10} (an unambiguous object prefix of at least 7 hex digits): '$F_quote'"
  local required absent
  if [ -n "$F_writer" ]; then
    case "$F_writer" in codex|kimi|agy|claude) ;; *) bad "writer '$F_writer' is not codex, kimi, agy or claude" ;; esac
    case "$seen" in *" writer-evidence "*) ;; *) bad "writer provenance has no writer-evidence" ;; esac
    absent="F_absent_$F_writer"; [ "${!absent}" = writer ] || bad "writer provenance has no absent-$F_writer: writer"
    for k in codex kimi agy claude; do
      [ "$k" = "$F_writer" ] && continue
      absent="F_absent_$k"; [ -z "${!absent}" ] || bad "unexpected absent-$k key"
    done
    writer_evidence_check "$sha" "$F_writer_evidence"
  else
    [ -z "$F_writer_evidence" ] && [ -z "$F_absent_codex" ] && [ -z "$F_absent_kimi" ] && [ -z "$F_absent_agy" ] && [ -z "$F_absent_claude" ] \
      || bad "writer-evidence and absent keys require writer"
  fi
  required="$(required_reviewers "$F_writer")"
  local cls=structural cap=not-checked at rounds vout vrc=0 evidence
  at="$F_rounds_at"
  [[ "$at" =~ ^[0-9a-f]{40}$ ]] || bad "rounds-at '$at' is not a full commit SHA"
  [ "$(git cat-file -t "$at" 2>/dev/null)" = commit ] || bad "rounds-at $at is not a commit"
  evidence="$(git rev-parse -q --verify "$EVIDENCE_PREFIX/$sha^{commit}" 2>/dev/null || true)"
  if [ "$evidence" = "$at" ]; then
    evidence_check "$sha" "$at" "$required" 1
    rounds="$EVIDENCE_ROUNDS"
  else
    git merge-base --is-ancestor "$at" "$sha" 2>/dev/null || bad "rounds-at ${at:0:10} is not ${sha:0:10} or its ancestor"
    if [ -n "$base" ]; then
      ! git merge-base --is-ancestor "$at" "$base" 2>/dev/null || bad "rounds-at ${at:0:10} is already on ${base:0:10}"
    fi
    vout="$(bash "$VERIFY" "$at" 2>&1)" || vrc=$?
    [ "$vrc" = 0 ] || [ "$vrc" = 1 ] || bad "rounds-at ${at:0:10} carries no well-formed review.sh note: ${vout//$'\n'/ }"
    rounds="$(printf '%s\n' "$vout" | sed -n 's/.*(round \([1-9][0-9]*\), reviewer .*/\1/p' | head -1)"
    [ -n "$rounds" ] || bad "rounds-at ${at:0:10}: no round count in '$vout'"
  fi
  if [ -n "$base" ]; then
    cls="$(diff_class "$sha" "$base")" || bad "no merge base of ${sha:0:10} and ${base:0:10}"
    cap="$(class_cap "$cls")"
    [ "$rounds" -ge "$cap" ] || bad "the review cap was not reached: a $cls change has cap $cap, and ${at:0:10} records $rounds round(s)"
  fi
  local r n files="record.txt"
  for r in $required; do
    n="F_request_$r"; [ -n "${!n}" ] || bad "the record has no $r request"
    n="F_verdict_$r"; [ -n "${!n}" ] || bad "the record has no $r verdict"
    files="$files"$'\n'"$r-request.txt"$'\n'"$r.txt"
  done
  tree_exact "$obj" "$(printf '%s\n' "$files" | LC_ALL=C sort)" || bad "the record tree is not exactly record.txt and every required reviewer's request and output as 100644 blobs"
  local entry file hash got text verdict verdicts=""
  for r in $required; do
    n="F_request_$r"; entry="${!n}"; file="${entry%% *}"; hash="${entry#* }"
    [ "$file" = "$r-request.txt" ] && [[ "$hash" =~ ^[0-9a-f]{64}$ ]] || bad "request-$r is '$entry', not '$r-request.txt <sha256>'"
    got="$(git cat-file blob "$obj:$file" | sha256)"; [ "$got" = "$hash" ] || bad "the sha256 of $file is $got, not the recorded $hash"
    request_check "$sha" "$obj" "$file" "$r"
    n="F_verdict_$r"; entry="${!n}"; file="${entry%% *}"; hash="${entry#* }"
    [ "$file" = "$r.txt" ] && [[ "$hash" =~ ^[0-9a-f]{64}$ ]] || bad "verdict-$r is '$entry', not '$r.txt <sha256>'"
    got="$(git cat-file blob "$obj:$file" | sha256)"; [ "$got" = "$hash" ] || bad "the sha256 of $file is $got, not the recorded $hash"
    text="$(git cat-file blob "$obj:$file")"; verdict="$(last_verdict <<< "$text")"
    case "$verdict" in ACCEPT|REJECT) ;; "") bad "the $r output has no verdict" ;; *) bad "the $r output's last verdict is '$verdict', not ACCEPT or REJECT" ;; esac
    if [ -n "$F_writer" ]; then
      [ "$verdict" = ACCEPT ] || bad "the $r output's last verdict is '$verdict'; writer-substituted records require ACCEPT"
      output_access_check "$text" "$r"
    fi
    verdicts="$verdicts${verdicts:+, }$r $verdict"
  done
  if [ -n "$base" ]; then
    echo "$sha signed off by the owner ($F_date; $cls change, cap $cap reached at ${at:0:10} with $rounds round(s); verdicts: $verdicts): \"$F_quote\""
  else
    echo "$sha owner sign-off structural checks passed ($rounds round(s) at ${at:0:10}; verdicts: $verdicts)"
  fi
}

cmd_verify() {
  local sha base="" record="" structural=0
  [ $# -ge 1 ] || die "usage: signoff.sh verify <commit> (--base BASE | --structural) [--record RECORD]"
  sha="$(git rev-parse -q --verify "$1^{commit}")" || die "verify: not a commit: $1"; shift
  while [ $# -gt 0 ]; do
    case "$1" in
      --base) [ $# -ge 2 ] || die "verify: --base needs a value"; base="$2"; shift 2 ;;
      --record) [ $# -ge 2 ] || die "verify: --record needs a value"; record="$2"; shift 2 ;;
      --structural) structural=1; shift ;;
      *) die "verify: unknown option $1" ;;
    esac
  done
  if [ "$structural" = 1 ]; then [ -z "$base" ] || die "verify: --structural and --base are exclusive"; else
    [ -n "$base" ] || die "verify: --base BASE is required (or choose --structural)"
    base="$(git rev-parse -q --verify "$base^{commit}")" || die "verify: --base is not a commit"
  fi
  if [ -z "$record" ]; then
    record="$(git rev-parse -q --verify "$PREFIX/$sha^{commit}" 2>/dev/null)" || { echo "$sha has no owner sign-off ($PREFIX/$sha)"; exit 2; }
  fi
  check "$sha" "$record" "$base"
}

case "${1:-}" in
  record) shift; cmd_record "$@" ;;
  verify) shift; cmd_verify "$@" ;;
  evidence) shift; cmd_evidence "$@" ;;
  -h|--help) usage ;;
  *) usage >&2; exit 2 ;;
esac
