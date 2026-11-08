#!/usr/bin/env bash
# OWNER SIGN-OFF RECORDS (ROADMAP R-0618; owner, 2026-11-08: "when a branch reaches its review-round
# cap, the owner may approve one exact commit SHA after Codex, Kimi and agy have all reviewed it; the
# approval is recorded"). A sign-off is review provenance for that one commit, as a clean
# refs/notes/concrete-review note is (scripts/review/verify.sh); train.sh accepts either for a member.
#
#   signoff.sh record <commit> --date YYYY-MM-DD --quote TEXT --codex FILE --kimi FILE --agy FILE
#   signoff.sh verify <commit>
#
# THE RECORD is the ref refs/owner-signoff/<full sha>, a commit whose tree holds record.txt and the
# three reviewers' verdict outputs, copied in byte for byte (codex.txt, kimi.txt, agy.txt), so any
# clone that fetches the ref can check them. record.txt is `key: value` lines, each key once:
#   signoff: owner
#   commit: <the 40-hex SHA approved>
#   date: YYYY-MM-DD
#   quote: <the owner's words, verbatim; they name the commit by a prefix of at least 7 hex digits>
#   verdict-codex: codex.txt <sha256>
#   verdict-kimi: kimi.txt <sha256>
#   verdict-agy: agy.txt <sha256>
# `record` writes the ref locally and pushes nothing (push it to both remotes: the pre-push hook takes
# refs/owner-signoff/* in its no-source-tree tier). Only the owner's recorded words go in a quote.
#
# `verify` accepts the record only if the ref exists, the tree holds exactly those four files, every
# key is present once and well formed, `commit` is the commit asked about, the quote names it, each
# file's sha256 is the recorded one, and each output's last VERDICT line is `VERDICT: ACCEPT`. It
# judges the exact commit: a sign-off says nothing about any other commit.
#
# Exit codes: 0 a valid sign-off; 1 a sign-off that does not qualify (the reason is printed);
#             2 no sign-off, or a usage error.
set -euo pipefail
PREFIX=refs/owner-signoff
REVIEWERS="codex kimi agy"

usage() { awk 'NR > 1 { if (!/^#/) exit; sub(/^# ?/, ""); print }' "${BASH_SOURCE[0]}"; }
die() { echo "signoff: $*" >&2; exit 2; }
sha256() { if command -v sha256sum >/dev/null 2>&1; then sha256sum | cut -d' ' -f1; else shasum -a 256 | cut -d' ' -f1; fi; }

cmd_record() {
  [ $# -ge 1 ] || die "record: name the commit"
  local sha date="" quote="" r f v; sha="$(git rev-parse -q --verify "$1^{commit}")" || die "record: not a commit: $1"; shift
  local codex="" kimi="" agy=""
  while [ $# -gt 0 ]; do
    [ $# -ge 2 ] || die "record: $1 needs a value"
    case "$1" in
      --date) date="$2" ;; --quote) quote="$2" ;;
      --codex) codex="$2" ;; --kimi) kimi="$2" ;; --agy) agy="$2" ;;
      *) die "record: unknown option $1" ;;
    esac; shift 2
  done
  [[ "$date" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}$ ]] || die "record: --date YYYY-MM-DD is required"
  [ -n "$quote" ] && [[ "$quote" != *$'\n'* ]] || die "record: --quote is required, on one line"
  local rec="signoff: owner
commit: $sha
date: $date
quote: $quote" entries=""
  for r in $REVIEWERS; do
    f="${!r}"; [ -f "$f" ] || die "record: --$r FILE is required (the reviewer's verdict output)"
    v="$(git hash-object -w -- "$f")"
    rec="$rec
verdict-$r: $r.txt $(sha256 < "$f")"
    entries="${entries}100644 blob $v	$r.txt
"
  done
  v="$(printf '%s\n' "$rec" | git hash-object -w --stdin)"
  local tree commit
  tree="$(printf '%s100644 blob %s\trecord.txt\n' "$entries" "$v" | git mktree)"
  commit="$(git commit-tree "$tree" -m "owner sign-off of $sha ($date)")"
  git update-ref "$PREFIX/$sha" "$commit"
  echo "signoff: recorded $PREFIX/$sha -> ${commit:0:10}; verify, then push it to both remotes"
  cmd_verify "$sha"
}

cmd_verify() {
  [ $# -eq 1 ] || die "usage: signoff.sh verify <commit>"
  local sha ref; sha="$(git rev-parse -q --verify "$1^{commit}")" || die "verify: not a commit: $1"
  ref="$PREFIX/$sha"
  git rev-parse -q --verify "$ref^{commit}" >/dev/null || { echo "$sha has no owner sign-off ($ref)"; exit 2; }
  bad() { echo "$sha owner sign-off does not qualify: $*"; exit 1; }
  local rec line k v seen=" " F_signoff="" F_commit="" F_date="" F_quote="" F_verdict_codex="" F_verdict_kimi="" F_verdict_agy=""
  rec="$(git cat-file blob "$ref:record.txt")"
  while IFS= read -r line; do
    [[ "$line" =~ ^([a-z-]+):\ (.*)$ ]] || bad "record.txt line is not key: value: '$line'"
    k="${BASH_REMATCH[1]}" v="${BASH_REMATCH[2]}"
    case "$seen" in *" $k "*) bad "record.txt repeats $k" ;; esac
    seen="$seen$k "
    case "$k" in signoff|commit|date|quote|verdict-codex|verdict-kimi|verdict-agy) ;; *) bad "record.txt has the unknown key $k" ;; esac
    printf -v "F_${k//-/_}" '%s' "$v"
  done <<< "$rec"
  for k in signoff commit date quote; do case "$seen" in *" $k "*) ;; *) bad "record.txt has no $k" ;; esac; done
  [ "$F_signoff" = owner ] || bad "signoff is '$F_signoff', not owner"
  [ "$F_commit" = "$sha" ] || bad "the record is for $F_commit, not $sha"
  [[ "$F_date" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}$ ]] || bad "date '$F_date' is not YYYY-MM-DD"
  local named=0 w
  for w in $(printf '%s' "$F_quote" | tr -c '0-9a-f' ' '); do
    [ "${#w}" -ge 7 ] && [ "${sha#"$w"}" != "$sha" ] && named=1
  done
  [ "$named" = 1 ] || bad "the quote does not name ${sha:0:10} (a prefix of at least 7 hex digits): '$F_quote'"
  local r n entry file hash got verdict
  for r in $REVIEWERS; do n="F_verdict_$r"; [ -n "${!n}" ] || bad "the record has no $r verdict"; done
  local want="agy.txt codex.txt kimi.txt record.txt" have
  have="$(git ls-tree --name-only "$ref" | sort | tr '\n' ' ')"
  [ "$have" = "$want " ] || bad "the record holds [${have% }], not exactly [$want]"
  for r in $REVIEWERS; do
    n="F_verdict_$r"; entry="${!n}"
    file="${entry%% *}" hash="${entry#* }"
    [ "$file" = "$r.txt" ] && [[ "$hash" =~ ^[0-9a-f]{64}$ ]] || bad "verdict-$r is '$entry', not '$r.txt <sha256>'"
    got="$(git cat-file blob "$ref:$file" | sha256)"
    [ "$got" = "$hash" ] || bad "the sha256 of $file is $got, not the recorded $hash"
    verdict="$(git cat-file blob "$ref:$file" | grep -E '^[[:space:]]*VERDICT: (ACCEPT|REJECT)[[:space:]]*$' | tail -1 | tr -d '[:space:]')" || true
    [ "$verdict" = "VERDICT:ACCEPT" ] || bad "the $r output's last verdict is '${verdict:-none}', not VERDICT: ACCEPT"
  done
  echo "$sha signed off by the owner ($F_date; codex, kimi and agy accepted): \"$F_quote\""
}

case "${1:-}" in
  record) shift; cmd_record "$@" ;;
  verify) shift; cmd_verify "$@" ;;
  -h|--help) usage ;;
  *) usage >&2; exit 2 ;;
esac
