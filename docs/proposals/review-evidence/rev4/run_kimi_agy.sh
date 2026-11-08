#!/usr/bin/env bash
# run3.sh <name> <repo> <sha> <diff-file> <context-file>
# Three read-only reviews (Codex, Kimi, agy) of one exact SHA. Outputs in $D/<name>/.
set -uo pipefail
name="$1" repo="$2" sha="$3" diff="$4" ctx="$5"
D="$(cd "$(dirname "$0")" && pwd)"; W="$D/$name"; rm -rf "$W"; mkdir -p "$W/tree"
git -C "$repo" archive "$sha" | tar -x -C "$W/tree"
cp "$diff" "$W/tree/REVIEW.diff"
CTX="$(cat "$ctx")"
ASK="List findings as file:line with a fix, marking each REAL DEFECT or PREFERENCE. Your last line must be exactly VERDICT: ACCEPT or VERDICT: REJECT."
RO="Read-only review: do not modify any file, do not run anything that changes state or connects to a network."
( cd "$W/tree" && timeout 2400 kimi -p "$RO The tree here is commit $sha; REVIEW.diff is its change. $CTX $ASK" \
    > "$W/kimi.out" 2>&1; echo "rc=$?" >> "$W/kimi.out" ) &
(
  sz=$(wc -c < "$diff")
  if [ "$sz" -lt 120000 ]; then parts=("$diff")
  else
    # split by file at diff --git boundaries into two halves
    awk -v out="$W/part" 'BEGIN{n=1} /^diff --git/{c++} {if (c>0 && size>60000 && /^diff --git/) {n++; size=0} print > (out n ".diff"); size+=length($0)+1}' "$diff"
    parts=("$W"/part*.diff)
  fi
  i=0
  for p in "${parts[@]}"; do i=$((i+1))
    timeout 1500 agy -p "$CTX This is part $i of ${#parts[@]} of the change at commit $sha. Do not try to read files; the diff is below. $ASK
=== DIFF ===
$(cat "$p")" > "$W/agy$i.out" 2>&1; echo "rc=$?" >> "$W/agy$i.out"
  done
) &
wait
echo "done $name"
