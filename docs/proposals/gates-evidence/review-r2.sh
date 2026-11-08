#!/bin/bash
# review-r2.sh NAME CLONE SHA BASE PREV GOALFILE : export SHA, run Codex then agy reviews (slot-limited to 2 of mine).
set -uo pipefail
N=$1 C=$2 SHA=$3 BASE=$4 PREV=$5 GOAL=$6
B=~/projects/gates-briefs; D=$B/ro-$N
slots() { local n=0 p; for p in $(pgrep -x 'codex|agy|kimi-code'); do case "$(readlink /proc/$p/cwd 2>/dev/null)" in $B/*) n=$((n+1));; esac; done; echo $n; }
waitslot() { for i in $(seq 1080); do [ "$(slots)" -lt 2 ] && return 0; sleep 20; done; }
rm -rf $D; mkdir -p $D; git -C $C archive $SHA | tar -x -C $D
git -C $C diff $BASE $SHA > $D/REVIEW.diff; git -C $C diff $PREV $SHA > $D/DELTA.diff
cp $B/${N%%[0-9]*}-codex-r1.md $D/PRIOR_CODEX_FINDINGS.md 2>/dev/null; cp $B/${N%%[0-9]*}-agy-r1.log $D/PRIOR_AGY_FINDINGS.md 2>/dev/null
cp $B/${N%%[0-9]*}-fix1.md $D/FIX_BRIEF.md 2>/dev/null
{ cat $B/LONGTERM.md; echo; cat $GOAL; } > $D/PROMPT.md
cd $D && git init -q && git add -A && git -c user.email=x -c user.name=x commit -qm export
waitslot
timeout 3600 systemd-run --user --scope -q -p TasksMax=256 -p MemoryMax=8G -- timeout 3500 codex exec -s danger-full-access --skip-git-repo-check -C $D -o $B/$N-codex.md "$(cat $D/PROMPT.md)" > $B/$N-codex.log 2>&1; echo "codex rc=$?" >> $B/$N-codex.log
waitslot
timeout 3600 systemd-run --user --scope -q -p TasksMax=256 -p MemoryMax=8G -- timeout 3500 agy --dangerously-skip-permissions -p "$(cat $D/PROMPT.md)" > $B/$N-agy.log 2>&1; echo "agy rc=$?" >> $B/$N-agy.log
echo "REVIEW-$N-DONE"
