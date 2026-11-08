#!/bin/bash
W=$1
( cd $W/codex && timeout 1800 codex exec -s danger-full-access --skip-git-repo-check "$(cat $W/PROMPT.md)" < /dev/null > $W/codex.out 2>&1 ) &
( cd $W/kimi && timeout 1800 kimi -p "$(cat $W/PROMPT.md)" < /dev/null > $W/kimi.out 2>&1 ) &
( cd $W/agy && timeout 1800 agy -p "$(cat $W/PROMPT_AGY.md)" < /dev/null > $W/agy.out 2>&1 ) &
wait
for m in codex kimi agy; do echo "== $m"; grep "VERDICT:" $W/$m.out | tail -1; done
