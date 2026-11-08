#!/bin/bash
W=/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/verdict6
( cd $W/codex && timeout 1500 codex exec -s danger-full-access --skip-git-repo-check "$(cat $W/PROMPT.md)" < /dev/null > $W/codex.out 2>&1 ) &
( cd $W/kimi && timeout 1500 kimi -p "$(cat $W/PROMPT.md)" < /dev/null > $W/kimi.out 2>&1 ) &
( cd $W/agy && timeout 1500 agy -p "$(cat $W/PROMPT_AGY.md)" < /dev/null > $W/agy.out 2>&1 ) &
wait
for m in codex kimi agy; do echo "== $m"; grep "ANSWER:" $W/$m.out | tail -1; done
