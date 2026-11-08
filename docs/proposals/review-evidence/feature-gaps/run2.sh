#!/bin/bash
F=/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/feature-gaps
( cd $F/codex && timeout 1800 codex exec -s danger-full-access --skip-git-repo-check - < $F/PROMPT2.md > $F/codex2.out 2>&1; echo "exit $?" >> $F/codex2.out ) &
( cd $F/kimi && timeout 1800 kimi -p "$(cat $F/PROMPT2.md)" < /dev/null > $F/kimi2.out 2>&1; echo "exit $?" >> $F/kimi2.out ) &
wait -n
( cd $F/agy && timeout 1800 agy -p "$(cat $F/PROMPT2_AGY.md)" < /dev/null > $F/agy2.out 2>&1; echo "exit $?" >> $F/agy2.out ) &
wait
