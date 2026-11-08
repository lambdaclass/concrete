#!/bin/bash
F=/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/feature-gaps
( cd $F/codex && timeout 1800 codex exec -s danger-full-access --skip-git-repo-check - < $F/PROMPT.md > $F/codex.out 2>&1; echo "exit $?" >> $F/codex.out ) &
( cd $F/kimi && timeout 1800 kimi -p "$(cat $F/PROMPT.md)" < /dev/null > $F/kimi.out 2>&1; echo "exit $?" >> $F/kimi.out ) &
wait
