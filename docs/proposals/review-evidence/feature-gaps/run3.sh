#!/bin/bash
F=/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/feature-gaps
( cd $F/codex && timeout 1800 codex exec -s danger-full-access --skip-git-repo-check - < $F/PROMPT3.md > $F/codex3.out 2>&1; echo "exit $?" >> $F/codex3.out ) &
( cd $F/agy && timeout 1800 agy -p "$(cat $F/PROMPT3_AGY.md)" < /dev/null > $F/agy3.out 2>&1; echo "exit $?" >> $F/agy3.out ) &
wait -n
( cd $F/kimi && timeout 1800 kimi -p "$(cat $F/PROMPT3.md)" < /dev/null > $F/kimi3.out 2>&1; echo "exit $?" >> $F/kimi3.out ) &
wait
