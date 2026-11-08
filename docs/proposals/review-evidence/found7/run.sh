#!/bin/bash
S=/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/found7
( cd $S/codex && timeout 2400 codex exec -s danger-full-access --skip-git-repo-check "$(cat $S/PROMPT.md)" < /dev/null > $S/codex.out 2>&1 ) &
( cd $S/kimi && timeout 2400 kimi -p "$(cat $S/PROMPT.md)" < /dev/null > $S/kimi.out 2>&1 ) &
( cd $S/agy && timeout 2400 agy -p "$(cat $S/PROMPT_AGY.md)" < /dev/null > $S/agy.out 2>&1 ) &
wait
for f in codex kimi agy; do echo "== $f"; grep -a "ANSWER:" $S/$f.out | tail -1; done
