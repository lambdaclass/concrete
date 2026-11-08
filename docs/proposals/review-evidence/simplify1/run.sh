#!/bin/bash
W=/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/simplify1; G=/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/grammar1/codex/_review/GRAMMAR_REVIEW.md
until [ -s $G ] && ! systemctl --user is-active -q grammar1-write; do sleep 30; done
for d in codex kimi; do cp $G $W/$d/_review/; done
{ echo "Answer directly from the context below. Do NOT read files or use tools; the grammar sheet is included at the end."; echo; cat $W/CTX.md; echo; cat $W/TASK.md; echo; echo "===== GRAMMAR_REVIEW.md ====="; cat $G; } > $W/PROMPT_AGY.md
( cd $W/codex && timeout 2400 codex exec -s danger-full-access --skip-git-repo-check "$(cat $W/PROMPT.md)" < /dev/null > $W/codex.out 2>&1 ) &
( cd $W/kimi && timeout 2400 kimi -p "$(cat $W/PROMPT.md)" < /dev/null > $W/kimi.out 2>&1 ) &
( cd $W/agy && timeout 2400 agy -p "$(cat $W/PROMPT_AGY.md)" < /dev/null > $W/agy.out 2>&1 ) &
wait
