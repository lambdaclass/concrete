set -u
cd /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/rs-r1/repo && timeout 1800 kimi -p "$(cat /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/rs-r1/prompt.txt)" < /dev/null > /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/rs-r1/kimi.out 2>&1 &
K=$!
cd /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/rs-r1/agyempty
timeout 1800 agy -p "Do not use any tools and do not run commands: review only the text in this message. $(sed 's/^Review REVIEW.diff:/Review the diff below (zero context):/; s/ and a changelog fragment:/, AGENTS.md and a changelog fragment: the writers rule ("can you have kimi, codex and agy implement things and we just review it"),/' /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/rs-r1/prompt.txt)

=== PENDING_RECORDS.md
$(cat /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/rs-r1/repo/PENDING_RECORDS.md)

=== diff, zero context
$(cat /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/rs-r1/agy.diff)" < /dev/null > /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/rs-r1/agy.out 2>&1
wait $K
echo all-done
