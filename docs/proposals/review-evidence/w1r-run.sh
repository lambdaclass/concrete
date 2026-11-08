#!/usr/bin/env bash
cd /home/unbalancedparen/projects/clones/wip-slice0-lexer-w1r
source scripts/checks/lib/bounded.sh
bounded_run 14400 -- codex exec -s danger-full-access "$(cat /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/w1r-brief.md)" < /dev/null > /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/codex-w1r.out 2>&1
echo "codex rc=$?" >> /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/codex-w1r.out
