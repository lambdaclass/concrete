#!/usr/bin/env bash
source "$HOME/projects/clones/signoff-record/scripts/checks/lib/bounded.sh"
cd /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/rev-lexw1/src
bounded_run 3600 -- kimi -p "$(cat /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/rev-lexw1/kimi-prompt.txt)" < /dev/null > /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/rev-lexw1/kimi.out 2>&1
echo "rc=$?" > /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/rev-lexw1/kimi.out.rc
