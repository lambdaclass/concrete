#!/usr/bin/env bash
cd /home/unbalancedparen/projects/clones/wip-signoff-hardening
source scripts/checks/lib/bounded.sh
bounded_run -r 60 14400 -- kimi -p "$(cat /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/sh-brief.md)" > /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/kimi-sh1.out 2>&1
echo "kimi rc=$?" >> /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/kimi-sh1.out
