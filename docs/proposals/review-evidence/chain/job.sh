#!/usr/bin/env bash
set -u
R=/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/chain; S=/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad
j="$1"; who="${j%%-*}"; k="${j#*-}"
case "$who" in
  codex) cd $R/$k/repo && GIT_OPTIONAL_LOCKS=0 timeout 2400 codex exec -s danger-full-access --skip-git-repo-check - < $R/$k/prompt.txt > $R/$k/codex.out 2>&1 ;;
  kimi)  cd $R/kimi-$k/repo && timeout 2400 kimi -p "$(cat $R/kimi-$k/prompt.txt)" < /dev/null > $R/kimi-$k/kimi.out 2>&1 ;;
  agy)   cd $R/agyempty && timeout 2400 agy -p "Do not use any tools and do not run commands: review only the text in this message. $(sed 's/You are in a git-archive copy.*and say which files you read\./The zero-context diff and PENDING_RECORDS.md are below./' $R/$k/prompt.txt)

=== PENDING_RECORDS.md
$(cat $S/PENDING_RECORDS.md)

=== diff, zero context
$(cat $R/$k/agy.diff)" < /dev/null > $R/$k/agy.out 2>&1 ;;
esac
echo "$j exit $?" >> $R/done.log
