#!/bin/bash
# remote.sh CLONE BRANCH NAME CMDFILE : run CMDFILE (bash) at BRANCH's head on concrete-ci-2 (bundle + run-at-ref)
set -euo pipefail
CLONE=$1 BR=$2 NAME=$3 CMDF=$4
S=/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/sc
cd "$CLONE"; SHA=$(git rev-parse "$BR")
git bundle create -q "$S/$NAME.bundle" origin/main.."$BR"
echo "sha=$SHA"
cat "$S/$NAME.bundle" | asagent "cat > ~/$NAME.bundle && scp -q ~/$NAME.bundle concrete-ci-2:$NAME.bundle"
tar -C "$CLONE" -cf - scripts/land/run-at-ref.sh scripts/checks/lib/git_sandbox.sh | asagent "rm -rf ~/$NAME-rar && mkdir -p ~/$NAME-rar && tar -C ~/$NAME-rar -xf - && ssh concrete-ci-2 'rm -rf $NAME-rar' && scp -qr ~/$NAME-rar concrete-ci-2:$NAME-rar"
cat "$CMDF" | asagent "cat > ~/$NAME-cmd.sh && scp -q ~/$NAME-cmd.sh concrete-ci-2:$NAME-cmd.sh"
asagent "ssh concrete-ci-2 'git -C ~/concrete2 fetch -q origin && RUN_AT_REF_SOURCE=\$HOME/$NAME.bundle timeout 5400 systemd-run --user --scope -q -p TasksMax=4096 -p MemoryMax=48G bash ~/$NAME-rar/scripts/land/run-at-ref.sh refs/heads/$BR bash \$HOME/$NAME-cmd.sh'"
