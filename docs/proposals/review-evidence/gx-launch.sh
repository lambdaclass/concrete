#!/usr/bin/env bash
# gx-launch.sh NAME SHA BRANCH SECS CMD: detached run of gx-run.sh, log ~/gx/NAME.log
mkdir -p "$HOME/gx"
setsid nohup bash -c "timeout -k 60 $4 bash \$HOME/gx-run.sh '$1' '$2' '$3' '$5'" > "$HOME/gx/$1.log" 2>&1 < /dev/null &
disown
echo started
