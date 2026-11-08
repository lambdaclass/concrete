#!/usr/bin/env bash
nohup systemd-run --user --scope -q -p TasksMax=16384 -p MemoryMax=120G timeout 10000 bash ~/gates-run.sh "$1" > ~/gates.log 2>&1 < /dev/null &
echo started
